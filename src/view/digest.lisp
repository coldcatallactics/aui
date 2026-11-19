(in-package #:aui)

;;;; -- Digest --

;;; The digest is a summary of a conversation, and it is shown beside the
;;; conversation. It contains the plan of the agent, the files that the agent
;;; changed, linked or named, and the messages sent to the agent. All three are
;;; read from the transcript. The digest is read again only if an entry arrives
;;; or if the agent starts or stops working. The file names in each answer are
;;; cached.

(defparameter *file-changing-tools* '("resource.edit")
  "A list of the tools that change the file named by the uri argument.")

(defstruct digest
  "A summary of one transcript at one length. It contains the current plan, the touched files, the sent messages and caches of file names."
  (transcript  nil)
  (length      -1  :type integer)
  (working-p   nil :type boolean)
  (directory   ""  :type string)
  (root        ""  :type string)
  (plan        nil)
  (plan-layout nil :type list)
  (files       nil :type list)
  (messages    nil :type list)
  (named       (make-hash-table :test #'eq)    :type hash-table)
  (resolved    (make-hash-table :test #'equal) :type hash-table))

(-> digest-create (transcript string) digest)
(defun digest-create (transcript directory)
  "Return an unread digest of TRANSCRIPT for a channel with the folder DIRECTORY.

The root of the digest is the true name of DIRECTORY. Files are compared by
their true names, which can differ from the given names if a folder is a link,
such as the temporary folder of a Mac."
  (make-digest :transcript transcript
               :directory  directory
               :root       (handler-case
                               (uiop:native-namestring
                                (truename (uiop:ensure-directory-pathname
                                           (uiop:parse-native-namestring directory))))
                             (error ()
                               directory))))

(-> digest--names (digest assistant-entry) list)
(defun digest--names (digest entry)
  "Return the possible file names in the answer ENTRY. The names are read once for each revision of the answer."
  (let ((known (gethash entry (digest-named digest))))
    (if (and known (= (first known) (entry-revision entry)))
        (rest known)
        (let ((names (files--named (assistant-entry-text entry))))
          (setf (gethash entry (digest-named digest)) (cons (entry-revision entry) names))
          names))))

(-> digest--file (digest string) (or null pathname))
(defun digest--file (digest name)
  "Return the local file named by NAME, relative to the folder of DIGEST, or NIL if there is no such file.

Each name is looked up once."
  (multiple-value-bind (known known-p) (gethash name (digest-resolved digest))
    (if known-p
        known
        (setf (gethash name (digest-resolved digest))
              (let ((file (link-file name (digest-directory digest))))
                (and file (pathname-name file) file))))))

(-> digest--read (digest) null)
(defun digest--read (digest)
  "Fill DIGEST from the transcript.

The plan is the latest plan update. The files are a list of (PATHNAME . CHANGED-P)
pairs, with the last touched file first and each file once. If a file was changed
by a tool, it is marked as changed even if an answer names it later. The messages
are the entries sent to the agent by the person and by other participants, with
the oldest first."
  (let ((plan     nil)
        (files    nil)
        (messages nil))
    (flet ((touch (name changed-p)
             (let ((file (digest--file digest name)))
               (when file
                 (let ((known (assoc file files :test #'equal)))
                   (setf files (cons (cons file (or changed-p (and known (rest known) t)))
                                     (remove known files))))))))
      (loop for entry across (transcript-entries (digest-transcript digest))
            do (typecase entry
                 (assistant-entry
                  (dolist (name (digest--names digest entry))
                    (touch name nil)))
                 (tool-entry
                  (cond
                    ((string= (tool-entry-name entry) "plan.update")
                     (setf plan entry))
                    ((and (member (tool-entry-name entry) *file-changing-tools* :test #'string=)
                          (eq (tool-entry-status entry) ':completed))
                     (let ((uri (rest (assoc "uri" (tool-entry-arguments entry) :test #'string=))))
                       (when (stringp uri)
                         (touch uri t))))))
                 (user-entry
                  (push entry messages)))))
    (setf (digest-plan digest)        plan
          (digest-plan-layout digest) nil
          (digest-files digest)       files
          (digest-messages digest)    (nreverse messages)))
  nil)

(-> workbench-digest (workbench) digest)
(defun workbench-digest (workbench)
  "Return the digest of the shown conversation.

The digest is read again if an entry arrived, if the agent started or stopped
working, or if the folder of the channel changed."
  (let* ((transcript (workbench-transcript workbench))
         (length     (transcript-length transcript))
         (working-p  (workbench-working-p workbench))
         (directory  (workbench-directory workbench))
         (digest     (workbench--digest workbench)))
    (unless (and digest
                 (eq transcript (digest-transcript digest))
                 (string= directory (digest-directory digest)))
      (setf digest                        (digest-create transcript directory)
            (workbench--digest workbench) digest))
    (unless (and (= length (digest-length digest))
                 (eq working-p (digest-working-p digest)))
      ;; The agent can create files while it works, so the names are looked up again.
      (when (and (digest-working-p digest) (not working-p))
        (clrhash (digest-resolved digest)))
      (setf (digest-length digest)    length
            (digest-working-p digest) working-p)
      (digest--read digest))
    digest))

(-> digest-plan-progress (digest) (values integer integer))
(defun digest-plan-progress (digest)
  "Return the number of done steps in the plan of DIGEST and the total number of steps."
  (let* ((arguments (and (digest-plan digest) (tool-entry-arguments (digest-plan digest))))
         (steps     (rest (or (assoc "plan" arguments :test #'string=)
                              (assoc "steps" arguments :test #'string=)))))
    (if (listp steps)
        (values (count-if (lambda (step)
                            (and (consp step)
                                 (member (rest (assoc "status" step :test #'string=)) '("completed" "done")
                                         :test #'equal)))
                          steps)
                (length steps))
        (values 0 0))))


;;;; -- File List --

(-> files-linked (workbench) list)
(defun files-linked (workbench)
  "Return a list of the local files that the agent changed, linked or named in the shown conversation.

The last touched file comes first, and each file is in the list once."
  (mapcar #'first (digest-files (workbench-digest workbench))))

(-> file-place (pathname string) string)
(defun file-place (file root)
  "Return the folder of FILE as a short, readable path.

If FILE is below ROOT, the path is relative to ROOT. Otherwise, the path is
relative to the home folder or to the root of the file system. For a file in
ROOT, the path is empty."
  (let ((folder (uiop:native-namestring (uiop:pathname-directory-pathname file)))
        (root   (uiop:native-namestring (uiop:ensure-directory-pathname root))))
    (if (uiop:string-prefix-p root folder)
        (string-right-trim "/" (subseq folder (length root)))
        (short-directory folder))))

(-> files-show (workbench) null)
(defun files-show (workbench)
  "Show a list of the files that the agent changed, linked or named in the shown conversation. Open the chosen file."
  (let* ((digest (workbench-digest workbench))
         (files  (digest-files digest))
         (root   (digest-root digest)))
    (if (null files)
        (workbench-show-panel workbench "Files"
                              (panel--empty "No local files were changed or linked in this conversation yet."))
        (workbench-pick workbench
                        (loop for (file . changed-p) in files
                              collect (make-choice :label  (file-namestring file)
                                                   :detail (format nil "~:[~;changed  ~]~A" changed-p
                                                                   (file-place file root))
                                                   :value  (uiop:native-namestring file)))
                        (lambda (name)
                          (workbench-open-link workbench name))
                        :title "Files")))
  nil)
