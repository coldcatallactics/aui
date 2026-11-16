(in-package #:aui)

;;;; -- Digest --

;;; What a conversation holds that is worth seeing beside it: the plan its
;;; agent keeps, the files the agent changed, linked or named, and the
;;; messages that were sent to the agent. All three are read from the
;;; transcript. Reading a long conversation again for every drawing would
;;; cost too much, so a digest is kept until an entry arrives or the agent
;;; starts or stops working, and it remembers what each answer names and
;;; which of those names are files.

(defparameter *file-changing-tools* '("resource.edit")
  "The tools whose call changes the file its uri names.")

(defstruct digest
  "A reading of one transcript at one length, holding the plan in force, the files touched, the messages sent, and what was learned on the way."
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
  "Return an unread digest of TRANSCRIPT for a channel that works in DIRECTORY.

The folder's true name is found once and kept as the digest's root. Files
are known by their true names, and a folder reached through a link, as the
temporary folder of a Mac is, has another than the one it goes by."
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
  "Return what ENTRY's answer may name as a file, read once for each state of the answer."
  (let ((known (gethash entry (digest-named digest))))
    (if (and known (= (first known) (entry-revision entry)))
        (rest known)
        (let ((names (files--named (assistant-entry-text entry))))
          (setf (gethash entry (digest-named digest)) (cons (entry-revision entry) names))
          names))))

(-> digest--file (digest string) (or null pathname))
(defun digest--file (digest name)
  "Return the file of this machine that NAME names from the digest's folder, or NIL when it names none. Each name is looked up once."
  (multiple-value-bind (known known-p) (gethash name (digest-resolved digest))
    (if known-p
        known
        (setf (gethash name (digest-resolved digest))
              (let ((file (link-file name (digest-directory digest))))
                (and file (pathname-name file) file))))))

(-> digest--read (digest) null)
(defun digest--read (digest)
  "Fill DIGEST from its transcript.

The plan is the latest update of it. The files are (PATHNAME . CHANGED-P),
the one touched last first, each once; a file a tool changed stays marked
changed when an answer names it later. The messages are the entries sent to
the agent, by the person and by other participants, oldest first."
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
  "Return the digest of the conversation shown, read again when an entry arrived, when the agent started or stopped working, or when the channel's folder changed."
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
      ;; A name that was no file while the agent worked may be one now.
      (when (and (digest-working-p digest) (not working-p))
        (clrhash (digest-resolved digest)))
      (setf (digest-length digest)    length
            (digest-working-p digest) working-p)
      (digest--read digest))
    digest))

(-> digest-plan-progress (digest) (values integer integer))
(defun digest-plan-progress (digest)
  "Return how many steps of DIGEST's plan are done and how many it has."
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


;;;; -- The Files, Listed --

(-> files-linked (workbench) list)
(defun files-linked (workbench)
  "Return the files of this machine that the agent changed, linked or named in the conversation shown, the one touched last first, each once, as pathnames."
  (mapcar #'first (digest-files (workbench-digest workbench))))

(-> file-place (pathname string) string)
(defun file-place (file root)
  "Return where FILE lies as a person reads it: its folder from ROOT, the true name of the channel's folder, when it lies beneath that, else from the home folder or the top. A file in ROOT itself lies nowhere to speak of."
  (let ((folder (uiop:native-namestring (uiop:pathname-directory-pathname file)))
        (root   (uiop:native-namestring (uiop:ensure-directory-pathname root))))
    (if (uiop:string-prefix-p root folder)
        (string-right-trim "/" (subseq folder (length root)))
        (short-directory folder))))

(-> files-show (workbench) null)
(defun files-show (workbench)
  "Offer the files the agent changed, linked or named in the conversation shown; the one chosen is opened."
  (let* ((digest (workbench-digest workbench))
         (files  (digest-files digest))
         (root   (digest-root digest)))
    (if (null files)
        (workbench-show-panel workbench "Files"
                              (panel--empty "The agent has changed or linked no file of this machine in this conversation yet."))
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
