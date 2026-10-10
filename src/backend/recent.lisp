(in-package #:aui)

;;;; -- Recent Conversations --

;;; A bounded, read-only view of Autolith's conversation store for the
;;; welcome screen, used before any Autolith process exists. It reads one
;;; header form per conversation and one picker sidecar, with *READ-EVAL*
;;; off, and skips anything it cannot read or validate. Once a backend is
;;; connected, its _autolith/conversations catalog takes over.

(defparameter *autolith-data-root* nil
  "Autolith's data root, or NIL to derive it from XDG_DATA_HOME.")

(defparameter *autolith-state-root* nil
  "Autolith's state root, or NIL to derive it from XDG_STATE_HOME.")

(defparameter *recent-conversation-limit* 200
  "How many conversations the welcome list reads at most, newest first by file date.")

(-> xdg-root (string string) pathname)
(defun xdg-root (variable fallback)
  "Return the autolith directory under the XDG VARIABLE, or under FALLBACK
relative to the home directory when the variable is unset or not absolute."
  (let ((value (uiop:getenv variable)))
    (uiop:ensure-directory-pathname
     (merge-pathnames "autolith/"
                      (if (and value (plusp (length value)) (char= #\/ (char value 0)))
                          (uiop:ensure-directory-pathname value)
                          (merge-pathnames fallback (user-homedir-pathname)))))))

(-> autolith-data-root () pathname)
(defun autolith-data-root ()
  "Return Autolith's data root."
  (or *autolith-data-root* (xdg-root "XDG_DATA_HOME" ".local/share/")))

(-> autolith-state-root () pathname)
(defun autolith-state-root ()
  "Return Autolith's state root."
  (or *autolith-state-root* (xdg-root "XDG_STATE_HOME" ".local/state/")))


;;; Bounded reading

(-> read-first-form (pathname) t)
(defun read-first-form (path)
  "Return the first readable form of the file at PATH, or NIL when the file
is missing, empty, or does not begin with a readable form. Evaluation during
reading is disabled."
  (handler-case
      (with-open-file (stream path :direction :input :external-format :utf-8
                                   :if-does-not-exist nil)
        (when stream
          (with-standard-io-syntax
            (let ((*read-eval* nil))
              (read stream nil nil)))))
    (error ()
      nil)))

(-> plist-record-p (t keyword) boolean)
(defun plist-record-p (form tag)
  "Return T when FORM is a list beginning with TAG followed by keyword and value pairs."
  (and (consp form)
       (eq (first form) tag)
       (loop for (key nil) on (rest form) by #'cddr
             always (keywordp key))))

(-> record-string (list keyword) (or null string))
(defun record-string (record key)
  "Return the string value of KEY in RECORD, coercing base strings, or NIL."
  (let ((value (getf (rest record) key)))
    (and (stringp value) (coerce value 'simple-string))))

(-> record-integer (list keyword) (or null integer))
(defun record-integer (record key)
  "Return the non-negative integer value of KEY in RECORD, or NIL."
  (let ((value (getf (rest record) key)))
    (and (integerp value) (>= value 0) value)))


;;; Conversation headers and picker sidecars

(-> conversation-header-path (pathname) (or null pathname))
(defun conversation-header-path (entry)
  "Return the file whose first form is the conversation header for ENTRY, a
flat version 1 file or a version 2 segment directory, or NIL."
  (cond
    ((uiop:directory-pathname-p entry)
     (let ((segments (sort (remove-if-not (lambda (file) (string-equal "sexp" (pathname-type file)))
                                          (uiop:directory-files entry))
                           #'string< :key #'file-namestring)))
       (first segments)))
    ((and (string-equal "sexp" (pathname-type entry))
          (not (search ".peers" (file-namestring entry))))
     entry)
    (t
     nil)))

(-> conversation-latest-write-date (pathname) (or null integer))
(defun conversation-latest-write-date (entry)
  "Return the newest file write date under ENTRY, or NIL."
  (handler-case
      (if (uiop:directory-pathname-p entry)
          (loop for file in (uiop:directory-files entry)
                maximize (or (file-write-date file) 0))
          (file-write-date entry))
    (error ()
      nil)))

(-> picker-record (string) (or null list))
(defun picker-record (conversation-id)
  "Return the validated picker sidecar record for CONVERSATION-ID, or NIL."
  (let ((form (read-first-form (merge-pathnames (format nil "conversation-picker/~A.sexp" conversation-id)
                                                (autolith-data-root)))))
    (and (plist-record-p form ':conversation-picker-metadata) form)))

(-> live-session-ids () list)
(defun live-session-ids ()
  "Return the conversation ids whose localgroup endpoint records name a live process."
  (let ((directory (merge-pathnames "localgroup/" (autolith-state-root))))
    (loop for file in (handler-case (uiop:directory-files directory) (error () nil))
          for form = (and (string-equal "sexp" (pathname-type file)) (read-first-form file))
          when (and (plist-record-p form ':localgroup-endpoint)
                    (record-string form ':session-id)
                    (process-alive-p (record-integer form ':pid)))
            collect (record-string form ':session-id))))

(-> process-alive-p ((or null integer)) boolean)
(defun process-alive-p (pid)
  "Return T when a process with PID exists, judged by the proc filesystem."
  (and pid (plusp pid)
       (uiop:directory-exists-p (format nil "/proc/~D/" pid))
       t))

(-> conversation-entry-from-store (pathname list) (or null conversation-entry))
(defun conversation-entry-from-store (entry live-ids)
  "Return the conversation entry for the store ENTRY, or NIL when its header
cannot be read or validated."
  (let* ((header-path (conversation-header-path entry))
         (header (and header-path (read-first-form header-path))))
    (when (and (plist-record-p header ':conversation)
               (record-string header ':id)
               (record-string header ':directory))
      (let* ((id (record-string header ':id))
             (picker (picker-record id))
             (written (conversation-latest-write-date entry)))
        (make-instance 'conversation-entry
                       :id id
                       :display-id (conversation-display-id id)
                       :title (or (and picker (record-string picker ':title))
                                  (record-string header ':title))
                       :directory (record-string header ':directory)
                       :model (record-string header ':model)
                       :created-at (record-integer header ':created-at)
                       :last-activity-at (or (and picker (record-integer picker ':source-write-date))
                                             (record-integer header ':last-activity-at)
                                             written)
                       :user-turn-count (or (and picker (record-integer picker ':user-turn-count))
                                            (record-integer header ':user-turn-count)
                                            0)
                       :preview (and picker (record-string picker ':preview))
                       :live-p (and (member id live-ids :test #'string=) t))))))

(-> recent-conversations-from-autolith () list)
(defun recent-conversations-from-autolith ()
  "Return conversation entries read from Autolith's store, newest first,
at most *RECENT-CONVERSATION-LIMIT* of them. Unreadable entries are skipped."
  (let* ((root (merge-pathnames "conversations/" (autolith-data-root)))
         (entries (handler-case
                      (append (uiop:subdirectories root) (uiop:directory-files root))
                    (error ()
                      nil)))
         (dated (loop for entry in entries
                      for date = (conversation-latest-write-date entry)
                      when (and date (conversation-header-path entry))
                        collect (cons date entry)))
         (newest (subseq (sort dated #'> :key #'first)
                         0 (min *recent-conversation-limit* (length dated))))
         (live-ids (live-session-ids)))
    (conversations-by-recency
     (loop for (nil . entry) in newest
           for conversation = (conversation-entry-from-store entry live-ids)
           when conversation
             collect conversation))))

(setf *recent-conversations-function* 'recent-conversations-from-autolith)
