(in-package #:aui)

;;;; -- Ledger --

;;; What a person decided about their files, kept outside every checkout so
;;; that a checkout holds nothing but its source. A file marked completed is
;;; remembered by its whole path with the digest of what it held at that
;;; moment. It counts as completed for as long as it holds exactly that, so a
;;; file that anyone changes afterwards is open again by itself. The folders
;;; a person folded in the list of files are remembered here too.
;;;
;;; There is one ledger for all folders, so the marks made with the home
;;; folder open are there when only one checkout inside it is open, and the
;;; other way round. Each window reads the file again before it writes it, so
;;; two windows do not undo each other's marks.

(defvar *ledger-directory* nil
  "The folder the ledger is kept in, or NIL for the person's state folder.")

(defparameter *ledger-name* "marks.sexp"
  "The name of the ledger's file in its folder. Every other file there is a ledger of one folder from before, which is taken in once.")

(defstruct ledger
  "What is remembered, as the FILE it is kept in, the ROOT of the folder open in this window with its closing slash, and, each by whole paths, the files marked COMPLETED with the digest each had then, as BASELINE the digest each file had when first seen, which is kept only where Git does not say what changed, and the FOLDS, the folders folded in the list of files. IMPORTED names the ledgers of one folder from before that were taken in, and SECTIONS holds, by the whole path of a file, the digests of the definitions in it that are completed."
  (file      nil)
  (root      ""  :type string)
  (completed (make-hash-table :test #'equal) :type hash-table)
  (baseline  (make-hash-table :test #'equal) :type hash-table)
  (folds     nil :type list)
  (imported  nil :type list)
  (sections  (make-hash-table :test #'equal) :type hash-table))

(-> ledger-directory () pathname)
(defun ledger-directory ()
  "Return the folder the ledger is kept in: the one set, else unslop's folder under the person's state folder."
  (or *ledger-directory*
      (let ((state (uiop:getenvp "XDG_STATE_HOME")))
        (merge-pathnames "unslop/"
                         (if state
                             (uiop:ensure-directory-pathname state)
                             (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(-> ledger-pathname () pathname)
(defun ledger-pathname ()
  "Return the file the ledger is kept in."
  (merge-pathnames *ledger-name* (ledger-directory)))

(-> ledger--string (t) (or null string))
(defun ledger--string (object)
  "Return OBJECT as a string of characters when it is a string, else NIL."
  (and (stringp object) (coerce object '(simple-array character (*)))))

(-> ledger--read-file (pathname) (values list boolean))
(defun ledger--read-file (pathname)
  "Return the property list the file at PATHNAME holds, and whether it could be read: a file that is not there reads as an empty list, one that does not hold a property list does not read."
  (if (not (probe-file pathname))
      (values nil t)
      (handler-case
          (let ((form (with-open-file (stream pathname :external-format ':utf-8)
                        (with-standard-io-syntax
                          (let ((*read-eval* nil))
                            (read stream nil nil))))))
            (if (and (listp form) (evenp (length form)))
                (values form t)
                (values nil nil)))
        (error ()
          (values nil nil)))))

(-> ledger--table (t) hash-table)
(defun ledger--table (pairs)
  "Return a table of PAIRS, a list of (NAME . DIGEST) as a ledger file holds them. Anything else in the list is left out."
  (let ((table (make-hash-table :test #'equal)))
    (when (listp pairs)
      (dolist (pair pairs)
        (when (and (consp pair) (stringp (first pair)) (stringp (rest pair)))
          (setf (gethash (ledger--string (first pair)) table) (ledger--string (rest pair))))))
    table))

(-> ledger--strings (t) list)
(defun ledger--strings (list)
  "Return the strings of LIST, leaving out anything else."
  (and (listp list) (mapcar #'ledger--string (remove-if-not #'stringp list))))

(-> ledger--take (ledger list) null)
(defun ledger--take (ledger plist)
  "Make LEDGER hold what PLIST, read from the ledger's file, says."
  (setf (ledger-completed ledger) (ledger--table (getf plist :completed))
        (ledger-baseline ledger)  (ledger--table (getf plist :baseline))
        (ledger-folds ledger)     (ledger--strings (getf plist :folds))
        (ledger-imported ledger)  (ledger--strings (getf plist :imported))
        (ledger-sections ledger)  (let ((table (make-hash-table :test #'equal)))
                                    (dolist (entry (getf plist :sections) table)
                                      (when (and (consp entry) (stringp (first entry)))
                                        (let ((digests (ledger--strings (rest entry))))
                                          (when digests
                                            (setf (gethash (ledger--string (first entry)) table) digests)))))))
  nil)

(-> ledger--import (ledger) boolean)
(defun ledger--import (ledger)
  "Take into LEDGER what the ledgers of one folder from before hold that it does not, and return whether there was any such ledger. Each is taken in once and left where it is."
  (let ((found nil))
    (dolist (file (uiop:directory-files (ledger-directory) "*.sexp"))
      (let ((name (file-namestring file)))
        (unless (or (string= name *ledger-name*)
                    (member name (ledger-imported ledger) :test #'string=))
          (multiple-value-bind (plist read-p) (ledger--read-file file)
            (let ((root (ledger--string (getf plist :root))))
              (when (and read-p root (plusp (length root)))
                (let ((root (if (char= #\/ (char root (1- (length root)))) root (format nil "~A/" root))))
                  (flet ((merge-into (table pairs)
                           (maphash (lambda (key digest)
                                      (let ((path (concatenate 'string root key)))
                                        (unless (gethash path table)
                                          (setf (gethash path table) digest))))
                                    (ledger--table pairs))))
                    (merge-into (ledger-completed ledger) (getf plist :completed))
                    (merge-into (ledger-baseline ledger) (getf plist :baseline))
                    (setf (ledger-folds ledger)
                          (union (ledger-folds ledger)
                                 (mapcar (lambda (folder) (concatenate 'string root folder))
                                         (ledger--strings (getf plist :folded)))
                                 :test #'string=))))
                (push name (ledger-imported ledger))
                (setf found t)))))))
    found))

(-> ledger--pairs (hash-table) list)
(defun ledger--pairs (table)
  "Return the entries of TABLE as (NAME . DIGEST), sorted by name."
  (sort (loop for name being the hash-keys of table using (hash-value digest)
              collect (cons name digest))
        #'string<
        :key #'first))

(-> ledger--write (ledger) boolean)
(defun ledger--write (ledger)
  "Write LEDGER to its file and return whether that worked. The file is written beside itself and moved into place."
  (let* ((pathname (ledger-file ledger))
         (beside   (make-pathname :type "new" :defaults pathname)))
    (handler-case
        (progn
          (ensure-directories-exist pathname)
          (with-open-file (stream beside :direction ':output :if-exists ':supersede :external-format ':utf-8)
            (with-standard-io-syntax
              (let ((*print-readably* nil)
                    (*print-pretty*   nil))
                (format stream "(:completed (~{~S~^~%             ~})~% :baseline (~{~S~^~%            ~})~% :folds (~{~S~^~%         ~})~% :sections (~{~S~^~%            ~})~% :imported (~{~S~^ ~}))~%"
                        (ledger--pairs (ledger-completed ledger))
                        (ledger--pairs (ledger-baseline ledger))
                        (sort (copy-list (ledger-folds ledger)) #'string<)
                        (sort (loop for path being the hash-keys of (ledger-sections ledger) using (hash-value digests)
                                    collect (cons path (sort (copy-list digests) #'string<)))
                              #'string<
                              :key #'first)
                        (sort (copy-list (ledger-imported ledger)) #'string<)))))
          (uiop:rename-file-overwriting-target beside pathname)
          t)
      (error ()
        nil))))

(-> ledger--change (ledger function) boolean)
(defun ledger--change (ledger change)
  "Read LEDGER's file again, call CHANGE with no arguments to change LEDGER, and write it, so that what another window wrote in the meantime stays. Return whether the file was written.

When the file is there and does not read, it is not written over: the
change is kept in this window alone."
  (multiple-value-bind (plist read-p) (ledger--read-file (ledger-file ledger))
    (when read-p
      (ledger--take ledger plist))
    (funcall change)
    (and read-p (ledger--write ledger))))

(-> ledger-open (pathname) ledger)
(defun ledger-open (root)
  "Return the ledger as the window for the folder ROOT sees it, read from its file, with the ledgers of one folder from before taken in."
  (let* ((name   (uiop:native-namestring (uiop:ensure-directory-pathname root)))
         (ledger (make-ledger :file (ledger-pathname)
                              :root (if (char= #\/ (char name (1- (length name)))) name (format nil "~A/" name)))))
    (multiple-value-bind (plist read-p) (ledger--read-file (ledger-file ledger))
      (ledger--take ledger plist)
      (when (and read-p
                 (uiop:directory-exists-p (ledger-directory))
                 (some (lambda (file)
                         (let ((name (file-namestring file)))
                           (not (or (string= name *ledger-name*)
                                    (member name (ledger-imported ledger) :test #'string=)))))
                       (uiop:directory-files (ledger-directory) "*.sexp")))
        (ledger--change ledger (lambda () (ledger--import ledger)))))
    ledger))

(-> ledger-path (ledger string) string)
(defun ledger-path (ledger name)
  "Return the whole path of the file or folder NAME of the folder LEDGER's window has open."
  (concatenate 'string (ledger-root ledger) name))

(-> ledger-entries (ledger hash-table) list)
(defun ledger-entries (ledger table)
  "Return the entries of TABLE, one of LEDGER's, that lie in the folder its window has open, as (NAME . DIGEST) with NAME from that folder."
  (let ((root    (ledger-root ledger))
        (entries nil))
    (maphash (lambda (path digest)
               (when (and (> (length path) (length root))
                          (string= root path :end2 (length root)))
                 (push (cons (subseq path (length root)) digest) entries)))
             table)
    (sort entries #'string< :key #'first)))

(-> ledger-completed-p (ledger string (or null string)) boolean)
(defun ledger-completed-p (ledger name digest)
  "Return T when the file NAME was marked completed while it held what DIGEST is the digest of."
  (let ((marked (gethash (ledger-path ledger name) (ledger-completed ledger))))
    (and marked digest (string= marked digest))))

(-> ledger-mark (ledger string (or null string)) boolean)
(defun ledger-mark (ledger name digest)
  "Remember that the file NAME is completed while it holds what DIGEST is the digest of, or forget that it was when DIGEST is NIL. Return whether the ledger could be written."
  (let ((path (ledger-path ledger name)))
    (ledger--change ledger
                    (lambda ()
                      (if digest
                          (setf (gethash path (ledger-completed ledger)) digest)
                          (remhash path (ledger-completed ledger)))))))

(-> ledger-note-baseline (ledger list) boolean)
(defun ledger-note-baseline (ledger pairs)
  "Remember the digests of PAIRS, (NAME . DIGEST) of files seen for the first time, as what those files held then. Return whether the ledger could be written."
  (ledger--change ledger
                  (lambda ()
                    (loop for (name . digest) in pairs
                          do (setf (gethash (ledger-path ledger name) (ledger-baseline ledger)) digest)))))

(-> ledger-folded-p (ledger string) boolean)
(defun ledger-folded-p (ledger folder)
  "Return T when FOLDER, a name from the open folder with its closing slash, is folded in the list of files."
  (and (member (ledger-path ledger folder) (ledger-folds ledger) :test #'string=) t))

(-> ledger-folded (ledger) list)
(defun ledger-folded (ledger)
  "Return the folders folded in the list that lie in the folder LEDGER's window has open, as names from it."
  (let ((root (ledger-root ledger)))
    (sort (loop for path in (ledger-folds ledger)
                when (and (> (length path) (length root))
                          (string= root path :end2 (length root)))
                  collect (subseq path (length root)))
          #'string<)))

(-> ledger-fold (ledger list boolean) boolean)
(defun ledger-fold (ledger folders folded-p)
  "Remember each of FOLDERS as folded in the list of files, or as unfolded when FOLDED-P is NIL. Return whether the ledger could be written. Nothing is written when nothing changes."
  (let ((paths (mapcar (lambda (folder) (ledger-path ledger folder)) folders)))
    (if (if folded-p
            (subsetp paths (ledger-folds ledger) :test #'string=)
            (null (intersection paths (ledger-folds ledger) :test #'string=)))
        t
        (ledger--change ledger
                        (lambda ()
                          (setf (ledger-folds ledger)
                                (if folded-p
                                    (union (ledger-folds ledger) paths :test #'string=)
                                    (set-difference (ledger-folds ledger) paths :test #'string=))))))))

(-> ledger-sections-completed (ledger string) list)
(defun ledger-sections-completed (ledger name)
  "Return the digests of the definitions of the file NAME that are marked completed."
  (gethash (ledger-path ledger name) (ledger-sections ledger)))

(-> ledger-mark-section (ledger string string &key (:completed-p boolean) (:present list)) boolean)
(defun ledger-mark-section (ledger name digest &key completed-p present)
  "Remember the definition of the file NAME whose words have DIGEST as completed, or forget that it was unless COMPLETED-P. Marks for definitions no longer among PRESENT, the digests of the file's definitions now, are dropped. Return whether the ledger could be written."
  (let ((path (ledger-path ledger name)))
    (ledger--change ledger
                    (lambda ()
                      (let* ((kept    (remove-if-not (lambda (known) (member known present :test #'string=))
                                                     (gethash path (ledger-sections ledger))))
                             (digests (if completed-p
                                          (adjoin digest kept :test #'string=)
                                          (remove digest kept :test #'string=))))
                        (if digests
                            (setf (gethash path (ledger-sections ledger)) digests)
                            (remhash path (ledger-sections ledger))))))))
