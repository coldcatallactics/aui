(in-package #:aui)

;;;; -- Ledger --


(defvar *ledger-directory* nil
  "The folder of the ledger, or NIL for the state folder of the person.")

(defparameter *ledger-name* "marks.sexp"
  "The file name of the ledger. All other files in the folder of the ledger are older ledgers of single folders. Each of them is imported once.")

(defstruct ledger
  "Ledger state. COMPLETED and BASELINE map paths to digests; FOLDS and IMPORTED are path lists; SECTIONS maps paths to completed-definition digests."
  (file      nil)
  (root      ""  :type string)
  (completed (make-hash-table :test #'equal) :type hash-table)
  (baseline  (make-hash-table :test #'equal) :type hash-table)
  (folds     nil :type list)
  (imported  nil :type list)
  (sections  (make-hash-table :test #'equal) :type hash-table))

(-> ledger-directory () pathname)
(defun ledger-directory ()
  "Return the configured ledger folder, or the unslop folder under the user's state folder."
  (or *ledger-directory*
      (let ((state (uiop:getenvp "XDG_STATE_HOME")))
        (merge-pathnames "unslop/"
                         (if state
                             (uiop:ensure-directory-pathname state)
                             (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(-> ledger-pathname () pathname)
(defun ledger-pathname ()
  "Return the ledger file."
  (merge-pathnames *ledger-name* (ledger-directory)))

(-> ledger--string (t) (or null string))
(defun ledger--string (object)
  "Return OBJECT as a string of characters if it is a string, or else NIL."
  (and (stringp object) (coerce object '(simple-array character (*)))))

(-> ledger--read-file (pathname) (values list boolean))
(defun ledger--read-file (pathname)
  "Read PATHNAME as a ledger property list. Missing files count as empty; malformed files are unreadable."
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
  "Return valid NAME/DIGEST pairs from PAIRS as a hash table."
  (let ((table (make-hash-table :test #'equal)))
    (when (listp pairs)
      (dolist (pair pairs)
        (when (and (consp pair) (stringp (first pair)) (stringp (rest pair)))
          (setf (gethash (ledger--string (first pair)) table) (ledger--string (rest pair))))))
    table))

(-> ledger--strings (t) list)
(defun ledger--strings (list)
  "Return the strings in LIST. Other elements are left out."
  (and (listp list) (mapcar #'ledger--string (remove-if-not #'stringp list))))

(-> ledger--take (ledger list) null)
(defun ledger--take (ledger plist)
  "Set the content of LEDGER from PLIST, which has the format of the ledger file."
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
  "Import each unimported single-folder ledger into LEDGER."
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
  "Write LEDGER atomically and return whether the write succeeded."
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
  "Reload LEDGER, apply CHANGE, and write it back without discarding changes made by another window."
  (multiple-value-bind (plist read-p) (ledger--read-file (ledger-file ledger))
    (when read-p
      (ledger--take ledger plist))
    (funcall change)
    (and read-p (ledger--write ledger))))

(-> ledger-open (pathname) ledger)
(defun ledger-open (root)
  "Return the ledger for the folder ROOT, read from the ledger file. Older ledgers of single folders are imported."
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
  "Return the whole path of NAME, which is a file or folder relative to the root of LEDGER."
  (concatenate 'string (ledger-root ledger) name))

(-> ledger-entries (ledger hash-table) list)
(defun ledger-entries (ledger table)
  "Return the entries of TABLE that are inside the root of LEDGER, as (NAME . DIGEST). TABLE is one of the tables of LEDGER, and each NAME is relative to the root."
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
  "Return T if the file NAME was marked completed with the digest DIGEST."
  (let ((marked (gethash (ledger-path ledger name) (ledger-completed ledger))))
    (and marked digest (string= marked digest))))

(-> ledger-mark (ledger string (or null string)) boolean)
(defun ledger-mark (ledger name digest)
  "Mark the file NAME as completed with the digest DIGEST. If DIGEST is NIL, remove the mark. Return T if the ledger could be written."
  (let ((path (ledger-path ledger name)))
    (ledger--change ledger
                    (lambda ()
                      (if digest
                          (setf (gethash path (ledger-completed ledger)) digest)
                          (remhash path (ledger-completed ledger)))))))

(-> ledger-note-baseline (ledger list) boolean)
(defun ledger-note-baseline (ledger pairs)
  "Record the digests in PAIRS as the first content of the files. PAIRS is a list of (NAME . DIGEST) for files seen for the first time. Return T if the ledger could be written."
  (ledger--change ledger
                  (lambda ()
                    (loop for (name . digest) in pairs
                          do (setf (gethash (ledger-path ledger name) (ledger-baseline ledger)) digest)))))

(-> ledger-folded-p (ledger string) boolean)
(defun ledger-folded-p (ledger folder)
  "Return T if FOLDER is folded in the list of files. FOLDER is a name relative to the root, with the closing slash."
  (and (member (ledger-path ledger folder) (ledger-folds ledger) :test #'string=) t))

(-> ledger-folded (ledger) list)
(defun ledger-folded (ledger)
  "Return the folded folders inside the root of LEDGER, as names relative to the root."
  (let ((root (ledger-root ledger)))
    (sort (loop for path in (ledger-folds ledger)
                when (and (> (length path) (length root))
                          (string= root path :end2 (length root)))
                  collect (subseq path (length root)))
          #'string<)))

(-> ledger-fold (ledger list boolean) boolean)
(defun ledger-fold (ledger folders folded-p)
  "Record each of FOLDERS as folded in the list of files, or as unfolded if FOLDED-P is NIL. Return T if the ledger could be written. If nothing changes, nothing is written."
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
  "Mark the definition with the digest DIGEST in the file NAME as completed if COMPLETED-P is true, or else remove the mark. PRESENT is a list of the current digests of the definitions in the file. Marks for other definitions are removed. Return T if the ledger could be written."
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
