(in-package #:aui)

;;;; -- Ledger --

;;; What a person decided about the files of a checkout, kept outside it so
;;; that the checkout holds nothing but its source. A file marked completed
;;; is remembered with the digest of what it held at that moment. It counts
;;; as completed for as long as it holds exactly that, so a file that anyone
;;; changes afterwards is open again by itself. The folders a person folded
;;; in the list of files are remembered here too. There is one ledger for
;;; each checkout, in a file named after the checkout's folder.

(defvar *ledger-directory* nil
  "The folder ledgers are kept in, or NIL for the person's state folder.")

(defstruct ledger
  "What is remembered about one checkout, as the FILE it is kept in, the checkout's ROOT, the files marked COMPLETED with the digest each had then, as BASELINE the digest each file had when first seen, which is kept only where Git does not say what changed, and the folders that are FOLDED in the list of files, as names from the root with their closing slash."
  (file      nil)
  (root      ""  :type string)
  (completed (make-hash-table :test #'equal) :type hash-table)
  (baseline  (make-hash-table :test #'equal) :type hash-table)
  (folded    nil :type list))

(-> ledger-directory () pathname)
(defun ledger-directory ()
  "Return the folder ledgers are kept in: the one set, else unslop's folder under the person's state folder."
  (or *ledger-directory*
      (let ((state (uiop:getenvp "XDG_STATE_HOME")))
        (merge-pathnames "unslop/"
                         (if state
                             (uiop:ensure-directory-pathname state)
                             (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(-> ledger-pathname (pathname) pathname)
(defun ledger-pathname (root)
  "Return where the ledger of the checkout at ROOT is kept: a file named after the folder and a digest of its whole name, so two folders of one name do not share a ledger."
  (let* ((name   (uiop:native-namestring root))
         (leaf   (or (first (last (pathname-directory root))) "root"))
         (digest (octets-digest (sb-ext:string-to-octets name :external-format ':utf-8))))
    (merge-pathnames (format nil "~A-~A.sexp"
                             (substitute-if-not #\_ #'alphanumericp (if (stringp leaf) leaf "root"))
                             (subseq digest 0 16))
                     (ledger-directory))))

(-> ledger--table (t) hash-table)
(defun ledger--table (pairs)
  "Return a table of PAIRS, a list of (NAME . DIGEST) as a ledger file holds them. Anything else in the list is left out."
  (let ((table (make-hash-table :test #'equal)))
    (when (listp pairs)
      (dolist (pair pairs)
        (when (and (consp pair) (stringp (first pair)) (stringp (rest pair)))
          (setf (gethash (first pair) table) (rest pair)))))
    table))

(-> ledger-open (pathname) ledger)
(defun ledger-open (root)
  "Return the ledger of the checkout at ROOT, read from its file when there is one. A file that does not read gives an empty ledger."
  (let* ((pathname (ledger-pathname root))
         (form     (and (probe-file pathname)
                        (handler-case
                            (with-open-file (stream pathname :external-format ':utf-8)
                              (let ((*read-eval* nil))
                                (read stream nil nil)))
                          (error ()
                            nil))))
         (plist    (and (listp form) (evenp (length form)) form)))
    (make-ledger :file      pathname
                 :root      (uiop:native-namestring root)
                 :completed (ledger--table (getf plist :completed))
                 :baseline  (ledger--table (getf plist :baseline))
                 :folded    (let ((folded (getf plist :folded)))
                              (and (listp folded) (remove-if-not #'stringp folded))))))

(-> ledger--pairs (hash-table) list)
(defun ledger--pairs (table)
  "Return the entries of TABLE as (NAME . DIGEST), sorted by name."
  (sort (loop for name being the hash-keys of table using (hash-value digest)
              collect (cons name digest))
        #'string<
        :key #'first))

(-> ledger-save (ledger) boolean)
(defun ledger-save (ledger)
  "Write LEDGER to its file and return whether that worked. The file is written beside itself and moved into place."
  (let* ((pathname (ledger-file ledger))
         (beside   (make-pathname :type "new" :defaults pathname)))
    (handler-case
        (progn
          (ensure-directories-exist pathname)
          (with-open-file (stream beside :direction ':output :if-exists ':supersede :external-format ':utf-8)
            (with-standard-io-syntax
              (let ((*print-pretty* nil))
                (format stream "(:root ~S~% :completed (~{~S~^~%             ~})~% :baseline (~{~S~^~%            ~})~% :folded (~{~S~^~%          ~}))~%"
                        (ledger-root ledger)
                        (ledger--pairs (ledger-completed ledger))
                        (ledger--pairs (ledger-baseline ledger))
                        (sort (copy-list (ledger-folded ledger)) #'string<)))))
          (uiop:rename-file-overwriting-target beside pathname)
          t)
      (error ()
        nil))))

(-> ledger-completed-p (ledger string (or null string)) boolean)
(defun ledger-completed-p (ledger name digest)
  "Return T when the file NAME was marked completed while it held what DIGEST is the digest of."
  (let ((marked (gethash name (ledger-completed ledger))))
    (and marked digest (string= marked digest))))

(-> ledger-mark (ledger string (or null string)) boolean)
(defun ledger-mark (ledger name digest)
  "Remember that the file NAME is completed while it holds what DIGEST is the digest of, or forget that it was when DIGEST is NIL. Return whether the ledger could be written."
  (if digest
      (setf (gethash name (ledger-completed ledger)) digest)
      (remhash name (ledger-completed ledger)))
  (ledger-save ledger))

(-> ledger-folded-p (ledger string) boolean)
(defun ledger-folded-p (ledger folder)
  "Return T when FOLDER, a name from the checkout's root with its closing slash, is folded in the list of files."
  (and (member folder (ledger-folded ledger) :test #'string=) t))

(-> ledger-fold (ledger list boolean) boolean)
(defun ledger-fold (ledger folders folded-p)
  "Remember each of FOLDERS as folded in the list of files, or as unfolded when FOLDED-P is NIL. Return whether the ledger could be written. Nothing is written when nothing changes."
  (let* ((before (ledger-folded ledger))
         (after  (if folded-p
                     (union before folders :test #'string=)
                     (set-difference before folders :test #'string=))))
    (or (= (length after) (length before))
        (progn
          (setf (ledger-folded ledger) after)
          (ledger-save ledger)))))
