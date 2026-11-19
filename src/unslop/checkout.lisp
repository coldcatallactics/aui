(in-package #:aui)

;;;; -- Checkout --

;;; A checkout is a folder of Lisp source that a person works through. A checkout
;;; has a list of source files and a list of the files that differ from the last
;;; commit. In a Git checkout, both lists come from Git. Files ignored by Git are
;;; not listed, and files count as edited until they are committed. Git is only
;;; asked for information. Nothing here adds, commits or changes anything. A folder
;;; that is not a Git checkout is walked, and the content of each file is recorded
;;; in the ledger when the file is first seen.

(defparameter *source-types* '("lisp" "asd" "cl" "lsp")
  "The file types of Lisp source files.")

(defstruct checkout
  "A folder of Lisp source. ROOT is the folder. GIT-P is true if the folder is in a Git checkout. PREFIX is the path from the top of the repository to the folder. FILES are the source files as names relative to the root, folder by folder."
  (root   nil)
  (git-p  nil :type boolean)
  (prefix ""  :type string)
  (files  nil :type list))

(-> checkout--git (pathname list) (values string boolean))
(defun checkout--git (root arguments)
  "Run Git with ARGUMENTS in the folder ROOT. Return the output and T if Git exited successfully."
  (handler-case
      (multiple-value-bind (output complaint code)
          (uiop:run-program (list* "git" "-C" (uiop:native-namestring root) arguments)
                            :input               nil
                            :output              ':string
                            :error-output        nil
                            :ignore-error-status t)
        (declare (ignore complaint))
        (values (or output "") (eql code 0)))
    (error ()
      (values "" nil))))

(-> checkout--records (string) list)
(defun checkout--records (output)
  "Return the records of OUTPUT. Each record in OUTPUT ends with a null character."
  (loop with start = 0
        for end = (position (code-char 0) output :start start)
        while end
        collect (subseq output start end)
        do (setf start (1+ end))))

(-> checkout--source-p (string) boolean)
(defun checkout--source-p (name)
  "Return T if the type of the file NAME is a type of Lisp source."
  (let ((dot (position #\. name :from-end t)))
    (and dot
         (member (subseq name (1+ dot)) *source-types* :test #'string-equal)
         t)))

(defparameter *walk-ignored-folders* '("quicklisp" "node_modules")
  "The folders that are left out of a walk through a folder that is not a Git checkout. These folders contain libraries by other people. Folders whose names start with a dot are also left out.")

(-> checkout--repository-p (pathname) boolean)
(defun checkout--repository-p (directory)
  "Return T if DIRECTORY is the top of a Git repository."
  (let ((git (merge-pathnames ".git" directory)))
    (and (or (uiop:directory-exists-p (uiop:ensure-directory-pathname git))
             (uiop:file-exists-p git))
         t)))

(-> checkout--tracked (pathname) (or (eql :walk) list))
(defun checkout--tracked (directory)
  "Return the source files of the Git repository at DIRECTORY that are tracked or would be added by Git, as names relative to DIRECTORY. Return :WALK if DIRECTORY is not a repository or if Git fails."
  (if (checkout--repository-p directory)
      (multiple-value-bind (output ok-p)
          (checkout--git directory '("ls-files" "-z" "--cached" "--others" "--exclude-standard"))
        (if ok-p
            (remove-if-not #'checkout--source-p (checkout--records output))
            ':walk))
      ':walk))

(-> checkout--walk (pathname) list)
(defun checkout--walk (root)
  "Return the source files under ROOT as names relative to ROOT. Folders whose names start with a dot are left out, and so are the folders in *WALK-IGNORED-FOLDERS*. In a Git repository under ROOT, only the files that are tracked or would be added by Git are returned. Files ignored by Git are left out."
  (let ((names  nil)
        (prefix (length (uiop:native-namestring root))))
    (uiop:collect-sub*directories
     root
     (constantly t)
     (lambda (directory)
       ;; Return true to walk into the folder. The files of a repository
       ;; are listed here by Git instead.
       (let ((leaf (first (last (pathname-directory directory)))))
         (and (stringp leaf)
              (plusp (length leaf))
              (char/= (char leaf 0) #\.)
              (not (member leaf *walk-ignored-folders* :test #'string=))
              (let ((files (checkout--tracked directory)))
                (or (eq files ':walk)
                    (let ((base (subseq (uiop:native-namestring directory) prefix)))
                      (dolist (name files nil)
                        (when (probe-file (merge-pathnames (uiop:parse-native-namestring name) directory))
                          (push (concatenate 'string base name) names)))))))))
     (lambda (directory)
       (dolist (file (uiop:directory-files directory))
         (let ((name (uiop:native-namestring file)))
           (when (checkout--source-p name)
             (push (subseq name prefix) names))))))
    names))

(-> checkout-folder (string) string)
(defun checkout-folder (name)
  "Return the folder of the file NAME with the closing slash, or the empty string for a file at the root."
  (let ((slash (position #\/ name :from-end t)))
    (if slash (subseq name 0 (1+ slash)) "")))

(-> checkout-folders-above (string) list)
(defun checkout-folders-above (name)
  "Return all folders that contain the file NAME, the outermost first. Each folder is a name relative to the root with the closing slash."
  (loop for slash = (position #\/ name) then (position #\/ name :start (1+ slash))
        while slash
        collect (subseq name 0 (1+ slash))))

(-> checkout--before-p (string string) boolean)
(defun checkout--before-p (first second)
  "Return T if the file FIRST comes before SECOND in the list. Files are sorted by folder and then by name. The files of one folder are together, before the folders inside it."
  (let ((first-folder  (checkout-folder first))
        (second-folder (checkout-folder second)))
    (and (if (string= first-folder second-folder)
             (string< first second)
             (string< first-folder second-folder))
         t)))

(-> checkout-list-files (checkout) list)
(defun checkout-list-files (checkout)
  "Return the current source files of CHECKOUT as names relative to the root, folder by folder. In a Git checkout, these are the files that are tracked or would be added by Git."
  (let ((root (checkout-root checkout)))
    (sort (if (checkout-git-p checkout)
              (remove-if-not (lambda (name)
                               (and (checkout--source-p name)
                                    (probe-file (merge-pathnames (uiop:parse-native-namestring name) root))))
                             (checkout--records
                              (checkout--git root '("ls-files" "-z" "--cached" "--others" "--exclude-standard"))))
              (checkout--walk root))
          #'checkout--before-p)))

(-> checkout-open (pathname) checkout)
(defun checkout-open (directory)
  "Return the checkout of the folder DIRECTORY with the list of source files. Signal a PROOF-ERROR if the folder does not exist."
  (let ((root (uiop:directory-exists-p (uiop:ensure-directory-pathname directory))))
    (unless root
      (error 'proof-error
             :reason  ':unreadable
             :message (format nil "~A is not a folder" (uiop:native-namestring directory))))
    (let ((root (truename root)))
      (multiple-value-bind (prefix git-p) (checkout--git root '("rev-parse" "--show-prefix"))
        (let ((checkout (make-checkout :root   root
                                       :git-p  git-p
                                       :prefix (string-right-trim '(#\Newline #\Return) prefix))))
          (setf (checkout-files checkout) (checkout-list-files checkout))
          checkout)))))

(-> checkout-file (checkout string) pathname)
(defun checkout-file (checkout name)
  "Return the pathname of the file NAME of CHECKOUT."
  (merge-pathnames (uiop:parse-native-namestring name) (checkout-root checkout)))

(-> checkout-edited (checkout) (or null hash-table))
(defun checkout-edited (checkout)
  "Return the files of CHECKOUT that differ from the last commit, as a hash table from the names to T. Return NIL if the folder is not in a Git checkout or if Git fails.

Files that are not tracked by Git yet count as differing."
  (when (checkout-git-p checkout)
    (multiple-value-bind (output ok-p)
        (checkout--git (checkout-root checkout)
                       '("status" "--porcelain=v1" "-z" "--untracked-files=all" "--" "."))
      (when ok-p
        (let ((edited  (make-hash-table :test #'equal))
              (prefix  (checkout-prefix checkout))
              (records (checkout--records output)))
          (loop while records
                do (let* ((record (pop records))
                          (state  (subseq record 0 (min 2 (length record))))
                          (path   (if (> (length record) 3) (subseq record 3) "")))
                     ;; For a rename or a copy, the original file is in a separate record.
                     (when (find-if (lambda (letter) (member letter '(#\R #\C))) state)
                       (pop records))
                     (when (and (>= (length path) (length prefix))
                                (string= prefix path :end2 (length prefix)))
                       (setf (gethash (subseq path (length prefix)) edited) t))))
          edited)))))
