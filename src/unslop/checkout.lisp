(in-package #:aui)

;;;; -- Checkout --

;;; The folder of Lisp source a person works through: which source files it
;;; holds and which of them differ from what was last committed. In a Git
;;; checkout both come from Git, so what Git ignores is not listed and a file
;;; counts as edited until it is committed. Git is only ever asked; nothing
;;; here adds, commits or changes anything. A folder that is no Git checkout
;;; is walked, and the ledger remembers what its files held when first seen.

(defparameter *source-types* '("lisp" "asd" "cl" "lsp")
  "The file types that hold Lisp source.")

(defstruct checkout
  "A folder of Lisp source, as its ROOT, whether Git knows it, the PREFIX that leads from the top of its repository to it, and its source FILES as names from the root, folder by folder."
  (root   nil)
  (git-p  nil :type boolean)
  (prefix ""  :type string)
  (files  nil :type list))

(-> checkout--git (pathname list) (values string boolean))
(defun checkout--git (root arguments)
  "Run Git on ARGUMENTS in the folder ROOT and return what it printed and whether it ended well."
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
  "Return the records of OUTPUT, which Git ended with a null character each."
  (loop with start = 0
        for end = (position (code-char 0) output :start start)
        while end
        collect (subseq output start end)
        do (setf start (1+ end))))

(-> checkout--source-p (string) boolean)
(defun checkout--source-p (name)
  "Return T when the file NAME holds Lisp source, going by its type."
  (let ((dot (position #\. name :from-end t)))
    (and dot
         (member (subseq name (1+ dot)) *source-types* :test #'string-equal)
         t)))

(defparameter *walk-ignored-folders* '("quicklisp" "node_modules")
  "The folders a walk through a folder Git does not know leaves out, besides those whose names start with a dot: they hold libraries of others.")

(-> checkout--repository-p (pathname) boolean)
(defun checkout--repository-p (directory)
  "Return T when DIRECTORY is the top of a Git repository."
  (let ((git (merge-pathnames ".git" directory)))
    (and (or (uiop:directory-exists-p (uiop:ensure-directory-pathname git))
             (uiop:file-exists-p git))
         t)))

(-> checkout--tracked (pathname) (or (eql :walk) list))
(defun checkout--tracked (directory)
  "Return the source files of the Git repository at DIRECTORY that Git tracks or would add, as names from it, or :WALK when DIRECTORY is no repository or Git does not answer."
  (if (checkout--repository-p directory)
      (multiple-value-bind (output ok-p)
          (checkout--git directory '("ls-files" "-z" "--cached" "--others" "--exclude-standard"))
        (if ok-p
            (remove-if-not #'checkout--source-p (checkout--records output))
            ':walk))
      ':walk))

(-> checkout--walk (pathname) list)
(defun checkout--walk (root)
  "Return the source files beneath ROOT as names from it. Folders whose names start with a dot are left out, and so are those of *WALK-IGNORED-FOLDERS*. A Git repository beneath ROOT gives the files Git tracks or would add, so what it ignores is left out too."
  (let ((names  nil)
        (prefix (length (uiop:native-namestring root))))
    (uiop:collect-sub*directories
     root
     (constantly t)
     (lambda (directory)
       ;; A folder entered is walked; a repository is listed here by Git instead.
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
  "Return the folder the file NAME lies in, with its closing slash, or the empty string for a file at the root."
  (let ((slash (position #\/ name :from-end t)))
    (if slash (subseq name 0 (1+ slash)) "")))

(-> checkout-folders-above (string) list)
(defun checkout-folders-above (name)
  "Return every folder the file NAME lies in, the outermost first, each as a name from the root with its closing slash."
  (loop for slash = (position #\/ name) then (position #\/ name :start (1+ slash))
        while slash
        collect (subseq name 0 (1+ slash))))

(-> checkout--before-p (string string) boolean)
(defun checkout--before-p (first second)
  "Return T when the file FIRST is listed before SECOND. Files are in the order of their folders and then of their names, so those of one folder stand together, ahead of the folders inside it."
  (let ((first-folder  (checkout-folder first))
        (second-folder (checkout-folder second)))
    (and (if (string= first-folder second-folder)
             (string< first second)
             (string< first-folder second-folder))
         t)))

(-> checkout-list-files (checkout) list)
(defun checkout-list-files (checkout)
  "Return the source files CHECKOUT holds now, as names from its root, folder by folder. In a Git checkout they are the files Git tracks and those it would add."
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
  "Return the checkout of the folder DIRECTORY with its source files listed. Signal a PROOF-ERROR when there is no such folder."
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
  "Return the files of CHECKOUT that differ from what was last committed, as a table from their names to T, or NIL when Git does not know the folder or does not answer.

A file Git does not track yet counts as differing."
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
                     ;; A rename or a copy names the file it came from in a record of its own.
                     (when (find-if (lambda (letter) (member letter '(#\R #\C))) state)
                       (pop records))
                     (when (and (>= (length path) (length prefix))
                                (string= prefix path :end2 (length prefix)))
                       (setf (gethash (subseq path (length prefix)) edited) t))))
          edited)))))
