(in-package #:aui)

;;;; -- Unslop: Checkout and Ledger Tests --

(fiveam:in-suite unslop)

(-> test-git (pathname &rest string) null)
(defun test-git (directory &rest arguments)
  "Run Git on ARGUMENTS in DIRECTORY as a person with no configuration of their own."
  (uiop:run-program (append (list "git" "-C" (uiop:native-namestring directory)
                                  "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                  "-c" "commit.gpgsign=false" "-c" "init.defaultBranch=main")
                            arguments)
                    :input nil :output nil :error-output nil)
  nil)

(-> test-edited-names (checkout) list)
(defun test-edited-names (checkout)
  "Return the names of the files of CHECKOUT that Git says differ from what was committed, sorted."
  (let ((edited (checkout-edited checkout)))
    (and edited
         (sort (loop for name being the hash-keys of edited collect name) #'string<))))

(fiveam:test a-git-checkout-lists-its-source-and-says-what-changed
  (with-unslop-folder (directory)
    (test-write-file (merge-pathnames "a.lisp" directory) "(a)")
    (test-write-file (merge-pathnames "system.asd" directory) "(b)")
    (test-write-file (merge-pathnames "src/deep/c.lisp" directory) "(c)")
    (test-write-file (merge-pathnames "notes.txt" directory) "not source")
    (test-write-file (merge-pathnames "build/made.lisp" directory) "(made)")
    (test-write-file (merge-pathnames ".gitignore" directory) (format nil "build/~%"))
    (test-git directory "init" "-q")
    (test-git directory "add" "-A")
    (test-git directory "commit" "-q" "-m" "first")
    (let ((checkout (checkout-open directory)))
      (fiveam:is (checkout-git-p checkout))
      ;; What Git ignores is not listed, and neither is what is not Lisp.
      (fiveam:is (equal '("a.lisp" "system.asd" "src/deep/c.lisp") (checkout-files checkout)))
      (fiveam:is (null (test-edited-names checkout)))
      (test-write-file (merge-pathnames "a.lisp" directory) "(a changed)")
      (test-write-file (merge-pathnames "src/new.lisp" directory) "(new)")
      (fiveam:is (equal '("a.lisp" "src/new.lisp") (test-edited-names checkout)))
      ;; A new file is listed once the files are listed again.
      (fiveam:is (equal '("a.lisp" "system.asd" "src/new.lisp" "src/deep/c.lisp")
                        (checkout-list-files checkout)))
      (test-git directory "add" "-A")
      (test-git directory "commit" "-q" "-m" "second")
      (fiveam:is (null (test-edited-names checkout))))
    ;; A folder inside the checkout names its files from itself.
    (let ((inner (checkout-open (merge-pathnames "src/" directory))))
      (fiveam:is (equal '("new.lisp" "deep/c.lisp") (checkout-files inner)))
      (test-write-file (merge-pathnames "src/deep/c.lisp" directory) "(c changed)")
      (test-write-file (merge-pathnames "a.lisp" directory) "(a again)")
      (fiveam:is (equal '("deep/c.lisp") (test-edited-names inner))))))

(fiveam:test a-folder-git-does-not-know-is-walked
  (with-unslop-folder (directory)
    (test-write-file (merge-pathnames "a.lisp" directory) "(a)")
    (test-write-file (merge-pathnames "src/b.cl" directory) "(b)")
    (test-write-file (merge-pathnames ".hidden/c.lisp" directory) "(c)")
    (test-write-file (merge-pathnames "readme.md" directory) "words")
    ;; The folder may lie inside a checkout of the person's; Git is kept from finding one.
    (let ((checkout (make-checkout :root (truename directory))))
      (fiveam:is (equal '("a.lisp" "src/b.cl") (checkout-list-files checkout)))
      (fiveam:is (null (checkout-edited checkout)))
      (fiveam:is (equal (merge-pathnames "src/b.cl" (truename directory)) (checkout-file checkout "src/b.cl")))))
  (fiveam:signals proof-error (checkout-open #p"/no/such/folder/anywhere/")))

(fiveam:test a-file-is-completed-for-as-long-as-it-holds-what-it-held
  (with-unslop-folder (directory)
    (let ((*ledger-directory* (merge-pathnames "state/" directory))
          (root               (merge-pathnames "work/project/" directory))
          (other              (merge-pathnames "else/project/" directory)))
      (let ((ledger (ledger-open root)))
        (fiveam:is (not (ledger-completed-p ledger "a.lisp" "one")))
        (fiveam:is (ledger-mark ledger "a.lisp" "one"))
        (fiveam:is (ledger-mark ledger "src/b.lisp" "two"))
        (fiveam:is (ledger-completed-p ledger "a.lisp" "one"))
        ;; The file changed: it is open again.
        (fiveam:is (not (ledger-completed-p ledger "a.lisp" "other")))
        (fiveam:is (not (ledger-completed-p ledger "a.lisp" nil))))
      ;; It is remembered outside the checkout, for that checkout alone.
      (let ((again (ledger-open root)))
        (fiveam:is (ledger-completed-p again "a.lisp" "one"))
        (fiveam:is (ledger-completed-p again "src/b.lisp" "two"))
        (fiveam:is (ledger-mark again "a.lisp" nil))
        (fiveam:is (not (ledger-completed-p (ledger-open root) "a.lisp" "one")))
        (fiveam:is (ledger-completed-p (ledger-open root) "src/b.lisp" "two")))
      (fiveam:is (not (ledger-completed-p (ledger-open other) "src/b.lisp" "two")))
      (fiveam:is (not (equal (ledger-pathname root) (ledger-pathname other))))
      (fiveam:is (null (uiop:directory-files root))))))

(fiveam:test a-digest-tells-two-files-apart
  (with-unslop-folder (directory)
    (let ((one (test-write-file (merge-pathnames "one.lisp" directory) "(a)"))
          (two (test-write-file (merge-pathnames "two.lisp" directory) "(b)")))
      (fiveam:is (string= (file-digest one) (file-digest one)))
      (fiveam:is (string/= (file-digest one) (file-digest two)))
      (fiveam:is (null (file-digest (merge-pathnames "absent.lisp" directory)))))))

(defmacro with-test-preferences (() &body body)
  "Run BODY with preferences of its own that are kept nowhere."
  `(let ((*preferences*           nil)
         (*preferences-read-p*    t)
         (*preferences-read-from* nil)
         (*preferences-pathname*  nil))
     ,@body))

(-> test-desk (pathname list) desk)
(defun test-desk (directory files)
  "Return a desk without a window for a checkout at DIRECTORY that holds the files named FILES."
  (make-instance 'desk
                 :checkout (make-checkout :root directory :files files)
                 :ledger   (ledger-open directory)
                 :files    files))

(-> test-shelf-rows (desk) list)
(defun test-shelf-rows (desk)
  "Return the rows of DESK's list of files, each as a list of its kind, its label and its depth."
  (mapcar (lambda (row)
            (list (shelf-row-kind row) (shelf-row-label row) (shelf-row-depth row)))
          (shelf-rows desk)))

(fiveam:test the-list-of-files-is-narrowed-and-grouped
  (with-unslop-folder (directory)
    (with-test-preferences ()
      (let* ((*ledger-directory* (merge-pathnames "state/" directory))
             (desk               (test-desk directory '("a.lisp" "src/b.lisp" "src/c.lisp" "tests/d.lisp"))))
        (fiveam:is (equal '((:file "a.lisp" 0) (:folder "src/" 0) (:file "b.lisp" 1) (:file "c.lisp" 1)
                            (:folder "tests/" 0) (:file "d.lisp" 1))
                          (test-shelf-rows desk)))
        (fiveam:is (string= "src/" (checkout-folder "src/b.lisp")))
        (fiveam:is (string= "" (checkout-folder "a.lisp")))
        ;; A folder's own files stand together, ahead of the folders inside it.
        (fiveam:is (equal '("a.lisp" "z.lisp" "src/a.lisp" "src/z.lisp" "src/deep/c.lisp")
                          (sort (list "src/z.lisp" "z.lisp" "src/deep/c.lisp" "a.lisp" "src/a.lisp")
                                #'checkout--before-p)))
        (setf (desk-filter desk) "SRC")
        (fiveam:is (equal '("src/b.lisp" "src/c.lisp") (shelf-names desk)))
        (setf (desk-filter desk) "")
        ;; Completed files are left out when the person hides them, but never the open one.
        (setf (gethash "src/c.lisp" (desk-completed desk)) t
              (gethash "a.lisp" (desk-completed desk))     t)
        (fiveam:is (= 2 (desk-completed-count desk)))
        (fiveam:is (= 4 (length (shelf-names desk))))
        (setf (preference ':hide-completed) t)
        (fiveam:is (equal '("src/b.lisp" "tests/d.lisp") (shelf-names desk)))
        (setf (desk-name desk) "src/c.lisp")
        (fiveam:is (equal '("src/b.lisp" "src/c.lisp" "tests/d.lisp") (shelf-names desk)))
        ;; A folder with nothing left to list is not listed.
        (setf (gethash "tests/d.lisp" (desk-completed desk)) t)
        (fiveam:is (equal '((:folder "src/" 0) (:file "b.lisp" 1) (:file "c.lisp" 1))
                          (test-shelf-rows desk)))))))

(fiveam:test the-list-of-files-is-a-tree-that-folds
  (with-unslop-folder (directory)
    (with-test-preferences ()
      (let* ((*ledger-directory* (merge-pathnames "state/" directory))
             (files              '("a.lisp" "lib/deep/only/x.lisp" "lib/deep/only/y.lisp" "src/main.lisp"
                                   "src/model/m.lisp" "src/view/sub/v.lisp"))
             (desk               (test-desk directory files)))
        (dolist (name files)
          (test-write-file (merge-pathnames name directory) "(form)"))
        ;; A folder stands ahead of what lies in it, and folders that hold one folder and nothing else share a row.
        (fiveam:is (equal '((:file "a.lisp" 0)
                            (:folder "lib/deep/only/" 0) (:file "x.lisp" 1) (:file "y.lisp" 1)
                            (:folder "src/" 0) (:file "main.lisp" 1)
                            (:folder "model/" 1) (:file "m.lisp" 2)
                            (:folder "view/sub/" 1) (:file "v.lisp" 2))
                          (test-shelf-rows desk)))
        (fiveam:is (equal '("lib/" "lib/deep/" "lib/deep/only/") (checkout-folders-above "lib/deep/only/x.lisp")))
        (fiveam:is (null (checkout-folders-above "a.lisp")))
        ;; A folder counts the files in it and below, and those that are completed.
        (setf (gethash "src/model/m.lisp" (desk-completed desk)) t)
        (let ((row (find "src/" (shelf-rows desk) :key #'shelf-row-name :test #'string=)))
          (fiveam:is (= 3 (shelf-row-files row)))
          (fiveam:is (= 1 (shelf-row-completed row)))
          (fiveam:is (not (shelf-row-folded-p row))))
        ;; What lies in a folded folder is left out, and the next window knows the folder is folded.
        (desk-toggle-folder desk "src/")
        (fiveam:is (equal '((:file "a.lisp" 0)
                            (:folder "lib/deep/only/" 0) (:file "x.lisp" 1) (:file "y.lisp" 1)
                            (:folder "src/" 0))
                          (test-shelf-rows desk)))
        (fiveam:is (shelf-row-folded-p (first (last (shelf-rows desk)))))
        (fiveam:is (ledger-folded-p (ledger-open directory) "src/"))
        ;; While the list is narrowed by name nothing is folded.
        (setf (desk-filter desk) "model")
        (fiveam:is (equal '((:folder "src/" 0) (:folder "model/" 1) (:file "m.lisp" 2))
                          (test-shelf-rows desk)))
        (fiveam:is (notany #'shelf-row-folded-p (shelf-rows desk)))
        (setf (desk-filter desk) "")
        ;; A file that is opened is in view, so its folders are unfolded.
        (desk-toggle-folder desk "src/view/sub/")
        (fiveam:is (desk-folded-p desk "src/view/sub/"))
        (desk-open-file desk "src/view/sub/v.lisp")
        (fiveam:is (not (desk-folded-p desk "src/")))
        (fiveam:is (not (desk-folded-p desk "src/view/sub/")))
        (fiveam:is (null (ledger-folded (ledger-open directory))))
        (desk-toggle-folder desk "lib/deep/only/")
        (fiveam:is (equal '("lib/deep/only/") (ledger-folded (ledger-open directory))))))))

(fiveam:test a-comment-is-found-again-and-so-is-what-follows-a-place
  (with-unslop-folder (directory)
    (with-test-preferences ()
      (let* ((*ledger-directory* (merge-pathnames "state/" directory))
             (desk               (test-desk directory '("a.lisp"))))
        (test-write-file (merge-pathnames "a.lisp" directory)
                         (format nil ";; One.~%(defun a ()~%  \"Doc.\"~%  ;; Two.~%  1)~%~%;; One.~%"))
        (desk-open-file desk "a.lisp")
        (let* ((proof (desk-proof desk))
               (above (proof-passage proof 0 0))
               (below (proof-passage proof 1 0)))
          (fiveam:is (string= "One." (passage-text above)))
          (fiveam:is (string= "One." (passage-text below)))
          ;; Of two passages with the same words, it is the one where the passage stood.
          (fiveam:is (equal '(0 . 0) (desk--find-again desk above)))
          (fiveam:is (equal '(1 . 0) (desk--find-again desk below)))
          ;; What follows a place in the text, among the kinds that are shown.
          (fiveam:is (equal '(0 . 0) (desk--passage-from desk 0)))
          (fiveam:is (equal '(0 . 1) (desk--passage-from desk 1)))
          (setf (preference ':hidden-kinds) '(:docstring :string))
          (fiveam:is (equal '(0 . 2) (desk--passage-from desk 1)))
          (fiveam:is (null (desk--passage-from desk (length (proof-text proof)))))
          ;; Only a comment is deleted, and nothing can be brought back before one was.
          (fiveam:is (not (desk-delete-comment desk 0 1)))
          (fiveam:is (search "only a comment" (desk-notice desk)))
          (fiveam:is (null (desk-removal desk)))
          (fiveam:is (string= (proof-text proof)
                              (uiop:read-file-string (merge-pathnames "a.lisp" directory)))))))))

(fiveam:test the-list-is-as-wide-as-its-share-of-the-window
  (fiveam:is (= 420 (shelf-width 1400)))
  (fiveam:is (= 768 (shelf-width 2560)))
  (fiveam:is (= 300 (shelf-width 900)))
  (fiveam:is (= 240 (shelf-width 600))))

(fiveam:test kinds-of-text-are-switched-off-but-never-all
  (with-unslop-folder (directory)
    (with-test-preferences ()
      (let* ((*ledger-directory* (merge-pathnames "state/" directory))
             (desk               (test-desk directory nil)))
        (fiveam:is (equal '(:comment :docstring :string) (desk-kinds)))
        (desk-toggle-kind desk ':string)
        (desk-toggle-kind desk ':comment)
        (fiveam:is (equal '(:docstring) (desk-kinds)))
        ;; The last kind shown stays.
        (desk-toggle-kind desk ':docstring)
        (fiveam:is (equal '(:docstring) (desk-kinds)))
        (desk-toggle-kind desk ':string)
        (fiveam:is (equal '(:docstring :string) (desk-kinds)))))))

(fiveam:test the-command-line-names-a-folder-and-little-else
  (let ((launch (unslop-parse '("/tmp/work" "--file" "src/a.lisp" "--size" "1200x800" "--shot" "/tmp/p.png"))))
    (fiveam:is (string= "/tmp/work" (unslop-launch-directory launch)))
    (fiveam:is (string= "src/a.lisp" (unslop-launch-file launch)))
    (fiveam:is (equal '(1200 . 800) (unslop-launch-size launch)))
    (fiveam:is (string= "/tmp/p.png" (unslop-launch-shot launch))))
  ;; Without a folder it is the one the command was started in.
  (fiveam:is (string= (uiop:native-namestring (uiop:getcwd)) (unslop-launch-directory (unslop-parse nil))))
  (fiveam:signals proof-error (unslop-parse '("--commit")))
  (fiveam:signals proof-error (unslop-parse '("one" "two")))
  (fiveam:signals proof-error (unslop-parse '("--size" "big")))
  (fiveam:signals proof-error (unslop-parse '("--file"))))

(fiveam:test a-text-that-would-show-nothing-is-described
  (fiveam:is (string= "empty" (page--unseen-words "")))
  (fiveam:is (string= "1 blank" (page--unseen-words " ")))
  (fiveam:is (string= "3 blanks" (page--unseen-words "   ")))
  (fiveam:is (string= "2 line ends" (page--unseen-words (format nil "~%~%"))))
  (fiveam:is (string= "blanks and line ends" (page--unseen-words (format nil " ~% "))))
  (fiveam:is (null (page--unseen-words " a "))))
