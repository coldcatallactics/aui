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
      (fiveam:is (equal '("a.lisp" "src/deep/c.lisp" "system.asd") (checkout-files checkout)))
      (fiveam:is (null (test-edited-names checkout)))
      (test-write-file (merge-pathnames "a.lisp" directory) "(a changed)")
      (test-write-file (merge-pathnames "src/new.lisp" directory) "(new)")
      (fiveam:is (equal '("a.lisp" "src/new.lisp") (test-edited-names checkout)))
      ;; A new file is listed once the files are listed again.
      (fiveam:is (equal '("a.lisp" "src/deep/c.lisp" "src/new.lisp" "system.asd")
                        (checkout-list-files checkout)))
      (test-git directory "add" "-A")
      (test-git directory "commit" "-q" "-m" "second")
      (fiveam:is (null (test-edited-names checkout))))
    ;; A folder inside the checkout names its files from itself.
    (let ((inner (checkout-open (merge-pathnames "src/" directory))))
      (fiveam:is (equal '("deep/c.lisp" "new.lisp") (checkout-files inner)))
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
