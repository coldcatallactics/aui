(in-package #:aui)

;;;; -- Unslop: Checkout and Ledger Tests --

(fiveam:in-suite unslop)

(-> test-git (pathname &rest string) null)
(defun test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and a fixed test configuration."
  (uiop:run-program (append (list "git" "-C" (uiop:native-namestring directory)
                                  "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                  "-c" "commit.gpgsign=false" "-c" "init.defaultBranch=main")
                            arguments)
                    :input nil :output nil :error-output nil)
  nil)

(-> test-edited-names (checkout) list)
(defun test-edited-names (checkout)
  "Return the sorted names of the files in CHECKOUT that differ from the last commit."
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
      ;; Files ignored by Git are not listed. Files that are not Lisp source are not listed either.
      (fiveam:is (equal '("a.lisp" "system.asd" "src/deep/c.lisp") (checkout-files checkout)))
      (fiveam:is (null (test-edited-names checkout)))
      (test-write-file (merge-pathnames "a.lisp" directory) "(a changed)")
      (test-write-file (merge-pathnames "src/new.lisp" directory) "(new)")
      (fiveam:is (equal '("a.lisp" "src/new.lisp") (test-edited-names checkout)))
      ;; A new file is listed after the files are listed again.
      (fiveam:is (equal '("a.lisp" "system.asd" "src/new.lisp" "src/deep/c.lisp")
                        (checkout-list-files checkout)))
      (test-git directory "add" "-A")
      (test-git directory "commit" "-q" "-m" "second")
      (fiveam:is (null (test-edited-names checkout))))
    ;; The file names of a folder inside the checkout are relative to that folder.
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
    (test-write-file (merge-pathnames "quicklisp/dists/x/y.lisp" directory) "(y)")
    (test-write-file (merge-pathnames "src/quicklisp/z.lisp" directory) "(z)")
    (test-write-file (merge-pathnames "readme.md" directory) "words")
    ;; The folder can be inside a Git checkout of the person.
    ;; The checkout is made without a search for Git, so that outer checkout is not found.
    (let ((checkout (make-checkout :root (truename directory))))
      (fiveam:is (equal '("a.lisp" "src/b.cl") (checkout-list-files checkout)))
      (fiveam:is (null (checkout-edited checkout)))
      ;; From a repository inside the folder, tracked and untracked files are listed.
      ;; Files ignored by Git are not listed.
      (let ((repository (merge-pathnames "repo/" directory)))
        (test-write-file (merge-pathnames "r.lisp" repository) "(r)")
        (test-write-file (merge-pathnames "lib/s.lisp" repository) "(s)")
        (test-write-file (merge-pathnames "build/made.lisp" repository) "(made)")
        (test-write-file (merge-pathnames ".gitignore" repository) (format nil "build/~%"))
        (test-git repository "init" "-q")
        (test-git repository "add" "r.lisp" ".gitignore")
        (test-git repository "commit" "-q" "-m" "first")
        (test-write-file (merge-pathnames "empty/.gitignore" directory) "x")
        (test-git (merge-pathnames "empty/" directory) "init" "-q")
        (test-write-file (merge-pathnames "empty/ignored.lisp" directory) "(i)")
        (test-write-file (merge-pathnames "empty/.gitignore" directory) (format nil "*.lisp~%")))
      (fiveam:is (equal '("a.lisp" "repo/r.lisp" "repo/lib/s.lisp" "src/b.cl") (checkout-list-files checkout)))
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
        ;; The file has changed, so it is not completed any more.
        (fiveam:is (not (ledger-completed-p ledger "a.lisp" "other")))
        (fiveam:is (not (ledger-completed-p ledger "a.lisp" nil))))
      ;; The mark is stored outside the checkout, under the full path of the file.
      (let ((again (ledger-open root)))
        (fiveam:is (ledger-completed-p again "a.lisp" "one"))
        (fiveam:is (ledger-completed-p again "src/b.lisp" "two"))
        (fiveam:is (ledger-mark again "a.lisp" nil))
        (fiveam:is (not (ledger-completed-p (ledger-open root) "a.lisp" "one")))
        (fiveam:is (ledger-completed-p (ledger-open root) "src/b.lisp" "two")))
      (fiveam:is (not (ledger-completed-p (ledger-open other) "src/b.lisp" "two")))
      ;; A ledger for a folder above the checkout or inside it has the same marks.
      (fiveam:is (ledger-completed-p (ledger-open (merge-pathnames "work/" directory)) "project/src/b.lisp" "two"))
      (fiveam:is (ledger-completed-p (ledger-open (merge-pathnames "work/project/src/" directory)) "b.lisp" "two"))
      (fiveam:is (equal '(("src/b.lisp" . "two"))
                        (let ((ledger (ledger-open root)))
                          (ledger-entries ledger (ledger-completed ledger)))))
      ;; If two ledgers are open at the same time, the marks of both are kept.
      (let ((one (ledger-open root))
            (two (ledger-open root)))
        (fiveam:is (ledger-mark one "c.lisp" "three"))
        (fiveam:is (ledger-mark two "d.lisp" "four"))
        (let ((after (ledger-open root)))
          (fiveam:is (ledger-completed-p after "c.lisp" "three"))
          (fiveam:is (ledger-completed-p after "d.lisp" "four"))))
      (fiveam:is (null (uiop:directory-files root))))))

(fiveam:test the-ledgers-of-one-folder-from-before-are-taken-in-once
  (with-unslop-folder (directory)
    (let* ((*ledger-directory* (merge-pathnames "state/" directory))
           (old                (merge-pathnames "mag-DC3EF141C495680A.sexp" *ledger-directory*)))
      ;; This file has the format of the first ledgers. The strings are written in the #A form.
      (test-write-file old (format nil "(:root #A((10) BASE-CHAR . \"/home/mag/\")~% :completed ((#A((22) BASE-CHAR . \"common-lisp/a/one.lisp\") . \"D1\")~%  (\"common-lisp/b/two.lisp\" . \"D2\"))~% :baseline ((\"common-lisp/a/one.lisp\" . \"B1\"))~% :folded (\"common-lisp/b/\"))~%"))
      (let ((ledger (ledger-open #p"/home/mag/common-lisp/")))
        (fiveam:is (ledger-completed-p ledger "a/one.lisp" "D1"))
        (fiveam:is (ledger-completed-p ledger "b/two.lisp" "D2"))
        (fiveam:is (ledger-folded-p ledger "b/"))
        (fiveam:is (equal '(("a/one.lisp" . "B1")) (ledger-entries ledger (ledger-baseline ledger))))
        ;; A mark that is removed does not come back from the old ledger.
        ;; The old ledger file is not deleted.
        (fiveam:is (ledger-mark ledger "a/one.lisp" nil)))
      (fiveam:is (probe-file old))
      (let ((again (ledger-open #p"/home/mag/")))
        (fiveam:is (not (ledger-completed-p again "common-lisp/a/one.lisp" "D1")))
        (fiveam:is (ledger-completed-p again "common-lisp/b/two.lisp" "D2")))
      ;; The new ledger has plain strings.
      (fiveam:is (null (search "#A" (uiop:read-file-string (ledger-pathname))))))))

(fiveam:test a-digest-tells-two-files-apart
  (with-unslop-folder (directory)
    (let ((one (test-write-file (merge-pathnames "one.lisp" directory) "(a)"))
          (two (test-write-file (merge-pathnames "two.lisp" directory) "(b)")))
      (fiveam:is (string= (file-digest one) (file-digest one)))
      (fiveam:is (string/= (file-digest one) (file-digest two)))
      (fiveam:is (null (file-digest (merge-pathnames "absent.lisp" directory)))))))

(defmacro with-test-preferences (() &body body)
  "Run BODY with new preferences that are not saved to a file."
  `(let ((*preferences*           nil)
         (*preferences-read-p*    t)
         (*preferences-read-from* nil)
         (*preferences-pathname*  nil))
     ,@body))

(-> test-desk (pathname list) desk)
(defun test-desk (directory files)
  "Return a desk without a window.
The checkout of the desk is at DIRECTORY and has the files FILES."
  (make-instance 'desk
                 :checkout (make-checkout :root directory :files files)
                 :ledger   (ledger-open directory)
                 :files    files))

(-> test-shelf-rows (desk) list)
(defun test-shelf-rows (desk)
  "Return the rows of the file list of DESK.
Each row is a list of the kind, the label and the depth of the row."
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
        ;; The files directly in a folder are sorted before the folders inside it.
        (fiveam:is (equal '("a.lisp" "z.lisp" "src/a.lisp" "src/z.lisp" "src/deep/c.lisp")
                          (sort (list "src/z.lisp" "z.lisp" "src/deep/c.lisp" "a.lisp" "src/a.lisp")
                                #'checkout--before-p)))
        (setf (desk-filter desk) "SRC")
        (fiveam:is (equal '("src/b.lisp" "src/c.lisp") (shelf-names desk)))
        (setf (desk-filter desk) "")
        ;; If the person hides completed files, they are left out.
        ;; However, the open file is always listed.
        (setf (gethash "src/c.lisp" (desk-completed desk)) t
              (gethash "a.lisp" (desk-completed desk))     t)
        (fiveam:is (= 2 (desk-completed-count desk)))
        (fiveam:is (= 4 (length (shelf-names desk))))
        (setf (preference ':hide-completed) t)
        (fiveam:is (equal '("src/b.lisp" "tests/d.lisp") (shelf-names desk)))
        (setf (desk-name desk) "src/c.lisp")
        (fiveam:is (equal '("src/b.lisp" "src/c.lisp" "tests/d.lisp") (shelf-names desk)))
        ;; A folder that has no files left to list is not listed.
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
        ;; A folder is listed before the files in it.
        ;; Folders that only contain one folder share a row.
        (fiveam:is (equal '((:file "a.lisp" 0)
                            (:folder "lib/deep/only/" 0) (:file "x.lisp" 1) (:file "y.lisp" 1)
                            (:folder "src/" 0) (:file "main.lisp" 1)
                            (:folder "model/" 1) (:file "m.lisp" 2)
                            (:folder "view/sub/" 1) (:file "v.lisp" 2))
                          (test-shelf-rows desk)))
        (fiveam:is (equal '("lib/" "lib/deep/" "lib/deep/only/") (checkout-folders-above "lib/deep/only/x.lisp")))
        (fiveam:is (null (checkout-folders-above "a.lisp")))
        ;; The row of a folder has the count of all files below the folder
        ;; and the count of the completed files.
        (setf (gethash "src/model/m.lisp" (desk-completed desk)) t)
        (let ((row (find "src/" (shelf-rows desk) :key #'shelf-row-name :test #'string=)))
          (fiveam:is (= 3 (shelf-row-files row)))
          (fiveam:is (= 1 (shelf-row-completed row)))
          (fiveam:is (not (shelf-row-folded-p row))))
        ;; The files in a folded folder are left out.
        ;; The fold is saved in the ledger for the next window.
        (desk-toggle-folder desk "src/")
        (fiveam:is (equal '((:file "a.lisp" 0)
                            (:folder "lib/deep/only/" 0) (:file "x.lisp" 1) (:file "y.lisp" 1)
                            (:folder "src/" 0))
                          (test-shelf-rows desk)))
        (fiveam:is (shelf-row-folded-p (first (last (shelf-rows desk)))))
        (fiveam:is (ledger-folded-p (ledger-open directory) "src/"))
        ;; While the list is filtered by name, no folder is folded.
        (setf (desk-filter desk) "model")
        (fiveam:is (equal '((:folder "src/" 0) (:folder "model/" 1) (:file "m.lisp" 2))
                          (test-shelf-rows desk)))
        (fiveam:is (notany #'shelf-row-folded-p (shelf-rows desk)))
        (setf (desk-filter desk) "")
        ;; If a file is opened, the folders above it are unfolded.
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
          ;; If two passages have the same words, the passage at the old position is found.
          (fiveam:is (equal '(0 . 0) (desk--find-again desk above)))
          (fiveam:is (equal '(1 . 0) (desk--find-again desk below)))
          ;; The first passage after a position in the text is found among the kinds that are shown.
          (fiveam:is (equal '(0 . 0) (desk--passage-from desk 0)))
          (fiveam:is (equal '(0 . 1) (desk--passage-from desk 1)))
          (setf (preference ':hidden-kinds) '(:docstring :string))
          (fiveam:is (equal '(0 . 2) (desk--passage-from desk 1)))
          (fiveam:is (null (desk--passage-from desk (length (proof-text proof)))))
          ;; Only comments can be deleted.
          ;; Nothing can be restored before a comment has been deleted.
          (fiveam:is (not (desk-delete-comment desk 0 1)))
          (fiveam:is (search "only a comment" (desk-notice desk)))
          (fiveam:is (null (desk-removal desk)))
          (fiveam:is (string= (proof-text proof)
                              (uiop:read-file-string (merge-pathnames "a.lisp" directory)))))))))

(fiveam:test a-section-is-completed-for-as-long-as-its-words-stay
  (with-unslop-folder (directory)
    (with-test-preferences ()
      (let* ((*ledger-directory* (merge-pathnames "state/" directory))
             (file               (merge-pathnames "a.lisp" directory))
             (source             (format nil ";; Top.~%~%(defun a ()~%  \"Doc a.\"~%  1)~%~%(defun b ()~%  \"Doc b.\"~%  2)~%"))
             (desk               (test-desk directory '("a.lisp"))))
        (test-write-file file source)
        (test-git directory "init" "-q")
        (test-git directory "add" "-A")
        (test-git directory "commit" "-q" "-m" "first")
        (desk-open-file desk "a.lisp")
        (fiveam:is (= 3 (length (proof-definitions (desk-proof desk)))))
        (fiveam:is (notany (lambda (index) (desk-section-edited-p desk index)) '(0 1 2)))
        ;; A marked section is folded to the heading. It is hidden if completed items are hidden.
        (fiveam:is (desk-toggle-section desk 1))
        (fiveam:is (desk-section-completed-p desk 1))
        (fiveam:is (= 1 (desk-sections-completed-count desk)))
        (fiveam:is (desk-section-folded-p desk 1))
        (fiveam:is (not (desk-section-folded-p desk 0)))
        (desk-toggle-section-fold desk 1)
        (fiveam:is (not (desk-section-folded-p desk 1)))
        (desk-toggle-section-fold desk 1)
        (fiveam:is (not (desk-section-hidden-p desk 1)))
        (setf (preference ':hide-completed) t)
        (fiveam:is (desk-section-hidden-p desk 1))
        (fiveam:is (not (desk-section-hidden-p desk 0)))
        (setf (preference ':hide-completed) nil)
        ;; The mark is saved. It is kept when the code changes and the section moves.
        (test-write-file file (format nil ";; Top.~%~%(defun z () 0)~%~%(defun a ()~%  \"Doc a.\"~%  (+ 1 0))~%~%(defun b ()~%  \"Doc b.\"~%  2)~%"))
        (desk-open-file desk "a.lisp")
        (fiveam:is (desk-section-completed-p desk 1))
        (fiveam:is (not (desk-section-edited-p desk 1)))
        ;; The words of the section changed. The section is not completed any more.
        ;; It is shown as edited, compared with the last Git commit.
        (test-write-file file (format nil ";; Top.~%~%(defun z () 0)~%~%(defun a ()~%  \"Doc of a.\"~%  (+ 1 0))~%~%(defun b ()~%  \"Doc b.\"~%  2)~%"))
        (desk-open-file desk "a.lisp")
        (fiveam:is (not (desk-section-completed-p desk 1)))
        (fiveam:is (desk-section-edited-p desk 1))
        (fiveam:is (not (desk-section-edited-p desk 2)))
        (fiveam:is (not (desk-section-edited-p desk 0)))
        ;; The marks of words that are not in the file any more are dropped
        ;; when a section of the file is marked next.
        (fiveam:is (desk-toggle-section desk 2))
        (fiveam:is (= 1 (length (ledger-sections-completed (ledger-open directory) "a.lisp"))))
        (fiveam:is (desk-toggle-section desk 2))
        (fiveam:is (null (ledger-sections-completed (ledger-open directory) "a.lisp")))))))

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
        ;; The last kind that is shown cannot be switched off.
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
  ;; If no folder is given, the folder is the current directory.
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
