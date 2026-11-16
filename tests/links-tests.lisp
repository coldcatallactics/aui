(in-package #:aui)

;;;; -- Link Tests --

(fiveam:def-suite links
  :description "What a link in a conversation leads to, and which files an answer names."
  :in          aui)

(fiveam:in-suite links)

(defmacro with-test-folder ((directory &rest names) &body body)
  "Run BODY with DIRECTORY bound to the namestring of a new folder holding a file for each of NAMES, and delete the folder afterwards."
  (let ((root (gensym "ROOT")))
    `(let* ((,root      (uiop:ensure-directory-pathname
                         (format nil "~Aaui-links-~D/" (uiop:temporary-directory) (random 1000000))))
            (,directory (namestring ,root)))
       (unwind-protect
            (progn
              (dolist (name (list ,@names))
                (let ((file (merge-pathnames (uiop:parse-native-namestring name) ,root)))
                  (ensure-directories-exist file)
                  (with-open-file (stream file :direction ':output :if-exists ':supersede)
                    (write-line "placeholder" stream))))
              ,@body)
         (uiop:delete-directory-tree ,root :validate (lambda (checked)
                                                       (search "aui-links-" (namestring checked))))))))

(fiveam:test a-page-is-told-from-a-file
  (fiveam:is (link-web-p "https://www.fltk.org/doc"))
  (fiveam:is (link-web-p "mailto:someone@example.org"))
  (fiveam:is (not (link-web-p "file:///tmp/notes.md")))
  (fiveam:is (not (link-web-p "src/view/window.lisp:12")))
  (fiveam:is (not (link-web-p "/tmp/notes.md")))
  (fiveam:is (not (link-web-p "C:notes"))))

(fiveam:test a-link-finds-its-file-from-the-channel's-folder
  (with-test-folder (directory "src/window.lisp" "notes on fonts.md")
    (flet ((found (link)
             (let ((file (link-file link directory)))
               (and file (enough-namestring file (truename directory))))))
      (fiveam:is (equal "src/window.lisp" (found "src/window.lisp")))
      (fiveam:is (equal "src/window.lisp" (found "src/window.lisp:12")))
      (fiveam:is (equal "src/window.lisp" (found "src/window.lisp:12:3")))
      (fiveam:is (equal "src/window.lisp" (found "src/window.lisp#L12")))
      (fiveam:is (equal "src/window.lisp" (found (format nil "~Asrc/window.lisp" directory))))
      (fiveam:is (equal "src/window.lisp" (found (format nil "file://~Asrc/window.lisp#L4" directory))))
      (fiveam:is (equal "notes on fonts.md" (found "notes%20on%20fonts.md")))
      (fiveam:is (equal "notes on fonts.md" (found (format nil "file://~Anotes%20on%20fonts.md" directory))))
      (fiveam:is (null (found "src/missing.lisp")))
      (fiveam:is (null (found "https://www.fltk.org"))))))

(fiveam:test an-answer-names-files-by-link-and-in-code
  (fiveam:is (equal '("notes.md" "src/window.lisp:12" "https://www.fltk.org")
                    (files--named (format nil "See [the notes](notes.md) and `src/window.lisp:12`.~%Run `make` and `git status`, then read [the site](https://www.fltk.org).~%```~%[not a link](inside/code.md)~%```")))))

(fiveam:test a-conversation-is-read-for-its-plan-files-and-messages
  (with-test-folder (directory "Makefile" "src/window.lisp" "notes.md")
    (let ((transcript (make-transcript)))
      (dolist (entry (list (make-instance 'user-entry :text "Why does the build fail?")
                           (make-instance 'tool-entry
                                          :name "plan.update" :status ':completed
                                          :arguments '(("plan" . ((("step" . "Read it") ("status" . "completed"))
                                                                  (("step" . "Fix it") ("status" . "pending"))))))
                           (make-instance 'assistant-entry :text "See `src/window.lisp` and [the notes](notes.md).")
                           (make-instance 'tool-entry
                                          :name "resource.edit" :status ':completed
                                          :arguments '(("uri" . "workspace:Makefile")))
                           (make-instance 'tool-entry
                                          :name "resource.edit" :status ':failed
                                          :arguments '(("uri" . "workspace:notes.md")))
                           (make-instance 'user-entry :text "The Makefile reads well." :sender "review")
                           (make-instance 'tool-entry
                                          :name "plan.update" :status ':completed
                                          :arguments '(("plan" . ((("step" . "Read it") ("status" . "completed"))
                                                                  (("step" . "Fix it") ("status" . "completed"))
                                                                  (("step" . "Check it") ("status" . "in_progress"))))))
                           (make-instance 'assistant-entry :text "The `Makefile` is fixed; `make` builds it.")))
        (transcript-append transcript entry))
      (let ((digest (make-digest :transcript transcript :directory directory)))
        (digest--read digest)
        ;; The plan is its latest update.
        (fiveam:is (equal '(2 3) (multiple-value-list (digest-plan-progress digest))))
        ;; The file touched last comes first; what a tool changed stays marked, what it failed to change is not.
        (fiveam:is (equal '(("Makefile" . t) ("notes.md" . nil) ("src/window.lisp" . nil))
                          (loop for (file . changed-p) in (digest-files digest)
                                collect (cons (enough-namestring file (truename directory)) changed-p))))
        (fiveam:is (equal "src" (file-place (first (third (digest-files digest))) directory)))
        (fiveam:is (equal "" (file-place (first (first (digest-files digest))) directory)))
        ;; The messages are the person's and the other participants', oldest first.
        (fiveam:is (equal '("Why does the build fail?" "The Makefile reads well.")
                          (mapcar #'user-entry-text (digest-messages digest))))))))
