(in-package #:aui)

;;;; -- Link Tests --

(fiveam:def-suite links
  :description "Tests for the targets of links and for the files mentioned in a conversation."
  :in          aui)

(fiveam:in-suite links)

(defmacro with-test-folder ((directory &rest names) &body body)
  "Run BODY with DIRECTORY bound to the namestring of a new folder.
The folder has a file for each of NAMES, and it is deleted after BODY."
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
      (let ((digest (digest-create transcript directory)))
        (digest--read digest)
        ;; The plan is taken from the latest update.
        (fiveam:is (equal '(2 3) (multiple-value-list (digest-plan-progress digest))))
        ;; The file that was used last is listed first. Files changed by a tool are marked.
        ;; Files from a failed edit are not marked.
        (fiveam:is (equal '(("Makefile" . t) ("notes.md" . nil) ("src/window.lisp" . nil))
                          (loop for (file . changed-p) in (digest-files digest)
                                collect (cons (enough-namestring file (truename directory)) changed-p))))
        (fiveam:is (equal "src" (file-place (first (third (digest-files digest))) (digest-root digest))))
        (fiveam:is (equal "" (file-place (first (first (digest-files digest))) (digest-root digest))))
        ;; The messages are from the person and from the other participants.
        ;; The oldest message is first.
        (fiveam:is (equal '("Why does the build fail?" "The Makefile reads well.")
                          (mapcar #'user-entry-text (digest-messages digest))))))))

(fiveam:test a-folder-reached-through-a-link-still-places-its-files
  (with-test-folder (directory "src/window.lisp")
    (let ((link (format nil "~Aaui-links-way-~D" (uiop:temporary-directory) (random 1000000))))
      (uiop:run-program (list "ln" "-s" (string-right-trim "/" directory) link))
      (unwind-protect
           (let* ((transcript (make-transcript))
                  (digest     (digest-create transcript (format nil "~A/" link))))
             (transcript-append transcript (make-instance 'assistant-entry :text "See `src/window.lisp`."))
             (digest--read digest)
             (fiveam:is (= 1 (length (digest-files digest))))
             (fiveam:is (equal "src" (file-place (first (first (digest-files digest))) (digest-root digest)))))
        (delete-file link)))))
