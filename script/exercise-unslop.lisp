;;;; -- Unslop exercise --

;;; Drive the unslop window offscreen and check each file operation.

(in-package #:aui)

(defvar *unslop-exercise-failures* 0
  "Failed expectation count.")

(defmacro unslop-expect (description form)
  "Evaluate FORM, report DESCRIPTION, and count failures."
  `(let ((held-p (handler-case ,form
                   (error (condition)
                     (format t "~&        ~A~%" condition)
                     nil))))
     (format t "~&  ~:[FAIL~;ok  ~]  ~A~%" held-p ,description)
     (unless held-p
       (incf *unslop-exercise-failures*))
     held-p))

(defparameter *unslop-exercise-source*
  (format nil "~{~A~%~}"
          '(";;;; -- Greeting --"
            ""
            ";; Above the function."
            "(defun greet (name)"
            "  \"Say hello to NAME.\""
            "  ;; Inside."
            "  (format t \"Hello, ~A!\" name)) ; behind"
            ""
            "(defparameter *limit* 10"
            "  \"How many.\")"))
  "Exercise source text.")

(defun unslop-exercise--write (pathname text)
  "Write TEXT to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede :external-format ':utf-8)
    (write-string text stream))
  pathname)

(defun unslop-exercise--read (pathname)
  "Read PATHNAME as UTF-8 text."
  (uiop:read-file-string pathname :external-format ':utf-8))

(defun unslop-exercise--git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY."
  (uiop:run-program (append (list "git" "-C" (uiop:native-namestring directory)
                                  "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                  "-c" "commit.gpgsign=false" "-c" "init.defaultBranch=main")
                            arguments)
                    :input nil :output ':string :error-output nil :ignore-error-status t))

(defun unslop-exercise--draw (desk)
  "Draw DESK and refresh its regions."
  (unslop-arrange desk)
  (lightfast:capture-window (desk-window desk)
                            (uiop:native-namestring
                             (merge-pathnames "unslop-exercise.ppm" (uiop:temporary-directory))))
  nil)

(defun unslop-exercise--region (desk identifier)
  "Find IDENTIFIER in DESK's latest drawing."
  (stage--find-region (desk-stage desk) identifier))

(defun unslop-exercise--click (desk identifier)
  "Click the center of IDENTIFIER, if it exists."
  (unslop-exercise--draw desk)
  (let ((region (unslop-exercise--region desk identifier))
        (stage  (desk-stage desk)))
    (when region
      (let ((position (format nil "~D ~D 1 0 0 0"
                              (+ (region-x region) (floor (region-width region) 2))
                              (+ (region-y region) (floor (region-height region) 2)))))
        (stage--pointer stage ':move position)
        (stage--pointer stage ':push position)
        (stage--pointer stage ':release position))
      (unslop-exercise--draw desk)
      t)))

(defun unslop-exercise--type (desk text)
  "Put TEXT in the editor."
  (setf (lightfast:value (desk-editor desk)) text)
  (desk-note-typing desk)
  nil)

(defun unslop-exercise--settle (desk seconds)
  "Run timers and collect background work for SECONDS."
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          do (lightfast:wait 0.05)
             (desk-take-up desk)
             (sleep 0.02)))
  nil)

(defun unslop-exercise--key (desk code &key (state 0))
  "Send CODE with STATE to the editor."
  (unslop--editor-key desk (make-key-event :code code :state state)))

(defun unslop-exercise--passage (desk text)
  "Find TEXT in the open file."
  (loop for definition in (proof-definitions (desk-proof desk))
        for definition-index from 0
        do (let ((index (position text (definition-passages definition) :key #'passage-text :test #'string=)))
             (when index
               (return (cons definition-index index))))))

(defun unslop-exercise-run ()
  "Run the exercise. Returns the number of failed expectations."
  (let* ((root               (uiop:ensure-directory-pathname
                              (format nil "~Aunslop-exercise-~D/" (uiop:temporary-directory) (random 1000000))))
         (work               (merge-pathnames "work/" root))
         (greet              (merge-pathnames "greet.lisp" work))
         (other              (merge-pathnames "src/other.lisp" work))
         (shots              (uiop:getenvp "AUI_EXERCISE_SHOTS"))
         (*ledger-directory* (merge-pathnames "state/" root)))
    (unwind-protect
         (progn
           (unslop-exercise--write greet *unslop-exercise-source*)
           (unslop-exercise--write other (format nil ";; One.~%(defun other ()~%  \"Other.\"~%  1)~%"))
           (unslop-exercise--write (merge-pathnames "notes.txt" work) "not source")
           (unslop-exercise--git work "init" "-q")
           (unslop-exercise--git work "add" "-A")
           (unslop-exercise--git work "commit" "-q" "-m" "first")
           (lightfast:load-library)
           (lightfast:enable-thread-wakeups)
           (theme-install)
           (let* ((launch                   (make-unslop-launch :directory (uiop:native-namestring work)
                                                                :size      '(1320 . 880)
                                                                :shot      "unused.png"))
                  (desk                     (unslop-make-desk launch))
                  (*stage-failure-function* (lambda (report)
                                              (incf *unslop-exercise-failures*)
                                              (format t "~&  FAIL  ~A~%" report))))
             (unslop-build desk launch)
             (stage-resize (desk-stage desk) 1320 880)
             (desk-open-file desk "greet.lisp")
             (unslop-exercise--draw desk)
             (unslop-expect "only the Lisp files of the checkout are listed"
                            (and (equal '("greet.lisp" "src/other.lisp") (desk-files desk))
                                 (unslop-exercise--region desk '(:shelf-file "src/other.lisp"))))
             (unslop-expect "the definitions and the texts of the open file are shown"
                            (and (unslop-exercise--region desk '(:passage 0 0))
                                 (unslop-exercise--region desk '(:passage 1 4))
                                 (unslop-exercise--region desk '(:passage 2 0))
                                 (null (unslop-exercise--region desk '(:passage 3 0)))))
             (unslop-expect "no files are edited or completed at the start"
                            (and (not (desk-edited-p desk "greet.lisp"))
                                 (zerop (desk-completed-count desk))))
             ;; Writing in a passage.
             (let ((place (unslop-exercise--passage desk "Say hello to NAME.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-expect "clicking a passage puts the editor over the passage, and the editor contains the text of the passage"
                              (and (equal place (desk-focus desk))
                                   (string= "Say hello to NAME." (desk-typed-text desk))
                                   (let ((layout (unslop--layout 1320 880)))
                                     (destructuring-bind (x y width height) (getf layout :page)
                                       (page-editor-box desk :x x :y y :width width :height height)))))
               (unslop-exercise--type desk (format nil "Greet NAME.~%Twice \"over\"."))
               (unslop-expect "typed text is not written to the file at once"
                              (and (desk-dirty-p desk)
                                   (string= *unslop-exercise-source* (unslop-exercise--read greet))))
               (unslop-exercise--settle desk 1.6)
               (unslop-expect "after the typing stops, the text is written to the file, and the rest of the file is unchanged"
                              (and (not (desk-dirty-p desk))
                                   (desk-saved-at desk)
                                   (search (format nil "  \"Greet NAME.~%Twice \\\"over\\\".\"~%  ;; Inside.")
                                           (unslop-exercise--read greet))
                                   (search "(format t \"Hello, ~A!\" name)) ; behind" (unslop-exercise--read greet))))
               (unslop-exercise--settle desk 0.6)
               (unslop-expect "the file is marked as edited, and nothing was staged or committed in Git"
                              (and (desk-edited-p desk "greet.lisp")
                                   (not (desk-edited-p desk "src/other.lisp"))
                                   (string= "" (unslop-exercise--git work "diff" "--cached" "--name-only"))
                                   (string= (format nil "1~%")
                                            (unslop-exercise--git work "rev-list" "--count" "HEAD")))))
             (when shots
               (unslop-exercise--draw desk)
               (save-window-picture (desk-window desk) (format nil "~A/unslop-editing.png" shots)))
             ;; Pressing Tab moves to the next passage. An empty comment is removed.
             (unslop-exercise--key desk #xff09)
             (unslop-expect "pressing Tab moves to the next passage"
                            (and (equal (unslop-exercise--passage desk "Inside.") (desk-focus desk))
                                 (string= "Inside." (desk-typed-text desk))))
             (unslop-exercise--type desk "")
             (unslop-exercise--settle desk 1.6)
             (unslop-expect "an emptied comment is not removed from the file while the editor is open"
                            (search ";; Inside." (unslop-exercise--read greet)))
             (unslop-exercise--key desk #xff1b)
             (unslop-expect "after the editor is closed, the empty comment and the line of the comment are removed"
                            (and (null (desk-focus desk))
                                 (null (search "Inside" (unslop-exercise--read greet)))
                                 (search (format nil "over\\\".\"~%  (format t") (unslop-exercise--read greet))))
             ;; Deleting a comment directly, and restoring it.
             (let ((place (unslop-exercise--passage desk "behind")))
               (unslop-exercise--click desk (list ':passage-delete (first place) (rest place)))
               (unslop-expect "clicking the cross beside a comment deletes the comment and the blank lines before it"
                              (and (search (format nil "name))~%") (unslop-exercise--read greet))
                                   (null (search "behind" (unslop-exercise--read greet)))
                                   (null (desk-focus desk))
                                   (desk-removal desk)
                                   (unslop-exercise--region desk ':restore-comment))))
             (when shots
                (save-window-picture (desk-window desk) (format nil "~A/unslop-deleted.png" shots)))
             (unslop-exercise--click desk ':restore-comment)
             (unslop-expect "clicking Undo restores the comment"
                            (and (search "name)) ; behind" (unslop-exercise--read greet))
                                 (null (desk-removal desk))
                                 (null (unslop-exercise--region desk ':restore-comment))))
             (let ((docstring (format nil "Greet NAME.~%Twice \"over\"."))
                   (control-d (list (char-code #\d) lightfast:+modifier-command+)))
               (let ((place (unslop-exercise--passage desk "Above the function.")))
                 (unslop-exercise--click desk (list ':passage (first place) (rest place)))
                 (unslop-exercise--type desk "Above, half typed")
                 (when shots
                   (unslop-exercise--draw desk)
                   (save-window-picture (desk-window desk) (format nil "~A/unslop-comment.png" shots)))
                 (unslop-exercise--key desk (first control-d) :state (second control-d))
                 (unslop-expect "pressing Ctrl+D deletes the comment in the editor and moves to the next text"
                                (and (null (search "Above" (unslop-exercise--read greet)))
                                     (search (format nil "~%~%(defun greet") (unslop-exercise--read greet))
                                     (equal (unslop-exercise--passage desk docstring) (desk-focus desk))
                                     (string= docstring (desk-typed-text desk))))
                 (unslop-exercise--click desk ':restore-comment)
                 (unslop-expect "clicking Undo restores that comment as it was in the file"
                                (and (search (format nil ";; Above the function.~%(defun greet")
                                             (unslop-exercise--read greet))
                                     (null (desk-focus desk)))))
               (let ((place (unslop-exercise--passage desk docstring)))
                 (unslop-exercise--click desk (list ':passage (first place) (rest place)))
                 (unslop-exercise--key desk (first control-d) :state (second control-d))
                 (unslop-expect "pressing Ctrl+D in a docstring does not delete it, and a notice is shown"
                                (and (search "Greet NAME." (unslop-exercise--read greet))
                                     (desk-notice desk)))
                 (unslop-exercise--key desk #xff1b)))
             ;; Sections of a file.
             (unslop-exercise--draw desk)
             (unslop-expect "the section that differs from the last Git commit is the only section marked as edited"
                            (and (desk-section-edited-p desk 1)
                                 (not (desk-section-edited-p desk 0))
                                 (not (desk-section-edited-p desk 2))))
             (unslop-exercise--click desk '(:section-check 1))
             (unslop-expect "clicking the box on a heading marks the section as completed and folds the section"
                            (and (desk-section-completed-p desk 1)
                                 (unslop-exercise--region desk '(:section 1))
                                 (null (unslop-exercise--region desk '(:passage 1 0)))
                                 (unslop-exercise--region desk '(:passage 2 0))))
             (when shots
                (save-window-picture (desk-window desk) (format nil "~A/unslop-sections.png" shots)))
             (unslop-exercise--click desk '(:section 1))
             (unslop-expect "clicking the heading unfolds the section"
                            (unslop-exercise--region desk '(:passage 1 0)))
             (unslop-exercise--click desk '(:section 1))
             (unslop-exercise--click desk ':hide-completed)
             (unslop-expect "if completed items are hidden, a completed section is hidden too"
                            (and (null (unslop-exercise--region desk '(:section 1)))
                                 (unslop-exercise--region desk '(:section 0))))
             (unslop-exercise--click desk ':hide-completed)
             (unslop-exercise--click desk '(:passage 0 0))
             (unslop-exercise--key desk #xff0d :state (logior lightfast:+modifier-command+ lightfast:+modifier-shift+))
             (unslop-expect "pressing Ctrl+Shift+Enter marks the section as completed and moves to the next open section"
                            (and (desk-section-completed-p desk 0)
                                 (equal '(2 . 0) (desk-focus desk))))
             (unslop-exercise--key desk #xff1b)
             (unslop-expect "the marks of the sections are saved for the next window"
                            (let ((again (unslop-make-desk launch)))
                              (desk-open-file again "greet.lisp")
                              (and (desk-section-completed-p again 0)
                                   (desk-section-completed-p again 1)
                                   (not (desk-section-completed-p again 2)))))
             (unslop-exercise--click desk '(:section-check 0))
             (unslop-exercise--click desk '(:section-check 1))
             ;; Completed files.
             (unslop-exercise--click desk ':complete)
             (unslop-expect "clicking the button marks the open file as completed, and the mark is saved outside the checkout"
                            (and (desk-completed-p desk "greet.lisp")
                                 (= 1 (desk-completed-count desk))
                                 (probe-file (ledger-file (desk-ledger desk)))
                                 (null (search (namestring work) (namestring (ledger-file (desk-ledger desk)))))
                                 ;; The checkout contains only the files from before.
                                 (equal '(".git/" "greet.lisp" "notes.txt" "src/")
                                        (sort (mapcar (lambda (pathname)
                                                        (enough-namestring pathname work))
                                                      (append (uiop:directory-files work)
                                                              (uiop:subdirectories work)))
                                              #'string<))))
             (unslop-exercise--settle desk 0.5)
             (unslop-expect "the file is still marked after the checkout is read again"
                            (desk-completed-p desk "greet.lisp"))
             (let ((place (unslop-exercise--passage desk "How many.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "How many at most.")
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "a completed file is marked as open again after a change"
                              (and (search "\"How many at most.\"" (unslop-exercise--read greet))
                                   (not (desk-completed-p desk "greet.lisp")))))
             (unslop-exercise--click desk '(:shelf-check "greet.lisp"))
             (unslop-expect "clicking the box in the list also marks a file as completed"
                            (desk-completed-p desk "greet.lisp"))
             (unslop-exercise--click desk ':hide-completed)
             (unslop-expect "completed files can be hidden from the list, except the open file"
                            (and (desk-hide-completed-p)
                                 (equal '("greet.lisp" "src/other.lisp") (shelf-names desk))))
             (unslop-exercise--click desk '(:shelf-file "src/other.lisp"))
             (unslop-expect "after another file is opened, the completed file is hidden from the list"
                            (and (equal "src/other.lisp" (desk-name desk))
                                 (equal '("src/other.lisp") (shelf-names desk))))
             (unslop-exercise--click desk ':hide-completed)
             ;; The kinds of text.
             (unslop-exercise--click desk '(:kind :string))
             (unslop-expect "a kind of text can be switched off"
                            (and (not (member ':string (desk-kinds)))
                                 (unslop-exercise--region desk '(:passage 0 0))
                                 (unslop-exercise--region desk '(:passage 0 1))))
             (unslop-exercise--click desk '(:kind :string))
             ;; The open file is changed by another program.
             (unslop-exercise--write other (format nil ";; One.~%(defun other ()~%  \"Other, rewritten.\"~%  1)~%"))
             (desk-check-disk desk)
             (unslop-expect "a file changed by another program is read again"
                            (unslop-exercise--passage desk "Other, rewritten."))
             (let ((place (unslop-exercise--passage desk "One.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "One, here.")
               (unslop-exercise--write other (format nil ";; One.~%(defun other ()~%  \"Other, again.\"~%  1)~%"))
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "text typed while another program wrote the file is merged into the new file"
                              (string= (format nil ";; One, here.~%(defun other ()~%  \"Other, again.\"~%  1)~%")
                                       (unslop-exercise--read other))))
             (let ((place (unslop-exercise--passage desk "One, here.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "One, lost?")
               (unslop-exercise--write other (format nil "(defun another ()~%  2)~%"))
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "if the code of the file changed too, nothing is written and a notice is shown"
                              (and (string= (format nil "(defun another ()~%  2)~%") (unslop-exercise--read other))
                                   (desk-notice desk))))
             ;; A passage taller than the window is edited in the visible part.
             (unslop-exercise--write other (format nil "~{;; Line ~D of a long comment.~%~}(defun other ()~%  1)~%"
                                                   (loop for line from 1 to 80 collect line)))
             (desk-open-file desk "src/other.lisp")
             (unslop-exercise--click desk '(:passage 0 0))
             (unslop-expect "for a passage taller than the page, the editor is over the visible part"
                            (let* ((layout (unslop--layout 1320 880))
                                   (box    (destructuring-bind (x y width height) (getf layout :page)
                                             (page-editor-box desk :x x :y y :width width :height height))))
                              (and box
                                   (> (fourth box) 400)
                                   (<= (+ (second box) (fourth box)) (- 880 *page-status-height*)))))
             (unslop-exercise--key desk #xff1b)
             ;; The filter.
             (setf (desk-filter desk) "gre")
             (unslop-expect "the list is filtered by name"
                            (equal '("greet.lisp" "src/other.lisp") (shelf-names desk)))
             (setf (desk-filter desk) "")
             ;; The tree of folders.
             (unslop-exercise--draw desk)
             (unslop-expect "the list is a tree, and each folder is listed before its files and subfolders"
                            (and (equal '((:file "greet.lisp" 0) (:folder "src/" 0) (:file "other.lisp" 1))
                                        (mapcar (lambda (row)
                                                  (list (shelf-row-kind row) (shelf-row-label row) (shelf-row-depth row)))
                                                (shelf-rows desk)))
                                 (unslop-exercise--region desk '(:shelf-folder "src/"))))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             (unslop-expect "clicking a folder folds it, and the files in the folder are hidden from the list"
                            (and (desk-folded-p desk "src/")
                                 (unslop-exercise--region desk '(:shelf-folder "src/"))
                                 (null (unslop-exercise--region desk '(:shelf-file "src/other.lisp")))))
             (when shots
                (save-window-picture (desk-window desk) (format nil "~A/unslop-folded.png" shots)))
             (setf (desk-filter desk) "oth")
             (unslop-exercise--draw desk)
             (unslop-expect "if the list is filtered by name, the matching files in a folded folder are shown"
                            (unslop-exercise--region desk '(:shelf-file "src/other.lisp")))
             (setf (desk-filter desk) "")
             (shelf-step desk -1)
             (shelf-step desk 1)
             (unslop-exercise--draw desk)
             (unslop-expect "a file opened with a key is visible, and the folder of the file is unfolded"
                            (and (equal "src/other.lisp" (desk-name desk))
                                 (not (desk-folded-p desk "src/"))
                                 (unslop-exercise--region desk '(:shelf-file "src/other.lisp"))))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             ;; Marks and folded folders are saved.
             (let ((again (unslop-make-desk launch)))
               (unslop-expect "the completed files are saved for the next window"
                              (and (desk-completed-p again "greet.lisp")
                                   (not (desk-completed-p again "src/other.lisp"))))
               (unslop-expect "the folded folders are saved for the next window"
                              (desk-folded-p again "src/")))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             (when shots
               (desk-open-file desk "greet.lisp")
               (unslop-exercise--draw desk)
               (save-window-picture (desk-window desk) (format nil "~A/unslop-end.png" shots)))
             (unslop-expect "nothing is staged in Git, and there is no new commit"
                            (and (string= "" (unslop-exercise--git work "diff" "--cached" "--name-only"))
                                 (string= (format nil "1~%")
                                          (unslop-exercise--git work "rev-list" "--count" "HEAD"))))))
      (uiop:delete-directory-tree root :validate          (lambda (checked)
                                                            (search "unslop-exercise-" (namestring checked)))
                                       :if-does-not-exist ':ignore)))
  *unslop-exercise-failures*)

(let ((failures (unslop-exercise-run)))
  (format t "~&~:[The unslop exercise passed.~;~:*~D expectation~:P failed.~]~%"
          (and (plusp failures) failures))
  (finish-output)
  (sb-ext:exit :code (if (zerop failures) 0 1) :abort t))
