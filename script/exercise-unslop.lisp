;;;; -- Unslop Exercise --

;;; Drive the unslop window offscreen against a throwaway checkout: open a
;;; file, write in a passage, wait for the save, leave a comment empty, mark
;;; a file completed, hide it, change a file from outside. Each step is
;;; checked against what the file on disk holds.

(in-package #:aui)

(defvar *unslop-exercise-failures* 0
  "How many expectations of the exercise failed.")

(defmacro unslop-expect (description form)
  "Evaluate FORM and say whether what DESCRIPTION states held."
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
  "The file the exercise writes in.")

(defun unslop-exercise--write (pathname text)
  "Write TEXT to the file at PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede :external-format ':utf-8)
    (write-string text stream))
  pathname)

(defun unslop-exercise--read (pathname)
  "Return what the file at PATHNAME holds."
  (uiop:read-file-string pathname :external-format ':utf-8))

(defun unslop-exercise--git (directory &rest arguments)
  "Run Git on ARGUMENTS in DIRECTORY and return what it printed."
  (uiop:run-program (append (list "git" "-C" (uiop:native-namestring directory)
                                  "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                  "-c" "commit.gpgsign=false" "-c" "init.defaultBranch=main")
                            arguments)
                    :input nil :output ':string :error-output nil :ignore-error-status t))

(defun unslop-exercise--draw (desk)
  "Draw DESK's window offscreen, so its regions are those of its current state."
  (unslop-arrange desk)
  (lightfast:capture-window (desk-window desk)
                            (uiop:native-namestring
                             (merge-pathnames "unslop-exercise.ppm" (uiop:temporary-directory))))
  nil)

(defun unslop-exercise--region (desk identifier)
  "Return the region the last drawing declared as IDENTIFIER, or NIL."
  (stage--find-region (desk-stage desk) identifier))

(defun unslop-exercise--click (desk identifier)
  "Click in the middle of the region IDENTIFIER and return whether it exists."
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
  "Put TEXT into the editor as if it had been typed."
  (setf (lightfast:value (desk-editor desk)) text)
  (desk-note-typing desk)
  nil)

(defun unslop-exercise--settle (desk seconds)
  "Let timers run and take up what was found for SECONDS."
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          do (lightfast:wait 0.05)
             (desk-take-up desk)
             (sleep 0.02)))
  nil)

(defun unslop-exercise--key (desk code &key (state 0))
  "Press the key with CODE in the editor."
  (unslop--editor-key desk (make-key-event :code code :state state)))

(defun unslop-exercise--passage (desk text)
  "Return the (DEFINITION . PASSAGE) numbers of the passage of the open file that says TEXT, or NIL."
  (loop for definition in (proof-definitions (desk-proof desk))
        for definition-index from 0
        do (let ((index (position text (definition-passages definition) :key #'passage-text :test #'string=)))
             (when index
               (return (cons definition-index index))))))

(defun unslop-exercise-run ()
  "Run the exercise and return how many expectations failed."
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
             (unslop-expect "the checkout's Lisp files are listed, and nothing else"
                            (and (equal '("greet.lisp" "src/other.lisp") (desk-files desk))
                                 (unslop-exercise--region desk '(:shelf-file "src/other.lisp"))))
             (unslop-expect "the open file shows its definitions and their texts"
                            (and (unslop-exercise--region desk '(:passage 0 0))
                                 (unslop-exercise--region desk '(:passage 1 4))
                                 (unslop-exercise--region desk '(:passage 2 0))
                                 (null (unslop-exercise--region desk '(:passage 3 0)))))
             (unslop-expect "nothing is edited or completed at the start"
                            (and (not (desk-edited-p desk "greet.lisp"))
                                 (zerop (desk-completed-count desk))))
             ;; Writing in a passage.
             (let ((place (unslop-exercise--passage desk "Say hello to NAME.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-expect "a click on a passage puts the editor over it with its words"
                              (and (equal place (desk-focus desk))
                                   (string= "Say hello to NAME." (desk-typed-text desk))
                                   (let ((layout (unslop--layout 1320 880)))
                                     (destructuring-bind (x y width height) (getf layout :page)
                                       (page-editor-box desk :x x :y y :width width :height height)))))
               (unslop-exercise--type desk (format nil "Greet NAME.~%Twice \"over\"."))
               (unslop-expect "what is typed is not written at once"
                              (and (desk-dirty-p desk)
                                   (string= *unslop-exercise-source* (unslop-exercise--read greet))))
               (unslop-exercise--settle desk 1.6)
               (unslop-expect "once the typing stops it is written to the file, and only it"
                              (and (not (desk-dirty-p desk))
                                   (desk-saved-at desk)
                                   (search (format nil "  \"Greet NAME.~%Twice \\\"over\\\".\"~%  ;; Inside.")
                                           (unslop-exercise--read greet))
                                   (search "(format t \"Hello, ~A!\" name)) ; behind" (unslop-exercise--read greet))))
               (unslop-exercise--settle desk 0.6)
               (unslop-expect "the file is marked edited, and Git was only asked"
                              (and (desk-edited-p desk "greet.lisp")
                                   (not (desk-edited-p desk "src/other.lisp"))
                                   (string= "" (unslop-exercise--git work "diff" "--cached" "--name-only"))
                                   (string= (format nil "1~%")
                                            (unslop-exercise--git work "rev-list" "--count" "HEAD")))))
             (when shots
               (unslop-exercise--draw desk)
               (unslop--picture desk (format nil "~A/unslop-editing.png" shots)))
             ;; Tab goes on to the next passage; a comment left empty goes away.
             (unslop-exercise--key desk #xff09)
             (unslop-expect "Tab goes to the next passage"
                            (and (equal (unslop-exercise--passage desk "Inside.") (desk-focus desk))
                                 (string= "Inside." (desk-typed-text desk))))
             (unslop-exercise--type desk "")
             (unslop-exercise--settle desk 1.6)
             (unslop-expect "a comment emptied while writing stays in the file"
                            (search ";; Inside." (unslop-exercise--read greet)))
             (unslop-exercise--key desk #xff1b)
             (unslop-expect "left empty, the comment is removed with its line"
                            (and (null (desk-focus desk))
                                 (null (search "Inside" (unslop-exercise--read greet)))
                                 (search (format nil "over\\\".\"~%  (format t") (unslop-exercise--read greet))))
             ;; Completed files.
             (unslop-exercise--click desk ':complete)
             (unslop-expect "the button marks the open file completed, outside the checkout"
                            (and (desk-completed-p desk "greet.lisp")
                                 (= 1 (desk-completed-count desk))
                                 (probe-file (ledger-file (desk-ledger desk)))
                                 (null (search (namestring work) (namestring (ledger-file (desk-ledger desk)))))
                                 ;; The checkout holds what it held: no file of unslop's.
                                 (equal '(".git/" "greet.lisp" "notes.txt" "src/")
                                        (sort (mapcar (lambda (pathname)
                                                        (enough-namestring pathname work))
                                                      (append (uiop:directory-files work)
                                                              (uiop:subdirectories work)))
                                              #'string<))))
             (unslop-exercise--settle desk 0.5)
             (unslop-expect "the mark holds after another look at the checkout"
                            (desk-completed-p desk "greet.lisp"))
             (let ((place (unslop-exercise--passage desk "How many.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "How many at most.")
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "a completed file that is changed is open again"
                              (and (search "\"How many at most.\"" (unslop-exercise--read greet))
                                   (not (desk-completed-p desk "greet.lisp")))))
             (unslop-exercise--click desk '(:shelf-check "greet.lisp"))
             (unslop-expect "the box in the list marks a file too"
                            (desk-completed-p desk "greet.lisp"))
             (unslop-exercise--click desk ':hide-completed)
             (unslop-expect "completed files can be left out of the list, but for the open one"
                            (and (desk-hide-completed-p)
                                 (equal '("greet.lisp" "src/other.lisp") (shelf-names desk))))
             (unslop-exercise--click desk '(:shelf-file "src/other.lisp"))
             (unslop-expect "another file opens, and the completed one is gone from the list"
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
             ;; Another program changes the open file.
             (unslop-exercise--write other (format nil ";; One.~%(defun other ()~%  \"Other, rewritten.\"~%  1)~%"))
             (desk-check-disk desk)
             (unslop-expect "a file changed by another program is read again"
                            (unslop-exercise--passage desk "Other, rewritten."))
             (let ((place (unslop-exercise--passage desk "One.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "One, here.")
               (unslop-exercise--write other (format nil ";; One.~%(defun other ()~%  \"Other, again.\"~%  1)~%"))
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "words typed while another program wrote the file go into what it wrote"
                              (string= (format nil ";; One, here.~%(defun other ()~%  \"Other, again.\"~%  1)~%")
                                       (unslop-exercise--read other))))
             (let ((place (unslop-exercise--passage desk "One, here.")))
               (unslop-exercise--click desk (list ':passage (first place) (rest place)))
               (unslop-exercise--type desk "One, lost?")
               (unslop-exercise--write other (format nil "(defun another ()~%  2)~%"))
               (unslop-exercise--key desk #xff1b)
               (unslop-expect "when its code changed too, nothing is written and the window says so"
                              (and (string= (format nil "(defun another ()~%  2)~%") (unslop-exercise--read other))
                                   (desk-notice desk))))
             ;; A passage taller than the window is written in through the part that shows.
             (unslop-exercise--write other (format nil "~{;; Line ~D of a long comment.~%~}(defun other ()~%  1)~%"
                                                   (loop for line from 1 to 80 collect line)))
             (desk-open-file desk "src/other.lisp")
             (unslop-exercise--click desk '(:passage 0 0))
             (unslop-expect "a passage taller than the page gets the editor over the part in view"
                            (let* ((layout (unslop--layout 1320 880))
                                   (box    (destructuring-bind (x y width height) (getf layout :page)
                                             (page-editor-box desk :x x :y y :width width :height height))))
                              (and box
                                   (> (fourth box) 400)
                                   (<= (+ (second box) (fourth box)) (- 880 *page-status-height*)))))
             (unslop-exercise--key desk #xff1b)
             ;; The filter.
             (setf (desk-filter desk) "gre")
             (unslop-expect "the filter narrows the list by name"
                            (equal '("greet.lisp" "src/other.lisp") (shelf-names desk)))
             (setf (desk-filter desk) "")
             ;; The tree of folders.
             (unslop-exercise--draw desk)
             (unslop-expect "the list is a tree, a folder ahead of its files and those further in"
                            (and (equal '((:file "greet.lisp" 0) (:folder "src/" 0) (:file "other.lisp" 1))
                                        (mapcar (lambda (row)
                                                  (list (shelf-row-kind row) (shelf-row-label row) (shelf-row-depth row)))
                                                (shelf-rows desk)))
                                 (unslop-exercise--region desk '(:shelf-folder "src/"))))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             (unslop-expect "a click on a folder folds it, and its files leave the list"
                            (and (desk-folded-p desk "src/")
                                 (unslop-exercise--region desk '(:shelf-folder "src/"))
                                 (null (unslop-exercise--region desk '(:shelf-file "src/other.lisp")))))
             (when shots
               (unslop--picture desk (format nil "~A/unslop-folded.png" shots)))
             (setf (desk-filter desk) "oth")
             (unslop-exercise--draw desk)
             (unslop-expect "while the list is narrowed by name, a folded folder shows what matches"
                            (unslop-exercise--region desk '(:shelf-file "src/other.lisp")))
             (setf (desk-filter desk) "")
             (shelf-step desk -1)
             (shelf-step desk 1)
             (unslop-exercise--draw desk)
             (unslop-expect "a file opened with a key is in view, its folder unfolded"
                            (and (equal "src/other.lisp" (desk-name desk))
                                 (not (desk-folded-p desk "src/"))
                                 (unslop-exercise--region desk '(:shelf-file "src/other.lisp"))))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             ;; Marks and folded folders are remembered.
             (let ((again (unslop-make-desk launch)))
               (unslop-expect "what was completed is remembered by the next window"
                              (and (desk-completed-p again "greet.lisp")
                                   (not (desk-completed-p again "src/other.lisp"))))
               (unslop-expect "and so is what was folded"
                              (desk-folded-p again "src/")))
             (unslop-exercise--click desk '(:shelf-folder "src/"))
             (when shots
               (desk-open-file desk "greet.lisp")
               (unslop-exercise--draw desk)
               (unslop--picture desk (format nil "~A/unslop-end.png" shots)))
             (unslop-expect "Git has nothing staged and no new commit"
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
