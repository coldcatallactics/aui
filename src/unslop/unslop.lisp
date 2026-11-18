(in-package #:aui)

;;;; -- Unslop --

;;; The unslop window and its start. It shows the source files of a Common
;;; Lisp checkout at the left and the words of one of them at the right, to
;;; be rewritten in place. What is typed is written to its file once the
;;; typing stops. Nothing here commits, stages or touches anything but the
;;; file being written in and the ledger kept outside the checkout.

(defparameter *unslop-default-size* '(1320 . 880)
  "The window's width and height when the command line names none.")

(defparameter *unslop-minimum-size* '(720 . 420)
  "The smallest the window is made.")

(defstruct unslop-launch
  "What the command line asked of unslop, as the DIRECTORY of the checkout, the window's SIZE as a pair of width and height or NIL, the file a picture is written to as SHOT or NIL, and the name of the FILE to open at the start or NIL."
  (directory nil)
  (size      nil :type (or null cons))
  (shot      nil :type (or null string))
  (file      nil :type (or null string)))

(defparameter *unslop-usage*
  "usage: unslop [DIRECTORY] [--file NAME] [--size WIDTHxHEIGHT] [--shot FILE]

Shows the comments, strings and docstrings of the Common Lisp source in
DIRECTORY, or in the current directory, for rewriting. What you type is
written to its file when you stop typing. Nothing is committed.

  --file NAME            open this file of the checkout at the start
  --size WIDTHxHEIGHT    make the window this size
  --shot FILE            write a picture of the window to FILE and leave
"
  "What unslop says of its command line.")

(-> unslop--parse-size (string) cons)
(defun unslop--parse-size (text)
  "Return the size TEXT names, such as 1200x800, as a pair of width and height. Signal a PROOF-ERROR when it names none."
  (let* ((cross  (position #\x text :test #'char-equal))
         (width  (and cross (parse-integer text :end cross :junk-allowed t)))
         (height (and cross (parse-integer text :start (1+ cross) :junk-allowed t))))
    (unless (and width height (> width 200) (> height 200))
      (error 'proof-error
             :reason  ':usage
             :message (format nil "~A is not a size; write one as 1200x800" text)))
    (cons width height)))

(-> unslop-parse (list) (or null unslop-launch))
(defun unslop-parse (arguments)
  "Return what the command line ARGUMENTS ask for, or NIL after printing the usage when they ask for that. Signal a PROOF-ERROR for an argument unslop does not know."
  (let ((launch (make-unslop-launch)))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (flet ((value ()
                        (or (pop arguments)
                            (error 'proof-error
                                   :reason  ':usage
                                   :message (format nil "~A needs a value" argument)))))
                 (cond
                   ((member argument '("--help" "-h") :test #'string=)
                    (write-string *unslop-usage*)
                    (return-from unslop-parse nil))
                   ((string= argument "--size")
                    (setf (unslop-launch-size launch) (unslop--parse-size (value))))
                   ((string= argument "--shot")
                    (setf (unslop-launch-shot launch) (value)))
                   ((string= argument "--file")
                    (setf (unslop-launch-file launch) (value)))
                   ((and (plusp (length argument)) (char= (char argument 0) #\-))
                    (error 'proof-error
                           :reason  ':usage
                           :message (format nil "unslop does not know ~A; try --help" argument)))
                   ((unslop-launch-directory launch)
                    (error 'proof-error
                           :reason  ':usage
                           :message "unslop takes one directory"))
                   (t
                    (setf (unslop-launch-directory launch) argument))))))
    (unless (unslop-launch-directory launch)
      (setf (unslop-launch-directory launch) (uiop:native-namestring (uiop:getcwd))))
    launch))


;;;; -- Layout --

(-> unslop--layout (integer integer) list)
(defun unslop--layout (width height)
  "Return where the panes of a window WIDTH by HEIGHT lie, as a property list of (X Y WIDTH HEIGHT) boxes: the :SHELF, the :PAGE and the :FIELD the file filter is typed in."
  (let ((shelf (min *shelf-width* (floor (* width 2) 5))))
    (list :shelf (list 0 0 shelf height)
          :page  (list (1+ shelf) 0 (max 0 (- width shelf 1)) height)
          :field (list 18 66 (- shelf 36) 26))))

(-> unslop--paint (desk stage) null)
(defun unslop--paint (desk stage)
  "Draw the whole window: the list of files, a hairline, and the open file's page."
  (let ((layout (unslop--layout (stage-width stage) (stage-height stage))))
    (destructuring-bind (x y width height) (getf layout :shelf)
      (shelf-paint desk stage :x x :y y :width width :height height)
      (paint-line (+ x width) y :x2 (+ x width) :y2 (+ y height)))
    (destructuring-bind (x y width height) (getf layout :page)
      (page-paint desk stage :x x :y y :width width :height height)))
  nil)

(-> unslop--place (t (or null list)) null)
(defun unslop--place (widget box)
  "Show WIDGET in BOX, an (X Y WIDTH HEIGHT) list, or hide it when BOX is NIL."
  (if box
      (destructuring-bind (x y width height) box
        (lightfast:resize-widget widget :x x :y y :width (max 1 width) :height (max 1 height))
        (lightfast:show widget))
      (lightfast:hide widget))
  nil)

(-> unslop-arrange (desk) null)
(defun unslop-arrange (desk)
  "Put the native fields where the window has them now: the filter over its box, the editor over the passage being written in."
  (let* ((stage  (desk-stage desk))
         (layout (unslop--layout (stage-width stage) (stage-height stage))))
    (unslop--place (desk-field desk) (getf layout :field))
    (unslop--place (desk-editor desk)
                   (destructuring-bind (x y width height) (getf layout :page)
                     (page-editor-box desk :x x :y y :width width :height height))))
  nil)

(-> unslop--reveal (desk) null)
(defun unslop--reveal (desk)
  "Scroll the page so that the passage being written in is in view."
  (let* ((stage  (desk-stage desk))
         (layout (unslop--layout (stage-width stage) (stage-height stage))))
    (destructuring-bind (x y width height) (getf layout :page)
      (page-reveal desk :x x :y y :width width :height height)))
  nil)


;;;; -- Keys --

(-> unslop--complete-and-go-on (desk) null)
(defun unslop--complete-and-go-on (desk)
  "Mark the open file completed, unless it is, and open the next file that is not."
  (let ((name (desk-name desk)))
    (when name
      (desk-leave desk)
      (unless (desk-completed-p desk name)
        (desk-toggle-completed desk name))
      (shelf-open-next-open desk)))
  nil)

(-> unslop--scroll (desk integer) null)
(defun unslop--scroll (desk amount)
  "Move the page by AMOUNT units, down when positive."
  (setf (desk-page-scroll desk) (max 0 (+ (desk-page-scroll desk) amount)))
  (desk-redraw desk)
  nil)

(-> unslop--shortcut (desk key-event) boolean)
(defun unslop--shortcut (desk event)
  "Handle a key that means the same wherever the keyboard is, and return whether it was used."
  (let ((key       (key-event-key event))
        (command-p (key-event-command-p event))
        (page      (floor (* 3 (stage-height (desk-stage desk))) 4)))
    (cond
      ((and command-p (member key '(:enter :keypad-enter)))
       (unslop--complete-and-go-on desk)
       t)
      ((and command-p (eql key #\s))
       (desk-save desk)
       t)
      ((and command-p (eql key #\f))
       (desk-leave desk)
       (lightfast:take-focus (desk-field desk))
       t)
      ((and command-p (eq key ':down))
       (shelf-step desk 1)
       t)
      ((and command-p (eq key ':up))
       (shelf-step desk -1)
       t)
      ((eq key ':page-down)
       (unslop--scroll desk page)
       t)
      ((eq key ':page-up)
       (unslop--scroll desk (- page))
       t)
      (t
       nil))))

(-> unslop--editor-key (desk key-event) boolean)
(defun unslop--editor-key (desk event)
  "Handle a key pressed in the editor before the editor sees it, and return whether it was used: Escape ends the writing, Tab goes to the next passage and Shift with Tab to the one before."
  (let ((key (key-event-key event)))
    (cond
      ((eq key ':escape)
       (desk-leave desk)
       t)
      ((eq key ':tab)
       (unless (page-step desk (if (key-event-shift-p event) -1 1))
         (desk-leave desk))
       (unslop--reveal desk)
       t)
      (t
       (unslop--shortcut desk event)))))

(-> unslop--stage-key (desk key-event) boolean)
(defun unslop--stage-key (desk event)
  "Handle a key pressed while no field has the keyboard: Tab begins with the first passage, and the keys that mean the same everywhere do what they do."
  (let ((key (key-event-key event)))
    (cond
      ((unslop--shortcut desk event)
       t)
      ((eq key ':tab)
       (when (page-step desk 1)
         (unslop--reveal desk))
       t)
      ((eq key ':home)
       (setf (desk-page-scroll desk) 0)
       (desk-redraw desk)
       t)
      ((eq key ':end)
       (setf (desk-page-scroll desk) most-positive-fixnum)
       (desk-redraw desk)
       t)
      (t
       nil))))

(-> unslop--field-key (desk key-event) boolean)
(defun unslop--field-key (desk event)
  "Handle a key pressed in the file filter: Escape empties it, or from an empty one gives the keyboard back, and Enter opens the first file listed."
  (let ((key   (key-event-key event))
        (field (desk-field desk)))
    (cond
      ((eq key ':escape)
       (if (plusp (length (desk-filter desk)))
           (setf (lightfast:value field) ""
                 (desk-filter desk)      "")
           (lightfast:take-focus (stage-canvas (desk-stage desk))))
       (desk-redraw desk)
       t)
      ((member key '(:enter :keypad-enter))
       (let ((first (find-if-not (lambda (name) (equal name (desk-name desk)))
                                 (shelf-names desk))))
         (when first
           (desk-open-file desk first)))
       (lightfast:take-focus (stage-canvas (desk-stage desk)))
       t)
      (t
       (unslop--shortcut desk event)))))


;;;; -- Construction --

(-> unslop--make-natives (desk t) list)
(defun unslop--make-natives (desk window)
  "Create the editor a passage is written in and the field the file filter is typed in, in WINDOW, and return them in that order."
  (let ((editor (lightfast:make-text-editor :parent window :x 0 :y 0 :width 200 :height 30))
        (field  (lightfast:make-input :parent window :x 0 :y 0 :width 200 :height 26))
        (style  (text-style ':code)))
    (lightfast:set-box editor lightfast:+box-flat-box+)
    (lightfast:set-text-font editor (theme-font (text-style-face style)))
    (lightfast:set-text-size editor (text-style-size style))
    (lightfast:text-set-wrap editor ':bounds)
    (lightfast:text-set-scrollbars editor ':none)
    (lightfast:on editor
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (when (with-stage-guard ("A key in the editor")
                            (unslop--editor-key desk (parse-key-event value)))
                      (lightfast:consume-event)))
                  :event lightfast:+event-key+)
    (lightfast:on editor
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (with-stage-guard ("Taking what was typed")
                      (desk-note-typing desk)))
                  :event lightfast:+event-change+)
    (lightfast:set-box field lightfast:+box-flat-box+)
    (lightfast:set-text-font field (theme-font ':mono))
    (lightfast:set-text-size field 14)
    (lightfast:set-when field ':changed)
    (lightfast:set-placeholder field "Find a file" :font (theme-font ':mono-italic))
    (lightfast:on field
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (setf (desk-filter desk)       (lightfast:value field)
                          (desk-shelf-scroll desk) 0)
                    (desk-redraw desk))
                  :event lightfast:+event-change+)
    (lightfast:on field
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (when (with-stage-guard ("A key in the file filter")
                            (unslop--field-key desk (parse-key-event value)))
                      (lightfast:consume-event)))
                  :event lightfast:+event-key+)
    (lightfast:hide editor)
    (list editor field)))

(-> unslop-build (desk unslop-launch) null)
(defun unslop-build (desk launch)
  "Create DESK's window, its stage and its native fields, at the size LAUNCH names."
  (let* ((size   (or (unslop-launch-size launch) *unslop-default-size*))
         (window (lightfast:make-window :width  (first size)
                                        :height (rest size)
                                        :label  (format nil "~A - unslop"
                                                        (string-right-trim
                                                         "/" (uiop:native-namestring
                                                              (checkout-root (desk-checkout desk)))))
                                        :app-id "unslop"))
         (stage  (make-stage window
                             :painter     (lambda (stage) (unslop--paint desk stage))
                             :key-handler (lambda (event) (unslop--stage-key desk event)))))
    (setf (desk-window desk)   window
          (desk-stage desk)    stage
          (desk-arranger desk) (lambda ()
                                 (unslop--reveal desk)
                                 (unslop-arrange desk)))
    (if (unslop-launch-size launch)
        (lightfast:set-size-range window
                                  :min-width  (first size)
                                  :min-height (rest size)
                                  :max-width  (first size)
                                  :max-height (rest size))
        (lightfast:set-size-range window
                                  :min-width  (first *unslop-minimum-size*)
                                  :min-height (rest *unslop-minimum-size*)))
    (lightfast:window-escape-closes window nil)
    (destructuring-bind (editor field) (unslop--make-natives desk window)
      (setf (stage-natives stage) (list editor field)
            (desk-editor desk)    editor
            (desk-field desk)     field))
    (lightfast:on-resize window
                         (lambda (resized)
                           (with-stage-guard ("Resizing the window")
                             (lightfast:refresh-geometry resized)
                             (stage-resize stage (lightfast:widget-width resized) (lightfast:widget-height resized))
                             (unslop-arrange desk))))
    (lightfast:on window
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (with-stage-guard ("Closing the window")
                      (desk-leave desk)))
                  :event lightfast:+event-close+))
  nil)

(-> unslop-make-desk (unslop-launch) desk)
(defun unslop-make-desk (launch)
  "Return the desk for what LAUNCH asks: the checkout read, its ledger opened, and what is completed and edited looked up."
  (let* ((checkout (checkout-open (uiop:parse-native-namestring (unslop-launch-directory launch))))
         (desk     (make-instance 'desk
                                  :checkout checkout
                                  :ledger   (ledger-open (checkout-root checkout))
                                  :files    (checkout-files checkout))))
    (desk-look desk :wait-p t)
    desk))


;;;; -- Running --

(-> unslop--picture (desk string) null)
(defun unslop--picture (desk path)
  "Write a picture of DESK's window to PATH: a PNG when PATH ends in png and an image tool is at hand, else a PPM."
  (let* ((png-p (string-equal (or (pathname-type path) "") "png"))
         (ppm   (if png-p (format nil "~A.ppm" path) path)))
    (lightfast:capture-window (desk-window desk) ppm)
    (when png-p
      (handler-case
          (progn
            (uiop:run-program (if (uiop:os-macosx-p)
                                  (list "sips" "-s" "format" "png" ppm "--out" path)
                                  (list "magick" ppm path)))
            (delete-file ppm))
        (error ()
          (format *error-output* "No image tool converted the picture; it is at ~A.~%" ppm)))))
  nil)

(-> unslop-run (unslop-launch) null)
(defun unslop-run (launch)
  "Show the unslop window LAUNCH asks for and run until it closes."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
  ;; A window that is only a picture remembers nothing.
  (unless (unslop-launch-shot launch)
    (setf *preferences-pathname* (uiop:xdg-config-home "aui" "unslop.sexp")))
  (let* ((desk   (unslop-make-desk launch))
         (unseen (and (unslop-launch-shot launch) (unslop-launch-size launch))))
    (unslop-build desk launch)
    (let ((file (or (unslop-launch-file launch)
                    (find-if-not (lambda (name) (desk-completed-p desk name)) (desk-files desk))
                    (first (desk-files desk)))))
      (when (and file (member file (desk-files desk) :test #'string=))
        (desk-open-file desk file)))
    (unless unseen
      (lightfast:show (desk-window desk))
      (lightfast:wait 0.05))
    (lightfast:refresh-geometry (desk-window desk))
    (stage-resize (desk-stage desk)
                  (lightfast:widget-width (desk-window desk))
                  (lightfast:widget-height (desk-window desk)))
    (unslop-arrange desk)
    (cond
      (unseen
       (unslop--picture desk (unslop-launch-shot launch)))
      (t
       (setf *stage-failure-function*
             (lambda (report)
               (setf (desk-notice desk) report)
               (desk-redraw desk)))
       (lightfast:add-timeout *refresh-seconds*
                              (lambda ()
                                (with-stage-guard ("Looking at the checkout")
                                  (desk-check-disk desk)
                                  (desk-look desk)))
                              :repeat t)
       (when (unslop-launch-shot launch)
         (lightfast:add-timeout 1.0
                                (lambda ()
                                  (unslop-arrange desk)
                                  (stage-flush (desk-stage desk))
                                  (lightfast:wait 0.3)
                                  (unslop--picture desk (unslop-launch-shot launch))
                                  (lightfast:quit))))
       (lightfast:run-with-idle
        (lambda ()
          (with-stage-guard ("Taking up what was found")
            (desk-take-up desk))
          (with-stage-guard ("Laying out the window")
            (when (stage-dirty-p (desk-stage desk))
              (unslop-arrange desk))
            (stage-flush (desk-stage desk)))))
       (desk-leave desk))))
  nil)

(-> unslop-main (&optional list) null)
(defun unslop-main (&optional (arguments (uiop:command-line-arguments)))
  "Run unslop with the command line ARGUMENTS and exit the process when the window closes."
  (setf (sb-ext:bytes-consed-between-gcs) (* 8 1024 1024))
  (let ((status 0))
    (handler-case
        (let ((launch (unslop-parse arguments)))
          (when launch
            (unslop-run launch)))
      ((or proof-error theme-error) (condition)
        (format *error-output* "~&unslop: ~A~%" condition)
        (setf status 64)))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code status :abort t))
  nil)

(-> unslop-image-toplevel () null)
(defun unslop-image-toplevel ()
  "Run unslop in a saved image with the process's own arguments."
  (unslop-main (rest sb-ext:*posix-argv*)))

(-> unslop-save-image (string) null)
(defun unslop-save-image (pathname)
  "Save this process as an executable at PATHNAME that starts unslop, and exit."
  (ensure-directories-exist pathname)
  (sb-ext:save-lisp-and-die pathname
                            :toplevel             #'unslop-image-toplevel
                            :executable           t
                            :save-runtime-options t)
  nil)
