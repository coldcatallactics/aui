(in-package #:aui)

;;;; -- Stage --

;;; The window's one drawing surface. Everything AUI shows is drawn here by
;;; its own code; FLTK contributes only the few native fields a person types
;;; into, laid on top. While it draws, a pane declares the regions that
;;; respond to the pointer; the stage keeps them from one drawing to the
;;; next, finds the region under the pointer, and tells it about clicks,
;;; drags and the wheel. Nothing is retained but that list.

(defstruct region
  "A rectangle of the stage that responds to the pointer, declared while drawing."
  (identifier nil)
  (x          0   :type integer)
  (y          0   :type integer)
  (width      0   :type integer)
  (height     0   :type integer)
  (action     nil :type (or null function))
  (menu       nil :type (or null function))
  (wheel      nil :type (or null function))
  (drag       nil :type (or null function))
  (cursor     ':default :type keyword)
  (tooltip    nil :type (or null string)))

(defclass stage ()
  ((window
    :initarg       :window
    :reader        stage-window
    :documentation "The window the stage fills.")
   (canvas
    :initform      nil
    :accessor      stage-canvas
    :documentation "The canvas everything is drawn on.")
   (width
    :initform      0
    :accessor      stage-width
    :type          integer
    :documentation "The stage's current width in FLTK units.")
   (height
    :initform      0
    :accessor      stage-height
    :type          integer
    :documentation "The stage's current height in FLTK units.")
   (painter
    :initarg       :painter
    :accessor      stage-painter
    :type          function
    :documentation "Called with the stage to draw everything and declare its regions.")
   (key-handler
    :initarg       :key-handler
    :initform      nil
    :accessor      stage-key-handler
    :type          (or null function)
    :documentation "Called with a key event when the stage has the keyboard; returns true when it used the key.")
   (drop-handler
    :initarg       :drop-handler
    :initform      nil
    :accessor      stage-drop-handler
    :type          (or null function)
    :documentation "Called with what was dropped on the stage: the dragged text, or file names one to a line.")
   (regions
    :initform      nil
    :accessor      stage--regions
    :type          list
    :documentation "The regions declared by the last drawing, topmost first.")
   (drawing
    :initform      nil
    :accessor      stage--drawing
    :type          list
    :documentation "The regions being declared by the drawing under way.")
   (hot
    :initform      nil
    :accessor      stage-hot
    :documentation "The identifier of the region under the pointer, or NIL.")
   (active
    :initform      nil
    :accessor      stage-active
    :documentation "The identifier of the region the pointer was pressed in, until release.")
   (pointer-x
    :initform      -1
    :accessor      stage-pointer-x
    :type          integer
    :documentation "The pointer's last known X, or -1 when it is outside.")
   (pointer-y
    :initform      -1
    :accessor      stage-pointer-y
    :type          integer
    :documentation "The pointer's last known Y, or -1 when it is outside.")
   (cursor
    :initform      ':default
    :accessor      stage--cursor
    :type          keyword
    :documentation "The cursor shape last set.")
   (dirty-p
    :initform      t
    :accessor      stage-dirty-p
    :type          boolean
    :documentation "Whether a redraw was asked for and not yet handed to FLTK.")
   (natives
    :initform      nil
    :accessor      stage-natives
    :type          list
    :documentation "The native widgets lying over the canvas, redrawn after it so its paint never covers them.")
   (animating-p
    :initform      nil
    :accessor      stage-animating-p
    :type          boolean
    :documentation "Whether the last drawing showed something that moves, so the stage keeps a clock running.")
   (phase
    :initform      0
    :accessor      stage-phase
    :type          integer
    :documentation "A counter that advances while something moves; marks derive their frame from it."))
  (:documentation "The window's custom-drawn surface and its pointer and keyboard dispatch."))


;;;; -- Guarding --

(defvar *stage-failure-function* nil
  "Called with a report string when a drawing or input handler signals an error, or NIL to only log it.")

(defmacro with-stage-guard ((what) &body body)
  "Run BODY and return its values; when it fails, report WHAT failed and return NIL instead of unwinding into FLTK.

A handler runs inside one of FLTK's callbacks, where a Lisp error, or a
stack or heap run out, has nowhere to go but through foreign frames. The
window stays up, the failure goes to standard error, and the interface is
told so that it can say so."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (serious-condition (,condition)
         (let ((report (format nil "~A failed: ~A" ,what ,condition)))
           (format *error-output* "~&aui: ~A~%" report)
           (when *stage-failure-function*
             (funcall *stage-failure-function* report)))
         nil))))


;;;; -- Clipping --

(defvar *clip* nil
  "The box (X Y WIDTH HEIGHT) that what is drawn now is cut to, or NIL when nothing cuts it.")

(-> clip--within (list (or null list)) list)
(defun clip--within (box clip)
  "Return the part of BOX, an (X Y WIDTH HEIGHT) list, that lies within CLIP, a box or NIL for everywhere."
  (if (null clip)
      box
      (destructuring-bind (x y width height) box
        (destructuring-bind (clip-x clip-y clip-width clip-height) clip
          (let ((left (max x clip-x))
                (top  (max y clip-y)))
            (list left
                  top
                  (max 0 (- (min (+ x width) (+ clip-x clip-width)) left))
                  (max 0 (- (min (+ y height) (+ clip-y clip-height)) top))))))))

(defmacro with-clip ((x y width height) &body body)
  "Run BODY with what it draws cut to the box X, Y, WIDTH, HEIGHT.

The regions BODY declares are cut to the box as well, so nothing that is
hidden answers the pointer. The four forms are evaluated once, in order."
  `(let ((*clip* (clip--within (list ,x ,y ,width ,height) *clip*)))
     (lightfast:with-clip ((first *clip*) (second *clip*) (third *clip*) (fourth *clip*))
       ,@body)))


;;;; -- Regions --

(-> stage-region
    (stage t
           &key (:x integer) (:y integer) (:width integer) (:height integer)
                (:action (or null function)) (:menu (or null function)) (:wheel (or null function))
                (:drag (or null function)) (:cursor keyword) (:tooltip (or null string)))
    boolean)
(defun stage-region (stage identifier
                     &key (x 0) (y 0) (width 0) (height 0) action menu wheel drag (cursor ':default) tooltip)
  "Declare that the box responds to the pointer as IDENTIFIER, and return whether the pointer is over it now.

ACTION is called with no arguments on a click; MENU on a secondary click;
WHEEL with the vertical and horizontal steps; DRAG with a keyword, :START,
:MOVE or :END, and the pointer's position. Regions declared later lie above
earlier ones. A region with an ACTION shows the hand unless CURSOR says
otherwise."
  (destructuring-bind (x y width height) (clip--within (list x y width height) *clip*)
    ;; A region wholly outside what is drawn is not there to be pointed at.
    (when (or (null *clip*) (and (plusp width) (plusp height)))
      (push (make-region :identifier identifier :x x :y y :width width :height height
                         :action action :menu menu :wheel wheel :drag drag
                         :cursor  (if (and action (eq cursor ':default)) ':hand cursor)
                         :tooltip tooltip)
            (stage--drawing stage))))
  (and (equal identifier (stage-hot stage)) t))

(-> stage-hot-p (stage t) boolean)
(defun stage-hot-p (stage identifier)
  "Return T when the pointer is over the region IDENTIFIER."
  (and (stage-hot stage) (equal identifier (stage-hot stage)) t))

(-> stage-pressed-p (stage t) boolean)
(defun stage-pressed-p (stage identifier)
  "Return T while the pointer is held down in the region IDENTIFIER."
  (and (stage-active stage) (equal identifier (stage-active stage)) t))

(-> stage--region-at (stage integer integer &key (:test (or null function))) (or null region))
(defun stage--region-at (stage x y &key test)
  "Return the topmost region containing X, Y that satisfies TEST, or NIL."
  (find-if (lambda (region)
             (and (<= (region-x region) x)
                  (< x (+ (region-x region) (region-width region)))
                  (<= (region-y region) y)
                  (< y (+ (region-y region) (region-height region)))
                  (or (null test) (funcall test region))))
           (stage--regions stage)))

(-> stage--find-region (stage t) (or null region))
(defun stage--find-region (stage identifier)
  "Return the region declared as IDENTIFIER by the last drawing, or NIL."
  (find identifier (stage--regions stage) :key #'region-identifier :test #'equal))


;;;; -- Redrawing --

(-> stage-redraw (stage) null)
(defun stage-redraw (stage)
  "Ask for the stage to be drawn again before the interface next waits."
  (setf (stage-dirty-p stage) t)
  nil)

(-> stage-flush (stage) null)
(defun stage-flush (stage)
  "Hand a pending redraw to FLTK, with the native widgets after the canvas."
  (when (and (stage-dirty-p stage) (stage-canvas stage))
    (setf (stage-dirty-p stage) nil)
    (lightfast:redraw (stage-canvas stage))
    (dolist (native (stage-natives stage))
      (lightfast:redraw native)))
  nil)

(-> stage--paint (stage) null)
(defun stage--paint (stage)
  "Draw the stage: paper, then everything the painter draws, collecting its regions."
  (setf (stage--drawing stage)   nil
        (stage-animating-p stage) nil)
  (let ((*on-ink-p* nil))
    (paint-fill 0 0 :width (stage-width stage) :height (stage-height stage) :color *paper*)
    (funcall (stage-painter stage) stage))
  (setf (stage--regions stage) (stage--drawing stage)
        (stage--drawing stage) nil)
  ;; The pointer has not moved, but what lies under it may have.
  (stage--track stage (stage-pointer-x stage) (stage-pointer-y stage) :redraw-p nil)
  nil)

(-> stage--track (stage integer integer &key (:redraw-p boolean)) null)
(defun stage--track (stage x y &key (redraw-p t))
  "Record the pointer at X, Y, and update the hot region and the cursor."
  (setf (stage-pointer-x stage) x
        (stage-pointer-y stage) y)
  (let* ((region (and (>= x 0) (>= y 0) (stage--region-at stage x y)))
         (hot    (and region (region-identifier region)))
         (cursor (if region (region-cursor region) ':default)))
    (unless (equal hot (stage-hot stage))
      (setf (stage-hot stage) hot)
      (lightfast:set-tooltip (stage-canvas stage) (and region (region-tooltip region)))
      (when redraw-p
        (stage-redraw stage)))
    (unless (eq cursor (stage--cursor stage))
      (setf (stage--cursor stage) cursor)
      (lightfast:set-cursor (stage-canvas stage) cursor)))
  nil)


;;;; -- Events --

(-> stage--numbers (string) list)
(defun stage--numbers (value)
  "Return the integers in the space-separated event VALUE."
  (let ((numbers nil)
        (start   0))
    (loop
      (let ((end (position #\Space value :start start)))
        (let ((number (parse-integer value :start start :end end :junk-allowed t)))
          (when number
            (push number numbers)))
        (if end
            (setf start (1+ end))
            (return))))
    (nreverse numbers)))

(-> stage--pointer (stage keyword string) null)
(defun stage--pointer (stage kind value)
  "Handle the pointer event KIND, whose VALUE holds position, button, wheel steps and modifier state."
  (destructuring-bind (&optional (x -1) (y -1) (button 0) (dx 0) (dy 0) &rest state) (stage--numbers value)
    (declare (ignore state))
    (ecase kind
      ((:move :enter)
       (stage--track stage x y))
      (:leave
       (stage--track stage -1 -1))
      (:push
       (stage--track stage x y)
       (let ((region (stage--region-at stage x y)))
         (setf (stage-active stage) (and region (region-identifier region)))
         (when (and region (region-drag region) (= button 1))
           (funcall (region-drag region) ':start x y))
         (stage-redraw stage)))
      (:drag
       (setf (stage-pointer-x stage) x
             (stage-pointer-y stage) y)
       (let ((region (and (stage-active stage) (stage--find-region stage (stage-active stage)))))
         (when (and region (region-drag region))
           (funcall (region-drag region) ':move x y)
           (stage-redraw stage))))
      (:release
       (let* ((active (stage-active stage))
              (region (and active (stage--find-region stage active)))
              (under  (stage--region-at stage x y)))
         (setf (stage-active stage) nil)
         (when region
           (when (region-drag region)
             (funcall (region-drag region) ':end x y))
           (when (and under (equal (region-identifier under) active))
             (cond
               ((and (= button 3) (region-menu region))
                (funcall (region-menu region)))
               ((and (= button 1) (region-action region))
                (funcall (region-action region))))))
         (stage--track stage x y)
         (stage-redraw stage)))
      (:wheel
       (let ((region (stage--region-at stage x y :test #'region-wheel)))
         (when region
           (funcall (region-wheel region) dy dx)
           (stage-redraw stage))))))
  nil)

(defstruct key-event
  "One key press, as FLTK's key code, the modifier state bits and the text the key types."
  (code  0  :type integer)
  (state 0  :type integer)
  (text  "" :type string))

(-> parse-key-event (string) key-event)
(defun parse-key-event (value)
  "Return the key event the callback VALUE describes: a key code, a modifier state and the typed text."
  (let* ((first-space  (position #\Space value))
         (second-space (and first-space (position #\Space value :start (1+ first-space)))))
    (make-key-event :code  (or (parse-integer value :end first-space :junk-allowed t) 0)
                    :state (or (and first-space
                                    (parse-integer value :start (1+ first-space) :end second-space
                                                         :junk-allowed t))
                               0)
                    :text  (if second-space (subseq value (1+ second-space)) ""))))

(defparameter *key-codes*
  '(:escape #xff1b :enter #xff0d :keypad-enter #xff8d :tab #xff09 :backspace #xff08 :delete #xffff
    :up #xff52 :down #xff54 :left #xff51 :right #xff53 :home #xff50 :end #xff57
    :page-up #xff55 :page-down #xff56)
  "FLTK's codes for the keys the interface names.")

(-> key-event-key (key-event) (or keyword character null))
(defun key-event-key (event)
  "Return EVENT's key as a keyword for a named key, the lowercase character for a letter, digit or sign, or NIL."
  (let ((code (key-event-code event)))
    (or (loop for (name value) on *key-codes* by (partial #'nthcdr 2)
              when (= value code)
                do (return name))
        (and (< 31 code 127) (char-downcase (code-char code))))))

(-> key-event-shift-p (key-event) boolean)
(defun key-event-shift-p (event)
  "Return T when Shift was held."
  (logtest #x10000 (key-event-state event)))

(-> key-event-control-p (key-event) boolean)
(defun key-event-control-p (event)
  "Return T when Control was held."
  (logtest #x40000 (key-event-state event)))

(-> key-event-alt-p (key-event) boolean)
(defun key-event-alt-p (event)
  "Return T when Alt, or Option on a Mac, was held."
  (logtest #x80000 (key-event-state event)))

(-> key-event-command-p (key-event) boolean)
(defun key-event-command-p (event)
  "Return T when the platform's command modifier was held: Command on a Mac, Control elsewhere."
  (logtest (if (uiop:os-macosx-p) #x400000 #x40000) (key-event-state event)))

(-> make-stage
    (t &key (:painter function) (:key-handler (or null function)) (:drop-handler (or null function))) stage)
(defun make-stage (window &key painter key-handler drop-handler)
  "Fill WINDOW with a stage drawn by PAINTER, sending keys to KEY-HANDLER and drops to DROP-HANDLER, and return it."
  (let* ((stage  (make-instance 'stage :window window :painter painter :key-handler key-handler
                                       :drop-handler drop-handler))
         (canvas (lightfast:make-canvas
                  :parent   window
                  :x        0
                  :y        0
                  :width    (lightfast:widget-width window)
                  :height   (lightfast:widget-height window)
                  :callback (lambda (widget event value)
                              (declare (ignore widget event value))
                              ;; A drawing that failed asks for no other, which would fail the same way.
                              (unless (with-stage-guard ("Drawing")
                                        (stage--paint stage)
                                        t)
                                (setf (stage-dirty-p stage) nil))))))
    (setf (stage-canvas stage) canvas
          (stage-width stage)  (lightfast:widget-width window)
          (stage-height stage) (lightfast:widget-height window))
    (lightfast:set-box canvas lightfast:+box-no-box+)
    (lightfast:canvas-set-focusable canvas t)
    (flet ((pointer (kind)
             (lambda (widget event value)
               (declare (ignore widget event))
               (with-stage-guard ("A pointer action")
                 (stage--pointer stage kind value)))))
      (lightfast:on canvas (pointer ':push) :event lightfast:+event-push+)
      (lightfast:on canvas (pointer ':drag) :event lightfast:+event-drag+)
      (lightfast:on canvas (pointer ':release) :event lightfast:+event-release+)
      (lightfast:on canvas (pointer ':wheel) :event lightfast:+event-wheel+)
      (lightfast:on canvas (pointer ':move) :event lightfast:+event-move+)
      (lightfast:on canvas (pointer ':enter) :event lightfast:+event-enter+)
      (lightfast:on canvas (pointer ':leave) :event lightfast:+event-leave+))
    (lightfast:on canvas
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (let ((handler (stage-drop-handler stage)))
                      (when handler
                        (with-stage-guard ("A drop")
                          (funcall handler value)))))
                  :event lightfast:+event-drop+)
    (lightfast:on canvas
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (let ((handler (stage-key-handler stage)))
                      (when handler
                        (with-stage-guard ("A key")
                          (funcall handler (parse-key-event value))))))
                  :event lightfast:+event-key+)
    stage))

(-> stage-resize (stage integer integer) null)
(defun stage-resize (stage width height)
  "Give the stage and its canvas the size WIDTH by HEIGHT."
  (setf (stage-width stage)  width
        (stage-height stage) height)
  (lightfast:resize-widget (stage-canvas stage) :x 0 :y 0 :width width :height height)
  (stage-redraw stage)
  nil)
