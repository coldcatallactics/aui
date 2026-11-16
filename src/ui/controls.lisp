(in-package #:aui)

;;;; -- Controls --

;;; The few controls the interface draws itself: buttons, chips, badges and
;;; list rows. Each draws on the stage being painted and declares its own
;;; region. Hover inverts, and a primary control starts inverted, as Interlisp
;;; Paper has it: background and text always turn together.

(-> paint-button
    (stage t string
           &key (:x integer) (:y integer) (:width integer) (:height integer)
                (:action (or null function)) (:primary-p boolean) (:enabled-p boolean)
                (:mark (or null keyword)) (:mark-size (or null integer)) (:style (or null text-style))
                (:tooltip (or null string)))
    null)
(defun paint-button (stage identifier label
                     &key (x 0) (y 0) (width 0) (height 0) action primary-p (enabled-p t) mark mark-size style
                          tooltip)
  "Draw a button with LABEL in the box and declare its region as IDENTIFIER.

A primary button is filled; the pointer over a button swaps it. A button
that is not ENABLED-P has a dashed frame and no action. MARK draws a mark
MARK-SIZE units across before the label, or alone when LABEL is empty. Beside
a label the mark sits on the middle of the label's letters, not of the box."
  (let* ((style    (or style (text-style ':label-bold)))
         (hot-p    (and enabled-p (stage-hot-p stage identifier)))
         (filled-p (if primary-p (not hot-p) hot-p)))
    (flet ((content ()
             (let* ((mark-size (or mark-size (min 16 (- height 8))))
                    (label-w   (text-width label style))
                    (gap       (if (and mark (plusp (length label))) 7 0))
                    (content-w (+ (if mark mark-size 0) gap label-w))
                    (start     (+ x (floor (- width content-w) 2))))
               (when mark
                 (paint-mark mark start
                             (if (plusp (length label))
                                 (multiple-value-bind (ascent descent) (text-metrics style)
                                   (- (+ y (floor (- height (+ ascent descent)) 2) ascent)
                                      (round (* 3/10 (text-style-size style)))
                                      (floor mark-size 2)))
                                 (+ y (floor (- height mark-size) 2)))
                             :size mark-size))
               (when (plusp (length label))
                 (paint-text-in label style
                                :x      (+ start (if mark (+ mark-size gap) 0))
                                :y      y
                                :width  (max 0 (- (+ x width) start (if mark (+ mark-size gap) 0)))
                                :height height)))))
      (cond
        ((and enabled-p filled-p)
         (with-inversion (x y width height)
           (content)))
        (t
         (paint-fill x y :width width :height height :color (ground-color))
         (content)))
      (unless (and enabled-p filled-p)
        (paint-frame x y :width width :height height :dashed-p (not enabled-p))))
    (when (and enabled-p action)
      (stage-region stage identifier :x x :y y :width width :height height :action action :tooltip tooltip)))
  nil)

(defclass button-block (flow-block)
  ((identifier
    :initarg       :identifier
    :reader        button-block-identifier
    :documentation "Names the button's region.")
   (label
    :initarg       :label
    :reader        button-block-label
    :type          string
    :documentation "The words on the button.")
   (action
    :initarg       :action
    :reader        button-block-action
    :type          function
    :documentation "Called when the button is clicked."))
  (:documentation "A button standing in the flow of a conversation, as wide as its words."))

(defmethod block-layout ((block button-block) width)
  "A button is one row high whatever the width."
  (declare (ignore width))
  30)

(defmethod block-paint ((block button-block) x y)
  "Draw the button at the block's left edge, as wide as its label needs."
  (when *paint-stage*
    (let ((label (button-block-label block)))
      (paint-button *paint-stage* (button-block-identifier block) label
                    :x      x
                    :y      y
                    :width  (min (block-width block) (+ 28 (text-width label (text-style ':label-bold))))
                    :height (block-height block)
                    :action (button-block-action block))))
  nil)

(defmethod block-leaves ((block button-block))
  "A button holds no text to select."
  nil)

(-> chip-width (string &key (:menu-p boolean)) integer)
(defun chip-width (label &key menu-p)
  "Return the width a chip showing LABEL takes."
  (+ 16 (text-width label (text-style ':meta)) (if menu-p 14 0)))

(-> paint-chip
    (stage t string
           &key (:x integer) (:y integer) (:height integer) (:action (or null function)) (:menu-p boolean)
                (:tooltip (or null string)) (:mark-color (or null list)))
    integer)
(defun paint-chip (stage identifier label &key (x 0) (y 0) (height 24) action menu-p tooltip mark-color)
  "Draw a chip, a small unframed label that inverts under the pointer, at X, Y and return its width.

MENU-P adds the small triangle that says the chip opens a menu. MARK-COLOR
puts a dot of that color before the label."
  (let* ((style (text-style ':meta))
         (dot   (if mark-color 12 0))
         (width (+ (chip-width label :menu-p menu-p) dot))
         (hot-p (and action (stage-hot-p stage identifier))))
    (flet ((content ()
             (when mark-color
               (paint-mark ':dot (+ x 4) (+ y (floor (- height 12) 2))
                           :size  12
                           :color (if hot-p (accent-color) mark-color)))
             (paint-text-in label style :x (+ x 8 dot) :y y :width (- width 16 dot) :height height)
             (when menu-p
               (paint-mark ':open (- (+ x width) 18) (+ y (floor (- height 10) 2)) :size 10))))
      (if hot-p
          (with-inversion (x y width height)
            (content))
          (content)))
    (when action
      (stage-region stage identifier :x x :y y :width width :height height :action action :tooltip tooltip))
    width))

(-> paint-badge (string &key (:right integer) (:y integer) (:height integer)) integer)
(defun paint-badge (text &key (right 0) (y 0) (height 18))
  "Draw TEXT as a small inverted badge whose right edge is at RIGHT, and return its width."
  (let* ((style (text-style ':meta-bold))
         (width (+ 10 (text-width text style)))
         (box-h (min height 18))
         (box-y (+ y (floor (- height box-h) 2))))
    (with-inversion ((- right width) box-y width box-h)
      (paint-text-in text style :x (- right width) :y box-y :width width :height box-h :align ':center))
    width))

(-> paint-window-frame (integer integer &key (:width integer) (:height integer) (:title (or null string))) integer)
(defun paint-window-frame (x y &key (width 0) (height 0) title)
  "Draw a window in the Interlisp manner: a paper box with a hairline, a dither shadow, and a black title bar when TITLE is given.

Return the height the title bar took, zero without a title."
  (let ((bar (if title 28 0)))
    (paint-dither (+ x *shadow-offset*) (+ y height) :width width :height *shadow-offset*)
    (paint-dither (+ x width) (+ y *shadow-offset*) :width *shadow-offset* :height (- height *shadow-offset*))
    (paint-fill x y :width width :height height :color *paper*)
    (paint-frame x y :width width :height height)
    (when title
      (with-inversion (x y width bar)
        (paint-text-in title (text-style ':label-bold) :x (+ x 10) :y y :width (- width 20) :height bar)))
    bar))
