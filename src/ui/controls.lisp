(in-package #:aui)

;;;; -- Controls --

;;; Stage controls: buttons, chips, badges, and window frames.

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
  "Draw a button labelled LABEL in the box and declare the region IDENTIFIER for it.

A primary button is filled. If the pointer is over a button, the fill is
swapped. If ENABLED-P is false, the button has a dashed frame and no action.
If MARK is given, the mark is drawn MARK-SIZE units wide before the label, or
alone if LABEL is empty. Next to a label, the mark is centered on the
letters of the label."
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
    :documentation "The identifier used for the region of the button.")
   (label
    :initarg       :label
    :reader        button-block-label
    :type          string
    :documentation "The label of the button.")
   (action
    :initarg       :action
    :reader        button-block-action
    :type          function
    :documentation "The function called if the button is clicked."))
  (:documentation "A button in the flow of a conversation. The button is as wide as the label."))

(defmethod block-layout ((block button-block) width)
  "Return the fixed height of a button."
  (declare (ignore width))
  30)

(defmethod block-paint ((block button-block) x y)
  "Draw the button at the left edge of the block. The button is as wide as the label."
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
  "Return NIL, because a button has no selectable text."
  nil)

(-> chip-width (string &key (:menu-p boolean)) integer)
(defun chip-width (label &key menu-p)
  "Return the width of a chip labelled LABEL."
  (+ 16 (text-width label (text-style ':meta)) (if menu-p 14 0)))

(-> paint-chip
    (stage t string
           &key (:x integer) (:y integer) (:height integer) (:action (or null function)) (:menu-p boolean)
                (:tooltip (or null string)) (:mark-color (or null list)))
    integer)
(defun paint-chip (stage identifier label &key (x 0) (y 0) (height 24) action menu-p tooltip mark-color)
  "Draw a chip at X, Y and return the width of the chip. A chip is a small label without a frame. It is inverted under the pointer.

If MENU-P is true, a small triangle is drawn to show that the chip has a
menu. If MARK-COLOR is given, a dot of that color is drawn before the label."
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
  "Draw TEXT as a small inverted badge and return the width of the badge. The right edge of the badge is at RIGHT."
  (let* ((style (text-style ':meta-bold))
         (width (+ 10 (text-width text style)))
         (box-h (min height 18))
         (box-y (+ y (floor (- height box-h) 2))))
    (with-inversion ((- right width) box-y width box-h)
      (paint-text-in text style :x (- right width) :y box-y :width width :height box-h :align ':center))
    width))

(-> paint-window-frame (integer integer &key (:width integer) (:height integer) (:title (or null string))) integer)
(defun paint-window-frame (x y &key (width 0) (height 0) title)
  "Draw a window frame as in Interlisp. The frame consists of a paper box, a hairline around it and a dither shadow. If TITLE is given, a black title bar is drawn.

Returns the height of the title bar, or zero if there is no title."
  (let ((bar (if title 28 0)))
    (paint-dither (+ x *shadow-offset*) (+ y height) :width width :height *shadow-offset*)
    (paint-dither (+ x width) (+ y *shadow-offset*) :width *shadow-offset* :height (- height *shadow-offset*))
    (paint-fill x y :width width :height height :color *paper*)
    (paint-frame x y :width width :height height)
    (when title
      (with-inversion (x y width bar)
        (paint-text-in title (text-style ':label-bold) :x (+ x 10) :y y :width (- width 20) :height bar)))
    bar))
