(in-package #:aui)

;;;; -- Painting --

;;; Drawing in the theme's terms, valid only inside the stage's draw callback.
;;; Every primitive takes its color from the ground it is drawn on unless told
;;; otherwise: ink on paper, paper on ink. An inverted block is therefore one
;;; fill and a rebinding, and everything drawn inside it turns with it.

(defvar *on-ink-p* nil
  "Whether drawing currently happens on an inverted, ink-filled ground.")

(-> ink-color () list)
(defun ink-color ()
  "Return the color text and lines take on the current ground."
  (if *on-ink-p* *paper* *ink*))

(-> ground-color () list)
(defun ground-color ()
  "Return the color of the current ground."
  (if *on-ink-p* *ink* *paper*))

(-> accent-color () list)
(defun accent-color ()
  "Return the brand green as it reads on the current ground."
  (if *on-ink-p* *green-light* *green*))

(-> ground-tint (list) list)
(defun ground-tint (color)
  "Return COLOR as it reads on the current ground: an accent ink turns to its lighter form on an inverted one."
  (or (and *on-ink-p*
           (rest (assoc color *accents-on-ink* :test #'equal)))
      color))

(-> paint--color (list) null)
(defun paint--color (color)
  "Make COLOR, a (RED GREEN BLUE) list, the drawing color."
  (lightfast:draw-color-rgb :red (first color) :green (second color) :blue (third color))
  nil)

(-> paint-fill (integer integer &key (:width integer) (:height integer) (:color (or null list))) null)
(defun paint-fill (x y &key (width 0) (height 0) color)
  "Fill the box with COLOR, the ground's ink by default."
  (when (and (plusp width) (plusp height))
    (paint--color (or color (ink-color)))
    (lightfast:draw-filled-rect x y width height))
  nil)

(-> paint-frame
    (integer integer &key (:width integer) (:height integer) (:color (or null list)) (:dashed-p boolean)) null)
(defun paint-frame (x y &key (width 0) (height 0) color dashed-p)
  "Draw a hairline around the box, dashed when DASHED-P, which is how a disabled control reads."
  (when (and (plusp width) (plusp height))
    (paint--color (or color (ink-color)))
    (when dashed-p
      (lightfast:draw-line-style :dashed t))
    (lightfast:draw-rect x y width height)
    (when dashed-p
      (lightfast:draw-line-style)))
  nil)

(-> paint-line (integer integer &key (:x2 integer) (:y2 integer) (:color (or null list)) (:width integer)) null)
(defun paint-line (x1 y1 &key (x2 x1) (y2 y1) color (width 0))
  "Draw a line from X1, Y1 to X2, Y2, WIDTH units thick, a hairline by default."
  (paint--color (or color (ink-color)))
  (when (plusp width)
    (lightfast:draw-line-style :width width))
  (lightfast:draw-line x1 y1 x2 y2)
  (when (plusp width)
    (lightfast:draw-line-style))
  nil)

(-> paint-dither
    (integer integer &key (:width integer) (:height integer) (:step integer) (:color (or null list))) null)
(defun paint-dither (x y &key (width 0) (height 0) (step 3) color)
  "Fill the box with one-bit dither in COLOR: STEP 3 a medium tone, 5 a light one."
  (when (and (plusp width) (plusp height))
    (let ((color (or color (ink-color))))
      (lightfast:draw-dither x y width height
                             :step step :red (first color) :green (second color) :blue (third color))))
  nil)

(defmacro with-inversion ((x y width height) &body body)
  "Fill the box with the ground's ink and run BODY drawing on it, so text and marks inside turn with it."
  (let ((box-x (gensym "X")) (box-y (gensym "Y")) (box-w (gensym "W")) (box-h (gensym "H")))
    `(let ((,box-x ,x) (,box-y ,y) (,box-w ,width) (,box-h ,height))
       (paint-fill ,box-x ,box-y :width ,box-w :height ,box-h)
       (let ((*on-ink-p* (not *on-ink-p*)))
         ,@body))))

(-> paint-text (string integer integer &key (:style text-style) (:color (or null list))) integer)
(defun paint-text (text x baseline &key (style (text-style ':body)) color)
  "Draw TEXT in STYLE with its baseline at BASELINE and its start at X, and return the X where it ends."
  (let ((width (text-width text style)))
    (when (plusp (length text))
      (let ((color (or color (text-style-color style))))
        (paint--color (if color (ground-tint color) (ink-color)))
        (lightfast:draw-font :font (theme-font (text-style-face style)) :size (text-style-size style))
        (lightfast:draw-text text x baseline)
        (when (text-style-underline-p style)
          (lightfast:draw-line x (+ baseline 2) (+ x width) (+ baseline 2)))
        (when (text-style-strike-p style)
          (let ((middle (- baseline (floor (text-style-size style) 3))))
            (lightfast:draw-line x middle (+ x width) middle)))))
    (+ x width)))

(-> paint-text-in
    (string text-style &key (:x integer) (:y integer) (:width integer) (:height integer) (:align keyword)
            (:color (or null list)))
    null)
(defun paint-text-in (text style &key (x 0) (y 0) (width 0) (height 0) (align ':left) color)
  "Draw one line of TEXT in STYLE inside the box, centered on its height and aligned :LEFT, :CENTER or :RIGHT.

Text wider than the box is cut at the box's edge with an ellipsis."
  (let* ((fitted   (text-fit text style width))
         (measured (text-width fitted style))
         (start    (ecase align
                     (:left
                      x)
                     (:center
                      (+ x (floor (- width measured) 2)))
                     (:right
                      (+ x (- width measured)))))
         (ascent   (text-metrics style))
         (descent  (nth-value 1 (text-metrics style)))
         (baseline (+ y (floor (- height (+ ascent descent)) 2) ascent)))
    (paint-text fitted start baseline :style style :color color))
  nil)

(-> text-fit (string text-style integer) string)
(defun text-fit (text style width)
  "Return TEXT, or its longest beginning followed by an ellipsis that fits WIDTH in STYLE."
  (if (<= (text-width text style) width)
      text
      (let ((ellipsis "…"))
        (loop for end from (1- (length text)) downto 1
              for candidate = (concatenate 'string
                                           (string-right-trim " " (subseq text 0 end))
                                           ellipsis)
              when (<= (text-width candidate style) width)
                do (return candidate)
              finally (return ellipsis)))))

(-> text-fit-tail (string text-style integer) string)
(defun text-fit-tail (text style width)
  "Return TEXT, or an ellipsis followed by its longest ending that fits WIDTH in STYLE.

A path says most at its end, so that is the end kept."
  (if (<= (text-width text style) width)
      text
      (let ((ellipsis "…"))
        (loop for start from 1 below (length text)
              for candidate = (concatenate 'string ellipsis (subseq text start))
              when (<= (text-width candidate style) width)
                do (return candidate)
              finally (return ellipsis)))))


;;;; -- Marks --

(-> paint-mark (keyword integer integer &key (:size integer) (:color (or null list))) null)
(defun paint-mark (mark x y &key (size 14) color)
  "Draw the small mark MARK in a SIZE by SIZE box whose top-left corner is X, Y.

Marks are drawn from lines and polygons rather than taken from a font, so
they look the same whatever faces the system has."
  (let* ((color  (if color (ground-tint color) (ink-color)))
         (middle (floor size 2))
         (inset  (max 2 (floor size 5)))
         (left   (+ x inset))
         (right  (+ x size (- inset)))
         (top    (+ y inset))
         (bottom (+ y size (- inset))))
    (paint--color color)
    (ecase mark
      (:closed
       (lightfast:draw-polygon (list left top right (+ y middle) left bottom) :filled t))
      (:open
       (lightfast:draw-polygon (list left top right top (+ x middle) bottom) :filled t))
      (:dot
       (lightfast:draw-filled-circle (+ x middle) (+ y middle) (max 2 (- middle inset))))
      (:point
       (lightfast:draw-filled-rect (+ x middle -1) (+ y middle -1) 3 3))
      (:ring
       (lightfast:draw-circle (+ x middle) (+ y middle) (max 2 (- middle inset))))
      (:square
       (lightfast:draw-filled-rect left top (- right left) (- bottom top)))
      (:check
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line left (+ y middle) (+ x middle (- 1)) bottom)
       (lightfast:draw-line (+ x middle (- 1)) bottom right top)
       (lightfast:draw-line-style))
      (:cross
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line left top right bottom)
       (lightfast:draw-line left bottom right top)
       (lightfast:draw-line-style))
      (:plus
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line left (+ y middle) right (+ y middle))
       (lightfast:draw-line (+ x middle) top (+ x middle) bottom)
       (lightfast:draw-line-style))
      (:arrow-up
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line (+ x middle) top (+ x middle) bottom)
       (lightfast:draw-line left (+ y middle) (+ x middle) top)
       (lightfast:draw-line right (+ y middle) (+ x middle) top)
       (lightfast:draw-line-style))
      (:arrow-down
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line (+ x middle) top (+ x middle) bottom)
       (lightfast:draw-line left (+ y middle) (+ x middle) bottom)
       (lightfast:draw-line right (+ y middle) (+ x middle) bottom)
       (lightfast:draw-line-style))
      (:panel
       (lightfast:draw-rect left top (- right left) (- bottom top))
       (lightfast:draw-filled-rect left top (max 2 (floor (- right left) 3)) (- bottom top)))
      (:search
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-circle (+ left (floor (- right left) 3)) (+ top (floor (- bottom top) 3))
                              (max 2 (floor (- right left) 3)))
       (lightfast:draw-line (+ x middle 1) (+ y middle 1) right bottom)
       (lightfast:draw-line-style))
      (:menu
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line left top right top)
       (lightfast:draw-line left (+ y middle) right (+ y middle))
       (lightfast:draw-line left bottom right bottom)
       (lightfast:draw-line-style))
      (:more
       (dolist (offset (list inset middle (- size inset)))
         (lightfast:draw-filled-circle (+ x offset) (+ y middle) 1)))))
  nil)

(defparameter *spinner-frames* 8
  "How many positions the working mark steps through.")

(-> paint-spinner (integer integer &key (:size integer) (:phase integer) (:color (or null list))) null)
(defun paint-spinner (x y &key (size 14) (phase 0) color)
  "Draw the working mark in a SIZE box at X, Y: eight dots on a ring, the one at PHASE filled and the rest outlined."
  (let* ((color  (or color (ink-color)))
         (center (floor size 2))
         (radius (- center 2)))
    (paint--color color)
    (dotimes (index *spinner-frames*)
      (let* ((angle (* 2 pi (/ index *spinner-frames*)))
             (dot-x (+ x center (round (* radius (sin angle)))))
             (dot-y (- (+ y center) (round (* radius (cos angle))))))
        (if (= index (mod phase *spinner-frames*))
            (lightfast:draw-filled-rect (- dot-x 1) (- dot-y 1) 3 3)
            (lightfast:draw-filled-rect dot-x dot-y 1 1)))))
  nil)
