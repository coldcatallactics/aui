(in-package #:aui)

;;;; -- Painting --

;;; Theme-aware drawing helpers, valid during a stage draw callback.

(defvar *on-ink-p* nil
  "True if the current ground is inverted and filled with ink.")

(-> ink-color () list)
(defun ink-color ()
  "Return the color of text and lines on the current ground."
  (if *on-ink-p* *paper* *ink*))

(-> ground-color () list)
(defun ground-color ()
  "Return the color of the current ground."
  (if *on-ink-p* *ink* *paper*))

(-> accent-color () list)
(defun accent-color ()
  "Return the brand green for the current ground."
  (if *on-ink-p* *green-light* *green*))

(-> ground-tint (list) list)
(defun ground-tint (color)
  "Return COLOR for the current ground. On an inverted ground, accent inks are replaced by their lighter versions."
  (or (and *on-ink-p*
           (rest (assoc color *accents-on-ink* :test #'equal)))
      color))

(-> paint--color (list) null)
(defun paint--color (color)
  "Set the drawing color to COLOR, which is a (RED GREEN BLUE) list."
  (lightfast:draw-color-rgb :red (first color) :green (second color) :blue (third color))
  nil)

(-> paint-fill (integer integer &key (:width integer) (:height integer) (:color (or null list))) null)
(defun paint-fill (x y &key (width 0) (height 0) color)
  "Fill the box with COLOR. The default is the ink color of the current ground."
  (when (and (plusp width) (plusp height))
    (paint--color (or color (ink-color)))
    (lightfast:draw-filled-rect x y width height))
  nil)

(-> paint-frame
    (integer integer &key (:width integer) (:height integer) (:color (or null list)) (:dashed-p boolean)) null)
(defun paint-frame (x y &key (width 0) (height 0) color dashed-p)
  "Draw a hairline around the box. The line is dashed if DASHED-P is true."
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
  "Draw a line from X1, Y1 to X2, Y2. The line is WIDTH units thick, and the default is a hairline."
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
  "Fill the box with one-bit dither in COLOR. A STEP of 3 is a medium tone, and a STEP of 5 is a light tone."
  (when (and (plusp width) (plusp height))
    (let ((color (or color (ink-color))))
      (lightfast:draw-dither x y width height
                             :step step :red (first color) :green (second color) :blue (third color))))
  nil)

(defmacro with-inversion ((x y width height) &body body)
  "Fill the box with the ink of the current ground. Run BODY with the ground inverted, so that text and marks drawn by BODY are inverted too."
  (let ((box-x (gensym "X")) (box-y (gensym "Y")) (box-w (gensym "W")) (box-h (gensym "H")))
    `(let ((,box-x ,x) (,box-y ,y) (,box-w ,width) (,box-h ,height))
       (paint-fill ,box-x ,box-y :width ,box-w :height ,box-h)
       (let ((*on-ink-p* (not *on-ink-p*)))
         ,@body))))

(-> paint-text (string integer integer &key (:style text-style) (:color (or null list))) integer)
(defun paint-text (text x baseline &key (style (text-style ':body)) color)
  "Draw TEXT in STYLE from X on the baseline BASELINE. Returns the X where the text ends."
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
  "Draw one line of TEXT in STYLE inside the box. The text is centered vertically and aligned by ALIGN, which is :LEFT, :CENTER or :RIGHT.

If TEXT is wider than the box, it is cut at the edge of the box and ends in an ellipsis."
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

(-> text--fit (string text-style integer &key (:cut function)) string)
(defun text--fit (text style width &key cut)
  "Return TEXT if it fits WIDTH in STYLE. Otherwise, return the longest cut of TEXT that fits.

CUT is a function of a character count. It returns TEXT cut to that many
characters, with an ellipsis. Longer cuts are not narrower, so the longest
cut that fits is found by binary search."
  (if (<= (text-width text style) width)
      text
      (let ((low  0)
            (high (1- (length text))))
        (loop while (< low high)
              do (let ((middle (ceiling (+ low high) 2)))
                   (if (<= (text-measure (funcall cut middle) style) width)
                       (setf low middle)
                       (setf high (1- middle)))))
        (funcall cut low))))

(-> text-fit (string text-style integer) string)
(defun text-fit (text style width)
  "Return TEXT if it fits WIDTH in STYLE. Otherwise, return the longest beginning of TEXT that fits, followed by an ellipsis."
  (text--fit text style width
             :cut (lambda (count)
                    (concatenate 'string (string-right-trim " " (subseq text 0 count)) "…"))))

(-> text-fit-tail (string text-style integer) string)
(defun text-fit-tail (text style width)
  "Return TEXT if it fits WIDTH in STYLE. Otherwise, return an ellipsis followed by the longest ending of TEXT that fits."
  (text--fit text style width
             :cut (lambda (count)
                    (concatenate 'string "…" (subseq text (- (length text) count))))))


;;;; -- Marks --

(-> paint-mark (keyword integer integer &key (:size integer) (:color (or null list))) null)
(defun paint-mark (mark x y &key (size 14) color)
  "Draw the mark MARK in a SIZE by SIZE box. The top-left corner of the box is at X, Y.

Marks are drawn from lines and polygons, so they look the same with all system fonts."
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
      ;; Three sliders across the box. The knobs are at different places. The
      ;; mark stands for settings.
      (:sliders
       (loop for row in (list (+ y (floor size 5)) (+ y middle) (- (+ y size) (floor size 5)))
             for part in '(11/16 1/4 9/16)
             do (let ((knob (+ x (round (* part size)))))
                  (lightfast:draw-filled-rect x (1- row) size 2)
                  (lightfast:draw-filled-rect (- knob 2) (- row 3) 4 6))))
      ;; A sheet that has a folded corner. The mark stands for a file.
      (:page
       (let* ((left   (1+ left))
              (right  (1- right))
              (top    (- top 2))
              (bottom (+ bottom 2))
              (fold   (max 3 (floor (- right left) 2))))
         (lightfast:draw-line left top (- right fold) top)
         (lightfast:draw-line (- right fold) top right (+ top fold))
         (lightfast:draw-line right (+ top fold) right bottom)
         (lightfast:draw-line right bottom left bottom)
         (lightfast:draw-line left bottom left top)
         (lightfast:draw-line (- right fold) top (- right fold) (+ top fold))
         (lightfast:draw-line (- right fold) (+ top fold) right (+ top fold))))
      (:slash
       (lightfast:draw-line-style :width 2)
       (lightfast:draw-line (- right 1) top (+ left 1) bottom)
       (lightfast:draw-line-style))
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
  "The number of positions of the working mark.")

(-> paint-spinner (integer integer &key (:size integer) (:phase integer) (:color (or null list))) null)
(defun paint-spinner (x y &key (size 14) (phase 0) color)
  "Draw the working mark in a SIZE box at X, Y.

The mark is the braille cell of the terminal, which has two columns of four
dots. One dot is missing. As PHASE advances, the gap moves down the left
column and then up the right column."
  (let* ((color (if color (ground-tint color) (ink-color)))
         (pitch (max 3 (round size 7/2)))
         (dot   (max 2 (floor pitch 2)))
         (left  (+ x (floor (- size (+ pitch dot)) 2)))
         (top   (+ y (floor (- size (+ (* 3 pitch) dot)) 2)))
         (step  (mod phase *spinner-frames*))
         (gap   (if (< step 4)
                    (cons 0 step)
                    (cons 1 (- 7 step)))))
    (paint--color color)
    (dotimes (column 2)
      (dotimes (row 4)
        (unless (and (= column (first gap)) (= row (rest gap)))
          (lightfast:draw-filled-rect (+ left (* column pitch)) (+ top (* row pitch)) dot dot)))))
  nil)
