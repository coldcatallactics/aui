(in-package #:aui)

;;;; -- Blocks --

;;; The pieces a document is stacked from: paragraphs, framed code, tables,
;;; rows that open and close, boxes around other blocks, images and rules.
;;; A block is laid out to a width, which fixes its height, and then painted
;;; at a position. Blocks that hold text expose it as leaves, paragraphs with
;;; their offsets, so the document can select across blocks.

(defvar *paint-stage* nil
  "The stage being painted, where blocks declare their pointer regions.")

(defvar *paint-top* most-negative-fixnum
  "The top of what is visible; blocks skip what lies wholly above it.")

(defvar *paint-bottom* most-positive-fixnum
  "The bottom of what is visible; blocks skip what lies wholly below it.")

(defvar *paint-selection* nil
  "A table from paragraph to its selected (START . END) characters during a paint, or NIL.")

(defclass flow-block ()
  ((margin-top
    :initarg       :margin-top
    :initform      0
    :accessor      block-margin-top
    :type          integer
    :documentation "Space kept above the block.")
   (margin-bottom
    :initarg       :margin-bottom
    :initform      0
    :accessor      block-margin-bottom
    :type          integer
    :documentation "Space kept below the block.")
   (height
    :initform      0
    :accessor      block-height
    :type          integer
    :documentation "The block's height at the width it was last laid out to, without its margins.")
   (width
    :initform      0
    :accessor      block-width
    :type          integer
    :documentation "The width the block was last laid out to."))
  (:documentation "One piece of a stacked document."))

(defgeneric block-layout (block width)
  (:documentation "Lay BLOCK out to WIDTH, record its height, and return that height."))

(defgeneric block-paint (block x y)
  (:documentation "Draw BLOCK with its top-left corner at X, Y on the stage being painted."))

(defgeneric block-leaves (block)
  (:documentation "Return BLOCK's selectable text as a list of (PARAGRAPH X Y), the offsets measured from the block's corner.")
  (:method ((block flow-block))
    "A block without text has no leaves."
    nil))

(defgeneric block-text (block)
  (:documentation "Return BLOCK's content as plain text, for copying.")
  (:method ((block flow-block))
    "The text of a block is the text of its leaves, one to a line."
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (leaf) (paragraph-text (first leaf))) (block-leaves block)))))

(defmethod block-layout :around ((block flow-block) width)
  "Remember the width and the height every layout arrives at."
  (setf (block-width block)  width
        (block-height block) (call-next-method)))

(-> block--selection (paragraph) (or null cons))
(defun block--selection (paragraph)
  "Return the selected range of PARAGRAPH in the paint under way, or NIL."
  (and *paint-selection* (gethash paragraph *paint-selection*)))

(-> block--paint-paragraph (paragraph integer integer) null)
(defun block--paint-paragraph (paragraph x y)
  "Draw PARAGRAPH at X, Y with its selection, and declare a region for every link in it."
  (paragraph-paint paragraph x y
                   :selection (block--selection paragraph)
                   :top       *paint-top*
                   :bottom    *paint-bottom*)
  (when *paint-stage*
    (dolist (line (paragraph-lines paragraph))
      (dolist (fragment (text-line-fragments line))
        (let ((link (and (fragment-span fragment) (span-link (fragment-span fragment)))))
          (when link
            (stage-region *paint-stage* (list ':link link (fragment-start fragment))
                          :x       (+ x (fragment-x fragment))
                          :y       (+ y (text-line-y line))
                          :width   (fragment-width fragment)
                          :height  (text-line-height line)
                          :action  (if (functionp link)
                                       link
                                       (lambda () (open-link link)))
                          :tooltip (and (stringp link) link)))))))
  nil)

(-> open-link (string) null)
(defun open-link (url)
  "Open URL in the person's browser."
  (handler-case
      (uiop:launch-program (list (if (uiop:os-macosx-p) "open" "xdg-open") url))
    (error ()
      nil))
  nil)


;;;; -- Text --

(defclass text-block (flow-block)
  ((paragraph
    :initarg       :paragraph
    :reader        text-block-paragraph
    :type          paragraph
    :documentation "The text.")
   (indent
    :initarg       :indent
    :initform      0
    :reader        text-block-indent
    :type          integer
    :documentation "How far the text is set in from the block's left edge.")
   (marker
    :initarg       :marker
    :initform      nil
    :reader        text-block-marker
    :documentation "What hangs in the indent before the first line: a string, a mark keyword, or NIL."))
  (:documentation "A paragraph, optionally indented behind a marker as a list item is."))

(-> make-text-block (list &key (:style text-style) (:indent integer) (:marker t) (:align keyword)
                               (:margin-top integer) (:margin-bottom integer))
    text-block)
(defun make-text-block (spans &key (style (text-style ':body)) (indent 0) marker (align ':left)
                                   (margin-top 0) (margin-bottom 0))
  "Return a text block of SPANS in STYLE."
  (make-instance 'text-block
                 :paragraph     (make-paragraph spans :style style :align align)
                 :indent        indent
                 :marker        marker
                 :margin-top    margin-top
                 :margin-bottom margin-bottom))

(defmethod block-layout ((block text-block) width)
  "A text block is as tall as its lines."
  (paragraph-layout (text-block-paragraph block) (max 1 (- width (text-block-indent block)))))

(defmethod block-paint ((block text-block) x y)
  "Draw the marker in the indent and the text beside it."
  (let* ((paragraph (text-block-paragraph block))
         (style     (paragraph-style paragraph))
         (indent    (text-block-indent block))
         (marker    (text-block-marker block)))
    (cond
      ((stringp marker)
       (paint-text marker (+ x (max 0 (- indent (text-width marker style) 8))) (+ y (text-baseline style))
                   :style style))
      (marker
       (let ((size 14))
         (paint-frame (+ x (- indent size 9)) (+ y (floor (- (text-style-line-height style) size) 2))
                      :width  size
                      :height size)
         (when (eq marker ':done)
           (paint-mark ':check (+ x (- indent size 9)) (+ y (floor (- (text-style-line-height style) size) 2))
                       :size size)))))
    (block--paint-paragraph paragraph (+ x indent) y)))

(defmethod block-leaves ((block text-block))
  "The paragraph, set in by the indent."
  (list (list (text-block-paragraph block) (text-block-indent block) 0)))


;;;; -- Code --

(defparameter *code-padding* 12
  "The space between a code frame and its text.")

(defclass code-block (flow-block)
  ((paragraph
    :initarg       :paragraph
    :reader        code-block-paragraph
    :type          paragraph
    :documentation "The code, wrapping wherever the width runs out.")
   (label
    :initarg       :label
    :initform      nil
    :reader        code-block-label
    :type          (or null string)
    :documentation "The words above the code, such as its language, or NIL.")
   (identifier
    :initarg       :identifier
    :initform      (gensym "CODE")
    :reader        code-block-identifier
    :documentation "Names the block's copy region."))
  (:documentation "Fixed-pitch text in a hairline frame with a copy action."))

(-> make-code-block (string &key (:label (or null string)) (:spans (or null list)) (:margin-top integer)
                                 (:margin-bottom integer))
    code-block)
(defun make-code-block (text &key label spans (margin-top 0) (margin-bottom 0))
  "Return a code block of TEXT, or of the already styled SPANS, under LABEL."
  (make-instance 'code-block
                 :paragraph     (make-paragraph (or spans (list (make-span text nil)))
                                                :style   (text-style ':code)
                                                :wrap    ':anywhere
                                                :hanging 18)
                 :label         label
                 :margin-top    margin-top
                 :margin-bottom margin-bottom))

(-> code-block--header-height (code-block) integer)
(defun code-block--header-height (block)
  "Return the height of BLOCK's header row."
  (declare (ignore block))
  (text-style-line-height (text-style ':meta)))

(defmethod block-layout ((block code-block) width)
  "The frame holds a header row and the wrapped code."
  (+ (* 2 *code-padding*)
     (code-block--header-height block)
     4
     (paragraph-layout (code-block-paragraph block) (max 1 (- width (* 2 *code-padding*))))))

(defmethod block-paint ((block code-block) x y)
  "Draw the frame, the label and copy action, and the code."
  (let* ((width   (block-width block))
         (meta    (text-style ':meta))
         (header  (code-block--header-height block))
         (copy-w  (+ (text-width "copy" meta) 12))
         (copy-x  (- (+ x width) copy-w *code-padding* -6))
         (copy-id (list ':copy (code-block-identifier block)))
         (hot-p   (and *paint-stage* (stage-hot-p *paint-stage* copy-id))))
    (paint-frame x y :width width :height (block-height block))
    (when (code-block-label block)
      (paint-text-in (code-block-label block) (text-style ':meta-bold)
                     :x      (+ x *code-padding*)
                     :y      (+ y 8)
                     :width  (- width copy-w (* 2 *code-padding*))
                     :height header))
    (if hot-p
        (with-inversion (copy-x (+ y 8) copy-w header)
          (paint-text-in "copy" meta :x copy-x :y (+ y 8) :width copy-w :height header :align ':center))
        (paint-text-in "copy" meta :x copy-x :y (+ y 8) :width copy-w :height header :align ':center))
    (when *paint-stage*
      (stage-region *paint-stage* copy-id
                    :x       copy-x
                    :y       (+ y 8)
                    :width   copy-w
                    :height  header
                    :action  (lambda ()
                               (lightfast:copy-text (paragraph-text (code-block-paragraph block))))
                    :tooltip "Copy this block"))
    (block--paint-paragraph (code-block-paragraph block)
                            (+ x *code-padding*) (+ y *code-padding* header 4))))

(defmethod block-leaves ((block code-block))
  "The code, inside the frame's padding and below its header."
  (list (list (code-block-paragraph block)
              *code-padding*
              (+ *code-padding* (code-block--header-height block) 4))))


;;;; -- Table --

(defclass table-block (flow-block)
  ((rows
    :initarg       :rows
    :reader        table-block-rows
    :type          list
    :documentation "The rows, each a list of cell paragraphs; the first row is the header.")
   (columns
    :initform      nil
    :accessor      table-block--columns
    :type          list
    :documentation "The laid-out column widths.")
   (row-heights
    :initform      nil
    :accessor      table-block--row-heights
    :type          list
    :documentation "The laid-out row heights."))
  (:documentation "A grid of cells with hairlines between them."))

(defparameter *cell-padding-x* 10
  "Horizontal space between a table cell's edge and its text.")

(defparameter *cell-padding-y* 5
  "Vertical space between a table cell's edge and its text.")

(defmethod block-layout ((block table-block) width)
  "Give each column its natural width, shrinking all in proportion when the table is too wide."
  (let* ((rows    (table-block-rows block))
         (count   (reduce #'max rows :key #'length :initial-value 0))
         (natural (loop for column from 0 below count
                        collect (loop for row in rows
                                      for cell = (nth column row)
                                      maximize (if cell
                                                   (progn (paragraph-layout cell 4000)
                                                          (paragraph-extent cell))
                                                   0))))
         (padding (* 2 *cell-padding-x*))
         (total   (+ 1 (reduce #'+ natural) (* count (1+ padding))))
         (scale   (if (and (> total width) (plusp (reduce #'+ natural)))
                      (/ (max 1 (- width 1 (* count (1+ padding)))) (reduce #'+ natural))
                      1))
         (columns (mapcar (lambda (natural-width) (max 24 (floor (* natural-width scale)))) natural))
         (heights (loop for row in rows
                        collect (+ (* 2 *cell-padding-y*)
                                   (loop for cell in row
                                         for column-width in columns
                                         maximize (paragraph-layout cell column-width))))))
    (setf (table-block--columns block)     columns
          (table-block--row-heights block) heights)
    (+ 1 (reduce #'+ heights) (length heights))))

(defmethod block-paint ((block table-block) x y)
  "Draw every cell's text and the grid."
  (let* ((columns (table-block--columns block))
         (padding (* 2 *cell-padding-x*))
         (width   (+ 1 (reduce #'+ columns) (* (length columns) (1+ padding))))
         (row-y   y))
    (paint-line x y :x2 (+ x width -1) :y2 y)
    (loop for row in (table-block-rows block)
          for height in (table-block--row-heights block)
          do (let ((cell-x x))
               (loop for cell in row
                     for column-width in columns
                     do (block--paint-paragraph cell (+ cell-x 1 *cell-padding-x*) (+ row-y 1 *cell-padding-y*))
                        (incf cell-x (+ column-width padding 1))))
             (incf row-y (1+ height))
             (paint-line x row-y :x2 (+ x width -1) :y2 row-y))
    (let ((line-x x))
      (paint-line line-x y :x2 line-x :y2 row-y)
      (dolist (column-width columns)
        (incf line-x (+ column-width padding 1))
        (paint-line line-x y :x2 line-x :y2 row-y)))))

(defmethod block-leaves ((block table-block))
  "Every cell, at its place in the grid."
  (let ((leaves  nil)
        (padding (* 2 *cell-padding-x*))
        (row-y   0))
    (loop for row in (table-block-rows block)
          for height in (table-block--row-heights block)
          do (let ((cell-x 0))
               (loop for cell in row
                     for column-width in (table-block--columns block)
                     do (push (list cell (+ cell-x 1 *cell-padding-x*) (+ row-y 1 *cell-padding-y*)) leaves)
                        (incf cell-x (+ column-width padding 1))))
             (incf row-y (1+ height)))
    (nreverse leaves)))


;;;; -- Rule --

(defclass rule-block (flow-block)
  ((label
    :initarg       :label
    :initform      nil
    :reader        rule-block-label
    :type          (or null string)
    :documentation "Words centered on the rule, or NIL for a bare one."))
  (:documentation "A band of dither across the column, with words in its middle when it marks something."))

(defmethod block-layout ((block rule-block) width)
  "A rule is one small line high."
  (declare (ignore width))
  (text-style-line-height (text-style ':meta)))

(defmethod block-paint ((block rule-block) x y)
  "Draw the dither band and the label on a strip of paper."
  (let* ((style  (text-style ':meta))
         (height (block-height block))
         (width  (block-width block))
         (band-y (+ y (floor (- height 5) 2)))
         (label  (rule-block-label block)))
    (paint-dither x band-y :width width :height 5 :step 3)
    (when label
      (let* ((label-width (+ (text-width label style) 24))
             (label-x     (+ x (floor (- width label-width) 2))))
        (paint-fill label-x y :width label-width :height height :color (ground-color))
        (paint-text-in label style :x label-x :y y :width label-width :height height :align ':center)))))


;;;; -- Row --

(defclass row-block (flow-block)
  ((identifier
    :initarg       :identifier
    :reader        row-block-identifier
    :documentation "Names the row's region.")
   (mark
    :initarg       :mark
    :initform      nil
    :accessor      row-block-mark
    :documentation "The mark before the title: a mark keyword, :SPINNER, or NIL.")
   (mark-color
    :initarg       :mark-color
    :initform      nil
    :accessor      row-block-mark-color
    :type          (or null list)
    :documentation "The mark's color, the ground's ink when NIL.")
   (title
    :initarg       :title
    :reader        row-block-title
    :type          list
    :documentation "The spans of the row's one line.")
   (trailing
    :initarg       :trailing
    :initform      nil
    :reader        row-block-trailing
    :type          (or null string)
    :documentation "Small text at the row's right edge, such as a duration.")
   (action
    :initarg       :action
    :initform      nil
    :reader        row-block-action
    :type          (or null function)
    :documentation "Called when the row is clicked.")
   (style
    :initarg       :style
    :reader        row-block-style
    :type          text-style
    :documentation "The style of the title where a span names none."))
  (:documentation "One line that summarizes something and responds to a click, as a tool call does."))

(defparameter *row-mark-size* 14
  "The side of the mark at the start of a row.")

(defmethod block-layout ((block row-block) width)
  "A row is one line of its style high."
  (declare (ignore width))
  (+ 4 (text-style-line-height (row-block-style block))))

(defmethod block-paint ((block row-block) x y)
  "Draw the mark, the title cut to fit, and the trailing text; invert the row under the pointer."
  (let* ((stage    *paint-stage*)
         (style    (row-block-style block))
         (meta     (text-style ':meta))
         (width    (block-width block))
         (height   (block-height block))
         (action   (row-block-action block))
         (hot-p    (and stage action (stage-hot-p stage (row-block-identifier block))))
         (trailing (row-block-trailing block))
         (trail-w  (if trailing (+ (text-width trailing meta) 10) 0))
         (text-x   (+ x 6 (if (row-block-mark block) (+ *row-mark-size* 8) 0))))
    (flet ((content ()
             (let ((mark   (row-block-mark block))
                   (mark-y (+ y (floor (- height *row-mark-size*) 2))))
               (cond
                 ((eq mark ':spinner)
                  (setf (stage-animating-p stage) t)
                  (paint-spinner (+ x 6) mark-y :size *row-mark-size* :phase (stage-phase stage)))
                 (mark
                  (paint-mark mark (+ x 6) mark-y
                              :size  *row-mark-size*
                              :color (and (not hot-p) (row-block-mark-color block))))))
             (let ((cursor   text-x)
                   (limit    (- (+ x width) trail-w 6))
                   (baseline (+ y 2 (text-baseline style))))
               (dolist (span (row-block-title block))
                 (let* ((span-style (or (span-style span) style))
                        (fitted     (text-fit (span-text span) span-style (max 0 (- limit cursor)))))
                   (when (and (plusp (- limit cursor)) (plusp (length fitted)))
                     (setf cursor (paint-text fitted cursor baseline :style span-style))))))
             (when trailing
               (paint-text-in trailing meta
                              :x      (- (+ x width) trail-w 6)
                              :y      y
                              :width  trail-w
                              :height height
                              :align  ':right))))
      (if hot-p
          (with-inversion (x y width height)
            (content))
          (content)))
    (when (and stage action)
      (stage-region stage (row-block-identifier block) :x x :y y :width width :height height :action action))))

(defmethod block-text ((block row-block))
  "The title's words."
  (apply #'concatenate 'string (mapcar #'span-text (row-block-title block))))


;;;; -- Box --

(defclass box-block (flow-block)
  ((children
    :initarg       :children
    :accessor      box-block-children
    :type          list
    :documentation "The blocks inside the box, top first.")
   (padding-x
    :initarg       :padding-x
    :initform      14
    :reader        box-block-padding-x
    :type          integer
    :documentation "Space between the box's sides and its children.")
   (padding-y
    :initarg       :padding-y
    :initform      10
    :reader        box-block-padding-y
    :type          integer
    :documentation "Space between the box's top and bottom and its children.")
   (frame-p
    :initarg       :frame-p
    :initform      t
    :reader        box-block-frame-p
    :type          boolean
    :documentation "Whether a hairline is drawn around the box.")
   (fill
    :initarg       :fill
    :initform      ':none
    :reader        box-block-fill
    :type          (member :none :ink :quiet)
    :documentation "What fills the box: nothing, ink with its content inverted, or light dither.")
   (shadow-p
    :initarg       :shadow-p
    :initform      nil
    :reader        box-block-shadow-p
    :type          boolean
    :documentation "Whether a dither band sits below and right of the box, as under a window.")
   (align
    :initarg       :align
    :initform      ':stretch
    :reader        box-block-align
    :type          (member :stretch :left :right)
    :documentation "Whether the box takes the whole width or hugs its content at one side.")
   (fraction
    :initarg       :fraction
    :initform      1
    :reader        box-block-fraction
    :type          real
    :documentation "The most of the available width a hugging box may take.")
   (offset
    :initform      0
    :accessor      box-block--offset
    :type          integer
    :documentation "Where the laid-out box starts within the available width.")
   (inner-width
    :initform      0
    :accessor      box-block--inner-width
    :type          integer
    :documentation "The width the children were laid out to."))
  (:documentation "A frame, a fill or just padding around other blocks."))

(defparameter *shadow-offset* 8
  "How far a box's dither shadow sits below and right of it.")

(-> box-block--stack-height (list) integer)
(defun box-block--stack-height (children)
  "Return the height of CHILDREN stacked with their margins, adjoining margins collapsed to the larger."
  (let ((height   0)
        (previous 0))
    (loop for child in children
          for first-p = t then nil
          do (incf height (if first-p 0 (max previous (block-margin-top child))))
             (incf height (block-height child))
             (setf previous (block-margin-bottom child)))
    height))

(defmethod block-layout ((block box-block) width)
  "Lay the children out inside the padding; a hugging box shrinks to its widest line of text."
  (let* ((children  (box-block-children block))
         (padding   (* 2 (box-block-padding-x block)))
         (shadow    (if (box-block-shadow-p block) *shadow-offset* 0))
         (available (max 1 (- (floor (* width (box-block-fraction block))) padding shadow)))
         (inner     available))
    (dolist (child children)
      (block-layout child available))
    (unless (eq (box-block-align block) ':stretch)
      (let ((extent (loop for child in children
                          maximize (if (typep child 'text-block)
                                       (+ (text-block-indent child)
                                          (paragraph-extent (text-block-paragraph child)))
                                       available))))
        (when (< extent available)
          (setf inner (max 24 extent))
          (dolist (child children)
            (block-layout child inner)))))
    (setf (box-block--inner-width block) inner
          (box-block--offset block)      (if (eq (box-block-align block) ':right)
                                             (- width inner padding shadow)
                                             0))
    (+ (* 2 (box-block-padding-y block)) shadow (box-block--stack-height children))))

(-> box-block--child-positions (box-block) list)
(defun box-block--child-positions (block)
  "Return (CHILD X Y) for each of BLOCK's children, measured from the block's corner."
  (let ((positions nil)
        (child-x   (+ (box-block--offset block) (box-block-padding-x block)))
        (child-y   (box-block-padding-y block))
        (previous  0))
    (loop for child in (box-block-children block)
          for first-p = t then nil
          do (incf child-y (if first-p 0 (max previous (block-margin-top child))))
             (push (list child child-x child-y) positions)
             (incf child-y (block-height child))
             (setf previous (block-margin-bottom child)))
    (nreverse positions)))

(defmethod block-paint ((block box-block) x y)
  "Draw the shadow, the fill and frame, then the children on the right ground."
  (let* ((shadow (if (box-block-shadow-p block) *shadow-offset* 0))
         (box-x  (+ x (box-block--offset block)))
         (box-w  (+ (box-block--inner-width block) (* 2 (box-block-padding-x block))))
         (box-h  (- (block-height block) shadow)))
    (when (plusp shadow)
      (paint-dither (+ box-x shadow) (+ y box-h) :width box-w :height shadow)
      (paint-dither (+ box-x box-w) (+ y shadow) :width shadow :height (- box-h shadow)))
    (flet ((children ()
             (loop for (child child-x child-y) in (box-block--child-positions block)
                   do (when (and (< (+ y child-y) *paint-bottom*)
                                 (> (+ y child-y (block-height child)) *paint-top*))
                        (block-paint child (+ x child-x) (+ y child-y))))))
      (ecase (box-block-fill block)
        (:ink
         (with-inversion (box-x y box-w box-h)
           (children)))
        (:quiet
         (paint-dither box-x y :width box-w :height box-h :step 5)
         (children))
        (:none
         (children))))
    (when (and (box-block-frame-p block) (not (eq (box-block-fill block) ':ink)))
      (paint-frame box-x y :width box-w :height box-h))))

(defmethod block-leaves ((block box-block))
  "The leaves of every child, moved to its place in the box."
  (loop for (child child-x child-y) in (box-block--child-positions block)
        append (loop for (paragraph leaf-x leaf-y) in (block-leaves child)
                     collect (list paragraph (+ child-x leaf-x) (+ child-y leaf-y)))))

(defmethod block-text ((block box-block))
  "The text of the children, one after another."
  (format nil "~{~A~^~%~}" (mapcar #'block-text (box-block-children block))))


;;;; -- Image --

(defclass image-block (flow-block)
  ((pathname
    :initarg       :pathname
    :reader        image-block-pathname
    :type          string
    :documentation "The image file.")
   (image
    :initform      nil
    :accessor      image-block--image
    :documentation "The loaded image handle, :MISSING when the file could not be read, or NIL before loading.")
   (extent
    :initform      nil
    :accessor      image-block--extent
    :type          (or null cons)
    :documentation "The (WIDTH . HEIGHT) the image is drawn at."))
  (:documentation "A picture, scaled down to fit the column."))

(defparameter *image-maximum-height* 320
  "The tallest an inline picture is drawn.")

(defmethod block-layout ((block image-block) width)
  "Scale the picture to fit the width and the height limit, keeping its shape."
  (unless (image-block--image block)
    (setf (image-block--image block)
          (or (lightfast:load-image (image-block-pathname block)) ':missing)))
  (let ((image (image-block--image block)))
    (if (eq image ':missing)
        (progn
          (setf (image-block--extent block) nil)
          (text-style-line-height (text-style ':meta)))
        (multiple-value-bind (image-width image-height) (lightfast:image-size image)
          (let* ((scale        (min 1 (/ width (max 1 image-width)) (/ *image-maximum-height* (max 1 image-height))))
                 (drawn-width  (max 1 (floor (* image-width scale))))
                 (drawn-height (max 1 (floor (* image-height scale)))))
            (setf (image-block--extent block) (cons drawn-width drawn-height))
            (+ drawn-height 2))))))

(defmethod block-paint ((block image-block) x y)
  "Draw the picture in a hairline frame, or the file's name when it cannot be shown."
  (let ((extent (image-block--extent block)))
    (if extent
        (progn
          (lightfast:draw-image (image-block--image block) (1+ x) (1+ y)
                                :width (first extent) :height (rest extent))
          (paint-frame x y :width (+ (first extent) 2) :height (+ (rest extent) 2)))
        (paint-text (format nil "[~A]" (file-namestring (image-block-pathname block))) x (+ y (text-baseline (text-style ':meta)))
                    :style (text-style ':meta)))))
