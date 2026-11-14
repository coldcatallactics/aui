(in-package #:aui)

;;;; -- Paragraphs --

;;; Text set in lines: runs of styled text broken at word boundaries to a
;;; width, with the positions kept so that a point can be mapped back to a
;;; character and a range of characters drawn selected. Everything a person
;;; reads in the transcript is a paragraph inside some block.

(defstruct (span (:constructor make-span (text style &key link)))
  "One run of a paragraph, TEXT set in STYLE, opening LINK when clicked."
  (text  ""  :type string)
  (style nil :type (or null text-style))
  (link  nil))

(defstruct fragment
  "A stretch of one span on one line, with where it starts, how wide it is, and where its text sits in the paragraph."
  (x     0   :type integer)
  (width 0   :type integer)
  (text  ""  :type string)
  (span  nil :type (or null span))
  (start 0   :type integer))

(defstruct text-line
  "One laid-out line, with its top within the paragraph, its height and baseline, its fragments and its character range."
  (y         0   :type integer)
  (height    0   :type integer)
  (baseline  0   :type integer)
  (width     0   :type integer)
  (fragments nil :type list)
  (start     0   :type integer)
  (end       0   :type integer))

(defclass paragraph ()
  ((spans
    :initarg       :spans
    :accessor      paragraph-spans
    :type          list
    :documentation "The runs of the paragraph in reading order.")
   (style
    :initarg       :style
    :reader        paragraph-style
    :type          text-style
    :documentation "The base style, which sets the height of an empty line and of spans that name none.")
   (align
    :initarg       :align
    :initform      ':left
    :reader        paragraph-align
    :type          (member :left :center :right)
    :documentation "How lines sit within the paragraph's width.")
   (wrap
    :initarg       :wrap
    :initform      ':words
    :reader        paragraph-wrap
    :type          (member :words :anywhere)
    :documentation "Whether lines break between words, as prose does, or wherever the width runs out, as code does.")
   (hanging
    :initarg       :hanging
    :initform      0
    :reader        paragraph-hanging
    :type          integer
    :documentation "How far lines after the first of each hard line are indented.")
   (width
    :initform      -1
    :accessor      paragraph-width
    :type          integer
    :documentation "The width the lines were last broken to, or -1 before any layout.")
   (lines
    :initform      nil
    :accessor      paragraph-lines
    :type          list
    :documentation "The laid-out lines, top first.")
   (height
    :initform      0
    :accessor      paragraph-height
    :type          integer
    :documentation "The height of the laid-out lines together.")
   (extent
    :initform      0
    :accessor      paragraph-extent
    :type          integer
    :documentation "The width of the widest laid-out line."))
  (:documentation "Styled text broken into lines."))

(-> make-paragraph (list &key (:style text-style) (:align keyword) (:wrap keyword) (:hanging integer)) paragraph)
(defun make-paragraph (spans &key (style (text-style ':body)) (align ':left) (wrap ':words) (hanging 0))
  "Return a paragraph of SPANS set in STYLE where a span names no style of its own."
  (make-instance 'paragraph :spans spans :style style :align align :wrap wrap :hanging hanging))

(-> paragraph-text (paragraph) string)
(defun paragraph-text (paragraph)
  "Return PARAGRAPH's characters without their styles."
  (apply #'concatenate 'string (mapcar #'span-text (paragraph-spans paragraph))))

(-> paragraph--span-style (paragraph span) text-style)
(defun paragraph--span-style (paragraph span)
  "Return the style SPAN is set in within PARAGRAPH."
  (or (span-style span) (paragraph-style paragraph)))


;;;; -- Layout --

(-> paragraph--tokens (paragraph) list)
(defun paragraph--tokens (paragraph)
  "Return PARAGRAPH's text as tokens (KIND TEXT SPAN START), KIND being :WORD, :SPACE or :BREAK.

When the paragraph wraps anywhere, every character is its own word."
  (let ((tokens   nil)
        (offset   0)
        (anywhere (eq (paragraph-wrap paragraph) ':anywhere)))
    (dolist (span (paragraph-spans paragraph))
      (let* ((text   (span-text span))
             (length (length text))
             (index  0))
        (loop while (< index length)
              do (let ((character (char text index)))
                   (cond
                     ((char= character #\Newline)
                      (push (list ':break "" span (+ offset index)) tokens)
                      (incf index))
                     ((char= character #\Space)
                      (let ((end (or (position #\Space text :start index :test #'char/=) length)))
                        (push (list ':space (subseq text index end) span (+ offset index)) tokens)
                        (setf index end)))
                     (anywhere
                      (push (list ':word (string character) span (+ offset index)) tokens)
                      (incf index))
                     (t
                      (let ((end (or (position-if (lambda (other)
                                                    (or (char= other #\Space) (char= other #\Newline)))
                                                  text :start index)
                                     length)))
                        (push (list ':word (subseq text index end) span (+ offset index)) tokens)
                        (setf index end))))))
        (incf offset length)))
    (nreverse tokens)))

(-> paragraph--split-word (string text-style integer) list)
(defun paragraph--split-word (word style width)
  "Return WORD cut into pieces that each fit WIDTH in STYLE, for a word longer than a whole line."
  (let ((pieces nil)
        (start  0))
    (loop while (< start (length word))
          do (let ((end (1+ start)))
               (loop while (and (< end (length word))
                                (<= (text-width (subseq word start (1+ end)) style) width))
                     do (incf end))
               (push (subseq word start end) pieces)
               (setf start end)))
    (nreverse pieces)))

(-> paragraph-layout (paragraph integer) integer)
(defun paragraph-layout (paragraph width)
  "Break PARAGRAPH into lines no wider than WIDTH and return its height. Laying out to the same width again costs nothing."
  (unless (= width (paragraph-width paragraph))
    (let* ((base        (paragraph-style paragraph))
           (hanging     (paragraph-hanging paragraph))
           (lines       nil)
           (fragments   nil)
           (x           0)
           (line-start  0)
           (position    0)
           (continued-p nil))
      (labels ((available ()
                 (max 1 (- width (if continued-p hanging 0))))

               (add (text span start)
                 "Extend the last fragment when it continues the same span, else begin a new one."
                 (let* ((style    (paragraph--span-style paragraph span))
                        (previous (first fragments)))
                   (if (and previous
                            (eq (fragment-span previous) span)
                            (= (+ (fragment-start previous) (length (fragment-text previous))) start))
                       (let ((joined (concatenate 'string (fragment-text previous) text)))
                         (setf (fragment-text previous)  joined
                               (fragment-width previous) (text-width joined style)
                               x                         (+ (fragment-x previous) (fragment-width previous))))
                       (let ((measured (text-width text style)))
                         (push (make-fragment :x x :width measured :text text :span span :start start)
                               fragments)
                         (incf x measured)))
                   (setf position (+ start (length text)))))

               (finish (hard-p)
                 "Close the line under way and begin the next."
                 (let* ((ordered  (nreverse fragments))
                        (height   (reduce #'max ordered
                                          :key           (lambda (fragment)
                                                           (text-style-line-height
                                                            (paragraph--span-style paragraph (fragment-span fragment))))
                                          :initial-value (text-style-line-height base)))
                        (baseline (reduce #'max ordered
                                          :key           (lambda (fragment)
                                                           (text-baseline
                                                            (paragraph--span-style paragraph (fragment-span fragment))))
                                          :initial-value (text-baseline base)))
                        (extent   (if ordered
                                      (let ((last (first (last ordered))))
                                        (+ (fragment-x last)
                                           (text-width (string-right-trim " " (fragment-text last))
                                                       (paragraph--span-style paragraph (fragment-span last)))))
                                      0))
                        (offset   (+ (if continued-p hanging 0)
                                     (ecase (paragraph-align paragraph)
                                       (:left
                                        0)
                                       (:center
                                        (max 0 (floor (- (available) extent) 2)))
                                       (:right
                                        (max 0 (- (available) extent)))))))
                   (dolist (fragment ordered)
                     (incf (fragment-x fragment) offset))
                   (push (make-text-line :height height :baseline baseline :width (+ offset extent)
                                         :fragments ordered :start line-start :end position)
                         lines)
                   (setf fragments   nil
                         x           0
                         line-start  position
                         continued-p (not hard-p)))))
        (dolist (token (paragraph--tokens paragraph))
          (destructuring-bind (kind text span start) token
            (let ((style (paragraph--span-style paragraph span)))
              (ecase kind
                (:break
                 (setf position (1+ start))
                 (finish t)
                 (setf line-start position))
                (:space
                 ;; A space that would start a wrapped line is dropped.
                 (if (and (zerop x) continued-p)
                     (setf position   (+ start (length text))
                           line-start position)
                     (add text span start)))
                (:word
                 (let ((measured (text-width text style)))
                   (cond
                     ((<= (+ x measured) (available))
                      (add text span start))
                     ((<= measured (available))
                      (finish nil)
                      (add text span start))
                     (t
                      (let ((offset start))
                        (dolist (piece (paragraph--split-word text style (available)))
                          (when (and (plusp x) (> (+ x (text-width piece style)) (available)))
                            (finish nil))
                          (add piece span offset)
                          (incf offset (length piece))))))))))))
        (when (or fragments (null lines))
          (finish t)))
      (let ((y      0)
            (extent 0))
        (dolist (line (setf lines (nreverse lines)))
          (setf (text-line-y line) y)
          (incf y (text-line-height line))
          (setf extent (max extent (text-line-width line))))
        (setf (paragraph-lines paragraph)  lines
              (paragraph-height paragraph) y
              (paragraph-extent paragraph) extent
              (paragraph-width paragraph)  width))))
  (paragraph-height paragraph))


;;;; -- Positions --

(-> paragraph--prefix-width (fragment text-style integer) integer)
(defun paragraph--prefix-width (fragment style count)
  "Return the width of the first COUNT characters of FRAGMENT in STYLE."
  (cond
    ((<= count 0)
     0)
    ((>= count (length (fragment-text fragment)))
     (fragment-width fragment))
    (t
     (values (lightfast:measure-text (subseq (fragment-text fragment) 0 count)
                                     :font (theme-font (text-style-face style))
                                     :size (text-style-size style))))))

(-> paragraph--line-at (paragraph integer) (or null text-line))
(defun paragraph--line-at (paragraph y)
  "Return the line of PARAGRAPH containing the height Y, the first for a Y above and the last for one below."
  (let ((lines (paragraph-lines paragraph)))
    (or (find-if (lambda (line) (< y (+ (text-line-y line) (text-line-height line)))) lines)
        (first (last lines)))))

(-> paragraph-index-at (paragraph integer integer) integer)
(defun paragraph-index-at (paragraph x y)
  "Return the index of the character boundary of PARAGRAPH nearest the point X, Y, measured from its corner."
  (let ((line (paragraph--line-at paragraph y)))
    (if (null line)
        0
        (let ((fragment (or (find-if (lambda (fragment) (< x (+ (fragment-x fragment) (fragment-width fragment))))
                                     (text-line-fragments line))
                            (first (last (text-line-fragments line))))))
          (cond
            ((null fragment)
             (text-line-start line))
            ((<= x (fragment-x fragment))
             (fragment-start fragment))
            (t
             (let* ((style  (paragraph--span-style paragraph (fragment-span fragment)))
                    (length (length (fragment-text fragment)))
                    (offset (- x (fragment-x fragment)))
                    (count  (loop for count from 1 to length
                                  for before = 0 then after
                                  for after = (paragraph--prefix-width fragment style count)
                                  when (< offset (/ (+ before after) 2))
                                    do (return (1- count))
                                  finally (return length))))
               (+ (fragment-start fragment) count))))))))

(-> paragraph-span-at (paragraph integer integer) (or null span))
(defun paragraph-span-at (paragraph x y)
  "Return the span of PARAGRAPH drawn at the point X, Y, or NIL when the point is on no text."
  (let ((line (find-if (lambda (line)
                         (and (>= y (text-line-y line))
                              (< y (+ (text-line-y line) (text-line-height line)))))
                       (paragraph-lines paragraph))))
    (when line
      (let ((fragment (find-if (lambda (fragment)
                                 (and (>= x (fragment-x fragment))
                                      (< x (+ (fragment-x fragment) (fragment-width fragment)))))
                               (text-line-fragments line))))
        (and fragment (fragment-span fragment))))))


;;;; -- Drawing --

(-> paragraph-paint (paragraph integer integer &key (:selection (or null cons)) (:top integer) (:bottom integer))
    null)
(defun paragraph-paint (paragraph x y &key selection (top most-negative-fixnum) (bottom most-positive-fixnum))
  "Draw PARAGRAPH's laid-out lines with its corner at X, Y, skipping lines outside TOP to BOTTOM.

SELECTION, a (START . END) pair of character indexes, is drawn inverted."
  (dolist (line (paragraph-lines paragraph))
    (let ((line-top (+ y (text-line-y line))))
      (when (and (< line-top bottom) (> (+ line-top (text-line-height line)) top))
        (let ((baseline (+ line-top (text-line-baseline line))))
          (dolist (fragment (text-line-fragments line))
            (let ((style (paragraph--span-style paragraph (fragment-span fragment))))
              (paint-text (fragment-text fragment) (+ x (fragment-x fragment)) baseline :style style)
              (when selection
                (let* ((start (max (first selection) (fragment-start fragment)))
                       (end   (min (rest selection)
                                   (+ (fragment-start fragment) (length (fragment-text fragment))))))
                  (when (< start end)
                    (let* ((from  (- start (fragment-start fragment)))
                           (to    (- end (fragment-start fragment)))
                           (left  (paragraph--prefix-width fragment style from))
                           (right (paragraph--prefix-width fragment style to)))
                      (with-inversion ((+ x (fragment-x fragment) left) line-top
                                       (- right left) (text-line-height line))
                        (paint-text (subseq (fragment-text fragment) from to) (+ x (fragment-x fragment) left) baseline
                                    :style style
                                    :color (ink-color)))))))))))))
  nil)

(-> spans (&rest t) list)
(defun spans (&rest items)
  "Return a list of spans from ITEMS: a string is a span in the paragraph's own style, a (TEXT STYLE) list one in STYLE, and a span stands."
  (loop for item in items
        when item
          collect (etypecase item
                    (string
                     (make-span item nil))
                    (span
                     item)
                    (cons
                     (make-span (first item) (second item))))))
