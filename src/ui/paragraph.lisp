(in-package #:aui)

;;;; -- Paragraphs --

;;; Styled text broken into lines. Runs of styled text are broken at word
;;; boundaries to fit a width. The positions of the fragments are kept, so
;;; that a point can be mapped back to a character and a range of characters
;;; can be drawn as selected. All text in the transcript is set in paragraphs
;;; inside blocks.

(defstruct (span (:constructor %make-span (text style link)))
  "A run of a paragraph. TEXT is set in STYLE, and LINK is opened if the run is clicked."
  (text  ""  :type string)
  (style nil :type (or null text-style))
  (link  nil))

(-> text--overwrite (string) string)
(defun text--overwrite (line)
  "Return LINE as a terminal shows it after the carriage returns. Each part of LINE is written over the part before it."
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
        (column 0))
    (loop for character across line
          do (cond
               ((char= character #\Return)
                (setf column 0))
               ((< column (length result))
                (setf (aref result column) character)
                (incf column))
               (t
                (vector-push-extend character result)
                (incf column))))
    (coerce result 'simple-string)))

(-> text-clean (string) string)
(defun text-clean (text)
  "Return TEXT as a terminal shows it.

Escape sequences for color and movement are removed. Text after a carriage
return is written over the line. Tabs are expanded to the next multiple of
eight columns. Other control characters are removed. TEXT is returned
unchanged if it contains none of these."
  (if (not (find-if (lambda (character)
                      (and (< (char-code character) 32) (char/= character #\Newline)))
                    text))
      text
      (with-output-to-string (stream)
        (let ((start 0))
          (loop
            (let* ((end    (position #\Newline text :start start))
                   (line   (subseq text start end))
                   (kept   (make-string-output-stream))
                   (index  0)
                   (column 0))
              ;; Handle escape sequences first, then tabs, then the other control characters.
              (loop while (< index (length line))
                    do (let ((character (char line index)))
                         (cond
                           ((and (char= character (code-char 27))
                                 (< (1+ index) (length line))
                                 (char= (char line (1+ index)) #\[))
                            (incf index 2)
                            (loop while (and (< index (length line))
                                             (not (<= 64 (char-code (char line index)) 126)))
                                  do (incf index))
                            (incf index))
                           ((char= character #\Tab)
                            (let ((spaces (- 8 (mod column 8))))
                              (dotimes (count spaces)
                                (write-char #\Space kept))
                              (incf column spaces))
                            (incf index))
                           ((char= character #\Return)
                            (write-char character kept)
                            (setf column 0)
                            (incf index))
                           ((< (char-code character) 32)
                            (incf index))
                           (t
                            (write-char character kept)
                            (incf column)
                            (incf index)))))
              (write-string (text--overwrite (string-right-trim '(#\Return) (get-output-stream-string kept)))
                            stream)
              (if end
                  (progn
                    (write-char #\Newline stream)
                    (setf start (1+ end)))
                  (return))))))))

(-> make-span (string (or null text-style) &key (:link t)) span)
(defun make-span (text style &key link)
  "Return a span of TEXT in STYLE that opens LINK if the span is clicked. TEXT is cleaned by TEXT-CLEAN first."
  (%make-span (text-clean text) style link))

(defstruct fragment
  "A part of one span on one line. It has an X position, a width, the text, the span and the index of the text in the paragraph."
  (x     0   :type integer)
  (width 0   :type integer)
  (text  ""  :type string)
  (span  nil :type (or null span))
  (start 0   :type integer))

(defstruct text-line
  "A laid-out line of a paragraph. It has a top, a height, a baseline, a width, fragments and a character range."
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
    :documentation "The spans of the paragraph in reading order.")
   (style
    :initarg       :style
    :reader        paragraph-style
    :type          text-style
    :documentation "The base style. It is used for spans that have no style and for the height of empty lines.")
   (align
    :initarg       :align
    :initform      ':left
    :reader        paragraph-align
    :type          (member :left :center :right)
    :documentation "The alignment of the lines within the width of the paragraph.")
   (wrap
    :initarg       :wrap
    :initform      ':words
    :reader        paragraph-wrap
    :type          (member :words :anywhere)
    :documentation "With :WORDS, lines are broken between words, as in prose. With :ANYWHERE, lines are broken where the width ends, as in code.")
   (hanging
    :initarg       :hanging
    :initform      0
    :reader        paragraph-hanging
    :type          integer
    :documentation "The indentation of the wrapped lines that follow the first line of each hard line.")
   (width
    :initform      -1
    :accessor      paragraph-width
    :type          integer
    :documentation "The width used for the last layout, or -1 before the first layout.")
   (lines
    :initform      nil
    :accessor      paragraph-lines
    :type          list
    :documentation "The laid-out lines, from top to bottom.")
   (height
    :initform      0
    :accessor      paragraph-height
    :type          integer
    :documentation "The total height of the laid-out lines.")
   (extent
    :initform      0
    :accessor      paragraph-extent
    :type          integer
    :documentation "The width of the widest laid-out line."))
  (:documentation "Styled text broken into lines."))

(-> make-paragraph (list &key (:style text-style) (:align keyword) (:wrap keyword) (:hanging integer)) paragraph)
(defun make-paragraph (spans &key (style (text-style ':body)) (align ':left) (wrap ':words) (hanging 0))
  "Return a paragraph of SPANS. Spans without a style are set in STYLE."
  (make-instance 'paragraph :spans spans :style style :align align :wrap wrap :hanging hanging))

(-> paragraph-text (paragraph) string)
(defun paragraph-text (paragraph)
  "Return the text of PARAGRAPH without the styles."
  (apply #'concatenate 'string (mapcar #'span-text (paragraph-spans paragraph))))

(-> paragraph--span-style (paragraph span) text-style)
(defun paragraph--span-style (paragraph span)
  "Return the style of SPAN within PARAGRAPH."
  (or (span-style span) (paragraph-style paragraph)))


;;;; -- Layout --

(-> paragraph--tokens (paragraph) list)
(defun paragraph--tokens (paragraph)
  "Return the text of PARAGRAPH as a list of tokens (KIND TEXT SPAN START). KIND is :WORD, :SPACE or :BREAK.

If the paragraph wraps :ANYWHERE, each character is a word."
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
  "Return WORD cut into pieces that fit WIDTH in STYLE."
  (let ((pieces nil)
        (start  0))
    (loop while (< start (length word))
          do (let ((end (1+ start)))
               (loop while (and (< end (length word))
                                (<= (text-measure (subseq word start (1+ end)) style) width))
                     do (incf end))
               (push (subseq word start end) pieces)
               (setf start end)))
    (nreverse pieces)))

(-> paragraph-layout (paragraph integer) integer)
(defun paragraph-layout (paragraph width)
  "Break PARAGRAPH into lines that fit WIDTH and return the height of PARAGRAPH. If WIDTH is the width of the last layout, the last layout is kept."
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
                 "Extend the last fragment if TEXT continues the same span. Otherwise, start a new fragment."
                 (let* ((style    (paragraph--span-style paragraph span))
                        (previous (first fragments)))
                   (if (and previous
                            (eq (fragment-span previous) span)
                            (= (+ (fragment-start previous) (length (fragment-text previous))) start))
                       (let ((joined (concatenate 'string (fragment-text previous) text)))
                         (setf (fragment-text previous)  joined
                               (fragment-width previous) (text-measure joined style)
                               x                         (+ (fragment-x previous) (fragment-width previous))))
                       (let ((measured (text-width text style)))
                         (push (make-fragment :x x :width measured :text text :span span :start start)
                               fragments)
                         (incf x measured)))
                   (setf position (+ start (length text)))))

               (finish (hard-p)
                 "Finish the current line and start the next line."
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
                                           (text-measure (string-right-trim " " (fragment-text last))
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
     (text-measure (subseq (fragment-text fragment) 0 count) style))))

(-> paragraph--line-at (paragraph integer) (or null text-line))
(defun paragraph--line-at (paragraph y)
  "Return the line of PARAGRAPH at the height Y. If Y is above the paragraph, return the first line. If Y is below it, return the last line."
  (let ((lines (paragraph-lines paragraph)))
    (or (find-if (lambda (line) (< y (+ (text-line-y line) (text-line-height line)))) lines)
        (first (last lines)))))

(-> paragraph-index-at (paragraph integer integer) integer)
(defun paragraph-index-at (paragraph x y)
  "Return the index of the character boundary of PARAGRAPH nearest to the point X, Y. The point is relative to the top-left corner of PARAGRAPH."
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


;;;; -- Drawing --

(-> paragraph-paint (paragraph integer integer &key (:selection (or null cons)) (:top integer) (:bottom integer))
    null)
(defun paragraph-paint (paragraph x y &key selection (top most-negative-fixnum) (bottom most-positive-fixnum))
  "Draw the laid-out lines of PARAGRAPH. The top-left corner of PARAGRAPH is at X, Y. Lines outside the range from TOP to BOTTOM are skipped.

SELECTION is a (START . END) pair of character indexes. The selected characters are drawn inverted."
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
  "Return a list of spans made from ITEMS.

Strings become spans in the style of the paragraph. (TEXT STYLE) lists
become spans in STYLE. Spans are used as they are. NIL items are skipped."
  (loop for item in items
        when item
          collect (etypecase item
                    (string
                     (make-span item nil))
                    (span
                     item)
                    (cons
                     (make-span (first item) (second item))))))
