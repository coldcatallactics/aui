(in-package #:aui)

;;;; -- Markdown --

;;; A small, deterministic reading of the Markdown that agents write, into a
;;; list of block nodes whose text is a list of inline nodes. Pure functions:
;;; no fonts, no widgets. The prose view gives the nodes their typography.
;;;
;;; A block node is one of
;;;   (:heading LEVEL INLINES)
;;;   (:paragraph INLINES)
;;;   (:item DEPTH MARKER INLINES)   MARKER is "•", a number such as "2.", or :DONE / :OPEN for a task
;;;   (:quote INLINES)
;;;   (:code LANGUAGE TEXT)
;;;   (:table HEADER-CELLS ROWS)     each cell a list of inline nodes
;;;   (:rule)
;;; and an inline node is (TEXT . PROPERTIES) with the properties :BOLD,
;;; :ITALIC, :CODE, :STRIKE and :LINK.

(-> markdown--blank-p (string) boolean)
(defun markdown--blank-p (line)
  "Return T when LINE holds nothing but spaces and tabs."
  (every (lambda (character) (member character '(#\Space #\Tab))) line))

(-> markdown--indent (string) integer)
(defun markdown--indent (line)
  "Return how many columns LINE is indented, a tab counting as four."
  (loop for character across line
        while (member character '(#\Space #\Tab))
        sum (if (char= character #\Tab) 4 1)))

(-> markdown--fence (string) (or null string))
(defun markdown--fence (line)
  "Return what follows the fence when LINE opens or closes a fenced code block, else NIL."
  (let ((trimmed (string-left-trim " " line)))
    (when (and (>= (length trimmed) 3)
               (or (string= "```" trimmed :end2 3) (string= "~~~" trimmed :end2 3)))
      (string-trim " " (subseq trimmed 3)))))

(-> markdown--heading (string) (values (or null integer) string))
(defun markdown--heading (line)
  "Return LINE's heading level and its text, or NIL when LINE is not a heading."
  (let ((hashes (or (position #\# line :test #'char/=) (length line))))
    (if (and (<= 1 hashes 6) (< hashes (length line)) (char= (char line hashes) #\Space))
        (values hashes (string-trim " #" (subseq line hashes)))
        (values nil ""))))

(-> markdown--rule-p (string) boolean)
(defun markdown--rule-p (line)
  "Return T when LINE is a horizontal rule: three or more of one of - * _ and nothing else."
  (let ((trimmed (remove #\Space line)))
    (and (>= (length trimmed) 3)
         (member (char trimmed 0) '(#\- #\* #\_))
         (every (lambda (character) (char= character (char trimmed 0))) trimmed)
         t)))

(-> markdown--item (string) (values (or null string keyword) integer string))
(defun markdown--item (line)
  "Return a list item LINE's marker, its nesting depth and its text; the marker is NIL when LINE is no item."
  (let* ((indent (markdown--indent line))
         (body   (string-left-trim '(#\Space #\Tab) line))
         (depth  (floor indent 2)))
    (cond
      ((and (>= (length body) 2) (find (char body 0) "-*+") (char= (char body 1) #\Space))
       (let ((text (string-left-trim " " (subseq body 2))))
         (cond
           ((and (>= (length text) 3) (string-equal "[x]" text :end2 3))
            (values ':done depth (string-left-trim " " (subseq text 3))))
           ((and (>= (length text) 3) (string= "[ ]" text :end2 3))
            (values ':open depth (string-left-trim " " (subseq text 3))))
           (t
            (values "•" depth text)))))
      (t
       (let ((digits (or (position-if-not #'digit-char-p body) 0)))
         (if (and (plusp digits)
                  (> (length body) (1+ digits))
                  (find (char body digits) ".)")
                  (char= (char body (1+ digits)) #\Space))
             (values (format nil "~A." (subseq body 0 digits)) depth
                     (string-left-trim " " (subseq body (+ digits 2))))
             (values nil depth "")))))))

(-> markdown--table-cells (string) list)
(defun markdown--table-cells (line)
  "Return the cell texts of the table row LINE."
  (let* ((trimmed (string-trim " " line))
         (inner   (string-trim "|" trimmed))
         (cells   nil)
         (start   0))
    (loop
      (let ((bar (position #\| inner :start start)))
        (push (string-trim " " (subseq inner start bar)) cells)
        (if bar
            (setf start (1+ bar))
            (return))))
    (nreverse cells)))

(-> markdown--table-row-p (string) boolean)
(defun markdown--table-row-p (line)
  "Return T when LINE looks like a table row: it starts with a bar and has another."
  (let ((trimmed (string-trim " " line)))
    (and (> (length trimmed) 2)
         (char= (char trimmed 0) #\|)
         (find #\| trimmed :start 1)
         t)))

(-> markdown--table-separator-p (string) boolean)
(defun markdown--table-separator-p (line)
  "Return T when LINE is the row of dashes under a table's header."
  (and (markdown--table-row-p line)
       (every (lambda (character) (find character "|-: ")) line)
       (find #\- line)
       t))


;;;; -- Inline --

(-> markdown--url-end (string integer) integer)
(defun markdown--url-end (text start)
  "Return where the bare URL beginning at START in TEXT ends, leaving closing punctuation outside it."
  (let ((end (or (position-if (lambda (character)
                                (member character '(#\Space #\Newline #\Tab #\< #\> #\" #\`)))
                              text :start start)
                 (length text))))
    (loop while (and (> end start) (find (char text (1- end)) ".,;:!?)]'"))
          do (decf end))
    end))

(-> markdown-inline (string) list)
(defun markdown-inline (text)
  "Return TEXT's inline nodes: (TEXT . PROPERTIES) for each run.

Handles `code`, **bold**, *italic*, _italic_, ***both***, ~~struck~~,
[label](url), ![alt](url) and bare http links. Markers without a partner
stay as text."
  (let ((nodes  nil)
        (plain  (make-string-output-stream))
        (index  0)
        (length (length text)))
    (labels ((flush ()
               (let ((pending (get-output-stream-string plain)))
                 (when (plusp (length pending))
                   (push (list pending) nodes))))

             (emit (run &rest properties)
               (flush)
               (when (plusp (length run))
                 (push (cons run properties) nodes)))

             (nested (run &rest properties)
               "Emit RUN's own inline nodes with PROPERTIES added to each."
               (flush)
               (dolist (node (markdown-inline run))
                 (push (cons (first node) (append properties (rest node))) nodes)))

             (delimited (marker)
               "Return the text between MARKER here and its next occurrence, advancing past both, or NIL."
               (let* ((start (+ index (length marker)))
                      (close (search marker text :start2 start)))
                 (when (and close (> close start)
                            (not (char= (char text start) #\Space))
                            (not (char= (char text (1- close)) #\Space)))
                   (setf index (+ close (length marker)))
                   (subseq text start close))))

             (word-boundary-p (position)
               (or (<= position 0)
                   (>= position length)
                   (not (alphanumericp (char text position))))))
      (loop while (< index length)
            do (let ((character (char text index)))
                 (cond
                   ((char= character #\\)
                    (when (< (1+ index) length)
                      (write-char (char text (1+ index)) plain))
                    (incf index 2))
                   ((char= character #\`)
                    (let* ((ticks (- (or (position #\` text :start index :test #'char/=) length) index))
                           (run   (delimited (make-string ticks :initial-element #\`))))
                      (if run
                          (emit (string-trim " " run) :code t)
                          (progn
                            (write-string (make-string ticks :initial-element #\`) plain)
                            (incf index ticks)))))
                   ((and (char= character #\*) (string= "***" text :start2 index :end2 (min length (+ index 3))))
                    (let ((run (delimited "***")))
                      (if run
                          (nested run :bold t :italic t)
                          (progn (write-string "***" plain) (incf index 3)))))
                   ((and (char= character #\*) (string= "**" text :start2 index :end2 (min length (+ index 2))))
                    (let ((run (delimited "**")))
                      (if run
                          (nested run :bold t)
                          (progn (write-string "**" plain) (incf index 2)))))
                   ((char= character #\*)
                    (let ((run (delimited "*")))
                      (if run
                          (nested run :italic t)
                          (progn (write-char #\* plain) (incf index)))))
                   ((and (char= character #\_) (word-boundary-p (1- index)))
                    (let* ((marker (if (string= "__" text :start2 index :end2 (min length (+ index 2))) "__" "_"))
                           (saved  index)
                           (run    (delimited marker)))
                      (if (and run (word-boundary-p index))
                          (if (string= marker "__")
                              (nested run :bold t)
                              (nested run :italic t))
                          (progn
                            (setf index saved)
                            (write-string marker plain)
                            (incf index (length marker))))))
                   ((and (char= character #\~) (string= "~~" text :start2 index :end2 (min length (+ index 2))))
                    (let ((run (delimited "~~")))
                      (if run
                          (nested run :strike t)
                          (progn (write-string "~~" plain) (incf index 2)))))
                   ((or (char= character #\[)
                        (and (char= character #\!) (< (1+ index) length) (char= (char text (1+ index)) #\[)))
                    (let* ((open    (if (char= character #\!) (1+ index) index))
                           (close   (position #\] text :start open))
                           (url-end (and close
                                         (< (1+ close) length)
                                         (char= (char text (1+ close)) #\()
                                         (position #\) text :start (1+ close)))))
                      (if url-end
                          (let ((label (subseq text (1+ open) close))
                                (url   (subseq text (+ close 2) url-end)))
                            ;; A label is text like any other: its own marks are read.
                            (if (plusp (length label))
                                (nested label :link url)
                                (emit url :link url))
                            (setf index (1+ url-end)))
                          (progn (write-char character plain) (incf index)))))
                   ((and (char= character #\h)
                         (word-boundary-p (1- index))
                         (or (string= "http://" text :start2 index :end2 (min length (+ index 7)))
                             (string= "https://" text :start2 index :end2 (min length (+ index 8)))))
                    (let ((end (markdown--url-end text index)))
                      (emit (subseq text index end) :link (subseq text index end))
                      (setf index end)))
                   (t
                    (write-char character plain)
                    (incf index)))))
      (flush))
    (nreverse nodes)))


;;;; -- Blocks --

(-> markdown-lines (string) list)
(defun markdown-lines (text)
  "Return TEXT's lines, keeping empty ones, with any carriage returns removed."
  (let ((lines nil)
        (start 0))
    (loop
      (let ((end (position #\Newline text :start start)))
        (push (string-right-trim '(#\Return) (subseq text start end)) lines)
        (if end
            (setf start (1+ end))
            (return))))
    (nreverse lines)))

(-> markdown-blocks (string &key (:breaks-p boolean)) list)
(defun markdown-blocks (text &key breaks-p)
  "Return TEXT's block nodes in order.

A fenced code block still open at the end of TEXT, as while an answer is
streaming, is returned as a code block of what has arrived. BREAKS-P keeps
the line breaks inside a paragraph, as a person who typed them meant them;
without it they are spaces, as Markdown has it."
  (let ((lines     (markdown-lines text))
        (blocks    nil)
        (paragraph nil))
    (labels ((flush ()
               (when paragraph
                 (push (list ':paragraph
                             (markdown-inline (format nil (if breaks-p "~{~A~^~%~}" "~{~A~^ ~}")
                                                      (nreverse paragraph))))
                       blocks)
                 (setf paragraph nil))))
      (loop while lines
            do (let ((line (pop lines)))
                 (multiple-value-bind (level heading) (markdown--heading line)
                   (multiple-value-bind (marker depth item) (markdown--item line)
                     (cond
                       ((markdown--fence line)
                        (flush)
                        (let ((language (markdown--fence line))
                              (code     nil))
                          (loop while (and lines (not (markdown--fence (first lines))))
                                do (push (pop lines) code))
                          (pop lines)
                          (push (list ':code
                                      (and (plusp (length language)) language)
                                      (format nil "~{~A~^~%~}" (nreverse code)))
                                blocks)))
                       ((markdown--blank-p line)
                        (flush))
                       (level
                        (flush)
                        (push (list ':heading level (markdown-inline heading)) blocks))
                       ((and (markdown--rule-p line) (null paragraph))
                        (push (list ':rule) blocks))
                       ((and (markdown--table-row-p line)
                             lines
                             (markdown--table-separator-p (first lines)))
                        (flush)
                        (pop lines)
                        (let ((header (mapcar #'markdown-inline (markdown--table-cells line)))
                              (rows   nil))
                          (loop while (and lines (markdown--table-row-p (first lines)))
                                do (push (mapcar #'markdown-inline (markdown--table-cells (pop lines))) rows))
                          (push (list ':table header (nreverse rows)) blocks)))
                       (marker
                        (flush)
                        ;; Lines indented under an item continue its text.
                        (let ((parts (list item)))
                          (loop while (and lines
                                           (not (markdown--blank-p (first lines)))
                                           (> (markdown--indent (first lines)) (* 2 depth))
                                           (null (markdown--item (first lines)))
                                           (null (markdown--fence (first lines))))
                                do (push (string-trim " " (pop lines)) parts))
                          (push (list ':item depth marker
                                      (markdown-inline (format nil "~{~A~^ ~}" (nreverse parts))))
                                blocks)))
                       ((and (plusp (length line)) (char= (char (string-left-trim " " line) 0) #\>))
                        (flush)
                        (let ((parts (list (string-left-trim "> " line))))
                          (loop while (and lines
                                           (plusp (length (string-left-trim " " (first lines))))
                                           (char= (char (string-left-trim " " (first lines)) 0) #\>))
                                do (push (string-left-trim "> " (pop lines)) parts))
                          (push (list ':quote (markdown-inline (format nil "~{~A~^ ~}" (nreverse parts))))
                                blocks)))
                       (t
                        (push (string-trim " " line) paragraph)))))))
      (flush))
    (nreverse blocks)))
