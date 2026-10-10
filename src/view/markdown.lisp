(in-package #:aui)

;;;; -- Markdown Runs --

;;; A small, deterministic reading of the Markdown Autolith writes, into runs
;;; of text with a style name. The transcript view turns the style names into
;;; style table indices. Pure functions; no widgets.

(deftype markdown-style ()
  "The typographic roles Markdown itself produces; the document adds more."
  '(member :body :bold :italic :bold-italic :code :code-block :code-bold :code-italic
    :heading-1 :heading-2 :heading-3 :link :bullet :list-number :quote :strike :table :marker))

(defstruct (run (:constructor make-run (text style &optional url)))
  "One stretch of text in one style named by a keyword; URL is set for links."
  (text "" :type string)
  (style ':body :type keyword)
  (url nil))

(-> markdown-fence-p (string) boolean)
(defun markdown-fence-p (line)
  "Return T when LINE opens or closes a fenced code block."
  (let ((trimmed (string-left-trim " " line)))
    (and (>= (length trimmed) 3)
         (string= "```" trimmed :end2 3))))

(-> markdown-heading-level (string) (or null integer))
(defun markdown-heading-level (line)
  "Return the heading level of LINE, or NIL when it is not a heading."
  (let ((hashes (or (position-if-not (lambda (character) (char= character #\#)) line)
                    (length line))))
    (when (and (<= 1 hashes 6)
               (< hashes (length line))
               (char= #\Space (char line hashes)))
      hashes)))

(-> markdown-list-marker (string) (values (or null integer) integer string))
(defun markdown-list-marker (line)
  "Return three values for a list item LINE: the length of its marker prefix
including the space after it, its indentation, and the label to show, a
bullet for -, * and + or the number itself for a numbered item. The first
value is NIL when LINE is not a list item."
  (let* ((indent (or (position-if-not (lambda (character) (char= character #\Space)) line)
                     (length line)))
         (rest (subseq line indent)))
    (cond
      ((and (>= (length rest) 2)
            (find (char rest 0) "-*+")
            (char= #\Space (char rest 1)))
       (values (+ indent 2) indent "• "))
      (t
       (let ((digits (or (position-if-not #'digit-char-p rest) 0)))
         (if (and (plusp digits)
                  (> (length rest) (1+ digits))
                  (char= #\. (char rest digits))
                  (char= #\Space (char rest (1+ digits))))
             (values (+ indent digits 2) indent (subseq rest 0 (+ digits 2)))
             (values nil indent "")))))))

(-> runs-merge-adjacent (list) list)
(defun runs-merge-adjacent (runs)
  "Return RUNS with neighbours of one style and no url joined into one run."
  (let ((merged nil))
    (dolist (run runs)
      (let ((previous (first merged)))
        (if (and previous
                 (eq (run-style previous) (run-style run))
                 (null (run-url previous))
                 (null (run-url run)))
            (setf (run-text previous) (concatenate 'string (run-text previous) (run-text run)))
            (push (make-run (run-text run) (run-style run) (run-url run)) merged))))
    (nreverse merged)))

(-> markdown--emphasis-style (integer keyword) keyword)
(defun markdown--emphasis-style (asterisks base)
  "Return the style for text inside ASTERISKS markers, one or two, on BASE.
Inside a heading or a quote the base style stands; elsewhere two markers mean
bold and one means italic."
  (cond
    ((not (eq base ':body)) base)
    ((= asterisks 2) ':bold)
    (t ':italic)))

(-> markdown--find-closing (string integer string) (or null integer))
(defun markdown--find-closing (text start marker)
  "Return the position of MARKER in TEXT at or after START, or NIL."
  (search marker text :start2 start))

(-> markdown-inline-runs (string &key (:base keyword)) list)
(defun markdown-inline-runs (text &key (base ':body))
  "Return the runs of one line of inline Markdown TEXT.
Handles `code`, **bold**, *italic*, _italic_, ***bold italic***,
~~struck~~, [label](url) and ![alt](url). Unmatched markers are kept as text."
  (let ((runs nil)
        (plain (make-string-output-stream))
        (position 0)
        (length (length text)))
    (labels ((flush ()
               (let ((pending (get-output-stream-string plain)))
                 (when (plusp (length pending))
                   (push (make-run pending base) runs))))
             (emit (run-text style &optional url)
               (flush)
               (when (plusp (length run-text))
                 (push (make-run run-text style url) runs)))
             (marker-run (marker style)
               (let* ((start (+ position (length marker)))
                      (close (markdown--find-closing text start marker)))
                 (when (and close (> close start))
                   (emit (subseq text start close) style)
                   (setf position (+ close (length marker)))
                   t))))
      (loop while (< position length)
            do (let ((character (char text position)))
                 (cond
                   ((and (char= character #\`) (marker-run "`" ':code)))
                   ((and (char= character #\~) (< (1+ position) length)
                         (char= #\~ (char text (1+ position)))
                         (marker-run "~~" ':strike)))
                   ((and (char= character #\!) (< (1+ position) length)
                         (char= #\[ (char text (1+ position)))
                         (let* ((close-bracket (position #\] text :start position))
                                (open-paren (and close-bracket (< (1+ close-bracket) length)
                                                 (char= #\( (char text (1+ close-bracket)))
                                                 (1+ close-bracket)))
                                (close-paren (and open-paren (position #\) text :start open-paren))))
                           (when (and close-bracket open-paren close-paren)
                             (let ((alt (subseq text (+ position 2) close-bracket)))
                               (emit (format nil "[image~@[: ~A~]]" (and (plusp (length alt)) alt)) ':link
                                     (subseq text (1+ open-paren) close-paren)))
                             (setf position (1+ close-paren))
                             t))))
                   ((and (char= character #\*) (< (+ position 2) length)
                         (string= "***" text :start2 position :end2 (+ position 3))
                         (marker-run "***" (if (eq base ':body) ':bold-italic base))))
                   ((and (char= character #\*) (< (1+ position) length)
                         (char= #\* (char text (1+ position)))
                         (marker-run "**" (markdown--emphasis-style 2 base))))
                   ((and (char= character #\*) (marker-run "*" (markdown--emphasis-style 1 base))))
                   ((and (char= character #\_)
                         (or (zerop position) (not (alphanumericp (char text (1- position)))))
                         (marker-run "_" (markdown--emphasis-style 1 base))))
                   ((and (char= character #\[)
                         (let* ((close-bracket (position #\] text :start position))
                                (open-paren (and close-bracket (< (1+ close-bracket) length)
                                                 (char= #\( (char text (1+ close-bracket)))
                                                 (1+ close-bracket)))
                                (close-paren (and open-paren (position #\) text :start open-paren))))
                           (when (and close-bracket open-paren close-paren)
                             (emit (subseq text (1+ position) close-bracket) ':link
                                   (subseq text (1+ open-paren) close-paren))
                             (setf position (1+ close-paren))
                             t))))
                   (t
                    (write-char character plain)
                    (incf position)))))
      (flush))
    (runs-merge-adjacent (nreverse runs))))

(-> markdown-table-row-p (string) boolean)
(defun markdown-table-row-p (line)
  "Return T when LINE is a table row: it begins with a pipe after indentation."
  (let ((trimmed (string-left-trim " " line)))
    (and (plusp (length trimmed)) (char= #\| (char trimmed 0)) t)))

(-> markdown-table-separator-p (string) boolean)
(defun markdown-table-separator-p (line)
  "Return T when LINE is the dashes row under a table header."
  (and (markdown-table-row-p line)
       (every (lambda (character) (find character "|-: ")) line)
       (find #\- line)
       t))

(-> markdown-table-cells (string) list)
(defun markdown-table-cells (line)
  "Return the trimmed cell texts of the table row LINE."
  (let* ((trimmed (string-trim " " line))
         (inner (string-trim "|" trimmed))
         (cells nil)
         (start 0))
    (loop for end = (position #\| inner :start start)
          do (push (string-trim " " (subseq inner start end)) cells)
          while end
          do (setf start (1+ end)))
    (nreverse cells)))

(-> markdown-table-runs (list) list)
(defun markdown-table-runs (lines)
  "Return runs showing the table LINES as a monospace grid: the header bold,
cells padded to their column's width, columns separated by two spaces."
  (let* ((rows (mapcar #'markdown-table-cells (remove-if #'markdown-table-separator-p lines)))
         (columns (reduce #'max rows :key #'length :initial-value 0))
         (widths (loop for column below columns
                       collect (reduce #'max rows
                                       :key (lambda (row) (length (or (nth column row) "")))
                                       :initial-value 0)))
         (runs nil))
    (loop for row in rows
          for header-p = t then nil
          do (loop for column below columns
                   for width in widths
                   for cell = (or (nth column row) "")
                   do (push (make-run (format nil "~vA~:[  ~;~]" width cell (= column (1- columns)))
                                      (if header-p ':code-bold ':table))
                            runs))
             (push (make-run (string #\Newline) ':body) runs))
    (nreverse runs)))

(-> markdown-task-marker (string) (or null string))
(defun markdown-task-marker (text)
  "Return the glyph for a task item beginning TEXT, or NIL when it is none."
  (cond
    ((or (uiop:string-prefix-p "[ ] " text)) "☐ ")
    ((or (uiop:string-prefix-p "[x] " text) (uiop:string-prefix-p "[X] " text)) "☑ ")
    (t nil)))

(-> markdown-runs (string) list)
(defun markdown-runs (text)
  "Return the runs for the Markdown document TEXT, line breaks included.
Fenced code becomes highlighted :CODE-BLOCK runs, headings become sized
heading runs that swallow the blank line after them, since their size already
sets them off, list items get a bullet or a bold number, task items a box,
quotes an italic indented run, tables a monospace grid, and everything else
goes through MARKDOWN-INLINE-RUNS."
  (let ((runs nil)
        (lines (split-lines-keeping-empty text)))
    (flet ((newline ()
             (push (make-run (string #\Newline) ':body) runs)))
      (loop while lines
            do (let ((line (pop lines)))
                 (cond
                   ((markdown-fence-p line)
                    (let ((language (string-trim " " (subseq (string-left-trim " " line) 3)))
                          (code nil))
                      (loop while (and lines (not (markdown-fence-p (first lines))))
                            do (push (pop lines) code))
                      (when lines (pop lines))
                      (dolist (run (code-runs (format nil "~{~A~%~}" (nreverse code)) language))
                        (push run runs))))
                   ((markdown-table-row-p line)
                    (let ((table (list line)))
                      (loop while (and lines (markdown-table-row-p (first lines)))
                            do (push (pop lines) table))
                      (dolist (run (markdown-table-runs (nreverse table)))
                        (push run runs))))
                   ((markdown-heading-level line)
                    (let* ((level (markdown-heading-level line))
                           (style (case level (1 ':heading-1) (2 ':heading-2) (t ':heading-3))))
                      (dolist (run (markdown-inline-runs (string-trim " " (subseq line level)) :base style))
                        (push run runs))
                      (newline)
                      (when (and lines (string= (string-trim " " (first lines)) ""))
                        (pop lines))))
                   ((and (> (length line) 0) (char= #\> (char (string-left-trim " " line) 0)))
                    (push (make-run "│ " ':quote) runs)
                    (dolist (run (markdown-inline-runs (string-trim " " (subseq (string-left-trim " " line) 1))
                                                       :base ':quote))
                      (push run runs))
                    (newline))
                   ((markdown-list-marker line)
                    (multiple-value-bind (marker indent label) (markdown-list-marker line)
                      (let* ((rest (subseq line marker))
                             (task (markdown-task-marker rest)))
                        (when (plusp indent)
                          (push (make-run (make-string indent :initial-element #\Space) ':body) runs))
                        (push (make-run (or task label)
                                        (cond (task ':marker)
                                              ((string= label "• ") ':bullet)
                                              (t ':list-number)))
                              runs)
                        (dolist (run (markdown-inline-runs (if task (subseq rest 4) rest)))
                          (push run runs))
                        (newline))))
                   (t
                    (dolist (run (markdown-inline-runs line))
                      (push run runs))
                    (newline))))))
    (nreverse runs)))

(defparameter *lisp-keywords*
  '("defun" "defmacro" "defvar" "defparameter" "defclass" "defmethod" "defgeneric" "defstruct"
    "deftype" "define-condition" "defpackage" "in-package" "let" "let*" "lambda" "if" "when"
    "unless" "cond" "case" "ecase" "typecase" "etypecase" "loop" "do" "dolist" "dotimes" "setf"
    "setq" "progn" "prog1" "and" "or" "not" "return" "return-from" "block" "handler-case"
    "handler-bind" "unwind-protect" "multiple-value-bind" "destructuring-bind" "flet" "labels"
    "quote" "function" "values" "nil" "t" "with-open-file" "with-slots" "with-accessors")
  "Words set in bold inside Lisp code.")

(defparameter *algol-keywords*
  '("if" "else" "elif" "for" "while" "do" "return" "function" "def" "class" "import" "from"
    "let" "const" "var" "fn" "pub" "struct" "enum" "match" "switch" "case" "break" "continue"
    "new" "try" "catch" "finally" "async" "await" "in" "not" "and" "or" "then" "fi" "done"
    "esac" "export" "local" "null" "true" "false" "None" "True" "False" "int" "void" "static")
  "Words set in bold inside code in curly-brace and scripting languages.")

(-> code-comment-start (string string) (or null integer))
(defun code-comment-start (line language)
  "Return where a comment starts in LINE for LANGUAGE, or NIL."
  (let ((lisp-p (member language '("lisp" "common-lisp" "cl" "scheme" "elisp" "clojure") :test #'string-equal))
        (hash-p (member language '("sh" "bash" "zsh" "shell" "python" "py" "ruby" "yaml" "toml" "make" "makefile" "nix")
                        :test #'string-equal)))
    (cond
      (lisp-p (position #\; line))
      (hash-p (position #\# line))
      ((string= language "") (or (search "//" line) (position #\; line)))
      (t (search "//" line)))))

(-> code-runs (string string) list)
(defun code-runs (code language)
  "Return :CODE-BLOCK runs for CODE with Algol-style monochrome highlighting:
keywords bold, comments italic, everything else roman."
  (let ((keywords (if (member language '("lisp" "common-lisp" "cl" "scheme" "elisp" "clojure") :test #'string-equal)
                      *lisp-keywords*
                      *algol-keywords*))
        (runs nil))
    (dolist (line (split-lines-keeping-empty code))
      (let* ((comment (code-comment-start line language))
             (code-part (if comment (subseq line 0 comment) line))
             (comment-part (if comment (subseq line comment) ""))
             (start 0))
        (loop while (< start (length code-part))
              do (let* ((word-start (position-if (lambda (character) (or (alphanumericp character)
                                                                           (find character "-*_")))
                                                 code-part :start start))
                        (word-end (and word-start
                                       (or (position-if-not (lambda (character) (or (alphanumericp character)
                                                                                      (find character "-*_")))
                                                            code-part :start word-start)
                                           (length code-part)))))
                   (cond
                     ((null word-start)
                      (push (make-run (subseq code-part start) ':code-block) runs)
                      (setf start (length code-part)))
                     (t
                      (when (> word-start start)
                        (push (make-run (subseq code-part start word-start) ':code-block) runs))
                      (let ((word (subseq code-part word-start word-end)))
                        (push (make-run word (if (member word keywords :test #'string=) ':code-bold ':code-block))
                              runs))
                      (setf start word-end)))))
        (when (plusp (length comment-part))
          (push (make-run comment-part ':code-italic) runs))
        (push (make-run (string #\Newline) ':code-block) runs)))
    (runs-merge-adjacent (nreverse runs))))

(-> split-lines-keeping-empty (string) list)
(defun split-lines-keeping-empty (text)
  "Split TEXT at newlines, keeping empty lines and dropping only the empty
tail that a final newline leaves."
  (let ((lines nil)
        (start 0))
    (loop for end = (position #\Newline text :start start)
          do (push (subseq text start end) lines)
          while end
          do (setf start (1+ end)))
    (when (and (rest lines) (string= "" (first lines)))
      (pop lines))
    (nreverse lines)))

(-> runs-text (list) string)
(defun runs-text (runs)
  "Return the plain text of RUNS joined."
  (apply #'concatenate 'string (mapcar #'run-text runs)))
