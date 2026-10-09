(in-package #:aui)

;;;; -- Markdown Runs --

;;; A small, deterministic reading of the Markdown Autolith writes, into runs
;;; of text with a style name. The transcript view turns the style names into
;;; style table indices. Pure functions; no widgets.

(deftype run-style ()
  "The typographic roles a run can take."
  '(member :body :bold :italic :bold-italic :code :code-block :heading :link :bullet))

(defstruct (run (:constructor make-run (text style &optional url)))
  "One stretch of text in one style; URL is set for links."
  (text "" :type string)
  (style ':body :type run-style)
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

(-> markdown--emphasis-style (integer (member :body :heading)) run-style)
(defun markdown--emphasis-style (asterisks base)
  "Return the style for text inside ASTERISKS markers, one or two, on BASE."
  (if (eq base ':heading)
      ':heading
      (if (= asterisks 2) ':bold ':italic)))

(-> markdown--find-closing (string integer string) (or null integer))
(defun markdown--find-closing (text start marker)
  "Return the position of MARKER in TEXT at or after START, or NIL."
  (search marker text :start2 start))

(-> markdown-inline-runs (string &key (:base (member :body :heading))) list)
(defun markdown-inline-runs (text &key (base ':body))
  "Return the runs of one line of inline Markdown TEXT.
Handles `code`, **bold**, *italic*, _italic_, ***bold italic*** and
[label](url). Unmatched markers are kept as text."
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
                   ((and (char= character #\*) (< (+ position 2) length)
                         (string= "***" text :start2 position :end2 (+ position 3))
                         (marker-run "***" (if (eq base ':heading) ':heading ':bold-italic))))
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

(-> markdown-runs (string) list)
(defun markdown-runs (text)
  "Return the runs for the Markdown document TEXT, line breaks included.
Fenced code becomes :CODE-BLOCK runs, headings become :HEADING runs, list
items get a bullet run, and everything else goes through MARKDOWN-INLINE-RUNS."
  (let ((runs nil)
        (in-fence nil))
    (flet ((newline ()
             (push (make-run (string #\Newline) ':body) runs)))
      (dolist (line (split-lines-keeping-empty text))
        (cond
          ((markdown-fence-p line)
           (setf in-fence (not in-fence)))
          (in-fence
           (push (make-run (concatenate 'string line (string #\Newline)) ':code-block) runs))
          ((markdown-heading-level line)
           (let ((level (markdown-heading-level line)))
             (dolist (run (markdown-inline-runs (string-trim " " (subseq line level)) :base ':heading))
               (push run runs))
             (newline)))
          ((markdown-list-marker line)
           (multiple-value-bind (marker indent label) (markdown-list-marker line)
             (push (make-run (concatenate 'string
                                          (make-string indent :initial-element #\Space)
                                          label)
                             ':bullet)
                   runs)
             (dolist (run (markdown-inline-runs (subseq line marker)))
               (push run runs))
             (newline)))
          (t
           (dolist (run (markdown-inline-runs line))
             (push run runs))
           (newline)))))
    (nreverse runs)))

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
