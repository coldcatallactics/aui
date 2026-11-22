(in-package #:aui)

;;;; -- Proof --


(defstruct passage
  "A source passage with its kind, shape, character range, text, and line range. Comments also retain their prefix, separator, margin, and whether they occupy whole lines."
  (kind      ':comment :type (member :comment :docstring :string))
  (shape     ':line    :type (member :line :block :string))
  (start     0         :type fixnum)
  (end       0         :type fixnum)
  (text      ""        :type string)
  (line      1         :type fixnum)
  (last-line 1         :type fixnum)
  (prefix    ";;"      :type string)
  (separator " "       :type string)
  (margin    ""        :type string)
  (alone-p   t         :type boolean))

(defstruct definition
  "A top-level form with passages. A comment run outside forms has an empty operator."
  (operator ""  :type string)
  (name     ""  :type string)
  (line     1   :type fixnum)
  (passages nil :type list))

(defstruct proof
  "A source file and its parsed definitions, skeleton, digest, and read problem if parsing failed."
  (pathname    nil)
  (text        ""  :type string)
  (digest      ""  :type string)
  (definitions nil :type list)
  (skeleton    ""  :type string)
  (problem     nil :type (or null string)))

(defparameter *documentation-keywords* '(":documentation" ":description" ":long-description")
  "The keywords that are followed by documentation strings in classes, conditions, packages and systems.")

(defparameter *function-definers* '("defun" "defmacro" "defmethod" "deftype" "define-compiler-macro" "defsetf"
                                    "define-setf-expander")
  "The operators in which the string after the lambda list is a docstring.")

(defparameter *variable-definers* '("defvar" "defparameter" "defconstant" "defglobal" "define-symbol-macro")
  "The operators in which the string after the value is a docstring.")


;;;; -- Digest --

(-> octets-digest ((simple-array (unsigned-byte 8) (*))) string)
(defun octets-digest (octets)
    "Return the FNV-1a digest and byte count of OCTETS."
  (let ((sum #xcbf29ce484222325))
    (declare (type (unsigned-byte 64) sum))
    (loop for octet across octets
          do (setf sum (logand #xFFFFFFFFFFFFFFFF
                               (* (logxor sum octet) #x100000001b3))))
    (format nil "~16,'0X-~D" sum (length octets))))

(-> file-octets (pathname) (simple-array (unsigned-byte 8) (*)))
(defun file-octets (pathname)
  "Return the content of the file at PATHNAME as octets."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(-> file-digest (pathname) (or null string))
(defun file-digest (pathname)
  "Return the digest of the content of the file at PATHNAME, or NIL if the file cannot be read."
  (handler-case (octets-digest (file-octets pathname))
    (error ()
      nil)))


;;;; -- Words as They Are Written --

(-> string-literal-text (string) string)
(defun string-literal-text (literal)
  "Return the words of LITERAL, which is a string written with quotes as in Lisp source. The words are the characters between the quotes. Each backslash is removed, and the character after it is kept."
  (with-output-to-string (stream)
    (let ((index 1)
          (end   (1- (length literal))))
      (loop while (< index end)
            do (let ((character (char literal index)))
                 (when (and (char= character #\\) (< (1+ index) end))
                   (incf index)
                   (setf character (char literal index)))
                 (write-char character stream)
                 (incf index))))))

(-> string-literal (string) string)
(defun string-literal (text)
  "Return TEXT as a string in Lisp source. The text is put between quotes, and a backslash is put before each quote and each backslash."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across text
          do (when (or (char= character #\") (char= character #\\))
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(-> text-lines (string) list)
(defun text-lines (text)
  "Return the lines of TEXT without the line ends. If TEXT ends with a line end, the last line is empty."
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start end))
        while end
        do (setf start (1+ end))))

(-> passage-source (passage string) string)
(defun passage-source (passage text)
  "Return TEXT written in the same way as PASSAGE in the file: behind the semicolons on each line, between the bars of a block comment, or between quotes.

The blanks at the end of each comment line are removed. An empty comment line
contains only the semicolons. Signal a PROOF-ERROR if the words of a block
comment would close the comment."
  (ecase (passage-shape passage)
    (:string
     (string-literal (remove #\Return text)))
    (:block
     (when (or (search "|#" text) (search "#|" text))
       (error 'proof-error
              :message "a block comment cannot contain the characters that open or close a block comment"))
     (format nil "#|~A|#" (remove #\Return text)))
    (:line
     (let ((prefix    (passage-prefix passage))
           (separator (passage-separator passage))
           (margin    (passage-margin passage)))
       (with-output-to-string (stream)
         (loop for line in (text-lines text)
               for first-p = t then nil
               do (let ((words (string-right-trim '(#\Space #\Tab) line)))
                    (unless first-p
                      (write-char #\Newline stream)
                      (write-string margin stream))
                    (write-string prefix stream)
                    (when (plusp (length words))
                      (write-string separator stream)
                      (write-string words stream)))))))))


;;;; -- Reading --

(-> proof--line-comment (string list &key (:alone-p boolean) (:margin string) (:line fixnum)) passage)
(defun proof--line-comment (text lexemes &key alone-p (margin "") (line 1))
  "Return one passage for LEXEMES, which are consecutive comment lines in TEXT that are written in the same way."
  (let* ((first     (first lexemes))
         (count     (or (position #\; text :start (lexeme-start first) :end (lexeme-end first) :test #'char/=)
                        (lexeme-end first)))
         (prefix    (subseq text (lexeme-start first) count))
         (rests     (loop for lexeme in lexemes
                          collect (subseq text
                                          (min (lexeme-end lexeme) (+ (lexeme-start lexeme) (length prefix)))
                                          (lexeme-end lexeme))))
         (separator (if (every (lambda (rest)
                                 (or (zerop (length rest)) (char= (char rest 0) #\Space)))
                               rests)
                        " "
                        "")))
    (make-passage :kind      ':comment
                  :shape     ':line
                  :start     (lexeme-start first)
                  :end       (lexeme-end (first (last lexemes)))
                  :line      line
                  :last-line (+ line (length lexemes) -1)
                  :prefix    prefix
                  :separator separator
                  :margin    margin
                  :alone-p   alone-p
                  :text      (format nil "~{~A~^~%~}"
                                     (loop for rest in rests
                                           collect (string-right-trim
                                                    '(#\Space #\Tab)
                                                    (subseq rest (min (length rest) (length separator)))))))))

(-> proof--brief (string fixnum fixnum) string)
(defun proof--brief (text start end)
  "Return the source in TEXT between START and END as one line. Each run of blanks becomes one blank. A long source is cut short."
  (let* ((flat  (with-output-to-string (stream)
                  (let ((blank-p   nil)
                        (written-p nil))
                    (loop for index from start below end
                          for character = (char text index)
                          do (if (lex--whitespace-p character)
                                 (setf blank-p t)
                                 (progn
                                   (when (and blank-p written-p)
                                     (write-char #\Space stream))
                                   (setf blank-p   nil
                                         written-p t)
                                   (write-char character stream)))))))
         (limit 72))
    (if (> (length flat) limit)
        (format nil "~A…" (subseq flat 0 limit))
        flat)))

(-> proof--operator-key (string) string)
(defun proof--operator-key (operator)
  "Return OPERATOR in lower case without the package prefix."
  (let ((colon (position #\: operator :from-end t)))
    (string-downcase (if colon (subseq operator (1+ colon)) operator))))

(-> proof--settle (definition string list) definition)
(defun proof--settle (definition text children)
  "Set the operator and the name of DEFINITION from CHILDREN, and mark the docstrings of the definition. Return DEFINITION.

CHILDREN are the members of the form in TEXT, in order. Each child is a list
(KIND START END PASSAGE), and PASSAGE is NIL for children that are not strings.
The docstring of a function is the string after the lambda list if more
follows that string. The docstring of a variable is the fourth member of the
form. The docstring of a structure is the third member."
  (flet ((kind (child) (first child))
         (source (child) (subseq text (second child) (third child)))
         (docstring (child)
           (when (and child (fourth child))
             (setf (passage-kind (fourth child)) ':docstring))))
    (let* ((operator  (and children (eq (kind (first children)) ':atom) (source (first children))))
           (key       (if operator (proof--operator-key operator) ""))
           (named     (second children))
           (lambda-at (position ':list children :key #'first :start (min 2 (length children)))))
      (declare (type (or null fixnum) lambda-at))
      (setf (definition-operator definition) (or operator "")
            (definition-name definition)
            (cond
              ((null named)
               "")
              ((and (string= key "defmethod") lambda-at)
               (proof--brief text (second named) (third (nth lambda-at children))))
              (t
               (proof--brief text (second named) (third named)))))
      (cond
        ((member key *function-definers* :test #'string=)
         (when (and lambda-at (< (+ lambda-at 2) (length children)))
           (docstring (nth (1+ lambda-at) children))))
        ((member key *variable-definers* :test #'string=)
         (docstring (nth 3 children)))
        ((string= key "defstruct")
         (docstring (nth 2 children))))))
  definition)

(defstruct (proof-frame (:constructor make-proof-frame ()))
  "A form that is being read. CHILDREN are the members of the form read so far, the newest first. Each child is a list (KIND START END PASSAGE)."
  (children nil :type list))

(-> proof--documentation-follows-p (string proof-frame) boolean)
(defun proof--documentation-follows-p (text frame)
  "Return T if the last member read in the form of FRAME is a keyword that is followed by documentation."
  (let ((previous (first (proof-frame-children frame))))
    (and previous
         (eq (first previous) ':atom)
         (member (subseq text (second previous) (third previous)) *documentation-keywords*
                 :test #'string-equal)
         t)))

(-> proof--definitions (string list) list)
(defun proof--definitions (text lexemes)
  "Return the definitions of TEXT in the order of the file. LEXEMES are the lexemes of TEXT.

Consecutive comment lines that start in the same column and have the same
number of semicolons are one passage. A comment directly above a top-level form
belongs to that form. A comment behind the form on the last line of the form
also belongs to it. Other comments outside forms are collected into definitions
of their own."
  (let ((definitions nil)
        (frames      nil)
        (current     nil)
        (loose       nil)
        (run         nil)
        (run-line    0)
        (run-column  0)
        (run-count   0)
        (run-first   1)
        (run-alone-p t)
        (run-margin  "")
        (closed      nil)
        (closed-line 0)
        (line        1)
        (line-start  0)
        (scanned     0))
    (labels ((advance (position)
               "Count the lines of TEXT up to POSITION."
               (loop for newline = (position #\Newline text :start scanned :end position)
                     while newline
                     do (incf line)
                        (setf line-start (1+ newline)
                              scanned    (1+ newline)))
               (setf scanned (max scanned position)))

             (alone-p (lexeme)
               "Return T if only blanks are before LEXEME on the line."
               (every #'lex--whitespace-p (subseq text line-start (lexeme-start lexeme))))

             (place (passage)
               "Add PASSAGE to the form being read, to the form that ended on the same line, or to the comments outside forms."
               (cond
                 (current
                  (push passage (definition-passages current)))
                 ;; The passage is behind a form, on the last line of the form. A form
                 ;; without words was not kept before, so it is kept now.
                 ((and closed (not (passage-alone-p passage)) (= (passage-line passage) closed-line))
                  (unless (definition-passages closed)
                    (push closed definitions))
                  (setf (definition-passages closed)
                        (append (definition-passages closed) (list passage))))
                 (t
                  (push passage loose))))

             (lines-within (lexeme)
               "Return the number of line ends in LEXEME."
               (count #\Newline text :start (lexeme-start lexeme) :end (lexeme-end lexeme)))

             (end-run ()
               "Make a passage of the comment lines collected so far."
               (when run
                 (place (proof--line-comment text (nreverse run)
                                             :alone-p run-alone-p
                                             :margin  run-margin
                                             :line    run-first))
                 (setf run nil)))

             (end-loose ()
               "Make a definition of the comments collected outside forms."
               (when loose
                 (let ((passages (nreverse loose)))
                   (push (make-definition :line (passage-line (first passages)) :passages passages)
                         definitions))
                 (setf loose nil)))

             (note (kind lexeme &optional passage)
               "Record a member of the form being read."
               (when frames
                 (push (list kind (lexeme-start lexeme) (lexeme-end lexeme) passage)
                       (proof-frame-children (first frames))))))
      (dolist (lexeme lexemes)
        (advance (lexeme-start lexeme))
        (let ((column (- (lexeme-start lexeme) line-start)))
          (ecase (lexeme-kind lexeme)
            (:comment
             (let ((alone-p (alone-p lexeme))
                   (count   (- (or (position #\; text :start (lexeme-start lexeme) :end (lexeme-end lexeme)
                                                      :test #'char/=)
                                   (lexeme-end lexeme))
                               (lexeme-start lexeme))))
               (unless (and run
                            alone-p
                            (= line (1+ run-line))
                            (= column run-column)
                            (= count run-count))
                 (end-run)
                 (setf run-first   line
                       run-column  column
                       run-count   count
                       run-alone-p alone-p
                       run-margin  (if alone-p
                                       (subseq text line-start (lexeme-start lexeme))
                                       (make-string column :initial-element #\Space))))
               (push lexeme run)
               (setf run-line line)))
            (:block
             (end-run)
             (place (make-passage :kind      ':comment
                                  :shape     ':block
                                  :start     (lexeme-start lexeme)
                                  :end       (lexeme-end lexeme)
                                  :line      line
                                  :last-line (+ line (lines-within lexeme))
                                  :alone-p   (alone-p lexeme)
                                  :text      (subseq text (+ (lexeme-start lexeme) 2) (- (lexeme-end lexeme) 2)))))
            (:string
             (end-run)
             (let ((passage (make-passage
                             :kind      (if (and frames (proof--documentation-follows-p text (first frames)))
                                            ':docstring
                                            ':string)
                             :shape     ':string
                             :start     (lexeme-start lexeme)
                             :end       (lexeme-end lexeme)
                             :line      line
                             :last-line (+ line (lines-within lexeme))
                             :alone-p   (alone-p lexeme)
                             :text      (string-literal-text
                                         (subseq text (lexeme-start lexeme) (lexeme-end lexeme))))))
               (note ':string lexeme passage)
               (place passage)))
            (:open
             (end-run)
             (if frames
                 (note ':list lexeme)
                 ;; A form starts here. The comment that ends on the line above
                 ;; belongs to the form.
                 (let ((above (and loose
                                   (passage-alone-p (first loose))
                                   (= (passage-last-line (first loose)) (1- line))
                                   (pop loose))))
                   (end-loose)
                   (setf current (make-definition :line line :passages (and above (list above)))
                         closed  nil)))
             (push (make-proof-frame) frames))
            (:close
             (end-run)
             (when frames
               (let ((frame (pop frames)))
                 (cond
                   (frames
                    ;; The list was noted when it opened. The end of the list is known now.
                    (let ((member (first (proof-frame-children (first frames)))))
                      (when (and member (eq (first member) ':list))
                        (setf (third member) (lexeme-end lexeme)))))
                   (t
                    (setf (definition-passages current) (nreverse (definition-passages current)))
                    (proof--settle current text (reverse (proof-frame-children frame)))
                    (when (definition-passages current)
                      (push current definitions))
                    (setf closed      current
                          closed-line line
                          current     nil))))))
            (:atom
             (end-run)
             (note ':atom lexeme))
            (:prefix
             (end-run)))))
      (end-run)
      ;; The words read in a form that does not close are also kept.
      (when current
        (setf (definition-passages current) (nreverse (definition-passages current)))
        (when frames
          (proof--settle current text (reverse (proof-frame-children (first (last frames))))))
        (when (definition-passages current)
          (push current definitions)))
      (end-loose))
    (nreverse definitions)))

(-> proof--skeleton (string list) string)
(defun proof--skeleton (text lexemes)
  "Return the code of TEXT without the words. The skeleton contains all LEXEMES except comments, and each string is replaced by one quote.

If two texts have the same skeleton, they only differ in comments and in the
content of strings."
  (with-output-to-string (stream)
    (dolist (lexeme lexemes)
      (case (lexeme-kind lexeme)
        ((:comment :block)
         nil)
        (:string
         (write-char #\" stream))
        (otherwise
         (write-string text stream :start (lexeme-start lexeme) :end (lexeme-end lexeme))
         (write-char #\Space stream))))))

(-> definition-digest (definition) string)
(defun definition-digest (definition)
  "Return the digest of the words of DEFINITION. The digest is only computed from the operator, the name, and the kind and the words of each passage."
  (octets-digest
   (sb-ext:string-to-octets
    (with-output-to-string (stream)
      (format stream "~A~C~A~C" (definition-operator definition) (code-char 0) (definition-name definition) (code-char 0))
      (dolist (passage (definition-passages definition))
        (format stream "~(~A~)~C~A~C" (passage-kind passage) (code-char 1) (passage-text passage) (code-char 0))))
    :external-format ':utf-8)))

(-> proof-from-text (string &key (:pathname t) (:digest string)) proof)
(defun proof-from-text (text &key pathname (digest ""))
  "Return the proof of the source TEXT. PATHNAME is the file that contains TEXT, and DIGEST is the digest of the file. If the words of TEXT cannot be read, the proof has a problem."
  (handler-case
      (let ((lexemes (lisp-lex text)))
        (make-proof :pathname    pathname
                    :text        text
                    :digest      digest
                    :definitions (proof--definitions text lexemes)
                    :skeleton    (proof--skeleton text lexemes)))
    (proof-error (condition)
      (make-proof :pathname pathname
                  :text     text
                  :digest   digest
                  :problem  (proof-error-message condition)))))

(-> proof-read (pathname) proof)
(defun proof-read (pathname)
  "Return the proof of the source file at PATHNAME.

If the file is not UTF-8, or if the lines end with a carriage return and a line
feed, the proof has a problem. If such a file were written back, more than the
words would change."
  (handler-case
      (let* ((octets (file-octets pathname))
             (digest (octets-digest octets))
             (text   (sb-ext:octets-to-string octets :external-format ':utf-8)))
        (cond
          ((mismatch octets (sb-ext:string-to-octets text :external-format ':utf-8))
           (make-proof :pathname pathname :digest digest :problem "the file is not written in UTF-8"))
          ((find #\Return text)
           (make-proof :pathname pathname :text text :digest digest
                       :problem "the lines of the file end with a carriage return and a line feed"))
          (t
           (proof-from-text text :pathname pathname :digest digest))))
    (file-error (condition)
      (make-proof :pathname pathname :problem (format nil "the file cannot be read: ~A" condition)))
    ;; The octets are not text.
    (error ()
      (make-proof :pathname pathname :problem "the file is not written in UTF-8"))))

(-> proof-passage (proof fixnum fixnum) (or null passage))
(defun proof-passage (proof definition passage)
  "Return the passage numbered PASSAGE in the definition numbered DEFINITION of PROOF, or NIL if there is none. Both numbers count from 0."
  (let ((found (nth definition (proof-definitions proof))))
    (and found (nth passage (definition-passages found)))))

(-> proof-passage-count (proof) fixnum)
(defun proof-passage-count (proof)
  "Return the number of passages in PROOF."
  (reduce #'+ (proof-definitions proof) :key (lambda (definition) (length (definition-passages definition)))))


;;;; -- Changing Words --

(-> passage-clean-text (passage string) string)
(defun passage-clean-text (passage text)
  "Return TEXT as PASSAGE would contain it after a write and a new read. Carriage returns are removed. In a line comment, the blanks at the end of each line are also removed."
  (if (eq (passage-shape passage) ':line)
      (format nil "~{~A~^~%~}"
              (mapcar (lambda (line)
                        (string-right-trim '(#\Space #\Tab) line))
                      (text-lines text)))
      (remove #\Return text)))

(-> proof--replace (proof fixnum fixnum &key (:source string)) proof)
(defun proof--replace (proof start end &key (source ""))
  "Return the proof of the text of PROOF in which the part between START and END is replaced by SOURCE.

Signal a PROOF-ERROR if the new text cannot be read, or if the code of the new
text differs from the code of PROOF."
  (let* ((old  (proof-text proof))
         (text (concatenate 'string (subseq old 0 start) source (subseq old end)))
         (new  (proof-from-text text :pathname (proof-pathname proof) :digest (proof-digest proof))))
    (when (proof-problem new)
      (error 'proof-error :message (format nil "not written: ~A" (proof-problem new))))
    (unless (string= (proof-skeleton new) (proof-skeleton proof))
      (error 'proof-error :message "not written: the edit would change the code as well as the words"))
    new))

(-> proof-revise (proof passage string) proof)
(defun proof-revise (proof passage text)
  "Return a new proof of the file of PROOF, in which the words of PASSAGE are replaced by TEXT. PROOF is not modified, and nothing is written to the file.

The new words are written in the same way as the passage. Signal a PROOF-ERROR
if they cannot be written that way, or if the edit would change more than the
passage."
  (let* ((source (passage-source passage text))
         (new    (proof--replace proof (passage-start passage) (passage-end passage) :source source))
         (found  (find (passage-start passage)
                       (loop for definition in (proof-definitions new)
                             append (definition-passages definition))
                       :key #'passage-start)))
    (unless (and found
                 (= (- (passage-end found) (passage-start found)) (length source))
                 (eq (passage-shape found) (passage-shape passage)))
      (error 'proof-error :message "not written: the edit would change more than this text"))
    new))

(-> proof-remove (proof passage) (values proof integer))
(defun proof-remove (proof passage)
  "Return a new proof of the file of PROOF without the comment PASSAGE, and the position in the text where the comment was removed. PROOF is not modified, and nothing is written to the file.

A comment that is alone on its lines is removed together with those lines. A
comment behind code is removed together with the blanks before it. Signal a
PROOF-ERROR if PASSAGE is a string, because strings are part of the code."
  (when (eq (passage-shape passage) ':string)
    (error 'proof-error :message "a string cannot be removed, only emptied"))
  (let* ((text  (proof-text proof))
         (start (passage-start passage))
         (end   (passage-end passage)))
    (if (passage-alone-p passage)
        ;; Remove whole lines, from the start of the first line to the start
        ;; of the line after the last.
        (let ((line-start (let ((newline (position #\Newline text :end start :from-end t)))
                            (if newline (1+ newline) 0)))
              (line-end   (let ((newline (position #\Newline text :start end)))
                            (if newline (1+ newline) (length text)))))
          (if (every #'lex--whitespace-p (subseq text end line-end))
              (values (proof--replace proof line-start line-end) line-start)
              (values (proof--replace proof start end) start)))
        (let ((blank-start (or (position-if-not (lambda (character)
                                                  (member character '(#\Space #\Tab)))
                                                text :end start :from-end t)
                               -1)))
          (values (proof--replace proof (1+ blank-start) end) (1+ blank-start))))))

(-> proof-write (proof proof) proof)
(defun proof-write (new old)
  "Write the text of the proof NEW to the file, and return NEW with the digest of the new file content.

The text is first written to a temporary file next to the file. The temporary
file gets the permissions of the file and is then renamed over the file. Signal
a PROOF-ERROR with the reason :CHANGED if the file content differs from the
content that OLD was read from."
  (let* ((pathname (proof-pathname old))
         (target   (truename pathname))
         (octets   (sb-ext:string-to-octets (proof-text new) :external-format ':utf-8))
         (beside   (make-pathname :name     (format nil ".~A.unslop-~D" (pathname-name target) (random 1000000))
                                  :defaults target)))
    (unless (equal (file-digest target) (proof-digest old))
      (error 'proof-error
             :reason  ':changed
             :message "not written: the file changed on disk since it was read"))
    (unwind-protect
         (progn
           (with-open-file (stream beside :direction ':output :element-type '(unsigned-byte 8)
                                          :if-exists ':supersede)
             (write-sequence octets stream))
           (sb-posix:chmod (uiop:native-namestring beside)
                           (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring target)))))
           (sb-posix:rename (uiop:native-namestring beside) (uiop:native-namestring target)))
      (when (probe-file beside)
        (handler-case (delete-file beside)
          (file-error ()
            nil))))
    (let ((written (copy-proof new)))
      (setf (proof-digest written) (octets-digest octets))
      written)))
