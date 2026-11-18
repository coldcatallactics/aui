(in-package #:aui)

;;;; -- Proof --

;;; One source file read for its words. A proof holds the file's text and,
;;; in the order of the file, its definitions: each top-level form that has
;;; words in it, and each run of comments that stands outside any form. A
;;; definition holds passages, and a passage is one stretch of words that a
;;; person reads: a comment, a docstring or any other string.
;;;
;;; A passage knows where it stands in the text and how it is written there,
;;; so that words changed by a person go back in the same place, behind the
;;; same semicolons or between the same quotes.

(defstruct passage
  "One stretch of words in a source file, with what it is, how it is written, where its source starts and ends, the words themselves and the lines they start and end on. A comment also keeps its semicolons, what separates them from the words, what stands before the semicolons on its later lines, and whether it stands alone on its lines."
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
  "One top-level form with words in it, as its OPERATOR, its NAME, the LINE it starts on and its PASSAGES in the order of the file. A run of comments outside any form is a definition with an empty operator."
  (operator ""  :type string)
  (name     ""  :type string)
  (line     1   :type fixnum)
  (passages nil :type list))

(defstruct proof
  "One source file read for its words, as its PATHNAME, its TEXT, the DIGEST of what the file held, its DEFINITIONS and its SKELETON, which is its code without its words. A file that cannot be read this way has a PROBLEM that says why and no definitions."
  (pathname    nil)
  (text        ""  :type string)
  (digest      ""  :type string)
  (definitions nil :type list)
  (skeleton    ""  :type string)
  (problem     nil :type (or null string)))

(defparameter *documentation-keywords* '(":documentation" ":description" ":long-description")
  "The keywords after which a string is documentation, in a class, a condition, a package or a system.")

(defparameter *function-definers* '("defun" "defmacro" "defmethod" "deftype" "define-compiler-macro" "defsetf"
                                    "define-setf-expander")
  "The operators whose string after the lambda list is a docstring.")

(defparameter *variable-definers* '("defvar" "defparameter" "defconstant" "defglobal" "define-symbol-macro")
  "The operators whose string after the value is a docstring.")


;;;; -- Digest --

(-> octets-digest ((simple-array (unsigned-byte 8) (*))) string)
(defun octets-digest (octets)
  "Return a short digest of OCTETS that differs, as good as always, when the octets do.

It only has to tell whether a file changed, so it is the FNV-1a sum of the
octets with their count, not a cryptographic one."
  (let ((sum #xcbf29ce484222325))
    (declare (type (unsigned-byte 64) sum))
    (loop for octet across octets
          do (setf sum (logand #xFFFFFFFFFFFFFFFF
                               (* (logxor sum octet) #x100000001b3))))
    (format nil "~16,'0X-~D" sum (length octets))))

(-> file-octets (pathname) (simple-array (unsigned-byte 8) (*)))
(defun file-octets (pathname)
  "Return what the file at PATHNAME holds, as octets."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(-> file-digest (pathname) (or null string))
(defun file-digest (pathname)
  "Return the digest of what the file at PATHNAME holds, or NIL when it cannot be read."
  (handler-case (octets-digest (file-octets pathname))
    (error ()
      nil)))


;;;; -- Words as They Are Written --

(-> string-literal-text (string) string)
(defun string-literal-text (literal)
  "Return the words of the string LITERAL, which is written with its quotes: what stands between them, each backslash giving way to the character after it."
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
  "Return TEXT written as a string in Lisp source: between quotes, with a backslash before each quote and each backslash."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across text
          do (when (or (char= character #\") (char= character #\\))
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(-> text-lines (string) list)
(defun text-lines (text)
  "Return the lines of TEXT without their line ends. A text that ends with a line end has an empty last line."
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start end))
        while end
        do (setf start (1+ end))))

(-> passage-source (passage string) string)
(defun passage-source (passage text)
  "Return TEXT written the way PASSAGE is written in its file: behind its semicolons line by line, between its bars, or between quotes.

A comment line loses the blanks at its end, and an empty one is its
semicolons alone. Signal a PROOF-ERROR when a block comment's words would
end the comment."
  (ecase (passage-shape passage)
    (:string
     (string-literal (remove #\Return text)))
    (:block
     (when (or (search "|#" text) (search "#|" text))
       (error 'proof-error
              :message "a block comment cannot hold the two characters that open or close one"))
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
  "Return the passage of the comment lines LEXEMES of TEXT, which follow one another and are written alike."
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
  "Return the source of TEXT between START and END on one line, its blank runs as one blank each, cut short when long."
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
  "Return OPERATOR in lower case without the package it is written with."
  (let ((colon (position #\: operator :from-end t)))
    (string-downcase (if colon (subseq operator (1+ colon)) operator))))

(-> proof--settle (definition string list) definition)
(defun proof--settle (definition text children)
  "Name DEFINITION from the CHILDREN of its form in TEXT and mark its docstrings, then return it.

CHILDREN are the form's own members in order, each as (KIND START END
PASSAGE), the passage NIL for anything but a string. A function's docstring
is the string after its lambda list when something follows it. A variable's
is its fourth member, and a structure's its third."
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
  "What is known of a form that is being read, as its CHILDREN so far, newest first, each (KIND START END PASSAGE)."
  (children nil :type list))

(-> proof--documentation-follows-p (string proof-frame) boolean)
(defun proof--documentation-follows-p (text frame)
  "Return T when the member of FRAME's form read last is a keyword after which a string is documentation."
  (let ((previous (first (proof-frame-children frame))))
    (and previous
         (eq (first previous) ':atom)
         (member (subseq text (second previous) (third previous)) *documentation-keywords*
                 :test #'string-equal)
         t)))

(-> proof--definitions (string list) list)
(defun proof--definitions (text lexemes)
  "Return the definitions of TEXT, whose lexemes are LEXEMES, in the order of the file.

Comment lines that follow one another, start in the same column and have as
many semicolons are one passage. A comment directly above a top-level form
belongs to that form, and so does one behind it on its last line. Other
comments outside any form are gathered into definitions of their own."
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
               "Whether nothing but blanks stands before LEXEME on its line."
               (every #'lex--whitespace-p (subseq text line-start (lexeme-start lexeme))))

             (place (passage)
               "Put PASSAGE where it belongs: in the form being read, or among the comments outside any."
               (cond
                 (current
                  (push passage (definition-passages current)))
                 ;; Behind a form, on the line it ends on. A form without words was not kept until now.
                 ((and closed (not (passage-alone-p passage)) (= (passage-line passage) closed-line))
                  (unless (definition-passages closed)
                    (push closed definitions))
                  (setf (definition-passages closed)
                        (append (definition-passages closed) (list passage))))
                 (t
                  (push passage loose))))

             (lines-within (lexeme)
               "How many line ends LEXEME holds."
               (count #\Newline text :start (lexeme-start lexeme) :end (lexeme-end lexeme)))

             (end-run ()
               "Make a passage of the comment lines gathered so far."
               (when run
                 (place (proof--line-comment text (nreverse run)
                                             :alone-p run-alone-p
                                             :margin  run-margin
                                             :line    run-first))
                 (setf run nil)))

             (end-loose ()
               "Make a definition of the comments gathered outside any form."
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
                 ;; A form begins. The comment that ends on the line above is its own.
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
                    ;; The list this closes was noted when it opened; now its end is known.
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
      ;; A form that never closes still gives up the words read in it.
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
  "Return the code of TEXT without its words: every lexeme of LEXEMES but the comments, each string as a quote alone.

Two texts with one skeleton differ in their comments and in what their
strings say, and in nothing a compiler reads otherwise."
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

(-> proof-from-text (string &key (:pathname t) (:digest string)) proof)
(defun proof-from-text (text &key pathname (digest ""))
  "Return the proof of the source TEXT, which the file at PATHNAME holds with DIGEST. A text that cannot be read for its words gives a proof with a problem."
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

A file that is not UTF-8, or that ends its lines the way DOS does, gives a
proof with a problem: writing it back would change more than words."
  (handler-case
      (let* ((octets (file-octets pathname))
             (digest (octets-digest octets))
             (text   (sb-ext:octets-to-string octets :external-format ':utf-8)))
        (cond
          ((mismatch octets (sb-ext:string-to-octets text :external-format ':utf-8))
           (make-proof :pathname pathname :digest digest :problem "the file is not written in UTF-8"))
          ((find #\Return text)
           (make-proof :pathname pathname :text text :digest digest
                       :problem "the file ends its lines with a carriage return and a line feed"))
          (t
           (proof-from-text text :pathname pathname :digest digest))))
    (file-error (condition)
      (make-proof :pathname pathname :problem (format nil "the file cannot be read: ~A" condition)))
    ;; Octets that are no text at all.
    (error ()
      (make-proof :pathname pathname :problem "the file is not written in UTF-8"))))

(-> proof-passage (proof fixnum fixnum) (or null passage))
(defun proof-passage (proof definition passage)
  "Return the passage numbered PASSAGE in the definition numbered DEFINITION of PROOF, both counted from 0, or NIL when there is none."
  (let ((found (nth definition (proof-definitions proof))))
    (and found (nth passage (definition-passages found)))))

(-> proof-passage-count (proof) fixnum)
(defun proof-passage-count (proof)
  "Return how many passages PROOF has."
  (reduce #'+ (proof-definitions proof) :key (lambda (definition) (length (definition-passages definition)))))


;;;; -- Changing Words --

(-> passage-clean-text (passage string) string)
(defun passage-clean-text (passage text)
  "Return TEXT as PASSAGE would hold it once written and read again: without carriage returns, and for a comment without the blanks that end its lines."
  (if (eq (passage-shape passage) ':line)
      (format nil "~{~A~^~%~}"
              (mapcar (lambda (line)
                        (string-right-trim '(#\Space #\Tab) line))
                      (text-lines text)))
      (remove #\Return text)))

(-> proof--replace (proof fixnum fixnum &key (:source string)) proof)
(defun proof--replace (proof start end &key (source ""))
  "Return the proof of PROOF's text with what stands between START and END replaced by SOURCE.

Signal a PROOF-ERROR when the result does not read, or when its code differs
from the code of PROOF. Words are all that may change."
  (let* ((old  (proof-text proof))
         (text (concatenate 'string (subseq old 0 start) source (subseq old end)))
         (new  (proof-from-text text :pathname (proof-pathname proof) :digest (proof-digest proof))))
    (when (proof-problem new)
      (error 'proof-error :message (format nil "not written: ~A" (proof-problem new))))
    (unless (string= (proof-skeleton new) (proof-skeleton proof))
      (error 'proof-error :message "not written: the change would alter the code, not only its words"))
    new))

(-> proof-revise (proof passage string) proof)
(defun proof-revise (proof passage text)
  "Return the proof of PROOF's file with the words of PASSAGE changed to TEXT. PROOF itself stays as it was, and nothing is written.

The new words are written the way the passage is written. Signal a
PROOF-ERROR when they cannot be, or when the change would reach beyond the
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
      (error 'proof-error :message "not written: the change would reach beyond this text"))
    new))

(-> proof-remove (proof passage) proof)
(defun proof-remove (proof passage)
  "Return the proof of PROOF's file without the comment PASSAGE. PROOF itself stays as it was, and nothing is written.

A comment that stands alone on its lines goes with those lines. One behind
code goes with the blanks before it. A string is never removed, since its
form needs it. Signal a PROOF-ERROR for one."
  (when (eq (passage-shape passage) ':string)
    (error 'proof-error :message "a string cannot be removed, only emptied"))
  (let* ((text  (proof-text proof))
         (start (passage-start passage))
         (end   (passage-end passage)))
    (if (passage-alone-p passage)
        ;; Whole lines: from the start of the first to the start of the one after the last.
        (let ((line-start (let ((newline (position #\Newline text :end start :from-end t)))
                            (if newline (1+ newline) 0)))
              (line-end   (let ((newline (position #\Newline text :start end)))
                            (if newline (1+ newline) (length text)))))
          (if (every #'lex--whitespace-p (subseq text end line-end))
              (proof--replace proof line-start line-end)
              (proof--replace proof start end)))
        (let ((blank-start (or (position-if-not (lambda (character)
                                                  (member character '(#\Space #\Tab)))
                                                text :end start :from-end t)
                               -1)))
          (proof--replace proof (1+ blank-start) end)))))

(-> proof-write (proof proof) proof)
(defun proof-write (new old)
  "Write the text of the proof NEW to its file, which must still hold what the proof OLD was read from, and return NEW with the digest of what the file holds now.

The text is written beside the file and moved over it, so the file is whole
at every moment, and it keeps its permissions. Signal a PROOF-ERROR whose
reason is :CHANGED when the file no longer holds what OLD was read from."
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
