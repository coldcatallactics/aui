(in-package #:aui)

;;;; -- Lexer --

;;; Lisp source cut into the pieces that matter to someone who edits its
;;; words: parentheses, strings, comments, and everything else as atoms. The
;;; lexer looks at characters only. It interns nothing and needs no package
;;; to exist, so it takes any file a Lisp reader would take and many it would
;;; refuse. Each piece says where it starts and ends in the text, which is
;;; what lets an edited string or comment be put back in place.

(define-condition proof-error (error)
  ((message
    :initarg       :message
    :reader        proof-error-message
    :type          string
    :documentation "What is wrong, in words fit to show.")
   (reason
    :initarg       :reason
    :initform      ':refused
    :reader        proof-error-reason
    :type          keyword
    :documentation "What kind of trouble it is: :UNREADABLE for a file that cannot be read as Lisp, :REFUSED for an edit that is not taken, :CHANGED for a file that changed on disk."))
  (:report (lambda (condition stream)
             (write-string (proof-error-message condition) stream)))
  (:documentation "A source file that cannot be read for its texts, or an edit to one that is not written."))

(defstruct lexeme
  "One piece of Lisp source, as its KIND and where its characters START and END in the text. A kind is one of the keywords OPEN, CLOSE, STRING, COMMENT for a comment to the end of a line, BLOCK for a comment between bars, PREFIX for a quote and its like, and ATOM."
  (kind  ':atom :type keyword)
  (start 0      :type fixnum)
  (end   0      :type fixnum))

(-> lex--whitespace-p (character) boolean)
(defun lex--whitespace-p (character)
  "Return T when CHARACTER separates pieces of Lisp source and is nothing itself."
  (and (or (member character '(#\Space #\Newline #\Tab #\Return #\Page #\Rubout))
           ;; The mark some editors open a file with.
           (= (char-code character) #xFEFF))
       t))

(-> lex--terminating-p (character) boolean)
(defun lex--terminating-p (character)
  "Return T when CHARACTER ends an atom that runs up to it."
  (and (member character '(#\( #\) #\' #\" #\; #\` #\,)) t))

(-> lex--atom-end (simple-string fixnum) fixnum)
(defun lex--atom-end (text start)
  "Return where the atom of TEXT that begins at START ends.

A backslash takes the character after it into the atom, and so do two bars
whatever stands between them."
  (let ((length (length text))
        (index  start))
    (declare (type fixnum length index))
    (loop while (< index length)
          do (let ((character (schar text index)))
               (cond
                 ((char= character #\\)
                  (setf index (min length (+ index 2))))
                 ((char= character #\|)
                  (incf index)
                  (loop while (and (< index length) (char/= (schar text index) #\|))
                        do (incf index (if (char= (schar text index) #\\) 2 1)))
                  (setf index (min length (1+ index))))
                 ((or (lex--whitespace-p character) (lex--terminating-p character))
                  (return))
                 (t
                  (incf index)))))
    (min index length)))

(-> lex--string-end (simple-string fixnum) fixnum)
(defun lex--string-end (text start)
  "Return where the string of TEXT whose opening quote is at START ends, after its closing quote. Signal a PROOF-ERROR when it never closes."
  (let ((length (length text))
        (index  (1+ start)))
    (declare (type fixnum length index))
    (loop
      (when (>= index length)
        (error 'proof-error
               :reason  ':unreadable
               :message (format nil "a string that opens at character ~D never closes" start)))
      (let ((character (schar text index)))
        (cond
          ((char= character #\\)
           (incf index 2))
          ((char= character #\")
           (return (1+ index)))
          (t
           (incf index)))))))

(-> lex--block-end (simple-string fixnum) fixnum)
(defun lex--block-end (text start)
  "Return where the block comment of TEXT that opens at START ends, after the bar and sharp that close it. Block comments nest. Signal a PROOF-ERROR when it never closes."
  (let ((length (length text))
        (index  (+ start 2))
        (depth  1))
    (declare (type fixnum length index depth))
    (loop
      (when (>= (1+ index) length)
        (error 'proof-error
               :reason  ':unreadable
               :message (format nil "a block comment that opens at character ~D never closes" start)))
      (let ((character (schar text index))
            (next      (schar text (1+ index))))
        (cond
          ((and (char= character #\|) (char= next #\#))
           (incf index 2)
           (when (zerop (decf depth))
             (return index)))
          ((and (char= character #\#) (char= next #\|))
           (incf index 2)
           (incf depth))
          (t
           (incf index)))))))

(-> lex--sharp (simple-string fixnum) lexeme)
(defun lex--sharp (text start)
  "Return the lexeme of TEXT that begins with the sharp sign at START."
  (let* ((length (length text))
         (next   (and (< (1+ start) length) (schar text (1+ start)))))
    (cond
      ((null next)
       (make-lexeme :kind ':atom :start start :end length))
      ((char= next #\|)
       (make-lexeme :kind ':block :start start :end (lex--block-end text start)))
      ;; A character: the one after the backslash whatever it is, then the rest of its name.
      ((char= next #\\)
       (make-lexeme :kind  ':atom
                    :start start
                    :end   (if (< (+ start 3) length)
                               (lex--atom-end text (+ start 3))
                               length)))
      ((char= next #\()
       (make-lexeme :kind ':open :start start :end (+ start 2)))
      ((member next '(#\' #\+ #\- #\. #\,))
       (make-lexeme :kind ':prefix :start start :end (+ start 2)))
      (t
       (let ((after (or (position-if-not #'digit-char-p text :start (1+ start)) length)))
         (cond
           ((and (< after length) (char= (schar text after) #\=) (> after (1+ start)))
            (make-lexeme :kind ':prefix :start start :end (1+ after)))
           ((and (< after length) (char= (schar text after) #\#) (> after (1+ start)))
            (make-lexeme :kind ':atom :start start :end (1+ after)))
           (t
            (make-lexeme :kind ':atom :start start :end (max (1+ start) (lex--atom-end text (1+ start)))))))))))

(-> lisp-lex (string) list)
(defun lisp-lex (source)
  "Return the lexemes of the Lisp SOURCE in order. Signal a PROOF-ERROR for a string or a block comment that never closes."
  (let* ((text    (coerce source 'simple-string))
         (length  (length text))
         (index   0)
         (lexemes nil))
    (declare (type fixnum length index))
    (flet ((take (kind end)
             (push (make-lexeme :kind kind :start index :end end) lexemes)
             (setf index end)))
      (loop while (< index length)
            do (let ((character (schar text index)))
                 (cond
                   ((lex--whitespace-p character)
                    (incf index))
                   ((char= character #\()
                    (take ':open (1+ index)))
                   ((char= character #\))
                    (take ':close (1+ index)))
                   ((char= character #\")
                    (take ':string (lex--string-end text index)))
                   ((char= character #\;)
                    (let* ((newline (or (position #\Newline text :start index) length))
                           (end     (if (and (> newline index) (char= (schar text (1- newline)) #\Return))
                                        (1- newline)
                                        newline)))
                      (take ':comment end)
                      (setf index newline)))
                   ((char= character #\,)
                    (take ':prefix (if (and (< (1+ index) length)
                                            (member (schar text (1+ index)) '(#\@ #\.)))
                                       (+ index 2)
                                       (1+ index))))
                   ((member character '(#\' #\`))
                    (take ':prefix (1+ index)))
                   ((char= character #\#)
                    (let ((lexeme (lex--sharp text index)))
                      (push lexeme lexemes)
                      (setf index (lexeme-end lexeme))))
                   (t
                    (take ':atom (max (1+ index) (lex--atom-end text index))))))))
    (nreverse lexemes)))
