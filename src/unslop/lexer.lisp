(in-package #:aui)

;;;; -- Lexer --

;;; A lexer for Lisp source. The source is cut into the pieces that matter for an
;;; edit of its words: parentheses, strings, comments, and atoms for everything else.
;;; Only characters are looked at. No symbols are interned, so files that a Lisp
;;; reader would refuse can be read too. Each piece has a start and an end in the
;;; text, so that an edited string or comment can be put back in place.

(define-condition proof-error (error)
  ((message
    :initarg       :message
    :reader        proof-error-message
    :type          string
    :documentation "A description of the problem that can be shown to a person.")
   (reason
    :initarg       :reason
    :initform      ':refused
    :reader        proof-error-reason
    :type          keyword
    :documentation "The kind of the problem: :UNREADABLE for a file that cannot be read as Lisp, :REFUSED for an edit that is not accepted, :CHANGED for a file that changed on disk."))
  (:report (lambda (condition stream)
             (write-string (proof-error-message condition) stream)))
  (:documentation "The error signalled if the texts of a source file cannot be read, or if an edit to the file cannot be written."))

(defstruct lexeme
  "One piece of Lisp source. KIND is the kind of the piece. START and END are the positions where the piece starts and ends in the text. The kinds are the keywords OPEN, CLOSE, STRING, COMMENT for a comment to the end of a line, BLOCK for a comment between bars, PREFIX for a quote and similar marks, and ATOM."
  (kind  ':atom :type keyword)
  (start 0      :type fixnum)
  (end   0      :type fixnum))

(-> lex--whitespace-p (character) boolean)
(defun lex--whitespace-p (character)
  "Return T if CHARACTER is whitespace between pieces of Lisp source."
  (and (or (member character '(#\Space #\Newline #\Tab #\Return #\Page #\Rubout))
           ;; The byte order mark, which some editors put at the start of a file.
           (= (char-code character) #xFEFF))
       t))

(-> lex--terminating-p (character) boolean)
(defun lex--terminating-p (character)
  "Return T if CHARACTER ends an atom."
  (and (member character '(#\( #\) #\' #\" #\; #\` #\,)) t))

(-> lex--atom-end (simple-string fixnum) fixnum)
(defun lex--atom-end (text start)
  "Return the end of the atom in TEXT that starts at START.

The character after a backslash belongs to the atom. Everything between two bars
also belongs to the atom."
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
  "Return the end of the string in TEXT whose opening quote is at START. The end is the position after the closing quote. Signal a PROOF-ERROR if the string does not close."
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
  "Return the end of the block comment in TEXT that opens at START. The end is the position after the closing bar and sharp sign. Block comments can be nested. Signal a PROOF-ERROR if the comment does not close."
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
  "Return the lexeme in TEXT that starts with the sharp sign at START."
  (let* ((length (length text))
         (next   (and (< (1+ start) length) (schar text (1+ start)))))
    (cond
      ((null next)
       (make-lexeme :kind ':atom :start start :end length))
      ((char= next #\|)
       (make-lexeme :kind ':block :start start :end (lex--block-end text start)))
      ;; A character literal contains the character after the backslash and the rest of the name.
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
  "Return the lexemes of the Lisp SOURCE in order. Signal a PROOF-ERROR if a string or a block comment does not close."
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
