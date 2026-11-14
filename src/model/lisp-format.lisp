(in-package #:aui)

;;;; -- Lisp Formatting --

;;; A form that arrives on one long line is hard to read. This sets such a
;;; form out on several lines, indented as Lisp is. It works on the text
;;; itself: every atom keeps the spelling it came with, so nothing the reader
;;; would resolve, drop or rename changes. Source that already has line
;;; breaks, or holds a comment, is left exactly as it came.

(defparameter *lisp-format-width* 86
  "The line width a reformatted form is set out to.")

(defparameter *lisp-format-minimum* 72
  "How long a one-line form is before it is worth setting out on several lines.")

(defparameter *lisp-format-body-heads*
  '(("defun" . 2) ("defmacro" . 2) ("defmethod" . 2) ("defgeneric" . 2) ("defclass" . 2)
    ("defstruct" . 1) ("defvar" . 1) ("defparameter" . 1) ("define-condition" . 2) ("lambda" . 1)
    ("let" . 1) ("let*" . 1) ("flet" . 1) ("labels" . 1) ("macrolet" . 1) ("when" . 1)
    ("unless" . 1) ("dolist" . 1) ("dotimes" . 1) ("progn" . 0) ("block" . 1)
    ("handler-case" . 1) ("handler-bind" . 1) ("unwind-protect" . 1) ("multiple-value-bind" . 2)
    ("destructuring-bind" . 2) ("case" . 1) ("ecase" . 1) ("typecase" . 1) ("etypecase" . 1)
    ("with-open-file" . 1) ("with-output-to-string" . 1) ("with-input-from-string" . 1)
    ("eval-when" . 1) ("loop" . 0))
  "Operators whose arguments after the first few are a body, indented by two, with how many arguments stay on the operator's line.")

(define-condition lisp-format-incomplete (error)
  ()
  (:report "The text ends inside a form.")
  (:documentation "Text to set out as Lisp ended before its forms were closed, so it is left as it came."))

(-> lisp-format--parse (string integer) (values t integer))
(defun lisp-format--parse (text start)
  "Return the node that begins at START in TEXT and the position after it.

A node is a string, an atom spelled as it came with any quoting prefix, or
(:LIST PREFIX . NODES). Signals LISP-FORMAT-INCOMPLETE when TEXT ends inside a
form."
  (let ((position start)
        (length   (length text)))
    (flet ((skip-space ()
             (loop while (and (< position length)
                              (member (char text position) '(#\Space #\Tab #\Newline #\Return)))
                   do (incf position))))
      (skip-space)
      (when (>= position length)
        (error 'lisp-format-incomplete))
      ;; Quote, backquote, comma, comma-at and a sharp-sign dispatch belong to
      ;; the form they stand before: 'x, `(a ,b), #'f, #(1 2), #p"file".
      (let ((prefix-start position))
        (loop while (and (< position length) (find (char text position) "'`,@"))
              do (incf position))
        (when (and (< (1+ position) length)
                   (char= (char text position) #\#)
                   (char/= (char text (1+ position)) #\\))
          (let ((end (position-if (lambda (character)
                                    (member character '(#\Space #\Tab #\Newline #\Return #\( #\) #\")))
                                  text :start position)))
            (when (and end (find (char text end) "(\""))
              (setf position end))))
        (let ((prefix (subseq text prefix-start position)))
          (cond
            ((>= position length)
             (error 'lisp-format-incomplete))
            ((char= (char text position) #\()
             (incf position)
             (let ((children nil))
               (loop
                 (skip-space)
                 (when (>= position length)
                   (error 'lisp-format-incomplete))
                 (when (char= (char text position) #\))
                   (incf position)
                   (return))
                 (multiple-value-bind (child after) (lisp-format--parse text position)
                   (push child children)
                   (setf position after)))
               (values (list* ':list prefix (nreverse children)) position)))
            ((char= (char text position) #\")
             (let ((end (loop for index from (1+ position) below length
                              do (cond
                                   ((char= (char text index) #\\)
                                    (incf index))
                                   ((char= (char text index) #\")
                                    (return (1+ index))))
                              finally (error 'lisp-format-incomplete))))
               (values (concatenate 'string prefix (subseq text position end)) end)))
            (t
             (let ((end position))
               ;; A character literal takes the character after the backslash whatever it is.
               (when (and (< (+ end 2) length)
                          (char= (char text end) #\#)
                          (char= (char text (1+ end)) #\\))
                 (setf end (+ end 3)))
               (loop while (and (< end length)
                                (not (member (char text end) '(#\Space #\Tab #\Newline #\Return #\( #\) #\"))))
                     do (incf end))
               (when (= end position)
                 (error 'lisp-format-incomplete))
               (values (concatenate 'string prefix (subseq text position end)) end)))))))))

(-> lisp-format--flat (t) string)
(defun lisp-format--flat (node)
  "Return NODE on one line."
  (if (stringp node)
      node
      (format nil "~A(~{~A~^ ~})" (second node) (mapcar #'lisp-format--flat (nthcdr 2 node)))))

(-> lisp-format--render (t integer) string)
(defun lisp-format--render (node column)
  "Return NODE set out to start at COLUMN: on one line when it fits the width, else broken after its operator as Lisp is indented."
  (let ((flat (lisp-format--flat node)))
    (if (or (stringp node)
            (<= (+ column (length flat)) *lisp-format-width*)
            (null (nthcdr 2 node)))
        flat
        (let* ((prefix   (second node))
               (children (nthcdr 2 node))
               (head     (first children))
               (open     (+ column (length prefix) 1)))
          (flet ((stack (nodes at)
                   (format nil "~{~A~}"
                           (mapcar (lambda (child)
                                     (format nil "~%~A~A"
                                             (make-string at :initial-element #\Space)
                                             (lisp-format--render child at)))
                                   nodes))))
            (if (stringp head)
                (let* ((arguments (rest children))
                       (body      (rest (assoc head *lisp-format-body-heads* :test #'string-equal)))
                       (beside    (+ open (length head) 1)))
                  (cond
                    ;; A body form keeps a few arguments beside its operator and indents the rest by two.
                    (body
                     (let ((kept (subseq arguments 0 (min body (length arguments))))
                           (at   beside))
                       (format nil "~A(~A~{~A~}~A)"
                               prefix head
                               (mapcar (lambda (child)
                                         (prog1 (format nil " ~A" (lisp-format--render child at))
                                           (incf at (1+ (length (lisp-format--flat child))))))
                                       kept)
                               (stack (nthcdr body arguments) (1+ open)))))
                    ((null arguments)
                     flat)
                    ;; A call keeps its first argument beside the operator and lines the others up under it.
                    (t
                     (format nil "~A(~A ~A~A)"
                             prefix head
                             (lisp-format--render (first arguments) beside)
                             (stack (rest arguments) beside)))))
                ;; A list of lists, such as bindings, stacks its members.
                (format nil "~A(~A~A)"
                        prefix
                        (lisp-format--render head open)
                        (stack (rest children) open))))))))

(-> lisp-format (string) string)
(defun lisp-format (source)
  "Return SOURCE set out on several lines when it is one long line of whole forms without a comment, else SOURCE itself."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
    (if (or (find #\Newline trimmed)
            (< (length trimmed) *lisp-format-minimum*)
            (find #\; trimmed)
            (search "#|" trimmed))
        source
        (handler-case
            (let ((nodes    nil)
                  (position 0))
              (loop while (find-if-not (lambda (character)
                                         (member character '(#\Space #\Tab)))
                                       trimmed :start position)
                    do (multiple-value-bind (node after) (lisp-format--parse trimmed position)
                         (push node nodes)
                         (setf position after)))
              (format nil "~{~A~^~%~}"
                      (mapcar (lambda (node)
                                (lisp-format--render node 0))
                              (nreverse nodes))))
          (error ()
            source)))))
