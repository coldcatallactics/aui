(in-package #:aui)

;;;; -- Highlighting --

;;; Source is colored by the highlighter of the Autolith terminal, so forms,
;;; commands and edits look the same as in the terminal. The result is plain
;;; data of roles and text. The ink of each role is chosen by the theme.

(defvar *highlight-library-sought-p* nil
  "True if this process has already looked for the native highlighter library of a release.")

(-> highlight--seek-library () null)
(defun highlight--seek-library ()
  "Look for the native highlighter library of a release. Do nothing after the first call."
  (unless *highlight-library-sought-p*
    (setf *highlight-library-sought-p* t)
    (installation-adopt))
  nil)

(defparameter *lisp-languages* '("lisp" "common-lisp" "cl" "elisp" "emacs-lisp" "scheme" "clojure")
  "The code fence names of the languages written in parentheses.")

(defparameter *lisp-file-types* '("lisp" "asd" "cl" "el" "scm" "clj" "sexp")
  "The file types of the languages written in parentheses.")

(-> highlight--lisp-p ((or null string) (or null string)) boolean)
(defun highlight--lisp-p (language pathname)
  "Return T if LANGUAGE is written in parentheses.
If LANGUAGE is NIL, use the file type of PATHNAME instead."
  (and (if language
           (member language *lisp-languages* :test #'string-equal)
           (and pathname
                (member (pathname-type (pathname pathname)) *lisp-file-types* :test #'equal)))
       t))

(-> highlight--nest (list string) list)
(defun highlight--nest (parts source)
  "Return PARTS with each parenthesis split off under the role of the nesting depth.
PARTS is a list of (ROLE . TEXT) for the Lisp SOURCE.

The roles of the depths are :SYNTAX-PARENTHESIS-1 to :SYNTAX-PARENTHESIS-6.
The roles repeat for deeper levels. If PARTS do not match SOURCE character for
character, return PARTS unchanged."
  (let ((roles (autolith::syntax--lisp-parenthesis-roles source)))
    (if (/= (length roles) (reduce #'+ parts :key (lambda (part) (length (rest part)))))
        parts
        (let ((offset 0)
              (nested nil))
          (loop for (role . text) in parts
                do (loop with start = 0
                         while (< start (length text))
                         do (let* ((own (or (aref roles (+ offset start)) role))
                                   (end (loop for index from (1+ start) below (length text)
                                              unless (eq own (or (aref roles (+ offset index)) role))
                                                do (return index)
                                              finally (return (length text)))))
                              (push (cons own (subseq text start end)) nested)
                              (setf start end)))
                   (incf offset (length text)))
          (nreverse nested)))))

(-> source-highlight (string &key (:language (or null string)) (:pathname (or null string))) list)
(defun source-highlight (source &key language pathname)
  "Return SOURCE as a list of (ROLE . TEXT), or NIL if the language of SOURCE is unknown.

LANGUAGE is the name of the language in a code fence, such as lisp or bash.
Otherwise, the language is chosen from the file name PATHNAME. The roles are
:PLAIN and the syntax roles of the terminal, such as :SYNTAX-KEYWORD,
:SYNTAX-FUNCTION, :SYNTAX-STRING, :SYNTAX-NUMBER and :SYNTAX-COMMENT. In Lisp,
each parenthesis has the role of the nesting depth, so both parentheses of a
pair have the same color."
  (highlight--seek-library)
  (handler-case
      (let* ((found (and language
                         (autolith::language-find (string-downcase language) :errorp nil)))
             (parts (and (or found pathname)
                         (autolith::highlight-spans source
                                                    :language      found
                                                    :pathname      pathname
                                                    :span-function #'cons))))
        (if (and parts (highlight--lisp-p language pathname))
            (highlight--nest parts source)
            parts))
    (error ()
      nil)))

(-> edit-highlight (string string &key (:pathname (or null string)) (:limit integer)) list)
(defun edit-highlight (old new &key pathname (limit 60))
  "Return the rows of the edit that replaced OLD with NEW, or NIL if the edit cannot be rendered.
Each row is a list of (ROLE . TEXT).

Each row starts with a gutter of the role :CONTEXT-GUTTER, :REMOVED-GUTTER or
:ADDED-GUTTER. The gutter contains the line number and the kind of change. The
rest of the row is the source of the line, colored for the language of the
file PATHNAME. At most LIMIT rows of the change are returned."
  (highlight--seek-library)
  (handler-case
      (autolith::render-diff :removed-content (and (plusp (length old)) old)
                             :added-content   (and (plusp (length new)) new)
                             :source-path     pathname
                             :line-limit      limit
                             :span-function   #'cons)
    (error ()
      nil)))
