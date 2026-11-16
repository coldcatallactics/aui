(in-package #:aui)

;;;; -- Highlighting --

;;; Source is colored by the highlighter Autolith's terminal uses, so a form,
;;; a command or an edit reads here as it does there. What comes back is plain
;;; data, roles and text; which ink a role takes is the theme's to say.

(defvar *highlight-library-sought-p* nil
  "Whether this process has looked for the highlighter's native library in a release yet.")

(-> highlight--seek-library () null)
(defun highlight--seek-library ()
  "Look once for the native library a release brings, before the highlighter looks for one to build."
  (unless *highlight-library-sought-p*
    (setf *highlight-library-sought-p* t)
    (installation-adopt))
  nil)

(defparameter *lisp-languages* '("lisp" "common-lisp" "cl" "elisp" "emacs-lisp" "scheme" "clojure")
  "The names a code fence gives the languages written in parentheses.")

(defparameter *lisp-file-types* '("lisp" "asd" "cl" "el" "scm" "clj" "sexp")
  "The types of the files written in parentheses.")

(-> highlight--lisp-p ((or null string) (or null string)) boolean)
(defun highlight--lisp-p (language pathname)
  "Return T when LANGUAGE, or else the type of the file PATHNAME names, is one written in parentheses."
  (and (if language
           (member language *lisp-languages* :test #'string-equal)
           (and pathname
                (member (pathname-type (pathname pathname)) *lisp-file-types* :test #'equal)))
       t))

(-> highlight--nest (list string) list)
(defun highlight--nest (parts source)
  "Return PARTS, the (ROLE . TEXT) of the Lisp SOURCE, with each parenthesis apart under the role of its nesting depth.

The depths are the ones Autolith's terminal colors, :SYNTAX-PARENTHESIS-1 to
:SYNTAX-PARENTHESIS-6 and around again. PARTS that do not spell SOURCE
character for character are returned as they are."
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
  "Return SOURCE as a list of (ROLE . TEXT) in order, or NIL when no language is known for it.

LANGUAGE names the language as a code fence would, such as lisp or bash;
PATHNAME lets a file's name choose it instead. The roles are :PLAIN and the
terminal's syntax roles, :SYNTAX-KEYWORD, :SYNTAX-FUNCTION, :SYNTAX-STRING,
:SYNTAX-NUMBER, :SYNTAX-COMMENT and the rest. In Lisp each parenthesis has
the role of its nesting depth, so the two of a pair read alike."
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
  "Return the rows of the edit that replaced OLD with NEW, each a list of (ROLE . TEXT), or NIL when it cannot be shown so.

Each row opens with a gutter, :CONTEXT-GUTTER, :REMOVED-GUTTER or
:ADDED-GUTTER, that numbers the line and says what happened to it; the rest
of the row is the line's source, colored for the language PATHNAME's name
implies. At most LIMIT rows of the change are returned."
  (highlight--seek-library)
  (handler-case
      (autolith::render-diff :removed-content (and (plusp (length old)) old)
                             :added-content   (and (plusp (length new)) new)
                             :source-path     pathname
                             :line-limit      limit
                             :span-function   #'cons)
    (error ()
      nil)))
