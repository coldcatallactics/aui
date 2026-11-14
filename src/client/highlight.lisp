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

(-> source-highlight (string &key (:language (or null string)) (:pathname (or null string))) list)
(defun source-highlight (source &key language pathname)
  "Return SOURCE as a list of (ROLE . TEXT) in order, or NIL when no language is known for it.

LANGUAGE names the language as a code fence would, such as lisp or bash;
PATHNAME lets a file's name choose it instead. The roles are :PLAIN and the
terminal's syntax roles, :SYNTAX-KEYWORD, :SYNTAX-FUNCTION, :SYNTAX-STRING,
:SYNTAX-NUMBER, :SYNTAX-COMMENT and the rest."
  (highlight--seek-library)
  (handler-case
      (let ((found (and language
                        (autolith::language-find (string-downcase language) :errorp nil))))
        (and (or found pathname)
             (autolith::highlight-spans source
                                        :language      found
                                        :pathname      pathname
                                        :span-function #'cons)))
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
