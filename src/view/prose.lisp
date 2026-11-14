(in-package #:aui)

;;;; -- Prose --

;;; Markdown's nodes given their typography: the blocks an answer is drawn
;;; from. Emphasis is weight and slope; code among prose is the typewriter
;;; face in blue; a link is underlined blue.

(defparameter *prose-paragraph-gap* 12
  "Space between two paragraphs of an answer.")

(defparameter *prose-item-indent* 26
  "How far each level of a list is set in.")

(-> prose--inline-style (text-style list) (or null text-style))
(defun prose--inline-style (base properties)
  "Return the style of an inline node with PROPERTIES inside text set in BASE, or NIL when it is BASE itself."
  (let ((bold-p   (getf properties :bold))
        (italic-p (getf properties :italic))
        (code-p   (getf properties :code))
        (link     (getf properties :link))
        (strike-p (getf properties :strike)))
    (if (or bold-p italic-p code-p link strike-p)
        (let ((style (copy-text-style base)))
          (cond
            (code-p
             (let ((code (text-style ':code)))
               (setf (text-style-face style) (face-variant ':mono :bold-p bold-p :italic-p italic-p)
                     ;; Inline code keeps the line it sits in: a little smaller, the same leading.
                     (text-style-size style) (max 11 (- (text-style-size base) 3))
                     (text-style-line-height style) (max (text-style-line-height base)
                                                         (text-style-line-height code)))))
            (t
             (let* ((face          (text-style-face base))
                    (base-bold-p   (member face '(:serif-bold :serif-bold-italic :mono-bold :mono-bold-italic)))
                    (base-italic-p (member face '(:serif-italic :serif-bold-italic :mono-italic
                                                  :mono-bold-italic))))
               (setf (text-style-face style)
                     (face-variant face
                                   :bold-p   (and (or bold-p base-bold-p) t)
                                   :italic-p (and (or italic-p base-italic-p) t))))))
          (when (or link code-p)
            (setf (text-style-color style) *blue*))
          (when link
            (setf (text-style-underline-p style) t))
          (when strike-p
            (setf (text-style-strike-p style) t))
          style)
        nil)))

(-> prose-spans (list text-style) list)
(defun prose-spans (inlines base)
  "Return the spans of the inline nodes INLINES for text set in BASE."
  (loop for (text . properties) in inlines
        collect (make-span text (prose--inline-style base properties) :link (getf properties :link))))

(-> prose--table (list list) table-block)
(defun prose--table (header rows)
  "Return the table block of HEADER cells and ROWS of cells, each cell a list of inline nodes."
  (let ((bold (text-style ':list-bold))
        (body (text-style ':list)))
    (make-instance 'table-block
                   :rows          (cons (mapcar (lambda (cell)
                                                  (make-paragraph (prose-spans cell bold) :style bold))
                                                header)
                                        (mapcar (lambda (row)
                                                  (mapcar (lambda (cell)
                                                            (make-paragraph (prose-spans cell body) :style body))
                                                          row))
                                                rows))
                   :margin-top    *prose-paragraph-gap*
                   :margin-bottom *prose-paragraph-gap*)))

(-> prose-blocks (string &key (:style text-style) (:breaks-p boolean)) list)
(defun prose-blocks (text &key (style (text-style ':body)) breaks-p)
  "Return the blocks of the Markdown TEXT, its running text set in STYLE.

BREAKS-P keeps the line breaks a person typed inside a paragraph."
  (let ((blocks nil))
    (dolist (node (markdown-blocks text :breaks-p breaks-p))
      (push
       (ecase (first node)
         (:heading
          (destructuring-bind (level inlines) (rest node)
            (let ((heading (text-style (case level
                                         (1
                                          ':heading-1)
                                         (2
                                          ':heading-2)
                                         (otherwise
                                          ':heading-3)))))
              (make-text-block (prose-spans inlines heading)
                               :style         heading
                               :margin-top    (if (= level 1) 26 20)
                               :margin-bottom 6))))
         (:paragraph
          (make-text-block (prose-spans (second node) style)
                           :style         style
                           :margin-top    *prose-paragraph-gap*
                           :margin-bottom *prose-paragraph-gap*))
         (:item
          (destructuring-bind (depth marker inlines) (rest node)
            (make-text-block (prose-spans inlines style)
                             :style         style
                             :indent        (* *prose-item-indent* (1+ depth))
                             :marker        marker
                             :margin-top    4
                             :margin-bottom 4)))
         (:quote
          (let ((quoted (copy-text-style style)))
            (setf (text-style-face quoted) (face-variant (text-style-face style) :italic-p t))
            (make-text-block (prose-spans (second node) quoted)
                             :style         quoted
                             :indent        *prose-item-indent*
                             :margin-top    *prose-paragraph-gap*
                             :margin-bottom *prose-paragraph-gap*)))
         (:code
          (destructuring-bind (language code) (rest node)
            (make-code-block code
                             :label         language
                             :spans         (code-spans code language)
                             :margin-top    *prose-paragraph-gap*
                             :margin-bottom *prose-paragraph-gap*)))
         (:table
          (prose--table (second node) (third node)))
         (:rule
          (make-instance 'rule-block :margin-top 14 :margin-bottom 14)))
       blocks))
    (nreverse blocks)))


;;;; -- Code Highlighting --

;;; Highlighting is light: defining words bold in plum, strings green,
;;; comments italic.

(defparameter *code-defining-words*
  '("defun" "defmacro" "defgeneric" "defmethod" "defclass" "defstruct" "defvar" "defparameter"
    "defpackage" "deftype" "define-condition" "lambda" "let" "let*" "flet" "labels" "if" "when"
    "unless" "cond" "case" "loop" "progn" "function" "def" "class" "return" "import" "from" "fn"
    "func" "const" "var" "for" "while" "else" "struct" "enum" "impl" "pub" "use" "match" "export"
    "async" "await" "static" "void" "public" "private" "interface" "type" "package" "elif" "try"
    "except" "raise" "with" "as" "in" "not" "and" "or" "then" "fi" "do" "done" "end")
  "Words set bold in code, across the languages agents most often write.")

(-> code--comment-start (string (or null string)) (or null integer))
(defun code--comment-start (line language)
  "Return where a comment begins on LINE of LANGUAGE, or NIL. Lisp comments start at a semicolon, most others at // or #."
  (let ((lisp-p (member language '("lisp" "common-lisp" "cl" "elisp" "emacs-lisp" "scheme" "clojure")
                        :test #'equal)))
    (cond
      (lisp-p
       (position #\; line))
      ((member language '("python" "py" "sh" "bash" "shell" "zsh" "ruby" "yaml" "toml" "nix" "make")
               :test #'equal)
       (let ((hash (position #\# line)))
         (and hash (or (zerop hash) (char= (char line (1- hash)) #\Space)) hash)))
      (t
       (search "//" line)))))

(-> code--string-end (string integer) integer)
(defun code--string-end (source start)
  "Return the position after the string literal that opens at START in SOURCE, the end of SOURCE when it is not closed."
  (let ((quote (char source start)))
    (loop for index from (1+ start) below (length source)
          do (cond
               ((char= (char source index) #\\)
                (incf index))
               ((char= (char source index) quote)
                (return (1+ index))))
          finally (return (length source)))))

(-> code-spans (string (or null string)) list)
(defun code-spans (code language)
  "Return the spans of CODE in LANGUAGE: comments italic, defining words bold plum, strings green, the rest plain."
  (let ((bold   (text-style ':code :face ':mono-bold :color *plum*))
        (italic (text-style ':code :face ':mono-italic))
        (quoted (text-style ':code :color *green*))
        (spans  nil))
    (flet ((emit (text style)
             ;; Neighbours in one style join, so a line of code is a few runs, not one a word.
             (when (plusp (length text))
               (let ((previous (first spans)))
                 (if (and previous (eq (span-style previous) style))
                     (setf (span-text previous) (concatenate 'string (span-text previous) text))
                     (push (make-span text style) spans))))))
      (loop for (line . more) on (markdown-lines code)
            do (let* ((comment (code--comment-start line language))
                      (source  (subseq line 0 comment))
                      (index   0))
                 (loop while (< index (length source))
                       do (let* ((next       (position-if (lambda (character)
                                                            (or (alpha-char-p character) (char= character #\")))
                                                          source :start index))
                                 (string-p   (and next (char= (char source next) #\")))
                                 (word-start (and next (not string-p) next))
                                 (word-end   (and word-start
                                                  (or (position-if-not
                                                       (lambda (character)
                                                         (or (alphanumericp character) (find character "-*_")))
                                                       source :start word-start)
                                                      (length source)))))
                            (cond
                              ((null next)
                               (emit (subseq source index) nil)
                               (setf index (length source)))
                              (string-p
                               (let ((end (code--string-end source next)))
                                 (emit (subseq source index next) nil)
                                 (emit (subseq source next end) quoted)
                                 (setf index end)))
                              (t
                               (emit (subseq source index word-start) nil)
                               (let ((word (subseq source word-start word-end)))
                                 (emit word (and (member word *code-defining-words* :test #'string=)
                                                 (or (zerop word-start)
                                                     (not (alphanumericp (char source (1- word-start)))))
                                                 bold)))
                               (setf index word-end)))))
                 (when comment
                   (emit (subseq line comment) italic))
                 (when more
                   (emit (string #\Newline) nil)))))
    (or (nreverse spans) (list (make-span "" nil)))))
