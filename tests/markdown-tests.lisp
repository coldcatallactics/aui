(in-package #:aui)

;;;; -- Markdown Tests --

(fiveam:def-suite markdown
  :description "Tests for reading Markdown into blocks and inline runs."
  :in          aui)

(fiveam:in-suite markdown)

(fiveam:test inline-runs-carry-their-marks
  (dolist (case '(("plain text" (("plain text")))
                  ("a **bold** word" (("a ") ("bold" :bold t) (" word")))
                  ("an *italic* word" (("an ") ("italic" :italic t) (" word")))
                  ("with `code` in it" (("with ") ("code" :code t) (" in it")))
                  ("***both***" (("both" :bold t :italic t)))
                  ("~~gone~~" (("gone" :strike t)))
                  ("see [the site](https://example.org) now"
                   (("see ") ("the site" :link "https://example.org") (" now")))
                  ("at https://example.org/x." (("at ") ("https://example.org/x" :link "https://example.org/x") (".")))
                  ("snake_case_name stays" (("snake_case_name stays")))
                  ("2 * 3 * 4" (("2 * 3 * 4")))
                  ("an unmatched ** marker" (("an unmatched ** marker")))))
    (destructuring-bind (text expected) case
      (fiveam:is (equal expected (markdown-inline text)) "~S read as ~S" text (markdown-inline text)))))

(fiveam:test bold-holds-what-is-inside-it
  (fiveam:is (equal '(("run " :bold t) ("make" :bold t :code t))
                    (markdown-inline "**run `make`**"))))

(fiveam:test blocks-are-told-apart
  (let ((blocks (markdown-blocks (format nil "# Title~2%Some text~%that continues.~2%- one~%- two~%  - nested~2%1. first~%2. second~2%> quoted~2%---~2%- [x] done~%- [ ] open"))))
    (fiveam:is (equal '(:heading :paragraph :item :item :item :item :item :quote :rule :item :item)
                      (mapcar #'first blocks)))
    (fiveam:is (= 1 (second (first blocks))))
    (fiveam:is (equal '(("Some text that continues.")) (second (second blocks))))
    (fiveam:is (equal '(0 0 1) (mapcar #'second (subseq blocks 2 5))))
    (fiveam:is (equal '("1." "2.") (mapcar #'third (subseq blocks 5 7))))
    (fiveam:is (equal '(:done :open) (mapcar #'third (last blocks 2))))))

(fiveam:test fenced-code-keeps-its-text
  (let ((blocks (markdown-blocks (format nil "Before~2%```lisp~%(+ 1 2)~%~%; a **comment**~%```~2%After"))))
    (fiveam:is (equal '(:paragraph :code :paragraph) (mapcar #'first blocks)))
    (fiveam:is (equal "lisp" (second (second blocks))))
    (fiveam:is (equal (format nil "(+ 1 2)~%~%; a **comment**") (third (second blocks))))))

(fiveam:test an-open-fence-is-still-code
  ;; While an answer is streamed, the last code block has no closing fence yet.
  (let ((blocks (markdown-blocks (format nil "Look:~2%```~%partial"))))
    (fiveam:is (equal '(:paragraph :code) (mapcar #'first blocks)))
    (fiveam:is (equal "partial" (third (second blocks))))))

(fiveam:test tables-have-a-header-and-rows
  (let* ((blocks (markdown-blocks (format nil "| Name | Value |~%|---|---|~%| a | `1` |~%| b | 2 |")))
         (table  (first blocks)))
    (fiveam:is (eq ':table (first table)))
    (fiveam:is (equal '((("Name")) (("Value"))) (second table)))
    (fiveam:is (= 2 (length (third table))))
    (fiveam:is (equal '(("1" :code t)) (second (first (third table)))))))

(fiveam:test a-bar-in-prose-is-not-a-table
  (fiveam:is (equal '(:paragraph) (mapcar #'first (markdown-blocks "| just a line with bars |")))))

(fiveam:test typed-line-breaks-are-kept-on-request
  (let ((soft (markdown-blocks (format nil "one~%two")))
        (hard (markdown-blocks (format nil "one~%two~2%- a~%- b") :breaks-p t)))
    (fiveam:is (equal "one two" (first (first (second (first soft))))))
    (fiveam:is (equal (format nil "one~%two") (first (first (second (first hard))))))
    (fiveam:is (equal '(:paragraph :item :item) (mapcar #'first hard)))))

(fiveam:test text-for-a-terminal-is-shown-as-the-terminal-would
  ;; The text after a carriage return is written over the line.
  ;; The progress output of git is written this way.
  (fiveam:is (string= (format nil "Rebasing (3/3)~%done")
                      (text-clean (format nil "Rebasing (1/3)~CRebasing (2/3)~CRebasing (3/3)~%done"
                                          #\Return #\Return))))
  ;; Colors are removed. A tab is expanded to the next multiple of eight columns.
  (fiveam:is (string= "ok  failed"
                      (text-clean (format nil "~C[32mok~C[0m  failed" (code-char 27) (code-char 27)))))
  (fiveam:is (string= "ab      c" (text-clean (format nil "ab~Cc" #\Tab))))
  ;; Windows line endings are read as line endings.
  (fiveam:is (string= (format nil "a~%b") (text-clean (format nil "a~C~%b" #\Return))))
  ;; Text that needs no cleaning is returned as the same string.
  (let ((plain "nothing to clean"))
    (fiveam:is (eq plain (text-clean plain)))))

(fiveam:test a-long-form-is-set-out-and-keeps-its-spelling
  (let ((source "(let ((suffix (if (uiop:os-macosx-p) \"dylib\" \"so\"))) (format nil \"build/liblightfast.~A\" suffix) (list #'car '(a b) #p\"/tmp/x\" #\\( `(x ,y ,@z)))"))
    (let ((formatted (lisp-format source)))
      ;; The result has several lines. Every atom is spelled as in the source.
      (fiveam:is (find #\Newline formatted))
      (fiveam:is (search "uiop:os-macosx-p" formatted))
      (fiveam:is (search "#p\"/tmp/x\"" formatted))
      (fiveam:is (search "#\\(" formatted))
      (fiveam:is (search "`(x ,y ,@z)" formatted))
      (fiveam:is (string= (remove-if (lambda (character) (member character '(#\Space #\Newline))) source)
                          (remove-if (lambda (character) (member character '(#\Space #\Newline))) formatted)))))
  ;; Short forms, forms with line breaks, forms with comments and broken forms
  ;; are returned unchanged.
  (fiveam:is (string= "(+ 1 2)" (lisp-format "(+ 1 2)")))
  (let ((commented "(progn (foo) ; a comment that must stay where it is, whatever the length of the line is"))
    (fiveam:is (eq commented (lisp-format commented))))
  (let ((broken "(this form is never closed (and is long enough to be worth formatting (if it were whole)"))
    (fiveam:is (eq broken (lisp-format broken)))))

(fiveam:test a-link's-label-keeps-its-own-marks
  (fiveam:is (equal '(("the notes" :bold t :link "notes.md")
                      (" and ")
                      ("bold" :link "x.md" :bold t)
                      (" and ")
                      ("it" :italic t :link "i.md"))
                    (markdown-inline "**[the notes](notes.md)** and [**bold**](x.md) and *[it](i.md)*")))
  (fiveam:is (equal "notes.md"
                    (getf (rest (first (markdown-inline "[`window.lisp`](notes.md)"))) :link)))
  (fiveam:is (equal '(("https://www.fltk.org" :link "https://www.fltk.org"))
                    (markdown-inline "[](https://www.fltk.org)"))))

(fiveam:test text-that-grows-is-settled-up-to-its-last-blank-line-outside-a-fence
  (fiveam:is (= 0 (markdown-settled-end "one paragraph still being")))
  (fiveam:is (= 5 (markdown-settled-end (format nil "one~%~%two"))))
  (let ((fenced (format nil "one~%~%```lisp~%(a)~%~%(b)")))
    (fiveam:is (= 5 (markdown-settled-end fenced)))
    (fiveam:is (= (+ (length fenced) 6)
                  (markdown-settled-end (format nil "~A~%```~%~%after" fenced)))))
  (fiveam:is (= 10 (markdown-settled-end (format nil "one~%~%two~%~%three") :start 5))))
