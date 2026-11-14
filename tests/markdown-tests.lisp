(in-package #:aui)

;;;; -- Markdown Tests --

(fiveam:def-suite markdown
  :description "The Markdown that agents write is read into blocks and inline runs."
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
  ;; While an answer streams, its last code block has no closing fence yet.
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
