(in-package #:aui)

(fiveam:def-suite markdown :in aui
  :description "Markdown to styled runs.")

(fiveam:in-suite markdown)

(-> styles-of (list) list)
(defun styles-of (runs)
  "Return (TEXT . STYLE) pairs for RUNS."
  (mapcar (lambda (run) (cons (run-text run) (run-style run))) runs))

(fiveam:test inline-emphasis-and-code
  (fiveam:is (equal '(("Use " . :body) ("apply-flat-theme" . :code) (" and " . :body)
                      ("Theme" . :bold) (" for " . :body) ("emphasis" . :italic) ("." . :body))
                    (styles-of (markdown-inline-runs "Use `apply-flat-theme` and **Theme** for *emphasis*.")))))

(fiveam:test underscores-only-at-word-boundaries
  (fiveam:is (equal '(("snake_case_name stays" . :body))
                    (styles-of (markdown-inline-runs "snake_case_name stays"))))
  (fiveam:is (equal '(("quiet" . :italic) (" word" . :body))
                    (styles-of (markdown-inline-runs "_quiet_ word")))))

(fiveam:test bold-italic-and-unmatched-markers
  (fiveam:is (equal '(("loud" . :bold-italic))
                    (styles-of (markdown-inline-runs "***loud***"))))
  (fiveam:is (equal '(("a * b and `tick" . :body))
                    (styles-of (markdown-inline-runs "a * b and `tick")))))

(fiveam:test links-keep-their-url
  (let ((runs (markdown-inline-runs "See [the README](https://example.org/r) now")))
    (fiveam:is (equal '(("See " . :body) ("the README" . :link) (" now" . :body)) (styles-of runs)))
    (fiveam:is (string= "https://example.org/r" (run-url (second runs))))))

(fiveam:test documents-have-fences-headings-and-lists
  (let ((runs (markdown-runs (format nil "# Title~%Text~%~%```lisp~%(+ 1 2)~%```~%- one~%2. two~%"))))
    (fiveam:is (equal (list (cons "Title" ':heading-1) (cons (string #\Newline) ':body)
                            (cons "Text" ':body) (cons (string #\Newline) ':body)
                            (cons (string #\Newline) ':body)
                            (cons (format nil "(+ 1 2)~%") ':code-block)
                            (cons "• " ':bullet) (cons "one" ':body) (cons (string #\Newline) ':body)
                            (cons "2. " ':list-number) (cons "two" ':body) (cons (string #\Newline) ':body))
                      (styles-of runs)))
    (fiveam:is (string= (format nil "Title~%Text~%~%(+ 1 2)~%• one~%2. two~%") (runs-text runs)))))

(fiveam:test headings-flatten-inner-emphasis
  (fiveam:is (equal '(("Bold heading" . :heading-2) ("
" . :body))
                    (styles-of (markdown-runs "## **Bold** heading"))))
  (fiveam:is (eq ':heading-1 (run-style (first (markdown-runs "# One")))))
  (fiveam:is (eq ':heading-3 (run-style (first (markdown-runs "#### Deep")))))
  (fiveam:is (null (markdown-heading-level "#hashtag")))
  (fiveam:is (= 3 (markdown-heading-level "### Deep"))))

(fiveam:test list-markers
  (fiveam:is (= 2 (markdown-list-marker "- item")))
  (fiveam:is (equal '(4 2 "• ") (multiple-value-list (markdown-list-marker "  * nested"))))
  (fiveam:is (equal '(4 0 "10. ") (multiple-value-list (markdown-list-marker "10. tenth"))))
  (fiveam:is (null (markdown-list-marker "-no space")))
  (fiveam:is (null (markdown-list-marker "3.14 is pi"))))

(fiveam:test adjacent-runs-merge
  (fiveam:is (equal '(("ab" . :body) ("c" . :code) ("d" . :body))
                    (styles-of (runs-merge-adjacent (list (make-run "a" ':body) (make-run "b" ':body)
                                                          (make-run "c" ':code) (make-run "d" ':body))))))
  (let ((merged (runs-merge-adjacent (list (make-run "x" ':link "u1") (make-run "y" ':link "u2")))))
    (fiveam:is (= 2 (length merged)))))

(fiveam:test empty-lines-are-kept
  (fiveam:is (equal '("a" "" "b") (split-lines-keeping-empty (format nil "a~%~%b~%"))))
  (fiveam:is (equal '("") (split-lines-keeping-empty ""))))

(fiveam:test strike-images-quotes-tasks-and-tables
  (fiveam:is (equal '(("old" . :strike) (" new" . :body)) (styles-of (markdown-inline-runs "~~old~~ new"))))
  (let ((image (first (markdown-inline-runs "![a cat](http://x/c.png)"))))
    (fiveam:is (string= "[image: a cat]" (run-text image)))
    (fiveam:is (string= "http://x/c.png" (run-url image))))
  (fiveam:is (equal '(("│ " . :quote) ("wise words" . :quote) ("
" . :body))
                    (styles-of (markdown-runs "> wise words"))))
  (fiveam:is (equal '(("☑ " . :marker) ("done" . :body) ("
" . :body) ("☐ " . :marker) ("todo" . :body) ("
" . :body))
                    (styles-of (markdown-runs (format nil "- [x] done~%- [ ] todo")))))
  (let ((runs (markdown-runs (format nil "| a | bb |~%|---|---|~%| 1 | 2 |"))))
    (fiveam:is (eq ':code-bold (run-style (first runs))))
    (fiveam:is (string= "a  " (run-text (first runs))))
    (fiveam:is (find ':table runs :key #'run-style))))

(fiveam:test code-is-highlighted-in-monochrome
  (let ((runs (code-runs (format nil "(defun f (x) ; add~%  (+ x 1))") "lisp")))
    (fiveam:is (equal '(("(" . :code-block) ("defun" . :code-bold) (" f (x) " . :code-block) ("; add" . :code-italic)
                        ("
  (+ x 1))
" . :code-block))
                      (styles-of runs))))
  (let ((runs (code-runs "if x: return y # note" "python")))
    (fiveam:is (eq ':code-bold (run-style (first runs))))
    (fiveam:is (eq ':code-italic (run-style (find "# note" runs :key #'run-text :test #'string=))))))
