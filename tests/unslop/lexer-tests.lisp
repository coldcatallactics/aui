(in-package #:aui)

;;;; -- Unslop: Lexer Tests --

(fiveam:def-suite unslop
  :description "The words of Lisp source are found, changed and written back, and nothing else is."
  :in          aui)

(fiveam:in-suite unslop)

(-> test-lexemes (string) list)
(defun test-lexemes (source)
  "Return the lexemes of SOURCE as (KIND . TEXT) pairs."
  (mapcar (lambda (lexeme)
            (cons (lexeme-kind lexeme) (subseq source (lexeme-start lexeme) (lexeme-end lexeme))))
          (lisp-lex source)))

(fiveam:test source-is-cut-into-its-pieces
  (fiveam:is (equal '((:open . "(") (:atom . "defun") (:atom . "f") (:open . "(") (:atom . "x") (:close . ")")
                      (:string . "\"doc\"") (:comment . "; why") (:atom . "x") (:close . ")"))
                    (test-lexemes (format nil "(defun f (x) \"doc\" ; why~% x)"))))
  (fiveam:is (equal '((:prefix . "'") (:open . "(") (:atom . "a") (:atom . ".") (:atom . "b") (:close . ")")
                      (:prefix . "#'") (:atom . "car") (:prefix . "`") (:open . "(") (:prefix . ",") (:atom . "a")
                      (:prefix . ",@") (:atom . "b") (:close . ")") (:prefix . "#+") (:atom . "sbcl")
                      (:atom . ":x"))
                    (test-lexemes "'(a . b) #'car `(,a ,@b) #+sbcl :x")))
  (fiveam:is (equal '((:open . "#(") (:atom . "1") (:close . ")") (:atom . "#p") (:string . "\"/tmp\"")
                      (:atom . "#:gensym") (:atom . "#b101"))
                    (test-lexemes "#(1) #p\"/tmp\" #:gensym #b101"))))

(fiveam:test a-quote-or-a-semicolon-in-a-character-opens-nothing
  (fiveam:is (equal '((:open . "(") (:atom . "list") (:atom . "#\\\"") (:atom . "#\\;") (:atom . "#\\(") (:atom . "#\\)") (:atom . "#\\Space") (:atom . "#\\\\") (:close . ")"))
                    (test-lexemes "(list #\\\" #\\; #\\( #\\) #\\Space #\\\\)")))
  ;; So does one between bars or behind a backslash in a symbol.
  (fiveam:is (equal '((:atom . "|a;b \"c|") (:atom . "x\\;y") (:atom . "z"))
                    (test-lexemes "|a;b \"c| x\\;y z"))))

(fiveam:test a-string-keeps-what-a-backslash-protects
  (fiveam:is (equal '((:string . "\"a \\\" b ; c\"") (:atom . "d"))
                    (test-lexemes "\"a \\\" b ; c\" d")))
  (fiveam:is (equal '((:string . "\"ends with a backslash \\\\\"") (:atom . "e"))
                    (test-lexemes "\"ends with a backslash \\\\\" e"))))

(fiveam:test block-comments-nest
  (fiveam:is (equal '((:atom . "a") (:block . "#| one #| two |# \" |#") (:atom . "b"))
                    (test-lexemes "a #| one #| two |# \" |# b"))))

(fiveam:test a-comment-ends-with-its-line
  (fiveam:is (equal '((:comment . ";; one") (:comment . ";; two \"") (:atom . "a"))
                    (test-lexemes (format nil ";; one~%;; two \"~%a")))))

(fiveam:test what-never-closes-is-refused
  (fiveam:signals proof-error (lisp-lex "(a \"never closed"))
  (fiveam:signals proof-error (lisp-lex "a #| never closed")))
