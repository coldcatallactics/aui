;;;; -- AUI Systems --

;;; The toolkit is what draws: the faces, the stage, paragraphs, blocks and
;;; controls, with the preferences a window keeps. It names nothing of
;;; Autolith, so a program that only needs a window loads it alone. AUI is
;;; the client of Autolith built on it, and unslop is a second program on it.

(asdf:defsystem #:aui/toolkit
  :description "What AUI draws its windows with: faces, a stage, paragraphs, blocks and controls on Lightfast."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:lightfast
                #:serapeum)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:module "model"
      :serial t
      :components
      ((:file "preferences")))
     (:module "ui"
      :serial t
      :components
      ((:file "theme")
       (:file "paint")
       (:file "stage")
       (:file "paragraph")
       (:file "blocks")
       (:file "controls")))))))

(asdf:defsystem #:aui
  :description "A native graphical client for Autolith's world of channels, built on Lightfast and FLTK."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:aui/toolkit
                #:autolith
                #:sito
                #:bordeaux-threads)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "model"
      :serial t
      :components
      ((:file "transcript")
       (:file "events")
       (:file "markdown")
       (:file "lisp-format")))
     (:module "client"
      :serial t
      :components
      ((:file "portable")
       (:file "installation")
       (:file "world")
       (:file "records")
       (:file "history")
       (:file "workspace")
       (:file "highlight")
       (:file "session")
       (:file "launch")))
     (:module "view"
      :serial t
      :components
      ((:file "prose")
       (:file "entries")
       (:file "document")
       (:file "workbench")
       (:file "files")
       (:file "digest")
       (:file "overlay")
       (:file "sidebar")
       (:file "composer")
       (:file "completion")
       (:file "chrome")
       (:file "activity")
       (:file "catalogs")
       (:file "sheets")
       (:file "panels")
       (:file "console")
       (:file "window")
       (:file "demo")))
     (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:aui/tests))))

;;; Unslop is a second program on the toolkit: a window for the words of a
;;; Common Lisp checkout. It loads nothing of Autolith.

(asdf:defsystem #:aui/unslop
  :description "A window for editing the comments, strings and docstrings of a Common Lisp checkout."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:aui/toolkit
                #:sb-posix)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "unslop"
      :serial t
      :components
      ((:file "lexer")
       (:file "proof")
       (:file "checkout")
       (:file "ledger")
       (:file "desk")
       (:file "shelf")
       (:file "page")
       (:file "unslop")))))))

(asdf:defsystem #:aui/test-support
  :description "The suite every AUI test belongs to and the function that runs it."
  :depends-on  (#:aui/toolkit
                #:fiveam)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")))))

(asdf:defsystem #:aui/unslop/tests
  :description "Tests for unslop, run without a display."
  :depends-on  (#:aui/unslop
                #:aui/test-support)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:module "unslop"
      :serial t
      :components
      ((:file "lexer-tests")
       (:file "proof-tests")
       (:file "checkout-tests")))))))

(asdf:defsystem #:aui/tests
  :description "Tests for AUI, run in one process without a display or credentials."
  :depends-on  (#:aui
                #:aui/test-support
                #:aui/unslop/tests)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "transcript-tests")
     (:file "markdown-tests")
     (:file "records-tests")
     (:file "history-tests")
     (:file "session-tests")
     (:file "document-tests")
     (:file "links-tests")
     (:file "launch-tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (unless (uiop:symbol-call '#:aui '#:run-tests)
                   (error "AUI tests failed."))))
