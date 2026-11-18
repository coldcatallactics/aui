;;;; -- AUI Systems --

(asdf:defsystem #:aui
  :description "A native graphical client for Autolith, built on Lightfast and FLTK."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:autolith
                #:lightfast
                #:serapeum
                #:sito
                #:bordeaux-threads)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:module "model"
      :serial t
      :components
      ((:file "transcript")
       (:file "events")
       (:file "markdown")
       (:file "preferences")
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
     (:module "ui"
      :serial t
      :components
      ((:file "theme")
       (:file "paint")
       (:file "stage")
       (:file "paragraph")
       (:file "blocks")
       (:file "controls")))
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

(asdf:defsystem #:aui/tests
  :description "Tests for AUI"
  :depends-on  (#:aui
                #:fiveam)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")
     (:file "transcript-tests")
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
