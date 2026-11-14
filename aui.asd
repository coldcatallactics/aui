;;;; -- AUI Systems --

(asdf:defsystem #:aui
  :description "A native graphical client for Autolith's world of channels, built on Lightfast and FLTK."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:autolith
                #:lightfast
                #:serapeum
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
       (:file "overlay")
       (:file "sidebar")
       (:file "composer")
       (:file "completion")
       (:file "chrome")
       (:file "activity")
       (:file "catalogs")
       (:file "sheets")
       (:file "panels")
       (:file "window")
       (:file "demo")))
     (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:aui/tests))))

(asdf:defsystem #:aui/tests
  :description "Tests for AUI, run in one process without a display or credentials."
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
     (:file "launch-tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (unless (uiop:symbol-call '#:aui '#:run-tests)
                   (error "AUI tests failed."))))
