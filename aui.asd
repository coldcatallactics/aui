(asdf:defsystem #:aui
  :description "A native graphical workbench for Autolith, built on Lightfast and FLTK."
  :author "Lambda Symbolics"
  :license "MIT"
  :depends-on (#:lightfast
               #:agentcomms
               #:argo
               #:serapeum
               #:bordeaux-threads)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "theme")
     (:file "transcript")
     (:file "events")
     (:file "catalog")
     (:file "backend-protocol")
     (:module "view"
      :serial t
      :components
      ((:file "markdown")
       (:file "document")
       (:file "transcript-view")
       (:file "workbench")
       (:file "settings-sheet")
       (:file "palette")
       (:file "welcome")
       (:file "fixtures")))
     (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:aui/tests))))

(asdf:defsystem #:aui/tests
  :description "Tests for AUI, run in one process without a display or credentials."
  :depends-on (#:aui #:fiveam)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")
     (:file "theme-tests")
     (:file "transcript-tests")
     (:file "events-tests")
     (:file "catalog-tests")
     (:file "markdown-tests")
     (:file "document-tests")
     (:file "palette-tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:aui '#:run-tests)
               (error "AUI tests failed."))))
