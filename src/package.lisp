(defpackage #:aui
  (:documentation "A native graphical client for Autolith.")
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:partial)
  (:export #:main
           #:run-tests))
