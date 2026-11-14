(defpackage #:aui
  (:documentation "A native graphical client for Autolith's world of channels.")
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:partial)
  (:export #:main
           #:run-tests))
