;;;; -- Exercise support --

(in-package #:aui)

(defvar *exercise-failures* 0
  "This is the number of failed expectations.")

(defmacro exercise-expect (description form)
  "Evaluate FORM and print DESCRIPTION as passed or failed."
  `(if ,form
       (format t "~&  ok    ~A~%" ,description)
       (progn
         (incf *exercise-failures*)
         (format t "~&  FAIL  ~A~%" ,description))))

(-> exercise--draw (workbench) null)
(defun exercise--draw (workbench)
  "Draw WORKBENCH and refresh its regions."
  (window-arrange workbench)
  (lightfast:capture-window (workbench-window workbench)
                            (uiop:native-namestring
                             (merge-pathnames "aui-exercise.ppm" (uiop:temporary-directory))))
  nil)

(-> exercise--region (workbench t) (or null region))
(defun exercise--region (workbench identifier)
  "Find IDENTIFIER in WORKBENCH's latest drawing."
  (stage--find-region (workbench-stage workbench) identifier))

(-> exercise--click (workbench t) boolean)
(defun exercise--click (workbench identifier)
  "Click the center of IDENTIFIER, if it exists."
  (let ((region (exercise--region workbench identifier))
        (stage  (workbench-stage workbench)))
    (when region
      (let ((position (format nil "~D ~D 1 0 0 0"
                              (+ (region-x region) (floor (region-width region) 2))
                              (+ (region-y region) (floor (region-height region) 2)))))
        (stage--pointer stage ':move position)
        (stage--pointer stage ':push position)
        (stage--pointer stage ':release position))
      t)))

(-> exercise--finish () null)
(defun exercise--finish ()
  "Print the result and exit with its status."
  (format t "~&~:[The interface exercise passed.~;~:*~D expectation~:P failed.~]~%"
          (and (plusp *exercise-failures*) *exercise-failures*))
  (finish-output)
  (sb-ext:exit :code (if (zerop *exercise-failures*) 0 1) :abort t))
