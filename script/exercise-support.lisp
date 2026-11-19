;;;; -- Exercise Support --

;;; The code shared by the exercises. It contains the counted expectations and
;;; the functions used to draw a window offscreen and to send clicks to the
;;; window in the same way as FLTK.

(in-package #:aui)

(defvar *exercise-failures* 0
  "The number of failed expectations of the exercise.")

(defmacro exercise-expect (description form)
  "Evaluate FORM and print DESCRIPTION as passed or failed."
  `(if ,form
       (format t "~&  ok    ~A~%" ,description)
       (progn
         (incf *exercise-failures*)
         (format t "~&  FAIL  ~A~%" ,description))))

(-> exercise--draw (workbench) null)
(defun exercise--draw (workbench)
  "Draw the window of WORKBENCH offscreen. The regions of the stage are then updated to the current state."
  (window-arrange workbench)
  (lightfast:capture-window (workbench-window workbench)
                            (uiop:native-namestring
                             (merge-pathnames "aui-exercise.ppm" (uiop:temporary-directory))))
  nil)

(-> exercise--region (workbench t) (or null region))
(defun exercise--region (workbench identifier)
  "Return the region declared as IDENTIFIER by the last drawing, or NIL."
  (stage--find-region (workbench-stage workbench) identifier))

(-> exercise--click (workbench t) boolean)
(defun exercise--click (workbench identifier)
  "Click in the middle of the region IDENTIFIER. Returns T if the region exists, and NIL otherwise."
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
  "Print the result of the exercise and exit with a status that matches the result."
  (format t "~&~:[The interface exercise passed.~;~:*~D expectation~:P failed.~]~%"
          (and (plusp *exercise-failures*) *exercise-failures*))
  (finish-output)
  (sb-ext:exit :code (if (zerop *exercise-failures*) 0 1) :abort t))
