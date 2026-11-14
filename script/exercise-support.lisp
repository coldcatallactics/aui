;;;; -- Exercise Support --

;;; What the exercises share: expectations that are counted, and the way to
;;; draw a window offscreen and click in it as FLTK would.

(in-package #:aui)

(defvar *exercise-failures* 0
  "How many expectations the exercise has seen fail.")

(defmacro exercise-expect (description form)
  "Evaluate FORM and report DESCRIPTION as held or failed."
  `(if ,form
       (format t "~&  ok    ~A~%" ,description)
       (progn
         (incf *exercise-failures*)
         (format t "~&  FAIL  ~A~%" ,description))))

(-> exercise--draw (workbench) null)
(defun exercise--draw (workbench)
  "Draw WORKBENCH's window offscreen, so its regions are those of its current state."
  (window-arrange workbench)
  (lightfast:capture-window (workbench-window workbench)
                            (uiop:native-namestring
                             (merge-pathnames "aui-exercise.ppm" (uiop:temporary-directory))))
  nil)

(-> exercise--region (workbench t) (or null region))
(defun exercise--region (workbench identifier)
  "Return the region the last drawing declared as IDENTIFIER, or NIL."
  (stage--find-region (workbench-stage workbench) identifier))

(-> exercise--click (workbench t) boolean)
(defun exercise--click (workbench identifier)
  "Click in the middle of the region IDENTIFIER and return whether it exists."
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
  "Say how the exercise went and leave with a status that says the same."
  (format t "~&~:[The interface exercise passed.~;~:*~D expectation~:P failed.~]~%"
          (and (plusp *exercise-failures*) *exercise-failures*))
  (finish-output)
  (sb-ext:exit :code (if (zerop *exercise-failures*) 0 1) :abort t))
