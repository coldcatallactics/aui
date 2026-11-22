(in-package #:aui)

;;;; -- Pictures --

(-> save-window-picture (t string) null)
(defun save-window-picture (window path)
  "Capture WINDOW at PATH, converting PNG requests from a temporary PPM."
  (let* ((png-p (string-equal (or (pathname-type path) "") "png"))
         (ppm   (if png-p (format nil "~A.ppm" path) path)))
    (lightfast:capture-window window ppm)
    (when png-p
      (handler-case
          (progn
            (uiop:run-program (if (uiop:os-macosx-p)
                                  (list "sips" "-s" "format" "png" ppm "--out" path)
                                  (list "magick" ppm path)))
            (delete-file ppm))
        (error ()
          (format *error-output* "Could not convert the screenshot to PNG. The PPM is at ~A.~%" ppm))))
    nil))
