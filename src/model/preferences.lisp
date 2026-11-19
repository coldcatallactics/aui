(in-package #:aui)

;;;; -- Preferences --

;;; The settings of the window that are kept between runs, such as the width
;;; of the console. They are stored as one property list in one file in the
;;; configuration directory of the person.

(defvar *preferences-pathname* nil
  "The file in which the preferences are stored, or NIL.
If it is NIL, the preferences are only kept for this run.")

(defvar *preferences* nil
  "The preferences as a property list, after they were read or set.")

(defvar *preferences-read-p* nil
  "True if the preferences file was read in this run.")

(defvar *preferences-read-from* nil
  "The file from which the preferences were last read.
The preferences are read again if `*preferences-pathname*' changes.")

(-> preferences-default-pathname () pathname)
(defun preferences-default-pathname ()
  "Return the default file of the preferences."
  (uiop:xdg-config-home "aui" "preferences.sexp"))

(-> preferences--read () list)
(defun preferences--read ()
  "Return the preferences in the file.
Return NIL if there is no file or if the file does not contain a property list."
  (let ((pathname *preferences-pathname*))
    (and pathname
         (probe-file pathname)
         (handler-case
             (with-open-file (stream pathname)
               (let* ((*read-eval* nil)
                      (form        (read stream nil nil)))
                 (and (listp form) (evenp (length form)) form)))
           (error ()
             nil)))))

(-> preference (keyword &optional t) t)
(defun preference (name &optional default)
  "Return the preference NAME, or DEFAULT if the preference is not set."
  (unless (and *preferences-read-p* (equal *preferences-read-from* *preferences-pathname*))
    (setf *preferences*           (preferences--read)
          *preferences-read-p*    t
          *preferences-read-from* *preferences-pathname*))
  (getf *preferences* name default))

(-> (setf preference) (t keyword) t)
(defun (setf preference) (value name)
  "Set the preference NAME to VALUE. Write the preferences to the file if there is one."
  (preference name)
  (setf (getf *preferences* name) value)
  (let ((pathname *preferences-pathname*))
    (when pathname
      (handler-case
          (progn
            (ensure-directories-exist pathname)
            (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
              (let ((*print-readably* nil)
                    (*print-pretty*   nil))
                (prin1 *preferences* stream)
                (terpri stream))))
        (error ()
          nil))))
  value)
