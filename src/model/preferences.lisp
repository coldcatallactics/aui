(in-package #:aui)

;;;; -- Preferences --

;;; The few things the window remembers between runs, such as how wide the
;;; console was left. They are one property list in one file under the
;;; person's configuration directory. Autolith's own settings stay Autolith's.

(defvar *preferences-pathname* nil
  "The file the preferences are kept in, or NIL to keep them for this run only.")

(defvar *preferences* nil
  "The preferences as a property list, once they were read or set.")

(defvar *preferences-read-p* nil
  "Whether the preferences file was read in this run.")

(-> preferences-default-pathname () pathname)
(defun preferences-default-pathname ()
  "Return where the preferences are kept when nothing says otherwise."
  (uiop:xdg-config-home "aui" "preferences.sexp"))

(-> preferences--read () list)
(defun preferences--read ()
  "Return the preferences in the file, or NIL when there is no file or it does not hold a property list."
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
  "Return the preference NAME, or DEFAULT when it was never set."
  (unless *preferences-read-p*
    (setf *preferences*        (preferences--read)
          *preferences-read-p* t))
  (getf *preferences* name default))

(-> (setf preference) (t keyword) t)
(defun (setf preference) (value name)
  "Set the preference NAME to VALUE and write the preferences to their file when they have one."
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
