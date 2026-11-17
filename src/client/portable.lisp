(in-package #:aui)

;;;; -- Portable Values --

;;; Autolith's structured client protocol carries JSON-shaped data as readable
;;; forms: an object is (:OBJECT KEY VALUE ...), an array (:ARRAY VALUE ...),
;;; false :FALSE and null :NULL. These read and build such forms.

(-> portable-object-p (t) boolean)
(defun portable-object-p (value)
  "Return T when VALUE is a portable object form."
  (and (consp value) (eq (first value) ':object)))

(-> portable-get (t string &optional t) t)
(defun portable-get (object key &optional default)
  "Return OBJECT's member KEY, or DEFAULT when OBJECT has none or holds null there."
  (if (portable-object-p object)
      (loop for (name value) on (rest object) by (partial #'nthcdr 2)
            when (equal name key)
              do (return (if (eq value ':null) default value))
            finally (return default))
      default))

(-> portable-member-p (t string) boolean)
(defun portable-member-p (object key)
  "Return T when the portable object OBJECT has a member KEY, whatever that member holds, null included."
  (and (portable-object-p object)
       (loop for (name) on (rest object) by (partial #'nthcdr 2)
               thereis (equal name key))
       t))

(-> portable-true-p (t) boolean)
(defun portable-true-p (value)
  "Return T when VALUE is a portable true: anything but false, null and NIL."
  (not (member value '(nil :false :null))))

(-> portable-items (t) list)
(defun portable-items (array)
  "Return the members of the portable array form ARRAY as a list, or NIL when it is not one."
  (if (and (consp array) (eq (first array) ':array))
      (rest array)
      nil))

(-> portable-object (&rest t) list)
(defun portable-object (&rest members)
  "Return a portable object of MEMBERS, alternating string keys and values; a NIL value is sent as null."
  (cons ':object
        (loop for (name value) on members by (partial #'nthcdr 2)
              collect name
              collect (if (null value) ':null value))))

(-> portable-string (t) (or null string))
(defun portable-string (value)
  "Return VALUE when it is a nonempty string, else NIL."
  (and (stringp value) (plusp (length value)) value))
