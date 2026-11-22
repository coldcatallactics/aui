(in-package #:aui)

(-> portable-object-p (t) boolean)
(defun portable-object-p (value)
  "Return T if VALUE is a portable object form."
  (and (consp value) (eq (first value) ':object)))

(-> portable-get (t string &optional t) t)
(defun portable-get (object key &optional default)
  "Return the member KEY of OBJECT. Return DEFAULT if OBJECT has no such member or if the member is null."
  (if (portable-object-p object)
      (loop for (name value) on (rest object) by (partial #'nthcdr 2)
            when (equal name key)
              do (return (if (eq value ':null) default value))
            finally (return default))
      default))

(-> portable-member-p (t string) boolean)
(defun portable-member-p (object key)
  "Return T if the portable object OBJECT has a member KEY, even if the member is null."
  (and (portable-object-p object)
       (loop for (name) on (rest object) by (partial #'nthcdr 2)
               thereis (equal name key))
       t))

(-> portable-true-p (t) boolean)
(defun portable-true-p (value)
  "Return T if VALUE is a portable true value. All values other than false, null and NIL are true."
  (not (member value '(nil :false :null))))

(-> portable-items (t) list)
(defun portable-items (array)
  "Return the members of the portable array ARRAY as a list. Return NIL if ARRAY is not a portable array."
  (if (and (consp array) (eq (first array) ':array))
      (rest array)
      nil))

(-> portable-object (&rest t) list)
(defun portable-object (&rest members)
  "Return a portable object built from MEMBERS, which alternate between string keys and values.
NIL values become null."
  (cons ':object
        (loop for (name value) on members by (partial #'nthcdr 2)
              collect name
              collect (if (null value) ':null value))))

(-> portable-string (t) (or null string))
(defun portable-string (value)
  "Return VALUE if it is a nonempty string. Otherwise, return NIL."
  (and (stringp value) (plusp (length value)) value))
