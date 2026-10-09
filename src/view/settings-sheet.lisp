(in-package #:aui)

;;;; -- Settings Sheet --

;;; A property sheet over the settings catalog: every setting with its value
;;; and where the value came from, and an editor for the one selected.

(defclass settings-sheet ()
  ((window       :reader settings-sheet-window)
   (workbench    :initarg :workbench :reader settings-sheet-workbench :type workbench)
   (list         :reader settings-sheet--list)
   (title-label  :reader settings-sheet--title-label)
   (detail-label :reader settings-sheet--detail-label)
   (source-label :reader settings-sheet--source-label)
   (choice       :reader settings-sheet--choice)
   (input        :reader settings-sheet--input)
   (apply-button :reader settings-sheet--apply-button)
   (rows         :initform nil :accessor settings-sheet-rows
                 :documentation "The SETTING-ENTRY or NIL (a group heading) behind each list line.")
   (selected     :initform nil :accessor settings-sheet-selected
                 :documentation "The SETTING-ENTRY being edited, or NIL."))
  (:documentation "The settings window of one workbench."))

(-> settings-sheet--line (setting-entry) string)
(defun settings-sheet--line (entry)
  "Return the list line for ENTRY: label, value and source."
  (format nil "  ~A~C~A~C~@[~(~A~)~]"
          (setting-entry-label entry) #\Tab
          (setting-entry-value-text entry) #\Tab
          (setting-entry-source entry)))

(-> setting-entry-value-text (setting-entry) string)
(defun setting-entry-value-text (entry)
  "Return ENTRY's value as the sheet shows it: on and off for switches."
  (let ((value (setting-entry-value entry)))
    (case (setting-entry-type entry)
      (:boolean (if value "on" "off"))
      (t (if (null value) "" (princ-to-string value))))))

(-> make-settings-sheet (workbench &key (:width integer) (:height integer)) settings-sheet)
(defun make-settings-sheet (workbench &key (width 820) (height 640))
  "Create the settings window for WORKBENCH and return the sheet, unshown."
  (let* ((sheet (make-instance 'settings-sheet :workbench workbench))
         (window (lightfast:make-window :width width :height height
                                        :label "autolith - settings" :app-id "aui")))
    (setf (slot-value sheet 'window) window)
    (let* ((heading (workbench--make-label window "Settings" ':prose-bold))
           (list (lightfast:make-browser :parent window :height 300
                                         :column-widths '(300 260 0)))
           (title-label (workbench--make-label window "" ':prose-bold))
           (detail-label (workbench--make-label window "Choose a setting to see what it does." ':prose))
           (source-label (workbench--make-label window "" ':prose-italic))
           (choice (lightfast:make-choice :parent window :height *control-height*))
           (input (lightfast:make-input :parent window :height *control-height*))
           (apply-button (lightfast:make-button :parent window :label "Apply" :height *control-height*
                                                :callback (lambda (widget event value)
                                                            (declare (ignore widget event value))
                                                            (settings-sheet-apply sheet)))))
      (setf (slot-value sheet 'list) list
            (slot-value sheet 'title-label) title-label
            (slot-value sheet 'detail-label) detail-label
            (slot-value sheet 'source-label) source-label
            (slot-value sheet 'choice) choice
            (slot-value sheet 'input) input
            (slot-value sheet 'apply-button) apply-button)
      (lightfast:set-label-size heading 24)
      (lightfast:set-label-align detail-label ':left ':inside ':wrap ':top)
      (lightfast:set-text-font list (font ':prose))
      (lightfast:set-text-font choice (font ':prose))
      (lightfast:set-text-size choice *prose-size*)
      (lightfast:set-text-font input (font ':prose))
      (lightfast:set-text-size input *prose-size*)
      (lightfast:on list (lambda (widget event value)
                           (declare (ignore widget event value))
                           (settings-sheet--selected sheet))
                    :event lightfast:+event-change+)
      (lightfast:layout-on-resize
       window
       (lightfast:make-layout-column
        :padding (spacing 5) :gap (spacing 3)
        :children (list (lightfast:make-layout-item heading :basis 40 :shrink 0)
                        (lightfast:make-layout-item list :basis 0 :grow 1)
                        (lightfast:make-layout-item title-label :basis *control-height* :shrink 0)
                        (lightfast:make-layout-item detail-label :basis 72 :shrink 0)
                        (lightfast:make-layout-item source-label :basis *control-height* :shrink 0)
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0 :gap (spacing 3)
                         :children (list (lightfast:make-layout-item choice :basis 0 :grow 1)
                                         (lightfast:make-layout-item input :basis 0 :grow 1)
                                         (lightfast:make-layout-item apply-button :basis 120 :shrink 0))))))
      (settings-sheet-refresh sheet))
    sheet))

(-> settings-sheet-refresh (settings-sheet) null)
(defun settings-sheet-refresh (sheet)
  "Rebuild the list from the workbench's settings catalog, grouped."
  (let* ((list (settings-sheet--list sheet))
         (entries (workbench-settings (settings-sheet-workbench sheet)))
         (groups (remove-duplicates (mapcar #'setting-entry-group entries) :test #'equal :from-end t))
         (rows nil))
    (lightfast:clear list)
    (dolist (group groups)
      (lightfast:add-item list (format nil "@b~A" (or group "Other")))
      (push nil rows)
      (dolist (entry entries)
        (when (equal (setting-entry-group entry) group)
          (lightfast:add-item list (settings-sheet--line entry))
          (push entry rows))))
    (setf (settings-sheet-rows sheet) (nreverse rows))
    (when (settings-sheet-selected sheet)
      (let ((current (find (setting-entry-name (settings-sheet-selected sheet)) entries
                           :key #'setting-entry-name)))
        (when current
          (settings-sheet--show-entry sheet current)))))
  nil)

(-> settings-sheet--selected (settings-sheet) null)
(defun settings-sheet--selected (sheet)
  "Show the editor for the list line just chosen."
  (let* ((index (first (lightfast:browser-selected-indices (settings-sheet--list sheet))))
         (entry (and index (nth index (settings-sheet-rows sheet)))))
    (when entry
      (settings-sheet--show-entry sheet entry)))
  nil)

(-> settings-sheet--show-entry (settings-sheet setting-entry) null)
(defun settings-sheet--show-entry (sheet entry)
  "Fill the detail area and editor for ENTRY."
  (setf (settings-sheet-selected sheet) entry)
  (setf (lightfast:label (settings-sheet--title-label sheet))
        (format nil "~A  (~(~A~))" (setting-entry-label entry) (setting-entry-name entry)))
  (setf (lightfast:label (settings-sheet--detail-label sheet)) (setting-entry-documentation entry))
  (setf (lightfast:label (settings-sheet--source-label sheet))
        (if (setting-entry-adjustable-p entry)
            (format nil "~@[From ~(~A~). ~]~@[Scope ~(~A~).~]"
                    (setting-entry-source entry) (setting-entry-scope entry))
            (format nil "Read only~@[: ~A~]" (setting-entry-reason entry))))
  (let ((choice (settings-sheet--choice sheet))
        (input (settings-sheet--input sheet))
        (apply-button (settings-sheet--apply-button sheet)))
    (lightfast:clear choice)
    (cond
      ((not (setting-entry-adjustable-p entry))
       (lightfast:hide choice)
       (lightfast:hide input)
       (lightfast:hide apply-button))
      ((member (setting-entry-type entry) '(:boolean :choice))
       (dolist (option (if (eq (setting-entry-type entry) ':boolean)
                           '("on" "off")
                           (mapcar #'princ-to-string (setting-entry-options entry))))
         (lightfast:add-item choice option))
       (setf (lightfast:value choice) (setting-entry-value-text entry))
       (lightfast:show choice)
       (lightfast:hide input)
       (lightfast:show apply-button))
      (t
       (setf (lightfast:value input) (setting-entry-value-text entry))
       (lightfast:hide choice)
       (lightfast:show input)
       (lightfast:show apply-button))))
  nil)

(-> settings-sheet-apply (settings-sheet) null)
(defun settings-sheet-apply (sheet)
  "Send the edited value of the selected setting to the backend."
  (let ((entry (settings-sheet-selected sheet)))
    (when (and entry (setting-entry-adjustable-p entry))
      (let ((text (if (member (setting-entry-type entry) '(:boolean :choice))
                      (lightfast:value (settings-sheet--choice sheet))
                      (lightfast:value (settings-sheet--input sheet)))))
        (backend-set-setting (workbench-backend (settings-sheet-workbench sheet))
                             (setting-entry-name entry) text))))
  nil)
