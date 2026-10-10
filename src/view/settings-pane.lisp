(in-package #:aui)

;;;; -- Settings Pane --

;;; Autolith's settings as a pane inside the main window: groups on the left,
;;; the chosen group's settings on the right, each with its own control, its
;;; documentation and where its value came from.

(defparameter *settings-row-height* 84
  "Height of one setting row: a control line and a documentation line.")

(defclass settings-pane ()
  ((workbench     :initarg :workbench :reader settings-pane-workbench :type workbench)
   (group         :reader settings-pane-group
                  :documentation "The Lightfast group holding the pane.")
   (heading       :reader settings-pane--heading)
   (close-button  :reader settings-pane--close-button)
   (groups-list   :reader settings-pane--groups-list)
   (rows          :reader settings-pane--rows
                  :documentation "The scroll holding the rows of the selected group.")
   (row-widgets   :initform nil :accessor settings-pane--row-widgets
                  :documentation "Every widget created for the current rows, destroyed on rebuild.")
   (groups        :initform nil :accessor settings-pane-groups
                  :documentation "The group names in catalog order.")
   (selected      :initform nil :accessor settings-pane-selected-group
                  :documentation "The group whose settings are shown."))
  (:documentation "The settings pane of one workbench."))

(-> setting-entry-value-text (setting-entry) string)
(defun setting-entry-value-text (entry)
  "Return ENTRY's value as the pane shows it: on and off for switches."
  (let ((value (setting-entry-value entry)))
    (case (setting-entry-type entry)
      (:boolean (if value "on" "off"))
      (t (if (null value) "" (princ-to-string value))))))

(-> make-settings-pane (workbench t) settings-pane)
(defun make-settings-pane (workbench parent)
  "Create the settings pane under PARENT for WORKBENCH, hidden."
  (let* ((pane (make-instance 'settings-pane :workbench workbench))
         (group (lightfast:make-group :parent parent :width 600 :height 400)))
    (setf (slot-value pane 'group) group)
    (lightfast:set-box group lightfast:+box-no-box+)
    (let ((heading (workbench--make-label group "Settings" ':prose-bold))
          (close-button (lightfast:make-button :parent group :label "Close ✕" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-show-transcript workbench))))
          (groups-list (lightfast:make-browser :parent group :width 200 :height 300))
          (rows (lightfast:make-scroll :parent group :width 400 :height 300)))
      (setf (slot-value pane 'heading) heading
            (slot-value pane 'close-button) close-button
            (slot-value pane 'groups-list) groups-list
            (slot-value pane 'rows) rows)
      (lightfast:set-label-size heading *heading-size*)
      (lightfast:set-text-font groups-list (font ':prose))
      (lightfast:set-text-size groups-list *prose-size*)
      (lightfast:set-box groups-list lightfast:+box-no-box+)
      (lightfast:browser-set-scrollbars groups-list ':vertical)
      (lightfast:browser-set-line-spacing groups-list *list-line-spacing*)
      (lightfast:set-box rows lightfast:+box-no-box+)
      (lightfast:on groups-list (lambda (widget event value)
                                  (declare (ignore widget event value))
                                  (let ((index (first (lightfast:browser-selected-indices groups-list))))
                                    (when index
                                      (settings-pane-show-group pane (nth index (settings-pane-groups pane))))))
                    :event lightfast:+event-change+))
    (lightfast:hide group)
    pane))

(-> settings-pane-layout (settings-pane) t)
(defun settings-pane-layout (pane)
  "Return the layout node placing the pane's widgets inside its group."
  (lightfast:make-layout-column
   :target (settings-pane-group pane) :basis 0 :grow 1 :gap (spacing 4)
   :children (list (lightfast:make-layout-row
                    :basis *control-height* :shrink 0 :gap (spacing 4)
                    :children (list (lightfast:make-layout-item (settings-pane--heading pane) :basis 0 :grow 1)
                                    (lightfast:make-layout-item (settings-pane--close-button pane) :basis 110 :shrink 0)))
                   (lightfast:make-layout-row
                    :basis 0 :grow 1 :gap (spacing 6)
                    :children (list (lightfast:make-layout-item (settings-pane--groups-list pane) :basis 200 :shrink 0)
                                    (lightfast:make-layout-item (settings-pane--rows pane) :basis 0 :grow 1))))))

(-> settings-pane-refresh (settings-pane) null)
(defun settings-pane-refresh (pane)
  "Rebuild the group list and the rows from the workbench's settings catalog."
  (let* ((entries (workbench-settings (settings-pane-workbench pane)))
         (groups (remove-duplicates (mapcar (lambda (entry) (or (setting-entry-group entry) "other")) entries)
                                    :test #'equal :from-end t))
         (list (settings-pane--groups-list pane)))
    (setf (settings-pane-groups pane) groups)
    (lightfast:clear list)
    (dolist (name groups)
      (lightfast:add-item list (format nil "~A~A" (string-upcase (subseq name 0 1)) (subseq name 1))))
    (unless (member (settings-pane-selected-group pane) groups :test #'equal)
      (setf (settings-pane-selected-group pane) (first groups)))
    (let ((position (position (settings-pane-selected-group pane) groups :test #'equal)))
      (when position
        (lightfast:browser-select list position)))
    (settings-pane--build-rows pane))
  nil)

(-> settings-pane-show-group (settings-pane (or null string)) null)
(defun settings-pane-show-group (pane name)
  "Show the settings of group NAME."
  (setf (settings-pane-selected-group pane) name)
  (settings-pane--build-rows pane)
  nil)

(-> settings-pane-show-entry (settings-pane setting-entry) null)
(defun settings-pane-show-entry (pane entry)
  "Show the group holding ENTRY."
  (settings-pane-show-group pane (or (setting-entry-group entry) "other"))
  (let ((position (position (settings-pane-selected-group pane) (settings-pane-groups pane) :test #'equal)))
    (when position
      (lightfast:browser-select (settings-pane--groups-list pane) position)))
  nil)

(-> settings-pane--build-rows (settings-pane) null)
(defun settings-pane--build-rows (pane)
  "Create one row per setting of the selected group inside the scroll."
  (dolist (widget (settings-pane--row-widgets pane))
    (lightfast:destroy widget))
  (setf (settings-pane--row-widgets pane) nil)
  (let* ((scroll (settings-pane--rows pane))
         (entries (remove-if-not (lambda (entry)
                                   (equal (or (setting-entry-group entry) "other")
                                          (settings-pane-selected-group pane)))
                                 (workbench-settings (settings-pane-workbench pane))))
         (width (max 480 (progn (lightfast:refresh-geometry scroll) (lightfast:widget-width scroll))))
         (label-width 260)
         (control-x 272)
         (control-width (max 200 (min 480 (- width control-x 24))))
         (source-x (+ control-x control-width 16))
         (y 0))
    (dolist (entry entries)
      (let ((widgets (settings-pane--make-row pane scroll entry y label-width control-x control-width source-x width)))
        (setf (settings-pane--row-widgets pane) (append (settings-pane--row-widgets pane) widgets)))
      (incf y *settings-row-height*))
    (lightfast:redraw scroll))
  nil)

(-> settings-pane--make-row
    (settings-pane t setting-entry integer integer integer integer integer integer) list)
(defun settings-pane--make-row (pane scroll entry y label-width control-x control-width source-x width)
  "Create the widgets of one setting row at Y inside SCROLL and return them."
  (let* ((backend (workbench-backend (settings-pane-workbench pane)))
         (name (setting-entry-name entry))
         (label (lightfast:make-label :parent scroll :x 0 :y y :width label-width :height *control-height*
                                      :label (setting-entry-label entry)))
         (documentation (lightfast:make-label :parent scroll :x 0 :y (+ y *control-height*)
                                              :width (- width 8) :height (- *settings-row-height* *control-height*)
                                              :label (format nil "~A~@[  ·  ~(~A~)~]" (setting-entry-documentation entry)
                                                             (setting-entry-source entry))))
         (source (lightfast:make-label :parent scroll :x source-x :y y :width (- width source-x) :height *control-height*
                                       :label (if (setting-entry-adjustable-p entry)
                                                  ""
                                                  (format nil "read only~@[: ~A~]" (setting-entry-reason entry)))))
         (control
           (cond
             ((not (setting-entry-adjustable-p entry))
              (lightfast:make-label :parent scroll :x control-x :y y :width control-width :height *control-height*
                                    :label (setting-entry-value-text entry)))
             ((eq (setting-entry-type entry) ':boolean)
              (let ((check (lightfast:make-check-button
                            :parent scroll :x control-x :y y :width control-width :height *control-height*
                            :label (if (setting-entry-value entry) "on" "off")
                            :callback (lambda (widget event value)
                                        (declare (ignore event))
                                        (backend-set-setting backend name (if (string= value "0") "off" "on"))
                                        (setf (lightfast:label widget) (if (string= value "0") "off" "on"))))))
                (setf (lightfast:value check) (if (setting-entry-value entry) "1" "0"))
                check))
             ((eq (setting-entry-type entry) ':choice)
              (let ((choice (lightfast:make-choice
                             :parent scroll :x control-x :y y :width control-width :height *control-height*
                             :items (mapcar #'princ-to-string (setting-entry-options entry))
                             :callback (lambda (widget event value)
                                         (declare (ignore widget event))
                                         (backend-set-setting backend name value)))))
                (setf (lightfast:value choice) (setting-entry-value-text entry))
                choice))
             (t
              (let ((input (lightfast:make-input
                            :parent scroll :x control-x :y y :width control-width :height *control-height*
                            :value (setting-entry-value-text entry))))
                (lightfast:set-when input ':enter-key)
                (lightfast:on input (lambda (widget event value)
                                      (declare (ignore widget event))
                                      (backend-set-setting backend name value))
                              :event lightfast:+event-activate+)
                input)))))
    (lightfast:set-label-align label ':left ':inside ':clip)
    (lightfast:set-label-font label (font ':prose-bold))
    (lightfast:set-label-size label *prose-size*)
    (lightfast:set-label-align documentation ':left ':inside ':clip)
    (lightfast:set-label-font documentation (font ':prose-italic))
    (lightfast:set-label-size documentation *small-size*)
    (lightfast:set-label-align source ':left ':inside ':clip)
    (lightfast:set-label-font source (font ':prose-italic))
    (lightfast:set-label-size source *meta-size*)
    (when (not (setting-entry-adjustable-p entry))
      (lightfast:set-label-align control ':left ':inside ':clip)
      (lightfast:set-label-font control (font ':prose))
      (lightfast:set-label-size control *prose-size*))
    (when (member (setting-entry-type entry) '(:choice :string :integer :pathname))
      (when (setting-entry-adjustable-p entry)
        (lightfast:set-text-font control (font ':prose))
        (lightfast:set-text-size control *prose-size*)))
    (list label documentation source control)))
