(in-package #:aui)

;;;; -- Welcome --

;;; The first window: choose a folder, or return to recent work.

(defclass welcome ()
  ((window             :reader welcome-window)
   (path-input         :reader welcome--path-input)
   (conversations-list :reader welcome--conversations-list)
   (folders-list       :reader welcome--folders-list)
   (entries            :initarg :entries :reader welcome-entries
                       :documentation "The CONVERSATION-ENTRYs listed, most recent first.")
   (folders            :reader welcome-folders
                       :documentation "(DIRECTORY . LAST-ACTIVITY) pairs listed, most recent first.")
   (on-open            :initarg :on-open :reader welcome-on-open
                       :documentation "Called with DIRECTORY and CONVERSATION-ID or NIL when the user chooses."))
  (:documentation "The folder and conversation chooser shown before any backend starts."))

(-> welcome--heading (t string) t)
(defun welcome--heading (parent text)
  "Create a bold left-aligned heading under PARENT."
  (let ((label (lightfast:make-label :parent parent :label text :height *control-height*)))
    (lightfast:set-label-align label ':left ':inside)
    (lightfast:set-label-font label (font ':prose-bold))
    (lightfast:set-label-size label *prose-size*)
    label))

(-> age-text (integer integer) string)
(defun age-text (then now)
  "Return how long ago THEN was relative to NOW, in words."
  (let ((seconds (max 0 (- now then))))
    (cond
      ((< seconds 60) "just now")
      ((< seconds 3600) (format nil "~D min ago" (floor seconds 60)))
      ((< seconds 86400) (format nil "~D h ago" (floor seconds 3600)))
      ((< seconds (* 30 86400)) (format nil "~D d ago" (floor seconds 86400)))
      (t (multiple-value-bind (second minute hour day month year) (decode-universal-time then)
           (declare (ignore second minute hour))
           (format nil "~D-~2,'0D-~2,'0D" year month day))))))

(-> conversation-entry-line (conversation-entry integer) string)
(defun conversation-entry-line (entry now)
  "Return the welcome list line for ENTRY."
  (format nil "~A~C~A~C~@[~A~]~C~A"
          (conversation-entry-name entry) #\Tab
          (conversation-entry-directory entry) #\Tab
          (conversation-entry-model entry) #\Tab
          (age-text (conversation-entry-activity-key entry) now)))

(-> make-welcome (&key (:entries list) (:on-open function) (:width integer) (:height integer)) welcome)
(defun make-welcome (&key entries on-open (width 960) (height 640))
  "Create and return the welcome window; call SHOW on WELCOME-WINDOW to open it."
  (let* ((welcome (make-instance 'welcome :entries entries :on-open on-open))
         (window (lightfast:make-window :width width :height height :label "autolith" :app-id "aui"))
         (now (get-universal-time)))
    (setf (slot-value welcome 'window) window
          (slot-value welcome 'folders) (recent-directories entries))
    (lightfast:window-escape-closes window nil)
    (let* ((wordmark (lightfast:make-label :parent window :label "Autolith" :height 44))
           (lead (lightfast:make-label :parent window
                                       :label "Choose a folder to start a conversation there, or return to recent work."
                                       :height *control-height*))
           (path-input (lightfast:make-input :parent window :height *control-height*
                                             :value (namestring (uiop:getcwd))))
           (browse (lightfast:make-button :parent window :label "Browse" :height *control-height*
                                          :callback (lambda (widget event value)
                                                      (declare (ignore widget event value))
                                                      (welcome--browse welcome))))
           (start (lightfast:make-button :parent window :label "Start here" :height *control-height*
                                         :callback (lambda (widget event value)
                                                     (declare (ignore widget event value))
                                                     (welcome--start welcome))))
           (conversations-heading (welcome--heading window "Recent conversations"))
           (conversations-list (lightfast:make-browser :parent window :height 200
                                                       :column-widths '(420 300 200 0)))
           (resume (lightfast:make-button :parent window :label "Resume ▸" :height *control-height*
                                          :callback (lambda (widget event value)
                                                      (declare (ignore widget event value))
                                                      (welcome--resume welcome))))
           (folders-heading (welcome--heading window "Recent folders"))
           (folders-list (lightfast:make-browser :parent window :height 120
                                                 :column-widths '(720 0))))
      (setf (slot-value welcome 'path-input) path-input
            (slot-value welcome 'conversations-list) conversations-list
            (slot-value welcome 'folders-list) folders-list)
      (lightfast:set-label-align wordmark ':left ':inside)
      (lightfast:set-label-font wordmark (font ':prose))
      (lightfast:set-label-size wordmark *display-size*)
      (lightfast:set-label-align lead ':left ':inside)
      (lightfast:set-label-font lead (font ':prose-italic))
      (lightfast:set-label-size lead *prose-size*)
      (lightfast:set-text-font path-input (font ':prose))
      (lightfast:set-text-size path-input *prose-size*)
      (dolist (list (list conversations-list folders-list))
        (lightfast:set-text-font list (font ':prose))
        (lightfast:set-text-size list *prose-size*)
        (lightfast:set-box list lightfast:+box-no-box+))
      (lightfast:set-when path-input ':enter-key)
      (lightfast:on path-input (lambda (widget event value)
                                 (declare (ignore widget event value))
                                 (welcome--start welcome))
                    :event lightfast:+event-activate+)
      (dolist (entry entries)
        (lightfast:add-item conversations-list (conversation-entry-line entry now)))
      (dolist (pair (welcome-folders welcome))
        (lightfast:add-item folders-list (format nil "~A~C~A" (first pair) #\Tab (age-text (rest pair) now))))
      (when entries
        (lightfast:browser-select conversations-list 0))
      (lightfast:on folders-list (lambda (widget event value)
                                   (declare (ignore widget event value))
                                   (welcome--folder-chosen welcome))
                    :event lightfast:+event-change+)
      (lightfast:on conversations-list (lambda (widget event value)
                                         (declare (ignore widget event value))
                                         (when (plusp (lightfast:event-clicks))
                                           (welcome--resume welcome)))
                    :event lightfast:+event-change+)
      (lightfast:layout-on-resize
       window
       (lightfast:make-layout-column
        :padding (list (spacing 6) (spacing 5) (spacing 6) (spacing 5)) :gap (spacing 4)
        :children (list (lightfast:make-layout-item wordmark :basis 48 :shrink 0)
                        (lightfast:make-layout-item lead :basis *control-height* :shrink 0)
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0 :gap (spacing 4) :max-width 900
                         :children (list (lightfast:make-layout-item path-input :basis 0 :grow 1)
                                         (lightfast:make-layout-item browse :basis 120 :shrink 0)
                                         (lightfast:make-layout-item start :basis 140 :shrink 0)))
                        (lightfast:make-layout-item conversations-heading :basis (+ *control-height* (spacing 5)) :shrink 0)
                        (lightfast:make-layout-item conversations-list :basis 0 :grow 3)
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0 :justify :start
                         :children (list (lightfast:make-layout-item resume :basis 140 :shrink 0)))
                        (lightfast:make-layout-item folders-heading :basis (+ *control-height* (spacing 5)) :shrink 0)
                        (lightfast:make-layout-item folders-list :basis 0 :grow 2)))))
    welcome))

(-> welcome--browse (welcome) null)
(defun welcome--browse (welcome)
  "Pick a folder with the native chooser into the path field."
  (let ((directory (lightfast:choose-directory :title "Choose a folder for Autolith"
                                               :preset-path (lightfast:value (welcome--path-input welcome)))))
    (when (and directory (plusp (length directory)))
      (setf (lightfast:value (welcome--path-input welcome)) directory)))
  nil)

(-> welcome--start (welcome) null)
(defun welcome--start (welcome)
  "Open a new conversation in the folder named by the path field."
  (let ((directory (string-trim " " (lightfast:value (welcome--path-input welcome)))))
    (cond
      ((and (plusp (length directory)) (uiop:directory-exists-p directory))
       (funcall (welcome-on-open welcome) (namestring (uiop:ensure-directory-pathname directory)) nil))
      (t
       (lightfast:alert-box (format nil "~A is not a folder." directory)))))
  nil)

(-> welcome--resume (welcome) null)
(defun welcome--resume (welcome)
  "Open the selected recent conversation."
  (let* ((index (first (lightfast:browser-selected-indices (welcome--conversations-list welcome))))
         (entry (and index (nth index (welcome-entries welcome)))))
    (when entry
      (funcall (welcome-on-open welcome) (conversation-entry-directory entry) (conversation-entry-id entry))))
  nil)

(-> welcome--folder-chosen (welcome) null)
(defun welcome--folder-chosen (welcome)
  "Copy the selected recent folder into the path field."
  (let* ((index (first (lightfast:browser-selected-indices (welcome--folders-list welcome))))
         (pair (and index (nth index (welcome-folders welcome)))))
    (when pair
      (setf (lightfast:value (welcome--path-input welcome)) (first pair))))
  nil)
