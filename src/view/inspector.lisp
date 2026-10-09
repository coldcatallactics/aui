(in-package #:aui)

;;;; -- Inspector --

;;; A side pane showing one object in full: a tool call's complete input and
;;; output, or a job with its state and the actions it allows.

(defclass inspector ()
  ((workbench      :initarg :workbench :reader inspector-workbench :type workbench)
   (group          :reader inspector-group
                   :documentation "The Lightfast group holding the pane; shown or hidden as one.")
   (title-label    :reader inspector--title-label)
   (subtitle-label :reader inspector--subtitle-label)
   (body           :reader inspector--body
                   :documentation "A mono text display with the object's full text.")
   (first-button   :reader inspector--first-button)
   (second-button  :reader inspector--second-button)
   (third-button   :reader inspector--third-button)
   (subject        :initform nil :accessor inspector-subject
                   :documentation "The TOOL-CALL block or JOB-ENTRY shown, or NIL."))
  (:documentation "The inspector pane of one workbench."))

(-> make-inspector (workbench t) inspector)
(defun make-inspector (workbench parent)
  "Create the inspector pane under PARENT for WORKBENCH, hidden and empty."
  (let* ((inspector (make-instance 'inspector :workbench workbench))
         (group (lightfast:make-group :parent parent :width 380 :height 400)))
    (setf (slot-value inspector 'group) group)
    (let ((title (workbench--make-label group "Inspector" ':prose-bold))
          (subtitle (workbench--make-label group "Choose a tool call or job to see it in full." ':prose-italic))
          (body (lightfast:make-text-display :parent group :width 380 :height 300))
          (first-button (lightfast:make-button :parent group :label "" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (inspector--first-action inspector))))
          (second-button (lightfast:make-button :parent group :label "" :height *control-height*
                                                :callback (lambda (widget event value)
                                                            (declare (ignore widget event value))
                                                            (inspector--second-action inspector))))
          (third-button (lightfast:make-button :parent group :label "" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (inspector--third-action inspector)))))
      (setf (slot-value inspector 'title-label) title
            (slot-value inspector 'subtitle-label) subtitle
            (slot-value inspector 'body) body
            (slot-value inspector 'first-button) first-button
            (slot-value inspector 'second-button) second-button
            (slot-value inspector 'third-button) third-button)
      (lightfast:set-label-align subtitle ':left ':inside ':wrap ':top)
      (lightfast:set-text-font body (font ':code))
      (lightfast:set-text-size body *code-size*)
      (lightfast:text-set-wrap body ':bounds)
      (lightfast:hide first-button)
      (lightfast:hide second-button)
      (lightfast:hide third-button))
    (lightfast:hide group)
    inspector))

(-> inspector-layout (inspector) t)
(defun inspector-layout (inspector)
  "Return the layout node placing the inspector's widgets inside its group."
  (lightfast:make-layout-column
   :target (inspector-group inspector) :basis 380 :shrink 0 :gap (spacing 3)
   :children (list (lightfast:make-layout-item (inspector--title-label inspector) :basis *control-height* :shrink 0)
                   (lightfast:make-layout-item (inspector--subtitle-label inspector) :basis 56 :shrink 0)
                   (lightfast:make-layout-item (inspector--body inspector) :basis 0 :grow 1)
                   (lightfast:make-layout-row
                    :basis *control-height* :shrink 0 :gap (spacing 3)
                    :children (list (lightfast:make-layout-item (inspector--first-button inspector) :basis 0 :grow 1)
                                    (lightfast:make-layout-item (inspector--second-button inspector) :basis 0 :grow 1)
                                    (lightfast:make-layout-item (inspector--third-button inspector) :basis 0 :grow 1))))))

(-> inspector--set-text (inspector string) null)
(defun inspector--set-text (inspector text)
  "Replace the body text."
  (let ((body (inspector--body inspector)))
    (lightfast:text-replace-styled body 0 (lightfast:text-length body) (list (cons text 0))))
  nil)

(-> inspector--buttons (inspector (or null string) (or null string) &optional (or null string)) null)
(defun inspector--buttons (inspector first-label second-label &optional third-label)
  "Label and show the action buttons, hiding those without a label."
  (flet ((set-button (button label)
           (cond
             (label
              (setf (lightfast:label button) label)
              (lightfast:show button))
             (t
              (lightfast:hide button)))))
    (set-button (inspector--first-button inspector) first-label)
    (set-button (inspector--second-button inspector) second-label)
    (set-button (inspector--third-button inspector) third-label))
  nil)

(-> inspector-show-tool-call (inspector tool-call) null)
(defun inspector-show-tool-call (inspector block)
  "Show BLOCK's full input and output."
  (setf (inspector-subject inspector) block)
  (setf (lightfast:label (inspector--title-label inspector))
        (if (plusp (length (tool-call-title block))) (tool-call-title block) (tool-call-name block)))
  (setf (lightfast:label (inspector--subtitle-label inspector))
        (format nil "~A, ~(~A~)~@[, wire status ~A~]~@[, at ~{~A~^ ~}~]"
                (tool-call-name block) (tool-call-status block) (tool-call-raw-status block)
                (tool-call-locations block)))
  (inspector--set-text inspector
                       (format nil "Input~%~%~A~%~%Output~%~%~A"
                               (or (tool-call-input block) "(none)")
                               (or (tool-call-output block) "(none yet)")))
  (inspector--buttons inspector "Copy output" nil)
  nil)

(-> inspector-show-job (inspector job-entry) null)
(defun inspector-show-job (inspector job)
  "Show JOB with its state and the actions it allows."
  (setf (inspector-subject inspector) job)
  (setf (lightfast:label (inspector--title-label inspector))
        (format nil "~A  ~(~A~)" (or (job-entry-role job) "job") (job-entry-state job)))
  (setf (lightfast:label (inspector--subtitle-label inspector))
        (format nil "Job ~A~@[ under ~A~]~@[, ~D s~]" (job-entry-id job) (job-entry-parent-id job)
                (job-entry-elapsed-seconds job)))
  (inspector--set-text inspector
                       (format nil "~@[Latest activity~%~%~A~%~%~]~@[Usage~%~%~S~%~%~]~@[Artifacts~%~%~{~A~%~}~]"
                               (job-entry-activity job) (job-entry-usage job) (job-entry-artifacts job)))
  (if (member (job-entry-state job) '(:running :pending :waiting))
      (inspector--buttons inspector "Send a message" "Cancel job" "Show transcript")
      (inspector--buttons inspector nil nil "Show transcript"))
  nil)

(-> inspector-show-job-transcript (inspector list) null)
(defun inspector-show-job-transcript (inspector transcript)
  "Show TRANSCRIPT, a plist from the job transcript request, when the inspector
still shows that job."
  (let ((subject (inspector-subject inspector)))
    (when (and (typep subject 'job-entry)
               (equal (job-entry-id subject) (getf transcript ':job-id)))
      (inspector--set-text inspector
                           (format nil "~A~@[~%~%~D more characters follow~]"
                                   (getf transcript ':content)
                                   (and (not (getf transcript ':complete-p))
                                        (getf transcript ':total)
                                        (getf transcript ':next-offset)
                                        (- (getf transcript ':total) (getf transcript ':next-offset)))))))
  nil)

(-> inspector--third-action (inspector) null)
(defun inspector--third-action (inspector)
  "Ask for the shown job's transcript."
  (let ((subject (inspector-subject inspector)))
    (when (typep subject 'job-entry)
      (backend-job-action (workbench-backend (inspector-workbench inspector)) (job-entry-id subject) ':inspect)))
  nil)

(-> inspector--first-action (inspector) null)
(defun inspector--first-action (inspector)
  "Copy a tool's output, or send a message to a job."
  (let ((subject (inspector-subject inspector)))
    (typecase subject
      (tool-call
       (lightfast:copy-text (or (tool-call-output subject) "")))
      (job-entry
       (let ((text (lightfast:input-dialog (format nil "Message for ~A" (job-entry-id subject)))))
         (when (and text (plusp (length text)))
           (backend-job-action (workbench-backend (inspector-workbench inspector))
                               (job-entry-id subject) ':send :text text))))))
  nil)

(-> inspector--second-action (inspector) null)
(defun inspector--second-action (inspector)
  "Cancel the shown job."
  (let ((subject (inspector-subject inspector)))
    (when (typep subject 'job-entry)
      (backend-job-action (workbench-backend (inspector-workbench inspector)) (job-entry-id subject) ':cancel)))
  nil)
