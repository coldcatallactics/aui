(in-package #:aui)

;;;; -- Workbench --

;;; The main window: header, rail, transcript, composer and status line, all
;;; driven by UI events from one backend. Everything here runs on the FLTK
;;; thread.

(defparameter *rail-breakpoint* 1100
  "Window width in pixels below which the rail is hidden.")

(defparameter *inspector-breakpoint* 1500
  "Window width in pixels below which an open inspector replaces the rail.")

(defparameter *header-height* 36
  "Height of the header row in pixels.")

(defparameter *composer-height* 112
  "Height of the composer row in pixels.")

(defparameter *status-height* 28
  "Height of the status row in pixels.")

(defparameter *control-height* 32
  "Height of buttons and single-line fields.")

(defclass workbench ()
  ((window              :reader workbench-window)
   (backend             :initarg :backend :reader workbench-backend :type backend)
   (transcript          :initform (make-transcript) :reader workbench-transcript :type transcript)
   (transcript-view     :reader workbench-transcript-view)
   (folder-label        :reader workbench--folder-label)
   (title-label         :reader workbench--title-label)
   (model-label         :reader workbench--model-label)
   (permission-label    :reader workbench--permission-label)
   (state-label         :reader workbench--state-label)
   (rail                :reader workbench--rail)
   (conversations-label :reader workbench--conversations-label)
   (conversations-list  :reader workbench--conversations-list)
   (jobs-label          :reader workbench--jobs-label)
   (jobs-list           :reader workbench--jobs-list)
   (approval-row        :reader workbench--approval-row)
   (approval-label      :reader workbench--approval-label)
   (approval-buttons    :initform nil :accessor workbench--approval-buttons)
   (composer            :reader workbench-composer)
   (send-button         :reader workbench--send-button)
   (stop-button         :reader workbench--stop-button)
   (status-label        :reader workbench--status-label)
   (commands-button     :reader workbench--commands-button)
   (settings-button     :reader workbench--settings-button)
   (inspector           :reader workbench-inspector)
   (inspector-visible-p :initform nil :accessor workbench-inspector-visible-p :type boolean)
   (sheet               :initform nil :accessor workbench--sheet
                        :documentation "The SETTINGS-SHEET once opened, or NIL.")
   (palette             :initform nil :accessor workbench--palette
                        :documentation "The PALETTE once opened, or NIL.")
   (settings            :initform nil :accessor workbench-settings
                        :documentation "The SETTING-ENTRYs of the latest settings catalog.")
   (models              :initform nil :accessor workbench-models
                        :documentation "The MODEL-ENTRYs of the latest models catalog.")
   (operations          :initform nil :accessor workbench-operations
                        :documentation "The OPERATION-ENTRYs of the latest operations catalog.")
   (conversations       :initform nil :accessor workbench-conversations
                        :documentation "The CONVERSATION-ENTRYs shown in the rail, in list order.")
   (jobs                :initform nil :accessor workbench-jobs
                        :documentation "The JOB-ENTRYs shown in the rail, in list order.")
   (pending-approval    :initform nil :accessor workbench-pending-approval
                        :documentation "The APPROVAL block awaiting the user, or NIL.")
   (connection-state    :initform ':starting :accessor workbench-connection-state)
   (session-state       :initform ':closed :accessor workbench-session-state)
   (turn-state          :initform ':idle :accessor workbench-turn-state)
   (snapshot            :initform nil :accessor workbench-snapshot
                        :documentation "The latest STATE-SNAPSHOT, or NIL.")
   (closed-p            :initform nil :accessor workbench-closed-p))
  (:documentation "One workspace window bound to one backend."))

(defvar *workbenches* nil
  "Every open workbench, so events can be routed by backend id.")

(-> find-workbench (t) (or null workbench))
(defun find-workbench (backend-id)
  "Return the workbench whose backend has BACKEND-ID, or NIL."
  (find backend-id *workbenches* :key (lambda (workbench) (backend-id (workbench-backend workbench)))
                                 :test #'equal))


;;; Construction

(-> workbench--make-label (t string keyword) t)
(defun workbench--make-label (parent text face)
  "Create a left-aligned label under PARENT in FACE."
  (let ((label (lightfast:make-label :parent parent :label text :height *control-height*)))
    (lightfast:set-label-align label ':left ':inside ':clip)
    (lightfast:set-label-font label (font face))
    (lightfast:set-label-size label *prose-size*)
    label))

(-> make-workbench (backend &key (:width integer) (:height integer)) workbench)
(defun make-workbench (backend &key (width 1280) (height 860))
  "Create the window for BACKEND, register it, and return the workbench.
The window is created but not shown; the caller shows it after starting the
backend so the first events have somewhere to land."
  (let* ((workbench (make-instance 'workbench :backend backend))
         (window (lightfast:make-window :width width :height height
                                        :label (format nil "autolith - ~A" (backend-directory backend))
                                        :app-id "aui")))
    (setf (slot-value workbench 'window) window)
    (lightfast:window-escape-closes window nil)
    (with-slots (folder-label title-label model-label permission-label state-label
                 rail conversations-label conversations-list jobs-label jobs-list
                 approval-row approval-label composer send-button stop-button status-label
                 transcript-view)
        workbench
      (setf folder-label (workbench--make-label window (backend-directory backend) ':prose-italic)
            title-label (workbench--make-label window "New conversation" ':prose)
            model-label (workbench--make-label window "" ':prose-italic)
            permission-label (workbench--make-label window "" ':prose-italic)
            state-label (workbench--make-label window "starting" ':prose-italic))
      (lightfast:set-label-align title-label ':left ':inside ':clip)
      (lightfast:set-label-align state-label ':right ':inside)
      (setf (slot-value workbench 'commands-button)
            (lightfast:make-button :parent window :label "Commands" :height *control-height*
                                   :callback (lambda (widget event value)
                                               (declare (ignore widget event value))
                                               (workbench-show-palette workbench)))
            (slot-value workbench 'settings-button)
            (lightfast:make-button :parent window :label "Settings" :height *control-height*
                                   :callback (lambda (widget event value)
                                               (declare (ignore widget event value))
                                               (workbench-show-settings workbench))))
      (lightfast:set-tooltip (slot-value workbench 'commands-button) "Ctrl+K")
      (lightfast:set-tooltip (slot-value workbench 'settings-button) "Ctrl+,")
      (setf rail (lightfast:make-group :parent window :width 240 :height 400))
      (setf conversations-label (workbench--make-label rail "Conversations here" ':prose-bold)
            conversations-list (lightfast:make-browser :parent rail :width 240 :height 200)
            jobs-label (workbench--make-label rail "Running work" ':prose-bold)
            jobs-list (lightfast:make-browser :parent rail :width 240 :height 160))
      (lightfast:set-text-font conversations-list (font ':prose))
      (lightfast:set-text-font jobs-list (font ':prose))
      (lightfast:on conversations-list
                    (lambda (widget event value)
                      (declare (ignore widget event value))
                      (workbench--conversation-chosen workbench))
                    :event lightfast:+event-change+)
      (lightfast:on jobs-list
                    (lambda (widget event value)
                      (declare (ignore widget event value))
                      (workbench--job-chosen workbench))
                    :event lightfast:+event-change+)
      (setf transcript-view (make-transcript-view (workbench-transcript workbench)
                                                  :parent window :width 600 :height 400))
      (setf (transcript-view-on-context transcript-view)
            (lambda (block-id) (workbench--block-menu workbench block-id)))
      (setf (slot-value workbench 'inspector) (make-inspector workbench window))
      (setf approval-row (lightfast:make-group :parent window :width 600 :height *control-height*))
      (setf approval-label (workbench--make-label approval-row "" ':prose-bold))
      (setf composer (lightfast:make-text-editor :parent window :width 600 :height *composer-height*))
      (lightfast:set-text-font composer (font ':prose))
      (lightfast:set-text-size composer *prose-size*)
      (lightfast:text-set-wrap composer ':bounds)
      (lightfast:on composer
                    (lambda (widget event value)
                      (declare (ignore widget event))
                      (workbench--composer-key workbench value))
                    :event lightfast:+event-key+)
      (setf send-button (lightfast:make-button :parent window :label "Send" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-send workbench)))
            stop-button (lightfast:make-button :parent window :label "Stop" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-stop workbench))))
      (setf status-label (workbench--make-label window "" ':prose-italic))
      (lightfast:set-label-size status-label (- *prose-size* 1))
      (lightfast:set-label-size folder-label (- *prose-size* 1))
      (lightfast:set-label-align status-label ':right ':inside ':clip))
    (lightfast:on window (lambda (widget event value)
                           (declare (ignore widget event value))
                           (workbench-close workbench))
                  :event lightfast:+event-close+)
    (lightfast:on window (lambda (widget event value)
                           (declare (ignore widget event))
                           (workbench--window-key workbench value))
                  :event lightfast:+event-key+)
    (lightfast:on-resize window (lambda (widget)
                                  (declare (ignore widget))
                                  (workbench--relayout workbench)))
    (workbench--relayout workbench)
    (workbench--refresh-status workbench)
    (push workbench *workbenches*)
    workbench))

(-> workbench--relayout (workbench) null)
(defun workbench--relayout (workbench)
  "Lay the window out for its current size, hiding the rail when narrow."
  (let* ((window (workbench-window workbench))
         (width (progn (lightfast:refresh-geometry window) (lightfast:widget-width window)))
         (height (lightfast:widget-height window))
         (inspector-p (workbench-inspector-visible-p workbench))
         (rail-p (and (>= width *rail-breakpoint*)
                      (or (not inspector-p) (>= width *inspector-breakpoint*))))
         (approval-p (and (workbench-pending-approval workbench) t))
         (gap (spacing 3))
         (margin (spacing 5))
         (rail (workbench--rail workbench))
         (approval-row (workbench--approval-row workbench)))
    (if rail-p (lightfast:show rail) (lightfast:hide rail))
    (if approval-p (lightfast:show approval-row) (lightfast:hide approval-row))
    (dolist (chip (list (workbench--model-label workbench) (workbench--permission-label workbench)))
      (if (>= width *rail-breakpoint*) (lightfast:show chip) (lightfast:hide chip)))
    (if inspector-p
        (lightfast:show (inspector-group (workbench-inspector workbench)))
        (lightfast:hide (inspector-group (workbench-inspector workbench))))
    (let* ((header (lightfast:make-layout-row
                    :basis *header-height* :shrink 0 :gap (spacing 4)
                    :children (append
                               (list (lightfast:make-layout-item (workbench--title-label workbench) :basis 0 :grow 1 :min-width 200))
                               (when (>= width *rail-breakpoint*)
                                 (list (lightfast:make-layout-item (workbench--model-label workbench) :basis 200 :shrink 1)
                                       (lightfast:make-layout-item (workbench--permission-label workbench) :basis 90 :shrink 1)))
                               (list (lightfast:make-layout-item (workbench--state-label workbench) :basis 110 :shrink 0)
                                     (lightfast:make-layout-item (workbench--commands-button workbench) :basis 110 :shrink 0)
                                     (lightfast:make-layout-item (workbench--settings-button workbench) :basis 100 :shrink 0)))))
           (rail-layout (lightfast:make-layout-column
                         :target rail :basis 240 :shrink 0 :gap gap
                         :children (list (lightfast:make-layout-item (workbench--conversations-label workbench)
                                                                     :basis *control-height* :shrink 0)
                                         (lightfast:make-layout-item (workbench--conversations-list workbench)
                                                                     :basis 0 :grow 3)
                                         (lightfast:make-layout-item (workbench--jobs-label workbench)
                                                                     :basis *control-height* :shrink 0)
                                         (lightfast:make-layout-item (workbench--jobs-list workbench)
                                                                     :basis 0 :grow 2))))
           (approval-layout (lightfast:make-layout-row
                             :target approval-row :basis *control-height* :shrink 0 :gap gap
                             :children (cons (lightfast:make-layout-item (workbench--approval-label workbench)
                                                                         :basis 0 :grow 1)
                                             (mapcar (lambda (button)
                                                       (lightfast:make-layout-item button :basis 200 :shrink 1))
                                                     (workbench--approval-buttons workbench)))))
           (composer-row (lightfast:make-layout-row
                          :basis *composer-height* :shrink 0 :gap gap
                          :children (list (lightfast:make-layout-item (workbench-composer workbench) :basis 0 :grow 1)
                                          (lightfast:make-layout-column
                                           :basis 112 :shrink 0 :gap gap
                                           :children (list (lightfast:make-layout-item (workbench--send-button workbench)
                                                                                       :basis *control-height* :shrink 0)
                                                           (lightfast:make-layout-item (workbench--stop-button workbench)
                                                                                       :basis *control-height* :shrink 0))))))
           (center (lightfast:make-layout-column
                    :basis 0 :grow 1 :gap gap
                    :children (append (list (lightfast:make-layout-item
                                             (transcript-view-display (workbench-transcript-view workbench))
                                             :basis 0 :grow 1))
                                      (when approval-p (list approval-layout))
                                      (list composer-row))))
           (body (lightfast:make-layout-row
                  :basis 0 :grow 1 :gap (spacing 5)
                  :children (append (when rail-p (list rail-layout))
                                    (list center)
                                    (when inspector-p (list (inspector-layout (workbench-inspector workbench)))))))
           (layout (lightfast:make-layout-column
                    :padding (list margin (spacing 3) margin (spacing 3)) :gap gap
                    :children (list header body
                                    (lightfast:make-layout-row
                                     :basis *status-height* :shrink 0 :gap (spacing 4)
                                     :children (list (lightfast:make-layout-item (workbench--folder-label workbench)
                                                                                 :basis 0 :grow 1)
                                                     (lightfast:make-layout-item (workbench--status-label workbench)
                                                                                 :basis 0 :grow 2)))))))
      (lightfast:apply-layout layout (lightfast:make-rect :width width :height height) :parent window)))
  nil)


;;; Actions

(-> workbench-send (workbench) null)
(defun workbench-send (workbench)
  "Send the composer text as a prompt and clear the composer."
  (let ((text (string-trim '(#\Space #\Newline #\Tab) (lightfast:value (workbench-composer workbench)))))
    (when (and (plusp (length text))
               (eq (workbench-turn-state workbench) ':idle)
               (eq (workbench-session-state workbench) ':ready))
      (backend-prompt (workbench-backend workbench) text)
      (setf (lightfast:value (workbench-composer workbench)) "")))
  nil)

(-> workbench-stop (workbench) null)
(defun workbench-stop (workbench)
  "Cancel the running turn."
  (when (member (workbench-turn-state workbench) '(:running :cancelling))
    (backend-cancel (workbench-backend workbench)))
  nil)

(defparameter *key-enter* #xff0d
  "FLTK's key number for Enter.")

(defparameter *key-keypad-enter* #xff8d
  "FLTK's key number for the keypad Enter.")

(defparameter *key-escape* #xff1b
  "FLTK's key number for Escape.")

(defparameter *state-control* #x40000
  "FLTK's state bit for Ctrl.")

(-> workbench--composer-key (workbench string) null)
(defun workbench--composer-key (workbench value)
  "Send on Ctrl+Enter, stop on Escape, decide approvals on Ctrl+1 to Ctrl+9."
  (destructuring-bind (key state &rest rest) (parse-integers value)
    (declare (ignore rest))
    (let ((control-p (logtest state *state-control*)))
      (cond
        ((and control-p (or (eql key *key-enter*) (eql key *key-keypad-enter*)))
         (lightfast:consume-event)
         (workbench-send workbench))
        ((eql key *key-escape*)
         (lightfast:consume-event)
         (workbench-stop workbench))
        ((and control-p (<= (char-code #\1) key (char-code #\9)))
         (lightfast:consume-event)
         (workbench-decide workbench (- key (char-code #\0))))
        ((workbench--global-key workbench key control-p)
         (lightfast:consume-event)))))
  nil)

(-> workbench--global-key (workbench integer boolean) boolean)
(defun workbench--global-key (workbench key control-p)
  "Act on a shortcut that works anywhere in the window and return T when it did."
  (cond
    ((and control-p (eql key (char-code #\k)))
     (workbench-show-palette workbench)
     t)
    ((and control-p (eql key (char-code #\,)))
     (workbench-show-settings workbench)
     t)
    ((and control-p (eql key (char-code #\i)))
     (workbench-toggle-inspector workbench)
     t)
    (t
     nil)))

(-> workbench-toggle-inspector (workbench) null)
(defun workbench-toggle-inspector (workbench)
  "Show or hide the inspector pane."
  (setf (workbench-inspector-visible-p workbench) (not (workbench-inspector-visible-p workbench)))
  (workbench--relayout workbench)
  nil)

(-> workbench-inspect-block (workbench string) null)
(defun workbench-inspect-block (workbench block-id)
  "Open the inspector on the tool call BLOCK-ID."
  (let ((block (transcript-find (workbench-transcript workbench) block-id)))
    (when (typep block 'tool-call)
      (inspector-show-tool-call (workbench-inspector workbench) block)
      (setf (workbench-inspector-visible-p workbench) t)
      (workbench--relayout workbench)))
  nil)

(-> workbench--block-menu (workbench string) null)
(defun workbench--block-menu (workbench block-id)
  "Offer the actions for the block BLOCK-ID under the pointer."
  (let* ((block (transcript-find (workbench-transcript workbench) block-id))
         (view (workbench-transcript-view workbench))
         (items (append (list (if (transcript-block-collapsed-p block) "Expand" "Collapse"))
                        (when (typep block 'tool-call) (list "Inspect"))
                        (list "Copy text")))
         (choice (lightfast:popup-menu items)))
    (when choice
      (let ((item (nth choice items)))
        (cond
          ((member item '("Expand" "Collapse") :test #'string=)
           (transcript-view-toggle view block-id))
          ((string= item "Inspect")
           (workbench-inspect-block workbench block-id))
          ((string= item "Copy text")
           (lightfast:copy-text (transcript-view-block-text view block-id)))))))
  nil)

(-> workbench--job-chosen (workbench) null)
(defun workbench--job-chosen (workbench)
  "Open the inspector on the job selected in the rail."
  (let* ((index (first (lightfast:browser-selected-indices (workbench--jobs-list workbench))))
         (job (and index (nth index (workbench-jobs workbench)))))
    (when job
      (inspector-show-job (workbench-inspector workbench) job)
      (setf (workbench-inspector-visible-p workbench) t)
      (workbench--relayout workbench)))
  nil)

(-> workbench--window-key (workbench string) null)
(defun workbench--window-key (workbench value)
  "Handle a key that no focused widget claimed."
  (destructuring-bind (key state &rest rest) (parse-integers value)
    (declare (ignore rest))
    (when (workbench--global-key workbench key (logtest state *state-control*))
      (lightfast:consume-event)))
  nil)

(-> workbench-show-settings (workbench &optional (or null setting-entry)) null)
(defun workbench-show-settings (workbench &optional entry)
  "Open the settings sheet, on ENTRY when given."
  (let ((sheet (or (workbench--sheet workbench)
                   (setf (workbench--sheet workbench) (make-settings-sheet workbench)))))
    (settings-sheet-refresh sheet)
    (when entry
      (settings-sheet--show-entry sheet entry))
    (lightfast:show (settings-sheet-window sheet)))
  nil)

(-> workbench-show-palette (workbench) null)
(defun workbench-show-palette (workbench)
  "Open the command palette."
  (let ((palette (or (workbench--palette workbench)
                     (setf (workbench--palette workbench) (make-palette workbench)))))
    (palette-open palette))
  nil)

(-> workbench-invoke (workbench operation-entry) null)
(defun workbench-invoke (workbench operation)
  "Run OPERATION through the backend, asking for arguments when it takes any."
  (cond
    ((operation-entry-terminal-owning-p operation)
     (transcript-notice (workbench-transcript workbench) :severity ':warning
                        :text (format nil "~A needs a terminal and cannot run from here yet."
                                      (operation-entry-name operation))))
    ((not (operation-entry-admitted-p operation))
     (transcript-notice (workbench-transcript workbench) :severity ':warning
                        :text (format nil "~A cannot run while Autolith is busy." (operation-entry-name operation))))
    (t
     (let ((arguments (if (operation-entry-argument-schema operation)
                          (lightfast:input-dialog (format nil "Arguments for ~A" (operation-entry-name operation)))
                          "")))
       (when arguments
         (backend-invoke (workbench-backend workbench) (operation-entry-name operation) arguments)))))
  nil)

(-> workbench-decide (workbench integer) null)
(defun workbench-decide (workbench option-number)
  "Answer the pending approval with its OPTION-NUMBERth offered option."
  (let* ((approval (workbench-pending-approval workbench))
         (option (and approval (nth (1- option-number) (approval-options approval)))))
    (when option
      (backend-decide-approval (workbench-backend workbench) (approval-request-id approval) (first option))
      (transcript-decide-approval (workbench-transcript workbench) (approval-request-id approval) (first option))
      (workbench--clear-approval workbench)))
  nil)

(-> workbench--conversation-chosen (workbench) null)
(defun workbench--conversation-chosen (workbench)
  "Open the conversation selected in the rail."
  (let* ((index (first (lightfast:browser-selected-indices (workbench--conversations-list workbench))))
         (entry (and index (nth index (workbench-conversations workbench)))))
    (when (and entry (eq (workbench-turn-state workbench) ':idle))
      (backend-open-conversation (workbench-backend workbench)
                                 :conversation-id (conversation-entry-id entry))))
  nil)

(-> workbench-close (workbench) null)
(defun workbench-close (workbench)
  "Stop the backend and forget the workbench; the window closes with it."
  (unless (workbench-closed-p workbench)
    (setf (workbench-closed-p workbench) t)
    (setf *workbenches* (remove workbench *workbenches*))
    (backend-stop (workbench-backend workbench)))
  nil)


;;; Presenting state

(-> workbench--refresh-status (workbench) null)
(defun workbench--refresh-status (workbench)
  "Rewrite the status line and the header state word from current state."
  (let* ((snapshot (workbench-snapshot workbench))
         (fraction (and snapshot (state-snapshot-context-fraction snapshot)))
         (approvals (length (transcript-pending-approvals (workbench-transcript workbench))))
         (connection (string-downcase (symbol-name (workbench-connection-state workbench))))
         (turn (string-downcase (symbol-name (workbench-turn-state workbench)))))
    (setf (lightfast:label (workbench--status-label workbench))
          (format nil "~A · ~A~@[ · context ~D percent~]~[~; · 1 approval waiting~:; · ~:*~D approvals waiting~]"
                  connection turn (and fraction (round (* 100 fraction))) approvals))
    (setf (lightfast:label (workbench--state-label workbench))
          (cond
            ((member (workbench-connection-state workbench) '(:lost :exited)) connection)
            ((eq (workbench-session-state workbench) ':loading) "loading")
            (t turn)))
    (let ((color (case (workbench-turn-state workbench)
                   (:running (accent ':green))
                   (:cancelling (accent ':orange))
                   (:interrupted (accent ':red))
                   (t *ink*))))
      (destructuring-bind (red green blue) color
        (lightfast:set-label-color-rgb (workbench--state-label workbench) :red red :green green :blue blue))))
  nil)

(-> workbench--show-approval (workbench approval) null)
(defun workbench--show-approval (workbench approval)
  "Present APPROVAL in the decision row with one button per offered option."
  (setf (workbench-pending-approval workbench) approval)
  (dolist (button (workbench--approval-buttons workbench))
    (lightfast:destroy button))
  (setf (workbench--approval-buttons workbench)
        (loop for option in (approval-options approval)
              for number from 1
              collect (let ((number number))
                        (lightfast:make-button :parent (workbench--approval-row workbench)
                                               :label (approval-option-text option number)
                                               :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-decide workbench number))))))
  (setf (lightfast:label (workbench--approval-label workbench))
        (format nil "Approval needed: ~A" (approval-title approval)))
  (workbench--relayout workbench)
  (workbench--refresh-status workbench)
  nil)

(-> workbench--clear-approval (workbench) null)
(defun workbench--clear-approval (workbench)
  "Take down the decision row."
  (setf (workbench-pending-approval workbench) nil)
  (dolist (button (workbench--approval-buttons workbench))
    (lightfast:destroy button))
  (setf (workbench--approval-buttons workbench) nil)
  (workbench--relayout workbench)
  (workbench--refresh-status workbench)
  nil)

(-> workbench--show-conversations (workbench list) null)
(defun workbench--show-conversations (workbench entries)
  "List ENTRIES in the rail, most recent first."
  (let ((list (workbench--conversations-list workbench))
        (sorted (conversations-by-recency entries)))
    (setf (workbench-conversations workbench) sorted)
    (lightfast:clear list)
    (dolist (entry sorted)
      (lightfast:add-item list (format nil "~A~@[  ~A~]"
                                       (or (conversation-entry-title entry) (conversation-entry-id entry))
                                       (and (conversation-entry-live-p entry) "live")))))
  nil)

(-> workbench--show-jobs (workbench) null)
(defun workbench--show-jobs (workbench)
  "List the current jobs in the rail."
  (let ((list (workbench--jobs-list workbench)))
    (lightfast:clear list)
    (dolist (job (workbench-jobs workbench))
      (lightfast:add-item list (format nil "~A  ~(~A~)~@[  ~Ds~]"
                                       (or (job-entry-role job) (job-entry-id job))
                                       (job-entry-state job)
                                       (job-entry-elapsed-seconds job)))))
  nil)

(-> workbench--apply-snapshot (workbench state-snapshot) null)
(defun workbench--apply-snapshot (workbench snapshot)
  "Show SNAPSHOT in the header."
  (setf (workbench-snapshot workbench) snapshot)
  (when (state-snapshot-title snapshot)
    (setf (lightfast:label (workbench--title-label workbench)) (state-snapshot-title snapshot)))
  (setf (lightfast:label (workbench--model-label workbench))
        (format nil "~@[~A~]~@[ · ~A~]" (state-snapshot-model snapshot) (state-snapshot-effort snapshot)))
  (setf (lightfast:label (workbench--permission-label workbench))
        (if (state-snapshot-permission-mode snapshot)
            (string-downcase (symbol-name (state-snapshot-permission-mode snapshot)))
            ""))
  (workbench--refresh-status workbench)
  nil)


;;; Event application

(defmethod apply-ui-event ((event ui-event) (workbench workbench))
  "Events without a specific presentation are ignored."
  (declare (ignore event workbench))
  nil)

(defmethod apply-ui-event ((event connection-event) (workbench workbench))
  (setf (workbench-connection-state workbench) (connection-event-state event))
  (case (connection-event-state event)
    ((:lost :exited)
     (transcript-notice (workbench-transcript workbench)
                        :severity ':failure
                        :text (format nil "Connection ~(~A~)~@[: ~A~]" (connection-event-state event)
                                      (connection-event-detail event))
                        :key ':connection)
     (when (workbench-pending-approval workbench)
       (transcript-decide-approval (workbench-transcript workbench)
                                   (approval-request-id (workbench-pending-approval workbench)) ':cancelled)
       (workbench--clear-approval workbench)))
    (:ready
     (transcript-notice (workbench-transcript workbench) :text "Connected" :key ':connection)))
  (workbench--refresh-status workbench))

(defmethod apply-ui-event ((event session-event) (workbench workbench))
  (setf (workbench-session-state workbench) (session-event-state event))
  (ecase (session-event-state event)
    (:loading
     (transcript-reset (workbench-transcript workbench))
     (transcript-begin-replay (workbench-transcript workbench))
     (setf (lightfast:label (workbench--title-label workbench))
           (or (session-event-title event) "Loading")))
    (:ready
     (transcript-end-replay (workbench-transcript workbench))
     (transcript-view-jump-to-end (workbench-transcript-view workbench))
     (setf (lightfast:label (workbench--title-label workbench))
           (or (session-event-title event) "New conversation")))
    (:closed
     nil))
  (workbench--refresh-status workbench))

(defmethod apply-ui-event ((event turn-event) (workbench workbench))
  (setf (workbench-turn-state workbench) (turn-event-state event))
  (when (eq (turn-event-state event) ':interrupted)
    (transcript-notice (workbench-transcript workbench) :severity ':warning
                       :text "The turn was interrupted; review the transcript before retrying."
                       :key ':interrupted))
  (when (and (member (turn-event-state event) '(:idle :interrupted))
             (workbench-pending-approval workbench))
    (transcript-decide-approval (workbench-transcript workbench)
                                (approval-request-id (workbench-pending-approval workbench)) ':cancelled)
    (workbench--clear-approval workbench))
  (workbench--refresh-status workbench))

(defmethod apply-ui-event ((event transcript-event) (workbench workbench))
  (transcript-event-apply event (workbench-transcript workbench))
  (let ((subject (inspector-subject (workbench-inspector workbench))))
    (when (and (typep subject 'tool-call)
               (eq (transcript-event-operation event) ':update-tool-call)
               (equal (tool-call-call-id subject) (getf (transcript-event-arguments event) ':call-id)))
      (inspector-show-tool-call (workbench-inspector workbench) subject)))
  (when (eq (transcript-event-operation event) ':decide-approval)
    (let ((pending (workbench-pending-approval workbench)))
      (when (and pending
                 (equal (approval-request-id pending)
                        (getf (transcript-event-arguments event) ':request-id)))
        (workbench--clear-approval workbench)))))

(defmethod apply-ui-event ((event approval-event) (workbench workbench))
  (let ((approval (transcript-append (workbench-transcript workbench)
                                     (make-instance 'approval
                                                    :request-id (approval-event-request-id event)
                                                    :call-id (approval-event-call-id event)
                                                    :title (approval-event-title event)
                                                    :arguments (approval-event-arguments event)
                                                    :directory (approval-event-directory event)
                                                    :options (approval-event-options event)
                                                    :deadline (approval-event-deadline event)))))
    (unless (transcript-replaying-p (workbench-transcript workbench))
      (workbench--show-approval workbench approval))))

(defmethod apply-ui-event ((event state-event) (workbench workbench))
  (let ((snapshot (state-event-snapshot event)))
    (when (typep snapshot 'state-snapshot)
      (workbench--apply-snapshot workbench snapshot))))

(defmethod apply-ui-event ((event jobs-event) (workbench workbench))
  (ecase (jobs-event-kind event)
    (:snapshot
     (setf (workbench-jobs workbench) (jobs-event-jobs event)))
    (:delta
     (let ((jobs (remove-if (lambda (job)
                              (or (member (job-entry-id job) (jobs-event-removed-ids event) :test #'equal)
                                  (find (job-entry-id job) (jobs-event-jobs event)
                                        :key #'job-entry-id :test #'equal)))
                            (workbench-jobs workbench))))
       (setf (workbench-jobs workbench) (append jobs (jobs-event-jobs event))))))
  (workbench--show-jobs workbench)
  (let ((subject (inspector-subject (workbench-inspector workbench))))
    (when (typep subject 'job-entry)
      (let ((current (find (job-entry-id subject) (workbench-jobs workbench) :key #'job-entry-id :test #'equal)))
        (when (and current (not (eq current subject)))
          (inspector-show-job (workbench-inspector workbench) current))))))

(defmethod apply-ui-event ((event catalog-event) (workbench workbench))
  (ecase (catalog-event-kind event)
    (:conversations
     (workbench--show-conversations workbench (catalog-event-items event)))
    (:settings
     (setf (workbench-settings workbench) (catalog-event-items event))
     (when (workbench--sheet workbench)
       (settings-sheet-refresh (workbench--sheet workbench))))
    (:models
     (setf (workbench-models workbench) (catalog-event-items event)))
    (:operations
     (setf (workbench-operations workbench) (catalog-event-items event)))))

(defmethod apply-ui-event ((event result-event) (workbench workbench))
  (case (result-event-outcome event)
    (:condition
     (transcript-notice (workbench-transcript workbench) :severity ':failure
                        :text (format nil "~@[~A: ~]~A" (result-event-condition-type event)
                                      (or (result-event-report event) "the request failed"))))
    (:busy
     (transcript-notice (workbench-transcript workbench) :severity ':warning
                        :text "Autolith is busy; the request was not admitted." :key ':busy))
    (:unsupported
     (transcript-notice (workbench-transcript workbench) :severity ':warning
                        :text "This Autolith does not support that request."))
    (t
     nil)))

(-> workbench-dispatch (ui-event) null)
(defun workbench-dispatch (event)
  "Apply EVENT to the workbench of its backend, dropping stale and orphaned events."
  (let ((workbench (find-workbench (ui-event-backend-id event))))
    (when (and workbench
               (ui-event-current-p event (backend-id (workbench-backend workbench))
                                   (backend-generation (workbench-backend workbench))))
      (apply-ui-event event workbench)))
  nil)
