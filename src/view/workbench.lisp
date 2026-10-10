(in-package #:aui)

;;;; -- Workbench --

;;; The main window: header, rail, transcript, composer and status line, all
;;; driven by UI events from one backend. Everything here runs on the FLTK
;;; thread.

(defparameter *permission-modes*
  '(("ask" "Ask before commands" "approve commands and external tools here")
    ("auto" "Auto" "classify commands with the configured policy")
    ("sandboxed" "Sandboxed" "run commands in the workspace sandbox")
    ("full-access" "Full access" "allow commands and external tools without approval"))
  "The ACP permission modes Autolith offers: id, label and description.")

(defparameter *rail-breakpoint* 1100
  "Window width in pixels below which the rail is hidden.")

(defparameter *inspector-breakpoint* 1500
  "Window width in pixels below which an open inspector replaces the rail.")

(defparameter *header-height* 36
  "Height of the header row in pixels.")

(defparameter *approval-detail-characters* 90
  "Longest the approval card shows of the request's arguments.")

(defparameter *prose-line-pitch* 25
  "Pixels from one line of 20 px prose to the next in a text editor.")

(defparameter *chips-breakpoint* 760
  "Narrowest window that still shows the model, effort and permission chips.")

(defparameter *list-line-spacing* 10
  "Pixels between the rows of a list, room above and below the text.")

(defparameter *menu-height* 34
  "Height of the menu bar in pixels.")

(defvar *open-conversations-function* nil
  "Called with no arguments to show the conversation manager; set by MAIN.")

(defparameter *shortcut-control* #x40000
  "FLTK's shortcut modifier bit for Ctrl.")

(defparameter *shortcut-shift* #x10000
  "FLTK's shortcut modifier bit for Shift.")

(defparameter *composer-height* 112
  "Height of the composer row in pixels.")

(defparameter *context-bar-width* 120
  "Width of the context occupancy bar in the status row.")

(defparameter *context-bar-height* 8
  "Height of the context occupancy bar.")

(defparameter *plan-view-max-height* 420
  "Tallest the plan section of the rail grows before it scrolls.")

(defparameter *plan-indent* 40
  "Pixels of a plan line taken by the marker and the display's margins.")

(defparameter *plan-line-height* 30
  "Height of one plan line in the rail, about the prose line pitch.")

(defparameter *job-poll-interval* 3.0
  "Seconds between refreshes of a viewed child job's transcript.")

(defparameter *animation-interval* 0.12
  "Seconds between frames of the spinner.")

(defparameter *spinner-dots* 8
  "How many dots the spinner's ring has.")

(defparameter *spinner-size* 26
  "Pixel size of the spinner and the state mark in the header.")

(defparameter *status-height* 28
  "Height of the status row in pixels.")

(defparameter *control-height* 38
  "Height of buttons and single-line fields.")

(defparameter *text-column-width* 780
  "Widest the transcript and composer grow, in pixels: about 95 characters of
17 px serif, so that lines stay readable on a wide window.")

(defparameter *rail-width* 260
  "Width of the rail in pixels.")

(defclass workbench ()
  ((window              :reader workbench-window)
   (backend             :initarg :backend :accessor workbench-backend :type backend)
   (transcript          :initform (make-transcript) :reader workbench-transcript :type transcript)
   (transcript-view     :reader workbench-transcript-view)
   (folder-label        :reader workbench--folder-label)
   (title-label         :reader workbench--title-label)
   (model-chip          :reader workbench--model-chip)
   (effort-chip         :reader workbench--effort-chip)
   (permission-chip     :reader workbench--permission-chip)
   (state-mark          :reader workbench--state-mark
                        :documentation "The canvas drawing the spinner or the trouble mark in the header.")
   (state-kind          :initform ':idle :accessor workbench--state-kind
                        :type (member :idle :busy :trouble)
                        :documentation "What the state mark draws: nothing, a turning spinner or a still dot.")
   (state-color         :initform '(0 0 0) :accessor workbench--state-color
                        :documentation "The (RED GREEN BLUE) the state mark is drawn in.")
   (rail                :reader workbench--rail)
   (left-spacer         :reader workbench--left-spacer)
   (right-spacer        :reader workbench--right-spacer)
   (composer-lines      :initform 1 :accessor workbench--composer-lines
                        :documentation "How many lines the composer text takes, for its height.")
   (approval-countdown  :reader workbench--approval-countdown
                        :documentation "The label counting the approval's seconds down.")
   (approval-detail     :reader workbench--approval-detail
                        :documentation "The label showing the request's arguments.")
   (document-request    :initform nil :accessor workbench--document-request
                        :documentation "(REQUEST-ID . TITLE) of a command whose text the aside view awaits, or NIL.")
   (jobs-label          :reader workbench--jobs-label)
   (jobs-view           :reader workbench--jobs-view
                        :documentation "The styled display listing the sub-agents, a mark and a name each.")
   (approval-row        :reader workbench--approval-row)
   (approval-label      :reader workbench--approval-label)
   (approval-buttons    :initform nil :accessor workbench--approval-buttons)
   (composer            :reader workbench-composer)
   (composer-frame      :reader workbench--composer-frame
                        :documentation "The hairline group around the composer, giving it padding.")
   (follow-ups          :initform nil :accessor workbench-follow-ups
                        :documentation "Prompts queued to be sent, oldest first, when the turn ends.")
   (send-button         :reader workbench--send-button)
   (stop-button         :reader workbench--stop-button)
   (status-label        :reader workbench--status-label)
   (context-bar         :reader workbench--context-bar
                        :documentation "The progress bar showing how full the context is.")
   (plan-label          :reader workbench--plan-label)
   (plan-view           :reader workbench--plan-view
                        :documentation "The styled display of the agent's plan in the rail.")
   (plan-lines          :initform 0 :accessor workbench--plan-lines
                        :documentation "How many plan entries the rail shows; zero hides the section.")
   (animation-timer     :initform nil :accessor workbench--animation-timer
                        :documentation "The repeating timeout animating the state word, or NIL.")
   (animation-phase     :initform 0 :accessor workbench--animation-phase
                        :documentation "Which frame of the working animation shows.")
   (menu                :reader workbench--menu
                        :documentation "The menu bar across the top of the window.")
   (inspector           :reader workbench-inspector)
   (inspector-visible-p :initform nil :accessor workbench-inspector-visible-p :type boolean)
   (center-mode         :initform ':transcript :accessor workbench-center-mode
                        :type (member :transcript :commands :settings :job :document)
                        :documentation "Which pane the center of the window shows.")
   (job-transcript      :initform (make-transcript) :reader workbench--job-transcript
                        :documentation "The transcript model holding the viewed child job's text.")
   (job-view            :reader workbench--job-view
                        :documentation "The TRANSCRIPT-VIEW showing the viewed child job.")
   (job-label           :reader workbench--job-label)
   (job-back-button     :reader workbench--job-back-button)
   (viewed-job          :initform nil :accessor workbench-viewed-job
                        :documentation "The JOB-ENTRY whose transcript the center shows, or NIL.")
   (job-content         :initform nil :accessor workbench--job-content
                        :documentation "The job transcript text last rendered, to skip unchanged refreshes.")
   (job-timer           :initform nil :accessor workbench--job-timer
                        :documentation "The repeating timeout refreshing the viewed job, or NIL.")
   (settings-pane       :reader workbench-settings-pane)
   (palette             :reader workbench-palette)
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
   (queued-prompt       :initform nil :accessor workbench-queued-prompt
                        :documentation "Text to send as soon as the session is ready and idle, or NIL.")
   (conversation-id     :initform nil :accessor workbench-conversation-id
                        :documentation "The open conversation's id, or NIL before one is ready.")
   (drafts              :initform (make-hash-table :test 'equal) :reader workbench--drafts
                        :documentation "Composer text kept per conversation id across switches.")
   (countdown-timer     :initform nil :accessor workbench--countdown-timer
                        :documentation "The repeating timeout refreshing the approval countdown, or NIL.")
   (connection-state    :initform ':starting :accessor workbench-connection-state)
   (session-state       :initform ':closed :accessor workbench-session-state)
   (turn-state          :initform ':idle :accessor workbench-turn-state)
   (snapshot            :initform nil :accessor workbench-snapshot
                        :documentation "The latest STATE-SNAPSHOT, or NIL.")
   (layout-size         :initform nil :accessor workbench-layout-size
                        :documentation "A (WIDTH HEIGHT) to lay the window out for instead of its real
size, used to inspect other layouts under a tiling compositor; NIL normally.")
   (closed-p            :initform nil :accessor workbench-closed-p))
  (:documentation "One workspace window bound to one backend."))

(defvar *workbenches* nil
  "Every open workbench, so events can be routed by backend id.")

(defvar *stopped-backends* nil
  "Backends told to stop whose processes the application still has to wait for.")

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

(-> workbench--make-chip (t string function) t)
(defun workbench--make-chip (parent text function)
  "Create a flat, clickable chip under PARENT showing TEXT; a click calls FUNCTION."
  (let ((chip (lightfast:make-button :parent parent :label (format nil "~A ▾" text) :height *control-height*
                                     :callback (lambda (widget event value)
                                                 (declare (ignore widget event value))
                                                 (funcall function)))))
    (lightfast:set-box chip lightfast:+box-no-box+)
    (lightfast:set-label-font chip (font ':prose-italic))
    (lightfast:set-label-size chip *prose-size*)
    (lightfast:set-tooltip chip "Click to change")
    chip))

(-> workbench--set-chip (t string) null)
(defun workbench--set-chip (chip text)
  "Show TEXT on CHIP with the dropdown mark."
  (setf (lightfast:label chip) (format nil "~A ▾" text))
  nil)

(-> workbench-choose-model (workbench) null)
(defun workbench-choose-model (workbench)
  "Offer the models with a stored credential in a menu and apply the choice."
  (let* ((models (remove-if-not #'model-entry-credential-p (workbench-models workbench)))
         (verified (remove-if-not #'model-entry-verified-p models))
         (shown (or verified models))
         (items (append (mapcar (lambda (model)
                                  (format nil "~A/~A" (or (model-entry-provider model) "other") (model-entry-id model)))
                                shown)
                        (list "-" "All models in the palette"))))
    (let ((choice (lightfast:popup-menu items)))
      (cond
        ((null choice) nil)
        ((>= choice (length shown)) (workbench-show-palette workbench))
        (t (backend-set-setting (workbench-backend workbench) ':model (model-entry-id (nth choice shown)))))))
  nil)

(-> workbench-choose-effort (workbench) null)
(defun workbench-choose-effort (workbench)
  "Offer the reasoning efforts the settings catalog lists and apply the choice."
  (let* ((entry (find ':reasoning-effort (workbench-settings workbench) :key #'setting-entry-name))
         (options (mapcar #'princ-to-string (and entry (setting-entry-options entry)))))
    (when options
      (let ((choice (lightfast:popup-menu options)))
        (when choice
          (backend-set-setting (workbench-backend workbench) ':reasoning-effort (nth choice options))))))
  nil)

(-> workbench-choose-permission (workbench) null)
(defun workbench-choose-permission (workbench)
  "Offer the permission modes and apply the choice."
  (let* ((labels (mapcar #'second *permission-modes*))
         (choice (lightfast:popup-menu labels)))
    (when choice
      (backend-set-mode (workbench-backend workbench) (first (nth choice *permission-modes*)))))
  nil)

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
                 rail jobs-label jobs-view
                 approval-row approval-label composer send-button stop-button status-label
                 transcript-view)
        workbench
      (setf folder-label (workbench--make-label window (folder-tail (backend-directory backend)) ':prose-italic))
      (lightfast:set-tooltip folder-label (backend-directory backend))
      (setf
            title-label (workbench--make-label window "New conversation" ':prose)
            (slot-value workbench 'state-mark)
            (lightfast:make-canvas :parent window :width *spinner-size* :height *spinner-size*
                                   :callback (lambda (widget event value)
                                               (declare (ignore event value))
                                               (workbench--draw-state-mark workbench widget))))
      (lightfast:set-box (workbench--state-mark workbench) lightfast:+box-no-box+)
      (lightfast:set-label-size title-label *heading-size*)
      (lightfast:set-label-align title-label ':left ':inside ':clip)
      (setf (slot-value workbench 'model-chip)
            (workbench--make-chip window "model" (lambda () (workbench-choose-model workbench)))
            (slot-value workbench 'effort-chip)
            (workbench--make-chip window "effort" (lambda () (workbench-choose-effort workbench)))
            (slot-value workbench 'permission-chip)
            (workbench--make-chip window "permissions" (lambda () (workbench-choose-permission workbench))))
      (setf (slot-value workbench 'menu) (lightfast:make-menu-bar :parent window :width 600 :height *menu-height*))
      (workbench--fill-menu workbench)
      (setf (slot-value workbench 'job-view)
            (make-transcript-view (workbench--job-transcript workbench) :parent window :width 600 :height 300)
            (slot-value workbench 'job-label) (workbench--make-label window "" ':prose-bold)
            (slot-value workbench 'job-back-button)
            (lightfast:make-button :parent window :label "Back to the conversation" :height *control-height*
                                   :callback (lambda (widget event value)
                                               (declare (ignore widget event value))
                                               (workbench-show-transcript workbench))))
      (lightfast:set-label-align (workbench--job-label workbench) ':left ':inside ':clip)
      (lightfast:set-tooltip (workbench--job-back-button workbench) "Escape")
      (dolist (widget (list (transcript-view-display (workbench--job-view workbench)) (workbench--job-label workbench)
                            (workbench--job-back-button workbench)))
        (lightfast:hide widget))
      (setf rail (lightfast:make-group :parent window :width *rail-width* :height 400))
      (lightfast:set-box rail lightfast:+box-no-box+)
      (setf (slot-value workbench 'left-spacer) (lightfast:make-box :parent window :width 1 :height 1)
            (slot-value workbench 'right-spacer) (lightfast:make-box :parent window :width 1 :height 1))
      (lightfast:set-box (workbench--left-spacer workbench) lightfast:+box-no-box+)
      (lightfast:set-box (workbench--right-spacer workbench) lightfast:+box-no-box+)
      (setf jobs-label (workbench--make-label rail "Agents" ':prose-bold)
            jobs-view (lightfast:make-text-display :parent rail :width *rail-width* :height 120)
            (slot-value workbench 'plan-label) (workbench--make-label rail "Plan" ':prose-bold)
            (slot-value workbench 'plan-view) (lightfast:make-text-display :parent rail :width *rail-width* :height 120))
      ;; Both rail displays share one style table: prose, then marks in ink,
      ;; orange, green and red.
      (dolist (view (list jobs-view (workbench--plan-view workbench)))
        (lightfast:set-box view lightfast:+box-no-box+)
        (lightfast:text-set-wrap view ':bounds)
        ;; The rail sizes these displays to their lines; they never scroll.
        (lightfast:text-set-scrollbars view ':none)
        (lightfast:text-set-styles view
                                   (list (list :font (font ':prose) :size *prose-size* :color *ink*)
                                         (list :font (font ':marker) :size *marker-size* :color *ink*)
                                         (list :font (font ':marker) :size *marker-size* :color (accent ':orange))
                                         (list :font (font ':marker) :size *marker-size* :color (accent ':green))
                                         (list :font (font ':marker) :size *marker-size* :color (accent ':red))))
        (lightfast:hide view))
      (lightfast:hide (workbench--plan-label workbench))
      (lightfast:hide jobs-label)
      (lightfast:on jobs-view
                    (lambda (widget event value)
                      (declare (ignore widget event))
                      (workbench--agent-clicked workbench value))
                    :event lightfast:+event-push+)
      (setf transcript-view (make-transcript-view (workbench-transcript workbench)
                                                  :parent window :width 600 :height 400))
      (setf (transcript-view-on-context transcript-view)
            (lambda (block-id) (workbench--block-menu workbench block-id)))
      (setf (slot-value workbench 'inspector) (make-inspector workbench window))
      (setf (slot-value workbench 'palette) (make-palette workbench window)
            (slot-value workbench 'settings-pane) (make-settings-pane workbench window))
      (setf approval-row (lightfast:make-group :parent window :width 600 :height (* 3 *control-height*)))
      (lightfast:set-box approval-row lightfast:+box-down-frame+)
      (setf approval-label (workbench--make-label approval-row "" ':prose-bold)
            (slot-value workbench 'approval-countdown) (workbench--make-label approval-row "" ':prose-italic)
            (slot-value workbench 'approval-detail) (workbench--make-label approval-row "" ':prose))
      (lightfast:set-label-align (workbench--approval-countdown workbench) ':right ':inside)
      (lightfast:set-label-font (workbench--approval-detail workbench) (font ':code))
      (lightfast:set-label-size (workbench--approval-detail workbench) *code-size*)
      (setf (slot-value workbench 'composer-frame)
            (lightfast:make-group :parent window :width 600 :height *composer-height*))
      (lightfast:set-box (workbench--composer-frame workbench) lightfast:+box-down-frame+)
      (setf composer (lightfast:make-text-editor :parent (workbench--composer-frame workbench)
                                                 :width 600 :height *composer-height*))
      (lightfast:set-box composer lightfast:+box-no-box+)
      (lightfast:set-text-font composer (font ':prose))
      (lightfast:set-text-size composer *prose-size*)
      (lightfast:text-set-wrap composer ':bounds)
      (lightfast:on composer
                    (lambda (widget event value)
                      (declare (ignore widget event))
                      (workbench--composer-key workbench value))
                    :event lightfast:+event-key+)
      (lightfast:on composer
                    (lambda (widget event value)
                      (declare (ignore widget event value))
                      (workbench--composer-grown workbench))
                    :event lightfast:+event-change+)
      (setf send-button (lightfast:make-button :parent window :label "Send" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-send workbench)))
            stop-button (lightfast:make-button :parent window :label "Stop" :height *control-height*
                                               :callback (lambda (widget event value)
                                                           (declare (ignore widget event value))
                                                           (workbench-stop workbench))))
      (setf status-label (workbench--make-label window "" ':prose-italic))
      (setf (slot-value workbench 'context-bar)
            (lightfast:make-progress :parent window :width *context-bar-width* :height *context-bar-height* :value "0"))
      (lightfast:hide (workbench--context-bar workbench))
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
    (transcript-notice (workbench-transcript workbench) :severity ':information :key ':starting
                       :text (format nil "Starting Autolith in ~A" (backend-directory backend)))
    (workbench--relayout workbench)
    (workbench--refresh-status workbench)
    (push workbench *workbenches*)
    workbench))

(-> workbench--relayout (workbench) null)
(defun workbench--relayout (workbench)
  "Lay the window out for its current size: rail on the left when wide enough,
a text column capped at *TEXT-COLUMN-WIDTH*, the inspector on the right when
open, and the decision row only while an approval waits."
  (let* ((window (workbench-window workbench))
         (pinned (workbench-layout-size workbench))
         (width (or (first pinned)
                    (progn (lightfast:refresh-geometry window) (lightfast:widget-width window))))
         (height (or (second pinned) (lightfast:widget-height window)))
         (inspector-p (workbench-inspector-visible-p workbench))
         (rail-content-p (or (plusp (workbench--plan-lines workbench)) (workbench-agents workbench)))
         (rail-p (and rail-content-p
                      (>= width *rail-breakpoint*)
                      (or (not inspector-p) (>= width *inspector-breakpoint*))))
         (approval-p (and (workbench-pending-approval workbench) t))
         (gap (spacing 4))
         (rail (workbench--rail workbench))
         (approval-row (workbench--approval-row workbench)))
    (if rail-p (lightfast:show rail) (lightfast:hide rail))
    (if approval-p (lightfast:show approval-row) (lightfast:hide approval-row))
    (if inspector-p
        (lightfast:show (inspector-group (workbench-inspector workbench)))
        (lightfast:hide (inspector-group (workbench-inspector workbench))))
    (let ((mode (workbench-center-mode workbench)))
      (dolist (widget (list (transcript-view-display (workbench--job-view workbench)) (workbench--job-label workbench)
                            (workbench--job-back-button workbench)))
        (if (member mode '(:job :document)) (lightfast:show widget) (lightfast:hide widget)))
      (dolist (widget (list (workbench--jobs-label workbench) (workbench--jobs-view workbench)))
        (if (workbench-agents workbench) (lightfast:show widget) (lightfast:hide widget)))
      (cond
        ((eq mode ':job)
         (workbench--show-job-label workbench))
        (t
         (setf (lightfast:label (workbench--send-button workbench)) "Send"
               (lightfast:label (workbench--stop-button workbench)) "Stop")))
      (dolist (widget (list (workbench--composer-frame workbench) (workbench--send-button workbench)))
        (if (member mode '(:transcript :job)) (lightfast:show widget) (lightfast:hide widget)))
      ;; Stop shows only while there is something to stop: a running turn, or
      ;; the viewed job while it runs.
      (if (and (member mode '(:transcript :job)) (workbench--stoppable-p workbench))
          (lightfast:show (workbench--stop-button workbench))
          (lightfast:hide (workbench--stop-button workbench)))
      (dolist (widget (list (transcript-view-display (workbench-transcript-view workbench)) (workbench--state-mark workbench)))
        (if (eq mode ':transcript) (lightfast:show widget) (lightfast:hide widget)))
      (when (and approval-p (not (eq mode ':transcript)))
        (lightfast:hide approval-row))
      (if (eq mode ':commands)
          (lightfast:show (palette-group (workbench-palette workbench)))
          (lightfast:hide (palette-group (workbench-palette workbench))))
      (if (eq mode ':settings)
          (lightfast:show (settings-pane-group (workbench-settings-pane workbench)))
          (lightfast:hide (settings-pane-group (workbench-settings-pane workbench)))))
    (let* ((chips-p (>= width *chips-breakpoint*))
           (header (lightfast:make-layout-row
                    :basis *header-height* :shrink 0 :gap gap
                    :children (list (lightfast:make-layout-item (workbench--title-label workbench) :basis 0 :grow 1 :min-width 360))))
           ;; The state mark sits centered under the transcript, where the
           ;; next words will appear; when idle the row shrinks to a breath.
           (state-row (lightfast:make-layout-row
                       :basis (if (eq (workbench--state-kind workbench) ':idle) (spacing 2) *spinner-size*)
                       :shrink 0 :justify ':center
                       :children (list (lightfast:make-layout-item (workbench--state-mark workbench)
                                                                   :basis *spinner-size* :shrink 0))))
           (rail-layout (lightfast:make-layout-column
                         :target rail :basis *rail-width* :shrink 0 :gap (spacing 2)
                         :children (append
                                    (when (plusp (workbench--plan-lines workbench))
                                      (list (lightfast:make-layout-item (workbench--plan-label workbench)
                                                                        :basis (+ *control-height* (spacing 4)) :shrink 0)
                                            (lightfast:make-layout-item (workbench--plan-view workbench)
                                                                        :basis (min *plan-view-max-height*
                                                                                    (+ (spacing 4) (* *plan-line-height*
                                                                                                      (workbench--plan-lines workbench))))
                                                                        :shrink 0)))
                                    (when (workbench-agents workbench)
                                      (list (lightfast:make-layout-item (workbench--jobs-label workbench)
                                                                        :basis (+ *control-height* (spacing 4)) :shrink 0)
                                            (lightfast:make-layout-item (workbench--jobs-view workbench)
                                                                        :basis (+ (spacing 4) (* *plan-line-height*
                                                                                                 (length (workbench-agents workbench))))
                                                                        :shrink 0))))))
           ;; The approval is a card: the question with the seconds left on
           ;; one line, the request's arguments on the next, and the choices
           ;; set to the right, each button as wide as its label and the
           ;; first allowing one filled with the mark green.
           (detail-p (plusp (length (lightfast:label (workbench--approval-detail workbench)))))
           (approval-layout (lightfast:make-layout-column
                             :target approval-row :shrink 0 :gap (spacing 2) :padding (spacing 3)
                             :basis (+ (* (if detail-p 3 2) *control-height*) (* (if detail-p 2 1) (spacing 2)) (* 2 (spacing 3)))
                             :children (append
                                        (list (lightfast:make-layout-row
                                               :basis *control-height* :shrink 0 :gap (spacing 3)
                                               :children (list (lightfast:make-layout-item (workbench--approval-label workbench)
                                                                                           :basis 0 :grow 1)
                                                               (lightfast:make-layout-item (workbench--approval-countdown workbench)
                                                                                           :basis 110 :shrink 0))))
                                        (when detail-p
                                          (list (lightfast:make-layout-item (workbench--approval-detail workbench)
                                                                            :basis *control-height* :shrink 0)))
                                        (list (lightfast:make-layout-row
                                               :basis *control-height* :shrink 0 :gap (spacing 3) :justify ':end
                                               :children (mapcar (lambda (button)
                                                                   (lightfast:make-layout-item
                                                                    button :shrink 0
                                                                    :basis (+ (spacing 5) (label-width (lightfast:label button)))))
                                                                 (workbench--approval-buttons workbench)))))))
           (composer-row (lightfast:make-layout-row
                          :basis (composer-height-for (workbench--composer-lines workbench)) :shrink 0 :gap (spacing 3)
                          :children (list (lightfast:make-layout-column
                                           :target (workbench--composer-frame workbench)
                                           :basis 0 :grow 1 :padding (spacing 2)
                                           :children (list (lightfast:make-layout-item (workbench-composer workbench)
                                                                                       :basis 0 :grow 1)))
                                          (lightfast:make-layout-column
                                           :basis (max 120 (+ (spacing 5)
                                                              (max (label-width (lightfast:label (workbench--send-button workbench)))
                                                                   (label-width (lightfast:label (workbench--stop-button workbench))))))
                                           :shrink 0 :gap (spacing 3)
                                           :children (list (lightfast:make-layout-item (workbench--send-button workbench)
                                                                                       :basis *control-height* :shrink 0)
                                                           (lightfast:make-layout-item (workbench--stop-button workbench)
                                                                                       :basis *control-height* :shrink 0))))))
           (center (ecase (workbench-center-mode workbench)
                     (:transcript
                      (lightfast:make-layout-column
                       :basis *text-column-width* :grow 0 :shrink 1 :max-width *text-column-width* :gap (spacing 4)
                       :children (append (list (lightfast:make-layout-item
                                                (transcript-view-display (workbench-transcript-view workbench))
                                                :basis 0 :grow 1)
                                               state-row)
                                         (when approval-p (list approval-layout))
                                         (list composer-row))))
                     (:job
                      (lightfast:make-layout-column
                       :basis *text-column-width* :grow 0 :shrink 1 :max-width *text-column-width* :gap (spacing 4)
                       :children (list (lightfast:make-layout-row
                                        :basis *control-height* :shrink 0 :gap (spacing 3)
                                        :children (list (lightfast:make-layout-item (workbench--job-label workbench)
                                                                                    :basis 0 :grow 1)
                                                        (lightfast:make-layout-item (workbench--job-back-button workbench)
                                                                                    :basis 250 :shrink 0)))
                                       (lightfast:make-layout-item
                                        (transcript-view-display (workbench--job-view workbench))
                                        :basis 0 :grow 1)
                                       composer-row)))
                     (:document
                      (lightfast:make-layout-column
                       :basis *text-column-width* :grow 0 :shrink 1 :max-width *text-column-width* :gap (spacing 4)
                       :children (list (lightfast:make-layout-row
                                        :basis *control-height* :shrink 0 :gap (spacing 3)
                                        :children (list (lightfast:make-layout-item (workbench--job-label workbench)
                                                                                    :basis 0 :grow 1)
                                                        (lightfast:make-layout-item (workbench--job-back-button workbench)
                                                                                    :basis 250 :shrink 0)))
                                       (lightfast:make-layout-item
                                        (transcript-view-display (workbench--job-view workbench))
                                        :basis 0 :grow 1))))
                     (:commands
                      (lightfast:make-layout-column
                       :basis 0 :grow 1 :max-width 900
                       :children (list (palette-layout (workbench-palette workbench)))))
                     (:settings
                      (lightfast:make-layout-column
                       :basis 0 :grow 1
                       :children (list (settings-pane-layout (workbench-settings-pane workbench)))))))
           (centered-p (and (member (workbench-center-mode workbench) '(:transcript :job :document)) (not inspector-p)))
           (body (lightfast:make-layout-row
                  :basis 0 :grow 1 :gap (spacing 6)
                  :children (append (when rail-p (list rail-layout))
                                    ;; Flexible blanks on both sides keep the capped text column
                                    ;; centered in the remaining width instead of hugging the rail.
                                    (when centered-p
                                      (list (lightfast:make-layout-item (workbench--left-spacer workbench) :basis 0 :grow 1)))
                                    (list center)
                                    (when centered-p
                                      (list (lightfast:make-layout-item (workbench--right-spacer workbench) :basis 0 :grow 1)))
                                    (when inspector-p (list (inspector-layout (workbench-inspector workbench)))))))
           (layout (lightfast:make-layout-column
                    :padding (list (spacing 5) (spacing 4) (spacing 5) (spacing 3)) :gap gap
                    :children (list (lightfast:make-layout-item (workbench--menu workbench) :basis *menu-height* :shrink 0)
                                    header body
                                    (lightfast:make-layout-row
                                     :basis *status-height* :shrink 0 :gap (spacing 4)
                                     :children (append
                                                (list (lightfast:make-layout-item (workbench--folder-label workbench)
                                                                                  :basis 0 :grow 1 :min-width 120)
                                                      (lightfast:make-layout-item (workbench--status-label workbench)
                                                                                  :basis 0 :grow 2)
                                                      (lightfast:make-layout-item (workbench--context-bar workbench)
                                                                                  :basis *context-bar-width* :shrink 0
                                                                                  :align-self ':center))
                                                (when chips-p
                                                  (list (lightfast:make-layout-item (workbench--model-chip workbench)
                                                                                    :basis (chip-width (workbench--model-chip workbench))
                                                                                    :shrink 0)
                                                        (lightfast:make-layout-item (workbench--effort-chip workbench)
                                                                                    :basis (chip-width (workbench--effort-chip workbench))
                                                                                    :shrink 0)
                                                        (lightfast:make-layout-item (workbench--permission-chip workbench)
                                                                                    :basis (chip-width (workbench--permission-chip workbench))
                                                                                    :shrink 0)))))))))
      (dolist (chip (list (workbench--model-chip workbench) (workbench--effort-chip workbench)
                          (workbench--permission-chip workbench)))
        (if chips-p (lightfast:show chip) (lightfast:hide chip)))
      (lightfast:apply-layout layout (lightfast:make-rect :width width :height height) :parent window)
      ;; FLTK centers each label in its box; the status row's labels and chips
      ;; are brought to one baseline explicitly.
      (lightfast:align-baselines (append (list (workbench--folder-label workbench) (workbench--status-label workbench))
                                         (when chips-p
                                           (list (workbench--model-chip workbench) (workbench--effort-chip workbench)
                                                 (workbench--permission-chip workbench))))
                                 :reference (workbench--folder-label workbench))
      (when (eq (workbench-center-mode workbench) ':settings)
        (settings-pane-fit (workbench-settings-pane workbench)))))
  nil)

(-> workbench--stoppable-p (workbench) boolean)
(defun workbench--stoppable-p (workbench)
  "Return T when the Stop button has something to stop."
  (if (eq (workbench-center-mode workbench) ':job)
      (let ((job (workbench-viewed-job workbench)))
        (and job (member (job-entry-state job) '(:running :pending :waiting)) t))
      (and (member (workbench-turn-state workbench) '(:running :cancelling)) t)))

(-> chip-width (t) integer)
(defun chip-width (chip)
  "Return the width CHIP wants for its label."
  (+ (spacing 4) (label-width (lightfast:label chip))))

(-> composer-height-for (integer) integer)
(defun composer-height-for (lines)
  "Return the composer row's height for LINES lines of text: the base height
until the text outgrows it, then up to twice that before it scrolls."
  (min (* 2 *composer-height*)
       (max *composer-height* (+ (* lines *prose-line-pitch*) (spacing 4) (spacing 2)))))

(-> workbench--composer-grown (workbench) null)
(defun workbench--composer-grown (workbench)
  "Follow the composer text with the composer's height."
  (let* ((composer (workbench-composer workbench))
         (width (max 100 (- (lightfast:widget-width composer) (spacing 3))))
         (lines (loop for line in (split-lines-keeping-empty (lightfast:value composer))
                      sum (max 1 (length (wrap-words line width))))))
    (unless (= lines (workbench--composer-lines workbench))
      (setf (workbench--composer-lines workbench) lines)
      (workbench--relayout workbench)))
  nil)


;;; Actions

(-> workbench-send (workbench) null)
(defun workbench-send (workbench)
  "Send the composer text as a prompt and clear the composer. While a turn
runs or the session is not ready yet, the text is queued as a follow-up."
  (let ((text (string-trim '(#\Space #\Newline #\Tab) (lightfast:value (workbench-composer workbench)))))
    (when (plusp (length text))
      (cond
        ((and (eq (workbench-center-mode workbench) ':job) (workbench-viewed-job workbench))
         (backend-job-action (workbench-backend workbench) (job-entry-id (workbench-viewed-job workbench))
                             ':send :text text))
        ((and (eq (workbench-turn-state workbench) ':idle)
              (eq (workbench-session-state workbench) ':ready))
         (backend-prompt (workbench-backend workbench) text))
        (t
         (workbench-queue-follow-up workbench text)))
      (setf (lightfast:value (workbench-composer workbench)) "")))
  nil)

(-> workbench-queue-follow-up (workbench string) null)
(defun workbench-queue-follow-up (workbench text)
  "Queue TEXT to be sent when the turn ends."
  (setf (workbench-follow-ups workbench) (append (workbench-follow-ups workbench) (list text)))
  (workbench--refresh-status workbench)
  nil)

(-> workbench--send-follow-up (workbench) null)
(defun workbench--send-follow-up (workbench)
  "Send the oldest queued follow-up when the session is ready and idle."
  (when (and (workbench-follow-ups workbench)
             (eq (workbench-session-state workbench) ':ready)
             (eq (workbench-turn-state workbench) ':idle))
    (backend-prompt (workbench-backend workbench) (pop (workbench-follow-ups workbench)))
    (workbench--refresh-status workbench))
  nil)

(-> workbench--send-queued-prompt (workbench) null)
(defun workbench--send-queued-prompt (workbench)
  "Send the queued prompt when the session is ready and idle."
  (let ((text (workbench-queued-prompt workbench)))
    (when (and text
               (eq (workbench-session-state workbench) ':ready)
               (eq (workbench-turn-state workbench) ':idle))
      (setf (workbench-queued-prompt workbench) nil)
      (setf (lightfast:value (workbench-composer workbench)) text)
      (workbench-send workbench)))
  nil)

(-> workbench-stop (workbench) null)
(defun workbench-stop (workbench)
  "Cancel the running turn, or the viewed job when the center shows one."
  (cond
    ((and (eq (workbench-center-mode workbench) ':job) (workbench-viewed-job workbench))
     (workbench-cancel-viewed-job workbench))
    ((member (workbench-turn-state workbench) '(:running :cancelling))
     (backend-cancel (workbench-backend workbench))))
  nil)

(defparameter *key-enter* #xff0d
  "FLTK's key number for Enter.")

(defparameter *key-keypad-enter* #xff8d
  "FLTK's key number for the keypad Enter.")

(defparameter *key-escape* #xff1b
  "FLTK's key number for Escape.")

(defparameter *state-control* #x40000
  "FLTK's state bit for Ctrl.")

(-> key-event-parts (string) (values (or null integer) integer))
(defun key-event-parts (value)
  "Return the key number and modifier state from Lightfast's key event VALUE,
or NIL and 0 when VALUE is not a key event."
  (let ((parts (parse-integers value)))
    (if (and (integerp (first parts)) (integerp (second parts)))
        (values (first parts) (second parts))
        (values nil 0))))

(defparameter *state-shift* #x10000
  "FLTK's state bit for Shift.")

(defparameter *state-alt* #x80000
  "FLTK's state bit for Alt.")

(defparameter *key-tab* #xff09
  "FLTK's key number for Tab.")

(defparameter *key-backspace* #xff08
  "FLTK's key number for Backspace.")

(defparameter *key-delete* #xffff
  "FLTK's key number for Delete.")

(defparameter *key-left* #xff51
  "FLTK's key number for the left arrow.")

(defparameter *key-right* #xff53
  "FLTK's key number for the right arrow.")

(-> workbench--composer-key (workbench string) null)
(defun workbench--composer-key (workbench value)
  "The composer's keys. Enter sends, Shift+Enter or Ctrl+Enter breaks the
line, Tab queues the text as a follow-up, Escape stops the turn and Ctrl+1 to
Ctrl+9 decide approvals. The line editing keys of a terminal composer apply:
Ctrl+A and Ctrl+E move to the ends of the line, Ctrl+K and Ctrl+U kill to the
end or the start of the line, Ctrl+W, Ctrl+Backspace and Alt+Backspace kill
the word before the caret, Ctrl+Delete and Alt+D the word after it, and
Ctrl+Left, Ctrl+Right, Alt+B and Alt+F move by a word."
  (multiple-value-bind (key state) (key-event-parts value)
    (let ((control-p (logtest state *state-control*))
          (shift-p (logtest state *state-shift*))
          (alt-p (logtest state *state-alt*))
          (composer (workbench-composer workbench)))
      (flet ((claim () (lightfast:consume-event))
             (letter-p (character) (eql key (char-code character))))
        (cond
          ((null key) nil)
          ((or (eql key *key-enter*) (eql key *key-keypad-enter*))
           (claim)
           (if (or shift-p control-p)
               (lightfast:text-insert composer (string #\Newline))
               (workbench-send workbench)))
          ((eql key *key-tab*)
           (claim)
           (workbench-queue-composer workbench))
          ((eql key *key-escape*)
           (claim)
           (workbench-stop workbench))
          ((and control-p (letter-p #\a)) (claim) (composer-move-line composer ':start))
          ((and control-p (letter-p #\e)) (claim) (composer-move-line composer ':end))
          ((and control-p (letter-p #\k)) (claim) (composer-kill-line composer ':end))
          ((and control-p (letter-p #\u)) (claim) (composer-kill-line composer ':start))
          ((and control-p (letter-p #\w)) (claim) (composer-kill-word composer ':backward))
          ((and (or control-p alt-p) (eql key *key-backspace*)) (claim) (composer-kill-word composer ':backward))
          ((and control-p (eql key *key-delete*)) (claim) (composer-kill-word composer ':forward))
          ((and control-p (eql key *key-left*)) (claim) (composer-move-word composer ':backward))
          ((and control-p (eql key *key-right*)) (claim) (composer-move-word composer ':forward))
          ((and alt-p (letter-p #\d)) (claim) (composer-kill-word composer ':forward))
          ((and alt-p (letter-p #\b)) (claim) (composer-move-word composer ':backward))
          ((and alt-p (letter-p #\f)) (claim) (composer-move-word composer ':forward))
          ((workbench--global-key workbench key control-p)
           (claim))))))
  nil)

(-> workbench-queue-composer (workbench) null)
(defun workbench-queue-composer (workbench)
  "Queue the composer text as a follow-up and clear the composer."
  (let* ((composer (workbench-composer workbench))
         (text (string-trim '(#\Space #\Newline #\Tab) (lightfast:value composer))))
    (when (plusp (length text))
      (workbench-queue-follow-up workbench text)
      (setf (lightfast:value composer) "")))
  nil)


;;; Line editing in the composer

(-> composer-caret-character (t) (values string integer))
(defun composer-caret-character (composer)
  "Return the composer text and the caret's character index within it."
  (let* ((text (lightfast:value composer))
         (bytes (lightfast:text-insert-position composer))
         (index 0)
         (count 0))
    (loop while (and (< index (length text)) (< count bytes))
          do (incf count (lightfast:character-byte-length (char text index)))
             (incf index))
    (values text index)))

(-> composer-set-caret-character (t string integer) null)
(defun composer-set-caret-character (composer text index)
  "Put the caret before character INDEX of TEXT."
  (setf (lightfast:text-insert-position composer)
        (lightfast:string-byte-length (subseq text 0 (min index (length text)))))
  nil)

(-> composer-delete-characters (t string integer integer) null)
(defun composer-delete-characters (composer text start end)
  "Delete characters START to END of TEXT from the composer and leave the
caret at START."
  (when (< start end)
    (lightfast:text-replace composer
                            (lightfast:string-byte-length (subseq text 0 start))
                            (lightfast:string-byte-length (subseq text 0 end))
                            "")
    (composer-set-caret-character composer text start))
  nil)

(-> line-bounds (string integer) (values integer integer))
(defun line-bounds (text index)
  "Return the start and end character indices of the line holding INDEX."
  (let ((start (let ((newline (position #\Newline text :end index :from-end t)))
                 (if newline (1+ newline) 0)))
        (end (or (position #\Newline text :start index) (length text))))
    (values start end)))

(-> composer-move-line (t (member :start :end)) null)
(defun composer-move-line (composer where)
  "Move the caret to the start or the end of its line."
  (multiple-value-bind (text index) (composer-caret-character composer)
    (multiple-value-bind (start end) (line-bounds text index)
      (composer-set-caret-character composer text (if (eq where ':start) start end))))
  nil)

(-> composer-kill-line (t (member :start :end)) null)
(defun composer-kill-line (composer where)
  "Delete from the caret to the end of its line, the line break itself when
the caret already sits at the end, or from the start of the line to the caret."
  (multiple-value-bind (text index) (composer-caret-character composer)
    (multiple-value-bind (start end) (line-bounds text index)
      (if (eq where ':end)
          (composer-delete-characters composer text index (if (= index end) (min (length text) (1+ end)) end))
          (composer-delete-characters composer text start index))))
  nil)

(-> word-boundary (string integer (member :backward :forward)) integer)
(defun word-boundary (text index direction)
  "Return the character index one word away from INDEX in DIRECTION."
  (flet ((word-char-p (position) (alphanumericp (char text position))))
    (ecase direction
      (:backward
       (let ((position index))
         (loop while (and (> position 0) (not (word-char-p (1- position)))) do (decf position))
         (loop while (and (> position 0) (word-char-p (1- position))) do (decf position))
         position))
      (:forward
       (let ((position index))
         (loop while (and (< position (length text)) (not (word-char-p position))) do (incf position))
         (loop while (and (< position (length text)) (word-char-p position)) do (incf position))
         position)))))

(-> composer-move-word (t (member :backward :forward)) null)
(defun composer-move-word (composer direction)
  "Move the caret by one word."
  (multiple-value-bind (text index) (composer-caret-character composer)
    (composer-set-caret-character composer text (word-boundary text index direction)))
  nil)

(-> composer-kill-word (t (member :backward :forward)) null)
(defun composer-kill-word (composer direction)
  "Delete one word before or after the caret."
  (multiple-value-bind (text index) (composer-caret-character composer)
    (let ((boundary (word-boundary text index direction)))
      (if (eq direction ':backward)
          (composer-delete-characters composer text boundary index)
          (composer-delete-characters composer text index boundary))))
  nil)

(-> workbench--global-key (workbench integer boolean) boolean)
(defun workbench--global-key (workbench key control-p)
  "Act on a shortcut that works anywhere in the window and return T when it did."
  (cond
    ((null key)
     nil)
    ((and (eql key *key-escape*) (not (eq (workbench-center-mode workbench) ':transcript)))
     (workbench-show-transcript workbench)
     t)
    ((and control-p (<= (char-code #\1) key (char-code #\9)))
     (workbench-decide workbench (- key (char-code #\0)))
     t)
    ((and control-p (eql key (char-code #\k)))
     (workbench-show-palette workbench)
     t)
    ((and control-p (eql key (char-code #\,)))
     (workbench-show-settings workbench)
     t)
    ((and control-p (eql key (char-code #\i)))
     (workbench-toggle-inspector workbench)
     t)
    ((and control-p (eql key (char-code #\o)))
     (workbench-open-conversations workbench)
     t)
    ((and control-p (eql key (char-code #\n)))
     (workbench-new-conversation workbench)
     t)
    ((and control-p (eql key (char-code #\q)))
     (lightfast:quit)
     t)
    (t
     nil)))

(-> workbench-open-conversations (workbench) null)
(defun workbench-open-conversations (workbench)
  "Show the conversation manager."
  (declare (ignore workbench))
  (when *open-conversations-function*
    (funcall *open-conversations-function*))
  nil)

(-> workbench-new-conversation (workbench) null)
(defun workbench-new-conversation (workbench)
  "Start a fresh conversation in this folder once the turn is over."
  (when (eq (workbench-turn-state workbench) ':idle)
    (backend-open-conversation (workbench-backend workbench)))
  nil)

(-> workbench--fill-menu (workbench) null)
(defun workbench--fill-menu (workbench)
  "Put the application's commands in the menu bar."
  (let ((menu (workbench--menu workbench)))
    (lightfast:set-box menu lightfast:+box-no-box+)
    (lightfast:set-text-font menu (font ':prose))
    (lightfast:set-text-size menu *prose-size*)
    (flet ((item (path function &key shortcut)
             (lightfast:add-menu-item menu path
                                      (lambda (widget value)
                                        (declare (ignore widget value))
                                        (funcall function))
                                      :shortcut (or shortcut 0))))
      (item "Autolith/Conversations..." (lambda () (workbench-open-conversations workbench))
            :shortcut (logior *shortcut-control* (char-code #\o)))
      (item "Autolith/New conversation here" (lambda () (workbench-new-conversation workbench))
            :shortcut (logior *shortcut-control* (char-code #\n)))
      (item "Autolith/Open another folder..." (lambda () (workbench-open-another-folder workbench))
            :shortcut (logior *shortcut-control* *shortcut-shift* (char-code #\o)))
      (item "Autolith/Restart Autolith" (lambda () (workbench-restart workbench)))
      (item "Autolith/Quit" (lambda () (lightfast:quit))
            :shortcut (logior *shortcut-control* (char-code #\q)))
      (item "View/Transcript" (lambda () (workbench-show-transcript workbench)))
      (item "View/Commands..." (lambda () (workbench-show-palette workbench))
            :shortcut (logior *shortcut-control* (char-code #\k)))
      (item "View/Settings..." (lambda () (workbench-show-settings workbench))
            :shortcut (logior *shortcut-control* (char-code #\,)))
      (item "View/Inspector" (lambda () (workbench-toggle-inspector workbench))
            :shortcut (logior *shortcut-control* (char-code #\i)))
      (item "View/Agenda" (lambda () (workbench-show-agenda workbench)))
      (item "View/Jump to latest" (lambda () (transcript-view-jump-to-end (workbench-transcript-view workbench))))
      (item "Turn/Stop" (lambda () (workbench-stop workbench)))
      (item "Turn/Full access from now" (lambda () (workbench-grant-full-access workbench)))))
  nil)

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

(-> workbench-agents (workbench) list)
(defun workbench-agents (workbench)
  "Return the jobs that are sub-agents: those with a role."
  (remove-if-not #'job-entry-role (workbench-jobs workbench)))

(-> workbench--agent-clicked (workbench string) null)
(defun workbench--agent-clicked (workbench value)
  "Show the sub-agent whose line was clicked in the rail."
  (destructuring-bind (x y &rest rest) (parse-integers value)
    (declare (ignore rest))
    (let* ((view (workbench--jobs-view workbench))
           (position (lightfast:text-position-at view x y))
           (line (and position (lightfast:text-line-of-position view position)))
           (job (and line (nth (1- line) (workbench-agents workbench)))))
      (when job
        (lightfast:consume-event)
        (workbench-show-job workbench job))))
  nil)

(defparameter *turning-marks* #("◐" "◓" "◑" "◒")
  "Quarter turns of a half-filled circle, the rail's mark for a running agent.")

(-> agent-mark (job-entry integer) (cons string integer))
(defun agent-mark (job phase)
  "Return the mark for JOB's state as (GLYPH . STYLE) for the rail: a turning
mark in green while it runs, a still one in ink when it is done, in red when
it failed, hollow while it waits."
  (case (job-entry-state job)
    ((:running) (cons (aref *turning-marks* (mod phase (length *turning-marks*))) 3))
    ((:completed) (cons "●" 1))
    ((:failed :abandoned) (cons "●" 4))
    ((:cancelled) (cons "●" 2))
    (t (cons "○" 1))))


(-> workbench-show-job (workbench job-entry) null)
(defun workbench-show-job (workbench job)
  "Show JOB's transcript in the center with the composer addressing it, and
keep it fresh while the job runs."
  (unless (and (workbench-viewed-job workbench)
               (equal (job-entry-id (workbench-viewed-job workbench)) (job-entry-id job)))
    (transcript-reset (workbench--job-transcript workbench))
    (setf (workbench--job-content workbench) nil)
    (transcript-notice (workbench--job-transcript workbench) :severity ':information :key ':loading
                       :text (format nil "Fetching the transcript of ~A" (job-entry-id job))))
  (setf (workbench-viewed-job workbench) job
        (workbench-center-mode workbench) ':job)
  (workbench--show-job-label workbench)
  (workbench--relayout workbench)
  (workbench--poll-job workbench)
  (unless (workbench--job-timer workbench)
    (setf (workbench--job-timer workbench)
          (lightfast:add-timeout *job-poll-interval* (lambda () (workbench--poll-job workbench)) :repeat t)))
  (workbench-focus-composer workbench)
  nil)

(-> workbench--show-job-label (workbench) null)
(defun workbench--show-job-label (workbench)
  "Describe the viewed job above its transcript."
  (let ((job (workbench-viewed-job workbench)))
    (when job
      (setf (lightfast:label (workbench--job-label workbench))
            (format nil "~A  ·  ~(~A~)~@[  ·  ~D s~]"
                    (or (job-entry-role job) (job-entry-id job)) (job-entry-state job) (job-entry-elapsed-seconds job)))
      ;; The composer's buttons say whom they address while a job is shown.
      (setf (lightfast:label (workbench--send-button workbench))
            (format nil "Send to ~A" (or (job-entry-role job) (job-entry-id job)))
            (lightfast:label (workbench--stop-button workbench)) "Cancel job")
      (if (workbench--stoppable-p workbench)
          (lightfast:show (workbench--stop-button workbench))
          (lightfast:hide (workbench--stop-button workbench)))))
  nil)

(-> workbench--poll-job (workbench) null)
(defun workbench--poll-job (workbench)
  "Ask for the viewed job's transcript while it is shown."
  (let ((job (workbench-viewed-job workbench)))
    (if (and job (eq (workbench-center-mode workbench) ':job))
        (backend-job-action (workbench-backend workbench) (job-entry-id job) ':inspect)
        (workbench--stop-job-timer workbench)))
  nil)

(-> workbench--stop-job-timer (workbench) null)
(defun workbench--stop-job-timer (workbench)
  "Stop refreshing the viewed job."
  (when (workbench--job-timer workbench)
    (lightfast:remove-timeout (workbench--job-timer workbench))
    (setf (workbench--job-timer workbench) nil))
  nil)

(-> workbench--job-transcript-arrived (workbench list) null)
(defun workbench--job-transcript-arrived (workbench value)
  "Render VALUE, a job transcript plist, when it belongs to the viewed job
and differs from what is shown."
  (let ((job (workbench-viewed-job workbench))
        (content (getf value ':content)))
    (when (and job (equal (job-entry-id job) (getf value ':job-id))
               (not (equal content (workbench--job-content workbench))))
      (setf (workbench--job-content workbench) content)
      (let ((transcript (workbench--job-transcript workbench)))
        (transcript-reset transcript)
        (if (plusp (length content))
            (transcript-append transcript (make-instance 'assistant-text :text content))
            (transcript-notice transcript :severity ':information :key ':empty
                               :text "The job has not written anything yet."))
        (transcript-view-jump-to-end (workbench--job-view workbench)))))
  nil)

(-> workbench-show-agenda (workbench) null)
(defun workbench-show-agenda (workbench)
  "Ask Autolith for the agenda and show its text in the aside view."
  (workbench-show-document workbench "Agenda" "Asking Autolith for the agenda")
  (setf (workbench--document-request workbench)
        (cons (backend-invoke (workbench-backend workbench) "agenda" nil) "Agenda"))
  nil)

(-> workbench-show-document (workbench string string) null)
(defun workbench-show-document (workbench title text)
  "Show TEXT under TITLE in the aside view, without a composer."
  (let ((transcript (workbench--job-transcript workbench)))
    (transcript-reset transcript)
    (transcript-append transcript (make-instance 'assistant-text :text text)))
  (setf (lightfast:label (workbench--job-label workbench)) title
        (workbench-viewed-job workbench) nil
        (workbench-center-mode workbench) ':document)
  (workbench--stop-job-timer workbench)
  (workbench--relayout workbench)
  nil)

(-> workbench--document-arrived (workbench result-event) boolean)
(defun workbench--document-arrived (workbench event)
  "Show the text of a command result the aside view awaits; T when it was."
  (let ((awaited (workbench--document-request workbench)))
    (when (and awaited (equal (car awaited) (result-event-request-id event)))
      (setf (workbench--document-request workbench) nil)
      (case (result-event-outcome event)
        (:ok
         (let* ((value (result-event-value event))
                (text (or (and (consp value) (cdr (assoc "text" value :test #'equal)))
                          "Autolith returned nothing to show.")))
           (workbench-show-document workbench (cdr awaited) text)))
        (t
         (workbench-show-document workbench (cdr awaited)
                                  (format nil "Autolith could not answer: ~A"
                                          (or (result-event-report event) "the request was not admitted")))))
      t)))

(-> workbench-cancel-viewed-job (workbench) null)
(defun workbench-cancel-viewed-job (workbench)
  "Cancel the job shown in the center."
  (let ((job (workbench-viewed-job workbench)))
    (when job
      (backend-job-action (workbench-backend workbench) (job-entry-id job) ':cancel)))
  nil)

(-> workbench--window-key (workbench string) null)
(defun workbench--window-key (workbench value)
  "Handle a key that no focused widget claimed."
  (multiple-value-bind (key state) (key-event-parts value)
    (when (and key (workbench--global-key workbench key (logtest state *state-control*)))
      (lightfast:consume-event)))
  nil)

(-> workbench-show-transcript (workbench) null)
(defun workbench-show-transcript (workbench)
  "Return the center of the window to the conversation."
  (setf (workbench-center-mode workbench) ':transcript
        (workbench-viewed-job workbench) nil)
  (workbench--stop-job-timer workbench)
  (workbench--relayout workbench)
  (workbench-focus-composer workbench)
  nil)

(-> workbench-show-settings (workbench &optional (or null setting-entry)) null)
(defun workbench-show-settings (workbench &optional entry)
  "Show the settings pane, on ENTRY's group when given."
  (let ((pane (workbench-settings-pane workbench)))
    (setf (workbench-center-mode workbench) ':settings)
    (workbench--relayout workbench)
    (settings-pane-refresh pane)
    (when entry
      (settings-pane-show-entry pane entry)))
  nil)

(-> workbench-show-palette (workbench) null)
(defun workbench-show-palette (workbench)
  "Show the command palette pane."
  (setf (workbench-center-mode workbench) ':commands)
  (workbench--relayout workbench)
  (palette-open (workbench-palette workbench))
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
                        :text (format nil "~A is unavailable through this interface." (operation-entry-name operation))))
    (t
     (let ((arguments (if (operation-needs-arguments-p operation)
                          (lightfast:input-dialog
                           (format nil "Arguments for ~A as a JSON object~@[; fields: ~{~A~^, ~}~]"
                                   (operation-entry-name operation)
                                   (operation-argument-names operation)))
                          "")))
       (when arguments
         (backend-invoke (workbench-backend workbench) (operation-entry-name operation) arguments)))))
  nil)

(-> operation-argument-names (operation-entry) list)
(defun operation-argument-names (operation)
  "Return the argument names OPERATION's schema declares, when it is an object schema."
  (let ((properties (plain-object-field (operation-entry-argument-schema operation) "properties")))
    (and (consp properties) (mapcar #'first properties))))

(-> operation-needs-arguments-p (operation-entry) boolean)
(defun operation-needs-arguments-p (operation)
  "Return T when OPERATION declares at least one argument."
  (and (operation-argument-names operation) t))

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

(-> workbench-same-folder-p (workbench conversation-entry) boolean)
(defun workbench-same-folder-p (workbench entry)
  "Return T when ENTRY lives in this workbench's folder."
  (string= (namestring (uiop:ensure-directory-pathname (backend-directory (workbench-backend workbench))))
           (namestring (uiop:ensure-directory-pathname (conversation-entry-directory entry)))))

(-> workbench-restart (workbench) null)
(defun workbench-restart (workbench)
  "Replace the exited backend with a fresh one for the same folder and reopen
the current conversation. Events from the old backend no longer find this
workbench and are dropped."
  (let* ((old (workbench-backend workbench))
         (new (make-backend (backend-directory old) (backend-queue old))))
    (backend-stop old)
    (push old *stopped-backends*)
    (setf (workbench-backend workbench) new
          (workbench-connection-state workbench) ':starting)
    (workbench--refresh-status workbench)
    (backend-start new)
    (backend-open-conversation new :conversation-id (workbench-conversation-id workbench)))
  nil)

(-> workbench-open-another-folder (workbench) null)
(defun workbench-open-another-folder (workbench)
  "Ask for a folder and open a new workspace window there. A workspace is one
Autolith process in one folder, so another folder means another window."
  (let ((directory (lightfast:choose-directory :title "Choose a folder for Autolith"
                                               :preset-path (backend-directory (workbench-backend workbench)))))
    (when (and directory (plusp (length directory)) (uiop:directory-exists-p directory))
      (open-workspace (namestring (uiop:ensure-directory-pathname directory)))))
  nil)

(-> workbench-close (workbench) null)
(defun workbench-close (workbench)
  "Stop the backend and forget the workbench; the window closes with it."
  (unless (workbench-closed-p workbench)
    (setf (workbench-closed-p workbench) t)
    (setf *workbenches* (remove workbench *workbenches*))
    (push (workbench-backend workbench) *stopped-backends*)
    (workbench--animate workbench nil)
    (workbench--stop-job-timer workbench)
    (backend-stop (workbench-backend workbench)))
  nil)

(-> workbench-title-for (workbench (or null string) (or null string)) string)
(defun workbench-title-for (workbench conversation-id title)
  "Return the title to show for CONVERSATION-ID: TITLE when given, else the
catalog's title, else the id, else a name for a fresh conversation."
  (or title
      (let ((entry (and conversation-id
                        (find conversation-id (workbench-conversations workbench)
                              :key #'conversation-entry-id :test #'equal))))
        (and entry (conversation-entry-title entry)))
      (and conversation-id (conversation-display-id conversation-id))
      "New conversation"))


;;; Drafts

(-> workbench--save-draft (workbench) null)
(defun workbench--save-draft (workbench)
  "Keep the composer text for the open conversation."
  (let ((id (workbench-conversation-id workbench)))
    (when id
      (setf (gethash id (workbench--drafts workbench)) (lightfast:value (workbench-composer workbench)))))
  nil)

(-> workbench--restore-draft (workbench) null)
(defun workbench--restore-draft (workbench)
  "Put back the draft kept for the open conversation. Text typed while the
session was still opening stays when no draft is kept."
  (let ((draft (gethash (workbench-conversation-id workbench) (workbench--drafts workbench))))
    (when (or draft (string= (lightfast:value (workbench-composer workbench)) ""))
      (setf (lightfast:value (workbench-composer workbench)) (or draft ""))))
  nil)


;;; Presenting state

(-> workbench--refresh-status (workbench) null)
(defun workbench--refresh-status (workbench)
  "Rewrite the status line and the header state word from current state."
  (let* ((snapshot (workbench-snapshot workbench))
         (fraction (and snapshot (state-snapshot-context-fraction snapshot)))
         (pending (workbench-pending-approval workbench))
         (remaining (and pending (approval-deadline pending)
                         (max 0 (- (approval-deadline pending) (get-universal-time)))))
         (connection (string-downcase (symbol-name (workbench-connection-state workbench)))))
    (setf (lightfast:label (workbench--status-label workbench))
          (let ((count (length (workbench-follow-ups workbench))))
            (if (plusp count) (format nil "~D follow-up~:P queued" count) "")))
    (setf (lightfast:label (workbench--approval-countdown workbench))
          (if remaining (format nil "~D s" remaining) ""))
    (let ((bar (workbench--context-bar workbench)))
      (cond
        (fraction
         (setf (lightfast:value bar) (format nil "~,2F" (* 100 fraction)))
         (lightfast:set-tooltip bar (format nil "Context ~D percent full" (round (* 100 fraction))))
         (lightfast:show bar))
        (t
         (lightfast:hide bar))))
    ;; The header shows the state as a mark, not a word: a turning spinner
    ;; while something runs, a still dot when something went wrong, nothing
    ;; when idle. The word lives in the mark's tooltip.
    (destructuring-bind (kind color word)
        (cond
          ((member (workbench-connection-state workbench) '(:lost :exited)) (list ':trouble (accent ':red) connection))
          ((eq (workbench-connection-state workbench) ':starting) (list ':busy *ink* "starting"))
          ((eq (workbench-session-state workbench) ':loading) (list ':busy *ink* "loading"))
          ((eq (workbench-turn-state workbench) ':running) (list ':busy (accent ':green) "working"))
          ((eq (workbench-turn-state workbench) ':cancelling) (list ':busy (accent ':orange) "stopping"))
          ((eq (workbench-turn-state workbench) ':interrupted) (list ':trouble (accent ':red) "interrupted"))
          (t (list ':idle *ink* "")))
      (let ((changed (not (eq kind (workbench--state-kind workbench)))))
        (setf (workbench--state-kind workbench) kind
              (workbench--state-color workbench) color)
        (when changed
          (workbench--relayout workbench)))
      (lightfast:set-tooltip (workbench--state-mark workbench) word)
      (lightfast:redraw (workbench--state-mark workbench))
      (workbench--animate workbench (or (eq kind ':busy)
                                        (and (find ':running (workbench-agents workbench) :key #'job-entry-state) t)))))
  nil)

(-> workbench--draw-state-mark (workbench t) null)
(defun workbench--draw-state-mark (workbench canvas)
  "Draw the state mark on CANVAS: a ring of dots with a lit head chasing
around while busy, a still dot in trouble, nothing when idle."
  (let* ((x (lightfast:widget-x canvas))
         (y (lightfast:widget-y canvas))
         (size (lightfast:widget-width canvas))
         (center-x (+ x (floor size 2)))
         (center-y (+ y (floor size 2)))
         (ring (floor (- size 8) 2))
         (dot 3))
    (destructuring-bind (red green blue) *paper*
      (lightfast:draw-color-rgb :red red :green green :blue blue))
    (lightfast:draw-filled-rect x y size size)
    (destructuring-bind (red green blue) (workbench--state-color workbench)
      (lightfast:draw-color-rgb :red red :green green :blue blue))
    (ecase (workbench--state-kind workbench)
      (:idle nil)
      (:trouble
       (lightfast:draw-filled-circle center-x center-y (+ dot 2)))
      (:busy
       (let ((head (mod (workbench--animation-phase workbench) *spinner-dots*)))
         (dotimes (index *spinner-dots*)
           (let* ((angle (* 2 pi (/ index *spinner-dots*)))
                  (dot-x (round (+ center-x (* ring (sin angle)))))
                  (dot-y (round (- center-y (* ring (cos angle)))))
                  (distance (mod (- head index) *spinner-dots*)))
             ;; The head and two trailing dots are lit; the tail fades to the
             ;; mark tier of the same hue, and the rest stay unlit.
             (cond
               ((= distance 0)
                (lightfast:draw-filled-circle dot-x dot-y dot))
               ((<= distance 2)
                (destructuring-bind (red green blue) (workbench--state-tail-color workbench)
                  (lightfast:draw-color-rgb :red red :green green :blue blue))
                (lightfast:draw-filled-circle dot-x dot-y (1- dot))
                (destructuring-bind (red green blue) (workbench--state-color workbench)
                  (lightfast:draw-color-rgb :red red :green green :blue blue)))))))))
    nil))

(-> workbench--state-tail-color (workbench) list)
(defun workbench--state-tail-color (workbench)
  "Return the lighter tier of the state color for the spinner's tail."
  (let ((color (workbench--state-color workbench)))
    (cond
      ((equal color (accent ':green)) (accent ':green :tier ':mark))
      ((equal color (accent ':orange)) (accent ':orange :tier ':mark))
      ((equal color (accent ':red)) (accent ':red :tier ':mark))
      (t color))))

(-> workbench--show-approval (workbench approval) null)
(defun workbench--show-approval (workbench approval)
  "Present APPROVAL in the decision row with one button per offered option."
  (setf (workbench-pending-approval workbench) approval)
  (dolist (button (workbench--approval-buttons workbench))
    (lightfast:destroy button))
  (setf (workbench--approval-buttons workbench)
        (append
         (loop for option in (approval-options approval)
               for number from 1
               collect (let* ((number number)
                              (button (lightfast:make-button :parent (workbench--approval-row workbench)
                                                             :label (second option)
                                                             :height *control-height*
                                                             :callback (lambda (widget event value)
                                                                         (declare (ignore widget event value))
                                                                         (workbench-decide workbench number)))))
                         (lightfast:set-tooltip button (format nil "Ctrl+~D" number))
                         button))
         (list (let ((button (lightfast:make-button :parent (workbench--approval-row workbench)
                                                    :label "Full access"
                                                    :height *control-height*
                                                    :callback (lambda (widget event value)
                                                                (declare (ignore widget event value))
                                                                (workbench-grant-full-access workbench)))))
                 (lightfast:set-tooltip button "Allow this and switch the session to full access, asking no more")
                 button))))
  (setf (lightfast:label (workbench--approval-label workbench))
        (format nil "Approve ~A?" (approval-title approval)))
  (setf (lightfast:label (workbench--approval-detail workbench))
        (let ((arguments (approval-arguments approval)))
          (if arguments
              (bounded-text (substitute #\Space #\Newline (readable-data-text arguments)) *approval-detail-characters*)
              "")))
  (let ((primary (or (position-if (lambda (option) (eq (third option) ':allow-once)) (approval-options approval))
                     (position-if (lambda (option) (eq (third option) ':allow-always)) (approval-options approval)))))
    (when primary
      (destructuring-bind (red green blue) (accent ':green :tier ':mark)
        (lightfast:set-color-rgb (nth primary (workbench--approval-buttons workbench)) :red red :green green :blue blue))))
  (workbench--relayout workbench)
  (workbench--refresh-status workbench)
  (unless (workbench--countdown-timer workbench)
    (setf (workbench--countdown-timer workbench)
          (lightfast:add-timeout 1.0 (lambda () (workbench--refresh-status workbench)) :repeat t)))
  nil)

(-> workbench-grant-full-access (workbench) null)
(defun workbench-grant-full-access (workbench)
  "Allow the pending request and switch the session to full access so that no
further approvals are asked."
  (let ((approval (workbench-pending-approval workbench)))
    (when approval
      (let ((allow (or (position-if (lambda (option) (eq (third option) ':allow-always)) (approval-options approval))
                       (position-if (lambda (option) (eq (third option) ':allow-once)) (approval-options approval))
                       0)))
        (workbench-decide workbench (1+ allow))
        (backend-set-mode (workbench-backend workbench) "full-access"))))
  nil)

(-> workbench--clear-approval (workbench) null)
(defun workbench--clear-approval (workbench)
  "Take down the decision row."
  (setf (workbench-pending-approval workbench) nil)
  (dolist (button (workbench--approval-buttons workbench))
    (lightfast:destroy button))
  (setf (workbench--approval-buttons workbench) nil)
  (when (workbench--countdown-timer workbench)
    (lightfast:remove-timeout (workbench--countdown-timer workbench))
    (setf (workbench--countdown-timer workbench) nil))
  (workbench--relayout workbench)
  (workbench--refresh-status workbench)
  nil)

(-> folder-tail (string) string)
(defun folder-tail (directory)
  "Return the last component of DIRECTORY for a compact label."
  (let* ((trimmed (string-right-trim "/" directory))
         (slash (position #\/ trimmed :from-end t)))
    (if slash (subseq trimmed (1+ slash)) trimmed)))

(-> workbench--show-conversations (workbench list) null)
(defun workbench--show-conversations (workbench entries)
  "Keep ENTRIES, most recent first, for the title and the conversation manager."
  (setf (workbench-conversations workbench) (conversations-by-recency entries))
  nil)

(-> workbench--show-jobs (workbench) null)
(defun workbench--show-jobs (workbench)
  "List the sub-agents in the rail, a mark and a name each."
  (workbench--show-agent-marks workbench)
  (workbench--relayout workbench)
  nil)

(-> list-height (integer integer) integer)
(defun list-height (rows cap)
  "Return the height a list of ROWS rows wants, at least one row and at most
CAP, so that the rail's sections follow each other in reading order."
  (min cap (+ (spacing 2) (* (max 1 rows) (+ *prose-size* *list-line-spacing* 6)))))

(-> workbench--show-agent-marks (workbench) null)
(defun workbench--show-agent-marks (workbench)
  "Turn the running agents' marks in the rail without relaying the window."
  (let* ((view (workbench--jobs-view workbench))
         (agents (workbench-agents workbench))
         (phase (workbench--animation-phase workbench))
         (runs (loop for job in agents
                     for index from 1
                     for mark = (agent-mark job phase)
                     collect (cons (format nil "~A " (car mark)) (cdr mark))
                     collect (cons (format nil "~A~:[~%~;~]" (job-entry-role job) (= index (length agents))) 0))))
    (lightfast:text-replace-styled view 0 (lightfast:text-length view) runs))
  nil)

(-> workbench--show-plan (workbench) null)
(defun workbench--show-plan (workbench)
  "Show the agent's plan in the rail, one line per entry with a state glyph,
or hide the section when there is no plan."
  (let* ((block (transcript-last-block (workbench-transcript workbench) 'plan-block))
         (entries (and block (plan-block-entries block)))
         (view (workbench--plan-view workbench))
         (width (- *rail-width* *plan-indent*))
         (indent (make-string (plan-indent-spaces) :initial-element #\Space))
         (wrapped (loop for (text) in entries collect (wrap-words text width)))
         (runs (loop for (nil . status) in entries
                     for lines in wrapped
                     for index from 1
                     collect (cons (plan-entry-marker status)
                                   (ecase status (:pending 1) (:in-progress 2) (:completed 3)))
                     collect (cons (format nil "~{~A~^~%~}~:[~%~;~]"
                                           (cons (first lines)
                                                 (mapcar (lambda (line) (concatenate 'string indent line)) (rest lines)))
                                           (= index (length entries)))
                                   0))))
    (lightfast:text-replace-styled view 0 (lightfast:text-length view) runs)
    (let ((lines (loop for lines in wrapped sum (length lines))))
      (unless (= lines (workbench--plan-lines workbench))
        (setf (workbench--plan-lines workbench) lines)
        (if entries
            (progn (lightfast:show view) (lightfast:show (workbench--plan-label workbench)))
            (progn (lightfast:hide view) (lightfast:hide (workbench--plan-label workbench))))
        (workbench--relayout workbench))))
  nil)

(-> plan-indent-spaces () integer)
(defun plan-indent-spaces ()
  "Return how many prose spaces are as wide as a plan marker and its space,
so that wrapped plan lines hang under the text, not the mark."
  (let ((marker (lightfast:measure-text (plan-entry-marker ':pending) :font (font ':marker) :size *marker-size*))
        (space (max 1 (lightfast:measure-text " " :font (font ':prose) :size *prose-size*))))
    (max 1 (round marker space))))

(-> wrap-words (string integer) list)
(defun wrap-words (text width)
  "Break TEXT at spaces into lines no wider than WIDTH pixels of prose,
measured; a word wider than WIDTH stands on its own line."
  (let ((lines nil)
        (line ""))
    (dolist (word (split-words text))
      (let ((candidate (if (string= line "") word (format nil "~A ~A" line word))))
        (if (or (string= line "")
                (<= (lightfast:measure-text candidate :font (font ':prose) :size *prose-size*) width))
            (setf line candidate)
            (progn (push line lines) (setf line word)))))
    (push line lines)
    (nreverse lines)))

(-> split-words (string) list)
(defun split-words (text)
  "Return the space-separated words of TEXT, empty ones dropped."
  (loop with start = 0
        for end = (position #\Space text :start start)
        for word = (subseq text start end)
        when (plusp (length word)) collect word
        while end
        do (setf start (1+ end))))

(-> label-width (string) integer)
(defun label-width (text)
  "Return the width of TEXT set as a label in the prose face."
  (lightfast:measure-text text :font (font ':prose) :size *prose-size*))

(-> workbench--animate (workbench t) null)
(defun workbench--animate (workbench active-p)
  "Run the state word's animation while ACTIVE-P, stop it otherwise."
  (cond
    ((and active-p (null (workbench--animation-timer workbench)))
     (setf (workbench--animation-timer workbench)
           (lightfast:add-timeout *animation-interval*
                                  (lambda ()
                                    (incf (workbench--animation-phase workbench))
                                    (workbench--refresh-status workbench)
                                    (when (find ':running (workbench-agents workbench) :key #'job-entry-state)
                                      (workbench--show-agent-marks workbench)))
                                  :repeat t)))
    ((and (not active-p) (workbench--animation-timer workbench))
     (lightfast:remove-timeout (workbench--animation-timer workbench))
     (setf (workbench--animation-timer workbench) nil
           (workbench--animation-phase workbench) 0)))
  nil)

(-> workbench--apply-snapshot (workbench state-snapshot) null)
(defun workbench--apply-snapshot (workbench snapshot)
  "Show SNAPSHOT in the header."
  (setf (workbench-snapshot workbench) snapshot)
  (when (state-snapshot-title snapshot)
    (setf (lightfast:label (workbench--title-label workbench)) (state-snapshot-title snapshot)))
  (workbench--set-chip (workbench--model-chip workbench) (or (state-snapshot-model snapshot) "model"))
  (workbench--set-chip (workbench--effort-chip workbench) (or (state-snapshot-effort snapshot) "effort"))
  (workbench--set-chip (workbench--permission-chip workbench)
                       (if (state-snapshot-permission-mode snapshot)
                           (string-downcase (symbol-name (state-snapshot-permission-mode snapshot)))
                           "permissions"))
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

(-> workbench-focus-composer (workbench) null)
(defun workbench-focus-composer (workbench)
  "Give the composer the keyboard focus."
  (lightfast:take-focus (workbench-composer workbench))
  nil)

(defmethod apply-ui-event ((event session-event) (workbench workbench))
  (setf (workbench-session-state workbench) (session-event-state event))
  (ecase (session-event-state event)
    (:loading
     ;; The backend owns the transcript reset and replay brackets.
     (workbench--save-draft workbench)
     (setf (lightfast:label (workbench--title-label workbench))
           (workbench-title-for workbench (session-event-conversation-id event)
                                (session-event-title event))))
    (:ready
     (transcript-view-jump-to-end (workbench-transcript-view workbench))
     (setf (workbench-conversation-id workbench) (session-event-conversation-id event))
     (workbench--restore-draft workbench)
     (workbench-focus-composer workbench)
     (workbench--send-queued-prompt workbench)
     (workbench--send-follow-up workbench)
     (setf (lightfast:label (workbench--title-label workbench))
           (workbench-title-for workbench (session-event-conversation-id event)
                                (session-event-title event))))
    (:closed
     (workbench--save-draft workbench)
     (setf (workbench-conversation-id workbench) nil)))
  (workbench--refresh-status workbench))

(defmethod apply-ui-event ((event turn-event) (workbench workbench))
  (setf (workbench-turn-state workbench) (turn-event-state event))
  (workbench--relayout workbench)
  (when (eq (turn-event-state event) ':idle)
    (workbench--send-follow-up workbench))
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
  (when (or (member (transcript-event-operation event) '(:replace-plan :reset))
            (and (eq (transcript-event-operation event) ':append)
                 (eq (getf (transcript-event-arguments event) ':class) 'plan-block)))
    (workbench--show-plan workbench))
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
  (let ((viewed (workbench-viewed-job workbench)))
    (when viewed
      (let ((current (find (job-entry-id viewed) (workbench-jobs workbench) :key #'job-entry-id :test #'equal)))
        (when current
          (setf (workbench-viewed-job workbench) current)
          (workbench--show-job-label workbench)))))
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
     (when (eq (workbench-center-mode workbench) ':settings)
       (settings-pane-refresh (workbench-settings-pane workbench))))
    (:models
     (setf (workbench-models workbench) (catalog-event-items event)))
    (:operations
     (setf (workbench-operations workbench) (catalog-event-items event)))))

(defmethod apply-ui-event ((event result-event) (workbench workbench))
  (when (workbench--document-arrived workbench event)
    (return-from apply-ui-event nil))
  (case (result-event-outcome event)
    (:ok
     (let ((value (result-event-value event)))
       (when (and (consp value) (eq (first value) ':job-id) (getf value ':content))
         (workbench--job-transcript-arrived workbench value)
         (inspector-show-job-transcript (workbench-inspector workbench) value))))
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
