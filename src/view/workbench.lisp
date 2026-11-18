(in-package #:aui)

;;;; -- Workbench --

;;; The window's state and its dealings with the client layer: which channel
;;; is shown, what it last said about itself, what waits for the person's
;;; answer, and how each event that crosses from a client thread changes
;;; that. The panes in the other files of this directory only read this
;;; state and call the actions here. Everything runs on the interface thread.

(defclass workbench ()
  ((launch
    :initarg       :launch
    :reader        workbench-launch
    :type          launch
    :documentation "What the command line asked for.")
   (world
    :initarg       :world
    :reader        workbench-world
    :type          world
    :documentation "The world of channels the window looks onto.")
   (queue
    :initarg       :queue
    :reader        workbench-queue
    :type          ui-queue
    :documentation "Where the client threads post what happened.")
   (window
    :initform      nil
    :accessor      workbench-window
    :documentation "The window.")
   (stage
    :initform      nil
    :accessor      workbench-stage
    :documentation "The window's drawing surface.")
   (generation
    :initform      0
    :accessor      workbench-generation
    :type          integer
    :documentation "Counts channel attachments; events of an earlier one are dropped.")
   (session
    :initform      nil
    :accessor      workbench-session
    :documentation "The attachment to the channel shown, or NIL.")
   (transcript
    :initform      (make-transcript)
    :accessor      workbench-transcript
    :type          transcript
    :documentation "The shown channel's history.")
   (projector
    :initform      nil
    :accessor      workbench-projector
    :documentation "Turns the shown channel's records and events into the transcript.")
   (document
    :initform      nil
    :accessor      workbench-document
    :documentation "The scrolling view of the transcript.")
   (attachment
    :initform      ':none
    :accessor      workbench-attachment
    :type          (member :none :opening :attached :failed :closed)
    :documentation "Where the attachment to the shown channel stands.")
   (progress
    :initform      nil
    :accessor      workbench-progress
    :type          list
    :documentation "The (STEP DESCRIPTION) of the opening under way, or NIL.")
   (failure
    :initform      nil
    :accessor      workbench-failure
    :type          (or null string)
    :documentation "Why the last opening failed or the channel went away, or NIL.")
   (status
    :initform      nil
    :accessor      workbench-status
    :type          list
    :documentation "The channel's status as it last reported it: a property list with :STATE, :MODEL, :CWD and so on.")
   (state
    :initform      nil
    :accessor      workbench-state
    :documentation "The channel's richer state in portable form: usage, title, plan presence.")
   (working-p
    :initform      nil
    :accessor      workbench-working-p
    :type          boolean
    :documentation "Whether the channel is doing work the person may stop.")
   (working-since
    :initform      nil
    :accessor      workbench-working-since
    :type          (or null integer)
    :documentation "The universal time the current work began, for the elapsed clock.")
   (approval
    :initform      nil
    :accessor      workbench-approval
    :type          list
    :documentation "The approval waiting for an answer as a property list of :ID, :KIND, :TITLE, :ENTRY and :OPTIONS, or NIL.")
   (pending
    :initform      nil
    :accessor      workbench-pending
    :type          list
    :documentation "Messages sent and not yet taken up by the agent, oldest first, each (IDENTIFIER TEXT STEER-P).")
   (operations
    :initform      nil
    :accessor      workbench-operations
    :type          list
    :documentation "The channel's commands and tools as portable rows, once fetched.")
   (settings
    :initform      nil
    :accessor      workbench-settings
    :type          list
    :documentation "The channel's settings as portable rows, once fetched.")
   (models
    :initform      nil
    :accessor      workbench-models
    :type          list
    :documentation "The models on offer as portable rows, once fetched.")
   (jobs
    :initform      nil
    :accessor      workbench-jobs
    :type          list
    :documentation "The channel's background jobs as portable rows, as last fetched.")
   (jobs-fetched-at
    :initform      0
    :accessor      workbench-jobs-fetched-at
    :type          integer
    :documentation "The universal time the jobs were last asked for.")
   (notice
    :initform      nil
    :accessor      workbench-notice
    :type          (or null string)
    :documentation "A line telling the person something did not work, shown above the composer until the next action.")
   (sidebar-shown-p
    :initform      t
    :accessor      workbench-sidebar-shown-p
    :type          boolean
    :documentation "Whether the person wants the channel list beside the conversation, on a window wide enough for both.")
   (drawer-open-p
    :initform      nil
    :accessor      workbench-drawer-open-p
    :type          boolean
    :documentation "Whether the channel list is open on a window too narrow to hold it beside the conversation, where it takes the whole window.")
   (sidebar-scroll
    :initform      0
    :accessor      workbench-sidebar-scroll
    :type          integer
    :documentation "How far the channel list is scrolled.")
   (filter
    :initform      ""
    :accessor      workbench-filter
    :type          string
    :documentation "What the person typed to narrow the channel list.")
   (editor
    :initform      nil
    :accessor      workbench-editor
    :documentation "The composer's native text editor.")
   (attachments
    :initform      nil
    :accessor      workbench-attachments
    :type          list
    :documentation "Namestrings of the images that go with the next message.")
   (overlay
    :initform      nil
    :accessor      workbench-overlay
    :documentation "The view laid over the window, such as the command list, or NIL.")
   (completion-index
    :initform      0
    :accessor      workbench-completion-index
    :type          integer
    :documentation "Which of the composer's completions is highlighted.")
   (completion-dismissed
    :initform      nil
    :accessor      workbench-completion-dismissed
    :type          (or null string)
    :documentation "The draft for which the person put the completions away, or NIL.")
   (history
    :initform      nil
    :accessor      workbench-history
    :type          list
    :documentation "What the person sent from this window, newest first, for recall with the arrow keys.")
   (history-index
    :initform      nil
    :accessor      workbench-history-index
    :type          (or null integer)
    :documentation "Which sent text the composer is showing while the person steps through them, or NIL.")
   (demo-entries
    :initform      nil
    :accessor      workbench-demo-entries
    :type          list
    :documentation "The channel tree shown by the scripted demonstration, in place of a world's.")
   (waiting
    :initform      nil
    :accessor      workbench--waiting
    :type          list
    :documentation "What the channel's own snapshot says it has not taken up yet, each as (TEXT STEER-P).")
   (waiting-pathname
    :initform      nil
    :accessor      workbench--waiting-pathname
    :documentation "The file of the channel's snapshot of waiting messages, :NONE when it has none, or NIL before it was looked for.")
   (waiting-read-at
    :initform      0
    :accessor      workbench--waiting-read-at
    :type          integer
    :documentation "When the snapshot of waiting messages was last read, in internal time units.")
   (digest
    :initform      nil
    :accessor      workbench--digest
    :documentation "What was read from the conversation shown for the console: its plan, files and messages, or NIL before the first reading.")
   (goal-record
    :initform      nil
    :accessor      workbench--goal-record
    :type          list
    :documentation "The newest goal record seen of the channel shown, or NIL when none was seen.")
   (goal-layout
    :initform      nil
    :accessor      workbench--goal-layout
    :type          list
    :documentation "The goal's objective as last laid out for the console, as (OBJECTIVE WIDTH BLOCK), or NIL.")
   (console-scroll
    :initform      0
    :accessor      workbench-console-scroll
    :type          integer
    :documentation "How far the console's sections are scrolled.")
   (console-share
    :initform      nil
    :accessor      workbench-console-share
    :type          (or null real)
    :documentation "The part of the width beside the channel list that the console takes, as the person left it, or NIL for what was remembered."))
  (:documentation "One AUI window and everything it knows."))

(-> workbench-redraw (workbench) null)
(defun workbench-redraw (workbench)
  "Ask for WORKBENCH's window to be drawn again."
  (when (workbench-stage workbench)
    (stage-redraw (workbench-stage workbench)))
  nil)

(-> workbench-demo-p (workbench) boolean)
(defun workbench-demo-p (workbench)
  "Return T when WORKBENCH shows the scripted demonstration."
  (launch-demo-p (workbench-launch workbench)))

(-> workbench-attached-p (workbench) boolean)
(defun workbench-attached-p (workbench)
  "Return T when WORKBENCH is attached to a channel and can talk to it."
  (and (eq (workbench-attachment workbench) ':attached) t))

(-> workbench-channel (workbench) (or null string))
(defun workbench-channel (workbench)
  "Return the identifier of the channel WORKBENCH shows, or NIL."
  (let ((session (workbench-session workbench)))
    (or (and session (session-identifier session))
        (getf (workbench-status workbench) :session-id))))


;;;; -- What the Channel Says About Itself --

(-> workbench-title (workbench) string)
(defun workbench-title (workbench)
  "Return the shown channel's title, or words for a channel that has none yet."
  (or (portable-string (getf (workbench-status workbench) :conversation-title))
      (portable-string (portable-get (workbench-state workbench) "title"))
      ;; The channel tree hears of a first turn's title before the turn ends.
      (and (not (workbench-demo-p workbench))
           (world-channel-title (workbench-world workbench) (workbench-channel workbench)))
      (case (workbench-attachment workbench)
        (:none
         "Autolith")
        (:opening
         "Opening")
        (otherwise
         "New channel"))))

(-> workbench-model (workbench) (or null string))
(defun workbench-model (workbench)
  "Return the shown channel's model, or NIL when unknown."
  (or (portable-string (portable-get (workbench-state workbench) "model"))
      (portable-string (getf (workbench-status workbench) :model))))

(-> workbench-effort (workbench) (or null string))
(defun workbench-effort (workbench)
  "Return the shown channel's reasoning effort, or NIL when unknown."
  (or (portable-string (portable-get (workbench-state workbench) "effort"))
      (portable-string (getf (workbench-status workbench) :reasoning-effort))))

(-> workbench-permission-mode (workbench) keyword)
(defun workbench-permission-mode (workbench)
  "Return the shown channel's permission mode."
  (let ((mode (getf (workbench-status workbench) :permission-mode)))
    (if (keywordp mode) mode ':ask)))

(-> workbench-directory (workbench) string)
(defun workbench-directory (workbench)
  "Return the folder the shown channel works in, or the launch directory before a channel is attached.

The channel's state, fetched after each piece of work, names the folder it
works in now; the status it sends by itself can be older than a change of
folder."
  (or (portable-string (portable-get (workbench-state workbench) "workspace"))
      (portable-string (getf (workbench-status workbench) :cwd))
      (world-working-directory (workbench-world workbench))))

(-> workbench-context (workbench) (values (or null integer) (or null integer) boolean))
(defun workbench-context (workbench)
  "Return the tokens of context used and the context limit, each NIL when unknown, and whether Autolith estimated the use instead of measuring it."
  (let ((usage (portable-get (workbench-state workbench) "usage")))
    (values (let ((used (portable-get usage "contextUsed")))
              (and (integerp used) used))
            (let ((limit (portable-get usage "contextLimit")))
              (and (integerp limit) limit))
            (eq (portable-get usage "contextMeasured") ':false))))

(-> workbench-goal (workbench) list)
(defun workbench-goal (workbench)
  "Return the goal of the channel shown as a property list of :OBJECTIVE, :STATUS, :KIND and :CREATED-AT, or NIL when it has none.

A channel whose state names its goal is taken at its word. One served by an
Autolith that does not name it there has the goal of the newest goal record
seen: the one at the head of the newest part of its history, those on the
pages of history read so far, and those that arrive while the window looks."
  (let ((state (workbench-state workbench)))
    (if (portable-member-p state "goal")
        (portable-goal (portable-get state "goal"))
        (let ((record (workbench--goal-record workbench)))
          (and record (record-goal record))))))

(-> short-directory (string) string)
(defun short-directory (directory)
  "Return DIRECTORY with the home directory written as a tilde and no trailing slash."
  (let* ((home    (namestring (user-homedir-pathname)))
         (trimmed (string-right-trim "/" directory)))
    (if (and (> (length home) 1)
             (>= (length directory) (length home))
             (string= home directory :end2 (length home)))
        (format nil "~~/~A" (string-right-trim "/" (subseq directory (length home))))
        (if (zerop (length trimmed)) "/" trimmed))))


;;;; -- Opening and Leaving Channels --

(-> workbench--reset-channel (workbench) null)
(defun workbench--reset-channel (workbench)
  "Forget everything about the channel shown and start an empty transcript."
  (images-forget)
  (let ((transcript (make-transcript)))
    (setf (workbench-transcript workbench)    transcript
          (workbench-projector workbench)     (make-projector transcript)
          (workbench-document workbench)      (make-document
                                               transcript
                                               :load-earlier (lambda () (workbench-load-earlier workbench)))
          (workbench-status workbench)        nil
          (workbench-state workbench)         nil
          (workbench-working-p workbench)     nil
          (workbench-working-since workbench) nil
          (workbench-approval workbench)      nil
          (workbench-pending workbench)       nil
          (workbench--waiting workbench)          nil
          (workbench--waiting-pathname workbench) nil
          (workbench--waiting-read-at workbench)  0
          (workbench--digest workbench)           nil
          (workbench--goal-record workbench)      nil
          (workbench-console-scroll workbench)    0
          (workbench-jobs workbench)          nil
          (workbench-progress workbench)      nil
          (workbench-failure workbench)       nil
          (workbench-notice workbench)        nil))
  nil)

(-> workbench-open (workbench &key (:channel (or null string)) (:directory (or null string)) (:new-p boolean)
                                   (:images list))
    null)
(defun workbench-open (workbench &key channel directory new-p images)
  "Leave the channel shown and attach to CHANNEL, or to the latest or a NEW-P channel for DIRECTORY."
  (let ((old (workbench-session workbench)))
    (when old
      (session-close old)))
  (incf (workbench-generation workbench))
  (workbench--reset-channel workbench)
  (setf (workbench-attachment workbench) ':opening
        (workbench-overlay workbench)    nil)
  (unless (workbench-demo-p workbench)
    (setf (workbench-session workbench)
          (session-open (workbench-world workbench) (workbench-queue workbench)
                        :generation (workbench-generation workbench)
                        :channel    channel
                        :directory  (or directory
                                        (and (null channel)
                                             (world-working-directory (workbench-world workbench))))
                        :new-p      new-p
                        :images     images)))
  (workbench-redraw workbench)
  nil)

(-> workbench-run-aside (workbench t function) null)
(defun workbench-run-aside (workbench request function)
  "Call FUNCTION on a thread of its own and post what it returns, or why it failed, as the result of REQUEST.

Such a request is about the world, not the channel shown, so its result is
stamped as it arrives and reaches the window whichever channel it shows then."
  (bordeaux-threads:make-thread
   (lambda ()
     (multiple-value-bind (outcome value report)
         (handler-case (values ':ok (funcall function) nil)
           (error (condition)
             (values ':condition nil (condition-report condition))))
       (ui-queue-push (workbench-queue workbench)
                      (make-instance 'result-event
                                     :generation (workbench-generation workbench)
                                     :request    request
                                     :outcome    outcome
                                     :value      value
                                     :report     report))))
   :name "AUI aside")
  nil)

(-> workbench-leave (workbench) null)
(defun workbench-leave (workbench)
  "Leave the channel shown without attaching to another."
  (let ((old (workbench-session workbench)))
    (when old
      (session-close old)))
  (incf (workbench-generation workbench))
  (workbench--reset-channel workbench)
  (setf (workbench-session workbench)    nil
        (workbench-attachment workbench) ':none
        (workbench-overlay workbench)    nil)
  (workbench-redraw workbench)
  nil)

(-> workbench-delete-channel (workbench string) null)
(defun workbench-delete-channel (workbench channel)
  "Delete CHANNEL, the channels nested in it and their histories, leaving it first when it is the one shown."
  (when (equal channel (workbench-channel workbench))
    (workbench-leave workbench))
  (let ((world (workbench-world workbench)))
    (workbench-run-aside
     workbench (list ':delete-channel channel)
     (lambda ()
       ;; A channel just left is released by its host a moment later.
       (loop for attempt from 1
             do (handler-case (return (world-delete-channel world channel))
                  (client-error (condition)
                    (if (< attempt 6)
                        (sleep 1/2)
                        (error condition))))))))
  nil)

(-> workbench-new-channel (workbench &key (:directory (or null string))) null)
(defun workbench-new-channel (workbench &key directory)
  "Open a new channel in DIRECTORY, by default the directory of the channel shown."
  (workbench-open workbench :directory (or directory (workbench-directory workbench)) :new-p t))

(-> workbench-new-channel-elsewhere (workbench) null)
(defun workbench-new-channel-elsewhere (workbench)
  "Let the person choose a folder and open a new channel there."
  (let ((directory (lightfast:choose-directory :title       "A folder for the new channel"
                                               :preset-path (workbench-directory workbench))))
    (when (and directory (plusp (length directory)))
      (workbench-new-channel workbench
                             :directory (namestring (uiop:ensure-directory-pathname directory)))))
  nil)

(-> workbench-change-directory (workbench) null)
(defun workbench-change-directory (workbench)
  "Let the person choose the folder the channel shown works in, and tell the channel to work there."
  (when (workbench-attached-p workbench)
    (let ((directory (lightfast:choose-directory :title       "A folder for the agent to work in"
                                                 :preset-path (workbench-directory workbench))))
      (when (and directory (plusp (length directory)))
        (workbench-submit workbench
                          (format nil "(cwd ~S)" (namestring (uiop:ensure-directory-pathname directory)))))))
  nil)

(-> workbench-goal-request (workbench keyword) null)
(defun workbench-goal-request (workbench action)
  "Do ACTION with the goal of the channel shown: :PAUSE, :RESUME or :CLEAR it, or :UPDATE it to what the person then types."
  (when (workbench-attached-p workbench)
    (if (eq action ':update)
        (workbench-ask workbench "Change the goal"
                       (lambda (objective)
                         (let ((objective (string-trim '(#\Space #\Newline #\Tab) objective)))
                           (when (plusp (length objective))
                             (workbench-submit workbench (format nil "(goal :update ~S)" objective)))))
                       :initial (or (getf (workbench-goal workbench) :objective) ""))
        (workbench-submit workbench (format nil "(goal ~(~S~))" action))))
  nil)

(-> workbench-stop-server (workbench) null)
(defun workbench-stop-server (workbench)
  "Stop the world server, and with it the work in every channel. The channel shown closes with it; opening a channel starts a server again."
  (unless (workbench-demo-p workbench)
    (let ((world (workbench-world workbench)))
      (workbench-run-aside workbench ':stop-server
                           (lambda ()
                             (world-stop-server world)))))
  nil)

(-> workbench-load-earlier (workbench) null)
(defun workbench-load-earlier (workbench)
  "Fetch the page of history before the earliest entry shown."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (setf (document-more-p (workbench-document workbench)) nil)
      (session-load-earlier session)))
  nil)

(-> workbench-close (workbench) null)
(defun workbench-close (workbench)
  "Leave the channel shown and stop watching the world, before the window goes away."
  (let ((session (workbench-session workbench)))
    (when session
      (session-close session)
      (setf (workbench-session workbench) nil)))
  (unless (workbench-demo-p workbench)
    (world-stop-watching (workbench-world workbench)))
  nil)


;;;; -- Asking the Channel --

(-> workbench-refresh (workbench &rest string) null)
(defun workbench-refresh (workbench &rest methods)
  "Fetch the channel's snapshots named by METHODS again, such as _autolith/state."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (dolist (method methods)
        (session-operation session method))))
  nil)

(-> workbench--command-p (string) boolean)
(defun workbench--command-p (text)
  "Return T when TEXT is a command or a Lisp form rather than a message to the agent."
  (let ((trimmed (string-left-trim '(#\Space #\Newline #\Tab) text)))
    (and (plusp (length trimmed))
         (member (char trimmed 0) '(#\/ #\( #\?))
         t)))

(defparameter *interface-commands*
  '("settings" "model" "effort" "permissions" "open" "new" "delete" "agenda" "memories")
  "Commands that, given no argument, open a picker or a page in the terminal and a view of its own in the window.")

(defparameter *terminal-only-commands*
  '("onboard" "close" "exit" "detach" "rollback" "auth" "complain" "wtf")
  "Commands that need a terminal and have no counterpart in the window yet; they are not offered.")

(-> command-text-name (string) (values (or null string) boolean))
(defun command-text-name (text)
  "Return the name of the command TEXT invokes and whether it gives the command arguments.

TEXT is a slash command such as /model or a Lisp form such as (model). The
name is NIL when TEXT is neither."
  (let* ((trimmed (string-trim '(#\Space #\Newline #\Tab) text))
         (length  (length trimmed)))
    (if (and (> length 1) (member (char trimmed 0) '(#\/ #\()))
        (let* ((end  (or (position-if (lambda (character)
                                        (member character '(#\Space #\Newline #\Tab #\))))
                                      trimmed :start 1)
                         length))
               (rest (string-trim '(#\Space #\Newline #\Tab #\)) (subseq trimmed end))))
          (values (string-downcase (subseq trimmed 1 end))
                  (plusp (length rest))))
        (values nil nil))))

(-> workbench-run-interface-command (workbench string) boolean)
(defun workbench-run-interface-command (workbench name)
  "Show the window's own view for the command NAME and return whether it has one."
  (cond
    ((equal name "settings")
     (workbench-show-overlay workbench ':settings)
     t)
    ((equal name "model")
     (composer--choose-model workbench)
     t)
    ((equal name "effort")
     (composer--choose-effort workbench)
     t)
    ((equal name "permissions")
     (composer--choose-mode workbench)
     t)
    ((equal name "agenda")
     (panel-show-agenda workbench)
     t)
    ((equal name "memories")
     (panel-show-memories workbench)
     t)
    ((equal name "open")
     (window-focus-filter workbench)
     t)
    ((equal name "new")
     (workbench-new-channel workbench)
     t)
    ((equal name "delete")
     (when (workbench-channel workbench)
       (sidebar-confirm-delete workbench (workbench-channel workbench)))
     t)
    (t
     nil)))

(-> workbench-submit (workbench string &key (:steer-p boolean) (:images list)) boolean)
(defun workbench-submit (workbench text &key steer-p images)
  "Send TEXT with IMAGES to the channel and return whether it went.

A message sent while the agent works either steers the running turn, when
STEER-P, or waits as a follow-up; it is shown above the composer until the
agent takes it up."
  (let ((session (workbench-session workbench))
        (trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) text)))
    (cond
      ((and (zerop (length trimmed)) (null images))
       nil)
      ;; A command that would open a terminal picker opens the window's view instead.
      ((multiple-value-bind (name arguments-p) (command-text-name trimmed)
         (and name
              (not arguments-p)
              (member name *interface-commands* :test #'string=)
              (workbench-run-interface-command workbench name)))
       t)
      ((not (and session (workbench-attached-p workbench)))
       (setf (workbench-notice workbench) "Not attached to a channel yet.")
       (workbench-redraw workbench)
       nil)
      (t
       (let ((identifier (format nil "aui-~D-~D" (get-universal-time) (random 1000000))))
         (unless (workbench--command-p trimmed)
           (setf (workbench-pending workbench)
                 (append (workbench-pending workbench) (list (list identifier trimmed steer-p)))))
         ;; What a command prints is shown under the command.
         (setf (projector-invocation (workbench-projector workbench))
               (and (workbench--command-p trimmed) trimmed))
         (setf (workbench-notice workbench)        nil
               (workbench-history-index workbench) nil)
         (unless (equal trimmed (first (workbench-history workbench)))
           (push trimmed (workbench-history workbench)))
         (session-submit session trimmed
                         :images     images
                         :steer-p    steer-p
                         :identifier identifier
                         :request    (list ':submit identifier))
         (document-scroll-to-end (workbench-document workbench))
         (workbench-redraw workbench)
         t)))))

(-> workbench-stop (workbench) null)
(defun workbench-stop (workbench)
  "Stop the channel's running turn."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-cancel session)))
  nil)

(-> workbench-answer (workbench string) null)
(defun workbench-answer (workbench value)
  "Answer the waiting approval with the choice VALUE."
  (let ((session  (workbench-session workbench))
        (approval (workbench-approval workbench)))
    (when (and session approval)
      (session-answer session (getf approval :id) value)
      (setf (workbench-approval workbench) nil)
      (workbench-redraw workbench)))
  nil)

(-> workbench-set-mode (workbench keyword) null)
(defun workbench-set-mode (workbench mode)
  "Select the channel's permission MODE."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-set-mode session mode)))
  nil)

(-> workbench-set-setting (workbench string t) null)
(defun workbench-set-setting (workbench name value)
  "Set the channel's setting NAME to VALUE and fetch what depends on it again."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-operation session "_autolith/set-setting"
                         :params  (portable-object "name" name "value" value)
                         :request (list ':set-setting name value))))
  nil)


;;;; -- Jobs --

(defparameter *jobs-poll-seconds* 3
  "How often the jobs are fetched again while any of them is live.")

(-> workbench-live-jobs (workbench) list)
(defun workbench-live-jobs (workbench)
  "Return the jobs that are waiting or running, as portable rows."
  (remove-if-not (lambda (job)
                   (member (portable-get job "state") '("pending" "running" "blocked" "yielded")
                           :test #'equal))
                 (workbench-jobs workbench)))

(-> workbench-refresh-jobs (workbench) null)
(defun workbench-refresh-jobs (workbench)
  "Fetch the channel's jobs again."
  (setf (workbench-jobs-fetched-at workbench) (get-universal-time))
  (workbench-refresh workbench "_autolith/jobs")
  nil)

(-> workbench-poll-jobs (workbench) null)
(defun workbench-poll-jobs (workbench)
  "Fetch the jobs again when some are live, or the channel says some are, and the last answer is old."
  (when (and (workbench-attached-p workbench)
             (or (workbench-live-jobs workbench)
                 ;; Autolith called the count of live jobs :TASK-LIVE-COUNT before 0.67.39.
                 (let ((count (or (getf (workbench-status workbench) :job-live-count)
                                  (getf (workbench-status workbench) :task-live-count))))
                   (and (integerp count) (plusp count))))
             (>= (- (get-universal-time) (workbench-jobs-fetched-at workbench)) *jobs-poll-seconds*))
    (workbench-refresh-jobs workbench))
  nil)

(-> workbench-cancel-job (workbench string) null)
(defun workbench-cancel-job (workbench identifier)
  "Cancel the job IDENTIFIER and the jobs it started."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-operation session "_autolith/job-cancel"
                         :params  (portable-object "jobId" identifier)
                         :request (list ':cancel-job identifier))))
  nil)


;;;; -- Events --

(-> workbench--note-status (workbench list) null)
(defun workbench--note-status (workbench status)
  "Take the channel's STATUS property list as its current one and derive whether it works."
  ;; A folder that changed under the window makes the fetched state stale.
  (let ((known (getf (workbench-status workbench) :cwd)))
    (when (and known (not (equal known (getf status :cwd))))
      (workbench-refresh workbench "_autolith/state")))
  (setf (workbench-status workbench) status)
  (let ((working-p (and (member (getf status :state) '(:active :working :cancelling)) t)))
    (unless (eq working-p (workbench-working-p workbench))
      (setf (workbench-working-p workbench)     working-p
            (workbench-working-since workbench) (and working-p (get-universal-time)))))
  (let ((window (workbench-window workbench)))
    (when window
      (setf (lightfast:label window) (format nil "~A - Autolith" (workbench-title workbench)))))
  nil)

(defparameter *waiting-poll-seconds* 3/2
  "How often the channel's snapshot of waiting messages is read again.")

(-> workbench-waiting (workbench) list)
(defun workbench-waiting (workbench)
  "Return the messages the channel has not taken up yet, each as (TEXT STEER-P), oldest first.

They are what the channel's own snapshot lists, whoever sent them and from
where, followed by what this window sent that the snapshot does not show yet."
  (let ((known (workbench--waiting workbench)))
    (append known
            (loop for (nil text steer-p) in (workbench-pending workbench)
                  unless (find text known :key #'first :test #'equal)
                    collect (list text steer-p)))))

(-> workbench-poll-waiting (workbench &key (:now-p boolean)) null)
(defun workbench-poll-waiting (workbench &key now-p)
  "Read the channel's snapshot of waiting messages again when the last reading is old, or at once when NOW-P."
  (let ((channel (workbench-channel workbench))
        (now     (get-internal-real-time)))
    (when (and channel
               (workbench-attached-p workbench)
               (not (workbench-demo-p workbench))
               (or now-p
                   (>= (- now (workbench--waiting-read-at workbench))
                       (* *waiting-poll-seconds* internal-time-units-per-second))))
      (setf (workbench--waiting-read-at workbench) now)
      (unless (workbench--waiting-pathname workbench)
        (setf (workbench--waiting-pathname workbench)
              (or (history-pending-pathname (world-configuration (workbench-world workbench)) channel)
                  ':none)))
      (let ((pathname (workbench--waiting-pathname workbench)))
        (when (pathnamep pathname)
          (let ((waiting (history-pending-inputs pathname channel)))
            (unless (equal waiting (workbench--waiting workbench))
              (setf (workbench--waiting workbench) waiting)
              (workbench-redraw workbench)))))))
  nil)

(-> workbench--drop-pending (workbench t) null)
(defun workbench--drop-pending (workbench submission)
  "Forget the pending message that SUBMISSION, a property list with an :ID, names."
  (let ((identifier (and (listp submission) (getf submission :id))))
    (when identifier
      (setf (workbench-pending workbench)
            (remove identifier (workbench-pending workbench) :key #'first :test #'equal))))
  nil)

(-> workbench--channel-event (workbench list) null)
(defun workbench--channel-event (workbench payload)
  "Apply one event of the structured client protocol to WORKBENCH."
  (let ((fields (rest payload)))
    (projector-apply (workbench-projector workbench) payload)
    (case (first payload)
      (:state
       (workbench--note-status workbench (rest (getf fields :state))))
      (:approval
       (setf (workbench-approval workbench)
             (list :id      (getf fields :id)
                   :kind    (getf fields :kind)
                   :title   (getf fields :title)
                   :entry   (getf fields :entry)
                   :options (getf fields :options))))
      (:approval-settled
       (when (equal (getf fields :id) (getf (workbench-approval workbench) :id))
         (setf (workbench-approval workbench) nil)))
      (:work-started
       (workbench--drop-pending workbench (getf fields :submission))
       (unless (workbench-working-p workbench)
         (setf (workbench-working-p workbench)     t
               (workbench-working-since workbench) (get-universal-time))))
      (:work-finished
       (workbench--drop-pending workbench (getf fields :submission))
       (when (getf fields :idle-p)
         (setf (workbench-working-p workbench)     nil
               (workbench-working-since workbench) nil
               (workbench-approval workbench)      nil))
       (workbench-refresh workbench "_autolith/state")
       (workbench-refresh-jobs workbench))
      (:record
       (let ((record (getf fields :record)))
         (when (eq (first record) ':message)
           (workbench--drop-pending workbench (getf (rest record) :submission))
           ;; A job that reports back is no longer running.
           (when (getf (rest record) :job-completion)
             (workbench-refresh-jobs workbench)))
         (when (eq (first record) ':goal)
           (setf (workbench--goal-record workbench)
                 (records-newest-goal (list record) (workbench--goal-record workbench)))
           (workbench-refresh workbench "_autolith/state"))))
      (:status
       (when (member (getf fields :status) '(:provider-request-completed :compaction-completed))
         (workbench-refresh workbench "_autolith/state"))
       (when (eq (getf fields :status) ':tool-call-completed)
         (workbench-refresh-jobs workbench)))))
  nil)

(-> workbench--attachment-event (workbench attachment-event) null)
(defun workbench--attachment-event (workbench event)
  "Follow the attachment to the shown channel through EVENT."
  (let ((detail (attachment-event-detail event)))
    (ecase (attachment-event-state event)
      (:opening
       (setf (workbench-attachment workbench) ':opening
             (workbench-progress workbench)   detail))
      (:attached
       (let ((projector (workbench-projector workbench)))
         (setf (workbench-attachment workbench)   ':attached
               (workbench-progress workbench)     nil
               (projector-channel projector)      (ui-event-channel event)
               (projector-boundary projector)     (or (getf detail :record-sequence) 0)
               (projector-image-root projector)   (getf detail :image-root)
               (workbench--goal-record workbench) (records-newest-goal
                                                   (list (getf detail :goal-record))
                                                   (workbench--goal-record workbench))
               ;; A request under way at attachment is shown from its records.
               (projector-catch-up-p projector)   (and (getf (getf detail :status) :active-turn-p) t)))
       (workbench--note-status workbench (getf detail :status))
       (setf (world-current-channel (workbench-world workbench)) (ui-event-channel event))
       (workbench-refresh workbench "_autolith/state" "_autolith/operations" "_autolith/settings"
                          "_autolith/models")
       ;; What the command line asked to send goes out once, to the first channel attached.
       (let ((prompt (launch-prompt (workbench-launch workbench))))
         (when (and prompt (= (workbench-generation workbench) 1))
           (workbench-submit workbench prompt))))
      (:failed
       (setf (workbench-attachment workbench) ':failed
             (workbench-progress workbench)   nil
             (workbench-failure workbench)    (or detail "The channel could not be opened.")))
      (:closed
       (setf (workbench-attachment workbench) ':closed
             (workbench-working-p workbench)  nil
             (workbench-approval workbench)   nil
             (workbench-failure workbench)    (or detail "The channel closed.")))))
  nil)

(-> workbench--result-event (workbench result-event) null)
(defun workbench--result-event (workbench event)
  "Take the outcome of a request WORKBENCH made."
  (let ((request (result-event-request event))
        (value   (result-event-value event))
        (ok-p    (eq (result-event-outcome event) ':ok)))
    (cond
      ((equal request "_autolith/state")
       (when ok-p
         (setf (workbench-state workbench) value)))
      ((equal request "_autolith/operations")
       (when ok-p
         (setf (workbench-operations workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/settings")
       (when ok-p
         (setf (workbench-settings workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/models")
       (when ok-p
         (setf (workbench-models workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/jobs")
       (when ok-p
         (setf (workbench-jobs workbench) (portable-items (portable-get value "items")))))
      ((and (consp request) (eq (first request) ':cancel-job))
       (workbench-refresh-jobs workbench)
       (unless ok-p
         (setf (workbench-notice workbench)
               (format nil "Could not cancel the job: ~A"
                       (or (result-event-report event) "the channel refused.")))))
      ((and (consp request) (eq (first request) ':set-setting))
       (destructuring-bind (name &optional choice again-p) (rest request)
         (cond
           (ok-p
            (workbench-refresh workbench "_autolith/state" "_autolith/settings"))
           ;; A choice whose values are keywords is refused as a string by the
           ;; structured setter; the settings command reads it as it is spelled.
           ((and (stringp choice) (not again-p) (workbench-session workbench))
            (session-operation (workbench-session workbench) "_autolith/invoke"
                               :params  (portable-object
                                         "operation" "settings"
                                         "arguments" (portable-object "input" (format nil "~A ~A" name choice)))
                               :request (list ':set-setting name choice t)))
           (t
            (setf (workbench-notice workbench)
                  (format nil "Could not change ~A: ~A" name
                          (or (result-event-report event) "the channel refused.")))))))
      ((and (consp request) (eq (first request) ':delete-channel))
       (unless ok-p
         (setf (workbench-notice workbench)
               (or (result-event-report event) "The channel could not be deleted."))))
      ((eq request ':stop-server)
       (unless ok-p
         (setf (workbench-notice workbench)
               (or (result-event-report event) "The world server did not stop."))))
      ((and (consp request) (eq (first request) ':open-link))
       (unless ok-p
         (setf (workbench-notice workbench)
               (format nil "Could not open ~A with any program on this machine." (second request)))))
      ((and (consp request) (eq (first request) ':sign-in))
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" (second request))
        (panel--empty (if ok-p
                          (princ-to-string value)
                          (format nil "The sign-in did not go through: ~A"
                                  (or (result-event-report event) "no reason was given.")))))
       (workbench-refresh workbench "_autolith/models" "_autolith/state"))
      ((and (consp request) (eq (first request) ':panel))
       (panel-take-output workbench (second request)
                          (or (and ok-p (portable-string (portable-get value "text")))
                              (result-event-report event)
                              "The channel had nothing to report.")))
      ((and (consp request) (eq (first request) ':submit))
       (unless ok-p
         (setf (workbench-pending workbench)
               (remove (second request) (workbench-pending workbench) :key #'first :test #'equal)
               (workbench-notice workbench)
               (format nil "Not sent: ~A" (or (result-event-report event) "the channel refused.")))))
      ((not ok-p)
       (when (result-event-report event)
         (setf (workbench-notice workbench) (result-event-report event))))))
  nil)

(-> workbench-apply (workbench ui-event) null)
(defun workbench-apply (workbench event)
  "Apply EVENT, which a client thread posted, to WORKBENCH; an event of a channel already left is dropped."
  (when (= (ui-event-generation event) (workbench-generation workbench))
    (etypecase event
      (channel-event
       (workbench--channel-event workbench (channel-event-payload event)))
      (history-event
       (let ((projector (workbench-projector workbench)))
         (transcript-prepend (workbench-transcript workbench)
                             (records->entries projector (history-event-records event)))
         (setf (workbench--goal-record workbench)
               (records-newest-goal (history-event-records event) (workbench--goal-record workbench)))
         (setf (document-more-p (workbench-document workbench)) (history-event-more-p event))))
      (attachment-event
       (workbench--attachment-event workbench event))
      (result-event
       (workbench--result-event workbench event)))
    (workbench-redraw workbench))
  nil)

(-> workbench-drain (workbench) null)
(defun workbench-drain (workbench)
  "Apply every event waiting in WORKBENCH's queue, and take what changed in the world's channel tree."
  ;; Each event is guarded by itself: one that fails must not take the rest with it.
  (dolist (event (ui-queue-drain (workbench-queue workbench)))
    (with-stage-guard ("Applying what the channel said")
      (workbench-apply workbench event)))
  (when (and (not (workbench-demo-p workbench))
             (world-refresh (workbench-world workbench)))
    (workbench-redraw workbench))
  nil)
