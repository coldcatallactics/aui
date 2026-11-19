(in-package #:aui)

;;;; -- Channel Session --

;;; One attachment of this client to one channel, as a structured client of
;;; the world server. It uses the same CHANNEL-SUBSCRIPTION as the ACP
;;; sessions and the piped scripts of Autolith. When a session starts, the
;;; channel is opened, and the newest page of history is read. After that, the
;;; events of the channel are forwarded to the interface queue. All calls here
;;; return immediately. Work that waits on the server runs on threads of the
;;; session, and the outcome is posted as an event.

(defclass session-subscription (autolith::channel-subscription)
  ((session
    :initarg       :session
    :reader        session-subscription-session
    :documentation "The session of this subscription."))
  (:default-initargs :name "aui")
  (:documentation "The structured client attachment of AUI to a channel."))

(defclass session ()
  ((world
    :initarg       :world
    :reader        session-world
    :type          world
    :documentation "The world that contains the channel.")
   (queue
    :initarg       :queue
    :reader        session-queue
    :type          ui-queue
    :documentation "The queue to which the events of the session are posted.")
   (generation
    :initarg       :generation
    :reader        session-generation
    :type          integer
    :documentation "The generation number stamped on all events of the session.
Events of sessions that were already left are dropped.")
   (identifier
    :initarg       :identifier
    :initform      nil
    :accessor      session--identifier
    :type          (or null string)
    :documentation "The identifier of the channel, or NIL before the server sent it.
It is read and written under the lock.")
   (subscription
    :initform      nil
    :accessor      session--subscription
    :documentation "The channel subscription, or NIL before the attachment.")
   (lock
    :initform      (bordeaux-threads:make-lock "AUI session")
    :reader        session--lock
    :documentation "The lock that protects the request queue, the identifier, the subscription and the closed flag.")
   (condition
    :initform      (bordeaux-threads:make-condition-variable :name "AUI session")
    :reader        session--condition
    :documentation "The condition variable used to wake the ordered worker.")
   (requests
    :initform      nil
    :accessor      session--requests
    :type          list
    :documentation "A list of the requests that wait for the worker, oldest first.")
   (worker
    :initform      nil
    :accessor      session--worker
    :documentation "The thread that opens the channel and then runs the ordered requests.")
   (closed-p
    :initform      nil
    :accessor      session--closed-p
    :type          boolean
    :documentation "True if the session was closed or if the channel was lost.
It is read and written under the lock.")
   (history-floor
    :initform      nil
    :accessor      session--history-floor
    :type          (or null integer)
    :documentation "The lowest sequence number of the records delivered as history, or NIL before the first page.")
   (history-cursor
    :initform      nil
    :accessor      session--history-cursor
    :documentation "The end of the page of history before the earliest delivered page, or NIL before the first page."))
  (:documentation "The attachment of this client to one channel."))

(-> session-identifier (session) (or null string))
(defun session-identifier (session)
  "Return the identifier of the channel of SESSION, or NIL before the server sent it."
  (bordeaux-threads:with-lock-held ((session--lock session))
    (session--identifier session)))

(-> session-closed-p (session) boolean)
(defun session-closed-p (session)
  "Return T if SESSION was closed or if the channel was lost."
  (bordeaux-threads:with-lock-held ((session--lock session))
    (session--closed-p session)))

(-> session--mark-closed (session) boolean)
(defun session--mark-closed (session)
  "Close SESSION to further work and wake the worker. Return T if SESSION was open before this call."
  (bordeaux-threads:with-lock-held ((session--lock session))
    (unless (session--closed-p session)
      (setf (session--closed-p session) t)
      (bordeaux-threads:condition-notify (session--condition session))
      t)))

(-> session--post (session symbol &rest t) ui-event)
(defun session--post (session class &rest initargs)
  "Post an event of CLASS for the channel of SESSION to the interface queue."
  (ui-queue-push (session-queue session)
                 (apply #'make-instance class
                        :channel    (session-identifier session)
                        :generation (session-generation session)
                        initargs)))

(defmethod autolith::channel-subscription-present-event ((subscription session-subscription) event)
  "Forward EVENT of the channel to the interface queue in the order of arrival."
  (session--post (session-subscription-session subscription) 'channel-event :payload event))

(defmethod autolith::channel-subscription-forget ((subscription session-subscription) &key wire-p)
  "Post a closed attachment event if the channel was lost while the session was open."
  (declare (ignore wire-p))
  (let ((session (session-subscription-session subscription)))
    (when (session--mark-closed session)
      (session--post session 'attachment-event
                     :state  ':closed
                     :detail (autolith::channel-subscription-reconnect-failure subscription))))
  nil)

(defmethod autolith::channel-subscription-prepare-attachment
    ((subscription session-subscription) greeting &key replay-p)
  "Post the attachment for GREETING and the newest page of history."
  (declare (ignore replay-p))
  (session--deliver-attachment (session-subscription-session subscription) greeting)
  nil)


;;;; -- Opening --

(-> session--deliver-history (session &key (:through (or null integer))) null)
(defun session--deliver-history (session &key through)
  "Read the page of history before the history already delivered by SESSION, and post the page."
  (multiple-value-bind (records more-p cursor)
      (history-read-page (world-configuration (session-world session))
                         (session-identifier session)
                         :cursor  (session--history-cursor session)
                         :before  (session--history-floor session)
                         :through through)
    (let ((first (find-if #'record-sequence records)))
      (when first
        (setf (session--history-floor session) (record-sequence first))))
    (setf (session--history-cursor session) cursor)
    (session--post session 'history-event :records records :more-p more-p))
  nil)

(-> session--deliver-attachment (session list) null)
(defun session--deliver-attachment (session greeting)
  "Post an attachment event for SESSION with the state of the channel from the GREETING of the server.
Then post the newest page of history."
  (let ((configuration (world-configuration (session-world session)))
        (identifier    (session-identifier session))
        (boundary      (getf (rest greeting) :record-sequence)))
    (session--post session 'attachment-event
                   :state  ':attached
                   :detail (list :status          (rest (getf (rest greeting) :state))
                                 :record-sequence boundary
                                 :image-root      (history-image-root configuration identifier)
                                 :goal-record     (history-goal-record configuration identifier)))
    (session--deliver-history session :through boundary))
  nil)

(-> session--release (session-subscription) null)
(defun session--release (subscription)
  "Close the connection of SUBSCRIPTION. Errors are ignored."
  (handler-case (autolith::channel-subscription-close subscription)
    (error ()
      nil))
  nil)

(-> session--attach (session string) null)
(defun session--attach (session identifier)
  "Attach SESSION to the opened channel IDENTIFIER.

The subscription does not claim to have started the channel. For this
reason, the work in the channel goes on after the session leaves it, as with
a terminal that detaches. If SESSION is closed during the attachment, the
connection is released here."
  (let ((subscription (make-instance 'session-subscription
                                     :session       session
                                     :identifier    identifier
                                     :configuration (world-configuration (session-world session))
                                     :approves-p    t
                                     :evaluates-p   t))
        (attached-p   nil))
    (bordeaux-threads:with-lock-held ((session--lock session))
      (setf (session--identifier session)   identifier
            (session--subscription session) subscription))
    (unwind-protect
         (unless (session-closed-p session)
           (autolith::channel-subscription-attach subscription)
           (setf attached-p (not (session-closed-p session))))
      (unless attached-p
        (session--release subscription))))
  nil)

(-> session--open-served (session &key (:channel (or null string)) (:directory (or null string))
                                      (:new-p boolean) (:images list) (:launch list))
    null)
(defun session--open-served (session &key channel directory new-p images launch)
  "Open the channel in the running world server and attach SESSION to it.
Post an event for each step of the opening."
  (let ((world (session-world session)))
    (session--attach
     session
     (handler-bind ((autolith::channel-loading-progress
                      (lambda (progress)
                        (session--post session 'attachment-event
                                       :state  ':opening
                                       :detail (list (autolith::channel-loading-progress-step progress)
                                                     (autolith::channel-loading-progress-description
                                                      progress))))))
       (autolith::world-client-open (world-configuration world)
                                    :directory       directory
                                    :channel         channel
                                    :new-p           new-p
                                    :permission-mode (world-permission-mode world)
                                    :immutable-p     (world-immutable-p world)
                                    :launch          launch
                                    :images          images
                                    ;; Progress is shown in the window. This
                                    ;; skips the boot screen and the first-run
                                    ;; tour of the terminal, because they wait
                                    ;; for a key.
                                    :switch-p        t))))
  nil)

(-> session--open (session &key (:channel (or null string)) (:directory (or null string)) (:new-p boolean)
                               (:images list) (:launch list))
    null)
(defun session--open (session &key channel directory new-p images launch)
  "Start the world server if no server runs. Open the channel, attach SESSION to it and
deliver the newest history. Post an event for each step and for a failure."
  (let ((world (session-world session)))
    (handler-case
        (progn
          ;; The autolith command prepares the image and the helpers of the server.
          (unless (world-served-p world)
            (session--post session 'attachment-event
                           :state  ':opening
                           :detail (list 0 "starting the world server"))
            (world-start-server world))
          (session--open-served session
                                :channel   channel
                                :directory directory
                                :new-p     new-p
                                :images    images
                                :launch    launch))
      (error (condition)
        (session--mark-closed session)
        (session--post session 'attachment-event
                       :state  ':failed
                       :detail (condition-report condition)))))
  nil)

(-> session--work (session) null)
(defun session--work (session)
  "Run the ordered requests of SESSION until SESSION is closed."
  (loop
    (let ((request (bordeaux-threads:with-lock-held ((session--lock session))
                     (loop while (and (null (session--requests session))
                                      (not (session--closed-p session)))
                           do (bordeaux-threads:condition-wait (session--condition session)
                                                               (session--lock session)))
                     (pop (session--requests session)))))
      (if request
          (funcall request)
          (return))))
  nil)

(-> session--enqueue (session function) null)
(defun session--enqueue (session function)
  "Add FUNCTION to the ordered requests of SESSION."
  (bordeaux-threads:with-lock-held ((session--lock session))
    (setf (session--requests session) (append (session--requests session) (list function)))
    (bordeaux-threads:condition-notify (session--condition session)))
  nil)

(-> session-open
    (world ui-queue &key (:generation integer) (:channel (or null string)) (:directory (or null string))
                         (:new-p boolean) (:images list))
    session)
(defun session-open (world queue &key (generation 0) channel directory new-p images)
  "Start to attach to CHANNEL and return the session immediately.
If CHANNEL is NIL, attach to the latest channel for DIRECTORY, or to a new
channel if NEW-P is true.

ATTACHMENT-EVENTs are posted for the opening, and then for the attachment or
for the failure. The attachment event contains the status of the channel. A
HISTORY-EVENT is posted after a successful attachment."
  (let ((session (make-instance 'session :world world :queue queue :generation generation :identifier channel))
        ;; The launch options from the command line only apply to the first channel
        ;; that is opened.
        (launch  (shiftf (world-launch world) nil)))
    (session--enqueue session
                      (lambda ()
                        (session--open session
                                       :channel   channel
                                       :directory directory
                                       :new-p     new-p
                                       :images    images
                                       :launch    launch)))
    (setf (session--worker session)
          (bordeaux-threads:make-thread (lambda () (session--work session)) :name "AUI session"))
    session))

(-> session-close (session) null)
(defun session-close (session)
  "Leave the channel without stopping it. Stop the delivery of events and release the connection."
  (session--mark-closed session)
  (let ((subscription (bordeaux-threads:with-lock-held ((session--lock session))
                        (session--subscription session))))
    (when subscription
      ;; The close waits for the reader thread. This can take long if the
      ;; server is slow, so the close runs on a separate thread.
      (bordeaux-threads:make-thread (lambda () (session--release subscription))
                                    :name "AUI session close")))
  nil)


;;;; -- Requests --

(-> session--call (session keyword list) list)
(defun session--call (session command arguments)
  "Send COMMAND with ARGUMENTS on the connection of SESSION. Return the fields of the reply.
Signal a CLIENT-ERROR if the channel is not attached."
  (let ((subscription (bordeaux-threads:with-lock-held ((session--lock session))
                        (and (not (session--closed-p session))
                             (session--subscription session)))))
    (unless subscription
      (error 'client-error :message "The channel is not attached."))
    (apply #'autolith::channel-subscription-call subscription command arguments)))

(-> session--report (session t function) null)
(defun session--report (session request function)
  "Call FUNCTION and post a result event for REQUEST.
The event contains the values of FUNCTION or the report of the failure."
  (handler-case
      (multiple-value-bind (outcome value report) (funcall function)
        (session--post session 'result-event
                       :request request :outcome outcome :value value :report report))
    (error (condition)
      (session--post session 'result-event
                     :request request
                     :outcome ':condition
                     :report  (condition-report condition))))
  nil)

(-> session--spawn (session t function) null)
(defun session--spawn (session request function)
  "Run FUNCTION for REQUEST on a new thread and post the outcome.
Requests that wait do not delay other requests."
  (bordeaux-threads:make-thread (lambda () (session--report session request function))
                                :name "AUI request")
  nil)

(-> session-submit
    (session string &key (:images list) (:steer-p boolean) (:identifier (or null string)) (:request t)) null)
(defun session-submit (session text &key images steer-p identifier (request ':submit))
  "Submit TEXT and IMAGES to the channel of SESSION.
If STEER-P is true, the submission steers the running turn.

Prose becomes a message to the agent. Lisp forms and slash commands are run
as in the terminal. Submissions are sent in the order in which they were made.
IDENTIFIER is the identifier of the submission in the events of the channel."
  (session--enqueue
   session
   (lambda ()
     (session--report
      session request
      (lambda ()
        (let ((reply (session--call
                      session ':submit
                      (list :text       text
                            :images     images
                            :steer-p    steer-p
                            :submission (list :id     (or identifier (autolith::make-identifier))
                                              :client (autolith::channel-subscription-client-identifier
                                                       (session--subscription session))
                                              :source (if steer-p ':steering ':prompt))))))
          (values ':ok (list :busy-p (getf reply :busy-p))))))))
  nil)

(-> session-cancel (session) null)
(defun session-cancel (session)
  "Cancel the running turn of the channel."
  (session--spawn session ':cancel
                  (lambda ()
                    (values ':ok (session--call session ':cancel nil))))
  nil)

(-> session-answer (session string string) null)
(defun session-answer (session approval value)
  "Answer the pending APPROVAL with the offered choice VALUE."
  (session--spawn session ':answer
                  (lambda ()
                    (values ':ok (session--call session ':answer (list :approval approval :value value)))))
  nil)

(-> session-set-mode (session keyword) null)
(defun session-set-mode (session mode)
  "Set the permission mode of the channel to MODE.
MODE is :ASK, :AUTO, :SANDBOXED or :FULL-ACCESS."
  (session--spawn session ':mode
                  (lambda ()
                    (values ':ok (session--call session ':mode (list :value mode)))))
  nil)

(-> session-operation (session string &key (:params list) (:request t)) null)
(defun session-operation (session method &key (params (portable-object)) (request method))
  "Run the client operation METHOD, such as _autolith/settings, with the portable object PARAMS.

Post a RESULT-EVENT that contains REQUEST and the typed outcome of the operation."
  (session--spawn
   session request
   (lambda ()
     (let* ((reply   (session--call session ':operation (list :method method :params params)))
            (result  (getf reply :result))
            (outcome (getf result :outcome)))
       (values (if (member outcome '(:ok :condition :busy :unsupported :cancelled)) outcome ':condition)
               (getf result :value)
               (getf result :report)))))
  nil)

(-> session-load-earlier (session) null)
(defun session-load-earlier (session)
  "Deliver the page of history before the earliest delivered page.

If the page cannot be read, deliver an empty page and mark more pages as
available. The person can then ask for the page again."
  (session--enqueue session
                    (lambda ()
                      (handler-case (session--deliver-history session)
                        (error ()
                          (session--post session 'history-event :records nil :more-p t)))))
  nil)
