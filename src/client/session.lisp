(in-package #:aui)

;;;; -- Channel Session --

;;; One attachment of this client to one channel, as a structured client of
;;; the world server: the same CHANNEL-SUBSCRIPTION Autolith's ACP sessions
;;; and piped scripts use. The session opens the channel, reads the newest
;;; page of its history, and from then on forwards the channel's events to
;;; the interface queue. Every call here returns at once; work that waits on
;;; the server runs on threads of the session's own, and its outcome arrives
;;; as an event.

(defclass session-subscription (autolith::channel-subscription)
  ((session
    :initarg       :session
    :reader        session-subscription-session
    :documentation "The session this subscription delivers to."))
  (:default-initargs :name "aui")
  (:documentation "AUI's structured client attachment to a channel."))

(defclass session ()
  ((world
    :initarg       :world
    :reader        session-world
    :type          world
    :documentation "The world the channel belongs to.")
   (queue
    :initarg       :queue
    :reader        session-queue
    :type          ui-queue
    :documentation "Where the session posts its events.")
   (generation
    :initarg       :generation
    :reader        session-generation
    :type          integer
    :documentation "Stamped on every event, so the interface drops those of a session it already left.")
   (identifier
    :initform      nil
    :accessor      session-identifier
    :type          (or null string)
    :documentation "The channel's identifier, once the server named it.")
   (subscription
    :initform      nil
    :accessor      session--subscription
    :documentation "The channel subscription, once attached.")
   (lock
    :initform      (bordeaux-threads:make-lock "AUI session")
    :reader        session--lock
    :documentation "Guards the request queue and the closed flag.")
   (condition
    :initform      (bordeaux-threads:make-condition-variable :name "AUI session")
    :reader        session--condition
    :documentation "Wakes the ordered worker.")
   (requests
    :initform      nil
    :accessor      session--requests
    :type          list
    :documentation "Ordered work waiting for the worker, oldest first.")
   (worker
    :initform      nil
    :accessor      session--worker
    :documentation "The thread that opens the channel and then sends ordered requests.")
   (closed-p
    :initform      nil
    :accessor      session-closed-p
    :type          boolean
    :documentation "Whether the session was closed or lost its channel.")
   (history-floor
    :initform      nil
    :accessor      session--history-floor
    :type          (or null integer)
    :documentation "The lowest record sequence already delivered as history, or NIL before the first page."))
  (:documentation "This client's attachment to one channel."))

(-> session--post (session symbol &rest t) ui-event)
(defun session--post (session class &rest initargs)
  "Post an event of CLASS for SESSION's channel to the interface."
  (ui-queue-push (session-queue session)
                 (apply #'make-instance class
                        :channel    (session-identifier session)
                        :generation (session-generation session)
                        initargs)))

(defmethod autolith::channel-subscription-present-event ((subscription session-subscription) event)
  "Forward the channel's EVENT to the interface queue in the order it arrived."
  (session--post (session-subscription-session subscription) 'channel-event :payload event))

(defmethod autolith::channel-subscription-forget ((subscription session-subscription) &key wire-p)
  "Tell the interface when the channel went away on its own rather than because the session left it."
  (declare (ignore wire-p))
  (let ((session (session-subscription-session subscription)))
    (unless (session-closed-p session)
      (setf (session-closed-p session) t)
      (session--post session 'attachment-event
                     :state  ':closed
                     :detail (autolith::channel-subscription-reconnect-failure subscription))))
  nil)


;;;; -- Opening --

(-> session--attach (session string boolean) list)
(defun session--attach (session identifier started-p)
  "Attach SESSION to the opened channel IDENTIFIER and return the server's greeting."
  (let ((subscription (make-instance 'session-subscription
                                     :session       session
                                     :identifier    identifier
                                     :configuration (world-configuration (session-world session))
                                     :started-p     started-p
                                     :approves-p    t)))
    (setf (session-identifier session)    identifier
          (session--subscription session) subscription)
    (autolith::channel-subscription-attach subscription)
    (autolith::localgroup-client-connection-attached
     (autolith::channel-subscription-connection subscription))))

(-> session--deliver-history (session &key (:through (or null integer))) null)
(defun session--deliver-history (session &key through)
  "Read the page of history below what SESSION already delivered and post it."
  (multiple-value-bind (records more-p)
      (history-read-page (world-configuration (session-world session))
                         (session-identifier session)
                         :before  (session--history-floor session)
                         :through through)
    (let ((first (find-if #'record-sequence records)))
      (when first
        (setf (session--history-floor session) (record-sequence first))))
    (session--post session 'history-event :records records :more-p more-p))
  nil)

(-> session--open (session &key (:channel (or null string)) (:directory (or null string)) (:new-p boolean)
                               (:images list))
    null)
(defun session--open (session &key channel directory new-p images)
  "Open the channel in the world server, attach to it and deliver its newest history, reporting each step."
  (let ((world (session-world session)))
    (handler-case
        (multiple-value-bind (identifier started-p)
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
                                           :launch          (shiftf (world-launch world) nil)
                                           :images          images
                                           ;; A window shows its own progress, so the
                                           ;; channel skips the terminal's boot screen
                                           ;; and first-run tour, which wait for a key.
                                           :switch-p        t))
          (let ((greeting (session--attach session identifier started-p)))
            (session--post session 'attachment-event
                           :state  ':attached
                           :detail (list :status          (rest (getf (rest greeting) :state))
                                         :record-sequence (getf (rest greeting) :record-sequence)
                                         :image-root      (history-image-root (world-configuration world)
                                                                              identifier)))
            (session--deliver-history session :through (getf (rest greeting) :record-sequence))))
      (error (condition)
        (setf (session-closed-p session) t)
        (session--post session 'attachment-event
                       :state  ':failed
                       :detail (condition-report condition)))))
  nil)

(-> session--work (session) null)
(defun session--work (session)
  "Run SESSION's ordered requests until it closes."
  (loop
    (let ((request (bordeaux-threads:with-lock-held ((session--lock session))
                     (loop while (and (null (session--requests session))
                                      (not (session-closed-p session)))
                           do (bordeaux-threads:condition-wait (session--condition session)
                                                               (session--lock session)))
                     (pop (session--requests session)))))
      (if request
          (funcall request)
          (return))))
  nil)

(-> session--enqueue (session function) null)
(defun session--enqueue (session function)
  "Add FUNCTION to SESSION's ordered work."
  (bordeaux-threads:with-lock-held ((session--lock session))
    (setf (session--requests session) (append (session--requests session) (list function)))
    (bordeaux-threads:condition-notify (session--condition session)))
  nil)

(-> session-open
    (world ui-queue &key (:generation integer) (:channel (or null string)) (:directory (or null string))
                         (:new-p boolean) (:images list))
    session)
(defun session-open (world queue &key (generation 0) channel directory new-p images)
  "Start attaching to CHANNEL, or to the latest or a NEW-P channel for DIRECTORY, and return the session at once.

ATTACHMENT-EVENTs report the opening, then the attachment with the channel's
status, or the failure; a HISTORY-EVENT follows a successful attachment."
  (let ((session (make-instance 'session :world world :queue queue :generation generation)))
    (setf (session-identifier session) channel)
    (session--enqueue session
                      (lambda ()
                        (session--open session
                                       :channel   channel
                                       :directory directory
                                       :new-p     new-p
                                       :images    images)))
    (setf (session--worker session)
          (bordeaux-threads:make-thread (lambda () (session--work session)) :name "AUI session"))
    session))

(-> session-close (session) null)
(defun session-close (session)
  "Leave the channel: stop delivering events and release the connection. The channel keeps running."
  (let ((subscription (session--subscription session)))
    (bordeaux-threads:with-lock-held ((session--lock session))
      (setf (session-closed-p session) t)
      (bordeaux-threads:condition-notify (session--condition session)))
    (when subscription
      ;; Closing joins the reader thread, which may be delivering; do it off
      ;; the interface thread so a slow server cannot freeze the window.
      (bordeaux-threads:make-thread
       (lambda ()
         (handler-case (autolith::channel-subscription-close subscription)
           (error ()
             nil)))
       :name "AUI session close")))
  nil)


;;;; -- Requests --

(-> session--call (session keyword list) list)
(defun session--call (session command arguments)
  "Send COMMAND with ARGUMENTS on SESSION's connection and return the reply's fields."
  (let ((subscription (session--subscription session)))
    (unless (and subscription (not (session-closed-p session)))
      (error 'client-error :message "The channel is not attached."))
    (apply #'autolith::channel-subscription-call subscription command arguments)))

(-> session--report (session t function) null)
(defun session--report (session request function)
  "Call FUNCTION and post its outcome for REQUEST: its portable value, or the failure's report."
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
  "Run FUNCTION for REQUEST on a thread of its own, so a request that waits never holds up another."
  (bordeaux-threads:make-thread (lambda () (session--report session request function))
                                :name "AUI request")
  nil)

(-> session-submit (session string &key (:images list) (:steer-p boolean) (:request t)) null)
(defun session-submit (session text &key images steer-p (request ':submit))
  "Submit TEXT with IMAGES to the channel, as a steer of the running turn when STEER-P.

Prose becomes a message to the agent; a Lisp form or a slash command runs as
the terminal would run it. Submissions keep the order they were made in."
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
                            :submission (list :id     (autolith::make-identifier)
                                              :client (autolith::channel-subscription-client-identifier
                                                       (session--subscription session))
                                              :source (if steer-p "steering" "prompt"))))))
          (values ':ok (list :busy-p (getf reply :busy-p))))))))
  nil)

(-> session-cancel (session) null)
(defun session-cancel (session)
  "Cancel the channel's running turn."
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
  "Select the channel's permission MODE: :ASK, :AUTO, :SANDBOXED or :FULL-ACCESS."
  (session--spawn session ':mode
                  (lambda ()
                    (values ':ok (session--call session ':mode (list :value mode)))))
  nil)

(-> session-operation (session string &key (:params list) (:request t)) null)
(defun session-operation (session method &key (params (portable-object)) (request method))
  "Run the client operation METHOD, such as _autolith/settings, with the portable object PARAMS.

The RESULT-EVENT carries REQUEST and the operation's typed outcome."
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
  "Deliver the page of history before the earliest one delivered so far."
  (session--enqueue session
                    (lambda ()
                      (handler-case (session--deliver-history session)
                        (error ()
                          nil))))
  nil)
