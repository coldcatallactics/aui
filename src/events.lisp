(in-package #:aui)

;;;; -- UI Events --

;;; Immutable messages from the backend to the view, and the queue that
;;; carries them across the thread boundary. Backend threads push; the FLTK
;;; thread drains and applies. Nothing here touches a widget.

(deftype connection-state ()
  "The lifecycle of the owned Autolith process and its connection."
  '(member :starting :ready :lost :closing :exited))

(deftype session-state ()
  "The lifecycle of the open ACP session."
  '(member :loading :ready :closed))

(deftype turn-state ()
  "The lifecycle of a prompt turn."
  '(member :idle :running :cancelling :interrupted))

(deftype result-outcome ()
  "How a request to the backend ended."
  '(member :ok :condition :busy :unsupported :cancelled))

(deftype transcript-operation ()
  "A transcript mutation named after the function that performs it."
  '(member :append :append-text :update-tool-call :replace-plan :decide-approval
           :notice :reset :begin-replay :end-replay))


;;; Events

(defclass ui-event ()
  ((backend-id       :initarg :backend-id :initform nil :reader ui-event-backend-id
                     :documentation "Which owned process sent this.")
   (generation       :initarg :generation :initform 0 :reader ui-event-generation :type integer
                     :documentation "The connection generation of that process; a reconnect increments it.")
   (session-id       :initarg :session-id :initform nil :reader ui-event-session-id
                     :documentation "The ACP session, or NIL before one exists.")
   (ingress-sequence :initform 0 :accessor ui-event-ingress-sequence :type integer
                     :documentation "Assigned by the queue on push; local ordering only.")
   (received-at      :initarg :received-at :initform (get-universal-time) :reader ui-event-received-at
                     :type integer
                     :documentation "Universal time at which the backend created the event."))
  (:documentation "The base of every backend to view message."))

(defclass connection-event (ui-event)
  ((state  :initarg :state :reader connection-event-state :type connection-state
           :documentation "The new connection state.")
   (detail :initarg :detail :initform nil :reader connection-event-detail
           :documentation "A short human readable reason, such as an exit status."))
  (:documentation "The owned process or its connection changed state."))

(defclass session-event (ui-event)
  ((state           :initarg :state :reader session-event-state :type session-state
                    :documentation "The new session state.")
   (conversation-id :initarg :conversation-id :initform nil :reader session-event-conversation-id
                    :documentation "The durable Autolith conversation id.")
   (title           :initarg :title :initform nil :reader session-event-title
                    :documentation "The conversation title when known.")
   (directory       :initarg :directory :initform nil :reader session-event-directory
                    :documentation "The workspace directory."))
  (:documentation "A session was opened, finished loading, or closed.
Loading brackets replayed history: between :LOADING and :READY the view
applies transcript events silently."))

(defclass turn-event (ui-event)
  ((state       :initarg :state :reader turn-event-state :type turn-state
                :documentation "The new turn state.")
   (stop-reason :initarg :stop-reason :initform nil :reader turn-event-stop-reason
                :documentation "The ACP stop reason when a turn ended."))
  (:documentation "The prompt turn changed state."))

(defclass transcript-event (ui-event)
  ((operation :initarg :operation :reader transcript-event-operation :type transcript-operation
              :documentation "Which transcript mutation to perform.")
   (arguments :initarg :arguments :initform nil :reader transcript-event-arguments
              :documentation "The mutation's arguments as an immutable plist."))
  (:documentation "One transcript mutation, applied by TRANSCRIPT-EVENT-APPLY."))

(defclass approval-event (ui-event)
  ((request-id :initarg :request-id :reader approval-event-request-id :type string
               :documentation "The identity the decision is resolved against.")
   (call-id    :initarg :call-id :initform nil :reader approval-event-call-id)
   (title      :initarg :title :initform "" :reader approval-event-title :type string)
   (arguments  :initarg :arguments :initform nil :reader approval-event-arguments)
   (directory  :initarg :directory :initform nil :reader approval-event-directory)
   (options    :initarg :options :initform nil :reader approval-event-options
               :documentation "The offered (OPTION-ID NAME KIND) choices.")
   (deadline   :initarg :deadline :initform nil :reader approval-event-deadline))
  (:documentation "A permission request is waiting for the user."))

(defclass state-event (ui-event)
  ((snapshot :initarg :snapshot :reader state-event-snapshot
             :documentation "The latest session state as immutable data."))
  (:documentation "A fresh session state snapshot; a later one supersedes it."))

(defclass jobs-event (ui-event)
  ((kind        :initarg :kind :reader jobs-event-kind :type (member :snapshot :delta)
                :documentation "Whether JOBS replaces the inventory or changes part of it.")
   (jobs        :initarg :jobs :initform nil :reader jobs-event-jobs
                :documentation "Job entries added or changed.")
   (removed-ids :initarg :removed-ids :initform nil :reader jobs-event-removed-ids
                :documentation "Job ids that left the inventory, for a delta."))
  (:documentation "The job inventory changed."))

(defclass catalog-event (ui-event)
  ((kind  :initarg :kind :reader catalog-event-kind
          :type (member :models :settings :operations :conversations)
          :documentation "Which catalog ITEMS replaces.")
   (items :initarg :items :initform nil :reader catalog-event-items))
  (:documentation "A catalog was fetched."))

(defclass result-event (ui-event)
  ((request-id     :initarg :request-id :reader result-event-request-id
                   :documentation "The request this answers.")
   (outcome        :initarg :outcome :reader result-event-outcome :type result-outcome)
   (value          :initarg :value :initform nil :reader result-event-value
                   :documentation "The result as immutable data when the outcome is :OK.")
   (condition-type :initarg :condition-type :initform nil :reader result-event-condition-type
                   :documentation "The condition's type name as a string, never a live condition.")
   (report         :initarg :report :initform nil :reader result-event-report
                   :documentation "The condition's report text.")
   (data           :initarg :data :initform nil :reader result-event-data
                   :documentation "Sanitized structured detail."))
  (:documentation "A request to the backend finished."))

(-> ui-event-current-p (ui-event t integer) boolean)
(defun ui-event-current-p (event backend-id generation)
  "Return T when EVENT comes from BACKEND-ID at GENERATION, the live connection."
  (and (equal (ui-event-backend-id event) backend-id)
       (= (ui-event-generation event) generation)))


;;; Queue

(defclass ui-queue ()
  ((lock          :initform (bordeaux-threads:make-lock "AUI events") :reader ui-queue--lock)
   (items         :initform nil :accessor ui-queue--items
                  :documentation "Pending events, newest first; reversed on drain.")
   (count         :initform 0 :accessor ui-queue--count :type integer)
   (next-sequence :initform 1 :accessor ui-queue--next-sequence :type integer)
   (wakeup        :initarg :wakeup :initform (constantly nil) :reader ui-queue-wakeup
                  :documentation "Called after every push, outside the lock, to wake the drainer."))
  (:documentation "A FIFO of events crossing from backend threads to the view thread."))

(-> make-ui-queue (&key (:wakeup function)) ui-queue)
(defun make-ui-queue (&key (wakeup (constantly nil)))
  "Return an empty queue whose WAKEUP function runs after each push.
The application passes LIGHTFAST:AWAKE; headless tests pass nothing."
  (make-instance 'ui-queue :wakeup wakeup))

(-> ui-queue-count (ui-queue) integer)
(defun ui-queue-count (queue)
  "Return the number of pending events."
  (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
    (ui-queue--count queue)))

(-> ui-queue-push (ui-queue ui-event) ui-event)
(defun ui-queue-push (queue event)
  "Enqueue EVENT, stamp its ingress sequence, wake the drainer, and return it.
Safe to call from any thread; the lock is held only for the append."
  (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
    (setf (ui-event-ingress-sequence event) (ui-queue--next-sequence queue))
    (incf (ui-queue--next-sequence queue))
    (push event (ui-queue--items queue))
    (incf (ui-queue--count queue)))
  (funcall (ui-queue-wakeup queue))
  event)

(-> ui-queue-drain (ui-queue &key (:limit integer)) list)
(defun ui-queue-drain (queue &key (limit 256))
  "Remove and return up to LIMIT pending events, oldest first.
The batch is swapped out under the lock and handed back without holding it."
  (let ((batch
          (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
            (let* ((pending (nreverse (ui-queue--items queue)))
                   (taken (min limit (length pending)))
                   (rest (nthcdr taken pending)))
              (setf (ui-queue--items queue) (nreverse rest)
                    (ui-queue--count queue) (length rest))
              (subseq pending 0 taken)))))
    batch))

(-> ui-events--mergeable-text-p (ui-event ui-event) boolean)
(defun ui-events--mergeable-text-p (earlier later)
  "Return T when LATER appends text to the same block as EARLIER on the same connection."
  (and (typep earlier 'transcript-event)
       (typep later 'transcript-event)
       (eq ':append-text (transcript-event-operation earlier))
       (eq ':append-text (transcript-event-operation later))
       (equal (getf (transcript-event-arguments earlier) ':id)
              (getf (transcript-event-arguments later) ':id))
       (ui-event-current-p later (ui-event-backend-id earlier) (ui-event-generation earlier))
       (equal (ui-event-session-id earlier) (ui-event-session-id later))))

(-> ui-events--merge-text (transcript-event transcript-event) transcript-event)
(defun ui-events--merge-text (earlier later)
  "Return one append-text event carrying both texts, keeping EARLIER's identity."
  (let ((merged (make-instance 'transcript-event
                               :backend-id (ui-event-backend-id earlier)
                               :generation (ui-event-generation earlier)
                               :session-id (ui-event-session-id earlier)
                               :received-at (ui-event-received-at earlier)
                               :operation ':append-text
                               :arguments (list ':id (getf (transcript-event-arguments earlier) ':id)
                                                ':text (concatenate 'string
                                                                    (getf (transcript-event-arguments earlier) ':text)
                                                                    (getf (transcript-event-arguments later) ':text))))))
    (setf (ui-event-ingress-sequence merged) (ui-event-ingress-sequence earlier))
    merged))

(-> ui-events--superseded-p (ui-event list) boolean)
(defun ui-events--superseded-p (event later-events)
  "Return T when a later state snapshot or jobs snapshot for the same session makes EVENT moot."
  (and (or (typep event 'state-event)
           (and (typep event 'jobs-event) (eq ':snapshot (jobs-event-kind event))))
       (find-if (lambda (later)
                  (and (eq (class-of later) (class-of event))
                       (or (not (typep later 'jobs-event)) (eq ':snapshot (jobs-event-kind later)))
                       (equal (ui-event-session-id later) (ui-event-session-id event))
                       (ui-event-current-p later (ui-event-backend-id event) (ui-event-generation event))))
                later-events)
       t))

(-> ui-events-coalesce (list) list)
(defun ui-events-coalesce (events)
  "Return EVENTS with adjacent text fragments for one block merged and
superseded state and jobs snapshots dropped. Order is preserved; approvals,
results, connection, session and turn events pass through untouched."
  (let ((result nil))
    (loop for (event . later) on events
          do (cond
               ((ui-events--superseded-p event later))
               ((and result (ui-events--mergeable-text-p (first result) event))
                (setf (first result) (ui-events--merge-text (first result) event)))
               (t
                (push event result))))
    (nreverse result)))


;;; Applying transcript events

(-> transcript-event-apply (transcript-event transcript) t)
(defun transcript-event-apply (event transcript)
  "Perform EVENT's mutation on TRANSCRIPT and return what the mutation returns."
  (let ((arguments (transcript-event-arguments event)))
    (ecase (transcript-event-operation event)
      (:append
       (transcript-append transcript
                          (apply #'make-instance (getf arguments ':class)
                                 (getf arguments ':initargs))))
      (:append-text
       (transcript-append-text transcript (getf arguments ':id) (getf arguments ':text)))
      (:update-tool-call
       (apply #'transcript-update-tool-call transcript (getf arguments ':call-id)
              (getf arguments ':changes)))
      (:replace-plan
       (transcript-replace-plan transcript (getf arguments ':entries)))
      (:decide-approval
       (transcript-decide-approval transcript (getf arguments ':request-id)
                                   (getf arguments ':decision)))
      (:notice
       (apply #'transcript-notice transcript arguments))
      (:reset
       (transcript-reset transcript))
      (:begin-replay
       (transcript-begin-replay transcript))
      (:end-replay
       (transcript-end-replay transcript)))))

(defgeneric apply-ui-event (event target)
  (:documentation "Apply EVENT to TARGET on the view thread. The workbench
specializes TARGET; TRANSCRIPT-EVENT-APPLY handles the transcript part."))
