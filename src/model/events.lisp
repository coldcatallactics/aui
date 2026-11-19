(in-package #:aui)

;;;; -- Interface Events --

;;; Immutable messages from the client threads to the interface thread, and
;;; the queue for these messages. Reader and worker threads push events to the
;;; queue. The interface thread takes the events from the queue and applies
;;; them.

(defclass ui-event ()
  ((channel
    :initarg       :channel
    :initform      nil
    :reader        ui-event-channel
    :type          (or null string)
    :documentation "The channel of the event, or NIL for events about the world.")
   (generation
    :initarg       :generation
    :initform      0
    :reader        ui-event-generation
    :type          integer
    :documentation "The generation of the attachment that produced the event.
Events of an attachment that was already left are dropped."))
  (:documentation "The base class of all messages to the interface thread."))

(defclass channel-event (ui-event)
  ((payload
    :initarg       :payload
    :reader        channel-event-payload
    :type          list
    :documentation "One event of the structured client protocol of Autolith, as it arrived."))
  (:documentation "An event from the channel, such as a record, streamed text, a status or an approval."))

(defclass history-event (ui-event)
  ((records
    :initarg       :records
    :initform      nil
    :reader        history-event-records
    :type          list
    :documentation "Durable conversation records, oldest first.")
   (more-p
    :initarg       :more-p
    :initform      nil
    :reader        history-event-more-p
    :type          boolean
    :documentation "True if older records exist."))
  (:documentation "A page of the durable history of a channel. The page is older than all shown entries."))

(defclass attachment-event (ui-event)
  ((state
    :initarg       :state
    :reader        attachment-event-state
    :type          (member :opening :attached :failed :closed)
    :documentation "The state of the attachment.")
   (detail
    :initarg       :detail
    :initform      nil
    :reader        attachment-event-detail
    :documentation "The detail of the state. For :OPENING, it is a list (STEP DESCRIPTION).
For :ATTACHED, it is the status plist of the channel. For :FAILED or :CLOSED,
it is a report string or NIL."))
  (:documentation "A change of the attachment of the interface to a channel."))

(defclass result-event (ui-event)
  ((request
    :initarg       :request
    :reader        result-event-request
    :documentation "The request, as the value given with the request.")
   (outcome
    :initarg       :outcome
    :reader        result-event-outcome
    :type          (member :ok :condition :busy :unsupported :cancelled)
    :documentation "The outcome of the request.")
   (value
    :initarg       :value
    :initform      nil
    :reader        result-event-value
    :documentation "The answer in the portable form of Autolith if the outcome is :OK.")
   (report
    :initarg       :report
    :initform      nil
    :reader        result-event-report
    :type          (or null string)
    :documentation "The reason for the failure of the request, as text."))
  (:documentation "The result of a request to a channel."))


;;;; -- Queue --

(defclass ui-queue ()
  ((lock
    :initform      (bordeaux-threads:make-lock "AUI events")
    :reader        ui-queue--lock
    :documentation "The lock that protects the pending events.")
   (items
    :initform      nil
    :accessor      ui-queue--items
    :type          list
    :documentation "The pending events, newest first.")
   (wakeup
    :initarg       :wakeup
    :initform      (constantly nil)
    :reader        ui-queue-wakeup
    :type          function
    :documentation "The function that wakes the interface thread.
It is called after each push, outside the lock."))
  (:documentation "A first-in, first-out queue of events for the interface thread."))

(-> make-ui-queue (&key (:wakeup function)) ui-queue)
(defun make-ui-queue (&key (wakeup (constantly nil)))
  "Return an empty queue. The WAKEUP function is called after each push."
  (make-instance 'ui-queue :wakeup wakeup))

(-> ui-queue-push (ui-queue ui-event) ui-event)
(defun ui-queue-push (queue event)
  "Add EVENT to QUEUE, call the wakeup function and return EVENT.
This function can be called from any thread."
  (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
    (push event (ui-queue--items queue)))
  (funcall (ui-queue-wakeup queue))
  event)

(-> ui-queue-drain (ui-queue) list)
(defun ui-queue-drain (queue)
  "Remove all pending events from QUEUE and return them, oldest first."
  (nreverse
   (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
     (shiftf (ui-queue--items queue) nil))))
