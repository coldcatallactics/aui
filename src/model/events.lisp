(in-package #:aui)

;;;; -- Interface Events --

;;; Immutable messages from the client threads to the interface thread, and
;;; the queue that carries them. Reader and worker threads push; the interface
;;; thread drains and applies. Nothing here touches a widget or a transcript.

(defclass ui-event ()
  ((channel
    :initarg       :channel
    :initform      nil
    :reader        ui-event-channel
    :type          (or null string)
    :documentation "The channel the event belongs to, or NIL for an event about the world.")
   (generation
    :initarg       :generation
    :initform      0
    :reader        ui-event-generation
    :type          integer
    :documentation "Which attachment of the interface produced the event; an event of an attachment already left is dropped."))
  (:documentation "The base of every message to the interface thread."))

(defclass channel-event (ui-event)
  ((payload
    :initarg       :payload
    :reader        channel-event-payload
    :type          list
    :documentation "One event of Autolith's structured client protocol, as it arrived."))
  (:documentation "The channel said something: a record, streamed text, a status, an approval."))

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
    :documentation "Whether older records exist beyond these."))
  (:documentation "A page of a channel's durable history, older than everything shown."))

(defclass attachment-event (ui-event)
  ((state
    :initarg       :state
    :reader        attachment-event-state
    :type          (member :opening :attached :failed :closed)
    :documentation "Where the attachment stands.")
   (detail
    :initarg       :detail
    :initform      nil
    :reader        attachment-event-detail
    :documentation "For :OPENING a (STEP DESCRIPTION) pair, for :ATTACHED the channel's status plist, for :FAILED or :CLOSED a report string or NIL."))
  (:documentation "The interface's attachment to a channel changed."))

(defclass result-event (ui-event)
  ((request
    :initarg       :request
    :reader        result-event-request
    :documentation "What the interface asked, as the value it gave when asking.")
   (outcome
    :initarg       :outcome
    :reader        result-event-outcome
    :type          (member :ok :condition :busy :unsupported :cancelled)
    :documentation "How the request ended.")
   (value
    :initarg       :value
    :initform      nil
    :reader        result-event-value
    :documentation "The answer in Autolith's portable form when the outcome is :OK.")
   (report
    :initarg       :report
    :initform      nil
    :reader        result-event-report
    :type          (or null string)
    :documentation "Why the request did not succeed, in words."))
  (:documentation "A request to a channel finished."))


;;;; -- Queue --

(defclass ui-queue ()
  ((lock
    :initform      (bordeaux-threads:make-lock "AUI events")
    :reader        ui-queue--lock
    :documentation "Guards the pending events.")
   (items
    :initform      nil
    :accessor      ui-queue--items
    :type          list
    :documentation "Pending events, newest first.")
   (wakeup
    :initarg       :wakeup
    :initform      (constantly nil)
    :reader        ui-queue-wakeup
    :type          function
    :documentation "Called after every push, outside the lock, to wake the interface thread."))
  (:documentation "A first-in, first-out queue of events crossing to the interface thread."))

(-> make-ui-queue (&key (:wakeup function)) ui-queue)
(defun make-ui-queue (&key (wakeup (constantly nil)))
  "Return an empty queue whose WAKEUP function runs after each push."
  (make-instance 'ui-queue :wakeup wakeup))

(-> ui-queue-push (ui-queue ui-event) ui-event)
(defun ui-queue-push (queue event)
  "Enqueue EVENT from any thread, wake the interface thread, and return EVENT."
  (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
    (push event (ui-queue--items queue)))
  (funcall (ui-queue-wakeup queue))
  event)

(-> ui-queue-drain (ui-queue) list)
(defun ui-queue-drain (queue)
  "Remove and return every pending event, oldest first."
  (nreverse
   (bordeaux-threads:with-lock-held ((ui-queue--lock queue))
     (shiftf (ui-queue--items queue) nil))))
