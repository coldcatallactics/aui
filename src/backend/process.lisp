(in-package #:aui)

;;;; -- ACP Process Ownership --

(define-condition backend-state-error (error)
  ((reason :initarg :reason :reader backend-state-error-reason
           :documentation "Why this operation cannot run."))
  (:report (lambda (condition stream)
             (write-string (backend-state-error-reason condition) stream)))
  (:documentation "An operation was requested outside its backend lifecycle."))

(defclass acp-backend (backend)
  ((lock :initform (bordeaux-threads:make-lock "AUI backend") :reader acp-backend--lock
         :documentation "Protects lifecycle, requests and permission waiters.")
   (projection-lock :initform (bordeaux-threads:make-lock "AUI projection")
                    :reader acp-backend--projection-lock
                    :documentation "Orders transcript projection from concurrent callbacks.")
   (transport-lock :initform (bordeaux-threads:make-lock "AUI transport ownership")
                   :reader acp-backend--transport-lock
                   :documentation "Serializes connection attachment with shutdown.")
   (condition :initform (bordeaux-threads:make-condition-variable) :reader acp-backend--condition
              :documentation "Wakes the operation worker.")
   (requests :initform nil :accessor acp-backend--requests
             :documentation "FIFO normal operations, each an identifier and function.")
   (worker :initform nil :accessor acp-backend--worker :documentation "The owned operation thread.")
   (state :initform ':new :accessor acp-backend--state :documentation "Process lifecycle state.")
   (client :initform nil :accessor acp-backend--client :documentation "The attached ACP client.")
   (channel :initform nil :accessor acp-backend--channel :documentation "The owned process channel.")
   (session :initform nil :accessor acp-backend--session :documentation "The open canonical session identifier.")
   (session-state :initform ':closed :accessor acp-backend--session-state
                  :documentation "Session admission state: closed, loading or ready.")
   (prompt-wire-id :initform nil :accessor acp-backend--prompt-wire-id
                   :documentation "The transmitted active prompt's JSON-RPC identifier.")
   (cancel-worker :initform nil :accessor acp-backend--cancel-worker
                  :documentation "The active turn's cancellation sender, joined before settling.")
   (shutdown-worker :initform nil :accessor acp-backend--shutdown-worker
                    :documentation "The owned shutdown thread, if shutdown was requested.")
   (turn :initform ':idle :accessor acp-backend--turn :documentation "Current prompt lifecycle.")
   (turn-token :initform nil :accessor acp-backend--turn-token
               :documentation "The admitted prompt identity, cleared before permission cleanup.")
   (mode :initform ':ask :accessor acp-backend--mode :documentation "The last confirmed permission mode.")
   (counter :initform 0 :accessor acp-backend--counter :documentation "Local request and segment identity counter.")
   (approvals :initform (make-hash-table :test 'equal) :reader acp-backend--approvals
              :documentation "Permission waiters owned by this connection.")
   (launcher :initarg :launcher :reader acp-backend--launcher
             :documentation "Function of this backend returning an owned channel.")
   (model :initarg :model :reader acp-backend-model :documentation "The requested model for new conversations.")
   (effort :initarg :effort :reader acp-backend-effort :documentation "The requested reasoning effort.")
   (loaded-p :initform nil :accessor acp-backend--loaded-p
             :documentation "Whether durable model choices may differ from launch defaults.")
   (extension-methods :initform nil :accessor acp-backend--extension-methods
                      :documentation "The _autolith/* methods the connected Autolith advertised.")
   (external-turn :initform nil :accessor acp-backend--external-turn
                  :documentation "The server epoch of a turn the server owns, such as an automatic
continuation or an invocation, while it runs; NIL otherwise."))
  (:documentation "One pristine Autolith process with an independent cancellation path."))

(defparameter *acp-event-generation* nil
  "The originating generation bound in each owned operation or callback.")

(defparameter *acp-event-session* ':current
  "The callback's captured session identity, or :CURRENT for owner operations.")

(-> acp-backend--identifier (acp-backend) string)
(defun acp-backend--identifier (backend)
  "Return a fresh backend-qualified identifier, safely across callbacks."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (format nil "~A:~D:~D" (backend-id backend) (backend-generation backend)
            (incf (acp-backend--counter backend)))))

(-> acp-backend--post (acp-backend symbol &rest t) ui-event)
(defun acp-backend--post (backend class &rest initargs)
  "Publish an event carrying its original connection identity."
  (ui-queue-push
   (backend-queue backend)
   (apply #'make-instance class :backend-id (backend-id backend)
          :generation (or *acp-event-generation* (backend-generation backend))
           :session-id (if (eq *acp-event-session* ':current)
                           (acp-backend--session backend)
                           *acp-event-session*) initargs)))

(-> acp-backend--result (acp-backend string result-outcome &key (:value t) (:report t) (:condition-type t)) ui-event)
(defun acp-backend--result (backend request outcome &key value report condition-type)
  "Publish a portable request outcome, without retaining a live condition."
  (acp-backend--post backend 'result-event :request-id request :outcome outcome
                     :value value :report report :condition-type condition-type))

(-> acp-backend--failure (acp-backend string condition) ui-event)
(defun acp-backend--failure (backend request condition)
  "Report a bounded diagnostic for an operation failure."
  (let ((report (princ-to-string condition)))
    (acp-backend--result backend request ':condition
                         :condition-type (princ-to-string (type-of condition))
                         :report (subseq report 0 (min 2000 (length report))))))

(-> acp-backend--queue-request (acp-backend string function) null)
(defun acp-backend--queue-request (backend request function)
  "Append an operation while the caller holds the backend state lock."
  (setf (acp-backend--requests backend)
        (nconc (acp-backend--requests backend) (list (list request function))))
  (bordeaux-threads:condition-notify (acp-backend--condition backend))
  nil)

(-> acp-backend--enqueue (acp-backend string function) boolean)
(defun acp-backend--enqueue (backend request function)
  "Queue a normal operation, or report that the connection is unavailable."
  (let ((accepted
          (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
            (when (member (acp-backend--state backend) '(:starting :ready))
              (acp-backend--queue-request backend request function)
              t))))
    (unless accepted
      (acp-backend--result backend request ':condition :report "The backend is not connected."))
    (not (null accepted))))

(-> acp-backend--worker-loop (acp-backend integer) null)
(defun acp-backend--worker-loop (backend generation)
  "Serve normal operations; cancellation and shutdown use a separate path."
  (let ((*acp-event-generation* generation))
    (loop
      for request =
        (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
          (loop while (and (null (acp-backend--requests backend))
                           (member (acp-backend--state backend) '(:starting :ready)))
                do (bordeaux-threads:condition-wait (acp-backend--condition backend)
                                                    (acp-backend--lock backend)))
          (when (member (acp-backend--state backend) '(:starting :ready))
            (pop (acp-backend--requests backend))))
      while request
      do (handler-case (funcall (second request))
           (error (condition) (acp-backend--failure backend (first request) condition)))))
  nil)

(defparameter *autolith-command* "autolith"
  "The Autolith launcher the backend starts, found on the path unless it is an
absolute pathname. The AUI_AUTOLITH environment variable overrides it, which is
how a development build of Autolith is tested.")

(-> autolith-command () string)
(defun autolith-command ()
  "Return the launcher to start, honoring AUI_AUTOLITH when it is set."
  (let ((override (uiop:getenv "AUI_AUTOLITH")))
    (if (and override (plusp (length override)))
        override
        *autolith-command*)))

(-> acp-backend--launch (acp-backend) agentcomms:acp-channel)
(defun acp-backend--launch (backend)
  "Launch only the requested pristine process, preserving the inherited environment."
  (agentcomms:acp-launch-agent
   "env" :arguments (list (format nil "AUTOLITH_MODEL=~A" (acp-backend-model backend))
                           (format nil "AUTOLITH_REASONING_EFFORT=~A" (acp-backend-effort backend))
                           (autolith-command) "--pristine" "acp")
   :directory (backend-directory backend)))

(-> make-acp-backend (string ui-queue &key (:model string) (:effort string) (:launcher function)) acp-backend)
(defun make-acp-backend (directory queue &key (model "gpt-5.6-luna") (effort "low")
                                             (launcher #'acp-backend--launch))
  "Return an unstarted backend for DIRECTORY; LAUNCHER permits protocol fixtures."
  (make-instance 'acp-backend :id (make-request-id)
                             :directory (namestring (uiop:ensure-directory-pathname directory))
                             :queue queue :model model :effort effort :launcher launcher))

(-> acp-backend--require-ready (acp-backend) t)
(defun acp-backend--require-ready (backend)
  "Require a live initialized client before an operation performs IPC."
  (unless (eq (acp-backend--state backend) ':ready)
    (error 'backend-state-error :reason "The backend connection is not ready."))
  t)

(-> acp-backend--require-session (acp-backend) string)
(defun acp-backend--require-session (backend)
  "Return the ready session identifier or signal a lifecycle error."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (acp-backend--require-ready backend)
    (unless (eq (acp-backend--session-state backend) ':ready)
      (error 'backend-state-error :reason "The conversation is not ready."))
    (acp-backend--session backend)))

(-> acp-backend--snapshot (acp-backend) state-snapshot)
(defun acp-backend--snapshot (backend)
  "Describe confirmed ACP state, leaving unavailable measurements unknown."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (make-instance 'state-snapshot :workspace (backend-directory backend)
                   :model (unless (acp-backend--loaded-p backend) (acp-backend-model backend))
                   :effort (unless (acp-backend--loaded-p backend) (acp-backend-effort backend))
                   :permission-mode (acp-backend--mode backend)
                   :turn-state (acp-backend--turn backend)
                   :health (string-downcase (symbol-name (acp-backend--state backend))))))

(-> acp-backend--publish-state (acp-backend) ui-event)
(defun acp-backend--publish-state (backend)
  "Publish the current confirmed state as an immutable snapshot."
  (acp-backend--post backend 'state-event :snapshot (acp-backend--snapshot backend)))

(-> acp-backend--abandon-requests (acp-backend) null)
(defun acp-backend--abandon-requests (backend)
  "Cancel queued operations after admission has stopped."
  (let ((requests
          (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
            (prog1 (acp-backend--requests backend)
              (setf (acp-backend--requests backend) nil)
              (bordeaux-threads:condition-notify (acp-backend--condition backend))))))
    (dolist (request requests)
      (acp-backend--result backend (first request) ':cancelled)))
  nil)
