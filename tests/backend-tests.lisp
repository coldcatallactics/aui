(in-package #:aui)

;;;; -- Deterministic ACP Backend Tests --

(fiveam:def-suite backend :in aui
  :description "The ACP backend over an in-memory agentcomms channel.")
(fiveam:in-suite backend)

(defparameter *backend-test-servers* nil
  "Agent fixtures whose connections must be closed after each backend test.")

(defparameter *backend-test-launch-gate* nil
  "Optional gate before the fixture launcher creates its channel.")

(defclass backend-test-gate ()
  ((lock :initform (bordeaux-threads:make-lock "AUI test gate") :reader backend-test-gate-lock
         :documentation "Protects a deterministic test gate.")
   (condition :initform (bordeaux-threads:make-condition-variable) :reader backend-test-gate-condition
              :documentation "Wakes gate waiters.")
   (entered :initform nil :accessor backend-test-gate-entered
            :documentation "Whether a worker reached the gate.")
   (released :initform nil :accessor backend-test-gate-released
             :documentation "Whether the gate may return."))
  (:documentation "A lock and condition-variable gate for race-free backend tests."))

(-> backend-test-gate-wait (backend-test-gate) null)
(defun backend-test-gate-wait (gate)
  "Block at GATE until the test releases it, recording entry first."
  (bordeaux-threads:with-lock-held ((backend-test-gate-lock gate))
    (setf (backend-test-gate-entered gate) t)
    (bordeaux-threads:condition-notify (backend-test-gate-condition gate))
    (loop until (backend-test-gate-released gate)
          do (bordeaux-threads:condition-wait (backend-test-gate-condition gate)
                                              (backend-test-gate-lock gate))))
  nil)

(-> backend-test-gate-await-entered (backend-test-gate) boolean)
(defun backend-test-gate-await-entered (gate)
  "Wait boundedly until a worker reaches GATE."
  (let ((deadline (+ (get-internal-real-time) (* 3 internal-time-units-per-second))))
    (bordeaux-threads:with-lock-held ((backend-test-gate-lock gate))
      (loop until (backend-test-gate-entered gate)
            when (>= (get-internal-real-time) deadline) do (return nil)
            do (bordeaux-threads:condition-wait (backend-test-gate-condition gate)
                                                (backend-test-gate-lock gate)
                                                :timeout 0.05))
      (backend-test-gate-entered gate))))

(-> backend-test-gate-release (backend-test-gate) null)
(defun backend-test-gate-release (gate)
  "Release GATE and wake its blocked worker."
  (bordeaux-threads:with-lock-held ((backend-test-gate-lock gate))
    (setf (backend-test-gate-released gate) t)
    (bordeaux-threads:condition-notify (backend-test-gate-condition gate)))
  nil)

(defclass backend-test-agent (acp:acp-agent)
  ((prompt-mode :initform :stream :accessor backend-test-agent-prompt-mode
                :documentation "Fixture prompt behavior.")
   (mode-gate :initform nil :accessor backend-test-agent-mode-gate
              :documentation "Optional gate in the mode RPC.")
   (load-gate :initform nil :accessor backend-test-agent-load-gate
              :documentation "Optional gate in the load RPC.")
   (load-failure :initform nil :accessor backend-test-agent-load-failure
                 :documentation "Optional condition text for load failure."))
  (:documentation "A deterministic ACP agent used only by the backend tests."))

(defmethod acp:agent-capabilities ((agent backend-test-agent))
  "Advertise the methods exercised by the fixture."
  (declare (ignore agent))
  (acp:acp-agent-capabilities :load-session t :close t))

(defmethod acp:agent-new-session
    ((agent backend-test-agent) &key cwd mcp-servers additional-directories params)
  "Create the fixed fixture session and return its id and extras."
  (declare (ignore agent cwd mcp-servers additional-directories params))
  (values "fixture-session" (acp:json-object)))

(defmethod acp:agent-load-session
    ((agent backend-test-agent) &key session-id cwd mcp-servers additional-directories params)
  "Load the fixed fixture session and replay one historical message."
  (declare (ignore cwd mcp-servers additional-directories params))
  (when (backend-test-agent-load-gate agent)
    (backend-test-gate-wait (backend-test-agent-load-gate agent)))
  (when (backend-test-agent-load-failure agent)
    (error "~A" (backend-test-agent-load-failure agent)))
  (acp:agent-send-update agent session-id
                         (acp:acp-update-agent-message
                          (acp:acp-text-content "replayed")))
  (acp:json-object))

(defmethod acp:agent-close-session ((agent backend-test-agent) session-id params)
  "Close the fixture session without retaining resources."
  (declare (ignore agent session-id params))
  nil)

(defmethod acp:agent-set-mode ((agent backend-test-agent) session-id mode-id params)
  "Record the selected fixture mode."
  (declare (ignore session-id params))
  (when (backend-test-agent-mode-gate agent)
    (backend-test-gate-wait (backend-test-agent-mode-gate agent)))
  (setf (backend-test-agent-prompt-mode agent) mode-id)
  nil)

(defmethod acp:agent-prompt ((agent backend-test-agent) session-id prompt params)
  "Emit deterministic interleaved reasoning, text and tool updates."
  (declare (ignore prompt params))
  (acp:agent-send-update agent session-id
                         (acp:acp-update-agent-thought
                          (acp:acp-text-content "thinking")))
  (acp:agent-send-update agent session-id
                         (acp:acp-update-agent-message
                          (acp:acp-text-content "hello")))
    (when (eq (backend-test-agent-prompt-mode agent) :permission)
      (multiple-value-bind (outcome option)
          (acp:agent-request-permission
           agent session-id
           (acp:acp-tool-call "fixture-call" "Run fixture" :name "shell.run"
                              :kind :execute :status :pending
                              :raw-input (acp:json-object "command" "true"))
           (list (acp:acp-permission-option "allow-once" "Allow once" :allow-once)
                 (acp:acp-permission-option "reject-once" "Reject once" :reject-once)))
        (declare (ignore outcome option))))
    (when (eq (backend-test-agent-prompt-mode agent) :cancel)
      (sleep 0.05)
      (acp:agent-check-cancelled agent session-id))
  (acp:agent-send-update agent session-id
                         (acp:acp-update-tool-call
                          (acp:acp-tool-call "fixture-call" "Run fixture" :name "shell.run"
                                              :kind :execute :status :completed
                                              :raw-output (acp:json-object "stdout" "ok"))))
  :end-turn)

(-> backend-test-launcher (acp-backend) t)
(defun backend-test-launcher (backend)
  "Launch a real in-memory agentcomms server for BACKEND."
  (declare (ignore backend))
  (when *backend-test-launch-gate*
    (backend-test-gate-wait *backend-test-launch-gate*))
  (multiple-value-bind (client-channel server-channel) (acp:make-acp-channel-pair)
    (let ((server (make-instance 'backend-test-agent)))
      (push server *backend-test-servers*)
      (acp:acp-agent-connect server server-channel :name "aui backend fixture")
      client-channel)))

(-> backend-test-close-servers () null)
(defun backend-test-close-servers ()
  "Close every fixture connection and release its channel threads."
  (dolist (server *backend-test-servers*)
    (let ((connection (acp:acp-agent-connection server)))
      (when connection
        (ignore-errors (acp:connection-close connection)))))
  (setf *backend-test-servers* nil))

(defmacro with-backend-fixture ((backend queue) &body body)
  "Run BODY with a real in-memory ACP backend and clean up on unwind."
  `(let* ((,queue (make-ui-queue))
          (,backend (make-acp-backend "/tmp/aui-backend-fixture/" ,queue
                                      :launcher #'backend-test-launcher)))
     (unwind-protect (progn ,@body)
       (ignore-errors (backend-stop ,backend))
       (backend-test-events-until
        ,queue (lambda (event)
                 (and (typep event 'connection-event)
                      (eq (connection-event-state event) ':exited))))
       (backend-test-close-servers))))

(defun backend-test-drain (queue)
  "Drain QUEUE at the test observation boundary."
  (ui-events-coalesce (ui-queue-drain queue :limit 10000)))

(defun backend-test-events-until (queue predicate &key (seconds 3))
  "Return observed events once PREDICATE sees one, or NIL on timeout."
  (let ((events nil)
        (deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop do (setf events (nconc events (backend-test-drain queue)))
          when (find-if predicate events) do (return events)
          when (>= (get-internal-real-time) deadline) do (return nil)
          do (sleep 0.01))))

(fiveam:test backend-start-and-open-session
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'connection-event)
                             (eq (connection-event-state event) ':ready)))))
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))))

(fiveam:test backend-streams-reasoning-text-and-tool-lifecycle
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is
     (backend-test-events-until
      queue (lambda (event)
              (and (typep event 'session-event)
                   (eq (session-event-state event) ':ready)))))
    (backend-prompt backend "hello")
    (let ((events
            (backend-test-events-until
             queue (lambda (event)
                     (and (typep event 'turn-event)
                            (eq (turn-event-state event) ':idle))))))
      (fiveam:is (not (null events)))
      (fiveam:is
       (not (null
             (find-if
              (lambda (event)
                (and (typep event 'transcript-event)
                     (eq (transcript-event-operation event) ':append-text)))
              events))))
      (fiveam:is
       (not (null (find-if (lambda (event) (typep event 'result-event))
                           events)))))))

(fiveam:test backend-permission-decision-and-cancellation
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let ((server (first *backend-test-servers*)))
      (setf (backend-test-agent-prompt-mode server) ':permission)
      (backend-prompt backend "permission")
      (let ((events (backend-test-events-until
                     queue (lambda (event) (typep event 'approval-event))))
            (approval nil))
        (fiveam:is (not (null events)))
        (setf approval (find-if (lambda (event) (typep event 'approval-event))
                                events))
        (backend-decide-approval backend
                                 (approval-event-request-id approval)
                                 "allow-once")
        (fiveam:is (backend-test-events-until
                    queue (lambda (event)
                            (and (typep event 'turn-event)
                                   (eq (turn-event-state event) ':idle))))))
      (setf (backend-test-agent-prompt-mode server) ':permission)
      (backend-prompt backend "cancel")
      (fiveam:is (backend-test-events-until
                  queue (lambda (event) (typep event 'approval-event))))
      (backend-cancel backend)
      (fiveam:is (backend-test-events-until
                  queue (lambda (event)
                          (and (typep event 'turn-event)
                                 (eq (turn-event-state event) ':idle))))))))

(fiveam:test backend-rejects-prompt-before-ready-and-overlap
  (with-backend-fixture (backend queue)
    (let ((request (backend-prompt backend "too soon")))
      (fiveam:is (backend-test-events-until
                  queue (lambda (event)
                          (and (typep event 'result-event)
                               (equal request (result-event-request-id event)))))))
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (backend-prompt backend "one")
    (let ((request (backend-prompt backend "two")))
      (fiveam:is (backend-test-events-until
                  queue (lambda (event)
                          (and (typep event 'result-event)
                               (equal request (result-event-request-id event)))))))))

(fiveam:test backend-mode-replay-and-close
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let ((request (backend-set-mode backend "auto")))
      (fiveam:is
       (backend-test-events-until
        queue (lambda (event)
                (and (typep event 'result-event)
                     (equal request (result-event-request-id event)))))))
    (backend-open-conversation backend :conversation-id "fixture-session")
    (let ((events (backend-test-events-until
                   queue (lambda (event)
                           (and (typep event 'session-event)
                                (eq (session-event-state event) ':ready))))))
      (fiveam:is (not (null (find-if
                             (lambda (event)
                               (and (typep event 'transcript-event)
                                    (eq (transcript-event-operation event)
                                        ':append-text)))
                             events)))))
    (backend-stop backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'connection-event)
                             (eq (connection-event-state event) ':exited)))))))

(fiveam:test backend-cancels-prompt-queued-before-transmission
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let* ((server (first *backend-test-servers*))
           (gate (make-instance 'backend-test-gate)))
      (setf (backend-test-agent-mode-gate server) gate)
      (backend-set-mode backend "auto")
      (fiveam:is (backend-test-gate-await-entered gate))
      (let ((request (backend-prompt backend "queued")))
        (backend-cancel backend)
        (backend-test-gate-release gate)
        (let ((events (backend-test-events-until
                       queue (lambda (event)
                               (and (typep event 'result-event)
                                    (equal request (result-event-request-id event)))))))
          (fiveam:is (not (null events)))
          (fiveam:is (eq ':cancelled
                         (result-event-outcome
                          (find-if (lambda (event)
                                     (and (typep event 'result-event)
                                          (equal request (result-event-request-id event))))
                                   events)))))))))

(fiveam:test backend-rejects-prompt-during-delayed-load
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let* ((server (first *backend-test-servers*))
           (gate (make-instance 'backend-test-gate)))
      (setf (backend-test-agent-load-gate server) gate)
      (backend-open-conversation backend :conversation-id "fixture-session")
      (fiveam:is (backend-test-gate-await-entered gate))
      (let ((request (backend-prompt backend "during-load")))
        (let ((events (backend-test-events-until
                       queue (lambda (event)
                               (and (typep event 'result-event)
                                    (equal request (result-event-request-id event)))))))
          (fiveam:is (eq ':busy
                         (result-event-outcome
                          (find-if (lambda (event)
                                     (and (typep event 'result-event)
                                          (equal request (result-event-request-id event))))
                                   events))))))
      (backend-test-gate-release gate))))

(fiveam:test backend-failed-load-closes-replay-and-rejects-prompt
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let ((server (first *backend-test-servers*)))
      (setf (backend-test-agent-load-failure server) "fixture load failed")
      (backend-open-conversation backend :conversation-id "fixture-session")
      (let ((events (backend-test-events-until
                     queue (lambda (event)
                             (and (typep event 'session-event)
                                  (eq (session-event-state event) ':closed))))))
        (fiveam:is (not (null events)))
        (fiveam:is (find-if (lambda (event)
                              (and (typep event 'transcript-event)
                                   (eq (transcript-event-operation event) ':end-replay)))
                            events)))
      (let ((request (backend-prompt backend "after-failure")))
        (let ((events (backend-test-events-until
                       queue (lambda (event)
                               (and (typep event 'result-event)
                                    (equal request (result-event-request-id event)))))))
          (fiveam:is (eq ':busy
                         (result-event-outcome
                          (find-if (lambda (event)
                                     (and (typep event 'result-event)
                                          (equal request (result-event-request-id event))))
                                   events)))))))))

(fiveam:test backend-shutdown-closes-channel-created-after-gate
  (with-backend-fixture (backend queue)
    (let ((gate (make-instance 'backend-test-gate)))
      (setf *backend-test-launch-gate* gate)
      (backend-start backend)
      (fiveam:is (backend-test-gate-await-entered gate))
      (backend-stop backend)
      (fiveam:is (not (backend-join backend :timeout 0.01)))
      (backend-test-gate-release gate)
      (setf *backend-test-launch-gate* nil)
      (fiveam:is (backend-test-events-until
                  queue (lambda (event)
                          (and (typep event 'connection-event)
                               (eq (connection-event-state event) ':exited)))))
      (fiveam:is (backend-join backend :timeout 1))
      (fiveam:is (not (bordeaux-threads:thread-alive-p (acp-backend--worker backend))))
      (fiveam:is (not (bordeaux-threads:thread-alive-p (acp-backend--shutdown-worker backend)))))))

(fiveam:test backend-late-permission-while-idle-is-cancelled
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let* ((client (acp-backend--client backend))
           (session (acp-backend--session backend))
           (tool (acp:acp-tool-call "late" "Late permission" :name "shell.run"))
           (options (vector (acp:json-object "optionId" "allow-once"
                                             "name" "Allow once"
                                             "kind" "allow_once"))))
      (fiveam:is (eq ':cancelled
                     (agentcomms:client-request-permission client session tool options nil)))
      (fiveam:is (null (find-if (lambda (event) (typep event 'approval-event))
                                (backend-test-drain queue)))))))

(fiveam:test backend-unsupported-catalog-is-typed
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-open-conversation backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'session-event)
                             (eq (session-event-state event) ':ready)))))
    (let ((request (backend-refresh backend ':models)))
      (let ((events (backend-test-events-until
                     queue (lambda (event)
                             (and (typep event 'result-event)
                                  (equal request
                                         (result-event-request-id event)))))))
        (let ((result (find-if
                       (lambda (event)
                         (and (typep event 'result-event)
                              (equal request
                                     (result-event-request-id event))))
                       events)))
          (fiveam:is (not (null result)))
          (fiveam:is (eq ':unsupported (result-event-outcome result))))))))

(fiveam:test backend-stop-publishes-exited
  (with-backend-fixture (backend queue)
    (backend-start backend)
    (backend-stop backend)
    (fiveam:is (backend-test-events-until
                queue (lambda (event)
                        (and (typep event 'connection-event)
                             (eq (connection-event-state event) ':exited)))))))
