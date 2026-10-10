(in-package #:aui)

(fiveam:def-suite extensions :in aui
  :description "The _autolith/* extension adapter over an in-memory agent.")

(fiveam:in-suite extensions)

(defclass extension-test-agent (backend-test-agent)
  ((settings :initform (list (acp:json-object "name" "model" "label" "Model" "group" "Model"
                                              "documentation" "The answering model." "type" "choice"
                                              "options" (vector "gpt-5.6-luna" "gpt-6-astra")
                                              "value" "gpt-5.6-luna" "source" "override" "scope" "durable"
                                              "adjustable" t)
                             (acp:json-object "name" "immutable-p" "label" "Immutable" "group" "Process"
                                              "type" "boolean" "value" (argo:json-false) "source" "default"
                                              "scope" "process" "adjustable" (argo:json-false)
                                              "reason" "fixed for the process"))
             :accessor extension-test-agent-settings
             :documentation "The mutable settings catalog the fixture serves.")
   (cancelled :initform nil :accessor extension-test-agent-cancelled
              :documentation "Job ids cancelled through the fixture.")
   (invoked :initform nil :accessor extension-test-agent-invoked
            :documentation "Operations invoked through the fixture."))
  (:documentation "An ACP agent that also serves the _autolith/* contract."))

(-> extension-ok (t) hash-table)
(defun extension-ok (value)
  "Return a successful extension response carrying VALUE."
  (acp:json-object "outcome" "ok" "value" value))

(-> extension-items (list) hash-table)
(defun extension-items (items)
  "Return a catalog value with ITEMS."
  (acp:json-object "items" (coerce items 'vector)))

(defmethod acp:agent-extension-request ((agent extension-test-agent) method params)
  "Serve the first-slice methods plus jobs, invoke and job actions."
  (cond
    ((string= method "_autolith/capabilities")
     (extension-ok (acp:json-object "schemaVersion" 1
                                    "methods" (vector "_autolith/state" "_autolith/settings"
                                                      "_autolith/set-setting" "_autolith/models"
                                                      "_autolith/operations" "_autolith/invoke"
                                                      "_autolith/jobs" "_autolith/job-cancel"
                                                      "_autolith/job-send")
                                    "features" (acp:json-object))))
    ((string= method "_autolith/state")
     (extension-ok (acp:json-object "workspace" "/tmp/aui-backend-fixture/" "title" "Fixture talk"
                                    "model" "gpt-5.6-luna" "effort" "low" "permissionMode" "ask"
                                    "turnState" "idle"
                                    "usage" (acp:json-object "input" 1200 "output" 300 "cached" :null
                                                             "contextUsed" 50000 "contextLimit" 200000
                                                             "contextMeasured" t)
                                    "liveJobCount" 1 "planPresent" t "health" "ok")))
    ((string= method "_autolith/settings")
     (extension-ok (extension-items (extension-test-agent-settings agent))))
    ((string= method "_autolith/set-setting")
     (let* ((name (acp:json-get params "name"))
            (item (find name (extension-test-agent-settings agent)
                        :key (lambda (setting) (acp:json-get setting "name")) :test #'equal)))
       (cond
         ((and item (acp:json-get item "adjustable"))
          (setf (gethash "value" item) (acp:json-get params "value")
                (gethash "source" item) "session")
          (extension-ok item))
         (t
          (acp:json-object "outcome" "condition" "report" "That setting cannot change."
                           "conditionType" "configuration-error")))))
    ((string= method "_autolith/models")
     (extension-ok (extension-items (list (acp:json-object "id" "gpt-5.6-luna" "provider" "chatgpt"
                                                           "credentialStored" t "recentlyVerified" t
                                                           "contextWindow" 400000)))))
    ((string= method "_autolith/operations")
     (extension-ok (extension-items (list (acp:json-object "name" "/compact" "kind" "command"
                                                           "description" "compact now" "admitted" t
                                                           "terminalOwning" (argo:json-false))
                                          (acp:json-object "name" "/settings" "kind" "command"
                                                           "description" "picker" "admitted" t
                                                           "terminalOwning" t)))))
    ((string= method "_autolith/invoke")
     (push (acp:json-get params "operation") (extension-test-agent-invoked agent))
     (acp:connection-notify (acp:acp-agent-connection agent) "_autolith/event"
                            (acp:json-object "sessionId" (acp:json-get params "sessionId") "sequence" 1
                                             "kind" "compaction" "data" (acp:json-object)))
     (extension-ok (acp:json-object "text" "Compacted 3 records." "data" :null)))
    ((string= method "_autolith/jobs")
     (extension-ok (extension-items (list (acp:json-object "id" "job-7" "parentId" :null "role" "reviewer"
                                                           "state" "running" "startedAt" 4000000000
                                                           "elapsedSeconds" 42 "usage" :null
                                                           "artifacts" (vector "a.txt") "activity" "reading")))))
    ((string= method "_autolith/job-cancel")
     (push (acp:json-get params "jobId") (extension-test-agent-cancelled agent))
     (extension-ok (acp:json-object)))
    ((string= method "_autolith/job-send")
     (extension-ok (acp:json-object)))
    (t
     (call-next-method))))

(-> extension-test-launcher (acp-backend) t)
(defun extension-test-launcher (backend)
  "Launch an in-memory agent that serves the extensions."
  (declare (ignore backend))
  (multiple-value-bind (client-channel server-channel) (acp:make-acp-channel-pair)
    (let ((server (make-instance 'extension-test-agent)))
      (push server *backend-test-servers*)
      (acp:acp-agent-connect server server-channel :name "aui extension fixture")
      client-channel)))

(defmacro with-extension-fixture ((backend queue &key (launcher '#'extension-test-launcher)) &body body)
  "Run BODY with a started backend and a session being opened over LAUNCHER's
agent. BODY's first wait sees every event from the start, including the
catalog bootstrap that follows session readiness."
  `(let* ((,queue (make-ui-queue))
          (,backend (make-acp-backend "/tmp/aui-backend-fixture/" ,queue :launcher ,launcher)))
     (unwind-protect
          (progn
            (backend-start ,backend)
            (backend-open-conversation ,backend)
            ,@body)
       (ignore-errors (backend-stop ,backend))
       (backend-test-events-until
        ,queue (lambda (event)
                 (and (typep event 'connection-event)
                      (eq (connection-event-state event) ':exited))))
       (backend-test-close-servers))))

(-> event-of (list symbol &optional function) t)
(defun event-of (events class &optional (predicate (constantly t)))
  "Return the first event in EVENTS of CLASS satisfying PREDICATE."
  (find-if (lambda (event) (and (typep event class) (funcall predicate event))) events))

(fiveam:test extensions-are-probed-and-catalogs-bootstrap
  (with-extension-fixture (backend queue)
    (let ((events (backend-test-events-until
                   queue (lambda (event) (and (typep event 'jobs-event) (jobs-event-jobs event))))))
      (fiveam:is (not (null events)))
      (fiveam:is (acp-backend-extension-p backend "_autolith/state"))
      (fiveam:is (not (acp-backend-extension-p backend "_autolith/resource")))
      (fiveam:is (event-of events 'session-event (lambda (event) (eq ':ready (session-event-state event)))))
      (let ((state (event-of events 'state-event
                             (lambda (event) (state-snapshot-title (state-event-snapshot event)))))
            (settings (event-of events 'catalog-event (lambda (event) (eq ':settings (catalog-event-kind event)))))
            (models (event-of events 'catalog-event (lambda (event) (eq ':models (catalog-event-kind event)))))
            (operations (event-of events 'catalog-event (lambda (event) (eq ':operations (catalog-event-kind event)))))
            (jobs (event-of events 'jobs-event (lambda (event) (jobs-event-jobs event)))))
        (fiveam:is (string= "Fixture talk" (state-snapshot-title (state-event-snapshot state))))
        (fiveam:is (= 1/4 (state-snapshot-context-fraction (state-event-snapshot state))))
        (fiveam:is (state-snapshot-plan-p (state-event-snapshot state)))
        (fiveam:is (= 2 (length (catalog-event-items settings))))
        (let ((model (find ':model (catalog-event-items settings) :key #'setting-entry-name))
              (immutable (find ':immutable-p (catalog-event-items settings) :key #'setting-entry-name)))
          (fiveam:is (equal '("gpt-5.6-luna" "gpt-6-astra") (setting-entry-options model)))
          (fiveam:is (setting-entry-adjustable-p model))
          (fiveam:is (eq ':durable (setting-entry-scope model)))
          (fiveam:is (not (setting-entry-adjustable-p immutable)))
          (fiveam:is (string= "fixed for the process" (setting-entry-reason immutable)))
          (fiveam:is (null (setting-entry-value immutable))))
        (fiveam:is (model-entry-verified-p (first (catalog-event-items models))))
        (fiveam:is (operation-entry-terminal-owning-p (second (catalog-event-items operations))))
        (let ((job (first (jobs-event-jobs jobs))))
          (fiveam:is (string= "job-7" (job-entry-id job)))
          (fiveam:is (eq ':running (job-entry-state job)))
          (fiveam:is (equal '("a.txt") (job-entry-artifacts job))))))))

(fiveam:test extension-settings-change-and-refresh
  (with-extension-fixture (backend queue)
    (backend-test-events-until queue (lambda (event) (typep event 'jobs-event)))
    (let ((request (backend-set-setting backend ':model "gpt-6-astra")))
      (let ((events (backend-test-events-until
                     queue (lambda (event)
                             (and (typep event 'catalog-event) (eq ':settings (catalog-event-kind event))
                                  (equal "gpt-6-astra"
                                         (setting-entry-value (find ':model (catalog-event-items event)
                                                                    :key #'setting-entry-name))))))))
        (fiveam:is (not (null events)))
        (let ((result (event-of events 'result-event (lambda (event) (equal request (result-event-request-id event))))))
          (fiveam:is (eq ':ok (result-event-outcome result))))))
    (let ((request (backend-set-setting backend ':immutable-p "on")))
      (let ((events (backend-test-events-until
                     queue (lambda (event) (and (typep event 'result-event)
                                                (equal request (result-event-request-id event)))))))
        (let ((result (event-of events 'result-event (lambda (event) (equal request (result-event-request-id event))))))
          (fiveam:is (eq ':condition (result-event-outcome result)))
          (fiveam:is (string= "configuration-error" (result-event-condition-type result))))))))

(fiveam:test extension-invoke-posts-a-notice-and-events-arrive
  (with-extension-fixture (backend queue)
    (backend-test-events-until queue (lambda (event) (typep event 'jobs-event)))
    (let* ((request (backend-invoke backend "/compact" ""))
           (events (backend-test-events-until
                    queue (lambda (event) (and (typep event 'result-event)
                                               (equal request (result-event-request-id event)))))))
      (fiveam:is (not (null events)))
      (fiveam:is (event-of events 'transcript-event
                           (lambda (event) (and (eq ':notice (transcript-event-operation event))
                                                (search "Compacted 3 records"
                                                        (getf (transcript-event-arguments event) ':text))))))
      (fiveam:is (or (event-of events 'transcript-event
                               (lambda (event) (eq ':compaction (getf (transcript-event-arguments event) ':key))))
                     (backend-test-events-until
                      queue (lambda (event) (and (typep event 'transcript-event)
                                                 (eq ':compaction (getf (transcript-event-arguments event) ':key)))))))
      (fiveam:is (equal '("/compact") (extension-test-agent-invoked (first *backend-test-servers*)))))))

(fiveam:test extension-job-actions
  (with-extension-fixture (backend queue)
    (backend-test-events-until queue (lambda (event) (typep event 'jobs-event)))
    (let* ((request (backend-job-action backend "job-7" ':cancel))
           (events (backend-test-events-until
                    queue (lambda (event) (and (typep event 'result-event)
                                               (equal request (result-event-request-id event)))))))
      (fiveam:is (eq ':ok (result-event-outcome
                           (event-of events 'result-event
                                     (lambda (event) (equal request (result-event-request-id event)))))))
      (fiveam:is (equal '("job-7") (extension-test-agent-cancelled (first *backend-test-servers*)))))
    (let* ((request (backend-job-action backend "job-7" ':send :text "hurry"))
           (events (backend-test-events-until
                    queue (lambda (event) (and (typep event 'result-event)
                                               (equal request (result-event-request-id event)))))))
      (fiveam:is (eq ':ok (result-event-outcome
                           (event-of events 'result-event
                                     (lambda (event) (equal request (result-event-request-id event))))))))))

(fiveam:test plain-agents-report-unsupported
  (with-extension-fixture (backend queue :launcher #'backend-test-launcher)
    (fiveam:is (backend-test-events-until
                queue (lambda (event) (and (typep event 'session-event) (eq ':ready (session-event-state event))))))
    (fiveam:is (null (acp-backend--extension-methods backend)))
    (let* ((request (backend-refresh backend ':settings))
           (events (backend-test-events-until
                    queue (lambda (event) (and (typep event 'result-event)
                                               (equal request (result-event-request-id event)))))))
      (fiveam:is (eq ':unsupported (result-event-outcome
                                    (event-of events 'result-event
                                              (lambda (event) (equal request (result-event-request-id event))))))))
    (let* ((request (backend-set-setting backend ':model "x"))
           (events (backend-test-events-until
                    queue (lambda (event) (and (typep event 'result-event)
                                               (equal request (result-event-request-id event)))))))
      (fiveam:is (eq ':unsupported (result-event-outcome
                                    (event-of events 'result-event
                                              (lambda (event) (equal request (result-event-request-id event))))))))))

(fiveam:test decoders-tolerate-missing-and-null-fields
  (let ((setting (json->setting-entry (acp:json-object "name" "model" "value" :null)))
        (job (json->job-entry (acp:json-object "id" "j" "state" "weird")))
        (snapshot (json->state-snapshot (acp:json-object "usage" (acp:json-object)) "/w/")))
    (fiveam:is (eq ':model (setting-entry-name setting)))
    (fiveam:is (eq ':command-line (setting-entry-source (json->setting-entry (acp:json-object "name" "x" "source" "command-line")))))
    (fiveam:is (null (setting-entry-source (json->setting-entry (acp:json-object "name" "x" "source" "not a word!")))))
    (fiveam:is (null (setting-entry-value setting)))
    (fiveam:is (eq ':string (setting-entry-type setting)))
    (fiveam:is (eq ':running (job-entry-state job)))
    (fiveam:is (string= "/w/" (state-snapshot-workspace snapshot)))
    (fiveam:is (null (state-snapshot-context-fraction snapshot)))
    (let ((transcript (json->job-transcript "j" (acp:json-object "content" "hello" "offset" 0
                                                                   "nextOffset" 5 "total" 50
                                                                   "complete" (argo:json-false)))))
      (fiveam:is (string= "hello" (getf transcript ':content)))
      (fiveam:is (= 50 (getf transcript ':total)))
      (fiveam:is (not (getf transcript ':complete-p))))))
