(in-package #:aui)

;;;; -- Autolith Extensions --

;;; The client side of the _autolith/* extension methods: a capabilities
;;; probe after initialize, reads of state, catalogs and jobs on their own
;;; control threads, mutations through the serialized operation queue, and
;;; _autolith/event notifications turned into refreshes. Without the
;;; extensions every operation here reports :UNSUPPORTED and the plain ACP
;;; path carries on.

(defparameter *extension-timeout* 20
  "Seconds an extension request may take before it is reported as a condition.")

(defparameter *extension-catalog-methods*
  '((:state . "_autolith/state")
    (:settings . "_autolith/settings")
    (:models . "_autolith/models")
    (:operations . "_autolith/operations")
    (:jobs . "_autolith/jobs")
    (:conversations . "_autolith/conversations"))
  "The extension method answering each refresh kind.")

(-> acp-backend-extension-p (acp-backend string) boolean)
(defun acp-backend-extension-p (backend method)
  "Return T when the connected Autolith advertised METHOD."
  (and (member method (acp-backend--extension-methods backend) :test #'string=) t))


;;; Wire helpers

(-> json-string (t string) (or null string))
(defun json-string (object key)
  "Return a fresh copy of the string at KEY in OBJECT, or NIL."
  (let ((value (and (agentcomms:json-object-p object) (agentcomms:json-get object key))))
    (and (stringp value) (copy-seq value))))

(-> json-integer (t string) (or null integer))
(defun json-integer (object key)
  "Return the integer at KEY in OBJECT, or NIL for anything else."
  (let ((value (and (agentcomms:json-object-p object) (agentcomms:json-get object key))))
    (and (integerp value) value)))

(-> json-boolean (t string) boolean)
(defun json-boolean (object key)
  "Return T when KEY in OBJECT is true; false, null and absence are NIL."
  (and (agentcomms:json-object-p object) (agentcomms:json-get object key) t))

(-> json-word (t string) (or null keyword))
(defun json-word (object key)
  "Return the short lowercase word at KEY in OBJECT as a keyword, or NIL.
Only letters, digits and hyphens are accepted, at most 32 of them, so a peer
cannot intern arbitrary text."
  (let ((value (json-string object key)))
    (and value
         (<= 1 (length value) 32)
         (every (lambda (character) (or (alphanumericp character) (char= character #\-))) value)
         (intern (string-upcase value) :keyword))))

(-> json-items (t) list)
(defun json-items (object)
  "Return the items array of a catalog value as a list."
  (let ((items (and (agentcomms:json-object-p object) (agentcomms:json-get object "items"))))
    (if (vectorp items) (agentcomms:json-sequence->list items) nil)))

(-> json-plain-value (t) t)
(defun json-plain-value (value)
  "Return VALUE as immutable Lisp data: strings copied, numbers and booleans
as they are, arrays as lists of plain values, and objects as alists of
(KEY . PLAIN-VALUE) ordered by key."
  (cond
    ((stringp value) (copy-seq value))
    ((or (null value) (eq value ':null) (argo:json-false-p value)) nil)
    ((or (numberp value) (eq value t)) value)
    ((vectorp value) (mapcar #'json-plain-value (agentcomms:json-sequence->list value)))
    ((hash-table-p value)
     (let ((pairs nil))
       (maphash (lambda (key item) (push (cons (copy-seq key) (json-plain-value item)) pairs)) value)
       (sort pairs #'string< :key #'first)))
    (t (agentcomms:json-encode value))))

(-> plain-object-field (t string) t)
(defun plain-object-field (object key)
  "Return the value of KEY in OBJECT, an alist from JSON-PLAIN-VALUE, or NIL."
  (and (consp object) (rest (assoc key object :test #'equal))))

(-> arguments->json-object (t) (values (or null hash-table) (or null string)))
(defun arguments->json-object (arguments)
  "Return ARGUMENTS as a JSON object for the wire: an object passes through,
an empty string or NIL becomes an empty object, and other text is decoded as
JSON. The second value is a report when the text is not a JSON object."
  (cond
    ((agentcomms:json-object-p arguments) (values arguments nil))
    ((or (null arguments) (and (stringp arguments) (zerop (length (string-trim " " arguments)))))
     (values (agentcomms:json-object) nil))
    ((stringp arguments)
     (handler-case
         (let ((decoded (agentcomms:json-decode arguments)))
           (if (agentcomms:json-object-p decoded)
               (values decoded nil)
               (values nil "Arguments must be a JSON object, such as {\"path\": \"src\"}.")))
       (error (condition)
         (values nil (format nil "Arguments are not valid JSON: ~A" condition)))))
    (t (values nil "Arguments must be a JSON object or text."))))

(-> setting-name-string (keyword) string)
(defun setting-name-string (name)
  "Return the wire spelling of the setting NAME."
  (string-downcase (symbol-name name)))


;;; Decoding catalog items

(-> json->setting-entry (t) setting-entry)
(defun json->setting-entry (item)
  "Decode one _autolith/settings item."
  (make-instance 'setting-entry
                 :name (intern (string-upcase (or (json-string item "name") "unnamed")) :keyword)
                 :label (or (json-string item "label") (json-string item "name") "")
                 :documentation (or (json-string item "documentation") "")
                 :group (json-string item "group")
                 :type (acp-data-keyword (agentcomms:json-get item "type")
                                         '(:boolean :choice :integer :string :pathname :derived) ':string)
                 :options (let ((options (agentcomms:json-get item "options")))
                            (if (vectorp options) (mapcar #'json-plain-value (agentcomms:json-sequence->list options)) nil))
                 :value (json-plain-value (agentcomms:json-get item "value"))
                 :source (json-word item "source")
                 :scope (acp-data-keyword (agentcomms:json-get item "scope")
                                          '(:process :durable :session :derived))
                 :adjustable-p (json-boolean item "adjustable")
                 :reason (json-string item "reason")))

(-> json->model-entry (t) model-entry)
(defun json->model-entry (item)
  "Decode one _autolith/models item."
  (make-instance 'model-entry
                 :id (or (json-string item "id") "")
                 :provider (json-string item "provider")
                 :credential-p (json-boolean item "credentialStored")
                 :verified-p (json-boolean item "recentlyVerified")
                 :context-window (json-integer item "contextWindow")))

(-> json->operation-entry (t) operation-entry)
(defun json->operation-entry (item)
  "Decode one _autolith/operations item."
  (make-instance 'operation-entry
                 :name (or (json-string item "name") "")
                 :kind (acp-data-keyword (agentcomms:json-get item "kind") '(:command :tool) ':tool)
                 :description (or (json-string item "description") "")
                 :argument-schema (json-plain-value (agentcomms:json-get item "argumentSchema"))
                 :admitted-p (json-boolean item "admitted")
                 :terminal-owning-p (json-boolean item "terminalOwning")))

(-> json->job-entry (t) job-entry)
(defun json->job-entry (item)
  "Decode one _autolith/jobs item."
  (make-instance 'job-entry
                 :id (or (json-string item "id") "")
                 :parent-id (json-string item "parentId")
                 :role (json-string item "role")
                 :state (acp-data-keyword (agentcomms:json-get item "state")
                                          '(:pending :running :waiting :completed :failed :cancelled :abandoned)
                                          ':running)
                 :started-at (json-integer item "startedAt")
                 :elapsed-seconds (json-integer item "elapsedSeconds")
                 :usage (json-plain-value (agentcomms:json-get item "usage"))
                 :artifacts (json-plain-value (agentcomms:json-get item "artifacts"))
                 :activity (json-string item "activity")))

(-> json->conversation-entry (t) conversation-entry)
(defun json->conversation-entry (item)
  "Decode one _autolith/conversations item."
  (make-instance 'conversation-entry
                 :id (or (json-string item "id") "")
                 :display-id (json-string item "displayId")
                 :title (json-string item "title")
                 :directory (or (json-string item "directory") "")
                 :model (json-string item "model")
                 :created-at (json-integer item "createdAt")
                 :last-activity-at (json-integer item "lastActivityAt")
                 :user-turn-count (or (json-integer item "userTurnCount") 0)
                 :preview (json-string item "preview")
                 :live-p (json-boolean item "live")))

(-> json->state-snapshot (t string) state-snapshot)
(defun json->state-snapshot (value workspace)
  "Decode a _autolith/state value; unknown measurements stay NIL."
  (let ((usage (and (agentcomms:json-object-p value) (agentcomms:json-get value "usage"))))
    (make-instance 'state-snapshot
                   :workspace (or (json-string value "workspace") workspace)
                   :title (json-string value "title")
                   :model (json-string value "model")
                   :effort (json-string value "effort")
                   :permission-mode (acp-data-keyword (agentcomms:json-get value "permissionMode")
                                                      '(:ask :auto :sandboxed :full-access))
                   :turn-state (acp-data-keyword (agentcomms:json-get value "turnState")
                                                 '(:idle :running :cancelling :interrupted))
                   :usage (when (agentcomms:json-object-p usage)
                            (append (when (json-integer usage "input") (list ':input (json-integer usage "input")))
                                    (when (json-integer usage "output") (list ':output (json-integer usage "output")))
                                    (when (json-integer usage "cached") (list ':cached (json-integer usage "cached")))
                                    (when (json-integer usage "contextUsed")
                                      (list ':context-used (json-integer usage "contextUsed")))
                                    (when (json-integer usage "contextLimit")
                                      (list ':context-limit (json-integer usage "contextLimit")))
                                    (list ':context-measured-p (json-boolean usage "contextMeasured"))))
                   :live-job-count (json-integer value "liveJobCount")
                   :plan-p (json-boolean value "planPresent")
                   :health (json-string value "health"))))


;;; Calling the server

(-> acp-backend--extension-call (acp-backend string list) (values keyword t (or null string) (or null string)))
(defun acp-backend--extension-call (backend method arguments)
  "Call METHOD with ARGUMENTS, alternating keys and values, on the current
session. Return the outcome keyword, the value, a report and a condition type.
Transport failures become :CONDITION; a missing method becomes :UNSUPPORTED."
  (let ((client (acp-backend--client backend))
        (session (acp-backend--session backend)))
    (handler-case
        (let* ((params (apply #'agentcomms:json-object
                              (append (when session (list "sessionId" session)) arguments)))
               (response (agentcomms:client-agent-request client method params :timeout *extension-timeout*))
               (outcome (acp-data-keyword (agentcomms:json-get response "outcome")
                                          '(:ok :condition :busy :unsupported :cancelled) ':condition)))
          (values outcome
                  (agentcomms:json-get response "value")
                  (or (json-string response "report")
                      (and (not (eq outcome ':ok)) (format nil "~A answered ~(~A~)." method outcome)))
                  (json-string response "conditionType")))
      (agentcomms:acp-remote-error (condition)
        (if (eql (agentcomms:acp-method-error-code condition) (agentcomms:acp-error-code ':method-not-found))
            (values ':unsupported nil (format nil "~A is not provided by this Autolith." method) nil)
            (values ':condition nil (princ-to-string condition) (princ-to-string (type-of condition)))))
      (error (condition)
        (values ':condition nil (princ-to-string condition) (princ-to-string (type-of condition)))))))

(-> acp-backend--probe-extensions (acp-backend) null)
(defun acp-backend--probe-extensions (backend)
  "Ask the connected Autolith which extension methods it implements."
  (multiple-value-bind (outcome value) (acp-backend--extension-call backend "_autolith/capabilities" nil)
    (let ((methods (and (eq outcome ':ok) (agentcomms:json-object-p value)
                        (agentcomms:json-get value "methods"))))
      (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
        (setf (acp-backend--extension-methods backend)
              (if (vectorp methods)
                  (remove-if-not #'stringp (mapcar #'json-plain-value (agentcomms:json-sequence->list methods)))
                  nil)))))
  nil)

(-> acp-backend--control (acp-backend string function) null)
(defun acp-backend--control (backend name function)
  "Run FUNCTION on its own thread so that it never waits behind a prompt."
  (let ((generation (backend-generation backend)))
    (bordeaux-threads:make-thread
     (lambda ()
       (let ((*acp-event-generation* generation))
         (handler-case (funcall function)
           (error (condition)
             (acp-backend--failure backend (acp-backend--identifier backend) condition)))))
     :name name))
  nil)

(-> acp-backend--publish-catalog (acp-backend keyword t) null)
(defun acp-backend--publish-catalog (backend kind value)
  "Post the decoded extension VALUE of KIND as the matching event."
  (ecase kind
    (:state
     (acp-backend--post backend 'state-event
                        :snapshot (json->state-snapshot value (backend-directory backend))))
    (:settings
     (acp-backend--post backend 'catalog-event :kind ':settings
                        :items (mapcar #'json->setting-entry (json-items value))))
    (:models
     (acp-backend--post backend 'catalog-event :kind ':models
                        :items (mapcar #'json->model-entry (json-items value))))
    (:operations
     (acp-backend--post backend 'catalog-event :kind ':operations
                        :items (mapcar #'json->operation-entry (json-items value))))
    (:jobs
     (acp-backend--post backend 'jobs-event :kind ':snapshot
                        :jobs (mapcar #'json->job-entry (json-items value))))
    (:conversations
     (acp-backend--post backend 'catalog-event :kind ':conversations
                        :items (mapcar #'json->conversation-entry (json-items value)))))
  nil)

(-> acp-backend--extension-refresh (acp-backend string keyword) boolean)
(defun acp-backend--extension-refresh (backend request kind)
  "Fetch KIND through its extension method on a control thread and return T,
or return NIL when the server does not provide it."
  (let ((method (rest (assoc kind *extension-catalog-methods*))))
    (when (and method (acp-backend-extension-p backend method))
      (acp-backend--control
       backend "AUI extension refresh"
       (lambda ()
         (multiple-value-bind (outcome value report condition-type)
             (acp-backend--extension-call backend method nil)
           (when (eq outcome ':ok)
             (acp-backend--publish-catalog backend kind value))
           (acp-backend--result backend request outcome :report report :condition-type condition-type))))
      t)))

(-> acp-backend--extension-bootstrap (acp-backend) null)
(defun acp-backend--extension-bootstrap (backend)
  "Fetch every catalog the server provides once a session is ready."
  (dolist (kind '(:state :settings :models :operations :jobs :conversations))
    (acp-backend--extension-refresh backend (acp-backend--identifier backend) kind))
  nil)

(-> acp-backend--extension-mutation (acp-backend string string list &key (:after function)) boolean)
(defun acp-backend--extension-mutation (backend request method arguments &key after)
  "Queue METHOD with ARGUMENTS on the serialized operation path and return T,
or return NIL when the server does not provide it. AFTER is called with the
value on success before the result is posted."
  (when (acp-backend-extension-p backend method)
    (acp-backend--enqueue
     backend request
     (lambda ()
       (multiple-value-bind (outcome value report condition-type)
           (acp-backend--extension-call backend method arguments)
         (when (and (eq outcome ':ok) after)
           (funcall after value))
         (acp-backend--result backend request outcome :value (json-plain-value value)
                                                      :report report :condition-type condition-type))))
    t))

(-> acp-backend--extension-set-setting (acp-backend string keyword t) boolean)
(defun acp-backend--extension-set-setting (backend request name value)
  "Change setting NAME to VALUE through the server, then refresh settings and state."
  (acp-backend--extension-mutation
   backend request "_autolith/set-setting"
   (list "name" (setting-name-string name) "value" value)
   :after (lambda (item)
            (declare (ignore item))
            (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':settings)
            (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':state))))

(-> acp-backend--extension-invoke (acp-backend string string t) boolean)
(defun acp-backend--extension-invoke (backend request operation arguments)
  "Run OPERATION with ARGUMENTS, a JSON object or its text, through the server
and show the result text as a notice. Malformed arguments are reported without
a round trip."
  (multiple-value-bind (object report) (arguments->json-object arguments)
    (cond
      ((not (acp-backend-extension-p backend "_autolith/invoke"))
       nil)
      (report
       (acp-backend--result backend request ':condition :report report :condition-type "invalid-arguments")
       t)
      (t
       (acp-backend--extension-invoke-object backend request operation object)))))

(-> acp-backend--extension-invoke-object (acp-backend string string hash-table) boolean)
(defun acp-backend--extension-invoke-object (backend request operation arguments)
  "Queue the invocation of OPERATION with the JSON object ARGUMENTS."
  (acp-backend--extension-mutation
   backend request "_autolith/invoke"
   (list "operation" operation "arguments" arguments)
   :after (lambda (value)
            (let ((text (json-string value "text")))
              (when (and text (plusp (length text)))
                (acp-backend--post backend 'transcript-event :operation ':notice
                                   :arguments (list ':text (format nil "~A: ~A" operation text))))))))

(-> acp-backend--extension-job-action (acp-backend string string keyword (or null string)) boolean)
(defun acp-backend--extension-job-action (backend request job-id action text)
  "Perform ACTION on JOB-ID through the server: cancel and inspect on the
control path, send on the serialized path."
  (ecase action
    (:send
     (acp-backend--extension-mutation backend request "_autolith/job-send"
                                      (list "jobId" job-id "text" (or text ""))))
    ((:cancel :inspect)
     (let ((method (if (eq action ':cancel) "_autolith/job-cancel" "_autolith/job-transcript")))
       (when (acp-backend-extension-p backend method)
         (acp-backend--control
          backend "AUI job action"
          (lambda ()
            (multiple-value-bind (outcome value report condition-type)
                (acp-backend--extension-call backend method
                                             (if (eq action ':cancel)
                                                 (list "jobId" job-id)
                                                 (list "jobId" job-id "offset" 0 "limit" 20000)))
              (when (and (eq outcome ':ok) (eq action ':cancel))
                (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':jobs))
              (acp-backend--result backend request outcome
                                   :value (if (and (eq outcome ':ok) (eq action ':inspect))
                                              (json->job-transcript job-id value)
                                              (json-plain-value value))
                                   :report report :condition-type condition-type))))
         t)))))

(-> json->job-transcript (string t) list)
(defun json->job-transcript (job-id value)
  "Decode a _autolith/job-transcript value into a plist for the inspector:
:JOB-ID, :CONTENT, :OFFSET, :NEXT-OFFSET, :TOTAL and :COMPLETE-P."
  (list ':job-id job-id
        ':content (or (json-string value "content") "")
        ':offset (json-integer value "offset")
        ':next-offset (json-integer value "nextOffset")
        ':total (json-integer value "total")
        ':complete-p (json-boolean value "complete")))


;;; Server events

(defmethod agentcomms:client-extension-notification ((client backend-client) method params)
  "Turn _autolith/event notifications into refreshes and notices."
  (let ((backend (backend-client-backend client)))
    (when (and (string= method "_autolith/event")
               (backend-client--current-p client)
               (agentcomms:json-object-p params))
      (let ((*acp-event-generation* (backend-client-generation client))
            (kind (json-string params "kind")))
        (cond
          ((equal kind "jobs")
           (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':jobs))
          ((equal kind "state")
           (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':state))
          ((equal kind "compaction")
           (acp-backend--post backend 'transcript-event :operation ':notice
                              :arguments (list ':severity ':information ':key ':compaction
                                               ':text "Autolith compacted the conversation context.")))
          ((equal kind "overflow")
           (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':jobs)
           (acp-backend--extension-refresh backend (acp-backend--identifier backend) ':state))
          (t
           nil)))))
  nil)
