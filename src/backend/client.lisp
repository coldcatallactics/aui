(in-package #:aui)

;;;; -- ACP Event Projection --

(defclass backend-client (agentcomms:acp-client)
  ((backend :initarg :backend :reader backend-client-backend :documentation "The process owner.")
   (generation :initarg :generation :reader backend-client-generation :documentation "Connection incarnation.")
   (segment-kind :initform nil :accessor backend-client--segment-kind :documentation "Contiguous text kind.")
   (segment-id :initform nil :accessor backend-client--segment-id :documentation "Contiguous text block identifier.")
   (tools :initform (make-hash-table :test 'equal) :reader backend-client--tools
          :documentation "Projected calls, keyed by wire call identifier."))
  (:documentation "An ACP client that publishes data events without touching the document."))

(defclass backend-approval ()
  ((condition :initform (bordeaux-threads:make-condition-variable) :reader backend-approval-condition
              :documentation "Wakes the owning permission handler.")
   (options :initarg :options :reader backend-approval-options :documentation "Allowed option identifiers.")
   (decision :initform nil :accessor backend-approval-decision :documentation "Selected id or :CANCELLED."))
  (:documentation "One bounded, exactly-once permission decision."))

(-> acp-data-text (t) string)
(defun acp-data-text (value)
  "Copy a text value or encode structured data for the inspector."
  (cond
    ((null value) "")
    ((stringp value) (copy-seq value))
    (t (agentcomms:json-encode value))))

(-> acp-data-keyword (t list &optional t) t)
(defun acp-data-keyword (value allowed &optional default)
  "Resolve a wire string against ALLOWED, without interning peer-controlled names."
  (or (and (stringp value)
           (find (substitute #\- #\_ value) allowed :key #'symbol-name :test #'string-equal))
      default))

(-> backend-client--current-p (backend-client &optional t) boolean)
(defun backend-client--current-p (client &optional session-id)
  "Whether CLIENT and optional SESSION-ID still belong to the live owner."
  (let ((backend (backend-client-backend client)))
    (and (eq client (acp-backend--client backend))
         (= (backend-client-generation client) (backend-generation backend))
         (or (null session-id) (equal session-id (acp-backend--session backend))))))

(-> backend-client--boundary (backend-client) null)
(defun backend-client--boundary (client)
  "Finish a contiguous text segment before projecting another object."
  (setf (backend-client--segment-kind client) nil
        (backend-client--segment-id client) nil)
  nil)

(-> backend-client--append (backend-client symbol &rest t) ui-event)
(defun backend-client--append (client class &rest initargs)
  "Publish construction data for a new document block."
  (acp-backend--post (backend-client-backend client) 'transcript-event :operation ':append
                     :arguments (list ':class class ':initargs initargs)))

(-> backend-client--text (backend-client symbol string) null)
(defun backend-client--text (client kind text)
  "Append TEXT to one contiguous KIND segment without reordering tool calls."
  (let ((backend (backend-client-backend client)))
    (unless (eq kind (backend-client--segment-kind client))
      (setf (backend-client--segment-kind client) kind
            (backend-client--segment-id client) (acp-backend--identifier backend))
      (backend-client--append client kind :id (backend-client--segment-id client) :text ""))
    (acp-backend--post backend 'transcript-event :operation ':append-text
                       :arguments (list ':id (backend-client--segment-id client) ':text (copy-seq text))))
  nil)

(-> backend-client--call-id (backend-client string) string)
(defun backend-client--call-id (client wire-id)
  "Qualify an ACP call identifier with its owning conversation."
  (format nil "~A/~A" (acp-backend--session (backend-client-backend client)) wire-id))

(-> backend-client--tool-status (t) tool-call-status)
(defun backend-client--tool-status (status)
  "Map the standard ACP status strings to document states."
  (cond
    ((equal status "in_progress") ':running)
    ((equal status "completed") ':completed)
    ((equal status "failed") ':failed)
    (t ':pending)))

(-> backend-client--content-text (t) string)
(defun backend-client--content-text (items)
  "Extract text content from an ACP tool result while preserving item order."
  (format nil "~{~A~^~%~}"
          (loop for item across (coerce items 'vector)
                for content = (agentcomms:json-get item "content")
                when (and content (equal "text" (agentcomms:json-get content "type")))
                  collect (acp-data-text (agentcomms:json-get content "text")))))

(-> backend-client--tool (backend-client hash-table) null)
(defun backend-client--tool (client update)
  "Upsert the standard tool-call fields, retaining the raw wire status."
  (backend-client--boundary client)
  (let* ((backend (backend-client-backend client))
         (wire-id (agentcomms:json-get update "toolCallId"))
         (call-id (and (stringp wire-id) (backend-client--call-id client wire-id)))
         (status (agentcomms:json-get update "status"))
         (title (agentcomms:json-get update "title"))
         (output (multiple-value-bind (value present-p) (gethash "rawOutput" update)
                   (if present-p (acp-data-text value)
                       (and (gethash "content" update)
                            (backend-client--content-text (gethash "content" update)))))))
    (unless call-id
      (error 'backend-state-error :reason "A tool update has no string toolCallId."))
    (cond
      ((gethash wire-id (backend-client--tools client))
       (acp-backend--post backend 'transcript-event :operation ':update-tool-call
                          :arguments (list ':call-id call-id
                                           ':changes (append
                                                      (when status (list ':status (backend-client--tool-status status)
                                                                         ':raw-status (copy-seq status)))
                                                      (when title (list ':title (copy-seq title)))
                                                      (when output (list ':output output))))))
      (t
       (setf (gethash (copy-seq wire-id) (backend-client--tools client)) t)
       (backend-client--append client 'tool-call :id (format nil "tool/~A" call-id)
                               :call-id call-id :name (acp-data-text (or (agentcomms:json-get update "name") title "tool"))
                               :title (acp-data-text title)
                               :kind (acp-data-keyword (agentcomms:json-get update "kind")
                                                      '(:read :edit :delete :move :search :execute :think :fetch :other) ':other)
                               :status (backend-client--tool-status status)
                               :raw-status (and status (copy-seq status))
                               :input (acp-data-text (agentcomms:json-get update "rawInput"))
                               :output output))))
  nil)

(defmethod agentcomms:client-session-update ((client backend-client) session-id update params)
  "Project an ordered ACP notification onto the immutable event queue."
  (declare (ignore params))
  (let* ((backend (backend-client-backend client))
         (*acp-event-generation* (backend-client-generation client))
         (*acp-event-session* session-id))
    (bordeaux-threads:with-lock-held ((acp-backend--projection-lock backend))
      (when (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
              (and (backend-client--current-p client session-id)
                   (eq (acp-backend--state backend) ':ready)
                   (member (acp-backend--session-state backend) '(:loading :ready))))
        (case (agentcomms:acp-update-kind update)
          ((:agent-message-chunk :agent-thought-chunk)
           (let ((content (agentcomms:json-get update "content")))
             (when (equal (agentcomms:json-get content "type") "text")
               (backend-client--text client
                                     (if (eq (agentcomms:acp-update-kind update) ':agent-message-chunk)
                                         'assistant-text 'reasoning)
                                     (agentcomms:acp-content-text content)))))
          (:user-message-chunk
           (backend-client--boundary client)
           (backend-client--append client 'user-message
                                   :text (acp-data-text (agentcomms:acp-content-text
                                                        (agentcomms:json-get update "content")))))
          ((:tool-call :tool-call-update) (backend-client--tool client update))
          (:plan
           (backend-client--boundary client)
           (acp-backend--post backend 'transcript-event :operation ':replace-plan
                              :arguments
                              (list ':entries
                                    (loop for entry across (coerce (agentcomms:json-get update "entries") 'vector)
                                          collect (cons (acp-data-text (agentcomms:json-get entry "content"))
                                                        (acp-data-keyword (agentcomms:json-get entry "status")
                                                                          '(:pending :in-progress :completed) ':pending))))))
          (:current-mode-update
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (setf (acp-backend--mode backend)
                   (acp-data-keyword (agentcomms:json-get update "currentModeId")
                                     '(:ask :auto :sandboxed :full-access))))
           (acp-backend--publish-state backend)))))
    nil))

(-> acp-backend--cancel-approvals (acp-backend) null)
(defun acp-backend--cancel-approvals (backend)
  "Resolve outstanding permissions without waiting on the network or prompt worker."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (maphash (lambda (id approval)
               (declare (ignore id))
               (unless (backend-approval-decision approval)
                 (setf (backend-approval-decision approval) ':cancelled)
                 (bordeaux-threads:condition-notify (backend-approval-condition approval))))
             (acp-backend--approvals backend)))
  nil)

(defmethod backend-decide-approval ((backend acp-backend) request-id option-id)
  "Resolve a current offered option exactly once; reject stale or invalid choices."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (let ((approval (gethash request-id (acp-backend--approvals backend))))
      (when (and approval (null (backend-approval-decision approval))
                 (eq (acp-backend--state backend) ':ready)
                 (eq (acp-backend--turn backend) ':running)
                 (acp-backend--turn-token backend)
                 (member option-id (backend-approval-options approval) :test #'equal))
        (setf (backend-approval-decision approval) (copy-seq option-id))
        (bordeaux-threads:condition-notify (backend-approval-condition approval))
        t))))

(defmethod agentcomms:client-request-permission ((client backend-client) session-id tool-call options params)
  "Wait on the owned handler thread, leaving the connection reader and cancellation free."
  (declare (ignore params))
  (let* ((backend (backend-client-backend client))
         (*acp-event-generation* (backend-client-generation client))
         (*acp-event-session* session-id)
         (request (acp-backend--identifier backend))
         (choices (loop for option in (coerce options 'list)
                        collect (list (copy-seq (agentcomms:json-get option "optionId"))
                                      (acp-data-text (agentcomms:json-get option "name"))
                                      (acp-data-keyword (agentcomms:json-get option "kind")
                                                        '(:allow-once :allow-always :reject-once :reject-always)))))
         (approval (make-instance 'backend-approval :options (mapcar #'first choices)))
         (deadline (+ (get-internal-real-time) (* 120 internal-time-units-per-second)))
         (published-p nil))
    (unwind-protect
         (progn
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (unless (and (backend-client--current-p client session-id)
                          (eq (acp-backend--state backend) ':ready)
                         (eq (acp-backend--turn backend) ':running)
                         (acp-backend--turn-token backend))
               (return-from agentcomms:client-request-permission ':cancelled))
             (setf (gethash request (acp-backend--approvals backend)) approval))
           (bordeaux-threads:with-lock-held ((acp-backend--projection-lock backend))
             (backend-client--boundary client)
             (acp-backend--post backend 'approval-event :request-id request
                                :call-id (backend-client--call-id client (agentcomms:json-get tool-call "toolCallId"))
                                :title (acp-data-text (agentcomms:json-get tool-call "title"))
                                :arguments (acp-data-text (agentcomms:json-get tool-call "rawInput"))
                                :directory (backend-directory backend) :options choices
                                :deadline (+ (get-universal-time) 120))
             (setf published-p t))
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (loop until (backend-approval-decision approval)
                   for remaining = (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)
                   do (when (<= remaining 0)
                        (setf (backend-approval-decision approval) ':cancelled)
                        (return))
                      (bordeaux-threads:condition-wait (backend-approval-condition approval)
                                                      (acp-backend--lock backend) :timeout remaining)))
           (if (stringp (backend-approval-decision approval))
               (values ':selected (backend-approval-decision approval))
               ':cancelled))
      (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
        (remhash request (acp-backend--approvals backend)))
      (when published-p
        (acp-backend--post backend 'transcript-event :operation ':decide-approval
                           :arguments (list ':request-id request
                                            ':decision (or (backend-approval-decision approval) ':cancelled)))))))

(defmethod agentcomms:peer-connection-closed :before ((client backend-client) connection reason)
  "Wake permission handlers before agentcomms joins them during connection closure."
  (declare (ignore connection))
  (let* ((backend (backend-client-backend client))
         (*acp-event-generation* (backend-client-generation client))
         (unexpected-p nil))
    (when (backend-client--current-p client)
      (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
        (unless (member (acp-backend--state backend) '(:closing :exited))
          (setf (acp-backend--state backend) ':lost
                (acp-backend--turn backend) ':interrupted
                unexpected-p t)))
      (acp-backend--cancel-approvals backend)
      (acp-backend--abandon-requests backend)
      (when unexpected-p
        (acp-backend--post backend 'turn-event :state ':interrupted)
        (acp-backend--post backend 'connection-event :state ':lost :detail reason))))
  nil)
