(in-package #:aui)

;;;; -- Session and Turn Admission --

(-> acp-backend--close-transport (acp-backend) null)
(defun acp-backend--close-transport (backend)
  "Close the attached connection without racing its initial attachment."
  (bordeaux-threads:with-lock-held ((acp-backend--transport-lock backend))
    (let* ((client (acp-backend--client backend))
           (connection (and client (agentcomms:acp-client-connection client))))
      (cond
        (connection
         (agentcomms:connection-close connection))
        ((acp-backend--channel backend)
         (agentcomms:channel-close (acp-backend--channel backend))))))
  nil)

(-> acp-backend--connect (acp-backend) null)
(defun acp-backend--connect (backend)
  "Initialize the owned connection; shutdown owns it as soon as it is attached."
  (let ((channel nil)
        (attached-p nil))
    (handler-case
        (unwind-protect
             (progn
               (setf channel (funcall (acp-backend--launcher backend) backend))
               (bordeaux-threads:with-lock-held ((acp-backend--transport-lock backend))
                 (let ((client (make-instance 'backend-client :backend backend
                                             :generation (backend-generation backend))))
                   (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
                     (unless (eq (acp-backend--state backend) ':starting)
                       (return-from acp-backend--connect nil))
                     (setf (acp-backend--channel backend) channel
                           (acp-backend--client backend) client
                           attached-p t))
                   (agentcomms:acp-client-connect client channel :request-timeout 30)))
               (agentcomms:client-initialize (acp-backend--client backend))
               (acp-backend--probe-extensions backend)
               (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
                 (when (eq (acp-backend--state backend) ':starting)
                   (setf (acp-backend--state backend) ':ready)
                   (acp-backend--post backend 'connection-event :state ':ready)))
               (acp-backend--publish-state backend))
          (when (and channel (not attached-p))
            (agentcomms:channel-close channel)))
      (error (condition)
        (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
          (unless (member (acp-backend--state backend) '(:closing :exited :lost))
            (setf (acp-backend--state backend) ':lost)
            (acp-backend--post backend 'connection-event :state ':lost
                               :detail "ACP startup failed.")))
        (acp-backend--close-transport backend)
        (acp-backend--abandon-requests backend)
        (error condition))))
  nil)

(defmethod backend-start ((backend acp-backend))
  "Start an operation owner and enqueue ACP negotiation ahead of session requests."
  (let ((request (acp-backend--identifier backend)))
    (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
      (unless (eq (acp-backend--state backend) ':new)
        (return-from backend-start nil))
      (setf (acp-backend--state backend) ':starting
            (acp-backend--requests backend) (list (list request (lambda () (acp-backend--connect backend)))))
      (acp-backend--post backend 'connection-event :state ':starting)
      (setf (acp-backend--worker backend)
            (bordeaux-threads:make-thread
             (lambda () (acp-backend--worker-loop backend (backend-generation backend)))
             :name "AUI ACP operations"))))
  nil)

(-> acp-backend--shutdown (acp-backend integer) null)
(defun acp-backend--shutdown (backend generation)
  "Close transport and join the operation owner before publishing exit."
  (let ((*acp-event-generation* generation))
    (handler-case (acp-backend--close-transport backend)
      (error (condition)
        (acp-backend--failure backend (acp-backend--identifier backend) condition)))
    (let ((worker (acp-backend--worker backend)))
      (when worker (bordeaux-threads:join-thread worker)))
    (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
      (setf (acp-backend--state backend) ':exited
            (acp-backend--session-state backend) ':closed
            (acp-backend--session backend) nil
            (acp-backend--turn backend) ':interrupted)
      (acp-backend--post backend 'connection-event :state ':exited)))
  nil)

(defmethod backend-stop ((backend acp-backend))
  "Stop admission, release permissions and asynchronously join owned workers."
  (let ((stop-p
          (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
            (unless (member (acp-backend--state backend) '(:closing :exited))
              (setf (acp-backend--state backend) ':closing)
              (acp-backend--post backend 'connection-event :state ':closing)
              t))))
    (when stop-p
      (acp-backend--cancel-approvals backend)
      (acp-backend--abandon-requests backend)
      (let ((generation (backend-generation backend)))
        (setf (acp-backend--shutdown-worker backend)
              (bordeaux-threads:make-thread
               (lambda () (acp-backend--shutdown backend generation))
               :name "AUI ACP shutdown")))))
  nil)

(defmethod backend-join ((backend acp-backend) &key timeout)
  "Join the shutdown owner, observing TIMEOUT without terminating its cleanup."
  (let ((thread (acp-backend--shutdown-worker backend))
        (deadline (and timeout (+ (get-internal-real-time)
                                  (round (* timeout internal-time-units-per-second))))))
    (unless thread
      (return-from backend-join (eq (acp-backend--state backend) ':exited)))
    (when (eq thread (bordeaux-threads:current-thread))
      (error 'backend-state-error :reason "A shutdown worker cannot join itself."))
    (when deadline
      (loop while (bordeaux-threads:thread-alive-p thread)
            for remaining = (/ (- deadline (get-internal-real-time))
                               internal-time-units-per-second)
            do (when (<= remaining 0)
                 (return-from backend-join nil))
               (sleep (min remaining 0.01))))
    (bordeaux-threads:join-thread thread)
    (eq (acp-backend--state backend) ':exited)))

(-> acp-backend--session-event (acp-backend symbol) ui-event)
(defun acp-backend--session-event (backend state)
  "Publish a session transition while the caller controls its lifecycle."
  (acp-backend--post backend 'session-event :state state
                     :conversation-id (acp-backend--session backend)
                     :directory (backend-directory backend)))

(-> acp-backend--open (acp-backend t) null)
(defun acp-backend--open (backend identifier)
  "Close the old session and bracket replay, retaining ownership if close fails."
  (acp-backend--require-ready backend)
  (let ((client (acp-backend--client backend))
        (opened-p nil))
    (handler-case
        (when (acp-backend--session backend)
          (agentcomms:client-close-session client (acp-backend--session backend)))
      (error (condition)
        (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
          (setf (acp-backend--session-state backend) ':ready)
          (acp-backend--session-event backend ':ready))
        (error condition)))
    (bordeaux-threads:with-lock-held ((acp-backend--projection-lock backend))
      (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
        (setf (acp-backend--session backend) identifier
              (acp-backend--loaded-p backend) (not (null identifier)))
        (acp-backend--session-event backend ':loading))
      (backend-client--boundary client)
      (clrhash (backend-client--tools client))
      (acp-backend--post backend 'transcript-event :operation ':reset)
      (acp-backend--post backend 'transcript-event :operation ':begin-replay))
    (unwind-protect
         (multiple-value-bind (session response)
             (if identifier
                 (values identifier (agentcomms:client-load-session client identifier
                                                                   (backend-directory backend)))
                 (agentcomms:client-new-session client (backend-directory backend)))
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (acp-backend--require-ready backend)
             (setf (acp-backend--session backend) session
                   (acp-backend--mode backend)
                   (acp-data-keyword
                    (agentcomms:json-get (agentcomms:json-get response "modes") "currentModeId")
                    '(:ask :auto :sandboxed :full-access))
                   opened-p t)))
      (bordeaux-threads:with-lock-held ((acp-backend--projection-lock backend))
        (acp-backend--post backend 'transcript-event :operation ':end-replay)
        (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
          (setf (acp-backend--session-state backend) (if opened-p ':ready ':closed))
          (unless opened-p
            (setf (acp-backend--session backend) nil))
          (acp-backend--session-event backend (if opened-p ':ready ':closed)))))
    (acp-backend--publish-state backend)
    (when opened-p
      (acp-backend--extension-bootstrap backend)))
  nil)

(defmethod backend-open-conversation ((backend acp-backend) &key conversation-id)
  "Reserve a session switch and cancel the previous prompt before its queued close."
  (let* ((request (acp-backend--identifier backend))
         (identifier (and conversation-id (copy-seq conversation-id)))
         (admitted
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (when (and (member (acp-backend--state backend) '(:starting :ready))
                        (not (eq (acp-backend--session-state backend) ':loading)))
               (setf (acp-backend--session-state backend) ':loading)
               (acp-backend--queue-request
                backend request
                (lambda ()
                  (acp-backend--open backend identifier)
                  (acp-backend--result backend request ':ok)))
               t))))
    (if admitted
        (backend-cancel backend)
        (acp-backend--result backend request ':busy :report "A session switch is unavailable."))
    request))

(-> acp-backend--send-cancel (acp-backend) null)
(defun acp-backend--send-cancel (backend)
  "Start one cancellation sender under the state lock: after prompt
transmission for a local prompt, or at once for a turn the server owns."
  (when (and (or (acp-backend--prompt-wire-id backend) (acp-backend--external-turn backend))
             (null (acp-backend--cancel-worker backend)))
    (let ((client (acp-backend--client backend))
          (session (acp-backend--session backend))
          (wire-id (acp-backend--prompt-wire-id backend))
          (generation (backend-generation backend)))
      (setf (acp-backend--cancel-worker backend)
            (bordeaux-threads:make-thread
             (lambda ()
               (let ((*acp-event-generation* generation))
                 (handler-case
                     (progn
                       (when wire-id
                         (agentcomms:connection-cancel-request
                          (agentcomms:acp-client-connection client) wire-id))
                       (agentcomms:client-cancel client session))
                   (error (condition)
                     (acp-backend--failure backend (acp-backend--identifier backend) condition)))))
             :name "AUI ACP cancellation"))))
  nil)

(-> acp-backend--prompt-sent (acp-backend t) null)
(defun acp-backend--prompt-sent (backend wire-id)
  "Record transmission and honor a cancellation that preceded it."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (setf (acp-backend--prompt-wire-id backend) wire-id)
    (when (eq (acp-backend--turn backend) ':cancelling)
      (acp-backend--send-cancel backend)))
  nil)

(-> acp-backend--finish-turn (acp-backend t) null)
(defun acp-backend--finish-turn (backend reason)
  "Join the cancellation sender before another prompt or session may be admitted."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (setf (acp-backend--turn-token backend) nil))
  (acp-backend--cancel-approvals backend)
  (let ((sender
          (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
            (prog1 (acp-backend--cancel-worker backend)
              (setf (acp-backend--prompt-wire-id backend) nil)))))
    (when sender (bordeaux-threads:join-thread sender)))
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (setf (acp-backend--cancel-worker backend) nil
          (acp-backend--external-turn backend) nil
          (acp-backend--turn backend)
          (if (eq (acp-backend--state backend) ':ready) ':idle ':interrupted))
    (acp-backend--post backend 'turn-event :state (acp-backend--turn backend)
                       :stop-reason reason))
  (acp-backend--publish-state backend)
  nil)

(-> acp-backend--prompt (acp-backend string string) null)
(defun acp-backend--prompt (backend request text)
  "Run an admitted prompt, or suppress one cancelled before it reached the worker."
  (let ((client (acp-backend--client backend))
        (reason ':error))
    (unwind-protect
         (handler-case
             (progn
               (acp-backend--require-ready backend)
               (if (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
                     (eq (acp-backend--turn backend) ':cancelling))
                   (setf reason ':cancelled)
                   (progn
                     (bordeaux-threads:with-lock-held ((acp-backend--projection-lock backend))
                       (backend-client--boundary client)
                       (backend-client--append client 'user-message :id request :text text))
                     (setf reason
                           (agentcomms:client-prompt
                            client (acp-backend--session backend)
                            (list (agentcomms:acp-text-content text)) :timeout nil
                            :on-sent (lambda (wire-id) (acp-backend--prompt-sent backend wire-id))))))
               (acp-backend--result backend request (if (eq reason ':cancelled) ':cancelled ':ok)
                                    :value reason))
           (error (condition)
             (if (eq (acp-backend--turn backend) ':cancelling)
                 (progn
                   (setf reason ':cancelled)
                   (acp-backend--result backend request ':cancelled))
                 (acp-backend--failure backend request condition))))
      (acp-backend--finish-turn backend reason)))
  nil)

(defmethod backend-prompt ((backend acp-backend) text &key attachments)
  "Reserve and enqueue one text prompt atomically against switches and shutdown."
  (let* ((request (acp-backend--identifier backend))
         (text (copy-seq text))
         (admitted
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (when (and (null attachments) (eq (acp-backend--state backend) ':ready)
                        (eq (acp-backend--session-state backend) ':ready)
                        (eq (acp-backend--turn backend) ':idle))
               (setf (acp-backend--turn backend) ':running
                     (acp-backend--turn-token backend) request)
               (acp-backend--post backend 'turn-event :state ':running)
               (acp-backend--queue-request backend request
                                           (lambda () (acp-backend--prompt backend request text)))
               t))))
    (cond
      (attachments
       (acp-backend--result backend request ':unsupported :report "This ACP backend accepts text prompts only."))
      ((not admitted)
       (acp-backend--result backend request ':busy :report "Wait for an open idle session before sending.")))
    request))

(defmethod backend-cancel ((backend acp-backend))
  "Record cancellation immediately and send it only after the matching prompt."
  (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
    (when (eq (acp-backend--turn backend) ':running)
      (setf (acp-backend--turn backend) ':cancelling)
      (acp-backend--post backend 'turn-event :state ':cancelling)
      (acp-backend--send-cancel backend)))
  (acp-backend--cancel-approvals backend)
  nil)

(-> acp-backend--set-mode (acp-backend string string) null)
(defun acp-backend--set-mode (backend request mode)
  "Apply an admitted mode change before subsequent queued prompts."
  (let ((session (acp-backend--session backend)))
    (acp-backend--require-ready backend)
    (agentcomms:client-set-mode (acp-backend--client backend) session mode)
    (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
      (setf (acp-backend--mode backend)
            (acp-data-keyword mode '(:ask :auto :sandboxed :full-access))))
    (acp-backend--publish-state backend)
    (acp-backend--result backend request ':ok :value mode))
  nil)

(defmethod backend-set-mode ((backend acp-backend) mode-id)
  "Change the permission mode: queued behind prompts when the session is idle,
sent at once on the control path while a turn runs, since ACP admits a mode
change at any time and the user asks for it mid-turn when approvals pile up."
  (let* ((request (acp-backend--identifier backend))
         (mode (copy-seq mode-id))
         (admitted
           (bordeaux-threads:with-lock-held ((acp-backend--lock backend))
             (when (and (eq (acp-backend--state backend) ':ready)
                        (eq (acp-backend--session-state backend) ':ready))
               (if (eq (acp-backend--turn backend) ':idle)
                   (acp-backend--queue-request
                    backend request (lambda () (acp-backend--set-mode backend request mode)))
                   (acp-backend--control backend "AUI set mode"
                                         (lambda () (acp-backend--set-mode backend request mode))))
               t))))
    (unless admitted
      (acp-backend--result backend request ':busy :report "Change permission mode in an open session."))
    request))

(defmethod backend-refresh ((backend acp-backend) kind)
  "Fetch KIND through an extension when the server provides it, else return
confirmed ACP state or report an unavailable runtime catalog."
  (let ((request (acp-backend--identifier backend)))
    (cond
      ((acp-backend--extension-refresh backend request kind)
       request)
      ((eq kind ':state)
       (acp-backend--publish-state backend)
       (acp-backend--result backend request ':ok)
       request)
      ((eq kind ':conversations)
       (acp-backend--enqueue backend request
                            (lambda ()
                              (acp-backend--post backend 'catalog-event :kind ':conversations
                                                 :items (recent-conversations))
                              (acp-backend--result backend request ':ok)))
       request)
      (t
       (acp-backend--result backend request ':unsupported
                            :report (format nil "~(~A~) requires Autolith extension support." kind))
       request))))

(defmethod backend-set-setting ((backend acp-backend) name value)
  "Change a setting through the extension, or report that plain ACP cannot."
  (let ((request (acp-backend--identifier backend)))
    (unless (acp-backend--extension-set-setting backend request name value)
      (acp-backend--result backend request ':unsupported
                           :report "Settings require Autolith extension support."))
    request))

(defmethod backend-invoke ((backend acp-backend) operation arguments)
  "Invoke an operation through the extension, or report that plain ACP cannot."
  (let ((request (acp-backend--identifier backend)))
    (unless (acp-backend--extension-invoke backend request operation arguments)
      (acp-backend--result backend request ':unsupported
                           :report "Operation invocation requires Autolith extension support."))
    request))

(defmethod backend-job-action ((backend acp-backend) job-id action &key text)
  "Act on a job through the extension, or report that plain ACP cannot."
  (let ((request (acp-backend--identifier backend)))
    (unless (acp-backend--extension-job-action backend request job-id action text)
      (acp-backend--result backend request ':unsupported
                           :report "Job control requires Autolith extension support."))
    request))

(setf *backend-factory* #'make-acp-backend)
