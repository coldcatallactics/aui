(in-package #:aui)

;;;; -- Record Projection --

;;; Turn what a channel says into transcript entries. Durable conversation
;;; records become entries when history is read; the structured client
;;; protocol's live events append to and update them while a turn runs. The
;;; two never show the same thing twice: a live turn is drawn from its text,
;;; reasoning and status events, and of its records only those the events do
;;; not carry are taken.

(defclass projector ()
  ((transcript
    :initarg       :transcript
    :reader        projector-transcript
    :type          transcript
    :documentation "The transcript the projection writes to.")
   (image-root
    :initarg       :image-root
    :initform      nil
    :accessor      projector-image-root
    :type          (or null pathname)
    :documentation "The directory holding the conversation's image artifacts, or NIL.")
   (boundary
    :initform      0
    :accessor      projector-boundary
    :type          integer
    :documentation "The last record sequence history covers; a live record at or below it was already shown.")
   (catch-up-p
    :initform      nil
    :accessor      projector-catch-up-p
    :type          boolean
    :documentation "Whether a provider request was already under way at attachment, so its records are taken instead of its events.")
   (answer
    :initform      nil
    :accessor      projector--answer
    :type          (or null assistant-entry)
    :documentation "The answer now streaming, or NIL.")
   (reasoning
    :initform      nil
    :accessor      projector--reasoning
    :type          (or null reasoning-entry)
    :documentation "The reasoning now streaming, or NIL.")
   (output
    :initform      nil
    :accessor      projector--output
    :type          (or null output-entry)
    :documentation "The command output entry still open for more text, or NIL."))
  (:documentation "The state that turns one channel's records and events into its transcript."))

(-> make-projector (transcript &key (:image-root (or null pathname))) projector)
(defun make-projector (transcript &key image-root)
  "Return a projector writing to TRANSCRIPT, resolving image artifacts below IMAGE-ROOT."
  (make-instance 'projector :transcript transcript :image-root image-root))


;;;; -- JSON --

(-> json->data (t) t)
(defun json->data (value)
  "Return the decoded JSON VALUE as plain Lisp data.

An object becomes an association list of string keys in their written order,
an array a list, false :FALSE and null :NULL; strings, numbers and T stand."
  (cond
    ((autolith::json-object-p value)
     (loop for key being the hash-keys of value using (hash-value member)
           collect (cons key (json->data member))))
    ((autolith::json-false-p value)
     ':false)
    ((stringp value)
     value)
    ((vectorp value)
     (map 'list #'json->data value))
    (t
     value)))

(-> json-arguments (t) list)
(defun json-arguments (source)
  "Return the association list of the tool arguments SOURCE: a JSON object, or its text.

Anything that does not decode to one object yields a single \"arguments\" member
holding the text, so a malformed call still shows what was asked."
  (handler-case
      (let ((decoded (if (stringp source)
                         (if (plusp (length source))
                             (autolith::json-decode source)
                             (autolith::json-object))
                         source)))
        (if (autolith::json-object-p decoded)
            (json->data decoded)
            (list (cons "arguments" (princ-to-string source)))))
    (error ()
      (list (cons "arguments" (princ-to-string source))))))


;;;; -- Records --

(-> record-sequence (list) (or null integer))
(defun record-sequence (record)
  "Return RECORD's durable sequence number, or NIL when it has none."
  (let ((sequence (getf (rest record) :seq)))
    (and (integerp sequence) sequence)))

(-> record--time (list) (or null integer))
(defun record--time (record)
  "Return RECORD's universal time, or NIL when it carries none in that form."
  (let ((time (getf (rest record) :time)))
    (and (integerp time) time)))

(-> record--image-pathnames (projector list) list)
(defun record--image-pathnames (projector descriptors)
  "Return the namestrings of the image artifacts DESCRIPTORS name that exist below PROJECTOR's image root."
  (let ((root (projector-image-root projector)))
    (when root
      (loop for descriptor in descriptors
            for artifact = (and (listp descriptor) (getf descriptor :artifact))
            for pathname = (and (stringp artifact) (merge-pathnames artifact root))
            when (and pathname (probe-file pathname))
              collect (namestring pathname)))))

(-> record--job-notice (list (or null integer)) notice-entry)
(defun record--job-notice (completion time)
  "Return the notice for a background job's COMPLETION, recorded at TIME."
  (let ((outcome (getf completion :outcome))
        (summary (getf completion :summary)))
    (make-instance 'notice-entry
                   :time   time
                   :tone   (if (eq outcome ':failure) ':failure ':quiet)
                   :text   (format nil "Job ~A~@[ · ~A~]"
                                   (case outcome
                                     (:cancellation-pending
                                      "cancellation pending")
                                     (:success
                                      "completed")
                                     (:failure
                                      "failed")
                                     (:cancelled
                                      "cancelled")
                                     (otherwise
                                      "result received"))
                                   (getf completion :job-id))
                   :detail (and (stringp summary) (plusp (length summary)) summary))))

(-> record--request-entry (list (or null integer)) entry)
(defun record--request-entry (request time)
  "Return the entry for an operation REQUEST that started a turn: a channel message, or a notice naming the operation."
  (let ((message (getf request :message)))
    (cond
      ((and message (member (getf message :kind) '(:mention :subscription)))
       (make-instance 'notice-entry
                      :time    time
                      :channel (getf message :target)
                      :text    (format nil "@~A mentioned from ~A"
                                       (or (getf message :sender-name) "participant")
                                       (channel-display-name (getf message :target)))))
      (message
       (make-instance 'user-entry
                      :time   time
                      :text   (or (getf message :text) (getf request :summary) "")
                      :sender (or (getf message :sender-name) "participant")
                      :origin (getf message :target)))
      (t
       (make-instance 'notice-entry
                      :time   time
                      :text   (format nil "~A" (getf request :operation))
                      :detail (getf request :summary))))))

(-> record--continuation-text (string) string)
(defun record--continuation-text (content)
  "Return the words that stand for the automatic continuation prompt CONTENT."
  (cond
    ((string= content autolith::*application-goal-continuation-prompt*)
     "Goal continues")
    ((string= content autolith::*application-job-completion-continuation-prompt*)
     "Continuing with job results")
    (t
     (format nil "Mission wakeup: ~A" content))))

(-> record--message-entry (projector list) (or null entry))
(defun record--message-entry (projector record)
  "Return the entry for the user message RECORD, or NIL for a message with nothing to show."
  (let* ((properties (rest record))
         (content    (getf properties :content))
         (time       (record--time record)))
    (when (eq (getf properties :role) ':user)
      (cond
        ((getf properties :job-completion)
         (record--job-notice (getf properties :job-completion) time))
        ((getf properties :operation-request)
         (record--request-entry (getf properties :operation-request) time))
        ((getf properties :automatic-p)
         (make-instance 'notice-entry
                        :time time
                        :text (record--continuation-text (if (stringp content) content ""))))
        (t
         (make-instance 'user-entry
                        :time   time
                        :text   (if (stringp content) content "")
                        :images (record--image-pathnames projector (getf properties :images))))))))

(-> record--item-entry (list) (or null entry))
(defun record--item-entry (record)
  "Return the entry for the provider item RECORD: an answer, reasoning or a tool call, or NIL."
  (let ((wire-json (getf (rest record) :wire-json))
        (time      (record--time record)))
    (handler-case
        (let* ((item (and (stringp wire-json) (autolith::json-decode wire-json)))
               (type (and (autolith::json-object-p item) (autolith::json-get item "type"))))
          (cond
            ((and (equal type "message") (equal (autolith::json-get item "role") "assistant"))
             (let ((text (autolith::application--visible-assistant-text item)))
               (and text (make-instance 'assistant-entry :time time :text text))))
            ((equal type "reasoning")
             (let ((summary (autolith::response-item-reasoning-summary item)))
               (and summary (make-instance 'reasoning-entry :time time :text summary))))
            ((equal type "function_call")
             (make-instance 'tool-entry
                            :time            time
                            :call-identifier (autolith::json-get item "call_id")
                            :name            (or (autolith::function-call-canonical-name item) "tool")
                            :arguments       (json-arguments (autolith::json-get item "arguments"))))
            ((equal type "web_search_call")
             (make-instance 'tool-entry
                            :time      time
                            :name      "web.search"
                            :status    ':completed
                            :arguments (list (cons "query" (autolith::web-search-call-detail item)))))
            (t
             nil)))
      (error ()
        nil))))

(-> record--seconds (list) (or null real))
(defun record--seconds (properties)
  "Return the wall-clock seconds the tool result PROPERTIES report, or NIL."
  (let ((microseconds (getf properties :real-microseconds)))
    (and (realp microseconds) (/ microseconds 1000000))))

(-> projector--settle-tool
    (projector (or null string) &key (:success-p boolean) (:output t) (:seconds (or null real))) null)
(defun projector--settle-tool (projector call-identifier &key success-p output seconds)
  "Record the result of the tool call CALL-IDENTIFIER in PROJECTOR's transcript."
  (let* ((transcript (projector-transcript projector))
         (entry      (and call-identifier (transcript-find-tool transcript call-identifier))))
    (when entry
      (setf (tool-entry-status entry)  (if success-p ':completed ':failed)
            (tool-entry-output entry)  (and output (princ-to-string output))
            (tool-entry-seconds entry) seconds)
      (transcript-touch transcript entry)))
  nil)

(-> record--side-status-text (list) string)
(defun record--side-status-text (record)
  "Return the text of the side status RECORD's spans."
  (with-output-to-string (stream)
    (dolist (span (getf (rest record) :spans))
      (let ((text (if (consp span) (autolith::terminal-span-text span) span)))
        (when (stringp text)
          (write-string text stream))))))

(-> record->entry (projector list) (or null entry))
(defun record->entry (projector record)
  "Return the entry RECORD adds to a transcript, or NIL when it adds none.

A tool result adds none: it settles the call's own entry in PROJECTOR's
transcript instead."
  (case (first record)
    (:message
     (record--message-entry projector record))
    (:provider-item
     (record--item-entry record))
    (:tool-result
     (let ((properties (rest record)))
       (projector--settle-tool projector (getf properties :call-id)
                               :success-p (and (member (getf properties :status) '(:ok :success)) t)
                               :output    (getf properties :output)
                               :seconds   (record--seconds properties)))
     nil)
    (:side-status
     (make-instance 'output-entry
                    :time (record--time record)
                    :text (string-right-trim '(#\Newline #\Space) (record--side-status-text record))))
    (:assistant-yield
     (let ((count (getf (rest record) :job-count)))
       (when (and (integerp count) (plusp count))
         (make-instance 'notice-entry
                        :time (record--time record)
                        :text (format nil "Yielded: ~D job~:P will wake the agent" count)))))
    ((:summary :native-compaction)
     (make-instance 'marker-entry :time (record--time record) :label "Context compacted"))
    (:turn-aborted
     (make-instance 'notice-entry
                    :time   (record--time record)
                    :tone   ':failure
                    :text   "The turn stopped"
                    :detail (let ((message (getf (rest record) :message)))
                              (and (stringp message) message))))
    (otherwise
     nil)))

(-> records->entries (projector list) list)
(defun records->entries (projector records)
  "Return the entries of RECORDS, oldest first, for a page of history.

Tool results settle calls among the returned entries, which are not in the
transcript yet, so the page is self-contained."
  (let ((entries nil)
        (calls   (make-hash-table :test #'equal)))
    (dolist (record records)
      (if (eq (first record) ':tool-result)
          (let* ((properties (rest record))
                 (entry      (gethash (getf properties :call-id) calls)))
            (when entry
              (setf (tool-entry-status entry)  (if (member (getf properties :status) '(:ok :success))
                                                   ':completed
                                                   ':failed)
                    (tool-entry-output entry)  (let ((output (getf properties :output)))
                                                 (and output (princ-to-string output)))
                    (tool-entry-seconds entry) (record--seconds properties))))
          (let ((entry (record->entry projector record)))
            (when entry
              (when (and (typep entry 'tool-entry) (tool-entry-call-identifier entry))
                (setf (gethash (tool-entry-call-identifier entry) calls) entry))
              (push entry entries)))))
    (nreverse entries)))


;;;; -- Live Events --

(-> projector--close-stream (projector) null)
(defun projector--close-stream (projector)
  "Mark the streaming answer and reasoning of PROJECTOR finished and forget them."
  (let ((transcript (projector-transcript projector))
        (answer     (projector--answer projector))
        (reasoning  (projector--reasoning projector)))
    (when answer
      (setf (assistant-entry-streaming-p answer) nil)
      (transcript-touch transcript answer))
    (when reasoning
      (setf (reasoning-entry-streaming-p reasoning) nil)
      (transcript-touch transcript reasoning))
    (setf (projector--answer projector)    nil
          (projector--reasoning projector) nil))
  nil)

(-> projector--append (projector entry) entry)
(defun projector--append (projector entry)
  "Append ENTRY to PROJECTOR's transcript, ending any open command output, and return it."
  (unless (typep entry 'output-entry)
    (setf (projector--output projector) nil))
  (transcript-append (projector-transcript projector) entry))

(-> projector--text (projector string) null)
(defun projector--text (projector delta)
  "Append the streamed answer text DELTA."
  (when (plusp (length delta))
    (let ((answer (projector--answer projector)))
      (unless answer
        (let ((reasoning (projector--reasoning projector)))
          (when reasoning
            (setf (reasoning-entry-streaming-p reasoning) nil
                  (projector--reasoning projector)        nil)
            (transcript-touch (projector-transcript projector) reasoning)))
        (setf answer                         (projector--append projector
                                                                (make-instance 'assistant-entry :streaming-p t))
              (projector--answer projector) answer))
      (setf (assistant-entry-text answer)
            (concatenate 'string (assistant-entry-text answer) delta))
      (transcript-touch (projector-transcript projector) answer)))
  nil)

(-> projector--reasoning-text (projector string) null)
(defun projector--reasoning-text (projector delta)
  "Append the streamed reasoning text DELTA."
  (when (plusp (length delta))
    (let ((reasoning (projector--reasoning projector)))
      (unless reasoning
        (setf reasoning                         (projector--append projector
                                                                   (make-instance 'reasoning-entry :streaming-p t))
              (projector--reasoning projector) reasoning))
      (setf (reasoning-entry-text reasoning)
            (concatenate 'string (reasoning-entry-text reasoning) delta))
      (transcript-touch (projector-transcript projector) reasoning)))
  nil)

(-> projector--persisted-answer (projector t) null)
(defun projector--persisted-answer (projector text)
  "Complete the streamed answer with its persisted TEXT, which a provider that does not stream sends whole."
  (when (and (stringp text) (plusp (length text)))
    (let ((answer (projector--answer projector)))
      (cond
        ((null answer)
         (projector--append projector (make-instance 'assistant-entry :text text)))
        ((string/= (assistant-entry-text answer) text)
         (setf (assistant-entry-text answer) text)
         (transcript-touch (projector-transcript projector) answer)))))
  (projector--close-stream projector)
  nil)

(-> projector--status (projector t list) null)
(defun projector--status (projector status details)
  "Apply the provider and tool lifecycle STATUS with its DETAILS."
  (case status
    (:provider-request-started
     (setf (projector-catch-up-p projector) nil)
     (projector--close-stream projector))
    (:assistant-response-persisted
     (projector--persisted-answer projector (getf details :text)))
    (:tool-call-started
     (projector--close-stream projector)
     (projector--append projector
                        (make-instance 'tool-entry
                                       :call-identifier (getf details :call-id)
                                       :name            (or (getf details :tool) "tool")
                                       :status          ':running
                                       :arguments       (json-arguments (or (getf details :arguments) "")))))
    (:tool-call-completed
     (projector--settle-tool projector (getf details :call-id)
                             :success-p (and (getf details :success-p) t)
                             :output    (getf details :output)
                             :seconds   (record--seconds details)))
    (:provider-request-completed
     (projector--close-stream projector))
    (:compaction-completed
     (projector--append projector (make-instance 'marker-entry :label "Context compacted")))
    (otherwise
     nil))
  nil)

(-> projector--present (projector t) null)
(defun projector--present (projector text)
  "Show the command or Lisp output TEXT, joining what one command prints in several parts.

Autolith echoes a local Lisp form as a line starting with an asterisk before
its output; that line becomes the entry's invocation."
  (when (stringp text)
    (let* ((trimmed (string-trim '(#\Newline) text))
           (echo-p  (and (> (length trimmed) 2)
                         (string= "* " trimmed :end2 2)
                         (null (position #\Newline trimmed))))
           (open    (projector--output projector)))
      (cond
        ((zerop (length (string-trim '(#\Newline #\Space) trimmed)))
         nil)
        (echo-p
         (setf (projector--output projector)
               (projector--append projector
                                  (make-instance 'output-entry :invocation (subseq trimmed 2)))))
        (open
         (setf (output-entry-text open)
               (if (plusp (length (output-entry-text open)))
                   (format nil "~A~%~A" (output-entry-text open) trimmed)
                   trimmed))
         (transcript-touch (projector-transcript projector) open))
        (t
         (setf (projector--output projector)
               (projector--append projector (make-instance 'output-entry :text trimmed)))))))
  nil)

(-> projector--live-record (projector list) null)
(defun projector--live-record (projector record)
  "Take the live RECORD when history did not show it and the turn's events do not carry it."
  (let ((sequence (record-sequence record)))
    (when (or (null sequence) (> sequence (projector-boundary projector)))
      (when (or (projector-catch-up-p projector)
                (not (member (first record) '(:provider-item :tool-result))))
        (let ((entry (record->entry projector record)))
          (when entry
            (projector--close-stream projector)
            (projector--append projector entry))))))
  nil)

(-> projector-apply (projector list) null)
(defun projector-apply (projector event)
  "Apply one event of the structured client protocol to PROJECTOR's transcript.

Events that say nothing about the transcript, such as approvals and state,
are left to the caller."
  (let ((fields (rest event)))
    (case (first event)
      (:text
       (unless (projector-catch-up-p projector)
         (projector--text projector (or (getf fields :delta) ""))))
      (:reasoning
       (unless (projector-catch-up-p projector)
         (projector--reasoning-text projector (or (getf fields :delta) ""))))
      (:status
       (let ((status (getf fields :status)))
         (unless (and (projector-catch-up-p projector)
                      (member status '(:assistant-response-persisted :tool-call-started
                                       :tool-call-completed)))
           (projector--status projector status (getf fields :details)))))
      (:present
       (projector--present projector (getf fields :text)))
      (:record
       (projector--live-record projector (getf fields :record)))
      (:work-started
       (setf (projector-catch-up-p projector) nil
             (projector--output projector)    nil))
      (:work-finished
       (projector--close-stream projector)
       (setf (projector--output projector) nil))
      (otherwise
       nil)))
  nil)

(-> channel-display-name (t) string)
(defun channel-display-name (identifier)
  "Return the channel IDENTIFIER as people read it, with its visual hyphen."
  (if (stringp identifier)
      (autolith::conversation-identifier-display identifier)
      "?"))
