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
    :documentation "The command output entry still open for more text, or NIL.")
   (streamed
    :initform      0
    :accessor      projector--streamed
    :type          integer
    :documentation "How many characters of the answer to the provider request under way have streamed, whatever entries they went to.")
   (unsettled
    :initform      nil
    :accessor      projector--unsettled
    :type          list
    :documentation "The tool results on the page of history read last whose calls stand on the page before it.")
   (invocation
    :initform      nil
    :accessor      projector-invocation
    :type          (or null string)
    :documentation "The command the person just sent, which the output that follows is labelled with, or NIL.")
   (channel
    :initform      nil
    :accessor      projector-channel
    :type          (or null string)
    :documentation "The identifier of the channel whose records these are, once known."))
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

(defparameter *compaction-label* "context compacted"
  "What marks the place where the conversation's context was compacted.")

(defparameter *goal-statuses* '(:active :paused :complete :verified :blocked :exhausted :cancelled :failed)
  "What a channel's goal can be said to be while the channel has one.")

(-> goal--create (t t &key (:kind t) (:created-at t)) list)
(defun goal--create (objective status &key kind created-at)
  "Return the goal with OBJECTIVE that is STATUS as a property list of :OBJECTIVE, :STATUS, :KIND and :CREATED-AT, or NIL when the two do not state a goal.

STATUS is one of the goal statuses, as a keyword or spelled out. :KIND is
:MISSION when KIND says so and :GOAL otherwise."
  (let ((status (and (or (stringp status) (symbolp status))
                     (find status *goal-statuses* :test #'string-equal))))
    (and status
         (stringp objective)
         (plusp (length objective))
         (list :objective  objective
               :status     status
               :kind       (if (and (or (stringp kind) (symbolp kind)) (string-equal kind "mission"))
                               ':mission
                               ':goal)
               :created-at (and (integerp created-at) created-at)))))

(-> record-goal (list) list)
(defun record-goal (record)
  "Return the goal the durable goal RECORD states, or NIL when it records that the channel's goal was cleared."
  (let ((properties (rest record)))
    (goal--create (getf properties :objective) (getf properties :status)
                  :kind       (getf properties :kind)
                  :created-at (getf properties :created-at))))

(-> portable-goal (t) list)
(defun portable-goal (object)
  "Return the goal the portable OBJECT of a channel's state names, or NIL when it names none."
  (goal--create (portable-get object "objective") (portable-get object "status")
                :kind       (portable-get object "kind")
                :created-at (portable-get object "createdAt")))

(-> records-newest-goal (list list) list)
(defun records-newest-goal (records known)
  "Return the newest goal record among RECORDS and KNOWN, a goal record or NIL: the one with the highest sequence."
  (dolist (record records known)
    (when (and (eq (first record) ':goal)
               (or (null known)
                   (>= (or (record-sequence record) 0) (or (record-sequence known) 0))))
      (setf known record))))

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
                   :text   (format nil "job ~A~@[ · ~A~]"
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

(-> record--request-entry (list (or null integer) (or null string)) entry)
(defun record--request-entry (request time channel)
  "Return the entry for an operation REQUEST that started a turn: a channel message, or a notice naming the operation.

A message keeps the channel its sender wrote from, unless that is CHANNEL,
the one the request arrived in."
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
                      :origin (let ((origin (or (getf message :source) (getf message :target))))
                                (and (stringp origin) (not (equal origin channel)) origin))))
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
     "goal continues")
    ((string= content autolith::*application-job-completion-continuation-prompt*)
     "continuing with job results")
    (t
     (format nil "mission wakeup: ~A" content))))

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
         (record--request-entry (getf properties :operation-request) time (projector-channel projector)))
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
            ((equal type "tool_search_output")
             (let ((names (autolith::application--tool-search-namespaces item)))
               (and names (make-instance 'schemas-entry :time time :names names))))
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

(-> record--schemas-entry (list) (or null entry))
(defun record--schemas-entry (record)
  "Return the entry of RECORD when it is the provider item that reports a lookup of tool schemas, which no live event carries, else NIL."
  (let ((wire-json (getf (rest record) :wire-json)))
    (and (eq (first record) ':provider-item)
         (stringp wire-json)
         ;; Most items are something else; only one that names the type is decoded.
         (search "tool_search_output" wire-json)
         (let ((entry (record--item-entry record)))
           (and (typep entry 'schemas-entry) entry)))))

(-> record--join-schemas (entry (or null entry)) boolean)
(defun record--join-schemas (entry previous)
  "Add the names of the schema lookup ENTRY to PREVIOUS when both are lookups, and return whether that was done.

Lookups that follow one another read as one, as in the terminal."
  (when (and (typep entry 'schemas-entry) (typep previous 'schemas-entry))
    (setf (schemas-entry-names previous)
          (remove-duplicates (append (schemas-entry-names previous) (schemas-entry-names entry))
                             :test     #'string=
                             :from-end t))
    t))

(-> record--seconds (list) (or null real))
(defun record--seconds (properties)
  "Return the wall-clock seconds the tool result PROPERTIES report, or NIL."
  (let ((microseconds (getf properties :real-microseconds)))
    (and (realp microseconds) (/ microseconds 1000000))))

(-> tool-result-sections (string string) (values list (or null string)))
(defun tool-result-sections (name output)
  "Return what the result OUTPUT of the tool NAME shows a person, and the status line it opens with.

The first value is a list of (LABEL . TEXT). A shell command's result is
split by the reader Autolith's terminal uses: one section for each capture
that holds lines, without the byte counts and log references kept for the
model, then a section for anything it noted, such as a timeout. The second
value is its exit line. Any other tool's result is one section and no status."
  (let ((parts (and (string= name "shell.run")
                    (fboundp 'autolith::application--shell-result-parts)
                    (handler-case (autolith::application--shell-result-parts output)
                      (error ()
                        nil)))))
    (if (null parts)
        (values (list (cons "result" output)) nil)
        (let* ((execution (getf parts :execution))
               (notes     (append (and execution
                                       (autolith::application--shell-execution-rows execution)
                                       (list execution))
                                  (getf parts :notes)))
               (sections  (loop for section in (getf parts :sections)
                                when (getf section :lines)
                                  collect (cons (getf section :label)
                                                (format nil "~{~A~^~%~}" (getf section :lines))))))
          (values (append sections
                          (and notes (list (cons "note" (format nil "~{~A~^~%~}" notes)))))
                  (getf parts :exit))))))

(-> tool-entry-settle (tool-entry &key (:success-p boolean) (:output t) (:seconds (or null real))) null)
(defun tool-entry-settle (entry &key success-p output seconds)
  "Record in the tool call ENTRY how the call ended."
  (setf (tool-entry-status entry)  (if success-p ':completed ':failed)
        (tool-entry-output entry)  (and output (princ-to-string output))
        (tool-entry-seconds entry) seconds)
  nil)

(-> record--result (list) list)
(defun record--result (properties)
  "Return what the tool result PROPERTIES say of their call, as the arguments of `tool-entry-settle'."
  (list :success-p (and (member (getf properties :status) '(:ok :success)) t)
        :output    (getf properties :output)
        :seconds   (record--seconds properties)))

(-> projector--settle-tool
    (projector (or null string) &rest t &key (:success-p boolean) (:output t) (:seconds (or null real))) null)
(defun projector--settle-tool (projector call-identifier &rest result &key success-p output seconds)
  "Record the result of the tool call CALL-IDENTIFIER in PROJECTOR's transcript."
  (declare (ignore success-p output seconds))
  (let* ((transcript (projector-transcript projector))
         (entry      (and call-identifier (transcript-find-tool transcript call-identifier))))
    (when entry
      (apply #'tool-entry-settle entry result)
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
     (apply #'projector--settle-tool projector (getf (rest record) :call-id) (record--result (rest record)))
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
                        :text (format nil "yielded: ~D job~:P will wake the agent" count)))))
    ((:summary :native-compaction)
     (make-instance 'marker-entry :time (record--time record) :label *compaction-label*))
    (:turn-aborted
     ;; A turn someone stopped is not a failure, and its condition says nothing
     ;; a person can read.
     (if (eq (getf (rest record) :reason) ':cancelled)
         (make-instance 'notice-entry
                        :time (record--time record)
                        :text "stopped")
         (make-instance 'notice-entry
                        :time   (record--time record)
                        :tone   ':failure
                        :text   "the turn stopped"
                        :detail (let ((message (getf (rest record) :message)))
                                  (and (stringp message) message)))))
    (otherwise
     nil)))

(-> records->entries (projector list) list)
(defun records->entries (projector records)
  "Return the entries of RECORDS, oldest first, for a page of history.

Tool results settle calls among the returned entries, which are not in the
transcript yet. A result whose call stands on the page before this one waits
in PROJECTOR for that page, which is read next."
  (let ((entries   nil)
        (calls     (make-hash-table :test #'equal))
        (unsettled nil))
    (flet ((settle (properties)
             (let ((entry (gethash (getf properties :call-id) calls)))
               (when entry
                 (apply #'tool-entry-settle entry (record--result properties))
                 t))))
      (dolist (record records)
        (if (eq (first record) ':tool-result)
            (unless (settle (rest record))
              (push (rest record) unsettled))
            (let ((entry (record->entry projector record)))
              (when (and entry (not (record--join-schemas entry (first entries))))
                (when (and (typep entry 'tool-entry) (tool-entry-call-identifier entry))
                  (setf (gethash (tool-entry-call-identifier entry) calls) entry))
                (push entry entries)))))
      (mapc #'settle (shiftf (projector--unsettled projector) unsettled)))
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
      (incf (projector--streamed projector) (length delta))
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
  "Complete the streamed answer with the part of its persisted TEXT that did not stream.

A provider that does not stream sends the whole of it here. One that streams
sent all of it already, in one entry or in several when a notice came between."
  (when (stringp text)
    (let ((streamed (projector--streamed projector)))
      (when (< streamed (length text))
        (projector--text projector (subseq text streamed)))))
  (setf (projector--streamed projector) 0)
  (projector--close-stream projector)
  nil)

(-> projector--append-marker (projector string) null)
(defun projector--append-marker (projector label)
  "Mark PROJECTOR's transcript with LABEL unless its last entry is that mark already.

A compaction is told twice while the window looks: by its status and by its record."
  (let* ((transcript (projector-transcript projector))
         (count      (transcript-length transcript))
         (last       (and (plusp count) (transcript-entry transcript (1- count)))))
    (unless (and (typep last 'marker-entry) (equal (marker-entry-label last) label))
      (projector--close-stream projector)
      (projector--append projector (make-instance 'marker-entry :label label))))
  nil)

(-> projector--status (projector t list) null)
(defun projector--status (projector status details)
  "Apply the provider and tool lifecycle STATUS with its DETAILS."
  (case status
    (:provider-request-started
     (setf (projector-catch-up-p projector) nil
           (projector--streamed projector)  0)
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
     (projector--append-marker projector *compaction-label*))
    (otherwise
     nil))
  nil)

(defparameter *notice-glyphs* '((#\∙ . :quiet) (#\! . :failure) (#\@ . :quiet) (#\◆ . :quiet) (#\↪ . :quiet))
  "The glyphs Autolith opens a one-line notice with, and how each should read.")

(-> projector--notice-line (string) (values (or null string) (or null keyword)))
(defun projector--notice-line (text)
  "Return the words and tone of TEXT when it is one of Autolith's one-line notices, else NIL.

Such a line opens with a glyph and a space; the glyph is the terminal's way
of marking it and is dropped."
  (let ((tone (and (> (length text) 2)
                   (char= (char text 1) #\Space)
                   (null (position #\Newline text))
                   (rest (assoc (char text 0) *notice-glyphs*)))))
    (if tone
        (values (string-trim " " (subseq text 2)) tone)
        (values nil nil))))

(-> projector--reasoning-summary (string) (or null string))
(defun projector--reasoning-summary (text)
  "Return the reasoning summary TEXT holds when it is the terminal's rendering of one, else NIL.

The terminal prints a summary under a heading of its own, each line behind a
bar; the heading and the bars are the terminal's and are dropped."
  (let* ((lines (markdown-lines text))
         (head  (string-trim '(#\Space) (or (first lines) ""))))
    (and (> (length head) 2)
         (search "reasoning summary" head)
         (not (alphanumericp (char head 0)))
         ;; The terminal wrapped the summary to its own width: rows that follow
         ;; one another are one paragraph, an empty row parts two.
         (let ((paragraphs nil)
               (words      nil))
           (dolist (line (rest lines))
             (let ((row (string-trim '(#\Space) (string-left-trim '(#\Space #\│ #\|) line))))
               (cond
                 ((plusp (length row))
                  (push row words))
                 (words
                  (push (format nil "~{~A~^ ~}" (nreverse words)) paragraphs)
                  (setf words nil)))))
           (when words
             (push (format nil "~{~A~^ ~}" (nreverse words)) paragraphs))
           (format nil "~{~A~^~2%~}" (nreverse paragraphs))))))

(-> projector--letters (string) string)
(defun projector--letters (text)
  "Return the letters and digits of TEXT, lowercased, which is what two renderings of one summary share."
  (string-downcase (remove-if-not #'alphanumericp text)))

(-> projector--note-reasoning (projector string) null)
(defun projector--note-reasoning (projector summary)
  "Show the reasoning SUMMARY as a thought unless the thought just shown already says it."
  (let* ((transcript (projector-transcript projector))
         (entries    (transcript-entries transcript))
         (recent     (find-if (lambda (entry)
                                (typep entry 'reasoning-entry))
                              entries
                              :from-end t
                              :start    (max 0 (- (length entries) 6))))
         (letters    (projector--letters summary)))
    (unless (or (zerop (length letters))
                (and recent (search letters (projector--letters (reasoning-entry-text recent)))))
      (projector--close-stream projector)
      (projector--append projector (make-instance 'reasoning-entry :text summary))))
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
        ;; The terminal's own rendering of a reasoning summary is a thought, shown once.
        ((projector--reasoning-summary trimmed)
         (projector--note-reasoning projector (projector--reasoning-summary trimmed)))
        ;; A command prints what it has to say; the NIL it returns is noise after it.
        ((and open
              (plusp (length (output-entry-text open)))
              (string= (string-trim '(#\Space) trimmed) "⇒ NIL"))
         nil)
        (echo-p
         (setf (projector-invocation projector) nil
               (projector--output projector)
               (projector--append projector
                                  (make-instance 'output-entry :invocation (subseq trimmed 2)))))
        ;; A notice is a line of its own, not part of a block, unless a command
        ;; the person ran is printing: what that prints is its output.
        ((and (or (null open) (null (output-entry-invocation open)))
              (projector--notice-line trimmed))
         (multiple-value-bind (words tone) (projector--notice-line trimmed)
           (projector--close-stream projector)
           ;; A notice that lists calls after its statement, as the one about
           ;; memories does, keeps the statement in its line and the calls behind it.
           (let ((calls (search " · (" words)))
             (projector--append
              projector
              (make-instance 'notice-entry
                             :tone   tone
                             :text   (if calls (subseq words 0 calls) words)
                             :detail (and calls
                                          (format nil "~{~A~^~%~}"
                                                  (loop with rest = (subseq words (+ calls 3))
                                                        for cut = (search " · " rest)
                                                        collect (subseq rest 0 cut)
                                                        while cut
                                                        do (setf rest (subseq rest (+ cut 3)))))))))))
        (open
         (setf (output-entry-text open)
               (if (plusp (length (output-entry-text open)))
                   (format nil "~A~%~A" (output-entry-text open) trimmed)
                   trimmed))
         (transcript-touch (projector-transcript projector) open))
        ;; A slash command is not echoed; its output goes by what the person sent.
        (t
         (setf (projector--output projector)
               (projector--append projector
                                  (make-instance 'output-entry
                                                 :text       trimmed
                                                 :invocation (shiftf (projector-invocation projector) nil))))))))
  nil)

(-> projector--live-record (projector list) null)
(defun projector--live-record (projector record)
  "Take the live RECORD when history did not show it and the turn's events do not carry it."
  (let ((sequence (record-sequence record)))
    (when (or (null sequence) (> sequence (projector-boundary projector)))
      (let* ((entry      (if (or (projector-catch-up-p projector)
                                 (not (member (first record) '(:provider-item :tool-result))))
                             (record->entry projector record)
                             (record--schemas-entry record)))
             (transcript (projector-transcript projector))
             (count      (transcript-length transcript))
             (last       (and (plusp count) (transcript-entry transcript (1- count)))))
        (cond
          ((null entry)
           nil)
          ((record--join-schemas entry last)
           (transcript-touch transcript last))
          ((typep entry 'marker-entry)
           (projector--append-marker projector (marker-entry-label entry)))
          (t
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
       (setf (projector--output projector)    nil
             (projector-invocation projector) nil))
      (otherwise
       nil)))
  nil)

(-> channel-display-name (t) string)
(defun channel-display-name (identifier)
  "Return the channel IDENTIFIER as people read it, with its visual hyphen."
  (if (stringp identifier)
      (autolith::conversation-identifier-display identifier)
      "?"))
