;;;; -- Live Exercise --

;;; Drive real turns through a world server, without a screen and without an
;;; account. The model is a script in this process that speaks the Chat
;;; Completions protocol on a local port; the world server is Autolith's own,
;;; started for a world that script/exercise-live makes and throws away. What
;;; a person does in the window is done here by handing it the same keys and
;;; clicks: send, queue, approve a command, stop, steer, run a command, and
;;; reopen the channel from its history.
;;;
;;; Usage: script/exercise-live

(load (merge-pathnames "exercise-support.lisp" *load-truename*))

(in-package #:aui)


;;;; -- A Model That Is a Script --

(defparameter *mock-title*
  "{\"title\": \"A short title\", \"agent_name\": \"bridge-builder\"}"
  "What the mock answers when Autolith asks for a channel's title and its agent's name.")

(defparameter *mock-answer*
  "## What I found

The bridge builds on both systems now. Three things changed:

1. The **Makefile** asks `fltk-config` for its flags.
2. Fonts are found through *CoreText* on a Mac.
3. The smoke tests run in CI.

```lisp
(defun bridge-library-name ()
  \"Return the file name of the bridge.\"
  #+darwin \"liblightfast.dylib\"
  #-darwin \"liblightfast.so\")
```

| System | Library | Fonts |
|--------|---------|-------|
| Linux | `.so` | Pango |
| macOS | `.dylib` | CoreText |

That is all of it.
"
  "What the mock model answers a question with: Markdown of every kind the window typesets.")

(-> mock--octets (string) (simple-array (unsigned-byte 8) (*)))
(defun mock--octets (string)
  "Return STRING encoded as UTF-8."
  (sb-ext:string-to-octets string :external-format ':utf-8))

(-> mock--pieces (string integer) list)
(defun mock--pieces (text size)
  "Return TEXT cut into pieces of SIZE characters, as a model streams it."
  (loop for start from 0 below (length text) by size
        collect (subseq text start (min (length text) (+ start size)))))

(-> mock--read-request (stream) (values string string))
(defun mock--read-request (stream)
  "Return the method and the body of the HTTP request arriving on STREAM."
  (let ((header (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (loop for octet = (read-byte stream nil nil)
          while octet
          do (vector-push-extend octet header)
          until (let ((length (length header)))
                  (and (>= length 4)
                       (= (aref header (- length 1)) 10)
                       (= (aref header (- length 2)) 13)
                       (= (aref header (- length 3)) 10)
                       (= (aref header (- length 4)) 13))))
    (let* ((text   (sb-ext:octets-to-string (coerce header '(simple-array (unsigned-byte 8) (*)))
                                            :external-format ':latin-1))
           (method (subseq text 0 (or (position #\Space text) 0)))
           (marker (search "content-length:" text :test #'char-equal))
           (length (if marker
                       (parse-integer text :start (+ marker 15) :junk-allowed t)
                       0))
           (body   (make-array (or length 0) :element-type '(unsigned-byte 8))))
      (read-sequence body stream)
      (values method (sb-ext:octets-to-string body :external-format ':utf-8)))))

(-> mock--send (stream string) null)
(defun mock--send (stream string)
  "Write STRING to STREAM and push it out."
  (write-sequence (mock--octets string) stream)
  (finish-output stream)
  nil)

(-> mock--send-json (stream t) null)
(defun mock--send-json (stream value)
  "Answer the request on STREAM with the JSON VALUE."
  (let ((body (mock--octets (autolith::json-encode value))))
    (mock--send stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: application/json~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                               #\Return #\Newline #\Return #\Newline (length body)
                               #\Return #\Newline #\Return #\Newline #\Return #\Newline))
    (write-sequence body stream)
    (finish-output stream))
  nil)

(-> mock--send-event (stream string) null)
(defun mock--send-event (stream data)
  "Send DATA as one server-sent event in one chunk of the chunked answer on STREAM."
  (let ((octets (mock--octets (format nil "data: ~A~C~C" data #\Newline #\Newline))))
    (mock--send stream (format nil "~X~C~C" (length octets) #\Return #\Newline))
    (write-sequence octets stream)
    (mock--send stream (format nil "~C~C" #\Return #\Newline)))
  nil)

(-> mock--chunk (t &key (:finish (or null string))) string)
(defun mock--chunk (delta &key finish)
  "Return a streamed completion chunk carrying DELTA, ending the answer with FINISH when given."
  (autolith::json-encode
   (autolith::json-object
    "id"      "chatcmpl-mock"
    "object"  "chat.completion.chunk"
    "model"   "mock-1"
    "choices" (vector (autolith::json-object "index"         0
                                             "delta"         delta
                                             "finish_reason" (or finish ':null))))))

(-> mock--text (t) string)
(defun mock--text (content)
  "Return the text of a message's CONTENT, which is a string or an array of parts."
  (cond
    ((stringp content)
     content)
    ((vectorp content)
     (format nil "~{~A~^ ~}"
             (loop for part across content
                   when (and (autolith::json-object-p part) (stringp (autolith::json-get part "text")))
                     collect (autolith::json-get part "text"))))
    (t
     "")))

(-> mock--spoken (t) list)
(defun mock--spoken (request)
  "Return REQUEST's messages without the temporary context Autolith ends each request with."
  (let ((messages (autolith::json-get request "messages")))
    (remove-if (lambda (message)
                 (let ((text (mock--text (autolith::json-get message "content"))))
                   (and (>= (length text) 17) (string= "Temporary context" text :end2 17))))
               (coerce (if (vectorp messages) messages #()) 'list))))

(-> mock--stream (stream t) null)
(defun mock--stream (stream request)
  "Answer REQUEST on STREAM as a model would, by what the person or a tool said last.

A request without tools is one of Autolith's own, a title or a
classification, and gets the title and the agent's name as Autolith asks for
them. A tool's result gets a closing sentence. A request made while a goal
is in force gets the words that say the goal is met. A message that mentions
a shell gets a call to the shell tool.
Anything else gets a thought and the Markdown answer, slowly when the message
says so."
  (let* ((tools     (let ((value (autolith::json-get request "tools")))
                      (if (vectorp value) (coerce value 'list) nil)))
         (spoken    (mock--spoken request))
         (last      (first (last spoken)))
         (last-user (mock--text
                     (autolith::json-get
                      (find "user" spoken
                            :key      (lambda (message)
                                        (autolith::json-get message "role"))
                            :test     #'equal
                            :from-end t)
                      "content")))
         (slow-p    (search "slowly" last-user :test #'char-equal))
         ;; While a goal is in force Autolith tells the model, in each request,
         ;; which marker says that the goal is met.
         (goal-p    (some (lambda (message)
                            (and (not (equal (autolith::json-get message "role") "assistant"))
                                 (search autolith::*application-goal-complete-marker*
                                         (mock--text (autolith::json-get message "content")))))
                          (let ((messages (autolith::json-get request "messages")))
                            (coerce (if (vectorp messages) messages #()) 'list)))))
    (flet ((say (text &key (member "content") (pause 0.02))
             (dolist (piece (mock--pieces text 14))
               (mock--send-event stream (mock--chunk (autolith::json-object member piece)))
               (sleep pause))))
      (mock--send stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C"
                                 #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                 #\Return #\Newline #\Return #\Newline))
      (mock--send-event stream (mock--chunk (autolith::json-object "role" "assistant" "content" "")))
      (cond
        ((null tools)
         (say *mock-title*)
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ((equal (autolith::json-get last "role") "tool")
         (say "The command ran and printed what you see above.")
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ;; A turn toward the goal: the model says the goal is met, as Autolith asks it to.
        (goal-p
         (say (format nil "The bridge builds. ~A" autolith::*application-goal-complete-marker*))
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ((search "shell" last-user :test #'char-equal)
         (let ((name (or (find-if (lambda (name)
                                    (and (stringp name) (search "shell" name)))
                                  (mapcar (lambda (tool)
                                            (autolith::json-get (autolith::json-get tool "function") "name"))
                                          tools))
                         "shell_run")))
           (say "I will run one command to look.")
           (mock--send-event
            stream
            (mock--chunk
             (autolith::json-object
              "tool_calls" (vector (autolith::json-object
                                    "index"    0
                                    "id"       "call_mock_1"
                                    "type"     "function"
                                    "function" (autolith::json-object
                                                "name"      name
                                                "arguments" "{\"command\": \"echo hello from the mock\"}"))))))
           (mock--send-event stream (mock--chunk (autolith::json-object) :finish "tool_calls"))))
        (t
         (say "The person asks about the bridge. I should say what changed and show it."
              :member "reasoning_content")
         (say *mock-answer* :pause (if slow-p 0.3 0.02))
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop"))))
      (mock--send-event
       stream
       (autolith::json-encode
        (autolith::json-object
         "id"      "chatcmpl-mock"
         "object"  "chat.completion.chunk"
         "model"   "mock-1"
         "choices" (vector)
         "usage"   (autolith::json-object "prompt_tokens" 1200 "completion_tokens" 180 "total_tokens" 1380))))
      (mock--send-event stream "[DONE]")
      (mock--send stream (format nil "0~C~C~C~C" #\Return #\Newline #\Return #\Newline))))
  nil)

(-> mock--answer (stream) null)
(defun mock--answer (stream)
  "Answer the one request arriving on STREAM."
  (multiple-value-bind (method body) (mock--read-request stream)
    (cond
      ((string= method "GET")
       (mock--send-json stream
                        (autolith::json-object
                         "object" "list"
                         "data"   (vector (autolith::json-object "id" "mock-1" "object" "model")))))
      (t
       (let ((request (autolith::json-decode body)))
         (if (eq (autolith::json-get request "stream") t)
             (mock--stream stream request)
             (mock--send-json
              stream
              (autolith::json-object
               "id"      "chatcmpl-mock"
               "object"  "chat.completion"
               "model"   "mock-1"
               "choices" (vector (autolith::json-object
                                  "index"         0
                                  "finish_reason" "stop"
                                  "message"       (autolith::json-object "role"    "assistant"
                                                                         "content" *mock-title*)))
               "usage"   (autolith::json-object "prompt_tokens" 10 "completion_tokens" 3 "total_tokens" 13))))))))
  nil)

(-> mock-start () integer)
(defun mock-start ()
  "Start the mock model on a free local port and return the port.

Each request is answered on a thread of its own; a client that goes away in
the middle of an answer, as a stopped turn does, ends only that thread."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 16)
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((client (sb-bsd-sockets:socket-accept socket)))
           (sb-thread:make-thread
            (lambda ()
              (let ((stream (sb-bsd-sockets:socket-make-stream client
                                                               :input        t
                                                               :output       t
                                                               :element-type '(unsigned-byte 8)
                                                               :buffering    ':full)))
                (unwind-protect
                     (handler-case (mock--answer stream)
                       (error ()
                         nil))
                  (ignore-errors (close stream :abort t)))))
            :name "mock model answer"))))
     :name "mock model")
    (nth-value 1 (sb-bsd-sockets:socket-name socket))))


;;;; -- A World of Its Own --

(-> exercise-live--world-root () string)
(defun exercise-live--world-root ()
  "Return the directory of the throwaway world this exercise may use.

Leave at once when the process was not given one, or when any of the places
Autolith keeps state or looks for a login lies outside it: the exercise starts
a world server, registers a provider and stops the server again, none of
which may happen to a person's own world."
  (let ((root (uiop:getenv "AUI_EXERCISE_WORLD")))
    (unless (and root
                 (plusp (length root))
                 (every (lambda (name)
                          (let ((value (uiop:getenv name)))
                            (and value (uiop:string-prefix-p root value))))
                        '("XDG_DATA_HOME" "XDG_STATE_HOME" "XDG_CONFIG_HOME" "CODEX_HOME" "GROK_HOME")))
      (format t "~&Run script/exercise-live, which gives the exercise a world of its own.~%")
      (finish-output)
      (sb-ext:exit :code 64 :abort t))
    root))

(-> exercise-live--prepare (string integer) null)
(defun exercise-live--prepare (root port)
  "Register the mock model listening on PORT as the only provider of the world under ROOT, with a key that opens nothing."
  (let* ((configuration (autolith::configuration-create :immutable-p t :defer-provider-validation-p t))
         (init          (merge-pathnames "init.lisp" (autolith::config :config-root configuration)))
         (keys          (autolith::configuration-api-keys-path configuration)))
    (unless (and (uiop:string-prefix-p root (namestring init))
                 (uiop:string-prefix-p root (namestring keys)))
      (format t "~&Autolith's configuration is not under ~A; not touching it.~%" root)
      (finish-output)
      (sb-ext:exit :code 64 :abort t))
    ;; The script that started this stops the server if this process dies first.
    (with-open-file (stream (format nil "~A/autolith-program" root) :direction ':output :if-exists ':supersede)
      (write-line (installation-program) stream))
    (ensure-directories-exist init)
    (with-open-file (stream init :direction ':output :if-exists ':supersede)
      (format stream "(register-openai-compatible-provider~% :name        \"mock\"~% :description \"The live exercise's model\"~% :endpoint    \"http://127.0.0.1:~D/v1/chat/completions\"~% :models      '(\"mock-1\"))~%"
              port))
    (ensure-directories-exist keys)
    (autolith::api-key-credential-manager-save-key
     (autolith::api-key-credential-manager-create :provider-name "mock" :pathname keys)
     "a-key-that-opens-nothing"))
  nil)


;;;; -- The Exercise --

(-> exercise-live--until (workbench function &key (:seconds real)) boolean)
(defun exercise-live--until (workbench predicate &key (seconds 60))
  "Apply what the channel says until PREDICATE holds and return whether it did within SECONDS."
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (main--settle workbench 0.2))))

(-> exercise-live--type (workbench string integer) null)
(defun exercise-live--type (workbench text code)
  "Put TEXT in the composer and press the key with the code CODE there."
  (setf (composer-text workbench) text)
  (window--editor-key workbench (make-key-event :code code))
  nil)

(-> exercise-live--entries (workbench symbol) list)
(defun exercise-live--entries (workbench class)
  "Return the entries of CLASS the window shows, oldest first."
  (remove-if-not (lambda (entry)
                   (typep entry class))
                 (coerce (transcript-entries (workbench-transcript workbench)) 'list)))

(-> exercise-live--picture (workbench string) null)
(defun exercise-live--picture (workbench name)
  "Write a picture of the window named NAME into the directory AUI_EXERCISE_SHOTS names, when it names one."
  (let ((directory (uiop:getenv "AUI_EXERCISE_SHOTS")))
    (when (and directory (plusp (length directory)))
      (window-arrange workbench)
      (main--picture workbench
                     (uiop:native-namestring
                      (merge-pathnames (format nil "~A.png" name)
                                       (uiop:ensure-directory-pathname directory))))))
  nil)

(-> exercise-live--idle-p (workbench) boolean)
(defun exercise-live--idle-p (workbench)
  "Return T when the channel neither works nor waits for an answer."
  (and (not (workbench-working-p workbench))
       (null (workbench-approval workbench))
       (null (workbench-pending workbench))))

(-> exercise-live-run () integer)
(defun exercise-live-run ()
  "Exercise live turns against the mock model and return how many expectations failed."
  (let ((root (exercise-live--world-root)))
    (exercise-live--prepare root (mock-start))
    (lightfast:load-library)
    (lightfast:enable-thread-wakeups)
    (theme-install)
    (let* ((directory                (uiop:native-namestring
                                      (ensure-directories-exist
                                       (uiop:ensure-directory-pathname (format nil "~A/work/bridge" root)))))
           (launch                   (launch-parse (list "-C" directory "--model" "mock/mock-1"
                                                         "--size" "1200x800" "--shot" "unused.png")))
           (workbench                (make-instance 'workbench
                                                    :launch launch
                                                    :world  (launch-world launch)
                                                    :queue  (make-ui-queue :wakeup #'lightfast:awake)))
           (*stage-failure-function* (lambda (report)
                                       (incf *exercise-failures*)
                                       (format t "~&  FAIL  ~A~%" report))))
      (workbench--reset-channel workbench)
      (window-build workbench)
      (main--start workbench)
      (window-show workbench)
      (unwind-protect
           (progn
             ;; The first open starts the world server, which takes its time.
             (let ((command-p nil))
               (exercise-expect "a first start brings up a world server and a channel"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (when (equal (second (workbench-progress workbench))
                                                                     "starting the world server")
                                                          (setf command-p t))
                                                        (workbench-attached-p workbench))
                                                      :seconds 600))
               (exercise-expect "the autolith command started that server" command-p))
             (main--settle workbench 1)
             (exercise-expect "the channel runs the model the command line named"
                              (search "mock-1" (or (workbench-model workbench) "")))
             ;; A turn, watched while it streams.
             (exercise-live--type workbench "Tell me about the bridge, slowly" #xff0d)
             (exercise-expect "Enter sends and the agent works"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (workbench-working-p workbench))))
             (exercise-expect "the answer streams while the agent works"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (and (workbench-working-p workbench)
                                      (some (lambda (entry)
                                              (search "What I found" (assistant-entry-text entry)))
                                            (exercise-live--entries workbench 'assistant-entry))))))
             (exercise-expect "what the model thought shows as a thought"
                              (exercise-live--entries workbench 'reasoning-entry))
             (exercise-live--picture workbench "live-streaming")
             ;; A follow-up queued while it works.
             (exercise-live--type workbench "Now use a shell tool to look around" #xff09)
             (main--settle workbench 0.5)
             (exercise-expect "Tab queues a follow-up"
                              (find "Now use a shell tool to look around" (workbench-pending workbench)
                                    :key #'second :test #'equal))
             (exercise-expect "what waits is read from the channel's own queue"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (workbench-poll-waiting workbench :now-p t)
                                 (find "Now use a shell tool to look around" (workbench--waiting workbench)
                                       :key #'first :test #'equal))
                               :seconds 20))
             ;; The follow-up asks for a command, and the channel asks first.
             (exercise-expect "the channel asks before a command runs"
                              (and (exercise-live--until workbench
                                                         (lambda ()
                                                           (workbench-approval workbench))
                                                         :seconds 120)
                                   (progn
                                     (exercise--draw workbench)
                                     (exercise--region workbench '(:approval "once")))))
             (exercise-live--picture workbench "live-approval")
             (exercise--click workbench '(:approval "once"))
             (exercise-expect "allowed once, the command runs and the turn ends"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (exercise-live--idle-p workbench))
                                                    :seconds 120))
             (main--settle workbench 1)
             (let ((tool (first (exercise-live--entries workbench 'tool-entry))))
               (exercise-expect "the call shows what the command printed"
                                (and tool
                                     (search "hello from the mock" (princ-to-string (tool-entry-output tool)))))
               (when tool
                 (setf (entry-expanded-p tool) t)
                 (workbench-redraw workbench)
                 (exercise-live--picture workbench "live-tool")
                 (setf (entry-expanded-p tool) nil)))
             ;; Stopping a turn in the middle.
             (exercise-live--type workbench "Tell me again, slowly" #xff0d)
             (exercise-live--until workbench
                                   (lambda ()
                                     (and (workbench-working-p workbench)
                                          (search "What I found"
                                                  (assistant-entry-text
                                                   (first (last (exercise-live--entries workbench
                                                                                        'assistant-entry))))))))
             (window--editor-key workbench (make-key-event :code #xff1b))
             (exercise-expect "Escape stops the turn"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (not (workbench-working-p workbench)))
                                                    :seconds 60))
             (main--settle workbench 1)
             (exercise-expect "a stopped turn says so quietly"
                              (let ((notice (first (last (exercise-live--entries workbench 'notice-entry)))))
                                (and notice
                                     (string= "stopped" (notice-entry-text notice))
                                     (eq ':quiet (notice-entry-tone notice)))))
             ;; Steering a running turn.
             (exercise-live--type workbench "Tell me once more, slowly" #xff0d)
             (exercise-live--until workbench
                                   (lambda ()
                                     (workbench-working-p workbench)))
             (main--settle workbench 1)
             (exercise-live--type workbench "Also say which fonts" #xff0d)
             (main--settle workbench 0.5)
             (exercise-expect "Enter while the agent works steers it"
                              (third (find "Also say which fonts" (workbench-pending workbench)
                                           :key #'second :test #'equal)))
             (exercise-expect "the agent takes the steering up"
                              (and (exercise-live--until workbench
                                                         (lambda ()
                                                           (exercise-live--idle-p workbench))
                                                         :seconds 180)
                                   (find "Also say which fonts" (exercise-live--entries workbench 'user-entry)
                                         :key #'user-entry-text :test #'equal)))
             ;; A command.
             (exercise-live--type workbench "/status" #xff0d)
             (exercise-expect "a command prints into the conversation under its name"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (some (lambda (entry)
                                         (and (search "mock-1" (output-entry-text entry))
                                              (equal "/status" (output-entry-invocation entry))))
                                       (exercise-live--entries workbench 'output-entry)))))
             (exercise-expect "the channel has the title the model gave it"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (search "short title" (workbench-title workbench)))))
             (exercise-expect "the channel list names the agent as the model named it"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (equal "bridge-builder"
                                        (getf (find (workbench-channel workbench)
                                                    (world-entries (workbench-world workbench))
                                                    :key  (lambda (entry)
                                                            (getf entry :id))
                                                    :test #'equal)
                                              :name)))))
             ;; The chips under the composer change the channel itself.
             (workbench-set-setting workbench "reasoning-effort" "low")
             (exercise-expect "the effort chip changes the channel's effort"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (equal "low" (workbench-effort workbench)))
                                                    :seconds 30))
             ;; Full access is a mode every platform has; a sandbox is not.
             (workbench-set-mode workbench ':full-access)
             (exercise-expect "the permission chip changes the channel's mode"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (eq ':full-access (workbench-permission-mode workbench)))
                                                    :seconds 30))
             ;; A setting whose values are keywords on the server is set by its spelling.
             (workbench-set-setting workbench "permission-mode" "auto")
             (exercise-expect "a saved choice with keyword values can be changed"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (let ((row (find "permission-mode" (workbench-settings workbench)
                                                  :key  (lambda (row)
                                                          (portable-get row "name"))
                                                  :test #'equal)))
                                   (and row (equal "auto" (portable-get row "value")))))
                               :seconds 30))
             ;; The channel works where the person says.
             (let ((folder (namestring (uiop:ensure-directory-pathname (format nil "~A/work/elsewhere" root)))))
               (ensure-directories-exist folder)
               (workbench-submit workbench (format nil "(cwd ~S)" folder))
               (exercise-expect "a channel's folder can be changed, and the window follows"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (equal folder (workbench-directory workbench)))
                                                      :seconds 30)))
             ;; A goal: set in the channel, known to the window, met when the agent says so.
             (workbench-submit workbench "(goal \"Keep the bridge building.\")")
             (exercise-expect "a goal set in the channel is known to the window"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (equal "Keep the bridge building." (getf (workbench-goal workbench) :objective)))
                               :seconds 60))
             (exercise-expect "the goal is complete once the agent says so"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (and (eq ':complete (getf (workbench-goal workbench) :status))
                                      (exercise-live--idle-p workbench)))
                               :seconds 180))
             ;; An Autolith from 0.67.57 on names the goal in the channel's state.
             (let ((state (workbench-state workbench)))
               (format t "~&  note  the goal is read from ~:[the newest goal record~;the channel's state~]~%"
                       (portable-member-p state "goal"))
               (exercise-expect "a state that names the goal names the one the window shows"
                                (or (not (portable-member-p state "goal"))
                                    (equal "Keep the bridge building."
                                           (portable-get (portable-get state "goal") "objective")))))
             (stage-resize (workbench-stage workbench) 1500 900)
             (exercise--draw workbench)
             (exercise-expect "the console names the goal and offers to change or clear it"
                              (and (exercise--region workbench '(:console-section :goal))
                                   (exercise--region workbench '(:console-goal :update))
                                   (exercise--region workbench '(:console-goal :clear))))
             (exercise-live--picture workbench "live-goal")
             (stage-resize (workbench-stage workbench) 1200 800)
             (exercise--draw workbench)
             ;; The same channel again, from its history.
             (let ((channel (workbench-channel workbench)))
               (workbench-open workbench :channel channel)
               (exercise-expect "the channel reopens from its history"
                                (and (exercise-live--until workbench
                                                           (lambda ()
                                                             (workbench-attached-p workbench))
                                                           :seconds 120)
                                     (progn
                                       (main--settle workbench 1.5)
                                       (equal channel (workbench-channel workbench)))))
               (exercise-expect "history holds the messages, the call and its output"
                                (and (>= (length (exercise-live--entries workbench 'user-entry)) 4)
                                     (let ((tool (first (exercise-live--entries workbench 'tool-entry))))
                                       (and tool
                                            (search "hello from the mock"
                                                    (princ-to-string (tool-entry-output tool)))))))
               (exercise-live--picture workbench "live-history")
               (exercise-expect "the goal is read from the channel's history"
                                (and (equal "Keep the bridge building."
                                            (getf (workbench-goal workbench) :objective))
                                     (eq ':complete (getf (workbench-goal workbench) :status))))
               (workbench-goal-request workbench ':clear)
               (exercise-expect "a goal cleared from the window is gone"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (null (workbench-goal workbench)))
                                                      :seconds 30)))
             (exercise-expect "nothing reported a failure" (null (workbench-notice workbench)))
             ;; Deleting the channel leaves it and takes it out of the world.
             (let ((channel (workbench-channel workbench)))
               (workbench-delete-channel workbench channel)
               (exercise-expect "a channel can be deleted"
                                (exercise-live--until
                                 workbench
                                 (lambda ()
                                   (and (null (workbench-channel workbench))
                                        (null (workbench-notice workbench))
                                        (not (find channel (world-entries (workbench-world workbench))
                                                   :key  (lambda (entry)
                                                           (getf entry :id))
                                                   :test #'equal))))
                                 :seconds 60)))
             ;; The world server was started for this exercise alone, and the window stops it.
             (workbench-stop-server workbench)
             (exercise-expect "the window stops the world server"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (not (world-served-p (workbench-world workbench))))
                                                    :seconds 60)))
        (workbench-close workbench)
        ;; A server the exercise did not get as far as stopping is stopped here.
        (uiop:run-program (list (installation-program) "stop")
                          :ignore-error-status t))))
  *exercise-failures*)

(exercise-live-run)
(exercise--finish)
