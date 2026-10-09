(in-package #:aui)

;;;; -- Visual Fixtures --

;;; Sample content for inspecting the views by eye. Not used by the
;;; application; script/visual-* run these.

(-> sample-transcript () transcript)
(defun sample-transcript ()
  "Return a transcript with one of every block kind, in states worth looking at."
  (let ((transcript (make-transcript)))
    (transcript-append transcript
                       (make-instance 'user-message
                                      :text "Please give lightfast a flat theme so the workbench can look like paper. Keep the classic theme for the old applications."))
    (transcript-append transcript
                       (make-instance 'reasoning
                                      :text (format nil "The user wants a theme layer rather than per-widget patches.~%I should look at where the literals live first.")))
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_a" :name "search.grep"
                                                 :title "search.grep · textsize(12)" :kind ':search
                                                 :status ':completed :input "{\"patterns\": [\"textsize(12)\"]}"
                                                 :output (format nil "native/cl_fltk_bridge_widgets.cpp: 26 matches~%native/cl_fltk_bridge_style.cpp: 2 matches")))
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_b" :name "shell.run"
                                                 :title "shell.run · make native && make smoke" :kind ':execute
                                                 :status ':running :input "make native && make smoke"))
    (transcript-append transcript
                       (make-instance 'assistant-text
                                      :text (format nil "I added `apply-flat-theme` beside the classic theme. The **Theme** struct carries every default, so widgets created later follow it:~%~%```lisp~%(lightfast:apply-flat-theme :background '(255 254 250)~%                            :label-font serif~%                            :mono-font mono)~%```~%~%Two things still need your eye:~%~%- the scrollbar slider is black on paper, which may be heavy~%- tabs show a doubled edge~%~%See [lightfast/README.md](file:///root/common-lisp/lightfast/README.md) for the usage note.")))
    (transcript-replace-plan transcript '(("Find the hardcoded style literals" . :completed)
                                          ("Route them through a theme struct" . :completed)
                                          ("Add the flat theme entry point" . :in-progress)
                                          ("Inspect the gallery at three widths" . :pending)))
    (transcript-append transcript
                       (make-instance 'approval :request-id "r1" :title "shell.run"
                                                :arguments "git push origin master"
                                                :directory "/root/common-lisp/lightfast"
                                                :options '(("once" "Allow once" :allow-once)
                                                           ("session" "Allow exact operation this session" :allow-always)
                                                           ("deny" "Reject" :reject-once))))
    (transcript-notice transcript :severity ':warning :text "Context is 72 percent full; compaction will run after this turn."
                                  :key ':context)
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_c" :name "resource.edit"
                                                 :title "resource.edit · src/theme.lisp" :kind ':edit
                                                 :status ':failed :input "{\"uri\": \"workspace:src/theme.lisp\"}"
                                                 :output "Revision mismatch: the file changed since it was read."))
    transcript))

(-> visual-transcript (&optional t) null)
(defun visual-transcript (&optional shot-p)
  "Show SAMPLE-TRANSCRIPT in a window; with SHOT-P, capture the screen and exit."
  (lightfast:load-library)
  (theme-install)
  (let* ((window (lightfast:make-window :width 900 :height 760 :label "AUI transcript"
                                        :app-id "aui"))
         (transcript (sample-transcript))
         (view (make-transcript-view transcript :parent window :x 24 :y 16 :width 852 :height 728)))
    (lightfast:layout-on-resize
     window
     (lightfast:make-layout-column
      :padding (list (spacing 5) (spacing 4) (spacing 5) (spacing 4))
      :children (list (lightfast:make-layout-item (transcript-view-display view) :grow 1 :basis 0))))
    (lightfast:show window)
    (cond
      (shot-p
       (loop repeat 40 do (lightfast:wait 0.05))
       (uiop:run-program '("grim" "/tmp/aui-transcript.png") :ignore-error-status t)
       (lightfast:destroy window))
      (t
       (lightfast:run))))
  nil)


;;; A scripted backend for the demo and for screenshots

(-> sample-conversations () list)
(defun sample-conversations ()
  "Return conversation entries for the demo welcome screen."
  (let ((now (get-universal-time)))
    (list (make-instance 'conversation-entry :id "93tEGL1" :display-id "9-3tEGL1" :title "Howdy"
                                             :directory "/root/common-lisp/aui/" :model "gpt-6-astra"
                                             :created-at (- now 7200) :last-activity-at (- now 300)
                                             :user-turn-count 3 :live-p t)
          (make-instance 'conversation-entry :id "g4P5Hsc" :display-id "g-4P5Hsc" :title "Optimize Autolith"
                                             :directory "/root/common-lisp/frob/" :model "gpt-6.1-sol"
                                             :created-at (- now 86400) :last-activity-at (- now 80000)
                                             :user-turn-count 12)
          (make-instance 'conversation-entry :id "a1o4YWw" :title "Design the flat theme"
                                             :directory "/root/common-lisp/lightfast/" :model "gpt-5.6-luna"
                                             :created-at (- now 200000) :last-activity-at (- now 190000)
                                             :user-turn-count 5))))

(-> sample-settings () list)
(defun sample-settings ()
  "Return setting entries for the demo settings sheet."
  (list (make-instance 'setting-entry :name ':model :label "Model" :group "Model"
                                      :documentation "The model that answers prompts in this session."
                                      :type ':choice :options '("gpt-5.6-luna" "gpt-6-astra" "claude-haiku-4-5")
                                      :value "gpt-5.6-luna" :source ':override :scope ':durable :adjustable-p t)
        (make-instance 'setting-entry :name ':reasoning-effort :label "Reasoning effort" :group "Model"
                                      :documentation "How long the model may think before answering."
                                      :type ':choice :options '("low" "medium" "high" "max")
                                      :value "low" :source ':environment :scope ':durable :adjustable-p t)
        (make-instance 'setting-entry :name ':reasoning-traces-p :label "Show reasoning" :group "Transcript"
                                      :documentation "Whether visible reasoning is streamed into the transcript."
                                      :type ':boolean :value t :source ':default :scope ':durable :adjustable-p t)
        (make-instance 'setting-entry :name ':compaction-threshold-percent :label "Compaction threshold" :group "Context"
                                      :documentation "Context occupancy, in percent, at which compaction runs."
                                      :type ':integer :value 80 :source ':default :scope ':durable :adjustable-p t)
        (make-instance 'setting-entry :name ':permission-mode :label "Permission mode" :group "Permissions"
                                      :documentation "How commands and external tools are authorized."
                                      :type ':choice :options '("ask" "auto" "sandbox" "full")
                                      :value "ask" :source ':override :scope ':durable :adjustable-p t)
        (make-instance 'setting-entry :name ':immutable-p :label "Immutable" :group "Process"
                                      :documentation "Whether source and configuration mutation is disabled."
                                      :type ':boolean :value nil :source ':default :scope ':process
                                      :adjustable-p nil :reason "fixed for the life of the process")))

(-> sample-operations () list)
(defun sample-operations ()
  "Return operation entries for the demo palette."
  (list (make-instance 'operation-entry :name "/compact" :kind ':command :description "compact the conversation now"
                                        :admitted-p t)
        (make-instance 'operation-entry :name "/new" :kind ':command :description "start a new conversation here"
                                        :admitted-p t)
        (make-instance 'operation-entry :name "/tasks" :kind ':command :description "list owned live and durable work"
                                        :admitted-p t)
        (make-instance 'operation-entry :name "/settings" :kind ':command :description "open the settings picker"
                                        :admitted-p t :terminal-owning-p t)
        (make-instance 'operation-entry :name "plan.list" :kind ':tool :description "show the workspace plan"
                                        :admitted-p t)
        (make-instance 'operation-entry :name "lisp.paren-check" :kind ':tool :description "check delimiters in a source tree"
                                        :argument-schema '(:path :string) :admitted-p t)
        (make-instance 'operation-entry :name "self.status" :kind ':tool :description "report live mutation state"
                                        :admitted-p nil)))

(-> sample-models () list)
(defun sample-models ()
  "Return model entries for the demo."
  (list (make-instance 'model-entry :id "gpt-5.6-luna" :provider "chatgpt" :credential-p t :verified-p t
                                    :context-window 400000)
        (make-instance 'model-entry :id "gpt-6-astra" :provider "chatgpt" :credential-p t :verified-p t
                                    :context-window 1000000)
        (make-instance 'model-entry :id "claude-haiku-4-5" :provider "anthropic" :credential-p nil)))

(defclass scripted-backend (backend)
  ((settings  :initform (sample-settings) :accessor scripted--settings)
   (lock      :initform (bordeaux-threads:make-lock "scripted backend") :reader scripted--lock)
   (waiter    :initform (bordeaux-threads:make-condition-variable) :reader scripted--waiter)
   (decision  :initform nil :accessor scripted--decision)
   (cancel-p  :initform nil :accessor scripted--cancel-p)
   (thread    :initform nil :accessor scripted--thread)
   (turns     :initform 0 :accessor scripted--turns))
  (:documentation "A backend that plays a fixed conversation, for the demo."))

(-> make-scripted-backend (string ui-queue) scripted-backend)
(defun make-scripted-backend (directory queue)
  "Return a scripted backend for DIRECTORY posting to QUEUE."
  (make-instance 'scripted-backend :id (format nil "demo-~A" directory) :directory directory :queue queue))

(-> scripted--post (scripted-backend symbol &rest t) ui-event)
(defun scripted--post (backend class &rest initargs)
  "Post an event of CLASS with INITARGS from BACKEND."
  (ui-queue-push (backend-queue backend)
                 (apply #'make-instance class
                        :backend-id (backend-id backend)
                        :generation (backend-generation backend)
                        :session-id "demo"
                        initargs)))

(-> scripted--append (scripted-backend symbol &rest t) ui-event)
(defun scripted--append (backend class &rest initargs)
  "Post a transcript append of a CLASS block with INITARGS."
  (scripted--post backend 'transcript-event :operation ':append
                                            :arguments (list ':class class ':initargs initargs)))

(-> scripted--stream (scripted-backend string string &key (:delay real)) null)
(defun scripted--stream (backend id text &key (delay 0.03))
  "Append TEXT to block ID word by word, pausing DELAY seconds between words."
  (let ((start 0))
    (loop for end = (position #\Space text :start start)
          do (scripted--post backend 'transcript-event :operation ':append-text
                                                       :arguments (list ':id id ':text (subseq text start (and end (1+ end)))))
             (sleep delay)
          while end
          do (setf start (1+ end))))
  nil)

(defmethod backend-start ((backend scripted-backend))
  (scripted--post backend 'connection-event :state ':starting)
  (scripted--post backend 'connection-event :state ':ready)
  (scripted--post backend 'catalog-event :kind ':conversations :items (sample-conversations))
  (scripted--post backend 'catalog-event :kind ':settings :items (scripted--settings backend))
  (scripted--post backend 'catalog-event :kind ':operations :items (sample-operations))
  (scripted--post backend 'catalog-event :kind ':models :items (sample-models))
  (scripted--post backend 'jobs-event :kind ':snapshot
                                      :jobs (list (make-instance 'job-entry :id "job-1" :role "reviewer"
                                                                            :state ':running :elapsed-seconds 42)))
  (scripted--post backend 'state-event
                  :snapshot (make-instance 'state-snapshot :workspace (backend-directory backend)
                                                           :title "Flat theme for lightfast"
                                                           :model "gpt-5.6-luna" :effort "low"
                                                           :permission-mode ':ask :turn-state ':idle
                                                           :usage '(:input 41200 :output 3900 :context-used 48000
                                                                    :context-limit 200000 :context-measured-p t)
                                                           :live-job-count 1 :plan-p t :health "ok"))
  nil)

(defmethod backend-stop ((backend scripted-backend))
  (setf (scripted--cancel-p backend) t)
  (scripted--post backend 'connection-event :state ':exited :detail "demo closed")
  nil)

(defmethod backend-open-conversation ((backend scripted-backend) &key conversation-id)
  (scripted--post backend 'session-event :state ':loading :conversation-id conversation-id
                                         :title (if conversation-id "Flat theme for lightfast" nil))
  (when conversation-id
    (dolist (block (coerce (transcript-blocks (sample-transcript)) 'list))
      (unless (typep block 'approval)
        (scripted--append-block backend block))))
  (scripted--post backend 'session-event :state ':ready :conversation-id (or conversation-id "demo-new")
                                         :title (if conversation-id "Flat theme for lightfast" "New conversation"))
  nil)

(-> scripted--append-block (scripted-backend transcript-block) null)
(defun scripted--append-block (backend block)
  "Post BLOCK as an append event by reading its public slots."
  (let ((initargs
          (etypecase block
            (user-message (list :text (user-message-text block)))
            (assistant-text (list :text (assistant-text-text block)))
            (reasoning (list :text (reasoning-text block)))
            (tool-call (list :call-id (tool-call-call-id block) :name (tool-call-name block)
                             :title (tool-call-title block) :kind (tool-call-kind block)
                             :status (tool-call-status block) :input (tool-call-input block)
                             :output (tool-call-output block)))
            (plan-block (list :entries (plan-block-entries block)))
            (notice (list :severity (notice-severity block) :text (notice-text block)
                          :key (notice-key block))))))
    (apply #'scripted--append backend (class-name (class-of block)) initargs))
  nil)

(defmethod backend-prompt ((backend scripted-backend) text &key attachments)
  (declare (ignore attachments))
  (let ((request-id (make-request-id))
        (turn (incf (scripted--turns backend))))
    (setf (scripted--cancel-p backend) nil
          (scripted--decision backend) nil)
    (setf (scripted--thread backend)
          (bordeaux-threads:make-thread
           (lambda () (scripted--play-turn backend turn text))
           :name "scripted turn"))
    request-id))

(-> scripted--cancelled-p (scripted-backend) boolean)
(defun scripted--cancelled-p (backend)
  "Return T when the turn was cancelled, posting the idle state once."
  (when (scripted--cancel-p backend)
    (scripted--post backend 'turn-event :state ':idle :stop-reason ':cancelled)
    t))

(-> scripted--play-turn (scripted-backend integer string) null)
(defun scripted--play-turn (backend turn text)
  "Play one scripted turn answering TEXT."
  (scripted--post backend 'turn-event :state ':running)
  (scripted--append backend 'user-message :id (format nil "user-~D" turn) :text text)
  (let ((reasoning-id (format nil "reasoning-~D" turn))
        (call-id (format nil "~D:call_demo" turn))
        (answer-id (format nil "answer-~D" turn)))
    (scripted--append backend 'reasoning :id reasoning-id :text "")
    (scripted--stream backend reasoning-id "The request is about the workbench itself. I will check the theme file before answering, then run the check script.")
    (unless (scripted--cancelled-p backend)
      (scripted--append backend 'tool-call :id (format nil "tool-~D" turn) :call-id call-id :name "shell.run"
                                           :kind ':execute :title "shell.run · ./script/check" :status ':running
                                           :input "./script/check")
      (scripted--post backend 'approval-event :request-id (format nil "approval-~D" turn) :call-id call-id
                                              :title "shell.run" :arguments "./script/check"
                                              :directory (backend-directory backend)
                                              :options '(("once" "Allow once" :allow-once)
                                                         ("session" "Allow exact operation this session" :allow-always)
                                                         ("deny" "Reject" :reject-once)))
      (bordeaux-threads:with-lock-held ((scripted--lock backend))
        (loop until (or (scripted--decision backend) (scripted--cancel-p backend))
              do (bordeaux-threads:condition-wait (scripted--waiter backend) (scripted--lock backend) :timeout 1)))
      (unless (scripted--cancelled-p backend)
        (let ((denied (equal (scripted--decision backend) "deny")))
          (scripted--post backend 'transcript-event :operation ':update-tool-call
                                                    :arguments (list ':call-id call-id
                                                                     ':changes (if denied
                                                                                   (list ':status ':denied :output "The user rejected the command.")
                                                                                   (list ':status ':completed
                                                                                         :output (format nil "Running test suite AUI~% Did 157 checks.~%    Pass: 157 (100%)~%    Skip: 0 ( 0%)~%    Fail: 0 ( 0%)")))))
          (sleep 0.3)
          (scripted--append backend 'assistant-text :id answer-id :text "")
          (scripted--stream backend answer-id
                            (if denied
                                "Understood, I did not run the check. Tell me when you would like me to try again, or run it yourself and paste the result."
                                (format nil "The check passes: **157 checks**, none failing. The theme file already carries the palette, so the next step is wiring the status line to `_autolith/state`.~%~%- the header shows model and permission mode~%- the status line shows context occupancy~%~%Shall I continue with the inspector?")))
          (unless (scripted--cancelled-p backend)
            (scripted--post backend 'jobs-event :kind ':delta :removed-ids '("job-1"))
            (scripted--post backend 'turn-event :state ':idle :stop-reason ':end-turn))))))
  nil)

(defmethod backend-cancel ((backend scripted-backend))
  (bordeaux-threads:with-lock-held ((scripted--lock backend))
    (setf (scripted--cancel-p backend) t)
    (bordeaux-threads:condition-notify (scripted--waiter backend)))
  (scripted--post backend 'turn-event :state ':cancelling)
  nil)

(defmethod backend-decide-approval ((backend scripted-backend) request-id option-id)
  (bordeaux-threads:with-lock-held ((scripted--lock backend))
    (setf (scripted--decision backend) option-id)
    (bordeaux-threads:condition-notify (scripted--waiter backend)))
  (scripted--post backend 'transcript-event :operation ':decide-approval
                                            :arguments (list ':request-id request-id ':decision option-id))
  nil)

(defmethod backend-set-mode ((backend scripted-backend) mode-id)
  (declare (ignore mode-id))
  (let ((request-id (make-request-id)))
    (scripted--post backend 'result-event :request-id request-id :outcome ':unsupported)
    request-id))

(defmethod backend-refresh ((backend scripted-backend) kind)
  (let ((request-id (make-request-id)))
    (when (eq kind ':conversations)
      (scripted--post backend 'catalog-event :kind ':conversations :items (sample-conversations)))
    (scripted--post backend 'result-event :request-id request-id :outcome ':ok)
    request-id))

(defmethod backend-set-setting ((backend scripted-backend) name value)
  (let ((request-id (make-request-id))
        (entry (find name (scripted--settings backend) :key #'setting-entry-name)))
    (cond
      ((and entry (setting-entry-adjustable-p entry))
       (setf (scripted--settings backend)
             (substitute (make-instance 'setting-entry
                                        :name name :label (setting-entry-label entry)
                                        :documentation (setting-entry-documentation entry)
                                        :group (setting-entry-group entry) :type (setting-entry-type entry)
                                        :options (setting-entry-options entry)
                                        :value (case (setting-entry-type entry)
                                                 (:boolean (string= value "on"))
                                                 (:integer (or (parse-integer value :junk-allowed t) 0))
                                                 (t value))
                                        :source ':session :scope (setting-entry-scope entry) :adjustable-p t)
                         entry (scripted--settings backend)))
       (scripted--post backend 'catalog-event :kind ':settings :items (scripted--settings backend))
       (scripted--post backend 'result-event :request-id request-id :outcome ':ok))
      (t
       (scripted--post backend 'result-event :request-id request-id :outcome ':condition
                                             :condition-type "configuration-error"
                                             :report (format nil "~(~A~) cannot be changed here." name))))
    request-id))

(defmethod backend-invoke ((backend scripted-backend) operation arguments)
  (let ((request-id (make-request-id)))
    (scripted--post backend 'transcript-event :operation ':notice
                                              :arguments (list ':text (format nil "Ran ~A~@[ with ~A~]" operation
                                                                              (and (plusp (length arguments)) arguments))))
    (scripted--post backend 'result-event :request-id request-id :outcome ':ok)
    request-id))

(defmethod backend-job-action ((backend scripted-backend) job-id action &key text)
  (declare (ignore job-id action text))
  (let ((request-id (make-request-id)))
    (scripted--post backend 'result-event :request-id request-id :outcome ':unsupported)
    request-id))
