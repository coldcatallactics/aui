(in-package #:aui)

;;;; -- Record Projection Tests --

(fiveam:def-suite records
  :description "Autolith's records and live events become the entries a person reads."
  :in          aui)

(fiveam:in-suite records)

(-> test-item-record (integer string) list)
(defun test-item-record (sequence wire-json)
  "Return a durable provider item record numbered SEQUENCE carrying WIRE-JSON."
  (list :provider-item :seq sequence :time 4000000000 :wire-json wire-json))

(defparameter *test-history*
  (list (list :message :seq 1 :time 4000000000 :role :user :content "Fix the build.")
        (test-item-record 2 "{\"type\":\"reasoning\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Look at the Makefile.\"}]}")
        (test-item-record 3 "{\"type\":\"function_call\",\"call_id\":\"call-a\",\"name\":\"shell_run\",\"arguments\":\"{\\\"command\\\":\\\"make\\\"}\"}")
        (list :tool-result :seq 4 :time 4000000001 :call-id "call-a" :tool "shell.run" :status :ok
                           :output "built" :real-microseconds 2500000)
        (test-item-record 5 "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"It builds now.\"}]}")
        (list :message :seq 6 :time 4000000002 :role :user :automatic-p t
                       :content "x" :job-completion (list :outcome :failure :job-id "job-7" :summary "exit 2"))
        (list :summary :seq 7 :time 4000000003 :through-seq 6 :content "summary"))
  "A short durable history in the shapes Autolith writes.")

(fiveam:test history-becomes-entries-in-order
  (let* ((projector (make-projector (make-transcript)))
         (entries   (records->entries projector *test-history*)))
    (fiveam:is (equal '(user-entry reasoning-entry tool-entry assistant-entry notice-entry marker-entry)
                      (mapcar #'type-of entries)))
    (fiveam:is (string= "Fix the build." (user-entry-text (first entries))))
    (fiveam:is (string= "Look at the Makefile." (reasoning-entry-text (second entries))))
    (fiveam:is (string= "It builds now." (assistant-entry-text (fourth entries))))))

(fiveam:test a-result-settles-its-call
  (let* ((projector (make-projector (make-transcript)))
         (tool      (third (records->entries projector *test-history*))))
    (fiveam:is (eq ':completed (tool-entry-status tool)))
    (fiveam:is (string= "built" (tool-entry-output tool)))
    (fiveam:is (= 5/2 (tool-entry-seconds tool)))
    (fiveam:is (equal '(("command" . "make")) (tool-entry-arguments tool)))))

(fiveam:test a-failed-job-reads-as-a-failure
  (let* ((projector (make-projector (make-transcript)))
         (notice    (fifth (records->entries projector *test-history*))))
    (fiveam:is (eq ':failure (notice-entry-tone notice)))
    (fiveam:is (search "job-7" (notice-entry-text notice)))
    (fiveam:is (string= "exit 2" (notice-entry-detail notice)))))

(fiveam:test streamed-text-grows-one-answer
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector '(:status :status :provider-request-started :details nil))
    (projector-apply projector '(:text :delta "Hello"))
    (projector-apply projector '(:text :delta ", world."))
    (fiveam:is (= 1 (transcript-length transcript)))
    (fiveam:is (string= "Hello, world." (assistant-entry-text (transcript-last transcript))))
    (fiveam:is (assistant-entry-streaming-p (transcript-last transcript)))
    (projector-apply projector '(:status :status :assistant-response-persisted
                                 :details (:text "Hello, world.")))
    (fiveam:is (= 1 (transcript-length transcript)))
    (fiveam:is (not (assistant-entry-streaming-p (transcript-last transcript))))))

(fiveam:test an-answer-that-does-not-stream-arrives-whole
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector '(:status :status :assistant-response-persisted :details (:text "All at once.")))
    (fiveam:is (string= "All at once." (assistant-entry-text (transcript-last transcript))))))

(fiveam:test a-live-tool-call-runs-then-settles
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector '(:text :delta "Checking."))
    (projector-apply projector '(:status :status :tool-call-started
                                 :details (:call-id "call-b" :tool "resource.read"
                                           :arguments "{\"uri\":\"workspace:README.md\"}")))
    (let ((tool (transcript-last transcript)))
      (fiveam:is (eq ':running (tool-entry-status tool)))
      (fiveam:is (equal '(("uri" . "workspace:README.md")) (tool-entry-arguments tool)))
      (projector-apply projector '(:status :status :tool-call-completed
                                   :details (:call-id "call-b" :tool "resource.read" :success-p nil
                                             :output "no such file")))
      (fiveam:is (eq ':failed (tool-entry-status tool)))
      (fiveam:is (string= "no such file" (tool-entry-output tool))))
    ;; Text after the call starts a new answer below it.
    (projector-apply projector '(:text :delta "It is missing."))
    (fiveam:is (= 3 (transcript-length transcript)))))

(fiveam:test live-records-do-not-repeat-the-turn
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (setf (projector-boundary projector) 4)
    ;; Already in history.
    (projector-apply projector (list :record :record (first *test-history*)))
    (fiveam:is (zerop (transcript-length transcript)))
    ;; Carried by the turn's own events.
    (projector-apply projector (list :record :record (fifth *test-history*)))
    (fiveam:is (zerop (transcript-length transcript)))
    ;; A new message from the person is taken from its record.
    (projector-apply projector
                     (list :record :record (list :message :seq 9 :role :user :content "And the tests?")))
    (fiveam:is (string= "And the tests?" (user-entry-text (transcript-last transcript))))))

(fiveam:test catching-up-takes-records-until-the-next-request
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (setf (projector-catch-up-p projector) t)
    (projector-apply projector '(:text :delta "half an answer"))
    (fiveam:is (zerop (transcript-length transcript)))
    (projector-apply projector (list :record :record (fifth *test-history*)))
    (fiveam:is (string= "It builds now." (assistant-entry-text (transcript-last transcript))))
    (projector-apply projector '(:status :status :provider-request-started :details nil))
    (projector-apply projector '(:text :delta "live again"))
    (fiveam:is (string= "live again" (assistant-entry-text (transcript-last transcript))))))

(fiveam:test command-output-joins-under-its-form
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector '(:work-started :kind :lisp))
    (projector-apply projector (list :present :text (format nil "* (status)~2%")))
    (projector-apply projector (list :present :text (format nil "autolith v1~%  model x~2%")))
    (projector-apply projector (list :present :text (format nil "⇒ NIL~2%")))
    (projector-apply projector '(:work-finished :kind :lisp :outcome :done))
    (fiveam:is (= 1 (transcript-length transcript)))
    (let ((output (transcript-last transcript)))
      (fiveam:is (string= "(status)" (output-entry-invocation output)))
      (fiveam:is (search "model x" (output-entry-text output)))
      ;; The NIL a printing command returns is not shown after its output.
      (fiveam:is (null (search "⇒ NIL" (output-entry-text output)))))
    ;; The next command starts its own entry.
    (projector-apply projector (list :present :text "settings"))
    (fiveam:is (= 2 (transcript-length transcript)))))

(fiveam:test a-form's-value-is-kept
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector (list :present :text (format nil "* (+ 1 2)~2%")))
    (projector-apply projector (list :present :text (format nil "⇒ 3~2%")))
    (fiveam:is (string= "⇒ 3" (output-entry-text (transcript-last transcript))))
    (projector-apply projector '(:work-finished :kind :lisp :outcome :done))
    (projector-apply projector (list :present :text (format nil "* (values)~2%")))
    (projector-apply projector (list :present :text (format nil "⇒ NIL~2%")))
    (fiveam:is (string= "⇒ NIL" (output-entry-text (transcript-last transcript))))))

(fiveam:test one-line-notices-read-as-notices
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector (list :present :text (format nil "∙ continuing with job results~%")))
    (projector-apply projector (list :present :text (format nil "! provider request failed~%")))
    (let ((entries (coerce (transcript-entries transcript) 'list)))
      (fiveam:is (equal '(notice-entry notice-entry) (mapcar #'type-of entries)))
      (fiveam:is (string= "continuing with job results" (notice-entry-text (first entries))))
      (fiveam:is (eq ':quiet (notice-entry-tone (first entries))))
      (fiveam:is (eq ':failure (notice-entry-tone (second entries)))))
    ;; Inside a command's output the same line stays part of the output.
    (projector-apply projector (list :present :text (format nil "* (tasks)~2%")))
    (projector-apply projector (list :present :text (format nil "∙ no jobs~%")))
    (fiveam:is (= 3 (transcript-length transcript)))
    (fiveam:is (string= "∙ no jobs" (output-entry-text (transcript-last transcript))))))

(fiveam:test portable-values-read-back
  (let ((object (portable-object "name" "model" "value" nil "list" '(:array 1 2))))
    (fiveam:is (string= "model" (portable-get object "name")))
    (fiveam:is (null (portable-get object "value")))
    (fiveam:is (eq ':missing (portable-get object "absent" ':missing)))
    (fiveam:is (equal '(1 2) (portable-items (portable-get object "list"))))
    (fiveam:is (portable-true-p t))
    (fiveam:is (not (portable-true-p ':false)))))

(fiveam:test command-text-names-its-command
  (dolist (case '(("/model" "model" nil)
                  ("(model)" "model" nil)
                  ("  (settings )" "settings" nil)
                  ("/model gpt-6" "model" t)
                  ("(open \"b-6\")" "open" t)
                  ("hello there" nil nil)
                  ("(" nil nil)))
    (destructuring-bind (text name arguments-p) case
      (multiple-value-bind (found found-arguments-p) (command-text-name text)
        (fiveam:is (equal name found) "~S names ~S" text found)
        (fiveam:is (eq arguments-p found-arguments-p) "~S arguments ~S" text found-arguments-p)))))

(fiveam:test a-stopped-turn-is-not-a-failure
  (let* ((projector (make-instance 'projector))
         (entries   (records->entries
                     projector
                     (list '(:turn-aborted :seq 1 :reason :cancelled
                             :message "#<APPLICATION-TURN-CANCELLED {1}>")
                           '(:turn-aborted :seq 2 :reason :application-error :message "the provider refused")))))
    (fiveam:is (equal '(notice-entry notice-entry) (mapcar #'type-of entries)))
    (fiveam:is (string= "Stopped" (notice-entry-text (first entries))))
    (fiveam:is (eq ':quiet (notice-entry-tone (first entries))))
    (fiveam:is (null (notice-entry-detail (first entries))))
    (fiveam:is (eq ':failure (notice-entry-tone (second entries))))
    (fiveam:is (string= "the provider refused" (notice-entry-detail (second entries))))))

(fiveam:test a-shell-result-shows-its-output-not-its-bookkeeping
  (multiple-value-bind (sections status)
      (tool-result-sections
       "shell.run"
       (format nil "exit 0~%Execution: exited; timeout no; cancelled no~%combined output: 26 bytes retained, 26 bytes observed; capture-complete yes; status complete~%Log: shell-log:abc/def/output~%hello from the mock~%Linux~%"))
    (fiveam:is (string= "exit 0" status))
    (fiveam:is (= 1 (length sections)))
    (fiveam:is (string= (format nil "hello from the mock~%Linux") (rest (first sections))))
    (fiveam:is (null (search "bytes retained" (rest (first sections))))))
  (multiple-value-bind (sections status) (tool-result-sections "resource.read" "the file's text")
    (fiveam:is (null status))
    (fiveam:is (equal '(("result" . "the file's text")) sections))))

(fiveam:test a-presented-reasoning-summary-is-a-thought-shown-once
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector (list ':reasoning :delta "**Checking model limit defaults**"))
    (projector-apply projector
                     (list ':present
                           :text (format nil "◇ reasoning summary~% │ Checking model limit defaults")))
    (projector-apply projector
                     (list ':present
                           :text (format nil "◇ reasoning summary~% │ Reading the provider file")))
    (let ((entries (coerce (transcript-entries transcript) 'list)))
      (fiveam:is (equal '(reasoning-entry reasoning-entry) (mapcar #'type-of entries)))
      (fiveam:is (string= "Reading the provider file" (reasoning-entry-text (second entries)))))))

(fiveam:test a-notice-does-not-join-output-nobody-asked-for
  (let* ((transcript (make-transcript))
         (projector  (make-projector transcript)))
    (projector-apply projector (list ':present :text "something the channel printed"))
    (projector-apply projector
                     (list ':present :text "! cache stalled · 153.0K / 191.2K input tokens cached"))
    (let ((entries (coerce (transcript-entries transcript) 'list)))
      (fiveam:is (equal '(output-entry notice-entry) (mapcar #'type-of entries)))
      (fiveam:is (search "cache stalled" (notice-entry-text (second entries)))))))

(fiveam:test a-wrapped-reasoning-summary-is-unwrapped
  (fiveam:is (string= (format nil "Planning the deadline capture~2%Checking Lisp apropos")
                      (projector--reasoning-summary
                       (format nil "◇ reasoning summary~%  │ Planning the deadline~%  │ capture~%  │~%  │ Checking Lisp apropos")))))

