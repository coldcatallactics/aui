(in-package #:aui)

(fiveam:def-suite transcript :in aui
  :description "The transcript block model and its notifications.")

(fiveam:in-suite transcript)

(defclass recording-listener ()
  ((changes :initform nil :accessor recording-listener-changes
            :documentation "Every (BLOCK-ID . CHANGE) received, newest first."))
  (:documentation "A listener that records what it is told, for tests."))

(defmethod transcript-listener-changed ((listener recording-listener) transcript block change)
  (declare (ignore transcript))
  (push (cons (and block (transcript-block-id block)) change)
        (recording-listener-changes listener)))

(-> recorded-changes (recording-listener) list)
(defun recorded-changes (listener)
  "Return the recorded changes oldest first."
  (reverse (recording-listener-changes listener)))

(fiveam:test append-assigns-sequence-and-id
  (let* ((transcript (make-transcript))
         (first-block (transcript-append transcript (make-instance 'user-message :text "hi")))
         (second-block (transcript-append transcript (make-instance 'assistant-text :id "answer"))))
    (fiveam:is (= 1 (transcript-block-sequence first-block)))
    (fiveam:is (= 2 (transcript-block-sequence second-block)))
    (fiveam:is (string= "user-message-1" (transcript-block-id first-block)))
    (fiveam:is (eq second-block (transcript-find transcript "answer")))
    (fiveam:is (= 2 (transcript-length transcript)))))

(fiveam:test duplicate-ids-are-rejected
  (let ((transcript (make-transcript)))
    (transcript-append transcript (make-instance 'notice :id "same"))
    (fiveam:signals transcript-duplicate-block
      (transcript-append transcript (make-instance 'notice :id "same")))))

(fiveam:test streaming-text-appends-and-notifies
  (let* ((transcript (make-transcript))
         (listener (transcript-add-listener transcript (make-instance 'recording-listener))))
    (transcript-append transcript (make-instance 'assistant-text :id "a"))
    (transcript-append-text transcript "a" "Hello, ")
    (transcript-append-text transcript "a" "world.")
    (fiveam:is (string= "Hello, world." (assistant-text-text (transcript-find transcript "a"))))
    (fiveam:is (equal '(("a" . :appended) ("a" . :text-appended) ("a" . :text-appended))
                      (recorded-changes listener)))))

(fiveam:test text-append-requires-a-text-block
  (let ((transcript (make-transcript)))
    (transcript-append transcript (make-instance 'user-message :id "u"))
    (fiveam:signals transcript-block-kind-error (transcript-append-text transcript "u" "x"))
    (fiveam:signals transcript-missing-block (transcript-append-text transcript "nowhere" "x"))))

(fiveam:test tool-calls-update-by-call-id
  (let* ((transcript (make-transcript))
         (listener (transcript-add-listener transcript (make-instance 'recording-listener))))
    (transcript-append transcript
                       (make-instance 'tool-call :id "t" :call-id "3:call_1" :name "shell.run"
                                                 :kind ':execute :status ':running))
    (let ((block (transcript-update-tool-call transcript "3:call_1"
                                              :status ':completed :raw-status "completed"
                                              :output "ok" :locations '("/tmp/x"))))
      (fiveam:is (eq ':completed (tool-call-status block)))
      (fiveam:is (string= "ok" (tool-call-output block)))
      (fiveam:is (equal '("/tmp/x") (tool-call-locations block)))
      (fiveam:is (transcript-block-collapsed-p block)))
    (fiveam:is (equal '(("t" . :appended) ("t" . :updated)) (recorded-changes listener)))
    (fiveam:signals transcript-missing-block
      (transcript-update-tool-call transcript "3:call_9" :status ':failed))))

(fiveam:test plan-is-replaced-in-place
  (let* ((transcript (make-transcript))
         (listener (transcript-add-listener transcript (make-instance 'recording-listener)))
         (first-plan (transcript-replace-plan transcript '(("read" . :completed) ("write" . :pending)))))
    (transcript-append transcript (make-instance 'assistant-text :id "later"))
    (let ((second-plan (transcript-replace-plan transcript '(("read" . :completed) ("write" . :completed)))))
      (fiveam:is (eq first-plan second-plan))
      (fiveam:is (= 2 (transcript-length transcript)))
      (fiveam:is (equal '(("read" . :completed) ("write" . :completed)) (plan-block-entries second-plan)))
      (fiveam:is (equal (list (cons (transcript-block-id first-plan) ':appended)
                              '("later" . :appended)
                              (cons (transcript-block-id first-plan) ':replaced))
                        (recorded-changes listener))))))

(fiveam:test empty-plans-stay-out-until-one-exists
  (let ((transcript (make-transcript)))
    (fiveam:is (null (transcript-replace-plan transcript nil)))
    (fiveam:is (zerop (transcript-length transcript)))
    (transcript-replace-plan transcript '(("a" . :pending)))
    (fiveam:is (not (null (transcript-replace-plan transcript nil))))
    (fiveam:is (= 1 (transcript-length transcript)))))

(fiveam:test approvals-decide-once
  (let* ((transcript (make-transcript))
         (approval (transcript-append transcript
                                      (make-instance 'approval :request-id "r1" :title "shell.run"
                                                               :options '(("once" "Allow once" :allow-once)
                                                                          ("deny" "Reject" :reject-once))))))
    (fiveam:is (approval-pending-p approval))
    (fiveam:is (equal (list approval) (transcript-pending-approvals transcript)))
    (transcript-decide-approval transcript "r1" "once")
    (transcript-decide-approval transcript "r1" ':cancelled)
    (fiveam:is (string= "once" (approval-decision approval)))
    (fiveam:is (null (transcript-pending-approvals transcript)))
    (fiveam:signals transcript-missing-block (transcript-decide-approval transcript "r2" "once"))))

(fiveam:test notices-deduplicate-by-key
  (let* ((transcript (make-transcript))
         (first-notice (transcript-notice transcript :severity ':warning :text "Connection lost" :key ':connection))
         (second-notice (transcript-notice transcript :severity ':information :text "Connected" :key ':connection))
         (third-notice (transcript-notice transcript :text "Compacted")))
    (fiveam:is (eq first-notice second-notice))
    (fiveam:is (string= "Connected" (notice-text first-notice)))
    (fiveam:is (eq ':information (notice-severity first-notice)))
    (fiveam:is (not (eq first-notice third-notice)))
    (fiveam:is (= 2 (transcript-length transcript)))))

(fiveam:test reset-starts-a-new-incarnation
  (let* ((transcript (make-transcript))
         (listener (transcript-add-listener transcript (make-instance 'recording-listener))))
    (transcript-append transcript (make-instance 'user-message :id "u"))
    (transcript-begin-replay transcript)
    (fiveam:is (transcript-replaying-p transcript))
    (transcript-end-replay transcript)
    (transcript-reset transcript)
    (fiveam:is (= 2 (transcript-incarnation transcript)))
    (fiveam:is (zerop (transcript-length transcript)))
    (fiveam:is (null (transcript-find transcript "u")))
    (let ((again (transcript-append transcript (make-instance 'user-message :id "u"))))
      (fiveam:is (= 1 (transcript-block-sequence again))))
    (fiveam:is (equal '(("u" . :appended) (nil . :replay-begin) (nil . :replay-end)
                        (nil . :reset) ("u" . :appended))
                      (recorded-changes listener)))))

(fiveam:test listeners-can-be-removed
  (let* ((transcript (make-transcript))
         (listener (transcript-add-listener transcript (make-instance 'recording-listener))))
    (transcript-remove-listener transcript listener)
    (transcript-append transcript (make-instance 'notice :text "quiet"))
    (fiveam:is (null (recorded-changes listener)))))
