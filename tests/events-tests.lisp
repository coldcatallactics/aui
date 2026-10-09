(in-package #:aui)

(fiveam:def-suite events :in aui
  :description "The event queue, coalescing and transcript event application.")

(fiveam:in-suite events)

(-> text-event (string string &key (:generation integer) (:session string)) transcript-event)
(defun text-event (id text &key (generation 1) (session "s"))
  "Return an append-text event for block ID."
  (make-instance 'transcript-event :backend-id "b" :generation generation :session-id session
                                   :operation ':append-text :arguments (list ':id id ':text text)))

(fiveam:test queue-is-fifo-and-stamps-sequences
  (let* ((wakeups 0)
         (queue (make-ui-queue :wakeup (lambda () (incf wakeups))))
         (first-event (ui-queue-push queue (make-instance 'turn-event :state ':running)))
         (second-event (ui-queue-push queue (make-instance 'turn-event :state ':idle))))
    (fiveam:is (= 2 wakeups))
    (fiveam:is (= 2 (ui-queue-count queue)))
    (fiveam:is (= 1 (ui-event-ingress-sequence first-event)))
    (fiveam:is (= 2 (ui-event-ingress-sequence second-event)))
    (fiveam:is (equal (list first-event second-event) (ui-queue-drain queue)))
    (fiveam:is (zerop (ui-queue-count queue)))
    (fiveam:is (null (ui-queue-drain queue)))))

(fiveam:test drain-honors-the-limit
  (let ((queue (make-ui-queue)))
    (dotimes (index 5)
      (ui-queue-push queue (make-instance 'turn-event :state ':running)))
    (let ((first-batch (ui-queue-drain queue :limit 2))
          (second-batch (ui-queue-drain queue :limit 10)))
      (fiveam:is (= 2 (length first-batch)))
      (fiveam:is (= 3 (length second-batch)))
      (fiveam:is (equal '(1 2) (mapcar #'ui-event-ingress-sequence first-batch)))
      (fiveam:is (equal '(3 4 5) (mapcar #'ui-event-ingress-sequence second-batch))))))

(fiveam:test pushes-from-threads-all-arrive
  (let* ((queue (make-ui-queue))
         (threads (loop for worker below 4
                        collect (bordeaux-threads:make-thread
                                 (lambda ()
                                   (dotimes (index 50)
                                     (ui-queue-push queue (make-instance 'turn-event :state ':running))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (let ((events (ui-queue-drain queue :limit 1000)))
      (fiveam:is (= 200 (length events)))
      (fiveam:is (equal (loop for index from 1 to 200 collect index)
                        (mapcar #'ui-event-ingress-sequence events))))))

(fiveam:test coalescing-merges-adjacent-text-for-one-block
  (let* ((events (list (text-event "a" "Hel") (text-event "a" "lo") (text-event "b" "!")
                       (text-event "a" " again")))
         (merged (ui-events-coalesce events)))
    (fiveam:is (= 3 (length merged)))
    (fiveam:is (string= "Hello" (getf (transcript-event-arguments (first merged)) ':text)))
    (fiveam:is (string= "!" (getf (transcript-event-arguments (second merged)) ':text)))
    (fiveam:is (string= " again" (getf (transcript-event-arguments (third merged)) ':text)))))

(fiveam:test coalescing-keeps-other-connections-apart
  (let ((merged (ui-events-coalesce (list (text-event "a" "x" :generation 1)
                                          (text-event "a" "y" :generation 2)))))
    (fiveam:is (= 2 (length merged)))))

(fiveam:test coalescing-drops-superseded-snapshots
  (let* ((old-state (make-instance 'state-event :backend-id "b" :generation 1 :session-id "s" :snapshot ':old))
         (approval (make-instance 'approval-event :backend-id "b" :generation 1 :session-id "s" :request-id "r"))
         (new-state (make-instance 'state-event :backend-id "b" :generation 1 :session-id "s" :snapshot ':new))
         (old-jobs (make-instance 'jobs-event :backend-id "b" :generation 1 :session-id "s" :kind ':snapshot))
         (delta (make-instance 'jobs-event :backend-id "b" :generation 1 :session-id "s" :kind ':delta))
         (merged (ui-events-coalesce (list old-state approval new-state old-jobs delta))))
    (fiveam:is (equal (list approval new-state old-jobs delta) merged))))

(fiveam:test transcript-events-apply-every-operation
  (let ((transcript (make-transcript)))
    (flet ((apply-operation (operation &rest arguments)
             (transcript-event-apply (make-instance 'transcript-event :operation operation
                                                                      :arguments arguments)
                                     transcript)))
      (apply-operation ':begin-replay)
      (fiveam:is (transcript-replaying-p transcript))
      (apply-operation ':append :class 'assistant-text :initargs '(:id "a"))
      (apply-operation ':append-text :id "a" :text "Hi")
      (apply-operation ':append :class 'tool-call
                                :initargs '(:call-id "1:c" :name "shell.run" :status :running))
      (apply-operation ':update-tool-call :call-id "1:c" :changes '(:status :completed :output "done"))
      (apply-operation ':replace-plan :entries '(("step" . :pending)))
      (apply-operation ':append :class 'approval :initargs '(:request-id "r"))
      (apply-operation ':decide-approval :request-id "r" :decision "once")
      (apply-operation ':notice :severity ':warning :text "slow" :key ':speed)
      (apply-operation ':end-replay)
      (fiveam:is (not (transcript-replaying-p transcript)))
      (fiveam:is (string= "Hi" (assistant-text-text (transcript-find transcript "a"))))
      (fiveam:is (eq ':completed (tool-call-status (transcript-find-tool-call transcript "1:c"))))
      (fiveam:is (equal '(("step" . :pending)) (plan-block-entries (transcript-last-block transcript 'plan-block))))
      (fiveam:is (string= "once" (approval-decision (transcript-find-approval transcript "r"))))
      (fiveam:is (= 5 (transcript-length transcript)))
      (apply-operation ':reset)
      (fiveam:is (zerop (transcript-length transcript))))))

(fiveam:test current-events-match-backend-and-generation
  (let ((event (make-instance 'turn-event :backend-id "b" :generation 3 :state ':idle)))
    (fiveam:is (ui-event-current-p event "b" 3))
    (fiveam:is (not (ui-event-current-p event "b" 4)))
    (fiveam:is (not (ui-event-current-p event "other" 3)))))
