(in-package #:aui)

(fiveam:def-suite scripted-backend :in aui
  :description "The scripted backend plays the event contract end to end, headless.")

(fiveam:in-suite scripted-backend)

(-> drain-into (ui-queue transcript) list)
(defun drain-into (queue transcript)
  "Apply pending transcript events to TRANSCRIPT, append approval blocks for
approval events as the workbench does, and return every non-transcript event."
  (let ((others nil))
    (dolist (event (ui-events-coalesce (ui-queue-drain queue :limit 10000)))
      (typecase event
        (transcript-event
         (transcript-event-apply event transcript))
        (approval-event
         (transcript-append transcript (make-instance 'approval
                                                      :request-id (approval-event-request-id event)
                                                      :title (approval-event-title event)
                                                      :options (approval-event-options event)))
         (push event others))
        (t
         (push event others))))
    (nreverse others)))

(-> wait-for (function &key (:seconds real)) boolean)
(defun wait-for (predicate &key (seconds 10))
  "Poll PREDICATE every few milliseconds for up to SECONDS; return its truth."
  (loop repeat (ceiling seconds 0.02)
        when (funcall predicate)
          do (return t)
        do (sleep 0.02)
        finally (return nil)))

(fiveam:test scripted-turn-plays-through-an-approval
  (let* ((queue (make-ui-queue))
         (backend (make-scripted-backend "/tmp/demo/" queue))
         (transcript (make-transcript))
         (events nil))
    (flet ((collect ()
             (setf events (append events (drain-into queue transcript)))
             events))
      (backend-start backend)
      (backend-open-conversation backend :conversation-id "93tEGL1")
      (collect)
      (fiveam:is (find-if (lambda (event) (and (typep event 'connection-event) (eq ':ready (connection-event-state event)))) events))
      (fiveam:is (find-if (lambda (event) (and (typep event 'session-event) (eq ':ready (session-event-state event)))) events))
      (fiveam:is (find-if (lambda (event) (and (typep event 'catalog-event) (eq ':settings (catalog-event-kind event)))) events))
      (fiveam:is (plusp (transcript-length transcript)))
      (backend-prompt backend "hello")
      (fiveam:is (wait-for (lambda ()
                             (collect)
                             (find-if (lambda (event) (typep event 'approval-event)) events))))
      (let ((approval (find-if (lambda (event) (typep event 'approval-event)) events)))
        (fiveam:is (= 3 (length (approval-event-options approval))))
        (backend-decide-approval backend (approval-event-request-id approval) "once"))
      (fiveam:is (wait-for (lambda ()
                             (collect)
                             (find-if (lambda (event) (and (typep event 'turn-event) (eq ':idle (turn-event-state event)))) events))))
      (let ((tool (transcript-find-tool-call transcript "1:call_demo"))
            (answer (transcript-last-block transcript 'assistant-text)))
        (fiveam:is (eq ':completed (tool-call-status tool)))
        (fiveam:is (search "157 checks" (assistant-text-text answer))))
      (backend-stop backend)
      (collect)
      (fiveam:is (find-if (lambda (event) (and (typep event 'connection-event) (eq ':exited (connection-event-state event)))) events)))))

(fiveam:test scripted-turn-can-be-cancelled
  (let* ((queue (make-ui-queue))
         (backend (make-scripted-backend "/tmp/demo/" queue))
         (transcript (make-transcript))
         (events nil))
    (flet ((collect ()
             (setf events (append events (drain-into queue transcript)))
             events))
      (backend-start backend)
      (backend-open-conversation backend)
      (backend-prompt backend "hello")
      (fiveam:is (wait-for (lambda () (collect) (find-if (lambda (event) (typep event 'approval-event)) events))))
      (backend-cancel backend)
      (fiveam:is (wait-for (lambda ()
                             (collect)
                             (find-if (lambda (event) (and (typep event 'turn-event) (eq ':idle (turn-event-state event))
                                                           (eq ':cancelled (turn-event-stop-reason event))))
                                      events))))
      (fiveam:is (null (transcript-last-block transcript 'assistant-text)))
      (backend-stop backend))))

(fiveam:test links-are-found-by-position
  (let* ((document (make-document))
         (block (make-instance 'assistant-text :id "a" :received-at 0
                                               :text "See [docs](https://example.org/d) and [more](https://example.org/m).")))
    (document-append document block)
    (let* ((span (document-span document "a"))
           (links (block-span-links span)))
      (fiveam:is (= 2 (length links)))
      (fiveam:is (string= "https://example.org/d" (document-link-at document (first (first links)))))
      (fiveam:is (string= "https://example.org/m" (document-link-at document (first (second links)))))
      (fiveam:is (null (document-link-at document 0))))))
