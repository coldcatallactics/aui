(in-package #:aui)

;;;; -- Session Tests --

(fiveam:def-suite session
  :description "A session tells the interface of its attachment in the order the channel's history needs."
  :in          aui)

(fiveam:in-suite session)

(-> test-call-with-session (function) t)
(defun test-call-with-session (function)
  "Call FUNCTION with a session of a channel in an empty world of its own, its subscription and its queue."
  (let ((root (uiop:ensure-directory-pathname
               (uiop:run-program '("mktemp" "-d") :output '(:string :stripped t)))))
    (unwind-protect
         (let* ((configuration (autolith::configuration-create :config-root       root
                                                               :data-root         root
                                                               :state-root        root
                                                               :working-directory root
                                                               :durable-p         nil))
                (queue         (make-ui-queue))
                (session       (make-instance 'session
                                              :world      (make-instance 'world :configuration configuration)
                                              :queue      queue
                                              :generation 1
                                              :identifier "channel")))
           (funcall function
                    session
                    (make-instance 'session-subscription
                                   :session       session
                                   :identifier    "channel"
                                   :configuration configuration)
                    queue))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))

(fiveam:test what-arrived-while-attaching-follows-the-attachment-and-its-history
  (test-call-with-session
   (lambda (session subscription queue)
     (declare (ignore session))
     (autolith::channel-subscription-begin-attachment subscription)
     (autolith::channel-subscription-event subscription '(:text :sequence 5 :text "early"))
     (fiveam:is (null (ui-queue-drain queue)))
     (autolith::channel-subscription-finish-attachment
      subscription '(:attached :sequence 4 :record-sequence 0 :state (:state :state :idle)))
     (let ((events (ui-queue-drain queue)))
       (fiveam:is (equal '(attachment-event history-event channel-event)
                         (mapcar #'type-of events)))
       (fiveam:is (eq ':attached (attachment-event-state (first events))))
       (fiveam:is (equal '(:state :idle) (getf (attachment-event-detail (first events)) :status)))))))

(fiveam:test a-channel-that-goes-away-closes-the-session-once
  (test-call-with-session
   (lambda (session subscription queue)
     (autolith::channel-subscription-forget subscription)
     (autolith::channel-subscription-forget subscription)
     (fiveam:is (session-closed-p session))
     (fiveam:is (equal '(:closed) (mapcar #'attachment-event-state (ui-queue-drain queue)))))))
