(in-package #:aui)

;;;; -- Transcript Tests --

(fiveam:def-suite transcript
  :description "The transcript model keeps order, identity and revisions."
  :in          aui)

(fiveam:in-suite transcript)

(fiveam:test entries-keep-their-order-and-identity
  (let* ((transcript (make-transcript))
         (first      (transcript-append transcript (make-instance 'user-entry :text "one")))
         (second     (transcript-append transcript (make-instance 'assistant-entry :text "two"))))
    (fiveam:is (= 2 (transcript-length transcript)))
    (fiveam:is (eq first (transcript-entry transcript 0)))
    (fiveam:is (eq second (transcript-last transcript)))
    (fiveam:is (eq first (transcript-find transcript (entry-identifier first))))
    (fiveam:is (string/= (entry-identifier first) (entry-identifier second)))))

(fiveam:test tool-entries-are-found-by-call
  (let* ((transcript (make-transcript))
         (tool       (transcript-append transcript
                                        (make-instance 'tool-entry :call-identifier "call-1" :name "shell.run"))))
    (fiveam:is (eq tool (transcript-find-tool transcript "call-1")))
    (fiveam:is (null (transcript-find-tool transcript "call-2")))))

(fiveam:test touching-an-entry-moves-both-revisions
  (let* ((transcript (make-transcript))
         (entry      (transcript-append transcript (make-instance 'assistant-entry)))
         (before     (transcript-revision transcript)))
    (transcript-touch transcript entry)
    (fiveam:is (= 1 (entry-revision entry)))
    (fiveam:is (> (transcript-revision transcript) before))))

(fiveam:test earlier-history-goes-in-front
  (let* ((transcript (make-transcript))
         (newest     (transcript-append transcript (make-instance 'user-entry :text "new")))
         (older      (list (make-instance 'user-entry :text "old 1")
                           (make-instance 'user-entry :text "old 2"))))
    (fiveam:is (= 2 (transcript-prepend transcript older)))
    (fiveam:is (equal '("old 1" "old 2" "new")
                      (map 'list #'user-entry-text (transcript-entries transcript))))
    (fiveam:is (eq newest (transcript-last transcript)))
    (fiveam:is (eq (first older) (transcript-find transcript (entry-identifier (first older)))))))

(fiveam:test clearing-forgets-everything
  (let ((transcript (make-transcript)))
    (transcript-append transcript (make-instance 'tool-entry :call-identifier "call-1"))
    (transcript-clear transcript)
    (fiveam:is (zerop (transcript-length transcript)))
    (fiveam:is (null (transcript-find-tool transcript "call-1")))
    (fiveam:is (null (transcript-last transcript)))))
