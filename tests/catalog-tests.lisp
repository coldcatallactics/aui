(in-package #:aui)

(fiveam:def-suite catalog :in aui
  :description "Catalog entries and their ordering helpers.")

(fiveam:in-suite catalog)

(-> conversation (string string &key (:activity (or null integer)) (:live-p boolean)) conversation-entry)
(defun conversation (id directory &key activity live-p)
  "Return a conversation entry for tests."
  (make-instance 'conversation-entry :id id :directory directory
                                     :last-activity-at activity :live-p live-p))

(fiveam:test conversations-sort-by-recency
  (let* ((old (conversation "old" "/a" :activity 100))
         (new (conversation "new" "/a" :activity 300))
         (undated (conversation "undated" "/b"))
         (live (conversation "live" "/b" :activity 300 :live-p t))
         (sorted (conversations-by-recency (list old undated new live))))
    (fiveam:is (equal '("live" "new" "old" "undated") (mapcar #'conversation-entry-id sorted)))))

(fiveam:test recent-directories-keep-the-latest-time
  (let ((pairs (recent-directories (list (conversation "1" "/a" :activity 100)
                                         (conversation "2" "/b" :activity 250)
                                         (conversation "3" "/a" :activity 200)))))
    (fiveam:is (equal '(("/b" . 250) ("/a" . 200)) pairs))))

(fiveam:test context-fraction-is-nil-when-unknown
  (fiveam:is (null (state-snapshot-context-fraction (make-instance 'state-snapshot))))
  (fiveam:is (= 1/4 (state-snapshot-context-fraction
                     (make-instance 'state-snapshot :usage '(:context-used 25 :context-limit 100)))))
  (fiveam:is (null (state-snapshot-context-fraction
                    (make-instance 'state-snapshot :usage '(:context-used 25 :context-limit 0))))))

(fiveam:test key-events-parse-defensively
  (fiveam:is (equal '(65307 0) (multiple-value-list (key-event-parts "65307 0 "))))
  (fiveam:is (equal '(72 262144) (multiple-value-list (key-event-parts "72 262144 H"))))
  (fiveam:is (equal '(nil 0) (multiple-value-list (key-event-parts "H"))))
  (fiveam:is (equal '(nil 0) (multiple-value-list (key-event-parts "")))))

(fiveam:test display-ids-carry-the-hyphen
  (fiveam:is (string= "9-3tEGL1" (conversation-display-id "93tEGL1")))
  (fiveam:is (string= "9-3tEGL1" (conversation-display-id "9-3tEGL1")))
  (fiveam:is (string= "x" (conversation-display-id "x")))
  (fiveam:is (string= "9-3tEGL1" (conversation-entry-name (make-instance 'conversation-entry :id "93tEGL1" :directory "/"))))
  (fiveam:is (string= "Howdy" (conversation-entry-name (make-instance 'conversation-entry :id "93tEGL1" :directory "/" :title "Howdy")))))

(fiveam:test composer-line-and-word-helpers
  (fiveam:is (equal '(0 5) (multiple-value-list (line-bounds "hello" 2))))
  (fiveam:is (equal '(6 11) (multiple-value-list (line-bounds (format nil "hello~%world") 8))))
  (fiveam:is (= 0 (word-boundary "hello world" 5 ':backward)))
  (fiveam:is (= 11 (word-boundary "hello world" 5 ':forward)))
  (fiveam:is (= 5 (word-boundary "hello world" 0 ':forward)))
  (fiveam:is (= 11 (word-boundary "hello world" 6 ':forward)))
  (fiveam:is (= 6 (word-boundary "hello world" 11 ':backward))))
