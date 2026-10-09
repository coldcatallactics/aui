(in-package #:aui)

(fiveam:def-suite recent :in aui
  :description "The bounded reader over Autolith's conversation store.")

(fiveam:in-suite recent)

(-> write-forms (pathname list) null)
(defun write-forms (path forms)
  "Write FORMS as text to PATH, creating directories."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede :external-format :utf-8)
    (dolist (form forms)
      (if (stringp form)
          (write-string form stream)
          (with-standard-io-syntax (prin1 form stream)))
      (terpri stream)))
  nil)

(defmacro with-sample-store ((root) &body body)
  "Run BODY with *AUTOLITH-DATA-ROOT* and *AUTOLITH-STATE-ROOT* bound to a
fresh temporary ROOT holding sample conversations, removing it afterwards."
  `(let* ((,root (uiop:ensure-directory-pathname
                  (format nil "/tmp/aui-recent-test-~D/" (random 1000000 (make-random-state t)))))
          (*autolith-data-root* ,root)
          (*autolith-state-root* ,root))
     (unwind-protect
          (progn
            (populate-sample-store ,root)
            ,@body)
       (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore))))

(defvar *sample-picker-date* 0
  "The source write date written into the sample picker, later than any file date.")

(-> populate-sample-store (pathname) null)
(defun populate-sample-store (root)
  "Create one flat conversation, one segmented one with a picker, one corrupt
file, one empty directory and two localgroup records under ROOT."
  (flet ((path (relative) (merge-pathnames relative root)))
    (write-forms (path "conversations/a1flat01.sexp")
                 (list '(:conversation :version 1 :id "a1flat01" :created-at 100
                         :directory "/tmp/flat/" :model "gpt-5.6-luna" :reasoning-effort "low")
                       '(:message :seq 1 :time 150 :role :user :content "hi")))
    (write-forms (path "conversations/b2seg002/00000000000000000001.sexp")
                 (list "(:CONVERSATION :VERSION 2 :ID #1=\"b2seg002\" :CREATED-AT 200 :DIRECTORY #A((11) BASE-CHAR . \"/tmp/seg/x/\") :PROMPT-CACHE-KEY #1# :TITLE \"Old title\" :MODEL \"gpt-6-astra\" :LAST-ACTIVITY-AT NIL :USER-TURN-COUNT 0)"
                       '(:message :seq 1 :time 210 :role :user :content "segmented")))
    (write-forms (path "conversations/b2seg002/00000000000000000500.sexp")
                 (list '(:conversation :version 2 :id "b2seg002" :created-at 200 :directory "/tmp/seg/x/"
                         :model "gpt-6-astra" :chunk-start-sequence 500)))
    (setf *sample-picker-date* (+ (get-universal-time) 3600))
    (write-forms (path "conversation-picker/b2seg002.sexp")
                 (list (list :conversation-picker-metadata :version 3 :source-write-date *sample-picker-date*
                             :user-turn-count 7 :title "Segmented title" :preview "the latest words")))
    (write-forms (path "conversations/c3bad003.sexp") (list "(:conversation :version 1 :id \"c3bad003\" (unbalanced"))
    (write-forms (path "conversations/d4eval04.sexp") (list "(:conversation :version 1 :id #.(error \"evaluated\") :directory \"/tmp/\")"))
    (ensure-directories-exist (path "conversations/e5empty5/"))
    (write-forms (path "conversations/b2seg002.peers.sexp") (list '(:version 1 :session "b2seg002")))
    (write-forms (path "localgroup/b2seg002.sexp")
                 (list (list :localgroup-endpoint :version 1 :session-id "b2seg002"
                             :pid (sb-posix:getpid) :address "127.0.0.1" :port 1 :token "x")))
    (write-forms (path "localgroup/a1flat01.sexp")
                 (list '(:localgroup-endpoint :version 1 :session-id "a1flat01" :pid 999999999 :token "x"))))
  nil)

(fiveam:test store-entries-are-read-validated-and-ordered
  (with-sample-store (root)
    (let ((entries (recent-conversations-from-autolith)))
      (fiveam:is (equal '("b2seg002" "a1flat01") (mapcar #'conversation-entry-id entries)))
      (let ((segmented (first entries))
            (flat (second entries)))
        (fiveam:is (string= "Segmented title" (conversation-entry-title segmented)))
        (fiveam:is (string= "/tmp/seg/x/" (conversation-entry-directory segmented)))
        (fiveam:is (string= "b-2seg002" (conversation-entry-display-id segmented)))
        (fiveam:is (= *sample-picker-date* (conversation-entry-last-activity-at segmented)))
        (fiveam:is (= 7 (conversation-entry-user-turn-count segmented)))
        (fiveam:is (string= "the latest words" (conversation-entry-preview segmented)))
        (fiveam:is (conversation-entry-live-p segmented))
        (fiveam:is (not (conversation-entry-live-p flat)))
        (fiveam:is (string= "gpt-5.6-luna" (conversation-entry-model flat)))
        (fiveam:is (null (conversation-entry-title flat)))
        (fiveam:is (integerp (conversation-entry-last-activity-at flat)))))))

(fiveam:test reader-never-evaluates-and-skips-garbage
  (with-sample-store (root)
    (fiveam:is (null (read-first-form (merge-pathnames "conversations/c3bad003.sexp" root))))
    (fiveam:is (null (read-first-form (merge-pathnames "conversations/d4eval04.sexp" root))))
    (fiveam:is (null (read-first-form (merge-pathnames "conversations/missing.sexp" root))))
    (fiveam:is (null (conversation-header-path (merge-pathnames "conversations/e5empty5/" root))))))

(fiveam:test records-are-validated-plists
  (fiveam:is (plist-record-p '(:conversation :id "a") ':conversation))
  (fiveam:is (not (plist-record-p '(:conversation "a" :id) ':conversation)))
  (fiveam:is (not (plist-record-p "text" ':conversation)))
  (fiveam:is (string= "x" (record-string '(:conversation :id "x") ':id)))
  (fiveam:is (null (record-string '(:conversation :id 5) ':id)))
  (fiveam:is (= 3 (record-integer '(:conversation :pid 3) ':pid)))
  (fiveam:is (null (record-integer '(:conversation :pid -3) ':pid))))

(fiveam:test xdg-roots-fall-back-to-home
  (let ((*autolith-data-root* nil))
    (fiveam:is (search "autolith/" (namestring (autolith-data-root))))
    (fiveam:is (uiop:directory-pathname-p (autolith-data-root)))))

(fiveam:test an-empty-store-yields-no-entries
  (let ((*autolith-data-root* (uiop:ensure-directory-pathname "/tmp/aui-recent-nowhere/"))
        (*autolith-state-root* (uiop:ensure-directory-pathname "/tmp/aui-recent-nowhere/")))
    (fiveam:is (null (recent-conversations-from-autolith)))
    (fiveam:is (null (live-session-ids)))))
