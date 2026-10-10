(in-package #:aui)

(fiveam:def-suite document :in aui
  :description "Blocks rendered to runs and byte spans.")

(fiveam:in-suite document)

(fiveam:test styles-have-indices-faces-and-colors
  (fiveam:is (= 0 (document-style-index ':body)))
  (fiveam:is (eq ':prose-bold (document-style-face ':heading-1)))
  (fiveam:is (equal *ink* (document-style-color ':body)))
  (fiveam:is (equal (accent ':blue) (document-style-color ':link)))
  (fiveam:is (document-style-underline-p ':link))
  (fiveam:is (= *heading-size* (document-style-size ':heading-1)))
  (fiveam:is (= *subheading-size* (document-style-size ':heading-2)))
  (fiveam:is (= *meta-size* (document-style-size ':meta)))
  (fiveam:is (= *code-size* (document-style-size ':code-block)))
  (fiveam:is (not (document-style-underline-p ':body)))
  (fiveam:signals error (document-style-index ':nonsense)))

(fiveam:test user-and-assistant-blocks-show-heads-and-markdown
  (let ((user-runs (block-runs (make-instance 'user-message :text "Hello **there**" :received-at 0)))
        (assistant-runs (block-runs (make-instance 'assistant-text :text "Sure." :received-at 0))))
    ;; A blank line separates a message from whatever stands above it.
    (fiveam:is (string= (string #\Newline) (run-text (first user-runs))))
    (fiveam:is (eq ':user-head (run-style (second user-runs))))
    (fiveam:is (string= "You" (run-text (second user-runs))))
    (fiveam:is (find "there" user-runs :key #'run-text :test #'string=))
    (fiveam:is (eq ':assistant-head (run-style (second assistant-runs))))
    (fiveam:is (search "Sure." (runs-text assistant-runs)))))

(fiveam:test collapsed-reasoning-shows-one-line
  (let* ((block (make-instance 'reasoning :text (format nil "First thought.~%Second thought.") :received-at 0))
         (collapsed (runs-text (block-runs block))))
    (fiveam:is (search "First thought." collapsed))
    (fiveam:is (not (search "Second thought." collapsed)))
    (setf (transcript-block-collapsed-p block) nil)
    (fiveam:is (search "Second thought." (runs-text (block-runs block))))))

(fiveam:test tool-calls-show-status-and-bounded-output
  (let* ((output (format nil "~{line ~D~%~}" (loop for index from 1 to 50 collect index)))
         (block (make-instance 'tool-call :call-id "1:c" :name "shell.run" :title "shell.run · make"
                                          :status ':completed :output output :received-at 0
                                          :collapsed-p nil))
         (text (runs-text (block-runs block))))
    (fiveam:is (search "completed" text))
    (fiveam:is (search "line 40" text))
    (fiveam:is (not (search "line 41" text)))
    (fiveam:is (search "10 more lines" text))
    (setf (transcript-block-collapsed-p block) t)
    (fiveam:is (not (search "line 1" (runs-text (block-runs block)))))
    (setf (tool-call-status block) ':failed)
    (fiveam:is (find ':failure (block-runs block) :key #'run-style))))

(fiveam:test approvals-record-only-refusals
  (let* ((block (make-instance 'approval :request-id "r" :title "shell.run" :received-at 0
                                         :arguments "{\"command\": \"ls\"}"
                                         :options '(("once" "Allow once" :allow-once)
                                                    ("deny" "Reject" :reject-once))))
         (pending (runs-text (block-runs block))))
    (fiveam:is (search "command: ls" pending))
    (fiveam:is (null (search "Allow once" pending)))
    (setf (approval-decision block) "once")
    (fiveam:is (null (search "Allow once" (runs-text (block-runs block)))))
    (setf (approval-decision block) "deny")
    (fiveam:is (search "Reject" (runs-text (block-runs block))))
    (setf (approval-decision block) ':cancelled)
    (fiveam:is (search "Cancelled" (runs-text (block-runs block))))))

(fiveam:test plans-and-notices-render
  (let ((plan (plan-entry-text '(("read" . :completed) ("write" . :in-progress))))
        (notice (block-runs (make-instance 'notice :severity ':failure :text "Connection lost" :received-at 0))))
    (fiveam:is (null (block-runs (make-instance 'plan-block :received-at 0 :entries '(("read" . :completed))))))
    (fiveam:is (search "● read" plan))
    (fiveam:is (search "◐ write" plan))
    (fiveam:is (string= "Failure" (run-text (second notice))))
    (fiveam:is (eq ':failure (run-style (second notice))))))

(fiveam:test document-spans-follow-byte-lengths
  (let* ((document (make-document))
         (first-block (make-instance 'user-message :id "u" :text "Příliš" :received-at 0))
         (second-block (make-instance 'assistant-text :id "a" :text "ok" :received-at 0))
         (first-runs (document-append document first-block))
         (second-runs (document-append document second-block))
         (first-span (document-span document "u"))
         (second-span (document-span document "a")))
    (fiveam:is (= (runs-byte-length first-runs) (block-span-end first-span)))
    (fiveam:is (= (block-span-end first-span) (block-span-start second-span)))
    (fiveam:is (= (+ (runs-byte-length first-runs) (runs-byte-length second-runs))
                  (document-length document)))
    (fiveam:is (string= "u" (document-block-at document 0)))
    (fiveam:is (string= "a" (document-block-at document (block-span-start second-span))))
    (fiveam:is (null (document-block-at document (document-length document))))))

(fiveam:test rerendering-shifts-later-spans
  (let* ((document (make-document))
         (first-block (make-instance 'assistant-text :id "a" :text "short" :received-at 0))
         (second-block (make-instance 'notice :id "n" :text "after" :received-at 0)))
    (document-append document first-block)
    (document-append document second-block)
    (let ((old-second-start (block-span-start (document-span document "n"))))
      (setf (assistant-text-text first-block) "a much longer piece of text")
      (multiple-value-bind (runs start end) (document-rerender document first-block)
        (fiveam:is (zerop start))
        (fiveam:is (= end old-second-start))
        (fiveam:is (= (runs-byte-length runs) (block-span-end (document-span document "a"))))
        (fiveam:is (= (block-span-end (document-span document "a"))
                      (block-span-start (document-span document "n"))))))
    (fiveam:signals error (document-rerender document (make-instance 'notice :id "missing")))
    (document-clear document)
    (fiveam:is (zerop (document-length document)))))

(fiveam:test tool-data-reads-as-fields
  (fiveam:is (string= (format nil "command: ls~%directory: /tmp") (readable-data-text "{\"directory\": \"/tmp\", \"command\": \"ls\"}")))
  (fiveam:is (string= "plain output" (readable-data-text "plain output")))
  (fiveam:is (string= (format nil "- a~%- 2") (readable-data-text "[\"a\", 2]")))
  (fiveam:is (string= (format nil "exit: 0~%output:~%  stdout: one~%    two")
                      (readable-data-text "{\"output\": {\"stdout\": \"one\\ntwo\"}, \"exit\": 0}"))))
