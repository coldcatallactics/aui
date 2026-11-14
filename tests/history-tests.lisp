(in-package #:aui)

;;;; -- History Tests --

(fiveam:def-suite history
  :description "The end of a long conversation log is read without reading the rest."
  :in          aui)

(fiveam:in-suite history)

(-> test-write-log (pathname integer) null)
(defun test-write-log (pathname count)
  "Write a conversation log of COUNT records at PATHNAME, in the form Autolith writes.

Every record carries text with line breaks, and some of it reads like the
start of a record, as a conversation about conversation logs does."
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (format stream "~S~%" (list :conversation :version 1 :id "test" :created-at 4000000000
                                                  :directory "/tmp/" :model "m" :reasoning-effort "low"))
        (loop for sequence from 1 to count
              do (format stream "~S~%"
                         (list :message :seq sequence :time 4000000000 :role :user
                                        :content (format nil "message ~D~%(:message :seq 7 :role :user)~%~A"
                                                         sequence
                                                         (make-string 300 :initial-element #\x))))))))
  nil)

(fiveam:test a-window-at-the-end-holds-the-newest-records
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 200)
    (multiple-value-bind (records start) (history--window pathname (history--file-length pathname) 4000)
      (fiveam:is (plusp start))
      (fiveam:is (plusp (length records)))
      (fiveam:is (< (length records) 200))
      (fiveam:is (= 200 (record-sequence (first (last records)))))
      ;; The run is contiguous: nothing was skipped and nothing read from inside a string.
      (fiveam:is (equal (loop for sequence from (record-sequence (first records)) to 200 collect sequence)
                        (mapcar #'record-sequence records))))))

(fiveam:test a-window-before-a-page-stops-at-it
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 200)
    (multiple-value-bind (newest start) (history--window pathname (history--file-length pathname) 4000)
      (let ((floor (record-sequence (first newest))))
        (multiple-value-bind (earlier earlier-start) (history--window pathname start 4000 :below floor)
          (fiveam:is (< earlier-start start))
          (fiveam:is (plusp (length earlier)))
          (fiveam:is (= (1- floor) (record-sequence (first (last earlier))))))))))

(fiveam:test a-window-wider-than-the-log-reads-it-all
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 12)
    (multiple-value-bind (records start) (history--window pathname (history--file-length pathname) 1000000)
      (fiveam:is (zerop start))
      (fiveam:is (= 12 (length records))))))
