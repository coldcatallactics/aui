(in-package #:aui)

;;;; -- History Tests --

(fiveam:def-suite history
  :description "The end of a long conversation log is read without reading the rest."
  :in          aui)

(fiveam:in-suite history)

(-> test-write-log (pathname integer &key (:first integer) (:batch integer)) null)
(defun test-write-log (pathname count &key (first 1) (batch 1))
  "Write a conversation log of COUNT records numbered from FIRST at PATHNAME, in the form Autolith writes.

Every record carries text with line breaks, and some of it reads like the
start of a record, as a conversation about conversation logs does. With BATCH
above one, that many records share each form, as a transaction writes them."
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (records          (loop for sequence from first below (+ first count)
                                    collect (list :message :seq sequence :time 4000000000 :role :user
                                                           :content (format nil "message ~D~%(:message :seq 7 :role :user)~%~A"
                                                                            sequence
                                                                            (make-string 300 :initial-element #\x))))))
        (format stream "~S~%" (list :conversation :version 1 :id "test" :created-at 4000000000
                                                  :directory "/tmp/" :model "m" :reasoning-effort "low"))
        (loop while records
              do (let ((form (loop repeat batch
                                   while records
                                   collect (pop records))))
                   (format stream "~S~%" (if (= batch 1)
                                             (first form)
                                             (list :sexp-store-batch :version 1 :records form))))))))
  nil)

(-> test-read-pages (list) list)
(defun test-read-pages (segments)
  "Return every page of the log SEGMENTS, newest segment first, as lists of sequence numbers, newest page first."
  (let ((cursor nil)
        (pages  nil))
    (loop
      (multiple-value-bind (records more-p next) (history--page segments :cursor cursor)
        (push (mapcar #'record-sequence records) pages)
        (unless more-p
          (return (nreverse pages)))
        (setf cursor next)))))

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
        (multiple-value-bind (earlier earlier-start) (history--window pathname start 4000)
          (fiveam:is (< earlier-start start))
          (fiveam:is (plusp (length earlier)))
          (fiveam:is (= (1- floor) (record-sequence (first (last earlier))))))))))

(fiveam:test a-window-wider-than-the-log-reads-it-all
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 12)
    (multiple-value-bind (records start) (history--window pathname (history--file-length pathname) 1000000)
      (fiveam:is (zerop start))
      (fiveam:is (= 12 (length records))))))

(fiveam:test pages-of-a-short-log-hold-every-record-once
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 1000)
    (let ((pages (test-read-pages (list pathname))))
      (fiveam:is (= *history-page-records* (length (first pages))))
      (fiveam:is (= 1000 (first (last (first pages)))))
      (fiveam:is (equal (loop for sequence from 1 to 1000 collect sequence)
                        (loop for page in (reverse pages) append page))))))

(fiveam:test pages-cross-from-one-part-of-a-log-to-the-part-before
  (uiop:with-temporary-file (:pathname older :type "sexp")
    (uiop:with-temporary-file (:pathname newer :type "sexp")
      (test-write-log older 700)
      (test-write-log newer 30 :first 701)
      (let ((pages (test-read-pages (list newer older))))
        (fiveam:is (every (lambda (page) (<= (length page) *history-page-records*)) pages))
        (fiveam:is (equal (loop for sequence from 1 to 730 collect sequence)
                          (loop for page in (reverse pages) append page)))))))

(fiveam:test a-page-never-divides-records-written-together
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (test-write-log pathname 1000 :batch 7)
    (let ((pages (test-read-pages (list pathname))))
      (fiveam:is (zerop (mod (1- (first (first pages))) 7)))
      (fiveam:is (equal (loop for sequence from 1 to 1000 collect sequence)
                        (loop for page in (reverse pages) append page))))))

(fiveam:test windows-too-narrow-for-a-page-still-read-every-part-of-a-log
  (uiop:with-temporary-file (:pathname older :type "sexp")
    (uiop:with-temporary-file (:pathname newer :type "sexp")
      (test-write-log older 100)
      (test-write-log newer 100 :first 101)
      (let ((*history-tail-octets*    2000)
            (*history-maximum-octets* 4000)
            (*history-scan-octets*    2000))
        (fiveam:is (equal (loop for sequence from 1 to 200 collect sequence)
                          (loop for page in (reverse (test-read-pages (list newer older))) append page)))))))
