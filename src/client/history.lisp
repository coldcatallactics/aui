(in-package #:aui)

;;;; -- History --

;;; Read the newest part of a channel's durable history without reading all of
;;; it. A long-lived conversation runs to hundreds of megabytes; the interface
;;; shows its last few hundred records and fetches earlier pages on request,
;;; as the terminal does with (history). Records are parsed by Autolith's own
;;; log reader; this file only decides where in a segment to start it.

(defparameter *history-page-records* 400
  "How many durable records one page of history holds at most.")

(defparameter *history-tail-octets* (* 3 1024 1024)
  "How much of a segment's end is examined first when the segment is longer than this.")

(defparameter *history-record-kinds*
  '(:message :provider-item :provider :tool-result :summary :configuration :native-compaction
    :user-operation :side-status :assistant-yield :turn-aborted :async-lisp-event
    :async-lisp-delivered)
  "Record kinds that mark a plausible record start when resynchronizing inside a segment.")

(-> history--file-length (pathname) integer)
(defun history--file-length (pathname)
  "Return the length of the file at PATHNAME in octets."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (file-length stream)))

(-> history--line-starts (pathname integer) list)
(defun history--line-starts (pathname from)
  "Return the octet positions at or after FROM in PATHNAME where a line opens a keyword-headed form, in order."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (buffer (make-array (- length from) :element-type '(unsigned-byte 8)))
           (starts nil))
      (file-position stream from)
      (read-sequence buffer stream)
      (loop for index from 0 below (- (length buffer) 2)
            do (when (and (= (aref buffer index) 10)
                          (= (aref buffer (+ index 1)) 40)
                          (= (aref buffer (+ index 2)) 58))
                 (push (+ from index 1) starts)))
      (nreverse starts))))

(-> history--read-from (pathname integer) (values list boolean))
(defun history--read-from (pathname position)
  "Return the records of PATHNAME from octet POSITION on, and whether they read as a well-formed run.

A run is well formed when every form is a record of a known kind and the
sequence numbers never fall, which a start inside a string or a record's
middle soon fails."
  (let ((records nil)
        (last    0)
        (sound-p t))
    (handler-case
        (autolith::conversation--map-records
         pathname
         (lambda (record)
           (let ((sequence (record-sequence record)))
             (cond
               ((eq (first record) ':conversation)
                nil)
               ((and (member (first record) *history-record-kinds*)
                     (or (null sequence) (>= sequence last)))
                (when sequence
                  (setf last sequence))
                (push record records))
               (t
                (setf sound-p nil)))))
         :start-position position)
      (error ()
        (setf sound-p nil)))
    (values (nreverse records) sound-p)))

(-> history--segment-tail (pathname integer) (values list boolean))
(defun history--segment-tail (pathname octets)
  "Return the records in about the last OCTETS of the segment PATHNAME, and whether the segment holds earlier ones."
  (let ((length (history--file-length pathname)))
    (if (<= length octets)
        (values (history--read-from pathname 0) nil)
        (let ((starts (history--line-starts pathname (- length octets))))
          (loop for start in starts
                do (multiple-value-bind (records sound-p) (history--read-from pathname start)
                     (when (and sound-p records)
                       (return (values records t))))
                finally (return (values (history--read-from pathname 0) nil)))))))

(-> history-read-page
    (t string &key (:before (or null integer)) (:through (or null integer)) (:limit integer))
    (values list boolean))
(defun history-read-page (configuration identifier &key before through (limit *history-page-records*))
  "Return up to LIMIT of channel IDENTIFIER's newest durable records, oldest first, and whether older ones remain.

BEFORE keeps only records whose sequence is below it, for a page earlier than
one already shown; THROUGH keeps only records at or below it, the boundary a
live attachment takes over from. A conversation with no durable history yet
yields no records."
  (let ((conversation (handler-case (autolith::conversation-replay-load configuration identifier)
                        (error ()
                          nil))))
    (if (null conversation)
        (values nil nil)
        (let ((segments (reverse (autolith::conversation-storage-pathnames
                                  (autolith::conversation-pathname conversation))))
              (page     nil)
              (more-p   nil))
          (flet ((wanted-p (record)
                   (let ((sequence (record-sequence record)))
                     (and (or (null before) (null sequence) (< sequence before))
                          (or (null through) (null sequence) (<= sequence through))))))
            (loop for segment in segments
                  for remaining = (rest segments) then (rest remaining)
                  do (let ((octets *history-tail-octets*))
                       (loop
                         (multiple-value-bind (records earlier-p) (history--segment-tail segment octets)
                           (let ((kept (remove-if-not #'wanted-p records)))
                             (when (or (>= (+ (length kept) (length page)) limit)
                                       (not earlier-p))
                               (setf page   (append kept page)
                                     more-p (or earlier-p (and remaining t)))
                               (return))
                             (setf octets (* octets 4))))))
                     (when (>= (length page) limit)
                       (return))))
          (let ((excess (- (length page) limit)))
            (if (plusp excess)
                (values (nthcdr excess page) t)
                (values page more-p)))))))

(-> history-image-root (t string) (or null pathname))
(defun history-image-root (configuration identifier)
  "Return the directory holding channel IDENTIFIER's image artifacts, or NIL when it has no durable history."
  (handler-case
      (autolith::conversation-image-artifact-root
       (autolith::conversation-replay-load configuration identifier))
    (error ()
      nil)))
