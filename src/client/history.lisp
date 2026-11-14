(in-package #:aui)

;;;; -- History --

;;; Read the newest part of a channel's durable history without reading all of
;;; it. A long-lived conversation runs to hundreds of megabytes; the interface
;;; shows its last few hundred records and fetches earlier pages on request,
;;; as the terminal does with (history). Records are parsed by Autolith's own
;;; log reader; this file only decides where in a segment to start it.

(defparameter *history-page-records* 400
  "How many durable records one page of history holds at most.")

(defparameter *history-tail-octets* (* 2 1024 1024)
  "How much of a segment's end one page reads at first; the window doubles while it holds too few records.")

(defparameter *history-minimum-records* 60
  "How many records a window should hold before it is taken as a page; a window with fewer is widened.")

(defparameter *history-scan-octets* (* 512 1024)
  "How far past a window's start a record boundary is looked for.")

(defparameter *history-maximum-octets* (* 16 1024 1024)
  "The widest window one page is allowed to read, however few records it holds.")

(defstruct history-cursor
  "Where the next earlier page of history ends, as the SEGMENT file and the octet OFFSET within it."
  (segment nil :type (or null pathname))
  (offset  0   :type integer))

(-> history--file-length (pathname) integer)
(defun history--file-length (pathname)
  "Return the length of the file at PATHNAME in octets."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (file-length stream)))

(-> history--line-starts (pathname integer integer) list)
(defun history--line-starts (pathname from to)
  "Return the octet positions between FROM and TO in PATHNAME where a line opens a keyword-headed form, in order."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((end    (min to (file-length stream)))
           (buffer (make-array (max 0 (- end from)) :element-type '(unsigned-byte 8)))
           (starts nil))
      (file-position stream from)
      (read-sequence buffer stream)
      (loop for index from 0 below (- (length buffer) 2)
            do (when (and (= (aref buffer index) 10)
                          (= (aref buffer (+ index 1)) 40)
                          (= (aref buffer (+ index 2)) 58))
                 (push (+ from index 1) starts)))
      (nreverse starts))))

(-> history--read-from (pathname integer &key (:below (or null integer))) (values list boolean))
(defun history--read-from (pathname position &key below)
  "Return the records of PATHNAME from octet POSITION on, and whether they read as a well-formed run.

Reading stops at the first record whose sequence reaches BELOW, so a page
before one already shown does not read what follows it. A run is well formed
when every form has a record's shape and the sequence numbers never fall,
which a start inside a string or a record's middle soon fails."
  (let ((records nil)
        (last    0)
        (sound-p t))
    (block reading
      (handler-case
          (autolith::conversation--map-records
           pathname
           (lambda (record)
             (let ((sequence (record-sequence record)))
               (cond
                 ((eq (first record) ':conversation)
                  nil)
                 ((and below sequence (>= sequence below))
                  (return-from reading))
                 ((and (keywordp (first record))
                       (evenp (length (rest record)))
                       (or (null sequence) (>= sequence last)))
                  (when sequence
                    (setf last sequence))
                  (push record records))
                 (t
                  (setf sound-p nil)
                  (return-from reading)))))
           :start-position position)
        (error ()
          (setf sound-p nil))))
    (values (nreverse records) sound-p)))

(-> history--window (pathname integer integer &key (:below (or null integer))) (values list integer))
(defun history--window (pathname offset octets &key below)
  "Return the records in about the OCTETS of the segment PATHNAME that end at OFFSET, and the octet position they start at.

The position is zero when the window reaches the segment's beginning."
  (let ((start (max 0 (- offset octets))))
    (if (zerop start)
        (values (history--read-from pathname 0 :below below) 0)
        (loop for candidate in (history--line-starts pathname start (min offset (+ start *history-scan-octets*)))
              do (multiple-value-bind (records sound-p) (history--read-from pathname candidate :below below)
                   (when (and sound-p records)
                     (return (values records candidate))))
              finally (return (values nil start))))))

(-> history-read-page
    (t string &key (:cursor (or null history-cursor)) (:before (or null integer)) (:through (or null integer))
       (:limit integer))
    (values list boolean (or null history-cursor)))
(defun history-read-page (configuration identifier &key cursor before through (limit *history-page-records*))
  "Return a page of channel IDENTIFIER's durable records, oldest first, whether older ones remain, and the cursor for the page before it.

Without CURSOR the page is the newest; with the cursor a call returned, it is
the one before that call's. BEFORE keeps only records whose sequence is below
it and THROUGH only those at or below it, the boundary a live attachment takes
over from. A page is what one bounded window of the log holds, at most LIMIT
records, so a conversation of any length opens in the time its last page
takes. A conversation with no durable history yet yields no records."
  (let ((conversation (handler-case (autolith::conversation-replay-load configuration identifier)
                        (error ()
                          nil))))
    (if (null conversation)
        (values nil nil nil)
        (let* ((segments (reverse (autolith::conversation-storage-pathnames
                                   (autolith::conversation-pathname conversation))))
               (segments (if cursor
                             (member (history-cursor-segment cursor) segments :test #'equal)
                             segments))
               (page     nil)
               (next     nil))
          (flet ((wanted-p (record)
                   (let ((sequence (record-sequence record)))
                     (and (or (null before) (null sequence) (< sequence before))
                          (or (null through) (null sequence) (<= sequence through))))))
            (loop for (segment . older) on segments
                  for first-p = t then nil
                  do (let ((offset (if (and first-p cursor)
                                       (history-cursor-offset cursor)
                                       (history--file-length segment)))
                           (octets *history-tail-octets*))
                       (loop
                         (multiple-value-bind (records start) (history--window segment offset octets :below before)
                           (let ((kept (remove-if-not #'wanted-p records)))
                             (when (or (zerop start)
                                       (>= (+ (length kept) (length page)) *history-minimum-records*)
                                       (>= octets *history-maximum-octets*))
                               (setf page (append kept page)
                                     next (cond
                                            ((plusp start)
                                             (make-history-cursor :segment segment :offset start))
                                            (older
                                             (make-history-cursor
                                              :segment (first older)
                                              :offset  (history--file-length (first older))))
                                            (t
                                             nil)))
                               (return))
                             (setf octets (* octets 2))))))
                     (when (or (>= (length page) *history-minimum-records*) (null next))
                       (return))))
          ;; A window can hold more than a page; the newest LIMIT of it are the page,
          ;; and the cursor still points before the whole window, so nothing is read twice.
          (let ((excess (- (length page) limit)))
            (values (if (plusp excess) (nthcdr excess page) page)
                    (and next t)
                    next))))))

(-> history-image-root (t string) (or null pathname))
(defun history-image-root (configuration identifier)
  "Return the directory holding channel IDENTIFIER's image artifacts, or NIL when it has no durable history."
  (handler-case
      (autolith::conversation-image-artifact-root
       (autolith::conversation-replay-load configuration identifier))
    (error ()
      nil)))
