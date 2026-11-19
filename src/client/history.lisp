(in-package #:aui)

;;;; -- History --

;;; Read the newest part of the durable history of a channel without reading
;;; all of it. Long conversations can be hundreds of megabytes. Only the last
;;; few hundred records are shown at first. Earlier pages are read on request,
;;; as with (history) in the terminal. Records are parsed by the log reader of
;;; Autolith. The code below only chooses the position in a segment at which
;;; the reader starts.

(defparameter *history-page-records* 400
  "The maximum number of durable records in one page of history.")

(defparameter *history-tail-octets* (* 2 1024 1024)
  "The number of octets at the end of a segment that are read first for one page.
The window is doubled while it contains too few records.")

(defparameter *history-minimum-records* 60
  "The number of records that a window needs to become a page.
Windows with fewer records are widened.")

(defparameter *history-scan-octets* (* 512 1024)
  "The distance in octets after the start of a window in which a record boundary is searched for.")

(defparameter *history-maximum-octets* (* 16 1024 1024)
  "The maximum width of a window in octets, even if the window contains few records.")

(defstruct history-cursor
  "The end of the next earlier page of history, as the SEGMENT file and the octet OFFSET in that file."
  (segment nil :type (or null pathname))
  (offset  0   :type integer))

(-> history--file-length (pathname) integer)
(defun history--file-length (pathname)
  "Return the length of the file at PATHNAME in octets."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (file-length stream)))

(-> history--line-starts (pathname integer integer) list)
(defun history--line-starts (pathname from to)
  "Return the octet positions between FROM and TO in PATHNAME at which a line starts with a form headed by a keyword."
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

(-> history--read-from (pathname integer &key (:end (or null integer))) (values list boolean list))
(defun history--read-from (pathname position &key end)
  "Return the records of PATHNAME from the octet POSITION on, T if the records form a well-formed run, and the start positions of the records.

Reading stops at the first form that starts at or after END. A run is well
formed if all forms have the shape of a record and the sequence numbers do not
decrease. These conditions usually fail soon after a start inside a string or
inside a record."
  (let ((records nil)
        (starts  nil)
        (last    0)
        (sound-p t))
    (block reading
      (handler-case
          (sexp-store:log-map
           (lambda (record start form-end)
             (declare (ignore form-end))
             (let ((sequence (record-sequence record)))
               (cond
                 ((and end (>= start end))
                  (return-from reading))
                 ((eq (first record) ':conversation)
                  nil)
                 ((and (keywordp (first record))
                       (evenp (length (rest record)))
                       (or (null sequence) (>= sequence last)))
                  (when sequence
                    (setf last sequence))
                  (push record records)
                  (push start starts))
                 (t
                  (setf sound-p nil)
                  (return-from reading)))))
           pathname
           :start-position   position
           :with-positions-p t)
        (error ()
          (setf sound-p nil))))
    (values (nreverse records) sound-p (nreverse starts))))

(-> history--window (pathname integer integer) (values list integer list))
(defun history--window (pathname offset octets)
  "Return the records in a window of about OCTETS octets that ends at OFFSET in the segment PATHNAME.
Also return the start position of the window and the start positions of the records.

The start position of the window is zero if the window reaches the beginning
of the segment."
  (let ((start (max 0 (- offset octets))))
    (if (zerop start)
        (multiple-value-bind (records sound-p starts) (history--read-from pathname 0 :end offset)
          (declare (ignore sound-p))
          (values records 0 starts))
        (loop for candidate in (history--line-starts pathname start (min offset (+ start *history-scan-octets*)))
              do (multiple-value-bind (records sound-p starts) (history--read-from pathname candidate :end offset)
                   (when (and sound-p records)
                     (return (values records candidate starts))))
              finally (return (values nil start nil))))))

(-> history--cut (list) list)
(defun history--cut (page)
  "Return the newest part of PAGE that fits in one page of history.
PAGE is a list of (START . RECORD), oldest first.

Records from one form are not split between pages. The page before this page
ends where this page starts."
  (let ((tail (nthcdr (max 0 (- (length page) *history-page-records*)) page)))
    (loop for rest on page
          until (or (eq rest tail)
                    (= (first (first rest)) (first (first tail))))
          finally (return rest))))

(-> history--wide-window (pathname integer &key (:wanted-p function) (:need integer)) (values list integer))
(defun history--wide-window (segment offset &key wanted-p need)
  "Return the records of a window of SEGMENT that ends at OFFSET, and the start position of the window.
The records are a list of (START . RECORD). Only records accepted by WANTED-P are returned.

The window is widened until it contains NEED records, reaches the beginning of
the segment, or reaches the maximum width."
  (loop for octets = *history-tail-octets* then (* octets 2)
        do (multiple-value-bind (records start starts) (history--window segment offset octets)
             (let ((kept (loop for record in records
                               for record-start in starts
                               when (funcall wanted-p record)
                                 collect (cons record-start record))))
               (when (or (zerop start)
                         (>= (length kept) need)
                         (>= octets *history-maximum-octets*))
                 (return (values kept start)))))))

(-> history--page
    (list &key (:cursor (or null history-cursor)) (:before (or null integer)) (:through (or null integer)))
    (values list boolean (or null history-cursor)))
(defun history--page (segments &key cursor before through)
  "Return a page of the records in the log SEGMENTS in the same form as `history-read-page'.
SEGMENTS are ordered newest first."
  (let ((page nil)
        (next (or cursor
                  (and segments
                       (make-history-cursor :segment (first segments)
                                            :offset  (history--file-length (first segments)))))))
    (flet ((wanted-p (record)
             (let ((sequence (record-sequence record)))
               (and (or (null before) (null sequence) (< sequence before))
                    (or (null through) (null sequence) (<= sequence through)))))

           (end-of-older (segment)
             (let ((older (second (member segment segments :test #'equal))))
               (and older
                    (make-history-cursor :segment older :offset (history--file-length older))))))
      (loop while (and next (< (length page) *history-minimum-records*))
            do (let ((segment (history-cursor-segment next)))
                 (multiple-value-bind (kept start)
                     (history--wide-window segment (history-cursor-offset next)
                                           :wanted-p #'wanted-p
                                           :need     (- *history-minimum-records* (length page)))
                   (let ((cut (history--cut (append kept page))))
                     (setf page cut
                           next (cond
                                  ((and kept (not (eq (first cut) (first kept))))
                                   (make-history-cursor :segment segment :offset (first (first cut))))
                                  ((plusp start)
                                   (make-history-cursor :segment segment :offset start))
                                  (t
                                   (end-of-older segment)))))))))
    (values (mapcar #'rest page) (and next t) next)))

(-> history-read-page
    (t string &key (:cursor (or null history-cursor)) (:before (or null integer)) (:through (or null integer)))
    (values list boolean (or null history-cursor)))
(defun history-read-page (configuration identifier &key cursor before through)
  "Return a page of the durable records of the channel IDENTIFIER, oldest first.
Also return T if older records exist, and the cursor for the page before this page.

Without CURSOR, the page is the newest page. With a cursor returned by an
earlier call, the page is the page before the page of that call. If BEFORE is
given, only records with a sequence number below BEFORE are returned. If
THROUGH is given, only records with a sequence number at or below THROUGH are
returned.

A page contains the newest records of one bounded window of the log. The
cursor is at the start of the first record of the page. For this reason, long
conversations open as fast as short ones, and no record is read twice or
missed. If the conversation has no durable history yet, return no records."
  (let ((conversation (handler-case (autolith::conversation-replay-load configuration identifier)
                        (error ()
                          nil))))
    (if conversation
        (history--page (reverse (autolith::conversation-storage-pathnames
                                 (autolith::conversation-pathname conversation)))
                       :cursor cursor :before before :through through)
        (values nil nil nil))))

(-> history-image-root (t string) (or null pathname))
(defun history-image-root (configuration identifier)
  "Return the directory that contains the image artifacts of the channel IDENTIFIER.
Return NIL if the channel has no durable history."
  (handler-case
      (autolith::conversation-image-artifact-root
       (autolith::conversation-replay-load configuration identifier))
    (error ()
      nil)))

(-> history-goal-record (t string) list)
(defun history-goal-record (configuration identifier)
  "Return the goal record that was current at the start of the newest part of the history of the channel IDENTIFIER.
Return NIL if there was no goal or if the channel has no durable history yet.

Autolith writes this record at the head of each part of the log of a
conversation. For this reason, the goal of a long conversation is known
without reading the whole log."
  (handler-case
      (let ((record (autolith::conversation-latest-goal-record
                     (autolith::conversation-replay-load configuration identifier))))
        (and (consp record) (eq (first record) ':goal) record))
    (error ()
      nil)))


;;;; -- What Waits --

;;; Each channel has a snapshot of the waiting messages. Some of these messages
;;; steer the running turn, and others follow it. The snapshot contains the
;;; waiting messages from all senders.

(-> history-pending-pathname (t string) (or null pathname))
(defun history-pending-pathname (configuration identifier)
  "Return the snapshot file of the waiting messages of the channel IDENTIFIER.
Return NIL if the channel has no history yet."
  (handler-case
      (autolith::configuration-pending-inputs-path
       configuration
       (autolith::conversation-pathname (autolith::conversation-replay-load configuration identifier)))
    (error ()
      nil)))

(-> history--input-text (t) (or null string))
(defun history--input-text (input)
  "Return the text of the waiting INPUT, or NIL if INPUT is not a message."
  (cond
    ((typep input '(or string autolith::user-message-input))
     (autolith::user-message-input-text input))
    ((consp input)
     (some #'history--input-text input))
    (t
     nil)))

(-> history-pending-inputs (pathname string) list)
(defun history-pending-inputs (pathname identifier)
  "Return the waiting messages of the channel IDENTIFIER, read from the snapshot at PATHNAME.
Each message is a list (TEXT STEER-P). The messages that steer the running turn come
first. The messages that follow the turn come after them. Both groups are oldest first."
  (handler-case
      (let* ((form  (and (probe-file pathname) (autolith::read-portable-form pathname)))
             (state (and form (autolith::input-state-from-form form identifier))))
        (append
         (loop for entry in (append (getf state :steering-in-flight-items) (getf state :steering-items))
               for text = (history--input-text entry)
               when text
                 collect (list text t))
         (loop for work in (getf state :work-items)
               for text = (and (consp work) (history--input-text (second work)))
               when text
                 collect (list text nil))))
    (error ()
      nil)))
