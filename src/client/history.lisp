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

(-> history--read-from (pathname integer &key (:end (or null integer))) (values list boolean list))
(defun history--read-from (pathname position &key end)
  "Return the records of PATHNAME from octet POSITION on, whether they read as a well-formed run, and the octet position each one's form starts at.

Reading stops at the first form that starts at or after END, so a page
before one already shown does not read what follows it. A run is well formed
when every form has a record's shape and the sequence numbers never fall,
which a start inside a string or a record's middle soon fails."
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
  "Return the records in about the OCTETS of the segment PATHNAME that end at OFFSET, the octet position they start at, and the position of each.

The position is zero when the window reaches the segment's beginning."
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
  "Return the newest part of PAGE, a list of (START . RECORD) oldest first, that makes one page of history.

A form that holds several records is never divided, so the page before this
one ends where this one starts."
  (let ((tail (nthcdr (max 0 (- (length page) *history-page-records*)) page)))
    (loop for rest on page
          until (or (eq rest tail)
                    (= (first (first rest)) (first (first tail))))
          finally (return rest))))

(-> history--wide-window (pathname integer &key (:wanted-p function) (:need integer)) (values list integer))
(defun history--wide-window (segment offset &key wanted-p need)
  "Return the records WANTED-P keeps of a window of SEGMENT that ends at OFFSET, each as (START . RECORD), and where the window starts.

The window is widened until it holds NEED of them, reaches the segment's
beginning, or is as wide as a window gets."
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
  "Return a page of the records in the log SEGMENTS, newest segment first, as `history-read-page' does."
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
  "Return a page of channel IDENTIFIER's durable records, oldest first, whether older ones remain, and the cursor for the page before it.

Without CURSOR the page is the newest; with the cursor a call returned, it is
the one before that call's. BEFORE keeps only records whose sequence is below
it and THROUGH only those at or below it, the boundary a live attachment takes
over from. A page is the newest records of one bounded window of the log, and
the cursor is where its first record starts, so a conversation of any length
opens in the time its last page takes and no record is read twice or missed.
A conversation with no durable history yet yields no records."
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
  "Return the directory holding channel IDENTIFIER's image artifacts, or NIL when it has no durable history."
  (handler-case
      (autolith::conversation-image-artifact-root
       (autolith::conversation-replay-load configuration identifier))
    (error ()
      nil)))

(-> history-goal-record (t string) list)
(defun history-goal-record (configuration identifier)
  "Return the goal record that stood when the newest part of channel IDENTIFIER's history was begun, or NIL when there was none or the channel has no durable history yet.

Autolith writes it at the head of each part of a conversation's log, so the
goal of a long conversation is known without reading the whole of it."
  (handler-case
      (let ((record (autolith::conversation-latest-goal-record
                     (autolith::conversation-replay-load configuration identifier))))
        (and (consp record) (eq (first record) ':goal) record))
    (error ()
      nil)))


;;;; -- What Waits --

;;; A channel keeps a snapshot of the messages it has not taken up yet: what
;;; steers the running turn and what follows it. A window that attaches to a
;;; channel others have written to reads that snapshot, so it shows what
;;; waits there whoever sent it.

(-> history-pending-pathname (t string) (or null pathname))
(defun history-pending-pathname (configuration identifier)
  "Return the file in which channel IDENTIFIER keeps the messages it has not taken up, or NIL when the channel has no history yet."
  (handler-case
      (autolith::configuration-pending-inputs-path
       configuration
       (autolith::conversation-pathname (autolith::conversation-replay-load configuration identifier)))
    (error ()
      nil)))

(-> history--input-text (t) (or null string))
(defun history--input-text (input)
  "Return the text of the waiting INPUT, or NIL when it is not a message."
  (cond
    ((typep input '(or string autolith::user-message-input))
     (autolith::user-message-input-text input))
    ((consp input)
     (some #'history--input-text input))
    (t
     nil)))

(-> history-pending-inputs (pathname string) list)
(defun history-pending-inputs (pathname identifier)
  "Return the messages channel IDENTIFIER has not taken up, read from its snapshot at PATHNAME, each as (TEXT STEER-P): those that steer the running turn first, then those that follow it, oldest first."
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
