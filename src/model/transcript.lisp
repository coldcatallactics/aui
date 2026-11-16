(in-package #:aui)

;;;; -- Transcript Model --

;;; A channel's history as an ordered sequence of entries, independent of how
;;; it is drawn. The projection in src/client/records.lisp turns Autolith's
;;; records and live events into the mutations below; the document view reads
;;; the entries and redraws those whose revision moved. Only the interface
;;; thread touches a transcript.

(deftype tool-status ()
  "How far a tool call got."
  '(member :pending :running :completed :failed))

(deftype notice-tone ()
  "How a notice should read: a quiet aside, a success, a warning or a failure."
  '(member :quiet :success :warning :failure))


;;;; -- Entries --

(defclass entry ()
  ((identifier
    :initarg       :identifier
    :initform      nil
    :accessor      entry-identifier
    :type          (or null string)
    :documentation "The entry's identity within its transcript, assigned on insertion when NIL.")
   (time
    :initarg       :time
    :initform      nil
    :reader        entry-time
    :type          (or null integer)
    :documentation "The universal time Autolith recorded for the entry, or NIL.")
   (revision
    :initform      0
    :accessor      entry-revision
    :type          integer
    :documentation "Incremented on every change, so a view knows its layout of the entry is stale.")
   (expanded-p
    :initarg       :expanded-p
    :initform      nil
    :accessor      entry-expanded-p
    :type          boolean
    :documentation "Whether the person opened the entry's detail."))
  (:documentation "One unit of a channel's history."))

(defclass user-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :reader        user-entry-text
    :type          string
    :documentation "What was written.")
   (images
    :initarg       :images
    :initform      nil
    :reader        user-entry-images
    :type          list
    :documentation "Namestrings of the images sent with the message.")
   (sender
    :initarg       :sender
    :initform      nil
    :reader        user-entry-sender
    :type          (or null string)
    :documentation "The participant who wrote it when that is not the person, as for a message from another channel.")
   (origin
    :initarg       :origin
    :initform      nil
    :reader        user-entry-origin
    :type          (or null string)
    :documentation "The channel the message came from, shown with the sender, or NIL."))
  (:documentation "A message to the channel's agent."))

(defclass assistant-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      assistant-entry-text
    :type          string
    :documentation "The agent's prose as Markdown, growing while it streams.")
   (streaming-p
    :initarg       :streaming-p
    :initform      nil
    :accessor      assistant-entry-streaming-p
    :type          boolean
    :documentation "Whether more of the text is still arriving."))
  (:documentation "One contiguous answer from the agent."))

(defclass reasoning-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      reasoning-entry-text
    :type          string
    :documentation "The visible reasoning summary, growing while it streams.")
   (streaming-p
    :initarg       :streaming-p
    :initform      nil
    :accessor      reasoning-entry-streaming-p
    :type          boolean
    :documentation "Whether more of the reasoning is still arriving."))
  (:documentation "A stretch of the agent's visible reasoning, closed by default."))

(defclass tool-entry (entry)
  ((call-identifier
    :initarg       :call-identifier
    :initform      nil
    :reader        tool-entry-call-identifier
    :type          (or null string)
    :documentation "The provider's identity for the call, which its result names.")
   (name
    :initarg       :name
    :initform      "tool"
    :reader        tool-entry-name
    :type          string
    :documentation "The tool's canonical name, such as shell.run.")
   (arguments
    :initarg       :arguments
    :initform      nil
    :accessor      tool-entry-arguments
    :type          list
    :documentation "The call's arguments as an association list of name and value, in the order given.")
   (status
    :initarg       :status
    :initform      ':pending
    :accessor      tool-entry-status
    :type          tool-status
    :documentation "How far the call got.")
   (output
    :initarg       :output
    :initform      nil
    :accessor      tool-entry-output
    :type          (or null string)
    :documentation "What the tool returned, once it has.")
   (seconds
    :initarg       :seconds
    :initform      nil
    :accessor      tool-entry-seconds
    :type          (or null real)
    :documentation "How long the call took by the clock, once known."))
  (:documentation "One tool call and its result."))

(defclass notice-entry (entry)
  ((tone
    :initarg       :tone
    :initform      ':quiet
    :reader        notice-entry-tone
    :type          notice-tone
    :documentation "How the notice should read.")
   (text
    :initarg       :text
    :initform      ""
    :reader        notice-entry-text
    :type          string
    :documentation "The notice's one-line statement.")
   (detail
    :initarg       :detail
    :initform      nil
    :reader        notice-entry-detail
    :type          (or null string)
    :documentation "Longer text shown beneath the statement, or NIL.")
   (channel
    :initarg       :channel
    :initform      nil
    :reader        notice-entry-channel
    :type          (or null string)
    :documentation "A channel the notice leads to when clicked, or NIL."))
  (:documentation "Something Autolith says about the conversation rather than in it."))

(defclass output-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      output-entry-text
    :type          string
    :documentation "The output as Autolith laid it out for a fixed-pitch face.")
   (invocation
    :initarg       :invocation
    :initform      nil
    :accessor      output-entry-invocation
    :type          (or null string)
    :documentation "The command or form whose output this is, when it is one."))
  (:documentation "What a command or a local Lisp form printed."))

(defclass schemas-entry (entry)
  ((names
    :initarg       :names
    :initform      nil
    :accessor      schemas-entry-names
    :type          list
    :documentation "The names of the groups of tools, in the order the agent first looked each up."))
  (:documentation "The agent looked up how groups of its tools are called. Lookups that follow one another are one entry."))

(defclass marker-entry (entry)
  ((label
    :initarg       :label
    :initform      ""
    :reader        marker-entry-label
    :type          string
    :documentation "The words on the rule."))
  (:documentation "A rule across the transcript, such as the point where context was compacted."))


;;;; -- Transcript --

(defclass transcript ()
  ((entries
    :initform      (make-array 64 :adjustable t :fill-pointer 0)
    :reader        transcript-entries
    :type          vector
    :documentation "The entries, oldest first.")
   (index
    :initform      (make-hash-table :test #'equal)
    :reader        transcript--index
    :type          hash-table
    :documentation "Each entry by its identifier.")
   (tools
    :initform      (make-hash-table :test #'equal)
    :reader        transcript--tools
    :type          hash-table
    :documentation "Each tool entry by its call identifier.")
   (counter
    :initform      0
    :accessor      transcript--counter
    :type          integer
    :documentation "The last generated entry identifier.")
   (revision
    :initform      0
    :accessor      transcript-revision
    :type          integer
    :documentation "Incremented on every change to the sequence or to an entry."))
  (:documentation "The ordered history of one channel."))

(-> make-transcript () transcript)
(defun make-transcript ()
  "Return an empty transcript."
  (make-instance 'transcript))

(-> transcript-length (transcript) integer)
(defun transcript-length (transcript)
  "Return how many entries TRANSCRIPT holds."
  (length (transcript-entries transcript)))

(-> transcript-entry (transcript integer) entry)
(defun transcript-entry (transcript position)
  "Return TRANSCRIPT's entry at POSITION, counted from the oldest."
  (aref (transcript-entries transcript) position))

(-> transcript-find (transcript string) (or null entry))
(defun transcript-find (transcript identifier)
  "Return TRANSCRIPT's entry named IDENTIFIER, or NIL."
  (values (gethash identifier (transcript--index transcript))))

(-> transcript-find-tool (transcript string) (or null tool-entry))
(defun transcript-find-tool (transcript call-identifier)
  "Return TRANSCRIPT's tool entry for CALL-IDENTIFIER, or NIL."
  (values (gethash call-identifier (transcript--tools transcript))))

(-> transcript-last (transcript) (or null entry))
(defun transcript-last (transcript)
  "Return TRANSCRIPT's newest entry, or NIL when it is empty."
  (let ((entries (transcript-entries transcript)))
    (when (plusp (length entries))
      (aref entries (1- (length entries))))))

(-> transcript--register (transcript entry) entry)
(defun transcript--register (transcript entry)
  "Give ENTRY an identifier when it lacks one and index it in TRANSCRIPT."
  (unless (entry-identifier entry)
    (setf (entry-identifier entry)
          (format nil "entry-~D" (incf (transcript--counter transcript)))))
  (setf (gethash (entry-identifier entry) (transcript--index transcript)) entry)
  (when (and (typep entry 'tool-entry) (tool-entry-call-identifier entry))
    (setf (gethash (tool-entry-call-identifier entry) (transcript--tools transcript)) entry))
  entry)

(-> transcript-append (transcript entry) entry)
(defun transcript-append (transcript entry)
  "Add ENTRY as TRANSCRIPT's newest and return it."
  (transcript--register transcript entry)
  (vector-push-extend entry (transcript-entries transcript))
  (incf (transcript-revision transcript))
  entry)

(-> transcript-prepend (transcript list) integer)
(defun transcript-prepend (transcript entries)
  "Put ENTRIES, oldest first, before everything TRANSCRIPT holds and return how many were added.

This is how an earlier page of history arrives."
  (let* ((existing (coerce (transcript-entries transcript) 'list))
         (vector   (transcript-entries transcript)))
    (setf (fill-pointer vector) 0)
    (dolist (entry entries)
      (vector-push-extend (transcript--register transcript entry) vector))
    (dolist (entry existing)
      (vector-push-extend entry vector))
    (incf (transcript-revision transcript))
    (length entries)))

(-> transcript-touch (transcript entry) entry)
(defun transcript-touch (transcript entry)
  "Record that ENTRY changed, so views lay it out again, and return it."
  (incf (entry-revision entry))
  (incf (transcript-revision transcript))
  entry)

(-> transcript-clear (transcript) transcript)
(defun transcript-clear (transcript)
  "Remove every entry from TRANSCRIPT and return it."
  (setf (fill-pointer (transcript-entries transcript)) 0)
  (clrhash (transcript--index transcript))
  (clrhash (transcript--tools transcript))
  (incf (transcript-revision transcript))
  transcript)

(-> transcript-plain-text (transcript) string)
(defun transcript-plain-text (transcript)
  "Return TRANSCRIPT's messages and answers as plain text, for copying a whole conversation."
  (with-output-to-string (stream)
    (loop for entry across (transcript-entries transcript)
          do (typecase entry
               (user-entry
                (format stream "You: ~A~2%" (user-entry-text entry)))
               (assistant-entry
                (format stream "~A~2%" (assistant-entry-text entry)))))))
