(in-package #:aui)

;;;; -- Transcript Model --

;;; The conversation as an ordered sequence of blocks, independent of how it
;;; is drawn. The backend describes mutations; the view listens. Only the
;;; model-owner thread, the FLTK thread in the application, calls into here.

(deftype tool-call-status ()
  "The normalized outcome of a tool call."
  '(member :pending :running :completed :failed :denied :cancelled :interrupted))

(deftype transcript-change ()
  "What a listener is told happened to a block."
  '(member :appended :text-appended :updated :replaced :reset :replay-begin :replay-end))

(deftype notice-severity ()
  "How loudly a notice should read."
  '(member :information :warning :failure))


;;; Blocks

(defclass transcript-block ()
  ((id           :initarg :id :initform nil :accessor transcript-block-id
                 :type (or null string)
                 :documentation "Identity within one transcript incarnation; generated when NIL.")
   (sequence     :initform 0 :accessor transcript-block-sequence :type integer
                 :documentation "Position assigned on append. Ordering only, never a durable record number.")
   (turn-id      :initarg :turn-id :initform nil :reader transcript-block-turn-id
                 :documentation "The user turn this block belongs to, when the backend knows it.")
   (received-at  :initarg :received-at :initform (get-universal-time) :reader transcript-block-received-at
                 :type integer
                 :documentation "Universal time at which AUI first saw the block.")
   (backend-time :initarg :backend-time :initform nil :reader transcript-block-backend-time
                 :documentation "The backend's own timestamp, when it supplied one.")
   (collapsed-p  :initarg :collapsed-p :initform nil :accessor transcript-block-collapsed-p
                 :type boolean
                 :documentation "View-owned disclosure state."))
  (:documentation "One unit of the conversation document."))

(defclass user-message (transcript-block)
  ((text        :initarg :text :initform "" :reader user-message-text :type string
                :documentation "The prompt as sent.")
   (attachments :initarg :attachments :initform nil :reader user-message-attachments
                :documentation "Resource links or paths sent with the prompt."))
  (:documentation "A prompt from the user."))

(defclass assistant-text (transcript-block)
  ((text :initarg :text :initform "" :accessor assistant-text-text :type string
         :documentation "Streamed prose, appended as fragments arrive."))
  (:documentation "One contiguous run of assistant prose."))

(defclass reasoning (transcript-block)
  ((text :initarg :text :initform "" :accessor reasoning-text :type string
         :documentation "Visible reasoning, appended as fragments arrive."))
  (:default-initargs :collapsed-p t)
  (:documentation "A run of visible reasoning, collapsed by default."))

(defclass tool-call (transcript-block)
  ((call-id             :initarg :call-id :reader tool-call-call-id :type string
                        :documentation "The session-qualified ACP tool call identifier.")
   (name                :initarg :name :reader tool-call-name :type string
                        :documentation "The canonical tool name, such as shell.run.")
   (kind                :initarg :kind :initform ':other :reader tool-call-kind :type keyword
                        :documentation "The ACP presentation category.")
   (title               :initarg :title :initform "" :accessor tool-call-title :type string
                        :documentation "The name with a bounded preview of its principal argument.")
   (status              :initarg :status :initform ':pending :accessor tool-call-status
                        :type tool-call-status
                        :documentation "The normalized outcome.")
   (raw-status          :initarg :raw-status :initform nil :accessor tool-call-raw-status
                        :documentation "The status exactly as the wire carried it.")
   (input               :initarg :input :initform nil :accessor tool-call-input
                        :documentation "The arguments as immutable data.")
   (output              :initarg :output :initform nil :accessor tool-call-output
                        :type (or null string)
                        :documentation "The bounded text result the agent received.")
   (locations           :initarg :locations :initform nil :accessor tool-call-locations
                        :documentation "Paths and lines the call touched.")
   (artifact-references :initarg :artifact-references :initform nil
                        :accessor tool-call-artifact-references
                        :documentation "References to logs and artifacts for the inspector."))
  (:default-initargs :collapsed-p t)
  (:documentation "One tool invocation and its outcome."))

(defclass plan-block (transcript-block)
  ((entries :initarg :entries :initform nil :accessor plan-block-entries
            :documentation "A list of (TEXT . STATUS) with STATUS in :PENDING :IN-PROGRESS :COMPLETED."))
  (:documentation "The agent's current plan; one block, replaced in place."))

(defclass approval (transcript-block)
  ((request-id  :initarg :request-id :reader approval-request-id :type string
                :documentation "The identity the backend resolves the decision against.")
   (call-id     :initarg :call-id :initform nil :reader approval-call-id
                :documentation "The tool call awaiting the decision, when there is one.")
   (title       :initarg :title :initform "" :reader approval-title :type string
                :documentation "What is being asked.")
   (arguments   :initarg :arguments :initform nil :reader approval-arguments
                :documentation "The exact operation arguments as immutable data.")
   (directory   :initarg :directory :initform nil :reader approval-directory
                :documentation "The working directory of a command approval.")
   (options     :initarg :options :initform nil :reader approval-options
                :documentation "The offered choices as (OPTION-ID NAME KIND) lists, rendered as offered.")
   (decision    :initform nil :accessor approval-decision
                :documentation "The chosen option id, or :CANCELLED, or NIL while pending.")
   (deadline    :initarg :deadline :initform nil :reader approval-deadline
                :documentation "Approximate universal time at which the backend gives up."))
  (:documentation "A permission request awaiting the user."))

(defclass notice (transcript-block)
  ((severity :initarg :severity :initform ':information :accessor notice-severity
             :type notice-severity
             :documentation "How loudly the notice should read.")
   (text     :initarg :text :initform "" :accessor notice-text :type string
             :documentation "The notice itself.")
   (key      :initarg :key :initform nil :reader notice-key
             :documentation "A deduplication key; a later notice with the same key updates this one."))
  (:documentation "A system notice: connection, compaction, usage, errors."))

(-> approval-pending-p (approval) boolean)
(defun approval-pending-p (approval)
  "Return T while APPROVAL has no decision."
  (null (approval-decision approval)))


;;; Transcript

(defclass transcript ()
  ((blocks        :initform (make-array 0 :adjustable t :fill-pointer t)
                  :reader transcript-blocks
                  :documentation "Every block in order.")
   (index         :initform (make-hash-table :test 'equal) :reader transcript--index
                  :documentation "Blocks by id.")
   (next-sequence :initform 1 :accessor transcript--next-sequence :type integer
                  :documentation "The sequence the next appended block receives.")
   (incarnation   :initform 1 :accessor transcript-incarnation :type integer
                  :documentation "Incremented by every reset; block ids are stable within one.")
   (replaying-p   :initform nil :accessor transcript-replaying-p :type boolean
                  :documentation "T while durable history is being restored.")
   (listeners     :initform nil :accessor transcript-listeners
                  :documentation "Objects told about every change through TRANSCRIPT-LISTENER-CHANGED."))
  (:documentation "The conversation document and its change notification."))

(defgeneric transcript-listener-changed (listener transcript block change)
  (:documentation "Tell LISTENER that BLOCK of TRANSCRIPT underwent CHANGE, a TRANSCRIPT-CHANGE.
BLOCK is NIL for :RESET, :REPLAY-BEGIN and :REPLAY-END."))

(define-condition transcript-error (error)
  ((transcript :initarg :transcript :reader transcript-error-transcript))
  (:documentation "A misuse of the transcript model."))

(define-condition transcript-duplicate-block (transcript-error)
  ((id :initarg :id :reader transcript-duplicate-block-id))
  (:report (lambda (condition stream)
             (format stream "A block with id ~S is already in the transcript."
                     (transcript-duplicate-block-id condition))))
  (:documentation "An append reused an id of the current incarnation."))

(define-condition transcript-missing-block (transcript-error)
  ((id :initarg :id :reader transcript-missing-block-id))
  (:report (lambda (condition stream)
             (format stream "No block with id ~S is in the transcript."
                     (transcript-missing-block-id condition))))
  (:documentation "A mutation named a block that does not exist."))

(define-condition transcript-block-kind-error (transcript-error)
  ((block :initarg :block :reader transcript-block-kind-error-block)
   (expected :initarg :expected :reader transcript-block-kind-error-expected))
  (:report (lambda (condition stream)
             (format stream "Block ~S is a ~A, not a ~A."
                     (transcript-block-id (transcript-block-kind-error-block condition))
                     (type-of (transcript-block-kind-error-block condition))
                     (transcript-block-kind-error-expected condition))))
  (:documentation "A mutation does not apply to the block's kind."))

(-> make-transcript () transcript)
(defun make-transcript ()
  "Return an empty transcript."
  (make-instance 'transcript))

(-> transcript-add-listener (transcript t) t)
(defun transcript-add-listener (transcript listener)
  "Register LISTENER for change notifications and return it."
  (pushnew listener (transcript-listeners transcript))
  listener)

(-> transcript-remove-listener (transcript t) null)
(defun transcript-remove-listener (transcript listener)
  "Stop notifying LISTENER."
  (setf (transcript-listeners transcript)
        (remove listener (transcript-listeners transcript)))
  nil)

(-> transcript-length (transcript) integer)
(defun transcript-length (transcript)
  "Return the number of blocks."
  (length (transcript-blocks transcript)))

(-> transcript-find (transcript string) (or null transcript-block))
(defun transcript-find (transcript id)
  "Return the block with ID, or NIL."
  (values (gethash id (transcript--index transcript))))

(-> transcript-last-block (transcript &optional symbol) (or null transcript-block))
(defun transcript-last-block (transcript &optional (kind 'transcript-block))
  "Return the most recent block of class KIND, or NIL."
  (let ((blocks (transcript-blocks transcript)))
    (loop for position from (1- (length blocks)) downto 0
          for block = (aref blocks position)
          when (typep block kind)
            do (return block))))

(-> transcript--notify (transcript (or null transcript-block) transcript-change) null)
(defun transcript--notify (transcript block change)
  "Tell every listener about CHANGE to BLOCK."
  (dolist (listener (transcript-listeners transcript))
    (transcript-listener-changed listener transcript block change))
  nil)

(-> transcript--require (transcript string) transcript-block)
(defun transcript--require (transcript id)
  "Return the block with ID or signal TRANSCRIPT-MISSING-BLOCK."
  (or (transcript-find transcript id)
      (error 'transcript-missing-block :transcript transcript :id id)))

(-> transcript--require-kind (transcript transcript-block symbol) transcript-block)
(defun transcript--require-kind (transcript block kind)
  "Return BLOCK when it is of class KIND, else signal TRANSCRIPT-BLOCK-KIND-ERROR."
  (unless (typep block kind)
    (error 'transcript-block-kind-error :transcript transcript :block block :expected kind))
  block)

(-> transcript--generate-id (transcript transcript-block) string)
(defun transcript--generate-id (transcript block)
  "Return a fresh id for BLOCK from its class and the next sequence."
  (format nil "~(~A~)-~D" (class-name (class-of block)) (transcript--next-sequence transcript)))

(-> transcript-append (transcript transcript-block) transcript-block)
(defun transcript-append (transcript block)
  "Append BLOCK, assigning its sequence and an id when it has none, and return it.
Signals TRANSCRIPT-DUPLICATE-BLOCK when the id is already present."
  (unless (transcript-block-id block)
    (setf (transcript-block-id block) (transcript--generate-id transcript block)))
  (when (transcript-find transcript (transcript-block-id block))
    (error 'transcript-duplicate-block :transcript transcript :id (transcript-block-id block)))
  (setf (transcript-block-sequence block) (transcript--next-sequence transcript))
  (incf (transcript--next-sequence transcript))
  (vector-push-extend block (transcript-blocks transcript))
  (setf (gethash (transcript-block-id block) (transcript--index transcript)) block)
  (transcript--notify transcript block ':appended)
  block)

(-> transcript-append-text (transcript string string) transcript-block)
(defun transcript-append-text (transcript id text)
  "Append TEXT to the assistant text or reasoning block ID and return the block."
  (let ((block (transcript--require transcript id)))
    (typecase block
      (assistant-text
       (setf (assistant-text-text block)
             (concatenate 'string (assistant-text-text block) text)))
      (reasoning
       (setf (reasoning-text block)
             (concatenate 'string (reasoning-text block) text)))
      (t
       (error 'transcript-block-kind-error :transcript transcript :block block
                                           :expected '(or assistant-text reasoning))))
    (transcript--notify transcript block ':text-appended)
    block))

(-> transcript-find-tool-call (transcript string) (or null tool-call))
(defun transcript-find-tool-call (transcript call-id)
  "Return the tool call block whose call id is CALL-ID, or NIL."
  (let ((blocks (transcript-blocks transcript)))
    (loop for position from (1- (length blocks)) downto 0
          for block = (aref blocks position)
          when (and (typep block 'tool-call)
                    (string= (tool-call-call-id block) call-id))
            do (return block))))

(-> transcript-update-tool-call
    (transcript string &key (:status (or null tool-call-status)) (:raw-status t)
                (:output (or null string)) (:title (or null string))
                (:locations t) (:artifact-references t))
    tool-call)
(defun transcript-update-tool-call (transcript call-id &key status raw-status output title
                                                            (locations nil locations-p)
                                                            (artifact-references nil references-p))
  "Update the tool call with CALL-ID with the supplied fields and return it.
Signals TRANSCRIPT-MISSING-BLOCK when no such call exists."
  (let ((block (or (transcript-find-tool-call transcript call-id)
                   (error 'transcript-missing-block :transcript transcript :id call-id))))
    (when status
      (setf (tool-call-status block) status))
    (when raw-status
      (setf (tool-call-raw-status block) raw-status))
    (when output
      (setf (tool-call-output block) output))
    (when title
      (setf (tool-call-title block) title))
    (when locations-p
      (setf (tool-call-locations block) locations))
    (when references-p
      (setf (tool-call-artifact-references block) artifact-references))
    (transcript--notify transcript block ':updated)
    block))

(-> transcript-replace-plan (transcript list) plan-block)
(defun transcript-replace-plan (transcript entries)
  "Make ENTRIES the current plan, replacing the existing plan block in place or
appending one, and return the block."
  (let ((block (transcript-last-block transcript 'plan-block)))
    (cond
      (block
       (setf (plan-block-entries block) entries)
       (transcript--notify transcript block ':replaced)
       block)
      (t
       (transcript-append transcript (make-instance 'plan-block :entries entries))))))

(-> transcript-find-approval (transcript string) (or null approval))
(defun transcript-find-approval (transcript request-id)
  "Return the approval block for REQUEST-ID, or NIL."
  (let ((blocks (transcript-blocks transcript)))
    (loop for position from (1- (length blocks)) downto 0
          for block = (aref blocks position)
          when (and (typep block 'approval)
                    (string= (approval-request-id block) request-id))
            do (return block))))

(-> transcript-decide-approval (transcript string (or string (eql :cancelled))) approval)
(defun transcript-decide-approval (transcript request-id decision)
  "Record DECISION for the approval REQUEST-ID and return the block.
A second decision leaves the first one standing."
  (let ((block (or (transcript-find-approval transcript request-id)
                   (error 'transcript-missing-block :transcript transcript :id request-id))))
    (when (approval-pending-p block)
      (setf (approval-decision block) decision)
      (transcript--notify transcript block ':updated))
    block))

(-> transcript-pending-approvals (transcript) list)
(defun transcript-pending-approvals (transcript)
  "Return the approvals still awaiting a decision, oldest first."
  (loop for block across (transcript-blocks transcript)
        when (and (typep block 'approval) (approval-pending-p block))
          collect block))

(-> transcript-notice (transcript &key (:severity notice-severity) (:text string) (:key t)) notice)
(defun transcript-notice (transcript &key (severity ':information) (text "") key)
  "Append a notice, or update the existing notice with the same KEY, and return it."
  (let ((existing (and key
                       (loop for block across (transcript-blocks transcript)
                             when (and (typep block 'notice) (equal (notice-key block) key))
                               do (return block)))))
    (cond
      (existing
       (setf (notice-severity existing) severity
             (notice-text existing) text)
       (transcript--notify transcript existing ':updated)
       existing)
      (t
       (transcript-append transcript
                          (make-instance 'notice :severity severity :text text :key key))))))

(-> transcript-reset (transcript) null)
(defun transcript-reset (transcript)
  "Forget every block and begin a new incarnation."
  (setf (fill-pointer (transcript-blocks transcript)) 0)
  (clrhash (transcript--index transcript))
  (setf (transcript--next-sequence transcript) 1
        (transcript-replaying-p transcript) nil)
  (incf (transcript-incarnation transcript))
  (transcript--notify transcript nil ':reset)
  nil)

(-> transcript-begin-replay (transcript) null)
(defun transcript-begin-replay (transcript)
  "Mark the start of durable history restoration."
  (setf (transcript-replaying-p transcript) t)
  (transcript--notify transcript nil ':replay-begin)
  nil)

(-> transcript-end-replay (transcript) null)
(defun transcript-end-replay (transcript)
  "Mark the end of durable history restoration."
  (setf (transcript-replaying-p transcript) nil)
  (transcript--notify transcript nil ':replay-end)
  nil)
