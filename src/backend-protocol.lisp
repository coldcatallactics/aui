(in-package #:aui)

;;;; -- Backend Protocol --

;;; What the views ask of a backend. Every operation returns at once; its
;;; outcome arrives later as events on the backend's queue, so the view thread
;;; never blocks on the process. The backend implementation lives in
;;; src/backend/ and specializes these generic functions.

(deftype catalog-kind ()
  "The snapshots a view may ask the backend to refresh."
  '(member :state :jobs :models :settings :operations :conversations))

(deftype job-action ()
  "What the view may do to an owned job."
  '(member :inspect :send :cancel))

(defclass backend ()
  ((id         :initarg :id :reader backend-id
               :documentation "Identifies this owned process in every event it posts.")
   (directory  :initarg :directory :reader backend-directory :type string
               :documentation "The workspace directory the process runs in.")
   (queue      :initarg :queue :reader backend-queue :type ui-queue
               :documentation "Where the backend posts UI events.")
   (generation :initform 1 :accessor backend-generation :type integer
               :documentation "Incremented when the connection is replaced; stale events carry the old value."))
  (:documentation "The view's handle on one owned Autolith process."))

(defgeneric backend-start (backend)
  (:documentation "Start the owned process and connect. Progress arrives as CONNECTION-EVENTs."))

(defgeneric backend-stop (backend)
  (:documentation "Close the session and connection and end the process. The final
CONNECTION-EVENT has state :EXITED."))

(defgeneric backend-join (backend &key timeout)
  (:documentation "Wait until a stopped BACKEND has released its process and threads, at most
TIMEOUT seconds, and return T when it has. Called by the application before it
exits so no Autolith process is orphaned."))

(defgeneric backend-open-conversation (backend &key conversation-id)
  (:documentation "Open a session: a new conversation, or CONVERSATION-ID loaded with its
history. Any open session is closed first. SESSION-EVENTs report progress and
bracket replayed history."))

(defgeneric backend-prompt (backend text &key attachments)
  (:documentation "Send TEXT with ATTACHMENTS as the next prompt and return a request id.
TURN-EVENTs and TRANSCRIPT-EVENTs follow; a RESULT-EVENT with the request id
reports admission failure."))

(defgeneric backend-cancel (backend)
  (:documentation "Cancel the running turn. Pending approvals resolve as cancelled."))

(defgeneric backend-decide-approval (backend request-id option-id)
  (:documentation "Answer the permission request REQUEST-ID with the offered OPTION-ID.
A request already resolved is left alone."))

(defgeneric backend-set-mode (backend mode-id)
  (:documentation "Select the permission mode MODE-ID and return a request id."))

(defgeneric backend-refresh (backend kind)
  (:documentation "Ask for a fresh snapshot of KIND, a CATALOG-KIND, and return a request id.
The answer is a STATE-EVENT, JOBS-EVENT or CATALOG-EVENT."))

(defgeneric backend-set-setting (backend name value)
  (:documentation "Change the Autolith setting NAME to VALUE and return a request id."))

(defgeneric backend-invoke (backend operation arguments)
  (:documentation "Invoke the command or tool OPERATION with ARGUMENTS and return a request id."))

(defgeneric backend-job-action (backend job-id action &key text)
  (:documentation "Perform ACTION, a JOB-ACTION, on JOB-ID; :SEND delivers TEXT. Returns a request id."))

(defvar *backend-factory* nil
  "A function of DIRECTORY and QUEUE returning a started-capable BACKEND, installed
by the backend module when it loads. NIL means no live backend is available.")

(defvar *recent-conversations-function* nil
  "A function of no arguments returning CONVERSATION-ENTRYs for the welcome
screen, installed by the backend module. NIL means none can be listed.")

(define-condition no-backend-available (error)
  ()
  (:report "No Autolith backend is installed in this build; only the demo can run.")
  (:documentation "MAKE-BACKEND was called without a backend module loaded."))

(-> make-backend (string ui-queue) backend)
(defun make-backend (directory queue)
  "Return a backend for DIRECTORY posting to QUEUE through *BACKEND-FACTORY*."
  (unless *backend-factory*
    (error 'no-backend-available))
  (funcall *backend-factory* directory queue))

(-> recent-conversations () list)
(defun recent-conversations ()
  "Return the conversations the welcome screen lists, most recent first, or
nothing when no backend module supplies them."
  (if *recent-conversations-function*
      (conversations-by-recency (funcall *recent-conversations-function*))
      nil))

(defvar *request-counter* 0
  "The last request id handed out by MAKE-REQUEST-ID.")

(-> make-request-id () string)
(defun make-request-id ()
  "Return a fresh request id for correlating a RESULT-EVENT."
  (format nil "request-~D" (incf *request-counter*)))
