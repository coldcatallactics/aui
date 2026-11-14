(in-package #:aui)

;;;; -- World Client --

;;; AUI's handle on Autolith's world of channels, through the client code
;;; Autolith's own terminal uses: the world endpoint record to find the
;;; server, WORLD-CLIENT-OPEN to open a channel in it, starting the server
;;; when none runs, and WORLD-SIDEBAR for the tree of channels. Everything
;;; that names an Autolith definition lives in this directory, so the
;;; interface above it depends on AUI's own vocabulary only.

(define-condition client-error (error)
  ((message
    :initarg       :message
    :reader        client-error-message
    :type          string
    :documentation "What went wrong, in words fit to show."))
  (:report (lambda (condition stream)
             (write-string (client-error-message condition) stream)))
  (:documentation "A failure talking to Autolith that the interface reports rather than crashes on."))

(-> condition-report (condition) string)
(defun condition-report (condition)
  "Return CONDITION's report, bounded so a runaway message cannot flood the interface."
  (let ((report (handler-case (princ-to-string condition)
                  (error ()
                    (format nil "~A" (type-of condition))))))
    (subseq report 0 (min 2000 (length report)))))

(defclass world ()
  ((configuration
    :initarg       :configuration
    :reader        world-configuration
    :documentation "The Autolith configuration locating the world server and its data.")
   (permission-mode
    :initarg       :permission-mode
    :initform      ':ask
    :reader        world-permission-mode
    :type          keyword
    :documentation "The permission mode a channel opened by this client asks for.")
   (immutable-p
    :initarg       :immutable-p
    :initform      nil
    :reader        world-immutable-p
    :type          boolean
    :documentation "Whether this launch asked for an immutable server.")
   (launch
    :initarg       :launch
    :initform      nil
    :accessor      world-launch
    :type          list
    :documentation "The launch options the command line stated, applied to the first channel opened and then spent.")
   (sidebar
    :initform      nil
    :accessor      world--sidebar
    :documentation "Autolith's sidebar model of the world's channels, once started."))
  (:documentation "This client's view of one Autolith world."))

(-> world-working-directory (world) string)
(defun world-working-directory (world)
  "Return the directory WORLD's client was launched for."
  (namestring (autolith::config :working-directory (world-configuration world))))

(-> world-autolith-version () string)
(defun world-autolith-version ()
  "Return the version of the Autolith this client was built against."
  autolith::*autolith-version*)


;;;; -- Channel Tree --

(-> world-start-watching (world) null)
(defun world-start-watching (world)
  "Begin gathering WORLD's channels on a thread of Autolith's own, once."
  (unless (world--sidebar world)
    (let ((sidebar (autolith::world-sidebar-create (world-configuration world))))
      (autolith::world-sidebar-start sidebar)
      (setf (world--sidebar world) sidebar)))
  nil)

(-> world-stop-watching (world) null)
(defun world-stop-watching (world)
  "Stop gathering WORLD's channels and wait for the gathering thread."
  (let ((sidebar (world--sidebar world)))
    (when sidebar
      (autolith::world-sidebar-stop sidebar)
      (setf (world--sidebar world) nil)))
  nil)

(-> world-refresh (world) boolean)
(defun world-refresh (world)
  "Take what was gathered since the last call and return whether the channel tree changed. Never blocks."
  (let ((sidebar (world--sidebar world)))
    (and sidebar
         (autolith::screen-panel-refresh sidebar)
         t)))

(-> world-entries (world) list)
(defun world-entries (world)
  "Return WORLD's channel tree as Autolith's sidebar entries: property lists whose :KIND is :INBOX, :WORKSPACE or :CHANNEL."
  (let ((sidebar (world--sidebar world)))
    (and sidebar (autolith::world-sidebar-entries sidebar))))

(-> world-channel-title (world (or null string)) (or null string))
(defun world-channel-title (world identifier)
  "Return the title WORLD's channel tree gives the channel IDENTIFIER, or NIL when it has none there."
  (and identifier
       (let ((entry (find identifier (world-entries world)
                          :key  (lambda (entry)
                                  (getf entry :id))
                          :test #'equal)))
         (and entry (portable-string (getf entry :title))))))

(-> (setf world-current-channel) ((or null string) world) (or null string))
(defun (setf world-current-channel) (identifier world)
  "Tell WORLD's channel tree that this client now shows channel IDENTIFIER."
  (let ((sidebar (world--sidebar world)))
    (when sidebar
      (setf (autolith::world-sidebar-current sidebar) identifier)
      (let ((inputs (autolith::world-sidebar-inputs sidebar)))
        (when inputs
          (autolith::world-sidebar--take sidebar inputs)))))
  identifier)

(-> world-toggle-node (world string) null)
(defun world-toggle-node (world key)
  "Open the workspace or channel node KEY of WORLD's tree when it shows closed, or close it."
  (let ((sidebar (world--sidebar world)))
    (when sidebar
      (autolith::world-sidebar--toggle sidebar key)))
  nil)


;;;; -- World Requests --

(-> world--entry (world) (or null cons))
(defun world--entry (world)
  "Return the reachable world server endpoint of WORLD, or NIL when none answers."
  (autolith::world-client-entry (world-configuration world)))

(-> world-server-status (world) (or null list))
(defun world-server-status (world)
  "Return the running world server's status as a property list, or NIL when no server runs. Blocks briefly."
  (handler-case
      (let ((entry (world--entry world)))
        (when entry
          (rest (getf (rest (autolith::localgroup-query-record entry ':status)) :status))))
    (error ()
      nil)))

(-> world-request (world keyword &rest t) list)
(defun world-request (world operation &rest arguments)
  "Ask WORLD's server to perform OPERATION with ARGUMENTS and return its answer's fields.

Signals CLIENT-ERROR when no server runs or the server refuses."
  (let ((entry (world--entry world)))
    (unless entry
      (error 'client-error :message "No world server is running."))
    (handler-case (rest (autolith::localgroup-query-record entry operation arguments))
      (autolith::autolith-error (condition)
        (error 'client-error :message (condition-report condition))))))

(-> world-tell (world string string) list)
(defun world-tell (world channel text)
  "Send TEXT to CHANNEL as the person, the way (tell) does."
  (world-request world ':tell :channel channel :text text))

(-> world-close-channel (world string) list)
(defun world-close-channel (world channel)
  "Ask the server to stop serving CHANNEL; its history stays."
  (world-request world ':close :channel channel))

(-> world-move-agent (world string (or null string)) list)
(defun world-move-agent (world participant channel)
  "Move the agent PARTICIPANT names into CHANNEL."
  (world-request world ':move :participant participant :channel channel))

(-> world-dismiss-agent (world string) list)
(defun world-dismiss-agent (world participant)
  "Park the agent PARTICIPANT names, so it resides in no channel."
  (world-request world ':dismiss :participant participant))

(-> world-participants (world) list)
(defun world-participants (world)
  "Return the world's participants as property lists of :NAME, :KIND, :IDENTIFIER and :RESIDENCE, read from the durable world."
  (handler-case
      (let ((durable (autolith::world-load (world-configuration world))))
        (loop for participant in (autolith::world-participants durable)
              collect (list :name       (autolith::participant-name participant)
                            :kind       (autolith::participant-kind participant)
                            :identifier (autolith::participant-identifier participant)
                            :residence  (autolith::participant-residence participant))))
    (error ()
      nil)))


;;;; -- Deleting a Channel --

(-> world-delete-channel (world string) integer)
(defun world-delete-channel (world channel)
  "Delete CHANNEL and every channel nested in it, with their histories, and return how many were deleted.

Autolith refuses for a channel that is shown somewhere or still has work
running; its reason is then signalled as a CLIENT-ERROR."
  (handler-case (autolith::channel-remove (world-configuration world) (list channel) :tool-name "the window")
    (autolith::autolith-error (condition)
      (error 'client-error :message (autolith::autolith-error-message condition)))
    (error (condition)
      (error 'client-error
             :message (format nil "The channel could not be deleted: ~A" (condition-report condition))))))


;;;; -- Signing In --

;;; A provider is signed in to by Autolith's own code, the code its auth
;;; command runs, in this process: a key is saved to Autolith's key store, a
;;; subscription opens the browser and waits for it. Nothing about either is
;;; reimplemented here.

(-> world--provider (world string) t)
(defun world--provider (world name)
  "Return Autolith's provider registered as NAME for signing in to."
  (autolith::provider-authentication-provider (world-configuration world) name))

(-> world-sign-in-kind (world string) (or null keyword))
(defun world-sign-in-kind (world name)
  "Return how the provider NAME is signed in to: :KEY for an API key, :BROWSER for a login in the browser, or NIL when this client does not know the provider."
  (handler-case
      (let ((manager (autolith::provider-credential-manager (world--provider world name))))
        (if (typep manager 'autolith::api-key-credential-manager)
            ':key
            ':browser))
    (error ()
      nil)))

(-> world-sign-in (world string &key (:key (or null string))) string)
(defun world-sign-in (world name &key key)
  "Sign in to the provider NAME and return what Autolith says about it.

With KEY the key is checked and saved; without, the browser is opened and
this waits until the login there is done. Signals a CLIENT-ERROR with
Autolith's reason when the sign-in fails."
  (handler-case
      (let ((provider (world--provider world name))
            (output   (make-string-output-stream)))
        (autolith::configuration-ensure-directories (world-configuration world))
        (with-input-from-string (input (format nil "~A~%" (or key "")))
          (let ((*standard-input*  input)
                (*standard-output* output))
            (autolith::provider-authenticate-with-method provider nil
                                                         :stream         output
                                                         :open-browser-p (null key)))))
    (autolith::autolith-error (condition)
      (error 'client-error :message (autolith::autolith-error-message condition)))
    (error (condition)
      (error 'client-error :message (condition-report condition)))))

(-> world-providers () list)
(defun world-providers ()
  "Return the names of the providers this client knows how to sign in to."
  (handler-case (mapcar #'autolith::provider-registration-name (autolith::provider-registrations))
    (error ()
      nil)))
