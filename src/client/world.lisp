(in-package #:aui)

;;;; -- World Client --

;;; AUI's handle on Autolith's world of channels, through the client code
;;; Autolith's own terminal uses: the world endpoint record to find the
;;; server, WORLD-CLIENT-OPEN to open a channel in it, and WORLD-SIDEBAR for
;;; the tree of channels. When no server runs, the autolith command starts
;;; one, as it would from a terminal. Everything
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
    :documentation "Autolith's sidebar model of the world's channels, once started.")
   (rows
    :initform      nil
    :accessor      world--rows
    :type          list
    :documentation "What the sidebar model last gathered and the rows made from it, as (GATHERED . ROWS), or NIL."))
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

(-> world--gathered-rows (list) list)
(defun world--gathered-rows (gathered)
  "Return the rows of the channels in GATHERED, what a sidebar model of an Autolith before 0.67.57 last gathered.

Such a model lists only what its open nodes show and names no parents, so
the rows are made from what it gathered."
  (let ((names    (getf gathered :names))
        (statuses (getf gathered :statuses)))
    (loop for channel in (getf gathered :channels)
          for identifier = (autolith::channel-identifier channel)
          for status = (find identifier statuses
                             :key  (lambda (status)
                                     (getf status :session-id))
                             :test #'equal)
          collect (list :id                 identifier
                        :parent             (autolith::channel-parent channel)
                        :name               (rest (assoc identifier names :test #'equal))
                        :title              (autolith::channel-title channel)
                        :state              (if status (getf status :state) ':absent)
                        :waiting-for-jobs-p (and status
                                                 (getf status :waiting-for-input-p)
                                                 (plusp (or (getf status :job-live-count) 0))
                                                 t)))))

(-> world-channel-rows (world) list)
(defun world-channel-rows (world)
  "Return every channel of WORLD but home, whether or not the tree shows it now, in the order of the tree.

Each is a property list with :ID, :PARENT, the channel it was started from,
:NAME, its agent's, :TITLE, :STATE and :WAITING-FOR-JOBS-P. They are the
channels of the sidebar model's whole tree, which Autolith gives from 0.67.57
on. The rows are made once for each gathering of the tree."
  (let* ((sidebar  (world--sidebar world))
         (gathered (and sidebar (autolith::world-sidebar-inputs sidebar)))
         (known    (world--rows world)))
    (if (and known (eq (first known) gathered))
        (rest known)
        (let ((rows (cond
                      ((null gathered)
                       nil)
                      ((fboundp 'autolith::world-sidebar-tree)
                       (remove-if-not (lambda (entry)
                                        (eq (getf entry :kind) ':channel))
                                      (funcall 'autolith::world-sidebar-tree sidebar)))
                      (t
                       (world--gathered-rows gathered)))))
          (setf (world--rows world) (cons gathered rows))
          rows))))

(-> channel-row-working-p (list) boolean)
(defun channel-row-working-p (row)
  "Return T when the channel the row ROW describes is doing work."
  (and (member (getf row :state) '(:active :working :cancelling)) t))

(-> channel-row--rank (list) integer)
(defun channel-row--rank (row)
  "Return how far down among its siblings the channel of ROW is listed: one at work first, then one that waits for jobs, then any other the server holds, and last one it does not hold."
  (cond
    ((channel-row-working-p row)
     0)
    ((getf row :waiting-for-jobs-p)
     1)
    ((eq (getf row :state) ':absent)
     3)
    (t
     2)))

(-> channel-descendants ((or null string) list) list)
(defun channel-descendants (identifier rows)
  "Return those of ROWS that stand beneath channel IDENTIFIER, each channel before the ones started from it, each with a :DEPTH that is 0 for a channel started from IDENTIFIER itself.

A row is a property list with an :ID and a :PARENT. Among the channels
started from one channel, those at work come first and those the server does
not hold last, and otherwise they keep the order of ROWS. A channel is listed
once, so rows that name each other as parents end."
  (let ((seen (list identifier)))
    (labels ((beneath (parent depth)
               (let ((children (stable-sort
                                (loop for row in rows
                                      for child = (getf row :id)
                                      when (and (stringp child)
                                                (equal (getf row :parent) parent)
                                                (not (member child seen :test #'equal)))
                                        collect (progn
                                                  (push child seen)
                                                  row))
                                #'<
                                :key #'channel-row--rank)))
                 (loop for row in children
                       append (cons (list* :depth depth row) (beneath (getf row :id) (1+ depth)))))))
      (and identifier (beneath identifier 0)))))

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

(-> world--last-line (string) string)
(defun world--last-line (text)
  "Return the last line of TEXT that says anything, or the empty string."
  (or (find-if (lambda (line)
                 (plusp (length line)))
               (mapcar (lambda (line)
                         (string-trim '(#\Space #\Tab #\Return) line))
                       (uiop:split-string text :separator '(#\Newline)))
               :from-end t)
      ""))

(-> world-served-p (world) boolean)
(defun world-served-p (world)
  "Return T when WORLD's server answers or is starting."
  (let ((configuration (world-configuration world)))
    (and (or (autolith::world-client-entry configuration)
             (autolith::world-server-running-p configuration))
         t)))

(-> world--run-autolith (world &rest string) (values (or null integer) (or null string)))
(defun world--run-autolith (world &rest arguments)
  "Run the autolith command on ARGUMENTS for WORLD, and return its exit code and the last thing it said, NIL when it said nothing.

The command is the launcher of the Autolith this client was built against. A
site configuration root this start named goes to it through its environment."
  (let ((site (autolith::config :site-config-root (world-configuration world))))
    (multiple-value-bind (output complaint code)
        (uiop:run-program (append (when site
                                    (list "env" (format nil "AUTOLITH_SITE_CONFIG_ROOT=~A"
                                                        (uiop:native-namestring site))))
                                  (cons (installation-program) arguments))
                          :input               nil
                          :output              ':string
                          :error-output        ':string
                          :ignore-error-status t)
      (let ((said (world--last-line (if (plusp (length (world--last-line complaint)))
                                        complaint
                                        output))))
        (values code (and (plusp (length said)) said))))))

(-> world-start-server (world) null)
(defun world-start-server (world)
  "Start WORLD's server with the autolith command when none runs, and return once that command has finished.

The command prepares the image and the helpers a server needs before it
starts one, which a release needs on its first start, so the server AUI
starts is the one a terminal would have started. Signal a CLIENT-ERROR with
what the command said when it fails and no server runs."
  (unless (world-served-p world)
    (multiple-value-bind (code said) (world--run-autolith world "start")
      (unless (or (eql code 0) (world-served-p world))
        (error 'client-error
               :message (format nil "The world server did not start~:[.~;: ~:*~A~]" said)))))
  nil)

(-> world-stop-server (world) null)
(defun world-stop-server (world)
  "Stop WORLD's server with the autolith command, and with it the work in every channel, and return once that command has finished.

The channels and their histories stay; the next channel opened starts a
server again. Signal a CLIENT-ERROR with what the command said when it fails
and the server still answers."
  (when (world--entry world)
    (multiple-value-bind (code said) (world--run-autolith world "stop")
      (unless (or (eql code 0) (null (world--entry world)))
        (error 'client-error
               :message (format nil "The world server did not stop~:[.~;: ~:*~A~]" said)))))
  nil)

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
