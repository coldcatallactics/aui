(in-package #:aui)

;;;; -- Catalogs --

;;; The data shapes the views render for things that are not transcript
;;; blocks: conversations, jobs, settings, models, operations and the session
;;; state snapshot. The backend fills them from Autolith; the view reads them.

(defclass conversation-entry ()
  ((id               :initarg :id :reader conversation-entry-id :type string
                     :documentation "The durable Autolith conversation id.")
   (display-id       :initarg :display-id :initform nil :reader conversation-entry-display-id
                     :documentation "The hyphenated form shown to people.")
   (title            :initarg :title :initform nil :reader conversation-entry-title
                     :documentation "The conversation title, when one exists.")
   (directory        :initarg :directory :reader conversation-entry-directory :type string
                     :documentation "The workspace directory as a namestring.")
   (model            :initarg :model :initform nil :reader conversation-entry-model)
   (created-at       :initarg :created-at :initform nil :reader conversation-entry-created-at
                     :documentation "Universal time of creation.")
   (last-activity-at :initarg :last-activity-at :initform nil :reader conversation-entry-last-activity-at
                     :documentation "Universal time of the last recorded activity.")
   (user-turn-count  :initarg :user-turn-count :initform 0 :reader conversation-entry-user-turn-count
                     :type integer)
   (preview          :initarg :preview :initform nil :reader conversation-entry-preview
                     :documentation "A short excerpt of the latest exchange.")
   (live-p           :initarg :live-p :initform nil :reader conversation-entry-live-p :type boolean
                     :documentation "T when a running session owns this conversation."))
  (:documentation "A conversation as the welcome screen and rail list it."))

(defclass job-entry ()
  ((id              :initarg :id :reader job-entry-id :type string)
   (parent-id       :initarg :parent-id :initform nil :reader job-entry-parent-id
                    :documentation "The owning job, or NIL for a root.")
   (role            :initarg :role :initform nil :reader job-entry-role
                    :documentation "The child role or tool name.")
   (state           :initarg :state :reader job-entry-state :type keyword
                    :documentation "The backend's job state, such as :RUNNING or :COMPLETED.")
   (started-at      :initarg :started-at :initform nil :reader job-entry-started-at)
   (elapsed-seconds :initarg :elapsed-seconds :initform nil :reader job-entry-elapsed-seconds)
   (usage           :initarg :usage :initform nil :reader job-entry-usage
                    :documentation "Reported usage as an immutable plist, or NIL when unknown.")
   (artifacts       :initarg :artifacts :initform nil :reader job-entry-artifacts
                    :documentation "Artifact references for the inspector.")
   (activity        :initarg :activity :initform nil :reader job-entry-activity
                    :documentation "The latest activity text."))
  (:documentation "One owned job, root or descendant, as the rail lists it."))

(defclass setting-entry ()
  ((name          :initarg :name :reader setting-entry-name :type keyword)
   (label         :initarg :label :reader setting-entry-label :type string)
   (documentation :initarg :documentation :initform "" :reader setting-entry-documentation :type string)
   (group         :initarg :group :initform nil :reader setting-entry-group
                  :documentation "The heading the settings sheet files it under.")
   (type          :initarg :type :reader setting-entry-type :type keyword
                  :documentation "One of :BOOLEAN :CHOICE :INTEGER :STRING :PATHNAME :DERIVED.")
   (options       :initarg :options :initform nil :reader setting-entry-options
                  :documentation "The finite choices, for a :CHOICE setting.")
   (value         :initarg :value :initform nil :reader setting-entry-value)
   (source        :initarg :source :initform nil :reader setting-entry-source
                  :documentation "Where the value came from: override, environment, file, default.")
   (scope         :initarg :scope :initform nil :reader setting-entry-scope
                  :documentation "One of :PROCESS :DURABLE :SESSION :DERIVED.")
   (adjustable-p  :initarg :adjustable-p :initform nil :reader setting-entry-adjustable-p :type boolean)
   (reason        :initarg :reason :initform nil :reader setting-entry-reason
                  :documentation "Why the setting cannot change now, when it cannot."))
  (:documentation "One Autolith setting as the settings sheet shows it."))

(defclass model-entry ()
  ((id             :initarg :id :reader model-entry-id :type string)
   (provider       :initarg :provider :initform nil :reader model-entry-provider)
   (credential-p   :initarg :credential-p :initform nil :reader model-entry-credential-p :type boolean
                   :documentation "T when a credential is stored for its provider.")
   (verified-p     :initarg :verified-p :initform nil :reader model-entry-verified-p :type boolean
                   :documentation "T when a request through it succeeded recently.")
   (context-window :initarg :context-window :initform nil :reader model-entry-context-window))
  (:documentation "One model the picker offers."))

(defclass operation-entry ()
  ((name             :initarg :name :reader operation-entry-name :type string)
   (kind             :initarg :kind :reader operation-entry-kind :type (member :command :tool))
   (description      :initarg :description :initform "" :reader operation-entry-description :type string)
   (argument-schema  :initarg :argument-schema :initform nil :reader operation-entry-argument-schema
                     :documentation "The argument description as immutable data.")
   (admitted-p       :initarg :admitted-p :initform nil :reader operation-entry-admitted-p :type boolean
                     :documentation "T when it may run right now.")
   (terminal-owning-p :initarg :terminal-owning-p :initform nil :reader operation-entry-terminal-owning-p
                      :type boolean
                      :documentation "T when it needs a terminal and so is unsupported here."))
  (:documentation "One command or tool the palette offers."))

(defclass state-snapshot ()
  ((workspace       :initarg :workspace :initform nil :reader state-snapshot-workspace)
   (title           :initarg :title :initform nil :reader state-snapshot-title)
   (model           :initarg :model :initform nil :reader state-snapshot-model)
   (effort          :initarg :effort :initform nil :reader state-snapshot-effort)
   (permission-mode :initarg :permission-mode :initform nil :reader state-snapshot-permission-mode)
   (turn-state      :initarg :turn-state :initform nil :reader state-snapshot-turn-state)
   (usage           :initarg :usage :initform nil :reader state-snapshot-usage
                    :documentation "A plist with :INPUT :OUTPUT :CACHED :CONTEXT-USED :CONTEXT-LIMIT
and :CONTEXT-MEASURED-P; absent keys are unknown, never zero.")
   (live-job-count  :initarg :live-job-count :initform nil :reader state-snapshot-live-job-count)
   (plan-p          :initarg :plan-p :initform nil :reader state-snapshot-plan-p :type boolean)
   (health          :initarg :health :initform nil :reader state-snapshot-health
                    :documentation "A short health word or NIL when unknown."))
  (:documentation "What the status line and header show about the session."))


;;; Identifiers

(-> conversation-display-id (string) string)
(defun conversation-display-id (id)
  "Return the conversation ID as Autolith shows it: the first character, a
hyphen, then the rest, so that a prefix is easy to remember and type."
  (if (and (> (length id) 1) (not (find #\- id)))
      (format nil "~A-~A" (subseq id 0 1) (subseq id 1))
      id))

(-> conversation-entry-name (conversation-entry) string)
(defun conversation-entry-name (entry)
  "Return ENTRY's title, or its display id when it has none."
  (or (conversation-entry-title entry)
      (conversation-entry-display-id entry)
      (conversation-display-id (conversation-entry-id entry))))


;;; Ordering helpers for the welcome screen

(-> conversation-entry-activity-key (conversation-entry) integer)
(defun conversation-entry-activity-key (entry)
  "Return the time to sort ENTRY by: its last activity, else its creation, else zero."
  (or (conversation-entry-last-activity-at entry)
      (conversation-entry-created-at entry)
      0))

(-> conversations-by-recency (list) list)
(defun conversations-by-recency (entries)
  "Return ENTRIES most recently active first, live ones ahead at equal times."
  (stable-sort (copy-list entries)
               (lambda (first-entry second-entry)
                 (let ((first-key (conversation-entry-activity-key first-entry))
                       (second-key (conversation-entry-activity-key second-entry)))
                   (or (> first-key second-key)
                       (and (= first-key second-key)
                            (conversation-entry-live-p first-entry)
                            (not (conversation-entry-live-p second-entry))))))))

(-> recent-directories (list) list)
(defun recent-directories (entries)
  "Return (DIRECTORY . LAST-ACTIVITY) pairs for the directories in ENTRIES,
most recently active first, one pair per directory."
  (let ((latest (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((directory (conversation-entry-directory entry))
            (key (conversation-entry-activity-key entry)))
        (setf (gethash directory latest) (max key (gethash directory latest 0)))))
    (let ((pairs nil))
      (maphash (lambda (directory key) (push (cons directory key) pairs)) latest)
      (stable-sort (sort pairs #'string< :key #'first) #'> :key #'rest))))

(-> state-snapshot-context-fraction (state-snapshot) (or null real))
(defun state-snapshot-context-fraction (snapshot)
  "Return the context occupancy of SNAPSHOT as a fraction, or NIL when unknown."
  (let* ((usage (state-snapshot-usage snapshot))
         (used (getf usage ':context-used))
         (limit (getf usage ':context-limit)))
    (when (and used limit (plusp limit))
      (/ used limit))))
