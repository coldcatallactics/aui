(in-package #:aui)

;;;; -- Main --

;;; Startup, the event loop, and the hand-over from the welcome window to a
;;; workbench. Everything here runs on the FLTK thread.

(defvar *queue* nil
  "The queue every backend posts to, drained by the event loop.")

(-> drain-ui-queue () null)
(defun drain-ui-queue ()
  "Apply every pending event to its workbench."
  (when *queue*
    (dolist (event (ui-events-coalesce (ui-queue-drain *queue*)))
      (workbench-dispatch event)))
  nil)

(-> open-workspace (string &key (:conversation-id (or null string))) workbench)
(defun open-workspace (directory &key conversation-id)
  "Start a backend for DIRECTORY, open CONVERSATION-ID or a new conversation,
show the workbench and return it."
  (let* ((backend (make-backend directory *queue*))
         (workbench (make-workbench backend)))
    (lightfast:show (workbench-window workbench))
    (backend-start backend)
    (backend-open-conversation backend :conversation-id conversation-id)
    workbench))

(-> show-welcome () welcome)
(defun show-welcome ()
  "Show the welcome window; choosing there opens a workspace and closes it."
  (let ((welcome nil))
    (setf welcome
          (make-welcome :entries (recent-conversations)
                        :on-open (lambda (directory conversation-id)
                                   (open-workspace directory :conversation-id conversation-id)
                                   (lightfast:hide (welcome-window welcome)))))
    (lightfast:show (welcome-window welcome))
    welcome))

(-> screenshot-after (real string) null)
(defun screenshot-after (seconds path)
  "Capture the AUI window to PATH with script/shot after SECONDS, then quit."
  (lightfast:add-timeout seconds
                         (lambda ()
                           (uiop:run-program (list (namestring (asdf:system-relative-pathname :aui "script/shot"))
                                                   path)
                                             :ignore-error-status t :error-output *error-output*)
                           (lightfast:quit)))
  nil)

(-> parse-arguments (list) list)
(defun parse-arguments (arguments)
  "Return a plist from command line ARGUMENTS: :DIRECTORY, :CONVERSATION-ID,
:DEMO-P, :SHOT-PATH, :DEMO-PROMPT and :OPEN-VIEW."
  (let ((result nil))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (cond
                 ((string= argument "--demo") (setf (getf result ':demo-p) t))
                 ((string= argument "--conversation") (setf (getf result ':conversation-id) (pop arguments)))
                 ((string= argument "--shot") (setf (getf result ':shot-path) (pop arguments)))
                 ((string= argument "--demo-prompt") (setf (getf result ':demo-prompt) (pop arguments)))
                 ((string= argument "--open") (setf (getf result ':open-view) (intern (string-upcase (pop arguments)) :keyword)))
                 ((and (plusp (length argument)) (char= #\- (char argument 0)))
                  (error "Unknown option ~A." argument))
                 (t (setf (getf result ':directory) argument)))))
    result))

(-> main (&key (:arguments list)) null)
(defun main (&key (arguments (uiop:command-line-arguments)))
  "Run AUI. With a directory argument, open a workspace there at once;
otherwise show the welcome window. --conversation ID resumes a conversation,
--demo uses the scripted backend, --open settings or --open palette opens that
view, and --shot PATH captures the window and exits."
  (destructuring-bind (&key directory conversation-id demo-p shot-path demo-prompt open-view)
      (parse-arguments arguments)
    (lightfast:load-library)
    (lightfast:enable-thread-wakeups)
    (theme-install)
    (setf *queue* (make-ui-queue :wakeup #'lightfast:awake))
    (let ((*backend-factory* (if demo-p #'make-scripted-backend *backend-factory*))
          (*recent-conversations-function* (if demo-p #'sample-conversations *recent-conversations-function*)))
      (cond
        (directory
         (let ((workbench (open-workspace (namestring (uiop:ensure-directory-pathname directory))
                                          :conversation-id conversation-id)))
           (when demo-prompt
             (lightfast:add-timeout 1.0 (lambda ()
                                          (setf (lightfast:value (workbench-composer workbench)) demo-prompt)
                                          (workbench-send workbench))))
           (when open-view
             (lightfast:add-timeout 0.5 (lambda ()
                                          (ecase open-view
                                            (:settings (workbench-show-settings workbench))
                                            (:palette (workbench-show-palette workbench))))))))
        (t
         (show-welcome)))
      (when shot-path
        (screenshot-after (if demo-prompt 4.5 2.0) shot-path))
      (lightfast:run-with-idle #'drain-ui-queue)))
  nil)
