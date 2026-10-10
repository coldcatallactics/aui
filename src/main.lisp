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

(defparameter *backend-stop-timeout* 5
  "Seconds to wait for each backend to release its Autolith process at exit.")

(-> stop-all-backends () null)
(defun stop-all-backends ()
  "Stop every backend still open and wait for all of them, so that no owned
Autolith process outlives the application."
  (dolist (workbench *workbenches*)
    (workbench-close workbench))
  (dolist (backend *stopped-backends*)
    (backend-join backend :timeout *backend-stop-timeout*))
  (setf *stopped-backends* nil)
  nil)

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
:DEMO-P, :SHOT-PATH, :SHOT-AFTER, :SIZE, :DEMO-PROMPT and :OPEN-VIEW."
  (let ((result nil))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (cond
                 ((string= argument "--demo") (setf (getf result ':demo-p) t))
                 ((string= argument "--conversation") (setf (getf result ':conversation-id) (pop arguments)))
                 ((string= argument "--shot") (setf (getf result ':shot-path) (pop arguments)))
                 ((string= argument "--shot-after")
                  (setf (getf result ':shot-after) (parse-integer (pop arguments))))
                 ((string= argument "--size")
                  (let* ((text (pop arguments))
                         (separator (position #\x text)))
                    (unless separator
                      (error "--size wants WIDTHxHEIGHT, not ~A." text))
                    (setf (getf result ':size) (list (parse-integer text :end separator)
                                                     (parse-integer text :start (1+ separator))))))
                 ((string= argument "--demo-prompt") (setf (getf result ':demo-prompt) (pop arguments)))
                 ((string= argument "--open") (setf (getf result ':open-view) (intern (string-upcase (pop arguments)) :keyword)))
                 ((and (plusp (length argument)) (char= #\- (char argument 0)))
                  (error "Unknown option ~A." argument))
                 (t (setf (getf result ':directory) argument)))))
    result))

(-> fix-window-size (t integer integer) null)
(defun fix-window-size (window width height)
  "Pin WINDOW to WIDTH by HEIGHT. A tiling compositor keeps a window with equal
minimum and maximum sizes at that size, which is how other widths are inspected."
  (lightfast:set-size-range window :min-width width :min-height height
                                   :max-width width :max-height height)
  (lightfast:resize-widget window :width width :height height)
  nil)

(-> open-view (workbench keyword) null)
(defun open-view (workbench view)
  "Open VIEW, one of :SETTINGS, :PALETTE or :INSPECTOR, on WORKBENCH."
  (ecase view
    (:settings (workbench-show-settings workbench))
    (:palette (workbench-show-palette workbench))
    (:inspector
     (let ((block (transcript-last-block (workbench-transcript workbench) 'tool-call)))
       (when block
         (workbench-inspect-block workbench (transcript-block-id block))))))
  nil)

(-> main (&key (:arguments list)) null)
(defun main (&key (arguments (uiop:command-line-arguments)))
  "Run AUI. With a directory argument, open a workspace there at once;
otherwise show the welcome window. --conversation ID resumes a conversation,
--demo uses the scripted backend, --open settings, palette or inspector opens
that view, --demo-prompt TEXT sends TEXT once the session is ready, --size
WIDTHxHEIGHT pins the window size for layout inspection, and --shot PATH
captures the window and exits after --shot-after SECONDS."
  (destructuring-bind (&key directory conversation-id demo-p shot-path shot-after size demo-prompt open-view)
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
           (when size
             (fix-window-size (workbench-window workbench) (first size) (second size)))
           (when demo-prompt
             (setf (workbench-queued-prompt workbench) demo-prompt))
           (when open-view
             (lightfast:add-timeout 0.5 (lambda () (open-view workbench open-view))))))
        (t
         (show-welcome)))
      (when shot-path
        (screenshot-after (or shot-after (if demo-prompt 5 2)) shot-path))
      (lightfast:run-with-idle #'drain-ui-queue)
      (stop-all-backends)))
  nil)
