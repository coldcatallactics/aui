(in-package #:aui)

;;;; -- Launch --

;;; The aui command line is Autolith's. Its options are the option objects
;;; Autolith's own top-level command defines, parsed by the same library and
;;; read by the same functions, so a flag means here what it means there.
;;; Options that concern the channel (--model, --provider, --permissions,
;;; --immutable, --image, --site-config-root) reach the world server exactly
;;; as a terminal sends them. Only a start that would open a channel in a
;;; terminal opens it in a window instead. Everything else (auth, models, status,
;;; update, data, usage, replay, an unattended or a rescue start) has no
;;; window, and this process becomes autolith to do it: Autolith's own image
;;; holds the person's private definitions and knows which of them still
;;; apply, which an image built around a window does not.

(defclass launch ()
  ((world
    :initarg       :world
    :reader        launch-world
    :type          world
    :documentation "The world the window attaches to.")
   (channel
    :initarg       :channel
    :initform      nil
    :reader        launch-channel
    :type          (or null string)
    :documentation "The channel to open, resolved to its full identifier, or NIL to choose by directory.")
   (pick-p
    :initarg       :pick-p
    :initform      nil
    :reader        launch-pick-p
    :type          boolean
    :documentation "Whether the start asked to pick a channel instead of opening one, as aui open does without an identifier.")
   (images
    :initarg       :images
    :initform      nil
    :reader        launch-images
    :type          list
    :documentation "Namestrings of images to preload into the composer.")
   (demo-p
    :initarg       :demo-p
    :initform      nil
    :reader        launch-demo-p
    :type          boolean
    :documentation "Whether to show the scripted demonstration instead of attaching to a server.")
   (shot
    :initarg       :shot
    :initform      nil
    :reader        launch-shot
    :type          (or null string)
    :documentation "Where to write a picture of the window before exiting, or NIL.")
   (shot-after
    :initarg       :shot-after
    :initform      2
    :reader        launch-shot-after
    :type          real
    :documentation "Seconds to wait before the picture is taken.")
   (size
    :initarg       :size
    :initform      nil
    :reader        launch-size
    :type          (or null cons)
    :documentation "The window's (WIDTH . HEIGHT) to start with, or NIL for the default.")
   (view
    :initarg       :view
    :initform      nil
    :reader        launch-view
    :type          (or null string)
    :documentation "A view to show at start, such as settings or commands, or NIL.")
   (draft
    :initarg       :draft
    :initform      nil
    :reader        launch-draft
    :type          (or null string)
    :documentation "Text to put in the composer at start, or NIL.")
   (prompt
    :initarg       :prompt
    :initform      nil
    :accessor      launch-prompt
    :type          (or null string)
    :documentation "Text to send as soon as a channel is attached, or NIL once it was sent or when none was given."))
  (:documentation "What one invocation of aui asked for, once its options were read."))

(-> launch--window-options () list)
(defun launch--window-options ()
  "Return the options only a window understands, added to Autolith's session options."
  (list
   (clingon:make-option ':string
                        :short-name  #\C
                        :long-name   "directory"
                        :key         ':directory
                        :parameter   "DIRECTORY"
                        :persistent  t
                        :description "work in DIRECTORY instead of the current directory")
   (clingon:make-option ':flag
                        :long-name   "demo"
                        :key         ':demo
                        :persistent  t
                        :description "show a scripted conversation without contacting a server")
   (clingon:make-option ':string
                        :long-name   "shot"
                        :key         ':shot
                        :parameter   "FILE"
                        :persistent  t
                        :description "write a picture of the window to FILE and exit")
   (clingon:make-option ':string
                        :long-name   "shot-after"
                        :key         ':shot-after
                        :parameter   "SECONDS"
                        :persistent  t
                        :description "wait SECONDS before taking the picture")
   (clingon:make-option ':string
                        :long-name   "size"
                        :key         ':size
                        :parameter   "WIDTHxHEIGHT"
                        :persistent  t
                        :description "start with a window of this size")
   (clingon:make-option ':string
                        :long-name   "view"
                        :key         ':view
                        :parameter   "NAME"
                        :persistent  t
                        :description "show a view at start: commands, settings or channels")
   (clingon:make-option ':string
                        :long-name   "draft"
                        :key         ':draft
                        :parameter   "TEXT"
                        :persistent  t
                        :description "put TEXT in the composer at start")
   (clingon:make-option ':string
                        :long-name   "prompt"
                        :key         ':prompt
                        :parameter   "TEXT"
                        :persistent  t
                        :description "send TEXT as soon as the channel is attached")))

(-> launch--open-command () clingon:command)
(defun launch--open-command ()
  "Return aui's open sub-command, which opens a channel in a window."
  (clingon:make-command
   :name        "open"
   :description "open a channel in a window, or pick one"
   :usage       "[TARGET]"
   :handler     #'identity))

(-> launch-command () clingon:command)
(defun launch-command ()
  "Return aui's command line: Autolith's top-level command with the window's options added and its open sub-command replaced."
  (let ((autolith (autolith::main--top-level-command)))
    (clingon:make-command
     :name         "aui"
     :description  "a window onto Autolith's channels"
     :long-description
     "aui takes autolith's options and sub-commands. A start that would open a
channel in a terminal opens it in a window; every other sub-command runs as
it does under autolith."
     :version      (world-autolith-version)
     :options      (append (autolith::main--session-options) (launch--window-options))
     :sub-commands (cons (launch--open-command)
                         (remove "open" (clingon:command-sub-commands autolith)
                                 :key #'clingon:command-name :test #'string=))
     :handler      #'identity)))

(-> launch--parse-size ((or null string)) (or null cons))
(defun launch--parse-size (text)
  "Return the (WIDTH . HEIGHT) that TEXT, written as WIDTHxHEIGHT, names, or NIL when it names none."
  (let ((cross (and text (position #\x text))))
    (when cross
      (let ((width  (parse-integer text :end cross :junk-allowed t))
            (height (parse-integer text :start (1+ cross) :junk-allowed t)))
        (and width height (plusp width) (plusp height)
             (cons width height))))))

(defparameter *launch-window-option-keys*
  '(:directory :demo :shot :shot-after :size :view :draft :prompt)
  "The keys of the options only a window understands.")

(-> launch--windowless-p (clingon:command) boolean)
(defun launch--windowless-p (command)
  "Return T when COMMAND is Autolith's to run without a window.

That is every sub-command but open, and a start that is unattended, a rescue
session, or one of Autolith's internal starts."
  (and (or (not (member (clingon:command-name command) '("aui" "open") :test #'string=))
           (some (lambda (key)
                   (clingon:getopt* command key))
                 '(:detached :pristine :localgroup-handoff :simulate-crash)))
       t))

(-> launch--become-autolith (list) null)
(defun launch--become-autolith (arguments)
  "Replace this process with autolith run on ARGUMENTS.

Signal a CLIENT-ERROR when the program cannot be started; on success this
never returns."
  (let* ((program (installation-program))
         (words   (cons program arguments))
         (count   (length words))
         (vector  (sb-alien:make-alien sb-alien:c-string (1+ count))))
    (finish-output)
    (finish-output *error-output*)
    (loop for word in words
          for index from 0
          do (setf (sb-alien:deref vector index) word))
    (setf (sb-alien:deref vector count) nil)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "execv" (function sb-alien:int sb-alien:c-string (* sb-alien:c-string)))
     program vector)
    (error 'client-error
           :message (format nil "Could not start ~A: ~A." program (sb-int:strerror)))))

(-> launch--hand-over (clingon:command list) null)
(defun launch--hand-over (command arguments)
  "Give the windowless COMMAND, read from ARGUMENTS, to autolith.

Signal a CLIENT-ERROR when the arguments also carry an option that only a
window understands, which autolith would not know."
  (let ((given (remove-if-not (lambda (key)
                                (clingon:getopt* command key))
                              *launch-window-option-keys*)))
    (when given
      (error 'client-error
             :message (format nil "~{--~(~A~)~^, ~} ~:[belongs~;belong~] to a window, and this start has none."
                              given (rest given)))))
  (launch--become-autolith arguments))

(-> launch--window (clingon:command) launch)
(defun launch--window (command)
  "Return the window launch COMMAND describes, reading its options the way Autolith's session start does."
  (let* ((open-p          (string= (clingon:command-name command) "open"))
         (open-id         (and open-p (autolith::main--single-selection command "conversation ID")))
         (immutable-p     (and (clingon:getopt* command ':immutable) t))
         (directory       (clingon:getopt* command ':directory))
         (configuration   (apply #'autolith::configuration-create
                                 :immutable-p                 immutable-p
                                 :defer-provider-validation-p t
                                 (append
                                  (when directory
                                    (list :working-directory
                                          (autolith::platform-truename
                                           autolith::*platform*
                                           (uiop:ensure-directory-pathname directory))))
                                  (autolith::main--site-root-arguments command)
                                  (autolith::main--model-arguments command))))
         (permission-mode (or (clingon:getopt* command ':permissions)
                              (autolith::config :permission-mode configuration)
                              ':ask))
         (demo-p          (and (clingon:getopt* command ':demo) t))
         (shot-after      (let ((text (clingon:getopt* command ':shot-after)))
                            (or (and text (parse-integer text :junk-allowed t)) 2))))
    (when (and (not open-p) (clingon:command-arguments command))
      (error 'autolith::configuration-error
             :message (format nil "Unknown command ~S." (first (clingon:command-arguments command)))))
    (make-instance
     'launch
     :world      (make-instance 'world
                                :configuration   configuration
                                :permission-mode permission-mode
                                :immutable-p     immutable-p
                                :launch          (autolith::main--launch-options
                                                  command :permission-mode permission-mode))
     :channel    (and open-id (not demo-p)
                      (autolith::main-resolve-conversation configuration open-id))
     :pick-p     (and open-p (null open-id))
     :images     (mapcar #'namestring
                         (autolith::main--image-pathnames (clingon:getopt* command ':images)))
     :demo-p     demo-p
     :shot       (clingon:getopt* command ':shot)
     :shot-after shot-after
     :size       (launch--parse-size (clingon:getopt* command ':size))
     :view       (clingon:getopt* command ':view)
     :draft      (clingon:getopt* command ':draft)
     :prompt     (clingon:getopt* command ':prompt))))

(-> launch--read (list) clingon:command)
(defun launch--read (arguments)
  "Return the command the command line ARGUMENTS name, with its options read.

Help, version and usage errors exit the process the way they do under
autolith."
  (handler-case
      (clingon:parse-command-line (launch-command)
                                  (autolith::main--normalize-update-arguments arguments))
    (clingon:exit-error (condition)
      (uiop:quit (clingon:exit-error-code condition)))
    (error (condition)
      (format *error-output* "~&~A~%" condition)
      (uiop:quit 64))))

(-> launch-parse (list) (or null launch))
(defun launch-parse (arguments)
  "Read the command line ARGUMENTS and return the window launch they ask for.

Arguments that name something Autolith does without a window turn this
process into autolith, so the call does not return for them."
  (let ((command (launch--read arguments)))
    (if (launch--windowless-p command)
        (launch--hand-over command arguments)
        (launch--window command))))
