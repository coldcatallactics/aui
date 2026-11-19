(in-package #:aui)

;;;; -- Launch --

;;; The command line of aui is the command line of Autolith. The options are
;;; the option objects defined by the top-level command of Autolith. They are
;;; parsed by the same library and read by the same functions, so each flag
;;; has the same meaning as in autolith. Options for the channel (--model,
;;; --provider, --permissions, --immutable, --image, --site-config-root) are
;;; sent to the world server in the same way as from a terminal. A channel
;;; that autolith would open in a terminal is opened in a window. For all
;;; other commands (auth, models, status, update, data, usage, replay, an
;;; unattended or a rescue start), this process becomes autolith. This is
;;; because the private definitions of the person are only loaded in the image
;;; of Autolith.

(defclass launch ()
  ((world
    :initarg       :world
    :reader        launch-world
    :type          world
    :documentation "The world to which the window attaches.")
   (channel
    :initarg       :channel
    :initform      nil
    :reader        launch-channel
    :type          (or null string)
    :documentation "The full identifier of the channel to open, or NIL.
If it is NIL, the channel is chosen by directory.")
   (pick-p
    :initarg       :pick-p
    :initform      nil
    :reader        launch-pick-p
    :type          boolean
    :documentation "True if the person asked to pick a channel instead of opening one, as with aui open without an identifier.")
   (images
    :initarg       :images
    :initform      nil
    :reader        launch-images
    :type          list
    :documentation "The namestrings of the images to put in the composer at start.")
   (demo-p
    :initarg       :demo-p
    :initform      nil
    :reader        launch-demo-p
    :type          boolean
    :documentation "True if the scripted demonstration is shown instead of attaching to a server.")
   (shot
    :initarg       :shot
    :initform      nil
    :reader        launch-shot
    :type          (or null string)
    :documentation "The file to which a picture of the window is written before exit, or NIL.")
   (shot-after
    :initarg       :shot-after
    :initform      2
    :reader        launch-shot-after
    :type          real
    :documentation "The number of seconds to wait before the picture is taken.")
   (size
    :initarg       :size
    :initform      nil
    :reader        launch-size
    :type          (or null cons)
    :documentation "The initial size of the window as (WIDTH . HEIGHT), or NIL for the default size.")
   (view
    :initarg       :view
    :initform      nil
    :reader        launch-view
    :type          (or null keyword)
    :documentation "The view to show at start, or NIL. The view is one of `*launch-views*'.")
   (draft
    :initarg       :draft
    :initform      nil
    :reader        launch-draft
    :type          (or null string)
    :documentation "The text to put in the composer at start, or NIL.")
   (prompt
    :initarg       :prompt
    :initform      nil
    :accessor      launch-prompt
    :type          (or null string)
    :documentation "The text to send when a channel is attached, or NIL.
It is NIL if no text was given or after the text was sent."))
  (:documentation "The options of one invocation of aui, after they were read."))

(-> launch--window-options () list)
(defun launch--window-options ()
  "Return the window options that are added to the session options of Autolith."
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
  "Return the open sub-command of aui. This sub-command opens a channel in a window."
  (clingon:make-command
   :name        "open"
   :description "open a channel in a window, or pick one"
   :usage       "[TARGET]"
   :handler     #'identity))

(-> launch-command () clingon:command)
(defun launch-command ()
  "Return the command line of aui.
It is the top-level command of Autolith with the window options and with a new open sub-command."
  (let ((autolith (autolith::main--top-level-command)))
    (clingon:make-command
     :name         "aui"
     :description  "a graphical client for Autolith"
     :long-description
     "aui has the same options and sub-commands as autolith. A channel that
autolith would open in a terminal is opened in a window. All other
sub-commands work as under autolith."
     :version      (world-autolith-version)
     :options      (append (autolith::main--session-options) (launch--window-options))
     :sub-commands (cons (launch--open-command)
                         (remove "open" (clingon:command-sub-commands autolith)
                                 :key #'clingon:command-name :test #'string=))
     :handler      #'identity)))

(-> launch--parse-size ((or null string)) (or null cons))
(defun launch--parse-size (text)
  "Return the size written as WIDTHxHEIGHT in TEXT as (WIDTH . HEIGHT).
Return NIL if TEXT does not contain a valid size."
  (let ((cross (and text (position #\x text))))
    (when cross
      (let ((width  (parse-integer text :end cross :junk-allowed t))
            (height (parse-integer text :start (1+ cross) :junk-allowed t)))
        (and width height (plusp width) (plusp height)
             (cons width height))))))

(defparameter *launch-window-option-keys*
  '(:directory :demo :shot :shot-after :size :view :draft :prompt)
  "The keys of the window options.")

(-> launch--windowless-p (clingon:command) boolean)
(defun launch--windowless-p (command)
  "Return T if autolith runs COMMAND without a window.

These commands are all sub-commands other than open, unattended starts,
rescue sessions and the internal starts of Autolith."
  (and (or (not (member (clingon:command-name command) '("aui" "open") :test #'string=))
           (some (lambda (key)
                   (clingon:getopt* command key))
                 '(:detached :pristine :localgroup-handoff :simulate-crash)))
       t))

(-> launch--become-autolith (list) null)
(defun launch--become-autolith (arguments)
  "Replace this process with autolith, run with ARGUMENTS.

This function does not return if autolith starts. Signal a CLIENT-ERROR if the
program cannot be started."
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
           :message (format nil "~A could not be started: ~A." program (sb-int:strerror)))))

(-> launch--hand-over (clingon:command list) null)
(defun launch--hand-over (command arguments)
  "Run the windowless COMMAND, read from ARGUMENTS, with autolith.

Signal a CLIENT-ERROR if ARGUMENTS also contain window options, because
autolith does not know them."
  (let ((given (remove-if-not (lambda (key)
                                (clingon:getopt* command key))
                              *launch-window-option-keys*)))
    (when given
      (error 'client-error
             :message (format nil "~{--~(~A~)~^, ~} ~:[is a window option~;are window options~]. This command does not open a window."
                              given (rest given)))))
  (launch--become-autolith arguments))

(-> launch--window (clingon:command) launch)
(defun launch--window (command)
  "Return the window launch described by COMMAND.
The options are read in the same way as in the session start of Autolith."
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
     :view       (launch--parse-view (clingon:getopt* command ':view))
     :draft      (clingon:getopt* command ':draft)
     :prompt     (clingon:getopt* command ':prompt))))

(defparameter *launch-views*
  '(:commands :settings :channels :agenda :memories :plan :model :permissions :menu :choice :prompt :top
    :approval :welcome)
  "The views that can be shown at start.")

(-> launch--parse-view ((or null string)) (or null keyword))
(defun launch--parse-view (name)
  "Return the view named by the option value NAME, or NIL if NAME is NIL.
Signal a `client-error' if NAME is not the name of a view."
  (and name
       (or (find name *launch-views* :test #'string-equal)
           (error 'client-error
                  :message (format nil "There is no view ~A. Choose one of ~{~(~A~)~^, ~}." name *launch-views*)))))

(-> launch--read (list) clingon:command)
(defun launch--read (arguments)
  "Parse the command line ARGUMENTS and return the command with the options.

For help, version and usage errors, exit the process in the same way as autolith."
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
  "Read the command line ARGUMENTS and return the window launch.

If ARGUMENTS request a command without a window, this process becomes
autolith, and the call does not return."
  (let ((command (launch--read arguments)))
    (if (launch--windowless-p command)
        (launch--hand-over command arguments)
        (launch--window command))))
