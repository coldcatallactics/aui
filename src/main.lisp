(in-package #:aui)

;;;; -- Main --

;;; Startup and the event loop. The interface runs on the thread that calls
;;; MAIN, which on macOS must be the process's first; client threads only
;;; post events and wake it.

(defparameter *tick-seconds* 0.12
  "How often the interface advances what moves while something is working.")

(-> main--picture (workbench string) null)
(defun main--picture (workbench path)
  "Write a picture of WORKBENCH's window to PATH: a PNG when PATH ends in png and an image tool is at hand, else a PPM."
  (let* ((png-p (string-equal (or (pathname-type path) "") "png"))
         (ppm   (if png-p (format nil "~A.ppm" path) path)))
    (lightfast:capture-window (workbench-window workbench) ppm)
    (when png-p
      (handler-case
          (progn
            (uiop:run-program (if (uiop:os-macosx-p)
                                  (list "sips" "-s" "format" "png" ppm "--out" path)
                                  (list "magick" ppm path)))
            (delete-file ppm))
        (error ()
          (format *error-output* "No image tool converted the picture; it is at ~A.~%" ppm)))))
  nil)

(-> main--settle (workbench real) null)
(defun main--settle (workbench seconds)
  "Apply what the world says for SECONDS.

A picture taken without a window then shows the channels as the server
reports them, not the moment before it answered."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          do (workbench-drain workbench)
             (lightfast:wait 0.05)
             (sleep 0.02)))
  nil)

(-> main--start (workbench) null)
(defun main--start (workbench)
  "Open what the launch asked for: the demonstration, a named channel, the picker, or the directory's channel."
  (let ((launch (workbench-launch workbench)))
    (cond
      ((launch-demo-p launch)
       (demo-install workbench)
       (when (equal (launch-view launch) "approval")
         (setf (workbench-approval workbench) (demo-approval)))
       (when (equal (launch-view launch) "welcome")
         (transcript-clear (workbench-transcript workbench))
         (setf (workbench-working-p workbench) nil
               (workbench-jobs workbench)      nil)))
      (t
       (world-start-watching (workbench-world workbench))
       (if (launch-pick-p launch)
           (setf (workbench-sidebar-shown-p workbench) t
                 (workbench-drawer-open-p workbench)   t)
           (workbench-open workbench
                           :channel (launch-channel launch)
                           :images  (launch-images launch)))))
    (when (launch-draft launch)
      (setf (composer-text workbench) (launch-draft launch)))
    (let ((view (launch-view launch)))
      (cond
        ((equal view "commands")
         (workbench-show-overlay workbench ':commands))
        ((equal view "settings")
         (workbench-show-overlay workbench ':settings))
        ((equal view "agenda")
         (panel-show-agenda workbench))
        ((equal view "memories")
         (panel-show-memories workbench))
        ((equal view "plan")
         (panel-show-plan workbench))
        ((equal view "model")
         (composer--choose-model workbench))
        ((equal view "permissions")
         (composer--choose-mode workbench))
        ((equal view "menu")
         (chrome--channel-menu workbench
                               :x (- (stage-width (workbench-stage workbench)) 50)
                               :y 47))
        ((equal view "choice")
         (workbench-show-overlay workbench ':settings)
         (let ((row (find "reasoning-effort" (workbench-settings workbench)
                          :key  (lambda (row)
                                  (portable-get row "name"))
                          :test #'equal)))
           (when row
             (catalog--change-setting workbench row))))
        ((equal view "prompt")
         (workbench-ask workbench "Rename the agent" (constantly nil)
                        :note    "The name of the agent in @mentions."
                        :initial "lightfast"))
        ((equal view "channels")
         (setf (workbench-sidebar-shown-p workbench) t
               (workbench-drawer-open-p workbench)   t))
        ((equal view "top")
         (setf (document-following-p (workbench-document workbench)) nil
               (document-scroll (workbench-document workbench))      0)))))
  nil)

(-> run-interface (launch) null)
(defun run-interface (launch)
  "Show the window LAUNCH asks for and run until it closes."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
  ;; A window that is only a picture or a demonstration remembers nothing.
  (unless (or (launch-demo-p launch) (launch-shot launch))
    (setf *preferences-pathname* (preferences-default-pathname)))
  (let* ((queue     (make-ui-queue :wakeup #'lightfast:awake))
         (workbench (make-instance 'workbench :launch launch :world (launch-world launch) :queue queue))
         (started   (get-internal-real-time)))
    (workbench--reset-channel workbench)
    (window-build workbench)
    (main--start workbench)
    (window-show workbench)
    (lightfast:add-timeout *tick-seconds*
                           (lambda ()
                             (let ((stage (workbench-stage workbench)))
                               (workbench-poll-jobs workbench)
                               (workbench-poll-waiting workbench)
                               (when (stage-animating-p stage)
                                 (incf (stage-phase stage))
                                 (stage-redraw stage))))
                           :repeat t)
    ;; A window that was never shown has no event loop to run: draw it once
    ;; offscreen and leave.
    (when (and (launch-shot launch) (launch-size launch))
      (unless (launch-demo-p launch)
        (main--settle workbench (launch-shot-after launch)))
      (window-arrange workbench)
      (main--picture workbench (launch-shot launch))
      (workbench-close workbench)
      (return-from run-interface nil))
    (when (launch-shot launch)
      (lightfast:add-timeout (launch-shot-after launch)
                             (lambda ()
                               (window-arrange workbench)
                               (stage-flush (workbench-stage workbench))
                               (lightfast:wait 0.3)
                               (main--picture workbench (launch-shot launch))
                               (workbench-close workbench)
                               (lightfast:quit))))
    (setf *stage-failure-function*
          (lambda (report)
            (setf (workbench-notice workbench) report)
            (workbench-redraw workbench)))
    (lightfast:run-with-idle
     (lambda ()
       (with-stage-guard ("Applying what the channel said")
         (workbench-drain workbench))
       (with-stage-guard ("Laying out the window")
         (when (stage-dirty-p (workbench-stage workbench))
           (window-arrange workbench))
         (stage-flush (workbench-stage workbench)))))
    (workbench-close workbench)
    (format *error-output* "~&AUI ran for ~D seconds.~%"
            (round (- (get-internal-real-time) started) internal-time-units-per-second)))
  nil)

(defparameter *nursery-megabytes* 8
  "How much the process allocates between two collections of garbage.

SBCL's own measure is a twentieth of the heap it may grow to, some two
hundred megabytes here, and what a window allocates is short-lived: left to
that measure it would hold all of it as garbage until the first collection.")

(-> main--keep-small () null)
(defun main--keep-small ()
  "Have garbage collected often, so that the process holds little more than what it uses."
  (setf (sb-ext:bytes-consed-between-gcs) (* *nursery-megabytes* 1024 1024))
  nil)

(-> main (&optional list) null)
(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run aui with the command line ARGUMENTS and exit the process when the window closes."
  (main--keep-small)
  (let ((status 0))
    (handler-case
        (let ((launch (launch-parse arguments)))
          (when launch
            (run-interface launch)))
      ((or autolith::autolith-error client-error theme-error) (condition)
        (format *error-output* "~&aui: ~A~%" condition)
        (setf status 64)))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code status :abort t))
  nil)


;;;; -- Saved Image --

(-> image-toplevel () null)
(defun image-toplevel ()
  "Run aui in a saved image with the process's own arguments."
  (main (rest sb-ext:*posix-argv*)))

(-> save-image (string) null)
(defun save-image (pathname)
  "Save this process as an executable at PATHNAME that starts aui, and exit.

The executable holds Autolith's client code and AUI together, so it starts in
a fraction of a second. It takes every argument for itself rather than for
the Lisp runtime."
  (ensure-directories-exist pathname)
  (sb-ext:save-lisp-and-die pathname
                            :toplevel             #'image-toplevel
                            :executable           t
                            :save-runtime-options t)
  nil)
