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

(-> main--start (workbench) null)
(defun main--start (workbench)
  "Open what the launch asked for: the demonstration, a named channel, the picker, or the directory's channel."
  (let ((launch (workbench-launch workbench)))
    (cond
      ((launch-demo-p launch)
       (demo-install workbench)
       (when (equal (launch-view launch) "approval")
         (setf (workbench-approval workbench) (demo-approval))))
      (t
       (world-start-watching (workbench-world workbench))
       (if (launch-pick-p launch)
           (setf (workbench-sidebar-shown-p workbench) t)
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
        ((equal view "channels")
         (setf (workbench-sidebar-shown-p workbench) t)))))
  nil)

(-> run-interface (launch) null)
(defun run-interface (launch)
  "Show the window LAUNCH asks for and run until it closes."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
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
                               (when (stage-animating-p stage)
                                 (incf (stage-phase stage))
                                 (stage-redraw stage))))
                           :repeat t)
    ;; A window that was never shown has no event loop to run: draw it once
    ;; offscreen and leave.
    (when (and (launch-shot launch) (launch-size launch))
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

(-> main (&optional list) null)
(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run aui with the command line ARGUMENTS and exit the process when the window closes."
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
