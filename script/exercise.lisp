;;;; -- Interface Exercise --

;;; Drive the demonstration the way a person would, without a screen: draw the
;;; window offscreen, then click, scroll, drag and type by handing the stage
;;; the events FLTK would, and check what each did. It needs a display to
;;; measure text with, so it runs beside the tests rather than among them.
;;;
;;; Usage: script/exercise

(in-package #:aui)

(defvar *exercise-failures* 0
  "How many expectations the exercise has seen fail.")

(defmacro exercise-expect (description form)
  "Evaluate FORM and report DESCRIPTION as held or failed."
  `(if ,form
       (format t "~&  ok    ~A~%" ,description)
       (progn
         (incf *exercise-failures*)
         (format t "~&  FAIL  ~A~%" ,description))))

(-> exercise--draw (workbench) null)
(defun exercise--draw (workbench)
  "Draw WORKBENCH's window offscreen, so its regions are those of its current state."
  (window-arrange workbench)
  (lightfast:capture-window (workbench-window workbench)
                            (uiop:native-namestring
                             (merge-pathnames "aui-exercise.ppm" (uiop:temporary-directory))))
  nil)

(-> exercise--region (workbench t) (or null region))
(defun exercise--region (workbench identifier)
  "Return the region the last drawing declared as IDENTIFIER, or NIL."
  (stage--find-region (workbench-stage workbench) identifier))

(-> exercise--click (workbench t) boolean)
(defun exercise--click (workbench identifier)
  "Click in the middle of the region IDENTIFIER and return whether it exists."
  (let ((region (exercise--region workbench identifier))
        (stage  (workbench-stage workbench)))
    (when region
      (let ((position (format nil "~D ~D 1 0 0 0"
                              (+ (region-x region) (floor (region-width region) 2))
                              (+ (region-y region) (floor (region-height region) 2)))))
        (stage--pointer stage ':move position)
        (stage--pointer stage ':push position)
        (stage--pointer stage ':release position))
      t)))

(-> exercise-run () integer)
(defun exercise-run ()
  "Exercise the demonstration and return how many expectations failed."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
  (let* ((launch                   (launch-parse '("--demo" "--size" "1200x800" "--shot" "unused.png")))
         (workbench                (make-instance 'workbench
                                                  :launch launch
                                                  :world  (launch-world launch)
                                                  :queue  (make-ui-queue)))
         (*stage-failure-function* (lambda (report)
                                     (incf *exercise-failures*)
                                     (format t "~&  FAIL  ~A~%" report))))
    (workbench--reset-channel workbench)
    (window-build workbench)
    (demo-install workbench)
    (window-show workbench)
    (let ((stage      (workbench-stage workbench))
          (document   (workbench-document workbench))
          (transcript (workbench-transcript workbench)))
      (exercise--draw workbench)
      (exercise-expect "the window declares its controls"
                       (every (lambda (identifier) (exercise--region workbench identifier))
                              '(:new-channel :model :effort :mode :attach :stop :channel-menu :document)))
      (exercise-expect "a wide window docks the channel list"
                       (exercise--region workbench '(:channel "B2Qweni")))
      ;; Scrolling.
      (setf (document-following-p document) nil
            (document-scroll document)      0)
      (exercise--draw workbench)
      (stage--pointer stage ':wheel "700 400 0 0 3 0")
      (exercise-expect "the wheel scrolls the conversation" (plusp (document-scroll document)))
      (setf (document-scroll document) 0)
      (exercise--draw workbench)
      ;; Opening a tool call.
      (let ((entry (find-if (lambda (entry)
                              (and (typep entry 'tool-entry) (eq (tool-entry-status entry) ':failed)))
                            (transcript-entries transcript))))
        (exercise-expect "a click opens a tool call"
                         (and (exercise--click workbench (list ':entry (entry-identifier entry)))
                              (entry-expanded-p entry)))
        (exercise--draw workbench)
        (exercise-expect "an opened call shows blocks to copy"
                         (find ':copy (stage--regions stage)
                               :key (lambda (region)
                                      (let ((identifier (region-identifier region)))
                                        (and (consp identifier) (first identifier)))))))
      ;; Selecting.
      (stage--pointer stage ':push "560 120 1 0 0 0")
      (stage--pointer stage ':drag "900 600 1 0 0 0")
      (exercise--draw workbench)
      (exercise-expect "a drag selects text across entries"
                       (let ((text (document-selected-text document)))
                         (and text (find #\Newline text))))
      (document-clear-selection document)
      ;; The command list.
      (window--stage-key workbench (make-key-event :code  (char-code #\k)
                                                   :state lightfast:+modifier-command+))
      (exercise--draw workbench)
      (exercise-expect "the command key opens the commands"
                       (and (workbench-overlay workbench)
                            (exercise--region workbench '(:operation "status"))))
      (overlay-key workbench (make-key-event :code #xff1b))
      (exercise-expect "Escape puts them away" (null (workbench-overlay workbench)))
      ;; Settings.
      (workbench-show-overlay workbench ':settings)
      (exercise--draw workbench)
      (exercise-expect "the settings list their values"
                       (exercise--region workbench '(:setting "reasoning-effort")))
      (workbench-show-overlay workbench nil)
      ;; An approval.
      (setf (workbench-approval workbench) (demo-approval))
      (exercise--draw workbench)
      (exercise-expect "an approval offers its choices"
                       (and (exercise--region workbench '(:approval "once"))
                            (exercise--region workbench '(:approval "deny"))))
      (exercise-expect "the choices read in order and stay inside the window"
                       (let ((first (exercise--region workbench '(:approval "auto")))
                             (last  (exercise--region workbench '(:approval "deny"))))
                         (and first last
                              (or (< (region-x first) (region-x last))
                                  (< (region-y first) (region-y last)))
                              (every (lambda (region)
                                       (let ((identifier (region-identifier region)))
                                         (or (not (and (consp identifier) (eq (first identifier) ':approval)))
                                             (and (>= (region-x region) 0)
                                                  (<= (+ (region-x region) (region-width region))
                                                      (stage-width stage))))))
                                     (stage--regions stage)))))
      ;; The same card in a narrow window, where the choices take two rows.
      (stage-resize stage 620 800)
      (exercise--draw workbench)
      (exercise-expect "a narrow window wraps the choices"
                       (let ((first (exercise--region workbench '(:approval "auto")))
                             (last  (exercise--region workbench '(:approval "deny"))))
                         (and first last
                              (< (region-y first) (region-y last))
                              (<= (+ (region-x last) (region-width last)) (stage-width stage)))))
      (stage-resize stage 1200 800)
      (setf (workbench-approval workbench) nil)
      ;; Completion.
      (setf (composer-text workbench) "/st")
      (exercise--draw workbench)
      (exercise-expect "a slash offers commands"
                       (equal '("/status") (mapcar #'completion-label (completion-current workbench))))
      (completion-key workbench (make-key-event :code #xff09))
      (exercise-expect "Tab takes the offer" (string= "/status" (composer-text workbench)))
      ;; Sending. The demonstration has no channel, so the composer says so.
      (setf (composer-text workbench) "hello")
      (window--editor-key workbench (make-key-event :code #xff0d))
      (exercise-expect "Enter in the composer sends"
                       (and (workbench-notice workbench)
                            (search "Not attached" (workbench-notice workbench))))
      (setf (workbench-notice workbench) nil
            (composer-text workbench)    "")
      ;; A narrow window.
      (stage-resize stage 700 900)
      (exercise--draw workbench)
      (exercise-expect "a narrow window keeps the conversation and offers the drawer"
                       (and (exercise--region workbench ':toggle-sidebar)
                            (exercise--region workbench ':document)))
      (exercise-expect "nothing reported a failure" (null (workbench-notice workbench)))))
  *exercise-failures*)

(let ((failures (exercise-run)))
  (format t "~&~:[The interface exercise passed.~;~:*~D expectation~:P failed.~]~%" (and (plusp failures) failures))
  (finish-output)
  (sb-ext:exit :code (if (zerop failures) 0 1) :abort t))
