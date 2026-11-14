;;;; -- Interface Exercise --

;;; Drive the demonstration the way a person would, without a screen: draw the
;;; window offscreen, then click, scroll, drag and type by handing the stage
;;; the events FLTK would, and check what each did. It needs a display to
;;; measure text with, so it runs beside the tests rather than among them.
;;;
;;; Usage: script/exercise

(load (merge-pathnames "exercise-support.lisp" *load-truename*))

(in-package #:aui)

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
      ;; In a window too low to hold them all, the wheel scrolls the list.
      (stage-resize stage 1200 420)
      (exercise--draw workbench)
      (stage--pointer stage ':wheel "600 250 0 0 2 0")
      (exercise--draw workbench)
      (exercise-expect "the wheel scrolls the commands"
                       (plusp (overlay-scroll (workbench-overlay workbench))))
      (stage-resize stage 1200 800)
      (exercise--draw workbench)
      (overlay-key workbench (make-key-event :code #xff1b))
      (exercise-expect "Escape puts them away" (null (workbench-overlay workbench)))
      ;; Settings.
      (workbench-show-overlay workbench ':settings)
      (exercise--draw workbench)
      (exercise-expect "the settings list their values"
                       (exercise--region workbench '(:setting "reasoning-effort")))
      ;; A choice among the settings opens a picker that returns to them.
      (exercise--click workbench '(:setting "reasoning-effort"))
      (exercise--draw workbench)
      (exercise-expect "a setting with choices opens a picker with the current one marked"
                       (and (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (find-if #'choice-current-p (overlay-choices (workbench-overlay workbench)))
                            (exercise--region workbench '(:choice 0))))
      (exercise-expect "the picker lies over the settings, which stay drawn beneath it"
                       (and (exercise--region workbench '(:overlay-window :settings))
                            (exercise--region workbench '(:overlay-window :picker))))
      (overlay-key workbench (make-key-event :code #xff1b))
      (exercise-expect "putting the picker away returns to the settings"
                       (eq ':settings (overlay-kind (workbench-overlay workbench))))
      ;; A setting that is a number asks for it in a prompt.
      (exercise--draw workbench)
      (exercise--click workbench '(:setting "channel-idle-minutes"))
      (exercise--draw workbench)
      (exercise-expect "a setting with a value asks for it in a prompt"
                       (and (eq ':prompt (overlay-kind (workbench-overlay workbench)))
                            (equal "30" (overlay-query (workbench-overlay workbench)))
                            (exercise--region workbench ':prompt-accept)))
      (workbench-show-overlay workbench nil)
      ;; The channel's menu, and the panels it leads to.
      (exercise--draw workbench)
      (exercise--click workbench ':channel-menu)
      (exercise--draw workbench)
      (exercise-expect "the channel's menu opens beside its button"
                       (and (eq ':menu (overlay-kind (workbench-overlay workbench)))
                            (exercise--region workbench '(:choice 0))))
      (let ((agenda (position "Agenda" (overlay-choices (workbench-overlay workbench))
                              :key #'choice-label :test #'equal)))
        (exercise--click workbench (list ':choice agenda)))
      (exercise--draw workbench)
      (exercise-expect "the agenda opens as a board with a tab for each state"
                       (and (eq ':board (overlay-kind (workbench-overlay workbench)))
                            (equal '("All" "Doing" "To do" "Blocked" "Notes" "Done")
                                   (mapcar #'first (overlay-tabs (workbench-overlay workbench))))
                            (exercise--region workbench '(:tab 1))))
      (exercise--click workbench '(:tab 1))
      (exercise--draw workbench)
      (exercise-expect "a tab shows what is in its state"
                       (and (= 1 (overlay-selection (workbench-overlay workbench)))
                            (= 1 (second (nth 1 (overlay-tabs (workbench-overlay workbench)))))))
      (workbench-show-overlay workbench nil)
      (panel-show-memories workbench)
      (exercise--draw workbench)
      (exercise-expect "what is remembered opens as a board"
                       (equal '("All" "Stale" "Unverified" "Current")
                              (mapcar #'first (overlay-tabs (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      ;; The chips under the composer.
      (exercise--draw workbench)
      (exercise--click workbench ':mode)
      (exercise--draw workbench)
      (exercise-expect "the permission chip opens a menu that says what each mode means"
                       (and (eq ':menu (overlay-kind (workbench-overlay workbench)))
                            (every #'choice-detail (overlay-choices (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (exercise--draw workbench)
      (exercise--click workbench ':model)
      (exercise--draw workbench)
      (exercise-expect "the model chip opens a picker"
                       (eq ':picker (overlay-kind (workbench-overlay workbench))))
      (workbench-show-overlay workbench nil)
      ;; The line editor's keys.
      (setf (composer-text workbench) "one two three")
      (composer-caret-to-end workbench)
      (window--editor-key workbench (make-key-event :code #xff08 :state lightfast:+modifier-control+))
      (exercise-expect "Ctrl-Backspace deletes a word" (string= "one two " (composer-text workbench)))
      (window--editor-key workbench (make-key-event :code (char-code #\u) :state lightfast:+modifier-control+))
      (exercise-expect "Ctrl-U deletes to the start of the line" (string= "" (composer-text workbench)))
      ;; The channel list can be put away with the pointer.
      (exercise--draw workbench)
      (exercise--click workbench ':hide-sidebar)
      (exercise--draw workbench)
      (exercise-expect "a button hides the channel list and another brings it back"
                       (and (not (workbench-sidebar-shown-p workbench))
                            (exercise--region workbench ':toggle-sidebar)))
      (exercise--click workbench ':toggle-sidebar)
      (exercise--draw workbench)
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
      ;; A wide window sets the console beside the conversation.
      (stage-resize stage 1500 900)
      (exercise--draw workbench)
      (exercise-expect "a wide window sets the console beside the conversation"
                       (let ((document (exercise--region workbench ':document))
                             (stop     (exercise--region workbench ':stop))
                             (menu     (exercise--region workbench ':channel-menu)))
                         (and document stop menu
                              (>= (region-x stop) (+ (region-x document) (region-width document)))
                              (>= (region-x menu) (+ (region-x document) (region-width document)))
                              (= (region-height document) 900))))
      ;; The edge between the conversation and the console can be dragged.
      (let* ((edge   (exercise--region workbench ':console-edge))
             (before (and edge (region-x edge))))
        (when edge
          (let ((from (format nil "~D 400 1 0 0 0" (+ (region-x edge) 4)))
                (to   (format nil "~D 400 1 0 0 0" (- (region-x edge) 120))))
            (stage--pointer stage ':push from)
            (stage--pointer stage ':drag to)
            (stage--pointer stage ':release to)))
        (exercise--draw workbench)
        (exercise-expect "dragging the console's edge makes it wider"
                         (let ((after (exercise--region workbench ':console-edge)))
                           (and before after (< (region-x after) (- before 80))))))
      (exercise-expect "what runs is listed by kind, each with its cross"
                       (and (exercise--region workbench '(:cancel-job "job-1"))
                            (exercise--region workbench '(:job "job-2"))
                            (equal '("Agents" "Commands") (mapcar #'first (activity-sections workbench)))))
      (stage-resize stage 1200 800)
      (exercise--draw workbench)
      ;; A narrow window.
      (stage-resize stage 700 900)
      (exercise--draw workbench)
      (exercise-expect "a narrow window keeps the conversation and offers the drawer"
                       (and (exercise--region workbench ':toggle-sidebar)
                            (exercise--region workbench ':document)))
      (exercise-expect "nothing reported a failure" (null (workbench-notice workbench)))))
  *exercise-failures*)

(exercise-run)
(exercise--finish)
