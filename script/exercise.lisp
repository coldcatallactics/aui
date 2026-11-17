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
                              '(:new-channel :delete-channel :model :effort :mode :attach :stop :channel-menu
                                :open-settings :open-files :open-commands :header-directory :document)))
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
      ;; A channel whose list of models did not arrive still offers the ones its setting takes.
      (let ((models (workbench-models workbench)))
        (setf (workbench-models workbench) nil)
        (exercise--draw workbench)
        (exercise--click workbench ':model)
        (exercise-expect "without a list of models the model chip offers the setting's choices"
                         (and (workbench-overlay workbench)
                              (equal '("gpt-6.1-sol" "gpt-6.1-terra")
                                     (mapcar #'choice-label (overlay-choices (workbench-overlay workbench))))))
        (workbench-show-overlay workbench nil)
        (setf (workbench-models workbench) models))
      ;; The foot of the channel list.
      (exercise--draw workbench)
      (exercise--click workbench ':delete-channel)
      (exercise-expect "the button at the list's foot asks before it deletes the channel shown"
                       (and (workbench-overlay workbench)
                            (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (search "Delete" (overlay-title (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      ;; A message from another agent leads to the channel it wrote from.
      (let ((message (find-if (lambda (entry)
                                (and (typep entry 'user-entry) (user-entry-sender entry)))
                              (transcript-entries transcript))))
        (document-scroll-to-end document)
        (exercise--draw workbench)
        (exercise-expect "a message from another agent has a button to its channel"
                         (and message
                              (exercise--region workbench (list ':entry-channel (entry-identifier message))))))
      ;; The files an answer links.
      (let* ((directory (uiop:ensure-directory-pathname
                         (format nil "~Aaui-exercise-~D/" (uiop:temporary-directory) (random 1000000))))
             (file      (merge-pathnames "notes on fonts.md" directory))
             (plan      (merge-pathnames "plan.md" directory)))
        (ensure-directories-exist file)
        (dolist (written (list file plan))
          (with-open-file (stream written :direction ':output :if-exists ':supersede)
            (write-line "placeholder" stream)))
        (unwind-protect
             (progn
               (setf (getf (workbench-status workbench) :cwd) (namestring directory))
               (transcript-append transcript
                                  (make-instance 'assistant-entry
                                                 :text (format nil "See [the notes](notes%20on%20fonts.md#L3), `~A:12`, the notes [again](notes%20on%20fonts.md), `make` and [the site](https://www.fltk.org)."
                                                               (uiop:native-namestring plan))))
               (exercise-expect "linked and named files are found from the channel's folder, each once, the last named first"
                                (equal (list (truename file) (truename plan)) (files-linked workbench)))
               ;; A file a tool changed says so in the console.
               (transcript-append transcript
                                  (make-instance 'tool-entry
                                                 :name "resource.edit" :status ':completed
                                                 :arguments '(("uri" . "workspace:plan.md"))))
               (stage-resize stage 1500 900)
               (exercise--draw workbench)
               (exercise-expect "the console lists the files, the one a tool changed marked and first"
                                (and (equal (list (cons (truename plan) t) (cons (truename file) nil))
                                            (digest-files (workbench-digest workbench)))
                                     (exercise--region workbench
                                                       (list ':console-file
                                                             (uiop:native-namestring (truename plan))))))
               (stage-resize stage 1200 800)
               (exercise--draw workbench)
               (exercise-expect "a link to a page is not taken for a file"
                                (and (link-web-p "https://www.fltk.org")
                                     (null (link-file "https://www.fltk.org" (namestring directory)))))
               (exercise--draw workbench)
               (exercise--click workbench ':open-files)
               (exercise-expect "the files button lists the files the agent linked"
                                (and (workbench-overlay workbench)
                                     (equal '("plan.md" "notes on fonts.md")
                                            (mapcar #'choice-label
                                                    (overlay-choices (workbench-overlay workbench))))))
               (workbench-show-overlay workbench nil))
          (setf (getf (workbench-status workbench) :cwd) "/home/you/lightfast/")
          (uiop:delete-directory-tree directory :validate (lambda (checked)
                                                            (search "aui-exercise-" (namestring checked))))))
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
      ;; The same card in a narrow window.
      (stage-resize stage 620 800)
      (exercise--draw workbench)
      (exercise-expect "a narrow window keeps the choices inside it"
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
                             (menu     (exercise--region workbench ':channel-menu))
                             (job      (exercise--region workbench '(:job "job-1"))))
                         (and document menu job
                              (>= (region-x menu) (+ (region-x document) (region-width document)))
                              (>= (region-x job) (+ (region-x document) (region-width document))))))
      (exercise-expect "the composer stays under the conversation, and under it what the agent does"
                       (let ((document (exercise--region workbench ':document))
                             (stop     (exercise--region workbench ':stop))
                             (meter    (exercise--region workbench ':context-meter)))
                         (and document stop meter
                              (< (region-x stop) (+ (region-x document) (region-width document)))
                              (>= (region-y stop) (+ (region-y document) (region-height document)))
                              (< (region-x meter) (+ (region-x document) (region-width document)))
                              (> (region-y meter) (region-y stop)))))
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
      ;; Under what runs, the console holds what was read from the conversation.
      (let ((first (find-if (lambda (entry)
                              (typep entry 'user-entry))
                            (transcript-entries transcript))))
        (exercise-expect "the console names the plan and the messages"
                         (and (exercise--region workbench '(:console-section :plan))
                              (exercise--region workbench '(:console-section :messages))
                              (exercise--region workbench (list ':console-message (entry-identifier first)))))
        (document-scroll-to-end document)
        (exercise--draw workbench)
        (exercise--click workbench (list ':console-message (entry-identifier first)))
        (exercise--draw workbench)
        (exercise-expect "a click on a message shows it in the conversation"
                         (and (not (document-following-p document))
                              (< (document-scroll document) 40)))
        (exercise--click workbench '(:console-section :messages))
        (exercise--draw workbench)
        (exercise-expect "a click on a part's name puts the part away"
                         (and (console--closed-p ':messages)
                              (exercise--region workbench '(:console-section :messages))
                              (null (exercise--region workbench (list ':console-message (entry-identifier first))))))
        (exercise--click workbench '(:console-section :messages))
        (exercise--draw workbench)
        (document-scroll-to-end document)
        (exercise--draw workbench))
      ;; The goal the channel works toward, and what a person can do with it.
      (exercise-expect "the console shows the goal, which can be paused, changed and cleared"
                       (and (exercise--region workbench '(:console-section :goal))
                            (exercise--region workbench '(:console-goal :pause))
                            (exercise--region workbench '(:console-goal :update))
                            (exercise--region workbench '(:console-goal :clear))
                            (null (exercise--region workbench '(:console-goal :resume)))))
      (exercise--click workbench '(:console-goal :clear))
      (exercise-expect "clearing the goal asks first"
                       (and (workbench-overlay workbench)
                            (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (search "Clear" (overlay-title (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (exercise--draw workbench)
      (exercise--click workbench '(:console-goal :update))
      (exercise-expect "changing the goal starts from what it says now"
                       (and (workbench-overlay workbench)
                            (eq ':prompt (overlay-kind (workbench-overlay workbench)))
                            (search "Lightfast builds" (overlay-query (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (let ((state (workbench-state workbench)))
        (setf (workbench-state workbench)
              (portable-object "goal" (portable-object "objective" "Port it." "status" "paused"
                                                       "kind" "mission")))
        (exercise--draw workbench)
        (exercise-expect "a paused mission can be resumed, and its objective is not changed from here"
                         (and (exercise--region workbench '(:console-goal :resume))
                              (null (exercise--region workbench '(:console-goal :pause)))
                              (null (exercise--region workbench '(:console-goal :update)))))
        ;; A channel whose state does not speak of goals has the goal of the newest record.
        (setf (workbench-state workbench)        (portable-object "title" "x")
              (workbench--goal-record workbench) (list :goal :seq 5 :objective "From a record." :status :active))
        (exercise--draw workbench)
        (exercise-expect "without a goal in the state the newest goal record states it"
                         (and (equal "From a record." (getf (workbench-goal workbench) :objective))
                              (exercise--region workbench '(:console-section :goal))))
        (setf (workbench--goal-record workbench) (list :goal :seq 6 :objective nil :status :cleared))
        (exercise--draw workbench)
        (exercise-expect "a goal that was cleared is not named"
                         (null (exercise--region workbench '(:console-section :goal))))
        (setf (workbench-state workbench)        state
              (workbench--goal-record workbench) nil)
        (exercise--draw workbench))
      ;; The channels started from the one shown.
      (exercise-expect "the console lists the channels beneath the one shown, each under its parent"
                       (let ((child      (exercise--region workbench '(:console-child "C4puSYz")))
                             (grandchild (exercise--region workbench '(:console-child "D7mWx2a"))))
                         (and (exercise--region workbench '(:console-section :children))
                              (equal '(("C4puSYz" . 0) ("D7mWx2a" . 1) ("E3nQr8b" . 0))
                                     (mapcar (lambda (row)
                                               (cons (getf row :id) (getf row :depth)))
                                             (workbench-children workbench)))
                              child grandchild
                              (< (region-y child) (region-y grandchild))
                              (null (exercise--region workbench '(:console-child "A55TkQx"))))))
      ;; More channels than the console lists.
      (let ((tree (workbench-demo-entries workbench)))
        (setf (workbench-demo-entries workbench)
              (append tree
                      (loop for index from 1 to 10
                            collect (list :kind ':channel :id (format nil "extra-~D" index)
                                          :name (format nil "extra-~D" index) :parent "B2Qweni" :depth 1
                                          :indent 2 :state ':absent :unread 0 :mentions 0 :pending 0))))
        (exercise--draw workbench)
        (exercise-expect "channels beyond those listed are counted in a last row"
                         (and (exercise--region workbench ':console-more-children)
                              (exercise--region workbench '(:console-child "extra-5"))
                              (null (exercise--region workbench '(:console-child "extra-6")))))
        (exercise--click workbench ':console-more-children)
        (exercise-expect "a click on that row offers every channel beneath the one shown"
                         (and (workbench-overlay workbench)
                              (eq ':picker (overlay-kind (workbench-overlay workbench)))
                              (= 13 (length (overlay-choices (workbench-overlay workbench))))))
        (workbench-show-overlay workbench nil)
        (setf (workbench-demo-entries workbench) tree)
        (exercise--draw workbench))
      (exercise-expect "what runs is listed by kind, each with its cross"
                       (and (exercise--region workbench '(:cancel-job "job-1"))
                            (exercise--region workbench '(:job "job-2"))
                            (equal '("Agents" "Commands") (mapcar #'first (activity-sections workbench)))))
      (stage-resize stage 1200 800)
      (exercise--draw workbench)
      ;; A narrow window.
      (stage-resize stage 700 900)
      (exercise--draw workbench)
      (exercise-expect "a window made narrow keeps the conversation and offers the channel list"
                       (and (exercise--region workbench ':toggle-sidebar)
                            (exercise--region workbench ':document)
                            (null (exercise--region workbench '(:channel "B2Qweni")))))
      (exercise--click workbench ':toggle-sidebar)
      (exercise--draw workbench)
      (exercise-expect "on a narrow window the channel list takes the whole window"
                       (let ((row (exercise--region workbench '(:channel "C4puSYz"))))
                         (and row
                              (= (region-width row) 700)
                              (null (exercise--region workbench ':document))
                              (null (exercise--region workbench ':send))
                              (null (exercise--region workbench ':stop)))))
      (window--stage-key workbench (make-key-event :code #xff1b))
      (exercise--draw workbench)
      (exercise-expect "Escape returns from the list to the conversation"
                       (and (exercise--region workbench ':document)
                            (null (exercise--region workbench '(:channel "C4puSYz")))))
      ;; An idle channel says where it works, and that can be clicked.
      (setf (workbench-working-p workbench) nil)
      (exercise--draw workbench)
      (exercise-expect "the folder under the composer is a control"
                       (exercise--region workbench ':status-directory))
      (setf (workbench-working-p workbench) t)
      (exercise-expect "nothing reported a failure" (null (workbench-notice workbench)))))
  *exercise-failures*)

(exercise-run)
(exercise--finish)
