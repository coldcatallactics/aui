(in-package #:aui)

;;;; -- Chrome --

;;; What surrounds the conversation: the header naming the channel, the line
;;; under the composer saying where the agent works and how full its context
;;; is, the card that asks for an approval, and what the window shows while
;;; there is no conversation to show.

(defparameter *header-height* 62
  "The height of the header above the conversation.")

(defparameter *status-height* 30
  "The height of the line under the composer.")

(-> workbench-tell (workbench string string) null)
(defun workbench-tell (workbench channel text)
  "Send TEXT to CHANNEL as the person, reporting a refusal above the composer."
  (handler-case (world-tell (workbench-world workbench) channel text)
    (client-error (condition)
      (setf (workbench-notice workbench) (client-error-message condition))))
  (workbench-redraw workbench)
  nil)

(-> workbench-close-channel (workbench string) null)
(defun workbench-close-channel (workbench channel)
  "Ask the server to stop serving CHANNEL, reporting a refusal above the composer."
  (handler-case (world-close-channel (workbench-world workbench) channel)
    (client-error (condition)
      (setf (workbench-notice workbench) (client-error-message condition))))
  (workbench-redraw workbench)
  nil)

(-> chrome--channel-agent (workbench) (or null string))
(defun chrome--channel-agent (workbench)
  "Return the name of the agent residing in the channel shown, or NIL when it has none."
  (unless (workbench-demo-p workbench)
    (let ((channel (workbench-channel workbench)))
      (loop for participant in (world-participants (workbench-world workbench))
            when (and (eq (getf participant :kind) ':agent)
                      (equal (getf participant :residence) channel))
              do (return (getf participant :name))))))

(-> chrome--other-agents (workbench) list)
(defun chrome--other-agents (workbench)
  "Return the names of the agents that do not reside in the channel shown, parked ones included."
  (unless (workbench-demo-p workbench)
    (let ((channel (workbench-channel workbench)))
      (loop for participant in (world-participants (workbench-world workbench))
            when (and (eq (getf participant :kind) ':agent)
                      (not (equal (getf participant :residence) channel))
                      (stringp (getf participant :name)))
              collect (getf participant :name)))))

(-> chrome--sign-in (workbench) null)
(defun chrome--sign-in (workbench)
  "Open a terminal running autolith auth, which is where a provider is signed in to until Autolith offers that to its clients."
  (handler-case
      (if (uiop:os-macosx-p)
          (uiop:launch-program
           (list "osascript" "-e" "tell application \"Terminal\" to do script \"autolith auth\""
                 "-e" "tell application \"Terminal\" to activate"))
          (uiop:launch-program
           (list "sh" "-c"
                 "for t in \"$TERMINAL\" xdg-terminal-exec foot alacritty kitty wezterm gnome-terminal konsole xterm; do command -v \"$t\" >/dev/null 2>&1 && exec \"$t\" -e autolith auth; done; exit 1")))
    (error ()
      (setf (workbench-notice workbench) "Run autolith auth in a terminal to sign in.")))
  (workbench-redraw workbench)
  nil)

(-> chrome--channel-menu (workbench) null)
(defun chrome--channel-menu (workbench)
  "Offer what can be done with the channel shown and its agent, and do what is chosen."
  (let ((agent (chrome--channel-agent workbench)))
    (popup-actions
     (append
      (list (list "New channel here"
                  (lambda () (workbench-new-channel workbench)))
            (list "New channel in another folder…"
                  (lambda () (workbench-new-channel-elsewhere workbench)))
            nil
            (list "Show status"
                  (lambda () (workbench-submit workbench "(status)")))
            (list "Show the agenda"
                  (lambda () (workbench-submit workbench "(agenda)")))
            (list "Show the jobs"
                  (lambda () (workbench-submit workbench "(tasks)")))
            (list "Show what it remembers"
                  (lambda () (workbench-submit workbench "(memories)")))
            (list "Compact the context now"
                  (lambda () (workbench-submit workbench "(compact)")))
            nil
            (list "Agent/Rename…"
                  (lambda ()
                    (let ((name (lightfast:input-dialog "A new name for this channel's agent"
                                                        :initial (or agent ""))))
                      (when name
                        (workbench-submit workbench (format nil "(rename ~S)" name)))))))
      (if agent
          (list (list "Agent/Park it"
                      (lambda () (workbench-submit workbench (format nil "(dismiss ~S)" agent)))))
          (list (list "Agent/Create one for this channel"
                      (lambda () (workbench-submit workbench "(domestic)")))))
      (mapcar (lambda (name)
                (list (format nil "Agent/Move here/~A" name)
                      (lambda () (workbench-submit workbench (format nil "(move ~S)" name)))))
              (chrome--other-agents workbench))
      (list nil
            (list "Sign in to a model provider…"
                  (lambda () (chrome--sign-in workbench)))
            (list "Copy channel identifier"
                  (lambda ()
                    (when (workbench-channel workbench)
                      (lightfast:copy-text (workbench-channel workbench)))))
            (list "Copy the conversation"
                  (lambda ()
                    (lightfast:copy-text (transcript-plain-text (workbench-transcript workbench)))))))))
  nil)

(-> header-paint
    (workbench stage &key (:x integer) (:y integer) (:width integer) (:sidebar-toggle-p boolean)) null)
(defun header-paint (workbench stage &key x y width sidebar-toggle-p)
  "Draw the header: the channel's title, a line about it in the interface's voice, and its menu."
  (let* ((left   (+ x 24 (if sidebar-toggle-p 38 0)))
         (right  (- (+ x width) 20))
         (title  (text-style ':heading-2))
         (meta   (text-style ':meta))
         (status (workbench-status workbench)))
    (paint-fill x y :width width :height *header-height* :color *paper*)
    (when sidebar-toggle-p
      (paint-button stage ':toggle-sidebar ""
                    :x       (+ x 18)
                    :y       (+ y 15)
                    :width   30
                    :height  30
                    :mark    ':menu
                    :action  (lambda ()
                               (setf (workbench-sidebar-shown-p workbench)
                                     (not (workbench-sidebar-shown-p workbench)))
                               (workbench-redraw workbench))
                    :tooltip "Channels (Ctrl+T)"))
    (when (workbench-channel workbench)
      (paint-button stage ':channel-menu ""
                    :x       (- right 30)
                    :y       (+ y 15)
                    :width   30
                    :height  30
                    :mark    ':more
                    :action  (lambda () (chrome--channel-menu workbench))
                    :tooltip "This channel")
      (decf right 44))
    (paint-text (text-fit (workbench-title workbench) title (- right left)) left (+ y 30) :style title)
    (let ((line (format nil "~@[~A · ~]~@[~A · ~]~A"
                        (and (workbench-channel workbench)
                             (channel-display-name (workbench-channel workbench)))
                        (and status (portable-string (getf status :autolith-version))
                             (string/= (getf status :autolith-version) (world-autolith-version))
                             (format nil "server ~A" (getf status :autolith-version)))
                        (short-directory (workbench-directory workbench)))))
      (paint-text (text-fit line meta (- right left)) left (+ y 50) :style meta))
    ;; The header parts from the conversation with a dither band, not a rule.
    (paint-dither x (+ y *header-height* -3) :width width :height 3 :step 3))
  nil)

(-> chrome--count (integer) string)
(defun chrome--count (tokens)
  "Return TOKENS as a person reads a token count: 950, 40.1K, 1.2M."
  (cond
    ((< tokens 1000)
     (princ-to-string tokens))
    ((< tokens 1000000)
     (format nil "~,1FK" (/ tokens 1000)))
    (t
     (format nil "~,1FM" (/ tokens 1000000)))))

(-> chrome--elapsed (integer) string)
(defun chrome--elapsed (seconds)
  "Return SECONDS as minutes and seconds."
  (multiple-value-bind (minutes rest) (floor seconds 60)
    (format nil "~D:~2,'0D" minutes rest)))

(-> status-paint (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun status-paint (workbench stage &key x y width)
  "Draw the line under the composer: what the agent is doing or where it works, and how full its context is."
  (let* ((meta   (text-style ':meta))
         (height *status-height*)
         (right  (+ x width)))
    (multiple-value-bind (used limit) (workbench-context workbench)
      (when (and used limit (plusp limit))
        (let* ((label   (format nil "~A / ~A" (chrome--count used) (chrome--count limit)))
               (label-w (text-width label meta))
               (bar-w   64)
               (bar-x   (- right label-w 10 bar-w))
               (bar-y   (+ y (floor (- height 8) 2)))
               (filled  (floor (* bar-w (min 1 (/ used limit))))))
          (paint-text-in label meta :x (- right label-w) :y y :width label-w :height height)
          (paint-frame bar-x bar-y :width bar-w :height 8)
          (paint-fill bar-x bar-y :width filled :height 8)
          (stage-region stage ':context-meter
                        :x       bar-x
                        :y       y
                        :width   (- right bar-x)
                        :height  height
                        :tooltip (format nil "Context: ~D of ~D tokens" used limit))
          (setf right (- bar-x 14)))))
    (cond
      ((workbench-working-p workbench)
       (setf (stage-animating-p stage) t)
       (paint-spinner x (+ y (floor (- height 14) 2)) :size 14 :phase (stage-phase stage) :color *green*)
       (paint-text-in (format nil "Working~@[ ~A~] · Esc stops"
                              (and (workbench-working-since workbench)
                                   (chrome--elapsed (- (get-universal-time)
                                                       (workbench-working-since workbench))))) meta
                      :x      (+ x 22)
                      :y      y
                      :width  (max 0 (- right x 22))
                      :height height))
      (t
       (paint-text-in (short-directory (workbench-directory workbench)) meta
                      :x      x
                      :y      y
                      :width  (max 0 (- right x))
                      :height height))))
  nil)


;;;; -- Approval --

(-> approval--label (list) string)
(defun approval--label (option)
  "Return the words on the button for the approval OPTION."
  (let ((value (getf option :value)))
    (cond
      ((equal value "once")
       "Allow once")
      ((equal value "always")
       "Always allow")
      ((equal value "auto")
       "Decide automatically")
      ((equal value "sandbox")
       "In the sandbox")
      ((equal value "full")
       "Full access")
      ((equal value "deny")
       "Deny")
      (t
       (string-capitalize (substitute #\Space #\- (or value "choose")))))))

(defparameter *approval-button-height* 34
  "The height of one approval choice.")

(-> approval--fill-rows (list integer) list)
(defun approval--fill-rows (buttons width)
  "Return BUTTONS, each (INDEX LABEL BUTTON-WIDTH OPTION), in rows filled as far as WIDTH allows."
  (let ((rows  nil)
        (row   nil)
        (taken 0))
    (dolist (button buttons)
      (let ((button-w (third button)))
        (when (and row (> (+ taken button-w) width))
          (push (nreverse row) rows)
          (setf row   nil
                taken 0))
        (push button row)
        (incf taken (+ button-w 8))))
    (when row
      (push (nreverse row) rows))
    (nreverse rows)))

(-> approval--rows (list integer) list)
(defun approval--rows (options width)
  "Return the approval OPTIONS as rows that fit WIDTH, in the order they are numbered.

Each row is a list of (INDEX LABEL BUTTON-WIDTH OPTION). When the choices
need more than one row they are shared out evenly, so that no row holds a
lone last choice, as long as the even rows still fit."
  (let* ((buttons (loop for option in options
                        for index from 0
                        collect (let ((label (format nil "~D  ~A" (1+ index) (approval--label option))))
                                  (list index label (+ 28 (text-width label (text-style ':label-bold))) option))))
         (filled  (approval--fill-rows buttons width))
         (count   (length filled)))
    (if (<= count 1)
        filled
        (let* ((each (ceiling (length buttons) count))
               (even (loop for rest on buttons by (lambda (list)
                                                    (nthcdr each list))
                           collect (subseq rest 0 (min each (length rest))))))
          (if (every (lambda (row)
                       (<= (- (reduce #'+ row :key (lambda (button)
                                                     (+ (third button) 8)))
                              8)
                           width))
                     even)
              even
              filled)))))

(-> approval--buttons-height (list integer) integer)
(defun approval--buttons-height (options width)
  "Return the height the choices among OPTIONS take when laid out in WIDTH."
  (let ((rows (length (approval--rows options width))))
    (if (zerop rows)
        0
        (- (* rows (+ *approval-button-height* 8)) 8))))

(-> approval-height (workbench integer) integer)
(defun approval-height (workbench width)
  "Return the height the approval card needs at WIDTH, zero when nothing waits for approval."
  (let ((approval (workbench-approval workbench)))
    (if (null approval)
        0
        (let ((entry (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                     :style (text-style ':code) :wrap ':anywhere)))
          (+ 28 14
             (min 160 (paragraph-layout entry (- width 44)))
             14
             (approval--buttons-height (getf approval :options) (- width 28))
             14 *shadow-offset* 10)))))

(-> approval-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun approval-paint (workbench stage &key x y width height)
  "Draw the approval card: a window titled with what is asked, the request in the typewriter face, and a button for each choice."
  (let* ((approval (workbench-approval workbench))
         (box-h    (- height *shadow-offset* 10))
         (bar      (paint-window-frame x y
                                       :width  width
                                       :height box-h
                                       :title  (format nil "Approve: ~A" (or (getf approval :title) "request"))))
         (entry    (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                   :style (text-style ':code) :wrap ':anywhere))
         (rows     (approval--rows (getf approval :options) (- width 28)))
         (button-y (- (+ y box-h) 14 (approval--buttons-height (getf approval :options) (- width 28)))))
    (paragraph-layout entry (- width 44))
    (lightfast:with-clip ((+ x 14) (+ y bar 14) (- width 28) (max 0 (- button-y y bar 20)))
      (paragraph-paint entry (+ x 22) (+ y bar 14)))
    ;; Choices in the order they are numbered, the first of them filled.
    (loop for row in rows
          for row-y from button-y by (+ *approval-button-height* 8)
          do (let ((cursor (+ x 14)))
               (loop for (index label button-w option) in row
                     do (let ((choice (getf option :value)))
                          (paint-button stage (list ':approval choice) label
                                        :x         cursor
                                        :y         row-y
                                        :width     button-w
                                        :height    *approval-button-height*
                                        :primary-p (zerop index)
                                        :action    (lambda () (workbench-answer workbench choice))
                                        :tooltip   (getf option :description))
                          (incf cursor (+ button-w 8)))))))
  nil)


;;;; -- Jobs --

(defparameter *jobs-shown* 4
  "How many live jobs are listed above the composer.")

(-> jobs-height (workbench) integer)
(defun jobs-height (workbench)
  "Return the height the list of live jobs needs."
  (* 24 (min *jobs-shown* (length (workbench-live-jobs workbench)))))

(-> jobs-paint (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun jobs-paint (workbench stage &key x y width)
  "Draw each live job as one line: a working mark, its role, what it is doing, how long it has run, and a way to cancel it."
  (let ((meta (text-style ':meta))
        (bold (text-style ':meta-bold)))
    (loop for job in (workbench-live-jobs workbench)
          for row-y from y by 24
          repeat *jobs-shown*
          do (let* ((identifier (portable-get job "id"))
                    (elapsed    (portable-get job "elapsedSeconds"))
                    (cancel-w   (chip-width "cancel"))
                    (right      (- (+ x width) cancel-w)))
               (setf (stage-animating-p stage) t)
               (if (equal (portable-get job "state") "running")
                   (paint-spinner (+ x 2) (+ row-y 5) :size 14 :phase (stage-phase stage) :color *green*)
                   (paint-mark ':ring (+ x 2) (+ row-y 5) :size 14))
               (when (stringp identifier)
                 (paint-chip stage (list ':cancel-job identifier) "cancel"
                             :x       right
                             :y       row-y
                             :height  24
                             :action  (lambda () (workbench-cancel-job workbench identifier))
                             :tooltip "Cancel this job"))
               (when (integerp elapsed)
                 (let ((text (chrome--elapsed elapsed)))
                   (decf right (+ 8 (text-width text meta)))
                   (paint-text text right (+ row-y 16) :style meta)))
               (let ((end (paint-text (princ-to-string (or (portable-get job "role") "job"))
                                      (+ x 24) (+ row-y 16)
                                      :style bold)))
                 (paint-text (text-fit (entries--one-line
                                        (princ-to-string (or (portable-get job "activity")
                                                             (portable-get job "state")
                                                             "")))
                                       meta (max 0 (- right end 20)))
                             (+ end 10) (+ row-y 16)
                             :style meta)))))
  nil)


;;;; -- Pending Messages --

(-> pending-height (workbench) integer)
(defun pending-height (workbench)
  "Return the height the lines for messages not yet taken up need."
  (* 24 (min 3 (length (workbench-pending workbench)))))

(-> pending-paint (workbench &key (:x integer) (:y integer) (:width integer)) null)
(defun pending-paint (workbench &key x y width)
  "Draw each message the agent has not taken up yet as one quiet line saying whether it steers or follows."
  (let ((meta (text-style ':meta))
        (bold (text-style ':meta-bold)))
    (loop for (nil text steer-p) in (last (workbench-pending workbench) 3)
          for row-y from y by 24
          do (let* ((label (if steer-p "steering" "follow-up"))
                    (end   (paint-text label (+ x 4) (+ row-y 16) :style bold)))
               (paint-text (text-fit (entries--one-line text) meta (- (+ x width) end 12)) (+ end 10) (+ row-y 16)
                           :style meta))))
  nil)


;;;; -- When There Is No Conversation --

(-> chrome--paint-centered (string text-style &key (:x integer) (:y integer) (:width integer)) integer)
(defun chrome--paint-centered (text style &key (x 0) (y 0) (width 0))
  "Draw TEXT in STYLE centered in WIDTH with its line's top at Y, and return the Y below it."
  (paint-text-in (text-fit text style width) style
                 :x      x
                 :y      y
                 :width  width
                 :height (text-style-line-height style)
                 :align  ':center)
  (+ y (text-style-line-height style)))

(defvar *mascot* nil
  "The loaded picture of Autolith's mascot, :MISSING when it could not be read, or NIL before it was tried.")

(defparameter *mascot-size* '(250 . 225)
  "The (WIDTH . HEIGHT) the mascot is drawn at.")

(-> chrome--paint-mascot (integer integer integer) integer)
(defun chrome--paint-mascot (x y width)
  "Draw the mascot centered in WIDTH with its top at Y, and return the Y below it; draw nothing and return Y when its picture is missing."
  (unless *mascot*
    (setf *mascot*
          (or (lightfast:load-image
               (uiop:native-namestring (asdf:system-relative-pathname :aui "assets/mascot.png")))
              ':missing)))
  (if (eq *mascot* ':missing)
      y
      (progn
        (lightfast:draw-image *mascot* (+ x (floor (- width (first *mascot-size*)) 2)) y
                              :width (first *mascot-size*) :height (rest *mascot-size*))
        (+ y (rest *mascot-size*) 10))))

(-> empty-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun empty-paint (workbench stage &key x y width height)
  "Draw what stands in for a conversation: the opening's progress, why it failed, or an invitation to write."
  (let* ((column  (min 560 (- width 48)))
         (left    (+ x (floor (- width column) 2)))
         (roomy-p (>= height 560))
         (top     (+ y (max 24 (floor (- height (if roomy-p 500 260)) 2)))))
    ;; With room to spare, the mascot stands above the words.
    (when (and roomy-p (member (workbench-attachment workbench) '(:opening :none :attached)))
      (setf top (chrome--paint-mascot left top column)))
    (ecase (workbench-attachment workbench)
      (:opening
       (destructuring-bind (&optional (step 0) (description "Connecting to the world")) (workbench-progress workbench)
         (let ((below (chrome--paint-centered "Autolith" (text-style ':display) :x left :y top :width column)))
           (setf below (chrome--paint-centered description (text-style ':label)
                                               :x     left
                                               :y     (+ below 18)
                                               :width column))
           (let ((bar-w 240))
             (paint-frame (+ left (floor (- column bar-w) 2)) (+ below 14) :width bar-w :height 12)
             (paint-fill (+ left (floor (- column bar-w) 2)) (+ below 14)
                         :width  (floor (* bar-w (min 6 step)) 6)
                         :height 12)))))
      ((:failed :closed)
       (let ((below  (chrome--paint-centered (if (eq (workbench-attachment workbench) ':failed)
                                                 "The channel did not open"
                                                 "The channel closed") (text-style ':title)
                                             :x     left
                                             :y     top
                                             :width column))
             (reason (make-paragraph (list (make-span (or (workbench-failure workbench) "") nil))
                                     :style (text-style ':small) :align ':center)))
         (paragraph-layout reason column)
         (paragraph-paint reason left (+ below 12))
         (paint-button stage ':retry "Try again"
                       :x         (+ left (floor (- column 160) 2))
                       :y         (+ below 28 (paragraph-height reason))
                       :width     160
                       :height    36
                       :primary-p t
                       :action    (lambda ()
                                    (workbench-open workbench :channel (workbench-channel workbench))))))
      (:none
       (let ((below (chrome--paint-centered "Autolith" (text-style ':display) :x left :y top :width column)))
         (setf below (chrome--paint-centered "Pick a channel, or start a new one." (text-style ':body :face ':serif-italic)
                                             :x     left
                                             :y     (+ below 12)
                                             :width column))
         (paint-button stage ':empty-new (format nil "New channel in ~A" (short-directory (workbench-directory workbench)))
                       :x         (+ left (floor (- column 340) 2))
                       :y         (+ below 22)
                       :width     340
                       :height    38
                       :primary-p t
                       :mark      ':plus
                       :action    (lambda () (workbench-new-channel workbench)))
         (paint-button stage ':empty-choose "Choose another folder…"
                       :x      (+ left (floor (- column 340) 2))
                       :y      (+ below 70)
                       :width  340
                       :height 38
                       :action (lambda () (workbench-new-channel-elsewhere workbench)))))
      (:attached
       (let ((below (chrome--paint-centered "What shall we work on?" (text-style ':display)
                                            :x     left
                                            :y     top
                                            :width column)))
         (setf below (chrome--paint-centered "Write below. A line starting with ( or / runs a command."
                                             (text-style ':body)
                                             :x     left
                                             :y     (+ below 14)
                                             :width column))
         ;; A path belongs to the typewriter face; in italics its slashes read as bars.
         (chrome--paint-centered (format nil "~@[~A in ~]~A"
                                         (workbench-model workbench)
                                         (short-directory (workbench-directory workbench)))
                                 (text-style ':label)
                                 :x     left
                                 :y     (+ below 10)
                                 :width column)))))
  nil)
