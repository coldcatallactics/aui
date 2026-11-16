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

(-> chrome--sign-in-to (workbench string) null)
(defun chrome--sign-in-to (workbench name)
  "Sign in to the provider NAME: ask for its key when it takes one, else open the browser and say so while Autolith waits for the login there."
  (let ((world (workbench-world workbench)))
    (ecase (world-sign-in-kind world name)
      (:key
       (workbench-ask workbench (format nil "API key for ~A" name)
                      (lambda (key)
                        (when (plusp (length (string-trim " " key)))
                          (workbench-show-panel workbench (format nil "Sign in to ~A" name) nil)
                          (workbench-run-aside workbench (list ':sign-in name)
                                               (lambda ()
                                                 (world-sign-in world name :key (string-trim " " key))))))
                      :note "Paste the key. It is checked and saved in the key store of Autolith on this machine."))
      (:browser
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" name)
        (panel--empty "The sign-in page is open in your browser. Finish signing in there."))
       (workbench-run-aside workbench (list ':sign-in name)
                            (lambda ()
                              (world-sign-in world name))))
      ((nil)
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" name)
        (panel--empty (format nil "Run autolith auth ~A in a terminal to sign in to this provider." name))))))
  nil)

(-> chrome--sign-in (workbench) null)
(defun chrome--sign-in (workbench)
  "Offer the providers to sign in to, those of the channel's model list and those this client knows, each with whether a login is stored."
  (let* ((stored (remove-duplicates
                  (loop for model in (workbench-models workbench)
                        when (portable-true-p (portable-get model "credentialStored"))
                          collect (portable-get model "provider"))
                  :test #'equal))
         (names  (sort (remove-duplicates
                        (append (loop for model in (workbench-models workbench)
                                      when (stringp (portable-get model "provider"))
                                        collect (portable-get model "provider"))
                                (world-providers))
                        :test #'equal)
                       #'string<)))
    (workbench-pick workbench
                    (mapcar (lambda (name)
                              (make-choice :label     name
                                           :detail    (if (member name stored :test #'equal)
                                                          "signed in"
                                                          "not signed in")
                                           :value     name
                                           :current-p (and (member name stored :test #'equal) t)))
                            names)
                    (lambda (name)
                      (chrome--sign-in-to workbench name))
                    :title "Sign in to a provider"))
  nil)

(-> chrome--move-agent-here (workbench) null)
(defun chrome--move-agent-here (workbench)
  "Offer the agents that live elsewhere, and move the one chosen into the channel shown."
  (let ((channel (workbench-channel workbench)))
    (workbench-pick
     workbench
     (loop for participant in (and (not (workbench-demo-p workbench))
                                   (world-participants (workbench-world workbench)))
           when (and (eq (getf participant :kind) ':agent)
                     (not (equal (getf participant :residence) channel))
                     (stringp (getf participant :name)))
             collect (make-choice :label  (getf participant :name)
                                  :detail (if (getf participant :residence)
                                              (format nil "in ~A" (channel-display-name (getf participant :residence)))
                                              "parked")
                                  :value  (getf participant :name)))
     (lambda (name)
       (workbench-submit workbench (format nil "(move ~S)" name)))
     :title "Move an agent here"))
  nil)

(-> chrome--channel-menu (workbench &key (:x integer) (:y integer)) null)
(defun chrome--channel-menu (workbench &key (x 0) (y 0))
  "Open the menu of what can be done with the channel shown and its agent, with its corner at X, Y."
  (let ((agent (chrome--channel-agent workbench)))
    (flet ((item (label function &optional detail)
             (make-choice :label label :detail detail :value function)))
      (workbench-menu
       workbench
       (append
        (list (item "New channel here"
                    (lambda () (workbench-new-channel workbench)))
              (item "New channel in another folder…"
                    (lambda () (workbench-new-channel-elsewhere workbench)))
              (make-choice)
              (item "Status"
                    (lambda () (panel-show-output workbench "Status" "status")))
              (item "Plan"
                    (lambda () (panel-show-plan workbench)))
              (item "Agenda"
                    (lambda () (panel-show-agenda workbench)))
              (item "Memories"
                    (lambda () (panel-show-memories workbench)))
              (item "Jobs"
                    (lambda () (panel-show-output workbench "Jobs" "tasks")))
              (item "Compact the context now"
                    (lambda () (workbench-submit workbench "(compact)")))
              (make-choice)
              (item "Rename the agent…"
                    (lambda ()
                      (workbench-ask workbench "Rename the agent"
                                     (lambda (name)
                                       (when (plusp (length (string-trim " " name)))
                                         (workbench-submit workbench (format nil "(rename ~S)" name))))
                                     :note    "The name of the agent in @mentions."
                                     :initial (or agent "")))))
        (if agent
            (list (item "Park the agent"
                        (lambda () (workbench-submit workbench (format nil "(dismiss ~S)" agent)))))
            (list (item "Create an agent for this channel"
                        (lambda () (workbench-submit workbench "(domestic)")))))
        (when (chrome--other-agents workbench)
          (list (item "Move an agent here…"
                      (lambda () (chrome--move-agent-here workbench)))))
        (list (make-choice)
              (item "Sign in to a model provider…"
                    (lambda () (chrome--sign-in workbench)))
              (item "Stop the world server…"
                    (lambda () (chrome--confirm-stop-server workbench)))
              (item "Copy the channel identifier"
                    (lambda ()
                      (when (workbench-channel workbench)
                        (lightfast:copy-text (workbench-channel workbench)))))
              (item "Copy the conversation"
                    (lambda ()
                      (lightfast:copy-text (transcript-plain-text (workbench-transcript workbench)))))))
       :x x
       :y y)))
  nil)

(-> chrome--confirm-stop-server (workbench) null)
(defun chrome--confirm-stop-server (workbench)
  "Ask whether the world server is to be stopped, and stop it on a yes."
  (workbench-pick workbench
                  (list (make-choice :label  "Stop the server"
                                     :detail "Work in every channel is stopped; channels and histories are kept"
                                     :value  ':stop)
                        (make-choice :label "Keep it running"
                                     :value ':keep))
                  (lambda (answer)
                    (when (eq answer ':stop)
                      (workbench-stop-server workbench)))
                  :title "Stop the world server?")
  nil)

(-> chrome-paint-directory
    (workbench stage t &key (:x integer) (:y integer) (:width integer) (:height integer)) integer)
(defun chrome-paint-directory (workbench stage identifier &key x y width height)
  "Draw the folder the channel shown works in, cut at its start to fit WIDTH, and return the width it took.

On an attached channel the folder is a region named IDENTIFIER: a click on it
asks for another folder."
  (let* ((meta      (text-style ':meta))
         (text      (text-fit-tail (short-directory (workbench-directory workbench)) meta (max 0 (- width 8))))
         (wide      (+ 8 (text-width text meta)))
         (enabled-p (workbench-attached-p workbench))
         (hot-p     (and enabled-p (stage-hot-p stage identifier))))
    (flet ((content ()
             (paint-text-in text meta :x (+ x 4) :y y :width (- wide 8) :height height)))
      (if hot-p
          (with-inversion (x y wide height)
            (content))
          (content)))
    (when enabled-p
      (stage-region stage identifier
                    :x       x
                    :y       y
                    :width   wide
                    :height  height
                    :action  (lambda () (workbench-change-directory workbench))
                    :tooltip "Change the folder the agent works in"))
    wide))

(-> header-paint
    (workbench stage &key (:x integer) (:y integer) (:width integer) (:sidebar-toggle-p boolean)
               (:band-p boolean))
    null)
(defun header-paint (workbench stage &key x y width sidebar-toggle-p (band-p t))
  "Draw the header: the channel's title, under it the channel's identifier and the folder it works in, and the buttons for the settings, the linked files, the commands and the channel's menu.

BAND-P draws the band of dither that parts the header from a conversation
beneath it."
  (let* ((left  (+ x 24 (if sidebar-toggle-p 38 0)))
         (right (- (+ x width) 20))
         (title (text-style ':heading-2))
         (meta  (text-style ':meta)))
    (paint-fill x y :width width :height *header-height* :color *paper*)
    (when sidebar-toggle-p
      (paint-button stage ':toggle-sidebar ""
                    :x       (+ x 18)
                    :y       (+ y 15)
                    :width   30
                    :height  30
                    :mark    ':panel
                    :action  (lambda () (window-toggle-sidebar workbench))
                    :tooltip "Show the channels (Ctrl+S)"))
    (flet ((button (identifier mark action tooltip)
             (paint-button stage identifier ""
                           :x       (- right 30)
                           :y       (+ y 15)
                           :width   30
                           :height  30
                           :mark    mark
                           :action  action
                           :tooltip tooltip)
             (decf right 36)))
      (when (workbench-channel workbench)
        (let ((menu-x (- right 30)))
          (button ':channel-menu ':more
                  (lambda ()
                    (chrome--channel-menu workbench :x menu-x :y (+ y 47)))
                  "This channel")))
      (button ':open-commands ':slash
              (lambda () (workbench-show-overlay workbench ':commands))
              "Commands (Ctrl+K)")
      (when (workbench-channel workbench)
        (button ':open-files ':page
                (lambda () (files-show workbench))
                "Files linked in this conversation"))
      (button ':open-settings ':sliders
              (lambda () (workbench-show-overlay workbench ':settings))
              "Settings (Ctrl+,)")
      (decf right 8))
    (paint-text (text-fit (workbench-title workbench) title (- right left)) left (+ y 30) :style title)
    (let ((cursor (if (workbench-channel workbench)
                      (paint-text (text-fit (format nil "~A · " (channel-display-name (workbench-channel workbench)))
                                            meta (- right left))
                                  left (+ y 50)
                                  :style meta)
                      left)))
      (chrome-paint-directory workbench stage ':header-directory
                              :x      (- cursor 4)
                              :y      (- (+ y 50) (text-baseline meta))
                              :width  (max 0 (- right cursor -4))
                              :height (text-style-line-height meta)))
    ;; Over a conversation the header ends on a band of dither, not a rule.
    (when band-p
      (paint-dither x (+ y *header-height* -3) :width width :height 3 :step 3)))
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
  "Draw the line under the composer: that the agent works and for how long, or the folder it works in while it is idle, and at the right how full its context is.

A count of context that Autolith estimated instead of measured opens with a tilde."
  (let* ((meta   (text-style ':meta))
         (height *status-height*)
         (right  (+ x width)))
    (multiple-value-bind (used limit estimated-p) (workbench-context workbench)
      (when (and used limit (plusp limit))
        (let* ((label   (format nil "~:[~;~~~]~A / ~A" estimated-p (chrome--count used) (chrome--count limit)))
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
                        :tooltip (format nil "Context: ~:[~;about ~]~D of ~D tokens~:[~;, estimated~]"
                                         estimated-p used limit estimated-p))
          (setf right (- bar-x 14)))))
    (cond
      ((workbench-working-p workbench)
       (setf (stage-animating-p stage) t)
       (paint-spinner x (+ y (floor (- height 14) 2)) :size 14 :phase (stage-phase stage) :color *green*)
       (paint-text-in (format nil "Working~@[ ~A~]"
                              (and (workbench-working-since workbench)
                                   (chrome--elapsed (- (get-universal-time)
                                                       (workbench-working-since workbench))))) meta
                      :x      (+ x 22)
                      :y      y
                      :width  (max 0 (- right x 22))
                      :height height))
      (t
       (chrome-paint-directory workbench stage ':status-directory
                               :x      (- x 4)
                               :y      (+ y (floor (- height (text-style-line-height meta)) 2))
                               :width  (max 0 (- right x -4))
                               :height (text-style-line-height meta)))))
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

(defparameter *approval-row-height* 28
  "The height of one choice in the approval card.")

(-> approval--sentence (string) string)
(defun approval--sentence (text)
  "Return TEXT opening with a capital, as a sentence does."
  (if (plusp (length text))
      (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
      text))

(-> approval-height (workbench integer) integer)
(defun approval-height (workbench width)
  "Return the height the approval card needs at WIDTH, zero when nothing waits for approval."
  (let ((approval (workbench-approval workbench)))
    (if (null approval)
        0
        (let ((entry (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                     :style (text-style ':code) :wrap ':anywhere)))
          (+ 28 12
             (min 120 (paragraph-layout entry (- width 44)))
             10
             (* *approval-row-height* (length (getf approval :options)))
             8 *shadow-offset* 10)))))

(-> approval-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun approval-paint (workbench stage &key x y width height)
  "Draw the approval card: what is asked in a title bar, the request in the typewriter face, and the choices as a numbered list, what each means beside it.

The card is the one thing in the window set in paper on ink, so that a
question waiting for the person is not taken for part of the conversation.
A choice is taken with its number or a click."
  (let* ((approval (workbench-approval workbench))
         (options  (getf approval :options))
         (box-h    (- height *shadow-offset* 10))
         (bar      28)
         (entry    (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                   :style (text-style ':code) :wrap ':anywhere))
         (rows-y   (- (+ y box-h) 8 (* *approval-row-height* (length options))))
         (label-w  (loop for option in options
                         maximize (text-width (approval--label option) (text-style ':label-bold))))
         (detail-x (+ x 14 30 label-w 22))
         ;; A card too narrow for both columns keeps the choices; what each
         ;; means is then in its tooltip.
         (detail-p (> (- (+ x width) detail-x 14) 220)))
    (paragraph-layout entry (- width 44))
    (paint-dither (+ x *shadow-offset*) (+ y box-h) :width width :height *shadow-offset*)
    (paint-dither (+ x width) (+ y *shadow-offset*) :width *shadow-offset* :height (- box-h *shadow-offset*))
    (stage-region stage ':approval-card :x x :y y :width width :height box-h :cursor ':default)
    (with-inversion (x y width box-h)
      (with-inversion ((+ x 3) (+ y 3) (- width 6) (- bar 3))
        (paint-text-in (format nil "Approve: ~A" (or (getf approval :title) "request")) (text-style ':label-bold)
                       :x      (+ x 10)
                       :y      (+ y 3)
                       :width  (- width 20)
                       :height (- bar 3)))
      (lightfast:with-clip ((+ x 14) (+ y bar 12) (- width 28) (max 0 (- rows-y y bar 20)))
        (paragraph-paint entry (+ x 22) (+ y bar 12)))
      (approval--paint-choices workbench stage options
                               :x        x
                               :y        rows-y
                               :width    width
                               :label-w  label-w
                               :detail-x (and detail-p detail-x))))
  nil)

(-> approval--paint-choices
    (workbench stage list
               &key (:x integer) (:y integer) (:width integer) (:label-w integer) (:detail-x (or null integer)))
    null)
(defun approval--paint-choices (workbench stage options &key x y width label-w detail-x)
  "Draw the choices OPTIONS of the approval card from Y down, one to a row: its number, its label LABEL-W wide, and from DETAIL-X what it means, when there is room for that."
  (let ((bold (text-style ':label-bold))
        (meta (text-style ':meta)))
    (loop for option in options
          for index from 0
          for top from y by *approval-row-height*
          do (let* ((choice (getf option :value))
                    (region (list ':approval choice))
                    (deny-p (equal choice "deny"))
                    (hot-p  (stage-hot-p stage region)))
               (flet ((content ()
                        (paint-text-in (format nil "~D" (1+ index)) bold
                                       :x      (+ x 14)
                                       :y      top
                                       :width  22
                                       :height *approval-row-height*
                                       :align  ':center)
                        (paint-text-in (approval--label option) bold
                                       :x      (+ x 14 30)
                                       :y      top
                                       :width  label-w
                                       :height *approval-row-height*
                                       :color  (and deny-p *red*))
                        (when detail-x
                          (paint-text-in (approval--sentence (or (getf option :description) "")) meta
                                         :x      detail-x
                                         :y      top
                                         :width  (max 0 (- (+ x width) detail-x 14))
                                         :height *approval-row-height*))))
                 (if hot-p
                     (with-inversion ((+ x 3) top (- width 6) *approval-row-height*)
                       (content))
                     (content)))
               (stage-region stage region
                             :x       (+ x 3)
                             :y       top
                             :width   (- width 6)
                             :height  *approval-row-height*
                             :action  (lambda () (workbench-answer workbench choice))
                             :tooltip (getf option :description)))))
  nil)


;;;; -- Pending Messages --

(defparameter *pending-shown* 5
  "How many waiting messages are listed above the composer, the newest ones.")

(-> pending-height (workbench) integer)
(defun pending-height (workbench)
  "Return the height the lines for messages not yet taken up need."
  (* 24 (min *pending-shown* (length (workbench-waiting workbench)))))

(-> pending-paint (workbench &key (:x integer) (:y integer) (:width integer)) null)
(defun pending-paint (workbench &key x y width)
  "Draw each message the agent has not taken up yet as one line: a filled label saying whether it steers or follows, then the message."
  (let ((meta (text-style ':meta))
        (bold (text-style ':meta-bold)))
    (loop for (text steer-p) in (last (workbench-waiting workbench) *pending-shown*)
          for row-y from y by 24
          do (let* ((label   (if steer-p "steering" "follow-up"))
                    (label-w (+ 12 (text-width label bold))))
               (with-inversion ((+ x 2) (+ row-y 3) label-w 18)
                 (paint-text-in label bold :x (+ x 2) :y (+ row-y 3) :width label-w :height 18 :align ':center))
               (paint-text-in (entries--one-line text) meta
                              :x      (+ x 2 label-w 10)
                              :y      (+ row-y 3)
                              :width  (max 0 (- width label-w 14))
                              :height 18))))
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
         (setf below (chrome--paint-centered "Write to the agent. Start a line with ( or / to run a command."
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
