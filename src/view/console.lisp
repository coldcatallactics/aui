(in-package #:aui)

;;;; -- Console --

;;; The console is the column at the right of the conversation in a wide
;;; window. Under the activity list, the console has a part for the goal of the
;;; channel, the plan of the agent, the child channels, the files and the
;;; messages sent to the agent. The goal and the child channels come from
;;; Autolith. The other parts are read from the conversation. Empty parts are
;;; left out. A click on the name of a part hides or shows the part, and the
;;; hidden parts are saved as a preference.

(defparameter *console-sections*
  '((:goal . "Goal") (:plan . "Plan") (:children . "Children") (:files . "Files") (:messages . "Messages"))
  "A list of the console parts as (KEY . NAME), in the order of drawing.")

(defparameter *console-row-height* 24
  "The height of a row in the console, such as a file or a message.")

(defparameter *console-section-gap* 12
  "The space above the name of each console part.")

(defparameter *console-files-shown* 10
  "The largest number of files listed in the console. The rest are counted in a last row.")

(defparameter *console-children-shown* 8
  "The largest number of child channels listed in the console. The rest are counted in a last row.")

(defparameter *console-goal-characters* 600
  "The largest number of characters of a goal objective shown in the console.")

(-> console--closed-p (keyword) boolean)
(defun console--closed-p (section)
  "Return T if the person has hidden the console part SECTION."
  (and (member section (preference ':console-closed)) t))

(-> console--toggle (workbench keyword) null)
(defun console--toggle (workbench section)
  "Hide or show the console part SECTION, and save the choice as a preference."
  (let ((closed (preference ':console-closed)))
    (setf (preference ':console-closed)
          (if (member section closed)
              (remove section closed)
              (cons section closed))))
  (workbench-redraw workbench)
  nil)


;;;; -- Content of the Parts --

(defstruct console-view
  "The content of the console parts at one drawing. DIGEST is the digest of the conversation, GOAL is the goal of the channel, and CHILDREN are the channels beneath the channel shown."
  (digest   nil)
  (goal     nil :type list)
  (children nil :type list))

(-> workbench-children (workbench) list)
(defun workbench-children (workbench)
  "Return the channels beneath the channel shown, as rows. Each channel comes before the channels started from it. The :DEPTH of a row is 0 for a channel started from the channel shown.

Return NIL for home, because all channels are beneath home."
  (let ((channel (workbench-channel workbench)))
    (and channel
         (not (sidebar--home-p workbench channel))
         (channel-descendants channel
                              (if (workbench-demo-p workbench)
                                  (remove-if-not (lambda (entry)
                                                   (eq (getf entry :kind) ':channel))
                                                 (workbench-demo-entries workbench))
                                  (world-channel-rows (workbench-world workbench)))))))

(-> console--view (workbench) console-view)
(defun console--view (workbench)
  "Return the current content of the console parts."
  (make-console-view :digest   (workbench-digest workbench)
                     :goal     (workbench-goal workbench)
                     :children (workbench-children workbench)))

(-> console--plan (digest integer) (values integer list))
(defun console--plan (digest width)
  "Return the height of the plan of DIGEST at WIDTH, and the blocks of the plan as a list of (BLOCK . TOP). The layout is cached in DIGEST for each width."
  (let ((known (digest-plan-layout digest)))
    (unless (and known (= (first known) width))
      (multiple-value-bind (height placed) (document--stack (entries--plan-steps (digest-plan digest)) width)
        (setf known                       (list width height placed)
              (digest-plan-layout digest) known)))
    (values (second known) (third known))))

(-> console--goal-block (workbench string integer) text-block)
(defun console--goal-block (workbench objective width)
  "Return the block of the goal OBJECTIVE, laid out at WIDTH. The block is cached for each objective and width. Only the beginning of a very long objective is shown."
  (let ((known (workbench--goal-layout workbench)))
    (if (and known (equal (first known) objective) (= (second known) width))
        (third known)
        (let ((block (make-text-block
                      (list (make-span (if (> (length objective) *console-goal-characters*)
                                           (format nil "~A…" (subseq objective 0 *console-goal-characters*))
                                           objective)
                                       nil))
                      :style (text-style ':list))))
          (block-layout block width)
          (setf (workbench--goal-layout workbench) (list objective width block))
          block))))

(-> console--goal-actions (list) list)
(defun console--goal-actions (goal)
  "Return the actions available for GOAL, as a list of (LABEL . ACTION). ACTION is a keyword for WORKBENCH-GOAL-REQUEST.

Active goals can be paused, and paused goals can be resumed. The objective of
a mission is part of the specification of the mission, so it cannot be changed
here."
  (append (case (getf goal :status)
            (:active
             (list (cons "Pause" ':pause)))
            (:paused
             (list (cons "Resume" ':resume))))
          (unless (eq (getf goal :kind) ':mission)
            (list (cons "Change…" ':update)))
          (list (cons "Clear…" ':clear))))

(-> console--present-p (console-view keyword) boolean)
(defun console--present-p (view section)
  "Return T if VIEW has content for the console part SECTION."
  (let ((digest (console-view-digest view)))
    (and (ecase section
           (:goal
            (console-view-goal view))
           (:plan
            (digest-plan digest))
           (:children
            (console-view-children view))
           (:files
            (digest-files digest))
           (:messages
            (digest-messages digest)))
         t)))

(-> console--note (console-view keyword) string)
(defun console--note (view section)
  "Return the note beside the name of the part SECTION. The note contains the status of the goal, the progress of the plan, the number of child channels and working channels, or the number of files or messages."
  (let ((digest (console-view-digest view)))
    (ecase section
      (:goal
       (let ((goal (console-view-goal view)))
         (format nil "~:[~;mission, ~]~(~A~)" (eq (getf goal :kind) ':mission) (getf goal :status))))
      (:plan
       (multiple-value-bind (done total) (digest-plan-progress digest)
         (format nil "~D of ~D" done total)))
      (:children
       (let* ((children (console-view-children view))
              (working  (count-if #'channel-row-working-p children)))
         (format nil "~D~:[, ~D working~;~]" (length children) (zerop working) working)))
      (:files
       (princ-to-string (length (digest-files digest))))
      (:messages
       (princ-to-string (length (digest-messages digest)))))))

(-> console--rows (integer integer) integer)
(defun console--rows (count shown)
  "Return the number of rows of a list of COUNT items, of which SHOWN items are listed. The rest are counted in a last row."
  (+ (min count shown)
     (if (> count shown) 1 0)))

(-> console--child-words (list) (or null string))
(defun console--child-words (row)
  "Return the activity of the channel of ROW in words, which are working or waiting for jobs. Return NIL if the channel is idle."
  (cond
    ((channel-row-working-p row)
     "working")
    ((getf row :waiting-for-jobs-p)
     "waiting for jobs")
    (t
     nil)))

(-> children-show (workbench) null)
(defun children-show (workbench)
  "Show all channels beneath the channel shown in a picker, and show the chosen channel."
  (let ((children (workbench-children workbench)))
    (when children
      (workbench-pick workbench
                      (loop for row in children
                            collect (multiple-value-bind (name title) (sidebar--channel-label row)
                                      (make-choice :label  name
                                                   :detail (format nil "~@[~A~]~:[~;  ~]~@[~A~]"
                                                                   (console--child-words row)
                                                                   (and (console--child-words row) title)
                                                                   title)
                                                   :value  (getf row :id))))
                      (lambda (identifier)
                        (sidebar--open workbench identifier))
                      :title "Children")))
  nil)

(-> console--body-height (workbench console-view keyword &key (:width integer)) integer)
(defun console--body-height (workbench view section &key (width 0))
  "Return the height of the content of the part SECTION at WIDTH."
  (let ((digest (console-view-digest view)))
    (ecase section
      (:goal
       (+ 4
          (block-height (console--goal-block workbench (getf (console-view-goal view) :objective) (- width 4)))
          4
          *console-row-height*))
      (:plan
       (+ 4 (values (console--plan digest (- width 4)))))
      (:children
       (* *console-row-height* (console--rows (length (console-view-children view)) *console-children-shown*)))
      (:files
       (* *console-row-height* (console--rows (length (digest-files digest)) *console-files-shown*)))
      (:messages
       (* *console-row-height* (length (digest-messages digest)))))))

(-> console--sections-height (workbench console-view integer) integer)
(defun console--sections-height (workbench view width)
  "Return the height of all parts with content in VIEW, including the names. For hidden parts, only the names are counted."
  (loop for (section) in *console-sections*
        when (console--present-p view section)
          sum (+ *console-section-gap*
                 *activity-heading-height*
                 (if (console--closed-p section)
                     0
                     (console--body-height workbench view section :width width)))))


;;;; -- Drawing --

(-> console--region
    (stage t &key (:x integer) (:y integer) (:width integer) (:height integer) (:action (or null function))
           (:tooltip (or null string)))
    boolean)
(defun console--region (stage identifier &key x y width height action tooltip)
  "Declare the part of the box inside the view of the console as the region IDENTIFIER. Return T if any of the box is inside the view."
  (let ((top    (max y *paint-top*))
        (bottom (min (+ y height) *paint-bottom*)))
    (when (> bottom top)
      (stage-region stage identifier
                    :x       x
                    :y       top
                    :width   width
                    :height  (- bottom top)
                    :action  action
                    :tooltip tooltip)
      t)))

(-> console--paint-heading
    (workbench stage keyword &key (:x integer) (:y integer) (:width integer) (:title string) (:note string))
    null)
(defun console--paint-heading (workbench stage section &key x y width title note)
  "Draw the name TITLE of the console part SECTION with NOTE beside it. At the right, draw a mark for the open or hidden state of the part. A click on the heading hides or shows the part."
  (let* ((identifier (list ':console-section section))
         (closed-p   (console--closed-p section))
         (height     *activity-heading-height*)
         (bold       (text-style ':meta-bold))
         (meta       (text-style ':meta))
         (title-w    (text-width title bold)))
    (flet ((content ()
             (paint-text-in title bold :x (+ x 2) :y y :width title-w :height height)
             (paint-text-in note meta
                            :x      (+ x 2 title-w 10)
                            :y      y
                            :width  (max 0 (- width title-w 34))
                            :height height)
             (paint-mark (if closed-p ':closed ':open) (- (+ x width) 14) (+ y (floor (- height 10) 2))
                         :size 10)))
      (if (stage-hot-p stage identifier)
          (with-inversion (x y width height)
            (content))
          (content)))
    (console--region stage identifier
                     :x       x
                     :y       y
                     :width   width
                     :height  height
                     :action  (lambda () (console--toggle workbench section))
                     :tooltip (if closed-p "Show this part" "Hide this part")))
  nil)

(-> console--confirm-clear (workbench) null)
(defun console--confirm-clear (workbench)
  "Ask the person whether to clear the goal of the channel, and clear it if the answer is yes."
  (workbench-pick workbench
                  (list (make-choice :label  "Clear"
                                     :detail "The agent stops working toward it"
                                     :value  ':clear)
                        (make-choice :label "Keep"
                                     :value ':keep))
                  (lambda (answer)
                    (when (eq answer ':clear)
                      (workbench-goal-request workbench ':clear)))
                  :title "Clear the goal?")
  nil)

(-> console--paint-goal (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-goal (workbench stage goal &key x y width)
  "Draw the objective of GOAL, and under it a chip for each action available for the goal."
  (let* ((block   (console--goal-block workbench (getf goal :objective) (- width 4)))
         (chip-y  (+ y 4 (block-height block) 4))
         ;; Only give the chips an action if they are fully in view.
         (whole-p (and (>= chip-y *paint-top*) (<= (+ chip-y *console-row-height*) *paint-bottom*))))
    (when (and (< y *paint-bottom*) (> chip-y *paint-top*))
      (let ((*paint-stage* nil))
        (block-paint block (+ x 2) (+ y 4))))
    (when (and (< chip-y *paint-bottom*) (> (+ chip-y *console-row-height*) *paint-top*))
      (loop with chip-x = (- x 6)
            for (label . action) in (console--goal-actions goal)
            do (let ((request action))
                 (incf chip-x
                       (paint-chip stage (list ':console-goal request) label
                                   :x      chip-x
                                   :y      chip-y
                                   :height *console-row-height*
                                   :action (and whole-p
                                                (lambda ()
                                                  (if (eq request ':clear)
                                                      (console--confirm-clear workbench)
                                                      (workbench-goal-request workbench request))))))))))
  nil)

(-> console--paint-child (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-child (workbench stage row &key x y width)
  "Draw ROW, which is one channel beneath the channel shown. The row contains the state mark, the name of the agent, the title and the activity in words. A click on the row shows the channel."
  (let* ((channel    (getf row :id))
         (identifier (list ':console-child channel))
         (height     *console-row-height*)
         (indent     (+ 2 (* 16 (or (getf row :depth) 0))))
         (bold       (text-style ':meta-bold))
         (meta       (text-style ':meta))
         (green      (text-style ':meta :color *green*))
         (words      (console--child-words row))
         (words-w    (if words (+ 10 (text-width words green)) 0))
         (right      (- (+ x width) 6)))
    (flet ((content ()
             (paint-channel-state (getf row :state) (+ x indent) (+ y (floor (- height 12) 2))
                                  :waiting-for-jobs-p (and (getf row :waiting-for-jobs-p) t))
             (multiple-value-bind (name title) (sidebar--channel-label row)
               (let* ((text-x (+ x indent 20))
                      (fitted (text-fit name bold (max 0 (- right words-w text-x))))
                      (end    (+ text-x (text-width fitted bold)))
                      (room   (- right words-w end 10)))
                 (paint-text-in fitted bold :x text-x :y y :width (- end text-x) :height height)
                 (when (and title (> room 40))
                   (paint-text-in (text-fit title meta room) meta
                                  :x      (+ end 10)
                                  :y      y
                                  :width  room
                                  :height height))))
             (when words
               (paint-text-in words green
                              :x      (- right words-w -10)
                              :y      y
                              :width  (- words-w 10)
                              :height height))))
      (if (stage-hot-p stage identifier)
          (with-inversion (x y width height)
            (content))
          (content)))
    (console--region stage identifier
                     :x       x
                     :y       y
                     :width   width
                     :height  height
                     :action  (lambda () (sidebar--open workbench channel))
                     :tooltip (channel-display-name channel)))
  nil)

(-> console--paint-children (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-children (workbench stage children &key x y width)
  "Draw CHILDREN, the channels beneath the channel shown. Each channel is indented under the channel that it was started from. The channels left out are counted in a last row. A click on that row shows all channels."
  (loop for row in (subseq children 0 (min *console-children-shown* (length children)))
        for row-y from y by *console-row-height*
        do (when (and (< row-y *paint-bottom*) (> (+ row-y *console-row-height*) *paint-top*))
             (console--paint-child workbench stage row :x x :y row-y :width width)))
  (console--paint-more stage ':console-more-children (- (length children) *console-children-shown*)
                       :x      x
                       :y      (+ y (* *console-row-height* *console-children-shown*))
                       :width  width
                       :action (lambda () (children-show workbench)))
  nil)

(-> console--paint-more
    (stage t integer &key (:x integer) (:y integer) (:width integer) (:action function)) null)
(defun console--paint-more (stage identifier hidden &key x y width action)
  "Draw a row with the number HIDDEN of the items left out of a list, if HIDDEN is positive. A click on the row calls ACTION."
  (when (plusp hidden)
    (flet ((content ()
             (paint-text-in (format nil "and ~D more" hidden) (text-style ':meta :face ':mono-italic)
                            :x      (+ x 22)
                            :y      y
                            :width  (- width 28)
                            :height *console-row-height*)))
      (if (stage-hot-p stage identifier)
          (with-inversion (x y width *console-row-height*)
            (content))
          (content)))
    (console--region stage identifier
                     :x      x
                     :y      y
                     :width  width
                     :height *console-row-height*
                     :action action))
  nil)

(-> console--paint-plan (digest &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-plan (digest &key x y width)
  "Draw the plan in DIGEST as a checklist. Done steps are struck through, and the current step is set apart."
  (multiple-value-bind (height placed) (console--plan digest (- width 4))
    (declare (ignore height))
    (let ((*paint-stage* nil))
      (loop for (block . top) in placed
            do (when (and (< (+ y top) *paint-bottom*)
                          (> (+ y top (block-height block)) *paint-top*))
                 (block-paint block (+ x 2) (+ y top))))))
  nil)

(-> console--paint-file
    (workbench stage pathname
               &key (:x integer) (:y integer) (:width integer) (:place string) (:changed-p boolean))
    null)
(defun console--paint-file (workbench stage file &key x y width (place "") changed-p)
  "Draw FILE, which was touched by the agent. The row contains the name of the file, the place PLACE relative to the folder of the channel, and the word changed if CHANGED-P is true. A click on the row opens the file."
  (let* ((name       (uiop:native-namestring file))
         (identifier (list ':console-file name))
         (height     *console-row-height*)
         (label      (text-style ':meta-bold :color *blue*))
         (meta       (text-style ':meta))
         (amber      (text-style ':meta :color *amber*))
         (right      (- (+ x width) 6))
         (changed-w  (if changed-p (+ 10 (text-width "changed" amber)) 0)))
    (flet ((content ()
             (paint-mark ':page (+ x 2) (+ y (floor (- height 14) 2)) :size 14)
             (let* ((fitted (text-fit (file-namestring file) label (max 0 (- right changed-w x 22))))
                    (end    (+ x 22 (text-width fitted label)))
                    (room   (- right changed-w end 12)))
               (paint-text-in fitted label :x (+ x 22) :y y :width (- end x 22) :height height)
               (when (> room 40)
                 (paint-text-in (text-fit-tail place meta room) meta
                                :x      (+ end 10)
                                :y      y
                                :width  room
                                :height height))
               (when changed-p
                 (paint-text-in "changed" amber
                                :x      (- right changed-w -10)
                                :y      y
                                :width  (- changed-w 10)
                                :height height)))))
      (if (stage-hot-p stage identifier)
          (with-inversion (x y width height)
            (content))
          (content)))
    (console--region stage identifier
                     :x       x
                     :y       y
                     :width   width
                     :height  height
                     :action  (lambda () (workbench-open-link workbench name))
                     :tooltip name))
  nil)

(-> console--paint-files (workbench stage digest &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-files (workbench stage digest &key x y width)
  "Draw the files in DIGEST, the most recently touched file first. The files left out are counted in a last row. A click on that row shows all files."
  (let* ((files  (digest-files digest))
         (hidden (- (length files) *console-files-shown*)))
    (loop for (file . changed-p) in (subseq files 0 (min *console-files-shown* (length files)))
          for row-y from y by *console-row-height*
          do (when (and (< row-y *paint-bottom*) (> (+ row-y *console-row-height*) *paint-top*))
               (console--paint-file workbench stage file
                                    :x         x
                                    :y         row-y
                                    :width     width
                                    :place     (file-place file (digest-root digest))
                                    :changed-p changed-p)))
    (console--paint-more stage ':console-more-files hidden
                         :x      x
                         :y      (+ y (* *console-row-height* *console-files-shown*))
                         :width  width
                         :action (lambda () (files-show workbench))))
  nil)

(-> console--message-line (user-entry) string)
(defun console--message-line (entry)
  "Return the start of the message ENTRY as one line without Markdown marks. If the sender is not the person, the line starts with the name of the sender."
  (let* ((text  (user-entry-text entry))
         (start (format nil "~{~A~}"
                        (mapcar #'first
                                (markdown-inline
                                 (entries--one-line (subseq text 0 (min 240 (length text)))))))))
    (format nil "~@[~A: ~]~A"
            (user-entry-sender entry)
            (cond
              ((plusp (length start))
               start)
              ((user-entry-images entry)
               "an image")
              (t
               "an empty message")))))

(-> console--paint-messages (workbench stage digest &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-messages (workbench stage digest &key x y width)
  "Draw the messages in DIGEST, the oldest first. Each message has the time it was sent, if the time is known. A click on a message shows it in the conversation."
  (let* ((meta   (text-style ':meta))
         (small  (text-style ':small))
         (height *console-row-height*)
         ;; Put the times in a column of their own if any message has a time.
         (indent (if (some #'entry-time (digest-messages digest)) 50 2)))
    (loop for entry in (digest-messages digest)
          for row-y from y by height
          do (when (and (< row-y *paint-bottom*) (> (+ row-y height) *paint-top*))
               (let ((identifier (list ':console-message (entry-identifier entry)))
                     (message    entry)
                     (top        row-y))
                 (flet ((content ()
                          (when (entry-time message)
                            (paint-text-in (entries--clock (entry-time message)) meta
                                           :x      (+ x 2)
                                           :y      top
                                           :width  44
                                           :height height))
                          (paint-text-in (console--message-line message) small
                                         :x      (+ x indent)
                                         :y      top
                                         :width  (max 0 (- width indent 6))
                                         :height height)))
                   (if (stage-hot-p stage identifier)
                       (with-inversion (x top width height)
                         (content))
                       (content)))
                 (console--region stage identifier
                                  :x      x
                                  :y      top
                                  :width  width
                                  :height height
                                  :action (lambda ()
                                            (document-scroll-to-entry (workbench-document workbench) message)
                                            (workbench-redraw workbench)))))))
  nil)

(-> console-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun console-paint (workbench stage &key x y width height)
  "Draw the parts of the console in the box. Only parts with content are drawn. Each part is drawn under the name of the part. The parts are scrolled by the wheel."
  (let* ((view    (console--view workbench))
         (digest  (console-view-digest view))
         (total   (console--sections-height workbench view width))
         (maximum (max 0 (- total height)))
         (scroll  (max 0 (min (workbench-console-scroll workbench) maximum))))
    (setf (workbench-console-scroll workbench) scroll)
    (when (plusp total)
      (stage-region stage ':console
                    :x      x
                    :y      y
                    :width  width
                    :height height
                    :wheel  (lambda (vertical horizontal)
                              (declare (ignore horizontal))
                              (setf (workbench-console-scroll workbench)
                                    (max 0 (min maximum
                                                (+ (workbench-console-scroll workbench) (* vertical 48))))))
                    :cursor ':default)
      (with-clip (x y width height)
        (let ((*paint-top*    y)
              (*paint-bottom* (+ y height))
              (top            (- y scroll)))
          (loop for (section . title) in *console-sections*
                do (when (console--present-p view section)
                     (incf top *console-section-gap*)
                     (when (and (< top *paint-bottom*) (> (+ top *activity-heading-height*) *paint-top*))
                       (console--paint-heading workbench stage section
                                               :x     x
                                               :y     top
                                               :width width
                                               :title title
                                               :note  (console--note view section)))
                     (incf top *activity-heading-height*)
                     (unless (console--closed-p section)
                       (ecase section
                         (:goal
                          (console--paint-goal workbench stage (console-view-goal view)
                                               :x     x
                                               :y     top
                                               :width width))
                         (:plan
                          (console--paint-plan digest :x x :y (+ top 4) :width width))
                         (:children
                          (console--paint-children workbench stage (console-view-children view)
                                                   :x     x
                                                   :y     top
                                                   :width width))
                         (:files
                          (console--paint-files workbench stage digest :x x :y top :width width))
                         (:messages
                          (console--paint-messages workbench stage digest :x x :y top :width width)))
                       (incf top (console--body-height workbench view section :width width)))))))
      (sheet-paint-scrollbar stage
                             :x       (+ x width 4)
                             :y       y
                             :height  height
                             :content total
                             :scroll  scroll)))
  nil)
