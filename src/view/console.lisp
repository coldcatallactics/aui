(in-package #:aui)

;;;; -- Console --

;;; The column at the right of the conversation in a wide window, under the
;;; channel's title. At its top is what runs for the channel, always in view.
;;; Under that are the parts read from the conversation, each under its name:
;;; the plan the agent keeps, the files the agent changed, linked or named,
;;; and the messages sent to the agent, as a table of contents. A click on a
;;; name puts the part away or brings it back, and which parts are put away
;;; is remembered. A part with nothing in it is not named at all. When the
;;; parts are taller than the window, the wheel moves them.

(defparameter *console-sections* '((:plan . "Plan") (:files . "Files") (:messages . "Messages"))
  "The console's parts as (KEY . NAME), in the order they are drawn.")

(defparameter *console-row-height* 24
  "The height of one file or one message in the console.")

(defparameter *console-section-gap* 12
  "The space above the name of each of the console's parts.")

(defparameter *console-files-shown* 10
  "How many files the console lists before it counts the rest.")

(-> console--closed-p (keyword) boolean)
(defun console--closed-p (section)
  "Return T when the person put the console's part SECTION away."
  (and (member section (preference ':console-closed)) t))

(-> console--toggle (workbench keyword) null)
(defun console--toggle (workbench section)
  "Put the console's part SECTION away, or bring it back, and remember that."
  (let ((closed (preference ':console-closed)))
    (setf (preference ':console-closed)
          (if (member section closed)
              (remove section closed)
              (cons section closed))))
  (workbench-redraw workbench)
  nil)


;;;; -- What Each Part Holds --

(-> console--plan (digest integer) (values integer list))
(defun console--plan (digest width)
  "Return the height of DIGEST's plan set WIDTH wide and its blocks as (BLOCK . TOP), laid out once for each plan and width."
  (let ((known (digest-plan-layout digest)))
    (unless (and known (= (first known) width))
      (multiple-value-bind (height placed) (document--stack (entries--plan-steps (digest-plan digest)) width)
        (setf known                       (list width height placed)
              (digest-plan-layout digest) known)))
    (values (second known) (third known))))

(-> console--present-p (digest keyword) boolean)
(defun console--present-p (digest section)
  "Return T when DIGEST has something for the console's part SECTION."
  (and (ecase section
         (:plan
          (digest-plan digest))
         (:files
          (digest-files digest))
         (:messages
          (digest-messages digest)))
       t))

(-> console--note (digest keyword) string)
(defun console--note (digest section)
  "Return what stands beside the name of the part SECTION: how far the plan is, or how many files or messages there are."
  (ecase section
    (:plan
     (multiple-value-bind (done total) (digest-plan-progress digest)
       (format nil "~D of ~D" done total)))
    (:files
     (princ-to-string (length (digest-files digest))))
    (:messages
     (princ-to-string (length (digest-messages digest))))))

(-> console--body-height (digest keyword integer) integer)
(defun console--body-height (digest section width)
  "Return the height of what the part SECTION holds, set WIDTH wide."
  (ecase section
    (:plan
     (+ 4 (values (console--plan digest (- width 4)))))
    (:files
     (let ((count (length (digest-files digest))))
       (* *console-row-height*
          (+ (min count *console-files-shown*)
             (if (> count *console-files-shown*) 1 0)))))
    (:messages
     (* *console-row-height* (length (digest-messages digest))))))

(-> console--sections-height (digest integer) integer)
(defun console--sections-height (digest width)
  "Return the height of every part DIGEST has something for, with their names, those put away counting their names only."
  (loop for (section) in *console-sections*
        when (console--present-p digest section)
          sum (+ *console-section-gap*
                 *activity-heading-height*
                 (if (console--closed-p section)
                     0
                     (console--body-height digest section width)))))


;;;; -- Drawing --

(-> console--region
    (stage t &key (:x integer) (:y integer) (:width integer) (:height integer) (:action (or null function))
           (:tooltip (or null string)))
    boolean)
(defun console--region (stage identifier &key x y width height action tooltip)
  "Declare the part of the box that lies inside the console's view as the region IDENTIFIER, and return whether any of it does."
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
  "Draw the name TITLE of the console's part SECTION with NOTE beside it and, at the right, the mark that says whether the part is put away. A click on it puts the part away or brings it back."
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
                     :tooltip (if closed-p "Show this part" "Put this part away")))
  nil)

(-> console--paint-plan (digest &key (:x integer) (:y integer) (:width integer)) null)
(defun console--paint-plan (digest &key x y width)
  "Draw the plan in DIGEST as a checklist: what is done struck through, the step in hand set apart."
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
  "Draw one file the agent touched: its name, PLACE, where it lies from the channel's folder, and that it was changed when CHANGED-P. A click opens it."
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
  "Draw the files in DIGEST, the one touched last first, and count those beyond what is listed in a last row that leads to all of them."
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
    (when (plusp hidden)
      (let ((row-y      (+ y (* *console-row-height* *console-files-shown*)))
            (identifier ':console-more-files))
        (flet ((content ()
                 (paint-text-in (format nil "and ~D more" hidden) (text-style ':meta :face ':mono-italic)
                                :x      (+ x 22)
                                :y      row-y
                                :width  (- width 28)
                                :height *console-row-height*)))
          (if (stage-hot-p stage identifier)
              (with-inversion (x row-y width *console-row-height*)
                (content))
              (content)))
        (console--region stage identifier
                         :x      x
                         :y      row-y
                         :width  width
                         :height *console-row-height*
                         :action (lambda () (files-show workbench))))))
  nil)

(-> console--message-line (user-entry) string)
(defun console--message-line (entry)
  "Return the message ENTRY as one line: who sent it when that was not the person, then how it begins, without the marks of its Markdown."
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
  "Draw the messages in DIGEST, oldest first, each with the time it was sent when that is known. A click on one shows it in the conversation."
  (let* ((meta   (text-style ':meta))
         (small  (text-style ':small))
         (height *console-row-height*)
         ;; The times stand in a column of their own when any message has one.
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
  "Draw the console's parts in the box: each part the conversation has something for, under its name, scrolled as the wheel left them."
  (let* ((digest  (workbench-digest workbench))
         (total   (console--sections-height digest width))
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
      (lightfast:with-clip (x y width height)
        (let ((*paint-top*    y)
              (*paint-bottom* (+ y height))
              (top            (- y scroll)))
          (loop for (section . title) in *console-sections*
                do (when (console--present-p digest section)
                     (incf top *console-section-gap*)
                     (when (and (< top *paint-bottom*) (> (+ top *activity-heading-height*) *paint-top*))
                       (console--paint-heading workbench stage section
                                               :x     x
                                               :y     top
                                               :width width
                                               :title title
                                               :note  (console--note digest section)))
                     (incf top *activity-heading-height*)
                     (unless (console--closed-p section)
                       (ecase section
                         (:plan
                          (console--paint-plan digest :x x :y (+ top 4) :width width))
                         (:files
                          (console--paint-files workbench stage digest :x x :y top :width width))
                         (:messages
                          (console--paint-messages workbench stage digest :x x :y top :width width)))
                       (incf top (console--body-height digest section width)))))))
      (sheet-paint-scrollbar stage
                             :x       (+ x width 4)
                             :y       y
                             :height  height
                             :content total
                             :scroll  scroll)))
  nil)
