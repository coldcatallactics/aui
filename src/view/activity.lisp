(in-package #:aui)

;;;; -- Activity --

;;; The list of jobs that run beside the conversation. These are the child
;;; agents, commands, evaluations and model calls started by the agent that
;;; have not finished yet. The jobs are grouped by kind under a heading, and
;;; only kinds with running jobs are listed. A person cancels a job with the
;;; cross at the end of the row or copies the cancel call from the menu.

(defparameter *activity-row-height* 24
  "The height of one job row in the activity list.")

(defparameter *activity-heading-height* 22
  "The height of the heading above each kind of activity.")

(-> activity--kind (list) keyword)
(defun activity--kind (job)
  "Return the kind of activity of JOB, which is decided by the role of the job.

Tool names contain a dot. Agent roles do not contain a dot."
  (let ((role (princ-to-string (or (portable-get job "role") ""))))
    (cond
      ((null (position #\. role))
       ':agents)
      ((uiop:string-prefix-p "shell" role)
       ':commands)
      ((uiop:string-prefix-p "lisp" role)
       ':evaluations)
      ((uiop:string-prefix-p "rlm" role)
       ':models)
      (t
       ':jobs))))

(-> activity-sections (workbench) list)
(defun activity-sections (workbench)
  "Return the live jobs of the channel of WORKBENCH grouped by kind, as a list of (NAME . JOBS) pairs.

Kinds without jobs are left out."
  (let ((jobs (workbench-live-jobs workbench)))
    (loop for (kind . name) in '((:agents . "Agents")
                                 (:commands . "Commands")
                                 (:evaluations . "Evaluations")
                                 (:models . "Model calls")
                                 (:jobs . "Jobs"))
          for members = (remove-if-not (lambda (job)
                                         (eq (activity--kind job) kind))
                                       jobs)
          when members
            collect (cons name members))))

(-> activity-height (workbench &key (:most integer)) integer)
(defun activity-height (workbench &key (most most-positive-fixnum))
  "Return the height of the activity list, limited to MOST units."
  (min most
       (loop for (nil . jobs) in (activity-sections workbench)
             sum (+ *activity-heading-height* (* *activity-row-height* (length jobs))))))

(-> activity--menu (workbench string) null)
(defun activity--menu (workbench identifier)
  "Open the menu of the job IDENTIFIER at the pointer."
  (let ((stage (workbench-stage workbench)))
    (workbench-menu
     workbench
     (list (make-choice :label "Cancel"
                        :value (lambda () (workbench-cancel-job workbench identifier)))
           (make-choice :label "Copy the cancel call"
                        :value (lambda ()
                                 (lightfast:copy-text (format nil "(job.cancel :id ~S)" identifier))))
           (make-choice :label "Copy the identifier"
                        :value (lambda () (lightfast:copy-text identifier))))
     :x (stage-pointer-x stage)
     :y (stage-pointer-y stage)))
  nil)

(-> activity--paint-row (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun activity--paint-row (workbench stage job &key x y width)
  "Draw one row for JOB.

The row contains the working mark, the identifier, the role, the current activity,
the elapsed time and a cancel button with a cross."
  (let* ((meta       (text-style ':meta))
         (identifier (portable-string (portable-get job "id")))
         (role       (princ-to-string (or (portable-get job "role") "job")))
         (elapsed    (portable-get job "elapsedSeconds"))
         (height     *activity-row-height*)
         (right      (- (+ x width) 22))
         (cursor     (+ x 22)))
    (if (equal (portable-get job "state") "running")
        (progn
          (setf (stage-animating-p stage) t)
          (paint-spinner (+ x 2) (+ y 5) :size 14 :phase (stage-phase stage) :color *green*))
        (paint-mark ':ring (+ x 2) (+ y 5) :size 14))
    (when identifier
      (paint-button stage (list ':cancel-job identifier) ""
                    :x         right
                    :y         (+ y 3)
                    :width     18
                    :height    18
                    :mark      ':cross
                    :mark-size 10
                    :action    (lambda () (workbench-cancel-job workbench identifier))
                    :tooltip   "Cancel")
      (stage-region stage (list ':job identifier)
                    :x      x
                    :y      y
                    :width  (- right x 4)
                    :height height
                    :menu   (lambda () (activity--menu workbench identifier))
                    :cursor ':default))
    (when (integerp elapsed)
      (let ((text (chrome--elapsed elapsed)))
        (decf right (+ 8 (text-width text meta)))
        (paint-text-in text meta :x right :y y :width (text-width text meta) :height height)))
    (flet ((part (text style)
             (let* ((fitted (text-fit text style (max 0 (- right cursor 8))))
                    (wide   (text-width fitted style)))
               (when (plusp (length fitted))
                 (paint-text-in fitted style :x cursor :y y :width wide :height height)
                 (setf cursor (+ cursor wide 10))))))
      (when identifier
        (part identifier (text-style ':meta-bold :color *blue*)))
      (part role (text-style ':meta :color (if (position #\. role) *amber* *plum*)))
      (part (entries--one-line (princ-to-string (or (portable-get job "activity")
                                                    (portable-get job "state")
                                                    "")))
            meta)))
  nil)

(-> activity-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun activity-paint (workbench stage &key x y width height)
  "Draw the activity list in the box. Each kind has a heading and one row for each running job of that kind.

The number of jobs that do not fit is shown in the last line."
  (let ((bottom  (+ y height))
        (row-y   y)
        (dropped 0))
    (with-clip (x y width height)
      (loop for (name . jobs) in (activity-sections workbench)
            do (if (> (+ row-y *activity-heading-height* *activity-row-height*) bottom)
                   (incf dropped (length jobs))
                   (progn
                     (paint-text-in name (text-style ':meta-bold)
                                    :x      (+ x 2)
                                    :y      row-y
                                    :width  (- width 4)
                                    :height *activity-heading-height*)
                     (incf row-y *activity-heading-height*)
                     (dolist (job jobs)
                       (if (> (+ row-y *activity-row-height*) bottom)
                           (incf dropped)
                           (progn
                             (activity--paint-row workbench stage job :x x :y row-y :width width)
                             (incf row-y *activity-row-height*)))))))
      (when (plusp dropped)
        (paint-fill x (- bottom 18) :width width :height 18 :color *paper*)
        (paint-text-in (format nil "and ~D more" dropped) (text-style ':meta :face ':mono-italic)
                       :x      (+ x 22)
                       :y      (- bottom 18)
                       :width  (- width 22)
                       :height 18))))
  nil)
