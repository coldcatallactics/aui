(in-package #:aui)

;;;; -- Panels --

;;; What the channel and its workspace know, each shown in a window of its
;;; own instead of printed into the conversation: the agenda, the plan, what
;;; is remembered, and the output of a command that only reports.

(-> panel--line (string &key (:style text-style) (:margin-bottom integer)) text-block)
(defun panel--line (text &key (style (text-style ':meta)) (margin-bottom 10))
  "Return a block of TEXT in STYLE, the interface's small voice by default."
  (make-text-block (list (make-span text nil)) :style style :margin-bottom margin-bottom))

(-> panel--empty (string) list)
(defun panel--empty (text)
  "Return the blocks of a panel that has nothing to list, saying so with TEXT."
  (list (panel--line text :style (text-style ':small :face ':serif-italic))))


;;;; -- Agenda --

(-> panel--agenda-blocks (list string) list)
(defun panel--agenda-blocks (items directory)
  "Return the blocks of the agenda ITEMS of the workspace at DIRECTORY: what is in hand first, then what is to do, blocked and noted, then what is done."
  (if (null items)
      (panel--empty "The agenda of this workspace is empty. Ask the agent to put something on it.")
      (let ((order '(:doing :todo :blocked :note :done)))
        (cons
         (panel--line (format nil "~A  ·  ~D item~:P" (short-directory directory) (length items))
                      :margin-bottom 14)
         (loop for item in (stable-sort (copy-list items) #'<
                                        :key (lambda (item)
                                               (or (position (getf item :status) order) 1)))
               append (let* ((status (getf item :status))
                             (style  (ecase status
                                       (:doing
                                        (text-style ':list-bold :color *blue*))
                                       (:blocked
                                        (text-style ':list :color *red*))
                                       (:done
                                        (text-style ':list :strike-p t))
                                       (:note
                                        (text-style ':list :face ':serif-italic))
                                       (:todo
                                        (text-style ':list)))))
                        (list
                         (make-text-block (list (make-span (getf item :text) style))
                                          :style      (text-style ':list)
                                          :indent     26
                                          :marker     (case status
                                                        (:done
                                                         ':done)
                                                        (:note
                                                         "–")
                                                        (otherwise
                                                         ':open))
                                          :margin-top 6)
                         (make-text-block
                          (list (make-span (format nil "~(~A~)  ·  ~A~@[  ·  ~D memor~:@P~]"
                                                   status
                                                   (getf item :id)
                                                   (and (getf item :memories)
                                                        (length (getf item :memories))))
                                           nil))
                          :style         (text-style ':meta)
                          :indent        26
                          :margin-bottom 6))))))))

(-> panel-show-agenda (workbench) null)
(defun panel-show-agenda (workbench)
  "Show the agenda of the workspace the channel works in."
  (let ((directory (workbench-directory workbench)))
    (workbench-show-panel
     workbench "Agenda"
     (handler-case (panel--agenda-blocks (if (workbench-demo-p workbench)
                                             (demo-agenda)
                                             (workspace-agenda directory))
                                         directory)
       (client-error (condition)
         (panel--empty (client-error-message condition))))))
  nil)


;;;; -- Memories --

(-> panel--memory-blocks (list) list)
(defun panel--memory-blocks (memories)
  "Return the blocks of MEMORIES: each one's title, why it may no longer hold, what it says, and where it is from."
  (if (null memories)
      (panel--empty "Nothing is remembered for this workspace yet. Ask the agent to remember something.")
      (let ((stale      (count ':stale memories :key (lambda (memory) (getf memory :status))))
            (unverified (count ':unverified memories :key (lambda (memory) (getf memory :status)))))
        (cons
         (panel--line (format nil "~D memor~:@P~[~:;  ·  ~:*~D stale~]~[~:;  ·  ~:*~D unverified~]"
                              (length memories) stale unverified)
                      :margin-bottom 14)
         (loop for memory in memories
               append (let ((status (getf memory :status)))
                        (append
                         (list (make-text-block
                                (list (make-span (or (getf memory :title) "") nil))
                                :style      (text-style ':list-bold)
                                :margin-top 10))
                         (unless (eq status ':current)
                           (list (panel--line (format nil "~(~A~): ~A" status
                                                      (entries--one-line (or (getf memory :summary) "")))
                                              :style         (text-style ':meta
                                                                         :color (if (eq status ':stale)
                                                                                    *red*
                                                                                    *amber*))
                                              :margin-bottom 2)))
                         (let ((blocks (prose-blocks (or (getf memory :content) "")
                                                     :style (text-style ':small))))
                           (when blocks
                             (setf (block-margin-top (first blocks))           2
                                   (block-margin-bottom (first (last blocks))) 4))
                           blocks)
                         (list (panel--line (format nil "~A  ·  ~(~A~)~@[  ·  ~A~]  ·  ~A"
                                                    (getf memory :id)
                                                    (getf memory :scope)
                                                    (getf memory :provenance)
                                                    (getf memory :date))
                                            :margin-bottom 8)))))))))

(-> panel-show-memories (workbench) null)
(defun panel-show-memories (workbench)
  "Show what is remembered for the workspace the channel works in."
  (workbench-show-panel
   workbench "Memories"
   (handler-case (panel--memory-blocks (if (workbench-demo-p workbench)
                                           (demo-memories)
                                           (workspace-memories (workbench-directory workbench))))
     (client-error (condition)
       (panel--empty (client-error-message condition)))))
  nil)


;;;; -- Output --

(-> panel-show-output (workbench string string) null)
(defun panel-show-output (workbench title operation)
  "Show what the channel's OPERATION reports in a window titled TITLE, without printing it into the conversation."
  (let ((session (workbench-session workbench)))
    (cond
      ((and session (workbench-attached-p workbench))
       (workbench-show-panel workbench title nil)
       (session-operation session "_autolith/invoke"
                          :params  (portable-object "operation" operation)
                          :request (list ':panel title)))
      (t
       (workbench-show-panel workbench title (panel--empty "Not attached to a channel yet.")))))
  nil)

(-> panel-take-output (workbench string string) null)
(defun panel-take-output (workbench title text)
  "Put TEXT, which the channel reported, into the panel titled TITLE when that is still the one shown."
  (let ((overlay (workbench-overlay workbench)))
    (when (and overlay
               (eq (overlay-kind overlay) ':panel)
               (equal (overlay-title overlay) title))
      (setf (overlay-content overlay)
            (make-instance 'box-block
                           :children  (list (make-text-block
                                             (list (make-span (string-right-trim '(#\Newline #\Space) text) nil))
                                             :style (text-style ':code)))
                           :frame-p   nil
                           :padding-x 0
                           :padding-y 0))
      (workbench-redraw workbench)))
  nil)


;;;; -- Plan --

(-> panel-show-plan (workbench) null)
(defun panel-show-plan (workbench)
  "Show the plan the agent keeps: its latest update in the conversation, or what the channel reports when none is in view."
  (let ((update (find-if (lambda (entry)
                           (and (typep entry 'tool-entry) (string= (tool-entry-name entry) "plan.update")))
                         (transcript-entries (workbench-transcript workbench))
                         :from-end t)))
    (if update
        (workbench-show-panel workbench "Plan" (entries--plan-steps update))
        (panel-show-output workbench "Plan" "plan.list")))
  nil)
