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


;;;; -- Cards --

(-> panel--card (list) box-block)
(defun panel--card (blocks)
  "Return a card, a framed box, holding BLOCKS."
  (make-instance 'box-block
                 :children      blocks
                 :padding-x     16
                 :padding-y     12
                 :margin-bottom 14))

(-> panel--card-head (list) text-block)
(defun panel--card-head (parts)
  "Return the line at the head of a card from PARTS, each a string or a (TEXT STYLE) list, set apart by middle dots."
  (make-text-block (loop for (part . more) on (remove nil parts)
                         collect (if (consp part)
                                     (make-span (first part) (second part))
                                     (make-span part nil))
                         when more
                           collect (make-span "  ·  " nil))
                   :style         (text-style ':meta)
                   :margin-bottom 6))

(-> panel--tabs (list list function) list)
(defun panel--tabs (names things card)
  "Return the tabs of a board over THINGS: one for all of them, then one for each of NAMES, a list of (KEY . LABEL), holding the things whose :STATUS is KEY, each drawn by the function CARD.

A tab with nothing in it says so."
  (flet ((tab (label members)
           (list* label
                  (length members)
                  (or (mapcar card members)
                      (panel--empty "Nothing here.")))))
    (cons (tab "All" things)
          (loop for (key . label) in names
                collect (tab label (remove-if-not (lambda (thing)
                                                    (eq (getf thing :status) key))
                                                  things))))))


;;;; -- Agenda --

(-> panel--agenda-card (list) box-block)
(defun panel--agenda-card (item)
  "Return the card of the agenda ITEM: what it is and how it stands at its head, its text as Markdown, and the memories attached to it."
  (let ((status (getf item :status)))
    (panel--card
     (append
      (list (panel--card-head
             (list (list (ecase status
                           (:doing
                            "Doing")
                           (:todo
                            "To do")
                           (:blocked
                            "Blocked")
                           (:note
                            "Note")
                           (:done
                            "Done"))
                         (text-style ':meta-bold
                                     :color (ecase status
                                              (:doing
                                               *blue*)
                                              (:blocked
                                               *red*)
                                              (:done
                                               *green*)
                                              (:note
                                               *plum*)
                                              (:todo
                                               nil))))
                   (getf item :id)
                   (and (getf item :memories)
                        (format nil "~D memor~:@P" (length (getf item :memories)))))))
      (let ((blocks (prose-blocks (or (getf item :text) "") :style (text-style ':list) :breaks-p t)))
        (when blocks
          (setf (block-margin-top (first blocks))           0
                (block-margin-bottom (first (last blocks))) 0))
        blocks)))))

(-> panel-show-agenda (workbench) null)
(defun panel-show-agenda (workbench)
  "Show the agenda of the workspace the channel works in as a board: a tab for each state an item can be in, and the items as cards."
  (let ((directory (workbench-directory workbench)))
    (handler-case
        (let ((items (stable-sort (copy-list (if (workbench-demo-p workbench)
                                                 (demo-agenda)
                                                 (workspace-agenda directory)))
                                  #'<
                                  :key (lambda (item)
                                         (or (position (getf item :status) '(:doing :todo :blocked :note :done))
                                             1)))))
          (workbench-show-board workbench
                                (format nil "Agenda of ~A" (short-directory directory))
                                (panel--tabs '((:doing . "Doing") (:todo . "To do") (:blocked . "Blocked")
                                               (:note . "Notes") (:done . "Done"))
                                             items
                                             #'panel--agenda-card)))
      (client-error (condition)
        (workbench-show-panel workbench "Agenda" (panel--empty (client-error-message condition))))))
  nil)


;;;; -- Memories --

(-> panel--memory-card (list) box-block)
(defun panel--memory-card (memory)
  "Return the card of MEMORY: how it stands and where it is from at its head, its title, why it may no longer hold, and what it says as Markdown."
  (let ((status (getf memory :status)))
    (panel--card
     (append
      (list (panel--card-head
             (list (list (ecase status
                           (:current
                            "Current")
                           (:stale
                            "Stale")
                           (:unverified
                            "Unverified"))
                         (text-style ':meta-bold
                                     :color (ecase status
                                              (:current
                                               *green*)
                                              (:stale
                                               *red*)
                                              (:unverified
                                               *amber*))))
                   (string-downcase (princ-to-string (getf memory :scope)))
                   (getf memory :date)
                   (getf memory :id)))
            (make-text-block (list (make-span (or (getf memory :title) "") nil))
                             :style         (text-style ':list-bold)
                             :margin-bottom 4))
      (unless (eq status ':current)
        (list (panel--line (entries--one-line (or (getf memory :summary) ""))
                           :style         (text-style ':meta :color (if (eq status ':stale) *red* *amber*))
                           :margin-bottom 6)))
      (let ((blocks (prose-blocks (or (getf memory :content) "") :style (text-style ':small))))
        (when blocks
          (setf (block-margin-top (first blocks))           0
                (block-margin-bottom (first (last blocks))) 0))
        blocks)
      (when (getf memory :provenance)
        (list (panel--line (format nil "from ~A" (getf memory :provenance)) :margin-bottom 0)))))))

(-> panel-show-memories (workbench) null)
(defun panel-show-memories (workbench)
  "Show what is remembered for the workspace the channel works in as a board: a tab for each state a memory can be in, and the memories as cards."
  (handler-case
      (let ((memories (if (workbench-demo-p workbench)
                          (demo-memories)
                          (workspace-memories (workbench-directory workbench)))))
        (workbench-show-board workbench
                              "Memories"
                              (panel--tabs '((:stale . "Stale") (:unverified . "Unverified")
                                             (:current . "Current"))
                                           memories
                                           #'panel--memory-card)))
    (client-error (condition)
      (workbench-show-panel workbench "Memories" (panel--empty (client-error-message condition)))))
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
