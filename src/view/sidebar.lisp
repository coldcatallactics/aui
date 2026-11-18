(in-package #:aui)

;;;; -- Sidebar --

;;; The world's channels at the window's left: home, then beneath it each
;;; group with its channels, children under their parents. A group is a
;;; repository with all its worktrees, or a folder that belongs to none. The
;;; tree is Autolith's own sidebar model, the one its terminal draws, and the
;;; grouping is its own too; this file only gives it a face. The current
;;; channel is inverted, a working agent carries the green dot, and unread
;;; counts sit at the right as small inverted badges. A new channel and the
;;; deletion of the one shown are at the list's foot, where the terminal has
;;; them. On a window too narrow to hold the list beside the conversation it
;;; takes the whole window.

(defparameter *sidebar-width* 292
  "The width of the docked channel list.")

(defparameter *sidebar-row-height* 30
  "The height of one channel row.")

(defparameter *sidebar-docked-minimum* 980
  "The narrowest window that still docks the channel list beside the conversation.")

(defparameter *sidebar-home-indent* 12
  "How far the workspaces and channels beneath home are set in from it.")

(defparameter *sidebar-list-top* 106
  "How far below the channel list's top edge its rows begin, under the heading and the filter.")

(defparameter *sidebar-footer-height* 56
  "The room at the channel list's foot for the buttons that make and delete a channel.")

(-> sidebar--indent (list) integer)
(defun sidebar--indent (entry)
  "Return how far ENTRY's row is set in for standing beneath home."
  (if (plusp (or (getf entry :indent) 0))
      *sidebar-home-indent*
      0))

(-> sidebar-entries (workbench) list)
(defun sidebar-entries (workbench)
  "Return the channel tree to show: the world's, or the demonstration's."
  (if (workbench-demo-p workbench)
      (workbench-demo-entries workbench)
      (world-entries (workbench-world workbench))))

(-> sidebar--matches-p (list string) boolean)
(defun sidebar--matches-p (entry filter)
  "Return T when the channel ENTRY's name, title or identifier contains FILTER, ignoring case."
  (and (some (lambda (text)
               (and (stringp text) (search filter text :test #'char-equal)))
             (list (getf entry :name) (getf entry :title) (getf entry :id)
                   (and (getf entry :id) (channel-display-name (getf entry :id)))))
       t))

(-> sidebar--visible-entries (workbench) list)
(defun sidebar--visible-entries (workbench)
  "Return the entries to list: the whole tree, or with a filter typed only the channels that match it."
  (let ((entries (sidebar-entries workbench))
        (filter  (string-trim " " (workbench-filter workbench))))
    (if (zerop (length filter))
        entries
        (remove-if-not (lambda (entry)
                         (and (eq (getf entry :kind) ':channel)
                              (sidebar--matches-p entry filter)))
                       entries))))

(-> sidebar--channel-label (list) (values string (or null string)))
(defun sidebar--channel-label (entry)
  "Return the name the channel ENTRY goes by and its title, the title NIL when it has none."
  (let ((name  (portable-string (getf entry :name)))
        (title (portable-string (getf entry :title))))
    (values (or name (channel-display-name (getf entry :id)))
            title)))

(-> sidebar--entry-label (workbench string) string)
(defun sidebar--entry-label (workbench identifier)
  "Return what the channel IDENTIFIER goes by in the list: its title, else its agent's name, else its identifier as people read it."
  (let ((entry (find identifier (sidebar-entries workbench)
                     :key  (lambda (entry)
                             (getf entry :id))
                     :test #'equal)))
    (or (portable-string (getf entry :title))
        (portable-string (getf entry :name))
        (channel-display-name identifier))))

(-> sidebar-confirm-delete (workbench string) null)
(defun sidebar-confirm-delete (workbench identifier)
  "Ask whether the channel IDENTIFIER is to be deleted, and delete it on a yes."
  (workbench-pick workbench
                  ;; Enter takes the first choice, so the one that loses nothing comes first.
                  (list (make-choice :label "Keep"
                                     :value ':keep)
                        (make-choice :label  "Delete"
                                     :detail "The channel, the channels nested in it and their histories"
                                     :value  ':delete))
                  (lambda (answer)
                    (when (eq answer ':delete)
                      (workbench-delete-channel workbench identifier)))
                  :title (format nil "Delete ~A?" (sidebar--entry-label workbench identifier)))
  nil)

(-> sidebar--close-drawer (workbench) null)
(defun sidebar--close-drawer (workbench)
  "Put the channel list away when it takes the whole window, so that what was chosen in it is seen."
  (setf (workbench-drawer-open-p workbench) nil)
  nil)

(-> sidebar--open (workbench string) null)
(defun sidebar--open (workbench identifier)
  "Show the channel IDENTIFIER, chosen in the list, and give the keyboard to the composer."
  (sidebar--close-drawer workbench)
  (unless (equal identifier (workbench-channel workbench))
    (workbench-open workbench :channel identifier))
  (window-arrange workbench)
  (window-focus-composer workbench)
  (workbench-redraw workbench)
  nil)

(-> sidebar--home-p (workbench (or null string)) boolean)
(defun sidebar--home-p (workbench identifier)
  "Return T when IDENTIFIER names home, the channel every other one stands beneath, which is never deleted."
  (and identifier
       (find-if (lambda (entry)
                  (and (eq (getf entry :kind) ':inbox)
                       (equal (getf entry :id) identifier)))
                (sidebar-entries workbench))
       t))

(-> sidebar--channel-menu (workbench list) null)
(defun sidebar--channel-menu (workbench entry)
  "Offer what can be done with the channel ENTRY in a menu at the pointer, and do what is chosen."
  (let ((identifier (getf entry :id))
        (stage      (workbench-stage workbench)))
    (workbench-menu
     workbench
     (list (make-choice :label "Open"
                        :value (lambda () (sidebar--open workbench identifier)))
           (make-choice :label "Send a message…"
                        :value (lambda ()
                                 (workbench-ask workbench
                                                (format nil "Message to ~A" (channel-display-name identifier))
                                                (lambda (text)
                                                  (when (plusp (length (string-trim " " text)))
                                                    (workbench-tell workbench identifier text))))))
           (make-choice :label "Copy identifier"
                        :value (lambda () (lightfast:copy-text identifier)))
           (make-choice)
           (make-choice :label "Stop serving"
                        :value (lambda () (workbench-close-channel workbench identifier)))
           (make-choice :label "Delete…"
                        :value (lambda () (sidebar-confirm-delete workbench identifier))))
     :x (stage-pointer-x stage)
     :y (stage-pointer-y stage)))
  nil)

(-> paint-channel-state (t integer integer &key (:waiting-for-jobs-p boolean)) null)
(defun paint-channel-state (state x y &key waiting-for-jobs-p)
  "Draw the mark of a channel in STATE in the 12 by 12 box at X, Y: the green dot for one at work, a ring for one that rests, with a green point in it when WAITING-FOR-JOBS-P, and a speck for one the server does not hold."
  (case state
    ((:active :working :cancelling)
     (paint-mark ':dot x y :size 12 :color (accent-color)))
    ((:idle :paused)
     ;; Idle with jobs still out: the ring keeps a point of the working green.
     (paint-mark ':ring x y :size 12)
     (when waiting-for-jobs-p
       (paint-mark ':point x y :size 12 :color (accent-color))))
    (:absent
     (paint-fill (+ x 5) (+ y 5) :width 2 :height 2))
    (otherwise
     (paint-mark ':ring x y :size 12)))
  nil)

(-> sidebar--paint-channel (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-channel (workbench stage entry &key x y width)
  "Draw one channel row: its state mark, its agent's name and title, and what is unread."
  (let* ((identifier (getf entry :id))
         (region     (list ':channel identifier))
         (current-p  (or (getf entry :current-p)
                         (equal identifier (workbench-channel workbench))))
         (hot-p      (stage-hot-p stage region))
         (state      (getf entry :state))
         (indent     (+ 14 (sidebar--indent entry) (* 16 (or (getf entry :depth) 0))))
         (height     *sidebar-row-height*)
         (unread     (or (getf entry :unread) 0))
         (mentions   (or (getf entry :mentions) 0))
         (pending    (or (getf entry :pending) 0)))
    (flet ((content ()
             (let ((mark-y (+ y (floor (- height 12) 2)))
                   (right  (- (+ x width) 10)))
               (paint-channel-state state (+ x indent) mark-y
                                    :waiting-for-jobs-p (and (getf entry :waiting-for-jobs-p) t))
               (when (plusp mentions)
                 (decf right (+ 6 (paint-badge (format nil "@~D" mentions) :right right :y y :height height))))
               (when (plusp unread)
                 (decf right (+ 6 (paint-badge (princ-to-string unread) :right right :y y :height height))))
               (when (plusp pending)
                 (let ((text (format nil "~D waiting" pending)))
                   (paint-text-in text (text-style ':meta)
                                  :x      (- right (text-width text (text-style ':meta)))
                                  :y      y
                                  :width  (text-width text (text-style ':meta))
                                  :height height)
                   (decf right (+ 8 (text-width text (text-style ':meta))))))
               (multiple-value-bind (name title) (sidebar--channel-label entry)
                 (let* ((text-x (+ x indent 20))
                        (bold   (text-style ':small :face ':serif-bold))
                        (plain  (text-style ':small))
                        (fitted (text-fit name bold (max 0 (- right text-x))))
                        (end    (paint-text fitted text-x (+ y (floor (- height (text-style-line-height bold)) 2)
                                                           (text-baseline bold))
                                            :style bold)))
                   (when (and title (> (- right end) 40))
                     (paint-text (text-fit title plain (- right end 10)) (+ end 10) (+ y (floor (- height (text-style-line-height plain)) 2)
                                                                                     (text-baseline plain))
                                 :style plain)))))))
      (if (or current-p hot-p)
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x       x
                  :y       y
                  :width   width
                  :height  height
                  :action  (lambda () (sidebar--open workbench identifier))
                  :menu    (lambda () (sidebar--channel-menu workbench entry))
                  :tooltip (channel-display-name identifier)))
  nil)

(-> sidebar--paint-workspace (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-workspace (workbench stage entry &key x y width)
  "Draw a workspace heading: its open or closed mark, its name, its path, and what a closed one hides."
  (let* ((key    (getf entry :key))
         (region (list ':workspace key))
         (height *sidebar-row-height*)
         (bold   (text-style ':meta-bold))
         (meta   (text-style ':meta))
         (hot-p  (stage-hot-p stage region))
         (left   (+ x (sidebar--indent entry))))
    (flet ((content ()
             (paint-mark (if (getf entry :open-p) ':open ':closed) (+ left 12) (+ y (floor (- height 10) 2))
                         :size 10)
             (let* ((right (- (+ x width) 10))
                    (end   (paint-text (text-fit (or (getf entry :name) "") bold (- width 60 (- left x)))
                                       (+ left 28)
                                       (+ y (floor (- height (text-style-line-height bold)) 2) (text-baseline bold))
                                       :style bold)))
               (when (and (not (getf entry :open-p)) (getf entry :hidden))
                 (decf right (+ 6 (paint-badge (format nil "+~D" (getf entry :hidden))
                                               :right  right
                                               :y      y
                                               :height height))))
               (when (> (- right end) 60)
                 (paint-text-in (text-fit-tail (or (getf entry :path) "") meta (- right end 12)) meta
                                :x      (+ end 12)
                                :y      y
                                :width  (- right end 12)
                                :height height)))))
      (if hot-p
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :action (lambda ()
                            (unless (workbench-demo-p workbench)
                              (world-toggle-node (workbench-world workbench) key))
                            (workbench-redraw workbench))))
  nil)

(-> sidebar--paint-home (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-home (workbench stage entry &key x y width)
  "Draw the row of home, the channel every other one stands beneath, with the count of what waits there unread."
  (let* ((identifier (getf entry :id))
         (region     (list ':channel identifier))
         (height     *sidebar-row-height*)
         (current-p  (or (getf entry :current-p) (equal identifier (workbench-channel workbench))))
         (hot-p      (stage-hot-p stage region))
         (unread     (or (getf entry :unread) 0)))
    (flet ((content ()
             (paint-text-in "Home" (text-style ':small :face ':serif-bold)
                            :x      (+ x 14)
                            :y      y
                            :width  (- width 80)
                            :height height)
             (when (plusp unread)
               (paint-badge (princ-to-string unread) :right (- (+ x width) 10) :y y :height height))))
      (if (or current-p hot-p)
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x       x
                  :y       y
                  :width   width
                  :height  height
                  :action  (lambda () (sidebar--open workbench identifier))
                  :tooltip "Home: what was sent to you, with every channel beneath it"))
  nil)

(-> sidebar--paint-footer (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-footer (workbench stage &key x y width)
  "Draw the two buttons at the list's foot: a new channel, and the deletion of the channel shown."
  (let* ((channel  (workbench-channel workbench))
         (delete-w 104)
         (new-w    (- width 28 8 delete-w)))
    (paint-fill x y :width width :height *sidebar-footer-height* :color *paper*)
    (paint-button stage ':new-channel "New channel"
                  :x         (+ x 14)
                  :y         (+ y 10)
                  :width     new-w
                  :height    34
                  :mark      ':plus
                  :mark-size 13
                  :action    (lambda ()
                               (sidebar--close-drawer workbench)
                               (workbench-new-channel workbench)
                               (window-arrange workbench)
                               (window-focus-composer workbench))
                  :tooltip   (format nil "A new channel in ~A (Ctrl+N)"
                                     (short-directory (workbench-directory workbench))))
    (paint-button stage ':delete-channel "Delete…"
                  :x         (+ x 14 new-w 8)
                  :y         (+ y 10)
                  :width     delete-w
                  :height    34
                  :style     (text-style ':label-bold :color *red*)
                  :enabled-p (and channel (not (sidebar--home-p workbench channel)))
                  :action    (lambda ()
                               (sidebar-confirm-delete workbench channel))
                  :tooltip   "Delete the channel shown, the channels nested in it and their histories"))
  nil)

(-> sidebar-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sidebar-paint (workbench stage &key x y width height)
  "Draw the channel list in the box: the wordmark with the button that puts the list away, the field that narrows it, its rows, and at its foot the buttons for a new channel and for deleting the one shown."
  (let* ((entries     (sidebar--visible-entries workbench))
         (top         (+ y *sidebar-list-top*))
         (bottom      (- (+ y height) *sidebar-footer-height*))
         (list-height (max 0 (- bottom top)))
         (content     (* *sidebar-row-height* (length entries)))
         (scroll      (max 0 (min (workbench-sidebar-scroll workbench) (max 0 (- content list-height))))))
    (setf (workbench-sidebar-scroll workbench) scroll)
    (paint-fill x y :width width :height height :color *paper*)
    (paint-text "Autolith" (+ x 14) (+ y 40) :style (text-style ':title))
    (paint-button stage ':hide-sidebar ""
                  :x       (- (+ x width) 42)
                  :y       (+ y 16)
                  :width   28
                  :height  28
                  :mark    ':panel
                  :action  (lambda () (window-toggle-sidebar workbench))
                  :tooltip "Hide the channels (Ctrl+S)")
    ;; The filter field is a native input laid over this frame; while an
    ;; overlay has it put away, what it holds is drawn in its place.
    (paint-frame (+ x 14) (+ y 58) :width (- width 28) :height 34)
    (when (workbench-overlay workbench)
      (let ((filter (workbench-filter workbench)))
        (paint-text-in (if (plusp (length filter)) filter "Find a channel")
                       (text-style ':list :face (if (plusp (length filter)) ':serif ':serif-italic))
                       :x      (+ x 20)
                       :y      (+ y 58)
                       :width  (- width 40)
                       :height 34)))
    (stage-region stage ':sidebar-list
                  :x      x
                  :y      top
                  :width  width
                  :height list-height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (workbench-sidebar-scroll workbench)
                                  (max 0 (+ (workbench-sidebar-scroll workbench)
                                            (* vertical *sidebar-row-height*))))))
    (with-clip (x top width list-height)
      (loop for entry in entries
            for row-y from (- top scroll) by *sidebar-row-height*
            do (when (and (< row-y bottom) (> (+ row-y *sidebar-row-height*) top))
                 (ecase (getf entry :kind)
                   (:inbox
                    (sidebar--paint-home workbench stage entry :x x :y row-y :width width))
                   (:workspace
                    (sidebar--paint-workspace workbench stage entry :x x :y row-y :width width))
                   (:channel
                    (sidebar--paint-channel workbench stage entry :x x :y row-y :width width)))))
      (when (null entries)
        (paint-text-in (if (plusp (length (workbench-filter workbench)))
                           "No channel matches."
                           "No channels yet.") (text-style ':small :face ':serif-italic)
                       :x      (+ x 14)
                       :y      top
                       :width  (- width 28)
                       :height 40)))
    (sidebar--paint-footer workbench stage :x x :y bottom :width width))
  nil)
