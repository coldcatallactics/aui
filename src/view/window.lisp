(in-package #:aui)

;;;; -- Window --

;;; The window itself: where each pane goes at the current size, the native
;;; fields laid over the stage, and what the keys do. Wide windows dock the
;;; channel list beside the conversation; narrow ones show it as a drawer
;;; over it, as the terminal does.

(defparameter *window-default-size* '(1180 . 820)
  "The window's (WIDTH . HEIGHT) when the command line names none.")

(defparameter *window-minimum-size* '(520 . 420)
  "The smallest (WIDTH . HEIGHT) the window is allowed.")

(defparameter *console-minimum-reading* 520
  "The narrowest the conversation may become for the console to stand beside it.")

(defparameter *console-minimum-width* 380
  "The narrowest the console may become.")

(defparameter *console-default-share* 42/100
  "The part of the width beside the channel list that the console takes until the person drags its edge.")

(-> window-console-share (workbench) real)
(defun window-console-share (workbench)
  "Return the part of the width beside the channel list that the console takes: where the person dragged it in this window, else what was remembered, else the default."
  (let ((share (or (workbench-console-share workbench)
                   (preference ':console-share))))
    (if (and (realp share) (< 0 share 1))
        share
        *console-default-share*)))

(-> (setf window-console-share) (real workbench) real)
(defun (setf window-console-share) (share workbench)
  "Take SHARE as the console's part of the width, for this window now and for the next run once the drag ends."
  (setf (workbench-console-share workbench) (max 1/10 (min 9/10 share)))
  (workbench-redraw workbench)
  share)

(-> window--layout (workbench integer integer) list)
(defun window--layout (workbench width height)
  "Return where each pane goes in a window WIDTH by HEIGHT, as a property list of (X Y WIDTH HEIGHT) boxes.

The conversation is a column with the composer at its foot and, under the
composer, the line that says what the agent is doing. A window wide enough
sets the console at the right of that column: what the channel is, what runs
for it, and under that the plan, the files and the messages read from the
conversation. A narrower one puts the header above the conversation and the
running things above the composer. :DOCKED-P says the channel list stands
beside the conversation, :DRAWER-P that it is shown on a window too narrow
for that, where it takes the whole window, and :SIDE-P that the console
stands beside the conversation."
  (let* ((roomy-p    (>= width *sidebar-docked-minimum*))
         (docked-p   (and roomy-p (workbench-sidebar-shown-p workbench)))
         (drawer-p   (and (not roomy-p) (workbench-drawer-open-p workbench)))
         (side-width (if docked-p *sidebar-width* 0))
         (main-x     side-width)
         (main-width (- width side-width))
         (side-p     (>= main-width (+ *console-minimum-reading* *console-minimum-width* 60)))
         (console-w  (if side-p
                         (max *console-minimum-width*
                              (min (- main-width *console-minimum-reading*)
                                   (round (* main-width (window-console-share workbench)))))
                         0))
         (reading-w  (- main-width console-w))
         (console-x  (+ main-x reading-w))
         (column     (max 200 (min (+ *document-measure* 24) (- reading-w 40))))
         (column-x   (+ main-x (floor (- reading-w column) 2)))
         (composer-h (composer-height workbench column))
         (composer-y (- height *status-height* 6 composer-h 2))
         (notice-h   (if (workbench-notice workbench) 26 0))
         (pending-h  (pending-height workbench))
         (approval-h (approval-height workbench column))
         (below      (+ notice-h pending-h approval-h))
         ;; The activity takes what it needs: in the console, of the upper
         ;; half of the room under its head; stacked, a few rows above the
         ;; composer.
         (activity-h (if side-p
                         (activity-height workbench :most (max 0 (floor (- height *header-height* 20) 2)))
                         (activity-height workbench :most (+ *activity-heading-height*
                                                             (* 4 *activity-row-height*)))))
         (console-y  (+ *header-height* 8 activity-h))
         (top        (if side-p 0 *header-height*))
         (stack-top  (- composer-y 10 below (if side-p 0 activity-h))))
    (list :docked-p   docked-p
          :drawer-p   drawer-p
          :side-p     side-p
          :sidebar    (if drawer-p
                          (list 0 0 width height)
                          (list 0 0 *sidebar-width* height))
          :header     (if side-p
                          (list console-x 0 console-w *header-height*)
                          (list main-x 0 main-width *header-height*))
          :document   (list main-x top (- reading-w (if side-p 3 0)) (max 40 (- stack-top top)))
          :divider    (list (- console-x 3) 0 3 height)
          :activity   (if side-p
                          (list (+ console-x 16) (+ *header-height* 8) (- console-w 32) activity-h)
                          (list column-x stack-top column activity-h))
          :console    (list (+ console-x 16) console-y (- console-w 32) (max 0 (- height console-y 12)))
          :completion (let ((height (completion-height workbench)))
                        (list column-x (- composer-y height 4) column height))
          :approval   (list column-x (- composer-y 10 below) column approval-h)
          :pending    (list column-x (- composer-y 10 notice-h pending-h) column pending-h)
          :notice     (list column-x (- composer-y 10 notice-h) column notice-h)
          :composer   (list column-x composer-y column composer-h)
          :status     (list (+ column-x 2) (- height *status-height* 6) (- column 4) *status-height*))))

(-> window--narrow-p (workbench) boolean)
(defun window--narrow-p (workbench)
  "Return T when the window is too narrow to hold the channel list beside the conversation."
  (let ((stage (workbench-stage workbench)))
    (and stage (< (stage-width stage) *sidebar-docked-minimum*))))

(-> window-drawer-p (workbench) boolean)
(defun window-drawer-p (workbench)
  "Return T when the channel list is open on a window too narrow to set it beside the conversation, where it takes the whole window."
  (and (window--narrow-p workbench)
       (workbench-drawer-open-p workbench)))

(-> window-show-sidebar (workbench boolean) null)
(defun window-show-sidebar (workbench shown-p)
  "Show the channel list when SHOWN-P, or put it away, in the way the window's width calls for, and give the keyboard to what is then in front."
  (if (window--narrow-p workbench)
      (setf (workbench-drawer-open-p workbench) shown-p)
      (setf (workbench-sidebar-shown-p workbench) shown-p))
  (window-arrange workbench)
  (if (window-drawer-p workbench)
      (lightfast:take-focus (second (stage-natives (workbench-stage workbench))))
      (window-focus-composer workbench))
  (workbench-redraw workbench)
  nil)

(-> window-toggle-sidebar (workbench) null)
(defun window-toggle-sidebar (workbench)
  "Show the channel list when it is put away, or put it away."
  (window-show-sidebar workbench
                       (not (if (window--narrow-p workbench)
                                (workbench-drawer-open-p workbench)
                                (workbench-sidebar-shown-p workbench)))))


;;;; -- Painting --

(-> window--paint-console-edge (workbench stage list) null)
(defun window--paint-console-edge (workbench stage layout)
  "Draw the band of dither that parts the conversation from the console. The band can be dragged, and where it is left is remembered."
  (destructuring-bind (x y width height) (getf layout :divider)
    (paint-dither x y
                  :width  width
                  :height height
                  :step   (if (or (stage-hot-p stage ':console-edge)
                                  (stage-pressed-p stage ':console-edge))
                              2
                              3))
    (let* ((main-x     (if (getf layout :docked-p) *sidebar-width* 0))
           (main-width (- (stage-width stage) main-x)))
      (stage-region stage ':console-edge
                    :x      (- x 3)
                    :y      y
                    :width  10
                    :height height
                    :drag   (lambda (phase drag-x drag-y)
                              (declare (ignore drag-y))
                              (setf (window-console-share workbench)
                                    (/ (- (+ main-x main-width) drag-x) main-width))
                              (when (eq phase ':end)
                                (setf (preference ':console-share)
                                      (float (workbench-console-share workbench)))))
                    :cursor ':west-east)))
  nil)

(-> window--paint-draft (workbench list) null)
(defun window--paint-draft (workbench box)
  "Draw what the composer holds in BOX, the place of its editor, while an overlay has the editor put away."
  (destructuring-bind (x y width height) box
    (let* ((text  (composer-text workbench))
           (draft (make-paragraph (if (plusp (length text))
                                      (list (make-span text nil))
                                      (list (make-span "Write to the agent"
                                                       (text-style ':body :face ':serif-italic))))
                                  :style (text-style ':body))))
      (paragraph-layout draft (- width 8))
      (with-clip (x y width height)
        (paragraph-paint draft (+ x 4) y))))
  nil)

(-> window--paint (workbench stage) null)
(defun window--paint (workbench stage)
  "Draw the whole window: the channel list alone when it takes the window, else the panes at their places, and the overlay over them."
  (let* ((layout (window--layout workbench (stage-width stage) (stage-height stage)))
         (side-p (getf layout :side-p)))
    (flet ((box (name)
             (getf layout name)))
      (cond
        ;; On a window too narrow for both, the channel list is all there is.
        ((getf layout :drawer-p)
         (destructuring-bind (x y width height) (box :sidebar)
           (sidebar-paint workbench stage :x x :y y :width width :height height)))
        (t
         (when (getf layout :docked-p)
           (destructuring-bind (x y width height) (box :sidebar)
             (sidebar-paint workbench stage :x x :y y :width width :height height)))
         (destructuring-bind (x y width height) (box :document)
           (if (and (workbench-attached-p workbench)
                    (plusp (transcript-length (workbench-transcript workbench))))
               (document-paint (workbench-document workbench) stage :x x :y y :width width :height height)
               (empty-paint workbench stage :x x :y y :width width :height height))
           ;; The conversation ends on a band of dither above what is written to it.
           (paint-dither x (+ y height -3) :width width :height 3 :step 3)
           (when (and side-p (not (getf layout :docked-p)))
             (paint-button stage ':toggle-sidebar ""
                           :x       (+ x 14)
                           :y       (+ y 14)
                           :width   28
                           :height  28
                           :mark    ':panel
                           :action  (lambda () (window-toggle-sidebar workbench))
                           :tooltip "Show the channels (Ctrl+S)")))
         (when side-p
           (window--paint-console-edge workbench stage layout))
         (destructuring-bind (x y width height) (box :header)
           (declare (ignore height))
           (header-paint workbench stage :x x :y y :width width
                                         :sidebar-toggle-p (and (not side-p) (not (getf layout :docked-p)))
                                         :band-p           (not side-p)))
         (destructuring-bind (x y width height) (box :activity)
           (when (plusp height)
             (activity-paint workbench stage :x x :y y :width width :height height)))
         (when side-p
           (destructuring-bind (x y width height) (box :console)
             (when (plusp height)
               (console-paint workbench stage :x x :y y :width width :height height))))
         (when (workbench-approval workbench)
           (destructuring-bind (x y width height) (box :approval)
             (approval-paint workbench stage :x x :y y :width width :height height)))
         (destructuring-bind (x y width height) (box :pending)
           (declare (ignore height))
           (pending-paint workbench :x x :y y :width width))
         (when (workbench-notice workbench)
           (destructuring-bind (x y width height) (box :notice)
             (paint-mark ':cross x (+ y 5) :size 14 :color *red*)
             (paint-text-in (workbench-notice workbench) (text-style ':meta :color *red*)
                            :x      (+ x 20)
                            :y      y
                            :width  (- width 20)
                            :height height)))
         (destructuring-bind (x y width height) (box :composer)
           (composer-paint workbench stage :x x :y y :width width :height height)
           (when (workbench-overlay workbench)
             (window--paint-draft workbench
                                  (composer-editor-box workbench :x x :y y :width width :height height))))
         (destructuring-bind (x y width height) (box :status)
           (declare (ignore height))
           (status-paint workbench stage :x x :y y :width width))
         (destructuring-bind (x y width height) (box :completion)
           (when (plusp height)
             (completion-paint workbench stage :x x :y y :width width)))))
      (overlay-paint workbench stage)))
  nil)


;;;; -- Native Fields --

(-> window--place (t list boolean) null)
(defun window--place (widget box shown-p)
  "Show WIDGET in BOX, an (X Y WIDTH HEIGHT) list, or hide it when not SHOWN-P."
  (if shown-p
      (destructuring-bind (x y width height) box
        (lightfast:resize-widget widget :x x :y y :width (max 1 width) :height (max 1 height))
        (lightfast:show widget))
      (lightfast:hide widget))
  nil)

(-> window-arrange (workbench) null)
(defun window-arrange (workbench)
  "Put the native fields where the current layout has them, hiding those the drawer or an overlay covers."
  (let* ((stage    (workbench-stage workbench))
         (width    (stage-width stage))
         (height   (stage-height stage))
         (layout   (window--layout workbench width height))
         (overlay  (workbench-overlay workbench))
         (drawer-p (getf layout :drawer-p))
         (natives  (stage-natives stage)))
    (destructuring-bind (editor filter query) natives
      (window--place editor
                     (destructuring-bind (x y box-width box-height) (getf layout :composer)
                       (composer-editor-box workbench :x x :y y :width box-width :height box-height))
                     (not (or overlay drawer-p)))
      (window--place filter
                     (destructuring-bind (x y box-width box-height) (getf layout :sidebar)
                       (declare (ignore box-height))
                       (list (+ x 17) (+ y 60) (- box-width 34) 30))
                     (and (not overlay) (or drawer-p (getf layout :docked-p))))
      (let ((box (and overlay (overlay-input-box overlay width height))))
        (window--place query (or box (list 0 0 1 1)) (and box t)))))
  nil)

(-> window-focus-composer (workbench) null)
(defun window-focus-composer (workbench)
  "Give the composer the keyboard."
  (let ((editor (workbench-editor workbench)))
    (when editor
      (lightfast:take-focus editor)))
  nil)

(-> window-focus-filter (workbench) null)
(defun window-focus-filter (workbench)
  "Show the channel list and give its filter field the keyboard."
  (window-show-sidebar workbench t)
  (lightfast:take-focus (second (stage-natives (workbench-stage workbench))))
  nil)


;;;; -- Keys --

(-> window--shortcut (workbench key-event) boolean)
(defun window--shortcut (workbench event)
  "Handle a key that means the same wherever the keyboard is, and return whether it was used."
  (let ((key       (key-event-key event))
        (command-p (key-event-command-p event)))
    (cond
      ((and command-p (eql key #\k))
       (workbench-show-overlay workbench (if (workbench-overlay workbench) nil ':commands))
       t)
      ((and command-p (eql key #\,))
       (workbench-show-overlay workbench ':settings)
       t)
      ((and command-p (eql key #\s))
       (window-toggle-sidebar workbench)
       t)
      ((and command-p (eql key #\n))
       (workbench-new-channel workbench)
       t)
      ((and command-p (eql key #\o))
       (window-focus-filter workbench)
       t)
      ;; With the channel list over the whole window, the page keys turn its pages.
      ((and (member key '(:page-up :page-down)) (window-drawer-p workbench))
       (let ((page (* *sidebar-row-height*
                      (max 1 (1- (floor (- (stage-height (workbench-stage workbench))
                                           *sidebar-list-top*
                                           *sidebar-footer-height*)
                                        *sidebar-row-height*))))))
         (setf (workbench-sidebar-scroll workbench)
               (max 0 (+ (workbench-sidebar-scroll workbench) (if (eq key ':page-up) (- page) page)))))
       (workbench-redraw workbench)
       t)
      ((eq key ':page-up)
       (document-scroll-by (workbench-document workbench)
                           (- (max 80 (- (fourth (document--viewport (workbench-document workbench))) 60))))
       (workbench-redraw workbench)
       t)
      ((eq key ':page-down)
       (document-scroll-by (workbench-document workbench)
                           (max 80 (- (fourth (document--viewport (workbench-document workbench))) 60)))
       (workbench-redraw workbench)
       t)
      ((and command-p (eq key ':end))
       (document-scroll-to-end (workbench-document workbench))
       (workbench-redraw workbench)
       t)
      ((and command-p (eq key ':home))
       (setf (document-following-p (workbench-document workbench)) nil
             (document-scroll (workbench-document workbench))      0)
       (workbench-redraw workbench)
       t)
      (t
       nil))))

(-> window--approval-key (workbench key-event) boolean)
(defun window--approval-key (workbench event)
  "Answer the waiting approval when EVENT is the digit of one of its choices, and return whether it was."
  (let* ((approval (workbench-approval workbench))
         (key      (key-event-key event))
         (digit    (and (characterp key) (digit-char-p key)))
         (option   (and approval digit (plusp digit) (nth (1- digit) (getf approval :options)))))
    (when option
      (workbench-answer workbench (getf option :value))
      t)))

(-> window--editor-key (workbench key-event) boolean)
(defun window--editor-key (workbench event)
  "Decide what a key pressed in the composer does and return whether the composer should not see it."
  (let* ((key     (key-event-key event))
         (empty-p (zerop (length (string-trim '(#\Space #\Newline) (composer-text workbench))))))
    (cond
      ((completion-key workbench event)
       t)
      ;; The line editor's keys come before the window's, as in the terminal.
      ((composer-edit-key workbench event)
       t)
      ((window--shortcut workbench event)
       t)
      ;; The arrows recall what was sent, from an empty composer or while stepping through.
      ((and (eq key ':up) (or empty-p (workbench-history-index workbench))
            (composer-recall workbench 1))
       t)
      ((and (eq key ':down) (workbench-history-index workbench)
            (composer-recall workbench -1))
       t)
      ((and (member key '(:enter :keypad-enter))
            (not (key-event-shift-p event))
            (not (key-event-alt-p event)))
       (composer-send workbench)
       t)
      ((eq key ':tab)
       (when (workbench-working-p workbench)
         (composer-send workbench :steer-p nil))
       t)
      ((eq key ':escape)
       (cond
         ((workbench-working-p workbench)
          (workbench-stop workbench)))
       (workbench-redraw workbench)
       t)
      ((and empty-p (window--approval-key workbench event))
       t)
      ((and (key-event-command-p event) (eql key #\c)
            (document-selected-text (workbench-document workbench)))
       (lightfast:copy-text (document-selected-text (workbench-document workbench)))
       t)
      (t
       ;; Typing changes the composer's height; draw again once the key has landed.
       (workbench-redraw workbench)
       nil))))

(-> window--stage-key (workbench key-event) boolean)
(defun window--stage-key (workbench event)
  "Handle a key pressed while the stage, not a field, has the keyboard."
  (cond
    ((workbench-overlay workbench)
     (overlay-key workbench event))
    ((window--shortcut workbench event)
     t)
    ((eq (key-event-key event) ':escape)
     (cond
       ((window-drawer-p workbench)
        (window-toggle-sidebar workbench))
       ((workbench-working-p workbench)
        (workbench-stop workbench)))
     t)
    ;; With the channel list over the whole window there is no composer to type into.
    ((window-drawer-p workbench)
     (lightfast:take-focus (second (stage-natives (workbench-stage workbench))))
     nil)
    ((window--approval-key workbench event)
     t)
    ((and (key-event-command-p event) (eql (key-event-key event) #\c))
     (let ((text (document-selected-text (workbench-document workbench))))
       (when text
         (lightfast:copy-text text)))
     t)
    (t
     ;; Anything else is the start of a message: hand the keyboard to the composer.
     (window-focus-composer workbench)
     nil)))


;;;; -- Drops --

(defparameter *image-types* '("png" "jpg" "jpeg" "gif" "webp")
  "The file types the composer takes as image attachments.")

(-> window--drop (workbench string) null)
(defun window--drop (workbench dropped)
  "Take what was dropped on the window: image files become attachments, anything else goes into the composer as text."
  (let ((images nil)
        (others nil))
    (dolist (line (markdown-lines dropped))
      (let* ((trimmed (string-trim '(#\Space #\Return) line))
             ;; A dropped file arrives as a URL, its spaces and the like written as percent escapes.
             (path    (if (and (> (length trimmed) 7) (string= "file://" trimmed :end2 7))
                          (quri:url-decode (subseq trimmed 7) :lenient t)
                          trimmed)))
        (cond
          ((zerop (length trimmed))
           nil)
          ((and (member (pathname-type path) *image-types* :test #'string-equal)
                (probe-file path))
           (push path images))
          (t
           (push trimmed others)))))
    (when images
      (setf (workbench-attachments workbench)
            (remove-duplicates (append (workbench-attachments workbench) (nreverse images))
                               :test #'string=)))
    (when others
      (setf (composer-text workbench)
            (format nil "~A~{~A~^ ~}" (composer-text workbench) (nreverse others))))
    (workbench-redraw workbench))
  nil)


;;;; -- Construction --

(-> window--make-natives (workbench t) list)
(defun window--make-natives (workbench window)
  "Create the composer's editor, the channel filter and the overlay's query field in WINDOW, and return them in that order."
  (let ((editor (lightfast:make-text-editor :parent window :x 0 :y 0 :width 200 :height 30))
        (filter (lightfast:make-input :parent window :x 0 :y 0 :width 200 :height 30))
        (query  (lightfast:make-input :parent window :x 0 :y 0 :width 200 :height 30)))
    (lightfast:set-box editor lightfast:+box-flat-box+)
    (lightfast:set-text-font editor (theme-font ':serif))
    (lightfast:set-text-size editor 19)
    (lightfast:text-set-wrap editor ':bounds)
    (lightfast:text-set-scrollbars editor ':none)
    (lightfast:set-placeholder editor "Write to the agent" :font (theme-font ':serif-italic))
    (lightfast:on editor
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (when (with-stage-guard ("A key in the composer")
                            (window--editor-key workbench (parse-key-event value)))
                      (lightfast:consume-event)))
                  :event lightfast:+event-key+)
    (lightfast:on editor
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (workbench-redraw workbench))
                  :event lightfast:+event-change+)
    (dolist (field (list filter query))
      (lightfast:set-box field lightfast:+box-flat-box+)
      (lightfast:set-text-font field (theme-font ':serif))
      (lightfast:set-text-size field 17)
      (lightfast:set-when field ':changed))
    (lightfast:set-placeholder filter "Find a channel" :font (theme-font ':serif-italic))
    (lightfast:set-placeholder query "Type to search" :font (theme-font ':serif-italic))
    (lightfast:on filter
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (setf (workbench-filter workbench)         (lightfast:value filter)
                          (workbench-sidebar-scroll workbench) 0)
                    (workbench-redraw workbench))
                  :event lightfast:+event-change+)
    (lightfast:on filter
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (with-stage-guard ("A key in the channel filter")
                      (let* ((key-event (parse-key-event value))
                             (key       (key-event-key key-event)))
                        (cond
                          ;; Escape empties the filter; from an empty one it leaves the list.
                          ((eq key ':escape)
                           (if (plusp (length (workbench-filter workbench)))
                               (setf (lightfast:value filter)     ""
                                     (workbench-filter workbench) "")
                               (sidebar--close-drawer workbench))
                           (window-arrange workbench)
                           (unless (window-drawer-p workbench)
                             (window-focus-composer workbench))
                           (workbench-redraw workbench)
                           (lightfast:consume-event))
                          ((member key '(:enter :keypad-enter))
                           (let ((first (find ':channel (sidebar--visible-entries workbench)
                                              :key (lambda (entry) (getf entry :kind)))))
                             (when first
                               (setf (lightfast:value filter)     ""
                                     (workbench-filter workbench) "")
                               (sidebar--open workbench (getf first :id))))
                           (lightfast:consume-event))
                          ((window--shortcut workbench key-event)
                           (lightfast:consume-event))))))
                  :event lightfast:+event-key+)
    (lightfast:on query
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (let ((overlay (workbench-overlay workbench)))
                      (when overlay
                        (setf (overlay-query overlay)     (lightfast:value query)
                              (overlay-selection overlay) 0
                              (overlay-scroll overlay)    0)
                        (workbench-redraw workbench))))
                  :event lightfast:+event-change+)
    (lightfast:on query
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (when (and (workbench-overlay workbench)
                               (with-stage-guard ("A key in the list")
                                 (overlay-key workbench (parse-key-event value))))
                      (lightfast:consume-event)))
                  :event lightfast:+event-key+)
    (list editor filter query)))

(-> window--make-system-menu (workbench t) null)
(defun window--make-system-menu (workbench window)
  "Give the application its menus in the system's menu bar. Only a Mac has one; elsewhere the keys alone do the work."
  (let ((menu (lightfast:make-menu-bar :parent window :x 0 :y 0 :width 1 :height 1 :system t)))
    (if (lightfast:menu-bar-system-p menu)
        (flet ((item (path shortcut function)
                 (lightfast:add-menu-item menu path
                                          (lambda (widget value)
                                            (declare (ignore widget value))
                                            (with-stage-guard ("A menu command")
                                              (funcall function)))
                                          :shortcut shortcut))
               (command (character)
                 (logior lightfast:+modifier-command+ (char-code character))))
          (item "Channel/New channel" (command #\n)
                (lambda () (workbench-new-channel workbench)))
          (item "Channel/New channel in another folder…" 0
                (lambda () (workbench-new-channel-elsewhere workbench)))
          (item "Channel/Find a channel…" (command #\o)
                (lambda () (window-focus-filter workbench)))
          (item "Channel/Stop the agent" 0
                (lambda () (workbench-stop workbench)))
          (item "View/Channels" (command #\s)
                (lambda () (window-toggle-sidebar workbench)))
          (item "View/Commands" (command #\k)
                (lambda () (workbench-show-overlay workbench ':commands)))
          (item "View/Settings…" (command #\,)
                (lambda () (workbench-show-overlay workbench ':settings)))
          (item "View/Newest output" 0
                (lambda ()
                  (document-scroll-to-end (workbench-document workbench))
                  (workbench-redraw workbench))))
        (lightfast:hide menu)))
  nil)

(-> window-build (workbench) null)
(defun window-build (workbench)
  "Create WORKBENCH's window, its stage and its native fields."
  (let* ((launch (workbench-launch workbench))
         (size   (or (launch-size launch) *window-default-size*))
         (window (lightfast:make-window :width  (first size)
                                        :height (rest size)
                                        :label  "Autolith"
                                        :app-id "aui"))
         (stage  (make-stage window
                             :painter      (lambda (stage) (window--paint workbench stage))
                             :key-handler  (lambda (event) (window--stage-key workbench event))
                             :drop-handler (lambda (dropped) (window--drop workbench dropped)))))
    (setf (workbench-window workbench) window
          (workbench-stage workbench)  stage
          *link-handler*               (lambda (link)
                                         (workbench-open-link workbench link))
          *entry-channel-opener*       (lambda (identifier)
                                         (workbench-open workbench :channel identifier)))
    ;; A size given on the command line is pinned, so a tiling compositor
    ;; floats the window at it and other layouts can be looked at.
    (if (launch-size launch)
        (lightfast:set-size-range window
                                  :min-width  (first size)
                                  :min-height (rest size)
                                  :max-width  (first size)
                                  :max-height (rest size))
        (lightfast:set-size-range window
                                  :min-width  (first *window-minimum-size*)
                                  :min-height (rest *window-minimum-size*)))
    (lightfast:window-escape-closes window nil)
    (when (uiop:os-macosx-p)
      (with-stage-guard ("Making the menus")
        (window--make-system-menu workbench window)))
    (let ((natives (window--make-natives workbench window)))
      (setf (stage-natives stage)       natives
            (workbench-editor workbench) (first natives)))
    (lightfast:on-resize window
                         (lambda (resized)
                           (with-stage-guard ("Resizing the window")
                             (lightfast:refresh-geometry resized)
                             (stage-resize stage (lightfast:widget-width resized) (lightfast:widget-height resized))
                             ;; A list opened over a narrow window does not come
                             ;; back when the window is made narrow again.
                             (unless (window--narrow-p workbench)
                               (setf (workbench-drawer-open-p workbench) nil))
                             (window-arrange workbench))))
    (lightfast:on window
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (with-stage-guard ("Closing the window")
                      (workbench-close workbench)))
                  :event lightfast:+event-close+))
  nil)

(-> window-show (workbench) null)
(defun window-show (workbench)
  "Show WORKBENCH's window at the size the window system gives it and hand the keyboard to the composer.

A window that is only to be photographed at a given size is never shown: it
is drawn offscreen at exactly that size, whatever the window system would
have made of it."
  (let* ((window   (workbench-window workbench))
         (stage    (workbench-stage workbench))
         (launch   (workbench-launch workbench))
         (unseen-p (and (launch-shot launch) (launch-size launch))))
    (unless unseen-p
      (lightfast:show window)
      (lightfast:wait 0.05))
    (lightfast:refresh-geometry window)
    (stage-resize stage (lightfast:widget-width window) (lightfast:widget-height window))
    (window-arrange workbench)
    (if (window-drawer-p workbench)
        (lightfast:take-focus (second (stage-natives stage)))
        (window-focus-composer workbench)))
  nil)
