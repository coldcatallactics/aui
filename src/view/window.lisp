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

(defvar *window-natives* nil
  "The native fields of the window being built: a property list of :EDITOR, :FILTER and :QUERY.")

(-> window--layout (workbench integer integer) list)
(defun window--layout (workbench width height)
  "Return where each pane goes in a window WIDTH by HEIGHT, as a property list of (X Y WIDTH HEIGHT) boxes.

:DOCKED-P and :DRAWER-P say how the channel list is shown."
  (let* ((shown-p    (workbench-sidebar-shown-p workbench))
         (docked-p   (and shown-p (>= width *sidebar-docked-minimum*)))
         (drawer-p   (and shown-p (not docked-p)))
         (side-width (if docked-p *sidebar-width* 0))
         (main-x     side-width)
         (main-width (- width side-width))
         (column     (max 200 (min (+ *document-measure* 24) (- main-width 40))))
         (column-x   (+ main-x (floor (- main-width column) 2)))
         (status-y   (- height *status-height* 6))
         (composer-h (composer-height workbench column))
         (composer-y (- status-y composer-h 2))
         (notice-h   (if (workbench-notice workbench) 26 0))
         (pending-h  (pending-height workbench))
         (approval-h (approval-height workbench column))
         (stack-top  (- composer-y 10 notice-h pending-h approval-h)))
    (list :docked-p   docked-p
          :drawer-p   drawer-p
          :sidebar    (list 0 0 (min *sidebar-width* (- width 40)) height)
          :header     (list main-x 0 main-width *header-height*)
          :document   (list main-x *header-height* main-width (max 40 (- stack-top *header-height*)))
          :completion (let ((height (completion-height workbench)))
                        (list column-x (- composer-y height 4) column height))
          :approval   (list column-x stack-top column approval-h)
          :pending    (list column-x (+ stack-top approval-h) column pending-h)
          :notice     (list column-x (+ stack-top approval-h pending-h) column notice-h)
          :composer   (list column-x composer-y column composer-h)
          :status     (list (+ column-x 2) status-y (- column 4) *status-height*))))


;;;; -- Painting --

(-> window--paint (workbench stage) null)
(defun window--paint (workbench stage)
  "Draw the whole window: the panes at their places, then the drawer and the overlay over them."
  (let ((layout (window--layout workbench (stage-width stage) (stage-height stage))))
    (flet ((box (name)
             (getf layout name)))
      (when (getf layout :docked-p)
        (destructuring-bind (x y width height) (box :sidebar)
          (sidebar-paint workbench stage :x x :y y :width width :height height)))
      (destructuring-bind (x y width height) (box :document)
        (if (and (workbench-attached-p workbench)
                 (plusp (transcript-length (workbench-transcript workbench))))
            (document-paint (workbench-document workbench) stage :x x :y y :width width :height height)
            (empty-paint workbench stage :x x :y y :width width :height height)))
      (destructuring-bind (x y width height) (box :header)
        (declare (ignore height))
        (header-paint workbench stage :x x :y y :width width
                                      :sidebar-toggle-p (not (getf layout :docked-p))))
      (when (workbench-approval workbench)
        (destructuring-bind (x y width height) (box :approval)
          (approval-paint workbench stage :x x :y y :width width :height height)))
      (destructuring-bind (x y width height) (box :pending)
        (declare (ignore height))
        (pending-paint workbench :x x :y y :width width))
      (when (workbench-notice workbench)
        (destructuring-bind (x y width height) (box :notice)
          (paint-mark ':cross x (+ y 5) :size 14 :color *red*)
          (paint-text-in (workbench-notice workbench) (text-style ':meta)
                         :x      (+ x 20)
                         :y      y
                         :width  (- width 20)
                         :height height)))
      (destructuring-bind (x y width height) (box :composer)
        (composer-paint workbench stage :x x :y y :width width :height height)
        ;; With the editor put away behind a drawer or an overlay, its text is drawn in its place.
        (when (or (getf layout :drawer-p) (workbench-overlay workbench))
          (destructuring-bind (edit-x edit-y edit-width edit-height)
              (composer-editor-box workbench :x x :y y :width width :height height)
            (let ((draft (make-paragraph (list (make-span (composer-text workbench) nil))
                                         :style (text-style ':body))))
              (paragraph-layout draft (- edit-width 8))
              (lightfast:with-clip (edit-x edit-y edit-width edit-height)
                (paragraph-paint draft (+ edit-x 4) edit-y))))))
      (destructuring-bind (x y width height) (box :status)
        (declare (ignore height))
        (status-paint workbench stage :x x :y y :width width))
      (destructuring-bind (x y width height) (box :completion)
        (when (plusp height)
          (completion-paint workbench stage :x x :y y :width width)))
      (when (getf layout :drawer-p)
        (destructuring-bind (x y width height) (box :sidebar)
          (paint-dither (+ x width) 0 :width (- (stage-width stage) width) :height (stage-height stage) :step 3)
          (stage-region stage ':drawer-ground
                        :x      (+ x width)
                        :y      0
                        :width  (- (stage-width stage) width)
                        :height (stage-height stage)
                        :action (lambda ()
                                  (setf (workbench-sidebar-shown-p workbench) nil)
                                  (workbench-redraw workbench))
                        :cursor ':default)
          (sidebar-paint workbench stage :x x :y y :width width :height height)))
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
                       (list (+ x 17) (+ y 104) (- box-width 34) 30))
                     (and (not overlay) (or drawer-p (getf layout :docked-p))))
      (window--place query (overlay-input-box width height) (and overlay t))))
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
  (setf (workbench-sidebar-shown-p workbench) t)
  (window-arrange workbench)
  (lightfast:take-focus (second (stage-natives (workbench-stage workbench))))
  (workbench-redraw workbench)
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
      ((and command-p (eql key #\t))
       (setf (workbench-sidebar-shown-p workbench) (not (workbench-sidebar-shown-p workbench)))
       (workbench-redraw workbench)
       t)
      ((and command-p (eql key #\n))
       (workbench-new-channel workbench)
       t)
      ((and command-p (eql key #\o))
       (window-focus-filter workbench)
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
         ((and (workbench-sidebar-shown-p workbench)
               (< (stage-width (workbench-stage workbench)) *sidebar-docked-minimum*))
          (setf (workbench-sidebar-shown-p workbench) nil))
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
     (when (workbench-working-p workbench)
       (workbench-stop workbench))
     t)
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
                    (let* ((key-event (parse-key-event value))
                           (key       (key-event-key key-event)))
                      (cond
                        ((eq key ':escape)
                         (setf (lightfast:value filter)     ""
                               (workbench-filter workbench) "")
                         (window-focus-composer workbench)
                         (workbench-redraw workbench)
                         (lightfast:consume-event))
                        ((member key '(:enter :keypad-enter))
                         (let ((first (find ':channel (sidebar--visible-entries workbench)
                                            :key (lambda (entry) (getf entry :kind)))))
                           (when first
                             (workbench-open workbench :channel (getf first :id))
                             (setf (lightfast:value filter)     ""
                                   (workbench-filter workbench) "")
                             (window-focus-composer workbench)))
                         (lightfast:consume-event))
                        ((window--shortcut workbench key-event)
                         (lightfast:consume-event)))))
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
                             :painter     (lambda (stage) (window--paint workbench stage))
                             :key-handler (lambda (event) (window--stage-key workbench event)))))
    (setf (workbench-window workbench) window
          (workbench-stage workbench)  stage)
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
    (let ((natives (window--make-natives workbench window)))
      (setf (stage-natives stage)       natives
            (workbench-editor workbench) (first natives)))
    (lightfast:on-resize window
                         (lambda (resized)
                           (lightfast:refresh-geometry resized)
                           (stage-resize stage (lightfast:widget-width resized) (lightfast:widget-height resized))
                           (window-arrange workbench)))
    (lightfast:on window
                  (lambda (widget event value)
                    (declare (ignore widget event value))
                    (workbench-close workbench))
                  :event lightfast:+event-close+)
    ;; The drawer starts closed on a window too narrow to dock it.
    (when (< (first size) *sidebar-docked-minimum*)
      (setf (workbench-sidebar-shown-p workbench) nil)))
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
    (when (< (stage-width stage) *sidebar-docked-minimum*)
      (setf (workbench-sidebar-shown-p workbench) nil))
    (window-arrange workbench)
    (window-focus-composer workbench))
  nil)
