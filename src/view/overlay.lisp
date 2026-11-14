(in-package #:aui)

;;;; -- Overlays --

;;; Everything laid over the window, all of it drawn here in the window's own
;;; manner: the list of commands and the settings, a picker for a choice among
;;; many, a prompt for one line of text, a panel that shows something the
;;; channel knows, and the small menus that open at a chip or a button. A
;;; picker or a prompt opened from the settings returns to them; one overlay
;;; is drawn at a time, so the ground is dimmed once.

(defstruct choice
  "One thing a menu or a picker offers, with the LABEL it goes by, the DETAIL said about it, the VALUE handed on when it is chosen, and whether it is the CURRENT-P one. A choice with no value is a divider."
  (label     ""  :type string)
  (detail    nil :type (or null string))
  (value     nil)
  (current-p nil :type boolean))

(defclass overlay ()
  ((kind
    :initarg       :kind
    :reader        overlay-kind
    :type          (member :commands :settings :picker :menu :prompt :panel :board)
    :documentation "Which view this is.")
   (title
    :initarg       :title
    :initform      ""
    :reader        overlay-title
    :type          string
    :documentation "The words in the window's title bar.")
   (query
    :initarg       :query
    :initform      ""
    :accessor      overlay-query
    :type          string
    :documentation "What the person typed: the words that narrow a list, or a prompt's answer.")
   (selection
    :initform      0
    :accessor      overlay-selection
    :type          integer
    :documentation "The index of the highlighted row among those shown.")
   (scroll
    :initform      0
    :accessor      overlay-scroll
    :type          integer
    :documentation "How far the content is scrolled: in rows for a list, in units for a panel.")
   (choices
    :initarg       :choices
    :initform      nil
    :reader        overlay-choices
    :type          list
    :documentation "What a picker or a menu offers.")
   (action
    :initarg       :action
    :initform      nil
    :reader        overlay-action
    :type          (or null function)
    :documentation "Called with the chosen value of a picker or a menu, or with a prompt's answer.")
   (note
    :initarg       :note
    :initform      nil
    :reader        overlay-note
    :type          (or null string)
    :documentation "What a prompt asks for, in a sentence, or NIL.")
   (content
    :initarg       :content
    :initform      nil
    :accessor      overlay-content
    :documentation "The block a panel shows, or NIL while it is still being fetched.")
   (tabs
    :initarg       :tabs
    :initform      nil
    :reader        overlay-tabs
    :type          list
    :documentation "The tabs of a board, each (LABEL COUNT BLOCK): its name, how many things it holds, and what it shows.")
   (anchor
    :initarg       :anchor
    :initform      nil
    :reader        overlay-anchor
    :type          list
    :documentation "Where a menu opens, as (X Y ABOVE-P): its corner, and whether it grows upward from there.")
   (parent
    :initarg       :parent
    :initform      nil
    :reader        overlay-parent
    :documentation "The overlay to return to when this one is put away, or NIL."))
  (:documentation "A view laid over the window."))

(defparameter *overlay-row-height* 46
  "The height of one row in the list of commands and in the settings.")

(defparameter *choice-row-height* 30
  "The height of one row of a picker or a menu.")

(defparameter *picker-filter-minimum* 9
  "How many choices a picker has before it offers a field to narrow them.")


;;;; -- Opening and Putting Away --

(-> overlay--field (workbench) t)
(defun overlay--field (workbench)
  "Return the native field an overlay's text is typed into, or NIL before the window has one."
  (let ((stage (workbench-stage workbench)))
    (and stage (third (stage-natives stage)))))

(-> overlay--focus (workbench) null)
(defun overlay--focus (workbench)
  "Give the keyboard to the overlay's field, to the stage when it has none, or back to the composer when no overlay is shown."
  (let ((stage   (workbench-stage workbench))
        (overlay (workbench-overlay workbench)))
    (when (and stage (stage-natives stage))
      (window-arrange workbench)
      (cond
        ((null overlay)
         (window-focus-composer workbench))
        ((overlay-input-box overlay (stage-width stage) (stage-height stage))
         (lightfast:take-focus (overlay--field workbench)))
        ((stage-canvas stage)
         (lightfast:take-focus (stage-canvas stage))))))
  nil)

(-> overlay-open (workbench overlay) null)
(defun overlay-open (workbench overlay)
  "Lay OVERLAY over WORKBENCH's window and give it the keyboard."
  (setf (workbench-overlay workbench) overlay)
  (let ((field (overlay--field workbench)))
    (when field
      (setf (lightfast:value field) (overlay-query overlay))))
  (when (eq (overlay-kind overlay) ':settings)
    (workbench-refresh workbench "_autolith/settings"))
  (overlay--focus workbench)
  (workbench-redraw workbench)
  nil)

(-> workbench-show-overlay (workbench (or null keyword)) null)
(defun workbench-show-overlay (workbench kind)
  "Lay the commands or the settings, as KIND says, over WORKBENCH's window, or put every overlay away when KIND is NIL."
  (if kind
      (overlay-open workbench
                    (make-instance 'overlay
                                   :kind  kind
                                   :title (ecase kind
                                            (:commands
                                             "Commands")
                                            (:settings
                                             "Settings"))))
      (progn
        (setf (workbench-overlay workbench) nil)
        (overlay--focus workbench)
        (workbench-redraw workbench)))
  nil)

(-> overlay-dismiss (workbench) null)
(defun overlay-dismiss (workbench)
  "Put the overlay shown away, returning to the one it was opened from when there is one."
  (let* ((overlay (workbench-overlay workbench))
         (parent  (and overlay (overlay-parent overlay))))
    (if parent
        (overlay-open workbench parent)
        (workbench-show-overlay workbench nil)))
  nil)

(-> overlay--returning (workbench) (or null overlay))
(defun overlay--returning (workbench)
  "Return the overlay a picker or a prompt opened now should return to: the settings, when they are what is shown."
  (let ((overlay (workbench-overlay workbench)))
    (and overlay (eq (overlay-kind overlay) ':settings) overlay)))

(-> workbench-pick (workbench list function &key (:title string)) null)
(defun workbench-pick (workbench choices action &key (title "Choose"))
  "Offer CHOICES in a window titled TITLE and call ACTION with the value of the one chosen."
  (let ((picker (make-instance 'overlay
                               :kind    ':picker
                               :title   title
                               :choices choices
                               :action  action
                               :parent  (overlay--returning workbench))))
    ;; The list opens on the choice in force.
    (setf (overlay-selection picker) (or (position-if #'choice-current-p choices) 0))
    (overlay-open workbench picker))
  nil)

(-> workbench-menu
    (workbench list &key (:x integer) (:y integer) (:above-p boolean) (:action function)) null)
(defun workbench-menu (workbench choices &key (x 0) (y 0) above-p (action #'funcall))
  "Open a menu of CHOICES with its corner at X, Y, growing upward when ABOVE-P.

ACTION is called with the value of the choice taken; without one each
choice's value is a function, and that is called."
  (let ((menu (make-instance 'overlay
                             :kind    ':menu
                             :choices choices
                             :action  action
                             :anchor  (list x y above-p))))
    ;; Nothing is highlighted until the pointer or a key says what.
    (setf (overlay-selection menu) -1)
    (overlay-open workbench menu))
  nil)

(-> workbench-ask (workbench string function &key (:note (or null string)) (:initial string)) null)
(defun workbench-ask (workbench title action &key note (initial ""))
  "Ask for one line of text in a window titled TITLE, starting from INITIAL, and call ACTION with what was entered.

NOTE says in a sentence what is asked for."
  (overlay-open workbench
                (make-instance 'overlay
                               :kind   ':prompt
                               :title  title
                               :note   note
                               :query  initial
                               :action action
                               :parent (overlay--returning workbench)))
  nil)

(-> workbench-show-panel (workbench string list) null)
(defun workbench-show-panel (workbench title blocks)
  "Show BLOCKS in a window titled TITLE; an empty list of BLOCKS says the content is still on its way."
  (overlay-open workbench
                (make-instance 'overlay
                               :kind    ':panel
                               :title   title
                               :content (and blocks
                                             (make-instance 'box-block
                                                            :children  blocks
                                                            :frame-p   nil
                                                            :padding-x 0
                                                            :padding-y 0))))
  nil)

(-> workbench-show-board (workbench string list) null)
(defun workbench-show-board (workbench title tabs)
  "Show a board titled TITLE: TABS down its left side, each (LABEL COUNT . BLOCKS), and the blocks of the chosen one beside them."
  (overlay-open workbench
                (make-instance 'overlay
                               :kind  ':board
                               :title title
                               :tabs  (loop for (label count . blocks) in tabs
                                            collect (list label
                                                          count
                                                          (make-instance 'box-block
                                                                         :children  blocks
                                                                         :frame-p   nil
                                                                         :padding-x 0
                                                                         :padding-y 0)))))
  nil)

(-> overlay-choose (workbench choice) null)
(defun overlay-choose (workbench choice)
  "Take CHOICE from the picker or menu shown: put it away, then do what the choice stands for."
  (let ((action (overlay-action (workbench-overlay workbench))))
    (when (choice-value choice)
      (overlay-dismiss workbench)
      (when action
        (funcall action (choice-value choice)))))
  nil)

(-> overlay-accept (workbench) null)
(defun overlay-accept (workbench)
  "Take the prompt's answer: put the prompt away, then hand the answer on."
  (let* ((overlay (workbench-overlay workbench))
         (action  (overlay-action overlay))
         (answer  (overlay-query overlay)))
    (overlay-dismiss workbench)
    (when action
      (funcall action answer)))
  nil)


;;;; -- Where an Overlay Sits --

(-> overlay-shown-choices (overlay) list)
(defun overlay-shown-choices (overlay)
  "Return the choices of OVERLAY that its query leaves: all of a menu's, and of a picker's those whose label or detail holds what was typed."
  (let ((query (overlay-query overlay)))
    (if (or (eq (overlay-kind overlay) ':menu) (zerop (length query)))
        (overlay-choices overlay)
        (remove-if-not (lambda (choice)
                         (or (search query (choice-label choice) :test #'char-equal)
                             (and (choice-detail choice)
                                  (search query (choice-detail choice) :test #'char-equal))))
                       (overlay-choices overlay)))))

(-> overlay--filter-p (overlay) boolean)
(defun overlay--filter-p (overlay)
  "Return T when OVERLAY is a picker with enough choices to be worth narrowing."
  (and (eq (overlay-kind overlay) ':picker)
       (>= (length (overlay-choices overlay)) *picker-filter-minimum*)))

(-> overlay--note-height (overlay integer) integer)
(defun overlay--note-height (overlay width)
  "Return the height OVERLAY's note takes when set WIDTH wide, zero when it has none."
  (let ((note (overlay-note overlay)))
    (if (and note (plusp (length note)))
        (paragraph-layout (make-paragraph (list (make-span note nil)) :style (text-style ':small))
                          (max 40 width))
        0)))

(-> overlay--label-width (overlay) integer)
(defun overlay--label-width (overlay)
  "Return the width of the widest label among OVERLAY's choices."
  (let ((style (text-style ':label-bold)))
    (loop for choice in (overlay-choices overlay)
          maximize (text-width (choice-label choice) style))))

(-> overlay--menu-size (overlay) (values integer integer))
(defun overlay--menu-size (overlay)
  "Return the width and height of OVERLAY's menu: as wide as its widest label and detail, a divider half a row high."
  (let* ((choices (overlay-choices overlay))
         (detail  (loop for choice in choices
                        maximize (if (choice-detail choice)
                                     (text-width (choice-detail choice) (text-style ':meta))
                                     0))))
    (values (+ 30 (overlay--label-width overlay) (if (plusp detail) (+ 20 detail) 0) 26)
            (+ 8 (loop for choice in choices
                       sum (if (choice-value choice)
                               *choice-row-height*
                               (floor *choice-row-height* 3)))))))

(-> overlay-box (overlay integer integer) list)
(defun overlay-box (overlay stage-width stage-height)
  "Return the (X Y WIDTH HEIGHT) OVERLAY's window takes on a stage of the given size."
  (flet ((centered (width height)
           (list (floor (- stage-width width) 2) (max 32 (floor (- stage-height height) 3)) width height)))
    (ecase (overlay-kind overlay)
      ((:commands :settings)
       (centered (min 760 (- stage-width 48)) (min 640 (- stage-height 96))))
      (:panel
       (centered (min 900 (- stage-width 48)) (min 760 (- stage-height 64))))
      (:board
       (centered (min 1120 (- stage-width 48)) (min 820 (- stage-height 64))))
      (:picker
       (let* ((width (min 620 (- stage-width 48)))
              (top   (+ 28 (if (overlay--filter-p overlay) 56 8)))
              (rows  (max 1 (min (length (overlay-choices overlay))
                                 (floor (- stage-height 96 top 8) *choice-row-height*)
                                 16))))
         (centered width (+ top (* rows *choice-row-height*) 8))))
      (:prompt
       (let ((width (min 560 (- stage-width 48))))
         (centered width
                   (+ 28 14
                      (let ((note (overlay--note-height overlay (- width 32))))
                        (if (plusp note) (+ note 10) 0))
                      36 14 32 14))))
      (:menu
       (multiple-value-bind (width height) (overlay--menu-size overlay)
         (destructuring-bind (x y above-p) (overlay-anchor overlay)
           (let ((width (min width (- stage-width 16))))
             (list (max 8 (min x (- stage-width width 8)))
                   (max 8 (min (if above-p (- y height) y) (- stage-height height 8)))
                   width
                   height))))))))

(-> overlay-input-box (overlay integer integer) list)
(defun overlay-input-box (overlay stage-width stage-height)
  "Return the (X Y WIDTH HEIGHT) of OVERLAY's field, or NIL when it has none."
  (destructuring-bind (x y width height) (overlay-box overlay stage-width stage-height)
    (declare (ignore height))
    (case (overlay-kind overlay)
      ((:commands :settings)
       (list (+ x 17) (+ y 28 13) (- width 34) 30))
      (:picker
       (and (overlay--filter-p overlay)
            (list (+ x 17) (+ y 28 13) (- width 34) 30)))
      (:prompt
       (let ((note (overlay--note-height overlay (- width 32))))
         (list (+ x 17) (+ y 28 14 (if (plusp note) (+ note 10) 0) 3) (- width 34) 30)))
      (otherwise
       nil))))


;;;; -- Drawing --

(-> overlay--paint-layer (workbench stage overlay) null)
(defun overlay--paint-layer (workbench stage overlay)
  "Draw OVERLAY: the dimmed ground under it, its window, and what its kind shows in it."
  (destructuring-bind (x y width height) (overlay-box overlay (stage-width stage) (stage-height stage))
    (let* ((kind   (overlay-kind overlay))
           (menu-p (eq kind ':menu))
           (top-p  (eq overlay (workbench-overlay workbench)))
           (field  (overlay-input-box overlay (stage-width stage) (stage-height stage))))
      ;; A menu leaves the window readable around it; everything else dims
      ;; what lies beneath, an overlay it was opened from included.
      (unless menu-p
        (paint-dither 0 0 :width (stage-width stage) :height (stage-height stage) :step 3))
      (stage-region stage (list ':overlay-ground kind)
                    :x      0
                    :y      0
                    :width  (stage-width stage)
                    :height (stage-height stage)
                    :action (lambda () (overlay-dismiss workbench))
                    :cursor ':default)
      (let ((bar (paint-window-frame x y
                                     :width  width
                                     :height height
                                     :title  (and (not menu-p) (overlay-title overlay)))))
        (stage-region stage (list ':overlay-window kind) :x x :y y :width width :height height :cursor ':default)
        (unless menu-p
          (paint-button stage (list ':overlay-close kind) ""
                        :x       (- (+ x width) 26)
                        :y       (+ y 4)
                        :width   20
                        :height  20
                        :mark    ':cross
                        :action  (lambda () (overlay-dismiss workbench))
                        :tooltip "Close (Esc)"))
        (when (and field (not (eq kind ':prompt)))
          (paint-frame (+ x 12) (+ y bar 10) :width (- width 24) :height 36)
          ;; The field itself is with the overlay on top; beneath it, what was typed is drawn.
          (unless top-p
            (let ((query (overlay-query overlay)))
              (paint-text-in (if (plusp (length query)) query "Type to search")
                             (text-style ':list :face (if (plusp (length query)) ':serif ':serif-italic))
                             :x      (+ x 20)
                             :y      (+ y bar 10)
                             :width  (- width 40)
                             :height 36))))
        (let* ((top    (+ y bar (if (and field (not (eq kind ':prompt))) 56 0)))
               (inside (- (+ y height) top 1)))
          (ecase kind
            (:commands
             (catalog-paint-commands workbench stage overlay
                                     :x (1+ x) :y top :width (- width 2) :height inside))
            (:settings
             (catalog-paint-settings workbench stage overlay
                                     :x (1+ x) :y top :width (- width 2) :height inside))
            ((:picker :menu)
             (sheet-paint-choices workbench stage overlay
                                  :x      (1+ x)
                                  :y      (+ top 4)
                                  :width  (- width 2)
                                  :height (- inside 8)))
            (:prompt
             (sheet-paint-prompt workbench stage overlay
                                 :x x :y (+ y bar) :width width :height (- height bar)))
            (:panel
             (sheet-paint-panel workbench stage overlay
                                :x (1+ x) :y top :width (- width 2) :height inside))
            (:board
             (sheet-paint-board workbench stage overlay
                                :x (1+ x) :y top :width (- width 2) :height inside)))))))
  nil)

(-> overlay-paint (workbench stage) null)
(defun overlay-paint (workbench stage)
  "Draw the overlay shown over everything else, on top of the overlays it was opened from, each dimmed by the one above it."
  (let ((layers nil))
    (loop for overlay = (workbench-overlay workbench) then (overlay-parent overlay)
          while overlay
          do (push overlay layers))
    (dolist (overlay layers)
      (overlay--paint-layer workbench stage overlay)))
  nil)


;;;; -- Keys --

(-> overlay--move (overlay integer) null)
(defun overlay--move (overlay step)
  "Move OVERLAY's highlight STEP rows among its choices, passing over dividers and stopping at either end."
  (let* ((choices (overlay-shown-choices overlay))
         (count   (length choices))
         (index   (overlay-selection overlay)))
    (loop
      (let ((next (+ index step)))
        (when (or (minusp next) (>= next count))
          (return))
        (setf index next)
        (when (choice-value (nth index choices))
          (setf (overlay-selection overlay) index)
          (return)))))
  nil)

(-> overlay-key (workbench key-event) boolean)
(defun overlay-key (workbench event)
  "Handle a key while an overlay is shown and return whether it was used."
  (let* ((overlay (workbench-overlay workbench))
         (kind    (overlay-kind overlay))
         (key     (key-event-key event)))
    (case key
      (:escape
       (overlay-dismiss workbench)
       t)
      ((:down :up)
       (let ((step (if (eq key ':down) 1 -1)))
         (ecase kind
           (:commands
            (setf (overlay-selection overlay) (max 0 (+ (overlay-selection overlay) step))))
           (:settings
            (setf (overlay-scroll overlay) (max 0 (+ (overlay-scroll overlay) step))))
           ((:picker :menu)
            (overlay--move overlay step))
           (:panel
            (setf (overlay-scroll overlay) (max 0 (+ (overlay-scroll overlay) (* step 40)))))
           (:board
            (setf (overlay-selection overlay)
                  (max 0 (min (1- (length (overlay-tabs overlay))) (+ (overlay-selection overlay) step)))
                  (overlay-scroll overlay)    0))
           (:prompt
            nil)))
       (workbench-redraw workbench)
       t)
      ((:page-down :page-up)
       (when (member kind '(:panel :board))
         (setf (overlay-scroll overlay)
               (max 0 (+ (overlay-scroll overlay) (if (eq key ':page-down) 400 -400))))
         (workbench-redraw workbench))
       t)
      ((:enter :keypad-enter)
       (ecase kind
         (:commands
          (catalog-run-selected workbench))
         ((:picker :menu)
          (let ((choice (and (>= (overlay-selection overlay) 0)
                             (nth (overlay-selection overlay) (overlay-shown-choices overlay)))))
            (when choice
              (overlay-choose workbench choice))))
         (:prompt
          (overlay-accept workbench))
         ((:settings :panel :board)
          nil))
       t)
      (otherwise
       nil))))
