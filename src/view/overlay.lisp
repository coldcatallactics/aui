(in-package #:aui)

;;;; -- Overlays --

;;; The views drawn over the window. They are the list of commands, the
;;; settings, pickers for one choice among many, prompts for one line of text,
;;; panels with information about the channel, and the small menus that open at
;;; a chip or a button. A picker or a prompt opened from the settings returns to
;;; the settings.

(defstruct choice
  "One entry of a menu or a picker. LABEL is the name of the entry, and DETAIL is a short description. VALUE is passed on if the entry is chosen. CURRENT-P is true for the entry in force. Choices without a value are dividers."
  (label     ""  :type string)
  (detail    nil :type (or null string))
  (value     nil)
  (current-p nil :type boolean))

(defclass overlay ()
  ((kind
    :initarg       :kind
    :reader        overlay-kind
    :type          (member :commands :settings :picker :menu :prompt :panel :board)
    :documentation "The kind of the overlay.")
   (title
    :initarg       :title
    :initform      ""
    :reader        overlay-title
    :type          string
    :documentation "The title of the overlay window.")
   (query
    :initarg       :query
    :initform      ""
    :accessor      overlay-query
    :type          string
    :documentation "The text typed by the person. It narrows a list, or it is the answer to a prompt.")
   (selection
    :initform      0
    :accessor      overlay-selection
    :type          integer
    :documentation "The index of the highlighted row among the rows shown.")
   (scroll
    :initform      0
    :accessor      overlay-scroll
    :type          integer
    :documentation "The scroll position of the content, in rows for a list and in pixels for a panel.")
   (choices
    :initarg       :choices
    :initform      nil
    :reader        overlay-choices
    :type          list
    :documentation "The choices of a picker or a menu.")
   (action
    :initarg       :action
    :initform      nil
    :reader        overlay-action
    :type          (or null function)
    :documentation "The function called with the chosen value of a picker or a menu, or with the answer to a prompt.")
   (note
    :initarg       :note
    :initform      nil
    :reader        overlay-note
    :type          (or null string)
    :documentation "A sentence that says what a prompt asks for, or NIL.")
   (content
    :initarg       :content
    :initform      nil
    :accessor      overlay-content
    :documentation "The block shown in a panel, or NIL while the block is fetched.")
   (tabs
    :initarg       :tabs
    :initform      nil
    :reader        overlay-tabs
    :type          list
    :documentation "The tabs of a board. Each tab is a list (LABEL COUNT BLOCK) of the name, the number of items and the block shown.")
   (anchor
    :initarg       :anchor
    :initform      nil
    :reader        overlay-anchor
    :type          list
    :documentation "The position of a menu, as a list (X Y ABOVE-P). X and Y are a corner of the menu. ABOVE-P is true if the menu grows upward from the corner.")
   (parent
    :initarg       :parent
    :initform      nil
    :reader        overlay-parent
    :documentation "The overlay shown again after this overlay is closed, or NIL."))
  (:documentation "A view drawn over the window."))

(defparameter *overlay-row-height* 46
  "The height of a row in the list of commands and in the settings.")

(defparameter *choice-row-height* 30
  "The height of a row in a picker or a menu.")

(defparameter *picker-filter-minimum* 9
  "The smallest number of choices for which a picker has a search field.")


;;;; -- Opening and Putting Away --

(-> overlay--field (workbench) t)
(defun overlay--field (workbench)
  "Return the native text field of the overlays, or NIL if the window has no field yet."
  (let ((stage (workbench-stage workbench)))
    (and stage (third (stage-natives stage)))))

(-> overlay--focus (workbench) null)
(defun overlay--focus (workbench)
  "Give the keyboard focus to the field of the overlay shown, or to the stage if the overlay has no field. If no overlay is shown, give the focus to the composer."
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
  "Show OVERLAY over the window of WORKBENCH and give it the keyboard focus."
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
  "Show the commands or the settings over the window of WORKBENCH, as selected by KIND. Close all overlays if KIND is NIL."
  (if kind
      (overlay-open workbench
                    (make-instance 'overlay
                                   :kind  kind
                                   :title (ecase kind
                                            (:commands
                                             "Commands")
                                            (:settings
                                             "Settings"))))
      (let ((field (overlay--field workbench)))
        (setf (workbench-overlay workbench) nil)
        ;; Clear the field, because the typed text can be a key.
        (when field
          (setf (lightfast:value field) ""))
        (overlay--focus workbench)
        (workbench-redraw workbench)))
  nil)

(-> overlay-dismiss (workbench) null)
(defun overlay-dismiss (workbench)
  "Close the overlay shown. Show the overlay that it was opened from, if there is one."
  (let* ((overlay (workbench-overlay workbench))
         (parent  (and overlay (overlay-parent overlay))))
    (if parent
        (overlay-open workbench parent)
        (workbench-show-overlay workbench nil)))
  nil)

(-> overlay--returning (workbench) (or null overlay))
(defun overlay--returning (workbench)
  "Return the overlay of the settings if it is shown, or NIL."
  (let ((overlay (workbench-overlay workbench)))
    (and overlay (eq (overlay-kind overlay) ':settings) overlay)))

(-> workbench-pick (workbench list function &key (:title string)) null)
(defun workbench-pick (workbench choices action &key (title "Choose"))
  "Show CHOICES in a picker titled TITLE. Call ACTION with the value of the chosen entry."
  (let ((picker (make-instance 'overlay
                               :kind    ':picker
                               :title   title
                               :choices choices
                               :action  action
                               :parent  (overlay--returning workbench))))
    ;; Highlight the choice in force.
    (setf (overlay-selection picker) (or (position-if #'choice-current-p choices) 0))
    (overlay-open workbench picker))
  nil)

(-> workbench-menu
    (workbench list &key (:x integer) (:y integer) (:above-p boolean) (:action function)) null)
(defun workbench-menu (workbench choices &key (x 0) (y 0) above-p (action #'funcall))
  "Open a menu of CHOICES with a corner at X, Y. The menu grows upward if ABOVE-P is true.

ACTION is called with the value of the chosen entry. The default ACTION
calls the value as a function."
  (let ((menu (make-instance 'overlay
                             :kind    ':menu
                             :choices choices
                             :action  action
                             :anchor  (list x y above-p))))
    ;; Highlight nothing until the pointer moves or a key is pressed.
    (setf (overlay-selection menu) -1)
    (overlay-open workbench menu))
  nil)

(-> workbench-ask (workbench string function &key (:note (or null string)) (:initial string)) null)
(defun workbench-ask (workbench title action &key note (initial ""))
  "Ask for one line of text in a prompt titled TITLE. Call ACTION with the text entered.

The field starts with the text INITIAL. NOTE is a sentence that says what is
asked for."
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
  "Show BLOCKS in a panel titled TITLE. If BLOCKS is empty, the panel is shown as waiting for the content."
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
  "Show a board titled TITLE. TABS is a list of (LABEL COUNT . BLOCKS). The tabs are drawn down the left side, and the blocks of the chosen tab are drawn beside them."
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
  "Close the picker or menu shown and call the action of the overlay with the value of CHOICE. Nothing happens for a divider."
  (let ((action (overlay-action (workbench-overlay workbench))))
    (when (choice-value choice)
      (overlay-dismiss workbench)
      (when action
        (funcall action (choice-value choice)))))
  nil)

(-> overlay-accept (workbench) null)
(defun overlay-accept (workbench)
  "Close the prompt shown and call the action of the prompt with the answer."
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
  "Return the choices of OVERLAY that match the query. All choices of a menu are returned. For a picker, only the choices whose label or detail contains the query are returned."
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
  "Return T if OVERLAY is a picker with enough choices for a search field."
  (and (eq (overlay-kind overlay) ':picker)
       (>= (length (overlay-choices overlay)) *picker-filter-minimum*)))

(-> overlay--note-height (overlay integer) integer)
(defun overlay--note-height (overlay width)
  "Return the height of the note of OVERLAY at WIDTH, or zero if there is no note."
  (let ((note (overlay-note overlay)))
    (if (and note (plusp (length note)))
        (paragraph-layout (make-paragraph (list (make-span note nil)) :style (text-style ':small))
                          (max 40 width))
        0)))

(-> overlay--label-width (overlay) integer)
(defun overlay--label-width (overlay)
  "Return the width of the widest label among the choices of OVERLAY."
  (let ((style (text-style ':label-bold)))
    (loop for choice in (overlay-choices overlay)
          maximize (text-width (choice-label choice) style))))

(-> overlay--menu-size (overlay) (values integer integer))
(defun overlay--menu-size (overlay)
  "Return the width and the height of the menu OVERLAY. The width fits the widest label and the widest detail. Dividers are a third of a row high."
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
  "Return the box (X Y WIDTH HEIGHT) of the window of OVERLAY on a stage of the given size."
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
  "Return the box (X Y WIDTH HEIGHT) of the field of OVERLAY, or NIL if OVERLAY has no field."
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
  "Draw OVERLAY, which consists of the dimmed ground under it, the window and the content of the window."
  (destructuring-bind (x y width height) (overlay-box overlay (stage-width stage) (stage-height stage))
    (let* ((kind   (overlay-kind overlay))
           (menu-p (eq kind ':menu))
           (top-p  (eq overlay (workbench-overlay workbench)))
           (field  (overlay-input-box overlay (stage-width stage) (stage-height stage))))
      ;; Dim everything beneath the overlay, including the overlay that it
      ;; was opened from. Menus are drawn without dimming.
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
          ;; Only the top overlay has the field. Beneath it, draw the typed text.
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
  "Draw the overlay shown on top of the overlays that it was opened from. Each layer is dimmed by the layer above it."
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
  "Move the highlight of OVERLAY by STEP rows. Dividers are skipped, and the highlight stops at either end."
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
  "Handle the key EVENT for the overlay shown. Return T if the key was used."
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
