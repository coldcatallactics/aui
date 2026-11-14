(in-package #:aui)

;;;; -- Overlays --

;;; Views laid over the window: the list of commands, found by typing, and
;;; the settings, each with its current value to change in place. They stand
;;; in for the terminal's pickers and its settings page. An overlay is a
;;; window in the Interlisp manner on a dithered ground; a click outside it or
;;; Escape puts it away.

(defclass overlay ()
  ((kind
    :initarg       :kind
    :reader        overlay-kind
    :type          (member :commands :settings)
    :documentation "Which view this is.")
   (query
    :initform      ""
    :accessor      overlay-query
    :type          string
    :documentation "What the person typed to narrow the list.")
   (selection
    :initform      0
    :accessor      overlay-selection
    :type          integer
    :documentation "The index of the highlighted row among those shown.")
   (scroll
    :initform      0
    :accessor      overlay-scroll
    :type          integer
    :documentation "How far the list is scrolled."))
  (:documentation "A view laid over the window."))

(defparameter *overlay-row-height* 46
  "The height of one row of an overlay's list.")

(-> workbench-show-overlay (workbench (or null keyword)) null)
(defun workbench-show-overlay (workbench kind)
  "Lay the view KIND over WORKBENCH's window, or put the overlay away when KIND is NIL."
  (setf (workbench-overlay workbench) (and kind (make-instance 'overlay :kind kind)))
  (when (eq kind ':settings)
    (workbench-refresh workbench "_autolith/settings"))
  (workbench-redraw workbench)
  nil)


;;;; -- Commands --

(defparameter *overlay-interface-commands*
  '("model" "effort" "permissions" "settings" "open" "new")
  "Commands whose terminal pickers the window answers with a view of its own.")

(defparameter *overlay-terminal-only-commands*
  '("onboard" "quit" "exit" "detach" "memories" "memory" "why" "rollback" "auth" "complain" "wtf")
  "Commands that need a terminal and have no counterpart in the window yet; they are not offered.")

(-> overlay--command-rows (workbench string) list)
(defun overlay--command-rows (workbench query)
  "Return the operations to list for QUERY: commands first, then tools, those not usable from a window left out."
  (let ((rows (remove-if (lambda (row)
                           (let ((name (portable-get row "name")))
                             (or (member name *overlay-terminal-only-commands* :test #'equal)
                                 (and (portable-true-p (portable-get row "terminalOwning"))
                                      (not (member name *overlay-interface-commands* :test #'equal))))))
                         (workbench-operations workbench))))
    (stable-sort
     (remove-if-not (lambda (row)
                      (or (zerop (length query))
                          (search query (or (portable-get row "name") "") :test #'char-equal)
                          (search query (or (portable-get row "description") "") :test #'char-equal)))
                    rows)
     (lambda (first second)
       (and (equal (portable-get first "kind") "command")
            (not (equal (portable-get second "kind") "command")))))))

(-> overlay--run (workbench list) null)
(defun overlay--run (workbench row)
  "Do what choosing the operation ROW means: open the window's own view, ask for its argument, or run it."
  (let ((name    (portable-get row "name"))
        (choices (mapcar #'princ-to-string (portable-items (portable-get row "choices")))))
    (workbench-show-overlay workbench nil)
    (cond
      ((equal name "model")
       (composer--choose-model workbench))
      ((equal name "effort")
       (composer--choose-effort workbench))
      ((equal name "permissions")
       (composer--choose-mode workbench))
      ((equal name "settings")
       (workbench-show-overlay workbench ':settings))
      ((equal name "new")
       (workbench-new-channel workbench))
      ((equal name "open")
       (setf (workbench-sidebar-shown-p workbench) t)
       (window-focus-filter workbench))
      ((not (equal (portable-get row "kind") "command"))
       (setf (composer-text workbench) (format nil "(~A )" name))
       (window-focus-composer workbench))
      (choices
       (let ((choice (lightfast:popup-menu (cons (format nil "~A (no argument)" name) choices))))
         (cond
           ((null choice)
            nil)
           ((zerop choice)
            (workbench-submit workbench (format nil "/~A" name)))
           (t
            (workbench-submit workbench (format nil "/~A ~A" name (nth (1- choice) choices)))))))
      ((portable-string (portable-get row "argumentHint"))
       (setf (composer-text workbench) (format nil "/~A " name))
       (window-focus-composer workbench))
      (t
       (workbench-submit workbench (format nil "/~A" name)))))
  nil)

(-> overlay--paint-commands
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun overlay--paint-commands (workbench stage overlay &key x y width height)
  "Draw the list of operations matching the overlay's query."
  (let* ((rows    (overlay--command-rows workbench (overlay-query overlay)))
         (visible (max 1 (floor height *overlay-row-height*)))
         (count   (length rows)))
    (setf (overlay-selection overlay) (max 0 (min (overlay-selection overlay) (1- count))))
    (setf (overlay-scroll overlay)
          (max 0 (min (overlay-scroll overlay) (max 0 (- count visible)))))
    (when (< (overlay-selection overlay) (overlay-scroll overlay))
      (setf (overlay-scroll overlay) (overlay-selection overlay)))
    (when (>= (overlay-selection overlay) (+ (overlay-scroll overlay) visible))
      (setf (overlay-scroll overlay) (- (overlay-selection overlay) visible -1)))
    (stage-region stage ':overlay-list
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (overlay-scroll overlay)
                                  (max 0 (min (max 0 (- count visible))
                                              (+ (overlay-scroll overlay) vertical))))))
    (when (zerop count)
      (paint-text-in (if (workbench-operations workbench)
                         "Nothing matches."
                         "The channel has not sent its commands yet.") (text-style ':small :face ':serif-italic)
                     :x      (+ x 16)
                     :y      y
                     :width  (- width 32)
                     :height 44))
    (lightfast:with-clip (x y width height)
      (loop for row in (nthcdr (overlay-scroll overlay) rows)
            for index from (overlay-scroll overlay)
            for row-y from y by *overlay-row-height*
            while (< row-y (+ y height))
            do (let* ((chosen row)
                      (region (list ':operation (portable-get row "name")))
                      (lit-p  (or (= index (overlay-selection overlay)) (stage-hot-p stage region))))
                 (flet ((content ()
                          (let* ((bold (text-style ':label-bold))
                                 (hint (portable-string (portable-get row "argumentHint")))
                                 (end  (paint-text (portable-get row "name") (+ x 16) (+ row-y 19) :style bold)))
                            (when hint
                              (paint-text (text-fit hint (text-style ':label) (- (+ x width) end 30)) (+ end 10) (+ row-y 19)
                                          :style (text-style ':label)))
                            (unless (equal (portable-get row "kind") "command")
                              (paint-text-in "tool" (text-style ':meta)
                                             :x      (- (+ x width) 60)
                                             :y      row-y
                                             :width  44
                                             :height 24
                                             :align  ':right))
                            (paint-text (text-fit (entries--one-line (or (portable-get row "description") ""))
                                                  (text-style ':small) (- width 32)) (+ x 16) (+ row-y 38)
                                        :style (text-style ':small)))))
                   (if lit-p
                       (with-inversion (x row-y width *overlay-row-height*)
                         (content))
                       (content)))
                 (stage-region stage region
                               :x      x
                               :y      row-y
                               :width  width
                               :height *overlay-row-height*
                               :action (lambda () (overlay--run workbench chosen)))))))
  nil)


;;;; -- Settings --

(-> overlay--setting-value-text (list) string)
(defun overlay--setting-value-text (row)
  "Return the setting ROW's value as it reads on a chip."
  (let ((value (portable-get row "value" ':null)))
    (cond
      ((equal (portable-get row "type") "boolean")
       (if (portable-true-p value) "on" "off"))
      ((eq value ':null)
       "none")
      ((stringp value)
       (if (zerop (length value)) "empty" value))
      (t
       (princ-to-string value)))))

(-> overlay--change-setting (workbench list) null)
(defun overlay--change-setting (workbench row)
  "Let the person change the setting ROW in the way its type calls for."
  (let ((name (portable-get row "name"))
        (type (portable-get row "type")))
    (cond
      ((equal type "boolean")
       (workbench-set-setting workbench name
                              (if (portable-true-p (portable-get row "value")) ':false t)))
      ((equal type "choice")
       (let* ((options (mapcar (lambda (option)
                                 (if (portable-true-p option)
                                     (princ-to-string option)
                                     "none"))
                               (portable-items (portable-get row "options"))))
              (choice  (and options (lightfast:popup-menu options))))
         (when choice
           (workbench-set-setting workbench name (nth choice options)))))
      (t
       (let ((answer (lightfast:input-dialog (or (portable-get row "label") name)
                                             :initial (let ((value (portable-get row "value")))
                                                        (if value (princ-to-string value) "")))))
         (when answer
           (workbench-set-setting workbench name
                                  (if (equal type "integer")
                                      (or (parse-integer answer :junk-allowed t) answer)
                                      answer)))))))
  nil)

(-> overlay--setting-rows (workbench string) list)
(defun overlay--setting-rows (workbench query)
  "Return the settings to list for QUERY, each group's rows together under its name: (:GROUP NAME) or (:SETTING ROW)."
  (let ((rows   (remove-if-not (lambda (row)
                                 (or (zerop (length query))
                                     (search query (or (portable-get row "label") "") :test #'char-equal)
                                     (search query (or (portable-get row "name") "") :test #'char-equal)))
                               (workbench-settings workbench)))
        (result nil)
        (groups nil))
    (dolist (row rows)
      (pushnew (or (portable-get row "group") "other") groups :test #'equal))
    (dolist (group (nreverse groups))
      (push (list ':group group) result)
      (dolist (row rows)
        (when (equal (or (portable-get row "group") "other") group)
          (push (list ':setting row) result))))
    (nreverse result)))

(-> overlay--paint-settings
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun overlay--paint-settings (workbench stage overlay &key x y width height)
  "Draw the settings as a property sheet: label and explanation at the left, the value as a chip at the right."
  (let* ((rows    (overlay--setting-rows workbench (overlay-query overlay)))
         (count   (length rows))
         (visible (max 1 (floor height *overlay-row-height*))))
    (setf (overlay-scroll overlay)
          (max 0 (min (overlay-scroll overlay) (max 0 (- count visible)))))
    (stage-region stage ':overlay-list
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (overlay-scroll overlay)
                                  (max 0 (min (max 0 (- count visible))
                                              (+ (overlay-scroll overlay) vertical))))))
    (when (zerop count)
      (paint-text-in (if (workbench-settings workbench)
                         "Nothing matches."
                         "The channel has not sent its settings yet.") (text-style ':small :face ':serif-italic)
                     :x      (+ x 16)
                     :y      y
                     :width  (- width 32)
                     :height 44))
    (lightfast:with-clip (x y width height)
      (loop for (kind value) in (nthcdr (overlay-scroll overlay) rows)
            for row-y from y by *overlay-row-height*
            while (< row-y (+ y height))
            do (ecase kind
                 (:group
                  (paint-text (string-capitalize value) (+ x 16) (+ row-y 32) :style (text-style ':label-bold))
                  (paint-dither (+ x 16) (+ row-y 40) :width (- width 32) :height 3))
                 (:setting
                  (let* ((row          value)
                         (adjustable-p (portable-true-p (portable-get row "adjustable")))
                         (text         (overlay--setting-value-text row))
                         (meta         (text-style ':meta))
                         (shown        (text-fit text meta 220))
                         (chip-w       (+ 16 (text-width shown meta)
                                          (if (and adjustable-p (not (equal (portable-get row "type") "boolean")))
                                              14
                                              0)))
                         (chip-x       (- (+ x width) chip-w 16)))
                    (paint-text (text-fit (or (portable-get row "label") (portable-get row "name") "")
                                          (text-style ':small :face ':serif-bold) (- chip-x x 28)) (+ x 16) (+ row-y 19)
                                :style (text-style ':small :face ':serif-bold))
                    (paint-text (text-fit (entries--one-line (or (portable-get row "documentation")
                                                                 (portable-get row "reason")
                                                                 ""))
                                          (text-style ':meta) (- chip-x x 28)) (+ x 16) (+ row-y 37)
                                :style (text-style ':meta))
                    (if adjustable-p
                        (progn
                          (paint-frame chip-x (+ row-y 10) :width chip-w :height 24)
                          (paint-chip stage (list ':setting (portable-get row "name")) shown
                                      :x       chip-x
                                      :y       (+ row-y 10)
                                      :height  24
                                      :menu-p  (not (equal (portable-get row "type") "boolean"))
                                      :action  (lambda () (overlay--change-setting workbench row))
                                      :tooltip (format nil "~A, ~A" (portable-get row "source")
                                                       (portable-get row "scope"))))
                        (progn
                          (paint-frame chip-x (+ row-y 10) :width chip-w :height 24 :dashed-p t)
                          (paint-text-in shown meta
                                         :x      (+ chip-x 8)
                                         :y      (+ row-y 10)
                                         :width  (- chip-w 16)
                                         :height 24)))))))))
  nil)


;;;; -- Frame --

(-> overlay-box (integer integer) list)
(defun overlay-box (stage-width stage-height)
  "Return the (X Y WIDTH HEIGHT) an overlay's window takes on a stage of the given size."
  (let* ((width  (min 760 (- stage-width 48)))
         (height (min 640 (- stage-height 96))))
    (list (floor (- stage-width width) 2) (max 32 (floor (- stage-height height) 3)) width height)))

(-> overlay-input-box (integer integer) list)
(defun overlay-input-box (stage-width stage-height)
  "Return the (X Y WIDTH HEIGHT) of the query field inside an overlay's window."
  (destructuring-bind (x y width height) (overlay-box stage-width stage-height)
    (declare (ignore height))
    (list (+ x 17) (+ y 28 13) (- width 34) 30)))

(-> overlay-paint (workbench stage) null)
(defun overlay-paint (workbench stage)
  "Draw the overlay over everything else: the dithered ground, its window, its query field's frame and its list."
  (let ((overlay (workbench-overlay workbench)))
    (when overlay
      (destructuring-bind (x y width height) (overlay-box (stage-width stage) (stage-height stage))
        (paint-dither 0 0 :width (stage-width stage) :height (stage-height stage) :step 3)
        (stage-region stage ':overlay-ground
                      :x      0
                      :y      0
                      :width  (stage-width stage)
                      :height (stage-height stage)
                      :action (lambda () (workbench-show-overlay workbench nil))
                      :cursor ':default)
        (let ((bar (paint-window-frame x y
                                       :width  width
                                       :height height
                                       :title  (ecase (overlay-kind overlay)
                                                 (:commands
                                                  "Commands")
                                                 (:settings
                                                  "Settings")))))
          (stage-region stage ':overlay-window :x x :y y :width width :height height :cursor ':default)
          (paint-button stage ':overlay-close ""
                        :x       (- (+ x width) 26)
                        :y       (+ y 4)
                        :width   20
                        :height  20
                        :mark    ':cross
                        :action  (lambda () (workbench-show-overlay workbench nil))
                        :tooltip "Close (Esc)")
          (paint-frame (+ x 12) (+ y bar 10) :width (- width 24) :height 36)
          (let ((list-y (+ y bar 56)))
            (ecase (overlay-kind overlay)
              (:commands
               (overlay--paint-commands workbench stage overlay
                                        :x (1+ x) :y list-y :width (- width 2) :height (- (+ y height) list-y 1)))
              (:settings
               (overlay--paint-settings workbench stage overlay
                                        :x (1+ x) :y list-y :width (- width 2)
                                        :height (- (+ y height) list-y 1)))))))))
  nil)

(-> overlay-key (workbench key-event) boolean)
(defun overlay-key (workbench event)
  "Handle a key while an overlay is shown and return whether it was used."
  (let ((overlay (workbench-overlay workbench)))
    (case (key-event-key event)
      (:escape
       (workbench-show-overlay workbench nil)
       t)
      (:down
       (incf (overlay-selection overlay))
       (when (eq (overlay-kind overlay) ':settings)
         (incf (overlay-scroll overlay)))
       (workbench-redraw workbench)
       t)
      (:up
       (setf (overlay-selection overlay) (max 0 (1- (overlay-selection overlay))))
       (when (eq (overlay-kind overlay) ':settings)
         (setf (overlay-scroll overlay) (max 0 (1- (overlay-scroll overlay)))))
       (workbench-redraw workbench)
       t)
      ((:enter :keypad-enter)
       (when (eq (overlay-kind overlay) ':commands)
         (let ((row (nth (overlay-selection overlay)
                         (overlay--command-rows workbench (overlay-query overlay)))))
           (when row
             (overlay--run workbench row))))
       t)
      (otherwise
       nil))))
