(in-package #:aui)

;;;; -- Sheets --

;;; The content of the smaller overlays. It consists of the rows of pickers and
;;; menus, the note, field and buttons of prompts, and the scrolled blocks of
;;; panels and boards.

(-> sheet-paint-scrollbar
    (stage &key (:x integer) (:y integer) (:height integer) (:content integer) (:scroll integer)) null)
(defun sheet-paint-scrollbar (stage &key (x 0) (y 0) (height 0) (content 0) (scroll 0))
  "Draw a scroll thumb at X and Y for content of the height CONTENT, which is shown in HEIGHT pixels from SCROLL. Nothing is drawn if the content fits."
  (declare (ignore stage))
  (when (and (> content height) (> height 40))
    (let* ((thumb (max 24 (floor (* height height) content)))
           (track (- height thumb))
           (top   (+ y (floor (* track (min scroll (- content height))) (- content height)))))
      (paint-dither x top :width 4 :height thumb :step 2)))
  nil)


;;;; -- Choices --

(-> sheet-paint-choices
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sheet-paint-choices (workbench stage overlay &key x y width height)
  "Draw the choices of the picker or menu OVERLAY, one choice in each row. The labels are drawn in one column and the details in the next. A check mark is drawn before the current choice."
  (let* ((menu-p   (eq (overlay-kind overlay) ':menu))
         (choices  (overlay-shown-choices overlay))
         (count    (length choices))
         (visible  (max 1 (floor height *choice-row-height*)))
         (bold     (text-style ':label-bold))
         (meta     (text-style ':meta))
         (detail-p (some #'choice-detail (overlay-choices overlay)))
         (label-w  (if detail-p
                       (min (overlay--label-width overlay) (floor (* width 45/100)))
                       (- width 40)))
         (detail-x (+ x 30 label-w 20)))
    (setf (overlay-selection overlay) (max -1 (min (overlay-selection overlay) (1- count))))
    (unless menu-p
      (setf (overlay-scroll overlay)
            (max 0 (min (overlay-scroll overlay) (max 0 (- count visible)))))
      (when (and (>= (overlay-selection overlay) 0)
                 (< (overlay-selection overlay) (overlay-scroll overlay)))
        (setf (overlay-scroll overlay) (overlay-selection overlay)))
      (when (>= (overlay-selection overlay) (+ (overlay-scroll overlay) visible))
        (setf (overlay-scroll overlay) (- (overlay-selection overlay) visible -1)))
      (stage-region stage ':overlay-list
                    :x      x
                    :y      y
                    :width  width
                    :height height
                    :wheel  (catalog--scroll overlay count visible)))
    (when (zerop count)
      (paint-text-in "Nothing matches." (text-style ':small :face ':serif-italic)
                     :x      (+ x 16)
                     :y      y
                     :width  (- width 32)
                     :height *choice-row-height*))
    (with-clip (x y width height)
      (let ((row-y y))
        (loop for choice in (if menu-p choices (nthcdr (overlay-scroll overlay) choices))
              for index from (if menu-p 0 (overlay-scroll overlay))
              while (< row-y (+ y height))
              do (if (null (choice-value choice))
                     ;; Draw a divider as a short band of dither.
                     (let ((gap (floor *choice-row-height* 3)))
                       (paint-dither (+ x 8) (+ row-y (floor gap 2) -1) :width (- width 16) :height 2 :step 3)
                       (incf row-y gap))
                     (let* ((chosen choice)
                            (region (list ':choice index))
                            (lit-p  (or (= index (overlay-selection overlay)) (stage-hot-p stage region)))
                            (top    row-y))
                       (flet ((content ()
                                (when (choice-current-p chosen)
                                  (paint-mark ':check (+ x 9) (+ top (floor (- *choice-row-height* 12) 2))
                                              :size 12))
                                (paint-text-in (choice-label chosen) bold
                                               :x      (+ x 30)
                                               :y      top
                                               :width  label-w
                                               :height *choice-row-height*)
                                (when (choice-detail chosen)
                                  (paint-text-in (choice-detail chosen) meta
                                                 :x      detail-x
                                                 :y      top
                                                 :width  (max 0 (- (+ x width) detail-x 16))
                                                 :height *choice-row-height*))))
                         (if lit-p
                             (with-inversion (x top width *choice-row-height*)
                               (content))
                             (content)))
                       (stage-region stage region
                                     :x      x
                                     :y      top
                                     :width  width
                                     :height *choice-row-height*
                                     :action (lambda () (overlay-choose workbench chosen)))
                       (incf row-y *choice-row-height*))))))
    (unless menu-p
      (sheet-paint-scrollbar stage
                             :x       (+ x width -9)
                             :y       y
                             :height  height
                             :content (* count *choice-row-height*)
                             :scroll  (* (overlay-scroll overlay) *choice-row-height*))))
  nil)


;;;; -- Prompt --

(-> sheet-paint-prompt
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sheet-paint-prompt (workbench stage overlay &key x y width height)
  "Draw the content of the prompt OVERLAY below the title bar. It consists of the note, the frame of the field, and the OK and Cancel buttons."
  (let* ((note   (overlay-note overlay))
         (field  (overlay-input-box overlay (stage-width stage) (stage-height stage)))
         (bottom (+ y height))
         (button 92))
    (when (and note (plusp (length note)))
      (let ((paragraph (make-paragraph (list (make-span note nil)) :style (text-style ':small))))
        (paragraph-layout paragraph (- width 32))
        (paragraph-paint paragraph (+ x 16) (+ y 14))))
    (destructuring-bind (field-x field-y field-width field-height) field
      (paint-frame (- field-x 5) (- field-y 3) :width (+ field-width 10) :height (+ field-height 6)))
    (paint-button stage ':prompt-accept "OK"
                  :x         (- (+ x width) button 14)
                  :y         (- bottom 46)
                  :width     button
                  :height    32
                  :primary-p t
                  :action    (lambda () (overlay-accept workbench))
                  :tooltip   "Enter")
    (paint-button stage ':prompt-cancel "Cancel"
                  :x       (- (+ x width) (* 2 button) 22)
                  :y       (- bottom 46)
                  :width   button
                  :height  32
                  :action  (lambda () (overlay-dismiss workbench))
                  :tooltip "Esc"))
  nil)


;;;; -- Panel --

(-> sheet-paint-panel
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sheet-paint-panel (workbench stage overlay &key x y width height)
  "Draw the scrolled content of the panel OVERLAY in the box. If there is no content yet, draw a waiting line instead."
  (declare (ignorable workbench))
  (let* ((content (overlay-content overlay))
         (inner   (max 80 (- width 56))))
    (if (null content)
        (paint-text-in "waiting for the answer" (text-style ':small :face ':serif-italic)
                       :x      (+ x 24)
                       :y      (+ y 8)
                       :width  inner
                       :height 44)
        (let* ((total   (+ (block-layout content inner) 36))
               (maximum (max 0 (- total height))))
          (setf (overlay-scroll overlay) (max 0 (min (overlay-scroll overlay) maximum)))
          (stage-region stage ':overlay-list
                        :x      x
                        :y      y
                        :width  width
                        :height height
                        :wheel  (lambda (vertical horizontal)
                                  (declare (ignore horizontal))
                                  (setf (overlay-scroll overlay)
                                        (max 0 (min maximum (+ (overlay-scroll overlay) (* vertical 48)))))))
          (with-clip (x y width height)
            (let ((*paint-stage*  stage)
                  (*paint-top*    y)
                  (*paint-bottom* (+ y height)))
              (block-paint content (+ x 24) (+ y 18 (- (overlay-scroll overlay))))))
          (sheet-paint-scrollbar stage
                                 :x       (+ x width -9)
                                 :y       y
                                 :height  height
                                 :content total
                                 :scroll  (overlay-scroll overlay)))))
  nil)


;;;; -- Board --

(defparameter *board-tabs-width* 190
  "The width of the column of tabs at the left of a board.")

(defparameter *board-tab-height* 34
  "The height of a tab of a board.")

(-> sheet-paint-board
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sheet-paint-board (workbench stage overlay &key x y width height)
  "Draw the board OVERLAY in the box. The tabs are drawn down the left side, each with the number of items. The scrolled blocks of the chosen tab are drawn beside them."
  (let* ((tabs   (overlay-tabs overlay))
         (chosen (max 0 (min (overlay-selection overlay) (1- (length tabs)))))
         (bold   (text-style ':label-bold))
         (meta   (text-style ':meta))
         (pane-x (+ x *board-tabs-width*))
         (pane-w (- width *board-tabs-width*))
         (inner  (max 80 (- pane-w 56))))
    (setf (overlay-selection overlay) chosen)
    (loop for (label count) in tabs
          for index from 0
          for tab-y from (+ y 10) by *board-tab-height*
          do (let* ((tab    index)
                    (region (list ':tab index))
                    (lit-p  (or (= index chosen) (stage-hot-p stage region)))
                    (top    tab-y))
               (flet ((content ()
                        (paint-text-in label bold
                                       :x      (+ x 16)
                                       :y      top
                                       :width  (- *board-tabs-width* 60)
                                       :height *board-tab-height*)
                        (paint-text-in (princ-to-string count) meta
                                       :x      (+ x (- *board-tabs-width* 44))
                                       :y      top
                                       :width  30
                                       :height *board-tab-height*
                                       :align  ':right)))
                 (if lit-p
                     (with-inversion (x top (- *board-tabs-width* 6) *board-tab-height*)
                       (content))
                     (content)))
               (stage-region stage region
                             :x      x
                             :y      top
                             :width  (- *board-tabs-width* 6)
                             :height *board-tab-height*
                             :action (lambda ()
                                       (setf (overlay-selection overlay) tab
                                             (overlay-scroll overlay)    0)
                                       (workbench-redraw workbench)))))
    ;; Draw a band of dither between the tabs and the content of the chosen tab.
    (paint-dither (- pane-x 4) y :width 3 :height height :step 3)
    (let ((content (third (nth chosen tabs))))
      (when content
        (let* ((total   (+ (block-layout content inner) 36))
               (maximum (max 0 (- total height))))
          (setf (overlay-scroll overlay) (max 0 (min (overlay-scroll overlay) maximum)))
          (stage-region stage ':overlay-list
                        :x      pane-x
                        :y      y
                        :width  pane-w
                        :height height
                        :wheel  (lambda (vertical horizontal)
                                  (declare (ignore horizontal))
                                  (setf (overlay-scroll overlay)
                                        (max 0 (min maximum (+ (overlay-scroll overlay) (* vertical 48)))))))
          (with-clip (pane-x y pane-w height)
            (let ((*paint-stage*  stage)
                  (*paint-top*    y)
                  (*paint-bottom* (+ y height)))
              (block-paint content (+ pane-x 24) (+ y 18 (- (overlay-scroll overlay))))))
          (sheet-paint-scrollbar stage
                                 :x       (+ x width -9)
                                 :y       y
                                 :height  height
                                 :content total
                                 :scroll  (overlay-scroll overlay))))))
  nil)
