(in-package #:aui)

;;;; -- Page --

;;; The page is the open file at the right side of the window, shown only by its
;;; words. Each definition has a heading with the operator and the name as they are
;;; written in the source. Under the heading are the passages of the definition in
;;; the order of the file: comments, docstrings and other strings, each with its
;;; kind in the margin. A person clicks a passage to put the editor over it, and
;;; clicks the cross next to a comment to delete the comment. All text is set in the
;;; typewriter face, because line breaks are kept as typed, and in this face the
;;; length of a line is visible.

(defparameter *page-header-height* 74
  "The height of the band above the page, which contains the file name.")

(defparameter *page-status-height* 30
  "The height of the status line under the page.")

(defparameter *page-margin* 24
  "The space at the left and the right side of the page.")

(defparameter *page-gutter* 96
  "The width of the margin with the kind of each passage.")

(defparameter *page-text-width* 900
  "The maximum width of a passage.")

(defparameter *page-heading-height* 30
  "The height of the heading of a definition.")

(defparameter *page-padding* 6
  "The space above and below the words of a passage.")

(defstruct page-row
  "One row of the page, which is either the heading of a definition or one passage. KIND is the kind of the row. DEFINITION and PASSAGE are the numbers of the definition and the passage. TOP is the position in the page, and HEIGHT is the height of the row. PARAGRAPH is the paragraph of the words of a passage."
  (kind       ':passage :type keyword)
  (definition 0         :type fixnum)
  (passage    0         :type fixnum)
  (top        0         :type integer)
  (height     0         :type integer)
  (paragraph  nil))

(-> page-text-box (integer integer) (values integer integer))
(defun page-text-box (x width)
  "Return the left edge and the width of the words of a page in a box at X with WIDTH."
  (values (+ x *page-margin* *page-gutter*)
          (max 120 (min *page-text-width* (- width (* 2 *page-margin*) *page-gutter*)))))

(defparameter *page-kept-paragraphs* 2000
  "The number of laid-out paragraphs that are kept for the open file, in addition to one paragraph for each text of the file.")

(-> page--layout (string integer) paragraph)
(defun page--layout (text width)
  "Return a new paragraph of TEXT laid out to WIDTH."
  (let ((paragraph (make-paragraph (list (make-span text nil)) :style (text-style ':code))))
    (paragraph-layout paragraph width)
    paragraph))

(-> page--paragraph (desk string integer &key (:typed-p boolean)) paragraph)
(defun page--paragraph (desk text width &key typed-p)
  "Return the paragraph of TEXT laid out to WIDTH. The paragraphs of the texts of the open file are kept. If TYPED-P is true, TEXT is the text in the editor, and only the paragraph of the last such text is kept."
  (let ((layouts (desk-layouts desk)))
    (unless (= width (desk-layout-width desk))
      (clrhash layouts)
      (setf (desk-typed-layout desk) nil
            (desk-layout-width desk) width))
    (cond
      (typed-p
       (let ((kept (desk-typed-layout desk)))
         (if (and kept (string= text (first kept)))
             (rest kept)
             (let ((paragraph (or (gethash text layouts) (page--layout text width))))
               (setf (desk-typed-layout desk) (cons text paragraph))
               paragraph))))
      (t
       ;; Each written text adds a paragraph. The paragraphs are cleared if there are too many,
       ;; so that a file written many times does not keep all of them.
       (when (>= (hash-table-count layouts)
                 (+ *page-kept-paragraphs* (if (desk-proof desk) (proof-passage-count (desk-proof desk)) 0)))
         (clrhash layouts))
       (or (gethash text layouts)
           (setf (gethash text layouts) (page--layout text width)))))))

(-> page-rows (desk integer) (values list integer))
(defun page-rows (desk width)
  "Return the rows of the page of the open file for words set to WIDTH, and the total height of the rows.

Only the passages of the kinds that the person wants to see have rows. A
definition without such passages has no heading. A folded definition only has
the heading, and a hidden definition has no rows. The row of the passage being
written in has the height of the text in the editor."
  (let ((proof (desk-proof desk))
        (kinds (desk-kinds))
        (focus (desk-focus desk))
        (rows  nil)
        (top   0))
    (when proof
      (loop for definition in (proof-definitions proof)
            for definition-index from 0
            do (let ((heading-p nil)
                     (folded-p  (desk-section-folded-p desk definition-index)))
                 (loop for passage in (definition-passages definition)
                       for passage-index from 0
                       until (desk-section-hidden-p desk definition-index)
                       do (when (member (passage-kind passage) kinds)
                            (unless heading-p
                              (setf heading-p t)
                              (incf top (if rows 14 6))
                              (push (make-page-row :kind       ':heading
                                                   :definition definition-index
                                                   :top        top
                                                   :height     *page-heading-height*)
                                    rows)
                              (incf top *page-heading-height*))
                            (unless folded-p
                              (let* ((focused-p (and focus
                                                     (= definition-index (first focus))
                                                     (= passage-index (rest focus))))
                                     (paragraph (if focused-p
                                                    (page--paragraph desk (desk-typed-text desk) width :typed-p t)
                                                    (page--paragraph desk (passage-text passage) width)))
                                     ;; Lines in the editor break a little earlier than in a
                                     ;; paragraph, so the passage being written in gets one
                                     ;; extra line.
                                     (height    (+ (* 2 *page-padding*)
                                                   (paragraph-height paragraph)
                                                   (if focused-p (text-style-line-height (text-style ':code)) 0))))
                                (push (make-page-row :kind       ':passage
                                                     :definition definition-index
                                                     :passage    passage-index
                                                     :top        top
                                                     :height     height
                                                     :paragraph  paragraph)
                                      rows)
                                (incf top height))))))))
    (values (nreverse rows) (+ top 24))))

(-> page--focus-row (desk list) (or null page-row))
(defun page--focus-row (desk rows)
  "Return the row in ROWS of the passage being written in, or NIL."
  (let ((focus (desk-focus desk)))
    (and focus
         (find-if (lambda (row)
                    (and (eq (page-row-kind row) ':passage)
                         (= (page-row-definition row) (first focus))
                         (= (page-row-passage row) (rest focus))))
                  rows))))

(-> page-body-box (integer integer) (values integer integer))
(defun page-body-box (y height)
  "Return the top and the height of the scrolled part of a page in a box at Y with HEIGHT."
  (values (+ y *page-header-height*)
          (max 0 (- height *page-header-height* *page-status-height*))))

(-> page--editor-span (integer integer &key (:top integer) (:bottom integer)) (values (or null integer) integer))
(defun page--editor-span (y height &key top bottom)
  "Return the top and the height of the editor over the row of a passage at Y with HEIGHT, in a view from TOP to BOTTOM. Return NIL and 0 if too little of the row is in view to write in.

For a passage that is taller than the view, the editor only covers the visible
part. The text in the editor scrolls with the caret."
  (let ((from (max (+ y *page-padding* -1) top))
        (to   (min (+ y height (- *page-padding*) 3) bottom)))
    (if (>= (- to from) 40)
        (values from (- to from))
        (values nil 0))))

(-> page-editor-box (desk &key (:x integer) (:y integer) (:width integer) (:height integer)) (or null list))
(defun page-editor-box (desk &key x y width height)
  "Return the box (X Y WIDTH HEIGHT) of the editor on a page in the given box. Return NIL if no passage is being written in, or if too little of the row of the passage is in view."
  (multiple-value-bind (text-x text-width) (page-text-box x width)
    (multiple-value-bind (body-y body-height) (page-body-box y height)
      (let ((row (page--focus-row desk (page-rows desk text-width))))
        (when row
          (multiple-value-bind (top span)
              (page--editor-span (+ body-y (- (page-row-top row) (desk-page-scroll desk)))
                                 (page-row-height row)
                                 :top    body-y
                                 :bottom (+ body-y body-height))
            (when top
              (list (- text-x 4) top (+ text-width 8) span))))))))

(-> page-reveal (desk &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun page-reveal (desk &key x y width height)
  "Scroll the page in the given box so that the passage being written in is fully in view, if the view is tall enough."
  (multiple-value-bind (text-x text-width) (page-text-box x width)
    (declare (ignore text-x))
    (multiple-value-bind (body-y body-height) (page-body-box y height)
      (declare (ignore body-y))
      (let ((row (page--focus-row desk (page-rows desk text-width))))
        (when row
          (let ((scroll (desk-page-scroll desk))
                (top    (- (page-row-top row) 40))
                (bottom (+ (page-row-top row) (page-row-height row) 16)))
            (cond
              ((< top scroll)
               (setf (desk-page-scroll desk) (max 0 top)))
              ((> bottom (+ scroll body-height))
               (setf (desk-page-scroll desk) (max 0 (min top (- bottom body-height)))))))))))
  nil)

(-> page-step (desk integer) boolean)
(defun page-step (desk step)
  "Start writing in the passage STEP places after the passage being written in, or before it if STEP is negative. Return T if there was such a passage. If no passage is being written in, start with the first."
  (multiple-value-bind (rows) (page-rows desk (max 120 (desk-layout-width desk)))
    (let* ((passages (remove ':heading rows :key #'page-row-kind))
           (current  (page--focus-row desk rows))
           (index    (and current (position current passages)))
           (next     (cond
                       ((null passages)
                        nil)
                       ((null index)
                        (first passages))
                       ((<= 0 (+ index step) (1- (length passages)))
                        (nth (+ index step) passages))
                       (t
                        nil))))
      (when next
        (desk-edit desk (page-row-definition next) (page-row-passage next))
        t))))

(-> page--kind-label (keyword) string)
(defun page--kind-label (kind)
  "Return the label in the margin for a passage of KIND."
  (ecase kind
    (:comment
     "comment")
    (:docstring
     "docstring")
    (:string
     "string")))

(-> page--unseen-words (string) (or null string))
(defun page--unseen-words (text)
  "Return a description of TEXT if TEXT is empty or only contains blanks and line ends, or else NIL."
  (cond
    ((zerop (length text))
     "empty")
    ((find-if-not (lambda (character)
                    (member character '(#\Space #\Tab #\Newline #\Return)))
                  text)
     nil)
    ((every (lambda (character) (char= character #\Space)) text)
     (format nil "~D blank~:P" (length text)))
    ((every (lambda (character) (char= character #\Newline)) text)
     (format nil "~D line end~:P" (length text)))
    (t
     "blanks and line ends")))

(-> page--paint-heading (desk stage page-row &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-heading (desk stage row &key x y width)
  "Draw the heading of the definition of ROW: the box for the completed mark, the operator, the name, the number of texts in a folded definition, the edited label, and the first line at the right. The person can click the heading to fold or unfold the definition."
  (let* ((index       (page-row-definition row))
         (definition  (nth index (proof-definitions (desk-proof desk))))
         (operator    (definition-operator definition))
         (completed-p (desk-section-completed-p desk index))
         (folded-p    (desk-section-folded-p desk index))
         (edited-p    (desk-section-edited-p desk index))
         (plum        (text-style ':label-bold :color *plum*))
         (bold        (text-style ':label-bold))
         (meta        (text-style ':meta))
         (amber       (text-style ':meta :color *amber*))
         (line        (format nil "line ~D" (definition-line definition)))
         (line-w      (text-width line meta))
         (edited-w    (if edited-p (+ (text-width "edited" amber) 16) 0))
         (count       (if folded-p
                          (let ((texts (count-if (lambda (passage) (member (passage-kind passage) (desk-kinds)))
                                                 (definition-passages definition))))
                            (format nil "~D ~:*~[texts~;text~:;texts~]" texts))
                          ""))
         (height      *page-heading-height*)
         (box         (list ':section-check index))
         (region      (list ':section index))
         (box-x       (+ x *page-margin*))
         (box-y       (+ y (floor (- height 13) 2) -1))
         (left        (+ box-x 22))
         (right       (- (+ x width) *page-margin*))
         (name-room   (max 0 (- right line-w edited-w 20 left))))
    (flet ((content ()
             (paint-frame box-x box-y :width 13 :height 13)
             (when completed-p
               (paint-mark ':check (- box-x 1) (- box-y 2) :size 16))
             ;; Draw the operator, the name and, if the definition is folded, the number of texts,
             ;; one after the other in the available room.
             (let ((at left))
               (flet ((say (text style)
                        (let ((width (min (text-width text style) (max 0 (- (+ left name-room) at)))))
                          (when (plusp width)
                            (paint-text-in text style :x at :y y :width width :height height))
                          (incf at (+ width 10)))))
                 (cond
                   ((zerop (length operator))
                    (say "comments" plum))
                   (t
                    (say operator plum)
                    (say (definition-name definition) bold)))
                 (when folded-p
                   (incf at 4)
                   (say count meta))))
             (when edited-p
               (paint-text-in "edited" amber
                              :x      (- right line-w edited-w)
                              :y      y
                              :width  (- edited-w 16)
                              :height height))
             (paint-text-in line meta :x (- right line-w) :y y :width line-w :height height)
             (paint-line box-x (+ y height -3) :x2 right :y2 (+ y height -3))))
      (if (or (stage-hot-p stage region) (stage-hot-p stage box))
          (with-inversion (box-x (- y 1) (- right box-x) (- height 3))
            (content))
          (content)))
    (stage-region stage region
                  :x       box-x
                  :y       y
                  :width   (- right box-x)
                  :height  height
                  :action  (lambda () (desk-toggle-section-fold desk index))
                  :tooltip (if folded-p "Show the texts" "Fold to the heading"))
    ;; The box is over the heading, so a click on the box marks the section and does not fold it.
    (stage-region stage box
                  :x       (- box-x 6)
                  :y       y
                  :width   26
                  :height  height
                  :action  (lambda () (desk-toggle-section desk index))
                  :tooltip (if completed-p
                               "Remove the mark"
                               "Mark this section completed (Ctrl+Shift+Enter while writing in it)")))
  nil)

(-> page--paint-delete (desk stage page-row &key (:x integer) (:y integer) (:shown-p boolean)) null)
(defun page--paint-delete (desk stage row &key x y shown-p)
  "Add the click region of the cross used to delete the comment of ROW, with the top left corner at X, Y. Draw the cross if SHOWN-P is true or if the pointer is over it."
  (let* ((definition (page-row-definition row))
         (index      (page-row-passage row))
         (identifier (list ':passage-delete definition index))
         (hot-p      (stage-hot-p stage identifier))
         (size       20))
    (cond
      (hot-p
       (with-inversion (x y size size)
         (paint-mark ':cross (+ x 4) (+ y 4) :size 12)))
      (shown-p
       (paint-mark ':cross (+ x 4) (+ y 4) :size 12)))
    (stage-region stage identifier
                  :x       x
                  :y       y
                  :width   size
                  :height  size
                  :action  (lambda () (desk-delete-comment desk definition index))
                  :tooltip "Delete this comment (Ctrl+D while writing in it)"))
  nil)

(-> page--paint-passage (desk stage page-row &key (:x integer) (:y integer) (:text-x integer) (:text-width integer))
    null)
(defun page--paint-passage (desk stage row &key x y text-x text-width)
  "Draw the passage of ROW with the top at Y. The kind is drawn in the margin at X, and the words are drawn from TEXT-X with TEXT-WIDTH. If the person clicks the passage, the editor is put over it with the caret at the pointer. A comment has a cross between the margin and the words. The cross is drawn while the pointer is on the passage or while the passage is being written in."
  (let* ((definition (page-row-definition row))
         (index      (page-row-passage row))
         (passage    (proof-passage (desk-proof desk) definition index))
         (identifier (list ':passage definition index))
         (focus      (desk-focus desk))
         (focused-p  (and focus (= definition (first focus)) (= index (rest focus))))
         (paragraph  (page-row-paragraph row))
         (height     (page-row-height row))
         (text-y     (+ y *page-padding*))
         (meta       (text-style ':meta)))
    (paint-text-in (page--kind-label (passage-kind passage)) meta
                   :x      (+ x *page-margin*)
                   :y      text-y
                   :width  (- *page-gutter* 10)
                   :height (text-style-line-height (text-style ':code)))
    (cond
      ;; The editor is over the words. If too little of the row is visible for the editor,
      ;; the words are drawn, and the person can click to scroll the passage back into view.
      (focused-p
       (paint-frame (- text-x 7) (- y 1) :width (+ text-width 14) :height (+ height 2))
       (unless (page--editor-span y height :top *paint-top* :bottom *paint-bottom*)
         (paragraph-paint paragraph text-x text-y :top *paint-top* :bottom *paint-bottom*)
         (stage-region stage identifier
                       :x      (- text-x 7)
                       :y      y
                       :width  (+ text-width 14)
                       :height height
                       :cursor ':insert
                       :action (lambda () (desk-refocus desk)))))
      (t
       (let ((unseen (page--unseen-words (passage-text passage))))
         (if unseen
             (paint-text-in unseen (text-style ':meta :face ':mono-italic)
                            :x      text-x
                            :y      text-y
                            :width  text-width
                            :height (text-style-line-height (text-style ':code)))
             (paragraph-paint paragraph text-x text-y :top *paint-top* :bottom *paint-bottom*)))
       (when (stage-hot-p stage identifier)
         (paint-frame (- text-x 7) (- y 1) :width (+ text-width 14) :height (+ height 2) :dashed-p t))
       ;; The margin is part of the click region of the passage, so the pointer is still on
       ;; the passage on its way to the cross.
       (stage-region stage identifier
                     :x      (+ x *page-margin*)
                     :y      y
                     :width  (+ (- text-x x *page-margin*) text-width 7)
                     :height height
                     :cursor ':insert
                     :action (lambda ()
                               (desk-edit desk definition index
                                          :caret (paragraph-index-at
                                                  paragraph
                                                  (- (stage-pointer-x stage) text-x)
                                                  (- (stage-pointer-y stage) text-y)))))))
    ;; The cross is over the margin of the passage, so a click on the cross deletes the
    ;; comment and does not start writing.
    (when (eq (passage-kind passage) ':comment)
      (page--paint-delete desk stage row
                          :x       (- text-x 32)
                          :y       (+ text-y (floor (- (text-style-line-height (text-style ':code)) 20) 2))
                          :shown-p (or focused-p (stage-hot-p stage identifier)))))
  nil)

(-> page--paint-header (desk stage &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-header (desk stage &key x y width)
  "Draw the band above the page: the name and the folder of the file, the number of passages, the switches for the kinds of passages, and the button used to mark the file completed."
  (let* ((name        (desk-name desk))
         (proof       (desk-proof desk))
         (completed-p (desk-completed-p desk name))
         (meta        (text-style ':meta))
         (left        (+ x *page-margin*))
         (right       (- (+ x width) *page-margin*))
         (button-w    (if completed-p 130 160))
         (kinds       (desk-kinds)))
    (paint-text-in (subseq name (length (checkout-folder name))) (text-style ':heading-2)
                   :x left :y (+ y 10) :width (max 0 (- right left button-w 20)) :height 30)
    (paint-text-in (format nil "~A~D ~:*~[texts~;text~:;texts~] · ~D of ~D sections completed~:[~; · edited~]"
                           (let ((folder (checkout-folder name)))
                             (if (zerop (length folder)) "" (format nil "~A · " folder)))
                           (proof-passage-count proof)
                           (desk-sections-completed-count desk)
                           (length (proof-definitions proof))
                           (desk-edited-p desk name))
                   meta
                   :x left :y (+ y 42) :width (max 0 (- right left 330)) :height 20)
    (paint-button stage ':complete (if completed-p "Completed" "Mark completed")
                  :x         (- right button-w)
                  :y         (+ y 10)
                  :width     button-w
                  :height    30
                  :primary-p completed-p
                  :mark      (and completed-p ':check)
                  :action    (lambda () (desk-toggle-completed desk name))
                  :tooltip   (if completed-p "Remove the mark" "Mark this file completed as it is now"))
    ;; Draw the switches for the kinds of passages, from the right.
    (let ((chip-x right))
      (dolist (kind (reverse *passage-kinds*))
        (let* ((label (format nil "~As" (page--kind-label kind)))
               (chip  kind))
          (decf chip-x (+ (chip-width label) 12 4))
          (paint-chip stage (list ':kind chip) label
                      :x          chip-x
                      :y          (+ y 42)
                      :height     22
                      :mark-color (if (member kind kinds) *green* *paper*)
                      :action     (lambda () (desk-toggle-kind desk chip))
                      :tooltip    (if (member kind kinds) "Hide these" "Show these")))))
    (paint-line x (+ y *page-header-height* -1) :x2 (+ x width) :y2 (+ y *page-header-height* -1)))
  nil)

(-> page--clock (integer) string)
(defun page--clock (time)
  "Return the universal TIME as hours, minutes and seconds in the local time zone."
  (multiple-value-bind (second minute hour) (decode-universal-time time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))

(-> page--paint-status (desk stage &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-status (desk stage &key x y width)
  "Draw the status line under the page. The line contains the notice, the typing state or the time of the last write, and the keys used to move through the texts. After the deletion of a comment, the line contains a message about the deletion and a chip used to bring the comment back."
  (let* ((meta    (text-style ':meta))
         (left    (+ x *page-margin*))
         (keys    "Tab next text   Esc done   Ctrl+D delete comment   Ctrl+Shift+Enter section completed   Ctrl+Enter file completed")
         (keys-w  (text-width keys meta))
         ;; A notice is drawn in place of the keys.
         (keys-p  (and (not (desk-notice desk)) (> width (+ keys-w 260))))
         (room    (max 0 (- width (* 2 *page-margin*) (if keys-p (+ keys-w 20) 0))))
         (removal (desk-removal desk)))
    (paint-line x y :x2 (+ x width) :y2 y)
    (cond
      ((desk-notice desk)
       (paint-text-in (format nil "! ~A" (desk-notice desk)) (text-style ':meta :color *red*)
                      :x left :y y :width room :height *page-status-height*))
      ((desk-dirty-p desk)
       (paint-text-in "typing" meta :x left :y y :width room :height *page-status-height*))
      (removal
       (let* ((green  (text-style ':meta :color *green*))
              (said   (format nil "comment at line ~D deleted" (removal-line removal)))
              (said-w (min (text-width said green) (max 0 (- room (chip-width "Undo") 8)))))
         (paint-text-in said green :x left :y y :width said-w :height *page-status-height*)
         (paint-chip stage ':restore-comment "Undo"
                     :x       (+ left said-w 8)
                     :y       (+ y (floor (- *page-status-height* 24) 2))
                     :action  (lambda () (desk-restore-comment desk))
                     :tooltip "Bring the comment back")))
      ((desk-saved-at desk)
       (paint-text-in (format nil "saved ~A" (page--clock (desk-saved-at desk))) (text-style ':meta :color *green*)
                      :x left :y y :width room :height *page-status-height*)))
    (when keys-p
      (paint-text-in keys meta
                     :x      (- (+ x width) *page-margin* keys-w)
                     :y      y
                     :width  keys-w
                     :height *page-status-height*)))
  nil)

(-> page-paint (desk stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun page-paint (desk stage &key x y width height)
  "Draw the page of the open file in the box. If no file is open or the open file cannot be edited, draw a short message instead."
  (let ((proof  (desk-proof desk))
        (italic (text-style ':small :face ':serif-italic)))
    (cond
      ((null proof)
       (paint-text-in (if (desk-files desk)
                          "Choose a file on the left."
                          "There is no Lisp source in this folder.")
                      italic
                      :x (+ x *page-margin*) :y (+ y 20) :width (- width (* 2 *page-margin*)) :height 30))
      (t
       (page--paint-header desk stage :x x :y y :width width)
       (multiple-value-bind (text-x text-width) (page-text-box x width)
         (multiple-value-bind (body-y body-height) (page-body-box y height)
           (multiple-value-bind (rows total) (page-rows desk text-width)
             (let* ((maximum (max 0 (- total body-height)))
                    (scroll  (max 0 (min (desk-page-scroll desk) maximum))))
               (setf (desk-page-scroll desk) scroll)
               ;; Stop writing on a click next to the texts, as on a click outside a field.
               (stage-region stage ':page
                             :x      x
                             :y      body-y
                             :width  width
                             :height body-height
                             :action (lambda () (desk-leave desk))
                             :wheel  (lambda (vertical horizontal)
                                       (declare (ignore horizontal))
                                       (setf (desk-page-scroll desk)
                                             (max 0 (min maximum (+ (desk-page-scroll desk) (* vertical 60))))))
                             :cursor ':arrow)
               (with-clip (x body-y width body-height)
                 (let ((*paint-top*    body-y)
                       (*paint-bottom* (+ body-y body-height)))
                   (cond
                     ((proof-problem proof)
                      (paint-text-in (format nil "This file cannot be edited: ~A." (proof-problem proof)) italic
                                     :x      (+ x *page-margin*)
                                     :y      (+ body-y 16)
                                     :width  (- width (* 2 *page-margin*))
                                     :height 30))
                     ((null rows)
                      (paint-text-in (cond
                                       ((zerop (proof-passage-count proof))
                                        "This file has no comments and no strings.")
                                       ((and (desk-hide-completed-p) (plusp (desk-sections-completed-count desk)))
                                        "All sections in this file are completed. Press Ctrl+Enter to mark the file completed.")
                                       (t
                                        "This file has no texts of the kinds that are shown."))
                                     italic
                                     :x      (+ x *page-margin*)
                                     :y      (+ body-y 16)
                                     :width  (- width (* 2 *page-margin*))
                                     :height 30))
                     (t
                      (dolist (row rows)
                        (let ((top (+ body-y (- (page-row-top row) scroll))))
                          (when (and (< top *paint-bottom*) (> (+ top (page-row-height row)) *paint-top*))
                            (if (eq (page-row-kind row) ':heading)
                                (page--paint-heading desk stage row :x x :y top :width width)
                                (page--paint-passage desk stage row
                                                     :x          x
                                                     :y          top
                                                     :text-x     text-x
                                                     :text-width text-width)))))))))
               (when (> total body-height 40)
                 (let* ((thumb (max 24 (floor (* body-height body-height) total)))
                        (top   (+ body-y (floor (* (- body-height thumb) scroll) (max 1 maximum)))))
                   (paint-dither (- (+ x width) 6) top :width 4 :height thumb :step 2)))))
           (page--paint-status desk stage :x x :y (+ body-y body-height) :width width))))))
  nil)
