(in-package #:aui)

;;;; -- Page --

;;; The open file at the window's right, as its words alone. Each definition
;;; is a heading, its operator and name as they stand in the source, and
;;; under it come its passages in the order of the file: comments, docstrings
;;; and other strings, each named in the margin. A click on a passage puts
;;; the editor over it. Everything is set in the typewriter face, because
;;; line breaks are kept as they are typed and the face shows how long a line
;;; has become.

(defparameter *page-header-height* 74
  "The height of the band over the page that names the file.")

(defparameter *page-status-height* 30
  "The height of the line under the page that says what was saved.")

(defparameter *page-margin* 24
  "The room at the page's left and right.")

(defparameter *page-gutter* 96
  "The width of the margin that names each passage's kind.")

(defparameter *page-text-width* 900
  "The widest a passage is set.")

(defparameter *page-heading-height* 30
  "The height of a definition's heading.")

(defparameter *page-padding* 6
  "The room above and below a passage's words.")

(defstruct page-row
  "One row of the page, a definition's heading or one passage, with the numbers of its DEFINITION and PASSAGE, its TOP in the page, its HEIGHT and for a passage the PARAGRAPH of its words."
  (kind       ':passage :type keyword)
  (definition 0         :type fixnum)
  (passage    0         :type fixnum)
  (top        0         :type integer)
  (height     0         :type integer)
  (paragraph  nil))

(-> page-text-box (integer integer) (values integer integer))
(defun page-text-box (x width)
  "Return where the words of a page in a box at X of WIDTH start, and how wide they are set."
  (values (+ x *page-margin* *page-gutter*)
          (max 120 (min *page-text-width* (- width (* 2 *page-margin*) *page-gutter*)))))

(-> page--paragraph (desk string integer) paragraph)
(defun page--paragraph (desk text width)
  "Return the paragraph of TEXT laid out WIDTH wide, made once for each text of the open file."
  (let ((layouts (desk-layouts desk)))
    (unless (= width (desk-layout-width desk))
      (clrhash layouts)
      (setf (desk-layout-width desk) width))
    ;; What is typed makes a new text with every key; they are not all kept.
    (when (> (hash-table-count layouts) 8000)
      (clrhash layouts))
    (or (gethash text layouts)
        (let ((paragraph (make-paragraph (list (make-span text nil)) :style (text-style ':code))))
          (paragraph-layout paragraph width)
          (setf (gethash text layouts) paragraph)))))

(-> page-rows (desk integer) (values list integer))
(defun page-rows (desk width)
  "Return the rows of the open file's page for words set WIDTH wide, and the height of all of them.

Only the kinds of passage the person wants are rows, and a definition none
of whose passages is wanted has no heading. The passage being written in is
as tall as what the editor holds."
  (let ((proof (desk-proof desk))
        (kinds (desk-kinds))
        (focus (desk-focus desk))
        (rows  nil)
        (top   0))
    (when proof
      (loop for definition in (proof-definitions proof)
            for definition-index from 0
            do (let ((heading-p nil))
                 (loop for passage in (definition-passages definition)
                       for passage-index from 0
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
                            (let* ((focused-p (and focus
                                                   (= definition-index (first focus))
                                                   (= passage-index (rest focus))))
                                   (paragraph (page--paragraph desk
                                                               (if focused-p
                                                                   (desk-typed-text desk)
                                                                   (passage-text passage))
                                                               width))
                                   ;; The editor breaks lines a little sooner than a paragraph does,
                                   ;; so the passage being written in has a line to spare.
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
                              (incf top height)))))))
    (values (nreverse rows) (+ top 24))))

(-> page--focus-row (desk list) (or null page-row))
(defun page--focus-row (desk rows)
  "Return the row among ROWS of the passage being written in, or NIL."
  (let ((focus (desk-focus desk)))
    (and focus
         (find-if (lambda (row)
                    (and (eq (page-row-kind row) ':passage)
                         (= (page-row-definition row) (first focus))
                         (= (page-row-passage row) (rest focus))))
                  rows))))

(-> page-body-box (integer integer) (values integer integer))
(defun page-body-box (y height)
  "Return the top and the height of the part of a page in a box at Y of HEIGHT that scrolls."
  (values (+ y *page-header-height*)
          (max 0 (- height *page-header-height* *page-status-height*))))

(-> page--editor-span (integer integer &key (:top integer) (:bottom integer)) (values (or null integer) integer))
(defun page--editor-span (y height &key top bottom)
  "Return the top and the height of the part of a passage's row at Y of HEIGHT that the editor covers in a view from TOP to BOTTOM, or NIL and 0 when too little of the row is in view to write in.

A passage taller than the view is written in through the part of it that
shows, and the editor moves its own text as the caret moves."
  (let ((from (max (+ y *page-padding* -1) top))
        (to   (min (+ y height (- *page-padding*) 3) bottom)))
    (if (>= (- to from) 40)
        (values from (- to from))
        (values nil 0))))

(-> page-editor-box (desk &key (:x integer) (:y integer) (:width integer) (:height integer)) (or null list))
(defun page-editor-box (desk &key x y width height)
  "Return the (X Y WIDTH HEIGHT) the editor takes on a page in the given box, or NIL when no passage is being written in or too little of its row is in view."
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
  "Scroll the page in the given box so that the passage being written in is wholly in view, when the view can hold it."
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
  "Begin writing in the passage STEP places after the one being written in, or before it when STEP is negative, and return whether there was one. With none being written in, begin with the first."
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
  "Return the word the margin has for a passage of KIND."
  (ecase kind
    (:comment
     "comment")
    (:docstring
     "docstring")
    (:string
     "string")))

(-> page--unseen-words (string) (or null string))
(defun page--unseen-words (text)
  "Return what to say of TEXT when drawing it would show nothing, because it is empty or all blanks and line ends, or NIL when it has something to show."
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

(-> page--paint-heading (desk page-row &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-heading (desk row &key x y width)
  "Draw the heading of ROW's definition: its operator, its name, and at the right the line it starts on."
  (let* ((definition (nth (page-row-definition row) (proof-definitions (desk-proof desk))))
         (operator   (definition-operator definition))
         (plum       (text-style ':label-bold :color *plum*))
         (bold       (text-style ':label-bold))
         (meta       (text-style ':meta))
         (line       (format nil "line ~D" (definition-line definition)))
         (line-w     (text-width line meta))
         (height     *page-heading-height*)
         (left       (+ x *page-margin*))
         (right      (- (+ x width) *page-margin*)))
    (if (zerop (length operator))
        (paint-text-in "comments" plum :x left :y y :width 200 :height height)
        (let ((operator-w (text-width operator plum)))
          (paint-text-in operator plum :x left :y y :width operator-w :height height)
          (paint-text-in (definition-name definition) bold
                         :x      (+ left operator-w 10)
                         :y      y
                         :width  (max 0 (- right line-w 20 left operator-w 10))
                         :height height)))
    (paint-text-in line meta :x (- right line-w) :y y :width line-w :height height)
    (paint-line left (+ y height -3) :x2 right :y2 (+ y height -3)))
  nil)

(-> page--paint-passage (desk stage page-row &key (:x integer) (:y integer) (:text-x integer) (:text-width integer))
    null)
(defun page--paint-passage (desk stage row &key x y text-x text-width)
  "Draw ROW's passage with its top at Y: its kind in the margin at X and its words from TEXT-X, TEXT-WIDTH wide. A click puts the editor over it with the caret where the pointer is."
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
      ;; The editor lies over the words. Where too little of the row shows for it,
      ;; the words are drawn, and a click brings the passage back into view.
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
       (stage-region stage identifier
                     :x      (- text-x 7)
                     :y      y
                     :width  (+ text-width 14)
                     :height height
                     :cursor ':insert
                     :action (lambda ()
                               (desk-edit desk definition index
                                          :caret (paragraph-index-at
                                                  paragraph
                                                  (- (stage-pointer-x stage) text-x)
                                                  (- (stage-pointer-y stage) text-y))))))))
  nil)

(-> page--paint-header (desk stage &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-header (desk stage &key x y width)
  "Draw the band over the page: the file's name and folder, how many passages it has, the switches for the kinds of passage, and the button that marks the file completed."
  (let* ((name        (desk-name desk))
         (proof       (desk-proof desk))
         (completed-p (desk-completed-p desk name))
         (meta        (text-style ':meta))
         (left        (+ x *page-margin*))
         (right       (- (+ x width) *page-margin*))
         (button-w    (if completed-p 130 160))
         (kinds       (desk-kinds)))
    (paint-text-in (subseq name (length (shelf-folder name))) (text-style ':heading-2)
                   :x left :y (+ y 10) :width (max 0 (- right left button-w 20)) :height 30)
    (paint-text-in (format nil "~A~D ~:*~[texts~;text~:;texts~]~:[~; · edited~]"
                           (let ((folder (shelf-folder name)))
                             (if (zerop (length folder)) "" (format nil "~A · " folder)))
                           (proof-passage-count proof)
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
                  :tooltip   (if completed-p "Take the mark away" "Mark this file completed as it is now"))
    ;; The switches for what is listed, from the right.
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
                      :tooltip    (if (member kind kinds) "Leave these out" "List these")))))
    (paint-line x (+ y *page-header-height* -1) :x2 (+ x width) :y2 (+ y *page-header-height* -1)))
  nil)

(-> page--clock (integer) string)
(defun page--clock (time)
  "Return the universal TIME as the hour, minute and second of this place."
  (multiple-value-bind (second minute hour) (decode-universal-time time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))

(-> page--paint-status (desk &key (:x integer) (:y integer) (:width integer)) null)
(defun page--paint-status (desk &key x y width)
  "Draw the line under the page: what was not written, or when the file was last written, and the keys that move through the texts."
  (let* ((meta   (text-style ':meta))
         (left   (+ x *page-margin*))
         (keys   "Tab next text   Esc done   Ctrl+Enter completed, next file")
         (keys-w (text-width keys meta))
         (room   (max 0 (- width (* 2 *page-margin*) keys-w 20))))
    (paint-line x y :x2 (+ x width) :y2 y)
    (cond
      ((desk-notice desk)
       (paint-text-in (format nil "! ~A" (desk-notice desk)) (text-style ':meta :color *red*)
                      :x left :y y :width room :height *page-status-height*))
      ((desk-dirty-p desk)
       (paint-text-in "typing" meta :x left :y y :width room :height *page-status-height*))
      ((desk-saved-at desk)
       (paint-text-in (format nil "saved ~A" (page--clock (desk-saved-at desk))) (text-style ':meta :color *green*)
                      :x left :y y :width room :height *page-status-height*)))
    (when (> width (+ keys-w 260))
      (paint-text-in keys meta
                     :x      (- (+ x width) *page-margin* keys-w)
                     :y      y
                     :width  keys-w
                     :height *page-status-height*)))
  nil)

(-> page-paint (desk stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun page-paint (desk stage &key x y width height)
  "Draw the open file's page in the box, or say what to do when no file is open or the open one is left alone."
  (let ((proof  (desk-proof desk))
        (italic (text-style ':small :face ':serif-italic)))
    (cond
      ((null proof)
       (paint-text-in (if (desk-files desk)
                          "Choose a file on the left."
                          "This folder holds no Lisp source.")
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
               ;; A click beside the texts ends the writing, as a click outside a field does.
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
                      (paint-text-in (format nil "This file is left alone: ~A." (proof-problem proof)) italic
                                     :x      (+ x *page-margin*)
                                     :y      (+ body-y 16)
                                     :width  (- width (* 2 *page-margin*))
                                     :height 30))
                     ((null rows)
                      (paint-text-in (if (plusp (proof-passage-count proof))
                                         "Every kind of text in this file is switched off."
                                         "This file has no comments and no strings.")
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
                                (page--paint-heading desk row :x x :y top :width width)
                                (page--paint-passage desk stage row
                                                     :x          x
                                                     :y          top
                                                     :text-x     text-x
                                                     :text-width text-width)))))))))
               (when (> total body-height 40)
                 (let* ((thumb (max 24 (floor (* body-height body-height) total)))
                        (top   (+ body-y (floor (* (- body-height thumb) scroll) (max 1 maximum)))))
                   (paint-dither (- (+ x width) 6) top :width 4 :height thumb :step 2)))))
           (page--paint-status desk :x x :y (+ body-y body-height) :width width))))))
  nil)
