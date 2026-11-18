(in-package #:aui)

;;;; -- Shelf --

;;; The list of the checkout's source files at the window's left, under the
;;; folders they lie in. Each file has a box that is checked when the file is
;;; completed, and the word edited behind it when it differs from what was
;;; last committed. The field above the list narrows it by name, and the
;;; switch at its foot leaves the completed files out.

(defparameter *shelf-width* 310
  "The width of the list of files.")

(defparameter *shelf-row-height* 26
  "The height of one file or folder in the list.")

(defparameter *shelf-list-top* 104
  "How far below the list's top edge its rows begin, under the title and the field.")

(defparameter *shelf-footer-height* 44
  "The room at the list's foot for the count of completed files and the switch.")

(-> shelf-names (desk) list)
(defun shelf-names (desk)
  "Return the files to list: those whose names hold what is in the filter, without the completed ones when the person hides them. The open file is listed whatever it is."
  (let ((filter (string-trim " " (desk-filter desk)))
        (hide-p (desk-hide-completed-p)))
    (remove-if-not (lambda (name)
                     (or (equal name (desk-name desk))
                         (and (or (zerop (length filter))
                                  (search filter name :test #'char-equal))
                              (not (and hide-p (desk-completed-p desk name))))))
                   (desk-files desk))))

(-> shelf-rows (desk) list)
(defun shelf-rows (desk)
  "Return the rows of the list in order: (:FOLDER . FOLDER) before the files of each folder, and (:FILE . NAME) for each file."
  (let ((rows   nil)
        (folder ':none))
    (dolist (name (shelf-names desk))
      (let ((own (checkout-folder name)))
        (unless (equal own folder)
          (setf folder own)
          (push (cons ':folder own) rows))
        (push (cons ':file name) rows)))
    (nreverse rows)))

(-> shelf-step (desk integer) null)
(defun shelf-step (desk step)
  "Open the file STEP places after the open one in the list, or before it when STEP is negative. With no file open, open the first."
  (let* ((names (shelf-names desk))
         (index (position (desk-name desk) names :test #'equal))
         (next  (if index
                    (nth (max 0 (min (1- (length names)) (+ index step))) names)
                    (first names))))
    (when (and next (not (equal next (desk-name desk))))
      (desk-open-file desk next)))
  nil)

(-> shelf-open-next-open (desk) null)
(defun shelf-open-next-open (desk)
  "Open the first file after the open one that is not completed, going round to the top of the list when there is none below."
  (let* ((names (shelf-names desk))
         (index (or (position (desk-name desk) names :test #'equal) -1))
         (after (append (nthcdr (1+ index) names) (subseq names 0 (max 0 index))))
         (next  (find-if-not (lambda (name) (desk-completed-p desk name)) after)))
    (when next
      (desk-open-file desk next)))
  nil)

(-> shelf--paint-file (desk stage string &key (:x integer) (:y integer) (:width integer)) null)
(defun shelf--paint-file (desk stage name &key x y width)
  "Draw one file: its box, checked when the file is completed, its name, and that it is edited."
  (let* ((height      *shelf-row-height*)
         (row         (list ':shelf-file name))
         (box         (list ':shelf-check name))
         (open-p      (equal name (desk-name desk)))
         (completed-p (desk-completed-p desk name))
         (edited-p    (desk-edited-p desk name))
         (style       (text-style (if completed-p ':label ':label-bold)))
         (amber       (text-style ':meta :color *amber*))
         (edited-w    (if edited-p (+ 10 (text-width "edited" amber)) 0))
         (box-x       (+ x 16))
         (box-y       (+ y (floor (- height 13) 2)))
         (text-x      (+ x 40))
         (right       (- (+ x width) 10)))
    (flet ((content ()
             (paint-frame box-x box-y :width 13 :height 13)
             (when completed-p
               (paint-mark ':check (- box-x 1) (- box-y 2) :size 16))
             (paint-text-in (text-fit-tail (subseq name (length (checkout-folder name))) style
                                           (max 0 (- right edited-w text-x)))
                            style
                            :x      text-x
                            :y      y
                            :width  (max 0 (- right edited-w text-x))
                            :height height)
             (when edited-p
               (paint-text-in "edited" amber
                              :x      (- right edited-w -10)
                              :y      y
                              :width  (- edited-w 10)
                              :height height))))
      (if (or open-p (stage-hot-p stage row) (stage-hot-p stage box))
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage row
                  :x       x
                  :y       y
                  :width   width
                  :height  height
                  :action  (lambda () (desk-open-file desk name))
                  :tooltip name)
    ;; The box lies over the row, so a click on it marks the file and does not open it.
    (stage-region stage box
                  :x       (- box-x 6)
                  :y       y
                  :width   26
                  :height  height
                  :action  (lambda () (desk-toggle-completed desk name))
                  :tooltip (if completed-p "Take the mark away" "Mark completed")))
  nil)

(-> shelf-paint (desk stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun shelf-paint (desk stage &key x y width height)
  "Draw the list of files in the box: the checkout's name, the folders with their files, and at the foot how many files are completed and the switch that hides those."
  (let* ((checkout (desk-checkout desk))
         (root     (checkout-root checkout))
         (rows     (shelf-rows desk))
         (list-y   (+ y *shelf-list-top*))
         (list-h   (max 0 (- height *shelf-list-top* *shelf-footer-height*)))
         (total    (* *shelf-row-height* (length rows)))
         (maximum  (max 0 (- total list-h)))
         (scroll   (max 0 (min (desk-shelf-scroll desk) maximum)))
         (meta     (text-style ':meta))
         (files    (length (desk-files desk))))
    ;; A file opened with a key may lie outside what the list shows.
    (when (shiftf (desk-shelf-reveal-p desk) nil)
      (let ((index (position (cons ':file (desk-name desk)) rows :test #'equal)))
        (when index
          (let ((top (* index *shelf-row-height*)))
            (cond
              ((< top scroll)
               (setf scroll (max 0 (- top *shelf-row-height*))))
              ((> (+ top *shelf-row-height*) (+ scroll list-h))
               (setf scroll (min maximum (- (+ top (* 2 *shelf-row-height*)) list-h)))))))))
    (setf (desk-shelf-scroll desk) scroll)
    (paint-text-in (let ((leaf (first (last (pathname-directory root)))))
                     (if (stringp leaf) leaf "/"))
                   (text-style ':heading-2)
                   :x (+ x 16) :y (+ y 10) :width (- width 32) :height 30)
    (paint-text-in (text-fit-tail (string-right-trim "/" (uiop:native-namestring root)) meta (- width 32))
                   meta
                   :x (+ x 16) :y (+ y 38) :width (- width 32) :height 18)
    (paint-frame (+ x 14) (+ y 64) :width (- width 28) :height 30)
    (stage-region stage ':shelf
                  :x      x
                  :y      list-y
                  :width  width
                  :height list-h
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (desk-shelf-scroll desk)
                                  (max 0 (min maximum (+ (desk-shelf-scroll desk) (* vertical 52))))))
                  :cursor ':default)
    (with-clip (x list-y width list-h)
      (loop for (kind . name) in rows
            for row-y from (- list-y scroll) by *shelf-row-height*
            do (when (and (< row-y (+ list-y list-h)) (> (+ row-y *shelf-row-height*) list-y))
                 (ecase kind
                   (:folder
                    (paint-text-in (if (zerop (length name)) "./" name) meta
                                   :x      (+ x 16)
                                   :y      (+ row-y 4)
                                   :width  (- width 32)
                                   :height (- *shelf-row-height* 4)))
                   (:file
                    (shelf--paint-file desk stage name :x x :y row-y :width width)))))
      (when (null rows)
        (paint-text-in (if (zerop files) "no Lisp source here" "no file matches") (text-style ':meta :face ':mono-italic)
                       :x (+ x 16) :y (+ list-y 6) :width (- width 32) :height 20)))
    (when (> total list-h 40)
      (let* ((thumb (max 24 (floor (* list-h list-h) total)))
             (top   (+ list-y (floor (* (- list-h thumb) scroll) (max 1 maximum)))))
        (paint-dither (- (+ x width) 5) top :width 4 :height thumb :step 2)))
    (let* ((foot-y (- (+ y height) *shelf-footer-height*))
           (label  (if (desk-hide-completed-p) "Show completed" "Hide completed")))
      (paint-line x foot-y :x2 (+ x width) :y2 foot-y)
      (paint-text-in (format nil "~D of ~D completed" (desk-completed-count desk) files) meta
                     :x (+ x 16) :y foot-y :width (- width 32 (chip-width label)) :height *shelf-footer-height*)
      (paint-chip stage ':hide-completed label
                  :x      (- (+ x width) (chip-width label) 8)
                  :y      (+ foot-y (floor (- *shelf-footer-height* 24) 2))
                  :action (lambda ()
                            (setf (preference ':hide-completed) (not (desk-hide-completed-p)))
                            (desk-redraw desk)))))
  nil)
