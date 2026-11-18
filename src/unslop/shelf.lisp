(in-package #:aui)

;;;; -- Shelf --

;;; The list of the checkout's source files at the window's left, as a tree
;;; of the folders they lie in. A folder stands ahead of what lies in it, its
;;; own files first, and a click folds it away or brings it back. Each file
;;; has a box that is checked when the file is completed, and the word edited
;;; behind it when it differs from what was last committed. The field above
;;; the list narrows it by name, and the switch at its foot leaves the
;;; completed files out.

(defparameter *shelf-share* 3/10
  "The part of the window's width the list of files takes.")

(defparameter *shelf-least-width* 300
  "The narrowest the list of files is in a window with room for it.")

(defparameter *shelf-indent* 14
  "How far a row stands to the right of the folder it lies in.")

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

(defstruct shelf-folder
  "A folder of the checkout in the tree of its files, as its PATH from the root with its closing slash, empty for the root itself, the names of its own FILES and the FOLDERS inside it."
  (path    ""  :type string)
  (files   nil :type list)
  (folders nil :type list))

(defstruct shelf-row
  "One row of the list of files, as its KIND, a folder or a file, its NAME from the checkout's root, the LABEL it shows and its DEPTH in the tree. A folder also says whether it is FOLDED-P, how many FILES lie in it and below, and how many of those are COMPLETED."
  (kind      ':file :type keyword)
  (name      ""     :type string)
  (label     ""     :type string)
  (depth     0      :type integer)
  (folded-p  nil    :type boolean)
  (files     0      :type integer)
  (completed 0      :type integer))

(-> shelf-width (integer) integer)
(defun shelf-width (width)
  "Return how wide the list of files is in a window WIDTH wide: its share of the window, not narrower than its least width, and never more than two fifths of a narrow window."
  (min (max *shelf-least-width* (round (* width *shelf-share*)))
       (floor (* width 2) 5)))

(-> shelf--tree (list) shelf-folder)
(defun shelf--tree (names)
  "Return the root of the tree of folders the files NAMES lie in. NAMES are in the order of the list, and every folder holds its files and its folders in that order."
  (let ((root  (make-shelf-folder))
        (table (make-hash-table :test #'equal)))
    (setf (gethash "" table) root)
    (labels ((folder (path)
               (or (gethash path table)
                   (let ((node   (make-shelf-folder :path path))
                         (parent (folder (checkout-folder (subseq path 0 (1- (length path)))))))
                     (push node (shelf-folder-folders parent))
                     (setf (gethash path table) node)))))
      (dolist (name names)
        (push name (shelf-folder-files (folder (checkout-folder name))))))
    (loop for node being the hash-values of table
          do (setf (shelf-folder-files node)   (nreverse (shelf-folder-files node))
                   (shelf-folder-folders node) (nreverse (shelf-folder-folders node))))
    root))

(-> shelf-folding-p (desk) boolean)
(defun shelf-folding-p (desk)
  "Return T when folded folders are drawn folded. They are not while the person narrows the list by name, so that every file that matches is in view."
  (zerop (length (string-trim " " (desk-filter desk)))))

(-> shelf-rows (desk) list)
(defun shelf-rows (desk)
  "Return the rows of the list in order, as SHELF-ROWs: the files at the root, then each folder ahead of what lies in it, its own files first.

A folder is listed when a file to list lies in it or below, and what lies in
a folded folder is left out. A folder that holds nothing but one other folder
is listed with it as one row, so a chain of such folders takes one row."
  (let ((listed    (make-hash-table :test #'equal))
        (folding-p (shelf-folding-p desk)))
    (dolist (name (shelf-names desk))
      (setf (gethash name listed) t))
    (labels ((file-rows (node depth)
               (loop with start = (length (shelf-folder-path node))
                     for name in (shelf-folder-files node)
                     when (gethash name listed)
                       collect (make-shelf-row :kind ':file :name name :label (subseq name start) :depth depth)))

             (folder-rows (node depth base)
               ;; BASE is how much of NODE's path the rows above it say already.
               (let ((files   (shelf-folder-files node))
                     (folders (shelf-folder-folders node))
                     (path    (shelf-folder-path node)))
                 (if (and (null files) folders (null (rest folders)))
                     (folder-rows (first folders) depth base)
                     (let ((inner     (list (file-rows node (1+ depth))))
                           (count     (length files))
                           (completed (count-if (lambda (name) (desk-completed-p desk name)) files))
                           (folded-p  (and folding-p (desk-folded-p desk path))))
                       (dolist (folder folders)
                         (multiple-value-bind (rows below done) (folder-rows folder (1+ depth) (length path))
                           (push rows inner)
                           (incf count below)
                           (incf completed done)))
                       (let ((inner (apply #'append (nreverse inner))))
                         (values (and inner
                                      (cons (make-shelf-row :kind      ':folder
                                                            :name      path
                                                            :label     (subseq path base)
                                                            :depth     depth
                                                            :folded-p  folded-p
                                                            :files     count
                                                            :completed completed)
                                            (and (not folded-p) inner)))
                                 count
                                 completed)))))))
      (let ((root (shelf--tree (desk-files desk))))
        (append (file-rows root 0)
                (loop for folder in (shelf-folder-folders root)
                      append (values (folder-rows folder 0 0))))))))

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

(-> shelf--row-left (shelf-row integer) integer)
(defun shelf--row-left (row x)
  "Return where what ROW shows begins in a list whose left edge is X: further right the deeper the row lies in the tree."
  (+ x 16 (* *shelf-indent* (shelf-row-depth row))))

(-> shelf--paint-guides (shelf-row integer integer) null)
(defun shelf--paint-guides (row x y)
  "Draw a faint line down ROW's height under the mark of each folder ROW lies in, so that what lies in one folder is seen to belong together."
  (dotimes (level (shelf-row-depth row))
    (paint-dither (+ x 20 (* *shelf-indent* level)) y :width 1 :height *shelf-row-height* :step 2))
  nil)

(-> shelf--paint-folder (desk stage shelf-row &key (:x integer) (:y integer) (:width integer)) null)
(defun shelf--paint-folder (desk stage row &key x y width)
  "Draw one folder: the mark that says whether it is folded, its name, and how many of the files in it are completed. A click folds it or unfolds it."
  (let* ((height  *shelf-row-height*)
         (path    (shelf-row-name row))
         (region  (list ':shelf-folder path))
         (style   (text-style ':meta))
         (left    (shelf--row-left row x))
         (count   (format nil "~D of ~D" (shelf-row-completed row) (shelf-row-files row)))
         (count-w (text-width count style))
         (right   (- (+ x width) 10))
         (text-x  (+ left 16))
         (text-w  (max 0 (- right count-w 10 text-x))))
    (flet ((content ()
             (paint-mark (if (shelf-row-folded-p row) ':closed ':open)
                         (- left 2)
                         (+ y (floor (- height 12) 2))
                         :size 12)
             (paint-text-in (text-fit-tail (shelf-row-label row) style text-w) style
                            :x      text-x
                            :y      y
                            :width  text-w
                            :height height)
             (paint-text-in count style
                            :x      (- right count-w)
                            :y      y
                            :width  count-w
                            :height height)))
      (if (stage-hot-p stage region)
          (with-inversion (x y width height)
            (content))
          (content)))
    ;; While the list is narrowed by name nothing is folded, so there is nothing to click for.
    (when (shelf-folding-p desk)
      (stage-region stage region
                    :x       x
                    :y       y
                    :width   width
                    :height  height
                    :action  (lambda () (desk-toggle-folder desk path))
                    :tooltip (format nil "~A  ~D of ~D files completed"
                                     path (shelf-row-completed row) (shelf-row-files row)))))
  nil)

(-> shelf--paint-file (desk stage shelf-row &key (:x integer) (:y integer) (:width integer)) null)
(defun shelf--paint-file (desk stage file &key x y width)
  "Draw one file: its box, checked when the file is completed, its name, and that it is edited."
  (let* ((height      *shelf-row-height*)
         (name        (shelf-row-name file))
         (row         (list ':shelf-file name))
         (box         (list ':shelf-check name))
         (open-p      (equal name (desk-name desk)))
         (completed-p (desk-completed-p desk name))
         (edited-p    (desk-edited-p desk name))
         (style       (text-style (if completed-p ':label ':label-bold)))
         (amber       (text-style ':meta :color *amber*))
         (edited-w    (if edited-p (+ 10 (text-width "edited" amber)) 0))
         (box-x       (shelf--row-left file x))
         (box-y       (+ y (floor (- height 13) 2)))
         (text-x      (+ box-x 24))
         (right       (- (+ x width) 10)))
    (flet ((content ()
             (paint-frame box-x box-y :width 13 :height 13)
             (when completed-p
               (paint-mark ':check (- box-x 1) (- box-y 2) :size 16))
             (paint-text-in (text-fit-tail (shelf-row-label file) style
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
  "Draw the list of files in the box: the checkout's name, the tree of folders with their files, and at the foot how many files are completed and the switch that hides those."
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
      (let ((index (position-if (lambda (row)
                                  (and (eq ':file (shelf-row-kind row))
                                       (equal (desk-name desk) (shelf-row-name row))))
                                rows)))
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
      (loop for row in rows
            for row-y from (- list-y scroll) by *shelf-row-height*
            do (when (and (< row-y (+ list-y list-h)) (> (+ row-y *shelf-row-height*) list-y))
                 (shelf--paint-guides row x row-y)
                 (ecase (shelf-row-kind row)
                   (:folder
                    (shelf--paint-folder desk stage row :x x :y row-y :width width))
                   (:file
                    (shelf--paint-file desk stage row :x x :y row-y :width width)))))
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
