(in-package #:aui)

;;;; -- Shelf --


(defparameter *shelf-share* 3/10
  "The share of the window width that is used by the list of files.")

(defparameter *shelf-least-width* 300
  "The minimum width of the list of files in a window with room for it.")

(defparameter *shelf-indent* 14
  "The indentation of a row relative to the folder that contains it.")

(defparameter *shelf-row-height* 26
  "The height of one file or folder in the list.")

(defparameter *shelf-list-top* 104
  "The distance from the top edge of the list to the first row, which leaves room for the title and the field.")

(defparameter *shelf-footer-height* 44
  "The height of the footer of the list, which contains the count of completed files and the switch.")

(-> shelf-names (desk) list)
(defun shelf-names (desk)
  "Return the files to list. These are the files whose names contain the filter text, without the completed files if the person hides them. The open file is always listed."
  (let ((filter (string-trim " " (desk-filter desk)))
        (hide-p (desk-hide-completed-p)))
    (remove-if-not (lambda (name)
                     (or (equal name (desk-name desk))
                         (and (or (zerop (length filter))
                                  (search filter name :test #'char-equal))
                              (not (and hide-p (desk-completed-p desk name))))))
                   (desk-files desk))))

(defstruct shelf-folder
  "A folder in the checkout tree. PATH is relative to the root; FILES and FOLDERS contain its children."
  (path    ""  :type string)
  (files   nil :type list)
  (folders nil :type list))

(defstruct shelf-row
  "A file-list row. Folders also record whether they are folded and their file counts."
  (kind      ':file :type keyword)
  (name      ""     :type string)
  (label     ""     :type string)
  (depth     0      :type integer)
  (folded-p  nil    :type boolean)
  (files     0      :type integer)
  (completed 0      :type integer))

(-> shelf-width (integer) integer)
(defun shelf-width (width)
  "Return the file-list width for a window of WIDTH."
  (min (max *shelf-least-width* (round (* width *shelf-share*)))
       (floor (* width 2) 5)))

(-> shelf--tree (list) shelf-folder)
(defun shelf--tree (names)
  "Return the folder tree containing NAMES, preserving their order."
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
  "Return T if folded folders are drawn folded. While the person narrows the list by name, the folders are drawn unfolded, so that all matching files are in view."
  (zerop (length (string-trim " " (desk-filter desk)))))

(-> shelf-rows (desk) list)
(defun shelf-rows (desk)
  "Return file-list rows in display order. Folders with one nested folder share a row with it."
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
               ;; BASE is the length of the part of the path of NODE that is shown
               ;; in the rows above.
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
  "Open the file STEP places after the open file in the list, or before it if STEP is negative. If no file is open, open the first file."
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
  "Open the first file after the open file that is not completed. If there is none below, continue from the top of the list."
  (let* ((names (shelf-names desk))
         (index (or (position (desk-name desk) names :test #'equal) -1))
         (after (append (nthcdr (1+ index) names) (subseq names 0 (max 0 index))))
         (next  (find-if-not (lambda (name) (desk-completed-p desk name)) after)))
    (when next
      (desk-open-file desk next)))
  nil)

(-> shelf--row-left (shelf-row integer) integer)
(defun shelf--row-left (row x)
  "Return the left edge of the content of ROW in a list whose left edge is X. Deeper rows start further right."
  (+ x 16 (* *shelf-indent* (shelf-row-depth row))))

(-> shelf--paint-guides (shelf-row integer integer) null)
(defun shelf--paint-guides (row x y)
  "Draw a faint line along the height of ROW under the mark of each folder that contains ROW. With these lines, the rows of one folder are seen as a group."
  (dotimes (level (shelf-row-depth row))
    (paint-dither (+ x 20 (* *shelf-indent* level)) y :width 1 :height *shelf-row-height* :step 2))
  nil)

(-> shelf--paint-folder (desk stage shelf-row &key (:x integer) (:y integer) (:width integer)) null)
(defun shelf--paint-folder (desk stage row &key x y width)
  "Draw one folder with the fold mark, the name, and the number of completed files in the folder. The person can click the folder to fold or unfold it."
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
    ;; While the list is narrowed by name, no folders are folded, so there is no click region.
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
  "Draw one file with the box, the name, and the edited label if the file is edited. The box is checked if the file is completed."
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
    ;; The box is over the row, so a click on the box marks the file and does not open it.
    (stage-region stage box
                  :x       (- box-x 6)
                  :y       y
                  :width   26
                  :height  height
                  :action  (lambda () (desk-toggle-completed desk name))
                  :tooltip (if completed-p "Remove the mark" "Mark completed")))
  nil)

(-> shelf-paint (desk stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun shelf-paint (desk stage &key x y width height)
  "Draw the list of files in the box: the name of the checkout, the tree of folders and files, and a footer with the number of completed files and the switch to hide them."
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
    ;; A file opened with a key can be outside the visible part of the list.
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
        (paint-text-in (if (zerop files) "no Lisp source here" "no matching files") (text-style ':meta :face ':mono-italic)
                       :x (+ x 16) :y (+ list-y 6) :width (- width 32) :height 20)))
    (when (> total list-h 40)
      (let* ((thumb (max 24 (floor (* list-h list-h) total)))
             (top   (+ list-y (floor (* (- list-h thumb) scroll) (max 1 maximum)))))
        (paint-dither (- (+ x width) 5) top :width 4 :height thumb :step 2)))
    (let* ((foot-y (- (+ y height) *shelf-footer-height*))
           (label  "Hide completed")
           (hide-p (desk-hide-completed-p))
           (switch (+ 22 (text-width label meta) 16))
           (right  (- (+ x width) 8))
           (left   (- right switch))
           (hot-p  (stage-hot-p stage ':hide-completed)))
      (paint-line x foot-y :x2 (+ x width) :y2 foot-y)
      (paint-text-in (format nil "~D of ~D completed" (desk-completed-count desk) files) meta
                     :x (+ x 16) :y foot-y :width (max 0 (- left x 24)) :height *shelf-footer-height*)
      ;; The box is checked while completed files are hidden, in the same way
      ;; as the box of a completed file.
      (flet ((content ()
               (let ((box-y (+ foot-y (floor (- *shelf-footer-height* 13) 2))))
                 (paint-frame (+ left 8) box-y :width 13 :height 13)
                 (when hide-p
                   (paint-mark ':check (+ left 7) (- box-y 2) :size 16)))
               (paint-text-in label meta
                              :x (+ left 30) :y foot-y :width (- switch 30) :height *shelf-footer-height*)))
        (if hot-p
            (with-inversion (left (+ foot-y 8) switch (- *shelf-footer-height* 16))
              (content))
            (content)))
      (stage-region stage ':hide-completed
                    :x       left
                    :y       (+ foot-y 8)
                    :width   switch
                    :height  (- *shelf-footer-height* 16)
                    :action  (lambda ()
                               (setf (preference ':hide-completed) (not hide-p))
                               (desk-redraw desk))
                    :tooltip (if hide-p "Show completed files" "Hide completed files"))))
  nil)
