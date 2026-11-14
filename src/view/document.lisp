(in-package #:aui)

;;;; -- Document View --

;;; The transcript as a scrolling column of blocks. Each entry is typeset
;;; once and again only when its revision moves; drawing touches only what is
;;; in view, so a long history costs what its visible part costs. A drag
;;; selects text across entries, and the selection is copied when released.

(defparameter *document-measure* 760
  "The widest the reading column gets, in FLTK units: about seventy characters of the body face.")

(defparameter *document-margin* 28
  "The least space kept at each side of the reading column.")

(defparameter *document-wheel-step* 54
  "How far one wheel step scrolls.")

(defstruct item
  "One entry as it is laid out, with its blocks, where it starts in the column, and its selectable leaves."
  (entry    nil)
  (revision -1  :type integer)
  (blocks   nil :type list)
  (top      0   :type integer)
  (height   0   :type integer)
  (leaves   nil :type list))

(defclass document ()
  ((transcript
    :initarg       :transcript
    :accessor      document-transcript
    :type          transcript
    :documentation "The transcript shown.")
   (items
    :initform      (make-array 64 :adjustable t :fill-pointer 0)
    :accessor      document--items
    :type          vector
    :documentation "The laid-out entries, in the transcript's order.")
   (laid-revision
    :initform      -1
    :accessor      document--laid-revision
    :type          integer
    :documentation "The transcript revision the items were last brought up to.")
   (column-width
    :initform      0
    :accessor      document--column-width
    :type          integer
    :documentation "The width the entries are laid out to.")
   (content-height
    :initform      0
    :accessor      document-content-height
    :type          integer
    :documentation "The height of every entry together.")
   (scroll
    :initform      0
    :accessor      document-scroll
    :type          integer
    :documentation "How far the column is scrolled from its top.")
   (following-p
    :initform      t
    :accessor      document-following-p
    :type          boolean
    :documentation "Whether the view stays at the newest entry as the transcript grows.")
   (viewport
    :initform      (list 0 0 0 0)
    :accessor      document--viewport
    :type          list
    :documentation "The box (X Y WIDTH HEIGHT) the document was last painted in.")
   (anchor
    :initform      nil
    :accessor      document--anchor
    :documentation "Where a selection began, as (ITEM-INDEX LEAF-INDEX CHARACTER), or NIL.")
   (focus
    :initform      nil
    :accessor      document--focus
    :documentation "Where the selection currently ends, in the anchor's form, or NIL.")
   (more-p
    :initform      nil
    :accessor      document-more-p
    :type          boolean
    :documentation "Whether earlier history exists beyond the first entry.")
   (load-earlier
    :initarg       :load-earlier
    :initform      nil
    :accessor      document-load-earlier
    :type          (or null function)
    :documentation "Called to fetch the page of history before the first entry."))
  (:documentation "A scrolling, selectable view of one transcript."))

(-> make-document (transcript &key (:load-earlier (or null function))) document)
(defun make-document (transcript &key load-earlier)
  "Return a document view of TRANSCRIPT."
  (make-instance 'document :transcript transcript :load-earlier load-earlier))


;;;; -- Layout --

(-> document--stack (list integer) (values integer list))
(defun document--stack (blocks width)
  "Lay BLOCKS out to WIDTH, one under another with margins collapsed, and return the height and (BLOCK . TOP) pairs."
  (let ((top      0)
        (previous 0)
        (placed   nil))
    (loop for block in blocks
          for first-p = t then nil
          do (block-layout block width)
             (incf top (if first-p (block-margin-top block) (max previous (block-margin-top block))))
             (push (cons block top) placed)
             (incf top (block-height block))
             (setf previous (block-margin-bottom block)))
    (values (+ top previous) (nreverse placed))))

(-> document--lay-item (document item) null)
(defun document--lay-item (document item)
  "Typeset ITEM's entry again and lay it out to the column."
  (let* ((entry      (item-entry item))
         (transcript (document-transcript document))
         (blocks     (entry-blocks entry
                                   (lambda ()
                                     (setf (entry-expanded-p entry) (not (entry-expanded-p entry)))
                                     (transcript-touch transcript entry)))))
    (multiple-value-bind (height placed) (document--stack blocks (document--column-width document))
      (setf (item-blocks item)   placed
            (item-height item)   height
            (item-revision item) (entry-revision entry)
            (item-leaves item)   (loop for (block . top) in placed
                                       append (loop for (paragraph leaf-x leaf-y) in (block-leaves block)
                                                    collect (list paragraph leaf-x (+ top leaf-y)))))))
  nil)

(-> document-sync (document integer) null)
(defun document-sync (document width)
  "Bring the laid-out items up to the transcript and to the column WIDTH wide.

Entries whose revision did not move keep their layout. When entries arrive
in front, as an earlier page of history does, the view stays on what it
showed."
  (let* ((transcript (document-transcript document))
         (entries    (transcript-entries transcript))
         (items      (document--items document))
         (resized-p  (/= width (document--column-width document))))
    (when (or resized-p (/= (transcript-revision transcript) (document--laid-revision document)))
      (setf (document--column-width document) width)
      (let* ((known    (make-hash-table :test #'eq))
             (first    (and (plusp (length items)) (item-entry (aref items 0))))
             (old-top  (and first (item-top (aref items 0))))
             (selected (document--anchor document)))
        (loop for item across items
              do (setf (gethash (item-entry item) known) item))
        (setf (fill-pointer items) 0)
        (loop for entry across entries
              do (let ((item (or (gethash entry known) (make-item :entry entry))))
                   (when (or resized-p (/= (item-revision item) (entry-revision entry)))
                     (document--lay-item document item))
                   (vector-push-extend item items)))
        (let ((top 0))
          (loop for item across items
                do (setf (item-top item) top)
                   (incf top (item-height item)))
          (setf (document-content-height document) top))
        ;; Keep the first entry that was shown where it was when a page went in front.
        (when (and first old-top (not (document-following-p document)))
          (let ((moved (find first items :key #'item-entry)))
            (when (and moved (/= (item-top moved) old-top))
              (incf (document-scroll document) (- (item-top moved) old-top)))))
        (when (and selected (not (eq (aref entries 0) first)))
          (setf (document--anchor document) nil
                (document--focus document)  nil)))
      (setf (document--laid-revision document) (transcript-revision transcript))))
  nil)

(-> document--maximum-scroll (document) integer)
(defun document--maximum-scroll (document)
  "Return the furthest DOCUMENT can scroll: its content less its viewport, with some air below the last entry."
  (max 0 (- (+ (document-content-height document) 32) (fourth (document--viewport document)))))

(-> document-scroll-by (document integer) null)
(defun document-scroll-by (document amount)
  "Scroll DOCUMENT by AMOUNT units, down when positive, and follow the newest entry again at the bottom."
  (let* ((maximum (document--maximum-scroll document))
         (target  (max 0 (min maximum (+ (document-scroll document) amount)))))
    (setf (document-scroll document)      target
          (document-following-p document) (>= target maximum))
    (when (and (<= target 0) (document-more-p document) (document-load-earlier document) (minusp amount))
      (funcall (document-load-earlier document))))
  nil)

(-> document-scroll-to-end (document) null)
(defun document-scroll-to-end (document)
  "Show DOCUMENT's newest entry and keep following it."
  (setf (document-following-p document) t)
  nil)


;;;; -- Selection --

(-> document--position< (list list) boolean)
(defun document--position< (first second)
  "Return T when selection position FIRST comes before SECOND in reading order."
  (loop for a in first
        for b in second
        do (cond
             ((< a b)
              (return t))
             ((> a b)
              (return nil)))
        finally (return nil)))

(-> document--item-at (document integer) (or null integer))
(defun document--item-at (document y)
  "Return the index of the item containing the column height Y, the nearest one when Y is between or beyond items."
  (let ((items (document--items document)))
    (when (plusp (length items))
      (let ((low  0)
            (high (1- (length items))))
        (loop while (< low high)
              do (let ((middle (floor (+ low high 1) 2)))
                   (if (<= (item-top (aref items middle)) y)
                       (setf low middle)
                       (setf high (1- middle)))))
        low))))

(-> document--position-at (document integer integer) (or null list))
(defun document--position-at (document x y)
  "Return the selection position nearest the stage point X, Y, or NIL when the document has no text."
  (destructuring-bind (view-x view-y view-width view-height) (document--viewport document)
    (declare (ignore view-height))
    (let* ((column-x (+ view-x (floor (- view-width (document--column-width document)) 2)))
           (local-x  (- x column-x))
           (local-y  (+ (- y view-y) (document-scroll document)))
           (index    (document--item-at document local-y)))
      (when index
        (let* ((item          (aref (document--items document) index))
               (item-y        (- local-y (item-top item)))
               (best          nil)
               (best-distance nil))
          (loop for (paragraph leaf-x leaf-y) in (item-leaves item)
                for leaf-index from 0
                do (let* ((bottom   (+ leaf-y (paragraph-height paragraph)))
                          (distance (cond
                                      ((< item-y leaf-y)
                                       (- leaf-y item-y))
                                      ((>= item-y bottom)
                                       (- item-y bottom -1))
                                      (t
                                       0))))
                     (when (or (null best-distance) (< distance best-distance))
                       (setf best-distance distance
                             best          (list index leaf-index
                                                 (paragraph-index-at paragraph (- local-x leaf-x)
                                                                     (- item-y leaf-y)))))))
          best)))))

(-> document--selection-range (document) (values (or null list) (or null list)))
(defun document--selection-range (document)
  "Return the selection's two ends in reading order, or NIL and NIL when nothing is selected."
  (let ((anchor (document--anchor document))
        (focus  (document--focus document)))
    (cond
      ((or (null anchor) (null focus) (equal anchor focus))
       (values nil nil))
      ((document--position< anchor focus)
       (values anchor focus))
      (t
       (values focus anchor)))))

(-> document--selection-table (document) (or null hash-table))
(defun document--selection-table (document)
  "Return a table from each selected paragraph to its selected (START . END), or NIL when nothing is selected."
  (multiple-value-bind (start end) (document--selection-range document)
    (when start
      (let ((table (make-hash-table :test #'eq))
            (items (document--items document)))
        (loop for index from (first start) to (min (first end) (1- (length items)))
              do (loop for (paragraph) in (item-leaves (aref items index))
                       for leaf-index from 0
                       do (let* ((position (list index leaf-index))
                                 (from     (cond
                                             ((equal position (butlast start))
                                              (third start))
                                             ((document--position< position start)
                                              nil)
                                             (t
                                              0)))
                                 (to       (cond
                                             ((equal position (butlast end))
                                              (third end))
                                             ((document--position< position end)
                                              (length (paragraph-text paragraph)))
                                             (t
                                              nil))))
                            (when (and from to (< from to))
                              (setf (gethash paragraph table) (cons from to))))))
        table))))

(-> document-selected-text (document) (or null string))
(defun document-selected-text (document)
  "Return the selected text, paragraphs on their own lines, or NIL when nothing is selected."
  (let ((table (document--selection-table document)))
    (when table
      (let ((parts nil))
        (loop for item across (document--items document)
              do (loop for (paragraph) in (item-leaves item)
                       do (let ((range (gethash paragraph table)))
                            (when range
                              (push (subseq (paragraph-text paragraph) (first range) (rest range)) parts)))))
        (and parts (format nil "~{~A~^~%~}" (nreverse parts)))))))

(-> document-clear-selection (document) null)
(defun document-clear-selection (document)
  "Select nothing."
  (setf (document--anchor document) nil
        (document--focus document)  nil)
  nil)

(-> document--drag (document keyword &key (:x integer) (:y integer)) null)
(defun document--drag (document phase &key (x 0) (y 0))
  "Follow a selecting drag at X, Y through PHASE; a finished selection is copied."
  (ecase phase
    (:start
     (setf (document--anchor document) (document--position-at document x y)
           (document--focus document)  nil))
    (:move
     (setf (document--focus document) (document--position-at document x y))
     ;; Dragging past an edge scrolls toward it.
     (destructuring-bind (view-x view-y view-width view-height) (document--viewport document)
       (declare (ignore view-x view-width))
       (cond
         ((< y (+ view-y 12))
          (document-scroll-by document (- *document-wheel-step*)))
         ((> y (+ view-y view-height -12))
          (document-scroll-by document *document-wheel-step*)))))
    (:end
     (let ((text (document-selected-text document)))
       (when text
         (lightfast:copy-text text)))))
  nil)


;;;; -- Drawing --

(-> document-paint (document stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun document-paint (document stage &key x y width height)
  "Draw DOCUMENT in the box X, Y, WIDTH, HEIGHT of STAGE."
  (let* ((column (max 120 (min *document-measure* (- width (* 2 *document-margin*)))))
         (left   (+ x (floor (- width column) 2))))
    (setf (document--viewport document) (list x y width height))
    (document-sync document column)
    (when (document-following-p document)
      (setf (document-scroll document) (document--maximum-scroll document)))
    (setf (document-scroll document)
          (max 0 (min (document-scroll document) (document--maximum-scroll document))))
    (stage-region stage ':document
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (document-scroll-by document (* vertical *document-wheel-step*)))
                  :drag   (lambda (phase drag-x drag-y)
                            (document--drag document phase :x drag-x :y drag-y))
                  :cursor ':insert)
    (lightfast:with-clip (x y width height)
      (let* ((scroll            (document-scroll document))
             (items             (document--items document))
             (first             (or (document--item-at document scroll) 0))
             (*paint-stage*     stage)
             (*paint-top*       y)
             (*paint-bottom*    (+ y height))
             (*paint-selection* (document--selection-table document)))
        (loop for index from first below (length items)
              do (let* ((item     (aref items index))
                        (item-top (+ y (- (item-top item) scroll))))
                   (when (>= item-top (+ y height))
                     (return))
                   (loop for (block . top) in (item-blocks item)
                         do (let ((block-top (+ item-top top)))
                              (when (and (< block-top (+ y height))
                                         (> (+ block-top (block-height block)) y))
                                (block-paint block left block-top))))))))
    (document--paint-scrollbar document stage :x x :y y :width width :height height))
  nil)

(-> document--paint-scrollbar
    (document stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun document--paint-scrollbar (document stage &key (x 0) (y 0) (width 0) (height 0))
  "Draw a thin thumb at the right edge when the content is taller than the view, and let it be dragged."
  (let* ((content (+ (document-content-height document) 32))
         (maximum (document--maximum-scroll document)))
    (when (and (plusp maximum) (> height 40))
      (let* ((thumb-height (max 28 (floor (* height height) content)))
             (track        (- height thumb-height))
             (thumb-y      (+ y (floor (* track (document-scroll document)) maximum)))
             (thumb-x      (- (+ x width) 8))
             (hot-p        (or (stage-hot-p stage ':scrollbar) (stage-pressed-p stage ':scrollbar))))
        (if hot-p
            (paint-fill (- thumb-x 2) thumb-y :width 6 :height thumb-height)
            (paint-dither thumb-x thumb-y :width 4 :height thumb-height :step 2))
        (stage-region stage ':scrollbar
                      :x      (- (+ x width) 14)
                      :y      y
                      :width  14
                      :height height
                      :drag   (lambda (phase drag-x drag-y)
                                (declare (ignore phase drag-x))
                                (when (plusp track)
                                  (let ((fraction (/ (- drag-y y (floor thumb-height 2)) track)))
                                    (setf (document-scroll document)
                                          (max 0 (min maximum (round (* maximum fraction))))
                                          (document-following-p document)
                                          (>= (document-scroll document) maximum)))))))))
  nil)
