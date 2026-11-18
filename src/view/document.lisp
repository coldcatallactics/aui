(in-package #:aui)

;;;; -- Document View --

;;; The transcript as a scrolling column of blocks. An entry is typeset when
;;; the view comes near it, and again there when its revision or the width
;;; moved, so a page of history that arrives, a long conversation and a new
;;; width all cost what is around the view and no more. Until then an entry
;;; has the height it last had, or an estimate; an entry far from the view
;;; gives its blocks up and keeps its height. A drag selects text across
;;; entries, and the selection is copied when released.

(defparameter *document-measure* 760
  "The widest the reading column gets, in FLTK units: about seventy characters of the body face.")

(defparameter *document-margin* 28
  "The least space kept at each side of the reading column.")

(defparameter *document-wheel-step* 54
  "How far one wheel step scrolls.")

(defparameter *document-kept-items* 200
  "How many entries on each side of those in view keep their blocks.")

(defparameter *document-item-estimate* 90
  "The height an entry is taken to have before it was ever laid out.")

(defstruct item
  "One entry as it is laid out, with its blocks, where it starts in the column, and its selectable leaves. WIDTH is the column width the blocks are laid out to, or zero for an item that holds none and keeps the height it last had."
  (entry    nil)
  (revision -1  :type integer)
  (width    0   :type integer)
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

(-> document--place (document item list) null)
(defun document--place (document item blocks)
  "Lay BLOCKS out to the column as ITEM's."
  (multiple-value-bind (height placed) (document--stack blocks (document--column-width document))
    (setf (item-blocks item) placed
          (item-height item) height
          (item-leaves item) (loop for (block . top) in placed
                                   append (loop for (paragraph leaf-x leaf-y) in (block-leaves block)
                                                collect (list paragraph leaf-x (+ top leaf-y))))))
  nil)

(-> document--entry-blocks (document entry) list)
(defun document--entry-blocks (document entry)
  "Typeset ENTRY into blocks; a click on what opens and closes it touches the transcript."
  (let ((transcript (document-transcript document)))
    (entry-blocks entry
                  (lambda ()
                    (setf (entry-expanded-p entry) (not (entry-expanded-p entry)))
                    (transcript-touch transcript entry)))))

(-> document--current-p (document item) boolean)
(defun document--current-p (document item)
  "Return T when ITEM holds the blocks of its entry as it is now, laid out to the column."
  (and (= (item-revision item) (entry-revision (item-entry item)))
       (= (item-width item) (document--column-width document))))

(-> document--lay-item (document item) null)
(defun document--lay-item (document item)
  "Bring ITEM's layout up to its entry and the column.

An entry whose revision moved, or whose item gave its blocks up, is typeset;
one that only meets a new width has its blocks laid out again. An entry that
cannot be typeset is shown as a line saying so, and the rest of the document
as usual."
  (let ((entry (item-entry item)))
    (handler-case
        (document--place document item
                         (if (and (plusp (item-width item))
                                  (= (item-revision item) (entry-revision entry)))
                             (mapcar #'first (item-blocks item))
                             (document--entry-blocks document entry)))
      (error (condition)
        (format *error-output* "~&aui: an entry could not be shown: ~A~%" condition)
        (document--place document item
                         (list (make-text-block
                                (spans (format nil "! this entry could not be shown: ~A"
                                               (condition-report condition)))
                                :style (text-style ':meta :color *red*))))))
    (setf (item-revision item) (entry-revision entry)
          (item-width item)    (document--column-width document)))
  nil)

(-> document--release-item (item) null)
(defun document--release-item (item)
  "Have ITEM give its blocks up and keep its height."
  (setf (item-blocks item) nil
        (item-leaves item) nil
        (item-width item)  0)
  nil)

(-> document--place-items (document) null)
(defun document--place-items (document)
  "Set where each item starts from the heights of those before it, and the height of them all."
  (let ((top 0))
    (loop for item across (document--items document)
          do (setf (item-top item) top)
             (incf top (item-height item)))
    (setf (document-content-height document) top))
  nil)

(-> document--aligned-p (vector vector) boolean)
(defun document--aligned-p (items entries)
  "Return T when ITEMS are the layouts of the first of ENTRIES in order, as they are unless entries went in front or away."
  (and (<= (length items) (length entries))
       (loop for item across items
             for entry across entries
             always (eq (item-entry item) entry))))

(-> document--new-item (entry) item)
(defun document--new-item (entry)
  "Return the item of ENTRY before it was laid out, as tall as an entry is taken to be."
  (make-item :entry entry :height *document-item-estimate*))

(-> document--items-for (document vector) vector)
(defun document--items-for (document entries)
  "Return the items of ENTRIES in order, each entry keeping the item it has in DOCUMENT."
  (let ((items (document--items document)))
    (if (document--aligned-p items entries)
        (loop for index from (length items) below (length entries)
              do (vector-push-extend (document--new-item (aref entries index)) items)
              finally (return items))
        (let ((known (make-hash-table :test #'eq))
              (next  (make-array (length entries) :adjustable t :fill-pointer 0)))
          (loop for item across items
                do (setf (gethash (item-entry item) known) item))
          (loop for entry across entries
                do (vector-push (or (gethash entry known) (document--new-item entry)) next))
          next))))

(-> document-sync (document integer) null)
(defun document-sync (document width)
  "Bring the items up to the transcript's entries, for a column WIDTH wide.

Nothing is laid out here: `document--bring-near' does that for what the view
is near. When entries arrive in front, as an earlier page of history does,
the view stays on what it showed."
  (let* ((transcript (document-transcript document))
         (entries    (transcript-entries transcript)))
    (setf (document--column-width document) width)
    (unless (= (transcript-revision transcript) (document--laid-revision document))
      (let* ((old      (document--items document))
             (first    (and (plusp (length old)) (item-entry (aref old 0))))
             (old-top  (and first (item-top (aref old 0))))
             (selected (document--anchor document))
             (items    (document--items-for document entries)))
        (setf (document--items document) items)
        (document--place-items document)
        ;; Keep the first entry that was shown where it was when a page went in front.
        (when (and first old-top (not (document-following-p document)))
          (let ((moved (find first items :key #'item-entry)))
            (when (and moved (/= (item-top moved) old-top))
              (incf (document-scroll document) (- (item-top moved) old-top)))))
        (when (and selected (not (and (plusp (length entries)) (eq (aref entries 0) first))))
          (setf (document--anchor document) nil
                (document--focus document)  nil)))
      (setf (document--laid-revision document) (transcript-revision transcript))))
  nil)

(-> document--bring-near (document) boolean)
(defun document--bring-near (document)
  "Lay out the items in the view and a view's height around it that are not laid out to the column, release those far from it, and return whether any was laid out.

An item above the view that changes height moves the scroll with it, so what
is shown stays where it is."
  (let* ((items  (document--items document))
         (height (fourth (document--viewport document)))
         (first  (or (document--item-at document (max 0 (- (document-scroll document) height))) 0))
         (last   first)
         (shift  0)
         (laid-p nil))
    (loop for index from first below (length items)
          for item = (aref items index)
          for top = (+ (item-top item) shift)
          while (< top (+ (document-scroll document) (* 2 height)))
          do (setf last index)
             (unless (document--current-p document item)
               (let ((before (item-height item)))
                 (document--lay-item document item)
                 (setf laid-p t)
                 (let ((grown (- (item-height item) before)))
                   (incf shift grown)
                   (when (< top (document-scroll document))
                     (incf (document-scroll document) grown))))))
    (unless (zerop shift)
      (document--place-items document))
    (loop for item across items
          for index from 0
          do (unless (or (zerop (item-width item))
                         (<= (- first *document-kept-items*) index (+ last *document-kept-items*)))
               (document--release-item item)))
    laid-p))

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

(-> document-scroll-to-entry (document entry) null)
(defun document-scroll-to-entry (document entry)
  "Scroll DOCUMENT so that ENTRY stands at the top of the view. The view follows the newest entry again only when that leaves it at the bottom."
  (let ((item (find entry (document--items document) :key #'item-entry)))
    (when item
      (let* ((maximum (document--maximum-scroll document))
             (target  (max 0 (min maximum (- (item-top item) 12)))))
        (setf (document-scroll document)      target
              (document-following-p document) (>= target maximum)))))
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
                                 ;; An entry typeset again since the selection was made may hold less text.
                                 (limit    (length (paragraph-text paragraph)))
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
                            (when (and from to (< from (min to limit)))
                              (setf (gethash paragraph table) (cons from (min to limit)))))))
        table))))

(-> document--lay-selection (document) null)
(defun document--lay-selection (document)
  "Lay out the selected items that gave their blocks up, so that their text can be read."
  (multiple-value-bind (start end) (document--selection-range document)
    (when start
      (let ((items (document--items document)))
        (loop for index from (first start) to (min (first end) (1- (length items)))
              do (unless (document--current-p document (aref items index))
                   (document--lay-item document (aref items index))))
        (document--place-items document))))
  nil)

(-> document-selected-text (document) (or null string))
(defun document-selected-text (document)
  "Return the selected text, paragraphs on their own lines, or NIL when nothing is selected."
  (document--lay-selection document)
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
    (flet ((settle ()
             (setf (document-scroll document)
                   (if (document-following-p document)
                       (document--maximum-scroll document)
                       (max 0 (min (document-scroll document) (document--maximum-scroll document)))))))
      (settle)
      ;; Laying out what is near can move the view, which then is near other entries.
      (loop repeat 8
            while (document--bring-near document)
            do (settle)))
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
    (with-clip (x y width height)
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
    (document--paint-scrollbar document stage :x x :y y :width width :height height)
    ;; Away from the end, offer the way back to it.
    (unless (document-following-p document)
      (paint-button stage ':to-newest ""
                    :x       (- (+ x width) 54)
                    :y       (- (+ y height) 44)
                    :width   30
                    :height  30
                    :mark    ':arrow-down
                    :action  (lambda () (document-scroll-to-end document))
                    :tooltip "Show the newest output and follow it")))
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
