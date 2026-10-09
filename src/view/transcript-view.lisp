(in-package #:aui)

;;;; -- Transcript View --

;;; One styled text display showing a transcript through the document layer.
;;; The view listens to the transcript, appends or re-renders block spans,
;;; follows the end while the user has not scrolled away, and toggles a
;;; block's disclosure when its header line is clicked.

(defclass transcript-view ()
  ((transcript :initarg :transcript :reader transcript-view-transcript :type transcript)
   (document   :initform (make-document) :reader transcript-view-document :type document)
   (display    :initarg :display :reader transcript-view-display
               :documentation "The Lightfast text display widget.")
   (follow-p   :initform t :accessor transcript-view-follow-p :type boolean
               :documentation "Whether appends scroll the end into view.")
   (on-context :initform nil :accessor transcript-view-on-context
               :documentation "Called with the block id under a right click, or NIL to ignore them."))
  (:documentation "A transcript rendered into one text display."))

(-> document-style-table () list)
(defun document-style-table ()
  "Return the Lightfast style plists for *DOCUMENT-STYLES*, in index order."
  (loop for (name face) in *document-styles*
        collect (list :font (font face)
                      :size (if (member face '(:code :code-bold :code-italic :code-bold-italic))
                                *code-size*
                                *prose-size*)
                      :color (document-style-color name)
                      :attributes (if (document-style-underline-p name)
                                      lightfast:+text-attribute-underline+
                                      0))))

(-> runs->styled (list) list)
(defun runs->styled (runs)
  "Return RUNS as (TEXT . STYLE-INDEX) pairs for the text display."
  (mapcar (lambda (run)
            (cons (run-text run) (document-style-index (run-style run))))
          runs))

(-> make-transcript-view
    (transcript &key (:parent t) (:x integer) (:y integer) (:width integer) (:height integer))
    transcript-view)
(defun make-transcript-view (transcript &key parent (x 0) (y 0) (width 400) (height 300))
  "Create a text display under PARENT showing TRANSCRIPT, and return the view."
  (let* ((display (lightfast:make-text-display :parent parent :x x :y y :width width :height height))
         (view (make-instance 'transcript-view :transcript transcript :display display)))
    (lightfast:set-box display lightfast:+box-no-box+)
    (lightfast:text-set-wrap display ':bounds)
    (lightfast:text-set-styles display (document-style-table))
    (dolist (block (coerce (transcript-blocks transcript) 'list))
      (transcript-view--append view block))
    (transcript-add-listener transcript view)
    (lightfast:on display (lambda (widget event value)
                            (declare (ignore widget event))
                            (transcript-view--clicked view value))
                  :event lightfast:+event-push+)
    (lightfast:on display (lambda (widget event value)
                            (declare (ignore widget event))
                            (transcript-view--key view value))
                  :event lightfast:+event-key+)
    view))

(-> transcript-view--append (transcript-view transcript-block) null)
(defun transcript-view--append (view block)
  "Render BLOCK at the end of the display."
  (lightfast:text-append-styled (transcript-view-display view)
                                (runs->styled (document-append (transcript-view-document view) block)))
  (transcript-view--follow view)
  nil)

(-> transcript-view--rerender (transcript-view transcript-block) null)
(defun transcript-view--rerender (view block)
  "Replace BLOCK's span with its current rendering."
  (multiple-value-bind (runs start end) (document-rerender (transcript-view-document view) block)
    (lightfast:text-replace-styled (transcript-view-display view) start end (runs->styled runs)))
  (transcript-view--follow view)
  nil)

(-> transcript-view--follow (transcript-view) null)
(defun transcript-view--follow (view)
  "Scroll to the end when the view is following it."
  (when (transcript-view-follow-p view)
    (lightfast:text-scroll-to-end (transcript-view-display view)))
  nil)

(-> transcript-view-jump-to-end (transcript-view) null)
(defun transcript-view-jump-to-end (view)
  "Resume following the end of the transcript and scroll there."
  (setf (transcript-view-follow-p view) t)
  (transcript-view--follow view))

(-> transcript-view-toggle (transcript-view string) null)
(defun transcript-view-toggle (view block-id)
  "Collapse or expand the block BLOCK-ID and redraw it."
  (let ((block (transcript-find (transcript-view-transcript view) block-id)))
    (when block
      (setf (transcript-block-collapsed-p block) (not (transcript-block-collapsed-p block)))
      (let ((follow (transcript-view-follow-p view)))
        (setf (transcript-view-follow-p view) nil)
        (transcript-view--rerender view block)
        (setf (transcript-view-follow-p view) follow))))
  nil)

(-> parse-integers (string) list)
(defun parse-integers (text)
  "Return the integers in the space separated TEXT."
  (loop with start = 0
        for end = (position #\Space text :start start)
        for token = (subseq text start end)
        when (plusp (length token))
          collect (parse-integer token :junk-allowed t)
        while end
        do (setf start (1+ end))))

(-> transcript-view--clicked (transcript-view string) null)
(defun transcript-view--clicked (view value)
  "Handle a mouse press described by VALUE, Lightfast's x y button dx dy state."
  (destructuring-bind (x y button &rest rest) (parse-integers value)
    (declare (ignore rest))
    (let* ((display (transcript-view-display view))
           (position (lightfast:text-position-at display x y))
           (document (transcript-view-document view))
           (block-id (and position (document-block-at document position))))
      (cond
        ((and (= button 3) block-id (transcript-view-on-context view))
         (funcall (transcript-view-on-context view) block-id))
        ((/= button 1)
         nil)
        ((and block-id
              (= (lightfast:text-line-of-position display position)
                 (lightfast:text-line-of-position
                  display (block-span-start (document-span document block-id)))))
         (transcript-view-toggle view block-id))
        (t
         ;; A click into the body means the reader is here, not at the end.
         (setf (transcript-view-follow-p view) nil)))))
  nil)

(-> transcript-view-block-text (transcript-view string) string)
(defun transcript-view-block-text (view block-id)
  "Return the rendered text of the block BLOCK-ID."
  (let ((span (document-span (transcript-view-document view) block-id)))
    (if span
        (lightfast:text-range (transcript-view-display view) (block-span-start span) (block-span-end span))
        "")))

(defparameter *key-end* #xff57
  "FLTK's key number for End.")

(-> transcript-view--key (transcript-view string) null)
(defun transcript-view--key (view value)
  "Handle a key described by VALUE, Lightfast's key state text."
  (let ((key (first (parse-integers value))))
    (when (eql key *key-end*)
      (lightfast:consume-event)
      (transcript-view-jump-to-end view)))
  nil)

(defmethod transcript-listener-changed ((view transcript-view) transcript block change)
  (declare (ignore transcript))
  (ecase change
    (:appended
     (transcript-view--append view block))
    ((:text-appended :updated :replaced)
     (transcript-view--rerender view block))
    (:reset
     (let ((display (transcript-view-display view)))
       (lightfast:text-replace-styled display 0 (lightfast:text-length display) nil)
       (document-clear (transcript-view-document view))
       (setf (transcript-view-follow-p view) t)))
    ((:replay-begin :replay-end)
     nil)))
