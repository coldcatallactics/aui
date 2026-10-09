(in-package #:aui)

;;;; -- Document --

;;; The rendered form of a transcript: each block becomes runs of styled text
;;; occupying a span of UTF-8 bytes in one text display. This file is pure:
;;; it decides what text and styles a block shows and where blocks begin and
;;; end; the transcript view hands the results to the widget.

(defparameter *document-styles*
  '((:body           :prose        :ink)
    (:bold           :prose-bold   :ink)
    (:italic         :prose-italic :ink)
    (:bold-italic    :prose-bold-italic :ink)
    (:code           :code         :ink)
    (:code-block     :code         :ink)
    (:heading        :prose-bold   :ink)
    (:link           :prose-italic (:blue :ink) :underline)
    (:bullet         :prose        :ink)
    (:meta           :prose-italic :ink)
    (:user-head      :prose-bold   (:blue :ink))
    (:assistant-head :prose-bold   (:green :ink))
    (:reasoning-head :prose-bold   (:violet :ink))
    (:tool-head      :prose-bold   :ink)
    (:approval-head  :prose-bold   (:orange :ink))
    (:failure        :prose-bold   (:red :ink))
    (:success        :prose-bold   (:green :ink)))
  "Every document style: its name, the theme face, the color as :INK or an
(ROLE TIER) accent, and optional attribute flags. The position of an entry is
its style index in the display's style table.")

(deftype document-style ()
  "A style name from *DOCUMENT-STYLES*."
  '(satisfies document-style-p))

(-> document-style-p (t) boolean)
(defun document-style-p (name)
  "Return T when NAME is a document style."
  (and (assoc name *document-styles*) t))

(-> document-style-index (keyword) integer)
(defun document-style-index (name)
  "Return the style table index of the document style NAME."
  (or (position name *document-styles* :key #'first)
      (error "Unknown document style ~S." name)))

(-> document-style-face (keyword) keyword)
(defun document-style-face (name)
  "Return the theme face of the document style NAME."
  (second (assoc name *document-styles*)))

(-> document-style-color (keyword) list)
(defun document-style-color (name)
  "Return the (RED GREEN BLUE) color of the document style NAME."
  (let ((color (third (assoc name *document-styles*))))
    (if (eq color ':ink)
        *ink*
        (accent (first color) :tier (second color)))))

(-> document-style-underline-p (keyword) boolean)
(defun document-style-underline-p (name)
  "Return T when the document style NAME is underlined."
  (and (member ':underline (cdddr (assoc name *document-styles*))) t))


;;; Rendering blocks to runs

(defparameter *tool-output-preview-lines* 40
  "How many lines of a tool's output an expanded block shows before eliding.")

(-> clock-text (integer) string)
(defun clock-text (universal-time)
  "Return UNIVERSAL-TIME as HH:MM in local time."
  (multiple-value-bind (second minute hour) (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(-> disclosure-marker (transcript-block) string)
(defun disclosure-marker (block)
  "Return the triangle showing whether BLOCK is collapsed."
  (if (transcript-block-collapsed-p block) "▸ " "▾ "))

(-> first-line (string &key (:limit integer)) string)
(defun first-line (text &key (limit 96))
  "Return the first line of TEXT as plain words, Markdown markers removed,
cut to LIMIT characters with an ellipsis."
  (let* ((end (or (position #\Newline text) (length text)))
         (line (string-trim " " (runs-text (markdown-inline-runs (subseq text 0 end))))))
    (if (> (length line) limit)
        (concatenate 'string (subseq line 0 (1- limit)) "…")
        line)))

(-> bounded-lines (string integer) (values string integer))
(defun bounded-lines (text limit)
  "Return the first LIMIT lines of TEXT and how many lines were left out."
  (let ((lines (split-lines-keeping-empty text)))
    (if (<= (length lines) limit)
        (values text 0)
        (values (format nil "~{~A~^~%~}" (subseq lines 0 limit))
                (- (length lines) limit)))))

(-> header-runs (string keyword transcript-block &key (:note string)) list)
(defun header-runs (label style block &key (note ""))
  "Return the runs of a block's header line: LABEL in STYLE, NOTE, and the time."
  (list (make-run label style)
        (make-run (format nil "~@[  ~A~]  ~A~%" (and (plusp (length note)) note)
                          (clock-text (transcript-block-received-at block)))
                  ':meta)))

(defgeneric block-runs (block)
  (:documentation "Return the runs that show BLOCK, ending with a blank line."))

(defmethod block-runs ((block user-message))
  (append (header-runs "You" ':user-head block)
          (markdown-runs (user-message-text block))
          (list (make-run (string #\Newline) ':body))))

(defmethod block-runs ((block assistant-text))
  (append (header-runs "Autolith" ':assistant-head block)
          (markdown-runs (assistant-text-text block))
          (list (make-run (string #\Newline) ':body))))

(defmethod block-runs ((block reasoning))
  (let ((text (reasoning-text block)))
    (if (transcript-block-collapsed-p block)
        (list (make-run (disclosure-marker block) ':reasoning-head)
              (make-run "Reasoning" ':reasoning-head)
              (make-run (format nil "  ~A~%~%" (first-line text)) ':meta))
        (append (list (make-run (disclosure-marker block) ':reasoning-head))
                (header-runs "Reasoning" ':reasoning-head block)
                (mapcar (lambda (run)
                          (if (member (run-style run) '(:body :bullet))
                              (make-run (run-text run) ':italic (run-url run))
                              run))
                        (markdown-runs text))
                (list (make-run (string #\Newline) ':body))))))

(-> tool-status-text (tool-call) (values string keyword))
(defun tool-status-text (block)
  "Return the status word of the tool call BLOCK and the style to show it in."
  (ecase (tool-call-status block)
    (:pending (values "waiting" ':meta))
    (:running (values "running" ':meta))
    (:completed (values "completed" ':success))
    (:failed (values "failed" ':failure))
    (:denied (values "denied" ':failure))
    (:cancelled (values "cancelled" ':meta))
    (:interrupted (values "interrupted" ':failure))))

(defmethod block-runs ((block tool-call))
  (multiple-value-bind (status-word status-style) (tool-status-text block)
    (let ((head (list (make-run (disclosure-marker block) ':tool-head)
                      (make-run (if (plusp (length (tool-call-title block)))
                                    (tool-call-title block)
                                    (tool-call-name block))
                                ':tool-head)
                      (make-run "  " ':meta)
                      (make-run status-word status-style)
                      (make-run (format nil "  ~A~%" (clock-text (transcript-block-received-at block)))
                                ':meta))))
      (if (transcript-block-collapsed-p block)
          (append head (list (make-run (string #\Newline) ':body)))
          (append head
                  (when (tool-call-input block)
                    (list (make-run (format nil "~A~%" (tool-call-input block)) ':code-block)))
                  (when (tool-call-output block)
                    (multiple-value-bind (shown omitted)
                        (bounded-lines (tool-call-output block) *tool-output-preview-lines*)
                      (append (list (make-run (format nil "~A~%" shown) ':code-block))
                              (when (plusp omitted)
                                (list (make-run (format nil "~D more lines in the inspector~%" omitted)
                                                ':meta))))))
                  (list (make-run (string #\Newline) ':body)))))))

(-> plan-entry-marker (keyword) string)
(defun plan-entry-marker (status)
  "Return the glyph for a plan entry STATUS."
  (ecase status
    (:pending "○ ")
    (:in-progress "◐ ")
    (:completed "● ")))

(defmethod block-runs ((block plan-block))
  (append (header-runs "Plan" ':tool-head block)
          (loop for (text . status) in (plan-block-entries block)
                append (list (make-run (plan-entry-marker status) ':body)
                             (make-run (format nil "~A~%" text)
                                       (if (eq status ':completed) ':meta ':body))))
          (list (make-run (string #\Newline) ':body))))

(-> approval-option-text (list integer) string)
(defun approval-option-text (option number)
  "Return the label of OPTION, an (ID NAME KIND) list, with its key NUMBER."
  (format nil "~A (~D)" (second option) number))

(defmethod block-runs ((block approval))
  (append (header-runs "Approval needed" ':approval-head block)
          (list (make-run (format nil "~A~%" (approval-title block)) ':bold))
          (when (approval-arguments block)
            (list (make-run (format nil "~A~%" (approval-arguments block)) ':code-block)))
          (when (approval-directory block)
            (list (make-run (format nil "in ~A~%" (approval-directory block)) ':meta)))
          (let ((decision (approval-decision block)))
            (cond
              ((null decision)
               (list (make-run (format nil "~{~A~^  ·  ~}~%"
                                       (loop for option in (approval-options block)
                                             for number from 1
                                             collect (approval-option-text option number)))
                               ':body)))
              ((eq decision ':cancelled)
               (list (make-run (format nil "Cancelled~%") ':meta)))
              (t
               (let ((option (find decision (approval-options block) :key #'first :test #'equal)))
                 (list (make-run (format nil "Decided: ~A~%" (if option (second option) decision))
                                 ':meta))))))
          (list (make-run (string #\Newline) ':body))))

(defmethod block-runs ((block notice))
  (list (make-run (ecase (notice-severity block)
                    (:information "Notice")
                    (:warning "Warning")
                    (:failure "Failure"))
                  (if (eq (notice-severity block) ':failure) ':failure ':meta))
        (make-run (format nil "  ~A~%~%" (notice-text block)) ':meta)))


;;; Spans

(defstruct (block-span (:constructor make-block-span (block-id start end &optional links)))
  "The byte range a block occupies in the display, and the links inside it
as (START END URL) byte ranges."
  (block-id "" :type string)
  (start 0 :type integer)
  (end 0 :type integer)
  (links nil :type list))

(-> runs-links (list integer) list)
(defun runs-links (runs start)
  "Return (START END URL) byte ranges for the link runs in RUNS placed at START."
  (let ((position start)
        (links nil))
    (dolist (run runs)
      (let ((length (lightfast:string-byte-length (run-text run))))
        (when (run-url run)
          (push (list position (+ position length) (run-url run)) links))
        (incf position length)))
    (nreverse links)))

(-> runs-byte-length (list) integer)
(defun runs-byte-length (runs)
  "Return the UTF-8 byte length of the text of RUNS."
  (loop for run in runs sum (lightfast:string-byte-length (run-text run))))

(defclass document ()
  ((spans :initform (make-array 0 :adjustable t :fill-pointer t) :reader document-spans
          :documentation "One span per rendered block, in transcript order."))
  (:documentation "Where each block of a transcript sits in the rendered text."))

(-> make-document () document)
(defun make-document ()
  "Return an empty document."
  (make-instance 'document))

(-> document-length (document) integer)
(defun document-length (document)
  "Return the byte length of the rendered text."
  (let ((spans (document-spans document)))
    (if (zerop (length spans))
        0
        (block-span-end (aref spans (1- (length spans)))))))

(-> document-append (document transcript-block) list)
(defun document-append (document block)
  "Record BLOCK at the end of DOCUMENT and return the runs to append."
  (let* ((runs (block-runs block))
         (start (document-length document)))
    (vector-push-extend (make-block-span (transcript-block-id block) start
                                         (+ start (runs-byte-length runs))
                                         (runs-links runs start))
                        (document-spans document))
    runs))

(-> document-span (document string) (or null block-span))
(defun document-span (document block-id)
  "Return the span of BLOCK-ID, or NIL."
  (find block-id (document-spans document) :key #'block-span-block-id :test #'string=))

(-> document-rerender (document transcript-block) (values list integer integer))
(defun document-rerender (document block)
  "Re-render BLOCK in place and return its new runs with the old byte START
and END to replace. Later spans shift by the change in length."
  (let* ((spans (document-spans document))
         (position (position (transcript-block-id block) spans
                             :key #'block-span-block-id :test #'string=)))
    (unless position
      (error "Block ~S is not in the document." (transcript-block-id block)))
    (let* ((span (aref spans position))
           (start (block-span-start span))
           (old-end (block-span-end span))
           (runs (block-runs block))
           (new-end (+ start (runs-byte-length runs)))
           (shift (- new-end old-end)))
      (setf (block-span-end span) new-end
            (block-span-links span) (runs-links runs start))
      (loop for index from (1+ position) below (length spans)
            for later = (aref spans index)
            do (incf (block-span-start later) shift)
               (incf (block-span-end later) shift)
               (setf (block-span-links later)
                     (mapcar (lambda (link)
                               (list (+ (first link) shift) (+ (second link) shift) (third link)))
                             (block-span-links later))))
      (values runs start old-end))))

(-> document-link-at (document integer) (or null string))
(defun document-link-at (document byte-position)
  "Return the url of the link under BYTE-POSITION, or NIL."
  (loop for span across (document-spans document)
        do (loop for (start end url) in (block-span-links span)
                 when (and (<= start byte-position) (< byte-position end))
                   do (return-from document-link-at url))))

(-> document-block-at (document integer) (or null string))
(defun document-block-at (document byte-position)
  "Return the id of the block whose span contains BYTE-POSITION, or NIL."
  (let ((span (find-if (lambda (span)
                         (and (<= (block-span-start span) byte-position)
                              (< byte-position (block-span-end span))))
                       (document-spans document))))
    (and span (block-span-block-id span))))

(-> document-clear (document) null)
(defun document-clear (document)
  "Forget every span."
  (setf (fill-pointer (document-spans document)) 0)
  nil)
