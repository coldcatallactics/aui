(in-package #:aui)

;;;; -- Completion --

;;; What the composer offers while the person types a command or a mention:
;;; the channel's commands for a draft that begins with a slash or an opening
;;; parenthesis, and the world's participants for a word that begins with @.
;;; The list sits above the composer; Up and Down move in it, Tab takes the
;;; highlighted entry, Escape puts it away.

(defparameter *completion-rows* 7
  "How many completions are shown at once.")

(defparameter *completion-row-height* 26
  "The height of one completion row.")

(defstruct completion
  "One offer, with the LABEL shown, the DETAIL beside it, and the TEXT that replaces the word being typed."
  (label  "" :type string)
  (detail "" :type string)
  (text   "" :type string))

(-> completion--word (string) (values string integer))
(defun completion--word (text)
  "Return the word at the end of TEXT and where it starts."
  (let ((start (1+ (or (position-if (lambda (character)
                                      (member character '(#\Space #\Newline #\Tab)))
                                    text :from-end t)
                       -1))))
    (values (subseq text start) start)))

(-> completion--commands (workbench string (member :slash :form)) list)
(defun completion--commands (workbench prefix notation)
  "Return the commands whose names begin with PREFIX, written in NOTATION: after a slash, or as a Lisp form."
  (loop for row in (workbench-operations workbench)
        for name = (portable-get row "name")
        when (and (stringp name)
                  (equal (portable-get row "kind") "command")
                  (not (member name *terminal-only-commands* :test #'string=))
                  (>= (length name) (length prefix))
                  (string-equal prefix name :end2 (length prefix)))
          collect (let ((opened (format nil "~:[(~;/~]~A" (eq notation ':slash) name)))
                    (make-completion
                     :label  opened
                     :detail (entries--one-line (or (portable-get row "description") ""))
                     :text   (cond
                               ((portable-string (portable-get row "argumentHint"))
                                (format nil "~A " opened))
                               ((eq notation ':form)
                                (format nil "~A)" opened))
                               (t
                                opened))))))

(-> completion--participants (workbench string) list)
(defun completion--participants (workbench prefix)
  "Return the world's participants whose names begin with PREFIX, as mentions."
  (unless (workbench-demo-p workbench)
    (loop for participant in (workbench-participants workbench)
          for name = (getf participant :name)
          when (and (stringp name)
                    (>= (length name) (length prefix))
                    (string-equal prefix name :end2 (length prefix)))
            collect (make-completion
                     :label  (format nil "@~A" name)
                     :detail (cond
                               ((eq (getf participant :kind) ':human)
                                "you")
                               ((getf participant :residence)
                                (format nil "agent in ~A" (channel-display-name (getf participant :residence))))
                               (t
                                "parked agent"))
                     :text   (format nil "@~A " name)))))

(-> completion-candidates (workbench string) list)
(defun completion-candidates (workbench text)
  "Return what to offer for the composer holding TEXT, or NIL when nothing applies."
  (multiple-value-bind (word start) (completion--word text)
    (cond
      ((zerop (length word))
       nil)
      ;; A command is completed only as the first thing in the draft.
      ((and (zerop start) (char= (char word 0) #\/))
       (completion--commands workbench (subseq word 1) ':slash))
      ((and (zerop start) (char= (char word 0) #\() (not (find #\) word)))
       (completion--commands workbench (subseq word 1) ':form))
      ((and (char= (char word 0) #\@) (not (find #\/ word)))
       (completion--participants workbench (subseq word 1)))
      (t
       nil))))

(defvar *completion-offer* nil
  "What was last offered and for what, as (TEXT OPERATIONS PARTICIPANTS . CANDIDATES), so one drawing works it out once.")

(-> completion--offer (workbench string) list)
(defun completion--offer (workbench text)
  "Return what to offer for the draft TEXT, worked out again only when the draft or what it is completed from changed."
  (let ((operations   (workbench-operations workbench))
        (participants (workbench--participants workbench)))
    (destructuring-bind (&optional known-text known-operations known-participants &rest candidates)
        *completion-offer*
      (if (and *completion-offer*
               (string= text known-text)
               (eq operations known-operations)
               (eq participants known-participants))
          candidates
          (let ((candidates (completion-candidates workbench text)))
            (setf *completion-offer* (list* text operations participants candidates))
            candidates)))))

(-> completion-current (workbench) list)
(defun completion-current (workbench)
  "Return the completions on offer now, or NIL when the list was put away for this draft."
  (let ((text (composer-text workbench)))
    (if (equal text (workbench-completion-dismissed workbench))
        nil
        (let ((candidates (completion--offer workbench text)))
          ;; One offer that the draft already spells out is nothing to offer.
          (if (and candidates
                   (null (rest candidates))
                   (string= (string-right-trim " " (completion-text (first candidates)))
                            (string-right-trim " " text)))
              nil
              candidates)))))

(-> completion-accept (workbench completion) null)
(defun completion-accept (workbench completion)
  "Replace the word being typed with COMPLETION's text."
  (let ((text (composer-text workbench)))
    (multiple-value-bind (word start) (completion--word text)
      (declare (ignore word))
      (setf (composer-text workbench)
            (concatenate 'string (subseq text 0 start) (completion-text completion)))
      (composer-caret-to-end workbench)))
  (setf (workbench-completion-index workbench) 0)
  nil)

(-> completion-key (workbench key-event) boolean)
(defun completion-key (workbench event)
  "Handle a key while completions are on offer and return whether it was used."
  (let* ((candidates (completion-current workbench))
         (count      (length candidates)))
    (when candidates
      (case (key-event-key event)
        (:down
         (setf (workbench-completion-index workbench)
               (mod (1+ (workbench-completion-index workbench)) count))
         (workbench-redraw workbench)
         t)
        (:up
         (setf (workbench-completion-index workbench)
               (mod (1- (workbench-completion-index workbench)) count))
         (workbench-redraw workbench)
         t)
        (:tab
         (completion-accept workbench
                            (nth (min (workbench-completion-index workbench) (1- count)) candidates))
         t)
        (:escape
         (setf (workbench-completion-dismissed workbench) (composer-text workbench))
         (workbench-redraw workbench)
         t)
        (otherwise
         nil)))))

(-> completion-height (workbench) integer)
(defun completion-height (workbench)
  "Return the height the completion list needs, zero when nothing is on offer."
  (let ((count (length (completion-current workbench))))
    (if (zerop count)
        0
        (+ 2 (* *completion-row-height* (min count *completion-rows*))))))

(-> completion-paint (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun completion-paint (workbench stage &key x y width)
  "Draw the completions in a framed list with its top-left corner at X, Y."
  (let* ((candidates (completion-current workbench))
         (count      (length candidates))
         (index      (min (workbench-completion-index workbench) (max 0 (1- count))))
         (first      (max 0 (min (- index (1- *completion-rows*)) (- count *completion-rows*))))
         (first      (max 0 (if (< index first) index first)))
         (height     (completion-height workbench))
         (bold       (text-style ':label-bold))
         (meta       (text-style ':meta))
         ;; One column for what is offered, one for what is said about it.
         (label-w    (min (floor (* width 2/5))
                          (loop for completion in candidates
                                maximize (text-width (completion-label completion) bold)))))
    (when candidates
      (paint-fill x y :width width :height height :color *paper*)
      (paint-frame x y :width width :height height)
      (loop for completion in (nthcdr first candidates)
            for row from first
            for row-y from (1+ y) by *completion-row-height*
            repeat *completion-rows*
            do (let* ((chosen completion)
                      (region (list ':completion row))
                      (lit-p  (or (= row index) (stage-hot-p stage region))))
                 (flet ((content ()
                          (paint-text-in (completion-label completion) bold
                                         :x      (+ x 10)
                                         :y      row-y
                                         :width  label-w
                                         :height *completion-row-height*)
                          (paint-text-in (completion-detail completion) meta
                                         :x      (+ x 10 label-w 18)
                                         :y      row-y
                                         :width  (max 0 (- width label-w 40))
                                         :height *completion-row-height*)))
                   (if lit-p
                       (with-inversion ((1+ x) row-y (- width 2) *completion-row-height*)
                         (content))
                       (content)))
                 (stage-region stage region
                               :x      x
                               :y      row-y
                               :width  width
                               :height *completion-row-height*
                               :action (lambda ()
                                         (completion-accept workbench chosen)
                                         (window-focus-composer workbench)))))))
  nil)
