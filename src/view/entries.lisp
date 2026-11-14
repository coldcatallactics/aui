(in-package #:aui)

;;;; -- Entries as Blocks --

;;; How each kind of transcript entry is drawn. A message from the person is
;;; a framed box at the right, as in any chat; an answer is plain prose at the
;;; full measure; what the agent did, thought and was told are quiet one-line
;;; rows that open on a click.

(defparameter *tool-preview-keys*
  '("command" "forms" "form" "uri" "path" "query" "pattern" "patterns" "url" "system" "name" "task" "text")
  "The argument names tried, in order, for a tool call's one-line preview.")

(defparameter *output-preview-lines* 14
  "How many lines of a tool's output an opened row shows before offering the rest.")

(defgeneric entry-blocks (entry toggle)
  (:documentation "Return the blocks that draw ENTRY. TOGGLE is a function of no arguments that opens or closes the entry's detail."))

(-> entries--value-text (t) string)
(defun entries--value-text (value)
  "Return the decoded JSON VALUE as it reads on one line."
  (cond
    ((stringp value)
     value)
    ((eq value ':false)
     "false")
    ((eq value ':null)
     "null")
    ((eq value t)
     "true")
    ((and (consp value) (consp (first value)) (stringp (first (first value))))
     (format nil "{~{~A~^, ~}}"
             (mapcar (lambda (member)
                       (format nil "~A: ~A" (first member) (entries--value-text (rest member))))
                     value)))
    ((listp value)
     (format nil "~{~A~^, ~}" (mapcar #'entries--value-text value)))
    (t
     (princ-to-string value))))

(-> entries--one-line (string) string)
(defun entries--one-line (text)
  "Return TEXT with every run of whitespace, line breaks included, as one space."
  (let ((result  (make-string-output-stream))
        (space-p nil))
    (loop for character across (string-trim '(#\Space #\Newline #\Tab #\Return) text)
          do (if (member character '(#\Space #\Newline #\Tab #\Return))
                 (setf space-p t)
                 (progn
                   (when space-p
                     (write-char #\Space result)
                     (setf space-p nil))
                   (write-char character result))))
    (get-output-stream-string result)))

(-> tool-entry-preview (tool-entry) string)
(defun tool-entry-preview (entry)
  "Return the one line that says what ENTRY's call is about: its command, path, query or first argument."
  (let* ((arguments (tool-entry-arguments entry))
         (member    (or (loop for key in *tool-preview-keys*
                              for found = (assoc key arguments :test #'string=)
                              when found
                                do (return found))
                        (first arguments))))
    (if member
        (entries--one-line (entries--value-text (rest member)))
        "")))

(-> entries--duration (real) string)
(defun entries--duration (seconds)
  "Return SECONDS as a person reads a duration: 0.4s, 12s, 3m 05s."
  (cond
    ((< seconds 10)
     (format nil "~,1Fs" seconds))
    ((< seconds 60)
     (format nil "~Ds" (round seconds)))
    (t
     (multiple-value-bind (minutes rest) (floor (round seconds) 60)
       (format nil "~Dm ~2,'0Ds" minutes rest)))))

(-> entries--clock (integer) string)
(defun entries--clock (universal-time)
  "Return the local hour and minute of UNIVERSAL-TIME."
  (multiple-value-bind (second minute hour) (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))


;;;; -- Messages --

(defmethod entry-blocks ((entry user-entry) toggle)
  "The person's message in a framed box at the right; a message from elsewhere says who sent it."
  (declare (ignore toggle))
  (let* ((sender (user-entry-sender entry))
         (text   (user-entry-text entry))
         (body   (append
                  (when sender
                    (list (make-text-block
                           (spans (list (format nil "~A~@[ in ~A~]" sender
                                                (and (user-entry-origin entry)
                                                     (channel-display-name (user-entry-origin entry))))
                                        (text-style ':meta-bold)))
                           :style (text-style ':meta-bold) :margin-bottom 4)))
                  (mapcar (lambda (pathname)
                            (make-instance 'image-block :pathname pathname :margin-bottom 8))
                          (user-entry-images entry))
                  (when (plusp (length text))
                    (list (make-text-block (list (make-span text nil)) :style (text-style ':body)))))))
    (list (make-instance 'box-block
                         :children      (or body (list (make-text-block (list (make-span "" nil)))))
                         :align         (if sender ':left ':right)
                         :fraction      85/100
                         :padding-x     16
                         :padding-y     9
                         :margin-top    22
                         :margin-bottom 22))))

(defmethod entry-blocks ((entry assistant-entry) toggle)
  "The agent's answer as prose."
  (declare (ignore toggle))
  (let ((blocks (prose-blocks (assistant-entry-text entry))))
    (when blocks
      (setf (block-margin-top (first blocks)) (max 8 (block-margin-top (first blocks)))))
    blocks))

(defmethod entry-blocks ((entry reasoning-entry) toggle)
  "A closed row saying the agent thought, opening to its reasoning in small italics."
  (let* ((text       (reasoning-entry-text entry))
         (expanded-p (entry-expanded-p entry))
         (label      (text-style ':label :face ':mono-italic))
         (row        (make-instance 'row-block
                                    :identifier (list ':entry (entry-identifier entry))
                                    :mark       (cond
                                                  ((reasoning-entry-streaming-p entry)
                                                   ':spinner)
                                                  (expanded-p
                                                   ':open)
                                                  (t
                                                   ':closed))
                                    :title      (spans (list (if (reasoning-entry-streaming-p entry)
                                                                 "Thinking"
                                                                 "Thought")
                                                             label)
                                                       (list (format nil "  ~A"
                                                                     (entries--one-line
                                                                      (subseq text 0 (min 160 (length text)))))
                                                             (text-style ':label)))
                                    :style      label
                                    :action     toggle
                                    :margin-top 2)))
    (if expanded-p
        (list row
              (make-instance 'box-block
                             :children      (prose-blocks text :style (text-style ':small :face ':serif-italic))
                             :frame-p       nil
                             :padding-x     28
                             :padding-y     2
                             :margin-bottom 8))
        (list row))))


;;;; -- Tools --

(-> entries--plan-blocks (tool-entry) list)
(defun entries--plan-blocks (entry)
  "Return the blocks of a plan update: its explanation and its steps as a checklist."
  (let* ((arguments   (tool-entry-arguments entry))
         (explanation (rest (assoc "explanation" arguments :test #'string=)))
         (steps       (rest (or (assoc "plan" arguments :test #'string=)
                                (assoc "steps" arguments :test #'string=))))
         (style       (text-style ':list)))
    (list
     (make-instance
      'box-block
      :padding-x     14
      :padding-y     10
      :margin-top    10
      :margin-bottom 10
      :children
      (append
       (list (make-text-block (spans (list "Plan" (text-style ':label-bold)))
                              :style (text-style ':label-bold) :margin-bottom 4))
       (when (and (stringp explanation) (plusp (length explanation)))
         (list (make-text-block (list (make-span explanation nil))
                                :style (text-style ':small :face ':serif-italic) :margin-bottom 6)))
       (loop for step in (and (listp steps) steps)
             for text = (and (consp step)
                             (rest (or (assoc "step" step :test #'string=)
                                       (assoc "text" step :test #'string=))))
             for status = (and (consp step) (rest (assoc "status" step :test #'string=)))
             when (stringp text)
               collect (make-text-block
                        (list (make-span text
                                         (cond
                                           ((member status '("in_progress" "doing") :test #'equal)
                                            (text-style ':list-bold))
                                           ((member status '("completed" "done") :test #'equal)
                                            (text-style ':list :strike-p t))
                                           (t
                                            nil))))
                        :style  style
                        :indent 26
                        :marker (if (member status '("completed" "done") :test #'equal) ':done ':open)
                        :margin-top 2 :margin-bottom 2)))))))

(defparameter *diff-preview-lines* 40
  "How many lines of each side of an edit an opened row shows.")

(-> entries--diff-spans (string string) list)
(defun entries--diff-spans (old new)
  "Return the spans of an edit as a diff: the lines of OLD marked removed, then the lines of NEW marked added.

The marks carry the color; the text stays ink, as Interlisp Paper has it."
  (let ((removed (text-style ':code :face ':mono-bold :color *red*))
        (added   (text-style ':code :face ':mono-bold :color *green*))
        (spans   nil))
    (flet ((side (text mark style)
             (let ((lines (markdown-lines (string-right-trim '(#\Newline) text))))
               (loop for line in lines
                     for index from 0
                     while (< index *diff-preview-lines*)
                     do (push (make-span mark style) spans)
                        (push (make-span (format nil "~A~%" line) nil) spans))
               (when (> (length lines) *diff-preview-lines*)
                 (push (make-span (format nil "  … ~D more lines~%" (- (length lines) *diff-preview-lines*))
                                  nil)
                       spans)))))
      (when (plusp (length old))
        (side old "- " removed))
      (when (plusp (length new))
        (side new "+ " added)))
    (nreverse spans)))

(-> entries--argument (tool-entry &rest string) t)
(defun entries--argument (entry &rest names)
  "Return the value of the first of ENTRY's arguments named by one of NAMES, or NIL."
  (loop for name in names
        for member = (assoc name (tool-entry-arguments entry) :test #'string=)
        when member
          do (return (rest member))))

(-> entries--preview (string) string)
(defun entries--preview (text)
  "Return the first lines of TEXT, as many as a result shows at once, and how many more there are."
  (let* ((lines (markdown-lines (string-right-trim '(#\Newline #\Space) text)))
         (shown (if (> (length lines) *output-preview-lines*)
                    (subseq lines 0 *output-preview-lines*)
                    lines)))
    (format nil "~{~A~^~%~}~@[~%… ~D more lines~]" shown
            (and (> (length lines) (length shown)) (- (length lines) (length shown))))))

(-> entries--tool-detail (tool-entry) flow-block)
(defun entries--tool-detail (entry)
  "Return the opened detail of ENTRY's call: what it was asked, as a command, a diff or its arguments, and what it returned."
  (let* ((arguments (tool-entry-arguments entry))
         (output    (tool-entry-output entry))
         (failed-p  (eq (tool-entry-status entry) ':failed))
         (command   (entries--argument entry "command"))
         (old-text  (entries--argument entry "old-text" "old_text" "old"))
         (new-text  (entries--argument entry "new-text" "new_text" "new" "content")))
    (make-instance
     'box-block
     :frame-p       nil
     :padding-x     28
     :padding-y     2
     :margin-bottom 10
     :children
     (append
      (cond
        ((stringp command)
         (list (make-code-block command :label "command" :margin-bottom 8)))
        ((or (stringp old-text) (stringp new-text))
         (list (make-code-block ""
                                :label         (let ((path (entries--argument entry "path" "uri")))
                                                 (if (stringp path) path "change"))
                                :spans         (entries--diff-spans (if (stringp old-text) old-text "")
                                                                    (if (stringp new-text) new-text ""))
                                :margin-bottom 8)))
        (arguments
         (list (make-code-block
                (format nil "~{~A~^~%~}"
                        (mapcar (lambda (member)
                                  (format nil "~A: ~A" (first member) (entries--value-text (rest member))))
                                arguments))
                :label         "arguments"
                :margin-bottom 8))))
      (when output
        (multiple-value-bind (sections status) (tool-result-sections (tool-entry-name entry) output)
          (let ((alone-p (null (rest sections))))
            (or (loop for (label . text) in sections
                      for first-p = t then nil
                      collect (make-code-block
                               (entries--preview text)
                               ;; One section is the result; several go by what they captured.
                               :label         (format nil "~A~@[ · ~A~]"
                                                      (cond
                                                        ((not alone-p)
                                                         label)
                                                        (failed-p
                                                         "failed")
                                                        (t
                                                         "result"))
                                                      (and first-p status))
                               :margin-bottom 8))
                ;; A command that printed nothing still says how it ended.
                (and status
                     (list (make-code-block status :label (if failed-p "failed" "result"))))))))))))

(defmethod entry-blocks ((entry tool-entry) toggle)
  "One row naming the tool and what it was asked, opening to its arguments and result; a plan update is drawn as the plan."
  (if (string= (tool-entry-name entry) "plan.update")
      (entries--plan-blocks entry)
      (let* ((status     (tool-entry-status entry))
             (expanded-p (entry-expanded-p entry))
             (seconds    (tool-entry-seconds entry))
             (row        (make-instance
                          'row-block
                          :identifier (list ':entry (entry-identifier entry))
                          :mark       (ecase status
                                        ((:pending :running)
                                         ':spinner)
                                        (:completed
                                         (if expanded-p ':open ':closed))
                                        (:failed
                                         ':cross))
                          :mark-color (and (eq status ':failed) *red*)
                          :title      (spans (list (tool-entry-name entry) (text-style ':label-bold))
                                             (list (format nil "  ~A" (tool-entry-preview entry))
                                                   (text-style ':label)))
                          :trailing   (and seconds (entries--duration seconds))
                          :style      (text-style ':label)
                          :action     toggle)))
        (if expanded-p
            (list row (entries--tool-detail entry))
            (list row)))))


;;;; -- Notices, Output and Markers --

(defmethod entry-blocks ((entry notice-entry) toggle)
  "A quiet line in the interface's voice, with its detail beneath when it has one."
  (declare (ignore toggle))
  (let* ((tone   (notice-entry-tone entry))
         (detail (notice-entry-detail entry))
         (row    (make-instance 'row-block
                                :identifier (list ':entry (entry-identifier entry))
                                :mark       (ecase tone
                                              (:quiet
                                               ':point)
                                              (:success
                                               ':check)
                                              ((:warning :failure)
                                               ':cross))
                                :mark-color (ecase tone
                                              (:quiet
                                               nil)
                                              (:success
                                               *green*)
                                              ((:warning :failure)
                                               *red*))
                                :title      (spans (list (notice-entry-text entry) (text-style ':label)))
                                :trailing   (and (entry-time entry) (entries--clock (entry-time entry)))
                                :style      (text-style ':label)
                                :margin-top 4)))
    (if detail
        (list row
              (make-instance 'box-block
                             :children      (list (make-text-block (list (make-span detail nil))
                                                                   :style (text-style ':small)))
                             :frame-p       nil
                             :padding-x     28
                             :padding-y     0
                             :margin-bottom 8))
        (list row))))

(defmethod entry-blocks ((entry output-entry) toggle)
  "What a command printed, in the typewriter face under the form that printed it."
  (declare (ignore toggle))
  (list (make-code-block (output-entry-text entry)
                         :label         (output-entry-invocation entry)
                         :margin-top    12
                         :margin-bottom 12)))

(defmethod entry-blocks ((entry marker-entry) toggle)
  "A rule across the column carrying the marker's words."
  (declare (ignore toggle))
  (list (make-instance 'rule-block
                       :label         (string-downcase (marker-entry-label entry))
                       :margin-top    18
                       :margin-bottom 18)))
