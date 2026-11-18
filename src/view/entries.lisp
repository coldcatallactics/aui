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

(defvar *entry-channel-opener* nil
  "Called with a channel's identifier to show that channel. The window sets it; without one a message does not lead to the channel it came from.")

(-> entries--message-body (user-entry) list)
(defun entries--message-body (entry)
  "Return the blocks of what ENTRY's message holds: its images, then its text, which is Markdown whose line breaks are meant."
  (let ((text (user-entry-text entry)))
    (append
     (mapcar (lambda (pathname)
               (make-instance 'image-block :pathname pathname :margin-bottom 8))
             (user-entry-images entry))
     (when (plusp (length text))
       (let ((blocks (prose-blocks text :breaks-p t)))
         (when blocks
           (setf (block-margin-top (first blocks))           0
                 (block-margin-bottom (first (last blocks))) 0))
         blocks)))))

(-> entries--participant-blocks (user-entry list) list)
(defun entries--participant-blocks (entry body)
  "Return the blocks of a message from another participant: its name in large letters, BODY, and a button that leads to the channel it wrote from."
  (let ((origin (user-entry-origin entry))
        (opener *entry-channel-opener*)
        (name   (text-style ':heading-2)))
    (list (make-instance
           'box-block
           :children      (append
                           (list (make-text-block (spans (list (user-entry-sender entry) name))
                                                  :style name :margin-bottom 4))
                           body
                           (when (and origin opener)
                             (list (make-instance 'button-block
                                                  :identifier (list ':entry-channel (entry-identifier entry))
                                                  :label      (format nil "Go to ~A" (channel-display-name origin))
                                                  :action     (lambda () (funcall opener origin))
                                                  :margin-top 10))))
           :frame-p       nil
           :padding-x     0
           :padding-y     0
           :margin-top    22
           :margin-bottom 22))))

(defmethod entry-blocks ((entry user-entry) toggle)
  "The person's message in a framed box at the right, as in any chat. A message from another participant stands at the left without a frame, under its name."
  (declare (ignore toggle))
  (let ((body (entries--message-body entry)))
    (if (user-entry-sender entry)
        (entries--participant-blocks entry body)
        (list (make-instance 'box-block
                             :children      (or body (list (make-text-block (list (make-span "" nil)))))
                             :align         ':right
                             :fraction      85/100
                             :padding-x     16
                             :padding-y     10
                             :margin-top    22
                             :margin-bottom 22)))))

(defvar *answers-under-way* (make-hash-table :test #'eq :weakness ':key)
  "For each answer still streaming, how much of its text is set in blocks already and those blocks, as (END . BLOCKS).")

(-> entries--streaming-blocks (assistant-entry) list)
(defun entries--streaming-blocks (entry)
  "Return the blocks of ENTRY's text so far, typesetting only what came after the last settled part.

An answer grows by a few words at a time, and all of it but the paragraph
being written reads the same as it did, so its blocks are kept."
  (let* ((text    (assistant-entry-text entry))
         (known   (gethash entry *answers-under-way*))
         (from    (if known (first known) 0))
         (end     (markdown-settled-end text :start from))
         (settled (append (rest known) (and (> end from) (prose-blocks (subseq text from end))))))
    (setf (gethash entry *answers-under-way*) (cons end settled))
    (append settled (prose-blocks (subseq text end)))))

(defmethod entry-blocks ((entry assistant-entry) toggle)
  "The agent's answer as prose; one that has finished is read whole."
  (declare (ignore toggle))
  (let ((blocks (if (assistant-entry-streaming-p entry)
                    (entries--streaming-blocks entry)
                    (progn
                      (remhash entry *answers-under-way*)
                      (prose-blocks (assistant-entry-text entry))))))
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
                                    ;; The summary's own emphasis shows; its markers do not.
                                    :title      (cons (make-span (if (reasoning-entry-streaming-p entry)
                                                                     "Thinking  "
                                                                     "Thought  ")
                                                                 label)
                                                      (prose-spans
                                                       (markdown-inline
                                                        (entries--one-line
                                                         (subseq text 0 (min 200 (length text)))))
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

(-> entries--plan-steps (tool-entry) list)
(defun entries--plan-steps (entry)
  "Return the blocks of what a plan update holds: its explanation, then its steps as a checklist with the one in hand set apart."
  (let* ((arguments   (tool-entry-arguments entry))
         (explanation (rest (assoc "explanation" arguments :test #'string=)))
         (steps       (rest (or (assoc "plan" arguments :test #'string=)
                                (assoc "steps" arguments :test #'string=))))
         (style       (text-style ':list)))
    (append
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
                                          (text-style ':list-bold :color *blue*))
                                         ((member status '("completed" "done") :test #'equal)
                                          (text-style ':list :strike-p t))
                                         (t
                                          nil))))
                      :style  style
                      :indent 26
                      :marker (if (member status '("completed" "done") :test #'equal) ':done ':open)
                      :margin-top 2 :margin-bottom 2)))))

(-> entries--plan-blocks (tool-entry) list)
(defun entries--plan-blocks (entry)
  "Return the blocks of a plan update in the conversation: a framed box headed Plan."
  (list
   (make-instance
    'box-block
    :padding-x     14
    :padding-y     10
    :margin-top    10
    :margin-bottom 10
    :children      (cons (make-text-block (spans (list "Plan" (text-style ':label-bold)))
                                          :style (text-style ':label-bold) :margin-bottom 4)
                         (entries--plan-steps entry)))))

(defparameter *diff-preview-lines* 40
  "How many lines of each side of an edit an opened row shows.")

(-> entries--diff-spans (string string) list)
(defun entries--diff-spans (old new)
  "Return the spans of an edit as a diff: the lines of OLD marked removed, then the lines of NEW marked added.

What is removed is red and what is added green, marks and text alike."
  (let ((removed (text-style ':code :color *red*))
        (added   (text-style ':code :color *green*))
        (spans   nil))
    (flet ((side (text mark style)
             (let ((lines (markdown-lines (string-right-trim '(#\Newline) text))))
               (loop for line in lines
                     for index from 0
                     while (< index *diff-preview-lines*)
                     do (push (make-span (format nil "~A~A~%" mark line) style) spans))
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

(defparameter *lisp-argument-names* '("forms" "form" "definition" "source" "expression" "code")
  "The argument names under which a tool that evaluates Lisp takes its source.")

(-> entries--lisp-tool-p (tool-entry) boolean)
(defun entries--lisp-tool-p (entry)
  "Return T when ENTRY's tool takes and returns Lisp: the evaluators of the workspace's image and of Autolith's own."
  (let ((name (tool-entry-name entry)))
    (and (or (uiop:string-prefix-p "lisp." name)
             (uiop:string-prefix-p "self." name))
         t)))

(-> entries--lisp-source (tool-entry) (values (or null string) (or null string)))
(defun entries--lisp-source (entry)
  "Return the Lisp source ENTRY's call carries, one form a line and set out where it came on one long line, and the argument it came under; NIL twice when it carries none."
  (loop for name in *lisp-argument-names*
        for value = (entries--argument entry name)
        when value
          do (return (values (lisp-format (if (and (listp value) (every #'stringp value))
                                              (format nil "~{~A~^~%~}" value)
                                              (entries--value-text value)))
                             name))
        finally (return (values nil nil))))

(-> entries--lisp-output (string) string)
(defun entries--lisp-output (text)
  "Return TEXT, what a Lisp tool printed, with each long one-line form in it set out on several lines."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (let ((trimmed (string-left-trim '(#\Space) line)))
                      (if (and (plusp (length trimmed)) (find (char trimmed 0) "(#'"))
                          (lisp-format line)
                          line)))
                  (markdown-lines text))))

(-> entries--result-blocks (tool-entry string) list)
(defun entries--result-blocks (entry output)
  "Return the blocks of what ENTRY's call returned: a shell command's captures without the bookkeeping, a patch as a patch, Lisp as Lisp."
  (let ((failed-p (eq (tool-entry-status entry) ':failed))
        (lisp-p   (entries--lisp-tool-p entry)))
    (multiple-value-bind (sections status) (tool-result-sections (tool-entry-name entry) output)
      (let ((alone-p (null (rest sections))))
        (or (loop for (label . text) in sections
                  for first-p = t then nil
                  collect (let ((shown (entries--preview (if lisp-p (entries--lisp-output text) text))))
                            (make-code-block
                             shown
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
                             :spans         (cond
                                              (lisp-p
                                               (code-spans shown "lisp"))
                                              ((patch-p shown)
                                               (patch-spans shown))
                                              (t
                                               nil))
                             :margin-bottom 8)))
            ;; A command that printed nothing still says how it ended.
            (and status
                 (list (make-code-block status :label (if failed-p "failed" "result")))))))))

(-> entries--argument-blocks (list) list)
(defun entries--argument-blocks (arguments)
  "Return the blocks that list ARGUMENTS, an association list of names and decoded values, one to a line; none when there are none."
  (when arguments
    (list (make-code-block
           (format nil "~{~A~^~%~}"
                   (mapcar (lambda (member)
                             (format nil "~A: ~A" (first member) (entries--value-text (rest member))))
                           arguments))
           :label         "arguments"
           :margin-bottom 8))))

(-> entries--tool-detail (tool-entry) flow-block)
(defun entries--tool-detail (entry)
  "Return the opened detail of ENTRY's call: what it was asked, as a command, Lisp source, a diff or its arguments, and what it returned."
  (let* ((arguments (tool-entry-arguments entry))
         (output    (tool-entry-output entry))
         (command   (entries--argument entry "command"))
         (old-text  (entries--argument entry "old-text" "old_text" "old"))
         (new-text  (entries--argument entry "new-text" "new_text" "new" "content")))
    (multiple-value-bind (source source-name) (if (entries--lisp-tool-p entry)
                                                  (entries--lisp-source entry)
                                                  (values nil nil))
      (make-instance
       'box-block
       :frame-p       nil
       :padding-x     28
       :padding-y     2
       :margin-bottom 10
       :children
       (append
        (cond
          ;; A command, and whatever else the call was given, such as the
          ;; shell session it runs in.
          ((stringp command)
           (cons (make-code-block command
                                  :label         "command"
                                  :spans         (code-spans command "bash")
                                  :margin-bottom 8)
                 (entries--argument-blocks (remove "command" arguments :key #'first :test #'equal))))
          (source
           (cons (make-code-block source
                                  :label         source-name
                                  :spans         (code-spans source "lisp")
                                  :margin-bottom 8)
                 (entries--argument-blocks (remove source-name arguments :key #'first :test #'equal))))
          ((or (stringp old-text) (stringp new-text))
           (let* ((path (entries--argument entry "path" "uri"))
                  (old  (if (stringp old-text) old-text ""))
                  (new  (if (stringp new-text) new-text "")))
             (list (make-code-block ""
                                    :label         (if (stringp path) path "change")
                                    :spans         (or (edit-spans old new :pathname (and (stringp path) path))
                                                       (entries--diff-spans old new))
                                    :margin-bottom 8))))
          (t
           (entries--argument-blocks arguments)))
        (when output
          (entries--result-blocks entry output)))))))

(-> entries--preview-spans (tool-entry) list)
(defun entries--preview-spans (entry)
  "Return the spans of ENTRY's one-line preview: a command colored as shell, Lisp as Lisp, anything else plain."
  (let* ((preview  (tool-entry-preview entry))
         (language (cond
                     ((stringp (entries--argument entry "command"))
                      "bash")
                     ((entries--lisp-tool-p entry)
                      "lisp")
                     (t
                      nil)))
         (parts    (and language (source-highlight preview :language language))))
    (if parts
        (loop for (role . text) in parts
              collect (let ((style (code-role-style role)))
                        ;; The row is set smaller than a code block; the ink carries over.
                        (make-span text (if style
                                            (text-style ':label
                                                        :face  (text-style-face style)
                                                        :color (text-style-color style))
                                            (text-style ':label)))))
        (list (make-span preview (text-style ':label))))))

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
                          :title      (cons (make-span (format nil "~A  " (tool-entry-name entry))
                                                       (text-style ':label-bold
                                                                   :color (if (eq status ':failed) *red* *amber*)))
                                            (entries--preview-spans entry))
                          :trailing   (and seconds (entries--duration seconds))
                          :style      (text-style ':label)
                          :action     toggle)))
        (if expanded-p
            (list row (entries--tool-detail entry))
            (list row)))))


;;;; -- Notices, Output and Markers --

(-> entries--detail-sections (string) (values list (or null string)))
(defun entries--detail-sections (detail)
  "Return the sections a notice's DETAIL shows and the status line it opens with.

What a finished job reports is a shell result, read as a tool's is; anything
else is one section."
  (if (and (> (length detail) 5) (string= "exit " detail :end2 5))
      (tool-result-sections "shell.run" detail)
      (values (list (cons "detail" detail)) nil)))

(-> entries--last-line (string) string)
(defun entries--last-line (text)
  "Return the last line of TEXT that says anything."
  (or (find-if (lambda (line)
                 (plusp (length (string-trim '(#\Space #\Tab) line))))
               (markdown-lines text)
               :from-end t)
      ""))

(defmethod entry-blocks ((entry notice-entry) toggle)
  "A quiet line in the interface's voice. What it has to add stays closed behind the line, which shows its last word and opens on a click."
  (let* ((tone       (notice-entry-tone entry))
         (detail     (notice-entry-detail entry))
         (expanded-p (and detail (entry-expanded-p entry)))
         (color      (ecase tone
                       (:quiet
                        nil)
                       (:success
                        *green*)
                       (:warning
                        *amber*)
                       (:failure
                        *red*))))
    (multiple-value-bind (sections status) (if detail (entries--detail-sections detail) (values nil nil))
      (let* ((preview (and detail
                           (not expanded-p)
                           (format nil "~@[~A  ~]~A" status
                                   (entries--one-line
                                    (entries--last-line (or (rest (first (last sections))) ""))))))
             (row     (make-instance 'row-block
                                     :identifier (list ':entry (entry-identifier entry))
                                     :mark       (cond
                                                   (expanded-p
                                                    ':open)
                                                   (detail
                                                    ':closed)
                                                   (t
                                                    (ecase tone
                                                      (:quiet
                                                       ':point)
                                                      (:success
                                                       ':check)
                                                      ((:warning :failure)
                                                       ':cross))))
                                     :mark-color color
                                     :title      (spans (list (notice-entry-text entry)
                                                              (text-style ':label :color color))
                                                        (and preview
                                                             (list (format nil "  ~A" preview)
                                                                   (text-style ':label :face ':mono-italic))))
                                     :trailing   (and (entry-time entry) (entries--clock (entry-time entry)))
                                     :style      (text-style ':label)
                                     ;; A notice with more to say opens; one about
                                     ;; another channel leads there.
                                     :action     (cond
                                                   (detail
                                                    toggle)
                                                   ((and (notice-entry-channel entry) *entry-channel-opener*)
                                                    (let ((opener  *entry-channel-opener*)
                                                          (channel (notice-entry-channel entry)))
                                                      (lambda () (funcall opener channel)))))
                                     :margin-top 4)))
        (if expanded-p
            (list row
                  (make-instance 'box-block
                                 :children      (loop for (label . text) in sections
                                                      for first-p = t then nil
                                                      collect (make-code-block
                                                               (entries--preview text)
                                                               :label         (if (and first-p status)
                                                                                  (format nil "~A · ~A" label status)
                                                                                  label)
                                                               :margin-bottom 4))
                                 :frame-p       nil
                                 :padding-x     28
                                 :padding-y     2
                                 :margin-bottom 8))
            (list row))))))

(defmethod entry-blocks ((entry schemas-entry) toggle)
  "A quiet line naming the groups of tools the agent looked up."
  (declare (ignore toggle))
  (list (make-instance 'row-block
                       :identifier (list ':entry (entry-identifier entry))
                       :mark       ':point
                       :title      (cons (make-span "Tool schemas  " (text-style ':label))
                                         (loop for name in (schemas-entry-names entry)
                                               for first-p = t then nil
                                               unless first-p
                                                 collect (make-span ", " (text-style ':label))
                                               collect (make-span name (text-style ':label :color *blue*))))
                       :style      (text-style ':label)
                       :margin-top 4)))

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
