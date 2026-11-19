(in-package #:aui)

;;;; -- Entries as Blocks --

;;; The drawing of each kind of transcript entry. A message from the person is
;;; a framed box at the right. An answer is prose at the full width. Tool calls,
;;; reasoning and notices are one-line rows that open on a click.

(defparameter *tool-preview-keys*
  '("command" "forms" "form" "uri" "path" "query" "pattern" "patterns" "url" "system" "name" "task" "text")
  "A list of the argument names that are tried in order for the one-line preview of a tool call.")

(defparameter *output-preview-lines* 14
  "The number of lines shown from the detail of an opened notice.")

(defparameter *output-open-lines* 200
  "The number of lines shown from the result of an opened tool call.")

(defgeneric entry-blocks (entry toggle)
  (:documentation "Return the blocks that draw ENTRY. TOGGLE is a function of no arguments that opens or closes the detail of the entry."))

(-> entries--value-text (t) string)
(defun entries--value-text (value)
  "Return the decoded JSON VALUE as text on one line."
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
  "Return TEXT with every run of whitespace replaced by one space. Line breaks count as whitespace."
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
  "Return the one-line preview of the tool call ENTRY. The preview is the command, path, query or first argument."
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
  "Return SECONDS as a readable duration, such as 0.4s, 12s or 3m 05s."
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
  "A function that takes the identifier of a channel and shows that channel, or NIL.

If it is NIL, messages and notices do not lead to the channel they came from.")

(-> entries--message-body (user-entry) list)
(defun entries--message-body (entry)
  "Return the blocks of the message ENTRY. The images come first, then the text.

The text is Markdown, and the line breaks in it are kept."
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
  "Return the blocks of the message ENTRY from another participant.

The blocks are the name of the sender in large letters, BODY, and a button that
leads to the channel the message came from."
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
                                                  :label      (format nil "go to ~A" (channel-display-name origin))
                                                  :action     (lambda () (funcall opener origin))
                                                  :margin-top 10))))
           :frame-p       nil
           :padding-x     0
           :padding-y     0
           :margin-top    22
           :margin-bottom 22))))

(defmethod entry-blocks ((entry user-entry) toggle)
  "Draw a message from the person in a framed box at the right.

Draw a message from another participant at the left without a frame, under the name of the sender."
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
  "A table of the answers that are still streaming. The value for each answer is (END . BLOCKS).

BLOCKS are the finished blocks of the answer, and END is the position in the text where they end.")

(-> entries--streaming-blocks (assistant-entry) list)
(defun entries--streaming-blocks (entry)
  "Return the blocks of the text of ENTRY so far.

The blocks of the settled part of the text are kept between calls. Only the text
after the settled part is typeset again."
  (let* ((text    (assistant-entry-text entry))
         (known   (gethash entry *answers-under-way*))
         (from    (if known (first known) 0))
         (end     (markdown-settled-end text :start from))
         (settled (append (rest known) (and (> end from) (prose-blocks (subseq text from end))))))
    (setf (gethash entry *answers-under-way*) (cons end settled))
    (append settled (prose-blocks (subseq text end)))))

(defmethod entry-blocks ((entry assistant-entry) toggle)
  "Draw the answer of the agent as prose. A finished answer is typeset whole."
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
  "Draw the reasoning of the agent as a closed row. The reasoning is shown in small italics in the opened row."
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
                                    ;; The emphasis of the summary is drawn without the Markdown markers.
                                    :title      (cons (make-span (if (reasoning-entry-streaming-p entry)
                                                                     "thinking  "
                                                                     "thought  ")
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
  "Return the blocks of the plan update ENTRY.

The explanation comes first. The steps follow as a checklist, and the current step is bold blue."
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
  "Return the blocks of the plan update ENTRY, which are a framed box with the heading plan."
  (list
   (make-instance
    'box-block
    :padding-x     14
    :padding-y     10
    :margin-top    10
    :margin-bottom 10
    :children      (cons (make-text-block (spans (list "plan" (text-style ':label-bold)))
                                          :style (text-style ':label-bold) :margin-bottom 4)
                         (entries--plan-steps entry)))))

(defparameter *diff-preview-lines* 40
  "The number of lines shown from each side of an edit in an opened row.")

(-> entries--diff-spans (string string) list)
(defun entries--diff-spans (old new)
  "Return the spans of an edit as a diff. The lines of OLD are marked as removed, then the lines of NEW are marked as added.

Removed lines are red and added lines are green."
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
                                  (text-style ':code :face ':mono-italic))
                       spans)))))
      (when (plusp (length old))
        (side old "- " removed))
      (when (plusp (length new))
        (side new "+ " added)))
    (nreverse spans)))

(-> entries--argument (tool-entry &rest string) t)
(defun entries--argument (entry &rest names)
  "Return the value of the first argument of ENTRY that has one of NAMES, or NIL."
  (loop for name in names
        for member = (assoc name (tool-entry-arguments entry) :test #'string=)
        when member
          do (return (rest member))))

(-> entries--preview (string &key (:lines integer)) string)
(defun entries--preview (text &key (lines *output-preview-lines*))
  "Return the first LINES lines of TEXT and the number of lines that are left out."
  (let* ((limit lines)
         (lines (markdown-lines (string-right-trim '(#\Newline #\Space) text)))
         (shown (if (> (length lines) limit)
                    (subseq lines 0 limit)
                    lines)))
    (format nil "~{~A~^~%~}~@[~%… ~D more lines~]" shown
            (and (> (length lines) (length shown)) (- (length lines) (length shown))))))

(defparameter *lisp-argument-names* '("forms" "form" "definition" "source" "expression" "code")
  "A list of the argument names that hold the source for tools that evaluate Lisp.")

(-> entries--lisp-tool-p (tool-entry) boolean)
(defun entries--lisp-tool-p (entry)
  "Return T if the tool of ENTRY takes and returns Lisp. These tools are the evaluators of the workspace image and of Autolith."
  (let ((name (tool-entry-name entry)))
    (and (or (uiop:string-prefix-p "lisp." name)
             (uiop:string-prefix-p "self." name))
         t)))

(-> entries--lisp-source (tool-entry) (values (or null string) (or null string)))
(defun entries--lisp-source (entry)
  "Return the Lisp source of the tool call ENTRY and the name of the argument that contains it.

The source has one form on each line, and long one-line forms are laid out on
several lines. Return NIL twice if the call has no Lisp source."
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
  "Return TEXT, the output of a Lisp tool, with each long one-line form laid out on several lines."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (let ((trimmed (string-left-trim '(#\Space) line)))
                      (if (and (plusp (length trimmed)) (find (char trimmed 0) "(#'"))
                          (lisp-format line)
                          line)))
                  (markdown-lines text))))

(-> entries--result-blocks (tool-entry string) list)
(defun entries--result-blocks (entry output)
  "Return the blocks of OUTPUT, the result of the tool call ENTRY.

The output of a shell command is split into the captured streams. Patches and
Lisp are highlighted."
  (let ((failed-p (eq (tool-entry-status entry) ':failed))
        (lisp-p   (entries--lisp-tool-p entry)))
    (multiple-value-bind (sections status) (tool-result-sections (tool-entry-name entry) output)
      (let ((alone-p (null (rest sections))))
        (or (loop for (label . text) in sections
                  for first-p = t then nil
                  collect (let ((shown (entries--preview (if lisp-p (entries--lisp-output text) text)
                                                         :lines *output-open-lines*)))
                            (make-code-block
                             shown
                             ;; One section is labelled as the result. Several are labelled by stream.
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
            ;; A command without output is shown with the status line.
            (and status
                 (list (make-code-block status :label (if failed-p "failed" "result")))))))))

(-> entries--argument-blocks (list) list)
(defun entries--argument-blocks (arguments)
  "Return the blocks that list ARGUMENTS, one on each line, or NIL if there are no arguments.

ARGUMENTS is an association list of names and decoded values."
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
  "Return the opened detail of the tool call ENTRY.

The detail contains the input of the call and the result. The input is shown as a
command, Lisp source, a diff or a list of arguments."
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
          ;; A command and the other arguments of the call, such as the
          ;; shell session.
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
  "Return the spans of the one-line preview of ENTRY.

Commands are highlighted as shell, Lisp is highlighted as Lisp, and the rest is plain."
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
                        ;; The row is smaller than a code block. However, it has the same inks.
                        (make-span text (if style
                                            (text-style ':label
                                                        :face  (text-style-face style)
                                                        :color (text-style-color style))
                                            (text-style ':label)))))
        (list (make-span preview (text-style ':label))))))

(defmethod entry-blocks ((entry tool-entry) toggle)
  "Draw a tool call as one row that contains the name of the tool and the preview.

The arguments and the result are shown in the opened row. A plan update is drawn as the plan."
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
  "Return the sections of DETAIL, the detail of a notice, and the status line.

The detail of a finished job is split like the result of a shell command.
Any other detail is one section."
  (if (and (> (length detail) 5) (string= "exit " detail :end2 5))
      (tool-result-sections "shell.run" detail)
      (values (list (cons "detail" detail)) nil)))

(-> entries--last-line (string) string)
(defun entries--last-line (text)
  "Return the last nonblank line of TEXT."
  (or (find-if (lambda (line)
                 (plusp (length (string-trim '(#\Space #\Tab) line))))
               (markdown-lines text)
               :from-end t)
      ""))

(defmethod entry-blocks ((entry notice-entry) toggle)
  "Draw a notice as a quiet row in the voice of the window.

If the notice has detail, the last line of the detail is shown in the closed row. The row opens on a click."
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
                                     ;; A notice with detail opens. A notice about
                                     ;; another channel leads to that channel.
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
  "Draw a quiet row that lists the groups of tools looked up by the agent."
  (declare (ignore toggle))
  (list (make-instance 'row-block
                       :identifier (list ':entry (entry-identifier entry))
                       :mark       ':point
                       :title      (cons (make-span "tool schemas  " (text-style ':label))
                                         (loop for name in (schemas-entry-names entry)
                                               for first-p = t then nil
                                               unless first-p
                                                 collect (make-span ", " (text-style ':label))
                                               collect (make-span name (text-style ':label :color *blue*))))
                       :style      (text-style ':label)
                       :margin-top 4)))

(defmethod entry-blocks ((entry output-entry) toggle)
  "Draw the output of a command in the typewriter face, under the form that printed it."
  (declare (ignore toggle))
  (list (make-code-block (output-entry-text entry)
                         :label         (output-entry-invocation entry)
                         :margin-top    12
                         :margin-bottom 12)))

(defmethod entry-blocks ((entry marker-entry) toggle)
  "Draw the marker as a rule across the column, with the label of the marker."
  (declare (ignore toggle))
  (list (make-instance 'rule-block
                       :label         (string-downcase (marker-entry-label entry))
                       :margin-top    18
                       :margin-bottom 18)))
