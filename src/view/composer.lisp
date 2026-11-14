(in-package #:aui)

;;;; -- Composer --

;;; Where the person writes: a native text editor in a hairline frame, with
;;; the channel's model, effort and permission mode as chips beneath it and
;;; the send button at the right. Enter sends and Shift+Enter breaks the
;;; line. While the agent works, Enter steers the running turn, Tab queues a
;;; follow-up for after it, and the send button becomes the stop button.

(defparameter *composer-line-height* 29
  "The height of one line of the composer's text.")

(defparameter *composer-maximum-lines* 9
  "How many lines the composer grows to before it scrolls.")

(defparameter *composer-toolbar-height* 34
  "The height of the row of chips and the send button.")

(defparameter *permission-modes*
  '((:ask         "Ask first"   "Ask before running a command it does not recognize")
    (:auto        "Auto"        "A classifier decides: sandbox, full access or refuse")
    (:sandboxed   "Sandbox"     "Run commands inside the workspace sandbox")
    (:full-access "Full access" "Run commands with your full privileges"))
  "The permission modes as (MODE LABEL EXPLANATION), in the order they are offered.")

(-> composer-text (workbench) string)
(defun composer-text (workbench)
  "Return what the composer holds."
  (let ((editor (workbench-editor workbench)))
    (if editor (lightfast:value editor) "")))

(-> (setf composer-text) (string workbench) string)
(defun (setf composer-text) (text workbench)
  "Replace what the composer holds with TEXT."
  (let ((editor (workbench-editor workbench)))
    (when editor
      (setf (lightfast:value editor) text)
      (workbench-redraw workbench)))
  text)

(-> composer-caret-to-end (workbench) null)
(defun composer-caret-to-end (workbench)
  "Put the composer's caret after its last character."
  (let ((editor (workbench-editor workbench)))
    (when editor
      (setf (lightfast:text-insert-position editor) (lightfast:text-length editor))))
  nil)

(-> composer-recall (workbench integer) boolean)
(defun composer-recall (workbench step)
  "Show the text sent STEP places earlier, or later when negative, and return whether there was one to show.

Stepping past the newest returns to an empty composer."
  (let* ((history (workbench-history workbench))
         (index   (workbench-history-index workbench))
         (target  (if index (+ index step) (if (plusp step) 0 -1))))
    (cond
      ((null history)
       nil)
      ((minusp target)
       (when index
         (setf (workbench-history-index workbench) nil
               (composer-text workbench)           ""))
       (and index t))
      ((>= target (length history))
       nil)
      (t
       (setf (workbench-history-index workbench) target
             (composer-text workbench)           (nth target history))
       (composer-caret-to-end workbench)
       t))))

(-> composer-text-height (workbench integer) integer)
(defun composer-text-height (workbench width)
  "Return the height the composer's text needs at WIDTH, between one line and the most it grows to."
  (let* ((text      (composer-text workbench))
         (paragraph (make-paragraph (list (make-span text nil)) :style (text-style ':body)))
         (height    (paragraph-layout paragraph (max 40 (- width 8)))))
    (max *composer-line-height*
         (min (* *composer-maximum-lines* *composer-line-height*) height))))

(-> composer-height (workbench integer) integer)
(defun composer-height (workbench width)
  "Return the height of the whole composer frame at WIDTH: attachments, text and toolbar."
  (+ 10
     (if (workbench-attachments workbench) 30 0)
     (composer-text-height workbench (- width 28))
     6
     *composer-toolbar-height*
     4))

(-> composer-send (workbench &key (:steer-p boolean)) null)
(defun composer-send (workbench &key (steer-p (workbench-working-p workbench)))
  "Send what the composer holds, with its attachments, and empty it when it went."
  (when (workbench-submit workbench (composer-text workbench)
                          :steer-p steer-p
                          :images  (workbench-attachments workbench))
    (setf (composer-text workbench)         ""
          (workbench-attachments workbench) nil))
  nil)

(-> composer-attach (workbench) null)
(defun composer-attach (workbench)
  "Let the person choose image files to send with the next message."
  (let ((files (lightfast:choose-files :title  "Attach images"
                                       :filter (format nil "Images~C*.{png,jpg,jpeg,gif,webp}" #\Tab))))
    (when files
      (setf (workbench-attachments workbench)
            (remove-duplicates (append (workbench-attachments workbench) files) :test #'string=))
      (workbench-redraw workbench)))
  nil)

(-> composer--offered-models (workbench) list)
(defun composer--offered-models (workbench)
  "Return the models to offer: those of providers the person has signed in to, or all of them when none is."
  (let* ((models (workbench-models workbench))
         (usable (remove-if-not (lambda (model)
                                  (portable-true-p (portable-get model "credentialStored")))
                                models)))
    (or usable models)))

(-> composer--choose-model (workbench) null)
(defun composer--choose-model (workbench)
  "Offer the models in a list that can be narrowed by typing, each with its provider, and select the one chosen."
  (let ((models  (composer--offered-models workbench))
        (current (workbench-model workbench)))
    (if (null models)
        (progn
          (setf (workbench-notice workbench) "No model list is available from this channel yet.")
          (workbench-redraw workbench))
        (workbench-pick workbench
                        (mapcar (lambda (model)
                                  (make-choice :label     (or (portable-get model "id") "")
                                               :detail    (portable-string (portable-get model "provider"))
                                               :value     (portable-get model "id")
                                               :current-p (equal (portable-get model "id") current)))
                                models)
                        (lambda (model)
                          (workbench-set-setting workbench "model" model))
                        :title "Model")))
  nil)

(-> composer--setting-options (workbench string) list)
(defun composer--setting-options (workbench name)
  "Return the values the channel offers for the setting NAME, as strings."
  (let ((row (find name (workbench-settings workbench)
                   :key (lambda (row) (portable-get row "name")) :test #'equal)))
    (loop for option in (portable-items (portable-get row "options"))
          collect (if (portable-object-p option)
                      (princ-to-string (or (portable-get option "value") (portable-get option "name")))
                      (princ-to-string option)))))

(-> composer--offer (workbench list function &key (:title string) (:x (or null integer)) (:y integer)) null)
(defun composer--offer (workbench choices action &key (title "Choose") x (y 0))
  "Offer CHOICES and call ACTION with the value taken: in a menu growing upward from X, Y when a chip was clicked there, else in a picker titled TITLE."
  (if x
      (workbench-menu workbench choices :x x :y y :above-p t :action action)
      (workbench-pick workbench choices action :title title))
  nil)

(-> composer--choose-effort (workbench &key (:x (or null integer)) (:y integer)) null)
(defun composer--choose-effort (workbench &key x (y 0))
  "Offer the reasoning efforts the model supports and select the one chosen."
  (let ((current (workbench-effort workbench)))
    (composer--offer workbench
                     (mapcar (lambda (option)
                               (make-choice :label option :value option :current-p (equal option current)))
                             (or (composer--setting-options workbench "reasoning-effort")
                                 '("low" "medium" "high")))
                     (lambda (effort)
                       (workbench-set-setting workbench "reasoning-effort" effort))
                     :title "Reasoning effort"
                     :x     x
                     :y     y))
  nil)

(-> composer--choose-mode (workbench &key (:x (or null integer)) (:y integer)) null)
(defun composer--choose-mode (workbench &key x (y 0))
  "Offer the permission modes, what each means beside it, and select the one chosen."
  (let ((current (workbench-permission-mode workbench)))
    (composer--offer workbench
                     (mapcar (lambda (mode)
                               (make-choice :label     (second mode)
                                            :detail    (third mode)
                                            :value     (first mode)
                                            :current-p (eq (first mode) current)))
                             *permission-modes*)
                     (lambda (mode)
                       (workbench-set-mode workbench mode))
                     :title "Permissions"
                     :x     x
                     :y     y))
  nil)


;;;; -- Editing Keys --

;;; The composer answers to the keys Autolith's line editor does, so hands
;;; that know the terminal find them here: Ctrl-A and Ctrl-E for the ends of
;;; the line, Ctrl-U and Ctrl-K to delete to them, Ctrl-W, Ctrl-Backspace and
;;; Alt-Backspace to delete a word back, Alt-D and Ctrl-Delete a word
;;; forward, Alt-B and Alt-F to move by words.

(-> composer--caret (workbench) integer)
(defun composer--caret (workbench)
  "Return the index of the character the composer's caret stands before."
  (let* ((editor (workbench-editor workbench))
         (text   (composer-text workbench))
         (octets (lightfast:text-insert-position editor))
         (passed 0))
    (loop for index from 0 below (length text)
          do (when (>= passed octets)
               (return index))
             (incf passed (lightfast:character-byte-length (char text index)))
          finally (return (length text)))))

(-> composer--octets (string integer) integer)
(defun composer--octets (text end)
  "Return how many octets the first END characters of TEXT take in the editor."
  (lightfast:string-byte-length (subseq text 0 end)))

(-> composer--delete (workbench integer integer) null)
(defun composer--delete (workbench start end)
  "Delete the composer's characters from START up to END and leave the caret where they were."
  (let ((editor (workbench-editor workbench))
        (text   (composer-text workbench)))
    (when (< start end)
      (lightfast:text-replace editor (composer--octets text start) (composer--octets text end) "")
      (setf (lightfast:text-insert-position editor) (composer--octets text start))
      (workbench-redraw workbench)))
  nil)

(-> composer--move (workbench integer) null)
(defun composer--move (workbench index)
  "Put the composer's caret before the character at INDEX."
  (setf (lightfast:text-insert-position (workbench-editor workbench))
        (composer--octets (composer-text workbench) index))
  nil)

(-> composer--word-start (string integer) integer)
(defun composer--word-start (text index)
  "Return where the word before INDEX in TEXT begins, passing over the space between."
  (let ((start index))
    (loop while (and (plusp start) (member (char text (1- start)) '(#\Space #\Tab #\Newline)))
          do (decf start))
    (loop while (and (plusp start) (not (member (char text (1- start)) '(#\Space #\Tab #\Newline))))
          do (decf start))
    start))

(-> composer--word-end (string integer) integer)
(defun composer--word-end (text index)
  "Return where the word after INDEX in TEXT ends, passing over the space between."
  (let ((end index))
    (loop while (and (< end (length text)) (member (char text end) '(#\Space #\Tab #\Newline)))
          do (incf end))
    (loop while (and (< end (length text)) (not (member (char text end) '(#\Space #\Tab #\Newline))))
          do (incf end))
    end))

(-> composer-edit-key (workbench key-event) boolean)
(defun composer-edit-key (workbench event)
  "Do what EVENT means to Autolith's line editor in the composer, and return whether it meant anything.

Ctrl-K deletes to the end of the line only when there is something there to
delete, so that at the end of the text it still opens the commands."
  (let* ((key       (key-event-key event))
         (control-p (key-event-control-p event))
         (alt-p     (key-event-alt-p event))
         (text      (composer-text workbench))
         (caret     (and (or control-p alt-p) (workbench-editor workbench) (composer--caret workbench)))
         (line      (and caret (1+ (or (position #\Newline text :end caret :from-end t) -1))))
         (line-end  (and caret (or (position #\Newline text :start caret) (length text)))))
    (cond
      ((null caret)
       nil)
      ((or (and control-p (member key '(:backspace #\w)))
           (and alt-p (eq key ':backspace)))
       (composer--delete workbench (composer--word-start text caret) caret)
       t)
      ((or (and alt-p (eql key #\d))
           (and control-p (eq key ':delete)))
       (composer--delete workbench caret (composer--word-end text caret))
       t)
      ((and control-p (eql key #\u))
       (composer--delete workbench line caret)
       t)
      ((and control-p (eql key #\k) (< caret line-end))
       (composer--delete workbench caret line-end)
       t)
      ((and control-p (eql key #\a))
       (composer--move workbench line)
       t)
      ((and control-p (eql key #\e))
       (composer--move workbench line-end)
       t)
      ((and alt-p (or (eql key #\b) (eq key ':left)))
       (composer--move workbench (composer--word-start text caret))
       t)
      ((and alt-p (or (eql key #\f) (eq key ':right)))
       (composer--move workbench (composer--word-end text caret))
       t)
      (t
       nil))))

(-> composer-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun composer-paint (workbench stage &key x y width height)
  "Draw the composer's frame, attachments, chips and send button; the editor itself is a native widget inside."
  (let* ((middle    (- (+ y height) 8 16))
         (chip-y    (- middle 12))
         (cursor    (+ x 8))
         (working-p (workbench-working-p workbench))
         (ready-p   (workbench-attached-p workbench)))
    (paint-fill x y :width width :height height :color *paper*)
    (paint-frame x y :width width :height height)
    (paint-frame (1+ x) (1+ y) :width (- width 2) :height (- height 2))
    ;; Attachments, each removable.
    (let ((chip-x (+ x 10)))
      (loop for pathname in (workbench-attachments workbench)
            do (let ((attached pathname))
                 (incf chip-x
                       (+ 6 (paint-chip stage (list ':attachment attached) (format nil "~A  ×" (file-namestring attached))
                                        :x       chip-x
                                        :y       (+ y 8)
                                        :height  22
                                        :action  (lambda ()
                                                   (setf (workbench-attachments workbench)
                                                         (remove attached (workbench-attachments workbench)
                                                                 :test #'string=))
                                                   (workbench-redraw workbench))
                                        :tooltip "Remove this image"))))))
    ;; Toolbar: attach, then what the next turn runs with. Everything on it
    ;; shares one middle line, eight units above the frame's bottom edge.
    (paint-button stage ':attach ""
                  :x         cursor
                  :y         (- middle 14)
                  :width     28
                  :height    28
                  :mark      ':plus
                  :mark-size 12
                  :action    (lambda () (composer-attach workbench))
                  :tooltip   "Attach images")
    (incf cursor 36)
    (when ready-p
      (incf cursor (+ 2 (paint-chip stage ':model (or (workbench-model workbench) "model")
                                    :x       cursor
                                    :y       chip-y
                                    :height  24
                                    :menu-p  t
                                    :action  (lambda () (composer--choose-model workbench))
                                    :tooltip "The model that answers")))
      (when (workbench-effort workbench)
        (let ((chip-x cursor))
          (incf cursor (+ 2 (paint-chip stage ':effort (workbench-effort workbench)
                                        :x       cursor
                                        :y       chip-y
                                        :height  24
                                        :menu-p  t
                                        :action  (lambda ()
                                                   (composer--choose-effort workbench :x chip-x :y (- chip-y 4)))
                                        :tooltip "How much the model reasons")))))
      (let ((mode   (find (workbench-permission-mode workbench) *permission-modes* :key #'first))
            (chip-x cursor))
        (when (< (+ cursor 150) (- (+ x width) 50))
          (paint-chip stage ':mode (if mode (second mode) "Ask first")
                      :x       cursor
                      :y       chip-y
                      :height  24
                      :menu-p  t
                      :action  (lambda ()
                                 (composer--choose-mode workbench :x chip-x :y (- chip-y 4)))
                      :tooltip (and mode (third mode))))))
    ;; Send, or stop while the agent works: as far from the right edge as from the bottom.
    (let ((button-x (- (+ x width) 32 8)))
      (if (and working-p (zerop (length (string-trim '(#\Space #\Newline) (composer-text workbench)))))
          (paint-button stage ':stop ""
                        :x         button-x
                        :y         (- middle 16)
                        :width     32
                        :height    32
                        :mark      ':square
                        :primary-p t
                        :action    (lambda () (workbench-stop workbench))
                        :tooltip   "Stop the agent (Esc)")
          (paint-button stage ':send ""
                        :x         button-x
                        :y         (- middle 16)
                        :width     32
                        :height    32
                        :mark      ':arrow-up
                        :primary-p t
                        :enabled-p ready-p
                        :action    (lambda () (composer-send workbench))
                        :tooltip   (if working-p
                                       "Steer the running turn (Enter); Tab queues a follow-up"
                                       "Send (Enter)")))))
  nil)

(-> composer-editor-box (workbench &key (:x integer) (:y integer) (:width integer) (:height integer)) list)
(defun composer-editor-box (workbench &key x y width height)
  "Return the (X Y WIDTH HEIGHT) of the editor inside a composer frame of the given box."
  (let ((top (+ y 8 (if (workbench-attachments workbench) 30 0))))
    (list (+ x 12)
          top
          (- width 24)
          (max *composer-line-height*
               (- (+ y height) top *composer-toolbar-height* 8)))))
