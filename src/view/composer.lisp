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

(-> composer--choose-model (workbench) null)
(defun composer--choose-model (workbench)
  "Offer the models grouped by provider and select the one chosen."
  (let* ((models (workbench-models workbench))
         (items  (mapcar (lambda (model)
                           (format nil "~A/~A" (or (portable-get model "provider") "models")
                                   (portable-get model "id")))
                         models)))
    (if (null items)
        (setf (workbench-notice workbench) "No model list is available from this channel yet.")
        (let ((choice (lightfast:popup-menu items)))
          (when choice
            (workbench-set-setting workbench "model" (portable-get (nth choice models) "id"))))))
  (workbench-redraw workbench)
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

(-> composer--choose-effort (workbench) null)
(defun composer--choose-effort (workbench)
  "Offer the reasoning efforts the model supports and select the one chosen."
  (let ((options (or (composer--setting-options workbench "reasoning-effort")
                     '("low" "medium" "high"))))
    (let ((choice (lightfast:popup-menu options)))
      (when choice
        (workbench-set-setting workbench "reasoning-effort" (nth choice options)))))
  nil)

(-> composer--choose-mode (workbench) null)
(defun composer--choose-mode (workbench)
  "Offer the permission modes and select the one chosen."
  (let ((choice (lightfast:popup-menu
                 (mapcar (lambda (mode) (format nil "~A: ~A" (second mode) (third mode)))
                         *permission-modes*))))
    (when choice
      (workbench-set-mode workbench (first (nth choice *permission-modes*)))))
  nil)

(-> composer-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun composer-paint (workbench stage &key x y width height)
  "Draw the composer's frame, attachments, chips and send button; the editor itself is a native widget inside."
  (let* ((toolbar-y (- (+ y height) *composer-toolbar-height* 4))
         (chip-y    (+ toolbar-y 5))
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
    ;; Toolbar: attach, then what the next turn runs with.
    (paint-button stage ':attach ""
                  :x       cursor
                  :y       (+ toolbar-y 3)
                  :width   28
                  :height  28
                  :mark    ':plus
                  :action  (lambda () (composer-attach workbench))
                  :tooltip "Attach images")
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
        (incf cursor (+ 2 (paint-chip stage ':effort (workbench-effort workbench)
                                      :x       cursor
                                      :y       chip-y
                                      :height  24
                                      :menu-p  t
                                      :action  (lambda () (composer--choose-effort workbench))
                                      :tooltip "How much the model reasons"))))
      (let ((mode (find (workbench-permission-mode workbench) *permission-modes* :key #'first)))
        (when (< (+ cursor 150) (- (+ x width) 50))
          (paint-chip stage ':mode (if mode (second mode) "Ask first")
                      :x       cursor
                      :y       chip-y
                      :height  24
                      :menu-p  t
                      :action  (lambda () (composer--choose-mode workbench))
                      :tooltip (and mode (third mode))))))
    ;; Send, or stop while the agent works.
    (let ((button-x (- (+ x width) 42)))
      (if (and working-p (zerop (length (string-trim '(#\Space #\Newline) (composer-text workbench)))))
          (paint-button stage ':stop ""
                        :x         button-x
                        :y         (+ toolbar-y 1)
                        :width     32
                        :height    32
                        :mark      ':square
                        :primary-p t
                        :action    (lambda () (workbench-stop workbench))
                        :tooltip   "Stop the agent (Esc)")
          (paint-button stage ':send ""
                        :x         button-x
                        :y         (+ toolbar-y 1)
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
