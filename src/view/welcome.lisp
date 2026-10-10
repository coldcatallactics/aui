(in-package #:aui)

;;;; -- Conversations --

;;; The conversation manager: the first window when AUI starts without a
;;; folder, and the window the Autolith menu opens from a workbench. A
;;; wordmark, a search field, and two short columns: the latest conversations
;;; and the latest folders. Searching lists the matching conversations.

(defparameter *manager-column-width* 360
  "Width of each of the manager's two columns in pixels.")

(defparameter *manager-recent-count* 4
  "How many recent conversations and folders the manager shows at rest.")

(defparameter *manager-search-count* 8
  "How many matching conversations the manager shows while searching.")

(defclass welcome ()
  ((window             :reader welcome-window)
   (search-input       :reader welcome--search-input)
   (conversations-list :reader welcome--conversations-list)
   (folders-list       :reader welcome--folders-list)
   (conversations-heading :reader welcome--conversations-heading)
   (entries            :initarg :entries :accessor welcome-entries
                       :documentation "Every CONVERSATION-ENTRY known, most recent first.")
   (shown              :initform nil :accessor welcome-shown
                       :documentation "The entries the conversation column shows, in list order.")
   (folders            :initform nil :accessor welcome-folders
                       :documentation "(DIRECTORY . LAST-ACTIVITY) pairs the folder column shows.")
   (on-open            :initarg :on-open :reader welcome-on-open
                       :documentation "Called with DIRECTORY and CONVERSATION-ID or NIL when the user chooses."))
  (:documentation "The conversation manager window."))

(-> welcome--heading (t string &key (:face keyword) (:size integer) (:align keyword)) t)
(defun welcome--heading (parent text &key (face ':prose-bold) (size *prose-size*) (align ':left))
  "Create a label under PARENT in FACE at SIZE, aligned ALIGN inside its box."
  (let ((label (lightfast:make-label :parent parent :label text :height *control-height*)))
    (lightfast:set-label-align label align ':inside ':clip)
    (lightfast:set-label-font label (font face))
    (lightfast:set-label-size label size)
    label))

(-> age-text (integer integer) string)
(defun age-text (then now)
  "Return how long ago THEN was relative to NOW, in words."
  (let ((seconds (max 0 (- now then))))
    (cond
      ((< seconds 60) "just now")
      ((< seconds 3600) (format nil "~D min ago" (floor seconds 60)))
      ((< seconds 86400) (format nil "~D h ago" (floor seconds 3600)))
      ((< seconds (* 30 86400)) (format nil "~D d ago" (floor seconds 86400)))
      (t (multiple-value-bind (second minute hour day month year) (decode-universal-time then)
           (declare (ignore second minute hour))
           (format nil "~D-~2,'0D-~2,'0D" year month day))))))

(-> conversation-entry-line (conversation-entry integer) string)
(defun conversation-entry-line (entry now)
  "Return the manager's line for ENTRY: its name, then its age."
  (format nil "~A~:[~;  ·  live~]~C~A"
          (conversation-entry-name entry) (conversation-entry-live-p entry) #\Tab
          (age-text (conversation-entry-activity-key entry) now)))

(-> conversation-entry-search-text (conversation-entry) string)
(defun conversation-entry-search-text (entry)
  "Return the text the search matches for ENTRY."
  (format nil "~A ~A ~@[~A~]" (conversation-entry-name entry) (conversation-entry-directory entry)
          (conversation-entry-model entry)))

(-> filter-conversations (list string) list)
(defun filter-conversations (entries query)
  "Return the ENTRIES matching QUERY, best matches first; all of them in their
own order when QUERY is blank."
  (let ((query (string-trim " " query)))
    (if (string= query "")
        entries
        (mapcar #'cdr
                (stable-sort (loop for entry in entries
                                   for score = (fuzzy-score query (conversation-entry-search-text entry))
                                   when score collect (cons score entry))
                             #'> :key #'car)))))

(-> make-welcome (&key (:entries list) (:on-open function) (:width integer) (:height integer)) welcome)
(defun make-welcome (&key entries on-open (width 1100) (height 720))
  "Create and return the conversation manager; call SHOW on WELCOME-WINDOW to open it."
  (let* ((welcome (make-instance 'welcome :entries entries :on-open on-open))
         (window (lightfast:make-window :width width :height height :label "Autolith" :app-id "aui")))
    (setf (slot-value welcome 'window) window)
    (lightfast:window-escape-closes window t)
    (let* ((wordmark (welcome--heading window "Autolith" :face ':prose-bold :size *wordmark-size* :align ':center))
           (lead (welcome--heading window "Pick up where you left off, or start somewhere new."
                                   :face ':prose-italic :align ':center))
           (search-input (lightfast:make-input :parent window :height (+ *control-height* 4)))
           (new-button (lightfast:make-button :parent window :label "New…" :height (+ *control-height* 4)
                                              :callback (lambda (widget event value)
                                                          (declare (ignore widget event value))
                                                          (welcome--new welcome))))
           (conversations-heading (welcome--heading window "Recent conversations"))
           (folders-heading (welcome--heading window "Recent folders"))
           (conversations-list (lightfast:make-browser :parent window :height 200 :column-widths '(250 0)))
           (folders-list (lightfast:make-browser :parent window :height 200 :column-widths '(250 0))))
      (setf (slot-value welcome 'search-input) search-input
            (slot-value welcome 'conversations-list) conversations-list
            (slot-value welcome 'folders-list) folders-list
            (slot-value welcome 'conversations-heading) conversations-heading)
      (lightfast:set-tooltip search-input "Type to search conversations by name, folder or model; Enter opens the first match")
      (lightfast:set-tooltip new-button "Choose a folder and start a conversation there")
      (lightfast:set-text-font search-input (font ':prose))
      (lightfast:set-text-size search-input *prose-size*)
      (dolist (list (list conversations-list folders-list))
        (lightfast:set-text-font list (font ':prose))
        (lightfast:set-text-size list *prose-size*)
        (lightfast:set-box list lightfast:+box-no-box+)
        (lightfast:browser-set-scrollbars list ':vertical)
        (lightfast:browser-set-line-spacing list *list-line-spacing*))
      (lightfast:set-when search-input ':changed ':enter-key)
      (lightfast:on search-input (lambda (widget event value)
                                   (declare (ignore widget event value))
                                   (if (member (lightfast:event-key) (list *key-enter* *key-keypad-enter*))
                                       (welcome--resume welcome)
                                       (welcome--refresh-list welcome)))
                    :event lightfast:+event-activate+)
      (lightfast:on folders-list (lambda (widget event value)
                                   (declare (ignore widget event value))
                                   (welcome--folder-chosen welcome))
                    :event lightfast:+event-change+)
      (lightfast:on conversations-list (lambda (widget event value)
                                         (declare (ignore widget event value))
                                         (welcome--resume welcome))
                    :event lightfast:+event-change+)
      (welcome-set-entries welcome entries)
      (labels ((blank ()
               (let ((box (lightfast:make-box :parent window :width 1 :height 1)))
                 (lightfast:set-box box lightfast:+box-no-box+)
                 (lightfast:make-layout-item box :basis 0 :grow 1)))
             (centered (height node)
               ;; Flexible blanks on both sides keep NODE in the middle.
               (lightfast:make-layout-row
                :basis height :shrink 0
                :children (list (blank) node (blank)))))
        (lightfast:layout-on-resize
         window
         (lightfast:make-layout-column
          :padding (list (spacing 6) (spacing 5) (spacing 6) (spacing 5)) :gap (spacing 4)
          :children (list (blank)
                          (lightfast:make-layout-item wordmark :basis (+ *wordmark-size* (spacing 3)) :shrink 0)
                          (lightfast:make-layout-item lead :basis *control-height* :shrink 0)
                          (centered (+ *control-height* 4)
                                    (lightfast:make-layout-row
                                     :basis (+ (* 2 *manager-column-width*) (spacing 6)) :shrink 0 :gap (spacing 3)
                                     :children (list (lightfast:make-layout-item search-input :basis 0 :grow 1)
                                                     (lightfast:make-layout-item new-button :basis 120 :shrink 0))))
                          (centered (+ *control-height* (spacing 2) (list-height *manager-search-count* 1000))
                                    (lightfast:make-layout-row
                                     :basis (+ (* 2 *manager-column-width*) (spacing 6)) :shrink 0 :gap (spacing 6)
                                     :children (list (lightfast:make-layout-column
                                                      :basis *manager-column-width* :shrink 0 :gap (spacing 2)
                                                      :children (list (lightfast:make-layout-item conversations-heading
                                                                                                  :basis *control-height* :shrink 0)
                                                                      (lightfast:make-layout-item conversations-list :basis 0 :grow 1)))
                                                     (lightfast:make-layout-column
                                                      :basis *manager-column-width* :shrink 0 :gap (spacing 2)
                                                      :children (list (lightfast:make-layout-item folders-heading
                                                                                                  :basis *control-height* :shrink 0)
                                                                      (lightfast:make-layout-item folders-list :basis 0 :grow 1))))))
                          (let ((bottom (blank)))
                            bottom))))))
    welcome))

(-> welcome-set-entries (welcome list) null)
(defun welcome-set-entries (welcome entries)
  "Show ENTRIES, most recent first, and the latest folders among them."
  (let ((now (get-universal-time)))
    (setf (welcome-entries welcome) (conversations-by-recency entries)
          (welcome-folders welcome) (subseq-bounded (recent-directories entries) *manager-recent-count*))
    (lightfast:clear (welcome--folders-list welcome))
    (dolist (pair (welcome-folders welcome))
      (lightfast:add-item (welcome--folders-list welcome)
                          (format nil "~A~C~A" (folder-tail (first pair)) #\Tab (age-text (rest pair) now))))
    (welcome--refresh-list welcome))
  nil)

(-> subseq-bounded (list integer) list)
(defun subseq-bounded (list count)
  "Return the first COUNT items of LIST, or all of them when there are fewer."
  (if (> (length list) count) (subseq list 0 count) list))

(-> welcome--refresh-list (welcome) null)
(defun welcome--refresh-list (welcome)
  "List the latest conversations, or those matching the search field."
  (let* ((now (get-universal-time))
         (query (string-trim " " (lightfast:value (welcome--search-input welcome))))
         (searching-p (plusp (length query)))
         (shown (subseq-bounded (filter-conversations (welcome-entries welcome) query)
                                (if searching-p *manager-search-count* *manager-recent-count*)))
         (list (welcome--conversations-list welcome)))
    (setf (welcome-shown welcome) shown)
    (lightfast:clear list)
    (dolist (entry shown)
      (lightfast:add-item list (conversation-entry-line entry now)))
    (setf (lightfast:label (welcome--conversations-heading welcome))
          (cond
            ((null (welcome-entries welcome)) "No conversations yet")
            (searching-p (format nil "~D match~:[es~;~]" (length shown) (= (length shown) 1)))
            (t "Recent conversations"))))
  nil)

(-> welcome-focus (welcome) null)
(defun welcome-focus (welcome)
  "Give the search field the keyboard focus."
  (lightfast:take-focus (welcome--search-input welcome))
  nil)

(-> welcome--new (welcome) null)
(defun welcome--new (welcome)
  "Choose a folder with the native chooser and start a conversation there."
  (let ((directory (lightfast:choose-directory :title "Choose a folder for Autolith"
                                               :preset-path (namestring (uiop:getcwd)))))
    (when (and directory (plusp (length directory)) (uiop:directory-exists-p directory))
      (funcall (welcome-on-open welcome) (namestring (uiop:ensure-directory-pathname directory)) nil)))
  nil)

(-> welcome--resume (welcome) null)
(defun welcome--resume (welcome)
  "Open the chosen conversation, or the first match of the search."
  (let* ((index (or (first (lightfast:browser-selected-indices (welcome--conversations-list welcome))) 0))
         (entry (nth index (welcome-shown welcome))))
    (when entry
      (funcall (welcome-on-open welcome) (conversation-entry-directory entry) (conversation-entry-id entry))))
  nil)

(-> welcome--folder-chosen (welcome) null)
(defun welcome--folder-chosen (welcome)
  "Start a new conversation in the chosen folder."
  (let* ((index (first (lightfast:browser-selected-indices (welcome--folders-list welcome))))
         (pair (and index (nth index (welcome-folders welcome)))))
    (when pair
      (funcall (welcome-on-open welcome) (first pair) nil)))
  nil)
