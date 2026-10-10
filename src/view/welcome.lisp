(in-package #:aui)

;;;; -- Conversations --

;;; The conversation manager: the first window when AUI starts without a
;;; folder, and the window the Autolith menu opens from a workbench. It lists
;;; every known conversation by recency with a search field, and starts new
;;; conversations in any folder.

(defparameter *conversation-columns* '(("Conversation" . 400) ("Folder" . 300) ("Model" . 180) ("Last activity" . 0))
  "The list's columns as (HEADING . WIDTH); a zero width takes the rest.")

(defclass welcome ()
  ((window             :reader welcome-window)
   (search-input       :reader welcome--search-input)
   (path-input         :reader welcome--path-input)
   (conversations-list :reader welcome--conversations-list)
   (folders-list       :reader welcome--folders-list)
   (count-label        :reader welcome--count-label)
   (entries            :initarg :entries :accessor welcome-entries
                       :documentation "Every CONVERSATION-ENTRY known, most recent first.")
   (shown              :initform nil :accessor welcome-shown
                       :documentation "The entries the list shows after the search, in list order.")
   (folders            :initform nil :accessor welcome-folders
                       :documentation "(DIRECTORY . LAST-ACTIVITY) pairs listed, most recent first.")
   (on-open            :initarg :on-open :reader welcome-on-open
                       :documentation "Called with DIRECTORY and CONVERSATION-ID or NIL when the user chooses."))
  (:documentation "The conversation manager window."))

(-> welcome--heading (t string &key (:face keyword) (:size integer)) t)
(defun welcome--heading (parent text &key (face ':prose-bold) (size *prose-size*))
  "Create a left-aligned label under PARENT in FACE at SIZE."
  (let ((label (lightfast:make-label :parent parent :label text :height *control-height*)))
    (lightfast:set-label-align label ':left ':inside ':clip)
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
  "Return the list line for ENTRY: name, folder, model and age in tab columns."
  (format nil "~A~:[~;  ·  live~]~C~A~C~@[~A~]~C~A"
          (conversation-entry-name entry) (conversation-entry-live-p entry) #\Tab
          (folder-tail (conversation-entry-directory entry)) #\Tab
          (conversation-entry-model entry) #\Tab
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
         (window (lightfast:make-window :width width :height height :label "Conversations" :app-id "aui")))
    (setf (slot-value welcome 'window) window)
    (lightfast:window-escape-closes window t)
    (let* ((wordmark (welcome--heading window "Conversations" :face ':prose :size *display-size*))
           (lead (welcome--heading window "Pick up where you left off, or start somewhere new." :face ':prose-italic))
           (search-input (lightfast:make-input :parent window :height (+ *control-height* 4)))
           (count-label (welcome--heading window "" :face ':prose-italic))
           (column-labels (loop for (heading) in *conversation-columns*
                                collect (welcome--heading window heading)))
           (conversations-list (lightfast:make-browser :parent window :height 300
                                                       :column-widths (mapcar #'cdr *conversation-columns*)))
           (open (lightfast:make-button :parent window :label "Open" :height *control-height*
                                        :callback (lambda (widget event value)
                                                    (declare (ignore widget event value))
                                                    (welcome--resume welcome))))
           (folders-heading (welcome--heading window "Start a new conversation"))
           (path-input (lightfast:make-input :parent window :height *control-height*
                                             :value (namestring (uiop:getcwd))))
           (browse (lightfast:make-button :parent window :label "Browse" :height *control-height*
                                          :callback (lambda (widget event value)
                                                      (declare (ignore widget event value))
                                                      (welcome--browse welcome))))
           (start (lightfast:make-button :parent window :label "Start here" :height *control-height*
                                         :callback (lambda (widget event value)
                                                     (declare (ignore widget event value))
                                                     (welcome--start welcome))))
           (folders-list (lightfast:make-browser :parent window :height 120 :column-widths '(600 0))))
      (setf (slot-value welcome 'search-input) search-input
            (slot-value welcome 'path-input) path-input
            (slot-value welcome 'conversations-list) conversations-list
            (slot-value welcome 'folders-list) folders-list
            (slot-value welcome 'count-label) count-label)
      (lightfast:set-label-align count-label ':right ':inside)
      (lightfast:set-tooltip search-input "Type to search by name, folder or model; Enter opens the first match")
      (dolist (input (list search-input path-input))
        (lightfast:set-text-font input (font ':prose))
        (lightfast:set-text-size input *prose-size*))
      (dolist (list (list conversations-list folders-list))
        (lightfast:set-text-font list (font ':prose))
        (lightfast:set-text-size list *prose-size*)
        (lightfast:set-box list lightfast:+box-no-box+)
        (lightfast:browser-set-scrollbars list ':vertical))
      (lightfast:set-when search-input ':changed ':enter-key)
      (lightfast:on search-input (lambda (widget event value)
                                   (declare (ignore widget event value))
                                   (if (member (lightfast:event-key) (list *key-enter* *key-keypad-enter*))
                                       (welcome--resume welcome)
                                       (welcome--refresh-list welcome)))
                    :event lightfast:+event-activate+)
      (lightfast:set-when path-input ':enter-key)
      (lightfast:on path-input (lambda (widget event value)
                                 (declare (ignore widget event value))
                                 (welcome--start welcome))
                    :event lightfast:+event-activate+)
      (lightfast:on folders-list (lambda (widget event value)
                                   (declare (ignore widget event value))
                                   (welcome--folder-chosen welcome))
                    :event lightfast:+event-change+)
      (lightfast:on conversations-list (lambda (widget event value)
                                         (declare (ignore widget event value))
                                         (when (plusp (lightfast:event-clicks))
                                           (welcome--resume welcome)))
                    :event lightfast:+event-change+)
      (welcome-set-entries welcome entries)
      (lightfast:layout-on-resize
       window
       (lightfast:make-layout-column
        :padding (list (spacing 6) (spacing 5) (spacing 6) (spacing 5)) :gap (spacing 3)
        :children (list (lightfast:make-layout-item wordmark :basis 52 :shrink 0)
                        (lightfast:make-layout-item lead :basis *control-height* :shrink 0)
                        (lightfast:make-layout-row
                         :basis (+ *control-height* 4) :shrink 0 :gap (spacing 4)
                         :children (list (lightfast:make-layout-item search-input :basis 0 :grow 1 :max-width 600)
                                         (lightfast:make-layout-item count-label :basis 0 :grow 1)))
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0
                         :children (loop for label in column-labels
                                         for (nil . column-width) in *conversation-columns*
                                         collect (if (zerop column-width)
                                                     (lightfast:make-layout-item label :basis 0 :grow 1)
                                                     (lightfast:make-layout-item label :basis column-width :shrink 0))))
                        (lightfast:make-layout-item conversations-list :basis 0 :grow 3)
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0 :justify :start
                         :children (list (lightfast:make-layout-item open :basis 120 :shrink 0)))
                        (lightfast:make-layout-item folders-heading :basis (+ *control-height* (spacing 5)) :shrink 0)
                        (lightfast:make-layout-row
                         :basis *control-height* :shrink 0 :gap (spacing 4) :max-width 900
                         :children (list (lightfast:make-layout-item path-input :basis 0 :grow 1)
                                         (lightfast:make-layout-item browse :basis 120 :shrink 0)
                                         (lightfast:make-layout-item start :basis 140 :shrink 0)))
                        (lightfast:make-layout-item folders-list :basis 0 :grow 2)))))
    welcome))

(-> welcome-set-entries (welcome list) null)
(defun welcome-set-entries (welcome entries)
  "Show ENTRIES, most recent first, and the folders they come from."
  (let ((now (get-universal-time)))
    (setf (welcome-entries welcome) (conversations-by-recency entries)
          (welcome-folders welcome) (recent-directories entries))
    (lightfast:clear (welcome--folders-list welcome))
    (dolist (pair (welcome-folders welcome))
      (lightfast:add-item (welcome--folders-list welcome)
                          (format nil "~A~C~A" (first pair) #\Tab (age-text (rest pair) now))))
    (welcome--refresh-list welcome))
  nil)

(-> welcome--refresh-list (welcome) null)
(defun welcome--refresh-list (welcome)
  "List the conversations matching the search field."
  (let* ((now (get-universal-time))
         (shown (filter-conversations (welcome-entries welcome) (lightfast:value (welcome--search-input welcome))))
         (list (welcome--conversations-list welcome)))
    (setf (welcome-shown welcome) shown)
    (lightfast:clear list)
    (dolist (entry shown)
      (lightfast:add-item list (conversation-entry-line entry now)))
    (when shown
      (lightfast:browser-select list 0))
    (setf (lightfast:label (welcome--count-label welcome))
          (cond
            ((null (welcome-entries welcome)) "No conversations yet")
            ((= (length shown) (length (welcome-entries welcome)))
             (format nil "~D conversation~:P" (length shown)))
            (t (format nil "~D of ~D" (length shown) (length (welcome-entries welcome)))))))
  nil)

(-> welcome-focus (welcome) null)
(defun welcome-focus (welcome)
  "Give the search field the keyboard focus."
  (lightfast:take-focus (welcome--search-input welcome))
  nil)

(-> welcome--browse (welcome) null)
(defun welcome--browse (welcome)
  "Pick a folder with the native chooser into the path field."
  (let ((directory (lightfast:choose-directory :title "Choose a folder for Autolith"
                                               :preset-path (lightfast:value (welcome--path-input welcome)))))
    (when (and directory (plusp (length directory)))
      (setf (lightfast:value (welcome--path-input welcome)) directory)))
  nil)

(-> welcome--start (welcome) null)
(defun welcome--start (welcome)
  "Open a new conversation in the folder named by the path field."
  (let ((directory (string-trim " " (lightfast:value (welcome--path-input welcome)))))
    (cond
      ((and (plusp (length directory)) (uiop:directory-exists-p directory))
       (funcall (welcome-on-open welcome) (namestring (uiop:ensure-directory-pathname directory)) nil))
      (t
       (lightfast:alert-box (format nil "~A is not a folder." directory)))))
  nil)

(-> welcome--resume (welcome) null)
(defun welcome--resume (welcome)
  "Open the selected conversation, or the first match of the search."
  (let* ((index (or (first (lightfast:browser-selected-indices (welcome--conversations-list welcome))) 0))
         (entry (nth index (welcome-shown welcome))))
    (when entry
      (funcall (welcome-on-open welcome) (conversation-entry-directory entry) (conversation-entry-id entry))))
  nil)

(-> welcome--folder-chosen (welcome) null)
(defun welcome--folder-chosen (welcome)
  "Copy the selected recent folder into the path field."
  (let* ((index (first (lightfast:browser-selected-indices (welcome--folders-list welcome))))
         (pair (and index (nth index (welcome-folders welcome)))))
    (when pair
      (setf (lightfast:value (welcome--path-input welcome)) (first pair))))
  nil)
