(in-package #:aui)

;;;; -- Command Palette --

;;; A pane inside the main window: one field and one list. Type, see the
;;; operations, conversations, settings, models and actions that match, press
;;; Enter to act on the chosen match. Escape returns to the transcript.

(defstruct (palette-item (:constructor make-palette-item (kind label detail object)))
  "One thing the palette can find."
  (kind ':operation :type (member :action :operation :conversation :setting :model :mode))
  (label "" :type string)
  (detail "" :type string)
  (object nil))

(defclass palette ()
  ((workbench :initarg :workbench :reader palette-workbench :type workbench)
   (group     :reader palette-group
              :documentation "The Lightfast group holding the pane.")
   (input     :reader palette--input)
   (list      :reader palette--list)
   (hint      :reader palette--hint)
   (matches   :initform nil :accessor palette-matches
              :documentation "The PALETTE-ITEMs shown, in list order."))
  (:documentation "The command palette pane of one workbench."))

(-> fuzzy-score (string string) (or null integer))
(defun fuzzy-score (query text)
  "Return how well QUERY matches TEXT as an ordered subsequence, higher is
better, or NIL when it does not match. Case is ignored; adjacent matches
and matches at word starts score more."
  (let ((query (string-downcase query))
        (text (string-downcase text))
        (score 0)
        (position 0)
        (previous -2))
    (loop for character across query
          do (let ((found (position character text :start position)))
               (unless found
                 (return-from fuzzy-score nil))
               (incf score (cond
                             ((= found (1+ previous)) 3)
                             ((or (zerop found) (not (alphanumericp (char text (1- found))))) 2)
                             (t 1)))
               (setf previous found
                     position (1+ found))))
    (- score (floor (length text) 16))))

(-> workbench-actions (workbench) list)
(defun workbench-actions (workbench)
  "Return the application actions the palette offers for WORKBENCH as
(LABEL DETAIL FUNCTION) lists, only those that apply right now."
  (append
   (list (list "Conversations" "every conversation across folders, Ctrl+O"
               (lambda () (workbench-open-conversations workbench)))
         (list "New conversation here" "start a fresh conversation in this folder, Ctrl+N"
               (lambda () (workbench-new-conversation workbench)))
         (list "Agenda" "what Autolith has on its agenda for this workspace"
               (lambda () (workbench-show-agenda workbench)))
         (list "Jump to latest" "follow the end of the transcript again"
               (lambda () (transcript-view-jump-to-end (workbench-transcript-view workbench))))
         (list (if (workbench-inspector-visible-p workbench) "Hide inspector" "Show inspector")
               "the pane for tool calls and jobs, Ctrl+I"
               (lambda () (workbench-toggle-inspector workbench)))
         (list "Settings" "every Autolith setting, Ctrl+,"
               (lambda () (workbench-show-settings workbench)))
         (list "Open another folder" "choose a folder and start Autolith there in a new window"
               (lambda () (workbench-open-another-folder workbench))))
   (when (member (workbench-connection-state workbench) '(:lost :exited))
     (list (list "Restart Autolith" "start the process again and reopen this conversation"
                 (lambda () (workbench-restart workbench)))))))

(-> palette-items (workbench) list)
(defun palette-items (workbench)
  "Return everything the palette can offer for WORKBENCH."
  (append (mapcar (lambda (action)
                    (destructuring-bind (label detail function) action
                      (make-palette-item ':action label detail function)))
                  (workbench-actions workbench))
          (mapcar (lambda (mode)
                    (destructuring-bind (id label description) mode
                      (make-palette-item ':mode (format nil "Permissions: ~A" label)
                                         (format nil "permission mode ~A, ~A" id description) id)))
                  *permission-modes*)
          (mapcar (lambda (operation)
                    (make-palette-item ':operation (operation-entry-name operation)
                                       (format nil "~A~A" (operation-entry-description operation)
                                               (cond
                                                 ((operation-entry-terminal-owning-p operation) "  (needs a terminal)")
                                                 ((not (operation-entry-admitted-p operation)) "  (unavailable)")
                                                 (t "")))
                                       operation))
                  (workbench-operations workbench))
          (mapcar (lambda (entry)
                    (make-palette-item ':conversation
                                       (conversation-entry-name entry)
                                       (format nil "conversation in ~A" (conversation-entry-directory entry))
                                       entry))
                  (workbench-conversations workbench))
          (mapcar (lambda (entry)
                    (make-palette-item ':setting (setting-entry-label entry)
                                       (format nil "setting, now ~A" (setting-entry-value-text entry))
                                       entry))
                  (workbench-settings workbench))
          (mapcar (lambda (model)
                    (make-palette-item ':model (model-entry-id model)
                                       (format nil "model~@[ via ~A~]~A" (model-entry-provider model)
                                               (cond
                                                 ((model-entry-verified-p model) ", verified")
                                                 ((model-entry-credential-p model) ", credential stored")
                                                 (t ", no credential")))
                                       model))
                  (workbench-models workbench))))

(-> palette-search (list string &key (:limit integer)) list)
(defun palette-search (items query &key (limit 40))
  "Return the ITEMS matching QUERY, best first, at most LIMIT of them.
An empty QUERY returns the first LIMIT items unsorted."
  (if (zerop (length (string-trim " " query)))
      (subseq items 0 (min limit (length items)))
      (let ((scored (loop for item in items
                          for score = (fuzzy-score query (palette-item-label item))
                          when score
                            collect (cons score item))))
        (mapcar #'rest (subseq (stable-sort scored #'> :key #'first)
                               0 (min limit (length scored)))))))

(-> make-palette (workbench t) palette)
(defun make-palette (workbench parent)
  "Create the palette pane under PARENT for WORKBENCH, hidden."
  (let* ((palette (make-instance 'palette :workbench workbench))
         (group (lightfast:make-group :parent parent :width 600 :height 400))
         (input (lightfast:make-input :parent group :height (+ *control-height* 6)))
         (list (lightfast:make-browser :parent group :height 300 :column-widths '(340 0)))
         (hint (workbench--make-label group "Type to search; Enter runs the choice, Escape returns." ':prose-italic)))
    (setf (slot-value palette 'group) group
          (slot-value palette 'input) input
          (slot-value palette 'list) list
          (slot-value palette 'hint) hint)
    (lightfast:set-box group lightfast:+box-no-box+)
    (lightfast:set-text-font input (font ':prose))
    (lightfast:set-text-size input *heading-size*)
    (lightfast:set-text-font list (font ':prose))
    (lightfast:set-text-size list *prose-size*)
    (lightfast:set-box list lightfast:+box-no-box+)
    (lightfast:browser-set-scrollbars list ':vertical)
    (lightfast:browser-set-line-spacing list *list-line-spacing*)
    (lightfast:set-label-size hint *small-size*)
    (lightfast:set-when input ':changed ':enter-key)
    (lightfast:on input (lambda (widget event value)
                          (declare (ignore widget event value))
                          (if (member (lightfast:event-key) (list *key-enter* *key-keypad-enter*))
                              (palette-run palette)
                              (palette-refresh palette)))
                  :event lightfast:+event-activate+)
    (lightfast:on list (lambda (widget event value)
                         (declare (ignore widget event value))
                         (when (plusp (lightfast:event-clicks))
                           (palette-run palette)))
                  :event lightfast:+event-change+)
    (lightfast:hide group)
    palette))

(-> palette-layout (palette) t)
(defun palette-layout (palette)
  "Return the layout node placing the palette's widgets inside its group."
  (lightfast:make-layout-column
   :target (palette-group palette) :basis 0 :grow 1 :gap (spacing 3)
   :children (list (lightfast:make-layout-item (palette--input palette) :basis (+ *control-height* 6) :shrink 0)
                   (lightfast:make-layout-item (palette--hint palette) :basis *control-height* :shrink 0)
                   (lightfast:make-layout-item (palette--list palette) :basis 0 :grow 1))))

(-> palette-open (palette) null)
(defun palette-open (palette)
  "Clear the query, refresh the matches and focus the field."
  (setf (lightfast:value (palette--input palette)) "")
  (palette-refresh palette)
  (lightfast:take-focus (palette--input palette))
  nil)

(-> palette-refresh (palette) null)
(defun palette-refresh (palette)
  "Fill the list with the matches for the current query."
  (let ((matches (palette-search (palette-items (palette-workbench palette))
                                 (lightfast:value (palette--input palette))))
        (list (palette--list palette)))
    (setf (palette-matches palette) matches)
    (lightfast:clear list)
    (dolist (item matches)
      (lightfast:add-item list (format nil "~A~C~A" (palette-item-label item) #\Tab (palette-item-detail item))))
    (when matches
      (lightfast:browser-select list 0)))
  nil)

(-> palette-run (palette) null)
(defun palette-run (palette)
  "Act on the selected match, else the first, and return to the transcript."
  (let* ((index (or (first (lightfast:browser-selected-indices (palette--list palette))) 0))
         (item (nth index (palette-matches palette)))
         (workbench (palette-workbench palette)))
    (when item
      (workbench-show-transcript workbench)
      (ecase (palette-item-kind item)
        (:action
         (funcall (palette-item-object item)))
        (:operation
         (workbench-invoke workbench (palette-item-object item)))
        (:conversation
         (backend-open-conversation (workbench-backend workbench)
                                    :conversation-id (conversation-entry-id (palette-item-object item))))
        (:setting
         (workbench-show-settings workbench (palette-item-object item)))
        (:model
         (backend-set-setting (workbench-backend workbench) ':model
                              (model-entry-id (palette-item-object item))))
        (:mode
         (backend-set-mode (workbench-backend workbench) (palette-item-object item))))))
  nil)
