(in-package #:aui)

;;;; -- Command Palette --

;;; One field and one list: type, see operations, conversations and settings
;;; that match, press Enter to act on the first or chosen match.

(defstruct (palette-item (:constructor make-palette-item (kind label detail object)))
  "One thing the palette can find."
  (kind ':operation :type (member :operation :conversation :setting :model :mode))
  (label "" :type string)
  (detail "" :type string)
  (object nil))

(defclass palette ()
  ((window    :reader palette-window)
   (workbench :initarg :workbench :reader palette-workbench :type workbench)
   (input     :reader palette--input)
   (list      :reader palette--list)
   (matches   :initform nil :accessor palette-matches
              :documentation "The PALETTE-ITEMs shown, in list order."))
  (:documentation "The command palette of one workbench."))

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

(defparameter *permission-modes*
  '(("ask" "Ask before commands" "approve commands and external tools here")
    ("auto" "Auto" "classify commands with the configured policy")
    ("sandboxed" "Sandboxed" "run commands in the workspace sandbox")
    ("full-access" "Full access" "allow commands and external tools without approval"))
  "The ACP permission modes Autolith offers: id, label and description.")

(-> palette-items (workbench) list)
(defun palette-items (workbench)
  "Return everything the palette can offer for WORKBENCH."
  (append (mapcar (lambda (mode)
                    (destructuring-bind (id label description) mode
                      (make-palette-item ':mode (format nil "Permissions: ~A" label)
                                         (format nil "permission mode ~A, ~A" id description) id)))
                  *permission-modes*)
          (mapcar (lambda (operation)
                    (make-palette-item ':operation (operation-entry-name operation)
                                       (format nil "~A~A" (operation-entry-description operation)
                                               (cond
                                                 ((operation-entry-terminal-owning-p operation) "  (needs a terminal)")
                                                 ((not (operation-entry-admitted-p operation)) "  (not now)")
                                                 (t "")))
                                       operation))
                  (workbench-operations workbench))
          (mapcar (lambda (entry)
                    (make-palette-item ':conversation
                                       (or (conversation-entry-title entry) (conversation-entry-id entry))
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

(-> make-palette (workbench &key (:width integer) (:height integer)) palette)
(defun make-palette (workbench &key (width 720) (height 480))
  "Create the palette window for WORKBENCH and return it, unshown."
  (let* ((palette (make-instance 'palette :workbench workbench))
         (window (lightfast:make-window :width width :height height :label "autolith - palette" :app-id "aui"))
         (input (lightfast:make-input :parent window :height (+ *control-height* 8)))
         (list (lightfast:make-browser :parent window :height 300 :column-widths '(260 0))))
    (setf (slot-value palette 'window) window
          (slot-value palette 'input) input
          (slot-value palette 'list) list)
    (lightfast:set-text-font input (font ':prose))
    (lightfast:set-text-size input (+ *prose-size* 2))
    (lightfast:set-text-font list (font ':prose))
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
    (lightfast:layout-on-resize
     window
     (lightfast:make-layout-column
      :padding (spacing 4) :gap (spacing 3)
      :children (list (lightfast:make-layout-item input :basis (+ *control-height* 8) :shrink 0)
                      (lightfast:make-layout-item list :basis 0 :grow 1))))
    (palette-refresh palette)
    palette))

(-> palette-open (palette) null)
(defun palette-open (palette)
  "Show the palette with an empty query and focus its field."
  (setf (lightfast:value (palette--input palette)) "")
  (palette-refresh palette)
  (lightfast:show (palette-window palette))
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
  "Act on the selected match, else the first, and hide the palette."
  (let* ((index (or (first (lightfast:browser-selected-indices (palette--list palette))) 0))
         (item (nth index (palette-matches palette)))
         (workbench (palette-workbench palette)))
    (when item
      (lightfast:hide (palette-window palette))
      (ecase (palette-item-kind item)
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
