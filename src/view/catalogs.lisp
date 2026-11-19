(in-package #:aui)

;;;; -- Catalogs --

;;; The two lists a channel sends about itself, each in an overlay: its
;;; commands, found by typing, and its settings, each with its current value
;;; to change in place. They stand in for the terminal's pickers and its
;;; settings page.

;;;; -- Commands --

(-> catalog--command-rows (workbench string) list)
(defun catalog--command-rows (workbench query)
  "Return the operations to list for QUERY: commands first, then tools, those not usable from a window left out."
  (let ((rows (remove-if (lambda (row)
                           (member (portable-get row "name") *terminal-only-commands* :test #'equal))
                         (workbench-operations workbench))))
    (stable-sort
     (remove-if-not (lambda (row)
                      (or (zerop (length query))
                          (search query (or (portable-get row "name") "") :test #'char-equal)
                          (search query (or (portable-get row "description") "") :test #'char-equal)))
                    rows)
     (lambda (first second)
       (and (equal (portable-get first "kind") "command")
            (not (equal (portable-get second "kind") "command")))))))

(-> catalog--run (workbench list) null)
(defun catalog--run (workbench row)
  "Do what choosing the operation ROW means: open the window's own view, ask for its argument, or run it."
  (let ((name    (portable-get row "name"))
        (choices (mapcar #'princ-to-string (portable-items (portable-get row "choices")))))
    (workbench-show-overlay workbench nil)
    (cond
      ((and (stringp name) (workbench-run-interface-command workbench name))
       nil)
      ((not (equal (portable-get row "kind") "command"))
       (setf (composer-text workbench) (format nil "(~A )" name))
       (window-focus-composer workbench))
      (choices
       ;; Autolith decodes the choice, because the same spelling can stand for a keyword or a string.
       (workbench-pick workbench
                       (cons (make-choice :label "no argument" :value "")
                             (mapcar (lambda (choice)
                                       (make-choice :label choice :value choice))
                                     choices))
                       (lambda (choice)
                         (if (zerop (length choice))
                             (workbench-submit workbench (format nil "(~A)" name))
                             (workbench-invoke workbench name choice)))
                       :title (format nil "(~A)" name)))
      ((portable-string (portable-get row "argumentHint"))
       (setf (composer-text workbench) (format nil "(~A " name))
       (window-focus-composer workbench))
      (t
       (workbench-submit workbench (format nil "(~A)" name)))))
  nil)

(-> catalog-run-selected (workbench) null)
(defun catalog-run-selected (workbench)
  "Run the command highlighted in the list of commands."
  (let* ((overlay (workbench-overlay workbench))
         (row     (nth (overlay-selection overlay) (catalog--command-rows workbench (overlay-query overlay)))))
    (when row
      (catalog--run workbench row)))
  nil)

(-> catalog--scroll (overlay integer integer) function)
(defun catalog--scroll (overlay count visible)
  "Return the wheel handler of a list of COUNT rows of which VISIBLE fit: it scrolls OVERLAY and keeps the highlight among the rows shown."
  (lambda (vertical horizontal)
    (declare (ignore horizontal))
    (let ((scroll (max 0 (min (max 0 (- count visible)) (+ (overlay-scroll overlay) vertical)))))
      (setf (overlay-scroll overlay)    scroll
            (overlay-selection overlay) (max scroll
                                             (min (overlay-selection overlay) (+ scroll visible -1)))))))

(-> catalog-paint-commands
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun catalog-paint-commands (workbench stage overlay &key x y width height)
  "Draw the list of operations matching OVERLAY's query."
  (let* ((rows    (catalog--command-rows workbench (overlay-query overlay)))
         (visible (max 1 (floor height *overlay-row-height*)))
         (count   (length rows)))
    (setf (overlay-selection overlay) (max 0 (min (overlay-selection overlay) (1- count))))
    (setf (overlay-scroll overlay)
          (max 0 (min (overlay-scroll overlay) (max 0 (- count visible)))))
    ;; The keys move the highlight; the list follows it.
    (when (< (overlay-selection overlay) (overlay-scroll overlay))
      (setf (overlay-scroll overlay) (overlay-selection overlay)))
    (when (>= (overlay-selection overlay) (+ (overlay-scroll overlay) visible))
      (setf (overlay-scroll overlay) (- (overlay-selection overlay) visible -1)))
    (stage-region stage ':overlay-list
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (catalog--scroll overlay count visible))
    (when (zerop count)
      (paint-text-in (if (workbench-operations workbench)
                         "Nothing matches."
                         "No commands yet.") (text-style ':small :face ':serif-italic)
                     :x      (+ x 16)
                     :y      y
                     :width  (- width 32)
                     :height 44))
    (with-clip (x y width height)
      (loop for row in (nthcdr (overlay-scroll overlay) rows)
            for index from (overlay-scroll overlay)
            for row-y from y by *overlay-row-height*
            while (< row-y (+ y height))
            do (let* ((chosen row)
                      (region (list ':operation (portable-get row "name")))
                      (lit-p  (or (= index (overlay-selection overlay)) (stage-hot-p stage region))))
                 (flet ((content ()
                          (let* ((bold (text-style ':label-bold))
                                 (hint (portable-string (portable-get row "argumentHint")))
                                 (end  (paint-text (portable-get row "name") (+ x 16) (+ row-y 19) :style bold)))
                            (when hint
                              (paint-text (text-fit hint (text-style ':label) (- (+ x width) end 30)) (+ end 10) (+ row-y 19)
                                          :style (text-style ':label)))
                            (unless (equal (portable-get row "kind") "command")
                              (paint-text-in "tool" (text-style ':meta :color *amber*)
                                             :x      (- (+ x width) 64)
                                             :y      row-y
                                             :width  44
                                             :height 24
                                             :align  ':right))
                            (paint-text (text-fit (entries--one-line (or (portable-get row "description") ""))
                                                  (text-style ':small) (- width 44)) (+ x 16) (+ row-y 38)
                                        :style (text-style ':small)))))
                   (if lit-p
                       (with-inversion (x row-y width *overlay-row-height*)
                         (content))
                       (content)))
                 (stage-region stage region
                               :x      x
                               :y      row-y
                               :width  width
                               :height *overlay-row-height*
                               :action (lambda () (catalog--run workbench chosen))))))
    (sheet-paint-scrollbar stage
                           :x       (+ x width -9)
                           :y       y
                           :height  height
                           :content (* count *overlay-row-height*)
                           :scroll  (* (overlay-scroll overlay) *overlay-row-height*)))
  nil)


;;;; -- Settings --

(-> catalog--setting-value-text (list) string)
(defun catalog--setting-value-text (row)
  "Return the setting ROW's value as it reads on a chip."
  (let ((value (portable-get row "value" ':null)))
    (cond
      ((equal (portable-get row "type") "boolean")
       (if (portable-true-p value) "on" "off"))
      ((eq value ':null)
       "none")
      ((stringp value)
       (if (zerop (length value)) "empty" value))
      (t
       (princ-to-string value)))))

(-> catalog--change-setting (workbench list) null)
(defun catalog--change-setting (workbench row)
  "Let the person change the setting ROW in the way its type calls for: a switch turns, a choice opens a picker, anything else a prompt."
  (let ((name  (portable-get row "name"))
        (type  (portable-get row "type"))
        (label (or (portable-get row "label") (portable-get row "name") "")))
    (cond
      ((equal type "boolean")
       (workbench-set-setting workbench name
                              (if (portable-true-p (portable-get row "value")) ':false t)))
      ((equal type "choice")
       (let ((current (catalog--setting-value-text row)))
         (workbench-pick workbench
                         (mapcar (lambda (option)
                                   (let ((text (if (portable-true-p option) (princ-to-string option) "none")))
                                     (make-choice :label text :value text :current-p (equal text current))))
                                 (portable-items (portable-get row "options")))
                         (lambda (value)
                           (workbench-set-setting workbench name value))
                         :title label)))
      (t
       (workbench-ask workbench label
                      (lambda (answer)
                        (workbench-set-setting workbench name
                                               (if (equal type "integer")
                                                   (or (parse-integer answer :junk-allowed t) answer)
                                                   answer)))
                      :note    (portable-string (portable-get row "documentation"))
                      :initial (let ((value (portable-get row "value")))
                                 (if value (princ-to-string value) ""))))))
  nil)

(-> catalog--setting-rows (workbench string) list)
(defun catalog--setting-rows (workbench query)
  "Return the settings to list for QUERY, each group's rows together under its name: (:GROUP NAME) or (:SETTING ROW)."
  (let ((rows   (remove-if-not (lambda (row)
                                 (or (zerop (length query))
                                     (search query (or (portable-get row "label") "") :test #'char-equal)
                                     (search query (or (portable-get row "name") "") :test #'char-equal)))
                               (workbench-settings workbench)))
        (result nil)
        (groups nil))
    (dolist (row rows)
      (pushnew (or (portable-get row "group") "other") groups :test #'equal))
    (dolist (group (nreverse groups))
      (push (list ':group group) result)
      (dolist (row rows)
        (when (equal (or (portable-get row "group") "other") group)
          (push (list ':setting row) result))))
    (nreverse result)))

(-> catalog-paint-settings
    (workbench stage overlay &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun catalog-paint-settings (workbench stage overlay &key x y width height)
  "Draw the settings OVERLAY lists as a property sheet: label and explanation at the left, the value as a chip at the right."
  (let* ((rows    (catalog--setting-rows workbench (overlay-query overlay)))
         (count   (length rows))
         (visible (max 1 (floor height *overlay-row-height*))))
    (setf (overlay-scroll overlay)
          (max 0 (min (overlay-scroll overlay) (max 0 (- count visible)))))
    (stage-region stage ':overlay-list
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (overlay-scroll overlay)
                                  (max 0 (min (max 0 (- count visible))
                                              (+ (overlay-scroll overlay) vertical))))))
    (when (zerop count)
      (paint-text-in (if (workbench-settings workbench)
                         "Nothing matches."
                         "No settings yet.") (text-style ':small :face ':serif-italic)
                     :x      (+ x 16)
                     :y      y
                     :width  (- width 32)
                     :height 44))
    (with-clip (x y width height)
      (loop for (kind value) in (nthcdr (overlay-scroll overlay) rows)
            for row-y from y by *overlay-row-height*
            while (< row-y (+ y height))
            do (ecase kind
                 ;; A group is named in the interface's bold; the space above it does the parting.
                 (:group
                  (paint-text (string-capitalize value) (+ x 16) (+ row-y 34) :style (text-style ':label-bold)))
                 (:setting
                  (let* ((row          value)
                         (adjustable-p (portable-true-p (portable-get row "adjustable")))
                         (text         (catalog--setting-value-text row))
                         (meta         (text-style ':meta))
                         (shown        (text-fit text meta 220))
                         (chip-w       (+ 16 (text-width shown meta)
                                          (if (and adjustable-p (not (equal (portable-get row "type") "boolean")))
                                              14
                                              0)))
                         (chip-x       (- (+ x width) chip-w 22)))
                    (paint-text (text-fit (or (portable-get row "label") (portable-get row "name") "")
                                          (text-style ':small :face ':serif-bold) (- chip-x x 28)) (+ x 16) (+ row-y 19)
                                :style (text-style ':small :face ':serif-bold))
                    (paint-text (text-fit (entries--one-line (or (portable-get row "documentation")
                                                                 (portable-get row "reason")
                                                                 ""))
                                          (text-style ':meta) (- chip-x x 28)) (+ x 16) (+ row-y 37)
                                :style (text-style ':meta))
                    (if adjustable-p
                        (progn
                          (paint-frame chip-x (+ row-y 10) :width chip-w :height 24)
                          (paint-chip stage (list ':setting (portable-get row "name")) shown
                                      :x       chip-x
                                      :y       (+ row-y 10)
                                      :height  24
                                      :menu-p  (not (equal (portable-get row "type") "boolean"))
                                      :action  (lambda () (catalog--change-setting workbench row))
                                      :tooltip (format nil "~A, ~A" (portable-get row "source")
                                                       (portable-get row "scope"))))
                        (progn
                          (paint-frame chip-x (+ row-y 10) :width chip-w :height 24 :dashed-p t)
                          (paint-text-in shown meta
                                         :x      (+ chip-x 8)
                                         :y      (+ row-y 10)
                                         :width  (- chip-w 16)
                                         :height 24))))))))
    (sheet-paint-scrollbar stage
                           :x       (+ x width -9)
                           :y       y
                           :height  height
                           :content (* count *overlay-row-height*)
                           :scroll  (* (overlay-scroll overlay) *overlay-row-height*)))
  nil)
