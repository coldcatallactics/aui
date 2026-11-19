(in-package #:aui)

;;;; -- Theme --

;;; The colors and faces of AUI. The look follows Interlisp Paper and the
;;; Autolith site. There is one paper ground and black ink. Content is set in
;;; a serif, and the interface is set in a typewriter face. Selection is shown
;;; by inversion, and dither is the only tone between paper and ink. Lines are
;;; hairlines, and corners are square. Accent inks are used for kinds of
;;; things, as in the terminal: green for what is alive or went well, red for
;;; what failed, amber for tools and warnings, blue for code, links and the
;;; step in hand, and plum for defining words in code.

(defparameter *paper* '(255 254 250)
  "The color of the ground.")

(defparameter *ink* '(0 0 0)
  "The color of the foreground.")

(defparameter *green* '(21 121 95)
  "The brand green on paper. It is used for what is working, live or accepted.")

(defparameter *green-light* '(63 182 139)
  "The brand green on ink.")

(defparameter *red* '(138 28 28)
  "The ink used for failures and for destructive choices.")

(defparameter *amber* '(140 84 0)
  "The ink used for tools and for warnings.")

(defparameter *blue* '(22 72 148)
  "The ink used for code among prose, for links and for the step in hand.")

(defparameter *plum* '(112 38 118)
  "The ink used for defining words in code.")

(defparameter *accents-on-ink*
  '(((21 121 95) . (63 182 139))
    ((138 28 28) . (246 132 120))
    ((140 84 0) . (244 196 96))
    ((22 72 148) . (150 196 252))
    ((112 38 118) . (226 168 232)))
  "A list of all accent inks associated with their lighter versions for an inverted ground.")

(defparameter *serif-families*
  '("Times New Roman MT Std" "Times New Roman" "Tinos" "Liberation Serif" "Nimbus Roman" "DejaVu Serif")
  "The serif families used for content, in order of preference.")

(defparameter *mono-families*
  '("CMU Typewriter Text" "Courier Prime" "Courier New" "Menlo" "DejaVu Sans Mono")
  "The typewriter families used for the interface and for code, in order of preference.")

(defparameter *type-scale*
  '(:display   (:serif-bold 40 44)
    :title     (:serif-bold 26 32)
    :heading-1 (:serif-bold 25 32)
    :heading-2 (:serif-bold 21 28)
    :heading-3 (:serif-bold 19 28)
    :body      (:serif 19 29)
    :body-bold (:serif-bold 19 29)
    :list      (:serif 17 24)
    :list-bold (:serif-bold 17 24)
    :small     (:serif 15 21)
    :code      (:mono 16 23)
    :label     (:mono 14 20)
    :label-bold (:mono-bold 14 20)
    :meta      (:mono 13 18)
    :meta-bold (:mono-bold 13 18))
  "A property list of all text roles associated with their face, size and line height in FLTK units.")

(defvar *fonts* nil
  "A property list of all faces associated with their loaded font numbers.")

(define-condition theme-error (error)
  ((message
    :initarg       :message
    :reader        theme-error-message
    :type          string
    :documentation "The description of the problem with the theme."))
  (:report (lambda (condition stream)
             (write-string (theme-error-message condition) stream)))
  (:documentation "The condition signalled if a face or a text role is missing from the theme."))

(-> theme--family-installed-p (string list) boolean)
(defun theme--family-installed-p (family installed)
  "Return T if the INSTALLED face names contain FAMILY or a style of FAMILY.

On some font systems, the name of the regular face is the family alone. On
others, it is the family followed by a style word."
  (let ((prefix (format nil "~A " family)))
    (and (find-if (lambda (name)
                    (or (string-equal family name)
                        (and (> (length name) (length prefix))
                             (string-equal prefix name :end2 (length prefix)))))
                  installed
                  :key #'first)
         t)))

(-> theme--family (list list) string)
(defun theme--family (candidates installed)
  "Return the first family of CANDIDATES that is in the INSTALLED face names. If none are installed, return the first candidate."
  (or (find-if (lambda (family) (theme--family-installed-p family installed)) candidates)
      (first candidates)))

(-> theme--face-name (string keyword list) string)
(defun theme--face-name (family style installed)
  "Return the name of the face of FAMILY in STYLE. A name from the INSTALLED faces is preferred."
  (let ((names (ecase style
                 (:regular
                  (list family (format nil "~A Regular" family)))
                 (:bold
                  (list (format nil "~A Bold" family)))
                 (:italic
                  (list (format nil "~A Italic" family) (format nil "~A Oblique" family)))
                 (:bold-italic
                  (list (format nil "~A Bold Italic" family) (format nil "~A BoldItalic" family)
                        (format nil "~A Bold Oblique" family))))))
    (or (find-if (lambda (name)
                   (find name installed :key #'first :test #'string-equal))
                 names)
        (first names))))

(defparameter *bundled-mono-family* "CMU Typewriter Text"
  "The family of the typewriter face that is bundled with AUI.")

(-> theme--register-bundled-fonts () boolean)
(defun theme--register-bundled-fonts ()
  "Register the fonts bundled with AUI for this process. Returns T if all fonts were registered."
  (let ((files (uiop:directory-files (asdf:system-relative-pathname :aui "assets/fonts/") "*.ttf")))
    (and files
         (handler-case (values (lightfast:register-font-files (mapcar #'uiop:native-namestring files)))
           (error ()
             nil))
         t)))

(-> theme-install () null)
(defun theme-install ()
  "Load the faces. Make all widgets created afterwards flat, with a paper ground and ink."
  (let* ((bundled-p (theme--register-bundled-fonts))
         (installed (lightfast:system-font-names))
         (serif     (theme--family *serif-families* installed))
         ;; A registered face is available by the family name, whatever the system list contains.
         (mono      (if bundled-p
                        *bundled-mono-family*
                        (theme--family *mono-families* installed))))
    ;; The Cairo driver of FLTK draws these faces with a line gap above the
    ;; baseline. The corrected copies are drawn at the same height as other faces.
    (lightfast:correct-font-baselines (list serif mono))
    (flet ((face (family style)
             (lightfast:load-font (theme--face-name family style installed))))
      (setf *fonts*
            (list ':serif             (face serif ':regular)
                  ':serif-bold        (face serif ':bold)
                  ':serif-italic      (face serif ':italic)
                  ':serif-bold-italic (face serif ':bold-italic)
                  ':mono              (face mono ':regular)
                  ':mono-bold         (face mono ':bold)
                  ':mono-italic       (face mono ':italic)
                  ':mono-bold-italic  (face mono ':bold-italic)))))
  (lightfast:apply-flat-theme :background *paper*
                              :foreground *ink*
                              :selection  *ink*
                              :label-font (theme-font ':mono)
                              :label-size 14
                              :text-font  (theme-font ':serif)
                              :text-size  19
                              :mono-font  (theme-font ':mono))
  nil)

(-> theme-font (keyword) integer)
(defun theme-font (face)
  "Return the font number loaded for FACE, such as :SERIF-BOLD or :MONO."
  (or (getf *fonts* face)
      (error 'theme-error :message (format nil "Face ~S is not loaded. Call THEME-INSTALL first." face))))


;;;; -- Text Styles --

(defstruct (text-style (:constructor %make-text-style))
  "The face, size, line height, color and marks of a run of text."
  (face        ':serif :type keyword)
  (size        19      :type integer)
  (line-height 29      :type integer)
  (color       nil     :type (or null list))
  (underline-p nil     :type boolean)
  (strike-p    nil     :type boolean))

(-> text-style (keyword &key (:face (or null keyword)) (:color (or null list)) (:underline-p boolean)
                             (:strike-p boolean))
    text-style)
(defun text-style (role &key face color underline-p strike-p)
  "Return the style of the text role ROLE from the type scale. FACE, COLOR,
UNDERLINE-P and STRIKE-P replace the values of the role.

A NIL COLOR means the foreground color of the place where the text is drawn."
  (let ((entry (getf *type-scale* role)))
    (unless entry
      (error 'theme-error :message (format nil "The type scale does not contain the text role ~S." role)))
    (destructuring-bind (role-face size line-height) entry
      (%make-text-style :face        (or face role-face)
                        :size        size
                        :line-height line-height
                        :color       color
                        :underline-p underline-p
                        :strike-p    strike-p))))

(-> face-variant (keyword &key (:bold-p boolean) (:italic-p boolean)) keyword)
(defun face-variant (face &key bold-p italic-p)
  "Return the face in the family of FACE that is bold if BOLD-P is true and italic if ITALIC-P is true."
  (let ((mono-p (member face '(:mono :mono-bold :mono-italic :mono-bold-italic))))
    (cond
      ((and bold-p italic-p)
       (if mono-p ':mono-bold-italic ':serif-bold-italic))
      (bold-p
       (if mono-p ':mono-bold ':serif-bold))
      (italic-p
       (if mono-p ':mono-italic ':serif-italic))
      (t
       (if mono-p ':mono ':serif)))))


;;;; -- Measurement --

(defparameter *text-width-limit* 20000
  "The number of widths cached for one face and size. The cache is cleared if the number is reached.")

(defvar *text-widths* (make-hash-table :test #'equal)
  "A table of the cached widths of strings, keyed by face and size.")

(defvar *face-metrics* (make-hash-table :test #'equal)
  "A table of the (HEIGHT . DESCENT) of faces, keyed by face and size.")

(-> text-measure (string text-style) integer)
(defun text-measure (text style)
  "Measure the advance width of TEXT in STYLE and return it.

The width is measured each time. Use `text-width' for words that are
measured often, because it caches the widths."
  (if (zerop (length text))
      0
      (values (lightfast:measure-text text
                                      :font (theme-font (text-style-face style))
                                      :size (text-style-size style)))))

(-> text-width (string text-style) integer)
(defun text-width (text style)
  "Return the advance width of TEXT in STYLE in FLTK units. The width is cached."
  (if (zerop (length text))
      0
      (let* ((key   (cons (text-style-face style) (text-style-size style)))
             (table (or (gethash key *text-widths*)
                        (setf (gethash key *text-widths*) (make-hash-table :test #'equal)))))
        (or (gethash text table)
            (progn
              (when (>= (hash-table-count table) *text-width-limit*)
                (clrhash table))
              (setf (gethash text table) (text-measure text style)))))))

(-> text-metrics (text-style) (values integer integer))
(defun text-metrics (style)
  "Return the ascent and the descent of the face of STYLE at the size of STYLE, as two values."
  (let* ((key     (cons (text-style-face style) (text-style-size style)))
         (metrics (or (gethash key *face-metrics*)
                      (setf (gethash key *face-metrics*)
                            (multiple-value-bind (width height descent)
                                (lightfast:measure-text "x"
                                                        :font (theme-font (text-style-face style))
                                                        :size (text-style-size style))
                              (declare (ignore width))
                              (cons height descent))))))
    (values (- (first metrics) (rest metrics)) (rest metrics))))

(-> text-baseline (text-style) integer)
(defun text-baseline (style)
  "Return the distance from the top of the line to the baseline of STYLE. The face is centered in the line height."
  (multiple-value-bind (ascent descent) (text-metrics style)
    (+ (floor (- (text-style-line-height style) (+ ascent descent)) 2) ascent)))
