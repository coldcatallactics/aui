(in-package #:aui)

;;;; -- Theme --

;;; The visual vocabulary: one paper ground, black ink, a serif for what is
;;; said and a typewriter face for what the interface says about it. After
;;; Interlisp Paper and the Autolith site: inversion instead of grey, dither
;;; as the only in-between tone, hairlines, no rounded corners. A few accent
;;; inks say what a thing is, as the terminal's colors do: green for what is
;;; alive or went well, red for what failed, amber for tools and warnings,
;;; blue for code, links and the step in hand, plum for the words that define.

(defparameter *paper* '(255 254 250)
  "The one ground color.")

(defparameter *ink* '(0 0 0)
  "The one foreground color.")

(defparameter *green* '(21 121 95)
  "The brand green as it reads on paper: what is working, live or accepted.")

(defparameter *green-light* '(63 182 139)
  "The brand green as it reads on ink.")

(defparameter *red* '(138 28 28)
  "The mark of failure and of the destructive choice.")

(defparameter *amber* '(140 84 0)
  "The ink of tools and of what asks for attention without having failed.")

(defparameter *blue* '(22 72 148)
  "The ink of code among prose, of links and of the step in hand.")

(defparameter *plum* '(112 38 118)
  "The ink of the words that define, in code.")

(defparameter *accents-on-ink*
  '(((21 121 95) . (63 182 139))
    ((138 28 28) . (246 132 120))
    ((140 84 0) . (244 196 96))
    ((22 72 148) . (150 196 252))
    ((112 38 118) . (226 168 232)))
  "Each accent ink as it reads on an inverted ground.")

(defparameter *serif-families*
  '("Times New Roman MT Std" "Times New Roman" "Tinos" "Liberation Serif" "Nimbus Roman" "DejaVu Serif")
  "The content voice, in order of preference.")

(defparameter *mono-families*
  '("CMU Typewriter Text" "Courier Prime" "Courier New" "Menlo" "DejaVu Sans Mono")
  "The interface voice and the face of code, in order of preference.")

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
  "Each named text role as its face, its size and the height of its line, in FLTK units.")

(defvar *fonts* nil
  "The loaded font numbers as a property list keyed by face, set by THEME-INSTALL.")

(define-condition theme-error (error)
  ((message
    :initarg       :message
    :reader        theme-error-message
    :type          string
    :documentation "What about the theme could not be used."))
  (:report (lambda (condition stream)
             (write-string (theme-error-message condition) stream)))
  (:documentation "The theme was asked for something it does not have."))

(-> theme--family-installed-p (string list) boolean)
(defun theme--family-installed-p (family installed)
  "Return T when the INSTALLED face names include FAMILY itself or one of its styles.

Font systems disagree on what the regular face is called: the family alone,
or the family followed by a style word."
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
  "Return the first of CANDIDATES among the INSTALLED face names, or the first candidate when none is."
  (or (find-if (lambda (family) (theme--family-installed-p family installed)) candidates)
      (first candidates)))

(-> theme--face-name (string keyword list) string)
(defun theme--face-name (family style installed)
  "Return the name the font system gives STYLE of FAMILY, among the INSTALLED faces when it is listed."
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
  "The family of the typewriter face AUI ships with.")

(-> theme--register-bundled-fonts () boolean)
(defun theme--register-bundled-fonts ()
  "Make the typefaces AUI ships with available to this process, and return whether the font system took them all.

The typewriter face is then the same on every machine. A face that cannot
be registered is left to the system's own copy or to the next family in the
list."
  (let ((files (uiop:directory-files (asdf:system-relative-pathname :aui "assets/fonts/") "*.ttf")))
    (and files
         (handler-case (values (lightfast:register-font-files (mapcar #'uiop:native-namestring files)))
           (error ()
             nil))
         t)))

(-> theme-install () null)
(defun theme-install ()
  "Load the faces and make every widget created afterwards flat, on paper, in ink.

Call once on the interface thread before any window exists."
  (let* ((bundled-p (theme--register-bundled-fonts))
         (installed (lightfast:system-font-names))
         (serif     (theme--family *serif-families* installed))
         ;; A face just registered is there whatever the system's list calls it.
         (mono      (if bundled-p
                        *bundled-mono-family*
                        (theme--family *mono-families* installed))))
    ;; FLTK's Cairo driver draws a face with a line gap above its baseline;
    ;; the corrected copies sit where every other face does.
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
      (error 'theme-error :message (format nil "Face ~S is not loaded; call THEME-INSTALL first." face))))


;;;; -- Text Styles --

(defstruct (text-style (:constructor %make-text-style))
  "How a run of text is set, as its face and size, the height of its line, and its marks."
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
  "Return the style of the text ROLE from the type scale, with FACE, COLOR and marks overriding its own.

A NIL color means the ink of wherever the text is drawn, so the same style
reads on paper and inverts on ink."
  (let ((entry (getf *type-scale* role)))
    (unless entry
      (error 'theme-error :message (format nil "No text role ~S in the type scale." role)))
    (destructuring-bind (role-face size line-height) entry
      (%make-text-style :face        (or face role-face)
                        :size        size
                        :line-height line-height
                        :color       color
                        :underline-p underline-p
                        :strike-p    strike-p))))

(-> face-variant (keyword &key (:bold-p boolean) (:italic-p boolean)) keyword)
(defun face-variant (face &key bold-p italic-p)
  "Return the face of FACE's family that is bold when BOLD-P and italic when ITALIC-P."
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

(defvar *text-widths* (make-hash-table :test #'equal)
  "The measured width of each string by face and size, so wrapping a paragraph measures each word once.")

(defvar *face-metrics* (make-hash-table :test #'equal)
  "The (HEIGHT . DESCENT) of each face and size.")

(-> text-width (string text-style) integer)
(defun text-width (text style)
  "Return the advance width of TEXT set in STYLE, in FLTK units."
  (if (zerop (length text))
      0
      (let* ((face  (text-style-face style))
             (size  (text-style-size style))
             (table (or (gethash (cons face size) *text-widths*)
                        (setf (gethash (cons face size) *text-widths*)
                              (make-hash-table :test #'equal)))))
        (or (gethash text table)
            (setf (gethash text table)
                  (values (lightfast:measure-text text :font (theme-font face) :size size)))))))

(-> text-metrics (text-style) (values integer integer))
(defun text-metrics (style)
  "Return the ascent and descent of STYLE's face at its size, as two values."
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
  "Return how far below the top of its line STYLE's baseline sits, the face centered in the line's height."
  (multiple-value-bind (ascent descent) (text-metrics style)
    (+ (floor (- (text-style-line-height style) (+ ascent descent)) 2) ascent)))
