(in-package #:aui)

;;;; -- Theme --

;;; One substrate, black ink, serif prose, typewriter code, and accent colors
;;; derived in OKLCH from the Lambda Symbolics green. Everything visual in AUI
;;; takes its values from here.

(defparameter *paper* '(255 254 250)
  "The single substrate, #FFFEFA, for windows and fields alike.")

(defparameter *ink* '(0 0 0)
  "Pure black for text and every edge.")

(defparameter *logo-green-hue* 165.9d0
  "The OKLCH hue of the Lambda Symbolics green #42B28A.")

(defparameter *accent-hues*
  '((:green  . 165.9d0)
    (:blue   . 250.0d0)
    (:red    . 25.0d0)
    (:orange . 60.0d0)
    (:yellow . 90.0d0)
    (:violet . 310.0d0))
  "Accent roles and their OKLCH hues. Green is the logo; the others are
spaced around it so each role reads as distinct at the same lightness.")

(defparameter *accent-tiers*
  '((:ink  . (0.50d0 . 0.118d0))
    (:mark . (0.689d0 . 0.118d0)))
  "Lightness and chroma of each accent tier. The ink tier is dark enough to
read against paper as a stroke or a glyph; the mark tier is the logo's own
lightness and is used only for small fills carrying black labels.")

(defparameter *prose-families*
  '("Times New Roman MT Std" "Times New Roman" "Liberation Serif" "Nimbus Roman")
  "Serif families for prose and labels, first installed one wins.")

(defparameter *code-families*
  '("CMU Typewriter Text" "IBM Plex Mono" "Berkeley Mono" "Liberation Mono")
  "Monospace families for code, first installed one wins.")

(defparameter *marker-families*
  '("Adwaita Mono" "Iosevka Term" "Unifont")
  "Families for marker glyphs the serif lacks: boxes, triangles, circles and
check marks. Chosen by measurement: at *MARKER-SIZE* beside the prose, once
the prose face has been through LIGHTFAST:CORRECT-FONT-BASELINES, their
shapes sit on the prose baseline and stand about one x-height tall.")

(defparameter *marker-size* 24
  "Pixel size of marker glyphs beside prose. Measured beside 20 px prose:
Adwaita Mono's boxes, triangles and circles then stand 12 px, from the cap
height to two pixels above the baseline, and their line height stays under
the first-level heading's, so the transcript's line pitch does not grow.")

(defparameter *prose-size* 20
  "Pixel size of prose, labels and controls.")

(defparameter *code-size* 18
  "Pixel size of code, nine tenths of the prose as Interlisp Paper asks; CMU
Typewriter's small x-height wants the size kept close. The measured residue
beside 20 px prose is one pixel above the baseline, the rounding floor of
FLTK's integer metrics.")

(defparameter *small-size* 18
  "Pixel size of secondary lines: setting documentation, hints and sources.")

(defparameter *meta-size* 20
  "Pixel size of metadata: times, status words, sources. The same as prose so
that a metadata run shares the baseline of the words beside it; italics and
placement carry the distinction.")

(defparameter *heading-size* 25
  "Pixel size of first-level headings inside messages and of pane titles.")

(defparameter *subheading-size* 22
  "Pixel size of second-level headings.")

(defparameter *small-heading-size* 20
  "Pixel size of third-level and deeper headings, bold at prose size.")

(defparameter *display-size* 32
  "Pixel size of the few display-scale words: the wordmark, a conversation title.")

(defparameter *spacing-scale* #(4 8 12 16 24 32)
  "The spacing scale in pixels; SPACING indexes it from 1.")

(defvar *fonts* nil
  "Loaded font numbers by face, filled by THEME-INSTALL on the display thread.")


;;; Color arithmetic

(-> srgb-channel->linear (real) double-float)
(defun srgb-channel->linear (value)
  "Return the linear light of one sRGB channel VALUE in 0 to 255."
  (let ((unit (/ value 255d0)))
    (if (<= unit 0.04045d0)
        (/ unit 12.92d0)
        (expt (/ (+ unit 0.055d0) 1.055d0) 2.4d0))))

(-> linear->srgb-channel (real) integer)
(defun linear->srgb-channel (value)
  "Return the 0 to 255 sRGB channel for linear light VALUE, clamped."
  (let* ((clamped (max 0d0 (min 1d0 value)))
         (unit (if (<= clamped 0.0031308d0)
                   (* 12.92d0 clamped)
                   (- (* 1.055d0 (expt clamped (/ 1d0 2.4d0))) 0.055d0))))
    (round (* unit 255d0))))

(-> oklch->rgb (real real real) list)
(defun oklch->rgb (lightness chroma hue)
  "Convert OKLCH LIGHTNESS (0 to 1), CHROMA and HUE in degrees to an sRGB
(RED GREEN BLUE) list of 0 to 255, clamping colors outside the gamut."
  (let* ((radians (* hue (/ pi 180d0)))
         (a (* chroma (cos radians)))
         (b (* chroma (sin radians)))
         (l-root (+ lightness (* 0.3963377774d0 a) (* 0.2158037573d0 b)))
         (m-root (- lightness (* 0.1055613458d0 a) (* 0.0638541728d0 b)))
         (s-root (- lightness (* 0.0894841775d0 a) (* 1.2914855480d0 b)))
         (l (expt l-root 3))
         (m (expt m-root 3))
         (s (expt s-root 3)))
    (list (linear->srgb-channel (+ (* 4.0767416621d0 l) (* -3.3077115913d0 m) (* 0.2309699292d0 s)))
          (linear->srgb-channel (+ (* -1.2684380046d0 l) (* 2.6097574011d0 m) (* -0.3413193965d0 s)))
          (linear->srgb-channel (+ (* -0.0041960863d0 l) (* -0.7034186147d0 m) (* 1.7076147010d0 s))))))

(-> relative-luminance (list) double-float)
(defun relative-luminance (rgb)
  "Return the WCAG relative luminance of the (RED GREEN BLUE) list RGB."
  (destructuring-bind (red green blue) rgb
    (+ (* 0.2126d0 (srgb-channel->linear red))
       (* 0.7152d0 (srgb-channel->linear green))
       (* 0.0722d0 (srgb-channel->linear blue)))))

(-> contrast-ratio (list list) double-float)
(defun contrast-ratio (first-color second-color)
  "Return the WCAG contrast ratio between two (RED GREEN BLUE) lists."
  (let* ((first-luminance (relative-luminance first-color))
         (second-luminance (relative-luminance second-color))
         (lighter (max first-luminance second-luminance))
         (darker (min first-luminance second-luminance)))
    (/ (+ lighter 0.05d0) (+ darker 0.05d0))))

(-> rgb->hex (list) string)
(defun rgb->hex (rgb)
  "Return RGB as an uppercase CSS hex string such as #42B28A."
  (format nil "#~{~2,'0X~}" rgb))


;;; Accents

(-> accent-roles () list)
(defun accent-roles ()
  "Return the accent role names in their defined order."
  (mapcar #'first *accent-hues*))

(-> accent (keyword &key (:tier keyword)) list)
(defun accent (role &key (tier ':ink))
  "Return the (RED GREEN BLUE) accent color for ROLE in TIER.

ROLE is one of ACCENT-ROLES; TIER is :INK for strokes and glyphs that must
read against paper, or :MARK for small fills that carry black labels."
  (let ((hue (rest (assoc role *accent-hues*)))
        (tier-values (rest (assoc tier *accent-tiers*))))
    (unless hue
      (error "Unknown accent role ~S; expected one of ~S." role (accent-roles)))
    (unless tier-values
      (error "Unknown accent tier ~S; expected :INK or :MARK." tier))
    (oklch->rgb (first tier-values) (rest tier-values) hue)))


;;; Spacing and fonts

(-> spacing (integer) integer)
(defun spacing (step)
  "Return the pixel value of spacing STEP, from 1 to the scale length."
  (unless (<= 1 step (length *spacing-scale*))
    (error "Spacing step ~D is outside 1 to ~D." step (length *spacing-scale*)))
  (aref *spacing-scale* (1- step)))

(-> theme--first-installed-family (list list) (or null string))
(defun theme--first-installed-family (candidates installed)
  "Return the first of CANDIDATES that names a family in INSTALLED, a list of
(NAME . ATTRIBUTES) as SYSTEM-FONT-NAMES returns, or NIL."
  (find-if (lambda (family)
             (find family installed :key #'first :test #'string=))
           candidates))

(-> theme--face-name (string keyword) string)
(defun theme--face-name (family face)
  "Return the font name of FACE in FAMILY as the font system spells it."
  (ecase face
    (:regular family)
    (:bold (format nil "~A Bold" family))
    (:italic (format nil "~A Italic" family))
    (:bold-italic (format nil "~A Bold Italic" family))))

(-> theme-fonts-load () list)
(defun theme-fonts-load ()
  "Load the four prose faces and four code faces and return them as a plist
keyed by :PROSE, :PROSE-BOLD, :PROSE-ITALIC, :PROSE-BOLD-ITALIC, :CODE,
:CODE-BOLD, :CODE-ITALIC, :CODE-BOLD-ITALIC and :MARKER. Needs a display."
  (let* ((installed (lightfast:system-font-names))
         (prose (or (theme--first-installed-family *prose-families* installed)
                    (first *prose-families*)))
         (code (or (theme--first-installed-family *code-families* installed)
                   (first *code-families*)))
         (marker (or (theme--first-installed-family *marker-families* installed)
                     (first *marker-families*))))
    ;; FLTK draws a face with a line gap above its baseline; the corrected
    ;; copies land where every other face does, so runs in one line meet.
    (lightfast:correct-font-baselines (list prose code marker))
    (flet ((face (family style)
             (let ((name (theme--face-name family style)))
               ;; CMU Typewriter spells its bold italic as one word.
               (lightfast:load-font
                (if (and (eq style ':bold-italic)
                         (not (find name installed :key #'first :test #'string=))
                         (find (format nil "~A BoldItalic" family) installed
                               :key #'first :test #'string=))
                    (format nil "~A BoldItalic" family)
                    name)))))
      (setf *fonts*
            (list ':prose (face prose ':regular)
                  ':prose-bold (face prose ':bold)
                  ':prose-italic (face prose ':italic)
                  ':prose-bold-italic (face prose ':bold-italic)
                  ':code (face code ':regular)
                  ':code-bold (face code ':bold)
                  ':code-italic (face code ':italic)
                  ':code-bold-italic (face code ':bold-italic)
                  ':marker (lightfast:load-font marker))))))

(-> font (keyword) integer)
(defun font (face)
  "Return the loaded font number for FACE, one of the THEME-FONTS-LOAD keys."
  (let ((number (getf *fonts* face)))
    (unless number
      (error "Font ~S is not loaded; call THEME-INSTALL first." face))
    number))

(-> theme-install () null)
(defun theme-install ()
  "Load the fonts and make Lightfast create every later widget in the AUI look.
Call once on the display thread before any window exists."
  (theme-fonts-load)
  (lightfast:apply-flat-theme :background *paper*
                              :foreground *ink*
                              :selection *ink*
                              :label-font (font ':prose)
                              :label-size *prose-size*
                              :text-font (font ':prose)
                              :text-size *prose-size*
                              :mono-font (font ':code))
  nil)
