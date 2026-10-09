(in-package #:aui)

(fiveam:def-suite theme :in aui
  :description "Palette derivation and theme arithmetic.")

(fiveam:in-suite theme)

(fiveam:test logo-green-is-the-mark-tier-green
  "The mark tier green reproduces the logo color within a rounding step."
  (destructuring-bind (red green blue) (accent ':green :tier ':mark)
    (fiveam:is (<= (abs (- red #x42)) 1))
    (fiveam:is (<= (abs (- green #xB2)) 1))
    (fiveam:is (<= (abs (- blue #x8A)) 1))))

(fiveam:test ink-tier-accents-read-against-paper
  "Every ink tier accent has at least 4.5:1 contrast on the substrate."
  (dolist (role (accent-roles))
    (fiveam:is (>= (contrast-ratio (accent role) *paper*) 4.5d0)
               "~A has only ~,2F:1" role (contrast-ratio (accent role) *paper*))))

(fiveam:test accents-are-distinct
  "No two roles share a color in either tier."
  (dolist (tier '(:ink :mark))
    (let ((colors (mapcar (lambda (role) (accent role :tier tier)) (accent-roles))))
      (fiveam:is (= (length colors) (length (remove-duplicates colors :test #'equal)))))))

(fiveam:test unknown-accent-signals
  (fiveam:signals error (accent ':pink))
  (fiveam:signals error (accent ':green :tier ':wash)))

(fiveam:test hex-formatting
  (fiveam:is (string= "#FFFEFA" (rgb->hex *paper*)))
  (fiveam:is (string= "#000000" (rgb->hex *ink*))))

(fiveam:test spacing-scale
  (fiveam:is (= 4 (spacing 1)))
  (fiveam:is (= 32 (spacing 6)))
  (fiveam:signals error (spacing 0))
  (fiveam:signals error (spacing 7)))

(fiveam:test family-selection-prefers-installed
  (let ((installed '(("Liberation Serif" . 0) ("CMU Typewriter Text" . 0))))
    (fiveam:is (string= "Liberation Serif"
                        (theme--first-installed-family *prose-families* installed)))
    (fiveam:is (null (theme--first-installed-family '("Nothing Here") installed)))))

(fiveam:test face-names
  (fiveam:is (string= "Times New Roman" (theme--face-name "Times New Roman" ':regular)))
  (fiveam:is (string= "Times New Roman Bold Italic" (theme--face-name "Times New Roman" ':bold-italic))))
