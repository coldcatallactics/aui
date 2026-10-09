(in-package #:aui)

(fiveam:def-suite palette :in aui
  :description "Fuzzy matching for the command palette.")

(fiveam:in-suite palette)

(fiveam:test fuzzy-scores-subsequences
  (fiveam:is (fuzzy-score "cmp" "/compact"))
  (fiveam:is (null (fuzzy-score "xyz" "/compact")))
  (fiveam:is (> (fuzzy-score "comp" "/compact") (fuzzy-score "cmpt" "/compact")))
  (fiveam:is (fuzzy-score "" "anything")))

(fiveam:test search-orders-best-first-and-limits
  (let* ((items (list (make-palette-item ':operation "/compact" "" nil)
                      (make-palette-item ':operation "plan.list" "" nil)
                      (make-palette-item ':setting "Compaction threshold" "" nil)))
         (matches (palette-search items "comp")))
    (fiveam:is (equal '("/compact" "Compaction threshold") (mapcar #'palette-item-label matches)))
    (fiveam:is (= 1 (length (palette-search items "" :limit 1))))
    (fiveam:is (null (palette-search items "zzz")))))

(fiveam:test setting-values-read-as-words
  (fiveam:is (string= "on" (setting-entry-value-text (make-instance 'setting-entry :name ':a :label "A" :type ':boolean :value t))))
  (fiveam:is (string= "off" (setting-entry-value-text (make-instance 'setting-entry :name ':a :label "A" :type ':boolean :value nil))))
  (fiveam:is (string= "80" (setting-entry-value-text (make-instance 'setting-entry :name ':a :label "A" :type ':integer :value 80)))))
