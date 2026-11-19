(in-package #:aui)

;;;; -- Test Suite --

(fiveam:def-suite aui
  :description "The suite that contains all AUI tests.")

(-> run-tests (&key (:suite symbol)) boolean)
(defun run-tests (&key (suite 'aui))
  "Run SUITE. Returns T if all checks passed."
  (let ((results (fiveam:run suite)))
    (fiveam:explain! results)
    (and (fiveam:results-status results) t)))
