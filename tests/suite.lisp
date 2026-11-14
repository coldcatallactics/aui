(in-package #:aui)

;;;; -- Test Suite --

(fiveam:def-suite aui
  :description "Every AUI test; runs without a display, a server or credentials.")

(-> run-tests (&key (:suite symbol)) boolean)
(defun run-tests (&key (suite 'aui))
  "Run SUITE and return T when every check passed."
  (let ((results (fiveam:run suite)))
    (fiveam:explain! results)
    (and (fiveam:results-status results) t)))
