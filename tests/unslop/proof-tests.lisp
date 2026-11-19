(in-package #:aui)

;;;; -- Unslop: Proof Tests --

(fiveam:in-suite unslop)

(defparameter *test-source*
  (format nil "~{~A~%~}"
          '(";;;; -- Things --"
            ""
            ";;; A loose"
            ";;; comment."
            ""
            ";; Above the function."
            "(defun greet (name)"
            "  \"Say hello to NAME.\""
            "  ;; Inside."
            "  (format t \"Hello, ~A!\" name)) ; behind"
            ""
            "(defparameter *limit* 10"
            "  \"How many.\")"
            ""
            "(defun only-string ()"
            "  \"returned\")"
            ""
            "(defclass thing ()"
            "  ((slot :documentation \"A slot.\"))"
            "  (:documentation \"A thing.\"))"))
  "A small source file that has comments, docstrings and strings.")

(-> test-outline (proof) list)
(defun test-outline (proof)
  "Return the definitions of PROOF as a list of (OPERATOR NAME (KIND . TEXT) ...) lists."
  (loop for definition in (proof-definitions proof)
        collect (list* (definition-operator definition)
                       (definition-name definition)
                       (loop for passage in (definition-passages definition)
                             collect (cons (passage-kind passage) (passage-text passage))))))

(-> test-find-passage (proof string) passage)
(defun test-find-passage (proof text)
  "Return the passage of PROOF that has the text TEXT."
  (or (loop for definition in (proof-definitions proof)
            thereis (find text (definition-passages definition) :key #'passage-text :test #'string=))
      (error "No passage has the text ~S." text)))

(defmacro with-unslop-folder ((directory) &body body)
  "Run BODY with DIRECTORY bound to a new empty folder.
The folder and all files in it are deleted after BODY."
  `(let ((,directory (uiop:ensure-directory-pathname
                      (format nil "~Aunslop-test-~D-~D/" (uiop:temporary-directory) (get-universal-time)
                              (random 1000000)))))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,directory
                                   :validate (lambda (checked)
                                               (search "unslop-test-" (namestring checked)))))))

(-> test-write-file (pathname string) pathname)
(defun test-write-file (pathname text)
  "Write TEXT to the file at PATHNAME in UTF-8 and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede :external-format ':utf-8)
    (write-string text stream))
  pathname)

(fiveam:test a-file-is-read-into-definitions-and-passages
  (let ((proof (proof-from-text *test-source*)))
    (fiveam:is (null (proof-problem proof)))
    (fiveam:is (equal '(("" "" (:comment . "-- Things --") (:comment . "A loose
comment."))
                        ("defun" "greet"
                         (:comment . "Above the function.")
                         (:docstring . "Say hello to NAME.")
                         (:comment . "Inside.")
                         (:string . "Hello, ~A!")
                         (:comment . "behind"))
                        ("defparameter" "*limit*" (:docstring . "How many."))
                        ;; If a string is the only form of a function,
                        ;; the string is the return value and not a docstring.
                        ("defun" "only-string" (:string . "returned"))
                        ("defclass" "thing" (:docstring . "A slot.") (:docstring . "A thing.")))
                      (test-outline proof)))
    (fiveam:is (equal '(1 7 12 15 18) (mapcar #'definition-line (proof-definitions proof))))
    (let ((behind (test-find-passage proof "behind"))
          (loose  (test-find-passage proof (format nil "A loose~%comment."))))
      (fiveam:is (not (passage-alone-p behind)))
      (fiveam:is (= 10 (passage-line behind)))
      (fiveam:is (string= ";;;" (passage-prefix loose)))
      (fiveam:is (equal '(3 4) (list (passage-line loose) (passage-last-line loose)))))
    (fiveam:is (= 11 (proof-passage-count proof)))))

(fiveam:test methods-are-named-with-their-lambda-lists
  (let ((proof (proof-from-text (format nil "(defmethod draw :around ((shape circle) stream)~%  \"Draw.\"~%  (call-next-method))~%(defun (setf size) (new thing)~%  \"Set it.\"~%  new)~%"))))
    (fiveam:is (equal '(("defmethod" "draw :around ((shape circle) stream)" (:docstring . "Draw."))
                        ("defun" "(setf size)" (:docstring . "Set it.")))
                      (test-outline proof)))))

(fiveam:test comments-written-without-a-blank-keep-their-shape
  (let* ((proof   (proof-from-text (format nil ";(old code)~%;(more)~%(a)~%")))
         (passage (first (definition-passages (first (proof-definitions proof))))))
    (fiveam:is (string= (format nil "(old code)~%(more)") (passage-text passage)))
    (fiveam:is (string= "" (passage-separator passage)))
    (fiveam:is (string= (proof-text proof)
                        (proof-text (proof-revise proof passage (passage-text passage)))))))

(fiveam:test changed-words-go-back-the-way-they-were-written
  (let ((proof (proof-from-text *test-source*)))
    (flet ((revised (old new)
             (proof-text (proof-revise proof (test-find-passage proof old) new))))
      ;; A comment that has more lines than before gets the same semicolons in front of each line.
      (fiveam:is (search (format nil "~%;; Above~%;; the function now.~%(defun greet")
                         (revised "Above the function." (format nil "Above~%the function now."))))
      ;; An indented comment is written at the same indentation.
      ;; An empty line only has the semicolons.
      (fiveam:is (search (format nil "~%  ;; One.~%  ;;~%  ;; Three.~%  (format")
                         (revised "Inside." (format nil "One.  ~%~%Three."))))
      ;; A comment after code is continued below the first line, at the same column.
      (let ((margin (make-string (position #\; "  (format t \"Hello, ~A!\" name)) ; behind")
                                 :initial-element #\Space)))
        (fiveam:is (search (format nil "name)) ; behind~%~A; and below~%" margin)
                           (revised "behind" (format nil "behind~%and below")))))
      ;; Quotes and backslashes in a string are escaped.
      (fiveam:is (search "(format t \"Say \\\"hi\\\" \\\\ ~A\" name)"
                         (revised "Hello, ~A!" "Say \"hi\" \\ ~A")))
      (fiveam:is (search (format nil "  \"Say hello~%to NAME.\"~%")
                         (revised "Say hello to NAME." (format nil "Say hello~%to NAME.")))))
    ;; The original proof is not changed.
    (fiveam:is (string= *test-source* (proof-text proof)))))

(fiveam:test a-revised-proof-reads-back-what-was-typed
  (let* ((proof (proof-from-text *test-source*))
         (new   (proof-revise proof (test-find-passage proof "Hello, ~A!") "Say \"hi\" \\ ~A")))
    (fiveam:is (eq ':string (passage-kind (test-find-passage new "Say \"hi\" \\ ~A"))))
    (fiveam:is (string= (proof-skeleton proof) (proof-skeleton new)))
    (fiveam:is (= (proof-passage-count proof) (proof-passage-count new)))))

(fiveam:test words-cannot-become-code
  (let* ((proof (proof-from-text *test-source*))
         (text  (format nil "Inside.~%(delete-file \"x\")~%\"quoted\" #| |#"))
         (new   (proof-revise proof (test-find-passage proof "Inside.") text)))
    (fiveam:is (string= (proof-skeleton proof) (proof-skeleton new)))
    (fiveam:is (string= text (passage-text (test-find-passage new text)))))
  ;; A block comment cannot be closed by the new text.
  (let* ((proof   (proof-from-text (format nil "#| note |#~%(a)~%")))
         (passage (first (definition-passages (first (proof-definitions proof))))))
    (fiveam:is (string= " note " (passage-text passage)))
    (fiveam:is (string= (format nil "#| better |#~%(a)~%")
                        (proof-text (proof-revise proof passage " better "))))
    (fiveam:signals proof-error (proof-revise proof passage "end |# (b)"))))

(fiveam:test a-comment-can-be-removed-and-a-string-cannot
  (let ((proof (proof-from-text *test-source*)))
    (flet ((removed (text)
             (proof-text (proof-remove proof (test-find-passage proof text)))))
      ;; If a comment is alone on the lines, the lines are removed with it.
      (fiveam:is (search (format nil ";;;; -- Things --~%~%~%;; Above") (removed (format nil "A loose~%comment."))))
      (fiveam:is (search (format nil "\"Say hello to NAME.\"~%  (format") (removed "Inside.")))
      ;; If a comment is after code, the comment is removed with the blanks before it.
      (fiveam:is (search (format nil "name))~%~%(defparameter") (removed "behind"))))
    (fiveam:is (string= (proof-skeleton proof)
                        (proof-skeleton (proof-remove proof (test-find-passage proof "Inside.")))))
    ;; The second value is the position of the removed comment.
    ;; The text before that position is not changed.
    (let ((text (proof-text proof)))
      (multiple-value-bind (without start) (proof-remove proof (test-find-passage proof "Inside."))
        (fiveam:is (= start (1+ (position #\Newline text :end (search ";; Inside." text) :from-end t))))
        (fiveam:is (string= (subseq text 0 start) (subseq (proof-text without) 0 start)))
        (fiveam:is (string= (subseq text (1+ (position #\Newline text :start start)))
                            (subseq (proof-text without) start)))))
    (fiveam:signals proof-error (proof-remove proof (test-find-passage proof "Hello, ~A!")))))

(fiveam:test a-proof-is-written-over-the-file-it-was-read-from
  (with-unslop-folder (directory)
    (let* ((pathname (test-write-file (merge-pathnames "greet.lisp" directory) *test-source*))
           (mode     #o640))
      (sb-posix:chmod (uiop:native-namestring pathname) mode)
      (let* ((proof   (proof-read pathname))
             (new     (proof-revise proof (test-find-passage proof "How many.") "How many at most."))
             (written (proof-write new proof)))
        (fiveam:is (null (proof-problem proof)))
        (fiveam:is (string= (proof-text new) (uiop:read-file-string pathname :external-format ':utf-8)))
        (fiveam:is (string= (proof-digest written) (file-digest pathname)))
        (fiveam:is (string/= (proof-digest written) (proof-digest proof)))
        (fiveam:is (= mode (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring pathname))))))
        ;; No other file is left in the folder.
        (fiveam:is (equal '("greet.lisp") (mapcar #'file-namestring (uiop:directory-files directory))))
        ;; A file that changed since it was read is not written over.
        (let ((condition (handler-case (proof-write new proof)
                           (proof-error (condition)
                             condition))))
          (fiveam:is (typep condition 'proof-error))
          (fiveam:is (eq ':changed (proof-error-reason condition))))
        ;; The written proof can be revised and written again.
        (let ((again (proof-revise written (test-find-passage written "How many at most.") "How many.")))
          (proof-write again written)
          (fiveam:is (string= *test-source* (uiop:read-file-string pathname :external-format ':utf-8))))))))

(fiveam:test files-that-would-not-come-back-whole-are-left-alone
  (with-unslop-folder (directory)
    (let ((latin (merge-pathnames "latin.lisp" directory))
          (dos   (test-write-file (merge-pathnames "dos.lisp" directory)
                                  (format nil "(a)~C~%(b)~C~%" #\Return #\Return)))
          (open  (test-write-file (merge-pathnames "open.lisp" directory) "(a \"never closed")))
      (with-open-file (stream latin :direction ':output :element-type '(unsigned-byte 8))
        (write-sequence (coerce '(40 34 233 34 41 10) '(vector (unsigned-byte 8))) stream))
      (fiveam:is (search "UTF-8" (proof-problem (proof-read latin))))
      (fiveam:is (search "carriage return" (proof-problem (proof-read dos))))
      (fiveam:is (search "never closes" (proof-problem (proof-read open))))
      (fiveam:is (search "cannot be read" (proof-problem (proof-read (merge-pathnames "absent.lisp" directory))))))))

(fiveam:test every-passage-of-this-program-goes-back-unchanged
  ;; If a passage is written back with the same words, the file does not change.
  ;; This is tested for every file of this program.
  (let ((files     (uiop:directory-files (asdf:system-relative-pathname :aui/unslop "src/unslop/") "*.lisp"))
        (passages  0)
        (differing nil))
    (dolist (file (append files
                          (uiop:directory-files (asdf:system-relative-pathname :aui/unslop "src/ui/") "*.lisp")))
      (let ((proof (proof-read file)))
        (fiveam:is (null (proof-problem proof)))
        (dolist (definition (proof-definitions proof))
          (dolist (passage (definition-passages definition))
            (incf passages)
            (unless (string= (proof-text proof)
                             (proof-text (proof-revise proof passage (passage-text passage))))
              (push (list (file-namestring file) (passage-line passage)) differing))))))
    (fiveam:is (> passages 300))
    (fiveam:is (null differing))))
