;;;; -- Source checks --

;;; Install the checker tools and run cheque with the command-line arguments.
;;;
;;; Usage: sbcl --script script/check-source.lisp [--fix] [--no-warnings] [--no-parinfer]

(require :asdf)
(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))
(require :sb-md5)

(defparameter *check-floating-tools* '("cheque")
  "Development tools that track their default branch.")

(defparameter *check-refresh-seconds* 3600
  "Seconds between refreshes of floating development tools.")

(defun check--tool-files (root)
  "Return the development-tool files under ROOT/checks/."
  (sort (uiop:directory-files (merge-pathnames "checks/" root))
        #'string<
        :key #'file-namestring))

(defun check--tool-root (root)
  "Return the cache directory for ROOT's current tool files."
  (let ((digest (sb-md5:md5sum-string
                 (with-output-to-string (stream)
                   (dolist (file (check--tool-files root))
                     (write-string (file-namestring file) stream)
                     (write-char #\Nul stream)
                     (write-string (uiop:read-file-string file) stream)
                     (write-char #\Nul stream)))
                 :external-format ':utf-8)))
    (uiop:xdg-cache-home "cheque/tools/" (format nil "~(~{~2,'0x~}~)/" (coerce digest 'list)))))

(defun check--run-qlot (tools &rest projects)
  "Install TOOLS or update PROJECTS in a child SBCL process.

The child loads qlot from Quicklisp with its default ASDF configuration."
  (let ((form `(progn
                 (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
                 (funcall (intern "QUICKLOAD" "QL") :qlot :silent t)
                 (progv (list (find-symbol "*PROJECT-ROOT*" "QLOT"))
                     (list ,(uiop:native-namestring tools))
                   ,(if projects
                        `(funcall (find-symbol "UPDATE" "QLOT") ',projects)
                        `(funcall (find-symbol "INSTALL" "QLOT")))))))
    (zerop (nth-value 2 (uiop:run-program (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                                                "--noinform" "--non-interactive" "--no-userinit"
                                                "--eval" (with-standard-io-syntax
                                                           (prin1-to-string form)))
                                          :directory           tools
                                          :output              ':interactive
                                          :error-output        ':interactive
                                          :ignore-error-status t)))))

(defun check--tools-complete-p (tools)
  "Return whether every Git tool in TOOLS/qlfile has been installed.

qlot can report success after a network failure leaves a distribution absent."
  (every (lambda (line)
           (let ((words (uiop:split-string line :separator " ")))
             (or (not (equal (first words) "git"))
                 (and (uiop:subdirectories
                       (merge-pathnames (format nil ".qlot/dists/~A/software/" (second words)) tools))
                      t))))
         (uiop:read-file-lines (merge-pathnames "qlfile" tools))))

(defun check--touch (file)
  "Rewrite FILE so its modification time is current."
  (with-open-file (stream file :direction ':output :if-exists ':supersede)
    (write-line (file-namestring file) stream)))

(defun check--ensure-tools (root)
  "Install the tools for ROOT, refresh floating tools when needed, and return setup.lisp."
  (let* ((tools     (check--tool-root root))
         (installed (merge-pathnames "installed" tools))
         (refreshed (merge-pathnames "refreshed" tools)))
    (unless (probe-file installed)
      (format t "~&Installing the locked development tools.~%")
      (finish-output)
      (dolist (file (check--tool-files root))
        (uiop:copy-file file (ensure-directories-exist (merge-pathnames (file-namestring file) tools))))
      (unless (and (check--run-qlot tools) (check--tools-complete-p tools))
        (format t "~&Installing the development tools failed. They are installed again at the next check.~%")
        (uiop:delete-directory-tree tools :validate t)
        (uiop:quit 2))
      (check--touch installed))
    (when (or (not (probe-file refreshed))
              (> (- (get-universal-time) (file-write-date refreshed)) *check-refresh-seconds*))
      (format t "~&Refreshing ~{~A~^, ~}.~%" *check-floating-tools*)
      (finish-output)
      (if (apply #'check--run-qlot tools *check-floating-tools*)
          (check--touch refreshed)
          (format t "~&Refreshing the development tools failed. The installed versions are used.~%")))
    (merge-pathnames ".qlot/setup.lisp" tools)))

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname (truename *load-truename*)))))
  (load (check--ensure-tools root))
  (asdf:load-system "cheque")
  (uiop:quit (uiop:symbol-call '#:cheque '#:main (uiop:command-line-arguments) :root root)))
