;;;; -- AUI Dependencies --

;;; Make Autolith, Lightfast and AUI the systems ASDF finds, through the
;;; dependency set Autolith itself was installed with. Every script loads this
;;; file first, and so does the source checks' clean compile.
;;;
;;; AUI is a client of the Autolith it was built against, so it loads that
;;; Autolith's source and the libraries its qlfile.lock pins instead of
;;; carrying a second copy of either. The Autolith checkout is named by
;;; AUI_AUTOLITH_ROOT, or else AUTOLITH_SOURCE_ROOT, or else found behind the
;;; autolith command on the path. Lightfast is named by AUI_LIGHTFAST_ROOT, or
;;; else is the checkout beside this one.

(require :asdf)

(defun setup--directory (name)
  "Return the directory the environment variable NAME holds, or NIL when it is unset or empty."
  (let ((value (uiop:getenvp name)))
    (and value (uiop:ensure-directory-pathname value))))

(defun setup--autolith-root ()
  "Return the Autolith source checkout AUI builds against, or signal an error naming the ways to choose one."
  (or (setup--directory "AUI_AUTOLITH_ROOT")
      (setup--directory "AUTOLITH_SOURCE_ROOT")
      (let ((command (ignore-errors
                      (string-trim '(#\Space #\Newline #\Return)
                                   (uiop:run-program '("sh" "-c" "command -v autolith")
                                                     :output :string)))))
        (when (and command (plusp (length command)) (probe-file command))
          ;; TRUENAME follows the usual ~/.local/bin/autolith link to bin/autolith.
          (uiop:pathname-parent-directory-pathname
           (uiop:pathname-directory-pathname (truename command)))))
      (error "No Autolith checkout found. Install Autolith so that autolith is on the path, ~
              or set AUI_AUTOLITH_ROOT to its source directory.")))

(defun setup--require-file (pathname description)
  "Return PATHNAME, or signal an error saying that DESCRIPTION is missing there."
  (or (probe-file pathname)
      (error "~A is missing at ~A." description (uiop:native-namestring pathname))))

(let* ((root      (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname (truename *load-truename*))))
       (autolith  (setup--autolith-root))
       (lightfast (or (setup--directory "AUI_LIGHTFAST_ROOT")
                      (merge-pathnames "../lightfast/" root)))
       (setup     (let ((override (uiop:getenvp "AUTOLITH_PROJECT_SETUP")))
                    (if override
                        (pathname override)
                        (merge-pathnames ".qlot/setup.lisp" autolith)))))
  (load (setup--require-file setup "Autolith's dependency setup, written by its script/bootstrap,"))
  (asdf:load-asd (setup--require-file (merge-pathnames "autolith.asd" autolith) "Autolith's system definition"))
  (asdf:load-asd (setup--require-file (merge-pathnames "lightfast.asd" lightfast) "Lightfast's system definition"))
  (asdf:load-asd (merge-pathnames "aui.asd" root)))
