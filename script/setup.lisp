;;;; -- AUI Dependencies --

(require :asdf)

(defun setup--directory (name)
  "Return the directory in the environment variable NAME, or NIL if the variable is unset or empty."
  (let ((value (uiop:getenvp name)))
    (and value (uiop:ensure-directory-pathname value))))

(defun setup--autolith-source (directory)
  "Return the source directory for the Autolith release or checkout at DIRECTORY."
  (let ((packaged (merge-pathnames "libexec/autolith/" directory)))
    (if (and (not (probe-file (merge-pathnames "autolith.asd" directory)))
             (probe-file (merge-pathnames "autolith.asd" packaged)))
        packaged
        directory)))

(defun setup--autolith-root ()
  "Find the Autolith source directory or signal an error."
  (setup--autolith-source
   (or (setup--directory "AUI_AUTOLITH_ROOT")
       (setup--directory "AUTOLITH_SOURCE_ROOT")
       (let ((command (ignore-errors
                       (string-trim '(#\Space #\Newline #\Return)
                                    (uiop:run-program '("sh" "-c" "command -v autolith")
                                                      :output :string)))))
         (when (and command (plusp (length command)) (probe-file command))
           ;; The command is usually a link from ~/.local/bin/autolith to bin/autolith.
           (uiop:pathname-parent-directory-pathname
            (uiop:pathname-directory-pathname (truename command)))))
       (error "Autolith was not found. Add autolith to PATH or set AUI_AUTOLITH_ROOT to a release or source checkout."))))

;;; SBCL builds of the same version share an ASDF cache even if their FASLs are incompatible.
#+sbcl
(defmethod asdf:perform :around ((operation asdf:load-op) (file asdf:cl-source-file))
  "Recompile FILE if the cached FASL cannot be loaded by this SBCL."
  (handler-case (call-next-method)
    (sb-fasl::invalid-fasl ()
      (asdf:perform (asdf:make-operation 'asdf:compile-op) file)
      (call-next-method))))

(defun setup--require-file (pathname description)
  "Return the path of the existing file or report DESCRIPTION and the missing PATHNAME."
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
  (load (setup--require-file setup "The dependency setup of Autolith (written by script/bootstrap)"))
  (asdf:load-asd (setup--require-file (merge-pathnames "autolith.asd" autolith) "The system definition of Autolith"))
  (asdf:load-asd (setup--require-file (merge-pathnames "lightfast.asd" lightfast) "The system definition of Lightfast"))
  (asdf:load-asd (merge-pathnames "aui.asd" root)))
