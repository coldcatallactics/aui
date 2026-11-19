;;;; -- AUI Dependencies --

;;; Register the systems of Autolith, Lightfast and AUI with ASDF. The
;;; dependencies are the ones that Autolith was installed with.
;;;
;;; AUI loads the source of the installed Autolith and the libraries pinned by
;;; the qlfile.lock of that Autolith. The Autolith directory is taken from
;;; AUI_AUTOLITH_ROOT, then from AUTOLITH_SOURCE_ROOT, then from the autolith
;;; command on the path. It is a source checkout or a release. A release has
;;; the source and the libraries in libexec/autolith. The Lightfast directory
;;; is taken from AUI_LIGHTFAST_ROOT, or else it is the checkout beside AUI.

(require :asdf)

(defun setup--directory (name)
  "Return the directory in the environment variable NAME, or NIL if the variable is unset or empty."
  (let ((value (uiop:getenvp name)))
    (and value (uiop:ensure-directory-pathname value))))

(defun setup--autolith-source (directory)
  "Return the directory of the Autolith source in DIRECTORY. This is DIRECTORY for a source checkout and libexec/autolith in DIRECTORY for a release."
  (let ((packaged (merge-pathnames "libexec/autolith/" directory)))
    (if (and (not (probe-file (merge-pathnames "autolith.asd" directory)))
             (probe-file (merge-pathnames "autolith.asd" packaged)))
        packaged
        directory)))

(defun setup--autolith-root ()
  "Return the directory of the Autolith source that AUI is built against. Signals an error if no Autolith is found."
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
       (error "Autolith was not found. Install Autolith so that the autolith command is on the path, ~
               or set AUI_AUTOLITH_ROOT to the release directory or the source checkout of Autolith."))))

;;; Two SBCLs of the same version share a directory of compiled files. The
;;; SBCL of a release and an SBCL built from source have different features,
;;; such as the garbage collector. Files compiled by one of them cannot be
;;; loaded by the other, and ASDF treats that as a failed load. Such files are
;;; compiled again by the SBCL in use.
#+sbcl
(defmethod asdf:perform :around ((operation asdf:load-op) (file asdf:cl-source-file))
  "Load FILE. If the compiled file was written by an SBCL with other features, compile FILE again first."
  (handler-case (call-next-method)
    (sb-fasl::invalid-fasl ()
      (asdf:perform (asdf:make-operation 'asdf:compile-op) file)
      (call-next-method))))

(defun setup--require-file (pathname description)
  "Return PATHNAME if the file exists. Otherwise, signal an error which says that DESCRIPTION is missing."
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
