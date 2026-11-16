;;;; -- AUI Dependencies --

;;; Make Autolith, Lightfast and AUI the systems ASDF finds, through the
;;; dependency set Autolith itself was installed with. Every script loads this
;;; file first, and so does the source checks' clean compile.
;;;
;;; AUI is a client of the Autolith it was built against, so it loads that
;;; Autolith's source and the libraries its qlfile.lock pins instead of
;;; carrying a second copy of either. Autolith is named by AUI_AUTOLITH_ROOT,
;;; or else AUTOLITH_SOURCE_ROOT, or else found behind the autolith command on
;;; the path. It is a source checkout, or a release, which keeps its source
;;; and libraries in libexec/autolith. Lightfast is named by
;;; AUI_LIGHTFAST_ROOT, or else is the checkout beside this one.

(require :asdf)

(defun setup--directory (name)
  "Return the directory the environment variable NAME holds, or NIL when it is unset or empty."
  (let ((value (uiop:getenvp name)))
    (and value (uiop:ensure-directory-pathname value))))

(defun setup--autolith-source (directory)
  "Return the directory under DIRECTORY that holds Autolith's source: DIRECTORY itself for a checkout, its libexec/autolith for a release."
  (let ((packaged (merge-pathnames "libexec/autolith/" directory)))
    (if (and (not (probe-file (merge-pathnames "autolith.asd" directory)))
             (probe-file (merge-pathnames "autolith.asd" packaged)))
        packaged
        directory)))

(defun setup--autolith-root ()
  "Return the Autolith source AUI builds against, or signal an error naming the ways to choose one."
  (setup--autolith-source
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
       (error "No Autolith found. Install Autolith so that autolith is on the path, ~
               or set AUI_AUTOLITH_ROOT to its release directory or its source checkout."))))

;;; Two SBCLs of one version share a directory of compiled files and still
;;; differ in how they were built: a release's collects garbage otherwise than
;;; one built from source. Each refuses what the other compiled, and ASDF
;;; takes that for a failed load, so a machine that has had both would build
;;; no more. Such a file is compiled again by the SBCL in use.
#+sbcl
(defmethod asdf:perform :around ((operation asdf:load-op) (file asdf:cl-source-file))
  "Load FILE, compiling it again first when its compiled file was written by an SBCL built with other features."
  (handler-case (call-next-method)
    (sb-fasl::invalid-fasl ()
      (asdf:perform (asdf:make-operation 'asdf:compile-op) file)
      (call-next-method))))

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
