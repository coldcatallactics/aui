(in-package #:aui)

;;;; -- Installation --

;;; Where the Autolith this client was built against is installed, and the
;;; command that starts it. Autolith is installed from a release or from
;;; source. A release directory holds its launcher at bin/autolith, its
;;; source at libexec/autolith and its native libraries in lib; a source
;;; checkout holds bin/autolith itself. The launcher prepares what Autolith
;;; needs to run, its image and its helpers, so whatever AUI starts of
;;; Autolith, it starts through that command and never by a path of its own.

(-> installation--release-root (pathname) (or null pathname))
(defun installation--release-root (source)
  "Return the release directory that holds the Autolith source directory SOURCE, or NIL when SOURCE is a checkout."
  (let ((root (uiop:pathname-parent-directory-pathname
               (uiop:pathname-parent-directory-pathname source))))
    (and (equal (last (pathname-directory source) 2) '("libexec" "autolith"))
         (probe-file (merge-pathnames "RELEASE" root))
         (probe-file (merge-pathnames "bin/autolith" root))
         root)))

(-> installation--program (pathname) string)
(defun installation--program (source)
  "Return the namestring of the autolith command of the Autolith whose source directory is SOURCE: the release's launcher, or the checkout's."
  (uiop:native-namestring
   (merge-pathnames "bin/autolith" (or (installation--release-root source) source))))

(-> installation-program () string)
(defun installation-program ()
  "Return the namestring of the autolith command of the Autolith this client was built against."
  (installation--program (asdf:system-source-directory ':autolith)))

(-> installation-adopt () null)
(defun installation-adopt ()
  "Name the highlighter's native library a release brings, unless the environment names one already.

A checkout builds that library on first use. A release brings it built,
because the machine it is installed on may have no compiler for it."
  (let ((root (installation--release-root (asdf:system-source-directory ':autolith))))
    (when (and root (not (uiop:getenvp "COLORLISP_NATIVE_LIBRARY")))
      (let ((library (find-if #'probe-file
                              (list (merge-pathnames "lib/libcolorlisp-tree-sitter.dylib" root)
                                    (merge-pathnames "lib/libcolorlisp-tree-sitter.so" root)))))
        (when library
          (setf (uiop:getenv "COLORLISP_NATIVE_LIBRARY") (uiop:native-namestring library))))))
  nil)
