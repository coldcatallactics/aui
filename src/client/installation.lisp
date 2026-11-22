(in-package #:aui)

(-> installation--release-root (pathname) (or null pathname))
(defun installation--release-root (source)
  "Return the release directory that contains the Autolith source directory SOURCE.
Return NIL if SOURCE is a checkout."
  (let ((root (uiop:pathname-parent-directory-pathname
               (uiop:pathname-parent-directory-pathname source))))
    (and (equal (last (pathname-directory source) 2) '("libexec" "autolith"))
         (probe-file (merge-pathnames "RELEASE" root))
         (probe-file (merge-pathnames "bin/autolith" root))
         root)))

(-> installation--program (pathname) string)
(defun installation--program (source)
  "Return the namestring of the autolith command for the Autolith source directory SOURCE.
The command is the launcher of the release or of the checkout."
  (uiop:native-namestring
   (merge-pathnames "bin/autolith" (or (installation--release-root source) source))))

(-> installation-program () string)
(defun installation-program ()
  "Return the namestring of the autolith command of the Autolith used to build this client."
  (installation--program (asdf:system-source-directory ':autolith)))

(-> installation-adopt () null)
(defun installation-adopt ()
  "Set COLORLISP_NATIVE_LIBRARY to the native highlighter library of the release.
Do nothing if the variable is already set or if Autolith is a checkout.

Checkouts build that library on first use. Releases contain the library
prebuilt, because the machine may not have a compiler for it."
  (let ((root (installation--release-root (asdf:system-source-directory ':autolith))))
    (when (and root (not (uiop:getenvp "COLORLISP_NATIVE_LIBRARY")))
      (let ((library (find-if #'probe-file
                              (list (merge-pathnames "lib/libcolorlisp-tree-sitter.dylib" root)
                                    (merge-pathnames "lib/libcolorlisp-tree-sitter.so" root)))))
        (when library
          (setf (uiop:getenv "COLORLISP_NATIVE_LIBRARY") (uiop:native-namestring library))))))
  nil)
