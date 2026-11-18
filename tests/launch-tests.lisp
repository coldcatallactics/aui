(in-package #:aui)

;;;; -- Launch Tests --

(fiveam:def-suite launch
  :description "The aui command line is Autolith's, with the window's options added."
  :in          aui)

(fiveam:in-suite launch)

(fiveam:test the-window-options-are-read
  (let ((launch (launch-parse '("--demo" "--size" "1400x900" "--view" "settings" "--draft" "hello"
                                "--permissions" "auto"))))
    (fiveam:is (launch-demo-p launch))
    (fiveam:is (equal '(1400 . 900) (launch-size launch)))
    (fiveam:is (eq ':settings (launch-view launch)))
    (fiveam:is (equal "hello" (launch-draft launch)))
    (fiveam:is (not (launch-pick-p launch)))
    (fiveam:is (null (launch-channel launch)))))

(fiveam:test autolith's-options-reach-the-world
  (let* ((launch (launch-parse '("--demo" "--permissions" "sandbox" "--model" "some-model" "--immutable")))
         (world  (launch-world launch)))
    (fiveam:is (eq ':sandboxed (world-permission-mode world)))
    (fiveam:is (world-immutable-p world))
    ;; What the command line stated goes to the server as a terminal would send it.
    (fiveam:is (equal "some-model" (getf (world-launch world) :model)))
    (fiveam:is (eq ':sandboxed (getf (world-launch world) :permission-mode)))
    (fiveam:is (getf (world-launch world) :immutable-p))))

(fiveam:test open-without-an-identifier-picks
  (let ((launch (launch-parse '("open" "--demo"))))
    (fiveam:is (launch-pick-p launch))
    (fiveam:is (null (launch-channel launch)))))

(fiveam:test the-directory-option-moves-the-workspace
  (let* ((directory (namestring (uiop:temporary-directory)))
         (launch    (launch-parse (list "--demo" "-C" directory))))
    (fiveam:is (equal (namestring (truename directory))
                      (namestring (truename (world-working-directory (launch-world launch))))))))

(fiveam:test sizes-are-parsed-or-refused
  (fiveam:is (equal '(800 . 600) (launch--parse-size "800x600")))
  (fiveam:is (null (launch--parse-size "800")))
  (fiveam:is (null (launch--parse-size "0x600")))
  (fiveam:is (null (launch--parse-size nil))))

(fiveam:test the-command-keeps-autolith's-sub-commands
  (let ((names (mapcar #'clingon:command-name (clingon:command-sub-commands (launch-command)))))
    (fiveam:is (member "open" names :test #'string=))
    (fiveam:is (member "status" names :test #'string=))
    (fiveam:is (member "auth" names :test #'string=))
    (fiveam:is (= 1 (count "open" names :test #'string=)))))

(fiveam:test what-has-no-window-is-autolith's
  (fiveam:is (launch--windowless-p (launch--read '("models"))))
  (fiveam:is (launch--windowless-p (launch--read '("status"))))
  (fiveam:is (launch--windowless-p (launch--read '("--detached"))))
  (fiveam:is (launch--windowless-p (launch--read '("open" "--detached"))))
  (fiveam:is (launch--windowless-p (launch--read '("--pristine"))))
  (fiveam:is (not (launch--windowless-p (launch--read '("--demo")))))
  (fiveam:is (not (launch--windowless-p (launch--read '("open")))))
  (fiveam:is (not (launch--windowless-p (launch--read '("--model" "some-model"))))))

(fiveam:test a-window's-options-are-refused-where-there-is-none
  (fiveam:signals client-error
    (launch-parse '("--size" "800x600" "models")))
  (fiveam:signals client-error
    (launch-parse '("-C" "/tmp" "--detached"))))

(fiveam:test the-autolith-program-is-the-one-built-against
  (fiveam:is (probe-file (installation-program))))

(fiveam:test a-release-is-started-by-its-own-launcher
  (let* ((root     (uiop:ensure-directory-pathname
                    (format nil "~Aaui-installation-~D/" (uiop:temporary-directory) (random 1000000))))
         (release  (merge-pathnames "releases/v1.2.3/" root))
         (packaged (merge-pathnames "libexec/autolith/" release))
         (checkout (merge-pathnames "checkout/" root)))
    (unwind-protect
         (progn
           (dolist (file (list (merge-pathnames "RELEASE" release)
                               (merge-pathnames "bin/autolith" release)
                               (merge-pathnames "bin/autolith" packaged)
                               (merge-pathnames "bin/autolith" checkout)))
             (ensure-directories-exist file)
             (with-open-file (stream file :direction ':output :if-exists ':supersede)
               (write-line "placeholder" stream)))
           ;; A release keeps its source two directories beneath its launcher.
           (fiveam:is (equal (installation--release-root packaged) release))
           (fiveam:is (string= (installation--program packaged)
                               (uiop:native-namestring (merge-pathnames "bin/autolith" release))))
           ;; A checkout is started by the launcher it holds itself.
           (fiveam:is (null (installation--release-root checkout)))
           (fiveam:is (string= (installation--program checkout)
                               (uiop:native-namestring (merge-pathnames "bin/autolith" checkout)))))
      (uiop:delete-directory-tree root :validate (lambda (directory)
                                                   (search "aui-installation-" (namestring directory)))))))
