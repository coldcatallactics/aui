(in-package #:aui)

;;;; -- Links and Files --

;;; The targets of links in the conversation, and the files named by an agent.
;;; Links to pages are opened in the browser. Links to local files are resolved
;;; from the folder of the channel and opened in the default program for the
;;; file. A file link can be a file URL, a path, or a path with a line number.

;;;; -- Link Targets --

(-> link-web-p (string) boolean)
(defun link-web-p (link)
  "Return T if LINK leads off this machine. Such links start with a scheme other than file, such as https or mailto."
  (let ((colon (position #\: link)))
    (and colon
         (> colon 1)
         (alpha-char-p (char link 0))
         (every (lambda (character)
                  (or (alphanumericp character) (find character "+.-")))
                (subseq link 0 colon))
         (not (string-equal "file" link :end2 colon))
         (or (uiop:string-prefix-p "//" (subseq link (1+ colon)))
             (member (subseq link 0 colon) '("mailto" "tel") :test #'string-equal))
         t)))

(-> link--percent-decode (string) string)
(defun link--percent-decode (text)
  "Return TEXT with the percent escapes decoded. The decoded octets are read as UTF-8."
  (if (null (position #\% text))
      text
      (let ((octets (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0)))
        (loop with index = 0
              while (< index (length text))
              do (let* ((character (char text index))
                        (octet     (and (char= character #\%)
                                        (<= (+ index 3) (length text))
                                        (parse-integer text
                                                       :start        (1+ index)
                                                       :end          (+ index 3)
                                                       :radix        16
                                                       :junk-allowed t))))
                   (if octet
                       (progn
                         (vector-push octet octets)
                         (incf index 3))
                       (progn
                         (loop for byte across (sb-ext:string-to-octets (string character) :external-format ':utf-8)
                               do (vector-push-extend byte octets))
                         (incf index)))))
        (handler-case (sb-ext:octets-to-string octets :external-format ':utf-8)
          (error ()
            text)))))

(-> link--without-position (string) string)
(defun link--without-position (path)
  "Return PATH without a line and column at the end, such as the :12:3 in file.lisp:12:3."
  (loop repeat 2
        do (let ((colon (position #\: path :from-end t)))
             (if (and colon
                      (plusp colon)
                      (< (1+ colon) (length path))
                      (every #'digit-char-p (subseq path (1+ colon))))
                 (setf path (subseq path 0 colon))
                 (return))))
  path)

(-> link--candidates (string) list)
(defun link--candidates (link)
  "Return a list of the paths that LINK can mean, with the most literal path first.

The scheme and the host are removed from file URLs and from workspace URIs.
The path as written comes first. Then come the path without a fragment and the
path without a line number. Then come the same paths with the percent escapes
decoded."
  (let* ((path  (cond
                  ((uiop:string-prefix-p "file://localhost/" link)
                   (subseq link 16))
                  ((uiop:string-prefix-p "file://" link)
                   (subseq link 7))
                  ((uiop:string-prefix-p "file:" link)
                   (subseq link 5))
                  ((uiop:string-prefix-p "workspace:" link)
                   (subseq link 10))
                  (t
                   link)))
         (hash  (position #\# path))
         (bare  (if hash (subseq path 0 hash) path))
         (plain (list path bare (link--without-position path) (link--without-position bare))))
    (remove-duplicates (remove "" (append plain (mapcar #'link--percent-decode plain)) :test #'string=)
                       :test     #'string=
                       :from-end t)))

(-> link--pathname (string string) (or null pathname))
(defun link--pathname (path directory)
  "Return PATH as a pathname, or NIL if PATH is not a valid pathname.

A tilde means the home folder. A relative PATH is merged with DIRECTORY."
  (handler-case
      (let ((expanded (cond
                        ((string= path "~")
                         (namestring (user-homedir-pathname)))
                        ((uiop:string-prefix-p "~/" path)
                         (concatenate 'string (namestring (user-homedir-pathname)) (subseq path 2)))
                        (t
                         path))))
        (merge-pathnames (uiop:parse-native-namestring expanded)
                         (uiop:ensure-directory-pathname (uiop:parse-native-namestring directory))))
    (error ()
      nil)))

(-> link-file (string string) (or null pathname))
(defun link-file (link directory)
  "Return the local file or folder named by LINK, or NIL if LINK leads to a page or to a missing file.

A relative path is merged with DIRECTORY."
  (unless (link-web-p link)
    (loop for candidate in (link--candidates link)
          for pathname = (link--pathname candidate directory)
          for found = (and pathname
                           (handler-case (probe-file pathname)
                             (error ()
                               nil)))
          when found
            do (return found))))


;;;; -- Opening --

(-> system--run (list) boolean)
(defun system--run (command)
  "Run COMMAND, a list of a program and its arguments. Returns T if the exit code is zero."
  (handler-case
      (eql 0 (nth-value 2 (uiop:run-program command
                                            :input               nil
                                            :output              nil
                                            :error-output        nil
                                            :ignore-error-status t)))
    (error ()
      nil)))

(-> system-open (string &key (:file-p boolean)) boolean)
(defun system-open (target &key file-p)
  "Open TARGET, the address of a page or the name of a file, in the default program of the system. Returns T on success.

On a Mac, if FILE-P is true and there is no default program for the file, the
file is opened in the text editor. If that fails, the file is shown in the Finder."
  (if (uiop:os-macosx-p)
      (or (system--run (list "open" target))
          (and file-p
               (or (system--run (list "open" "-t" target))
                   (system--run (list "open" "-R" target)))))
      (system--run (list "xdg-open" target))))

(-> workbench-open-link (workbench string) null)
(defun workbench-open-link (workbench link)
  "Open the target of LINK. Pages are opened in the browser and local files in the default program.

If LINK names no local file, or if the target cannot be opened, the error is
reported above the composer."
  (flet ((open-aside (target file-p)
           (workbench-run-aside workbench (list ':open-link target)
                                (lambda ()
                                  (unless (system-open target :file-p file-p)
                                    (error 'client-error :message "It could not be opened."))))))
    (if (link-web-p link)
        (open-aside link nil)
        (let ((file (link-file link (workbench-directory workbench))))
          (if file
              (open-aside (uiop:native-namestring file) t)
              (progn
                (setf (workbench-notice workbench)
                      (format nil "~A is not a file on this machine" link))
                (workbench-redraw workbench))))))
  nil)


;;;; -- Files Named by an Agent --

(-> files--named (string) list)
(defun files--named (text)
  "Return a list of the possible file names in the Markdown TEXT, in the written order.

The list contains the target of each link and each code span that looks like a path."
  (let ((fenced-p nil)
        (names    nil))
    (dolist (line (markdown-lines text))
      (if (uiop:string-prefix-p "```" (string-left-trim '(#\Space) line))
          (setf fenced-p (not fenced-p))
          (unless fenced-p
            (loop for (words . properties) in (markdown-inline line)
                  do (let ((link (getf properties :link)))
                       (cond
                         ((stringp link)
                          (push link names))
                         ((and (getf properties :code)
                               (or (position #\/ words) (position #\. words))
                               (null (position #\Space words)))
                          (push words names))))))))
    (nreverse names)))
