(in-package #:aui)

;;;; -- Links and Files --

;;; Where a link in the conversation leads, and the list of the files an
;;; agent named. A link to a page goes to the browser. A link to a file of
;;; this machine is found from the folder the channel works in, whatever form
;;; the agent gave it (a file URL, a path, a path with a line number after
;;; it), and handed to the program the system opens such a file with. The
;;; list gathers every such file an answer links or names in code.

;;;; -- What a Link Names --

(-> link-web-p (string) boolean)
(defun link-web-p (link)
  "Return T when LINK leads off this machine: it opens with a scheme other than file, such as https or mailto."
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
  "Return TEXT with each percent sign and two hexadecimal digits read as the octet they spell, the octets as UTF-8."
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
  "Return PATH without the line and column written after it, as in file.lisp:12:3."
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
  "Return the paths LINK may mean, the most literal first.

A file URL loses its scheme and host. After the path as written come the
path without a fragment and the path without a line number, since an agent
often points into a file, and then the same with percent signs read, as a
link spells a space."
  (let* ((path  (cond
                  ((uiop:string-prefix-p "file://localhost/" link)
                   (subseq link 16))
                  ((uiop:string-prefix-p "file://" link)
                   (subseq link 7))
                  ((uiop:string-prefix-p "file:" link)
                   (subseq link 5))
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
  "Return PATH as a pathname, a tilde read as the home folder and a relative path taken from DIRECTORY, or NIL when it cannot be one."
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
  "Return the file or folder of this machine that LINK names, or NIL when it leads to a page or to nothing that exists.

A relative path is taken from DIRECTORY, the folder the channel works in."
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
  "Run COMMAND, a program and its arguments, and return whether it ended well."
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
  "Hand TARGET, a page's address or a file's name, to the program this system opens such a thing with, and return whether one took it.

On a Mac a file that no program claims, as source code often is, is opened in
the text editor instead, and failing that shown in the Finder."
  (if (uiop:os-macosx-p)
      (or (system--run (list "open" target))
          (and file-p
               (or (system--run (list "open" "-t" target))
                   (system--run (list "open" "-R" target)))))
      (system--run (list "xdg-open" target))))

(-> workbench-open-link (workbench string) null)
(defun workbench-open-link (workbench link)
  "Open what LINK leads to: a page in the browser, a file of this machine in the program the system has for it.

A link that names no file here, and one that no program opened, are reported
above the composer."
  (flet ((open-aside (target file-p)
           (workbench-run-aside workbench (list ':open-link target)
                                (lambda ()
                                  (unless (system-open target :file-p file-p)
                                    (error 'client-error :message "No program opened it."))))))
    (if (link-web-p link)
        (open-aside link nil)
        (let ((file (link-file link (workbench-directory workbench))))
          (if file
              (open-aside (uiop:native-namestring file) t)
              (progn
                (setf (workbench-notice workbench)
                      (format nil "~A is not a file on this machine." link))
                (workbench-redraw workbench))))))
  nil)


;;;; -- The Files an Agent Named --

(-> files--named (string) list)
(defun files--named (text)
  "Return what the Markdown TEXT may name as a file, in the order written: the target of each link, and each run of code that looks like a path."
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

(-> files-linked (workbench) list)
(defun files-linked (workbench)
  "Return the files of this machine that the agent's answers in the conversation shown link or name, the one named last first, each once, as pathnames."
  (let ((directory (workbench-directory workbench))
        (files     nil))
    (loop for entry across (transcript-entries (workbench-transcript workbench))
          do (when (typep entry 'assistant-entry)
               (dolist (name (files--named (assistant-entry-text entry)))
                 (let ((file (link-file name directory)))
                   (when (and file (pathname-name file))
                     (setf files (cons file (remove file files :test #'equal))))))))
    files))

(-> files-show (workbench) null)
(defun files-show (workbench)
  "Offer the files the agent linked or named in the conversation shown; the one chosen is opened."
  (let ((files (files-linked workbench)))
    (if (null files)
        (workbench-show-panel workbench "Files"
                              (panel--empty "The agent has linked no file of this machine in this conversation yet."))
        (workbench-pick workbench
                        (mapcar (lambda (file)
                                  (make-choice :label  (file-namestring file)
                                               :detail (short-directory
                                                        (uiop:native-namestring
                                                         (uiop:pathname-directory-pathname file)))
                                               :value  (uiop:native-namestring file)))
                                files)
                        (lambda (name)
                          (workbench-open-link workbench name))
                        :title "Files")))
  nil)
