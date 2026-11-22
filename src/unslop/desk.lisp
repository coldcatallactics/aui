(in-package #:aui)

;;;; -- Desk --


(defparameter *autosave-seconds* 0.7
  "The delay in seconds between the last keystroke and the write of the typed text to the file.")

(defparameter *refresh-seconds* 3
  "The interval in seconds between two looks at the checkout. A look finds new files, edited files and files changed since they were completed.")

(defparameter *passage-kinds* '(:comment :docstring :string)
  "The kinds of passages, in the order in which their switches are shown.")

(defstruct removal
  "A deleted comment, with proofs before and after deletion and its first line."
  (before nil :type (or null proof))
  (after  nil :type (or null proof))
  (line   0   :type integer))

(defclass desk ()
  ((checkout
    :initarg       :checkout
    :reader        desk-checkout
    :type          checkout
    :documentation "The folder of source that the person works through in the window.")
   (ledger
    :initarg       :ledger
    :reader        desk-ledger
    :type          ledger
    :documentation "The ledger with the marks for the checkout.")
   (window
    :initform      nil
    :accessor      desk-window
    :documentation "The window.")
   (stage
    :initform      nil
    :accessor      desk-stage
    :documentation "The drawing surface of the window.")
   (editor
    :initform      nil
    :accessor      desk-editor
    :documentation "The native text editor in which a passage is written.")
   (field
    :initform      nil
    :accessor      desk-field
    :documentation "The native field in which the file filter is typed.")
   (files
    :initarg       :files
    :initform      nil
    :accessor      desk-files
    :type          list
    :documentation "The source files of the checkout as sorted names relative to the root.")
   (edited
    :initform      nil
    :accessor      desk-edited
    :documentation "The files that differ from the last commit, as a hash table from names to T, or NIL if they are not known yet.")
   (completed
    :initform      (make-hash-table :test #'equal)
    :accessor      desk-completed
    :type          hash-table
    :documentation "The files that are completed now, as a hash table from names to T. These are the marked files that have not changed since they were marked.")
   (filter
    :initform      ""
    :accessor      desk-filter
    :type          string
    :documentation "The text typed by the person to narrow the list of files.")
   (shelf-scroll
    :initform      0
    :accessor      desk-shelf-scroll
    :type          integer
    :documentation "The scroll offset of the list of files.")
   (shelf-reveal-p
    :initform      nil
    :accessor      desk-shelf-reveal-p
    :type          boolean
    :documentation "True if the list of files is to be scrolled at the next draw, so that the open file is in view.")
   (arranger
    :initform      nil
    :accessor      desk-arranger
    :type          (or null function)
    :documentation "A function of no arguments that puts the editor over the passage being written in and scrolls that passage into view.")
   (name
    :initform      nil
    :accessor      desk-name
    :type          (or null string)
    :documentation "The name of the open file, or NIL.")
   (proof
    :initform      nil
    :accessor      desk-proof
    :type          (or null proof)
    :documentation "The open file read for its words, or NIL.")
   (page-scroll
    :initform      0
    :accessor      desk-page-scroll
    :type          integer
    :documentation "The scroll offset of the page of the open file.")
   (layouts
    :initform      (make-hash-table :test #'equal)
    :reader        desk-layouts
    :type          hash-table
    :documentation "The paragraphs laid out for the open file, as a hash table from the words of a passage to the paragraph.")
   (typed-layout
    :initform      nil
    :accessor      desk-typed-layout
    :documentation "The paragraph of the text in the editor, as (TEXT . PARAGRAPH), or NIL. It is kept apart from LAYOUTS, so that the text after each key does not add a paragraph to LAYOUTS.")
   (layout-width
    :initform      -1
    :accessor      desk-layout-width
    :type          integer
    :documentation "The width to which the paragraphs in LAYOUTS were laid out.")
   (focus
    :initform      nil
    :accessor      desk-focus
    :type          (or null cons)
    :documentation "The passage being written in as (DEFINITION . PASSAGE), or NIL. Both numbers count from 0.")
   (dirty-p
    :initform      nil
    :accessor      desk-dirty-p
    :type          boolean
    :documentation "True if typed text is not in the file yet.")
   (timer
    :initform      nil
    :accessor      desk--timer
    :documentation "The timeout for the save of the typed text, or NIL if no save is pending.")
   (saved-at
    :initform      nil
    :accessor      desk-saved-at
    :type          (or null integer)
    :documentation "The universal time of the last write to the open file from this window, or NIL.")
   (notice
    :initform      nil
    :accessor      desk-notice
    :type          (or null string)
    :documentation "A notice about an action that failed, or NIL. The notice is shown until a later action succeeds.")
   (sections
    :initform      nil
    :accessor      desk--sections
    :documentation "The digests of the definitions of the open file as (PROOF . VECTOR), or NIL. PROOF is the proof from which the digests were computed.")
   (head
    :initform      ':unknown
    :accessor      desk--head
    :documentation "The digests of the definitions of the open file in the last Git commit, or :UNKNOWN if Git did not provide them.")
   (toggled
    :initform      (make-hash-table :test #'equal)
    :reader        desk--toggled
    :type          hash-table
    :documentation "The definitions of the open file that the person folded or unfolded against the mark, as a hash table keyed by digest. These are completed definitions that were unfolded, and open definitions that were folded.")
   (removal
    :initform      nil
    :accessor      desk--removal
    :type          (or null removal)
    :documentation "The last deleted comment, or NIL.")
   (marks
    :initform      0
    :accessor      desk--marks
    :type          integer
    :documentation "The number of marks and saves made by the person. It is used to discard the results of looks at the checkout that started before a change.")
   (found
    :initform      nil
    :accessor      desk--found
    :documentation "The result of the last look at the checkout, or NIL if the result was taken up.")
   (looking-p
    :initform      nil
    :accessor      desk--looking-p
    :type          boolean
    :documentation "True while a look at the checkout is running.")
   (stamps
    :initform      (make-hash-table :test #'equal)
    :reader        desk--stamps
    :type          hash-table
    :documentation "A hash table from file names to the write date, size and digest of each file. It is used so that unchanged files are not read again."))
  (:documentation "The state of one unslop window."))

(-> desk-redraw (desk) null)
(defun desk-redraw (desk)
  "Request a new draw of the window of DESK."
  (when (desk-stage desk)
    (stage-redraw (desk-stage desk)))
  nil)

(-> desk-pathname (desk string) pathname)
(defun desk-pathname (desk name)
  "Return the pathname of the file NAME in the checkout of DESK."
  (checkout-file (desk-checkout desk) name))

(-> desk-kinds () list)
(defun desk-kinds ()
  "Return the kinds of passages that the person wants to see."
  (let ((hidden (preference ':hidden-kinds)))
    (remove-if (lambda (kind) (member kind hidden)) *passage-kinds*)))

(-> desk-toggle-kind (desk keyword) null)
(defun desk-toggle-kind (desk kind)
  "Show the passages of KIND if they are hidden, or else hide them, and save the choice. The last shown kind cannot be hidden."
  (let ((hidden (preference ':hidden-kinds)))
    (cond
      ((member kind hidden)
       (setf (preference ':hidden-kinds) (remove kind hidden)))
      ((> (length (desk-kinds)) 1)
       (desk-leave desk)
       (setf (preference ':hidden-kinds) (cons kind hidden)))))
  (desk-redraw desk)
  nil)

(-> desk-hide-completed-p () boolean)
(defun desk-hide-completed-p ()
  "Return T if the person wants completed files left out of the list."
  (and (preference ':hide-completed) t))


;;;; -- Completed and Edited --

(-> desk-completed-p (desk string) boolean)
(defun desk-completed-p (desk name)
  "Return T if the file NAME is completed, which means that it is marked and has not changed since."
  (and (gethash name (desk-completed desk)) t))

(-> desk-edited-p (desk string) boolean)
(defun desk-edited-p (desk name)
  "Return T if the file NAME differs from the last commit."
  (let ((edited (desk-edited desk)))
    (and edited (gethash name edited) t)))

(-> desk--current-digest (desk string) (or null string))
(defun desk--current-digest (desk name)
  "Return the digest of the current content of the file NAME."
  (if (and (equal name (desk-name desk)) (desk-proof desk) (not (desk-dirty-p desk)))
      (or (file-digest (desk-pathname desk name)) (proof-digest (desk-proof desk)))
      (file-digest (desk-pathname desk name))))

(-> desk-toggle-completed (desk string) null)
(defun desk-toggle-completed (desk name)
  "Mark the file NAME as completed with the current content, or remove the mark. The mark is recorded in the ledger.

If NAME is the open file, the typed text that is not written yet is written
first. The mark is then for the text that the person sees."
  (when (equal name (desk-name desk))
    (desk-save desk))
  (incf (desk--marks desk))
  (if (desk-completed-p desk name)
      (progn
        (remhash name (desk-completed desk))
        (ledger-mark (desk-ledger desk) name nil))
      (let ((digest (desk--current-digest desk name)))
        (cond
          ((null digest)
           (setf (desk-notice desk) (format nil "~A cannot be read, so it is not marked" name)))
          ((ledger-mark (desk-ledger desk) name digest)
           (setf (gethash name (desk-completed desk)) t))
          (t
           (setf (gethash name (desk-completed desk)) t
                 (desk-notice desk)
                 (format nil "the mark could not be written to ~A"
                         (uiop:native-namestring (ledger-file (desk-ledger desk)))))))))
  (desk-redraw desk)
  nil)

(-> desk-folded-p (desk string) boolean)
(defun desk-folded-p (desk folder)
  "Return T if FOLDER is folded in the list of files."
  (ledger-folded-p (desk-ledger desk) folder))

(-> desk--fold (desk list boolean) null)
(defun desk--fold (desk folders folded-p)
  "Fold FOLDERS in the list of files, or unfold them if FOLDED-P is NIL. Set a notice if the ledger could not be written."
  (unless (ledger-fold (desk-ledger desk) folders folded-p)
    (setf (desk-notice desk)
          (format nil "the folded folders could not be written to ~A"
                  (uiop:native-namestring (ledger-file (desk-ledger desk))))))
  nil)

(-> desk-toggle-folder (desk string) null)
(defun desk-toggle-folder (desk folder)
  "Fold FOLDER in the list of files, or unfold it, and record the state in the ledger."
  (desk--fold desk (list folder) (not (desk-folded-p desk folder)))
  (desk-redraw desk)
  nil)

(-> desk-completed-count (desk) integer)
(defun desk-completed-count (desk)
  "Return the number of completed files of DESK."
  (count-if (lambda (name) (desk-completed-p desk name)) (desk-files desk)))


;;;; -- Looking at the Checkout --

(-> desk--stamp-digest (hash-table pathname string) (or null string))
(defun desk--stamp-digest (stamps pathname name)
  "Return the digest of the file at PATHNAME. The file is only read if the write date or the size differs from the entry for NAME in STAMPS."
  (handler-case
      (let* ((status (sb-posix:stat (uiop:native-namestring pathname)))
             (stamp  (list (sb-posix:stat-mtime status) (sb-posix:stat-size status)))
             (known  (gethash name stamps)))
        (if (and known (equal (first known) stamp))
            (rest known)
            (let ((digest (file-digest pathname)))
              (setf (gethash name stamps) (cons stamp digest))
              digest)))
    (error ()
      nil)))

(-> desk--look (desk list list) list)
(defun desk--look (desk marked baseline)
  "Look at the checkout of DESK and return the result as a property list. The list contains the :FILES, the :EDITED hash table, the :COMPLETED hash table, and the :FIRST-SEEN files with their digests.

MARKED and BASELINE are entries of the ledger as (NAME . DIGEST). BASELINE is
only used if Git does not provide the edited files."
  (let* ((checkout  (desk-checkout desk))
         (stamps    (desk--stamps desk))
         (files     (checkout-list-files checkout))
         (edited    (checkout-edited checkout))
         (completed (make-hash-table :test #'equal))
         (seen      nil))
    (loop for (name . digest) in marked
          do (when (equal digest (desk--stamp-digest stamps (checkout-file checkout name) name))
               (setf (gethash name completed) t)))
    (unless edited
      (setf edited (make-hash-table :test #'equal))
      (dolist (name files)
        (let ((digest (desk--stamp-digest stamps (checkout-file checkout name) name))
              (first  (rest (assoc name baseline :test #'string=))))
          (cond
            ((null digest)
             nil)
            ((null first)
             (push (cons name digest) seen))
            ((string/= first digest)
             (setf (gethash name edited) t))))))
    (list :files files :edited edited :completed completed :first-seen seen)))

(-> desk-look (desk &key (:wait-p boolean)) null)
(defun desk-look (desk &key wait-p)
  "Start a look at the checkout of DESK on a separate thread, unless a look is running already. If WAIT-P is true, look now on this thread and take up the result."
  (let ((marked   (ledger-entries (desk-ledger desk) (ledger-completed (desk-ledger desk))))
        (baseline (ledger-entries (desk-ledger desk) (ledger-baseline (desk-ledger desk))))
        (marks    (desk--marks desk)))
    (cond
      (wait-p
       (setf (desk--found desk) (list* :marks marks (desk--look desk marked baseline)))
       (desk-take-up desk))
      ((not (desk--looking-p desk))
       (setf (desk--looking-p desk) t)
       (sb-thread:make-thread
        (lambda ()
          (let ((found (handler-case (desk--look desk marked baseline)
                         (error ()
                           nil))))
            (setf (desk--found desk)     (and found (list* :marks marks found))
                  (desk--looking-p desk) nil)
            (lightfast:awake)))
        :name "unslop look"))))
  nil)

(-> desk-take-up (desk) boolean)
(defun desk-take-up (desk)
  "Take up the result of the last look at the checkout, and return T if there was a result.

The completed files are only taken if nothing was marked or saved since the
look started. Otherwise a new look is started."
  (let ((found (shiftf (desk--found desk) nil)))
    (when found
      (let ((before (and (desk-name desk) (desk-edited-p desk (desk-name desk)))))
        (setf (desk-files desk)  (getf found :files)
              (desk-edited desk) (getf found :edited))
        ;; After a commit, the open file and its sections are the same as in the commit.
        (when (and (desk-name desk) (not (eq before (desk-edited-p desk (desk-name desk)))))
          (desk-read-head desk)))
      (if (= (getf found :marks) (desk--marks desk))
          (setf (desk-completed desk) (getf found :completed))
          (desk-look desk))
      (let ((seen (getf found :first-seen)))
        (when seen
          (ledger-note-baseline (desk-ledger desk) seen)))
      (desk-redraw desk)
      t)))


;;;; -- The Open File --

(-> desk--forget-layouts (desk) null)
(defun desk--forget-layouts (desk)
  "Remove the paragraphs laid out for the open file."
  (clrhash (desk-layouts desk))
  (setf (desk-typed-layout desk) nil)
  nil)

(-> desk-open-file (desk string) null)
(defun desk-open-file (desk name)
  "Open the file NAME and read it for its words. The typed text in the previous file is written first. The folders that contain NAME are unfolded in the list of files, so that NAME is in view."
  (desk-leave desk)
  (desk--forget-layouts desk)
  (setf (desk-name desk)           name
        (desk-proof desk)          (proof-read (desk-pathname desk name))
        (desk-shelf-reveal-p desk) t
        (desk-page-scroll desk)    0
        (desk-saved-at desk)    nil
        (desk-notice desk)      nil
        (desk--removal desk)    nil)
  (clrhash (desk--toggled desk))
  (desk-read-head desk)
  (desk--fold desk (checkout-folders-above name) nil)
  (desk-redraw desk)
  nil)

(-> desk-passage (desk) (or null passage))
(defun desk-passage (desk)
  "Return the passage being written in, or NIL."
  (let ((focus (desk-focus desk)))
    (and focus
         (desk-proof desk)
         (proof-passage (desk-proof desk) (first focus) (rest focus)))))

(-> desk-typed-text (desk) string)
(defun desk-typed-text (desk)
  "Return the text in the editor."
  (let ((editor (desk-editor desk)))
    (if editor (lightfast:value editor) "")))

(-> desk--cancel-timer (desk) null)
(defun desk--cancel-timer (desk)
  "Cancel the pending save of the typed text."
  (let ((timer (shiftf (desk--timer desk) nil)))
    (when timer
      (lightfast:remove-timeout timer)))
  nil)

(-> desk-note-typing (desk) null)
(defun desk-note-typing (desk)
  "Record that the text in the editor changed, and schedule a save after *AUTOSAVE-SECONDS*."
  (let ((passage (desk-passage desk)))
    (when passage
      (setf (desk-dirty-p desk)
            (string/= (passage-clean-text passage (desk-typed-text desk)) (passage-text passage)))
      (desk--cancel-timer desk)
      (when (desk-dirty-p desk)
        (setf (desk--timer desk)
              (lightfast:add-timeout *autosave-seconds*
                                     (lambda ()
                                       (with-stage-guard ("Saving")
                                         (setf (desk--timer desk) nil)
                                         (desk-save desk))))))
      (desk-redraw desk)))
  nil)

(-> desk--written (desk proof) null)
(defun desk--written (desk proof)
  "Set PROOF as the proof of the open file after a write, and update the marks and the edited files."
  (let ((name (desk-name desk)))
    (setf (desk-proof desk)     proof
          (desk-dirty-p desk)   nil
          (desk-saved-at desk)  (get-universal-time)
          (desk-notice desk)    nil
          (desk--removal desk)  nil)
    (incf (desk--marks desk))
    ;; The file content changed. The file is only completed if the mark is for this content.
    (if (ledger-completed-p (desk-ledger desk) name (proof-digest proof))
        (setf (gethash name (desk-completed desk)) t)
        (remhash name (desk-completed desk)))
    (when (and (desk-edited desk) (checkout-git-p (desk-checkout desk)))
      (setf (gethash name (desk-edited desk)) t))
    (desk-look desk))
  nil)

(-> desk--change (desk passage string &key (:remove-p boolean)) proof)
(defun desk--change (desk passage text &key remove-p)
  "Write the open file with the words of PASSAGE changed to TEXT, or without PASSAGE if REMOVE-P is true. Return the proof of the written text."
  (let* ((old (desk-proof desk))
         (new (if remove-p
                  (proof-remove old passage)
                  (proof-revise old passage text))))
    (proof-write new old)))

(-> desk-save (desk &key (:leaving-p boolean)) boolean)
(defun desk-save (desk &key leaving-p)
  "Write the typed text of the passage being written in to the file. Return T if the file contains the text afterwards.

An empty comment is only removed together with its lines if LEAVING-P is
true. If the file changed on disk in the meantime, the file is read again. If
the code is still the same, the words are written into the new content. If the
words cannot be written, the reason is set as the notice, and the words are
kept in the editor."
  (desk--cancel-timer desk)
  (let ((passage (desk-passage desk))
        (focus   (desk-focus desk)))
    (if (null passage)
        t
        (let* ((text     (passage-clean-text passage (desk-typed-text desk)))
               (empty-p  (zerop (length (string-trim '(#\Space #\Tab #\Newline) text))))
               (remove-p (and empty-p (eq (passage-kind passage) ':comment))))
          (cond
            ((string= text (passage-text passage))
             (setf (desk-dirty-p desk) nil)
             t)
            ((and remove-p (not leaving-p))
             nil)
            (t
             (flet ((attempt (passage)
                      (desk--written desk (desk--change desk passage text :remove-p remove-p))
                      (when remove-p
                        (setf (desk-focus desk) nil))
                      (desk-redraw desk)
                      t))
               (handler-case (attempt passage)
                 (proof-error (condition)
                   (if (eq (proof-error-reason condition) ':changed)
                       ;; Another program wrote the file. If the code is the same,
                       ;; the passage is still in the same place.
                       (let* ((old   (desk-proof desk))
                              (fresh (proof-read (desk-pathname desk (desk-name desk))))
                              (again (and (null (proof-problem fresh))
                                          (string= (proof-skeleton fresh) (proof-skeleton old))
                                          (proof-passage fresh (first focus) (rest focus)))))
                         (cond
                           ((and again (eq (passage-shape again) (passage-shape passage)))
                            (setf (desk-proof desk) fresh)
                            (handler-case (attempt again)
                              (proof-error (second)
                                (setf (desk-notice desk) (proof-error-message second))
                                (desk-redraw desk)
                                nil)))
                           (t
                            (setf (desk-notice desk)
                                  "not written: the file was changed by another program, so copy your words and open the file again")
                            (desk-redraw desk)
                            nil)))
                       (progn
                         (setf (desk-notice desk) (proof-error-message condition))
                         (desk-redraw desk)
                         nil)))
                 (error (condition)
                   (setf (desk-notice desk) (format nil "not written: ~A" condition))
                   (desk-redraw desk)
                   nil)))))))))

(-> desk-edit (desk fixnum fixnum &key (:caret (or null integer))) null)
(defun desk-edit (desk definition passage &key caret)
  "Start writing in the passage numbered PASSAGE of the definition numbered DEFINITION in the open file. The caret is put before the character numbered CARET, or at the end if CARET is NIL.

The typed text of another passage is written first."
  (desk-leave desk)
  (let ((found  (and (desk-proof desk) (proof-passage (desk-proof desk) definition passage)))
        (editor (desk-editor desk)))
    (when (and found editor)
      (setf (desk-focus desk)   (cons definition passage)
            (desk-dirty-p desk) nil)
      (setf (lightfast:value editor) (passage-text found))
      (setf (lightfast:text-insert-position editor)
            (if caret
                (lightfast:string-byte-length
                 (subseq (passage-text found) 0 (max 0 (min caret (length (passage-text found))))))
                (lightfast:text-length editor)))
      ;; A hidden field does not get the keyboard focus, so the editor is put in place first.
      (when (desk-arranger desk)
        (funcall (desk-arranger desk)))
      (lightfast:take-focus editor)
      (desk-redraw desk)))
  nil)

(-> desk-refocus (desk) null)
(defun desk-refocus (desk)
  "Scroll the passage being written in back into view, and give the keyboard focus to the editor."
  (when (and (desk-focus desk) (desk-editor desk))
    (when (desk-arranger desk)
      (funcall (desk-arranger desk)))
    (lightfast:take-focus (desk-editor desk))
    (desk-redraw desk))
  nil)

(-> desk-leave (desk) null)
(defun desk-leave (desk)
  "Stop writing in the passage being written in. The typed text is written, and an empty comment is removed."
  (when (desk-focus desk)
    (desk-save desk :leaving-p t)
    (setf (desk-focus desk)   nil
          (desk-dirty-p desk) nil)
    (when (desk-stage desk)
      (lightfast:take-focus (stage-canvas (desk-stage desk))))
    (desk-redraw desk))
  nil)


;;;; -- Deleting a Comment --

(-> desk--find-again (desk passage) (or null cons))
(defun desk--find-again (desk passage)
  "Return the current numbers (DEFINITION . PASSAGE) of PASSAGE in the open file, or NIL if there is none. PASSAGE is from an older proof of the file. The result is the passage with the same kind and the same words that is nearest to the old position of PASSAGE."
  (let ((nearest  nil)
        (distance nil))
    (loop for definition in (proof-definitions (desk-proof desk))
          for definition-index from 0
          do (loop for candidate in (definition-passages definition)
                   for passage-index from 0
                   do (when (and (eq (passage-kind candidate) (passage-kind passage))
                                 (string= (passage-text candidate) (passage-text passage)))
                        (let ((away (abs (- (passage-start candidate) (passage-start passage)))))
                          (when (or (null distance) (< away distance))
                            (setf nearest  (cons definition-index passage-index)
                                  distance away))))))
    nearest))

(-> desk--passage-from (desk integer) (or null cons))
(defun desk--passage-from (desk position)
  "Return the numbers (DEFINITION . PASSAGE) of the first passage in the open file that starts at POSITION or after it, or NIL if there is none. Only passages of the kinds that the person wants to see are counted."
  (let ((kinds (desk-kinds)))
    (loop for definition in (proof-definitions (desk-proof desk))
          for definition-index from 0
          do (loop for passage in (definition-passages definition)
                   for passage-index from 0
                   do (when (and (member (passage-kind passage) kinds)
                                 (>= (passage-start passage) position))
                        (return-from desk--passage-from (cons definition-index passage-index)))))))

(-> desk--delete (desk passage &key (:go-on-p boolean)) boolean)
(defun desk--delete (desk comment &key go-on-p)
  "Write the open file without COMMENT, which is a passage of the proof of the file. Return T if the write succeeded. If GO-ON-P is true, start writing in the next passage."
  (let ((before   (desk-proof desk))
        (after    nil)
        (position 0))
    (handler-case
        (multiple-value-bind (without start) (proof-remove before comment)
          (setf after    (proof-write without before)
                position start))
      (proof-error (condition)
        (setf (desk-notice desk)
              (if (eq (proof-error-reason condition) ':changed)
                  "not deleted: the file was changed by another program"
                  (proof-error-message condition))))
      (error (condition)
        (setf (desk-notice desk) (format nil "not deleted: ~A" condition))))
    (cond
      (after
       (desk--written desk after)
       (setf (desk--removal desk)
             (make-removal :before before :after after :line (passage-line comment)))
       (let ((next (and go-on-p (desk--passage-from desk position))))
         (when next
           (desk-edit desk (first next) (rest next))))
       (desk-redraw desk)
       t)
      (t
       ;; Show the file as it was written by another program.
       (desk-check-disk desk)
       (desk-redraw desk)
       nil))))

(-> desk-delete-comment (desk fixnum fixnum &key (:go-on-p boolean)) boolean)
(defun desk-delete-comment (desk definition passage &key go-on-p)
  "Delete the comment numbered PASSAGE of the definition numbered DEFINITION from the open file. If the comment is alone on its lines, the lines are deleted too. Return T if the file was written.

Typed text in the comment is deleted with it. Typed text in another passage is
written first. If GO-ON-P is true, start writing in the passage after the
comment. The comment can be brought back until something else is written to
the file."
  (let* ((focus   (desk-focus desk))
         (here-p  (and focus (= definition (first focus)) (= passage (rest focus))))
         (comment (and (desk-proof desk) (proof-passage (desk-proof desk) definition passage))))
    (cond
      ((null comment)
       nil)
      ((not (eq (passage-kind comment) ':comment))
       (setf (desk-notice desk) "only a comment can be deleted, and a string can only be emptied")
       (desk-redraw desk)
       nil)
      (t
       (cond
         (here-p
          (desk--cancel-timer desk)
          (setf (desk-focus desk)   nil
                (desk-dirty-p desk) nil)
          (when (desk-stage desk)
            (lightfast:take-focus (stage-canvas (desk-stage desk)))))
         (focus
          ;; The write of the other passage can change the numbers of the passages, so the comment
          ;; is found again.
          (let ((read (desk-proof desk)))
            (desk-leave desk)
            (unless (eq read (desk-proof desk))
              (let ((again (desk--find-again desk comment)))
                (setf comment (and again (proof-passage (desk-proof desk) (first again) (rest again)))))))))
       (and comment
            (desk--delete desk comment :go-on-p go-on-p))))))

(-> desk-removal (desk) (or null removal))
(defun desk-removal (desk)
  "Return the last deleted comment if it can be brought back, or else NIL. It can be brought back if nothing was written to the open file since the deletion and no typed text is waiting to be written."
  (let ((removal (desk--removal desk)))
    (and removal
         (eq (removal-after removal) (desk-proof desk))
         (not (desk-dirty-p desk))
         removal)))

(-> desk-restore-comment (desk) boolean)
(defun desk-restore-comment (desk)
  "Bring back the last deleted comment, and return T if the open file contains it again."
  (desk-leave desk)
  (let ((removal (desk-removal desk))
        (again   nil))
    (when removal
      (handler-case
          (setf again (proof-write (removal-before removal) (removal-after removal)))
        (proof-error (condition)
          (setf (desk-notice desk)
                (if (eq (proof-error-reason condition) ':changed)
                    "not brought back: the file was changed by another program"
                    (proof-error-message condition))))
        (error (condition)
          (setf (desk-notice desk) (format nil "not brought back: ~A" condition))))
      (if again
          (desk--written desk again)
          (desk-check-disk desk))
      (desk-redraw desk))
    (and again t)))


;;;; -- Sections --

;;; Each definition of the open file is a section of the page. Sections can be
;;; marked completed one by one. A mark is recorded by the digest of the words of
;;; the section. The mark is valid as long as the words are the same, even if the
;;; definition moves or the code changes. A completed section is folded to its
;;; heading. It is left out of the page while the person hides completed things.

(-> desk-section-digest (desk integer) string)
(defun desk-section-digest (desk index)
  "Return the digest of the words of the definition numbered INDEX in the open file."
  (let ((proof (desk-proof desk)))
    (unless (eq (first (desk--sections desk)) proof)
      (setf (desk--sections desk)
            (cons proof (map 'vector #'definition-digest (proof-definitions proof)))))
    (aref (rest (desk--sections desk)) index)))

(-> desk--section-digests (desk) list)
(defun desk--section-digests (desk)
  "Return the digests of all definitions in the open file."
  (loop for index below (length (proof-definitions (desk-proof desk)))
        collect (desk-section-digest desk index)))

(-> desk-section-completed-p (desk integer) boolean)
(defun desk-section-completed-p (desk index)
  "Return T if the definition numbered INDEX in the open file is marked completed with its current words."
  (and (member (desk-section-digest desk index)
               (ledger-sections-completed (desk-ledger desk) (desk-name desk))
               :test #'string=)
       t))

(-> desk-section-edited-p (desk integer) boolean)
(defun desk-section-edited-p (desk index)
  "Return T if the words of the definition numbered INDEX in the open file differ from the words of all definitions in the last Git commit of the file."
  (let ((head (desk--head desk)))
    (and (listp head)
         (not (member (desk-section-digest desk index) head :test #'string=))
         t)))

(-> desk-section-folded-p (desk integer) boolean)
(defun desk-section-folded-p (desk index)
  "Return T if only the heading of the definition numbered INDEX in the open file is shown. This is the case if exactly one of two things is true: the definition is completed, or the person folded it."
  (let ((toggled-p (gethash (desk-section-digest desk index) (desk--toggled desk))))
    (if (desk-section-completed-p desk index) (not toggled-p) (and toggled-p t))))

(-> desk-section-hidden-p (desk integer) boolean)
(defun desk-section-hidden-p (desk index)
  "Return T if the definition numbered INDEX in the open file is left out of the page. This is the case if it is completed and the person hides completed things."
  (and (desk-hide-completed-p) (desk-section-completed-p desk index)))

(-> desk-sections-completed-count (desk) integer)
(defun desk-sections-completed-count (desk)
  "Return the number of completed definitions in the open file."
  (loop for index below (length (proof-definitions (desk-proof desk)))
        count (desk-section-completed-p desk index)))

(-> desk-toggle-section-fold (desk integer) null)
(defun desk-toggle-section-fold (desk index)
  "Fold the definition numbered INDEX in the open file to its heading, or unfold it."
  (let ((digest (desk-section-digest desk index)))
    (if (gethash digest (desk--toggled desk))
        (remhash digest (desk--toggled desk))
        (setf (gethash digest (desk--toggled desk)) t)))
  (desk-redraw desk)
  nil)

(-> desk-toggle-section (desk integer) boolean)
(defun desk-toggle-section (desk index)
  "Mark the definition numbered INDEX in the open file as completed, or remove the mark. Return T if the ledger could be written. Typed text in the definition is written first."
  (let ((focus (desk-focus desk)))
    (when (and focus (= (first focus) index))
      (desk-leave desk)))
  (let* ((digest      (desk-section-digest desk index))
         (completed-p (not (desk-section-completed-p desk index)))
         (written-p   (ledger-mark-section (desk-ledger desk) (desk-name desk) digest
                                           :completed-p completed-p
                                           :present     (desk--section-digests desk))))
    (remhash digest (desk--toggled desk))
    (unless written-p
      (setf (desk-notice desk)
            (format nil "the mark could not be written to ~A"
                    (uiop:native-namestring (ledger-file (desk-ledger desk))))))
    (desk-redraw desk)
    written-p))

(-> desk-read-head (desk) null)
(defun desk-read-head (desk)
  "Read the open file from the last Git commit, and store the digests of the definitions in it. If Git does not provide the file, the digests are set to :UNKNOWN."
  (setf (desk--head desk) ':unknown)
  (let ((name (desk-name desk)))
    (when (and name (desk-proof desk) (null (proof-problem (desk-proof desk))))
      (let ((pathname (desk-pathname desk name)))
        (multiple-value-bind (output ok-p)
            (checkout--git (uiop:pathname-directory-pathname pathname)
                           (list "show" (format nil "HEAD:./~A" (file-namestring pathname))))
          (when ok-p
            (let ((head (proof-from-text output)))
              (unless (proof-problem head)
                (setf (desk--head desk) (mapcar #'definition-digest (proof-definitions head)))))))))
    nil))


;;;; -- The File on Disk --

(-> desk-check-disk (desk) null)
(defun desk-check-disk (desk)
  "Read the open file again if another program changed it and no typed text is waiting to be written."
  (let ((proof (desk-proof desk))
        (name  (desk-name desk)))
    (when (and proof name (not (desk-dirty-p desk)))
      (let ((digest (file-digest (desk-pathname desk name))))
        (when (and digest (string/= digest (proof-digest proof)))
          (let ((fresh (proof-read (desk-pathname desk name)))
                (focus (desk-focus desk)))
            (setf (desk-proof desk) fresh)
            (desk--forget-layouts desk)
            (desk-read-head desk)
            ;; The passage being written in can have other words now, or it can be gone.
            (when focus
              (let ((passage (proof-passage fresh (first focus) (rest focus))))
                (if (and passage (desk-editor desk))
                    (setf (lightfast:value (desk-editor desk)) (passage-text passage))
                    (setf (desk-focus desk) nil))))
            (desk-redraw desk))))))
  nil)
