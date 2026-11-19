(in-package #:aui)

;;;; -- Desk --

;;; What the unslop window knows and does: the checkout and its ledger, which
;;; file is open, which passage is being written in, and when to save. The
;;; panes in the other files of this directory read this state and call the
;;; actions here. Everything runs on the interface thread, except the look at
;;; the checkout that a thread of its own takes every few seconds; what that
;;; thread found is handed over and taken up on the interface thread.

(defparameter *autosave-seconds* 0.7
  "How long after the last keystroke what was typed is written to its file.")

(defparameter *refresh-seconds* 3
  "How often the checkout is looked at again for new files, edited files and files changed since they were completed.")

(defparameter *passage-kinds* '(:comment :docstring :string)
  "The kinds of passage, in the order their switches are shown.")

(defstruct removal
  "A comment deleted from the open file, as the proofs of the file BEFORE and AFTER and the LINE the comment began on."
  (before nil :type (or null proof))
  (after  nil :type (or null proof))
  (line   0   :type integer))

(defclass desk ()
  ((checkout
    :initarg       :checkout
    :reader        desk-checkout
    :type          checkout
    :documentation "The folder of source the window works through.")
   (ledger
    :initarg       :ledger
    :reader        desk-ledger
    :type          ledger
    :documentation "What is remembered about the checkout outside it.")
   (window
    :initform      nil
    :accessor      desk-window
    :documentation "The window.")
   (stage
    :initform      nil
    :accessor      desk-stage
    :documentation "The window's drawing surface.")
   (editor
    :initform      nil
    :accessor      desk-editor
    :documentation "The native text editor a passage is written in.")
   (field
    :initform      nil
    :accessor      desk-field
    :documentation "The native field the file filter is typed in.")
   (files
    :initarg       :files
    :initform      nil
    :accessor      desk-files
    :type          list
    :documentation "The checkout's source files as names from its root, sorted.")
   (edited
    :initform      nil
    :accessor      desk-edited
    :documentation "The files that differ from what was last committed, as a table from names to T, or NIL when that is not known yet.")
   (completed
    :initform      (make-hash-table :test #'equal)
    :accessor      desk-completed
    :type          hash-table
    :documentation "The files that are completed now, as a table from names to T: those marked that still hold what they held then.")
   (filter
    :initform      ""
    :accessor      desk-filter
    :type          string
    :documentation "What the person typed to narrow the list of files.")
   (shelf-scroll
    :initform      0
    :accessor      desk-shelf-scroll
    :type          integer
    :documentation "How far the list of files is scrolled.")
   (shelf-reveal-p
    :initform      nil
    :accessor      desk-shelf-reveal-p
    :type          boolean
    :documentation "Whether the list is to be scrolled so that the open file is in view when it is next drawn.")
   (arranger
    :initform      nil
    :accessor      desk-arranger
    :type          (or null function)
    :documentation "Called with no arguments to put the editor over the passage being written in and bring that passage into view. The window sets it.")
   (name
    :initform      nil
    :accessor      desk-name
    :type          (or null string)
    :documentation "The name of the file that is open, or NIL.")
   (proof
    :initform      nil
    :accessor      desk-proof
    :type          (or null proof)
    :documentation "The open file read for its words, or NIL.")
   (page-scroll
    :initform      0
    :accessor      desk-page-scroll
    :type          integer
    :documentation "How far the open file's page is scrolled.")
   (layouts
    :initform      (make-hash-table :test #'equal)
    :reader        desk-layouts
    :type          hash-table
    :documentation "Paragraphs already laid out for the open file, from a passage's words to the paragraph.")
   (typed-layout
    :initform      nil
    :accessor      desk-typed-layout
    :documentation "The paragraph of what the editor holds, as (TEXT . PARAGRAPH), or NIL. It is kept apart from the other paragraphs, so that a new text for each key does not add a paragraph that is kept.")
   (layout-width
    :initform      -1
    :accessor      desk-layout-width
    :type          integer
    :documentation "The width the kept paragraphs were laid out to.")
   (focus
    :initform      nil
    :accessor      desk-focus
    :type          (or null cons)
    :documentation "The passage being written in as (DEFINITION . PASSAGE), both counted from 0, or NIL.")
   (dirty-p
    :initform      nil
    :accessor      desk-dirty-p
    :type          boolean
    :documentation "Whether something was typed that is not in the file yet.")
   (timer
    :initform      nil
    :accessor      desk--timer
    :documentation "The timeout that will save what was typed, or NIL when none is pending.")
   (saved-at
    :initform      nil
    :accessor      desk-saved-at
    :type          (or null integer)
    :documentation "The universal time the open file was last written from this window, or NIL.")
   (notice
    :initform      nil
    :accessor      desk-notice
    :type          (or null string)
    :documentation "A line saying that something was not written, shown until the next thing works.")
   (sections
    :initform      nil
    :accessor      desk--sections
    :documentation "The digests of the open file's definitions, as (PROOF . VECTOR) for the proof they were made from, or NIL.")
   (head
    :initform      ':unknown
    :accessor      desk--head
    :documentation "The digests of the definitions the open file has in Git's last commit, or :UNKNOWN when Git does not say.")
   (toggled
    :initform      (make-hash-table :test #'equal)
    :reader        desk--toggled
    :type          hash-table
    :documentation "The definitions of the open file, by digest, that the person folded or unfolded against what their mark says: a completed one opened, an open one folded.")
   (removal
    :initform      nil
    :accessor      desk--removal
    :type          (or null removal)
    :documentation "The comment deleted last, or NIL. It can be brought back for as long as the open file holds what was written then.")
   (marks
    :initform      0
    :accessor      desk--marks
    :type          integer
    :documentation "Counts what the person marked and saved, so that a look at the checkout begun before a change does not undo it.")
   (found
    :initform      nil
    :accessor      desk--found
    :documentation "What the last look at the checkout found and the interface thread has not taken up yet, or NIL.")
   (looking-p
    :initform      nil
    :accessor      desk--looking-p
    :type          boolean
    :documentation "Whether a look at the checkout is under way.")
   (stamps
    :initform      (make-hash-table :test #'equal)
    :reader        desk--stamps
    :type          hash-table
    :documentation "For the thread that looks at the checkout: each file's write date, size and digest, so an unchanged file is not read again."))
  (:documentation "One unslop window and everything it knows."))

(-> desk-redraw (desk) null)
(defun desk-redraw (desk)
  "Ask for DESK's window to be drawn again."
  (when (desk-stage desk)
    (stage-redraw (desk-stage desk)))
  nil)

(-> desk-pathname (desk string) pathname)
(defun desk-pathname (desk name)
  "Return the pathname of the file NAME of DESK's checkout."
  (checkout-file (desk-checkout desk) name))

(-> desk-kinds () list)
(defun desk-kinds ()
  "Return the kinds of passage the person wants shown."
  (let ((hidden (preference ':hidden-kinds)))
    (remove-if (lambda (kind) (member kind hidden)) *passage-kinds*)))

(-> desk-toggle-kind (desk keyword) null)
(defun desk-toggle-kind (desk kind)
  "Show the passages of KIND when they are hidden, or hide them, and remember that. The last kind shown stays shown."
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
  "Return T when the person wants completed files left out of the list."
  (and (preference ':hide-completed) t))


;;;; -- Completed and Edited --

(-> desk-completed-p (desk string) boolean)
(defun desk-completed-p (desk name)
  "Return T when the file NAME is completed: marked, and unchanged since."
  (and (gethash name (desk-completed desk)) t))

(-> desk-edited-p (desk string) boolean)
(defun desk-edited-p (desk name)
  "Return T when the file NAME differs from what was last committed."
  (let ((edited (desk-edited desk)))
    (and edited (gethash name edited) t)))

(-> desk--current-digest (desk string) (or null string))
(defun desk--current-digest (desk name)
  "Return the digest of what the file NAME holds now."
  (if (and (equal name (desk-name desk)) (desk-proof desk) (not (desk-dirty-p desk)))
      (or (file-digest (desk-pathname desk name)) (proof-digest (desk-proof desk)))
      (file-digest (desk-pathname desk name))))

(-> desk-toggle-completed (desk string) null)
(defun desk-toggle-completed (desk name)
  "Mark the file NAME completed as it is now, or take the mark from it, and remember that outside the checkout.

What was typed in it and not yet written is written first, so the mark is
for what the person sees."
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
  "Return T when FOLDER is folded in the list of files."
  (ledger-folded-p (desk-ledger desk) folder))

(-> desk--fold (desk list boolean) null)
(defun desk--fold (desk folders folded-p)
  "Fold FOLDERS in the list of files, or unfold them when FOLDED-P is NIL, and say so when that could not be remembered outside the checkout."
  (unless (ledger-fold (desk-ledger desk) folders folded-p)
    (setf (desk-notice desk)
          (format nil "the folded folders could not be written to ~A"
                  (uiop:native-namestring (ledger-file (desk-ledger desk))))))
  nil)

(-> desk-toggle-folder (desk string) null)
(defun desk-toggle-folder (desk folder)
  "Fold FOLDER in the list of files, or unfold it, and remember that outside the checkout."
  (desk--fold desk (list folder) (not (desk-folded-p desk folder)))
  (desk-redraw desk)
  nil)

(-> desk-completed-count (desk) integer)
(defun desk-completed-count (desk)
  "Return how many of DESK's files are completed."
  (count-if (lambda (name) (desk-completed-p desk name)) (desk-files desk)))


;;;; -- Looking at the Checkout --

(-> desk--stamp-digest (hash-table pathname string) (or null string))
(defun desk--stamp-digest (stamps pathname name)
  "Return the digest of the file at PATHNAME, reading it only when its write date or size differ from what STAMPS holds for NAME."
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
  "Look at DESK's checkout and return what was found as a property list: its :FILES, the :EDITED table, the :COMPLETED table, and the :FIRST-SEEN files with their digests.

MARKED and BASELINE are the ledger's entries as (NAME . DIGEST), copied by
the caller, because this runs on a thread of its own. Where Git says what
changed, the baseline is not looked at."
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
  "Start a look at DESK's checkout on a thread of its own, unless one is under way. With WAIT-P, look now on this thread and take up what is found."
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
  "Take up what the last look at the checkout found, and return whether there was anything.

The completed files are taken only when nothing was marked or saved since
the look began, since the look would not know of it."
  (let ((found (shiftf (desk--found desk) nil)))
    (when found
      (let ((before (and (desk-name desk) (desk-edited-p desk (desk-name desk)))))
        (setf (desk-files desk)  (getf found :files)
              (desk-edited desk) (getf found :edited))
        ;; A commit makes the open file the same as Git's last commit, and its sections with it.
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
  "Forget the paragraphs laid out for the open file."
  (clrhash (desk-layouts desk))
  (setf (desk-typed-layout desk) nil)
  nil)

(-> desk-open-file (desk string) null)
(defun desk-open-file (desk name)
  "Show the file NAME, read for its words. What was being written in the file shown before is written first, and the folders NAME lies in are unfolded in the list, so that it is in view there."
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
  "Return what the editor holds."
  (let ((editor (desk-editor desk)))
    (if editor (lightfast:value editor) "")))

(-> desk--cancel-timer (desk) null)
(defun desk--cancel-timer (desk)
  "Drop the save that is waiting for the typing to stop."
  (let ((timer (shiftf (desk--timer desk) nil)))
    (when timer
      (lightfast:remove-timeout timer)))
  nil)

(-> desk-note-typing (desk) null)
(defun desk-note-typing (desk)
  "Take note that the editor's text changed: it is saved once the typing has stopped for a moment."
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
  "Take PROOF, just written to the open file, as what the file holds."
  (let ((name (desk-name desk)))
    (setf (desk-proof desk)     proof
          (desk-dirty-p desk)   nil
          (desk-saved-at desk)  (get-universal-time)
          (desk-notice desk)    nil
          (desk--removal desk)  nil)
    (incf (desk--marks desk))
    ;; The file holds something else now, so a mark on it stands only if it was for exactly this.
    (if (ledger-completed-p (desk-ledger desk) name (proof-digest proof))
        (setf (gethash name (desk-completed desk)) t)
        (remhash name (desk-completed desk)))
    (when (and (desk-edited desk) (checkout-git-p (desk-checkout desk)))
      (setf (gethash name (desk-edited desk)) t))
    (desk-look desk))
  nil)

(-> desk--change (desk passage string &key (:remove-p boolean)) proof)
(defun desk--change (desk passage text &key remove-p)
  "Write the open file with PASSAGE's words changed to TEXT, or without PASSAGE when REMOVE-P, and return the proof of what was written."
  (let* ((old (desk-proof desk))
         (new (if remove-p
                  (proof-remove old passage)
                  (proof-revise old passage text))))
    (proof-write new old)))

(-> desk-save (desk &key (:leaving-p boolean)) boolean)
(defun desk-save (desk &key leaving-p)
  "Write what was typed in the passage being written in to its file, and return whether the file now holds it.

A comment left without any words is removed with its lines, but only when
the person leaves it, which LEAVING-P says: an editor emptied to write
something else is not a wish to delete. A file that changed on disk in the
meantime is read again and the words are put into it, when its code is
still the same. What cannot be written is said in the notice, and the
editor keeps the words."
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
                       ;; Someone else wrote the file. With the same code in it the words still have their place.
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
                                  "not written: the file was changed by another program; copy your words and open the file again")
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
  "Begin writing in the passage numbered PASSAGE of the definition numbered DEFINITION of the open file, with the caret before the character numbered CARET or at the end.

What was being written elsewhere is written first."
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
      ;; A field that is not shown takes no keyboard, so the editor is put in place first.
      (when (desk-arranger desk)
        (funcall (desk-arranger desk)))
      (lightfast:take-focus editor)
      (desk-redraw desk)))
  nil)

(-> desk-refocus (desk) null)
(defun desk-refocus (desk)
  "Bring the passage being written in back into view and give the editor the keyboard."
  (when (and (desk-focus desk) (desk-editor desk))
    (when (desk-arranger desk)
      (funcall (desk-arranger desk)))
    (lightfast:take-focus (desk-editor desk))
    (desk-redraw desk))
  nil)

(-> desk-leave (desk) null)
(defun desk-leave (desk)
  "Stop writing in the passage being written in: what was typed is written, and a comment left empty is removed."
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
  "Return the numbers (DEFINITION . PASSAGE) the open file now has for PASSAGE, which was read from it before it was last written: the passage of its kind and with its words that stands nearest to where it stood. NIL when there is none."
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
  "Return the numbers (DEFINITION . PASSAGE) of the first passage of the open file, among the kinds the person wants, that begins at POSITION in the text or after it. NIL when there is none."
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
  "Write the open file without COMMENT, a comment of its proof, and return whether that worked. With GO-ON-P the passage that followed it is written in next."
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
       ;; What another program wrote is shown, so the person sees what is there now.
       (desk-check-disk desk)
       (desk-redraw desk)
       nil))))

(-> desk-delete-comment (desk fixnum fixnum &key (:go-on-p boolean)) boolean)
(defun desk-delete-comment (desk definition passage &key go-on-p)
  "Delete the comment numbered PASSAGE of the definition numbered DEFINITION from the open file, with its lines when it stands alone on them, and return whether the file was written.

What was typed in the comment goes with it, and what is being written in
another passage is written first. With GO-ON-P the passage that followed the
comment is written in next. The comment can be brought back for as long as
nothing else is written to the file."
  (let* ((focus   (desk-focus desk))
         (here-p  (and focus (= definition (first focus)) (= passage (rest focus))))
         (comment (and (desk-proof desk) (proof-passage (desk-proof desk) definition passage))))
    (cond
      ((null comment)
       nil)
      ((not (eq (passage-kind comment) ':comment))
       (setf (desk-notice desk) "only a comment can be deleted; a string can be emptied")
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
          ;; Writing the other passage may renumber the passages, so the comment is found again.
          (let ((read (desk-proof desk)))
            (desk-leave desk)
            (unless (eq read (desk-proof desk))
              (let ((again (desk--find-again desk comment)))
                (setf comment (and again (proof-passage (desk-proof desk) (first again) (rest again)))))))))
       (and comment
            (desk--delete desk comment :go-on-p go-on-p))))))

(-> desk-removal (desk) (or null removal))
(defun desk-removal (desk)
  "Return the comment deleted last when it can be brought back: nothing was written to the open file since, and nothing typed waits to be written. NIL otherwise."
  (let ((removal (desk--removal desk)))
    (and removal
         (eq (removal-after removal) (desk-proof desk))
         (not (desk-dirty-p desk))
         removal)))

(-> desk-restore-comment (desk) boolean)
(defun desk-restore-comment (desk)
  "Bring back the comment deleted last, and return whether the open file holds it again."
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

;;; Each definition of the open file is a section of the page, which can be
;;; marked completed on its own. A mark is kept by the digest of the
;;; section's words, so it holds for as long as they stay as they were,
;;; wherever the definition moves and whatever happens to its code. A
;;; completed section is folded to its heading, and left out of the page
;;; while the person hides completed things.

(-> desk-section-digest (desk integer) string)
(defun desk-section-digest (desk index)
  "Return the digest of the words of the open file's definition numbered INDEX."
  (let ((proof (desk-proof desk)))
    (unless (eq (first (desk--sections desk)) proof)
      (setf (desk--sections desk)
            (cons proof (map 'vector #'definition-digest (proof-definitions proof)))))
    (aref (rest (desk--sections desk)) index)))

(-> desk--section-digests (desk) list)
(defun desk--section-digests (desk)
  "Return the digests of every definition of the open file."
  (loop for index below (length (proof-definitions (desk-proof desk)))
        collect (desk-section-digest desk index)))

(-> desk-section-completed-p (desk integer) boolean)
(defun desk-section-completed-p (desk index)
  "Return T when the open file's definition numbered INDEX is marked completed with the words it has now."
  (and (member (desk-section-digest desk index)
               (ledger-sections-completed (desk-ledger desk) (desk-name desk))
               :test #'string=)
       t))

(-> desk-section-edited-p (desk integer) boolean)
(defun desk-section-edited-p (desk index)
  "Return T when the words of the open file's definition numbered INDEX differ from those of every definition in Git's last commit of the file."
  (let ((head (desk--head desk)))
    (and (listp head)
         (not (member (desk-section-digest desk index) head :test #'string=))
         t)))

(-> desk-section-folded-p (desk integer) boolean)
(defun desk-section-folded-p (desk index)
  "Return T when only the heading of the open file's definition numbered INDEX is shown: it is completed, or the person folded it, but not both."
  (let ((toggled-p (gethash (desk-section-digest desk index) (desk--toggled desk))))
    (if (desk-section-completed-p desk index) (not toggled-p) (and toggled-p t))))

(-> desk-section-hidden-p (desk integer) boolean)
(defun desk-section-hidden-p (desk index)
  "Return T when the open file's definition numbered INDEX is left out of the page: it is completed, and the person hides completed things."
  (and (desk-hide-completed-p) (desk-section-completed-p desk index)))

(-> desk-sections-completed-count (desk) integer)
(defun desk-sections-completed-count (desk)
  "Return how many of the open file's definitions are completed."
  (loop for index below (length (proof-definitions (desk-proof desk)))
        count (desk-section-completed-p desk index)))

(-> desk-toggle-section-fold (desk integer) null)
(defun desk-toggle-section-fold (desk index)
  "Fold the open file's definition numbered INDEX to its heading, or unfold it."
  (let ((digest (desk-section-digest desk index)))
    (if (gethash digest (desk--toggled desk))
        (remhash digest (desk--toggled desk))
        (setf (gethash digest (desk--toggled desk)) t)))
  (desk-redraw desk)
  nil)

(-> desk-toggle-section (desk integer) boolean)
(defun desk-toggle-section (desk index)
  "Mark the open file's definition numbered INDEX completed, or take the mark from it, and return whether that could be remembered. What is being written in it is written first."
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
  "Read the open file as Git's last commit has it, for which of its definitions are edited."
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
  "Read the open file again when another program changed it and nothing typed here waits to be written."
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
            ;; The passage being written in may say something else now, or be gone.
            (when focus
              (let ((passage (proof-passage fresh (first focus) (rest focus))))
                (if (and passage (desk-editor desk))
                    (setf (lightfast:value (desk-editor desk)) (passage-text passage))
                    (setf (desk-focus desk) nil))))
            (desk-redraw desk))))))
  nil)
