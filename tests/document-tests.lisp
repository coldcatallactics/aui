(in-package #:aui)

;;;; -- Document Tests --

(fiveam:def-suite document
  :description "A document lays out what is near its view and keeps only the heights of the rest."
  :in          aui)

(fiveam:in-suite document)

(defclass test-block (flow-block)
  ()
  (:documentation "A block of no text whose height depends on its width alone, so it is laid out without a display."))

(defmethod block-layout ((block test-block) width)
  "A hundred units tall at a width of four hundred, and taller as it narrows."
  (floor 40000 width))

(defmethod block-paint ((block test-block) x y)
  "Draw nothing."
  (declare (ignore x y))
  nil)

(defclass test-entry (entry)
  ()
  (:documentation "An entry typeset as one test block."))

(defmethod entry-blocks ((entry test-entry) toggle)
  "One block."
  (declare (ignore toggle))
  (list (make-instance 'test-block)))

(-> test-document (integer) document)
(defun test-document (count)
  "Return a document of COUNT test entries with a view four hundred units tall and a column four hundred wide, where an entry is as tall as it was taken to be."
  (let* ((transcript               (make-transcript))
         (document                 (make-document transcript))
         (*document-item-estimate* 100))
    (dotimes (index count)
      (transcript-append transcript (make-instance 'test-entry)))
    (setf (document--viewport document) (list 0 0 400 400))
    (document-sync document 400)
    document))

(-> test-laid-out (document) list)
(defun test-laid-out (document)
  "Return the indices of DOCUMENT's items that hold blocks."
  (loop for item across (document--items document)
        for index from 0
        when (item-blocks item)
          collect index))

(fiveam:test only-what-is-near-the-view-is-laid-out
  (let ((document (test-document 1000)))
    (fiveam:is (null (test-laid-out document)))
    (document--bring-near document)
    (fiveam:is (equal '(0 1 2 3 4 5 6 7) (test-laid-out document)))))

(fiveam:test items-far-from-the-view-keep-their-height-and-nothing-else
  (let ((document (test-document 1000)))
    (fiveam:is (= 100000 (document-content-height document)))
    (setf (document-scroll document) 50000)
    (document--bring-near document)
    (let ((laid (test-laid-out document)))
      (fiveam:is (member 500 laid))
      (fiveam:is (< (length laid) (+ 20 (* 2 *document-kept-items*))))
      (fiveam:is (null (member 0 laid)))
      (fiveam:is (null (member 999 laid))))
    (fiveam:is (= 100000 (document-content-height document)))
    ;; The view comes back to the start, and what is there is laid out again.
    (setf (document-scroll document) 0)
    (document--bring-near document)
    (fiveam:is (member 0 (test-laid-out document)))
    (fiveam:is (null (member 500 (test-laid-out document))))))

(fiveam:test a-new-width-is-met-by-what-is-near-and-the-view-stays-on-what-it-showed
  (let ((document (test-document 1000)))
    (setf (document-following-p document) nil
          (document-scroll document)      50000)
    (document--bring-near document)
    ;; Half the width doubles the height of what is laid out again.
    (document-sync document 200)
    (document--bring-near document)
    (let ((items (document--items document)))
      (fiveam:is (= 200 (item-height (aref items 500))))
      (fiveam:is (= 100 (item-height (aref items 0))))
      ;; The entry at the top of the view is still the one that was there.
      (fiveam:is (= (item-top (aref items 500)) (document-scroll document))))))
