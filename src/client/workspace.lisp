(in-package #:aui)

(-> workspace--configuration (string) t)
(defun workspace--configuration (directory)
  "Return an immutable Autolith configuration for the workspace at DIRECTORY."
  (autolith::configuration-create
   :immutable-p                 t
   :defer-provider-validation-p t
   :working-directory           (autolith::platform-truename
                                 autolith::*platform*
                                 (uiop:ensure-directory-pathname directory))))

(-> workspace-agenda (string) list)
(defun workspace-agenda (directory)
  "Return the agenda of the workspace at DIRECTORY, oldest item first.

Each item is a property list of :ID, :STATUS, :TEXT and :MEMORIES. The status
is one of :TODO, :DOING, :BLOCKED, :DONE and :NOTE. The memories are the
identifiers of the memories attached to the item. Signal a CLIENT-ERROR if the
agenda cannot be read."
  (handler-case
      (let* ((configuration (workspace--configuration directory))
             (record        (autolith::agenda-current configuration (autolith::agenda-load configuration))))
        (loop for item in (and record (autolith::workspace-agenda-items record))
              collect (list :id       (autolith::agenda-item-identifier item)
                            :status   (autolith::agenda-item-status item)
                            :text     (autolith::agenda-item-text item)
                            :memories (autolith::agenda-item-memory-identifiers item))))
    (error (condition)
      (error 'client-error
             :message (format nil "The agenda could not be read: ~A" (condition-report condition))))))

(-> workspace-memories (string &key (:query (or null string))) list)
(defun workspace-memories (directory &key query)
  "Return the memories that apply to the workspace at DIRECTORY, stale memories first.

If QUERY is given, only the memories found by a recall search for QUERY are
returned. Each memory is a property list of :ID, :TITLE, :CONTENT, :STATUS,
:SUMMARY, :SCOPE, :PROVENANCE and :DATE. The :ID is the short identifier that
people type. The status is one of :CURRENT, :STALE and :UNVERIFIED. The
summary is the reason why the memory is not current. Signal a CLIENT-ERROR if
the memories cannot be read."
  (handler-case
      (let* ((configuration (workspace--configuration directory))
             (memories      (if (and query (plusp (length query)))
                                (autolith::memory-search configuration query)
                                (autolith::memory-list configuration)))
             (assessor      (autolith::memory-assessor-create configuration)))
        (stable-sort
         (loop for memory in memories
               collect (let ((assessment (autolith::memory-assess assessor memory)))
                         (list :id         (autolith::memory-short-identifier memory)
                               :title      (autolith::application-memory--title memory)
                               :content    (autolith::memory-content memory)
                               :status     (autolith::memory-assessment-status assessment)
                               :summary    (autolith::memory-assessment-summary assessment)
                               :scope      (autolith::memory-scope memory)
                               :provenance (autolith::application-memory--provenance memory)
                               :date       (autolith::application-memory--date
                                            (autolith::memory-updated-at memory)))))
         #'<
         :key (lambda (memory)
                (ecase (getf memory :status)
                  (:stale
                   0)
                  (:unverified
                   1)
                  (:current
                   2)))))
    (error (condition)
      (error 'client-error
             :message (format nil "The memories could not be read: ~A" (condition-report condition))))))
