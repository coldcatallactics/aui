(in-package #:aui)

;;;; -- Demonstration --

;;; A scripted channel that needs no server: a short conversation with every
;;; kind of entry, a channel tree, and the snapshots a live channel sends.
;;; aui --demo shows it, which is how the interface is looked at and
;;; photographed where no Autolith runs, on a build machine for one.

(defparameter *demo-answer*
  "The build fails for **two reasons**, both in the `Makefile`:

1. The library name is hardcoded as `liblightfast.so`, but macOS wants a *dynamic library* with the `.dylib` suffix.
2. The link line asks `pkg-config` for Pango and fontconfig, which FLTK does not use there.

## What I changed

```lisp
(defun library-name ()
  ;; The bridge is a dylib on macOS.
  (if (uiop:os-macosx-p) \"liblightfast.dylib\" \"liblightfast.so\"))
```

| Platform | Suffix | Text backend |
|---|---|---|
| Linux | `.so` | Pango and Cairo |
| macOS | `.dylib` | CoreText |

> Nothing else needed to change; see https://www.fltk.org for the details.

- [x] Portable Makefile
- [ ] Run the smoke tests on a Mac"
  "The answer the demonstration's agent gives.")

(-> demo--entries () list)
(defun demo--entries ()
  "Return the demonstration's conversation, oldest first."
  (list
   (make-instance 'user-entry
                  :text "Why does the build fail on macOS? Have a look at the Makefile and fix it.")
   (make-instance 'reasoning-entry
                  :text "The Makefile hardcodes a .so suffix and asks pkg-config for Pango, neither of which exists on macOS.")
   (make-instance 'tool-entry
                  :call-identifier "demo-1" :name "resource.read" :status ':completed :seconds 2/5
                  :arguments '(("uri" . "workspace:Makefile"))
                  :output    "BRIDGE := build/liblightfast.so")
   (make-instance 'tool-entry
                  :call-identifier "demo-2" :name "shell.run" :status ':failed :seconds 12
                  :arguments '(("command" . "make native && make check"))
                  :output    (format nil "c++: error: unrecognized option~%make: *** [Makefile:39] Error 1"))
   (make-instance 'tool-entry
                  :call-identifier "demo-3" :name "fs.edit" :status ':completed :seconds 3/10
                  :expanded-p t
                  :arguments  (list (cons "path" "Makefile")
                                    (cons "old-text" "BRIDGE := build/liblightfast.so")
                                    (cons "new-text" (format nil "SUFFIX := $(if $(filter Darwin,$(shell uname -s)),dylib,so)~%BRIDGE := build/liblightfast.$(SUFFIX)")))
                  :output     "Replaced 1 occurrence in Makefile.")
   (make-instance 'assistant-entry :text *demo-answer*)
   (make-instance 'output-entry
                  :invocation "(status)"
                  :text       (format nil "autolith v~A~%  model           gpt-6.1-sol (effort high)~%  workspace       ~~/common-lisp/lightfast/~%  context         40.1K of 272.0K used (15%), compacts at 80%"
                                      (world-autolith-version)))
   (make-instance 'tool-entry
                  :name "plan.update" :status ':completed
                  :arguments '(("explanation" . "Port first, then verify.")
                               ("plan" . ((("step" . "Make the Makefile portable") ("status" . "completed"))
                                          (("step" . "Guard the fontconfig code") ("status" . "in_progress"))
                                          (("step" . "Run the checks on a Mac") ("status" . "pending"))))))
   (make-instance 'user-entry :text "Thanks. Run the checks on a Mac next.")
   (make-instance 'tool-entry
                  :call-identifier "demo-4" :name "shell.run" :status ':running
                  :arguments '(("command" . "gh run watch")))))

(-> demo--tree () list)
(defun demo--tree ()
  "Return the demonstration's channel tree in the shape of Autolith's sidebar entries."
  (list (list :kind ':inbox :id "inbox" :unread 2)
        (list :kind ':workspace :key "/home/you/lightfast/" :name "lightfast" :path "~/lightfast"
              :children-p t :open-p t)
        (list :kind ':channel :id "B2Qweni" :name "lightfast" :title "Port the bridge to macOS" :depth 0
              :state ':active :current-p t :unread 0 :mentions 0 :pending 0)
        (list :kind ':channel :id "C4puSYz" :name "review" :title "Read the Makefile" :depth 1
              :state ':working :unread 3 :mentions 0 :pending 0)
        (list :kind ':channel :id "A55TkQx" :name "lightfast-2" :title "Canvas pointer events" :depth 0
              :state ':idle :waiting-for-jobs-p t :unread 0 :mentions 1 :pending 0)
        (list :kind ':workspace :key "/home/you/aui/" :name "aui" :path "~/aui" :children-p t :open-p t)
        (list :kind ':channel :id "g4P5Hsc" :name "aui" :title "A window onto the channels" :depth 0
              :state ':absent :unread 0 :mentions 0 :pending 0)
        (list :kind ':workspace :key "/home/you/notes/" :name "notes" :path "~/notes" :children-p t
              :open-p nil :hidden 4)))

(-> demo--operations () list)
(defun demo--operations ()
  "Return a few operations in the shape a channel sends them."
  (flet ((command (name description &optional hint)
           (portable-object "name" name "kind" "command" "description" description
                            "argumentHint" hint "terminalOwning" ':false "choices" '(:array))))
    (list (command "status" "Show the version, model, usage and limits.")
          (command "compact" "Compact the context now.")
          (command "model" "Choose the model that answers." "[MODEL]")
          (command "goal" "Set or show the channel's goal." "[OBJECTIVE]")
          (command "agenda" "Show the workspace agenda.")
          (command "skills" "List the skills the agent can load.")
          (command "tasks" "List the channel's jobs." "[list|get|cancel]")
          (portable-object "name" "resource.read" "kind" "tool"
                           "description" "Read a resource: a workspace file, a memory, a channel."
                           "argumentHint" nil "terminalOwning" ':false "choices" '(:array)))))

(-> demo--settings () list)
(defun demo--settings ()
  "Return a few settings in the shape a channel sends them."
  (flet ((setting (name label group type value documentation &optional options)
           (portable-object "name" name "label" label "group" group "type" type "value" value
                            "documentation" documentation "adjustable" t "source" "saved" "scope" "durable"
                            "options" (cons ':array options))))
    (list (setting "model" "Model" "model" "choice" "gpt-6.1-sol" "The model that answers."
                   '("gpt-6.1-sol" "gpt-6.1-terra"))
          (setting "reasoning-effort" "Reasoning effort" "model" "choice" "high"
                   "How much the model reasons before it answers." '("low" "medium" "high"))
          (setting "reasoning-traces-p" "Reasoning traces" "transcript" "boolean" t
                   "Show the model's reasoning summaries.")
          (setting "turn-timestamps-p" "Timestamps" "transcript" "boolean" ':false
                   "Show when each turn was sent.")
          (setting "channel-idle-minutes" "Idle minutes" "behavior" "integer" 30
                   "How long an unused channel stays in memory."))))

(-> demo-install (workbench) null)
(defun demo-install (workbench)
  "Fill WORKBENCH with the demonstration: a conversation, a channel tree and a working agent."
  (workbench--reset-channel workbench)
  (dolist (entry (demo--entries))
    (transcript-append (workbench-transcript workbench) entry))
  (setf (workbench-attachment workbench)    ':attached
        (workbench-demo-entries workbench)  (demo--tree)
        (workbench-status workbench)        (list :session-id         "B2Qweni"
                                                  :state              ':active
                                                  :cwd                "/home/you/lightfast/"
                                                  :conversation-title "Port the bridge to macOS"
                                                  :model              "gpt-6.1-sol"
                                                  :reasoning-effort   "high"
                                                  :permission-mode    ':ask
                                                  :autolith-version   (world-autolith-version))
        (workbench-state workbench)         (portable-object
                                             "usage" (portable-object "contextUsed" 40100
                                                                      "contextLimit" 272000))
        (workbench-working-p workbench)     t
        (workbench-working-since workbench) (- (get-universal-time) 42)
        (workbench-jobs workbench)          (list (portable-object "id" "job-1" "role" "review"
                                                                   "state" "running" "elapsedSeconds" 73
                                                                   "activity" "resource.read › search.content")
                                                  (portable-object "id" "job-2" "role" "shell.run"
                                                                   "state" "pending" "elapsedSeconds" nil
                                                                   "activity" "queued"))
        (workbench-operations workbench)    (demo--operations)
        (workbench-settings workbench)      (demo--settings)
        (workbench-models workbench)        (list (portable-object "id" "gpt-6.1-sol" "provider" "chatgpt")
                                                  (portable-object "id" "gpt-6.1-terra" "provider" "chatgpt")))
  nil)

(-> demo-approval () list)
(defun demo-approval ()
  "Return an approval as a channel asks for one, for showing the approval card."
  (list :id      "demo-approval"
        :kind    ':command
        :title   "run gh run watch"
        :entry   "gh run watch --exit-status 123456789"
        :options (list (list :value "auto" :description "Classify automatically this session")
                       (list :value "once" :description "Allow this command this once")
                       (list :value "always" :description "Always allow this exact command here")
                       (list :value "sandbox" :description "Allow sandboxed commands for this session")
                       (list :value "full" :description "Full user privileges for this session")
                       (list :value "deny" :description "Do not run the command"))))
