(in-package #:aui)

;;;; -- Demonstration --

(defparameter *demo-answer*
    "The build fails for two reasons:

1. The Makefile uses `liblightfast.so`; macOS uses `liblightfast.dylib`.
2. The link line asks `pkg-config` for Pango and fontconfig, which this FLTK build does not use.

## Changes

```lisp
(defun library-name ()
  (if (uiop:os-macosx-p) \"liblightfast.dylib\" \"liblightfast.so\"))
```

| Platform | Suffix | Text backend |
|---|---|---|
| Linux | `.so` | Pango and Cairo |
| macOS | `.dylib` | CoreText |

`fltk-config --ldflags` already names the frameworks, so remove the duplicate flags.

- [x] Portable Makefile
- [ ] Run the smoke tests on macOS"
  "The answer of the agent in the demonstration.")

(-> demo--entries () list)
(defun demo--entries ()
  "Return the entries of the demonstration conversation, with the oldest first."
  (list
   (make-instance 'user-entry
                  :text (format nil "Why does the build fail on macOS?~2%- have a look at the `Makefile`~%- fix it, and tell me what you changed"))
   (make-instance 'reasoning-entry
                    :text "It hardcodes a .so suffix and adds Pango and fontconfig flags that this FLTK build does not need.")
   (make-instance 'tool-entry
                  :call-identifier "demo-1" :name "resource.read" :status ':completed :seconds 2/5
                  :arguments '(("uri" . "workspace:Makefile"))
                  :output    "BRIDGE := build/liblightfast.so")
   (make-instance 'tool-entry
                  :call-identifier "demo-2" :name "shell.run" :status ':failed :seconds 12
                  :arguments '(("command" . "make native && make check"))
                  :output    (format nil "c++: error: unrecognized option~%make: *** [Makefile:39] Error 1"))
   (make-instance 'notice-entry
                  :tone     ':failure
                    :text   "job failed · exec:117"
                    :detail (format nil "exit 127~%Execution: exited; timeout no; cancelled no~%combined output: 164 bytes retained, 164 bytes observed; capture-complete yes; status complete~%make: c++: command not found~%make: *** [Makefile:39] Error 127"))
   (make-instance 'tool-entry
                  :call-identifier "demo-3" :name "fs.edit" :status ':completed :seconds 3/10
                  :expanded-p t
                  :arguments  (list (cons "path" "Makefile")
                                    (cons "old-text" "BRIDGE := build/liblightfast.so")
                                    (cons "new-text" (format nil "SUFFIX := $(if $(filter Darwin,$(shell uname -s)),dylib,so)~%BRIDGE := build/liblightfast.$(SUFFIX)")))
                  :output     "Replaced 1 occurrence in Makefile.")
   (make-instance 'tool-entry
                  :call-identifier "demo-5" :name "lisp.eval" :status ':completed :seconds 1/10
                  :expanded-p t
                  :arguments  (list (cons "forms"
                                          (list "(let ((suffix (if (uiop:os-macosx-p) \"dylib\" \"so\"))) (format nil \"build/liblightfast.~A\" suffix))")))
                  :output     "\"build/liblightfast.so\"")
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
   (make-instance 'user-entry
                  :sender "review"
                  :origin "C4puSYz"
                  :text   "The Makefile reads well. One thing: `fltk-config --ldflags` already names the frameworks, so the line that adds them again can go.")
   (make-instance 'user-entry :text "Thanks. Run the checks on a Mac next.")
   (make-instance 'tool-entry
                  :call-identifier "demo-4" :name "shell.run" :status ':running
                  :arguments '(("command" . "gh run watch")))))

(-> demo--tree () list)
(defun demo--tree ()
  "Return the channel tree of the demonstration in the shape of the sidebar entries of Autolith."
  (list (list :kind ':inbox :id "home" :unread 2)
        (list :kind ':workspace :key "/home/you/lightfast/" :name "lightfast" :path "~/lightfast"
              :indent 2 :children-p t :open-p t)
        (list :kind ':channel :id "B2Qweni" :name "bridge-port" :title "Port the bridge to macOS" :depth 0
              :indent 2 :state ':active :current-p t :unread 0 :mentions 0 :pending 0)
        (list :kind ':channel :id "C4puSYz" :name "review" :title "Read the Makefile" :depth 1
              :parent "B2Qweni" :indent 2 :state ':working :unread 3 :mentions 0 :pending 0)
        (list :kind ':channel :id "D7mWx2a" :name "fonts" :title "Find the fonts on a Mac" :depth 2
              :parent "C4puSYz" :indent 2 :state ':idle :waiting-for-jobs-p t :unread 0 :mentions 0
              :pending 0)
        (list :kind ':channel :id "E3nQr8b" :name "smoke" :title "Write the smoke tests" :depth 1
              :parent "B2Qweni" :indent 2 :state ':absent :unread 0 :mentions 0 :pending 0)
        (list :kind ':channel :id "A55TkQx" :name "pointer-events" :title "Canvas pointer events" :depth 0
              :indent 2 :state ':idle :waiting-for-jobs-p t :unread 0 :mentions 1 :pending 0)
        (list :kind ':workspace :key "/home/you/aui/" :name "aui" :path "~/aui" :indent 2 :children-p t
              :open-p t)
        (list :kind ':channel :id "g4P5Hsc" :name "window" :title "A window onto the channels" :depth 0
              :indent 2 :state ':absent :unread 0 :mentions 0 :pending 0)
        (list :kind ':workspace :key "/home/you/notes/" :name "notes" :path "~/notes" :indent 2
              :children-p t :open-p nil :hidden 4)))

(-> demo--operations () list)
(defun demo--operations ()
  "Return a few operations in the shape used by channels."
  (flet ((command (name description &optional hint)
           (portable-object "name" name "kind" "command" "description" description
                            "argumentHint" hint "terminalOwning" ':false "choices" '(:array))))
    (list (command "status" "Show the version, model, usage and limits.")
          (command "compact" "Compact the context now.")
          (command "model" "Choose the model for replies." "[MODEL]")
          (command "goal" "Set or show the channel goal." "[OBJECTIVE]")
          (command "agenda" "Show the workspace agenda.")
          (command "skills" "List the skills the agent can load.")
          (command "tasks" "List the channel's jobs." "[list|get|cancel]")
          (portable-object "name" "resource.read" "kind" "tool"
                           "description" "Read a resource: a workspace file, a memory, a channel."
                           "argumentHint" nil "terminalOwning" ':false "choices" '(:array)))))

(-> demo--settings () list)
(defun demo--settings ()
  "Return a few settings in the shape used by channels."
  (flet ((setting (name label group type value documentation &optional options)
           (portable-object "name" name "label" label "group" group "type" type "value" value
                            "documentation" documentation "adjustable" t "source" "saved" "scope" "durable"
                            "options" (cons ':array options))))
      (list (setting "model" "Model" "model" "choice" "gpt-6.1-sol" "Model used for replies."
                     '("gpt-6.1-sol" "gpt-6.1-terra"))
            (setting "reasoning-effort" "Reasoning effort" "model" "choice" "high"
                     "How much reasoning to use before replying." '("low" "medium" "high"))
            (setting "reasoning-traces-p" "Reasoning traces" "transcript" "boolean" t
                     "Show reasoning summaries.")
            (setting "turn-timestamps-p" "Timestamps" "transcript" "boolean" ':false
                     "Show when turns were sent.")
            (setting "channel-idle-minutes" "Idle minutes" "behavior" "integer" 30
                     "Minutes before an unused channel leaves memory."))))

(-> demo-agenda () list)
(defun demo-agenda ()
  "Return an agenda in the shape returned by the workspace reader."
  (list (list :id "a-7Hq2" :status ':done :text "Make the Makefile portable" :memories nil)
        (list :id "a-9Kd4" :status ':doing :text "Guard the fontconfig code behind the platform" :memories '("m-1"))
        (list :id "a-2Xp8" :status ':todo :text "Run the smoke tests on a Mac" :memories nil)
        (list :id "a-5Lm1" :status ':blocked :text "Retina rendering: needs someone at a screen" :memories nil)
        (list :id "a-3Rt6" :status ':note :text "Homebrew's FLTK is a shared library" :memories nil)))

(-> demo-memories () list)
(defun demo-memories ()
  "Return a list of memories in the shape returned by the workspace reader."
  (list (list :id "m-4Tq9" :title "The bridge library is named per platform" :status ':stale
              :summary "Makefile changed since this was written"
              :content "`liblightfast.so` on Linux and `liblightfast.dylib` on macOS; the name comes from `LIGHTFAST_LIBRARY` when that is set."
              :scope ':workspace :provenance "Makefile" :date "2026-11-13")
        (list :id "m-8Wc2" :title "Commits are tiny and title-only" :status ':current :summary nil
              :content "One purpose a commit, an imperative title under 72 characters, no body and no trailers. Push after each."
              :scope ':global :provenance "you said so" :date "2026-11-02")
        (list :id "m-1Zb5" :title "FLTK binds Command plus and minus to scaling" :status ':unverified
              :summary "its source could not be checked"
              :content "On macOS the window scales on Cmd +, Cmd - and Cmd 0 before the application sees the key."
              :scope ':workspace :provenance "fltk.org" :date "2026-11-14")))

(-> demo-install (workbench) null)
(defun demo-install (workbench)
  "Fill WORKBENCH with the demonstration, which has a conversation, a channel tree and a working agent."
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
                                                                      "contextLimit" 272000)
                                             "goal"  (portable-object
                                                      "objective" "Lightfast builds, passes its checks and draws a window on macOS."
                                                      "status"    "active"
                                                      "kind"      "goal"))
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
  "Return an approval request in the shape used by channels."
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
