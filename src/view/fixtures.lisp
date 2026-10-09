(in-package #:aui)

;;;; -- Visual Fixtures --

;;; Sample content for inspecting the views by eye. Not used by the
;;; application; script/visual-* run these.

(-> sample-transcript () transcript)
(defun sample-transcript ()
  "Return a transcript with one of every block kind, in states worth looking at."
  (let ((transcript (make-transcript)))
    (transcript-append transcript
                       (make-instance 'user-message
                                      :text "Please give lightfast a flat theme so the workbench can look like paper. Keep the classic theme for the old applications."))
    (transcript-append transcript
                       (make-instance 'reasoning
                                      :text (format nil "The user wants a theme layer rather than per-widget patches.~%I should look at where the literals live first.")))
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_a" :name "search.grep"
                                                 :title "search.grep · textsize(12)" :kind ':search
                                                 :status ':completed :input "{\"patterns\": [\"textsize(12)\"]}"
                                                 :output (format nil "native/cl_fltk_bridge_widgets.cpp: 26 matches~%native/cl_fltk_bridge_style.cpp: 2 matches")))
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_b" :name "shell.run"
                                                 :title "shell.run · make native && make smoke" :kind ':execute
                                                 :status ':running :input "make native && make smoke"))
    (transcript-append transcript
                       (make-instance 'assistant-text
                                      :text (format nil "I added `apply-flat-theme` beside the classic theme. The **Theme** struct carries every default, so widgets created later follow it:~%~%```lisp~%(lightfast:apply-flat-theme :background '(255 254 250)~%                            :label-font serif~%                            :mono-font mono)~%```~%~%Two things still need your eye:~%~%- the scrollbar slider is black on paper, which may be heavy~%- tabs show a doubled edge~%~%See [lightfast/README.md](file:///root/common-lisp/lightfast/README.md) for the usage note.")))
    (transcript-replace-plan transcript '(("Find the hardcoded style literals" . :completed)
                                          ("Route them through a theme struct" . :completed)
                                          ("Add the flat theme entry point" . :in-progress)
                                          ("Inspect the gallery at three widths" . :pending)))
    (transcript-append transcript
                       (make-instance 'approval :request-id "r1" :title "shell.run"
                                                :arguments "git push origin master"
                                                :directory "/root/common-lisp/lightfast"
                                                :options '(("once" "Allow once" :allow-once)
                                                           ("session" "Allow exact operation this session" :allow-always)
                                                           ("deny" "Reject" :reject-once))))
    (transcript-notice transcript :severity ':warning :text "Context is 72 percent full; compaction will run after this turn."
                                  :key ':context)
    (transcript-append transcript
                       (make-instance 'tool-call :call-id "1:call_c" :name "resource.edit"
                                                 :title "resource.edit · src/theme.lisp" :kind ':edit
                                                 :status ':failed :input "{\"uri\": \"workspace:src/theme.lisp\"}"
                                                 :output "Revision mismatch: the file changed since it was read."))
    transcript))

(-> visual-transcript (&optional t) null)
(defun visual-transcript (&optional shot-p)
  "Show SAMPLE-TRANSCRIPT in a window; with SHOT-P, capture the screen and exit."
  (lightfast:load-library)
  (theme-install)
  (let* ((window (lightfast:make-window :width 900 :height 760 :label "AUI transcript"
                                        :app-id "aui"))
         (transcript (sample-transcript))
         (view (make-transcript-view transcript :parent window :x 24 :y 16 :width 852 :height 728)))
    (lightfast:layout-on-resize
     window
     (lightfast:make-layout-column
      :padding (list (spacing 5) (spacing 4) (spacing 5) (spacing 4))
      :children (list (lightfast:make-layout-item (transcript-view-display view) :grow 1 :basis 0))))
    (lightfast:show window)
    (cond
      (shot-p
       (loop repeat 40 do (lightfast:wait 0.05))
       (uiop:run-program '("grim" "/tmp/aui-transcript.png") :ignore-error-status t)
       (lightfast:destroy window))
      (t
       (lightfast:run))))
  nil)
