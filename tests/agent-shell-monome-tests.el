;;; agent-shell-monome-tests.el --- Tests for agent-shell-monome -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs -Q --batch -L .. -L . -l agent-shell-monome-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-shell-monome)

(ert-deftest agent-shell-monome--pack-int32-roundtrip ()
  (dolist (n '(0 1 7770 12002 65535 16777216 2147483647 -1 -2147483648))
    (let ((packed (agent-shell-monome--pack-int32 n)))
      (should (= 4 (length packed)))
      (should (equal n (car (agent-shell-monome--read-int32 packed 0)))))))

(ert-deftest agent-shell-monome--pack-string-padding ()
  ;; OSC strings are null-terminated and padded to a multiple of 4 bytes.
  (should (= 8 (length (agent-shell-monome--pack-string "/foo"))))    ; 4 + 4 nulls
  (should (= 4 (length (agent-shell-monome--pack-string ",is"))))     ; 3 + 1 null
  (should (= 8 (length (agent-shell-monome--pack-string ",iis"))))    ; 4 + 4 nulls
  (should (= 4 (length (agent-shell-monome--pack-string ""))))        ; 0 + 4 nulls
  ;; First null appears immediately after the payload.
  (let ((packed (agent-shell-monome--pack-string "/foo")))
    (should (= 0 (aref packed 4)))))

(ert-deftest agent-shell-monome--encode-decode-roundtrip ()
  (let* ((encoded (agent-shell-monome--encode-message
                   "/serialosc/list" '((s . "127.0.0.1") (i . 7770))))
         (decoded (agent-shell-monome--decode-message encoded)))
    (should (equal "/serialosc/list" (map-elt decoded :address)))
    (should (equal '("127.0.0.1" 7770) (map-elt decoded :args)))))

(ert-deftest agent-shell-monome--decode-device-reply ()
  ;; Build what serialosc would send back, then decode it.
  (let* ((encoded (agent-shell-monome--encode-message
                   "/serialosc/device"
                   '((s . "m0000001") (s . "monome 64") (i . 7100))))
         (decoded (agent-shell-monome--decode-message encoded)))
    (should (equal "/serialosc/device" (map-elt decoded :address)))
    (should (equal '("m0000001" "monome 64" 7100) (map-elt decoded :args)))))

(ert-deftest agent-shell-monome--decode-grid-key ()
  ;; Synthesize the key-down message a device would send.
  (let* ((encoded (agent-shell-monome--encode-message
                   "/monome/grid/key"
                   '((i . 3) (i . 5) (i . 1))))
         (decoded (agent-shell-monome--decode-message encoded)))
    (should (equal "/monome/grid/key" (map-elt decoded :address)))
    (should (equal '(3 5 1) (map-elt decoded :args)))))

(ert-deftest agent-shell-monome--decode-rejects-non-osc ()
  (should-not (agent-shell-monome--decode-message "garbage"))
  (should-not (agent-shell-monome--decode-message "")))

(ert-deftest agent-shell-monome--coord-slot-roundtrip ()
  (let ((agent-shell-monome--state '((:grid-width . 8) (:grid-height . 8))))
    (dotimes (slot 64)
      (should (= slot (agent-shell-monome--slot-for-coord
                       (agent-shell-monome--coord-for-slot slot)))))))

(ert-deftest agent-shell-monome--classify-device ()
  (should (eq 'grid (agent-shell-monome--classify-device "monome 64")))
  (should (eq 'grid (agent-shell-monome--classify-device "monome 128 grid")))
  ;; A real grid here reports the word-typed name "monome one" -- it has
  ;; no digits, so the old "monome [0-9]+" rule dropped it on the floor.
  (should (eq 'grid (agent-shell-monome--classify-device "monome one")))
  (should (eq 'grid (agent-shell-monome--classify-device "Monome One")))
  (should (eq 'arc (agent-shell-monome--classify-device "monome arc 4")))
  (should (eq 'arc (agent-shell-monome--classify-device "monome arc")))
  (should (eq 'arc (agent-shell-monome--classify-device "arc")))
  (should-not (agent-shell-monome--classify-device "midi controller"))
  (should-not (agent-shell-monome--classify-device nil)))

(ert-deftest agent-shell-monome--decode-enc-delta ()
  (let* ((encoded (agent-shell-monome--encode-message
                   "/monome-arc/enc/delta"
                   '((i . 1) (i . -3))))
         (decoded (agent-shell-monome--decode-message encoded)))
    (should (equal "/monome-arc/enc/delta" (alist-get :address decoded)))
    (should (equal '(1 -3) (alist-get :args decoded)))))

(ert-deftest agent-shell-monome--selector-snap ()
  ;; With 4 buffers and 4 ticks/step, +4 should advance by one and
  ;; wrap mod 4.  Use sentinel "buffers" since the function only
  ;; counts them.
  (cl-letf* ((buffers '(a b c d))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
    (let ((agent-shell-monome--state
           (list (cons :selected-index 0)
                 (cons :selector-accumulator 0)))
          (agent-shell-monome-arc-selector-ticks-per-step 4))
      (agent-shell-monome--selector-on-delta 4)
      (should (= 1 (alist-get :selected-index agent-shell-monome--state)))
      (agent-shell-monome--selector-on-delta 4)
      (agent-shell-monome--selector-on-delta 4)
      (agent-shell-monome--selector-on-delta 4)
      (should (= 0 (alist-get :selected-index agent-shell-monome--state)))
      (agent-shell-monome--selector-on-delta -4)
      (should (= 3 (alist-get :selected-index agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--selector-snap-sub-step-does-nothing ()
  (cl-letf* ((buffers '(a b c d))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
    (let ((agent-shell-monome--state
           (list (cons :selected-index 0)
                 (cons :selector-accumulator 0)))
          (agent-shell-monome-arc-selector-ticks-per-step 4))
      (agent-shell-monome--selector-on-delta 3)
      (should (= 0 (alist-get :selected-index agent-shell-monome--state)))
      (should (= 3 (alist-get :selector-accumulator agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--selector-shows-buffer-on-change ()
  ;; Crossing a step boundary displays the newly selected buffer;
  ;; sub-step motion that leaves the selection unchanged does not.
  (cl-letf* ((buffers '(a b c d))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             (shown 'unset)
             ((symbol-function 'pop-to-buffer)
              (lambda (buf &rest _) (setq shown buf))))
    (let ((agent-shell-monome--state
           (list (cons :selected-index 0)
                 (cons :selector-accumulator 0)))
          (agent-shell-monome-arc-selector-ticks-per-step 4))
      ;; Sub-step: selection stays on buffer a, nothing displayed.
      (agent-shell-monome--selector-on-delta 3)
      (should (eq 'unset shown))
      ;; Completes a step (3 + 1): advance to index 1 -> display buffer b.
      (agent-shell-monome--selector-on-delta 1)
      (should (eq 'b shown)))))

(ert-deftest agent-shell-monome--decision-dead-zone ()
  ;; Sub-threshold motion accumulates without firing.
  (let ((agent-shell-monome--state
         (list (cons :decision-accumulator 0)
               (cons :pending-permissions nil)))
        (agent-shell-monome-arc-decision-threshold 40)
        (fired nil))
    (cl-letf (((symbol-function 'agent-shell-monome--decide)
               (lambda (choice) (setq fired choice))))
      (agent-shell-monome--decision-on-delta 10)
      (agent-shell-monome--decision-on-delta 20)
      (should-not fired)
      (agent-shell-monome--decision-on-delta 15)  ; total 45 > 40
      (should (eq 'allow fired)))))

(ert-deftest agent-shell-monome--decision-reject ()
  (let ((agent-shell-monome--state
         (list (cons :decision-accumulator 0)
               (cons :pending-permissions nil)))
        (agent-shell-monome-arc-decision-threshold 40)
        (fired nil))
    (cl-letf (((symbol-function 'agent-shell-monome--decide)
               (lambda (choice) (setq fired choice))))
      (agent-shell-monome--decision-on-delta -50)
      (should (eq 'reject fired)))))

(ert-deftest agent-shell-monome--permissions-answered-oldest-first ()
  ;; Two prompts arriving back-to-back both queue up, and ring 3 answers
  ;; them in arrival order (oldest first) -- the second no longer steals
  ;; the dial from the first.
  (let ((agent-shell-monome--state
         (list (cons :pending-permissions nil)
               (cons :saved-responder nil)))
        (fired nil))
    (cl-flet ((perm (n)
                (list (cons :options
                            (list (list (cons :kind "allow_once")
                                        (cons :option-id (format "allow-%d" n)))
                                  (list (cons :kind "reject_once")
                                        (cons :option-id (format "reject-%d" n)))))
                      (cons :tool-call (format "tool-%d" n))
                      (cons :respond (lambda (id) (push (cons n id) fired))))))
      (agent-shell-monome--responder (perm 1))
      (agent-shell-monome--responder (perm 2))
      ;; Both queued, oldest (tool-1) at the head.
      (should (= 2 (length (alist-get :pending-permissions
                                      agent-shell-monome--state))))
      (should (equal "tool-1"
                     (alist-get :tool-call
                                (car (alist-get :pending-permissions
                                                agent-shell-monome--state)))))
      ;; Allowing answers the oldest prompt with its allow option.
      (agent-shell-monome--decide 'allow)
      (should (equal '(1 . "allow-1") (car fired)))
      (should (= 1 (length (alist-get :pending-permissions
                                      agent-shell-monome--state))))
      ;; The next decision answers the second prompt, then the queue drains.
      (agent-shell-monome--decide 'reject)
      (should (equal '(2 . "reject-2") (car fired)))
      (should-not (alist-get :pending-permissions agent-shell-monome--state))
      ;; A decision with nothing queued is a harmless no-op.
      (agent-shell-monome--decide 'allow)
      (should (= 2 (length fired))))))

(ert-deftest agent-shell-monome--project-column-grouping ()
  ;; Two buffers in project A and one in project B should land in two
  ;; columns: A's two stack vertically, B's takes the next free column.
  (let* ((projects '((a . "/proj/a") (b . "/proj/a") (c . "/proj/b")))
         (agent-shell-monome--state
          (list (cons :bindings nil)
                (cons :project-columns nil)
                (cons :grid-width 8)
                (cons :grid-height 8))))
    (cl-letf (((symbol-function 'agent-shell-buffers)
               (lambda () '(a b c)))
              ((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'agent-shell-monome--project-for-buffer)
               (lambda (buf) (alist-get buf projects))))
      (agent-shell-monome--assign-new-buffers)
      (let ((bindings (alist-get :bindings agent-shell-monome--state)))
        ;; a -> col 0 row 0
        (should (equal '(0 . 0) (car (rassq 'a bindings))))
        ;; b -> col 0 row 1 (same project, stacked)
        (should (equal '(0 . 1) (car (rassq 'b bindings))))
        ;; c -> col 1 row 0 (new project, new column)
        (should (equal '(1 . 0) (car (rassq 'c bindings))))))))

(ert-deftest agent-shell-monome--project-column-stable-on-new-buffer ()
  ;; Adding a third buffer to project A should land in col 0 row 2,
  ;; not disturb existing placements.
  (let* ((projects '((a . "/proj/a") (b . "/proj/a") (c . "/proj/b")
                     (d . "/proj/a")))
         (agent-shell-monome--state
          (list (cons :bindings nil)
                (cons :project-columns nil)
                (cons :grid-width 8)
                (cons :grid-height 8))))
    (cl-letf (((symbol-function 'agent-shell-buffers)
               (lambda () '(a b c)))
              ((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'agent-shell-monome--project-for-buffer)
               (lambda (buf) (alist-get buf projects))))
      (agent-shell-monome--assign-new-buffers)
      ;; Now add d.
      (cl-letf (((symbol-function 'agent-shell-buffers)
                 (lambda () '(a b c d))))
        (agent-shell-monome--assign-new-buffers)
        (let ((bindings (alist-get :bindings agent-shell-monome--state)))
          (should (equal '(0 . 0) (car (rassq 'a bindings))))
          (should (equal '(0 . 1) (car (rassq 'b bindings))))
          (should (equal '(1 . 0) (car (rassq 'c bindings))))
          (should (equal '(0 . 2) (car (rassq 'd bindings)))))))))

(ert-deftest agent-shell-monome--tokens-rate ()
  ;; History entries within the window contribute; older are pruned.
  (let* ((now (float-time))
         (agent-shell-monome--state
          (list (cons :tokens-history
                      (list (cons now 100)
                            (cons (- now 5) 50)
                            (cons (- now 1000) 9999)))))
         (agent-shell-monome-arc-tokens-window-seconds 60.0))
    (agent-shell-monome--prune-tokens-history)
    ;; 150 tokens over 60 sec window = 2.5/sec
    (should (< (abs (- 2.5 (agent-shell-monome--tokens-rate))) 0.01))))

(ert-deftest agent-shell-monome--ring-map-message-shape ()
  ;; A ring is drawn with one /ring/map carrying the ring number plus 64
  ;; clamped levels -- not 64 separate /ring/set packets that overflow the
  ;; arc's USB write path and leave later rings half-lit.
  (let ((agent-shell-monome--state
         (list (cons :arc-prefix "/monome-arc")
               (cons :last-ring-maps nil)))
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-arc)
               (lambda (address args) (push (cons address args) sent))))
      (let ((leds (make-vector 64 0)))
        (aset leds 0 15)
        (aset leds 63 99)               ; out of range -> clamps to 15
        (agent-shell-monome--set-ring-map 2 leds)
        (should (= 1 (length sent)))
        (let* ((msg (car sent))
               (args (cdr msg)))
          (should (equal "/monome-arc/ring/map" (car msg)))
          (should (= 65 (length args)))         ; ring number + 64 levels
          (should (equal '(i . 2) (nth 0 args))) ; ring number
          (should (equal '(i . 15) (nth 1 args))) ; led 0
          (should (equal '(i . 0) (nth 2 args)))  ; led 1
          (should (equal '(i . 15) (nth 64 args)))) ; led 63, clamped
        ;; Identical content is deduped -- no resend.
        (agent-shell-monome--set-ring-map 2 leds)
        (should (= 1 (length sent)))
        ;; A change resends the whole ring.
        (aset leds 1 7)
        (agent-shell-monome--set-ring-map 2 leds)
        (should (= 2 (length sent)))))))

(ert-deftest agent-shell-monome--start-serialosc-disabled ()
  ;; With management off, nothing is launched and no process is stored.
  (let ((agent-shell-monome--state (list (cons :serialosc-process nil)))
        (agent-shell-monome-manage-serialosc nil))
    (should-not (agent-shell-monome--start-serialosc))
    (should-not (alist-get :serialosc-process agent-shell-monome--state))))

(ert-deftest agent-shell-monome--start-serialosc-missing-executable ()
  ;; When the executable is not found we fall back to an external daemon
  ;; rather than erroring or recording a process.
  (let ((agent-shell-monome--state (list (cons :serialosc-process nil)))
        (agent-shell-monome-manage-serialosc t))
    (cl-letf (((symbol-function 'agent-shell-monome--serialosc-port-in-use-p)
               (lambda () nil))
              ((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should-not (agent-shell-monome--start-serialosc))
      (should-not (alist-get :serialosc-process agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--serialosc-port-in-use-when-free ()
  ;; A bindable local port reports not-in-use (the probe binds and frees it).
  (let ((agent-shell-monome-serialosc-host "127.0.0.1")
        (agent-shell-monome-serialosc-port 17795))
    (should-not (agent-shell-monome--serialosc-port-in-use-p))))

(ert-deftest agent-shell-monome--serialosc-port-in-use-when-bind-fails ()
  ;; Any bind failure (here: an address on no local interface) is treated
  ;; as "in use", so the bridge never launches a doomed second daemon.
  (let ((agent-shell-monome-serialosc-host "203.0.113.1") ; TEST-NET-3, RFC 5737
        (agent-shell-monome-serialosc-port 17795))
    (should (agent-shell-monome--serialosc-port-in-use-p))))

(ert-deftest agent-shell-monome--start-serialosc-adopts-running ()
  ;; When a serialosc already holds the discovery port, adopt it: do not
  ;; launch (even with the executable present) and record no process.
  (let ((agent-shell-monome--state (list (cons :serialosc-process nil)))
        (agent-shell-monome-manage-serialosc t))
    (cl-letf (((symbol-function 'agent-shell-monome--serialosc-port-in-use-p)
               (lambda () t))
              ((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/serialoscd"))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "must not launch a competing serialosc"))))
      (should-not (agent-shell-monome--start-serialosc))
      (should-not (alist-get :serialosc-process agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--stop-serialosc-no-process ()
  ;; Stopping when we never started one is a no-op.
  (let ((agent-shell-monome--state (list (cons :serialosc-process nil))))
    (agent-shell-monome--stop-serialosc)
    (should-not (alist-get :serialosc-process agent-shell-monome--state))))

;;;; Hold-to-talk (voice input)

(ert-deftest agent-shell-monome--hold-tap-switches-without-recording ()
  ;; A quick press/release on a bound key keeps its old meaning: switch to
  ;; the buffer (on press) and cancel the armed hold timer (on release),
  ;; without ever starting a recording.
  (let ((agent-shell-monome--state
         (list (cons :bindings (list (cons (cons 1 2) 'buf)))
               (cons :htt-down-coord nil)
               (cons :htt-timer nil)
               (cons :htt-recording nil)))
        (agent-shell-monome-hold-to-talk t)
        (popped nil)
        (cancelled nil))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq popped b)))
              ((symbol-function 'whisper-run) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) (lambda (tm) (setq cancelled tm))))
      ;; Press: switches and arms the hold timer.
      (agent-shell-monome--on-grid-key 1 2 1)
      (should (eq 'buf popped))
      (should (equal '(1 . 2) (alist-get :htt-down-coord agent-shell-monome--state)))
      (should (eq 'fake-timer (alist-get :htt-timer agent-shell-monome--state)))
      ;; Quick release: a tap -- timer cancelled, nothing recorded.
      (agent-shell-monome--on-grid-key 1 2 0)
      (should (eq 'fake-timer cancelled))
      (should-not (alist-get :htt-timer agent-shell-monome--state))
      (should-not (alist-get :htt-down-coord agent-shell-monome--state))
      (should-not (alist-get :htt-recording agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--hold-records-then-stops ()
  ;; Held past the threshold the timer fires and recording starts; the
  ;; release toggles whisper back off to transcribe.
  (let ((agent-shell-monome--state
         (list (cons :bindings (list (cons (cons 0 0) 'buf)))
               (cons :htt-down-coord nil)
               (cons :htt-timer nil)
               (cons :htt-recording nil)
               (cons :htt-target nil)))
        (agent-shell-monome-hold-to-talk t)
        (recording nil)                 ; whisper's recording state
        (runs 0))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) (lambda (_) nil))
              ((symbol-function 'whisper-run) (lambda (&rest _) (setq runs (1+ runs))))
              ((symbol-function 'whisper-recording-p) (lambda () recording))
              ((symbol-function 'whisper-transcribing-p) (lambda () nil)))
      ;; Press arms the timer.
      (agent-shell-monome--on-grid-key 0 0 1)
      (should (eq 'fake-timer (alist-get :htt-timer agent-shell-monome--state)))
      ;; Timer fires (whisper not yet recording): recording begins.
      (agent-shell-monome--htt-begin 'buf)
      (should (eq 'buf (alist-get :htt-recording agent-shell-monome--state)))
      (should (eq 'buf (alist-get :htt-target agent-shell-monome--state)))
      (should-not (alist-get :htt-timer agent-shell-monome--state))
      (should (= 1 runs))
      ;; Release while recording toggles whisper off and clears state.
      (setq recording t)
      (agent-shell-monome--on-grid-key 0 0 0)
      (should (= 2 runs))
      (should-not (alist-get :htt-recording agent-shell-monome--state))
      (should-not (alist-get :htt-down-coord agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--hold-disabled-is-plain-switch ()
  ;; With the feature off, a press just switches buffers -- no timer is armed.
  (let ((agent-shell-monome--state
         (list (cons :bindings (list (cons (cons 0 0) 'buf)))
               (cons :htt-down-coord nil)
               (cons :htt-timer nil)
               (cons :htt-recording nil)))
        (agent-shell-monome-hold-to-talk nil)
        (popped nil))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq popped b)))
              ((symbol-function 'whisper-run) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (error "must not arm a hold timer when disabled"))))
      (agent-shell-monome--on-grid-key 0 0 1)
      (should (eq 'buf popped))
      (should-not (alist-get :htt-down-coord agent-shell-monome--state))
      (should-not (alist-get :htt-timer agent-shell-monome--state))
      ;; The matching release is a clean no-op.
      (agent-shell-monome--on-grid-key 0 0 0))))

(ert-deftest agent-shell-monome--transcription-inserts-at-target ()
  ;; The whisper hook inserts the (trimmed) transcription at the target
  ;; buffer's prompt and empties the stdout buffer so whisper itself
  ;; inserts nothing at point.
  (let ((target (generate-new-buffer " *htt-target*"))
        (stdout (generate-new-buffer " *htt-stdout*"))
        (agent-shell-monome-hold-to-talk-submit nil))
    (unwind-protect
        (let ((agent-shell-monome--state (list (cons :htt-target target))))
          (with-current-buffer target (insert "PROMPT> "))
          (with-current-buffer stdout
            (insert "  hello world  ")
            (agent-shell-monome--whisper-transcription-handler)
            (should (= 0 (buffer-size))))
          (should-not (alist-get :htt-target agent-shell-monome--state))
          (with-current-buffer target
            (should (equal "PROMPT> hello world" (buffer-string)))))
      (kill-buffer target)
      (kill-buffer stdout))))

(ert-deftest agent-shell-monome--transcription-without-target-is-noop ()
  ;; Ordinary `whisper-run' usage (no hold-to-talk target) is untouched:
  ;; the stdout buffer keeps its text for whisper's own insertion.
  (let ((stdout (generate-new-buffer " *htt-stdout*")))
    (unwind-protect
        (let ((agent-shell-monome--state (list (cons :htt-target nil))))
          (with-current-buffer stdout
            (insert "some transcription")
            (agent-shell-monome--whisper-transcription-handler)
            (should (equal "some transcription" (buffer-string)))))
      (kill-buffer stdout))))

;;;; Empty-press spawn roots at the pressed column's project

(ert-deftest agent-shell-monome--project-for-column-reverse-lookup ()
  ;; :project-columns maps project -> column; the reverse lookup recovers
  ;; the project that owns a given column, or nil for an unclaimed one.
  (let ((agent-shell-monome--state
         (list (cons :project-columns '(("/proj/a" . 0) ("/proj/b" . 1))))))
    (should (equal "/proj/a" (agent-shell-monome--project-for-column 0)))
    (should (equal "/proj/b" (agent-shell-monome--project-for-column 1)))
    (should-not (agent-shell-monome--project-for-column 2))))

(ert-deftest agent-shell-monome--empty-press-spawns-into-pressed-column ()
  ;; Pressing an unlit key in a column owned by project B spawns a shell
  ;; rooted at B -- not at whatever buffer Emacs happens to have focused.
  (let ((agent-shell-monome--state
         (list (cons :bindings nil)
               (cons :project-columns '(("/proj/a" . 0) ("/proj/b" . 1)))))
        (agent-shell-monome-spawn-on-empty-press t)
        (spawned-at 'unset))
    (cl-letf (((symbol-function 'agent-shell--new-shell)
               (lambda (&rest args) (setq spawned-at (plist-get args :location))))
              ;; Focus resolves to project A; the press must ignore it.
              ((symbol-function 'agent-shell-monome--current-project-root)
               (lambda () "/proj/a")))
      ;; Empty key at column 1 (project B's column), any row.
      (agent-shell-monome--on-grid-key-down 1 4)
      (should (equal "/proj/b" spawned-at)))))

(ert-deftest agent-shell-monome--empty-press-unclaimed-column-falls-back ()
  ;; An empty press in a column no project owns yet falls back to the
  ;; selected buffer's project.
  (let ((agent-shell-monome--state
         (list (cons :bindings nil)
               (cons :project-columns '(("/proj/a" . 0)))))
        (agent-shell-monome-spawn-on-empty-press t)
        (spawned-at 'unset))
    (cl-letf (((symbol-function 'agent-shell--new-shell)
               (lambda (&rest args) (setq spawned-at (plist-get args :location))))
              ((symbol-function 'agent-shell-monome--current-project-root)
               (lambda () "/fallback")))
      (agent-shell-monome--on-grid-key-down 5 0)
      (should (equal "/fallback" spawned-at)))))

;;;; Selected buffer follows the focused agent-shell window

(ert-deftest agent-shell-monome--selected-buffer-follows-focus ()
  ;; When the selected window shows an agent-shell buffer, that is the
  ;; selection and :selected-index is synced to it -- so a grid tap (which
  ;; never moves the ring-1 dial) still steers the arc's scroll/effort.
  (cl-letf* ((buffers '(a b c))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'selected-window) (lambda () 'win))
             ((symbol-function 'window-buffer) (lambda (_) 'b)))
    (let ((agent-shell-monome--state (list (cons :selected-index 0))))
      (should (eq 'b (agent-shell-monome--selected-buffer)))
      (should (= 1 (alist-get :selected-index agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--selected-buffer-falls-back-off-shell ()
  ;; When focus is not on an agent-shell buffer, fall back to the dial
  ;; position and leave :selected-index untouched.
  (cl-letf* ((buffers '(a b c))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'selected-window) (lambda () 'win))
             ((symbol-function 'window-buffer) (lambda (_) 'not-a-shell)))
    (let ((agent-shell-monome--state (list (cons :selected-index 2))))
      (should (eq 'c (agent-shell-monome--selected-buffer)))
      (should (= 2 (alist-get :selected-index agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--indexed-buffer-ignores-focus ()
  ;; The dial accessor reflects :selected-index regardless of focus, so the
  ;; selector can advance without snapping back to the focused window.
  (cl-letf* ((buffers '(a b c))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'selected-window) (lambda () 'win))
             ((symbol-function 'window-buffer) (lambda (_) 'a)))
    (let ((agent-shell-monome--state (list (cons :selected-index 2))))
      (should (eq 'c (agent-shell-monome--indexed-buffer))))))

(ert-deftest agent-shell-monome--selector-advances-despite-focus ()
  ;; Regression: with the focused window showing buffer a, turning ring 1
  ;; one step still advances the dial to b and displays b -- the
  ;; focus-following selection must not drag the dial back to a.
  (cl-letf* ((buffers '(a b c d))
             ((symbol-function 'agent-shell-buffers) (lambda () buffers))
             ((symbol-function 'buffer-live-p) (lambda (_) t))
             ((symbol-function 'selected-window) (lambda () 'win))
             ((symbol-function 'window-buffer) (lambda (_) 'a)) ; focus on a
             (shown 'unset)
             ((symbol-function 'pop-to-buffer)
              (lambda (buf &rest _) (setq shown buf))))
    (let ((agent-shell-monome--state
           (list (cons :selected-index 0)
                 (cons :selector-accumulator 0)))
          (agent-shell-monome-arc-selector-ticks-per-step 4))
      (agent-shell-monome--selector-on-delta 4)
      (should (= 1 (alist-get :selected-index agent-shell-monome--state)))
      (should (eq 'b shown)))))

;;;; Scroll targets the focused window, not a background copy

(ert-deftest agent-shell-monome--scroll-target-prefers-selected-window ()
  ;; When the selected window is the one showing the buffer, scroll there
  ;; rather than letting get-buffer-window pick a stray background copy.
  (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
            ((symbol-function 'selected-window) (lambda () 'sel-win))
            ((symbol-function 'window-buffer) (lambda (_) 'buf))
            ((symbol-function 'get-buffer-window)
             (lambda (&rest _) (error "must not reach for a background window"))))
    (should (eq 'sel-win (agent-shell-monome--scroll-target-window 'buf)))))

(ert-deftest agent-shell-monome--scroll-target-other-window-when-unfocused ()
  ;; If the buffer is not in the selected window, fall back to any window
  ;; showing it.
  (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
            ((symbol-function 'selected-window) (lambda () 'sel-win))
            ((symbol-function 'window-buffer) (lambda (_) 'other-buf))
            ((symbol-function 'get-buffer-window) (lambda (&rest _) 'bg-win)))
    (should (eq 'bg-win (agent-shell-monome--scroll-target-window 'buf)))))

;;;; Ring 4 token-rate spinner

(ert-deftest agent-shell-monome--spinner-parked-when-idle ()
  ;; Zero saturation (no tokens) leaves the spinner head where it is.
  (let ((agent-shell-monome--state (list (cons :tokens-spinner-phase 12.0)))
        (agent-shell-monome-arc-tokens-spinner-max-rps 1.0)
        (agent-shell-monome-tick-seconds 0.1))
    (should (= 12.0 (agent-shell-monome--advance-spinner-phase 0.0)))
    (should (= 12.0 (agent-shell-monome--advance-spinner-phase 0.0)))))

(ert-deftest agent-shell-monome--spinner-speed-scales-and-wraps ()
  ;; Advance per tick is max-rps * 64 * tick-seconds * saturation, kept as a
  ;; float phase in [0, 64).  Here: 1 rev/s * 64 * 0.25s = 16 LEDs/tick at
  ;; full saturation.
  (let ((agent-shell-monome--state (list (cons :tokens-spinner-phase 0.0)))
        (agent-shell-monome-arc-tokens-spinner-max-rps 1.0)
        (agent-shell-monome-tick-seconds 0.25))
    (should (= 16.0 (agent-shell-monome--advance-spinner-phase 1.0)))
    (should (= 32.0 (agent-shell-monome--advance-spinner-phase 1.0)))
    (should (= 48.0 (agent-shell-monome--advance-spinner-phase 1.0)))
    ;; 48 + 16 = 64 wraps back to 0.
    (should (= 0.0 (agent-shell-monome--advance-spinner-phase 1.0)))
    ;; Half the rate advances half as fast.
    (should (= 8.0 (agent-shell-monome--advance-spinner-phase 0.5)))))

(provide 'agent-shell-monome-tests)
;;; agent-shell-monome-tests.el ends here
