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
  ;; Two prompts for the *selected* buffer queue up in arrival order and
  ;; ring 3 answers them oldest-first; a prompt from a different buffer
  ;; is invisible to the dial so it can never be answered by accident.
  (let ((agent-shell-monome--state
         (list (cons :pending-permissions nil)
               (cons :saved-responder nil)))
        (fired nil))
    (cl-letf* (((symbol-function 'agent-shell-monome--selected-buffer)
                (lambda () 'selected-buf))
               ;; Route every incoming prompt to 'selected-buf except one
               ;; tagged for 'other-buf, so we can prove foreign prompts
               ;; stay untouched.
               ((symbol-function 'agent-shell-monome--owner-buffer-for-request)
                (lambda (id) (if (equal id "req-foreign") 'other-buf 'selected-buf))))
      (cl-flet ((perm (n &optional foreign)
                  (list (cons :options
                              (list (list (cons :kind "allow_once")
                                          (cons :option-id (format "allow-%d" n)))
                                    (list (cons :kind "reject_once")
                                          (cons :option-id (format "reject-%d" n)))))
                        (cons :tool-call
                              (list (cons :permission-request-id
                                          (if foreign "req-foreign"
                                            (format "req-%d" n)))))
                        (cons :respond (lambda (id) (push (cons n id) fired))))))
        (agent-shell-monome--responder (perm 1))
        (agent-shell-monome--responder (perm 99 'foreign)) ;; other-buf
        (agent-shell-monome--responder (perm 2))
        ;; All three sit on the global queue; only two are for the selected buf.
        (should (= 3 (length (alist-get :pending-permissions
                                        agent-shell-monome--state))))
        (should (= 2 (length (agent-shell-monome--pending-for-selected))))
        ;; Allowing answers the selected buffer's oldest prompt.
        (agent-shell-monome--decide 'allow)
        (should (equal '(1 . "allow-1") (car fired)))
        ;; The other-buf prompt was left in place, so 2 remain: one for
        ;; other-buf and one still for selected-buf.
        (should (= 2 (length (alist-get :pending-permissions
                                        agent-shell-monome--state))))
        (agent-shell-monome--decide 'reject)
        (should (equal '(2 . "reject-2") (car fired)))
        ;; Only the foreign prompt is left; ring 3 must not touch it.
        (should (= 1 (length (alist-get :pending-permissions
                                        agent-shell-monome--state))))
        (should-not (agent-shell-monome--pending-for-selected))
        ;; A decision with nothing eligible is a harmless no-op.
        (agent-shell-monome--decide 'allow)
        (should (= 2 (length fired)))))))

(ert-deftest agent-shell-monome--decide-noop-when-no-selected-buffer ()
  ;; With no selected shell (empty picker) ring 3 must not fire even if
  ;; prompts are queued -- otherwise a random dial nudge could answer a
  ;; prompt in a buffer the user cannot currently see.
  (let ((agent-shell-monome--state
         (list (cons :pending-permissions
                     (list (list (cons :respond (lambda (_) (error "must not fire")))
                                 (cons :allow-id "a")
                                 (cons :reject-id "r")
                                 (cons :buffer 'buf)))))))
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () nil)))
      (agent-shell-monome--decide 'allow)
      (agent-shell-monome--decide 'reject)
      (should (= 1 (length (alist-get :pending-permissions
                                      agent-shell-monome--state)))))))

(ert-deftest agent-shell-monome--prune-permissions-drops-dead-owners ()
  ;; Entries whose owner buffer is dead should be dropped so the ring-3
  ;; backlog count reflects reachable prompts only.
  (let* ((live (generate-new-buffer " *asm-live*"))
         (dead (generate-new-buffer " *asm-dead*"))
         (agent-shell-monome--state
          (list (cons :pending-permissions
                      (list (list (cons :buffer live))
                            (list (cons :buffer dead))
                            ;; Nil owner is tolerated: no lookup was
                            ;; possible at enqueue time, so leave it.
                            (list (cons :buffer nil)))))))
    (unwind-protect
        (progn
          (kill-buffer dead)
          (agent-shell-monome--prune-permissions)
          (should (= 2 (length (alist-get :pending-permissions
                                          agent-shell-monome--state)))))
      (when (buffer-live-p live) (kill-buffer live)))))

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

(ert-deftest agent-shell-monome--flywheel-spins-up-with-tokens ()
  ;; A burst kicks angular velocity up by its share of a full-speed burst,
  ;; successive bursts accumulate, and a burst past full speed saturates
  ;; rather than overspinning.
  (let ((agent-shell-monome--state (list (cons :tokens-spinner-velocity 0.0)))
        (agent-shell-monome-arc-tokens-spinner-max-rps 1.0)
        (agent-shell-monome-arc-tokens-spinner-spinup-tokens 1000.0))
    (agent-shell-monome--spin-up-flywheel 250)
    (should (< (abs (- 0.25 (alist-get :tokens-spinner-velocity
                                       agent-shell-monome--state)))
               0.001))
    (agent-shell-monome--spin-up-flywheel 250)
    (should (< (abs (- 0.5 (alist-get :tokens-spinner-velocity
                                      agent-shell-monome--state)))
               0.001))
    (agent-shell-monome--spin-up-flywheel 9000)
    (should (= 1.0 (alist-get :tokens-spinner-velocity
                              agent-shell-monome--state)))))

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

;;;; Grid tap-on-open sends ENTER

(ert-deftest agent-shell-monome--tap-on-open-buffer-sends-enter ()
  ;; Tapping the key of the buffer that is already open submits it (ENTER)
  ;; on release, rather than being a no-op re-switch.
  (let* ((buf (generate-new-buffer " *tap-enter*"))
         (agent-shell-monome--state
          (list (cons :bindings (list (cons (cons 2 3) buf)))
                (cons :tap-coord nil) (cons :tap-reopen nil)
                (cons :htt-down-coord nil) (cons :htt-timer nil)
                (cons :htt-recording nil)))
         (agent-shell-monome-tap-open-sends-enter t)
         (agent-shell-monome-hold-to-talk nil)
         (submitted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'selected-window) (lambda () 'win))
                  ((symbol-function 'window-buffer) (lambda (_) buf))
                  ((symbol-function 'shell-maker-submit)
                   (lambda (&rest _) (setq submitted (1+ submitted)))))
          (agent-shell-monome--on-grid-key 2 3 1) ; press
          (should (equal '(2 . 3) (alist-get :tap-coord agent-shell-monome--state)))
          (should (eq buf (alist-get :tap-reopen agent-shell-monome--state)))
          (should (= 0 submitted))      ; nothing on press
          (agent-shell-monome--on-grid-key 2 3 0) ; release -> submit
          (should (= 1 submitted))
          (should-not (alist-get :tap-coord agent-shell-monome--state))
          (should-not (alist-get :tap-reopen agent-shell-monome--state)))
      (kill-buffer buf))))

(ert-deftest agent-shell-monome--tap-on-unfocused-buffer-just-switches ()
  ;; Tapping a key whose buffer is NOT the open one only switches -- the
  ;; ENTER gesture is reserved for re-tapping the already-open buffer.
  (let* ((buf (generate-new-buffer " *tap-switch*"))
         (other (generate-new-buffer " *open-elsewhere*"))
         (agent-shell-monome--state
          (list (cons :bindings (list (cons (cons 0 0) buf)))
                (cons :tap-coord nil) (cons :tap-reopen nil)
                (cons :htt-down-coord nil) (cons :htt-timer nil)
                (cons :htt-recording nil)))
         (agent-shell-monome-tap-open-sends-enter t)
         (agent-shell-monome-hold-to-talk nil)
         (submitted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'selected-window) (lambda () 'win))
                  ((symbol-function 'window-buffer) (lambda (_) other))
                  ((symbol-function 'shell-maker-submit)
                   (lambda (&rest _) (setq submitted (1+ submitted)))))
          (agent-shell-monome--on-grid-key 0 0 1)
          (should-not (alist-get :tap-reopen agent-shell-monome--state))
          (agent-shell-monome--on-grid-key 0 0 0)
          (should (= 0 submitted)))
      (kill-buffer buf)
      (kill-buffer other))))

(ert-deftest agent-shell-monome--tap-open-disabled-sends-nothing ()
  ;; With the option off, re-tapping the open buffer stays a plain switch.
  (let* ((buf (generate-new-buffer " *tap-disabled*"))
         (agent-shell-monome--state
          (list (cons :bindings (list (cons (cons 0 0) buf)))
                (cons :tap-coord nil) (cons :tap-reopen nil)
                (cons :htt-down-coord nil) (cons :htt-timer nil)
                (cons :htt-recording nil)))
         (agent-shell-monome-tap-open-sends-enter nil)
         (agent-shell-monome-hold-to-talk nil)
         (submitted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'selected-window) (lambda () 'win))
                  ((symbol-function 'window-buffer) (lambda (_) buf))
                  ((symbol-function 'shell-maker-submit)
                   (lambda (&rest _) (setq submitted (1+ submitted)))))
          (agent-shell-monome--on-grid-key 0 0 1)
          (should-not (alist-get :tap-reopen agent-shell-monome--state))
          (agent-shell-monome--on-grid-key 0 0 0)
          (should (= 0 submitted)))
      (kill-buffer buf))))

(ert-deftest agent-shell-monome--hold-on-open-buffer-records-not-submits ()
  ;; Holding the open buffer's key records voice as usual; the release must
  ;; finish transcription and NOT also fire the tap-to-submit ENTER.
  (let* ((buf (generate-new-buffer " *tap-hold*"))
         (agent-shell-monome--state
          (list (cons :bindings (list (cons (cons 0 0) buf)))
                (cons :tap-coord nil) (cons :tap-reopen nil)
                (cons :htt-down-coord nil) (cons :htt-timer nil)
                (cons :htt-recording nil) (cons :htt-target nil)))
         (agent-shell-monome-tap-open-sends-enter t)
         (agent-shell-monome-hold-to-talk t)
         (recording nil)
         (submitted 0))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'selected-window) (lambda () 'win))
                  ((symbol-function 'window-buffer) (lambda (_) buf))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
                  ((symbol-function 'cancel-timer) (lambda (_) nil))
                  ((symbol-function 'whisper-run) (lambda (&rest _) nil))
                  ((symbol-function 'whisper-recording-p) (lambda () recording))
                  ((symbol-function 'whisper-transcribing-p) (lambda () nil))
                  ((symbol-function 'shell-maker-submit)
                   (lambda (&rest _) (setq submitted (1+ submitted)))))
          (agent-shell-monome--on-grid-key 0 0 1) ; press open buffer's key
          (should (eq buf (alist-get :tap-reopen agent-shell-monome--state)))
          (agent-shell-monome--htt-begin buf)     ; hold threshold passes
          (setq recording t)
          (agent-shell-monome--on-grid-key 0 0 0) ; release while recording
          (should (= 0 submitted)))               ; transcribed, not submitted
      (kill-buffer buf))))

;;;; Ring 4 token-momentum flywheel

(ert-deftest agent-shell-monome--flywheel-coasts-and-advances ()
  ;; With no fresh tokens the velocity halves every half-life of seconds and
  ;; the head phase advances by that velocity (rev/s * 64 * tick-seconds).
  (let ((agent-shell-monome--state (list (cons :tokens-spinner-velocity 1.0)
                                         (cons :tokens-spinner-phase 0.0)))
        (agent-shell-monome-arc-tokens-spinner-max-rps 1.0)
        (agent-shell-monome-arc-tokens-spinner-halflife 0.1)
        (agent-shell-monome-tick-seconds 0.1))
    ;; One tick == one half-life: velocity 1.0 -> 0.5.
    (should (< (abs (- 0.5 (agent-shell-monome--coast-flywheel))) 0.001))
    ;; Phase advanced by 0.5 rev * 64 * 0.1s = 3.2 LEDs.
    (should (< (abs (- 3.2 (alist-get :tokens-spinner-phase
                                      agent-shell-monome--state)))
               0.001))))

(ert-deftest agent-shell-monome--flywheel-parks-when-too-slow ()
  ;; Velocity too small to move the head half an LED in one tick (the floor
  ;; at tick 0.1s is ~0.078 rev/s) snaps to zero and the head holds still.
  (let ((agent-shell-monome--state (list (cons :tokens-spinner-velocity 0.05)
                                         (cons :tokens-spinner-phase 12.0)))
        (agent-shell-monome-arc-tokens-spinner-halflife 100.0)
        (agent-shell-monome-tick-seconds 0.1))
    (should (= 0.0 (agent-shell-monome--coast-flywheel)))
    (should (= 12.0 (alist-get :tokens-spinner-phase
                               agent-shell-monome--state)))))

;;;; Bottom-row delete hotkey

(ert-deftest agent-shell-monome--assign-skips-hotkey-row ()
  ;; The bottom row is reserved for hotkeys, so the assigner must never
  ;; place a buffer there even when it is the only row left in a column.
  (let* ((agent-shell-monome--state
          (list (cons :bindings nil)
                (cons :project-columns nil)
                (cons :grid-width 4)
                (cons :grid-height 4))))
    (cl-letf (((symbol-function 'agent-shell-buffers)
               ;; Five buffers all in the same project would want rows
               ;; 0..4 of column 0; with row 3 reserved only 3 fit.
               (lambda () '(a b c d e)))
              ((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'agent-shell-monome--project-for-buffer)
               (lambda (_) "/proj")))
      (agent-shell-monome--assign-new-buffers)
      (let ((bindings (alist-get :bindings agent-shell-monome--state)))
        (should (equal '(0 . 0) (car (rassq 'a bindings))))
        (should (equal '(0 . 1) (car (rassq 'b bindings))))
        (should (equal '(0 . 2) (car (rassq 'c bindings))))
        ;; Row 3 is the hotkey row; d and e must not land there.
        (should-not (rassq 'd bindings))
        (should-not (rassq 'e bindings))
        (should-not (seq-some (lambda (entry) (= 3 (cdr (car entry))))
                              bindings))))))

(ert-deftest agent-shell-monome--delete-key-coord-bottom-right ()
  ;; The delete hotkey is the bottom-right cell of the reported grid,
  ;; so its coordinate must track :grid-width/:grid-height rather than
  ;; being hard-coded to an 8x8.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 16) (cons :grid-height 8))))
    (should (equal (cons 15 7) (agent-shell-monome--delete-key-coord)))
    (should (agent-shell-monome--delete-key-p 15 7))
    (should-not (agent-shell-monome--delete-key-p 15 6))
    (should-not (agent-shell-monome--delete-key-p 14 7))
    (should (agent-shell-monome--hotkey-row-p 7))
    (should-not (agent-shell-monome--hotkey-row-p 6))))

(ert-deftest agent-shell-monome--delete-key-arm-and-disarm ()
  ;; Pressing the delete hotkey sets :delete-key-down; releasing clears
  ;; it -- and neither event should switch, spawn, or arm hold-to-talk.
  (let ((agent-shell-monome--state
         (list (cons :bindings nil)
               (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :delete-key-down nil)
               (cons :htt-down-coord nil)
               (cons :tap-coord nil)))
        (agent-shell-monome-hold-to-talk t)
        (agent-shell-monome-spawn-on-empty-press t))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (error "delete key must not switch")))
              ((symbol-function 'agent-shell--new-shell)
               (lambda (&rest _) (error "delete key must not spawn")))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (error "delete key must not arm hold"))))
      (agent-shell-monome--on-grid-key 3 3 1)
      (should (alist-get :delete-key-down agent-shell-monome--state))
      (should-not (alist-get :tap-coord agent-shell-monome--state))
      (should-not (alist-get :htt-down-coord agent-shell-monome--state))
      (agent-shell-monome--on-grid-key 3 3 0)
      (should-not (alist-get :delete-key-down agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--delete-plus-tap-kills-buffer ()
  ;; With delete armed, tapping a bound key kills that buffer -- not
  ;; switch, not spawn -- and does not leave a tap-coord/hold armed
  ;; that a later release would try to resolve.
  (let ((agent-shell-monome--state
         (list (cons :bindings (list (cons (cons 0 0) 'buf)))
               (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :delete-key-down t)
               (cons :htt-down-coord nil)
               (cons :tap-coord nil)))
        (agent-shell-monome-hold-to-talk t)
        (killed nil))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (error "delete gesture must not switch")))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (error "delete gesture must not arm hold")))
              ((symbol-function 'kill-buffer)
               (lambda (b) (setq killed b))))
      (agent-shell-monome--on-grid-key 0 0 1)
      (should (eq killed 'buf))
      (should-not (alist-get :tap-coord agent-shell-monome--state))
      (should-not (alist-get :htt-down-coord agent-shell-monome--state))
      ;; The release of the buffer key should be a clean no-op --
      ;; no tap/hold state was ever recorded.
      (agent-shell-monome--on-grid-key 0 0 0))))

(ert-deftest agent-shell-monome--delete-plus-empty-tap-does-nothing ()
  ;; With delete armed, tapping an *unbound* key must not spawn a shell.
  (let ((agent-shell-monome--state
         (list (cons :bindings nil)
               (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :delete-key-down t)))
        (agent-shell-monome-spawn-on-empty-press t))
    (cl-letf (((symbol-function 'agent-shell--new-shell)
               (lambda (&rest _) (error "delete gesture must not spawn"))))
      (agent-shell-monome--on-grid-key 1 1 1)
      (agent-shell-monome--on-grid-key 1 1 0))))

(ert-deftest agent-shell-monome--non-delete-hotkey-row-key-is-noop ()
  ;; Other bottom-row keys are reserved for future hotkeys, so pressing
  ;; one must not switch, spawn, or arm anything today.  Uses an 8-wide
  ;; grid so there is a slot in the middle that is not claimed by any
  ;; existing hotkey (allow, reject, interrupt, show-all, favorites,
  ;; delete).  Presses column 2 -- unclaimed on 8-wide.
  (let ((agent-shell-monome--state
         (list (cons :bindings nil)
               (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :delete-key-down nil)))
        (agent-shell-monome-spawn-on-empty-press t))
    (cl-letf (((symbol-function 'agent-shell--new-shell)
               (lambda (&rest _) (error "hotkey row must not spawn")))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (error "hotkey row must not switch"))))
      (agent-shell-monome--on-grid-key 2 3 1)
      (agent-shell-monome--on-grid-key 2 3 0)
      (should-not (alist-get :delete-key-down agent-shell-monome--state)))))

(defun agent-shell-monome-tests--last-level-for-coord (sent coord)
  "Return the last brightness sent to grid COORD across SENT packets.
SENT is a list of (ADDRESS . ((i . X) (i . Y) (i . LEVEL))) as recorded
by the send-grid stub in these tests, most recent first."
  (catch 'found
    (dolist (packet sent)
      (let ((args (cdr packet)))
        (when (and (equal (car args) (cons 'i (car coord)))
                   (equal (nth 1 args) (cons 'i (cdr coord))))
          (throw 'found (cdr (nth 2 args))))))
    nil))

(ert-deftest agent-shell-monome--render-hotkeys-pulses-when-armed ()
  ;; Both hotkeys draw dim when idle and pulse bright while their armed
  ;; flags are set, so the user always sees the arm state without
  ;; needing to look away from the grid.  Uses an 8-wide grid so the
  ;; delete (7) and favorites (6) coords are clear of the review keys
  ;; sitting at (2) and (3).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :grid-prefix "/monome-grid")
               (cons :last-leds nil)
               (cons :delete-key-down nil)
               (cons :favorites-key-down nil)))
        (agent-shell-monome-level-idle 2)
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-grid)
               (lambda (address args) (push (cons address args) sent)))
              ;; --render-hotkeys also paints the interrupt LED, which
              ;; reads the selected buffer -- keep this test focused on
              ;; the two pulsing hotkeys by stubbing that away.
              ((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () nil)))
      ;; Idle: both hotkeys at level-idle.
      (agent-shell-monome--render-hotkeys 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 7 3))))
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 6 3))))
      (setq sent nil)
      ;; Delete armed, favorites idle: only delete pulses.
      (setf (alist-get :delete-key-down agent-shell-monome--state) t)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (agent-shell-monome--render-hotkeys 0)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 7 3))))
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 6 3))))
      (setq sent nil)
      ;; Favorites armed, delete idle: only favorites pulses.
      (setf (alist-get :delete-key-down agent-shell-monome--state) nil)
      (setf (alist-get :favorites-key-down agent-shell-monome--state) t)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (agent-shell-monome--render-hotkeys 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 7 3))))
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 6 3))))
      (setq sent nil)
      ;; Dim half of the pulse (tick 2 of 4) for the armed favorites key.
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (agent-shell-monome--render-hotkeys 2)
      (should (= 8 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 6 3)))))))

;;;; Favorites hotkey (project picker)

(ert-deftest agent-shell-monome--favorites-key-coord-left-of-delete ()
  ;; The favorites key must be exactly one column left of the delete
  ;; key on the reserved hotkey row, regardless of grid size.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8) (cons :grid-height 8))))
    (should (equal (cons 6 7) (agent-shell-monome--favorites-key-coord)))
    (should (agent-shell-monome--favorites-key-p 6 7))
    (should-not (agent-shell-monome--favorites-key-p 7 7))
    (should-not (agent-shell-monome--favorites-key-p 6 6))))

(ert-deftest agent-shell-monome--favorites-key-shows-picker ()
  ;; Pressing the favorites key must open the picker (via display-buffer)
  ;; and mark the key as held.  It must not switch, spawn, or arm hold.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :bindings nil)
               (cons :favorites-key-down nil)
               (cons :favorites-index 0)
               (cons :favorites-scroll-accumulator 5)
               (cons :favorites-window nil)))
        (agent-shell-monome-favorite-projects '("/proj/a/" "/proj/b/"))
        (displayed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buf &rest _) (setq displayed buf) 'fake-window))
              ((symbol-function 'window-live-p) (lambda (_) nil))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (error "favorites key must not switch"))))
      (agent-shell-monome--on-grid-key-down 2 3)
      (should (alist-get :favorites-key-down agent-shell-monome--state))
      (should (bufferp displayed))
      (should (eq 'fake-window
                  (alist-get :favorites-window agent-shell-monome--state)))
      ;; Accumulator reset each press, so an old value cannot leak an
      ;; unexpected first step on the next turn.
      (should (= 0 (alist-get :favorites-scroll-accumulator
                              agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--favorites-scroll-steps-with-encoder ()
  ;; Encoder deltas on the favorites encoder step the picker index once
  ;; per `agent-shell-monome-favorites-ticks-per-step' ticks, wrapping.
  (let ((agent-shell-monome--state
         (list (cons :favorites-key-down t)
               (cons :favorites-index 0)
               (cons :favorites-scroll-accumulator 0)))
        (agent-shell-monome-favorite-projects '("/a/" "/b/" "/c/"))
        (agent-shell-monome-favorites-ticks-per-step 4)
        (agent-shell-monome-arc-scroll-encoder 1)
        (agent-shell-monome-arc-selector-encoder 0)
        (agent-shell-monome-arc-decision-encoder 2)
        (agent-shell-monome-arc-tokens-encoder 3))
    (cl-letf (((symbol-function 'agent-shell-monome--render-favorites-buffer)
               (lambda () nil)))
      ;; Sub-step accumulates without moving.
      (agent-shell-monome--on-enc-delta 1 2)
      (should (= 0 (alist-get :favorites-index agent-shell-monome--state)))
      ;; Cross the threshold: advance to entry 1.
      (agent-shell-monome--on-enc-delta 1 2)
      (should (= 1 (alist-get :favorites-index agent-shell-monome--state)))
      ;; Two more full steps wraps 1 -> 2 -> 0.
      (agent-shell-monome--on-enc-delta 1 8)
      (should (= 0 (alist-get :favorites-index agent-shell-monome--state)))
      ;; Negative deltas walk backwards, wrapping 0 -> 2.
      (agent-shell-monome--on-enc-delta 1 -4)
      (should (= 2 (alist-get :favorites-index agent-shell-monome--state))))))

(ert-deftest agent-shell-monome--favorites-scroll-inactive-goes-to-shell ()
  ;; When the favorites key is not held, the scroll encoder must reach
  ;; the normal shell-scroll path -- not the favorites picker.
  (let ((agent-shell-monome--state
         (list (cons :favorites-key-down nil)
               (cons :favorites-index 0)))
        (agent-shell-monome-arc-scroll-encoder 1)
        (agent-shell-monome-arc-selector-encoder 0)
        (agent-shell-monome-arc-decision-encoder 2)
        (agent-shell-monome-arc-tokens-encoder 3)
        (scrolled nil))
    (cl-letf (((symbol-function 'agent-shell-monome--scroll-on-delta)
               (lambda (d) (setq scrolled d)))
              ((symbol-function 'agent-shell-monome--favorites-on-delta)
               (lambda (&rest _)
                 (error "favorites route must not fire when key is up"))))
      (agent-shell-monome--on-enc-delta 1 3)
      (should (= 3 scrolled)))))

(ert-deftest agent-shell-monome--favorites-release-spawns-and-closes ()
  ;; On release: close the picker window, then spawn a new agent-shell
  ;; rooted at the currently selected favorite.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :favorites-key-down t)
               (cons :favorites-index 1)
               (cons :favorites-window 'fake-window)))
        (agent-shell-monome-favorite-projects '("/proj/a/" "/proj/b/"))
        (deleted nil)
        (spawned-at 'unset))
    (cl-letf (((symbol-function 'window-live-p)
               (lambda (w) (eq w 'fake-window)))
              ((symbol-function 'delete-window)
               (lambda (w) (setq deleted w)))
              ((symbol-function 'agent-shell--new-shell)
               (lambda (&rest args)
                 (setq spawned-at (plist-get args :location)))))
      (agent-shell-monome--on-grid-key-up 2 3)
      (should-not (alist-get :favorites-key-down agent-shell-monome--state))
      (should (eq deleted 'fake-window))
      (should-not (alist-get :favorites-window agent-shell-monome--state))
      ;; Spawned into favorite index 1 (/proj/b/).
      (should (equal "/proj/b/" spawned-at)))))

(ert-deftest agent-shell-monome--favorites-release-empty-list-noop ()
  ;; An empty favorites list must not spawn on release, but must still
  ;; disarm cleanly.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :favorites-key-down t)
               (cons :favorites-index 0)
               (cons :favorites-window nil)))
        (agent-shell-monome-favorite-projects nil))
    (cl-letf (((symbol-function 'agent-shell--new-shell)
               (lambda (&rest _)
                 (error "empty favorites list must not spawn")))
              ((symbol-function 'window-live-p) (lambda (_) nil)))
      (agent-shell-monome--on-grid-key-up 2 3)
      (should-not (alist-get :favorites-key-down agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--current-favorite-clamps-index ()
  ;; A stale :favorites-index past the list end must not crash or point
  ;; past the last entry -- it clamps (modulo) into the list.
  (let ((agent-shell-monome--state
         (list (cons :favorites-index 99)))
        (agent-shell-monome-favorite-projects '("/a/" "/b/" "/c/")))
    (should (equal "/a/" (agent-shell-monome--current-favorite)))))

(ert-deftest agent-shell-monome--current-favorite-nil-when-empty ()
  (let ((agent-shell-monome--state (list (cons :favorites-index 0)))
        (agent-shell-monome-favorite-projects nil))
    (should-not (agent-shell-monome--current-favorite))))

;;;; Show-all hotkey (tile every agent-shell buffer)

(ert-deftest agent-shell-monome--show-all-key-coord-third-from-right ()
  ;; The show-all key must sit exactly two columns left of delete on the
  ;; hotkey row (i.e. one column left of favorites), regardless of grid
  ;; size, so counting from the right stays consistent as more hotkeys
  ;; are added.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 16) (cons :grid-height 8))))
    (should (equal (cons 13 7) (agent-shell-monome--show-all-key-coord)))
    (should (agent-shell-monome--show-all-key-p 13 7))
    (should-not (agent-shell-monome--show-all-key-p 14 7))
    (should-not (agent-shell-monome--show-all-key-p 15 7))))

(ert-deftest agent-shell-monome--show-all-toggles-on-then-off ()
  ;; First tap tiles every live shell (snapshotting the pre-show config
  ;; first); second tap restores the snapshotted config verbatim.  The
  ;; bright/dim LED state follows the flag.  We stub the tile-dimensions
  ;; picker to a fixed 2x2 so this test does not depend on the batch
  ;; frame's actual size.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :show-all-window-config nil)))
        (placed nil)
        (restored nil)
        (window-counter 0))
    (cl-letf (((symbol-function 'agent-shell-buffers)
               (lambda () '(a b c d)))
              ((symbol-function 'buffer-live-p) (lambda (_) t))
              ((symbol-function 'current-window-configuration)
               (lambda () 'saved-config))
              ((symbol-function 'delete-other-windows) (lambda () nil))
              ((symbol-function 'selected-window) (lambda () 'W0))
              ((symbol-function 'split-window)
               (lambda (&rest _)
                 (setq window-counter (1+ window-counter))
                 (intern (format "W%d" window-counter))))
              ((symbol-function 'set-window-buffer)
               (lambda (window buf) (push (cons window buf) placed)))
              ((symbol-function 'select-window) (lambda (_w) nil))
              ((symbol-function 'balance-windows) (lambda (&rest _) nil))
              ((symbol-function 'agent-shell-monome--tile-dimensions)
               (lambda (&rest _) (cons 2 2)))
              ((symbol-function 'set-window-configuration)
               (lambda (c) (setq restored c))))
      ;; First press: tile.
      (agent-shell-monome--on-grid-key-down 1 3)
      (should (eq 'saved-config
                  (alist-get :show-all-window-config
                             agent-shell-monome--state)))
      ;; Every one of the four buffers landed in some window.
      (should (equal '(a b c d)
                     (sort (mapcar #'cdr placed) #'string<)))
      ;; Second press: restore the saved config exactly.
      (agent-shell-monome--on-grid-key-down 1 3)
      (should (eq 'saved-config restored))
      (should-not (alist-get :show-all-window-config
                             agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--tile-dimensions-picks-square-when-even ()
  ;; 4 shells in a landscape frame tile as 2x2 (waste=0, aspect fine),
  ;; not 3x2 with a stray blank cell nor 4x1 slivers.
  (should (equal (cons 2 2)
                 (agent-shell-monome--tile-dimensions 4 200 50))))

(ert-deftest agent-shell-monome--tile-dimensions-prefers-wider-than-tall ()
  ;; A single row of narrow slivers for many agents is exactly what the
  ;; old \"one column per agent\" layout gave; the new picker must NOT
  ;; land on cols=n even for landscape frames.
  (let* ((dims (agent-shell-monome--tile-dimensions 6 200 50))
         (cols (car dims))
         (rows (cdr dims)))
    (should (> rows 1))
    (should (< cols 6))
    (should (>= (* cols rows) 6))))

(ert-deftest agent-shell-monome--tile-dimensions-trivial-cases ()
  ;; N of 0 or 1 collapses to a single pane; N == 2 in a landscape
  ;; frame is a single row of two.
  (should (equal (cons 1 1) (agent-shell-monome--tile-dimensions 0 200 50)))
  (should (equal (cons 1 1) (agent-shell-monome--tile-dimensions 1 200 50)))
  (should (equal (cons 2 1) (agent-shell-monome--tile-dimensions 2 200 50))))

(ert-deftest agent-shell-monome--show-all-noop-with-no-shells ()
  ;; Tapping with no live agent-shell buffers must not touch the window
  ;; layout, save nothing, and leave the toggle off.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :show-all-window-config nil))))
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
              ((symbol-function 'current-window-configuration)
               (lambda () (error "must not snapshot when no shells")))
              ((symbol-function 'delete-other-windows)
               (lambda () (error "must not rearrange when no shells"))))
      (agent-shell-monome--on-grid-key-down 1 3)
      (should-not (alist-get :show-all-window-config
                             agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--show-all-release-is-noop ()
  ;; The key is tap-to-toggle (not held-modifier), so releases must not
  ;; touch the show-all state -- otherwise the very release that
  ;; completes the tap-on gesture would immediately toggle back off.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :show-all-window-config 'saved))))
    (agent-shell-monome--on-grid-key-up 1 3)
    (should (eq 'saved (alist-get :show-all-window-config
                                  agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--show-all-led-bright-when-active ()
  ;; Show-all's LED is dim (level-idle) when off and steadily bright when
  ;; on -- no pulse, since it is a toggle rather than a held modifier.
  ;; Uses an 8-wide grid so show-all at (5, 3) doesn't collide with the
  ;; permission keys sitting at (0, 3) / (1, 3).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :grid-prefix "/monome-grid")
               (cons :last-leds nil)
               (cons :delete-key-down nil)
               (cons :favorites-key-down nil)
               (cons :show-all-window-config nil)))
        (agent-shell-monome-level-idle 2)
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-grid)
               (lambda (address args) (push (cons address args) sent)))
              ((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () nil)))
      (agent-shell-monome--render-hotkeys 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 5 3))))
      ;; Activate: the LED goes to full brightness on the very next
      ;; render, and stays there across tick phases (no pulse).
      (setf (alist-get :show-all-window-config
                       agent-shell-monome--state) 'saved)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-hotkeys 0)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 5 3))))
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-hotkeys 2)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 5 3)))))))

;;;; Hold-to-talk SomaFM ducking

(ert-deftest agent-shell-monome--somafm-duck-drops-and-restores ()
  ;; Duck records the applied step count in :htt-somafm-ducked; unduck
  ;; feeds that exact count back to volume-up so relative moves cancel,
  ;; and clears the field so a second unduck is a no-op.
  (let ((agent-shell-monome--state (list (cons :htt-somafm-ducked nil)))
        (agent-shell-monome-hold-to-talk-duck-somafm t)
        (agent-shell-monome-hold-to-talk-somafm-duck-steps 60)
        (down nil)
        (up nil))
    (cl-letf (((symbol-function 'somafm-player-volume-down)
               (lambda (n) (setq down n)))
              ((symbol-function 'somafm-player-volume-up)
               (lambda (n) (setq up n)))
              ((symbol-function 'get-process)
               (lambda (name) (and (equal name "somafm player") 'fake-proc))))
      (agent-shell-monome--htt-duck-somafm)
      (should (= 60 down))
      (should (= 60 (alist-get :htt-somafm-ducked
                               agent-shell-monome--state)))
      (agent-shell-monome--htt-unduck-somafm)
      (should (= 60 up))
      (should-not (alist-get :htt-somafm-ducked
                             agent-shell-monome--state))
      ;; Idempotent: unduck with nothing armed does not touch the volume.
      (setq up nil)
      (agent-shell-monome--htt-unduck-somafm)
      (should-not up))))

(ert-deftest agent-shell-monome--somafm-duck-noop-when-disabled ()
  ;; With the duck disabled, neither volume-down is called nor the state
  ;; field set.
  (let ((agent-shell-monome--state (list (cons :htt-somafm-ducked nil)))
        (agent-shell-monome-hold-to-talk-duck-somafm nil)
        (called nil))
    (cl-letf (((symbol-function 'somafm-player-volume-down)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'get-process)
               (lambda (_) 'fake-proc)))
      (agent-shell-monome--htt-duck-somafm)
      (should-not called)
      (should-not (alist-get :htt-somafm-ducked
                             agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--somafm-duck-noop-when-player-absent ()
  ;; No SomaFM player process running means no duck (safe to leave the
  ;; feature on even for users who don't run SomaFM most of the time).
  (let ((agent-shell-monome--state (list (cons :htt-somafm-ducked nil)))
        (agent-shell-monome-hold-to-talk-duck-somafm t)
        (called nil))
    (cl-letf (((symbol-function 'somafm-player-volume-down)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'get-process) (lambda (_) nil)))
      (agent-shell-monome--htt-duck-somafm)
      (should-not called)
      (should-not (alist-get :htt-somafm-ducked
                             agent-shell-monome--state)))))

(ert-deftest agent-shell-monome--htt-end-unducks-on-release ()
  ;; The release path (--htt-end) always unducks, even when whisper is
  ;; no longer recording (e.g. it stopped itself).  Otherwise a duck
  ;; that succeeded but whose recording ended asynchronously would leave
  ;; the volume permanently low.
  (let ((agent-shell-monome--state
         (list (cons :htt-recording 'buf)
               (cons :htt-down-coord (cons 0 0))
               (cons :htt-somafm-ducked 42)))
        (up nil))
    (cl-letf (((symbol-function 'whisper-recording-p) (lambda () nil))
              ((symbol-function 'somafm-player-volume-up)
               (lambda (n) (setq up n)))
              ((symbol-function 'get-process) (lambda (_) 'fake-proc)))
      (agent-shell-monome--htt-end)
      (should (= 42 up))
      (should-not (alist-get :htt-somafm-ducked
                             agent-shell-monome--state)))))

;;;; Interrupt hotkey (bail out of the selected shell's turn)

(ert-deftest agent-shell-monome--interrupt-key-coord-fourth-from-right ()
  ;; Interrupt lives one column left of show-all on the hotkey row, so
  ;; the four right-side hotkeys (delete, favorites, show-all, interrupt)
  ;; sit in that order counting from the far right.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 16) (cons :grid-height 8))))
    (should (equal (cons 12 7) (agent-shell-monome--interrupt-key-coord)))
    (should (agent-shell-monome--interrupt-key-p 12 7))
    (should-not (agent-shell-monome--interrupt-key-p 13 7))
    (should-not (agent-shell-monome--interrupt-key-p 12 6))))

(ert-deftest agent-shell-monome--interrupt-key-fires-on-selected ()
  ;; A press on the interrupt key must run agent-shell-interrupt with
  ;; FORCE non-nil (skipping the modal prompt), scoped to the selected
  ;; buffer -- not to whatever buffer happens to be current when the
  ;; OSC filter fires.  Uses a real buffer since `with-current-buffer'
  ;; is a macro whose set-buffer call cannot be cl-letf'd.
  (let ((buf (generate-new-buffer " *asm-interrupt-test*"))
        (agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :bindings nil)))
        (called-buf nil)
        (called-force nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
                   (lambda () buf))
                  ((symbol-function 'agent-shell-interrupt)
                   (lambda (&optional force)
                     (setq called-buf (current-buffer))
                     (setq called-force force))))
          (agent-shell-monome--on-grid-key-down 0 3)
          (should (eq buf called-buf))
          (should called-force))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-shell-monome--interrupt-key-noop-without-selection ()
  ;; With no selected shell, the trigger must not error and must not
  ;; call agent-shell-interrupt (which would fail with "not in a shell"
  ;; if invoked outside an agent-shell-mode buffer).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4))))
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () nil))
              ((symbol-function 'agent-shell-interrupt)
               (lambda (&rest _)
                 (error "must not fire with no selected buffer"))))
      (agent-shell-monome--on-grid-key-down 0 3))))

(ert-deftest agent-shell-monome--interrupt-led-follows-selected-status ()
  ;; The interrupt key's brightness mirrors the selected shell's status:
  ;; dim when idle or unselected, bright when blocked (so it doubles as
  ;; an at-a-glance "worth interrupting?" cue right by the trigger).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 4)
               (cons :grid-height 4)
               (cons :grid-prefix "/monome-grid")
               (cons :last-leds nil)))
        (agent-shell-monome-level-idle 2)
        (agent-shell-monome-level-blocked 15)
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-grid)
               (lambda (address args) (push (cons address args) sent))))
      ;; No selected buffer -> dim.
      (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
                 (lambda () nil)))
        (agent-shell-monome--render-interrupt-led)
        (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                      sent (cons 0 3)))))
      (setq sent nil)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      ;; Selected shell blocked -> full bright.
      (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
                 (lambda () 'buf))
                ((symbol-function 'buffer-live-p) (lambda (_) t))
                ((symbol-function 'agent-shell-status)
                 (lambda (&rest _) 'blocked)))
        (agent-shell-monome--render-interrupt-led)
        (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                       sent (cons 0 3))))))))

;;;; Permission hotkeys (bottom-left pair: allow / reject)

(ert-deftest agent-shell-monome--permission-key-coords-are-leftmost ()
  ;; The two leftmost columns of the hotkey row are reserved for
  ;; permission answers: x=0 allow, x=1 reject.  Fixed by column, so
  ;; they sit in the same spot regardless of grid width.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 16) (cons :grid-height 8))))
    (should (equal (cons 0 7) (agent-shell-monome--allow-key-coord)))
    (should (equal (cons 1 7) (agent-shell-monome--reject-key-coord)))
    (should (agent-shell-monome--allow-key-p 0 7))
    (should (agent-shell-monome--reject-key-p 1 7))
    (should-not (agent-shell-monome--allow-key-p 1 7))
    (should-not (agent-shell-monome--allow-key-p 0 6))))

(ert-deftest agent-shell-monome--allow-key-answers-oldest-pending ()
  ;; A tap on the allow key answers the selected shell's oldest pending
  ;; prompt with its allow option; the reject key does the same with the
  ;; reject option.  Uses an 8-wide grid so the two permission keys sit
  ;; in truly unclaimed slots (no collision with right-side hotkeys).
  (let* ((log nil)
         (agent-shell-monome--state
          (list (cons :grid-width 8)
                (cons :grid-height 4)
                (cons :bindings nil)
                (cons :pending-permissions
                      (list (list (cons :respond
                                        (lambda (id) (push (cons 'fired id) log)))
                                  (cons :allow-id "allow-1")
                                  (cons :reject-id "reject-1")
                                  (cons :buffer 'sel)))))))
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () 'sel)))
      (agent-shell-monome--on-grid-key-down 0 3)
      (should (equal '(fired . "allow-1") (car log)))
      (should-not (alist-get :pending-permissions agent-shell-monome--state))
      ;; Requeue and try reject.
      (setf (alist-get :pending-permissions agent-shell-monome--state)
            (list (list (cons :respond
                              (lambda (id) (push (cons 'fired id) log)))
                        (cons :allow-id "allow-2")
                        (cons :reject-id "reject-2")
                        (cons :buffer 'sel))))
      (agent-shell-monome--on-grid-key-down 1 3)
      (should (equal '(fired . "reject-2") (car log))))))

(ert-deftest agent-shell-monome--permission-keys-noop-without-pending ()
  ;; Pressing allow / reject with nothing queued for the selected shell
  ;; must be silent -- no error, no side effect.  A press with no
  ;; selected buffer at all must also be a no-op (never answer an
  ;; off-screen shell's prompt by accident).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :bindings nil)
               (cons :pending-permissions nil))))
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () nil)))
      (agent-shell-monome--on-grid-key-down 0 3)
      (agent-shell-monome--on-grid-key-down 1 3))
    ;; With a selected buffer but nothing queued -- still silent.
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () 'sel)))
      (agent-shell-monome--on-grid-key-down 0 3)
      (agent-shell-monome--on-grid-key-down 1 3))))

(ert-deftest agent-shell-monome--permission-leds-bright-when-pending ()
  ;; The two permission keys light bright when the selected shell has a
  ;; pending prompt and stay at level-idle otherwise, so the pair is
  ;; visually silent unless there is something to respond to.  Only the
  ;; selected buffer's prompts count -- an off-screen shell's queued
  ;; prompt must not trip the LEDs.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :grid-prefix "/monome-grid")
               (cons :last-leds nil)
               (cons :delete-key-down nil)
               (cons :favorites-key-down nil)
               (cons :show-all-window-config nil)
               (cons :pending-permissions nil)))
        (agent-shell-monome-level-idle 2)
        (agent-shell-monome-level-blocked 15)
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-grid)
               (lambda (address args) (push (cons address args) sent)))
              ((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () 'sel)))
      ;; No pending: dim.
      (agent-shell-monome--render-hotkeys 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 0 3))))
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 1 3))))
      ;; Foreign-buffer prompt: LEDs stay dim (dial-parity rule).
      (setf (alist-get :pending-permissions agent-shell-monome--state)
            (list (list (cons :buffer 'other))))
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-hotkeys 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 0 3))))
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 1 3))))
      ;; Selected-buffer prompt: both keys bright.
      (setf (alist-get :pending-permissions agent-shell-monome--state)
            (list (list (cons :buffer 'sel))))
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-hotkeys 0)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 0 3))))
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 1 3)))))))

;;;; Magit review workflow (bottom-row back / forward pair)

(ert-deftest agent-shell-monome--review-key-coords-are-central ()
  ;; The review pair sits at columns 2 (back) and 3 (forward) on the
  ;; hotkey row.  Fixed columns, so their positions don't depend on grid
  ;; width -- on an 8-wide grid they land in the free slot between the
  ;; allow/reject pair (0,1) and the right-side hotkeys (interrupt
  ;; onward).
  (let ((agent-shell-monome--state
         (list (cons :grid-width 16) (cons :grid-height 8))))
    (should (equal (cons 2 7) (agent-shell-monome--review-back-key-coord)))
    (should (equal (cons 3 7) (agent-shell-monome--review-forward-key-coord)))
    (should (agent-shell-monome--review-back-key-p 2 7))
    (should (agent-shell-monome--review-forward-key-p 3 7))
    (should-not (agent-shell-monome--review-back-key-p 3 7))
    (should-not (agent-shell-monome--review-forward-key-p 2 7))))

(ert-deftest agent-shell-monome--review-forward-dispatches-per-state ()
  ;; Forward's meaning depends on the current phase: nil -> start,
  ;; :review -> advance a hunk, :confirm -> enter commit, :commit ->
  ;; finalize, :sync -> no-op (sync always runs to completion).  Stubs
  ;; the phase handlers to capture which one gets called from the
  ;; central dispatcher, so we're testing routing, not the phase logic
  ;; itself.
  (let ((agent-shell-monome--state (list (cons :review-state nil)))
        (calls nil))
    (cl-letf (((symbol-function 'agent-shell-monome--review-start)
               (lambda () (push 'start calls)))
              ((symbol-function 'agent-shell-monome--review-advance-hunk)
               (lambda () (push 'advance calls)))
              ((symbol-function 'agent-shell-monome--review-enter-commit)
               (lambda () (push 'enter-commit calls)))
              ((symbol-function 'agent-shell-monome--review-finalize-commit)
               (lambda () (push 'finalize calls))))
      (agent-shell-monome--review-forward)
      (agent-shell-monome--set-review-state :review)
      (agent-shell-monome--review-forward)
      (agent-shell-monome--set-review-state :confirm)
      (agent-shell-monome--review-forward)
      (agent-shell-monome--set-review-state :commit)
      (agent-shell-monome--review-forward)
      (agent-shell-monome--set-review-state :sync)
      (agent-shell-monome--review-forward))
    (should (equal '(finalize enter-commit advance start)
                   (seq-take calls 4)))))

(ert-deftest agent-shell-monome--review-back-dispatches-per-state ()
  ;; Back is a rewind: nil is a no-op (workflow starts on forward),
  ;; :review walks back a hunk, :confirm returns to :review, :commit
  ;; cancels and returns to :confirm, :sync is a no-op.
  (let ((agent-shell-monome--state (list (cons :review-state nil)))
        (calls nil))
    (cl-letf (((symbol-function 'agent-shell-monome--review-retreat-hunk)
               (lambda () (push 'retreat calls)))
              ((symbol-function 'agent-shell-monome--review-back-to-review)
               (lambda () (push 'back-to-review calls)))
              ((symbol-function 'agent-shell-monome--review-cancel-commit)
               (lambda () (push 'cancel-commit calls))))
      (agent-shell-monome--review-back)  ;; nil -> nothing
      (agent-shell-monome--set-review-state :review)
      (agent-shell-monome--review-back)
      (agent-shell-monome--set-review-state :confirm)
      (agent-shell-monome--review-back)
      (agent-shell-monome--set-review-state :commit)
      (agent-shell-monome--review-back))
    (should (equal '(cancel-commit back-to-review retreat) calls))))

(ert-deftest agent-shell-monome--review-advance-past-end-enters-confirm ()
  ;; When magit's `magit-section-forward' errors ("No next section"),
  ;; the walker treats that as "end of the diff" and transitions to
  ;; :confirm on the same tap -- so the user doesn't have to press
  ;; twice at the boundary.
  (let ((buf (generate-new-buffer " *asm-review-test*"))
        (agent-shell-monome--state (list (cons :review-state :review))))
    (unwind-protect
        (progn
          (setf (alist-get :review-magit-buffer agent-shell-monome--state)
                buf)
          (cl-letf (((symbol-function 'magit-section-forward)
                     (lambda () (user-error "No next section"))))
            (agent-shell-monome--review-advance-hunk))
          (should (eq :confirm (agent-shell-monome--review-state))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-shell-monome--confirm-allow-enters-commit-not-permission ()
  ;; During :confirm the allow key must run the review's commit
  ;; transition, not the pending-permission decision path (even if a
  ;; prompt happens to be queued for the selected buffer).  Reject
  ;; likewise aborts the review instead of rejecting the prompt.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :bindings nil)
               (cons :review-state :confirm)
               (cons :pending-permissions
                     (list (list (cons :allow-id "a")
                                 (cons :reject-id "r")
                                 (cons :respond (lambda (_) (error "nope")))
                                 (cons :buffer 'sel))))))
        (called nil))
    (cl-letf (((symbol-function 'agent-shell-monome--selected-buffer)
               (lambda () 'sel))
              ((symbol-function 'agent-shell-monome--review-enter-commit)
               (lambda () (push 'commit called)))
              ((symbol-function 'agent-shell-monome--review-abort)
               (lambda () (push 'abort called))))
      (agent-shell-monome--on-grid-key-down 0 3)
      (agent-shell-monome--set-review-state :confirm) ;; commit stub didn't touch it
      (agent-shell-monome--on-grid-key-down 1 3))
    (should (equal '(abort commit) called))))

(ert-deftest agent-shell-monome--forward-tap-in-commit-runs-finalize ()
  ;; With a captured commit buffer, pressing forward arms hold-to-talk;
  ;; a quick release (timer still pending) resolves as a tap and calls
  ;; --review-forward, which in :commit means finalize.  Uses a stub
  ;; whisper (fboundp check gates the arm path).
  (let* ((buf (generate-new-buffer " *asm-commit-test*"))
         (agent-shell-monome--state
          (list (cons :grid-width 8)
                (cons :grid-height 4)
                (cons :bindings nil)
                (cons :review-state :commit)
                (cons :review-commit-buffer buf)
                (cons :htt-down-coord nil)
                (cons :htt-timer nil)
                (cons :htt-recording nil)))
         (agent-shell-monome-hold-to-talk t)
         (agent-shell-monome-hold-threshold 999)
         (finalized nil))
    (unwind-protect
        (cl-letf (((symbol-function 'whisper-run) (lambda () nil))
                  ((symbol-function 'agent-shell-monome--review-finalize-commit)
                   (lambda () (push 'finalize finalized))))
          (agent-shell-monome--on-grid-key-down 3 3)
          (should (equal (cons 3 3)
                         (alist-get :htt-down-coord
                                    agent-shell-monome--state)))
          (should (alist-get :htt-timer agent-shell-monome--state))
          (agent-shell-monome--on-grid-key-up 3 3)
          (should (equal '(finalize) finalized))
          (should-not (alist-get :htt-down-coord
                                 agent-shell-monome--state))
          (should-not (alist-get :htt-timer agent-shell-monome--state)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-shell-monome--transcription-plain-insert-skips-shell-submit ()
  ;; With :htt-plain-insert set (as the commit-message dictation path
  ;; does), the transcription handler must ignore hold-to-talk-submit
  ;; and just insert text at point-max -- calling shell-maker-submit on
  ;; a non-shell buffer like COMMIT_EDITMSG would error.
  (let* ((target (generate-new-buffer " *asm-plain-insert*"))
         (agent-shell-monome--state
          (list (cons :htt-target target)
                (cons :htt-plain-insert t)))
         (agent-shell-monome-hold-to-talk-submit t))
    (unwind-protect
        (cl-letf (((symbol-function 'shell-maker-submit)
                   (lambda (&rest _)
                     (error "shell-maker-submit must not be called"))))
          (with-temp-buffer
            (insert "  hello  ")
            (agent-shell-monome--whisper-transcription-handler))
          (with-current-buffer target
            (should (equal "hello" (buffer-string))))
          (should-not (alist-get :htt-plain-insert
                                 agent-shell-monome--state)))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest agent-shell-monome--review-leds-bright-when-active ()
  ;; Both review LEDs are dim at level-idle when :review-state is nil
  ;; and go steady bright once any review phase is running -- the pair
  ;; is a visible marker of "review is in flight."  Forward pulses
  ;; during commit-message dictation to signal the mic being live.
  (let ((agent-shell-monome--state
         (list (cons :grid-width 8)
               (cons :grid-height 4)
               (cons :grid-prefix "/monome-grid")
               (cons :last-leds nil)
               (cons :review-state nil)
               (cons :htt-recording nil)))
        (agent-shell-monome-level-idle 2)
        (sent nil))
    (cl-letf (((symbol-function 'agent-shell-monome--send-grid)
               (lambda (address args) (push (cons address args) sent))))
      ;; Idle.
      (agent-shell-monome--render-review-leds 0)
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 2 3))))
      (should (= 2 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 3 3))))
      ;; Active (any non-nil state).
      (setf (alist-get :review-state agent-shell-monome--state) :review)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-review-leds 0)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 2 3))))
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 3 3))))
      ;; Commit + recording: forward pulses; tick 2 lands in the dark
      ;; half of the pulse.
      (setf (alist-get :review-state agent-shell-monome--state) :commit)
      (setf (alist-get :htt-recording agent-shell-monome--state) 'buf)
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-review-leds 0)
      (should (= 15 (agent-shell-monome-tests--last-level-for-coord
                     sent (cons 3 3))))
      (setf (alist-get :last-leds agent-shell-monome--state) nil)
      (setq sent nil)
      (agent-shell-monome--render-review-leds 2)
      (should (= 0 (agent-shell-monome-tests--last-level-for-coord
                    sent (cons 3 3)))))))

(provide 'agent-shell-monome-tests)
;;; agent-shell-monome-tests.el ends here
