;;; agent-shell-monome.el --- Monome grid + arc control for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: noonker <noonker@pm.me>
;; URL: https://github.com/noonker/agent-shell-monome
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (agent-shell "0.1"))
;; Keywords: tools, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Reflect `agent-shell' buffer state on a monome grid via serialosc,
;; and (optionally) drive selection / scroll / decision via a monome
;; arc.
;;
;; Usage:
;;
;;   M-x agent-shell-monome-start
;;   M-x agent-shell-monome-stop
;;
;; By default the bridge launches its own serialosc (the daemon that
;; exposes monome devices over OSC) for the lifetime of the session and
;; shuts it down on stop -- no separate service needed.  Set
;; `agent-shell-monome-manage-serialosc' to nil to rely on an external
;; one instead.
;;
;; Grid:
;;   Each agent-shell buffer occupies one grid key.
;;   - Dim     - buffer exists, agent idle.
;;   - Pulsing - agent is busy.
;;   - Bright  - agent is blocked waiting for input.
;;   Tapping a lit key switches to that buffer.
;;   Holding a lit key past `agent-shell-monome-hold-threshold' records
;;   voice into that buffer's shell via the `whisper' package: the key
;;   blinks while the mic is live, and on release the audio is
;;   transcribed and inserted at the buffer's prompt.  Requires the
;;   `whisper' package (https://github.com/natrys/whisper.el); see
;;   `agent-shell-monome-hold-to-talk' to disable.
;;
;; Arc (uses the first 3 encoders):
;;   Ring 1 (selector) - one indicator LED per buffer at even spacing,
;;                       brightness reflects status, a brighter "pointer"
;;                       wedge marks the currently selected buffer.
;;                       Snap-to-nearest.
;;   Ring 2 (scroll)   - turn to scroll the selected buffer up/down.
;;                       LED dot tracks point's relative position.
;;   Ring 3 (decision) - turn right past a threshold to allow, left
;;                       past a threshold to reject a pending permission
;;                       prompt.  Prompts are answered oldest-first when
;;                       several are waiting.  Sub-threshold motion is
;;                       shown as a half-ring fill.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'seq)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell" (&key shell-buffer))
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--get-available-thought-levels "agent-shell" (state))
(declare-function agent-shell--current-thought-level-id "agent-shell" (state))
(declare-function agent-shell--config-option-set-thought-level-id "agent-shell")
(declare-function agent-shell--new-shell "agent-shell")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function whisper-run "whisper")
(declare-function whisper-recording-p "whisper")
(declare-function whisper-transcribing-p "whisper")
(declare-function shell-maker-submit "shell-maker")
(defvar agent-shell-permission-responder-function)
(defvar whisper-insert-text-at-point)
(defvar whisper-after-transcription-hook)

;;; Customization

(defgroup agent-shell-monome nil
  "Monome integration for `agent-shell'."
  :group 'agent-shell
  :prefix "agent-shell-monome-")

(defcustom agent-shell-monome-serialosc-host "127.0.0.1"
  "Host where serialosc is running."
  :type 'string)

(defcustom agent-shell-monome-serialosc-port 12002
  "UDP port for serialosc discovery."
  :type 'integer)

(defcustom agent-shell-monome-manage-serialosc t
  "When non-nil, run serialosc as a child process for this session.
serialosc is the daemon that exposes monome devices over OSC; the bridge
cannot see any device without it.  With this enabled the bridge starts
its own copy on `agent-shell-monome-start' and stops it on
`agent-shell-monome-stop', so no separate service is required.  If a
serialosc is already listening on the discovery port, the bridge adopts
it instead of launching a competing instance (and leaves it running on
stop).  Set to nil when serialosc is already managed elsewhere (e.g. a
system or user service)."
  :type 'boolean)

(defcustom agent-shell-monome-serialosc-command "serialoscd"
  "Program used to launch serialosc when managing it.
Resolved with `executable-find', so a bare name on `exec-path' or an
absolute path both work."
  :type 'string)

(defcustom agent-shell-monome-serialosc-startup-delay 0.7
  "Seconds to wait after launching serialosc before discovering devices.
Gives a freshly started daemon time to bind its discovery port."
  :type 'number)

(defcustom agent-shell-monome-listen-port 7770
  "Local UDP port bound for OSC messages from serialosc and devices."
  :type 'integer)

(defcustom agent-shell-monome-tick-seconds 0.1
  "Seconds between LED refresh ticks."
  :type 'number)

(defcustom agent-shell-monome-level-idle 2
  "LED brightness (0-15) for an idle agent-shell buffer."
  :type 'integer)

(defcustom agent-shell-monome-level-blocked 15
  "LED brightness (0-15) when an agent is blocked waiting for input."
  :type 'integer)

(defcustom agent-shell-monome-busy-levels '(4 5 6 7 8 9 8 7 6 5)
  "Cycle of LED brightnesses (0-15) used while an agent is busy."
  :type '(repeat integer))

(defcustom agent-shell-monome-arc-encoder-count 4
  "Number of encoders on the connected arc (1, 2, or 4)."
  :type 'integer)

(defcustom agent-shell-monome-arc-selector-encoder 0
  "Encoder index used as the buffer-selector dial (ring 1 in UX terms)."
  :type 'integer)

(defcustom agent-shell-monome-arc-scroll-encoder 1
  "Encoder index used to scroll the selected buffer (ring 2)."
  :type 'integer)

(defcustom agent-shell-monome-arc-decision-encoder 2
  "Encoder index used for yes/no decisions (ring 3)."
  :type 'integer)

(defcustom agent-shell-monome-arc-tokens-encoder 3
  "Encoder index used to display token rate / change effort (ring 4)."
  :type 'integer)

(defcustom agent-shell-monome-arc-tokens-window-seconds 60.0
  "Seconds of token deltas retained to compute the rolling token rate."
  :type 'number)

(defcustom agent-shell-monome-arc-tokens-max-rate 200.0
  "Token rate (tokens/sec) at which the ring 4 spinner reaches full speed."
  :type 'number)

(defcustom agent-shell-monome-arc-tokens-spinner-max-rps 1.0
  "Ring 4 spinner speed, in revolutions per second, at the max token rate.
Ring 4 shows token throughput as a wedge rotating around the ring: it
sits still when idle and spins this fast once the rolling token rate
reaches `agent-shell-monome-arc-tokens-max-rate', scaling linearly in
between.  Expressed per second, so it is independent of
`agent-shell-monome-tick-seconds'."
  :type 'number)

(defcustom agent-shell-monome-arc-effort-ticks-per-step 32
  "Encoder delta ticks required to step thought-level by one."
  :type 'integer)

(defcustom agent-shell-monome-arc-selector-ticks-per-step 16
  "Encoder delta ticks required to move the selector by one buffer.
Lower = more sensitive."
  :type 'integer)

(defcustom agent-shell-monome-arc-scroll-ticks-per-line 8
  "Encoder delta ticks per line of buffer scroll."
  :type 'integer)

(defcustom agent-shell-monome-arc-decision-threshold 80
  "Accumulated encoder delta required to commit a yes/no decision.
Acts as the dead-zone size against jitter."
  :type 'integer)

(defcustom agent-shell-monome-arc-decision-decay 0.85
  "Per-tick multiplier applied to the decision accumulator when idle.
Values in [0, 1).  Smaller = forget motion faster."
  :type 'number)

(defcustom agent-shell-monome-spawn-on-empty-press t
  "When non-nil, pressing an unbound grid key spawns a new agent-shell.
The new shell is started in the project of the currently selected
window's buffer, falling back to its `default-directory'."
  :type 'boolean)

(defcustom agent-shell-monome-hold-to-talk t
  "When non-nil, holding a bound grid key records voice for that shell.
Press and hold a key mapped to an `agent-shell' buffer; once held past
`agent-shell-monome-hold-threshold' the bridge starts recording audio
via the `whisper' package, and on release it transcribes the recording
and inserts the text at that shell's prompt.  A quick tap keeps its
original meaning of switching to the buffer.

Requires the `whisper' package (https://github.com/natrys/whisper.el);
when it is unavailable the gesture degrades to a plain tap-to-switch."
  :type 'boolean)

(defcustom agent-shell-monome-hold-threshold 0.3
  "Seconds a grid key must be held before voice recording begins.
Holds shorter than this are treated as a tap (switch to the buffer);
longer holds start recording.  Audio spoken during this initial window
is not captured, so keep it small."
  :type 'number)

(defcustom agent-shell-monome-hold-to-talk-submit nil
  "When non-nil, submit the transcribed text instead of only inserting it.
With nil (the default) the transcription is inserted at the shell prompt
and left for you to review and send by hand (or via the arc decision
ring).  With non-nil it is sent immediately through `shell-maker-submit'."
  :type 'boolean)

(defcustom agent-shell-monome-grid-prefix "/monome-grid"
  "OSC address prefix configured on the grid at start.
Set per-device so messages can be disambiguated when grid and arc are
present together."
  :type 'string)

(defcustom agent-shell-monome-arc-prefix "/monome-arc"
  "OSC address prefix configured on the arc at start."
  :type 'string)

;;; State

(defvar agent-shell-monome--state nil
  "Global state alist, or nil when not running.

Top-level keys:
  :process              - UDP datagram process (listen + send).
  :serialosc-process    - serialosc child process when we manage it, else nil.
  :timer                - Repeating refresh timer.
  :tick                 - Tick counter.

  ;; Grid device
  :grid-id              - Reported device id.
  :grid-host            - Device IP.
  :grid-port            - Device OSC port.
  :grid-prefix          - OSC address prefix configured on the grid.
  :grid-width           - Grid width in keys.
  :grid-height          - Grid height in keys.
  :bindings             - Alist of ((X . Y) . BUFFER).
  :last-leds            - Alist of ((X . Y) . LEVEL).

  ;; Arc device
  :arc-id               - Reported device id.
  :arc-host             - Device IP.
  :arc-port             - Device OSC port.
  :arc-prefix           - OSC address prefix configured on the arc.
  :selected-index       - Index into (agent-shell-buffers) selected by ring 1.
  :scroll-accumulator   - Accumulated delta for ring 2.
  :decision-accumulator - Accumulated delta for ring 3.
  :tokens-spinner-phase - Ring 4 spinner head position, a float in [0, 64),
                          advanced each tick at a token-rate-scaled speed.
  :last-ring-leds       - Alist of ((N . X) . LEVEL) of last sent ring LED.

  ;; Permissions
  :pending-permissions  - FIFO queue (oldest first) of pending permissions;
                          ring 3 answers the head.  Each entry is an alist
                          of :respond :allow-id :reject-id :tool-call.
  :saved-responder      - Previous `agent-shell-permission-responder-function'.

  ;; Hold-to-talk (voice input via whisper)
  :htt-down-coord       - (X . Y) of the key whose hold gesture is active,
                          or nil.  Matched on release to resolve tap vs hold.
  :htt-timer            - Timer pending the hold threshold, or nil once it
                          has fired (recording started) or been cancelled.
  :htt-recording        - Buffer currently being recorded into, or nil.
                          Its grid key blinks while set.
  :htt-target           - Buffer to insert the transcription into, read by
                          the `whisper-after-transcription-hook' handler.")

;;; OSC codec
;;
;; Subset of OSC 1.0 needed for serialosc, grid, and arc control:
;; int32 and string arguments, no bundles, big-endian.

(defun agent-shell-monome--pack-int32 (n)
  "Pack integer N as a 4-byte big-endian OSC int32."
  (unibyte-string
   (logand (ash n -24) #xff)
   (logand (ash n -16) #xff)
   (logand (ash n -8) #xff)
   (logand n #xff)))

(defun agent-shell-monome--pack-string (s)
  "Pack string S as an OSC string (null terminated, 4-byte padded).
Always returns a unibyte string so concatenation stays byte-clean."
  (let* ((bytes (encode-coding-string s 'utf-8))
         (total-nulls (- 4 (mod (length bytes) 4))))
    (concat bytes (make-string total-nulls 0))))

(defun agent-shell-monome--encode-message (address args)
  "Encode an OSC message with ADDRESS and ARGS into a unibyte string.

ARGS is a list of (TYPE . VALUE) pairs.  TYPE is the symbol `i' (int32)
or `s' (string).

Example: (agent-shell-monome--encode-message
          \"/sys/port\" \\='((i . 7770)))"
  (let ((tag (concat "," (mapconcat (lambda (a) (symbol-name (car a))) args ""))))
    (apply #'concat
           (agent-shell-monome--pack-string address)
           (agent-shell-monome--pack-string tag)
           (mapcar (lambda (a)
                     (pcase (car a)
                       ('i (agent-shell-monome--pack-int32 (cdr a)))
                       ('s (agent-shell-monome--pack-string (cdr a)))
                       (_ (error "Unsupported OSC arg type: %S" (car a)))))
                   args))))

(defun agent-shell-monome--read-string (bytes offset)
  "Read an OSC string from BYTES starting at OFFSET.
Return (STRING . NEW-OFFSET)."
  (let ((end offset))
    (while (and (< end (length bytes))
                (/= (aref bytes end) 0))
      (setq end (1+ end)))
    (let* ((s (substring bytes offset end))
           (after-null (1+ end))
           (padded (+ after-null (mod (- 4 (mod after-null 4)) 4))))
      (cons (decode-coding-string s 'utf-8) padded))))

(defun agent-shell-monome--read-int32 (bytes offset)
  "Read an OSC int32 from BYTES at OFFSET.
Return (INT . NEW-OFFSET)."
  (let ((n (logior (ash (aref bytes offset) 24)
                   (ash (aref bytes (+ offset 1)) 16)
                   (ash (aref bytes (+ offset 2)) 8)
                   (aref bytes (+ offset 3)))))
    (when (>= n #x80000000)
      (setq n (- n #x100000000)))
    (cons n (+ offset 4))))

(defun agent-shell-monome--decode-message (bytes)
  "Decode an OSC message from BYTES.
Return an alist of the form

  ((:address . \"/foo/bar\")
   (:args . (1 \"two\" 3)))

Bundles and unsupported types yield nil."
  (when (and (> (length bytes) 0)
             (= (aref bytes 0) ?/))
    (let* ((addr (agent-shell-monome--read-string bytes 0))
           (tags (agent-shell-monome--read-string bytes (cdr addr)))
           (offset (cdr tags))
           (tag-string (car tags))
           args)
      (when (and (> (length tag-string) 0)
                 (= (aref tag-string 0) ?,))
        (catch 'done
          (dotimes (i (1- (length tag-string)))
            (pcase (aref tag-string (1+ i))
              (?i (let ((r (agent-shell-monome--read-int32 bytes offset)))
                    (push (car r) args)
                    (setq offset (cdr r))))
              (?s (let ((r (agent-shell-monome--read-string bytes offset)))
                    (push (car r) args)
                    (setq offset (cdr r))))
              (_ (throw 'done nil)))))
        (list (cons :address (car addr))
              (cons :args (nreverse args)))))))

;;; Networking

(defun agent-shell-monome--send (host port payload)
  "Send unibyte string PAYLOAD to HOST:PORT via the listener socket."
  (when-let ((proc (alist-get :process agent-shell-monome--state)))
    (set-process-datagram-address
     proc
     (apply #'vector (append (mapcar #'string-to-number
                                     (split-string host "\\." t))
                             (list port))))
    (process-send-string proc payload)))

(defun agent-shell-monome--send-message (host port address args)
  "Send OSC message ADDRESS with ARGS to HOST:PORT."
  (agent-shell-monome--send
   host port
   (agent-shell-monome--encode-message address args)))

(defun agent-shell-monome--send-grid (address args)
  "Send OSC ADDRESS with ARGS to the grid device."
  (when-let ((host (alist-get :grid-host agent-shell-monome--state))
             (port (alist-get :grid-port agent-shell-monome--state)))
    (agent-shell-monome--send-message host port address args)))

(defun agent-shell-monome--send-arc (address args)
  "Send OSC ADDRESS with ARGS to the arc device."
  (when-let ((host (alist-get :arc-host agent-shell-monome--state))
             (port (alist-get :arc-port agent-shell-monome--state)))
    (agent-shell-monome--send-message host port address args)))

;;; serialosc process management

(defun agent-shell-monome--serialosc-live-p ()
  "Return non-nil when this bridge owns a running serialosc process."
  (when-let ((proc (alist-get :serialosc-process agent-shell-monome--state)))
    (process-live-p proc)))

(defun agent-shell-monome--serialosc-port-in-use-p ()
  "Return non-nil when the serialosc discovery port is already bound.
Probe by trying to bind the discovery port; a bind failure means a
serialosc (or another listener) already holds it.  A second serialoscd
launched against a bound port exits immediately, so detecting this lets
the bridge adopt the running daemon instead of spawning a doomed
competitor and leaving discovery dependent on the pre-existing --
possibly orphaned or wedged -- instance."
  (condition-case _err
      (let ((proc (make-network-process
                   :name "agent-shell-monome-probe"
                   :type 'datagram
                   :family 'ipv4
                   :service agent-shell-monome-serialosc-port
                   :host agent-shell-monome-serialosc-host
                   :server t
                   :noquery t)))
        (delete-process proc)
        nil)
    (error t)))

(defun agent-shell-monome--start-serialosc ()
  "Launch serialosc as a child process when configured to manage it.
Return non-nil when a process was started by this call.  Does nothing --
and returns nil -- when `agent-shell-monome-manage-serialosc' is nil,
when one is already running under this bridge, when a serialosc is
already listening on the discovery port (we adopt it rather than launch
a second instance that would collide on the port and exit), or when the
executable cannot be found (in which case discovery falls back to any
externally managed serialosc)."
  (when (and agent-shell-monome-manage-serialosc
             (not (agent-shell-monome--serialosc-live-p)))
    (let ((program (executable-find agent-shell-monome-serialosc-command)))
      (cond
       ((agent-shell-monome--serialosc-port-in-use-p)
        (message "agent-shell-monome: serialosc already listening on %s:%d; \
using it"
                 agent-shell-monome-serialosc-host
                 agent-shell-monome-serialosc-port)
        nil)
       (program
        (let ((proc (make-process
                     :name "agent-shell-monome-serialosc"
                     :command (list program)
                     :buffer (get-buffer-create " *agent-shell-monome-serialosc*")
                     :noquery t
                     :connection-type 'pipe)))
          (setf (alist-get :serialosc-process agent-shell-monome--state) proc)
          (message "agent-shell-monome: started serialosc (%s)" program)
          t))
       (t
        (message "agent-shell-monome: %s not found on exec-path; \
relying on an existing serialosc"
                 agent-shell-monome-serialosc-command)
        nil)))))

(defun agent-shell-monome--stop-serialosc ()
  "Stop the serialosc child process if this bridge started one."
  (when-let ((proc (alist-get :serialosc-process agent-shell-monome--state)))
    (when (process-live-p proc)
      (ignore-errors (delete-process proc)))
    (setf (alist-get :serialosc-process agent-shell-monome--state) nil)))

;;; Discovery

(defun agent-shell-monome--discover ()
  "Ask serialosc to list devices.
serialosc will reply with one `/serialosc/device' message per device."
  (agent-shell-monome--send-message
   agent-shell-monome-serialosc-host
   agent-shell-monome-serialosc-port
   "/serialosc/list"
   `((s . ,agent-shell-monome-serialosc-host)
     (i . ,agent-shell-monome-listen-port))))

(defun agent-shell-monome--classify-device (type)
  "Return `grid' or `arc' for serialosc device TYPE string, or nil.
serialosc names arcs with \"arc\" in the type and every other monome
device it enumerates is a grid (e.g. \"monome 64\", \"monome 128\",
\"monome one\"), so match \"arc\" first and treat any remaining
\"monome\"/\"grid\" type as a grid."
  (let ((type (downcase (or type ""))))
    (cond
     ((string-match-p "arc" type) 'arc)
     ((or (string-match-p "grid" type)
          (string-match-p "monome" type))
      'grid)
     (t nil))))

(defun agent-shell-monome--on-device-found (id type port)
  "Record a discovered device with ID, TYPE, and OSC PORT."
  (pcase (agent-shell-monome--classify-device type)
    ('grid
     (message "agent-shell-monome: grid %s (%s) on port %d" id type port)
     (setf (alist-get :grid-id agent-shell-monome--state) id)
     (setf (alist-get :grid-host agent-shell-monome--state)
           agent-shell-monome-serialosc-host)
     (setf (alist-get :grid-port agent-shell-monome--state) port)
     (agent-shell-monome--send-grid
      "/sys/port" `((i . ,agent-shell-monome-listen-port)))
     (agent-shell-monome--send-grid
      "/sys/host" `((s . ,agent-shell-monome-serialosc-host)))
     (agent-shell-monome--send-grid
      "/sys/prefix" `((s . ,agent-shell-monome-grid-prefix)))
     (agent-shell-monome--send-grid "/sys/info" nil)
     (agent-shell-monome--clear-grid))
    ('arc
     (message "agent-shell-monome: arc %s (%s) on port %d" id type port)
     (setf (alist-get :arc-id agent-shell-monome--state) id)
     (setf (alist-get :arc-host agent-shell-monome--state)
           agent-shell-monome-serialosc-host)
     (setf (alist-get :arc-port agent-shell-monome--state) port)
     (agent-shell-monome--send-arc
      "/sys/port" `((i . ,agent-shell-monome-listen-port)))
     (agent-shell-monome--send-arc
      "/sys/host" `((s . ,agent-shell-monome-serialosc-host)))
     (agent-shell-monome--send-arc
      "/sys/prefix" `((s . ,agent-shell-monome-arc-prefix)))
     (agent-shell-monome--clear-arc))
    (_ (message "agent-shell-monome: ignoring unknown device type %S" type))))

;;; Incoming message dispatch

(defun agent-shell-monome--on-message (message)
  "Handle a decoded OSC MESSAGE alist."
  (let ((address (alist-get :address message))
        (args (alist-get :args message))
        (grid-prefix (or (alist-get :grid-prefix agent-shell-monome--state)
                         agent-shell-monome-grid-prefix))
        (arc-prefix (or (alist-get :arc-prefix agent-shell-monome--state)
                        agent-shell-monome-arc-prefix)))
    (cond
     ((string= address "/serialosc/device")
      (when (>= (length args) 3)
        (agent-shell-monome--on-device-found
         (nth 0 args) (nth 1 args) (nth 2 args))))
     ((string= address "/sys/size")
      ;; Only grid reports size; arc has no size reply.
      (when (>= (length args) 2)
        (setf (alist-get :grid-width agent-shell-monome--state) (nth 0 args))
        (setf (alist-get :grid-height agent-shell-monome--state) (nth 1 args))))
     ((string= address "/sys/prefix")
      ;; Confirmation echo from the device.  We do not rely on this --
      ;; we set the prefix proactively at discovery -- but track it for
      ;; debugging.
      nil)
     ((string= address (concat grid-prefix "/grid/key"))
      (when (>= (length args) 3)
        (agent-shell-monome--on-grid-key
         (nth 0 args) (nth 1 args) (nth 2 args))))
     ((string= address (concat arc-prefix "/enc/delta"))
      (when (>= (length args) 2)
        (agent-shell-monome--on-enc-delta
         (nth 0 args) (nth 1 args))))
     ((string= address (concat arc-prefix "/enc/key"))
      ;; Arc encoder push.  Currently unused but acknowledged so we
      ;; do not log a spurious decode error.
      nil))))

(defun agent-shell-monome--filter (_proc string)
  "Process filter: decode OSC packet STRING and dispatch."
  (condition-case err
      (when-let ((msg (agent-shell-monome--decode-message string)))
        (agent-shell-monome--on-message msg))
    (error (message "agent-shell-monome: decode error: %S" err))))

;;; Grid: key handling, coord mapping, LED rendering

(defun agent-shell-monome--project-for-buffer (buffer)
  "Return a string identifying BUFFER's project root, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (featurep 'project)
               (when-let* ((proj (project-current))
                           (root (project-root proj)))
                 (expand-file-name root)))
          (expand-file-name default-directory)))))

(defun agent-shell-monome--current-project-root ()
  "Best-effort root for a new agent-shell spawned from the grid.
Prefer the project of the selected window's buffer; fall back to its
`default-directory'."
  (with-current-buffer (window-buffer (selected-window))
    (or (and (featurep 'project)
             (when-let* ((proj (project-current))
                         (root (project-root proj)))
               (expand-file-name root)))
        (expand-file-name default-directory))))

(defun agent-shell-monome--spawn-shell-here (&optional col)
  "Start a new agent-shell, if possible.
With COL, root it at the project that already owns grid column COL, so an
empty press in a project's column spawns into that project rather than
wherever Emacs focus happens to be.  Fall back to the selected window's
project when COL is nil or owns no project yet."
  (when (fboundp 'agent-shell--new-shell)
    (let ((root (or (and col (agent-shell-monome--project-for-column col))
                    (agent-shell-monome--current-project-root))))
      (condition-case err
          (agent-shell--new-shell :location root)
        (error
         (message "agent-shell-monome: spawn failed: %S" err))))))

(defun agent-shell-monome--on-grid-key (x y state)
  "Handle a grid key event at (X, Y) with STATE (1=down, 0=up)."
  (if (= state 1)
      (agent-shell-monome--on-grid-key-down x y)
    (agent-shell-monome--on-grid-key-up x y)))

(defun agent-shell-monome--on-grid-key-down (x y)
  "Handle a grid key press at (X, Y).
Switches to (or spawns) the buffer at that coordinate as before, and --
for a bound, live buffer -- arms the hold-to-talk timer so a sustained
hold begins recording."
  (let ((buffer (alist-get (cons x y)
                           (alist-get :bindings agent-shell-monome--state)
                           nil nil #'equal)))
    (cond
     ((and buffer (buffer-live-p buffer))
      (pop-to-buffer buffer)
      (agent-shell-monome--maybe-arm-hold (cons x y) buffer))
     (buffer
      (agent-shell-monome--prune-bindings))
     (agent-shell-monome-spawn-on-empty-press
      (agent-shell-monome--spawn-shell-here x)))))

(defun agent-shell-monome--on-grid-key-up (x y)
  "Handle a grid key release at (X, Y).
Resolves the hold-to-talk gesture armed on key-down: a release before
the threshold is a tap (the buffer switch already happened, nothing more
to do); a release after it stops the in-progress recording, which
triggers transcription and insertion."
  (when (equal (cons x y)
               (alist-get :htt-down-coord agent-shell-monome--state))
    (if-let ((timer (alist-get :htt-timer agent-shell-monome--state)))
        ;; Released before the threshold: a tap.  Recording never began.
        (progn
          (cancel-timer timer)
          (setf (alist-get :htt-timer agent-shell-monome--state) nil)
          (setf (alist-get :htt-down-coord agent-shell-monome--state) nil))
      ;; Held past the threshold: recording is running -- stop it.
      (agent-shell-monome--htt-end))))

(defun agent-shell-monome--maybe-arm-hold (coord buffer)
  "Arm the hold-to-talk timer for COORD targeting BUFFER.
No-op unless `agent-shell-monome-hold-to-talk' is on, the `whisper'
package is available, and no other hold gesture is already in flight
\(only one recording runs at a time, since whisper is single-instance)."
  (when (and agent-shell-monome-hold-to-talk
             (fboundp 'whisper-run)
             (null (alist-get :htt-down-coord agent-shell-monome--state))
             (null (alist-get :htt-recording agent-shell-monome--state)))
    (setf (alist-get :htt-down-coord agent-shell-monome--state) coord)
    (setf (alist-get :htt-timer agent-shell-monome--state)
          (run-at-time agent-shell-monome-hold-threshold nil
                       #'agent-shell-monome--htt-begin buffer))))

(defun agent-shell-monome--htt-begin (buffer)
  "Begin recording audio for hold-to-talk into BUFFER.
Called from the hold timer once a key has been held past the threshold.
Records via `whisper-run'; the transcription is routed back to BUFFER by
`agent-shell-monome--whisper-transcription-handler'."
  (when agent-shell-monome--state
    (setf (alist-get :htt-timer agent-shell-monome--state) nil)
    (cond
     ((not (buffer-live-p buffer))
      (setf (alist-get :htt-down-coord agent-shell-monome--state) nil))
     ((not (fboundp 'whisper-run))
      (message "agent-shell-monome: whisper not available; install whisper.el")
      (setf (alist-get :htt-down-coord agent-shell-monome--state) nil))
     ((or (and (fboundp 'whisper-recording-p) (whisper-recording-p))
          (and (fboundp 'whisper-transcribing-p) (whisper-transcribing-p)))
      (message "agent-shell-monome: whisper busy; ignoring hold")
      (setf (alist-get :htt-down-coord agent-shell-monome--state) nil))
     (t
      (setf (alist-get :htt-target agent-shell-monome--state) buffer)
      (setf (alist-get :htt-recording agent-shell-monome--state) buffer)
      (condition-case err
          ;; Bind `whisper-insert-text-at-point' off for the duration of
          ;; the start call: it skips whisper's read-only buffer guard
          ;; (which would otherwise error if the timer fires while a
          ;; read-only buffer is current) and avoids stashing a point
          ;; marker we never use -- our hook inserts into the target
          ;; buffer's prompt itself.
          (let ((whisper-insert-text-at-point nil))
            (whisper-run))
        (error
         (message "agent-shell-monome: failed to start recording: %S" err)
         (setf (alist-get :htt-target agent-shell-monome--state) nil)
         (setf (alist-get :htt-recording agent-shell-monome--state) nil)
         (setf (alist-get :htt-down-coord agent-shell-monome--state) nil)))))))

(defun agent-shell-monome--htt-end ()
  "Stop the in-progress hold-to-talk recording.
A second `whisper-run' toggles recording off, which kicks off
transcription; the resulting text is inserted by
`agent-shell-monome--whisper-transcription-handler'."
  (setf (alist-get :htt-recording agent-shell-monome--state) nil)
  (setf (alist-get :htt-down-coord agent-shell-monome--state) nil)
  (when (and (fboundp 'whisper-recording-p) (whisper-recording-p))
    (condition-case err
        (whisper-run)
      (error (message "agent-shell-monome: failed to stop recording: %S" err)))))

(defun agent-shell-monome--insert-transcription (buffer text)
  "Insert TEXT at BUFFER's shell prompt, submitting when configured.
With `agent-shell-monome-hold-to-talk-submit' non-nil the text is sent
via `shell-maker-submit'; otherwise it is appended to the prompt input
for review."
  (with-current-buffer buffer
    (if (and agent-shell-monome-hold-to-talk-submit
             (fboundp 'shell-maker-submit))
        (shell-maker-submit :input text)
      (goto-char (point-max))
      (insert text))))

(defun agent-shell-monome--whisper-transcription-handler ()
  "Route a whisper transcription to the hold-to-talk target buffer.
Added to `whisper-after-transcription-hook' and run in the whisper
stdout buffer with the transcribed text as its contents.  When a
hold-to-talk recording is outstanding, grab the text, erase the stdout
buffer so whisper skips its own point insertion, and insert the text at
the target buffer's prompt.  A no-op otherwise, leaving ordinary
`whisper-run' usage untouched."
  (when-let ((target (and agent-shell-monome--state
                          (alist-get :htt-target agent-shell-monome--state))))
    (let ((text (string-trim (buffer-string))))
      (setf (alist-get :htt-target agent-shell-monome--state) nil)
      ;; Emptying the stdout buffer leaves whisper nothing to insert at
      ;; point, so the transcription lands only where we put it.
      (erase-buffer)
      (when (and (not (string-empty-p text)) (buffer-live-p target))
        (condition-case err
            (agent-shell-monome--insert-transcription target text)
          (error (message "agent-shell-monome: insert failed: %S" err)))))))

(defun agent-shell-monome--coord-for-slot (slot)
  "Convert SLOT index to a (X . Y) cons in row-major order."
  (let ((w (or (alist-get :grid-width agent-shell-monome--state) 8)))
    (cons (mod slot w) (/ slot w))))

(defun agent-shell-monome--slot-for-coord (coord)
  "Convert (X . Y) COORD to a slot index in row-major order."
  (let ((w (or (alist-get :grid-width agent-shell-monome--state) 8)))
    (+ (car coord) (* (cdr coord) w))))

(defun agent-shell-monome--total-slots ()
  "Return the total number of slots available on the grid."
  (* (or (alist-get :grid-width agent-shell-monome--state) 8)
     (or (alist-get :grid-height agent-shell-monome--state) 8)))

(defun agent-shell-monome--prune-bindings ()
  "Drop entries for dead buffers and clear their LEDs."
  (let* ((bindings (alist-get :bindings agent-shell-monome--state))
         (alive (seq-filter (lambda (entry) (buffer-live-p (cdr entry)))
                            bindings))
         (dead (seq-difference bindings alive)))
    (dolist (entry dead)
      (agent-shell-monome--set-grid-led (car (car entry)) (cdr (car entry)) 0))
    (setf (alist-get :bindings agent-shell-monome--state) alive)))

(defun agent-shell-monome--column-for-project (project)
  "Return the grid column assigned to PROJECT, allocating one if needed.
Returns nil when every column is already taken by a different project."
  (let ((cols (alist-get :project-columns agent-shell-monome--state)))
    (or (alist-get project cols nil nil #'equal)
        (let* ((w (or (alist-get :grid-width agent-shell-monome--state) 8))
               (used (mapcar #'cdr cols))
               (col 0))
          (while (and (< col w) (member col used))
            (setq col (1+ col)))
          (when (< col w)
            (setf (alist-get project cols nil nil #'equal) col)
            (setf (alist-get :project-columns agent-shell-monome--state) cols)
            col)))))

(defun agent-shell-monome--project-for-column (col)
  "Return the project root currently assigned to grid column COL, or nil."
  (car (rassoc col (alist-get :project-columns agent-shell-monome--state))))

(defun agent-shell-monome--prune-project-columns ()
  "Drop column assignments whose project has no remaining buffers."
  (let* ((bindings (alist-get :bindings agent-shell-monome--state))
         (active (delete-dups
                  (delq nil
                        (mapcar (lambda (e)
                                  (agent-shell-monome--project-for-buffer
                                   (cdr e)))
                                bindings))))
         (cols (alist-get :project-columns agent-shell-monome--state)))
    (setf (alist-get :project-columns agent-shell-monome--state)
          (seq-filter (lambda (entry)
                        (member (car entry) active))
                      cols))))

(defun agent-shell-monome--assign-new-buffers ()
  "Give each known agent-shell buffer a grid coordinate.
Buffers sharing a project stack vertically in the same column.  Each
project gets the lowest free column; new buffers within a project take
the lowest free row in that column."
  (let* ((bindings (alist-get :bindings agent-shell-monome--state))
         (bound (mapcar #'cdr bindings))
         (h (or (alist-get :grid-height agent-shell-monome--state) 8)))
    (dolist (buffer (agent-shell-buffers))
      (unless (memq buffer bound)
        (when-let* ((project (agent-shell-monome--project-for-buffer buffer))
                    (col (agent-shell-monome--column-for-project project)))
          (let* ((rows-in-col
                  (mapcar (lambda (e) (cdr (car e)))
                          (seq-filter (lambda (e) (= (car (car e)) col))
                                      bindings)))
                 (row 0))
            (while (and (< row h) (member row rows-in-col))
              (setq row (1+ row)))
            (when (< row h)
              (push (cons (cons col row) buffer) bindings))))))
    (setf (alist-get :bindings agent-shell-monome--state) bindings)))

(defun agent-shell-monome--set-grid-led (x y level)
  "Set grid LED at (X, Y) to LEVEL (0-15), deduping against last sent."
  (let* ((coord (cons x y))
         (last (alist-get :last-leds agent-shell-monome--state))
         (prefix (or (alist-get :grid-prefix agent-shell-monome--state)
                     agent-shell-monome-grid-prefix)))
    (unless (equal level (alist-get coord last nil nil #'equal))
      (agent-shell-monome--send-grid
       (concat prefix "/grid/led/level/set")
       `((i . ,x) (i . ,y) (i . ,level)))
      (setf (alist-get coord last nil nil #'equal) level)
      (setf (alist-get :last-leds agent-shell-monome--state) last))))

(defun agent-shell-monome--busy-level ()
  "Return the current pulsing busy level based on tick counter."
  (let* ((tick (or (alist-get :tick agent-shell-monome--state) 0))
         (levels agent-shell-monome-busy-levels))
    (nth (mod tick (length levels)) levels)))

(defun agent-shell-monome--level-for-buffer (buffer)
  "Return the LED level appropriate for BUFFER's current status."
  (pcase (agent-shell-status :shell-buffer buffer)
    ('blocked agent-shell-monome-level-blocked)
    ('busy (agent-shell-monome--busy-level))
    (_ agent-shell-monome-level-idle)))

(defun agent-shell-monome--clear-grid ()
  "Turn off every grid LED."
  (let ((w (or (alist-get :grid-width agent-shell-monome--state) 8))
        (h (or (alist-get :grid-height agent-shell-monome--state) 8)))
    (dotimes (y h)
      (dotimes (x w)
        (agent-shell-monome--set-grid-led x y 0)))))

;;; Arc: ring rendering primitives

(defun agent-shell-monome--set-ring-all (n level)
  "Set all 64 LEDs on ring N to LEVEL.
Resets cached per-LED state for ring N so the next individual set
re-sends regardless of the cached value."
  (let ((prefix (or (alist-get :arc-prefix agent-shell-monome--state)
                    agent-shell-monome-arc-prefix)))
    (agent-shell-monome--send-arc
     (concat prefix "/ring/all") `((i . ,n) (i . ,level)))
    ;; Invalidate per-LED cache entries for this ring.
    (let ((last (alist-get :last-ring-leds agent-shell-monome--state)))
      (setf (alist-get :last-ring-leds agent-shell-monome--state)
            (seq-remove (lambda (entry) (= (car (car entry)) n)) last)))
    ;; And the whole-ring map cache, so the next map re-sends for this ring.
    (setf (alist-get :last-ring-maps agent-shell-monome--state)
          (assq-delete-all n (alist-get :last-ring-maps
                                        agent-shell-monome--state)))))

(defun agent-shell-monome--set-ring-map (n levels)
  "Set all 64 LEDs of ring N from LEVELS in a single /ring/map message.
LEVELS is a 64-element sequence of brightnesses (each clamped to 0-15).

Sending the whole ring as one OSC packet -- instead of up to 64
/ring/set packets -- keeps the arc's USB write path from overflowing:
libmonome drops writes on EAGAIN, and at the 10Hz refresh a 256-packet
burst left the later-drawn rings (decision, tokens) permanently
half-lit.  One atomic packet per ring also self-corrects, since each
refresh restates the ring's full state.  Deduped against the last map
sent for ring N so an unchanged ring stays quiet."
  (let* ((vec (vconcat levels))
         (last (alist-get :last-ring-maps agent-shell-monome--state))
         (prefix (or (alist-get :arc-prefix agent-shell-monome--state)
                     agent-shell-monome-arc-prefix)))
    (unless (equal vec (alist-get n last))
      (agent-shell-monome--send-arc
       (concat prefix "/ring/map")
       (cons (cons 'i n)
             (mapcar (lambda (l) (cons 'i (max 0 (min 15 (truncate l)))))
                     (append vec nil))))
      (setf (alist-get n last) vec)
      (setf (alist-get :last-ring-maps agent-shell-monome--state) last))))

(defun agent-shell-monome--set-ring-led (n x level)
  "Set LED X (0-63) on ring N to LEVEL (0-15), deduping."
  (let* ((key (cons n (mod x 64)))
         (last (alist-get :last-ring-leds agent-shell-monome--state))
         (prefix (or (alist-get :arc-prefix agent-shell-monome--state)
                     agent-shell-monome-arc-prefix)))
    (unless (equal level (alist-get key last nil nil #'equal))
      (agent-shell-monome--send-arc
       (concat prefix "/ring/set")
       `((i . ,n) (i . ,(mod x 64)) (i . ,level)))
      (setf (alist-get key last nil nil #'equal) level)
      (setf (alist-get :last-ring-leds agent-shell-monome--state) last))))

(defun agent-shell-monome--clear-arc ()
  "Turn off every LED on every ring."
  (dotimes (n agent-shell-monome-arc-encoder-count)
    (agent-shell-monome--set-ring-all n 0)))

;;; Arc: encoder dispatch

(defun agent-shell-monome--on-enc-delta (n delta)
  "Handle an encoder DELTA on encoder N."
  (cond
   ((= n agent-shell-monome-arc-selector-encoder)
    (agent-shell-monome--selector-on-delta delta))
   ((= n agent-shell-monome-arc-scroll-encoder)
    (agent-shell-monome--scroll-on-delta delta))
   ((= n agent-shell-monome-arc-decision-encoder)
    (agent-shell-monome--decision-on-delta delta))
   ((= n agent-shell-monome-arc-tokens-encoder)
    (agent-shell-monome--effort-on-delta delta))))

;;; Arc: ring 1 (buffer selector)

(defun agent-shell-monome--indexed-buffer ()
  "Return the agent-shell buffer the ring-1 dial points at, or nil.
This reflects `:selected-index' only, independent of Emacs focus, so the
selector can advance the dial without it being snapped back to the
focused window mid-turn."
  (let* ((buffers (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (n (length buffers)))
    (when (> n 0)
      (nth (mod (or (alist-get :selected-index agent-shell-monome--state) 0) n)
           buffers))))

(defun agent-shell-monome--selected-buffer ()
  "Return the currently selected agent-shell buffer, or nil.
When the selected Emacs window is showing an agent-shell buffer, treat
that as the selection and sync `:selected-index' to it -- so the arc's
scroll/effort rings and ring 1's wedge act on whatever shell you are
actually looking at, even after a grid tap or an ordinary buffer switch
\(neither of which moves the ring-1 dial).  Otherwise fall back to the
buffer the dial points at."
  (let* ((buffers (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (focused-pos (and buffers
                           (seq-position buffers
                                         (window-buffer (selected-window))
                                         #'eq))))
    (if focused-pos
        (progn
          (setf (alist-get :selected-index agent-shell-monome--state)
                focused-pos)
          (nth focused-pos buffers))
      (agent-shell-monome--indexed-buffer))))

(defun agent-shell-monome--show-selected-buffer ()
  "Display the dial's selected buffer in Emacs.
Mirrors the grid key gesture (`pop-to-buffer') so turning the selector
dial switches the visible/active agent-shell, not just the arc ring's
pointer wedge.  Uses the dial position (not the focus-following
selection) so advancing the dial actually moves off the focused buffer.
Errors are swallowed since this runs from the OSC process filter."
  (when-let ((buffer (agent-shell-monome--indexed-buffer)))
    (when (buffer-live-p buffer)
      (condition-case err
          (pop-to-buffer buffer)
        (error (message "agent-shell-monome: show buffer failed: %S" err))))))

(defun agent-shell-monome--selector-on-delta (delta)
  "Advance buffer selection by DELTA encoder ticks.
When the selection lands on a different buffer, display it so the dial
changes which agent-shell is shown in Emacs."
  (let* ((acc (+ (or (alist-get :selector-accumulator
                                agent-shell-monome--state) 0)
                 delta))
         (step agent-shell-monome-arc-selector-ticks-per-step)
         (buffers (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (n (length buffers))
         (before (when (> n 0)
                   (mod (or (alist-get :selected-index
                                       agent-shell-monome--state) 0)
                        n))))
    (when (> n 0)
      (while (>= acc step)
        (setf (alist-get :selected-index agent-shell-monome--state)
              (mod (1+ (or (alist-get :selected-index agent-shell-monome--state)
                           0))
                   n))
        (setq acc (- acc step)))
      (while (<= acc (- step))
        (setf (alist-get :selected-index agent-shell-monome--state)
              (mod (1- (or (alist-get :selected-index agent-shell-monome--state)
                           0))
                   n))
        (setq acc (+ acc step)))
      (let ((after (mod (or (alist-get :selected-index
                                       agent-shell-monome--state) 0)
                        n)))
        (unless (eql before after)
          (agent-shell-monome--show-selected-buffer))))
    (setf (alist-get :selector-accumulator agent-shell-monome--state) acc)))

(defun agent-shell-monome--selector-positions ()
  "Return a list of (LED-INDEX . BUFFER) marker positions for ring 1."
  (let* ((buffers (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (n (length buffers)))
    (when (> n 0)
      (seq-map-indexed
       (lambda (buf i)
         (cons (mod (round (/ (* 64.0 i) n)) 64) buf))
       buffers))))

(defun agent-shell-monome--render-selector ()
  "Draw ring 1: indicator marks per buffer + pointer wedge at selection."
  (let* ((n agent-shell-monome-arc-selector-encoder)
         (positions (agent-shell-monome--selector-positions))
         (selected (agent-shell-monome--selected-buffer))
         (leds (make-vector 64 0)))
    (dolist (entry positions)
      (aset leds (car entry)
            (agent-shell-monome--level-for-buffer (cdr entry))))
    ;; Pointer wedge: 3 LEDs at the selected mark, brighter.
    (when-let ((mark (car (rassq selected positions))))
      (dolist (off '(-1 0 1))
        (aset leds (mod (+ mark off) 64) 15)))
    (agent-shell-monome--set-ring-map n leds)))

;;; Arc: ring 2 (scroll)

(defun agent-shell-monome--scroll-target-window (buffer)
  "Return a window displaying BUFFER, or nil.
Prefer the selected window when it is the one showing BUFFER, so scrolling
never lands on a stray background copy of the same buffer on another
window or frame."
  (when (buffer-live-p buffer)
    (if (eq (window-buffer (selected-window)) buffer)
        (selected-window)
      (get-buffer-window buffer t))))

(defun agent-shell-monome--scroll-on-delta (delta)
  "Scroll the selected buffer by DELTA encoder ticks."
  (let* ((buffer (agent-shell-monome--selected-buffer))
         (acc (+ (or (alist-get :scroll-accumulator agent-shell-monome--state) 0)
                 delta))
         (step agent-shell-monome-arc-scroll-ticks-per-line)
         (lines 0))
    (when buffer
      (while (>= acc step)
        (setq lines (1+ lines))
        (setq acc (- acc step)))
      (while (<= acc (- step))
        (setq lines (1- lines))
        (setq acc (+ acc step)))
      (unless (zerop lines)
        (with-current-buffer buffer
          (let ((win (agent-shell-monome--scroll-target-window buffer)))
            (if win
                (with-selected-window win
                  (condition-case nil
                      (if (> lines 0)
                          (scroll-up lines)
                        (scroll-down (- lines)))
                    (beginning-of-buffer (goto-char (point-min)))
                    (end-of-buffer (goto-char (point-max)))))
              (forward-line lines))))))
    (setf (alist-get :scroll-accumulator agent-shell-monome--state) acc)))

(defun agent-shell-monome--scroll-position-fraction (buffer)
  "Return a number in [0, 1] for window-start position in BUFFER, or 0.5."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((win (agent-shell-monome--scroll-target-window buffer))
             (pos (if win (window-start win) (point)))
             (size (max 1 (- (point-max) (point-min)))))
        (max 0.0 (min 1.0 (/ (float (- pos (point-min))) size)))))))

(defun agent-shell-monome--render-scroll ()
  "Draw ring 2: a dim track plus a bright dot tracking scroll position."
  (let* ((n agent-shell-monome-arc-scroll-encoder)
         (buffer (agent-shell-monome--selected-buffer))
         (frac (if buffer
                   (agent-shell-monome--scroll-position-fraction buffer)
                 0.5))
         (pos (mod (round (* frac 64)) 64))
         (leds (make-vector 64 0)))
    (dotimes (x 64)
      (when (zerop (mod x 8)) (aset leds x 1)))
    (dolist (off '(-1 0 1))
      (aset leds (mod (+ pos off) 64) 12))
    (agent-shell-monome--set-ring-map n leds)))

;;; Arc: ring 3 (decision)

(defun agent-shell-monome--decision-on-delta (delta)
  "Accumulate DELTA toward a yes/no decision and fire if threshold crossed."
  (let* ((acc (+ (or (alist-get :decision-accumulator agent-shell-monome--state)
                     0)
                 delta))
         (threshold agent-shell-monome-arc-decision-threshold))
    (cond
     ((>= acc threshold)
      (agent-shell-monome--decide 'allow)
      (setq acc 0))
     ((<= acc (- threshold))
      (agent-shell-monome--decide 'reject)
      (setq acc 0)))
    (setf (alist-get :decision-accumulator agent-shell-monome--state) acc)))

(defun agent-shell-monome--decay-decision-accumulator ()
  "Decay the decision accumulator toward zero (called each tick)."
  (let* ((acc (or (alist-get :decision-accumulator agent-shell-monome--state) 0))
         (decayed (truncate (* acc agent-shell-monome-arc-decision-decay))))
    (setf (alist-get :decision-accumulator agent-shell-monome--state) decayed)))

(defun agent-shell-monome--decide (choice)
  "Respond to the oldest pending permission with CHOICE (`allow' or `reject').
Permissions are answered oldest-first: this pops the head of the
`:pending-permissions' queue, so a second prompt arriving while the first
is unanswered waits its turn instead of stealing the dial."
  (when-let* ((queue (alist-get :pending-permissions agent-shell-monome--state))
              (pending (car queue))
              (respond (alist-get :respond pending))
              (option-id (alist-get (if (eq choice 'allow) :allow-id :reject-id)
                                    pending)))
    (condition-case err
        (funcall respond option-id)
      (error (message "agent-shell-monome: respond failed: %S" err)))
    (setf (alist-get :pending-permissions agent-shell-monome--state) (cdr queue))
    (message "agent-shell-monome: sent %s" choice)))

(defun agent-shell-monome--render-decision ()
  "Draw ring 3: left half fills on negative accumulator, right on positive.
The ring is \"armed\" whenever a prompt is waiting; small dots clockwise of
12 o'clock count how many more are queued behind the one being answered."
  (let* ((n agent-shell-monome-arc-decision-encoder)
         (acc (or (alist-get :decision-accumulator agent-shell-monome--state)
                  0))
         (threshold agent-shell-monome-arc-decision-threshold)
         (fraction (min 1.0 (/ (abs (float acc)) (max 1 threshold))))
         (span (max 1 (round (* 31 fraction))))
         (queue (alist-get :pending-permissions agent-shell-monome--state))
         (pending (car queue))
         (base (if pending 3 0))
         (fill (round (+ 4 (* 11 fraction))))
         (leds (make-vector 64 base)))
    (when pending
      ;; Highlight the 12 o'clock (top) and 6 o'clock (bottom) lightly,
      ;; so the user can see the ring is "armed".
      (aset leds 0 6)
      (aset leds 32 6)
      ;; One dim dot per extra queued prompt, clockwise of 12 o'clock, so a
      ;; backlog reads as a small stack instead of being invisible.
      (dotimes (i (min 6 (1- (length queue))))
        (aset leds (* 2 (1+ i)) 4)))
    (cond
     ((> acc 0)
      ;; Right half: LEDs 1..31 fill clockwise from 0.
      (dotimes (i span)
        (aset leds (mod (1+ i) 64) fill)))
     ((< acc 0)
      ;; Left half: LEDs 63..33 fill counterclockwise from 0.
      (dotimes (i span)
        (aset leds (mod (- 64 (1+ i)) 64) fill))))
    (agent-shell-monome--set-ring-map n leds)))

;;; Permission stash via responder function

(defun agent-shell-monome--responder (permission)
  "Enqueue PERMISSION's respond closure so ring 3 can answer it later.
Appended to the tail of `:pending-permissions' so prompts are answered in
arrival order (oldest first).  Returns nil so the normal interactive UI is
still shown.  If a previous responder existed, chain to it and honor its
return value."
  (let* ((options (alist-get :options permission))
         (tool-call (alist-get :tool-call permission))
         (allow-id (when-let ((o (seq-find
                                  (lambda (x)
                                    (equal (alist-get :kind x) "allow_once"))
                                  options)))
                     (alist-get :option-id o)))
         (reject-id (when-let ((o (seq-find
                                   (lambda (x)
                                     (equal (alist-get :kind x) "reject_once"))
                                   options)))
                      (alist-get :option-id o)))
         (respond (alist-get :respond permission))
         (consumed nil)
         (idempotent (lambda (option-id)
                       (unless consumed
                         (setq consumed t)
                         (funcall respond option-id)))))
    (setf (alist-get :pending-permissions agent-shell-monome--state)
          (append (alist-get :pending-permissions agent-shell-monome--state)
                  (list (list (cons :respond idempotent)
                              (cons :allow-id allow-id)
                              (cons :reject-id reject-id)
                              (cons :tool-call tool-call)))))
    (when-let ((prev (alist-get :saved-responder agent-shell-monome--state))
               ((functionp prev)))
      (funcall prev permission))))

;;; Arc: ring 4 (token rate + effort control)
;;
;; The wheel displays token usage rate across all known agent-shell
;; buffers (visual output), and -- when the agent advertises a
;; thought_level config option -- turning it nudges the selected
;; buffer's effort level up or down (input).

(defun agent-shell-monome--ensure-tokens-subscribed (buffer)
  "Ensure we are subscribed to BUFFER's turn-complete event."
  (when (and (buffer-live-p buffer)
             (fboundp 'agent-shell-subscribe-to)
             (not (alist-get buffer
                             (alist-get :tokens-subscriptions
                                        agent-shell-monome--state))))
    (let ((token (agent-shell-subscribe-to
                  :shell-buffer buffer
                  :event 'turn-complete
                  :on-event (lambda (event)
                              (agent-shell-monome--on-turn-complete
                               buffer event)))))
      (let ((subs (alist-get :tokens-subscriptions agent-shell-monome--state)))
        (setf (alist-get buffer subs) token)
        (setf (alist-get :tokens-subscriptions agent-shell-monome--state) subs)))))

(defun agent-shell-monome--on-turn-complete (buffer event)
  "Record token delta from a turn-complete EVENT on BUFFER."
  (let* ((data (alist-get :data event))
         (usage (alist-get :usage data))
         (total (or (alist-get :total-tokens usage) 0))
         (last-totals (alist-get :tokens-last-totals
                                 agent-shell-monome--state))
         (last (or (alist-get buffer last-totals) 0))
         (delta (max 0 (- total last))))
    (when (> delta 0)
      (push (cons (float-time) delta)
            (alist-get :tokens-history agent-shell-monome--state)))
    (setf (alist-get buffer last-totals) total)
    (setf (alist-get :tokens-last-totals agent-shell-monome--state)
          last-totals)))

(defun agent-shell-monome--prune-tokens-history ()
  "Drop history entries older than the moving window."
  (let* ((now (float-time))
         (cutoff (- now agent-shell-monome-arc-tokens-window-seconds))
         (history (alist-get :tokens-history agent-shell-monome--state)))
    (setf (alist-get :tokens-history agent-shell-monome--state)
          (seq-filter (lambda (entry) (>= (car entry) cutoff)) history))))

(defun agent-shell-monome--tokens-rate ()
  "Return current token rate (tokens/second) over the moving window."
  (let* ((history (alist-get :tokens-history agent-shell-monome--state))
         (sum (seq-reduce #'+ (mapcar #'cdr history) 0))
         (window agent-shell-monome-arc-tokens-window-seconds))
    (/ (float sum) (max 1.0 window))))

(defun agent-shell-monome--effort-info ()
  "Return (LEVELS CURRENT-INDEX) for the selected buffer, or nil.
LEVELS is the list of available thought-levels (each an alist with
`:value' and `:name').  CURRENT-INDEX is the position of the current
level in LEVELS."
  (when-let* ((buffer (agent-shell-monome--selected-buffer))
              ((fboundp 'agent-shell--state))
              ((fboundp 'agent-shell--get-available-thought-levels))
              ((fboundp 'agent-shell--current-thought-level-id))
              (state (with-current-buffer buffer (agent-shell--state)))
              (levels (agent-shell--get-available-thought-levels state)))
    (let* ((current-id (agent-shell--current-thought-level-id state))
           (index (or (seq-position
                       levels current-id
                       (lambda (level id) (equal (alist-get :value level) id)))
                      0)))
      (list levels index))))

(defun agent-shell-monome--effort-on-delta (delta)
  "Step the selected buffer's thought-level by accumulated DELTA."
  (let* ((acc (+ (or (alist-get :effort-accumulator agent-shell-monome--state) 0)
                 delta))
         (step agent-shell-monome-arc-effort-ticks-per-step)
         (info (agent-shell-monome--effort-info)))
    (when info
      (let* ((levels (nth 0 info))
             (index (nth 1 info))
             (target index))
        (while (>= acc step)
          (setq target (min (1- (length levels)) (1+ target)))
          (setq acc (- acc step)))
        (while (<= acc (- step))
          (setq target (max 0 (1- target)))
          (setq acc (+ acc step)))
        (unless (= target index)
          (when (fboundp 'agent-shell--config-option-set-thought-level-id)
            (with-current-buffer (agent-shell-monome--selected-buffer)
              (condition-case err
                  (agent-shell--config-option-set-thought-level-id
                   :thought-level-id (alist-get :value (nth target levels)))
                (error (message "agent-shell-monome: effort change failed: %S"
                                err))))))))
    (setf (alist-get :effort-accumulator agent-shell-monome--state) acc)))

(defun agent-shell-monome--advance-spinner-phase (saturation)
  "Advance and return the ring 4 spinner head position for this tick.
Speed scales with SATURATION (the token rate as a fraction in [0, 1] of
`agent-shell-monome-arc-tokens-max-rate'): a saturation of 0 parks the
wedge, 1 spins it at `agent-shell-monome-arc-tokens-spinner-max-rps'
revolutions per second.  The phase is kept as a float in [0, 64) so slow
speeds accumulate instead of rounding away to a standstill."
  (let* ((rps (* agent-shell-monome-arc-tokens-spinner-max-rps saturation))
         (advance (* rps 64.0 agent-shell-monome-tick-seconds))
         (phase (mod (+ (or (alist-get :tokens-spinner-phase
                                       agent-shell-monome--state)
                            0.0)
                        advance)
                     64.0)))
    (setf (alist-get :tokens-spinner-phase agent-shell-monome--state) phase)
    phase))

(defun agent-shell-monome--render-tokens ()
  "Draw ring 4: a token-rate spinner plus effort markers.
The wedge is a comet -- a bright head trailing a fading tail -- rotating
at a speed proportional to the rolling token rate (faster = more tokens,
still when idle).  Higher rates also lengthen and brighten the comet.
Effort-level markers are overlaid so the ring still doubles as the
thought-level control."
  (let* ((n agent-shell-monome-arc-tokens-encoder)
         (saturation (min 1.0 (/ (agent-shell-monome--tokens-rate)
                                 (max 1.0 agent-shell-monome-arc-tokens-max-rate))))
         (phase (agent-shell-monome--advance-spinner-phase saturation))
         (head (mod (round phase) 64))
         (head-level (min 15 (round (+ 3 (* 12 saturation)))))
         (tail (max 2 (round (+ 2 (* 6 saturation)))))
         (info (agent-shell-monome--effort-info))
         (leds (make-vector 64 0)))
    ;; Comet: bright head with a tail fading off behind it (lower indices).
    (dotimes (k tail)
      (let ((level (round (* head-level (/ (float (- tail k)) tail)))))
        (when (> level 0)
          (aset leds (mod (- head k) 64) level))))
    ;; Effort-level markers overlaid: one per level, brightest at current.
    (when info
      (let* ((levels (nth 0 info))
             (current (nth 1 info))
             (count (length levels)))
        (when (> count 0)
          (dotimes (i count)
            (let ((pos (mod (round (/ (* 64.0 i) count)) 64))
                  (brightness (if (= i current) 15 5)))
              (aset leds pos brightness))))))
    (agent-shell-monome--set-ring-map n leds)))

(defun agent-shell-monome--unsubscribe-tokens (buffer token)
  "Unsubscribe a tokens subscription TOKEN for BUFFER, if possible."
  (when (and token
             (buffer-live-p buffer)
             (fboundp 'agent-shell-unsubscribe))
    (with-current-buffer buffer
      (ignore-errors
        (agent-shell-unsubscribe :subscription token)))))

(defun agent-shell-monome--prune-tokens-subscriptions ()
  "Drop subscription entries for dead buffers."
  (let* ((subs (alist-get :tokens-subscriptions agent-shell-monome--state))
         (alive (seq-filter (lambda (entry) (buffer-live-p (car entry))) subs))
         (dead (seq-difference subs alive)))
    (dolist (entry dead)
      (agent-shell-monome--unsubscribe-tokens (car entry) (cdr entry)))
    (setf (alist-get :tokens-subscriptions agent-shell-monome--state) alive)))

;;; Main tick

(defun agent-shell-monome--render-grid ()
  "Refresh the grid LEDs from current state.
The buffer currently being recorded into (hold-to-talk) blinks so the
live mic is unmistakable; every other key reflects its buffer status."
  (when (alist-get :grid-port agent-shell-monome--state)
    (agent-shell-monome--prune-bindings)
    (agent-shell-monome--prune-project-columns)
    (agent-shell-monome--assign-new-buffers)
    (let ((recording (alist-get :htt-recording agent-shell-monome--state))
          (tick (or (alist-get :tick agent-shell-monome--state) 0)))
      (dolist (entry (alist-get :bindings agent-shell-monome--state))
        (agent-shell-monome--set-grid-led
         (car (car entry)) (cdr (car entry))
         (if (eq (cdr entry) recording)
             (if (< (mod tick 4) 2) 15 0)
           (agent-shell-monome--level-for-buffer (cdr entry))))))))

(defun agent-shell-monome--render-arc ()
  "Refresh all arc rings from current state."
  (when (alist-get :arc-port agent-shell-monome--state)
    ;; Periodically drop the map cache so every ring restates itself; this
    ;; heals any /ring/map packet the arc's flaky USB write path dropped.
    (when (zerop (mod (or (alist-get :tick agent-shell-monome--state) 0) 50))
      (setf (alist-get :last-ring-maps agent-shell-monome--state) nil))
    (agent-shell-monome--decay-decision-accumulator)
    (agent-shell-monome--prune-tokens-history)
    (agent-shell-monome--prune-tokens-subscriptions)
    (dolist (buffer (agent-shell-buffers))
      (agent-shell-monome--ensure-tokens-subscribed buffer))
    (when (< agent-shell-monome-arc-selector-encoder
             agent-shell-monome-arc-encoder-count)
      (agent-shell-monome--render-selector))
    (when (< agent-shell-monome-arc-scroll-encoder
             agent-shell-monome-arc-encoder-count)
      (agent-shell-monome--render-scroll))
    (when (< agent-shell-monome-arc-decision-encoder
             agent-shell-monome-arc-encoder-count)
      (agent-shell-monome--render-decision))
    (when (< agent-shell-monome-arc-tokens-encoder
             agent-shell-monome-arc-encoder-count)
      (agent-shell-monome--render-tokens))))

(defun agent-shell-monome--tick ()
  "One refresh tick: redraw grid and arc."
  (setf (alist-get :tick agent-shell-monome--state)
        (1+ (or (alist-get :tick agent-shell-monome--state) 0)))
  (agent-shell-monome--render-grid)
  (agent-shell-monome--render-arc))

;;; Entry points

;;;###autoload
(defun agent-shell-monome-start ()
  "Start the monome bridge for `agent-shell'."
  (interactive)
  (when agent-shell-monome--state
    (user-error "agent-shell-monome already running; call agent-shell-monome-stop first"))
  (require 'agent-shell)
  (let ((proc (make-network-process
               :name "agent-shell-monome"
               :type 'datagram
               :family 'ipv4
               :service agent-shell-monome-listen-port
               :host "127.0.0.1"
               :server t
               :noquery t
               :coding '(binary . binary)
               :filter #'agent-shell-monome--filter)))
    (setq agent-shell-monome--state
          (list (cons :process proc)
                (cons :serialosc-process nil)
                (cons :timer nil)
                (cons :tick 0)
                ;; Grid
                (cons :grid-id nil)
                (cons :grid-host nil)
                (cons :grid-port nil)
                (cons :grid-prefix agent-shell-monome-grid-prefix)
                (cons :grid-width 8)
                (cons :grid-height 8)
                (cons :bindings nil)
                (cons :project-columns nil)
                (cons :last-leds nil)
                ;; Arc
                (cons :arc-id nil)
                (cons :arc-host nil)
                (cons :arc-port nil)
                (cons :arc-prefix agent-shell-monome-arc-prefix)
                (cons :selected-index 0)
                (cons :selector-accumulator 0)
                (cons :scroll-accumulator 0)
                (cons :decision-accumulator 0)
                (cons :effort-accumulator 0)
                (cons :tokens-history nil)
                (cons :tokens-last-totals nil)
                (cons :tokens-subscriptions nil)
                (cons :tokens-spinner-phase 0.0)
                (cons :last-ring-leds nil)
                (cons :last-ring-maps nil)
                ;; Permissions
                (cons :pending-permissions nil)
                (cons :saved-responder agent-shell-permission-responder-function)
                ;; Hold-to-talk
                (cons :htt-down-coord nil)
                (cons :htt-timer nil)
                (cons :htt-recording nil)
                (cons :htt-target nil)))
    (setq agent-shell-permission-responder-function
          #'agent-shell-monome--responder)
    (setf (alist-get :timer agent-shell-monome--state)
          (run-at-time 0 agent-shell-monome-tick-seconds
                       #'agent-shell-monome--tick))
    ;; If we just launched serialosc, give it a moment to bind its
    ;; discovery port before asking it to list devices; otherwise the
    ;; daemon is already up and we can discover immediately.
    (if (agent-shell-monome--start-serialosc)
        (run-at-time agent-shell-monome-serialosc-startup-delay nil
                     #'agent-shell-monome--discover)
      (agent-shell-monome--discover))
    (message "agent-shell-monome started on UDP %d"
             agent-shell-monome-listen-port)))

;;;###autoload
(defun agent-shell-monome-stop ()
  "Stop the monome bridge."
  (interactive)
  (unless agent-shell-monome--state
    (user-error "agent-shell-monome is not running"))
  (when-let ((timer (alist-get :timer agent-shell-monome--state)))
    (cancel-timer timer))
  (when-let ((timer (alist-get :htt-timer agent-shell-monome--state)))
    (cancel-timer timer))
  (dolist (entry (alist-get :tokens-subscriptions agent-shell-monome--state))
    (agent-shell-monome--unsubscribe-tokens (car entry) (cdr entry)))
  (ignore-errors (agent-shell-monome--clear-grid))
  (ignore-errors (agent-shell-monome--clear-arc))
  ;; Stop serialosc only after the clears above have been flushed to the
  ;; devices through it.
  (agent-shell-monome--stop-serialosc)
  (setq agent-shell-permission-responder-function
        (alist-get :saved-responder agent-shell-monome--state))
  (when-let ((proc (alist-get :process agent-shell-monome--state)))
    (delete-process proc))
  (setq agent-shell-monome--state nil)
  (message "agent-shell-monome stopped"))

;;;###autoload
(defun agent-shell-monome ()
  "Toggle the monome bridge for `agent-shell'."
  (interactive)
  (if agent-shell-monome--state
      (agent-shell-monome-stop)
    (agent-shell-monome-start)))

;;; whisper integration (hold-to-talk)

;; Registered once, regardless of load order.  The handler no-ops unless
;; a hold-to-talk recording set `:htt-target', so it never disturbs other
;; users of `whisper-run'.
(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook
            #'agent-shell-monome--whisper-transcription-handler))

(provide 'agent-shell-monome)

;;; agent-shell-monome.el ends here
