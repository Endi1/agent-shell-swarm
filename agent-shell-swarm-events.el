;;; agent-shell-swarm-events.el --- Event subscriptions for agent-shell-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Event-driven refresh: the dashboard subscribes to every shell's ACP
;; events and reverts itself (debounced) when anything changes.  Also
;; feeds the per-shell changed-files record from file-write events and
;; completed file-modifying tool calls.

;;; Code:

(require 'map)
(require 'seq)
(require 'agent-shell)
(require 'agent-shell-swarm-shell)

;; Defined in agent-shell-swarm.el; only needed at runtime, by which
;; point the main file is loaded.
(defvar agent-shell-swarm--buffer-name)

(defvar agent-shell-swarm-refresh-debounce 0.3
  "Seconds to wait before refreshing after a shell event.
Coalesces event bursts (e.g. tool-call updates) into one refresh.")

(defconst agent-shell-swarm--handler-version 3
  "Bump when the subscription event handler changes behavior.
Subscriptions live inside each shell's state and survive re-evaluating
this file; recording the version lets `agent-shell-swarm--sync-subscriptions'
replace stale handlers instead of keeping them forever.  A `defconst'
so reloading the file actually updates it — `defvar' would keep the
old value and defeat the whole mechanism.")

(defvar-local agent-shell-swarm--subscriptions nil
  "Alist of (SHELL-BUFFER VERSION . TOKEN) subscriptions held by the dashboard.")
(put 'agent-shell-swarm--subscriptions 'permanent-local t)

(defun agent-shell-swarm--subscription-token (entry)
  "Return the subscription token of ENTRY, tolerating the pre-version shape."
  (if (consp (cdr entry)) (cddr entry) (cdr entry)))

(defvar-local agent-shell-swarm--refresh-timer nil
  "Pending debounce timer for an event-driven refresh.")
(put 'agent-shell-swarm--refresh-timer 'permanent-local t)

(defun agent-shell-swarm--schedule-refresh (dashboard)
  "Revert DASHBOARD soon, coalescing bursts of shell events."
  (when (buffer-live-p dashboard)
    (with-current-buffer dashboard
      (unless agent-shell-swarm--refresh-timer
        (setq agent-shell-swarm--refresh-timer
              (run-with-timer
               agent-shell-swarm-refresh-debounce nil
               (lambda ()
                 (when (buffer-live-p dashboard)
                   (with-current-buffer dashboard
                     (setq agent-shell-swarm--refresh-timer nil)
                     (revert-buffer))))))))))

(defun agent-shell-swarm--on-shell-event (dashboard event)
  "Handle EVENT from the shell in the current buffer; refresh DASHBOARD."
  (pcase (map-elt event :event)
    ('file-write
     (when-let* ((path (map-nested-elt event '(:data :path))))
       (agent-shell-swarm--record-changed-file path)))
    ('tool-call-update
     (let ((tool-call (map-nested-elt event '(:data :tool-call))))
       (when (and (member (map-elt tool-call :kind)
                          agent-shell-swarm--file-tool-kinds)
                  (equal (map-elt tool-call :status) "completed"))
         (seq-doseq (location (map-elt tool-call :locations))
           (when-let* ((path (map-elt location 'path)))
             (agent-shell-swarm--record-changed-file path)))))))
  (agent-shell-swarm--schedule-refresh dashboard))

(defun agent-shell-swarm--sync-subscriptions ()
  "Subscribe the dashboard to events of any shell it isn't watching yet.
Runs on every refresh, so shells created after the dashboard get
picked up.  Dead shells are pruned; their subscriptions died with
their buffer-local state."
  (let ((dashboard (current-buffer)))
    (setq agent-shell-swarm--subscriptions
          (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                      agent-shell-swarm--subscriptions))
    (dolist (shell-buffer (agent-shell-buffers))
      (let ((entry (assq shell-buffer agent-shell-swarm--subscriptions)))
        ;; Subscribed with an older handler (file re-evaluated since):
        ;; drop it and resubscribe below.
        (when (and entry (not (equal (car-safe (cdr entry))
                                     agent-shell-swarm--handler-version)))
          (with-current-buffer shell-buffer
            (agent-shell-unsubscribe
             :subscription (agent-shell-swarm--subscription-token entry)))
          (setq agent-shell-swarm--subscriptions
                (delq entry agent-shell-swarm--subscriptions))
          (setq entry nil))
        (unless entry
          (push (cons shell-buffer
                      (cons agent-shell-swarm--handler-version
                            (agent-shell-subscribe-to
                             :shell-buffer shell-buffer
                             :on-event (lambda (event)
                                         (agent-shell-swarm--on-shell-event
                                          dashboard event)))))
                agent-shell-swarm--subscriptions))))))

(defun agent-shell-swarm--teardown ()
  "Cancel the refresh timer and drop all shell subscriptions."
  (when (timerp agent-shell-swarm--refresh-timer)
    (cancel-timer agent-shell-swarm--refresh-timer)
    (setq agent-shell-swarm--refresh-timer nil))
  (dolist (entry agent-shell-swarm--subscriptions)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (agent-shell-unsubscribe
         :subscription (agent-shell-swarm--subscription-token entry)))))
  (setq agent-shell-swarm--subscriptions nil))

(defun agent-shell-swarm--on-new-shell ()
  "Refresh the dashboard when a new agent shell starts.
On `agent-shell-mode-hook' globally; the refresh subscribes the
dashboard to the new shell's events."
  (when-let* ((dashboard (get-buffer agent-shell-swarm--buffer-name)))
    (agent-shell-swarm--schedule-refresh dashboard)))

(provide 'agent-shell-swarm-events)

;;; agent-shell-swarm-events.el ends here
