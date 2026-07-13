;;; agent-shell-swarm-shell.el --- Per-shell primitives for agent-shell-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Reading a single agent-shell buffer's state: status, usage, pending
;; permissions, and the changed-files record.  Everything here takes a
;; shell buffer (or its state) as input and has no dashboard knowledge,
;; except `agent-shell-swarm--shell-buffer-at-point', the shared
;; "which shell does this dashboard row refer to" helper.

;;; Code:

(require 'map)
(require 'seq)
(require 'tabulated-list)
(require 'agent-shell)

(defun agent-shell-swarm--status (shell-buffer)
  "Return a status string for SHELL-BUFFER.

One of \"ready\", \"busy\", \"blocked\", \"dead\" (agent process
exited), or \"?\" when the status cannot be determined."
  (let* ((state (buffer-local-value 'agent-shell--state shell-buffer))
         (process (map-nested-elt state '(:client :process))))
    (cond ((and process (not (process-live-p process))) "dead")
          ;; `agent-shell-status' errors on shells that haven't finished
          ;; initializing; don't let one shell break the whole dashboard.
          ((condition-case nil
               (symbol-name (agent-shell-status :shell-buffer shell-buffer))
             (error nil)))
          (t "?"))))

(defun agent-shell-swarm--status-face (status)
  "Return the face used to display STATUS."
  (pcase status
    ("busy" 'warning)
    ("blocked" 'error)
    ("dead" 'shadow)
    ("?" 'shadow)
    (_ 'success)))

(defun agent-shell-swarm--context-percent (state)
  "Return STATE's context window usage as a percent string.
Returns \"\" when the agent hasn't reported a context size.  High
usage is highlighted (warning at 75%, error at 90%)."
  (let ((used (map-nested-elt state '(:usage :context-used)))
        (size (map-nested-elt state '(:usage :context-size))))
    (if (or (null size) (zerop size))
        ""
      (let ((percent (floor (* 100.0 (/ (float (or used 0)) size)))))
        (propertize (format "%d%%" percent)
                    'face (cond ((>= percent 90) 'error)
                                ((>= percent 75) 'warning)
                                (t 'default)))))))

(defun agent-shell-swarm--cost (state)
  "Return STATE's accumulated cost as a string, or \"\" if unreported."
  (let ((amount (map-nested-elt state '(:usage :cost-amount))))
    (if (and amount (> amount 0))
        (format "%s%.2f"
                (or (map-nested-elt state '(:usage :cost-currency)) "$")
                amount)
      "")))

(defun agent-shell-swarm--age (time)
  "Return TIME as a compact age relative to now, or \"\" when nil."
  (if (null time)
      ""
    (let ((seconds (float-time (time-subtract (current-time) time))))
      (cond ((< seconds 60) (format "%ds" (max 0 (floor seconds))))
            ((< seconds 3600) (format "%dm" (floor seconds 60)))
            ((< seconds 86400) (format "%dh" (floor seconds 3600)))
            (t (format "%dd" (floor seconds 86400)))))))

(defun agent-shell-swarm--session-ready-p (shell-buffer)
  "Return non-nil when SHELL-BUFFER can accept prompt input.
Mirrors the session check `agent-shell--insert-to-shell-buffer'
performs, so we can fail with a friendlier error."
  (with-current-buffer shell-buffer
    (or (map-nested-elt agent-shell--state '(:session :id))
        (eq agent-shell-session-strategy 'new-deferred))))

(defun agent-shell-swarm--pending-permission (shell-buffer)
  "Return the (TOOL-CALL-ID . TOOL-CALL) awaiting permission in SHELL-BUFFER."
  (seq-find (lambda (entry)
              (map-elt (cdr entry) :permission-request-id))
            (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                     :tool-calls)))

;;; Changed files

(defvar-local agent-shell-swarm--changed-files nil
  "Files this shell has written, newest first.
Lives in the shell buffer; recorded while a dashboard is subscribed,
so writes made before the first dashboard render are not captured.

Two sources feed it: `file-write' events (agents using ACP's client
filesystem API) and completed file-touching tool calls (agents like
Claude Code that write directly and only report the tool call).")

(defconst agent-shell-swarm--file-tool-kinds '("edit" "delete" "move")
  "ACP tool-call kinds that modify files (per the ToolKind schema).")

(defun agent-shell-swarm--record-changed-file (path)
  "Record PATH in the current shell's changed-files list."
  (unless (member path agent-shell-swarm--changed-files)
    (push path agent-shell-swarm--changed-files)))

;;; Dashboard row context

(defun agent-shell-swarm--shell-buffer-at-point ()
  "Return the live shell buffer for the dashboard row at point.
Signals a `user-error' when there is no row or its buffer is dead."
  (let ((shell-buffer (tabulated-list-get-id)))
    (unless shell-buffer
      (user-error "No agent at point"))
    (unless (buffer-live-p shell-buffer)
      (user-error "Shell buffer no longer exists"))
    shell-buffer))

(provide 'agent-shell-swarm-shell)

;;; agent-shell-swarm-shell.el ends here
