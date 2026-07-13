;;; agent-shell-swarm.el --- Dashboard for agent-shell instances -*- lexical-binding: t; -*-

;; Author: Endi Sukaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (agent-shell "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/esukaj/agent-shell-swarm

;;; Commentary:

;; A dashboard for monitoring multiple running `agent-shell' instances.
;;
;; M-x agent-shell-swarm opens a buffer listing every agent-shell
;; buffer along with its status (ready/busy/blocked/dead), agent name,
;; model, working directory, and session title.
;;
;; In the dashboard:
;;   RET  visit the shell buffer at point
;;   s    send a prompt to the agent at point
;;   i    interrupt the agent at point
;;   k    kill the agent at point (also on x)
;;   n    start a new agent
;;   b    jump to the next blocked agent
;;   d    inspect the agent: pending permission + tool calls
;;   m/u  mark/unmark the agent at point (U unmarks all)
;;   K    kill all marked agents
;;   I    interrupt all marked agents
;;   g    refresh the list
;;
;; In the detail view (d): a answers a pending permission request,
;; g refreshes, q quits.
;;
;; The dashboard refreshes itself when shells emit events (turn
;; complete, permission requests, tool calls, ...), so it stays
;; current without polling.
;;
;; With `evil-mode', the same keys work in normal/motion state, except
;; kill is only on x (k stays line-up) and refresh is on gr.
;;
;; The mode line shows a swarm summary: agent count, busy/blocked
;; counts, and total cost.

;;; Code:

(require 'map)
(require 'seq)
(require 'tabulated-list)
(require 'agent-shell)

(defconst agent-shell-swarm--buffer-name "*agent-shell swarm*"
  "Name of the swarm dashboard buffer.")

(defconst agent-shell-swarm--list-format
  [("Buffer" 30 t)
   ("Status" 8 t)
   ("Agent" 10 t)
   ("Model" 18 t)
   ("Mode" 14 t)
   ("Ctx" 4 t :right-align t)
   ("Cost" 8 t :right-align t)
   ("Age" 4 t :right-align t)
   ("Directory" 26 t)
   ("Title" 0 t)]
  "Column layout of the dashboard.")

(defun agent-shell-swarm--fit-row (row)
  "Truncate each cell in ROW to its column's width.
Overlong cells break `tabulated-list-mode' alignment, which pads but
never truncates.  Directory cells keep their tail (the informative
part); all others keep their head.  The last column (width 0) is
left untouched.

Truncation uses ASCII \"...\" rather than the Unicode ellipsis: the
latter has ambiguous Unicode width, so terminals may render it two
cells wide and knock every following column off by one."
  (dotimes (index (length row))
    (pcase-let ((`(,name ,width . ,_)
                 (aref agent-shell-swarm--list-format index)))
      (let ((cell (aref row index)))
        ;; Newlines (e.g. in multiline session titles) would spill the
        ;; rest of the cell onto its own table line.
        (when (string-match-p "[\n\r\t]" cell)
          (setq cell (string-trim (replace-regexp-in-string "[\n\r\t]+" " " cell)))
          (aset row index cell))
        (when (and (> width 0) (> (string-width cell) width))
          (aset row index
                (if (equal name "Directory")
                    (let ((tail (substring cell (- (length cell) (- width 3)))))
                      (concat (propertize "..." 'face (get-text-property 0 'face tail))
                              tail))
                  (truncate-string-to-width cell width nil nil "...")))))))
  row)

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

(defun agent-shell-swarm--entry (shell-buffer)
  "Build a `tabulated-list-entries' entry for SHELL-BUFFER."
  (let* ((state (buffer-local-value 'agent-shell--state shell-buffer))
         (status (agent-shell-swarm--status shell-buffer))
         (agent (or (map-nested-elt state '(:agent-config :mode-line-name))
                    (map-nested-elt state '(:agent-config :buffer-name))
                    "?"))
         (model (or (agent-shell-get-model-name state) ""))
         (mode (or (agent-shell-get-mode-name state) ""))
         (directory (abbreviate-file-name
                     (buffer-local-value 'default-directory shell-buffer)))
         (title (string-trim (or (map-nested-elt state '(:session :title)) ""))))
    (list shell-buffer
          (agent-shell-swarm--fit-row
           (vector (propertize (buffer-name shell-buffer)
                               'face 'font-lock-variable-name-face)
                   (propertize status
                               'face (agent-shell-swarm--status-face status))
                   agent
                   model
                   mode
                   (agent-shell-swarm--context-percent state)
                   (agent-shell-swarm--cost state)
                   (agent-shell-swarm--age (map-elt state :last-activity-time))
                   (propertize directory 'face 'font-lock-doc-face)
                   title)))))

(defun agent-shell-swarm--entries ()
  "Return dashboard entries for all live agent-shell buffers."
  (mapcar #'agent-shell-swarm--entry (agent-shell-buffers)))

(defun agent-shell-swarm--shell-buffer-at-point ()
  "Return the live shell buffer for the dashboard row at point.
Signals a `user-error' when there is no row or its buffer is dead."
  (let ((shell-buffer (tabulated-list-get-id)))
    (unless shell-buffer
      (user-error "No agent at point"))
    (unless (buffer-live-p shell-buffer)
      (user-error "Shell buffer no longer exists"))
    shell-buffer))

(defun agent-shell-swarm--session-ready-p (shell-buffer)
  "Return non-nil when SHELL-BUFFER can accept prompt input.
Mirrors the session check `agent-shell--insert-to-shell-buffer'
performs, so we can fail with a friendlier error."
  (with-current-buffer shell-buffer
    (or (map-nested-elt agent-shell--state '(:session :id))
        (eq agent-shell-session-strategy 'new-deferred))))

(defun agent-shell-swarm-visit ()
  "Visit the agent-shell buffer at point."
  (interactive)
  (pop-to-buffer (agent-shell-swarm--shell-buffer-at-point)))

(defun agent-shell-swarm-send-prompt ()
  "Send a prompt to the agent at point without leaving the dashboard.
Asks for confirmation when the agent is busy or blocked, since the
input would queue behind (or interleave with) in-flight work."
  (interactive)
  (let* ((shell-buffer (agent-shell-swarm--shell-buffer-at-point))
         (name (buffer-name shell-buffer))
         (status (agent-shell-swarm--status shell-buffer)))
    (unless (agent-shell-swarm--session-ready-p shell-buffer)
      (user-error "%s is still initializing; try again shortly" name))
    (when (and (member status '("busy" "blocked"))
               (not (yes-or-no-p (format "%s is %s; send anyway? " name status))))
      (user-error "Cancelled"))
    (let ((prompt (string-trim (read-string (format "Send to %s: " name)))))
      (when (string-empty-p prompt)
        (user-error "Nothing to send"))
      (agent-shell--insert-to-shell-buffer :shell-buffer shell-buffer
                                           :text prompt
                                           :submit t
                                           :no-focus t)
      (revert-buffer)
      (message "Sent to %s" name))))

(defun agent-shell-swarm-interrupt ()
  "Interrupt the agent at point if it is busy or blocked."
  (interactive)
  (let* ((shell-buffer (agent-shell-swarm--shell-buffer-at-point))
         (status (agent-shell-swarm--status shell-buffer)))
    (if (not (member status '("busy" "blocked")))
        (message "%s has nothing to interrupt (status: %s)"
                 (buffer-name shell-buffer) status)
      (with-current-buffer shell-buffer
        (agent-shell-interrupt))
      (revert-buffer))))

(defun agent-shell-swarm-kill ()
  "Kill the agent-shell buffer at point, after confirmation."
  (interactive)
  (let ((shell-buffer (agent-shell-swarm--shell-buffer-at-point)))
    (when (yes-or-no-p (format "Kill agent %s? " (buffer-name shell-buffer)))
      (kill-buffer shell-buffer)
      (revert-buffer))))

(defun agent-shell-swarm-new-agent ()
  "Start a new agent from the dashboard.
Prompts for a working directory, defaulting to the directory of the
agent at point, then for an agent config.  The shell's own working
directory logic still applies (`agent-shell-cwd' may prefer the
project root containing the chosen directory)."
  (interactive)
  (let* ((at-point (tabulated-list-get-id))
         (initial (if (and at-point (buffer-live-p at-point))
                      (buffer-local-value 'default-directory at-point)
                    default-directory))
         (directory (read-directory-name "Start agent in: " initial nil t))
         (config (or (agent-shell-select-config :prompt "Start new agent: ")
                     (user-error "No agent selected")))
         (default-directory directory))
    (agent-shell--start :config config :no-focus t :new-session t)
    (revert-buffer)))

(defun agent-shell-swarm--blocked-row-positions ()
  "Return dashboard positions of rows whose agent is blocked."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((buffer (tabulated-list-get-id))
                    ((buffer-live-p buffer))
                    ((equal (agent-shell-swarm--status buffer) "blocked")))
          (push (point) positions))
        (forward-line 1)))
    (nreverse positions)))

(defun agent-shell-swarm-next-blocked ()
  "Move point to the next blocked agent, wrapping around the list.
Blocked agents are the ones waiting on a permission response."
  (interactive)
  (let* ((positions (agent-shell-swarm--blocked-row-positions))
         (next (or (seq-find (lambda (position)
                               (> position (line-beginning-position)))
                             positions)
                   (car positions))))
    (if next
        (goto-char next)
      (message "No blocked agents"))))

;;; Event-driven refresh

(defvar agent-shell-swarm-refresh-debounce 0.3
  "Seconds to wait before refreshing after a shell event.
Coalesces event bursts (e.g. tool-call updates) into one refresh.")

(defvar-local agent-shell-swarm--subscriptions nil
  "Alist of (SHELL-BUFFER . TOKEN) event subscriptions held by the dashboard.")
(put 'agent-shell-swarm--subscriptions 'permanent-local t)

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
      (unless (assq shell-buffer agent-shell-swarm--subscriptions)
        (push (cons shell-buffer
                    (agent-shell-subscribe-to
                     :shell-buffer shell-buffer
                     :on-event (lambda (_event)
                                 (agent-shell-swarm--schedule-refresh dashboard))))
              agent-shell-swarm--subscriptions)))))

(defun agent-shell-swarm--teardown ()
  "Cancel the refresh timer and drop all shell subscriptions."
  (when (timerp agent-shell-swarm--refresh-timer)
    (cancel-timer agent-shell-swarm--refresh-timer)
    (setq agent-shell-swarm--refresh-timer nil))
  (dolist (entry agent-shell-swarm--subscriptions)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (agent-shell-unsubscribe :subscription (cdr entry)))))
  (setq agent-shell-swarm--subscriptions nil))

(defun agent-shell-swarm--on-new-shell ()
  "Refresh the dashboard when a new agent shell starts.
On `agent-shell-mode-hook' globally; the refresh subscribes the
dashboard to the new shell's events."
  (when-let* ((dashboard (get-buffer agent-shell-swarm--buffer-name)))
    (agent-shell-swarm--schedule-refresh dashboard)))

;;; Marks and bulk operations

(defun agent-shell-swarm--marked-shells ()
  "Return the shell buffers of all marked dashboard rows."
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (eq (char-after) ?*) (tabulated-list-get-id))
          (push (tabulated-list-get-id) marked))
        (forward-line 1)))
    (nreverse marked)))

(defun agent-shell-swarm--apply-marks (shells)
  "Re-mark dashboard rows whose shell buffer is in SHELLS."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (memq (tabulated-list-get-id) shells)
        (tabulated-list-put-tag "*"))
      (forward-line 1))))

(defun agent-shell-swarm--revert (&rest _)
  "Revert the dashboard, preserving row marks across the refresh."
  (let ((marked (agent-shell-swarm--marked-shells)))
    (tabulated-list-revert)
    (agent-shell-swarm--apply-marks marked)))

(defun agent-shell-swarm-mark ()
  "Mark the agent at point and move down."
  (interactive)
  (agent-shell-swarm--shell-buffer-at-point)
  (tabulated-list-put-tag "*" t))

(defun agent-shell-swarm-unmark ()
  "Unmark the agent at point and move down."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun agent-shell-swarm-unmark-all ()
  "Unmark all agents."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag " ")
      (forward-line 1))))

(defun agent-shell-swarm-kill-marked ()
  "Kill all marked agents, after a single confirmation."
  (interactive)
  (let ((marked (seq-filter #'buffer-live-p (agent-shell-swarm--marked-shells))))
    (unless marked
      (user-error "No marked agents"))
    (when (yes-or-no-p (format "Kill %d marked agent%s? "
                               (length marked)
                               (if (= 1 (length marked)) "" "s")))
      (dolist (shell-buffer marked)
        (kill-buffer shell-buffer))
      (revert-buffer))))

(defun agent-shell-swarm-interrupt-marked ()
  "Interrupt every marked agent that is busy or blocked."
  (interactive)
  (let* ((marked (seq-filter #'buffer-live-p (agent-shell-swarm--marked-shells)))
         (interruptible (seq-filter
                         (lambda (buffer)
                           (member (agent-shell-swarm--status buffer)
                                   '("busy" "blocked")))
                         marked)))
    (unless marked
      (user-error "No marked agents"))
    (if (not interruptible)
        (message "No marked agent is busy or blocked")
      (when (yes-or-no-p (format "Interrupt %d agent%s? "
                                 (length interruptible)
                                 (if (= 1 (length interruptible)) "" "s")))
        (dolist (shell-buffer interruptible)
          (with-current-buffer shell-buffer
            ;; Force: we already confirmed once for the whole set.
            (agent-shell-interrupt t)))
        (revert-buffer)))))

;;; Detail view

(defconst agent-shell-swarm--detail-buffer-name "*agent-shell swarm detail*"
  "Name of the agent detail buffer.")

(defvar-local agent-shell-swarm-detail--shell-buffer nil
  "Shell buffer this detail view describes.")

(defun agent-shell-swarm--pending-permission (shell-buffer)
  "Return the (TOOL-CALL-ID . TOOL-CALL) awaiting permission in SHELL-BUFFER."
  (seq-find (lambda (entry)
              (map-elt (cdr entry) :permission-request-id))
            (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                     :tool-calls)))

(defun agent-shell-swarm--format-tool-call (tool-call)
  "Format TOOL-CALL as a one-line summary."
  (format "  [%s] %s — %s"
          (or (map-elt tool-call :kind) "?")
          (replace-regexp-in-string "[\n\r\t]+" " "
                                    (string-trim (or (map-elt tool-call :title) "")))
          (or (map-elt tool-call :status) "?")))

(defun agent-shell-swarm-detail-render ()
  "Render the detail view from its shell's live state."
  (let* ((shell-buffer agent-shell-swarm-detail--shell-buffer)
         (state (buffer-local-value 'agent-shell--state shell-buffer))
         (status (agent-shell-swarm--status shell-buffer))
         (line (line-number-at-pos))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (buffer-name shell-buffer) 'face 'bold)
            " — "
            (propertize status 'face (agent-shell-swarm--status-face status))
            "\n\n")
    (insert (format "Agent:      %s\n"
                    (or (map-nested-elt state '(:agent-config :mode-line-name)) "?"))
            (format "Model:      %s\n" (or (agent-shell-get-model-name state) ""))
            (format "Mode:       %s\n" (or (agent-shell-get-mode-name state) ""))
            (format "Directory:  %s\n"
                    (abbreviate-file-name
                     (buffer-local-value 'default-directory shell-buffer)))
            (format "Context:    %s\n" (agent-shell-swarm--context-percent state))
            (format "Cost:       %s\n" (agent-shell-swarm--cost state)))
    (when-let* ((pending (agent-shell-swarm--pending-permission shell-buffer))
                (tool-call (cdr pending)))
      (insert "\n"
              (propertize "Pending permission" 'face 'error)
              (substitute-command-keys
               " (\\[agent-shell-swarm-detail-answer-permission] to answer):\n")
              (agent-shell-swarm--format-tool-call tool-call) "\n")
      (when-let* ((raw-input (map-elt tool-call :raw-input)))
        (insert "  Input:\n")
        (map-do (lambda (key value)
                  (insert (format "    %s: %s\n" key
                                  (truncate-string-to-width
                                   (replace-regexp-in-string
                                    "[\n\r\t]+" " " (format "%s" value))
                                   120 nil nil "..."))))
                raw-input))
      (insert (format "  Options: %s\n"
                      (mapconcat (lambda (action) (map-elt action :option))
                                 (map-elt tool-call :permission-actions)
                                 " · "))))
    (when-let* ((tool-calls (map-elt state :tool-calls)))
      (insert "\n" (propertize "Tool calls:" 'face 'bold) "\n")
      (dolist (entry tool-calls)
        (insert (agent-shell-swarm--format-tool-call (cdr entry)) "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun agent-shell-swarm-detail-refresh ()
  "Re-render the detail view."
  (interactive)
  (unless (buffer-live-p agent-shell-swarm-detail--shell-buffer)
    (user-error "Shell buffer no longer exists"))
  (agent-shell-swarm-detail-render))

(defun agent-shell-swarm-detail-answer-permission ()
  "Answer the shell's pending permission request.
Prompts for one of the options the agent offered (allow once,
allow always, reject, ...) and sends the response."
  (interactive)
  (let ((shell-buffer agent-shell-swarm-detail--shell-buffer))
    (unless (buffer-live-p shell-buffer)
      (user-error "Shell buffer no longer exists"))
    (let* ((pending (or (agent-shell-swarm--pending-permission shell-buffer)
                        (user-error "No pending permission")))
           (tool-call (cdr pending))
           (actions (or (map-elt tool-call :permission-actions)
                        (user-error "Permission request offers no options")))
           (choice (completing-read
                    (format "Respond to \"%s\": "
                            (string-trim (or (map-elt tool-call :title) "")))
                    (mapcar (lambda (action) (map-elt action :option)) actions)
                    nil t))
           (action (seq-find (lambda (action)
                               (equal (map-elt action :option) choice))
                             actions))
           (state (buffer-local-value 'agent-shell--state shell-buffer)))
      (agent-shell--send-permission-response
       :client (map-elt state :client)
       :request-id (map-elt tool-call :permission-request-id)
       :option-id (map-elt action :option-id)
       :state state
       :tool-call-id (car pending))
      (agent-shell-swarm-detail-render)
      (message "Sent: %s" choice))))

(defvar-keymap agent-shell-swarm-detail-mode-map
  :doc "Keymap for `agent-shell-swarm-detail-mode'."
  :parent special-mode-map
  "a" #'agent-shell-swarm-detail-answer-permission
  "g" #'agent-shell-swarm-detail-refresh)

(define-derived-mode agent-shell-swarm-detail-mode special-mode "Agent-Swarm-Detail"
  "Major mode showing details for one agent-shell instance.")

(defun agent-shell-swarm-inspect ()
  "Show details for the agent at point.
Includes any pending permission request (answerable with
\\<agent-shell-swarm-detail-mode-map>\\[agent-shell-swarm-detail-answer-permission]) and the tool calls of the current session."
  (interactive)
  (let ((shell-buffer (agent-shell-swarm--shell-buffer-at-point))
        (detail (get-buffer-create agent-shell-swarm--detail-buffer-name)))
    (with-current-buffer detail
      (agent-shell-swarm-detail-mode)
      (setq agent-shell-swarm-detail--shell-buffer shell-buffer)
      (agent-shell-swarm-detail-render))
    (pop-to-buffer detail)))

(defun agent-shell-swarm--sync-detail ()
  "Keep a live detail buffer in sync with dashboard refreshes."
  (when-let* ((detail (get-buffer agent-shell-swarm--detail-buffer-name)))
    (with-current-buffer detail
      (when (buffer-live-p agent-shell-swarm-detail--shell-buffer)
        (agent-shell-swarm-detail-render)))))

;;; Summary

(defun agent-shell-swarm--summary ()
  "Return a one-line summary of the swarm for the mode line."
  (let* ((buffers (agent-shell-buffers))
         (statuses (mapcar #'agent-shell-swarm--status buffers))
         (busy (seq-count (apply-partially #'equal "busy") statuses))
         (blocked (seq-count (apply-partially #'equal "blocked") statuses))
         (costs nil))
    (dolist (buffer buffers)
      (let* ((state (buffer-local-value 'agent-shell--state buffer))
             (amount (map-nested-elt state '(:usage :cost-amount)))
             (currency (or (map-nested-elt state '(:usage :cost-currency)) "$")))
        (when (and amount (> amount 0))
          (setf (alist-get currency costs 0 nil #'equal)
                (+ amount (alist-get currency costs 0 nil #'equal))))))
    (string-join
     (delq nil
           (list (format "%d agent%s" (length buffers)
                         (if (= (length buffers) 1) "" "s"))
                 (when (> busy 0)
                   (propertize (format "%d busy" busy) 'face 'warning))
                 (when (> blocked 0)
                   (propertize (format "%d blocked" blocked) 'face 'error))
                 (when costs
                   (mapconcat (lambda (entry)
                                (format "%s%.2f" (car entry) (cdr entry)))
                              costs " "))))
     " · ")))

(defun agent-shell-swarm--update-mode-line ()
  "Refresh the swarm summary shown in the dashboard's mode line."
  (setq mode-line-process (list ": " (agent-shell-swarm--summary))))

(defvar-keymap agent-shell-swarm-mode-map
  :doc "Keymap for `agent-shell-swarm-mode'."
  :parent tabulated-list-mode-map
  "RET" #'agent-shell-swarm-visit
  "s" #'agent-shell-swarm-send-prompt
  "i" #'agent-shell-swarm-interrupt
  "k" #'agent-shell-swarm-kill
  "x" #'agent-shell-swarm-kill
  "n" #'agent-shell-swarm-new-agent
  "b" #'agent-shell-swarm-next-blocked
  "d" #'agent-shell-swarm-inspect
  "m" #'agent-shell-swarm-mark
  "u" #'agent-shell-swarm-unmark
  "U" #'agent-shell-swarm-unmark-all
  "K" #'agent-shell-swarm-kill-marked
  "I" #'agent-shell-swarm-interrupt-marked)

(declare-function evil-define-key* "evil-core")

(with-eval-after-load 'evil
  ;; Evil's normal/motion states shadow the mode map, so register the
  ;; dashboard keys as auxiliary bindings for those states.  Kill stays
  ;; off `k' here: that must remain line-up navigation under evil.
  (evil-define-key* '(normal motion) agent-shell-swarm-mode-map
    (kbd "RET") #'agent-shell-swarm-visit
    "s" #'agent-shell-swarm-send-prompt
    "i" #'agent-shell-swarm-interrupt
    "x" #'agent-shell-swarm-kill
    "n" #'agent-shell-swarm-new-agent
    "b" #'agent-shell-swarm-next-blocked
    "d" #'agent-shell-swarm-inspect
    "m" #'agent-shell-swarm-mark
    "u" #'agent-shell-swarm-unmark
    "U" #'agent-shell-swarm-unmark-all
    "K" #'agent-shell-swarm-kill-marked
    "I" #'agent-shell-swarm-interrupt-marked
    "gr" #'revert-buffer)
  (evil-define-key* '(normal motion) agent-shell-swarm-detail-mode-map
    "a" #'agent-shell-swarm-detail-answer-permission
    "q" #'quit-window
    "gr" #'agent-shell-swarm-detail-refresh))

(define-derived-mode agent-shell-swarm-mode tabulated-list-mode "Agent-Swarm"
  "Major mode listing all running `agent-shell' instances."
  (setq tabulated-list-format agent-shell-swarm--list-format)
  (setq tabulated-list-entries #'agent-shell-swarm--entries)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'agent-shell-swarm--revert)
  (add-hook 'tabulated-list-revert-hook
            #'agent-shell-swarm--sync-subscriptions nil t)
  (add-hook 'tabulated-list-revert-hook
            #'agent-shell-swarm--update-mode-line nil t)
  (add-hook 'tabulated-list-revert-hook
            #'agent-shell-swarm--sync-detail nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-swarm--teardown nil t)
  ;; Global: new shells (created outside the dashboard too) trigger a
  ;; refresh, which in turn subscribes the dashboard to their events.
  (add-hook 'agent-shell-mode-hook #'agent-shell-swarm--on-new-shell))

;;;###autoload
(defun agent-shell-swarm ()
  "Open a dashboard listing all running `agent-shell' instances."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-swarm--buffer-name)))
    (with-current-buffer buffer
      ;; Re-enable the mode even when already active: the column layout
      ;; is buffer-local, so a pre-existing dashboard would otherwise
      ;; keep rendering with a stale `tabulated-list-format'.
      (agent-shell-swarm-mode)
      (revert-buffer))
    (pop-to-buffer buffer)))

(provide 'agent-shell-swarm)

;;; agent-shell-swarm.el ends here
