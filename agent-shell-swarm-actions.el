;;; agent-shell-swarm-actions.el --- Dashboard commands for agent-shell-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands acting on the agent at point (visit, send, interrupt, kill,
;; fork, new agent, worktree agent, next-blocked) and mark-based bulk operations.
;; All run in the dashboard buffer.

;;; Code:

(require 'map)
(require 'seq)
(require 'tabulated-list)
(require 'agent-shell)
(require 'agent-shell-swarm-shell)

(defun agent-shell-swarm--git (&rest arguments)
  "Run Git with ARGUMENTS and return its trimmed output.
Signal a `user-error' containing Git's output when the command fails."
  (unless (executable-find "git")
    (user-error "Git executable not found"))
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil arguments)))
      (if (zerop status)
          (string-trim (buffer-string))
        (user-error "Git failed: %s"
                    (string-trim (buffer-string)))))))

(defun agent-shell-swarm--git-root (directory)
  "Return the Git worktree root containing DIRECTORY."
  (condition-case nil
      (agent-shell-swarm--git "-C" directory "rev-parse" "--show-toplevel")
    (user-error
     (user-error "%s is not inside a Git repository"
                 (abbreviate-file-name directory)))))

;;; Row commands

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

(defun agent-shell-swarm-new-worktree-agent ()
  "Create a fresh worktree from main and start an agent-shell in it.

The repository is found from `default-directory'.  Prompt for the new
worktree's directory and agent config, add a detached worktree at
`main', then fast-forward it from main's configured remote (or
`origin').  A detached checkout is used because Git does not permit
`main' to be checked out in multiple worktrees at once."
  (interactive)
  (let* ((root (agent-shell-swarm--git-root default-directory))
         (name (file-name-nondirectory (directory-file-name root)))
         (parent (file-name-directory (directory-file-name root)))
         (worktree (directory-file-name
                    (expand-file-name
                     (read-directory-name
                      "New worktree directory: " parent nil nil
                      (concat name "-worktree")))))
         (config (or (agent-shell-select-config
                      :prompt "Start agent in new worktree: ")
                     (user-error "No agent selected")))
         (remote (condition-case nil
                     (agent-shell-swarm--git
                      "-C" root "config" "--get" "branch.main.remote")
                   (user-error "origin"))))
    (when (file-exists-p worktree)
      (user-error "%s already exists" (abbreviate-file-name worktree)))
    (agent-shell-swarm--git "-C" root "worktree" "add" "--detach"
                            worktree "main")
    (condition-case error-data
        (agent-shell-swarm--git "-C" worktree "pull" "--ff-only"
                                remote "main")
      (user-error
       ;; Do not leave an unusable worktree behind if updating it failed.
       (ignore-errors
         (agent-shell-swarm--git "-C" root "worktree" "remove" "--force"
                                 worktree))
       (signal (car error-data) (cdr error-data))))
    (let ((default-directory (file-name-as-directory worktree)))
      (agent-shell--start :config config :new-session t))
    (when (derived-mode-p 'agent-shell-swarm-mode)
      (revert-buffer))
    (message "Started agent in %s" (abbreviate-file-name worktree))))

(defun agent-shell-swarm-fork ()
  "Fork the agent at point: a new shell continuing the same conversation.
Only available when the agent advertises session-fork support."
  (interactive)
  (let* ((shell-buffer (agent-shell-swarm--shell-buffer-at-point))
         (state (buffer-local-value 'agent-shell--state shell-buffer))
         (session-id (map-nested-elt state '(:session :id)))
         (config (map-elt state :agent-config)))
    (unless session-id
      (user-error "%s has no active session to fork" (buffer-name shell-buffer)))
    (unless (map-elt state :supports-session-fork)
      (user-error "%s does not support session forking" (buffer-name shell-buffer)))
    ;; Same arguments agent-shell's own fork command uses, but stay in
    ;; the dashboard; the fork appears as a new row.
    (let ((default-directory (buffer-local-value 'default-directory shell-buffer)))
      (agent-shell--start :config config
                          :session-strategy 'new
                          :fork-session-id session-id
                          :new-session t
                          :no-focus t))
    (revert-buffer)
    (message "Forked %s" (buffer-name shell-buffer))))

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

(provide 'agent-shell-swarm-actions)

;;; agent-shell-swarm-actions.el ends here
