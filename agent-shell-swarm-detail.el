;;; agent-shell-swarm-detail.el --- Detail view for agent-shell-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Detail view for one agent: header info, the pending permission
;; request (answerable in place), changed files, and tool-call history.
;; Opened from the dashboard with `agent-shell-swarm-inspect' and kept
;; in sync with dashboard refreshes.

;;; Code:

(require 'button)
(require 'map)
(require 'seq)
(require 'agent-shell)
(require 'agent-shell-swarm-shell)

(defconst agent-shell-swarm--detail-buffer-name "*agent-shell swarm detail*"
  "Name of the agent detail buffer.")

(defvar-local agent-shell-swarm-detail--shell-buffer nil
  "Shell buffer this detail view describes.")

(defun agent-shell-swarm--format-tool-call (tool-call)
  "Format TOOL-CALL as a one-line summary."
  (format "  [%s] %s — %s"
          (or (map-elt tool-call :kind) "?")
          (replace-regexp-in-string "[\n\r\t]+" " "
                                    (string-trim (or (map-elt tool-call :title) "")))
          (or (map-elt tool-call :status) "?")))

(defun agent-shell-swarm--display-path (path shell-buffer)
  "Return PATH relative to SHELL-BUFFER's directory when it lies inside it."
  (let ((relative (file-relative-name
                   path (buffer-local-value 'default-directory shell-buffer))))
    (if (string-prefix-p ".." relative)
        (abbreviate-file-name path)
      relative)))

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
    (let ((files (buffer-local-value 'agent-shell-swarm--changed-files
                                     shell-buffer)))
      (insert "\n"
              (propertize (format "Changed files (%d):" (length files))
                          'face 'bold)
              "\n")
      (if (null files)
          (insert (propertize
                   "  none recorded (tracked since the dashboard first opened)\n"
                   'face 'shadow))
        (dolist (path (reverse files))
          (insert "  "
                  (buttonize (agent-shell-swarm--display-path path shell-buffer)
                             #'find-file path)
                  "\n"))))
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

(provide 'agent-shell-swarm-detail)

;;; agent-shell-swarm-detail.el ends here
