;;; agent-shell-swarm.el --- Dashboard for agent-shell instances -*- lexical-binding: t; -*-

;; Author: Endi Sukaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/Endi1/agent-shell-swarm

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
;;   n/p  move to the next/previous agent (as in Dired)
;;   +    start a new agent
;;   b    jump to the next blocked agent
;;   v    inspect the agent: pending permission, tool calls, changed files
;;   f    fork the agent (new shell, same conversation)
;;   m/u  mark/unmark the agent at point (U unmarks all)
;;   d    mark the agent for killing; x kills marked agents
;;   D    kill the agent at point immediately
;;   K    kill all marked agents (alias for x)
;;   I    interrupt all marked agents
;;   g    refresh the list
;;
;; In the detail view (v): a answers a pending permission request,
;; g refreshes, q quits.
;;
;; The dashboard refreshes itself when shells emit events (turn
;; complete, permission requests, tool calls, ...), so it stays
;; current without polling.
;;
;; With `evil-mode', the same Dired-like keys work in normal/motion
;; state and refresh is on gr.
;;
;; The mode line shows a swarm summary: agent count, busy/blocked
;; counts, and total cost.
;;
;; The package is split into:
;;   agent-shell-swarm-shell.el    per-shell state readers
;;   agent-shell-swarm-events.el   event subscriptions and refresh
;;   agent-shell-swarm-actions.el  row commands and bulk operations
;;   agent-shell-swarm-detail.el   the detail view
;;   agent-shell-swarm.el          the dashboard itself (this file)

;;; Code:

(require 'map)
(require 'seq)
(require 'tabulated-list)
(require 'agent-shell)
(require 'agent-shell-swarm-shell)
(require 'agent-shell-swarm-events)
(require 'agent-shell-swarm-actions)
(require 'agent-shell-swarm-detail)

(defconst agent-shell-swarm--buffer-name "*agent-shell swarm*"
  "Name of the swarm dashboard buffer.")

;;; Rendering

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

;;; Mode

(defvar-keymap agent-shell-swarm-mode-map
  :doc "Keymap for `agent-shell-swarm-mode'."
  :parent tabulated-list-mode-map
  "RET" #'agent-shell-swarm-visit
  "n" #'next-line
  "p" #'previous-line
  "+" #'agent-shell-swarm-new-agent
  "s" #'agent-shell-swarm-send-prompt
  "i" #'agent-shell-swarm-interrupt
  "b" #'agent-shell-swarm-next-blocked
  "v" #'agent-shell-swarm-inspect
  "f" #'agent-shell-swarm-fork
  "m" #'agent-shell-swarm-mark
  "d" #'agent-shell-swarm-mark
  "u" #'agent-shell-swarm-unmark
  "U" #'agent-shell-swarm-unmark-all
  "x" #'agent-shell-swarm-kill-marked
  "K" #'agent-shell-swarm-kill-marked
  "D" #'agent-shell-swarm-kill
  "I" #'agent-shell-swarm-interrupt-marked)

(declare-function evil-define-key* "evil-core")

(with-eval-after-load 'evil
  ;; Evil's normal/motion states shadow the mode map, so register the
  ;; dashboard's Dired-like keys as auxiliary bindings for those states.
  (evil-define-key* '(normal motion) agent-shell-swarm-mode-map
    (kbd "RET") #'agent-shell-swarm-visit
    "n" #'next-line
    "p" #'previous-line
    "+" #'agent-shell-swarm-new-agent
    "s" #'agent-shell-swarm-send-prompt
    "i" #'agent-shell-swarm-interrupt
    "b" #'agent-shell-swarm-next-blocked
    "v" #'agent-shell-swarm-inspect
    "f" #'agent-shell-swarm-fork
    "m" #'agent-shell-swarm-mark
    "d" #'agent-shell-swarm-mark
    "u" #'agent-shell-swarm-unmark
    "U" #'agent-shell-swarm-unmark-all
    "x" #'agent-shell-swarm-kill-marked
    "K" #'agent-shell-swarm-kill-marked
    "D" #'agent-shell-swarm-kill
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
