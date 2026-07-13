;;; agent-shell-swarm-test.el --- Tests for agent-shell-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'map)
(require 'agent-shell-swarm)

(cl-defun agent-shell-swarm-test--make-shell
    (name &key (agent "Claude") model mode ctx-used ctx-size cost currency
          age session-id client-process (dir "/tmp/") title no-shell-maker
          tool-calls supports-fork)
  "Create a fake `agent-shell-mode' buffer NAME populated for tests.

NO-SHELL-MAKER omits the shell-maker config, simulating a shell
that hasn't finished initializing (its status reads as \"?\")."
  (with-current-buffer (get-buffer-create name)
    (setq major-mode 'agent-shell-mode)
    (unless no-shell-maker
      (setq-local shell-maker--config (make-shell-maker-config :name "agent")))
    (setq-local agent-shell--state
                (list (cons :agent-config (list (cons :mode-line-name agent)))
                      (cons :buffer (current-buffer))
                      (cons :event-subscriptions nil)
                      (cons :tool-calls tool-calls)
                      (cons :supports-session-fork supports-fork)
                      (cons :client (and client-process
                                         (list (cons :process client-process))))
                      (cons :last-activity-time
                            (and age (time-subtract (current-time) age)))
                      (cons :usage (list (cons :context-used (or ctx-used 0))
                                         (cons :context-size (or ctx-size 0))
                                         (cons :cost-amount (or cost 0))
                                         (cons :cost-currency currency)))
                      (cons :session
                            (list (cons :id session-id)
                                  (cons :model-id model)
                                  (cons :mode-id mode)
                                  (cons :modes
                                        (when mode
                                          (list (list (cons :id mode)
                                                      (cons :name mode)))))
                                  (cons :title title)))))
    (setq-local default-directory dir)
    (current-buffer)))

(defmacro agent-shell-swarm-test--with-swarm (&rest body)
  "Run BODY, then kill all fake shells and any dashboard buffer."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (let ((kill-buffer-query-functions nil)
           (kill-buffer-hook nil))
       (dolist (buffer (buffer-list))
         (when (with-current-buffer buffer
                 (or (derived-mode-p 'agent-shell-mode)
                     (derived-mode-p 'agent-shell-swarm-mode)))
           (kill-buffer buffer))))))

(defun agent-shell-swarm-test--dashboard ()
  "Return the dashboard's contents as a plain string."
  (with-current-buffer agent-shell-swarm--buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agent-shell-swarm-test--goto-row (name)
  "Move point in the dashboard to the row for buffer NAME."
  (with-current-buffer agent-shell-swarm--buffer-name
    (goto-char (point-min))
    (while (not (equal (and (tabulated-list-get-id)
                            (buffer-name (tabulated-list-get-id)))
                       name))
      (when (eobp)
        (error "No dashboard row for %s" name))
      (forward-line 1))))

;;; Rendering

(ert-deftest agent-shell-swarm-test-entry-full-state ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell
     "Claude Agent @ demo"
     :model "fable" :mode "Default" :ctx-used 96000 :ctx-size 128000
     :cost 1.41 :currency "USD" :age 154 :session-id "sid"
     :dir "/tmp/demo/" :title "Fix the flaky test")
    (agent-shell-swarm)
    (let ((content (agent-shell-swarm-test--dashboard)))
      (dolist (expected '("Claude Agent @ demo" "ready" "Claude" "fable"
                          "Default" "75%" "USD1.41" "2m" "/tmp/demo/"
                          "Fix the flaky test"))
        (should (string-match-p (regexp-quote expected) content))))))

(ert-deftest agent-shell-swarm-test-entry-sparse-state ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Bare Agent" :agent "Pi")
    (agent-shell-swarm)
    (should (string-match-p "Bare Agent +ready +Pi"
                            (agent-shell-swarm-test--dashboard)))))

(ert-deftest agent-shell-swarm-test-status-unknown ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Half Baked" :no-shell-maker t)
    (agent-shell-swarm)
    (should (string-match-p "Half Baked +?" (agent-shell-swarm-test--dashboard)))))

(ert-deftest agent-shell-swarm-test-status-dead ()
  (agent-shell-swarm-test--with-swarm
    (let ((process (make-process :name "swarm-test-dead" :command '("true"))))
      (while (process-live-p process)
        (accept-process-output nil 0.05))
      (agent-shell-swarm-test--make-shell "Dead Agent" :client-process process)
      (agent-shell-swarm)
      (should (string-match-p "Dead Agent +dead"
                              (agent-shell-swarm-test--dashboard))))))

(ert-deftest agent-shell-swarm-test-fit-row-truncation ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell
     "An Extremely Long Buffer Name Exceeding Thirty Chars"
     :model "Default (recommended)"
     :dir "/tmp/a/very/deeply/nested/directory/path/")
    (agent-shell-swarm)
    (let ((content (agent-shell-swarm-test--dashboard)))
      (should (string-match-p (regexp-quote "An Extremely Long Buffer Na...") content))
      (should (string-match-p (regexp-quote "Default (recomm...") content))
      ;; Directory keeps its tail, not its head.
      (should (string-match-p (regexp-quote "nested/directory/path/") content))
      (should-not (string-match-p (regexp-quote ".../tmp/a") content)))))

(ert-deftest agent-shell-swarm-test-multiline-title-stays-one-row ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell
     "ML Agent" :session-id "s" :title "First line.\n\nSecond line")
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (should (= 1 (count-lines (point-min) (point-max)))))
    (should (string-match-p "First line. Second line"
                            (agent-shell-swarm-test--dashboard)))))

;;; Commands

(ert-deftest agent-shell-swarm-test-kill ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Doomed Agent")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Doomed Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (agent-shell-swarm-kill))
      (should-not (get-buffer "Doomed Agent"))
      (should (= 0 (count-lines (point-min) (point-max)))))))

(ert-deftest agent-shell-swarm-test-kill-declined ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Spared Agent")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Spared Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (agent-shell-swarm-kill))
      (should (get-buffer "Spared Agent")))))

(ert-deftest agent-shell-swarm-test-interrupt-busy ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Busy Agent" :session-id "s")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Busy Agent")
    (let (interrupted)
      (cl-letf (((symbol-function 'agent-shell-swarm--status)
                 (lambda (_) "busy"))
                ((symbol-function 'agent-shell-interrupt)
                 (lambda (&rest _) (setq interrupted (current-buffer)))))
        (with-current-buffer agent-shell-swarm--buffer-name
          (agent-shell-swarm-interrupt)))
      ;; Interrupt must run inside the shell's own buffer.
      (should (eq interrupted (get-buffer "Busy Agent"))))))

(ert-deftest agent-shell-swarm-test-interrupt-ready-is-noop ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Idle Agent" :session-id "s")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Idle Agent")
    (let (interrupted)
      (cl-letf (((symbol-function 'agent-shell-interrupt)
                 (lambda (&rest _) (setq interrupted t))))
        (with-current-buffer agent-shell-swarm--buffer-name
          (agent-shell-swarm-interrupt)))
      (should-not interrupted))))

(ert-deftest agent-shell-swarm-test-send-prompt ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Ready Agent" :session-id "sid")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Ready Agent")
    (let (sent)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (cl-function
                  (lambda (&key shell-buffer text submit no-focus)
                    (setq sent (list shell-buffer text submit no-focus))))))
        (with-current-buffer agent-shell-swarm--buffer-name
          (agent-shell-swarm-send-prompt)))
      (should (equal sent (list (get-buffer "Ready Agent") "do the thing" t t))))))

(ert-deftest agent-shell-swarm-test-send-requires-session ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Init Agent")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Init Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (should-error (agent-shell-swarm-send-prompt) :type 'user-error))))

(ert-deftest agent-shell-swarm-test-send-to-busy-declined ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Busy Agent" :session-id "sid")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Busy Agent")
    (cl-letf (((symbol-function 'agent-shell-swarm--status) (lambda (_) "busy"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'agent-shell--insert-to-shell-buffer)
               (lambda (&rest _) (error "Must not send"))))
      (with-current-buffer agent-shell-swarm--buffer-name
        (should-error (agent-shell-swarm-send-prompt) :type 'user-error)))))

(ert-deftest agent-shell-swarm-test-new-agent ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Existing" :dir "/tmp/existing/")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Existing")
    (let (started started-dir dir-default)
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (_prompt initial &rest _)
                   (setq dir-default initial)
                   "/tmp/chosen/"))
                ((symbol-function 'agent-shell-select-config)
                 (lambda (&rest _) '((:buffer-name . "Fake"))))
                ((symbol-function 'agent-shell--start)
                 (cl-function
                  (lambda (&key config no-focus new-session &allow-other-keys)
                    (setq started (list config no-focus new-session)
                          started-dir default-directory)))))
        (with-current-buffer agent-shell-swarm--buffer-name
          (agent-shell-swarm-new-agent)))
      ;; Directory prompt defaults to the row at point's directory.
      (should (equal dir-default "/tmp/existing/"))
      ;; The shell starts in the chosen directory, in the background.
      (should (equal started-dir "/tmp/chosen/"))
      (should (equal started '(((:buffer-name . "Fake")) t t))))))

(ert-deftest agent-shell-swarm-test-next-blocked-cycles ()
  (agent-shell-swarm-test--with-swarm
    (dolist (name '("A" "B" "C" "D"))
      (agent-shell-swarm-test--make-shell name))
    (cl-letf (((symbol-function 'agent-shell-swarm--status)
               (lambda (buffer)
                 (if (member (buffer-name buffer) '("B" "D")) "blocked" "ready"))))
      (agent-shell-swarm)
      (with-current-buffer agent-shell-swarm--buffer-name
        (goto-char (point-min))
        (agent-shell-swarm-next-blocked)
        (let ((first (buffer-name (tabulated-list-get-id))))
          (should (member first '("B" "D")))
          (agent-shell-swarm-next-blocked)
          (let ((second (buffer-name (tabulated-list-get-id))))
            (should (member second '("B" "D")))
            (should-not (equal first second))
            ;; Third jump wraps back around.
            (agent-shell-swarm-next-blocked)
            (should (equal (buffer-name (tabulated-list-get-id)) first))))))))

(ert-deftest agent-shell-swarm-test-next-blocked-none ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Calm Agent")
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (goto-char (point-min))
      (let ((before (point)))
        (agent-shell-swarm-next-blocked)
        (should (= before (point)))))))

;;; Summary

(ert-deftest agent-shell-swarm-test-summary ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "A" :cost 0.30 :currency "USD" :session-id "s")
    (agent-shell-swarm-test--make-shell "B" :cost 5.62 :currency "USD" :session-id "s")
    (cl-letf (((symbol-function 'agent-shell-swarm--status)
               (lambda (buffer)
                 (if (equal (buffer-name buffer) "A") "busy" "blocked"))))
      (agent-shell-swarm)
      (with-current-buffer agent-shell-swarm--buffer-name
        ;; `format-mode-line' is unreliable in batch mode; read the
        ;; summary string straight out of `mode-line-process'.
        (let ((summary (substring-no-properties (cadr mode-line-process))))
          (should (string-match-p "2 agents" summary))
          (should (string-match-p "1 busy" summary))
          (should (string-match-p "1 blocked" summary))
          (should (string-match-p "USD5.92" summary)))))))

(ert-deftest agent-shell-swarm-test-summary-quiet-when-idle ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "A" :session-id "s")
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (let ((summary (substring-no-properties (cadr mode-line-process))))
        (should (string-match-p "1 agent" summary))
        (should-not (string-match-p "busy\\|blocked\\|\\$" summary))))))

;;; Event-driven refresh

(ert-deftest agent-shell-swarm-test-subscribes-to-shells ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "A")
    (agent-shell-swarm-test--make-shell "B")
    (agent-shell-swarm)
    (dolist (name '("A" "B"))
      (should (= 1 (length (map-elt (buffer-local-value
                                     'agent-shell--state (get-buffer name))
                                    :event-subscriptions)))))
    ;; Re-invoking the dashboard must not stack duplicate subscriptions.
    (agent-shell-swarm)
    (dolist (name '("A" "B"))
      (should (= 1 (length (map-elt (buffer-local-value
                                     'agent-shell--state (get-buffer name))
                                    :event-subscriptions)))))))

(ert-deftest agent-shell-swarm-test-event-triggers-refresh ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Evented" :session-id "s" :title "Old title")
    (agent-shell-swarm)
    (with-current-buffer "Evented"
      (map-put! (map-elt agent-shell--state :session) :title "Fresh title")
      (agent-shell--emit-event :event 'turn-complete))
    ;; Refresh is debounced; let the timer fire.
    (sit-for (+ agent-shell-swarm-refresh-debounce 0.2))
    (should (string-match-p "Fresh title" (agent-shell-swarm-test--dashboard)))))

(ert-deftest agent-shell-swarm-test-unsubscribes-on-kill ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Watched")
    (agent-shell-swarm)
    (should (= 1 (length (map-elt (buffer-local-value
                                   'agent-shell--state (get-buffer "Watched"))
                                  :event-subscriptions))))
    (kill-buffer agent-shell-swarm--buffer-name)
    (should (= 0 (length (map-elt (buffer-local-value
                                   'agent-shell--state (get-buffer "Watched"))
                                  :event-subscriptions))))))

(ert-deftest agent-shell-swarm-test-resubscribes-on-handler-version-bump ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Versioned" :session-id "s")
    ;; Subscribe under an old handler version...
    (let ((agent-shell-swarm--handler-version 1))
      (agent-shell-swarm))
    ;; ...then refresh under the current one: replaced, not duplicated.
    (with-current-buffer agent-shell-swarm--buffer-name
      (revert-buffer))
    (should (= 1 (length (map-elt (buffer-local-value
                                   'agent-shell--state (get-buffer "Versioned"))
                                  :event-subscriptions))))
    ;; The fresh subscription runs the current handler.
    (with-current-buffer "Versioned"
      (agent-shell--emit-event :event 'file-write
                               :data '((:path . "/tmp/after-reload.py")))
      (should (equal agent-shell-swarm--changed-files
                     '("/tmp/after-reload.py"))))))

;;; Marks and bulk operations

(ert-deftest agent-shell-swarm-test-marks-survive-revert ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Marked")
    (agent-shell-swarm-test--make-shell "Unmarked")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Marked")
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-mark)
      (revert-buffer)
      (should (equal (mapcar #'buffer-name (agent-shell-swarm--marked-shells))
                     '("Marked"))))))

(ert-deftest agent-shell-swarm-test-kill-marked ()
  (agent-shell-swarm-test--with-swarm
    (dolist (name '("A" "B" "C"))
      (agent-shell-swarm-test--make-shell name))
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-test--goto-row "A")
      (agent-shell-swarm-mark)
      (agent-shell-swarm-test--goto-row "C")
      (agent-shell-swarm-mark)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (agent-shell-swarm-kill-marked))
      (should-not (get-buffer "A"))
      (should (get-buffer "B"))
      (should-not (get-buffer "C")))))

(ert-deftest agent-shell-swarm-test-interrupt-marked-only-busy ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "BusyOne")
    (agent-shell-swarm-test--make-shell "IdleOne")
    (cl-letf (((symbol-function 'agent-shell-swarm--status)
               (lambda (buffer)
                 (if (equal (buffer-name buffer) "BusyOne") "busy" "ready"))))
      (agent-shell-swarm)
      (with-current-buffer agent-shell-swarm--buffer-name
        (agent-shell-swarm-test--goto-row "BusyOne")
        (agent-shell-swarm-mark)
        (agent-shell-swarm-test--goto-row "IdleOne")
        (agent-shell-swarm-mark)
        (let (interrupted)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'agent-shell-interrupt)
                     (lambda (&rest _) (push (buffer-name) interrupted))))
            (agent-shell-swarm-interrupt-marked))
          (should (equal interrupted '("BusyOne"))))))))

;;; Detail view and permissions

(defconst agent-shell-swarm-test--pending-tool-calls
  '(("tc-1" . ((:title . "Run make test")
               (:kind . "execute")
               (:status . "pending")
               (:permission-request-id . 42)
               (:raw-input . ((command . "make test")))
               (:permission-actions . (((:option . "Allow")
                                        (:option-id . "allow")
                                        (:kind . "allow_once"))
                                       ((:option . "Reject")
                                        (:option-id . "reject")
                                        (:kind . "reject_once"))))))
    ("tc-0" . ((:title . "Read foo.py")
               (:kind . "read")
               (:status . "completed")))))

(ert-deftest agent-shell-swarm-test-detail-renders ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell
     "Blocked Agent" :session-id "s"
     :tool-calls (copy-tree agent-shell-swarm-test--pending-tool-calls))
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Blocked Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-inspect))
    (with-current-buffer agent-shell-swarm--detail-buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Pending permission" content))
        (should (string-match-p "Run make test" content))
        (should (string-match-p "command: make test" content))
        (should (string-match-p "Allow · Reject" content))
        ;; Completed tool call listed too.
        (should (string-match-p "\\[read\\] Read foo.py — completed" content))))))

(ert-deftest agent-shell-swarm-test-detail-answer-permission ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell
     "Blocked Agent" :session-id "s"
     :tool-calls (copy-tree agent-shell-swarm-test--pending-tool-calls))
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Blocked Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-inspect))
    (let (response)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Allow"))
                ((symbol-function 'agent-shell--send-permission-response)
                 (cl-function
                  (lambda (&key request-id option-id tool-call-id &allow-other-keys)
                    (setq response (list request-id option-id tool-call-id))))))
        (with-current-buffer agent-shell-swarm--detail-buffer-name
          (agent-shell-swarm-detail-answer-permission)))
      (should (equal response '(42 "allow" "tc-1"))))))

(ert-deftest agent-shell-swarm-test-detail-answer-without-pending-errors ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Calm Agent" :session-id "s")
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Calm Agent")
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-inspect))
    (with-current-buffer agent-shell-swarm--detail-buffer-name
      (should-error (agent-shell-swarm-detail-answer-permission)
                    :type 'user-error))))

;;; Changed-files tracking

(ert-deftest agent-shell-swarm-test-file-write-tracking ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Writer" :session-id "s" :dir "/tmp/repo/")
    (agent-shell-swarm)
    (with-current-buffer "Writer"
      (agent-shell--emit-event :event 'file-write
                               :data '((:path . "/tmp/repo/src/foo.py")))
      (agent-shell--emit-event :event 'file-write
                               :data '((:path . "/tmp/elsewhere/bar.py")))
      ;; Duplicate write must not double up.
      (agent-shell--emit-event :event 'file-write
                               :data '((:path . "/tmp/repo/src/foo.py")))
      (should (equal agent-shell-swarm--changed-files
                     '("/tmp/elsewhere/bar.py" "/tmp/repo/src/foo.py"))))
    (agent-shell-swarm-test--goto-row "Writer")
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-inspect))
    (with-current-buffer agent-shell-swarm--detail-buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Changed files (2):" content))
        ;; Inside the shell's directory → relative; outside → absolute.
        (should (string-match-p "^  src/foo\\.py$" content))
        (should (string-match-p "^  /tmp/elsewhere/bar\\.py$" content))))))

(ert-deftest agent-shell-swarm-test-tool-call-file-tracking ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Editor" :session-id "s" :dir "/tmp/repo/")
    (agent-shell-swarm)
    (with-current-buffer "Editor"
      ;; Shapes as observed live from Claude Code: locations is a
      ;; vector of alists with symbol keys.
      (agent-shell--emit-event
       :event 'tool-call-update
       :data `((:tool-call-id . "tc-1")
               (:tool-call . ((:kind . "edit")
                              (:status . "completed")
                              (:locations . [((path . "/tmp/repo/foo.py"))])))))
      ;; Pending edits and completed non-file tools must not record.
      (agent-shell--emit-event
       :event 'tool-call-update
       :data `((:tool-call-id . "tc-2")
               (:tool-call . ((:kind . "edit")
                              (:status . "pending")
                              (:locations . [((path . "/tmp/repo/pending.py"))])))))
      (agent-shell--emit-event
       :event 'tool-call-update
       :data `((:tool-call-id . "tc-3")
               (:tool-call . ((:kind . "execute")
                              (:status . "completed")
                              (:locations . [((path . "/tmp/repo/ran.sh"))])))))
      (should (equal agent-shell-swarm--changed-files '("/tmp/repo/foo.py"))))))

;;; Fork

(ert-deftest agent-shell-swarm-test-fork ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Origin" :session-id "sid-1" :dir "/tmp/repo/"
                                        :supports-fork t)
    (agent-shell-swarm)
    (agent-shell-swarm-test--goto-row "Origin")
    (let (started started-dir)
      (cl-letf (((symbol-function 'agent-shell--start)
                 (cl-function
                  (lambda (&key config fork-session-id new-session no-focus
                           &allow-other-keys)
                    (setq started (list config fork-session-id new-session no-focus)
                          started-dir default-directory)))))
        (with-current-buffer agent-shell-swarm--buffer-name
          (agent-shell-swarm-fork)))
      (should (equal (map-elt (car started) :mode-line-name) "Claude"))
      (should (equal (cdr started) '("sid-1" t t)))
      (should (equal started-dir "/tmp/repo/")))))

(ert-deftest agent-shell-swarm-test-fork-requires-session-and-support ()
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "No Session")
    (agent-shell-swarm-test--make-shell "No Fork" :session-id "sid-2")
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (agent-shell-swarm-test--goto-row "No Session")
      (should-error (agent-shell-swarm-fork) :type 'user-error)
      (agent-shell-swarm-test--goto-row "No Fork")
      (should-error (agent-shell-swarm-fork) :type 'user-error))))

;;; Evil integration

(ert-deftest agent-shell-swarm-test-evil-bindings ()
  (skip-unless (require 'evil nil t))
  (agent-shell-swarm-test--with-swarm
    (agent-shell-swarm-test--make-shell "Evil Agent")
    (agent-shell-swarm)
    (with-current-buffer agent-shell-swarm--buffer-name
      (evil-local-mode 1)
      (evil-normal-state)
      (dolist (binding '(("s" . agent-shell-swarm-send-prompt)
                         ("i" . agent-shell-swarm-interrupt)
                         ("x" . agent-shell-swarm-kill)
                         ("n" . agent-shell-swarm-new-agent)
                         ("b" . agent-shell-swarm-next-blocked)
                         ("d" . agent-shell-swarm-inspect)
                         ("f" . agent-shell-swarm-fork)
                         ("m" . agent-shell-swarm-mark)
                         ("u" . agent-shell-swarm-unmark)
                         ("U" . agent-shell-swarm-unmark-all)
                         ("K" . agent-shell-swarm-kill-marked)
                         ("I" . agent-shell-swarm-interrupt-marked)
                         ("gr" . revert-buffer)))
        (should (eq (key-binding (kbd (car binding))) (cdr binding))))
      ;; k must remain evil navigation, not kill.
      (should-not (eq (key-binding "k") 'agent-shell-swarm-kill)))))

(provide 'agent-shell-swarm-test)

;;; agent-shell-swarm-test.el ends here
