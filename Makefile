EMACS ?= emacs
BATCH = $(EMACS) --batch --eval "(progn (require 'package) (package-initialize))" -L .

ELS = agent-shell-swarm-shell.el \
      agent-shell-swarm-events.el \
      agent-shell-swarm-actions.el \
      agent-shell-swarm-detail.el \
      agent-shell-swarm.el

.PHONY: compile test check

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(ELS)
	rm -f *.elc

test:
	$(BATCH) -l ert -l tests/agent-shell-swarm-test.el -f ert-run-tests-batch-and-exit

check: compile test
