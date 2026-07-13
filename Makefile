EMACS ?= emacs
BATCH = $(EMACS) --batch --eval "(progn (require 'package) (package-initialize))" -L .

.PHONY: compile test check

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile agent-shell-swarm.el
	rm -f agent-shell-swarm.elc

test:
	$(BATCH) -l ert -l tests/agent-shell-swarm-test.el -f ert-run-tests-batch-and-exit

check: compile test
