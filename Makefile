# Build and test drover in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages, one subdirectory
# each, such as elpaca's builds directory.  OPENCODE_EL, AGENT_SHELL_EL
# and CLAUDE_CODE_IDE_EL are the checkouts of the front ends drover's
# backends compile and test against: their sources, since a build
# directory lags behind new modules.  agent-shell needs shell-maker and
# acp, claude-code-ide websocket and web-server, all from DEPS.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
OPENCODE_EL ?= $(HOME)/.emacs.d/elpaca/sources/opencode
AGENT_SHELL_EL ?= $(HOME)/.emacs.d/elpaca/sources/agent-shell
CLAUDE_CODE_IDE_EL ?= $(HOME)/.emacs.d/elpaca/sources/claude-code-ide
LOAD_PATH = -L . -L test \
  -L $(DEPS)/magit-section -L $(DEPS)/compat -L $(DEPS)/cond-let -L $(DEPS)/llama \
  -L $(OPENCODE_EL) -L $(DEPS)/plz -L $(DEPS)/transient \
  -L $(AGENT_SHELL_EL) -L $(DEPS)/shell-maker -L $(DEPS)/acp \
  -L $(CLAUDE_CODE_IDE_EL) -L $(DEPS)/websocket -L $(DEPS)/web-server
# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
BATCH = $(EMACS) -Q --batch $(LOAD_PATH) $(PREFER_NEWER)

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard drover*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile test lint clean

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -l test/drover-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
