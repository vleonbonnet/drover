# Build and test drover in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages, one subdirectory
# each, such as elpaca's builds directory.  OPENCODE_EL, AGENT_SHELL_EL
# and CLAUDE_CODE_IDE_EL are the checkouts of the front ends drover's
# backends compile and test against: their sources, since a build
# directory lags behind new modules.  agent-shell needs shell-maker and
# acp, claude-code-ide websocket and web-server, all from DEPS.  Give
# them as absolute file names: compile-standalone compiles elsewhere.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
OPENCODE_EL ?= $(HOME)/.emacs.d/elpaca/sources/opencode
AGENT_SHELL_EL ?= $(HOME)/.emacs.d/elpaca/sources/agent-shell
CLAUDE_CODE_IDE_EL ?= $(HOME)/.emacs.d/elpaca/sources/claude-code-ide
# What drover's Package-Requires names, and what those need: a package
# manager compiles drover with these alone, as elpaca does.
REQUIRED_PATH = \
  -L $(DEPS)/magit-section -L $(DEPS)/compat -L $(DEPS)/cond-let -L $(DEPS)/llama \
  -L $(OPENCODE_EL) -L $(DEPS)/plz -L $(DEPS)/transient
LOAD_PATH = -L . -L test $(REQUIRED_PATH) \
  -L $(AGENT_SHELL_EL) -L $(DEPS)/shell-maker -L $(DEPS)/acp \
  -L $(CLAUDE_CODE_IDE_EL) -L $(DEPS)/websocket -L $(DEPS)/web-server
# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
BATCH = $(EMACS) -Q --batch $(LOAD_PATH) $(PREFER_NEWER)
NO_WARNINGS = --eval '(setq byte-compile-error-on-warn t)'

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard drover*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile compile-standalone test lint clean

all: compile test

compile:
	$(BATCH) $(NO_WARNINGS) -f batch-byte-compile $(SOURCES)
	@$(MAKE) --no-print-directory compile-standalone

# Every file, the backends of optional front ends included, compiles
# with the required packages alone, as a package manager compiles it,
# in a copy of the sources so that no front end's checkout is found
# beside them.  Its byte code must be the byte code compiled with the
# front ends present, if any, so that the backends work either way: the
# same but for the numbers of uninterned symbols, which count those
# made before in the compiling Emacs.
UNNUMBER = LC_ALL=C sed -E 's/(\#:[A-Za-z_-]*[A-Za-z_-])[0-9]+/\1/g'
compile-standalone:
	@build=$$(mktemp -d); status=0; \
	cp $(SOURCES) "$$build" && \
	(cd "$$build" && $(EMACS) -Q --batch -L . $(REQUIRED_PATH) $(PREFER_NEWER) $(NO_WARNINGS) \
	   -f batch-byte-compile $(SOURCES)) || status=1; \
	if [ $$status -eq 0 ]; then \
	  for elc in $(SOURCES:.el=.elc); do \
	    if [ -f "$$elc" ]; then \
	      $(UNNUMBER) "$$elc" > "$$build/present"; \
	      $(UNNUMBER) "$$build/$$elc" > "$$build/alone"; \
	      if ! cmp -s "$$build/present" "$$build/alone"; then \
	        echo "$$elc differs when compiled without the optional front ends"; status=1; \
	      fi; \
	    fi; \
	  done; \
	fi; \
	rm -rf "$$build"; \
	[ $$status -ne 0 ] || echo "Compiled with the required packages alone: $(SOURCES)"; \
	exit $$status

test:
	$(BATCH) -l test/drover-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
