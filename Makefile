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
# Spyglass, which the remote layer loads as it starts: its checkout,
# with channels.  Without it, the tests of the remote layer are skipped.
SPYGLASS_EL ?= $(HOME)/local/projects/spyglass
# What drover's Package-Requires names, and what those need: a package
# manager compiles drover with these alone, as elpaca does.
REQUIRED_PATH = \
  -L $(DEPS)/magit-section -L $(DEPS)/compat -L $(DEPS)/cond-let -L $(DEPS)/llama \
  -L $(OPENCODE_EL) -L $(DEPS)/plz -L $(DEPS)/transient
LOAD_PATH = -L . -L test $(REQUIRED_PATH) \
  -L $(AGENT_SHELL_EL) -L $(DEPS)/shell-maker -L $(DEPS)/acp \
  -L $(CLAUDE_CODE_IDE_EL) -L $(DEPS)/websocket -L $(DEPS)/web-server \
  -L $(SPYGLASS_EL)
# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
BATCH = $(EMACS) -Q --batch $(LOAD_PATH) $(PREFER_NEWER)
# A batch Emacs keeps SIGPIPE's default action, which kills it when it
# writes to a peer that just closed, as the tests of the remote layer
# do; ignored signals stay ignored across exec.
NOPIPE = trap '' PIPE;
NO_WARNINGS = --eval '(setq byte-compile-error-on-warn t)'

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard drover*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile compile-standalone test test-alone lint clean bench bench-idle bench-remote real-data

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

# Standard input is closed, so that a test reading it fails rather than
# waits.
test:
	$(NOPIPE) $(BATCH) -l test/drover-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit < /dev/null

# Each test file alone, in a batch Emacs of its own, so that each
# requires what it uses rather than what files before it loaded: the
# log of a file failing shows, the others a line each.
test-alone:
	@status=0; log=$$(mktemp); \
	for test in $(TESTS); do \
	  if $(NOPIPE) $(BATCH) -l $$test -f ert-run-tests-batch-and-exit < /dev/null > "$$log" 2>&1; then \
	    echo "$$test: $$(grep '^Ran ' "$$log")"; \
	  else \
	    cat "$$log"; echo "$$test failed alone"; status=1; \
	  fi; \
	done; \
	rm -f "$$log"; exit $$status

# Times the list over thousands of sessions, as compiled: not part of
# test.  opencode.el, whose look the list draws with, is compiled too,
# as installed, in a copy of OPENCODE_EL's sources, which stay as they
# are.  EMACS_GUI names a graphical Emacs to run it in instead, where
# measuring text and collecting garbage cost what they cost the user;
# DROVER_BENCH_FONT, DROVER_BENCH_THEME and DROVER_BENCH_THEME_DIRECTORY
# choose its font and theme.  See test/drover-bench.el.
EMACS_GUI ?=
BENCH_FILES = -l test/drover-test-helpers.el -l test/drover-bench.el --eval '(drover-bench-main)'
OPENCODE_DEPS = -L $(DEPS)/plz -L $(DEPS)/transient -L $(DEPS)/cond-let -L $(DEPS)/compat -L $(DEPS)/llama
bench: compile
	@opencode=$$(mktemp -d); status=0; \
	cp $(OPENCODE_EL)/opencode*.el "$$opencode" && \
	(cd "$$opencode" && $(EMACS) -Q --batch -L . $(OPENCODE_DEPS) -f batch-byte-compile opencode*.el \
	   > /dev/null 2>&1) || { echo "Could not compile opencode.el from $(OPENCODE_EL)"; status=1; }; \
	if [ $$status -eq 0 ]; then \
	  if [ -n "$(EMACS_GUI)" ]; then \
	    out=$$(mktemp); \
	    DROVER_BENCH_OUTPUT=$$out $(EMACS_GUI) -Q --chdir $(CURDIR) -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) \
	      $(BENCH_FILES); \
	    cat $$out; rm -f $$out; \
	  else \
	    $(EMACS) -Q --batch -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) $(BENCH_FILES) || status=1; \
	  fi; \
	fi; \
	rm -rf "$$opencode"; exit $$status

# Measures what drover costs in a graphical Emacs while nothing happens,
# while a session streams, and once Emacs lost focus: processor time,
# allocation, redisplays, the minute's refresh.  Not part of test; it
# needs EMACS_GUI, and takes a few minutes.  See test/drover-idle-bench.el.
IDLE_BENCH_FILES = -l test/drover-test-helpers.el -l test/drover-idle-bench.el --eval '(drover-idle-bench-main)'
bench-idle: compile
	@if [ -z "$(EMACS_GUI)" ]; then echo "bench-idle needs EMACS_GUI, a graphical Emacs"; exit 1; fi; \
	opencode=$$(mktemp -d); status=0; \
	cp $(OPENCODE_EL)/opencode*.el "$$opencode" && \
	(cd "$$opencode" && $(EMACS) -Q --batch -L . $(OPENCODE_DEPS) -f batch-byte-compile opencode*.el \
	   > /dev/null 2>&1) || { echo "Could not compile opencode.el from $(OPENCODE_EL)"; status=1; }; \
	if [ $$status -eq 0 ]; then \
	  out=$$(mktemp -d)/idle.txt; \
	  DROVER_BENCH_OUTPUT=$$out timeout 600 $(EMACS_GUI) -Q --chdir $(CURDIR) \
	    -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) $(IDLE_BENCH_FILES) -ApplePersistenceIgnoreState YES || status=1; \
	  cat $$out; echo "Profiles: $$(dirname $$out)"; \
	fi; \
	rm -rf "$$opencode"; exit $$status

# Times the remote layer between two batch Emacs: this one following
# the 2511 sessions of an agent, here through 127.0.0.1, or with
# REMOTE=HOST on another host through SSH -W, its files in a temporary
# directory there, removed after.  BACK=PROGRAM has the agent dial back
# through SSH run as PROGRAM there, with BACK_ARGS, comma-separated.
# See test/drover-remote-bench.el.
REMOTE ?=
SSH ?= ssh
REMOTE_EMACS ?= emacs
BACK ?=
BACK_ARGS ?=
BACK_HOST ?=
bench-remote: compile
	$(NOPIPE) DROVER_BENCH_REMOTE="$(REMOTE)" DROVER_BENCH_SSH="$(SSH)" \
	DROVER_BENCH_REMOTE_EMACS="$(REMOTE_EMACS)" DROVER_BENCH_BACK="$(BACK)" \
	DROVER_BENCH_BACK_ARGS="$(BACK_ARGS)" DROVER_BENCH_BACK_HOST="$(BACK_HOST)" \
	$(BATCH) -l test/drover-test-helpers.el -l test/drover-remote-bench.el -f drover-remote-bench-main

# Indexes the user's own Claude Code transcripts and Codex rollouts,
# ~/.claude and ~/.codex or DROVER_CLAUDE_HOME and DROVER_CODEX_HOME,
# read only, and times the list over them: not part of test.  See
# test/drover-real-data.el.
real-data: compile
	$(BATCH) -l test/drover-test-helpers.el -l test/drover-real-data.el

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
