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
# with channels and spyglass-reconnect.  Without it, the tests of the
# remote layer are skipped.
SPYGLASS_EL ?= $(HOME)/local/projects/spyglass
# What drover's Package-Requires names, and what those need: a package
# manager compiles drover with these alone, as elpaca does.
REQUIRED_PATH = \
  -L $(DEPS)/magit-section -L $(DEPS)/compat -L $(DEPS)/cond-let -L $(DEPS)/llama \
  -L $(OPENCODE_EL) -L $(DEPS)/transient
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
# One test file for test-one, and the tests of it to run, a regexp.
FILE ?=
SELECTOR ?=

.PHONY: all compile compile-standalone test-compile test test-alone test-one lint clean bench bench-bulk bench-search bench-idle bench-remote bench-usage real-data

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

# The byte code the tests load, as a package manager installs it,
# compiled again once a source changed, drover's or one of the packages
# it loads, whose macros expand into it; byte code whose source is gone
# is removed.  Warnings are for compile to report.
TEST_DEPENDS = $(foreach dir,$(filter-out -L . test,$(LOAD_PATH)),$(wildcard $(dir)/*.el))
test-compile:
	@for elc in drover*.elc; do [ ! -f "$$elc" ] || [ -f "$${elc%c}" ] || rm -f "$$elc"; done; \
	stale=; for elc in $(SOURCES:.el=.elc); do [ -f "$$elc" ] || stale=1; done; \
	if [ -z "$$stale" ]; then \
	  oldest=$$(ls -tr $(SOURCES:.el=.elc) | head -1); \
	  for el in $(SOURCES) $(TEST_DEPENDS); do [ "$$el" -nt "$$oldest" ] && stale=1 && break; done; \
	fi; \
	if [ -n "$$stale" ]; then \
	  log=$$(mktemp); \
	  if $(BATCH) -f batch-byte-compile $(SOURCES) > "$$log" 2>&1; then \
	    echo "Compiled $(words $(SOURCES)) files for the tests"; rm -f "$$log"; \
	  else \
	    cat "$$log"; rm -f "$$log"; exit 1; \
	  fi; \
	fi

# Each test file runs alone, in a batch Emacs of its own, so that each
# requires what it uses rather than what files before it loaded, and
# JOBS of them at once, through test/batch-tests.sh: each under TIMEOUT
# seconds and RSS_LIMIT kB of resident memory, its standard input
# closed so that a test reading it fails rather than waits.  A line
# tells each Emacs's count as it ends, then the log of each that failed
# shows.  TEST_SHARDS names the longest files, longest first, and how
# many Emacs share the tests of each, which start first: shares that
# take about as long as one another by the times of the last run.
# SELECTOR, a regexp, runs some tests only.  TEST_CACHE keeps between
# runs those times and the trampolines native compilation makes for
# primitives tests redefine (test/batch-tests.el).
JOBS ?= $(shell n=$$(getconf _NPROCESSORS_ONLN 2> /dev/null || echo 4); [ "$$n" -gt 8 ] && n=8; echo $$n)
TIMEOUT ?= 600
RSS_LIMIT ?= 2097152
TEST_CACHE ?= $(patsubst %/,%,$(or $(TMPDIR),$(TEMP),/tmp))/drover-tests
TEST_SHARDS = drover-remote-test:6 drover-list-test:4 drover-command-test:3 drover-robustness-test:2
shards = $(or $(word 2,$(subst :, ,$(filter $(1):%,$(TEST_SHARDS)))),1)
SHARDED = $(foreach shard,$(TEST_SHARDS),test/$(word 1,$(subst :, ,$(shard))).el)
TEST_JOBS = $(foreach test,$(filter $(TESTS),$(SHARDED)) $(filter-out $(SHARDED),$(TESTS)),\
  $(foreach n,$(call shards,$(basename $(notdir $(test)))),\
    $(foreach i,$(wordlist 1,$(n),1 2 3 4 5 6 7 8 9),$(test).$(i).$(n))))
TEST_LOGS ?=
TEST_ENV = SELECTOR="$(SELECTOR)" BATCH_TESTS_ELN="$(TEST_CACHE)/eln"

test: test-compile
	@logs=$$(mktemp -d "$${TMPDIR:-/tmp}/drover-tests.XXXXXX"); start=$$(date +%s); \
	mkdir "$$logs/times"; cp "$(TEST_CACHE)"/times/*.times "$$logs/times" 2> /dev/null; \
	$(MAKE) --no-print-directory -j$(JOBS) TEST_LOGS="$$logs" $(addprefix test-job/,$(TEST_JOBS)); \
	test/batch-tests.sh report "$$logs" $(JOBS) $$(($$(date +%s) - start)) "$(TEST_CACHE)/times"

# One Emacs of test: test-job/FILE.I.N runs the I-th of N shares of
# the tests of FILE, by the times of the run before this one.
test-job/%:
	@job=$*; n=$${job##*.}; job=$${job%.*}; i=$${job##*.}; file=$${job%.*}; \
	base=$$(basename $$file .el); name=$$base; [ $$n -eq 1 ] || name=$$base.$$i; \
	$(TEST_ENV) BATCH_TESTS_FILE=$$file BATCH_TESTS_SHARD=$$i/$$n \
	BATCH_TESTS_TIMES="$(TEST_LOGS)/times/$$base.times" BATCH_TESTS_TIMES_OUT="$(TEST_LOGS)/$$name.times" \
	test/batch-tests.sh run -t $(TIMEOUT) -m $(RSS_LIMIT) -o "$(TEST_LOGS)/$$name.log" -n $$name \
	  $(BATCH) -l $$file -l test/batch-tests.el -f batch-tests-run

# Each test file whole, alone, one at a time.
test-alone:
	@$(MAKE) --no-print-directory test JOBS=1 TEST_SHARDS=

# One test file, FILE, alone, or the tests of it SELECTOR matches: what
# to run while working, before the whole of test.
test-one: test-compile
	@if [ -z "$(FILE)" ]; then echo "test-one needs FILE, a test file"; exit 2; fi
	$(NOPIPE) $(TEST_ENV) BATCH_TESTS_FILE="$(FILE)" $(BATCH) -l $(FILE) -l test/batch-tests.el -f batch-tests-run \
	  < /dev/null

# Times the list over thousands of sessions, as compiled: not part of
# test.  opencode.el, whose look the list draws with, is compiled too,
# as installed, in a copy of OPENCODE_EL's sources, which stay as they
# are.  EMACS_GUI names a graphical Emacs to run it in instead, where
# measuring text and collecting garbage cost what they cost the user;
# DROVER_BENCH_FONT, DROVER_BENCH_THEME and DROVER_BENCH_THEME_DIRECTORY
# choose its font and theme.  See test/drover-bench.el.
#
# A graphical Emacs starts through test/gui-emacs.sh: on macOS through
# `open', neither activated nor shown, its frames off screen and
# transparent, so that nothing comes in front of the user's windows nor
# takes the focus; under GUI_TIMEOUT and a memory guard of 2 GB, its
# status and output returned.  Never start one otherwise.
EMACS_GUI ?=
GUI_TIMEOUT ?= 600
GUI = test/gui-emacs.sh -t $(GUI_TIMEOUT) $(EMACS_GUI)
BENCH_FILES = -l test/drover-test-helpers.el -l test/drover-bench.el --eval '(drover-bench-main)'
OPENCODE_DEPS = -L $(DEPS)/transient -L $(DEPS)/cond-let -L $(DEPS)/compat -L $(DEPS)/llama
bench: compile
	@opencode=$$(mktemp -d); status=0; \
	cp $(OPENCODE_EL)/opencode*.el "$$opencode" && \
	(cd "$$opencode" && $(EMACS) -Q --batch -L . $(OPENCODE_DEPS) -f batch-byte-compile opencode*.el \
	   > /dev/null 2>&1) || { echo "Could not compile opencode.el from $(OPENCODE_EL)"; status=1; }; \
	if [ $$status -eq 0 ]; then \
	  if [ -n "$(EMACS_GUI)" ]; then \
	    out=$$(mktemp); \
	    DROVER_BENCH_OUTPUT=$$out $(GUI) -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) \
	      $(BENCH_FILES) || status=1; \
	    cat $$out; rm -f $$out; \
	  else \
	    $(EMACS) -Q --batch -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) $(BENCH_FILES) || status=1; \
	  fi; \
	fi; \
	rm -rf "$$opencode"; exit $$status

# Times an action on every one of thousands of sessions at once, the
# list showing them: the longest Emacs is kept from its timers, and
# until the last session settled.  `make bench' runs it too; EMACS_GUI
# as for bench.  See test/drover-bulk-bench.el.
bench-bulk:
	@$(MAKE) --no-print-directory bench \
	  BENCH_FILES="-l test/drover-test-helpers.el -l test/drover-bulk-bench.el --eval '(drover-bulk-bench-main)'"

# Times the search of sessions over thousands of them, each key typed
# and its candidates, then ripgrep searching transcripts of fixtures and
# what it costs Emacs's main thread.  `make bench' runs it too; EMACS_GUI
# as for bench.  See test/drover-search-bench.el.
bench-search:
	@$(MAKE) --no-print-directory bench \
	  BENCH_FILES="-l test/drover-test-helpers.el -l test/drover-search-bench.el --eval '(drover-search-bench-run)'"

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
	  DROVER_BENCH_OUTPUT=$$out $(GUI) \
	    -L "$$opencode" $(LOAD_PATH) $(PREFER_NEWER) $(IDLE_BENCH_FILES) || status=1; \
	  cat $$out; echo "Profiles: $$(dirname $$out)"; \
	fi; \
	rm -rf "$$opencode"; exit $$status

# Times the remote layer between two batch Emacs: this one following
# the 2511 sessions of an agent, here through 127.0.0.1, or with
# REMOTE=HOST on another host through SSH -W, its files in a temporary
# directory there, removed after.  BACK=PROGRAM has the agent dial back
# through SSH run as PROGRAM there, with BACK_ARGS, comma-separated.
# SEARCH=WORDS, comma-separated, measures instead searches of what was
# said in the conversations Claude Code recorded on the agent's host,
# read only, for each word; CLONE=MEGABYTES a Claude Code conversation
# of a transcript that large cloned to the agent's host and back.  See
# test/drover-remote-bench.el.
REMOTE ?=
SSH ?= ssh
REMOTE_EMACS ?= emacs
BACK ?=
BACK_ARGS ?=
BACK_HOST ?=
SEARCH ?=
CLONE ?=
bench-remote: compile
	$(NOPIPE) DROVER_BENCH_REMOTE="$(REMOTE)" DROVER_BENCH_SSH="$(SSH)" \
	DROVER_BENCH_REMOTE_EMACS="$(REMOTE_EMACS)" DROVER_BENCH_BACK="$(BACK)" \
	DROVER_BENCH_BACK_ARGS="$(BACK_ARGS)" DROVER_BENCH_BACK_HOST="$(BACK_HOST)" \
	DROVER_BENCH_SEARCH="$(SEARCH)" DROVER_BENCH_CLONE="$(CLONE)" \
	$(BATCH) -l test/drover-test-helpers.el -l test/drover-remote-bench.el -f drover-remote-bench-main

# Times the totals of usage over OpenCode's database of 2500 sessions
# and 120 Claude Code transcripts, written in a throwaway home: the
# first read in slices, the facts, each session's totals, the reports,
# the list with its costs, a turn read, and an Emacs starting from what
# was kept.  See test/drover-usage-bench.el.
bench-usage: compile
	$(BATCH) -l test/drover-test-helpers.el -l test/drover-usage-bench.el -f drover-usage-bench-main

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
