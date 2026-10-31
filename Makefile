# Build and test drover in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages, one subdirectory
# each, such as elpaca's builds directory.  OPENCODE_EL is the
# opencode.el checkout the OpenCode backend compiles and tests against:
# its sources, since a build directory lags behind new modules.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
OPENCODE_EL ?= $(HOME)/.emacs.d/elpaca/sources/opencode
LOAD_PATH = -L . -L test \
  -L $(DEPS)/magit-section -L $(DEPS)/compat -L $(DEPS)/cond-let -L $(DEPS)/llama \
  -L $(OPENCODE_EL) -L $(DEPS)/plz -L $(DEPS)/transient
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
