# AGENTS.md - drover

Emacs package gathering the sessions of coding agent front ends (opencode.el, Claude Code, Codex, agent-shell, claude-code-ide) into one list, with attention tracking and actions on any session. The module map and design choices are in `doc/design.org`, the protocol between hosts in `doc/remote.org`: read the section of what you change first.

## Commands

- `make test-one FILE=test/drover-list-test.el` (`SELECTOR=regexp` for some of its tests): the file you change, in seconds. Use it while working.
- `make compile lint test`: once before merging. `compile` must stay free of warnings and `lint` (checkdoc) clean; `test` takes about 20 s, each file in a batch Emacs of its own, so no test may count on another having run.
- Benchmarks (`make bench`, `bench-bulk`, `bench-search`, `bench-idle`, `bench-usage`) only for performance work. With `EMACS_GUI` they start one hidden graphical Emacs: one run at a time.
- Core, the Windows host: `make bench-remote REMOTE=core SSH=myssh REMOTE_EMACS=/ucrt64/bin/emacs`.

## The user's Emacs

You may evaluate code in the user's running Emacs through Anvil, to inspect state or to try a change; follow `~/.emacs.d/AGENTS.md` (no buffers displayed, no prompts). drover loads over an older copy of itself, so a reload is safe.

- This Mac installs drover from this checkout. After merging to main: `(val/elpaca/update-packages 'drover :rebuild t)` with `emacs-eval-async`.
- Core runs its own drover, from GitHub: once main is pushed, `myssh core "/ucrt64/bin/emacsclient -f ~/.emacs.d/server/server --eval \"(val/elpaca/update-packages 'drover)\""`.
- Both run in the background. The outcome, such as `updated a1b2c3d → e4f5a6b · rebuilt · reloaded`, or the failure: `(list (car (gethash (val/elpaca/package-key 'drover) val/elpaca/stories)) (val/maintenance/failure 'update (val/elpaca/package-key 'drover)))`.

## Boundaries

- Never start a graphical Emacs yourself (`emacs` without `--batch`, `Emacs.app`, `open -a Emacs`): on macOS it takes the user's screen and keyboard. Only the `EMACS_GUI` benchmarks start one, hidden, through `test/gui-emacs.sh`; an experiment needing a graphical frame becomes one of those.
- Tests never use the user's OpenCode service, Claude Code or Codex sessions, Org files, repositories or ID locations: use the fake backend of `test/drover-test-helpers.el`, the throwaway homes of `test/drover-transcript-fixtures.el` and temporary directories.

## Conventions the code does not show

- Code running outside a command (timers, hooks, process filters and sentinels, callbacks, file notifications) runs its body in `drover-with-reporting`; a hook drover runs for others goes through `drover-run-hook-reporting`. Report through `drover-message`, `drover-warn` and `drover-error-message`, never `error-message-string` nor `%S` of an error.
- Text from outside is bounded where drover takes it in (`drover--bound-record`, `drover-transcript-text`). What a peer sends is JSON, never given to `read`.
- Long-lived structures hold a `stamp` slot first, are checked with `drover-layout-current-p`, and the file defining them calls `drover-note-layout`. What drover keeps reaches back to what holds it only through `drover-link`.
- An action on several sessions runs as a job (`drover--bulk-start`), never in the command.
- On an Org heading drover writes only DROVER_ properties and LOGBOOK notes (`drover-task-org-write`), and finds tasks without `org-id-find`.
- Comments are sparse and explain why. Docstrings are complete sentences; the first line stands alone.

## Finishing

Commit messages: an imperative subject, then prose paragraphs saying what changed and why. Merge the branch onto the local main, run `make compile lint test` there, push main, remove the worktree and its branch, then update drover on this Mac and on Core as above.
