# AGENTS.md - drover

Emacs package gathering the sessions of coding agent front ends (opencode.el first) into one place: attention tracking, one list of sessions, and actions on any session.

## Rules

- Never evaluate code in the user's running Emacs to test this package (no `emacsclient`, no Emacs MCP evaluation). Every test runs in a separate `emacs -Q --batch` process (`make test`).
- Never start, stop or restart the user's OpenCode service. Tests use the fake backend in `test/drover-test-helpers.el`, or stub the opencode.el functions they call.
- `make compile` must stay free of warnings, and `make lint` (checkdoc) clean.
- Backends are authoritative: drover renders what backends report and keeps only interface state (point, expanded sections, the last session visited), and the older sessions a backend fetched, while they are shown.
- Drover has one view of sessions, the list of `drover-list.el`, in an ordinary or a side window; improve it rather than adding another. It uses the faces and glyphs of `drover-ui.el` (strings in faces through `drover-face`, so font lock keeps them) and the commands of `drover-keys.el`; it sets `drover-view-p` and `drover-session-at-point-function`, draws `drover-view-groups` (grouped by `drover-group-by`, sorted by `drover-sort-by`), and inherits `drover-session-map` in its keymap. What a line shows is decided from the window's width and `drover-list-fields`, not by special cases.
- One key, one action: a key of `drover-session-map` means the same in every list, and no key belongs to two of the sets of `drover-dispatch` (session, prompt, global). Its List group (moving in drover's list, here or beside) uses keys of no set, but RET, which opens there and sends in prompt buffers: the menu shows one RET at a time. The list binds only standard keys of its own (RET, C-o, q, and Magit's n, p, TAB). View settings belong in the View menu, `drover-view`, not in keys of their own. The places other packages extend `drover-dispatch` from (after b and ', after p, after O) stay stable.
- Comments are sparse and explain why, not what. Docstrings are complete sentences; the first line stands alone.

## Layout

See `doc/design.org` for the module map and the main design choices.
