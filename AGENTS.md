# AGENTS.md - drover

Emacs package gathering the sessions of coding agent front ends (opencode.el first) into one place: attention tracking, dashboards, and actions on any session.

## Rules

- Never evaluate code in the user's running Emacs to test this package (no `emacsclient`, no Emacs MCP evaluation). Every test runs in a separate `emacs -Q --batch` process (`make test`).
- Never start, stop or restart the user's OpenCode service. Tests use the fake backend in `test/drover-test-helpers.el`, or stub the opencode.el functions they call.
- `make compile` must stay free of warnings, and `make lint` (checkdoc) clean.
- Backends are authoritative: drover renders what backends report and keeps only interface state (point, expanded sections, the last session visited).
- Every view of sessions uses the faces and glyphs of `drover-ui.el` (strings in faces through `drover-face`, so font lock keeps them) and the commands of `drover-keys.el`; it sets `drover-view-p` and `drover-session-at-point-function`, draws `drover-view-trees` grouped by `drover-group-by`, and inherits `drover-session-map` in its keymap.
- One key, one action: a key of `drover-session-map` means the same in every list, and no key belongs to two of the sets of `drover-dispatch` (session, prompt, global). Views bind only standard keys of their own (RET, n, p, TAB, q).
- Comments are sparse and explain why, not what. Docstrings are complete sentences; the first line stands alone.

## Layout

See `doc/design.org` for the module map and the main design choices.
