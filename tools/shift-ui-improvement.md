# Future EPW UI improvement checklist

This document is the implementation and acceptance checklist for the second
round of Future EPW workflow UI work. It records the remaining problems found
after commit `5e2ef36` and is updated as each item is completed.

## Goal

Keep scientific work, persisted run state, foreground output, background watch,
and CLI output on one semantic progress model. Every potentially long operation
must identify the active stage and business unit before it starts, continue to
show liveness while it is running, and leave an actionable terminal result.

## Problems and required fixes

### 1. Reporter ownership and long-operation liveness

- [x] Disable native Downloader progress whenever a `ShiftReporter` owns the
  workflow UI, including HTTP fallback inside extraction.
- [x] Bridge extraction fallback downloads into the workflow reporter so log
  mode cannot become silent and dynamic mode cannot create competing progress
  regions.
- [x] Keep elapsed time visibly moving during synchronous OPeNDAP/RNetCDF work,
  even when the underlying library cannot report byte progress.
- [x] Preserve the actual access method used by an extraction plan after an
  OPeNDAP-to-HTTP fallback.

### 2. Semantic status model

- [x] Replace the universal `Cases ready / outputs` row with stage-specific
  metrics for resolver, download, extraction, coverage, morph, and EPW output.
- [x] Keep the status region height stable while allowing its content to adapt
  to the current stage.
- [x] Keep `Recent` as the last completed, skipped, rejected, fallback, or failed
  milestone; transient updates must not overwrite it.
- [x] Add the next workflow stage to the live state.
- [x] Support aggregate download progress and multiple active files instead of
  representing all concurrent work as one mutable current unit.

### 3. Heartbeat and live watch state

- [x] Merge heartbeat details into the in-memory current state so catalog role,
  transfer metrics, and access method are not lost.
- [x] Separate Console refresh cadence from durable job heartbeat cadence.
  `heartbeat = 0` must not rewrite the DuckDB-backed live sidecar on every curl
  callback.
- [x] Publish transient semantic UI state in the background live sidecar without
  writing animation-only events to `shift_run_event`.
- [x] Let `shift_watch()` prefer that transient state while retaining persisted
  events as the durable reconstruction path.

### 4. Download progress

- [x] Surface aggregate bytes, speed, ETA, active task count, and up to two active
  filenames when those values are known.
- [x] Do not invent a workflow-wide ETA; ETA applies only to measured downloads.
- [x] Keep normal output compact and expose filenames and low-level transfer
  context only at detail/debug levels.

### 5. Width, reflow, and log integrity

- [x] Respect actual terminal widths below 40 columns.
- [x] Use display-width-aware trimming and padding for CJK and ANSI text.
- [x] Constrain dynamic rows and compact tables, but never truncate append-only
  or redirected log messages.
- [x] Wrap detailed missing-case reasons below their case rows instead of
  compressing them into a single table cell.
- [x] Keep resolver tables usable on narrow terminals and show attempt duration
  and a short failure category when available.

### 6. Copy, startup, completion, and errors

- [x] Remove duplicated context such as `[Resolve][DKRZ][future] DKRZ · future`;
  structured context belongs to the reporter prefix and the message describes
  only the action or result.
- [x] Include the output directory and unresolved member/grid choices in the
  startup summary without restoring the original long specification dump.
- [x] For background runs, print exact watch, cancel, and logs commands including
  a custom store path.
- [x] Include the run identity in watched live status.
- [x] Show the output directory once in the normal completion summary and list
  individual files only in detail/debug output.
- [x] Use short node names in normal failure context and reserve full URLs for
  debug output.

### 7. R/CLI watch and logs

- [x] Keep foreground and watch views on the same semantic state and detail
  rules.
- [x] Make CLI JSONL follow output a typed initial snapshot plus event deltas and
  a terminal snapshot, rather than repeated untyped full snapshots.
- [x] Identify whether `shift_logs()` rows came from persisted workflow events
  or a background process log.
- [x] Keep human progress completely separate from `--json`, `--jsonl`, and
  `--quiet` output.

### 8. Accessibility and non-interactive behavior

- [x] Preserve textual status labels so colour is never the only signal.
- [x] Treat `TERM=dumb`, CI, and non-dynamic output as append-only log mode.
- [x] Document log mode as the stable option for screen readers, reduced-motion
  use, captured logs, and redirected output.

### 9. Validation

- [x] Add unit tests for narrow terminals, CJK width, stage-specific metrics,
  heartbeat throttling, transient live state, fallback download bridging,
  access-method accuracy, and typed JSONL deltas.
- [x] Run focused UI/workflow/CLI tests.
- [x] Run the complete test suite.
- [x] Run `R CMD check` with no errors, warnings, or notes attributable to this
  work.

### 10. Motion and live-dashboard refinement

- [x] Add an explicit `motion` policy and presentation-only `refresh` cadence
  without adding either value to the scientific plan or `spec_hash`.
- [x] Keep animation frames independent from durable job heartbeat writes and
  cooperative cancellation checks.
- [x] Replace the plain four-row text block with a stable stage rail,
  determinate or indeterminate progress, current-unit context, stage metrics,
  and a three-row context section.
- [x] Animate indefinite work only in dynamic terminals; use a stable marker for
  reduced motion and no animation in logs, JSON, JSONL, or null output.
- [x] Show the active resolver node together with recent node outcomes while
  resolution is still running.
- [x] Keep the last two meaningful milestones with glyphs that remain distinct
  without colour.
- [x] Advance R `shift_watch()` animation between store polls so visual refresh
  does not increase DuckDB or live-sidecar polling frequency.
- [x] Update public documentation and examples for `motion` and `refresh`.
- [x] Add the new renderer and cadence tests to the full validation matrix.
- [x] Re-run the complete test suite and `R CMD check`.

### 11. Atomic terminal framebuffer

- [x] Replace the one-cli-progress-bar-per-row workaround with one renderer
  that submits the complete dashboard in a single terminal write.
- [x] Reuse cli's public output-connection, ANSI/dynamic capability, width,
  Unicode, styling, and cursor-visibility APIs.
- [x] Adapt cli's upstream multiline status algorithm only for cursor-up,
  erase-line, stale-tail cleanup, and painted-line ownership.
- [x] Keep one cli-owned compact status row for RStudio and other dynamic
  consoles without reliable cursor-up support.
- [x] Suspend and restore the frame around alerts, resolver tables, case tables,
  and terminal summaries.
- [x] Share the same renderer across foreground runs, R `shift_watch()`, and CLI
  `shift watch`.
- [x] Test atomic writes, shorter replacement frames, nested suspend/restore,
  compact fallback, cursor cleanup, and dynamic-renderer failure fallback.

### 12. WezTerm visual hierarchy refinement

- [x] Keep the foreground startup plan inside the replaceable live frame so
  `STARTING` and `RUNNING` summaries do not accumulate above one another.
- [x] Treat resolver node attempts as indeterminate failover rather than a
  workflow percentage.
- [x] Keep exactly one animated focus in the dashboard and make the stage rail
  and resolver history static.
- [x] Replace full resolver exceptions with structured normal-mode outcomes;
  retain complete causes in detail tables, events, and logs.
- [x] Use the current terminal width on wide displays and reflow complete plan
  fields at semantic boundaries instead of truncating them at a fixed 112-column
  measure.
- [x] Reserve the final terminal column so WezTerm and other autowrapping
  terminals retain the complete right border.
- [x] Reflow current operation, status, failure summary, and diagnostic evidence
  under aligned semantic prefixes; losslessly split unbreakable identifiers.
- [x] Collapse the stage rail from full flow to current-plus-next and then
  current-only forms rather than truncating an arbitrary suffix.
- [x] Use a restrained semantic palette: cyan for active work, green for
  completion, yellow for fallback, red for failure, and dim text for labels and
  pending stages.
- [x] Add a quiet full-width panel at 60 columns and above, with the run header
  embedded in the top rule and one-cell padding around semantic content.
- [x] Separate active execution from recent outcomes with labelled `Workflow`
  and `Activity` rules while keeping the dividers visually dim.
- [x] Preserve an undecorated, variable-height view below 60 columns so borders
  never displace essential status content.
- [x] Suppress routine stage-success and case-table transcript lines in normal
  dynamic mode because the live Recent section already owns those milestones.
- [x] Add regression tests for the replaceable startup frame, one-spinner rule,
  resolver progress semantics, short node outcomes, responsive panel hierarchy,
  and wide-terminal measure.

### 13. Durable failure dashboard and actionable diagnosis

- [x] Emphasize the title-like `Plan`, `Flow`, `Status`, and `Summary` labels
  with one bold accent treatment, use a bold danger treatment for `Failure`
  and `Stopped`, and keep secondary labels quiet without changing alignment.
- [x] Commit the final failed or cancelled dashboard to terminal scrollback
  instead of clearing it before R prints the terminal condition.
- [x] Replace the running `Activity` section with a stable `Diagnosis` section
  containing attempt counts, one domain cause, the closest CMIP6 identity, and
  the first missing requirement.
- [x] Preserve exact scenario, variable, year, member, and grid evidence in
  structured resolver conditions and persisted run events.
- [x] Aggregate repeated ESGF mirror failures into one resolver diagnosis while
  retaining complete per-node attempts for detail output and logs.
- [x] Keep normal dynamic failures inside the committed dashboard; do not print
  a second resolver table immediately below it. Detail/debug and append-only log
  modes retain the complete tables.
- [x] Remove internal `tryCatch` call-site labels such as `value[[3L]]()` from
  user-facing R errors by raising the final workflow condition with `call = NULL`.
- [x] Distinguish transient retryable failures from scientific coverage
  failures. Only the former show `Retry`; the latter warn that an unchanged
  resume will repeat the failure and direct the user to inspect or change the
  request.
- [x] Shorten copyable recovery commands by omitting the default store path,
  while preserving explicit non-default stores and limiting log inspection to
  the latest 20 rows.
- [x] Reconstruct the same terminal diagnosis in `shift_watch()` from persisted
  event details after the foreground process exits.
- [x] Add regression tests for final-frame commit, normal/detail boundaries,
  narrow terminals, no-colour output, persisted watch reconstruction, concise
  commands, typed resolver errors, and missing aggregate counters.

## Comparator boundary

The local `esgpull` implementation is used only as an architectural comparator:
one live-region owner, aggregate plus active-file progress, and a strict split
between interactive rendering and non-interactive logs. This package will not
add a Rich-style TUI dependency or copy `esgpull`'s visual treatment.

## Completion contract

This work is complete only when every checkbox above is checked, the tests and
package check pass, and foreground, background watch, log, none, JSON, and JSONL
modes all preserve their documented output contracts.

## Validation record

- Motion/dashboard tests cover full, reduced, and no-motion rendering; cached
  watch animation; atomic variable-height frame painting; wide and narrow
  layouts; compact fallback; live resolver rows; terminal freeze;
  one-spinner resolver rendering; durable failed-frame commit; no-colour and
  narrow failure diagnosis; persisted watch reconstruction; and 24- to
  180-column reflow, semantic diagnostic continuation rows, unbreakable tokens,
  and a reserved final terminal column.
- Focused tests: `shift-ui`, `shift-stage`, `cli-shift`, and `dataset` passed.
- Complete test suite passed; opt-in live ESGF tests were skipped by their
  existing environment guards.
- `R CMD check --no-tests --no-manual`: **status OK**. The complete test suite
  was run separately because installed-package checks exercise the existing
  long-running network/downloader matrix serially.
