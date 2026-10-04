#' @include shift-stage.R
NULL

# ShiftUiOptions keeps runtime presentation preferences separate from the
# scientific ShiftPlan so display choices never alter deterministic artifacts.
# ShiftUiOptions {{{
ShiftUiOptions <- S7::new_class(
    "ShiftUiOptions",
    properties = list(
        progress = shift_stage__prop_string(min.chars = 1L),
        detail = shift_stage__prop_string(min.chars = 1L),
        motion = shift_stage__prop_string(min.chars = 1L),
        refresh = S7::new_property(S7::class_numeric),
        heartbeat = S7::new_property(S7::class_numeric),
        # Transient batch context never participates in a scientific plan hash.
        batch_context = S7::new_property(S7::class_list, default = list()),
        # CLI result emitters own their final batch receipt; child progress is
        # still rendered normally. This flag never enters persisted intent.
        batch_receipt = S7::new_property(S7::class_logical, default = TRUE)
    )
)
# }}}

# Detail levels are ordered so every renderer applies the same visibility
# contract to foreground runs, background logs, and watch snapshots.
SHIFT_UI_DETAIL_LEVELS <- c("normal", "detail", "debug")

#' @rdname shift_api
#' @param progress In [shift_ui()], workflow presentation mode: `"auto"`,
#'   `"dynamic"`, `"log"`, or `"none"`.
#' @param detail Presentation detail level. `"normal"` shows task progress,
#'   `"detail"` adds selection, reuse, and fallback decisions, and `"debug"`
#'   also shows full URLs, paths, and low-level transfer context.
#' @param motion Dynamic-terminal animation policy: `"auto"`, `"full"`,
#'   `"reduced"`, or `"none"`. This never changes log, JSON, or workflow data.
#' @param refresh In [shift_control()], whether to refresh remote catalogs and
#'   service addresses instead of resuming persisted inputs. In [shift_ui()],
#'   minimum seconds between visual animation frames. In `ShiftRun` inspectors,
#'   whether to reload persisted state first.
#' @param heartbeat Minimum seconds between job-liveness updates. Durable
#'   writes are always throttled to at least one second.
#'   Use `progress = "log"` for screen readers, reduced-motion use, redirected
#'   output, and stable captured logs.
#' @export
# shift_ui {{{
shift_ui <- function(
    progress = c("auto", "dynamic", "log", "none"),
    detail = c("normal", "detail", "debug"),
    motion = c("auto", "full", "reduced", "none"),
    refresh = 0.12,
    heartbeat = 10
) {
    progress <- match.arg(progress)
    detail <- match.arg(detail)
    motion <- match.arg(motion)
    checkmate::assert_number(refresh, lower = 0.05, finite = TRUE)
    checkmate::assert_number(heartbeat, lower = 0, finite = TRUE)
    ShiftUiOptions(
        progress = progress,
        detail = detail,
        motion = motion,
        refresh = as.numeric(refresh),
        heartbeat = as.numeric(heartbeat)
    )
}
# }}}

# Resolve auto mode once per reporter so a run does not switch presentation
# when its surrounding output device changes halfway through execution.
# shift_ui__ui_mode {{{
shift_ui__ui_mode <- function(ui) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(ui@progress, "auto")) {
        return(ui@progress)
    }
    # CI and deliberately simple terminals require append-only output even when
    # a surrounding process happens to expose a pseudo-TTY.
    ci <- tolower(trimws(Sys.getenv("CI")))
    if (
        (nzchar(ci) && !ci %in% c("0", "false", "no")) ||
            identical(tolower(Sys.getenv("TERM")), "dumb")
    ) {
        return("log")
    }
    # Rscript is non-interactive even when a human is watching it in a real
    # terminal. TTY capability, not `interactive()`, is the useful boundary.
    if (isTRUE(cli::is_dynamic_tty())) "dynamic" else "log"
}
# }}}

# Resolve animation independently from output mode. Log, null, redirected, and
# machine-readable renderers remain static even if full motion was requested.
# shift_ui__ui_motion {{{
shift_ui__ui_motion <- function(ui, mode = shift_ui__ui_mode(ui)) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(mode, "dynamic")) {
        return("none")
    }
    if (identical(ui@motion, "auto")) "full" else ui@motion
}
# }}}

# Compare one requested detail level against the immutable UI configuration.
# shift_ui__ui_at_least {{{
shift_ui__ui_at_least <- function(ui, level = c("normal", "detail", "debug")) {
    level <- match.arg(level)
    match(ui@detail, SHIFT_UI_DETAIL_LEVELS) >=
        match(level, SHIFT_UI_DETAIL_LEVELS)
}
# }}}

# Resolve the current display width through cli so terminal, RStudio, and
# redirected output use the same cross-platform width policy as styling. Narrow
# terminals therefore reflow instead of overflowing a nominal width.
# shift_ui__ui_width {{{
shift_ui__ui_width <- function(width = NULL) {
    if (is.null(width)) {
        # error {{{
        width <- tryCatch(cli::console_width(), error = function(e) 80L)
        # }}}
    }
    width <- suppressWarnings(as.integer(width[[1L]]))
    if (!length(width) || is.na(width) || width < 1L) 80L else width
}
# }}}

# Reserve the terminal's final column for autowrap safety. Several terminals,
# including WezTerm, wrap or visually drop a glyph painted in the last column;
# keeping one column unused makes the right panel border deterministic across
# terminals without bringing back a fixed readable-measure cap.
# shift_ui__ui_dashboard_width {{{
shift_ui__ui_dashboard_width <- function(width = NULL) {
    max(1L, shift_ui__ui_width(width) - 1L)
}
# }}}

# Reserve one terminal row for cursor ownership and the next shell prompt.
# Native TTY dimensions follow terminal resizes. Explicit options also support
# IDE panes and reproducible recordings with caller-owned dimensions.
# shift_ui__ui_height {{{
shift_ui__ui_height <- function(height = NULL) {
    height <- shift_stage__coalesce(height, getOption("epwshiftr.ui_height"))
    if (is.null(height)) {
        height <- shift_stage__coalesce(
            shift__ui_terminal_height(),
            Sys.getenv("LINES", "24")
        )
    }
    height <- suppressWarnings(as.integer(height[1L]))
    if (!length(height) || is.na(height) || height < 2L) {
        height <- 24L
    }
    max(1L, height - 1L)
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
