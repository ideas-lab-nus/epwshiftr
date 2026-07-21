# ShiftUiOptions keeps runtime presentation preferences separate from the
# scientific ShiftPlan so display choices never alter deterministic artifacts.
ShiftUiOptions <- S7::new_class(
    "ShiftUiOptions",
    properties = list(
        progress = shift_prop_string(min.chars = 1L),
        detail = shift_prop_string(min.chars = 1L),
        motion = shift_prop_string(min.chars = 1L),
        refresh = S7::new_property(S7::class_numeric),
        heartbeat = S7::new_property(S7::class_numeric)
    )
)

# Detail levels are ordered so every renderer applies the same visibility
# contract to foreground runs, background logs, and watch snapshots.
SHIFT_UI_DETAIL_LEVELS <- c("normal", "detail", "debug")

#' @rdname shift_api
#' @param progress In [shift_ui()], workflow presentation mode: `"auto"`,
#'   `"dynamic"`, `"log"`, or `"none"`. In low-level collect helpers, a
#'   logical controlling their native query progress display.
#' @param detail Presentation detail level. `"normal"` shows task progress,
#'   `"detail"` adds selection, reuse, and fallback decisions, and `"debug"`
#'   also shows full URLs, paths, and low-level transfer context.
#' @param motion Dynamic-terminal animation policy: `"auto"`, `"full"`,
#'   `"reduced"`, or `"none"`. This never changes log, JSON, or workflow data.
#' @param refresh In [shift_ui()], minimum seconds between visual animation
#'   frames. In `ShiftRun` inspectors, whether to reload persisted state first.
#' @param heartbeat Minimum seconds between job-liveness updates. Durable
#'   writes are always throttled to at least one second.
#'   Use `progress = "log"` for screen readers, reduced-motion use, redirected
#'   output, and stable captured logs.
#' @export
shift_ui <- function(progress = c("auto", "dynamic", "log", "none"),
                     detail = c("normal", "detail", "debug"),
                     motion = c("auto", "full", "reduced", "none"),
                     refresh = 0.12, heartbeat = 10) {
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

# Resolve auto mode once per reporter so a run does not switch presentation
# when its surrounding output device changes halfway through execution.
shift__ui_mode <- function(ui) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(ui@progress, "auto")) {
        return(ui@progress)
    }
    # CI and deliberately simple terminals require append-only output even when
    # a surrounding process happens to expose a pseudo-TTY.
    ci <- tolower(trimws(Sys.getenv("CI")))
    if ((nzchar(ci) && !ci %in% c("0", "false", "no")) ||
        identical(tolower(Sys.getenv("TERM")), "dumb")) {
        return("log")
    }
    # Rscript is non-interactive even when a human is watching it in a real
    # terminal. TTY capability, not `interactive()`, is the useful boundary.
    if (isTRUE(cli::is_dynamic_tty())) "dynamic" else "log"
}

# Resolve animation independently from output mode. Log, null, redirected, and
# machine-readable renderers remain static even if full motion was requested.
shift__ui_motion <- function(ui, mode = shift__ui_mode(ui)) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(mode, "dynamic")) {
        return("none")
    }
    if (identical(ui@motion, "auto")) "full" else ui@motion
}

# Compare one requested detail level against the immutable UI configuration.
shift__ui_at_least <- function(ui, level = c("normal", "detail", "debug")) {
    level <- match.arg(level)
    match(ui@detail, SHIFT_UI_DETAIL_LEVELS) >= match(level, SHIFT_UI_DETAIL_LEVELS)
}

# Resolve the current display width through cli so terminal, RStudio, and
# redirected output use the same cross-platform width policy as styling. Narrow
# terminals therefore reflow instead of overflowing a nominal width.
shift__ui_width <- function(width = NULL) {
    if (is.null(width)) {
        width <- tryCatch(cli::console_width(), error = function(e) 80L)
    }
    width <- suppressWarnings(as.integer(width[[1L]]))
    if (!length(width) || is.na(width) || width < 1L) 80L else width
}

# Reserve the terminal's final column for autowrap safety. Several terminals,
# including WezTerm, wrap or visually drop a glyph painted in the last column;
# keeping one column unused makes the right panel border deterministic across
# terminals without bringing back a fixed readable-measure cap.
shift__ui_dashboard_width <- function(width = NULL) {
    max(1L, shift__ui_width(width) - 1L)
}

# Fit plain user-facing text into one terminal row without relying on colour or
# terminal-specific clipping for essential status information. cli performs
# display-width-aware trimming for ANSI and wide CJK characters.
shift__ui_fit <- function(x, width = shift__ui_width()) {
    x <- gsub("[\r\n]+", " ", as.character(shift_coalesce(x, "")))
    width <- shift__ui_width(width)
    cli::ansi_strtrim(x, width)
}

# Split one unbreakable ANSI token by display width while preserving styles and
# wide Unicode boundaries. cli's prose wrapper intentionally keeps long paths
# and identifiers intact, so this is the final lossless fallback for dashboard
# values that would otherwise be trimmed.
shift__ui_hard_wrap <- function(x, width) {
    width <- max(1L, shift__ui_width(width))
    remaining <- as.character(shift_coalesce(x, ""))[[1L]]
    lines <- character()
    while (cli::ansi_nchar(remaining, type = "width") > width) {
        characters <- cli::ansi_nchar(remaining, type = "chars")
        lower <- 1L
        upper <- characters
        best <- 0L
        while (lower <= upper) {
            middle <- as.integer(floor((lower + upper) / 2))
            candidate <- cli::ansi_substr(remaining, 1L, middle)
            if (cli::ansi_nchar(candidate, type = "width") <= width) {
                best <- middle
                lower <- middle + 1L
            } else {
                upper <- middle - 1L
            }
        }
        # A one-column terminal cannot display a double-width glyph. Consume it
        # anyway so the loop progresses; shift__ui_fit() provides the safe mark.
        best <- max(1L, best)
        lines <- c(lines, shift__ui_fit(
            cli::ansi_substr(remaining, 1L, best), width))
        remaining <- cli::ansi_substr(remaining, best + 1L, characters)
    }
    c(lines, remaining)
}

# Combine cli's word-aware wrapping with the unbreakable-token fallback so
# prose prefers natural boundaries without ever losing a long identifier.
shift__ui_wrap_lines <- function(value, width) {
    width <- max(1L, shift__ui_width(width))
    wrapped <- cli::ansi_strwrap(value, width = width)
    if (!length(wrapped)) {
        return("")
    }
    unlist(lapply(wrapped, shift__ui_hard_wrap, width = width),
        use.names = FALSE)
}

# Wrap prose after a fixed semantic prefix and align every continuation row
# beneath its value. Long unbreakable tokens use the lossless hard-wrap helper,
# so paths and identifiers remain fully available in diagnostic frames.
shift__ui_prefixed_lines <- function(prefix, value, width,
                                     continuation = NULL) {
    width <- max(1L, shift__ui_width(width))
    prefix <- as.character(shift_coalesce(prefix, ""))[[1L]]
    value <- gsub("[\r\n]+", " ",
        as.character(shift_coalesce(value, ""))[[1L]])
    prefix_width <- cli::ansi_nchar(prefix, type = "width")
    if (prefix_width >= width) {
        return(c(shift__ui_fit(prefix, width),
            shift__ui_wrap_lines(value, width)))
    }
    value_width <- max(1L, width - prefix_width)
    wrapped <- shift__ui_wrap_lines(value, value_width)
    continuation <- shift_coalesce(continuation,
        strrep(" ", min(prefix_width, width)))
    lines <- paste0(c(prefix, rep(continuation,
        max(0L, length(wrapped) - 1L))), wrapped)
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Render a title-like dashboard field as one row when possible and as aligned
# continuation rows when its value grows. The fixed label remains the visual
# anchor in colour terminals and the plain-text anchor under NO_COLOR.
shift__ui_labeled_lines <- function(label, value, width) {
    first_prefix <- shift__ui_labeled_line(label, "")
    continuation <- shift__ui_labeled_line("", "")
    shift__ui_prefixed_lines(first_prefix, value, width, continuation)
}

# Pack complete semantic fields into the available value width. Separators are
# added only when both neighbouring fields fit, so narrow layouts reflow at
# meaningful boundaries before the final display-width safety trim is needed.
shift__ui_pack_items <- function(items, width, separator = " \u00b7 ") {
    width <- max(1L, as.integer(width))
    items <- as.character(shift_coalesce(items, character()))
    items <- items[!is.na(items) & nzchar(items)]
    if (!length(items)) {
        return("")
    }
    lines <- character()
    current <- ""
    for (item in items) {
        candidate <- if (nzchar(current)) {
            paste0(current, separator, item)
        } else {
            item
        }
        if (cli::ansi_nchar(candidate, type = "width") <= width) {
            current <- candidate
            next
        }
        if (nzchar(current)) {
            lines <- c(lines, current)
            current <- ""
        }
        # An unusually long individual value, such as a custom method name,
        # still wraps safely without forcing the complete dashboard to widen.
        wrapped <- shift__ui_wrap_lines(item, width = width)
        if (length(wrapped) > 1L) {
            lines <- c(lines, wrapped[-length(wrapped)])
        }
        current <- wrapped[[length(wrapped)]]
    }
    c(lines, current)
}

# Render the scientific plan as one row when it fits and as aligned continuation
# rows otherwise. The structured `items` form is preferred, while persisted
# snapshots from earlier runs can still be split on the visible separator.
shift__ui_plan_lines <- function(plan_context, width = shift__ui_width()) {
    width <- shift__ui_width(width)
    label_width <- 9L
    items <- shift_coalesce(plan_context$items, character())
    if (!length(items)) {
        line <- as.character(shift_coalesce(
            plan_context$line, "Workflow context unavailable"))[[1L]]
        items <- strsplit(line, " \u00b7 ", fixed = TRUE)[[1L]]
    }
    # Extremely narrow terminals receive the heading on its own row so the
    # fixed label does not consume every column and hide the plan values.
    if (width <= label_width) {
        return(c(shift__ui_fit(shift__ui_labeled_line("Plan", ""), width),
            unlist(lapply(items, shift__ui_wrap_lines, width = width),
                use.names = FALSE)))
    }
    value_width <- width - label_width
    values <- shift__ui_pack_items(items, value_width)
    vapply(seq_along(values), function(i) {
        shift__ui_labeled_line(if (i == 1L) "Plan" else "", values[[i]])
    }, character(1L))
}

# Pad a compact table cell using terminal display width rather than bytes or R
# character count, which keeps mixed Latin/CJK rows aligned.
shift__ui_cell <- function(x, width) {
    x <- ifelse(is.na(x), "\u2014", as.character(x))
    cli::ansi_align(shift__ui_fit(x, width), width, align = "left")
}

# Format named workflow periods compactly for the startup summary.
shift__ui_periods <- function(periods) {
    periods <- data.table::as.data.table(periods)
    if (!nrow(periods) || !all(c("period", "year") %in% names(periods))) {
        return("no periods")
    }
    labels <- unique(as.character(periods$period))
    paste(vapply(labels, function(label) {
        years <- periods[period == label]$year
        if (length(unique(years)) == 1L) {
            sprintf("%s (%d)", label, years[[1L]])
        } else {
            sprintf("%s (%d\u2013%d)", label, min(years), max(years))
        }
    }, character(1L)), collapse = ", ")
}

# Describe the reference input without exposing matching fields or plan IDs in
# the normal startup view; those remain available through shift_explain().
shift__ui_reference <- function(method) {
    reference <- method@reference
    if (is.null(reference)) {
        return(if (isTRUE(morpher__recipe_accepts_reference(method@recipe))) {
            "baseline EPW"
        } else {
            "no reference"
        })
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        periods_table <- data.table::as.data.table(reference@periods)
        period_names <- unique(periods_table$period)
        periods <- if (length(period_names) == 1L) {
            years <- periods_table$year
            if (length(unique(years)) == 1L) {
                as.character(years[[1L]])
            } else {
                sprintf("%d\u2013%d", min(years), max(years))
            }
        } else {
            shift__ui_periods(periods_table)
        }
        return(sprintf("%s %s", reference@mode, periods))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied climate reference")
    }
    "reference supplied"
}

# Return the exact execution-stage sequence implied by one plan. Reporter stage
# events persist the next stage so a background watch does not need to rebuild
# the scientific plan merely to explain what comes next.
shift__ui_stage_sequence <- function(plan) {
    reference <- plan@meta$method@reference
    reference_expected <- S7::S7_inherits(reference, ShiftReferenceSpec) &&
        identical(reference@mode, "historical")
    c(
        "resolve",
        if (identical(plan@meta$control@download, "always")) "download",
        "extract_future",
        if (isTRUE(reference_expected)) "extract_reference",
        "coverage", "morph", "write_epw"
    )
}

# Format unresolved or explicit CMIP6 selections for the startup summary.
shift__ui_selection <- function(plan) {
    climate <- plan@meta$climate
    member <- if (is.null(climate) || is.null(climate@member)) {
        "member auto"
    } else {
        sprintf("member %s", paste(climate@member, collapse = ", "))
    }
    grid <- if (is.null(climate) || is.null(climate@grid)) {
        "grid auto"
    } else {
        sprintf("grid %s", climate@grid)
    }
    paste(member, grid, sep = " \u00b7 ")
}

# Build the compact startup summary shown before any network request. Normal
# output confirms the delivery directory and pending CMIP selections; detail
# output additionally exposes the run policy and internal store.
shift__ui_plan_summary <- function(plan, run_id, background = FALSE,
                                   width = shift__ui_width(), detail = "normal") {
    request <- plan@meta$request@meta
    model <- shift_coalesce(shift_display_values(request$source), "<model>")
    scenarios <- shift_coalesce(shift_display_values(request$experiment), "<scenario>")
    status <- if (isTRUE(background)) "QUEUED" else "STARTING"
    output_dir <- shift_coalesce(plan@meta$epw$export_dir, "<output directory>")
    lines <- c(
        shift__ui_fit(sprintf("Future EPW \u00b7 %s \u00b7 %s", run_id, status), width),
        shift__ui_fit(sprintf("%s \u00b7 %s \u00b7 %s",
            model, scenarios, shift__ui_periods(plan@meta$periods)), width),
        shift__ui_fit(sprintf("%s \u00b7 %s \u00b7 %d expected output(s)",
            plan@meta$method@name, shift__ui_reference(plan@meta$method),
            nrow(plan@meta$expected_cases)), width),
        shift__ui_fit(sprintf("Selection %s", shift__ui_selection(plan)), width),
        shift__ui_fit(sprintf("Output %s", shift_display_path(output_dir)), width)
    )
    if (!identical(detail, "normal")) {
        lines <- c(lines, shift__ui_fit(sprintf("Policy %s \u00b7 store %s",
            if (isTRUE(plan@meta$control@allow_partial)) {
                "partial cases allowed"
            } else {
                "all cases required"
            },
            shift_display_path(plan@store_path)), width))
    }
    lines
}

# Extract the compact scientific context carried inside every foreground frame.
# Keeping this separate from the startup receipt lets dynamic mode replace its
# first frame instead of leaving a duplicated five-line transcript behind.
shift__ui_plan_context <- function(plan) {
    request <- plan@meta$request@meta
    model <- paste(as.character(shift_coalesce(request$source, "<model>")),
        collapse = ", ")
    scenarios <- paste(as.character(shift_coalesce(
        request$experiment, "<scenario>")), collapse = " + ")
    reference <- shift__ui_reference(plan@meta$method)
    expected <- nrow(plan@meta$expected_cases)
    items <- c(
            model,
            scenarios,
            shift__ui_periods(plan@meta$periods),
            sprintf("%s / %s", plan@meta$method@name, reference),
            sprintf("%d EPW%s", expected, if (expected == 1L) "" else "s")
        )
    list(
        line = paste(items, collapse = " \u00b7 "),
        items = items,
        selection = shift__ui_selection(plan),
        output = shift_coalesce(plan@meta$epw$export_dir, "<output directory>")
    )
}

# Map internal stage identifiers onto short labels that remain readable in the
# fixed status region and in redirected logs.
shift__ui_stage_label <- function(stage) {
    labels <- c(
        planned = "Plan", collect = "Collect", resolve = "Resolve",
        download = "Download", extract = "Extract",
        extract_future = "Extract future",
        extract_reference = "Extract reference", coverage = "Coverage",
        morph = "Morph", write_epw = "Write EPW", export_epw = "Export EPW",
        completed = "Completed",
        resume = "Resume"
    )
    value <- labels[[as.character(shift_coalesce(stage, "planned"))]]
    shift_coalesce(value, gsub("_", " ", as.character(stage), fixed = TRUE))
}

# Abbreviate a run identity to the stable suffix users need when reading a live
# dashboard. Startup receipts, logs, and persisted records retain the full ID.
shift__ui_run_short <- function(run_id) {
    run_id <- as.character(shift_coalesce(run_id, ""))[[1L]]
    run_id <- sub("^run_", "", run_id)
    if (!nzchar(run_id) || nchar(run_id) <= 8L) {
        return(run_id)
    }
    substr(run_id, nchar(run_id) - 7L, nchar(run_id))
}

# Safely read one scalar numeric metric from current reporter details.
shift__ui_metric_number <- function(details, name, default = NA_real_) {
    value <- details[[name]]
    if (is.null(value) || !length(value)) {
        return(default)
    }
    value <- suppressWarnings(as.numeric(value[[1L]]))
    if (!length(value) || is.na(value) || !is.finite(value)) default else value
}

# Format a measured download ETA without implying an ETA for the whole workflow.
shift__ui_eta <- function(seconds) {
    if (is.null(seconds) || !length(seconds) || is.na(seconds) || !is.finite(seconds)) {
        return(NULL)
    }
    paste("ETA", shift__format_elapsed(seconds))
}

# Classify fixed left-hand labels by their information role. Accent labels form
# the dashboard's reading outline, while terminal-problem labels reinforce the
# corresponding state without making colour the only source of meaning.
shift__ui_label_role <- function(label) {
    if (label %in% c("Plan", "Flow", "Status", "Summary")) {
        return("accent")
    }
    if (label %in% c("Failure", "Stopped")) {
        return("danger")
    }
    "quiet"
}

# Give title-like labels a consistent visual hierarchy while retaining the
# existing fixed width. NO_COLOR and narrow terminals keep the same words and
# alignment, so styling remains an enhancement rather than required semantics.
shift__ui_labeled_line <- function(label, value) {
    label <- sprintf("%-9s", label)
    label <- switch(shift__ui_label_role(trimws(label)),
        accent = cli::style_bold(cli::col_blue(label)),
        danger = cli::style_bold(cli::col_red(label)),
        cli::style_dim(label)
    )
    paste0(label, value)
}

# Pad one semantic row inside the live panel while styling only the border.
# The content keeps its own state colours and remains readable with NO_COLOR.
shift__ui_panel_line <- function(value, width) {
    width <- shift__ui_width(width)
    inner_width <- max(1L, width - 4L)
    value <- cli::ansi_align(shift__ui_fit(value, inner_width),
        inner_width, align = "left")
    paste0(cli::style_dim("\u2502 "), value, cli::style_dim(" \u2502"))
}

# Draw top, middle, and bottom panel rules with display-width-aware labels.
# This remains a pure formatter so the framebuffer still owns all cursor work.
shift__ui_panel_rule <- function(label = NULL, width,
                                 kind = c("top", "middle", "bottom")) {
    kind <- match.arg(kind)
    width <- shift__ui_width(width)
    glyphs <- switch(kind,
        top = c("\u256d", "\u256e"),
        middle = c("\u251c", "\u2524"),
        bottom = c("\u2570", "\u256f")
    )
    inner_width <- max(1L, width - 2L)
    if (is.null(label) || !length(label) || !nzchar(cli::ansi_strip(label))) {
        return(cli::style_dim(paste0(glyphs[[1L]],
            strrep("\u2500", inner_width), glyphs[[2L]])))
    }
    label <- shift__ui_fit(label, max(1L, inner_width - 3L))
    used <- cli::ansi_nchar(label, type = "width") + 3L
    paste0(
        cli::style_dim(paste0(glyphs[[1L]], "\u2500 ")),
        label,
        cli::style_dim(paste0(" ",
            strrep("\u2500", max(0L, inner_width - used)), glyphs[[2L]]))
    )
}

# Apply colour only to semantic state. Ordinary configuration values remain in
# the terminal's default foreground colour instead of becoming a wall of green.
shift__ui_status_style <- function(status) {
    status <- toupper(as.character(shift_coalesce(status, "running"))[[1L]])
    styled <- switch(tolower(status),
        completed = cli::col_green(status),
        partial = cli::col_yellow(status),
        failed = cli::col_red(status),
        cancelled = cli::col_red(status),
        stopping = cli::col_yellow(status),
        waiting = cli::col_blue(status),
        queued = cli::col_blue(status),
        cli::col_cyan(status)
    )
    cli::style_bold(styled)
}

# Format a determinate stage row only for work whose total is meaningful.
# Resolver node failover is intentionally excluded because attempt count is not
# a trustworthy estimate of elapsed workflow completion.
shift__ui_determinate <- function(current, total, width) {
    if (is.na(current) || is.na(total) || total <= 0) {
        return(NULL)
    }
    bar_width <- if (width >= 100L) 22L else if (width >= 72L) 14L else 8L
    percent <- as.integer(round(max(0, min(1, current / total)) * 100))
    sprintf("%s  %d/%d \u00b7 %d%%",
        shift__ui_bar(current, total, bar_width),
        as.integer(current), as.integer(total), percent)
}

# Build the stage-progress field. Each stage exposes its own measurable unit;
# long transfer or selection metrics continue on aligned rows, while resolver
# work remains indeterminate instead of presenting a misleading percentage.
shift__ui_metric_line <- function(state, width = shift__ui_width()) {
    width <- shift__ui_width(width)
    details <- shift_coalesce(state$current_details, list())
    stage <- as.character(shift_coalesce(state$stage, "planned"))[[1L]]
    current <- shift__ui_metric_number(details, "current",
        shift_coalesce(state$unit_current, NA_real_))
    total <- shift__ui_metric_number(details, "total",
        shift_coalesce(state$unit_total, NA_real_))
    elapsed <- shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    plan_context <- shift_coalesce(state$plan_context, list())

    if (identical(stage, "resolve")) {
        attempt <- if (!is.na(current) && !is.na(total)) {
            sprintf("node %d of %d", as.integer(current), as.integer(total))
        } else {
            "checking catalogs"
        }
        value <- paste(c(attempt, plan_context$selection,
            sprintf("%s elapsed", elapsed)), collapse = " \u00b7 ")
        return(shift__ui_labeled_lines("Status", value, width))
    }

    if (identical(details$unit_type, "download_session") ||
        identical(stage, "download")) {
        parts <- c(shift__ui_determinate(current, total, width))
        bytes_done <- shift__ui_metric_number(details, "bytes_done")
        bytes_total <- shift__ui_metric_number(details, "bytes_total")
        if (!is.na(bytes_done)) {
            parts <- c(parts, if (is.na(bytes_total)) {
                shift__ui_bytes(bytes_done)
            } else {
                sprintf("%s/%s", shift__ui_bytes(bytes_done),
                    shift__ui_bytes(bytes_total))
            })
        }
        speed <- shift__ui_metric_number(details, "speed_bps")
        if (!is.na(speed) && speed > 0) {
            parts <- c(parts, paste0(shift__ui_bytes(speed), "/s"))
        }
        eta <- shift__ui_eta(shift__ui_metric_number(details, "eta_seconds"))
        if (!is.null(eta)) parts <- c(parts, eta)
        active <- shift__ui_metric_number(details, "active_task_count")
        if (!is.na(active) && active > 0) {
            parts <- c(parts, sprintf("%d active", as.integer(active)))
        }
        return(shift__ui_labeled_lines("Transfer",
            paste(parts, collapse = " \u00b7 "), width))
    }

    if (stage %in% c("extract_future", "extract_reference")) {
        parts <- c(shift__ui_determinate(current, total, width))
        if (!is.null(details$access_method) && length(details$access_method)) {
            parts <- c(parts, as.character(details$access_method[[1L]]))
        }
        return(shift__ui_labeled_lines("Plans",
            paste(parts, collapse = " \u00b7 "), width))
    }

    cases_ready <- as.integer(shift_coalesce(state$cases_ready, 0L))
    cases_total <- as.integer(shift_coalesce(state$cases_total, 0L))
    outputs <- as.integer(shift_coalesce(state$outputs_completed, 0L))
    if (identical(stage, "coverage")) {
        value <- paste(c(shift__ui_determinate(cases_ready, cases_total, width),
            sprintf("ready %d", cases_ready),
            sprintf("missing %d", max(0L, cases_total - cases_ready))),
            collapse = " \u00b7 ")
        return(shift__ui_labeled_lines("Cases", value, width))
    }
    if (identical(stage, "morph")) {
        completed <- if (!is.na(current)) as.integer(current) else 0L
        target <- if (!is.na(total)) as.integer(total) else cases_ready
        value <- shift_coalesce(shift__ui_determinate(completed, target, width),
            sprintf("%d/%d", completed, target))
        return(shift__ui_labeled_lines("Cases", value, width))
    }
    if (identical(stage, "write_epw")) {
        current_case <- if (!is.na(current)) as.integer(current) else outputs
        target <- if (!is.na(total)) as.integer(total) else cases_total
        value <- paste(c(shift__ui_determinate(current_case, target, width),
            sprintf("exported %d/%d", outputs, cases_total)),
            collapse = " \u00b7 ")
        return(shift__ui_labeled_lines("EPWs", value, width))
    }

    value <- paste(c(plan_context$selection, sprintf("%s elapsed", elapsed)),
        collapse = " \u00b7 ")
    shift__ui_labeled_lines("Status", value, width)
}

# Return one terminal-safe animation frame without making motion essential to
# understanding the active state. Reduced motion uses a stable marker.
shift__ui_spinner <- function(motion = c("none", "full", "reduced"), frame = 0L) {
    motion <- match.arg(motion)
    if (identical(motion, "none")) {
        return("")
    }
    if (identical(motion, "reduced")) {
        return("\u25cf")
    }
    frames <- c("\u280b", "\u2819", "\u2839", "\u2838", "\u283c", "\u2834", "\u2826", "\u2827", "\u2807", "\u280f")
    frames[[as.integer(frame) %% length(frames) + 1L]]
}

# Map durable outcomes and live states to symbols before optional colour is
# applied. Every colour retains a distinct glyph for monochrome terminals.
shift__ui_state_symbol <- function(status, motion = "none", frame = 0L,
                                   colour = TRUE) {
    status <- as.character(shift_coalesce(status, "pending"))[[1L]]
    symbol <- switch(status,
        completed = "\u2714",
        skipped = "\u21aa",
        reused = "\u21aa",
        partial = "!",
        fallback = "\u21aa",
        rejected = "\u2716",
        failed = "\u2716",
        cancelled = "\u25a0",
        stopping = "!",
        waiting = "\u25cb",
        running = shift__ui_spinner(motion, frame),
        active = shift__ui_spinner(motion, frame),
        current = "\u25cf",
        queued = "\u25cb",
        pending = "\u25cb",
        "\u2022"
    )
    if (!nzchar(symbol)) {
        symbol <- "\u2022"
    }
    if (!isTRUE(colour)) {
        return(symbol)
    }
    switch(status,
        completed = cli::col_green(symbol),
        skipped = cli::col_blue(symbol),
        reused = cli::col_blue(symbol),
        fallback = cli::col_blue(symbol),
        partial = cli::col_yellow(symbol),
        stopping = cli::col_yellow(symbol),
        waiting = cli::col_blue(symbol),
        rejected = cli::col_yellow(symbol),
        failed = cli::col_red(symbol),
        cancelled = cli::col_red(symbol),
        running = cli::col_cyan(symbol),
        active = cli::col_cyan(symbol),
        current = cli::col_cyan(symbol),
        symbol
    )
}

# Format the workflow as a compact stage rail. Wide terminals show the whole
# route; narrow terminals retain only the current and next stages.
shift__ui_stage_rail <- function(state, width = shift__ui_width(),
                                 motion = "none", frame = 0L) {
    width <- shift__ui_width(width)
    sequence <- as.character(shift_coalesce(state$stage_sequence, character()))
    current <- as.character(shift_coalesce(state$stage, "planned"))[[1L]]
    if (!length(sequence)) {
        sequence <- unique(c(current, as.character(shift_coalesce(state$next_stage, character()))))
    }
    sequence <- sequence[!is.na(sequence) & nzchar(sequence)]
    if (!length(sequence)) {
        return(shift__ui_labeled_lines("Flow",
            "Waiting for workflow stages", width))
    }
    short <- c(
        collect = "Collect", resolve = "Resolve", download = "Download",
        extract = "Extract", extract_future = "Future",
        extract_reference = "Reference", coverage = "Coverage",
        morph = "Morph", write_epw = "EPW", export_epw = "Export"
    )
    labels <- vapply(sequence, function(stage) {
        if (stage %in% names(short)) short[[stage]] else shift__ui_stage_label(stage)
    }, character(1L))
    completed <- as.character(shift_coalesce(state$completed_stages, character()))
    terminal <- as.character(shift_coalesce(state$status, "running"))[[1L]]
    current_index <- match(current, sequence)
    values <- vapply(seq_along(sequence), function(i) {
        stage <- sequence[[i]]
        stage_status <- if (stage %in% completed ||
            identical(terminal, "completed")) {
            "completed"
        } else if (identical(stage, current)) {
            if (terminal %in% c("failed", "cancelled", "partial", "stopping")) terminal else "current"
        } else {
            "pending"
        }
        label <- switch(stage_status,
            completed = cli::col_green(labels[[i]]),
            current = cli::style_bold(cli::col_cyan(labels[[i]])),
            failed = cli::style_bold(cli::col_red(labels[[i]])),
            cancelled = cli::style_bold(cli::col_red(labels[[i]])),
            partial = cli::style_bold(cli::col_yellow(labels[[i]])),
            stopping = cli::style_bold(cli::col_yellow(labels[[i]])),
            cli::style_dim(labels[[i]])
        )
        paste(shift__ui_state_symbol(stage_status, motion = "none", frame), label)
    }, character(1L))
    connector <- cli::style_dim("  \u203a  ")
    full <- shift__ui_labeled_line("Flow", paste(values, collapse = connector))
    if (cli::ansi_nchar(full, type = "width") <= width) {
        return(full)
    }
    position <- if (is.na(current_index)) 1L else current_index
    # Prefer a current-plus-next compact rail, then drop the preview when the
    # terminal narrows further. This preserves stage identity without blindly
    # trimming a long full rail at an arbitrary glyph.
    next_value <- if (position < length(values)) {
        paste("next", values[[position + 1L]])
    } else {
        "final stage"
    }
    candidates <- c(
        sprintf("[%d/%d] %s \u00b7 %s", position, length(sequence),
            values[[position]], next_value),
        sprintf("[%d/%d] %s", position, length(sequence), values[[position]]),
        values[[position]]
    )
    rows <- vapply(candidates, function(value) {
        shift__ui_labeled_line("Flow", value)
    }, character(1L))
    fitting <- which(cli::ansi_nchar(rows, type = "width") <= width)
    if (length(fitting)) {
        return(rows[[fitting[[1L]]]])
    }
    shift__ui_fit(rows[[length(rows)]], width)
}

# Draw a width-bounded determinate bar using display-safe block characters.
shift__ui_bar <- function(current, total, width = 18L) {
    width <- max(4L, as.integer(width))
    if (is.na(current) || is.na(total) || total <= 0) {
        return(cli::style_dim(strrep("\u2500", width)))
    }
    ratio <- max(0, min(1, current / total))
    filled <- min(width, as.integer(floor(ratio * width)))
    paste0(
        cli::col_cyan(strrep("\u2501", filled)),
        cli::style_dim(strrep("\u2500", width - filled))
    )
}

# Select the stage-specific count used by the determinate progress row.
shift__ui_progress_values <- function(state) {
    details <- shift_coalesce(state$current_details, list())
    current <- shift__ui_metric_number(details, "current",
        shift_coalesce(state$unit_current, NA_real_))
    total <- shift__ui_metric_number(details, "total",
        shift_coalesce(state$unit_total, NA_real_))
    if (identical(state$stage, "coverage")) {
        current <- as.numeric(shift_coalesce(state$cases_ready, 0L))
        total <- as.numeric(shift_coalesce(state$cases_total, 0L))
    }
    list(current = current, total = total)
}

# Keep two recent business milestones below a quiet section heading. Long
# milestones use indented continuation rows; the framebuffer already owns a
# variable-height region and erases stale rows when the next frame contracts.
shift__ui_recent_lines <- function(state, width = shift__ui_width()) {
    values <- as.character(shift_coalesce(state$recent_events,
        shift_coalesce(state$last_event, character())))
    values <- values[!is.na(values) & nzchar(values)]
    values <- utils::tail(values, 2L)
    outcomes <- as.character(shift_coalesce(state$recent_outcomes,
        character()))
    if (length(outcomes) < length(values)) {
        outcomes <- c(rep("completed", length(values) - length(outcomes)), outcomes)
    }
    outcomes <- utils::tail(outcomes, length(values))
    if (!length(values)) {
        values <- "No completed activity yet"
        outcomes <- "pending"
    }
    values <- unlist(lapply(seq_along(values), function(i) {
        prefix <- paste0("  ",
            shift__ui_state_symbol(outcomes[[i]], motion = "none"), " ")
        shift__ui_prefixed_lines(prefix, values[[i]], width)
    }), use.names = FALSE)
    values <- c(values, rep("", max(0L, 2L - length(values))))
    c(
        shift__ui_fit(shift__ui_labeled_line("Recent", ""), width),
        values
    )
}

# Render a durable completion receipt from final case counts and exported paths.
# The output directory carries location context once; individual rows therefore
# use basenames so the useful scenario/period identity survives narrow widths.
shift__ui_result_lines <- function(state, width = shift__ui_width()) {
    outputs <- as.integer(shift_coalesce(state$outputs_completed, 0L))
    total <- as.integer(shift_coalesce(state$cases_total, outputs))
    if (is.na(total) || total < outputs) {
        total <- outputs
    }
    missing <- max(0L, total - outputs)
    summary <- shift_coalesce(state$result_summary, sprintf(
        "%d/%d EPW%s exported \u00b7 %d missing",
        outputs, total, if (total == 1L) "" else "s", missing
    ))
    lines <- shift__ui_labeled_lines("Summary", summary, width)

    output_dir <- shift_coalesce(state$output_dir,
        shift_coalesce(state$plan_context$output, NULL))
    if (!is.null(output_dir) && length(output_dir) &&
        !is.na(output_dir[[1L]]) && nzchar(output_dir[[1L]])) {
        lines <- c(lines, shift__ui_labeled_lines(
            "Output", shift_display_path(output_dir[[1L]]), width))
    }

    paths <- as.character(shift_coalesce(state$output_paths, character()))
    paths <- unique(paths[!is.na(paths) & nzchar(paths)])
    limit <- suppressWarnings(as.numeric(shift_coalesce(
        state$output_path_limit, 5L))[[1L]])
    if (!length(limit) || is.na(limit) || limit < 1) {
        limit <- 5L
    }
    shown <- if (is.finite(limit)) utils::head(paths, as.integer(limit)) else paths
    omitted <- length(paths) - length(shown)
    if (length(shown)) {
        for (i in seq_along(shown)) {
            lines <- c(lines, shift__ui_labeled_lines(
                if (i == 1L) "Files" else "",
                basename(shown[[i]]), width
            ))
        }
    }
    if (omitted > 0L) {
        lines <- c(lines, shift__ui_labeled_lines(
            "", sprintf("\u2026 %d more output%s", omitted,
                if (omitted == 1L) "" else "s"), width))
    }
    lines
}

# Render one compact terminal diagnosis from structured failure fields. Values
# wrap under their semantic prefix so the durable failure card preserves the
# actionable cause and closest-candidate evidence at every terminal width.
shift__ui_failure_lines <- function(state, width = shift__ui_width()) {
    failure <- shift_coalesce(state$failure_details, list())
    # Missing counters are valid for non-resolver failures and render as zero
    # rather than leaking NA into the fixed terminal row.
    number <- function(name) {
        value <- suppressWarnings(as.integer(failure[[name]]))
        if (!length(value) || is.na(value[[1L]])) 0L else value[[1L]]
    }
    counts <- c(
        if (number("coverage_failures")) sprintf(
            "%d incomplete", number("coverage_failures")),
        if (number("timeout_failures")) sprintf(
            "%d timeout", number("timeout_failures")),
        if (number("network_failures")) sprintf(
            "%d network", number("network_failures")),
        if (number("other_failures")) sprintf(
            "%d other", number("other_failures"))
    )
    checked <- number("nodes_checked")
    summary <- if (checked > 0L) {
        paste(c(sprintf("%d checked", checked), counts,
            sprintf("%d usable", number("usable_nodes"))),
            collapse = " \u00b7 ")
    } else {
        shift_coalesce(failure$kind, "workflow failed")
    }
    reason <- as.character(shift_coalesce(
        failure$cause,
        shift_coalesce(failure$summary,
            shift_coalesce(state$last_event, "Workflow failed"))))[[1L]]
    closest <- shift_coalesce(failure$closest, list())
    identity <- c(closest$model, closest$member, closest$grid)
    identity <- as.character(identity[!vapply(identity, is.null, logical(1L))])
    identity <- identity[!is.na(identity) & nzchar(identity)]
    missing <- as.character(shift_coalesce(failure$missing, character()))
    missing <- missing[!is.na(missing) & nzchar(missing)]
    evidence <- c(
        if (length(identity)) paste("Closest", paste(identity, collapse = "/")),
        if (length(missing)) paste("Missing", missing[[1L]])
    )
    if (!length(evidence)) {
        evidence <- "Inspect the persisted run for complete diagnostics"
    }
    failed_prefix <- paste0("  ",
        shift__ui_state_symbol("failed", motion = "none"), " ")
    info_prefix <- paste0("  ", cli::col_blue("i"), " ")
    c(
        shift__ui_labeled_lines("Summary", summary, width),
        shift__ui_prefixed_lines(failed_prefix, reason, width),
        shift__ui_prefixed_lines(info_prefix,
            paste(evidence, collapse = " \u00b7 "), width)
    )
}

# Reduce a resolver outcome to a stable, actionable phrase for the live frame.
# Complete errors remain available in persisted events, detail tables, and logs.
shift__ui_node_result_short <- function(row) {
    outcome <- as.character(shift_coalesce(row$outcome, "rejected"))[[1L]]
    result <- as.character(shift_coalesce(row$result, outcome))[[1L]]
    future <- suppressWarnings(as.numeric(shift_coalesce(row$future, NA_real_)))
    reference <- suppressWarnings(as.numeric(shift_coalesce(row$reference, NA_real_)))
    if (outcome %in% c("completed", "skipped", "reused")) {
        return(result)
    }
    if (!is.na(reference) && reference == 0 && !is.na(future) && future > 0) {
        return("no reference files")
    }
    if (!is.na(future) && future == 0) {
        return("no future files")
    }
    switch(shift__ui_error_kind(result),
        timeout = "request timed out",
        network = "network error",
        coverage = "incomplete coverage",
        ambiguity = "ambiguous selection",
        shift__ui_fit(shift__error_summary(result), 48L)
    )
}

# Show at most two completed resolver decisions. The active node already owns
# the single animated `Now` row, so repeating it here would create visual noise.
shift__ui_live_node_lines <- function(state, width = shift__ui_width(),
                                      motion = "none", frame = 0L) {
    rows <- data.table::as.data.table(shift_coalesce(state$node_rows,
        data.table::data.table()))
    attempts <- nrow(rows)
    values <- character()
    if (nrow(rows)) {
        rows <- utils::tail(rows, 2L)
        values <- unlist(lapply(seq_len(nrow(rows)), function(i) {
            outcome <- if ("outcome" %in% names(rows)) {
                as.character(shift_coalesce(rows$outcome[[i]], "rejected"))
            } else {
                "rejected"
            }
            row <- as.list(rows[i])
            counts <- character()
            if ("future" %in% names(rows) && !is.na(rows$future[[i]])) {
                counts <- c(counts, sprintf("%d future", rows$future[[i]]))
            }
            if ("reference" %in% names(rows) && !is.na(rows$reference[[i]])) {
                counts <- c(counts, sprintf("%d reference", rows$reference[[i]]))
            }
            if ("duration" %in% names(rows) && !is.na(rows$duration[[i]]) &&
                nzchar(rows$duration[[i]]) && !identical(rows$duration[[i]], "\u2014")) {
                counts <- c(counts, rows$duration[[i]])
            }
            suffix <- if (length(counts)) paste0(" \u00b7 ",
                paste(counts, collapse = " \u00b7 ")) else ""
            prefix <- sprintf("  %s %-6s ",
                shift__ui_state_symbol(outcome, motion = "none", frame),
                rows$node[[i]])
            shift__ui_prefixed_lines(prefix,
                paste0(shift__ui_node_result_short(row), suffix), width)
        }), use.names = FALSE)
    }
    values <- c(values, rep("", max(0L, 2L - length(values))))
    details <- shift_coalesce(state$current_details, list())
    current <- shift__ui_metric_number(details, "current", 0)
    total <- shift__ui_metric_number(details, "total", NA_real_)
    heading <- if (!is.na(total) && total > 0) {
        sprintf("%d tried \u00b7 %d remaining", attempts,
            max(0L, as.integer(total) - as.integer(current)))
    } else {
        sprintf("%d tried", attempts)
    }
    c(
        shift__ui_fit(shift__ui_labeled_line("Attempts", heading), width),
        vapply(values, shift__ui_fit, character(1L), width = width)
    )
}

# Render the shared responsive live dashboard used by foreground reporters and
# shift_watch(). Stable row ownership keeps animation readable in R terminals;
# wide terminals add labelled section rules while narrow terminals omit chrome.
shift__ui_status_lines <- function(state, width = shift__ui_width(),
                                   motion = c("none", "full", "reduced"),
                                   frame = 0L) {
    motion <- match.arg(motion)
    terminal_width <- shift__ui_width(width)
    width <- shift__ui_dashboard_width(terminal_width)
    panel <- terminal_width >= 60L
    # Panel borders consume two glyphs and their interior padding consumes two
    # more. Format semantic content against that real budget first so the panel
    # renderer never has to crop an otherwise wrappable value.
    content_width <- if (isTRUE(panel)) max(1L, width - 4L) else width
    elapsed <- shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    run_label <- shift__ui_run_short(state$run_id)
    status <- as.character(shift_coalesce(state$status, "running"))[[1L]]
    plan_context <- shift_coalesce(state$plan_context, list())
    task_label <- as.character(shift_coalesce(state$task_label,
        shift_coalesce(plan_context$title, "Future EPW")))[[1L]]
    header_parts <- c(
        cli::style_bold(task_label),
        shift__ui_status_style(status),
        cli::style_dim(elapsed),
        if (nzchar(run_label)) cli::style_dim(paste("run", run_label))
    )
    header <- paste(header_parts[!vapply(header_parts, is.null, logical(1L))],
        collapse = "  ")
    plan_lines <- shift__ui_plan_lines(plan_context, width = content_width)
    details <- shift_coalesce(state$current_details, list())
    current_context <- character()
    if (!is.null(details$node) && length(details$node) && !is.na(details$node[[1L]])) {
        current_context <- c(current_context, shift__node_label(details$node[[1L]]))
    }
    if (!is.null(details$catalog_role) && length(details$catalog_role) &&
        !is.na(details$catalog_role[[1L]])) {
        role <- as.character(details$catalog_role[[1L]])
        current_context <- c(current_context,
            if (role %in% c("future", "reference")) paste(role, "catalog") else role)
    }
    current_label <- shift_coalesce(state$unit_label,
        shift_coalesce(state$stage_message, "Waiting"))
    if (length(current_context)) {
        current_label <- paste(c(current_context, current_label), collapse = " \u00b7 ")
    }
    current_status <- if (status %in% c("queued", "waiting", "stopping", "completed",
        "partial", "failed", "cancelled")) status else "running"
    current_label_name <- if (identical(status, "failed")) {
        "Failure"
    } else if (identical(status, "cancelled")) {
        "Stopped"
    } else {
        "Now"
    }
    current <- shift__ui_labeled_lines(current_label_name, sprintf("%s %s",
        shift__ui_state_symbol(current_status, motion, frame),
        cli::style_bold(current_label)), content_width)
    metrics <- shift__ui_metric_line(state, width = content_width)
    terminal_problem <- status %in% c("failed", "cancelled")
    terminal_result <- status %in% c("completed", "partial", "waiting")
    context <- if (isTRUE(terminal_problem)) {
        shift__ui_failure_lines(state, content_width)
    } else if (isTRUE(terminal_result)) {
        shift__ui_result_lines(state, content_width)
    } else if (identical(state$stage, "resolve")) {
        shift__ui_live_node_lines(state, content_width, motion, frame)
    } else {
        shift__ui_recent_lines(state, content_width)
    }
    header <- shift__ui_fit(header, width)
    plan_lines <- vapply(plan_lines, shift__ui_fit, character(1L),
        width = content_width)
    workflow <- vapply(c(
        shift__ui_stage_rail(state, content_width, motion, frame),
        current,
        metrics
    ), shift__ui_fit, character(1L), width = content_width)
    if (!isTRUE(panel)) {
        return(c(header, plan_lines, workflow, context))
    }
    c(
        shift__ui_panel_rule(header, width, "top"),
        vapply(plan_lines, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(cli::style_bold("Workflow"), width, "middle"),
        vapply(workflow, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(cli::style_bold(
            if (isTRUE(terminal_problem)) {
                "Diagnosis"
            } else if (isTRUE(terminal_result)) {
                "Results"
            } else {
                "Activity"
            }
        ), width, "middle"),
        vapply(context, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(width = width, kind = "bottom")
    )
}

# Collapse the same semantic dashboard into one useful status row for RStudio
# and other dynamic consoles that support carriage returns but not cursor-up.
shift__ui_compact_line <- function(state, width = shift__ui_width(),
                                   motion = c("none", "full", "reduced"),
                                   frame = 0L) {
    motion <- match.arg(motion)
    width <- shift__ui_width(width)
    status <- as.character(shift_coalesce(state$status, "running"))[[1L]]
    stage <- shift__ui_stage_label(shift_coalesce(state$stage, "planned"))
    values <- shift__ui_progress_values(state)
    counter <- if (!is.na(values$current) && !is.na(values$total) &&
        values$total > 0) {
        sprintf("%d/%d", as.integer(values$current), as.integer(values$total))
    } else {
        NULL
    }
    details <- shift_coalesce(state$current_details, list())
    context <- character()
    if (!is.null(details$node) && length(details$node) &&
        !is.na(details$node[[1L]])) {
        context <- c(context, shift__node_label(details$node[[1L]]))
    }
    if (!is.null(details$catalog_role) && length(details$catalog_role) &&
        !is.na(details$catalog_role[[1L]])) {
        context <- c(context, as.character(details$catalog_role[[1L]]))
    }
    unit <- as.character(shift_coalesce(state$unit_label,
        shift_coalesce(state$stage_message, "Waiting")))[[1L]]
    # Avoid repeating node/catalog prefixes already embedded in the business
    # unit while still retaining them for terse persisted unit labels.
    if (length(context) && !all(vapply(context, grepl, logical(1L),
        x = unit, fixed = TRUE))) {
        unit <- paste(c(context, unit), collapse = " \u00b7 ")
    }
    marker <- if (status %in% c("failed", "cancelled", "partial", "stopping", "waiting",
        "completed")) status else "running"
    parts <- c(
        paste(shift__ui_state_symbol(marker, motion, frame), stage),
        counter,
        unit,
        shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    )
    shift__ui_fit(paste(parts[!is.na(parts) & nzchar(parts)],
        collapse = " \u00b7 "), width)
}

# Format byte counts locally so workflow UI does not depend on units objects or
# on the downloader's table renderer.
shift__ui_bytes <- function(bytes) {
    if (is.null(bytes) || !length(bytes)) {
        return("?")
    }
    bytes <- suppressWarnings(as.numeric(bytes[[1L]]))
    if (!length(bytes) || is.na(bytes) || !is.finite(bytes)) {
        return("?")
    }
    labels <- c("B", "KiB", "MiB", "GiB", "TiB")
    power <- if (bytes <= 0) 0L else min(floor(log(bytes, 1024)), length(labels) - 1L)
    value <- bytes / (1024^power)
    sprintf(if (power == 0L) "%.0f %s" else "%.1f %s", value, labels[[power + 1L]])
}

# Convert an index-node URL into the stable short name used in every normal and
# detail view. Unknown nodes fall back to their host name.
shift__node_label <- function(node) {
    node <- as.character(shift_coalesce(node, "unknown"))[[1L]]
    normalized <- tryCatch(query__normalize_node(node), error = function(e) node)
    known <- vapply(INDEX_NODES, function(value) {
        identical(tryCatch(query__normalize_node(value), error = function(e) value),
            normalized)
    }, logical(1L))
    if (any(known)) {
        return(names(INDEX_NODES)[which(known)[[1L]]])
    }
    parsed <- tryCatch(curl::curl_parse_url(normalized), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$host) || !nzchar(parsed$host)) {
        normalized
    } else {
        parsed$host
    }
}

# Classify common resolver failures into short, stable categories while keeping
# the complete error text available in the result column and persisted event.
shift__ui_error_kind <- function(message) {
    message <- tolower(as.character(shift_coalesce(message, ""))[[1L]])
    if (grepl("timed? out|timeout|operation too slow", message)) return("timeout")
    if (grepl("http|status code|could not resolve|connect|ssl|certificate", message)) return("network")
    if (grepl("missing|coverage|complete|required variable|year|empty|no .*files?", message)) return("coverage")
    if (grepl("ambiguous|multiple|more than one", message)) return("ambiguity")
    "error"
}

# Format resolver attempts as a width-safe table. Normal output uses stable
# short outcomes; detail and debug retain the complete persisted exception.
shift__ui_node_table <- function(rows, width = shift__ui_width(),
                                 detail = "normal") {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift__ui_width(width)
    display_max <- function(x) max(cli::ansi_nchar(as.character(x), type = "width"))
    node_width <- min(12L, max(4L, display_max(c("Node", rows$node))))
    include_counts <- width >= 56L
    include_duration <- width >= 72L && "duration" %in% names(rows)
    columns <- c("Node")
    sizes <- c(node_width)
    if (include_counts) {
        columns <- c(columns, "Future", "Reference")
        sizes <- c(sizes, 7L, 9L)
    }
    if (include_duration) {
        columns <- c(columns, "Time")
        sizes <- c(sizes, 7L)
    }
    result_width <- max(1L, width - 2L - sum(sizes) - 2L * length(sizes))
    columns <- c(columns, "Result")
    sizes <- c(sizes, result_width)
    row_line <- function(values) {
        shift__ui_fit(paste0("  ", paste(mapply(
            shift__ui_cell, values, sizes, USE.NAMES = FALSE), collapse = "  ")), width)
    }
    lines <- c(cli::style_bold("Resolver attempts"),
        cli::style_dim(row_line(columns)))
    for (i in seq_len(nrow(rows))) {
        values <- c(rows$node[[i]])
        if (include_counts) {
            values <- c(values, rows$future[[i]], rows$reference[[i]])
        }
        if (include_duration) {
            values <- c(values, rows$duration[[i]])
        }
        result <- if (identical(detail, "normal")) {
            shift__ui_node_result_short(as.list(rows[i]))
        } else {
            # Persisted cli conditions may carry semantic ANSI styling that is
            # misleading after truncation; the table applies its own outcomes.
            cli::ansi_strip(rows$result[[i]])
        }
        values <- c(values, result)
        lines <- c(lines, row_line(values))
    }
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Format user cases independently from extraction plans. Narrow terminals omit
# the member column before truncating scenario or missing-reason information.
shift__ui_case_table <- function(rows, width = shift__ui_width(),
                                 detail = "normal") {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift__ui_width(width)
    scenario <- if ("experiment_id" %in% names(rows)) rows$experiment_id else rep("\u2014", nrow(rows))
    period <- if ("period" %in% names(rows)) rows$period else rep("\u2014", nrow(rows))
    member <- if ("variant_label" %in% names(rows)) rows$variant_label else rep("\u2014", nrow(rows))
    status <- if ("status" %in% names(rows)) rows$status else rep("unknown", nrow(rows))
    include_member <- width >= 68L
    display_max <- function(x) max(cli::ansi_nchar(as.character(x), type = "width"))
    scenario_width <- min(14L, max(8L, display_max(c("Scenario", scenario))))
    period_width <- min(12L, max(6L, display_max(c("Period", period))))
    member_width <- if (include_member) min(14L, max(6L,
        display_max(c("Member", member)))) else 0L
    fixed <- scenario_width + period_width + member_width +
        if (include_member) 10L else 7L
    status_width <- max(10L, width - fixed)
    header <- if (include_member) {
        sprintf("  %s  %s  %s  %s", shift__ui_cell("Scenario", scenario_width),
            shift__ui_cell("Period", period_width), shift__ui_cell("Member", member_width),
            shift__ui_cell("Status", status_width))
    } else {
        sprintf("  %s  %s  %s", shift__ui_cell("Scenario", scenario_width),
            shift__ui_cell("Period", period_width), shift__ui_cell("Status", status_width))
    }
    lines <- c("Cases", header)
    for (i in seq_len(nrow(rows))) {
        value <- status[[i]]
        line <- if (include_member) {
            sprintf("  %s  %s  %s  %s", shift__ui_cell(scenario[[i]], scenario_width),
                shift__ui_cell(period[[i]], period_width), shift__ui_cell(member[[i]], member_width),
                shift__ui_cell(value, status_width))
        } else {
            sprintf("  %s  %s  %s", shift__ui_cell(scenario[[i]], scenario_width),
                shift__ui_cell(period[[i]], period_width), shift__ui_cell(value, status_width))
        }
        lines <- c(lines, line)
        if (!identical(detail, "normal") && "missing_reason" %in% names(rows) &&
            !is.na(rows$missing_reason[[i]]) && nzchar(rows$missing_reason[[i]])) {
            reason_width <- max(1L, width - 4L)
            wrapped <- cli::ansi_strwrap(rows$missing_reason[[i]], width = reason_width)
            lines <- c(lines, paste0("    ", wrapped))
        }
    }
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Decode persisted event details without allowing a malformed historical event
# to break shift_watch() for the rest of an otherwise readable run.
shift__ui_event_details <- function(events) {
    if (!nrow(events)) {
        return(list())
    }
    if (!"details_json" %in% names(events)) {
        return(rep(list(list()), nrow(events)))
    }
    lapply(events$details_json, function(value) {
        if (is.null(value) || !length(value) || is.na(value) || !nzchar(value)) {
            return(list())
        }
        tryCatch(jsonlite::fromJSON(value, simplifyVector = TRUE),
            error = function(e) list())
    })
}

# Rebuild the planned stage route from the persisted scientific specification
# before a queued worker has emitted its first reporter event.
shift__ui_stage_sequence_from_row <- function(row) {
    row <- data.table::as.data.table(row)
    if (!nrow(row) || !"spec_json" %in% names(row) ||
        is.na(row$spec_json[[1L]]) || !nzchar(row$spec_json[[1L]])) {
        return(character())
    }
    spec <- tryCatch(jsonlite::fromJSON(row$spec_json[[1L]],
        simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(spec)) {
        return(character())
    }
    task <- as.character(shift_coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        current <- as.character(shift_coalesce(row$current_stage[[1L]], task))
        return(current)
    }
    reference_mode <- as.character(shift_coalesce(
        spec$method$reference_mode, "none"))[[1L]]
    download <- as.character(shift_coalesce(spec$control$download, "auto"))[[1L]]
    c(
        "resolve",
        if (identical(download, "always")) "download",
        "extract_future",
        if (identical(reference_mode, "historical")) "extract_reference",
        "coverage", "morph", "write_epw"
    )
}

# Format the named period list stored in a canonical workflow specification
# without reconstructing a complete ShiftPlan in watch clients.
shift__ui_periods_from_spec <- function(periods) {
    if (is.null(periods) || !length(periods)) {
        return("no periods")
    }
    if (is.atomic(periods) && !is.null(names(periods))) {
        periods <- split(as.integer(periods), names(periods))
    }
    labels <- names(periods)
    if (is.null(labels) || !length(labels)) {
        labels <- rep("period", length(periods))
    }
    paste(vapply(seq_along(periods), function(i) {
        years <- suppressWarnings(as.integer(periods[[i]]))
        years <- years[!is.na(years)]
        if (!length(years)) {
            return(labels[[i]])
        }
        if (length(unique(years)) == 1L) {
            sprintf("%s (%d)", labels[[i]], years[[1L]])
        } else {
            sprintf("%s (%d\u2013%d)", labels[[i]], min(years), max(years))
        }
    }, character(1L)), collapse = ", ")
}

# Describe a persisted reference using only explicit values in the run spec;
# this display helper never infers a historical reference from missing data.
shift__ui_reference_from_spec <- function(method) {
    mode <- as.character(shift_coalesce(method$reference_mode, "none"))[[1L]]
    if (identical(mode, "baseline_epw")) return("baseline EPW")
    if (identical(mode, "none")) return("no reference")
    if (identical(mode, "historical")) {
        periods <- shift__ui_periods_from_spec(method$reference$periods)
        periods <- sub("^[^(]+ \\(", "", periods)
        periods <- sub("\\)$", "", periods)
        return(paste("historical", periods))
    }
    "supplied reference"
}

# Rebuild the one-line dashboard context from persisted intent so foreground,
# R watch, and CLI watch retain the same visual hierarchy across sessions.
shift__ui_plan_context_from_row <- function(row, cases_total = 0L) {
    row <- data.table::as.data.table(row)
    if (!nrow(row) || !"spec_json" %in% names(row) ||
        is.na(row$spec_json[[1L]]) || !nzchar(row$spec_json[[1L]])) {
        return(list())
    }
    spec <- tryCatch(jsonlite::fromJSON(row$spec_json[[1L]],
        simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(spec)) {
        return(list())
    }
    task <- as.character(shift_coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        current <- as.character(shift_coalesce(row$current_stage[[1L]], task))
        label <- shift__task_label(current)
        return(list(
            title = label,
            line = label,
            items = c(label, sprintf("store %s",
                shift_display_path(shift_coalesce(spec$store, "<store>")))),
            selection = NULL,
            output = if ("output_dir" %in% names(row)) row$output_dir[[1L]] else NULL
        ))
    }
    climate <- shift_coalesce(spec$climate, spec$request)
    model <- paste(as.character(shift_coalesce(
        climate$model, climate$source)), collapse = ", ")
    scenarios <- paste(as.character(shift_coalesce(
        climate$scenarios, climate$experiment)), collapse = " + ")
    method <- as.character(shift_coalesce(spec$method$name, "method"))[[1L]]
    reference <- shift__ui_reference_from_spec(spec$method)
    expected <- as.integer(cases_total)
    line <- c(
        if (nzchar(model)) model,
        if (nzchar(scenarios)) scenarios,
        shift__ui_periods_from_spec(spec$periods),
        sprintf("%s / %s", method, reference),
        if (expected > 0L) sprintf("%d EPW%s", expected,
            if (expected == 1L) "" else "s")
    )
    member <- if (is.null(spec$climate$member)) "member auto" else
        sprintf("member %s", paste(spec$climate$member, collapse = ", "))
    grid <- if (is.null(spec$climate$grid)) "grid auto" else
        sprintf("grid %s", paste(spec$climate$grid, collapse = ", "))
    list(
        line = paste(line, collapse = " \u00b7 "),
        items = line,
        selection = paste(member, grid, sep = " \u00b7 "),
        output = if ("output_dir" %in% names(row)) row$output_dir[[1L]] else NULL
    )
}

# Reconstruct the same semantic live state from persisted tables that the
# foreground reporter maintains in memory.
shift__ui_table_state <- function(row, events, cases) {
    row <- data.table::as.data.table(row)
    events <- data.table::as.data.table(events)
    cases <- data.table::as.data.table(cases)
    details <- shift__ui_event_details(events)
    stage <- row$current_stage[[1L]]
    stage_indices <- which(vapply(details, function(x) {
        isTRUE(x$phase %in% c("stage", "operation")) &&
            identical(x$stage, stage)
    }, logical(1L)))
    stage_index <- if (length(stage_indices)) utils::tail(stage_indices, 1L) else NA_integer_
    unit_indices <- which(vapply(details, function(x) {
        identical(x$phase, "unit") && identical(x$stage, stage)
    }, logical(1L)))
    unit_index <- if (length(unit_indices)) utils::tail(unit_indices, 1L) else NA_integer_
    milestone_indices <- which(events$status %in%
        c("completed", "skipped", "rejected", "fallback", "failed", "cancelled", "partial"))
    last_index <- if (length(milestone_indices)) utils::tail(milestone_indices, 1L) else NA_integer_
    failure_indices <- which(events$status %in% c("failed", "cancelled"))
    failure_index <- if (length(failure_indices)) {
        utils::tail(failure_indices, 1L)
    } else {
        NA_integer_
    }
    recent_indices <- utils::tail(milestone_indices, 3L)
    completed_stages <- unique(vapply(seq_along(details), function(i) {
        operation_done <- identical(details[[i]]$phase, "operation") &&
            isTRUE(details[[i]]$outcome %in% c("completed", "partial"))
        if ((identical(details[[i]]$phase, "stage") &&
            identical(events$status[[i]], "completed")) || operation_done) {
            as.character(details[[i]]$stage)
        } else {
            NA_character_
        }
    }, character(1L)))
    completed_stages <- completed_stages[!is.na(completed_stages)]
    started_at <- row$started_at[[1L]]
    stopped_at <- row$completed_at[[1L]]
    if (is.na(stopped_at)) {
        terminal <- row$status[[1L]] %in%
            c("waiting", "completed", "partial", "failed", "cancelled")
        # Older or partially written terminal rows may lack completed_at. In
        # that case freeze elapsed time at their last durable activity rather
        # than making a completed or failed run appear to keep executing.
        if (isTRUE(terminal) && "updated_at" %in% names(row) &&
            !is.na(row$updated_at[[1L]])) {
            stopped_at <- row$updated_at[[1L]]
        } else if (isTRUE(terminal) && nrow(events) &&
            "created_at" %in% names(events) &&
            !is.na(events$created_at[[nrow(events)]])) {
            stopped_at <- events$created_at[[nrow(events)]]
        } else {
            stopped_at <- Sys.time()
        }
    }
    elapsed <- if (is.na(started_at)) 0 else as.numeric(difftime(
        stopped_at, started_at, units = "secs"))
    stage_details <- if (is.na(stage_index)) list() else details[[stage_index]]
    unit_details <- if (is.na(unit_index)) list() else details[[unit_index]]
    stage_sequence <- as.character(shift_coalesce(
        stage_details$stage_sequence, character()))
    if (!length(stage_sequence)) {
        stage_sequence <- shift__ui_stage_sequence_from_row(row)
    }
    fallback_stage_message <- switch(row$status[[1L]],
        queued = "Waiting for background worker",
        waiting = "Waiting for the next shift stage",
        completed = "Workflow completed",
        partial = "Workflow completed with missing cases",
        failed = "Workflow failed",
        cancelled = "Workflow cancelled",
        stopping = "Waiting for cancellation boundary",
        "Waiting for next workflow event"
    )
    plan_context <- shift__ui_plan_context_from_row(row, nrow(cases))
    output_paths <- if ("export_path" %in% names(cases)) {
        as.character(cases$export_path)
    } else {
        character()
    }
    list(
        run_id = row$run_id[[1L]],
        task_label = shift_coalesce(plan_context$title, "Future EPW"),
        status = row$status[[1L]],
        stage = stage,
        stage_message = if (is.na(stage_index)) {
            fallback_stage_message
        } else {
            events$message[[stage_index]]
        },
        stage_current = stage_details$current,
        stage_total = stage_details$total,
        unit_label = if (is.na(unit_index)) NULL else events$message[[unit_index]],
        unit_current = unit_details$current,
        unit_total = unit_details$total,
        current_details = unit_details,
        next_stage = stage_details$next_stage,
        stage_sequence = stage_sequence,
        completed_stages = completed_stages,
        plan_context = plan_context,
        cases_ready = sum(cases$status %in% c("ready", "morphing", "morphed", "completed")),
        cases_total = if (nrow(cases)) nrow(cases) else 0L,
        outputs_completed = sum(cases$status %in% "completed"),
        output_dir = plan_context$output,
        output_paths = output_paths,
        output_path_limit = 5L,
        result_summary = if (row$status[[1L]] %in%
            c("waiting", "completed", "partial") && !is.na(last_index)) {
            events$message[[last_index]]
        } else {
            NULL
        },
        last_event = if (is.na(last_index)) "No completed event yet" else events$message[[last_index]],
        recent_events = if (!length(recent_indices)) character() else
            as.character(events$message[recent_indices]),
        recent_outcomes = if (!length(recent_indices)) character() else
            as.character(events$status[recent_indices]),
        node_rows = shift__ui_event_nodes(events),
        failure_details = if (is.na(failure_index)) list() else
            details[[failure_index]],
        elapsed_seconds = elapsed
    )
}

# Reconstruct the resolver-attempt table from terminal index-node events.
shift__ui_event_nodes <- function(events) {
    events <- data.table::as.data.table(events)
    details <- shift__ui_event_details(events)
    rows <- lapply(seq_along(details), function(i) {
        value <- details[[i]]
        if (!identical(value$unit_type, "index_node") ||
            !events$status[[i]] %in%
                c("completed", "skipped", "rejected", "failed")) {
            return(NULL)
        }
        data.table::data.table(
            node = shift__node_label(value$node),
            future = shift_coalesce(value$future_files, NA_integer_),
            reference = shift_coalesce(value$reference_files, NA_integer_),
            outcome = as.character(events$status[[i]]),
            duration = if (is.null(value$elapsed_seconds)) {
                "\u2014"
            } else {
                shift__format_elapsed(value$elapsed_seconds)
            },
            result = if (events$status[[i]] %in% c("completed", "skipped")) {
                shift_coalesce(value$result, "selected")
            } else {
                error <- shift_coalesce(value$error, events$message[[i]])
                kind <- shift_coalesce(value$error_kind, shift__ui_error_kind(error))
                sprintf("%s: %s", kind, error)
            }
        )
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Build the complete watch view once so R and CLI renderers cannot drift in
# stage, case, resolver, or width semantics.
shift__ui_table_view <- function(row, cases, events,
                                 width = shift__ui_width(), detail = "normal",
                                 motion = "none", frame = 0L) {
    state <- shift__ui_table_state(row, events, cases)
    # Normal watch output mirrors the foreground receipt's five-file cap;
    # explicit detail/debug views retain every persisted export path.
    state$output_path_limit <- if (identical(detail, "normal")) 5L else Inf
    list(
        state = state,
        lines = shift__ui_status_lines(state, width = width,
            motion = motion, frame = frame),
        compact = shift__ui_compact_line(state, width = width,
            motion = motion, frame = frame),
        nodes = shift__ui_node_table(shift__ui_event_nodes(events), width = width,
            detail = detail),
        cases = shift__ui_case_table(cases,
            width = width, detail = detail)
    )
}

# Adapt a live ShiftRun handle to the table-based view shared with the CLI.
shift__ui_run_view <- function(run, width = shift__ui_width(),
                               detail = "normal", motion = "none", frame = 0L) {
    view <- shift__ui_table_view(
        row = run@meta$run,
        cases = shift_cases(run, refresh = FALSE),
        events = run@meta$events,
        width = width,
        detail = detail,
        motion = motion,
        frame = frame
    )
    # Active background workers publish transient transfer state beside their
    # durable events. Prefer it for the four live rows while retaining tables
    # reconstructed from persisted resolver/case data.
    if (!is.null(run@meta$ui_state) && length(run@meta$ui_state)) {
        state <- run@meta$ui_state
        row <- data.table::as.data.table(run@meta$run)
        if (nrow(row) && row$status[[1L]] %in% c("queued", "running", "stopping") &&
            !is.na(row$started_at[[1L]])) {
            # Watch animation advances from wall-clock elapsed time even when
            # the worker has not emitted a new durable heartbeat frame.
            state$elapsed_seconds <- as.numeric(difftime(
                Sys.time(), row$started_at[[1L]], units = "secs"))
        }
        view$state <- state
        view$lines <- shift__ui_status_lines(state, width = width,
            motion = motion, frame = frame)
        view$compact <- shift__ui_compact_line(state, width = width,
            motion = motion, frame = frame)
    }
    view
}

# Render a complete persisted snapshot once. This is the non-animated fallback
# and the final frame for both R and CLI watch commands.
shift__ui_print_view <- function(view, include_tables = TRUE) {
    for (line in view$lines) {
        if (nzchar(cli::ansi_strip(line))) {
            cli::cli_verbatim(line)
        }
    }
    if (isTRUE(include_tables)) {
        for (line in c(view$nodes, view$cases)) {
            cli::cli_verbatim(line)
        }
    }
    invisible(view)
}

# Format one persisted event for append-only watch logs with the same stage,
# node, and catalog-role context used by foreground log reporters.
shift__ui_persisted_event_line <- function(event, detail = "normal",
                                           width = NULL) {
    details <- shift__ui_event_details(event)[[1L]]
    context <- c(shift__ui_stage_label(event$stage[[1L]]))
    if (!is.null(details$node) && length(details$node)) {
        context <- c(context, if (identical(detail, "debug")) {
            as.character(details$node[[1L]])
        } else {
            shift__node_label(details$node)
        })
    }
    if (!is.null(details$catalog_role) && length(details$catalog_role)) {
        context <- c(context, as.character(details$catalog_role[[1L]]))
    }
    line <- sprintf("%s [%s] %s",
        format(event$created_at[[1L]], "%F %T"),
        paste(context, collapse = "]["), event$message[[1L]])
    if (is.null(width)) line else shift__ui_fit(line, width)
}

# Select an event delta before applying any presentation limit so a watch
# client cannot silently lose milestones when more than one page arrives
# between polls. A missing cursor is reported separately because bounded live
# sidecars may legitimately have discarded older events.
shift__ui_event_delta <- function(events, last_event_id = NA_character_,
                                  initial_limit = 10L,
                                  initial = is.na(last_event_id)) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    newest <- if (nrow(events)) {
        as.character(events$event_id[[nrow(events)]])
    } else {
        NA_character_
    }
    if (isTRUE(initial)) {
        rows <- if (initial_limit == 0L) events[0] else utils::tail(
            events, initial_limit)
        return(list(rows = rows, cursor = newest, gap = FALSE))
    }
    if (!nrow(events)) {
        return(list(rows = events, cursor = last_event_id, gap = FALSE))
    }
    if (is.na(last_event_id) || !nzchar(last_event_id)) {
        return(list(rows = events, cursor = newest, gap = FALSE))
    }
    position <- match(last_event_id, events$event_id)
    if (is.na(position)) {
        return(list(rows = events, cursor = newest, gap = TRUE))
    }
    rows <- if (position < nrow(events)) {
        events[seq.int(position + 1L, nrow(events))]
    } else {
        events[0]
    }
    list(rows = rows, cursor = newest, gap = FALSE)
}

# Normalize event details to a stable JSON shape shared by Console reporters,
# persisted run events, and CLI/R watch views.
shift__progress_details <- function(stage = NULL, phase = NULL,
                                    unit_type = NULL, unit_label = NULL,
                                    current = NULL, total = NULL, node = NULL,
                                    scenario = NULL, variable = NULL, period = NULL,
                                    access_method = NULL, elapsed_seconds = NULL,
                                    outcome = NULL, ...) {
    values <- c(list(
        stage = stage,
        phase = phase,
        unit_type = unit_type,
        unit_label = unit_label,
        current = current,
        total = total,
        node = node,
        scenario = scenario,
        variable = variable,
        period = period,
        access_method = access_method,
        elapsed_seconds = elapsed_seconds,
        outcome = outcome
    ), list(...))
    values[!vapply(values, is.null, logical(1L))]
}

# ShiftReporter is the single runtime sink for workflow messages and durable
# milestone events. Heartbeats remain transient to avoid frequent store writes.
ShiftReporter <- R6::R6Class(
    "ShiftReporter",
    lock_class = TRUE,
    public = list(
        # Bind one reporter to a stable run/job identity and resolve its
        # presentation mode once for the lifetime of the execution attempt.
        initialize = function(ui = shift_ui(), store = NULL, run_id = NULL,
                              job_id = NULL, background = FALSE,
                              step_id = NULL) {
            if (!S7::S7_inherits(ui, ShiftUiOptions)) {
                cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
            }
            private$ui_value <- ui
            private$mode_value <- shift__ui_mode(ui)
            private$motion_value <- shift__ui_motion(ui, private$mode_value)
            private$renderer <- tryCatch(
                shift__ui_renderer(private$mode_value),
                error = function(e) NULL
            )
            # An explicitly requested dynamic mode still degrades safely when
            # the current output connection has no live rendering capability.
            if (identical(private$mode_value, "dynamic") &&
                is.null(private$renderer)) {
                private$mode_value <- "log"
                private$motion_value <- "none"
            }
            private$store <- store
            private$run_id_value <- run_id
            private$job_id_value <- job_id
            private$step_id_value <- step_id
            private$background <- isTRUE(background)
            private$started_at <- Sys.time()
            private$last_heartbeat <- as.POSIXct(NA)
            private$last_liveness <- as.POSIXct(NA)
            private$last_refresh <- as.POSIXct(NA)
            private$animation_frame <- 0L
            private$status <- if (isTRUE(background)) "queued" else "running"
        },

        # Start a generic standalone shift operation without requiring a
        # Future EPW plan. The same semantic state feeds foreground frames,
        # persisted events, and later shift_watch() reconstruction.
        operation_started = function(task, label, context = list(),
                                     stage_sequence = task,
                                     completed_stages = character()) {
            checkmate::assert_string(task, min.chars = 1L)
            checkmate::assert_string(label, min.chars = 1L)
            checkmate::assert_list(context)
            private$task_label <- label
            private$status <- "running"
            private$stage <- task
            private$stage_sequence <- unique(as.character(stage_sequence))
            private$completed_stages <- unique(as.character(completed_stages))
            private$stage_started_at <- Sys.time()
            private$stage_message <- shift_coalesce(context$message,
                paste("Preparing", tolower(label)))
            private$plan_context <- utils::modifyList(list(
                title = label,
                items = c(label, sprintf("store %s",
                    shift_display_path(shift_coalesce(context$store, "<store>")))),
                selection = NULL,
                output = NULL
            ), context)
            position <- match(task, private$stage_sequence)
            private$next_stage <- if (!is.na(position) &&
                position < length(private$stage_sequence)) {
                private$stage_sequence[[position + 1L]]
            } else {
                NULL
            }
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (!identical(private$mode_value, "none")) {
                private$emit("info", sprintf("%s run %s started.",
                    label, private$run_id_value))
                for (line in shift__ui_plan_lines(private$plan_context,
                    width = private$width())) {
                    private$emit("verbatim", line)
                }
            }
            private$persist(task, "running", private$stage_message,
                shift__progress_details(stage = task, phase = "operation",
                    unit_type = "shift_operation", outcome = "running",
                    stage_sequence = private$stage_sequence,
                    next_stage = private$next_stage))
            invisible(self)
        },

        # Commit an operation that produced its final delivery artifact while
        # preserving the dashboard receipt in terminal scrollback.
        operation_completed = function(summary, output_paths = character(),
                                       output_dir = NULL) {
            private$finish_operation("completed", summary,
                output_paths = output_paths, output_dir = output_dir)
            invisible(self)
        },

        # Commit one successful intermediate step without terminating its run.
        # The framebuffer closes at the R prompt; the returned stage carries
        # run/step identity into the next invocation.
        operation_waiting = function(summary, output_paths = character(),
                                     output_dir = NULL) {
            private$finish_operation("waiting", summary,
                output_paths = output_paths, output_dir = output_dir)
            invisible(self)
        },

        # Close the caller's framebuffer after handing work to an existing
        # detached subsystem while keeping the durable run in running state.
        operation_detached = function(summary, output_paths = character(),
                                      output_dir = NULL) {
            private$finish_operation("running", summary,
                output_paths = output_paths, output_dir = output_dir)
            invisible(self)
        },

        # Reuse the established failure receipt for generic operations so
        # Future EPW and standalone stages never print competing error panels.
        operation_failed = function(message, cancelled = FALSE,
                                    details = list()) {
            self$run_failed(message, cancelled = cancelled, details = details)
        },

        # Render the scientific plan summary before any remote operation and
        # include control commands when a process job has only been queued.
        run_started = function(plan, run_id, background = FALSE) {
            private$run_id_value <- run_id
            private$background <- isTRUE(background)
            private$status <- if (isTRUE(background)) "queued" else "running"
            private$task_label <- "Future EPW"
            private$cases_total <- nrow(plan@meta$expected_cases)
            private$stage_sequence <- shift__ui_stage_sequence(plan)
            private$plan_context <- shift__ui_plan_context(plan)
            if (!identical(private$mode_value, "none")) {
                # Foreground dynamic runs introduce the plan as a replaceable
                # first frame. Logs and queued jobs retain a permanent receipt.
                if (identical(private$mode_value, "dynamic") &&
                    !isTRUE(background)) {
                    private$stage <- if (length(private$stage_sequence)) {
                        private$stage_sequence[[1L]]
                    } else {
                        "planned"
                    }
                    private$next_stage <- if (length(private$stage_sequence) > 1L) {
                        private$stage_sequence[[2L]]
                    } else {
                        NULL
                    }
                    private$stage_message <- "Preparing resolver"
                    private$render_dynamic(force = TRUE)
                } else {
                    summary <- shift__ui_plan_summary(plan, run_id,
                        background = background, width = private$width(),
                        detail = private$ui_value@detail)
                    private$emit("info", summary[[1L]])
                    for (line in summary[-1L]) {
                        private$emit("verbatim", line)
                    }
                }
                if (isTRUE(background)) {
                    # Background control commands include the exact store path
                    # so they remain valid after the returned R handle is gone.
                    quoted_store <- encodeString(plan@store_path, quote = '"')
                    private$emit("text", sprintf(
                        "Watch   shift_watch(\"%s\", store = %s)", run_id, quoted_store))
                    private$emit("text", sprintf(
                        "Cancel  shift_cancel(\"%s\", store = %s)", run_id, quoted_store))
                    private$emit("text", sprintf(
                        "Logs    shift_logs(\"%s\", store = %s)", run_id, quoted_store))
                }
            }
            invisible(self)
        },

        # Start a durable workflow stage and close any dynamic unit left by the
        # preceding stage before emitting its new status.
        stage_started = function(stage, message, current = NULL, total = NULL, details = list()) {
            private$stage <- stage
            private$status <- "running"
            private$stage_message <- message
            private$stage_current <- current
            private$stage_total <- total
            private$stage_started_at <- Sys.time()
            private$current_details <- NULL
            stage_position <- match(stage, private$stage_sequence)
            private$next_stage <- if (length(stage_position) && !is.na(stage_position) &&
                stage_position < length(private$stage_sequence)) {
                private$stage_sequence[[stage_position + 1L]]
            } else {
                NULL
            }
            if (identical(private$mode_value, "log")) {
                private$emit("info", private$format_event(message,
                    current = current, total = total,
                    details = list(stage = stage, phase = "stage")))
            } else {
                private$render_dynamic(force = TRUE)
            }
            private$persist(stage, "running", message,
                utils::modifyList(shift__progress_details(
                    stage = stage, phase = "stage", current = current,
                    total = total, next_stage = private$next_stage,
                    stage_sequence = private$stage_sequence), details))
            invisible(self)
        },

        # Start a user-meaningful business unit such as a node, variable, or
        # scenario-period case and initialize dynamic progress when available.
        unit_started = function(message, current = NULL, total = NULL, details = list()) {
            private$unit_started_at <- Sys.time()
            private$current_details <- utils::modifyList(
                shift__progress_details(
                    stage = private$stage,
                    phase = "unit",
                    unit_label = message,
                    unit_base_label = message,
                    current = current,
                    total = total
                ),
                details
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else {
                private$emit("verbatim", private$format_event(message,
                    current = current, total = total,
                    details = private$current_details))
            }
            private$persist(private$stage, "running", message, private$current_details)
            invisible(self)
        },

        # Complete the current business unit with a structured outcome that can
        # later be reconstructed by watch clients.
        unit_completed = function(message, current = NULL, total = NULL,
                                  outcome = "completed", details = list()) {
            elapsed <- private$elapsed(private$unit_started_at)
            event_details <- utils::modifyList(
                shift_coalesce(private$current_details, shift__progress_details(stage = private$stage)),
                c(details, list(unit_label = message,
                    unit_base_label = message, current = current, total = total,
                    elapsed_seconds = elapsed, outcome = outcome))
            )
            private$current_details <- event_details
            private$last_event <- message
            private$add_recent(message, outcome)
            private$capture_business_result(message, event_details)
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit("warning", private$format_event(message,
                        current = current, total = total, details = event_details))
                }
                private$render_dynamic(force = TRUE)
            } else if (shift__ui_at_least(private$ui_value, "detail") ||
                outcome %in% c("failed", "fallback")) {
                event_type <- if (identical(outcome, "failed")) {
                    "warning"
                } else if (outcome %in% c("fallback", "rejected")) {
                    "verbatim"
                } else {
                    "success"
                }
                private$emit(event_type,
                    private$format_event(message, current = current, total = total,
                        details = event_details))
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },

        # Persist a meaningful change to the current business unit without
        # treating transient animation frames as durable workflow events.
        unit_updated = function(message, current = NULL, total = NULL, details = list()) {
            event_details <- utils::modifyList(
                shift_coalesce(private$current_details, shift__progress_details(stage = private$stage)),
                c(details, list(unit_label = message,
                    unit_base_label = message, current = current,
                    total = total, outcome = "updated"))
            )
            private$current_details <- event_details
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (shift__ui_at_least(private$ui_value, "detail")) {
                private$emit("verbatim", private$format_event(message,
                    current = current, total = total, details = event_details))
            }
            private$persist(private$stage, "updated", message, event_details)
            invisible(self)
        },

        # Record deterministic resume/reuse outcomes with a dedicated reporter
        # method so callers do not need to encode skipped semantics themselves.
        unit_skipped = function(message, current = NULL, total = NULL, details = list()) {
            self$unit_completed(message, current = current, total = total,
                outcome = "skipped", details = details)
        },

        # Record an operational milestone that is relevant to the current stage
        # but is not itself a countable business unit.
        notice = function(message, outcome = "info", details = list()) {
            event_details <- utils::modifyList(
                shift__progress_details(stage = private$stage, phase = "notice",
                    outcome = outcome),
                details
            )
            if (outcome %in% c("completed", "skipped", "rejected", "fallback", "failed", "cancelled")) {
                private$last_event <- message
                private$add_recent(message, outcome)
            }
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit("warning", private$format_event(message,
                        details = event_details))
                }
                private$render_dynamic(force = TRUE)
            } else if (!identical(private$mode_value, "none")) {
                private$emit(if (outcome %in% c("failed", "fallback")) "warning" else "verbatim",
                    private$format_event(message, details = event_details))
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },

        # Update the user-case snapshot after coverage or output transitions.
        # The same rows are later reconstructed from shift_run_case by watch.
        cases_updated = function(cases, show = FALSE) {
            private$case_rows <- data.table::as.data.table(data.table::copy(cases))
            private$cases_total <- nrow(private$case_rows)
            private$cases_ready <- sum(private$case_rows$status %in%
                c("ready", "morphing", "morphed", "completed"))
            private$outputs_completed <- sum(private$case_rows$status %in% "completed")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            if (isTRUE(show) && !identical(private$mode_value, "none") &&
                (!identical(private$mode_value, "dynamic") ||
                    shift__ui_at_least(private$ui_value, "detail"))) {
                private$render_case_table()
            }
            invisible(self)
        },

        # Check cooperative cancellation at explicit workflow boundaries even
        # when no heartbeat or progress output is currently being rendered.
        check_cancel = function(stage = private$stage) {
            if (!is.null(private$store) && !is.null(private$run_id_value) && !is.null(private$job_id_value)) {
                shift__job_check_cancel(private$store, private$run_id_value, private$job_id_value, stage)
            }
            invisible(FALSE)
        },

        # Close the dynamic unit and persist the terminal milestone for the
        # current stage together with its elapsed time.
        stage_completed = function(message, details = list()) {
            elapsed <- private$elapsed(private$stage_started_at)
            private$last_event <- message
            private$completed_stages <- unique(c(private$completed_stages,
                private$stage))
            private$add_recent(message, "completed")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
                # The live Recent section already retains this milestone.
                # Only explicit detail mode adds a scrolling resolver table.
                if (identical(private$stage, "resolve") &&
                    shift__ui_at_least(private$ui_value, "detail")) {
                    private$render_node_table()
                }
            } else if (!identical(private$mode_value, "none")) {
                private$emit("success", private$format_event(message,
                    details = list(stage = private$stage, phase = "stage")))
                if (identical(private$stage, "resolve")) {
                    private$render_node_table()
                }
            }
            private$persist(private$stage, "completed", message,
                utils::modifyList(shift__progress_details(
                    stage = private$stage,
                    phase = "stage",
                    elapsed_seconds = elapsed,
                    outcome = "completed"
                ), details))
            invisible(self)
        },

        # Refresh transient liveness and cancellation state without persisting
        # animation-only heartbeat events in the run history.
        heartbeat = function(message = NULL, details = list(), force = FALSE) {
            now <- Sys.time()
            # Keep a stable base label separate from the transient elapsed
            # suffix so repeated heartbeats never grow the displayed message.
            private$current_details <- utils::modifyList(
                shift_coalesce(private$current_details,
                    shift__progress_details(stage = private$stage, phase = "unit")),
                details
            )
            label <- shift_coalesce(message, shift_coalesce(
                private$current_details$unit_base_label,
                shift_coalesce(private$current_details$unit_label, "Working")))
            private$current_details$unit_base_label <- shift_coalesce(
                private$current_details$unit_base_label, label)
            elapsed <- private$elapsed(private$unit_started_at)
            private$current_details$unit_label <- label
            private$current_details$elapsed_seconds <- elapsed
            due_liveness <- isTRUE(force) || is.na(private$last_heartbeat) ||
                as.numeric(difftime(now, private$last_heartbeat, units = "secs")) >=
                    max(1, private$ui_value@heartbeat)
            if (isTRUE(due_liveness)) {
                private$last_heartbeat <- now
                # Cancellation and durable heartbeat checks follow the slower
                # liveness cadence, not the animation frame rate.
                if (!is.null(private$store) && !is.null(private$run_id_value) &&
                    !is.null(private$job_id_value)) {
                    shift__job_check_cancel(private$store, private$run_id_value,
                        private$job_id_value, shift_coalesce(private$stage, "working"))
                }
                private$touch_job(force = TRUE)
            }
            if (identical(private$mode_value, "none")) {
                return(invisible(due_liveness))
            }
            status <- sprintf("%s (%s elapsed)", label, shift__format_elapsed(elapsed))
            if (identical(private$mode_value, "dynamic")) {
                refreshed <- private$render_dynamic(force = force)
                return(invisible(isTRUE(refreshed) || isTRUE(due_liveness)))
            } else if (isTRUE(due_liveness)) {
                private$emit("verbatim", private$format_event(status,
                    details = private$current_details))
            }
            invisible(due_liveness)
        },

        # Render one terminal completion receipt from the refreshed run state.
        # Frame terminals commit it to scrollback; compact/log renderers retain
        # the append-only text summary that remains suitable for redirection.
        run_completed = function(run, outputs = data.table::data.table()) {
            elapsed <- private$elapsed(private$started_at)
            status <- shift_status(run, refresh = FALSE)
            private$status <- status
            if (identical(status, "completed")) {
                private$completed_stages <- private$stage_sequence
            }
            paths <- shift_coalesce(outputs$export_path, outputs$path)
            paths <- as.character(paths[!is.na(paths) & nzchar(paths)])
            output_dir <- if (nrow(run@meta$run) &&
                "output_dir" %in% names(run@meta$run)) {
                run@meta$run$output_dir[[1L]]
            } else {
                NULL
            }
            if ((is.null(output_dir) || !length(output_dir) ||
                is.na(output_dir[[1L]]) || !nzchar(output_dir[[1L]])) &&
                length(paths)) {
                output_dir <- dirname(paths[[1L]])
            }
            private$output_paths <- paths
            private$output_dir <- output_dir
            private$output_path_limit <- if (
                shift__ui_at_least(private$ui_value, "detail")) Inf else 5L
            private$add_recent(sprintf("%d EPW output(s) ready", nrow(outputs)), status)
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            renderer_backend <- if (is.null(private$renderer)) NULL else
                tryCatch(private$renderer$backend(), error = function(e) NULL)
            committed_frame <- identical(private$mode_value, "dynamic") &&
                identical(renderer_backend, "frame")
            private$close_renderer(result = "done", preserve = TRUE)
            if (!isTRUE(committed_frame)) {
                private$emit("success", sprintf(
                    "Future EPW run %s %s: %d output(s) in %s.",
                    private$run_id_value, status, nrow(outputs),
                    shift__format_elapsed(elapsed)))
            }
            if (!isTRUE(committed_frame) &&
                !identical(private$mode_value, "none") && nrow(outputs)) {
                if (!is.null(output_dir) && length(output_dir) &&
                    !is.na(output_dir[[1L]]) && nzchar(output_dir[[1L]])) {
                    private$emit("text", sprintf("Output directory: %s",
                        shift_display_path(output_dir[[1L]])))
                }
                if (shift__ui_at_least(private$ui_value, "detail")) {
                    for (path in paths) {
                        private$emit("path", path)
                    }
                }
            }
            invisible(self)
        },

        # Close transient UI resources before showing a terminal failure or
        # cancellation message.
        run_failed = function(message = NULL, cancelled = FALSE,
                              details = list()) {
            private$status <- if (isTRUE(cancelled)) "cancelled" else "failed"
            private$failure_details <- shift_coalesce(details, list())
            terminal_message <- shift_coalesce(message,
                shift_coalesce(private$failure_details$summary,
                    if (isTRUE(cancelled)) "Workflow cancelled" else
                        "Workflow failed"))
            if (!is.null(terminal_message)) {
                private$last_event <- terminal_message
                private$add_recent(terminal_message, private$status)
                private$current_details <- utils::modifyList(
                    shift_coalesce(private$current_details, list()),
                    list(
                        unit_label = terminal_message,
                        unit_base_label = terminal_message,
                        outcome = private$status
                    )
                )
            }
            was_dynamic <- identical(private$mode_value, "dynamic")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            private$close_renderer(result = if (isTRUE(cancelled)) {
                "cancelled"
            } else {
                "failed"
            }, preserve = TRUE)
            # The caller raises the one primary cli condition. Reporter output
            # here is deliberately limited to structured context tables so a
            # failure is never printed once by the reporter and again by rlang.
            if (!isTRUE(cancelled)) {
                # The committed dashboard owns normal dynamic diagnostics.
                # Logs and explicit detail modes retain complete tables.
                if (!isTRUE(was_dynamic) ||
                    shift__ui_at_least(private$ui_value, "detail")) {
                    private$render_node_table(force = TRUE)
                    private$render_case_table(force = TRUE, detail = "detail")
                }
            } else if (!is.null(terminal_message) && !isTRUE(was_dynamic)) {
                private$emit("warning", terminal_message)
            }
            invisible(self)
        },

        # Keep cancellation rendering distinct at call sites while sharing the
        # same cleanup and warning behavior as other terminal failures.
        run_cancelled = function(message) {
            self$run_failed(message, cancelled = TRUE)
        },

        # Emit low-level paths, URLs, and reuse details only when explicitly
        # requested by the caller.
        detail = function(message, level = c("detail", "debug")) {
            level <- match.arg(level)
            if (shift__ui_at_least(private$ui_value, level)) {
                private$emit("text", message)
            }
            invisible(self)
        },

        # Expose immutable reporter context to workflow adapters without
        # leaking its mutable private state.
        mode = function() private$mode_value,
        # Return the validated UI options used to create this reporter.
        ui = function() private$ui_value,
        # Return the durable run identity associated with persisted events.
        run_id = function() private$run_id_value,
        # Return the current execution-attempt identity used for heartbeats.
        job_id = function() private$job_id_value,
        # Return the persisted step currently owning reporter events.
        step_id = function() private$step_id_value,
        # Return the current business context for terminal diagnostics without
        # exposing the reporter's mutable private environment.
        context = function() shift_coalesce(private$current_details, list()),
        # Return the semantic view state for unit tests and alternate renderers.
        snapshot = function() private$view_state(),

        # Explicitly release the live terminal renderer when a caller exits
        # through an unusual but non-error path.
        close = function() {
            private$close_renderer(result = "done")
            invisible(self)
        }
    ),
    private = list(
        ui_value = NULL,
        mode_value = NULL,
        motion_value = NULL,
        store = NULL,
        run_id_value = NULL,
        job_id_value = NULL,
        step_id_value = NULL,
        background = FALSE,
        status = NULL,
        stage = NULL,
        renderer = NULL,
        started_at = NULL,
        stage_started_at = NULL,
        unit_started_at = NULL,
        last_heartbeat = NULL,
        last_liveness = NULL,
        last_refresh = NULL,
        animation_frame = 0L,
        current_details = NULL,
        stage_message = NULL,
        stage_current = NULL,
        stage_total = NULL,
        stage_sequence = character(),
        completed_stages = character(),
        next_stage = NULL,
        last_event = NULL,
        recent_events = character(),
        recent_outcomes = character(),
        cases_ready = 0L,
        cases_total = 0L,
        outputs_completed = 0L,
        node_rows = NULL,
        case_rows = NULL,
        plan_context = NULL,
        failure_details = list(),
        output_dir = NULL,
        output_paths = character(),
        output_path_limit = 5L,
        task_label = "Future EPW",
        result_summary = NULL,

        # Map reporter message kinds onto cli output while temporarily
        # releasing an active framebuffer. Console rendering failures are
        # contained because presentation must never abort scientific work.
        emit = function(type, message) {
            if (identical(private$mode_value, "none")) {
                return(invisible(NULL))
            }
            emit_one <- function() {
                tryCatch(
                    switch(type,
                        success = cli::cli_alert_success("{message}"),
                        warning = cli::cli_alert_warning("{message}"),
                        danger = cli::cli_alert_danger("{message}"),
                        info = cli::cli_alert_info("{message}"),
                        verbatim = cli::cli_verbatim(message),
                        path = cli::cli_text("  {.path {message}}"),
                        cli::cli_text("{message}")
                    ),
                    error = function(e) invisible(NULL)
                )
            }
            private$with_output(emit_one)
            invisible(NULL)
        },

        # Execute a related group of cli emissions under one framebuffer
        # clear/restore cycle so multi-line tables do not flicker row by row.
        with_output = function(code) {
            if (is.null(private$renderer)) {
                return(code())
            }
            private$renderer$suspend(code)
        },

        # Persist one structured milestone and update job liveness as one
        # reporter-side operation.
        persist = function(stage, status, message, details) {
            if (is.null(private$store) || is.null(private$run_id_value)) {
                return(invisible(NULL))
            }
            # Job heartbeat persistence immediately snapshots the same event;
            # suppress the first snapshot to avoid two full live JSON rewrites
            # for every reporter milestone.
            shift__run_event(private$store, private$run_id_value, stage, status,
                message, details, snapshot = FALSE,
                step_id = private$step_id_value)
            private$touch_job(force = TRUE)
            invisible(NULL)
        },

        # Best-effort heartbeat updates must never replace the workflow error
        # that triggered reporter cleanup.
        touch_job = function(force = FALSE) {
            now <- Sys.time()
            due <- isTRUE(force) || is.na(private$last_liveness) ||
                as.numeric(difftime(now, private$last_liveness, units = "secs")) >=
                    max(1, private$ui_value@heartbeat)
            if (!isTRUE(due)) {
                return(invisible(FALSE))
            }
            private$last_liveness <- now
            if (!is.null(private$store) && !is.null(private$job_id_value) &&
                exists("shift__job_touch", mode = "function")) {
                try(shift__job_touch(private$store, private$job_id_value,
                    ui_state = private$view_state()), silent = TRUE)
            }
            invisible(TRUE)
        },

        # Release the active framebuffer exactly once. Terminal workflow
        # outcomes commit their final semantic frame; routine cleanup clears
        # transient output.
        close_renderer = function(result = "done", preserve = FALSE) {
            if (!is.null(private$renderer)) {
                if (isTRUE(preserve) &&
                    is.function(private$renderer$commit)) {
                    private$renderer$commit(result = result)
                } else {
                    private$renderer$close(result = result)
                }
                private$renderer <- NULL
            }
            invisible(NULL)
        },

        # Normalize missing timestamps to zero so summaries remain renderable
        # during early launch failures.
        elapsed = function(start) {
            if (is.null(start) || length(start) == 0L || is.na(start)) {
                return(0)
            }
            as.numeric(difftime(Sys.time(), start, units = "secs"))
        },

        # Resolve the output width at render time so tests, IDE resizing, and
        # redirected 80-column logs all share the same clipping behavior.
        width = function() shift__ui_width(),

        # Keep only user-meaningful terminal milestones in the fixed activity
        # feed. Animation ticks and routine updates never enter this buffer.
        add_recent = function(message, outcome) {
            private$recent_events <- utils::tail(
                c(private$recent_events, message), 3L)
            private$recent_outcomes <- utils::tail(
                c(private$recent_outcomes, outcome), 3L)
            invisible(message)
        },

        # Assemble the semantic state consumed by the shared status formatter.
        view_state = function() {
            details <- shift_coalesce(private$current_details, list())
            list(
                run_id = private$run_id_value,
                task_label = private$task_label,
                status = private$status,
                stage = private$stage,
                stage_message = private$stage_message,
                stage_current = private$stage_current,
                stage_total = private$stage_total,
                unit_label = details$unit_label,
                unit_current = details$current,
                unit_total = details$total,
                current_details = details,
                next_stage = private$next_stage,
                stage_sequence = private$stage_sequence,
                completed_stages = private$completed_stages,
                cases_ready = private$cases_ready,
                cases_total = private$cases_total,
                outputs_completed = private$outputs_completed,
                last_event = private$last_event,
                recent_events = private$recent_events,
                recent_outcomes = private$recent_outcomes,
                node_rows = private$node_rows,
                plan_context = private$plan_context,
                failure_details = private$failure_details,
                output_dir = private$output_dir,
                output_paths = private$output_paths,
                output_path_limit = private$output_path_limit,
                result_summary = private$result_summary,
                elapsed_seconds = private$elapsed(private$started_at)
            )
        },

        # Finalize a generic operation in one place so completed and waiting
        # receipts share identical rendering and persistence semantics.
        finish_operation = function(status, summary, output_paths = character(),
                                    output_dir = NULL) {
            checkmate::assert_choice(status, c("completed", "waiting", "running"))
            checkmate::assert_string(summary, min.chars = 1L)
            private$status <- status
            private$result_summary <- summary
            private$last_event <- summary
            private$completed_stages <- unique(c(private$completed_stages,
                private$stage))
            private$output_paths <- as.character(output_paths)
            private$output_dir <- output_dir
            private$add_recent(summary, status)
            private$current_details <- utils::modifyList(
                shift_coalesce(private$current_details, list()),
                list(unit_label = summary, unit_base_label = summary,
                    outcome = status)
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            renderer_backend <- if (is.null(private$renderer)) NULL else
                tryCatch(private$renderer$backend(), error = function(e) NULL)
            committed_frame <- identical(private$mode_value, "dynamic") &&
                identical(renderer_backend, "frame")
            private$close_renderer(result = status, preserve = TRUE)
            if (!isTRUE(committed_frame) &&
                !identical(private$mode_value, "none")) {
                private$emit(if (identical(status, "completed")) "success" else "info",
                    sprintf("%s run %s %s: %s",
                        private$task_label, private$run_id_value, status,
                        summary))
            }
            invisible(NULL)
        },

        # Refresh the complete dashboard as one atomic frame on its own visual
        # cadence; compact terminals receive the matching one-line summary.
        render_dynamic = function(force = TRUE) {
            if (!identical(private$mode_value, "dynamic")) {
                return(invisible(FALSE))
            }
            now <- Sys.time()
            due <- isTRUE(force) || is.na(private$last_refresh) ||
                as.numeric(difftime(now, private$last_refresh, units = "secs")) >=
                    private$ui_value@refresh
            if (!isTRUE(due)) {
                return(invisible(FALSE))
            }
            private$last_refresh <- now
            private$animation_frame <- private$animation_frame + 1L
            state <- private$view_state()
            lines <- shift__ui_status_lines(state,
                width = private$width(), motion = private$motion_value,
                frame = private$animation_frame)
            compact <- shift__ui_compact_line(state,
                width = private$width(), motion = private$motion_value,
                frame = private$animation_frame)
            refreshed <- !is.null(private$renderer) &&
                isTRUE(private$renderer$draw(lines, compact = compact))
            if (!isTRUE(refreshed)) {
                private$fallback_to_log(lines)
                return(invisible(FALSE))
            }
            invisible(TRUE)
        },

        # Degrade a broken dynamic renderer exactly once to durable line logs.
        # Presentation failures must remain visible without aborting or hiding
        # the scientific workflow that is still running underneath them.
        fallback_to_log = function(lines) {
            private$close_renderer(result = "failed")
            private$mode_value <- "log"
            private$motion_value <- "none"
            private$emit("warning",
                "Dynamic progress is unavailable; switched to line-by-line logs.")
            for (line in lines) {
                private$emit("verbatim", line)
            }
            private$persist(
                shift_coalesce(private$stage, "ui"),
                "warning",
                "Dynamic progress was unavailable; switched to line-by-line logs.",
                shift__progress_details(
                    stage = shift_coalesce(private$stage, "ui"),
                    phase = "notice",
                    unit_type = "ui",
                    outcome = "fallback"
                )
            )
            invisible(NULL)
        },

        # Prefix append-only log events with stable workflow context. Full URLs
        # are restricted to debug mode while normal logs use short node names.
        format_event = function(message, current = NULL, total = NULL,
                                details = list()) {
            stage <- shift__ui_stage_label(shift_coalesce(details$stage, private$stage))
            context <- character()
            node <- details$node
            if (!is.null(node) && length(node) && !is.na(node[[1L]])) {
                node <- as.character(node[[1L]])
                if (!shift__ui_at_least(private$ui_value, "debug")) {
                    node <- shift__node_label(node)
                }
                context <- c(context, node)
            }
            phase <- details$catalog_role
            if (is.null(phase) && !identical(details$phase, "stage") &&
                !identical(details$phase, "unit") && !identical(details$phase, "notice")) {
                phase <- details$phase
            }
            if (!is.null(phase) && length(phase) && !is.na(phase[[1L]])) {
                context <- c(context, as.character(phase[[1L]]))
            }
            prefix <- paste0("[", paste(c(stage, context), collapse = "]["), "]")
            counter <- if (!is.null(current) && !is.null(total)) {
                sprintf(" %d/%d", as.integer(current), as.integer(total))
            } else {
                ""
            }
            # Append-only logs must retain the complete message. Width-bounded
            # trimming is reserved for dynamic rows and compact tables.
            sprintf("%s%s %s", prefix, counter, message)
        },

        # Capture node, case, and output outcomes while keeping their event
        # persistence independent from terminal rendering.
        capture_business_result = function(message, details) {
            if (identical(details$unit_type, "index_node")) {
                row <- data.table::data.table(
                    node = shift__node_label(details$node),
                    future = shift_coalesce(details$future_files, NA_integer_),
                    reference = shift_coalesce(details$reference_files, NA_integer_),
                    outcome = as.character(shift_coalesce(details$outcome,
                        "rejected")),
                    duration = shift__format_elapsed(
                        shift_coalesce(details$elapsed_seconds, 0)),
                    result = if (details$outcome %in% c("completed", "skipped")) {
                        shift_coalesce(details$result, "selected")
                    } else {
                        error <- shift_coalesce(details$error, message)
                        kind <- shift_coalesce(details$error_kind,
                            shift__ui_error_kind(error))
                        sprintf("%s: %s", kind, error)
                    }
                )
                private$node_rows <- data.table::rbindlist(
                    list(private$node_rows, row), use.names = TRUE, fill = TRUE)
            }
            if (identical(details$unit_type, "epw_export") &&
                details$outcome %in% c("completed", "skipped")) {
                private$outputs_completed <- max(private$outputs_completed,
                    as.integer(shift_coalesce(details$current, 0L)))
            }
            invisible(NULL)
        },

        # Print a compact resolver-attempt table after resolve or immediately
        # before a resolve failure; result text receives the remaining width.
        render_node_table = function(force = FALSE) {
            rows <- private$node_rows
            if (is.null(rows) || !nrow(rows)) {
                return(invisible(NULL))
            }
            private$with_output(function() {
                for (line in shift__ui_node_table(rows, width = private$width(),
                    detail = private$ui_value@detail)) {
                    private$emit("verbatim", line)
                }
            })
            invisible(NULL)
        },

        # Print the user-level case matrix rather than exposing extraction-plan
        # rows as the main progress model.
        render_case_table = function(force = FALSE,
                                     detail = private$ui_value@detail) {
            rows <- private$case_rows
            if (is.null(rows) || !nrow(rows) ||
                (!isTRUE(force) && !shift__ui_at_least(private$ui_value, "normal"))) {
                return(invisible(NULL))
            }
            private$with_output(function() {
                for (line in shift__ui_case_table(rows, width = private$width(),
                    detail = detail)) {
                    private$emit("verbatim", line)
                }
            })
            invisible(NULL)
        }
    )
)

# Construct a reporter after a run and optional job have durable identities.
shift__reporter <- function(ui = shift_ui(), store = NULL, run_id = NULL,
                            job_id = NULL, background = FALSE,
                            step_id = NULL) {
    ShiftReporter$new(
        ui = ui,
        store = store,
        run_id = run_id,
        job_id = job_id,
        background = background,
        step_id = step_id
    )
}

# Format workflow durations without pretending that remote work has a reliable
# ETA while it is still running.
shift__format_elapsed <- function(seconds) {
    seconds <- max(0, round(as.numeric(seconds)))
    hours <- seconds %/% 3600L
    minutes <- (seconds %% 3600L) %/% 60L
    secs <- seconds %% 60L
    if (hours > 0L) {
        return(sprintf("%dh %02dm %02ds", hours, minutes, secs))
    }
    if (minutes > 0L) {
        return(sprintf("%dm %02ds", minutes, secs))
    }
    sprintf("%ds", secs)
}
