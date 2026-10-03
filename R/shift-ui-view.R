#' @include shift-ui.R
NULL

# Format workflow state as bounded console text, tables and dashboard panels.

# Render runtime presentation policy separately from scientific workflow intent
# so users can inspect TUI behaviour without seeing a raw S7 property dump.
shift__print_ui_options <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("Shift UI")
    shift__print_facts(list(
        "Progress" = x@progress,
        "Detail" = x@detail,
        "Motion" = x@motion,
        "Refresh" = sprintf("%.2f s", x@refresh),
        "Heartbeat" = sprintf("%g s", x@heartbeat)
    ))
    invisible(x)
}

# ShiftUiOptions participates in the same width/verbose print contract as all
# other public Shift configuration objects.
S7::method(print, ShiftUiOptions) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_ui_options(x, width = opts$width, verbose = opts$verbose)
}

# Query POSIX TTY dimensions at most twice a second. Redirected output never
# opens /dev/tty, and other hosts retain the explicit option / LINES fallback.
shift__ui_terminal_height <- local({
    checked <- as.POSIXct(NA)
    value <- NULL
    function() {
        if (
            .Platform$OS.type == "windows" ||
                !file.exists("/bin/stty") ||
                !isTRUE(isatty(cli::cli_output_connection()))
        ) {
            return(NULL)
        }
        now <- Sys.time()
        if (
            !is.na(checked) &&
                as.numeric(difftime(now, checked, units = "secs")) < 0.5
        ) {
            return(value)
        }
        checked <<- now
        size <- tryCatch(
            suppressWarnings(system2(
                "/bin/stty",
                "size",
                stdin = "/dev/tty",
                stdout = TRUE,
                stderr = FALSE
            )),
            error = function(error) character()
        )
        rows <- if (length(size)) {
            suppressWarnings(as.integer(
                strsplit(trimws(size[[1L]]), "[[:space:]]+")[[1L]][1L]
            ))
        } else {
            NA_integer_
        }
        value <<- if (!is.na(rows) && rows >= 2L) rows else NULL
        value
    }
})

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
        lines <- c(
            lines,
            shift__ui_fit(
                cli::ansi_substr(remaining, 1L, best),
                width
            )
        )
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
    unlist(
        lapply(wrapped, shift__ui_hard_wrap, width = width),
        use.names = FALSE
    )
}

# Wrap prose after a fixed semantic prefix and align every continuation row
# beneath its value. Long unbreakable tokens use the lossless hard-wrap helper,
# so paths and identifiers remain fully available in diagnostic frames.
shift__ui_prefixed_lines <- function(
    prefix,
    value,
    width,
    continuation = NULL
) {
    width <- max(1L, shift__ui_width(width))
    prefix <- as.character(shift_coalesce(prefix, ""))[[1L]]
    value <- gsub("[\r\n]+", " ", as.character(shift_coalesce(value, ""))[[1L]])
    prefix_width <- cli::ansi_nchar(prefix, type = "width")
    if (prefix_width >= width) {
        return(c(
            shift__ui_fit(prefix, width),
            shift__ui_wrap_lines(value, width)
        ))
    }
    value_width <- max(1L, width - prefix_width)
    wrapped <- shift__ui_wrap_lines(value, value_width)
    continuation <- shift_coalesce(
        continuation,
        strrep(" ", min(prefix_width, width))
    )
    lines <- paste0(
        c(prefix, rep(continuation, max(0L, length(wrapped) - 1L))),
        wrapped
    )
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Render a title-like dashboard field as one row when possible and as aligned
# continuation rows when its value grows. The fixed label remains the visual
# anchor in colour terminals and the plain-text anchor under NO_COLOR.
shift__ui_labeled_lines <- function(label, value, width) {
    first_prefix <- shift__ui_labeled_line(label, "")
    continuation <- strrep(" ", cli::ansi_nchar(first_prefix, type = "width"))
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
            plan_context$line,
            "Workflow context unavailable"
        ))[[1L]]
        items <- strsplit(line, " \u00b7 ", fixed = TRUE)[[1L]]
    }
    # Extremely narrow terminals receive the heading on its own row so the
    # fixed label does not consume every column and hide the plan values.
    if (width <= label_width) {
        return(c(
            shift__ui_fit(shift__ui_labeled_line("Plan", ""), width),
            unlist(
                lapply(items, shift__ui_wrap_lines, width = width),
                use.names = FALSE
            )
        ))
    }
    value_width <- width - label_width
    values <- shift__ui_pack_items(items, value_width)
    vapply(
        seq_along(values),
        function(i) {
            shift__ui_labeled_line(if (i == 1L) "Plan" else "", values[[i]])
        },
        character(1L)
    )
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
    paste(
        vapply(
            labels,
            function(label) {
                years <- periods[period == label]$year
                if (length(unique(years)) == 1L) {
                    sprintf("%s (%d)", label, years[[1L]])
                } else {
                    sprintf("%s (%d\u2013%d)", label, min(years), max(years))
                }
            },
            character(1L)
        ),
        collapse = ", "
    )
}

# Describe the reference input without exposing matching fields or plan IDs in
# the normal startup view; those remain available through shift_explain().
shift__ui_reference <- function(reference) {
    if (is.null(reference)) {
        return("no reference")
    }
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        return(sprintf(
            "%s %s %d\u2013%d (%s)",
            toupper(reference@dataset),
            reference@product,
            min(reference@years),
            max(reference@years),
            reference@access
        ))
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
        return(sprintf("%s %s %s", reference@role, reference@mode, periods))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied climate reference")
    }
    "reference supplied"
}

# Format unresolved or explicit CMIP6 selections for the startup summary. The
# table strategy is part of the scientific selection because enhanced monthly
# recipes may resolve Amon and LImon on different grids.
shift__ui_selection <- function(plan) {
    climate <- plan@meta$climate
    request <- plan@meta$request@meta
    member_value <- if (is.null(climate)) request$variant else climate@member
    grid_value <- if (is.null(climate)) {
        request$filters$grid_label
    } else {
        climate@grid
    }
    table_value <- if (is.null(climate)) {
        request$filters$table_id
    } else {
        climate@table
    }
    member <- if (is.null(member_value)) {
        "member auto"
    } else {
        sprintf("member %s", paste(member_value, collapse = ", "))
    }
    grid <- if (is.null(grid_value)) {
        "grid auto"
    } else {
        sprintf("grid %s", paste(grid_value, collapse = ", "))
    }
    tables <- sprintf("tables %s", shift__format_cmip6_tables(table_value))
    paste(member, grid, tables, sep = " \u00b7 ")
}

# Build the compact startup summary shown before any network request. Normal
# output confirms the delivery directory and pending CMIP selections; detail
# output additionally exposes the run policy and internal store.
shift__ui_plan_summary <- function(
    plan,
    run_id,
    background = FALSE,
    width = shift__ui_width(),
    detail = "normal"
) {
    request <- plan@meta$request@meta
    model <- shift_coalesce(shift__display_values(request$source), "<model>")
    scenarios <- shift_coalesce(
        shift__display_values(request$experiment),
        "<scenario>"
    )
    status <- if (isTRUE(background)) "QUEUED" else "STARTING"
    output_dir <- shift_coalesce(plan@meta$epw$export_dir, "<output directory>")
    transform_label <- plan@meta$transform@label
    lines <- c(
        shift__ui_fit(
            sprintf("Future EPW \u00b7 %s \u00b7 %s", run_id, status),
            width
        ),
        shift__ui_fit(
            sprintf(
                "%s \u00b7 %s \u00b7 %s",
                model,
                scenarios,
                shift__ui_periods(plan@meta$periods)
            ),
            width
        ),
        shift__ui_fit(
            sprintf(
                "%s \u00b7 %s \u00b7 %d expected output(s)",
                transform_label,
                shift__ui_reference(plan@meta$reference),
                nrow(plan@meta$expected_cases)
            ),
            width
        ),
        shift__ui_prefixed_lines(
            "Selection ",
            shift__ui_selection(plan),
            width
        ),
        shift__ui_fit(
            sprintf("Output %s", shift__display_path(output_dir)),
            width
        )
    )
    if (!is.null(plan@meta$observed_reference)) {
        lines <- c(
            lines,
            shift__ui_labeled_lines(
                "Observed",
                shift__ui_reference(plan@meta$observed_reference),
                width
            )
        )
    }
    if (identical(plan@meta$transform@output_type, "multi_year")) {
        lines[[3L]] <- shift__ui_fit(
            sprintf(
                "%s \u00b7 %d case(s); one EPW per weather year",
                transform_label,
                nrow(plan@meta$expected_cases)
            ),
            width
        )
    }
    if (!identical(detail, "normal")) {
        option_summary <- shift__format_options(
            unclass(plan@meta$transform@options)
        )
        lines <- c(
            lines,
            if (!is.null(option_summary)) {
                shift__ui_labeled_lines(
                    "Options",
                    option_summary,
                    width
                )
            },
            shift__ui_fit(
                sprintf(
                    "Policy %s \u00b7 store %s",
                    if (isTRUE(plan@meta$control@allow_partial)) {
                        "partial cases allowed"
                    } else {
                        "all cases required"
                    },
                    shift__display_path(plan@store_path)
                ),
                width
            )
        )
    }
    lines
}

# Map internal stage identifiers onto short labels that remain readable in the
# fixed status region and in redirected logs.
shift__ui_stage_label <- function(stage) {
    labels <- c(
        planned = "Plan",
        datasets = "Datasets",
        collect = "Collect",
        resolve = "Resolve",
        download = "Download",
        extract = "Extract",
        extract_future = "Extract future",
        extract_reference = "Extract reference",
        coverage = "Coverage",
        morph = "Morph",
        write_epw = "Write EPW",
        export_epw = "Export EPW",
        completed = "Completed",
        resume = "Resume",
        reanalysis = "Reanalysis",
        extract_observed_reference = "Observed reference",
        batch = "Batch"
    )
    key <- as.character(shift_coalesce(stage, "planned"))[[1L]]
    # Named atomic vectors throw on an unknown `[[` key, so extension stages
    # must be checked before lookup and then rendered through the generic label.
    if (key %in% names(labels)) {
        return(unname(labels[[key]]))
    }
    gsub("_", " ", key, fixed = TRUE)
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
    if (
        is.null(seconds) ||
            !length(seconds) ||
            is.na(seconds) ||
            !is.finite(seconds)
    ) {
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
    label <- paste0(
        label,
        strrep(" ", max(1L, 9L - cli::ansi_nchar(label, type = "width")))
    )
    label <- switch(
        shift__ui_label_role(trimws(label)),
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
    value <- cli::ansi_align(
        shift__ui_fit(value, inner_width),
        inner_width,
        align = "left"
    )
    paste0(cli::style_dim("\u2502 "), value, cli::style_dim(" \u2502"))
}

# Draw top, middle, and bottom panel rules with display-width-aware labels.
# This remains a pure formatter so the framebuffer still owns all cursor work.
shift__ui_panel_rule <- function(
    label = NULL,
    width,
    kind = c("top", "middle", "bottom")
) {
    kind <- match.arg(kind)
    width <- shift__ui_width(width)
    glyphs <- switch(
        kind,
        top = c("\u256d", "\u256e"),
        middle = c("\u251c", "\u2524"),
        bottom = c("\u2570", "\u256f")
    )
    inner_width <- max(1L, width - 2L)
    if (is.null(label) || !length(label) || !nzchar(cli::ansi_strip(label))) {
        return(cli::style_dim(paste0(
            glyphs[[1L]],
            strrep("\u2500", inner_width),
            glyphs[[2L]]
        )))
    }
    label <- shift__ui_fit(label, max(1L, inner_width - 3L))
    used <- cli::ansi_nchar(label, type = "width") + 3L
    paste0(
        cli::style_dim(paste0(glyphs[[1L]], "\u2500 ")),
        label,
        cli::style_dim(paste0(
            " ",
            strrep("\u2500", max(0L, inner_width - used)),
            glyphs[[2L]]
        ))
    )
}

# Apply colour only to semantic state. Ordinary configuration values remain in
# the terminal's default foreground colour instead of becoming a wall of green.
shift__ui_status_style <- function(status, stage = NULL) {
    state <- tolower(as.character(shift_coalesce(status, "running"))[[1L]])
    # `waiting` is the durable state-machine term. The user-facing label names
    # the finished intermediate artifact without implying a paused command.
    label <- if (identical(state, "waiting")) {
        if (isTRUE(stage %in% c("collect", "datasets"))) {
            "CATALOG READY"
        } else {
            "STEP COMPLETE"
        }
    } else {
        toupper(state)
    }
    styled <- switch(
        state,
        completed = cli::col_green(label),
        partial = cli::col_yellow(label),
        failed = cli::col_red(label),
        cancelled = cli::col_red(label),
        stopping = cli::col_yellow(label),
        waiting = cli::col_blue(label),
        queued = cli::col_blue(label),
        cli::col_cyan(label)
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
    bar_width <- if (width >= 100L) {
        22L
    } else if (width >= 72L) {
        14L
    } else {
        8L
    }
    percent <- as.integer(round(max(0, min(1, current / total)) * 100))
    sprintf(
        "%s  %d/%d \u00b7 %d%%",
        shift__ui_bar(current, total, bar_width),
        as.integer(current),
        as.integer(total),
        percent
    )
}

# Build the stage-progress field. Each stage exposes its own measurable unit;
# long transfer or selection metrics continue on aligned rows, while resolver
# work remains indeterminate instead of presenting a misleading percentage.
shift__ui_metric_line <- function(state, width = shift__ui_width()) {
    width <- shift__ui_width(width)
    details <- shift_coalesce(state$current_details, list())
    stage <- as.character(shift_coalesce(state$stage, "planned"))[[1L]]
    current <- shift__ui_metric_number(
        details,
        "current",
        shift_coalesce(state$unit_current, NA_real_)
    )
    total <- shift__ui_metric_number(
        details,
        "total",
        shift_coalesce(state$unit_total, NA_real_)
    )
    ordinal <- current
    # A started unit is in progress, not completed. Older snapshots without a
    # phase retain their historical counter semantics.
    if (
        identical(details$phase, "unit") &&
            is.null(details$outcome) &&
            !is.na(current)
    ) {
        current <- max(0L, current - 1L)
    }
    elapsed <- shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    plan_context <- shift_coalesce(state$plan_context, list())

    if (identical(details$unit_type, "catalog")) {
        return(shift__ui_query_lines(state, width))
    }

    if (identical(details$unit_type, "epw_summary")) {
        return(shift__ui_labeled_lines(
            "EPWs",
            shift_coalesce(
                shift__ui_determinate(current, total, width),
                "Reading local files"
            ),
            width
        ))
    }
    if (
        isTRUE(
            details$unit_type %in%
                c("reanalysis_variable", "reanalysis_request")
        )
    ) {
        return(shift__ui_labeled_lines(
            "Variables",
            paste(
                c(
                    shift__ui_determinate(current, total, width),
                    if (!is.null(details$request_id)) {
                        paste("request", details$request_id)
                    },
                    details$status
                ),
                collapse = " \u00b7 "
            ),
            width
        ))
    }

    if (identical(stage, "resolve")) {
        attempt <- if (!is.na(current) && !is.na(total)) {
            sprintf("node %d of %d", as.integer(ordinal), as.integer(total))
        } else {
            "checking catalogs"
        }
        value <- paste(
            c(attempt, plan_context$selection),
            collapse = " \u00b7 "
        )
        return(shift__ui_labeled_lines("Status", value, width))
    }

    if (
        identical(details$unit_type, "download_session") ||
            identical(stage, "download")
    ) {
        parts <- c(shift__ui_determinate(current, total, width))
        bytes_done <- shift__ui_metric_number(details, "bytes_done")
        bytes_total <- shift__ui_metric_number(details, "bytes_total")
        if (!is.na(bytes_done)) {
            parts <- c(
                parts,
                if (is.na(bytes_total)) {
                    shift__ui_bytes(bytes_done)
                } else {
                    sprintf(
                        "%s/%s",
                        shift__ui_bytes(bytes_done),
                        shift__ui_bytes(bytes_total)
                    )
                }
            )
        }
        speed <- shift__ui_metric_number(details, "speed_bps")
        if (!is.na(speed) && speed > 0) {
            parts <- c(parts, paste0(shift__ui_bytes(speed), "/s"))
        }
        eta <- shift__ui_eta(shift__ui_metric_number(details, "eta_seconds"))
        if (!is.null(eta)) {
            parts <- c(parts, eta)
        }
        active <- shift__ui_metric_number(details, "active_task_count")
        if (!is.na(active) && active > 0) {
            parts <- c(parts, sprintf("%d active", as.integer(active)))
        }
        return(shift__ui_labeled_lines(
            "Transfer",
            paste(parts, collapse = " \u00b7 "),
            width
        ))
    }

    if (stage %in% c("extract_future", "extract_reference")) {
        parts <- c(shift__ui_determinate(current, total, width))
        if (!is.null(details$access_method) && length(details$access_method)) {
            parts <- c(parts, as.character(details$access_method[[1L]]))
        }
        return(shift__ui_labeled_lines(
            "Plans",
            paste(parts, collapse = " \u00b7 "),
            width
        ))
    }

    cases_ready <- as.integer(shift_coalesce(state$cases_ready, 0L))
    cases_total <- as.integer(shift_coalesce(state$cases_total, 0L))
    outputs <- as.integer(shift_coalesce(state$outputs_completed, 0L))
    if (identical(stage, "coverage")) {
        value <- paste(
            c(
                shift__ui_determinate(cases_ready, cases_total, width),
                sprintf("ready %d", cases_ready),
                sprintf("missing %d", max(0L, cases_total - cases_ready))
            ),
            collapse = " \u00b7 "
        )
        return(shift__ui_labeled_lines("Cases", value, width))
    }
    if (identical(stage, "morph")) {
        completed <- if (!is.na(current)) as.integer(current) else 0L
        target <- if (!is.na(total)) as.integer(total) else cases_ready
        value <- shift_coalesce(
            shift__ui_determinate(completed, target, width),
            sprintf("%d/%d", completed, target)
        )
        return(shift__ui_labeled_lines("Cases", value, width))
    }
    if (identical(stage, "write_epw")) {
        current_case <- if (!is.na(current)) as.integer(current) else outputs
        target <- if (!is.na(total)) as.integer(total) else cases_total
        value <- paste(
            c(
                shift__ui_determinate(current_case, target, width),
                sprintf("exported %d files", outputs)
            ),
            collapse = " \u00b7 "
        )
        return(shift__ui_labeled_lines("EPWs", value, width))
    }

    if (is.null(plan_context$selection)) {
        return(character())
    }
    value <- paste(plan_context$selection, collapse = " \u00b7 ")
    shift__ui_labeled_lines("Status", value, width)
}

# Describe actual request activity separately from total operation time. Only
# received responses/rows are counted; no elapsed-time estimate implies server
# health or overall completion. Cached responses are labelled separately.
shift__ui_query_lines <- function(state, width = shift__ui_width()) {
    details <- shift_coalesce(state$current_details, list())
    if (is.null(details$request_started_at)) {
        return(character())
    }
    now <- shift_coalesce(state$now_seconds, as.numeric(Sys.time()))
    active <- isTRUE(details$transfer_state %in% c("started", "transfer"))
    request_time <- if (active) {
        now - details$request_started_at
    } else {
        details$request_seconds
    }
    parts <- c(
        if (active) {
            paste("waiting", shift__format_elapsed(request_time))
        } else {
            paste(
                "response in",
                shift__format_elapsed(shift_coalesce(request_time, 0))
            )
        },
        if (isTRUE(details$query_timeout > 0) && active) {
            paste("timeout", shift__format_elapsed(details$query_timeout))
        },
        if (!is.null(details$last_response_at) && active) {
            paste(
                "last response",
                shift__format_elapsed(now - details$last_response_at),
                "ago"
            )
        }
    )
    counts <- c(
        sprintf("%d responses", shift_coalesce(details$responses, 0L)),
        sprintf("%d cached", shift_coalesce(details$cache_hits, 0L)),
        sprintf(
            "%d %s catalog records received",
            shift_coalesce(details$records_received, 0L),
            shift_coalesce(details$catalog_role, "")
        )
    )
    c(
        shift__ui_labeled_lines(
            "Request",
            paste(parts, collapse = " \u00b7 "),
            width
        ),
        shift__ui_labeled_lines(
            "Received",
            paste(counts, collapse = " \u00b7 "),
            width
        )
    )
}

# Return one terminal-safe animation frame without making motion essential to
# understanding the active state. Reduced motion uses a stable marker.
shift__ui_spinner <- function(
    motion = c("none", "full", "reduced"),
    frame = 0L
) {
    motion <- match.arg(motion)
    if (identical(motion, "none")) {
        return("")
    }
    if (identical(motion, "reduced")) {
        return("\u25cf")
    }
    frames <- c(
        "\u280b",
        "\u2819",
        "\u2839",
        "\u2838",
        "\u283c",
        "\u2834",
        "\u2826",
        "\u2827",
        "\u2807",
        "\u280f"
    )
    frames[[as.integer(frame) %% length(frames) + 1L]]
}

# Map durable outcomes and live states to symbols before optional colour is
# applied. Every colour retains a distinct glyph for monochrome terminals.
shift__ui_state_symbol <- function(
    status,
    motion = "none",
    frame = 0L,
    colour = TRUE
) {
    status <- as.character(shift_coalesce(status, "pending"))[[1L]]
    symbol <- switch(
        status,
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
    switch(
        status,
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
shift__ui_stage_rail <- function(
    state,
    width = shift__ui_width(),
    motion = "none",
    frame = 0L
) {
    width <- shift__ui_width(width)
    sequence <- as.character(shift_coalesce(state$stage_sequence, character()))
    current <- as.character(shift_coalesce(state$stage, "planned"))[[1L]]
    if (!length(sequence)) {
        sequence <- unique(c(
            current,
            as.character(shift_coalesce(state$next_stage, character()))
        ))
    }
    sequence <- sequence[!is.na(sequence) & nzchar(sequence)]
    if (!length(sequence)) {
        return(shift__ui_labeled_lines(
            "Flow",
            "Waiting for workflow stages",
            width
        ))
    }
    short <- c(
        collect = "Collect",
        resolve = "Resolve",
        download = "Download",
        extract = "Extract",
        extract_future = "Future",
        extract_reference = "Reference",
        coverage = "Coverage",
        morph = "Morph",
        write_epw = "EPW",
        export_epw = "Export"
    )
    labels <- vapply(
        sequence,
        function(stage) {
            if (stage %in% names(short)) {
                short[[stage]]
            } else {
                shift__ui_stage_label(stage)
            }
        },
        character(1L)
    )
    completed <- as.character(shift_coalesce(
        state$completed_stages,
        character()
    ))
    terminal <- as.character(shift_coalesce(state$status, "running"))[[1L]]
    current_index <- match(current, sequence)
    values <- vapply(
        seq_along(sequence),
        function(i) {
            stage <- sequence[[i]]
            stage_status <- if (
                stage %in% completed || identical(terminal, "completed")
            ) {
                "completed"
            } else if (identical(stage, current)) {
                if (
                    terminal %in%
                        c("failed", "cancelled", "partial", "stopping")
                ) {
                    terminal
                } else {
                    "current"
                }
            } else {
                "pending"
            }
            label <- switch(
                stage_status,
                completed = cli::col_green(labels[[i]]),
                current = cli::style_bold(cli::col_cyan(labels[[i]])),
                failed = cli::style_bold(cli::col_red(labels[[i]])),
                cancelled = cli::style_bold(cli::col_red(labels[[i]])),
                partial = cli::style_bold(cli::col_yellow(labels[[i]])),
                stopping = cli::style_bold(cli::col_yellow(labels[[i]])),
                cli::style_dim(labels[[i]])
            )
            paste(
                shift__ui_state_symbol(stage_status, motion = "none", frame),
                label
            )
        },
        character(1L)
    )
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
        sprintf(
            "[%d/%d] %s \u00b7 %s",
            position,
            length(sequence),
            values[[position]],
            next_value
        ),
        sprintf("[%d/%d] %s", position, length(sequence), values[[position]]),
        values[[position]]
    )
    rows <- vapply(
        candidates,
        function(value) {
            shift__ui_labeled_line("Flow", value)
        },
        character(1L)
    )
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

# Keep two recent business milestones below a quiet section heading. Long
# milestones use indented continuation rows; the framebuffer already owns a
# variable-height region and erases stale rows when the next frame contracts.
shift__ui_recent_lines <- function(state, width = shift__ui_width()) {
    values <- as.character(shift_coalesce(
        state$recent_events,
        shift_coalesce(state$last_event, character())
    ))
    values <- values[!is.na(values) & nzchar(values)]
    values <- utils::tail(values, 2L)
    outcomes <- as.character(shift_coalesce(state$recent_outcomes, character()))
    if (length(outcomes) < length(values)) {
        outcomes <- c(
            rep("completed", length(values) - length(outcomes)),
            outcomes
        )
    }
    outcomes <- utils::tail(outcomes, length(values))
    if (!length(values)) {
        values <- "No completed activity yet"
        outcomes <- "pending"
    }
    values <- unlist(
        lapply(seq_along(values), function(i) {
            prefix <- paste0(
                "  ",
                shift__ui_state_symbol(outcomes[[i]], motion = "none"),
                " "
            )
            shift__ui_prefixed_lines(prefix, values[[i]], width)
        }),
        use.names = FALSE
    )
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
    summary <- shift_coalesce(
        state$result_summary,
        sprintf(
            "%d/%d EPW%s exported \u00b7 %d missing",
            outputs,
            total,
            if (total == 1L) "" else "s",
            missing
        )
    )
    lines <- shift__ui_labeled_lines("Summary", summary, width)

    output_dir <- shift_coalesce(
        state$output_dir,
        shift_coalesce(state$plan_context$output, NULL)
    )
    if (
        !is.null(output_dir) &&
            length(output_dir) &&
            !is.na(output_dir[[1L]]) &&
            nzchar(output_dir[[1L]])
    ) {
        lines <- c(
            lines,
            shift__ui_labeled_lines(
                "Output",
                shift__display_path(output_dir[[1L]]),
                width
            )
        )
    }

    paths <- as.character(shift_coalesce(state$output_paths, character()))
    paths <- unique(paths[!is.na(paths) & nzchar(paths)])
    limit <- suppressWarnings(as.numeric(shift_coalesce(
        state$output_path_limit,
        5L
    ))[[1L]])
    if (!length(limit) || is.na(limit) || limit < 1) {
        limit <- 5L
    }
    shown <- if (is.finite(limit)) {
        utils::head(paths, as.integer(limit))
    } else {
        paths
    }
    omitted <- length(paths) - length(shown)
    if (length(shown)) {
        for (i in seq_along(shown)) {
            lines <- c(
                lines,
                shift__ui_labeled_lines(
                    if (i == 1L) "Files" else "",
                    basename(shown[[i]]),
                    width
                )
            )
        }
    }
    if (omitted > 0L) {
        lines <- c(
            lines,
            shift__ui_labeled_lines(
                "",
                sprintf(
                    "\u2026 %d more output%s",
                    omitted,
                    if (omitted == 1L) "" else "s"
                ),
                width
            )
        )
    }
    if (!is.null(state$field_summary)) {
        lines <- c(
            lines,
            shift__ui_labeled_lines("Fields", state$field_summary, width)
        )
    }
    for (message in utils::head(state$warning_messages, 3L)) {
        lines <- c(lines, shift__ui_labeled_lines("Warning", message, width))
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
        if (number("coverage_failures")) {
            sprintf(
                "%d incomplete",
                number("coverage_failures")
            )
        },
        if (number("timeout_failures")) {
            sprintf(
                "%d timeout",
                number("timeout_failures")
            )
        },
        if (number("network_failures")) {
            sprintf(
                "%d network",
                number("network_failures")
            )
        },
        if (number("other_failures")) {
            sprintf(
                "%d other",
                number("other_failures")
            )
        }
    )
    checked <- number("nodes_checked")
    summary <- if (checked > 0L) {
        paste(
            c(
                sprintf("%d checked", checked),
                counts,
                sprintf("%d usable", number("usable_nodes"))
            ),
            collapse = " \u00b7 "
        )
    } else {
        shift_coalesce(failure$kind, "workflow failed")
    }
    reason <- as.character(shift_coalesce(
        failure$cause,
        shift_coalesce(
            failure$summary,
            shift_coalesce(state$last_event, "Workflow failed")
        )
    ))[[1L]]
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
    failed_prefix <- paste0(
        "  ",
        shift__ui_state_symbol("failed", motion = "none"),
        " "
    )
    info_prefix <- paste0("  ", cli::col_blue("i"), " ")
    c(
        shift__ui_labeled_lines("Summary", summary, width),
        shift__ui_prefixed_lines(failed_prefix, reason, width),
        shift__ui_prefixed_lines(
            info_prefix,
            paste(evidence, collapse = " \u00b7 "),
            width
        )
    )
}

# Reduce a resolver outcome to a stable, actionable phrase for the live frame.
# Complete errors remain available in persisted events, detail tables, and logs.
shift__ui_node_result_short <- function(row) {
    outcome <- as.character(shift_coalesce(row$outcome, "rejected"))[[1L]]
    result <- as.character(shift_coalesce(row$result, outcome))[[1L]]
    future <- suppressWarnings(as.numeric(shift_coalesce(row$future, NA_real_)))
    reference <- suppressWarnings(as.numeric(shift_coalesce(
        row$reference,
        NA_real_
    )))
    if (outcome %in% c("completed", "skipped", "reused")) {
        return(result)
    }
    if (!is.na(reference) && reference == 0 && !is.na(future) && future > 0) {
        return("no reference files")
    }
    if (!is.na(future) && future == 0) {
        return("no future files")
    }
    switch(
        shift__ui_error_kind(result),
        timeout = "request timed out",
        network = "network error",
        coverage = "incomplete coverage",
        ambiguity = "ambiguous selection",
        shift__ui_fit(shift__error_summary(result), 48L)
    )
}

# Show at most two completed resolver decisions. The active node already owns
# the single animated `Now` row, so repeating it here would create visual noise.
shift__ui_live_node_lines <- function(
    state,
    width = shift__ui_width(),
    motion = "none",
    frame = 0L
) {
    rows <- data.table::as.data.table(shift_coalesce(
        state$node_rows,
        data.table::data.table()
    ))
    attempts <- nrow(rows)
    values <- character()
    if (nrow(rows)) {
        rows <- utils::tail(rows, 2L)
        values <- unlist(
            lapply(seq_len(nrow(rows)), function(i) {
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
                if (
                    "reference" %in% names(rows) && !is.na(rows$reference[[i]])
                ) {
                    counts <- c(
                        counts,
                        sprintf("%d reference", rows$reference[[i]])
                    )
                }
                if (
                    "duration" %in%
                        names(rows) &&
                        !is.na(rows$duration[[i]]) &&
                        nzchar(rows$duration[[i]]) &&
                        !identical(rows$duration[[i]], "\u2014")
                ) {
                    counts <- c(counts, rows$duration[[i]])
                }
                suffix <- if (length(counts)) {
                    paste0(" \u00b7 ", paste(counts, collapse = " \u00b7 "))
                } else {
                    ""
                }
                prefix <- sprintf(
                    "  %s %-6s ",
                    shift__ui_state_symbol(outcome, motion = "none", frame),
                    rows$node[[i]]
                )
                shift__ui_prefixed_lines(
                    prefix,
                    paste0(shift__ui_node_result_short(row), suffix),
                    width
                )
            }),
            use.names = FALSE
        )
    }
    values <- c(values, rep("", max(0L, 2L - length(values))))
    details <- shift_coalesce(state$current_details, list())
    current <- shift__ui_metric_number(details, "current", 0)
    total <- shift__ui_metric_number(details, "total", NA_real_)
    heading <- if (!is.na(total) && total > 0) {
        sprintf(
            "%d tried \u00b7 %d remaining",
            attempts,
            max(0L, as.integer(total) - as.integer(current))
        )
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
shift__ui_status_lines <- function(
    state,
    width = shift__ui_width(),
    motion = c("none", "full", "reduced"),
    frame = 0L
) {
    motion <- match.arg(motion)
    if (
        identical(state$batch_context$kind, "discovery") &&
            identical(state$stage, "discovery")
    ) {
        return(shift_batch__discovery_lines(state, width, motion, frame))
    }
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
    task_label <- as.character(shift_coalesce(
        state$task_label,
        shift_coalesce(plan_context$title, "Future EPW")
    ))[[1L]]
    header_parts <- c(
        cli::style_bold(task_label),
        shift__ui_status_style(status, state$stage),
        cli::style_dim(elapsed),
        if (nzchar(run_label) && !identical(state$detail, "normal")) {
            cli::style_dim(paste("run", run_label))
        }
    )
    header <- paste(
        header_parts[!vapply(header_parts, is.null, logical(1L))],
        collapse = "  "
    )
    plan_lines <- shift__ui_plan_lines(plan_context, width = content_width)
    # Standalone task labels and the generic input placeholder add no context
    # beyond the title. Keep real selections and paths when they are present.
    if (
        length(plan_context$items) &&
            all(
                plan_context$items %in%
                    c(task_label, "input request")
            )
    ) {
        plan_lines <- character()
    }
    batch <- shift_coalesce(state$batch_context, list())
    if (length(batch)) {
        batch_line <- if (identical(batch$kind, "discovery")) {
            shift__ui_labeled_lines("Discovery", batch$message, content_width)
        } else {
            shift__ui_labeled_lines(
                "Batch",
                sprintf(
                    "%s \u00b7 child %d/%d \u00b7 %d completed \u00b7 %d failed",
                    batch$id,
                    batch$current,
                    batch$total,
                    batch$completed,
                    batch$failed
                ),
                content_width
            )
        }
        plan_lines <- c(batch_line, plan_lines)
    }
    details <- shift_coalesce(state$current_details, list())
    current_context <- character()
    if (
        !is.null(details$node) &&
            length(details$node) &&
            !is.na(details$node[[1L]])
    ) {
        current_context <- c(
            current_context,
            shift__node_label(details$node[[1L]])
        )
    }
    if (
        !is.null(details$catalog_role) &&
            length(details$catalog_role) &&
            !is.na(details$catalog_role[[1L]])
    ) {
        role <- as.character(details$catalog_role[[1L]])
        current_context <- c(
            current_context,
            if (role %in% c("future", "reference")) {
                paste(role, "catalog")
            } else {
                role
            }
        )
    }
    current_label <- shift_coalesce(
        state$unit_label,
        shift_coalesce(state$stage_message, "Waiting")
    )
    if (length(current_context)) {
        current_label <- paste(
            c(current_context, current_label),
            collapse = " \u00b7 "
        )
    }
    current_status <- if (
        status %in%
            c(
                "queued",
                "waiting",
                "stopping",
                "completed",
                "partial",
                "failed",
                "cancelled"
            )
    ) {
        status
    } else {
        "running"
    }
    if (identical(current_status, "waiting")) {
        current_status <- "completed"
    }
    current_label_name <- if (identical(status, "failed")) {
        "Failure"
    } else if (identical(status, "cancelled")) {
        "Stopped"
    } else if (
        isTRUE(
            details$unit_type %in%
                c("reanalysis_variable", "reanalysis_request")
        )
    ) {
        "Calibration"
    } else {
        "Now"
    }
    current <- shift__ui_labeled_lines(
        current_label_name,
        sprintf(
            "%s %s",
            shift__ui_state_symbol(current_status, motion, frame),
            cli::style_bold(current_label)
        ),
        content_width
    )
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
    plan_lines <- vapply(
        plan_lines,
        shift__ui_fit,
        character(1L),
        width = content_width
    )
    workflow <- vapply(
        c(
            if (length(state$stage_sequence) > 1L) {
                shift__ui_stage_rail(state, content_width, motion, frame)
            },
            if (!terminal_result) current,
            if (
                !terminal_result ||
                    isTRUE(
                        state$stage %in%
                            c("coverage", "morph", "write_epw")
                    )
            ) {
                metrics
            }
        ),
        shift__ui_fit,
        character(1L),
        width = content_width
    )
    if (!isTRUE(panel)) {
        return(c(header, plan_lines, workflow, context))
    }
    c(
        shift__ui_panel_rule(header, width, "top"),
        vapply(plan_lines, shift__ui_panel_line, character(1L), width = width),
        if (length(workflow)) {
            c(
                shift__ui_panel_rule(
                    cli::style_bold("Workflow"),
                    width,
                    "middle"
                ),
                vapply(
                    workflow,
                    shift__ui_panel_line,
                    character(1L),
                    width = width
                )
            )
        },
        shift__ui_panel_rule(
            cli::style_bold(
                if (isTRUE(terminal_problem)) {
                    "Diagnosis"
                } else if (isTRUE(terminal_result)) {
                    "Results"
                } else {
                    "Activity"
                }
            ),
            width,
            "middle"
        ),
        vapply(context, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(width = width, kind = "bottom")
    )
}

# Collapse the same semantic dashboard into one useful status row for RStudio
# and other dynamic consoles that support carriage returns but not cursor-up.
shift__ui_compact_line <- function(
    state,
    width = shift__ui_width(),
    motion = c("none", "full", "reduced"),
    frame = 0L
) {
    motion <- match.arg(motion)
    width <- shift__ui_width(width)
    status <- as.character(shift_coalesce(state$status, "running"))[[1L]]
    stage <- shift__ui_stage_label(shift_coalesce(state$stage, "planned"))
    # Lead with the method and total in compact discovery views so cropping
    # cannot discard the batch context behind a long catalog unit label.
    batch <- shift_coalesce(state$batch_context, list())
    # Discovery owns a compact status contract too: scope and request liveness
    # outrank long method names when an IDE exposes only one replaceable row.
    if (
        identical(batch$kind, "discovery") &&
            identical(state$stage, "discovery")
    ) {
        finished <- status %in% c("completed", "failed", "cancelled")
        summary <- if (finished) {
            shift_coalesce(state$result_summary, state$unit_label)
        } else {
            query <- shift_coalesce(state$current_details, list())
            paste(
                c(
                    if (!is.null(batch$current)) {
                        sprintf("method %d/%d", batch$current, batch$total)
                    },
                    batch$scope,
                    if (
                        isTRUE(
                            query$transfer_state %in% c("started", "transfer")
                        )
                    ) {
                        paste(
                            "wait",
                            shift__format_elapsed(
                                as.numeric(Sys.time()) -
                                    query$request_started_at
                            )
                        )
                    } else if (!is.null(batch$alternative)) {
                        sprintf(
                            "inputs %d/%d",
                            batch$alternative,
                            batch$alternatives
                        )
                    }
                ),
                collapse = " \u00b7 "
            )
        }
        return(shift__ui_fit(
            paste(
                shift__ui_state_symbol(status, motion, frame),
                if (finished) toupper(status) else "Discovery",
                summary
            ),
            width
        ))
    }
    if (identical(batch$kind, "discovery")) {
        stage <- if (!is.null(batch$method_label)) {
            sprintf(
                "Discovery %d/%d: %s / inputs %d/%d / %s",
                batch$current,
                batch$total,
                batch$method_label,
                batch$alternative,
                batch$alternatives,
                batch$scope
            )
        } else {
            paste("Discovery", batch$message)
        }
    }
    values <- shift__ui_progress_values(state)
    counter <- if (
        !is.na(values$current) && !is.na(values$total) && values$total > 0
    ) {
        sprintf("%d/%d", as.integer(values$current), as.integer(values$total))
    } else {
        NULL
    }
    if (identical(batch$kind, "discovery")) {
        counter <- NULL
    }
    details <- shift_coalesce(state$current_details, list())
    if (
        isTRUE(
            details$unit_type %in%
                c("reanalysis_variable", "reanalysis_request")
        )
    ) {
        stage <- "Calibration"
        if (!is.null(counter)) counter <- paste(counter, "variables")
    }
    context <- character()
    if (
        !is.null(details$node) &&
            length(details$node) &&
            !is.na(details$node[[1L]])
    ) {
        context <- c(context, shift__node_label(details$node[[1L]]))
    }
    if (
        !is.null(details$catalog_role) &&
            length(details$catalog_role) &&
            !is.na(details$catalog_role[[1L]])
    ) {
        context <- c(context, as.character(details$catalog_role[[1L]]))
    }
    unit <- as.character(shift_coalesce(
        state$unit_label,
        shift_coalesce(state$stage_message, "Waiting")
    ))[[1L]]
    # Avoid repeating node/catalog prefixes already embedded in the business
    # unit while still retaining them for terse persisted unit labels.
    if (
        length(context) &&
            !all(vapply(context, grepl, logical(1L), x = unit, fixed = TRUE))
    ) {
        unit <- paste(c(context, unit), collapse = " \u00b7 ")
    }
    marker <- if (
        status %in%
            c(
                "failed",
                "cancelled",
                "partial",
                "stopping",
                "waiting",
                "completed"
            )
    ) {
        status
    } else {
        "running"
    }
    if (identical(marker, "waiting")) {
        marker <- "completed"
    }
    # A single warning glyph cannot distinguish pending cancellation from a
    # partial result; keep the stopping state explicit even in one-line views.
    if (identical(status, "stopping")) {
        stage <- paste("STOPPING", stage)
    }
    if (identical(status, "waiting")) {
        stage <- paste(
            cli::ansi_strip(shift__ui_status_style(status, state$stage)),
            stage
        )
    }
    parts <- c(
        paste(shift__ui_state_symbol(marker, motion, frame), stage),
        counter,
        unit,
        shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    )
    shift__ui_fit(
        paste(parts[!is.na(parts) & nzchar(parts)], collapse = " \u00b7 "),
        width
    )
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
    power <- if (bytes <= 0) {
        0L
    } else {
        min(floor(log(bytes, 1024)), length(labels) - 1L)
    }
    value <- bytes / (1024^power)
    sprintf(
        if (power == 0L) "%.0f %s" else "%.1f %s",
        value,
        labels[[power + 1L]]
    )
}

# Convert an index-node URL into the stable short name used in every normal and
# detail view. Unknown nodes fall back to their host name.
shift__node_label <- function(node) {
    node <- as.character(shift_coalesce(node, "unknown"))[[1L]]
    normalized <- tryCatch(query__normalize_node(node), error = function(e) {
        node
    })
    known <- vapply(
        INDEX_NODES,
        function(value) {
            identical(
                tryCatch(query__normalize_node(value), error = function(e) {
                    value
                }),
                normalized
            )
        },
        logical(1L)
    )
    if (any(known)) {
        return(names(INDEX_NODES)[which(known)[[1L]]])
    }
    parsed <- tryCatch(curl::curl_parse_url(normalized), error = function(e) {
        NULL
    })
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
    if (grepl("timed? out|timeout|operation too slow", message)) {
        return("timeout")
    }
    if (
        grepl(
            "http|status code|could not resolve|connect|ssl|certificate",
            message
        )
    ) {
        return("network")
    }
    if (
        grepl(
            "missing|coverage|complete|required variable|year|empty|no .*files?",
            message
        )
    ) {
        return("coverage")
    }
    if (grepl("ambiguous|multiple|more than one", message)) {
        return("ambiguity")
    }
    "error"
}

# Format resolver attempts as a width-safe table. Normal output uses stable
# short outcomes; detail and debug retain the complete persisted exception.
shift__ui_node_table <- function(
    rows,
    width = shift__ui_width(),
    detail = "normal"
) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift__ui_width(width)
    # Persisted resolver events may omit counts or labels. Replace missing cells
    # before measuring widths so the table remains stable.
    shift__display_max <- function(x) {
        x <- as.character(x)
        x[is.na(x) | !nzchar(x)] <- "\u2014"
        max(cli::ansi_nchar(x, type = "width"))
    }
    node_width <- min(12L, max(4L, shift__display_max(c("Node", rows$node))))
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
        shift__ui_fit(
            paste0(
                "  ",
                paste(
                    mapply(
                        shift__ui_cell,
                        values,
                        sizes,
                        USE.NAMES = FALSE
                    ),
                    collapse = "  "
                )
            ),
            width
        )
    }
    lines <- c(
        cli::style_bold("Resolver attempts"),
        cli::style_dim(row_line(columns))
    )
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
shift__ui_case_table <- function(
    rows,
    width = shift__ui_width(),
    detail = "normal"
) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    # Multi-method/model views use wrapped identity rows so terminal width
    # cannot erase the columns that distinguish otherwise identical cases.
    if (
        "method" %in%
            names(rows) ||
            ("source_id" %in%
                names(rows) &&
                data.table::uniqueN(rows$source_id) > 1L)
    ) {
        lines <- "Cases"
        columns <- intersect(
            c(
                "method",
                "scale",
                "reconstruction",
                "model",
                "source_id",
                "experiment_id",
                "period",
                "variant_label",
                "status"
            ),
            names(rows)
        )
        if ("model" %in% columns) {
            columns <- setdiff(columns, "source_id")
        }
        for (index in seq_len(nrow(rows))) {
            values <- vapply(
                columns,
                function(column) {
                    as.character(rows[[column]][[index]])
                },
                character(1L)
            )
            lines <- c(
                lines,
                shift__ui_prefixed_lines(
                    "  ",
                    paste(values, collapse = " \u00b7 "),
                    width
                )
            )
            if (
                !identical(detail, "normal") &&
                    "missing_reason" %in% names(rows) &&
                    !is.na(rows$missing_reason[[index]]) &&
                    nzchar(rows$missing_reason[[index]])
            ) {
                lines <- c(
                    lines,
                    shift__ui_prefixed_lines(
                        "    ",
                        rows$missing_reason[[index]],
                        width
                    )
                )
            }
        }
        return(lines)
    }
    width <- shift__ui_width(width)
    scenario <- if ("experiment_id" %in% names(rows)) {
        rows$experiment_id
    } else {
        rep("\u2014", nrow(rows))
    }
    period <- if ("period" %in% names(rows)) {
        rows$period
    } else {
        rep("\u2014", nrow(rows))
    }
    member <- if ("variant_label" %in% names(rows)) {
        rows$variant_label
    } else {
        rep("\u2014", nrow(rows))
    }
    status <- if ("status" %in% names(rows)) {
        rows$status
    } else {
        rep("unknown", nrow(rows))
    }
    include_member <- width >= 68L
    # Planned cases legitimately carry unresolved member/grid values. Replace
    # them before measuring columns so NA cannot propagate into ansi_align() as
    # a literal "NA" suffix in the static dashboard table.
    shift__display_max <- function(x) {
        x <- as.character(x)
        x[is.na(x) | !nzchar(x)] <- "\u2014"
        max(cli::ansi_nchar(x, type = "width"))
    }
    scenario_width <- min(
        14L,
        max(8L, shift__display_max(c("Scenario", scenario)))
    )
    period_width <- min(12L, max(6L, shift__display_max(c("Period", period))))
    member_width <- if (include_member) {
        min(14L, max(6L, shift__display_max(c("Member", member))))
    } else {
        0L
    }
    fixed <- scenario_width +
        period_width +
        member_width +
        if (include_member) 10L else 7L
    status_width <- max(10L, width - fixed)
    header <- if (include_member) {
        sprintf(
            "  %s  %s  %s  %s",
            shift__ui_cell("Scenario", scenario_width),
            shift__ui_cell("Period", period_width),
            shift__ui_cell("Member", member_width),
            shift__ui_cell("Status", status_width)
        )
    } else {
        sprintf(
            "  %s  %s  %s",
            shift__ui_cell("Scenario", scenario_width),
            shift__ui_cell("Period", period_width),
            shift__ui_cell("Status", status_width)
        )
    }
    lines <- c("Cases", header)
    for (i in seq_len(nrow(rows))) {
        value <- status[[i]]
        line <- if (include_member) {
            sprintf(
                "  %s  %s  %s  %s",
                shift__ui_cell(scenario[[i]], scenario_width),
                shift__ui_cell(period[[i]], period_width),
                shift__ui_cell(member[[i]], member_width),
                shift__ui_cell(value, status_width)
            )
        } else {
            sprintf(
                "  %s  %s  %s",
                shift__ui_cell(scenario[[i]], scenario_width),
                shift__ui_cell(period[[i]], period_width),
                shift__ui_cell(value, status_width)
            )
        }
        lines <- c(lines, line)
        if (
            !identical(detail, "normal") &&
                "missing_reason" %in% names(rows) &&
                !is.na(rows$missing_reason[[i]]) &&
                nzchar(rows$missing_reason[[i]])
        ) {
            reason_width <- max(1L, width - 4L)
            wrapped <- cli::ansi_strwrap(
                rows$missing_reason[[i]],
                width = reason_width
            )
            lines <- c(lines, paste0("    ", wrapped))
        }
    }
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Build the complete watch view once so R and CLI renderers cannot drift in
# stage, case, resolver, or width semantics.
shift__ui_table_view <- function(
    row,
    cases,
    events,
    width = shift__ui_width(),
    detail = "normal",
    motion = "none",
    frame = 0L,
    outputs = NULL,
    diagnostics = NULL,
    ui_state = NULL
) {
    state <- shift__ui_table_state(row, events, cases)
    state <- shift__ui_live_state(row, state, ui_state)
    if (!is.null(outputs) && state$status %in% c("completed", "partial")) {
        completion <- shift__ui_completion(
            cases,
            outputs,
            shift_coalesce(diagnostics, shift_diagnostics_empty())
        )
        state[names(completion)] <- completion
        state$outputs_completed <- nrow(outputs)
        state$output_paths <- shift_coalesce(outputs$export_path, outputs$path)
    }
    # Normal watch output mirrors the foreground receipt's five-file cap;
    # explicit detail/debug views retain every persisted export path.
    state$output_path_limit <- if (identical(detail, "normal")) 5L else Inf
    list(
        state = state,
        lines = shift__ui_status_lines(
            state,
            width = width,
            motion = motion,
            frame = frame
        ),
        compact = shift__ui_compact_line(
            state,
            width = width,
            motion = motion,
            frame = frame
        ),
        nodes = shift__ui_node_table(
            shift__ui_event_nodes(events),
            width = width,
            detail = detail
        ),
        cases = shift__ui_case_table(cases, width = width, detail = detail)
    )
}

# Adapt a live ShiftRun handle to the table-based view shared with the CLI.
shift__ui_run_view <- function(
    run,
    width = shift__ui_width(),
    detail = "normal",
    motion = "none",
    frame = 0L
) {
    # Cached handles may have no store or morph identity. Keep their existing
    # preview usable without opening the user's unrelated default store.
    outputs <- if (is.null(run@store_path) || !nzchar(run@store_path)) {
        run@meta$outputs
    } else {
        tryCatch(shift_outputs(run, refresh = FALSE), error = function(error) {
            NULL
        })
    }
    shift__ui_table_view(
        row = run@meta$run,
        cases = shift_cases(run, refresh = FALSE),
        events = run@meta$events,
        width = width,
        detail = detail,
        motion = motion,
        frame = frame,
        outputs = outputs,
        diagnostics = shift_diagnostics(run, refresh = FALSE),
        ui_state = run@meta$ui_state
    )
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
shift__ui_persisted_event_line <- function(
    event,
    detail = "normal",
    width = NULL
) {
    details <- shift__ui_event_details(event)[[1L]]
    context <- c(shift__ui_stage_label(event$stage[[1L]]))
    if ("method" %in% names(event)) {
        context <- c(event$method[[1L]], event$model[[1L]], context)
    }
    if (!is.null(details$node) && length(details$node)) {
        context <- c(
            context,
            if (identical(detail, "debug")) {
                as.character(details$node[[1L]])
            } else {
                shift__node_label(details$node)
            }
        )
    }
    if (!is.null(details$catalog_role) && length(details$catalog_role)) {
        context <- c(context, as.character(details$catalog_role[[1L]]))
    }
    line <- sprintf(
        "%s [%s] %s",
        format(event$created_at[[1L]], "%F %T"),
        paste(context, collapse = "]["),
        event$message[[1L]]
    )
    if (is.null(width)) line else shift__ui_fit(line, width)
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
