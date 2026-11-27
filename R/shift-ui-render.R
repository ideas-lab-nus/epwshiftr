#' @include shift-stage.R shift-ui-state.R
NULL

# Unified terminal presentation layer for Shift workflows:
# 1. Shared low-level layout & styling primitives (`shift_ui_render__*`),
#    reusable by `R/cli-render.R`.
# 2. Live ANSI framebuffer (`ShiftFrameRenderer`) and shared watch loop.
# 3. Single-run, discovery, and batch dashboard views.
# 4. S7 console `print()` receipts and actionable failure aborts.

# Shared terminal capability, geometry, and ANSI layout primitives {{{

# Query POSIX TTY row count at most twice a second; redirected output never
# opens /dev/tty.
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
            error = function(e) character()
        )
        rows <- if (length(size)) {
            suppressWarnings(as.integer(strsplit(
                trimws(size[[1L]]),
                "[[:space:]]+"
            )[[1L]][1L]))
        } else {
            NA_integer_
        }
        value <<- if (!is.na(rows) && rows >= 2L) rows else NULL
        value
    }
})

# shift_ui_render__width {{{
shift_ui_render__width <- function(width = NULL) {
    if (is.null(width)) {
        width <- tryCatch(cli::console_width(), error = function(e) 80L)
    }
    w <- suppressWarnings(as.integer(width[[1L]]))
    if (!length(w) || is.na(w) || w < 1L) 80L else w
}
# }}}

# shift_ui_render__dashboard_width {{{
shift_ui_render__dashboard_width <- function(width = NULL) {
    max(1L, shift_ui_render__width(width) - 1L)
}
# }}}

# shift_ui_render__height {{{
shift_ui_render__height <- function(height = NULL) {
    h <- shift_stage__coalesce(height, getOption("epwshiftr.ui_height"))
    if (is.null(h)) {
        h <- shift_stage__coalesce(
            shift__ui_terminal_height(),
            Sys.getenv("LINES", "24")
        )
    }
    h <- suppressWarnings(as.integer(h[1L]))
    if (!length(h) || is.na(h) || h < 2L) {
        h <- 24L
    }
    max(1L, h - 1L)
}
# }}}

# shift_ui_render__layout {{{
shift_ui_render__layout <- function(width = NULL) {
    terminal <- shift_ui_render__width(width)
    outer <- shift_ui_render__dashboard_width(terminal)
    panel <- terminal >= 60L
    list(
        terminal = terminal,
        width = outer,
        panel = panel,
        content_width = if (panel) max(1L, outer - 4L) else outer
    )
}
# }}}

# shift_ui_render__use_width {{{
shift_ui_render__use_width <- function(width, env = parent.frame()) {
    if (is.null(width)) {
        return(invisible(NULL))
    }
    old <- options(width = width, cli.width = width)
    withr::defer(options(old), envir = env)
    invisible(NULL)
}
# }}}

# shift_ui_render__mode {{{
shift_ui_render__mode <- function(ui) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(ui@progress, "auto")) {
        return(ui@progress)
    }
    ci <- tolower(trimws(Sys.getenv("CI")))
    if (
        (nzchar(ci) && !ci %in% c("0", "false", "no")) ||
            identical(tolower(Sys.getenv("TERM")), "dumb")
    ) {
        return("log")
    }
    if (isTRUE(cli::is_dynamic_tty())) "dynamic" else "log"
}
# }}}

# shift_ui_render__motion {{{
shift_ui_render__motion <- function(ui, mode = shift_ui_render__mode(ui)) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(mode, "dynamic")) {
        return("none")
    }
    if (identical(ui@motion, "auto")) "full" else ui@motion
}
# }}}

# shift_ui_render__open_session {{{
shift_ui_render__open_session <- function(ui) {
    mode <- shift_ui_render__mode(ui)
    motion <- shift_ui_render__motion(ui, mode)
    renderer <- tryCatch(shift_tui__ui_renderer(mode), error = function(e) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) {
        mode <- "log"
        motion <- "none"
    }
    list(mode = mode, motion = motion, renderer = renderer)
}
# }}}

# shift_ui_render__fit {{{
shift_ui_render__fit <- function(x, width = shift_ui_render__width()) {
    x <- gsub("[\r\n]+", " ", as.character(shift_stage__coalesce(x, "")))
    cli::ansi_strtrim(x, shift_ui_render__width(width))
}
# }}}

# shift_ui_render__hard_wrap {{{
shift_ui_render__hard_wrap <- function(x, width) {
    width <- max(1L, shift_ui_render__width(width))
    rem <- as.character(shift_stage__coalesce(x, ""))[[1L]]
    lines <- character()
    while (cli::ansi_nchar(rem, type = "width") > width) {
        n <- cli::ansi_nchar(rem, type = "chars")
        lo <- 1L
        hi <- n
        best <- 0L
        while (lo <= hi) {
            mid <- as.integer((lo + hi) %/% 2L)
            if (
                cli::ansi_nchar(
                    cli::ansi_substr(rem, 1L, mid),
                    type = "width"
                ) <=
                    width
            ) {
                best <- mid
                lo <- mid + 1L
            } else {
                hi <- mid - 1L
            }
        }
        best <- max(1L, best)
        lines <- c(
            lines,
            shift_ui_render__fit(cli::ansi_substr(rem, 1L, best), width)
        )
        rem <- cli::ansi_substr(rem, best + 1L, n)
    }
    c(lines, rem)
}
# }}}

# shift_ui_render__wrap_lines {{{
shift_ui_render__wrap_lines <- function(value, width) {
    width <- max(1L, shift_ui_render__width(width))
    wrapped <- cli::ansi_strwrap(value, width = width)
    if (!length(wrapped)) {
        return("")
    }
    unlist(
        lapply(wrapped, shift_ui_render__hard_wrap, width = width),
        use.names = FALSE
    )
}
# }}}

# shift_ui_render__prefixed_lines {{{
shift_ui_render__prefixed_lines <- function(
    prefix,
    value,
    width,
    continuation = NULL
) {
    width <- max(1L, shift_ui_render__width(width))
    prefix <- as.character(shift_stage__coalesce(prefix, ""))[[1L]]
    value <- gsub(
        "[\r\n]+",
        " ",
        as.character(shift_stage__coalesce(value, ""))[[1L]]
    )
    pw <- cli::ansi_nchar(prefix, type = "width")
    if (pw >= width) {
        return(c(
            shift_ui_render__fit(prefix, width),
            shift_ui_render__wrap_lines(value, width)
        ))
    }
    wrapped <- shift_ui_render__wrap_lines(value, max(1L, width - pw))
    cont <- shift_stage__coalesce(continuation, strrep(" ", min(pw, width)))
    lines <- paste0(
        c(prefix, rep(cont, max(0L, length(wrapped) - 1L))),
        wrapped
    )
    vapply(lines, shift_ui_render__fit, character(1L), width = width)
}
# }}}

# shift_ui_render__labeled_line {{{
shift_ui_render__labeled_line <- function(label, value) {
    padded <- paste0(
        label,
        strrep(" ", max(1L, 9L - cli::ansi_nchar(label, type = "width")))
    )
    trimmed <- trimws(padded)
    styled <- if (trimmed %in% c("Plan", "Flow", "Status", "Summary")) {
        cli::style_bold(cli::col_blue(padded))
    } else if (trimmed %in% c("Failure", "Stopped")) {
        cli::style_bold(cli::col_red(padded))
    } else {
        cli::style_dim(padded)
    }
    paste0(styled, value)
}
# }}}

# shift_ui_render__labeled_lines {{{
shift_ui_render__labeled_lines <- function(label, value, width) {
    pfx <- shift_ui_render__labeled_line(label, "")
    cont <- strrep(" ", cli::ansi_nchar(pfx, type = "width"))
    shift_ui_render__prefixed_lines(pfx, value, width, cont)
}
# }}}

# shift_ui_render__field {{{
shift_ui_render__field <- function(label, value, width, compact = FALSE) {
    if (isTRUE(compact)) {
        shift_ui_render__fit(shift_ui_render__labeled_line(label, value), width)
    } else {
        shift_ui_render__labeled_lines(label, value, width)
    }
}
# }}}

# shift_ui_render__pack_items {{{
shift_ui_render__pack_items <- function(items, width, separator = " \u00b7 ") {
    width <- max(1L, as.integer(width))
    items <- shift_ui_state__nonempty(shift_stage__coalesce(items, character()))
    if (!length(items)) {
        return("")
    }
    lines <- character()
    cur <- ""
    for (item in items) {
        cand <- if (nzchar(cur)) paste0(cur, separator, item) else item
        if (cli::ansi_nchar(cand, type = "width") <= width) {
            cur <- cand
            next
        }
        if (nzchar(cur)) {
            lines <- c(lines, cur)
            cur <- ""
        }
        wrapped <- shift_ui_render__wrap_lines(item, width = width)
        if (length(wrapped) > 1L) {
            lines <- c(lines, wrapped[-length(wrapped)])
        }
        cur <- wrapped[[length(wrapped)]]
    }
    c(lines, cur)
}
# }}}

# shift_ui_render__display_max {{{
shift_ui_render__display_max <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "\u2014"
    max(cli::ansi_nchar(x, type = "width"))
}
# }}}

# shift_ui_render__col_width {{{
shift_ui_render__col_width <- function(header, values, lo, hi) {
    min(hi, max(lo, shift_ui_render__display_max(c(header, values))))
}
# }}}

# shift_ui_render__cell {{{
shift_ui_render__cell <- function(x, width) {
    x <- ifelse(is.na(x), "\u2014", as.character(x))
    cli::ansi_align(shift_ui_render__fit(x, width), width, align = "left")
}
# }}}

# shift_ui_render__table_row {{{
shift_ui_render__table_row <- function(values, sizes, width) {
    cells <- mapply(shift_ui_render__cell, values, sizes, USE.NAMES = FALSE)
    shift_ui_render__fit(paste0("  ", paste(cells, collapse = "  ")), width)
}
# }}}

# Semantic colour styling helper shared across badges, symbols, and stage rail.
# shift_ui_render__colorize {{{
shift_ui_render__colorize <- function(status, text, default = NULL) {
    fn <- switch(
        status,
        completed = cli::col_green,
        skipped = cli::col_blue,
        reused = cli::col_blue,
        fallback = cli::col_blue,
        waiting = cli::col_blue,
        queued = cli::col_blue,
        partial = cli::col_yellow,
        stopping = cli::col_yellow,
        rejected = cli::col_yellow,
        failed = cli::col_red,
        cancelled = cli::col_red,
        running = cli::col_cyan,
        active = cli::col_cyan,
        current = cli::col_cyan,
        default
    )
    if (is.function(fn)) fn(text) else text
}
# }}}

# shift_ui_render__status_badge {{{
shift_ui_render__status_badge <- function(status, stage = NULL) {
    st <- tolower(as.character(shift_stage__coalesce(status, "running"))[[1L]])
    label <- if (identical(st, "waiting")) {
        if (isTRUE(stage %in% c("collect", "datasets"))) {
            "CATALOG READY"
        } else {
            "STEP COMPLETE"
        }
    } else {
        toupper(st)
    }
    cli::style_bold(shift_ui_render__colorize(
        st,
        label,
        default = cli::col_cyan
    ))
}
# }}}

# shift_ui_render__spinner {{{
shift_ui_render__spinner <- function(
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
# }}}

SHIFT_UI_GLYPHS <- c(
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
    current = "\u25cf",
    queued = "\u25cb",
    pending = "\u25cb"
)

# shift_ui_render__state_symbol {{{
shift_ui_render__state_symbol <- function(
    status,
    motion = "none",
    frame = 0L,
    colour = TRUE
) {
    st <- as.character(shift_stage__coalesce(status, "pending"))[[1L]]
    sym <- if (st %in% c("running", "active")) {
        shift_ui_render__spinner(motion, frame)
    } else if (st %in% names(SHIFT_UI_GLYPHS)) {
        unname(SHIFT_UI_GLYPHS[[st]])
    } else {
        "\u2022"
    }
    if (!nzchar(sym)) {
        sym <- "\u2022"
    }
    if (!isTRUE(colour) || identical(st, "queued")) {
        return(sym)
    }
    shift_ui_render__colorize(st, sym)
}
# }}}

# shift_ui_render__bar {{{
shift_ui_render__bar <- function(current, total, width = 18L) {
    width <- max(4L, as.integer(width))
    if (is.na(current) || is.na(total) || total <= 0) {
        return(cli::style_dim(strrep("\u2500", width)))
    }
    filled <- min(
        width,
        as.integer(floor(max(0, min(1, current / total)) * width))
    )
    paste0(
        cli::col_cyan(strrep("\u2501", filled)),
        cli::style_dim(strrep("\u2500", width - filled))
    )
}
# }}}

# shift_ui_render__determinate {{{
shift_ui_render__determinate <- function(current, total, width) {
    if (is.na(current) || is.na(total) || total <= 0) {
        return(NULL)
    }
    bw <- if (width >= 100L) {
        22L
    } else if (width >= 72L) {
        14L
    } else {
        8L
    }
    pct <- as.integer(round(max(0, min(1, current / total)) * 100))
    sprintf(
        "%s  %d/%d \u00b7 %d%%",
        shift_ui_render__bar(current, total, bw),
        as.integer(current),
        as.integer(total),
        pct
    )
}
# }}}

# shift_ui_render__panel_rule {{{
shift_ui_render__panel_rule <- function(
    label = NULL,
    width,
    kind = c("top", "middle", "bottom")
) {
    kind <- match.arg(kind)
    width <- shift_ui_render__width(width)
    g <- switch(
        kind,
        top = c("\u256d", "\u256e"),
        middle = c("\u251c", "\u2524"),
        bottom = c("\u2570", "\u256f")
    )
    inner <- max(1L, width - 2L)
    if (is.null(label) || !length(label) || !nzchar(cli::ansi_strip(label))) {
        return(cli::style_dim(paste0(
            g[[1L]],
            strrep("\u2500", inner),
            g[[2L]]
        )))
    }
    label <- shift_ui_render__fit(label, max(1L, inner - 3L))
    used <- cli::ansi_nchar(label, type = "width") + 3L
    paste0(
        cli::style_dim(paste0(g[[1L]], "\u2500 ")),
        label,
        cli::style_dim(paste0(
            " ",
            strrep("\u2500", max(0L, inner - used)),
            g[[2L]]
        ))
    )
}
# }}}

# shift_ui_render__panel_line {{{
shift_ui_render__panel_line <- function(value, width) {
    inner <- max(1L, shift_ui_render__width(width) - 4L)
    val <- cli::ansi_align(
        shift_ui_render__fit(value, inner),
        inner,
        align = "left"
    )
    paste0(cli::style_dim("\u2502 "), val, cli::style_dim(" \u2502"))
}
# }}}

# shift_ui_render__panel_box {{{
shift_ui_render__panel_box <- function(
    header,
    sections,
    width,
    panel = TRUE,
    wrap_header = FALSE
) {
    if (!isTRUE(panel)) {
        hd <- if (isTRUE(wrap_header)) {
            shift_ui_render__wrap_lines(header, width)
        } else {
            shift_ui_render__fit(header, width)
        }
        return(c(
            hd,
            unlist(lapply(sections, `[[`, "lines"), use.names = FALSE)
        ))
    }
    lines <- shift_ui_render__panel_rule(header, width, "top")
    for (sec in sections) {
        if (isTRUE(sec$omit_if_empty) && !length(sec$lines)) {
            next
        }
        if (!is.null(sec$title)) {
            lines <- c(
                lines,
                shift_ui_render__panel_rule(
                    cli::style_bold(sec$title),
                    width,
                    "middle"
                )
            )
        }
        lines <- c(
            lines,
            vapply(
                sec$lines,
                shift_ui_render__panel_line,
                character(1L),
                width = width
            )
        )
    }
    c(lines, shift_ui_render__panel_rule(width = width, kind = "bottom"))
}
# }}}

# shift_ui_render__panel_header {{{
shift_ui_render__panel_header <- function(
    title,
    status,
    stage = NULL,
    elapsed = NULL,
    run_id = NULL,
    detail = NULL,
    sep = "  "
) {
    run_label <- if (!is.null(run_id)) {
        shift_ui_view__ui_run_short(run_id)
    } else {
        ""
    }
    parts <- list(
        cli::style_bold(title),
        shift_ui_render__status_badge(status, stage),
        if (!is.null(elapsed)) {
            cli::style_dim(shift_ui_view__format_elapsed(elapsed))
        },
        if (nzchar(run_label) && !identical(detail, "normal")) {
            cli::style_dim(paste("run", run_label))
        }
    )
    paste(unlist(parts, use.names = FALSE), collapse = sep)
}
# }}}

# shift_ui_render__event_context {{{
shift_ui_render__event_context <- function(
    stage,
    details = list(),
    detail = "normal",
    prefix = character(),
    include_phase = FALSE
) {
    ctx <- c(prefix, shift_ui_view__ui_stage_label(stage))
    node <- shift_ui_state__chr1(details$node)
    if (!is.null(node)) {
        ctx <- c(
            ctx,
            if (identical(detail, "debug")) {
                node
            } else {
                shift_ui_view__node_label(node)
            }
        )
    }
    role <- shift_ui_state__chr1(details$catalog_role)
    if (
        is.null(role) &&
            isTRUE(include_phase) &&
            !isTRUE(details$phase %in% c("stage", "unit", "notice"))
    ) {
        role <- shift_ui_state__chr1(details$phase)
    }
    c(ctx, role)
}
# }}}

# shift_ui_render__print_view {{{
shift_ui_render__print_view <- function(view, include_tables = TRUE) {
    for (line in view$lines) {
        if (nzchar(cli::ansi_strip(line))) cli::cli_verbatim(line)
    }
    if (isTRUE(include_tables)) {
        for (line in c(view$nodes, view$cases)) {
            cli::cli_verbatim(line)
        }
    }
    invisible(view)
}
# }}}

# Back-compatible primitive aliases preserved for external callers.
shift_ui__ui_width <- shift_ui_render__width
shift_ui__ui_dashboard_width <- shift_ui_render__dashboard_width
shift_ui__ui_height <- shift_ui_render__height
shift_ui__ui_mode <- shift_ui_render__mode
shift_ui__ui_motion <- shift_ui_render__motion
shift_ui_view__ui_fit <- shift_ui_render__fit
shift_ui_view__ui_wrap_lines <- shift_ui_render__wrap_lines
shift_ui_view__ui_prefixed_lines <- shift_ui_render__prefixed_lines
shift_ui_view__ui_labeled_lines <- shift_ui_render__labeled_lines
shift_ui_view__ui_print_view <- shift_ui_render__print_view
shift_print__print_use_width <- shift_ui_render__use_width

# }}}

# Live ANSI Framebuffer & Watch Loop {{{

SHIFT_TUI_ERASE_LINE <- "\033[K"

# shift_tui__tui_cursor_up {{{
shift_tui__tui_cursor_up <- function(lines) {
    n <- as.integer(lines)
    if (!length(n) || is.na(n) || n <= 0L) "" else sprintf("\033[%dA", n)
}
# }}}

# shift_tui__tui_normalize_lines {{{
shift_tui__tui_normalize_lines <- function(lines) {
    gsub(
        "[\r\n]+",
        " ",
        as.character(shift_stage__coalesce(lines, character()))
    )
}
# }}}

# ShiftFrameRenderer owns the live terminal region for one workflow.
# ShiftFrameRenderer {{{
ShiftFrameRenderer <- R6::R6Class(
    "ShiftFrameRenderer",
    lock_class = TRUE,
    public = list(
        initialize = function(
            output = cli::cli_output_connection(),
            backend = c("frame", "compact"),
            writer = NULL
        ) {
            backend <- match.arg(backend)
            if (!inherits(output, "connection")) {
                cli::cli_abort("`output` must be an R connection.")
            }
            if (!is.null(writer) && !is.function(writer)) {
                cli::cli_abort("`writer` must be a function or `NULL`.")
            }
            private$output <- output
            private$backend_value <- backend
            private$writer <- shift_stage__coalesce(writer, function(text) {
                cat(text, file = output, sep = "")
                flush.console()
                invisible(NULL)
            })
        },

        draw = function(lines, compact = NULL) {
            lines <- shift_tui__tui_normalize_lines(lines)
            if (!length(lines)) {
                return(invisible(FALSE))
            }
            compact <- shift_tui__tui_normalize_lines(
                shift_stage__coalesce(compact, lines[[1L]])
            )[[1L]]
            private$last_frame <- lines
            private$last_compact <- compact
            private$frame_compacted <- identical(
                private$backend_value,
                "frame"
            ) &&
                length(lines) > shift_ui_render__height()
            if (private$frame_compacted) {
                lines <- compact
            }
            if (isTRUE(private$suspended)) {
                return(invisible(TRUE))
            }
            if (identical(private$backend_value, "compact")) {
                return(invisible(private$draw_compact(compact)))
            }
            invisible(private$draw_frame(lines))
        },

        clear = function() {
            if (identical(private$backend_value, "compact")) {
                private$close_compact("done")
            } else {
                private$clear_frame()
            }
            invisible(NULL)
        },

        suspend = function(code) {
            if (!is.function(code)) {
                cli::cli_abort("`code` must be a function.")
            }
            if (
                isTRUE(private$suspended) ||
                    !identical(private$backend_value, "frame") ||
                    !isTRUE(private$active_value)
            ) {
                return(code())
            }
            private$clear_frame()
            private$suspended <- TRUE
            on.exit(
                {
                    private$suspended <- FALSE
                    if (!isTRUE(private$closed) && length(private$last_frame)) {
                        self$draw(private$last_frame, private$last_compact)
                    }
                },
                add = TRUE
            )
            code()
        },

        commit = function(result = c("done", "failed", "cancelled")) {
            private$teardown(match.arg(result), preserve = TRUE)
        },
        close = function(result = c("done", "failed", "cancelled")) {
            private$teardown(match.arg(result), preserve = FALSE)
        },
        backend = function() private$backend_value,
        active = function() isTRUE(private$active_value)
    ),
    private = list(
        output = NULL,
        backend_value = NULL,
        writer = NULL,
        painted_lines = 0L,
        last_frame = character(),
        frame_compacted = FALSE,
        last_compact = NULL,
        compact_id = NULL,
        cursor_hidden = FALSE,
        suspended = FALSE,
        active_value = FALSE,
        closed = FALSE,

        write = function(text) {
            tryCatch(
                {
                    if (cli::num_ansi_colors() > 1L) {
                        text <- paste0("\033[0m", text, "\033[0m")
                    }
                    private$writer(text)
                    TRUE
                },
                error = function(e) FALSE
            )
        },

        draw_frame = function(lines) {
            if (isTRUE(private$closed)) {
                return(FALSE)
            }
            if (!isTRUE(private$cursor_hidden)) {
                try(cli::ansi_hide_cursor(private$output), silent = TRUE)
                private$cursor_hidden <- TRUE
            }
            prev <- min(private$painted_lines, shift_ui_render__height())
            cur <- length(lines)
            body <- paste(
                paste0("\r", lines, SHIFT_TUI_ERASE_LINE),
                collapse = "\n"
            )
            tail <- if (prev > cur) {
                ex <- prev - cur
                paste0(
                    strrep(paste0("\n\r", SHIFT_TUI_ERASE_LINE), ex),
                    shift_tui__tui_cursor_up(ex),
                    "\r"
                )
            } else {
                ""
            }
            out <- paste0(
                if (prev > 1L) shift_tui__tui_cursor_up(prev - 1L) else "",
                body,
                "\r",
                tail
            )
            if (!isTRUE(private$write(out))) {
                return(FALSE)
            }
            private$painted_lines <- cur
            private$active_value <- TRUE
            TRUE
        },

        clear_frame = function() {
            n <- min(private$painted_lines, shift_ui_render__height())
            if (n <= 0L) {
                private$active_value <- FALSE
                return(invisible(NULL))
            }
            up <- if (n > 1L) shift_tui__tui_cursor_up(n - 1L) else ""
            erased <- paste(
                rep(paste0("\r", SHIFT_TUI_ERASE_LINE), n),
                collapse = "\n"
            )
            private$write(paste0(up, erased, up))
            private$painted_lines <- 0L
            private$active_value <- FALSE
            invisible(NULL)
        },

        draw_compact = function(line) {
            if (isTRUE(private$closed)) {
                return(FALSE)
            }
            if (!length(private$compact_id)) {
                private$compact_id <- tryCatch(
                    cli::cli_progress_bar(
                        name = "Future EPW",
                        total = NA,
                        status = line,
                        format = "{cli::pb_status}",
                        current = FALSE,
                        auto_terminate = FALSE,
                        .auto_close = FALSE
                    ),
                    error = function(e) NULL
                )
            }
            if (!length(private$compact_id)) {
                return(FALSE)
            }
            ok <- tryCatch(
                {
                    cli::cli_progress_update(
                        id = private$compact_id,
                        inc = 0L,
                        status = line,
                        force = TRUE
                    )
                    TRUE
                },
                error = function(e) FALSE
            )
            private$active_value <- isTRUE(ok)
            ok
        },

        teardown = function(result, preserve = FALSE) {
            if (isTRUE(private$closed)) {
                return(invisible(NULL))
            }
            if (identical(private$backend_value, "compact")) {
                private$close_compact(
                    if (identical(result, "done")) "done" else "failed"
                )
            } else {
                if (isTRUE(preserve)) {
                    if (isTRUE(private$frame_compacted)) {
                        private$clear_frame()
                        private$write(paste0(
                            paste(private$last_frame, collapse = "\n"),
                            "\n"
                        ))
                    } else if (isTRUE(private$active_value)) {
                        private$write("\n")
                    }
                } else {
                    private$clear_frame()
                }
                if (isTRUE(private$cursor_hidden)) {
                    try(cli::ansi_show_cursor(private$output), silent = TRUE)
                    private$cursor_hidden <- FALSE
                }
            }
            private$closed <- TRUE
            private$active_value <- FALSE
            private$painted_lines <- 0L
            private$last_frame <- character()
            private$last_compact <- NULL
            invisible(NULL)
        },

        close_compact = function(result = "done") {
            if (length(private$compact_id)) {
                try(
                    cli::cli_progress_done(
                        id = private$compact_id,
                        result = result
                    ),
                    silent = TRUE
                )
            }
            private$compact_id <- NULL
            private$active_value <- FALSE
            invisible(NULL)
        }
    )
)
# }}}

# shift_tui__ui_renderer_backend {{{
shift_tui__ui_renderer_backend <- function(
    output = cli::cli_output_connection()
) {
    ansi <- tryCatch(
        isTRUE(base::isatty(output)) && isTRUE(cli::is_ansi_tty(output)),
        error = function(e) FALSE
    )
    if (ansi) {
        return("frame")
    }
    dyn <- tryCatch(isTRUE(cli::is_dynamic_tty(output)), error = function(e) {
        FALSE
    })
    if (dyn) "compact" else "log"
}
# }}}

# shift_tui__ui_renderer {{{
shift_tui__ui_renderer <- function(
    mode = c("dynamic", "log", "none"),
    output = cli::cli_output_connection(),
    backend = NULL,
    writer = NULL
) {
    if (!identical(match.arg(mode), "dynamic")) {
        return(NULL)
    }
    backend <- shift_stage__coalesce(
        backend,
        shift_tui__ui_renderer_backend(output)
    )
    if (identical(backend, "log")) {
        return(NULL)
    }
    ShiftFrameRenderer$new(output = output, backend = backend, writer = writer)
}
# }}}

# shift_tui__watch_loop {{{
shift_tui__watch_loop <- function(
    poll,
    is_done,
    render_view,
    print_view,
    ui = shift_ui(),
    interval = 1,
    event_delta = NULL,
    initial_cursor = NA_character_,
    gap_message = paste(
        "Some older workflow events are no longer available",
        "in the live buffer; continuing from its oldest event."
    ),
    on_interrupt = NULL,
    on_poll = NULL,
    max_polls = Inf,
    animate = TRUE,
    now_fn = shift_job__watch_now,
    sleep_fn = shift_job__watch_sleep
) {
    session <- shift_ui_render__open_session(ui)
    mode <- session$mode
    motion <- session$motion
    renderer <- session$renderer
    close_renderer <- function(result = "done") {
        if (!is.null(renderer)) {
            renderer$close(result = result)
            renderer <<- NULL
        }
    }
    on.exit(close_renderer(), add = TRUE)
    sleep_for <- function() {
        if (isTRUE(animate) && identical(mode, "dynamic")) {
            if (identical(motion, "full")) {
                return(ui@refresh)
            }
            if (identical(motion, "reduced")) return(max(1, ui@refresh))
        }
        interval
    }
    frame <- 0L
    polls <- 0L
    first <- TRUE
    last_poll <- as.POSIXct(NA)
    cursor <- initial_cursor
    cursor_init <- FALSE
    snapshot <- NULL
    tryCatch(
        repeat {
            now <- now_fn()
            due <- first ||
                is.na(last_poll) ||
                as.numeric(difftime(now, last_poll, units = "secs")) >= interval
            if (due) {
                polls <- polls + 1L
                snapshot <- poll(polls)
                last_poll <- now
            }
            done <- isTRUE(is_done(snapshot, polls))
            if (due && is.function(on_poll)) {
                on_poll(snapshot, first = first, done = done)
            }
            if (identical(mode, "log") && due && is.function(event_delta)) {
                delta <- event_delta(snapshot, cursor, initial = !cursor_init)
                if (cursor_init) {
                    if (isTRUE(delta$gap) && !is.null(gap_message)) {
                        cli::cli_alert_info(gap_message)
                    }
                    for (i in seq_len(nrow(delta$rows))) {
                        ev_line <- shift_ui_view__ui_persisted_event_line(
                            delta$rows[i],
                            detail = ui@detail
                        )
                        cli::cli_text("{ev_line}")
                    }
                }
                cursor <- delta$cursor
                cursor_init <- TRUE
            }
            if (done) {
                close_renderer("done")
                if (!identical(mode, "none")) {
                    print_view(snapshot, final = TRUE)
                }
                break
            }
            frame <- frame + 1L
            if (identical(mode, "dynamic")) {
                view <- render_view(snapshot, motion = motion, frame = frame)
                if (
                    !isTRUE(renderer$draw(view$lines, compact = view$compact))
                ) {
                    close_renderer("failed")
                    mode <- "log"
                    motion <- "none"
                    print_view(snapshot, final = FALSE)
                    if (is.function(event_delta)) {
                        cursor <- event_delta(
                            snapshot,
                            cursor,
                            initial = TRUE
                        )$cursor
                        cursor_init <- TRUE
                    }
                }
            } else if (identical(mode, "log") && due && first) {
                print_view(snapshot, final = FALSE)
            }
            first <- FALSE
            if (!is.infinite(max_polls) && polls >= max_polls) {
                if (identical(mode, "dynamic")) {
                    close_renderer("done")
                    print_view(snapshot, final = TRUE)
                }
                break
            }
            sleep_fn(sleep_for())
        },
        interrupt = function(e) {
            close_renderer("cancelled")
            if (is.function(on_interrupt)) on_interrupt(mode)
        }
    )
    invisible(snapshot)
}
# }}}

# }}}

# Single-Run Dashboard Views & Tables {{{

# shift_ui_view__ui_plan_lines {{{
shift_ui_view__ui_plan_lines <- function(
    plan_context,
    width = shift_ui_render__width()
) {
    width <- shift_ui_render__width(width)
    items <- shift_stage__coalesce(plan_context$items, character())
    if (!length(items)) {
        line <- as.character(shift_stage__coalesce(
            plan_context$line,
            "Workflow context unavailable"
        ))[[1L]]
        items <- strsplit(line, " \u00b7 ", fixed = TRUE)[[1L]]
    }
    if (width <= 9L) {
        return(c(
            shift_ui_render__fit(
                shift_ui_render__labeled_line("Plan", ""),
                width
            ),
            unlist(
                lapply(items, shift_ui_render__wrap_lines, width = width),
                use.names = FALSE
            )
        ))
    }
    vals <- shift_ui_render__pack_items(items, width - 9L)
    vapply(
        seq_along(vals),
        function(i) {
            shift_ui_render__labeled_line(
                if (i == 1L) "Plan" else "",
                vals[[i]]
            )
        },
        character(1L)
    )
}
# }}}

# shift_ui_view__ui_plan_summary {{{
shift_ui_view__ui_plan_summary <- function(
    plan,
    run_id,
    background = FALSE,
    width = shift_ui_render__width(),
    detail = "normal"
) {
    f <- shift_ui_state__plan_fields(plan)
    model <- shift_stage__coalesce(
        shift_print__display_values(f$request$source),
        "<model>"
    )
    scens <- shift_stage__coalesce(
        shift_print__display_values(f$request$experiment),
        "<scenario>"
    )
    status <- if (isTRUE(background)) "QUEUED" else "STARTING"
    n_exp <- nrow(f$expected_cases)
    t_line <- if (identical(f$transform@output_type, "multi_year")) {
        sprintf(
            "%s \u00b7 %d case(s); one EPW per weather year",
            f$transform@label,
            n_exp
        )
    } else {
        sprintf(
            "%s \u00b7 %s \u00b7 %d expected output(s)",
            f$transform@label,
            shift_ui_view__ui_reference(f$meta$reference),
            n_exp
        )
    }
    lines <- c(
        shift_ui_render__fit(
            sprintf("Future EPW \u00b7 %s \u00b7 %s", run_id, status),
            width
        ),
        shift_ui_render__fit(
            sprintf(
                "%s \u00b7 %s \u00b7 %s",
                model,
                scens,
                shift_ui_view__ui_periods(f$meta$periods)
            ),
            width
        ),
        shift_ui_render__fit(t_line, width),
        shift_ui_render__prefixed_lines(
            "Selection ",
            shift_ui_view__ui_selection(plan),
            width
        ),
        shift_ui_render__fit(
            paste("Output", shift_print__display_path(f$output_dir)),
            width
        ),
        if (!is.null(f$meta$observed_reference)) {
            shift_ui_render__labeled_lines(
                "Observed",
                shift_ui_view__ui_reference(f$meta$observed_reference),
                width
            )
        }
    )
    if (!identical(detail, "normal")) {
        opts <- shift_print__format_options(unclass(f$transform@options))
        policy <- if (isTRUE(f$control@allow_partial)) {
            "partial cases allowed"
        } else {
            "all cases required"
        }
        lines <- c(
            lines,
            if (!is.null(opts)) {
                shift_ui_render__labeled_lines("Options", opts, width)
            },
            shift_ui_render__fit(
                sprintf(
                    "Policy %s \u00b7 store %s",
                    policy,
                    shift_print__display_path(plan@store_path)
                ),
                width
            )
        )
    }
    lines
}
# }}}

# shift_ui_view__ui_query_lines {{{
shift_ui_view__ui_query_lines <- function(
    state,
    width = shift_ui_render__width()
) {
    d <- shift_stage__coalesce(state$current_details, list())
    if (is.null(d$request_started_at)) {
        return(character())
    }
    now <- shift_stage__coalesce(state$now_seconds, as.numeric(Sys.time()))
    active <- isTRUE(d$transfer_state %in% c("started", "transfer"))
    req_t <- if (active) {
        now - d$request_started_at
    } else {
        shift_stage__coalesce(d$request_seconds, 0)
    }
    parts <- c(
        paste(
            if (active) "waiting" else "response in",
            shift_ui_view__format_elapsed(req_t)
        ),
        if (isTRUE(d$query_timeout > 0) && active) {
            paste("timeout", shift_ui_view__format_elapsed(d$query_timeout))
        },
        if (!is.null(d$last_response_at) && active) {
            paste(
                "last response",
                shift_ui_view__format_elapsed(now - d$last_response_at),
                "ago"
            )
        }
    )
    counts <- c(
        sprintf("%d responses", shift_stage__coalesce(d$responses, 0L)),
        sprintf("%d cached", shift_stage__coalesce(d$cache_hits, 0L)),
        sprintf(
            "%d %s catalog records received",
            shift_stage__coalesce(d$records_received, 0L),
            shift_stage__coalesce(d$catalog_role, "")
        )
    )
    c(
        shift_ui_render__labeled_lines(
            "Request",
            paste(parts, collapse = " \u00b7 "),
            width
        ),
        shift_ui_render__labeled_lines(
            "Received",
            paste(counts, collapse = " \u00b7 "),
            width
        )
    )
}
# }}}

# Shared helpers for single-run status and compact renderers.
# shift_ui_render__is_calibration {{{
shift_ui_render__is_calibration <- function(details) {
    isTRUE(
        details$unit_type %in% c("reanalysis_variable", "reanalysis_request")
    )
}
# }}}

# shift_ui_render__status_marker {{{
shift_ui_render__status_marker <- function(status, include_queued = FALSE) {
    known <- c(
        "failed",
        "cancelled",
        "partial",
        "stopping",
        "waiting",
        "completed"
    )
    if (isTRUE(include_queued)) {
        known <- c("queued", known)
    }
    if (!status %in% known) {
        return("running")
    }
    if (identical(status, "waiting")) "completed" else status
}
# }}}

# shift_ui_view__ui_metric_line {{{
shift_ui_view__ui_metric_line <- function(
    state,
    width = shift_ui_render__width()
) {
    width <- shift_ui_render__width(width)
    d <- shift_stage__coalesce(state$current_details, list())
    stage <- as.character(shift_stage__coalesce(state$stage, "planned"))[[1L]]
    prog <- shift_ui_state__ui_progress_values(state)
    cur <- prog$current
    tot <- prog$total
    ctx <- shift_stage__coalesce(state$plan_context, list())
    row <- function(label, ...) {
        shift_ui_render__labeled_lines(
            label,
            paste(c(...), collapse = " \u00b7 "),
            width
        )
    }
    det <- function(a = cur, b = tot) shift_ui_render__determinate(a, b, width)

    if (identical(d$unit_type, "catalog")) {
        return(shift_ui_view__ui_query_lines(state, width))
    }
    if (identical(d$unit_type, "epw_summary")) {
        return(row("EPWs", shift_stage__coalesce(det(), "Reading local files")))
    }
    if (shift_ui_render__is_calibration(d)) {
        req_id <- if (!is.null(d$request_id)) paste("request", d$request_id)
        return(row("Variables", det(), req_id, d$status))
    }
    if (identical(stage, "resolve")) {
        att <- if (!is.na(cur) && !is.na(tot)) {
            sprintf("node %d of %d", as.integer(cur), as.integer(tot))
        } else {
            "checking catalogs"
        }
        return(row("Status", att, ctx$selection))
    }
    if (
        identical(d$unit_type, "download_session") ||
            identical(stage, "download")
    ) {
        bd <- shift_ui_view__ui_metric_number(d, "bytes_done")
        bt <- shift_ui_view__ui_metric_number(d, "bytes_total")
        spd <- shift_ui_view__ui_metric_number(d, "speed_bps")
        act <- shift_ui_view__ui_metric_number(d, "active_task_count")
        bytes_str <- if (!is.na(bd)) {
            if (is.na(bt)) {
                shift_ui_view__ui_bytes(bd)
            } else {
                sprintf(
                    "%s/%s",
                    shift_ui_view__ui_bytes(bd),
                    shift_ui_view__ui_bytes(bt)
                )
            }
        }
        return(row(
            "Transfer",
            det(),
            bytes_str,
            if (!is.na(spd) && spd > 0) {
                paste0(shift_ui_view__ui_bytes(spd), "/s")
            },
            shift_ui_view__ui_eta(shift_ui_view__ui_metric_number(
                d,
                "eta_seconds"
            )),
            if (!is.na(act) && act > 0) sprintf("%d active", as.integer(act))
        ))
    }
    if (stage %in% c("extract_future", "extract_reference")) {
        return(row("Plans", det(), shift_ui_state__chr1(d$access_method)))
    }
    ready <- as.integer(shift_stage__coalesce(state$cases_ready, 0L))
    total <- as.integer(shift_stage__coalesce(state$cases_total, 0L))
    outs <- as.integer(shift_stage__coalesce(state$outputs_completed, 0L))
    if (identical(stage, "coverage")) {
        return(row(
            "Cases",
            det(ready, total),
            sprintf("ready %d", ready),
            sprintf("missing %d", max(0L, total - ready))
        ))
    }
    if (identical(stage, "morph")) {
        c_val <- if (!is.na(cur)) as.integer(cur) else 0L
        t_val <- if (!is.na(tot)) as.integer(tot) else ready
        return(row(
            "Cases",
            shift_stage__coalesce(
                det(c_val, t_val),
                sprintf("%d/%d", c_val, t_val)
            )
        ))
    }
    if (identical(stage, "write_epw")) {
        c_val <- if (!is.na(cur)) as.integer(cur) else outs
        t_val <- if (!is.na(tot)) as.integer(tot) else total
        return(row(
            "EPWs",
            det(c_val, t_val),
            sprintf("exported %d files", outs)
        ))
    }
    if (is.null(ctx$selection)) character() else row("Status", ctx$selection)
}
# }}}

SHIFT_UI_RAIL_SHORT <- c(
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

# shift_ui_render__stage_rail {{{
shift_ui_render__stage_rail <- function(
    state,
    width = shift_ui_render__width(),
    motion = "none",
    frame = 0L
) {
    width <- shift_ui_render__width(width)
    seq_s <- as.character(shift_stage__coalesce(
        state$stage_sequence,
        character()
    ))
    cur <- as.character(shift_stage__coalesce(state$stage, "planned"))[[1L]]
    if (!length(seq_s)) {
        seq_s <- unique(c(
            cur,
            as.character(shift_stage__coalesce(state$next_stage, character()))
        ))
    }
    seq_s <- shift_ui_state__nonempty(seq_s)
    if (!length(seq_s)) {
        return(shift_ui_render__labeled_lines(
            "Flow",
            "Waiting for workflow stages",
            width
        ))
    }
    labels <- vapply(
        seq_s,
        function(s) {
            if (s %in% names(SHIFT_UI_RAIL_SHORT)) {
                SHIFT_UI_RAIL_SHORT[[s]]
            } else {
                shift_ui_view__ui_stage_label(s)
            }
        },
        character(1L)
    )
    done <- as.character(shift_stage__coalesce(
        state$completed_stages,
        character()
    ))
    term <- as.character(shift_stage__coalesce(state$status, "running"))[[1L]]
    vals <- vapply(
        seq_along(seq_s),
        function(i) {
            st <- seq_s[[i]]
            ss <- if (st %in% done || identical(term, "completed")) {
                "completed"
            } else if (identical(st, cur)) {
                if (term %in% c("failed", "cancelled", "partial", "stopping")) {
                    term
                } else {
                    "current"
                }
            } else {
                "pending"
            }
            lbl <- if (identical(ss, "completed")) {
                cli::col_green(labels[[i]])
            } else if (identical(ss, "pending")) {
                cli::style_dim(labels[[i]])
            } else {
                cli::style_bold(shift_ui_render__colorize(ss, labels[[i]]))
            }
            paste(shift_ui_render__state_symbol(ss, "none", frame), lbl)
        },
        character(1L)
    )
    full <- shift_ui_render__labeled_line(
        "Flow",
        paste(vals, collapse = cli::style_dim("  \u203a  "))
    )
    if (cli::ansi_nchar(full, type = "width") <= width) {
        return(full)
    }
    pos <- match(cur, seq_s)
    if (is.na(pos)) {
        pos <- 1L
    }
    nxt <- if (pos < length(vals)) {
        paste("next", vals[[pos + 1L]])
    } else {
        "final stage"
    }
    rows <- vapply(
        c(
            sprintf(
                "[%d/%d] %s \u00b7 %s",
                pos,
                length(seq_s),
                vals[[pos]],
                nxt
            ),
            sprintf("[%d/%d] %s", pos, length(seq_s), vals[[pos]]),
            vals[[pos]]
        ),
        function(v) shift_ui_render__labeled_line("Flow", v),
        character(1L)
    )
    fit <- which(cli::ansi_nchar(rows, type = "width") <= width)
    if (length(fit)) {
        rows[[fit[[1L]]]]
    } else {
        shift_ui_render__fit(rows[[length(rows)]], width)
    }
}
# }}}

# shift_ui_render__recent_lines {{{
shift_ui_render__recent_lines <- function(
    state,
    width = shift_ui_render__width()
) {
    vals <- shift_ui_state__nonempty(shift_stage__coalesce(
        state$recent_events,
        shift_stage__coalesce(state$last_event, character())
    ))
    vals <- utils::tail(vals, 2L)
    outs <- as.character(shift_stage__coalesce(
        state$recent_outcomes,
        character()
    ))
    if (length(outs) < length(vals)) {
        outs <- c(rep("completed", length(vals) - length(outs)), outs)
    }
    outs <- utils::tail(outs, length(vals))
    if (!length(vals)) {
        vals <- "No completed activity yet"
        outs <- "pending"
    }
    body <- unlist(
        lapply(seq_along(vals), function(i) {
            pfx <- paste0(
                "  ",
                shift_ui_render__state_symbol(outs[[i]], "none"),
                " "
            )
            shift_ui_render__prefixed_lines(pfx, vals[[i]], width)
        }),
        use.names = FALSE
    )
    c(
        shift_ui_render__fit(
            shift_ui_render__labeled_line("Recent", ""),
            width
        ),
        body,
        rep("", max(0L, 2L - length(body)))
    )
}
# }}}

# shift_ui_render__result_lines {{{
shift_ui_render__result_lines <- function(
    state,
    width = shift_ui_render__width()
) {
    outs <- as.integer(shift_stage__coalesce(state$outputs_completed, 0L))
    tot <- as.integer(shift_stage__coalesce(state$cases_total, outs))
    if (is.na(tot) || tot < outs) {
        tot <- outs
    }
    summary <- shift_stage__coalesce(
        state$result_summary,
        sprintf(
            "%d/%d EPW%s exported \u00b7 %d missing",
            outs,
            tot,
            if (tot == 1L) "" else "s",
            max(0L, tot - outs)
        )
    )
    out_dir <- shift_ui_state__chr1(shift_stage__coalesce(
        state$output_dir,
        state$plan_context$output
    ))
    paths <- unique(shift_ui_state__nonempty(shift_stage__coalesce(
        state$output_paths,
        character()
    )))
    lim <- suppressWarnings(as.numeric(shift_stage__coalesce(
        state$output_path_limit,
        5L
    ))[[1L]])
    if (!length(lim) || is.na(lim) || lim < 1) {
        lim <- 5L
    }
    shown <- if (is.finite(lim)) utils::head(paths, as.integer(lim)) else paths
    omitted <- length(paths) - length(shown)
    c(
        shift_ui_render__labeled_lines("Summary", summary, width),
        if (!is.null(out_dir)) {
            shift_ui_render__labeled_lines(
                "Output",
                shift_print__display_path(out_dir),
                width
            )
        },
        unlist(
            lapply(seq_along(shown), function(i) {
                shift_ui_render__labeled_lines(
                    if (i == 1L) "Files" else "",
                    basename(shown[[i]]),
                    width
                )
            }),
            use.names = FALSE
        ),
        if (omitted > 0L) {
            shift_ui_render__labeled_lines(
                "",
                sprintf(
                    "\u2026 %d more output%s",
                    omitted,
                    if (omitted == 1L) "" else "s"
                ),
                width
            )
        },
        if (!is.null(state$field_summary)) {
            shift_ui_render__labeled_lines("Fields", state$field_summary, width)
        },
        unlist(
            lapply(utils::head(state$warning_messages, 3L), function(m) {
                shift_ui_render__labeled_lines("Warning", m, width)
            }),
            use.names = FALSE
        )
    )
}
# }}}

# shift_ui_render__failure_lines {{{
shift_ui_render__failure_lines <- function(
    state,
    width = shift_ui_render__width()
) {
    failure <- shift_stage__coalesce(state$failure_details, list())
    f <- shift_ui_state__failure_fields(failure)
    counts <- c(
        if (f$coverage) sprintf("%d incomplete", f$coverage),
        if (f$timeout) sprintf("%d timeout", f$timeout),
        if (f$network) sprintf("%d network", f$network),
        if (f$other) sprintf("%d other", f$other)
    )
    summary <- if (f$checked > 0L) {
        paste(
            c(
                sprintf("%d checked", f$checked),
                counts,
                sprintf("%d usable", f$usable)
            ),
            collapse = " \u00b7 "
        )
    } else {
        shift_stage__coalesce(failure$kind, "workflow failed")
    }
    reason <- as.character(shift_stage__coalesce(
        failure$cause,
        shift_stage__coalesce(
            failure$summary,
            shift_stage__coalesce(state$last_event, "Workflow failed")
        )
    ))[[1L]]
    ev <- c(
        if (length(f$identity)) {
            paste("Closest", paste(f$identity, collapse = "/"))
        },
        if (length(f$missing)) paste("Missing", f$missing[[1L]])
    )
    if (!length(ev)) {
        ev <- "Inspect the persisted run for complete diagnostics"
    }
    c(
        shift_ui_render__labeled_lines("Summary", summary, width),
        shift_ui_render__prefixed_lines(
            paste0("  ", shift_ui_render__state_symbol("failed", "none"), " "),
            reason,
            width
        ),
        shift_ui_render__prefixed_lines(
            paste0("  ", cli::col_blue("i"), " "),
            paste(ev, collapse = " \u00b7 "),
            width
        )
    )
}
# }}}

# shift_ui_render__node_result_short {{{
shift_ui_render__node_result_short <- function(row) {
    outcome <- as.character(shift_stage__coalesce(row$outcome, "rejected"))[[
        1L
    ]]
    result <- as.character(shift_stage__coalesce(row$result, outcome))[[1L]]
    if (outcome %in% c("completed", "skipped", "reused")) {
        return(result)
    }
    fut <- suppressWarnings(as.numeric(shift_stage__coalesce(
        row$future,
        NA_real_
    )))
    ref <- suppressWarnings(as.numeric(shift_stage__coalesce(
        row$reference,
        NA_real_
    )))
    if (!is.na(ref) && ref == 0 && !is.na(fut) && fut > 0) {
        return("no reference files")
    }
    if (!is.na(fut) && fut == 0) {
        return("no future files")
    }
    switch(
        shift_ui_view__ui_error_kind(result),
        timeout = "request timed out",
        network = "network error",
        coverage = "incomplete coverage",
        ambiguity = "ambiguous selection",
        shift_ui_render__fit(shift_print__error_summary(result), 48L)
    )
}
# }}}

# shift_ui_render__live_node_lines {{{
shift_ui_render__live_node_lines <- function(
    state,
    width = shift_ui_render__width(),
    motion = "none",
    frame = 0L
) {
    rows <- data.table::as.data.table(shift_stage__coalesce(
        state$node_rows,
        data.table::data.table()
    ))
    attempts <- nrow(rows)
    vals <- character()
    if (attempts) {
        rows <- utils::tail(rows, 2L)
        vals <- unlist(
            lapply(seq_len(nrow(rows)), function(i) {
                out <- if ("outcome" %in% names(rows)) {
                    as.character(shift_stage__coalesce(
                        rows$outcome[[i]],
                        "rejected"
                    ))
                } else {
                    "rejected"
                }
                dur <- if ("duration" %in% names(rows)) {
                    shift_ui_state__chr1(rows$duration[[i]])
                }
                cnts <- c(
                    if ("future" %in% names(rows) && !is.na(rows$future[[i]])) {
                        sprintf("%d future", rows$future[[i]])
                    },
                    if (
                        "reference" %in%
                            names(rows) &&
                            !is.na(rows$reference[[i]])
                    ) {
                        sprintf("%d reference", rows$reference[[i]])
                    },
                    if (!is.null(dur) && !identical(dur, "\u2014")) dur
                )
                sfx <- if (length(cnts)) {
                    paste0(" \u00b7 ", paste(cnts, collapse = " \u00b7 "))
                } else {
                    ""
                }
                pfx <- sprintf(
                    "  %s %-6s ",
                    shift_ui_render__state_symbol(out, "none", frame),
                    rows$node[[i]]
                )
                shift_ui_render__prefixed_lines(
                    pfx,
                    paste0(
                        shift_ui_render__node_result_short(as.list(rows[i])),
                        sfx
                    ),
                    width
                )
            }),
            use.names = FALSE
        )
    }
    vals <- c(vals, rep("", max(0L, 2L - length(vals))))
    d <- shift_stage__coalesce(state$current_details, list())
    cur <- shift_ui_view__ui_metric_number(d, "current", 0)
    tot <- shift_ui_view__ui_metric_number(d, "total", NA_real_)
    hd <- if (!is.na(tot) && tot > 0) {
        sprintf(
            "%d tried \u00b7 %d remaining",
            attempts,
            max(0L, as.integer(tot) - as.integer(cur))
        )
    } else {
        sprintf("%d tried", attempts)
    }
    c(
        shift_ui_render__fit(
            shift_ui_render__labeled_line("Attempts", hd),
            width
        ),
        vapply(vals, shift_ui_render__fit, character(1L), width = width)
    )
}
# }}}

# shift_ui_render__current_context {{{
shift_ui_render__current_context <- function(details, suffix_catalog = TRUE) {
    node <- shift_ui_state__chr1(details$node)
    role <- shift_ui_state__chr1(details$catalog_role)
    c(
        if (!is.null(node)) shift_ui_view__node_label(node),
        if (!is.null(role)) {
            if (isTRUE(suffix_catalog) && role %in% c("future", "reference")) {
                paste(role, "catalog")
            } else {
                role
            }
        }
    )
}
# }}}

# shift_ui_view__ui_status_lines {{{
shift_ui_view__ui_status_lines <- function(
    state,
    width = shift_ui_render__width(),
    motion = c("none", "full", "reduced"),
    frame = 0L
) {
    motion <- match.arg(motion)
    if (
        identical(state$batch_context$kind, "discovery") &&
            identical(state$stage, "discovery")
    ) {
        return(shift_batch_ui__discovery_lines(state, width, motion, frame))
    }
    lay <- shift_ui_render__layout(width)
    cw <- lay$content_width
    status <- as.character(shift_stage__coalesce(state$status, "running"))[[1L]]
    ctx <- shift_stage__coalesce(state$plan_context, list())
    task_label <- as.character(shift_stage__coalesce(
        state$task_label,
        shift_stage__coalesce(ctx$title, "Future EPW")
    ))[[1L]]
    header <- shift_ui_render__fit(
        shift_ui_render__panel_header(
            task_label,
            status,
            stage = state$stage,
            elapsed = shift_stage__coalesce(state$elapsed_seconds, 0),
            run_id = state$run_id,
            detail = state$detail
        ),
        lay$width
    )
    plan_lines <- shift_ui_view__ui_plan_lines(ctx, width = cw)
    if (
        length(ctx$items) && all(ctx$items %in% c(task_label, "input request"))
    ) {
        plan_lines <- character()
    }
    batch <- shift_stage__coalesce(state$batch_context, list())
    if (length(batch)) {
        bline <- if (identical(batch$kind, "discovery")) {
            shift_ui_render__labeled_lines("Discovery", batch$message, cw)
        } else {
            shift_ui_render__labeled_lines(
                "Batch",
                sprintf(
                    "%s \u00b7 child %d/%d \u00b7 %d completed \u00b7 %d failed",
                    batch$id,
                    batch$current,
                    batch$total,
                    batch$completed,
                    batch$failed
                ),
                cw
            )
        }
        plan_lines <- c(bline, plan_lines)
    }
    d <- shift_stage__coalesce(state$current_details, list())
    cur_ctx <- shift_ui_render__current_context(d, suffix_catalog = TRUE)
    cur_lbl <- shift_stage__coalesce(
        state$unit_label,
        shift_stage__coalesce(state$stage_message, "Waiting")
    )
    if (length(cur_ctx)) {
        cur_lbl <- paste(c(cur_ctx, cur_lbl), collapse = " \u00b7 ")
    }
    cur_stat <- shift_ui_render__status_marker(status, include_queued = TRUE)
    cur_name <- if (identical(status, "failed")) {
        "Failure"
    } else if (identical(status, "cancelled")) {
        "Stopped"
    } else if (shift_ui_render__is_calibration(d)) {
        "Calibration"
    } else {
        "Now"
    }
    current <- shift_ui_render__labeled_lines(
        cur_name,
        sprintf(
            "%s %s",
            shift_ui_render__state_symbol(cur_stat, motion, frame),
            cli::style_bold(cur_lbl)
        ),
        cw
    )
    metrics <- shift_ui_view__ui_metric_line(state, width = cw)
    term_prob <- status %in% c("failed", "cancelled")
    term_res <- status %in% c("completed", "partial", "waiting")
    context <- if (term_prob) {
        shift_ui_render__failure_lines(state, cw)
    } else if (term_res) {
        shift_ui_render__result_lines(state, cw)
    } else if (identical(state$stage, "resolve")) {
        shift_ui_render__live_node_lines(state, cw, motion, frame)
    } else {
        shift_ui_render__recent_lines(state, cw)
    }
    plan_lines <- vapply(
        plan_lines,
        shift_ui_render__fit,
        character(1L),
        width = cw
    )
    workflow <- vapply(
        c(
            if (length(state$stage_sequence) > 1L) {
                shift_ui_render__stage_rail(state, cw, motion, frame)
            },
            if (!term_res) current,
            if (
                !term_res ||
                    isTRUE(state$stage %in% c("coverage", "morph", "write_epw"))
            ) {
                metrics
            }
        ),
        shift_ui_render__fit,
        character(1L),
        width = cw
    )
    shift_ui_render__panel_box(
        header,
        list(
            list(title = NULL, lines = plan_lines),
            list(title = "Workflow", lines = workflow, omit_if_empty = TRUE),
            list(
                title = if (term_prob) {
                    "Diagnosis"
                } else if (term_res) {
                    "Results"
                } else {
                    "Activity"
                },
                lines = context
            )
        ),
        width = lay$width,
        panel = lay$panel
    )
}
# }}}

# shift_ui_view__ui_compact_line {{{
shift_ui_view__ui_compact_line <- function(
    state,
    width = shift_ui_render__width(),
    motion = c("none", "full", "reduced"),
    frame = 0L
) {
    motion <- match.arg(motion)
    width <- shift_ui_render__width(width)
    status <- as.character(shift_stage__coalesce(state$status, "running"))[[1L]]
    stage <- shift_ui_view__ui_stage_label(shift_stage__coalesce(
        state$stage,
        "planned"
    ))
    batch <- shift_stage__coalesce(state$batch_context, list())
    if (
        identical(batch$kind, "discovery") &&
            identical(state$stage, "discovery")
    ) {
        fin <- status %in% c("completed", "failed", "cancelled")
        summary <- if (fin) {
            shift_stage__coalesce(state$result_summary, state$unit_label)
        } else {
            q <- shift_stage__coalesce(state$current_details, list())
            paste(
                c(
                    if (!is.null(batch$current)) {
                        sprintf("method %d/%d", batch$current, batch$total)
                    },
                    batch$scope,
                    if (
                        isTRUE(q$transfer_state %in% c("started", "transfer"))
                    ) {
                        paste(
                            "wait",
                            shift_ui_view__format_elapsed(
                                as.numeric(Sys.time()) - q$request_started_at
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
        return(shift_ui_render__fit(
            paste(
                shift_ui_render__state_symbol(status, motion, frame),
                if (fin) toupper(status) else "Discovery",
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
    vals <- shift_ui_state__ui_progress_values(state)
    counter <- if (
        !identical(batch$kind, "discovery") &&
            !is.na(vals$current) &&
            !is.na(vals$total) &&
            vals$total > 0
    ) {
        sprintf("%d/%d", as.integer(vals$current), as.integer(vals$total))
    }
    d <- shift_stage__coalesce(state$current_details, list())
    if (shift_ui_render__is_calibration(d)) {
        stage <- "Calibration"
        if (!is.null(counter)) counter <- paste(counter, "variables")
    }
    ctx <- shift_ui_render__current_context(d, suffix_catalog = FALSE)
    unit <- as.character(shift_stage__coalesce(
        state$unit_label,
        shift_stage__coalesce(state$stage_message, "Waiting")
    ))[[1L]]
    if (
        length(ctx) &&
            !all(vapply(ctx, grepl, logical(1L), x = unit, fixed = TRUE))
    ) {
        unit <- paste(c(ctx, unit), collapse = " \u00b7 ")
    }
    marker <- shift_ui_render__status_marker(status)
    if (identical(status, "stopping")) {
        stage <- paste("STOPPING", stage)
    }
    if (identical(status, "waiting")) {
        stage <- paste(
            cli::ansi_strip(shift_ui_render__status_badge(status, state$stage)),
            stage
        )
    }
    parts <- c(
        paste(shift_ui_render__state_symbol(marker, motion, frame), stage),
        counter,
        unit,
        shift_ui_view__format_elapsed(shift_stage__coalesce(
            state$elapsed_seconds,
            0
        ))
    )
    shift_ui_render__fit(
        paste(shift_ui_state__nonempty(parts), collapse = " \u00b7 "),
        width
    )
}
# }}}

# shift_ui_view__ui_node_table {{{
shift_ui_view__ui_node_table <- function(
    rows,
    width = shift_ui_render__width(),
    detail = "normal"
) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift_ui_render__width(width)
    inc_cnt <- width >= 56L
    inc_dur <- width >= 72L && "duration" %in% names(rows)
    cols <- c(
        "Node",
        if (inc_cnt) c("Future", "Reference"),
        if (inc_dur) "Time"
    )
    sizes <- c(
        shift_ui_render__col_width("Node", rows$node, 4L, 12L),
        if (inc_cnt) c(7L, 9L),
        if (inc_dur) 7L
    )
    cols <- c(cols, "Result")
    sizes <- c(sizes, max(1L, width - 2L - sum(sizes) - 2L * length(sizes)))
    body <- vapply(
        seq_len(nrow(rows)),
        function(i) {
            res <- if (identical(detail, "normal")) {
                shift_ui_render__node_result_short(as.list(rows[i]))
            } else {
                cli::ansi_strip(rows$result[[i]])
            }
            vals <- c(
                rows$node[[i]],
                if (inc_cnt) c(rows$future[[i]], rows$reference[[i]]),
                if (inc_dur) rows$duration[[i]],
                res
            )
            shift_ui_render__table_row(vals, sizes, width)
        },
        character(1L)
    )
    lines <- c(
        cli::style_bold("Resolver attempts"),
        cli::style_dim(shift_ui_render__table_row(cols, sizes, width)),
        body
    )
    vapply(lines, shift_ui_render__fit, character(1L), width = width)
}
# }}}

# shift_ui_view__ui_case_table {{{
shift_ui_view__ui_case_table <- function(
    rows,
    width = shift_ui_render__width(),
    detail = "normal"
) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift_ui_render__width(width)
    miss_lines <- function(i, use_pfx = FALSE) {
        if (identical(detail, "normal") || !"missing_reason" %in% names(rows)) {
            return(character())
        }
        reason <- shift_ui_state__chr1(rows$missing_reason[[i]])
        if (is.null(reason)) {
            return(character())
        }
        if (use_pfx) {
            shift_ui_render__prefixed_lines("    ", reason, width)
        } else {
            paste0(
                "    ",
                cli::ansi_strwrap(reason, width = max(1L, width - 4L))
            )
        }
    }
    multi <- "method" %in%
        names(rows) ||
        ("source_id" %in%
            names(rows) &&
            data.table::uniqueN(rows$source_id) > 1L)
    if (multi) {
        cols <- intersect(
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
        if ("model" %in% cols) {
            cols <- setdiff(cols, "source_id")
        }
        body <- unlist(
            lapply(seq_len(nrow(rows)), function(i) {
                vals <- vapply(
                    cols,
                    function(col) as.character(rows[[col]][[i]]),
                    character(1L)
                )
                c(
                    shift_ui_render__prefixed_lines(
                        "  ",
                        paste(vals, collapse = " \u00b7 "),
                        width
                    ),
                    miss_lines(i, TRUE)
                )
            }),
            use.names = FALSE
        )
        return(c("Cases", body))
    }
    col_or <- function(nm, def = "\u2014") {
        if (nm %in% names(rows)) rows[[nm]] else rep(def, nrow(rows))
    }
    scen <- col_or("experiment_id")
    per <- col_or("period")
    mem <- col_or("variant_label")
    stat <- col_or("status", "unknown")
    inc_m <- width >= 68L
    sw <- shift_ui_render__col_width("Scenario", scen, 8L, 14L)
    pw <- shift_ui_render__col_width("Period", per, 6L, 12L)
    mw <- if (inc_m) shift_ui_render__col_width("Member", mem, 6L, 14L) else 0L
    cols <- c("Scenario", "Period", if (inc_m) "Member", "Status")
    sizes <- c(
        sw,
        pw,
        if (inc_m) mw,
        max(10L, width - (sw + pw + mw + if (inc_m) 10L else 7L))
    )
    body <- unlist(
        lapply(seq_len(nrow(rows)), function(i) {
            vals <- c(scen[[i]], per[[i]], if (inc_m) mem[[i]], stat[[i]])
            c(
                shift_ui_render__table_row(vals, sizes, width),
                miss_lines(i, FALSE)
            )
        }),
        use.names = FALSE
    )
    vapply(
        c("Cases", shift_ui_render__table_row(cols, sizes, width), body),
        shift_ui_render__fit,
        character(1L),
        width = width
    )
}
# }}}

# shift_ui_view__ui_table_view {{{
shift_ui_view__ui_table_view <- function(
    row,
    cases,
    events,
    width = shift_ui_render__width(),
    detail = "normal",
    motion = "none",
    frame = 0L,
    outputs = NULL,
    diagnostics = NULL,
    ui_state = NULL
) {
    state <- shift_ui_state__ui_live_state(
        row,
        shift_ui_state__ui_table_state(row, events, cases),
        ui_state
    )
    if (!is.null(outputs) && state$status %in% c("completed", "partial")) {
        comp <- shift_inspect__completion(
            cases,
            outputs,
            shift_stage__coalesce(diagnostics, shift_stage__diagnostics_empty())
        )
        state[names(comp)] <- comp
        state$outputs_completed <- nrow(outputs)
        state$output_paths <- shift_stage__coalesce(
            outputs$export_path,
            outputs$path
        )
    }
    state$output_path_limit <- if (identical(detail, "normal")) 5L else Inf
    list(
        state = state,
        lines = shift_ui_view__ui_status_lines(state, width, motion, frame),
        compact = shift_ui_view__ui_compact_line(state, width, motion, frame),
        nodes = shift_ui_view__ui_node_table(
            shift_ui_state__ui_event_nodes(events),
            width,
            detail
        ),
        cases = shift_ui_view__ui_case_table(cases, width, detail)
    )
}
# }}}

# shift_ui_view__ui_run_view {{{
shift_ui_view__ui_run_view <- function(
    run,
    width = shift_ui_render__width(),
    detail = "normal",
    motion = "none",
    frame = 0L
) {
    outputs <- if (is.null(run@store_path) || !nzchar(run@store_path)) {
        run@meta$outputs
    } else {
        tryCatch(shift_outputs(run, refresh = FALSE), error = function(e) NULL)
    }
    shift_ui_view__ui_table_view(
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
# }}}

# shift_ui_view__ui_persisted_event_line {{{
shift_ui_view__ui_persisted_event_line <- function(
    event,
    detail = "normal",
    width = NULL
) {
    details <- shift_ui_state__ui_event_details(event)[[1L]]
    pfx <- if ("method" %in% names(event)) {
        c(event$method[[1L]], event$model[[1L]])
    } else {
        character()
    }
    ctx <- shift_ui_render__event_context(
        event$stage[[1L]],
        details,
        detail,
        prefix = pfx
    )
    line <- sprintf(
        "%s [%s] %s",
        format(event$created_at[[1L]], "%F %T"),
        paste(ctx, collapse = "]["),
        event$message[[1L]]
    )
    if (is.null(width)) line else shift_ui_render__fit(line, width)
}
# }}}

# }}}

# Batch & Model-Discovery Presentation {{{

# shift_batch_ui__discover_models {{{
shift_batch_ui__discover_models <- function(
    climate,
    transforms,
    periods,
    references,
    store,
    ui,
    site = NULL
) {
    target <- if (length(climate@model)) {
        length(climate@model)
    } else {
        climate@n_models
    }
    ui@batch_context <- list(
        kind = "discovery",
        total = length(transforms),
        target_models = target,
        common = climate@common,
        site = site,
        scenarios = climate@scenarios,
        periods = shift_ui_view__ui_periods(periods)
    )
    shift_reporter__with_operation(
        ui = ui,
        task = "discovery",
        label = "Discover CMIP6 models",
        context = list(
            items = c(
                site,
                if (is.null(target)) {
                    "all compatible GCMs"
                } else {
                    paste(target, "GCMs requested")
                },
                paste(climate@scenarios, collapse = " + "),
                shift_ui_view__ui_periods(periods)
            )
        ),
        interrupt_message = "Model discovery interrupted.",
        code = function(reporter) {
            res <- shift_run__with_reporter(
                reporter,
                shift_batch__discover_candidates(
                    climate,
                    transforms,
                    periods,
                    references,
                    store,
                    ui
                )
            )
            reporter$discovery_updated(list(
                selected_models = res$identities$source_id
            ))
            reporter$operation_completed(
                if (climate@common) {
                    sprintf(
                        "%d GCM(s) selected with complete coverage across all %d method(s)",
                        nrow(res$identities),
                        length(transforms)
                    )
                } else {
                    sprintf(
                        "%d method/model selections with complete coverage in separate pools",
                        nrow(res$selection)
                    )
                }
            )
            res
        }
    )
}
# }}}

# shift_batch_ui__discovery_update {{{
shift_batch_ui__discovery_update <- function(context, reset = FALSE) {
    rep <- shift_run__current_reporter()
    if (!is.null(rep) && identical(rep$ui()@batch_context$kind, "discovery")) {
        rep$discovery_updated(context, reset = reset)
    }
    invisible(NULL)
}
# }}}

# shift_batch_ui__discovery_notice {{{
shift_batch_ui__discovery_notice <- function(message, outcome = "completed") {
    rep <- shift_run__current_reporter()
    if (!is.null(rep) && identical(rep$ui()@batch_context$kind, "discovery")) {
        rep$notice(message, outcome = outcome)
    }
    invisible(NULL)
}
# }}}

# shift_batch_ui__discovery_variables {{{
shift_batch_ui__discovery_variables <- function(variables, detail = "normal") {
    if (!length(variables)) {
        return(NULL)
    }
    if (!identical(detail, "normal")) {
        return(paste(variables, collapse = ", "))
    }
    hum <- if ("hurs" %in% variables) {
        "relative humidity (hurs)"
    } else if (all(c("huss", "ps") %in% variables)) {
        "specific humidity + pressure (huss + ps)"
    }
    paste(c(paste(length(variables), "variables"), hum), collapse = " \u00b7 ")
}
# }}}

# shift_batch_ui__discovery_lines {{{
shift_batch_ui__discovery_lines <- function(state, width, motion, frame) {
    b <- state$batch_context
    status <- shift_stage__coalesce(state$status, "running")
    term <- status %in% c("completed", "failed", "cancelled")
    lay <- shift_ui_render__layout(width)
    inner <- lay$content_width
    row <- function(label, val) {
        if (!length(val)) {
            character()
        } else {
            shift_ui_render__labeled_lines(
                label,
                paste(val, collapse = " \u00b7 "),
                inner
            )
        }
    }
    title <- shift_ui_render__panel_header(
        "Discover CMIP6 models",
        status,
        elapsed = state$elapsed_seconds,
        sep = " "
    )
    plan <- c(
        row("Site", b$site),
        row(
            "Target",
            c(
                if (is.null(b$target_models)) {
                    "all compatible GCMs"
                } else {
                    paste(b$target_models, "GCMs")
                },
                paste(length(b$scenarios), "scenarios"),
                paste(b$total, "methods")
            )
        ),
        row("Scenarios", b$scenarios),
        row("Periods", b$periods)
    )
    current <- if (!term) {
        c(
            row(
                "Method",
                if (!is.null(b$current)) {
                    sprintf("%d/%d: %s", b$current, b$total, b$method_label)
                }
            ),
            row(
                "Inputs",
                c(
                    if (!is.null(b$alternative)) {
                        sprintf(
                            "combination %d/%d",
                            b$alternative,
                            b$alternatives
                        )
                    },
                    shift_batch_ui__discovery_variables(
                        b$variables,
                        shift_stage__coalesce(state$detail, "normal")
                    )
                )
            ),
            row("Check", c(b$scope, b$scope_periods)),
            row(
                "Node",
                if (!is.null(b$node)) {
                    c(
                        shift_ui_view__node_label(b$node),
                        if (isTRUE(b$node_index > 1L)) {
                            sprintf(
                                "fallback node %d/%d",
                                b$node_index,
                                b$node_total
                            )
                        }
                    )
                }
            ),
            row(
                "Now",
                c(
                    shift_ui_render__state_symbol("running", motion, frame),
                    shift_stage__coalesce(state$unit_label, state$stage_message)
                )
            ),
            shift_ui_view__ui_query_lines(state, inner),
            row(
                "Cancel",
                if (interactive()) {
                    "Interrupt R to cancel"
                } else {
                    "Ctrl+C to cancel"
                }
            )
        )
    } else {
        character()
    }
    pool <- if (identical(b$common, FALSE)) {
        row("Pool", "Independent selection for each method")
    } else {
        row(
            "Common",
            if (is.null(b$common_models)) {
                "Pending checks across all methods"
            } else {
                paste(b$common_models, "compatible GCMs")
            }
        )
    }
    result <- if (identical(status, "completed")) {
        c(
            row("Summary", state$result_summary),
            row("Models", b$selected_models)
        )
    } else if (term) {
        shift_ui_render__failure_lines(state, inner)
    } else {
        c(pool, shift_ui_render__recent_lines(state, inner))
    }
    chrome <- if (lay$panel) 4L else 1L
    if (
        !term &&
            length(plan) + length(current) + length(result) + chrome >
                shift_ui_render__height()
    ) {
        recent <- state
        recent$recent_events <- utils::tail(state$recent_events, 1L)
        recent$recent_outcomes <- utils::tail(state$recent_outcomes, 1L)
        result <- c(pool, shift_ui_render__recent_lines(recent, inner))
    }
    shift_ui_render__panel_box(
        title,
        list(
            list(title = NULL, lines = plan),
            list(
                title = "Current search",
                lines = current,
                omit_if_empty = TRUE
            ),
            list(
                title = if (term) "Results" else "Recent checks",
                lines = result
            )
        ),
        width = lay$width,
        panel = lay$panel
    )
}
# }}}

# shift_batch_ui__diagnostic_lines {{{
shift_batch_ui__diagnostic_lines <- function(
    snapshot,
    width,
    limit = 3L,
    compact = FALSE
) {
    rows <- data.table::copy(snapshot$diagnostics)
    rows <- rows[rows$severity %in% c("error", "warning")]
    if (!nrow(rows)) {
        return(character())
    }
    rows <- rows[order(match(rows$severity, c("error", "warning")))]
    shown <- min(nrow(rows), limit)
    field <- function(l, v) shift_ui_render__field(l, v, width, compact)
    lines <- unlist(
        lapply(seq_len(shown), function(i) {
            r <- rows[i]
            act <- store__chr1(r$action)
            c(
                field(
                    if (identical(r$severity, "error")) {
                        "Failure"
                    } else {
                        "Warning"
                    },
                    paste(
                        c(r$method, r$model, r$message),
                        collapse = " \u00b7 "
                    )
                ),
                if (!is.na(act) && nzchar(act)) field("Action", act)
            )
        }),
        use.names = FALSE
    )
    if (shown < nrow(rows)) {
        lines <- c(
            lines,
            field(
                "More",
                sprintf(
                    "%d more diagnostics; use shift diagnostics",
                    nrow(rows) - shown
                )
            )
        )
    }
    lines
}
# }}}

# shift_batch_ui__child_lines {{{
shift_batch_ui__child_lines <- function(
    child,
    index,
    state,
    width,
    detail,
    compact,
    motion,
    frame
) {
    field <- function(l, v) shift_ui_render__field(l, v, width, compact)
    id <- shift_ui_state__nonempty(c(child$site_id, child$method, child$model))
    label <- sprintf(
        "%s %s [%s] \u00b7 %s",
        shift_ui_render__state_symbol(child$status, motion, frame),
        cli::style_bold(paste(id, collapse = " / ")),
        child$method_status,
        child$status
    )
    cfg <- shift_ui_state__nonempty(c(
        child$scale,
        child$reconstruction,
        if (!is.na(child$current_stage)) {
            shift_ui_view__ui_stage_label(child$current_stage)
        }
    ))
    lines <- c(
        field(sprintf("#%d", index), label),
        field("", cli::style_dim(paste(cfg, collapse = " \u00b7 ")))
    )
    active <- child$status %in% c("queued", "running", "stopping")
    if (length(state) && (active || !identical(detail, "normal"))) {
        cur <- shift_stage__coalesce(state$unit_label, state$stage_message)
        metric <- shift_ui_view__ui_metric_line(state, width)
        upd <- store__chr1(state$updated_at)
        elap <- paste(
            shift_ui_view__format_elapsed(shift_stage__coalesce(
                state$elapsed_seconds,
                0
            )),
            "elapsed"
        )
        lines <- c(
            lines,
            if (!is.null(cur)) field(if (active) "Now" else "Last", cur),
            if (compact) utils::head(metric, 1L) else metric,
            if (!is.null(state$last_event) && !identical(detail, "normal")) {
                field("Event", state$last_event)
            },
            field(
                "Updated",
                paste(c(if (!is.na(upd)) upd, elap), collapse = " \u00b7 ")
            )
        )
    }
    if (!identical(detail, "normal")) {
        vals <- shift_ui_state__nonempty(c(
            child$member,
            child$grid,
            child$run_id
        ))
        lines <- c(lines, field("Identity", paste(vals, collapse = " \u00b7 ")))
    }
    lines
}
# }}}

# shift_batch_ui__view {{{
shift_batch_ui__view <- function(
    snapshot,
    width = shift_ui_render__width(),
    detail = "normal",
    motion = "none",
    frame = 0L,
    height = Inf
) {
    s <- snapshot$batch
    lay <- shift_ui_render__layout(width)
    cw <- lay$content_width
    compact <- is.finite(height)
    field <- function(l, v) shift_ui_render__field(l, v, cw, compact)
    header <- shift_ui_render__panel_header("Future EPW Batch", s$status)
    counts <- c(
        completed = s$completed,
        active = s$active,
        failed = s$failed,
        partial = s$partial,
        waiting = s$waiting,
        cancelled = shift_stage__coalesce(s$cancelled, 0L)
    )
    if (compact) {
        counts <- counts[
            counts > 0 | names(counts) %in% c("completed", "active")
        ]
    }
    prog <- snapshot$source_progress
    overview <- c(
        field("Batch", s$batch_id),
        field(
            "Matrix",
            sprintf(
                "%d configurations \u00b7 %d models \u00b7 %d children \u00b7 %d cases",
                s$configurations,
                s$models,
                s$children,
                s$cases
            )
        ),
        field(
            "Status",
            paste(
                sprintf("%d %s", counts, names(counts)),
                collapse = " \u00b7 "
            )
        ),
        if (identical(prog$unit_type, "source_reads")) {
            field(
                "Sources",
                sprintf(
                    "%d/%d files finished; %d active",
                    prog$current,
                    prog$total,
                    prog$active
                )
            )
        }
    )
    children <- snapshot$children
    groups <- lapply(seq_len(nrow(children)), function(i) {
        ch <- children[i]
        rows <- shift_batch_ui__child_lines(
            ch,
            i,
            snapshot$activity[[ch$child_key]],
            cw,
            detail,
            compact,
            motion,
            frame
        )
        if (!identical(detail, "normal") && nrow(snapshot$outputs)) {
            comp <- shift_inspect__completion(
                data.table::data.table(),
                snapshot$outputs[snapshot$outputs$child_key == ch$child_key],
                shift_stage__diagnostics_empty()
            )
            if (!is.null(comp$field_summary)) {
                rows <- c(rows, field("Fields", comp$field_summary))
            }
        }
        rows
    })
    exec <- snapshot$execution
    diag_lim <- if (compact) {
        1L
    } else if (identical(detail, "normal")) {
        3L
    } else {
        Inf
    }
    results <- c(
        field(
            "Summary",
            sprintf("%d EPW files \u00b7 %d warnings", s$epw_files, s$warnings)
        ),
        shift_batch_ui__diagnostic_lines(
            snapshot,
            cw,
            limit = diag_lim,
            compact = compact
        ),
        if (nrow(exec)) {
            field(
                "Call",
                sprintf(
                    "%d children reused \u00b7 %d started/resumed \u00b7 %s",
                    sum(exec$action == "reused"),
                    sum(exec$action != "reused"),
                    shift_ui_view__format_elapsed(shift_stage__coalesce(
                        snapshot$call_elapsed_seconds,
                        sum(exec$elapsed_seconds, na.rm = TRUE)
                    ))
                )
            )
        },
        field("Output", s$output_dir)
    )
    omitted <- 0L
    shortened <- FALSE
    if (compact) {
        prio <- match(
            children$status,
            c(
                "failed",
                "blocked",
                "running",
                "stopping",
                "queued",
                "partial",
                "cancelled",
                "planned",
                "waiting",
                "completed"
            )
        )
        focus <- c(
            utils::head(which(children$status %in% c("failed", "blocked")), 1L),
            utils::head(
                which(children$status %in% c("running", "stopping", "queued")),
                1L
            )
        )
        indices <- unique(c(
            focus,
            order(prio, seq_len(nrow(children)), na.last = TRUE)
        ))
        rem <- max(
            0L,
            height -
                ((if (lay$panel) 4L else 1L) +
                    length(overview) +
                    length(results)) -
                1L
        )
        f_rows <- if (rem >= 2L * length(focus)) 2L else 1L
        selected <- list()
        for (pos in seq_along(indices)) {
            idx <- indices[[pos]]
            resv <- sum(match(focus, indices) > pos) * f_rows
            if (rem - resv < (if (idx %in% focus) f_rows else 2L)) {
                break
            }
            keep <- min(length(groups[[idx]]), rem - resv)
            shortened <- shortened || keep < length(groups[[idx]])
            selected[[length(selected) + 1L]] <- utils::head(
                groups[[idx]],
                keep
            )
            rem <- rem - keep
        }
        omitted <- length(groups) - length(selected)
        groups <- selected
    }
    workflows <- c(
        unlist(groups, use.names = FALSE),
        if (omitted > 0L) {
            field(
                "More",
                sprintf("%d children hidden; use shift show --verbose", omitted)
            )
        } else if (shortened) {
            field("More", "Activity shortened; use shift show --verbose")
        }
    )
    diag_mode <- s$status %in% c("failed", "blocked", "cancelled", "partial")
    lines <- shift_ui_render__panel_box(
        header,
        list(
            list(title = NULL, lines = overview),
            list(title = "Workflows", lines = workflows),
            list(
                title = if (diag_mode) "Diagnosis" else "Results",
                lines = results
            )
        ),
        width = lay$width,
        panel = lay$panel,
        wrap_header = TRUE
    )
    compact_line <- shift_ui_render__fit(
        sprintf(
            "Batch %s \u00b7 %d/%d children complete \u00b7 %d EPW files \u00b7 %d warnings",
            s$status,
            s$completed,
            s$children,
            s$epw_files,
            s$warnings
        ),
        lay$width
    )
    if (compact && length(lines) > height) {
        body <- c(
            compact_line,
            shift_batch_ui__diagnostic_lines(
                snapshot,
                cw,
                limit = 1L,
                compact = TRUE
            ),
            field("More", "Full details: shift show --verbose")
        )
        lines <- if (lay$panel && height >= 4L) {
            shift_ui_render__panel_box(
                header,
                list(list(
                    title = NULL,
                    lines = utils::head(body, height - 2L)
                )),
                width = lay$width,
                panel = TRUE
            )
        } else {
            utils::head(body, height)
        }
    }
    list(
        lines = lines,
        compact = compact_line,
        nodes = character(),
        cases = if (!identical(detail, "normal")) {
            shift_ui_view__ui_case_table(snapshot$cases, lay$width, detail)
        } else {
            character()
        }
    )
}
# }}}

# shift_batch_ui__report {{{
shift_batch_ui__report <- function(x, ui) {
    if (
        isTRUE(ui@batch_receipt) &&
            !identical(shift_ui_render__mode(ui), "none")
    ) {
        shift_ui_render__print_view(shift_batch_ui__view(
            shift_batch_ui__snapshot(x, refresh = FALSE),
            detail = ui@detail
        ))
    }
    invisible(x)
}
# }}}

# shift_batch_ui__watch {{{
shift_batch_ui__watch <- function(x, follow, interval, events, ui) {
    shift_tui__watch_loop(
        poll = function(polls) {
            x <<- shift_batch__refresh(x)
            shift_batch_ui__snapshot(x, event_count = events, refresh = FALSE)
        },
        is_done = function(snap, polls) {
            !follow ||
                (snap$batch$active == 0L &&
                    !snap$batch$status %in% c("queued", "running", "stopping"))
        },
        render_view = function(snap, motion, frame) {
            shift_batch_ui__view(
                snap,
                detail = ui@detail,
                motion = motion,
                frame = frame,
                height = shift_ui_render__height()
            )
        },
        print_view = function(snap, final = FALSE) {
            shift_ui_render__print_view(
                shift_batch_ui__view(snap, detail = ui@detail),
                include_tables = TRUE
            )
        },
        ui = ui,
        interval = interval,
        event_delta = function(snap, cursor, initial) {
            shift_batch_ui__event_delta(
                shift_stage__coalesce(
                    attr(snap, "shift_ui_events"),
                    snap$events
                ),
                cursor = cursor,
                initial_limit = events,
                initial = initial
            )
        },
        initial_cursor = list(),
        gap_message = "Older batch events are no longer available.",
        on_interrupt = function(mode) {
            if (!identical(mode, "none")) {
                cli::cli_alert_info(
                    "Stopped watching; batch children continue. Use shift_cancel() to cancel them."
                )
            }
        }
    )
    x
}
# }}}

# }}}

# Console Receipts, S7 Print Methods & Error Formatting {{{

# shift_print__print_options {{{
shift_print__print_options <- function(dots, default_n = 10L) {
    if (is.null(names(dots))) {
        names(dots) <- rep("", length(dots))
    }
    unknown <- setdiff(names(dots), c("n", "width", "verbose"))
    unknown <- unknown[nzchar(unknown)]
    if (any(!nzchar(names(dots))) || length(unknown)) {
        sup <- c(names(dots)[!nzchar(names(dots))], unknown)
        sup[!nzchar(sup)] <- "<unnamed>"
        cli::cli_abort(
            "Unsupported print argument(s): {paste(sup, collapse = ', ')}."
        )
    }
    n <- if ("n" %in% names(dots)) dots$n else default_n
    if (is.null(n)) {
        n <- Inf
    }
    checkmate::assert_number(n, lower = 1, finite = FALSE)
    if (!is.infinite(n)) {
        n <- as.integer(n)
    }
    w <- dots$width
    checkmate::assert_integerish(w, lower = 40L, len = 1L, null.ok = TRUE)
    v <- shift_stage__coalesce(dots$verbose, FALSE)
    checkmate::assert_flag(v)
    list(n = n, width = if (is.null(w)) NULL else as.integer(w), verbose = v)
}
# }}}

# shift_print__print_time {{{
shift_print__print_time <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NULL)
    }
    if (inherits(x[[1L]], "POSIXt")) {
        format(x[[1L]], tz = Sys.timezone(), usetz = TRUE)
    } else {
        as.character(x[[1L]])
    }
}
# }}}

# shift_print__col_sum {{{
shift_print__col_sum <- function(dt, col) {
    if (col %in% names(dt)) sum(dt[[col]], na.rm = TRUE) else 0
}
# }}}

# shift_print__cached_or_store {{{
shift_print__cached_or_store <- function(x, cached, reader) {
    rows <- if (is.data.frame(cached)) {
        data.table::as.data.table(cached)
    } else {
        data.table::data.table()
    }
    if (nrow(rows)) {
        return(list(data = rows, error = NULL))
    }
    opened <- tryCatch(shift_inspect__store(x), error = identity)
    if (inherits(opened, "condition")) {
        return(list(
            data = data.table::data.table(),
            error = conditionMessage(opened)
        ))
    }
    on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    val <- tryCatch(reader(opened), error = identity)
    if (inherits(val, "condition")) {
        return(list(
            data = data.table::data.table(),
            error = conditionMessage(val)
        ))
    }
    list(data = data.table::as.data.table(val), error = NULL)
}
# }}}

# shift_print__print_table {{{
shift_print__print_table <- function(
    x,
    title,
    columns,
    n = 10L,
    total_rows = NULL,
    empty = "No rows.",
    more_hint = "use the corresponding shift_*() inspector for all rows.",
    hidden_hint = "Use the corresponding shift_*() inspector for all columns."
) {
    checkmate::assert_string(title, min.chars = 1L)
    x <- data.table::as.data.table(shift_stage__coalesce(
        x,
        data.table::data.table()
    ))
    if (is.null(total_rows)) {
        total_rows <- nrow(x)
    }
    cli::cli_rule(title)
    if (!nrow(x)) {
        cli::cli_alert_info(empty)
        return(invisible(NULL))
    }
    shown <- if (is.infinite(n)) x else utils::head(x, n)
    epwshiftr_cli_render_table(
        shown,
        columns = columns,
        max_rows = if (is.infinite(n)) nrow(shown) else n,
        show_types = FALSE,
        more_hint = more_hint,
        hidden_hint = hidden_hint,
        total_rows = as.integer(total_rows)
    )
    invisible(NULL)
}
# }}}

# shift_print__print_stage_intro {{{
shift_print__print_stage_intro <- function(x, title, facts = list()) {
    esg__print_header(title)
    esg__print_facts(c(
        list(
            "Status" = tryCatch(shift_status(x), error = function(e) "unknown")
        ),
        facts
    ))
    invisible(NULL)
}
# }}}

# shift_print__print_workflow {{{
shift_print__print_workflow <- function(x, verbose = FALSE) {
    ids <- shift_inspect__ids(x)
    diags <- shift_diagnostics(x)
    if (isTRUE(verbose)) {
        cli::cli_rule("Workflow")
        esg__print_facts(list(
            "Status" = tryCatch(shift_status(x), error = function(e) "unknown"),
            "Store" = shift_print__display_path(x@store_path),
            "Query ID" = ids$query_id,
            "Run ID" = ids$run_id,
            "Step ID" = ids$step_id
        ))
    }
    if (nrow(diags)) {
        cnt <- table(diags$severity)
        cli::cli_rule("Diagnostics")
        esg__print_facts(list(
            "Counts" = paste(
                sprintf("%s %s", cnt, names(cnt)),
                collapse = " \u00b7 "
            )
        ))
    }
    invisible(NULL)
}
# }}}

# shift_print__print_store_notice {{{
shift_print__print_store_notice <- function(error) {
    if (!is.null(error) && nzchar(error)) {
        cli::cli_rule("Diagnostics")
        cli::cli_alert_warning("Persisted preview unavailable: {error}")
    }
    invisible(NULL)
}
# }}}

# shift_print__format_epw {{{
shift_print__format_epw <- function(epw, full = FALSE) {
    if (is.null(epw)) {
        return(NULL)
    }
    path <- if (is.character(epw) && length(epw) == 1L) {
        epw
    } else {
        tryCatch(epw_file_coerce(epw)$path(), error = function(e) NULL)
    }
    if (is.null(path)) {
        return(class(epw)[[1L]])
    }
    if (isTRUE(full)) {
        normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    } else {
        basename(path)
    }
}
# }}}

# shift_print__print_stage_receipt {{{
shift_print__print_stage_receipt <- function(
    x,
    title,
    facts,
    table,
    table_title,
    columns,
    n,
    empty,
    more_hint,
    error = NULL,
    verbose = FALSE,
    before_table = NULL
) {
    shift_print__print_stage_intro(x, title, facts)
    if (is.function(before_table)) {
        before_table()
    }
    shift_print__print_table(
        table,
        table_title,
        columns,
        n = n,
        empty = empty,
        more_hint = more_hint
    )
    shift_print__print_store_notice(error)
    shift_print__print_workflow(x, verbose = verbose)
    invisible(x)
}
# }}}

# shift_print__print_files {{{
shift_print__print_files <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    ids <- shift_inspect__ids(x)
    fields <- unique(shift_ui_state__nonempty(x@meta$result_fields))
    req <- shift_stage__root(x)
    fmt_f <- function(f, empty) {
        if (length(f)) cli::format_inline("{length(f)} | [ {f} ]") else empty
    }
    store <- tryCatch(shift_inspect__store(x), error = identity)
    unavail <- inherits(store, "condition")
    if (!unavail) {
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        sm <- shift_inspect__file_catalog_summary(store, ids$query_id)
        sm <- if (nrow(sm)) {
            sm[1L]
        } else {
            data.table::data.table(file_count = 0L, total_size = 0)
        }
        runs <- shift_inspect__query_run(store, ids$query_id)
        run <- if (nrow(runs)) runs[1L] else data.table::data.table()
        file_count <- as.integer(sm$file_count[[1L]])
        if (!length(fields)) {
            fields <- names(shift_inspect__file_catalog_preview(
                store,
                ids$query_id,
                n = 1L
            ))
        }
        facts <- list(
            "Index Node" = if (nrow(run)) run$index_node[[1L]],
            "Collected at" = if (nrow(run)) {
                shift_print__print_time(run$created_at)
            },
            "Result count" = format(
                file_count,
                big.mark = ",",
                scientific = FALSE
            ),
            "Total size" = format_size_units(sm$total_size[[1L]]),
            "Fields" = fmt_f(fields, "0")
        )
    } else {
        facts <- list(
            "Index Node" = if (!is.null(req)) {
                shift_stage__coalesce(req@meta$options$index_node, "auto")
            } else {
                "unavailable"
            },
            "Result count" = shift_stage__coalesce(
                x@meta$file_count,
                "unavailable"
            ),
            "Fields" = fmt_f(fields, "unavailable")
        )
    }
    esg__print_header("ESGF Query Result [File]")
    esg__print_facts(facts)
    if (!is.null(req)) {
        esg__print_parameters(shift_resolve__as_query(req)$state()$parameter)
    }
    if (unavail) {
        cli::cli_rule("Files")
        cli::cli_alert_info(
            "Cached File rows are not available on this handle."
        )
        shift_print__print_store_notice(conditionMessage(store))
    } else {
        preview <- if (file_count < 1L) {
            data.table::data.table()
        } else {
            shift_inspect__file_catalog_preview(store, ids$query_id, n = n)
        }
        shift_print__print_table(
            preview,
            "Files",
            columns = c(
                "source_id",
                "experiment_id",
                "variable_id",
                "variant_label",
                "grid_label",
                "table_id",
                "datetime_start",
                "datetime_end",
                "size",
                "filename",
                "data_node"
            ),
            n = n,
            total_rows = file_count,
            empty = "No matching file records. Review the ESGF query constraints and collect again.",
            more_hint = "use `shift_files()` for all records.",
            hidden_hint = "Use `shift_files()` for all columns."
        )
    }
    shift_print__print_workflow(x, verbose = verbose)
    invisible(x)
}
# }}}

# shift_print__print_plan {{{
shift_print__print_plan <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    f <- shift_ui_state__plan_fields(x)
    cparts <- shift_ui_state__nonempty(c(
        shift_print__display_values(f$model),
        shift_print__display_values(f$scenarios)
    ))
    cases <- data.table::copy(data.table::as.data.table(f$expected_cases))
    if ("years" %in% names(cases)) {
        data.table::set(
            cases,
            j = "years",
            value = vapply(
                cases$years,
                shift_print__format_years,
                character(1L)
            )
        )
    }
    shift_print__print_stage_receipt(
        x,
        title = "Future EPW Plan",
        facts = list(
            "Climate" = paste(cparts, collapse = " \u00b7 "),
            "Periods" = shift_print__format_periods(f$meta$periods),
            "Transform" = f$transform@label,
            "Reference" = shift_print__format_reference(
                f$meta$reference,
                f$meta$recipe
            ),
            "Observed reference" = shift_print__format_reference(
                f$meta$observed_reference
            ),
            "Selection" = shift_ui_view__format_selection(
                f$member,
                f$grid,
                f$table
            ),
            "Expected outputs" = nrow(cases),
            "Output directory" = shift_print__display_path(
                f$meta$epw$export_dir
            )
        ),
        before_table = if (isTRUE(verbose)) {
            function() {
                cli::cli_rule("Discovery")
                esg__print_facts(list(
                    "Frequency" = shift_print__format_cmip6_frequencies(
                        f$frequency
                    ),
                    "Table" = shift_print__format_cmip6_tables(f$table),
                    "Index nodes" = shift_print__display_values(
                        f$nodes,
                        max = Inf
                    ),
                    "Download" = f$control@download,
                    "Remote refresh" = f$control@refresh,
                    "Partial outputs" = f$control@allow_partial,
                    "Output layout" = f$control@output_layout
                ))
            }
        },
        table = cases,
        table_title = "Expected outputs",
        columns = c(
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "period",
            "years",
            "status",
            "missing_reason"
        ),
        n = n,
        empty = "No expected output cases.",
        more_hint = "use `shift_cases()` for all expected cases.",
        verbose = verbose
    )
}
# }}}

# shift_print__print_download {{{
shift_print__print_download <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    ids <- shift_inspect__ids(x)
    read <- shift_print__cached_or_store(x, x@meta$session, function(store) {
        if (is.null(ids$session_id) || is.na(ids$session_id)) {
            return(data.table::data.table())
        }
        store$download_status(session_id = ids$session_id)
    })
    tasks <- read$data
    has_st <- nrow(tasks) && "status" %in% names(tasks)
    cnts <- if (has_st) table(tasks$status) else integer()
    done <- if (has_st) {
        sum(tasks$status %in% c("done", "skipped", "verified"))
    } else {
        0L
    }
    b_done <- shift_print__col_sum(tasks, "bytes_done")
    b_tot <- shift_print__col_sum(tasks, "size")
    task_str <- if (nrow(tasks)) {
        sfx <- if (length(cnts)) {
            paste0(
                " \u00b7 ",
                paste(
                    sprintf("%s %d", names(cnts), cnts),
                    collapse = " \u00b7 "
                )
            )
        } else {
            ""
        }
        sprintf("%d/%d complete%s", done, nrow(tasks), sfx)
    } else {
        "none"
    }
    shift_print__print_stage_receipt(
        x,
        title = "CMIP6 Download",
        facts = list(
            "Session" = ids$session_id,
            "Tasks" = task_str,
            "Transfer" = if (b_tot > 0) {
                sprintf(
                    "%s / %s",
                    format_size_units(b_done),
                    format_size_units(b_tot)
                )
            }
        ),
        table = tasks,
        table_title = "Tasks",
        columns = c(
            "status",
            "filename",
            "bytes_done",
            "size",
            "speed_bps",
            "eta_seconds",
            "data_node",
            "attempts",
            "last_error"
        ),
        n = n,
        empty = "No download tasks are registered.",
        more_hint = "use `shift_data()` or the Downloader inspectors for all tasks.",
        error = read$error,
        verbose = verbose
    )
}
# }}}

# shift_print__print_climate {{{
shift_print__print_climate <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    read <- shift_print__cached_or_store(x, x@meta$coverage, function(store) {
        store$coverage(plan_id = shift_inspect__ids(x)$plan_id)
    })
    cov <- read$data
    site <- tryCatch(shift_inspect__target(x), error = function(e) NULL)
    done <- if (nrow(cov) && "complete" %in% names(cov)) {
        sum(cov$complete %in% TRUE)
    } else {
        0L
    }
    n_rows <- shift_print__col_sum(cov, "output_rows")
    shift_print__print_stage_receipt(
        x,
        title = "Extracted Climate",
        facts = list(
            "Site" = if (!is.null(site)) {
                shift_stage__coalesce(site@label, site@id)
            },
            "Periods" = shift_print__format_periods(x@meta$periods),
            "Coverage" = sprintf("%d/%d complete", done, nrow(cov)),
            "Variables" = if ("variable_id" %in% names(cov)) {
                shift_print__display_values(unique(cov$variable_id))
            },
            "Rows" = if (n_rows > 0) {
                format(n_rows, big.mark = ",", scientific = FALSE)
            }
        ),
        table = cov,
        table_title = "Coverage",
        columns = c(
            "complete",
            "status",
            "experiment_id",
            "variable_id",
            "variant_label",
            "grid_label",
            "time_start",
            "time_stop",
            "output_time_count",
            "output_rows",
            "last_error"
        ),
        n = n,
        empty = "No extraction coverage is available.",
        more_hint = "use `shift_coverage()` for all extraction plans.",
        error = read$error,
        verbose = verbose
    )
}
# }}}

# shift_print__print_morphed {{{
shift_print__print_morphed <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    recipe <- x@meta$recipe
    transform <- x@meta$transform
    read <- shift_print__cached_or_store(x, x@meta$results, function(store) {
        shift_inspect__morph_result_rows(store, shift_inspect__ids(x)$morph_id)
    })
    if (!nrow(read$data)) {
        plan <- data.table::as.data.table(shift_stage__coalesce(
            x@meta$plan,
            data.table::data.table()
        ))
        if (nrow(plan)) read$data <- plan
    }
    rows <- read$data
    t_label <- if (S7::S7_inherits(transform, WeatherTransformSpec)) {
        transform@label
    } else {
        shift_print__format_morph_method(
            shift_stage__coalesce(recipe$name, recipe$backend),
            recipe,
            missing_original_morphing_profile = "legacy"
        )
    }
    shift_print__print_stage_receipt(
        x,
        title = "Morphed EPW",
        facts = list(
            "Transform" = t_label,
            "Reference" = shift_print__format_reference(
                shift_stage__coalesce(x@meta$reference_spec, x@meta$reference),
                recipe
            ),
            "Cases" = if ("case_id" %in% names(rows)) {
                data.table::uniqueN(rows$case_id)
            } else {
                nrow(rows)
            },
            "Results" = nrow(rows)
        ),
        table = rows,
        table_title = "Morph results",
        columns = c(
            "case_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "status",
            "row_count",
            "output_path",
            "last_error"
        ),
        n = n,
        empty = "No morph results are available.",
        more_hint = "use `shift_data()` for complete morph data.",
        error = read$error,
        verbose = verbose
    )
}
# }}}

# shift_print__print_outputs_stage {{{
shift_print__print_outputs_stage <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    read <- shift_print__cached_or_store(x, x@meta$outputs, function(store) {
        shift_inspect__epw_output_rows(store, shift_inspect__ids(x)$morph_id)
    })
    outs <- read$data
    pcols <- intersect(c("export_path", "path", "output_path"), names(outs))
    existing <- if (length(pcols) && nrow(outs)) {
        length(shift_ui_state__nonempty(outs[[pcols[[1L]]]]))
    } else {
        0L
    }
    shift_print__print_stage_receipt(
        x,
        title = "EPW Outputs",
        facts = list(
            "Outputs" = sprintf(
                "%d registered \u00b7 %d path%s",
                nrow(outs),
                existing,
                if (existing == 1L) "" else "s"
            ),
            "Export directory" = if (!is.null(x@meta$export_dir)) {
                shift_print__display_path(x@meta$export_dir)
            }
        ),
        table = outs,
        table_title = "Outputs",
        columns = c(
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "path",
            "export_path",
            "created_at"
        ),
        n = n,
        empty = "No EPW outputs are registered.",
        more_hint = "use `shift_outputs()` for all output records.",
        error = read$error,
        verbose = verbose
    )
}
# }}}

# shift_print__print_cmip6 {{{
shift_print__print_cmip6 <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    esg__print_header("CMIP6 Climate")
    esg__print_facts(list(
        "Model" = if (is.null(x@model)) {
            if (is.null(x@n_models)) {
                "auto (all compatible models)"
            } else {
                sprintf("auto (%d models)", x@n_models)
            }
        } else {
            shift_print__display_values(x@model)
        },
        "Scenarios" = shift_print__display_values(x@scenarios),
        "Member" = shift_print__format_auto(x@member),
        "Grid" = shift_print__format_auto(x@grid),
        "Frequency" = if (is.null(x@frequency)) {
            "inferred by weather method"
        } else {
            shift_print__format_cmip6_frequencies(x@frequency)
        },
        "Table" = shift_print__format_cmip6_tables(x@table),
        "Activity" = x@activity,
        "Index nodes" = sprintf("%d-node failover", length(x@index_nodes)),
        "Data node" = shift_print__format_auto(x@data_node)
    ))
    if (isTRUE(verbose)) {
        shift_print__print_table(
            data.table::data.table(
                priority = seq_along(x@index_nodes),
                index_node = x@index_nodes
            ),
            "Discovery",
            c("priority", "index_node"),
            n = n,
            more_hint = "increase `n` to show every index node."
        )
        if (length(x@filters)) {
            cli::cli_rule("Filters")
            esg__print_facts(list(
                "Values" = shift_print__format_options(x@filters)
            ))
        }
    }
    invisible(x)
}
# }}}

# shift_print__print_run {{{
shift_print__print_run <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    refresh_err <- NULL
    run <- x
    if (is.null(x@store_path) || !nzchar(x@store_path)) {
        refresh_err <- "No store is associated with this cached run."
    } else {
        ref <- tryCatch(shift_inspect__refresh(x), error = identity)
        if (inherits(ref, "condition")) {
            refresh_err <- conditionMessage(ref)
        } else {
            run <- ref
        }
    }
    w <- shift_ui_render__width(width)
    view <- tryCatch(
        shift_ui_view__ui_run_view(
            run,
            width = w,
            detail = if (isTRUE(verbose)) "detail" else "normal",
            motion = "none"
        ),
        error = identity
    )
    cap_rows <- function(lines, label) {
        if (!length(lines) || is.infinite(n)) {
            return(lines)
        }
        pfx <- min(2L, length(lines))
        rec <- max(0L, length(lines) - pfx)
        if (rec <= n) {
            return(lines)
        }
        c(
            lines[seq_len(pfx + n)],
            cli::style_dim(shift_ui_render__fit(
                sprintf("  \u2026 %d more %s", rec - n, label),
                w
            ))
        )
    }
    if (inherits(view, "condition")) {
        shift_print__print_stage_intro(
            run,
            "Shift Run",
            list(
                "Run" = run@ids$run_id,
                "Stage" = run@meta$run$current_stage,
                "Snapshot" = "cached metadata only"
            )
        )
        refresh_err <- paste(
            c(refresh_err, conditionMessage(view)),
            collapse = "; "
        )
    } else {
        view$nodes <- cap_rows(view$nodes, "resolver attempt(s)")
        view$cases <- cap_rows(view$cases, "case(s)")
        shift_ui_render__print_view(view, include_tables = TRUE)
    }
    shift_print__print_store_notice(refresh_err)
    invisible(x)
}
# }}}

# Construct an S7 print method forwarding validated print options and width.
# shift_print__method {{{
shift_print__method <- function(printer, with_n = TRUE) {
    function(x, ...) {
        opts <- shift_print__print_options(list(...))
        shift_ui_render__use_width(opts$width)
        if (isTRUE(with_n)) {
            printer(x, n = opts$n, width = opts$width, verbose = opts$verbose)
        } else {
            printer(x, width = opts$width, verbose = opts$verbose)
        }
    }
}
# }}}

# S7 print methods {{{
S7::method(print, ShiftUiOptions) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        esg__print_header("Shift UI")
        esg__print_facts(list(
            "Progress" = x@progress,
            "Detail" = x@detail,
            "Motion" = x@motion,
            "Refresh" = sprintf("%.2f s", x@refresh),
            "Heartbeat" = sprintf("%g s", x@heartbeat)
        ))
        invisible(x)
    },
    with_n = FALSE
)
S7::method(print, ShiftRequest) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        q <- shift_resolve__as_query(x)
        node <- if (is.null(x@meta$options$index_node)) {
            "auto"
        } else {
            q$index_node()
        }
        esg__print_query(node, q$state()$parameter, title = "ESGF request")
        shift_print__print_workflow(x, verbose = verbose)
        invisible(x)
    },
    with_n = FALSE
)
S7::method(print, ShiftFiles) <- shift_print__method(shift_print__print_files)
S7::method(print, ShiftPlan) <- shift_print__method(shift_print__print_plan)
S7::method(print, ShiftRun) <- shift_print__method(shift_print__print_run)
S7::method(print, ShiftDownload) <- shift_print__method(
    shift_print__print_download
)
S7::method(print, ShiftClimate) <- shift_print__method(
    shift_print__print_climate
)
S7::method(print, ShiftMorphed) <- shift_print__method(
    shift_print__print_morphed
)
S7::method(print, ShiftOutputs) <- shift_print__method(
    shift_print__print_outputs_stage
)
S7::method(print, ShiftCmip6Spec) <- shift_print__method(
    shift_print__print_cmip6
)
S7::method(print, ShiftControl) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        esg__print_header("Shift Control")
        esg__print_facts(list(
            "Strict" = x@strict,
            "Allow partial" = x@allow_partial,
            "Download" = x@download,
            "Resume" = x@resume,
            "Overwrite" = x@overwrite,
            "Remote refresh" = x@refresh,
            "Extraction" = x@extraction_method,
            "Output layout" = x@output_layout
        ))
        invisible(x)
    },
    with_n = FALSE
)
S7::method(print, ShiftReferenceSpec) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        esg__print_header("Climate Reference")
        esg__print_facts(list(
            "Mode" = x@mode,
            "Role" = x@role,
            "Periods" = shift_print__format_periods(x@periods),
            "Plan IDs" = shift_print__display_values(x@plan_id),
            "Experiment" = x@experiment,
            "Activity" = x@activity,
            "Match" = shift_print__display_values(x@match)
        ))
        if (isTRUE(verbose)) {
            details <- list(
                "Filters" = shift_print__format_options(x@filters),
                "Options" = shift_print__format_options(x@options),
                "Collect" = shift_print__format_options(x@collect),
                "Extract" = shift_print__format_options(x@extract)
            )
            if (
                any(vapply(
                    details,
                    function(v) !is.null(v) && nzchar(v),
                    logical(1L)
                ))
            ) {
                cli::cli_rule("Workflow options")
                esg__print_facts(details)
            }
        }
        invisible(x)
    },
    with_n = FALSE
)
S7::method(print, ShiftSite) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        esg__print_header("EPW Site")
        esg__print_facts(list(
            "ID" = x@id,
            "Label" = x@label,
            "Coordinates" = sprintf("%.6f, %.6f", x@lon, x@lat),
            "EPW" = shift_print__format_epw(x@epw, full = verbose)
        ))
        if (isTRUE(verbose) && length(x@metadata)) {
            cli::cli_rule("Metadata")
            esg__print_facts(list(
                "Values" = shift_print__format_options(x@metadata)
            ))
        }
        shift_print__print_workflow(x, verbose = verbose)
        invisible(x)
    },
    with_n = FALSE
)
S7::method(print, ShiftStage) <- shift_print__method(
    function(x, width = NULL, verbose = FALSE) {
        shift_print__print_stage_intro(
            x,
            "Shift Stage",
            list("Class" = class(x)[[1L]], "Stage" = x@stage)
        )
        shift_print__print_workflow(x, verbose = verbose)
        invisible(x)
    },
    with_n = FALSE
)
# }}}

# shift_print__error_summary {{{
shift_print__error_summary <- function(message) {
    msg <- cli::ansi_strip(as.character(shift_stage__coalesce(
        message,
        "Unknown error."
    )))
    lines <- trimws(unlist(strsplit(msg, "[\r\n]+")))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) {
        return("Unknown error.")
    }
    sub("^[!xX][[:space:]]*", "", lines[[1L]])
}
# }}}

# shift_print__abort_run {{{
shift_print__abort_run <- function(
    error,
    run_id,
    store_path,
    stage,
    details,
    debug = FALSE
) {
    res <- if (inherits(error, "epwshiftr_shift_resolution_error")) {
        error$resolution
    }
    message <- conditionMessage(error)
    fctx <- if (is.null(res)) {
        shift_print__failure_context(details, debug = debug)
    } else {
        ""
    }
    ev <- shift_print__resolution_evidence(res)
    inspect_cmd <- sprintf(
        "shift_diagnostics(%s)",
        shift_print__run_command("shift_run_get", run_id, store_path)
    )
    resume_cmd <- shift_print__run_command("shift_resume", run_id, store_path)
    logs_cmd <- shift_print__run_command(
        "shift_logs",
        run_id,
        store_path,
        "tail = 20L"
    )
    cause <- if (is.null(res)) {
        shift_print__error_summary(message)
    } else {
        shift_stage__coalesce(res$cause, res$summary)
    }
    cli::cli_abort(
        c(
            "Future EPW run {.val {run_id}} failed during {.val {stage}}.",
            "x" = paste0("Cause: ", cause),
            if (length(ev)) stats::setNames(ev, rep("i", length(ev))),
            if (nzchar(fctx)) c("i" = fctx),
            if (!is.null(res) && identical(res$recovery, "change_request")) {
                c(
                    "!" = paste(
                        "Resuming this request unchanged will repeat the",
                        "coverage failure. Adjust the climate selection or reference first."
                    )
                )
            },
            "i" = "Inspect: {.code {inspect_cmd}}",
            if (is.null(res) || isTRUE(res$retryable)) {
                c("i" = "Retry: {.code {resume_cmd}}")
            },
            "i" = "Logs: {.code {logs_cmd}}"
        ),
        class = "epwshiftr_shift_error",
        run_id = run_id,
        store = store_path,
        stage = stage,
        original_message = message,
        source_error = error,
        call = NULL
    )
}
# }}}

# }}}

# vim: fdm=marker :
