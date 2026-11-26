# ShiftFrameRenderer owns the live terminal region for one workflow. Its ANSI
# painting algorithm follows the multiline status-bar implementation added to
# r-lib/cli in #819, while all capability detection, width handling, styling,
# and cursor visibility continue to use cli's public API.
# ShiftFrameRenderer {{{
ShiftFrameRenderer <- R6::R6Class(
    "ShiftFrameRenderer",
    lock_class = TRUE,
    public = list(
        # Bind the renderer to one output connection. `backend` is resolved by
        # shift_tui__ui_renderer() in production and can be explicit in byte-level
        # tests so tests never depend on the host terminal.
        # initialize {{{
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
        # }}}

        # Paint a complete dashboard frame atomically. Compact terminals keep
        # cli's mature single-line status renderer instead of receiving cursor
        # movement sequences that their host IDE may not support.
        # draw {{{
        draw = function(lines, compact = NULL) {
            lines <- shift_tui__tui_normalize_lines(lines)
            if (!length(lines)) {
                return(invisible(FALSE))
            }
            compact <- shift_tui__tui_normalize_lines(
                shift_stage__coalesce(compact, lines[[1L]])
            )[[1L]]
            # A frame taller than the viewport cannot be repainted with
            # cursor-up sequences. Semantic batch views fit their own sections;
            # other unexpectedly tall views retain a safe compact live row.
            private$last_frame <- lines
            private$last_compact <- compact
            private$frame_compacted <- identical(
                private$backend_value,
                "frame"
            ) &&
                length(lines) > shift_ui__ui_height()
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
        # }}}

        # Remove the currently painted region without discarding the last
        # semantic frame, allowing suspend() to restore it after normal output.
        # clear {{{
        clear = function() {
            if (identical(private$backend_value, "compact")) {
                private$close_compact("done")
                return(invisible(NULL))
            }
            private$clear_frame()
            invisible(NULL)
        },
        # }}}

        # Temporarily release the framebuffer while cli emits alerts, tables,
        # or diagnostics. Nested calls are intentionally idempotent so an
        # entire table can be emitted under one clear/restore cycle.
        # suspend {{{
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
        # }}}

        # Commit the last semantic frame to terminal scrollback before
        # releasing cursor ownership. Every terminal workflow outcome uses this
        # path so its final receipt remains visible after live animation stops.
        # commit {{{
        commit = function(result = c("done", "failed", "cancelled")) {
            private$teardown(match.arg(result), preserve = TRUE)
        },
        # }}}

        # Release terminal resources exactly once. Every normal, error, and
        # interrupt path may call close(), so cleanup must remain idempotent.
        # close {{{
        close = function(result = c("done", "failed", "cancelled")) {
            private$teardown(match.arg(result), preserve = FALSE)
        },
        # }}}

        # Expose the immutable backend for reporter integration tests and for
        # choosing the matching compact formatter without leaking frame state.
        # backend {{{
        backend = function() private$backend_value,
        # }}}

        # Report whether a frame or compact status line currently owns output.
        # active {{{
        active = function() isTRUE(private$active_value)
        # }}}
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

        # Write one control string so the terminal never exposes a partially
        # updated dashboard between individual workflow rows.
        # write {{{
        write = function(text) {
            tryCatch(
                {
                    # Bound every ANSI write with a reset. An interrupted coloured
                    # message outside this renderer must not tint the entire next
                    # frame, and a frame must not leak styles into the R prompt.
                    if (cli::num_ansi_colors() > 1L) {
                        text <- paste0("\033[0m", text, "\033[0m")
                    }
                    private$writer(text)
                    TRUE
                },
                error = function(e) FALSE
            )
        },
        # }}}

        # Paint all rows using the same cursor-up, erase-line, and stale-tail
        # handling used by cli's upstream multiline status implementation.
        # draw_frame {{{
        draw_frame = function(lines) {
            if (isTRUE(private$closed)) {
                return(FALSE)
            }
            if (!isTRUE(private$cursor_hidden)) {
                try(cli::ansi_hide_cursor(private$output), silent = TRUE)
                private$cursor_hidden <- TRUE
            }
            previous <- private$painted_lines
            current <- length(lines)
            # After a terminal shrinks, its old rows may already be in
            # scrollback. Never walk or erase beyond the current viewport.
            previous <- min(previous, shift_ui__ui_height())
            output <- ""
            if (previous > 1L) {
                output <- shift_tui__tui_cursor_up(previous - 1L)
            }
            for (i in seq_len(current)) {
                suffix <- if (i < current) "\n" else "\r"
                output <- paste0(
                    output,
                    "\r",
                    lines[[i]],
                    SHIFT_TUI_ERASE_LINE,
                    suffix
                )
            }
            # A shorter replacement frame must explicitly erase rows that are
            # no longer present or their old content remains below the UI.
            if (previous > current) {
                for (i in seq_len(previous - current)) {
                    output <- paste0(output, "\n\r", SHIFT_TUI_ERASE_LINE)
                }
                output <- paste0(
                    output,
                    shift_tui__tui_cursor_up(previous - current),
                    "\r"
                )
            }
            if (!isTRUE(private$write(output))) {
                return(FALSE)
            }
            private$painted_lines <- current
            private$active_value <- TRUE
            TRUE
        },
        # }}}

        # Clear every painted row and return the cursor to the top of the old
        # region, matching cli's behavior before emitting ordinary output.
        # clear_frame {{{
        clear_frame = function() {
            count <- min(private$painted_lines, shift_ui__ui_height())
            if (count <= 0L) {
                private$active_value <- FALSE
                return(invisible(NULL))
            }
            output <- if (count > 1L) {
                shift_tui__tui_cursor_up(count - 1L)
            } else {
                ""
            }
            for (i in seq_len(count)) {
                output <- paste0(
                    output,
                    "\r",
                    SHIFT_TUI_ERASE_LINE,
                    if (i < count) "\n" else ""
                )
            }
            if (count > 1L) {
                output <- paste0(output, shift_tui__tui_cursor_up(count - 1L))
            }
            private$write(output)
            private$painted_lines <- 0L
            private$active_value <- FALSE
            invisible(NULL)
        },
        # }}}

        # Create or update the one cli-owned status row used by RStudio and
        # other dynamic consoles without reliable cursor-up support.
        # draw_compact {{{
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
        # }}}

        # Shared idempotent teardown for commit() and close().
        # teardown {{{
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
                    # Oversized receipts are written once into scrollback after
                    # clearing the bounded live row so every output path survives.
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
        # }}}

        # Close a compact cli bar defensively because an IDE may already have
        # removed it while processing another top-level console operation.
        # close_compact {{{
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
        # }}}
    )
)
# }}}

# CSI erase-line is the only terminal control sequence not exposed by cli's
# public API that the framebuffer needs in addition to cursor-up.
SHIFT_TUI_ERASE_LINE <- "\033[K"

# Build a standards-based cursor-up sequence for the exact number of rows that
# the renderer has previously painted.
# shift_tui__tui_cursor_up {{{
shift_tui__tui_cursor_up <- function(lines) {
    lines <- as.integer(lines)
    if (!length(lines) || is.na(lines) || lines <= 0L) {
        ""
    } else {
        sprintf("\033[%dA", lines)
    }
}
# }}}

# Remove embedded line breaks before a semantic row reaches either renderer.
# Dashboard formatters already bound display width; this final normalization
# prevents one malformed label from changing framebuffer ownership.
# shift_tui__tui_normalize_lines {{{
shift_tui__tui_normalize_lines <- function(lines) {
    lines <- as.character(shift_stage__coalesce(lines, character()))
    gsub("[\r\n]+", " ", lines)
}
# }}}

# Resolve the live backend with cli's public capability checks. A multi-line
# frame additionally requires a real TTY because IDE consoles may support ANSI
# colour without supporting cursor-up movement.
# shift_tui__ui_renderer_backend {{{
shift_tui__ui_renderer_backend <- function(
    output = cli::cli_output_connection()
) {
    ansi <- tryCatch(
        isTRUE(base::isatty(output)) && isTRUE(cli::is_ansi_tty(output)),
        error = function(e) FALSE
    )
    if (isTRUE(ansi)) {
        return("frame")
    }
    dynamic <- tryCatch(
        isTRUE(cli::is_dynamic_tty(output)),
        error = function(e) FALSE
    )
    if (isTRUE(dynamic)) "compact" else "log"
}
# }}}

# Construct one renderer for a complete foreground or watch lifecycle. Log and
# null modes deliberately return NULL because they never own terminal rows.
# shift_tui__ui_renderer {{{
shift_tui__ui_renderer <- function(
    mode = c("dynamic", "log", "none"),
    output = cli::cli_output_connection(),
    backend = NULL,
    writer = NULL
) {
    mode <- match.arg(mode)
    if (!identical(mode, "dynamic")) {
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

# Shared watch/follow state machine for R single-run, R batch, and CLI watch
# loops. Owns renderer lifecycle, dynamic-to-log fallback, lossless event-delta
# emission, poll vs animation cadence, count limits, and clean interrupt exits.
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
    mode <- shift_ui__ui_mode(ui)
    motion <- shift_ui__ui_motion(ui, mode)
    renderer <- tryCatch(shift_tui__ui_renderer(mode), error = function(e) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) {
        mode <- "log"
        motion <- "none"
    }
    close_renderer <- function(result = "done") {
        if (!is.null(renderer)) {
            renderer$close(result = result)
            renderer <<- NULL
        }
        invisible(NULL)
    }
    on.exit(close_renderer(), add = TRUE)
    sleep_interval <- function() {
        if (isTRUE(animate) && identical(mode, "dynamic")) {
            if (identical(motion, "full")) {
                return(ui@refresh)
            }
            if (identical(motion, "reduced")) {
                return(max(1, ui@refresh))
            }
        }
        interval
    }
    frame <- 0L
    polls <- 0L
    first <- TRUE
    last_poll <- as.POSIXct(NA)
    cursor <- initial_cursor
    cursor_initialized <- FALSE
    snapshot <- NULL
    tryCatch(
        repeat {
            now <- now_fn()
            poll_due <- isTRUE(first) ||
                is.na(last_poll) ||
                as.numeric(difftime(now, last_poll, units = "secs")) >= interval
            if (isTRUE(poll_due)) {
                polls <- polls + 1L
                snapshot <- poll(polls)
                last_poll <- now
            }
            done <- isTRUE(is_done(snapshot, polls))
            if (isTRUE(poll_due) && is.function(on_poll)) {
                on_poll(snapshot, first = first, done = done)
            }
            if (
                identical(mode, "log") &&
                    isTRUE(poll_due) &&
                    is.function(event_delta)
            ) {
                delta <- event_delta(
                    snapshot,
                    cursor,
                    initial = !cursor_initialized
                )
                if (isTRUE(cursor_initialized)) {
                    if (isTRUE(delta$gap) && !is.null(gap_message)) {
                        cli::cli_alert_info(gap_message)
                    }
                    for (index in seq_len(nrow(delta$rows))) {
                        cli::cli_text(
                            "{shift_ui_view__ui_persisted_event_line(delta$rows[index], detail = ui@detail)}"
                        )
                    }
                }
                cursor <- delta$cursor
                cursor_initialized <- TRUE
            }
            if (isTRUE(done)) {
                close_renderer(result = "done")
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
                    close_renderer(result = "failed")
                    mode <- "log"
                    motion <- "none"
                    print_view(snapshot, final = FALSE)
                    if (is.function(event_delta)) {
                        cursor <- event_delta(
                            snapshot,
                            cursor,
                            initial = TRUE
                        )$cursor
                        cursor_initialized <- TRUE
                    }
                }
            } else if (
                identical(mode, "log") && isTRUE(poll_due) && isTRUE(first)
            ) {
                print_view(snapshot, final = FALSE)
            }
            first <- FALSE
            if (!is.infinite(max_polls) && polls >= max_polls) {
                if (identical(mode, "dynamic")) {
                    close_renderer(result = "done")
                    print_view(snapshot, final = TRUE)
                }
                break
            }
            sleep_fn(sleep_interval())
        },
        interrupt = function(error) {
            close_renderer(result = "cancelled")
            if (is.function(on_interrupt)) {
                on_interrupt(mode)
            }
        }
    )
    invisible(snapshot)
}
# }}}

# vim: fdm=marker :
