# ShiftFrameRenderer owns the live terminal region for one workflow. Its ANSI
# painting algorithm follows the multiline status-bar implementation added to
# r-lib/cli in #819, while all capability detection, width handling, styling,
# and cursor visibility continue to use cli's public API.
ShiftFrameRenderer <- R6::R6Class(
    "ShiftFrameRenderer",
    lock_class = TRUE,
    public = list(
        # Bind the renderer to one output connection. `backend` is resolved by
        # shift__ui_renderer() in production and can be explicit in byte-level
        # tests so tests never depend on the host terminal.
        initialize = function(output = cli::cli_output_connection(),
                              backend = c("frame", "compact"),
                              writer = NULL) {
            backend <- match.arg(backend)
            if (!inherits(output, "connection")) {
                cli::cli_abort("`output` must be an R connection.")
            }
            if (!is.null(writer) && !is.function(writer)) {
                cli::cli_abort("`writer` must be a function or `NULL`.")
            }
            private$output <- output
            private$backend_value <- backend
            private$writer <- shift_coalesce(writer, function(text) {
                cat(text, file = output, sep = "")
                flush.console()
                invisible(NULL)
            })
        },

        # Paint a complete dashboard frame atomically. Compact terminals keep
        # cli's mature single-line status renderer instead of receiving cursor
        # movement sequences that their host IDE may not support.
        draw = function(lines, compact = NULL) {
            lines <- shift__tui_normalize_lines(lines)
            if (!length(lines)) {
                return(invisible(FALSE))
            }
            compact <- shift__tui_normalize_lines(
                shift_coalesce(compact, lines[[1L]]))[[1L]]
            private$last_frame <- lines
            private$last_compact <- compact
            if (isTRUE(private$suspended)) {
                return(invisible(TRUE))
            }
            if (identical(private$backend_value, "compact")) {
                return(invisible(private$draw_compact(compact)))
            }
            invisible(private$draw_frame(lines))
        },

        # Remove the currently painted region without discarding the last
        # semantic frame, allowing suspend() to restore it after normal output.
        clear = function() {
            if (identical(private$backend_value, "compact")) {
                private$close_compact("done")
                return(invisible(NULL))
            }
            private$clear_frame()
            invisible(NULL)
        },

        # Temporarily release the framebuffer while cli emits alerts, tables,
        # or diagnostics. Nested calls are intentionally idempotent so an
        # entire table can be emitted under one clear/restore cycle.
        suspend = function(code) {
            if (!is.function(code)) {
                cli::cli_abort("`code` must be a function.")
            }
            if (isTRUE(private$suspended) ||
                !identical(private$backend_value, "frame") ||
                !isTRUE(private$active_value)) {
                return(code())
            }
            private$clear_frame()
            private$suspended <- TRUE
            on.exit({
                private$suspended <- FALSE
                if (!isTRUE(private$closed) && length(private$last_frame)) {
                    private$draw_frame(private$last_frame)
                }
            }, add = TRUE)
            code()
        },

        # Commit the last semantic frame to terminal scrollback before
        # releasing cursor ownership. Terminal failure states use this path so
        # their workflow context remains visible above the final R condition.
        commit = function(result = c("done", "failed", "cancelled")) {
            result <- match.arg(result)
            if (isTRUE(private$closed)) {
                return(invisible(NULL))
            }
            if (identical(private$backend_value, "compact")) {
                private$close_compact(if (identical(result, "done")) {
                    "done"
                } else {
                    "failed"
                })
            } else {
                # draw_frame() leaves the cursor at column zero of the final
                # row; one newline preserves the frame and frees the next row
                # for the durable diagnostic block without repainting it.
                if (isTRUE(private$active_value)) {
                    private$write("\n")
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

        # Release terminal resources exactly once. Every normal, error, and
        # interrupt path may call close(), so cleanup must remain idempotent.
        close = function(result = c("done", "failed", "cancelled")) {
            result <- match.arg(result)
            if (isTRUE(private$closed)) {
                return(invisible(NULL))
            }
            if (identical(private$backend_value, "compact")) {
                private$close_compact(if (identical(result, "done")) {
                    "done"
                } else {
                    "failed"
                })
            } else {
                private$clear_frame()
                if (isTRUE(private$cursor_hidden)) {
                    try(cli::ansi_show_cursor(private$output), silent = TRUE)
                    private$cursor_hidden <- FALSE
                }
            }
            private$closed <- TRUE
            private$active_value <- FALSE
            private$last_frame <- character()
            private$last_compact <- NULL
            invisible(NULL)
        },

        # Expose the immutable backend for reporter integration tests and for
        # choosing the matching compact formatter without leaking frame state.
        backend = function() private$backend_value,

        # Report whether a frame or compact status line currently owns output.
        active = function() isTRUE(private$active_value)
    ),
    private = list(
        output = NULL,
        backend_value = NULL,
        writer = NULL,
        painted_lines = 0L,
        last_frame = character(),
        last_compact = NULL,
        compact_id = NULL,
        cursor_hidden = FALSE,
        suspended = FALSE,
        active_value = FALSE,
        closed = FALSE,

        # Write one control string so the terminal never exposes a partially
        # updated dashboard between individual workflow rows.
        write = function(text) {
            tryCatch({
                private$writer(text)
                TRUE
            }, error = function(e) FALSE)
        },

        # Paint all rows using the same cursor-up, erase-line, and stale-tail
        # handling used by cli's upstream multiline status implementation.
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
            output <- ""
            if (previous > 1L) {
                output <- shift__tui_cursor_up(previous - 1L)
            }
            for (i in seq_len(current)) {
                suffix <- if (i < current) "\n" else "\r"
                output <- paste0(output, "\r", lines[[i]],
                    SHIFT_TUI_ERASE_LINE, suffix)
            }
            # A shorter replacement frame must explicitly erase rows that are
            # no longer present or their old content remains below the UI.
            if (previous > current) {
                for (i in seq_len(previous - current)) {
                    output <- paste0(output, "\n\r", SHIFT_TUI_ERASE_LINE)
                }
                output <- paste0(output,
                    shift__tui_cursor_up(previous - current), "\r")
            }
            if (!isTRUE(private$write(output))) {
                return(FALSE)
            }
            private$painted_lines <- current
            private$active_value <- TRUE
            TRUE
        },

        # Clear every painted row and return the cursor to the top of the old
        # region, matching cli's behavior before emitting ordinary output.
        clear_frame = function() {
            count <- private$painted_lines
            if (count <= 0L) {
                private$active_value <- FALSE
                return(invisible(NULL))
            }
            output <- if (count > 1L) {
                shift__tui_cursor_up(count - 1L)
            } else {
                ""
            }
            for (i in seq_len(count)) {
                output <- paste0(output, "\r", SHIFT_TUI_ERASE_LINE,
                    if (i < count) "\n" else "")
            }
            if (count > 1L) {
                output <- paste0(output, shift__tui_cursor_up(count - 1L))
            }
            private$write(output)
            private$painted_lines <- 0L
            private$active_value <- FALSE
            invisible(NULL)
        },

        # Create or update the one cli-owned status row used by RStudio and
        # other dynamic consoles without reliable cursor-up support.
        draw_compact = function(line) {
            if (isTRUE(private$closed)) {
                return(FALSE)
            }
            if (!length(private$compact_id)) {
                private$compact_id <- tryCatch(cli::cli_progress_bar(
                    name = "Future EPW",
                    total = NA,
                    status = line,
                    format = "{cli::pb_status}",
                    current = FALSE,
                    auto_terminate = FALSE,
                    .auto_close = FALSE
                ), error = function(e) NULL)
            }
            if (!length(private$compact_id)) {
                return(FALSE)
            }
            ok <- tryCatch({
                cli::cli_progress_update(id = private$compact_id, inc = 0L,
                    status = line, force = TRUE)
                TRUE
            }, error = function(e) FALSE)
            private$active_value <- isTRUE(ok)
            ok
        },

        # Close a compact cli bar defensively because an IDE may already have
        # removed it while processing another top-level console operation.
        close_compact = function(result = "done") {
            if (length(private$compact_id)) {
                try(cli::cli_progress_done(id = private$compact_id,
                    result = result), silent = TRUE)
            }
            private$compact_id <- NULL
            private$active_value <- FALSE
            invisible(NULL)
        }
    )
)

# CSI erase-line is the only terminal control sequence not exposed by cli's
# public API that the framebuffer needs in addition to cursor-up.
SHIFT_TUI_ERASE_LINE <- "\033[K"

# Build a standards-based cursor-up sequence for the exact number of rows that
# the renderer has previously painted.
shift__tui_cursor_up <- function(lines) {
    lines <- as.integer(lines)
    if (!length(lines) || is.na(lines) || lines <= 0L) "" else
        sprintf("\033[%dA", lines)
}

# Remove embedded line breaks before a semantic row reaches either renderer.
# Dashboard formatters already bound display width; this final normalization
# prevents one malformed label from changing framebuffer ownership.
shift__tui_normalize_lines <- function(lines) {
    lines <- as.character(shift_coalesce(lines, character()))
    gsub("[\r\n]+", " ", lines)
}

# Resolve the live backend with cli's public capability checks. A multi-line
# frame additionally requires a real TTY because IDE consoles may support ANSI
# colour without supporting cursor-up movement.
shift__ui_renderer_backend <- function(output = cli::cli_output_connection()) {
    ansi <- tryCatch(
        isTRUE(base::isatty(output)) && isTRUE(cli::is_ansi_tty(output)),
        error = function(e) FALSE
    )
    if (isTRUE(ansi)) {
        return("frame")
    }
    dynamic <- tryCatch(isTRUE(cli::is_dynamic_tty(output)),
        error = function(e) FALSE)
    if (isTRUE(dynamic)) "compact" else "log"
}

# Construct one renderer for a complete foreground or watch lifecycle. Log and
# null modes deliberately return NULL because they never own terminal rows.
shift__ui_renderer <- function(mode = c("dynamic", "log", "none"),
                               output = cli::cli_output_connection(),
                               backend = NULL, writer = NULL) {
    mode <- match.arg(mode)
    if (!identical(mode, "dynamic")) {
        return(NULL)
    }
    backend <- shift_coalesce(backend, shift__ui_renderer_backend(output))
    if (identical(backend, "log")) {
        return(NULL)
    }
    ShiftFrameRenderer$new(output = output, backend = backend, writer = writer)
}
