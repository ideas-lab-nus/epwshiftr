epwshiftr_cli_context <- function(parsed) {
    args <- parsed$args
    if (isTRUE(parsed$help)) {
        return(list(group = "help", command = paste(args, collapse = " "), action = NULL))
    }
    if (!length(args)) {
        return(list(group = NULL, command = NULL, action = NULL))
    }
    if (identical(args[[1L]], "help")) {
        topic <- args[-1L]
        return(list(group = "help", command = paste(topic, collapse = " "), action = NULL))
    }
    list(
        group = args[[1L]],
        command = if (length(args) >= 2L) args[[2L]] else NULL,
        action = if (length(args) >= 3L) args[[3L]] else NULL
    )
}


epwshiftr_cli_render <- function(result, context = NULL) {
    if (is.null(context)) {
        context <- list()
    }
    if (is.character(result)) {
        return(epwshiftr_cli_render_help(result))
    }
    group <- context$group
    command <- context$command
    action <- context$action

    if (identical(group, "doctor")) {
        return(epwshiftr_cli_render_doctor(result))
    }
    if (identical(group, "query")) {
        return(epwshiftr_cli_render_query(result, command))
    }
    if (identical(group, "download")) {
        return(epwshiftr_cli_render_download(result, command, action))
    }
    if (identical(group, "esgf") && identical(command, "report")) {
        return(epwshiftr_cli_render_esgf_report(result))
    }
    if (identical(group, "storage")) {
        return(epwshiftr_cli_render_storage(result, command, action))
    }
    epwshiftr_cli_render_default(result)
}


epwshiftr_cli_render_help <- function(lines) {
    for (line in lines) {
        cli::cli_text("{line}")
    }
    invisible(NULL)
}


epwshiftr_cli_render_doctor <- function(result) {
    cli::cli_h1("epwshiftr doctor")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$checks,
        title = "Checks",
        columns = c("check", "status", "message", "detail")
    )
    invisible(NULL)
}


epwshiftr_cli_render_query <- function(result, command) {
    if (identical(command, "list")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Stored ESGF queries",
            columns = c("query_id", "label", "tracked", "last_checked_at", "created_at", "updated_at")
        ))
    }
    if (identical(command, "search")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("ESGF search")
            epwshiftr_cli_render_summary(result[setdiff(names(result), "query")], "Dry run")
            if (!is.null(result$query)) {
                epwshiftr_cli_render_default(result$query, title = "Query parameters")
            }
            return(invisible(NULL))
        }
        return(epwshiftr_cli_render_table(
            result,
            title = "ESGF search results",
            columns = c("title", "filename", "variable_id", "experiment_id", "source_id", "data_node", "size", "id", "dataset_id")
        ))
    }
    if (identical(command, "preview")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("Query update preview")
            epwshiftr_cli_render_summary(result$summary, "Summary")
            epwshiftr_cli_render_table(
                result$changes,
                title = "Changes",
                columns = c("change_type", "filename", "size", "version", "data_node", "file_key", "query_id")
            )
            return(invisible(NULL))
        }
        return(epwshiftr_cli_render_table(
            result,
            title = "Query update preview",
            columns = c("query_id", "label", "file_total", "current_count", "new_count", "changed_count", "stale_count")
        ))
    }
    if (identical(command, "update")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Query updates",
            columns = c("query_id", "label", "new_count", "changed_count", "stale_count", "file_total", "created_at", "update_id")
        ))
    }
    if (identical(command, "show")) {
        return(epwshiftr_cli_render_named_list(result, title = "Query"))
    }
    title <- switch(
        command,
        status = "Query status",
        files = "Query files",
        updates = "Query updates",
        changes = "Query changes",
        tags = "Query tags",
        track = "Query tracking",
        untrack = "Query tracking",
        tag = "Query tags",
        untag = "Query tags",
        remove = "Query removal",
        "Query result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_download <- function(result, command, action = NULL) {
    if (identical(command, "preflight")) {
        return(epwshiftr_cli_render_download_preflight(result))
    }
    if (identical(command, "watch")) {
        return(epwshiftr_cli_render_download_watch(result))
    }
    if (identical(command, "logs") || identical(command, "events")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download events",
            columns = c("created_at", "event", "status", "error", "data_node", "session_id", "task_id", "file_key")
        ))
    }
    if (identical(command, "status") || identical(command, "tasks")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download tasks",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error", "session_id", "task_id", "file_key")
        ))
    }
    if (identical(command, "sessions")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download sessions",
            columns = c("session_id", "status", "task_count", "label", "created_at", "finished_at", "started_at")
        ))
    }
    if (identical(command, "nodes") || identical(command, "reset-nodes")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Data nodes",
            columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until", "dry_run")
        ))
    }
    if (identical(command, "config")) {
        return(epwshiftr_cli_render_named_list(result, title = paste("Download config", epwshiftr_cli_string_default(action, ""))))
    }
    title <- switch(
        command,
        run = "Download run",
        resume = "Download resume",
        verify = "Download verification",
        cancel = "Download cancellation",
        retry = "Download retry",
        "Download result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_download_preflight <- function(result) {
    cli::cli_h1("Download preflight")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$changes,
        title = "Changes",
        columns = c("change_type", "filename", "size", "version", "data_node", "file_key")
    )
    epwshiftr_cli_render_table(
        result$files,
        title = "Files",
        columns = c("status", "filename", "size", "local_path", "file_key")
    )
    epwshiftr_cli_render_table(
        result$candidates,
        title = "Candidates",
        columns = c("data_node", "priority", "probe_ok", "probe_latency", "probe_cached", "target_rel_path", "file_key", "url")
    )
    invisible(NULL)
}


epwshiftr_cli_render_download_watch <- function(result) {
    cli::cli_h1("Download activity")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$tasks,
        title = "Tasks",
        columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error", "session_id", "task_id", "file_key")
    )
    epwshiftr_cli_render_table(
        result$nodes,
        title = "Data nodes",
        columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until")
    )
    epwshiftr_cli_render_table(
        result$events,
        title = "Recent events",
        columns = c("created_at", "event", "status", "error", "session_id", "task_id", "file_key")
    )
    invisible(NULL)
}


epwshiftr_cli_render_esgf_report <- function(result) {
    cli::cli_h1("ESGF report")
    epwshiftr_cli_render_table(
        result$summary,
        title = "Summary",
        columns = c(
            "query_id", "label", "tracked", "file_total", "current_count", "local_count",
            "bytes_missing", "download_incomplete", "download_retryable", "last_download_session_id"
        )
    )
    epwshiftr_cli_render_table(
        result$updates,
        title = "Updates",
        columns = c("query_id", "new_count", "changed_count", "stale_count", "file_total", "created_at", "update_id")
    )
    epwshiftr_cli_render_table(
        result$changes,
        title = "Changes",
        columns = c("change_type", "filename", "size", "version", "data_node", "file_key", "query_id")
    )
    epwshiftr_cli_render_table(
        result$downloads,
        title = "Downloads",
        columns = c("status", "filename", "bytes_done", "size", "last_error", "session_id", "task_id", "file_key")
    )
    epwshiftr_cli_render_table(
        result$nodes,
        title = "Data nodes",
        columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until")
    )
    invisible(NULL)
}


epwshiftr_cli_render_storage <- function(result, command, action = NULL) {
    if (identical(command, "report")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("Storage report")
            epwshiftr_cli_render_summary(result$summary, "Summary")
            epwshiftr_cli_render_table(result$downloads, "Download files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$registered, "Registered files", c("source", "file_key", "relative_path", "size"))
            epwshiftr_cli_render_table(result$untracked_files, "Untracked files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$missing_records, "Missing records", c("source", "file_key", "relative_path", "size"))
            epwshiftr_cli_render_table(result$tmp, "Temporary files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$orphan_records, "Orphan records", c("file_key", "filename", "size", "local_path"))
            return(invisible(NULL))
        }
        return(epwshiftr_cli_render_summary(result, "Storage report"))
    }
    if (identical(command, "validate")) {
        cli::cli_h1("Storage validation")
        epwshiftr_cli_render_summary(result$summary, "Summary")
        epwshiftr_cli_render_table(result$files, "Files", c("file_key", "filename", "local_path", "exists", "size_ok", "checksum_ok", "layout_ok"))
        epwshiftr_cli_render_table(result$artifacts, "Artifacts", c("artifact_id", "file_key", "relative_path", "exists", "size_ok"))
        epwshiftr_cli_render_table(result$untracked, "Untracked files", c("relative_path", "size", "mtime"))
        epwshiftr_cli_render_table(result$actions, "Actions", c("action", "target", "file_key", "from", "to", "reason"))
        return(invisible(NULL))
    }
    if (identical(command, "layout")) {
        return(epwshiftr_cli_render_summary(result, paste("Storage layout", epwshiftr_cli_string_default(action, ""))))
    }
    title <- switch(
        command,
        repair = "Storage repair",
        cleanup = "Storage cleanup",
        "Storage result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_default <- function(result, title = NULL) {
    if (is.data.frame(result)) {
        return(epwshiftr_cli_render_table(result, title = title))
    }
    if (is.list(result)) {
        return(epwshiftr_cli_render_named_list(result, title = title))
    }
    if (!is.null(title)) {
        cli::cli_h2(title)
    }
    value <- epwshiftr_cli_format_cell(result)
    cli::cli_text("{value}")
    invisible(NULL)
}


epwshiftr_cli_render_named_list <- function(result, title = NULL) {
    if (!is.null(title) && nzchar(trimws(title))) {
        cli::cli_h1(trimws(title))
    }
    if (!length(result)) {
        cli::cli_alert_info("No values.")
        return(invisible(NULL))
    }
    nms <- names(result)
    if (is.null(nms)) {
        nms <- rep("", length(result))
    }
    scalar <- vapply(result, epwshiftr_cli_is_scalar_value, logical(1L))
    if (any(scalar)) {
        epwshiftr_cli_render_summary(result[scalar], "Values")
    }
    for (i in which(!scalar)) {
        name <- nms[[i]]
        subtitle <- if (nzchar(name)) epwshiftr_cli_title(name) else NULL
        epwshiftr_cli_render_default(result[[i]], title = subtitle)
    }
    invisible(NULL)
}


epwshiftr_cli_render_summary <- function(x, title = "Summary") {
    if (is.null(x)) {
        return(invisible(NULL))
    }
    if (is.data.frame(x)) {
        if (!nrow(x)) {
            cli::cli_h2(title)
            cli::cli_alert_info("No rows.")
            return(invisible(NULL))
        }
        if (nrow(x) > 1L) {
            return(epwshiftr_cli_render_table(x, title = title))
        }
        x <- as.list(x[1L, , drop = FALSE])
    }
    if (!is.list(x)) {
        x <- list(value = x)
    }
    cli::cli_h2(title)
    for (name in names(x)) {
        label <- epwshiftr_cli_title(name)
        value <- paste(epwshiftr_cli_format_named_cell(x[[name]], name), collapse = ", ")
        cli::cli_text("{label}: {value}")
    }
    invisible(NULL)
}


epwshiftr_cli_render_table <- function(x, title = NULL, columns = NULL, max_rows = 20L) {
    x <- epwshiftr_cli_as_data_frame(x)
    if (!is.null(title) && nzchar(title)) {
        cli::cli_h2(title)
    }
    if (!nrow(x)) {
        cli::cli_alert_info("No rows.")
        return(invisible(NULL))
    }
    columns <- epwshiftr_cli_pick_columns(x, columns)
    if (!length(columns)) {
        cli::cli_alert_info("No printable columns.")
        return(invisible(NULL))
    }
    shown <- x[seq_len(min(nrow(x), max_rows)), columns, drop = FALSE]
    display <- as.data.frame(lapply(names(shown), function(name) {
        value <- epwshiftr_cli_format_named_cell(shown[[name]], name)
        epwshiftr_cli_truncate_cell(value, epwshiftr_cli_column_max_width(name))
    }), stringsAsFactors = FALSE)
    names(display) <- names(shown)

    adapted <- epwshiftr_cli_adapt_table_columns(
        display = display,
        raw = shown,
        max_width = epwshiftr_cli_console_width()
    )
    display <- adapted$display
    shown <- adapted$raw

    lines <- epwshiftr_cli_table_lines(
        display,
        header = epwshiftr_cli_title(names(display)),
        align = epwshiftr_cli_table_alignments(shown)
    )
    cli::cli_verbatim(lines)
    extra <- nrow(x) - nrow(shown)
    if (extra > 0L) {
        cli::cli_alert_info("{extra} more rows; use --json for full output.")
    }
    if (length(adapted$dropped)) {
        hidden <- paste(adapted$dropped, collapse = ", ")
        cli::cli_alert_info("Hidden columns for console width: {hidden}. Use --json for full output.")
    }
    invisible(NULL)
}


epwshiftr_cli_as_data_frame <- function(x) {
    if (is.null(x)) {
        return(data.frame())
    }
    if (data.table::is.data.table(x)) {
        return(as.data.frame(x))
    }
    if (is.data.frame(x)) {
        return(as.data.frame(x))
    }
    if (is.list(x) && all(vapply(x, epwshiftr_cli_is_scalar_value, logical(1L)))) {
        return(as.data.frame(x, stringsAsFactors = FALSE))
    }
    data.frame(value = epwshiftr_cli_format_cell(x), stringsAsFactors = FALSE)
}


epwshiftr_cli_pick_columns <- function(x, columns = NULL) {
    available <- names(x)
    if (is.null(columns)) {
        return(available)
    }
    picked <- intersect(columns, available)
    if (length(picked)) {
        return(picked)
    }
    available
}


epwshiftr_cli_column_max_width <- function(name) {
    name <- tolower(name)
    if (grepl("url|path|detail|message|error|hint", name)) {
        return(64L)
    }
    if (grepl("(^|_)id$|_id$|key", name)) {
        return(28L)
    }
    if (grepl("filename|label|title", name)) {
        return(40L)
    }
    24L
}


epwshiftr_cli_table_lines <- function(x, header = names(x), align = NULL, border = "single") {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    header <- as.character(header)
    if (!ncol(x)) {
        return(character())
    }
    if (is.null(align)) {
        align <- rep("left", ncol(x))
    }
    align <- rep_len(align, ncol(x))
    chars <- epwshiftr_cli_table_border(border)
    body <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    header <- rep_len(header, ncol(body))
    widths <- vapply(seq_along(body), function(i) {
        max(cli::ansi_nchar(c(header[[i]], body[[i]]), type = "width"), na.rm = TRUE)
    }, integer(1L))

    lines <- c(
        epwshiftr_cli_table_rule(widths, chars, "top"),
        epwshiftr_cli_table_row(cli::style_bold(header), widths, rep("left", length(widths)), chars),
        epwshiftr_cli_table_rule(widths, chars, "mid")
    )
    for (i in seq_len(nrow(body))) {
        row <- vapply(body, `[[`, character(1L), i)
        lines <- c(lines, epwshiftr_cli_table_row(row, widths, align, chars))
    }
    c(lines, epwshiftr_cli_table_rule(widths, chars, "bottom"))
}


epwshiftr_cli_adapt_table_columns <- function(display, raw, max_width = epwshiftr_cli_console_width(), min_columns = 2L) {
    if (!ncol(display)) {
        return(list(display = display, raw = raw, dropped = character()))
    }
    max_width <- as.integer(max_width[[1L]])
    if (is.na(max_width) || max_width <= 0L) {
        max_width <- 80L
    }
    min_columns <- max(1L, min(as.integer(min_columns[[1L]]), ncol(display)))
    dropped <- character()
    while (ncol(display) > min_columns && epwshiftr_cli_table_width(display, raw) > max_width) {
        drop <- names(display)[[ncol(display)]]
        dropped <- c(drop, dropped)
        display <- display[-ncol(display)]
        raw <- raw[-ncol(raw)]
    }
    list(display = display, raw = raw, dropped = dropped)
}


epwshiftr_cli_table_width <- function(display, raw) {
    lines <- epwshiftr_cli_table_lines(
        display,
        header = epwshiftr_cli_title(names(display)),
        align = epwshiftr_cli_table_alignments(raw)
    )
    if (!length(lines)) {
        return(0L)
    }
    max(cli::ansi_nchar(lines, type = "width"), na.rm = TRUE)
}


epwshiftr_cli_console_width <- function() {
    width <- getOption("width", 80L)
    width <- suppressWarnings(as.integer(width[[1L]]))
    if (is.na(width) || width < 40L) {
        return(80L)
    }
    width
}


epwshiftr_cli_table_border <- function(border = "single") {
    border <- match.arg(border, c("single", "ascii", "none"))
    if (identical(border, "single") && !isTRUE(cli::is_utf8_output())) {
        border <- "ascii"
    }
    switch(
        border,
        single = list(
            top = c(left = "\u250c", mid = "\u252c", right = "\u2510"),
            middle = c(left = "\u251c", mid = "\u253c", right = "\u2524"),
            bottom = c(left = "\u2514", mid = "\u2534", right = "\u2518"),
            horizontal = "\u2500",
            vertical = "\u2502",
            margin = 1L
        ),
        ascii = list(
            top = c(left = "+", mid = "+", right = "+"),
            middle = c(left = "+", mid = "+", right = "+"),
            bottom = c(left = "+", mid = "+", right = "+"),
            horizontal = "-",
            vertical = "|",
            margin = 1L
        ),
        none = list(
            top = c(left = "", mid = "", right = ""),
            middle = c(left = "", mid = "", right = ""),
            bottom = c(left = "", mid = "", right = ""),
            horizontal = "",
            vertical = "  ",
            margin = 0L
        )
    )
}


epwshiftr_cli_table_rule <- function(widths, chars, position = c("top", "mid", "bottom")) {
    position <- match.arg(position)
    pieces <- switch(position, top = chars$top, mid = chars$middle, bottom = chars$bottom)
    cell_widths <- widths + chars$margin * 2L
    cells <- vapply(cell_widths, function(width) paste(rep(chars$horizontal, width), collapse = ""), character(1L))
    paste0(pieces[["left"]], paste(cells, collapse = pieces[["mid"]]), pieces[["right"]])
}


epwshiftr_cli_table_row <- function(values, widths, align, chars) {
    values <- as.character(values)
    cells <- vapply(seq_along(values), function(i) {
        padded <- epwshiftr_cli_table_pad(values[[i]], widths[[i]], align[[i]])
        paste0(
            paste(rep(" ", chars$margin), collapse = ""),
            padded,
            paste(rep(" ", chars$margin), collapse = "")
        )
    }, character(1L))
    paste0(chars$vertical, paste(cells, collapse = chars$vertical), chars$vertical)
}


epwshiftr_cli_table_pad <- function(value, width, align = "left") {
    value <- as.character(value[[1L]])
    visible <- cli::ansi_nchar(value, type = "width")
    pad <- max(0L, as.integer(width) - as.integer(visible))
    left <- switch(
        align,
        right = pad,
        center = floor(pad / 2L),
        0L
    )
    right <- pad - left
    paste0(strrep(" ", left), value, strrep(" ", right))
}


epwshiftr_cli_table_alignments <- function(x) {
    vapply(names(x), function(name) {
        value <- x[[name]]
        if (is.numeric(value) || is.integer(value)) {
            return("right")
        }
        if (grepl("bytes|size|count|attempt|priority|latency|rate|total|done|missing|queued|error|cancelled|skipped", name, ignore.case = TRUE)) {
            return("right")
        }
        "left"
    }, character(1L))
}


epwshiftr_cli_truncate_cell <- function(x, width) {
    x <- as.character(x)
    width <- as.integer(width[[1L]])
    if (is.na(width) || width < 4L) {
        return(x)
    }
    vapply(x, function(value) {
        if (is.na(value) || nchar(value, type = "width") <= width) {
            return(value)
        }
        paste0(substr(value, 1L, width - 3L), "...")
    }, character(1L))
}


epwshiftr_cli_format_named_cell <- function(x, name) {
    if (grepl("bytes|size", name, ignore.case = TRUE)) {
        return(epwshiftr_cli_format_bytes(x))
    }
    if (grepl("(^|_)at$|time|date|until", name, ignore.case = TRUE) && inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
        return(epwshiftr_cli_format_time(x))
    }
    epwshiftr_cli_format_cell(x)
}


epwshiftr_cli_format_cell <- function(x) {
    if (is.null(x)) {
        return("-")
    }
    if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
        return(epwshiftr_cli_format_time(x))
    }
    if (is.list(x) && !is.data.frame(x)) {
        return(vapply(x, function(value) paste(epwshiftr_cli_format_cell(value), collapse = ","), character(1L)))
    }
    if (is.logical(x)) {
        out <- ifelse(is.na(x), "-", ifelse(x, "yes", "no"))
        return(as.character(out))
    }
    if (length(x) == 0L) {
        return("-")
    }
    out <- as.character(x)
    out[is.na(out) | !nzchar(out)] <- "-"
    out
}


epwshiftr_cli_format_time <- function(x) {
    out <- as.character(x)
    ok <- !is.na(x)
    if (inherits(x, c("POSIXct", "POSIXlt"))) {
        out[ok] <- format(as.POSIXct(x[ok], tz = "UTC"), "%Y-%m-%d %H:%M:%S %Z")
    }
    out[!ok | !nzchar(out)] <- "-"
    out
}


epwshiftr_cli_format_bytes <- function(x) {
    if (is.null(x) || !length(x)) {
        return("-")
    }
    bytes <- suppressWarnings(as.numeric(x))
    out <- rep("-", length(bytes))
    ok <- !is.na(bytes)
    if (!any(ok)) {
        return(out)
    }
    units <- c("B", "KB", "MB", "GB", "TB", "PB")
    for (i in which(ok)) {
        value <- bytes[[i]]
        unit <- 1L
        while (abs(value) >= 1024 && unit < length(units)) {
            value <- value / 1024
            unit <- unit + 1L
        }
        out[[i]] <- if (unit == 1L) {
            sprintf("%.0f %s", value, units[[unit]])
        } else {
            sprintf("%.1f %s", value, units[[unit]])
        }
    }
    out
}


epwshiftr_cli_title <- function(x) {
    x <- gsub("_", " ", as.character(x), fixed = TRUE)
    x <- gsub("-", " ", x, fixed = TRUE)
    vapply(strsplit(x, " ", fixed = TRUE), function(parts) {
        parts <- parts[nzchar(parts)]
        if (!length(parts)) {
            return("")
        }
        parts <- paste0(toupper(substring(parts, 1L, 1L)), substring(parts, 2L))
        paste(parts, collapse = " ")
    }, character(1L))
}


epwshiftr_cli_is_scalar_value <- function(x) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (is.data.frame(x)) {
        return(FALSE)
    }
    if (is.list(x)) {
        return(FALSE)
    }
    length(x) <= 1L
}


epwshiftr_cli_string_default <- function(x, y) {
    if (is.null(x) || !length(x) || is.na(x[[1L]]) || !nzchar(as.character(x[[1L]]))) y else x
}
