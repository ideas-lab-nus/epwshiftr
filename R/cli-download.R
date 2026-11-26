# cli_download__config_specs {{{
cli_download__config_specs <- function() {
    opt <- function(field, type = "count", policy = NULL, positive = TRUE) {
        list(field = field, type = type, policy = policy, positive = positive)
    }
    list(
        "--workers" = opt("n_workers", positive = FALSE),
        "--retries" = opt("retries"),
        "--timeout" = opt("timeout"),
        "--connect-timeout" = opt("connect_timeout", "count_or_null"),
        "--proxy" = opt("proxy", "string_or_null"),
        "--useragent" = opt("useragent", "string_or_null"),
        "--ssl-verifypeer" = opt("ssl_verifypeer", "bool"),
        "--chunk-size" = opt("chunk_size", "count_or_null", "transfer_policy"),
        "--bandwidth-limit" = opt(
            "bandwidth_limit",
            "count_or_null",
            "transfer_policy"
        ),
        "--low-speed-limit" = opt(
            "low_speed_limit",
            "count_or_null",
            "transfer_policy"
        ),
        "--low-speed-time" = opt(
            "low_speed_time",
            "count_or_null",
            "transfer_policy"
        ),
        "--host-concurrency" = opt(
            "host_concurrency",
            "count_or_null",
            "resource_policy"
        ),
        "--disk-preflight" = opt("disk_preflight", "bool", "resource_policy"),
        "--min-free-space" = opt(
            "min_free_space",
            "count",
            "resource_policy",
            FALSE
        ),
        "--cooldown-after-failures" = opt(
            "cooldown_after_failures",
            "count",
            "node_policy"
        ),
        "--cooldown-seconds" = opt("cooldown_seconds", "count", "node_policy"),
        "--history-ttl-seconds" = opt(
            "history_ttl_seconds",
            "count",
            "node_policy"
        ),
        "--min-attempts" = opt("min_attempts", "count", "node_policy")
    )
}
# }}}

# cli_download__parse_scope {{{
cli_download__parse_scope <- function(
    args,
    flags = character(),
    options = character(),
    tail_default = NULL
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = flags,
        options = options,
        no_positionals = TRUE
    )
    list(
        parsed = parsed,
        query_id = parsed$options[["--query"]],
        session_id = parsed$options[["--session"]],
        job_id = parsed$options[["--job"]],
        task_id = epwshiftr_cli_csv(parsed$options[["--task"]]),
        status = epwshiftr_cli_csv(parsed$options[["--status"]]),
        tail = if (!is.null(tail_default)) {
            epwshiftr_cli_count_or_default(
                parsed$options[["--tail"]],
                "--tail",
                tail_default,
                positive = FALSE
            )
        }
    )
}
# }}}

# epwshiftr_cli_download {{{
epwshiftr_cli_download <- function(
    store,
    command,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    if (identical(command, "config")) {
        return(epwshiftr_cli_download_config(store, args))
    }
    if (identical(command, "preflight")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = c("--probe", "--no-probe"),
            options = c(
                "--replica",
                "--service",
                "--strategy",
                "--probe-concurrency",
                "--probe-cache-seconds"
            )
        )
        return(do.call(
            store$download_preflight,
            c(
                list(
                    query_id = epwshiftr_cli_required_position(
                        parsed,
                        "query_id"
                    ),
                    downloader = epwshiftr_cli_downloader(store)
                ),
                epwshiftr_cli_download_plan_args(parsed)
            )
        ))
    }
    if (identical(command, "run")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = c("--probe", "--no-probe", "--background", CLI_RUN_FLAGS),
            options = c(
                "--replica",
                "--service",
                "--strategy",
                "--probe-concurrency",
                "--probe-cache-seconds",
                "--session-label",
                "--mode"
            )
        )
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        catalog <- shift_inspect__file_catalog(store, query_id)
        files <- shift_stage__new(
            ShiftFiles,
            "files",
            store_path = store$path,
            ids = list(query_id = query_id),
            meta = list(
                request = NULL,
                dataset_count = NA_integer_,
                file_count = nrow(catalog),
                variables = unique(catalog$variable_id),
                fields = names(catalog)
            )
        )
        download <- do.call(
            shift_download,
            c(
                list(
                    x = files,
                    session_label = parsed$options[["--session-label"]],
                    overwrite = parsed$flags[["--overwrite"]],
                    resume = !parsed$flags[["--no-resume"]],
                    background = parsed$flags[["--background"]],
                    mode = shift_stage__coalesce(
                        parsed$options[["--mode"]],
                        "process"
                    ),
                    ui = epwshiftr_cli_task_ui(
                        parsed,
                        json = json,
                        jsonl = jsonl,
                        quiet = quiet
                    )
                ),
                epwshiftr_cli_download_plan_args(parsed)
            )
        )
        result <- if (is.data.frame(download@meta$session)) {
            download@meta$session
        } else {
            data.table::data.table(
                session_id = as.character(download@ids$session_id)
            )
        }
        return(cli_morph__attach_ids(result, download))
    }
    if (identical(command, "status")) {
        s <- cli_download__parse_scope(
            args,
            options = c("--query", "--session")
        )
        return(store$download_status(
            query_id = s$query_id,
            session_id = s$session_id,
            downloader = epwshiftr_cli_downloader(store)
        ))
    }
    if (identical(command, "sessions")) {
        epwshiftr_cli_parse_command(args, no_positionals = TRUE)
        return(epwshiftr_cli_downloader(store)$sessions())
    }
    if (identical(command, "tasks")) {
        s <- cli_download__parse_scope(
            args,
            options = c("--session", "--job", "--status")
        )
        return(epwshiftr_cli_downloader(store)$tasks(
            session_id = s$session_id,
            job_id = s$job_id,
            status = s$status
        ))
    }
    if (identical(command, "events")) {
        s <- cli_download__parse_scope(
            args,
            options = c("--session", "--job", "--task")
        )
        return(epwshiftr_cli_downloader(store)$events(
            session_id = s$session_id,
            job_id = s$job_id,
            task_id = s$task_id
        ))
    }
    if (identical(command, "watch")) {
        s <- cli_download__parse_scope(
            args,
            flags = "--follow",
            options = c(
                "--query",
                "--session",
                "--job",
                "--events",
                "--interval",
                "--count"
            )
        )
        p <- s$parsed
        watch_args <- list(
            store = store,
            query_id = s$query_id,
            session_id = s$session_id,
            job_id = s$job_id,
            event_count = epwshiftr_cli_count_or_default(
                p$options[["--events"]],
                "--events",
                10L,
                positive = FALSE
            )
        )
        if (!isTRUE(p$flags[["--follow"]])) {
            return(do.call(epwshiftr_cli_download_watch, watch_args))
        }
        return(do.call(
            epwshiftr_cli_download_watch_follow,
            c(
                watch_args,
                list(
                    interval = epwshiftr_cli_download_interval(
                        p$options[["--interval"]],
                        1
                    ),
                    count = epwshiftr_cli_count_or_default(
                        p$options[["--count"]],
                        "--count",
                        Inf,
                        positive = FALSE
                    ),
                    jsonl = jsonl,
                    quiet = quiet
                )
            )
        ))
    }
    if (identical(command, "jobs")) {
        s <- cli_download__parse_scope(args, options = "--status")
        return(epwshiftr_cli_downloader(store)$jobs(status = s$status))
    }
    if (identical(command, "logs")) {
        s <- cli_download__parse_scope(
            args,
            options = c("--session", "--job", "--task", "--tail"),
            tail_default = 50L
        )
        dl <- epwshiftr_cli_downloader(store)
        if (!is.null(s$job_id)) {
            return(dl$job_logs(job_id = s$job_id, tail = s$tail))
        }
        return(epwshiftr_cli_download_logs(
            downloader = dl,
            session_id = s$session_id,
            task_id = s$task_id,
            tail = s$tail
        ))
    }
    if (identical(command, "stop")) {
        s <- cli_download__parse_scope(
            args,
            flags = "--force",
            options = "--job"
        )
        if (is.null(s$job_id)) {
            epwshiftr_cli_usage_abort("download stop requires --job.")
        }
        return(epwshiftr_cli_downloader(store)$stop_job(
            job_id = s$job_id,
            force = s$parsed$flags[["--force"]]
        ))
    }
    if (identical(command, "daemon")) {
        if (!length(args)) {
            epwshiftr_cli_usage_abort(
                "Missing download daemon command: start, status, or stop."
            )
        }
        action <- args[[1L]]
        rest <- args[-1L]
        dl <- epwshiftr_cli_downloader(store)
        if (identical(action, "start")) {
            p <- epwshiftr_cli_parse_command(
                rest,
                options = c("--port", "--heartbeat-interval"),
                no_positionals = TRUE
            )
            return(dl$daemon_start(
                port = epwshiftr_cli_count(p$options[["--port"]], "--port"),
                heartbeat_interval = epwshiftr_cli_download_interval(
                    p$options[["--heartbeat-interval"]],
                    5
                )
            ))
        }
        if (identical(action, "status")) {
            epwshiftr_cli_parse_command(rest, no_positionals = TRUE)
            return(dl$daemon_status())
        }
        if (identical(action, "stop")) {
            p <- epwshiftr_cli_parse_command(
                rest,
                flags = "--force",
                no_positionals = TRUE
            )
            return(dl$daemon_stop(force = p$flags[["--force"]]))
        }
        epwshiftr_cli_usage_abort(sprintf(
            "Unknown download daemon command: %s",
            action
        ))
    }
    if (command %in% c("resume", "verify", "cancel")) {
        flags <- if (identical(command, "resume")) {
            c("--overwrite", "--no-progress")
        } else {
            character()
        }
        s <- cli_download__parse_scope(
            args,
            flags = flags,
            options = c("--session", "--task")
        )
        dl <- epwshiftr_cli_downloader(store)
        out <- if (identical(command, "resume")) {
            dl$resume(
                session_id = s$session_id,
                task_id = s$task_id,
                overwrite = s$parsed$flags[["--overwrite"]],
                progress = !s$parsed$flags[["--no-progress"]] &&
                    !isTRUE(quiet) &&
                    !isTRUE(json) &&
                    !isTRUE(jsonl)
            )
        } else {
            dl[[command]](session_id = s$session_id, task_id = s$task_id)
        }
        if (!identical(command, "cancel")) {
            store$sync_downloads(dl)
        }
        return(out)
    }
    if (identical(command, "nodes")) {
        p <- epwshiftr_cli_parse_command(
            args,
            options = "--service",
            no_positionals = TRUE
        )
        return(data.table::as.data.table(epwshiftr_cli_downloader(
            store
        )$data_nodes(service = p$options[["--service"]])))
    }
    if (identical(command, "reset-nodes")) {
        p <- epwshiftr_cli_parse_command(
            args,
            flags = "--execute",
            options = c("--node", "--service"),
            no_positionals = TRUE
        )
        dl <- epwshiftr_cli_downloader(store)
        if (isTRUE(p$flags[["--execute"]])) {
            return(dl$reset_data_nodes(
                data_node = p$options[["--node"]],
                service = p$options[["--service"]]
            ))
        }
        nodes <- data.table::as.data.table(dl$data_nodes(
            service = p$options[["--service"]]
        ))
        if (!is.null(p$options[["--node"]]) && nrow(nodes)) {
            nodes <- nodes[nodes[["data_node"]] == p$options[["--node"]]]
        }
        data.table::set(
            nodes,
            j = "dry_run",
            value = rep_len(TRUE, nrow(nodes))
        )
        return(nodes[])
    }
    if (identical(command, "retry")) {
        s <- cli_download__parse_scope(
            args,
            flags = "--run",
            options = c("--query", "--session", "--status")
        )
        return(store$retry_downloads(
            query_id = s$query_id,
            session_id = s$session_id,
            downloader = epwshiftr_cli_downloader(store),
            status = shift_stage__coalesce(s$status, c("error", "cancelled")),
            run = s$parsed$flags[["--run"]]
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown download command: %s", command))
}
# }}}

# epwshiftr_cli_download_watch {{{
epwshiftr_cli_download_watch <- function(
    store,
    query_id = NULL,
    session_id = NULL,
    job_id = NULL,
    event_count = 10L
) {
    downloader <- epwshiftr_cli_downloader(store)
    tasks <- data.table::as.data.table(
        if (is.null(job_id)) {
            store$download_status(
                query_id = query_id,
                session_id = session_id,
                downloader = downloader
            )
        } else {
            downloader$tasks(session_id = session_id, job_id = job_id)
        }
    )
    task_id <- if (nrow(tasks) && "task_id" %in% names(tasks)) tasks$task_id
    events <- if (!is.null(query_id) && !nrow(tasks)) {
        data.table::data.table()
    } else {
        epwshiftr_cli_download_logs(
            downloader = downloader,
            session_id = session_id,
            job_id = job_id,
            task_id = task_id,
            tail = event_count
        )
    }
    out <- list(
        summary = epwshiftr_cli_download_watch_summary(
            tasks,
            downloader,
            session_id
        ),
        tasks = tasks[],
        nodes = data.table::as.data.table(downloader$data_nodes())[],
        events = events
    )
    if (!is.null(job_id)) {
        out$jobs <- downloader$job_status(job_id = job_id)
    }
    out
}
# }}}

# epwshiftr_cli_download_watch_follow {{{
epwshiftr_cli_download_watch_follow <- function(
    store,
    query_id = NULL,
    session_id = NULL,
    job_id = NULL,
    event_count = 10L,
    interval = 1,
    count = Inf,
    jsonl = FALSE,
    quiet = FALSE
) {
    i <- 0L
    repeat {
        i <- i + 1L
        snapshot <- epwshiftr_cli_download_watch(
            store = store,
            query_id = query_id,
            session_id = session_id,
            job_id = job_id,
            event_count = event_count
        )
        if (!isTRUE(quiet)) {
            if (isTRUE(jsonl)) {
                epwshiftr_cli_download_emit_jsonl_snapshot(snapshot)
            } else {
                cat("\014")
                epwshiftr_cli_with_theme(
                    epwshiftr_cli_render_download_watch(snapshot)
                )
            }
        }
        if (
            (!is.infinite(count) && i >= count) ||
                !epwshiftr_cli_download_snapshot_active(snapshot)
        ) {
            break
        }
        Sys.sleep(interval)
    }
    structure(snapshot, class = c("epwshiftr_cli_emitted", class(snapshot)))
}
# }}}

# epwshiftr_cli_download_logs {{{
epwshiftr_cli_download_logs <- function(
    downloader,
    session_id = NULL,
    job_id = NULL,
    task_id = NULL,
    tail = 50L
) {
    events <- data.table::as.data.table(downloader$events(
        session_id = session_id,
        job_id = job_id,
        task_id = task_id
    ))
    if (nrow(events) && "created_at" %in% names(events)) {
        data.table::setorderv(events, "created_at", 1L)
    }
    epwshiftr_cli_tail_rows(events, tail)
}
# }}}

# epwshiftr_cli_download_watch_summary {{{
epwshiftr_cli_download_watch_summary <- function(
    tasks,
    downloader,
    session_id = NULL
) {
    statuses <- c(
        "queued",
        "downloading",
        "done",
        "error",
        "cancelled",
        "skipped"
    )
    counts <- stats::setNames(integer(length(statuses)), statuses)
    if (nrow(tasks) && "status" %in% names(tasks)) {
        counts[] <- as.integer(table(factor(tasks$status, levels = statuses)))
    }
    sum_col <- function(col, default = NA_real_, require_any = FALSE) {
        if (!nrow(tasks) || !col %in% names(tasks)) {
            return(default)
        }
        vals <- suppressWarnings(as.numeric(tasks[[col]]))
        if (require_any && !any(!is.na(vals))) {
            default
        } else {
            sum(vals, na.rm = TRUE)
        }
    }
    bytes_done <- sum_col("bytes_done", default = 0)
    bytes_total <- sum_col("size")
    speed_bps <- sum_col("speed_bps", require_any = TRUE)
    eta_seconds <- if (
        !is.na(speed_bps) &&
            speed_bps > 0 &&
            !is.na(bytes_total) &&
            bytes_total > bytes_done
    ) {
        (bytes_total - bytes_done) / speed_bps
    } else {
        NA_real_
    }
    last_error <- NA_character_
    if (nrow(tasks) && "last_error" %in% names(tasks)) {
        errors <- tasks$last_error[
            !is.na(tasks$last_error) & nzchar(tasks$last_error)
        ]
        if (length(errors)) last_error <- utils::tail(errors, 1L)
    }
    data.frame(
        task_count = as.integer(nrow(tasks)),
        queued = counts[["queued"]],
        downloading = counts[["downloading"]],
        done = counts[["done"]],
        error = counts[["error"]],
        cancelled = counts[["cancelled"]],
        skipped = counts[["skipped"]],
        bytes_done = as.numeric(bytes_done),
        bytes_total = as.numeric(bytes_total),
        speed_bps = as.numeric(speed_bps),
        eta_seconds = as.numeric(eta_seconds),
        download_incomplete = as.integer(sum(counts[c(
            "queued",
            "downloading",
            "error",
            "cancelled"
        )])),
        download_retryable = as.integer(sum(counts[c("error", "cancelled")])),
        last_download_session_id = epwshiftr_cli_download_last_session_id(
            tasks,
            downloader,
            session_id
        ),
        last_error = last_error,
        stringsAsFactors = FALSE
    )
}
# }}}

# epwshiftr_cli_download_last_session_id {{{
epwshiftr_cli_download_last_session_id <- function(
    tasks,
    downloader,
    session_id = NULL
) {
    if (!is.null(session_id)) {
        return(session_id)
    }
    if (nrow(tasks) && "session_id" %in% names(tasks)) {
        sessions <- tasks$session_id[
            !is.na(tasks$session_id) & nzchar(tasks$session_id)
        ]
        if (length(sessions)) return(utils::tail(sessions, 1L))
    }
    sessions <- data.table::as.data.table(tryCatch(
        downloader$sessions(),
        error = function(e) data.frame()
    ))
    if (!nrow(sessions) || !"session_id" %in% names(sessions)) {
        return(NA_character_)
    }
    if ("created_at" %in% names(sessions)) {
        data.table::setorderv(sessions, "created_at", 1L)
    }
    utils::tail(sessions$session_id, 1L)
}
# }}}

# epwshiftr_cli_download_snapshot_active {{{
epwshiftr_cli_download_snapshot_active <- function(snapshot) {
    if (
        !is.null(snapshot$jobs) &&
            nrow(snapshot$jobs) &&
            "status" %in% names(snapshot$jobs)
    ) {
        return(any(
            snapshot$jobs$status %in% c("queued", "running", "stopping")
        ))
    }
    tasks <- snapshot$tasks
    !is.null(tasks) &&
        nrow(tasks) &&
        "status" %in% names(tasks) &&
        any(tasks$status %in% c("queued", "downloading"))
}
# }}}

# epwshiftr_cli_emit_jsonl {{{
epwshiftr_cli_emit_jsonl <- function(x) {
    cli__write_json(x, pretty = FALSE, flush = TRUE)
}
# }}}

# epwshiftr_cli_download_emit_jsonl_snapshot {{{
epwshiftr_cli_download_emit_jsonl_snapshot <- function(snapshot) {
    emitted <- FALSE
    at <- downloader__now()
    emit_rows <- function(rows, type, key) {
        if (is.null(rows) || !nrow(rows)) {
            return(invisible(NULL))
        }
        for (i in seq_len(nrow(rows))) {
            record <- list(type = type, emitted_at = at)
            record[[key]] <- epwshiftr_cli_row_object(rows, i)
            epwshiftr_cli_emit_jsonl(record)
            emitted <<- TRUE
        }
    }
    emit_rows(snapshot$jobs, "job", "job")
    emit_rows(snapshot$tasks, "progress", "task")
    emit_rows(snapshot$events, "event", "event")
    if (!isTRUE(emitted)) {
        epwshiftr_cli_emit_jsonl(list(
            type = "summary",
            emitted_at = at,
            summary = if (
                !is.null(snapshot$summary) && nrow(snapshot$summary)
            ) {
                epwshiftr_cli_row_object(snapshot$summary, 1L)
            } else {
                list()
            }
        ))
    }
    invisible(NULL)
}
# }}}

# epwshiftr_cli_row_object {{{
epwshiftr_cli_row_object <- function(x, i) {
    row <- as.data.frame(x[i, , drop = FALSE], stringsAsFactors = FALSE)
    stats::setNames(lapply(row, `[[`, 1L), names(row))
}
# }}}

# epwshiftr_cli_download_interval {{{
epwshiftr_cli_download_interval <- function(value, default) {
    if (is.null(value)) {
        return(default)
    }
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out) || out < 0) {
        epwshiftr_cli_usage_abort("Interval must be a non-negative number.")
    }
    out
}
# }}}

# epwshiftr_cli_tail_rows {{{
epwshiftr_cli_tail_rows <- function(rows, n) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows) || n <= 0L) {
        return(rows[0L])
    }
    rows[seq.int(max(1L, nrow(rows) - n + 1L), nrow(rows))]
}
# }}}

# epwshiftr_cli_download_plan_args {{{
epwshiftr_cli_download_plan_args <- function(parsed) {
    args <- list()
    for (name in c("--replica", "--service", "--strategy")) {
        if (!is.null(parsed$options[[name]])) {
            args[[gsub("-", "_", sub("^--", "", name))]] <- parsed$options[[
                name
            ]]
        }
    }
    if (
        isTRUE(parsed$flags[["--probe"]]) &&
            isTRUE(parsed$flags[["--no-probe"]])
    ) {
        epwshiftr_cli_usage_abort("Use only one of --probe or --no-probe.")
    }
    if (isTRUE(parsed$flags[["--probe"]])) {
        args$probe <- TRUE
    }
    if (isTRUE(parsed$flags[["--no-probe"]])) {
        args$probe <- FALSE
    }
    for (name in c("--probe-concurrency", "--probe-cache-seconds")) {
        if (!is.null(parsed$options[[name]])) {
            args[[gsub("-", "_", sub("^--", "", name))]] <- epwshiftr_cli_count(
                parsed$options[[name]],
                name
            )
        }
    }
    args
}
# }}}

# epwshiftr_cli_download_config {{{
epwshiftr_cli_download_config <- function(store, args) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort(
            "Missing download config command: show or set."
        )
    }
    action <- args[[1L]]
    rest <- args[-1L]
    if (identical(action, "show")) {
        epwshiftr_cli_parse_command(rest, no_positionals = TRUE)
        return(epwshiftr_cli_downloader_config(epwshiftr_cli_downloader(store)))
    }
    if (identical(action, "set")) {
        parsed <- epwshiftr_cli_parse_command(
            rest,
            options = names(cli_download__config_specs()),
            no_positionals = TRUE
        )
        current <- epwshiftr_cli_downloader(store)
        network <- current$network_policy
        params <- epwshiftr_cli_apply_download_config_options(
            list(
                retries = current$max_retries,
                timeout = current$timeout,
                ssl_verifypeer = network$ssl_verifypeer,
                proxy = network$proxy,
                connect_timeout = network$connect_timeout,
                useragent = network$useragent,
                n_workers = current$n_workers,
                node_policy = current$node_policy,
                transfer_policy = current$transfer_policy,
                resource_policy = current$resource_policy
            ),
            parsed$options
        )
        paths <- epwshiftr_cli_downloader_paths(store)
        downloader <- do.call(
            Downloader$new,
            c(
                list(
                    dest = paths$dest,
                    temp = paths$temp,
                    manifest = paths$manifest
                ),
                params
            )
        )
        return(epwshiftr_cli_downloader_config(downloader))
    }
    epwshiftr_cli_usage_abort(sprintf(
        "Unknown download config command: %s",
        action
    ))
}
# }}}

# epwshiftr_cli_downloader_paths {{{
epwshiftr_cli_downloader_paths <- function(store) {
    private <- tryCatch(store$.__enclos_env__$private, error = function(e) NULL)
    if (
        is.null(private) ||
            is.null(private$download_dir) ||
            is.null(private$tmp_download_dir)
    ) {
        cli::cli_abort("Cannot resolve store downloader paths.")
    }
    list(
        dest = private$download_dir,
        temp = private$tmp_download_dir,
        manifest = file.path(
            private$download_dir,
            "_downloader",
            "manifest.duckdb"
        )
    )
}
# }}}

# epwshiftr_cli_downloader {{{
epwshiftr_cli_downloader <- function(store, ...) {
    paths <- epwshiftr_cli_downloader_paths(store)
    Downloader$new(
        dest = paths$dest,
        temp = paths$temp,
        manifest = paths$manifest,
        ...
    )
}
# }}}

# epwshiftr_cli_downloader_config {{{
epwshiftr_cli_downloader_config <- function(downloader) {
    list(
        manifest = downloader$manifest,
        data_dir = downloader$data_dir,
        tmp_dir = downloader$tmp_dir,
        retries = downloader$max_retries,
        timeout = downloader$timeout,
        n_workers = downloader$n_workers,
        network_policy = downloader$network_policy,
        node_policy = downloader$node_policy,
        transfer_policy = downloader$transfer_policy,
        resource_policy = downloader$resource_policy
    )
}
# }}}

# epwshiftr_cli_apply_download_config_options {{{
epwshiftr_cli_apply_download_config_options <- function(params, options) {
    specs <- cli_download__config_specs()
    for (option in names(specs)) {
        raw <- options[[option]]
        if (is.null(raw)) {
            next
        }
        spec <- specs[[option]]
        value <- switch(
            spec$type,
            count = epwshiftr_cli_count(raw, option, positive = spec$positive),
            count_or_null = epwshiftr_cli_count_or_null(
                raw,
                option,
                positive = spec$positive
            ),
            string_or_null = epwshiftr_cli_string_or_null(raw),
            bool = epwshiftr_cli_bool(raw, option)
        )
        if (is.null(spec$policy)) {
            params[[spec$field]] <- value
        } else {
            params[[spec$policy]][[spec$field]] <- value
        }
    }
    invisible(params)
}
# }}}

# vim: fdm=marker :
