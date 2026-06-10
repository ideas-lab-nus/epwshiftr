epwshiftr_cli_download <- function(store, command, args) {
    if (identical(command, "config")) {
        return(epwshiftr_cli_download_config(store, args))
    }
    if (identical(command, "preflight")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = c("--probe", "--no-probe"),
            options = c("--replica", "--service", "--strategy", "--probe-concurrency", "--probe-cache-seconds")
        )
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        return(do.call(store$download_preflight, c(
            list(query_id = query_id, downloader = epwshiftr_cli_downloader(store)),
            epwshiftr_cli_download_plan_args(parsed)
        )))
    }
    if (identical(command, "run")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = c("--probe", "--no-probe", "--overwrite", "--no-resume", "--no-progress"),
            options = c("--replica", "--service", "--strategy", "--probe-concurrency", "--probe-cache-seconds", "--session-label")
        )
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        return(do.call(store$download_query, c(
            list(
                query_id = query_id,
                downloader = epwshiftr_cli_downloader(store),
                session_label = parsed$options[["--session-label"]],
                overwrite = parsed$flags[["--overwrite"]],
                resume = !parsed$flags[["--no-resume"]],
                progress = !parsed$flags[["--no-progress"]]
            ),
            epwshiftr_cli_download_plan_args(parsed)
        )))
    }
    if (identical(command, "status")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--query", "--session"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$download_status(
            query_id = parsed$options[["--query"]],
            session_id = parsed$options[["--session"]],
            downloader = epwshiftr_cli_downloader(store)
        ))
    }
    if (identical(command, "sessions")) {
        parsed <- epwshiftr_cli_parse_command(args)
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_downloader(store)$sessions())
    }
    if (identical(command, "tasks")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--session", "--status"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_downloader(store)$tasks(
            session_id = parsed$options[["--session"]],
            status = epwshiftr_cli_csv(parsed$options[["--status"]])
        ))
    }
    if (identical(command, "events")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--session", "--task"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_downloader(store)$events(
            session_id = parsed$options[["--session"]],
            task_id = epwshiftr_cli_csv(parsed$options[["--task"]])
        ))
    }
    if (identical(command, "watch")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--query", "--session", "--events"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_download_watch(
            store = store,
            query_id = parsed$options[["--query"]],
            session_id = parsed$options[["--session"]],
            event_count = epwshiftr_cli_count_or_default(parsed$options[["--events"]], "--events", 10L, positive = FALSE)
        ))
    }
    if (identical(command, "logs")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--session", "--task", "--tail"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_download_logs(
            downloader = epwshiftr_cli_downloader(store),
            session_id = parsed$options[["--session"]],
            task_id = epwshiftr_cli_csv(parsed$options[["--task"]]),
            tail = epwshiftr_cli_count_or_default(parsed$options[["--tail"]], "--tail", 50L, positive = FALSE)
        ))
    }
    if (identical(command, "resume")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = c("--overwrite", "--no-progress"), options = c("--session", "--task"))
        epwshiftr_cli_assert_no_positionals(parsed)
        downloader <- epwshiftr_cli_downloader(store)
        out <- downloader$resume(
            session_id = parsed$options[["--session"]],
            task_id = epwshiftr_cli_csv(parsed$options[["--task"]]),
            overwrite = parsed$flags[["--overwrite"]],
            progress = !parsed$flags[["--no-progress"]]
        )
        store$sync_downloads(downloader)
        return(out)
    }
    if (identical(command, "verify")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--session", "--task"))
        epwshiftr_cli_assert_no_positionals(parsed)
        downloader <- epwshiftr_cli_downloader(store)
        out <- downloader$verify(
            session_id = parsed$options[["--session"]],
            task_id = epwshiftr_cli_csv(parsed$options[["--task"]])
        )
        store$sync_downloads(downloader)
        return(out)
    }
    if (identical(command, "cancel")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--session", "--task"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_downloader(store)$cancel(
            session_id = parsed$options[["--session"]],
            task_id = epwshiftr_cli_csv(parsed$options[["--task"]])
        ))
    }
    if (identical(command, "nodes")) {
        parsed <- epwshiftr_cli_parse_command(args, options = "--service")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(data.table::as.data.table(epwshiftr_cli_downloader(store)$data_nodes(service = parsed$options[["--service"]])))
    }
    if (identical(command, "reset-nodes")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--execute", options = c("--node", "--service"))
        epwshiftr_cli_assert_no_positionals(parsed)
        downloader <- epwshiftr_cli_downloader(store)
        if (isTRUE(parsed$flags[["--execute"]])) {
            return(downloader$reset_data_nodes(data_node = parsed$options[["--node"]], service = parsed$options[["--service"]]))
        }
        nodes <- data.table::as.data.table(downloader$data_nodes(service = parsed$options[["--service"]]))
        if (!is.null(parsed$options[["--node"]]) && nrow(nodes)) {
            nodes <- nodes[nodes[["data_node"]] == parsed$options[["--node"]]]
        }
        nodes[, dry_run := TRUE]
        return(nodes[])
    }
    if (identical(command, "retry")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--run", options = c("--query", "--session", "--status"))
        epwshiftr_cli_assert_no_positionals(parsed)
        status <- epwshiftr_cli_csv(parsed$options[["--status"]])
        if (is.null(status)) {
            status <- c("error", "cancelled")
        }
        return(store$retry_downloads(
            query_id = parsed$options[["--query"]],
            session_id = parsed$options[["--session"]],
            downloader = epwshiftr_cli_downloader(store),
            status = status,
            run = parsed$flags[["--run"]]
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown download command: %s", command))
}


epwshiftr_cli_download_watch <- function(store, query_id = NULL, session_id = NULL, event_count = 10L) {
    downloader <- epwshiftr_cli_downloader(store)
    tasks <- data.table::as.data.table(store$download_status(
        query_id = query_id,
        session_id = session_id,
        downloader = downloader
    ))
    task_id <- if (nrow(tasks) && "task_id" %in% names(tasks)) tasks$task_id else NULL
    events <- if (!is.null(query_id) && !nrow(tasks)) {
        data.table::data.table()
    } else {
        epwshiftr_cli_download_logs(
            downloader = downloader,
            session_id = session_id,
            task_id = task_id,
            tail = event_count
        )
    }
    nodes <- data.table::as.data.table(downloader$data_nodes())
    list(
        summary = epwshiftr_cli_download_watch_summary(tasks, downloader, session_id),
        tasks = tasks[],
        nodes = nodes[],
        events = events
    )
}


epwshiftr_cli_download_logs <- function(downloader, session_id = NULL, task_id = NULL, tail = 50L) {
    events <- data.table::as.data.table(downloader$events(session_id = session_id, task_id = task_id))
    if (nrow(events) && "created_at" %in% names(events)) {
        data.table::setorderv(events, "created_at", 1L)
    }
    epwshiftr_cli_tail_rows(events, tail)
}


epwshiftr_cli_download_watch_summary <- function(tasks, downloader, session_id = NULL) {
    statuses <- c("queued", "downloading", "done", "error", "cancelled", "skipped")
    counts <- stats::setNames(integer(length(statuses)), statuses)
    if (nrow(tasks) && "status" %in% names(tasks)) {
        observed <- table(factor(tasks$status, levels = statuses))
        counts[] <- as.integer(observed)
    }
    bytes_done <- if (nrow(tasks) && "bytes_done" %in% names(tasks)) {
        sum(suppressWarnings(as.numeric(tasks$bytes_done)), na.rm = TRUE)
    } else {
        0
    }
    bytes_total <- if (nrow(tasks) && "size" %in% names(tasks)) {
        sum(suppressWarnings(as.numeric(tasks$size)), na.rm = TRUE)
    } else {
        NA_real_
    }
    last_error <- NA_character_
    if (nrow(tasks) && "last_error" %in% names(tasks)) {
        errors <- tasks$last_error[!is.na(tasks$last_error) & nzchar(tasks$last_error)]
        if (length(errors)) {
            last_error <- tail(errors, 1L)
        }
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
        download_incomplete = as.integer(sum(counts[c("queued", "downloading", "error", "cancelled")])),
        download_retryable = as.integer(sum(counts[c("error", "cancelled")])),
        last_download_session_id = epwshiftr_cli_download_last_session_id(tasks, downloader, session_id),
        last_error = last_error,
        stringsAsFactors = FALSE
    )
}


epwshiftr_cli_download_last_session_id <- function(tasks, downloader, session_id = NULL) {
    if (!is.null(session_id)) {
        return(session_id)
    }
    if (nrow(tasks) && "session_id" %in% names(tasks)) {
        sessions <- tasks$session_id[!is.na(tasks$session_id) & nzchar(tasks$session_id)]
        if (length(sessions)) {
            return(tail(sessions, 1L))
        }
    }
    sessions <- data.table::as.data.table(tryCatch(downloader$sessions(), error = function(e) data.frame()))
    if (!nrow(sessions) || !"session_id" %in% names(sessions)) {
        return(NA_character_)
    }
    if ("created_at" %in% names(sessions)) {
        data.table::setorderv(sessions, "created_at", 1L)
    }
    tail(sessions$session_id, 1L)
}


epwshiftr_cli_tail_rows <- function(rows, n) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows) || n <= 0L) {
        return(rows[0L])
    }
    rows[seq.int(max(1L, nrow(rows) - n + 1L), nrow(rows))]
}


epwshiftr_cli_download_plan_args <- function(parsed) {
    args <- list()
    for (name in c("--replica", "--service", "--strategy")) {
        if (!is.null(parsed$options[[name]])) {
            key <- gsub("-", "_", sub("^--", "", name))
            args[[key]] <- parsed$options[[name]]
        }
    }
    if (isTRUE(parsed$flags[["--probe"]]) && isTRUE(parsed$flags[["--no-probe"]])) {
        epwshiftr_cli_usage_abort("Use only one of --probe or --no-probe.")
    }
    if (isTRUE(parsed$flags[["--probe"]])) {
        args$probe <- TRUE
    }
    if (isTRUE(parsed$flags[["--no-probe"]])) {
        args$probe <- FALSE
    }
    if (!is.null(parsed$options[["--probe-concurrency"]])) {
        args$probe_concurrency <- epwshiftr_cli_count(parsed$options[["--probe-concurrency"]], "--probe-concurrency")
    }
    if (!is.null(parsed$options[["--probe-cache-seconds"]])) {
        args$probe_cache_seconds <- epwshiftr_cli_count(parsed$options[["--probe-cache-seconds"]], "--probe-cache-seconds")
    }
    args
}


epwshiftr_cli_download_config <- function(store, args) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort("Missing download config command: show or set.")
    }
    action <- args[[1L]]
    rest <- args[-1L]
    if (identical(action, "show")) {
        parsed <- epwshiftr_cli_parse_command(rest)
        epwshiftr_cli_assert_no_positionals(parsed)
        return(epwshiftr_cli_downloader_config(epwshiftr_cli_downloader(store)))
    }
    if (identical(action, "set")) {
        parsed <- epwshiftr_cli_parse_command(
            rest,
            options = c(
                "--workers", "--retries", "--timeout", "--connect-timeout",
                "--proxy", "--useragent", "--ssl-verifypeer",
                "--chunk-size", "--bandwidth-limit", "--low-speed-limit", "--low-speed-time",
                "--host-concurrency", "--disk-preflight", "--min-free-space",
                "--cooldown-after-failures", "--cooldown-seconds", "--history-ttl-seconds", "--min-attempts"
            )
        )
        epwshiftr_cli_assert_no_positionals(parsed)
        current <- epwshiftr_cli_downloader(store)
        network <- current$network_policy
        node_policy <- current$node_policy
        transfer_policy <- current$transfer_policy
        resource_policy <- current$resource_policy
        params <- list(
            retries = current$max_retries,
            timeout = current$timeout,
            ssl_verifypeer = network$ssl_verifypeer,
            proxy = network$proxy,
            connect_timeout = network$connect_timeout,
            useragent = network$useragent,
            n_workers = current$n_workers,
            node_policy = node_policy,
            transfer_policy = transfer_policy,
            resource_policy = resource_policy
        )
        params <- epwshiftr_cli_apply_download_config_options(params, parsed$options)
        paths <- epwshiftr_cli_downloader_paths(store)
        downloader <- do.call(Downloader$new, c(list(
            dest = paths$dest,
            temp = paths$temp,
            manifest = paths$manifest
        ), params))
        downloader$save_config(paths$config)
        return(epwshiftr_cli_downloader_config(downloader))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown download config command: %s", action))
}


epwshiftr_cli_downloader_paths <- function(store) {
    private <- tryCatch(store$.__enclos_env__$private, error = function(e) NULL)
    if (is.null(private) || is.null(private$download_dir) || is.null(private$tmp_download_dir)) {
        cli::cli_abort("Cannot resolve store downloader paths.")
    }
    list(
        dest = private$download_dir,
        temp = private$tmp_download_dir,
        manifest = file.path(private$download_dir, "_downloader", "manifest.duckdb"),
        config = file.path(private$download_dir, "_downloader", "config.json")
    )
}


epwshiftr_cli_downloader <- function(store, ...) {
    paths <- epwshiftr_cli_downloader_paths(store)
    config <- if (file.exists(paths$config)) paths$config else NULL
    Downloader$new(
        dest = paths$dest,
        temp = paths$temp,
        manifest = paths$manifest,
        config = config,
        ...
    )
}


epwshiftr_cli_downloader_config <- function(downloader) {
    list(
        config_file = downloader$config_file,
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


epwshiftr_cli_apply_download_config_options <- function(params, options) {
    set_count <- function(option, target, positive = TRUE) {
        if (!is.null(options[[option]])) {
            params[[target]] <<- epwshiftr_cli_count(options[[option]], option, positive = positive)
        }
    }
    set_nullable_count <- function(option, policy, field, positive = TRUE) {
        if (!is.null(options[[option]])) {
            params[[policy]][[field]] <<- epwshiftr_cli_count_or_null(options[[option]], option, positive = positive)
        }
    }
    set_count("--workers", "n_workers", positive = FALSE)
    set_count("--retries", "retries")
    set_count("--timeout", "timeout")
    if (!is.null(options[["--connect-timeout"]])) {
        params$connect_timeout <- epwshiftr_cli_count_or_null(options[["--connect-timeout"]], "--connect-timeout")
    }
    if (!is.null(options[["--proxy"]])) {
        params$proxy <- epwshiftr_cli_string_or_null(options[["--proxy"]])
    }
    if (!is.null(options[["--useragent"]])) {
        params$useragent <- epwshiftr_cli_string_or_null(options[["--useragent"]])
    }
    if (!is.null(options[["--ssl-verifypeer"]])) {
        params$ssl_verifypeer <- epwshiftr_cli_bool(options[["--ssl-verifypeer"]], "--ssl-verifypeer")
    }
    set_nullable_count("--chunk-size", "transfer_policy", "chunk_size")
    set_nullable_count("--bandwidth-limit", "transfer_policy", "bandwidth_limit")
    set_nullable_count("--low-speed-limit", "transfer_policy", "low_speed_limit")
    set_nullable_count("--low-speed-time", "transfer_policy", "low_speed_time")
    set_nullable_count("--host-concurrency", "resource_policy", "host_concurrency")
    if (!is.null(options[["--disk-preflight"]])) {
        params$resource_policy$disk_preflight <- epwshiftr_cli_bool(options[["--disk-preflight"]], "--disk-preflight")
    }
    if (!is.null(options[["--min-free-space"]])) {
        params$resource_policy$min_free_space <- epwshiftr_cli_count(options[["--min-free-space"]], "--min-free-space", positive = FALSE)
    }
    if (!is.null(options[["--cooldown-after-failures"]])) {
        params$node_policy$cooldown_after_failures <- epwshiftr_cli_count(options[["--cooldown-after-failures"]], "--cooldown-after-failures")
    }
    if (!is.null(options[["--cooldown-seconds"]])) {
        params$node_policy$cooldown_seconds <- epwshiftr_cli_count(options[["--cooldown-seconds"]], "--cooldown-seconds")
    }
    if (!is.null(options[["--history-ttl-seconds"]])) {
        params$node_policy$history_ttl_seconds <- epwshiftr_cli_count(options[["--history-ttl-seconds"]], "--history-ttl-seconds")
    }
    if (!is.null(options[["--min-attempts"]])) {
        params$node_policy$min_attempts <- epwshiftr_cli_count(options[["--min-attempts"]], "--min-attempts")
    }
    invisible(params)
}
