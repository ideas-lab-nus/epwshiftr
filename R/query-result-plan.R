#' @include query-result.R
NULL

# Rank source candidates and construct download plans from query results.

query_result__node_policy <- function(node_policy = NULL) {
    if (exists("downloader__node_policy_defaults", mode = "function")) {
        return(downloader__node_policy_defaults(node_policy))
    }
    if (is.null(node_policy)) {
        return(list(history_ttl_seconds = 14L * 24L * 3600L))
    }
    node_policy
}

query_result__node_stats <- function(
    node_stats,
    service = "HTTPServer",
    node_policy = NULL
) {
    if (is.null(node_stats)) {
        return(NULL)
    }
    node_policy <- query_result__node_policy(node_policy)
    stats <- data.table::as.data.table(node_stats)
    required <- c(
        "data_node",
        "service",
        "success_count",
        "failure_count",
        "avg_latency"
    )
    if (!all(required %in% names(stats))) {
        return(NULL)
    }
    wanted_service <- service
    stats <- stats[stats[["service"]] == wanted_service]
    if (!nrow(stats)) {
        return(NULL)
    }
    if (
        "updated_at" %in%
            names(stats) &&
            !is.null(node_policy$history_ttl_seconds)
    ) {
        updated_at <- as.POSIXct(stats$updated_at, tz = "UTC")
        fresh <- is.na(updated_at) |
            updated_at >= Sys.time() - node_policy$history_ttl_seconds
        stats <- stats[fresh]
        if (!nrow(stats)) {
            return(NULL)
        }
    }
    stats[, node_success_count := suppressWarnings(as.integer(success_count))]
    stats[, node_failure_count := suppressWarnings(as.integer(failure_count))]
    stats[is.na(node_success_count), node_success_count := 0L]
    stats[is.na(node_failure_count), node_failure_count := 0L]
    stats[, node_attempt_count := node_success_count + node_failure_count]
    stats[,
        node_success_rate := data.table::fifelse(
            node_attempt_count > 0L,
            node_success_count / node_attempt_count,
            NA_real_
        )
    ]
    stats[, node_avg_latency := suppressWarnings(as.numeric(avg_latency))]
    if ("probe_success_count" %in% names(stats)) {
        stats[,
            node_probe_success_count := suppressWarnings(as.integer(
                probe_success_count
            ))
        ]
        stats[is.na(node_probe_success_count), node_probe_success_count := 0L]
    } else {
        stats[, node_probe_success_count := NA_integer_]
    }
    if ("probe_failure_count" %in% names(stats)) {
        stats[,
            node_probe_failure_count := suppressWarnings(as.integer(
                probe_failure_count
            ))
        ]
        stats[is.na(node_probe_failure_count), node_probe_failure_count := 0L]
    } else {
        stats[, node_probe_failure_count := NA_integer_]
    }
    if ("cooldown_until" %in% names(stats)) {
        stats[, node_cooldown_until := as.POSIXct(cooldown_until, tz = "UTC")]
        stats[,
            node_is_cooling_down := !is.na(node_cooldown_until) &
                node_cooldown_until > Sys.time()
        ]
    } else {
        stats[, node_cooldown_until := as.POSIXct(NA)]
        stats[, node_is_cooling_down := FALSE]
    }
    if ("updated_at" %in% names(stats)) {
        stats[, node_updated_at := as.POSIXct(updated_at, tz = "UTC")]
    } else {
        stats[, node_updated_at := as.POSIXct(NA)]
    }
    if ("last_probe_at" %in% names(stats)) {
        stats[, node_last_probe_at := as.POSIXct(last_probe_at, tz = "UTC")]
    } else {
        stats[, node_last_probe_at := as.POSIXct(NA)]
    }
    stats[, .(
        data_node,
        service,
        node_success_count,
        node_failure_count,
        node_attempt_count,
        node_success_rate,
        node_avg_latency,
        node_probe_success_count,
        node_probe_failure_count,
        node_cooldown_until,
        node_is_cooling_down,
        node_updated_at,
        node_last_probe_at
    )]
}

query_result__apply_nodes <- function(
    plan,
    node_stats,
    service = "HTTPServer",
    node_policy = NULL
) {
    stats <- query_result__node_stats(
        node_stats,
        service = service,
        node_policy = node_policy
    )
    if (is.null(stats) || !nrow(plan)) {
        plan[, `:=`(
            node_success_count = NA_integer_,
            node_failure_count = NA_integer_,
            node_attempt_count = NA_integer_,
            node_success_rate = NA_real_,
            node_avg_latency = NA_real_,
            node_probe_success_count = NA_integer_,
            node_probe_failure_count = NA_integer_,
            node_cooldown_until = as.POSIXct(NA),
            node_is_cooling_down = FALSE,
            node_updated_at = as.POSIXct(NA),
            node_last_probe_at = as.POSIXct(NA),
            node_cooldown_rank = 0L
        )]
        return(plan[])
    }
    out <- merge(
        plan,
        stats,
        by = c("data_node", "service"),
        all.x = TRUE,
        sort = FALSE
    )
    out[is.na(node_is_cooling_down), node_is_cooling_down := FALSE]
    out[,
        node_cooldown_rank := data.table::fifelse(node_is_cooling_down, 1L, 0L)
    ]
    out[,
        all_candidates_cooling := all(node_cooldown_rank == 1L),
        by = "logical_file_id"
    ]
    out[all_candidates_cooling %in% TRUE, node_cooldown_rank := 0L]
    out[, all_candidates_cooling := NULL]
    out[]
}

query_result__download_plan <- function(
    result,
    service = "HTTPServer",
    probe = FALSE,
    strategy = c("fastest", "first", "stable"),
    node_stats = NULL,
    network_policy = NULL,
    node_policy = NULL,
    probe_concurrency = 1L,
    probe_cache_seconds = 3600L
) {
    strategy <- match.arg(strategy)
    checkmate::assert_string(service)
    checkmate::assert_flag(probe)
    checkmate::assert_count(probe_concurrency, positive = TRUE)
    checkmate::assert_count(probe_cache_seconds, positive = FALSE)

    dt <- result$to_data_table()
    n <- nrow(dt)
    if (!n) {
        return(data.table::data.table())
    }
    urls <- priv(result)$get_url(service, service)
    filename <- if ("filename" %in% result$fields) {
        result$filename
    } else {
        query_result__col(dt, "title")
    }
    plan <- data.table::data.table(
        logical_file_id = query_result__file_key(dt),
        record_index = seq_len(n),
        file_key = query_result__col(dt, "file_key"),
        esgf_id = query_result__col(dt, "id"),
        dataset_id = query_result__col(dt, "dataset_id"),
        filename = filename,
        subdir = NA_character_,
        checksum = query_result__col(dt, "checksum"),
        checksum_type = tolower(query_result__col(
            dt,
            "checksum_type",
            "sha256"
        )),
        size = suppressWarnings(as.numeric(query_result__col(
            dt,
            "size",
            NA_real_
        ))),
        url = urls,
        service = service,
        data_node = query_result__col(dt, "data_node"),
        priority = seq_len(n),
        probe_latency = NA_real_,
        probe_throughput = NA_real_,
        probe_cached = FALSE
    )
    plan <- plan[!is.na(url) & nzchar(url)]
    if (!nrow(plan)) {
        return(plan)
    }
    if (probe) {
        probes <- query_result__latency_table(
            plan$url,
            data_node = plan$data_node,
            service = service,
            network_policy = network_policy,
            node_stats = node_stats,
            node_policy = node_policy,
            probe_concurrency = probe_concurrency,
            probe_cache_seconds = probe_cache_seconds
        )
        plan[, probe_latency := probes$probe_latency]
        plan[, probe_throughput := probes$probe_throughput]
        plan[, probe_cached := probes$probe_cached]
    }
    plan <- query_result__apply_nodes(
        plan,
        node_stats = node_stats,
        service = service,
        node_policy = node_policy
    )
    if (identical(strategy, "fastest")) {
        plan[, probe_missing := is.na(probe_latency)]
        plan[, node_missing := is.na(node_success_rate)]
        data.table::setorderv(
            plan,
            c(
                "logical_file_id",
                "node_cooldown_rank",
                "probe_missing",
                "probe_latency",
                "node_missing",
                "node_success_rate",
                "node_avg_latency",
                "priority"
            ),
            c(1L, 1L, 1L, 1L, 1L, -1L, 1L, 1L)
        )
        plan[, c("probe_missing", "node_missing") := NULL]
    } else if (identical(strategy, "stable")) {
        plan[, node_missing := is.na(node_success_rate)]
        data.table::setorderv(
            plan,
            c(
                "logical_file_id",
                "node_cooldown_rank",
                "node_missing",
                "node_success_rate",
                "data_node",
                "url",
                "priority"
            ),
            c(1L, 1L, 1L, -1L, 1L, 1L, 1L)
        )
        plan[, node_missing := NULL]
    } else {
        data.table::setorderv(plan, c("logical_file_id", "priority"))
    }
    plan[, priority := seq_len(.N), by = "logical_file_id"]
    plan[]
}

# query_result__resolve_downloader {{{
# Resolve the shared downloader contract used by public download methods and
# HTTP fallback paths.
query_result__resolve_downloader <- function(
    downloader = NULL,
    store = NULL,
    message
) {
    if (!is.null(downloader)) {
        return(downloader)
    }
    if (!is.null(store)) {
        return(store$downloader())
    }

    cli::cli_abort(message)
}

# query_result__download {{{
# Shared implementation for File and Aggregation result downloads. The public
# wrappers keep class-specific argument defaults and delegate the common work here.
query_result__download <- function(
    result,
    downloader = NULL,
    store = NULL,
    replica,
    service = "HTTPServer",
    probe = TRUE,
    strategy = c("fastest", "first", "stable"),
    probe_concurrency = NULL,
    probe_cache_seconds = 3600L,
    session_label = NULL,
    run = TRUE,
    ...
) {
    strategy <- match.arg(strategy)
    downloader <- query_result__resolve_downloader(
        downloader,
        store,
        "`download()` requires an explicit `store` or persistent `downloader`."
    )

    # Reuse downloader history and network settings so ranking stays consistent
    # with explicit calls to $download_plan().
    node_stats <- tryCatch(
        downloader$data_nodes(service = service),
        error = function(e) NULL
    )
    network_policy <- tryCatch(downloader$network_policy, error = function(e) {
        NULL
    })
    node_policy <- tryCatch(downloader$node_policy, error = function(e) NULL)
    if (is.null(probe_concurrency)) {
        probe_concurrency <- min(max(downloader$n_workers, 1L), 8L)
    }

    # Let each result class keep its own $download_plan() replica semantics.
    plan <- result$download_plan(
        replica = replica,
        service = service,
        probe = probe,
        strategy = strategy,
        node_stats = node_stats,
        network_policy = network_policy,
        node_policy = node_policy,
        probe_concurrency = probe_concurrency,
        probe_cache_seconds = probe_cache_seconds
    )
    tryCatch(
        downloader$record_probes(plan, probed = probe),
        error = function(e) NULL
    )
    session_id <- downloader$enqueue(plan, session_label = session_label)
    if (isTRUE(run)) {
        downloader$run(session_id = session_id, ...)
    }

    session_id
}
