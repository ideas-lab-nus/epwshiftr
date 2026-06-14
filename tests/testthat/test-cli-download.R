# epwshiftr_cli_download() {{{

test_that("epwshiftr_cli_download() dispatches download workflows", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    query_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("ssp585")$
            variable_id("tas")$
            limit(1L),
        label = "cli esgf",
        track = TRUE
    )

    file_docs <- cli_test_file_docs()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            response <- cli_test_response(file_docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    updated <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "update", query_id))
    expect_equal(updated$status, 0L)

    config <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "config", "show"))
    expect_equal(config$status, 0L)
    expect_named(config$result, c(
        "manifest", "data_dir", "tmp_dir", "retries", "timeout", "n_workers",
        "network_policy", "node_policy", "transfer_policy", "resource_policy"
    ))

    config_set <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "download", "config", "set",
        "--workers", "0", "--timeout", "120", "--bandwidth-limit", "4096",
        "--host-concurrency", "2", "--ssl-verifypeer", "false", "--disk-preflight", "false",
        "--min-free-space", "0", "--cooldown-seconds", "10"
    ))
    expect_equal(config_set$status, 0L)
    expect_equal(config_set$result$n_workers, 0L)
    expect_equal(config_set$result$timeout, 120L)
    expect_false(config_set$result$network_policy$ssl_verifypeer)
    expect_equal(config_set$result$transfer_policy$bandwidth_limit, 4096)
    expect_equal(config_set$result$resource_policy$host_concurrency, 2)
    expect_false(config_set$result$resource_policy$disk_preflight)
    expect_equal(config_set$result$node_policy$cooldown_seconds, 10L)

    config_reload <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "config", "show"))
    expect_equal(config_reload$status, 0L)
    expect_equal(config_reload$result$n_workers, 0L)
    expect_equal(config_reload$result$timeout, 120L)

    dl <- store$downloader(n_workers = 0L)
    catalog <- store$query("SELECT file_key FROM file_catalog")
    src <- tempfile()
    writeLines("cli download placeholder", src)
    session_id <- dl$enqueue(data.table::data.table(
        logical_file_id = "cli-download-task",
        file_key = catalog$file_key[[1L]],
        filename = "cli-download.nc",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L
    ))

    download_status <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "status", "--query", query_id, "--session", session_id))
    expect_equal(download_status$status, 0L)
    expect_equal(download_status$result$session_id, session_id)
    expect_equal(download_status$result$status, "queued")

    sessions <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "sessions"))
    expect_equal(sessions$status, 0L)
    expect_true(session_id %in% sessions$result$session_id)

    tasks <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "tasks", "--session", session_id, "--status", "queued"))
    expect_equal(tasks$status, 0L)
    expect_equal(tasks$result$task_id, download_status$result$task_id)

    events <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "events", "--session", session_id))
    expect_equal(events$status, 0L)
    expect_true("enqueue" %in% events$result$event)

    watch <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "watch", "--query", query_id, "--session", session_id, "--events", "1"))
    expect_equal(watch$status, 0L)
    expect_named(watch$result, c("summary", "tasks", "nodes", "events"))
    expect_equal(watch$result$summary$task_count, 1L)
    expect_equal(watch$result$summary$queued, 1L)
    expect_equal(watch$result$summary$last_download_session_id, session_id)
    expect_lte(nrow(watch$result$events), 1L)

    watch_text <- capture.output(
        watch_rendered <- epwshiftr_cli(c("--store", dir, "download", "watch", "--query", query_id, "--session", session_id, "--events", "1")),
        type = "message"
    )
    expect_equal(watch_rendered$status, 0L)
    expect_true(any(grepl("Download activity", watch_text)))
    expect_true(any(grepl("Tasks", watch_text)))
    expect_true(any(grepl("Recent events", watch_text)))
    expect_false(any(grepl("^\\$summary", watch_text)))

    logs <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "logs", "--session", session_id, "--tail", "1"))
    expect_equal(logs$status, 0L)
    expect_lte(nrow(logs$result), 1L)
    expect_true(all(logs$result$session_id == session_id))

    logs_text <- capture.output(
        logs_rendered <- epwshiftr_cli(c("--store", dir, "download", "logs", "--session", session_id, "--tail", "1")),
        type = "message"
    )
    expect_equal(logs_rendered$status, 0L)
    expect_true(any(grepl("Download events", logs_text)))
    expect_false(any(grepl("^\\[\\[|^\\$", logs_text)))

    launched <- list()
    withr::local_options(list(epwshiftr.downloader.launcher = function(kind, id, manifest, log_path) {
        launched[[length(launched) + 1L]] <<- list(kind = kind, id = id, manifest = manifest, log_path = log_path)
        TRUE
    }))
    background <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "download", "run", query_id,
        "--background", "--mode", "process", "--no-probe", "--no-progress"
    ))
    expect_equal(background$status, 0L)
    expect_equal(background$result$status, "queued")
    expect_match(background$result$job_id, "^job-")
    expect_length(launched, 1L)
    expect_equal(launched[[1L]]$kind, "job")

    jobs <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "jobs"))
    expect_equal(jobs$status, 0L)
    expect_true(background$result$job_id %in% jobs$result$job_id)

    job_tasks <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "tasks", "--job", background$result$job_id))
    expect_equal(job_tasks$status, 0L)
    expect_true(all(job_tasks$result$job_id == background$result$job_id))

    job_watch <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "watch", "--job", background$result$job_id, "--events", "1"))
    expect_equal(job_watch$status, 0L)
    expect_named(job_watch$result, c("summary", "tasks", "nodes", "events", "jobs"))
    expect_equal(job_watch$result$jobs$job_id, background$result$job_id)

    jsonl_text <- capture.output(
        jsonl_watch <- epwshiftr_cli(c(
            "--store", dir, "--jsonl", "download", "watch",
            "--job", background$result$job_id, "--follow", "--count", "1"
        ))
    )
    expect_equal(jsonl_watch$status, 0L)
    expect_gte(length(jsonl_text), 1L)
    jsonl <- jsonlite::fromJSON(jsonl_text[[1L]])
    expect_true(jsonl$type %in% c("job", "progress", "event", "summary"))

    job_logs <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "logs", "--job", background$result$job_id))
    expect_equal(job_logs$status, 0L)
    expect_true(all(c("job_id", "line", "message") %in% names(job_logs$result)))

    stopped <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "stop", "--job", background$result$job_id))
    expect_equal(stopped$status, 0L)
    expect_equal(stopped$result$status, "cancelled")

    nodes <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "nodes", "--service", "HTTPServer"))
    expect_equal(nodes$status, 0L)
    expect_s3_class(nodes$result, "data.table")

    reset_nodes <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "reset-nodes", "--service", "HTTPServer"))
    expect_equal(reset_nodes$status, 0L)
    expect_s3_class(reset_nodes$result, "data.table")

    retry <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "retry", "--query", query_id, "--session", session_id))
    expect_equal(retry$status, 0L)
    expect_equal(nrow(retry$result), 0L)

    cancelled <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "cancel", "--session", session_id))
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "cancelled")

    resumed <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "resume", "--session", session_id, "--no-progress"))
    expect_equal(resumed$status, 0L)
    expect_true(all(resumed$result$status %in% c("done", "skipped")))

    verified <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "verify", "--session", session_id))
    expect_equal(verified$status, 0L)
    expect_true(all(verified$result$checksum_ok))
})

# }}}
