# epwshiftr_cli_download() {{{
test_that("epwshiftr_cli_download() dispatches download workflows", {
    skip_on_cran()

    fixture <- cli_test_store(label = "cli esgf")
    dir <- fixture$dir
    store <- fixture$store
    query_id <- fixture$query_id
    on.exit(store$close(), add = TRUE)

    file_docs <- esgf_test__file_docs(
        "cli-file.nc",
        opendap_url = "https://example.org/dods/cli-file.nc.html",
        download_url = "https://example.org/fileServer/cli-file.nc"
    )
    cli_test_mock_query_collect(file_docs)

    updated <- cli_run(dir, "query", "update", query_id)
    expect_equal(updated$status, 0L)

    config <- cli_run(dir, "download", "config", "show")
    expect_equal(config$status, 0L)
    expect_named(
        config$result,
        c(
            "manifest",
            "data_dir",
            "tmp_dir",
            "retries",
            "timeout",
            "n_workers",
            "network_policy",
            "node_policy",
            "transfer_policy",
            "resource_policy"
        )
    )

    config_set <- cli_run(
        dir,
        "download",
        "config",
        "set",
        "--workers",
        "0",
        "--timeout",
        "120",
        "--bandwidth-limit",
        "4096",
        "--host-concurrency",
        "2",
        "--ssl-verifypeer",
        "false",
        "--disk-preflight",
        "false",
        "--min-free-space",
        "0",
        "--cooldown-seconds",
        "10"
    )
    expect_equal(config_set$status, 0L)
    expect_equal(config_set$result$n_workers, 0L)
    expect_equal(config_set$result$timeout, 120L)
    expect_false(config_set$result$network_policy$ssl_verifypeer)
    expect_equal(config_set$result$transfer_policy$bandwidth_limit, 4096)
    expect_equal(config_set$result$resource_policy$host_concurrency, 2)
    expect_false(config_set$result$resource_policy$disk_preflight)
    expect_equal(config_set$result$node_policy$cooldown_seconds, 10L)

    config_reload <- cli_run(dir, "download", "config", "show")
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

    download_status <- cli_run(
        dir,
        "download",
        "status",
        "--query",
        query_id,
        "--session",
        session_id
    )
    expect_equal(download_status$status, 0L)
    expect_equal(download_status$result$session_id, session_id)
    expect_equal(download_status$result$status, "queued")

    sessions <- cli_run(dir, "download", "sessions")
    expect_equal(sessions$status, 0L)
    expect_true(session_id %in% sessions$result$session_id)

    tasks <- cli_run(
        dir,
        "download",
        "tasks",
        "--session",
        session_id,
        "--status",
        "queued"
    )
    expect_equal(tasks$status, 0L)
    expect_equal(tasks$result$task_id, download_status$result$task_id)

    events <- cli_run(dir, "download", "events", "--session", session_id)
    expect_equal(events$status, 0L)
    expect_true("enqueue" %in% events$result$event)

    watch <- cli_run(
        dir,
        "download",
        "watch",
        "--query",
        query_id,
        "--session",
        session_id,
        "--events",
        "1"
    )
    expect_equal(watch$status, 0L)
    expect_named(watch$result, c("summary", "tasks", "nodes", "events"))
    expect_equal(watch$result$summary$task_count, 1L)
    expect_equal(watch$result$summary$queued, 1L)
    expect_equal(watch$result$summary$last_download_session_id, session_id)
    expect_lte(nrow(watch$result$events), 1L)

    watch_text <- capture.output(
        watch_rendered <- cli_run(
            dir,
            "download",
            "watch",
            "--query",
            query_id,
            "--session",
            session_id,
            "--events",
            "1",
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(watch_rendered$status, 0L)
    expect_true(any(grepl("Download activity", watch_text)))
    expect_true(any(grepl("Tasks", watch_text)))
    expect_true(any(grepl("Recent events", watch_text)))
    expect_false(any(grepl("^\\$summary", watch_text)))

    logs <- cli_run(
        dir,
        "download",
        "logs",
        "--session",
        session_id,
        "--tail",
        "1"
    )
    expect_equal(logs$status, 0L)
    expect_lte(nrow(logs$result), 1L)
    expect_true(all(logs$result$session_id == session_id))

    logs_text <- capture.output(
        logs_rendered <- cli_run(
            dir,
            "download",
            "logs",
            "--session",
            session_id,
            "--tail",
            "1",
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(logs_rendered$status, 0L)
    expect_true(any(grepl("Download events", logs_text)))
    expect_false(any(grepl("^\\[\\[|^\\$", logs_text)))

    launched <- list()
    test_local_dependencies(list(downloader__launch_process = function(
        kind,
        id,
        manifest,
        log_path
    ) {
        launched[[length(launched) + 1L]] <<- list(
            kind = kind,
            id = id,
            manifest = manifest,
            log_path = log_path
        )
        TRUE
    }))
    background <- cli_run(
        dir,
        "download",
        "run",
        query_id,
        "--background",
        "--mode",
        "process",
        "--no-probe",
        "--no-progress"
    )
    expect_equal(background$status, 0L)
    expect_equal(background$result$status, "queued")
    expect_match(background$result$job_id, "^job-")
    expect_true(all(c("run_id", "step_id") %in% names(background$result)))
    expect_length(launched, 1L)
    expect_equal(launched[[1L]]$kind, "job")

    jobs <- cli_run(dir, "download", "jobs")
    expect_equal(jobs$status, 0L)
    expect_true(background$result$job_id %in% jobs$result$job_id)

    job_tasks <- cli_run(
        dir,
        "download",
        "tasks",
        "--job",
        background$result$job_id
    )
    expect_equal(job_tasks$status, 0L)
    expect_true(all(job_tasks$result$job_id == background$result$job_id))

    job_watch <- cli_run(
        dir,
        "download",
        "watch",
        "--job",
        background$result$job_id,
        "--events",
        "1"
    )
    expect_equal(job_watch$status, 0L)
    expect_named(
        job_watch$result,
        c("summary", "tasks", "nodes", "events", "jobs")
    )
    expect_equal(job_watch$result$jobs$job_id, background$result$job_id)

    jsonl_text <- capture.output(
        jsonl_watch <- cli_run(
            dir,
            "--jsonl",
            "download",
            "watch",
            "--job",
            background$result$job_id,
            "--follow",
            "--count",
            "1",
            quiet = FALSE
        )
    )
    expect_equal(jsonl_watch$status, 0L)
    expect_gte(length(jsonl_text), 1L)
    jsonl <- jsonlite::fromJSON(jsonl_text[[1L]])
    expect_true(jsonl$type %in% c("job", "progress", "event", "summary"))

    job_logs <- cli_run(
        dir,
        "download",
        "logs",
        "--job",
        background$result$job_id
    )
    expect_equal(job_logs$status, 0L)
    expect_true(all(c("job_id", "line", "message") %in% names(job_logs$result)))

    shift_background <- shift_run_get(background$result$run_id, store = dir)
    expect_equal(shift_status(shift_background), "running")
    stopped <- cli_run(
        dir,
        "download",
        "stop",
        "--job",
        background$result$job_id
    )
    expect_equal(stopped$status, 0L)
    expect_equal(stopped$result$status, "cancelled")
    shift_cancelled <- shift_run_get(background$result$run_id, store = dir)
    expect_equal(shift_status(shift_cancelled), "cancelled")

    nodes <- cli_run(dir, "download", "nodes", "--service", "HTTPServer")
    expect_equal(nodes$status, 0L)
    expect_s3_class(nodes$result, "data.table")

    reset_nodes <- cli_run(
        dir,
        "download",
        "reset-nodes",
        "--service",
        "HTTPServer"
    )
    expect_equal(reset_nodes$status, 0L)
    expect_s3_class(reset_nodes$result, "data.table")

    retry <- cli_run(
        dir,
        "download",
        "retry",
        "--query",
        query_id,
        "--session",
        session_id
    )
    expect_equal(retry$status, 0L)
    expect_equal(nrow(retry$result), 0L)

    cancelled <- cli_run(dir, "download", "cancel", "--session", session_id)
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "cancelled")

    resumed <- cli_run(
        dir,
        "download",
        "resume",
        "--session",
        session_id,
        "--no-progress"
    )
    expect_equal(resumed$status, 0L)
    expect_true(all(resumed$result$status %in% c("done", "skipped")))

    verified <- cli_run(dir, "download", "verify", "--session", session_id)
    expect_equal(verified$status, 0L)
    expect_true(all(verified$result$checksum_ok))
})
# }}}

# epwshiftr_cli_storage() / epwshiftr_cli_esgf() {{{
test_that("epwshiftr_cli_storage() / epwshiftr_cli_esgf() dispatch storage and ESGF commands", {
    fixture <- cli_test_store(label = "cli esgf")
    dir <- fixture$dir
    store <- fixture$store
    query_id <- fixture$query_id
    on.exit(store$close(), add = TRUE)

    file_docs <- esgf_test__file_docs(
        "cli-file.nc",
        opendap_url = "https://example.org/dods/cli-file.nc.html",
        download_url = "https://example.org/fileServer/cli-file.nc"
    )
    cli_test_mock_query_collect(file_docs)

    updated <- cli_run(dir, "query", "update", query_id)
    expect_equal(updated$status, 0L)

    esgf_help <- cli_run(dir, "esgf", "help")
    expect_equal(esgf_help$status, 0L)
    expect_match(esgf_help$result[[1L]], "Usage: epwshiftr esgf")

    status <- cli_run(dir, "esgf", "report", "--query", query_id)
    expect_equal(status$status, 0L)
    expect_named(
        status$result,
        c("summary", "updates", "changes", "downloads", "nodes")
    )
    expect_equal(status$result$summary$query_id, query_id)

    esgf_text <- capture.output(
        esgf_rendered <- cli_run(
            dir,
            "esgf",
            "report",
            "--query",
            query_id,
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(esgf_rendered$status, 0L)
    expect_true(any(grepl("ESGF report", esgf_text)))
    expect_true(any(grepl("Summary", esgf_text)))
    expect_true(any(grepl("Downloads", esgf_text)))
    expect_false(any(grepl("^\\$summary", esgf_text)))

    old_workflow <- cli_run(dir, "workflow", "report", "--query", query_id)
    expect_equal(old_workflow$status, 2L)

    storage <- cli_run(dir, "storage", "report", "--detail")
    expect_equal(storage$status, 0L)
    expect_named(
        storage$result,
        c(
            "summary",
            "downloads",
            "registered",
            "untracked_files",
            "missing_records",
            "tmp",
            "orphan_records"
        )
    )

    layout <- cli_run(dir, "storage", "layout", "show")
    expect_equal(layout$status, 0L)
    expect_equal(layout$result$layout, "flat")

    layout_set <- cli_run(
        dir,
        "storage",
        "layout",
        "set",
        "--layout",
        "drs",
        "--include-version",
        "false",
        "--collision",
        "suffix"
    )
    expect_equal(layout_set$status, 0L)
    expect_equal(layout_set$result$layout, "drs")
    expect_false(layout_set$result$include_version)
    expect_equal(layout_set$result$collision, "suffix")

    validated <- cli_run(
        dir,
        "storage",
        "validate",
        "--query",
        query_id,
        "--checksum"
    )
    expect_equal(validated$status, 0L)
    expect_named(
        validated$result,
        c("summary", "files", "artifacts", "untracked", "actions")
    )

    repaired <- cli_run(dir, "storage", "repair")
    expect_equal(repaired$status, 0L)
    expect_true(all(repaired$result$dry_run))

    cleanup <- cli_run(
        dir,
        "storage",
        "cleanup",
        "--scope",
        "tmp,missing_records",
        "--older-than",
        "0"
    )
    expect_equal(cleanup$status, 0L)
    expect_s3_class(cleanup$result, "data.table")
})
# }}}

test_that("extract CLI plans, runs, checks coverage, and lists artifacts", {
    skip_on_cran()

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)
    setup <- cli_shift_test_store_with_query(nc)

    plan <- cli_run(
        setup$dir,
        "extract",
        "plan",
        "--query",
        setup$query_id,
        "--site-id",
        "SIN",
        "--lon",
        "103.98",
        "--lat",
        "1.37",
        "--time",
        "2060-01-02T00:00:00Z,2060-01-03T23:59:59Z",
        "--variable",
        "tas",
        "--method",
        "nearest",
        "--filter",
        "source_id=EC-Earth3"
    )
    expect_equal(plan$status, 0L)
    expect_true(length(plan$result$plan_id) >= 1L)
    expect_equal(unique(plan$result$site_id), "SIN")

    plan_ids <- paste(plan$result$plan_id, collapse = ",")
    run <- cli_run(
        setup$dir,
        "extract",
        "run",
        "--plan",
        plan_ids,
        "--fallback",
        "auto"
    )
    expect_equal(run$status, 0L)
    expect_true(all(run$result$status == "done"))
    expect_true(all(c("run_id", "step_id") %in% names(run$result)))

    coverage <- cli_run(setup$dir, "extract", "coverage", "--plan", plan_ids)
    expect_equal(coverage$status, 0L)
    expect_true(all(coverage$result$complete))

    artifacts <- cli_run(setup$dir, "extract", "artifacts", "--plan", plan_ids)
    expect_equal(artifacts$status, 0L)
    expect_true(nrow(artifacts$result) >= 1L)
    expect_true(all(artifacts$result$role %in% "derived"))

    store <- EsgStore$new(setup$dir)
    suppressWarnings(store$query(sprintf(
        "UPDATE extraction_plan SET status = 'failed', last_error = 'forced failure' WHERE plan_id = %s",
        shift_stage__sql_string(plan$result$plan_id[[1L]])
    )))
    store$close()

    first_plan <- plan$result$plan_id[[1L]]
    retry_preview <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan
    )
    expect_equal(retry_preview$status, 0L)
    expect_equal(retry_preview$result$status, "failed")
    expect_true(retry_preview$result$dry_run)

    retry_other_status <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan,
        "--status",
        "done"
    )
    expect_equal(retry_other_status$status, 0L)
    expect_equal(nrow(retry_other_status$result), 0L)

    retry_bad_status <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--status",
        "bogus"
    )
    expect_equal(retry_bad_status$status, 2L)
    expect_match(retry_bad_status$error, "--status")

    retry_run <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan,
        "--run",
        "--overwrite"
    )
    expect_equal(retry_run$status, 0L)
    expect_equal(retry_run$result$status, "done")

    rendered <- capture.output(
        rendered_cov <- cli_run(
            setup$dir,
            "extract",
            "coverage",
            "--plan",
            plan_ids,
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(rendered_cov$status, 0L)
    expect_true(any(grepl("Extraction coverage", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    missing <- cli_run(
        setup$dir,
        "extract",
        "plan",
        "--query",
        setup$query_id
    )
    expect_equal(missing$status, 2L)
    expect_match(missing$error, "--site-id")
})

# vim: fdm=marker :
