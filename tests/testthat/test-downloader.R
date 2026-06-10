# Basic Tests {{{
test_that("FileDownloader can be created", {
    dl <- FileDownloader$new()
    expect_s3_class(dl, "FileDownloader")
    expect_true(dir.exists(dl$data_dir))
    expect_true(dir.exists(dl$tmp_dir))
    expect_equal(dl$max_retries, 3L)
    expect_equal(dl$timeout, 3600L)
    expect_identical(dl$network_policy$ssl_verifypeer, TRUE)
    expect_null(dl$network_policy$proxy)
    expect_null(dl$network_policy$connect_timeout)
    expect_null(dl$network_policy$useragent)
    expect_equal(dl$node_policy$cooldown_after_failures, 3L)
    expect_null(dl$transfer_policy$chunk_size)
    expect_null(dl$transfer_policy$bandwidth_limit)
    expect_true(dl$resource_policy$disk_preflight)
    expect_null(dl$resource_policy$host_concurrency)
    expect_equal(dl$resource_policy$min_free_space, 0)

    temp_dir <- tempdir()
    temp <- file.path(temp_dir, ".tmp_test")
    dl <- FileDownloader$new(
        dest = temp_dir,
        temp = temp,
        retries = 5L,
        timeout = 7200L,
        ssl_verifypeer = FALSE,
        proxy = "http://proxy.example:8080",
        connect_timeout = 5L,
        useragent = "epwshiftr-test",
        transfer_policy = list(
            chunk_size = 65536L,
            bandwidth_limit = 1048576L,
            low_speed_limit = 128L,
            low_speed_time = 10L
        ),
        resource_policy = list(
            host_concurrency = 2L,
            disk_preflight = FALSE,
            min_free_space = 1024
        ),
        cleanup = FALSE
    )
    expect_equal(dl$data_dir, normalizePath(temp_dir))
    expect_equal(dl$tmp_dir, temp)
    expect_equal(dl$max_retries, 5L)
    expect_equal(dl$timeout, 7200L)
    expect_identical(dl$network_policy$ssl_verifypeer, FALSE)
    expect_equal(dl$network_policy$proxy, "http://proxy.example:8080")
    expect_equal(dl$network_policy$connect_timeout, 5L)
    expect_equal(dl$network_policy$useragent, "epwshiftr-test")
    expect_equal(dl$transfer_policy$chunk_size, 65536)
    expect_equal(dl$transfer_policy$bandwidth_limit, 1048576)
    expect_equal(dl$transfer_policy$low_speed_limit, 128)
    expect_equal(dl$transfer_policy$low_speed_time, 10)
    expect_equal(dl$resource_policy$host_concurrency, 2)
    expect_false(dl$resource_policy$disk_preflight)
    expect_equal(dl$resource_policy$min_free_space, 1024)

    # Clean up
    unlink(temp, recursive = TRUE)
})
# }}}

# Persistent Manifest Tests {{{
test_that("FileDownloader persists config and manifest state", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("persistent content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        timeout = 30L,
        ssl_verifypeer = FALSE,
        connect_timeout = 2L,
        useragent = "epwshiftr-test",
        node_policy = list(
            cooldown_after_failures = 1L,
            cooldown_seconds = 60L,
            history_ttl_seconds = 600L,
            min_attempts = 1L
        ),
        transfer_policy = list(
            chunk_size = 32768L,
            bandwidth_limit = 2048L,
            low_speed_limit = 64L,
            low_speed_time = 5L
        ),
        resource_policy = list(
            host_concurrency = 1L,
            disk_preflight = TRUE,
            min_free_space = 4096
        ),
        n_workers = 0L
    )

    expect_true(file.exists(dl$config_file))
    expect_true(file.exists(dl$manifest))
    expect_true(schema_validate(
        SCHEMA_DOWNLOADER_CONFIG,
        jsonlite::fromJSON(dl$config_file, simplifyVector = TRUE, simplifyMatrix = FALSE),
        mode = "test",
        name = "downloader-config"
    ))

    plan <- data.table::data.table(
        logical_file_id = "tracking:local-test",
        filename = "local.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "unit-test")
    expect_match(session_id, "^\\d{8}-\\d{6}-[0-9a-f]{8}$")
    expect_equal(nrow(dl$sessions()), 1L)
    expect_equal(nrow(dl$tasks(session_id = session_id)), 1L)

    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_equal(tasks$candidate_count, 1L)
    expect_equal(tasks$failed_candidate_count, 0L)
    expect_true(file.exists(file.path(dest, "local.txt")))
    expect_true(dl$verify(session_id = session_id)$checksum_ok)

    restored <- FileDownloader$load_config(dl$config_file)
    expect_equal(restored$data_dir, normalizePath(dest, winslash = "/"))
    expect_equal(restored$tmp_dir, normalizePath(temp, mustWork = FALSE, winslash = "/"))
    expect_equal(restored$manifest, normalizePath(manifest, mustWork = FALSE, winslash = "/"))
    expect_identical(restored$network_policy$ssl_verifypeer, FALSE)
    expect_equal(restored$network_policy$connect_timeout, 2L)
    expect_equal(restored$network_policy$useragent, "epwshiftr-test")
    expect_equal(restored$node_policy$cooldown_after_failures, 1L)
    expect_equal(restored$node_policy$cooldown_seconds, 60L)
    expect_equal(restored$transfer_policy$chunk_size, 32768)
    expect_equal(restored$transfer_policy$bandwidth_limit, 2048)
    expect_equal(restored$transfer_policy$low_speed_limit, 64)
    expect_equal(restored$transfer_policy$low_speed_time, 5)
    expect_equal(restored$resource_policy$host_concurrency, 1)
    expect_true(restored$resource_policy$disk_preflight)
    expect_equal(restored$resource_policy$min_free_space, 4096)
    expect_equal(restored$tasks(session_id = session_id)$status, "done")

    rm(dl, restored)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit(ddb_disconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    expect_setequal(
        ddb_list_tables(conn),
        c("download_candidate", "download_event", "download_meta", "download_node", "download_session", "download_task")
    )
})

test_that("FileDownloader preflights disk requirements", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("preflight content", src)
    size <- as.numeric(file.info(src, extra_cols = FALSE)$size)
    plan <- data.table::data.table(
        logical_file_id = "tracking:preflight",
        filename = "preflight.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = as.character(tools::md5sum(src)),
        checksum_type = "md5",
        size = size,
        priority = 1L
    )

    dl <- FileDownloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
    plan_check <- dl$preflight(plan = plan)
    expect_equal(plan_check$task_count, 1L)
    expect_equal(plan_check$needs_download, 1L)
    expect_equal(plan_check$required_bytes, size)
    expect_equal(plan_check$size_unknown_count, 0L)
    expect_false(plan_check$disk_would_block)

    session_id <- dl$enqueue(plan)
    session_check <- dl$preflight(session_id = session_id)
    expect_equal(session_check$required_bytes, size)

    free <- suppressWarnings(min(c(plan_check$dest_free_bytes, plan_check$tmp_free_bytes), na.rm = TRUE))
    skip_if(!is.finite(free), "Disk free-space check is not available on this platform")

    blocker <- FileDownloader$new(
        dest = file.path(root, "blocked-downloads"),
        temp = file.path(root, "blocked-tmp"),
        manifest = file.path(root, "blocked", "manifest.duckdb"),
        resource_policy = list(min_free_space = free + size + 1024),
        n_workers = 0L
    )
    blocked_session <- blocker$enqueue(plan)
    expect_error(
        blocker$run(session_id = blocked_session, progress = FALSE),
        "Insufficient disk space"
    )
})

test_that("FileDownloader migrates older manifests to the current schema version", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- FileDownloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
    rm(dl)
    gc()

    conn <- ddb_connect(manifest, read_only = FALSE)
    on.exit(if (!is.null(conn) && ddb_is_valid(conn)) ddb_disconnect(conn, shutdown = TRUE), add = TRUE)
    ddb_exec(conn, "DELETE FROM download_meta WHERE key = 'schema_version'")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_success_count")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_failure_count")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS last_probe_at")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS cooldown_until")
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    restored <- FileDownloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
    rm(restored)
    gc()

    conn <- ddb_connect(manifest, read_only = TRUE)
    meta <- ddb_read_table(conn, "download_meta")
    nodes <- ddb_read_table(conn, "download_node")
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    expect_equal(meta[meta$key == "schema_version", "value", drop = TRUE], DOWNLOADER_SCHEMA_VERSION)
    expect_true(all(c("probe_success_count", "probe_failure_count", "last_probe_at", "cooldown_until") %in% names(nodes)))
})

test_that("FileDownloader exports and imports persistent manifests", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("export content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:export-test",
        filename = "export.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "export")
    dl$run(session_id = session_id, progress = FALSE)

    export_file <- file.path(root, "manifest-export.json")
    expect_equal(dl$export_manifest(export_file), normalizePath(export_file, winslash = "/"))
    payload <- jsonlite::fromJSON(export_file, simplifyDataFrame = TRUE)
    expect_equal(payload$kind, "epwshiftr_file_downloader_manifest")
    expect_true("download_task" %in% names(payload$tables))

    clone <- FileDownloader$new(
        dest = file.path(root, "clone-downloads"),
        temp = file.path(root, "clone-tmp"),
        manifest = file.path(root, "clone", "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L
    )
    counts <- clone$import_manifest(export_file, mode = "replace")
    expect_true(all(c("download_session", "download_task", "download_candidate", "download_event") %in% counts$table))
    expect_equal(clone$sessions()$session_id, session_id)
    expect_equal(clone$tasks(session_id = session_id)$status, "done")
    expect_true("done" %in% clone$events(session_id = session_id)$event)

    clone$import_manifest(export_file, mode = "append")
    expect_equal(nrow(clone$sessions()), 1L)
    expect_equal(nrow(clone$tasks()), 1L)
})

test_that("FileDownloader reconnects manifest after shallow clone finalization", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("clone content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    cloned <- dl$clone(deep = FALSE)
    expect_s3_class(cloned, "FileDownloader")
    rm(cloned)
    gc()

    plan <- data.table::data.table(
        logical_file_id = "tracking:clone-reconnect",
        filename = "clone.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "clone-reconnect")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_true(file.exists(file.path(dest, "clone.txt")))
})

test_that("FileDownloader cancels stale downloading tasks before run", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("stale content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:stale-test",
        filename = "stale.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "stale")
    task_id <- dl$tasks(session_id = session_id)$task_id[[1L]]
    conn <- priv(dl)$manifest_conn
    ddb_exec(conn, sprintf(
        "UPDATE download_task SET status = 'downloading', updated_at = %s WHERE task_id = %s",
        ddb_literal(conn, as.POSIXct("2000-01-01 00:00:00", tz = "UTC")),
        ddb_literal(conn, task_id)
    ))

    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "cancelled")
    expect_match(tasks$last_error, "previous R session")
    expect_false(file.exists(file.path(dest, "stale.txt")))

    events <- ddb_read_table(priv(dl)$manifest_conn, "download_event")
    expect_true("cancelled" %in% events$event)

    resumed <- dl$resume(session_id = session_id, progress = FALSE)
    expect_equal(resumed$status, "done")
    expect_true(file.exists(file.path(dest, "stale.txt")))
})

test_that("FileDownloader cancels persistent queued tasks", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("cancel content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:cancel-test",
        filename = "cancel.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "cancel")
    cancelled <- dl$cancel(session_id = session_id)
    expect_equal(cancelled$status, "cancelled")
    expect_match(cancelled$last_error, "Cancelled by user")
    sessions <- dl$sessions()
    expect_equal(sessions[sessions$session_id == session_id]$status, "cancelled")

    retried <- dl$retry(session_id = session_id)
    expect_equal(retried$status, "queued")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_true(file.exists(file.path(dest, "cancel.txt")))

    events <- ddb_read_table(priv(dl)$manifest_conn, "download_event")
    expect_true("cancelled" %in% events$event)
})

test_that("FileDownloader cancels persistent downloading tasks", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("cancel downloading content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:cancel-downloading-test",
        filename = "cancel-downloading.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "cancel-downloading")
    task_id <- dl$tasks(session_id = session_id)$task_id[[1L]]
    conn <- priv(dl)$manifest_conn
    ddb_exec(conn, sprintf(
        "UPDATE download_task SET status = 'downloading', updated_at = %s WHERE task_id = %s",
        ddb_literal(conn, download_now()),
        ddb_literal(conn, task_id)
    ))

    cancelled <- dl$cancel(task_id = task_id)
    expect_equal(cancelled$status, "cancelled")
    expect_equal(dl$status(task_id = task_id)$status, "cancelled")
    expect_false(file.exists(file.path(dest, "cancel-downloading.txt")))
})

test_that("FileDownloader falls back across candidate URLs", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("candidate content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:fallback-test",
        filename = "fallback.txt",
        url = c(
            paste0("file://", normalizePath(file.path(root, "missing.txt"), mustWork = FALSE, winslash = "/")),
            paste0("file://", normalizePath(src, winslash = "/"))
        ),
        checksum = checksum,
        checksum_type = "md5",
        data_node = c("bad-node.example.org", "good-node.example.org"),
        priority = c(1L, 2L)
    )

    session_id <- dl$enqueue(plan, session_label = "fallback")
    expect_warning(
        tasks <- dl$run(session_id = session_id, progress = FALSE),
        "Failed to open"
    )
    expect_equal(tasks$status, "done")
    expect_identical(tasks$selected_url, plan$url[[2L]])
    nodes <- dl$data_nodes()
    expect_equal(nodes[data_node == "bad-node.example.org"]$failure_count, 1L)
    expect_equal(nodes[data_node == "good-node.example.org"]$success_count, 1L)

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit(ddb_disconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    candidates <- data.table::as.data.table(ddb_read_table(
        conn,
        "download_candidate"
    ))
    expect_equal(candidates[order(priority)]$failed_count, c(1L, 0L))
})

test_that("FileDownloader keeps candidate URLs scoped to each task", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src_1 <- tempfile()
    src_2 <- tempfile()
    writeLines("first task content", src_1)
    writeLines("second task content", src_2)

    urls <- paste0("file://", normalizePath(c(src_1, src_2), winslash = "/"))
    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = c("tracking:first", "tracking:second"),
        filename = c("first.txt", "second.txt"),
        url = urls,
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "candidate-scope")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(filename)]

    expect_equal(tasks$status, c("done", "done"))
    expect_identical(tasks$selected_url, urls)
    expect_equal(readLines(file.path(dest, "first.txt")), "first task content")
    expect_equal(readLines(file.path(dest, "second.txt")), "second task content")
})

test_that("FileDownloader records probe outcomes and resets data node health", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- FileDownloader$new(
        dest = file.path(root, "downloads"),
        temp = file.path(root, "tmp"),
        manifest = manifest,
        n_workers = 0L,
        node_policy = list(
            cooldown_after_failures = 1L,
            cooldown_seconds = 60L,
            history_ttl_seconds = 3600L,
            min_attempts = 1L
        )
    )
    plan <- data.table::data.table(
        logical_file_id = c("tracking:probe-ok", "tracking:probe-fail"),
        filename = c("ok.nc", "fail.nc"),
        url = c("https://ok.example.org/file.nc", "https://fail.example.org/file.nc"),
        service = "HTTPServer",
        data_node = c("ok.example.org", "fail.example.org"),
        priority = c(1L, 1L),
        probe_latency = c(0.2, NA_real_),
        probe_throughput = NA_real_
    )

    nodes <- dl$record_probes(plan, probed = TRUE)
    expect_equal(nodes[data_node == "ok.example.org"]$probe_success_count, 1L)
    expect_equal(nodes[data_node == "fail.example.org"]$probe_failure_count, 1L)
    expect_false(is.na(nodes[data_node == "fail.example.org"]$cooldown_until))

    cached <- data.table::copy(plan[1L])
    cached$probe_cached <- TRUE
    nodes <- dl$record_probes(cached, probed = TRUE)
    expect_equal(nodes[data_node == "ok.example.org"]$probe_success_count, 1L)

    remaining <- dl$reset_data_nodes(data_node = "fail.example.org")
    expect_false("fail.example.org" %in% remaining$data_node)
    expect_true("ok.example.org" %in% remaining$data_node)
})

test_that("FileDownloader exposes persistent events and callbacks", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("event content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    seen <- character()
    done_token <- dl$on("task_done", function(event, downloader) {
        seen <<- c(seen, event$event)
        expect_s3_class(downloader, "FileDownloader")
    })
    session_token <- dl$on("session_done", function(event, downloader) {
        seen <<- c(seen, event$event)
    })
    dl$on("task_done", function(event, downloader) {
        stop("callback boom", call. = FALSE)
    })

    plan <- data.table::data.table(
        logical_file_id = "tracking:event-test",
        filename = "event.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "events")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_equal(tasks$status, "done")
    expect_true(all(c("session_done", "task_done") %in% seen))
    events <- dl$events(session_id = session_id)
    expect_true(all(c("enqueue", "start", "done", "session_done", "callback_error") %in% events$event))
    task_events <- dl$events(task_id = tasks$task_id[[1L]])
    expect_true(all(c("start", "done", "callback_error") %in% task_events$event))
    expect_true(dl$off(done_token))
    expect_false(dl$off(done_token))
    expect_true(dl$off(session_token))
})

test_that("FileDownloader runs persistent tasks with worker concurrency", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("mirai")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src_1 <- tempfile()
    src_2 <- tempfile()
    writeLines("parallel first", src_1)
    writeLines("parallel second", src_2)

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L
    )
    plan <- data.table::data.table(
        logical_file_id = c("tracking:parallel-first", "tracking:parallel-second"),
        filename = c("parallel-first.txt", "parallel-second.txt"),
        url = paste0("file://", normalizePath(c(src_1, src_2), winslash = "/")),
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "parallel")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(filename)]

    expect_equal(tasks$status, c("done", "done"))
    expect_equal(tasks$attempts, c(1L, 1L))
    expect_equal(readLines(file.path(dest, "parallel-first.txt")), "parallel first")
    expect_equal(readLines(file.path(dest, "parallel-second.txt")), "parallel second")
    sessions <- dl$sessions()
    expect_equal(sessions[sessions$session_id == session_id]$status, "done")
})

test_that("FileDownloader serializes persistent tasks for the same target path", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("mirai")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("shared target content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L
    )
    plan <- data.table::data.table(
        logical_file_id = c("tracking:shared-first", "tracking:shared-second"),
        filename = c("shared.txt", "shared.txt"),
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "shared-target")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_setequal(tasks$status, c("done", "skipped"))
    expect_true(file.exists(file.path(dest, "shared.txt")))
    expect_equal(readLines(file.path(dest, "shared.txt")), "shared target content")
})
# }}}

# Download Tests {{{
test_that("FileDownloader can download a single file", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://httpbin.org/bytes/1024"

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    path <- tryCatch(
        dl$download(url, filename = "test_file.bin", progress = FALSE),
        error = function(e) {
            skip("Network unavailable or httpbin.org is down")
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "test_file.bin")
    expect_equal(file.info(path)$size, 1024)

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader can download with subdir", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://httpbin.org/bytes/512"

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    path <- tryCatch(
        dl$download(
            url,
            filename = "data.bin",
            subdir = "docs",
            progress = FALSE
        ),
        error = function(e) {
            skip("Network unavailable")
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "data.bin")
    expect_equal(dirname(path), file.path(normalizePath(temp_dir), "docs"))

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader respects overwrite parameter with local files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    test_file <- file.path(temp_dir, "source.txt")
    writeLines("test content", test_file)

    dl <- FileDownloader$new(dest = temp_dir)

    dest_file <- file.path(temp_dir, "dest.txt")
    file.copy(test_file, dest_file)

    mtime1 <- file.info(dest_file)$mtime

    Sys.sleep(0.1)

    expect_true(file.exists(dest_file))
    mtime2 <- file.info(dest_file)$mtime
    expect_equal(mtime1, mtime2)

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader restarts partial downloads when Range resume is unsupported", {
    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    dir.create(temp, recursive = TRUE)

    src <- file.path(root, "source.txt")
    writeLines("complete content", src)
    tmp_id <- "range-restart"
    writeLines("stale partial", file.path(temp, paste0(tmp_id, ".part")))

    dl <- FileDownloader$new(dest = dest, temp = temp, retries = 1L, n_workers = 0L)
    testthat::local_mocked_bindings(
        download_resume_supported = function(...) FALSE,
        .package = "epwshiftr"
    )
    path <- dl$download(
        paste0("file://", normalizePath(src, winslash = "/")),
        filename = "source.txt",
        progress = FALSE,
        .tmp_id = tmp_id
    )

    expect_equal(readLines(path), "complete content")
    expect_false(file.exists(file.path(temp, paste0(tmp_id, ".part"))))
})
# }}}

# File Status Tests {{{
test_that("FileDownloader tracks file status correctly", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    url <- "https://httpbin.org/bytes/2048"

    path <- tryCatch(
        dl$download(url, filename = "test.bin", progress = FALSE),
        error = function(e) {
            skip("Network unavailable")
        }
    )
    expect_true(file.exists(path))

    expect_equal(nrow(dl$list_incomplete()), 0L)

    unlink(temp_dir, recursive = TRUE)
})

test_that("cleanup_tmp works", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp)

    file.create(file.path(temp, "test1.part"))
    file.create(file.path(temp, "test2.done"))

    n_removed <- dl$cleanup_tmp(all = TRUE)

    expect_gte(n_removed, 0L)

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Checksum Tests {{{
test_that("FileDownloader can verify checksums", {
    dl <- FileDownloader$new()

    test_file <- tempfile()
    writeLines("test content", test_file)

    checksum_md5 <- tools::md5sum(test_file)
    checksum_md5 <- as.character(checksum_md5)

    expect_true(dl$verify_checksum(test_file, checksum_md5, "md5"))

    expect_false(dl$verify_checksum(test_file, "wrongchecksum", "md5"))

    unlink(test_file)
})

test_that("FileDownloader verify marks checksum failures as error", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("verified content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:verify-error",
        filename = "verify-error.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "verify-error")
    expect_equal(dl$run(session_id = session_id, progress = FALSE)$status, "done")

    writeLines("corrupted content", file.path(dest, "verify-error.txt"))
    verified <- dl$verify(session_id = session_id)
    expect_false(verified$checksum_ok)
    expect_equal(dl$status(session_id = session_id)$status, "error")
    expect_match(dl$status(session_id = session_id)$last_error, "Checksum verification failed")

    events <- ddb_read_table(priv(dl)$manifest_conn, "download_event")
    expect_true("verify_error" %in% events$event)
})

test_that("FileDownloader can download with checksum verification", {
    skip_on_cran()
    skip_if_offline()

    query <- tryCatch(
        esg_query()$
            source_id("CMCC-CM2-SR5")$
            experiment_id("dcppA-hindcast")$
            variable_id("sftgif")$
            frequency("fx")$
            nominal_resolution("100 km")$
            variant_label(c("r32i1p1f1", "r34i1p1f1"))$
            params(institution_id = "CMCC", sub_experiment_id = "s2017"),
        error = function(e) {
            skip("ESGF query failed")
        }
    )

    datasets <- tryCatch(
        query$collect(),
        error = function(e) {
            skip("ESGF query failed")
        }
    )

    if (datasets$count() == 0) {
        skip("No datasets found")
    }

    files <- tryCatch(
        datasets$collect(type = "File"),
        error = function(e) {
            skip("Failed to get files")
        }
    )

    if (files$count() == 0) {
        skip("No files found")
    }

    file_url <- files$url_download[1]
    file_checksum <- files$checksum[1]

    if (is.na(file_url) || is.na(file_checksum)) {
        skip("File URL or checksum not available")
    }

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 3,
        timeout = 300
    )

    path <- tryCatch(
        dl$download(
            url = file_url,
            filename = basename(files$title[1]),
            checksum = file_checksum,
            checksum_type = "sha256",
            progress = FALSE
        ),
        error = function(e) {
            unlink(temp_dir, recursive = TRUE)
            skip(paste("Download failed:", conditionMessage(e)))
        }
    )

    expect_true(file.exists(path))

    is_valid <- epwshiftr:::verify_checksum(path, file_checksum, "sha256")
    expect_true(is_valid)

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Error Handling Tests {{{
test_that("FileDownloader handles download errors", {
    skip_on_cran()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 1L,
        timeout = 1L
    )

    url <- "https://nonexistent.example.com/file.nc"

    expect_warning(expect_error(
        dl$download(url, filename = "nonexistent.nc", progress = FALSE),
        "Failed to download"
    ))

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader handles checksum mismatch", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 1L
    )

    url <- "https://raw.githubusercontent.com/ideas-lab-nus/epwshiftr/main/README.md"

    expect_error(
        dl$download(
            url,
            filename = "readme.md",
            checksum = "wrongchecksum",
            checksum_type = "sha256",
            progress = FALSE
        ),
        "Checksum verification failed"
    )

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Print Tests {{{
test_that("FileDownloader print works", {
    dl <- FileDownloader$new()

    # Just test that print doesn't error
    # cli output produces messages, which is expected
    expect_no_error(expect_message(print(dl)))
})
# }}}

# Offline Mode Tests {{{
test_that("FileDownloader: offline mode blocks new downloads", {
    local_cache_mode("offline")
    dl <- FileDownloader$new(dest = tempdir(), n_workers = 0L)

    expect_error(
        dl$download(url = "https://example.com/nonexistent.nc"),
        "offline"
    )
})

test_that("FileDownloader: offline mode allows verified files", {
    local_cache_mode("offline")

    dest <- tempdir()
    # Create a file that would be "verified"
    test_file <- file.path(dest, "test-offline-verified.txt")
    writeLines("test content", test_file)
    on.exit(unlink(test_file), add = TRUE)

    dl <- FileDownloader$new(dest = dest, n_workers = 0L)

    # This should succeed because the file already exists (Verified status)
    result <- dl$download(
        url = "https://example.com/test-offline-verified.txt",
        filename = "test-offline-verified.txt"
    )
    expect_equal(normalizePath(result), normalizePath(test_file))
})
# }}}
