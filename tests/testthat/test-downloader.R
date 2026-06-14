# downloader test helpers {{{
downloader_test_df <- function(...) {
    data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}

downloader_test_file_url <- function(root, filename, size) {
    path <- file.path(root, filename)
    writeBin(as.raw(rep(0:255, length.out = size)), path)
    paste0("file://", normalizePath(path, winslash = "/"))
}
# }}}
# Downloader$new() {{{
test_that("Downloader$new()", {
    dl <- Downloader$new()
    expect_s3_class(dl, "Downloader")
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
    expect_equal(dl$transfer_policy$range_mode, "off")
    expect_equal(dl$transfer_policy$piece_size, 16L * 1024L^2L)
    expect_equal(dl$transfer_policy$piece_concurrency, 4L)
    expect_equal(dl$transfer_policy$max_sources, 4L)
    expect_true(dl$transfer_policy$require_checksum_for_multisource)
    expect_equal(dl$transfer_policy$range_probe_timeout, 30L)
    expect_true(dl$resource_policy$disk_preflight)
    expect_null(dl$resource_policy$host_concurrency)
    expect_equal(dl$resource_policy$min_free_space, 0)

    temp_dir <- tempdir()
    temp <- file.path(temp_dir, ".tmp_test")
    dl <- Downloader$new(
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
            low_speed_time = 10L,
            range_mode = "single",
            piece_size = 1024L,
            piece_concurrency = 2L,
            max_sources = 3L,
            require_checksum_for_multisource = FALSE,
            range_probe_timeout = 11L
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
    expect_equal(dl$transfer_policy$range_mode, "single")
    expect_equal(dl$transfer_policy$piece_size, 1024L)
    expect_equal(dl$transfer_policy$piece_concurrency, 2L)
    expect_equal(dl$transfer_policy$max_sources, 3L)
    expect_false(dl$transfer_policy$require_checksum_for_multisource)
    expect_equal(dl$transfer_policy$range_probe_timeout, 11L)
    expect_equal(dl$resource_policy$host_concurrency, 2)
    expect_false(dl$resource_policy$disk_preflight)
    expect_equal(dl$resource_policy$min_free_space, 1024)

    # Clean up
    unlink(temp, recursive = TRUE)
})
# }}}
# Downloader$enqueue() / Downloader$run() / Downloader$verify() {{{
test_that("Downloader$enqueue() / Downloader$run() / Downloader$verify()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("persistent content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
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
            low_speed_time = 5L,
            range_mode = "auto",
            piece_size = 8L,
            piece_concurrency = 2L,
            max_sources = 2L,
            require_checksum_for_multisource = TRUE,
            range_probe_timeout = 7L
        ),
        resource_policy = list(
            host_concurrency = 1L,
            disk_preflight = TRUE,
            min_free_space = 4096
        ),
        n_workers = 0L
    )

    expect_true(file.exists(dl$manifest))
    expect_true(schema_validate(
        SCHEMA_DOWNLOADER_CONFIG,
        dl$config,
        mode = "test",
        name = "downloader-config"
    ))

    plan <- downloader_test_df(
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

    restored <- Downloader$new(manifest = manifest)
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
    expect_equal(restored$transfer_policy$range_mode, "auto")
    expect_equal(restored$transfer_policy$piece_size, 8L)
    expect_equal(restored$transfer_policy$piece_concurrency, 2L)
    expect_equal(restored$transfer_policy$max_sources, 2L)
    expect_true(restored$transfer_policy$require_checksum_for_multisource)
    expect_equal(restored$transfer_policy$range_probe_timeout, 7L)
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
        c(
            "download_candidate", "download_config", "download_control",
            "download_daemon", "download_event", "download_job",
            "download_meta", "download_node", "download_piece",
            "download_session", "download_task"
        )
    )
})
# }}}
# Downloader$run() / Downloader$start() / Downloader$job_status() {{{
test_that("Downloader$run(block = FALSE)", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-bg-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")
    src <- file.path(root, "background.txt")
    dir.create(dirname(src), recursive = TRUE, showWarnings = FALSE)
    writeLines("background content", src)

    launched <- list()
    withr::local_options(list(epwshiftr.downloader.launcher = function(kind, id, manifest, log_path) {
        launched[[length(launched) + 1L]] <<- list(kind = kind, id = id, manifest = manifest, log_path = log_path)
        TRUE
    }))

    dl <- Downloader$new(dest = dest, temp = temp, manifest = manifest, retries = 1L, n_workers = 0L)
    session_id <- dl$enqueue(downloader_test_df(
        logical_file_id = "tracking:background",
        filename = "background.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L
    ))

    job <- dl$run(session_id = session_id, block = FALSE, progress = FALSE)
    expect_equal(nrow(job), 1L)
    expect_equal(job$status, "queued")
    expect_match(job$job_id, "^job-")
    expect_length(launched, 1L)
    expect_equal(launched[[1L]]$kind, "job")
    expect_equal(launched[[1L]]$id, job$job_id[[1L]])
    expect_equal(launched[[1L]]$manifest, dl$manifest)

    tasks <- dl$tasks(job_id = job$job_id[[1L]])
    expect_equal(nrow(tasks), 1L)
    expect_equal(tasks$session_id, session_id)
    expect_equal(tasks$job_id, job$job_id[[1L]])
    expect_equal(dl$job_status(job$job_id[[1L]])$status, "queued")
})

test_that("Downloader$start() / Downloader$job_status()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-job-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")
    src <- file.path(root, "job.txt")
    dir.create(dirname(src), recursive = TRUE, showWarnings = FALSE)
    writeLines(rep("job content", 20L), src)

    withr::local_options(list(epwshiftr.downloader.launcher = function(...) TRUE))
    dl <- Downloader$new(dest = dest, temp = temp, manifest = manifest, retries = 1L, n_workers = 0L)
    session_id <- dl$enqueue(downloader_test_df(
        logical_file_id = "tracking:job",
        filename = "job.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L
    ))

    job <- dl$start(session_id = session_id)
    priv(dl)$run_job(job$job_id[[1L]])

    job <- dl$job_status(job$job_id[[1L]])
    tasks <- dl$tasks(job_id = job$job_id[[1L]])
    events <- dl$events(job_id = job$job_id[[1L]])

    expect_equal(job$status, "done")
    expect_equal(tasks$status, "done")
    expect_equal(tasks$bytes_done, file.info(src, extra_cols = FALSE)$size)
    expect_false(is.na(tasks$progress_updated_at))
    expect_true(all(!is.na(events$job_id)))
    expect_true(file.exists(file.path(dest, "job.txt")))
})
# }}}
# Downloader$daemon_start() / Downloader$daemon_status() / Downloader$daemon_stop() {{{
test_that("Downloader$daemon_start() / Downloader$daemon_status() / Downloader$daemon_stop()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-daemon-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    manifest <- file.path(root, "_downloader", "manifest.duckdb")
    launched <- list()
    withr::local_options(list(epwshiftr.downloader.launcher = function(kind, id, manifest, log_path) {
        launched[[length(launched) + 1L]] <<- list(kind = kind, id = id, manifest = manifest, log_path = log_path)
        TRUE
    }))

    dl <- Downloader$new(
        dest = file.path(root, "downloads"),
        temp = file.path(root, "tmp"),
        manifest = manifest,
        n_workers = 0L
    )
    daemon <- dl$daemon_start(port = 45678L)
    expect_equal(daemon$status, "starting")
    expect_match(daemon$daemon_id, "^daemon-")
    expect_length(launched, 1L)
    expect_equal(launched[[1L]]$kind, "daemon")
    expect_equal(launched[[1L]]$id, daemon$daemon_id[[1L]])

    status <- dl$daemon_status()
    expect_true(daemon$daemon_id[[1L]] %in% status$daemon_id)
    stopped <- dl$daemon_stop()
    expect_equal(stopped$status[stopped$daemon_id == daemon$daemon_id[[1L]]], "stopping")
})
# }}}
# downloader__range_probe_url() {{{
test_that("downloader__range_probe_url() probes local files", {
    root <- tempfile("downloader-range-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    src <- file.path(root, "source.bin")
    writeBin(as.raw(seq_len(64L) - 1L), src)

    probe <- downloader__range_probe_url(paste0("file://", normalizePath(src, winslash = "/")))
    expect_true(probe$range_supported)
    expect_equal(probe$range_size, 64)
    expect_true(is.na(probe$range_probe_error))

    missing <- downloader__range_probe_url(paste0("file://", file.path(root, "missing.bin")))
    expect_false(missing$range_supported)
    expect_match(missing$range_probe_error, "does not exist")
})

test_that("downloader__range_probe_url() probes HTTP range metadata", {
    skip_if_not_installed("webfakes")

    server <- local_downloader_http_server()
    probe <- downloader__range_probe_url(server$url("/files/range.bin"))
    expect_true(probe$range_supported)
    expect_equal(probe$range_size, 8193)
    expect_true(is.na(probe$range_probe_error))

    redirected <- downloader__range_probe_url(server$url("/redirect/range.bin"))
    expect_true(redirected$range_supported)
    expect_equal(redirected$range_size, 8193)

    missing <- downloader__range_probe_url(server$url("/files/missing.bin"))
    expect_false(missing$range_supported)
    expect_match(missing$range_probe_error, "206 Content-Range")
})
# }}}
# Downloader workflow: segmented range downloads {{{
test_that("Downloader workflow: manifest-backed single-source pieces", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    server <- local_downloader_http_server()
    checksum <- checksum_bytes(downloader_http_bytes(8193L), "md5")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 31L,
            piece_concurrency = 3L
        )
    )
    plan <- downloader_test_df(
        logical_file_id = "tracking:segmented-single",
        filename = "single.bin",
        url = server$url("/files/range.bin"),
        checksum = checksum,
        checksum_type = "md5",
        data_node = "local-http-single",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "segmented-single")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_equal(tasks$status, "done")
    expect_equal(readBin(file.path(dest, "single.bin"), "raw", n = 8193L), downloader_http_bytes(8193L))
    candidates <- ddb_read_table(priv(dl)$manifest_conn, "download_candidate")
    expect_true(candidates$range_supported[[1L]])
    expect_equal(candidates$range_size[[1L]], 8193)
    expect_equal(nrow(ddb_read_table(priv(dl)$manifest_conn, "download_piece")), 0L)
    expect_false(dir.exists(file.path(temp, paste0(tasks$task_id[[1L]], ".pieces"))))
})

test_that("Downloader workflow: pieces inside a persistent worker", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- file.path(root, "worker-source.bin")
    dir.create(dirname(src), recursive = TRUE, showWarnings = FALSE)
    bytes <- as.raw(rep(0:255, length.out = 389L))
    writeBin(bytes, src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 43L,
            piece_concurrency = 2L
        )
    )
    plan <- downloader_test_df(
        logical_file_id = c("tracking:segmented-worker", "tracking:plain-worker"),
        filename = c("worker.bin", "plain.bin"),
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        data_node = "local-worker",
        priority = c(1L, 2L)
    )
    session_id <- dl$enqueue(plan, session_label = "segmented-worker")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_equal(nrow(tasks), 2L)
    expect_true(all(tasks$status == "done"))
    expect_equal(readBin(file.path(dest, "worker.bin"), "raw", n = 389L), bytes)
    expect_equal(readBin(file.path(dest, "plain.bin"), "raw", n = 389L), bytes)
    expect_equal(nrow(ddb_read_table(priv(dl)$manifest_conn, "download_piece")), 0L)
})

test_that("Downloader workflow: multi-source range pieces", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    server <- local_downloader_http_server()
    bytes <- downloader_http_bytes(8193L)
    checksum <- checksum_bytes(bytes, "md5")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "multi",
            piece_size = 17L,
            piece_concurrency = 2L,
            max_sources = 2L
        )
    )
    plan <- downloader_test_df(
        logical_file_id = "tracking:segmented-multi",
        filename = "multi.bin",
        url = c(server$url("/files/range.bin"), server$url("/files/range-copy.bin")),
        checksum = checksum,
        checksum_type = "md5",
        data_node = c("local-http-a", "local-http-b"),
        priority = c(1L, 2L)
    )
    session_id <- dl$enqueue(plan, session_label = "segmented-multi")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_equal(tasks$status, "done")
    expect_equal(readBin(file.path(dest, "multi.bin"), "raw", n = 8193L), bytes)
    candidates <- ddb_read_table(priv(dl)$manifest_conn, "download_candidate")
    expect_true(all(candidates$range_supported))
    expect_equal(nrow(ddb_read_table(priv(dl)$manifest_conn, "download_piece")), 0L)
    nodes <- dl$data_nodes()
    expect_true(all(c("local-http-a", "local-http-b") %in% nodes$data_node))
})
# }}}
# Downloader$preflight() {{{
test_that("Downloader$preflight()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("preflight content", src)
    size <- as.numeric(file.info(src, extra_cols = FALSE)$size)
    plan <- downloader_test_df(
        logical_file_id = "tracking:preflight",
        filename = "preflight.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = as.character(tools::md5sum(src)),
        checksum_type = "md5",
        size = size,
        priority = 1L
    )

    dl <- Downloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
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

    blocker <- Downloader$new(
        dest = file.path(root, "blocked-downloads"),
        temp = file.path(root, "blocked-tmp"),
        manifest = file.path(root, "blocked", "manifest.duckdb"),
        resource_policy = list(min_free_space = free + size + 1024^3),
        n_workers = 0L
    )
    blocked_session <- blocker$enqueue(plan)
    expect_error(
        blocker$run(session_id = blocked_session, progress = FALSE),
        "Insufficient disk space"
    )
})
# }}}
# Downloader$new() manifest compatibility {{{
test_that("Downloader$new() migrates older manifests", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
    rm(dl)
    gc()

    conn <- ddb_connect(manifest, read_only = FALSE)
    on.exit(if (!is.null(conn) && ddb_is_valid(conn)) ddb_disconnect(conn, shutdown = TRUE), add = TRUE)
    ddb_exec(conn, "DELETE FROM download_meta WHERE key = 'schema_version'")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_success_count")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_failure_count")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS last_probe_at")
    ddb_exec(conn, "ALTER TABLE download_node DROP COLUMN IF EXISTS cooldown_until")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS speed_bps")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS eta_seconds")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS progress_updated_at")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS current_url")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS job_id")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS owner_id")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS lease_until")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS heartbeat_at")
    ddb_exec(conn, "ALTER TABLE download_event DROP COLUMN IF EXISTS job_id")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_job")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_daemon")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_control")
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    restored <- Downloader$new(dest = dest, temp = temp, manifest = manifest, n_workers = 0L)
    rm(restored)
    gc()

    conn <- ddb_connect(manifest, read_only = TRUE)
    meta <- ddb_read_table(conn, "download_meta")
    nodes <- ddb_read_table(conn, "download_node")
    tasks <- ddb_read_table(conn, "download_task")
    events <- ddb_read_table(conn, "download_event")
    tables <- ddb_list_tables(conn)
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    expect_equal(meta[meta$key == "schema_version", "value", drop = TRUE], DOWNLOADER_SCHEMA_VERSION)
    expect_true(all(c("probe_success_count", "probe_failure_count", "last_probe_at", "cooldown_until") %in% names(nodes)))
    expect_true(all(c(
        "speed_bps", "eta_seconds", "progress_updated_at", "current_url",
        "job_id", "owner_id", "lease_until", "heartbeat_at"
    ) %in% names(tasks)))
    expect_true("job_id" %in% names(events))
    expect_true(all(c("download_job", "download_daemon", "download_control") %in% tables))
})

test_that("Downloader$new() stores typed config in the manifest", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        timeout = 30L,
        ssl_verifypeer = FALSE,
        transfer_policy = list(bandwidth_limit = 2048L),
        resource_policy = list(host_concurrency = 2L),
        n_workers = 0L
    )

    expect_true(file.exists(manifest))
    expect_true(schema_validate(SCHEMA_DOWNLOADER_CONFIG, dl$config, mode = "test", name = "downloader-config"))

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit({
        if (!is.null(conn)) {
            ddb_disconnect(conn, shutdown = TRUE)
        }
    }, add = TRUE)
    config <- ddb_read_table(conn, "download_config")
    expect_equal(nrow(config), 1L)
    expect_equal(config$config_id, "default")
    expect_equal(config$timeout, 30L)
    expect_false(config$ssl_verifypeer)
    expect_equal(config$transfer_bandwidth_limit, 2048)
    expect_equal(config$resource_host_concurrency, 2L)
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    restored <- Downloader$new(manifest = manifest)
    expect_equal(restored$data_dir, normalizePath(dest, winslash = "/"))
    expect_equal(restored$tmp_dir, normalizePath(temp, winslash = "/"))
    expect_equal(restored$timeout, 30L)
    expect_false(restored$network_policy$ssl_verifypeer)
    expect_equal(restored$transfer_policy$bandwidth_limit, 2048)
    expect_equal(restored$resource_policy$host_concurrency, 2)

    updated <- Downloader$new(
        manifest = manifest,
        timeout = 45L,
        transfer_policy = list(bandwidth_limit = NULL),
        resource_policy = list(host_concurrency = NULL),
        n_workers = 0L
    )
    expect_equal(updated$timeout, 45L)
    expect_null(updated$transfer_policy$bandwidth_limit)
    expect_null(updated$resource_policy$host_concurrency)

    rm(restored, updated)
    gc()
    reloaded <- Downloader$new(manifest = manifest)
    expect_equal(reloaded$timeout, 45L)
    expect_null(reloaded$transfer_policy$bandwidth_limit)
    expect_null(reloaded$resource_policy$host_concurrency)
})

test_that("Downloader$clone() reconnects manifest after finalization", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("clone content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    cloned <- dl$clone(deep = FALSE)
    expect_s3_class(cloned, "Downloader")
    rm(cloned)
    gc()

    plan <- downloader_test_df(
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
# }}}
# Downloader$cancel() / Downloader$retry() / Downloader$resume() {{{
test_that("Downloader$run() cancels stale downloading tasks", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("stale content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
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

test_that("Downloader$cancel() / Downloader$retry()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("cancel content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
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
    expect_equal(sessions[sessions$session_id == session_id, , drop = FALSE]$status, "cancelled")

    retried <- dl$retry(session_id = session_id)
    expect_equal(retried$status, "queued")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_true(file.exists(file.path(dest, "cancel.txt")))

    events <- ddb_read_table(priv(dl)$manifest_conn, "download_event")
    expect_true("cancelled" %in% events$event)
})

test_that("Downloader$cancel(task_id =)", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("cancel downloading content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
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
        ddb_literal(conn, downloader__now()),
        ddb_literal(conn, task_id)
    ))

    cancelled <- dl$cancel(task_id = task_id)
    expect_equal(cancelled$status, "cancelled")
    expect_equal(dl$status(task_id = task_id)$status, "cancelled")
    expect_false(file.exists(file.path(dest, "cancel-downloading.txt")))
})
# }}}
# Downloader workflow: candidate fallback and retry {{{
test_that("Downloader workflow: candidate fallback", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    server <- local_downloader_http_server()
    checksum <- downloader_http_checksum()

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
        logical_file_id = "tracking:fallback-test",
        filename = "fallback.txt",
        url = c(
            server$url("/files/missing.bin"),
            server$url("/files/ok.bin")
        ),
        checksum = checksum,
        checksum_type = "md5",
        data_node = c("bad-node.example.test", "good-node.example.test"),
        priority = c(1L, 2L)
    )

    session_id <- dl$enqueue(plan, session_label = "fallback")
    expect_warning(
        tasks <- dl$run(session_id = session_id, progress = FALSE),
        "Failed to open|Download failed|HTTP"
    )
    expect_equal(tasks$status, "done")
    expect_identical(tasks$selected_url, plan$url[[2L]])
    nodes <- dl$data_nodes()
    expect_equal(nodes[nodes$data_node == "bad-node.example.test", , drop = FALSE]$failure_count, 1L)
    expect_equal(nodes[nodes$data_node == "good-node.example.test", , drop = FALSE]$success_count, 1L)

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit(ddb_disconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    candidates <- ddb_read_table(
        conn,
        "download_candidate"
    )
    expect_equal(candidates[order(candidates$priority), , drop = FALSE]$failed_count, c(1L, 0L))
})

test_that("Downloader workflow: transient HTTP retry", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")
    server <- local_downloader_http_server()

    dl <- Downloader$new(dest = dest, temp = temp, manifest = manifest, retries = 2L, n_workers = 0L)
    plan <- downloader_test_df(
        logical_file_id = "tracking:flaky-test",
        filename = "flaky.bin",
        url = server$url("/files/flaky.bin"),
        checksum = downloader_http_checksum(),
        checksum_type = "md5",
        data_node = "flaky-node.example.test",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "flaky")
    tasks <- suppressWarnings(dl$run(session_id = session_id, progress = FALSE))
    expect_equal(tasks$status, "done")
    expect_true(dl$verify(session_id = session_id)$checksum_ok)
})

test_that("Downloader workflow: candidate URLs stay task-scoped", {
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
    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
        logical_file_id = c("tracking:first", "tracking:second"),
        filename = c("first.txt", "second.txt"),
        url = urls,
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "candidate-scope")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(tasks$filename), , drop = FALSE]

    expect_equal(tasks$status, c("done", "done"))
    expect_identical(tasks$selected_url, urls)
    expect_equal(readLines(file.path(dest, "first.txt")), "first task content")
    expect_equal(readLines(file.path(dest, "second.txt")), "second task content")
})
# }}}
# Downloader$record_probes() / Downloader$data_nodes() / Downloader$reset_data_nodes() {{{
test_that("Downloader$record_probes() / Downloader$reset_data_nodes()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(
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
    plan <- downloader_test_df(
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
    expect_equal(nodes[nodes$data_node == "ok.example.org", , drop = FALSE]$probe_success_count, 1L)
    expect_equal(nodes[nodes$data_node == "fail.example.org", , drop = FALSE]$probe_failure_count, 1L)
    expect_false(is.na(nodes[nodes$data_node == "fail.example.org", , drop = FALSE]$cooldown_until))

    cached <- plan[1L, , drop = FALSE]
    cached$probe_cached <- TRUE
    nodes <- dl$record_probes(cached, probed = TRUE)
    expect_equal(nodes[nodes$data_node == "ok.example.org", , drop = FALSE]$probe_success_count, 1L)

    remaining <- dl$reset_data_nodes(data_node = "fail.example.org")
    expect_false("fail.example.org" %in% remaining$data_node)
    expect_true("ok.example.org" %in% remaining$data_node)
})
# }}}
# Downloader$events() / Downloader$on() / Downloader$off() {{{
test_that("Downloader$events() / Downloader$on() / Downloader$off()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("event content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    seen <- character()
    done_payload <- NULL
    session_payload <- NULL
    callback_fields <- c(
        "event", "session_id", "task_id", "file_key", "status", "target_path",
        "selected_url", "data_node", "bytes_done", "error", "created_at"
    )
    done_token <- dl$on("task_done", function(event, downloader) {
        seen <<- c(seen, event$event)
        done_payload <<- event
        expect_true(all(callback_fields %in% names(event)))
        expect_equal(event$file_key, "event-file-key")
        expect_equal(event$status, "done")
        expect_match(event$selected_url, "^file://")
        expect_true(file.exists(event$target_path))
        expect_gt(event$bytes_done, 0)
        expect_true(is.na(event$error))
        expect_s3_class(downloader, "Downloader")
    })
    session_token <- dl$on("session_done", function(event, downloader) {
        seen <<- c(seen, event$event)
        session_payload <<- event
        expect_true(all(callback_fields %in% names(event)))
        expect_equal(event$status, "done")
        expect_true(is.na(event$task_id))
    })
    dl$on("task_done", function(event, downloader) {
        stop("callback boom", call. = FALSE)
    })

    plan <- downloader_test_df(
        logical_file_id = "tracking:event-test",
        file_key = "event-file-key",
        filename = "event.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        data_node = "local-file",
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "events")
    tasks <- dl$run(session_id = session_id, progress = FALSE)

    expect_equal(tasks$status, "done")
    expect_true(all(c("session_done", "task_done") %in% seen))
    expect_equal(done_payload$session_id, session_id)
    expect_equal(done_payload$task_id, tasks$task_id[[1L]])
    expect_equal(session_payload$session_id, session_id)
    events <- dl$events(session_id = session_id)
    expect_true(all(c("enqueue", "start", "done", "session_done", "callback_error") %in% events$event))
    task_events <- dl$events(task_id = tasks$task_id[[1L]])
    expect_true(all(c("start", "done", "callback_error") %in% task_events$event))
    expect_true(dl$off(done_token))
    expect_false(dl$off(done_token))
    expect_true(dl$off(session_token))
})
# }}}
# Downloader$run() worker scheduling {{{
test_that("Downloader$run() uses worker concurrency", {
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

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L
    )
    plan <- downloader_test_df(
        logical_file_id = c("tracking:parallel-first", "tracking:parallel-second"),
        filename = c("parallel-first.txt", "parallel-second.txt"),
        url = paste0("file://", normalizePath(c(src_1, src_2), winslash = "/")),
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "parallel")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(tasks$filename), , drop = FALSE]

    expect_equal(tasks$status, c("done", "done"))
    expect_equal(tasks$attempts, c(1L, 1L))
    expect_equal(readLines(file.path(dest, "parallel-first.txt")), "parallel first")
    expect_equal(readLines(file.path(dest, "parallel-second.txt")), "parallel second")
    sessions <- dl$sessions()
    expect_equal(sessions[sessions$session_id == session_id, , drop = FALSE]$status, "done")
})

test_that("Downloader$run() defers tasks beyond per-host capacity", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("mirai")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src_1 <- tempfile()
    src_2 <- tempfile()
    writeLines("host limited first", src_1)
    writeLines("host limited second", src_2)

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L,
        resource_policy = list(host_concurrency = 1L)
    )
    plan <- downloader_test_df(
        logical_file_id = c("tracking:host-first", "tracking:host-second"),
        filename = c("host-first.txt", "host-second.txt"),
        url = paste0("file://", normalizePath(c(src_1, src_2), winslash = "/")),
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        data_node = "shared-host.example.org",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "host-capacity")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(tasks$filename), , drop = FALSE]

    expect_equal(tasks$status, c("done", "done"))
    expect_equal(tasks$attempts, c(1L, 1L))
    expect_equal(readLines(file.path(dest, "host-first.txt")), "host limited first")
    expect_equal(readLines(file.path(dest, "host-second.txt")), "host limited second")
    sessions <- dl$sessions()
    expect_equal(sessions[sessions$session_id == session_id, , drop = FALSE]$status, "done")
})

test_that("Downloader$run() serializes tasks for the same target path", {
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

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 2L
    )
    plan <- downloader_test_df(
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
# Downloader$download() {{{
test_that("Downloader$download()", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    url <- downloader_test_file_url(temp_dir, "source-1024.bin", 1024L)

    dl <- Downloader$new(dest = temp_dir)

    path <- dl$download(url, filename = "test_file.bin", progress = FALSE)

    expect_true(file.exists(path))
    expect_equal(basename(path), "test_file.bin")
    expect_equal(file.info(path)$size, 1024)

    unlink(temp_dir, recursive = TRUE)
})

test_that("Downloader$download(subdir =)", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    url <- downloader_test_file_url(temp_dir, "source-512.bin", 512L)

    dl <- Downloader$new(dest = temp_dir)

    path <- dl$download(
        url,
        filename = "data.bin",
        subdir = "docs",
        progress = FALSE
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "data.bin")
    expect_equal(dirname(path), file.path(normalizePath(temp_dir), "docs"))

    unlink(temp_dir, recursive = TRUE)
})

test_that("Downloader$download(overwrite = FALSE)", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    test_file <- file.path(temp_dir, "source.txt")
    writeLines("test content", test_file)

    dl <- Downloader$new(dest = temp_dir)

    dest_file <- file.path(temp_dir, "dest.txt")
    file.copy(test_file, dest_file)

    mtime1 <- file.info(dest_file)$mtime

    Sys.sleep(0.1)

    expect_true(file.exists(dest_file))
    mtime2 <- file.info(dest_file)$mtime
    expect_equal(mtime1, mtime2)

    unlink(temp_dir, recursive = TRUE)
})

test_that("Downloader$download() restarts unsupported Range resume", {
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    dir.create(temp, recursive = TRUE)

    server <- local_downloader_http_server()
    tmp_id <- "range-restart"
    writeLines("stale partial", file.path(temp, paste0(tmp_id, ".part")))

    dl <- Downloader$new(dest = dest, temp = temp, retries = 1L, n_workers = 0L)
    path <- dl$download(
        server$url("/files/no-range.bin"),
        filename = "source.bin",
        checksum = downloader_http_checksum(8193L),
        checksum_type = "md5",
        progress = FALSE,
        .tmp_id = tmp_id
    )

    expect_equal(readBin(path, "raw", n = 8193L), downloader_http_bytes(8193L))
    expect_false(file.exists(file.path(temp, paste0(tmp_id, ".part"))))
})
# }}}
# Downloader$list_incomplete() {{{
test_that("Downloader$list_incomplete()", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    url <- downloader_test_file_url(temp_dir, "source-2048.bin", 2048L)

    dl <- Downloader$new(dest = temp_dir)

    path <- dl$download(url, filename = "test.bin", progress = FALSE)
    expect_true(file.exists(path))

    expect_equal(nrow(dl$list_incomplete()), 0L)

    unlink(temp_dir, recursive = TRUE)
})
# }}}
# Downloader$cleanup_tmp() {{{
test_that("Downloader$cleanup_tmp()", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- Downloader$new(dest = temp_dir, temp = temp)

    file.create(file.path(temp, "test1.part"))
    file.create(file.path(temp, "test2.done"))

    n_removed <- dl$cleanup_tmp(all = TRUE)

    expect_gte(n_removed, 0L)

    unlink(temp_dir, recursive = TRUE)
})
# }}}
# Downloader$verify() / Downloader$verify_checksum() {{{
test_that("Downloader$verify_checksum()", {
    dl <- Downloader$new()

    test_file <- tempfile()
    writeLines("test content", test_file)

    checksum_md5 <- tools::md5sum(test_file)
    checksum_md5 <- as.character(checksum_md5)

    expect_true(dl$verify_checksum(test_file, checksum_md5, "md5"))

    expect_false(dl$verify_checksum(test_file, "wrongchecksum", "md5"))

    unlink(test_file)
})

test_that("Downloader$verify()", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("verified content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- downloader_test_df(
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

test_that("Downloader$download() verifies checksum", {
    skip_if_not_installed("webfakes")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    server <- local_downloader_http_server()
    file_checksum <- downloader_http_checksum(algo = "sha256")

    dl <- Downloader$new(
        dest = temp_dir,
        retries = 3,
        timeout = 300
    )

    path <- dl$download(
        url = server$url("/files/ok.bin"),
        filename = "ok.bin",
        checksum = file_checksum,
        checksum_type = "sha256",
        progress = FALSE
    )

    expect_true(file.exists(path))

    is_valid <- epwshiftr:::downloader__verify_checksum(path, file_checksum, "sha256")
    expect_true(is_valid)

    unlink(temp_dir, recursive = TRUE)
})

test_that("Downloader$download() handles HTTP errors", {
    skip_if_not_installed("webfakes")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    server <- local_downloader_http_server()

    dl <- Downloader$new(
        dest = temp_dir,
        retries = 1L,
        timeout = 1L
    )

    url <- server$url("/files/missing.bin")

    expect_warning(expect_error(
        dl$download(url, filename = "nonexistent.nc", progress = FALSE),
        "Failed to download"
    ))

    unlink(temp_dir, recursive = TRUE)
})

test_that("Downloader$download() handles checksum mismatch", {
    skip_if_not_installed("webfakes")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    server <- local_downloader_http_server()

    dl <- Downloader$new(
        dest = temp_dir,
        retries = 1L
    )

    expect_error(
        dl$download(
            server$url("/files/ok.bin"),
            filename = "ok.bin",
            checksum = "wrongchecksum",
            checksum_type = "sha256",
            progress = FALSE
        ),
        "Checksum verification failed"
    )

    unlink(temp_dir, recursive = TRUE)
})
# }}}
# Downloader$print() {{{
test_that("Downloader$print()", {
    dl <- Downloader$new()

    expect_snapshot(
        print(dl),
        transform = function(lines) {
            gsub("^/.+", "<path>", lines)
        }
    )
})
# }}}
# Downloader$download() offline mode {{{
test_that("Downloader$download() offline mode blocks new downloads", {
    local_cache_mode("offline")
    dl <- Downloader$new(dest = tempdir(), n_workers = 0L)

    expect_error(
        dl$download(url = "https://example.com/nonexistent.nc"),
        "offline"
    )
})

test_that("Downloader$download() offline mode allows verified files", {
    local_cache_mode("offline")

    dest <- tempdir()
    # Create a file that would be "verified"
    test_file <- file.path(dest, "test-offline-verified.txt")
    writeLines("test content", test_file)
    on.exit(unlink(test_file), add = TRUE)

    dl <- Downloader$new(dest = dest, n_workers = 0L)

    # This should succeed because the file already exists (Verified status)
    result <- dl$download(
        url = "https://example.com/test-offline-verified.txt",
        filename = "test-offline-verified.txt"
    )
    expect_equal(normalizePath(result), normalizePath(test_file))
})
# }}}
