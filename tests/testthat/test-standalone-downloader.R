# downloader_test_df() / downloader_test_file_url() / downloader_test_queued_job() {{{
# downloader_test__http_server {{{
downloader_test__file_env <- environment()
downloader_test__http_state <- new.env(parent = emptyenv())
downloader_test__http_state$server <- NULL

downloader_test__http_server <- function() {
    skip_if_not_installed("webfakes")
    srv <- downloader_test__http_state$server
    if (is.null(srv)) {
        srv <- local_downloader_http_server(env = downloader_test__file_env)
        downloader_test__http_state$server <- srv
        withr::defer(
            downloader_test__http_state$server <- NULL,
            envir = downloader_test__file_env
        )
    }
    res <- curl::curl_fetch_memory(srv$url("/reset"))
    stopifnot(res$status_code == 200L)
    srv
}
# }}}

# downloader_test_df {{{
downloader_test_df <- function(...) {
    data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}
# }}}

# downloader_test_file_url {{{
downloader_test_file_url <- function(root, filename, size) {
    path <- file.path(root, filename)
    writeBin(as.raw(rep(0:255, length.out = size)), path)
    paste0("file://", normalizePath(path, winslash = "/"))
}
# }}}

# downloader_test__fixture {{{
downloader_test__fixture <- function(
    n_workers = 0L,
    retries = 1L,
    ...,
    manifest = TRUE,
    env = parent.frame()
) {
    root <- withr::local_tempdir("downloader-", .local_envir = env)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    m <- if (isTRUE(manifest)) file.path(root, "_downloader", "manifest.duckdb")
    if (is.character(manifest)) {
        m <- manifest
    }
    args <- list(
        dest = dest,
        temp = temp,
        retries = retries,
        n_workers = n_workers,
        ...
    )
    if (!is.null(m)) {
        args$manifest <- m
    }
    dl <- do.call(Downloader$new, args)
    if (!is.null(m)) {
        withr::defer(
            try(priv(dl)$disconnect_manifest(), silent = TRUE),
            envir = env
        )
    }
    list(root = root, dest = dest, temp = temp, manifest = m, dl = dl)
}
# }}}

# downloader_test__local_plan {{{
downloader_test__local_plan <- function(
    root,
    text,
    filename,
    logical_file_id = paste0("tracking:", tools::file_path_sans_ext(filename)),
    checksum = TRUE,
    ...
) {
    n <- length(filename)
    src <- file.path(root, sprintf("src-%d-%s", seq_len(n), basename(filename)))
    for (i in seq_len(n)) {
        dir.create(dirname(src[[i]]), recursive = TRUE, showWarnings = FALSE)
        if (is.raw(text)) {
            writeBin(text, src[[i]])
        } else {
            val <- if (n == 1L) text else text[[min(i, length(text))]]
            writeLines(val, src[[i]])
        }
    }
    args <- list(
        logical_file_id = logical_file_id,
        filename = filename,
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L,
        ...
    )
    if (isTRUE(checksum)) {
        args$checksum <- as.character(tools::md5sum(src))
        args$checksum_type <- "md5"
    }
    plan <- do.call(downloader_test_df, args)
    attr(plan, "src") <- src
    plan
}
# }}}

# downloader_test_queued_job {{{
downloader_test_queued_job <- function(label = "job") {
    root <- tempfile("downloader-job-")
    withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())

    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")
    src <- file.path(root, paste0(label, ".txt"))
    dir.create(dirname(src), recursive = TRUE, showWarnings = FALSE)
    writeLines(paste(label, "content"), src)

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    session_id <- dl$enqueue(downloader_test_df(
        logical_file_id = paste0("tracking:", label),
        filename = paste0(label, ".txt"),
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L
    ))

    test_local_dependencies(list(downloader__launch_process = function(...) {
        TRUE
    }))
    job <- dl$run(session_id = session_id, block = FALSE, progress = FALSE)
    task_id <- dl$tasks(job_id = job$job_id[[1L]])$task_id[[1L]]

    list(
        root = root,
        dl = dl,
        session_id = session_id,
        task_id = task_id,
        job = job
    )
}
# }}}

# }}}
# Downloader$new() {{{
test_that("Downloader$new()", {
    dl <- Downloader$new()
    expect_s3_class(dl, "Downloader")
    expect_true(dir.exists(dl$data_dir) && dir.exists(dl$tmp_dir))
    expect_equal(c(dl$max_retries, dl$timeout), c(3L, 3600L))
    expect_equal(
        dl$network_policy,
        list(
            ssl_verifypeer = TRUE,
            proxy = NULL,
            connect_timeout = NULL,
            useragent = NULL
        )
    )
    expect_equal(dl$node_policy$cooldown_after_failures, 3L)
    expect_equal(dl$transfer_policy, DOWNLOADER_TRANSFER_POLICY_DEFAULT)
    expect_equal(dl$resource_policy, DOWNLOADER_RESOURCE_POLICY_DEFAULT)

    d <- withr::local_tempdir("downloader-new-")
    tmp <- file.path(d, ".tmp_test")
    tp <- list(
        chunk_size = 65536,
        bandwidth_limit = 1048576,
        low_speed_limit = 128,
        low_speed_time = 10,
        range_mode = "single",
        piece_size = 1024L,
        piece_concurrency = 2L,
        max_sources = 3L,
        require_checksum_for_multisource = FALSE,
        range_probe_timeout = 11L
    )
    rp <- list(
        host_concurrency = 2L,
        disk_preflight = FALSE,
        min_free_space = 1024
    )
    dl <- Downloader$new(
        dest = d,
        temp = tmp,
        retries = 5L,
        timeout = 7200L,
        ssl_verifypeer = FALSE,
        proxy = "http://proxy.example:8080",
        connect_timeout = 5L,
        useragent = "epwshiftr-test",
        transfer_policy = tp,
        resource_policy = rp,
        cleanup = FALSE
    )
    expect_equal(dl$data_dir, normalizePath(d, winslash = "/"))
    expect_equal(dl$tmp_dir, normalizePath(tmp, "/", mustWork = FALSE))
    expect_equal(c(dl$max_retries, dl$timeout), c(5L, 7200L))
    expect_equal(
        dl$network_policy,
        list(
            ssl_verifypeer = FALSE,
            proxy = "http://proxy.example:8080",
            connect_timeout = 5L,
            useragent = "epwshiftr-test"
        )
    )
    expect_equal(dl$transfer_policy, tp)
    expect_equal(dl$resource_policy, rp)
})
# }}}
# Downloader$enqueue() {{{
test_that("Downloader$enqueue() creates sessions and tasks", {
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

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit(ddb_disconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    expect_setequal(
        ddb_list_tables(conn),
        c(
            "download_candidate",
            "download_config",
            "download_control",
            "download_daemon",
            "download_event",
            "download_job",
            "download_meta",
            "download_node",
            "download_piece",
            "download_session",
            "download_task"
        )
    )
})
# }}}
# Downloader$sessions() / $tasks() / $status() / $run() / $verify() {{{
test_that("Downloader$run() manages sessions, tasks, status, verify, and persisted config", {
    np <- list(
        cooldown_after_failures = 1L,
        cooldown_seconds = 60L,
        history_ttl_seconds = 600L,
        min_attempts = 1L
    )
    tp <- list(
        chunk_size = 32768,
        bandwidth_limit = 2048,
        low_speed_limit = 64,
        low_speed_time = 5,
        range_mode = "auto",
        piece_size = 8L,
        piece_concurrency = 2L,
        max_sources = 2L,
        require_checksum_for_multisource = TRUE,
        range_probe_timeout = 7L
    )
    rp <- list(
        host_concurrency = 1L,
        disk_preflight = TRUE,
        min_free_space = 4096
    )
    fx <- downloader_test__fixture(
        timeout = 30L,
        ssl_verifypeer = FALSE,
        connect_timeout = 2L,
        useragent = "epwshiftr-test",
        node_policy = np,
        transfer_policy = tp,
        resource_policy = rp
    )
    dl <- fx$dl
    plan <- downloader_test__local_plan(
        fx$root,
        "persistent content",
        "local.txt"
    )
    sid <- dl$enqueue(plan, session_label = "unit-test")

    s <- dl$sessions()
    expect_equal(
        c(nrow(s), s$session_id, s$label, s$status),
        c("1", sid, "unit-test", "queued")
    )

    q <- dl$tasks(session_id = sid)
    expect_equal(c(nrow(q), q$session_id, q$status), c("1", sid, "queued"))
    expect_equal(nrow(dl$tasks(session_id = sid, status = "done")), 0L)
    expect_equal(nrow(dl$tasks(status = "queued")), 1L)

    tid <- q$task_id[[1L]]
    expect_equal(dl$status(session_id = sid)$task_id, tid)
    expect_equal(dl$status(task_id = tid)$session_id, sid)
    expect_equal(nrow(dl$status(task_id = "missing-task")), 0L)

    t <- dl$run(session_id = sid, progress = FALSE)
    expect_equal(t$status, "done")
    expect_equal(c(t$candidate_count, t$failed_candidate_count), c(1L, 0L))
    expect_true(file.exists(file.path(fx$dest, "local.txt")))
    expect_true(dl$verify(session_id = sid)$checksum_ok)

    r <- Downloader$new(manifest = fx$manifest)
    withr::defer(priv(r)$disconnect_manifest())
    expect_equal(r$data_dir, normalizePath(fx$dest, "/"))
    expect_equal(r$tmp_dir, normalizePath(fx$temp, "/", mustWork = FALSE))
    expect_equal(r$manifest, normalizePath(fx$manifest, "/", mustWork = FALSE))
    expect_equal(
        r$network_policy,
        list(
            ssl_verifypeer = FALSE,
            proxy = NULL,
            connect_timeout = 2L,
            useragent = "epwshiftr-test"
        )
    )
    expect_equal(r$node_policy[names(np)], np)
    expect_equal(r$transfer_policy, tp)
    expect_equal(r$resource_policy, rp)
    expect_equal(r$tasks(session_id = sid)$status, "done")
})
# }}}
# Downloader background jobs & daemon {{{
test_that("Downloader$run(block = FALSE), $jobs(), $job_logs(), and $stop_job()", {
    fx <- downloader_test__fixture()
    dl <- fx$dl
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
            manifest = manifest
        )
        TRUE
    }))
    p <- downloader_test__local_plan(
        fx$root,
        "bg content",
        "bg.txt",
        checksum = FALSE
    )
    sid <- dl$enqueue(p)

    job <- dl$run(session_id = sid, block = FALSE, progress = FALSE)
    jid <- job$job_id[[1L]]
    expect_equal(c(nrow(job), job$status), c("1", "queued"))
    expect_match(jid, "^job-")
    expect_length(launched, 1L)
    expect_equal(
        launched[[1L]],
        list(kind = "job", id = jid, manifest = dl$manifest)
    )

    tasks <- dl$tasks(job_id = jid)
    expect_equal(
        c(nrow(tasks), tasks$session_id, tasks$job_id),
        c("1", sid, jid)
    )
    expect_equal(dl$job_status(jid)$status, "queued")
    expect_equal(dl$jobs()$job_id, jid)
    expect_equal(dl$jobs(status = "queued")$job_id, jid)
    expect_equal(nrow(dl$jobs(status = "done")), 0L)

    lp <- job$log_path[[1L]]
    dir.create(dirname(lp), recursive = TRUE, showWarnings = FALSE)
    writeLines(c("first", "second", "third"), lp)
    logs <- dl$job_logs(jid, tail = 2L)
    expect_equal(logs$job_id, rep(jid, 2L))
    expect_equal(logs$line, 1:2)
    expect_equal(logs$message, c("second", "third"))
    expect_error(dl$job_logs("missing-job"), "Downloader job not found")

    stopped <- dl$stop_job(jid)
    expect_equal(stopped$status, "cancelled")
    expect_equal(stopped$error, "Cancelled by user.")
    t2 <- dl$tasks(job_id = jid)
    expect_equal(t2$status, "cancelled")
    expect_match(t2$last_error, "Cancelled by user")
    ctl <- ddb_read_table(priv(dl)$manifest_conn, "download_control")
    expect_equal(
        c(ctl$target_type, ctl$target_id, ctl$command, ctl$status),
        c("job", jid, "stop", "queued")
    )
    expect_error(dl$stop_job("missing-job"), "Downloader job not found")
})

test_that("Downloader$start() / Downloader$job_status()", {
    fx <- downloader_test__fixture()
    p <- downloader_test__local_plan(
        fx$root,
        rep("job content", 20L),
        "job.txt",
        checksum = FALSE
    )
    test_local_dependencies(list(downloader__launch_process = function(...) {
        TRUE
    }))

    job <- fx$dl$start(session_id = fx$dl$enqueue(p))
    jid <- job$job_id[[1L]]
    priv(fx$dl)$run_job(jid)

    t <- fx$dl$tasks(job_id = jid)
    expect_equal(fx$dl$job_status(jid)$status, "done")
    expect_equal(t$status, "done")
    expect_equal(
        t$bytes_done,
        file.info(attr(p, "src"), extra_cols = FALSE)$size
    )
    expect_false(is.na(t$progress_updated_at))
    expect_true(all(!is.na(fx$dl$events(job_id = jid)$job_id)))
    expect_true(file.exists(file.path(fx$dest, "job.txt")))
})

test_that("Downloader$daemon_start() / $daemon_status() / $daemon_stop()", {
    fx <- downloader_test__fixture()
    launched <- list()
    test_local_dependencies(list(downloader__launch_process = function(
        kind,
        id,
        manifest,
        log_path
    ) {
        launched[[length(launched) + 1L]] <<- list(kind = kind, id = id)
        TRUE
    }))
    d <- fx$dl$daemon_start(port = 45678L)
    did <- d$daemon_id[[1L]]
    expect_equal(d$status, "starting")
    expect_match(did, "^daemon-")
    expect_equal(launched[[1L]], list(kind = "daemon", id = did))
    expect_true(did %in% fx$dl$daemon_status()$daemon_id)
    s <- fx$dl$daemon_stop()
    expect_equal(s$status[s$daemon_id == did], "stopping")
})
# }}}
# downloader__worker_dependencies() {{{
test_that("serialized downloader worker dependencies support both worker algorithms", {
    root <- tempfile("downloader-worker-dependencies-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    src <- file.path(root, "source.bin")
    bytes <- as.raw(seq_len(64L) - 1L)
    writeBin(bytes, src)
    checksum <- as.character(tools::md5sum(src))
    file_url <- paste0("file://", normalizePath(src, winslash = "/"))

    dependencies <- unserialize(serialize(
        downloader__worker_dependencies(),
        NULL
    ))
    ordinary <- downloader__worker_download(
        url = file_url,
        filename = "ordinary.bin",
        subdir = NULL,
        dest = dest,
        temp = temp,
        retries = 1L,
        timeout = 30L,
        overwrite = FALSE,
        checksum = checksum,
        checksum_type = "md5",
        resume = TRUE,
        tmp_id = "ordinary",
        transfer_policy = list(chunk_size = 16L),
        worker_dependencies = dependencies
    )
    expect_true(ordinary$ok)
    expect_identical(readBin(ordinary$path, "raw", n = 64L), bytes)

    pieces <- data.frame(
        piece_index = 1L,
        start_byte = 0,
        end_byte = 63,
        byte_count = 64,
        path = file.path(temp, "segmented.pieces", "piece-1.part"),
        status = "pending",
        attempts = 0L,
        bytes_done = 0,
        last_error = NA_character_,
        candidate_id = "candidate-1",
        stringsAsFactors = FALSE
    )
    segmented <- downloader__worker_segmented_download(
        candidates = data.frame(
            candidate_id = "candidate-1",
            url = file_url,
            stringsAsFactors = FALSE
        ),
        pieces = pieces,
        filename = "segmented.bin",
        subdir = NULL,
        dest = dest,
        temp = temp,
        retries = 1L,
        timeout = 30L,
        overwrite = FALSE,
        checksum = checksum,
        checksum_type = "md5",
        tmp_id = "segmented",
        transfer_policy = list(chunk_size = 16L, piece_concurrency = 1L),
        mode_used = "single",
        worker_dependencies = dependencies
    )
    expect_true(segmented$ok)
    expect_identical(readBin(segmented$path, "raw", n = 64L), bytes)
})
# }}}
# downloader__range_probe_url() {{{
test_that("downloader__range_probe_url() checks local files and HTTP range metadata", {
    root <- withr::local_tempdir("downloader-range-")
    src <- file.path(root, "source.bin")
    writeBin(as.raw(seq_len(64L) - 1L), src)

    p1 <- downloader__range_probe_url(paste0(
        "file://",
        normalizePath(src, "/")
    ))
    expect_true(p1$range_supported && is.na(p1$range_probe_error))
    expect_equal(p1$range_size, 64)
    p2 <- downloader__range_probe_url(paste0(
        "file://",
        file.path(root, "m.bin")
    ))
    expect_false(p2$range_supported)
    expect_match(p2$range_probe_error, "does not exist")

    skip_on_cran()
    skip_if_not_installed("webfakes")
    srv <- downloader_test__http_server()
    p3 <- downloader__range_probe_url(srv$url("/files/range.bin"))
    expect_true(p3$range_supported && is.na(p3$range_probe_error))
    expect_equal(p3$range_size, 8193)
    p4 <- downloader__range_probe_url(srv$url("/redirect/range.bin"))
    expect_true(p4$range_supported)
    expect_equal(p4$range_size, 8193)
    p5 <- downloader__range_probe_url(srv$url("/files/missing.bin"))
    expect_false(p5$range_supported)
    expect_match(p5$range_probe_error, "206 Content-Range")
})
# }}}
# Downloader$run() segmented pieces {{{
test_that("Downloader$run() downloads manifest-backed single-source pieces", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    srv <- downloader_test__http_server()
    bytes <- downloader_http_bytes(8193L)
    tp <- list(range_mode = "single", piece_size = 31L, piece_concurrency = 3L)
    fx <- downloader_test__fixture(transfer_policy = tp)
    plan <- downloader_test_df(
        logical_file_id = "tracking:segmented-single",
        filename = "single.bin",
        url = srv$url("/files/range.bin"),
        checksum = checksum_bytes(bytes, "md5"),
        checksum_type = "md5",
        data_node = "local-http-single",
        priority = 1L
    )
    sid <- fx$dl$enqueue(plan, session_label = "segmented-single")
    t <- fx$dl$run(session_id = sid, progress = FALSE)
    conn <- priv(fx$dl)$manifest_conn

    expect_equal(t$status, "done")
    expect_equal(readBin(file.path(fx$dest, "single.bin"), "raw", 8193L), bytes)
    cand <- ddb_read_table(conn, "download_candidate")
    expect_true(cand$range_supported[[1L]])
    expect_equal(cand$range_size[[1L]], 8193)
    expect_equal(nrow(ddb_read_table(conn, "download_piece")), 0L)
    expect_false(dir.exists(file.path(
        fx$temp,
        paste0(t$task_id[[1L]], ".pieces")
    )))
})

test_that("Downloader$run() downloads pieces inside a persistent worker", {
    bytes <- as.raw(rep(0:255, length.out = 389L))
    tp <- list(range_mode = "single", piece_size = 43L, piece_concurrency = 2L)
    fx <- downloader_test__fixture(n_workers = 2L, transfer_policy = tp)
    src <- file.path(fx$root, "worker-source.bin")
    writeBin(bytes, src)
    plan <- downloader_test_df(
        logical_file_id = c(
            "tracking:segmented-worker",
            "tracking:plain-worker"
        ),
        filename = c("worker.bin", "plain.bin"),
        url = paste0("file://", normalizePath(src, "/")),
        checksum = as.character(tools::md5sum(src)),
        checksum_type = "md5",
        data_node = "local-worker",
        priority = c(1L, 2L)
    )
    sid <- fx$dl$enqueue(plan, session_label = "segmented-worker")
    t <- fx$dl$run(session_id = sid, progress = FALSE)

    expect_equal(nrow(t), 2L)
    expect_true(all(t$status == "done"))
    expect_equal(readBin(file.path(fx$dest, "worker.bin"), "raw", 389L), bytes)
    expect_equal(readBin(file.path(fx$dest, "plain.bin"), "raw", 389L), bytes)
    expect_equal(
        nrow(ddb_read_table(priv(fx$dl)$manifest_conn, "download_piece")),
        0L
    )
})

test_that("Downloader$run() downloads multi-source range pieces", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    srv <- downloader_test__http_server()
    bytes <- downloader_http_bytes(8193L)
    tp <- list(
        range_mode = "multi",
        piece_size = 17L,
        piece_concurrency = 2L,
        max_sources = 2L
    )
    fx <- downloader_test__fixture(transfer_policy = tp)
    urls <- c(srv$url("/files/range.bin"), srv$url("/files/range-copy.bin"))
    plan <- downloader_test_df(
        logical_file_id = "tracking:segmented-multi",
        filename = "multi.bin",
        url = urls,
        checksum = checksum_bytes(bytes, "md5"),
        checksum_type = "md5",
        data_node = c("local-http-a", "local-http-b"),
        priority = c(1L, 2L)
    )
    sid <- fx$dl$enqueue(plan, session_label = "segmented-multi")
    t <- fx$dl$run(session_id = sid, progress = FALSE)
    conn <- priv(fx$dl)$manifest_conn

    expect_equal(t$status, "done")
    expect_equal(readBin(file.path(fx$dest, "multi.bin"), "raw", 8193L), bytes)
    expect_true(all(ddb_read_table(conn, "download_candidate")$range_supported))
    expect_equal(nrow(ddb_read_table(conn, "download_piece")), 0L)
    expect_true(all(
        c("local-http-a", "local-http-b") %in% fx$dl$data_nodes()$data_node
    ))
})
# }}}
# Downloader$preflight() {{{
test_that("Downloader$preflight()", {
    fx <- downloader_test__fixture()
    p <- downloader_test__local_plan(
        fx$root,
        "preflight content",
        "preflight.txt"
    )
    sz <- as.numeric(file.info(attr(p, "src"), extra_cols = FALSE)$size)
    p$size <- sz

    chk <- fx$dl$preflight(plan = p)
    expect_equal(
        c(chk$task_count, chk$needs_download, chk$size_unknown_count),
        c(1L, 1L, 0L)
    )
    expect_equal(chk$required_bytes, sz)
    expect_false(chk$disk_would_block)
    expect_equal(
        fx$dl$preflight(session_id = fx$dl$enqueue(p))$required_bytes,
        sz
    )

    free <- suppressWarnings(min(
        c(chk$dest_free_bytes, chk$tmp_free_bytes),
        na.rm = TRUE
    ))
    skip_if(!is.finite(free), "Disk free-space check unavailable")

    blk <- downloader_test__fixture(
        resource_policy = list(min_free_space = 1e100)
    )
    expect_error(
        blk$dl$run(session_id = blk$dl$enqueue(p), progress = FALSE),
        "Insufficient disk space"
    )
})
# }}}
# Downloader$new() {{{
test_that("Downloader$new() migrates older manifests", {
    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        n_workers = 0L
    )
    rm(dl)
    gc()

    conn <- ddb_connect(manifest, read_only = FALSE)
    on.exit(
        if (!is.null(conn) && ddb_is_valid(conn)) {
            ddb_disconnect(conn, shutdown = TRUE)
        },
        add = TRUE
    )
    ddb_exec(conn, "DELETE FROM download_meta WHERE key = 'schema_version'")
    ddb_exec(
        conn,
        "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_success_count"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_node DROP COLUMN IF EXISTS probe_failure_count"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_node DROP COLUMN IF EXISTS last_probe_at"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_node DROP COLUMN IF EXISTS cooldown_until"
    )
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS speed_bps")
    ddb_exec(
        conn,
        "ALTER TABLE download_task DROP COLUMN IF EXISTS eta_seconds"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_task DROP COLUMN IF EXISTS progress_updated_at"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_task DROP COLUMN IF EXISTS current_url"
    )
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS job_id")
    ddb_exec(conn, "ALTER TABLE download_task DROP COLUMN IF EXISTS owner_id")
    ddb_exec(
        conn,
        "ALTER TABLE download_task DROP COLUMN IF EXISTS lease_until"
    )
    ddb_exec(
        conn,
        "ALTER TABLE download_task DROP COLUMN IF EXISTS heartbeat_at"
    )
    ddb_exec(conn, "ALTER TABLE download_event DROP COLUMN IF EXISTS job_id")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_job")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_daemon")
    ddb_exec(conn, "DROP TABLE IF EXISTS download_control")
    ddb_exec(
        conn,
        "ALTER TABLE download_config DROP COLUMN transfer_piece_size"
    )
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    restored <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        n_workers = 0L
    )
    expect_identical(restored$transfer_policy$piece_size, 16777216L)
    expect_null(restored$transfer_policy$bandwidth_limit)
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

    expect_equal(
        meta[meta$key == "schema_version", "value", drop = TRUE],
        DOWNLOADER_SCHEMA_VERSION
    )
    expect_true(all(
        c(
            "probe_success_count",
            "probe_failure_count",
            "last_probe_at",
            "cooldown_until"
        ) %in%
            names(nodes)
    ))
    expect_true(all(
        c(
            "speed_bps",
            "eta_seconds",
            "progress_updated_at",
            "current_url",
            "job_id",
            "owner_id",
            "lease_until",
            "heartbeat_at"
        ) %in%
            names(tasks)
    ))
    expect_true("job_id" %in% names(events))
    expect_true(all(
        c("download_job", "download_daemon", "download_control") %in% tables
    ))
})

test_that("Downloader$new() stores typed config in the manifest", {
    fx <- downloader_test__fixture(
        timeout = 30L,
        ssl_verifypeer = FALSE,
        transfer_policy = list(bandwidth_limit = 2048L),
        resource_policy = list(host_concurrency = 2L)
    )
    expect_true(file.exists(fx$manifest))
    expect_true(schema_validate(
        SCHEMA_DOWNLOADER_CONFIG,
        fx$dl$config,
        mode = "test",
        name = "downloader-config"
    ))

    priv(fx$dl)$disconnect_manifest()
    conn <- ddb_connect(fx$manifest, read_only = TRUE)
    cfg <- ddb_read_table(conn, "download_config")
    ddb_disconnect(conn, shutdown = TRUE)
    expect_equal(nrow(cfg), 1L)
    expect_equal(cfg$config_id, "default")
    expect_equal(cfg$timeout, 30L)
    expect_false(cfg$ssl_verifypeer)
    expect_equal(cfg$transfer_bandwidth_limit, 2048)
    expect_equal(cfg$resource_host_concurrency, 2L)

    r <- Downloader$new(manifest = fx$manifest)
    expect_equal(r$data_dir, normalizePath(fx$dest, "/"))
    expect_equal(r$tmp_dir, normalizePath(fx$temp, "/"))
    expect_equal(r$timeout, 30L)
    expect_false(r$network_policy$ssl_verifypeer)
    expect_equal(r$transfer_policy$bandwidth_limit, 2048)
    expect_equal(r$resource_policy$host_concurrency, 2)

    u <- Downloader$new(
        manifest = fx$manifest,
        timeout = 45L,
        n_workers = 0L,
        transfer_policy = list(bandwidth_limit = NULL),
        resource_policy = list(host_concurrency = NULL)
    )
    expect_equal(u$timeout, 45L)
    expect_null(u$transfer_policy$bandwidth_limit)
    expect_null(u$resource_policy$host_concurrency)
    priv(r)$disconnect_manifest()
    priv(u)$disconnect_manifest()

    r2 <- Downloader$new(manifest = fx$manifest)
    withr::defer(priv(r2)$disconnect_manifest())
    expect_equal(r2$timeout, 45L)
    expect_null(r2$transfer_policy$bandwidth_limit)
    expect_null(r2$resource_policy$host_concurrency)
})
# }}}
# Downloader$clone() {{{
test_that("Downloader$clone() reconnects manifest after finalization", {
    fx <- downloader_test__fixture()
    cl <- fx$dl$clone(deep = FALSE)
    expect_s3_class(cl, "Downloader")
    rm(cl)
    gc()

    p <- downloader_test__local_plan(fx$root, "clone content", "clone.txt")
    sid <- fx$dl$enqueue(p, session_label = "clone-reconnect")
    expect_equal(fx$dl$run(session_id = sid, progress = FALSE)$status, "done")
    expect_true(file.exists(file.path(fx$dest, "clone.txt")))
})

# Force collection after enqueue's connection check, when a real allocation
# could otherwise finalize a clone while the shared connection is in use.
test_that("Downloader clones preserve manifest ownership during collection", {
    root <- withr::local_tempdir("downloader-clone-ownership-")
    p <- downloader_test_df(
        logical_file_id = "tracking:clone-ownership",
        filename = "clone.txt",
        url = "http://127.0.0.1:1/clone.txt"
    )
    for (deep in c(FALSE, TRUE)) {
        d <- file.path(root, as.character(deep))
        m <- file.path(d, "manifest.duckdb")
        dl <- Downloader$new(
            dest = file.path(d, "dl"),
            manifest = m,
            n_workers = 0L
        )
        cl <- dl$clone(deep = deep)
        s1 <- dl$enqueue({
            rm(cl)
            gc()
            p
        })
        expect_identical(
            dl$tasks(session_id = s1)$logical_file_id,
            p$logical_file_id
        )

        cl <- dl$clone(deep = deep)
        s2 <- cl$enqueue({
            rm(dl)
            gc()
            p
        })
        expect_identical(
            cl$tasks(session_id = s2)$logical_file_id,
            p$logical_file_id
        )
        rm(cl)
        gc()

        re <- Downloader$new(manifest = m, n_workers = 0L)
        expect_identical(
            re$tasks(session_id = s1)$logical_file_id,
            p$logical_file_id
        )
        expect_identical(
            re$tasks(session_id = s2)$logical_file_id,
            p$logical_file_id
        )
        rm(re)
        gc()
    }
})
# }}}
# Downloader$run() / $resume() / $cancel() / $retry() {{{
test_that("Downloader$run() cancels stale downloading tasks", {
    fx <- downloader_test__fixture()
    p <- downloader_test__local_plan(fx$root, "stale content", "stale.txt")
    sid <- fx$dl$enqueue(p, session_label = "stale")
    tid <- fx$dl$tasks(session_id = sid)$task_id[[1L]]
    conn <- priv(fx$dl)$manifest_conn
    old_ts <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
    sql <- sprintf(
        "UPDATE download_task SET status = 'downloading', updated_at = %s WHERE task_id = %s",
        ddb_literal(conn, old_ts),
        ddb_literal(conn, tid)
    )
    ddb_exec(conn, sql)

    t <- fx$dl$run(session_id = sid, progress = FALSE)
    expect_equal(t$status, "cancelled")
    expect_match(t$last_error, "previous R session")
    expect_false(file.exists(file.path(fx$dest, "stale.txt")))
    expect_true("cancelled" %in% ddb_read_table(conn, "download_event")$event)
    expect_equal(
        fx$dl$resume(session_id = sid, progress = FALSE)$status,
        "done"
    )
    expect_true(file.exists(file.path(fx$dest, "stale.txt")))
})

# An interrupted row replacement must leave the original task runnable
# together with its existing partial bytes.
test_that("Downloader resumes the same task after a manifest write fails", {
    bytes <- charToRaw("retained download bytes")
    fx <- downloader_test__fixture()
    p <- downloader_test__local_plan(fx$root, bytes, "source.txt")
    sid <- fx$dl$enqueue(p)
    orig <- fx$dl$cancel(session_id = sid)
    part <- file.path(fx$temp, paste0(orig$task_id, ".part"))
    writeBin(bytes[1:8], part)

    local({
        append <- downloader__ddb_append_table
        msg <- "interrupted before the task row was restored"
        local_mocked_bindings(downloader__ddb_append_table = function(
            conn,
            table,
            rows,
            ...
        ) {
            if (identical(table, "download_task")) {
                stop(msg, call. = FALSE)
            }
            append(conn, table, rows, ...)
        })
        expect_error(fx$dl$resume(sid), msg)
    })
    priv(fx$dl)$disconnect_manifest()
    dl <- Downloader$new(manifest = fx$manifest, n_workers = 0L)
    withr::defer(priv(dl)$disconnect_manifest())
    r <- dl$tasks(session_id = sid)
    expect_identical(r$task_id, orig$task_id)
    expect_identical(r$status, "cancelled")
    expect_identical(readBin(part, "raw", n = 8L), bytes[1:8])

    c1 <- dl$resume(sid, progress = FALSE)
    expect_identical(c1$task_id, orig$task_id)
    expect_identical(c1$session_id, sid)
    expect_identical(c1$status, "done")
    expect_identical(readBin(c1$target_path, "raw", length(bytes)), bytes)
})

test_that("Downloader$cancel() / Downloader$retry() cancel by session_id and task_id", {
    fx <- downloader_test__fixture()
    dl <- fx$dl
    p <- downloader_test__local_plan(fx$root, "cancel content", "cancel.txt")
    sid <- dl$enqueue(p, session_label = "cancel")

    c1 <- dl$cancel(session_id = sid)
    expect_equal(c1$status, "cancelled")
    expect_match(c1$last_error, "Cancelled by user")
    s <- dl$sessions()
    expect_equal(s$status[s$session_id == sid], "cancelled")

    expect_equal(dl$retry(session_id = sid)$status, "queued")
    expect_equal(dl$run(session_id = sid, progress = FALSE)$status, "done")
    expect_true(file.exists(file.path(fx$dest, "cancel.txt")))
    conn <- priv(dl)$manifest_conn
    expect_true("cancelled" %in% ddb_read_table(conn, "download_event")$event)

    # Cancel an active downloading task by task_id
    p2 <- downloader_test__local_plan(fx$root, "cancel dl", "cancel-dl.txt")
    sid2 <- dl$enqueue(p2, session_label = "cancel-downloading")
    tid <- dl$tasks(session_id = sid2)$task_id[[1L]]
    sql <- sprintf(
        "UPDATE download_task SET status = 'downloading', updated_at = %s WHERE task_id = %s",
        ddb_literal(conn, downloader__now()),
        ddb_literal(conn, tid)
    )
    ddb_exec(conn, sql)
    expect_equal(dl$cancel(task_id = tid)$status, "cancelled")
    expect_equal(dl$status(task_id = tid)$status, "cancelled")
    expect_false(file.exists(file.path(fx$dest, "cancel-dl.txt")))
})
# }}}
# Downloader$run() {{{
test_that("Downloader$run() falls back across candidate URLs", {
    skip_on_cran()
    skip_if_not_installed("webfakes")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    server <- downloader_test__http_server()
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
    expect_equal(
        nodes[
            nodes$data_node == "bad-node.example.test",
            ,
            drop = FALSE
        ]$failure_count,
        1L
    )
    expect_equal(
        nodes[
            nodes$data_node == "good-node.example.test",
            ,
            drop = FALSE
        ]$success_count,
        1L
    )

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    on.exit(ddb_disconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    candidates <- ddb_read_table(
        conn,
        "download_candidate"
    )
    expect_equal(
        candidates[order(candidates$priority), , drop = FALSE]$failed_count,
        c(1L, 0L)
    )
})

test_that("Downloader$run() retries transient HTTP failures", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    srv <- downloader_test__http_server()
    fx <- downloader_test__fixture(retries = 2L)
    p <- downloader_test_df(
        logical_file_id = "tracking:flaky-test",
        filename = "flaky.bin",
        url = srv$url("/files/flaky.bin"),
        checksum = downloader_http_checksum(),
        checksum_type = "md5",
        data_node = "flaky-node.example.test",
        priority = 1L
    )
    sid <- fx$dl$enqueue(p, session_label = "flaky")
    t <- suppressWarnings(fx$dl$run(session_id = sid, progress = FALSE))
    expect_equal(t$status, "done")
    expect_true(fx$dl$verify(session_id = sid)$checksum_ok)
})

test_that("Downloader$run() keeps candidate URLs task-scoped", {
    fx <- downloader_test__fixture()
    txt <- c("first task content", "second task content")
    p <- downloader_test__local_plan(fx$root, txt, c("first.txt", "second.txt"))
    t <- fx$dl$run(session_id = fx$dl$enqueue(p), progress = FALSE)
    t <- t[order(t$filename), , drop = FALSE]

    expect_equal(t$status, c("done", "done"))
    expect_identical(t$selected_url, p$url)
    expect_equal(readLines(file.path(fx$dest, "first.txt")), txt[[1L]])
    expect_equal(readLines(file.path(fx$dest, "second.txt")), txt[[2L]])
})
# }}}
# Downloader node tracking {{{
test_that("Downloader node tracking records, lists, and resets data_nodes", {
    np <- list(
        cooldown_after_failures = 1L,
        cooldown_seconds = 60L,
        history_ttl_seconds = 3600L,
        min_attempts = 1L
    )
    fx <- downloader_test__fixture(node_policy = np)
    hosts <- c("ok.example.org", "fail.example.org")
    plan <- downloader_test_df(
        logical_file_id = c("tracking:probe-ok", "tracking:probe-fail"),
        filename = c("ok.nc", "fail.nc"),
        url = paste0("https://", hosts, "/file.nc"),
        service = "HTTPServer",
        data_node = hosts,
        priority = c(1L, 1L),
        probe_latency = c(0.2, NA_real_),
        probe_throughput = NA_real_
    )
    row_for <- function(df, h) df[df$data_node == h, , drop = FALSE]

    nodes <- fx$dl$record_probes(plan, probed = TRUE)
    expect_equal(row_for(nodes, hosts[[1L]])$probe_success_count, 1L)
    expect_equal(row_for(nodes, hosts[[2L]])$probe_failure_count, 1L)
    expect_false(is.na(row_for(nodes, hosts[[2L]])$cooldown_until))

    cached <- plan[1L, , drop = FALSE]
    cached$probe_cached <- TRUE
    n2 <- fx$dl$record_probes(cached, probed = TRUE)
    expect_equal(row_for(n2, hosts[[1L]])$probe_success_count, 1L)

    listed <- fx$dl$data_nodes()
    expect_true(all(hosts %in% listed$data_node))
    expect_equal(row_for(listed, hosts[[1L]])$probe_success_count, 1L)
    expect_equal(row_for(listed, hosts[[2L]])$probe_failure_count, 1L)

    rem <- fx$dl$reset_data_nodes(data_node = hosts[[2L]])
    expect_false(hosts[[2L]] %in% rem$data_node)
    expect_true(hosts[[1L]] %in% rem$data_node)
})
# }}}
# Downloader callbacks and events {{{
test_that("Downloader$on(), $off(), and $events() dispatch callbacks and log manifest events", {
    fx <- downloader_test__fixture()
    dl <- fx$dl
    seen <- character()
    done_ev <- NULL
    sess_ev <- NULL
    fields <- c(
        "event",
        "session_id",
        "task_id",
        "file_key",
        "status",
        "target_path",
        "selected_url",
        "data_node",
        "bytes_done",
        "error",
        "created_at"
    )
    tok1 <- dl$on("task_done", function(ev, d) {
        seen <<- c(seen, ev$event)
        done_ev <<- ev
        expect_true(all(fields %in% names(ev)))
        expect_equal(c(ev$file_key, ev$status), c("event-file-key", "done"))
        expect_match(ev$selected_url, "^file://")
        expect_true(file.exists(ev$target_path) && is.na(ev$error))
        expect_gt(ev$bytes_done, 0)
        expect_s3_class(d, "Downloader")
    })
    tok2 <- dl$on("session_done", function(ev, d) {
        seen <<- c(seen, ev$event)
        sess_ev <<- ev
        expect_true(all(fields %in% names(ev)) && is.na(ev$task_id))
        expect_equal(ev$status, "done")
    })
    dl$on("task_done", function(ev, d) stop("callback boom", call. = FALSE))

    p <- downloader_test__local_plan(
        fx$root,
        "event content",
        "event.txt",
        file_key = "event-file-key",
        data_node = "local-file"
    )
    sid <- dl$enqueue(p, session_label = "events")
    t <- dl$run(session_id = sid, progress = FALSE)

    expect_equal(t$status, "done")
    expect_true(all(c("session_done", "task_done") %in% seen))
    expect_equal(c(done_ev$session_id, sess_ev$session_id), c(sid, sid))
    expect_equal(done_ev$task_id, t$task_id[[1L]])
    expect_true(dl$off(tok1) && !dl$off(tok1) && dl$off(tok2))

    evs <- dl$events(session_id = sid)$event
    expect_true(all(
        c("enqueue", "start", "done", "session_done", "callback_error") %in% evs
    ))
    tevs <- dl$events(task_id = t$task_id[[1L]])$event
    expect_true(all(c("start", "done", "callback_error") %in% tevs))
})
# }}}
# Downloader$run() worker concurrency {{{
test_that("Downloader$run() uses worker concurrency", {
    fx <- downloader_test__fixture(n_workers = 2L)
    txt <- c("parallel first", "parallel second")
    fns <- c("parallel-first.txt", "parallel-second.txt")
    p <- downloader_test__local_plan(fx$root, txt, fns)
    sid <- fx$dl$enqueue(p, session_label = "parallel")
    t <- fx$dl$run(session_id = sid, progress = FALSE)
    t <- t[order(t$filename), , drop = FALSE]

    expect_equal(t$status, c("done", "done"))
    expect_equal(t$attempts, c(1L, 1L))
    expect_equal(readLines(file.path(fx$dest, fns[[1L]])), txt[[1L]])
    expect_equal(readLines(file.path(fx$dest, fns[[2L]])), txt[[2L]])
    s <- fx$dl$sessions()
    expect_equal(s$status[s$session_id == sid], "done")
})

test_that("Downloader$run() defers tasks beyond per-host capacity", {
    rp <- list(host_concurrency = 1L)
    fx <- downloader_test__fixture(n_workers = 2L, resource_policy = rp)
    txt <- c("host limited first", "host limited second")
    fns <- c("host-first.txt", "host-second.txt")
    p <- downloader_test__local_plan(
        fx$root,
        txt,
        fns,
        data_node = "shared-host.example.org"
    )
    sid <- fx$dl$enqueue(p, session_label = "host-capacity")
    t <- fx$dl$run(session_id = sid, progress = FALSE)
    t <- t[order(t$filename), , drop = FALSE]

    expect_equal(t$status, c("done", "done"))
    expect_equal(t$attempts, c(1L, 1L))
    expect_equal(readLines(file.path(fx$dest, fns[[1L]])), txt[[1L]])
    expect_equal(readLines(file.path(fx$dest, fns[[2L]])), txt[[2L]])
    s <- fx$dl$sessions()
    expect_equal(s$status[s$session_id == sid], "done")
})

test_that("Downloader$run() serializes tasks for the same target path", {
    fx <- downloader_test__fixture(n_workers = 2L)
    src <- file.path(fx$root, "shared-src.txt")
    writeLines("shared target content", src)
    p <- downloader_test_df(
        logical_file_id = c("tracking:shared-first", "tracking:shared-second"),
        filename = c("shared.txt", "shared.txt"),
        url = paste0("file://", normalizePath(src, "/")),
        checksum = as.character(tools::md5sum(src)),
        checksum_type = "md5",
        priority = 1L
    )
    t <- fx$dl$run(session_id = fx$dl$enqueue(p), progress = FALSE)
    expect_setequal(t$status, c("done", "skipped"))
    expect_equal(
        readLines(file.path(fx$dest, "shared.txt")),
        "shared target content"
    )
})
# }}}
# Downloader$download() {{{
test_that("Downloader$download() handles basic, subdir, and overwrite = FALSE", {
    fx <- downloader_test__fixture(manifest = FALSE)
    u1 <- downloader_test_file_url(fx$root, "source-1024.bin", 1024L)
    p1 <- fx$dl$download(u1, filename = "test_file.bin", progress = FALSE)
    expect_true(file.exists(p1))
    expect_equal(basename(p1), "test_file.bin")
    expect_equal(file.info(p1)$size, 1024)

    u2 <- downloader_test_file_url(fx$root, "source-512.bin", 512L)
    p2 <- fx$dl$download(
        u2,
        filename = "data.bin",
        subdir = "docs",
        progress = FALSE
    )
    expect_true(file.exists(p2))
    expect_equal(basename(p2), "data.bin")
    expect_equal(dirname(p2), normalizePath(file.path(fx$dest, "docs"), "/"))

    src <- file.path(fx$root, "source.txt")
    writeLines("test content", src)
    dst <- file.path(fx$dest, "dest.txt")
    file.copy(src, dst)
    mt <- file.info(dst)$mtime
    Sys.sleep(0.1)
    u3 <- paste0("file://", normalizePath(src, "/"))
    p3 <- fx$dl$download(
        u3,
        filename = "dest.txt",
        progress = FALSE,
        overwrite = FALSE
    )
    expect_equal(normalizePath(p3), normalizePath(dst))
    expect_equal(file.info(dst)$mtime, mt)
})

test_that("Downloader$download() restarts unsupported Range resume", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    fx <- downloader_test__fixture(manifest = FALSE)
    dir.create(fx$temp, recursive = TRUE, showWarnings = FALSE)
    srv <- downloader_test__http_server()
    part <- file.path(fx$temp, "range-restart.part")
    writeLines("stale partial", part)

    p <- fx$dl$download(
        srv$url("/files/no-range.bin"),
        filename = "source.bin",
        checksum = downloader_http_checksum(8193L),
        checksum_type = "md5",
        progress = FALSE,
        .tmp_id = "range-restart"
    )
    expect_equal(readBin(p, "raw", n = 8193L), downloader_http_bytes(8193L))
    expect_false(file.exists(part))
})

test_that("Downloader$download() verifies checksum and rejects mismatch", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    fx <- downloader_test__fixture(
        manifest = FALSE,
        retries = 1L,
        timeout = 300
    )
    srv <- downloader_test__http_server()
    ck <- downloader_http_checksum(algo = "sha256")
    u <- srv$url("/files/ok.bin")

    p <- fx$dl$download(
        u,
        filename = "ok.bin",
        checksum = ck,
        checksum_type = "sha256",
        progress = FALSE
    )
    expect_true(file.exists(p))
    expect_true(epwshiftr:::downloader__verify_checksum(p, ck, "sha256"))
    expect_error(
        fx$dl$download(
            u,
            filename = "bad.bin",
            checksum = "wrong",
            checksum_type = "sha256",
            progress = FALSE
        ),
        "Checksum verification failed"
    )
})

test_that("Downloader$download() handles HTTP errors", {
    skip_on_cran()
    skip_if_not_installed("webfakes")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    server <- downloader_test__http_server()

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

test_that("Downloader$download() offline mode blocks new downloads", {
    local_cache_mode("offline")
    dl <- Downloader$new(
        dest = withr::local_tempdir("downloader-offline-"),
        n_workers = 0L
    )

    expect_error(
        dl$download(url = "https://example.com/nonexistent.nc"),
        "offline"
    )
})

test_that("Downloader$download() offline mode allows verified files", {
    local_cache_mode("offline")

    dest <- withr::local_tempdir("downloader-offline-")
    test_file <- file.path(dest, "test-offline-verified.txt")
    writeLines("test content", test_file)

    dl <- Downloader$new(dest = dest, n_workers = 0L)

    result <- dl$download(
        url = "https://example.com/test-offline-verified.txt",
        filename = "test-offline-verified.txt"
    )
    expect_equal(normalizePath(result), normalizePath(test_file))
})
# }}}
# Downloader async task access {{{
test_that("async task access handles empty lists and rejects unknown IDs", {
    dl <- Downloader$new(n_workers = 0L)
    expect_equal(dl$wait_for_tasks(progress = FALSE), list())
    expect_error(
        dl$get_task_status("missing-task"),
        "Task ID 'missing-task' not found"
    )
    expect_error(
        dl$cancel_task("missing-task"),
        "Task ID 'missing-task' not found"
    )
})
# }}}
# Downloader$list_incomplete() / $cleanup_tmp() / $verify_checksum() / $verify() {{{
test_that("Downloader$list_incomplete(), $cleanup_tmp(), and $verify_checksum()", {
    fx <- downloader_test__fixture(manifest = FALSE)
    u <- downloader_test_file_url(fx$root, "source-2048.bin", 2048L)
    p <- fx$dl$download(u, filename = "test.bin", progress = FALSE)
    expect_true(file.exists(p))
    expect_equal(nrow(fx$dl$list_incomplete()), 0L)

    dir.create(fx$temp, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(fx$temp, c("test1.part", "test2.done")))
    expect_gte(fx$dl$cleanup_tmp(all = TRUE), 0L)

    f <- file.path(fx$root, "checksum.txt")
    writeLines("test content", f)
    md5 <- as.character(tools::md5sum(f))
    expect_true(fx$dl$verify_checksum(f, md5, "md5"))
    expect_false(fx$dl$verify_checksum(f, "wrongchecksum", "md5"))
})

test_that("Downloader$verify()", {
    fx <- downloader_test__fixture()
    p <- downloader_test__local_plan(fx$root, "verified content", "verify.txt")
    sid <- fx$dl$enqueue(p, session_label = "verify-error")
    expect_equal(fx$dl$run(session_id = sid, progress = FALSE)$status, "done")

    writeLines("corrupted content", file.path(fx$dest, "verify.txt"))
    expect_false(fx$dl$verify(session_id = sid)$checksum_ok)
    st <- fx$dl$status(session_id = sid)
    expect_equal(st$status, "error")
    expect_match(st$last_error, "Checksum verification failed")
    evs <- ddb_read_table(priv(fx$dl)$manifest_conn, "download_event")$event
    expect_true("verify_error" %in% evs)
})
# }}}
# Downloader$print() {{{
test_that("Downloader$print()", {
    dl <- Downloader$new()

    expect_snapshot(
        print(dl),
        transform = function(lines) {
            unlist(
                lapply(lines, function(line) {
                    inline_path <- regexec(
                        "^(\\s*\\* (Data|Temporary) directory:)\\s+/.+",
                        line
                    )
                    match <- regmatches(line, inline_path)[[1]]
                    if (length(match)) {
                        return(c(match[[2]], "<path>"))
                    }

                    gsub("^\\s*/.+", "<path>", line)
                }),
                use.names = FALSE
            )
        }
    )
})
# }}}

# Reuse real download inputs while exercising independent instance lifetimes.
# Destroying the first downloader must not stop the second one's queued work.
test_that("Downloader owns its worker pool without interrupting other instances", {
    root <- tempfile("downloader-pools-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    source <- file.path(root, "source.bin")
    writeBin(downloader_http_bytes(), source)
    checksum <- unname(tools::md5sum(source))
    prefix <- if (.Platform$OS.type == "windows") "file:///" else "file://"
    url <- paste0(prefix, normalizePath(source, winslash = "/"))
    first <- Downloader$new(dest = file.path(root, "first"), n_workers = 1L)
    second <- Downloader$new(dest = file.path(root, "second"), n_workers = 1L)
    on.exit(first$.__enclos_env__$private$finalize(), add = TRUE)
    on.exit(second$.__enclos_env__$private$finalize(), add = TRUE)
    one <- first$download(url, block = FALSE)
    expect_named(first$get_tasks(), one)
    expect_s3_class(first$get_tasks()[[one]], "DownloadTask")
    result <- first$wait_for_tasks(one, progress = FALSE)[[one]]
    expect_identical(result$task_id, one)
    expect_identical(result$url, url)
    expect_identical(result$filename, "source.bin")
    expect_identical(result$status, DownloadStatus$Completed)
    expect_null(result$error)
    expect_s3_class(result$completed_at, "POSIXct")
    two <- second$download(
        url,
        checksum = checksum,
        checksum_type = "md5",
        block = FALSE
    )
    expect_false(identical(
        first$.__enclos_env__$private$compute_profile,
        second$.__enclos_env__$private$compute_profile
    ))
    first$.__enclos_env__$private$finalize()
    second$wait_for_tasks(two, progress = FALSE)
    expect_identical(
        second$get_task_status(two)$status,
        DownloadStatus$Completed
    )
    expect_identical(
        unname(tools::md5sum(file.path(root, "second", "source.bin"))),
        checksum
    )
})

# A missing local source fails inside a real worker and must not be reported as
# a completed download merely because mirai returned an atomic error object.
test_that("Downloader reports asynchronous worker failures", {
    root <- tempfile("downloader-worker-error-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dl <- Downloader$new(dest = root, retries = 1L, n_workers = 1L)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    prefix <- if (.Platform$OS.type == "windows") "file:///" else "file://"
    url <- paste0(prefix, normalizePath(root, winslash = "/"), "/absent.bin")
    id <- dl$download(url, block = FALSE)
    result <- dl$wait_for_tasks(id, progress = FALSE)[[id]]
    expect_identical(result$status, DownloadStatus$Failed)
    expect_true(nzchar(result$error))
    expect_false(file.exists(file.path(root, "absent.bin")))
})

# Exercise actual GET semantics through the public downloader. The fixture
# records Range requests and returns only tiny tails for offsets above 2 GB.
test_that("ordinary HTTP resume validates GETs and retries at the written offset", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- webfakes::new_app()
    app$locals$bytes <- as.raw(rep(0:255, 32))
    app$locals$start <- numeric()
    app$get("/offsets", function(req, res, locals) res$send_json(locals$start))
    app$head("/:mode", function(req, res, locals) {
        start <- as.numeric(gsub("[^0-9]", "", req$get_header("Range")))
        total <- if (start > 2^31) start + 4 else length(locals$bytes)
        res$set_status(206L)$set_header(
            "Content-Range",
            sprintf("bytes %.0f-%.0f/%.0f", start, total - 1, total)
        )$send(raw())
    })
    app$get("/:mode", function(req, res, locals) {
        start <- as.numeric(gsub("[^0-9]", "", req$get_header("Range")))
        mode <- req$params$mode
        if (mode == "ignore") {
            return(res$send(locals$bytes))
        }
        locals$start <- c(locals$start, start)
        total <- if (mode == "large") start + 4 else length(locals$bytes)
        reported <- if (mode == "wrong") start + 1 else start
        res$set_status(206L)$set_header(
            "Content-Range",
            sprintf("bytes %.0f-%.0f/%.0f", reported, total - 1, total)
        )
        if (mode == "large") {
            return(res$send(as.raw(1:4)))
        }
        if (mode == "retry" && length(locals$start) == 1L) {
            # Send a cleanly terminated but short chunked tail. The guard
            # must keep its valid bytes and reject finalization.
            res$send_chunk(locals$bytes[(start + 1):2048])
            return(invisible(NULL))
        }
        res$send(locals$bytes[(start + 1):total])
    })
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    temp <- file.path(root, "temp")
    downloader <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = temp,
        retries = 2L,
        n_workers = 0L
    )
    prefix <- app$locals$bytes[1:1024]
    # Retry is the first request so only this attempt hits the truncation branch.
    writeBin(prefix, file.path(temp, "retry.part"))
    expect_warning(
        result <- downloader$download(
            server$url("/retry"),
            filename = "retry.bin",
            progress = FALSE,
            checksum = checksum_bytes(app$locals$bytes, "md5"),
            checksum_type = "md5",
            .tmp_id = "retry"
        ),
        "Waiting.*before retry"
    )
    expect_identical(readBin(result, "raw", n = 8192L), app$locals$bytes)
    offsets <- curl::curl_fetch_memory(server$url("/offsets"))$content
    expect_equal(jsonlite::fromJSON(rawToChar(offsets)), c(1024, 2048))
    for (mode in c("ignore", "wrong")) {
        partial <- file.path(temp, paste0(mode, ".part"))
        writeBin(prefix, partial)
        expect_error(suppressWarnings(downloader$download(
            server$url(paste0("/", mode)),
            filename = paste0(mode, ".bin"),
            progress = FALSE,
            .tmp_id = mode
        )))
        expect_identical(readBin(partial, "raw", n = 8192L), prefix)
    }
    # Check the real server response at a large offset without allocating a
    # multi-gigabyte file in the test filesystem.
    start <- 2^31 + 123
    expect_true(downloader__resume_supported(server$url("/large"), start, 5L))
    handle <- curl::new_handle(resume_from_large = start)
    guard <- downloader__worker_dependencies()$resume_guard(handle, start)
    body <- raw()
    response <- curl::curl_fetch_stream(
        server$url("/large"),
        function(chunk) {
            guard$accept(chunk)
            body <<- chunk
            TRUE
        },
        handle = handle
    )
    expect_no_error(guard$finish(response))
    expect_identical(body, as.raw(1:4))
})

# Simulate owner-record write failures without filling a real disk or touching
# a production manifest. Download payloads remain outside the generated lock.
test_that('manifest acquisition refuses an unpublished owner record', {
    root <- withr::local_tempdir()
    manifest <- file.path(root, 'manifest.duckdb')
    payload <- file.path(root, 'retained.part')
    writeBin(charToRaw('retained-generated-partial'), payload)
    before <- readBin(payload, 'raw', n = file.info(payload)$size)
    testthat::local_mocked_bindings(
        write_json = function(...) stop('generated ownership write failure'),
        .package = 'jsonlite'
    )
    expect_error(
        downloader__manifest_acquire_lock(manifest, timeout = 0),
        'Cannot publish manifest lock ownership'
    )
    expect_false(dir.exists(paste0(manifest, '.lock')))
    expect_identical(
        readBin(payload, 'raw', n = file.info(payload)$size),
        before
    )
})

# A write can return normally while leaving truncated bytes. Readback must be
# checked before the acquisition is returned to a Downloader or Store caller.
test_that('manifest acquisition verifies its own owner publication', {
    root <- withr::local_tempdir()
    manifest <- file.path(root, 'manifest.duckdb')
    testthat::local_mocked_bindings(
        write_json = function(x, path, ...) writeLines('{"pid":', path),
        .package = 'jsonlite'
    )
    expect_error(
        downloader__manifest_acquire_lock(manifest, timeout = 0),
        'Cannot publish manifest lock ownership'
    )
    expect_false(dir.exists(paste0(manifest, '.lock')))
})

# Failure cleanup is limited to the just-created ownership record. Unknown
# entries are preserved for investigation instead of being recursively erased.
test_that('failed publication never erases an unexpected lock entry', {
    root <- withr::local_tempdir()
    manifest <- file.path(root, 'manifest.duckdb')
    testthat::local_mocked_bindings(
        write_json = function(x, path, ...) {
            writeLines(
                'preserve-generated-evidence',
                file.path(dirname(path), 'unexpected.txt')
            )
            stop('generated write failure with extra evidence')
        },
        .package = 'jsonlite'
    )
    expect_error(
        downloader__manifest_acquire_lock(manifest, timeout = 0),
        'Cannot publish manifest lock ownership'
    )
    expect_identical(
        readLines(file.path(paste0(manifest, '.lock'), 'unexpected.txt')),
        'preserve-generated-evidence'
    )
})

# Ordinary acquisition still publishes the original token and releases only its
# exact record; neither the public API nor the stale-lock takeover policy changes.
test_that('successful owner publication keeps the original release contract', {
    root <- withr::local_tempdir()
    manifest <- file.path(root, 'manifest.duckdb')
    release <- downloader__manifest_acquire_lock(manifest, timeout = 0)
    owner <- downloader__manifest_lock_owner(paste0(manifest, '.lock'))
    expect_identical(owner$pid, Sys.getpid())
    expect_identical(owner$hostname, unname(Sys.info()[['nodename']]))
    expect_true(nzchar(owner$token))
    release()
    expect_false(dir.exists(paste0(manifest, '.lock')))
})

# Recreate the on-disk evidence left by a process that exited without releasing
# its lock. A real short-lived R child provides a PID known to have exited.
lock_recovery_test__owner_cache <- new.env(parent = emptyenv())
lock_recovery_test__owner_cache$owner <- NULL

lock_recovery_test__owner <- function(directory) {
    owner <- lock_recovery_test__owner_cache$owner
    if (
        is.null(owner) ||
            !isTRUE(downloader__local_pid_absent(owner$pid, owner$hostname))
    ) {
        out <- system2(
            file.path(R.home("bin"), "Rscript"),
            c("--vanilla", "-e", shQuote("cat(Sys.getpid())")),
            stdout = TRUE,
            stderr = FALSE
        )
        pid <- as.integer(trimws(paste(out, collapse = "")))
        stopifnot(is.null(attr(out, "status")), length(pid) == 1L, !is.na(pid))
        owner <- list(
            pid = pid,
            hostname = unname(Sys.info()[["nodename"]])
        )
        lock_recovery_test__owner_cache$owner <- owner
    }
    owner_path <- file.path(directory, "owner.json")
    jsonlite::write_json(owner, owner_path, auto_unbox = TRUE)
    jsonlite::read_json(owner_path)
}

test_that("manifest acquisition recovers an exited local owner immediately", {
    root <- withr::local_tempdir()
    path <- file.path(root, "manifest")
    lock <- paste0(path, ".lock")
    dir.create(lock)
    lock_recovery_test__owner(lock)

    release <- manifest_acquire_lock(path, timeout = 0)
    current <- jsonlite::read_json(file.path(lock, "owner.json"))
    expect_identical(current$pid, Sys.getpid())
    expect_length(list.files(root, "orphan"), 1L)
    release()
    expect_false(dir.exists(lock))
})

test_that("manifest acquisition respects old locks with live or unknown owners", {
    root <- withr::local_tempdir()
    path <- file.path(root, "manifest")
    lock <- paste0(path, ".lock")
    owner_path <- file.path(lock, "owner.json")

    release <- manifest_acquire_lock(path, timeout = 0)
    Sys.setFileTime(lock, Sys.time() - 2 * 86400)
    expect_error(manifest_acquire_lock(path, timeout = 0), "locked")
    release()
    expect_false(dir.exists(lock))

    dir.create(lock)
    Sys.setFileTime(lock, Sys.time() - 2 * 86400)
    expect_error(manifest_acquire_lock(path, timeout = 0), "locked")
    writeLines("{", owner_path)
    Sys.setFileTime(lock, Sys.time() - 2 * 86400)
    expect_error(manifest_acquire_lock(path, timeout = 0), "locked")

    # A fractional PID must never be coerced to the exited process's PID,
    # even when the difference falls within checkmate's default tolerance.
    owner <- lock_recovery_test__owner(lock)
    fractional <- owner
    fractional$pid <- fractional$pid + 1e-8
    jsonlite::write_json(fractional, owner_path, auto_unbox = TRUE, digits = NA)
    expect_error(manifest_acquire_lock(path, timeout = 0), "locked")

    # An exited local PID also cannot authorize taking over a foreign lock.
    foreign <- owner
    foreign$hostname <- paste0(foreign$hostname, "-foreign")
    jsonlite::write_json(foreign, owner_path, auto_unbox = TRUE)
    expect_error(manifest_acquire_lock(path, timeout = 0), "locked")
    expect_identical(jsonlite::read_json(owner_path), foreign)
    expect_length(list.files(root, "orphan"), 0L)
})

test_that('a release closure never removes a newer lock owner', {
    root <- withr::local_tempdir()
    path <- file.path(root, 'manifest')
    release <- manifest_acquire_lock(path, timeout = 0)
    old <- paste0(path, '.lock')
    expect_true(file.rename(old, file.path(root, 'preserved-old')))
    newer <- manifest_acquire_lock(path, timeout = 0)
    release()
    expect_true(dir.exists(old))
    newer()
    expect_false(dir.exists(old))
})

# Concurrent waiters use the current helper definitions in fresh R processes,
# rather than accidentally testing a globally installed older package.
test_that('simultaneous orphan recovery preserves mutual exclusion', {
    root <- withr::local_tempdir()
    path <- file.path(root, 'manifest')
    dir.create(paste0(path, '.lock'))
    lock_recovery_test__owner(paste0(path, '.lock'))
    names <- c('path', 'metadata', 'owner', 'stale')
    names <- c(
        paste0('downloader__manifest_lock_', names),
        'downloader__manifest_reclaim_lock',
        'downloader__manifest_acquire_lock',
        'downloader__owner_identity_valid',
        'downloader__local_pid_absent'
    )
    env <- new.env(parent = baseenv())
    for (name in names) {
        fun <- get(name)
        environment(fun) <- env
        assign(name, fun, env)
    }
    functions <- file.path(root, 'functions.rds')
    saveRDS(env, functions)
    script <- file.path(root, "waiter.R")
    writeLines(
        c(
            "args <- commandArgs(trailingOnly = TRUE)",
            "env <- readRDS(args[[1L]])",
            "root <- dirname(args[[1L]])",
            "file.create(args[[2L]])",
            "deadline <- Sys.time() + 15",
            "while (!file.exists(file.path(root, 'start')) && Sys.time() < deadline) Sys.sleep(0.02)",
            "stopifnot(file.exists(file.path(root, 'start')))",
            "release <- env$downloader__manifest_acquire_lock(file.path(root, 'manifest'), timeout = 15)",
            "inside <- file.path(root, 'inside')",
            "stopifnot(dir.create(inside, showWarnings = FALSE))",
            "Sys.sleep(0.1)",
            "unlink(inside, recursive = TRUE)",
            "release()"
        ),
        script
    )
    ready <- file.path(root, paste0("ready-", seq_len(3L)))
    test_env <- environment()
    children <- lapply(ready, function(marker) {
        child <- processx::process$new(
            file.path(R.home("bin"), "Rscript"),
            c("--vanilla", script, functions, marker),
            stdout = paste0(marker, ".log"),
            stderr = "2>&1",
            cleanup_tree = TRUE
        )
        withr::defer(if (child$is_alive()) child$kill_tree(), envir = test_env)
        child
    })

    # Release the waiters together after every process has started.
    deadline <- Sys.time() + 10
    while (!all(file.exists(ready)) && Sys.time() < deadline) {
        Sys.sleep(0.02)
    }
    expect_true(all(file.exists(ready)))
    file.create(file.path(root, "start"))
    for (child in children) {
        child$wait(timeout = 20000)
        expect_identical(child$get_exit_status(), 0L)
    }
    expect_length(list.files(root, "orphan"), 1L)
    expect_false(dir.exists(paste0(path, ".lock")))
})

# The copyable module's pool helper must run without the package namespace.
test_that("standalone worker startup has no host-package dependencies", {
    skip_on_cran()
    start <- downloader__start_pool
    environment(start) <- baseenv()
    profile <- paste0("standalone-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    start(1L, .compute = profile)
    job <- mirai::mirai(42L, .compute = profile)
    expect_identical(mirai::collect_mirai(job), 42L)
})

# Live ESGF downloader workflow {{{
# live_esgf_files {{{
live_esgf_files <- function() {
    q <- esg_query(INDEX_NODES[["DKRZ"]])$source_id(
        "CMCC-CM2-SR5"
    )$experiment_id("dcppA-hindcast")$variable_id("sftgif")$frequency(
        "fx"
    )$nominal_resolution("100 km")$variant_label(c(
        "r32i1p1f1",
        "r34i1p1f1"
    ))$params(institution_id = "CMCC", sub_experiment_id = "s2017")$limit(2L)

    datasets <- allow_live_esgf_dict_warnings(q$collect())
    files <- datasets$collect(type = "File", limit = 2L)
    if (files$count() < 2L) {
        skip("Live ESGF query returned fewer than two file records.")
    }

    plan <- files$download_plan(
        replica = "current",
        probe = TRUE,
        strategy = "fastest"
    )
    plan <- plan[!is.na(url) & nzchar(url)]
    if (nrow(plan) < 2L) {
        skip("Live ESGF query returned fewer than two HTTPServer URLs.")
    }
    if (any(plan$size[seq_len(2L)] > 1024^2)) {
        skip(
            "Live ESGF query returned files larger than the live downloader budget."
        )
    }
    checksums <- plan$checksum[seq_len(2L)]
    if (any(is.na(checksums) | !nzchar(checksums))) {
        skip("Live ESGF query returned files without checksums.")
    }

    list(files = files, plan = plan[seq_len(2L)])
}
# }}}

# write_live_partial {{{
write_live_partial <- function(url, file, bytes = 1024L) {
    handle <- curl::new_handle()
    curl::handle_setopt(handle, followlocation = TRUE, timeout = 60)
    curl::handle_setheaders(handle, Range = sprintf("bytes=0-%d", bytes - 1L))
    curl::curl_fetch_disk(url, file, handle = handle)
    invisible(file)
}
# }}}

# capture_live_warnings {{{
capture_live_warnings <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(
        force(expr),
        warning = function(w) {
            message <- conditionMessage(w)
            if (is_live_esgf_transient_warning(message)) {
                warnings <<- c(warnings, message)
                invokeRestart("muffleWarning")
            }
        }
    )
    list(value = value, warnings = warnings)
}
# }}}

# skip_live_download_error {{{
skip_live_download_error <- function(expr) {
    tryCatch(
        allow_live_esgf_transient_warnings(force(expr)),
        error = function(e) {
            msg <- conditionMessage(e)
            pattern <- paste(
                c(
                    "Failed to download file after",
                    "cannot open the connection",
                    "Empty reply from server",
                    "Timeout was reached",
                    "Could not resolve host",
                    "SSL connect error",
                    "Recv failure"
                ),
                collapse = "|"
            )
            if (grepl(pattern, msg)) {
                skip(sprintf("Live ESGF download endpoint failed: %s", msg))
            }
            stop(e)
        }
    )
}
# }}}

test_that("Downloader$enqueue() / Downloader$run() / Downloader$verify() live ESGF workflow covers persistent methods and resume", {
    skip_live_esgf()

    live <- tryCatch(live_esgf_files(), error = function(e) {
        skip(conditionMessage(e))
    })
    files <- live$files
    plan <- live$plan
    bad_url <- "http://127.0.0.1:1/epwshiftr-live-missing.nc"

    root <- tempfile("live-esgf-downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 2L,
        timeout = 240L,
        n_workers = 0L
    )

    expect_s3_class(dl, "Downloader")
    expect_s3_class(dl$clone(deep = FALSE), "Downloader")
    expect_s3_class(print(dl), "Downloader")
    expect_identical(dl$get_tasks(), list())
    expect_identical(dl$wait_for_tasks(), list())
    expect_error(dl$get_task_status("missing-task"), "not found")
    expect_error(dl$cancel_task("missing-task"), "not found")

    expect_true(file.exists(dl$manifest))

    shortcut_dir <- file.path(root, "shortcut")
    shortcut <- Downloader$new(
        dest = shortcut_dir,
        temp = file.path(root, "shortcut-tmp"),
        retries = 2L,
        timeout = 240L,
        n_workers = 0L
    )
    shortcut_path <- skip_live_download_error(shortcut$download(
        url = plan$url[[1L]],
        filename = plan$filename[[1L]],
        checksum = plan$checksum[[1L]],
        checksum_type = plan$checksum_type[[1L]],
        progress = FALSE,
        block = FALSE
    ))
    expect_true(file.exists(shortcut_path))
    expect_true(shortcut$verify_checksum(
        shortcut_path,
        plan$checksum[[1L]],
        tolower(plan$checksum_type[[1L]])
    ))
    file.create(file.path(shortcut$tmp_dir, "manual.part"))
    expect_equal(nrow(shortcut$list_incomplete()), 1L)
    expect_equal(shortcut$cleanup_tmp(all = TRUE), 1L)

    resume_plan <- data.table::copy(plan[1L])
    resume_plan$logical_file_id <- paste0(
        resume_plan$logical_file_id,
        ":resume"
    )
    resume_session <- dl$enqueue(resume_plan, session_label = "live-resume")
    resume_task <- dl$tasks(session_id = resume_session)
    partial_path <- file.path(
        dl$tmp_dir,
        paste0(resume_task$task_id[[1L]], ".part")
    )
    skip_live_download_error(write_live_partial(
        resume_plan$url[[1L]],
        partial_path
    ))
    expect_true(file.exists(partial_path))
    expect_gt(file.info(partial_path)$size, 0)
    expect_equal(nrow(dl$list_incomplete()), 1L)

    resumed <- skip_live_download_error(dl$resume(
        session_id = resume_session,
        progress = FALSE
    ))
    expect_equal(resumed$status, "done")
    expect_true(file.exists(resumed$target_path[[1L]]))
    expect_true(dl$verify(session_id = resume_session)$checksum_ok)
    expect_true(dl$verify(task_id = resume_task$task_id[[1L]])$checksum_ok)
    expect_equal(dl$status(task_id = resume_task$task_id[[1L]])$status, "done")

    fallback_plan <- data.table::copy(plan[2L])
    fallback_plan <- data.table::rbindlist(
        list(
            data.table::copy(fallback_plan)[, `:=`(
                url = bad_url,
                priority = 1L
            )],
            data.table::copy(fallback_plan)[, priority := 2L]
        ),
        use.names = TRUE,
        fill = TRUE
    )
    fallback_session <- dl$enqueue(
        fallback_plan,
        session_label = "live-fallback"
    )
    fallback_run <- capture_live_warnings(dl$run(
        session_id = fallback_session,
        progress = FALSE
    ))
    expect_true(any(grepl(
        "Failed to open|Download failed",
        fallback_run$warnings
    )))
    fallback_tasks <- fallback_run$value
    expect_equal(fallback_tasks$status, "done")
    expect_identical(fallback_tasks$selected_url, plan$url[[2L]])

    error_plan <- data.table::copy(plan[1L])
    error_plan$logical_file_id <- paste0(error_plan$logical_file_id, ":error")
    error_plan$filename <- paste0("error-", error_plan$filename)
    error_plan$url <- bad_url
    error_plan$checksum <- NA_character_
    error_plan$checksum_type <- "sha256"
    error_session <- dl$enqueue(error_plan, session_label = "live-error")
    error_run <- capture_live_warnings(dl$run(
        session_id = error_session,
        progress = FALSE
    ))
    expect_true(any(grepl(
        "Failed to open|Download failed",
        error_run$warnings
    )))
    error_tasks <- error_run$value
    expect_equal(error_tasks$status, "error")
    expect_equal(dl$status(session_id = error_session)$status, "error")
    retried <- dl$retry(task_id = error_tasks$task_id[[1L]])
    expect_equal(retried$status, "queued")
    retry_run <- capture_live_warnings(dl$run(
        task_id = error_tasks$task_id[[1L]],
        progress = FALSE
    ))
    expect_true(any(grepl(
        "Failed to open|Download failed",
        retry_run$warnings
    )))
    expect_equal(retry_run$value$status, "error")

    sessions <- dl$sessions()
    expect_true(all(
        c(resume_session, fallback_session, error_session) %in%
            sessions$session_id
    ))
    expect_gte(nrow(dl$tasks()), 3L)

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    candidates <- data.table::as.data.table(ddb_read_table(
        conn,
        "download_candidate"
    ))
    fallback_tid <- fallback_tasks$task_id[[1L]]
    failed_candidate <- candidates[
        candidates[["task_id"]] == fallback_tid
    ][order(priority)][1L]
    expect_equal(failed_candidate$failed_count, 1L)
    ddb_disconnect(conn, shutdown = TRUE)

    restored <- Downloader$new(manifest = manifest)
    expect_equal(
        restored$manifest,
        normalizePath(manifest, mustWork = FALSE, winslash = "/")
    )
    expect_true(all(
        c(resume_session, fallback_session, error_session) %in%
            restored$sessions()$session_id
    ))
    expect_true(restored$verify(session_id = resume_session)$checksum_ok)

    store_dir <- file.path(root, "store")
    store <- EsgStore$new(store_dir)
    on.exit(store$close(), add = TRUE)
    store_dl <- store$downloader(n_workers = 0L, retries = 2L, timeout = 240L)
    store_session <- skip_live_download_error(store$download_files(
        files = files,
        replica = "current",
        downloader = store_dl,
        run = TRUE,
        session_label = "live-store-download",
        probe = TRUE,
        progress = FALSE
    ))
    store_tasks <- store_dl$tasks(session_id = store_session)
    expect_equal(sort(store_tasks$status), c("done", "done"))
    expect_true(all(file.exists(store_tasks$target_path)))
    expect_true(all(store_dl$verify(session_id = store_session)$checksum_ok))

    catalog <- store$query(
        "SELECT filename, local_path, local_artifact_id FROM file_catalog"
    )
    expect_equal(nrow(catalog), 2L)
    expect_false(any(is.na(catalog$local_path)))
    expect_false(any(is.na(catalog$local_artifact_id)))

    for (path in store_tasks$target_path) {
        ds <- EsgDataset$new(path)
        ds$open()
        on.exit(ds$close(), add = TRUE)
        expect_true(length(ds$get_variables()) > 0L)
        expect_true(length(ds$get_dimensions()) > 0L)
    }
})
# }}}

# vim: fdm=marker :
