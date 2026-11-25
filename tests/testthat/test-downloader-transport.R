# Actual GET evidence must survive process lifetimes and retain its original case.
test_that("Downloader binds resumable bytes to actual GET evidence", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_transport__app()
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "actual-get",
        filename = "data.bin",
        url = server$url("/strong")
    ))
    first <- dl$run(session, progress = FALSE)
    expect_identical(first$status, "error")
    evidence <- dl$tasks(session)
    expect_identical(evidence$transfer_etag, '"Actual-GET-A"')
    expect_equal(evidence$transfer_bytes, 1024)
    expect_identical(evidence$transfer_validator_kind, "etag")
    expect_false(evidence$transfer_checksum_verified)
    dl$.__enclos_env__$private$finalize()
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    complete <- dl$resume(session, progress = FALSE)
    expect_identical(complete$status, "done")
    expect_identical(
        readBin(complete$target_path, "raw", 8192),
        app$locals$bytes
    )
    evidence <- dl$tasks(session)
    expect_true(evidence$transfer_complete)
    expect_equal(evidence$transfer_bytes, 8192)
    expect_identical(
        evidence$transfer_sha256,
        unname(tools::sha256sum(complete$target_path))
    )
    request <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_identical(request$range, c("", "bytes=1024-"))
    expect_identical(request$if_range, c("", '"Actual-GET-A"'))
})

# A fixed publisher checksum keeps the original input identity even when a
# server answers an If-Range request with a changed complete representation.
test_that("Downloader reports changed fixed content without replacing retained bytes", {
    skip_on_cran()
    app <- downloader_transport__app("changed")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    temp <- file.path(root, "temp")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = temp,
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    session <- dl$enqueue(data.frame(
        logical_file_id = "fixed-source",
        filename = "data.bin",
        checksum = checksum_bytes(app$locals$bytes, "md5"),
        checksum_type = "md5",
        url = server$url("/changed")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "error")
    expect_match(result$last_error, "source_changed")
    retained <- list.files(temp, "[.]part[.]retained-", full.names = TRUE)
    expect_length(retained, 1L)
    expect_identical(readBin(retained, "raw", 1024), app$locals$bytes[1:1024])
    expect_false(file.exists(result$target_path))
})

# A 416 is useful only together with independent complete-file evidence; its
# response still has to be recorded as the exact request used by this task.
test_that("Downloader accepts 416 only with verified complete-file evidence", {
    skip_on_cran()
    server <- webfakes::new_app_process(downloader_transport__app("complete"))
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    bytes <- as.raw(rep(0:255, 32))
    session <- dl$enqueue(data.frame(
        logical_file_id = "complete-source",
        filename = "data.bin",
        checksum = checksum_bytes(bytes, "md5"),
        checksum_type = "md5",
        url = server$url("/complete")
    ))
    task <- dl$tasks(session)
    writeBin(bytes, file.path(dl$tmp_dir, paste0(task$task_id, ".part")))
    result <- dl$run(session, progress = FALSE)
    expect_identical(result$status, "done")
    evidence <- dl$tasks(session)
    expect_true(evidence$transfer_checksum_verified)
    expect_equal(evidence$transfer_size, 8192)
    expect_identical(evidence$transfer_request_url, server$url("/complete"))
    # Matching byte count alone is insufficient when content is wrong.
    bad <- dl$enqueue(data.frame(
        logical_file_id = "bad-complete",
        filename = "bad.bin",
        checksum = checksum_bytes(bytes, "md5"),
        checksum_type = "md5",
        url = server$url("/complete")
    ))
    bad_task <- dl$tasks(bad)
    partial <- file.path(dl$tmp_dir, paste0(bad_task$task_id, ".part"))
    writeBin(rev(bytes), partial)
    expect_identical(dl$run(bad, progress = FALSE)$status, "error")
    expect_identical(readBin(partial, "raw", 8192), rev(bytes))
})

# Weak validators and recent modification dates do not establish a fixed HTTP
# representation; qualifying Last-Modified dates do permit the retained prefix.
test_that("Downloader distinguishes strong validators and preserves unsafe partials", {
    skip_on_cran()
    for (mode in c("weak", "missing", "lm-strong", "lm-recent")) {
        app <- downloader_transport__app(mode)
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = file.path(root, "manifest.duckdb"),
            retries = 1L,
            n_workers = 0L
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = mode,
            filename = "data.bin",
            url = server$url(paste0("/", mode))
        ))
        expect_identical(dl$run(session, progress = FALSE)$status, "error")
        evidence <- dl$tasks(session)
        expect_identical(
            evidence$transfer_validator_kind,
            if (mode == "lm-strong") "last-modified" else NA_character_
        )
        result <- dl$resume(session, progress = FALSE)
        expect_identical(result$status, "done")
        expect_identical(
            readBin(result$target_path, "raw", 8192),
            app$locals$bytes
        )
        requests <- jsonlite::fromJSON(rawToChar(
            curl::curl_fetch_memory(server$url("/requests"))$content
        ))
        expect_identical(
            requests$range[[2L]],
            if (mode == "lm-strong") "bytes=1024-" else ""
        )
        retained <- list.files(
            dl$tmp_dir,
            "[.]part[.]retained-",
            full.names = TRUE
        )
        expect_length(retained, if (mode == "lm-strong") 0L else 1L)
        if (length(retained)) {
            expect_identical(
                readBin(retained, "raw", 1024),
                app$locals$bytes[1:1024]
            )
        }
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# Invalid resumed headers cannot write even one byte over the saved checkpoint.
test_that("Downloader protects saved bytes from inconsistent resumed responses", {
    skip_on_cran()
    for (mode in c(
        "offset",
        "total",
        "encoding",
        "long",
        "validator",
        "range416"
    )) {
        app <- downloader_transport__app(mode)
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = file.path(root, "manifest.duckdb"),
            retries = 1L,
            n_workers = 0L
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = mode,
            filename = "data.bin",
            url = server$url(paste0("/", mode))
        ))
        expect_identical(dl$run(session, progress = FALSE)$status, "error")
        original <- dl$tasks(session)
        result <- dl$resume(session, progress = FALSE)
        expect_identical(result$status, "error")
        expect_match(
            result$last_error,
            if (mode %in% c("total", "validator")) "source_changed" else "HTTP"
        )
        partial <- file.path(dl$tmp_dir, paste0(original$task_id, ".part"))
        expect_identical(
            readBin(partial, "raw", 8192),
            app$locals$bytes[1:1024]
        )
        expect_identical(
            dl$tasks(session)$transfer_sha256,
            original$transfer_sha256
        )
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# A server may ignore Range while retaining the original validator. Its full
# body replaces a preserved prefix and must never be appended to that prefix.
test_that("Downloader handles validated 200 replacements from byte zero", {
    skip_on_cran()
    app <- downloader_transport__app("ignore")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    session <- dl$enqueue(data.frame(
        logical_file_id = "ignore",
        filename = "data.bin",
        url = server$url("/ignore")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done")
    expect_identical(readBin(result$target_path, "raw", 8192), app$locals$bytes)
    retained <- list.files(dl$tmp_dir, "[.]part[.]retained-", full.names = TRUE)
    expect_length(retained, 1L)
    expect_identical(readBin(retained, "raw", 1024), app$locals$bytes[1:1024])
})

# Real mirai workers must return the same bytes and persisted GET receipt as
# the ordinary transfer path, including an interrupted attempt and resumption.
test_that("Downloader workers preserve actual response evidence across retries", {
    skip_on_cran()
    app <- downloader_transport__app()
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 1L
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    session <- dl$enqueue(data.frame(
        logical_file_id = "worker-source",
        filename = "data.bin",
        url = server$url("/worker")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    before <- dl$tasks(session)
    expect_equal(before$transfer_bytes, 1024)
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done")
    after <- dl$tasks(session)
    expect_identical(after$task_id, before$task_id)
    expect_identical(after$transfer_generation, before$transfer_generation)
    expect_identical(after$transfer_etag, '"Actual-GET-A"')
    expect_true(after$transfer_complete)
    expect_false(after$transfer_checksum_verified)
    expect_identical(readBin(result$target_path, "raw", 8192), app$locals$bytes)
})

# A new R process reopens the public manifest. Neither its class namespace nor
# in-memory state can supply the validator that authorizes its Range request.
test_that("Downloader resumes persisted GET checkpoints in a new R process", {
    skip_on_cran()
    skip_if_not_installed("processx")
    app <- downloader_transport__app()
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "restart-source",
        filename = "data.bin",
        url = server$url("/restart")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    before <- dl$tasks(session)
    dl$.__enclos_env__$private$finalize()
    output <- file.path(root, "restarted.rds")
    code <- paste0(
        ".libPaths(",
        paste(deparse(.libPaths()), collapse = ""),
        ");library(epwshiftr);",
        "d<-Downloader$new(manifest=",
        encodeString(manifest, quote = '"'),
        ");",
        "d$resume(",
        encodeString(session, quote = '"'),
        ",progress=FALSE);",
        "saveRDS(d$tasks(),",
        encodeString(output, quote = '"'),
        ");d$.__enclos_env__$private$finalize()"
    )
    child <- processx::run(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", code),
        timeout = 30000,
        error_on_status = FALSE,
        env = c(R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep))
    )
    expect_equal(child$status, 0L, info = paste(child$stdout, child$stderr))
    after <- readRDS(output)
    expect_identical(after$status, "done")
    expect_identical(after$task_id, before$task_id)
    expect_identical(after$transfer_generation, before$transfer_generation)
    expect_identical(
        after$transfer_sha256,
        unname(tools::sha256sum(after$target_path))
    )
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_identical(requests$range, c("", "bytes=1024-"))
})

# Configured range mode has the same actual-response contract as an ordinary
# transfer; a precheck cannot legitimize a wrong Content-Range of equal length.
test_that("Downloader checks actual piece ranges before publishing configured transfers", {
    skip_on_cran()
    for (mode in c("valid", "worker", "wrong-offset", "changed-validator")) {
        app <- downloader_transport__piece_app(mode)
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = file.path(root, "manifest.duckdb"),
            retries = 1L,
            n_workers = if (mode == "worker") 1L else 0L,
            transfer_policy = list(
                range_mode = "single",
                piece_size = 2048L,
                piece_concurrency = 2L
            )
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = mode,
            filename = "data.bin",
            url = server$url("/data")
        ))
        result <- dl$run(session, progress = FALSE)
        if (mode %in% c("valid", "worker")) {
            expect_identical(result$status, "done")
            evidence <- dl$tasks(session)
            expect_identical(evidence$transfer_etag, '"Actual-Pieces-A"')
            expect_true(evidence$transfer_complete)
            expect_false(evidence$transfer_checksum_verified)
            expect_identical(
                evidence$transfer_sha256,
                unname(tools::sha256sum(result$target_path))
            )
            expect_identical(
                readBin(result$target_path, "raw", 8192),
                app$locals$bytes
            )
        } else {
            expect_identical(result$status, "error")
            expect_match(
                result$last_error,
                if (mode == "wrong-offset") {
                    "Content-Range"
                } else {
                    "source_changed"
                }
            )
            expect_false(file.exists(result$target_path))
        }
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# Killing a replacement process after its new body starts must not attach its
# validator to the previous weakly identified bytes during the next invocation.
test_that("Downloader keeps generations separate across an interrupted replacement", {
    skip_on_cran()
    skip_if_not_installed("processx")
    app <- downloader_transport__app("kill")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "kill-source",
        filename = "data.bin",
        url = server$url("/kill")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    before <- dl$tasks(session)
    partial <- file.path(dl$tmp_dir, paste0(before$task_id, ".part"))
    dl$.__enclos_env__$private$finalize()
    code <- paste0(
        ".libPaths(",
        paste(deparse(.libPaths()), collapse = ""),
        ");library(epwshiftr);",
        "d<-Downloader$new(manifest=",
        encodeString(manifest, quote = '"'),
        ");",
        "d$resume(",
        encodeString(session, quote = '"'),
        ",progress=FALSE)"
    )
    child <- processx::process$new(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", code),
        stdout = "|",
        stderr = "|",
        env = c(R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep))
    )
    on.exit(if (child$is_alive()) child$kill_tree(), add = TRUE)
    deadline <- Sys.time() + 15
    while (
        child$is_alive() &&
            (!file.exists(partial) || file.info(partial)$size < 2048) &&
            Sys.time() < deadline
    ) {
        Sys.sleep(0.02)
    }
    expect_true(child$is_alive())
    expect_equal(file.info(partial)$size, 2048)
    child$kill_tree()
    child$wait(timeout = 5000)
    expect_false(child$is_alive())
    changed <- as.raw((as.integer(app$locals$bytes) + 1L) %% 256L)
    expect_identical(readBin(partial, "raw", 8192), changed[1:2048])
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    interrupted <- dl$tasks(session)
    expect_identical(interrupted$transfer_etag, '"Actual-GET-B"')
    expect_equal(interrupted$transfer_bytes, 0)
    expect_false(identical(
        interrupted$transfer_generation,
        before$transfer_generation
    ))
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done")
    expect_identical(readBin(result$target_path, "raw", 8192), changed)
    retained <- list.files(dl$tmp_dir, "[.]part[.]retained-", full.names = TRUE)
    expect_length(retained, 2L)
    retained_bytes <- lapply(retained, function(path) {
        readBin(path, "raw", 8192)
    })
    expect_true(any(vapply(
        retained_bytes,
        identical,
        logical(1L),
        app$locals$bytes[1:1024]
    )))
    expect_true(any(vapply(
        retained_bytes,
        identical,
        logical(1L),
        changed[1:2048]
    )))
})

# Complete files retain their original transfer hash even without an expected
# publisher checksum. Verification and both scheduling paths reject local edits.
test_that("Downloader rejects edited complete files without replacing saved evidence", {
    skip_on_cran()
    for (workers in c(0L, 2L)) {
        server <- local_downloader_http_server()
        root <- withr::local_tempdir()
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = file.path(root, "manifest.duckdb"),
            retries = 1L,
            n_workers = workers
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = c("retained-a", "retained-b"),
            filename = c("a.bin", "b.bin"),
            url = server$url("/files/ok.bin")
        ))
        expect_true(all(dl$run(session, progress = FALSE)$status == "done"))
        original <- dl$tasks(session)
        for (path in original$target_path) {
            writeBin(as.raw(rep(255L, 4096)), path)
        }
        checked <- dl$verify(session)
        expect_false(any(checked$checksum_ok))
        expect_true(all(checked$status == "error"))
        result <- dl$resume(session, progress = FALSE)
        expect_true(all(result$status == "error"))
        expect_true(all(grepl("source_changed", result$last_error)))
        expect_identical(
            dl$tasks(session)$transfer_sha256,
            original$transfer_sha256
        )
        expect_false(any(dl$tasks(session)$transfer_checksum_verified))
        expect_true(all(vapply(
            result$target_path,
            function(path) {
                identical(readBin(path, "raw", 4096), as.raw(rep(255L, 4096)))
            },
            logical(1L)
        )))
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# Publisher proof can validate a replica's bytes even when its opaque validator
# differs. That new response validator must not be assigned to the old prefix.
test_that("Downloader keeps byte-producing validators when publisher proof permits appending", {
    skip_on_cran()
    app <- downloader_transport__app("validator")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    session <- dl$enqueue(data.frame(
        logical_file_id = "checksum-source",
        filename = "data.bin",
        checksum = checksum_bytes(app$locals$bytes, "md5"),
        checksum_type = "md5",
        url = server$url("/validator")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    original <- dl$tasks(session)
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done")
    evidence <- dl$tasks(session)
    expect_identical(evidence$transfer_etag, original$transfer_etag)
    expect_false(evidence$transfer_validator_bound)
    expect_true(evidence$transfer_checksum_verified)
    expect_identical(readBin(result$target_path, "raw", 8192), app$locals$bytes)
})
