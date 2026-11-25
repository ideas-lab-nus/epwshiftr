# This public HTTP slice covers a server whose bounded GET ignores Range even
# though negotiation succeeded. Complete bytes must go through a zero-based
# ordinary transfer without changing the saved user policy.
test_that("Downloader recovers configured pieces when bounded ranges receive full content", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_fallback__app()
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 2048L,
            piece_concurrency = 2L
        )
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "full-source",
        filename = "data.bin",
        url = server$url("/data")
    ))
    policy <- dl$transfer_policy
    result <- dl$run(session, progress = FALSE)
    expect_identical(result$status, "done", info = result$last_error)
    if (identical(result$status, "done")) {
        expect_identical(
            readBin(result$target_path, "raw", 8192),
            app$locals$bytes
        )
        evidence <- dl$tasks(session)
        expect_identical(evidence$transfer_etag, '"Actual-A"')
        expect_true(evidence$transfer_validator_bound)
        expect_true(evidence$transfer_complete)
        expect_false(evidence$transfer_checksum_verified)
        expect_identical(
            evidence$transfer_sha256,
            unname(tools::sha256sum(result$target_path))
        )
    }
    expect_identical(
        dl$transfer_policy[sort(names(policy))],
        policy[sort(names(policy))]
    )
    dl$.__enclos_env__$private$finalize()
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    expect_identical(
        dl$transfer_policy[sort(names(policy))],
        policy[sort(names(policy))]
    )
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_true(any(requests$range == "bytes=0-2047"))
    expect_true(any(requests$range == ""))
})

# A completed 206 piece must keep its original strong representation across
# manifest reopen; a later full response cannot erase that earlier anchor.
test_that("Downloader rejects changed full responses after reopening saved pieces", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_fallback__app("changed")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 2048L,
            piece_concurrency = 1L
        )
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "saved-source",
        filename = "data.bin",
        url = server$url("/data")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    original <- dl$tasks(session)
    piece_dir <- file.path(dl$tmp_dir, paste0(original$task_id, ".pieces"))
    original_piece <- list.files(piece_dir, full.names = TRUE)
    original_piece <- original_piece[file.info(original_piece)$size == 2048]
    expect_length(original_piece, 1L)
    original_bytes <- readBin(original_piece[[1L]], "raw", 2048)
    dl$.__enclos_env__$private$finalize()
    curl::curl_fetch_memory(server$url("/advance"))
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "error", info = result$last_error)
    expect_match(result$last_error, "source_changed")
    expect_false(file.exists(result$target_path))
    retained <- list.files(piece_dir, full.names = TRUE)
    expect_true(any(vapply(
        retained,
        function(path) {
            identical(readBin(path, "raw", 8192), original_bytes)
        },
        logical(1L)
    )))
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_false(any(requests$range == ""))
})

# Once whole-file recovery itself is interrupted, a fresh Downloader must use
# the retained whole-file offset and the same original representation anchors.
test_that("Downloader resumes interrupted full recovery after manifest reopen", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_fallback__app("interrupt")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 2048L,
            piece_concurrency = 2L
        )
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "interrupted-full",
        filename = "data.bin",
        url = server$url("/data")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    original <- dl$tasks(session)
    expect_equal(original$transfer_bytes, 1024)
    partial <- file.path(dl$tmp_dir, paste0(original$task_id, ".part"))
    expect_identical(readBin(partial, "raw", 8192), app$locals$bytes[1:1024])
    dl$.__enclos_env__$private$finalize()
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done", info = result$last_error)
    if (identical(result$status, "done")) {
        evidence <- dl$tasks(session)
        expect_identical(evidence$task_id, original$task_id)
        expect_identical(
            evidence$transfer_generation,
            original$transfer_generation
        )
        expect_identical(evidence$transfer_etag, original$transfer_etag)
        expect_identical(
            readBin(result$target_path, "raw", 8192),
            app$locals$bytes
        )
    }
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_identical(
        tail(unlist(requests$range, use.names = FALSE), 1L),
        "bytes=1024-"
    )
})

# Identical actual representations permit zero-based recovery while the earlier
# valid piece and its identity remain available as retained history.
test_that("Downloader preserves saved pieces during matching full recovery in both runtimes", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    for (workers in c(0L, 2L)) {
        app <- downloader_fallback__app("matching")
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        manifest <- file.path(root, "manifest.duckdb")
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = manifest,
            retries = 1L,
            n_workers = workers,
            transfer_policy = list(
                range_mode = "single",
                piece_size = 2048L,
                piece_concurrency = 1L
            )
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = "matching-source",
            filename = "data.bin",
            url = server$url("/data")
        ))
        expect_identical(dl$run(session, progress = FALSE)$status, "error")
        original <- dl$tasks(session)
        policy <- dl$transfer_policy
        piece_dir <- file.path(dl$tmp_dir, paste0(original$task_id, ".pieces"))
        original_piece <- list.files(piece_dir, full.names = TRUE)
        original_piece <- original_piece[file.info(original_piece)$size == 2048]
        expect_length(original_piece, 1L)
        original_hash <- unname(tools::sha256sum(original_piece))
        dl$.__enclos_env__$private$finalize()
        curl::curl_fetch_memory(server$url("/advance"))
        dl <- Downloader$new(manifest = manifest)
        result <- dl$resume(session, progress = FALSE)
        expect_identical(result$status, "done", info = result$last_error)
        if (identical(result$status, "done")) {
            evidence <- dl$tasks(session)
            expect_identical(evidence$task_id, original$task_id)
            expect_identical(
                readBin(result$target_path, "raw", 8192),
                app$locals$bytes
            )
            expect_identical(evidence$transfer_etag, '"Actual-A"')
            expect_true(evidence$transfer_validator_bound)
            expect_false(evidence$transfer_checksum_verified)
        }
        retained <- list.files(
            piece_dir,
            "[.]done[.]retained-",
            full.names = TRUE
        )
        expect_true(original_hash %in% unname(tools::sha256sum(retained)))
        expect_false(file.exists(original_piece))
        expect_identical(
            dl$transfer_policy[sort(names(policy))],
            policy[sort(names(policy))]
        )
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# One complete response does not authorize ignoring a representation conflict
# observed by another active request in the same bounded-piece batch.
test_that("Downloader reports representation contradictions before accepting batch recovery", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_fallback__app("batch-change")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = file.path(root, "manifest.duckdb"),
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 2048L,
            piece_concurrency = 2L
        )
    )
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    session <- dl$enqueue(data.frame(
        logical_file_id = "batch-change",
        filename = "data.bin",
        url = server$url("/data")
    ))
    result <- dl$run(session, progress = FALSE)
    expect_identical(result$status, "error", info = result$last_error)
    expect_match(result$last_error, "source_changed")
    expect_false(file.exists(result$target_path))
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_false(any(requests$range == ""))
})

# Configured single/multi/auto selection shares the same actual-response guard
# in synchronous and installed worker runtimes, including checksum-bound mirrors.
test_that("Downloader recovers full responses across configured modes and runtimes", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    for (workers in c(0L, 2L)) {
        for (mode in c("single", "multi", "auto")) {
            app <- downloader_fallback__app()
            server <- webfakes::new_app_process(app)
            root <- withr::local_tempdir()
            dl <- Downloader$new(
                dest = file.path(root, "dest"),
                temp = file.path(root, "temp"),
                manifest = file.path(root, "manifest.duckdb"),
                retries = 1L,
                n_workers = workers,
                transfer_policy = list(
                    range_mode = mode,
                    piece_size = 2048L,
                    piece_concurrency = 2L
                )
            )
            session <- dl$enqueue(data.frame(
                logical_file_id = "full-mirrors",
                filename = "data.bin",
                url = server$url(c("/data?copy=one", "/data?copy=two")),
                checksum = checksum_bytes(app$locals$bytes, "md5"),
                checksum_type = "md5"
            ))
            policy <- dl$transfer_policy
            result <- dl$run(session, progress = FALSE)
            expect_identical(
                result$status,
                "done",
                info = paste(
                    mode,
                    workers,
                    result$last_error
                )
            )
            if (identical(result$status, "done")) {
                evidence <- dl$tasks(session)
                expect_identical(
                    readBin(result$target_path, "raw", 8192),
                    app$locals$bytes
                )
                expect_identical(evidence$transfer_etag, '"Actual-A"')
                expect_true(evidence$transfer_complete)
                expect_true(evidence$transfer_checksum_verified)
                expect_identical(
                    evidence$transfer_sha256,
                    unname(tools::sha256sum(result$target_path))
                )
            }
            expect_identical(
                dl$transfer_policy[sort(names(policy))],
                policy[sort(names(policy))]
            )
            requests <- jsonlite::fromJSON(rawToChar(
                curl::curl_fetch_memory(server$url("/requests"))$content
            ))
            expect_true(any(requests$range == "bytes=0-2047"))
            expect_true(any(requests$range == ""))
            dl$.__enclos_env__$private$finalize()
            server$stop()
        }
    }
})

# Publisher identity remains binding when a complete replacement uses a new
# response validator. Failed content never enters the requested target, and the
# original piece and its producer receipt remain readable without modification.
test_that("Downloader retains original evidence when full recovery fails its publisher checksum", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    for (workers in c(0L, 2L)) {
        app <- downloader_fallback__app("checksum-change")
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        manifest <- file.path(root, "manifest.duckdb")
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = manifest,
            retries = 1L,
            n_workers = workers,
            transfer_policy = list(
                range_mode = "single",
                piece_size = 2048L,
                piece_concurrency = 1L
            )
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = "checksum-fixed",
            filename = "data.bin",
            url = server$url("/data"),
            checksum = checksum_bytes(app$locals$bytes, "md5"),
            checksum_type = "md5"
        ))
        expect_identical(dl$run(session, progress = FALSE)$status, "error")
        original <- dl$tasks(session)
        piece_dir <- file.path(dl$tmp_dir, paste0(original$task_id, ".pieces"))
        original_piece <- list.files(piece_dir, "[.]done$", full.names = TRUE)
        expect_length(original_piece, 1L)
        original_hash <- unname(tools::sha256sum(original_piece))
        original_receipt <- readLines(paste0(original_piece, ".transfer.json"))
        dl$.__enclos_env__$private$finalize()
        curl::curl_fetch_memory(server$url("/advance"))
        dl <- Downloader$new(manifest = manifest)
        result <- dl$resume(session, progress = FALSE)
        expect_identical(result$status, "error", info = result$last_error)
        expect_match(result$last_error, "source_changed.*publisher checksum")
        expect_false(file.exists(result$target_path))
        evidence <- dl$tasks(session)
        expect_false(evidence$transfer_checksum_verified)
        expect_identical(evidence$transfer_etag, '"Actual-B"')
        retained <- list.files(
            piece_dir,
            "[.]done[.]retained-",
            full.names = TRUE
        )
        expect_true(original_hash %in% unname(tools::sha256sum(retained)))
        receipts <- list.files(
            piece_dir,
            "[.]transfer[.]json[.]retained-",
            full.names = TRUE
        )
        expect_true(any(vapply(
            receipts,
            function(path) {
                identical(readLines(path), original_receipt)
            },
            logical(1L)
        )))
        requests <- jsonlite::fromJSON(rawToChar(
            curl::curl_fetch_memory(server$url("/requests"))$content
        ))
        expect_true(any(requests$range == ""))
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# A partial saved piece is insufficient complete-file evidence for a rejected
# bounded request. The range failure must preserve the earlier bytes and receipt.
test_that("Downloader rejects piece 416 responses without complete-file evidence", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    for (workers in c(0L, 2L)) {
        app <- downloader_fallback__app("range416")
        server <- webfakes::new_app_process(app)
        root <- withr::local_tempdir()
        manifest <- file.path(root, "manifest.duckdb")
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = manifest,
            retries = 1L,
            n_workers = workers,
            transfer_policy = list(
                range_mode = "single",
                piece_size = 2048L,
                piece_concurrency = 1L
            )
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = "incomplete-range",
            filename = "data.bin",
            url = server$url("/data")
        ))
        expect_identical(dl$run(session, progress = FALSE)$status, "error")
        original <- dl$tasks(session)
        piece_dir <- file.path(dl$tmp_dir, paste0(original$task_id, ".pieces"))
        original_piece <- list.files(piece_dir, "[.]done$", full.names = TRUE)
        expect_length(original_piece, 1L)
        original_hash <- unname(tools::sha256sum(original_piece))
        original_receipt <- readLines(paste0(original_piece, ".transfer.json"))
        dl$.__enclos_env__$private$finalize()
        curl::curl_fetch_memory(server$url("/advance"))
        dl <- Downloader$new(manifest = manifest)
        result <- dl$resume(session, progress = FALSE)
        expect_identical(result$status, "error", info = result$last_error)
        expect_match(result$last_error, "HTTP.*Content-Range")
        expect_false(file.exists(result$target_path))
        expect_identical(
            unname(tools::sha256sum(original_piece)),
            original_hash
        )
        expect_identical(
            readLines(paste0(original_piece, ".transfer.json")),
            original_receipt
        )
        requests <- jsonlite::fromJSON(rawToChar(
            curl::curl_fetch_memory(server$url("/requests"))$content
        ))
        expect_false(any(requests$range == ""))
        dl$.__enclos_env__$private$finalize()
        server$stop()
    }
})

# A matching server may ignore the resumed whole-file Range too. Without a
# Content-Length, the original total still constrains the complete zero-based
# body rather than the discarded suffix length.
test_that("Downloader accepts a chunked full response after interrupted recovery", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    app <- downloader_fallback__app("interrupt-full")
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    root <- withr::local_tempdir()
    manifest <- file.path(root, "manifest.duckdb")
    dl <- Downloader$new(
        dest = file.path(root, "dest"),
        temp = file.path(root, "temp"),
        manifest = manifest,
        retries = 1L,
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 2048L,
            piece_concurrency = 2L
        )
    )
    session <- dl$enqueue(data.frame(
        logical_file_id = "chunked-recovery",
        filename = "data.bin",
        url = server$url("/data")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    original <- dl$tasks(session)
    expect_equal(original$transfer_bytes, 1024)
    dl$.__enclos_env__$private$finalize()
    dl <- Downloader$new(manifest = manifest)
    on.exit(dl$.__enclos_env__$private$finalize(), add = TRUE)
    result <- dl$resume(session, progress = FALSE)
    expect_identical(result$status, "done", info = result$last_error)
    if (identical(result$status, "done")) {
        expect_identical(
            readBin(result$target_path, "raw", 8192),
            app$locals$bytes
        )
        expect_identical(dl$tasks(session)$transfer_etag, '"Actual-A"')
        expect_true(dl$tasks(session)$transfer_complete)
    }
})

# vim: fdm=marker :
