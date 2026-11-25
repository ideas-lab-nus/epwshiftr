# Retained history must not hide genuine progress in the replacement generation.
# Both attempts use EsgStore's normal HTTP fallback and real persistent readers.
test_that("pooled extraction observes a fresh transfer below its old partial size", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    reference <- EsgStore$new(file.path(root, "reference"))
    withr::defer(reference$close())
    reference_query <- reference$add_files(esgf_test__file_result(
        esgf_test__file_docs(path)
    ))
    reference_plan <- reference$plan_region(
        reference_query,
        104,
        1.37,
        c("2060-01-01", "2060-01-03")
    )
    stopifnot(
        reference$extract(reference_plan$plan_id, fallback = "error")$status ==
            "done"
    )
    expected <- recovery_test__values(reference)
    release <- file.path(root, "release")
    requests <- file.path(root, "requests")
    server <- webfakes::new_app_process(source_pool_transfer_test__app(
        bytes,
        release,
        requests
    ))
    withr::defer(server$stop())
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    query <- store$add_files(recovery_test__files(path, server))
    plan <- store$plan_region(query, 104, 1.37, c("2060-01-01", "2060-01-03"))
    shared <- file.path(root, "shared")
    pool <- source_pool(
        1L,
        timeout = 2,
        download_timeout = 25,
        download_dir = shared
    )
    withr::defer(pool$close(force = TRUE))
    old <- ps::ps_handle(pool$status()[[1L]]$pid)
    first <- source_pool_transfer_test__drain(
        pool,
        store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(first$status, "failed")
    expect_match(first$last_error, "deadline")
    expect_false(ps::ps_is_running(old))
    partial <- list.files(
        shared,
        "[.]part$",
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE
    )
    expect_length(partial, 1L)
    expect_equal(file.info(partial)$size, 20480)
    stopifnot(file.create(release))
    # A separate request synchronizes with the released first HTTP response.
    curl::curl_fetch_memory(server$url("/counts-not-found"))
    second <- source_pool_transfer_test__drain(
        pool,
        store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(second$status, "done", info = second$last_error)
    expect_identical(trimws(readLines(requests)), c("full", "full"))
    retained <- list.files(
        shared,
        "[.]part[.]retained-",
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE
    )
    expect_length(retained, 1L)
    expect_identical(readBin(retained, "raw", n = 20480), bytes[1:20480])
    if (identical(second$status, "done")) {
        receipt_path <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        receipt <- jsonlite::read_json(receipt_path[[1L]])
        expect_identical(receipt$local_sha256, checksum_file(path))
        expect_equal(recovery_test__values(store), expected)
    }
})

# The persisted Downloader policy is a user setting; pool liveness must observe
# actual range-piece growth without overriding that policy or reading arrays.
test_that("pooled extraction observes nested range-piece growth", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    release <- file.path(root, "release")
    requests <- file.path(root, "requests")
    server <- webfakes::new_app_process(source_pool_transfer_test__app(
        bytes,
        release,
        requests,
        ranges = TRUE
    ))
    withr::defer(server$stop())
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    query <- store$add_files(recovery_test__files(
        path,
        server,
        checksum = checksum_file(path)
    ))
    plan <- store$plan_region(query, 104, 1.37, c("2060-01-01", "2060-01-03"))
    shared <- file.path(root, "shared")
    pool <- source_pool(
        1L,
        timeout = 2,
        download_timeout = 25,
        download_dir = shared
    )
    withr::defer(pool$close(force = TRUE))
    first <- source_pool_transfer_test__drain(
        pool,
        store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(first$status, "failed")
    expect_match(first$last_error, "deadline")
    manifests <- list.files(
        shared,
        "^manifest[.]duckdb$",
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE
    )
    expect_length(manifests, 1L)
    downloader <- Downloader$new(
        manifest = manifests[[1L]],
        n_workers = 0L,
        transfer_policy = list(
            range_mode = "single",
            piece_size = 8192L,
            piece_concurrency = 1L
        )
    )
    expect_identical(downloader$transfer_policy$range_mode, "single")
    task <- downloader$tasks()
    # Release the public Downloader before the reader takes its lifetime lock.
    rm(downloader)
    gc()
    stopifnot(file.create(release))
    curl::curl_fetch_memory(server$url("/counts-not-found"))
    second <- source_pool_transfer_test__drain(
        pool,
        store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(second$status, "done", info = second$last_error)
    observed <- trimws(readLines(requests))
    expect_true(any(grepl("^bytes=0-8191$", observed)))
    expect_true(any(grepl("^bytes=8192-16383$", observed)))
    if (identical(second$status, "done")) {
        receipt_path <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        receipt <- jsonlite::read_json(receipt_path[[1L]])
        expect_identical(receipt$local_sha256, checksum_file(path))
        expect_identical(receipt$task_id, task$task_id[[1L]])
        downloader <- Downloader$new(manifest = manifests[[1L]], n_workers = 0L)
        expect_identical(downloader$transfer_policy$range_mode, "single")
        expect_true(all(downloader$tasks()$status == "done"))
        rm(downloader)
        gc()
    }
})

# Legacy adoption must follow the exact saved temp directory instead of waiting
# for a canonical manifest that this valid recovery does not need to create.
test_that("pooled extraction observes its adopted legacy transfer location", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    server <- webfakes::new_app_process(source_pool_transfer_test__app(
        readBin(path, "raw", n = file.info(path)$size),
        file.path(root, "release"),
        file.path(root, "requests"),
        ranges = TRUE,
        bootstrap = FALSE
    ))
    withr::defer(server$stop())
    fixture <- source_pool_transfer_test__legacy(root, path, server)
    pool <- source_pool(
        1L,
        timeout = 2,
        download_timeout = 25,
        download_dir = fixture$shared
    )
    withr::defer(pool$close(force = TRUE))
    reader <- pool$status()[[1L]]$pid
    result <- source_pool_transfer_test__drain(
        pool,
        fixture$store$extract(
            plan_id = fixture$plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(result$status, "done", info = result$last_error)
    expect_true(any(grepl(
        "^bytes=1024-$",
        trimws(readLines(file.path(root, "requests")))
    )))
    if (identical(result$status, "done")) {
        expect_identical(pool$status()[[1L]]$pid, reader)
        receipts <- list.files(
            file.path(fixture$store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        receipt <- jsonlite::read_json(receipts[[1L]])
        expect_identical(receipt$cache_manifest, fixture$manifest)
        expect_identical(receipt$task_id, fixture$task$task_id[[1L]])
        expect_identical(receipt$session_id, fixture$session)
        expect_identical(receipt$local_sha256, checksum_file(path))
    }
})

# Other tasks and retained histories can still be written by unrelated work.
# Their growth cannot authorize extending a stalled selected reader's deadline.
test_that("unrelated legacy bytes and retained history do not hide a stalled transfer", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    noise <- file.path(root, "actual-retained")
    server <- webfakes::new_app_process(source_pool_transfer_test__app(
        readBin(path, "raw", n = file.info(path)$size),
        file.path(root, "release"),
        file.path(root, "requests"),
        ranges = TRUE,
        bootstrap = FALSE,
        stall = TRUE,
        noise = noise
    ))
    withr::defer(server$stop())
    fixture <- source_pool_transfer_test__legacy(root, path, server)
    pool <- source_pool(
        1L,
        timeout = 2,
        download_timeout = 25,
        download_dir = fixture$shared
    )
    withr::defer(pool$close(force = TRUE))
    old <- ps::ps_handle(pool$status()[[1L]]$pid)
    result <- source_pool_transfer_test__drain(
        pool,
        fixture$store$extract(
            plan_id = fixture$plan$plan_id,
            fallback = "auto",
            pool = pool
        )
    )
    expect_identical(result$status, "failed")
    expect_match(result$last_error, "deadline")
    expect_false(ps::ps_is_running(old))
    expect_equal(file.info(fixture$partial)$size, 2048)
    for (name in c(
        "unrelated.part",
        "old.part.retained-history",
        "old.pieces.retained-history/000001.done"
    )) {
        expect_gt(file.info(file.path(noise, name))$size, 2048)
    }
})

# vim: fdm=marker :
