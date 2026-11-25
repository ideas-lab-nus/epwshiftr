# Generic public downloads already retain byte-linked GET evidence. A later
# offline extraction must adopt that exact saved task rather than require a new
# publisher checksum or mistake this normal two-step workflow for legacy data.
test_that("fresh R extracts verified generic downloads without the remote source", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    log <- file.path(root, "requests.txt")
    server <- webfakes::new_app_process(durable_test__app(
        readBin(path, "raw", file.info(path)$size),
        log
    ))
    nc <- RNetCDF::open.nc(path)
    expected <- as.double(RNetCDF::var.get.nc(
        nc,
        "tas",
        start = c(2L, 1L, 1L),
        count = c(1L, 1L, 2L)
    ))
    RNetCDF::close.nc(nc)
    withr::defer(server$stop())
    cases <- tasks <- manifests <- stats::setNames(
        vector("list", 2L),
        c("ordinary", "pooled")
    )
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        downloader <- store$downloader(n_workers = 1L, retries = 1L)
        session <- store$download_files(
            files = recovery_test__files(path, server),
            replica = "current",
            downloader = downloader,
            progress = FALSE,
            probe = FALSE
        )
        tasks[[mode]] <- downloader$tasks(session_id = session)
        manifests[[mode]] <- downloader$manifest
        expect_identical(tasks[[mode]]$status, "done")
        expect_true(tasks[[mode]]$transfer_complete)
        expect_identical(tasks[[mode]]$transfer_sha256, checksum_file(path))
        file <- store$query("SELECT * FROM file_catalog")
        plan <- store$plan_region(
            file$query_id,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        cases[[mode]] <- list(
            path = store$path,
            plans = plan$plan_id,
            mode = mode,
            fallback = "error"
        )
        store$close()
        rm(downloader)
        gc()
    }
    requests <- durable_test__requests(log)
    durable_test__record(
        "generic-downloaded-inputs",
        list(
            tasks = tasks,
            manifests = manifests,
            expected = expected,
            requests = requests,
            source_sha256 = checksum_file(path)
        ),
        c(
            path,
            log,
            vapply(tasks, function(task) task$target_path, character(1L))
        )
    )
    server$stop()

    unlink(path)
    readback <- durable_test__launch(
        cases,
        withr::local_tempdir(),
        mode = FALSE
    )
    expect_false(identical(readback$pid, Sys.getpid()))
    expect_identical(durable_test__requests(log), requests)
    for (index in seq_along(cases)) {
        answer <- readback$answers[[index]]
        expect_identical(
            answer$result$status,
            "done",
            info = names(cases)[[index]]
        )
        expect_identical(nrow(answer$snapshot$results), 1L)
        # Readback is verification only and must retain the user's saved worker policy.
        inspection <- Downloader$new(manifest = manifests[[index]])
        expect_identical(inspection$config$n_workers, 1L)
        rm(inspection)
        gc()
        if (nrow(answer$snapshot$artifacts)) {
            lineage <- jsonlite::fromJSON(
                answer$snapshot$artifacts$metadata_json,
                simplifyVector = FALSE
            )$source_lineage
            expect_identical(lineage$receipt$task_id, tasks[[index]]$task_id)
            expect_identical(
                lineage$receipt$local_sha256,
                tasks[[index]]$transfer_sha256
            )
            expect_false(lineage$receipt$checksum_verified)
            expect_identical(lineage$receipt$manifest, manifests[[index]])
            expect_identical(
                lineage$receipt$session_id,
                tasks[[index]]$session_id
            )
            expect_equal(
                answer$snapshot$values[[1L]],
                expected,
                tolerance = 1e-10
            )
        }
    }
})

# vim: fdm=marker :
