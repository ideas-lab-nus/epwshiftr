# Killing an R owner is different from an injected transaction error. The public
# reporter acts as an external release seam at two observable durable boundaries.
test_that("Store publication readback distinguishes actual killed R processes", {
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
    for (boundary in c("source", "committed")) {
        store <- EsgStore$new(file.path(root, boundary))
        query <- store$add_files(recovery_test__files(
            path,
            server,
            record = boundary
        ))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        case <- list(
            path = store$path,
            plans = plan$plan_id,
            mode = if (boundary == "source") "pooled" else "ordinary",
            boundary = boundary,
            marker = file.path(root, paste0(boundary, ".rds")),
            release = file.path(root, paste0(boundary, ".release"))
        )
        store$close()
        child <- durable_test__launch(
            list(case),
            withr::local_tempdir(),
            mode = FALSE,
            wait = FALSE
        )
        withr::defer(if (child$process$is_alive()) child$process$kill_tree())
        deadline <- as.double(Sys.time()) + 45
        while (
            !file.exists(case$marker) &&
                child$process$is_alive() &&
                as.double(Sys.time()) < deadline
        ) {
            Sys.sleep(0.02)
        }
        if (!file.exists(case$marker)) {
            child$process$kill_tree()
            stop(paste(
                "Reporter boundary was not reached",
                readLines(child$stderr, warn = FALSE)
            ))
        }
        observed <- readRDS(case$marker)
        expect_identical(observed$pid, child$process$get_pid())
        expect_false(identical(observed$pid, Sys.getpid()))
        expect_length(observed$outputs, if (boundary == "source") 0L else 1L)
        descendants <- ps::ps_children(
            ps::ps_handle(observed$pid),
            recursive = TRUE
        )
        descendant_pids <- vapply(descendants, ps::ps_pid, integer(1L))
        # mirai readers need not be OS descendants; the public pool gives their
        # exact owned PIDs, so terminate and confirm those handles explicitly.
        readers <- lapply(observed$readers, ps::ps_handle)
        if (boundary == "source") {
            expect_length(readers, 1L)
        }
        owned <- c(descendants, readers)

        child$process$kill_tree()
        child$process$wait(5000)
        for (handle in readers) {
            # Parent loss can already have ended this exact reader.
            if (ps::ps_is_running(handle)) {
                try(ps::ps_kill(handle), silent = TRUE)
            }
        }
        deadline <- as.double(Sys.time()) + 5
        while (
            any(vapply(owned, ps::ps_is_running, logical(1L))) &&
                as.double(Sys.time()) < deadline
        ) {
            Sys.sleep(0.02)
        }
        stopped <- !any(vapply(owned, ps::ps_is_running, logical(1L)))
        expect_true(stopped)
        if (!stopped) {
            stop("Owned readers have not stopped; do not restart extraction")
        }

        durable_test__record(
            paste0("killed-", boundary),
            list(
                observed = observed,
                descendants = descendant_pids,
                reader_pids = observed$readers,
                exit_status = child$process$get_exit_status(),
                source_sha256 = checksum_file(path)
            ),
            c(
                path,
                case$marker,
                child$input,
                child$stdout,
                child$stderr,
                observed$references,
                observed$outputs
            )
        )

        expect_false(child$process$is_alive())
        expect_false(identical(child$process$get_exit_status(), 0L))
        # Each retry starts a second R process after all killed handles are gone.
        case$boundary <- NULL
        case$fallback <- "error"
        reopened <- EsgStore$new(case$path, create = FALSE)
        retained <- durable_test__snapshot(reopened)
        expect_identical(
            nrow(retained$results),
            if (boundary == "source") 0L else 1L
        )
        expect_identical(retained$reference_md5, observed$reference_md5)
        if (boundary == "committed") {
            expect_identical(retained$output_md5, observed$output_md5)
        }
        reopened$close()
        requests <- durable_test__requests(log)
        retry <- durable_test__launch(
            list(case),
            withr::local_tempdir(),
            mode = FALSE
        )
        expect_false(identical(retry$pid, observed$pid))
        expect_identical(retry$answers[[1L]]$result$status, "done")
        expect_identical(nrow(retry$answers[[1L]]$snapshot$results), 1L)
        expect_identical(nrow(retry$answers[[1L]]$snapshot$artifacts), 1L)
        expect_identical(
            retry$answers[[1L]]$snapshot$reference_md5,
            observed$reference_md5
        )
        expect_identical(durable_test__requests(log), requests)
        reference <- jsonlite::read_json(observed$references[[1L]])
        lineage <- jsonlite::fromJSON(
            retry$answers[[1L]]$snapshot$artifacts$metadata_json,
            simplifyVector = FALSE
        )$source_lineage
        expect_identical(lineage$receipt$task_id, reference$task_id)
        expect_identical(lineage$receipt$local_sha256, reference$local_sha256)
        expect_equal(
            retry$answers[[1L]]$snapshot$values[[1L]],
            expected,
            tolerance = 1e-10
        )
        durable_test__record(
            paste0("retried-", boundary),
            list(
                requests = requests,
                lineage = lineage,
                pid = retry$pid,
                expected = expected
            ),
            log
        )
        if (boundary == "committed") {
            expect_identical(
                retry$answers[[1L]]$snapshot$output_md5,
                observed$output_md5
            )
        }
    }
})

# vim: fdm=marker :
