# Independent owning R processes share one real, qualified retained transfer.
# A filesystem gate exposes the active GET boundary without guessed sleep times.
test_that("independent Store processes serialize one retained HTTP transfer lifecycle", {
    skip_on_cran()
    withr::local_options(
        epwshiftr.cache = FALSE,
        epwshiftr.progress = FALSE,
        epwshiftr.mirai_workers = 1L
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    bytes <- readBin(path, "raw", file.info(path)$size)
    gate <- file.path(root, "release")
    server <- webfakes::new_app_process(http_acceptance__app(
        bytes,
        partial = TRUE,
        gate = gate
    ))
    withr::defer(server$stop())
    shared <- file.path(root, "shared")
    paths <- file.path(root, c("store-a", "store-b"))
    plans <- vector("list", 2L)
    for (index in 1:2) {
        store <- EsgStore$new(paths[[index]])
        query <- store$add_files(recovery_test__files(
            path,
            server,
            checksum = checksum_file(path)
        ))
        plans[[index]] <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03 23:59:59")
        )$plan_id
        if (index == 1L) {
            file <- data.table::as.data.table(store$query(
                "SELECT * FROM file_catalog"
            ))
        }
        store$close()
    }
    seed_store <- EsgStore$new(paths[[1L]], create = FALSE)
    dl <- http_acceptance__legacy_downloader(seed_store, file, shared)
    session <- dl$enqueue(data.frame(
        logical_file_id = "qualified-retained-source",
        filename = file$filename,
        file_key = file$file_key,
        dataset_id = file$dataset_id,
        checksum = checksum_file(path),
        checksum_type = "sha256",
        size = length(bytes),
        url = server$url("/files/source.nc")
    ))
    expect_identical(dl$run(session, progress = FALSE)$status, "error")
    original <- dl$tasks(session)
    expect_equal(original$transfer_bytes, 1024)
    expect_identical(
        original$transfer_request_url,
        server$url("/files/source.nc")
    )
    manifest <- dl$manifest
    priv(dl)$disconnect_manifest()
    seed_store$close()
    # This body uses namespaces and explicit arguments only. Keeping baseenv
    # avoids serializing the surrounding test suite into either child process.
    run_child <- function(store_path, plan_id, shared, ready, libs) {
        .libPaths(libs)
        Sys.setenv(R_LIBS_USER = paste(libs, collapse = .Platform$path.sep))
        options(
            epwshiftr.cache = FALSE,
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        store <- epwshiftr::EsgStore$new(store_path, create = FALSE)
        on.exit(store$close(), add = TRUE)
        pool <- epwshiftr::source_pool(1L, timeout = 15, download_dir = shared)
        on.exit(pool$close(force = TRUE), add = TRUE)
        started <- as.double(Sys.time())
        writeLines(as.character(Sys.getpid()), ready)
        ticket <- store$extract(
            plan_id = plan_id,
            fallback = "auto",
            pool = pool
        )
        deadline <- Sys.time() + 45
        while (identical(ticket$status, "pending") && Sys.time() < deadline) {
            pool$poll()
            Sys.sleep(.01)
        }
        stopifnot(
            identical(ticket$status, "done"),
            all(ticket$value$status == "done")
        )
        receipts <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        stopifnot(length(receipts) == 1L)
        receipt <- jsonlite::read_json(receipts, simplifyVector = TRUE)
        output <- store$query(
            "SELECT output_path FROM extraction_result"
        )$output_path
        output <- file.path(store$path, output)
        rows <- store$query(paste0(
            "SELECT value FROM read_parquet('",
            output,
            "') ORDER BY cf_year,cf_month,cf_day,cf_second_of_day"
        ))
        list(
            pid = Sys.getpid(),
            started = started,
            ended = as.double(Sys.time()),
            result = ticket$value,
            receipt = receipt,
            values = rows$value,
            output_sha256 = unname(tools::sha256sum(output))
        )
    }
    environment(run_child) <- baseenv()
    ready <- file.path(root, c("ready-a", "ready-b"))
    start_child <- function(index) {
        callr::r_bg(
            run_child,
            args = list(
                paths[[index]],
                plans[[index]],
                shared,
                ready[[index]],
                .libPaths()
            ),
            libpath = .libPaths(),
            stdout = file.path(root, paste0("child-", index, ".out")),
            stderr = file.path(root, paste0("child-", index, ".err")),
            supervise = TRUE
        )
    }
    # A child exiting early is a fixture failure, never evidence that the next
    # caller acquired a transfer lock. Retain process-tree supervision on error.
    await_marker <- function(marker, process) {
        deadline <- Sys.time() + 30
        while (
            !file.exists(marker) && process$is_alive() && Sys.time() < deadline
        ) {
            Sys.sleep(.01)
        }
        if (!file.exists(marker)) {
            process$get_result()
            stop(
                "Independent caller did not reach the observable transfer boundary"
            )
        }
    }
    first <- start_child(1L)
    withr::defer(if (first$is_alive()) first$kill_tree())
    await_marker(paste0(gate, ".started"), first)
    second <- start_child(2L)
    withr::defer(if (second$is_alive()) second$kill_tree())
    await_marker(ready[[2L]], second)
    release_time <- as.double(Sys.time())
    file.create(gate)
    deadline <- Sys.time() + 60
    while ((first$is_alive() || second$is_alive()) && Sys.time() < deadline) {
        Sys.sleep(.01)
    }
    expect_false(first$is_alive())
    expect_false(second$is_alive())
    if (first$is_alive() || second$is_alive()) {
        stop("Independent Store callers exceeded the deadline")
    }
    values <- list(first$get_result(), second$get_result())
    expect_false(identical(values[[1L]]$pid, values[[2L]]$pid))
    expect_true(values[[1L]]$started < release_time)
    expect_true(values[[2L]]$started < release_time)
    nc <- RNetCDF::open.nc(path)
    expected <- RNetCDF::var.get.nc(nc, "tas")[2L, 1L, 1:3]
    RNetCDF::close.nc(nc)
    for (index in 1:2) {
        receipt <- values[[index]]$receipt
        expect_identical(receipt$session_id, session)
        expect_identical(receipt$task_id, original$task_id)
        expect_identical(receipt$local_sha256, checksum_file(path))
        expect_true(receipt$checksum_verified_by_reader)
        expect_equal(values[[index]]$values, expected, tolerance = 0)
        reopened <- EsgStore$new(paths[[index]], create = FALSE)
        expect_identical(
            reopened$extract(plan_id = plans[[index]])$status,
            "done"
        )
        expect_identical(http_acceptance__receipt(reopened), receipt)
        reopened$close()
    }
    requests <- http_acceptance__requests(server)
    expect_equal(nrow(requests), 2L)
    expect_identical(requests$range, c("", "bytes=1024-"))
    expect_true(all(requests$end >= requests$start))
    stats <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/stats"))$content
    ))
    expect_equal(stats$maximum_active, 1L)
    expect_equal(stats$active, 0L)
    dl <- Downloader$new(manifest = manifest, n_workers = 0L)
    expect_identical(dl$tasks(session)$task_id, original$task_id)
    expect_identical(dl$tasks(session)$transfer_sha256, checksum_file(path))
    priv(dl)$disconnect_manifest()
    http_acceptance__save(
        "independent-processes",
        list(
            fixture_sha256 = checksum_file(path),
            original = original,
            requests = requests,
            stats = stats,
            release_time = release_time,
            callers = values
        )
    )
})

# vim: fdm=marker :
