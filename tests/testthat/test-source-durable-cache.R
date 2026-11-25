# The cache option controls public consumption in both execution modes. Invalid
# bytes reject only their source; explicit replacement keeps the old generation,
# while cache-off acquisition and valid offline reuse retain native values.
test_that("Store consumers isolate edited cache bytes across cache options", {
    root <- withr::local_tempdir()
    paths <- file.path(root, c("source.nc", "peer.nc"))
    write_local_cmip6_netcdf_fixture(paths[[1L]], 2060L, calendar = "360_day")
    file.copy(paths[[1L]], paths[[2L]])
    nc <- RNetCDF::open.nc(paths[[2L]], write = TRUE)
    RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
    RNetCDF::close.nc(nc)
    expected <- lapply(seq_along(paths), function(index) {
        nc <- RNetCDF::open.nc(paths[[index]])
        on.exit(RNetCDF::close.nc(nc))
        as.double(RNetCDF::var.get.nc(
            nc,
            "tas",
            start = c(index + 1L, 1L, 1L),
            count = c(1L, 1L, 2L)
        ))
    })
    logs <- file.path(root, c("source-requests.txt", "peer-requests.txt"))
    servers <- lapply(seq_along(paths), function(index) {
        webfakes::new_app_process(durable_test__app(
            readBin(paths[[index]], "raw", file.info(paths[[index]])$size),
            logs[[index]]
        ))
    })
    withr::defer(lapply(servers, function(server) server$stop()))
    offline <- stats::setNames(vector("list", 2L), c("ordinary", "pooled"))
    for (mode in c("ordinary", "pooled")) {
        cache <- file.path(root, paste0(mode, "-cache"))
        withr::local_options(
            epwshiftr.cache = TRUE,
            epwshiftr.progress = FALSE,
            epwshiftr.dir_cache = cache
        )
        stores <- vector("list", 4L)
        seed <- durable_test__producer(
            file.path(root, paste0(mode, "-seed")),
            mode,
            servers,
            paths
        )
        stores[[1L]] <- seed$store
        baseline <- durable_test__snapshot(seed$store)
        cached <- list.files(
            cache,
            "[.]rds$",
            recursive = TRUE,
            full.names = TRUE
        )
        expect_identical(length(cached), 2L, info = mode)
        key <- readRDS(cached[[1L]])$source_lineage$request$file_key
        bad_cache <- cached[[
            if (identical(key, "master:durable-1")) 1L else 2L
        ]]
        warm <- durable_test__producer(
            file.path(root, paste0(mode, "-warm")),
            mode,
            servers,
            paths,
            acquire = FALSE
        )
        stores[[2L]] <- warm$store
        requests <- durable_test__requests(logs)
        expect_true(
            all(durable_test__consume(warm)$status == "done"),
            info = mode
        )
        expect_identical(durable_test__requests(logs), requests, info = mode)
        con <- file(bad_cache, "ab")
        writeBin(as.raw(1:4), con)
        close(con)
        retained <- checksum_file(bad_cache)
        sidecar <- checksum_file(paste0(bad_cache, ".json"))
        consumer <- durable_test__producer(
            file.path(root, paste0(mode, "-edited")),
            mode,
            servers,
            paths,
            acquire = FALSE
        )
        stores[[3L]] <- consumer$store
        result <- durable_test__consume(consumer)
        result <- result[match(consumer$plans, result$plan_id), ]
        expect_identical(result$status, c("failed", "done"), info = mode)
        expect_match(result$last_error[[1L]], "source_changed", info = mode)
        expect_match(
            result$last_error[[1L]],
            bad_cache,
            fixed = TRUE,
            info = mode
        )
        expect_identical(checksum_file(bad_cache), retained, info = mode)
        expect_identical(
            checksum_file(paste0(bad_cache, ".json")),
            sidecar,
            info = mode
        )
        expect_identical(durable_test__requests(logs), requests, info = mode)
        off <- durable_test__producer(
            file.path(root, paste0(mode, "-off")),
            mode,
            servers,
            paths,
            acquire = FALSE
        )
        stores[[4L]] <- off$store
        withr::with_options(list(epwshiftr.cache = FALSE), {
            expect_true(
                all(durable_test__consume(off)$status == "done"),
                info = mode
            )
        })
        expect_identical(checksum_file(bad_cache), retained, info = mode)
        readback <- durable_test__snapshot(off$store)
        for (index in seq_along(off$plans)) {
            row <- match(off$plans[[index]], readback$results$plan_id)
            expect_equal(
                readback$values[[row]],
                expected[[index]],
                tolerance = 1e-10,
                info = mode
            )
        }
        expect_true(
            all(lengths(durable_test__requests(logs)) > lengths(requests)),
            info = mode
        )
        expect_true(
            all(
                durable_test__consume(consumer, overwrite = TRUE)$status ==
                    "done"
            ),
            info = mode
        )
        expect_identical(
            checksum_file(paste0(bad_cache, ".retained-", retained)),
            retained,
            info = mode
        )
        expect_identical(
            checksum_file(paste0(bad_cache, ".retained-", retained, ".json")),
            sidecar,
            info = mode
        )
        expect_error(
            durable_test__consume(consumer, resume = FALSE),
            class = "epwshiftr_store_extract_conflict"
        )
        requests <- durable_test__requests(logs)
        expect_true(
            all(
                durable_test__consume(
                    consumer,
                    resume = FALSE,
                    overwrite = TRUE
                )$status ==
                    "done"
            ),
            info = mode
        )
        expect_identical(durable_test__requests(logs), requests, info = mode)
        repaired <- durable_test__snapshot(consumer$store)
        expect_identical(nrow(repaired$results), 2L)
        expect_identical(nrow(repaired$artifacts), 2L)
        for (index in seq_along(consumer$plans)) {
            row <- match(consumer$plans[[index]], repaired$results$plan_id)
            expect_equal(
                repaired$values[[row]],
                expected[[index]],
                tolerance = 1e-10,
                info = mode
            )
        }
        target <- durable_test__producer(
            file.path(root, paste0(mode, "-offline")),
            mode,
            servers,
            paths,
            acquire = FALSE
        )
        offline[[mode]] <- list(
            path = target$store$path,
            plans = target$plans,
            mode = mode
        )
        target$store$close()
        for (store in stores) {
            durable_test__remove_native(store)
            store$close()
        }
        durable_test__record(
            paste0(mode, "-cache"),
            list(
                expected = expected,
                source_sha256 = vapply(paths, checksum_file, character(1L)),
                retained_cache_sha256 = retained,
                retained_sidecar_sha256 = sidecar,
                requests = requests,
                repaired = repaired
            ),
            c(
                paths,
                logs,
                bad_cache,
                paste0(bad_cache, ".retained-", retained),
                paste0(bad_cache, ".retained-", retained, ".json")
            )
        )
    }
    unlink(paths)
    lapply(servers, function(server) server$stop())
    requests <- durable_test__requests(logs)
    for (mode in names(offline)) {
        cache <- file.path(root, paste0(mode, "-cache"))
        readback <- durable_test__launch(list(offline[[mode]]), cache)
        expect_true(
            all(readback$answers[[1L]]$result$status == "done"),
            info = mode
        )
        expect_false(identical(readback$pid, Sys.getpid()))
        expect_identical(durable_test__requests(logs), requests, info = mode)
        saved <- readback$answers[[1L]]$snapshot
        for (index in seq_along(offline[[mode]]$plans)) {
            row <- match(offline[[mode]]$plans[[index]], saved$results$plan_id)
            expect_equal(
                saved$values[[row]],
                expected[[index]],
                tolerance = 1e-10,
                info = mode
            )
        }
    }
})

# Ordinary missing/conflicting cache records already have the lineage regression.
# Cover the remaining pooled prefetch gate with a healthy independent cache peer.
test_that("pooled Store consumption isolates missing and conflicting cache lineage", {
    root <- withr::local_tempdir()
    cache <- file.path(root, "cache")
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = cache
    )
    paths <- file.path(root, c("source.nc", "peer.nc"))
    lapply(
        paths,
        write_local_cmip6_netcdf_fixture,
        year = 2060L,
        calendar = "360_day"
    )
    logs <- file.path(root, c("source-requests.txt", "peer-requests.txt"))
    servers <- lapply(seq_along(paths), function(index) {
        webfakes::new_app_process(durable_test__app(
            readBin(paths[[index]], "raw", file.info(paths[[index]])$size),
            logs[[index]]
        ))
    })
    withr::defer(lapply(servers, function(server) server$stop()))
    seed <- durable_test__producer(
        file.path(root, "seed"),
        "pooled",
        servers,
        paths
    )
    withr::defer(seed$store$close())
    cached <- list.files(cache, "[.]rds$", recursive = TRUE, full.names = TRUE)
    key <- readRDS(cached[[1L]])$source_lineage$request$file_key
    bad <- cached[[if (identical(key, "master:durable-1")) 1L else 2L]]
    healthy <- setdiff(cached, bad)
    healthy_hash <- checksum_file(healthy)
    original <- readRDS(bad)
    sidecar <- jsonlite::read_json(paste0(bad, ".json"))
    requests <- durable_test__requests(logs)
    for (fault in c("missing", "conflict")) {
        if (fault == "missing") {
            unlink(paste0(bad, ".json"))
        } else {
            changed <- original
            changed$source_lineage$request$identity$source_id <- "WrongModel"
            saveRDS(changed, bad, compress = FALSE, version = 3L)
            sidecar$sha256 <- checksum_file(bad)
            sidecar$source_lineage <- changed$source_lineage
            jsonlite::write_json(
                sidecar,
                paste0(bad, ".json"),
                auto_unbox = TRUE,
                digits = NA,
                null = "null",
                na = "null"
            )
        }
        retained <- checksum_file(bad)
        consumer <- durable_test__producer(
            file.path(root, fault),
            "pooled",
            servers,
            paths,
            acquire = FALSE
        )
        result <- durable_test__consume(consumer)
        result <- result[match(consumer$plans, result$plan_id), ]
        expect_identical(result$status, c("failed", "done"), info = fault)
        expect_match(
            result$last_error[[1L]],
            if (fault == "missing") {
                "insufficient.*integrity"
            } else {
                "source conflict"
            },
            info = fault
        )
        expect_match(result$last_error[[1L]], bad, fixed = TRUE, info = fault)
        expect_identical(checksum_file(bad), retained, info = fault)
        expect_identical(checksum_file(healthy), healthy_hash, info = fault)
        expect_identical(durable_test__requests(logs), requests, info = fault)
        expect_identical(
            nrow(consumer$store$query("SELECT * FROM extraction_result")),
            1L
        )
        consumer$store$close()
    }
    durable_test__record(
        "pooled-cache-evidence-faults",
        list(
            requests = requests,
            healthy_sha256 = healthy_hash,
            retained_bad_sha256 = checksum_file(bad)
        ),
        c(paths, logs, bad, paste0(bad, ".json"), healthy)
    )
})

# vim: fdm=marker :
