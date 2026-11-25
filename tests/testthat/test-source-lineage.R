# A completed extraction must carry the actual acquisition before its cache or
# Parquet can become a reusable source for another Store or an offline reopen.
test_that("completed Store outputs retain honest durable source lineage", {
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = withr::local_tempdir()
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    pool <- source_pool(
        1L,
        timeout = 15,
        download_dir = file.path(root, "shared")
    )
    withr::defer(pool$close(force = TRUE))
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        query <- store$add_files(recovery_test__files(
            path,
            server,
            record = mode
        ))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        result <- store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            result <- recovery_test__drain(pool, result)
        }
        expect_identical(result$status, "done", info = mode)
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        saved <- jsonlite::fromJSON(
            artifact$metadata_json[[1L]],
            simplifyVector = FALSE
        )$source_lineage
        expect_identical(
            saved$schema,
            "epwshiftr-source-lineage-v1",
            info = mode
        )
        expect_identical(
            saved$receipt$task_id,
            jsonlite::read_json(list.files(
                file.path(store$path, "sources"),
                "^pool-download-.*[.]json$",
                full.names = TRUE
            )[[1L]])$task_id
        )
        expect_false(saved$receipt$checksum_verified, info = mode)
        expect_null(saved$request$checksum, info = mode)
        expect_identical(
            saved$receipt$local_sha256,
            checksum_file(path),
            info = mode
        )
        file <- store$query("SELECT * FROM file_catalog")
        cache <- store__extract_cache_path(plan, file)
        cached <- readRDS(cache)
        expect_identical(
            cached$source_lineage$receipt$task_id,
            saved$receipt$task_id,
            info = mode
        )
        store$close()
        reopened <- EsgStore$new(store$path, create = FALSE)
        withr::defer(reopened$close())
        withr::local_options(epwshiftr.cache = "offline")
        expect_identical(
            reopened$extract(plan_id = plan$plan_id)$status,
            "done",
            info = mode
        )
        withr::local_options(epwshiftr.cache = TRUE)
    }
})

# Public artifact registration recreates historical records without a source
# chain, conflicting catalogue snapshots and retained output edits.
test_that("completed Store reuse rejects absent conflicting or edited evidence", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    for (fault in c("missing", "conflict", "bytes")) {
        store <- EsgStore$new(file.path(root, fault))
        withr::defer(store$close())
        query <- store$add_files(esgf_test__file_result(esgf_test__file_docs(
            path
        )))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        expect_identical(store$extract(plan_id = plan$plan_id)$status, "done")
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        output <- store$artifact_path(artifact$artifact_id)
        metadata <- jsonlite::fromJSON(
            artifact$metadata_json,
            simplifyVector = FALSE
        )
        if (fault == "missing") {
            metadata$source_lineage <- NULL
        }
        if (fault == "conflict") {
            metadata$source_lineage$request$identity$source_id <- "WrongModel"
        }
        if (fault != "bytes") {
            store$register_artifact(
                "extract",
                output,
                file_key = plan$file_key,
                query_id = plan$query_id,
                metadata = metadata
            )
        } else {
            con <- file(output, "ab")
            writeBin(as.raw(1:4), con)
            close(con)
        }
        retained <- checksum_file(output)
        result <- store$extract(plan_id = plan$plan_id)
        expect_identical(result$status, "failed", info = fault)
        expect_match(
            result$last_error,
            switch(
                fault,
                missing = "insufficient.*lineage",
                conflict = "source conflict",
                bytes = "source_changed"
            ),
            info = fault
        )
        expect_match(
            result$last_error,
            plan$plan_id,
            fixed = TRUE,
            info = fault
        )
        expect_identical(checksum_file(output), retained, info = fault)
        repaired <- store$extract(plan_id = plan$plan_id, overwrite = TRUE)
        expect_identical(repaired$status, "done", info = fault)
        expect_identical(
            store$extract(plan_id = plan$plan_id)$status,
            "done",
            info = fault
        )
    }
})

# A shared cache key can outlive its acquisition; weak HTTP ownership must still
# be checked, with the rejected generation retained until explicit replacement.
test_that("Store shared cache reuse checks the saved source before opening", {
    cache_dir <- withr::local_tempdir()
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = cache_dir
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    stores <- lapply(1:2, function(i) {
        EsgStore$new(file.path(root, paste0("store", i)))
    })
    withr::defer(lapply(stores, function(s) s$close()))
    plans <- lapply(stores, function(store) {
        q <- store$add_files(recovery_test__files(path, server))
        store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    })
    expect_identical(
        stores[[1L]]$extract(plan_id = plans[[1L]]$plan_id)$status,
        "done"
    )
    cached <- list.files(
        cache_dir,
        "[.]rds$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_length(cached, 1L)
    # Keep the requested source identity unchanged. A self-consistent cache
    # digest cannot authorize a lineage descriptor claiming another endpoint.
    payload <- readRDS(cached)
    payload$source_lineage$request$endpoint <- server$url("/foreign.nc")
    saveRDS(payload, cached, version = 3L, compress = FALSE)
    jsonlite::write_json(
        list(
            sha256 = checksum_file(cached),
            source_lineage = payload$source_lineage
        ),
        paste0(cached, ".json"),
        auto_unbox = TRUE,
        na = "null",
        null = "null",
        digits = NA
    )
    before <- checksum_file(cached)
    failed <- stores[[2L]]$extract(plan_id = plans[[2L]]$plan_id)
    expect_identical(failed$status, "failed")
    expect_match(failed$last_error, "source conflict")
    expect_identical(checksum_file(cached), before)
    expect_identical(
        stores[[2L]]$extract(
            plan_id = plans[[2L]]$plan_id,
            overwrite = TRUE
        )$status,
        "done"
    )
    expect_equal(
        recovery_test__values(stores[[2L]]),
        recovery_test__values(stores[[1L]])
    )
    expect_true(file.exists(paste0(cached, ".retained-", before)))
})

# Already catalogued local sources must retain the original native digest when
# serving a new site; neither execution adapter can certify edited bytes anew.
test_that("registered native sources reject changes before another site reads", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(readBin(
        path,
        "raw",
        n = file.info(path)$size
    )))
    withr::defer(server$stop())
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        q <- store$add_files(recovery_test__files(path, server))
        initial <- store$plan_region(
            q,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        expect_identical(
            store$extract(plan_id = initial$plan_id)$status,
            "done"
        )
        catalog <- store$query("SELECT * FROM file_catalog")
        native <- file.path(store$path, catalog$local_path)
        nc <- RNetCDF::open.nc(native, write = TRUE)
        RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
        RNetCDF::close.nc(nc)
        retained <- checksum_file(native)
        next_plan <- store$plan_region(
            q,
            104.5,
            1.5,
            c("2060-01-01", "2060-01-03")
        )
        pool <- source_pool(1L, timeout = 15)
        result <- store$extract(
            plan_id = next_plan$plan_id,
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            result <- recovery_test__drain(pool, result)
        }
        pool$close(force = TRUE)
        expect_identical(result$status, "failed", info = mode)
        expect_match(result$last_error, "source_changed", info = mode)
        expect_identical(checksum_file(native), retained, info = mode)
        repaired <- store$extract(plan_id = next_plan$plan_id, overwrite = TRUE)
        expect_identical(repaired$status, "done", info = mode)
        replacement <- store$query("SELECT * FROM file_catalog")
        replaced_path <- file.path(store$path, replacement$local_path)
        expect_false(identical(replaced_path, native), info = mode)
        expect_identical(
            checksum_file(replaced_path),
            checksum_file(path),
            info = mode
        )
        expect_identical(checksum_file(native), retained, info = mode)
        expect_length(
            list.files(
                file.path(store$path, "sources"),
                "^pool-download-.*[.]json$"
            ),
            2L
        )
    }
})

# Generic Downloader may skip an existing legacy target, but missing producer
# evidence remains observable and cannot certify that target for Store recovery.
test_that("legacy complete downloads without producer evidence are not adopted", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(readBin(
        path,
        "raw",
        n = file.info(path)$size
    )))
    withr::defer(server$stop())
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    q <- store$add_files(recovery_test__files(path, server))
    file <- store$query("SELECT * FROM file_catalog")
    downloader <- store$downloader(n_workers = 0L, cleanup = FALSE)
    session <- downloader$enqueue(data.frame(
        logical_file_id = "old-source",
        file_key = file$file_key,
        dataset_id = file$dataset_id,
        filename = file$filename,
        url = file$url_download,
        size = file.info(path)$size
    ))
    old <- downloader$tasks(session_id = session)
    dir.create(dirname(old$target_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(path, old$target_path)
    nc <- RNetCDF::open.nc(old$target_path, write = TRUE)
    RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
    RNetCDF::close.nc(nc)
    retained <- checksum_file(old$target_path)
    downloader$resume(session_id = session, progress = FALSE)
    verification <- downloader$verify(session_id = session)
    expect_true(verification$checksum_ok)
    expect_identical(verification$integrity_evidence, "missing")
    priv(downloader)$disconnect_manifest()
    plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    expect_identical(store$extract(plan_id = plan$plan_id)$status, "done")
    expect_equal(
        recovery_test__values(store),
        c(299.242919921875, 299.328735351562),
        tolerance = 1e-10
    )
    reference <- jsonlite::read_json(list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )[[1L]])
    expect_false(identical(reference$task_id, old$task_id))
    expect_false(identical(reference$session_id, session))
    expect_identical(checksum_file(old$target_path), retained)
})

# A retained publisher proof applies to its exact expected content, even when
# catalogue identity, version and endpoint stay fixed across a later refresh.
test_that("completed outputs reject changed publisher content at the same endpoint", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        q <- store$add_files(recovery_test__files(
            path,
            server,
            checksum = checksum_file(path)
        ))
        plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
        pool <- source_pool(1L, timeout = 15)
        result <- store$extract(
            plan_id = plan$plan_id,
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            result <- recovery_test__drain(pool, result)
        }
        pool$close(force = TRUE)
        expect_identical(result$status, "done", info = mode)
        before <- store$query("SELECT * FROM file_catalog")
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        output <- store$artifact_path(artifact$artifact_id)
        retained <- checksum_file(output)
        store$add_files(recovery_test__files(
            path,
            server,
            checksum = strrep("f", 64L)
        ))
        current <- store$query("SELECT * FROM file_catalog")
        expect_identical(current$file_key, before$file_key)
        expect_identical(current$url_download, before$url_download)
        expect_identical(current$version, before$version)
        result <- store$extract(plan_id = plan$plan_id)
        expect_identical(result$status, "failed", info = mode)
        expect_match(result$last_error, "source conflict", info = mode)
        expect_identical(checksum_file(output), retained, info = mode)
    }
})

# Saved source snapshots and output hashes must suffice after every native copy
# and the remote node have gone away, for both resume and cache publication.
test_that("complete saved lineage supports offline outputs and cache reuse", {
    cache <- withr::local_tempdir()
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = cache
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    cases <- vector("list", 2L)
    for (index in 1:2) {
        mode <- c("ordinary", "pooled")[[index]]
        store <- EsgStore$new(file.path(root, mode))
        q <- store$add_files(recovery_test__files(path, server, record = mode))
        plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
        pool <- source_pool(1L, timeout = 15)
        result <- store$extract(
            plan_id = plan$plan_id,
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            result <- recovery_test__drain(pool, result)
        }
        pool$close(force = TRUE)
        expect_identical(result$status, "done", info = mode)
        reference <- jsonlite::read_json(list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )[[1L]])
        unlink(reference$cache_path)
        cases[[index]] <- list(path = store$path, plan_id = plan$plan_id)
        store$close()
    }
    unlink(path)
    server$stop()
    withr::local_options(epwshiftr.cache = "offline")
    for (case in cases) {
        store <- EsgStore$new(case$path, create = FALSE)
        withr::defer(store$close())
        expect_identical(store$extract(plan_id = case$plan_id)$status, "done")
        expect_error(
            store$extract(plan_id = case$plan_id, resume = FALSE),
            class = "epwshiftr_store_extract_conflict"
        )
        expect_identical(
            store$extract(
                plan_id = case$plan_id,
                resume = FALSE,
                overwrite = TRUE
            )$status,
            "done"
        )
        expect_equal(
            recovery_test__values(store),
            c(299.242919921875, 299.328735351562),
            tolerance = 1e-10
        )
    }
})

# A source acquisition can survive a failed result transaction; only a complete
# retry publishes its source snapshot together with Parquet/result registration.
test_that("failed publication retains immutable source evidence across reopen", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    store <- EsgStore$new(file.path(root, "store"))
    q <- store$add_files(recovery_test__files(path, server))
    plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    fault <- exchange_test__fail_publication_commit(store, plan$plan_id)
    result <- store$extract(plan_id = plan$plan_id)
    expect_identical(result$status, "failed")
    expect_identical(fault$staged$status, "done")
    expect_equal(nrow(store$query("SELECT * FROM extraction_result")), 0L)
    expect_equal(
        nrow(store$query("SELECT * FROM artifact WHERE kind = 'extract'")),
        0L
    )
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    expect_length(references, 1L)
    retained <- checksum_file(references)
    receipt <- jsonlite::read_json(references[[1L]])
    store$close()
    store <- EsgStore$new(store$path, create = FALSE)
    withr::defer(store$close())
    expect_error(
        store$extract(plan_id = plan$plan_id),
        class = "epwshiftr_store_extract_conflict"
    )
    expect_identical(
        store$extract(plan_id = plan$plan_id, overwrite = TRUE)$status,
        "done"
    )
    artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
    saved <- jsonlite::fromJSON(artifact$metadata_json, simplifyVector = FALSE)
    expect_identical(saved$source_lineage$receipt$task_id, receipt$task_id)
    expect_identical(checksum_file(references), retained)
    expect_identical(store$extract(plan_id = plan$plan_id)$status, "done")
})

# A source snapshot does not certify cached weather bytes when the cache's own
# saved digest is absent; legacy structural readability remains a generic helper.
test_that("extraction caches require saved integrity evidence before reuse", {
    cache <- withr::local_tempdir()
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = cache
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    stores <- lapply(1:2, function(i) {
        EsgStore$new(file.path(root, paste0("store", i)))
    })
    withr::defer(lapply(stores, function(s) s$close()))
    plans <- lapply(stores, function(store) {
        q <- store$add_files(recovery_test__files(path, server))
        store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    })
    expect_identical(
        stores[[1L]]$extract(plan_id = plans[[1L]]$plan_id)$status,
        "done"
    )
    saved <- list.files(cache, "[.]rds$", recursive = TRUE, full.names = TRUE)
    expect_length(saved, 1L)
    before <- checksum_file(saved)
    unlink(paste0(saved, ".json"))
    result <- stores[[2L]]$extract(plan_id = plans[[2L]]$plan_id)
    expect_identical(result$status, "failed")
    expect_match(result$last_error, "insufficient.*integrity")
    expect_identical(checksum_file(saved), before)
})

# Historical local artifacts expose absent acquisition metadata separately from
# a contradictory saved endpoint; current publisher proof can validate bytes.
test_that("legacy local source records distinguish missing and conflicting evidence", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    server <- webfakes::new_app_process(recovery_test__app(
        readBin(path, "raw", n = file.info(path)$size)
    ))
    withr::defer(server$stop())
    for (fault in c("missing", "conflict", "publisher")) {
        store <- EsgStore$new(file.path(root, fault))
        withr::defer(store$close())
        q <- store$add_files(recovery_test__files(
            path,
            server,
            checksum = if (fault == "publisher") {
                checksum_file(path)
            } else {
                NA_character_
            }
        ))
        first <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
        expect_identical(store$extract(plan_id = first$plan_id)$status, "done")
        file <- store$query("SELECT * FROM file_catalog")
        native <- file.path(store$path, file$local_path)
        retained <- checksum_file(native)
        store$register_artifact(
            "netcdf",
            native,
            role = "download",
            project = "CMIP6",
            query_id = file$query_id,
            file_key = file$file_key,
            source_url = if (fault == "conflict") {
                "https://different.example/source.nc"
            } else {
                file$url_download
            },
            metadata = list()
        )
        next_plan <- store$plan_region(
            q,
            104.5,
            1.5,
            c("2060-01-01", "2060-01-03")
        )
        result <- store$extract(plan_id = next_plan$plan_id)
        if (fault == "publisher") {
            expect_identical(result$status, "done")
        } else {
            expect_identical(result$status, "failed", info = fault)
            expect_match(
                result$last_error,
                if (fault == "missing") {
                    "insufficient.*lineage"
                } else {
                    "source conflict"
                },
                info = fault
            )
        }
        expect_identical(checksum_file(native), retained, info = fault)
    }
})

# vim: fdm=marker :
