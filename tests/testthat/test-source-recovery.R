# The same catalogue key and ETag cannot authorize cross-endpoint reuse without
# publisher content evidence, even when filename, length and native grid agree.
test_that("Store recovery keeps weakly identified HTTP sources independent", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    first_path <- file.path(root, "source.nc")
    second_path <- file.path(root, "different.nc")
    write_local_cmip6_netcdf_fixture(first_path, 2060L, calendar = "360_day")
    file.copy(first_path, second_path)
    nc <- RNetCDF::open.nc(second_path, write = TRUE)
    values <- RNetCDF::var.get.nc(nc, "tas")
    RNetCDF::var.put.nc(nc, "tas", values + 7)
    RNetCDF::close.nc(nc)
    first_server <- webfakes::new_app_process(recovery_test__app(
        readBin(first_path, "raw", n = file.info(first_path)$size)
    ))
    second_server <- webfakes::new_app_process(recovery_test__app(
        readBin(second_path, "raw", n = file.info(second_path)$size)
    ))
    withr::defer(first_server$stop())
    withr::defer(second_server$stop())
    pool <- source_pool(
        1L,
        timeout = 15,
        download_dir = file.path(root, "shared")
    )
    withr::defer(pool$close(force = TRUE))
    for (mode in c("ordinary", "pooled")) {
        observed <- vector("list", 2L)
        for (index in 1:2) {
            store <- EsgStore$new(file.path(root, paste0(mode, index)))
            withr::defer(store$close())
            query <- store$add_files(recovery_test__files(
                first_path,
                if (index == 1L) first_server else second_server
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
            expect_identical(
                result$status,
                "done",
                info = paste(mode, result$last_error)
            )
            observed[[index]] <- recovery_test__values(store)
        }
        expect_equal(
            observed[[2L]] - observed[[1L]],
            rep(7, length(observed[[1L]])),
            info = mode
        )
    }
})

# A local file digest is useful provenance, but does not mean a missing
# publisher checksum was verified. Both adapters publish the exact used task.
test_that("Store recovery receipts distinguish local hashes from publisher verification", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
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
        query <- store$add_files(recovery_test__files(path, server))
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
        references <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        expect_length(references, 1L)
        if (length(references) != 1L) {
            next
        }
        reference <- jsonlite::read_json(
            references[[1L]],
            simplifyVector = TRUE
        )
        expect_false(reference$checksum_verified_by_reader, info = mode)
        expect_identical(
            reference$local_sha256,
            checksum_file(path),
            info = mode
        )
        downloader <- Downloader$new(
            manifest = reference$cache_manifest,
            n_workers = 0L
        )
        tasks <- downloader$tasks(session_id = reference$session_id)
        expect_identical(tasks$task_id, reference$task_id, info = mode)
        expect_identical(
            reference$source_url,
            server$url("/files/source.nc"),
            info = mode
        )
        expect_identical(
            reference$request$file_key,
            store$query("SELECT file_key FROM file_catalog")$file_key,
            info = mode
        )
        priv(downloader)$disconnect_manifest()
    }
})

# Seed a pre-change Downloader manifest through its public API. A valid old
# partial must keep its original IDs, while unrelated completed tasks stay out
# of the requesting Store's source registration.
test_that("Store recovery adopts an exact legacy task without importing peers", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(recovery_test__app(bytes))
    withr::defer(server$stop())
    shared <- file.path(root, "shared")
    pool <- source_pool(1L, timeout = 15, download_dir = shared)
    withr::defer(pool$close(force = TRUE))
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        query <- store$add_files(recovery_test__files(
            path,
            server,
            checksum = checksum_file(path)
        ))
        file <- data.table::as.data.table(store$query(
            "SELECT * FROM file_catalog"
        ))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        downloader <- if (mode == "ordinary") {
            store$downloader(n_workers = 0L, cleanup = FALSE)
        } else {
            # This is the released pool directory layout, used here as a
            # backwards-readable fixture rather than the new locator.
            fields <- c(
                "source_id",
                "experiment_id",
                "variant_label",
                "grid_label",
                "table_id",
                "variable_id",
                "title",
                "version",
                "tracking_id",
                "checksum_type",
                "checksum"
            )
            key <- checksum_bytes(serialize(
                as.list(file[1L, fields, with = FALSE]),
                NULL
            ))
            dest <- file.path(shared, "_source_pool", key)
            Downloader$new(
                dest = dest,
                temp = file.path(dest, ".tmp"),
                manifest = file.path(dest, "manifest.duckdb"),
                n_workers = 0L,
                cleanup = FALSE
            )
        }
        session <- downloader$enqueue(data.frame(
            logical_file_id = "legacy-source",
            filename = file$filename,
            file_key = file$file_key,
            dataset_id = file$dataset_id,
            checksum = checksum_file(path),
            checksum_type = "sha256",
            size = length(bytes),
            url = server$url("/files/source.nc")
        ))
        task <- downloader$tasks(session_id = session)
        partial <- file.path(
            downloader$config$temp,
            paste0(task$task_id, ".part")
        )
        writeBin(bytes[1:1024], partial)
        foreign <- downloader$enqueue(data.frame(
            logical_file_id = "foreign-source",
            filename = "foreign.nc",
            file_key = "foreign-record",
            url = server$url("/files/source.nc"),
            checksum = checksum_file(path),
            checksum_type = "sha256"
        ))
        downloader$run(session_id = foreign, progress = FALSE)
        manifest <- downloader$manifest
        priv(downloader)$disconnect_manifest()
        result <- store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            result <- recovery_test__drain(pool, result)
        }
        expect_identical(
            result$status,
            "done",
            info = paste(mode, result$last_error)
        )
        references <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        expect_length(references, 1L)
        if (length(references) != 1L) {
            next
        }
        reference <- jsonlite::read_json(references[[1L]])
        expect_identical(
            reference$cache_manifest,
            normalizePath(manifest),
            info = mode
        )
        expect_identical(reference$session_id, session, info = mode)
        expect_identical(reference$task_id, task$task_id[[1L]], info = mode)
        expect_true(reference$checksum_verified_by_reader, info = mode)
        expect_identical(
            reference$local_sha256,
            checksum_file(path),
            info = mode
        )
        expect_false(any(
            store$query("SELECT file_key FROM artifact")$file_key ==
                "foreign-record",
            na.rm = TRUE
        ))
    }
})

# Verified replicas may share one native read, while every requesting catalogue
# row still receives its own exact receipt and source association.
test_that("one pooled recovery registers each verified replica request", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(recovery_test__app(bytes))
    mirror <- webfakes::new_app_process(recovery_test__app(bytes))
    withr::defer(server$stop())
    withr::defer(mirror$stop())
    docs <- data.table::rbindlist(list(
        esgf_test__file_docs(
            path,
            opendap_url = server$url("/dods/source.nc"),
            download_url = server$url("/files/source.nc")
        ),
        esgf_test__file_docs(
            path,
            opendap_url = mirror$url("/dods/source.nc"),
            download_url = mirror$url("/files/source.nc")
        )
    ))
    data.table::set(docs, j = "master_id", value = c("record-a", "record-b"))
    data.table::set(docs, j = "checksum", value = checksum_file(path))
    data.table::set(docs, j = "size", value = length(bytes))
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    query <- store$add_files(esgf_test__file_result(docs))
    plan <- store$plan_region(query, 104, 1.37, c("2060-01-01", "2060-01-03"))
    expect_equal(nrow(plan), 2L)
    pool <- source_pool(
        1L,
        timeout = 15,
        download_dir = file.path(root, "shared")
    )
    withr::defer(pool$close(force = TRUE))
    result <- recovery_test__drain(
        pool,
        store$extract(plan_id = plan$plan_id, fallback = "auto", pool = pool)
    )
    expect_true(
        all(result$status == "done"),
        info = paste(result$last_error, collapse = "\n")
    )
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    expect_length(references, 2L)
    receipts <- lapply(references, jsonlite::read_json)
    expect_setequal(
        vapply(receipts, function(x) x$request$file_key, character(1L)),
        store$query("SELECT file_key FROM file_catalog")$file_key
    )
    expect_length(
        unique(vapply(receipts, function(x) x$task_id, character(1L))),
        1L
    )
    expect_length(
        unique(vapply(receipts, function(x) x$session_id, character(1L))),
        1L
    )
    counts <- as.integer(rawToChar(
        curl::curl_fetch_memory(server$url("/counts"))$content
    )) +
        as.integer(rawToChar(
            curl::curl_fetch_memory(mirror$url("/counts"))$content
        ))
    expect_equal(counts, 1L)
})

# vim: fdm=marker :
