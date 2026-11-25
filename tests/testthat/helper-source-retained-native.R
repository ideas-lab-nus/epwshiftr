# Exercise rejected external native bytes through the public pooled Store seam.
# Saved task rows and producer sidecars are readback evidence, never test hooks.
retained_native_test__check <- function(publisher) {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    source_hash <- checksum_file(path)
    server <- webfakes::new_app_process(recovery_test__app(readBin(
        path,
        "raw",
        n = file.info(path)$size
    )))
    withr::defer(server$stop())
    pool <- source_pool(
        1L,
        timeout = 15,
        download_dir = file.path(root, "shared")
    )
    withr::defer(pool$close(force = TRUE))
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    query <- store$add_files(recovery_test__files(
        path,
        server,
        checksum = if (publisher) source_hash else NA_character_
    ))
    initial <- store$plan_region(
        query,
        104,
        1.37,
        c("2060-01-01", "2060-01-03")
    )
    first <- recovery_test__drain(
        pool,
        store$extract(plan_id = initial$plan_id, pool = pool)
    )
    expect_identical(first$status, "done", info = first$last_error)
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    expect_length(references, 1L)
    reference_path <- references[[1L]]
    reference <- jsonlite::read_json(reference_path, simplifyVector = TRUE)
    native <- reference$cache_path
    inspect <- Downloader$new(
        manifest = reference$cache_manifest,
        n_workers = 0L
    )
    before <- inspect$tasks(session_id = reference$session_id)
    proof <- file.path(
        inspect$config$temp,
        paste0(reference$task_id, ".transfer.json")
    )
    priv(inspect)$disconnect_manifest()
    expect_true(file.exists(proof))
    proof_hash <- checksum_file(proof)
    reference_hash <- checksum_file(reference_path)
    expect_identical(before$transfer_sha256, source_hash)
    expect_true(before$transfer_complete)
    expect_identical(before$task_id, reference$task_id)
    expect_identical(before$target_path, native)
    expect_identical(before$selected_url, server$url("/files/source.nc"))
    initial_gets <- as.integer(rawToChar(
        curl::curl_fetch_memory(server$url("/counts"))$content
    ))
    expect_identical(initial_gets, 1L)

    # Keep a valid NetCDF with changed values, so a later successful extraction
    # would expose replacement or wrong-source reuse rather than a parser error.
    nc <- RNetCDF::open.nc(native, write = TRUE)
    RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
    RNetCDF::close.nc(nc)
    retained_hash <- checksum_file(native)
    expect_false(identical(retained_hash, source_hash))
    next_plan <- store$plan_region(
        query,
        104.01,
        1.37,
        c("2060-01-01", "2060-01-03")
    )
    defaults <- vector("list", 2L)
    for (attempt in seq_len(2L)) {
        defaults[[attempt]] <- recovery_test__drain(
            pool,
            store$extract(plan_id = next_plan$plan_id, pool = pool)
        )
        expect_identical(
            defaults[[attempt]]$status,
            "failed",
            info = paste("attempt", attempt)
        )
        expect_match(
            defaults[[attempt]]$last_error,
            "source_changed|checksum|integrity",
            info = paste("attempt", attempt)
        )
        expect_identical(
            checksum_file(native),
            retained_hash,
            info = paste("attempt", attempt)
        )
        expect_identical(
            checksum_file(proof),
            proof_hash,
            info = paste("attempt", attempt)
        )
        expect_identical(
            checksum_file(reference_path),
            reference_hash,
            info = paste("attempt", attempt)
        )
        expect_identical(
            as.integer(rawToChar(
                curl::curl_fetch_memory(server$url("/counts"))$content
            )),
            initial_gets,
            info = paste("attempt", attempt)
        )
    }
    repaired <- recovery_test__drain(
        pool,
        store$extract(
            plan_id = next_plan$plan_id,
            pool = pool,
            overwrite = TRUE
        )
    )
    expect_identical(repaired$status, "done", info = repaired$last_error)
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    replacement_paths <- setdiff(references, reference_path)
    expect_length(replacement_paths, 1L)
    replacement <- if (length(replacement_paths) == 1L) {
        jsonlite::read_json(replacement_paths[[1L]], simplifyVector = TRUE)
    } else {
        NULL
    }
    replacement_task <- NULL
    if (!is.null(replacement)) {
        expect_false(identical(replacement$cache_path, native))
        expect_false(identical(replacement$task_id, reference$task_id))
        expect_false(identical(replacement$session_id, reference$session_id))
        expect_identical(replacement$local_sha256, source_hash)
        expect_identical(checksum_file(replacement$cache_path), source_hash)
        expect_identical(replacement$source_url, server$url("/files/source.nc"))
        expect_identical(replacement$checksum_verified_by_reader, publisher)
        expect_identical(
            replacement$request$file_key,
            reference$request$file_key
        )
        inspect <- Downloader$new(
            manifest = reference$cache_manifest,
            n_workers = 0L
        )
        expect_identical(
            inspect$tasks(session_id = reference$session_id),
            before
        )
        replacement_task <- inspect$tasks(session_id = replacement$session_id)
        expect_identical(replacement_task$target_path, replacement$cache_path)
        expect_identical(replacement_task$transfer_sha256, source_hash)
        expect_true(replacement_task$transfer_complete)
        priv(inspect)$disconnect_manifest()
    }
    expect_identical(checksum_file(native), retained_hash)
    expect_identical(checksum_file(proof), proof_hash)
    expect_identical(checksum_file(reference_path), reference_hash)
    expect_identical(
        as.integer(rawToChar(
            curl::curl_fetch_memory(server$url("/counts"))$content
        )),
        initial_gets + 1L
    )
    output <- store$query(sprintf(
        "SELECT output_path FROM extraction_result WHERE plan_id = '%s'",
        next_plan$plan_id
    ))
    expect_identical(nrow(output), 1L)
    actual <- expected <- output_hash <- NULL
    if (nrow(output) == 1L && identical(repaired$status, "done")) {
        output_hash <- checksum_file(file.path(store$path, output$output_path))
        actual <- store$query(paste0(
            "SELECT value FROM read_parquet('",
            file.path(store$path, output$output_path),
            "') ORDER BY cf_year,cf_month,cf_day,cf_second_of_day"
        ))$value
        nc <- RNetCDF::open.nc(path)
        expected <- as.double(RNetCDF::var.get.nc(
            nc,
            "tas",
            start = c(2L, 1L, 1L),
            count = c(1L, 1L, 2L)
        ))
        RNetCDF::close.nc(nc)
        expect_equal(actual, expected, tolerance = 1e-10)
    }
    durable_test__record(
        paste0(
            "external-native-retention-",
            if (publisher) "publisher" else "local"
        ),
        list(
            source_hash = source_hash,
            retained_hash = retained_hash,
            proof_hash = proof_hash,
            original = reference,
            original_task = before,
            defaults = defaults,
            repaired = repaired,
            replacement = replacement,
            replacement_task = replacement_task,
            actual = actual,
            expected = expected,
            output_hash = output_hash,
            gets = as.integer(rawToChar(
                curl::curl_fetch_memory(server$url("/counts"))$content
            ))
        ),
        c(path, native, proof, reference_path, replacement_paths)
    )
}

# vim: fdm=marker :
