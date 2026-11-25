# A new publisher expectation constrains the same catalogue resource even when
# its original GET was legitimately acquired without a publisher checksum.
test_that("completed HTTP results reject a newly contradictory publisher checksum", {
    run_case <- function(mode) {
        withr::local_options(
            epwshiftr.cache = FALSE,
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        root <- withr::local_tempdir()
        path <- file.path(root, "source.nc")
        changed <- file.path(root, "changed.nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
        file.copy(path, changed)
        nc <- RNetCDF::open.nc(changed, write = TRUE)
        RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
        RNetCDF::close.nc(nc)
        source_sha <- checksum_file(path)
        server <- webfakes::new_app_process(http_acceptance__app(
            readBin(path, "raw", file.info(path)$size)
        ))
        withr::defer(server$stop())
        store <- EsgStore$new(file.path(root, "store"))
        withr::defer(store$close())
        query <- store$add_files(recovery_test__files(path, server))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03 23:59:59")
        )
        pool <- if (mode == "pooled") {
            source_pool(
                1L,
                timeout = 15,
                download_dir = file.path(root, "shared")
            )
        }
        if (!is.null(pool)) {
            withr::defer(pool$close(force = TRUE))
        }
        result <- store$extract(plan_id = plan$plan_id, pool = pool)
        if (!is.null(pool)) {
            result <- durable_test__drain(pool, result)
        }
        expect_identical(result$status, "done", info = result$last_error)
        receipt <- http_acceptance__receipt(store)
        expect_false(receipt$checksum_verified_by_reader)
        expect_identical(receipt$local_sha256, source_sha)
        output <- file.path(
            store$path,
            store$query("SELECT output_path FROM extraction_result")$output_path
        )
        before <- checksum_file(output)
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        requests <- http_acceptance__requests(server)
        current <- recovery_test__files(
            path,
            server,
            checksum = checksum_file(changed)
        )
        original_key <- store$query(
            "SELECT file_key FROM file_catalog"
        )$file_key
        store$add_files(current)
        expect_identical(
            store$query("SELECT file_key FROM file_catalog")$file_key,
            original_key
        )
        unlink(receipt$cache_path)
        server$stop()
        reused <- store$extract(plan_id = plan$plan_id, pool = pool)
        if (!is.null(pool)) {
            reused <- durable_test__drain(pool, reused)
        }
        expect_identical(reused$status, "failed", info = mode)
        expect_match(
            reused$last_error,
            "source.*(conflict|changed)|checksum",
            ignore.case = TRUE
        )
        expect_identical(checksum_file(output), before)
        expect_identical(
            store$query("SELECT * FROM artifact WHERE kind = 'extract'"),
            artifact
        )
        expect_identical(http_acceptance__receipt(store), receipt)
        http_acceptance__save(
            paste0("checksum-upgrade-mismatch-", mode),
            list(
                original_source_sha256 = source_sha,
                current_publisher_sha256 = checksum_file(changed),
                request_log_before_stop = requests,
                result = reused,
                original_receipt = receipt,
                retained_output_sha256 = checksum_file(output)
            )
        )
    }
    for (mode in c("ordinary", "pooled")) {
        run_case(mode)
    }
})

# A saved GET SHA can prove a matching new SHA offline without rewriting history.
# A newly required MD5 needs intact original native bytes when no MD5 was saved.
test_that("HTTP checksum upgrades use available proof and preserve historical verification", {
    run_case <- function(mode, initially_verified) {
        withr::local_options(
            epwshiftr.cache = FALSE,
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        root <- withr::local_tempdir()
        path <- file.path(root, "source.nc")
        write_local_cmip6_netcdf_fixture(path, 2060L)
        sha <- checksum_file(path)
        md5 <- checksum_file(path, "md5")
        server <- webfakes::new_app_process(http_acceptance__app(readBin(
            path,
            "raw",
            file.info(path)$size
        )))
        withr::defer(server$stop())
        opendap <- server$url("/dods/source.nc")
        download <- server$url("/files/source.nc")
        store <- EsgStore$new(file.path(root, "store"))
        withr::defer(store$close())
        query <- store$add_files(content_constraints__files(
            path,
            opendap,
            download,
            if (initially_verified) sha else NA_character_
        ))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03 23:59:59")
        )
        pool <- if (mode == "pooled") {
            source_pool(
                1L,
                timeout = 15,
                download_dir = file.path(root, "shared")
            )
        }
        if (!is.null(pool)) {
            withr::defer(pool$close(force = TRUE))
        }
        first <- durable_test__drain(
            pool,
            store$extract(plan_id = plan$plan_id, pool = pool)
        )
        expect_identical(first$status, "done", info = first$last_error)
        receipt <- http_acceptance__receipt(store)
        output <- file.path(
            store$path,
            store$query("SELECT output_path FROM extraction_result")$output_path
        )
        output_sha <- checksum_file(output)
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        values <- recovery_test__values(store)
        expect_identical(
            receipt$checksum_verified_by_reader,
            initially_verified
        )
        current_md5 <- content_constraints__files(
            path,
            opendap,
            download,
            md5,
            "md5"
        )
        current_sha <- content_constraints__files(path, opendap, download, sha)
        server$stop()
        store$add_files(current_md5)
        with_native <- durable_test__drain(
            pool,
            store$extract(plan_id = plan$plan_id, pool = pool)
        )
        expect_identical(
            with_native$status,
            "done",
            info = with_native$last_error
        )
        expect_identical(http_acceptance__receipt(store), receipt)
        unlink(receipt$cache_path)
        store$add_files(current_sha)
        matching_sha <- durable_test__drain(
            pool,
            store$extract(plan_id = plan$plan_id, pool = pool)
        )
        expect_identical(
            matching_sha$status,
            "done",
            info = matching_sha$last_error
        )
        store$add_files(current_md5)
        without_native <- durable_test__drain(
            pool,
            store$extract(plan_id = plan$plan_id, pool = pool)
        )
        expect_identical(without_native$status, "failed", info = mode)
        expect_match(
            without_native$last_error,
            "insufficient.*current publisher checksum"
        )
        expect_identical(http_acceptance__receipt(store), receipt)
        expect_identical(
            http_acceptance__receipt(store)$checksum_verified_by_reader,
            initially_verified
        )
        expect_identical(checksum_file(output), output_sha)
        expect_identical(recovery_test__values(store), values)
        expect_identical(
            store$query("SELECT * FROM artifact WHERE kind = 'extract'"),
            artifact
        )
        http_acceptance__save(
            paste(
                "checksum-upgrade-proof",
                mode,
                initially_verified,
                sep = "-"
            ),
            list(
                original_sha256 = sha,
                new_md5 = md5,
                intact_native = with_native,
                unavailable_algorithm = without_native,
                matching_sha256 = matching_sha,
                original_receipt = receipt,
                retained_output_sha256 = checksum_file(output)
            )
        )
    }
    for (mode in c("ordinary", "pooled")) {
        for (initially_verified in c(FALSE, TRUE)) {
            run_case(mode, initially_verified)
        }
    }
})

# Added native constraints must be checked against the saved full-file digest,
# without extending first-read or unchanged OPeNDAP selection policy.
test_that("saved native results check newly added publisher constraints offline", {
    run_case <- function(mode, constraint) {
        withr::local_options(
            epwshiftr.cache = FALSE,
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        root <- withr::local_tempdir()
        path <- file.path(root, "source.nc")
        write_local_cmip6_netcdf_fixture(path, 2060L)
        sha <- checksum_file(path)
        store <- EsgStore$new(file.path(root, "store"))
        withr::defer(store$close())
        query <- store$add_files(content_constraints__files(path, path, path))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03 23:59:59")
        )
        pool <- if (mode == "pooled") source_pool(1L, timeout = 15)
        if (!is.null(pool)) {
            withr::defer(pool$close(force = TRUE))
        }
        first <- durable_test__drain(
            pool,
            store$extract(
                plan_id = plan$plan_id,
                fallback = "error",
                pool = pool
            )
        )
        expect_identical(first$status, "done", info = first$last_error)
        output <- file.path(
            store$path,
            store$query("SELECT output_path FROM extraction_result")$output_path
        )
        before <- checksum_file(output)
        artifact <- store$query("SELECT * FROM artifact WHERE kind = 'extract'")
        lineage <- jsonlite::fromJSON(
            artifact$metadata_json,
            simplifyVector = FALSE
        )$source_lineage
        expect_identical(lineage$native_sha256, sha)
        wanted <- switch(
            constraint,
            mismatch = checksum_bytes(charToRaw("different native content")),
            unavailable = checksum_file(path, "md5"),
            matched = sha
        )
        current <- content_constraints__files(
            path,
            path,
            path,
            wanted,
            if (constraint == "unavailable") "md5" else "sha256"
        )
        unlink(path)
        store$add_files(current)
        result <- durable_test__drain(
            pool,
            store$extract(
                plan_id = plan$plan_id,
                fallback = "error",
                pool = pool
            )
        )
        if (constraint == "matched") {
            expect_identical(result$status, "done", info = result$last_error)
        } else {
            expect_identical(
                result$status,
                "failed",
                info = paste(mode, constraint)
            )
            expect_match(
                result$last_error,
                if (constraint == "unavailable") {
                    "insufficient.*current publisher checksum"
                } else {
                    "source.*(conflict|changed)|checksum"
                },
                ignore.case = TRUE
            )
        }
        expect_identical(checksum_file(output), before)
        expect_identical(
            store$query("SELECT * FROM artifact WHERE kind = 'extract'"),
            artifact
        )
        http_acceptance__save(
            paste("native-checksum-upgrade", mode, constraint, sep = "-"),
            list(
                original_sha256 = sha,
                current_checksum = wanted,
                saved_lineage = lineage,
                result = result,
                retained_output_sha256 = before
            )
        )
    }
    for (mode in c("ordinary", "pooled")) {
        for (constraint in c("mismatch", "unavailable", "matched")) {
            run_case(mode, constraint)
        }
    }
})

# vim: fdm=marker :
