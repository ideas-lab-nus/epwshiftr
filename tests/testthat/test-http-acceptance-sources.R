# Couple bytes, endpoints, public task identity and values in one A/B matrix.
# Completed A also warms a shared cache; interrupted A occupies the old shared
# transfer layout, with one historical prefix deliberately lacking a sidecar.
test_that("Store HTTP recovery keeps completed and interrupted source owners independent", {
    run_case <- function(mode, same_record, retained) {
        withr::local_options(
            epwshiftr.cache = retained == "complete",
            epwshiftr.dir_cache = tempfile("http-acceptance-cache-"),
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        root <- withr::local_tempdir()
        first <- file.path(
            root,
            "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc"
        )
        second <- file.path(root, "second.nc")
        write_local_cmip6_netcdf_fixture(first, 2060L, calendar = "360_day")
        file.copy(first, second)
        nc <- RNetCDF::open.nc(second, write = TRUE)
        RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
        expected_b <- RNetCDF::var.get.nc(nc, "tas")[2L, 1L, 1:3]
        RNetCDF::close.nc(nc)
        nc <- RNetCDF::open.nc(first)
        expected_a <- RNetCDF::var.get.nc(nc, "tas")[2L, 1L, 1:3]
        RNetCDF::close.nc(nc)
        bytes_a <- readBin(first, "raw", file.info(first)$size)
        server_a <- webfakes::new_app_process(http_acceptance__app(
            bytes_a,
            partial = retained == "interrupted"
        ))
        server_b <- webfakes::new_app_process(http_acceptance__app(
            readBin(second, "raw", file.info(second)$size)
        ))
        withr::defer(server_a$stop())
        withr::defer(server_b$stop())
        shared <- file.path(root, "shared")
        pool <- if (mode == "pooled") {
            source_pool(1L, timeout = 15, download_dir = shared)
        }
        if (!is.null(pool)) {
            withr::defer(pool$close(force = TRUE))
        }
        a <- EsgStore$new(file.path(root, "store-a"))
        b <- EsgStore$new(file.path(root, "store-b"))
        withr::defer(a$close())
        withr::defer(b$close())
        query_a <- a$add_files(recovery_test__files(
            first,
            server_a,
            record = "record-a"
        ))
        query_b <- b$add_files(recovery_test__files(
            first,
            server_b,
            record = if (same_record) "record-a" else "record-b"
        ))
        file_a <- data.table::as.data.table(a$query(
            "SELECT * FROM file_catalog"
        ))
        file_b <- data.table::as.data.table(b$query(
            "SELECT * FROM file_catalog"
        ))
        expect_identical(file_a$file_key == file_b$file_key, same_record)
        if (retained == "complete") {
            plan_a <- a$plan_region(
                query_a,
                104,
                1.37,
                c("2060-01-01", "2060-01-03 23:59:59")
            )
            result_a <- a$extract(
                plan_id = plan_a$plan_id,
                fallback = "auto",
                pool = pool
            )
            if (!is.null(pool)) {
                result_a <- recovery_test__drain(pool, result_a)
            }
            expect_identical(
                result_a$status,
                "done",
                info = result_a$last_error
            )
            expect_equal(recovery_test__values(a), expected_a, tolerance = 0)
            receipt_a <- http_acceptance__receipt(a)
            manifest_a <- receipt_a$cache_manifest
            session_a <- receipt_a$session_id
            task_a <- receipt_a$task_id
            saved_a <- receipt_a$cache_path
            if (is.null(saved_a)) {
                saved_a <- receipt_a$path
            }
            sidecar_a <- list.files(
                dirname(manifest_a),
                paste0(task_a, "[.]json$"),
                full.names = TRUE,
                recursive = TRUE
            )
            dl <- Downloader$new(manifest = manifest_a, n_workers = 0L)
            sidecar_a <- unique(c(
                sidecar_a,
                file.path(dl$config$temp, paste0(task_a, ".transfer.json"))
            ))
            priv(dl)$disconnect_manifest()
            cache_a <- list.files(
                getOption("epwshiftr.dir_cache"),
                "[.]rds$",
                full.names = TRUE,
                recursive = TRUE
            )
            cache_sha_a <- vapply(cache_a, checksum_file, character(1L))
            output_a <- file.path(
                a$path,
                a$query("SELECT output_path FROM extraction_result")$output_path
            )
            output_sha_a <- checksum_file(output_a)
        } else {
            dl <- http_acceptance__legacy_downloader(
                b,
                file_a,
                if (!is.null(pool)) shared
            )
            session_a <- dl$enqueue(data.frame(
                logical_file_id = "retained-a",
                filename = file_a$filename,
                file_key = file_a$file_key,
                dataset_id = file_a$dataset_id,
                checksum_type = "sha256",
                checksum = NA_character_,
                size = length(bytes_a),
                url = server_a$url("/files/source.nc")
            ))
            result_a <- dl$run(session_a, progress = FALSE)
            expect_identical(result_a$status, "error")
            original <- dl$tasks(session_a)
            task_a <- original$task_id
            manifest_a <- dl$manifest
            saved_a <- file.path(dl$config$temp, paste0(task_a, ".part"))
            sidecar_a <- sub("[.]part$", ".transfer.json", saved_a)
            expect_identical(
                readBin(saved_a, "raw", length(bytes_a)),
                bytes_a[1:1024]
            )
            # This historical file cannot gain producer evidence merely because
            # a later unrelated request finds the same filename and directory.
            if (mode == "pooled" && !same_record) {
                unlink(sidecar_a)
                sidecar_a <- character()
            }
            priv(dl)$disconnect_manifest()
        }
        sha_a <- checksum_file(saved_a)
        sidecar_sha_a <- if (length(sidecar_a)) {
            vapply(sidecar_a, checksum_file, character(1L))
        } else {
            character()
        }
        requests_a <- http_acceptance__requests(server_a)
        plan_b <- b$plan_region(
            query_b,
            104,
            1.37,
            c("2060-01-01", "2060-01-03 23:59:59")
        )
        result_b <- b$extract(
            plan_id = plan_b$plan_id,
            fallback = "auto",
            pool = pool
        )
        if (!is.null(pool)) {
            result_b <- recovery_test__drain(pool, result_b)
        }
        expect_identical(
            result_b$status,
            "done",
            info = paste(mode, same_record, retained, result_b$last_error)
        )
        if (!identical(result_b$status, "done")) {
            return(invisible(NULL))
        }
        receipt_b <- http_acceptance__receipt(b)
        expect_equal(recovery_test__values(b), expected_b, tolerance = 0)
        expect_equal(
            recovery_test__values(b) - expected_a,
            rep(7, 3L),
            tolerance = 0
        )
        expect_identical(receipt_b$local_sha256, checksum_file(second))
        expect_false(receipt_b$checksum_verified_by_reader)
        expect_identical(receipt_b$source_url, server_b$url("/files/source.nc"))
        expect_identical(
            receipt_b$request$endpoint,
            server_b$url("/files/source.nc")
        )
        expect_identical(receipt_b$request$file_key, file_b$file_key)
        expect_false(identical(receipt_b$session_id, session_a))
        expect_false(identical(receipt_b$task_id, task_a))
        expect_false(identical(receipt_b$cache_path, saved_a))
        expect_identical(checksum_file(saved_a), sha_a)
        if (length(sidecar_a)) {
            expect_identical(
                vapply(sidecar_a, checksum_file, character(1L)),
                sidecar_sha_a
            )
        }
        expect_identical(http_acceptance__requests(server_a), requests_a)
        observed_b <- http_acceptance__requests(server_b)
        expect_equal(nrow(observed_b), 1L)
        expect_identical(observed_b$range, "")
        if (retained == "complete") {
            expect_identical(checksum_file(output_a), output_sha_a)
            expect_identical(
                vapply(cache_a, checksum_file, character(1L)),
                cache_sha_a
            )
            expect_length(
                list.files(
                    getOption("epwshiftr.dir_cache"),
                    "[.]rds$",
                    recursive = TRUE
                ),
                2L
            )
        }

        dl <- Downloader$new(
            manifest = receipt_b$cache_manifest,
            n_workers = 0L
        )
        task_b <- dl$tasks(session_id = receipt_b$session_id)
        expect_identical(task_b$task_id, receipt_b$task_id)
        expect_identical(
            task_b$transfer_request_url,
            server_b$url("/files/source.nc")
        )
        expect_identical(task_b$transfer_sha256, checksum_file(second))
        expect_identical(task_b$target_path, receipt_b$cache_path)
        priv(dl)$disconnect_manifest()
        http_acceptance__save(
            paste(mode, same_record, retained, sep = "-"),
            list(
                fixture_a_sha256 = checksum_file(first),
                fixture_b_sha256 = checksum_file(second),
                retained_a_sha256 = sha_a,
                retained_sidecar_sha256 = sidecar_sha_a,
                a_session = session_a,
                a_task = task_a,
                a_path = saved_a,
                a_manifest = manifest_a,
                a_requests = requests_a,
                b_requests = observed_b,
                b_receipt = receipt_b,
                b_task = task_b,
                b_values = recovery_test__values(b)
            )
        )
    }
    for (mode in c("ordinary", "pooled")) {
        for (same_record in c(TRUE, FALSE)) {
            for (retained in c("complete", "interrupted")) {
                run_case(mode, same_record, retained)
            }
        }
    }
})

# vim: fdm=marker :
