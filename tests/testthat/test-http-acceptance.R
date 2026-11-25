# Publisher proof permits two HTTP resources to supply pieces; resource-local
# validators must not be promoted to a validator for the assembled file.
test_that("HTTP piece receipts identify every actual contributing resource", {
    skip_on_cran()
    bytes <- as.raw(rep(0:255, 16L))
    server <- webfakes::new_app_process(http_acceptance__app(bytes))
    withr::defer(server$stop())
    urls <- c(server$url("/files/first.bin"), server$url("/files/second.bin"))
    for (workers in c(0L, 1L)) {
        root <- withr::local_tempdir()
        dl <- Downloader$new(
            dest = file.path(root, "dest"),
            temp = file.path(root, "temp"),
            manifest = file.path(root, "manifest.duckdb"),
            n_workers = workers,
            retries = 1L,
            transfer_policy = list(
                range_mode = "multi",
                piece_size = 1024L,
                piece_concurrency = 2L,
                max_sources = 2L
            )
        )
        session <- dl$enqueue(data.frame(
            logical_file_id = "verified-replicas",
            filename = "data.bin",
            url = urls,
            checksum_type = "sha256",
            checksum = checksum_bytes(bytes),
            data_node = c("node-a", "node-b")
        ))
        result <- dl$run(session, progress = FALSE)
        expect_identical(result$status, "done", info = result$last_error)
        if (!identical(result$status, "done")) {
            print(http_acceptance__requests(server))
            next
        }
        task <- dl$tasks(session)
        expect_setequal(jsonlite::fromJSON(task$transfer_sources), urls)
        expect_identical(task$transfer_origin, "get-pieces")
        expect_false(task$transfer_validator_bound)
        expect_true(task$transfer_checksum_verified)
        expect_true(task$transfer_complete)
        expect_identical(task$transfer_sha256, checksum_bytes(bytes))
        expect_identical(
            readBin(result$target_path, "raw", length(bytes)),
            bytes
        )
        requests <- http_acceptance__requests(server)
        actual <- requests[requests$range != "bytes=0-0", , drop = FALSE]
        expect_setequal(
            unique(actual$path),
            c("/files/first.bin", "/files/second.bin")
        )
        http_acceptance__save(
            paste0("pieces-", workers),
            list(
                fixture_sha256 = checksum_bytes(bytes),
                task = task,
                observed_requests = requests,
                output_sha256 = checksum_file(result$target_path)
            )
        )
        priv(dl)$disconnect_manifest()
    }
})

# A successful native download may still fail to provide the requested variable.
# Keep both original service addresses, the actual local read target and cause.
test_that("HTTP fallback failures retain both service addresses and read context", {
    withr::local_options(
        epwshiftr.cache = FALSE,
        epwshiftr.progress = FALSE,
        epwshiftr.mirai_workers = 1L
    )
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = "huss")
    bytes <- readBin(path, "raw", file.info(path)$size)
    server <- webfakes::new_app_process(http_acceptance__app(bytes))
    withr::defer(server$stop())
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        query <- store$add_files(recovery_test__files(
            path,
            server,
            checksum = checksum_file(path)
        ))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
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
        result <- store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = pool
        )
        if (!is.null(pool)) {
            result <- recovery_test__drain(pool, result)
        }
        expect_identical(result$status, "failed", info = mode)
        expect_match(result$last_error, "OPeNDAP open failed", info = mode)
        expect_match(
            result$last_error,
            server$url("/dods/source.nc"),
            fixed = TRUE,
            info = mode
        )
        expect_match(result$last_error, "HTTPServer read failed", info = mode)
        expect_match(
            result$last_error,
            server$url("/files/source.nc"),
            fixed = TRUE,
            info = mode
        )
        expect_match(
            result$last_error,
            "source[.]nc.*(variable|Variable)|[Vv]ariable.*tas",
            info = mode
        )
        expect_equal(nrow(store$query("SELECT * FROM extraction_result")), 0L)
        dap_requests <- http_acceptance__requests(server, "/dap-requests")
        expect_gt(nrow(dap_requests), 0L)

        http_acceptance__save(
            paste0("failures-", mode),
            list(
                source_sha256 = checksum_file(path),
                result = result,
                observed_requests = http_acceptance__requests(server)
            )
        )
    }
})

# vim: fdm=marker :
