# Verified replicas of one version retain a shared cache even when catalogue
# records and HTTP resources differ; persisted lineage names the actual A bytes.
test_that("verified HTTP replicas reuse a warm cache across catalogue records", {
    run_case <- function(mode) {
        root <- withr::local_tempdir()
        cache <- file.path(root, "cache")
        withr::local_options(
            epwshiftr.cache = TRUE,
            epwshiftr.dir_cache = cache,
            epwshiftr.progress = FALSE,
            epwshiftr.mirai_workers = 1L
        )
        path <- file.path(
            root,
            "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc"
        )
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
        bytes <- readBin(path, "raw", file.info(path)$size)
        servers <- lapply(1:2, function(i) {
            webfakes::new_app_process(http_acceptance__app(bytes))
        })
        withr::defer(lapply(servers, function(server) server$stop()))
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
        values <- vector("list", 2L)
        lineage <- vector("list", 2L)
        for (index in 1:2) {
            store <- EsgStore$new(file.path(root, paste0("store-", index)))
            query <- store$add_files(recovery_test__files(
                path,
                servers[[index]],
                record = paste0("record-", index),
                checksum = checksum_file(path)
            ))
            plan <- store$plan_region(
                query,
                104,
                1.37,
                c("2060-01-01", "2060-01-03 23:59:59")
            )
            result <- store$extract(
                plan_id = plan$plan_id,
                fallback = "auto",
                pool = pool
            )
            if (!is.null(pool)) {
                result <- recovery_test__drain(pool, result)
            }
            expect_identical(result$status, "done", info = result$last_error)
            values[[index]] <- recovery_test__values(store)
            metadata <- store$query(
                "SELECT metadata_json FROM artifact WHERE kind = 'extract'"
            )$metadata_json
            lineage[[index]] <- jsonlite::fromJSON(
                metadata,
                simplifyVector = FALSE
            )$source_lineage
            store$close()
        }
        expect_identical(values[[2L]], values[[1L]])
        expect_identical(
            lineage[[2L]]$receipt$task_id,
            lineage[[1L]]$receipt$task_id
        )
        expect_identical(
            lineage[[2L]]$receipt$source_url,
            servers[[1L]]$url("/files/source.nc")
        )
        expect_identical(
            lineage[[2L]]$receipt$local_sha256,
            checksum_file(path)
        )
        expect_true(lineage[[2L]]$receipt$checksum_verified)
        expect_equal(nrow(http_acceptance__requests(servers[[1L]])), 1L)
        expect_length(http_acceptance__requests(servers[[2L]]), 0L)
        expect_length(list.files(cache, "[.]rds$", recursive = TRUE), 1L)
        http_acceptance__save(
            paste0("replicas-", mode),
            list(
                fixture_sha256 = checksum_file(path),
                values = values,
                lineage = lineage,
                producer_requests = http_acceptance__requests(servers[[1L]]),
                replica_requests = http_acceptance__requests(servers[[2L]])
            )
        )
    }
    for (mode in c("ordinary", "pooled")) {
        run_case(mode)
    }
})

# vim: fdm=marker :
