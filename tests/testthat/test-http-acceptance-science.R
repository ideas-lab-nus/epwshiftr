# Compare HTTP adapters with both direct native Store output and independent
# NetCDF coordinates/values across the non-Gregorian February boundary.
test_that("HTTP fallback preserves native calendars bounds missingness and site cells", {
    withr::local_options(
        epwshiftr.cache = FALSE,
        epwshiftr.progress = FALSE,
        epwshiftr.mirai_workers = 1L
    )
    for (calendar in c("360_day", "365_day")) {
        root <- withr::local_tempdir()
        path <- file.path(root, "source.nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = calendar)
        indices <- if (calendar == "360_day") 58:62 else 59:61
        nc <- RNetCDF::open.nc(path, write = TRUE)
        RNetCDF::var.put.nc(
            nc,
            "tas",
            NA_real_,
            start = c(2L, 1L, indices[[2L]]),
            count = c(1L, 1L, 1L)
        )
        RNetCDF::var.put.nc(
            nc,
            "tas",
            NA_real_,
            start = c(4L, 3L, tail(indices, 1L)),
            count = c(1L, 1L, 1L)
        )
        native <- list(
            values = RNetCDF::var.get.nc(nc, "tas"),
            time = RNetCDF::var.get.nc(nc, "time"),
            bounds = RNetCDF::var.get.nc(nc, "time_bnds"),
            units = RNetCDF::att.get.nc(nc, "tas", "units"),
            calendar = RNetCDF::att.get.nc(nc, "time", "calendar")
        )
        RNetCDF::close.nc(nc)
        sites <- data.frame(
            site_id = c("a", "b"),
            lon = c(103.98, -106),
            lat = c(1.37, 41)
        )
        time <- c("2060-02-28", "2060-03-02 23:59:59")
        local <- EsgStore$new(file.path(root, "native"))
        docs <- esgf_test__file_docs(path)
        data.table::set(docs, j = "checksum", value = checksum_file(path))
        query <- local$add_files(esgf_test__file_result(docs))
        plan <- local$plan_regions(query, sites, time)
        expect_true(all(
            local$extract(plan_id = plan$plan_id, fallback = "error")$status ==
                "done"
        ))
        reference <- http_acceptance__rows(local)
        local$close()
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
            plan <- store$plan_regions(query, sites, time)
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
            expect_true(
                all(result$status == "done"),
                info = paste(result$last_error, collapse = "\n")
            )
            observed <- http_acceptance__rows(store)
            expect_identical(observed, reference)
            expected <- c(
                native$values[2L, 1L, indices],
                native$values[4L, 3L, indices]
            )
            expect_identical(is.na(observed$value), is.na(expected))
            expect_equal(observed$value, expected, tolerance = 0)
            expect_identical(
                observed$site_id,
                rep(c("a", "b"), each = length(indices))
            )
            expect_identical(unique(observed$cf_calendar), native$calendar)
            expect_identical(unique(observed$units), native$units)
            expect_identical(observed$cf_year, rep(2060L, length(expected)))
            expect_identical(
                observed$cf_month,
                rep(
                    if (calendar == "360_day") {
                        c(2L, 2L, 2L, 3L, 3L)
                    } else {
                        c(2L, 3L, 3L)
                    },
                    2L
                )
            )
            expect_identical(
                observed$cf_day,
                rep(
                    if (calendar == "360_day") {
                        c(28L, 29L, 30L, 1L, 2L)
                    } else {
                        c(28L, 1L, 2L)
                    },
                    2L
                )
            )
            expect_equal(
                observed$cf_second_of_day,
                rep(43200, length(expected))
            )
            origin <- as.POSIXct("2060-01-01", tz = "UTC")
            expect_equal(
                observed$time,
                origin + rep(native$time[indices], 2L) * 86400
            )
            expect_equal(
                observed$time_bound_start,
                origin + rep(native$bounds[1L, indices], 2L) * 86400
            )
            expect_equal(
                observed$time_bound_end,
                origin + rep(native$bounds[2L, indices], 2L) * 86400
            )
            cells <- store$query(
                "SELECT p.site_id,g.grid_lon,g.grid_lat FROM extraction_grid_source g JOIN extraction_plan p USING(plan_id) ORDER BY p.site_id,g.source_index"
            )
            expect_identical(cells$site_id, c("a", "b"))
            expect_equal(cells$grid_lon, c(104, 254))
            expect_equal(cells$grid_lat, c(1, 41))
            http_acceptance__save(
                paste("science", calendar, mode, sep = "-"),
                list(
                    fixture_sha256 = checksum_file(path),
                    native_time = native$time[indices],
                    native_bounds = native$bounds[, indices],
                    expected_values = expected,
                    observed = observed,
                    cells = cells,
                    receipt = http_acceptance__receipt(store),
                    observed_requests = http_acceptance__requests(server)
                )
            )
            if (!is.null(pool)) {
                pool$close(force = TRUE)
            }
            store$close()
        }
        server$stop()
    }
})

# vim: fdm=marker :
