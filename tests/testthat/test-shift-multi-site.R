# Serialize test inputs through the same JSON boundary as user configs.
multi_site__write_config <- function(config) {
    path <- tempfile(fileext = ".json")
    jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
    path
}

# Create a genuine second EPW header while retaining deterministic weather data.
multi_site__epw <- function() {
    path <- tempfile(fileext = ".epw")
    lines <- readLines(get_cache_epw())
    header <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
    header[c(2L, 7L, 8L, 9L, 10L)] <- c(
        "Second city",
        "45.75",
        "126.63",
        "8",
        "140"
    )
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    path
}

# Keep discovery deterministic but exercise public planning and persisted plans.
multi_site__plan <- function(
    sites,
    store = tempfile("multi-site-store-"),
    ...
) {
    shift_future_epw(
        sites = sites,
        climate = shift_cmip6(model = 1L, scenarios = c("ssp126", "ssp585")),
        periods = list(mid = 2050:2051),
        methods = c("original_morphing", "bws_btws"),
        dir = tempfile("multi-site-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none"),
        ...
    )
}

# Keep a failed shared source read visible in the saved batch even when no
# child run was started, so the user can inspect and resume that batch.
test_that("shared prefetch failure persists as a blocked batch", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    root <- tempfile("blocked-shared-batch-")
    batch <- multi_site__plan(
        list(shift_site("South", epw = get_cache_epw())),
        store = root
    )
    # This test isolates failure persistence after input resolution. The input
    # resolver itself is exercised with real local catalogs in shared tests.
    testthat::local_mocked_bindings(
        shift_batch__resolve_inputs = function(batch, reporter = NULL) batch,
        shift_batch__prefetch = function(...) stop("source connection closed")
    )
    expect_error(shift_batch__resume(batch), "source connection closed")
    restored <- shift_batch_get(batch@ids$batch_id, root)
    expect_identical(shift_status(restored), "blocked")
    expect_true(
        "batch_shared_read_failed" %in%
            shift_diagnostics(restored)$code
    )
    expect_length(restored@meta$shared_failure_history, 1L)
})

test_that("multiple sites share discovery and retain distinct durable plans", {
    calls <- 0L
    test_local_dependencies(list(
        availability = function(...) {
            calls <<- calls + 1L
            test_cmip6_availability(...)
        },
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    sites <- list(
        shift_site("South", epw = get_cache_epw()),
        shift_site("North", epw = multi_site__epw())
    )
    original <- sites
    root <- tempfile("multi-site-persistence-")
    batch <- multi_site__plan(sites, root)
    expect_identical(sites, original)
    expect_s7_class(batch, ShiftBatch)
    manifest <- batch@meta$manifest
    expect_identical(manifest$site_id, rep(c("North", "South"), each = 2L))
    expect_identical(
        manifest$method,
        rep(c("original_morphing", "bws_btws"), 2L)
    )
    expect_equal(data.table::uniqueN(manifest$store), 4L)
    expect_equal(data.table::uniqueN(manifest$output_dir), 4L)
    expect_s3_class(batch@meta$shared_plan$acquisitions, "data.table")
    expect_s3_class(batch@meta$shared_plan$consumers, "data.table")
    expect_s3_class(batch@meta$shared_plan$unmatched, "data.table")
    expect_equal(nrow(shift_cases(batch)), 8L)
    expect_identical(unique(shift_cases(batch)$site_id), c("North", "South"))
    child_sites <- lapply(batch@meta$children, function(child) child@meta$site)
    expect_equal(child_sites[[1L]]@lon, 126.63)
    expect_equal(child_sites[[1L]]@lat, 45.75)
    expect_identical(child_sites[[1L]]@id, "North")
    expect_identical(child_sites[[3L]]@id, "South")
    expect_false(identical(child_sites[[1L]]@lon, child_sites[[3L]]@lon))

    first_calls <- calls
    single <- multi_site__plan(sites[1L])
    expect_identical(calls, first_calls * 2L)
    expect_length(single@meta$children, 2L)
    # Input ordering does not invalidate the saved discovery.
    calls <- 0L
    reordered <- multi_site__plan(sites[2:1], root)
    expect_identical(reordered@ids$batch_id, batch@ids$batch_id)
    expect_identical(calls, 0L)
    reopened <- shift_batch_get(batch@ids$batch_id, root)
    expect_identical(reopened@meta$manifest, reordered@meta$manifest)
    expect_identical(reopened@meta$periods, reordered@meta$periods)
    expect_identical(reopened@meta$climate, reordered@meta$climate)
    expect_identical(reopened@meta$shared_plan, reordered@meta$shared_plan)
    expect_identical(
        lapply(reopened@meta$children, shift__plan_spec),
        lapply(reordered@meta$children, shift__plan_spec)
    )
    expect_identical(calls, 0L)
    snapshot <- shift_batch__snapshot(reopened, refresh = FALSE)
    expect_setequal(snapshot$children$site_id, c("North", "South"))
    expect_setequal(shift_summary(reopened)$site_id, c("North", "South"))
})

test_that("site objects preserve coordinates, metadata and EPW identities", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    epw <- get_cache_epw()
    sites <- list(
        shift_site(
            "a/b",
            lon = 110,
            lat = 30,
            epw = epw,
            metadata = list(note = "target")
        ),
        shift_site("a-b", lon = 120, lat = 40, epw = epw)
    )
    batch <- multi_site__plan(sites)
    expect_length(batch@meta$children, 4L)
    expect_equal(
        data.table::uniqueN(dirname(batch@meta$manifest$output_dir)),
        2L
    )
    child <- batch@meta$children[[3L]]
    expect_identical(child@meta$site@metadata, list(note = "target"))
    expect_identical(
        shift__plan_from_spec(shift__plan_spec(child))@meta$site@metadata,
        list(note = "target")
    )
    original <- batch@ids$batch_id
    sites[[1L]]@lon <- 111
    expect_false(identical(multi_site__plan(sites)@ids$batch_id, original))
    sites[[1L]]@lon <- 110
    sites[[1L]]@id <- "changed"
    expect_false(identical(multi_site__plan(sites)@ids$batch_id, original))
    sites[[1L]]@id <- "a/b"
    sites[[1L]]@epw <- multi_site__epw()
    expect_false(identical(multi_site__plan(sites)@ids$batch_id, original))
})

test_that("site constructors and collections reject invalid inputs before discovery", {
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    epw <- get_cache_epw()
    site <- shift_site("A", epw = epw)
    expect_error(multi_site__plan(epw), "shift_site")
    expect_error(
        multi_site__plan(data.table::data.table(id = "A", epw = epw)),
        "shift_site"
    )
    expect_error(multi_site__plan(list()), "non-empty")
    expect_error(multi_site__plan(list(site, site)), "duplicated")
    expect_error(multi_site__plan(shift_site(" ", epw = epw)), "whitespace")
    expect_error(shift_site(NA_character_, epw = epw), "NA|missing")
    expect_error(
        shift_site("A", lon = NA_real_, lat = NA_real_, epw = epw),
        "NA|missing"
    )
    expect_error(shift_site("A", lat = 91, epw = epw), "<= 90")
    expect_error(multi_site__plan(shift_site("A", 110, 30)), "baseline EPW")
    expect_error(
        shift_site("A", epw = "/missing/baseline.epw"),
        "does not exist"
    )
})

test_that("site defaults are independent of explicitly supplied coordinates", {
    epw <- get_cache_epw()
    derived <- shift_site("A", epw = epw)
    explicit <- shift_site("A", derived@lon, derived@lat, epw = epw)
    expect_identical(explicit, derived)
    sites <- shift_batch__sites(list(
        explicit,
        shift_site("B", 110, 30, epw = epw)
    ))
    expect_s3_class(sites, "data.table")
    expect_identical(sites$site[[2L]]@label, derived@label)
    expect_equal(sites$site[[2L]]@lon, 110)
})

test_that("single and multiple site calls use one batch contract", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    site <- shift_site("A", epw = get_cache_epw())
    root <- tempfile()
    single <- multi_site__plan(site, root)
    wrapped <- multi_site__plan(list(site), root)
    expect_s7_class(single, ShiftBatch)
    expect_identical(single@ids$batch_id, wrapped@ids$batch_id)
    expect_identical(single@meta$manifest$site_id, c("A", "A"))
    expect_false("epw" %in% names(formals(shift_future_epw)))
    expect_identical(names(formals(shift_future_epw))[[1L]], "sites")
})

test_that("version 3 location arrays plan and restore through the CLI", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    config <- epwshiftr_cli_shift_example_config()
    config$version <- 3L
    config$sites <- list(
        list(id = "South", epw = config$sites[[1L]]$epw),
        list(id = "North", epw = multi_site__epw(), label = "North")
    )
    config$climate$model <- 1L
    config$dir <- tempfile("cli-multi-output-")
    path <- multi_site__write_config(config)
    root <- tempfile("cli-multi-store-")
    base <- c("--quiet", "--store", root, "shift")
    checked <- epwshiftr_cli(c(base, "config", "validate", "--config", path))
    expect_equal(checked$status, 0L, info = checked$error)
    expect_equal(checked$result$validation, "local")
    expect_identical(checked$result$sites$site_id, c("North", "South"))
    planned <- epwshiftr_cli(c(base, "run", "--config", path, "--dry-run"))
    expect_equal(planned$status, 0L, info = planned$error)
    expect_length(planned$result$batch_id, 1L)
    expect_equal(nrow(planned$result$children), 2L)
    shown <- epwshiftr_cli(c(base, "show", "--batch", planned$result$batch_id))
    expect_equal(shown$status, 0L, info = shown$error)
    expect_setequal(shown$result$children$site_id, c("North", "South"))
    config$sites <- config$sites[1L]
    expect_silent(epwshiftr_cli_read_shift_config(multi_site__write_config(
        config
    )))
    config$epw <- get_cache_epw()
    expect_error(
        epwshiftr_cli_read_shift_config(multi_site__write_config(config)),
        "epw"
    )
    config$epw <- NULL
    config$version <- 2L
    expect_error(
        epwshiftr_cli_read_shift_config(multi_site__write_config(config)),
        "version 3"
    )
    config$version <- 1L
    expect_error(
        epwshiftr_cli_read_shift_config(multi_site__write_config(config)),
        "version 3"
    )
})


test_that("multi-site references are resolved for each location", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    sites <- list(
        shift_site("A", epw = get_cache_epw()),
        shift_site("B", epw = multi_site__epw())
    )
    plan <- shift_future_epw(
        sites = sites,
        climate = shift_cmip6(model = 1L, scenarios = "ssp585"),
        periods = 2050:2051,
        methods = c("original_morphing", "isimip3basd"),
        calibration = shift_era5(1995:2014),
        dir = tempfile(),
        store = tempfile(),
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    children <- plan@meta$children
    expect_null(children[[1L]]@meta$observed_reference)
    expect_null(children[[3L]]@meta$observed_reference)
    expect_s7_class(children[[2L]]@meta$observed_reference, ShiftReanalysisSpec)
    expect_s7_class(children[[4L]]@meta$observed_reference, ShiftReanalysisSpec)
    expect_false(identical(
        children[[2L]]@meta$site@lon,
        children[[4L]]@meta$site@lon
    ))
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    expect_error(
        multi_site__plan(
            sites,
            reference = shift_reference_plan(
                "existing-plan",
                epw_morph_periods(hist = 1995:2014)
            )
        ),
        "cannot share an extracted reference plan"
    )
})

test_that("new site inputs validate EPW generation metadata before discovery", {
    path <- multi_site__epw()
    lines <- readLines(path)
    header <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
    header[[9L]] <- "unknown"
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    sites <- shift_site("A", epw = path)
    expect_error(multi_site__plan(sites), "EPW time zone")
    header[[9L]] <- "8"
    header[[10L]] <- "unknown"
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    expect_error(multi_site__plan(sites), "EPW elevation")
})

test_that("multi-site batches execute locally and reuse each site's outputs", {
    # Fixture catalog identities must not reuse another test's disk cache.
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("RNetCDF")
    skip_if_not_installed("duckdb")
    test_local_dependencies(list(
        availability = function(...) {
            args <- list(...)
            args$source <- "EC-Earth3"
            do.call(test_cmip6_availability, args)
        },
        shift__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    transform <- monthly_transform("epwshiftr")
    variables <- epw_morph_variables(transform__recipe(transform))
    files <- stats::setNames(
        vapply(
            variables,
            function(variable) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
    withr::defer(unlink(files))
    docs <- data.table::rbindlist(
        lapply(variables, function(variable) {
            cli_shift_test_file_docs(
                basename(files[[variable]]),
                opendap_url = files[[variable]],
                download_url = files[[variable]],
                variable_id = variable,
                frequency = "mon",
                table_id = "Amon"
            )
        }),
        fill = TRUE
    )
    for (column in c("dataset_id", "master_id")) {
        data.table::set(
            docs,
            j = column,
            value = paste0("future-", docs$variable_id)
        )
    }
    data.table::set(
        docs,
        j = "instance_id",
        value = paste0("future-", docs$variable_id, ".v20260101")
    )
    data.table::set(
        docs,
        j = "tracking_id",
        value = paste0("hdl:21.14100/future-", docs$variable_id)
    )
    data.table::set(
        docs,
        j = "id",
        value = paste0(docs$title, "|future-", docs$variable_id)
    )
    data.table::set(
        docs,
        j = "checksum",
        value = vapply(
            docs$variable_id,
            function(variable) store_hash_file(files[[variable]], "sha256"),
            character(1L)
        )
    )
    calls <- cli_shift_test_mock_collect(docs)
    sites <- list(
        shift_site("A", lon = 103.98, lat = 1.37, epw = get_cache_epw()),
        shift_site("B", lon = 104.4, lat = 2, epw = get_cache_epw())
    )
    plan <- shift_future_epw(
        sites = sites,
        climate = shift_cmip6(
            model = 1L,
            scenarios = "ssp585",
            frequency = "mon",
            table = "Amon",
            grid = "gr",
            index_nodes = "https://example.org"
        ),
        periods = 2060,
        transform = transform,
        dir = tempfile("multi-export-"),
        store = tempfile("multi-store-"),
        dry_run = TRUE,
        control = shift_control(strict = FALSE),
        ui = shift_ui(progress = "none")
    )
    discovery_store <- EsgStore$new(file.path(
        plan@store_path,
        "discovery"
    ))
    discovery_store$add_files(cli_shift_test_file_result(docs))
    discovery_store$close()
    plan@meta$shared_plan <- shift_batch__plan_from_discovery(
        plan@meta$children,
        plan@meta$manifest,
        file.path(plan@store_path, "discovery")
    )
    expect_gt(nrow(plan@meta$shared_plan$acquisitions), 0L)
    shift_batch__receipt_write(plan)
    completed <- shift_run(plan, ui = shift_ui(progress = "none"))
    expect_null(completed@meta[["shared_failure"]])
    expect_identical(shift_status(completed), "completed")
    shared_receipts <- list.files(
        file.path(plan@store_path, "shared-acquisitions"),
        pattern = "[.]json$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_gt(length(shared_receipts), 0L)
    output <- shift_outputs(completed)
    expect_equal(data.table::uniqueN(output$site_id), 2L)
    expect_equal(data.table::uniqueN(output$export_path), 2L)
    expect_true(all(file.exists(output$export_path)))
    # Compare the actual extracted cells and native monthly axis against the
    # NetCDF source, independently of successful execution and file existence.
    nc <- RNetCDF::open.nc(files[["tas"]])
    source_values <- RNetCDF::var.get.nc(nc, "tas")
    source_time <- RNetCDF::var.get.nc(nc, "time")
    source_units <- RNetCDF::att.get.nc(nc, "time", "units")
    expect_identical(RNetCDF::att.get.nc(nc, "NC_GLOBAL", "frequency"), "mon")
    expect_identical(RNetCDF::att.get.nc(nc, "NC_GLOBAL", "table_id"), "Amon")
    RNetCDF::close.nc(nc)
    for (i in seq_along(completed@meta$children)) {
        child <- completed@meta$children[[i]]
        climate <- shift_stage_new(
            ShiftClimate,
            "climate",
            store_path = child@store_path,
            ids = list(
                plan_id = jsonlite::fromJSON(child@meta$run$plan_ids_json[[1L]])
            )
        )
        raw <- shift_data(climate, n = Inf, variables = "tas")
        expect_equal(nrow(raw), 12L)
        expect_equal(raw$lon, rep(sites[[i]]@lon, 12L))
        expect_equal(raw$lat, rep(sites[[i]]@lat, 12L))
        store <- shift_store(child)
        shared_events <- store$query(paste(
            "SELECT details_json FROM shift_run_event",
            "WHERE details_json LIKE '%shared_cache%'"
        ))
        expect_gt(nrow(shared_events), 0L)
        grid <- morpher__private_store(store)$read_table(
            "extraction_grid_source"
        )
        grid <- grid[grid$variable_id == "tas"]
        expect_equal(unique(grid$grid_lon), c(104, 104.5)[[i]])
        expect_equal(unique(grid$grid_lat), c(1, 2)[[i]])
        expect_true(all(grid$weight == 1))
        store$close()
        expect_equal(
            raw$value,
            source_values[c(2L, 3L)[[i]], c(1L, 2L)[[i]], ],
            tolerance = 1e-7
        )
        expect_identical(unique(raw$units), "K")
        expect_identical(as.integer(format(raw$time, "%m")), seq_len(12L))
        expect_true(all(raw$year == 2060L))
        # The serialized EPW must expose a complete chronological hourly year.
        weather <- epw_file_read(output$export_path[[i]])$data()
        expect_equal(nrow(weather), 8760L)
        expect_true(all(is.finite(weather$dry_bulb_temperature)))
    }
    expect_length(source_time, 12L)
    expect_match(source_units, "days since 2060-01-01")
    expect_equal(
        nrow(shift_data(
            completed,
            n = 2L,
            columns = c("site_id", "dry_bulb_temperature")
        )),
        2L
    )
    hashes <- tools::md5sum(output$export_path)
    collected <- length(calls$types)
    reopened <- shift_batch_get(plan@ids$batch_id, plan@store_path)
    expect_identical(shift_outputs(reopened)$export_path, output$export_path)
    resumed <- shift_resume(reopened, ui = shift_ui(progress = "none"))
    expect_identical(shift_status(resumed), "completed")
    expect_identical(tools::md5sum(shift_outputs(resumed)$export_path), hashes)
    expect_identical(length(calls$types), collected)
    expect_setequal(shift_summary(resumed)$site_id, c("A", "B"))
    restored <- shift_future_epw(
        sites,
        plan@meta$climate,
        2060,
        transform = transform,
        dir = plan@meta$output_dir,
        store = dirname(dirname(plan@store_path)),
        control = shift_control(strict = FALSE),
        ui = shift_ui("none")
    )
    expect_identical(restored@meta$execution$action, c("reused", "reused"))
    for (control in list(
        shift_control(strict = TRUE),
        shift_control(strict = FALSE, extraction_method = "idw")
    )) {
        changed <- shift_future_epw(
            sites,
            plan@meta$climate,
            2060,
            transform = transform,
            dir = plan@meta$output_dir,
            store = dirname(dirname(plan@store_path)),
            control = control,
            dry_run = TRUE,
            ui = shift_ui("none")
        )
        expect_false(identical(changed@ids$batch_id, plan@ids$batch_id))
        expect_true(all(vapply(
            changed@meta$children,
            function(x) S7::S7_inherits(x, ShiftPlan),
            logical(1L)
        )))
        expect_identical(changed@meta$children[[1L]]@meta$control, control)
    }
    expect_identical(tools::md5sum(output$export_path), hashes)
})


test_that("R and CLI reject invalid method periods before discovery", {
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    config <- epwshiftr_cli_shift_example_config()
    config$transform <- list(scale = "hourly", method = "kernel_qdm")
    config$periods <- list(mid = 2050L)
    config$calibration <- list(dataset = "era5", years = "1995:2014")
    path <- multi_site__write_config(config)
    root <- tempfile()
    for (command in list(c("config", "validate"), c("run", "--dry-run"))) {
        result <- epwshiftr_cli(c(
            "--quiet",
            "--store",
            root,
            "shift",
            command,
            "--config",
            path
        ))
        expect_equal(result$status, 1L)
        expect_match(result$error, "at least two weather years")
    }
    expect_error(
        shift_future_epw(
            shift_site(epw = get_cache_epw()),
            shift_cmip6("Model-A", "ssp585"),
            2050L,
            transform = hourly_transform("kernel_qdm"),
            calibration = shift_era5(1995:2014),
            dir = tempfile(),
            store = root,
            dry_run = TRUE
        ),
        "at least two weather years"
    )
    expect_false(dir.exists(root))
})
