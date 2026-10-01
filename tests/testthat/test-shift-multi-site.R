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

test_that("multiple sites share discovery and retain distinct durable plans", {
    calls <- 0L
    withr::local_options(list(
        epwshiftr.cmip6.availability = function(...) {
            calls <<- calls + 1L
            test_cmip6_availability(...)
        },
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    sites <- data.table::data.table(
        site_id = c("South", "North"),
        epw = c(get_cache_epw(), multi_site__epw())
    )
    data.table::setkeyv(sites, "epw")
    original <- data.table::copy(sites)
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
    # Ordering and table implementation do not invalidate the saved discovery.
    calls <- 0L
    reordered <- multi_site__plan(as.data.frame(sites[2:1]), root)
    expect_identical(reordered@ids$batch_id, batch@ids$batch_id)
    expect_identical(calls, 0L)
    reopened <- shift_batch_get(batch@ids$batch_id, root)
    expect_identical(reopened@meta$manifest, reordered@meta$manifest)
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
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
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

test_that("invalid locations fail before catalog access", {
    withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
        stop("Unexpected catalog access")
    }))
    epw <- get_cache_epw()
    sites <- data.table::data.table(site_id = c("A", "B"), epw = epw)
    expect_error(multi_site__plan(sites, epw = epw), "exactly one")
    expect_error(multi_site__plan(sites[0]), "length|non-empty")
    expect_error(multi_site__plan(sites[, .(epw)]), "site_id")
    expect_error(multi_site__plan(sites[, .(site_id)]), "epw")
    expect_error(multi_site__plan(list()), "non-empty")
    expect_error(multi_site__plan(list(sites)), "site table")
    expect_error(
        multi_site__plan(list(shift_site("A", 110, 30))),
        "baseline EPW"
    )
    for (ids in list(c("A", "A"), c("A", NA_character_), c("A", " "))) {
        invalid <- data.table::copy(sites)
        data.table::set(invalid, j = "site_id", value = ids)
        expect_error(multi_site__plan(invalid), "duplicated|missing|whitespace")
    }
    for (coords in list(c(NA_real_, Inf), c(0, 91))) {
        invalid <- data.table::copy(sites)
        data.table::set(invalid, j = "lat", value = coords)
        expect_error(multi_site__plan(invalid), "finite|<= 90")
    }
    data.table::set(sites, i = 2L, j = "epw", value = "/missing/baseline.epw")
    expect_error(multi_site__plan(sites), "does not exist|No such file")
})

test_that("partial optional site fields use the baseline header", {
    epw <- get_cache_epw()
    sites <- data.table::data.table(
        site_id = c("A", "B"),
        epw = epw,
        lon = c(NA_real_, 110),
        lat = c(NA_real_, 30),
        label = c(NA_character_, "B")
    )
    result <- shift_batch__sites(sites = sites)
    expected <- shift_site(epw = epw)
    expect_s3_class(result, "data.table")
    expect_identical(result$site[[1L]]@lon, expected@lon)
    expect_identical(result$site[[1L]]@lat, expected@lat)
    expect_identical(result$site[[1L]]@label, expected@label)
    expect_equal(result$site[[2L]]@lon, 110)
    expect_identical(result$site[[2L]]@label, "B")
    data.table::set(sites, j = "label", value = NA_character_)
    result <- shift_batch__sites(sites = sites)
    expect_identical(result$site[[2L]]@label, expected@label)
})

test_that("old single-site calls retain their original plan and batch identities", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    epw <- get_cache_epw()
    climate <- shift_cmip6(model = "Model-A", scenarios = "ssp585")
    transform <- monthly_transform("epwshiftr")
    plan <- shift_future_epw(
        epw,
        climate,
        2050,
        transform,
        tempfile("single-output-"),
        store = tempfile("single-store-"),
        dry_run = TRUE
    )
    expect_s7_class(plan, ShiftPlan)
    expect_identical(plan@meta$site, shift_site(epw = epw))
    batch <- shift_future_epw(
        epw,
        climate,
        2050,
        transform = list(transform),
        methods = NULL,
        dir = tempfile("single-output-"),
        dry_run = TRUE
    )
    expect_s7_class(batch, ShiftPlan)
    climate <- shift_cmip6(model = 1L, scenarios = "ssp585")
    batch <- shift_future_epw(
        epw,
        climate,
        2050,
        methods = "original_morphing",
        dir = tempfile("single-output-"),
        store = tempfile("single-store-"),
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    transform <- monthly_transform("original_morphing")
    references <- list(list(
        model_historical = shift__reference_spec_value(
            historical_reference(1995:2014),
            "model_historical"
        ),
        observed_reference = NULL
    ))
    names(references) <- "monthly-original_morphing"
    expected <- store__hash(
        "shift-batch-v1",
        shift__epw_identity(epw)$checksum,
        shift__climate_spec_value(climate),
        list(`monthly-original_morphing` = transform__spec_value(transform)),
        split(2050L, "2050"),
        references
    )
    expect_identical(batch@ids$batch_id, expected)
    expect_false("site_id" %in% names(batch@meta$manifest))
})

test_that("version 3 location arrays plan and restore through the CLI", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    config <- epwshiftr_cli_shift_example_config()
    config$version <- 3L
    config$sites <- list(
        list(site_id = "South", epw = config$epw),
        list(site_id = "North", epw = multi_site__epw(), label = "North")
    )
    config$epw <- NULL
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
        "exactly one"
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
        "versions 2 and 3"
    )
})


test_that("multi-site references are resolved for each location", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    sites <- data.table::data.table(
        site_id = c("A", "B"),
        epw = c(get_cache_epw(), multi_site__epw())
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
    withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
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
    withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
        stop("Unexpected catalog access")
    }))
    sites <- data.table::data.table(site_id = "A", epw = path)
    expect_error(multi_site__plan(sites), "EPW time zone for A")
    header[[9L]] <- "8"
    header[[10L]] <- "unknown"
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    expect_error(multi_site__plan(sites), "EPW elevation for A")
})

test_that("multi-site batches execute locally and reuse each site's outputs", {
    skip_if_not_installed("RNetCDF")
    skip_if_not_installed("duckdb")
    withr::local_options(list(
        epwshiftr.cmip6.availability = function(...) {
            args <- list(...)
            args$source <- "EC-Earth3"
            do.call(test_cmip6_availability, args)
        },
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
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
                    variable_id = variable
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
    completed <- shift_run(plan, ui = shift_ui(progress = "none"))
    expect_identical(shift_status(completed), "completed")
    output <- shift_outputs(completed)
    expect_equal(data.table::uniqueN(output$site_id), 2L)
    expect_equal(data.table::uniqueN(output$export_path), 2L)
    expect_true(all(file.exists(output$export_path)))
    hashes <- tools::md5sum(output$export_path)
    collected <- length(calls$types)
    reopened <- shift_batch_get(plan@ids$batch_id, plan@store_path)
    expect_identical(shift_outputs(reopened)$export_path, output$export_path)
    resumed <- shift_resume(reopened, ui = shift_ui(progress = "none"))
    expect_identical(shift_status(resumed), "completed")
    expect_identical(tools::md5sum(shift_outputs(resumed)$export_path), hashes)
    expect_identical(length(calls$types), collected)
    expect_setequal(shift_summary(resumed)$site_id, c("A", "B"))
})
