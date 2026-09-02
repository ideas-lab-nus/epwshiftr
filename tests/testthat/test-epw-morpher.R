test_that("get_cache_epw() prepares a stable local EPW fixture", {
    dir <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = dir)

    path <- get_cache_epw()

    expect_true(file.exists(path))
    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")

    epw <- epw_file_read(path)
    expect_equal(epw$location()$city, "Singapore")
    expect_equal(epw$location()$country, "Singapore")
    expect_equal(nrow(epw$data()), 8760L)
    expect_gt(sum(epw$data()$liquid_precip_depth, na.rm = TRUE), 0)

    expect_identical(get_cache_epw(), path)

    stale_lines <- readLines(path, warn = FALSE)
    stale_weather <- strsplit(stale_lines[-seq_len(8L)], ",", fixed = TRUE)
    stale_weather <- lapply(stale_weather, function(x) {
        x[[34L]] <- "0.0"
        x[[35L]] <- "0.0"
        x
    })
    writeLines(c(stale_lines[seq_len(8L)], vapply(stale_weather, paste, character(1L), collapse = ",")), path)

    expect_identical(get_cache_epw(), path)
    epw <- epw_file_read(path)
    expect_gt(sum(epw$data()$liquid_precip_depth, na.rm = TRUE), 0)
})

test_that("packaged Singapore EPW fixture is readable", {
    path <- system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr",
        mustWork = TRUE
    )

    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")
    expect_equal(epw_file_read(path)$location()$city, "Singapore")

    external <- test_external_epw(path)
    original_path <- external$path()
    converted <- epw_file_coerce(external)
    expect_true(inherits(converted, "EpwFile"))
    expect_equal(converted$location()$city, "Singapore")
    # Conversion saves a deep clone and must not mutate the caller's object.
    expect_identical(external$path(), original_path)
})


test_that("EpwMorpher$summarise_climate() selects 360-day CF years and months", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        nc,
        2060L,
        calendar = "360_day",
        n_years = 2L
    )
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- epw_morpher_test_file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(epw_morpher_test_result(docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-12-30T00:00:00Z", "2061-01-01T23:59:59Z"),
        site_id = "SIN"
    )
    expect_equal(store$extract(plan_id = plan$plan_id)$status, "done")

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute"))
    )
    climate <- morpher$summarise_climate(
        plan$plan_id,
        epw_morph_periods(`2061` = 2061L),
        strict = FALSE
    )

    expect_identical(unique(climate$period), "2061")
    expect_identical(unique(climate$month), 1L)
    expect_true(all(climate$n_records == 1L))
})


test_that("epw_morpher() / EpwMorpher$required_variables() / EpwMorpher$summarise_climate() / EpwMorpher$summarise_baseline() / EpwMorpher$plan() / EpwMorpher$diagnose() / EpwMorpher$check() / EpwMorpher$run() / EpwMorpher$write_epw() / EpwMorpher$status() / EpwMorpher$outputs() create relaxed future EPW outputs from store extracts", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- epw_morpher_test_file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(epw_morpher_test_result(docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN"
    )
    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(processed$status, "done")

    external_epw <- test_external_epw(get_cache_epw())
    original_external_path <- external_epw$path()
    morpher <- epw_morpher(
        store = store,
        epw = external_epw,
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute")),
        label = "singapore"
    )
    expect_true(inherits(morpher, "EpwMorpher"))
    expect_identical(external_epw$path(), original_external_path)
    expect_setequal(morpher$required_variables(), epw_morph_variables("recommended"))

    periods <- epw_morph_periods(`2060s` = 2060L)
    strict_preflight <- morpher$preflight(plan$plan_id, periods, strict = TRUE)
    expect_named(strict_preflight, morpher__diagnostic_columns())
    expect_true(all(c("missing_required_variable", "missing_month") %in% strict_preflight$code))
    expect_true(any(strict_preflight$severity == "error"))
    expect_error(
        morpher$summarise_climate(plan$plan_id, periods, strict = TRUE),
        "blocking issues"
    )

    relaxed_preflight <- morpher$preflight(plan$plan_id, periods, strict = FALSE)
    expect_true(any(relaxed_preflight$severity == "warning"))
    missing_hurs <- relaxed_preflight[code == "missing_required_variable" & variable_id == "hurs"]
    expect_match(missing_hurs$message, "requires near-surface relative humidity")
    expect_match(missing_hurs$action, "relative humidity and dew point")

    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = FALSE)
    expect_equal(unique(climate$variable_id), "tas")
    expect_equal(unique(climate$units), "K")

    baseline <- morpher$summarise_baseline()
    expect_true(all(c("dry_bulb_temperature", "relative_humidity") %in% baseline$epw_field))

    preview <- morpher$preview_plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(preview$plan$status, "blocked")
    expect_equal(nrow(morpher$status(preview$plan$morph_id)), 0L)
    expect_true(any(preview$diagnostics$severity == "error"))

    blocked <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(blocked$status, "blocked")
    expect_true(any(morpher$diagnose(blocked$morph_id)$severity == "error"))
    expect_error(morpher$check(blocked$morph_id), "blocking issues")

    relaxed <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = FALSE
    )
    expect_equal(relaxed$status, "planned")
    expect_true(any(morpher$diagnose(relaxed$morph_id)$severity == "warning"))

    results <- morpher$run(relaxed$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))
    expect_identical(
        results$result_id,
        morpher__hash(relaxed$morph_id, results$case_id, result_path)
    )

    result_data <- read_test_parquet(result_path)
    expect_true(all(c("source_id", "experiment_id", "variant_label", "period") %in% names(result_data)))
    expect_equal(unique(result_data$period), "2060s")
    expect_equal(morpher$status(relaxed$morph_id)$status, "result_done")

    expect_error(
        morpher$write_epw(
            morph_id = relaxed$morph_id,
            dir = tempfile("future-epw-outside-")
        ),
        "inside the epwshiftr store root"
    )

    outputs <- morpher$write_epw(
        morph_id = relaxed$morph_id,
        dir = "outputs/future-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_identical(
        outputs$output_id,
        morpher__hash(relaxed$morph_id, outputs$case_id, output_path)
    )
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(epw_file_read(output_path), "EpwFile"))

    expect_equal(morpher$status(relaxed$morph_id)$status, "epw_written")
    expect_equal(nrow(morpher$outputs(relaxed$morph_id)), 1L)
})

test_that("epw_morpher() / EpwMorpher$summarise_climate() / EpwMorpher$summarise_baseline() / EpwMorpher$plan() / EpwMorpher$diagnose() / EpwMorpher$check() / EpwMorpher$run() / EpwMorpher$write_epw() / EpwMorpher$status() / EpwMorpher$outputs() complete strict outputs with recommended variables", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    variables <- epw_morph_variables("recommended")
    nc <- stats::setNames(
        vapply(variables, function(variable_id) {
            path <- tempfile(fileext = ".nc")
            write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = variable_id)
            path
        }, character(1L)),
        variables
    )
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- data.table::rbindlist(lapply(variables, function(variable_id) {
        epw_morpher_test_file_docs(
            path = basename(nc[[variable_id]]),
            opendap_url = nc[[variable_id]],
            download_url = nc[[variable_id]],
            variable_id = variable_id
        )
    }), fill = TRUE)
    query_id <- store$add_files(epw_morpher_test_result(as.data.frame(docs)))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
        site_id = "SIN",
        variable_id = variables
    )
    expect_setequal(plan$variable_id, variables)

    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(nrow(processed), length(variables))
    expect_true(all(processed$status == "done"))

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute")),
        label = "singapore"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    preflight <- morpher$preflight(plan$plan_id, periods, strict = TRUE)
    expect_equal(nrow(preflight), 0L)

    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = TRUE)
    expect_setequal(unique(climate$variable_id), variables)

    baseline <- morpher$summarise_baseline()
    preview <- morpher$preview_plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(preview$plan$status, "planned")
    expect_equal(nrow(preview$diagnostics), 0L)
    expect_equal(nrow(morpher$status(preview$plan$morph_id)), 0L)

    bad_climate <- data.table::copy(climate)
    bad_climate[, summary_id := paste0(summary_id, "-bad-units")]
    bad_climate[variable_id == "tas", units := "bad_unit"]
    bad_climate[, summary_row_id := morpher__hash_rows(summary_id, plan_id, variable_id, period, month, stat)]
    morpher__replace_rows(store, "epw_climate_summary", bad_climate, "summary_row_id")
    bad_strict <- morpher$preview_plan(
        summary_id = unique(bad_climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(bad_strict$plan$status, "blocked")
    expect_true(any(bad_strict$diagnostics$code == "unit_conversion_failed"))
    bad_relaxed <- morpher$preview_plan(
        summary_id = unique(bad_climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = FALSE
    )
    expect_equal(bad_relaxed$plan$status, "planned")
    expect_true(any(bad_relaxed$diagnostics$severity == "warning"))

    strict <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(strict$status, "planned")
    expect_equal(nrow(morpher$diagnose(strict$morph_id)), 0L)
    expect_silent(morpher$check(strict$morph_id))

    results <- morpher$run(strict$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))
    expect_equal(morpher$status(strict$morph_id)$status, "result_done")

    result_data <- read_test_parquet(result_path)
    epw <- epw_file_read(get_cache_epw())
    baseline_data <- data.table::as.data.table(epw$data())
    expect_true(all(c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature",
        "atmospheric_pressure",
        "global_horizontal_radiation",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "wind_speed",
        "total_sky_cover",
        "opaque_sky_cover",
        "liquid_precip_depth",
        "liquid_precip_rate"
    ) %in% names(result_data)))
    expect_true(any(abs(result_data$dry_bulb_temperature - baseline_data$dry_bulb_temperature) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$dew_point_temperature - baseline_data$dew_point_temperature) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$diffuse_horizontal_radiation - baseline_data$diffuse_horizontal_radiation) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$direct_normal_radiation - baseline_data$direct_normal_radiation) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$liquid_precip_depth - baseline_data$liquid_precip_depth) > 1e-6, na.rm = TRUE))
    expect_setequal(unique(result_data$liquid_precip_rate), c(0, 1))

    resumed_results <- morpher$run(strict$morph_id, overwrite = FALSE, resume = TRUE)
    expect_equal(resumed_results$result_id, results$result_id)

    override_morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute", methods = c(tdb = "shift", rh = "shift"))),
        label = "singapore"
    )
    override_baseline <- override_morpher$summarise_baseline()
    override <- override_morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(override_baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(override$status, "planned")
    expect_false(identical(override$morph_id, strict$morph_id))
    override_results <- override_morpher$run(override$morph_id, overwrite = TRUE)
    override_data <- read_test_parquet(store_abs_path(override_results$output_path, root = store$path))
    # Enhanced auto temperature intentionally degrades to shift when optional
    # tasmax/tasmin are absent, so this explicit override is numerically equal.
    expect_equal(override_data$dry_bulb_temperature, result_data$dry_bulb_temperature)
    expect_true(any(abs(override_data$relative_humidity - result_data$relative_humidity) > 1e-6, na.rm = TRUE))

    change_morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = epw_morph_recipe("belcher"),
        label = "singapore-change"
    )
    change_baseline <- change_morpher$summarise_baseline()
    change_missing_ref <- change_morpher$preview_plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(change_baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(change_missing_ref$plan$status, "planned")
    expect_false(any(change_missing_ref$diagnostics$code == "missing_reference_climate"))
    expect_true(all(!is.na(change_missing_ref$factors$reference)))
    expect_equal(change_missing_ref$factors$reference, change_missing_ref$factors$baseline)
    baseline_reference <- change_morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(change_baseline$baseline_id),
        strict = TRUE
    )
    baseline_reference_results <- change_morpher$run(baseline_reference$morph_id, overwrite = TRUE)
    baseline_reference_data <- read_test_parquet(
        store_abs_path(baseline_reference_results$output_path, root = store$path)
    )
    expect_true(any(abs(
        baseline_reference_data$dry_bulb_temperature - baseline_data$dry_bulb_temperature
    ) > 1e-6, na.rm = TRUE))
    change <- change_morpher$plan(
        summary_id = unique(climate$summary_id),
        reference_summary_id = unique(climate$summary_id),
        baseline_id = unique(change_baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(change$status, "planned")
    change_factors <- store$query(sprintf(
        "SELECT * FROM epw_morph_factor WHERE morph_id = '%s'",
        change$morph_id
    ))
    expect_true("reference" %in% names(change_factors))
    expect_true(any(!is.na(change_factors$reference)))
    change_results <- change_morpher$run(change$morph_id, overwrite = TRUE)
    change_data <- read_test_parquet(store_abs_path(change_results$output_path, root = store$path))
    expect_lt(max(abs(change_data$dry_bulb_temperature - baseline_data$dry_bulb_temperature), na.rm = TRUE), 1e-6)

    outputs <- morpher$write_epw(
        morph_id = strict$morph_id,
        dir = "outputs/future-epw-strict",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(epw_file_read(output_path), "EpwFile"))

    expect_equal(morpher$status(strict$morph_id)$status, "epw_written")
    expect_equal(nrow(morpher$outputs(strict$morph_id)), 1L)

    resumed_outputs <- morpher$write_epw(
        morph_id = strict$morph_id,
        dir = "outputs/future-epw-strict",
        separate = FALSE,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_equal(resumed_outputs$output_id, outputs$output_id)

    workflow_dir <- tempfile("esg-store-workflow-")
    workflow_store <- EsgStore$new(workflow_dir)
    on.exit(workflow_store$close(), add = TRUE)
    workflow_query_id <- workflow_store$add_files(epw_morpher_test_result(as.data.frame(docs)))
    workflow_plan <- workflow_store$plan_region(
        query_id = workflow_query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
        site_id = "SIN",
        variable_id = variables
    )
    workflow_processed <- workflow_store$extract(plan_id = workflow_plan$plan_id)
    expect_true(all(workflow_processed$status == "done"))
    workflow_morpher <- epw_morpher(
        store = workflow_store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute")),
        label = "singapore"
    )
    workflow_no_epw <- workflow_morpher$workflow(
        plan_id = workflow_plan$plan_id,
        periods = periods,
        strict = TRUE,
        dir = NULL,
        overwrite = TRUE
    )
    expect_named(workflow_no_epw, c("preflight", "climate", "baseline", "preview", "plan", "diagnostics", "results", "outputs"))
    expect_null(workflow_no_epw$outputs)
    expect_equal(workflow_morpher$status(workflow_no_epw$plan$morph_id)$status, "result_done")

    workflow <- workflow_morpher$workflow(
        plan_id = workflow_plan$plan_id,
        periods = periods,
        strict = TRUE,
        dir = "outputs/workflow-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_named(workflow, c("preflight", "climate", "baseline", "preview", "plan", "diagnostics", "results", "outputs"))
    expect_equal(workflow$plan$status, "planned")
    expect_equal(workflow_morpher$status(workflow$plan$morph_id)$status, "epw_written")
    expect_equal(nrow(workflow$outputs), 1L)
})
