epw_morpher_test_response <- function(docs) {
    list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = stats::setNames(list(), character())
        ),
        response = list(
            numFound = nrow(docs),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
        facet_counts = list(
            facet_queries = stats::setNames(list(), character()),
            facet_fields = stats::setNames(list(), character()),
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
}

epw_morpher_test_params <- function() {
    query_param__as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
}

epw_morpher_test_result <- function(docs) {
    query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = epw_morpher_test_params(),
        result = epw_morpher_test_response(docs)
    )
}

epw_morpher_test_file_docs <- function(path, opendap_url, download_url, variable_id = "tas") {
    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", path),
        master_id = sprintf("%s.master", path),
        replica = FALSE,
        tracking_id = "hdl:21.14100/local-test-2060",
        title = path,
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = variable_id,
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("%s|application/netcdf|OPENDAP", opendap_url),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

test_that("get_cache_epw() prepares a stable local EPW fixture", {
    dir <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = dir)

    path <- get_cache_epw()

    expect_true(file.exists(path))
    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")

    epw <- eplusr::read_epw(path)
    expect_equal(epw$location()$city, "Singapore")
    expect_equal(epw$location()$country, "Singapore")
    expect_equal(nrow(epw$data()), 8760L)

    expect_identical(get_cache_epw(), path)
})

test_that("epw_morph_recipe() accepts morph.R statistical downscaling method overrides", {
    recipe <- epw_morph_recipe(methods = c(tdb = "shift", rh = "shift"))

    expect_s3_class(recipe, "epw_morph_recipe")
    expect_equal(recipe$backend, "belcher")
    expect_equal(recipe$methods[c("tdb", "rh")], c(tdb = "shift", rh = "shift"))
    expect_equal(
        recipe$rules[epw_field == "dry_bulb_temperature", method],
        "shift"
    )
    expect_equal(
        recipe$rules[epw_field == "relative_humidity", method],
        "shift"
    )
    expect_equal(epw_morph_variables(recipe), epw_morph_variables("recommended"))
    expect_error(epw_morph_recipe(methods = c(foo = "shift")), "Unknown")
    expect_error(epw_morph_recipe(methods = c(tdb = "scale")), "Unsupported")
})

test_that("R6 EPW morphing backends can be looked up, registered, and selected", {
    expect_true("belcher" %in% epw_morph_backends())

    belcher <- epw_morph_backend("belcher")
    expect_true(inherits(belcher, "EpwMorphBackend"))
    expect_equal(belcher$required_variables(), c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt"))
    expect_equal(epw_morph_variables(belcher), epw_morph_variables("recommended"))
    expect_equal(epw_morph_variables("belcher"), epw_morph_variables("recommended"))
    expect_equal(belcher$validate_methods(c(tdb = "shift"))[["tdb"]], "shift")
    expect_error(epw_morph_backend("missing-backend"), "Unknown")
    expect_error(epw_morph_register_backend("not-a-backend", list()), "EpwMorphBackend")

    backend_name <- paste0("testbackend", Sys.getpid())
    rules <- data.table::data.table(
        step = "dry",
        epw_field = "dry_bulb_temperature",
        variable_id = "tas",
        optional_variable_id = NA_character_,
        method = "offset",
        required = TRUE,
        derived = FALSE,
        method_choices = list(c("offset", "plus_two"))
    )
    runner <- function(context, backend) {
        epw <- context$epw$clone()
        suppressMessages(epw$drop_unit())
        data <- data.table::as.data.table(epw$data())
        offset <- if (identical(context$recipe$methods[["dry"]], "plus_two")) 2 else 1
        data[, `:=`(
            dry_bulb_temperature = dry_bulb_temperature + offset,
            custom_backend = backend$name
        )]
        epw_morph_result(context, epw = epw, data = data)
    }
    custom <- EpwMorphBackend$new(
        name = backend_name,
        methods = c(dry = "offset"),
        method_choices = c("offset", "plus_two"),
        rules = rules,
        runner = runner
    )

    epw_morph_register_backend(backend_name, custom, overwrite = TRUE)
    expect_identical(epw_morph_backend(backend_name), custom)
    expect_error(epw_morph_register_backend(backend_name, custom), "already registered")

    recipe <- epw_morph_recipe(name = backend_name, backend = backend_name, methods = c(dry = "plus_two"))
    expect_equal(recipe$methods[["dry"]], "plus_two")
    expect_equal(epw_morph_variables(recipe), "tas")
    expect_error(
        epw_morph_recipe(name = backend_name, backend = backend_name, methods = c(dry = "scale")),
        "Unsupported"
    )
    context <- epw_morph_context(
        epw = eplusr::read_epw(get_cache_epw()),
        climate = data.table::data.table(
            time = as.POSIXct("2060-01-01", tz = "UTC"),
            variable_id = "tas",
            period = "2060s",
            year = 2060L,
            lon = 104,
            lat = 1,
            dist = 0,
            units = "K",
            value = 300
        ),
        recipe = recipe
    )
    result <- epw_morph_run_context(context)

    expect_s3_class(result, "epw_morph_result")
    expect_equal(unique(result$data$custom_backend), backend_name)
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
        site_id = "SIN",
        nearest = 1L
    )
    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(processed$status, "done")

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        label = "singapore"
    )
    expect_true(inherits(morpher, "EpwMorpher"))
    expect_setequal(morpher$required_variables(), epw_morph_variables("recommended"))

    periods <- epw_morph_periods(`2060s` = 2060L)
    strict_preflight <- morpher$preflight(plan$plan_id, periods, strict = TRUE)
    expect_named(strict_preflight, epw_morph_diagnostic_columns())
    expect_true(all(c("missing_required_variable", "missing_month") %in% strict_preflight$code))
    expect_true(any(strict_preflight$severity == "error"))
    expect_error(
        morpher$summarise_climate(plan$plan_id, periods, strict = TRUE),
        "blocking issues"
    )

    relaxed_preflight <- morpher$preflight(plan$plan_id, periods, strict = FALSE)
    expect_true(any(relaxed_preflight$severity == "warning"))

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

    result_data <- read_test_parquet(result_path)
    expect_true(all(c("source_id", "experiment_id", "variant_label", "period") %in% names(result_data)))
    expect_equal(unique(result_data$period), "2060s")
    expect_equal(morpher$status(relaxed$morph_id)$status, "result_done")

    outputs <- morpher$write_epw(
        morph_id = relaxed$morph_id,
        dir = "outputs/future-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(eplusr::read_epw(output_path), "Epw"))

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
        variable_id = variables,
        nearest = 1L
    )
    expect_setequal(plan$variable_id, variables)

    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(nrow(processed), length(variables))
    expect_true(all(processed$status == "done"))

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
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
    bad_climate[, summary_row_id := epw_morph_hash_rows(summary_id, plan_id, variable_id, period, month, stat)]
    epw_morph_replace_rows(store, "epw_climate_summary", bad_climate, "summary_row_id")
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
    epw <- eplusr::read_epw(get_cache_epw())
    suppressMessages(epw$drop_unit())
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
        "opaque_sky_cover"
    ) %in% names(result_data)))
    expect_true(any(abs(result_data$dry_bulb_temperature - baseline_data$dry_bulb_temperature) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$dew_point_temperature - baseline_data$dew_point_temperature) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$diffuse_horizontal_radiation - baseline_data$diffuse_horizontal_radiation) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(result_data$direct_normal_radiation - baseline_data$direct_normal_radiation) > 1e-6, na.rm = TRUE))

    resumed_results <- morpher$run(strict$morph_id, overwrite = FALSE, resume = TRUE)
    expect_equal(resumed_results$result_id, results$result_id)

    override_morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = epw_morph_recipe(methods = c(tdb = "shift", rh = "shift")),
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
    expect_true(any(abs(override_data$dry_bulb_temperature - result_data$dry_bulb_temperature) > 1e-6, na.rm = TRUE))
    expect_true(any(abs(override_data$relative_humidity - result_data$relative_humidity) > 1e-6, na.rm = TRUE))

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
    expect_true(inherits(eplusr::read_epw(output_path), "Epw"))

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
        variable_id = variables,
        nearest = 1L
    )
    workflow_processed <- workflow_store$extract(plan_id = workflow_plan$plan_id)
    expect_true(all(workflow_processed$status == "done"))
    workflow_morpher <- epw_morpher(
        store = workflow_store,
        epw = get_cache_epw(),
        site_id = "SIN",
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
