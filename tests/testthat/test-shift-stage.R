# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_request() and shift_site() create inspectable S7 stages", {
    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "mon",
        time = 2060L,
        filters = list(table_id = "Amon")
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = "baseline.epw")
    site_from_path <- shift_site(epw = get_cache_epw(), id = "SIN")
    site_from_first_arg <- shift_site(get_cache_epw())
    site_from_epw <- shift_site(epw_file_read(get_cache_epw()))
    site_from_external_epw <- shift_site(test_external_epw(get_cache_epw()))

    expect_true(S7::S7_inherits(req, ShiftRequest))
    expect_true(S7::S7_inherits(site, ShiftSite))
    expect_true(S7::S7_inherits(site_from_path, ShiftSite))
    expect_equal(shift_status(req), "new")
    expect_equal(shift_status(site), "new")
    expect_equal(req@meta$time, c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"))
    expect_equal(site_from_path@lon, 103.98)
    expect_equal(site_from_path@lat, 1.37)
    expect_equal(site_from_first_arg@id, "SGP_Singapore.486980_IWEC")
    expect_equal(site_from_first_arg@label, "Singapore")
    expect_equal(site_from_epw@id, "486980")
    expect_equal(site_from_epw@lon, 103.98)
    expect_equal(site_from_epw@lat, 1.37)
    expect_true(inherits(site_from_external_epw@epw, "EpwFile"))
    expect_equal(site_from_external_epw@id, "486980")
    expect_named(shift_diagnostics(req), shift_diagnostic_columns())
    expect_equal(data.table::as.data.table(req)$variables, "tas,hurs")
    expect_true(data.table::as.data.table(site)$has_epw)
})

test_that("shift_request() applies ESGF control filters through typed setters", {
    req <- shift_request(
        project = "CMIP6",
        variables = "tas",
        filters = list(latest = TRUE, replica = FALSE, table_id = "Amon")
    )
    query <- shift_as_query(req)

    expect_true(query_param__value(query$latest()))
    expect_false(query_param__value(query$replica()))
    expect_identical(query_param__value(query$params()$table_id), "Amon")
})

test_that("shift_request() preserves provider facet values", {
    req <- shift_request(
        project = "cmip6",
        frequency = c("monthly", "daily", "3hr")
    )
    query <- shift_as_query(req)

    expect_identical(req@meta$project, "cmip6")
    expect_identical(req@meta$frequency, c("monthly", "daily", "3hr"))
    expect_identical(query_param__value(query$project()), "cmip6")
    expect_identical(
        query_param__value(query$frequency()),
        c("monthly", "daily", "3hr")
    )
})

test_that("shift_cmip6 preserves the established positional member argument", {
    climate <- shift_cmip6(
        "BCC-CSM2-MR",
        "ssp585",
        "r1i1p1f1"
    )

    expect_identical(climate@model, "BCC-CSM2-MR")
    expect_identical(climate@member, "r1i1p1f1")
    expect_null(climate@n_models)
})

test_that("shift_cmip6 model values express explicit and automatic selection", {
    default <- shift_cmip6(scenarios = "ssp585")
    bounded <- shift_cmip6(model = 2L, scenarios = "ssp585")
    all_models <- shift_cmip6(model = NULL, scenarios = "ssp585")
    explicit <- shift_cmip6(
        model = c("Model-A", "Model-B"),
        scenarios = "ssp585"
    )

    expect_null(default@model)
    expect_identical(default@n_models, 3L)
    expect_null(bounded@model)
    expect_identical(bounded@n_models, 2L)
    expect_null(all_models@model)
    expect_null(all_models@n_models)
    expect_identical(explicit@model, c("Model-A", "Model-B"))
    expect_null(explicit@n_models)
    expect_identical(formals(shift_cmip6)$model, 3L)
    expect_false("n_models" %in% names(formals(shift_cmip6)))
    expect_error(shift_cmip6(model = 0L, scenarios = "ssp585"))
    expect_error(shift_cmip6(model = 1.5, scenarios = "ssp585"))
})

test_that("Shift scientific labels preserve table policy and partitions", {
    expect_identical(shift__format_cmip6_tables(NULL), "auto by variable")
    expect_identical(shift__format_cmip6_tables("Amon"), "Amon (forced)")
    expect_identical(
        shift__format_cmip6_tables(c(snd = "LImon")),
        "auto by variable · snd=LImon"
    )
    expect_identical(
        shift__format_cmip6_frequencies(c(tas = "3hrPt", rsds = "3hr")),
        "tas=3hrPt | rsds=3hr"
    )
    selection <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        partition_key = "Amon=gn;LImon=gr"
    )
    expect_identical(
        shift__format_cmip6_partitions(selection),
        "Amon=gn · LImon=gr"
    )

    climate <- shift_cmip6(
        "BCC-CSM2-MR", "ssp585", table = c(snd = "LImon")
    )
    encoded <- shift__spec_json(list(
        climate = shift__climate_spec_value(climate)
    ))
    decoded <- jsonlite::fromJSON(encoded, simplifyVector = TRUE)$climate
    expect_identical(decoded$table$snd, "LImon")
    expect_identical(
        shift__climate_from_spec(decoded)@table,
        c(snd = "LImon")
    )

    hourly_climate <- shift_cmip6(
        "BCC-CSM2-MR",
        "ssp585",
        frequency = c(tas = "3hrPt", rsds = "3hr")
    )
    hourly_encoded <- shift__spec_json(list(
        climate = shift__climate_spec_value(hourly_climate)
    ))
    hourly_decoded <- jsonlite::fromJSON(
        hourly_encoded,
        simplifyVector = TRUE
    )$climate
    expect_identical(
        shift__climate_from_spec(hourly_decoded)@frequency,
        c(tas = "3hrPt", rsds = "3hr")
    )

    bounded <- shift_cmip6(model = 2L, scenarios = "ssp585")
    bounded_spec <- shift__climate_spec_value(bounded)
    expect_identical(
        shift__climate_from_spec(bounded_spec)@n_models,
        2L
    )
    all_models <- shift_cmip6(model = NULL, scenarios = "ssp585")
    all_spec <- shift__climate_spec_value(all_models)
    expect_null(shift__climate_from_spec(all_spec)@n_models)

    request <- shift_cmip6_scenario(
        source = "BCC-CSM2-MR",
        scenario = "ssp585",
        variables = c("tas", "rsds"),
        frequency = c(tas = "3hrPt", rsds = "3hr")
    )
    request_encoded <- shift__spec_json(
        shift__request_spec_value(request)
    )
    request_decoded <- jsonlite::fromJSON(
        request_encoded,
        simplifyVector = TRUE
    )
    expect_identical(
        shift__request_frequency_from_spec(request_decoded$frequency),
        c(tas = "3hrPt", rsds = "3hr")
    )
    expect_null(shift__cmip6_request_table_spec(c("3hr", "day")))
    expect_identical(shift__cmip6_request_table_spec("3hr"), "3hr")
})

test_that("shift_cmip6_scenario() and shift_plan() describe future EPW workflows", {
    transform <- monthly_transform("original_morphing")
    req <- shift_cmip6_scenario(
        source = "BCC-CSM2-MR",
        scenario = c("ssp126", "ssp585"),
        member = "r1i1p1f1",
        years = 2055:2065,
        variables = morpher__input_variables(transform__recipe(transform)),
        frequency = "mon",
        grid_label = "gn",
        data_node = "esgf.ceda.ac.uk",
        index_node = "https://esgf-data.dkrz.de"
    )

    expect_equal(req@meta$project, "CMIP6")
    expect_equal(req@meta$experiment, c("ssp126", "ssp585"))
    expect_equal(req@meta$time, c("2055-01-01T00:00:00Z", "2065-12-31T23:59:59Z"))
    expect_equal(req@meta$filters$activity_id, "ScenarioMIP")
    expect_equal(req@meta$filters$table_id, "Amon")
    expect_true(all(c("tas", "hurs", "pr") %in% req@meta$variables))

    site <- shift_site(id = "SIN", epw = get_cache_epw())
    plan <- shift_plan(
        request = req,
        site = site,
        periods = list(`2060s` = "2055:2065"),
        store = tempfile("shift-store-"),
        transform = transform,
        reference = historical_reference("1995:2014"),
        epw = list(export_dir = tempfile("future-epw-"))
    )
    explain <- shift_explain(plan)

    expect_equal(shift_status(plan), "planned")
    expect_true(all(c("request", "transform", "reference", "output") %in% explain$step))
    expect_match(explain$detail[explain$step == "request"], "BCC-CSM2-MR")
})

test_that("shift diagnostics normalize empty partial tables", {
    partial <- data.table::data.table(stage = character(), severity = character())
    diagnostics <- shift_diagnostics_normalize(partial)

    expect_named(diagnostics, shift_diagnostic_columns())
    expect_equal(nrow(diagnostics), 0L)
})

test_that("shift reference specs validate manual and automatic reference inputs", {
    periods <- epw_morph_periods(reference = 1995L)

    historical <- shift_reference_historical(periods)
    manual <- shift_reference_plan("plan-reference", periods)

    expect_true(S7::S7_inherits(historical, ShiftReferenceSpec))
    expect_true(S7::S7_inherits(manual, ShiftReferenceSpec))
    expect_equal(historical@mode, "historical")
    expect_equal(historical@role, "model_historical")
    expect_equal(historical@experiment, "historical")
    expect_equal(historical@activity, "CMIP")
    expect_equal(manual@mode, "plan")
    expect_equal(manual@role, "model_historical")
    expect_equal(manual@plan_id, "plan-reference")
    expect_error(shift_reference_historical(NULL), "data.frame")
    expect_error(shift_reference_plan(character(), periods), "length >= 1")
})

test_that("target-year vectors expand to independently named periods", {
    periods <- shift__periods_from_input(c(2050, 2080))

    expect_identical(periods$period, c("2050", "2080"))
    expect_identical(periods$year, c(2050L, 2080L))
    expect_error(
        shift__periods_from_input(c(2050, 2050)),
        "duplicated"
    )
})

test_that("weather transforms remain reusable and validate execution references", {
    historical <- historical_reference(1995:2014)
    manual <- shift_reference_plan("plan-reference", epw_morph_periods(reference = 1995L))

    transform <- monthly_transform("original_morphing")
    expect_true(S7::S7_inherits(transform, WeatherTransformSpec))
    expect_false("reference" %in% S7::props(transform))
    expect_true(transform__validate_execution_inputs(transform, historical))
    expect_true(transform__validate_execution_inputs(transform, manual))
    expect_error(
        transform__validate_execution_inputs(transform),
        "requires.*reference"
    )
    expect_error(
        transform__validate_execution_inputs(transform, 1995:2014),
        "ShiftReferenceSpec"
    )
    expect_error(
        transform__validate_execution_inputs(
            monthly_transform("epwshiftr"),
            observed_reference = manual
        ),
        "does not use.*observed_reference"
    )
})

test_that("shift_future_epw() validates explicit transforms and returns a task plan", {
    transform <- monthly_transform("epwshiftr")
    climate <- shift_cmip6(
        model = "EC-Earth3",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        frequency = "mon",
        table = "Amon"
    )
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = climate,
        periods = list(`2060s` = 2060L),
        transform = transform,
        dir = tempfile("future-epw-"),
        control = shift_control(strict = FALSE),
        store = tempfile("shift-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_true(S7::S7_inherits(plan@meta$climate, ShiftCmip6Spec))
    expect_equal(plan@meta$climate@model, "EC-Earth3")
    expect_equal(plan@meta$climate@scenarios, "ssp585")
    expect_equal(shift_status(plan), "planned")
    expect_equal(nrow(shift_cases(plan)), 1L)
    spec <- shift__plan_spec(plan)
    expect_null(spec$request)
    expect_equal(spec$climate$model, "EC-Earth3")
    expect_equal(spec$climate$scenarios, "ssp585")
    expect_true(S7::S7_inherits(
        shift__plan_from_spec(spec)@meta$climate,
        ShiftCmip6Spec
    ))
    legacy_spec <- spec
    legacy_spec$version <- 1L
    expect_error(
        shift__plan_from_spec(legacy_spec),
        "unsupported schema version"
    )

    external_store <- tempfile("shift-external-epw-store-")
    external <- test_external_epw(get_cache_epw())
    original_external_path <- external$path()
    external_plan <- shift_future_epw(
        sites = shift_site(epw = external),
        climate = climate,
        periods = list(`2060s` = 2060L),
        transform = transform,
        dir = tempfile("future-epw-"),
        control = shift_control(strict = FALSE),
        store = external_store,
        dry_run = TRUE
    )@meta$children[[1L]]
    expect_true(inherits(external_plan@meta$site@epw, "EpwFile"))
    expect_true(startsWith(
        external_plan@meta$site@epw$path(),
        normalizePath(external_store, winslash = "/", mustWork = TRUE)
    ))
    expect_identical(external$path(), original_external_path)
    expect_error(
        shift_future_epw(
            sites = shift_site(epw = get_cache_epw()),
            climate = shift_cmip6("EC-Earth3", "ssp585"),
            periods = list(`2060s` = 2060L),
            transform = "original_morphing",
            dir = tempfile("future-epw-"),
            dry_run = TRUE
        )@meta$children[[1L]],
        "WeatherTransformSpec"
    )
    expect_error(
        shift_future_epw(
            sites = shift_site(epw = get_cache_epw()),
            model = "EC-Earth3",
            scenarios = "ssp585",
            periods = list(`2060s` = 2060L),
            transform = transform,
            dir = tempfile("future-epw-"),
            dry_run = TRUE
        )@meta$children[[1L]],
        "unused arguments"
    )
})

test_that("weather transforms remain reusable across execution contexts", {
    transform <- monthly_transform("epwshiftr")
    first <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "EC-Earth3",
            scenarios = "ssp126",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = 2050,
        transform = transform,
        dir = tempfile("first-future-epw-"),
        control = shift_control(strict = FALSE),
        store = tempfile("first-shift-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    second <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            member = "r1i1p1f1",
            grid = "gn",
            frequency = "mon",
            table = "Amon"
        ),
        periods = c(2050, 2080),
        transform = transform,
        dir = tempfile("second-future-epw-"),
        control = shift_control(strict = FALSE),
        store = tempfile("second-shift-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]

    # Planning must bind run-specific models, scenarios, and periods to each
    # plan without mutating the reusable scientific transform specification.
    expect_identical(first@meta$transform, transform)
    expect_identical(second@meta$transform, transform)
    expect_identical(transform@method, "epwshiftr")
    expect_identical(first@meta$periods$period, "2050")
    expect_setequal(second@meta$periods$period, c("2050", "2080"))
    expect_equal(nrow(shift_cases(first)), 1L)
    expect_equal(nrow(shift_cases(second)), 4L)
})

test_that("morph case failures update only their matching public case", {
    cases <- data.table::data.table(
        source_id = "IPSL-CM6A-LR",
        experiment_id = c("ssp126", "ssp585"),
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        period = "2050",
        status = "ready",
        missing_reason = NA_character_
    )
    morph_cases <- data.table::copy(cases)[, `:=`(
        status = c("failed", "completed"),
        last_error = c("bounded target failed", NA_character_)
    )]

    updated <- shift__apply_morph_case_status(cases, morph_cases)

    expect_identical(updated$status, c("failed", "ready"))
    expect_identical(
        updated$missing_reason,
        c("bounded target failed", NA_character_)
    )
})

test_that("shift_extract() fallback policy is available from collected files", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    docs <- esgf_test__file_docs(
        basename(nc),
        download_url = sprintf("https://example.org/%s", basename(nc)),
        include_opendap = FALSE
    )
    docs$size <- file.info(nc)$size
    docs$checksum <- NA_character_
    docs$checksum_type <- NA_character_

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = get_cache_epw())
    files <- shift_collect(req, store = tempfile("shift-store-"))
    periods <- epw_morph_periods(`2060s` = 2060L)
    time <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")

    remote_only <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "error"
    )
    expect_equal(shift_status(remote_only), "blocked")
    expect_true(any(shift_coverage(remote_only)$status %in% "failed"))
    expect_match(
        paste(shift_diagnostics(remote_only)$message, collapse = "\n"),
        "OPeNDAP is not available"
    )

    queued <- shift_download(files, run = FALSE, probe = FALSE)
    task <- data.table::as.data.table(queued)[1L]
    target <- task$target_path[[1L]]
    if (!shift_test__is_absolute_path(target)) {
        target <- file.path(shift_store(files)$path, target)
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(nc, target, overwrite = TRUE))

    local_fallback <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "auto"
    )
    expect_equal(shift_status(local_fallback), "extracted")
    expect_true(all(shift_coverage(local_fallback)$complete))
})
