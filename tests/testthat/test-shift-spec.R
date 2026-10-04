# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
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
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = "baseline.epw"
    )
    site_from_path <- shift_site(epw = get_cache_epw(), id = "SIN")
    site_from_first_arg <- shift_site(get_cache_epw())
    site_from_epw <- shift_site(epw_file_read(get_cache_epw()))
    site_from_external_epw <- shift_site(test_external_epw(get_cache_epw()))

    expect_true(S7::S7_inherits(req, ShiftRequest))
    expect_true(S7::S7_inherits(site, ShiftSite))
    expect_true(S7::S7_inherits(site_from_path, ShiftSite))
    expect_equal(shift_status(req), "new")
    expect_equal(shift_status(site), "new")
    expect_equal(
        req@meta$time,
        c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    expect_equal(site_from_path@lon, 103.98)
    expect_equal(site_from_path@lat, 1.37)
    expect_equal(site_from_first_arg@id, "SGP_Singapore.486980_IWEC")
    expect_equal(site_from_first_arg@label, "Singapore")
    expect_equal(site_from_epw@id, "486980")
    expect_equal(site_from_epw@lon, 103.98)
    expect_equal(site_from_epw@lat, 1.37)
    expect_true(inherits(site_from_external_epw@epw, "EpwFile"))
    expect_equal(site_from_external_epw@id, "486980")
    expect_named(shift_diagnostics(req), shift_stage__diagnostic_columns())
    expect_equal(data.table::as.data.table(req)$variables, "tas,hurs")
    expect_true(data.table::as.data.table(site)$has_epw)
})

test_that("shift_request() applies ESGF control filters through typed setters", {
    req <- shift_request(
        project = "CMIP6",
        variables = "tas",
        filters = list(latest = TRUE, replica = FALSE, table_id = "Amon")
    )
    query <- shift_resolve__as_query(req)

    expect_true(query_param__value(query$latest()))
    expect_false(query_param__value(query$replica()))
    expect_identical(query_param__value(query$params()$table_id), "Amon")
})

test_that("shift_request() preserves provider facet values", {
    req <- shift_request(
        project = "cmip6",
        frequency = c("monthly", "daily", "3hr")
    )
    query <- shift_resolve__as_query(req)

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
    expect_identical(shift_print__format_cmip6_tables(NULL), "auto by variable")
    expect_identical(shift_print__format_cmip6_tables("Amon"), "Amon (forced)")
    expect_identical(
        shift_print__format_cmip6_tables(c(snd = "LImon")),
        "auto by variable · snd=LImon"
    )
    expect_identical(
        shift_print__format_cmip6_frequencies(c(tas = "3hrPt", rsds = "3hr")),
        "tas=3hrPt | rsds=3hr"
    )
    selection <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        partition_key = "Amon=gn;LImon=gr"
    )
    expect_identical(
        shift_print__format_cmip6_partitions(selection),
        "Amon=gn · LImon=gr"
    )

    climate <- shift_cmip6(
        "BCC-CSM2-MR",
        "ssp585",
        table = c(snd = "LImon")
    )
    encoded <- shift_persist__spec_json(list(
        climate = shift_persist__climate_spec_value(climate)
    ))
    decoded <- jsonlite::fromJSON(encoded, simplifyVector = TRUE)$climate
    expect_identical(decoded$table$snd, "LImon")
    expect_identical(
        shift_persist__climate_from_spec(decoded)@table,
        c(snd = "LImon")
    )

    hourly_climate <- shift_cmip6(
        "BCC-CSM2-MR",
        "ssp585",
        frequency = c(tas = "3hrPt", rsds = "3hr")
    )
    hourly_encoded <- shift_persist__spec_json(list(
        climate = shift_persist__climate_spec_value(hourly_climate)
    ))
    hourly_decoded <- jsonlite::fromJSON(
        hourly_encoded,
        simplifyVector = TRUE
    )$climate
    expect_identical(
        shift_persist__climate_from_spec(hourly_decoded)@frequency,
        c(tas = "3hrPt", rsds = "3hr")
    )

    bounded <- shift_cmip6(model = 2L, scenarios = "ssp585")
    bounded_spec <- shift_persist__climate_spec_value(bounded)
    expect_identical(
        shift_persist__climate_from_spec(bounded_spec)@n_models,
        2L
    )
    all_models <- shift_cmip6(model = NULL, scenarios = "ssp585")
    all_spec <- shift_persist__climate_spec_value(all_models)
    expect_null(shift_persist__climate_from_spec(all_spec)@n_models)

    request <- shift_cmip6_scenario(
        source = "BCC-CSM2-MR",
        scenario = "ssp585",
        variables = c("tas", "rsds"),
        frequency = c(tas = "3hrPt", rsds = "3hr")
    )
    request_encoded <- shift_persist__spec_json(
        shift_persist__request_spec_value(request)
    )
    request_decoded <- jsonlite::fromJSON(
        request_encoded,
        simplifyVector = TRUE
    )
    expect_identical(
        shift_persist__request_frequency_from_spec(request_decoded$frequency),
        c(tas = "3hrPt", rsds = "3hr")
    )
    expect_null(shift_spec__cmip6_request_table_spec(c("3hr", "day")))
    expect_identical(shift_spec__cmip6_request_table_spec("3hr"), "3hr")
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
    expect_equal(
        req@meta$time,
        c("2055-01-01T00:00:00Z", "2065-12-31T23:59:59Z")
    )
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
    expect_true(all(
        c("request", "transform", "reference", "output") %in% explain$step
    ))
    expect_match(explain$detail[explain$step == "request"], "BCC-CSM2-MR")
})

test_that("target-year vectors expand to independently named periods", {
    periods <- shift_spec__periods_from_input(c(2050, 2080))

    expect_identical(periods$period, c("2050", "2080"))
    expect_identical(periods$year, c(2050L, 2080L))
    expect_error(
        shift_spec__periods_from_input(c(2050, 2050)),
        "duplicated"
    )
})

test_that("weather transforms remain reusable and validate execution references", {
    historical <- historical_reference(1995:2014)
    manual <- shift_reference_plan(
        "plan-reference",
        epw_morph_periods(reference = 1995L)
    )

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
    spec <- shift_persist__plan_spec(plan)
    expect_null(spec$request)
    expect_equal(spec$climate$model, "EC-Earth3")
    expect_equal(spec$climate$scenarios, "ssp585")
    expect_true(S7::S7_inherits(
        shift_persist__plan_from_spec(spec)@meta$climate,
        ShiftCmip6Spec
    ))
    legacy_spec <- spec
    legacy_spec$version <- 1L
    expect_error(
        shift_persist__plan_from_spec(legacy_spec),
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

test_that("R and CLI share strict year syntax", {
    for (value in c(
        "1995.5",
        "1995:1996.5",
        "1995::1996",
        "1995,x",
        "1995,",
        "1995,,1996",
        "",
        " "
    )) {
        expect_error(historical_reference(value), "year|character")
        expect_error(epwshiftr_cli_years(value), "year|character")
    }
    expected <- c(1995L, 1996L, 2001L, 2002L)
    expect_identical(
        historical_reference("1996:1995,2001:2002")@periods$year,
        expected
    )
    expect_identical(epwshiftr_cli_years("1996:1995,2001:2002"), expected)
    expect_error(historical_reference(1995.5), "integer")
    expect_error(historical_reference(NA_character_), "missing")
})

test_that("CMIP6 fields remain valid after construction", {
    climate <- shift_cmip6("Model-A", "ssp245")
    expect_error(climate@member <- c("r1i1p1f1", NA_character_), "member")
    expect_error(climate@grid <- c("gn", "gr"), "grid")
    expect_error(climate@scenarios <- character(), "scenarios")
    expect_error(climate@common <- c(TRUE, FALSE), "common")
    expect_error(climate@frequency <- c("day", "mon"), "frequency")
    expect_error(climate@table <- c(tas = "Amon", tas = "Amon"), "table")
    expect_error(shift_cmip6("", "ssp245"), "model")
})

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
