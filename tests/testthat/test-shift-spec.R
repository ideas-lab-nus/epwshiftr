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
    expect_equal(nrow(shift_diagnostics(req)), 0L)
    expect_equal(data.table::as.data.table(req)$variables, "tas,hurs")
    expect_true(data.table::as.data.table(site)$has_epw)
})

spec_test__future_plan <- function(
    climate,
    periods = list(`2060s` = 2060L),
    transform = monthly_transform("epwshiftr"),
    site = shift_site(epw = get_cache_epw()),
    store = tempfile("shift-store-"),
    dir = tempfile("future-epw-")
) {
    shift_epw_future(
        sites = site,
        climate = climate,
        periods = periods,
        transform = transform,
        dir = dir,
        control = shift_control(strict = FALSE),
        store = store,
        dry_run = TRUE
    )@meta$children[[1L]]
}

test_that("shift_request() applies typed ESGF controls and preserves provider facets", {
    req <- shift_request(
        project = "CMIP6",
        variables = "tas",
        filters = list(latest = TRUE, replica = FALSE, table_id = "Amon")
    )
    query <- shift_resolve__as_query(req)

    expect_true(query_param__value(query$latest()))
    expect_false(query_param__value(query$replica()))
    expect_identical(query_param__value(query$params()$table_id), "Amon")

    provider_req <- shift_request(
        project = "cmip6",
        frequency = c("monthly", "daily", "3hr")
    )
    provider_query <- shift_resolve__as_query(provider_req)
    expect_identical(provider_req@meta$project, "cmip6")
    expect_identical(provider_req@meta$frequency, c("monthly", "daily", "3hr"))
    expect_identical(query_param__value(provider_query$project()), "cmip6")
    expect_identical(
        query_param__value(provider_query$frequency()),
        c("monthly", "daily", "3hr")
    )
})

test_that("shift_cmip6() handles positional member and explicit or automatic model selection", {
    positional <- shift_cmip6("BCC-CSM2-MR", "ssp585", "r1i1p1f1")
    expect_identical(positional@model, "BCC-CSM2-MR")
    expect_identical(positional@member, "r1i1p1f1")
    expect_null(positional@n_models)

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

    roundtrip <- function(climate) {
        encoded <- shift_persist__spec_json(list(
            climate = shift_persist__climate_spec_value(climate)
        ))
        decoded <- jsonlite::fromJSON(encoded, simplifyVector = TRUE)$climate
        shift_persist__climate_from_spec(decoded)
    }

    expect_identical(
        roundtrip(shift_cmip6(
            "BCC-CSM2-MR",
            "ssp585",
            table = c(snd = "LImon")
        ))@table,
        c(snd = "LImon")
    )
    expect_identical(
        roundtrip(shift_cmip6(
            "BCC-CSM2-MR",
            "ssp585",
            frequency = c(tas = "3hrPt", rsds = "3hr")
        ))@frequency,
        c(tas = "3hrPt", rsds = "3hr")
    )
    expect_identical(
        roundtrip(shift_cmip6(model = 2L, scenarios = "ssp585"))@n_models,
        2L
    )
    expect_null(
        roundtrip(shift_cmip6(model = NULL, scenarios = "ssp585"))@n_models
    )
    expect_null(shift_spec__cmip6_request_table_spec(c("3hr", "day")))
    expect_identical(shift_spec__cmip6_request_table_spec("3hr"), "3hr")
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
    periods <- epw_morph_periods(reference = 1995L)
    historical <- historical_reference(1995:2014)
    manual <- shift_reference_from_plan("plan-reference", periods)

    expect_equal(manual@mode, "plan")
    expect_equal(manual@plan_id, "plan-reference")
    expect_error(shift_reference_from_plan(character(), periods), "length >= 1")

    transform <- monthly_transform("original_morphing")
    expect_true(S7::S7_inherits(transform, WeatherTransformSpec))
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

test_that("shift_epw_future() validates plans, schema versions, EPW isolation, and reusable transforms", {
    transform <- monthly_transform("epwshiftr")
    mk_cmip6 <- function(model, scenarios, grid = "gr") {
        shift_cmip6(
            model,
            scenarios,
            "r1i1p1f1",
            grid = grid,
            frequency = "mon",
            table = "Amon"
        )
    }
    climate <- mk_cmip6("EC-Earth3", "ssp585")
    plan <- spec_test__future_plan(climate, transform = transform)

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_true(S7::S7_inherits(plan@meta$climate, ShiftCmip6Spec))
    expect_equal(
        c(plan@meta$climate@model, plan@meta$climate@scenarios),
        c("EC-Earth3", "ssp585")
    )
    expect_equal(shift_status(plan), "planned")
    expect_equal(nrow(shift_cases(plan)), 1L)
    spec <- shift_persist__plan_spec(plan)
    expect_null(spec$request)
    expect_equal(
        c(spec$climate$model, spec$climate$scenarios),
        c("EC-Earth3", "ssp585")
    )
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
    orig_path <- external$path()
    ext_plan <- spec_test__future_plan(
        climate,
        transform = transform,
        site = shift_site(epw = external),
        store = external_store
    )
    expect_true(inherits(ext_plan@meta$site@epw, "EpwFile"))
    expect_true(startsWith(
        ext_plan@meta$site@epw$path(),
        normalizePath(external_store, winslash = "/", mustWork = TRUE)
    ))
    expect_identical(external$path(), orig_path)

    expect_error(
        spec_test__future_plan(
            shift_cmip6("EC-Earth3", "ssp585"),
            transform = "original_morphing"
        ),
        "WeatherTransformSpec"
    )
    expect_error(
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            model = "EC-Earth3",
            scenarios = "ssp585",
            periods = list(`2060s` = 2060L),
            transform = transform,
            dir = tempfile("future-epw-"),
            dry_run = TRUE
        ),
        "unused arguments"
    )

    # Planning binds run-specific models/scenarios/periods without mutating transform
    first <- spec_test__future_plan(
        mk_cmip6("EC-Earth3", "ssp126"),
        periods = 2050,
        transform = transform
    )
    second <- spec_test__future_plan(
        mk_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585"), "gn"),
        periods = c(2050, 2080),
        transform = transform
    )
    expect_identical(first@meta$transform, transform)
    expect_identical(second@meta$transform, transform)
    expect_identical(transform@method, "epwshiftr")
    expect_identical(first@meta$periods$period, "2050")
    expect_setequal(second@meta$periods$period, c("2050", "2080"))
    expect_equal(
        c(nrow(shift_cases(first)), nrow(shift_cases(second))),
        c(1L, 4L)
    )
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
    bad <- list(
        member = c("r1i1p1f1", NA_character_),
        grid = c("gn", "gr"),
        scenarios = character(),
        common = c(TRUE, FALSE),
        frequency = c("day", "mon"),
        table = c(tas = "Amon", tas = "Amon")
    )
    for (field in names(bad)) {
        expect_error(S7::prop(climate, field) <- bad[[field]], field)
    }
    expect_error(shift_cmip6("", "ssp245"), "model")
})

# Provide independent real scenario/reference fixtures and query counters.
# Compact native templates are reused only inside this R process; each test
# receives separate files, and its mock and file cleanup share its lifetime.
# spec_test__scenario_inputs {{{
spec_test__scenario_inputs <- function(.local_envir = parent.frame()) {
    # Exercise the production fallback for both future scenarios and the
    # explicit historical reference instead of supplying direct hurs.
    original_morphing_recipe <- transform__recipe(monthly_transform(
        "original_morphing"
    ))
    variables <- unique(c(
        setdiff(epw_morph_variables(original_morphing_recipe), "hurs"),
        "huss",
        "ps"
    ))
    write_year_nc <- function(year) {
        stats::setNames(
            vapply(
                variables,
                function(variable_id) {
                    path <- tempfile(fileext = ".nc")
                    write_local_cmip6_netcdf_fixture(
                        path,
                        year,
                        variable_id = variable_id,
                        frequency = "mon"
                    )
                    path
                },
                character(1L)
            ),
            variables
        )
    }
    future_nc <- write_year_nc(2060L)
    reference_nc <- write_year_nc(1995L)
    withr::defer(unlink(c(future_nc, reference_nc)), envir = .local_envir)

    # Represent each scenario-variable pair with a distinct ESGF identity while
    # reusing compact local NetCDF fixtures for the two scenario catalogs.
    workflow_docs <- function(paths, experiments, activity, start, end) {
        data.table::rbindlist(
            lapply(experiments, function(experiment_id) {
                data.table::rbindlist(
                    lapply(names(paths), function(variable_id) {
                        docs <- esgf_test__file_docs(
                            basename(paths[[variable_id]]),
                            opendap_url = paths[[variable_id]],
                            download_url = paths[[variable_id]],
                            variable_id = variable_id,
                            datetime_start = start,
                            datetime_end = end
                        )
                        docs$checksum <- checksum_file(paths[[variable_id]])
                        docs$activity_id <- activity
                        docs$source_id <- "BCC-CSM2-MR"
                        docs$experiment_id <- experiment_id
                        docs$grid_label <- "gn"
                        docs$frequency <- "mon"
                        docs$table_id <- "Amon"
                        docs$dataset_id <- sprintf(
                            "dataset-%s-%s",
                            experiment_id,
                            variable_id
                        )
                        docs$master_id <- sprintf(
                            "master-%s-%s",
                            experiment_id,
                            variable_id
                        )
                        docs$instance_id <- sprintf(
                            "instance-%s-%s.v1",
                            experiment_id,
                            variable_id
                        )
                        docs$tracking_id <- sprintf(
                            "hdl:test/%s-%s",
                            experiment_id,
                            variable_id
                        )
                        docs$id <- sprintf(
                            "%s-%s|%s",
                            experiment_id,
                            variable_id,
                            docs$dataset_id
                        )
                        docs
                    }),
                    fill = TRUE
                )
            }),
            fill = TRUE
        )
    }
    future_docs <- workflow_docs(
        future_nc,
        c("ssp126", "ssp585"),
        "ScenarioMIP",
        "2060-01-01T00:00:00Z",
        "2060-12-31T23:59:59Z"
    )
    reference_docs <- workflow_docs(
        reference_nc,
        "historical",
        "CMIP",
        "1995-01-01T00:00:00Z",
        "1995-12-31T23:59:59Z"
    )

    calls <- new.env(parent = emptyenv())
    calls$file_calls <- 0L
    calls$historical_file_calls <- 0L
    calls$future_scenarios <- c("ssp126", "ssp585")
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            type <- query_param__value(params$type())
            experiments <- as.character(shift_test__param_value(
                params,
                "experiment_id"
            ))
            variables_requested <- as.character(shift_test__param_value(
                params,
                "variable_id"
            ))
            experiments <- experiments[
                !is.na(experiments) & nzchar(experiments)
            ]
            variables_requested <- variables_requested[
                !is.na(variables_requested) & nzchar(variables_requested)
            ]
            if (identical(type, "Dataset")) {
                # File discovery is constrained through the selected Dataset
                # identity, so remember the preceding Dataset experiments for
                # the subsequent mocked File request.
                calls$dataset_experiments <- experiments
            }
            requested_experiments <- if (length(experiments)) {
                experiments
            } else {
                calls$dataset_experiments
            }
            docs <- if (identical(type, "Dataset")) {
                dataset <- esgf_test__dataset_docs(
                    if (length(variables_requested)) {
                        variables_requested[[1L]]
                    } else {
                        "tas"
                    }
                )
                dataset$source_id <- "BCC-CSM2-MR"
                dataset$experiment_id <- if (length(experiments)) {
                    experiments[[1L]]
                } else {
                    "ssp585"
                }
                dataset$frequency <- "mon"
                dataset
            } else {
                calls$file_calls <- calls$file_calls + 1L
                # Select historical fixtures only when the method explicitly
                # requested that experiment; baseline-reference runs never do.
                historical <- "historical" %in% requested_experiments
                if (historical) {
                    calls$historical_file_calls <- calls$historical_file_calls +
                        1L
                }
                catalog <- if (historical) reference_docs else future_docs
                if (!historical) {
                    catalog <- catalog[
                        catalog$experiment_id %in% calls$future_scenarios
                    ]
                }
                if (length(requested_experiments) && !historical) {
                    catalog <- catalog[
                        catalog$experiment_id %in% requested_experiments
                    ]
                }
                if (length(variables_requested)) {
                    catalog <- catalog[
                        catalog$variable_id %in% variables_requested
                    ]
                }
                as.data.frame(catalog)
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = .local_envir
    )

    calls
}
# }}}

# spec_test__future_run {{{
spec_test__future_run <- function(
    transform = monthly_transform("original_morphing"),
    reference = historical_reference(1995L),
    control = shift_control(strict = TRUE, overwrite = TRUE),
    dir = tempfile("shift-run-output-"),
    store = tempfile("shift-run-store-"),
    ...
) {
    shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = transform,
        reference = reference,
        dir = dir,
        control = control,
        store = store,
        ...
    )@meta$children[[1L]]
}
# }}}

test_that("shift_epw_future() completes baseline scenarios without historical discovery", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- spec_test__scenario_inputs()

    baseline_reference_run <- spec_test__future_run(
        transform = monthly_transform("epwshiftr"),
        reference = NULL
    )
    expect_equal(shift_status(baseline_reference_run), "completed")
    expect_equal(nrow(shift_outputs(baseline_reference_run)), 2L)
    expect_equal(calls$historical_file_calls, 0L)
})

test_that("shift_epw_future() preserves strict and partial missing-scenario outcomes", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- spec_test__scenario_inputs()

    calls$future_scenarios <- "ssp585"
    missing_run <- spec_test__future_run(
        control = shift_control(),
        ui = shift_ui("none")
    )
    expect_identical(shift_status(missing_run), "failed")
    missing_diagnostics <- shift_diagnostics(missing_run)
    expect_true("shift_resolver_exhausted" %in% missing_diagnostics$code)
    expect_true(any(grepl("ssp126", missing_diagnostics$message, fixed = TRUE)))

    partial <- spec_test__future_run(
        control = shift_control(
            strict = TRUE,
            allow_partial = TRUE,
            overwrite = TRUE
        )
    )
    expect_equal(shift_status(partial), "partial")
    expect_equal(nrow(shift_outputs(partial)), 1L)
    expect_equal(nrow(shift_missing(partial)), 1L)
    expect_equal(shift_missing(partial)$experiment_id, "ssp126")
})

test_that("shift_epw_future() resumes referenced scenarios after an export interruption", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- spec_test__scenario_inputs()

    calls$future_scenarios <- c("ssp126", "ssp585")
    export_attempts <- 0L
    original_export <- shift_export__export_outputs
    testthat::local_mocked_bindings(
        shift_export__export_outputs = function(...) {
            export_attempts <<- export_attempts + 1L
            if (export_attempts == 1L) {
                stop("simulated interruption after morphing", call. = FALSE)
            }
            original_export(...)
        },
        .package = "epwshiftr"
    )
    failed_run <- spec_test__future_run()
    expect_equal(shift_status(failed_run), "failed")
    expect_false(is.na(failed_run@meta$run$completed_at[[1L]]))
    expect_gt(nrow(shift_logs(failed_run)), 0L)
    file_calls_before_resume <- calls$file_calls
    resumed <- shift_resume(failed_run)
    expect_equal(shift_status(resumed), "completed")
    expect_equal(calls$file_calls, file_calls_before_resume)
    expect_equal(nrow(shift_outputs(resumed)), 2L)
})

test_that("shift_epw_future() completes an explicit historical reference run", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- spec_test__scenario_inputs()

    output_dir <- tempfile("shift-run-output-")
    run <- spec_test__future_run(dir = output_dir)
    store_path <- run@store_path

    expect_equal(shift_status(run), "completed")
    expect_equal(nrow(shift_outputs(run)), 2L)
    expect_equal(nrow(shift_missing(run)), 0L)
    expect_true(all(file.exists(shift_outputs(run)$export_path)))
    expect_true(all(vapply(
        shift_outputs(run)$export_path,
        function(path) {
            inherits(epw_file_read(path), "EpwFile")
        },
        logical(1L)
    )))
    expect_equal(calls$historical_file_calls, 1L)
    run_tables <- c("shift_run", "shift_run_case", "shift_run_event")
    expect_true(all(vapply(
        run_tables,
        function(table) {
            nrow(morpher__private_store(shift_inspect__store(run))$read_table(
                table
            )) >=
                1L
        },
        logical(1L)
    )))
    expect_equal(nrow(shift_history(store_path, type = "run")), 1L)
    expect_equal(
        shift_status(shift_run_get(shift_inspect__ids(run)$run_id, store_path)),
        "completed"
    )
    expect_equal(
        shift_inspect__ids(shift_resume(run))$run_id,
        shift_inspect__ids(run)$run_id
    )
    delivery_files <- list.files(output_dir, recursive = TRUE, all.files = TRUE)
    expect_false(any(grepl("\\.(duckdb|parquet|json)$", delivery_files)))
})

# vim: fdm=marker :
