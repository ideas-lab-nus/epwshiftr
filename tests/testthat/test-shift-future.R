# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Provide independent real scenario/reference fixtures and query counters.
# Compact native templates are reused only inside this R process; each test
# receives separate files, and its mock and file cleanup share its lifetime.
# stage_morph_test__scenario_inputs {{{
stage_morph_test__scenario_inputs <- function(.local_envir = parent.frame()) {
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
    future_nc <- stats::setNames(
        vapply(
            variables,
            function(variable_id) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable_id,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
    reference_nc <- stats::setNames(
        vapply(
            variables,
            function(variable_id) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    1995L,
                    variable_id = variable_id,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
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

test_that("shift_epw_future() completes baseline scenarios without historical discovery", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- stage_morph_test__scenario_inputs()

    baseline_reference_run <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-run-baseline-reference-output-"),
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = tempfile("shift-run-baseline-reference-store-")
    )@meta$children[[1L]]
    expect_equal(shift_status(baseline_reference_run), "completed")
    expect_equal(nrow(shift_outputs(baseline_reference_run)), 2L)
    expect_equal(calls$historical_file_calls, 0L)
})

test_that("shift_epw_future() preserves strict and partial missing-scenario outcomes", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- stage_morph_test__scenario_inputs()

    calls$future_scenarios <- "ssp585"
    missing_store <- tempfile("shift-default-missing-store-")
    missing_run <- (shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = tempfile("shift-default-missing-output-"),
        store = missing_store,
        ui = shift_ui("none")
    )@meta$children[[1L]])
    expect_identical(shift_status(missing_run), "failed")
    missing_diagnostics <- shift_diagnostics(missing_run)
    expect_true("shift_resolver_exhausted" %in% missing_diagnostics$code)
    expect_true(any(grepl("ssp126", missing_diagnostics$message, fixed = TRUE)))

    partial <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = tempfile("shift-partial-output-"),
        control = shift_control(
            strict = TRUE,
            allow_partial = TRUE,
            overwrite = TRUE
        ),
        store = tempfile("shift-partial-store-")
    )@meta$children[[1L]]
    expect_equal(shift_status(partial), "partial")
    expect_equal(nrow(shift_outputs(partial)), 1L)
    expect_equal(nrow(shift_missing(partial)), 1L)
    expect_equal(shift_missing(partial)$experiment_id, "ssp126")
})

test_that("shift_epw_future() resumes referenced scenarios after an export interruption", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())

    calls <- stage_morph_test__scenario_inputs()

    calls$future_scenarios <- c("ssp126", "ssp585")
    resume_store <- tempfile("shift-resume-store-")
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
    failed_run <- (shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = tempfile("shift-resume-output-"),
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = resume_store
    )@meta$children[[1L]])
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

    calls <- stage_morph_test__scenario_inputs()

    store_path <- tempfile("shift-run-store-")
    output_dir <- tempfile("shift-run-output-")
    run <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = output_dir,
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = store_path
    )@meta$children[[1L]]
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
