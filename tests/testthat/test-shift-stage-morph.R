# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("humidity fallback persists a canonical hurs extraction artifact", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    inputs <- c("huss", "tas", "ps")
    variables <- c(inputs, "snd")
    paths <- stats::setNames(
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
    on.exit(unlink(paths), add = TRUE)
    docs <- data.table::rbindlist(
        lapply(variables, function(variable) {
            row <- esgf_test__file_docs(
                basename(paths[[variable]]),
                opendap_url = paths[[variable]],
                download_url = paths[[variable]],
                variable_id = variable
            )
            row$master_id <- sprintf("humidity-%s", variable)
            row$tracking_id <- sprintf("hdl:test/humidity-%s", variable)
            row$id <- sprintf("humidity-%s|dataset", variable)
            row
        }),
        fill = TRUE
    )
    # Model the production layout where optional snow depth is a separate
    # complete identity that has no atmospheric humidity source variables.
    docs[
        variable_id == "snd",
        `:=`(
            frequency = "mon",
            table_id = "LImon"
        )
    ]
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test__mock_collect(docs, calls)

    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, epw = get_cache_epw())
    climate <- request |>
        shift_collect(store = tempfile("shift-derived-hurs-store-")) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            fallback = "error"
        )
    derived <- shift_climate__derive_hurs_climate(
        climate,
        epw_morph_recipe("original_morphing")
    )
    coverage <- shift_coverage(derived)
    hurs <- coverage[variable_id == "hurs"]
    snd <- coverage[variable_id == "snd"]
    data <- shift_data(derived, variables = "hurs")

    expect_equal(nrow(hurs), 1L)
    expect_true(hurs$complete[[1L]])
    expect_equal(nrow(snd), 1L)
    expect_identical(snd$table_id[[1L]], "LImon")
    expect_true(all(data$units == "%"))
    expect_true(all(is.finite(data$value)))
    expect_true(all(data$derived_from == "huss,tas,ps"))
    expect_true(all(data$value > 0 & data$value < 150))

    store <- shift_store(derived)
    artifact_id <- store$query(sprintf(
        "SELECT artifact_id FROM extraction_result WHERE plan_id = %s LIMIT 1",
        ddb_literal(priv(store)$conn, hurs$plan_id[[1L]])
    ))$artifact_id[[1L]]
    artifact <- store$query(sprintf(
        "SELECT metadata_json FROM artifact WHERE artifact_id = %s",
        ddb_literal(priv(store)$conn, artifact_id)
    ))
    expect_match(artifact$metadata_json[[1L]], "huss,tas,ps")

    reused <- shift_climate__derive_hurs_climate(
        derived,
        epw_morph_recipe("original_morphing"),
        resume = TRUE
    )
    expect_equal(shift_ids(reused)$plan_id, shift_ids(derived)$plan_id)
})

test_that("shift_future_epw() completes baseline and explicit-reference scenario cases", {
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

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
    on.exit(unlink(c(future_nc, reference_nc)), add = TRUE)

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
        .package = "epwshiftr"
    )

    store_path <- tempfile("shift-run-store-")
    output_dir <- tempfile("shift-run-output-")
    baseline_reference_run <- shift_future_epw(
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

    run <- shift_future_epw(
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
            nrow(morpher__private_store(shift_store(run))$read_table(table)) >=
                1L
        },
        logical(1L)
    )))
    expect_equal(nrow(shift_runs(store_path)), 1L)
    expect_equal(
        shift_status(shift_run_get(shift_ids(run)$run_id, store_path)),
        "completed"
    )
    expect_equal(shift_ids(shift_resume(run))$run_id, shift_ids(run)$run_id)
    delivery_files <- list.files(output_dir, recursive = TRUE, all.files = TRUE)
    expect_false(any(grepl("\\.(duckdb|parquet|json)$", delivery_files)))

    calls$future_scenarios <- "ssp585"
    missing_store <- tempfile("shift-default-missing-store-")
    missing_run <- (shift_future_epw(
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

    partial <- shift_future_epw(
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
    failed_run <- (shift_future_epw(
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

test_that("shift_morph() uses complete extraction plans by default", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    good <- esgf_test__file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc,
        variable_id = "tas"
    )
    bad <- esgf_test__file_docs(
        "hurs_missing_opendap.nc",
        opendap_url = "https://example.org/hurs_missing_opendap.nc",
        download_url = "https://example.org/hurs_missing_opendap.nc",
        variable_id = "hurs",
        include_opendap = FALSE
    )
    docs <- data.table::rbindlist(list(good, bad), fill = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "day"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    climate <- req |>
        shift_collect(
            store = tempfile("shift-store-"),
            label = "complete-subset"
        ) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            fallback = "error"
        )

    coverage <- shift_coverage(climate)
    expect_true(any(coverage$complete))
    expect_true(any(!coverage$complete))

    transform <- daily_transform("epwshiftr")
    morphed <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE
    )
    blocked <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE,
        complete_only = FALSE,
        overwrite = TRUE
    )

    expect_equal(
        shift_ids(morphed)$plan_id,
        coverage$plan_id[coverage$complete]
    )
    expect_true(any(
        shift_diagnostics(morphed)$code %in% "ignored_incomplete_extraction"
    ))
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(blocked), "blocked")
    expect_equal(shift_status(shift_complete(morphed)), "partial")
})

test_that("shift_morph() resolves automatic and manual historical references", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    variables <- epw_morph_variables(
        transform__recipe(monthly_transform("original_morphing"))
    )
    future_nc <- stats::setNames(
        vapply(
            variables,
            function(variable_id) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable_id
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
                    variable_id = variable_id
                )
                path
            },
            character(1L)
        ),
        variables
    )
    on.exit(unlink(c(future_nc, reference_nc)), add = TRUE)

    future_docs <- data.table::rbindlist(
        lapply(variables, function(variable_id) {
            docs <- esgf_test__file_docs(
                basename(future_nc[[variable_id]]),
                opendap_url = future_nc[[variable_id]],
                download_url = future_nc[[variable_id]],
                variable_id = variable_id
            )
            docs$frequency <- "mon"
            docs$table_id <- "Amon"
            docs
        }),
        fill = TRUE
    )
    reference_docs <- data.table::rbindlist(
        lapply(variables, function(variable_id) {
            docs <- esgf_test__file_docs(
                basename(reference_nc[[variable_id]]),
                opendap_url = reference_nc[[variable_id]],
                download_url = reference_nc[[variable_id]],
                variable_id = variable_id,
                datetime_start = "1995-01-01T00:00:00Z",
                datetime_end = "1995-12-31T23:59:59Z"
            )
            docs$frequency <- "mon"
            docs$table_id <- "Amon"
            docs
        }),
        fill = TRUE
    )
    future_docs[, `:=`(
        dataset_id = paste0("future-", variable_id),
        master_id = paste0("future-", variable_id),
        instance_id = paste0("future-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/future-", variable_id),
        id = paste0(title, "|future-", variable_id)
    )]
    reference_docs[, `:=`(activity_id = "CMIP", experiment_id = "historical")]
    reference_docs[, `:=`(
        dataset_id = paste0("historical-", variable_id),
        master_id = paste0("historical-", variable_id),
        instance_id = paste0("historical-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/historical-", variable_id),
        id = paste0(title, "|historical-", variable_id)
    )]
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test__mock_collect_sequence(list(future_docs, reference_docs), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "mon"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    store_path <- tempfile("shift-store-")
    future_periods <- epw_morph_periods(`2060s` = 2060L)
    reference_periods <- epw_morph_periods(reference = 1995L)

    climate <- req |>
        shift_collect(store = store_path, label = "future") |>
        shift_extract(
            site = site,
            periods = future_periods,
            variables = variables
        )

    transform <- monthly_transform("original_morphing")
    recipe <- transform__recipe(transform)
    collect_count_before_baseline <- length(calls$collect_times)
    baseline_reference <- shift_morph(
        climate,
        transform = monthly_transform("epwshiftr"),
        strict = TRUE,
        overwrite = TRUE
    )
    expect_true(S7::S7_inherits(baseline_reference, ShiftMorphed))
    expect_null(baseline_reference@meta$reference)
    expect_equal(length(calls$collect_times), collect_count_before_baseline)
    morpher <- morpher__from_recipe(
        epw = get_cache_epw(),
        store = shift_store(climate),
        recipe = recipe
    )
    missing_reference <- morpher$preflight(
        plan_id = shift_ids(climate)$plan_id,
        periods = future_periods,
        strict = FALSE
    )
    expect_true(any(missing_reference$code == "missing_reference_climate"))
    auto <- shift_morph(
        climate,
        transform = transform,
        reference = shift_reference_historical(reference_periods),
        strict = TRUE,
        overwrite = TRUE
    )
    historical_collect_times <- calls$collect_times[3:4]
    expect_equal(
        vapply(historical_collect_times, `[[`, character(1L), "type"),
        c("Dataset", "File")
    )
    expect_true(all(vapply(
        historical_collect_times,
        function(x) {
            is.null(x$datetime_start) && is.null(x$datetime_stop)
        },
        logical(1L)
    )))
    reference_climate <- auto@meta$reference
    reference_ids <- shift_ids(reference_climate)
    plan_reference <- shift_reference_plan(
        reference_ids$plan_id,
        reference_periods
    )
    manual <- shift_morph(
        climate,
        transform = transform,
        reference = reference_climate,
        strict = TRUE
    )
    manual_plan <- shift_morph(
        climate,
        transform = transform,
        reference = plan_reference,
        strict = TRUE
    )

    expect_true(S7::S7_inherits(auto, ShiftMorphed))
    expect_true(S7::S7_inherits(reference_climate, ShiftClimate))
    expect_true(S7::S7_inherits(auto@meta$reference_spec, ShiftReferenceSpec))
    expect_equal(auto@meta$reference_spec@mode, "historical")
    expect_equal(shift_status(auto), "morphed")
    expect_equal(shift_status(reference_climate), "extracted")
    reference_rows <- shift_inspect__extraction_result_rows(
        shift_store(reference_climate),
        reference_ids$plan_id
    )
    expect_equal(unique(reference_rows$experiment_id), "historical")
    expect_equal(shift_status(manual), "morphed")
    expect_equal(shift_status(manual_plan), "morphed")
    expect_error(
        shift_morph(
            climate,
            transform = monthly_transform("epwshiftr"),
            observed_reference = reference_climate
        ),
        "does not use.*observed_reference"
    )
    expect_true(sum(calls$values %in% "File") >= 2L)
})
