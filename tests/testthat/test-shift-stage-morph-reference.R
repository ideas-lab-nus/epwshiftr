# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

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
    plan_reference <- shift_reference_from_plan(
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

# vim: fdm=marker :
