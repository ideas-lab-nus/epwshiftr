test_that("esgf_query() compatibility wrapper preserves legacy shapes", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- FALSE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    dataset_response <- list(
        responseHeader = list(params = list(fq = "type:Dataset")),
        response = list(
            numFound = 1L,
            docs = data.frame(
                id = "dataset-id",
                mip_era = "CMIP6",
                activity_drs = "ScenarioMIP",
                institution_id = "ECMWF",
                source_id = "EC-Earth3",
                experiment_id = "ssp585",
                member_id = "r1i1p1f1",
                table_id = "day",
                frequency = "day",
                grid_label = "gr",
                version = "v20240101",
                nominal_resolution = "100 km",
                variable_id = "tas",
                variable_long_name = "Near-Surface Air Temperature",
                variable_units = "K",
                data_node = "example.org",
                pid = "hdl:21.14100/mock-dataset",
                extra = "drop-me",
                stringsAsFactors = FALSE
            )
        )
    )
    file_response <- list(
        responseHeader = list(params = list(fq = "type:File")),
        response = list(
            numFound = 1L,
            docs = data.frame(
                id = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                dataset_id = "dataset-id",
                mip_era = "CMIP6",
                activity_drs = "ScenarioMIP",
                institution_id = "ECMWF",
                source_id = "EC-Earth3",
                experiment_id = "ssp585",
                member_id = "r1i1p1f1",
                table_id = "day",
                frequency = "day",
                grid_label = "gr",
                version = "v20240101",
                nominal_resolution = "100 km",
                variable_id = "tas",
                variable_long_name = "Near-Surface Air Temperature",
                variable_units = "K",
                data_node = "example.org",
                size = "42",
                url = I(list("https://example.org/tas.nc|HTTPServer|application/netcdf")),
                tracking_id = "tracking-id",
                extra = "drop-me",
                stringsAsFactors = FALSE
            )
        )
    )

    testthat::local_mocked_bindings(
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
            expect_match(index_node, "esgf-node\\.llnl\\.gov$")
            expect_null(required_fields)
            expect_false(all)
            expect_true(limit)
            expect_false(constraints)

            type <- params$type$value
            response <- if (identical(type, "Dataset")) dataset_response else file_response
            list(response = response, docs = response$response$docs)
        },
        .package = "epwshiftr"
    )

    expect_warning(
        qd <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L),
        "deprecated"
    )
    expect_named(qd, c(
        "dataset_id", "mip_era", "activity_drs", "institution_id", "source_id",
        "experiment_id", "member_id", "table_id", "frequency", "grid_label",
        "version", "nominal_resolution", "variable_id", "variable_long_name",
        "variable_units", "data_node", "dataset_pid"
    ))
    expect_named(attr(qd, "response")$response$docs, RES_DATASET)

    qf <- suppressWarnings(esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L, type = "File"))
    expect_named(qf, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start", "datetime_end",
        "file_size", "data_node", "file_url", "tracking_id"
    ))
    expect_named(attr(qf, "response")$response$docs, RES_FILE)
})

test_that("esgf_query() keeps legacy empty-result behavior", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- TRUE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    empty_response <- list(
        responseHeader = list(params = list(fq = "type:Dataset")),
        response = list(numFound = 0L, docs = data.frame(stringsAsFactors = FALSE))
    )

    testthat::local_mocked_bindings(
        query_collect = function(...) list(response = empty_response, docs = empty_response$response$docs),
        .package = "epwshiftr"
    )

    expect_message(
        q <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L),
        "No matched data\\. Please examine the actual response"
    )
    expect_equal(q, data.table::data.table(), ignore_attr = TRUE)
    expect_identical(attr(q, "response"), empty_response)
})

test_that("init_cmip6_index() consumes the compatibility wrapper contract", {
    calls <- character()
    qd <- data.table::data.table(
        dataset_id = "dataset-id", mip_era = "CMIP6", activity_drs = "ScenarioMIP",
        institution_id = "ECMWF", source_id = "EC-Earth3", experiment_id = "ssp585",
        member_id = "r1i1p1f1", table_id = "day", frequency = "day", grid_label = "gr",
        version = "v20240101", nominal_resolution = "100 km", variable_id = "tas",
        variable_long_name = "Near-Surface Air Temperature", variable_units = "K",
        data_node = "example.org", dataset_pid = "hdl:21.14100/mock-dataset"
    )
    qf <- data.table::data.table(
        file_id = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
        dataset_id = "dataset-id", mip_era = "CMIP6", activity_drs = "ScenarioMIP",
        institution_id = "ECMWF", source_id = "EC-Earth3", experiment_id = "ssp585",
        member_id = "r1i1p1f1", table_id = "day", frequency = "day", grid_label = "gr",
        version = "v20240101", nominal_resolution = "100 km", variable_id = "tas",
        variable_long_name = "Near-Surface Air Temperature", variable_units = "K",
        datetime_start = ISOdatetime(2060, 1, 1, 0, 0, 0, "UTC"),
        datetime_end = ISOdatetime(2060, 12, 31, 0, 0, 0, "UTC"),
        file_size = "42", data_node = "example.org", file_url = "https://example.org/tas.nc",
        tracking_id = "tracking-id"
    )

    testthat::local_mocked_bindings(
        esgf_query = function(..., type = "Dataset") {
            calls <<- c(calls, type)
            if (identical(type, "Dataset")) qd else qf
        },
        .package = "epwshiftr"
    )

    idx <- init_cmip6_index(variable = "tas", source = "EC-Earth3", experiment = "ssp585", years = 2060)
    expect_identical(calls, c("Dataset", "File"))
    expect_identical(idx$dataset_pid, "hdl:21.14100/mock-dataset")
    expect_named(idx, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start", "datetime_end",
        "file_size", "data_node", "file_url", "dataset_pid", "tracking_id"
    ))
})

