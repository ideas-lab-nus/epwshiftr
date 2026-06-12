test_that("esgf_query() compatibility wrapper preserves legacy shapes", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- FALSE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    calls <- list()

    dataset_response <- list(
        responseHeader = list(params = list(fq = c("type:Dataset", "latest:True"))),
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
                version = 20240101L,
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
        responseHeader = list(params = list(fq = c("type:File", "latest:True"))),
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
                version = 20240101L,
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
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            expect_false(all)
            expect_false(constraints)

            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                limit = limit
            )

            type <- query_param_value(params$flat()$type)
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
    expect_match(calls[[1L]]$index_node, "esgf-node\\.ornl\\.gov/esgf-1-5-bridge$")
    expect_type(qd$version, "character")
    fq_qd <- unlist(attr(qd, "response")$responseHeader$params$fq)
    expect_true("replica:false" %in% fq_qd)
    expect_false(any(grepl("^latest:", fq_qd)))

    resolution_param <- calls[[1L]]$params$nominal_resolution()
    resolution_value <- query_param_value(resolution_param)
    expect_identical(resolution_value, c("100km", "50km", "100+km", "50+km"))
    expect_true(resolution_param@encoded)

    qf <- suppressWarnings(esgf_query(
        variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L,
        type = "File", host = "https%3A%2F%2Fesgf.ceda.ac.uk%2Fesg-search%2Fsearch%2F"
    ))
    expect_named(qf, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start", "datetime_end",
        "file_size", "data_node", "file_url", "tracking_id"
    ))
    expect_named(attr(qf, "response")$response$docs, RES_FILE)
    expect_identical(calls[[2L]]$index_node, "https://esgf.ceda.ac.uk")
    expect_identical(query_param_value(calls[[2L]]$params$type()), "Dataset")
    expect_identical(calls[[3L]]$index_node, "https://esgf.ceda.ac.uk")
    expect_identical(query_param_value(calls[[3L]]$params$type()), "File")
    expect_setequal(calls[[3L]]$required_fields, EsgResultFile$private_fields$required_fields)
    expect_type(qf$version, "character")
    fq_qf <- unlist(attr(qf, "response")$responseHeader$params$fq)
    expect_true("replica:false" %in% fq_qf)
    expect_false(any(grepl("^latest:", fq_qf)))
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
    expect_true("replica:false" %in% unlist(attr(q, "response")$responseHeader$params$fq))
    expect_false(any(grepl("^latest:", unlist(attr(q, "response")$responseHeader$params$fq))))
    empty_response$responseHeader$params$fq <- c("type:Dataset", "replica:false")
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
