cli_shift_test_response <- function(docs) {
    list(
        responseHeader = list(status = 0L, QTime = 0L, params = stats::setNames(list(), character())),
        response = list(numFound = nrow(docs), start = 0L, docs = docs, maxScore = 1),
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

cli_shift_test_dataset_docs <- function(variable_id = "tas") {
    data.frame(
        id = "dataset-1",
        instance_id = "dataset-1.v20260101",
        master_id = "dataset-1",
        size = 123,
        access = I(list(c("OPENDAP", "HTTPServer"))),
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variable_id = variable_id[[1L]],
        frequency = "day",
        variant_label = "r1i1p1f1",
        data_node = "example.org",
        check.names = FALSE
    )
}

cli_shift_test_file_docs <- function(path, opendap_url = path, download_url = path, variable_id = "tas") {
    docs <- data.frame(
        id = sprintf("%s|dataset-1", basename(path)),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", basename(path)),
        master_id = sprintf("%s.master", basename(path)),
        replica = FALSE,
        tracking_id = sprintf("hdl:21.14100/cli-shift-%s", variable_id),
        title = basename(path),
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

cli_shift_test_file_result <- function(docs) {
    params <- query_param__as_store(list(
        project = "CMIP6",
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
    query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = params,
        result = cli_shift_test_response(docs)
    )
}

cli_shift_test_mock_collect <- function(file_docs, calls = new.env(parent = emptyenv())) {
    calls$types <- character()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                cli_shift_test_dataset_docs(unique(file_docs$variable_id))
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- cli_shift_test_response(docs)
            calls$types <- c(calls$types, type)
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
    calls
}

cli_shift_test_config <- function(path, store = NULL, epw = get_cache_epw()) {
    config <- list(
        request = list(
            project = "CMIP6",
            experiment = "ssp585",
            variables = "tas",
            frequency = "day"
        ),
        site = list(
            id = "SIN",
            lon = 103.98,
            lat = 1.37,
            label = "singapore",
            epw = epw
        ),
        collect = Filter(Negate(is.null), list(store = store, label = "cli-shift")),
        extract = list(
            periods = list(`2060s` = 2060L),
            time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
            fallback = "auto"
        ),
        morph = list(
            recipe = "belcher_absolute",
            strict = FALSE,
            methods = list(tdb = "shift")
        ),
        epw = list(
            dir = "cli-shift-epw",
            separate = FALSE
        )
    )
    jsonlite::write_json(config, path, auto_unbox = TRUE, pretty = TRUE)
    invisible(path)
}

cli_shift_test_store_with_query <- function(nc) {
    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    docs <- cli_shift_test_file_docs(basename(nc), opendap_url = nc, download_url = nc)
    query_id <- store$add_files(cli_shift_test_file_result(docs))
    store$close()
    list(dir = dir, query_id = query_id)
}

cli_shift_test_store_with_extract <- function(nc) {
    setup <- cli_shift_test_store_with_query(nc)
    plan <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "extract", "plan",
        "--query", setup$query_id,
        "--site-id", "SIN",
        "--lon", "103.98",
        "--lat", "1.37",
        "--time", "2060-01-02T00:00:00Z,2060-01-03T23:59:59Z",
        "--variable", "tas"
    ))
    run <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "extract", "run",
        "--plan", paste(plan$result$plan_id, collapse = ",")
    ))
    testthat::expect_equal(plan$status, 0L)
    testthat::expect_equal(run$status, 0L)
    testthat::expect_true(all(run$result$status == "done"))
    c(setup, list(plan_id = plan$result$plan_id))
}
