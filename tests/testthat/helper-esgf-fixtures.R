# fixture_path {{{
fixture_path <- function(...) {
    testthat::test_path("fixtures", ...)
}
# }}}

# fixture_json {{{
fixture_json <- function(...) {
    path <- fixture_path(...)
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyMatrix = FALSE)
}
# }}}

# read_fixture_json {{{
read_fixture_json <- function(...) {
    readLines(fixture_path(...), warn = FALSE)
}
# }}}

# Build the canonical Solr response envelope shared by query, Store, CLI, and
# staged-workflow tests while allowing time-sensitive tests to supply a timestamp.
# esgf_test__response {{{
esgf_test__response <- function(
    docs,
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    params = stats::setNames(list(), character()),
    facet_fields = stats::setNames(list(), character()),
    num_found = if (is.data.frame(docs)) nrow(docs) else length(docs)
) {
    list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = params
        ),
        response = list(
            numFound = as.integer(num_found),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
        facet_counts = list(
            facet_queries = stats::setNames(list(), character()),
            facet_fields = facet_fields,
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = timestamp
    )
}
# }}}

# esgf_fixture_response {{{
esgf_fixture_response <- function(name) {
    response <- fixture_json("esgf", name)
    response$timestamp <- as.POSIXct("2020-02-02 22:22:22", tz = "UTC")
    response
}
# }}}

# esgf_fixture_collect {{{
esgf_fixture_collect <- function(
    params,
    dataset = "dataset-success.json",
    file = "file-success.json",
    response = NULL
) {
    if (is.null(response)) {
        type <- query_param__value(query_param__as_store(params)$type())
        response <- esgf_fixture_response(
            if (identical(type, "File")) file else dataset
        )
    }
    list(response = response, docs = response$response$docs, parameter = params)
}
# }}}

# Build a deterministic Dataset document for offline workflow discovery.
# esgf_test__dataset_docs {{{
esgf_test__dataset_docs <- function(variable_id = "tas", frequency = "day") {
    data.table::data.table(
        id = "dataset-1",
        instance_id = "dataset-1.v20260101",
        master_id = "dataset-1",
        size = 123,
        access = I(list(c("OPENDAP", "HTTPServer"))),
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variable_id = variable_id[[1L]],
        frequency = frequency[[1L]],
        variant_label = "r1i1p1f1",
        data_node = "example.org"
    )
}
# }}}

# Build File identity, coverage and endpoint fields shared by CLI and R tests.
# esgf_test__file_docs {{{
esgf_test__file_docs <- function(
    path,
    opendap_url = path,
    download_url = path,
    variable_id = "tas",
    include_opendap = TRUE,
    include_download = TRUE,
    datetime_start = "2060-01-01T00:00:00Z",
    datetime_end = "2060-12-31T23:59:59Z",
    frequency = "day",
    table_id = "day"
) {
    docs <- data.table::data.table(
        id = sprintf("%s|dataset-1", basename(path)),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", basename(path)),
        master_id = sprintf("%s.master", basename(path)),
        replica = FALSE,
        tracking_id = sprintf("hdl:21.14100/shift-test-%s", variable_id),
        title = basename(path),
        version = 20260101L,
        latest = TRUE,
        retracted = FALSE,
        deprecated = FALSE,
        datetime_start = datetime_start,
        datetime_end = datetime_end,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = frequency,
        table_id = table_id,
        variable_id = variable_id,
        grid_label = "gr"
    )
    urls <- character()
    if (isTRUE(include_opendap)) {
        urls <- c(urls, sprintf("%s|application/netcdf|OPENDAP", opendap_url))
    }
    if (isTRUE(include_download)) {
        urls <- c(
            urls,
            sprintf("%s|application/netcdf|HTTPServer", download_url)
        )
    }
    data.table::set(docs, j = "url", value = list(list(urls)))
    docs
}
# }}}

# Wrap shared File documents in the same result class consumed by the store.
# esgf_test__file_result {{{
esgf_test__file_result <- function(docs) {
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
        result = esgf_test__response(docs)
    )
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
