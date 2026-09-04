fixture_path <- function(...) {
    testthat::test_path("fixtures", ...)
}

fixture_json <- function(...) {
    path <- fixture_path(...)
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyMatrix = FALSE)
}

read_fixture_json <- function(...) {
    readLines(fixture_path(...), warn = FALSE)
}

# Build the canonical Solr response envelope shared by query, Store, CLI, and
# staged-workflow tests while allowing time-sensitive tests to supply a timestamp.
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

esgf_fixture_response <- function(name) {
    response <- fixture_json("esgf", name)
    response$timestamp <- as.POSIXct("2020-02-02 22:22:22", tz = "UTC")
    response
}

esgf_fixture_collect <- function(params,
                                 dataset = "dataset-success.json",
                                 file = "file-success.json",
                                 response = NULL) {
    if (is.null(response)) {
        type <- query_param__value(query_param__as_store(params)$type())
        response <- esgf_fixture_response(if (identical(type, "File")) file else dataset)
    }
    list(response = response, docs = response$response$docs, parameter = params)
}
