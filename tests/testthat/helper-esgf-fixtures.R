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
