cli_test_response <- function(docs) {
    list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = stats::setNames(list(), character())
        ),
        response = list(
            numFound = nrow(docs),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
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

cli_test_file_docs <- function(path = "cli-file.nc", download_url = NULL) {
    if (is.null(download_url)) {
        download_url <- sprintf("https://example.org/fileServer/%s", path)
    }
    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", path),
        master_id = sprintf("%s.master", path),
        replica = FALSE,
        tracking_id = "hdl:21.14100/cli-test",
        title = path,
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("https://example.org/dods/%s.html|application/netcdf|OPENDAP", path),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

cli_test_store <- function(label = "cli query", track = TRUE) {
    skip_if_not_installed("duckdb")
    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    query_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("ssp585")$
            variable_id("tas")$
            limit(1L),
        label = label,
        track = track
    )
    list(dir = dir, store = store, query_id = query_id)
}
