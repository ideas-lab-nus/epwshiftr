test_that("schema constants are standalone SchemaDoc objects", {
    expect_true(S7::S7_inherits(SCHEMA_QUERY, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESPONSE, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESULT_DATASET, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESULT_FILE, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESULT_AGGREGATION, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_ESG_DICT, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_DOWNLOADER_CONFIG, SchemaDoc))
})

test_that("schema constants expose expected logical paths", {
    expect_true("$parameter" %in% schema_paths(SCHEMA_QUERY))

    response_paths <- schema_paths(SCHEMA_RESPONSE)
    expect_true("$responseHeader$params" %in% response_paths)
    expect_true("$response$docs" %in% response_paths)
    expect_true("$facet_counts$facet_fields" %in% response_paths)

    result_paths <- schema_paths(SCHEMA_RESULT_DATASET)
    expect_true("$parameter" %in% result_paths)
    expect_true("$response" %in% result_paths)
    expect_true("$defs$response_header$params" %in% result_paths)
    expect_true("$defs$response_body$docs" %in% result_paths)
    expect_true("$defs$facet_counts$facet_fields" %in% result_paths)

    expect_true("$parameter" %in% schema_paths(SCHEMA_RESULT_FILE))
    expect_true("$parameter" %in% schema_paths(SCHEMA_RESULT_AGGREGATION))

    dict_paths <- schema_paths(SCHEMA_ESG_DICT)
    expect_true("$project" %in% dict_paths)
    expect_true("$profile" %in% dict_paths)
    expect_true("$payload$any[2]$vocab" %in% dict_paths)
    expect_true("$payload$any[2]$request" %in% dict_paths)

    downloader_paths <- schema_paths(SCHEMA_DOWNLOADER_CONFIG)
    expect_true("$schema_version" %in% downloader_paths)
    expect_true("$manifest" %in% downloader_paths)
    expect_true("$n_workers" %in% downloader_paths)
})

test_that("downloader config schema validates persistent downloader config", {
    config <- list(
        schema_version = "1.0.0",
        dest = tempdir(),
        temp = file.path(tempdir(), "downloads"),
        manifest = file.path(tempdir(), "manifest.duckdb"),
        retries = 3L,
        timeout = 3600L,
        cleanup = TRUE,
        n_workers = 0L
    )

    expect_true(schema_validate(SCHEMA_DOWNLOADER_CONFIG, config, mode = "test", name = "downloader-config"))

    bad <- config
    bad$retries <- -1L
    expect_false(schema_validate(SCHEMA_DOWNLOADER_CONFIG, bad, mode = "test", name = "bad-downloader-config"))
})

test_that("result schema JSON files use local reusable definitions", {
    required_defs <- c(
        "index_node",
        "parameter",
        "parameter_facet",
        "parameter_query",
        "parameter_control",
        "parameter_others",
        "response",
        "response_header",
        "response_body",
        "response_docs",
        "facet_counts",
        "timestamp",
        "context",
        "context_time_filter"
    )

    for (filename in c("result-dataset.json", "result-file.json", "result-aggregation.json")) {
        schema_file <- test_path("..", "..", "inst", "extdata", "schema", filename)
        json <- jsonlite::fromJSON(schema_file, simplifyVector = TRUE, simplifyMatrix = FALSE)

        expect_named(json$fields, c("index_node", "parameter", "response", "context"))
        expect_identical(json$fields$index_node$`$ref`, "#/$defs/index_node")
        expect_identical(json$fields$parameter$`$ref`, "#/$defs/parameter")
        expect_identical(json$fields$response$`$ref`, "#/$defs/response")
        expect_identical(json$fields$context$`$ref`, "#/$defs/context")
        expect_true(all(required_defs %in% names(json[["$defs"]])))
    }
})

test_that("schema validates saved query JSON fixtures", {
    query_file <- test_path("_snaps", "query", "query_empty.json")
    query_json <- jsonlite::fromJSON(query_file, simplifyVector = TRUE, simplifyMatrix = FALSE)

    expect_true(schema_validate(SCHEMA_QUERY, query_json, mode = "test", name = query_file))

    query_json$parameter <- "not a parameter list"
    expect_false(schema_validate(SCHEMA_QUERY, query_json, mode = "test", name = "bad-query"))

    bad_file <- tempfile(fileext = ".json")
    jsonlite::write_json(query_json, bad_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(bad_file))
})

schema_test_response <- function(docs) {
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
        timestamp = format.POSIXct(
            Sys.time(),
            digits = 6,
            tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OS6Z"
        )
    )
}

schema_test_result_json <- function(type, docs, context = NULL) {
    result <- list(
        index_node = "https://example.org",
        parameter = query_param_as_store(list(type = type, format = FORMAT_JSON))$serialize(null = TRUE),
        response = schema_test_response(docs)
    )
    if (!is.null(context)) {
        result$context <- context
    }

    result
}

schema_test_dataset_docs <- function() {
    data.frame(
        id = "dataset-1",
        size = 1,
        source_id = "source-a",
        check.names = FALSE
    )
}

schema_test_file_docs <- function() {
    docs <- data.frame(
        id = "file-1",
        dataset_id = "dataset-1",
        size = 1,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = "file-instance-1",
        master_id = "master-file-1",
        replica = FALSE,
        tracking_id = "hdl:21.14100/mock-file",
        title = "file.nc",
        version = 20260101L,
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    )))
    docs
}

test_that("schema validates saved query result JSON fixtures", {
    result_file <- test_path("_snaps", "query-result", "dataset.json")
    dataset_json <- jsonlite::fromJSON(result_file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    file_json <- schema_test_result_json("File", schema_test_file_docs())
    aggregation_json <- schema_test_result_json("Aggregation", schema_test_file_docs())

    expect_true(schema_validate(SCHEMA_RESULT_DATASET, dataset_json, mode = "test", name = result_file))
    expect_true(schema_validate(SCHEMA_RESULT_FILE, file_json, mode = "test", name = "file-result"))
    expect_true(schema_validate(SCHEMA_RESULT_AGGREGATION, aggregation_json, mode = "test", name = "aggregation-result"))

    expect_false(schema_validate(SCHEMA_RESULT_DATASET, file_json, mode = "test", name = "file-as-dataset"))
    expect_false(schema_validate(SCHEMA_RESULT_FILE, aggregation_json, mode = "test", name = "aggregation-as-file"))

    dataset_json$response$response$docs$not_a_solr_field <- seq_len(nrow(dataset_json$response$response$docs))
    expect_false(schema_validate(SCHEMA_RESULT_DATASET, dataset_json, mode = "test", name = "bad-result"))

    file_missing_required <- file_json
    file_missing_required$response$response$docs$dataset_id <- NULL
    expect_false(schema_validate(SCHEMA_RESULT_FILE, file_missing_required, mode = "test", name = "bad-file-result"))

    aggregation_missing_required <- aggregation_json
    aggregation_missing_required$response$response$docs$title <- NULL
    expect_false(schema_validate(
        SCHEMA_RESULT_AGGREGATION,
        aggregation_missing_required,
        mode = "test",
        name = "bad-aggregation-result"
    ))

    empty_file_json <- schema_test_result_json("File", schema_test_file_docs()[0L, ])
    empty_file_json$response$response$docs <- data.frame()
    expect_true(schema_validate(SCHEMA_RESULT_FILE, empty_file_json, mode = "test", name = "empty-file-result"))

    time_context <- list(time_filter = list(
        start = "2050-01-01T00:00:00Z",
        stop = "2050-12-31T23:59:59Z",
        method = "drs",
        unknown = "kept",
        total = 2L,
        selected = 1L,
        unknown_count = 0L
    ))
    file_with_context <- schema_test_result_json("File", schema_test_file_docs(), context = time_context)
    expect_true(schema_validate(SCHEMA_RESULT_FILE, file_with_context, mode = "test", name = "file-result-context"))
    file_with_context$context$time_filter$method <- "metadata"
    expect_false(schema_validate(SCHEMA_RESULT_FILE, file_with_context, mode = "test", name = "bad-result-context"))

    bad_file <- tempfile(fileext = ".json")
    jsonlite::write_json(file_missing_required, bad_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_result("file")$load(bad_file))

    bad_file <- tempfile(fileext = ".json")
    jsonlite::write_json(dataset_json, bad_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_result()$load(bad_file))
})

test_that("dataset result schema validates local minimal results", {
    result_json <- schema_test_result_json("Dataset", schema_test_dataset_docs())

    expect_true(schema_validate(SCHEMA_RESULT_DATASET, result_json, mode = "test", name = "local-dataset-result"))

    bad_file <- tempfile(fileext = ".json")
    result_json$response$response$docs$not_a_solr_field <- seq_len(nrow(result_json$response$response$docs))
    jsonlite::write_json(result_json, bad_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_result()$load(bad_file))
})
