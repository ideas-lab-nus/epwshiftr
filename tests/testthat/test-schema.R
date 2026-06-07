test_that("schema constants are standalone SchemaDoc objects", {
    expect_true(S7::S7_inherits(SCHEMA_QUERY, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESPONSE, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_RESULT_DATASET, SchemaDoc))
    expect_true(S7::S7_inherits(SCHEMA_ESG_DICT, SchemaDoc))
})

test_that("schema constants expose expected logical paths", {
    expect_true("$parameter" %in% schema_paths(SCHEMA_QUERY))

    response_paths <- schema_paths(SCHEMA_RESPONSE)
    expect_true("$responseHeader$params" %in% response_paths)
    expect_true("$response$docs" %in% response_paths)
    expect_true("$facet_counts$facet_fields" %in% response_paths)

    result_paths <- schema_paths(SCHEMA_RESULT_DATASET)
    expect_true("$parameter" %in% result_paths)
    expect_true("$response$responseHeader$params" %in% result_paths)
    expect_true("$response$response$docs" %in% result_paths)
    expect_true("$response$facet_counts$facet_fields" %in% result_paths)

    dict_paths <- schema_paths(SCHEMA_ESG_DICT)
    expect_true("$project" %in% dict_paths)
    expect_true("$profile" %in% dict_paths)
    expect_true("$payload$any[2]$vocab" %in% dict_paths)
    expect_true("$payload$any[2]$request" %in% dict_paths)
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

test_that("schema validates saved dataset result JSON fixtures", {
    result_file <- test_path("_snaps", "query-result", "dataset.json")
    result_json <- jsonlite::fromJSON(result_file, simplifyVector = TRUE, simplifyMatrix = FALSE)

    expect_true(schema_validate(SCHEMA_RESULT_DATASET, result_json, mode = "test", name = result_file))

    result_json$response$response$docs$not_a_solr_field <- seq_len(nrow(result_json$response$response$docs))
    expect_false(schema_validate(SCHEMA_RESULT_DATASET, result_json, mode = "test", name = "bad-result"))

    bad_file <- tempfile(fileext = ".json")
    jsonlite::write_json(result_json, bad_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_result()$load(bad_file))
})
