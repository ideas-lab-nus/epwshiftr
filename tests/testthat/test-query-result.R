host <- get_fast_host()

# EsgfQueryResultDataset {{{
test_that("ESGF Query Result Dataset works", {
    skip_on_cran()

    q <- query_esgf(host, FALSE)$
        activity_id("ScenarioMIP")$
        source_id("AWI-CM-1-1-MR")$
        frequency("day")$
        variable_id("tas")$
        experiment_id("ssp585")$
        variant_label("r1i1p1f1")$
        fields(c("source_id", "experiment_id", "frequency"))$
        limit(2)

    # can create a new result dataset from EsgfQuery$collect
    expect_s3_class(datasets <- q$collect(), "EsgfQueryResultDataset")

    # $to_dt(): can extract the data into a data.table
    expect_s3_class(datasets$to_dt(), "data.table")
    # $to_dt(): can only keep selected fields
    expect_s3_class(datasets$to_dt(c("source_id", "frequency")), "data.table")
    expect_equal(names(datasets$to_dt(c("source_id", "frequency"))), c("source_id", "frequency"))
    # $to_dt(): can keep the special format of certain fields
    expect_s3_class(datasets$to_dt(formatted = TRUE)$size, "units")
    expect_true(any(vapply(
        datasets$to_dt(formatted = TRUE)$url, data.table::is.data.table, logical(1L)
    )))

    # $has_opendap(): can test accessibility
    expect_type(datasets$has_opendap(), "logical")
    expect_length(datasets$has_opendap(), 2L)

    # $has_download(): can test accessibility
    expect_type(datasets$has_download(), "logical")
    expect_length(datasets$has_download(), 2L)

    # $count(): can get length
    expect_equal(datasets$count(), 2L)

    # $field: can add active bindings for all fields
    expect_equal(
        sort(datasets$fields),
        c(
            "access", "activity_id", "experiment_id", "frequency", "id",
            "index_node", "number_of_aggregations", "number_of_files",
            "project", "size", "source_id", "url", "variable_id",
            "variant_label"
        )
    )

    # $id
    expect_type(datasets$id, "character")
    expect_length(datasets$id, 2L)

    # $url
    expect_type(datasets$url, "list")
    expect_length(datasets$url, 2L)
    expect_true(any(vapply(
        datasets$url, data.table::is.data.table, logical(1L)
    )))

    # $size
    expect_s3_class(datasets$size, "units")
    expect_length(datasets$size, 2L)

    # $index_node
    expect_type(datasets$index_node, "character")
    expect_length(datasets$index_node, 2L)

    # $access
    expect_type(datasets$access, "list")
    expect_length(datasets$access, 2L)
    expect_type(datasets$access[[1]], "character")

    # $number_of_aggregations
    expect_type(datasets$number_of_aggregations, "integer")
    expect_length(datasets$number_of_aggregations, 2L)

    # $number_of_files
    expect_type(datasets$number_of_files, "integer")
    expect_length(datasets$number_of_files, 2L)

    # $save() empty datasets
    fe <- tempfile(fileext = ".json")
    datasets$.__enclos_env__$private$response$responseHeader$QTime <- 10L
    datasets$.__enclos_env__$private$response$timestamp <- as.POSIXct(
        "2020-02-02 22:22:22.123456", "UTC"
    )
    expect_snapshot_file(datasets$save(fe), "dataset_empty.json")

    # $load() empty datasets
    expect_s3_class(de <- new_query_result(EsgfQueryResultDataset)$load(fe), "EsgfQueryResultDataset")
    expect_equal(priv(de)$url_host,    priv(datasets)$url_host)
    expect_equal(priv(de)$parameter,   priv(datasets)$parameter)
    expect_equal(priv(de)$response,    priv(datasets)$response)
    expect_equal(priv(de)$last_result, priv(datasets)$last_result)

    # $collect():
    ## $collect(): can specify dataset index
    expect_s3_class(datasets$collect(1, limit = 2, fields = "id"), "EsgfQueryResultFile")

    # $save() collected datasets
    fc <- tempfile(fileext = ".json")
    datasets$.__enclos_env__$private$last_result$response$timestamp <- as.POSIXct(
        "2020-02-02 22:22:22.123456", "UTC"
    )
    datasets$.__enclos_env__$private$last_result$response$responseHeader$QTime <- 10L
    expect_snapshot_file(datasets$save(fc), "dataset_collected.json")

    # $load() collected datasets
    expect_s3_class(dc <- new_query_result(EsgfQueryResultDataset)$load(fc), "EsgfQueryResultDataset")
    expect_equal(priv(dc)$url_host,    priv(datasets)$url_host)
    expect_equal(priv(dc)$parameter,   priv(datasets)$parameter)
    expect_equal(priv(dc)$response,    priv(datasets)$response)
    expect_equal(priv(dc)$last_result, priv(datasets)$last_result)

    ## $collect(): can specify dataset id
    expect_s3_class(datasets$collect(datasets$id[1], fields = "id"), "EsgfQueryResultFile")

    ## $collect(): can limit fields and record number
    expect_s3_class(files <- datasets$collect(fields = "id", limit = 1), "EsgfQueryResultFile")
    expect_length(files$fields, 12L)

    ## $collect(): can collect all fields
    expect_s3_class(files <- datasets$collect(fields = "id", all = TRUE), "EsgfQueryResultFile")
    expect_equal(files$count(), sum(datasets$number_of_files))

    ## $collect(): can specify specific parameters
    expect_s3_class(datasets$collect(fields = "id", limit = 1, replica = FALSE), "EsgfQueryResultFile")

    ## $collect(): can collect all possible fields
    expect_s3_class(files <- datasets$collect(limit = 1), "EsgfQueryResultFile")
    expect_length(files$fields, 55L)

    ## $collect(): can stop if unsupported parameters found
    expect_error(datasets$collect(experiment_id = "ssp585"), "Unsupported")

    ## $collect(): can collect aggregation
    expect_s3_class(datasets$collect(fields = "id", limit = 2, type = "Aggregation"), "EsgfQueryResultAggregation")

    # $print()
    expect_snapshot(datasets$print(), transform = function(out) {
        out[grepl("^\\* Collected at: ", out)] <- "* Collected at: yyyy-mm-dd HH:MM:SS"
        out[grepl("^\\* Total size: ", out)]   <- "* Total size: XX [GiB]"
        gsub("\\d+ Files, \\d+\\.\\d+ GiB \\| \\d+ Aggregation[s]?", "XX Files, XX GiB | X Aggregations", out)
    })
})
# }}}

# EsgfQueryResultFile {{{
test_that("ESGF Query Result File works", {
    skip_on_cran()

    files <- query_esgf(host, FALSE)$
        activity_id("ScenarioMIP")$
        source_id("AWI-CM-1-1-MR")$
        frequency("day")$
        variable_id("tas")$
        experiment_id("ssp585")$
        variant_label("r1i1p1f1")$
        fields(c("source_id", "experiment_id", "frequency"))$
        limit(2)$
        collect()$
        collect(limit = 1)

    # $to_dt(): can extract the data into a data.table
    expect_s3_class(files$to_dt(), "data.table")
    # $to_dt(): can only keep selected fields
    expect_s3_class(files$to_dt(c("checksum", "checksum_type")), "data.table")
    expect_equal(names(files$to_dt(c("checksum", "checksum_type"))), c("checksum", "checksum_type"))
    # $to_dt(): can keep the special format of certain fields
    expect_s3_class(files$to_dt(formatted = TRUE)$size, "units")
    expect_s3_class(files$to_dt(formatted = TRUE)$url[[1L]], "data.table")

    # $id
    expect_type(files$id, "character")
    expect_length(files$id, 1L)

    # $url
    expect_type(files$url, "list")
    expect_length(files$url, 1L)
    expect_s3_class(files$url[[1L]], "data.table")

    # $size
    expect_s3_class(files$size, "units")
    expect_length(files$size, 1L)

    # $dataset_id
    expect_type(files$dataset_id, "character")
    expect_length(files$dataset_id, 1L)

    # $checksum
    expect_type(files$checksum, "character")
    expect_length(files$checksum, 1L)

    # $checksum_type
    expect_type(files$checksum_type, "character")
    expect_length(files$checksum_type, 1L)

    # $data_node
    expect_type(files$data_node, "character")
    expect_length(files$data_node, 1L)

    # $filename
    expect_type(files$filename, "character")
    expect_length(files$filename, 1L)

    # $tracking_id
    expect_type(files$tracking_id, "character")
    expect_length(files$tracking_id, 1L)

    # $url_opendap: can extract opendap url
    expect_type(files$url_opendap, "character")
    expect_length(files$url_opendap, 1L)

    # $url_download: can extract download url
    expect_type(files$url_download, "character")
    expect_length(files$url_download, 1L)

    expect_true(
        all(EsgfQueryResultFile$private_fields$required_fields %in% files$fields)
    )

    # $print()
    expect_snapshot(files$print(), transform = function(out) {
        out[grepl("^\\* Collected at: ", out)] <- "* Collected at: yyyy-mm-dd HH:MM:SS"
        out[grepl("^\\* Total size: ", out)]   <- "* Total size: XX [MiB]"
        gsub("\\d+\\.\\d+ MiB \\| Access", "XX MiB | Access", out)
    })
})
# }}}

# EsgfQueryResultAggregation {{{
test_that("ESGF Query Result Aggregation works", {
    skip_on_cran()

    aggs <- query_esgf(host, FALSE)$
        activity_id("ScenarioMIP")$
        source_id("AWI-CM-1-1-MR")$
        frequency("day")$
        variable_id("tas")$
        experiment_id("ssp585")$
        variant_label("r1i1p1f1")$
        fields(c("source_id", "experiment_id", "frequency"))$
        limit(2)$
        collect()$
        collect(fields = "id", limit = 2, type = "Aggregation")

    # $to_dt(): can extract the data into a data.table
    expect_s3_class(aggs$to_dt(), "data.table")
    # $to_dt(): can only keep selected fields
    expect_s3_class(aggs$to_dt(c("url", "size")), "data.table")
    expect_equal(names(aggs$to_dt(c("url", "size"))), c("url", "size"))
    # $to_dt(): can keep the special format of certain fields
    expect_s3_class(aggs$to_dt(formatted = TRUE)$size, "units")
    expect_s3_class(aggs$to_dt(formatted = TRUE)$url[[1L]], "data.table")

    # $id
    expect_type(aggs$id, "character")
    expect_length(aggs$id, 2L)

    # $url
    expect_type(aggs$url, "list")
    expect_length(aggs$url, 2L)
    expect_s3_class(aggs$url[[1L]], "data.table")

    # $size
    expect_s3_class(aggs$size, "units")
    expect_length(aggs$size, 2L)

    # $dataset_id
    expect_type(aggs$dataset_id, "character")
    expect_length(aggs$dataset_id, 2L)

    # $data_node
    expect_type(aggs$data_node, "character")
    expect_length(aggs$data_node, 2L)

    # $url_opendap: can extract opendap url
    expect_type(aggs$url_opendap, "character")
    expect_length(aggs$url_opendap, 2L)

    # $url_download: can extract download url
    expect_type(aggs$url_download, "character")
    expect_length(aggs$url_download, 2L)

    expect_true(
        all(EsgfQueryResultAggregation$private_fields$required_fields %in% aggs$fields)
    )

    # $print()
    expect_snapshot(aggs$print(), transform = function(out) {
        out[grepl("^\\* Collected at: ", out)] <- "* Collected at: yyyy-mm-dd HH:MM:SS"
        out
    })
})
# }}}

# result_esgf() {{{
test_that("result_esgf() works", {
    expect_s3_class(result_esgf(), "EsgfQueryResultDataset")
    expect_s3_class(result_esgf("file"), "EsgfQueryResultFile")
    expect_s3_class(result_esgf("aggregation"), "EsgfQueryResultAggregation")
})
# }}}

# vim: fdm=marker :
