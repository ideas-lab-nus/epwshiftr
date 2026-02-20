# EsgResultDataset {{{
local_test_cache()
test_that("ESGF Query Result Dataset works", {
    skip_on_cran()

    index_node <- INDEX_NODES[["DKRZ"]]
    q <- esg_query(index_node)$activity_id("ScenarioMIP")$source_id("AWI-CM-1-1-MR")$frequency("day")$variable_id(
        "tas"
    )$variant_label("r1i1p1f1")$fields(c("source_id", "experiment_id", "frequency"))$limit(2)

    # can create a new result dataset from Esg$collect
    datasets <- expect_s3_class(q$collect(), "EsgResultDataset")

    # $to_data_table(): can extract the data into a data.table
    expect_s3_class(datasets$to_data_table(), "data.table")
    # $to_data_table(): can only keep selected fields
    expect_s3_class(datasets$to_data_table(c("source_id", "frequency")), "data.table")
    expect_equal(names(datasets$to_data_table(c("source_id", "frequency"))), c("source_id", "frequency"))
    # $to_data_table(): can keep the special format of certain fields
    expect_s3_class(datasets$to_data_table(formatted = TRUE)$size, "units")
    expect_true(any(vapply(
        datasets$to_data_table(formatted = TRUE)$url,
        data.table::is.data.table,
        logical(1L)
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
            "access",
            "activity_id",
            "experiment_id",
            "frequency",
            "id",
            "index_node",
            "number_of_aggregations",
            "number_of_files",
            "project",
            "size",
            "source_id",
            "url",
            "variable_id",
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
        datasets$url,
        data.table::is.data.table,
        logical(1L)
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
    file <- tempfile(fileext = ".json")
    expect_type(datasets$save(file), "character")
    expect_true(file.exists(file))
    file_copied <- tempfile(fileext = ".json")
    expect_true(file.copy(file, file_copied))
    expect_snapshot_file(file_copied, "dataset.json", transform = transform_json)

    # $load() empty datasets
    expect_s3_class(de <- new_query_result(EsgResultDataset)$load(file), "EsgResultDataset")
    expect_equal(priv(de)$index_node, priv(datasets)$index_node)
    expect_equal(priv(de)$parameter, priv(datasets)$parameter)
    # manually add the cache key since '$save()' will exclude it
    priv(de)$response$cache <- priv(datasets)$response$cache
    # keep the column order the same since '$save()' then '$load()' may change
    # the order
    priv(de)$response$response$docs <- priv(de)$response$response$docs[,
        names(priv(datasets)$response$response$docs)
    ]
    expect_equal(priv(de)$response, priv(datasets)$response)

    # $collect():
    ## $collect(): can specify dataset index
    expect_s3_class(datasets$collect(1, limit = 2, fields = "id"), "EsgResultFile")

    ## $collect(): can specify dataset id
    expect_s3_class(datasets$collect(datasets$id[1], fields = "id"), "EsgResultFile")

    ## $collect(): can limit fields and record number
    expect_s3_class(files <- datasets$collect(fields = "id", limit = 1), "EsgResultFile")
    expect_length(files$fields, 12L)

    ## $collect(): can collect all fields
    expect_s3_class(files <- datasets$collect(fields = "id", all = TRUE), "EsgResultFile")
    expect_equal(files$count(), sum(datasets$number_of_files))

    ## $collect(): can specify specific parameters
    expect_s3_class(datasets$collect(fields = "id", limit = 1, replica = FALSE), "EsgResultFile")

    ## $collect(): can collect all possible fields
    expect_s3_class(files <- datasets$collect(limit = 1), "EsgResultFile")
    expect_length(files$fields, 56L)

    ## $collect(): can stop if unsupported parameters found
    expect_error(datasets$collect(experiment_id = "ssp585"), "Unsupported")

    ## $collect(): can collect aggregation
    expect_s3_class(datasets$collect(fields = "id", limit = 2, type = "Aggregation"), "EsgResultAggregation")

    # $print()
    expect_snapshot(datasets$print(), transform = transform_print)
})
# }}}

# EsgResultFile {{{
test_that("ESGF Query Result File works", {
    skip_on_cran()
    index_node <- INDEX_NODES[["DKRZ"]]

    files <- esg_query(index_node)$activity_id("ScenarioMIP")$source_id("AWI-CM-1-1-MR")$frequency("day")$variable_id(
        "tas"
    )$experiment_id("ssp585")$variant_label("r1i1p1f1")$fields(c("source_id", "experiment_id", "frequency"))$limit(
        2
    )$collect()$collect(limit = 1)

    # $to_data_table(): can extract the data into a data.table
    expect_s3_class(files$to_data_table(), "data.table")
    # $to_data_table(): can only keep selected fields
    expect_s3_class(files$to_data_table(c("checksum", "checksum_type")), "data.table")
    expect_equal(names(files$to_data_table(c("checksum", "checksum_type"))), c("checksum", "checksum_type"))
    # $to_data_table(): can keep the special format of certain fields
    expect_s3_class(files$to_data_table(formatted = TRUE)$size, "units")
    expect_s3_class(files$to_data_table(formatted = TRUE)$url[[1L]], "data.table")

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
        all(EsgResultFile$private_fields$required_fields %in% files$fields)
    )

    # $print()
    expect_snapshot(files$print(), transform = transform_print)
})
# }}}

# EsgResultAggregation {{{
test_that("ESGF Query Result Aggregation works", {
    skip_on_cran()
    index_node <- INDEX_NODES[["DKRZ"]]

    aggs <- esg_query(index_node)$activity_id("ScenarioMIP")$source_id("AWI-CM-1-1-MR")$frequency("day")$variable_id(
        "tas"
    )$experiment_id("ssp585")$variant_label("r1i1p1f1")$fields(c("source_id", "experiment_id", "frequency"))$limit(
        2
    )$collect()$collect(fields = "id", limit = 2, type = "Aggregation")

    # $to_data_table(): can extract the data into a data.table
    expect_s3_class(aggs$to_data_table(), "data.table")
    # $to_data_table(): can only keep selected fields
    expect_s3_class(aggs$to_data_table(c("url", "size")), "data.table")
    expect_equal(names(aggs$to_data_table(c("url", "size"))), c("url", "size"))
    # $to_data_table(): can keep the special format of certain fields
    expect_s3_class(aggs$to_data_table(formatted = TRUE)$size, "units")
    expect_s3_class(aggs$to_data_table(formatted = TRUE)$url[[1L]], "data.table")

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
        all(EsgResultAggregation$private_fields$required_fields %in% aggs$fields)
    )

    # $print()
    expect_snapshot(aggs$print(), transform = transform_print)
})
# }}}

# result_esgf() {{{
test_that("result_esgf() works", {
    expect_s3_class(esg_result(), "EsgResultDataset")
    expect_s3_class(esg_result("file"), "EsgResultFile")
    expect_s3_class(esg_result(), "EsgResultDataset")

    expect_snapshot(esg_result("file")$print(), transform = transform_print)
    expect_snapshot(esg_result("aggregation")$print(), transform = transform_print)
    expect_snapshot(result_esgf("aggregation")$print(), transform = transform_print)
})
# }}}

# vim: fdm=marker :
