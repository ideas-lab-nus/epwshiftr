# EsgResultDataset {{{
local_test_cache(scope = "persist")

query_result_test_response <- function(docs) {
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
        timestamp = Sys.time()
    )
}

query_result_test_params <- function(type = "Dataset", ...) {
    values <- list(...)
    params <- list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 1L,
        type = type,
        format = FORMAT_JSON
    )
    params[names(values)] <- values

    query_param_as_store(params)
}

query_result_test_object <- function(type = "Dataset", docs, params = query_result_test_params(type)) {
    generator <- switch(
        type,
        Dataset = EsgResultDataset,
        File = EsgResultFile,
        Aggregation = EsgResultAggregation
    )
    new_query_result(
        generator,
        index_node = "https://example.org",
        params = params,
        result = query_result_test_response(docs)
    )
}

query_result_test_state <- function(type = "Dataset", docs, params = query_result_test_params(type)) {
    list(
        index_node = "https://example.org",
        parameter = params,
        response = query_result_test_response(docs)
    )
}

query_result_test_dataset_docs <- function(access = TRUE) {
    docs <- data.frame(
        id = c("dataset-1", "dataset-2"),
        source_id = c("source-a", "source-b"),
        experiment_id = c("ssp126", "ssp585"),
        size = c(1, 2),
        check.names = FALSE
    )
    if (access) {
        docs$access <- I(list(c("OPENDAP", "HTTPServer"), "HTTPServer"))
    }
    docs
}

query_result_test_file_docs <- function(url = "https://example.org/file.nc|application/netcdf|HTTPServer") {
    docs <- data.frame(
        id = "file-1",
        dataset_id = "dataset-1",
        size = 1,
        checksum = "abc",
        checksum_type = "SHA256",
        tracking_id = "hdl:21.14100/mock-file",
        title = "file.nc",
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(list(url))
    docs
}

test_that("loaded ESGF query results restore dynamic fields", {
    state <- query_result_test_state("Dataset", query_result_test_dataset_docs())
    testthat::local_mocked_bindings(
        query_load = function(file, schema = NULL) state,
        .package = "epwshiftr"
    )

    loaded <- expect_s3_class(esg_result("dataset")$load("dataset.json"), "EsgResultDataset")
    expect_identical(loaded$source_id, c("source-a", "source-b"))
    expect_identical(loaded$experiment_id, c("ssp126", "ssp585"))
    expect_true(all(c("source_id", "experiment_id") %in% loaded$fields))
    expect_s3_class(loaded$to_data_table(), "data.table")
    expect_s3_class(loaded$to_dt(), "data.table")

    loaded_in_place <- esg_result("dataset")
    expect_s3_class(loaded_in_place$load("dataset.json"), "EsgResultDataset")
    expect_identical(loaded_in_place$source_id, c("source-a", "source-b"))
})

test_that("loaded ESGF query results validate result type", {
    state <- query_result_test_state("File", query_result_test_file_docs(), query_result_test_params("File"))
    testthat::local_mocked_bindings(
        query_load = function(file, schema = NULL) state,
        .package = "epwshiftr"
    )

    expect_error(esg_result("dataset")$load("file.json"), "Cannot load 'File' result")
    expect_s3_class(esg_result("file")$load("file.json"), "EsgResultFile")
})

test_that("base ESGF query results cannot choose a saved-result schema", {
    result <- new_query_result(
        EsgResult,
        index_node = "https://example.org",
        params = query_result_test_params("Dataset"),
        result = query_result_test_response(query_result_test_dataset_docs())
    )
    file <- tempfile(fileext = ".json")
    expect_error(result$save(file), "untyped ESGF result")
    writeLines("{}", file)
    expect_error(result$load(file), "untyped ESGF result")
})

test_that("ESGF query results save/load through real JSON files", {
    cases <- list(
        dataset = list(
            type = "Dataset",
            docs = query_result_test_dataset_docs()
        ),
        file = list(
            type = "File",
            docs = query_result_test_file_docs(c(
                "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
                "https://example.org/file.nc|application/netcdf|HTTPServer"
            ))
        ),
        aggregation = list(
            type = "Aggregation",
            docs = query_result_test_file_docs(c(
                "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
                "https://example.org/file.nc|application/netcdf|HTTPServer"
            ))
        )
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        result <- query_result_test_object(case$type, case$docs, query_result_test_params(case$type))
        file <- tempfile(fileext = ".json")

        expect_type(result$save(file), "character")
        json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
        expect_named(json$parameter, c("facet", "query", "control", "others"))

        loaded <- expect_s3_class(esg_result(case_name)$load(file), class(result)[[1L]])
        expect_identical(loaded$id, result$id)
        expect_identical(
            priv(loaded)$parameter$serialize(null = TRUE),
            priv(result)$parameter$serialize(null = TRUE)
        )
        expect_identical(names(loaded$to_data_table()), names(result$to_data_table()))
        expect_error(loaded$to_data_table(character()), "Must have length")

        unlink(file)
    }
})

test_that("empty query result fields are stable character vectors", {
    empty_dataset <- query_result_test_object(
        "Dataset",
        data.frame(check.names = FALSE),
        query_result_test_params("Dataset")
    )
    expect_identical(empty_dataset$fields, character())

    empty_file_with_fields <- query_result_test_object(
        "File",
        query_result_test_file_docs(character())[0L, ],
        query_result_test_params("File")
    )
    expect_true(is.character(empty_file_with_fields$fields))
    expect_true(all(c("filename", "url_opendap", "url_download") %in% empty_file_with_fields$fields))

    empty_file_without_fields <- query_result_test_object(
        "File",
        data.frame(check.names = FALSE),
        query_result_test_params("File")
    )
    expect_identical(empty_file_without_fields$fields, character())

    empty_aggregation_with_fields <- query_result_test_object(
        "Aggregation",
        query_result_test_file_docs(character())[0L, ],
        query_result_test_params("Aggregation")
    )
    expect_true(is.character(empty_aggregation_with_fields$fields))
    expect_true(all(c("url_opendap", "url_download") %in% empty_aggregation_with_fields$fields))

    empty_aggregation_without_fields <- query_result_test_object(
        "Aggregation",
        data.frame(check.names = FALSE),
        query_result_test_params("Aggregation")
    )
    expect_identical(empty_aggregation_without_fields$fields, character())
})

test_that("empty child query results save/load through real JSON files", {
    empty_file_docs <- query_result_test_file_docs(character())[0L, ]

    for (case_name in c("file", "aggregation")) {
        type <- switch(case_name, file = "File", aggregation = "Aggregation")
        result <- query_result_test_object(type, empty_file_docs, query_result_test_params(type))
        file <- tempfile(fileext = ".json")

        expect_type(result$save(file), "character")
        loaded <- expect_s3_class(esg_result(case_name)$load(file), class(result)[[1L]])
        expect_equal(loaded$count(), 0L)
        expect_identical(loaded$fields, character())
        expect_s3_class(loaded$to_data_table(), "data.table")

        unlink(file)
    }
})

test_that("empty dataset results collect empty child results without querying", {
    docs <- data.frame(id = character(), size = numeric(), check.names = FALSE)
    datasets <- query_result_test_object("Dataset", docs)

    testthat::local_mocked_bindings(
        query_collect = function(...) stop("query_collect should not be called"),
        .package = "epwshiftr"
    )

    files <- expect_s3_class(datasets$collect(type = "File"), "EsgResultFile")
    expect_equal(files$count(), 0L)
    expect_identical(files$fields, character())
    expect_identical(query_param_value(priv(files)$parameter$type()), "File")

    aggs <- expect_s3_class(datasets$collect(type = "Aggregation"), "EsgResultAggregation")
    expect_equal(aggs$count(), 0L)
    expect_identical(aggs$fields, character())
    expect_identical(query_param_value(priv(aggs)$parameter$type()), "Aggregation")
})

test_that("dataset result collect inherits controls and normalizes limit", {
    params <- query_result_test_params("Dataset", latest = FALSE, distrib = FALSE, replica = FALSE)
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = "dataset-1", size = 1, check.names = FALSE),
        params
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints
            )
            response <- query_result_test_response(query_result_test_file_docs())
            list(response = response, docs = response$response$docs)
        },
        .package = "epwshiftr"
    )

    expect_s3_class(datasets$collect(fields = "id", limit = NULL), "EsgResultFile")
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param_value(calls[[1L]]$params$latest()))
    expect_false(query_param_value(calls[[1L]]$params$distrib()))
    expect_false(query_param_value(calls[[1L]]$params$replica()))

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, latest = TRUE, distrib = TRUE, replica = TRUE),
        "EsgResultFile"
    )
    expect_equal(calls[[2L]]$limit, 1L)
    expect_true(query_param_value(calls[[2L]]$params$latest()))
    expect_true(query_param_value(calls[[2L]]$params$distrib()))
    expect_true(query_param_value(calls[[2L]]$params$replica()))
})

test_that("dataset access helpers tolerate missing access fields", {
    datasets <- query_result_test_object("Dataset", query_result_test_dataset_docs(access = FALSE))
    expect_identical(datasets$has_opendap(), c(FALSE, FALSE))
    expect_identical(datasets$has_download(), c(FALSE, FALSE))
})

test_that("URL helpers preserve result length for missing and malformed URLs", {
    docs <- data.frame(
        id = paste0("file-", 1:4),
        dataset_id = "dataset-1",
        size = 1,
        checksum = "abc",
        checksum_type = "SHA256",
        tracking_id = "hdl:21.14100/mock-file",
        title = paste0("file-", 1:4, ".nc"),
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(list(
        character(),
        "malformed",
        "https://example.org/file-3.nc|application/netcdf|HTTPServer",
        c(
            "https://example.org/dods/file-4.html|application/netcdf|OPENDAP",
            "https://example.org/dods/file-4-replica.html|application/netcdf|OPENDAP",
            "https://example.org/file-4.nc|application/netcdf|HTTPServer"
        )
    ))

    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    aggs <- query_result_test_object("Aggregation", docs, query_result_test_params("Aggregation"))

    expect_identical(
        files$url_download,
        c(NA_character_, NA_character_, "https://example.org/file-3.nc", "https://example.org/file-4.nc")
    )
    expect_warning(
        opendap <- files$url_opendap,
        "record 4 \\(id: file-4\\)"
    )
    expect_identical(opendap, c(NA_character_, NA_character_, NA_character_, "https://example.org/dods/file-4"))
    expect_identical(aggs$url_download, files$url_download)
    expect_warning(
        aggs_opendap <- aggs$url_opendap,
        "record 4 \\(id: file-4\\)"
    )
    expect_identical(aggs_opendap, opendap)
})

test_that("URL warning context is robust for nested or missing field values", {
    docs <- query_result_test_file_docs(c(
        "https://example.org/dods/file-1.html|application/netcdf|OPENDAP",
        "https://example.org/dods/file-1-replica.html|application/netcdf|OPENDAP"
    ))
    docs$id <- I(list(list(NA_character_, character())))
    docs$dataset_id <- I(list(c("dataset-1", "extra-context")))

    files <- query_result_test_object("File", docs, query_result_test_params("File"))

    expect_warning(
        opendap <- files$url_opendap,
        "record 1 \\(dataset_id: dataset-1\\)"
    )
    expect_identical(opendap, "https://example.org/dods/file-1")
})

test_that("open_dataset fallback behavior is explicit before side effects", {
    http_only <- query_result_test_object(
        "File",
        query_result_test_file_docs("https://example.org/file.nc|application/netcdf|HTTPServer"),
        query_result_test_params("File")
    )
    expect_error(http_only$open_dataset(fallback = "error"), "OPeNDAP is not available")
    expect_error(http_only$open_dataset(fallback = "ask"), "non-interactive")

    no_urls <- query_result_test_object(
        "File",
        query_result_test_file_docs(character()),
        query_result_test_params("File")
    )
    expect_error(no_urls$open_dataset(fallback = "auto"), "HTTPServer download URL")

    aggs_no_urls <- query_result_test_object(
        "Aggregation",
        query_result_test_file_docs(character()),
        query_result_test_params("Aggregation")
    )
    expect_error(aggs_no_urls$open_dataset(fallback = "auto"), "HTTPServer download URLs")
})

test_that("open_dataset falls back to HTTP after OPeNDAP open failures", {
    calls <- new.env(parent = emptyenv())
    calls$opened <- list()
    calls$downloads <- character()
    calls$closed <- list()

    FakeEsgDataset <- R6::R6Class(
        "FakeEsgDataset",
        lock_objects = FALSE,
        public = list(
            target = NULL,
            initialize = function(target, nc_handles = NULL) {
                self$target <- target
                private$nc_handles <- if (is.null(nc_handles)) {
                    vector("list", length(target))
                } else {
                    nc_handles
                }
                private$opened <- all(!vapply(private$nc_handles, is.null, logical(1L)))
            },
            open = function() {
                missing <- vapply(private$nc_handles, is.null, logical(1L))
                calls$opened[[length(calls$opened) + 1L]] <- self$target[missing]
                if (any(grepl("fail-opendap", self$target[missing]))) {
                    stop("remote boom", call. = FALSE)
                }
                private$nc_handles[missing] <- as.list(sprintf("handle:%s", self$target[missing]))
                private$opened <- TRUE
                self
            },
            close = function() {
                calls$closed[[length(calls$closed) + 1L]] <- private$nc_handles
                private$nc_handles <- vector("list", length(self$target))
                private$opened <- FALSE
                invisible(self)
            }
        ),
        active = list(
            is_open = function() {
                private$opened
            }
        ),
        private = list(
            nc_handles = NULL,
            opened = FALSE
        )
    )
    FakeFileDownloader <- R6::R6Class(
        "FakeFileDownloader",
        public = list(
            download = function(url, ...) {
                calls$downloads <- c(calls$downloads, url)
                local_path <- tempfile(fileext = ".nc")
                writeLines("netcdf", local_path)
                local_path
            }
        )
    )

    testthat::local_mocked_bindings(
        EsgDataset = FakeEsgDataset,
        FileDownloader = FakeFileDownloader,
        .package = "epwshiftr"
    )

    file_result <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            "https://example.org/fail-opendap.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        )),
        query_result_test_params("File")
    )

    err <- tryCatch(file_result$open_dataset(fallback = "error"), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "OPeNDAP is not available")
    expect_match(conditionMessage(err$parent), "remote boom")
    expect_error(file_result$open_dataset(fallback = "ask"), "non-interactive")

    ds <- expect_s3_class(file_result$open_dataset(fallback = "auto"), "FakeEsgDataset")
    expect_true(file.exists(ds$target))
    expect_identical(tail(calls$downloads, 1L), "https://example.org/file.nc")

    agg_docs <- data.frame(
        id = c("file-1", "file-2"),
        dataset_id = "dataset-1",
        size = c(1, 2),
        checksum = c("abc", "def"),
        checksum_type = "SHA256",
        tracking_id = c("hdl:21.14100/mock-file-1", "hdl:21.14100/mock-file-2"),
        title = c("file-1.nc", "file-2.nc"),
        data_node = "example.org",
        check.names = FALSE
    )
    agg_docs$url <- I(list(
        c(
            "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file-1.nc|application/netcdf|HTTPServer"
        ),
        "https://example.org/file-2.nc|application/netcdf|HTTPServer"
    ))
    agg_result <- query_result_test_object("Aggregation", agg_docs, query_result_test_params("Aggregation"))

    opened_before <- length(calls$opened)
    downloads_before <- calls$downloads
    agg_ds <- expect_s3_class(agg_result$open_dataset(fallback = "auto"), "FakeEsgDataset")
    expect_length(agg_ds$target, 2L)
    expect_identical(agg_ds$target[[1L]], "https://example.org/dods/file-1.nc")
    expect_true(file.exists(agg_ds$target[[2L]]))
    expect_identical(tail(calls$downloads, length(calls$downloads) - length(downloads_before)), "https://example.org/file-2.nc")
    new_open_calls <- calls$opened[(opened_before + 1L):length(calls$opened)]
    expect_identical(new_open_calls[[1L]], "https://example.org/dods/file-1.nc")
    expect_identical(new_open_calls[[2L]], agg_ds$target[[2L]])

    agg_fail_docs <- agg_docs
    agg_fail_docs$url <- I(list(
        c(
            "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file-1.nc|application/netcdf|HTTPServer"
        ),
        c(
            "https://example.org/fail-opendap-file-2.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file-2.nc|application/netcdf|HTTPServer"
        )
    ))
    agg_fail_result <- query_result_test_object("Aggregation", agg_fail_docs, query_result_test_params("Aggregation"))

    err <- tryCatch(agg_fail_result$open_dataset(fallback = "error"), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "OPeNDAP is not available")
    expect_match(conditionMessage(err$parent), "remote boom")
    expect_true(any(vapply(calls$closed, function(handles) {
        identical(handles, list("handle:https://example.org/dods/file-1.nc"))
    }, logical(1L))))

    downloads_before <- calls$downloads
    agg_fail_ds <- expect_s3_class(agg_fail_result$open_dataset(fallback = "auto"), "FakeEsgDataset")
    expect_identical(agg_fail_ds$target[[1L]], "https://example.org/dods/file-1.nc")
    expect_true(file.exists(agg_fail_ds$target[[2L]]))
    expect_identical(tail(calls$downloads, length(calls$downloads) - length(downloads_before)), "https://example.org/file-2.nc")
})

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
            "number_of_files",
            "project",
            "size",
            "source_id",
            "variable_id",
            "variant_label"
        )
    )

    # $id
    expect_type(datasets$id, "character")
    expect_length(datasets$id, 2L)

    # $url
    expect_null(datasets$url)

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
    de <- expect_s3_class(new_query_result(EsgResultDataset)$load(file), "EsgResultDataset")
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

# esg_result() {{{
test_that("esg_result() works", {
    expect_s3_class(esg_result(), "EsgResultDataset")
    expect_s3_class(esg_result("file"), "EsgResultFile")
    expect_s3_class(esg_result(), "EsgResultDataset")

    expect_snapshot(esg_result("file")$print(), transform = transform_print)
    expect_snapshot(esg_result("aggregation")$print(), transform = transform_print)
    expect_snapshot(esg_result("aggregation")$print(), transform = transform_print)
})
# }}}

# vim: fdm=marker :
