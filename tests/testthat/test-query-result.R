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

query_result_test_object <- function(type = "Dataset", docs, params = query_result_test_params(type), context = NULL) {
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
        result = query_result_test_response(docs),
        context = context
    )
}

query_result_test_state <- function(type = "Dataset", docs, params = query_result_test_params(type), context = NULL) {
    list(
        index_node = "https://example.org",
        parameter = params,
        response = query_result_test_response(docs),
        context = context
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
        instance_id = "file-instance-1",
        master_id = "master-file-1",
        replica = FALSE,
        tracking_id = "hdl:21.14100/mock-file",
        title = "file.nc",
        version = 20260101L,
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(list(url))
    docs
}

query_result_test_file_time_docs <- function(type = "File") {
    docs <- data.frame(
        id = c(
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20500101-20501231.nc|dataset-1",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20800101-20801231.nc|dataset-1",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_unknown.nc|dataset-1"
        ),
        dataset_id = "dataset-1",
        size = c(1, 2, 3),
        checksum = c("abc", "def", "ghi"),
        checksum_type = "SHA256",
        instance_id = c("file-instance-2050", "file-instance-2080", "file-instance-unknown"),
        master_id = c("master-file-2050", "master-file-2080", "master-file-unknown"),
        replica = FALSE,
        version = c(20260101L, 20260101L, 20260101L),
        tracking_id = c(
            "hdl:21.14100/mock-file-2050",
            "hdl:21.14100/mock-file-2080",
            "hdl:21.14100/mock-file-unknown"
        ),
        title = c(
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20500101-20501231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20800101-20801231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_unknown.nc"
        ),
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(list(
        "https://example.org/dods/2050.nc.html|application/netcdf|OPENDAP",
        "https://example.org/dods/2080.nc.html|application/netcdf|OPENDAP",
        "https://example.org/dods/unknown.nc.html|application/netcdf|OPENDAP"
    ))
    if (identical(type, "Aggregation")) {
        docs$checksum <- NULL
        docs$checksum_type <- NULL
        docs$master_id <- NULL
        docs$tracking_id <- NULL
    }

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

test_that("ESGF query results preserve ESGF doc timestamp fields on save", {
    docs <- query_result_test_file_docs()
    docs$timestamp <- "2026-06-09T00:00:00Z"
    docs$`_timestamp` <- "2026-06-10T01:14:40.946Z"
    docs$version <- "v20240509"
    result <- query_result_test_object("File", docs, query_result_test_params("File"))
    file <- tempfile(fileext = ".json")

    expect_type(result$save(file), "character")
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_true("timestamp" %in% names(json$response$response$docs))
    expect_true("_timestamp" %in% names(json$response$response$docs))
    expect_identical(json$response$response$docs$timestamp, "2026-06-09T00:00:00Z")
    expect_identical(json$response$response$docs$`_timestamp`, "2026-06-10T01:14:40.946Z")

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_true("timestamp" %in% loaded$fields)
    expect_true("_timestamp" %in% loaded$fields)
    expect_identical(loaded$timestamp, "2026-06-09T00:00:00Z")
    expect_identical(loaded$`_timestamp`, "2026-06-10T01:14:40.946Z")
    expect_identical(loaded$version, "v20240509")
})

test_that("ESGF query results expose recorded query URLs", {
    params <- query_result_test_params("Dataset")
    result <- query_result_test_object("Dataset", query_result_test_dataset_docs(), params)
    expect_identical(
        result$query_url(),
        stats::setNames(query_build("https://example.org", params), "page1")
    )
    expect_identical(result$query_url("all"), result$query_url())

    urls <- c("https://example.org/search?page=1", "https://example.org/search?page=2")
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File"),
        context = list(query_url = urls)
    )
    expect_identical(result$query_url(), stats::setNames(urls[[1L]], "page1"))
    expect_identical(result$query_url("all"), stats::setNames(urls, c("page1", "page2")))
    expect_error(result$query_url("last"), "'arg' should be one of")
})

test_that("result query URL context persists through save/load", {
    urls <- c("https://example.org/search?page=1", "https://example.org/search?page=2")
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File"),
        context = list(query_url = urls)
    )
    file <- tempfile(fileext = ".json")

    expect_type(result$save(file), "character")
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_identical(json$context$query_url, urls)

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(loaded$query_url("all"), stats::setNames(urls, c("page1", "page2")))
})

test_that("File and Aggregation results filter time using DRS filename ranges", {
    for (type in c("File", "Aggregation")) {
        result <- query_result_test_object(type, query_result_test_file_time_docs(type), query_result_test_params(type))

        expect_false("datetime_start" %in% result$fields)
        warnings <- character()
        filtered <- withCallingHandlers(
            result$filter_time("2050-06-01", "2050-06-30", method = "drs"),
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        expect_true(any(grepl("DRS filename", warnings)))
        expect_true(any(grepl("Could not parse", warnings)))
        expect_s3_class(filtered, class(result)[[1L]])
        expect_identical(filtered$id, result$id[c(1L, 3L)])
        expect_false("datetime_start" %in% result$fields)
        expect_true(all(c("datetime_start", "datetime_end") %in% filtered$fields))
        expect_identical(filtered$time_filter$method, "drs")
        expect_identical(filtered$time_filter$total, 3L)
        expect_identical(filtered$time_filter$selected, 2L)
        expect_identical(filtered$time_filter$unknown_count, 1L)

        dt <- filtered$to_data_table(c("id", "datetime_start", "datetime_end"))
        expect_identical(dt$datetime_start[[1L]], "2050-01-01T00:00:00Z")
        expect_identical(dt$datetime_end[[1L]], "2050-12-31T23:59:59Z")
        expect_true(is.na(dt$datetime_start[[2L]]))
        expect_true(is.na(dt$datetime_end[[2L]]))
    }
})

test_that("result time filter context persists through save/load", {
    result <- query_result_test_object("File", query_result_test_file_time_docs(), query_result_test_params("File"))
    filtered <- suppressWarnings(result$filter_time("2050-06-01", "2050-06-30", method = "drs"))
    file <- tempfile(fileext = ".json")

    expect_type(filtered$save(file), "character")
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_named(json$context, "time_filter")
    expect_identical(json$context$time_filter$method, "drs")

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(loaded$time_filter, filtered$time_filter)
    expect_identical(loaded$id, filtered$id)
})

test_that("File results filter time using OPeNDAP time axes", {
    FakeEsgDataset <- R6::R6Class(
        "FakeEsgDataset",
        public = list(
            target = NULL,
            initialize = function(target) {
                self$target <- target
            },
            open = function() {
                private$opened <- TRUE
                self
            },
            close = function() {
                private$opened <- FALSE
                invisible(self)
            },
            get_time_axis = function(...) {
                if (grepl("unknown", self$target)) {
                    stop("no time axis", call. = FALSE)
                }
                if (grepl("2050", self$target)) {
                    values <- as.POSIXct(c("2050-01-01", "2050-12-31"), tz = "UTC")
                } else {
                    values <- as.POSIXct(c("2080-01-01", "2080-12-31"), tz = "UTC")
                }
                list(values = values)
            }
        ),
        active = list(
            is_open = function() private$opened
        ),
        private = list(
            opened = FALSE
        )
    )
    testthat::local_mocked_bindings(EsgDataset = FakeEsgDataset, .package = "epwshiftr")

    result <- query_result_test_object("File", query_result_test_file_time_docs(), query_result_test_params("File"))
    warnings <- character()
    filtered <- withCallingHandlers(
        result$filter_time("2050-06-01", "2050-06-30", method = "opendap"),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_true(any(grepl("OPeNDAP time axes", warnings)))
    expect_identical(filtered$id, result$id[c(1L, 3L)])
    expect_identical(filtered$time_filter$method, "opendap")
    expect_identical(filtered$time_filter$unknown_count, 1L)
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

test_that("to_data_table accepts all advertised fields", {
    docs <- query_result_test_file_docs(c(
        "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    ))

    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    expect_identical(
        files$fields,
        c(names(docs), "filename", "url_opendap", "url_download")
    )
    expect_identical(names(files$to_data_table()), files$fields)
    expect_identical(names(files$to_data_table(files$fields)), files$fields)
    expect_identical(
        names(files$to_data_table(c("url_download", "id", "filename"))),
        c("url_download", "id", "filename")
    )
    expect_identical(files$to_data_table("filename")$filename, "file.nc")
    expect_identical(files$to_data_table("url_opendap")$url_opendap, "https://example.org/dods/file.nc")
    expect_identical(files$to_data_table("url_download")$url_download, "https://example.org/file.nc")
    expect_error(files$to_data_table(character()), "Must have length")

    aggs <- query_result_test_object("Aggregation", docs, query_result_test_params("Aggregation"))
    expect_identical(
        aggs$fields,
        c(names(docs), "url_opendap", "url_download")
    )
    expect_identical(names(aggs$to_data_table()), aggs$fields)
    expect_identical(names(aggs$to_data_table(aggs$fields)), aggs$fields)
    expect_identical(
        names(aggs$to_data_table(c("url_opendap", "url_download"))),
        c("url_opendap", "url_download")
    )
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

    for (case_name in c("file", "aggregation")) {
        type <- switch(case_name, file = "File", aggregation = "Aggregation")
        generator <- switch(case_name, file = EsgResultFile, aggregation = EsgResultAggregation)
        response <- query_result_test_response(empty_file_docs)
        response$response$docs <- list()
        result <- new_query_result(
            generator,
            "https://example.org",
            query_result_test_params(type),
            response
        )
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

    expect_error(datasets$collect(which = 1L, type = "File"), "empty Dataset result")
    expect_error(datasets$collect(which = "dataset-1", type = "Aggregation"), "empty Dataset result")

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
    params$source_id("AWI-CM-1-1-MR")
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
            params$fields(c(query_param_value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(datasets$collect(fields = "id", limit = NULL), "EsgResultFile")
    expect_identical(calls[[1L]]$index_node, "https://example.org")
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param_value(calls[[1L]]$params$latest()))
    expect_false(query_param_value(calls[[1L]]$params$distrib()))
    expect_false(query_param_value(calls[[1L]]$params$replica()))
    expect_identical(query_param_value(calls[[1L]]$params$source_id()), "AWI-CM-1-1-MR")
    expect_true(all(EsgResultFile$private_fields$required_fields %in% query_param_value(priv(files)$parameter$fields())))

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, latest = TRUE, distrib = TRUE, replica = TRUE),
        "EsgResultFile"
    )
    expect_equal(calls[[2L]]$limit, 1L)
    expect_true(query_param_value(calls[[2L]]$params$latest()))
    expect_true(query_param_value(calls[[2L]]$params$distrib()))
    expect_true(query_param_value(calls[[2L]]$params$replica()))

    aggs <- expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, type = "Aggregation", index_node = "esg-dn1.nsc.liu.se"),
        "EsgResultAggregation"
    )
    expect_identical(calls[[3L]]$index_node, "https://esg-dn1.nsc.liu.se")
    expect_identical(priv(aggs)$index_node, "https://esg-dn1.nsc.liu.se")
    expect_equal(calls[[3L]]$limit, 1L)
    expect_false(query_param_value(calls[[3L]]$params$latest()))
    expect_false(query_param_value(calls[[3L]]$params$distrib()))
    expect_false(query_param_value(calls[[3L]]$params$replica()))
    expect_null(calls[[3L]]$params$project())
    expect_null(calls[[3L]]$params$source_id())
    expect_identical(query_param_value(calls[[3L]]$params$flat()$dataset_id), "dataset-1")
    expect_true(all(EsgResultAggregation$private_fields$required_fields %in% query_param_value(priv(aggs)$parameter$fields())))
})

test_that("dataset result collect accepts child facets and clears datetime constraints", {
    params <- query_result_test_params("Dataset")
    params$datetime_range(
        start = "2050-01-01T00:00:00Z",
        stop = "2080-12-31T23:59:59Z"
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = "dataset-1", size = 1, check.names = FALSE),
        params
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
            calls[[length(calls) + 1L]] <<- list(params = params, limit = limit)
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(query_param_value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, source_id = "AWI-CM-1-1-MR"),
        "EsgResultFile"
    )
    expect_identical(query_param_value(calls[[1L]]$params$source_id()), "AWI-CM-1-1-MR")
    expect_identical(calls[[1L]]$params$render(c("datetime_start", "datetime_stop")), character())

    expect_error(datasets$collect(datetime_start = "2050"), "controlled")
    expect_error(datasets$collect(time = "all"), "controlled")
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

test_that("download_plan builds current HTTPServer plans with logical file identity", {
    files <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        )),
        query_result_test_params("File")
    )

    plan <- files$download_plan(replica = "current", probe = FALSE, strategy = "first")
    expect_s3_class(plan, "data.table")
    expect_named(
        plan,
        c(
            "logical_file_id", "record_index", "file_key", "esgf_id",
            "dataset_id", "filename", "subdir", "checksum",
            "checksum_type", "size", "url", "service", "data_node",
            "priority", "probe_latency", "probe_throughput", "probe_cached",
            "node_success_count", "node_failure_count",
            "node_attempt_count", "node_success_rate", "node_avg_latency",
            "node_probe_success_count", "node_probe_failure_count",
            "node_cooldown_until", "node_is_cooling_down",
            "node_updated_at", "node_last_probe_at", "node_cooldown_rank"
        )
    )
    expect_identical(plan$logical_file_id, "master:master-file-1")
    expect_identical(plan$url, "https://example.org/file.nc")
    expect_identical(plan$checksum_type, "sha256")
    expect_identical(plan$priority, 1L)

    checksum_docs <- query_result_test_file_docs("https://example.org/file.nc|application/netcdf|HTTPServer")
    checksum_docs$master_id <- NA_character_
    checksum_docs$tracking_id <- NA_character_
    checksum_result <- query_result_test_object("File", checksum_docs, query_result_test_params("File"))
    checksum_plan <- checksum_result$download_plan(replica = "current", probe = FALSE)
    expect_identical(checksum_plan$logical_file_id, "checksum:abc:1:file.nc")

    expect_error(files$download(run = FALSE), "explicit `store` or persistent `downloader`")
})

test_that("download_plan deduplicates URL probes", {
    docs <- data.table::rbindlist(list(
        query_result_test_file_docs("https://same.example.org/file.nc|application/netcdf|HTTPServer"),
        query_result_test_file_docs("https://same.example.org/file.nc|application/netcdf|HTTPServer")
    ), fill = TRUE)
    docs$id <- c("file-1", "file-2")
    docs$data_node <- "same.example.org"
    docs$master_id <- c("master-file-1", "master-file-2")

    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    calls <- 0L
    testthat::local_mocked_bindings(
        query_result_probe_url = function(url, timeout = 5, network_policy = NULL) {
            calls <<- calls + 1L
            list(latency = 0.5, throughput = NA_real_)
        },
        .package = "epwshiftr"
    )

    plan <- files$download_plan(
        replica = "current",
        probe = TRUE,
        strategy = "first",
        probe_concurrency = 1L,
        probe_cache_seconds = 0L
    )
    expect_equal(calls, 1L)
    expect_equal(plan$probe_latency, c(0.5, 0.5))
    expect_false(any(plan$probe_cached))
})

test_that("download_plan reuses fresh data node probe cache", {
    docs <- query_result_test_file_docs("https://cache.example.org/file.nc|application/netcdf|HTTPServer")
    docs$data_node <- "cache.example.org"
    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    node_stats <- data.table::data.table(
        data_node = "cache.example.org",
        service = "HTTPServer",
        success_count = 1L,
        failure_count = 0L,
        avg_latency = 0.25,
        probe_success_count = 1L,
        probe_failure_count = 0L,
        last_probe_at = Sys.time(),
        updated_at = Sys.time()
    )
    testthat::local_mocked_bindings(
        query_result_probe_url = function(...) {
            stop("cached probe should not hit the network")
        },
        .package = "epwshiftr"
    )

    plan <- files$download_plan(
        replica = "current",
        probe = TRUE,
        node_stats = node_stats,
        probe_cache_seconds = 3600L
    )
    expect_equal(plan$probe_latency, 0.25)
    expect_true(plan$probe_cached)
})

test_that("download_plan uses data node history to rank replica candidates", {
    docs <- data.table::rbindlist(list(
        query_result_test_file_docs("https://slow.example.org/file.nc|application/netcdf|HTTPServer"),
        query_result_test_file_docs("https://fast.example.org/file.nc|application/netcdf|HTTPServer")
    ), fill = TRUE)
    docs$id <- c("file-slow", "file-fast")
    docs$data_node <- c("slow.example.org", "fast.example.org")
    docs$master_id <- "master-file-shared"
    docs$tracking_id <- "hdl:21.14100/mock-file-shared"

    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    node_stats <- data.table::data.table(
        data_node = c("slow.example.org", "fast.example.org"),
        service = "HTTPServer",
        success_count = c(1L, 10L),
        failure_count = c(9L, 0L),
        avg_latency = c(10, 1)
    )

    plan <- files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = node_stats
    )
    expect_identical(plan$data_node, c("fast.example.org", "slow.example.org"))
    expect_equal(plan$priority, c(1L, 2L))
    expect_equal(plan$node_success_rate, c(1, 0.1))
})

test_that("download_plan ranks cooling data nodes after available candidates", {
    docs <- data.table::rbindlist(list(
        query_result_test_file_docs("https://cooling.example.org/file.nc|application/netcdf|HTTPServer"),
        query_result_test_file_docs("https://ready.example.org/file.nc|application/netcdf|HTTPServer")
    ), fill = TRUE)
    docs$id <- c("file-cooling", "file-ready")
    docs$data_node <- c("cooling.example.org", "ready.example.org")
    docs$master_id <- "master-file-cooldown"
    docs$tracking_id <- "hdl:21.14100/mock-file-cooldown"

    files <- query_result_test_object("File", docs, query_result_test_params("File"))
    node_stats <- data.table::data.table(
        data_node = c("cooling.example.org", "ready.example.org"),
        service = "HTTPServer",
        success_count = c(10L, 1L),
        failure_count = c(0L, 0L),
        avg_latency = c(0.1, 5),
        probe_success_count = c(0L, 0L),
        probe_failure_count = c(3L, 0L),
        cooldown_until = c(Sys.time() + 3600, as.POSIXct(NA)),
        updated_at = Sys.time()
    )

    plan <- files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = node_stats
    )
    expect_identical(plan$data_node, c("ready.example.org", "cooling.example.org"))
    expect_equal(plan$node_cooldown_rank, c(0L, 1L))

    all_cooling <- node_stats
    all_cooling$cooldown_until <- Sys.time() + 3600
    plan <- files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = all_cooling
    )
    expect_true(all(plan$node_cooldown_rank == 0L))
})

test_that("open_dataset fallback behavior is explicit before side effects", {
    http_only <- query_result_test_object(
        "File",
        query_result_test_file_docs("https://example.org/file.nc|application/netcdf|HTTPServer"),
        query_result_test_params("File")
    )
    expect_error(http_only$open_dataset(fallback = "error"), "OPeNDAP is not available")
    expect_error(http_only$open_dataset(fallback = "ask"), "non-interactive")
    expect_error(http_only$open_dataset(fallback = "auto"), "explicit `store` or `downloader`")

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
    expect_error(
        aggs_no_urls$open_dataset(fallback = "auto"),
        "record 1 \\(id: file-1\\)"
    )
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
            initialize = function(target) {
                self$target <- target
                private$nc_handles <- vector("list", length(target))
                private$opened <- FALSE
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
    FakeDownloader <- R6::R6Class(
        "FakeDownloader",
        public = list(
            plan = NULL,
            enqueue = function(plan, session_label = NULL) {
                self$plan <- data.table::as.data.table(plan)
                "session-1"
            },
            run = function(session_id = NULL, ...) {
                paths <- vapply(self$plan$url, function(url) {
                    calls$downloads <- c(calls$downloads, url)
                    local_path <- tempfile(fileext = ".nc")
                    writeLines("netcdf", local_path)
                    local_path
                }, character(1L))
                data.table::data.table(
                    task_id = paste0("task-", seq_along(paths)),
                    session_id = session_id,
                    logical_file_id = self$plan$logical_file_id,
                    status = "done",
                    target_path = paths,
                    selected_url = self$plan$url
                )
            }
        )
    )

    testthat::local_mocked_bindings(
        EsgDataset = FakeEsgDataset,
        Downloader = FakeDownloader,
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

    ds <- expect_s3_class(
        file_result$open_dataset(fallback = "auto", downloader = FakeDownloader$new()),
        "FakeEsgDataset"
    )
    expect_true(file.exists(ds$target))
    expect_identical(tail(calls$downloads, 1L), "https://example.org/file.nc")
    expect_error(file_result$open_dataset(index = 1L), "unused argument")

    multi_file_docs <- data.frame(
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
    multi_file_docs$url <- I(list(
        "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
        "https://example.org/dods/file-2.nc.html|application/netcdf|OPENDAP"
    ))
    multi_files <- query_result_test_object("File", multi_file_docs, query_result_test_params("File"))
    opened_before <- length(calls$opened)
    ds_all <- expect_s3_class(multi_files$open_dataset(fallback = "auto"), "FakeEsgDataset")
    expect_identical(ds_all$target, c(
        "https://example.org/dods/file-1.nc",
        "https://example.org/dods/file-2.nc"
    ))
    expect_equal(length(calls$opened) - opened_before, 2L)

    ds_one <- expect_s3_class(
        multi_files$open_dataset(which = "file-2", aggregate = FALSE, fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(ds_one$target, "https://example.org/dods/file-2.nc")
    expect_error(multi_files$open_dataset(which = 1:2, aggregate = FALSE), "aggregate = FALSE")

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
    expect_message(
        agg_ds <- expect_s3_class(
            agg_result$open_dataset(fallback = "auto", downloader = FakeDownloader$new()),
            "FakeEsgDataset"
        ),
        "record 2 \\(id: file-2\\)"
    )
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
    expect_match(conditionMessage(err), "record 2 \\(id: file-2\\)")
    expect_match(conditionMessage(err$parent), "remote boom")
    expect_true(any(vapply(calls$closed, function(handles) {
        identical(handles, list("handle:https://example.org/dods/file-1.nc"))
    }, logical(1L))))

    downloads_before <- calls$downloads
    expect_message(
        agg_fail_ds <- expect_s3_class(
            agg_fail_result$open_dataset(fallback = "auto", downloader = FakeDownloader$new()),
            "FakeEsgDataset"
        ),
        "record 2 \\(id: file-2\\)"
    )
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
    expect_true(
        all(c(EsgResultFile$private_fields$required_fields, "filename", "url_opendap", "url_download") %in% files$fields)
    )

    ## $collect(): can collect all fields
    expect_s3_class(files <- datasets$collect(fields = "id", all = TRUE), "EsgResultFile")
    expect_equal(files$count(), sum(datasets$number_of_files))

    ## $collect(): can specify specific parameters
    expect_s3_class(datasets$collect(fields = "id", limit = 1, replica = FALSE), "EsgResultFile")

    ## $collect(): can collect all possible fields
    expect_s3_class(files <- datasets$collect(limit = 1), "EsgResultFile")
    expect_length(files$fields, 56L)

    ## $collect(): can specify additional child-result facet filters
    expect_s3_class(datasets$collect(fields = "id", limit = 1, experiment_id = "ssp585"), "EsgResultFile")

    ## $collect(): can stop if controlled query parameters are supplied through dots
    expect_error(datasets$collect(datetime_start = "2050"), "controlled")

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
        all(c("data_node", "dataset_id", "id", "size", "title", "url", "url_opendap", "url_download") %in% aggs$fields)
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
