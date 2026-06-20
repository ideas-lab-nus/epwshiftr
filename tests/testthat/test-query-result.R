# EsgResult {{{
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
        latest = NULL,
        distrib = TRUE,
        limit = 1L,
        type = type,
        format = QUERY_PARAM__FORMAT_JSON
    )
    params[names(values)] <- values

    query_param__as_store(params)
}

query_result_test_object <- function(type = "Dataset", docs, params = query_result_test_params(type), context = NULL) {
    generator <- switch(
        type,
        Dataset = EsgResultDataset,
        File = EsgResultFile,
        Aggregation = EsgResultAggregation
    )
    query_result__new(
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

query_result_test_contract_dataset_docs <- function() {
    docs <- data.frame(
        id = c(
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp126.r1i1p1f1.day.tas.gn.v20190529|esgf.example.org",
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp245.r1i1p1f1.day.tas.gn.v20190529|esgf.example.org"
        ),
        version = c(20190529L, 20190529L),
        activity_id = c("ScenarioMIP", "ScenarioMIP"),
        data_node = c("esgf.example.org", "esgf.example.org"),
        experiment_id = c("ssp126", "ssp245"),
        frequency = c("day", "day"),
        index_node = c("https://esgf.example.org", "https://esgf.example.org"),
        instance_id = c("dataset-instance-ssp126", "dataset-instance-ssp245"),
        latest = c(TRUE, TRUE),
        master_id = c("dataset-master-ssp126", "dataset-master-ssp245"),
        number_of_files = c(2L, 1L),
        project = c("CMIP6", "CMIP6"),
        replica = c(FALSE, FALSE),
        size = c(1024^3, 2 * 1024^3),
        source_id = c("AWI-CM-1-1-MR", "AWI-CM-1-1-MR"),
        variable_id = c("tas", "tas"),
        variant_label = c("r1i1p1f1", "r1i1p1f1"),
        check.names = FALSE
    )
    docs$access <- I(list(c("OPENDAP", "HTTPServer"), "HTTPServer"))
    docs
}

query_result_test_contract_file_docs <- function() {
    dataset_ids <- query_result_test_contract_dataset_docs()$id
    docs <- data.frame(
        id = c(
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20500101-20501231.nc|dataset-ssp126",
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20510101-20511231.nc|dataset-ssp126",
            "tas_day_AWI-CM-1-1-MR_ssp245_r1i1p1f1_gn_20500101-20501231.nc|dataset-ssp245"
        ),
        version = c(20190529L, 20190529L, 20190529L),
        activity_id = c("ScenarioMIP", "ScenarioMIP", "ScenarioMIP"),
        institution_id = c("AWI", "AWI", "AWI"),
        dataset_id = c(dataset_ids[[1L]], dataset_ids[[1L]], dataset_ids[[2L]]),
        size = c(1024^2, 2 * 1024^2, 3 * 1024^2),
        checksum = c("sha-1", "sha-2", "sha-3"),
        checksum_type = c("SHA256", "SHA256", "SHA256"),
        instance_id = c("file-instance-1", "file-instance-2", "file-instance-3"),
        master_id = c("file-master-1", "file-master-2", "file-master-3"),
        replica = c(FALSE, FALSE, FALSE),
        tracking_id = c("hdl:21.14100/mock-1", "hdl:21.14100/mock-2", "hdl:21.14100/mock-3"),
        title = c(
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20500101-20501231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20510101-20511231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp245_r1i1p1f1_gn_20500101-20501231.nc"
        ),
        data_node = c("esgf.example.org", "esgf.example.org", "esgf.example.org"),
        check.names = FALSE
    )
    docs$url <- I(list(
        c(
            "https://esgf.example.org/dods/ssp126-2050.nc.html|application/netcdf|OPENDAP",
            "https://esgf.example.org/files/ssp126-2050.nc|application/netcdf|HTTPServer"
        ),
        c(
            "https://esgf.example.org/dods/ssp126-2051.nc.html|application/netcdf|OPENDAP",
            "https://esgf.example.org/files/ssp126-2051.nc|application/netcdf|HTTPServer"
        ),
        c(
            "https://esgf.example.org/dods/ssp245-2050.nc.html|application/netcdf|OPENDAP",
            "https://esgf.example.org/files/ssp245-2050.nc|application/netcdf|HTTPServer"
        )
    ))
    docs
}

query_result_test_contract_aggregation_docs <- function() {
    dataset_ids <- query_result_test_contract_dataset_docs()$id
    docs <- data.frame(
        id = c(
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp126.r1i1p1f1.day.tas.gn.tas.20500101.aggregation|esgf.example.org",
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp245.r1i1p1f1.day.tas.gn.tas.20500101.aggregation|esgf.example.org"
        ),
        version = c(20190529L, 20190529L),
        activity_id = c("ScenarioMIP", "ScenarioMIP"),
        institution_id = c("AWI", "AWI"),
        data_node = c("esgf.example.org", "esgf.example.org"),
        dataset_id = c(dataset_ids[[1L]], dataset_ids[[2L]]),
        instance_id = c("aggregation-instance-1", "aggregation-instance-2"),
        master_id = c("aggregation-master-1", "aggregation-master-2"),
        replica = c(FALSE, FALSE),
        size = c(0, 0),
        title = c("tas aggregation 2050 ssp126", "tas aggregation 2050 ssp245"),
        check.names = FALSE
    )
    docs$url <- I(list(
        c(
            "https://esgf.example.org/dods/ssp126-aggregation.ncml|application/netcdf|OPENDAP",
            "https://esgf.example.org/files/ssp126-aggregation.ncml|application/netcdf|HTTPServer"
        ),
        c(
            "https://esgf.example.org/dods/ssp245-aggregation.ncml|application/netcdf|OPENDAP",
            "https://esgf.example.org/files/ssp245-aggregation.ncml|application/netcdf|HTTPServer"
        )
    ))
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
# EsgResult$load() / EsgResult$save() {{{
test_that("EsgResult$load() restores dynamic fields", {
    state <- query_result_test_state("Dataset", query_result_test_dataset_docs())
    testthat::local_mocked_bindings(
        query__load = function(file, schema = NULL) state,
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

test_that("EsgResult$load() validates result type", {
    state <- query_result_test_state("File", query_result_test_file_docs(), query_result_test_params("File"))
    testthat::local_mocked_bindings(
        query__load = function(file, schema = NULL) state,
        .package = "epwshiftr"
    )

    expect_error(esg_result("dataset")$load("file.json"), "Cannot load 'File' result")
    expect_s3_class(esg_result("file")$load("file.json"), "EsgResultFile")
})

test_that("EsgResult$save() / EsgResult$load() reject untyped base results", {
    result <- query_result__new(
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

test_that("EsgResult$save() / EsgResult$load() round-trip through JSON files", {
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
        expect_true(all(c("project", "fields", "type", "limit", "format") %in% names(json$parameter)))

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

test_that("EsgResult$save() / EsgResult$load() preserve ESGF doc timestamp fields", {
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
# }}}
# EsgResult$query_url() {{{
test_that("EsgResult$query_url() exposes recorded query URLs", {
    params <- query_result_test_params("Dataset")
    result <- query_result_test_object("Dataset", query_result_test_dataset_docs(), params)
    expect_identical(
        result$query_url(),
        stats::setNames(query__build("https://example.org", params), "page1")
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

test_that("EsgResult$query_url() context persists through save/load", {
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
# }}}
# EsgResult$slice() / EsgResult$selection() {{{
test_that("EsgResult$slice() / EsgResult$selection() support local selection", {
    urls <- c("https://example.org/search?page=1", "https://example.org/search?page=2")
    time_filter <- list(
        start = "2050-01-01T00:00:00Z",
        stop = "2050-12-31T23:59:59Z",
        method = "drs"
    )
    cases <- list(
        Dataset = query_result_test_dataset_docs(),
        File = query_result_test_file_time_docs("File"),
        Aggregation = query_result_test_file_time_docs("Aggregation")
    )

    for (type in names(cases)) {
        result <- query_result_test_object(
            type,
            cases[[type]],
            query_result_test_params(type),
            context = list(query_url = urls, time_filter = time_filter)
        )
        priv(result)$response$response$numFound <- 10L

        expect_identical(result[], result)
        expect_identical(result$selection(), list(
            source_count = as.integer(nrow(cases[[type]])),
            source_num_found = 10L,
            source_indices = seq_len(nrow(cases[[type]]))
        ))

        selected <- result[c(nrow(cases[[type]]), 1L)]
        expect_s3_class(selected, class(result)[[1L]])
        expect_identical(selected$id, result$id[c(nrow(cases[[type]]), 1L)])
        expect_identical(selected$query_url("all"), stats::setNames(urls, c("page1", "page2")))
        expect_identical(selected$time_filter, time_filter)
        expect_identical(selected$selection(), list(
            source_count = as.integer(nrow(cases[[type]])),
            source_num_found = 10L,
            source_indices = as.integer(c(nrow(cases[[type]]), 1L))
        ))

        chained <- selected[2L]
        expect_identical(chained$id, result$id[1L])
        expect_identical(chained$selection()$source_indices, 1L)

        logical_selected <- result[seq_len(nrow(cases[[type]])) %% 2L == 1L]
        expect_identical(logical_selected$id, result$id[seq_len(nrow(cases[[type]])) %% 2L == 1L])

        character_selected <- result[result$id[1L]]
        expect_identical(character_selected$id, result$id[1L])

        empty <- result$slice(NULL)
        expect_s3_class(empty, class(result)[[1L]])
        expect_identical(empty$id, character())
        expect_identical(empty$selection(), list(
            source_count = as.integer(nrow(cases[[type]])),
            source_num_found = 10L,
            source_indices = integer()
        ))

        expect_identical(result[-1L]$id, result$id[-1L])
    }
})
# }}}
# EsgResult$filter() {{{
test_that("EsgResult$filter() filters with predicates", {
    result <- query_result_test_object("File", query_result_test_file_time_docs("File"), query_result_test_params("File"))

    filtered <- result$filter(function(dt) grepl("2050", dt$title))
    expect_s3_class(filtered, "EsgResultFile")
    expect_identical(filtered$id, result$id[1L])
    expect_identical(filtered$selection()$source_indices, 1L)

    formatted <- result$filter(function(dt) rep(TRUE, nrow(dt)), formatted = TRUE)
    expect_identical(formatted$id, result$id)

    expect_error(result$filter(function(dt) TRUE), "predicate result")
    expect_error(result$filter(function(dt) c(TRUE, NA, FALSE)), "predicate result")
    expect_error(result$filter(function(dt) seq_len(nrow(dt))), "predicate result")
})
# }}}
# EsgResult$slice() / EsgResult$selection() {{{
test_that("EsgResult$slice() / EsgResult$selection() reject invalid selectors", {
    result <- query_result_test_object("File", query_result_test_file_time_docs("File"), query_result_test_params("File"))

    expect_error(result[c(1L, 1L)], "duplicate indices")
    expect_error(result[c(result$id[1L], result$id[1L])], "duplicate record IDs")
    expect_error(result[4L], "between 1 and 3")
    expect_error(result[c(1L, -2L)], "must not mix")
    expect_error(result[0L], "must not contain zero")
    expect_error(result[c(TRUE, FALSE)], "length")
    expect_error(result["missing-id"], "Unknown record ID")
    expect_error(result[1L, ], "one-dimensional")
    expect_error(result[1L, drop = TRUE], "one-dimensional")
})

test_that("EsgResult$slice() / EsgResult$selection() context persists through save/load", {
    result <- query_result_test_object("File", query_result_test_file_time_docs("File"), query_result_test_params("File"))
    priv(result)$response$response$numFound <- 10L
    selected <- result[c(3L, 1L)]
    file <- tempfile(fileext = ".json")

    expect_type(selected$save(file), "character")
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_named(json$context, "selection")
    expect_identical(json$context$selection$source_count, 3L)
    expect_identical(json$context$selection$source_num_found, 10L)
    expect_identical(json$context$selection$source_indices, c(3L, 1L))

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(loaded$id, selected$id)
    expect_identical(loaded$selection(), selected$selection())

    empty <- result$slice(integer())
    empty_file <- tempfile(fileext = ".json")
    expect_type(empty$save(empty_file), "character")
    loaded_empty <- expect_s3_class(esg_result("file")$load(empty_file), "EsgResultFile")
    expect_identical(loaded_empty$selection(), empty$selection())
})
# }}}
# EsgResult$reachable() {{{
test_that("EsgResult$reachable() returns per-record service probe diagnostics", {
    docs <- data.frame(
        id = c("file-ok", "file-dup", "file-missing", "file-fail"),
        dataset_id = "dataset-1",
        size = c(1, 2, 3, 4),
        checksum = c("abc", "def", "ghi", "jkl"),
        checksum_type = "SHA256",
        instance_id = paste0("file-instance-", 1:4),
        master_id = paste0("master-file-", 1:4),
        replica = FALSE,
        tracking_id = paste0("hdl:21.14100/mock-file-", 1:4),
        title = paste0("file-", 1:4, ".nc"),
        version = 20260101L,
        data_node = c("same.example.org", "same.example.org", "missing.example.org", "bad.example.org"),
        check.names = FALSE
    )
    docs$url <- I(list(
        c(
            "https://same.example.org/dods/file.nc|application/netcdf|OPENDAP",
            "https://same.example.org/file.nc|application/netcdf|HTTPServer"
        ),
        "https://same.example.org/dods/file.nc|application/netcdf|OPENDAP",
        character(),
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object("File", docs, query_result_test_params("File"))

    calls <- character()
    timeouts <- numeric()
    agents <- character()
    testthat::local_mocked_bindings(
        query_result__reach_url = function(url, timeout = 5, network_policy = NULL) {
            calls <<- c(calls, url)
            timeouts <<- c(timeouts, timeout)
            useragent <- if (is.null(network_policy$useragent)) NA_character_ else network_policy$useragent
            agents <<- c(agents, useragent)
            if (is.na(url) || !nzchar(url)) {
                return(list(reachable = NA, latency_ms = NA_real_, error = "Missing URL."))
            }
            if (grepl("bad", url)) {
                return(list(reachable = FALSE, latency_ms = NA_real_, error = "boom"))
            }

            list(reachable = TRUE, latency_ms = 125, error = NA_character_)
        },
        .package = "epwshiftr"
    )

    diag <- result$reachable(
        level = "url",
        probe = list(timeout = 9, network_policy = list(useragent = "test-agent"))
    )

    expect_named(diag, c(
        "record_index", "id", "data_node", "service", "url",
        "reachable", "latency_ms", "error", "probe_level",
        "probe_url", "probe_cached"
    ))
    expect_s3_class(diag, "data.table")
    expect_identical(diag$record_index, 1:4)
    expect_identical(diag$id, docs$id)
    expect_identical(diag$data_node, docs$data_node)
    expect_identical(diag$service, rep("OPENDAP", 4L))
    expect_identical(diag$url, c(
        "https://same.example.org/dods/file.nc",
        "https://same.example.org/dods/file.nc",
        NA_character_,
        "https://bad.example.org/dods/file.nc"
    ))
    expect_identical(diag$reachable, c(TRUE, TRUE, NA, FALSE))
    expect_equal(diag$latency_ms, c(125, 125, NA, NA))
    expect_identical(diag$error, c(NA_character_, NA_character_, "Missing URL.", "boom"))
    expect_identical(diag$probe_level, rep("url", 4L))
    expect_identical(diag$probe_url, diag$url)
    expect_false(any(diag$probe_cached))
    expect_equal(sum(calls == "https://same.example.org/dods/file.nc", na.rm = TRUE), 1L)
    expect_equal(sum(calls == "https://bad.example.org/dods/file.nc", na.rm = TRUE), 1L)
    expect_true(any(is.na(calls)))
    expect_true(all(timeouts == 9))
    expect_true(all(agents == "test-agent"))

    selected <- result$slice(diag$reachable %in% TRUE)
    expect_identical(selected$id, c("file-ok", "file-dup"))

    http <- result$reachable(service = "HTTPServer", level = "url")
    expect_identical(http$service, rep("HTTPServer", 4L))
    expect_identical(http$url, c("https://same.example.org/file.nc", rep(NA_character_, 3L)))
    expect_identical(http$reachable, c(TRUE, NA, NA, NA))

    empty <- query_result_test_object("File", docs[0L, , drop = FALSE], query_result_test_params("File"))
    empty_diag <- empty$reachable()
    expect_named(empty_diag, names(diag))
    expect_equal(nrow(empty_diag), 0L)
})

test_that("EsgResult$reachable() probes data node root URLs by default", {
    docs <- data.frame(
        id = c("file-a", "file-b", "file-missing", "file-fallback"),
        dataset_id = "dataset-1",
        size = c(1, 2, 3, 4),
        checksum = c("abc", "def", "ghi", "jkl"),
        checksum_type = "SHA256",
        instance_id = paste0("file-instance-", 1:4),
        master_id = paste0("master-file-", 1:4),
        replica = FALSE,
        tracking_id = paste0("hdl:21.14100/mock-file-", 1:4),
        title = paste0("file-", 1:4, ".nc"),
        version = 20260101L,
        data_node = c("same.example.org", "same.example.org", "missing.example.org", NA_character_),
        check.names = FALSE
    )
    docs$url <- I(list(
        "https://same.example.org/dods/a.nc|application/netcdf|OPENDAP",
        "https://same.example.org/dods/b.nc|application/netcdf|OPENDAP",
        character(),
        "https://fallback.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object("File", docs, query_result_test_params("File"))

    calls <- character()
    testthat::local_mocked_bindings(
        query_result__reach_node_urls = function(urls, timeout = 5,
                                                                       network_policy = NULL,
                                                                       probe_concurrency = 1L) {
            calls <<- c(calls, urls)
            stats::setNames(lapply(urls, function(url) {
                list(
                    reachable = grepl("same|fallback", url),
                    latency_ms = if (grepl("fallback", url)) 44 else 22,
                    error = if (grepl("same|fallback", url)) NA_character_ else "node boom",
                    probe_url = url
                )
            }), urls)
        },
        .package = "epwshiftr"
    )

    diag <- result$reachable(probe = list(cache_seconds = 0L, cache_failures_seconds = 0L))

    expect_identical(diag$url, c(
        "https://same.example.org/dods/a.nc",
        "https://same.example.org/dods/b.nc",
        NA_character_,
        "https://fallback.example.org/dods/file.nc"
    ))
    expect_identical(diag$reachable, c(TRUE, TRUE, NA, TRUE))
    expect_identical(diag$error, c(NA_character_, NA_character_, "Missing URL.", NA_character_))
    expect_identical(diag$probe_level, rep("data_node", 4L))
    expect_identical(diag$probe_url, c(
        "https://same.example.org/",
        "https://same.example.org/",
        NA_character_,
        "https://fallback.example.org/"
    ))
    expect_equal(sum(calls == "https://same.example.org/"), 1L)
    expect_false(any(grepl("/dods/", calls, fixed = TRUE)))
    expect_false(any(grepl("missing.example.org", calls, fixed = TRUE)))
})
# }}}
# EsgResult$repair_urls() {{{
test_that("EsgResult$repair_urls() repairs unreachable OPeNDAP URLs using reachable replicas", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(docs) <- NULL
    docs$id <- c("file-bad", "file-ok")
    docs$dataset_id <- c("dataset-bad|bad.example.org", "dataset-ok|ok.example.org")
    docs$master_id <- c("master-file-bad", "master-file-ok")
    docs$instance_id <- c("instance-bad", "instance-ok")
    docs$data_node <- c("bad.example.org", "ok.example.org")
    docs$url <- I(list(
        c(
            "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
            "https://bad.example.org/file.nc|application/netcdf|HTTPServer"
        ),
        c(
            "https://ok.example.org/dods/file.nc|application/netcdf|OPENDAP",
            "https://ok.example.org/file.nc|application/netcdf|HTTPServer"
        )
    ))
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File"),
        context = list(query_url = "https://origin.example.org/search")
    )

    candidate_docs <- query_result_test_file_docs(c(
        "https://replica.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://replica.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    candidate_docs$id <- "file-repaired"
    candidate_docs$dataset_id <- "dataset-bad|replica.example.org"
    candidate_docs$master_id <- "master-file-bad"
    candidate_docs$instance_id <- "instance-bad"
    candidate_docs$data_node <- "replica.example.org"
    candidate_docs$replica <- TRUE

    probe_calls <- list()
    collect_calls <- list()
    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(data_node, timeout = 5, network_policy = NULL,
                                                           probe_concurrency = 1L,
                                                           cache_seconds = 3600L,
                                                           cache_failures_seconds = 0L) {
            probe_calls[[length(probe_calls) + 1L]] <<- list(
                data_node = data_node,
                timeout = timeout,
                network_policy = network_policy,
                probe_concurrency = probe_concurrency,
                cache_seconds = cache_seconds,
                cache_failures_seconds = cache_failures_seconds
            )
            data.table::data.table(
                data_node = data_node,
                reachable = grepl("ok|replica", data_node),
                latency_ms = ifelse(grepl("replica", data_node), 10, ifelse(grepl("ok", data_node), 50, NA_real_)),
                error = ifelse(grepl("ok|replica", data_node), NA_character_, "boom"),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                 limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            collect_calls[[length(collect_calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints
            )
            expect_identical(index_node, "https://replica-index.example.org")
            expect_identical(query_param__value(params$type()), "File")
            expect_null(params$project())
            expect_identical(query_param__value(params$params()$instance_id), "instance-bad")
            expect_true(all(EsgResultFile$private_fields$required_fields %in% required_fields))
            expect_true(all)
            expect_false(constraints)
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://replica-index.example.org/replicas")
            )
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(
        result$repair_urls(
            index_node = "replica-index.example.org",
            probe = list(timeout = 11, concurrency = 2L, network_policy = list(useragent = "repair-test"))
        ),
        "EsgResultFile"
    )

    expect_identical(result$id, c("file-bad", "file-ok"))
    expect_identical(repaired$id, c("file-repaired", "file-ok"))
    expect_identical(repaired$data_node, c("replica.example.org", "ok.example.org"))
    expect_identical(repaired$dataset_id, c("dataset-bad|replica.example.org", "dataset-ok|ok.example.org"))
    expect_equal(repaired$count(), 2L)
    expect_identical(repaired$selection(), result$selection())
    expect_equal(length(collect_calls), 1L)
    expect_equal(length(probe_calls), 2L)
    expect_true(all(vapply(probe_calls, function(x) x$timeout, numeric(1L)) == 11))
    expect_true(all(vapply(probe_calls, function(x) x$probe_concurrency, integer(1L)) == 2L))
    expect_true(all(vapply(probe_calls, function(x) x$network_policy$useragent, character(1L)) == "repair-test"))
    expect_true(all(vapply(probe_calls, function(x) x$cache_seconds, integer(1L)) == 3600L))
    expect_true(all(vapply(probe_calls, function(x) x$cache_failures_seconds, integer(1L)) == 0L))
    expect_identical(unname(repaired$query_url("all")), c(
        "https://origin.example.org/search",
        "https://replica-index.example.org/replicas"
    ))
})

test_that("EsgResult$repair_urls() prefers reachable replicas already present in the current result", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(docs) <- NULL
    docs$id <- c("file-bad", "file-current-replica")
    docs$dataset_id <- c("dataset-1|bad.example.org", "dataset-1|good.example.org")
    docs$master_id <- c("master-file-current", "master-file-current")
    docs$instance_id <- c("instance-current", "instance-current")
    docs$data_node <- c("bad.example.org", "good.example.org")
    docs$replica <- c(FALSE, TRUE)
    docs$url <- I(list(
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://good.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object("File", docs, query_result_test_params("File"))

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(data_node, timeout = 5, network_policy = NULL,
                                                           probe_concurrency = 1L,
                                                           cache_seconds = 3600L,
                                                           cache_failures_seconds = 0L) {
            data.table::data.table(
                data_node = data_node,
                reachable = data_node == "good.example.org",
                latency_ms = ifelse(data_node == "good.example.org", 7, NA_real_),
                error = ifelse(data_node == "good.example.org", NA_character_, "bad node"),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(...) {
            stop("current result replica should avoid an external query")
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(result$repair_urls(), "EsgResultFile")

    expect_identical(repaired$id, c("file-current-replica", "file-current-replica"))
    expect_identical(repaired$data_node, c("good.example.org", "good.example.org"))
})
# }}}
# EsgResult$expand_replicas() {{{
test_that("EsgResult$expand_replicas() falls back to master and version without crossing versions", {
    docs <- query_result_test_file_docs()
    docs$instance_id <- NA_character_
    docs$master_id <- "master-file-fallback"
    docs$version <- 20260101L
    result <- query_result_test_object("File", docs, query_result_test_params("File"))

    candidate_docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(candidate_docs) <- NULL
    candidate_docs$id <- c("file-same-version", "file-other-version")
    candidate_docs$data_node <- c("same.example.org", "other.example.org")
    candidate_docs$version <- c(20260101L, 20270101L)

    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                 limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            expect_null(params$project())
            expect_identical(query_param__value(params$params()$master_id), "master-file-fallback")
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/master-version")
            )
        },
        .package = "epwshiftr"
    )

    expanded <- expect_s3_class(result$expand_replicas(), "EsgResultFile")
    expect_identical(expanded$id, "file-same-version")
    expect_identical(expanded$version, 20260101L)
})
# }}}
# EsgResult$repair_urls() {{{
test_that("EsgResult$repair_urls() repairs HTTPServer URLs independently", {
    docs <- query_result_test_file_docs(c(
        "https://opendap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://bad-http.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    docs$id <- "file-http-bad"
    docs$dataset_id <- "dataset-http|bad-http.example.org"
    docs$master_id <- "master-file-http"
    docs$data_node <- "bad-http.example.org"
    result <- query_result_test_object("File", docs, query_result_test_params("File"))

    candidate_docs <- query_result_test_file_docs(c(
        "https://opendap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://http-replica.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    candidate_docs$id <- "file-http-repaired"
    candidate_docs$dataset_id <- "dataset-http|http-replica.example.org"
    candidate_docs$master_id <- "master-file-http"
    candidate_docs$data_node <- "http-replica.example.org"

    probed <- character()
    testthat::local_mocked_bindings(
        query_result__reach_urls = function(urls, timeout = 5, network_policy = NULL,
                                                     probe_concurrency = 1L) {
            probed <<- c(probed, urls)
            data.table::data.table(
                url = urls,
                reachable = grepl("http-replica", urls),
                latency_ms = ifelse(grepl("http-replica", urls), 8, NA_real_),
                error = ifelse(grepl("http-replica", urls), NA_character_, "bad http")
            )
        },
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                 limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            expect_identical(index_node, "https://example.org")
            expect_identical(query_param__value(params$type()), "File")
            expect_null(params$project())
            expect_identical(query_param__value(params$params()$instance_id), "file-instance-1")
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/http-replicas")
            )
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(
        result$repair_urls(service = "HTTPServer", probe = list(level = "url")),
        "EsgResultFile"
    )

    expect_identical(repaired$id, "file-http-repaired")
    expect_identical(repaired$data_node, "http-replica.example.org")
    expect_true(any(grepl("bad-http", probed)))
    expect_true(any(grepl("http-replica", probed)))
})

test_that("EsgResult$repair_urls() repairs Aggregation URLs with replica queries", {
    docs <- query_result_test_file_docs("https://bad.example.org/dods/agg.nc|application/netcdf|OPENDAP")
    docs$id <- "aggregation-bad"
    docs$dataset_id <- "dataset-agg|bad.example.org"
    docs$master_id <- "master-aggregation"
    docs$data_node <- "bad.example.org"
    result <- query_result_test_object("Aggregation", docs, query_result_test_params("Aggregation"))

    candidate_docs <- query_result_test_file_docs("https://agg-replica.example.org/dods/agg.nc|application/netcdf|OPENDAP")
    candidate_docs$id <- "aggregation-repaired"
    candidate_docs$dataset_id <- "dataset-agg|agg-replica.example.org"
    candidate_docs$master_id <- "master-aggregation"
    candidate_docs$data_node <- "agg-replica.example.org"

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(data_node, timeout = 5, network_policy = NULL,
                                                           probe_concurrency = 1L,
                                                           cache_seconds = 3600L,
                                                           cache_failures_seconds = 0L) {
            data.table::data.table(
                data_node = data_node,
                reachable = grepl("agg-replica", data_node),
                latency_ms = ifelse(grepl("agg-replica", data_node), 12, NA_real_),
                error = ifelse(grepl("agg-replica", data_node), NA_character_, "bad agg"),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                 limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            expect_identical(query_param__value(params$type()), "Aggregation")
            expect_null(params$project())
            expect_identical(query_param__value(params$params()$instance_id), "file-instance-1")
            expect_true(all(EsgResultAggregation$private_fields$required_fields %in% required_fields))
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/aggregation-replicas")
            )
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(result$repair_urls(), "EsgResultAggregation")

    expect_identical(repaired$id, "aggregation-repaired")
    expect_identical(repaired$data_node, "agg-replica.example.org")
    expect_identical(unname(repaired$query_url("all"))[[2L]], "https://example.org/aggregation-replicas")
})

test_that("EsgResult$repair_urls() keeps original records when repair is impossible", {
    docs <- query_result_test_file_docs("https://missing-master.example.org/dods/file.nc|application/netcdf|OPENDAP")
    docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(docs) <- NULL
    docs$id <- c("file-missing-master", "file-no-replica")
    docs$master_id <- c(NA_character_, "master-no-replica")
    docs$instance_id <- c(NA_character_, NA_character_)
    docs$data_node <- c("missing-master.example.org", "no-replica.example.org")
    docs$url <- I(list(
        "https://missing-master.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://no-replica.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object("File", docs, query_result_test_params("File"))
    candidate_docs <- query_result_test_file_docs("https://still-bad.example.org/dods/file.nc|application/netcdf|OPENDAP")
    candidate_docs$id <- "file-still-bad"
    candidate_docs$master_id <- "master-no-replica"
    candidate_docs$instance_id <- NA_character_
    candidate_docs$data_node <- "still-bad.example.org"

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(data_node, timeout = 5, network_policy = NULL,
                                                           probe_concurrency = 1L,
                                                           cache_seconds = 3600L,
                                                           cache_failures_seconds = 0L) {
            data.table::data.table(
                data_node = data_node,
                reachable = rep(FALSE, length(data_node)),
                latency_ms = rep(NA_real_, length(data_node)),
                error = rep("unreachable", length(data_node)),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                 limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            expect_null(params$project())
            expect_identical(query_param__value(params$params()$master_id), "master-no-replica")
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/no-replica")
            )
        },
        .package = "epwshiftr"
    )

    warnings <- character()
    repaired <- withCallingHandlers(
        result$repair_urls(),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_true(any(grepl("instance_id", warnings)))
    expect_true(any(grepl("No reachable OPENDAP replica", warnings)))
    expect_identical(repaired$id, result$id)
    expect_identical(repaired$data_node, result$data_node)
    expect_identical(unname(repaired$query_url("all"))[[2L]], "https://example.org/no-replica")

    expect_error(result$repair_urls(probe = list(foo = 1)), "Unknown `probe` field")
})
# }}}
# EsgResult$filter_time() {{{
test_that("EsgResult$filter_time() filters File and Aggregation results using DRS filename ranges", {
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
        expect_identical(filtered$selection()$source_indices, c(1L, 3L))

        dt <- filtered$to_data_table(c("id", "datetime_start", "datetime_end"))
        expect_identical(dt$datetime_start[[1L]], "2050-01-01T00:00:00Z")
        expect_identical(dt$datetime_end[[1L]], "2050-12-31T23:59:59Z")
        expect_true(is.na(dt$datetime_start[[2L]]))
        expect_true(is.na(dt$datetime_end[[2L]]))
    }
})

test_that("EsgResult$filter_time() context persists through save/load", {
    result <- query_result_test_object("File", query_result_test_file_time_docs(), query_result_test_params("File"))
    filtered <- suppressWarnings(result$filter_time("2050-06-01", "2050-06-30", method = "drs"))
    file <- tempfile(fileext = ".json")

    expect_type(filtered$save(file), "character")
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_true(all(c("time_filter", "selection") %in% names(json$context)))
    expect_identical(json$context$time_filter$method, "drs")

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(loaded$time_filter, filtered$time_filter)
    expect_identical(loaded$selection(), filtered$selection())
    expect_identical(loaded$id, filtered$id)
})

test_that("EsgResult$filter_time() filters File results using OPeNDAP time axes", {
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
# }}}
# EsgResult$fields {{{
test_that("EsgResult$fields returns stable character vectors for empty results", {
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
# }}}
# EsgResult$to_data_table() {{{
test_that("EsgResult$to_data_table() accepts all advertised fields", {
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
# }}}
# }}}
# EsgResultDataset {{{
# EsgResult$save() / EsgResult$load() {{{
test_that("EsgResult$save() / EsgResult$load() preserve empty child results", {
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
        result <- query_result__new(
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
# }}}
# EsgResultDataset$collect() {{{
test_that("EsgResultDataset$collect() handles empty child results without querying", {
    docs <- data.frame(id = character(), size = numeric(), check.names = FALSE)
    datasets <- query_result_test_object("Dataset", docs)

    testthat::local_mocked_bindings(
        query__collect = function(...) stop("query__collect should not be called"),
        .package = "epwshiftr"
    )

    expect_error(datasets$collect(which = 1L, type = "File"), "empty Dataset result")
    expect_error(datasets$collect(which = "dataset-1", type = "Aggregation"), "empty Dataset result")

    files <- expect_s3_class(datasets$collect(type = "File"), "EsgResultFile")
    expect_equal(files$count(), 0L)
    expect_identical(files$fields, character())
    expect_identical(query_param__value(priv(files)$parameter$type()), "File")

    aggs <- expect_s3_class(datasets$collect(type = "Aggregation"), "EsgResultAggregation")
    expect_equal(aggs$count(), 0L)
    expect_identical(aggs$fields, character())
    expect_identical(query_param__value(priv(aggs)$parameter$type()), "Aggregation")
})

test_that("EsgResultDataset$collect() inherits controls and normalizes limit", {
    params <- query_result_test_params("Dataset", latest = FALSE, distrib = FALSE, replica = FALSE)
    params$source_id("AWI-CM-1-1-MR")
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = "dataset-1", size = 1, check.names = FALSE),
        params
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints
            )
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(datasets$collect(fields = "id", limit = NULL), "EsgResultFile")
    expect_identical(calls[[1L]]$index_node, "https://example.org")
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param__value(calls[[1L]]$params$latest()))
    expect_false(query_param__value(calls[[1L]]$params$distrib()))
    expect_false(query_param__value(calls[[1L]]$params$replica()))
    expect_null(calls[[1L]]$params$project())
    expect_null(calls[[1L]]$params$source_id())
    expect_identical(query_param__value(calls[[1L]]$params$state()$dataset_id), "dataset-1")
    expect_true(all(EsgResultFile$private_fields$required_fields %in% query_param__value(priv(files)$parameter$fields())))

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, latest = TRUE, distrib = TRUE, replica = TRUE),
        "EsgResultFile"
    )
    expect_equal(calls[[2L]]$limit, 1L)
    expect_true(query_param__value(calls[[2L]]$params$latest()))
    expect_true(query_param__value(calls[[2L]]$params$distrib()))
    expect_true(query_param__value(calls[[2L]]$params$replica()))

    aggs <- expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, type = "Aggregation", index_node = "esg-dn1.nsc.liu.se"),
        "EsgResultAggregation"
    )
    expect_identical(calls[[3L]]$index_node, "https://esg-dn1.nsc.liu.se")
    expect_identical(priv(aggs)$index_node, "https://esg-dn1.nsc.liu.se")
    expect_equal(calls[[3L]]$limit, 1L)
    expect_false(query_param__value(calls[[3L]]$params$latest()))
    expect_false(query_param__value(calls[[3L]]$params$distrib()))
    expect_false(query_param__value(calls[[3L]]$params$replica()))
    expect_null(calls[[3L]]$params$project())
    expect_null(calls[[3L]]$params$source_id())
    expect_identical(query_param__value(calls[[3L]]$params$state()$dataset_id), "dataset-1")
    expect_true(all(EsgResultAggregation$private_fields$required_fields %in% query_param__value(priv(aggs)$parameter$fields())))
})

test_that("EsgResultDataset$collect() accepts data node scope and clears datetime constraints", {
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
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            calls[[length(calls) + 1L]] <<- list(params = params, limit = limit)
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, data_node = "example.org"),
        "EsgResultFile"
    )
    expect_identical(query_param__value(calls[[1L]]$params$data_node()), "example.org")
    expect_identical(calls[[1L]]$params$render(c("datetime_start", "datetime_stop")), character())

    expect_error(datasets$collect(source_id = "AWI-CM-1-1-MR"), "unsupported parameter")
    expect_error(datasets$collect(bbox = "0,0,1,1"), "unsupported parameter")
    expect_error(datasets$collect(start = "2050"), "unsupported parameter")
    expect_error(datasets$collect(datetime_start = "2050"), "controlled")
    expect_error(datasets$collect(time = "all"), "controlled")
})

test_that("EsgResultDataset$collect() ignores record index node metadata", {
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = c("dataset-1", "dataset-2", "dataset-3"),
            index_node = c("us-index", NA, "https://idx-b.example.org"),
            size = c(1, 1, 1),
            check.names = FALSE
        ),
        query_result_test_params("Dataset")
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            dataset_id <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(index_node = index_node, dataset_id = dataset_id)
            docs <- data.frame(
                id = paste0("file-", dataset_id),
                dataset_id = dataset_id,
                size = seq_along(dataset_id),
                url = I(rep(list("https://example.org/file.nc|application/netcdf|HTTPServer"), length(dataset_id))),
                check.names = FALSE
            )
            response <- query_result_test_response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params,
                context = list(query_url = paste0(index_node, "/search?", paste(dataset_id, collapse = ",")))
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, index_node = "fallback.example.org"),
        "EsgResultFile"
    )
    expect_identical(
        vapply(calls, `[[`, character(1L), "index_node"),
        "https://fallback.example.org"
    )
    expect_length(calls, 1L)
    expect_identical(unlist(lapply(calls, `[[`, "dataset_id"), use.names = FALSE), datasets$id)
    expect_identical(files$count(), 3L)
    expect_length(priv(files)$context$query_url, 1L)
})
# }}}
# EsgResultDataset$expand_replicas() {{{
test_that("EsgResultDataset$expand_replicas() queries dataset replicas by identity", {
    docs <- data.frame(
        id = "dataset-1|node-a.example.org",
        instance_id = "dataset-instance-1",
        master_id = "dataset-master-1",
        version = 20260101L,
        data_node = "node-a.example.org",
        size = 1,
        check.names = FALSE
    )
    datasets <- query_result_test_object("Dataset", docs, query_result_test_params("Dataset"))

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            calls[[length(calls) + 1L]] <<- list(index_node = index_node, params = params, required_fields = required_fields)
            if (!is.null(params$params()$instance_id)) {
                expect_identical(query_param__value(params$params()$instance_id), "dataset-instance-1")
                out <- docs[rep(1L, 2L), , drop = FALSE]
                out$id <- c("dataset-1|node-a.example.org", "dataset-1|node-b.example.org")
                out$data_node <- c("node-a.example.org", "node-b.example.org")
            } else {
                expect_identical(query_param__value(params$params()$master_id), "dataset-master-1")
                out <- docs[rep(1L, 2L), , drop = FALSE]
                out$id <- c("dataset-1.v20260101|node-a.example.org", "dataset-1.v20270101|node-a.example.org")
                out$version <- c(20260101L, 20270101L)
            }
            expect_identical(index_node, "https://replica-index.example.org")
            expect_null(params$project())
            expect_identical(query_param__value(params$type()), "Dataset")
            expect_true(all(EsgResultDataset$private_fields$required_fields %in% required_fields))
            list(
                response = query_result_test_response(out),
                docs = out,
                parameter = query_param__clone(params),
                context = list(query_url = paste0(index_node, "/dataset-replicas"))
            )
        },
        .package = "epwshiftr"
    )

    same_version <- expect_s3_class(datasets$expand_replicas(index_node = "replica-index.example.org"), "EsgResultDataset")
    expect_identical(same_version$data_node, c("node-a.example.org", "node-b.example.org"))

    logical_dataset <- expect_s3_class(
        datasets$expand_replicas(by = "master_id", index_node = "replica-index.example.org"),
        "EsgResultDataset"
    )
    expect_identical(logical_dataset$version, c(20260101L, 20270101L))
    expect_length(calls, 2L)
})
# }}}
# EsgResultDataset$has_opendap() / EsgResultDataset$has_download() {{{
test_that("EsgResultDataset$has_opendap() / EsgResultDataset$has_download() tolerate missing access fields", {
    datasets <- query_result_test_object("Dataset", query_result_test_dataset_docs(access = FALSE))
    expect_identical(datasets$has_opendap(), c(FALSE, FALSE))
    expect_identical(datasets$has_download(), c(FALSE, FALSE))
})
# }}}
# }}}
# EsgResultFile {{{
# EsgResultFile$url_opendap / EsgResultFile$url_download {{{
test_that("EsgResultFile$url_opendap / EsgResultFile$url_download preserve result length for missing and malformed URLs", {
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

test_that("EsgResultFile$url_opendap warns with robust nested or missing field context", {
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
# }}}
# EsgResultFile$download_plan() {{{
test_that("EsgResultFile$download_plan() builds current HTTPServer plans with logical file identity", {
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

test_that("EsgResultFile$download_plan() deduplicates URL probes", {
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
        query_result__latency_url = function(url, timeout = 5, network_policy = NULL) {
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

test_that("EsgResultFile$download_plan() reuses fresh data node probe cache", {
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
        query_result__latency_url = function(...) {
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

test_that("EsgResultFile$download_plan() uses data node history to rank replica candidates", {
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

test_that("EsgResultFile$download_plan() ranks cooling data nodes after available candidates", {
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
# }}}
# }}}
# EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() {{{
test_that("EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() validates fallback before side effects", {
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

test_that("EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() falls back to HTTP after OPeNDAP open failures", {
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
            opened = FALSE,
            context = list()
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
    expect_identical(ds_all$.__enclos_env__$private$context$selection$source_indices, c(1L, 2L))
    expect_equal(length(calls$opened) - opened_before, 2L)

    ds_one <- expect_s3_class(
        multi_files$open_dataset(which = "file-2", fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(ds_one$target, "https://example.org/dods/file-2.nc")
    expect_identical(ds_one$.__enclos_env__$private$context$selection$source_indices, 2L)

    ds_selected <- expect_s3_class(
        multi_files$open_dataset(which = 1:2, fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(ds_selected$target, c(
        "https://example.org/dods/file-1.nc",
        "https://example.org/dods/file-2.nc"
    ))
    expect_identical(ds_selected$.__enclos_env__$private$context$selection$source_indices, c(1L, 2L))
    expect_error(multi_files$open_dataset(aggregate = FALSE), "unused argument")

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
    expect_identical(agg_ds$.__enclos_env__$private$context$selection$source_indices, c(1L, 2L))
    expect_identical(tail(calls$downloads, length(calls$downloads) - length(downloads_before)), "https://example.org/file-2.nc")
    new_open_calls <- calls$opened[(opened_before + 1L):length(calls$opened)]
    expect_identical(new_open_calls[[1L]], "https://example.org/dods/file-1.nc")
    expect_identical(new_open_calls[[2L]], agg_ds$target[[2L]])

    agg_one_index <- expect_s3_class(
        agg_result$open_dataset(which = 1L, fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(agg_one_index$target, "https://example.org/dods/file-1.nc")
    expect_identical(agg_one_index$.__enclos_env__$private$context$selection$source_indices, 1L)

    agg_one_id <- expect_s3_class(
        agg_result$open_dataset(which = "file-1", fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(agg_one_id$target, "https://example.org/dods/file-1.nc")
    expect_identical(agg_one_id$.__enclos_env__$private$context$selection$source_indices, 1L)
    expect_error(agg_result$open_dataset(aggregate = FALSE), "unused argument")

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
# }}}
# EsgResultDataset$to_data_table() {{{
test_that("EsgResultDataset$to_data_table() supports offline fixtures", {
    params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    datasets <- expect_s3_class(
        query_result_test_object("Dataset", query_result_test_contract_dataset_docs(), params),
        "EsgResultDataset"
    )

    expect_s3_class(datasets$to_data_table(), "data.table")
    expect_s3_class(datasets$to_data_table(c("source_id", "frequency")), "data.table")
    expect_equal(names(datasets$to_data_table(c("source_id", "frequency"))), c("source_id", "frequency"))
    expect_s3_class(datasets$to_data_table(formatted = TRUE)$size, "units")
})
# }}}
# EsgResultDataset$fields / EsgResultDataset$id / EsgResultDataset$url / EsgResultDataset$size / EsgResultDataset$index_node / EsgResultDataset$access / EsgResultDataset$number_of_files / EsgResultDataset$has_opendap() / EsgResultDataset$has_download() / EsgResultDataset$count() {{{
test_that("EsgResultDataset$fields / EsgResultDataset$id / EsgResultDataset$url / EsgResultDataset$size / EsgResultDataset$index_node / EsgResultDataset$access / EsgResultDataset$number_of_files / EsgResultDataset$has_opendap() / EsgResultDataset$has_download() / EsgResultDataset$count() expose offline fixture fields", {
    params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    datasets <- expect_s3_class(
        query_result_test_object("Dataset", query_result_test_contract_dataset_docs(), params),
        "EsgResultDataset"
    )

    expect_type(datasets$has_opendap(), "logical")
    expect_length(datasets$has_opendap(), 2L)

    expect_type(datasets$has_download(), "logical")
    expect_length(datasets$has_download(), 2L)

    expect_equal(datasets$count(), 2L)

    expect_equal(
        sort(datasets$fields),
        c(
            "access",
            "activity_id",
            "data_node",
            "experiment_id",
            "frequency",
            "id",
            "index_node",
            "instance_id",
            "latest",
            "master_id",
            "number_of_files",
            "project",
            "replica",
            "size",
            "source_id",
            "variable_id",
            "variant_label",
            "version"
        )
    )

    expect_type(datasets$id, "character")
    expect_length(datasets$id, 2L)

    expect_null(datasets$url)

    expect_s3_class(datasets$size, "units")
    expect_length(datasets$size, 2L)

    expect_type(datasets$index_node, "character")
    expect_length(datasets$index_node, 2L)

    expect_type(datasets$access, "list")
    expect_length(datasets$access, 2L)
    expect_type(datasets$access[[1]], "character")

    expect_type(datasets$number_of_files, "integer")
    expect_length(datasets$number_of_files, 2L)
})
# }}}
# EsgResultDataset$save() / EsgResultDataset$load() {{{
test_that("EsgResultDataset$save() / EsgResultDataset$load() round-trip offline fixtures", {
    params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    datasets <- expect_s3_class(
        query_result_test_object("Dataset", query_result_test_contract_dataset_docs(), params),
        "EsgResultDataset"
    )

    file <- tempfile(fileext = ".json")
    expect_type(datasets$save(file), "character")
    expect_true(file.exists(file))
    file_copied <- tempfile(fileext = ".json")
    expect_true(file.copy(file, file_copied))
    expect_snapshot_file(file_copied, "dataset.json", transform = transform_json)

    de <- expect_s3_class(query_result__new(EsgResultDataset)$load(file), "EsgResultDataset")
    expect_equal(priv(de)$index_node, priv(datasets)$index_node)
    expect_equal(priv(de)$parameter, priv(datasets)$parameter)
    # manually add the cache key since '$save()' will exclude it
    priv(de)$response$cache <- priv(datasets)$response$cache
    # keep the column order the same since '$save()' then '$load()' may change
    # the order
    priv(de)$response$response$docs <- priv(de)$response$response$docs[,
        names(priv(datasets)$response$response$docs)
    ]
    expect_equal(priv(de)$response, priv(datasets)$response, ignore_attr = TRUE)
})
# }}}
# EsgResultDataset$collect() {{{
test_that("EsgResultDataset$collect() uses offline child fixtures", {
    params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    datasets <- expect_s3_class(
        query_result_test_object("Dataset", query_result_test_contract_dataset_docs(), params),
        "EsgResultDataset"
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            docs <- switch(
                type,
                File = query_result_test_contract_file_docs(),
                Aggregation = query_result_test_contract_aggregation_docs()
            )
            dataset_id <- query_param__value(params$state()$dataset_id)
            if (!is.null(dataset_id)) {
                docs <- docs[docs$dataset_id %in% dataset_id, , drop = FALSE]
            }
            if (!isTRUE(all) && is.numeric(limit) && length(limit) == 1L) {
                docs <- docs[seq_len(min(nrow(docs), limit)), , drop = FALSE]
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            fields <- unique(c(fields, required_fields))
            params$fields(fields)
            response <- query_result_test_response(docs)
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                type = type,
                all = all,
                limit = limit,
                constraints = constraints,
                dict_check = dict_check
            )
            list(response = response, docs = docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    expect_s3_class(files_by_index <- datasets$collect(1, limit = 2, fields = "id"), "EsgResultFile")
    expect_equal(files_by_index$count(), 2L)

    expect_s3_class(files_by_id <- datasets$collect(datasets$id[1], fields = "id"), "EsgResultFile")
    expect_equal(files_by_id$count(), 2L)

    expect_s3_class(files <- datasets$collect(fields = "id", limit = 1), "EsgResultFile")
    expect_true(
        all(c(EsgResultFile$private_fields$required_fields, "filename", "url_opendap", "url_download") %in% files$fields)
    )

    expect_s3_class(files <- datasets$collect(fields = "id", all = TRUE), "EsgResultFile")
    expect_equal(files$count(), sum(datasets$number_of_files))
    expect_true(calls[[4L]]$all)

    expect_s3_class(datasets$collect(fields = "id", limit = 1, replica = FALSE), "EsgResultFile")

    expect_s3_class(files <- datasets$collect(limit = 1), "EsgResultFile")
    expect_true(all(EsgResultFile$private_fields$required_fields %in% files$fields))

    dataset_data_node <- datasets$data_node[[1]]
    expect_s3_class(datasets$collect(fields = "id", limit = 1, data_node = dataset_data_node), "EsgResultFile")

    expect_error(datasets$collect(bbox = "0,0,1,1"), "unsupported parameter")
    expect_error(datasets$collect(start = "2050"), "unsupported parameter")
    expect_error(datasets$collect(datetime_start = "2050"), "controlled")

    expect_s3_class(aggs <- datasets$collect(fields = "id", limit = 2, type = "Aggregation"), "EsgResultAggregation")
    expect_equal(aggs$count(), 2L)
    expect_true(all(EsgResultAggregation$private_fields$required_fields %in% aggs$fields))
})
# }}}
# EsgResultDataset$print() {{{
test_that("EsgResultDataset$print() snapshots offline fixtures", {
    params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    datasets <- expect_s3_class(
        query_result_test_object("Dataset", query_result_test_contract_dataset_docs(), params),
        "EsgResultDataset"
    )

    expect_snapshot(datasets$print(), transform = transform_print)
})
# }}}
# EsgResultFile$to_data_table() {{{
test_that("EsgResultFile$to_data_table() supports offline fixtures", {
    params <- query_result_test_params(
        "File",
        fields = c("source_id", "experiment_id", "frequency"),
        dataset_id = query_result_test_contract_dataset_docs()$id[[1L]],
        limit = 1L
    )
    files <- expect_s3_class(
        query_result_test_object("File", query_result_test_contract_file_docs()[1L, , drop = FALSE], params),
        "EsgResultFile"
    )

    expect_s3_class(files$to_data_table(), "data.table")
    expect_s3_class(files$to_data_table(c("checksum", "checksum_type")), "data.table")
    expect_equal(names(files$to_data_table(c("checksum", "checksum_type"))), c("checksum", "checksum_type"))
    expect_s3_class(files$to_data_table(formatted = TRUE)$size, "units")
    expect_s3_class(files$to_data_table(formatted = TRUE)$url[[1L]], "data.table")
})
# }}}
# EsgResultFile$fields / EsgResultFile$id / EsgResultFile$url / EsgResultFile$size / EsgResultFile$dataset_id / EsgResultFile$checksum / EsgResultFile$checksum_type / EsgResultFile$data_node / EsgResultFile$filename / EsgResultFile$tracking_id / EsgResultFile$url_opendap / EsgResultFile$url_download {{{
test_that("EsgResultFile$fields / EsgResultFile$id / EsgResultFile$url / EsgResultFile$size / EsgResultFile$dataset_id / EsgResultFile$checksum / EsgResultFile$checksum_type / EsgResultFile$data_node / EsgResultFile$filename / EsgResultFile$tracking_id / EsgResultFile$url_opendap / EsgResultFile$url_download expose offline fixture fields", {
    params <- query_result_test_params(
        "File",
        fields = c("source_id", "experiment_id", "frequency"),
        dataset_id = query_result_test_contract_dataset_docs()$id[[1L]],
        limit = 1L
    )
    files <- expect_s3_class(
        query_result_test_object("File", query_result_test_contract_file_docs()[1L, , drop = FALSE], params),
        "EsgResultFile"
    )

    expect_type(files$id, "character")
    expect_length(files$id, 1L)

    expect_type(files$url, "list")
    expect_length(files$url, 1L)
    expect_s3_class(files$url[[1L]], "data.table")

    expect_s3_class(files$size, "units")
    expect_length(files$size, 1L)

    expect_type(files$dataset_id, "character")
    expect_length(files$dataset_id, 1L)

    expect_type(files$checksum, "character")
    expect_length(files$checksum, 1L)

    expect_type(files$checksum_type, "character")
    expect_length(files$checksum_type, 1L)

    expect_type(files$data_node, "character")
    expect_length(files$data_node, 1L)

    expect_type(files$filename, "character")
    expect_length(files$filename, 1L)

    expect_type(files$tracking_id, "character")
    expect_length(files$tracking_id, 1L)

    expect_type(files$url_opendap, "character")
    expect_length(files$url_opendap, 1L)

    expect_type(files$url_download, "character")
    expect_length(files$url_download, 1L)

    expect_true(
        all(EsgResultFile$private_fields$required_fields %in% files$fields)
    )
})
# }}}
# EsgResultFile$print() {{{
test_that("EsgResultFile$print() snapshots offline fixtures", {
    params <- query_result_test_params(
        "File",
        fields = c("source_id", "experiment_id", "frequency"),
        dataset_id = query_result_test_contract_dataset_docs()$id[[1L]],
        limit = 1L
    )
    files <- expect_s3_class(
        query_result_test_object("File", query_result_test_contract_file_docs()[1L, , drop = FALSE], params),
        "EsgResultFile"
    )

    expect_snapshot(files$print(), transform = transform_print)
})
# }}}
# EsgResultAggregation$to_data_table() {{{
test_that("EsgResultAggregation$to_data_table() supports offline fixtures", {
    params <- query_result_test_params(
        "Aggregation",
        fields = "id",
        dataset_id = query_result_test_contract_dataset_docs()$id,
        limit = 2L
    )
    aggs <- expect_s3_class(
        query_result_test_object("Aggregation", query_result_test_contract_aggregation_docs(), params),
        "EsgResultAggregation"
    )

    expect_s3_class(aggs$to_data_table(), "data.table")
    expect_s3_class(aggs$to_data_table(c("url", "size")), "data.table")
    expect_equal(names(aggs$to_data_table(c("url", "size"))), c("url", "size"))
    expect_s3_class(aggs$to_data_table(formatted = TRUE)$size, "units")
    expect_s3_class(aggs$to_data_table(formatted = TRUE)$url[[1L]], "data.table")
})
# }}}
# EsgResultAggregation$fields / EsgResultAggregation$id / EsgResultAggregation$url / EsgResultAggregation$size / EsgResultAggregation$dataset_id / EsgResultAggregation$data_node / EsgResultAggregation$url_opendap / EsgResultAggregation$url_download {{{
test_that("EsgResultAggregation$fields / EsgResultAggregation$id / EsgResultAggregation$url / EsgResultAggregation$size / EsgResultAggregation$dataset_id / EsgResultAggregation$data_node / EsgResultAggregation$url_opendap / EsgResultAggregation$url_download expose offline fixture fields", {
    params <- query_result_test_params(
        "Aggregation",
        fields = "id",
        dataset_id = query_result_test_contract_dataset_docs()$id,
        limit = 2L
    )
    aggs <- expect_s3_class(
        query_result_test_object("Aggregation", query_result_test_contract_aggregation_docs(), params),
        "EsgResultAggregation"
    )

    expect_type(aggs$id, "character")
    expect_length(aggs$id, 2L)

    expect_type(aggs$url, "list")
    expect_length(aggs$url, 2L)
    expect_s3_class(aggs$url[[1L]], "data.table")

    expect_s3_class(aggs$size, "units")
    expect_length(aggs$size, 2L)

    expect_type(aggs$dataset_id, "character")
    expect_length(aggs$dataset_id, 2L)

    expect_type(aggs$data_node, "character")
    expect_length(aggs$data_node, 2L)

    expect_type(aggs$url_opendap, "character")
    expect_length(aggs$url_opendap, 2L)

    expect_type(aggs$url_download, "character")
    expect_length(aggs$url_download, 2L)

    expect_true(
        all(c("data_node", "dataset_id", "id", "size", "title", "url", "url_opendap", "url_download") %in% aggs$fields)
    )
})
# }}}
# EsgResultAggregation$print() {{{
test_that("EsgResultAggregation$print() snapshots offline fixtures", {
    params <- query_result_test_params(
        "Aggregation",
        fields = "id",
        dataset_id = query_result_test_contract_dataset_docs()$id,
        limit = 2L
    )
    aggs <- expect_s3_class(
        query_result_test_object("Aggregation", query_result_test_contract_aggregation_docs(), params),
        "EsgResultAggregation"
    )

    expect_snapshot(aggs$print(), transform = transform_print)
})
# }}}
# esg_result() {{{
test_that("esg_result() constructs typed empty query results", {
    expect_s3_class(esg_result(), "EsgResultDataset")
    expect_s3_class(esg_result("file"), "EsgResultFile")
    expect_s3_class(esg_result(), "EsgResultDataset")

    expect_snapshot(esg_result("file")$print(), transform = transform_print)
    expect_snapshot(esg_result("aggregation")$print(), transform = transform_print)
    expect_snapshot(esg_result("aggregation")$print(), transform = transform_print)
})
# }}}
# vim: fdm=marker :
