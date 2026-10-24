# EsgResult {{{
local_test_cache(scope = "persist")
withr::local_options(list(epwshiftr.progress = FALSE))

# query_result_test_response {{{
query_result_test_response <- function(docs) {
    esgf_test__response(docs, timestamp = Sys.time())
}
# }}}

# query_result_test_params {{{
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
# }}}

# query_result_test_object {{{
query_result_test_object <- function(
    type = "Dataset",
    docs,
    params = query_result_test_params(type),
    context = NULL
) {
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
# }}}

# query_result_test_state {{{
query_result_test_state <- function(
    type = "Dataset",
    docs,
    params = query_result_test_params(type),
    context = NULL
) {
    list(
        index_node = "https://example.org",
        parameter = params,
        response = query_result_test_response(docs),
        context = context
    )
}
# }}}

# query_result_test_dataset_docs {{{
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
# }}}

# query_result_test_file_docs {{{
query_result_test_file_docs <- function(
    url = "https://example.org/file.nc|application/netcdf|HTTPServer"
) {
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
# }}}

# query_result_test_contract_dataset_docs {{{
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
# }}}

# query_result_test_contract_file_docs {{{
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
        instance_id = c(
            "file-instance-1",
            "file-instance-2",
            "file-instance-3"
        ),
        master_id = c("file-master-1", "file-master-2", "file-master-3"),
        replica = c(FALSE, FALSE, FALSE),
        tracking_id = c(
            "hdl:21.14100/mock-1",
            "hdl:21.14100/mock-2",
            "hdl:21.14100/mock-3"
        ),
        title = c(
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20500101-20501231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20510101-20511231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp245_r1i1p1f1_gn_20500101-20501231.nc"
        ),
        data_node = c(
            "esgf.example.org",
            "esgf.example.org",
            "esgf.example.org"
        ),
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
# }}}

# query_result_test_contract_aggregation_docs {{{
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
# }}}

# query_result_test_file_time_docs {{{
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
        instance_id = c(
            "file-instance-2050",
            "file-instance-2080",
            "file-instance-unknown"
        ),
        master_id = c(
            "master-file-2050",
            "master-file-2080",
            "master-file-unknown"
        ),
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
# }}}
# EsgResult$load() / EsgResult$save() {{{
test_that("EsgResult$load() restores dynamic fields", {
    state <- query_result_test_state(
        "Dataset",
        query_result_test_dataset_docs()
    )
    testthat::local_mocked_bindings(
        query__load = function(file, schema = NULL) state,
        .package = "epwshiftr"
    )

    loaded <- expect_s3_class(
        esg_result("dataset")$load("dataset.json"),
        "EsgResultDataset"
    )
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
    state <- query_result_test_state(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File")
    )
    testthat::local_mocked_bindings(
        query__load = function(file, schema = NULL) state,
        .package = "epwshiftr"
    )

    expect_error(
        esg_result("dataset")$load("file.json"),
        "Cannot load 'File' result"
    )
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
        result <- query_result_test_object(
            case$type,
            case$docs,
            query_result_test_params(case$type)
        )
        file <- tempfile(fileext = ".json")

        expect_type(result$save(file), "character")
        json <- jsonlite::fromJSON(
            file,
            simplifyVector = TRUE,
            simplifyMatrix = FALSE
        )
        expect_true(all(
            c("project", "fields", "type", "limit", "format") %in%
                names(json$parameter)
        ))

        loaded <- expect_s3_class(
            esg_result(case_name)$load(file),
            class(result)[[1L]]
        )
        expect_identical(loaded$id, result$id)
        expect_identical(
            priv(loaded)$parameter$serialize(null = TRUE),
            priv(result)$parameter$serialize(null = TRUE)
        )
        expect_identical(
            names(loaded$to_data_table()),
            names(result$to_data_table())
        )
        expect_error(loaded$to_data_table(character()), "Must have length")

        unlink(file)
    }
})

test_that("EsgResult$save() / EsgResult$load() preserve ESGF doc timestamp fields", {
    docs <- query_result_test_file_docs()
    docs$timestamp <- "2026-06-09T00:00:00Z"
    docs$`_timestamp` <- "2026-06-10T01:14:40.946Z"
    docs$version <- "v20240509"
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    file <- tempfile(fileext = ".json")

    expect_type(result$save(file), "character")
    json <- jsonlite::fromJSON(
        file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    expect_true("timestamp" %in% names(json$response$response$docs))
    expect_true("_timestamp" %in% names(json$response$response$docs))
    expect_identical(
        json$response$response$docs$timestamp,
        "2026-06-09T00:00:00Z"
    )
    expect_identical(
        json$response$response$docs$`_timestamp`,
        "2026-06-10T01:14:40.946Z"
    )

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_true("timestamp" %in% loaded$fields)
    expect_true("_timestamp" %in% loaded$fields)
    expect_identical(loaded$timestamp, "2026-06-09T00:00:00Z")
    expect_identical(loaded$`_timestamp`, "2026-06-10T01:14:40.946Z")
    expect_identical(loaded$version, "v20240509")
})

test_that("EsgResult$save() / EsgResult$load() preserve File publish_path", {
    docs <- query_result_test_file_docs()
    docs$publish_path <- "/css03_data/CMIP6/ScenarioMIP/mock/file.nc"
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    file <- tempfile(fileext = ".json")

    expect_type(result$save(file), "character")
    json <- jsonlite::fromJSON(
        file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    expect_true("publish_path" %in% names(json$response$response$docs))
    expect_identical(
        json$response$response$docs$publish_path,
        docs$publish_path
    )

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_true("publish_path" %in% loaded$fields)
    expect_identical(loaded$publish_path, docs$publish_path)
})
# }}}
# EsgResult$query_url() {{{
test_that("EsgResult$query_url() exposes recorded query URLs", {
    params <- query_result_test_params("Dataset")
    result <- query_result_test_object(
        "Dataset",
        query_result_test_dataset_docs(),
        params
    )
    expect_identical(
        result$query_url(),
        stats::setNames(query__build("https://example.org", params), "page1")
    )
    expect_identical(result$query_url("all"), result$query_url())

    urls <- c(
        "https://example.org/search?page=1",
        "https://example.org/search?page=2"
    )
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File"),
        context = list(query_url = urls)
    )
    expect_identical(result$query_url(), stats::setNames(urls[[1L]], "page1"))
    expect_identical(
        result$query_url("all"),
        stats::setNames(urls, c("page1", "page2"))
    )
    expect_error(result$query_url("last"), "'arg' should be one of")
})

test_that("EsgResult$query_url() context persists through save/load", {
    urls <- c(
        "https://example.org/search?page=1",
        "https://example.org/search?page=2"
    )
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File"),
        context = list(query_url = urls)
    )
    file <- tempfile(fileext = ".json")

    expect_type(result$save(file), "character")
    json <- jsonlite::fromJSON(
        file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    expect_identical(json$context$query_url, urls)

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(
        loaded$query_url("all"),
        stats::setNames(urls, c("page1", "page2"))
    )
})
# }}}
# EsgResult$slice() / EsgResult$selection() {{{
test_that("EsgResult$slice() / EsgResult$selection() support local selection", {
    urls <- c(
        "https://example.org/search?page=1",
        "https://example.org/search?page=2"
    )
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
        expect_identical(
            result$selection(),
            list(
                source_count = as.integer(nrow(cases[[type]])),
                source_num_found = 10L,
                source_indices = seq_len(nrow(cases[[type]]))
            )
        )

        selected <- result[c(nrow(cases[[type]]), 1L)]
        expect_s3_class(selected, class(result)[[1L]])
        expect_identical(selected$id, result$id[c(nrow(cases[[type]]), 1L)])
        expect_identical(
            selected$query_url("all"),
            stats::setNames(urls, c("page1", "page2"))
        )
        expect_identical(selected$time_filter, time_filter)
        expect_identical(
            selected$selection(),
            list(
                source_count = as.integer(nrow(cases[[type]])),
                source_num_found = 10L,
                source_indices = as.integer(c(nrow(cases[[type]]), 1L))
            )
        )

        chained <- selected[2L]
        expect_identical(chained$id, result$id[1L])
        expect_identical(chained$selection()$source_indices, 1L)

        logical_selected <- result[seq_len(nrow(cases[[type]])) %% 2L == 1L]
        expect_identical(
            logical_selected$id,
            result$id[seq_len(nrow(cases[[type]])) %% 2L == 1L]
        )

        character_selected <- result[result$id[1L]]
        expect_identical(character_selected$id, result$id[1L])

        empty <- result$slice(NULL)
        expect_s3_class(empty, class(result)[[1L]])
        expect_identical(empty$id, character())
        expect_identical(
            empty$selection(),
            list(
                source_count = as.integer(nrow(cases[[type]])),
                source_num_found = 10L,
                source_indices = integer()
            )
        )

        expect_identical(result[-1L]$id, result$id[-1L])
    }
})
# }}}
# EsgResult$filter() {{{
test_that("EsgResult$filter() filters with predicates", {
    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs("File"),
        query_result_test_params("File")
    )

    filtered <- result$filter(function(dt) grepl("2050", dt$title))
    expect_s3_class(filtered, "EsgResultFile")
    expect_identical(filtered$id, result$id[1L])
    expect_identical(filtered$selection()$source_indices, 1L)

    formatted <- result$filter(
        function(dt) rep(TRUE, nrow(dt)),
        formatted = TRUE
    )
    expect_identical(formatted$id, result$id)

    expect_error(result$filter(function(dt) TRUE), "predicate result")
    expect_error(
        result$filter(function(dt) c(TRUE, NA, FALSE)),
        "predicate result"
    )
    expect_error(
        result$filter(function(dt) seq_len(nrow(dt))),
        "predicate result"
    )
})
# }}}
# EsgResult$slice() / EsgResult$selection() {{{
test_that("EsgResult$slice() / EsgResult$selection() reject invalid selectors", {
    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs("File"),
        query_result_test_params("File")
    )

    expect_error(result[c(1L, 1L)], "duplicate indices")
    expect_error(
        result[c(result$id[1L], result$id[1L])],
        "duplicate record IDs"
    )
    expect_error(result[4L], "between 1 and 3")
    expect_error(result[c(1L, -2L)], "must not mix")
    expect_error(result[0L], "must not contain zero")
    expect_error(result[c(TRUE, FALSE)], "length")
    expect_error(result["missing-id"], "Unknown record ID")
    expect_error(result[1L, ], "one-dimensional")
    expect_error(result[1L, drop = TRUE], "one-dimensional")
})

test_that("EsgResult$slice() / EsgResult$selection() context persists through save/load", {
    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs("File"),
        query_result_test_params("File")
    )
    priv(result)$response$response$numFound <- 10L
    selected <- result[c(3L, 1L)]
    file <- tempfile(fileext = ".json")

    expect_type(selected$save(file), "character")
    json <- jsonlite::fromJSON(
        file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    expect_named(json$context, c("directory_collection", "selection"))
    expect_false(json$context$directory_collection)
    expect_identical(json$context$selection$source_count, 3L)
    expect_identical(json$context$selection$source_num_found, 10L)
    expect_identical(json$context$selection$source_indices, c(3L, 1L))

    loaded <- expect_s3_class(esg_result("file")$load(file), "EsgResultFile")
    expect_identical(loaded$id, selected$id)
    expect_identical(loaded$selection(), selected$selection())

    empty <- result$slice(integer())
    empty_file <- tempfile(fileext = ".json")
    expect_type(empty$save(empty_file), "character")
    loaded_empty <- expect_s3_class(
        esg_result("file")$load(empty_file),
        "EsgResultFile"
    )
    expect_identical(loaded_empty$selection(), empty$selection())
})
# }}}
# query_result__run_url_checks() {{{
test_that("concurrent URL check wrappers preserve their HTTP semantics", {
    skip_on_cran()
    skip_if_not_installed("webfakes")

    ok_server <- local_downloader_http_server()
    missing_server <- local_downloader_http_server()
    ok_url <- ok_server$url("/files/ok.bin")
    missing_url <- missing_server$url("/files/missing.bin")
    urls <- c(ok_url, missing_url, ok_url)

    node <- query_result__reach_node_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_named(node, c(ok_url, missing_url))
    expect_true(all(vapply(node, function(x) isTRUE(x$reachable), logical(1L))))
    expect_identical(node[[ok_url]]$probe_url, ok_url)
    expect_identical(node[[missing_url]]$probe_url, missing_url)

    reachable <- query_result__reach_http_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_named(reachable, c(ok_url, missing_url))
    expect_true(reachable[[ok_url]]$reachable)
    expect_false(reachable[[missing_url]]$reachable)
    expect_match(reachable[[missing_url]]$error, "404")

    latency <- query_result__latency_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_named(latency, c(ok_url, missing_url))
    expect_true(all(vapply(
        latency,
        function(x) is.finite(x$latency),
        logical(1L)
    )))
    expect_true(all(vapply(
        latency,
        function(x) is.na(x$throughput),
        logical(1L)
    )))
})

test_that("OPeNDAP URL checks require a valid DDS response", {
    skip_on_cran()
    skip_if_not_installed("webfakes")

    # Use distinct local hosts because the callr-backed fixture server handles
    # only one simultaneous connection per process. Production ESGF endpoints
    # are still checked through the shared concurrent curl implementation.
    valid_server <- local_downloader_http_server()
    html_server <- local_downloader_http_server()
    missing_server <- local_downloader_http_server()
    valid <- valid_server$url("/dods/valid.nc")
    html <- html_server$url("/dods/html.nc")
    missing <- missing_server$url("/dods/missing.nc")
    checked <- query_result__check_opendap_urls(
        c(valid, html, missing),
        timeout = 5,
        concurrency = 3L
    )

    expect_true(checked[[valid]]$reachable)
    expect_false(checked[[html]]$reachable)
    expect_match(checked[[html]]$error, "valid DDS")
    expect_false(checked[[missing]]$reachable)
    expect_match(checked[[missing]]$error, "404")
    expect_true(query_result__valid_dds(charToRaw(
        "Dataset { Float32 tas[time = 1]; } valid.nc;"
    )))
    expect_false(query_result__valid_dds(charToRaw("<html>error</html>")))
})
# }}}
# EsgResult$reachable() {{{
test_that("EsgResult$reachable() returns per-record service diagnostics", {
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
        data_node = c(
            "same.example.org",
            "same.example.org",
            "missing.example.org",
            "bad.example.org"
        ),
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
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    calls <- character()
    timeouts <- numeric()
    agents <- character()
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            calls <<- c(calls, unique(urls))
            timeouts <<- c(timeouts, timeout)
            useragent <- if (is.null(network_policy$useragent)) {
                NA_character_
            } else {
                network_policy$useragent
            }
            agents <<- c(agents, useragent)
            missing <- is.na(urls) | !nzchar(urls)
            bad <- !missing & grepl("bad", urls)
            data.table::data.table(
                url = urls,
                reachable = data.table::fifelse(
                    missing,
                    NA,
                    !bad
                ),
                latency_ms = data.table::fifelse(
                    !missing & !bad,
                    125,
                    NA_real_
                ),
                error = data.table::fifelse(
                    missing,
                    "Missing URL.",
                    data.table::fifelse(bad, "boom", NA_character_)
                ),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )

    diag <- result$reachable(
        level = "url",
        probe = list(
            timeout = 9,
            network_policy = list(useragent = "test-agent")
        )
    )

    expect_named(
        diag,
        c(
            "record_index",
            "id",
            "data_node",
            "service",
            "url",
            "reachable",
            "latency_ms",
            "error",
            "probe_level",
            "probe_url",
            "probe_cached"
        )
    )
    expect_s3_class(diag, "data.table")
    expect_identical(diag$record_index, 1:4)
    expect_identical(diag$id, docs$id)
    expect_identical(diag$data_node, docs$data_node)
    expect_identical(diag$service, rep("OPENDAP", 4L))
    expect_identical(
        diag$url,
        c(
            "https://same.example.org/dods/file.nc",
            "https://same.example.org/dods/file.nc",
            NA_character_,
            "https://bad.example.org/dods/file.nc"
        )
    )
    expect_identical(diag$reachable, c(TRUE, TRUE, NA, FALSE))
    expect_equal(diag$latency_ms, c(125, 125, NA, NA))
    expect_identical(
        diag$error,
        c(NA_character_, NA_character_, "Missing URL.", "boom")
    )
    expect_identical(diag$probe_level, rep("url", 4L))
    expect_identical(diag$probe_url, diag$url)
    expect_false(any(diag$probe_cached))
    expect_equal(
        sum(calls == "https://same.example.org/dods/file.nc", na.rm = TRUE),
        1L
    )
    expect_equal(
        sum(calls == "https://bad.example.org/dods/file.nc", na.rm = TRUE),
        1L
    )
    expect_true(any(is.na(calls)))
    expect_true(all(timeouts == 9))
    expect_true(all(agents == "test-agent"))

    selected <- result$slice(diag$reachable %in% TRUE)
    expect_identical(selected$id, c("file-ok", "file-dup"))

    http <- result$reachable(service = "HTTPServer", level = "url")
    expect_identical(http$service, rep("HTTPServer", 4L))
    expect_identical(
        http$url,
        c("https://same.example.org/file.nc", rep(NA_character_, 3L))
    )
    expect_identical(http$reachable, c(TRUE, NA, NA, NA))

    empty <- query_result_test_object(
        "File",
        docs[0L, , drop = FALSE],
        query_result_test_params("File")
    )
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
        data_node = c(
            "same.example.org",
            "same.example.org",
            "missing.example.org",
            NA_character_
        ),
        check.names = FALSE
    )
    docs$url <- I(list(
        "https://same.example.org/dods/a.nc|application/netcdf|OPENDAP",
        "https://same.example.org/dods/b.nc|application/netcdf|OPENDAP",
        character(),
        "https://fallback.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    calls <- character()
    testthat::local_mocked_bindings(
        query_result__reach_node_urls = function(
            urls,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L
        ) {
            calls <<- c(calls, urls)
            stats::setNames(
                lapply(urls, function(url) {
                    list(
                        reachable = grepl("same|fallback", url),
                        latency_ms = if (grepl("fallback", url)) 44 else 22,
                        error = if (grepl("same|fallback", url)) {
                            NA_character_
                        } else {
                            "node boom"
                        },
                        probe_url = url
                    )
                }),
                urls
            )
        },
        .package = "epwshiftr"
    )

    diag <- result$reachable(
        probe = list(cache_seconds = 0L, cache_failures_seconds = 0L)
    )

    expect_identical(
        diag$url,
        c(
            "https://same.example.org/dods/a.nc",
            "https://same.example.org/dods/b.nc",
            NA_character_,
            "https://fallback.example.org/dods/file.nc"
        )
    )
    expect_identical(diag$reachable, c(TRUE, TRUE, NA, TRUE))
    expect_identical(
        diag$error,
        c(NA_character_, NA_character_, "Missing URL.", NA_character_)
    )
    expect_identical(diag$probe_level, rep("data_node", 4L))
    expect_identical(
        diag$probe_url,
        c(
            "https://same.example.org/",
            "https://same.example.org/",
            NA_character_,
            "https://fallback.example.org/"
        )
    )
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
    docs$dataset_id <- c(
        "dataset-bad|bad.example.org",
        "dataset-ok|ok.example.org"
    )
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
        query_result__reach_nodes = function(
            data_node,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
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
                latency_ms = ifelse(
                    grepl("replica", data_node),
                    10,
                    ifelse(grepl("ok", data_node), 50, NA_real_)
                ),
                error = ifelse(
                    grepl("ok|replica", data_node),
                    NA_character_,
                    "boom"
                ),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
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
            expect_identical(
                query_param__value(params$params()$instance_id),
                "instance-bad"
            )
            expect_true(all(
                EsgResultFile$private_fields$required_fields %in%
                    required_fields
            ))
            expect_true(all)
            expect_false(constraints)
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(
                    query_url = "https://replica-index.example.org/replicas"
                )
            )
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(
        result$repair_urls(
            index_node = "replica-index.example.org",
            probe = list(
                timeout = 11,
                concurrency = 2L,
                network_policy = list(useragent = "repair-test")
            )
        ),
        "EsgResultFile"
    )

    expect_identical(result$id, c("file-bad", "file-ok"))
    expect_identical(repaired$id, c("file-bad", "file-ok"))
    expect_identical(repaired$data_node, c("bad.example.org", "ok.example.org"))
    expect_identical(
        repaired$dataset_id,
        c("dataset-bad|bad.example.org", "dataset-ok|ok.example.org")
    )
    expect_identical(
        repaired$url_opendap[[1L]],
        "https://replica.example.org/dods/file.nc"
    )
    expect_identical(
        repaired$url_download[[1L]],
        "https://bad.example.org/file.nc"
    )
    expect_equal(repaired$count(), 2L)
    expect_identical(repaired$selection(), result$selection())
    expect_equal(length(collect_calls), 1L)
    expect_equal(length(probe_calls), 2L)
    expect_true(all(
        vapply(probe_calls, function(x) x$timeout, numeric(1L)) == 11
    ))
    expect_true(all(
        vapply(probe_calls, function(x) x$probe_concurrency, integer(1L)) == 2L
    ))
    expect_true(all(
        vapply(
            probe_calls,
            function(x) x$network_policy$useragent,
            character(1L)
        ) ==
            "repair-test"
    ))
    expect_true(all(
        vapply(probe_calls, function(x) x$cache_seconds, integer(1L)) == 3600L
    ))
    expect_true(all(
        vapply(
            probe_calls,
            function(x) x$cache_failures_seconds,
            integer(1L)
        ) ==
            0L
    ))
    expect_identical(
        unname(repaired$query_url("all")),
        c(
            "https://origin.example.org/search",
            "https://replica-index.example.org/replicas"
        )
    )
})

test_that("EsgResult$repair_urls() prefers reachable replicas already present in the current result", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(docs) <- NULL
    docs$id <- c("file-bad", "file-current-replica")
    docs$dataset_id <- c(
        "dataset-1|bad.example.org",
        "dataset-1|good.example.org"
    )
    docs$master_id <- c("master-file-current", "master-file-current")
    docs$instance_id <- c("instance-current", "instance-current")
    docs$data_node <- c("bad.example.org", "good.example.org")
    docs$replica <- c(FALSE, TRUE)
    docs$url <- I(list(
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://good.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(
            data_node,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            data.table::data.table(
                data_node = data_node,
                reachable = data_node == "good.example.org",
                latency_ms = ifelse(
                    data_node == "good.example.org",
                    7,
                    NA_real_
                ),
                error = ifelse(
                    data_node == "good.example.org",
                    NA_character_,
                    "bad node"
                ),
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

    expect_identical(repaired$id, c("file-bad", "file-current-replica"))
    expect_identical(
        repaired$data_node,
        c("bad.example.org", "good.example.org")
    )
    expect_identical(
        repaired$url_opendap,
        rep("https://good.example.org/dods/file.nc", 2L)
    )
})
# }}}
# EsgResult$expand_replicas() {{{
test_that("EsgResult$expand_replicas() falls back to master and version without crossing versions", {
    docs <- query_result_test_file_docs()
    docs$instance_id <- NA_character_
    docs$master_id <- "master-file-fallback"
    docs$version <- 20260101L
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    candidate_docs <- docs[rep(1L, 2L), , drop = FALSE]
    row.names(candidate_docs) <- NULL
    candidate_docs$id <- c("file-same-version", "file-other-version")
    candidate_docs$data_node <- c("same.example.org", "other.example.org")
    candidate_docs$version <- c(20260101L, 20270101L)

    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$master_id),
                "master-file-fallback"
            )
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
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

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
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            probed <<- c(probed, urls)
            data.table::data.table(
                url = urls,
                reachable = grepl("http-replica", urls),
                latency_ms = ifelse(grepl("http-replica", urls), 8, NA_real_),
                error = ifelse(
                    grepl("http-replica", urls),
                    NA_character_,
                    "bad http"
                ),
                probe_cached = FALSE
            )
        },
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            expect_identical(index_node, "https://example.org")
            expect_identical(query_param__value(params$type()), "File")
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$instance_id),
                "file-instance-1"
            )
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

    expect_identical(repaired$id, "file-http-bad")
    expect_identical(repaired$data_node, "bad-http.example.org")
    expect_identical(
        repaired$url_opendap,
        "https://opendap.example.org/dods/file.nc"
    )
    expect_identical(
        repaired$url_download,
        "https://http-replica.example.org/file.nc"
    )
    expect_true(any(grepl("bad-http", probed)))
    expect_true(any(grepl("http-replica", probed)))
})

test_that("File service resolution composes compatible replica URLs", {
    original_docs <- query_result_test_file_docs(c(
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://bad.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    original_docs$id <- "file-original"
    original_docs$instance_id <- "instance-shared"
    original_docs$master_id <- "master-shared"
    original_docs$data_node <- "bad.example.org"
    result <- query_result_test_object(
        "File",
        original_docs,
        query_result_test_params("File")
    )

    opendap_replica <- query_result_test_file_docs(c(
        "https://dap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://dap.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    opendap_replica$id <- "file-dap"
    opendap_replica$instance_id <- "instance-shared"
    opendap_replica$master_id <- "master-shared"
    opendap_replica$data_node <- "dap.example.org"
    http_replica <- query_result_test_file_docs(c(
        "https://http.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://http.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    http_replica$id <- "file-http"
    http_replica$instance_id <- "instance-shared"
    http_replica$master_id <- "master-shared"
    http_replica$data_node <- "http.example.org"
    candidate_docs <- data.table::rbindlist(
        list(opendap_replica, http_replica),
        use.names = TRUE,
        fill = TRUE
    )

    testthat::local_mocked_bindings(
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            good <- if (identical(service, "OPENDAP")) {
                grepl("dap.example.org", urls, fixed = TRUE)
            } else {
                grepl("http.example.org", urls, fixed = TRUE)
            }
            data.table::data.table(
                url = urls,
                reachable = good,
                latency_ms = ifelse(good, 10, NA_real_),
                error = ifelse(good, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/replicas")
            )
        },
        .package = "epwshiftr"
    )

    resolved <- query_result__resolve_file_services(
        result,
        check = list(
            level = "url",
            cache_seconds = 0L,
            cache_failures_seconds = 0L
        )
    )

    expect_s3_class(resolved$result, "EsgResultFile")
    expect_identical(resolved$result$id, "file-original")
    expect_identical(
        resolved$result$url_opendap,
        "https://dap.example.org/dods/file.nc"
    )
    expect_identical(
        resolved$result$url_download,
        "https://bad.example.org/file.nc"
    )
    expect_equal(sum(resolved$diagnostics$selected), 2L)
    expect_true(all(is.na(
        resolved$diagnostics[service == "HTTPServer", reachable]
    )))
    expect_true(all(
        resolved$diagnostics[service == "HTTPServer", probe_level] == "deferred"
    ))

    incompatible <- data.table::copy(candidate_docs)
    incompatible$checksum[[2L]] <- "different"
    expect_identical(
        query_result__compatible_content(original_docs, incompatible),
        c(TRUE, FALSE)
    )
    expect_identical(
        query_result__compatible_file_groups(incompatible),
        c(1L, 2L)
    )
})

test_that("File service resolution retains unchecked HTTP recovery candidates", {
    dead <- query_result_test_file_docs(c(
        "https://dead.example.org/dods/dead.nc|application/netcdf|OPENDAP",
        "https://dead.example.org/files/dead.nc|application/netcdf|HTTPServer"
    ))
    dead$id <- "file-dead"
    dead$instance_id <- "instance-dead"
    dead$master_id <- "master-dead"
    http_only <- query_result_test_file_docs(c(
        "https://http-only.example.org/dods/http.nc|application/netcdf|OPENDAP",
        "https://http-only.example.org/files/http.nc|application/netcdf|HTTPServer"
    ))
    http_only$id <- "file-http-only"
    http_only$instance_id <- "instance-http-only"
    http_only$master_id <- "master-http-only"
    docs <- data.table::rbindlist(
        list(dead, http_only),
        use.names = TRUE,
        fill = TRUE
    )
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    testthat::local_mocked_bindings(
        query_result__repair_urls = function(
            result,
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            result
        },
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            available <- identical(service, "HTTPServer") &
                grepl("http-only", urls, fixed = TRUE)
            data.table::data.table(
                url = urls,
                reachable = available,
                latency_ms = ifelse(available, 5, NA_real_),
                error = ifelse(available, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )

    resolved <- query_result__resolve_file_services(
        result,
        check = list(level = "url", cache_seconds = 0L)
    )
    expect_identical(resolved$result$id, c("file-dead", "file-http-only"))
    expect_true(all(is.na(resolved$result$url_opendap)))
    expect_identical(
        resolved$result$url_download,
        c(
            "https://dead.example.org/files/dead.nc",
            "https://http-only.example.org/files/http.nc"
        )
    )
    expect_equal(sum(resolved$diagnostics$selected), 2L)
    expect_true(all(is.na(
        resolved$diagnostics[service == "HTTPServer", reachable]
    )))
})

test_that("File service resolution collapses compatible distributed replicas", {
    dap <- query_result_test_file_docs(c(
        "https://dap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://dap.example.org/files/file.nc|application/netcdf|HTTPServer"
    ))
    dap$id <- "file-dap-replica"
    dap$instance_id <- "file-instance-dap"
    dap$data_node <- "dap.example.org"
    http <- query_result_test_file_docs(c(
        "https://http.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://http.example.org/files/file.nc|application/netcdf|HTTPServer"
    ))
    http$id <- "file-http-replica"
    http$instance_id <- "file-instance-http"
    http$data_node <- "http.example.org"
    docs <- data.table::rbindlist(
        list(dap, http),
        use.names = TRUE,
        fill = TRUE
    )
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    testthat::local_mocked_bindings(
        query_result__repair_urls = function(
            result,
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            result
        },
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            available <- if (identical(service, "OPENDAP")) {
                grepl("dap.example.org", urls, fixed = TRUE)
            } else {
                grepl("http.example.org", urls, fixed = TRUE)
            }
            data.table::data.table(
                url = urls,
                reachable = available,
                latency_ms = ifelse(available, 5, NA_real_),
                error = ifelse(available, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )

    resolved <- query_result__resolve_file_services(
        result,
        check = list(level = "url", cache_seconds = 0L)
    )
    expect_equal(resolved$result$count(), 1L)
    expect_match(resolved$result$url_opendap, "dap\\.example\\.org")
    expect_match(resolved$result$url_download, "dap\\.example\\.org")
    expect_equal(sum(resolved$diagnostics$selected), 2L)
})

test_that("URL check cache keys distinguish HTTP and OPeNDAP contracts", {
    url <- "https://example.org/data/file.nc"
    opendap_key <- query_result__reach_cache_key(
        "url:OPENDAP",
        url,
        timeout = 5
    )
    http_key <- query_result__reach_cache_key(
        "url:HTTPSERVER",
        url,
        timeout = 5
    )

    expect_false(identical(opendap_key, http_key))
})

test_that("EsgResult$repair_urls() repairs Aggregation URLs with replica queries", {
    docs <- query_result_test_file_docs(
        "https://bad.example.org/dods/agg.nc|application/netcdf|OPENDAP"
    )
    docs$id <- "aggregation-bad"
    docs$dataset_id <- "dataset-agg|bad.example.org"
    docs$master_id <- "master-aggregation"
    docs$data_node <- "bad.example.org"
    result <- query_result_test_object(
        "Aggregation",
        docs,
        query_result_test_params("Aggregation")
    )

    candidate_docs <- query_result_test_file_docs(
        "https://agg-replica.example.org/dods/agg.nc|application/netcdf|OPENDAP"
    )
    candidate_docs$id <- "aggregation-repaired"
    candidate_docs$dataset_id <- "dataset-agg|agg-replica.example.org"
    candidate_docs$master_id <- "master-aggregation"
    candidate_docs$data_node <- "agg-replica.example.org"

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(
            data_node,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            data.table::data.table(
                data_node = data_node,
                reachable = grepl("agg-replica", data_node),
                latency_ms = ifelse(
                    grepl("agg-replica", data_node),
                    12,
                    NA_real_
                ),
                error = ifelse(
                    grepl("agg-replica", data_node),
                    NA_character_,
                    "bad agg"
                ),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            expect_identical(query_param__value(params$type()), "Aggregation")
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$instance_id),
                "file-instance-1"
            )
            expect_true(all(
                EsgResultAggregation$private_fields$required_fields %in%
                    required_fields
            ))
            list(
                response = query_result_test_response(candidate_docs),
                docs = candidate_docs,
                parameter = query_param__clone(params),
                context = list(
                    query_url = "https://example.org/aggregation-replicas"
                )
            )
        },
        .package = "epwshiftr"
    )

    repaired <- expect_s3_class(result$repair_urls(), "EsgResultAggregation")

    expect_identical(repaired$id, "aggregation-bad")
    expect_identical(repaired$data_node, "bad.example.org")
    expect_identical(
        repaired$url_opendap,
        "https://agg-replica.example.org/dods/agg.nc"
    )
    expect_identical(
        unname(repaired$query_url("all"))[[2L]],
        "https://example.org/aggregation-replicas"
    )
})

test_that("EsgResult$repair_urls() keeps original records when repair is impossible", {
    docs <- query_result_test_file_docs(
        "https://missing-master.example.org/dods/file.nc|application/netcdf|OPENDAP"
    )
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
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    candidate_docs <- query_result_test_file_docs(
        "https://still-bad.example.org/dods/file.nc|application/netcdf|OPENDAP"
    )
    candidate_docs$id <- "file-still-bad"
    candidate_docs$master_id <- "master-no-replica"
    candidate_docs$instance_id <- NA_character_
    candidate_docs$data_node <- "still-bad.example.org"

    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(
            data_node,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            data.table::data.table(
                data_node = data_node,
                reachable = rep(FALSE, length(data_node)),
                latency_ms = rep(NA_real_, length(data_node)),
                error = rep("unreachable", length(data_node)),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$master_id),
                "master-no-replica"
            )
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
    expect_identical(
        unname(repaired$query_url("all"))[[2L]],
        "https://example.org/no-replica"
    )

    expect_error(
        result$repair_urls(probe = list(foo = 1)),
        "Unknown `probe` field"
    )
})
# }}}
# EsgResult$filter_time() {{{
test_that("time-range helpers normalize columns and preserve paired fallback", {
    short <- list(datetime_start = "2050-01-01T00:00:00Z")
    expect_identical(
        query_result__character_column(short, "datetime_start", 3L),
        c("2050-01-01T00:00:00Z", NA_character_, NA_character_)
    )
    expect_identical(
        query_result__character_column(short, "datetime_end", 3L),
        rep(NA_character_, 3L)
    )

    docs <- data.frame(
        datetime_start = c(
            "2040-01-01T00:00:00Z",
            "2059-06-01T00:00:00Z",
            NA_character_
        ),
        datetime_end = c(
            "2040-12-31T23:59:59Z",
            NA_character_,
            NA_character_
        ),
        check.names = FALSE
    )
    calls <- new.env(parent = emptyenv())
    calls$count <- 0L
    ranges <- query_result__fill_time_ranges(docs, function() {
        calls$count <- calls$count + 1L
        c(
            "tas_day_Model_x_20400101-20401231.nc",
            "tas_day_Model_x_20500101-20501231.nc",
            "tas_day_Model_x_unknown.nc"
        )
    })

    expect_identical(calls$count, 1L)
    expect_s3_class(ranges$datetime_start, "POSIXct")
    expect_identical(
        format(ranges$datetime_start[[1L]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "2040-01-01T00:00:00Z"
    )
    expect_identical(
        format(ranges$datetime_start[[2L]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "2050-01-01T00:00:00Z"
    )
    expect_identical(
        format(ranges$datetime_end[[2L]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "2050-12-31T23:59:59Z"
    )
    expect_true(is.na(ranges$datetime_start[[3L]]))
    expect_true(is.na(ranges$datetime_end[[3L]]))

    complete <- docs[1L, , drop = FALSE]
    untouched <- expect_silent(query_result__fill_time_ranges(
        complete,
        function() stop("complete metadata must not resolve labels")
    ))
    expect_identical(
        format(untouched$datetime_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "2040-01-01T00:00:00Z"
    )
})

test_that("EsgResult$filter_time() filters File and Aggregation results using DRS filename ranges", {
    for (type in c("File", "Aggregation")) {
        result <- query_result_test_object(
            type,
            query_result_test_file_time_docs(type),
            query_result_test_params(type)
        )

        expect_false("datetime_start" %in% result$fields)
        warnings <- character()
        filtered <- withCallingHandlers(
            result$filter_time("2050-06-01", "2050-06-30", method = "drs"),
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        expect_identical(
            warnings,
            sprintf(
                "Could not parse a DRS time range for 1 %s record(s); keeping those records.",
                tolower(type)
            )
        )
        expect_s3_class(filtered, class(result)[[1L]])
        expect_identical(filtered$id, result$id[c(1L, 3L)])
        expect_false("datetime_start" %in% result$fields)
        expect_true(all(
            c("datetime_start", "datetime_end") %in% filtered$fields
        ))
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

test_that("EsgResult$filter_time() is silent when every DRS range can be parsed", {
    docs <- query_result_test_file_time_docs("File")[1:2, , drop = FALSE]
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    filtered <- expect_silent(
        result$filter_time("2050-06-01", "2050-06-30", method = "drs")
    )

    expect_identical(filtered$id, docs$id[[1L]])
    expect_identical(filtered$time_filter$unknown_count, 0L)
})

test_that("EsgResult$filter_time() auto mode preserves metadata and fills DRS gaps", {
    docs <- query_result_test_file_time_docs("File")
    docs$datetime_start <- c(
        "2050-01-01T00:00:00Z",
        NA_character_,
        NA_character_
    )
    docs$datetime_end <- c(
        "2050-12-31T23:59:59Z",
        NA_character_,
        NA_character_
    )
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    warnings <- character()
    filtered <- withCallingHandlers(
        result$filter_time("2050-06-01", "2050-06-30", method = "auto"),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    ranges <- filtered$to_data_table(
        c("id", "datetime_start", "datetime_end")
    )

    expect_identical(filtered$time_filter$method, "auto")
    expect_identical(filtered$id, docs$id[c(1L, 3L)])
    expect_identical(ranges$datetime_start[[1L]], "2050-01-01T00:00:00Z")
    expect_true(is.na(ranges$datetime_start[[2L]]))
    expect_true(any(grepl("metadata or DRS", warnings, fixed = TRUE)))
})

test_that("EsgResult$filter_time() context persists through save/load", {
    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs(),
        query_result_test_params("File")
    )
    filtered <- suppressWarnings(result$filter_time(
        "2050-06-01",
        "2050-06-30",
        method = "drs"
    ))
    file <- tempfile(fileext = ".json")

    expect_type(filtered$save(file), "character")
    json <- jsonlite::fromJSON(
        file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
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
                    values <- as.POSIXct(
                        c("2050-01-01", "2050-12-31"),
                        tz = "UTC"
                    )
                } else {
                    values <- as.POSIXct(
                        c("2080-01-01", "2080-12-31"),
                        tz = "UTC"
                    )
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
    testthat::local_mocked_bindings(
        EsgDataset = FakeEsgDataset,
        .package = "epwshiftr"
    )

    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs(),
        query_result_test_params("File")
    )
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
    expect_true(all(
        c("filename", "url_opendap", "url_download") %in%
            empty_file_with_fields$fields
    ))

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
    expect_true(all(
        c("url_opendap", "url_download") %in%
            empty_aggregation_with_fields$fields
    ))

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

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
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
    expect_identical(
        files$to_data_table("url_opendap")$url_opendap,
        "https://example.org/dods/file.nc"
    )
    expect_identical(
        files$to_data_table("url_download")$url_download,
        "https://example.org/file.nc"
    )
    expect_error(files$to_data_table(character()), "Must have length")

    aggs <- query_result_test_object(
        "Aggregation",
        docs,
        query_result_test_params("Aggregation")
    )
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
        result <- query_result_test_object(
            type,
            empty_file_docs,
            query_result_test_params(type)
        )
        file <- tempfile(fileext = ".json")

        expect_type(result$save(file), "character")
        loaded <- expect_s3_class(
            esg_result(case_name)$load(file),
            class(result)[[1L]]
        )
        expect_equal(loaded$count(), 0L)
        expect_identical(loaded$fields, character())
        expect_s3_class(loaded$to_data_table(), "data.table")

        unlink(file)
    }

    for (case_name in c("file", "aggregation")) {
        type <- switch(case_name, file = "File", aggregation = "Aggregation")
        generator <- switch(
            case_name,
            file = EsgResultFile,
            aggregation = EsgResultAggregation
        )
        response <- query_result_test_response(empty_file_docs)
        response$response$docs <- list()
        response$facet_counts$facet_fields <- list()
        result <- query_result__new(
            generator,
            "https://example.org",
            query_result_test_params(type),
            response
        )
        file <- tempfile(fileext = ".json")

        expect_type(result$save(file), "character")
        loaded <- expect_s3_class(
            esg_result(case_name)$load(file),
            class(result)[[1L]]
        )
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
        query__collect = function(...) {
            stop("query__collect should not be called")
        },
        .package = "epwshiftr"
    )

    expect_error(
        datasets$collect(which = 1L, type = "File"),
        "empty Dataset result"
    )
    expect_error(
        datasets$collect(which = "dataset-1", type = "Aggregation"),
        "empty Dataset result"
    )

    files <- expect_s3_class(datasets$collect(type = "File"), "EsgResultFile")
    expect_equal(files$count(), 0L)
    expect_identical(files$fields, character())
    expect_identical(query_param__value(priv(files)$parameter$type()), "File")

    aggs <- expect_s3_class(
        datasets$collect(type = "Aggregation"),
        "EsgResultAggregation"
    )
    expect_equal(aggs$count(), 0L)
    expect_identical(aggs$fields, character())
    expect_identical(
        query_param__value(priv(aggs)$parameter$type()),
        "Aggregation"
    )
})

test_that("EsgResultDataset$collect() inherits controls and normalizes limit", {
    params <- query_result_test_params(
        "Dataset",
        latest = FALSE,
        distrib = FALSE,
        replica = FALSE
    )
    params$source_id("AWI-CM-1-1-MR")
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = "dataset-1", size = 1, check.names = FALSE),
        params
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints
            )
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        datasets$collect(fields = "id", limit = NULL),
        "EsgResultFile"
    )
    expect_identical(calls[[1L]]$index_node, "https://example.org")
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param__value(calls[[1L]]$params$latest()))
    expect_false(query_param__value(calls[[1L]]$params$distrib()))
    expect_false(query_param__value(calls[[1L]]$params$replica()))
    expect_null(calls[[1L]]$params$project())
    expect_null(calls[[1L]]$params$source_id())
    expect_identical(
        query_param__value(calls[[1L]]$params$state()$dataset_id),
        "dataset-1"
    )
    expect_true(all(
        EsgResultFile$private_fields$required_fields %in%
            query_param__value(priv(files)$parameter$fields())
    ))

    expect_s3_class(
        datasets$collect(
            fields = "id",
            limit = 1L,
            latest = TRUE,
            distrib = TRUE,
            replica = TRUE
        ),
        "EsgResultFile"
    )
    expect_equal(calls[[2L]]$limit, 1L)
    expect_true(query_param__value(calls[[2L]]$params$latest()))
    expect_true(query_param__value(calls[[2L]]$params$distrib()))
    expect_true(query_param__value(calls[[2L]]$params$replica()))

    aggs <- expect_s3_class(
        datasets$collect(
            fields = "id",
            limit = 1L,
            type = "Aggregation",
            index_node = "esg-dn1.nsc.liu.se"
        ),
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
    expect_identical(
        query_param__value(calls[[3L]]$params$state()$dataset_id),
        "dataset-1"
    )
    expect_true(all(
        EsgResultAggregation$private_fields$required_fields %in%
            query_param__value(priv(aggs)$parameter$fields())
    ))
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
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(params = params, limit = limit)
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1L, data_node = "example.org"),
        "EsgResultFile"
    )
    expect_identical(
        query_param__value(calls[[1L]]$params$data_node()),
        "example.org"
    )
    expect_identical(
        calls[[1L]]$params$render(c("datetime_start", "datetime_stop")),
        character()
    )

    expect_error(
        datasets$collect(source_id = "AWI-CM-1-1-MR"),
        "unsupported parameter"
    )
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
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            dataset_id <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                dataset_id = dataset_id
            )
            docs <- data.frame(
                id = paste0("file-", dataset_id),
                dataset_id = dataset_id,
                size = seq_along(dataset_id),
                url = I(rep(
                    list(
                        "https://example.org/file.nc|application/netcdf|HTTPServer"
                    ),
                    length(dataset_id)
                )),
                check.names = FALSE
            )
            response <- query_result_test_response(docs)
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        index_node,
                        "/search?",
                        paste(dataset_id, collapse = ",")
                    )
                )
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        datasets$collect(
            fields = "id",
            limit = 1L,
            index_node = "fallback.example.org"
        ),
        "EsgResultFile"
    )
    expect_identical(
        vapply(calls, `[[`, character(1L), "index_node"),
        "https://fallback.example.org"
    )
    expect_length(calls, 1L)
    expect_identical(
        unlist(lapply(calls, `[[`, "dataset_id"), use.names = FALSE),
        datasets$id
    )
    # The global row cap also applies if a backend returns too many rows.
    expect_identical(files$count(), 1L)
    expect_length(priv(files)$context$query_url, 1L)
})

test_that("EsgResultDataset$collect() batches large child queries", {
    dataset_id <- sprintf(
        "dataset-%03d",
        seq_len(QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE * 2L + 5L)
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = dataset_id,
            size = seq_along(dataset_id),
            check.names = FALSE
        ),
        query_result_test_params("Dataset")
    )

    calls <- list()
    # Mock child collection so the test can inspect per-batch query inputs
    # without issuing ESGF requests.
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress = FALSE,
            progress_label = NULL,
            progress_callback = NULL
        ) {
            batch_id <- query_param__value(params$state()$dataset_id)
            call_id <- length(calls) + 1L
            calls[[call_id]] <<- list(
                index_node = index_node,
                dataset_id = batch_id,
                dict_check = dict_check,
                all = all,
                limit = limit
            )

            docs <- data.frame(
                id = paste0("file-", batch_id),
                dataset_id = batch_id,
                size = seq_along(batch_id),
                check.names = FALSE
            )
            docs$url <- I(rep(
                list(
                    "https://example.org/file.nc|application/netcdf|HTTPServer"
                ),
                length(batch_id)
            ))
            params$fields(unique(c(
                query_param__value(params$fields()),
                required_fields
            )))
            response <- query_result_test_response(docs)
            list(
                response = response,
                docs = docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        "https://example.org/search?page=",
                        call_id
                    )
                )
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        datasets$collect(fields = "id", all = TRUE),
        "EsgResultFile"
    )

    expect_length(calls, 3L)
    expect_equal(
        vapply(calls, function(call) length(call$dataset_id), integer(1L)),
        c(50L, 50L, 5L)
    )
    expect_identical(
        unlist(lapply(calls, `[[`, "dataset_id"), use.names = FALSE),
        dataset_id
    )
    expect_true(calls[[1L]]$dict_check)
    expect_false(any(vapply(calls[-1L], `[[`, logical(1L), "dict_check")))
    expect_equal(files$count(), length(dataset_id))
    expect_length(priv(files)$context$query_url, 3L)
    expect_identical(
        query_param__value(priv(files)$parameter$state()$dataset_id),
        dataset_id
    )
    expect_true(all(
        EsgResultFile$private_fields$required_fields %in%
            query_param__value(priv(files)$parameter$fields())
    ))
})

test_that("EsgResultDataset$collect() keeps limit global across child batches", {
    dataset_id <- sprintf(
        "dataset-%03d",
        seq_len(QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE * 2L + 20L)
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = dataset_id,
            size = seq_along(dataset_id),
            check.names = FALSE
        ),
        query_result_test_params("Dataset")
    )

    calls <- list()
    # Mock child collection so the test can verify the public `limit` remains
    # global instead of being applied independently to every batch.
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            batch_id <- query_param__value(params$state()$dataset_id)
            param_limit <- query_param__value(params$limit())
            take <- min(length(batch_id), as.integer(param_limit))
            docs <- data.frame(
                id = paste0("file-", batch_id[seq_len(take)]),
                dataset_id = batch_id[seq_len(take)],
                size = seq_len(take),
                check.names = FALSE
            )
            docs$url <- I(rep(
                list(
                    "https://example.org/file.nc|application/netcdf|HTTPServer"
                ),
                take
            ))
            params$fields(unique(c(
                query_param__value(params$fields()),
                required_fields
            )))
            calls[[length(calls) + 1L]] <<- list(
                dataset_id = batch_id,
                limit = limit,
                param_limit = param_limit
            )

            response <- query_result_test_response(docs)
            list(
                response = response,
                docs = docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        "https://example.org/search?page=",
                        length(calls)
                    )
                )
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        datasets$collect(fields = "id", limit = 55L),
        "EsgResultFile"
    )

    expect_length(calls, 2L)
    expect_equal(vapply(calls, `[[`, integer(1L), "limit"), c(55L, 5L))
    expect_equal(vapply(calls, `[[`, integer(1L), "param_limit"), c(55L, 5L))
    expect_equal(files$count(), 55L)
    expect_identical(query_param__value(priv(files)$parameter$limit()), 55L)
    expect_identical(
        query_param__value(priv(files)$parameter$state()$dataset_id),
        dataset_id
    )
})

test_that("EsgResultDataset$collect() passes progress to child query only for non-empty results", {
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = c("dataset-1", "dataset-2"),
            size = c(1, 1),
            check.names = FALSE
        ),
        query_result_test_params("Dataset")
    )
    empty <- query_result_test_object(
        "Dataset",
        data.frame(id = character(), size = numeric(), check.names = FALSE),
        query_result_test_params("Dataset")
    )

    callback <- function(event) invisible(event)
    priv(datasets)$progress_callback <- callback
    calls <- list()
    # Mock child collection so the test can inspect progress forwarding
    # without issuing ESGF requests.
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress = FALSE,
            progress_label = NULL,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(
                callback = progress_callback,
                progress = progress,
                progress_label = progress_label
            )
            response <- query_result_test_response(query_result_test_file_docs())
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    expect_s3_class(
        files <- datasets$collect(fields = "id", progress = TRUE),
        "EsgResultFile"
    )
    expect_identical(calls[[1L]]$callback, callback)
    expect_false("progress_callback" %in% names(priv(files)))
    expect_length(calls, 1L)
    expect_true(calls[[1L]]$progress)
    expect_identical(calls[[1L]]$progress_label, "Collecting File records")

    expect_s3_class(
        datasets$collect(fields = "id", type = "Aggregation", progress = TRUE),
        "EsgResultAggregation"
    )
    expect_identical(calls[[2L]]$callback, callback)
    expect_length(calls, 2L)
    expect_true(calls[[2L]]$progress)
    expect_identical(
        calls[[2L]]$progress_label,
        "Collecting Aggregation records"
    )

    expect_s3_class(
        empty$collect(fields = "id", progress = TRUE),
        "EsgResultFile"
    )
    expect_length(calls, 2L)
})

test_that("EsgResultDataset$collect() labels progress for child query batches", {
    dataset_id <- sprintf(
        "dataset-%03d",
        seq_len(QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE * 2L + 1L)
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = dataset_id,
            size = seq_along(dataset_id),
            check.names = FALSE
        ),
        query_result_test_params("Dataset")
    )

    calls <- list()
    # Mock child collection so the test can inspect per-batch progress inputs
    # without issuing ESGF requests.
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress = FALSE,
            progress_label = NULL,
            progress_callback = NULL
        ) {
            batch_id <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(
                dataset_id = batch_id,
                progress = progress,
                progress_label = progress_label
            )

            # Return one child File row per Dataset ID so the batch merge path
            # sees realistic row counts and URL fields.
            docs <- data.frame(
                id = paste0("file-", batch_id),
                dataset_id = batch_id,
                size = seq_along(batch_id),
                check.names = FALSE
            )
            docs$url <- I(rep(
                list(
                    "https://example.org/file.nc|application/netcdf|HTTPServer"
                ),
                length(batch_id)
            ))
            params$fields(unique(c(
                query_param__value(params$fields()),
                required_fields
            )))
            response <- query_result_test_response(docs)
            list(
                response = response,
                docs = docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        "https://example.org/search?page=",
                        length(calls)
                    )
                )
            )
        },
        .package = "epwshiftr"
    )

    expect_s3_class(
        datasets$collect(fields = "id", all = TRUE, progress = TRUE),
        "EsgResultFile"
    )

    expect_length(calls, 3L)
    expect_true(all(vapply(calls, `[[`, logical(1L), "progress")))
    expect_identical(
        vapply(calls, `[[`, character(1L), "progress_label"),
        sprintf("Collecting File records (batch %d/3)", seq_along(calls))
    )
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
    datasets <- query_result_test_object(
        "Dataset",
        docs,
        query_result_test_params("Dataset")
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields
            )
            if (!is.null(params$params()$instance_id)) {
                expect_identical(
                    query_param__value(params$params()$instance_id),
                    "dataset-instance-1"
                )
                out <- docs[rep(1L, 2L), , drop = FALSE]
                out$id <- c(
                    "dataset-1|node-a.example.org",
                    "dataset-1|node-b.example.org"
                )
                out$data_node <- c("node-a.example.org", "node-b.example.org")
            } else {
                expect_identical(
                    query_param__value(params$params()$master_id),
                    "dataset-master-1"
                )
                out <- docs[rep(1L, 2L), , drop = FALSE]
                out$id <- c(
                    "dataset-1.v20260101|node-a.example.org",
                    "dataset-1.v20270101|node-a.example.org"
                )
                out$version <- c(20260101L, 20270101L)
            }
            expect_identical(index_node, "https://replica-index.example.org")
            expect_null(params$project())
            expect_identical(query_param__value(params$type()), "Dataset")
            expect_true(all(
                EsgResultDataset$private_fields$required_fields %in%
                    required_fields
            ))
            list(
                response = query_result_test_response(out),
                docs = out,
                parameter = query_param__clone(params),
                context = list(
                    query_url = paste0(index_node, "/dataset-replicas")
                )
            )
        },
        .package = "epwshiftr"
    )

    same_version <- expect_s3_class(
        datasets$expand_replicas(index_node = "replica-index.example.org"),
        "EsgResultDataset"
    )
    expect_identical(
        same_version$data_node,
        c("node-a.example.org", "node-b.example.org")
    )

    logical_dataset <- expect_s3_class(
        datasets$expand_replicas(
            by = "master_id",
            index_node = "replica-index.example.org"
        ),
        "EsgResultDataset"
    )
    expect_identical(logical_dataset$version, c(20260101L, 20270101L))
    expect_length(calls, 2L)
})
# }}}
# EsgResultDataset$has_opendap() / EsgResultDataset$has_download() {{{
test_that("EsgResultDataset$has_opendap() / EsgResultDataset$has_download() tolerate missing access fields", {
    datasets <- query_result_test_object(
        "Dataset",
        query_result_test_dataset_docs(access = FALSE)
    )
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

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    aggs <- query_result_test_object(
        "Aggregation",
        docs,
        query_result_test_params("Aggregation")
    )

    expect_identical(
        files$url_download,
        c(
            NA_character_,
            NA_character_,
            "https://example.org/file-3.nc",
            "https://example.org/file-4.nc"
        )
    )
    expect_warning(
        opendap <- files$url_opendap,
        "record 4 \\(id: file-4\\)"
    )
    expect_identical(
        opendap,
        c(
            NA_character_,
            NA_character_,
            NA_character_,
            "https://example.org/dods/file-4"
        )
    )
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

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    expect_warning(
        opendap <- files$url_opendap,
        "record 1 \\(dataset_id: dataset-1\\)"
    )
    expect_identical(opendap, "https://example.org/dods/file-1")
})
# }}}
# Service preflight checks {{{
test_that("File service resolution can preflight OPeNDAP by sampled data node", {
    pairs <- lapply(seq_len(6L), function(index) {
        # Build one logical file replica on each candidate data node.
        make_replica <- function(node, replica) {
            doc <- query_result_test_file_docs(c(
                sprintf(
                    "https://%s/dods/file-%d.nc|application/netcdf|OPENDAP",
                    node,
                    index
                ),
                sprintf(
                    "https://%s/files/file-%d.nc|application/netcdf|HTTPServer",
                    node,
                    index
                )
            ))
            doc$id <- sprintf("file-%d-%s", index, node)
            doc$instance_id <- sprintf("instance-%d", index)
            doc$master_id <- sprintf("master-%d", index)
            doc$tracking_id <- sprintf("tracking-%d", index)
            doc$checksum <- sprintf("checksum-%d", index)
            doc$filename <- sprintf("file-%d.nc", index)
            doc$title <- doc$filename
            doc$size <- 100 + index
            doc$variable_id <- sprintf("var-%d", index)
            doc$data_node <- node
            doc$replica <- replica
            doc
        }
        data.table::rbindlist(
            list(
                make_replica("bad.example.org", FALSE),
                make_replica("good.example.org", TRUE)
            ),
            use.names = TRUE,
            fill = TRUE
        )
    })
    docs <- data.table::rbindlist(pairs, use.names = TRUE, fill = TRUE)
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )

    checked_urls <- character()
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(...) {
            stop(
                "sampled node preflight must not run exhaustive replica repair"
            )
        },
        query_result__reach_service_urls = function(
            urls,
            service,
            timeout = 5,
            network_policy = NULL,
            concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            checked_urls <<- c(checked_urls, urls)
            good <- grepl("good.example.org", urls, fixed = TRUE)
            data.table::data.table(
                url = urls,
                reachable = good,
                latency_ms = data.table::fifelse(good, 10, NA_real_),
                error = data.table::fifelse(good, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )

    resolved <- query_result__resolve_file_services(
        result,
        check = list(
            level = "url",
            sample_per_node = 3L,
            cache_seconds = 0L,
            cache_failures_seconds = 0L
        )
    )

    expect_equal(resolved$result$count(), 6L)
    expect_true(all(grepl(
        "good.example.org",
        resolved$result$url_opendap,
        fixed = TRUE
    )))
    expect_length(checked_urls, 6L)
    expect_equal(sum(grepl("bad.example.org", checked_urls, fixed = TRUE)), 3L)
    expect_equal(sum(grepl("good.example.org", checked_urls, fixed = TRUE)), 3L)
    expect_true(all(
        resolved$diagnostics[service == "OPENDAP", probe_level] ==
            "service_node_sample"
    ))
    expect_equal(
        sum(
            resolved$diagnostics[service == "OPENDAP", selected]
        ),
        6L
    )
})

# Keep transport deterministic while exercising real callback indexing, wave
# admission, deadlines and fallback decisions through the shared URL runner.
test_that("URL checks bound active waves and preserve completed results", {
    mode <- "success"
    pools <- list()
    deadlines <- numeric()
    serial_urls <- character()
    cancelled <- 0L
    testthat::local_mocked_bindings(
        multi_cancel = function(handle) {
            cancelled <<- cancelled + 1L
        },
        new_pool = function(total_con, host_con) {
            pool <- new.env(parent = emptyenv())
            pool$callbacks <- list()
            pool$capacity <- total_con
            pool$index <- length(pools) + 1L
            pools[[pool$index]] <<- pool
            pool
        },
        multi_add = function(handle, done, fail, pool) {
            pool$callbacks[[length(pool$callbacks) + 1L]] <- list(
                done = done,
                fail = fail
            )
            invisible(handle)
        },
        multi_run = function(timeout, poll, pool) {
            deadlines <<- c(deadlines, timeout)
            if (mode == "error" && pool$index == 2L) {
                stop("pool failed")
            }
            if (mode == "timeout") {
                return(invisible(NULL))
            }
            for (i in rev(seq_along(pool$callbacks))) {
                if (mode == "failed" && i == 1L) {
                    pool$callbacks[[i]]$fail("unavailable")
                } else {
                    pool$callbacks[[i]]$done(list(status_code = 200L))
                }
            }
            invisible(NULL)
        },
        .package = "curl"
    )
    # Reset per-call observations while retaining the chosen transport mode.
    run <- function(count, retry = FALSE) {
        pools <<- list()
        deadlines <<- numeric()
        serial_urls <<- character()
        cancelled <<- 0L
        query_result__run_url_checks(
            if (count) {
                paste0("https://example.org/", seq_len(count))
            } else {
                character()
            },
            timeout = 5,
            network_policy = NULL,
            concurrency = 3L,
            retry_failed = retry,
            serial_check = function(url) {
                serial_urls <<- c(serial_urls, url)
                list(reachable = TRUE, url = url)
            },
            done_result = function(response, url, started_at) {
                list(reachable = TRUE, url = url)
            }
        )
    }
    out <- run(8L)
    expect_identical(
        vapply(pools, function(x) length(x$callbacks), integer(1L)),
        c(3L, 3L, 2L)
    )
    expect_equal(deadlines, rep(10, 3L))
    expect_identical(cancelled, 8L)
    expect_identical(
        vapply(out, `[[`, character(1L), "url"),
        stats::setNames(names(out), names(out))
    )
    expect_length(serial_urls, 0L)
    mode <- "failed"
    out <- run(8L)
    expect_equal(sum(vapply(out, `[[`, logical(1L), "reachable")), 5L)
    expect_length(serial_urls, 0L)
    mode <- "timeout"
    out <- run(20L)
    expect_false(any(vapply(out, `[[`, logical(1L), "reachable")))
    expect_length(serial_urls, 0L)
    out <- run(2L)
    expect_length(serial_urls, 2L)
    expect_true(all(vapply(out, `[[`, logical(1L), "reachable")))
    out <- run(8L, retry = TRUE)
    expect_length(serial_urls, 8L)
    mode <- "error"
    out <- run(8L)
    expect_equal(sum(vapply(out, `[[`, logical(1L), "reachable")), 8L)
    expect_identical(serial_urls, paste0("https://example.org/", 4:6))
    expect_length(pools, 3L)
    expect_identical(cancelled, 8L)
    expect_identical(run(0L), stats::setNames(list(), character()))
})

# Missing catalog metadata falls back to the exact URL host. An unsuccessful
# sample must not certify other files on that host as usable OPeNDAP inputs.
test_that("sampled service checks handle missing metadata and partial failure", {
    docs <- data.table::rbindlist(lapply(seq_len(4L), function(i) {
        row <- query_result_test_file_docs(sprintf(
            "https://node.example.org/f%d.nc|application/netcdf|OPENDAP",
            i
        ))
        row$id <- paste0("id", i)
        row$data_node <- NA_character_
        row$variable_id <- NA_character_
        row
    }))
    docs$url[[4L]] <- character()
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    count <- 0L
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = function(urls, ...) {
            count <<- length(urls)
            data.table::data.table(
                url = urls,
                reachable = c(TRUE, FALSE),
                latency_ms = c(10, NA_real_),
                error = c(NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )
    out <- query_result__sample_service_nodes(result, sample_per_node = 2L)
    expect_identical(count, 2L)
    expect_false(any(out$reachable %in% TRUE))
    expect_identical(out$record_index, 1:4)
    expect_identical(out$error[4L], "Missing URL.")
    expect_equal(
        nrow(query_result__sample_service_nodes(result$slice(integer()))),
        0L
    )
    expect_error(
        query_result__sample_service_nodes(result, sample_per_node = 0L),
        "Must be >= 1"
    )
})

# Distinct endpoint hosts must remain distinct when catalog node metadata is
# absent, otherwise a failed host could invalidate a healthy host's files.
test_that("sampled service checks keep missing-metadata hosts separate", {
    docs <- data.table::rbindlist(lapply(
        c("healthy.example.org", "failed.example.org"),
        function(host) {
            row <- query_result_test_file_docs(paste0(
                "https://",
                host,
                "/f.nc|application/netcdf|OPENDAP"
            ))
            row$id <- host
            row$data_node <- NA_character_
            row$variable_id <- NA_character_
            row
        }
    ))
    result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    calls <- list()
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = function(urls, ...) {
            calls[[length(calls) + 1L]] <<- urls
            good <- grepl("healthy.example.org", urls, fixed = TRUE)
            data.table::data.table(
                url = urls,
                reachable = good,
                latency_ms = 1,
                error = data.table::fifelse(good, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )
    out <- query_result__sample_service_nodes(result)
    expect_length(calls, 2L)
    expect_identical(out$reachable, c(TRUE, FALSE))
    expect_identical(out$id, docs$id)
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

    plan <- files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "first"
    )
    expect_s3_class(plan, "data.table")
    expect_named(
        plan,
        c(
            "logical_file_id",
            "record_index",
            "file_key",
            "esgf_id",
            "dataset_id",
            "filename",
            "subdir",
            "checksum",
            "checksum_type",
            "size",
            "url",
            "service",
            "data_node",
            "priority",
            "probe_latency",
            "probe_throughput",
            "probe_cached",
            "node_success_count",
            "node_failure_count",
            "node_attempt_count",
            "node_success_rate",
            "node_avg_latency",
            "node_probe_success_count",
            "node_probe_failure_count",
            "node_cooldown_until",
            "node_is_cooling_down",
            "node_updated_at",
            "node_last_probe_at",
            "node_cooldown_rank"
        )
    )
    expect_identical(plan$logical_file_id, "master:master-file-1")
    expect_identical(plan$url, "https://example.org/file.nc")
    expect_identical(plan$checksum_type, "sha256")
    expect_identical(plan$priority, 1L)

    checksum_docs <- query_result_test_file_docs(
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    )
    checksum_docs$master_id <- NA_character_
    checksum_docs$tracking_id <- NA_character_
    checksum_result <- query_result_test_object(
        "File",
        checksum_docs,
        query_result_test_params("File")
    )
    checksum_plan <- checksum_result$download_plan(
        replica = "current",
        probe = FALSE
    )
    expect_identical(checksum_plan$logical_file_id, "checksum:abc:1:file.nc")

    expect_error(
        files$download(run = FALSE),
        "explicit `store` or persistent `downloader`"
    )
})

test_that("EsgResultFile$download_plan() deduplicates URL probes", {
    docs <- data.table::rbindlist(
        list(
            query_result_test_file_docs(
                "https://same.example.org/file.nc|application/netcdf|HTTPServer"
            ),
            query_result_test_file_docs(
                "https://same.example.org/file.nc|application/netcdf|HTTPServer"
            )
        ),
        fill = TRUE
    )
    docs$id <- c("file-1", "file-2")
    docs$data_node <- "same.example.org"
    docs$master_id <- c("master-file-1", "master-file-2")

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    calls <- 0L
    testthat::local_mocked_bindings(
        query_result__latency_url = function(
            url,
            timeout = 5,
            network_policy = NULL
        ) {
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
    docs <- query_result_test_file_docs(
        "https://cache.example.org/file.nc|application/netcdf|HTTPServer"
    )
    docs$data_node <- "cache.example.org"
    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
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
    docs <- data.table::rbindlist(
        list(
            query_result_test_file_docs(
                "https://slow.example.org/file.nc|application/netcdf|HTTPServer"
            ),
            query_result_test_file_docs(
                "https://fast.example.org/file.nc|application/netcdf|HTTPServer"
            )
        ),
        fill = TRUE
    )
    docs$id <- c("file-slow", "file-fast")
    docs$data_node <- c("slow.example.org", "fast.example.org")
    docs$master_id <- "master-file-shared"
    docs$tracking_id <- "hdl:21.14100/mock-file-shared"

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
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
    docs <- data.table::rbindlist(
        list(
            query_result_test_file_docs(
                "https://cooling.example.org/file.nc|application/netcdf|HTTPServer"
            ),
            query_result_test_file_docs(
                "https://ready.example.org/file.nc|application/netcdf|HTTPServer"
            )
        ),
        fill = TRUE
    )
    docs$id <- c("file-cooling", "file-ready")
    docs$data_node <- c("cooling.example.org", "ready.example.org")
    docs$master_id <- "master-file-cooldown"
    docs$tracking_id <- "hdl:21.14100/mock-file-cooldown"

    files <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
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
    expect_identical(
        plan$data_node,
        c("ready.example.org", "cooling.example.org")
    )
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
    testthat::local_mocked_bindings(
        menu = function(...) 2L,
        .package = "utils"
    )

    http_only <- query_result_test_object(
        "File",
        query_result_test_file_docs(
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        ),
        query_result_test_params("File")
    )
    expect_error(
        http_only$open_dataset(fallback = "error"),
        "OPeNDAP is not available"
    )
    expect_error(
        http_only$open_dataset(fallback = "ask"),
        "non-interactive|Operation cancelled"
    )
    expect_error(
        http_only$open_dataset(fallback = "auto"),
        "explicit `store` or `downloader`"
    )

    no_urls <- query_result_test_object(
        "File",
        query_result_test_file_docs(character()),
        query_result_test_params("File")
    )
    expect_error(
        no_urls$open_dataset(fallback = "auto"),
        "HTTPServer download URL"
    )

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
    testthat::local_mocked_bindings(
        menu = function(...) 2L,
        .package = "utils"
    )

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
            open = function(
                progress = getOption("epwshiftr.progress", interactive())
            ) {
                missing <- vapply(private$nc_handles, is.null, logical(1L))
                calls$opened[[length(calls$opened) + 1L]] <- self$target[
                    missing
                ]
                if (any(grepl("fail-opendap", self$target[missing]))) {
                    stop("remote boom", call. = FALSE)
                }
                private$nc_handles[missing] <- as.list(sprintf(
                    "handle:%s",
                    self$target[missing]
                ))
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
                paths <- vapply(
                    self$plan$url,
                    function(url) {
                        calls$downloads <- c(calls$downloads, url)
                        local_path <- tempfile(fileext = ".nc")
                        writeLines("netcdf", local_path)
                        local_path
                    },
                    character(1L)
                )
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
        query_result__repair_urls = function(
            result,
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            result
        },
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

    err <- tryCatch(
        file_result$open_dataset(fallback = "error"),
        error = function(e) e
    )
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "OPeNDAP is not available")
    expect_match(conditionMessage(err$parent), "remote boom")
    expect_error(
        file_result$open_dataset(fallback = "ask"),
        "non-interactive|Operation cancelled"
    )

    ds <- expect_s3_class(
        file_result$open_dataset(
            fallback = "auto",
            downloader = FakeDownloader$new()
        ),
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
    multi_files <- query_result_test_object(
        "File",
        multi_file_docs,
        query_result_test_params("File")
    )
    opened_before <- length(calls$opened)
    ds_all <- expect_s3_class(
        multi_files$open_dataset(fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(
        ds_all$target,
        c(
            "https://example.org/dods/file-1.nc",
            "https://example.org/dods/file-2.nc"
        )
    )
    expect_identical(
        ds_all$.__enclos_env__$private$context$selection$source_indices,
        c(1L, 2L)
    )
    expect_equal(length(calls$opened) - opened_before, 2L)

    ds_one <- expect_s3_class(
        multi_files$open_dataset(which = "file-2", fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(ds_one$target, "https://example.org/dods/file-2.nc")
    expect_identical(
        ds_one$.__enclos_env__$private$context$selection$source_indices,
        2L
    )

    ds_selected <- expect_s3_class(
        multi_files$open_dataset(which = 1:2, fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(
        ds_selected$target,
        c(
            "https://example.org/dods/file-1.nc",
            "https://example.org/dods/file-2.nc"
        )
    )
    expect_identical(
        ds_selected$.__enclos_env__$private$context$selection$source_indices,
        c(1L, 2L)
    )
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
    agg_result <- query_result_test_object(
        "Aggregation",
        agg_docs,
        query_result_test_params("Aggregation")
    )

    opened_before <- length(calls$opened)
    downloads_before <- calls$downloads
    expect_message(
        agg_ds <- expect_s3_class(
            agg_result$open_dataset(
                fallback = "auto",
                downloader = FakeDownloader$new()
            ),
            "FakeEsgDataset"
        ),
        "record 2 \\(id: file-2\\)"
    )
    expect_length(agg_ds$target, 2L)
    expect_identical(agg_ds$target[[1L]], "https://example.org/dods/file-1.nc")
    expect_true(file.exists(agg_ds$target[[2L]]))
    expect_identical(
        agg_ds$.__enclos_env__$private$context$selection$source_indices,
        c(1L, 2L)
    )
    expect_identical(
        tail(
            calls$downloads,
            length(calls$downloads) - length(downloads_before)
        ),
        "https://example.org/file-2.nc"
    )
    new_open_calls <- calls$opened[(opened_before + 1L):length(calls$opened)]
    expect_identical(new_open_calls[[1L]], "https://example.org/dods/file-1.nc")
    expect_identical(new_open_calls[[2L]], agg_ds$target[[2L]])

    agg_one_index <- expect_s3_class(
        agg_result$open_dataset(which = 1L, fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(agg_one_index$target, "https://example.org/dods/file-1.nc")
    expect_identical(
        agg_one_index$.__enclos_env__$private$context$selection$source_indices,
        1L
    )

    agg_one_id <- expect_s3_class(
        agg_result$open_dataset(which = "file-1", fallback = "auto"),
        "FakeEsgDataset"
    )
    expect_identical(agg_one_id$target, "https://example.org/dods/file-1.nc")
    expect_identical(
        agg_one_id$.__enclos_env__$private$context$selection$source_indices,
        1L
    )
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
    agg_fail_result <- query_result_test_object(
        "Aggregation",
        agg_fail_docs,
        query_result_test_params("Aggregation")
    )

    err <- tryCatch(
        agg_fail_result$open_dataset(fallback = "error"),
        error = function(e) e
    )
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "OPeNDAP is not available")
    expect_match(conditionMessage(err), "record 2 \\(id: file-2\\)")
    expect_match(conditionMessage(err$parent), "remote boom")
    expect_true(any(vapply(
        calls$closed,
        function(handles) {
            identical(
                handles,
                list("handle:https://example.org/dods/file-1.nc")
            )
        },
        logical(1L)
    )))

    downloads_before <- calls$downloads
    expect_message(
        agg_fail_ds <- expect_s3_class(
            agg_fail_result$open_dataset(
                fallback = "auto",
                downloader = FakeDownloader$new()
            ),
            "FakeEsgDataset"
        ),
        "record 2 \\(id: file-2\\)"
    )
    expect_identical(
        agg_fail_ds$target[[1L]],
        "https://example.org/dods/file-1.nc"
    )
    expect_true(file.exists(agg_fail_ds$target[[2L]]))
    expect_identical(
        tail(
            calls$downloads,
            length(calls$downloads) - length(downloads_before)
        ),
        "https://example.org/file-2.nc"
    )

    # A missing HTTP URL on the selected catalog row must not abort before the
    # fallback helper has a chance to recover it from a compatible replica.
    repairable_docs <- query_result_test_file_docs(character())
    repairable <- query_result_test_object(
        "File",
        repairable_docs,
        query_result_test_params("File")
    )
    repaired_docs <- repairable_docs
    repaired_docs$url <- I(list(
        "https://replica.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    repaired <- query_result_test_object(
        "File",
        repaired_docs,
        query_result_test_params("File")
    )
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(
            result,
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            repaired
        },
        .package = "epwshiftr"
    )
    repaired_ds <- expect_s3_class(
        repairable$open_dataset(
            fallback = "auto",
            downloader = FakeDownloader$new()
        ),
        "FakeEsgDataset"
    )
    expect_true(file.exists(repaired_ds$target))
    expect_identical(
        tail(calls$downloads, 1L),
        "https://replica.example.org/file.nc"
    )
})

test_that("EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() report open progress", {
    calls <- new.env(parent = emptyenv())
    calls$open_progress <- logical()
    calls$fallback_progress <- list()

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
            open = function(
                progress = getOption("epwshiftr.progress", interactive())
            ) {
                calls$open_progress <- c(calls$open_progress, progress)
                missing <- vapply(private$nc_handles, is.null, logical(1L))
                private$nc_handles[missing] <- as.list(sprintf(
                    "handle:%s",
                    self$target[missing]
                ))
                private$opened <- TRUE
                invisible(self)
            },
            close = function() {
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

    bars <- list()
    updates <- list()
    dones <- list()
    testthat::local_mocked_bindings(
        EsgDataset = FakeEsgDataset,
        query_result__http_fallback = function(
            result,
            indices,
            downloader,
            session_label = NULL,
            progress = TRUE,
            missing_message = "HTTPServer download URLs are missing."
        ) {
            calls$fallback_progress[[
                length(calls$fallback_progress) + 1L
            ]] <<- progress
            sprintf("/tmp/fallback-%d.nc", indices)
        },
        .package = "epwshiftr"
    )
    testthat::local_mocked_bindings(
        cli_progress_bar = function(name = NULL, total = NA, ...) {
            bars[[length(bars) + 1L]] <<- list(name = name, total = total)
            paste0("progress-", length(bars))
        },
        cli_progress_update = function(id = NULL, set = NULL, ...) {
            updates[[length(updates) + 1L]] <<- list(id = id, set = set)
        },
        cli_progress_done = function(id = NULL, result = "done", ...) {
            dones[[length(dones) + 1L]] <<- list(id = id, result = result)
        },
        .package = "cli"
    )

    file_docs <- data.frame(
        id = c("file-1", "file-2"),
        dataset_id = "dataset-1",
        size = c(1, 2),
        checksum = c("abc", "def"),
        checksum_type = "SHA256",
        instance_id = c("file-instance-1", "file-instance-2"),
        master_id = c("master-file-1", "master-file-2"),
        replica = FALSE,
        tracking_id = c("hdl:21.14100/mock-file-1", "hdl:21.14100/mock-file-2"),
        title = c("file-1.nc", "file-2.nc"),
        version = 20260101L,
        data_node = "example.org",
        check.names = FALSE
    )
    file_docs$url <- I(list(
        "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file-2.nc|application/netcdf|HTTPServer"
    ))
    file_result <- query_result_test_object(
        "File",
        file_docs,
        query_result_test_params("File")
    )

    ds <- expect_s3_class(
        file_result$open_dataset(
            fallback = "auto",
            downloader = list(),
            progress = TRUE
        ),
        "FakeEsgDataset"
    )
    expect_identical(
        ds$target,
        c("https://example.org/dods/file-1.nc", "/tmp/fallback-2.nc")
    )
    expect_equal(bars[[1L]], list(name = "Opening File records", total = 2L))
    expect_equal(vapply(updates[1:2], `[[`, integer(1L), "set"), c(1L, 2L))
    expect_equal(dones[[1L]], list(id = "progress-1", result = "done"))
    expect_true(calls$fallback_progress[[1L]])
    expect_true(all(!calls$open_progress))

    bars_before <- length(bars)
    fallback_before <- length(calls$fallback_progress)
    file_result$open_dataset(
        fallback = "auto",
        downloader = list(),
        progress = FALSE
    )
    expect_equal(length(bars), bars_before)
    expect_false(calls$fallback_progress[[fallback_before + 1L]])

    agg_docs <- file_docs
    agg_docs$url <- I(list(
        "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
        "https://example.org/dods/file-2.nc.html|application/netcdf|OPENDAP"
    ))
    agg_result <- query_result_test_object(
        "Aggregation",
        agg_docs,
        query_result_test_params("Aggregation")
    )
    agg_result$open_dataset(fallback = "auto", progress = TRUE)

    expect_equal(
        tail(bars, 1L)[[1L]],
        list(name = "Opening Aggregation records", total = 2L)
    )
    expect_equal(vapply(tail(updates, 2L), `[[`, integer(1L), "set"), c(1L, 2L))
    expect_equal(tail(dones, 1L)[[1L]]$result, "done")
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
        query_result_test_object(
            "Dataset",
            query_result_test_contract_dataset_docs(),
            params
        ),
        "EsgResultDataset"
    )

    expect_s3_class(datasets$to_data_table(), "data.table")
    expect_true(withVisible(datasets$to_data_table())$visible)
    expect_s3_class(
        datasets$to_data_table(c("source_id", "frequency")),
        "data.table"
    )
    expect_equal(
        names(datasets$to_data_table(c("source_id", "frequency"))),
        c("source_id", "frequency")
    )
    expect_type(datasets$to_data_table(formatted = TRUE)$size, "character")
    expect_equal(
        datasets$to_data_table(formatted = TRUE)$size,
        c("1.00 GiB", "2.00 GiB")
    )
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
        query_result_test_object(
            "Dataset",
            query_result_test_contract_dataset_docs(),
            params
        ),
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

    expect_type(datasets$size, "double")
    expect_length(datasets$size, 2L)
    expect_s3_class(datasets$size, "epwshiftr_bytes")
    expect_equal(format(datasets$size), c("1.00 GiB", "2.00 GiB"))

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
        query_result_test_object(
            "Dataset",
            query_result_test_contract_dataset_docs(),
            params
        ),
        "EsgResultDataset"
    )

    file <- tempfile(fileext = ".json")
    expect_type(datasets$save(file), "character")
    expect_true(file.exists(file))
    file_copied <- tempfile(fileext = ".json")
    expect_true(file.copy(file, file_copied))
    expect_snapshot_file(
        file_copied,
        "dataset.json",
        transform = transform_json
    )

    de <- expect_s3_class(
        query_result__new(EsgResultDataset)$load(file),
        "EsgResultDataset"
    )
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
        query_result_test_object(
            "Dataset",
            query_result_test_contract_dataset_docs(),
            params
        ),
        "EsgResultDataset"
    )

    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
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

    expect_s3_class(
        files_by_index <- datasets$collect(1, limit = 2, fields = "id"),
        "EsgResultFile"
    )
    expect_equal(files_by_index$count(), 2L)

    expect_s3_class(
        files_by_id <- datasets$collect(datasets$id[1], fields = "id"),
        "EsgResultFile"
    )
    expect_equal(files_by_id$count(), 2L)

    expect_s3_class(
        files <- datasets$collect(fields = "id", limit = 1),
        "EsgResultFile"
    )
    expect_true(
        all(
            c(
                EsgResultFile$private_fields$required_fields,
                "filename",
                "url_opendap",
                "url_download"
            ) %in%
                files$fields
        )
    )

    expect_s3_class(
        files <- datasets$collect(fields = "id", all = TRUE),
        "EsgResultFile"
    )
    expect_equal(files$count(), sum(datasets$number_of_files))
    expect_true(calls[[4L]]$all)

    expect_s3_class(
        datasets$collect(fields = "id", limit = 1, replica = FALSE),
        "EsgResultFile"
    )

    expect_s3_class(files <- datasets$collect(limit = 1), "EsgResultFile")
    expect_true(
        all(EsgResultFile$private_fields$required_fields %in% files$fields)
    )

    dataset_data_node <- datasets$data_node[[1]]
    expect_s3_class(
        datasets$collect(
            fields = "id",
            limit = 1,
            data_node = dataset_data_node
        ),
        "EsgResultFile"
    )

    expect_error(datasets$collect(bbox = "0,0,1,1"), "unsupported parameter")
    expect_error(datasets$collect(start = "2050"), "unsupported parameter")
    expect_error(datasets$collect(datetime_start = "2050"), "controlled")

    expect_s3_class(
        aggs <- datasets$collect(
            fields = "id",
            limit = 2,
            type = "Aggregation"
        ),
        "EsgResultAggregation"
    )
    expect_equal(aggs$count(), 2L)
    expect_true(all(
        EsgResultAggregation$private_fields$required_fields %in% aggs$fields
    ))
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
        query_result_test_object(
            "Dataset",
            query_result_test_contract_dataset_docs(),
            params
        ),
        "EsgResultDataset"
    )
    priv(datasets)$response$response$docs$number_of_aggregations <- c(2L, 0L)

    expect_snapshot(datasets$print(), transform = transform_print)

    truncated <- capture.output(
        datasets$print(n = 1L),
        type = "message"
    )
    expect_match(
        paste(cli::ansi_strip(truncated), collapse = "\n"),
        "# ... with 1 more item",
        fixed = TRUE
    )
})

test_that("EsgResult$print_contents() batches content output", {
    docs <- query_result_test_contract_dataset_docs()
    docs$number_of_aggregations <- c(2L, 0L)
    datasets <- query_result_test_object(
        "Dataset",
        docs,
        query_result_test_params("Dataset", limit = 2L)
    )

    cat_lines <- list()
    testthat::local_mocked_bindings(
        cli_rule = function(...) NULL,
        cat_line = function(...) {
            cat_lines[[length(cat_lines) + 1L]] <<- list(...)
        },
        .package = "cli"
    )

    priv(datasets)$print_contents("Dataset", 2L)

    expect_length(cat_lines, 1L)
    lines <- cat_lines[[1L]][[1L]]
    expect_length(lines, 4L)
    expect_identical(
        lines[c(1L, 3L)],
        sprintf("[%s] %s", lpad(seq_len(2L), "0"), docs$id)
    )
    expect_match(lines[[2L]], "2 Files", fixed = TRUE)
    expect_match(lines[[2L]], "2 Aggregations", fixed = TRUE)
    expect_match(lines[[4L]], "1 Files", fixed = TRUE)
    expect_match(lines[[4L]], "0 Aggregations", fixed = TRUE)
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
        query_result_test_object(
            "File",
            query_result_test_contract_file_docs()[1L, , drop = FALSE],
            params
        ),
        "EsgResultFile"
    )

    expect_s3_class(files$to_data_table(), "data.table")
    expect_true(withVisible(files$to_data_table())$visible)
    expect_s3_class(
        files$to_data_table(c("checksum", "checksum_type")),
        "data.table"
    )
    expect_equal(
        names(files$to_data_table(c("checksum", "checksum_type"))),
        c("checksum", "checksum_type")
    )
    expect_type(files$to_data_table(formatted = TRUE)$size, "character")
    expect_equal(files$to_data_table(formatted = TRUE)$size, "1.00 MiB")
    expect_s3_class(
        files$to_data_table(formatted = TRUE)$url[[1L]],
        "data.table"
    )
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
        query_result_test_object(
            "File",
            query_result_test_contract_file_docs()[1L, , drop = FALSE],
            params
        ),
        "EsgResultFile"
    )

    expect_type(files$id, "character")
    expect_length(files$id, 1L)

    expect_type(files$url, "list")
    expect_length(files$url, 1L)
    expect_s3_class(files$url[[1L]], "data.table")

    expect_type(files$size, "double")
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
        query_result_test_object(
            "File",
            query_result_test_contract_file_docs()[1L, , drop = FALSE],
            params
        ),
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
        query_result_test_object(
            "Aggregation",
            query_result_test_contract_aggregation_docs(),
            params
        ),
        "EsgResultAggregation"
    )

    expect_s3_class(aggs$to_data_table(), "data.table")
    expect_true(withVisible(aggs$to_data_table())$visible)
    expect_s3_class(aggs$to_data_table(c("url", "size")), "data.table")
    expect_equal(names(aggs$to_data_table(c("url", "size"))), c("url", "size"))
    expect_type(aggs$to_data_table(formatted = TRUE)$size, "character")
    expect_s3_class(
        aggs$to_data_table(formatted = TRUE)$url[[1L]],
        "data.table"
    )
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
        query_result_test_object(
            "Aggregation",
            query_result_test_contract_aggregation_docs(),
            params
        ),
        "EsgResultAggregation"
    )

    expect_type(aggs$id, "character")
    expect_length(aggs$id, 2L)

    expect_type(aggs$url, "list")
    expect_length(aggs$url, 2L)
    expect_s3_class(aggs$url[[1L]], "data.table")

    expect_type(aggs$size, "double")
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
        all(
            c(
                "data_node",
                "dataset_id",
                "id",
                "size",
                "title",
                "url",
                "url_opendap",
                "url_download"
            ) %in%
                aggs$fields
        )
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
        query_result_test_object(
            "Aggregation",
            query_result_test_contract_aggregation_docs(),
            params
        ),
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
    expect_snapshot(
        esg_result("aggregation")$print(),
        transform = transform_print
    )
})
# }}}

# Check exact rendered boundaries and verify that a singleton is never rejected
# solely because it exceeds the soft budget.
test_that("query value batching measures complete rendered URLs", {
    build_url <- function(x) {
        paste0("https://example.org/?id=", paste(x, collapse = ","))
    }
    boundary <- nchar(build_url(c("a", "b")), type = "bytes")
    expect_identical(
        query_result__query_value_batches(character(), build_url),
        list()
    )
    expect_identical(
        unname(query_result__query_value_batches(
            c("a", "b"),
            build_url,
            max_url_bytes = boundary
        )),
        list(c("a", "b"))
    )
    expect_identical(
        unname(query_result__query_value_batches(
            c("a", "b"),
            build_url,
            max_url_bytes = boundary - 1L
        )),
        list("a", "b")
    )
    expect_identical(
        unname(query_result__query_value_batches(
            c("a", "b", "a"),
            build_url,
            batch_size = 2L
        )),
        list(c("a", "b"), "a")
    )
    expect_identical(
        unname(query_result__query_value_batches(
            "long",
            build_url,
            max_url_bytes = 1L
        )),
        list("long")
    )
    expect_error(
        query_result__query_value_batches(NA_character_, build_url),
        "missing"
    )
    expect_error(query_result__query_value_batches(1:2, build_url), "character")
    expect_error(
        query_result__query_value_batches("a", build_url, batch_size = 0L),
        "Must be >= 1"
    )
    expect_error(
        query_result__query_value_batches("a", build_url, max_url_bytes = 0L),
        "Must be >= 1"
    )
})

# Realistic long IDs trigger multiple requests even below the former 50-ID
# threshold; mock only transport so the public collection/merge path still runs.
test_that("short Dataset lists with long IDs preserve limits and query receipts", {
    ids <- paste0(strrep("long.model.", 50L), seq_len(6L), "|node.example.org")
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = ids, size = 1),
        query_result_test_params("Dataset")
    )
    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            batch <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(
                ids = batch,
                limit = limit,
                dict_check = dict_check
            )
            take <- if (isTRUE(all)) {
                length(batch)
            } else {
                min(length(batch), as.integer(limit))
            }
            docs <- data.frame(
                id = paste0("file-", batch[seq_len(take)]),
                dataset_id = batch[seq_len(take)],
                size = 1
            )
            docs$url <- I(rep(
                list(
                    "https://example.org/file.nc|application/netcdf|HTTPServer"
                ),
                take
            ))
            params$fields(unique(c(
                query_param__value(params$fields()),
                required_fields
            )))
            list(
                response = query_result_test_response(docs),
                docs = docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        "https://example.org/search?batch=",
                        length(calls)
                    )
                )
            )
        },
        .package = "epwshiftr"
    )
    files <- datasets$collect(all = TRUE)
    expect_gt(length(calls), 1L)
    expect_identical(files$dataset_id, ids)
    expect_length(priv(files)$context$query_url, length(calls))
    expect_identical(
        query_param__value(priv(files)$parameter$state()$dataset_id),
        ids
    )
    expect_true(calls[[1L]]$dict_check)
    expect_false(any(vapply(calls[-1L], `[[`, logical(1L), "dict_check")))
    calls <- list()
    limited <- datasets$collect(limit = 4L)
    expect_equal(limited$count(), 4L)
    expect_identical(limited$dataset_id, ids[1:4])
    expect_equal(vapply(calls, `[[`, integer(1L), "limit"), c(4L, 2L))
})

# Both replica discovery entry points must shard long identities and retain
# their complete original facet and every transport receipt after merging.
test_that("replica collection batches long instance and master identities", {
    ids <- paste0(strrep("long.model.", 50L), seq_len(6L))
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File")
    )
    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = TRUE,
            limit = TRUE,
            constraints = TRUE
        ) {
            state <- params$params()
            facet <- if (!is.null(state$instance_id)) {
                "instance_id"
            } else {
                "master_id"
            }
            batch <- query_param__value(state[[facet]])
            calls[[length(calls) + 1L]] <<- batch
            docs <- data.frame(id = batch, size = 1)
            docs$url <- I(rep(
                list(
                    "https://example.org/file.nc|application/netcdf|HTTPServer"
                ),
                length(batch)
            ))
            list(
                response = query_result_test_response(docs),
                docs = docs,
                parameter = params,
                context = list(
                    query_url = paste0(
                        "https://example.org/search?batch=",
                        length(calls)
                    )
                )
            )
        },
        .package = "epwshiftr"
    )
    for (mode in c("instance", "identity_master", "master")) {
        calls <- list()
        identity <- data.table::data.table(
            instance_id = ids,
            master_id = ids,
            has_instance = mode == "instance",
            has_master_version = mode != "instance"
        )
        out <- if (mode == "master") {
            query_result__collect_master(result, ids, "File")
        } else {
            query_result__collect_identity(result, identity, "File")
        }
        expect_gt(length(calls), 1L)
        expect_identical(unlist(calls, use.names = FALSE), ids)
        expect_identical(out$id, ids)
        expect_length(priv(out)$context$query_url, length(calls))
        facet <- if (mode == "instance") "instance_id" else "master_id"
        expect_identical(
            query_param__value(priv(out)$parameter$params()[[facet]]),
            ids
        )
        expect_true(all(
            vapply(
                calls,
                function(x) {
                    nchar(
                        utils::URLencode(
                            paste(x, collapse = ","),
                            reserved = TRUE
                        ),
                        type = "bytes"
                    )
                },
                integer(1L)
            ) <=
                QUERY_RESULT_COLLECT_MAX_URL_BYTES
        ))
    }
})

# Candidate identity compares checksums only when their algorithms agree.
test_that("raw candidate plans retain different digest algorithms without merging contradictions", {
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$checksum_type <- c("SHA256", "MD5")
    docs$checksum <- c("sha-value", "md5-value")
    docs$url <- I(list(
        "https://one.example/a.nc|application/netcdf|OPENDAP",
        "https://two.example/a.nc|application/netcdf|OPENDAP"
    ))
    files <- query_result_test_object("File", docs)
    plan <- files$access_plan(expand = FALSE)
    expect_equal(nrow(plan), 2L)
    expect_length(unique(plan$source_key), 1L)
    docs$checksum_type <- c("SHA256", "SHA256")
    expect_error(
        query_result_test_object("File", docs)$access_plan(expand = FALSE),
        "Conflicting"
    )
})

# Interleaved catalog rows retain the first appearance of compatible file groups,
# including sparse records and replicas with independently declared digest types.
test_that("raw candidate plans preserve interleaved file and version ordering", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 7L), ]
    docs$id <- paste0("row-", seq_len(nrow(docs)))
    docs$instance_id <- c("a", "b", "a", "c", "a", "b", "a")
    docs$master_id <- docs$instance_id
    docs$version <- c(1L, 1L, 2L, 1L, 1L, NA_integer_, NA_integer_)
    docs$checksum_type <- c(
        "SHA256",
        "SHA256",
        "SHA256",
        "SHA256",
        "MD5",
        NA,
        NA
    )
    docs$checksum <- c("a-sha", "b-sha", "a2-sha", "c-sha", "a-md5", NA, NA)
    docs$size <- c(1, 1, 2, 1, 1, NA_real_, NA_real_)
    docs$url <- I(lapply(seq_len(nrow(docs)), function(i) {
        paste0("https://node", i, ".example/file.nc|application/netcdf|OPENDAP")
    }))
    files <- query_result_test_object("File", docs)
    plan <- files$access_plan(expand = FALSE)
    expect_identical(plan$record_index, seq_len(7L))
    expect_identical(plan$content_group, c(1L, 2L, 3L, 4L, 1L, 2L, 1L))
    expect_identical(
        plan$source_key[c(1L, 2L, 3L, 4L, 1L, 2L, 1L)],
        plan$source_key
    )
    expect_equal(
        nrow(query_result_test_object("File", docs[FALSE, ])$access_plan(
            expand = FALSE
        )),
        0L
    )
})

# Sparse metadata must not make contradictory content look like one file.
test_that("raw candidate plans reject a contradiction after an interleaved sparse replica", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 4L), ]
    docs$instance_id <- c("a", "b", "a", "a")
    docs$master_id <- docs$instance_id
    docs$checksum <- c("first", "other", NA_character_, "conflicting")
    docs$url <- I(lapply(seq_len(nrow(docs)), function(i) {
        paste0("https://node", i, ".example/file.nc|application/netcdf|OPENDAP")
    }))
    expect_error(
        query_result_test_object("File", docs)$access_plan(expand = FALSE),
        "Conflicting replica content"
    )
})

# A real DAP2 server and real source workers exercise transport faults without
# substituting package scheduling, native selection or durable storage.
test_that("raw subsets switch failed replicas and repair only missing native corners", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-subsets-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$data_node <- c("first", "second")
    docs$url <- I(lapply(c("bad.nc", "fast.nc"), function(name) {
        paste0(server$url(), name, "#show=url|application/netcdf|OPENDAP")
    }))
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = c("first-day", "fourth-day"),
        case_id = "original",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04")
    )
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 2L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_identical(
        attempts$data_node[attempts$native_values > 0],
        c("first", rep("second", 4L))
    )
    expect_equal(sum(!attempts$success), 1L)
    expect_equal(
        attempts$native_values[attempts$native_values > 0],
        c(4, rep(1, 4L))
    )
    saved <- read_subset(dest, case_id = "original")
    expect_equal(sort(saved$values$value), c(1:4, 13:16))
    expect_equal(nrow(saved$neighborhoods), 8L)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(sort(unique(saved$values$cf_day)), c(1L, 4L))
    before <- rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    )
    # DAP request evidence excludes hidden libnetcdf whole-variable prefetch.
    traffic <- jsonlite::fromJSON(before)
    weather <- traffic[grepl("[.]dods tas", traffic)]
    expect_true(all(grepl(
        "tas%5b[03](:[03])?%5d%5b[01](:[01])?%5d%5b[01](:[01])?%5d$",
        weather
    )))
    expect_length(weather, 6L)
    requests$request_id <- paste0(requests$request_id, "-linked")
    requests$case_id <- "linked"
    cached <- files$fetch_subset(
        requests,
        dest,
        workers = 2L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(cached$complete)
    expect_equal(nrow(read_subset(dest, case_id = "linked")$values), 8L)
    after <- rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    )
    expect_identical(after, before)
    blocks <- subset_status(dest, "blocks")
    writeBin(as.raw(0), blocks$path[[1L]])
    repaired <- files$fetch_subset(
        requests,
        dest,
        workers = 2L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(repaired$complete)
    expect_equal(
        sort(read_subset(dest, case_id = "linked")$values$value),
        c(1:4, 13:16)
    )
    repaired_attempts <- subset_status(dest, "attempts")
    expect_equal(
        sum(repaired_attempts$native_values) - sum(attempts$native_values),
        4
    )
    expect_error(files$fetch_subset(requests[0L, ], dest), "rows")
    unmatched <- requests[1L, ]
    unmatched$variable_id <- "rsds"
    missing <- files$fetch_subset(unmatched, dest, discover = FALSE)
    expect_false(missing$complete)
    expect_equal(nrow(missing$unmatched_requests), 1L)
})

# A fresh invocation ignores older cache while retaining every block committed
# by its own failed endpoint. Native values and endpoint reads are independent
# observations of the public acquisition and offline reader contracts.
test_that("raw subset fresh invocation retains committed replica progress", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-fresh-invocation-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$data_node <- c("first", "second")
    docs$url <- I(lapply(c("bad-bounds.nc", "fast-bounds.nc"), function(name) {
        paste0(server$url(), name, "|application/netcdf|OPENDAP")
    }))
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = c("one", "four"),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04")
    )
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE,
        resume = FALSE
    )
    expect_true(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_equal(sum(attempts$native_values), 8)
    expect_identical(
        attempts$data_node[attempts$native_values > 0],
        c("first", rep("second", 4L))
    )
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_identical(unique(raw$values$cf_calendar), "360_day")
    expect_equal(sort(unique(raw$values$bound_start_value)), c(0, 3))
    # A separate fresh invocation must refresh those now pre-existing blocks.
    refreshed <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE,
        resume = FALSE
    )
    expect_true(refreshed$complete)
    expect_equal(sum(subset_status(dest, "attempts")$native_values), 16)
    expect_equal(sort(read_subset(dest)$values$value), c(1:4, 13:16))
})

# Native interval identity from this invocation must survive a failed Replica
# even when the caller explicitly ignores coverage from previous invocations.
test_that("raw subset fresh invocation preserves native bounds across replicas", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-fresh-bounds-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 3L), ]
    docs$variable_id <- "tas"
    docs$data_node <- c("partial", "wrong-bounds", "consistent")
    docs$url <- I(lapply(
        c("bad-bounds.nc", "wrong-bounds.nc", "fast-bounds.nc"),
        function(name) {
            paste0(server$url(), name, "|application/netcdf|OPENDAP")
        }
    ))
    requests <- data.frame(
        request_id = c("one", "four"),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04")
    )
    result <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE,
        resume = FALSE
    )
    expect_true(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_true(any(
        attempts$data_node == "wrong-bounds" &
            attempts$error_kind == "source_metadata"
    ))
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_equal(sort(unique(raw$values$bound_start_value)), c(0, 3))
    expect_equal(sum(attempts$native_values), 8)
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(
            paste0(server$url(), "requests")
        )$content
    ))
    expect_false(any(grepl("wrong-bounds.nc.dods tas", traffic, fixed = TRUE)))
})

# An earlier contiguous block covers both requested days, but refreshing just
# one day cannot authorize that older block during directory completion.
test_that("raw subset fresh invocation excludes old blocks across directory phases", {
    skip_if_not_installed("webfakes")
    local_cache_mode("off")
    app <- subset_test__dap_app()
    # One initial contiguous read succeeds; the fresh call commits one day,
    # then all subsequent weather reads fail, including the recursive phase.
    app$locals$weather <- -1L
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "bad.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    docs <- docs[c(1L, 1L), ]
    docs$id[[2L]] <- "file-2"
    docs$instance_id[[2L]] <- "file-instance-2"
    docs$master_id[[2L]] <- "master-file-2"
    docs$url[[2L]] <- paste0(server$url(), "fast.nc|application/netcdf|OPENDAP")
    index_app <- webfakes::new_app()
    index_app$locals$docs <- docs
    index_app$get("/esg-search/search", function(req, res, locals) {
        offset <- if (is.null(req$query$offset)) {
            0L
        } else {
            as.integer(req$query$offset)
        }
        response <- list(
            responseHeader = list(status = 0L),
            response = list(
                numFound = 2L,
                start = offset,
                docs = list(as.list(locals$docs[offset + 1L, ]))
            )
        )
        res$set_header("Content-Type", "application/json")$send(
            jsonlite::toJSON(response, auto_unbox = TRUE)
        )
    })
    index <- webfakes::new_app_process(index_app)
    on.exit(index$stop(), add = TRUE)
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", index$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs[1L, ], num_found = 2L)
    )
    dest <- tempfile("raw-fresh-directory-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    requests <- data.frame(
        request_id = "initial",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-04"
    )
    expect_true(
        partial$slice(1L)$fetch_subset(
            requests,
            dest,
            workers = 1L,
            discover = FALSE,
            retries = 0L
        )$complete
    )
    expect_equal(sum(subset_status(dest, "attempts")$native_values), 16)
    requests <- requests[c(1L, 1L), ]
    requests$request_id <- c("one", "four")
    requests$time_start <- requests$time_stop <- c("2060-01-01", "2060-01-04")
    result <- partial$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        resume = FALSE
    )
    expect_true(result$directory_complete)
    expect_false(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_equal(sum(attempts$native_values), 28)
    # Both phases must try the unresolved source instead of accepting its old
    # sixteen-value block as freshly acquired coverage.
    expect_gte(
        sum(!attempts$success & grepl("bad.nc", attempts$url, fixed = TRUE)),
        2L
    )
})

# Measured reads affect the next public acquisition; clients on another
# network start from catalogue order instead of inheriting local latency.
test_that("raw subset acquisition ranks actual node reads within the current client", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-ranking-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    acquire <- function(identity, endpoints, nodes, network = "local") {
        docs <- query_result_test_file_docs()
        docs <- docs[rep(1L, length(endpoints)), ]
        docs$master_id <- identity
        docs$instance_id <- identity
        docs$variable_id <- "tas"
        docs$data_node <- nodes
        docs$url <- I(lapply(endpoints, function(endpoint) {
            paste0(server$url(), endpoint, "|application/netcdf|OPENDAP")
        }))
        query_result_test_object("File", docs)$fetch_subset(
            requests,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE,
            network_id = network
        )
    }
    expect_true(acquire("learn-slow", "slow.nc", "slow")$complete)
    expect_true(acquire("learn-fast", "fast.nc", "fast")$complete)
    expect_true(
        acquire("ranked", c("slow.nc", "fast.nc"), c("slow", "fast"))$complete
    )
    attempts <- subset_status(dest, "attempts")
    expect_identical(
        attempts$data_node[attempts$native_values > 0],
        c("slow", "fast", "fast")
    )
    expect_true(
        acquire(
            "new-network",
            c("slow.nc", "fast.nc"),
            c("slow", "fast"),
            "other"
        )$complete
    )
    expect_identical(
        tail(subset_status(dest, "attempts")$data_node, 1L),
        "slow"
    )
})

# Native source identity is validated before saved points are reused on a
# replacement endpoint; an incompatible candidate must remain an explicit gap.
test_that("raw subset recovery rejects incompatible replica metadata", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-identity-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$data_node <- c("partial", "wrong-units")
    docs$url <- I(lapply(c("bad.nc", "wrong.nc"), function(name) {
        paste0(server$url(), name, "|application/netcdf|OPENDAP")
    }))
    requests <- data.frame(
        request_id = c("one", "four"),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04")
    )
    result <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_false(result$complete)
    attempts <- subset_status(dest, "attempts")
    incompatible <- attempts[attempts$data_node == "wrong-units", ]
    expect_true(all(!incompatible$success))
    expect_identical(incompatible$error_kind, "source_metadata")
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), 1:4)
    expect_true(nrow(raw$gaps$incomplete_sources) > 0L)
})

# A blocked remote weather read retires its own reader; an unrelated source
# uses another slot and remains available through the same public result.
test_that("raw subset timeouts release workers while unrelated sources finish", {
    skip_if_not_installed("webfakes")
    slow <- webfakes::new_app_process(subset_test__dap_app())
    healthy <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(slow$stop(), add = TRUE)
    on.exit(healthy$stop(), add = TRUE)
    dest <- tempfile("raw-timeout-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$master_id <- docs$instance_id <- c("blocked-source", "healthy-source")
    docs$variable_id <- "tas"
    docs$data_node <- c("blocked", "healthy")
    docs$url <- I(list(
        paste0(slow$url(), "hang.nc|application/netcdf|OPENDAP"),
        paste0(healthy$url(), "fast.nc|application/netcdf|OPENDAP")
    ))
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    before <- ps::ps_children(ps::ps_handle())
    owned_before <- vapply(before, ps::ps_pid, integer(1L))
    result <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 2L,
        timeout = 2,
        retries = 0L,
        discover = FALSE
    )
    expect_false(result$complete)
    expect_equal(sort(result$jobs$status), c("done", "failed"))
    attempts <- subset_status(dest, "attempts")
    expect_true(any(
        attempts$data_node == "blocked" & attempts$error_kind == "transport"
    ))
    expect_equal(sort(read_subset(dest)$values$value), 1:4)
    # The captured parent owns the readers; closing acquisition must leave no
    # newly launched child behind, including a reader stuck in native code.
    after <- ps::ps_children(ps::ps_handle())
    expect_true(all(vapply(after, ps::ps_pid, integer(1L)) %in% owned_before))
})

# Saved-block progress keeps a healthy native reader alive for longer than
# the stall timeout, without extending an idle or failed reader indefinitely.
test_that("raw subset block progress extends a healthy reader beyond the stall timeout", {
    # Four two-second reads exceed the five-second stall budget. This keeps
    # startup/instrumentation jitter out of the assertion while still failing
    # if completed-block progress does not renew the reader deadline.
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-progress-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "progress.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    requests <- data.frame(
        request_id = paste0("day-", 1:4),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = sprintf("2060-01-%02d", c(1, 3, 5, 7)),
        time_stop = sprintf("2060-01-%02d", c(1, 3, 5, 7))
    )
    result <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 1L,
        timeout = 5,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    expect_equal(
        sort(read_subset(dest)$values$value),
        c(1:4, 9:12, 17:20, 25:28)
    )
    attempts <- subset_status(dest, "attempts")
    expect_true(all(attempts$success))
    expect_true(sum(attempts$elapsed_seconds[attempts$native_values > 0]) > 5)
})

# Interrupt the external filesystem publication boundary after its checked
# receipt is durable; the next public call must adopt the existing data.
test_that("raw subset restart adopts published blocks before manifest registration", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-publication-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    local({
        rename <- base::file.rename
        interrupted <- FALSE
        local_mocked_bindings(
            file.rename = function(from, to) {
                result <- rename(from, to)
                if (
                    !interrupted &&
                        isTRUE(result) &&
                        grepl("/pending-[^/]+[.]rds[.]json$", to)
                ) {
                    interrupted <<- TRUE
                    stop("controlled filesystem interruption", call. = FALSE)
                }
                result
            },
            .package = "base"
        )
        expect_error(
            files$fetch_subset(
                requests,
                dest,
                workers = 1L,
                retries = 0L,
                discover = FALSE
            ),
            "controlled filesystem interruption"
        )
    })
    before <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(
            paste0(server$url(), "requests")
        )$content
    ))
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    expect_equal(sort(read_subset(dest)$values$value), 1:4)
    after <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(
            paste0(server$url(), "requests")
        )$content
    ))
    expect_equal(
        sum(grepl("[.]dods tas", after)),
        sum(grepl("[.]dods tas", before))
    )
    expect_equal(sum(subset_status(dest, "attempts")$native_values), 0)
})

# Repeated publication must not remove a receipt already needed by committed
# blocks, even when the final source result repeats its first block's demand.
test_that("raw subset receipt publication preserves committed demand coverage", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-receipt-publication-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = "one",
        case_id = "original",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    receipt <- NULL
    interrupted <- FALSE
    local({
        rename <- base::file.rename
        local_mocked_bindings(
            file.rename = function(from, to) {
                if (grepl("/demand-[^/]+[.]rds$", to)) {
                    if (identical(to, receipt)) {
                        interrupted <<- TRUE
                        stop(
                            "controlled repeated receipt publication",
                            call. = FALSE
                        )
                    }
                    receipt <<- to
                }
                rename(from, to)
            },
            .package = "base"
        )
        tryCatch(
            files$fetch_subset(requests, dest, workers = 1L, discover = FALSE),
            error = function(e) {
                if (
                    !grepl("controlled repeated receipt", conditionMessage(e))
                ) {
                    stop(e)
                }
            }
        )
    })
    expect_true(file.exists(receipt))
    expect_equal(sort(read_subset(dest)$values$value), 1:4)
    expect_equal(nrow(read_subset(dest)$gaps$incomplete_requests), 0L)
    expect_false(interrupted)
    before <- subset_status(dest, "attempts")
    requests$request_id <- "linked"
    requests$case_id <- "linked"
    expect_true(
        files$fetch_subset(
            requests,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    expect_identical(subset_status(dest, "attempts"), before)
    saved <- read_subset(dest, case_id = "linked")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$links$request_id), "linked")
    # Manifest coverage verifies the immutable payload directly. Losing an
    # auxiliary cache sidecar cannot authorize replacing that committed file.
    unlink(paste0(receipt, ".json"))
    local({
        rename <- base::file.rename
        local_mocked_bindings(
            file.rename = function(from, to) {
                if (identical(to, receipt)) {
                    stop("committed receipt replacement")
                }
                rename(from, to)
            },
            .package = "base"
        )
        expect_true(
            files$fetch_subset(
                requests,
                dest,
                workers = 1L,
                discover = FALSE,
                resume = FALSE
            )$complete
        )
    })
    expect_equal(sort(read_subset(dest, case_id = "linked")$values$value), 1:4)
})

# A process identity inspection fault happens after a real worker is running;
# failure of the public acquisition must still retire its exact owned profile.
test_that("raw subset worker bootstrap failure retires its owned profile", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-bootstrap-failure-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    handle <- NULL
    profile <- NULL
    on.exit(
        {
            if (!is.null(handle) && ps::ps_is_running(handle)) {
                ps::ps_kill(handle)
            }
            if (!is.null(profile)) mirai::daemons(0L, .compute = profile)
        },
        add = TRUE
    )
    local({
        daemons <- mirai::daemons
        # Record only the profile created by the public call, forwarding every
        # real launch and shutdown unchanged to the installed mirai runtime.
        local_mocked_bindings(
            daemons = function(n, ..., .compute = NULL) {
                if (n > 0L && !is.null(.compute)) {
                    profile <<- .compute
                }
                daemons(n, ..., .compute = .compute)
            },
            .package = "mirai"
        )
        command <- ps::ps_cmdline
        local_mocked_bindings(
            ps_cmdline = function(p = ps::ps_handle()) {
                value <- command(p)
                if (is.null(handle) && ps::ps_pid(p) != Sys.getpid()) {
                    handle <<- p
                    stop("controlled bootstrap identity failure", call. = FALSE)
                }
                value
            },
            .package = "ps"
        )
        expect_error(
            files$fetch_subset(
                requests,
                dest,
                workers = 1L,
                discover = FALSE
            ),
            "controlled bootstrap identity failure"
        )
    })
    expect_false(is.null(handle))
    deadline <- proc.time()[["elapsed"]] + 5
    while (ps::ps_is_running(handle) && proc.time()[["elapsed"]] < deadline) {
        Sys.sleep(.01)
    }
    expect_false(ps::ps_is_running(handle))
    expect_identical(mirai::status(.compute = profile)$connections, 0L)
    expect_identical(mirai::status(.compute = profile)$daemons, 0L)
    expect_equal(nrow(subset_status(dest, "attempts")), 0L)
    # Clean the actual failing fixture on the old implementation as well, so
    # observing RED does not leave a worker owned by this test behind.
    if (ps::ps_is_running(handle)) ps::ps_kill(handle)
})

# Historical demand receipts keep gaps visible independently of the latest
# successful request for another window in the same physical source.
test_that("raw subset coverage follows each case and rejects empty native windows", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-demand-history-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "A",
        case_id = "A",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    a <- request
    unlink(subset_status(dest, "blocks")$path[[1L]])
    request$request_id <- request$case_id <- "B"
    request$time_start <- request$time_stop <- "2060-01-02"
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_equal(sort(read_subset(dest, case_id = "B")$values$value), 5:8)
    historical <- read_subset(dest, case_id = "A")
    expect_equal(nrow(historical$values), 0L)
    expect_true(nrow(historical$gaps$incomplete_sources) > 0L)
    request$request_id <- request$case_id <- "C"
    request$time_start <- request$time_stop <- "2060-01-20"
    expect_false(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_true(
        nrow(read_subset(dest, case_id = "C")$gaps$incomplete_sources) > 0L
    )
    before <- sum(subset_status(dest, "attempts")$native_values)
    expect_true(
        files$fetch_subset(
            a,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_equal(sum(subset_status(dest, "attempts")$native_values) - before, 4)
    expect_equal(sort(read_subset(dest, case_id = "A")$values$value), 1:4)
    expect_equal(
        nrow(read_subset(dest, case_id = "B")$gaps$incomplete_sources),
        0L
    )
    # A local selection receipt is required independently of weather payload
    # hashes. Its loss must expose a gap, then recover without weather rereads.
    for (path in list.files(
        file.path(dest, "subsets"),
        pattern = "^demand-.*[.]rds$",
        full.names = TRUE
    )) {
        writeBin(as.raw(0), path)
    }
    expect_true(
        nrow(read_subset(dest, case_id = "A")$gaps$incomplete_sources) > 0L
    )
    before <- sum(subset_status(dest, "attempts")$native_values)
    expect_true(
        files$fetch_subset(
            a,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_equal(sum(subset_status(dest, "attempts")$native_values), before)
})

# Local block hashes cannot override contradictory published immutable
# content evidence supplied by a later catalogue result for the same version.
test_that("raw subset reuse rejects contradictory published content evidence", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-content-evidence-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    request <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    expect_true(
        query_result_test_object("File", docs)$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    docs$checksum <- "contradictory-published-sha256"
    expect_error(
        query_result_test_object("File", docs)$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        ),
        "Conflicting published content"
    )
    expect_equal(sort(read_subset(dest)$values$value), 1:4)
})

# Identical timestamps can still describe different intervals. Read only
# requested native bounds and reject a replacement before mixing its values.
test_that("raw subset fallback rejects incompatible native time bounds", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-bounds-identity-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$data_node <- c("partial", "wrong-bounds")
    docs$url <- I(lapply(c("bad-bounds.nc", "wrong-bounds.nc"), function(name) {
        paste0(server$url(), name, "|application/netcdf|OPENDAP")
    }))
    request <- data.frame(
        request_id = c("one", "four"),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04")
    )
    result <- query_result_test_object("File", docs)$fetch_subset(
        request,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_false(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_true(any(
        attempts$data_node == "wrong-bounds" &
            attempts$error_kind == "source_metadata"
    ))
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), 1:4)
    expect_equal(unique(raw$values$bound_start_value), 0)
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(
            paste0(server$url(), "requests")
        )$content
    ))
    expect_false(any(grepl("wrong-bounds.nc.dods tas", traffic, fixed = TRUE)))
    bounds_reads <- traffic[grepl("[.]dods time_bnds", traffic)]
    expect_true(length(bounds_reads) > 0L)
    expect_false(any(grepl("time_bnds%5b0:7%5d", bounds_reads, fixed = TRUE)))
})

# Many distinct target coordinates share the same enclosing cell. Reusing
# native coverage must preserve every city mapping without repeated weather I/O.
test_that("raw subset acquisition shares native coverage across many overlapping cities", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-many-cities-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    requests <- data.frame(
        request_id = paste0("request-", 1:80),
        site_id = rep(paste0("city-", 1:40), each = 2L),
        lon = rep(seq(10.1, 10.9, length.out = 40L), each = 2L),
        lat = .25,
        time_start = rep(c("2060-01-01", "2060-01-04"), 40L),
        time_stop = rep(c("2060-01-01", "2060-01-04"), 40L)
    )
    result <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_equal(nrow(raw$neighborhoods), 320L)
    expect_equal(sum(subset_status(dest, "attempts")$native_values), 8)
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(
            paste0(server$url(), "requests")
        )$content
    ))
    expect_equal(sum(grepl("[.]dods tas", traffic)), 2L)
})

# An original unsliced File query can finish its pages after known weather
# reads start. The frozen completion is reused by the same original input.
test_that("raw subset directory completion preserves explicit selection and local resume", {
    skip_if_not_installed("webfakes")
    local_cache_mode("off")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    docs <- docs[c(1L, 1L), ]
    docs$id[[2L]] <- "file-2"
    docs$instance_id[[2L]] <- "file-instance-2"
    docs$master_id[[2L]] <- "master-file-2"
    index_app <- webfakes::new_app()
    index_app$locals$pages <- lapply(seq_len(2L), function(i) {
        jsonlite::toJSON(
            esgf_test__response(docs[i, ], num_found = 2L),
            auto_unbox = TRUE
        )
    })
    changed <- docs
    changed$id <- c("different-1", "different-2")
    changed$instance_id <- c("different-instance-1", "different-instance-2")
    changed$master_id <- c("different-master-1", "different-master-2")
    index_app$locals$changed_pages <- lapply(seq_len(2L), function(i) {
        jsonlite::toJSON(
            esgf_test__response(changed[i, ], num_found = 2L),
            auto_unbox = TRUE
        )
    })
    conflict <- docs
    conflict$checksum <- "conflicting-published-content"
    index_app$locals$conflict_pages <- lapply(seq_len(2L), function(i) {
        jsonlite::toJSON(
            esgf_test__response(conflict[i, ], num_found = 2L),
            auto_unbox = TRUE
        )
    })
    index_app$locals$conflict <- FALSE
    index_app$locals$changed <- FALSE
    index_app$locals$queries <- list()
    index_app$locals$weather <- paste0(server$url(), "requests")
    index_app$get("/esg-search/search", function(req, res, locals) {
        weather <- rawToChar(curl::curl_fetch_memory(locals$weather)$content)
        locals$queries[[length(locals$queries) + 1L]] <- list(
            query = req$query,
            known_read_started = grepl("dods tas", weather)
        )
        offset <- if (is.null(req$query$offset)) {
            0L
        } else {
            as.integer(req$query$offset)
        }
        pages <- if (locals$conflict) {
            locals$conflict_pages
        } else if (locals$changed) {
            locals$changed_pages
        } else {
            locals$pages
        }
        res$set_header("Content-Type", "application/json")$send(pages[[
            offset + 1L
        ]])
    })
    index_app$get("/conflict", function(req, res, locals) {
        locals$conflict <- TRUE
        res$send("ok")
    })
    index_app$get("/change", function(req, res, locals) {
        locals$changed <- TRUE
        res$send("ok")
    })
    index_app$get("/queries", function(req, res, locals) {
        res$send_json(locals$queries)
    })
    index <- webfakes::new_app_process(index_app)
    on.exit(index$stop(), add = TRUE)
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", index$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs[1L, ], num_found = 2L)
    )
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    dest <- tempfile("raw-directory-completion-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    first <- partial$fetch_subset(requests, dest, workers = 1L, retries = 0L)
    expect_true(first$complete)
    expect_true(first$directory_complete)
    expect_equal(length(unique(read_subset(dest)$links$source_key)), 2L)
    expect_equal(nrow(read_subset(dest)$values), 8L)
    queries <- rawToChar(
        curl::curl_fetch_memory(paste0(index$url(), "queries"))$content
    )
    evidence <- jsonlite::fromJSON(queries)
    expect_equal(nrow(evidence), 2L)
    expect_true(all(unlist(evidence$known_read_started)))
    expect_true(all(evidence$query$type == "File"))
    expect_true(all(evidence$query$project == "CMIP6"))
    before <- subset_status(dest, "attempts")
    expect_true(
        partial$fetch_subset(requests, dest, workers = 1L)$directory_complete
    )
    expect_identical(subset_status(dest, "attempts"), before)
    expect_identical(
        rawToChar(
            curl::curl_fetch_memory(
                paste0(index$url(), "queries")
            )$content
        ),
        queries
    )
    expect_equal(length(unique(read_subset(dest)$links$source_key)), 2L)
    # Even selecting every currently held row explicitly must freeze science
    # inputs; a complete catalog and an old contextless saved partial also freeze.
    for (selected in list(
        partial$slice(1L),
        query_result_test_object("File", docs)$slice(1L)
    )) {
        selected_dest <- tempfile("raw-directory-selected-")
        on.exit(unlink(selected_dest, recursive = TRUE), add = TRUE)
        selected$fetch_subset(requests, selected_dest, workers = 1L)
        expect_equal(
            length(unique(read_subset(selected_dest)$links$source_key)),
            1L
        )
        expect_identical(
            rawToChar(
                curl::curl_fetch_memory(
                    paste0(index$url(), "queries")
                )$content
            ),
            queries
        )
    }
    saved_path <- tempfile(fileext = ".json")
    on.exit(unlink(saved_path), add = TRUE)
    partial$save(saved_path)
    old <- jsonlite::read_json(saved_path, simplifyVector = FALSE)
    old$context$directory_collection <- NULL
    jsonlite::write_json(old, saved_path, auto_unbox = TRUE, null = "null")
    old_partial <- esg_result("file")$load(saved_path)
    old_dest <- tempfile("raw-directory-old-context-")
    on.exit(unlink(old_dest, recursive = TRUE), add = TRUE)
    expect_false(
        old_partial$fetch_subset(
            requests,
            old_dest,
            workers = 1L
        )$directory_complete
    )
    expect_equal(length(unique(read_subset(old_dest)$links$source_key)), 1L)
    expect_identical(
        rawToChar(
            curl::curl_fetch_memory(
                paste0(index$url(), "queries")
            )$content
        ),
        queries
    )
    # resume=FALSE refreshes each source once across the two phases, including
    # newly discovered Files that already have durable blocks from another call.
    refresh_dest <- tempfile("raw-directory-refresh-")
    on.exit(unlink(refresh_dest, recursive = TRUE), add = TRUE)
    full <- partial$clone(deep = TRUE)
    priv(full)$response$response$docs <- docs
    full$fetch_subset(requests, refresh_dest, workers = 1L)
    expect_identical(
        rawToChar(
            curl::curl_fetch_memory(
                paste0(index$url(), "queries")
            )$content
        ),
        queries
    )
    prior_values <- sum(subset_status(refresh_dest, "attempts")$native_values)
    expect_true(
        partial$fetch_subset(
            requests,
            refresh_dest,
            workers = 1L,
            resume = FALSE
        )$directory_complete
    )
    expect_equal(
        sum(subset_status(refresh_dest, "attempts")$native_values) -
            prior_values,
        8
    )
    # A damaged completion plus an index that drops known source identity must
    # retain the original data and report the unresolved directory explicitly.
    frozen <- list.files(
        file.path(dest, "subsets"),
        "^directory-.*[.]rds$",
        full.names = TRUE
    )
    expect_length(frozen, 1L)
    writeBin(as.raw(0), frozen)
    curl::curl_fetch_memory(paste0(index$url(), "change"))
    attempts <- subset_status(dest, "attempts")
    expect_warning(
        changed_result <- partial$fetch_subset(
            requests,
            dest,
            workers = 1L,
            timeout = 1
        ),
        "Ignoring invalid saved File directory"
    )
    expect_false(changed_result$directory_complete)
    expect_true(changed_result$complete)
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_equal(sort(read_subset(dest)$values$value), 1:4)
    expect_equal(nrow(read_subset(dest)$gaps$directory), 1L)

    # Published content contradictions are source errors, not directory transport
    # gaps. The first phase's already committed native data remains readable.
    curl::curl_fetch_memory(paste0(index$url(), "conflict"))
    conflict_dest <- tempfile("raw-directory-content-conflict-")
    on.exit(unlink(conflict_dest, recursive = TRUE), add = TRUE)
    expect_error(
        partial$fetch_subset(
            requests,
            conflict_dest,
            workers = 1L
        ),
        "Conflicting published content metadata"
    )
    expect_equal(sort(read_subset(conflict_dest)$values$value), 1:4)

    # Index unavailability cannot prevent a known healthy File from finishing.
    # This case checks directory isolation, not a one-second deadline. Allow
    # local dispatch/initialization before the first healthy HTTP request;
    # dedicated stall tests separately check worker retirement.
    unavailable <- partial$clone(deep = TRUE)
    priv(unavailable)$index_node <- "http://127.0.0.1:1"
    failed_dest <- tempfile("raw-directory-unavailable-")
    on.exit(unlink(failed_dest, recursive = TRUE), add = TRUE)
    failed <- unavailable$fetch_subset(
        requests,
        failed_dest,
        workers = 1L,
        timeout = 2
    )
    expect_true(failed$complete)
    expect_false(failed$directory_complete)
    expect_equal(nrow(read_subset(failed_dest)$gaps$directory), 1L)
    expect_equal(sort(read_subset(failed_dest)$values$value), 1:4)
})

# A complete selected File can be reusable while its originating catalogue is
# still partial. Directory gaps stay visible offline and after cache-only resume.
test_that("raw subset reports partial directories independently of native completion", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-partial-directory-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    index_app <- webfakes::new_app()
    index_app$locals$response <- jsonlite::toJSON(
        esgf_test__response(docs),
        auto_unbox = TRUE
    )
    index_app$get("/esg-search/search", function(req, res, locals) {
        res$set_header("Content-Type", "application/json")$send(locals$response)
    })
    index <- webfakes::new_app_process(index_app)
    on.exit(index$stop(), add = TRUE)
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", index$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs, num_found = 2L),
        context = NULL
    )
    plan <- partial$access_plan(expand = FALSE)
    expect_false(attr(plan, "directory_complete"))
    expect_false(attr(partial$access_plan(), "directory_complete"))
    first <- partial$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(first$complete)
    expect_false(first$directory_complete)
    expect_false(subset_status(dest)$directory_complete)
    expect_equal(read_subset(dest)$gaps$directory$request_id, "one")
    before <- subset_status(dest, "attempts")
    expect_false(
        partial$fetch_subset(
            requests,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$directory_complete
    )
    expect_identical(subset_status(dest, "attempts"), before)
    # Deliberately selecting a File from a fully collected catalogue is complete
    # for that selection, even though another File is intentionally omitted.
    all_docs <- docs[c(1L, 1L), ]
    all_docs$id[[2L]] <- "other-file"
    all_docs$instance_id[[2L]] <- "other-file-instance"
    selected <- query_result_test_object("File", all_docs)$slice(1L)
    expect_true(attr(
        selected$access_plan(expand = FALSE),
        "directory_complete"
    ))
    result <- selected$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    expect_true(result$directory_complete)
    expect_true(subset_status(dest)$directory_complete)
    expect_equal(nrow(read_subset(dest)$gaps$directory), 0L)
    expect_identical(subset_status(dest, "attempts"), before)
    # An empty explicit selection retains the fully collected directory's origin.
    expect_true(attr(
        selected$slice(integer())$access_plan(expand = FALSE),
        "directory_complete"
    ))
    # A read-only view is a controlled local persistence fault: the manifest can
    # read the old links, but cannot replace them after writing new provenance.
    conn <- duckdb::dbConnect(
        duckdb::duckdb(),
        dbdir = file.path(dest, "manifest.duckdb")
    )
    duckdb::sql_exec(
        paste(
            "ALTER TABLE subset_links RENAME TO saved_links;",
            "CREATE VIEW subset_links AS SELECT * FROM saved_links"
        ),
        conn = conn
    )
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    changed <- requests
    changed$lon <- 10.75
    expect_error(
        partial$fetch_subset(changed, dest, discover = TRUE),
        "View|view"
    )
    expect_true(subset_status(dest)$directory_complete)
    expect_equal(read_subset(dest)$links$lon, 10.25)
    conn <- duckdb::dbConnect(
        duckdb::duckdb(),
        dbdir = file.path(dest, "manifest.duckdb")
    )
    duckdb::sql_exec(
        paste(
            "DROP VIEW subset_links;",
            "ALTER TABLE saved_links RENAME TO subset_links"
        ),
        conn = conn
    )
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    # A newly supplied partial directory cannot be hidden by unrelated complete
    # requests, and selecting an earlier request retains its own provenance.
    partial_request <- requests
    partial_request$request_id <- "other"
    partial_request$site_id <- "other-city"
    partial$fetch_subset(partial_request, dest, discover = TRUE)
    expect_false(subset_status(dest)$directory_complete)
    expect_equal(nrow(read_subset(dest, site_id = "city")$gaps$directory), 0L)
    expect_equal(
        read_subset(dest, site_id = "other-city")$gaps$directory$request_id,
        "other"
    )
    # Model an older persisted schema after another native persistence error.
    # Start with ample GC headroom so the failed result survives until the
    # deliberately delayed finalization below, rather than being collected while
    # later manifest operations allocate their temporary R objects.
    gc(FALSE)
    conn <- duckdb::dbConnect(
        duckdb::duckdb(),
        dbdir = file.path(dest, "manifest.duckdb")
    )
    duckdb::sql_exec(
        paste(
            "ALTER TABLE subset_links RENAME TO saved_links;",
            "CREATE VIEW subset_links AS SELECT * FROM saved_links"
        ),
        conn = conn
    )
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    expect_error(
        partial$fetch_subset(changed, dest, discover = TRUE),
        "View|view"
    )
    conn <- duckdb::dbConnect(
        duckdb::duckdb(),
        dbdir = file.path(dest, "manifest.duckdb")
    )
    duckdb::sql_exec(
        paste(
            "DROP VIEW subset_links;",
            "ALTER TABLE saved_links RENAME TO subset_links;",
            "DROP TABLE subset_directory"
        ),
        conn = conn
    )
    duckdb::dbDisconnect(conn, shutdown = TRUE)
    # This collection is the adversarial trigger: a stale failed native result
    # must never checkpoint its old directory over a successor connection's DDL.
    # Public offline readers must still expose the missing evidence as unknown.
    gc(FALSE)
    expect_true(is.na(subset_status(dest)$directory_complete))
    expect_true(all(is.na(read_subset(dest)$gaps$directory$directory_complete)))
    selected$fetch_subset(requests, dest, discover = TRUE)
    expect_true(is.na(subset_status(dest)$directory_complete))
    expect_equal(nrow(read_subset(dest, site_id = "city")$gaps$directory), 0L)
    expect_identical(subset_status(dest, "attempts"), before)
    empty <- read_subset(dest, site_id = "absent")
    expect_equal(nrow(empty$values), 0L)
    expect_equal(nrow(empty$links), 0L)
    expect_equal(nrow(empty$gaps$directory), 0L)
})

# A fresh native read can change target membership without changing block ID.
# An interruption before the pending receipt must preserve earlier case inputs.
test_that("raw subset refresh preserves committed artifacts before pending publication", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-immutable-block-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    original <- data.frame(
        request_id = "original",
        case_id = "original",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    expect_true(
        files$fetch_subset(
            original,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    committed <- subset_status(dest, "blocks")
    expect_equal(nrow(committed), 1L)
    previous <- read_subset(dest, case_id = "original")
    changed <- original
    changed$request_id <- changed$case_id <- "changed"
    changed$lon <- 10.75
    metadata_written <- NULL
    local({
        write <- store__extract_cache_write
        local_mocked_bindings(
            store__extract_cache_write = function(path, payload) {
                result <- write(path, payload)
                # Match the canonical block ID, allowing immutable content
                # suffixes. The fault follows the real metadata publication.
                if (
                    startsWith(basename(path), committed$block_id[[1L]]) &&
                        endsWith(path, ".rds")
                ) {
                    metadata_written <<- path
                    stop("controlled pre-pending interruption", call. = FALSE)
                }
                result
            }
        )
        expect_error(
            files$fetch_subset(
                changed,
                dest,
                workers = 1L,
                discover = FALSE,
                resume = FALSE
            ),
            "controlled pre-pending interruption"
        )
    })
    expect_true(file.exists(metadata_written))
    expect_equal(
        readRDS(metadata_written)$neighborhoods$target_lon,
        rep(10.75, 4L)
    )
    expect_length(list.files(file.path(dest, "subsets"), "^pending-"), 0L)
    expect_identical(checksum_file(committed$path), committed$sha256[[1L]])
    expect_identical(
        checksum_file(committed$metadata_path),
        committed$metadata_sha256[[1L]]
    )
    retained <- subset_status(dest, "blocks")
    expect_identical(retained$path, committed$path)
    expect_identical(retained$metadata_path, committed$metadata_path)
    saved <- read_subset(dest, case_id = "original")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$values$cf_day), 1L)
    expect_equal(saved$neighborhoods, previous$neighborhoods)
    expect_equal(nrow(saved$gaps$incomplete_sources), 0L)
    expect_equal(nrow(saved$gaps$incomplete_requests), 0L)
    expect_gt(
        nrow(read_subset(dest, case_id = "changed")$gaps$incomplete_requests),
        0L
    )
    refresh <- subset_status(dest, "coverage")
    refresh <- refresh[refresh$request_id == "changed"]
    expect_true(refresh$axis_known)
    expect_equal(refresh$expected_native_values, 4)
    expect_equal(refresh$corner_count, 4L)
    expect_false(refresh$complete)
    expect_equal(refresh$available_native_values, 0)
    attempts <- subset_status(dest, "attempts")
    traffic <- curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    # The failed new case may remain a gap; the original request needs no read.
    files$fetch_subset(original, dest, workers = 1L, discover = FALSE)
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content,
        traffic
    )
    # The later interruption window must still adopt refreshed descriptors
    # when an older reference with the same canonical block ID already exists.
    local({
        rename <- base::file.rename
        local_mocked_bindings(
            file.rename = function(from, to) {
                result <- rename(from, to)
                if (
                    isTRUE(result) && grepl("/pending-[^/]+[.]rds[.]json$", to)
                ) {
                    stop(
                        "controlled pending refresh interruption",
                        call. = FALSE
                    )
                }
                result
            },
            .package = "base"
        )
        expect_error(
            files$fetch_subset(
                changed,
                dest,
                workers = 1L,
                discover = FALSE,
                resume = FALSE
            ),
            "controlled pending refresh interruption"
        )
    })
    expect_identical(
        checksum_file(committed$metadata_path),
        committed$metadata_sha256[[1L]]
    )
    attempts <- subset_status(dest, "attempts")
    traffic <- curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    expect_true(
        files$fetch_subset(
            changed,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    adopted <- subset_status(dest, "blocks")
    expect_identical(adopted$block_id, committed$block_id)
    expect_false(identical(adopted$metadata_path, committed$metadata_path))
    expect_equal(
        readRDS(adopted$metadata_path)$neighborhoods$target_lon,
        rep(10.75, 4L)
    )
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content,
        traffic
    )
    expect_equal(sort(read_subset(dest, case_id = "changed")$values$value), 1:4)
    expect_equal(
        sort(read_subset(dest, case_id = "original")$values$value),
        1:4
    )
    rollback <- changed
    rollback$request_id <- rollback$case_id <- "rollback"
    rollback$lon <- 10.5
    local({
        append <- ddb_append_table
        local_mocked_bindings(
            ddb_append_table = function(conn, table, rows, ...) {
                result <- append(conn, table, rows, ...)
                if (identical(table, "subset_blocks")) {
                    stop("controlled block transaction rollback", call. = FALSE)
                }
                result
            }
        )
        expect_error(
            files$fetch_subset(
                rollback,
                dest,
                workers = 1L,
                discover = FALSE,
                resume = FALSE
            ),
            "controlled block transaction rollback"
        )
    })
    expect_identical(subset_status(dest, "blocks"), adopted)
    expect_identical(checksum_file(adopted$path), adopted$sha256[[1L]])
    expect_identical(
        checksum_file(adopted$metadata_path),
        adopted$metadata_sha256[[1L]]
    )
    expect_equal(
        sort(read_subset(dest, case_id = "original")$values$value),
        1:4
    )
    expect_equal(sort(read_subset(dest, case_id = "changed")$values$value), 1:4)
})

# Disabling old coverage reuse must retain its scientific interval identity.
# A new client observation prevents earlier good-node timing from masking the
# incompatible endpoint that the caller explicitly places first on refresh.
test_that("raw subset refresh validates saved native bounds without reusing coverage", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-refresh-bounds-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    expect_true(
        query_result_test_object("File", docs)$fetch_subset(
            requests,
            dest,
            workers = 1L,
            discover = FALSE,
            network_id = "original"
        )$complete
    )
    original <- read_subset(dest)
    before <- nrow(subset_status(dest, "attempts"))
    docs <- docs[c(1L, 1L), ]
    docs$data_node <- c("wrong-bounds", "consistent")
    docs$url <- I(lapply(
        c("wrong-bounds.nc", "fast-bounds.nc"),
        function(name) {
            paste0(server$url(), name, "|application/netcdf|OPENDAP")
        }
    ))
    refreshed <- query_result_test_object("File", docs)$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE,
        resume = FALSE,
        network_id = "refresh"
    )
    expect_true(refreshed$complete)
    attempts <- subset_status(dest, "attempts")[-seq_len(before)]
    expect_identical(
        unique(attempts$data_node),
        c("wrong-bounds", "consistent")
    )
    expect_identical(attempts$error_kind[[1L]], "source_metadata")
    expect_equal(
        attempts$native_values[attempts$data_node == "wrong-bounds"],
        0
    )
    expect_equal(sum(attempts$native_values), 4)
    saved <- read_subset(dest)
    expect_equal(sort(saved$values$value), 1:4)
    expect_equal(unique(saved$values$bound_start_value), 0)
    expect_equal(unique(saved$values$bound_end_value), 1)
    expect_equal(
        saved$values[, c("bound_start_value", "bound_end_value")],
        original$values[, c("bound_start_value", "bound_end_value")]
    )
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
    expect_false(any(grepl("wrong-bounds.nc.dods tas", traffic, fixed = TRUE)))
    expect_equal(
        sum(grepl("fast-bounds.nc.dods tas", traffic, fixed = TRUE)),
        2L
    )
    # Loss of weather bytes does not erase verified interval metadata. Repair
    # must check the same identity even after invalidation moves the old row
    # into audit history, without counting that history as completed coverage.
    for (damage in c("missing", "corrupt")) {
        block <- subset_status(dest, "blocks")
        if (damage == "missing") {
            unlink(block$path)
        } else {
            writeBin(as.raw(0), block$path)
        }
        before <- nrow(subset_status(dest, "attempts"))
        expect_true(
            query_result_test_object("File", docs)$fetch_subset(
                requests,
                dest,
                workers = 1L,
                retries = 0L,
                discover = FALSE,
                network_id = paste0("repair-", damage)
            )$complete
        )
        attempts <- subset_status(dest, "attempts")[-seq_len(before)]
        expect_identical(
            unique(attempts$data_node),
            c("wrong-bounds", "consistent")
        )
        expect_identical(attempts$error_kind[[1L]], "source_metadata")
        expect_equal(
            attempts$native_values[attempts$data_node == "wrong-bounds"],
            0
        )
        expect_equal(sum(attempts$native_values), 4)
        saved <- read_subset(dest)
        expect_equal(sort(saved$values$value), 1:4)
        expect_equal(unique(saved$values$bound_start_value), 0)
        expect_equal(unique(saved$values$bound_end_value), 1)
        if (damage == "corrupt") {
            expect_false(identical(
                subset_status(dest, "blocks")$path,
                block$path
            ))
            expect_identical(readBin(block$path, "raw", n = 1L), as.raw(0))
        }
    }
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
    expect_false(any(grepl("wrong-bounds.nc.dods tas", traffic, fixed = TRUE)))
    expect_equal(
        sum(grepl("fast-bounds.nc.dods tas", traffic, fixed = TRUE)),
        4L
    )
})

# Published subannual periods exclude unrelated Files before native OPeNDAP
# reads, without rounding 360-day dates into the Gregorian calendar.
test_that("raw subset windows match native File periods at their published precision", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-native-file-window-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$id <- docs$instance_id <- docs$master_id <- c("january", "june")
    docs$title <- c("tas_206001-206001.nc", "tas_206006-206006.nc")
    docs$variable_id <- "tas"
    docs$url <- I(lapply(c("fast.nc", "june.nc"), function(name) {
        paste0(server$url(), name, "|application/netcdf|OPENDAP")
    }))
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "june",
        case_id = "summer",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-06-01",
        time_stop = "2060-06-01"
    )
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    saved <- read_subset(dest, case_id = "summer")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$values$cf_month), 6L)
    expect_equal(nrow(saved$neighborhoods), 4L)
    expect_equal(nrow(saved$gaps$incomplete_requests), 0L)
    attempts <- subset_status(dest, "attempts")
    expect_identical(unique(attempts$url), paste0(server$url(), "june.nc"))
    traffic <- curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    expect_false(any(grepl(
        "fast.nc",
        jsonlite::fromJSON(rawToChar(traffic)),
        fixed = TRUE
    )))
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content,
        traffic
    )
    request$request_id <- "missing"
    request$case_id <- "missing"
    request$time_start <- request$time_stop <- "2060-05-01"
    expect_false(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_gt(
        nrow(read_subset(dest, case_id = "missing")$gaps$incomplete_requests),
        0L
    )
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content,
        traffic
    )
    # A window spanning Files legitimately keeps each File's available native
    # portion; the paper workflow separately judges whole-case time coverage.
    request$request_id <- request$case_id <- "spanning"
    request$time_start <- "2060-01-01"
    request$time_stop <- "2060-06-01"
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    spanning <- read_subset(dest, case_id = "spanning")
    expect_equal(sort(spanning$values$value), sort(c(1:32, 1:4)))
    expect_equal(sort(unique(spanning$values$cf_month)), c(1L, 6L))
})

# A DRS boundary is an inclusive calendar field tuple at the stated precision,
# including February 30 and subdaily File endpoints. Native CF remains the
# final authority when a label is absent or cannot be parsed safely.
test_that("raw subset File windows retain nonGregorian and subdaily endpoints", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    destinations <- tempfile("raw-native-periods-")
    dir.create(destinations)
    on.exit(unlink(destinations, recursive = TRUE), add = TRUE)
    examples <- data.frame(
        first = c(
            "20600229",
            "2060010100",
            "206001010000",
            "20600101000000",
            "20600101"
        ),
        last = c(
            "20600230",
            "2060010103",
            "206001010003",
            "20600101000003",
            "20600104"
        ),
        origin = c(
            "february.nc",
            "hour.nc",
            "minute.nc",
            "second.nc",
            "fast.nc"
        ),
        endpoint = c(
            "2060-02-30",
            "2060-01-01T03:00:00Z",
            "2060-01-01T00:03:00Z",
            "2060-01-01T00:00:03Z",
            "2060-01-04"
        ),
        value = c(29L, 13L, 13L, 13L, 13L)
    )
    for (i in seq_len(nrow(examples))) {
        dest <- file.path(destinations, as.character(i))
        docs <- query_result_test_file_docs()
        docs <- docs[c(1L, 1L), ]
        docs$id <- docs$instance_id <- docs$master_id <- c("outside", "inside")
        docs$title <- paste0(
            "tas_",
            c(examples$first[[i]], examples$last[[i]]),
            "-",
            c(examples$first[[i]], examples$last[[i]]),
            ".nc"
        )
        docs$variable_id <- "tas"
        docs$url <- I(lapply(
            c(paste0("outside-", examples$origin[[i]]), examples$origin[[i]]),
            function(name) {
                paste0(server$url(), name, "|application/netcdf|OPENDAP")
            }
        ))
        files <- query_result_test_object("File", docs)
        request <- data.frame(
            request_id = "native",
            case_id = "native",
            site_id = "city",
            lon = 10.25,
            lat = .25,
            time_start = examples$endpoint[[i]],
            time_stop = examples$endpoint[[i]]
        )
        expect_true(
            files$fetch_subset(
                request,
                dest,
                workers = 1L,
                retries = 0L,
                discover = FALSE
            )$complete
        )
        expect_equal(
            sort(read_subset(dest, case_id = "native")$values$value),
            examples$value[[i]] + 0:3
        )
        expect_identical(
            unique(subset_status(dest, "attempts")$url),
            paste0(server$url(), examples$origin[[i]])
        )
        expect_equal(
            files$access_plan(expand = FALSE)$file_start_year,
            c(2060L, 2060L)
        )
    }
    for (label in c(
        "unknown.nc",
        "tas_206013-206013.nc",
        "tas_206006-206001.nc"
    )) {
        dest <- file.path(destinations, label)
        docs <- query_result_test_file_docs(paste0(
            server$url(),
            "june.nc|application/netcdf|OPENDAP"
        ))
        docs$title <- label
        docs$variable_id <- "tas"
        request <- data.frame(
            request_id = "unknown",
            site_id = "city",
            lon = 10.25,
            lat = .25,
            time_start = "2060-06-01",
            time_stop = "2060-06-01"
        )
        expect_true(
            query_result_test_object("File", docs)$fetch_subset(
                request,
                dest,
                workers = 1L,
                retries = 0L,
                discover = FALSE
            )$complete
        )
        expect_equal(sort(read_subset(dest)$values$value), 1:4)
    }
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
    expect_false(any(grepl("outside-", traffic, fixed = TRUE)))
})

# Every OPeNDAP service advertised by one File remains a real execution option.
# The first address is unreachable; the second serves the original native cells.
test_that("raw subsets retain all advertised OPeNDAP endpoints", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("all-opendap-services-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    bad <- "http://127.0.0.1:1/unavailable.nc"
    good <- paste0(server$url(), "fast.nc")
    docs <- query_result_test_file_docs()
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        c(bad, good, paste0(good, ".html")),
        "|application/netcdf|OPENDAP"
    )))
    files <- query_result_test_object("File", docs)
    plan <- files$access_plan(expand = FALSE)
    expect_identical(plan$url, c(bad, good))
    expect_identical(plan$record_index, c(1L, 1L))
    expect_length(unique(plan$source_key), 1L)
    requests <- data.frame(
        request_id = "one-day",
        case_id = "original",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        timeout = 5,
        discover = FALSE
    )
    expect_true(result$complete)
    attempts <- subset_status(dest, "attempts")
    expect_true(any(!attempts$success))
    expect_equal(sum(attempts$native_values), 4)
    saved <- read_subset(dest, case_id = "original")
    expect_equal(sort(saved$values$value), 1:4)
    expect_equal(nrow(saved$neighborhoods), 4L)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_identical(unique(saved$values$cf_day), 1L)
    before <- curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    requests$request_id <- "linked-day"
    requests$case_id <- "linked"
    expect_true(
        files$fetch_subset(
            requests,
            dest,
            workers = 1L,
            retries = 0L,
            timeout = 5,
            discover = FALSE
        )$complete
    )
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content,
        before
    )
    expect_equal(sort(read_subset(dest, case_id = "linked")$values$value), 1:4)
})

# Expanding per-record services cannot change the File metadata they belong to,
# or hide a File with no OPeNDAP service behind HTTP-only acquisition.
test_that("raw subset endpoint inventories preserve File rows and missing services", {
    docs <- query_result_test_file_docs()
    docs <- docs[rep(1L, 3L), ]
    docs$id <- paste0("record-", 1:3)
    docs$instance_id <- docs$master_id <- paste0("logical-", 1:3)
    docs$variable_id <- "tas"
    docs$title <- paste0(
        "tas_Amon_model_historical_member_grid_",
        c(
            "206001-206001.nc",
            "206006-206006.nc",
            "206012-206012.nc"
        )
    )
    docs$data_node <- NA_character_
    docs$url <- I(list(
        c(
            "https://one.example/a.nc.html|application/netcdf|OPENDAP",
            "https://one.example/a.nc|application/netcdf|OPENDAP",
            "https://two.example/a.nc|application/netcdf|OPENDAP"
        ),
        "https://three.example/b.nc|application/netcdf|OPENDAP",
        "https://http.example/c.nc|application/netcdf|HTTPServer"
    ))
    files <- query_result_test_object("File", docs)
    plan <- files$access_plan(expand = FALSE)
    expect_identical(plan$record_index, c(1L, 1L, 2L, 3L))
    expect_identical(
        plan$url,
        c(
            "https://one.example/a.nc",
            "https://two.example/a.nc",
            "https://three.example/b.nc",
            NA_character_
        )
    )
    expect_identical(
        plan$data_node,
        c("one.example", "two.example", "three.example", NA_character_)
    )
    expect_identical(
        plan$file_start_key,
        c(
            "20600100000000",
            "20600100000000",
            "20600600000000",
            "20601200000000"
        )
    )
    expect_length(unique(plan$source_key), 3L)
    expect_identical(plan$source_key[[1L]], plan$source_key[[2L]])
    expect_identical(attr(plan, "selection"), files$selection())
    missing <- files$slice(3L)
    dest <- tempfile("missing-opendap-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    requests <- data.frame(
        request_id = "missing",
        case_id = "case",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-12-01",
        time_stop = "2060-12-01"
    )
    result <- missing$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_false(result$complete)
    expect_equal(sum(subset_status(dest, "attempts")$native_values), 0)
    expect_equal(nrow(subset_status(dest, "blocks")), 0L)
})

# Transport partitioning must not enter native identity: a larger later budget
# reuses intact blocks and repairs only the exact lost time/corner ranges.
test_that("acquisition transport budgets preserve raw corners and exact resume", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[1L, ]
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    )))
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = "native",
        case_id = "case",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    small <- tempfile("transport-small-")
    large <- tempfile("transport-large-")
    on.exit(unlink(c(small, large), recursive = TRUE), add = TRUE)
    first <- files$fetch_subset(
        requests,
        small,
        workers = 1L,
        discover = FALSE,
        max_values = 8L
    )
    expect_true(first$complete)
    blocks <- subset_status(small, "blocks")
    expect_equal(nrow(blocks), 4L)
    # Public values retain native keys; transport block order is not identity.
    canonical <- function(values) {
        values <- data.table::copy(values)
        data.table::setorderv(values, c("ind_lon", "ind_lat", "time_index"))
        values
    }
    expected <- read_subset(small)
    expected$values <- canonical(expected$values)
    expect_equal(nrow(expected$values), 32L)
    expect_equal(
        data.table::uniqueN(expected$values[, c("grid_lat", "grid_lon")]),
        4L
    )
    second <- files$fetch_subset(
        requests,
        large,
        workers = 1L,
        discover = FALSE,
        max_values = 32L
    )
    expect_true(second$complete)
    expect_equal(nrow(subset_status(large, "blocks")), 1L)
    actual <- read_subset(large)
    expect_equal(canonical(actual$values), expected$values)
    expect_equal(actual$neighborhoods, expected$neighborhoods)
    attempts <- subset_status(small, "attempts")
    again <- files$fetch_subset(
        requests,
        small,
        workers = 1L,
        discover = FALSE,
        max_values = 32L
    )
    expect_true(again$complete)
    expect_equal(subset_status(small, "attempts"), attempts)
    writeBin(as.raw(0), blocks$path[[2L]])
    repaired <- files$fetch_subset(
        requests,
        small,
        workers = 1L,
        discover = FALSE,
        max_values = 32L
    )
    expect_true(repaired$complete)
    expect_equal(canonical(read_subset(small)$values), expected$values)
    expect_equal(
        sum(subset_status(small, "attempts")$native_values) -
            sum(attempts$native_values),
        8
    )
    expect_error(files$fetch_subset(requests, small, max_values = 3L))
})

# A File-specific absence must never quarantine every other File on its node;
# node-wide cooling requires independent recent service failures and expires.
test_that("acquisition replica cooldown is scoped and expires from saved observations", {
    candidates <- data.table::data.table(
        source_key = c("a", "b", "c"),
        url = c("u", "v", "w"),
        data_node = c("node", "node", "other")
    )
    history <- data.frame(
        source_key = "a",
        url = "u",
        data_node = "node",
        client = "client",
        observed_at = 1000,
        native_values = 0,
        success = FALSE,
        error = "NetCDF: HTTP 404 Not Found",
        error_kind = "transport"
    )
    until <- opendap__cooling_until(
        candidates,
        history,
        "client",
        60,
        now = 1020
    )
    expect_equal(until, c(1480, 0, 0))
    expect_equal(
        opendap__cooling_until(candidates, history, "other", 60, now = 1020),
        c(0, 0, 0)
    )
    expect_equal(
        opendap__cooling_until(candidates, history, "client", 0, now = 1020),
        c(0, 0, 0)
    )
    expect_true(all(
        opendap__cooling_until(candidates, history, "client", 60, now = 1500) <=
            1500
    ))
    # Unknown native-library I/O errors may be File-specific: cool that URL only.
    history$error <- "NetCDF: I/O failure"
    expect_equal(
        opendap__cooling_until(candidates, history, "client", 60, now = 1020),
        c(1060, 0, 0)
    )
    service <- history[rep(1L, 3L), ]
    service$source_key <- c("a", "d", "e")
    service$url <- c("u", "x", "y")
    service$error <- "connection reset"
    expect_equal(
        opendap__cooling_until(candidates, service, "client", 60, now = 1020),
        c(1060, 1060, 0)
    )
    success <- service[1L, ]
    success$source_key <- "f"
    success$url <- "z"
    success$observed_at <- 1001
    success$native_values <- 4
    success$success <- TRUE
    success$error <- NA_character_
    success$error_kind <- "data"
    expect_equal(
        opendap__cooling_until(
            candidates,
            rbind(service, success),
            "client",
            60,
            now = 1020
        ),
        c(1060, 0, 0)
    )
    success$source_key <- "a"
    success$url <- "u"
    expect_equal(
        opendap__cooling_until(
            candidates,
            rbind(history, success),
            "client",
            60,
            now = 1020
        ),
        c(0, 0, 0)
    )
    history$error_kind <- "source_metadata"
    expect_equal(
        opendap__cooling_until(candidates, history, "client", 60, now = 1020),
        c(0, 0, 0)
    )
    expect_length(
        opendap__cooling_until(
            candidates[0L],
            history,
            "client",
            60,
            now = 1020
        ),
        0L
    )
})

# A service that is down must release its worker and leave an explicit gap.
# Finite later invocations reuse the same client-specific failure evidence.
test_that("acquisition replica cooldown avoids hot retries and keeps later recovery possible", {
    skip_if_not_installed("webfakes")
    app <- webfakes::new_app()
    app$use(function(req, res) {
        Sys.sleep(.05)
        res$set_status(503L)$send("temporarily unavailable")
    })
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[1L, ]
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        server$url(),
        "offline.nc|application/netcdf|OPENDAP"
    )))
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "gap",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("replica-cooldown-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    first <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        retries = 2L,
        replica_cooldown = 60
    )
    expect_false(first$complete)
    before <- subset_status(dest, "attempts")
    expect_equal(sum(!before$success), 1L)
    expect_false(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            retries = 2L,
            replica_cooldown = 60
        )$complete
    )
    expect_equal(subset_status(dest, "attempts"), before)
    # The previous owner has closed the test manifest; move the failure clock
    # back instead of sleeping for a minute or changing the running clock.
    db <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(db, "UPDATE subset_attempts SET observed_at = observed_at - 61")
    ddb_disconnect(db)
    expect_false(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            retries = 0L,
            replica_cooldown = 60
        )$complete
    )
    expect_equal(sum(!subset_status(dest, "attempts")$success), 2L)
    expect_error(files$fetch_subset(request, dest, replica_cooldown = -1))
    # Known endpoints cooling must not suppress discovery of a healthy Replica.
    healthy <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(healthy$stop(), add = TRUE)
    replacement <- docs
    replacement$url <- I(list(paste0(
        healthy$url(),
        "fast.nc|application/netcdf|OPENDAP"
    )))
    additions <- query_result_test_object("File", replacement)$access_plan(
        expand = FALSE
    )
    discover <- function(job) additions
    environment(discover) <- list2env(
        list(additions = additions),
        parent = baseenv()
    )
    local_mocked_bindings(opendap__discover = discover)
    recovered <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = TRUE,
        retries = 0L,
        replica_cooldown = 60
    )
    expect_true(recovered$complete)
    expect_equal(sort(read_subset(dest)$values$value), 1:32)
    expect_equal(sum(!subset_status(dest, "attempts")$success), 2L)
})

# Pending Files on an occupied node must not repeatedly materialize an unchanged
# attempt ledger. Count the real coordinator reads while real DAP workers run.
test_that("acquisition scheduling refreshes history only for new observations", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()
    docs <- docs[c(1L, 1L), ]
    docs$instance_id <- c("first", "second")
    docs$master_id <- docs$instance_id
    docs$variable_id <- "tas"
    docs$data_node <- "same-node"
    docs$url <- I(rep(
        list(paste0(server$url(), "slow.nc|application/netcdf|OPENDAP")),
        2L
    ))
    request <- data.frame(
        request_id = "bounded",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("history-refresh-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    original <- ddb_read_table
    reads <- 0L
    local_mocked_bindings(ddb_read_table = function(conn, name, ...) {
        if (identical(name, "subset_attempts")) {
            reads <<- reads + 1L
        }
        original(conn, name, ...)
    })
    result <- query_result_test_object("File", docs)$fetch_subset(
        request,
        dest,
        workers = 2L,
        node_workers = 1L,
        discover = FALSE,
        retries = 0L
    )
    expect_true(result$complete)
    expect_lte(reads, 5L)
    expect_equal(nrow(read_subset(dest)$values), 64L)
})

# Slow native DAP errors need independent File evidence across a full failure
# cycle; a single missing File or a successful native read cannot close a node.
test_that("acquisition node cooldown corroborates slow DAP failures and resets", {
    candidates <- data.table::data.table(
        source_key = c("next", "other"),
        url = c("next", "other"),
        data_node = c("node", "other")
    )
    history <- data.frame(
        source_key = c("a", "b", "c"),
        url = c("u", "v", "w"),
        data_node = "node",
        client = "client",
        observed_at = c(1000, 1090, 1180),
        native_values = 0,
        success = FALSE,
        error = "NetCDF: DAP failure",
        error_kind = "transport"
    )
    # Keep the caller clock explicit so slow-failure evidence is deterministic.
    cool <- function(rows, now = 1181, seconds = 60) {
        opendap__cooling_until(candidates, rows, "client", seconds, now)
    }
    expect_equal(cool(history), c(1240, 0))
    expect_equal(cool(history[1:2, ]), c(0, 0))
    duplicate <- history
    duplicate$source_key <- "one"
    expect_equal(cool(duplicate), c(0, 0))
    for (error in c(
        "NetCDF: DAP failure HTTP 404",
        "NetCDF: file not found",
        "NetCDF: DAP failure 410 Gone",
        "NetCDF: DAP failure; HTTP/1.1 404",
        "NetCDF: DAP failure; HTTP/2 410"
    )) {
        missing <- history
        missing$error <- error
        expect_equal(cool(missing), c(0, 0))
    }
    explicit <- history
    explicit$error <- "execution deadline"
    expect_equal(cool(explicit), c(1240, 0))
    explicit$error <- "NetCDF: HTTP/1.1 503"
    expect_equal(cool(explicit), c(1240, 0))
    expect_equal(cool(history, seconds = 0), c(0, 0))
    expect_true(all(cool(history, now = 2000) <= 2000))
    future <- history
    future$observed_at <- future$observed_at + 1000
    expect_equal(cool(future), c(0, 0))
    foreign <- history
    foreign$client <- "other"
    expect_equal(cool(foreign), c(0, 0))
    metadata <- history
    metadata$error_kind <- "source_metadata"
    expect_equal(cool(metadata), c(0, 0))
    success <- history[1L, ]
    success$observed_at <- 1180.5
    success$success <- TRUE
    success$native_values <- 4
    success$error <- NA_character_
    expect_equal(cool(rbind(history, success)), c(0, 0))
    # Metadata-only success does not demonstrate that native weather reads work.
    success$native_values <- 0
    expect_equal(cool(rbind(history, success)), c(1240, 0))
})

# Backoff is finite and derived from persisted observations, independent of the
# calling process; later success restores the original recovery opportunity.
test_that("acquisition node cooldown backs off repeated failures with a finite cap", {
    history <- data.frame(
        source_key = paste0("file", 1:9),
        url = paste0("url", 1:9),
        data_node = "node",
        client = "client",
        observed_at = 1001:1009,
        native_values = 0,
        success = FALSE,
        error = "NetCDF: DAP failure",
        error_kind = "transport"
    )
    candidates <- data.table::data.table(
        source_key = "next",
        url = "next",
        data_node = "node"
    )
    deadlines <- vapply(
        3:9,
        function(n) {
            opendap__cooling_until(
                candidates,
                history[seq_len(n), ],
                "client",
                60,
                1010
            )
        },
        numeric(1L)
    )
    expect_equal(deadlines, 1003:1009 + c(60, 120, 240, 480, 480, 480, 480))
    endpoint <- data.table::as.data.table(history)
    endpoint$source_key <- "one"
    endpoint$url <- "one"
    expect_equal(opendap__cooldown_deadline(endpoint, 60), 1489)
    success <- endpoint[1L, ]
    success$observed_at <- 1010
    success$native_values <- 4
    success$success <- TRUE
    expect_equal(opendap__cooldown_deadline(rbind(endpoint, success), 60), 0)
    absent <- endpoint
    absent$error <- "HTTP 404 Not Found"
    expect_equal(opendap__cooldown_deadline(absent, 60), 1489)
    # Protocol versions contain digits before the status. Their File absences
    # still use the immediate endpoint interval and never become node evidence.
    for (error in c(
        "NetCDF: DAP failure; HTTP/1.1 404",
        "NetCDF: DAP failure; HTTP/2 410"
    )) {
        absent$error <- error
        expect_equal(opendap__cooldown_deadline(absent[1L], 60), 1481)
    }
})

# The real coordinator must release a sick node, keep another node's raw
# corners, and recover every deferred File without replacing committed data.
test_that("acquisition node cooldown preserves raw corners and later exact recovery", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()[rep(1L, 6L), ]
    docs$master_id <- docs$instance_id <- paste0("node-file-", 1:6)
    docs$variable_id <- "tas"
    docs$data_node <- c(rep("unavailable", 5L), "healthy")
    docs$url <- I(lapply(1:6, function(i) {
        paste0(
            server$url(),
            "source",
            i,
            ".nc|application/netcdf|OPENDAP"
        )
    }))
    request <- data.frame(
        request_id = "raw",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("node-recovery-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    original <- opendap__read_source
    # Inject the observed native condition at the transport seam; healthy DAP
    # reads, publication, saved observations and scheduling remain real.
    reader <- function(job) {
        if (job$data_node == "unavailable") {
            stop("NetCDF: DAP failure")
        }
        original(job)
    }
    environment(reader) <- list2env(
        list(original = original),
        parent = baseenv()
    )
    local_mocked_bindings(opendap__read_source = reader)
    files <- query_result_test_object("File", docs)
    first <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        retries = 0L,
        replica_cooldown = 60,
        max_values = 4L
    )
    expect_false(first$complete)
    attempts <- subset_status(dest, "attempts")
    expect_equal(sum(!attempts$success), 3L)
    expect_equal(sort(read_subset(dest)$values$value), 1:32)
    blocks <- subset_status(dest, "blocks")
    hashes <- tools::md5sum(blocks$path)
    again <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        retries = 0L,
        replica_cooldown = 60,
        max_values = 64L
    )
    expect_false(again$complete)
    expect_equal(subset_status(dest, "attempts"), attempts)
    # Both previous writers are closed: age this private manifest's observations
    # rather than sleeping or altering the system clock.
    db <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(db, "UPDATE subset_attempts SET observed_at = observed_at - 1000")
    ddb_disconnect(db)
    local_mocked_bindings(opendap__read_source = original)
    recovered <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        retries = 0L,
        replica_cooldown = 60,
        max_values = 64L
    )
    expect_true(recovered$complete)
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), sort(rep(1:32, 6L)))
    expect_identical(unique(raw$values$cf_calendar), "360_day")
    expect_equal(sort(unique(raw$values$cf_day)), 1:8)
    expect_equal(tools::md5sum(names(hashes)), hashes)
    points <- data.table::as.data.table(raw$values)[,
        list(
            points = data.table::uniqueN(paste(ind_lon, ind_lat)),
            times = data.table::uniqueN(time_index)
        ),
        by = "source_key"
    ]
    expect_true(all(points$points == 4L))
    expect_true(all(points$times == 8L))
    saved <- subset_status(dest, "attempts")
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            replica_cooldown = 60
        )$complete
    )
    expect_equal(subset_status(dest, "attempts"), saved)
})

# Full failed-job cycles can outlast a fixed cooldown evidence window. Keep
# distinct-File corroboration for native transport failures without cooling a
# known missing File or allowing unbounded/stale history to close a node.
test_that("acquisition failure tail keeps slow DAP and IO corroboration", {
    history <- data.frame(
        source_key = c("a", "b", "c"),
        url = c("u", "v", "w"),
        data_node = "node",
        client = "client",
        observed_at = c(1000, 1275, 1550),
        elapsed_seconds = 275,
        native_values = 0,
        success = FALSE,
        error = "NetCDF: DAP failure",
        error_kind = "transport"
    )
    candidates <- data.table::data.table(
        source_key = "next",
        url = "next",
        data_node = "node"
    )
    # Pin the node, client and interval while exposing the clock for repeatable evidence gates.
    cool <- function(rows, now = 1551) {
        opendap__cooling_until(candidates, rows, "client", 60, now)
    }
    expect_equal(cool(history), 1610)
    history$error <- "Failed to open OPeNDAP connection: NetCDF: I/O failure"
    expect_equal(cool(history), 1610)
    expect_equal(cool(history[1:2, ]), 0)
    one <- history
    one$source_key <- "same"
    expect_equal(cool(one), 0)
    for (error in c(
        "NetCDF: I/O failure; HTTP/1.1 404",
        "NetCDF: I/O failure; HTTP/2 410",
        "NetCDF: I/O failure; file not found"
    )) {
        missing <- history
        missing$error <- error
        expect_equal(cool(missing), 0)
    }
    # One node's slow operation must not make stale evidence actionable elsewhere.
    short <- history
    short$elapsed_seconds <- 10
    other <- history[1L, ]
    other$data_node <- "other"
    other$observed_at <- 1550
    other$elapsed_seconds <- 1e9
    expect_equal(cool(rbind(short, other)), 0)
    stale <- history
    stale$observed_at <- c(1000, 100000, 200000)
    stale$elapsed_seconds <- 1e9
    expect_equal(cool(stale, now = 200001), 0)
    # Successful native progress restores eligibility even after slow errors.
    success <- history[1L, ]
    success$observed_at <- 1550.5
    success$success <- TRUE
    success$native_values <- 4
    success$error <- NA_character_
    expect_equal(cool(rbind(history, success)), 0)
    expect_true(cool(history, now = 3000) <= 3000)
    # Invalid durations cannot extend the evidence window or produce NA gates.
    for (duration in c(NA_real_, Inf, -1)) {
        invalid <- history
        invalid$elapsed_seconds <- duration
        expect_equal(cool(invalid), 0)
    }
})


# A source can expose its complete native contract before a single weather
# value arrives. Preserve that denominator across failure and exact recovery.
test_that("acquisition recovery retains first-block metadata and checked coverage", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app(fail_first = TRUE))
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()[1L, ]
    docs$master_id <- docs$instance_id <- "metadata-first-source"
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        server$url(),
        "source.nc|application/netcdf|OPENDAP"
    )))
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "raw",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("metadata-first-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    first <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        retries = 0L
    )
    expect_false(first$complete)
    coverage <- subset_status(dest, "coverage")
    expect_true(coverage$axis_known)
    expect_equal(coverage$corner_count, 4L)
    expect_equal(coverage$expected_native_values, 32)
    expect_equal(coverage$available_native_values, 0)
    expect_false(coverage$complete)
    expect_identical(coverage$status, "failed")
    expect_identical(coverage$gap_kind, "transport")
    expect_true(coverage$retryable)
    timing <- subset_status(dest, "timing")
    expect_true(is.finite(timing$recorded_metadata_seconds))
    expect_equal(timing$successful_native_values, 0)
    expect_equal(timing$successful_read_seconds, 0)
    expect_true(timing$failed_task_seconds > 0)
    demands <- list.files(
        file.path(dest, "subsets"),
        "^demand-.*[.]rds$",
        full.names = TRUE
    )
    hashes <- tools::sha256sum(demands)
    curl::curl_fetch_memory(paste0(server$url(), "recover"))
    recovered <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        discover = FALSE,
        max_values = 4L
    )
    expect_true(recovered$complete)
    coverage <- subset_status(dest, "coverage")
    expect_true(coverage$complete)
    expect_equal(coverage$available_native_values, 32)
    expect_false(coverage$retryable)
    raw <- read_subset(dest)
    expect_identical(unique(raw$values$cf_calendar), "360_day")
    expect_equal(sort(raw$values$value), as.double(1:32))
    expect_equal(tools::sha256sum(names(hashes)), hashes)
    saved <- subset_status(dest, "attempts")
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    expect_equal(subset_status(dest, "attempts"), saved)
})

# Read-only coverage must inspect exact receipt/block identity, preserve all
# case links, and refuse to certify a stale job or a corrupt immutable block.
test_that("acquisition recovery coverage ignores stale labels and invalid artifacts", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()[1L, ]
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        server$url(),
        "source.nc|application/netcdf|OPENDAP"
    )))
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = c("a", "b"),
        case_id = c("case-a", "case-b"),
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("checked-coverage-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            max_values = 4L
        )$complete
    )
    c <- subset_status(dest, "coverage")
    expect_equal(nrow(c), 2L)
    expect_equal(length(unique(c$demand_id)), 1L)
    expect_true(all(c$complete))
    expect_equal(
        subset_status(dest, "coverage", source_key = unique(c$source_key)),
        c
    )
    expect_equal(
        nrow(subset_status(dest, "coverage", source_key = "absent")),
        0L
    )
    expect_equal(nrow(subset_status(dest, "timing", source_key = "absent")), 0L)
    expect_error(
        subset_status(dest, "blocks", source_key = unique(c$source_key)),
        "only coverage"
    )
    db <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(db, "UPDATE subset_jobs SET status='running'")
    ddb_disconnect(db)
    expect_true(all(subset_status(dest, "coverage")$complete))
    blocks <- subset_status(dest, "blocks")
    path <- blocks$path[[1L]]
    con <- file(path, "ab")
    writeBin(as.raw(0L), con)
    close(con)
    partial <- subset_status(dest, "coverage")
    expect_false(any(partial$complete))
    expect_true(all(partial$status == "partial"))
    expect_true(all(partial$available_native_values == 28))
    expect_true(all(partial$retryable))
    expect_equal(nrow(subset_status(dest, "blocks")), nrow(blocks))
    unknown <- request
    unknown$request_id <- c("c", "d")
    unknown$time_start <- "2060-01-09"
    unknown$time_stop <- "2060-01-10"
    expect_false(
        files$fetch_subset(
            unknown,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    all <- subset_status(dest, "coverage")
    empty <- all[all$request_id %in% unknown$request_id]
    expect_true(all(empty$axis_known))
    expect_equal(empty$expected_native_values, c(0, 0))
    expect_false(any(empty$complete))
    expect_true(all(empty$gap_kind == "empty_selection"))
    expect_false(any(empty$retryable))
    db <- ddb_connect(file.path(dest, "manifest.duckdb"))
    selection <- ddb_read_table(db, "subset_metadata")
    ddb_disconnect(db)
    for (path in selection$metadata_path) {
        con <- file(path, "ab")
        writeBin(as.raw(1L), con)
        close(con)
    }
    expect_true(all(is.na(subset_status(dest, "timing")$recorded_open_seconds)))
})

# Restoration is gated by corroborated failures until a real native value
# succeeds; metadata-only success, other clients, and File absences cannot reset it.
test_that("acquisition recovery node admission requires real native success", {
    rows <- data.table::data.table(
        source_key = letters[1:3],
        url = letters[1:3],
        data_node = "recovering",
        client = "local",
        observed_at = 10:12,
        elapsed_seconds = 1,
        success = FALSE,
        native_values = 0,
        error_kind = "transport",
        error = "NetCDF: I/O failure"
    )
    candidates <- data.table::data.table(data_node = c("recovering", "healthy"))
    expect_equal(
        opendap__recovery_limits(candidates, rows, "local", 60, 4L, now = 800),
        c(1L, 4L)
    )
    expect_equal(
        opendap__recovery_limits(candidates, rows, "local", 0, 4L, now = 800),
        c(4L, 4L)
    )
    expect_equal(
        opendap__recovery_limits(candidates, rows, "other", 60, 4L, now = 800),
        c(4L, 4L)
    )
    expect_equal(
        opendap__recovery_limits(
            candidates,
            rows,
            "local",
            60,
            4L,
            now = 90000
        ),
        c(4L, 4L)
    )
    metadata <- data.table::copy(rows[1L])
    data.table::set(metadata, j = "success", value = TRUE)
    data.table::set(metadata, j = "observed_at", value = 799)
    expect_equal(
        opendap__recovery_limits(
            candidates,
            rbind(rows, metadata),
            "local",
            60,
            4L,
            now = 800
        ),
        c(1L, 4L)
    )
    data.table::set(metadata, j = "native_values", value = 1)
    expect_equal(
        opendap__recovery_limits(
            candidates,
            rbind(rows, metadata),
            "local",
            60,
            4L,
            now = 800
        ),
        c(4L, 4L)
    )
    data.table::set(
        rows,
        j = "error",
        value = "NetCDF: I/O failure; HTTP/2 404"
    )
    expect_equal(
        opendap__recovery_limits(candidates, rows, "local", 60, 4L, now = 800),
        c(4L, 4L)
    )
    reason <- opendap__gap_reason(
        "Compatible OPeNDAP replicas are cooling until 2026-10-25 00:00:00 UTC."
    )
    expect_identical(reason$kind, "deferred")
    expect_true(reason$retryable)
    expect_equal(
        reason$retry_after,
        as.double(as.POSIXct("2026-10-25", tz = "UTC"))
    )
    expect_false(
        opendap__gap_reason("Replica grid identity conflicts")$retryable
    )
    expect_true(opendap__gap_reason("NetCDF: file not found")$retryable)
    expect_false(
        opendap__gap_reason("Replica identity 404 conflicts")$retryable
    )
    expect_true(
        opendap__gap_reason("No compatible OPeNDAP candidate.")$retryable
    )
    # Native time unions count each point once across overlapping transport
    # pieces; a two-point longitude fragment cannot invent the other corners.
    pieces <- data.table::data.table(
        time_start_index = c(1L, 3L, 7L),
        time_count = c(4L, 4L, 2L)
    )
    expect_equal(opendap__range_values(list(c(1L, 8L)), pieces), 8)
    expect_equal(opendap__range_values(list(c(1L, 2L), c(7L, 2L)), pieces), 4)
})

# Eligibility is shared node evidence. Inspecting several Files must aggregate
# it once per policy, and a disabled cooldown must not load any attempt history.
test_that("acquisition recovery coverage shares eligibility history", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app(fail_first = TRUE))
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs()[rep(1L, 4L), ]
    docs$instance_id <- docs$master_id <- paste0("eligibility-", seq_len(4L))
    docs$variable_id <- "tas"
    docs$url <- I(rep(
        list(paste0(server$url(), "source.nc|application/netcdf|OPENDAP")),
        4L
    ))
    request <- data.frame(
        request_id = "shared",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    dest <- tempfile("shared-eligibility-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    expect_false(
        query_result_test_object("File", docs)$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    conn <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(conn, "UPDATE subset_controls SET replica_cooldown=60")
    ddb_disconnect(conn)
    calls <- 0L
    cooling <- opendap__cooling_until
    local_mocked_bindings(opendap__cooling_until = function(...) {
        calls <<- calls + 1L
        cooling(...)
    })
    coverage <- subset_status(dest, "coverage")
    expect_equal(nrow(coverage), 4L)
    expect_true(all(is.finite(coverage$retry_after)))
    expect_equal(calls, 1L)
    keys <- coverage$source_key
    conn <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(
        conn,
        paste0(
            "UPDATE subset_controls SET replica_cooldown=120 WHERE source_key=",
            ddb_literal(conn, keys[[1L]])
        )
    )
    ddb_exec(
        conn,
        paste0(
            "UPDATE subset_controls SET client='isolated' WHERE source_key=",
            ddb_literal(conn, keys[[2L]])
        )
    )
    ddb_exec(
        conn,
        paste0(
            "UPDATE subset_controls SET replica_cooldown=0 WHERE source_key=",
            ddb_literal(conn, keys[[3L]])
        )
    )
    ddb_disconnect(conn)
    calls <- 0L
    mixed <- subset_status(dest, "coverage")
    expect_equal(calls, 3L)
    expect_true(is.na(mixed$retry_after[[2L]]))
    expect_true(is.na(mixed$retry_after[[3L]]))
    expect_gt(mixed$retry_after[[1L]] - mixed$retry_after[[4L]], 50)
    expect_equal(
        subset_status(dest, "coverage", source_key = keys[[1L]]),
        mixed[1L]
    )
    conn <- ddb_connect(file.path(dest, "manifest.duckdb"))
    ddb_exec(conn, "UPDATE subset_controls SET replica_cooldown=0")
    ddb_disconnect(conn)
    queries <- 0L
    query <- ddb_query
    local_mocked_bindings(ddb_query = function(conn, sql, ...) {
        if (grepl("SELECT * FROM subset_attempts WHERE", sql, fixed = TRUE)) {
            queries <<- queries + 1L
        }
        query(conn, sql, ...)
    })
    disabled <- subset_status(dest, "coverage")
    expect_true(all(is.na(disabled$retry_after)))
    expect_equal(queries, 0L)
})

# vim: fdm=marker :
