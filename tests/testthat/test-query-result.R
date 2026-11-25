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
        version = rep(20190529L, 3L),
        activity_id = rep("ScenarioMIP", 3L),
        institution_id = rep("AWI", 3L),
        dataset_id = c(dataset_ids[[1L]], dataset_ids[[1L]], dataset_ids[[2L]]),
        size = c(1024^2, 2 * 1024^2, 3 * 1024^2),
        checksum = c("sha-1", "sha-2", "sha-3"),
        checksum_type = rep("SHA256", 3L),
        instance_id = paste0("file-instance-", 1:3),
        master_id = paste0("file-master-", 1:3),
        replica = rep(FALSE, 3L),
        tracking_id = paste0("hdl:21.14100/mock-", 1:3),
        title = c(
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20500101-20501231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20510101-20511231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp245_r1i1p1f1_gn_20500101-20501231.nc"
        ),
        data_node = rep("esgf.example.org", 3L),
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
        instance_id = paste0("file-instance-", c("2050", "2080", "unknown")),
        master_id = paste0("master-file-", c("2050", "2080", "unknown")),
        replica = FALSE,
        version = rep(20260101L, 3L),
        tracking_id = paste0(
            "hdl:21.14100/mock-file-",
            c("2050", "2080", "unknown")
        ),
        title = c(
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20500101-20501231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20800101-20801231.nc",
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_unknown.nc"
        ),
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(as.list(paste0(
        "https://example.org/dods/",
        c("2050", "2080", "unknown"),
        ".nc.html|application/netcdf|OPENDAP"
    )))
    if (identical(type, "Aggregation")) {
        docs$checksum <- docs$checksum_type <- docs$master_id <- docs$tracking_id <- NULL
    }
    docs
}
# }}}

# Shared test mocks and DAP fixture helpers {{{
qr_test__mock_reach_nodes <- function(
    ok_fn,
    latency_fn = NULL,
    error_msg = "boom",
    log = NULL
) {
    function(
        data_node,
        timeout = 5,
        network_policy = NULL,
        probe_concurrency = 1L,
        cache_seconds = 3600L,
        cache_failures_seconds = 0L
    ) {
        if (!is.null(log)) {
            log$calls[[length(log$calls) + 1L]] <- list(
                data_node = data_node,
                timeout = timeout,
                network_policy = network_policy,
                probe_concurrency = probe_concurrency,
                cache_seconds = cache_seconds,
                cache_failures_seconds = cache_failures_seconds
            )
        }
        good <- ok_fn(data_node)
        lat <- if (is.null(latency_fn)) {
            ifelse(good, 10, NA_real_)
        } else {
            latency_fn(data_node)
        }
        data.table::data.table(
            data_node = data_node,
            reachable = good,
            latency_ms = lat,
            error = ifelse(good, NA_character_, error_msg),
            probe_url = paste0("https://", data_node, "/"),
            probe_cached = FALSE
        )
    }
}

qr_test__mock_reach_urls <- function(
    ok_fn,
    latency_ms = 10,
    error_msg = "unavailable",
    log = NULL
) {
    function(
        urls,
        service,
        timeout = 5,
        network_policy = NULL,
        concurrency = 1L,
        cache_seconds = 3600L,
        cache_failures_seconds = 0L
    ) {
        if (!is.null(log)) {
            log$urls <- c(log$urls, urls)
            log$timeouts <- c(log$timeouts, timeout)
            log$agents <- c(
                log$agents,
                if (is.null(network_policy$useragent)) {
                    NA_character_
                } else {
                    network_policy$useragent
                }
            )
        }
        missing <- is.na(urls) | !nzchar(urls)
        good <- !missing & ok_fn(urls, service)
        data.table::data.table(
            url = urls,
            reachable = data.table::fifelse(missing, NA, good),
            latency_ms = data.table::fifelse(good, latency_ms, NA_real_),
            error = data.table::fifelse(
                missing,
                "Missing URL.",
                data.table::fifelse(good, NA_character_, error_msg)
            ),
            probe_cached = FALSE
        )
    }
}

qr_test__fake_dataset_class <- function(calls = NULL) {
    R6::R6Class(
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
                if (!is.null(calls)) {
                    calls$opened[[length(calls$opened) + 1L]] <- self$target[
                        missing
                    ]
                    calls$open_progress <- c(calls$open_progress, progress)
                }
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
                if (!is.null(calls)) {
                    calls$closed[[
                        length(calls$closed) + 1L
                    ]] <- private$nc_handles
                }
                private$nc_handles <- vector("list", length(self$target))
                private$opened <- FALSE
                invisible(self)
            },
            get_time_axis = function(...) {
                if (grepl("unknown", self$target)) {
                    stop("no time axis", call. = FALSE)
                }
                yr <- if (grepl("2050", self$target)) "2050" else "2080"
                list(
                    values = as.POSIXct(
                        c(paste0(yr, "-01-01"), paste0(yr, "-12-31")),
                        tz = "UTC"
                    )
                )
            }
        ),
        active = list(is_open = function() private$opened),
        private = list(nc_handles = NULL, opened = FALSE, context = list())
    )
}

qr_test__dap_server <- function(..., env = parent.frame()) {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app(...))
    withr::defer(server$stop(), envir = env)
    server
}

qr_test__dap_files <- function(
    server,
    endpoints,
    nodes = NULL,
    ids = NULL,
    titles = NULL,
    variable_id = "tas",
    distinct = FALSE
) {
    n <- length(endpoints)
    docs <- query_result_test_file_docs()[rep(1L, n), , drop = FALSE]
    row.names(docs) <- NULL
    docs$variable_id <- variable_id
    if (!is.null(nodes)) {
        docs$data_node <- nodes
    }
    if (!is.null(ids)) {
        docs$id <- docs$instance_id <- docs$master_id <- ids
    } else if (isTRUE(distinct)) {
        docs$id <- docs$instance_id <- docs$master_id <- paste0(
            "file-",
            seq_len(n)
        )
    }
    if (!is.null(titles)) {
        docs$title <- titles
    }
    docs$url <- I(lapply(endpoints, function(ep) {
        paste0(server$url(), ep, "|application/netcdf|OPENDAP")
    }))
    query_result_test_object("File", docs)
}

qr_test__dap_request <- function(
    time_start = "2060-01-01",
    time_stop = time_start,
    request_id = if (length(time_start) == 1L) {
        "one"
    } else {
        paste0("req-", seq_along(time_start))
    },
    case_id = NULL,
    site_id = "city",
    lon = 10.25,
    lat = 0.25,
    ...
) {
    df <- data.frame(
        request_id = request_id,
        site_id = site_id,
        lon = lon,
        lat = lat,
        time_start = time_start,
        time_stop = time_stop,
        ...,
        stringsAsFactors = FALSE
    )
    if (!is.null(case_id)) {
        df$case_id <- case_id
    }
    df
}

qr_test__dap_traffic <- function(server) {
    jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
}

qr_test__frozen_plan <- function(files, requests, key, root) {
    plan <- file.path(root, "plan")
    dir.create(plan, recursive = TRUE, showWarnings = FALSE)
    catalogue <- files$save(file.path(plan, "catalogue.json"))
    selection <- data.frame(
        source_key = key,
        owner = "pc",
        batch = "pc-fixture",
        catalogue = basename(catalogue),
        catalogue_sha256 = checksum_file(catalogue),
        record_indices = "[1]"
    )
    selected <- file.path(plan, "files.parquet")
    requested <- file.path(plan, "requests.parquet")
    write_parquet_file(selection, selected)
    write_parquet_file(requests, requested)
    metadata <- list(
        files_sha256 = checksum_file(selected),
        requests_sha256 = checksum_file(requested),
        capacities = list(pc = 1L)
    )
    jsonlite::write_json(
        metadata,
        file.path(plan, "manifest.json"),
        auto_unbox = TRUE
    )
    denominator <- file.path(root, "denominator.rds")
    saveRDS(
        list(
            source_files_sha256 = metadata$files_sha256,
            requests_sha256 = metadata$requests_sha256,
            demands = cbind(
                requests,
                selection[
                    rep(1L, nrow(requests)),
                    c("source_key", "owner", "batch")
                ]
            )
        ),
        denominator
    )
    journal <- file.path(root, "journal")
    dir.create(journal, recursive = TRUE, showWarnings = FALSE)
    state <- opendap_recovery__persist(
        list(
            generation = 0L,
            started = as.double(Sys.time()),
            configuration = list(
                version = 2L,
                identity = list(
                    node = "pc",
                    files = metadata$files_sha256,
                    requests = metadata$requests_sha256,
                    denominator = checksum_file(denominator)
                ),
                max_rounds = Inf,
                max_wait = Inf,
                max_elapsed = Inf
            ),
            phase = "blocked_error",
            round = 1L,
            admissions = 1L,
            wait_seconds = 0,
            attempted = c("pc-fixture" = FALSE),
            in_flight = list(
                batch = "pc-fixture",
                source_keys = key,
                started = as.double(Sys.time())
            )
        ),
        journal
    )
    list(
        plan = plan,
        denominator = denominator,
        journal = journal,
        state = state
    )
}
# }}}

# EsgResult$load() / EsgResult$save() / EsgResult$query_url() {{{
test_that("EsgResult$load() / $save() validate types, restore dynamic fields, and round-trip JSON", {
    # Mocked load restores dynamic fields and validates result type
    local({
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

        state <- query_result_test_state(
            "File",
            query_result_test_file_docs(),
            query_result_test_params("File")
        )
        expect_error(
            esg_result("dataset")$load("file.json"),
            "Cannot load 'File' result"
        )
        expect_s3_class(esg_result("file")$load("file.json"), "EsgResultFile")
    })

    # Reject untyped base results
    base_res <- query_result__new(
        EsgResult,
        index_node = "https://example.org",
        params = query_result_test_params("Dataset"),
        result = query_result_test_response(query_result_test_dataset_docs())
    )
    f_untyped <- tempfile(fileext = ".json")
    on.exit(unlink(f_untyped), add = TRUE)
    expect_error(base_res$save(f_untyped), "untyped ESGF result")
    writeLines("{}", f_untyped)
    expect_error(base_res$load(f_untyped), "untyped ESGF result")
})

test_that("EsgResult$save() / $load() round-trip Dataset/File/Aggregation with timestamps, publish_path, and query_url", {
    both_urls <- c(
        "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    )
    file_docs <- query_result_test_file_docs(both_urls)
    file_docs$timestamp <- "2026-06-09T00:00:00Z"
    file_docs$`_timestamp` <- "2026-06-10T01:14:40.946Z"
    file_docs$version <- "v20240509"
    file_docs$publish_path <- "/css03_data/CMIP6/ScenarioMIP/mock/file.nc"

    cases <- list(
        dataset = list(
            type = "Dataset",
            docs = query_result_test_dataset_docs()
        ),
        file = list(type = "File", docs = file_docs),
        aggregation = list(
            type = "Aggregation",
            docs = query_result_test_file_docs(both_urls)
        )
    )
    urls <- c(
        "https://example.org/search?page=1",
        "https://example.org/search?page=2"
    )

    for (case_name in names(cases)) {
        case <- cases[[case_name]]
        result <- query_result_test_object(
            case$type,
            case$docs,
            query_result_test_params(case$type),
            context = list(query_url = urls)
        )
        file <- tempfile(fileext = ".json")
        on.exit(unlink(file), add = TRUE)

        expect_type(result$save(file), "character")
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
        expect_identical(
            loaded$query_url(),
            stats::setNames(urls[[1L]], "page1")
        )
        expect_identical(
            loaded$query_url("all"),
            stats::setNames(urls, c("page1", "page2"))
        )
        expect_error(loaded$to_data_table(character()), "Must have length")
        if (case_name == "file") {
            expect_identical(loaded$timestamp, "2026-06-09T00:00:00Z")
            expect_identical(loaded$`_timestamp`, "2026-06-10T01:14:40.946Z")
            expect_identical(loaded$version, "v20240509")
            expect_identical(loaded$publish_path, file_docs$publish_path)
        }
    }

    # Default query_url() builds from params and rejects unknown 'which'
    params <- query_result_test_params("Dataset")
    ds <- query_result_test_object(
        "Dataset",
        query_result_test_dataset_docs(),
        params
    )
    expect_identical(
        ds$query_url(),
        stats::setNames(query__build("https://example.org", params), "page1")
    )
    expect_error(ds$query_url("last"), "'arg' should be one of")
})
# }}}

# EsgResult$slice() / EsgResult$selection() / EsgResult$filter() {{{
test_that("EsgResult$slice(), $selection(), and $filter() support local selection, validation, and persistence", {
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
        n <- nrow(cases[[type]])

        expect_identical(result[], result)
        expect_identical(
            result$selection(),
            list(
                source_count = as.integer(n),
                source_num_found = 10L,
                source_indices = seq_len(n)
            )
        )

        selected <- result[c(n, 1L)]
        expect_identical(selected$id, result$id[c(n, 1L)])
        expect_identical(
            selected$query_url("all"),
            stats::setNames(urls, c("page1", "page2"))
        )
        expect_identical(selected$time_filter, time_filter)
        expect_identical(selected[2L]$id, result$id[1L])
        expect_identical(selected[2L]$selection()$source_indices, 1L)
        expect_identical(
            result[seq_len(n) %% 2L == 1L]$id,
            result$id[seq_len(n) %% 2L == 1L]
        )
        expect_identical(result[result$id[1L]]$id, result$id[1L])
        expect_identical(result[-1L]$id, result$id[-1L])
        expect_identical(
            result$slice(NULL)$selection()$source_indices,
            integer()
        )
    }

    # Filter predicates, invalid selectors, and selection save/load persistence
    result <- query_result_test_object(
        "File",
        query_result_test_file_time_docs("File"),
        query_result_test_params("File")
    )
    priv(result)$response$response$numFound <- 10L
    filtered <- result$filter(function(dt) grepl("2050", dt$title))
    expect_identical(filtered$id, result$id[1L])
    expect_identical(filtered$selection()$source_indices, 1L)
    expect_identical(
        result$filter(function(dt) rep(TRUE, nrow(dt)), formatted = TRUE)$id,
        result$id
    )

    expect_error(result$filter(function(dt) TRUE), "predicate result")
    expect_error(
        result$filter(function(dt) c(TRUE, NA, FALSE)),
        "predicate result"
    )
    expect_error(
        result$filter(function(dt) seq_len(nrow(dt))),
        "predicate result"
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

    for (sel in list(result[c(3L, 1L)], result$slice(integer()))) {
        f <- tempfile(fileext = ".json")
        on.exit(unlink(f), add = TRUE)
        expect_type(sel$save(f), "character")
        loaded <- esg_result("file")$load(f)
        if (sel$count() > 0L) {
            expect_identical(loaded$id, sel$id)
        }
        expect_identical(loaded$selection(), sel$selection())
    }
})
# }}}

# query_result__run_url_checks() & EsgResult$reachable() {{{
test_that("concurrent URL check wrappers preserve HTTP and OPeNDAP DDS semantics", {
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

    reachable <- query_result__reach_http_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_true(reachable[[ok_url]]$reachable)
    expect_false(reachable[[missing_url]]$reachable)
    expect_match(reachable[[missing_url]]$error, "404")

    latency <- query_result__latency_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_true(all(vapply(
        latency,
        function(x) is.finite(x$latency) && is.na(x$throughput),
        logical(1L)
    )))

    # OPeNDAP URL checks require a valid DDS response
    valid_s <- local_downloader_http_server()
    html_s <- local_downloader_http_server()
    valid <- valid_s$url("/dods/valid.nc")
    html <- html_s$url("/dods/html.nc")
    missing_dap <- missing_server$url("/dods/missing.nc")
    checked <- query_result__check_opendap_urls(
        c(valid, html, missing_dap),
        timeout = 5,
        concurrency = 3L
    )
    expect_true(checked[[valid]]$reachable)
    expect_false(checked[[html]]$reachable)
    expect_match(checked[[html]]$error, "valid DDS")
    expect_false(checked[[missing_dap]]$reachable)
    expect_match(checked[[missing_dap]]$error, "404")
    expect_true(query_result__valid_dds(charToRaw(
        "Dataset { Float32 tas[time = 1]; } valid.nc;"
    )))
    expect_false(query_result__valid_dds(charToRaw("<html>error</html>")))
})

test_that("EsgResult$reachable() returns URL-level and default data-node diagnostics", {
    docs <- query_result_test_file_docs()[rep(1L, 4L), , drop = FALSE]
    row.names(docs) <- NULL
    docs$id <- c("file-ok", "file-dup", "file-missing", "file-fail")
    docs$data_node <- c(
        "same.example.org",
        "same.example.org",
        "missing.example.org",
        "bad.example.org"
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

    log <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(
            function(u, s) !grepl("bad", u),
            latency_ms = 125,
            error_msg = "boom",
            log = log
        ),
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
    expect_identical(diag$reachable, c(TRUE, TRUE, NA, FALSE))
    expect_identical(
        diag$error,
        c(NA_character_, NA_character_, "Missing URL.", "boom")
    )
    expect_true(all(log$timeouts == 9) && all(log$agents == "test-agent"))
    expect_identical(
        result$slice(diag$reachable %in% TRUE)$id,
        c("file-ok", "file-dup")
    )

    http <- result$reachable(service = "HTTPServer", level = "url")
    expect_identical(http$reachable, c(TRUE, NA, NA, NA))
    expect_equal(nrow(result$slice(integer())$reachable()), 0L)

    # Default data_node level probes node roots once per host and falls back to URL host when data_node is NA
    docs$data_node[[4L]] <- NA_character_
    docs$url[[
        4L
    ]] <- "https://fallback.example.org/dods/file.nc|application/netcdf|OPENDAP"
    node_result <- query_result_test_object(
        "File",
        docs,
        query_result_test_params("File")
    )
    node_calls <- character()
    testthat::local_mocked_bindings(
        query_result__reach_node_urls = function(urls, ...) {
            node_calls <<- c(node_calls, urls)
            stats::setNames(
                lapply(urls, function(u) {
                    list(
                        reachable = TRUE,
                        latency_ms = 22,
                        error = NA_character_,
                        probe_url = u
                    )
                }),
                urls
            )
        },
        .package = "epwshiftr"
    )
    node_diag <- node_result$reachable(
        probe = list(cache_seconds = 0L, cache_failures_seconds = 0L)
    )
    expect_identical(node_diag$probe_level, rep("data_node", 4L))
    expect_identical(
        node_diag$probe_url,
        c(
            "https://same.example.org/",
            "https://same.example.org/",
            NA_character_,
            "https://fallback.example.org/"
        )
    )
    expect_equal(sum(node_calls == "https://same.example.org/"), 1L)
    expect_false(any(grepl("/dods/|missing\\.example\\.org", node_calls)))
})
# }}}

# EsgResult$repair_urls(), $expand_replicas(), and File service resolution {{{
test_that("EsgResult$repair_urls() repairs OPeNDAP, HTTPServer, and Aggregation URLs and prefers current replicas", {
    # External OPeNDAP replica repair with probe forwarding and query_url recording
    docs <- query_result_test_file_docs()[c(1L, 1L), , drop = FALSE]
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

    candidate <- query_result_test_file_docs(c(
        "https://replica.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://replica.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    candidate$id <- "file-repaired"
    candidate$master_id <- "master-file-bad"
    candidate$instance_id <- "instance-bad"
    candidate$data_node <- "replica.example.org"
    candidate$replica <- TRUE

    probe_log <- new.env(parent = emptyenv())
    probe_log$calls <- list()
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(
            function(dn) grepl("ok|replica", dn),
            latency_fn = function(dn) {
                ifelse(
                    grepl("replica", dn),
                    10,
                    ifelse(grepl("ok", dn), 50, NA_real_)
                )
            },
            log = probe_log
        ),
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            ...
        ) {
            expect_identical(index_node, "https://replica-index.example.org")
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$instance_id),
                "instance-bad"
            )
            list(
                response = query_result_test_response(candidate),
                docs = candidate,
                parameter = query_param__clone(params),
                context = list(
                    query_url = "https://replica-index.example.org/replicas"
                )
            )
        },
        .package = "epwshiftr"
    )
    repaired <- result$repair_urls(
        index_node = "replica-index.example.org",
        probe = list(
            timeout = 11,
            concurrency = 2L,
            network_policy = list(useragent = "repair-test")
        )
    )
    expect_identical(
        repaired$url_opendap[[1L]],
        "https://replica.example.org/dods/file.nc"
    )
    expect_identical(
        repaired$url_download[[1L]],
        "https://bad.example.org/file.nc"
    )
    expect_identical(
        unname(repaired$query_url("all")),
        c(
            "https://origin.example.org/search",
            "https://replica-index.example.org/replicas"
        )
    )
    expect_true(all(vapply(
        probe_log$calls,
        function(x) x$timeout == 11 && x$probe_concurrency == 2L,
        logical(1L)
    )))

    # Prefer reachable replica already in current result without external query
    curr_docs <- docs
    curr_docs$master_id <- "master-current"
    curr_docs$instance_id <- "instance-current"
    curr_docs$data_node <- c("bad.example.org", "good.example.org")
    curr_docs$url <- I(list(
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://good.example.org/dods/file.nc|application/netcdf|OPENDAP"
    ))
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            dn == "good.example.org"
        }),
        query__collect = function(...) {
            stop("current result replica should avoid external query")
        },
        .package = "epwshiftr"
    )
    curr_repaired <- query_result_test_object("File", curr_docs)$repair_urls()
    expect_identical(
        curr_repaired$url_opendap,
        rep("https://good.example.org/dods/file.nc", 2L)
    )

    # Independent HTTPServer repair and Aggregation repair
    http_docs <- query_result_test_file_docs(c(
        "https://opendap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://bad-http.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    http_cand <- query_result_test_file_docs(c(
        "https://opendap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://http-replica.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    http_cand$data_node <- "http-replica.example.org"
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(function(
            u,
            s
        ) {
            grepl("http-replica", u)
        }),
        query__collect = function(index_node, params, ...) {
            list(
                response = query_result_test_response(http_cand),
                docs = http_cand,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/http-replicas")
            )
        },
        .package = "epwshiftr"
    )
    http_rep <- query_result_test_object("File", http_docs)$repair_urls(
        service = "HTTPServer",
        probe = list(level = "url")
    )
    expect_identical(
        http_rep$url_opendap,
        "https://opendap.example.org/dods/file.nc"
    )
    expect_identical(
        http_rep$url_download,
        "https://http-replica.example.org/file.nc"
    )

    agg_docs <- query_result_test_file_docs(
        "https://bad.example.org/dods/agg.nc|application/netcdf|OPENDAP"
    )
    agg_docs$data_node <- "bad.example.org"
    agg_cand <- query_result_test_file_docs(
        "https://agg-replica.example.org/dods/agg.nc|application/netcdf|OPENDAP"
    )
    agg_cand$data_node <- "agg-replica.example.org"
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            grepl("agg-replica", dn)
        }),
        query__collect = function(index_node, params, ...) {
            expect_identical(query_param__value(params$type()), "Aggregation")
            list(
                response = query_result_test_response(agg_cand),
                docs = agg_cand,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/agg-replicas")
            )
        },
        .package = "epwshiftr"
    )
    agg_rep <- query_result_test_object(
        "Aggregation",
        agg_docs,
        query_result_test_params("Aggregation")
    )$repair_urls()
    expect_s3_class(agg_rep, "EsgResultAggregation")
    expect_identical(
        agg_rep$url_opendap,
        "https://agg-replica.example.org/dods/agg.nc"
    )

    # Keeps originals and warns when repair is impossible; rejects unknown probe fields
    unrep_docs <- docs
    unrep_docs$master_id <- c(NA_character_, "master-no-replica")
    unrep_docs$instance_id <- c(NA_character_, NA_character_)
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            rep(FALSE, length(dn))
        }),
        query__collect = function(index_node, params, ...) {
            list(
                response = query_result_test_response(unrep_docs[
                    2L,
                    ,
                    drop = FALSE
                ]),
                docs = unrep_docs[2L, , drop = FALSE],
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/no-replica")
            )
        },
        .package = "epwshiftr"
    )
    unrep_res <- query_result_test_object("File", unrep_docs)
    warnings <- character()
    kept <- withCallingHandlers(
        unrep_res$repair_urls(),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_true(any(grepl("instance_id", warnings)))
    expect_true(any(grepl("No reachable OPENDAP replica", warnings)))
    expect_identical(kept$id, unrep_res$id)
    expect_error(
        unrep_res$repair_urls(probe = list(foo = 1)),
        "Unknown `probe` field"
    )
})

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

    cand <- docs[c(1L, 1L), , drop = FALSE]
    cand$id <- c("file-same-version", "file-other-version")
    cand$version <- c(20260101L, 20270101L)
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, ...) {
            expect_null(params$project())
            expect_identical(
                query_param__value(params$params()$master_id),
                "master-file-fallback"
            )
            list(
                response = query_result_test_response(cand),
                docs = cand,
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

test_that("File service resolution composes compatible replicas, collapses duplicates, and defers HTTP checks", {
    orig <- query_result_test_file_docs(c(
        "https://bad.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://bad.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    orig$id <- "file-original"
    orig$instance_id <- "instance-shared"
    orig$master_id <- "master-shared"
    orig$data_node <- "bad.example.org"

    dap <- orig
    dap$id <- "file-dap"
    dap$data_node <- "dap.example.org"
    dap$url <- I(list(c(
        "https://dap.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://dap.example.org/files/file.nc|application/netcdf|HTTPServer"
    )))
    http <- orig
    http$id <- "file-http"
    http$data_node <- "http.example.org"
    http$url <- I(list(c(
        "https://http.example.org/dods/file.nc|application/netcdf|OPENDAP",
        "https://http.example.org/files/file.nc|application/netcdf|HTTPServer"
    )))
    cands <- data.table::rbindlist(
        list(dap, http),
        use.names = TRUE,
        fill = TRUE
    )

    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(function(
            u,
            s
        ) {
            if (identical(s, "OPENDAP")) {
                grepl("dap.example.org", u, fixed = TRUE)
            } else {
                grepl("http.example.org", u, fixed = TRUE)
            }
        }),
        query__collect = function(index_node, params, ...) {
            list(
                response = query_result_test_response(cands),
                docs = cands,
                parameter = query_param__clone(params),
                context = list(query_url = "https://example.org/replicas")
            )
        },
        .package = "epwshiftr"
    )

    resolved <- query_result__resolve_file_services(
        query_result_test_object("File", orig),
        check = list(
            level = "url",
            cache_seconds = 0L,
            cache_failures_seconds = 0L
        )
    )
    expect_identical(
        resolved$result$url_opendap,
        "https://dap.example.org/dods/file.nc"
    )
    expect_identical(
        resolved$result$url_download,
        "https://bad.example.org/file.nc"
    )
    expect_true(all(
        resolved$diagnostics[service == "HTTPServer", probe_level] == "deferred"
    ))

    incompatible <- data.table::copy(cands)
    incompatible$checksum[[2L]] <- "different"
    expect_identical(
        query_result__compatible_content(orig, incompatible),
        c(TRUE, FALSE)
    )
    expect_identical(
        query_result__compatible_file_groups(incompatible),
        c(1L, 2L)
    )

    # Collapses compatible distributed replicas and retains unchecked HTTP recovery candidates
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(result, ...) result,
        .package = "epwshiftr"
    )
    collapsed <- query_result__resolve_file_services(
        query_result_test_object("File", cands),
        check = list(level = "url", cache_seconds = 0L)
    )
    expect_equal(collapsed$result$count(), 1L)
    expect_match(collapsed$result$url_opendap, "dap\\.example\\.org")

    dead_and_http <- data.table::rbindlist(
        list(orig, http),
        use.names = TRUE,
        fill = TRUE
    )
    dead_and_http$instance_id <- c("inst-dead", "inst-http")
    dead_and_http$master_id <- c("m-dead", "m-http")
    retained <- query_result__resolve_file_services(
        query_result_test_object("File", dead_and_http),
        check = list(level = "url", cache_seconds = 0L)
    )
    expect_true(all(is.na(retained$result$url_opendap)))
    expect_identical(
        retained$result$url_download,
        c(
            "https://bad.example.org/file.nc",
            "https://http.example.org/files/file.nc"
        )
    )

    # URL check cache keys distinguish HTTP and OPeNDAP contracts
    expect_false(identical(
        query_result__reach_cache_key(
            "url:OPENDAP",
            "https://example.org/a.nc",
            timeout = 5
        ),
        query_result__reach_cache_key(
            "url:HTTPSERVER",
            "https://example.org/a.nc",
            timeout = 5
        )
    ))
})
# }}}

# EsgResult$filter_time(), $fields, and $to_data_table() {{{
test_that("EsgResult$filter_time() supports DRS, auto, OPeNDAP time axes, and persistence", {
    # Time-range normalization and paired fallback
    short <- list(datetime_start = "2050-01-01T00:00:00Z")
    expect_identical(
        query_result__character_column(short, "datetime_start", 3L),
        c("2050-01-01T00:00:00Z", NA_character_, NA_character_)
    )
    expect_identical(
        query_result__character_column(short, "datetime_end", 3L),
        rep(NA_character_, 3L)
    )
    meta_docs <- data.frame(
        datetime_start = c(
            "2040-01-01T00:00:00Z",
            "2059-06-01T00:00:00Z",
            NA_character_
        ),
        datetime_end = c("2040-12-31T23:59:59Z", NA_character_, NA_character_),
        check.names = FALSE
    )
    ranges <- query_result__fill_time_ranges(meta_docs, function() {
        c(
            "tas_day_M_x_20400101-20401231.nc",
            "tas_day_M_x_20500101-20501231.nc",
            "tas_day_M_x_unknown.nc"
        )
    })
    expect_identical(
        format(ranges$datetime_start[[2L]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "2050-01-01T00:00:00Z"
    )
    expect_true(is.na(ranges$datetime_start[[3L]]))
    expect_silent(query_result__fill_time_ranges(
        meta_docs[1L, , drop = FALSE],
        function() stop("should not run")
    ))

    # DRS filtering across File & Aggregation, silent when all parseable
    for (type in c("File", "Aggregation")) {
        result <- query_result_test_object(
            type,
            query_result_test_file_time_docs(type),
            query_result_test_params(type)
        )
        expect_warning(
            filtered <- result$filter_time(
                "2050-06-01",
                "2050-06-30",
                method = "drs"
            ),
            sprintf(
                "Could not parse a DRS time range for 1 %s record",
                tolower(type)
            )
        )
        expect_identical(filtered$id, result$id[c(1L, 3L)])
        expect_identical(filtered$time_filter$unknown_count, 1L)
        expect_identical(filtered$selection()$source_indices, c(1L, 3L))
    }

    silent_res <- query_result_test_object(
        "File",
        query_result_test_file_time_docs("File")[1:2, , drop = FALSE]
    )
    expect_identical(
        expect_silent(silent_res$filter_time(
            "2050-06-01",
            "2050-06-30",
            method = "drs"
        ))$time_filter$unknown_count,
        0L
    )

    # Auto mode preserves metadata and fills DRS gaps, and persists via save/load
    auto_docs <- query_result_test_file_time_docs("File")
    auto_docs$datetime_start <- c(
        "2050-01-01T00:00:00Z",
        NA_character_,
        NA_character_
    )
    auto_docs$datetime_end <- c(
        "2050-12-31T23:59:59Z",
        NA_character_,
        NA_character_
    )
    expect_warning(
        auto_flt <- query_result_test_object("File", auto_docs)$filter_time(
            "2050-06-01",
            "2050-06-30",
            method = "auto"
        ),
        "metadata or DRS"
    )
    expect_identical(auto_flt$id, auto_docs$id[c(1L, 3L)])

    f <- tempfile(fileext = ".json")
    on.exit(unlink(f), add = TRUE)
    auto_flt$save(f)
    loaded <- esg_result("file")$load(f)
    expect_identical(loaded$time_filter, auto_flt$time_filter)
    expect_identical(loaded$selection(), auto_flt$selection())

    # OPeNDAP time axis filtering
    testthat::local_mocked_bindings(
        EsgDataset = qr_test__fake_dataset_class(),
        .package = "epwshiftr"
    )
    expect_warning(
        dap_flt <- query_result_test_object(
            "File",
            query_result_test_file_time_docs()
        )$filter_time("2050-06-01", "2050-06-30", method = "opendap"),
        "OPeNDAP time axes"
    )
    expect_identical(
        dap_flt$id,
        query_result_test_file_time_docs()$id[c(1L, 3L)]
    )
    expect_identical(dap_flt$time_filter$method, "opendap")
})

test_that("EsgResult$fields and $to_data_table() expose stable fields for empty and populated results", {
    for (type in c("Dataset", "File", "Aggregation")) {
        expect_identical(
            query_result_test_object(
                type,
                data.frame(check.names = FALSE)
            )$fields,
            character()
        )
    }
    empty_file <- query_result_test_object(
        "File",
        query_result_test_file_docs(character())[0L, ]
    )
    expect_true(all(
        c("filename", "url_opendap", "url_download") %in% empty_file$fields
    ))

    docs <- query_result_test_file_docs(c(
        "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    ))
    files <- query_result_test_object("File", docs)
    expect_identical(
        files$fields,
        c(names(docs), "filename", "url_opendap", "url_download")
    )
    expect_identical(
        names(files$to_data_table(c("url_download", "id", "filename"))),
        c("url_download", "id", "filename")
    )
    expect_identical(
        files$to_data_table("url_opendap")$url_opendap,
        "https://example.org/dods/file.nc"
    )
    expect_error(files$to_data_table(character()), "Must have length")

    aggs <- query_result_test_object(
        "Aggregation",
        docs,
        query_result_test_params("Aggregation")
    )
    expect_identical(aggs$fields, c(names(docs), "url_opendap", "url_download"))
    expect_identical(names(aggs$to_data_table(aggs$fields)), aggs$fields)
})
# }}}

# EsgResultDataset {{{
test_that("EsgResultDataset handles empty child save/load, empty collect, and missing access fields", {
    empty_file_docs <- query_result_test_file_docs(character())[0L, ]
    for (case_name in c("file", "aggregation")) {
        type <- switch(case_name, file = "File", aggregation = "Aggregation")
        result <- query_result_test_object(
            type,
            empty_file_docs,
            query_result_test_params(type)
        )
        f <- tempfile(fileext = ".json")
        on.exit(unlink(f), add = TRUE)
        result$save(f)
        loaded <- esg_result(case_name)$load(f)
        expect_equal(loaded$count(), 0L)
        expect_identical(loaded$fields, character())
    }

    empty_ds <- query_result_test_object(
        "Dataset",
        data.frame(id = character(), size = numeric())
    )
    testthat::local_mocked_bindings(
        query__collect = function(...) stop("should not query"),
        .package = "epwshiftr"
    )
    expect_error(
        empty_ds$collect(which = 1L, type = "File"),
        "empty Dataset result"
    )
    expect_equal(empty_ds$collect(type = "File")$count(), 0L)
    expect_equal(empty_ds$collect(type = "Aggregation")$count(), 0L)

    no_access <- query_result_test_object(
        "Dataset",
        query_result_test_dataset_docs(access = FALSE)
    )
    expect_identical(no_access$has_opendap(), c(FALSE, FALSE))
    expect_identical(no_access$has_download(), c(FALSE, FALSE))
})

test_that("EsgResultDataset$collect() inherits controls, clears datetime constraints, ignores record index_node, and validates params", {
    params <- query_result_test_params(
        "Dataset",
        latest = FALSE,
        distrib = FALSE,
        replica = FALSE
    )
    params$source_id("AWI-CM-1-1-MR")
    params$datetime_range(
        start = "2050-01-01T00:00:00Z",
        stop = "2080-12-31T23:59:59Z"
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(
            id = c("dataset-1", "dataset-2"),
            index_node = c("us-index", "https://idx-b.example.org"),
            size = c(1, 1),
            check.names = FALSE
        ),
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
            ...
        ) {
            dataset_id <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                limit = limit,
                all = all,
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
            params$fields(unique(c(
                query_param__value(params$fields()),
                required_fields
            )))
            list(
                response = query_result_test_response(docs),
                docs = docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    files <- datasets$collect(
        1L,
        fields = "id",
        limit = NULL,
        data_node = "example.org"
    )
    expect_s3_class(files, "EsgResultFile")
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param__value(calls[[1L]]$params$latest()))
    expect_false(query_param__value(calls[[1L]]$params$distrib()))
    expect_identical(
        query_param__value(calls[[1L]]$params$data_node()),
        "example.org"
    )
    expect_identical(
        calls[[1L]]$params$render(c("datetime_start", "datetime_stop")),
        character()
    )
    expect_null(calls[[1L]]$params$project())
    expect_null(calls[[1L]]$params$source_id())

    # Overrides, Aggregation target, and ignoring record index_node metadata
    aggs <- datasets$collect(
        fields = "id",
        limit = 1L,
        type = "Aggregation",
        latest = TRUE,
        distrib = TRUE,
        replica = TRUE,
        index_node = "esg-dn1.nsc.liu.se"
    )
    expect_s3_class(aggs, "EsgResultAggregation")
    expect_identical(calls[[2L]]$index_node, "https://esg-dn1.nsc.liu.se")
    expect_identical(aggs$count(), 1L)
    expect_true(query_param__value(calls[[2L]]$params$latest()))

    expect_error(
        datasets$collect(source_id = "AWI-CM-1-1-MR"),
        "unsupported parameter"
    )
    expect_error(datasets$collect(bbox = "0,0,1,1"), "unsupported parameter")
    expect_error(datasets$collect(start = "2050"), "unsupported parameter")
    expect_error(datasets$collect(datetime_start = "2050"), "controlled")
    expect_error(datasets$collect(time = "all"), "controlled")
})

test_that("EsgResultDataset$collect() batches large and long-ID queries, enforces global limits, and forwards progress", {
    dataset_id <- sprintf(
        "dataset-%03d",
        seq_len(QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE * 2L + 5L)
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = dataset_id, size = seq_along(dataset_id))
    )
    callback <- function(event) invisible(event)
    priv(datasets)$progress_callback <- callback

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
            progress = FALSE,
            progress_label = NULL,
            progress_callback = NULL
        ) {
            batch <- query_param__value(params$state()$dataset_id)
            param_limit <- query_param__value(params$limit())
            take <- if (isTRUE(all)) {
                length(batch)
            } else {
                min(length(batch), as.integer(param_limit))
            }
            calls[[length(calls) + 1L]] <<- list(
                ids = batch,
                limit = limit,
                param_limit = param_limit,
                dict_check = dict_check,
                progress = progress,
                progress_label = progress_label,
                callback = progress_callback
            )
            docs <- data.frame(
                id = paste0("file-", batch[seq_len(take)]),
                dataset_id = batch[seq_len(take)],
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
            list(
                response = query_result_test_response(docs),
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

    files <- datasets$collect(fields = "id", all = TRUE, progress = TRUE)
    expect_length(calls, 3L)
    expect_equal(
        vapply(calls, function(c) length(c$ids), integer(1L)),
        c(50L, 50L, 5L)
    )
    expect_true(calls[[1L]]$dict_check)
    expect_false(any(vapply(calls[-1L], `[[`, logical(1L), "dict_check")))
    expect_identical(calls[[1L]]$callback, callback)
    expect_identical(
        vapply(calls, `[[`, character(1L), "progress_label"),
        sprintf("Collecting File records (batch %d/3)", 1:3)
    )
    expect_equal(files$count(), length(dataset_id))
    expect_length(priv(files)$context$query_url, 3L)

    # Global limit stops across batches
    calls <- list()
    limited <- datasets$collect(fields = "id", limit = 55L)
    expect_length(calls, 2L)
    expect_equal(vapply(calls, `[[`, integer(1L), "limit"), c(55L, 5L))
    expect_equal(limited$count(), 55L)

    # Short Dataset lists with long IDs still split by URL byte budget
    long_ids <- paste0(
        strrep("long.model.", 50L),
        seq_len(6L),
        "|node.example.org"
    )
    long_ds <- query_result_test_object(
        "Dataset",
        data.frame(id = long_ids, size = 1)
    )
    calls <- list()
    long_files <- long_ds$collect(limit = 4L)
    expect_gt(length(calls), 1L)
    expect_equal(long_files$count(), 4L)
    expect_equal(vapply(calls, `[[`, integer(1L), "limit"), c(4L, 2L))
})

test_that("EsgResultDataset$expand_replicas() queries dataset replicas by instance_id and master_id", {
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
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, ...) {
            out <- docs[c(1L, 1L), , drop = FALSE]
            if (!is.null(params$params()$instance_id)) {
                out$data_node <- c("node-a.example.org", "node-b.example.org")
            } else {
                out$version <- c(20260101L, 20270101L)
            }
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
    expect_identical(
        datasets$expand_replicas(
            index_node = "replica-index.example.org"
        )$data_node,
        c("node-a.example.org", "node-b.example.org")
    )
    expect_identical(
        datasets$expand_replicas(
            by = "master_id",
            index_node = "replica-index.example.org"
        )$version,
        c(20260101L, 20270101L)
    )
})
# }}}

# EsgResultFile URLs, service preflight checks, and download_plan() {{{
test_that("EsgResultFile/Aggregation url_opendap & url_download preserve length and warn with context", {
    docs <- query_result_test_file_docs()[rep(1L, 4L), , drop = FALSE]
    docs$id <- paste0("file-", 1:4)
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
    for (type in c("File", "Aggregation")) {
        obj <- query_result_test_object(
            type,
            docs,
            query_result_test_params(type)
        )
        expect_identical(
            obj$url_download,
            c(
                NA_character_,
                NA_character_,
                "https://example.org/file-3.nc",
                "https://example.org/file-4.nc"
            )
        )
        expect_warning(dap <- obj$url_opendap, "record 4 \\(id: file-4\\)")
        expect_identical(
            dap,
            c(
                NA_character_,
                NA_character_,
                NA_character_,
                "https://example.org/dods/file-4"
            )
        )
    }

    nested <- query_result_test_file_docs(c(
        "https://example.org/dods/file-1.html|application/netcdf|OPENDAP",
        "https://example.org/dods/file-1-replica.html|application/netcdf|OPENDAP"
    ))
    nested$id <- I(list(list(NA_character_, character())))
    nested$dataset_id <- I(list(c("dataset-1", "extra-context")))
    expect_warning(
        expect_identical(
            query_result_test_object("File", nested)$url_opendap,
            "https://example.org/dods/file-1"
        ),
        "record 1 \\(dataset_id: dataset-1\\)"
    )
})

test_that("sampled service preflight checks sample per node and keep missing-metadata hosts separate", {
    make_row <- function(i, node, replica = FALSE) {
        doc <- query_result_test_file_docs(c(
            sprintf(
                "https://%s/dods/file-%d.nc|application/netcdf|OPENDAP",
                node,
                i
            ),
            sprintf(
                "https://%s/files/file-%d.nc|application/netcdf|HTTPServer",
                node,
                i
            )
        ))
        doc$id <- sprintf("file-%d-%s", i, node)
        doc$instance_id <- sprintf("instance-%d", i)
        doc$master_id <- sprintf("master-%d", i)
        doc$tracking_id <- sprintf("tracking-%d", i)
        doc$checksum <- sprintf("checksum-%d", i)
        doc$title <- sprintf("file-%d.nc", i)
        doc$size <- 100 + i
        doc$variable_id <- sprintf("var-%d", i)
        doc$data_node <- node
        doc$replica <- replica
        doc
    }
    docs <- data.table::rbindlist(
        unlist(
            lapply(1:6, function(i) {
                list(
                    make_row(i, "bad.example.org", FALSE),
                    make_row(i, "good.example.org", TRUE)
                )
            }),
            recursive = FALSE
        ),
        use.names = TRUE,
        fill = TRUE
    )
    log <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(...) {
            stop("must not run exhaustive repair")
        },
        query_result__reach_service_urls = qr_test__mock_reach_urls(
            function(u, s) grepl("good.example.org", u, fixed = TRUE),
            log = log
        ),
        .package = "epwshiftr"
    )
    resolved <- query_result__resolve_file_services(
        query_result_test_object("File", docs),
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
    expect_length(log$urls, 6L)
    expect_true(all(
        resolved$diagnostics[service == "OPENDAP", probe_level] ==
            "service_node_sample"
    ))

    # Missing metadata falls back to URL host, fails whole host on partial failure, and isolates distinct hosts
    host_docs <- data.table::rbindlist(lapply(1:4, function(i) {
        r <- query_result_test_file_docs(sprintf(
            "https://%s/f%d.nc|application/netcdf|OPENDAP",
            if (i <= 2L) "healthy.example.org" else "partial.example.org",
            i
        ))
        r$id <- paste0("id", i)
        r$data_node <- NA_character_
        r$variable_id <- NA_character_
        r
    }))
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = function(urls, ...) {
            good <- grepl("healthy", urls) | grepl("f3", urls)
            data.table::data.table(
                url = urls,
                reachable = good,
                latency_ms = 1,
                error = ifelse(good, NA_character_, "unavailable"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )
    out <- query_result__sample_service_nodes(
        query_result_test_object("File", host_docs),
        sample_per_node = 2L
    )
    expect_identical(out$reachable, c(TRUE, TRUE, FALSE, FALSE))
    expect_error(
        query_result__sample_service_nodes(
            query_result_test_object("File", host_docs),
            sample_per_node = 0L
        ),
        "Must be >= 1"
    )
})

test_that("URL checks bound active waves and preserve completed results", {
    mode <- "success"
    pools <- list()
    deadlines <- numeric()
    serial_urls <- character()
    cancelled <- 0L
    testthat::local_mocked_bindings(
        multi_cancel = function(handle) cancelled <<- cancelled + 1L,
        new_pool = function(total_con, host_con) {
            pool <- new.env(parent = emptyenv())
            pool$callbacks <- list()
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
    mode <- "failed"
    expect_equal(sum(vapply(run(8L), `[[`, logical(1L), "reachable")), 5L)
    mode <- "timeout"
    expect_false(any(vapply(run(20L), `[[`, logical(1L), "reachable")))
    expect_true(all(vapply(run(2L), `[[`, logical(1L), "reachable")))
    expect_length(serial_urls, 2L)
    mode <- "error"
    expect_equal(sum(vapply(run(8L), `[[`, logical(1L), "reachable")), 8L)
    expect_identical(serial_urls, paste0("https://example.org/", 4:6))
    expect_identical(run(0L), stats::setNames(list(), character()))
})

test_that("EsgResultFile$download_plan() builds logical IDs, deduplicates probes, reuses cache, and ranks nodes", {
    files <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        ))
    )
    plan <- files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "first"
    )
    expect_s3_class(plan, "data.table")
    expect_identical(plan$logical_file_id, "master:master-file-1")
    expect_identical(plan$checksum_type, "sha256")
    expect_error(
        files$download(run = FALSE),
        "explicit `store` or persistent `downloader`"
    )

    chk_docs <- query_result_test_file_docs(
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    )
    chk_docs$master_id <- chk_docs$tracking_id <- NA_character_
    expect_identical(
        query_result_test_object("File", chk_docs)$download_plan(
            replica = "current",
            probe = FALSE
        )$logical_file_id,
        "checksum:abc:1:file.nc"
    )

    # Deduplicates probes and reuses fresh node_stats cache
    dup_docs <- query_result_test_file_docs(
        "https://same.example.org/file.nc|application/netcdf|HTTPServer"
    )[c(1L, 1L), ]
    dup_docs$id <- c("file-1", "file-2")
    dup_docs$data_node <- "same.example.org"
    dup_docs$master_id <- c("m1", "m2")
    calls <- 0L
    testthat::local_mocked_bindings(
        query_result__latency_url = function(...) {
            calls <<- calls + 1L
            list(latency = 0.5, throughput = NA_real_)
        },
        .package = "epwshiftr"
    )
    probed <- query_result_test_object("File", dup_docs)$download_plan(
        replica = "current",
        probe = TRUE,
        strategy = "first",
        probe_concurrency = 1L,
        probe_cache_seconds = 0L
    )
    expect_equal(calls, 1L)
    expect_equal(probed$probe_latency, c(0.5, 0.5))

    cached_stats <- data.table::data.table(
        data_node = "same.example.org",
        service = "HTTPServer",
        success_count = 1L,
        failure_count = 0L,
        avg_latency = 0.25,
        probe_success_count = 1L,
        probe_failure_count = 0L,
        last_probe_at = Sys.time(),
        updated_at = Sys.time()
    )
    cached_plan <- query_result_test_object(
        "File",
        dup_docs[1L, ]
    )$download_plan(
        replica = "current",
        probe = TRUE,
        node_stats = cached_stats,
        probe_cache_seconds = 3600L
    )
    expect_equal(calls, 1L)
    expect_true(cached_plan$probe_cached)
    expect_equal(cached_plan$probe_latency, 0.25)

    # Ranks by node history and puts cooling nodes behind available candidates
    rep_docs <- query_result_test_file_docs()[c(1L, 1L), ]
    rep_docs$id <- c("file-a", "file-b")
    rep_docs$data_node <- c("slow.example.org", "fast.example.org")
    rep_docs$url <- I(list(
        "https://slow.example.org/file.nc|application/netcdf|HTTPServer",
        "https://fast.example.org/file.nc|application/netcdf|HTTPServer"
    ))
    rep_files <- query_result_test_object("File", rep_docs)
    hist_stats <- data.table::data.table(
        data_node = c("slow.example.org", "fast.example.org"),
        service = "HTTPServer",
        success_count = c(1L, 10L),
        failure_count = c(9L, 0L),
        avg_latency = c(10, 1)
    )
    ranked <- rep_files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = hist_stats
    )
    expect_identical(
        ranked$data_node,
        c("fast.example.org", "slow.example.org")
    )

    cool_stats <- data.table::copy(hist_stats)
    cool_stats[, `:=`(
        probe_success_count = 0L,
        probe_failure_count = c(0L, 3L),
        cooldown_until = c(as.POSIXct(NA), Sys.time() + 3600),
        updated_at = Sys.time()
    )]
    cooled <- rep_files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = cool_stats
    )
    expect_identical(
        cooled$data_node,
        c("slow.example.org", "fast.example.org")
    )
    expect_equal(cooled$node_cooldown_rank, c(0L, 1L))
    cool_stats$cooldown_until <- Sys.time() + 3600
    expect_true(all(
        rep_files$download_plan(
            replica = "current",
            probe = FALSE,
            strategy = "fastest",
            node_stats = cool_stats
        )$node_cooldown_rank ==
            0L
    ))
})
# }}}

# EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() {{{
test_that("open_dataset() validates fallbacks, falls back to HTTP, repairs URLs, and reports progress", {
    testthat::local_mocked_bindings(menu = function(...) 2L, .package = "utils")

    http_only <- query_result_test_object(
        "File",
        query_result_test_file_docs(
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        )
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
    expect_error(
        query_result_test_object(
            "File",
            query_result_test_file_docs(character())
        )$open_dataset(fallback = "auto"),
        "HTTPServer download URL"
    )
    expect_error(
        query_result_test_object(
            "Aggregation",
            query_result_test_file_docs(character()),
            query_result_test_params("Aggregation")
        )$open_dataset(fallback = "auto"),
        "record 1 \\(id: file-1\\)"
    )

    calls <- new.env(parent = emptyenv())
    calls$opened <- list()
    calls$closed <- list()
    calls$downloads <- character()
    calls$open_progress <- logical()
    FakeEsgDataset <- qr_test__fake_dataset_class(calls)
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
                        p <- tempfile(fileext = ".nc")
                        writeLines("netcdf", p)
                        p
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
        query_result__repair_urls = function(result, ...) result,
        .package = "epwshiftr"
    )

    file_res <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            "https://example.org/fail-opendap.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        ))
    )
    err <- tryCatch(file_res$open_dataset(fallback = "error"), error = identity)
    expect_match(conditionMessage(err), "OPeNDAP is not available")
    expect_match(conditionMessage(err$parent), "remote boom")
    ds <- file_res$open_dataset(
        fallback = "auto",
        downloader = FakeDownloader$new()
    )
    expect_true(file.exists(ds$target))
    expect_identical(tail(calls$downloads, 1L), "https://example.org/file.nc")
    expect_error(file_res$open_dataset(index = 1L), "unused argument")

    # Multi-file selection and Aggregation partial-failure cleanup + fallback
    agg_docs <- query_result_test_file_docs()[c(1L, 1L), ]
    agg_docs$id <- c("file-1", "file-2")
    agg_docs$url <- I(list(
        c(
            "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file-1.nc|application/netcdf|HTTPServer"
        ),
        c(
            "https://example.org/fail-opendap-file-2.nc.html|application/netcdf|OPENDAP",
            "https://example.org/file-2.nc|application/netcdf|HTTPServer"
        )
    ))
    agg_res <- query_result_test_object(
        "Aggregation",
        agg_docs,
        query_result_test_params("Aggregation")
    )
    expect_identical(
        agg_res$open_dataset(which = "file-1", fallback = "auto")$target,
        "https://example.org/dods/file-1.nc"
    )
    expect_error(
        agg_res$open_dataset(fallback = "error"),
        "record 2 \\(id: file-2\\)"
    )
    expect_true(any(vapply(
        calls$closed,
        identical,
        logical(1L),
        list("handle:https://example.org/dods/file-1.nc")
    )))
    expect_message(
        agg_ds <- agg_res$open_dataset(
            fallback = "auto",
            downloader = FakeDownloader$new()
        ),
        "record 2 \\(id: file-2\\)"
    )
    expect_identical(agg_ds$target[[1L]], "https://example.org/dods/file-1.nc")
    expect_true(file.exists(agg_ds$target[[2L]]))

    # Missing HTTP URL recovers via replica repair
    repaired_docs <- query_result_test_file_docs(
        "https://replica.example.org/file.nc|application/netcdf|HTTPServer"
    )
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(result, ...) {
            query_result_test_object("File", repaired_docs)
        },
        .package = "epwshiftr"
    )
    rep_ds <- query_result_test_object(
        "File",
        query_result_test_file_docs(character())
    )$open_dataset(
        fallback = "auto",
        downloader = FakeDownloader$new()
    )
    expect_true(file.exists(rep_ds$target))
    expect_identical(
        tail(calls$downloads, 1L),
        "https://replica.example.org/file.nc"
    )

    # Progress reporting for File and Aggregation open_dataset()
    bars <- list()
    testthat::local_mocked_bindings(
        cli_progress_bar = function(name = NULL, total = NA, ...) {
            bars[[length(bars) + 1L]] <<- list(name = name, total = total)
            paste0("p-", length(bars))
        },
        cli_progress_update = function(...) NULL,
        cli_progress_done = function(...) NULL,
        .package = "cli"
    )
    ok_docs <- agg_docs
    ok_docs$url <- I(list(
        "https://example.org/dods/file-1.nc.html|application/netcdf|OPENDAP",
        "https://example.org/dods/file-2.nc.html|application/netcdf|OPENDAP"
    ))
    query_result_test_object("File", ok_docs)$open_dataset(
        fallback = "auto",
        progress = TRUE
    )
    query_result_test_object("File", ok_docs)$open_dataset(
        fallback = "auto",
        progress = FALSE
    )
    query_result_test_object(
        "Aggregation",
        ok_docs,
        query_result_test_params("Aggregation")
    )$open_dataset(fallback = "auto", progress = TRUE)
    expect_equal(
        bars,
        list(
            list(name = "Opening File records", total = 2L),
            list(name = "Opening Aggregation records", total = 2L)
        )
    )
})
# }}}

# Offline contract fixtures & snapshots {{{
test_that("Dataset, File, and Aggregation offline fixtures expose fields, to_data_table(), and batched print_contents()", {
    ds_params <- query_result_test_params(
        "Dataset",
        activity_id = "ScenarioMIP",
        source_id = "AWI-CM-1-1-MR",
        frequency = "day",
        variable_id = "tas",
        variant_label = "r1i1p1f1",
        fields = c("source_id", "experiment_id", "frequency"),
        limit = 2L
    )
    ds_docs <- query_result_test_contract_dataset_docs()
    datasets <- query_result_test_object("Dataset", ds_docs, ds_params)
    expect_equal(datasets$count(), 2L)
    expect_equal(
        datasets$to_data_table(formatted = TRUE)$size,
        c("1.00 GiB", "2.00 GiB")
    )
    expect_s3_class(datasets$size, "epwshiftr_bytes")
    expect_null(datasets$url)
    expect_type(datasets$has_opendap(), "logical")
    expect_type(datasets$has_download(), "logical")

    # Batched print_contents()
    ds_docs$number_of_aggregations <- c(2L, 0L)
    ds_print <- query_result_test_object(
        "Dataset",
        ds_docs,
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
    priv(ds_print)$print_contents("Dataset", 2L)
    expect_length(cat_lines, 1L)
    expect_length(cat_lines[[1L]][[1L]], 4L)

    file_params <- query_result_test_params(
        "File",
        fields = c("source_id", "experiment_id", "frequency"),
        dataset_id = ds_docs$id[[1L]],
        limit = 1L
    )
    files <- query_result_test_object(
        "File",
        query_result_test_contract_file_docs()[1L, , drop = FALSE],
        file_params
    )
    expect_equal(files$to_data_table(formatted = TRUE)$size, "1.00 MiB")
    expect_s3_class(files$url[[1L]], "data.table")
    expect_true(all(
        EsgResultFile$private_fields$required_fields %in% files$fields
    ))

    agg_params <- query_result_test_params(
        "Aggregation",
        fields = "id",
        dataset_id = ds_docs$id,
        limit = 2L
    )
    aggs <- query_result_test_object(
        "Aggregation",
        query_result_test_contract_aggregation_docs(),
        agg_params
    )
    expect_equal(names(aggs$to_data_table(c("url", "size"))), c("url", "size"))
    expect_s3_class(aggs$url[[1L]], "data.table")
    expect_length(aggs$url_opendap, 2L)
})

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
    priv(de)$response$cache <- priv(datasets)$response$cache
    priv(de)$response$response$docs <- priv(de)$response$response$docs[,
        names(priv(datasets)$response$response$docs)
    ]
    expect_equal(priv(de)$response, priv(datasets)$response, ignore_attr = TRUE)
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
# }}}

# Query value and replica identity batching {{{
test_that("query value batching measures rendered URLs and shards long replica identities", {
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

    ids <- paste0(strrep("long.model.", 50L), seq_len(6L))
    result <- query_result_test_object(
        "File",
        query_result_test_file_docs(),
        query_result_test_params("File")
    )
    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, ...) {
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
        expect_identical(out$id, ids)
        expect_length(priv(out)$context$query_url, length(calls))
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
# }}}

# Candidate identity compares checksums only when their algorithms agree,
# preserves interleaved ordering, and rejects contradictions across sparse rows.
test_that("raw candidate plans preserve digest algorithms, interleaved ordering, and reject contradictions", {
    docs <- query_result_test_file_docs()[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$checksum_type <- c("SHA256", "MD5")
    docs$checksum <- c("sha-value", "md5-value")
    docs$url <- I(list(
        "https://one.example/a.nc|application/netcdf|OPENDAP",
        "https://two.example/a.nc|application/netcdf|OPENDAP"
    ))
    plan <- query_result_test_object("File", docs)$access_plan(expand = FALSE)
    expect_equal(nrow(plan), 2L)
    expect_length(unique(plan$source_key), 1L)
    docs$checksum_type <- c("SHA256", "SHA256")
    expect_error(
        query_result_test_object("File", docs)$access_plan(expand = FALSE),
        "Conflicting"
    )

    interleaved <- query_result_test_file_docs()[rep(1L, 7L), ]
    interleaved$id <- paste0("row-", 1:7)
    interleaved$instance_id <- interleaved$master_id <- c(
        "a",
        "b",
        "a",
        "c",
        "a",
        "b",
        "a"
    )
    interleaved$version <- c(1L, 1L, 2L, 1L, 1L, NA_integer_, NA_integer_)
    interleaved$checksum_type <- c(
        "SHA256",
        "SHA256",
        "SHA256",
        "SHA256",
        "MD5",
        NA,
        NA
    )
    interleaved$checksum <- c(
        "a-sha",
        "b-sha",
        "a2-sha",
        "c-sha",
        "a-md5",
        NA,
        NA
    )
    interleaved$size <- c(1, 1, 2, 1, 1, NA_real_, NA_real_)
    interleaved$url <- I(lapply(1:7, function(i) {
        paste0("https://node", i, ".example/file.nc|application/netcdf|OPENDAP")
    }))
    iplan <- query_result_test_object("File", interleaved)$access_plan(
        expand = FALSE
    )
    expect_identical(iplan$content_group, c(1L, 2L, 3L, 4L, 1L, 2L, 1L))
    expect_identical(
        iplan$source_key[c(1L, 2L, 3L, 4L, 1L, 2L, 1L)],
        iplan$source_key
    )
    expect_equal(
        nrow(query_result_test_object("File", interleaved[FALSE, ])$access_plan(
            expand = FALSE
        )),
        0L
    )

    sparse <- query_result_test_file_docs()[rep(1L, 4L), ]
    sparse$instance_id <- sparse$master_id <- c("a", "b", "a", "a")
    sparse$checksum <- c("first", "other", NA_character_, "conflicting")
    sparse$url <- I(lapply(1:4, function(i) {
        paste0("https://node", i, ".example/file.nc|application/netcdf|OPENDAP")
    }))
    expect_error(
        query_result_test_object("File", sparse)$access_plan(expand = FALSE),
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

# Fresh invocations ignore older cache while retaining committed replica progress
# and validating native interval bounds before accepting replacement endpoints.
test_that("raw subset fresh invocation retains progress and rejects incompatible native time bounds", {
    server <- qr_test__dap_server()
    requests <- qr_test__dap_request(
        c("2060-01-01", "2060-01-04"),
        request_id = c("one", "four")
    )

    # resume=FALSE retains progress from failed endpoint, then refreshes on a second fresh call
    dest1 <- withr::local_tempdir()
    files1 <- qr_test__dap_files(
        server,
        c("bad-bounds.nc", "fast-bounds.nc"),
        nodes = c("first", "second")
    )
    expect_true(
        files1$fetch_subset(
            requests,
            dest1,
            workers = 1L,
            retries = 0L,
            discover = FALSE,
            resume = FALSE
        )$complete
    )
    attempts <- subset_status(dest1, "attempts")
    expect_equal(sum(attempts$native_values), 8)
    expect_identical(
        attempts$data_node[attempts$native_values > 0],
        c("first", rep("second", 4L))
    )
    raw <- read_subset(dest1)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_equal(sort(unique(raw$values$bound_start_value)), c(0, 3))
    expect_true(
        files1$fetch_subset(
            requests,
            dest1,
            workers = 1L,
            retries = 0L,
            discover = FALSE,
            resume = FALSE
        )$complete
    )
    expect_equal(sum(subset_status(dest1, "attempts")$native_values), 16)

    # Incompatible bounds replica is rejected without reading weather or full time_bnds
    dest2 <- withr::local_tempdir()
    files2 <- qr_test__dap_files(
        server,
        c("bad-bounds.nc", "wrong-bounds.nc", "fast-bounds.nc"),
        nodes = c("partial", "wrong-bounds", "consistent")
    )
    expect_true(
        files2$fetch_subset(
            requests,
            dest2,
            workers = 1L,
            retries = 0L,
            discover = FALSE,
            resume = FALSE
        )$complete
    )
    att2 <- subset_status(dest2, "attempts")
    expect_true(any(
        att2$data_node == "wrong-bounds" & att2$error_kind == "source_metadata"
    ))
    expect_equal(sum(att2$native_values), 8)
    traffic <- qr_test__dap_traffic(server)
    expect_false(any(grepl("wrong-bounds.nc.dods tas", traffic, fixed = TRUE)))
    bounds_reads <- traffic[grepl("[.]dods time_bnds", traffic)]
    expect_true(length(bounds_reads) > 0L)
    expect_false(any(grepl("time_bnds%5b0:7%5d", bounds_reads, fixed = TRUE)))

    # Without a consistent fallback, resume=TRUE leaves explicit partial result
    dest3 <- withr::local_tempdir()
    files3 <- qr_test__dap_files(
        server,
        c("bad-bounds.nc", "wrong-bounds.nc"),
        nodes = c("partial", "wrong-bounds")
    )
    expect_false(
        files3$fetch_subset(
            requests,
            dest3,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_equal(sort(read_subset(dest3)$values$value), 1:4)
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
                if (!is.null(n) && n > 0L && !is.null(.compute)) {
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
        "View|view|base table"
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
        "View|view|base table"
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

# Slow native DAP/IO errors need independent File corroboration, cap exponential
# backoff, ignore File-specific 404/410 absences, and reset on real native progress.
test_that("acquisition node cooldown corroborates slow DAP/IO failures, caps backoff, and resets", {
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
        elapsed_seconds = 90,
        native_values = 0,
        success = FALSE,
        error = "NetCDF: DAP failure",
        error_kind = "transport"
    )
    cool <- function(rows, now = 1181, seconds = 60) {
        opendap__cooling_until(candidates, rows, "client", seconds, now)
    }
    expect_equal(cool(history), c(1240, 0))
    expect_equal(cool(history[1:2, ]), c(0, 0))
    expect_equal(cool(transform(history, source_key = "one")), c(0, 0))
    for (err in c(
        "NetCDF: DAP failure HTTP 404",
        "NetCDF: file not found",
        "NetCDF: DAP failure; HTTP/1.1 404",
        "NetCDF: I/O failure; HTTP/2 410"
    )) {
        expect_equal(cool(transform(history, error = err)), c(0, 0))
    }
    for (err in c(
        "execution deadline",
        "NetCDF: HTTP/1.1 503",
        "Failed to open OPeNDAP connection: NetCDF: I/O failure"
    )) {
        expect_equal(cool(transform(history, error = err)), c(1240, 0))
    }
    expect_equal(cool(history, seconds = 0), c(0, 0))
    expect_true(all(cool(history, now = 2000) <= 2000))
    expect_equal(cool(transform(history, client = "other")), c(0, 0))
    expect_equal(
        cool(transform(history, error_kind = "source_metadata")),
        c(0, 0)
    )

    # Real native progress resets cooldown; metadata-only success does not
    success <- transform(
        history[1L, ],
        observed_at = 1180.5,
        success = TRUE,
        native_values = 4,
        error = NA_character_
    )
    expect_equal(cool(rbind(history, success)), c(0, 0))
    expect_equal(
        cool(rbind(history, transform(success, native_values = 0))),
        c(1240, 0)
    )

    # Failure tail spans slow operations, rejects invalid/unbounded durations, and caps backoff
    tail_hist <- transform(
        history,
        observed_at = c(1000, 1275, 1550),
        elapsed_seconds = 275
    )
    expect_equal(
        opendap__cooling_until(candidates[1L], tail_hist, "client", 60, 1551),
        1610
    )
    for (dur in c(NA_real_, Inf, -1)) {
        expect_equal(
            opendap__cooling_until(
                candidates[1L],
                transform(tail_hist, elapsed_seconds = dur),
                "client",
                60,
                1551
            ),
            0
        )
    }
    long_hist <- data.frame(
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
    deadlines <- vapply(
        3:9,
        function(n) {
            opendap__cooling_until(
                candidates[1L],
                long_hist[seq_len(n), ],
                "client",
                60,
                1010
            )
        },
        numeric(1L)
    )
    expect_equal(deadlines, 1003:1009 + c(60, 120, 240, 480, 480, 480, 480))
    endpoint <- data.table::as.data.table(transform(
        long_hist,
        source_key = "one",
        url = "one"
    ))
    expect_equal(opendap__cooldown_deadline(endpoint, 60), 1489)
    expect_equal(
        opendap__cooldown_deadline(
            rbind(
                endpoint,
                transform(
                    endpoint[1L],
                    observed_at = 1010,
                    native_values = 4,
                    success = TRUE
                )
            ),
            60
        ),
        0
    )
    expect_equal(
        opendap__cooldown_deadline(
            transform(endpoint[1L], error = "NetCDF: DAP failure; HTTP/2 410"),
            60
        ),
        1481
    )
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


# A published File can use both Julian coordinates and point bounds. The public
# acquisition must retain native values and resume its exact blocks without I/O.
test_that("raw subset acquisition resumes Julian instantaneous bounds exactly", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app(
        calendar = "julian",
        point_bounds = TRUE
    ))
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-julian-point-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    requests <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), as.double(1:32))
    expect_equal(nrow(raw$neighborhoods), 4L)
    expect_identical(unique(raw$values$cf_calendar), "julian")
    expect_equal(raw$values$bound_start_value, raw$values$bound_end_value)
    artifacts <- list.files(
        file.path(dest, "subsets"),
        "[.]parquet$",
        full.names = TRUE
    )
    hashes <- tools::sha256sum(artifacts)
    attempts <- subset_status(dest, "attempts")
    result <- files$fetch_subset(
        requests,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    expect_identical(tools::sha256sum(artifacts), hashes)
    expect_equal(nrow(subset_status(dest, "attempts")), nrow(attempts))
})

# A corrected decoder must reuse native payloads, recheck earlier selection
# receipts, and never label a legacy decoded window complete without that check.
test_that("CF normalization resumes raw bounds after legacy selection rechecks", {
    skip_if_not_installed("webfakes")
    units <- "hours since 2060-01-01 00:00:00 +02:00"
    server <- webfakes::new_app_process(subset_test__dap_app(
        calendar = "standard",
        point_bounds = TRUE,
        time_units = units
    ))
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("raw-cf-normalization-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01T05:00:00"
    )
    result <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), as.double(9:32))
    expect_equal(sort(unique(raw$values$time_value)), as.double(2:7))
    expect_identical(unique(raw$values$time_units), units)
    expect_equal(raw$values$bound_start_value, raw$values$time_value)
    expect_equal(raw$values$bound_end_value, raw$values$time_value)
    expect_equal(raw$values$time_bound_start, raw$values$time)
    expect_equal(nrow(raw$neighborhoods), 4L)
    # Model old derived columns in memory; the raw payload and bounds remain
    # authoritative and the refresh must recover the identical corrected view.
    stale <- data.table::copy(raw$values)
    data.table::set(stale, j = "time", value = stale$time + 7200)
    data.table::set(
        stale,
        j = "cf_second_of_day",
        value = stale$cf_second_of_day + 7200
    )
    data.table::set(
        stale,
        j = "time_bound_start",
        value = stale$time_bound_start + 7200
    )
    expect_equal(opendap__refresh_clock(stale), raw$values)
    blocks <- subset_status(dest, "blocks")
    artifacts <- c(blocks$path, blocks$metadata_path)
    conn <- ddb_connect(file.path(dest, "manifest.duckdb"))
    demands <- data.table::as.data.table(ddb_read_table(conn, "subset_demands"))
    descriptor <- readRDS(demands$receipt_path[[1L]])
    legacy_id <- store__hash(
        "raw-demand-receipt-v1",
        descriptor$source_key[[1L]],
        descriptor$lon[[1L]],
        descriptor$lat[[1L]],
        descriptor$time_start[[1L]],
        descriptor$time_stop[[1L]]
    )
    descriptor$demand_id <- legacy_id
    legacy_path <- file.path(dest, "subsets", "legacy-selection.rds")
    store__extract_cache_write(legacy_path, descriptor)
    data.table::set(demands, j = "demand_id", value = legacy_id)
    data.table::set(demands, j = "receipt_path", value = legacy_path)
    data.table::set(
        demands,
        j = "receipt_sha256",
        value = checksum_file(legacy_path)
    )
    ddb_write_table(
        conn,
        "subset_demands",
        as.data.frame(demands),
        overwrite = TRUE
    )
    # Publish a synthetic pre-fix clock and selection in this test directory.
    # The native raw columns, contract and weather values are unchanged.
    for (i in seq_len(nrow(blocks))) {
        metadata <- readRDS(blocks$metadata_path[[i]])
        if (!is.null(metadata$demands)) {
            metadata$demands$demand_id <- legacy_id
        }
        path <- file.path(
            dest,
            "subsets",
            paste0("legacy-metadata-", i, ".rds")
        )
        store__extract_cache_write(path, metadata)
        data.table::set(blocks, i = i, j = "metadata_path", value = path)
        data.table::set(
            blocks,
            i = i,
            j = "metadata_sha256",
            value = checksum_file(path)
        )
        path <- file.path(
            dest,
            "subsets",
            paste0("legacy-block-", i, ".parquet")
        )
        ddb_exec(
            conn,
            sprintf(
                paste(
                    "COPY (SELECT * REPLACE (time + INTERVAL '2 hours' AS time,",
                    "cf_second_of_day + 7200 AS cf_second_of_day,",
                    "time_bound_start + INTERVAL '2 hours' AS time_bound_start,",
                    "time_bound_end + INTERVAL '2 hours' AS time_bound_end)",
                    "FROM read_parquet(%s)) TO %s (FORMAT PARQUET)"
                ),
                ddb_literal(conn, blocks$path[[i]]),
                ddb_literal(conn, path)
            )
        )
        data.table::set(blocks, i = i, j = "path", value = path)
        data.table::set(
            blocks,
            i = i,
            j = "sha256",
            value = checksum_file(path)
        )
    }
    ddb_write_table(
        conn,
        "subset_blocks",
        as.data.frame(blocks),
        overwrite = TRUE
    )
    ddb_disconnect(conn)
    artifacts <- c(blocks$path, blocks$metadata_path)
    hashes <- tools::sha256sum(artifacts)
    expect_equal(read_subset(dest)$values, raw$values)
    expect_equal(nrow(read_subset(dest)$gaps$incomplete_requests), 1L)
    expect_false(all(subset_status(dest, "coverage")$complete))
    curl::curl_fetch_memory(paste0(server$url(), "reset"))
    result <- files$fetch_subset(
        request,
        dest,
        workers = 1L,
        retries = 0L,
        discover = FALSE
    )
    expect_true(result$complete)
    expect_identical(tools::sha256sum(artifacts), hashes)
    expect_equal(read_subset(dest)$values, raw$values)
    expect_equal(nrow(read_subset(dest)$gaps$incomplete_requests), 0L)
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
    expect_false(any(grepl("fast-bounds.nc.dods tas", traffic, fixed = TRUE)))
    expect_true(any(grepl("fast-bounds.nc.dods time", traffic, fixed = TRUE)))
    attempts <- subset_status(dest, "attempts")
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
    expect_identical(tools::sha256sum(artifacts), hashes)
})

# Persisted climatological values remain explicitly statistical through real
# DAP transport, immutable publication, read-back, and exact native resume.
test_that("CF climatology acquisition preserves statistics and exact cached bounds", {
    # Each source order owns a server/destination and independent cleanup scope.
    check_order <- function(reversed) {
        skip_if_not_installed("webfakes")
        method <- "time: minimum within years time: mean over years"
        server <- webfakes::new_app_process(subset_test__dap_app(
            climatology_method = method,
            bounds_reversed = reversed
        ))
        on.exit(server$stop(), add = TRUE)
        dest <- tempfile("raw-climatology-")
        on.exit(unlink(dest, recursive = TRUE), add = TRUE)
        docs <- query_result_test_file_docs(paste0(
            server$url(),
            "fast-bounds.nc|application/netcdf|OPENDAP"
        ))
        docs$variable_id <- "tas"
        files <- query_result_test_object("File", docs)
        request <- data.frame(
            request_id = "one",
            site_id = "city",
            lon = 10.25,
            lat = .25,
            time_start = "2060-01-01",
            time_stop = "2060-01-08"
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
        raw <- read_subset(dest)
        expect_equal(sort(raw$values$value), as.double(1:32))
        expect_identical(unique(raw$values$time_kind), "climatology")
        expect_identical(unique(raw$values$climatology_cell_methods), method)
        expect_identical(
            jsonlite::fromJSON(raw$values$climatology_methods[[1L]])$method,
            c("minimum", "mean")
        )
        expect_equal(
            raw$values$climatology_bound_start_value,
            raw$values$time_value
        )
        expect_equal(
            raw$values$climatology_bound_end_value - raw$values$time_value,
            rep(10801, 32L)
        )
        expect_false("time_bound_start" %in% names(raw$values))
        expect_equal(nrow(raw$neighborhoods), 4L)
        blocks <- subset_status(dest, "blocks")
        artifacts <- unique(c(
            blocks$path,
            blocks$metadata_path,
            list.files(
                file.path(dest, "subsets"),
                pattern = "[.](rds|parquet|json)$",
                recursive = TRUE,
                full.names = TRUE
            )
        ))
        hashes <- tools::sha256sum(artifacts)
        saved <- readRDS(blocks$metadata_path[[1L]])$bounds
        expect_identical(unique(saved$time_kind), "climatology")
        expect_identical(unique(saved$cell_methods), method)
        expect_identical(unique(saved$bounds_name), "time_bnds")
        dimensions <- if (reversed) c("time", "bnds") else c("bnds", "time")
        position <- match("time", dimensions)
        expect_identical(unique(saved$bounds_time_dimension), position)
        expect_identical(
            jsonlite::fromJSON(saved$bounds_dimensions[[1L]]),
            dimensions
        )
        expect_identical(
            unique(raw$values$climatology_bounds_time_dimension),
            position
        )
        expect_identical(
            jsonlite::fromJSON(raw$values$climatology_bounds_dimensions[[1L]]),
            dimensions
        )
        reconstructed <- cbind(saved$bound_start_value, saved$bound_end_value)
        expected_bounds <- cbind(as.double(0:7), as.double(0:7 + 10801))
        if (position == 2L) {
            reconstructed <- t(reconstructed)
            expected_bounds <- t(expected_bounds)
        }
        expect_identical(reconstructed, expected_bounds)
        missing_order <- data.table::copy(raw$values)
        data.table::set(
            missing_order,
            j = c(
                "climatology_bounds_time_dimension",
                "climatology_bounds_dimensions"
            ),
            value = NULL
        )
        expect_error(
            opendap__refresh_clock(missing_order),
            "Incomplete cached CF climatology"
        )
        wrong_order <- data.table::copy(raw$values)
        data.table::set(
            wrong_order,
            j = "climatology_bounds_time_dimension",
            value = 3L
        )
        expect_error(opendap__refresh_clock(wrong_order), "dimension")
        # Stale derived fields are rebuilt without changing raw weather or bounds.
        stale <- data.table::copy(raw$values)
        data.table::set(stale, j = "climatology_start_year", value = 1L)
        expect_equal(opendap__refresh_clock(stale), raw$values)
        unmarked <- data.table::copy(raw$values)
        data.table::set(unmarked, j = "time_kind", value = NULL)
        expect_error(opendap__refresh_clock(unmarked), "climatolog")
        curl::curl_fetch_memory(paste0(server$url(), "reset"))
        attempts <- subset_status(dest, "attempts")
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
        expect_identical(tools::sha256sum(artifacts), hashes)
        expect_equal(read_subset(dest)$values, raw$values)
        traffic <- jsonlite::fromJSON(rawToChar(
            curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
        ))
        expect_false(any(grepl(
            "fast-bounds.nc.dods tas",
            traffic,
            fixed = TRUE
        )))
    }
    check_order(FALSE)
    check_order(TRUE)
})

# Ordinary nested daily statistics must survive real native transport and
# checksum-verified resume without being reconstructed as cyclic climatologies.
test_that("CMIP6 monthly statistics resume ordinary native blocks unchanged", {
    skip_if_not_installed("webfakes")
    method <- "area: mean time: maximum within days time: mean over days"
    server <- webfakes::new_app_process(subset_test__dap_app(
        ordinary_method = method,
        time_values = 30 * 0:7
    ))
    on.exit(server$stop(), add = TRUE)
    dest <- tempfile("ordinary-monthly-")
    on.exit(unlink(dest, recursive = TRUE), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "fast-bounds.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-08-30"
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
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), as.double(1:32))
    expect_identical(unique(raw$values$cell_methods), method)
    expect_equal(sort(unique(raw$values$time_value)), as.double(30 * 0:7))
    expect_equal(
        raw$values$bound_end_value - raw$values$bound_start_value,
        rep(30, 32L)
    )
    expect_false(any(startsWith(names(raw$values), "climatology_")))
    for (endpoint in c("bound_start_value", "bound_end_value")) {
        invalid <- data.table::copy(raw$values)
        data.table::set(invalid, j = endpoint, value = NULL)
        expect_error(opendap__refresh_clock(invalid), "explicit time bounds")
        invalid <- data.table::copy(raw$values)
        data.table::set(invalid, i = 1L, j = endpoint, value = NA_real_)
        expect_error(opendap__refresh_clock(invalid), "explicit time bounds")
    }
    blocks <- subset_status(dest, "blocks")
    artifacts <- c(blocks$path, blocks$metadata_path)
    hashes <- tools::sha256sum(artifacts)
    attempts <- subset_status(dest, "attempts")
    curl::curl_fetch_memory(paste0(server$url(), "reset"))
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE
        )$complete
    )
    expect_identical(tools::sha256sum(artifacts), hashes)
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_equal(read_subset(dest)$values, raw$values)
    traffic <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "requests"))$content
    ))
    expect_false(any(grepl("fast-bounds.nc.dods tas", traffic, fixed = TRUE)))
})

# Check idempotent receipt registration without weakening corruption or rollback guards.
test_that("serial overhead fixes register only changed demand receipts", {
    dest <- tempfile("serial-demands-")
    store <- opendap__open(dest)
    on.exit(store$close(), add = TRUE)
    conn <- priv(store)$conn
    d <- data.table::data.table(
        source_key = "serial-source",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08",
        native_ranges = "[[1,8]]"
    )
    data.table::set(d, j = "demand_id", value = opendap__demand_ids(d))
    appended <- 0L
    append <- ddb_append_table
    local_mocked_bindings(
        .package = "epwshiftr",
        ddb_append_table = function(conn, table, rows, ...) {
            if (table == "subset_demands") {
                appended <<- appended + nrow(rows)
            }
            append(conn, table, rows, ...)
        }
    )
    opendap__save_demands(conn, d, dest)
    first <- ddb_read_table(conn, "subset_demands")
    sha <- tools::sha256sum(first$receipt_path)
    opendap__save_demands(conn, d, dest)
    expect_identical(appended, 1L)
    expect_identical(ddb_read_table(conn, "subset_demands"), first)
    expect_identical(tools::sha256sum(first$receipt_path), sha)
    changed <- data.table::copy(d)
    data.table::set(changed, j = "native_ranges", value = "[[1,4]]")
    opendap__save_demands(conn, changed, dest)
    expect_identical(appended, 2L)
    expect_true(file.exists(first$receipt_path))
    latest <- ddb_read_table(conn, "subset_demands")
    expect_false(identical(latest$receipt_path, first$receipt_path))
    # A damaged byte-addressed receipt is re-created only from its exact payload.
    writeBin(as.raw(1L), latest$receipt_path)
    opendap__save_demands(conn, changed, dest)
    expect_identical(
        unname(tools::sha256sum(latest$receipt_path)),
        latest$receipt_sha256
    )
    expect_identical(appended, 2L)
    opendap__save_demands(conn, d[0L], dest)
    expect_identical(ddb_read_table(conn, "subset_demands"), latest)

    # Roll back a failed receipt replacement without corrupting existing receipts
    data.table::set(changed, j = "native_ranges", value = "[[1,2]]")
    with_mocked_bindings(
        expect_error(
            opendap__save_demands(conn, changed, dest),
            "append failure"
        ),
        ddb_append_table = function(conn, table, rows, ...) {
            if (table == "subset_demands") {
                stop("append failure")
            }
            append(conn, table, rows, ...)
        },
        .package = "epwshiftr"
    )
    expect_identical(ddb_read_table(conn, "subset_demands"), latest)
    expect_identical(
        unname(tools::sha256sum(latest$receipt_path)),
        latest$receipt_sha256
    )
})

# The real public call must keep other associations and avoid whole-table rewrites.
test_that("serial overhead fixes update only current associations", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "source.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    dest <- tempfile("serial-links-")
    request <- data.frame(
        request_id = "one",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    values <- read_subset(dest)$values
    attempts <- subset_status(dest, "attempts")
    original <- ddb_write_table
    local_mocked_bindings(
        .package = "epwshiftr",
        ddb_write_table = function(conn, table, rows, ...) {
            if (
                table %in%
                    c("subset_links", "subset_candidates", "subset_content")
            ) {
                stop("Unscoped ledger rewrite")
            }
            original(conn, table, rows, ...)
        }
    )
    other <- request
    other$request_id <- "two"
    expect_true(
        files$fetch_subset(other, dest, workers = 1L, discover = FALSE)$complete
    )
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_equal(read_subset(dest)$values, values)
    store <- opendap__open(dest)
    on.exit(store$close(), add = TRUE)
    expect_setequal(
        ddb_read_table(priv(store)$conn, "subset_links")$request_id,
        c("one", "two")
    )
})

# Reuse one private pool across calls, and retire it after a caller-side failure.
test_that("serial overhead fixes reuse and reset only the owned acquisition pool", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    session <- opendap__session(1L, 2)
    on.exit(session$close(), add = TRUE)
    peer <- source__pool(1L, timeout = 2)
    on.exit(peer$close(force = TRUE), add = TRUE)
    peer_pid <- peer$status()[[1L]]$pid
    dest <- tempfile("serial-session-")
    request <- data.frame(
        request_id = "city",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    make_files <- function(id) {
        docs <- query_result_test_file_docs(paste0(
            server$url(),
            "source",
            id,
            ".nc|application/netcdf|OPENDAP"
        ))
        docs$variable_id <- "tas"
        docs$instance_id <- docs$master_id <- paste0("serial-", id)
        query_result_test_object("File", docs)
    }
    run <- function(id) {
        opendap__fetch(
            make_files(id),
            request,
            dest,
            workers = 1L,
            timeout = 2,
            discover = FALSE,
            session = session
        )
    }
    expect_true(run(1L)$complete)
    pid <- session$acquire(1L, 2)$status()[[1L]]$pid
    handle <- ps::ps_handle(pid)
    expect_true(run(2L)$complete)
    expect_identical(session$acquire(1L, 2)$status()[[1L]]$pid, pid)
    expect_error(session$acquire(2L, 2))
    state <- opendap__job_state
    with_mocked_bindings(
        .package = "epwshiftr",
        expect_error(run(3L), "owned caller failure"),
        opendap__job_state = function(store, key, signature, status, ...) {
            if (status == "running") {
                stop("owned caller failure")
            }
            state(store, key, signature, status, ...)
        }
    )
    expect_false(ps::ps_is_running(handle))
    expect_true(ps::ps_is_running(ps::ps_handle(peer_pid)))
    expect_true(run(3L)$complete)
    expect_false(identical(session$acquire(1L, 2)$status()[[1L]]$pid, pid))
    session$close()
    expect_error(session$acquire(1L, 2))
})

# A manifest replacement failure must retain every prior demand reference.
# Real inbox transfers decode once, while a byte change before commit still fails closed.
test_that("serial overhead fixes decode raw transfers once and reject changed bytes", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app())
    on.exit(server$stop(), add = TRUE)
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "source.nc|application/netcdf|OPENDAP"
    ))
    docs$variable_id <- "tas"
    files <- query_result_test_object("File", docs)
    request <- data.frame(
        request_id = "city",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08"
    )
    decoded <- 0L
    commits <- 0L
    mutate <- "none"
    # Shadow only the coordinator's decoder, retaining the actual acquisition and commit.
    shadow <- new.env(parent = environment(opendap__drain))
    shadow$readRDS <- function(file, ...) {
        if (grepl("^block-[0-9]+[.]rds$", basename(file))) {
            decoded <<- decoded + 1L
        }
        base::readRDS(file, ...)
    }
    drain <- opendap__drain
    environment(drain) <- shadow
    commit <- opendap__commit
    local_mocked_bindings(
        opendap__drain = drain,
        opendap__commit = function(
            store,
            job,
            path,
            progress,
            received = NULL
        ) {
            stopifnot(!is.null(received))
            commits <<- commits + 1L
            if (mutate == "data") {
                writeBin(as.raw(1L), path)
            }
            if (mutate == "receipt") {
                jsonlite::write_json(
                    list(sha256 = "changed"),
                    paste0(path, ".json"),
                    auto_unbox = TRUE
                )
            }
            commit(store, job, path, progress, received)
        },
        .package = "epwshiftr"
    )
    dest <- tempfile("serial-decode-")
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE
        )$complete
    )
    expect_gt(commits, 0L)
    expect_identical(decoded, commits)
    expect_equal(nrow(read_subset(dest)$values), 32L)
    mutate <- "data"
    expect_error(
        files$fetch_subset(
            request,
            tempfile("serial-change-"),
            workers = 1L,
            discover = FALSE
        ),
        "changed before commit"
    )
    mutate <- "receipt"
    expect_error(
        files$fetch_subset(
            request,
            tempfile("serial-receipt-change-"),
            workers = 1L,
            discover = FALSE
        ),
        "changed before commit"
    )
})

# Coordinate read rejection is local to its File/endpoint, never evidence of a broken identity or a node-wide outage.
test_that("CF offset rejection stays retryable and local to its endpoint", {
    error <- tryCatch(
        parse_cf_time(c(0, NA_real_), "days since 2000-01-01", "noleap"),
        error = identity
    )
    expect_s3_class(error, "error")
    expect_identical(
        conditionMessage(error),
        "Invalid offsets for CF time coordinate."
    )
    expect_identical(opendap__failure_kind(error), "source_coordinate")
    reason <- opendap__gap_reason(conditionMessage(error))
    expect_identical(reason$kind, "source_coordinate")
    expect_true(reason$retryable)
    history <- data.table::data.table(
        source_key = paste0("coordinate", 1:3),
        url = paste0("url", 1:3),
        data_node = "one-node",
        client = "local",
        observed_at = 1001:1003,
        native_values = 0,
        success = FALSE,
        elapsed_seconds = 1,
        open_seconds = 0,
        error = conditionMessage(error),
        error_kind = "source_coordinate"
    )
    candidates <- data.table::as.data.table(history[, c(
        "source_key",
        "url",
        "data_node"
    )])
    expect_equal(opendap__cooldown_deadline(history[1L, ], 60), 1061)
    expect_equal(opendap__cooldown_deadline(history, 60, node = TRUE), 0)
    expect_equal(
        opendap__cooling_until(candidates, history, "local", 60, now = 1004),
        1061:1063
    )
    expect_equal(
        opendap__recovery_limits(
            candidates,
            history,
            "local",
            60,
            4L,
            now = 1004
        ),
        rep(4L, 3L)
    )
    peer <- candidates[1L]
    data.table::set(peer, j = "source_key", value = "independent")
    expect_equal(
        opendap__cooling_until(peer, history, "local", 60, now = 1004),
        0
    )
    # Exact matching keeps unknown implementation errors and content conflicts fatal or non-retryable.
    for (message in c(
        "Invalid offsets for CF time coordinate. extra",
        "Invalid offsets for CF time coordinate",
        "Manifest publication failed"
    )) {
        expect_identical(opendap__failure_kind(message), "internal")
        expect_false(opendap__gap_reason(message)$retryable)
    }
    expect_identical(
        opendap__failure_kind("Replica grid identity conflicts"),
        "source_metadata"
    )
    expect_false(
        opendap__gap_reason("Replica grid identity conflicts")$retryable
    )
    expect_error(
        parse_cf_time(Inf, "days since 2000-01-01"),
        "Invalid offsets",
        fixed = TRUE
    )
})

# Drive the public acquisition path through a real native-parser error:
# healthy peer files survive and resume cleanly, and same-identity replicas fall back.
test_that("CF offset rejection preserves healthy files, recovers later, and permits replica fallback", {
    server <- qr_test__dap_server()
    request <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-08",
        request_id = "raw"
    )
    original <- opendap__read_source
    reader <- function(job) {
        if (job$data_node == "invalid-coordinate") {
            get("parse_cf_time", asNamespace("epwshiftr"))(
                c(0, NA_real_),
                "days since 2000-01-01",
                "noleap"
            )
        }
        original(job)
    }
    environment(reader) <- list2env(
        list(original = original),
        parent = baseenv()
    )
    local_mocked_bindings(opendap__read_source = reader)

    # Distinct files: healthy peer completes, rejected file stays retryable, later call recovers only it
    dest <- withr::local_tempdir()
    files <- qr_test__dap_files(
        server,
        c("coordinate.nc", "healthy.nc"),
        nodes = c("invalid-coordinate", "healthy"),
        ids = c("coordinate-file", "healthy-file")
    )
    expect_false(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            retries = 0L
        )$complete
    )
    attempts <- subset_status(dest, "attempts")
    expect_identical(
        attempts$error_kind[!attempts$success],
        "source_coordinate"
    )
    missing <- subset_status(dest, "coverage")[!complete]
    expect_identical(missing$gap_kind, "source_coordinate")
    expect_true(missing$retryable && !missing$axis_known)
    hashes <- tools::sha256sum(subset_status(dest, "blocks")$path)

    # Same-identity replica fallback succeeds immediately despite rejected first endpoint
    dest_rep <- withr::local_tempdir()
    rep_files <- qr_test__dap_files(
        server,
        c("coordinate.nc", "consistent.nc"),
        nodes = c("invalid-coordinate", "consistent")
    )
    expect_true(
        rep_files$fetch_subset(
            request,
            dest_rep,
            workers = 1L,
            discover = FALSE,
            retries = 0L
        )$complete
    )
    rep_att <- subset_status(dest_rep, "attempts")
    expect_equal(length(unique(rep_att$source_key)), 1L)
    expect_identical(rep_att$error_kind[[1L]], "source_coordinate")
    expect_true(rep_att$success[[2L]])
    expect_equal(sort(read_subset(dest_rep)$values$value), as.double(1:32))

    # Removing fault recovers rejected peer while leaving healthy blocks untouched
    local_mocked_bindings(opendap__read_source = original)
    expect_true(
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            discover = FALSE,
            retries = 0L
        )$complete
    )
    saved <- subset_status(dest, "attempts")
    resumed <- saved[seq.int(nrow(attempts) + 1L, nrow(saved)), ]
    expect_identical(unique(resumed$source_key), missing$source_key[[1L]])
    expect_equal(tools::sha256sum(names(hashes)), hashes)
    expect_equal(
        sort(read_subset(dest)$values$value),
        sort(rep(as.double(1:32), 2L))
    )
    expect_true(all(subset_status(dest, "coverage")$complete))
})

test_that("frozen recovery reuses imported blocks with verified routes", {
    skip_if_not_installed("webfakes")
    identity <- c(
        tracking_id = "hdl:21.14100/mock-file",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day"
    )
    server <- webfakes::new_app_process(
        subset_test__dap_app(global_attributes = identity)
    )
    withr::defer(server$stop())
    docs <- query_result_test_file_docs(paste0(
        server$url(),
        "missing.nc#show=url|application/netcdf|OPENDAP"
    ))
    docs$title <- "fast.nc"
    docs$source_id <- "fixture"
    docs$experiment_id <- "ssp245"
    docs$table_id <- "day"
    docs$variable_id <- "tas"
    docs$checksum <- strrep("a", 64L)
    files <- query_result_test_object("File", docs)
    catalogue <- files$access_plan(expand = FALSE)
    key <- unique(catalogue$source_key)
    routes <- data.frame(
        source_key = key,
        url = paste0(server$url(), "fast.nc#show=url"),
        logical_version = catalogue$version,
        physical_version = "20260102",
        tracking_id = docs$tracking_id,
        checksum = docs$checksum,
        size = as.character(docs$size),
        receipt_sha256 = strrep("b", 64L)
    )
    requests <- data.frame(
        request_id = c("first", "missing"),
        case_id = "fixture-case",
        site_id = "fixture-site",
        lon = 10.25,
        lat = 0.25,
        time_start = c("2060-01-01", "2060-01-04"),
        time_stop = c("2060-01-01", "2060-01-04"),
        source_id = docs$source_id,
        experiment_id = docs$experiment_id,
        table_id = docs$table_id,
        variable_id = docs$variable_id
    )
    root <- withr::local_tempdir()
    original <- file.path(root, "original")
    destination <- file.path(root, "destination")
    traffic <- function() {
        response <- curl::curl_fetch_memory(paste0(server$url(), "requests"))
        calls <- jsonlite::fromJSON(rawToChar(response$content))
        calls[grepl("[.]dods tas", calls)]
    }
    wrong <- routes
    wrong$tracking_id <- "hdl:wrong"
    expect_error(
        files$fetch_subset(requests[1L, ], original, verified_replicas = wrong),
        "does not match the frozen File identity"
    )
    first <- files$fetch_subset(
        requests[1L, ],
        original,
        workers = 1L,
        retries = 0L,
        discover = FALSE,
        verified_replicas = routes
    )
    expect_true(first$complete)
    before <- subset_status(original, "blocks")
    expect_length(traffic(), 1L)
    expect_equal(
        import_subset_blocks(original, destination, key)[["subset_blocks"]],
        1L
    )
    expect_true(all(import_subset_blocks(original, destination, key) == 0L))

    # Recreate the persisted layout of an interrupted recovery-v1 admission.
    frozen <- qr_test__frozen_plan(files, requests, key, root)
    old <- frozen$state
    recover <- function(max_batches) {
        resume_subsets(
            frozen$plan,
            frozen$denominator,
            frozen$journal,
            destination,
            verified_replicas = routes,
            max_batches = max_batches,
            discover = FALSE
        )
    }
    expect_identical(recover(0L), old)

    # Block publication using a real filesystem collision in the test Store.
    # The inherited admission and the moved-aside prior payload must survive.
    subsets <- file.path(destination, "subsets")
    held <- file.path(destination, "held")
    stopifnot(file.rename(subsets, held), file.create(subsets))
    failed <- suppressWarnings(recover(1L))
    expect_identical(failed$phase, "blocked_error")
    expect_identical(failed$in_flight, old$in_flight)
    expect_identical(failed$admissions, old$admissions)
    expect_identical(
        checksum_file(file.path(held, basename(before$path))),
        before$sha256
    )
    expect_length(traffic(), 1L)
    stopifnot(unlink(subsets) == 0L, file.rename(held, subsets))

    after <- recover(1L)
    expect_identical(after$phase, "complete")
    expect_identical(after$admissions, old$admissions)
    expect_identical(after$started, old$started)
    expect_null(after$in_flight)
    blocks <- subset_status(destination, "blocks")
    expect_identical(
        blocks$sha256[match(before$block_id, blocks$block_id)],
        before$sha256
    )
    added <- blocks[!blocks$block_id %in% before$block_id]
    expect_true(all(added$time_start_index == 4L))
    expect_equal(sum(added$lat_count * added$lon_count * added$time_count), 4L)
    requests_sent <- traffic()
    expect_length(requests_sent, 1L + nrow(added))
    expect_true(all(grepl(
        "tas%5b3(:3)?%5d",
        requests_sent[-1L],
        ignore.case = TRUE
    )))
    expect_identical(recover(1L)$phase, "complete")
    expect_identical(traffic(), requests_sent)
    saved <- read_subset(destination, case_id = "fixture-case")
    expect_identical(sort(as.integer(saved$values$value)), c(1:4, 13:16))
})

test_that("native DAP slabs preserve their exact time indices", {
    source <- array(as.integer(1:12), dim = c(2L, 2L, 3L))
    source[1L, 1L, 2L] <- NA_integer_
    requested <- list()
    read <- function(var, start, count, index, collapse) {
        requested[[length(requested) + 1L]] <<- c(start, count)
        if (count[[3L]] > 1L) {
            stop("NetCDF: DAP server error")
        }
        source[,, start[[3L]], drop = FALSE]
    }
    args <- list(
        var = "tas",
        start = c(1L, 1L, 1L),
        count = c(2L, 2L, 3L),
        index = 1L,
        collapse = FALSE
    )
    expect_identical(
        do.call(
            dataset__read_dap_weather,
            c(list(dataset = list(var_get = read)), args)
        ),
        source
    )
    expect_identical(
        vapply(Filter(function(x) x[[6L]] == 1L, requested), "[[", 0L, 3L),
        1:3
    )
    denied <- function(...) stop("Permission denied")
    expect_error(
        do.call(
            dataset__read_dap_weather,
            c(list(dataset = list(var_get = denied)), args)
        ),
        "Permission denied"
    )
    expect_identical(
        dataset__boundary_calendar("noleap"),
        dataset__boundary_calendar("365_day")
    )
    expect_false(identical(
        dataset__boundary_calendar("360_day"),
        dataset__boundary_calendar("365_day")
    ))
    expect_identical(normalize_cf_calendar("noleap"), "noleap")
})

test_that("fetch, read-back, status and frozen recovery agree on the missing corner", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(subset_test__dap_app(
        fail_last_corner = TRUE
    ))
    withr::defer(server$stop())
    dest <- withr::local_tempdir()
    docs <- data.frame(
        id = "coverage-file",
        dataset_id = "coverage-dataset",
        size = 32,
        checksum = strrep("a", 64L),
        checksum_type = "SHA256",
        instance_id = "coverage-instance",
        master_id = "coverage-master",
        replica = FALSE,
        tracking_id = "hdl:21.14100/coverage-fixture",
        title = "source.nc",
        version = 20260101L,
        data_node = "fixture",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day",
        variable_id = "tas"
    )
    docs$url <- I(list(paste0(
        server$url(),
        "source.nc|application/netcdf|OPENDAP"
    )))
    files <- query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = query_param__as_store(list(type = "File", project = "CMIP6")),
        result = esgf_test__response(docs, timestamp = Sys.time())
    )
    request <- data.frame(
        request_id = "two-days",
        case_id = "city",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-02",
        source_id = docs$source_id,
        experiment_id = docs$experiment_id,
        table_id = docs$table_id,
        variable_id = docs$variable_id
    )
    fetch <- function(request, resume = TRUE) {
        files$fetch_subset(
            request,
            dest,
            workers = 1L,
            retries = 0L,
            discover = FALSE,
            resume = resume,
            max_values = 4L
        )
    }
    control <- function(path) {
        curl::curl_fetch_memory(paste0(server$url(), path))$content
    }
    weather_requests <- function() {
        calls <- jsonlite::fromJSON(rawToChar(control("requests")))
        grep("[.]dods tas", calls, value = TRUE)
    }

    # Seed one full day so the real worker reads only missing points on day two.
    first <- request
    first$request_id <- "one-day"
    first$time_stop <- first$time_start
    expect_true(fetch(first)$complete)
    expect_false(fetch(request)$complete)

    # The fixture's second-day values are 5–8, with 8 at native (2,2,2).
    raw <- read_subset(dest)
    expect_equal(sort(raw$values$value), as.double(1:7))
    expect_identical(raw$gaps$incomplete_requests$request_id, "two-days")
    checked <- subset_status(dest, "coverage")
    checked <- checked[checked$request_id == "two-days", ]
    expect_false(checked$complete)
    expect_equal(checked$available_native_values, 7)
    expect_equal(checked$expected_native_values, 8)
    before <- subset_status(dest, "blocks")
    old_hashes <- tools::sha256sum(c(before$path, before$metadata_path))

    # A different site shares the same grid. Its interrupted forced refresh
    # publishes selection metadata but no coverage for the new request.
    control("fail")
    pending <- request
    pending$request_id <- pending$case_id <- pending$site_id <- "pending"
    pending$lon <- 10.75
    expect_false(fetch(pending, resume = FALSE)$complete)
    checked <- subset_status(dest, "coverage")
    expect_equal(
        checked$available_native_values[match(
            c("two-days", "pending"),
            checked$request_id
        )],
        c(7, 0)
    )
    waiting <- checked[checked$request_id == "pending", ]
    expect_true(waiting$axis_known)
    expect_equal(waiting$expected_native_values, 8)
    expect_false(waiting$complete)
    raw <- read_subset(dest, case_id = "pending")
    expect_equal(nrow(raw$values), 0)
    expect_identical(raw$gaps$incomplete_requests$request_id, "pending")

    # Build the same frozen-plan/journal inputs used by a real recovery call.
    frozen <- qr_test__frozen_plan(
        files,
        request,
        checked$source_key[[1L]],
        withr::local_tempdir()
    )
    original <- frozen$state
    plan <- frozen$plan
    denominator <- frozen$denominator
    journal <- frozen$journal
    control("recover")
    control("reset")

    # Simulate a transport error after the real worker has written its last
    # complete block. The coordinator still drains and commits that block.
    drain <- opendap__drain
    local_mocked_bindings(opendap__drain = function(store, state, client) {
        changed <- drain(store, state, client)
        if (is.null(state$error) && !is.null(state$result)) {
            state$error <- simpleError("NetCDF: connection reset after data")
        }
        changed
    })
    resume_batch <- function() {
        resume_subsets(
            plan,
            denominator,
            journal,
            dest,
            max_batches = 1L,
            discover = FALSE
        )
    }
    resumed <- resume_batch()
    expect_equal(resumed$summary$files_complete, 1)
    expect_equal(resumed$summary$available_native_values_known_axes, 8)
    expect_identical(resumed$admissions, original$admissions)
    expect_identical(tools::sha256sum(names(old_hashes)), old_hashes)
    added <- subset_status(dest, "blocks")
    added <- added[!added$block_id %in% before$block_id, ]
    expect_equal(nrow(added), 1)
    expect_equal(
        added[,
            c(
                "time_start_index",
                "ind_lat",
                "ind_lon",
                "time_count",
                "lat_count",
                "lon_count"
            ),
            with = FALSE
        ] |>
            as.list() |>
            unlist(use.names = FALSE),
        c(2, 2, 2, 1, 1, 1)
    )
    traffic <- weather_requests()
    expect_length(traffic, 1L)
    expect_match(
        utils::URLdecode(traffic),
        "tas[[]1(:1(:1)?)?[]][[]1(:1(:1)?)?[]][[]1(:1(:1)?)?[]]"
    )
    saved <- subset_status(dest, "coverage")
    expect_true(saved$complete[saved$request_id == "two-days"])
    expect_equal(
        saved$available_native_values[saved$request_id == "two-days"],
        8
    )
    expect_false(saved$complete[saved$request_id == "pending"])
    expect_equal(
        saved$available_native_values[saved$request_id == "pending"],
        0
    )

    raw <- read_subset(dest, case_id = "city")
    expect_equal(sort(raw$values$value), as.double(1:8))
    expect_equal(nrow(raw$gaps$incomplete_requests), 0)
    expect_true(fetch(request)$complete)
    expect_identical(resume_batch()$phase, "complete")
    expect_identical(weather_requests(), traffic)

    # Still report verified coverage when the attempt failed after committing.
    late_failure <- fetch(request, resume = FALSE)
    expect_identical(late_failure$jobs$status, "failed")
    expect_match(late_failure$jobs$error, "connection reset after data")
    expect_true(late_failure$complete)

    # An unsuccessful forced refresh cannot claim blocks from a prior call.
    control("fail")
    expect_false(fetch(request, resume = FALSE)$complete)
    saved <- subset_status(dest, "coverage")
    expect_true(saved$complete[saved$request_id == "two-days"])
})

# vim: fdm=marker :
