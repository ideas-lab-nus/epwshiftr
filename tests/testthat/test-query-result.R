# EsgResult {{{
local_test_cache(scope = "persist")
withr::local_options(list(epwshiftr.progress = FALSE))

# query_result_test_response {{{
query_result_test_response <- function(docs) {
    esgf_test__response(docs, timestamp = Sys.time())
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

# qr_test__svc_url {{{
qr_test__svc_url <- function(host, path, svc = "OPENDAP") {
    sprintf("https://%s/%s|application/netcdf|%s", host, path, svc)
}
# }}}

# query_result_test_contract_dataset_docs {{{
query_result_test_contract_dataset_docs <- function() {
    exps <- c("ssp126", "ssp245")
    docs <- data.frame(
        id = sprintf(
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.%s.r1i1p1f1.day.tas.gn.v20190529|esgf.example.org",
            exps
        ),
        version = rep(20190529L, 2L),
        activity_id = rep("ScenarioMIP", 2L),
        data_node = rep("esgf.example.org", 2L),
        experiment_id = exps,
        frequency = rep("day", 2L),
        index_node = rep("https://esgf.example.org", 2L),
        instance_id = paste0("dataset-instance-", exps),
        latest = c(TRUE, TRUE),
        master_id = paste0("dataset-master-", exps),
        number_of_files = c(2L, 1L),
        project = rep("CMIP6", 2L),
        replica = c(FALSE, FALSE),
        size = c(1024^3, 2 * 1024^3),
        source_id = rep("AWI-CM-1-1-MR", 2L),
        variable_id = rep("tas", 2L),
        variant_label = rep("r1i1p1f1", 2L),
        check.names = FALSE
    )
    docs$access <- I(list(c("OPENDAP", "HTTPServer"), "HTTPServer"))
    docs
}
# }}}

# query_result_test_contract_file_docs {{{
query_result_test_contract_file_docs <- function() {
    ds_ids <- query_result_test_contract_dataset_docs()$id
    stems <- c("ssp126-2050", "ssp126-2051", "ssp245-2050")
    titles <- sprintf(
        "tas_day_AWI-CM-1-1-MR_%s_r1i1p1f1_gn_%s0101-%s1231.nc",
        c("ssp126", "ssp126", "ssp245"),
        c("2050", "2051", "2050"),
        c("2050", "2051", "2050")
    )
    docs <- data.frame(
        id = paste0(titles, "|dataset-", c("ssp126", "ssp126", "ssp245")),
        version = rep(20190529L, 3L),
        activity_id = rep("ScenarioMIP", 3L),
        institution_id = rep("AWI", 3L),
        dataset_id = ds_ids[c(1L, 1L, 2L)],
        size = (1:3) * 1024^2,
        checksum = paste0("sha-", 1:3),
        checksum_type = rep("SHA256", 3L),
        instance_id = paste0("file-instance-", 1:3),
        master_id = paste0("file-master-", 1:3),
        replica = rep(FALSE, 3L),
        tracking_id = paste0("hdl:21.14100/mock-", 1:3),
        title = titles,
        data_node = rep("esgf.example.org", 3L),
        check.names = FALSE
    )
    docs$url <- I(lapply(stems, function(s) {
        c(
            qr_test__svc_url(
                "esgf.example.org",
                paste0("dods/", s, ".nc.html")
            ),
            qr_test__svc_url(
                "esgf.example.org",
                paste0("files/", s, ".nc"),
                "HTTPServer"
            )
        )
    }))
    docs
}
# }}}

# query_result_test_contract_aggregation_docs {{{
query_result_test_contract_aggregation_docs <- function() {
    ds_ids <- query_result_test_contract_dataset_docs()$id
    exps <- c("ssp126", "ssp245")
    docs <- data.frame(
        id = sprintf(
            "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.%s.r1i1p1f1.day.tas.gn.tas.20500101.aggregation|esgf.example.org",
            exps
        ),
        version = rep(20190529L, 2L),
        activity_id = rep("ScenarioMIP", 2L),
        institution_id = rep("AWI", 2L),
        data_node = rep("esgf.example.org", 2L),
        dataset_id = ds_ids,
        instance_id = paste0("aggregation-instance-", 1:2),
        master_id = paste0("aggregation-master-", 1:2),
        replica = c(FALSE, FALSE),
        size = c(0, 0),
        title = paste("tas aggregation 2050", exps),
        check.names = FALSE
    )
    docs$url <- I(lapply(exps, function(e) {
        c(
            qr_test__svc_url(
                "esgf.example.org",
                paste0("dods/", e, "-aggregation.ncml")
            ),
            qr_test__svc_url(
                "esgf.example.org",
                paste0("files/", e, "-aggregation.ncml"),
                "HTTPServer"
            )
        )
    }))
    docs
}
# }}}

# query_result_test_file_time_docs {{{
query_result_test_file_time_docs <- function(type = "File") {
    tags <- c("2050", "2080", "unknown")
    titles <- paste0(
        "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_",
        c("20500101-20501231", "20800101-20801231", "unknown"),
        ".nc"
    )
    docs <- data.frame(
        id = paste0(titles, "|dataset-1"),
        dataset_id = "dataset-1",
        size = 1:3,
        checksum = c("abc", "def", "ghi"),
        checksum_type = "SHA256",
        instance_id = paste0("file-instance-", tags),
        master_id = paste0("master-file-", tags),
        replica = FALSE,
        version = rep(20260101L, 3L),
        tracking_id = paste0("hdl:21.14100/mock-file-", tags),
        title = titles,
        data_node = "example.org",
        check.names = FALSE
    )
    docs$url <- I(as.list(qr_test__svc_url(
        "example.org",
        paste0("dods/", tags, ".nc.html")
    )))
    if (identical(type, "Aggregation")) {
        docs$checksum <- docs$checksum_type <- docs$master_id <- docs$tracking_id <- NULL
    }
    docs
}
# }}}

# Shared test mocks and row builders {{{
qr_test__file_docs_rows <- function(urls, ...) {
    docs <- query_result_test_file_docs()[rep(1L, length(urls)), , drop = FALSE]
    row.names(docs) <- NULL
    fields <- list(...)
    for (nm in names(fields)) {
        docs[[nm]] <- fields[[nm]]
    }
    docs$url <- I(as.list(urls))
    docs
}

qr_test__mock_collect <- function(
    docs,
    url = "https://example.org/replicas",
    check = NULL
) {
    function(index_node, params, required_fields = NULL, ...) {
        if (!is.null(check)) {
            check(index_node, params, required_fields, ...)
        }
        out <- if (is.function(docs)) docs(index_node, params, ...) else docs
        qurl <- if (is.function(url)) url(index_node, params) else url
        list(
            response = query_result_test_response(out),
            docs = out,
            parameter = query_param__clone(params),
            context = list(query_url = qurl)
        )
    }
}

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
            ua <- network_policy$useragent
            log$agents <- c(log$agents, if (is.null(ua)) NA_character_ else ua)
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
                        paste0(yr, c("-01-01", "-12-31")),
                        tz = "UTC"
                    )
                )
            }
        ),
        active = list(is_open = function() private$opened),
        private = list(nc_handles = NULL, opened = FALSE, context = list())
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
    f_untyped <- withr::local_tempfile(fileext = ".json")
    expect_error(base_res$save(f_untyped), "untyped ESGF result")
    writeLines("{}", f_untyped)
    expect_error(base_res$load(f_untyped), "untyped ESGF result")

    # Round-trip Dataset, File (with timestamps & publish_path), and Aggregation
    both <- c(
        qr_test__svc_url("example.org", "dods/file.nc.html"),
        qr_test__svc_url("example.org", "file.nc", "HTTPServer")
    )
    file_docs <- query_result_test_file_docs(both)
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
            docs = query_result_test_file_docs(both)
        )
    )
    urls <- paste0("https://example.org/search?page=", 1:2)
    for (nm in names(cases)) {
        res <- query_result_test_object(
            cases[[nm]]$type,
            cases[[nm]]$docs,
            context = list(query_url = urls)
        )
        f <- withr::local_tempfile(fileext = ".json")
        expect_type(res$save(f), "character")
        got <- expect_s3_class(esg_result(nm)$load(f), class(res)[[1L]])
        expect_identical(got$id, res$id)
        expect_identical(
            priv(got)$parameter$serialize(null = TRUE),
            priv(res)$parameter$serialize(null = TRUE)
        )
        expect_identical(names(got$to_data_table()), names(res$to_data_table()))
        expect_identical(got$query_url(), stats::setNames(urls[[1L]], "page1"))
        expect_identical(
            got$query_url("all"),
            stats::setNames(urls, c("page1", "page2"))
        )
        expect_error(got$to_data_table(character()), "Must have length")
        if (nm == "file") {
            expect_identical(got$timestamp, "2026-06-09T00:00:00Z")
            expect_identical(got$`_timestamp`, "2026-06-10T01:14:40.946Z")
            expect_identical(got$version, "v20240509")
            expect_identical(got$publish_path, file_docs$publish_path)
        }
    }

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
    urls <- paste0("https://example.org/search?page=", 1:2)
    tf <- list(
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
        res <- query_result_test_object(
            type,
            cases[[type]],
            context = list(query_url = urls, time_filter = tf)
        )
        priv(res)$response$response$numFound <- 10L
        n <- nrow(cases[[type]])
        expect_identical(res[], res)
        expect_identical(
            res$selection(),
            list(
                source_count = as.integer(n),
                source_num_found = 10L,
                source_indices = seq_len(n)
            )
        )
        sel <- res[c(n, 1L)]
        expect_identical(sel$id, res$id[c(n, 1L)])
        expect_identical(
            sel$query_url("all"),
            stats::setNames(urls, c("page1", "page2"))
        )
        expect_identical(sel$time_filter, tf)
        expect_identical(sel[2L]$id, res$id[1L])
        expect_identical(sel[2L]$selection()$source_indices, 1L)
        odd <- seq_len(n) %% 2L == 1L
        expect_identical(res[odd]$id, res$id[odd])
        expect_identical(res[res$id[1L]]$id, res$id[1L])
        expect_identical(res[-1L]$id, res$id[-1L])
        expect_identical(res$slice(NULL)$selection()$source_indices, integer())
    }

    res <- query_result_test_object("File", query_result_test_file_time_docs())
    priv(res)$response$response$numFound <- 10L
    flt <- res$filter(function(dt) grepl("2050", dt$title))
    expect_identical(flt$id, res$id[1L])
    expect_identical(flt$selection()$source_indices, 1L)
    expect_identical(
        res$filter(function(dt) rep(TRUE, nrow(dt)), formatted = TRUE)$id,
        res$id
    )
    for (bad in list(
        function(dt) TRUE,
        function(dt) c(TRUE, NA, FALSE),
        function(dt) seq_len(nrow(dt))
    )) {
        expect_error(res$filter(bad), "predicate result")
    }
    bad_sel <- list(
        list(c(1L, 1L), "duplicate indices"),
        list(c(res$id[1L], res$id[1L]), "duplicate record IDs"),
        list(4L, "between 1 and 3"),
        list(c(1L, -2L), "must not mix"),
        list(0L, "must not contain zero"),
        list(c(TRUE, FALSE), "length"),
        list("missing-id", "Unknown record ID")
    )
    for (spec in bad_sel) {
        expect_error(res[spec[[1L]]], spec[[2L]])
    }
    expect_error(res[1L, ], "one-dimensional")
    expect_error(res[1L, drop = TRUE], "one-dimensional")

    for (sel in list(res[c(3L, 1L)], res$slice(integer()))) {
        f <- withr::local_tempfile(fileext = ".json")
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

    ok_url <- local_downloader_http_server()$url("/files/ok.bin")
    miss_srv <- local_downloader_http_server()
    miss_url <- miss_srv$url("/files/missing.bin")
    urls <- c(ok_url, miss_url, ok_url)

    node <- query_result__reach_node_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_named(node, c(ok_url, miss_url))
    expect_true(all(vapply(node, function(x) isTRUE(x$reachable), logical(1L))))

    reach <- query_result__reach_http_urls(
        urls,
        timeout = 5,
        probe_concurrency = 2L
    )
    expect_true(reach[[ok_url]]$reachable)
    expect_false(reach[[miss_url]]$reachable)
    expect_match(reach[[miss_url]]$error, "404")

    lat <- query_result__latency_urls(urls, timeout = 5, probe_concurrency = 2L)
    expect_true(all(vapply(
        lat,
        function(x) is.finite(x$latency) && is.na(x$throughput),
        logical(1L)
    )))

    # OPeNDAP URL checks require a valid DDS response
    valid <- local_downloader_http_server()$url("/dods/valid.nc")
    html <- local_downloader_http_server()$url("/dods/html.nc")
    miss_dap <- miss_srv$url("/dods/missing.nc")
    chk <- query_result__check_opendap_urls(
        c(valid, html, miss_dap),
        timeout = 5,
        concurrency = 3L
    )
    expect_true(chk[[valid]]$reachable)
    expect_false(chk[[html]]$reachable)
    expect_match(chk[[html]]$error, "valid DDS")
    expect_false(chk[[miss_dap]]$reachable)
    expect_match(chk[[miss_dap]]$error, "404")
    expect_true(query_result__valid_dds(charToRaw(
        "Dataset { Float32 tas[time = 1]; } valid.nc;"
    )))
    expect_false(query_result__valid_dds(charToRaw("<html>error</html>")))
})

test_that("EsgResult$reachable() returns URL-level and default data-node diagnostics", {
    u_same <- qr_test__svc_url("same.example.org", "dods/file.nc")
    docs <- qr_test__file_docs_rows(
        list(
            c(
                u_same,
                qr_test__svc_url("same.example.org", "file.nc", "HTTPServer")
            ),
            u_same,
            character(),
            qr_test__svc_url("bad.example.org", "dods/file.nc")
        ),
        id = c("file-ok", "file-dup", "file-missing", "file-fail"),
        data_node = paste0(c("same", "same", "missing", "bad"), ".example.org")
    )
    result <- query_result_test_object("File", docs)

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
    expect_identical(
        result$reachable(service = "HTTPServer", level = "url")$reachable,
        c(TRUE, NA, NA, NA)
    )
    expect_equal(nrow(result$slice(integer())$reachable()), 0L)

    # Default data_node level checks node roots once per host and falls back to URL host
    docs$data_node[[4L]] <- NA_character_
    docs$url[[4L]] <- qr_test__svc_url("fallback.example.org", "dods/file.nc")
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
    ndiag <- query_result_test_object("File", docs)$reachable(
        probe = list(cache_seconds = 0L, cache_failures_seconds = 0L)
    )
    expect_identical(ndiag$probe_level, rep("data_node", 4L))
    expect_identical(
        ndiag$probe_url,
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
    pair <- function(host) {
        c(
            qr_test__svc_url(host, "dods/file.nc"),
            qr_test__svc_url(host, "file.nc", "HTTPServer")
        )
    }
    docs <- qr_test__file_docs_rows(
        list(pair("bad.example.org"), pair("ok.example.org")),
        id = c("file-bad", "file-ok"),
        dataset_id = c(
            "dataset-bad|bad.example.org",
            "dataset-ok|ok.example.org"
        ),
        master_id = c("master-file-bad", "master-file-ok"),
        instance_id = c("instance-bad", "instance-ok"),
        data_node = c("bad.example.org", "ok.example.org")
    )
    result <- query_result_test_object(
        "File",
        docs,
        context = list(query_url = "https://origin.example.org/search")
    )
    cand <- qr_test__file_docs_rows(
        list(pair("replica.example.org")),
        id = "file-repaired",
        master_id = "master-file-bad",
        instance_id = "instance-bad",
        data_node = "replica.example.org",
        replica = TRUE
    )

    call_log <- new.env(parent = emptyenv())
    call_log$calls <- list()
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
            log = call_log
        ),
        query__collect = qr_test__mock_collect(
            cand,
            url = "https://replica-index.example.org/replicas",
            check = function(index_node, params, ...) {
                expect_identical(
                    index_node,
                    "https://replica-index.example.org"
                )
                expect_null(params$project())
                expect_identical(
                    query_param__value(params$params()$instance_id),
                    "instance-bad"
                )
            }
        ),
        .package = "epwshiftr"
    )
    rep_res <- result$repair_urls(
        index_node = "replica-index.example.org",
        probe = list(
            timeout = 11,
            concurrency = 2L,
            network_policy = list(useragent = "repair-test")
        )
    )
    expect_identical(
        rep_res$url_opendap[[1L]],
        "https://replica.example.org/dods/file.nc"
    )
    expect_identical(
        rep_res$url_download[[1L]],
        "https://bad.example.org/file.nc"
    )
    expect_identical(
        unname(rep_res$query_url("all")),
        c(
            "https://origin.example.org/search",
            "https://replica-index.example.org/replicas"
        )
    )
    expect_true(all(vapply(
        call_log$calls,
        function(x) x$timeout == 11 && x$probe_concurrency == 2L,
        logical(1L)
    )))

    # Prefer reachable replica already in current result without external query
    curr_docs <- qr_test__file_docs_rows(
        c(
            qr_test__svc_url("bad.example.org", "dods/file.nc"),
            qr_test__svc_url("good.example.org", "dods/file.nc")
        ),
        id = c("file-bad", "file-ok"),
        master_id = "master-current",
        instance_id = "instance-current",
        data_node = c("bad.example.org", "good.example.org")
    )
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            dn == "good.example.org"
        }),
        query__collect = function(...) {
            stop("current result replica should avoid external query")
        },
        .package = "epwshiftr"
    )
    expect_identical(
        query_result_test_object("File", curr_docs)$repair_urls()$url_opendap,
        rep("https://good.example.org/dods/file.nc", 2L)
    )

    # Independent HTTPServer repair and Aggregation repair
    u_dap <- qr_test__svc_url("opendap.example.org", "dods/file.nc")
    http_docs <- query_result_test_file_docs(c(
        u_dap,
        qr_test__svc_url("bad-http.example.org", "file.nc", "HTTPServer")
    ))
    http_cand <- qr_test__file_docs_rows(
        list(c(
            u_dap,
            qr_test__svc_url(
                "http-replica.example.org",
                "file.nc",
                "HTTPServer"
            )
        )),
        data_node = "http-replica.example.org"
    )
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(function(
            u,
            s
        ) {
            grepl("http-replica", u)
        }),
        query__collect = qr_test__mock_collect(
            http_cand,
            url = "https://example.org/http-replicas"
        ),
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

    agg_docs <- qr_test__file_docs_rows(
        qr_test__svc_url("bad.example.org", "dods/agg.nc"),
        data_node = "bad.example.org"
    )
    agg_cand <- qr_test__file_docs_rows(
        qr_test__svc_url("agg-replica.example.org", "dods/agg.nc"),
        data_node = "agg-replica.example.org"
    )
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            grepl("agg-replica", dn)
        }),
        query__collect = qr_test__mock_collect(
            agg_cand,
            url = "https://example.org/agg-replicas",
            check = function(index_node, params, ...) {
                expect_identical(
                    query_param__value(params$type()),
                    "Aggregation"
                )
            }
        ),
        .package = "epwshiftr"
    )
    agg_rep <- query_result_test_object("Aggregation", agg_docs)$repair_urls()
    expect_s3_class(agg_rep, "EsgResultAggregation")
    expect_identical(
        agg_rep$url_opendap,
        "https://agg-replica.example.org/dods/agg.nc"
    )

    # Keeps originals and warns when repair is impossible; rejects unknown option fields
    unrep_docs <- docs
    unrep_docs$master_id <- c(NA_character_, "master-no-replica")
    unrep_docs$instance_id <- c(NA_character_, NA_character_)
    testthat::local_mocked_bindings(
        query_result__reach_nodes = qr_test__mock_reach_nodes(function(dn) {
            rep(FALSE, length(dn))
        }),
        query__collect = qr_test__mock_collect(
            unrep_docs[2L, , drop = FALSE],
            url = "https://example.org/no-replica"
        ),
        .package = "epwshiftr"
    )
    unrep <- query_result_test_object("File", unrep_docs)
    warns <- character()
    kept <- withCallingHandlers(
        unrep$repair_urls(),
        warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_true(any(grepl("instance_id", warns)))
    expect_true(any(grepl("No reachable OPENDAP replica", warns)))
    expect_identical(kept$id, unrep$id)
    expect_error(
        unrep$repair_urls(probe = list(foo = 1)),
        "Unknown `probe` field"
    )
})

test_that("EsgResult$expand_replicas() falls back to master and version without crossing versions", {
    docs <- query_result_test_file_docs()
    docs$instance_id <- NA_character_
    docs$master_id <- "master-file-fallback"
    docs$version <- 20260101L
    cand <- docs[c(1L, 1L), , drop = FALSE]
    cand$id <- c("file-same-version", "file-other-version")
    cand$version <- c(20260101L, 20270101L)
    testthat::local_mocked_bindings(
        query__collect = qr_test__mock_collect(
            cand,
            url = "https://example.org/master-version",
            check = function(index_node, params, ...) {
                expect_null(params$project())
                expect_identical(
                    query_param__value(params$params()$master_id),
                    "master-file-fallback"
                )
            }
        ),
        .package = "epwshiftr"
    )
    expanded <- expect_s3_class(
        query_result_test_object("File", docs)$expand_replicas(),
        "EsgResultFile"
    )
    expect_identical(expanded$id, "file-same-version")
    expect_identical(expanded$version, 20260101L)
})

test_that("File service resolution composes compatible replicas, collapses duplicates, and defers HTTP checks", {
    pair <- function(host, http_path = "files/file.nc") {
        c(
            qr_test__svc_url(host, "dods/file.nc"),
            qr_test__svc_url(host, http_path, "HTTPServer")
        )
    }
    orig <- qr_test__file_docs_rows(
        list(pair("bad.example.org", "file.nc")),
        id = "file-original",
        instance_id = "instance-shared",
        master_id = "master-shared",
        data_node = "bad.example.org"
    )
    cands <- qr_test__file_docs_rows(
        list(pair("dap.example.org"), pair("http.example.org")),
        id = c("file-dap", "file-http"),
        instance_id = "instance-shared",
        master_id = "master-shared",
        data_node = c("dap.example.org", "http.example.org")
    )

    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(function(
            u,
            s
        ) {
            host <- if (identical(s, "OPENDAP")) {
                "dap.example.org"
            } else {
                "http.example.org"
            }
            grepl(host, u, fixed = TRUE)
        }),
        query__collect = qr_test__mock_collect(cands),
        .package = "epwshiftr"
    )

    chk <- list(level = "url", cache_seconds = 0L, cache_failures_seconds = 0L)
    resolved <- query_result__resolve_file_services(
        query_result_test_object("File", orig),
        check = chk
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

    incompat <- data.table::as.data.table(cands)
    data.table::set(incompat, i = 2L, j = "checksum", value = "different")
    expect_identical(
        query_result__compatible_content(orig, incompat),
        c(TRUE, FALSE)
    )
    expect_identical(query_result__compatible_file_groups(incompat), c(1L, 2L))

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

    dead_http <- data.table::rbindlist(
        list(orig, cands[2L, , drop = FALSE]),
        use.names = TRUE,
        fill = TRUE
    )
    data.table::set(
        dead_http,
        j = c("instance_id", "master_id"),
        value = list(c("inst-dead", "inst-http"), c("m-dead", "m-http"))
    )
    retained <- query_result__resolve_file_services(
        query_result_test_object("File", dead_http),
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

# EsgResult$filter_time(), $fields, and empty results {{{
test_that("EsgResult$filter_time() supports DRS, auto, OPeNDAP time axes, and persistence", {
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
        paste0(
            "tas_day_M_x_",
            c("20400101-20401231", "20500101-20501231", "unknown"),
            ".nc"
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
        res <- query_result_test_object(
            type,
            query_result_test_file_time_docs(type)
        )
        expect_warning(
            flt <- res$filter_time("2050-06-01", "2050-06-30", method = "drs"),
            sprintf(
                "Could not parse a DRS time range for 1 %s record",
                tolower(type)
            )
        )
        expect_identical(flt$id, res$id[c(1L, 3L)])
        expect_identical(flt$time_filter$unknown_count, 1L)
        expect_identical(flt$selection()$source_indices, c(1L, 3L))
    }

    tdocs <- query_result_test_file_time_docs("File")
    silent_flt <- expect_silent(
        query_result_test_object(
            "File",
            tdocs[1:2, , drop = FALSE]
        )$filter_time(
            "2050-06-01",
            "2050-06-30",
            method = "drs"
        )
    )
    expect_identical(silent_flt$time_filter$unknown_count, 0L)

    # Auto mode preserves metadata and fills DRS gaps, and persists via save/load
    auto_docs <- tdocs
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

    f <- withr::local_tempfile(fileext = ".json")
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
        dap_flt <- query_result_test_object("File", tdocs)$filter_time(
            "2050-06-01",
            "2050-06-30",
            method = "opendap"
        ),
        "OPeNDAP time axes"
    )
    expect_identical(dap_flt$id, tdocs$id[c(1L, 3L)])
    expect_identical(dap_flt$time_filter$method, "opendap")
})

test_that("EsgResult$fields, $to_data_table(), and empty Dataset/File/Aggregation results stay consistent", {
    for (type in c("Dataset", "File", "Aggregation")) {
        expect_identical(
            query_result_test_object(
                type,
                data.frame(check.names = FALSE)
            )$fields,
            character()
        )
    }
    empty_docs <- query_result_test_file_docs(character())[0L, ]
    expect_true(all(
        c("filename", "url_opendap", "url_download") %in%
            query_result_test_object("File", empty_docs)$fields
    ))

    for (nm in c("file", "aggregation")) {
        type <- if (nm == "file") "File" else "Aggregation"
        f <- withr::local_tempfile(fileext = ".json")
        query_result_test_object(type, empty_docs)$save(f)
        loaded <- esg_result(nm)$load(f)
        expect_equal(loaded$count(), 0L)
        expect_identical(loaded$fields, character())
    }

    docs <- query_result_test_file_docs(c(
        qr_test__svc_url("example.org", "dods/file.nc.html"),
        qr_test__svc_url("example.org", "file.nc", "HTTPServer")
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

    aggs <- query_result_test_object("Aggregation", docs)
    expect_identical(aggs$fields, c(names(docs), "url_opendap", "url_download"))
    expect_identical(names(aggs$to_data_table(aggs$fields)), aggs$fields)

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

    no_acc <- query_result_test_object(
        "Dataset",
        query_result_test_dataset_docs(FALSE)
    )
    expect_identical(no_acc$has_opendap(), c(FALSE, FALSE))
    expect_identical(no_acc$has_download(), c(FALSE, FALSE))
})
# }}}

# EsgResultDataset {{{
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
    u_http <- qr_test__svc_url("example.org", "file.nc", "HTTPServer")
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            ...
        ) {
            ds_id <- query_param__value(params$state()$dataset_id)
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                limit = limit,
                all = all,
                dataset_id = ds_id
            )
            docs <- qr_test__file_docs_rows(
                rep(u_http, length(ds_id)),
                id = paste0("file-", ds_id),
                dataset_id = ds_id,
                size = seq_along(ds_id)
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
    p1 <- calls[[1L]]$params
    expect_equal(calls[[1L]]$limit, this$data_max_limit)
    expect_false(query_param__value(p1$latest()))
    expect_false(query_param__value(p1$distrib()))
    expect_identical(query_param__value(p1$data_node()), "example.org")
    expect_identical(
        p1$render(c("datetime_start", "datetime_stop")),
        character()
    )
    expect_null(p1$project())
    expect_null(p1$source_id())

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

    for (args in list(
        list(source_id = "AWI-CM-1-1-MR"),
        list(bbox = "0,0,1,1"),
        list(start = "2050")
    )) {
        expect_error(do.call(datasets$collect, args), "unsupported parameter")
    }
    for (args in list(list(datetime_start = "2050"), list(time = "all"))) {
        expect_error(do.call(datasets$collect, args), "controlled")
    }
})

test_that("EsgResultDataset$collect() batches large and long-ID queries, enforces global limits, and forwards progress", {
    ds_id <- sprintf(
        "dataset-%03d",
        seq_len(QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE * 2L + 5L)
    )
    datasets <- query_result_test_object(
        "Dataset",
        data.frame(id = ds_id, size = seq_along(ds_id))
    )
    callback <- function(event) invisible(event)
    priv(datasets)$progress_callback <- callback

    calls <- list()
    u_http <- qr_test__svc_url("example.org", "file.nc", "HTTPServer")
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
            plim <- query_param__value(params$limit())
            take <- if (isTRUE(all)) {
                length(batch)
            } else {
                min(length(batch), as.integer(plim))
            }
            calls[[length(calls) + 1L]] <<- list(
                ids = batch,
                limit = limit,
                param_limit = plim,
                dict_check = dict_check,
                progress = progress,
                progress_label = progress_label,
                callback = progress_callback
            )
            docs <- qr_test__file_docs_rows(
                rep(u_http, take),
                id = paste0("file-", batch[seq_len(take)]),
                dataset_id = batch[seq_len(take)],
                size = seq_len(take)
            )
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
    expect_equal(files$count(), length(ds_id))
    expect_length(priv(files)$context$query_url, 3L)

    calls <- list()
    limited <- datasets$collect(fields = "id", limit = 55L)
    expect_length(calls, 2L)
    expect_equal(vapply(calls, `[[`, integer(1L), "limit"), c(55L, 5L))
    expect_equal(limited$count(), 55L)

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
    ds <- query_result_test_object("Dataset", docs)
    testthat::local_mocked_bindings(
        query__collect = qr_test__mock_collect(
            function(index_node, params, ...) {
                out <- docs[c(1L, 1L), , drop = FALSE]
                if (!is.null(params$params()$instance_id)) {
                    out$data_node <- c(
                        "node-a.example.org",
                        "node-b.example.org"
                    )
                } else {
                    out$version <- c(20260101L, 20270101L)
                }
                out
            },
            url = function(index_node, params) {
                paste0(index_node, "/dataset-replicas")
            }
        ),
        .package = "epwshiftr"
    )
    expect_identical(
        ds$expand_replicas(index_node = "replica-index.example.org")$data_node,
        c("node-a.example.org", "node-b.example.org")
    )
    expect_identical(
        ds$expand_replicas(
            by = "master_id",
            index_node = "replica-index.example.org"
        )$version,
        c(20260101L, 20270101L)
    )
})
# }}}

# EsgResultFile URLs, service preflight checks, and download_plan() {{{
test_that("EsgResultFile/Aggregation url_opendap & url_download preserve length and warn with context", {
    docs <- qr_test__file_docs_rows(
        list(
            character(),
            "malformed",
            qr_test__svc_url("example.org", "file-3.nc", "HTTPServer"),
            c(
                qr_test__svc_url("example.org", "dods/file-4.html"),
                qr_test__svc_url("example.org", "dods/file-4-replica.html"),
                qr_test__svc_url("example.org", "file-4.nc", "HTTPServer")
            )
        ),
        id = paste0("file-", 1:4)
    )
    for (type in c("File", "Aggregation")) {
        obj <- query_result_test_object(type, docs)
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
        qr_test__svc_url("example.org", "dods/file-1.html"),
        qr_test__svc_url("example.org", "dods/file-1-replica.html")
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
    idx <- rep(1:6, each = 2L)
    nodes <- rep(c("bad.example.org", "good.example.org"), 6L)
    docs <- qr_test__file_docs_rows(
        Map(
            function(i, node) {
                c(
                    qr_test__svc_url(node, sprintf("dods/file-%d.nc", i)),
                    qr_test__svc_url(
                        node,
                        sprintf("files/file-%d.nc", i),
                        "HTTPServer"
                    )
                )
            },
            idx,
            nodes
        ),
        id = sprintf("file-%d-%s", idx, nodes),
        instance_id = sprintf("instance-%d", idx),
        master_id = sprintf("master-%d", idx),
        tracking_id = sprintf("tracking-%d", idx),
        checksum = sprintf("checksum-%d", idx),
        title = sprintf("file-%d.nc", idx),
        size = 100 + idx,
        variable_id = sprintf("var-%d", idx),
        data_node = nodes,
        replica = rep(c(FALSE, TRUE), 6L)
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

    hosts <- c(rep("healthy.example.org", 2L), rep("partial.example.org", 2L))
    host_docs <- qr_test__file_docs_rows(
        qr_test__svc_url(hosts, paste0("f", 1:4, ".nc")),
        id = paste0("id", 1:4),
        data_node = NA_character_,
        variable_id = NA_character_
    )
    testthat::local_mocked_bindings(
        query_result__reach_service_urls = qr_test__mock_reach_urls(
            function(urls, s) grepl("healthy|f3", urls),
            latency_ms = 1
        ),
        .package = "epwshiftr"
    )
    host_files <- query_result_test_object("File", host_docs)
    expect_identical(
        query_result__sample_service_nodes(
            host_files,
            sample_per_node = 2L
        )$reachable,
        c(TRUE, TRUE, FALSE, FALSE)
    )
    expect_error(
        query_result__sample_service_nodes(host_files, sample_per_node = 0L),
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

test_that("EsgResultFile$download_plan() builds logical IDs, deduplicates checks, reuses cache, and ranks nodes", {
    files <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            qr_test__svc_url("example.org", "dods/file.nc.html"),
            qr_test__svc_url("example.org", "file.nc", "HTTPServer")
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

    chk_docs <- query_result_test_file_docs()
    chk_docs$master_id <- chk_docs$tracking_id <- NA_character_
    expect_identical(
        query_result_test_object("File", chk_docs)$download_plan(
            replica = "current",
            probe = FALSE
        )$logical_file_id,
        "checksum:abc:1:file.nc"
    )

    dup_docs <- qr_test__file_docs_rows(
        rep(qr_test__svc_url("same.example.org", "file.nc", "HTTPServer"), 2L),
        id = c("file-1", "file-2"),
        data_node = "same.example.org",
        master_id = c("m1", "m2")
    )
    calls <- 0L
    testthat::local_mocked_bindings(
        query_result__latency_url = function(...) {
            calls <<- calls + 1L
            list(latency = 0.5, throughput = NA_real_)
        },
        .package = "epwshiftr"
    )
    checked <- query_result_test_object("File", dup_docs)$download_plan(
        replica = "current",
        probe = TRUE,
        strategy = "first",
        probe_concurrency = 1L,
        probe_cache_seconds = 0L
    )
    expect_equal(calls, 1L)
    expect_equal(checked$probe_latency, c(0.5, 0.5))

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
    cached <- query_result_test_object("File", dup_docs[1L, ])$download_plan(
        replica = "current",
        probe = TRUE,
        node_stats = cached_stats,
        probe_cache_seconds = 3600L
    )
    expect_equal(calls, 1L)
    expect_true(cached$probe_cached)
    expect_equal(cached$probe_latency, 0.25)

    nodes <- c("slow.example.org", "fast.example.org")
    rep_files <- query_result_test_object(
        "File",
        qr_test__file_docs_rows(
            qr_test__svc_url(nodes, "file.nc", "HTTPServer"),
            id = c("file-a", "file-b"),
            data_node = nodes
        )
    )
    hist_stats <- data.table::data.table(
        data_node = nodes,
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
    expect_identical(ranked$data_node, rev(nodes))

    cool_stats <- data.table::copy(hist_stats)
    data.table::set(
        cool_stats,
        j = c(
            "probe_success_count",
            "probe_failure_count",
            "cooldown_until",
            "updated_at"
        ),
        value = list(
            0L,
            c(0L, 3L),
            c(as.POSIXct(NA), Sys.time() + 3600),
            Sys.time()
        )
    )
    cooled <- rep_files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = cool_stats
    )
    expect_identical(cooled$data_node, nodes)
    expect_equal(cooled$node_cooldown_rank, c(0L, 1L))
    data.table::set(cool_stats, j = "cooldown_until", value = Sys.time() + 3600)
    all_cool <- rep_files$download_plan(
        replica = "current",
        probe = FALSE,
        strategy = "fastest",
        node_stats = cool_stats
    )
    expect_true(all(all_cool$node_cooldown_rank == 0L))
})
# }}}

# EsgResultFile$open_dataset() / EsgResultAggregation$open_dataset() {{{
test_that("open_dataset() validates fallbacks, falls back to HTTP, repairs URLs, and reports progress", {
    testthat::local_mocked_bindings(menu = function(...) 2L, .package = "utils")

    http_only <- query_result_test_object("File", query_result_test_file_docs())
    for (spec in list(
        list("error", "OPeNDAP is not available"),
        list("ask", "non-interactive|Operation cancelled"),
        list("auto", "explicit `store` or `downloader`")
    )) {
        expect_error(http_only$open_dataset(fallback = spec[[1L]]), spec[[2L]])
    }
    empty_url_docs <- query_result_test_file_docs(character())
    expect_error(
        query_result_test_object("File", empty_url_docs)$open_dataset(
            fallback = "auto"
        ),
        "HTTPServer download URL"
    )
    expect_error(
        query_result_test_object("Aggregation", empty_url_docs)$open_dataset(
            fallback = "auto"
        ),
        "record 1 \\(id: file-1\\)"
    )

    calls <- new.env(parent = emptyenv())
    calls$opened <- calls$closed <- list()
    calls$downloads <- character()
    calls$open_progress <- logical()
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
        EsgDataset = qr_test__fake_dataset_class(calls),
        Downloader = FakeDownloader,
        query_result__repair_urls = function(result, ...) result,
        .package = "epwshiftr"
    )

    file_res <- query_result_test_object(
        "File",
        query_result_test_file_docs(c(
            qr_test__svc_url("example.org", "fail-opendap.nc.html"),
            qr_test__svc_url("example.org", "file.nc", "HTTPServer")
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

    agg_docs <- qr_test__file_docs_rows(
        list(
            c(
                qr_test__svc_url("example.org", "dods/file-1.nc.html"),
                qr_test__svc_url("example.org", "file-1.nc", "HTTPServer")
            ),
            c(
                qr_test__svc_url("example.org", "fail-opendap-file-2.nc.html"),
                qr_test__svc_url("example.org", "file-2.nc", "HTTPServer")
            )
        ),
        id = c("file-1", "file-2")
    )
    agg_res <- query_result_test_object("Aggregation", agg_docs)
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
    rep_docs <- query_result_test_file_docs(
        qr_test__svc_url("replica.example.org", "file.nc", "HTTPServer")
    )
    testthat::local_mocked_bindings(
        query_result__repair_urls = function(result, ...) {
            query_result_test_object("File", rep_docs)
        },
        .package = "epwshiftr"
    )
    rep_ds <- query_result_test_object("File", empty_url_docs)$open_dataset(
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
    ok_docs <- qr_test__file_docs_rows(
        qr_test__svc_url("example.org", paste0("dods/file-", 1:2, ".nc.html")),
        id = c("file-1", "file-2")
    )
    query_result_test_object("File", ok_docs)$open_dataset(
        fallback = "auto",
        progress = TRUE
    )
    query_result_test_object("File", ok_docs)$open_dataset(
        fallback = "auto",
        progress = FALSE
    )
    query_result_test_object("Aggregation", ok_docs)$open_dataset(
        fallback = "auto",
        progress = TRUE
    )
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

# Query value batching {{{
test_that("query_result__query_value_batches measures rendered URL boundaries", {
    burl <- function(x) {
        paste0("https://example.org/?id=", paste(x, collapse = ","))
    }
    lim <- nchar(burl(c("a", "b")), type = "bytes")
    batch <- function(...) {
        unname(query_result__query_value_batches(..., build_url = burl))
    }
    expect_identical(
        query_result__query_value_batches(character(), burl),
        list()
    )
    expect_identical(batch(c("a", "b"), max_url_bytes = lim), list(c("a", "b")))
    expect_identical(
        batch(c("a", "b"), max_url_bytes = lim - 1L),
        list("a", "b")
    )
    expect_identical(
        batch(c("a", "b", "a"), batch_size = 2L),
        list(c("a", "b"), "a")
    )
    expect_identical(batch("long", max_url_bytes = 1L), list("long"))
})
# }}}

# Live ESGF service resolution and HTTP recovery {{{
# Query one current daily-temperature File set from the first responsive ESGF
# index node, retaining node failures so a live-run error remains actionable.
# live_esgf_tas_files {{{
live_esgf_tas_files <- function() {
    nodes <- unique(c(INDEX_NODES[["DKRZ"]], INDEX_NODES[["ORNL"]]))
    failures <- character()
    for (node in nodes) {
        files <- tryCatch(
            {
                query <- esg_query(node)$activity_id("ScenarioMIP")$source_id(
                    "MPI-ESM1-2-LR"
                )$experiment_id("ssp585")$variant_label("r1i1p1f1")$frequency(
                    "day"
                )$variable_id("tas")$params(table_id = "day")$limit(20L)
                allow_live_esgf_dict_warnings(query$collect(
                    type = "File",
                    fields = "*",
                    all = FALSE,
                    limit = TRUE
                ))
            },
            error = identity
        )
        if (!inherits(files, "error") && files$count() > 0L) {
            return(list(node = node, files = files))
        }
        reason <- if (inherits(files, "error")) {
            conditionMessage(files)
        } else {
            "no matching File records"
        }
        failures <- c(failures, sprintf("%s: %s", node, reason))
    }
    stop(
        paste(
            "No configured ESGF index node returned the live tas fixture.",
            paste(failures, collapse = " | ")
        ),
        call. = FALSE
    )
}
# }}}

# Find one monthly temperature file small enough to exercise a real HTTP
# recovery without turning the opt-in service check into a large data job.
# live_esgf_bounded_http_file {{{
live_esgf_bounded_http_file <- function(max_bytes = 64 * 1024^2) {
    nodes <- unique(c(INDEX_NODES[["DKRZ"]], INDEX_NODES[["ORNL"]]))
    failures <- character()
    for (node in nodes) {
        files <- tryCatch(
            {
                query <- esg_query(node)$activity_id("ScenarioMIP")$source_id(
                    "MPI-ESM1-2-LR"
                )$experiment_id("ssp585")$variant_label("r1i1p1f1")$frequency(
                    "mon"
                )$variable_id("tas")$params(table_id = "Amon")$limit(100L)
                allow_live_esgf_dict_warnings(query$collect(
                    type = "File",
                    fields = "*",
                    all = FALSE,
                    limit = TRUE
                ))
            },
            error = identity
        )
        if (inherits(files, "error")) {
            failures <- c(
                failures,
                sprintf("%s: %s", node, conditionMessage(files))
            )
            next
        }
        if (!files$count()) {
            failures <- c(
                failures,
                sprintf("%s: no matching File records", node)
            )
            next
        }
        data <- files$to_data_table()
        size <- suppressWarnings(as.numeric(data[["size"]]))
        has_http <- !is.na(data[["url_download"]]) &
            nzchar(data[["url_download"]])
        eligible <- which(
            is.finite(size) & size > 0 & size <= max_bytes & has_http
        )
        if (length(eligible)) {
            chosen <- eligible[[order(size[eligible], eligible)[[1L]]]]
            return(list(
                node = node,
                files = files$slice(chosen),
                size = size[[chosen]],
                max_bytes = max_bytes
            ))
        }
        failures <- c(
            failures,
            sprintf(
                "%s: no HTTPServer file at or below %.1f MiB",
                node,
                max_bytes / 1024^2
            )
        )
    }
    testthat::skip(paste(
        "No bounded live ESGF HTTP recovery file is currently available.",
        paste(failures, collapse = " | ")
    ))
}
# }}}

# Keep retries on the same pinned File selection: an index response alone does
# not establish that one of its OPeNDAP services is currently readable.
live_esgf__resolve_tas <- function(
    files,
    index_node,
    attempts = 3L,
    retry_delay = 1
) {
    checkmate::assert_count(attempts, positive = TRUE)
    checkmate::assert_number(retry_delay, lower = 0, finite = TRUE)
    observations <- vector("list", attempts)
    for (attempt in seq_len(attempts)) {
        resolved <- query_result__resolve_file_services(
            files,
            index_node = index_node,
            check = list(
                level = "url",
                timeout = 30,
                concurrency = 2L,
                cache_seconds = 0L,
                cache_failures_seconds = 0L
            )
        )
        observations[[attempt]] <- as.data.frame(resolved$diagnostics)
        if (
            any(
                resolved$diagnostics$service == "OPENDAP" &
                    resolved$diagnostics$selected
            )
        ) {
            return(resolved)
        }
        # Retry only this published selection; do not change the model, dates,
        # scientific coverage assertion, endpoint checks or real value read.
        if (attempt < attempts) Sys.sleep(retry_delay * attempt)
    }
    stop(
        "No reachable OPeNDAP service after ",
        attempts,
        " attempts on the same File selection. Diagnostics: ",
        as.character(jsonlite::toJSON(
            observations,
            auto_unbox = TRUE,
            na = "null"
        )),
        call. = FALSE
    )
}

# A temporary failure must be retried without changing File identity, while
# permanent failure remains a hard error with every failed endpoint retained.
test_that("live ESGF service retry retains the pinned selection and finite failure", {
    pinned <- new.env(parent = emptyenv())
    seen <- list()
    calls <- 0L
    local_mocked_bindings(query_result__resolve_file_services = function(
        result,
        index_node,
        check
    ) {
        calls <<- calls + 1L
        seen[[calls]] <<- result
        list(
            diagnostics = data.table::data.table(
                service = "OPENDAP",
                selected = calls > 1L,
                url = "https://fixture.example/file.nc",
                reachable = calls > 1L,
                error = if (calls > 1L) NA_character_ else "HTTP 503"
            )
        )
    })
    result <- live_esgf__resolve_tas(
        pinned,
        "https://index.example",
        retry_delay = 0
    )
    expect_true(any(result$diagnostics$selected))
    expect_equal(calls, 2L)
    expect_true(all(vapply(seen, identical, logical(1L), pinned)))
    calls <- 0L
    local_mocked_bindings(query_result__resolve_file_services = function(
        result,
        index_node,
        check
    ) {
        calls <<- calls + 1L
        list(
            diagnostics = data.table::data.table(
                service = "OPENDAP",
                selected = FALSE,
                url = "https://fixture.example/unavailable.nc",
                reachable = FALSE,
                error = "HTTP 503"
            )
        )
    })
    expect_error(
        live_esgf__resolve_tas(
            pinned,
            "https://index.example",
            retry_delay = 0
        ),
        "3.*OPeNDAP|OPeNDAP.*3"
    )
    expect_equal(calls, 3L)
})

test_that("live ESGF File coverage, service resolution, and one-cell read", {
    skip_live_esgf()

    live <- live_esgf_tas_files()
    files <- live$files$filter_time(
        "2050-01-01T00:00:00Z",
        "2050-12-31T23:59:59Z",
        method = "drs"
    )
    expect_gt(files$count(), 0L)

    # Apply the same File-year kernel used by batch and single-model workflows
    # before touching any data endpoint.
    candidates <- shift_resolve__cmip6_candidates(
        files$to_data_table(),
        models = "MPI-ESM1-2-LR",
        experiments = "ssp585",
        variables = "tas",
        years = 2050L,
        frequency = "day",
        table = "day",
        requirements = list(tas = list("tas")),
        grid = "gn"
    )
    expect_true(any(candidates$complete))

    resolved <- live_esgf__resolve_tas(files, live$node)
    expect_equal(resolved$result$count(), 1L)
    expect_true(any(
        resolved$diagnostics$service == "OPENDAP" &
            resolved$diagnostics$selected
    ))
    expect_true(any(
        resolved$diagnostics$service == "HTTPServer" &
            resolved$diagnostics$selected
    ))

    opendap <- resolved$result$url_opendap[[1L]]
    dds <- query_result__check_opendap_url(opendap, timeout = 30)
    expect_true(dds$reachable, info = dds$error)

    dataset <- EsgDataset$new(opendap)
    dataset$open()
    on.exit(dataset$close(), add = TRUE)
    value <- dataset$var_get(
        "tas",
        start = c(1L, 1L, 1L),
        count = c(1L, 1L, 1L),
        collapse = TRUE
    )
    expect_type(value, "double")
    expect_length(value, 1L)
    expect_true(is.finite(value))
})

test_that("live ESGF HTTP recovery downloads one bounded real file", {
    skip_live_esgf()

    live <- live_esgf_bounded_http_file()
    expect_lte(live$size, live$max_bytes)

    # Keep the provider's real HTTPServer URL but use a deterministic local
    # refusal for OPeNDAP so this test does not depend on a public outage.
    docs <- priv(live$files)$get_docs()
    docs$url[[1L]] <- query_result__set_service_url(
        docs$url[[1L]],
        "OPENDAP",
        "http://127.0.0.1:9/epwshiftr-live-esgf.nc"
    )
    forced <- priv(live$files)$result_with_docs(docs)

    store <- EsgStore$new(withr::local_tempdir())
    withr::defer(store$close())
    dataset <- forced$open_dataset(
        fallback = "auto",
        store = store,
        progress = FALSE
    )
    withr::defer(dataset$close())

    local <- dataset$reachable(level = "url")
    expect_identical(local$service, "local")
    expect_true(local$reachable)
    expect_true(file.exists(local$url))
    expect_lte(unname(file.info(local$url)$size), live$max_bytes)
    value <- dataset$var_get(
        "tas",
        start = c(1L, 1L, 1L),
        count = c(1L, 1L, 1L),
        collapse = TRUE
    )
    expect_type(value, "double")
    expect_length(value, 1L)
    expect_true(is.finite(value))
})
# }}}

# }}}
# vim: fdm=marker :
