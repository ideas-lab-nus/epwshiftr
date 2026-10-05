# Decode only query fields needed by the fake transport; responses still pass
# through the real collector, pagination and result merger.
# query_uri__param {{{
query_uri__param <- function(url, name) {
    query <- sub("^[^?]*\\?", "", url)
    parts <- strsplit(query, "&", fixed = TRUE)[[1L]]
    value <- sub(
        paste0("^", name, "="),
        "",
        parts[startsWith(parts, paste0(name, "="))]
    )
    if (!length(value)) {
        return(character())
    }
    strsplit(utils::URLdecode(value), ",", fixed = TRUE)[[1L]]
}
# }}}

# Return a minimal Solr page whose IDs make restart duplicates observable.
# query_uri__response {{{
query_uri__response <- function(ids, total = length(ids)) {
    list(
        response = list(
            numFound = as.integer(total),
            start = 0L,
            docs = data.frame(id = ids, stringsAsFactors = FALSE)
        )
    )
}
# }}}

# Model libcurl's typed status at the JSON transport boundary.
# query_uri__reject {{{
query_uri__reject <- function(url) {
    stop(errorCondition(
        "HTTP 414",
        class = "epwshiftr_query_uri_too_long",
        status_code = 414L,
        url = url
    ))
}
# }}}

test_that("complete URLs include normalized fields and fixed parameter overhead", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        query_uri__response(query_uri__param(url, "dataset_id"))
    })
    ids <- paste0(strrep("x", 450L), 1:3)
    store <- QueryParamStore$new()$params(dataset_id = ids)$fields("id")$limit(
        10L
    )
    required <- paste0("field", seq_len(70L))
    out <- query_result__collect_batched(
        "https://example.org",
        store,
        "dataset_id",
        required_fields = required,
        all = TRUE,
        constraints = FALSE
    )
    expect_gt(length(seen), 1L)
    expect_true(all(
        nchar(seen, type = "bytes") <= QUERY_RESULT_COLLECT_MAX_URL_BYTES
    ))
    expect_true(all(vapply(
        seen,
        function(url) {
            all(required %in% query_uri__param(url, "fields"))
        },
        logical(1L)
    )))
    expect_identical(out$docs$id, ids)
    expect_identical(query_param__value(store$fields()), "id")
    expect_identical(query_param__value(out$parameter$state()$dataset_id), ids)
})

test_that("an oversized singleton is sent and may succeed", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        query_uri__response(query_uri__param(url, "dataset_id"))
    })
    ids <- c(strrep("a", 2500L), "b")
    store <- QueryParamStore$new()$params(dataset_id = ids)$limit(10L)
    out <- query_result__collect_batched(
        "https://example.org",
        store,
        "dataset_id"
    )
    expect_length(seen, 2L)
    expect_gt(
        nchar(seen[[1L]], type = "bytes"),
        QUERY_RESULT_COLLECT_MAX_URL_BYTES
    )
    expect_identical(out$docs$id, ids)
})

test_that("414 retries halve all three identity facets and preserve limits", {
    for (facet in c("dataset_id", "instance_id", "master_id")) {
        seen <- list()
        local_mocked_bindings(cache__read_json = function(url, ...) {
            ids <- query_uri__param(url, facet)
            seen[[length(seen) + 1L]] <<- list(
                url = url,
                ids = ids,
                limit = as.integer(query_uri__param(url, "limit"))
            )
            if (length(ids) > 2L) {
                query_uri__reject(url)
            }
            query_uri__response(
                head(ids, as.integer(query_uri__param(url, "limit"))),
                length(ids)
            )
        })
        store <- QueryParamStore$new()$limit(5L)
        query_result__merge_params(
            store,
            stats::setNames(list(letters[1:8]), facet)
        )
        out <- query_result__collect_batched(
            "https://example.org",
            store,
            facet
        )
        expect_identical(out$docs$id, letters[1:5])
        expect_equal(
            vapply(seen, function(x) length(x$ids), integer(1L)),
            c(8L, 4L, 2L, 2L, 4L, 2L)
        )
        expect_equal(seen[[6L]]$limit, 1L)
        expect_identical(out$context$query_url, vapply(seen, `[[`, "", "url"))
        expect_identical(
            query_param__value(out$parameter$state()[[facet]]),
            letters[1:8]
        )
        expect_identical(query_param__value(out$parameter$limit()), 5L)
    }
})

test_that("414 during pagination discards partial pages before restarting", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        ids <- query_uri__param(url, "dataset_id")
        offset <- as.integer(query_uri__param(url, "offset"))
        if (length(ids) > 1L && offset > 0L) {
            query_uri__reject(url)
        }
        docs <- paste0(rep(ids, each = 2L), rep(1:2, length(ids)))
        query_uri__response(docs[offset + 1L], length(docs))
    })
    store <- QueryParamStore$new()$params(dataset_id = c("a", "b"))$limit(1L)
    out <- query_result__collect_batched(
        "https://example.org",
        store,
        "dataset_id",
        all = TRUE,
        limit = 1L
    )
    expect_identical(out$docs$id, c("a1", "a2", "b1", "b2"))
    expect_length(seen, 6L)
    expect_identical(out$context$query_url, seen)
    expect_identical(query_param__value(out$parameter$offset()), 0L)
    expect_equal(out$response$response$numFound, 4L)
})

test_that("only a rejected singleton ends 414 splitting", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        query_uri__reject(url)
    })
    store <- QueryParamStore$new()$params(dataset_id = c("a", "b"))
    expect_error(
        query_result__collect_batched(
            "https://example.org",
            store,
            "dataset_id"
        ),
        "single-ID request.*HTTP 414",
        class = "epwshiftr_query_uri_too_long"
    )
    expect_length(seen, 2L)
    expect_identical(query_uri__param(seen[[2L]], "dataset_id"), "a")
})

test_that("unrelated failures do not trigger splitting", {
    count <- 0L
    local_mocked_bindings(cache__read_json = function(url, ...) {
        count <<- count + 1L
        stop("HTTP 503")
    })
    store <- QueryParamStore$new()$params(dataset_id = c("a", "b"))
    expect_error(
        query_result__collect_batched(
            "https://example.org",
            store,
            "dataset_id"
        ),
        "HTTP 503"
    )
    expect_identical(count, 1L)
})

test_that("duplicates crossing count boundaries do not duplicate results", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        query_uri__response(query_uri__param(url, "dataset_id"))
    })
    ids <- c(paste0("id", seq_len(51L)), "id1")
    store <- QueryParamStore$new()$params(dataset_id = ids)
    out <- query_result__collect_batched(
        "https://example.org",
        store,
        "dataset_id",
        all = TRUE
    )
    expect_length(seen, 2L)
    expect_identical(out$docs$id, unique(ids))
    expect_identical(query_param__value(out$parameter$state()$dataset_id), ids)
})

test_that("explicit offsets and negated facets are not split", {
    seen <- character()
    local_mocked_bindings(cache__read_json = function(url, ...) {
        seen <<- c(seen, url)
        query_uri__response(character())
    })
    ids <- paste0(strrep("x", 1000L), 1:3)
    store <- QueryParamStore$new()$params(dataset_id = ids)$offset(2L)
    query_result__collect_batched("https://example.org", store, "dataset_id")
    expect_length(seen, 1L)
    expect_identical(query_uri__param(seen[[1L]], "offset"), "2")
    store$offset(0L)
    state <- store$state()
    state$dataset_id <- QueryParamFacet(ids, negate = TRUE)
    store$restore(state)
    query_result__collect_batched("https://example.org", store, "dataset_id")
    expect_length(seen, 2L)
})

test_that("curl HTTP status survives JSON error wrapping without message matching", {
    status <- 414L
    local_mocked_bindings(
        curl_fetch_memory = function(...) stop("localized transport message"),
        handle_data = function(...) list(status_code = status),
        .package = "curl"
    )
    url <- "https://example.org/esg-search/search?dataset_id=a"
    error <- tryCatch(cache__read_json(url, cache = FALSE), error = identity)
    expect_s3_class(error, "epwshiftr_query_uri_too_long")
    expect_identical(error$url, url)
    expect_identical(error$status_code, 414L)
    status <- 503L
    error <- tryCatch(cache__read_json(url, cache = FALSE), error = identity)
    expect_false(inherits(error, "epwshiftr_query_uri_too_long"))
    expect_match(conditionMessage(error), "localized transport message")
    status <- 0L
    expect_error(
        cache__read_json(url, cache = FALSE),
        "localized transport message"
    )
})

# Isolate the HTTP fixture from the loaded package so the child process serves
# real 414 responses without serializing test or instrumentation environments.
# query_uri__http_app {{{
query_uri__http_app <- function() {
    app <- webfakes::new_app()
    app$get("/esg-search/search", function(req, res) {
        ids <- strsplit(req$query$dataset_id, ",", fixed = TRUE)[[1L]]
        if (length(ids) > 1L || identical(ids, "reject")) {
            return(res$set_status(414L)$send("URI rejected"))
        }
        res$set_header("Content-Type", "application/json")$send(
            jsonlite::toJSON(
                list(
                    response = list(
                        numFound = 1L,
                        start = 0L,
                        docs = list(list(id = ids[[1L]]))
                    )
                ),
                auto_unbox = TRUE
            )
        )
    })
    app
}
# }}}
environment(query_uri__http_app) <- baseenv()

test_that("real HTTP 414 responses split and singleton URLs reach the server", {
    skip_if_not_installed("webfakes")
    withr::local_options(epwshiftr.cache = FALSE)
    server <- webfakes::new_app_process(query_uri__http_app())
    withr::defer(server$stop())
    node <- sub("/$", "", server$url())
    store <- QueryParamStore$new()$params(dataset_id = c("a", "b"))
    out <- query_result__collect_batched(node, store, "dataset_id", all = TRUE)
    expect_identical(out$docs$id, c("a", "b"))
    expect_length(out$context$query_url, 3L)
    id <- strrep("x", 1900L)
    store$params(dataset_id = id)
    out <- query_result__collect_batched(node, store, "dataset_id")
    expect_identical(out$docs$id, id)
    expect_gt(
        nchar(out$context$query_url, type = "bytes"),
        QUERY_RESULT_COLLECT_MAX_URL_BYTES
    )
    store$params(dataset_id = "reject")
    expect_error(
        query_result__collect_batched(node, store, "dataset_id"),
        "single-ID request",
        class = "epwshiftr_query_uri_too_long"
    )
})
