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

# vim: fdm=marker :
