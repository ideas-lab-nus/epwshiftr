# Query one current daily-temperature File set from the first responsive ESGF
# index node, retaining node failures so a live-run error remains actionable.
live_esgf_tas_files <- function() {
    nodes <- unique(c(INDEX_NODES[["DKRZ"]], INDEX_NODES[["ORNL"]]))
    failures <- character()
    for (node in nodes) {
        files <- tryCatch({
            query <- esg_query(node)$
                activity_id("ScenarioMIP")$
                source_id("MPI-ESM1-2-LR")$
                experiment_id("ssp585")$
                variant_label("r1i1p1f1")$
                frequency("day")$
                variable_id("tas")$
                params(table_id = "day")$
                limit(20L)
            allow_live_esgf_dict_warnings(query$collect(
                type = "File",
                fields = "*",
                all = FALSE,
                limit = TRUE
            ))
        }, error = identity)
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

# Find one monthly temperature file small enough to exercise a real HTTP
# recovery without turning the opt-in service check into a large data job.
live_esgf_bounded_http_file <- function(max_bytes = 64 * 1024^2) {
    nodes <- unique(c(INDEX_NODES[["DKRZ"]], INDEX_NODES[["ORNL"]]))
    failures <- character()
    for (node in nodes) {
        files <- tryCatch({
            query <- esg_query(node)$
                activity_id("ScenarioMIP")$
                source_id("MPI-ESM1-2-LR")$
                experiment_id("ssp585")$
                variant_label("r1i1p1f1")$
                frequency("mon")$
                variable_id("tas")$
                params(table_id = "Amon")$
                limit(100L)
            allow_live_esgf_dict_warnings(query$collect(
                type = "File",
                fields = "*",
                all = FALSE,
                limit = TRUE
            ))
        }, error = identity)
        if (inherits(files, "error")) {
            failures <- c(
                failures,
                sprintf("%s: %s", node, conditionMessage(files))
            )
            next
        }
        if (!files$count()) {
            failures <- c(failures, sprintf("%s: no matching File records", node))
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
    candidates <- shift__cmip6_candidates(
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

    resolved <- query_result__resolve_file_services(
        files,
        index_node = live$node,
        check = list(
            level = "url",
            timeout = 30,
            concurrency = 2L,
            cache_seconds = 0L,
            cache_failures_seconds = 0L
        )
    )
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
    skip_if_not_installed("RNetCDF")

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
