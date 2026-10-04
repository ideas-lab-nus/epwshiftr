# cli_shift_test_mock_collect {{{
cli_shift_test_mock_collect <- function(
    file_docs,
    calls = new.env(parent = emptyenv())
) {
    calls$types <- character()
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
            docs <- if (identical(type, "Dataset")) {
                esgf_test__dataset_docs(
                    unique(file_docs$variable_id),
                    unique(file_docs$frequency)
                )
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            calls$types <- c(calls$types, type)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
    calls
}
# }}}

# cli_shift_test_config {{{
cli_shift_test_config <- function(path, store = NULL, epw = get_cache_epw()) {
    config <- list(
        version = 3L,
        sites = list(list(id = "Singapore", epw = epw)),
        climate = list(
            provider = "cmip6",
            model = "EC-Earth3",
            scenarios = "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = list(
            scale = "monthly",
            method = "epwshiftr"
        ),
        reference = NULL,
        dir = tempfile("cli-shift-export-"),
        control = list(
            strict = FALSE,
            allow_partial = FALSE,
            download = "auto",
            resume = TRUE,
            overwrite = TRUE,
            output_layout = "flat"
        )
    )
    jsonlite::write_json(
        config,
        path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
    )
    invisible(path)
}
# }}}

# cli_shift_test_store_with_query {{{
cli_shift_test_store_with_query <- function(nc) {
    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    docs <- esgf_test__file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(esgf_test__file_result(docs))
    store$close()
    list(dir = dir, query_id = query_id)
}
# }}}

# cli_shift_test_store_with_extract {{{
cli_shift_test_store_with_extract <- function(nc) {
    setup <- cli_shift_test_store_with_query(nc)
    plan <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        setup$dir,
        "extract",
        "plan",
        "--query",
        setup$query_id,
        "--site-id",
        "SIN",
        "--lon",
        "103.98",
        "--lat",
        "1.37",
        "--time",
        "2060-01-01T00:00:00Z,2060-12-31T23:59:59Z",
        "--variable",
        "tas"
    ))
    run <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        setup$dir,
        "extract",
        "run",
        "--plan",
        paste(plan$result$plan_id, collapse = ",")
    ))
    testthat::expect_equal(plan$status, 0L)
    testthat::expect_equal(run$status, 0L)
    testthat::expect_true(all(run$result$status == "done"))
    c(setup, list(plan_id = plan$result$plan_id))
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
