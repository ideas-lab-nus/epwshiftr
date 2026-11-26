# Thin alias kept while test-shift-discovery-ui.R still references this name.
# cli_shift_test_mock_collect {{{
cli_shift_test_mock_collect <- function(
    file_docs,
    calls = new.env(parent = emptyenv())
) {
    shift_test__mock_collect(file_docs, calls = calls, env = parent.frame())
}
# }}}

# cli_run {{{
cli_run <- function(store, ..., quiet = TRUE) {
    args <- c(
        if (isTRUE(quiet)) "--quiet",
        if (!is.null(store)) c("--store", store),
        c(...)
    )
    epwshiftr_cli(args)
}
# }}}

# cli_test_store {{{
cli_test_store <- function(label = "cli query", track = TRUE) {
    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    query_id <- store$add_query(
        esg_query("https://example.org")$experiment_id("ssp585")$variable_id(
            "tas"
        )$limit(1L),
        label = label,
        track = track
    )
    list(dir = dir, store = store, query_id = query_id)
}
# }}}

# cli_test_mock_query_collect {{{
cli_test_mock_query_collect <- function(file_docs, env = parent.frame()) {
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
            response <- esgf_test__response(file_docs)
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
        .package = "epwshiftr",
        .env = env
    )
}
# }}}

# cli_shift_test_write_config {{{
cli_shift_test_write_config <- function(
    config,
    path = tempfile(fileext = ".json")
) {
    jsonlite::write_json(
        config,
        path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
    )
    path
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
    invisible(cli_shift_test_write_config(config, path))
}
# }}}

# cli_shift_test_mock_monthly_fixtures {{{
cli_shift_test_mock_monthly_fixtures <- function(env = parent.frame()) {
    variables <- epw_morph_variables(
        transform__recipe(monthly_transform("epwshiftr"))
    )
    files <- stats::setNames(
        vapply(
            variables,
            function(variable_id) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable_id,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
    withr::defer(unlink(files), envir = env)
    docs <- data.table::rbindlist(
        lapply(variables, function(variable_id) {
            esgf_test__file_docs(
                basename(files[[variable_id]]),
                opendap_url = files[[variable_id]],
                download_url = files[[variable_id]],
                variable_id = variable_id,
                frequency = "mon",
                table_id = "Amon"
            )
        }),
        fill = TRUE
    )
    # Each synthetic variable represents a separate CMIP6 File identity.
    future_id <- paste0("future-", docs$variable_id)
    data.table::set(docs, j = "dataset_id", value = future_id)
    data.table::set(docs, j = "master_id", value = future_id)
    data.table::set(
        docs,
        j = "instance_id",
        value = paste0(future_id, ".v20260101")
    )
    data.table::set(
        docs,
        j = "tracking_id",
        value = paste0("hdl:21.14100/", future_id)
    )
    data.table::set(docs, j = "id", value = paste0(docs$title, "|", future_id))
    data.table::set(
        docs,
        j = "checksum",
        value = vapply(
            files[docs$variable_id],
            checksum_file,
            character(1L),
            algo = "sha256"
        )
    )
    shift_test__mock_collect(docs, env = env)
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
    plan <- cli_run(
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
    )
    run <- cli_run(
        setup$dir,
        "extract",
        "run",
        "--plan",
        paste(plan$result$plan_id, collapse = ",")
    )
    testthat::expect_equal(plan$status, 0L)
    testthat::expect_equal(run$status, 0L)
    testthat::expect_true(all(run$result$status == "done"))
    c(setup, list(plan_id = plan$result$plan_id))
}
# }}}

# vim: fdm=marker :
