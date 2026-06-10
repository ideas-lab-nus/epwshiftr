cli_test_response <- function(docs) {
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
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
}

cli_test_file_docs <- function(path = "cli-file.nc", download_url = NULL) {
    if (is.null(download_url)) {
        download_url <- sprintf("https://example.org/fileServer/%s", path)
    }
    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", path),
        master_id = sprintf("%s.master", path),
        replica = FALSE,
        tracking_id = "hdl:21.14100/cli-test",
        title = path,
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("https://example.org/dods/%s.html|application/netcdf|OPENDAP", path),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

test_that("epwshiftr_cli reports usage and JSON output", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    query_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("ssp585")$
            variable_id("tas")$
            limit(1L),
        label = "cli query",
        track = TRUE
    )

    listed <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "list"))
    expect_equal(listed$status, 0L)
    expect_equal(listed$result$query_id, query_id)

    json_text <- capture.output(
        json <- epwshiftr_cli(c("--store", dir, "--json", "query", "list"))
    )
    parsed <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json$status, 0L)
    expect_equal(parsed$query_id, query_id)

    bad_text <- capture.output(
        bad <- epwshiftr_cli(c("--store", dir, "--json", "query", "add"))
    )
    bad_json <- jsonlite::fromJSON(paste(bad_text, collapse = "\n"))
    expect_equal(bad$status, 2L)
    expect_match(bad$error, "Unknown query command")
    expect_equal(bad_json$status, 2L)

    quiet_bad <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "preflight"))
    expect_equal(quiet_bad$status, 2L)
    expect_match(quiet_bad$error, "Missing required argument")
})

test_that("epwshiftr_cli dispatches store workflow commands", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    query_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("ssp585")$
            variable_id("tas")$
            limit(1L),
        label = "cli workflow",
        track = TRUE
    )

    file_docs <- cli_test_file_docs()
    testthat::local_mocked_bindings(
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
            response <- cli_test_response(file_docs)
            params$fields(c(query_param_value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    preview <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "preview", query_id, "--detail"))
    expect_equal(preview$status, 0L)
    expect_named(preview$result, c("summary", "changes"))
    expect_equal(preview$result$summary$query_id, query_id)

    updated <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "update", query_id))
    expect_equal(updated$status, 0L)
    expect_equal(nrow(updated$result), 1L)

    status <- epwshiftr_cli(c("--quiet", "--store", dir, "workflow", "report", "--query", query_id))
    expect_equal(status$status, 0L)
    expect_named(status$result, c("summary", "updates", "changes", "downloads", "nodes"))
    expect_equal(status$result$summary$query_id, query_id)

    storage <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "report", "--detail"))
    expect_equal(storage$status, 0L)
    expect_named(storage$result, c("summary", "downloads", "registered", "untracked_files", "missing_records", "tmp", "orphan_records"))

    validated <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "validate", "--checksum"))
    expect_equal(validated$status, 0L)
    expect_named(validated$result, c("summary", "files", "artifacts", "untracked", "actions"))

    repaired <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "repair"))
    expect_equal(repaired$status, 0L)
    expect_true(all(repaired$result$dry_run))

    dl <- store$downloader(n_workers = 0L)
    catalog <- store$query("SELECT file_key FROM file_catalog")
    src <- tempfile()
    writeLines("cli download placeholder", src)
    session_id <- dl$enqueue(data.table::data.table(
        logical_file_id = "cli-download-task",
        file_key = catalog$file_key[[1L]],
        filename = "cli-download.nc",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        priority = 1L
    ))

    download_status <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "status", "--query", query_id, "--session", session_id))
    expect_equal(download_status$status, 0L)
    expect_equal(download_status$result$session_id, session_id)
    expect_equal(download_status$result$status, "queued")

    retry <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "retry", "--query", query_id, "--session", session_id))
    expect_equal(retry$status, 0L)
    expect_equal(nrow(retry$result), 0L)
})

test_that("install_cli and uninstall_cli manage generated launchers", {
    bin_dir <- tempfile("epwshiftr-bin-")
    name <- "epwshiftr-test"
    installed <- install_cli(bin_dir = bin_dir, name = name)
    expect_true(file.exists(installed$path))
    expect_equal(installed$bin_dir, normalizePath(bin_dir, mustWork = FALSE, winslash = "/"))
    expect_false(installed$in_path)

    content <- readLines(installed$path, warn = FALSE)
    expect_true(any(grepl("Generated by epwshiftr::install_cli\\(\\)", content)))
    expect_true(any(grepl("epwshiftr::epwshiftr_cli\\(exit = TRUE\\)", content)))
    expect_error(install_cli(bin_dir = bin_dir, name = name), "already exists")
    overwritten <- install_cli(bin_dir = bin_dir, name = name, overwrite = TRUE)
    expect_equal(overwritten$path, installed$path)
    if (.Platform$OS.type != "windows") {
        expect_equal(unname(file.access(installed$path, 1L)), 0L)
    }

    removed <- uninstall_cli(bin_dir = bin_dir, name = name)
    expect_true(removed$removed)
    expect_false(file.exists(installed$path))

    dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)
    unsafe <- file.path(bin_dir, if (.Platform$OS.type == "windows") paste0(name, ".cmd") else name)
    writeLines("not generated by epwshiftr", unsafe)
    expect_error(uninstall_cli(bin_dir = bin_dir, name = name), "Refusing to remove")
    expect_true(file.exists(unsafe))
})
