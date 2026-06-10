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

    shown <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "show", query_id))
    expect_equal(shown$status, 0L)
    expect_named(shown$result, c("query", "tags", "graph", "status"))
    expect_equal(shown$result$query$query_id, query_id)

    query_status <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "status", query_id))
    expect_equal(query_status$status, 0L)
    expect_equal(query_status$result$query_id, query_id)

    query_files <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "files", query_id, "--status", "current,stale"))
    expect_equal(query_files$status, 0L)
    expect_equal(nrow(query_files$result), 0L)

    query_updates <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "updates", query_id, "--latest"))
    expect_equal(query_updates$status, 0L)
    expect_equal(nrow(query_updates$result), 0L)

    query_changes <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "changes", query_id, "--latest"))
    expect_equal(query_changes$status, 0L)
    expect_equal(nrow(query_changes$result), 0L)

    tagged <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "tag", query_id, "daily", "cmip"))
    expect_equal(tagged$status, 0L)
    expect_setequal(tagged$result$tag, c("daily", "cmip"))

    tags <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "tags", query_id))
    expect_equal(tags$status, 0L)
    expect_setequal(tags$result$tag, c("daily", "cmip"))

    untagged <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "untag", query_id, "cmip"))
    expect_equal(untagged$status, 0L)
    expect_equal(untagged$result$tag, "daily")

    untracked <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "untrack", query_id))
    expect_equal(untracked$status, 0L)
    expect_false(untracked$result$tracked)

    tracked <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "track", query_id))
    expect_equal(tracked$status, 0L)
    expect_true(tracked$result$tracked)

    remove_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("historical")$
            variable_id("pr")$
            limit(1L),
        label = "remove me",
        track = FALSE
    )
    remove_preview <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "remove", remove_id))
    expect_equal(remove_preview$status, 0L)
    expect_true(remove_preview$result$dry_run)
    expect_true(remove_id %in% store$queries()$query_id)
    removed <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "remove", remove_id, "--execute"))
    expect_equal(removed$status, 0L)
    expect_false(remove_id %in% store$queries()$query_id)

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

    layout <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "layout", "show"))
    expect_equal(layout$status, 0L)
    expect_equal(layout$result$layout, "flat")

    layout_set <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "storage", "layout", "set",
        "--layout", "drs", "--include-version", "false", "--collision", "suffix"
    ))
    expect_equal(layout_set$status, 0L)
    expect_equal(layout_set$result$layout, "drs")
    expect_false(layout_set$result$include_version)
    expect_equal(layout_set$result$collision, "suffix")

    validated <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "validate", "--query", query_id, "--checksum"))
    expect_equal(validated$status, 0L)
    expect_named(validated$result, c("summary", "files", "artifacts", "untracked", "actions"))

    repaired <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "repair"))
    expect_equal(repaired$status, 0L)
    expect_true(all(repaired$result$dry_run))

    cleanup <- epwshiftr_cli(c("--quiet", "--store", dir, "storage", "cleanup", "--scope", "tmp,missing_records", "--older-than", "0"))
    expect_equal(cleanup$status, 0L)
    expect_s3_class(cleanup$result, "data.table")

    config <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "config", "show"))
    expect_equal(config$status, 0L)
    expect_named(config$result, c(
        "config_file", "manifest", "data_dir", "tmp_dir", "retries", "timeout", "n_workers",
        "network_policy", "node_policy", "transfer_policy", "resource_policy"
    ))

    config_set <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "download", "config", "set",
        "--workers", "0", "--timeout", "120", "--bandwidth-limit", "4096",
        "--host-concurrency", "2", "--ssl-verifypeer", "false", "--disk-preflight", "false",
        "--min-free-space", "0", "--cooldown-seconds", "10"
    ))
    expect_equal(config_set$status, 0L)
    expect_equal(config_set$result$n_workers, 0L)
    expect_equal(config_set$result$timeout, 120L)
    expect_false(config_set$result$network_policy$ssl_verifypeer)
    expect_equal(config_set$result$transfer_policy$bandwidth_limit, 4096)
    expect_equal(config_set$result$resource_policy$host_concurrency, 2)
    expect_false(config_set$result$resource_policy$disk_preflight)
    expect_equal(config_set$result$node_policy$cooldown_seconds, 10L)

    config_reload <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "config", "show"))
    expect_equal(config_reload$status, 0L)
    expect_equal(config_reload$result$n_workers, 0L)
    expect_equal(config_reload$result$timeout, 120L)

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

    sessions <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "sessions"))
    expect_equal(sessions$status, 0L)
    expect_true(session_id %in% sessions$result$session_id)

    tasks <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "tasks", "--session", session_id, "--status", "queued"))
    expect_equal(tasks$status, 0L)
    expect_equal(tasks$result$task_id, download_status$result$task_id)

    events <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "events", "--session", session_id))
    expect_equal(events$status, 0L)
    expect_true("enqueue" %in% events$result$event)

    nodes <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "nodes", "--service", "HTTPServer"))
    expect_equal(nodes$status, 0L)
    expect_s3_class(nodes$result, "data.table")

    reset_nodes <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "reset-nodes", "--service", "HTTPServer"))
    expect_equal(reset_nodes$status, 0L)
    expect_s3_class(reset_nodes$result, "data.table")

    retry <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "retry", "--query", query_id, "--session", session_id))
    expect_equal(retry$status, 0L)
    expect_equal(nrow(retry$result), 0L)

    cancelled <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "cancel", "--session", session_id))
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "cancelled")

    resumed <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "resume", "--session", session_id, "--no-progress"))
    expect_equal(resumed$status, 0L)
    expect_true(all(resumed$result$status %in% c("done", "skipped")))

    verified <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "verify", "--session", session_id))
    expect_equal(verified$status, 0L)
    expect_true(all(verified$result$checksum_ok))
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
