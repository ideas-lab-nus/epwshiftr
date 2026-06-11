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

    missing_dir <- tempfile("missing-store-")
    root_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help"))
    expect_equal(root_help$status, 0L)
    expect_match(root_help$result[[1L]], "Usage: epwshiftr")
    expect_false(dir.exists(missing_dir))

    group_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "query", "help"))
    expect_equal(group_help$status, 0L)
    expect_match(group_help$result[[1L]], "Usage: epwshiftr query")
    expect_false(dir.exists(missing_dir))

    command_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "download", "run", "--help"))
    expect_equal(command_help$status, 0L)
    expect_match(command_help$result[[1L]], "Usage: epwshiftr download run")
    expect_false(dir.exists(missing_dir))

    watch_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "download", "watch"))
    expect_equal(watch_help$status, 0L)
    expect_match(watch_help$result[[1L]], "Usage: epwshiftr download watch")
    expect_false(dir.exists(missing_dir))

    help_topic <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "storage", "validate"))
    expect_equal(help_topic$status, 0L)
    expect_match(help_topic$result[[1L]], "Usage: epwshiftr storage validate")
    expect_false(dir.exists(missing_dir))

    bad_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "missing"))
    expect_equal(bad_help$status, 2L)
    expect_match(bad_help$error, "Unknown help topic")
    expect_false(dir.exists(missing_dir))

    doctor_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "doctor", "--help"))
    expect_equal(doctor_help$status, 0L)
    expect_match(doctor_help$result[[1L]], "Usage: epwshiftr doctor")
    expect_false(dir.exists(missing_dir))

    missing_doctor <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "doctor"))
    expect_equal(missing_doctor$status, 0L)
    expect_named(missing_doctor$result, c("summary", "checks"))
    expect_true(any(missing_doctor$result$checks$check == "store_path"))
    expect_false(dir.exists(missing_dir))

    doctor_text <- capture.output(
        doctor_rendered <- epwshiftr_cli(c("--store", missing_dir, "doctor")),
        type = "message"
    )
    expect_equal(doctor_rendered$status, 0L)
    expect_true(any(grepl("epwshiftr doctor", doctor_text)))
    expect_true(any(grepl("Summary", doctor_text)))
    expect_true(any(grepl("Checks", doctor_text)))
    expect_false(any(grepl("^\\$summary", doctor_text)))
    expect_false(dir.exists(missing_dir))

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    store$downloader(n_workers = 0L)

    doctor <- epwshiftr_cli(c("--quiet", "--store", dir, "doctor"))
    expect_equal(doctor$status, 0L)
    expect_named(doctor$result, c("summary", "checks"))
    expect_true(all(c("store_manifest", "store_schema", "downloader_manifest", "downloader_schema", "index_node") %in% doctor$result$checks$check))
    expect_equal(doctor$result$checks$status[doctor$result$checks$check == "store_manifest"], "ok")
    expect_equal(doctor$result$checks$status[doctor$result$checks$check == "downloader_manifest"], "ok")

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

    list_text <- capture.output(
        listed_rendered <- epwshiftr_cli(c("--store", dir, "query", "list")),
        type = "message"
    )
    expect_equal(listed_rendered$status, 0L)
    expect_true(any(grepl("Stored ESGF queries", list_text)))
    expect_true(any(grepl("Cli Query|cli query", list_text)))
    expect_false(any(grepl("^\\[\\[|^\\$", list_text)))

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

    dry_search <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "search",
        "--index-node", "https://example.org", "--dry-run",
        "--columns", "variable_id,source_id",
        "project=CMIP6", "source_id!=BCC-CSM2-MR", "variable_id=tas,pr",
        "datetime_start=2050", "datetime_stop=2050-12-31", "latest=true"
    ))
    expect_equal(dry_search$status, 0L)
    expect_equal(dry_search$result$type, "Dataset")
    expect_match(dry_search$result$url, "project=CMIP6")
    expect_match(dry_search$result$url, "source_id!=BCC-CSM2-MR", fixed = TRUE)
    expect_match(dry_search$result$url, "latest=true")
    decoded_search_url <- URLdecode(dry_search$result$url)
    expect_match(decoded_search_url, "datetime_start:[* TO 2050-01-01T00:00:00Z]", fixed = TRUE)
    expect_match(decoded_search_url, "datetime_stop:[2050-12-31T00:00:00Z TO *]", fixed = TRUE)
    expect_false("columns" %in% names(dry_search$result))

    mixed_negate <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "search", "--dry-run",
        "source_id=MPI-ESM1-2-HR", "source_id!=BCC-CSM2-MR"
    ))
    expect_equal(mixed_negate$status, 2L)
    expect_match(mixed_negate$error, "Cannot combine positive and negated values")

    negated_condition <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "search", "--dry-run",
        "datetime_start!=2050"
    ))
    expect_equal(negated_condition$status, 2L)
    expect_match(negated_condition$error, "Query condition cannot be negated")

    before_dry_add <- store$queries()$query_id
    dry_add <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "add",
        "--index-node", "https://example.org", "--dry-run",
        "--label", "dry add", "--tag", "cmip", "--tag", "tas",
        "project=CMIP6", "variable_id=tas", "latest=true"
    ))
    expect_equal(dry_add$status, 0L)
    expect_true(dry_add$result$dry_run)
    expect_match(dry_add$result$url, "project=CMIP6")
    expect_match(dry_add$result$url, "latest=true")
    expect_equal(dry_add$result$label, "dry add")
    expect_setequal(dry_add$result$tags, c("cmip", "tas"))
    expect_setequal(store$queries()$query_id, before_dry_add)

    added <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "add",
        "--index-node", "https://example.org",
        "--label", "added from cli", "--track",
        "--tag", "cmip", "--tag", "daily",
        "project=CMIP6", "source_id!=BCC-CSM2-MR,CESM2", "variable_id=tas,pr", "latest=true"
    ))
    expect_equal(added$status, 0L)
    expect_equal(added$result$label, "added from cli")
    expect_true(added$result$tracked)
    expect_equal(added$result$index_node, "https://example.org")
    expect_setequal(store$query_tags(added$result$query_id)$tag, c("cmip", "daily"))
    added_query <- esg_query()$load(file.path(dir, added$result$query_file))
    expect_true(query_param_negate(added_query$source_id()))
    expect_equal(query_param_value(added_query$source_id()), c("BCC-CSM2-MR", "CESM2"))

    time_added <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "add",
        "--index-node", "https://example.org",
        "--label", "time from cli",
        "project=CMIP6", "variable_id=tas", "datetime_start=2050", "datetime_end=2060"
    ))
    expect_equal(time_added$status, 0L)
    time_query <- esg_query()$load(file.path(dir, time_added$result$query_file))
    time_range <- time_query$datetime_range()
    expect_false(is.null(time_range$start))
    expect_false(is.null(time_range$stop))

    json_text <- capture.output(
        json <- epwshiftr_cli(c("--store", dir, "--json", "query", "list"))
    )
    parsed <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json$status, 0L)
    expect_true(query_id %in% parsed$query_id)

    query_file <- tempfile(fileext = ".json")
    esg_query("https://example.org")$
        experiment_id("ssp126")$
        variable_id("tas")$
        save(query_file)
    imported <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "add", "--query-file", query_file, "--label", "imported", "--tag", "imported", "--track"))
    expect_equal(imported$status, 0L)
    expect_equal(imported$result$label, "imported")
    expect_true(imported$result$tracked)
    expect_equal(store$query_tags(imported$result$query_id)$tag, "imported")

    empty_add <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "add"))
    expect_equal(empty_add$status, 2L)
    expect_match(empty_add$error, "requires --query-file")

    mixed_add <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "add", "--query-file", query_file, "project=CMIP6"))
    expect_equal(mixed_add$status, 2L)
    expect_match(mixed_add$error, "cannot combine --query-file")

    bad_text <- capture.output(
        bad <- epwshiftr_cli(c("--store", dir, "--json", "query", "does-not-exist"))
    )
    bad_json <- jsonlite::fromJSON(paste(bad_text, collapse = "\n"))
    expect_equal(bad$status, 2L)
    expect_match(bad$error, "Unknown query command")
    expect_equal(bad_json$status, 2L)

    quiet_bad <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "preflight"))
    expect_equal(quiet_bad$status, 2L)
    expect_match(quiet_bad$error, "Missing required argument")
})

test_that("epwshiftr_cli dispatches esgf store commands", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    query_id <- store$add_query(
        esg_query("https://example.org")$
            experiment_id("ssp585")$
            variable_id("tas")$
            limit(1L),
        label = "cli esgf",
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

    searched <- epwshiftr_cli(c(
        "--quiet", "--store", dir, "query", "search",
        "--index-node", "https://example.org", "--type", "File",
        "--fields", "id,title", "--columns", "title,id", "--limit", "1",
        "experiment_id=ssp585", "variable_id=tas"
    ))
    expect_equal(searched$status, 0L)
    expect_equal(nrow(searched$result), 1L)
    expect_true(all(c("id", "title") %in% names(searched$result)))

    json_search_text <- capture.output(
        json_search <- epwshiftr_cli(c(
            "--store", dir, "--json", "query", "search",
            "--index-node", "https://example.org", "--type", "File",
            "--fields", "id,title,source_id", "--columns", "title",
            "--limit", "1",
            "experiment_id=ssp585", "variable_id=tas"
        ))
    )
    expect_equal(json_search$status, 0L)
    json_search_result <- jsonlite::fromJSON(paste(json_search_text, collapse = "\n"))
    expect_true(all(c("id", "title", "source_id") %in% names(json_search_result)))

    capture.output(
        bad_columns <- epwshiftr_cli(c(
            "--store", dir, "query", "search",
            "--index-node", "https://example.org", "--type", "File",
            "--fields", "id,title", "--columns", "missing_column",
            "--limit", "1",
            "experiment_id=ssp585", "variable_id=tas"
        )),
        type = "message"
    )
    expect_equal(bad_columns$status, 2L)
    expect_match(bad_columns$error, "Unknown display column")

    preview <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "preview", query_id, "--detail"))
    expect_equal(preview$status, 0L)
    expect_named(preview$result, c("summary", "changes"))
    expect_equal(preview$result$summary$query_id, query_id)

    preview_text <- capture.output(
        preview_rendered <- epwshiftr_cli(c("--store", dir, "query", "preview", query_id, "--detail")),
        type = "message"
    )
    expect_equal(preview_rendered$status, 0L)
    expect_true(any(grepl("Query update preview", preview_text)))
    expect_true(any(grepl("Summary", preview_text)))
    expect_true(any(grepl("Changes", preview_text)))
    expect_false(any(grepl("^\\$summary", preview_text)))

    updated <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "update", query_id))
    expect_equal(updated$status, 0L)
    expect_equal(nrow(updated$result), 1L)

    esgf_help <- epwshiftr_cli(c("--quiet", "--store", dir, "esgf", "help"))
    expect_equal(esgf_help$status, 0L)
    expect_match(esgf_help$result[[1L]], "Usage: epwshiftr esgf")

    status <- epwshiftr_cli(c("--quiet", "--store", dir, "esgf", "report", "--query", query_id))
    expect_equal(status$status, 0L)
    expect_named(status$result, c("summary", "updates", "changes", "downloads", "nodes"))
    expect_equal(status$result$summary$query_id, query_id)

    esgf_text <- capture.output(
        esgf_rendered <- epwshiftr_cli(c("--store", dir, "esgf", "report", "--query", query_id)),
        type = "message"
    )
    expect_equal(esgf_rendered$status, 0L)
    expect_true(any(grepl("ESGF report", esgf_text)))
    expect_true(any(grepl("Summary", esgf_text)))
    expect_true(any(grepl("Downloads", esgf_text)))
    expect_false(any(grepl("^\\$summary", esgf_text)))

    old_workflow <- epwshiftr_cli(c("--quiet", "--store", dir, "workflow", "report", "--query", query_id))
    expect_equal(old_workflow$status, 2L)

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
        "manifest", "data_dir", "tmp_dir", "retries", "timeout", "n_workers",
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

    watch <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "watch", "--query", query_id, "--session", session_id, "--events", "1"))
    expect_equal(watch$status, 0L)
    expect_named(watch$result, c("summary", "tasks", "nodes", "events"))
    expect_equal(watch$result$summary$task_count, 1L)
    expect_equal(watch$result$summary$queued, 1L)
    expect_equal(watch$result$summary$last_download_session_id, session_id)
    expect_lte(nrow(watch$result$events), 1L)

    watch_text <- capture.output(
        watch_rendered <- epwshiftr_cli(c("--store", dir, "download", "watch", "--query", query_id, "--session", session_id, "--events", "1")),
        type = "message"
    )
    expect_equal(watch_rendered$status, 0L)
    expect_true(any(grepl("Download activity", watch_text)))
    expect_true(any(grepl("Tasks", watch_text)))
    expect_true(any(grepl("Recent events", watch_text)))
    expect_false(any(grepl("^\\$summary", watch_text)))

    logs <- epwshiftr_cli(c("--quiet", "--store", dir, "download", "logs", "--session", session_id, "--tail", "1"))
    expect_equal(logs$status, 0L)
    expect_lte(nrow(logs$result), 1L)
    expect_true(all(logs$result$session_id == session_id))

    logs_text <- capture.output(
        logs_rendered <- epwshiftr_cli(c("--store", dir, "download", "logs", "--session", session_id, "--tail", "1")),
        type = "message"
    )
    expect_equal(logs_rendered$status, 0L)
    expect_true(any(grepl("Download events", logs_text)))
    expect_false(any(grepl("^\\[\\[|^\\$", logs_text)))

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

test_that("epwshiftr_cli table output adapts to console width", {
    withr::local_options(width = 54L)

    rows <- data.frame(
        status = "queued",
        filename = "very-long-climate-file-name-for-cli-output.nc",
        bytes_done = 0,
        size = 1024,
        attempts = 0,
        last_error = "this error message should be hidden before core status columns",
        session_id = paste(rep("s", 64L), collapse = ""),
        task_id = paste(rep("t", 64L), collapse = ""),
        file_key = paste(rep("f", 64L), collapse = ""),
        stringsAsFactors = FALSE
    )

    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Narrow table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error", "session_id", "task_id", "file_key")
        ),
        type = "message"
    )

    table_lines <- text[grepl("^[\u250c\u2502\u251c\u2514+|]", text)]
    expect_true(length(table_lines) > 0L)
    expect_lte(max(cli::ansi_nchar(table_lines, type = "width")), 54L)
    expect_true(any(grepl("Hidden columns for console width", text)))
    expect_true(any(grepl("Status", text)))
})

test_that("epwshiftr_cli table output highlights status and progress", {
    withr::local_options(cli.num_colors = 256L, width = 120L)

    danger <- epwshiftr:::epwshiftr_cli_color_status("error")
    success <- epwshiftr:::epwshiftr_cli_color_status("done")
    expect_false(identical(danger, "error"))
    expect_false(identical(success, "done"))
    expect_equal(cli::ansi_nchar(danger, type = "width"), 5L)
    expect_equal(cli::ansi_nchar(success, type = "width"), 4L)

    rows <- data.frame(
        status = c("error", "done"),
        filename = c("failed.nc", "done.nc"),
        bytes_done = c(128, 1024),
        size = c(1024, 1024),
        attempts = c(2L, 1L),
        last_error = c("temporary failure", NA_character_),
        stringsAsFactors = FALSE
    )
    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Progress table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error")
        ),
        type = "message"
    )

    expect_true(any(grepl("Progress", text)))
    expect_true(any(grepl("\\[[#-]+\\]", text)))
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
