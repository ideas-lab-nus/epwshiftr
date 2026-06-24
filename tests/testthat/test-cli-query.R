# epwshiftr_cli_query() {{{
test_that("epwshiftr_cli_query() dispatches stored query commands", {
    skip_if_not_installed("duckdb")

    fixture <- cli_test_store()
    dir <- fixture$dir
    store <- fixture$store
    query_id <- fixture$query_id
    on.exit(store$close(), add = TRUE)

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
    expect_true(query_param__negate(added_query$source_id()))
    expect_equal(query_param__value(added_query$source_id()), c("BCC-CSM2-MR", "CESM2"))

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
})
# }}}
# epwshiftr_cli_query() {{{
test_that("epwshiftr_cli_query() dispatches ESGF-backed query commands", {
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
    collect_calls <- list()
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
            progress_label = NULL
        ) {
            collect_calls[[length(collect_calls) + 1L]] <<- list(
                progress = progress,
                progress_label = progress_label
            )
            response <- cli_test_response(file_docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
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
    expect_false(any(vapply(collect_calls, `[[`, logical(1L), "progress")))

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
    expect_false(any(vapply(tail(collect_calls, 2L), `[[`, logical(1L), "progress")))

    jsonl_search_text <- capture.output(
        jsonl_search <- epwshiftr_cli(c(
            "--store", dir, "--jsonl", "query", "search",
            "--index-node", "https://example.org", "--type", "File",
            "--fields", "id,title,source_id", "--columns", "title",
            "--limit", "1",
            "experiment_id=ssp585", "variable_id=tas"
        ))
    )
    expect_equal(jsonl_search$status, 0L)
    expect_match(paste(jsonl_search_text, collapse = "\n"), "\"id\"")
    expect_false(any(vapply(tail(collect_calls, 2L), `[[`, logical(1L), "progress")))

    capture.output(
        no_progress <- epwshiftr_cli(c(
            "--store", dir, "query", "search",
            "--index-node", "https://example.org", "--type", "File",
            "--fields", "id,title", "--columns", "title,id", "--limit", "1",
            "--no-progress",
            "experiment_id=ssp585", "variable_id=tas"
        )),
        type = "message"
    )
    expect_equal(no_progress$status, 0L)
    expect_false(any(vapply(tail(collect_calls, 2L), `[[`, logical(1L), "progress")))

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
    expect_true(all(vapply(tail(collect_calls, 2L), `[[`, logical(1L), "progress")))
    expect_identical(
        vapply(tail(collect_calls, 2L), `[[`, character(1L), "progress_label"),
        c("Collecting Dataset records", "Collecting File records")
    )

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
})
# }}}
