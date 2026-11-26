# epwshiftr_cli_storage() / epwshiftr_cli_esgf() {{{
test_that("epwshiftr_cli_storage() / epwshiftr_cli_esgf() dispatch storage and ESGF commands", {
    fixture <- cli_test_store(label = "cli esgf")
    dir <- fixture$dir
    store <- fixture$store
    query_id <- fixture$query_id
    on.exit(store$close(), add = TRUE)

    file_docs <- esgf_test__file_docs(
        "cli-file.nc",
        opendap_url = "https://example.org/dods/cli-file.nc.html",
        download_url = "https://example.org/fileServer/cli-file.nc"
    )
    cli_test_mock_query_collect(file_docs)

    updated <- cli_run(dir, "query", "update", query_id)
    expect_equal(updated$status, 0L)

    esgf_help <- cli_run(dir, "esgf", "help")
    expect_equal(esgf_help$status, 0L)
    expect_match(esgf_help$result[[1L]], "Usage: epwshiftr esgf")

    status <- cli_run(dir, "esgf", "report", "--query", query_id)
    expect_equal(status$status, 0L)
    expect_named(
        status$result,
        c("summary", "updates", "changes", "downloads", "nodes")
    )
    expect_equal(status$result$summary$query_id, query_id)

    esgf_text <- capture.output(
        esgf_rendered <- cli_run(
            dir,
            "esgf",
            "report",
            "--query",
            query_id,
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(esgf_rendered$status, 0L)
    expect_true(any(grepl("ESGF report", esgf_text)))
    expect_true(any(grepl("Summary", esgf_text)))
    expect_true(any(grepl("Downloads", esgf_text)))
    expect_false(any(grepl("^\\$summary", esgf_text)))

    old_workflow <- cli_run(dir, "workflow", "report", "--query", query_id)
    expect_equal(old_workflow$status, 2L)

    storage <- cli_run(dir, "storage", "report", "--detail")
    expect_equal(storage$status, 0L)
    expect_named(
        storage$result,
        c(
            "summary",
            "downloads",
            "registered",
            "untracked_files",
            "missing_records",
            "tmp",
            "orphan_records"
        )
    )

    layout <- cli_run(dir, "storage", "layout", "show")
    expect_equal(layout$status, 0L)
    expect_equal(layout$result$layout, "flat")

    layout_set <- cli_run(
        dir,
        "storage",
        "layout",
        "set",
        "--layout",
        "drs",
        "--include-version",
        "false",
        "--collision",
        "suffix"
    )
    expect_equal(layout_set$status, 0L)
    expect_equal(layout_set$result$layout, "drs")
    expect_false(layout_set$result$include_version)
    expect_equal(layout_set$result$collision, "suffix")

    validated <- cli_run(
        dir,
        "storage",
        "validate",
        "--query",
        query_id,
        "--checksum"
    )
    expect_equal(validated$status, 0L)
    expect_named(
        validated$result,
        c("summary", "files", "artifacts", "untracked", "actions")
    )

    repaired <- cli_run(dir, "storage", "repair")
    expect_equal(repaired$status, 0L)
    expect_true(all(repaired$result$dry_run))

    cleanup <- cli_run(
        dir,
        "storage",
        "cleanup",
        "--scope",
        "tmp,missing_records",
        "--older-than",
        "0"
    )
    expect_equal(cleanup$status, 0L)
    expect_s3_class(cleanup$result, "data.table")
})
# }}}

# vim: fdm=marker :
