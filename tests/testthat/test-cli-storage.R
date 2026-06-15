# epwshiftr_cli_storage() / epwshiftr_cli_esgf() {{{
test_that("epwshiftr_cli_storage() and epwshiftr_cli_esgf() dispatch store commands", {
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
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            response <- cli_test_response(file_docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    updated <- epwshiftr_cli(c("--quiet", "--store", dir, "query", "update", query_id))
    expect_equal(updated$status, 0L)

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
})
# }}}
