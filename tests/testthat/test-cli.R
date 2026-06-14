# epwshiftr_cli() {{{
test_that("epwshiftr_cli() emits JSON output and dispatch errors", {
    skip_if_not_installed("duckdb")

    fixture <- cli_test_store()
    dir <- fixture$dir
    store <- fixture$store
    query_id <- fixture$query_id
    on.exit(store$close(), add = TRUE)

    json_text <- capture.output(
        json <- epwshiftr_cli(c("--store", dir, "--json", "query", "list"))
    )
    parsed <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json$status, 0L)
    expect_true(query_id %in% parsed$query_id)

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
# }}}
