test_that("EsgExtractStore creates a DuckDB manifest and store layout", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    expect_true(dir.exists(dir))
    expect_true(dir.exists(file.path(dir, "queries")))
    expect_true(dir.exists(file.path(dir, "downloads")))
    expect_true(dir.exists(file.path(dir, "data")))
    expect_equal(store$path, normalizePath(dir, mustWork = TRUE))
    expect_true(file.exists(store$manifest))
    expect_true(store$is_open)

    tables <- DBI::dbListTables(epwshiftr:::priv(store)$conn)
    expect_setequal(
        tables,
        c("query_run", "file_catalog", "extraction_plan", "extraction_result")
    )

    store$close()
    expect_false(store$is_open)
})

test_that("EsgExtractStore can reopen an existing store", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    EsgExtractStore$new(dir)$close()

    store <- EsgExtractStore$new(dir, create = FALSE)
    on.exit(store$close(), add = TRUE)

    expect_true(store$is_open)
    expect_true(file.exists(file.path(dir, "manifest.duckdb")))
})
