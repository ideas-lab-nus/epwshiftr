# Reopening a store must inspect persisted state without issuing redundant DDL.
test_that("current store reopen preserves rows without redundant schema writes", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$set_meta("sentinel", "keep")
    before <- ddb_query(
        priv(store)$conn,
        "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, ordinal_position"
    )
    store$close()
    original <- ddb_exec
    statements <- character()
    local_mocked_bindings(
        ddb_exec = function(conn, sql) {
            statements <<- c(statements, sql)
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    store <- EsgStore$new(path, create = FALSE)
    on.exit(store$close(), add = TRUE)
    expect_identical(store$get_meta("sentinel"), "keep")
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    expect_false(any(grepl("CREATE TABLE|ALTER TABLE", statements)))
    expect_identical(
        ddb_query(
            priv(store)$conn,
            "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, ordinal_position"
        ),
        before
    )
})

# A same-version manifest can lose a table or an additive column between opens.
# Verify recovery against a fresh complete catalog, including persisted rows.
test_that("current store repairs missing tables and additive columns", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-repair-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    store$set_meta("sentinel", "keep")
    catalog <- ddb_query(
        priv(store)$conn,
        "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, column_name"
    )
    ddb_exec(priv(store)$conn, "DROP TABLE shift_run_event")
    ddb_exec(
        priv(store)$conn,
        "ALTER TABLE epw_output DROP COLUMN provenance_json"
    )
    ddb_exec(priv(store)$conn, "ALTER TABLE file_catalog DROP COLUMN latest")
    store$close()
    store <- EsgStore$new(path, create = FALSE)
    expect_identical(
        ddb_query(
            priv(store)$conn,
            "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, column_name"
        ),
        catalog
    )
    expect_identical(store$get_meta("sentinel"), "keep")
})

# Checking a valid store once must not authorize later opens after its version
# or metadata has changed; no connection/path-level success cache is allowed.
test_that("reopening still rejects changed or missing schema versions", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-version-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$close()
    for (version in c("older", "newer", NA_character_)) {
        conn <- ddb_connect(file.path(path, "manifest.duckdb"))
        ddb_exec(conn, "DELETE FROM store_meta WHERE key = 'schema_version'")
        if (!is.na(version)) {
            ddb_exec(
                conn,
                sprintf(
                    "INSERT INTO store_meta VALUES ('schema_version', '%s', NULL)",
                    version
                )
            )
        }
        ddb_exec(conn, "DROP TABLE IF EXISTS shift_run_event")
        ddb_disconnect(conn)
        expect_error(EsgStore$new(path, create = FALSE), "Store schema version")
        conn <- ddb_connect(file.path(path, "manifest.duckdb"))
        expect_false("shift_run_event" %in% ddb_list_tables(conn))
        ddb_disconnect(conn)
    }
})

# An interrupted additive repair must be retried from the database state. The
# original live instance owns the connection so cleanup remains deterministic.
test_that("schema repair retries after a failed DDL statement", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-retry-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    ddb_exec(priv(store)$conn, "DROP TABLE shift_run_event")
    original <- ddb_exec
    attempts <- 0L
    local_mocked_bindings(
        ddb_exec = function(conn, sql) {
            if (grepl("CREATE TABLE IF NOT EXISTS shift_run_event", sql)) {
                attempts <<- attempts + 1L
                if (attempts == 1L) stop("interrupted schema repair")
            }
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    expect_error(priv(store)$init_schema(), "interrupted schema repair")
    expect_false("shift_run_event" %in% ddb_list_tables(priv(store)$conn))
    expect_invisible(priv(store)$init_schema())
    expect_true("shift_run_event" %in% ddb_list_tables(priv(store)$conn))
    expect_identical(attempts, 2L)
})

# Fresh manifests must initialize all workflow families and preserve empty-table
# behavior before any artifact or run has been recorded.
test_that("fresh store initializes current empty workflow tables", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-fresh-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    for (table in c(
        "artifact",
        "esg_file",
        "file_catalog",
        "extraction_plan",
        "epw_source",
        "epw_output",
        "shift_run",
        "shift_run_event"
    )) {
        rows <- ddb_read_table(priv(store)$conn, table)
        expect_identical(nrow(rows), 0L)
        expect_gt(ncol(rows), 0L)
    }
    expect_true(
        "provenance_json" %in%
            names(ddb_read_table(
                priv(store)$conn,
                "epw_output"
            ))
    )
})

# vim: fdm=marker :
