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

# Creating a store must send its schema in a few batches, not one call per
# declaration, and the resulting catalog must equal the one built statement by
# statement.
test_that("fresh store schema is created in batches with the same catalog", {
    skip_if_not_installed("duckdb")
    catalog_sql <- paste(
        "SELECT table_name, column_name, data_type, column_default, is_nullable",
        "FROM information_schema.columns ORDER BY table_name, ordinal_position"
    )
    original <- ddb_exec
    sent <- character()
    local_mocked_bindings(
        ddb_exec = function(conn, sql) {
            sent <<- c(sent, sql)
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    path <- tempfile("store-schema-batch-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    batched <- ddb_query(priv(store)$conn, catalog_sql)

    declarations <- sum(lengths(regmatches(
        sent,
        gregexpr("CREATE TABLE IF NOT EXISTS|ALTER TABLE", sent)
    )))
    expect_gt(declarations, 40L)
    expect_lt(sum(grepl("CREATE TABLE|ALTER TABLE", sent)), 6L)

    # Execute the same declarations one at a time against an empty database and
    # compare the resulting columns, types, defaults and nullability.
    statements <- unlist(lapply(sent, function(sql) {
        parts <- strsplit(sql, ";\n", fixed = TRUE)[[1L]]
        parts[grepl("CREATE TABLE|ALTER TABLE", parts)]
    }))
    conn <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(conn), add = TRUE)
    for (sql in statements) {
        original(conn, sql)
    }
    sequential <- ddb_query(conn, catalog_sql)
    expect_identical(
        batched[order(batched$table_name, batched$column_name), ],
        sequential[order(sequential$table_name, sequential$column_name), ]
    )
})

# The schema version is read at most once per open: a new manifest has nothing
# to read, a reopened one is read once for the check, and the version row is
# only written when it is absent.
test_that("opening a store reads the schema version at most once", {
    skip_if_not_installed("duckdb")
    original <- ddb_read_table
    reads <- character()
    local_mocked_bindings(
        ddb_read_table = function(conn, table, ...) {
            reads <<- c(reads, table)
            original(conn, table, ...)
        },
        .package = "epwshiftr"
    )
    path <- tempfile("store-schema-reads-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)

    store <- EsgStore$new(path)
    expect_identical(sum(reads == "store_meta"), 0L)
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    stamp <- ddb_query(
        priv(store)$conn,
        "SELECT updated_at FROM store_meta WHERE key = 'schema_version'"
    )$updated_at
    store$close()

    reads <- character()
    store <- EsgStore$new(path, create = FALSE)
    on.exit(store$close(), add = TRUE)
    expect_identical(sum(reads == "store_meta"), 1L)
    # A matching version is not rewritten on reopen.
    expect_identical(
        ddb_query(
            priv(store)$conn,
            "SELECT updated_at FROM store_meta WHERE key = 'schema_version'"
        )$updated_at,
        stamp
    )
})

# A manifest that has tables but lost its version row is still rejected rather
# than silently stamped with the current version.
test_that("a manifest with tables but no version row is rejected", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-noversion-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$close()
    conn <- ddb_connect(file.path(path, "manifest.duckdb"))
    ddb_exec(conn, "DELETE FROM store_meta WHERE key = 'schema_version'")
    ddb_disconnect(conn)
    expect_error(
        EsgStore$new(path, create = FALSE),
        "Store schema version .*missing"
    )
    conn <- ddb_connect(file.path(path, "manifest.duckdb"))
    on.exit(ddb_disconnect(conn), add = TRUE)
    expect_identical(
        nrow(ddb_query(
            conn,
            "SELECT * FROM store_meta WHERE key = 'schema_version'"
        )),
        0L
    )
})

# A failing statement stops the batch, but everything before it stays applied,
# exactly the state one-by-one execution would have left behind.
test_that("a failing schema statement keeps earlier declarations", {
    skip_if_not_installed("duckdb")
    path <- tempfile("store-schema-partial-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    original <- ddb_exec
    local_mocked_bindings(
        ddb_exec = function(conn, sql) {
            if (grepl("CREATE TABLE IF NOT EXISTS extraction_plan", sql)) {
                # Run only the declarations ahead of the failing one, then fail.
                parts <- strsplit(sql, ";\n", fixed = TRUE)[[1L]]
                stop_at <- grep("IF NOT EXISTS extraction_plan", parts)[[1L]]
                original(
                    conn,
                    paste(parts[seq_len(stop_at - 1L)], collapse = ";\n")
                )
                stop("controlled schema failure")
            }
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    expect_error(EsgStore$new(path), "controlled schema failure")
    conn <- ddb_connect(file.path(path, "manifest.duckdb"))
    on.exit(ddb_disconnect(conn), add = TRUE)
    tables <- ddb_list_tables(conn)
    expect_true("store_meta" %in% tables)
    expect_true("artifact" %in% tables)
    expect_false("extraction_plan" %in% tables)
})

# vim: fdm=marker :
