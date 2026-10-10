# Identity-row reads must return exactly what store$query() would, check the
# store lifecycle once, and still reject a closed store instead of returning an
# empty table.
test_that("identity row reads match store queries with one lifecycle check", {
    skip_if_not_installed("duckdb")
    path <- tempfile("inspect-rows-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    store$set_meta("sentinel", "keep")

    expected <- store$query(
        "SELECT * FROM \"store_meta\" WHERE \"key\" IN ('sentinel')"
    )
    expect_identical(
        shift_inspect__rows(store, "store_meta", "key", "sentinel"),
        expected
    )
    expect_identical(
        shift_inspect__rows(store, "store_meta", "key", "no-such-key"),
        store$query(
            "SELECT * FROM \"store_meta\" WHERE \"key\" IN ('no-such-key')"
        )
    )
    expect_equal(
        nrow(shift_inspect__rows(store, "store_meta", "key", "sentinel")),
        1L
    )

    # Count validity checks made by this package; the store must be checked once
    # per read, not once per wrapper layer.
    original <- ddb_is_valid
    checks <- 0L
    local_mocked_bindings(
        ddb_is_valid = function(conn) {
            checks <<- checks + 1L
            original(conn)
        },
        .package = "epwshiftr"
    )
    shift_inspect__rows(store, "store_meta", "key", "sentinel")
    expect_identical(checks, 1L)
})

test_that("identity row reads reject a closed store", {
    skip_if_not_installed("duckdb")
    path <- tempfile("inspect-rows-closed-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$close()
    expect_error(
        shift_inspect__rows(store, "store_meta", "key", "schema_version"),
        "closed"
    )
})

# vim: fdm=marker :
