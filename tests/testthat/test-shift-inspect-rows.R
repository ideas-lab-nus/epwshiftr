# Identity-row reads must match store$query(), including empty results and
# errors from a closed store.
test_that("identity row reads match store queries", {
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
})

test_that("identity row reads reject a closed store", {
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
