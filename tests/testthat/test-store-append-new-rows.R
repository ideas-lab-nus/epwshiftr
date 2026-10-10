# append_new_rows() inserts only rows whose key is not yet stored. It must keep
# its earlier behavior while asking DuckDB only about the keys being inserted
# instead of reading the whole key column back into R.
store_append_test__store <- function(env = parent.frame()) {
    store <- EsgStore$new(tempfile("store-append-"))
    withr::defer(store$close(), envir = env)
    ddb_exec(
        priv(store)$conn,
        "CREATE TABLE append_t (k VARCHAR PRIMARY KEY, v VARCHAR, n INTEGER)"
    )
    store
}

store_append_test__rows <- function(keys, v = "new") {
    data.frame(
        k = keys,
        v = v,
        n = seq_along(keys),
        stringsAsFactors = FALSE
    )
}

# The previous implementation, kept as an oracle: read every key, filter in R.
store_append_test__oracle <- function(conn, table, rows, key) {
    current <- ddb_query(
        conn,
        sprintf(
            "SELECT %s FROM %s",
            ddb_ident(conn, key),
            ddb_ident(conn, table)
        )
    )[[key]]
    rows <- rows[!rows[[key]] %in% current, , drop = FALSE]
    if (nrow(rows)) {
        ddb_append_table(conn, table, rows)
    }
    invisible(NULL)
}

store_append_test__state <- function(store) {
    out <- ddb_query(priv(store)$conn, "SELECT k, v, n FROM append_t")
    out[order(out$k), , drop = FALSE]
}

test_that("append_new_rows skips stored keys and matches the earlier behavior", {
    skip_if_not_installed("duckdb")
    new <- store_append_test__store()
    old <- store_append_test__store()
    seed <- store_append_test__rows(c("a", "b", "c"), v = "seed")
    for (s in list(new, old)) {
        ddb_append_table(priv(s)$conn, "append_t", seed)
    }
    batch <- store_append_test__rows(c("b", "d", "e", "a"), v = "batch")
    priv(new)$append_new_rows("append_t", batch, "k")
    store_append_test__oracle(priv(old)$conn, "append_t", batch, "k")
    expect_identical(
        store_append_test__state(new),
        store_append_test__state(old)
    )
    state <- store_append_test__state(new)
    expect_identical(state$k, c("a", "b", "c", "d", "e"))
    # Stored rows keep their values; only d and e were inserted.
    expect_identical(state$v, c("seed", "seed", "seed", "batch", "batch"))

    # Nothing to add, an empty batch and an empty table are all no-ops.
    priv(new)$append_new_rows("append_t", batch, "k")
    priv(new)$append_new_rows("append_t", batch[0, ], "k")
    expect_identical(nrow(store_append_test__state(new)), 5L)
    empty <- store_append_test__store()
    priv(empty)$append_new_rows(
        "append_t",
        store_append_test__rows("first"),
        "k"
    )
    expect_identical(store_append_test__state(empty)$k, "first")
})

test_that("append_new_rows keeps its failure behavior for bad keys", {
    skip_if_not_installed("duckdb")
    new <- store_append_test__store()
    old <- store_append_test__store()
    # A repeated key inside one batch violates the primary key either way.
    dup <- store_append_test__rows(c("x", "x"))
    expect_error(priv(new)$append_new_rows("append_t", dup, "k"))
    expect_error(store_append_test__oracle(
        priv(old)$conn,
        "append_t",
        dup,
        "k"
    ))
    # A missing key cannot match a stored key, so the row is attempted and the
    # primary key rejects it, as before.
    missing <- store_append_test__rows(c("y", NA))
    expect_error(priv(new)$append_new_rows("append_t", missing, "k"))
    expect_error(
        store_append_test__oracle(priv(old)$conn, "append_t", missing, "k")
    )
    expect_identical(
        store_append_test__state(new),
        store_append_test__state(old)
    )
})

test_that("append_new_rows handles factor keys and keys that need quoting", {
    skip_if_not_installed("duckdb")
    store <- store_append_test__store()
    odd <- c(
        "it's",
        "semi;colon",
        "quote\"d",
        "CaSe",
        "case",
        "tab\tkey",
        "ünï"
    )
    ddb_append_table(
        priv(store)$conn,
        "append_t",
        store_append_test__rows(odd[1:4], v = "seed")
    )
    batch <- store_append_test__rows(odd)
    batch$k <- factor(batch$k, levels = odd)
    priv(store)$append_new_rows("append_t", batch, "k")
    state <- store_append_test__state(store)
    expect_setequal(state$k, odd)
    expect_identical(sum(state$v == "seed"), 4L)
    # Case differences are different keys, as they are in R.
    expect_true(all(c("CaSe", "case") %in% state$k))
})

test_that("append_new_rows queries only the keys it inserts, in bounded chunks", {
    skip_if_not_installed("duckdb")
    store <- store_append_test__store()
    stored <- sprintf("k%06d", seq(1L, 9000L, by = 2L))
    ddb_append_table(
        priv(store)$conn,
        "append_t",
        store_append_test__rows(stored, v = "seed")
    )
    # 4500 keys span three chunks; half of them already exist.
    keys <- sprintf("k%06d", 1:4500)
    original <- ddb_query
    sql <- character()
    local_mocked_bindings(
        ddb_query = function(conn, query) {
            sql <<- c(sql, query)
            original(conn, query)
        },
        .package = "epwshiftr"
    )
    priv(store)$append_new_rows("append_t", store_append_test__rows(keys), "k")
    expect_identical(length(sql), 3L)
    expect_true(all(grepl(" WHERE ", sql, fixed = TRUE)))
    expect_false(any(grepl("^SELECT \"k\" FROM \"append_t\"$", sql)))
    expect_true(all(nchar(sql) < 200000L))

    state <- store_append_test__state(store)
    expected <- sort(union(stored, keys))
    expect_identical(state$k, expected)
    # Pre-existing rows were not overwritten; new ones carry the batch value.
    expect_identical(
        state$v[state$k %in% stored],
        rep("seed", length(stored))
    )
    expect_true(all(state$v[!state$k %in% stored] == "new"))
})

# vim: fdm=marker :
