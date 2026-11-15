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

# Read all persisted fields in key order to check insertion outcomes.
store_append_test__state <- function(store) {
    out <- ddb_query(priv(store)$conn, "SELECT k, v, n FROM append_t")
    out[order(out$k), , drop = FALSE]
}

test_that("append_new_rows preserves stored values and inserts only new keys", {
    store <- store_append_test__store()
    seed <- store_append_test__rows(c("a", "b", "c"), v = "seed")
    ddb_append_table(priv(store)$conn, "append_t", seed)
    batch <- store_append_test__rows(c("b", "d", "e", "a"), v = "batch")
    priv(store)$append_new_rows("append_t", batch, "k")
    state <- store_append_test__state(store)
    expect_identical(state$k, c("a", "b", "c", "d", "e"))
    expect_identical(state$v, c("seed", "seed", "seed", "batch", "batch"))
    expect_identical(state$n, c(1L, 2L, 3L, 2L, 3L))

    # Replaying the batch or submitting no rows must preserve every field.
    priv(store)$append_new_rows("append_t", batch, "k")
    priv(store)$append_new_rows("append_t", batch[0, ], "k")
    expect_identical(store_append_test__state(store), state)
    empty <- store_append_test__store()
    priv(empty)$append_new_rows(
        "append_t",
        store_append_test__rows("first"),
        "k"
    )
    expect_identical(store_append_test__state(empty)$k, "first")
})

test_that("append_new_rows rejects bad keys without partial insertion", {
    store <- store_append_test__store()
    dup <- store_append_test__rows(c("x", "x"))
    expect_error(priv(store)$append_new_rows("append_t", dup, "k"))
    expect_identical(nrow(store_append_test__state(store)), 0L)
    missing <- store_append_test__rows(c("y", NA))
    expect_error(priv(store)$append_new_rows("append_t", missing, "k"))
    expect_identical(nrow(store_append_test__state(store)), 0L)
})

test_that("append_new_rows handles factor keys and keys that need quoting", {
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
})

test_that("append_new_rows preserves results across multiple batches", {
    store <- store_append_test__store()
    stored <- sprintf("k%06d", seq(1L, 9000L, by = 2L))
    ddb_append_table(
        priv(store)$conn,
        "append_t",
        store_append_test__rows(stored, v = "seed")
    )
    # 4500 keys span three chunks; half of them already exist.
    keys <- sprintf("k%06d", 1:4500)
    priv(store)$append_new_rows("append_t", store_append_test__rows(keys), "k")
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
