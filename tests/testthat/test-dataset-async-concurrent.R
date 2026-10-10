# Keep two real callers and their independent nested async workers concurrent.

# concurrent dataset callers keep async open and read operations independent {{{
test_that("concurrent dataset callers keep async open and read operations independent", {
    skip_on_cran()

    paths <- c(
        dataset_test__table_file(
            time_vals = c(0, 1, 2),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(11, 12, 13)
        ),
        dataset_test__table_file(
            time_vals = c(3, 4, 5),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(21, 22, 23)
        )
    )
    on.exit(unlink(paths), add = TRUE)

    results <- dataset_test__lapply(
        seq_along(paths),
        function(i, paths, start, count) {
            ds <- EsgDataset$new(paths[[i]])
            private <- ds$.__enclos_env__$private
            on.exit(ds$close(), add = TRUE)

            ds$open(async = TRUE, timeout = DATASET_TEST_ASYNC_TIMEOUT)

            opened <- list(
                is_open = ds$is_open,
                async_state = private$async_state,
                async_task_is_null = is.null(private$async_task),
                values = as.numeric(ds$var_get("tas", collapse = TRUE))
            )

            # Use a fresh dataset so synchronous open preserves the original
            # caller-owned handles and idle state before the async read task.
            ds$close()
            ds <- EsgDataset$new(paths[[i]])
            private <- ds$.__enclos_env__$private
            ds$open()
            initial_async_state <- private$async_state
            async_values <- as.numeric(ds$var_get(
                "tas",
                start = start,
                count = count,
                collapse = TRUE,
                async = TRUE,
                timeout = DATASET_TEST_ASYNC_TIMEOUT
            ))
            read <- list(
                initial_async_state = initial_async_state,
                is_open = ds$is_open,
                async_state = private$async_state,
                async_task_is_null = is.null(private$async_task),
                async_values = async_values,
                sync_values = as.numeric(ds$var_get(
                    "tas",
                    start = start,
                    count = count,
                    collapse = TRUE
                ))
            )
            list(opened = opened, read = read)
        },
        paths = paths,
        start = c(2L, 1L, 1L),
        count = c(2L, 1L, 1L)
    )

    expect_false(any(vapply(results, inherits, logical(1L), "try-error")))
    opened <- lapply(results, `[[`, "opened")
    read <- lapply(results, `[[`, "read")
    for (caller in read) {
        expect_identical(caller$initial_async_state, "idle")
    }
    # Check each phase before considering value equivalence; sharing callers
    # must not hide an uncleared task or closed handle after either operation.
    for (phase in list(opened, read)) {
        expect_true(all(vapply(phase, `[[`, logical(1L), "is_open")))
        expect_true(all(vapply(phase, `[[`, logical(1L), "async_task_is_null")))
        expect_equal(
            vapply(phase, `[[`, character(1L), "async_state"),
            rep("completed", 2L)
        )
    }
    expect_equal(
        lapply(opened, `[[`, "values"),
        list(c(11, 12, 13), c(21, 22, 23))
    )
    expect_equal(
        lapply(read, `[[`, "async_values"),
        list(c(12, 13), c(22, 23))
    )
    expect_equal(
        lapply(read, `[[`, "async_values"),
        lapply(read, `[[`, "sync_values")
    )
})
# }}}

# vim: fdm=marker :
