# Preserve synchronous results and caller-owned handles across real async tasks.

# EsgDataset$open(async = TRUE) reports progress and keeps caller-owned handles {{{
test_that("EsgDataset$open(async = TRUE) reports progress and keeps caller-owned handles", {
    path <- dataset_test__table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    on.exit(unlink(path), add = TRUE)

    updates <- list()
    testthat::local_mocked_bindings(
        cli_progress_bar = function(...) "progress-id",
        cli_progress_update = function(id = NULL, set = NULL, ...) {
            updates[[length(updates) + 1L]] <<- list(id = id, set = set)
        },
        cli_progress_done = function(...) NULL,
        .package = "cli"
    )

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    returned <- ds$open(
        async = TRUE,
        timeout = DATASET_TEST_ASYNC_TIMEOUT,
        progress = TRUE
    )
    on.exit(ds$close(), add = TRUE)

    expect_equal(updates, list(list(id = "progress-id", set = 1L)))
    expect_identical(returned, ds)
    expect_true(ds$is_open)
    expect_identical(private$async_state, "completed")
    expect_null(private$async_task)
    expect_equal(
        as.numeric(ds$var_get(
            "tas",
            start = c(1L, 1L, 1L),
            count = c(2L, 1L, 1L),
            collapse = TRUE
        )),
        c(11, 12)
    )
})
# }}}

# EsgDataset$var_get(async = TRUE) matches sync results and keeps open-state checks {{{
test_that("EsgDataset$var_get(async = TRUE) matches sync results and keeps open-state checks", {
    path1 <- dataset_test__table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    path2 <- dataset_test__table_file(
        time_vals = c(3, 4, 5),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(21, 22, 23)
    )
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds_closed <- EsgDataset$new(path1)
    expect_error(
        ds_closed$var_get("tas", timeout = DATASET_TEST_ASYNC_TIMEOUT),
        "only supported"
    )

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    start <- c(1L, 1L, 1L)
    count <- c(2L, 1L, 1L)

    expect_equal(
        ds$var_get(
            "tas",
            start = start,
            count = count,
            index = 2L,
            collapse = TRUE,
            async = TRUE,
            timeout = DATASET_TEST_ASYNC_TIMEOUT
        ),
        ds$var_get(
            "tas",
            start = start,
            count = count,
            index = 2L,
            collapse = TRUE
        )
    )

    expect_true(ds$is_open)
})
# }}}

# EsgDataset$read_array(async = TRUE) matches sync results and keeps open-state checks {{{
test_that("EsgDataset$read_array(async = TRUE) matches sync results and keeps open-state checks", {
    path1 <- dataset_test__table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    path2 <- dataset_test__table_file(
        time_vals = c(3, 4, 5),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(21, 22, 23)
    )
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds_closed <- EsgDataset$new(path1)
    expect_error(
        ds_closed$read_array(
            "tas",
            async = TRUE,
            timeout = DATASET_TEST_ASYNC_TIMEOUT
        ),
        "not open"
    )

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    start <- c(1L, 1L, 1L)
    count <- c(2L, 1L, 1L)

    expect_equal(
        ds$read_array(
            "tas",
            start = start,
            count = count,
            collapse = FALSE,
            async = TRUE,
            timeout = DATASET_TEST_ASYNC_TIMEOUT
        ),
        ds$read_array("tas", start = start, count = count, collapse = FALSE)
    )

    expect_true(ds$is_open)
})
# }}}

# EsgDataset$read_data_table(async = TRUE) matches sync results and keeps open-state checks {{{
test_that("EsgDataset$read_data_table(async = TRUE) matches sync results and keeps open-state checks", {
    path1 <- dataset_test__table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    path2 <- dataset_test__table_file(
        time_vals = c(3, 4, 5),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(21, 22, 23)
    )
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    start <- c(1L, 1L, 1L)
    count <- c(2L, 1L, 1L)

    expect_equal(
        ds$read_data_table(
            "tas",
            start = start,
            count = count,
            rbind = TRUE,
            async = TRUE,
            timeout = DATASET_TEST_ASYNC_TIMEOUT
        ),
        ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    )

    expect_true(ds$is_open)
})
# }}}

# vim: fdm=marker :
