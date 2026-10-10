# Retain real timeout and both cancellation cleanup paths.

# private$collect_async_task() surfaces timeout errors and clears lifecycle state {{{
test_that("private$collect_async_task() surfaces timeout errors and clears lifecycle state", {
    path <- dataset_test__table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "simulate timeout",
        handler = function(urls, nc_handles) {
            Sys.sleep(0.3)
            TRUE
        },
        timeout = 0.05
    )

    expect_error(private$collect_async_task(task), "timed out")
    expect_identical(private$async_state, "timed_out")
    expect_null(private$async_task)
    expect_true(task$backend_released)
})
# }}}

# EsgDataset$close() best-effort cancels pending internal async work {{{
test_that("EsgDataset$close() best-effort cancels pending internal async work", {
    path <- dataset_test__table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    ds$open()
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "simulate cancellation",
        handler = function(urls, nc_handles) {
            Sys.sleep(5)
            TRUE
        },
        timeout = 10
    )

    ds$close()

    expect_false(ds$is_open)
    expect_true(task$cancellation_requested)
    expect_true(task$backend_released)
    expect_identical(private$async_state, "cancelled")
    expect_null(private$async_task)
})
# }}}

# private$cancel_async_task() keeps cancelled terminal state {{{
test_that("private$cancel_async_task() keeps cancelled terminal state", {
    path <- dataset_test__table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "cancel-race task",
        handler = function(urls, nc_handles) {
            Sys.sleep(5)
            TRUE
        },
        timeout = 10
    )

    # stop_mirai() is best-effort and may report FALSE when the dispatcher has
    # already delivered cancellation. The observable contract is the resolved
    # cancellation state checked below, not this timing-sensitive return value.
    mirai::stop_mirai(task$mirai_obj)
    while (mirai::unresolved(task$mirai_obj)) {
        Sys.sleep(0.01)
    }

    requested <- private$cancel_async_task(task = task, clear = TRUE)

    expect_false(requested)
    expect_identical(task$status, "cancelled")
    expect_true(inherits(task$error, "epwshiftr_async_cancelled"))
    expect_match(conditionMessage(task$error), "cancelled", fixed = TRUE)
    expect_identical(private$async_state, "cancelled")
    expect_true(task$backend_released)
    expect_null(private$async_task)
})
# }}}

# vim: fdm=marker :
