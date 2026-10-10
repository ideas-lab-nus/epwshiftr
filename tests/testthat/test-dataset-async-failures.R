# Verify failed async operations and partial handle transfers leave usable state.

# EsgDataset$open(async = TRUE) failures leave the dataset closed and clean {{{
test_that("EsgDataset$open(async = TRUE) failures leave the dataset closed and clean", {
    path <- tempfile(fileext = ".nc")
    if (file.exists(path)) {
        unlink(path)
    }

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    expect_error(
        ds$open(async = TRUE, timeout = DATASET_TEST_ASYNC_TIMEOUT),
        "Failed to open OPeNDAP connection"
    )

    expect_false(ds$is_open)
    expect_identical(private$async_state, "failed")
    expect_null(private$async_task)
    expect_true(all(vapply(private$nc_handles, is.null, logical(1L))))
})
# }}}

# dataset__detach_handles() / dataset__adopt_handles() transfer partially opened handles {{{
test_that("dataset__detach_handles() / dataset__adopt_handles() transfer partially opened handles", {
    path_opened <- dataset_test__table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12)
    )
    path_pending <- dataset_test__table_file(
        time_vals = c(2, 3),
        time_units = "days since 2000-01-03 00:00:00",
        tas_vals = c(13, 14)
    )
    on.exit(unlink(c(path_opened, path_pending)), add = TRUE)

    expect_error(
        EsgDataset$new(path_opened, nc_handles = list(NULL)),
        "unused argument"
    )

    source <- EsgDataset$new(path_opened)
    source$open()
    handles <- dataset__detach_handles(source)
    on.exit(dataset__close_handles(path_opened, handles), add = TRUE)
    source_private <- source$.__enclos_env__$private

    expect_false(source$is_open)
    expect_true(all(vapply(source_private$nc_handles, is.null, logical(1L))))

    ds <- EsgDataset$new(c(path_opened, path_pending))
    dataset__adopt_handles(ds, list(handles[[1L]], NULL))
    handles <- vector("list", length(handles))
    private <- ds$.__enclos_env__$private
    on.exit(ds$close(), add = TRUE)

    expect_false(ds$is_open)
    expect_false(is.null(private$nc_handles[[1L]]))
    expect_null(private$nc_handles[[2L]])

    ds$open()

    expect_true(ds$is_open)
    expect_false(is.null(private$nc_handles[[1L]]))
    expect_false(is.null(private$nc_handles[[2L]]))
    expect_equal(as.numeric(ds$var_get("tas", index = 1L)), c(11, 12))
    expect_equal(as.numeric(ds$var_get("tas", index = 2L)), c(13, 14))

    ds$close()
    expect_false(ds$is_open)
    expect_true(all(vapply(private$nc_handles, is.null, logical(1L))))

    missing_path <- tempfile(fileext = ".nc")
    if (file.exists(missing_path)) {
        unlink(missing_path)
    }
    failing_source <- EsgDataset$new(path_opened)
    failing_source$open()
    failing_handles <- dataset__detach_handles(failing_source)
    failing <- EsgDataset$new(c(path_opened, missing_path))
    dataset__adopt_handles(failing, list(failing_handles[[1L]], NULL))
    failing_handles <- vector("list", length(failing_handles))
    failing_private <- failing$.__enclos_env__$private

    expect_error(failing$open(), "Failed to open OPeNDAP connection")
    expect_false(failing$is_open)
    expect_true(all(vapply(failing_private$nc_handles, is.null, logical(1L))))
})
# }}}

# EsgDataset$var_get(async = TRUE) failures clear task state and keep sync handles usable {{{
test_that("EsgDataset$var_get(async = TRUE) failures clear task state and keep sync handles usable", {
    path <- dataset_test__table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)
    private <- ds$.__enclos_env__$private

    expect_error(
        ds$var_get(
            "missing_var",
            async = TRUE,
            timeout = DATASET_TEST_ASYNC_TIMEOUT
        ),
        "Failed to read variable data"
    )

    expect_true(ds$is_open)
    expect_identical(private$async_state, "failed")
    expect_null(private$async_task)
    expect_equal(as.numeric(ds$var_get("tas", collapse = TRUE)), c(11, 12, 13))
})
# }}}

# vim: fdm=marker :
