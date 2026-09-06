test_that("retry status resolution preserves defaults and command choices", {
    expect_identical(
        cli_retry__resolve_statuses(NULL, c("failed", "done")),
        "failed"
    )
    expect_identical(
        cli_retry__resolve_statuses("failed,done", c("failed", "done")),
        c("failed", "done")
    )
    expect_error(
        cli_retry__resolve_statuses("missing", c("failed", "done")),
        "--status must be one of: failed, done.",
        fixed = TRUE
    )
})


test_that("retry candidate preparation preserves preview and execution rules", {
    candidates <- data.table::data.table(
        id = 1:3,
        status = c("failed", "done", "failed")
    )

    preview <- cli_retry__prepare_candidates(candidates, "failed", FALSE)
    expect_false(preview$execute)
    expect_identical(preview$candidates$id, c(1L, 3L))
    expect_true(all(preview$candidates$dry_run))

    execution <- cli_retry__prepare_candidates(candidates, "done", TRUE)
    expect_true(execution$execute)
    expect_identical(execution$candidates$id, 2L)
    expect_false("dry_run" %in% names(execution$candidates))

    empty <- candidates[0L]
    prepared_empty <- cli_retry__prepare_candidates(empty, "failed", TRUE)
    expect_false(prepared_empty$execute)
    expect_identical(names(prepared_empty$candidates), names(empty))
})
