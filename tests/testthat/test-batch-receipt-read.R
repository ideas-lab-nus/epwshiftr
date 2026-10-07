# Force the non-atomic copy fallback and try reading while its destination is
# incomplete. The reader must respect publication ownership, not parse or retry
# the partial file. Zero lock timeout makes this contention check deterministic.
test_that("batch readers cannot enter an incomplete receipt publication", {
    root <- withr::local_tempdir()
    expected <- list(id = "receipt-test", status = "finished", pid = 123L)
    shift_batch_execution__job_write(root, expected)
    with_lock <- manifest_with_lock
    local_mocked_bindings(manifest_with_lock = function(path, expr, ...) {
        with_lock(path, expr, timeout = 0)
    })
    copy_file <- base::file.copy
    copied <- FALSE
    local_mocked_bindings(
        file.rename = function(...) FALSE,
        file.copy = function(from, to, ...) {
            writeLines('{"status":', to)
            expect_error(
                shift_batch_execution__job_read(root),
                "Manifest is locked"
            )
            copied <<- TRUE
            copy_file(from, to, ...)
        },
        .package = "base"
    )
    shift_batch_execution__job_write(root, expected)
    expect_true(copied)
    expect_identical(shift_batch_execution__job_read(root), expected)
    expect_false(dir.exists(manifest_lock_path(file.path(
        root,
        "batch-job.json"
    ))))
})

# Failed replacement must be explicit and release the receipt lock. A surviving
# previous receipt must remain readable, with no leaked temporary JSON files.
test_that("failed receipt publication reports failure and releases ownership", {
    root <- withr::local_tempdir()
    previous <- list(id = "previous", status = "finished")
    shift_batch_execution__job_write(root, previous)
    local_mocked_bindings(
        file.rename = function(...) FALSE,
        file.copy = function(...) FALSE,
        .package = "base"
    )
    expect_error(
        shift_batch_execution__job_write(
            root,
            list(id = "next", status = "queued")
        ),
        "Could not publish batch receipt"
    )
    expect_identical(shift_batch_execution__job_read(root), previous)
    expect_identical(list.files(root, all.files = FALSE), "batch-job.json")
})

# Missing state remains distinct from unreadable or malformed state. Parsing
# failures propagate immediately and must not leave ownership behind.
test_that("missing and corrupt batch receipts remain distinguishable", {
    root <- withr::local_tempdir()
    expect_null(shift_batch_execution__job_read(file.path(root, "absent")))
    expect_null(shift_batch_execution__job_read(root))
    writeLines('{"status":', file.path(root, "batch-job.json"))
    expect_error(shift_batch_execution__job_read(root), "parse error")
    expect_false(dir.exists(manifest_lock_path(file.path(
        root,
        "batch-job.json"
    ))))
})

# vim: fdm=marker :
