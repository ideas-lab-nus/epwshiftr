# Preserve failure draining, dispatch limits and independent source completion.

test_that("fatal source failures drain active work without dispatching more", {
    withr::local_options(epwshiftr.mirai_workers = 2L)
    paths <- file.path(
        tempdir(),
        paste0("source-pool-", Sys.getpid(), "-", 1:3)
    )
    on.exit(unlink(paths), add = TRUE)
    jobs <- Map(
        function(index, path) list(index = index, path = path),
        1:3,
        paths
    )
    expect_error(
        source__apply(
            jobs,
            function(job) {
                if (job$index == 1L) {
                    stop("source failed")
                }
                Sys.sleep(0.1)
                file.create(job$path)
            },
            function(job, value) NULL
        ),
        "source failed"
    )
    expect_true(file.exists(paths[[2L]]))
    expect_false(file.exists(paths[[3L]]))
    # A synchronous single-task read must remain usable after failure cleanup.
    value <- NULL
    source__apply(list(1), identity, function(job, result) value <<- result)
    expect_identical(value, 1)
})

test_that("collector failures drain readers and stop dispatching", {
    withr::local_options(epwshiftr.mirai_workers = 2L)
    root <- withr::local_tempdir()
    jobs <- lapply(1:3, function(index) list(index = index, root = root))
    expect_error(
        source__apply(
            jobs,
            function(job) {
                if (job$index == 1L) {
                    deadline <- Sys.time() + 10
                    while (
                        !file.exists(file.path(job$root, "started-2")) &&
                            Sys.time() < deadline
                    ) {
                        Sys.sleep(0.01)
                    }
                }
                file.create(file.path(job$root, paste0("started-", job$index)))
                if (job$index == 2L) {
                    Sys.sleep(0.3)
                }
                file.create(file.path(job$root, paste0("done-", job$index)))
            },
            function(job, result) {
                stop("persist conflict")
            }
        ),
        "persist conflict"
    )
    expect_true(file.exists(file.path(root, "done-2")))
    expect_false(file.exists(file.path(root, "started-3")))
})

# The no-reporter, one-worker route is deliberately synchronous; its exception
# handling is separate from the mirai callback path checked in test-source-read.
test_that("serial file isolation callbacks allow independent sources to finish", {
    withr::local_options(epwshiftr.mirai_workers = 1L)
    owner <- Sys.getpid()
    done <- integer()
    failed <- integer()
    source__apply(
        as.list(1:3),
        function(job) {
            expect_identical(Sys.getpid(), owner)
            if (job == 1L) {
                stop("unavailable source")
            }
            job
        },
        function(job, value) done <<- c(done, job),
        on_error = function(job, error) failed <<- c(failed, job)
    )
    expect_setequal(done, 2:3)
    expect_identical(failed, 1L)
})

# vim: fdm=marker :
