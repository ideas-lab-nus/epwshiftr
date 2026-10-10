# Run with Rscript .github/testing/tests/test-check-monitor-scheduling.R. Synthetic callr results
# and atomic receipt files exercise terminal scans without workers or sleeps.
source(".github/testing/check-parallel.R")

testthat::test_that("terminal and post-merge scans audit newly registered lifetimes", {
    root <- withr::local_tempdir()
    library <- file.path(root, "library")
    tests <- file.path(library, "epwshiftr", "tests", "testthat")
    dir.create(tests, recursive = TRUE)
    writeLines("", file.path(tests, "test-fixture.R"))
    script <- file.path(root, "coverage.R")
    writeLines("", script)
    runtime <- new.env(parent = globalenv())
    sys.source(".github/testing/check-parallel.R", runtime)
    scans <- 0L
    registered <- integer()
    trace_directory <- NULL
    # Creation-bound fake identities have no relationship to host processes.
    publish <- function(pid) {
        registered <<- c(registered, pid)
        saveRDS(
            list(
                pid = pid,
                create_time = as.POSIXct(pid, origin = "1970-01-01", tz = "UTC")
            ),
            file.path(trace_directory, paste0("process-", pid, ".rds"))
        )
    }
    runtime$proc.time <- function() {
        structure(
            c(0, 0, 0, 0, 0),
            names = c(
                "user.self",
                "sys.self",
                "elapsed",
                "user.child",
                "sys.child"
            ),
            class = "proc_time"
        )
    }
    runtime$checks__observe <- function(...) c(rss = 0, processes = 0)
    runtime$checks__monitor <- function(log = NULL, windows = FALSE) {
        checks__monitor(log, windows = FALSE)
    }
    real_receipts <- runtime$checks__receipts
    runtime$checks__receipts <- function(
        directories,
        registry,
        receipts,
        monitor
    ) {
        scans <<- scans + 1L
        # Normal scan, parent-complete scan and exit-entry scan have completed.
        # Publish a new lifetime immediately before the final ready decision.
        if (scans == 4L) {
            publish(90002L)
        }
        real_receipts(directories, registry, receipts, monitor)
    }
    runtime$coverage__merge <- function(prepared, trace_directories, timeout) {
        testthat::expect_true(90002L %in% registered)
        publish(90003L)
        structure(
            list(),
            trace_manifest = data.frame(
                pid = registered,
                create_time = as.POSIXct(
                    registered,
                    origin = "1970-01-01",
                    tz = "UTC"
                ),
                user = 0,
                system = 0,
                jit_level = compiler::enableJIT(-1),
                jit_env = Sys.getenv("R_ENABLE_JIT")
            )
        )
    }
    runtime$coverage__report <- function(coverage, path) invisible(NULL)
    testthat::local_mocked_bindings(
        r_bg = function(func, args, libpath, env, ...) {
            directory <- args[[1L]]
            trace_directory <<- file.path(directory, "traces")
            publish(90001L)
            saveRDS(
                data.frame(
                    passed = 1L,
                    failed = 0L,
                    error = FALSE,
                    skipped = 0L,
                    warning = 0L
                ),
                file.path(directory, "results.rds")
            )
            write.csv(
                data.frame(file = "test-fixture.R", elapsed = 0, cpu = 0),
                file.path(directory, "files.csv"),
                row.names = FALSE
            )
            list(
                is_alive = function() FALSE,
                get_result = function() {
                    list(
                        error = NULL,
                        pid = 90001L,
                        create_time = as.POSIXct(
                            90001,
                            origin = "1970-01-01",
                            tz = "UTC"
                        ),
                        timing = c(user.self = 0, sys.self = 0)
                    )
                }
            )
        },
        .package = "callr"
    )
    testthat::local_mocked_bindings(
        ps_is_running = function(...) FALSE,
        ps_kill = function(...) stop("Fixture attempted process termination."),
        .package = "ps"
    )
    output <- file.path(root, "run")
    result <- runtime$checks__run(
        library,
        output,
        shards = 1L,
        coverage = list(),
        coverage_script = script,
        timeout = 1
    )
    audit <- read.csv(file.path(output, "process-exit-audit.csv"))
    testthat::expect_identical(result$passed, 1L)
    testthat::expect_setequal(audit$pid, 90001:90003)
    testthat::expect_true(all(audit$exited))
    testthat::expect_true(all(audit$coverage_registered))
    testthat::expect_false(anyNA(audit$verified_at))
    testthat::expect_gte(scans, 7L)
    testthat::expect_identical(result$monitor_operations[["receipt_reads"]], 3L)
})

# vim: fdm=marker :
