# Run with Rscript .github/testing/tests/test-check-runner-boundaries.R. Fake callr processes
# exercise the runner's terminal phases without workers, sockets or timed sleeps.
source(".github/testing/check-parallel.R")

# Run the real supervisor around one durable fake shard and a controllable clock.
checks_test__run_fixture <- function(
    merge_elapsed = 0,
    report_elapsed = 0,
    jit_level = compiler::enableJIT(-1),
    jit_env = Sys.getenv("R_ENABLE_JIT")
) {
    root <- withr::local_tempdir()
    library <- file.path(root, "library")
    tests <- file.path(library, "epwshiftr", "tests", "testthat")
    dir.create(tests, recursive = TRUE)
    writeLines("", file.path(tests, "test-fixture.R"))
    script <- file.path(root, "coverage.R")
    writeLines("", script)
    clock <- new.env(parent = emptyenv())
    clock$elapsed <- 0
    clock$budget <- NULL
    clock$reported <- FALSE
    clock$env <- NULL
    create_time <- ps::ps_create_time(ps::ps_handle())
    pid <- Sys.getpid()
    runtime <- new.env(parent = globalenv())
    sys.source(".github/testing/check-parallel.R", runtime)
    runtime$proc.time <- function() {
        structure(
            c(0, 0, clock$elapsed, 0, 0),
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
    runtime$checks__track <- function(handle, registry) {
        identity <- checks__identity(pid, create_time)
        registry[[identity]] <- list(
            handle = handle,
            pid = pid,
            create_time = create_time,
            cpu = 0,
            rss = 0,
            exited = TRUE,
            verified_at = Sys.time()
        )
        invisible(identity)
    }
    runtime$checks__receipts <- function(
        directories,
        registry,
        receipts,
        monitor
    ) {
        receipts$fixture <- list(pid = pid, create_time = create_time)
        invisible(1L)
    }
    runtime$coverage__merge <- function(prepared, trace_directories, timeout) {
        clock$budget <- timeout
        clock$elapsed <- clock$elapsed + merge_elapsed
        structure(
            list(),
            trace_manifest = data.frame(
                pid = pid,
                create_time = create_time,
                user = 0,
                system = 0,
                jit_level = jit_level,
                jit_env = jit_env
            )
        )
    }
    runtime$coverage__report <- function(coverage, path) {
        clock$reported <- TRUE
        clock$elapsed <- clock$elapsed + report_elapsed
        invisible(NULL)
    }
    testthat::local_mocked_bindings(
        r_bg = function(func, args, libpath, env, ...) {
            clock$env <- env
            directory <- args[[1L]]
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
                        pid = pid,
                        create_time = create_time,
                        timing = c(user.self = 0, sys.self = 0)
                    )
                }
            )
        },
        .package = "callr"
    )
    # Never let fixture cleanup inspect or signal the real test process.
    testthat::local_mocked_bindings(
        ps_is_running = function(...) FALSE,
        ps_kill = function(...) stop("Fixture attempted process termination."),
        .package = "ps"
    )
    result <- tryCatch(
        runtime$checks__run(
            library,
            file.path(root, "run"),
            shards = 1L,
            coverage = list(),
            coverage_script = script,
            timeout = 1
        ),
        error = identity
    )
    list(
        result = result,
        budget = clock$budget,
        reported = clock$reported,
        env = clock$env
    )
}

testthat::test_that("ready processes cannot satisfy an expired run deadline", {
    registry <- new.env(parent = emptyenv())
    monitor <- checks__monitor()
    testthat::expect_error(
        checks__exit_ready(registry, monitor, elapsed = 2, timeout = 1),
        "timed out during process exit verification"
    )
    testthat::expect_true(checks__exit_ready(
        registry,
        monitor,
        elapsed = 0,
        timeout = 1
    ))
})

testthat::test_that("merge and report must finish inside the shared deadline", {
    merged <- checks_test__run_fixture(merge_elapsed = 2)
    testthat::expect_s3_class(merged$result, "error")
    testthat::expect_match(
        conditionMessage(merged$result),
        "timed out during coverage merge"
    )
    testthat::expect_identical(merged$budget, 1)
    testthat::expect_false(merged$reported)
    reported <- checks_test__run_fixture(report_elapsed = 2)
    testthat::expect_s3_class(reported$result, "error")
    testthat::expect_match(
        conditionMessage(reported$result),
        "timed out during coverage report"
    )
    testthat::expect_true(reported$reported)
    completed <- checks_test__run_fixture(
        merge_elapsed = .2,
        report_elapsed = .3
    )
    testthat::expect_identical(completed$result$passed, 1L)
    testthat::expect_equal(completed$result$elapsed, .5)
})

testthat::test_that("parallel workers retain explicit live opt-ins and original defaults", {
    flags <- c("EPWSHIFTR_RUN_LIVE_ESGF", "EPWSHIFTR_RUN_LIVE_ERA5")
    withr::local_envvar(stats::setNames(c(NA_character_, NA_character_), flags))
    disabled <- checks_test__run_fixture()
    testthat::expect_identical(unname(disabled$env[flags]), c("false", "false"))
    for (values in list(
        c("true", "yes"),
        c("1", "false"),
        c("FALSE", "TRUE")
    )) {
        withr::with_envvar(stats::setNames(values, flags), {
            enabled <- checks_test__run_fixture()
            testthat::expect_identical(unname(enabled$env[flags]), values)
            testthat::expect_identical(enabled$result$passed, 1L)
        })
    }
})

testthat::test_that("explicit startup JIT is checked before reporting coverage", {
    withr::local_envvar(c(R_ENABLE_JIT = "3"))
    testthat::local_mocked_bindings(
        enableJIT = function(level) {
            stopifnot(identical(level, -1))
            3L
        },
        .package = "compiler"
    )
    matched <- checks_test__run_fixture(jit_level = 3L, jit_env = "3")
    testthat::expect_identical(matched$result$jit_level, 3L)
    testthat::expect_identical(matched$result$jit_env, "3")
    testthat::expect_true(matched$reported)
    for (observed in list(list(0L, "3"), list(3L, "0"))) {
        mismatch <- checks_test__run_fixture(
            jit_level = observed[[1L]],
            jit_env = observed[[2L]]
        )
        testthat::expect_s3_class(mismatch$result, "error")
        testthat::expect_match(
            conditionMessage(mismatch$result),
            "JIT observations do not match"
        )
        testthat::expect_false(mismatch$reported)
    }
    testthat::expect_error(
        checks__validate_jit(
            data.frame(jit_level = 3L, jit_env = "3"),
            0L,
            "3"
        ),
        "JIT observations"
    )
    for (level in 0:3) {
        value <- as.character(level)
        testthat::expect_null(checks__validate_jit(
            data.frame(jit_level = level, jit_env = value),
            level,
            value
        ))
    }
    # No explicit recognized startup policy means record, without forcing defaults.
    for (value in c("", "other")) {
        withr::with_envvar(c(R_ENABLE_JIT = value), {
            result <- checks_test__run_fixture(jit_level = 0L, jit_env = "")
            testthat::expect_identical(result$result$jit_env, value)
            testthat::expect_true(result$reported)
        })
    }
})

# vim: fdm=marker :
