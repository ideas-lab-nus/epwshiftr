# Exercise the same command-line boundary as CI with real coverage test packages.
root <- Sys.getenv(
    "COVERAGE_CHECK_OUTPUT",
    tempfile("coverage-supervisor-check-")
)
dir.create(root, showWarnings = FALSE)
entry <- normalizePath(".github/run-coverage.R", mustWork = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")

# Build one independent fixture package for each observable outcome.
coverage_check__fixture <- function(name, test) {
    pkg <- file.path(root, name)
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
    writeLines(
        c(
            "Package: supervisionfixture",
            "Version: 0.0.1",
            "Title: Coverage Supervision Fixture",
            "Description: Exercise bounded coverage and diagnostic preservation.",
            "License: MIT",
            "Author: Test",
            "Maintainer: Test <test@example.org>"
        ),
        file.path(pkg, "DESCRIPTION")
    )
    writeLines("export(value)", file.path(pkg, "NAMESPACE"))
    writeLines(
        c("# Exercise a real source line.", "value <- function() 42"),
        file.path(pkg, "R", "fixture.R")
    )
    entry <- readLines("tests/testthat.R", warn = FALSE)
    writeLines(
        gsub("epwshiftr", "supervisionfixture", entry, fixed = TRUE),
        file.path(pkg, "tests", "testthat.R")
    )
    writeLines(
        c('test_that("coverage outcome is retained", {', test, '})'),
        file.path(pkg, "tests", "testthat", "test-fixture-outcome.R")
    )
    pkg
}

# Run the entry point rather than mocking its process management implementation.
coverage_check__run <- function(name, test, timeout = 45) {
    pkg <- coverage_check__fixture(name, test)
    output <- file.path(root, paste0(name, "-output"))
    result <- processx::run(
        rscript,
        c("--vanilla", entry, pkg, output, timeout),
        env = c(
            "current",
            "EPWSHIFTR_COVERAGE_PROGRESS" = "2",
            "EPWSHIFTR_COVERAGE_STALL" = "6"
        ),
        timeout = 90,
        error_on_status = FALSE
    )
    if (result$status != 0L) {
        cat(result$stdout, result$stderr)
    }
    list(result = result, output = output)
}

success <- coverage_check__run("success", "stopifnot(identical(value(), 42))")
stopifnot(
    success$result$status == 0L,
    file.exists(file.path(success$output, "coverage-report", "coverage.rds")),
    identical(
        readRDS(file.path(success$output, "status.rds"))$status,
        "success"
    )
)
cat("PASS: successful real coverage retains its complete report and status.\n")

failure <- coverage_check__run("failure", 'stop("intentional fixture failure")')
stopifnot(
    failure$result$status != 0L,
    identical(
        readRDS(file.path(failure$output, "status.rds"))$status,
        "failure"
    ),
    file.exists(file.path(failure$output, "diagnostics", "processes.csv"))
)
cat(
    "PASS: a failing coverage child fails the command and preserves diagnostics.\n"
)

# The timeout must stop an owned grandchild while leaving an unrelated R alone.
coverage_check__timeout <- function() {
    sentinel <- callr::r_bg(function() Sys.sleep(90), supervise = TRUE)
    on.exit(sentinel$kill_tree(), add = TRUE)
    marker <- file.path(root, "owned-grandchild.rds")
    script <- file.path(root, "detached-worker.R")
    writeLines(
        c(
            "library(supervisionfixture)",
            sprintf(
                'saveRDS(list(pid = Sys.getpid(), created = ps::ps_create_time(ps::ps_handle())), %s)',
                encodeString(marker, quote = '"')
            ),
            "Sys.sleep(90)"
        ),
        script
    )
    hanging <- c(
        'stopifnot(identical(value(), 42))',
        sprintf(
            'launcher <- callr::r_bg(function(script, libraries) { worker <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("--vanilla", script), env = c("current", R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep)), cleanup = FALSE, cleanup_tree = FALSE); Sys.sleep(.3) }, args = list(%s, .libPaths()), supervise = FALSE)',
            encodeString(script, quote = '"')
        ),
        'repeat Sys.sleep(1)'
    )
    timed <- tryCatch(
        coverage_check__run("timeout", hanging, timeout = 20),
        finally = if (!file.exists(marker)) sentinel$kill_tree()
    )
    stopifnot(
        timed$result$status != 0L,
        file.exists(marker),
        identical(
            readRDS(file.path(timed$output, "status.rds"))$status,
            "timeout"
        ),
        sentinel$is_alive()
    )
    identity <- readRDS(marker)
    handle <- ps::ps_handle(identity$pid, identity$created)
    end <- Sys.time() + 5
    while (ps::ps_is_running(handle) && Sys.time() < end) {
        Sys.sleep(.05)
    }
    stopifnot(!ps::ps_is_running(handle))
    output_paths <- list.files(
        file.path(timed$output, "diagnostics"),
        pattern = "Rout",
        full.names = TRUE
    )
    output_lines <- unlist(lapply(output_paths, readLines, warn = FALSE))
    stopifnot(any(grepl("fixture-outcome:", output_lines, fixed = TRUE)))
    snapshots <- utils::read.csv(file.path(
        timed$output,
        "diagnostics",
        "processes.csv"
    ))
    if (.Platform$OS.type == "unix") {
        stopifnot(any(snapshots$pid == identity$pid & snapshots$parent == 1L))
    }
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
        stopifnot(
            length(list.files(
                file.path(timed$output, "diagnostics"),
                "^sample-"
            )) >
                0L
        )
    }
    cat(
        "PASS: a timed-out run retains native diagnostics and kills only its owned tree.\n"
    )
}
coverage_check__timeout()

# vim: fdm=marker :
