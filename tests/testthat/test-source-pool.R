# Exercise actual readers so stable process reuse, source exclusion and caller
# ownership cannot pass through a mocked scheduler implementation.
test_that("persistent readers reuse processes and serialize a physical source", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    pool <- source_pool(2L, timeout = 10)
    on.exit(pool$close(force = TRUE), add = TRUE)
    initial <- vapply(pool$status(), `[[`, integer(1L), "pid")
    owner <- Sys.getpid()
    values <- list()
    jobs <- list(
        list(source_key = "a", id = 1L),
        list(source_key = "a", id = 2L),
        list(source_key = "b", id = 3L)
    )
    read <- function(job) {
        start <- as.double(Sys.time())
        Sys.sleep(.1)
        list(
            id = job$id,
            pid = Sys.getpid(),
            start = start,
            end = as.double(Sys.time())
        )
    }
    collect <- function(job, value) {
        expect_identical(Sys.getpid(), owner)
        values[[as.character(job$id)]] <<- value
    }
    ticket <- pool$submit(jobs, read, collect)
    deadline <- as.double(Sys.time()) + 15
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(as.double(Sys.time()) < deadline)
        Sys.sleep(.01)
    }
    expect_identical(ticket$status, "done")
    expect_lte(values[["1"]]$end, values[["2"]]$start)
    expect_length(unique(vapply(values, `[[`, integer(1L), "pid")), 2L)
    ticket <- pool$submit(list(list(source_key = "c", id = 4L)), read, collect)
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(as.double(Sys.time()) < deadline)
        Sys.sleep(.01)
    }
    expect_true(values[["4"]]$pid %in% initial)
    expect_identical(vapply(pool$status(), `[[`, integer(1L), "pid"), initial)
})

# A resolved timeout is insufficient: the old OS process must be absent before
# the queued same-source request is allowed to run on its replacement.
test_that("a deadline retires the real reader before source reuse", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    pool <- source_pool(1L, timeout = .5)
    on.exit(pool$close(force = TRUE), add = TRUE)
    old <- ps::ps_handle(pool$status()[[1L]]$pid)
    failed <- NULL
    result <- NULL
    ticket <- pool$submit(
        list(
            list(source_key = "a", wait = 5),
            list(source_key = "a", wait = .01)
        ),
        function(job) {
            Sys.sleep(job$wait)
            Sys.getpid()
        },
        function(job, value) {
            result <<- value
        },
        on_error = function(job, error) {
            failed <<- error
        }
    )
    deadline <- as.double(Sys.time()) + 20
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(as.double(Sys.time()) < deadline)
        Sys.sleep(.01)
    }
    expect_s3_class(failed, "epwshiftr_source_timeout")
    expect_false(ps::ps_is_running(old))
    expect_identical(result, pool$status()[[1L]]$pid)
})

# Reader output must survive the real daemon/host boundary only when requested.
# A subprocess captures OS stdout/stderr without changing testthat's own sinks.
test_that("persistent reader output is opt-in and preserves process reuse", {
    expect_error(source_pool(1L, output = NA), "output")
    script <- tempfile(fileext = ".R")
    # Readers already require an installed package. The subprocess must load
    # that same namespace's library ahead of any older user installation.
    libraries <- shift_execution__library_paths()
    writeLines(
        c(
            paste0(
                ".libPaths(",
                paste(deparse(libraries), collapse = ""),
                ")"
            ),
            "library(epwshiftr)",
            "run <- function() {",
            "enabled <- identical(commandArgs(TRUE)[[1L]], 'yes')",
            "pool <- source_pool(1L, timeout = 5, output = enabled)",
            "on.exit(pool$close(force = TRUE), add = TRUE)",
            "worker <- pool$status()[[1L]]$pid",
            "worker_handle <- ps::ps_handle(worker)",
            "values <- integer()",
            "for (i in 1:2) {",
            "ticket <- pool$submit(list(list(source_key = 'generated')), function(job) {cat('EPWSHIFTR_READER_STDOUT\\n');message('EPWSHIFTR_READER_STDERR');Sys.getpid()}, function(job, value) {values <<- c(values, value)})",
            "deadline <- as.double(Sys.time()) + 10",
            "while (ticket$status == 'pending') {pool$poll();stopifnot(as.double(Sys.time()) < deadline);Sys.sleep(.01)}",
            "stopifnot(ticket$status == 'done')",
            "}",
            "stopifnot(length(values) == 2L, all(values == worker), worker != Sys.getpid())",
            "pool$close()",
            "stopifnot(!ps::ps_is_running(worker_handle))",
            "cat('OUTPUT_READER_REUSE_PASSED\\n')",
            "}",
            "run()"
        ),
        script
    )
    for (enabled in c(FALSE, TRUE)) {
        result <- processx::run(
            file.path(R.home("bin"), "Rscript"),
            c("--vanilla", script, if (enabled) "yes" else "no"),
            env = c(
                R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep)
            ),
            timeout = 45,
            cleanup_tree = TRUE,
            error_on_status = FALSE
        )
        expect_identical(result$status, 0L, info = result$stderr)
        expect_match(result$stdout, "OUTPUT_READER_REUSE_PASSED")
        expect_identical(
            grepl("EPWSHIFTR_READER_STDOUT", result$stdout, fixed = TRUE),
            enabled
        )
        expect_identical(
            grepl("EPWSHIFTR_READER_STDERR", result$stderr, fixed = TRUE),
            enabled
        )
    }
})

# The public Store method lets its caller publish data from reusable readers
# for two distinct physical sources and reuse completed output on a later call.
test_that("Store publishes pooled extraction in its calling process", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    fixture <- exchange_test__fixture()
    on.exit(lapply(fixture$stores, function(store) store$close()), add = TRUE)
    store <- fixture$stores[[1L]]
    pool <- source_pool(2L, timeout = 20)
    on.exit(pool$close(force = TRUE), add = TRUE)
    readers <- vapply(pool$status(), `[[`, integer(1L), "pid")
    expect_true(all(readers != Sys.getpid()))
    ids <- fixture$plans$plan_id
    ticket <- store$extract(ids, fallback = "error", pool = pool)
    expect_identical(ticket$status, "pending")
    expect_equal(nrow(store$query("SELECT * FROM extraction_result")), 0L)
    deadline <- Sys.time() + 25
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(Sys.time() < deadline)
        Sys.sleep(.01)
    }
    expect_identical(ticket$status, "done")
    expect_true(all(ticket$value$status == "done"))
    expect_setequal(ticket$value$plan_id, ids)
    results <- store$query("SELECT * FROM extraction_result ORDER BY plan_id")
    expect_equal(nrow(results), 4L)
    expect_true(all(store$validate()$checksum_ok))
    again <- store$extract(ids, fallback = "error", pool = pool)
    expect_identical(again$status, "done")
    expect_true(all(again$value$status == "done"))
    expect_identical(
        store$query("SELECT * FROM extraction_result ORDER BY plan_id"),
        results
    )
    expect_identical(vapply(pool$status(), `[[`, integer(1L), "pid"), readers)
})

# Generate local retained-transfer metadata without opening a remote source.
source_pool_test__progress_job <- function(mode, seconds = 1) {
    directory <- tempfile("pool-transfer-progress-")
    dir.create(directory)
    temporary <- file.path(directory, ".tmp")
    dir.create(temporary)
    list(
        source_key = basename(directory),
        mode = mode,
        seconds = seconds,
        file = list(title = "generated.nc"),
        http = if (mode == "native") {
            NULL
        } else {
            list(
                dest = directory,
                temp = temporary,
                manifest = file.path(directory, "generated-manifest.json")
            )
        }
    )
}

# This real worker writes bytes in small flushed increments, or deliberately
# stalls/rewrites the same bytes. The owner observes file sizes, never payloads.
source_pool_test__progress_read <- function(job) {
    if (job$mode == "native") {
        Sys.sleep(job$seconds)
        return(TRUE)
    }
    writeLines("generated-control", job$http$manifest)
    con <- file(file.path(job$http$temp, "generated.part"), "wb")
    on.exit(close(con))
    end <- as.double(Sys.time()) + job$seconds
    repeat {
        if (job$mode == "rewrite") {
            seek(con, 0, origin = "start")
        }
        writeBin(as.raw(1), con)
        flush(con)
        if (job$mode == "stall") {
            Sys.sleep(job$seconds)
        }
        if (as.double(Sys.time()) >= end) {
            break
        }
        Sys.sleep(.03)
    }
    TRUE
}
environment(source_pool_test__progress_read) <- baseenv()

# Bound the test independently of the reader's deliberately smaller deadline.
source_pool_test__progress_drain <- function(pool, ticket) {
    end <- as.double(Sys.time()) + 10
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(as.double(Sys.time()) < end)
        Sys.sleep(.01)
    }
}

test_that("growing HTTP transfers outlive the native deadline", {
    pool <- source_pool(1L, timeout = .2, download_timeout = 3)
    withr::defer(pool$close(force = TRUE))
    job <- source_pool_test__progress_job("grow")
    collected <- FALSE
    ticket <- pool$submit(
        list(job),
        source_pool_test__progress_read,
        function(job, value) collected <<- value
    )
    source_pool_test__progress_drain(pool, ticket)
    expect_identical(ticket$status, "done")
    expect_true(collected)
    expect_gt(file.info(file.path(job$http$temp, "generated.part"))$size, 1)
    expect_equal(pool$download_timeout, 3L)
})

test_that("stalls, rewrites and native reads keep their short deadline", {
    for (mode in c("stall", "rewrite", "native")) {
        pool <- source_pool(1L, timeout = .2, download_timeout = 3)
        job <- source_pool_test__progress_job(mode, seconds = 2)
        old <- ps::ps_handle(pool$status()[[1L]]$pid)
        ticket <- pool$submit(
            list(job),
            source_pool_test__progress_read,
            function(job, value) NULL
        )
        source_pool_test__progress_drain(pool, ticket)
        expect_identical(ticket$status, "failed", info = mode)
        expect_false(ps::ps_is_running(old), info = mode)
        expect_match(conditionMessage(ticket$error), "deadline", info = mode)
        pool$close()
    }
})

test_that("a growing HTTP transfer also has an absolute total limit", {
    pool <- source_pool(1L, timeout = .2, download_timeout = .6)
    withr::defer(pool$close(force = TRUE))
    job <- source_pool_test__progress_job("grow", seconds = 2)
    ticket <- pool$submit(
        list(job),
        source_pool_test__progress_read,
        function(job, value) NULL
    )
    source_pool_test__progress_drain(pool, ticket)
    expect_identical(ticket$status, "failed")
    expect_gt(file.info(file.path(job$http$temp, "generated.part"))$size, 1)
    expect_error(
        source_pool(1L, timeout = 2, download_timeout = 1),
        "download_timeout"
    )
    config <- store__pool_download_config(
        data.table::data.table(title = "x.nc"),
        tempdir(),
        1800L
    )
    expect_identical(config$node_policy$cooldown_seconds, 1L)
})

# vim: fdm=marker :
