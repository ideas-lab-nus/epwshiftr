# Exercise the CI-only coverage adapter against a tiny installed package. This
# stays outside package tests because it deliberately invokes R CMD INSTALL.
# Run with: uvr run tools/test-coverage-adapter.R -- /path/to/package
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else "."
source(file.path(root, ".github", "coverage.R"), local = TRUE)

testthat::test_that("prepared shards preserve counters and reject incomplete receipts", {
    directory <- withr::local_tempdir()
    package <- file.path(directory, "source")
    dir.create(package)
    dir.create(file.path(package, "R"))
    writeLines(
        c(
            "Package: coveragetoy",
            "Version: 0.0.1",
            "Title: Coverage Adapter Fixture",
            "Description: Minimal fixture for coverage adapter validation.",
            'Authors@R: person("Test", "Author", email="test@example.org", role=c("aut", "cre"))',
            "License: MIT"
        ),
        file.path(package, "DESCRIPTION")
    )
    writeLines("export(covered, uncovered)", file.path(package, "NAMESPACE"))
    writeLines(
        c(
            "# A branch called by independent test processes.",
            "covered <- function(x) {",
            "    if (x) 1 else 2",
            "}",
            "# An uncalled function retains zero coverage after merging.",
            "uncovered <- function() {",
            "    42",
            "}"
        ),
        file.path(package, "R", "toy.R")
    )
    prepared <- coverage__prepare(package, file.path(directory, "prepared"))
    descriptor <- readRDS(prepared)
    testthat::expect_true(file.exists(descriptor$parse_data))
    parser <- readRDS(descriptor$parse_data)
    testthat::expect_named(
        parser,
        c("version", "package", "r_version", "covr_version", "data")
    )
    # Parser caches may retain source-file environments, but never execution
    # closures, counters, namespace parents or other process-specific state.
    testthat::expect_true(all(vapply(
        parser$data,
        function(data) {
            source <- attr(data, "srcfile")
            is.data.frame(data) &&
                is.environment(source) &&
                identical(parent.env(source), emptyenv()) &&
                all(vapply(
                    as.list.environment(source, all.names = TRUE),
                    is.atomic,
                    logical(1L)
                ))
        },
        logical(1L)
    )))
    invalid_parser <- parser
    invalid_parser$r_version <- "0.0.0"
    invalid_parser_path <- file.path(directory, "invalid-parse-data.rds")
    saveRDS(invalid_parser, invalid_parser_path)
    testthat::expect_error(
        coverage__load_parse_data(invalid_parser_path, descriptor$package),
        "does not match"
    )
    source_map <- readRDS(descriptor$source_map)
    testthat::expect_identical(source_map$identity$covr_version, "3.6.5")
    testthat::expect_named(source_map$identity$source, c("rdb", "rdx"))
    # A recursive type check excludes environments, closures and pointers even
    # in attributes; the artifact contains coordinates and identity strings only.
    pure_data <- function(x) {
        if (!(is.atomic(x) || is.list(x) || is.null(x))) {
            return(FALSE)
        }
        children <- if (is.list(x)) x else list()
        attrs <- attributes(x)
        all(vapply(c(children, attrs), pure_data, logical(1L)))
    }
    testthat::expect_true(pure_data(source_map))

    # Validate boundary failures without instrumenting another namespace. The
    # mock also proves that empty mappings fall back to normal traversal and
    # that a failing traversal restores the original locked covr binding.
    original_imputer <- get("impute_srcref", asNamespace("covr"))
    boundary <- source_map
    boundary$identity$package <- "base"
    boundary_path <- file.path(directory, "boundary-source-map.rds")
    check_boundary <- function(payload, load_parse_data = NULL) {
        saveRDS(payload, boundary_path)
        coverage__trace_environment(
            asNamespace("base"),
            boundary_path,
            source_map$identity$source,
            load_parse_data = load_parse_data
        )
    }
    for (field in c(
        "package",
        "r_version",
        "covr_version",
        "imputer",
        "source"
    )) {
        invalid_map <- boundary
        invalid_map$identity[[field]] <- "different"
        testthat::expect_error(check_boundary(invalid_map), "do not match")
    }
    invalid_map <- boundary
    invalid_map$data[[1L]] <- list(value = list(new.env()))
    testthat::expect_error(check_boundary(invalid_map), "non-coordinate")
    invalid_map$data[[1L]] <- list(value = list(rep(NA_integer_, 8L)))
    testthat::expect_error(check_boundary(invalid_map), "non-coordinate")
    invalid_map$data[[1L]] <- c(value = "invalid")
    testthat::expect_error(check_boundary(invalid_map), "non-coordinate")
    local({
        testthat::local_mocked_bindings(
            trace_environment = function(env) stop("forced traversal failure"),
            .package = "covr"
        )
        testthat::expect_error(check_boundary(boundary), "forced traversal")
        testthat::expect_identical(
            get("impute_srcref", asNamespace("covr")),
            original_imputer
        )
    })
    local({
        testthat::local_mocked_bindings(
            trace_environment = function(env) invisible(NULL),
            .package = "covr"
        )
        boundary$data <- list()
        testthat::expect_null(check_boundary(boundary))
        testthat::expect_identical(
            get("impute_srcref", asNamespace("covr")),
            original_imputer
        )
        testthat::expect_true(bindingIsLocked(
            "impute_srcref",
            asNamespace("covr")
        ))
    })
    # Parser data stays cold on a map hit. A real miss reads it once, and both
    # temporary covr bindings must be restored even if reading the cache fails.
    local({
        testthat::local_mocked_bindings(
            trace_environment = function(env) invisible(NULL),
            .package = "covr"
        )
        original_parser <- get("get_parse_data", asNamespace("covr"))
        original_parser_locked <- bindingIsLocked(
            "get_parse_data",
            asNamespace("covr")
        )
        testthat::expect_null(check_boundary(boundary, function() {
            stop("parser cache must remain unread")
        }))
        testthat::expect_identical(
            get("get_parse_data", asNamespace("covr")),
            original_parser
        )
    })
    local({
        testthat::local_mocked_bindings(
            trace_environment = function(env) {
                get("get_parse_data", asNamespace("covr"))(NULL)
                get("get_parse_data", asNamespace("covr"))(NULL)
                invisible(NULL)
            },
            get_parse_data = function(srcfile) TRUE,
            .package = "covr"
        )
        original_parser <- get("get_parse_data", asNamespace("covr"))
        original_parser_locked <- bindingIsLocked(
            "get_parse_data",
            asNamespace("covr")
        )
        reads <- 0L
        testthat::expect_null(check_boundary(boundary, function() {
            reads <<- reads + 1L
        }))
        testthat::expect_identical(reads, 1L)
        testthat::expect_identical(
            get("get_parse_data", asNamespace("covr")),
            original_parser
        )
        testthat::expect_error(
            check_boundary(boundary, function() {
                stop("forced parser load failure")
            }),
            "forced parser load failure"
        )
        testthat::expect_identical(
            get("get_parse_data", asNamespace("covr")),
            original_parser
        )
        testthat::expect_identical(
            get("impute_srcref", asNamespace("covr")),
            original_imputer
        )
        testthat::expect_identical(
            bindingIsLocked("get_parse_data", asNamespace("covr")),
            original_parser_locked
        )
        testthat::expect_true(bindingIsLocked(
            "impute_srcref",
            asNamespace("covr")
        ))
    })
    template <- readRDS(descriptor$template)
    testthat::expect_true(all(
        vapply(template, function(x) x$value, numeric(1L)) == 0
    ))
    testthat::expect_error(
        coverage__prepare(package, file.path(directory, "prepared")),
        "empty directory"
    )
    shards <- file.path(directory, c("shard-1", "shard-2"))
    # Each shard uses a fresh R session and independent trace destination, while
    # reusing only the immutable instrumented installation from preparation.
    for (index in seq_along(shards)) {
        dir.create(shards[[index]])
        callr::r(
            function(library, value) {
                original <- get("impute_srcref", asNamespace("covr"))
                original_parser <- get("get_parse_data", asNamespace("covr"))
                original_parser_locked <- bindingIsLocked(
                    "get_parse_data",
                    asNamespace("covr")
                )
                library(coveragetoy, lib.loc = library)
                stopifnot(identical(
                    original,
                    get("impute_srcref", asNamespace("covr"))
                ))
                stopifnot(
                    identical(
                        original_parser,
                        get("get_parse_data", asNamespace("covr"))
                    ),
                    length(get("package_parse_data", asNamespace("covr"))) == 0L
                )
                coveragetoy::covered(value)
                # One shard also loads the same instrumented package in a
                # child process, exercising inherited worker trace ownership.
                if (value) {
                    callr::r(
                        function(library) {
                            library(coveragetoy, lib.loc = library)
                            coveragetoy::covered(FALSE)
                        },
                        args = list(library = library),
                        libpath = .libPaths(),
                        env = c(
                            COVERAGE_DIR = Sys.getenv("COVERAGE_DIR"),
                            R_COVR = "true",
                            R_ENABLE_JIT = "0"
                        )
                    )
                }
            },
            args = list(library = descriptor$library, value = index == 1L),
            libpath = c(descriptor$library, .libPaths()),
            env = c(
                COVERAGE_DIR = shards[[index]],
                R_COVR = "true",
                R_ENABLE_JIT = "0"
            )
        )
    }
    # The numeric handoff preserves every zero/count and receipt while legacy
    # callers continue receiving fully populated covr counter records.
    metadata <- readRDS(descriptor$metadata)
    legacy <- coverage__collect(shards[[1L]], metadata, timeout = 0)
    numeric <- coverage__collect(
        shards[[1L]],
        metadata,
        timeout = 0,
        prepared_identity = coverage__source_identity(metadata),
        values_only = TRUE
    )
    testthat::expect_null(numeric$counters)
    testthat::expect_identical(numeric$counts, legacy$counts)
    testthat::expect_identical(
        numeric$counts,
        vapply(legacy$counters, function(counter) counter$value, numeric(1L))
    )
    testthat::expect_identical(numeric$manifest, legacy$manifest)
    coverage <- coverage__merge(prepared, shards, timeout = 0)
    tally <- covr::tally_coverage(coverage, by = "line")
    template_tally <- covr::tally_coverage(template, by = "line")
    testthat::expect_identical(names(coverage), names(template))
    testthat::expect_equal(nrow(tally), nrow(template_tally))
    testthat::expect_equal(tally$value[tally$functions == "uncovered"], 0)
    testthat::expect_true(all(tally$value[tally$functions == "covered"] > 0))
    testthat::expect_equal(nrow(attr(coverage, "trace_manifest")), 3L)
    manifest <- attr(coverage, "trace_manifest")
    testthat::expect_identical(manifest$jit_level, rep(0L, 3L))
    testthat::expect_identical(manifest$jit_env, rep("0", 3L))
    # Compare every actual registration with its collected manifest row.
    for (index in seq_len(nrow(manifest))) {
        receipt <- readRDS(file.path(
            manifest$directory[[index]],
            paste0("process-", manifest$pid[[index]], ".rds")
        ))
        testthat::expect_identical(
            receipt$jit_level,
            manifest$jit_level[[index]]
        )
        testthat::expect_identical(receipt$jit_env, manifest$jit_env[[index]])
    }

    for (shard in shards) {
        resource <- list.files(
            shard,
            pattern = "^resource-[0-9]+.rds$",
            full.names = TRUE
        )
        testthat::expect_length(resource, if (shard == shards[[1L]]) 2L else 1L)
        for (receipt in resource) {
            usage <- readRDS(receipt)
            testthat::expect_named(usage, c("pid", "user", "system", "elapsed"))
            testthat::expect_true(all(unlist(usage) >= 0))
        }
    }
    # Remove mappings only in this disposable fixture. The original parser
    # fallback must reproduce every key, owner and canonical execution count.
    local({
        missing_map <- source_map
        missing_map$data <- list()
        saveRDS(missing_map, descriptor$source_map)
        withr::defer(saveRDS(source_map, descriptor$source_map))
        fallback_shard <- file.path(directory, "fallback-shard")
        dir.create(fallback_shard)
        restored <- callr::r(
            function(library) {
                namespace <- asNamespace("covr")
                parser <- get("get_parse_data", namespace)
                imputer <- get("impute_srcref", namespace)
                library(coveragetoy, lib.loc = library)
                stopifnot(length(get("package_parse_data", namespace)) > 0L)
                coveragetoy::covered(TRUE)
                coveragetoy::covered(FALSE)
                coveragetoy::covered(FALSE)
                list(
                    parser = identical(
                        parser,
                        get("get_parse_data", namespace)
                    ),
                    imputer = identical(
                        imputer,
                        get("impute_srcref", namespace)
                    )
                )
            },
            args = list(library = descriptor$library),
            libpath = c(descriptor$library, .libPaths()),
            env = c(COVERAGE_DIR = fallback_shard, R_COVR = "true")
        )
        testthat::expect_identical(
            restored,
            list(parser = TRUE, imputer = TRUE)
        )
        fallback <- coverage__merge(prepared, fallback_shard, timeout = 0)
        testthat::expect_identical(names(fallback), names(coverage))
        testthat::expect_identical(
            covr::tally_coverage(fallback, by = "line"),
            tally
        )
        # OnLoad hook errors can be reported while namespace loading continues.
        # The unchanged strict collector must reject those incomplete traces,
        # and failed cache reads must not leave covr bindings replaced.
        for (fault in c("missing", "corrupt", "identity")) {
            local({
                unlink(descriptor$parse_data)
                withr::defer(saveRDS(
                    parser,
                    descriptor$parse_data,
                    compress = FALSE
                ))
                if (fault == "corrupt") {
                    writeLines("invalid RDS", descriptor$parse_data)
                } else if (fault == "identity") {
                    saveRDS(invalid_parser, descriptor$parse_data)
                }
                bad_shard <- file.path(directory, paste0("parser-", fault))
                dir.create(bad_shard)
                restored <- callr::r(
                    function(library) {
                        namespace <- asNamespace("covr")
                        parser <- get("get_parse_data", namespace)
                        imputer <- get("impute_srcref", namespace)
                        library(coveragetoy, lib.loc = library)
                        list(
                            parser = identical(
                                parser,
                                get("get_parse_data", namespace)
                            ),
                            imputer = identical(
                                imputer,
                                get("impute_srcref", namespace)
                            )
                        )
                    },
                    args = list(library = descriptor$library),
                    libpath = c(descriptor$library, .libPaths()),
                    env = c(COVERAGE_DIR = bad_shard, R_COVR = "true"),
                    stderr = file.path(bad_shard, "stderr.log")
                )
                testthat::expect_identical(
                    restored,
                    list(parser = TRUE, imputer = TRUE)
                )
                testthat::expect_error(
                    coverage__merge(prepared, bad_shard, timeout = 0),
                    "metadata|keys|trace"
                )
            })
        }
    })
    # The public opt-out preserves the unmodified covr instrumentation path.
    # Execute the same branches and compare every canonical line and count.
    uncached_path <- coverage__prepare(
        package,
        file.path(directory, "uncached"),
        parse_cache = FALSE
    )
    uncached <- readRDS(uncached_path)
    testthat::expect_null(uncached$parse_data)
    testthat::expect_null(uncached$source_map)
    uncached_shard <- file.path(directory, "uncached-shard")
    dir.create(uncached_shard)
    callr::r(
        function(library) {
            original <- get("impute_srcref", asNamespace("covr"))
            library(coveragetoy, lib.loc = library)
            stopifnot(identical(
                original,
                get("impute_srcref", asNamespace("covr"))
            ))
            coveragetoy::covered(TRUE)
            coveragetoy::covered(FALSE)
            coveragetoy::covered(FALSE)
        },
        args = list(library = uncached$library),
        libpath = c(uncached$library, .libPaths()),
        env = c(COVERAGE_DIR = uncached_shard, R_COVR = "true")
    )
    baseline <- coverage__merge(uncached_path, uncached_shard, timeout = 0)
    testthat::expect_identical(names(coverage), names(baseline))
    testthat::expect_identical(
        tally,
        covr::tally_coverage(baseline, by = "line")
    )
    report <- file.path(directory, "report")
    coverage__report(coverage, report)
    testthat::expect_true(all(file.exists(file.path(
        report,
        c(
            "coverage.rds",
            "coverage-by-file.csv",
            "coverage.xml",
            "summary.md",
            "trace-manifest.csv"
        )
    ))))
    testthat::expect_error(
        coverage__merge(prepared, rep(shards[[1L]], 2L)),
        "distinct"
    )

    # Tamper only with copied output receipts. Each failure checks that a
    # superficially plausible partial result cannot count as complete coverage.
    trace <- list.files(
        shards[[1L]],
        pattern = "^covr_trace_",
        full.names = TRUE
    )
    trace <- trace[[1L]]
    counts <- readRDS(trace)
    saveRDS(counts[-1L], trace)
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "complete saved source"
    )
    saveRDS(
        stats::setNames(
            rep(0, length(counts)),
            rep(names(counts)[[1L]], length(counts))
        ),
        trace
    )
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "complete saved source"
    )
    invalid <- counts
    invalid[[1L]] <- NA_real_
    saveRDS(invalid, trace)
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "complete saved source"
    )
    saveRDS(counts, trace)
    writeLines("99999999", file.path(shards[[1L]], "expected-trace-99999999"))
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "Unfinished or unregistered"
    )
    unlink(file.path(shards[[1L]], "expected-trace-99999999"))
    file.copy(trace, file.path(shards[[1L]], "covr_trace_99999999"))
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "Unfinished or unregistered"
    )
    unlink(file.path(shards[[1L]], "covr_trace_99999999"))
    pid <- sub("^covr_trace_", "", basename(trace))
    process_path <- file.path(shards[[1L]], paste0("process-", pid, ".rds"))
    process <- readRDS(process_path)
    testthat::expect_s3_class(process$create_time, "POSIXt")
    testthat::expect_false(ps::ps_is_running(ps::ps_handle(
        process$pid,
        time = process$create_time
    )))
    for (field in c("jit_level", "jit_env")) {
        invalid_values <- if (field == "jit_level") {
            list(NULL, NA_integer_, -1L, 4L, 1.5, "0", c(0L, 1L))
        } else {
            list(NULL, NA_character_, 0L, c("0", "1"))
        }
        for (value in invalid_values) {
            invalid_jit <- process
            invalid_jit[field] <- list(value)
            saveRDS(invalid_jit, process_path)
            testthat::expect_error(
                coverage__merge(prepared, shards, 0),
                "JIT observation"
            )
        }
    }
    saveRDS(process, process_path)
    invalid_process <- process
    invalid_process$create_time <- invalid_process$registered_at + 1
    saveRDS(invalid_process, process_path)
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "process identity"
    )
    saveRDS(process, process_path)
    file.copy(process_path, file.path(shards[[1L]], "process-99999999.rds"))
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "Unfinished or unregistered"
    )
    unlink(file.path(shards[[1L]], "process-99999999.rds"))
    resource_path <- file.path(shards[[1L]], paste0("resource-", pid, ".rds"))
    usage <- readRDS(resource_path)
    invalid_usage <- usage
    invalid_usage$user <- -1
    saveRDS(invalid_usage, resource_path)
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "process accounting"
    )
    saveRDS(usage, resource_path)
    metadata_path <- file.path(shards[[1L]], "covr-source-metadata.rds")
    metadata <- readRDS(metadata_path)
    metadata[[1L]]$functions <- "changed_owner"
    saveRDS(metadata, metadata_path)
    testthat::expect_error(
        coverage__merge(prepared, shards, 0),
        "metadata differs"
    )
})

# vim: fdm=marker :
