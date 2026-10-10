# Restore executable bits only on the three known programs in this project's
# private processx copy. Never chmod a shared cache, symlink, or hard-linked file.
checks__processx_permissions <- function(root) {
    if (.Platform$OS.type == "windows") {
        return(invisible(NULL))
    }
    package <- normalizePath(find.package("processx"), winslash = "/")
    programs <- file.path(package, "bin", c("px", "sock", "supervisor"))
    missing <- programs[file.exists(programs) & file.access(programs, 1L) != 0L]
    if (!length(missing)) {
        return(invisible(NULL))
    }
    local_library <- file.path(
        normalizePath(root, winslash = "/"),
        ".uvr",
        "library"
    )
    if (!startsWith(package, paste0(local_library, "/"))) {
        stop(
            "processx executables are outside the project library; repair their installation before testing."
        )
    }
    for (program in missing) {
        linked <- Sys.readlink(program)
        if (!is.na(linked) && nzchar(linked)) {
            stop("Refusing to change a linked processx program.")
        }
        flags <- if (Sys.info()[["sysname"]] == "Darwin") {
            c("-f", "%l")
        } else {
            c("-c", "%h")
        }
        links <- system2("stat", c(flags, shQuote(program)), stdout = TRUE)
        if (!identical(links, "1")) {
            stop("Refusing to change a shared processx program.")
        }
        Sys.chmod(
            program,
            mode = as.octmode(bitwOr(as.integer(file.info(program)$mode), 64L))
        )
        if (file.access(program, 1L) != 0L) {
            stop("Could not restore processx executable permissions.")
        }
    }
    invisible(NULL)
}

# Prepare one immutable installation, then measure independent complete runs.
# Usage: uvr run tools/check-performance.R -- ordinary|coverage OUTPUT [SHARDS] [ROUNDS]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || !args[[1L]] %in% c("ordinary", "coverage")) {
    stop("Usage: ordinary|coverage OUTPUT [SHARDS=4] [ROUNDS=1]")
}
mode <- args[[1L]]
output <- args[[2L]]
shards <- if (length(args) >= 3L) as.integer(args[[3L]]) else 4L
rounds <- if (length(args) >= 4L) as.integer(args[[4L]]) else 1L
stopifnot(!dir.exists(output), shards %in% 1:8, rounds >= 1L)
root <- normalizePath(".", winslash = "/")
dir.create(output, recursive = TRUE)
output <- normalizePath(output, winslash = "/")
source("tests/support/check-parallel.R")
source(".github/coverage.R")
Sys.setenv(
    R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep),
    NOT_CRAN = if (mode == "coverage" || .Platform$OS.type != "windows") {
        "true"
    } else {
        "false"
    }
)
checks__processx_permissions(root)
preparation_started <- proc.time()
prepared <- NULL
if (mode == "coverage") {
    prepared <- coverage__prepare(
        root,
        file.path(output, "prepared"),
        quiet = FALSE
    )
    library <- readRDS(prepared)$library
} else {
    library <- file.path(output, "library")
    dir.create(library)
    processx::run(
        file.path(
            R.home("bin"),
            if (.Platform$OS.type == "windows") "R.exe" else "R"
        ),
        c(
            "CMD",
            "INSTALL",
            "--install-tests",
            "--with-keep.source",
            "--with-keep.parse.data",
            "--no-multiarch",
            "-l",
            library,
            root
        ),
        timeout = 300,
        stdout = file.path(output, "install.stdout"),
        stderr = file.path(output, "install.stderr")
    )
}
saveRDS(
    proc.time() - preparation_started,
    file.path(output, "preparation-time.rds")
)
# Coverage worker instrumentation changes the relative cost of asynchronous files.
weights <- read.csv(file.path(
    "tests/support",
    if (mode == "coverage") "coverage-durations.csv" else "test-durations.csv"
))
metrics <- vector("list", rounds)
for (round in seq_len(rounds)) {
    metrics[[round]] <- checks__run(
        library,
        file.path(output, paste0("round-", round)),
        shards = shards,
        durations = weights,
        coverage = prepared,
        coverage_script = file.path(root, ".github/coverage.R")
    )
    print(metrics[[round]])
    saveRDS(metrics[seq_len(round)], file.path(output, "rounds.rds"))
    # A complete run above the threshold already rules out three passing rounds.
    # Preserve its metrics and avoid repeating the same unsuccessful candidate.
    if (metrics[[round]]$elapsed >= 600) {
        stop(
            "Completed round did not meet the 600-second target; metrics retained."
        )
    }
}
saveRDS(metrics, file.path(output, "rounds.rds"))
if (any(vapply(metrics, function(x) x$elapsed >= 600, logical(1L)))) {
    stop(
        "All tests completed, but at least one round did not meet the 600-second target."
    )
}

# vim: fdm=marker :
