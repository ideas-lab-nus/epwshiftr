# Run the complete package check with the active R libraries and supervise it from
# a separate process: native socket waits cannot be interrupted by R time limits.
local_check__main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    timeout <- if (length(args)) as.numeric(args[[1L]]) else 1800
    stopifnot(length(timeout) == 1L, is.finite(timeout), timeout > 0)
    root <- normalizePath(".", mustWork = TRUE)
    stopifnot(file.exists(file.path(root, "DESCRIPTION")))
    output <- if (length(args) > 1L) {
        args[[2L]]
    } else {
        file.path(
            root,
            "reports",
            paste0("local-check-", format(Sys.time(), "%Y%m%d-%H%M%S"))
        )
    }
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, mustWork = TRUE)
    libraries <- .libPaths()
    # Propagate the active libraries to R CMD check and mirai daemons;
    # setting .libPaths() only in this parent leaves child R sessions divergent.
    Sys.setenv(R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep))
    required <- c(mirai = "2.7.3", nanonext = "1.10.3")
    for (package in names(required)) {
        if (
            !requireNamespace(package, quietly = TRUE) ||
                utils::packageVersion(package) <
                    package_version(required[[package]])
        ) {
            stop(
                "Install the required runtime in the active R library: ",
                package,
                " >= ",
                required[[package]],
                call. = FALSE
            )
        }
    }
    writeLines(
        c(
            capture.output(sessionInfo()),
            "",
            libraries,
            capture.output(nanonext::nng_version())
        ),
        file.path(output, "runtime.txt")
    )
    message("Check artifacts: ", output)
    process <- callr::r_bg(
        function(root, output, libraries) {
            .libPaths(libraries)
            Sys.setenv(
                R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep)
            )
            result <- rcmdcheck::rcmdcheck(
                root,
                args = "--no-manual",
                check_dir = file.path(output, "check"),
                error_on = "never"
            )
            saveRDS(result, file.path(output, "result.rds"))
            list(
                errors = result$errors,
                warnings = result$warnings,
                notes = result$notes
            )
        },
        args = list(root, output, libraries),
        libpath = libraries,
        stdout = file.path(output, "stdout.log"),
        stderr = file.path(output, "stderr.log"),
        supervise = TRUE
    )
    on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
    start <- proc.time()[["elapsed"]]
    while (process$is_alive()) {
        if (proc.time()[["elapsed"]] - start >= timeout) {
            # Sample only descendants owned by this check. Never kill unrelated
            # R sessions or retry the same stalled command without diagnosis.
            if (identical(Sys.info()[["sysname"]], "Darwin")) {
                children <- tryCatch(
                    ps::ps_children(
                        ps::ps_handle(process$get_pid()),
                        recursive = TRUE
                    ),
                    error = function(error) list()
                )
                for (child in children) {
                    try(
                        {
                            if (identical(ps::ps_name(child), "R")) {
                                pid <- ps::ps_pid(child)
                                processx::run(
                                    "sample",
                                    c(
                                        as.character(pid),
                                        "1",
                                        "-file",
                                        file.path(
                                            output,
                                            paste0("sample-", pid, ".txt")
                                        )
                                    ),
                                    timeout = 5000,
                                    error_on_status = FALSE
                                )
                            }
                        },
                        silent = TRUE
                    )
                }
            }
            process$kill_tree()
            stop(
                "Package check exceeded ",
                timeout,
                " seconds. Logs retained at ",
                output,
                "; this is an incomplete check, not a pass.",
                call. = FALSE
            )
        }
        process$poll_io(1000)
    }
    result <- process$get_result()
    print(result)
    if (
        length(result$errors) || length(result$warnings) || length(result$notes)
    ) {
        stop("Package check reported findings; inspect ", output, call. = FALSE)
    }
    invisible(result)
}

local_check__main()
