# Check the actual R file operations used by content-addressed store artifacts.
# This diagnostic never changes the registry or shortens persisted identities.
check__windows_paths <- function() {
    if (.Platform$OS.type != "windows") {
        message("Windows long-path check does not apply on this platform.")
        return(invisible(TRUE))
    }
    root <- tempfile("epwshiftr-paths-")
    # Keep individual components short while exceeding the legacy full-path
    # limit, including when the calling session already has a long temp path.
    parent <- file.path(root, strrep("a", 60L), strrep("b", 60L))
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
    temporary <- file.path(parent, "temporary.rds")
    path <- file.path(parent, paste0(strrep("c", 120L), ".rds"))
    payload <- list(value = 42L)
    ok <- tryCatch(
        suppressWarnings({
            saveRDS(payload, temporary)
            isTRUE(file.rename(temporary, path)) &&
                file.exists(path) &&
                identical(readRDS(path), payload)
        }),
        error = function(error) FALSE
    )
    if (!ok) {
        stop(
            paste(
                "This R process cannot write, rename and read long Windows paths.",
                "Enable Win32 long paths (LongPathsEnabled=1) with administrator approval,",
                "then start a new R process and rerun this check before package tests.",
                "See https://learn.microsoft.com/windows/win32/fileio/maximum-file-path-limitation"
            ),
            call. = FALSE
        )
    }
    message(
        "Windows long-path write, rename and read passed (",
        nchar(path),
        " characters)."
    )
    invisible(TRUE)
}

check__windows_paths()
