# Canonicalize a workflow directory without requiring its final components to
# exist. The deepest existing ancestor is resolved first so aliases introduced
# by symbolic links cannot bypass output/store isolation.
shift__canonical_workflow_path <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)

    current <- normalizePath(
        path.expand(path),
        winslash = "/",
        mustWork = FALSE
    )
    suffix <- character()

    # Walk upward until normalizePath() can resolve the real filesystem
    # identity. Retain missing components for reconstruction below.
    while (!file.exists(current)) {
        parent <- dirname(current)
        if (identical(parent, current)) {
            break
        }
        suffix <- c(basename(current), suffix)
        current <- parent
    }
    if (file.exists(current)) {
        current <- normalizePath(current, winslash = "/", mustWork = TRUE)
    }

    # Rebuild unresolved components one at a time. Handling dot components here
    # is necessary because normalizePath(..., mustWork = FALSE) leaves them in
    # paths whose descendants do not exist yet.
    for (component in suffix) {
        if (!nzchar(component) || identical(component, ".")) {
            next
        }
        if (identical(component, "..")) {
            current <- dirname(current)
        } else {
            current <- file.path(current, component)
        }
    }
    normalizePath(current, winslash = "/", mustWork = FALSE)
}

# Flip one alphabetic character in a basename so the containing filesystem can
# be probed for case-sensitive path identity without creating probe files.
shift__path_case_variant <- function(path) {
    name <- basename(path)
    chars <- strsplit(name, "", fixed = TRUE)[[1L]]
    positions <- which(grepl("[[:alpha:]]", chars))
    if (!length(positions)) {
        return(NA_character_)
    }
    position <- positions[[1L]]
    char <- chars[[position]]
    chars[[position]] <- if (identical(char, toupper(char))) {
        tolower(char)
    } else {
        toupper(char)
    }
    file.path(dirname(path), paste0(chars, collapse = ""))
}

# Detect the case-comparison semantics of the filesystem containing a
# canonical path. Windows is defined as case-insensitive; other platforms are
# detected from an existing ancestor so case-sensitive macOS volumes remain
# distinguishable from the usual case-insensitive ones.
shift__workflow_path_case_sensitive <- function(path) {
    if (identical(.Platform$OS.type, "windows")) {
        return(FALSE)
    }

    current <- path
    while (!file.exists(current)) {
        parent <- dirname(current)
        if (identical(parent, current)) {
            return(TRUE)
        }
        current <- parent
    }
    current <- normalizePath(current, winslash = "/", mustWork = TRUE)

    repeat {
        variant <- shift__path_case_variant(current)
        if (!is.na(variant) && file.exists(variant)) {
            resolved <- normalizePath(variant, winslash = "/", mustWork = TRUE)
            if (identical(resolved, current)) {
                return(FALSE)
            }
        }
        parent <- dirname(current)
        if (identical(parent, current)) {
            return(TRUE)
        }
        current <- parent
    }
}

# Test an ancestor relationship at a complete path-component boundary, rather
# than by a raw prefix that would confuse sibling names such as output and
# output-cache.
shift__workflow_path_contains <- function(parent, child) {
    if (identical(parent, child)) {
        return(TRUE)
    }
    prefix <- if (endsWith(parent, "/")) parent else paste0(parent, "/")
    startsWith(child, prefix)
}

# Enforce the high-level workflow contract that the delivery directory contains
# exported EPWs only and never overlaps persistent store artifacts.
shift__validate_delivery_store_paths <- function(dir, store) {
    checkmate::assert_string(dir, min.chars = 1L)
    if (inherits(store, "EsgStore")) {
        store <- store$path
    }
    checkmate::assert_string(store, min.chars = 1L)

    paths <- list(
        dir = shift__canonical_workflow_path(dir),
        store = shift__canonical_workflow_path(store)
    )

    # Case folding is used only when either path is on a case-insensitive
    # filesystem. Overlapping paths necessarily share that filesystem.
    case_sensitive <- c(
        shift__workflow_path_case_sensitive(paths$dir),
        shift__workflow_path_case_sensitive(paths$store)
    )
    comparable <- paths
    if (!all(case_sensitive)) {
        comparable <- lapply(comparable, tolower)
    }

    overlap <- shift__workflow_path_contains(comparable$dir, comparable$store) ||
        shift__workflow_path_contains(comparable$store, comparable$dir)
    if (isTRUE(overlap)) {
        cli::cli_abort(c(
            "{.arg dir} and {.arg store} must be separate, non-overlapping directories.",
            "x" = "{.arg dir} resolves to {.path {paths$dir}} and {.arg store} resolves to {.path {paths$store}}.",
            "i" = "{.arg dir} receives exported EPW files only.",
            "i" = "Use a separate {.arg store} directory, or omit {.arg store} to use {.code epwshiftr.dir_store}."
        ))
    }
    invisible(paths)
}
