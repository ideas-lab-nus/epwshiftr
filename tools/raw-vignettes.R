# Manage locally rendered raw vignettes through one explicit check/render
# command without executing expensive vignette code in CI.

# Verify that relative raw-vignette paths are resolved from the package root.
vignette__assert_package_root <- function() {
    if (!file.exists("DESCRIPTION")) {
        stop("Run this script from the package root.", call. = FALSE)
    }
    invisible(NULL)
}

# Return the deterministic source-to-output inventory shared by rendering and
# the checksum-only CI freshness check.
vignette__checksum_manifest <- function() {
    source_paths <- sort(list.files(
        "vignettes-raw",
        pattern = "[.]Rmd$",
        recursive = TRUE,
        full.names = TRUE
    ))
    output_paths <- sub("^vignettes-raw/", "vignettes/", source_paths)

    if (!length(source_paths)) {
        stop("No raw vignettes were found.", call. = FALSE)
    }

    missing_outputs <- output_paths[!file.exists(output_paths)]
    if (length(missing_outputs)) {
        stop(
            "Precompiled vignette output is missing:\n",
            paste(missing_outputs, collapse = "\n"),
            call. = FALSE
        )
    }

    data.frame(
        source = source_paths,
        output = output_paths,
        source_md5 = unname(tools::md5sum(source_paths)),
        output_md5 = unname(tools::md5sum(output_paths)),
        stringsAsFactors = FALSE
    )
}

# Write checksums only after local rendering and output validation succeed.
vignette__write_checksum_manifest <- function(
    path = "vignettes-raw/precompiled-checksums.csv"
) {
    manifest <- vignette__checksum_manifest()
    utils::write.csv(
        manifest,
        path,
        row.names = FALSE,
        quote = TRUE,
        na = ""
    )
    invisible(manifest)
}

# Compare committed raw sources and generated outputs with the last successful
# local-render manifest without executing any vignette code.
vignette__check_checksum_manifest <- function(
    path = "vignettes-raw/precompiled-checksums.csv"
) {
    if (!file.exists(path)) {
        stop(
            "Raw-vignette checksum manifest is missing: ",
            path,
            "\nRun locally: Rscript tools/raw-vignettes.R render",
            call. = FALSE
        )
    }

    expected_columns <- c("source", "output", "source_md5", "output_md5")
    recorded <- utils::read.csv(
        path,
        colClasses = "character",
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    if (!identical(names(recorded), expected_columns)) {
        stop(
            "Raw-vignette checksum manifest has an invalid schema.",
            call. = FALSE
        )
    }

    if (
        anyNA(recorded) ||
        any(!nzchar(as.matrix(recorded))) ||
        anyDuplicated(recorded$source) ||
        anyDuplicated(recorded$output)
    ) {
        stop(
            "Raw-vignette checksum manifest contains missing or duplicate entries.",
            call. = FALSE
        )
    }

    current <- vignette__checksum_manifest()
    recorded <- recorded[order(recorded$source), , drop = FALSE]

    inventory_changed <- !identical(recorded$source, current$source) ||
        !identical(recorded$output, current$output)
    details <- character()

    if (inventory_changed) {
        added_sources <- setdiff(current$source, recorded$source)
        removed_sources <- setdiff(recorded$source, current$source)
        details <- c(
            "vignette inventory or source-to-output mapping changed",
            paste0("raw source added: ", added_sources),
            paste0("raw source removed: ", removed_sources)
        )
    } else {
        source_changed <- recorded$source_md5 != current$source_md5
        output_changed <- recorded$output_md5 != current$output_md5

        if (any(source_changed)) {
            details <- c(
                details,
                paste0("raw source changed: ", current$source[source_changed])
            )
        }
        if (any(output_changed)) {
            details <- c(
                details,
                paste0(
                    "precompiled output changed: ",
                    current$output[output_changed]
                )
            )
        }
    }

    if (length(details)) {
        stop(
            "Precompiled vignettes are not fresh:\n",
            paste0("- ", details, collapse = "\n"),
            "\nRun locally: Rscript tools/raw-vignettes.R render",
            call. = FALSE
        )
    }

    invisible(current)
}

# Check committed source/output checksums through the lightweight CI path.
vignette__check <- function() {
    vignette__assert_package_root()
    vignette__check_checksum_manifest()
    message("Precompiled raw vignettes are fresh.")
    invisible(NULL)
}

# Require the packages used only by the expensive local rendering path.
vignette__require_render_dependencies <- function() {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("Install pkgload before rendering raw vignettes.", call. = FALSE)
    }
    if (!requireNamespace("rawvignette", quietly = TRUE)) {
        stop(
            "Install rawvignette before rendering raw vignettes:\n",
            "  pak::pak(\"matthewkling/rawvignette\")",
            call. = FALSE
        )
    }
    invisible(NULL)
}

# Normalize generated articles so committed output is stable across local
# rendering environments.
vignette__trim_trailing_whitespace <- function(path) {
    lines <- readLines(path, warn = FALSE)
    trimmed <- sub("[ \t]+$", "", lines)
    if (!identical(lines, trimmed)) {
        writeLines(trimmed, path, useBytes = TRUE)
    }
    invisible(path)
}

# Find embedded rendering failures before a new checksum manifest is trusted.
vignette__render_errors <- function(article_paths) {
    errors <- lapply(article_paths, function(path) {
        lines <- readLines(path, warn = FALSE)
        index <- grep("^#> Error", lines)
        if (!length(index)) {
            return(NULL)
        }
        data.frame(
            file = path,
            line = index,
            text = lines[index],
            stringsAsFactors = FALSE
        )
    })
    errors <- errors[lengths(errors) > 0L]
    if (!length(errors)) {
        return(NULL)
    }
    do.call(rbind, errors)
}

# Render raw vignettes locally, validate generated output, and record the exact
# committed source/output checksums consumed later by CI.
vignette__render <- function() {
    vignette__assert_package_root()
    vignette__require_render_dependencies()

    pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
    rawvignette::precompile_raw_vignettes()

    article_paths <- list.files(
        "vignettes/articles",
        pattern = "[.]Rmd$",
        full.names = TRUE
    )
    invisible(lapply(article_paths, vignette__trim_trailing_whitespace))

    freshness <- rawvignette::check_raw_vignettes()
    if (is.data.frame(freshness) && any(freshness$status != "fresh")) {
        stale <- freshness$name[freshness$status != "fresh"]
        stop(
            "Raw vignette output is stale: ",
            paste(stale, collapse = ", "),
            call. = FALSE
        )
    }

    render_errors <- vignette__render_errors(article_paths)
    if (!is.null(render_errors) && nrow(render_errors)) {
        details <- sprintf(
            "%s:%d: %s",
            render_errors$file,
            render_errors$line,
            render_errors$text
        )
        stop(
            "Rendered raw vignette output contains errors:\n",
            paste(details, collapse = "\n"),
            call. = FALSE
        )
    }

    vignette__write_checksum_manifest()
    vignette__check_checksum_manifest()
    message("Rendered raw vignettes and updated their checksum manifest.")
    invisible(NULL)
}

# Dispatch the single repository tool to an explicit lightweight check or
# expensive local render operation.
vignette__main <- function(args = commandArgs(trailingOnly = TRUE)) {
    choices <- c("check", "render")
    if (length(args) != 1L || !args[[1L]] %in% choices) {
        stop(
            "Usage: Rscript tools/raw-vignettes.R <check|render>",
            call. = FALSE
        )
    }

    switch(
        args[[1L]],
        check = vignette__check(),
        render = vignette__render()
    )
}

# Run the command dispatcher only when this file is invoked as a script, which
# also keeps its helpers reusable for focused local maintenance.
if (sys.nframe() == 0L) {
    vignette__main()
}
