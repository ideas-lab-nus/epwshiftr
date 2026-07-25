# Return the deterministic source-to-output inventory used by both local
# rendering and the CI-only freshness check.
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

# Write the content checksums only after local raw-vignette rendering and
# validation have completed successfully.
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

# Compare committed raw sources and precompiled outputs with the checksums from
# the last successful local render without executing any vignette code.
vignette__check_checksum_manifest <- function(
    path = "vignettes-raw/precompiled-checksums.csv"
) {
    if (!file.exists(path)) {
        stop(
            "Raw-vignette checksum manifest is missing: ",
            path,
            "\nRun locally: Rscript tools/render-raw-vignettes.R",
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
            "\nRun locally: Rscript tools/render-raw-vignettes.R",
            call. = FALSE
        )
    }

    invisible(current)
}
