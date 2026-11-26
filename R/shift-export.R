#' @include shift-stage.R
NULL

# Resolve and validate the target export directory for ShiftOutputs export.
# shift_export__resolve_target_dir {{{
shift_export__resolve_target_dir <- function(dir = NULL, export_dir = NULL) {
    checkmate::assert_string(dir, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(export_dir, min.chars = 1L, null.ok = TRUE)
    if (is.null(dir) && is.null(export_dir)) {
        cli::cli_abort(c(
            "Exporting a {.cls ShiftOutputs} stage with {.fn shift_epw_write} requires a target directory.",
            "i" = "Supply `dir` or `export_dir`."
        ))
    }
    if (!is.null(dir) && !is.null(export_dir)) {
        norm_dir <- normalizePath(
            path.expand(dir),
            winslash = "/",
            mustWork = FALSE
        )
        norm_export <- normalizePath(
            path.expand(export_dir),
            winslash = "/",
            mustWork = FALSE
        )
        if (!identical(norm_dir, norm_export)) {
            cli::cli_abort(c(
                "Supply only one of `dir` or `export_dir` when exporting a {.cls ShiftOutputs} stage.",
                "x" = "`dir` ({.path {dir}}) and `export_dir` ({.path {export_dir}}) differ."
            ))
        }
        return(export_dir)
    }
    shift_stage__coalesce(export_dir, dir)
}
# }}}

# Compute the user-facing export path for one generated EPW row.
# shift_export__export_target_path {{{
shift_export__export_target_path <- function(row, dir, separate = TRUE) {
    path <- row$path[[1L]]
    filename <- basename(path)
    if (isTRUE(separate)) {
        parts <- unlist(
            row[,
                intersect(
                    c(
                        "source_id",
                        "experiment_id",
                        "variant_label",
                        "period",
                        "sequence_id",
                        "weather_year"
                    ),
                    names(row)
                ),
                with = FALSE
            ],
            use.names = FALSE
        )
        parts <- morpher__safe_path(parts[!is.na(parts) & nzchar(parts)])
        return(do.call(file.path, as.list(c(dir, parts, filename))))
    }
    file.path(dir, filename)
}
# }}}

# Copy registered EPW outputs to a user-facing directory and annotate the stage
# with absolute export paths.
# shift_export__export_outputs {{{
shift_export__export_outputs <- function(
    x,
    dir,
    separate = TRUE,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
    outputs <- data.table::copy(shift_outputs(x))
    if (!nrow(outputs)) {
        return(x)
    }
    store_path <- shift_inspect__with_store(x, function(store) store$path)
    export_path <- character(nrow(outputs))
    for (i in seq_len(nrow(outputs))) {
        if (!is.null(reporter)) {
            reporter$check_cancel("write_epw")
            label <- sprintf(
                "Exporting %s/%s/%s",
                outputs$experiment_id[[i]],
                outputs$variant_label[[i]],
                outputs$period[[i]]
            )
            reporter$unit_started(
                label,
                current = i,
                total = nrow(outputs),
                details = list(
                    unit_type = "epw_export",
                    scenario = outputs$experiment_id[[i]],
                    period = outputs$period[[i]]
                )
            )
        }
        source <- store_abs_path(outputs$path[[i]], root = store_path)
        target <- shift_export__export_target_path(
            outputs[i],
            dir = dir,
            separate = separate
        )
        if (!file.exists(source)) {
            cli::cli_abort(
                "Cannot export missing EPW output: {.path {source}}."
            )
        }
        if (file.exists(target) && !isTRUE(overwrite) && !isTRUE(resume)) {
            cli::cli_abort("Export target already exists: {.path {target}}.")
        }
        reused <- file.exists(target) && !isTRUE(overwrite) && isTRUE(resume)
        if (!file.exists(target) || isTRUE(overwrite)) {
            dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
            ok <- file.copy(source, target, overwrite = overwrite)
            if (!isTRUE(ok)) {
                cli::cli_abort(
                    "Failed to export EPW output to {.path {target}}."
                )
            }
        }
        export_path[[i]] <- normalizePath(
            target,
            winslash = "/",
            mustWork = TRUE
        )
        if (!is.null(reporter)) {
            if (isTRUE(reused)) {
                reporter$unit_skipped(
                    sprintf("Reused export %s", basename(target)),
                    current = i,
                    total = nrow(outputs),
                    details = list(export_path = export_path[[i]])
                )
            } else {
                reporter$unit_completed(
                    sprintf("Exported %s", basename(target)),
                    current = i,
                    total = nrow(outputs),
                    outcome = "completed",
                    details = list(export_path = export_path[[i]])
                )
            }
        }
    }
    data.table::set(outputs, j = "export_path", value = export_path)
    x@meta$outputs <- outputs
    x@meta$export_dir <- dir
    x@meta$paths <- outputs$path
    x
}
# }}}

# vim: fdm=marker :
