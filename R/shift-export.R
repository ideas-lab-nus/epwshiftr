#' @include shift-stage.R
NULL

#' @rdname shift_api
#' @export
# shift_epw_export {{{
shift_epw_export <- function(
    x,
    dir,
    separate = TRUE,
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    shift_stage__assert_stage(x)
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    reporter <- shift_run__current_reporter()
    if (is.null(reporter)) {
        args <- list(
            dir = dir,
            separate = separate,
            overwrite = overwrite,
            resume = resume
        )
        spec <- args
        spec$dir <- normalizePath(
            path.expand(dir),
            winslash = "/",
            mustWork = FALSE
        )
        return(shift_stage__run_if_top(
            "export_epw",
            x,
            shift_epw_export,
            args = args,
            ui = ui,
            spec = spec,
            auto_complete = TRUE
        ))
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        x <- shift_epw_write(
            x,
            separate = separate,
            overwrite = overwrite,
            resume = resume
        )
    }
    if (!S7::S7_inherits(x, ShiftOutputs)) {
        cli::cli_abort(
            "{.fn shift_epw_export} expects a {.cls ShiftOutputs} or {.cls ShiftMorphed} stage."
        )
    }

    shift_export__export_outputs(
        x,
        dir = dir,
        separate = separate,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
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
