# Resolve and validate statuses selected for a retry command.
cli_retry__resolve_statuses <- function(value, choices) {
    statuses <- epwshiftr_cli_csv(value)
    # Both retry commands use failed work as the default selection.
    if (is.null(statuses)) {
        statuses <- "failed"
    }
    if (any(!statuses %in% choices)) {
        epwshiftr_cli_usage_abort(sprintf(
            "--status must be one of: %s.",
            paste(choices, collapse = ", ")
        ))
    }
    statuses
}


# Filter retry candidates and decide whether execution should proceed.
cli_retry__prepare_candidates <- function(candidates, statuses, run) {
    if (nrow(candidates)) {
        # The plural selector avoids data.table resolving it as the candidate
        # table's singular status column.
        candidates <- candidates[candidates[["status"]] %in% statuses]
    }

    execute <- isTRUE(run) && nrow(candidates) > 0L
    # Preview rows are explicitly marked, while empty results retain their
    # command-specific schema.
    if (!execute && nrow(candidates)) {
        candidates[, dry_run := TRUE]
    }

    list(candidates = candidates, execute = execute)
}
