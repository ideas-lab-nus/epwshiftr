# Capture the resolver's actual candidate tables, not a later reconstruction
# using a changed catalog or selection policy. Explanations refer only to the
# requested models and frequency/table/grid combinations actually evaluated.
# shift_selection__evidence {{{
shift_selection__evidence <- function(
    plan,
    future,
    historical,
    selected,
    reference_required
) {
    future <- data.table::as.data.table(data.table::copy(future))
    historical <- data.table::as.data.table(data.table::copy(historical))
    selected <- data.table::as.data.table(data.table::copy(selected))
    meta <- plan@meta
    climate <- meta$climate
    member <- if (is.null(climate)) {
        meta$request@meta$variant
    } else {
        climate@member
    }
    grid <- if (is.null(climate)) {
        meta$request@meta$filters$grid_label
    } else {
        climate@grid
    }
    models <- if (is.null(climate)) meta$request@meta$source else climate@model
    identity <- c(
        "source_id",
        "variant_label",
        "frequency",
        "required_partition_key",
        "requirement_key"
    )
    if (nrow(future)) {
        matched_reference <- rep(!isTRUE(reference_required), nrow(future))
        if (isTRUE(reference_required) && nrow(historical)) {
            matching <- future[
                historical[historical$complete %in% TRUE],
                on = identity,
                which = TRUE,
                nomatch = 0L
            ]
            matched_reference[unique(matching)] <- TRUE
        }
        chosen <- rep(FALSE, nrow(future))
        if (nrow(selected)) {
            matching <- future[
                selected,
                on = identity,
                which = TRUE,
                nomatch = 0L
            ]
            chosen[unique(matching)] <- TRUE
        }
        reason <- rep(
            if (nrow(selected)) {
                "eligible_alternative_not_selected"
            } else {
                "selection_not_completed"
            },
            nrow(future)
        )
        reason[!matched_reference] <- "no_complete_matching_reference"
        reason[!future$complete %in% TRUE] <- "future_coverage_incomplete"
        if (!is.null(member)) {
            reason[
                !future$variant_label %in% member
            ] <- "user_member_constraint"
        }
        if (!is.null(grid)) {
            reason[!future$grid_label %in% grid] <- "user_grid_constraint"
        }
        reason[chosen] <- "selected"
        data.table::set(
            future,
            j = "matching_reference",
            value = matched_reference
        )
        data.table::set(future, j = "selected", value = chosen)
        data.table::set(future, j = "reason", value = reason)
    }
    list(
        scope = "file_candidates_for_requested_models",
        package_version = as.character(utils::packageVersion("epwshiftr")),
        required_variables = morpher__variable_requirements(meta$recipe),
        input_variables = morpher__input_variables(meta$recipe),
        reference_required = reference_required,
        allow_partial = meta$control@allow_partial,
        member_constraint = member,
        grid_constraint = grid,
        models_without_candidates = setdiff(models, future$source_id),
        future = as.data.frame(future),
        reference = as.data.frame(historical),
        selected = as.data.frame(selected)
    )
}
# }}}

# Attach immutable selection evidence to the existing run event store. Content
# identity avoids duplicate records when shared inputs are registered again.
# shift_selection__persist {{{
shift_selection__persist <- function(store, run_id, evidence) {
    if (is.character(evidence)) {
        evidence <- jsonlite::fromJSON(evidence, simplifyVector = FALSE)
    }
    shift_job__run_event(
        store,
        run_id,
        stage = "resolve",
        status = "selection",
        message = "Recorded input selection evidence.",
        details = list(kind = "input_selection", evidence = evidence),
        snapshot = FALSE,
        event_id = store__hash(
            run_id,
            "input_selection",
            shift_persist__spec_json(evidence)
        )
    )
    invisible(NULL)
}
# }}}

# Decode only known tabular fields. Keep attempts and checks as lists so a
# single record has the same public shape as several node/phase records.
# shift_selection__decode {{{
shift_selection__decode <- function(record) {
    record$checks <- lapply(record$checks, function(check) {
        for (field in c("future", "reference", "selected")) {
            rows <- lapply(check[[field]], function(row) {
                lapply(row, function(value) if (is.null(value)) NA else value)
            })
            check[[field]] <- data.table::rbindlist(
                rows,
                use.names = TRUE,
                fill = TRUE
            )
        }
        check
    })
    record
}
# }}}

#' Inspect Recorded Climate Input Selection
#'
#' Read the original workflow configuration, pinned selection, and candidate
#' checks saved during File-catalog resolution. No remote queries or candidate
#' recalculation are performed. Missing records in older runs remain explicitly
#' unavailable.
#'
#' @param x A `ShiftPlan`, finished `ShiftRun`, a stage associated with a run,
#'   or a `ShiftBatch`. Running jobs must finish before durable inspection.
#'
#' @return A list with `state`, `run_id`, `requested`, `resolved`, `attempts`,
#'   `cases`, and `diagnostics`. Batches return a named list per child.
#'   Each attempt identifies its index node and outcome. Its `checks` retain
#'   candidate tables before and after service resolution, query IDs, required
#'   variables and explicit constraints. Future candidate rows carry a `reason`.
#'
#' @details
#' The scope is the File-level candidates evaluated for requested models.
#' It does not include every model in ESGF, candidates excluded by upstream
#' Dataset discovery or server filters, or a history of arbitrary user filters.
#' `eligible_alternative_not_selected` describes execution selection, not a
#' scientific ranking. `future_coverage_incomplete` and
#' `no_complete_matching_reference` describe coverage or input pairing.
#' `user_member_constraint` and `user_grid_constraint` identify explicit limits
#' when those alternatives occur in the evaluated table. An empty check list
#' means failure occurred before candidates could be evaluated. An attempt may
#' fail after selecting candidates, for example during service access.
#'
#' Plans return `state = "planned"`. Finished runs without captured candidates
#' return `state = "not_recorded"`, retaining any saved configuration and final
#' selection. Captured attempts return `state = "recorded"`, which does not
#' imply successful weather generation. Inspect the attempt outcome and cases.
#'
#' @export
# shift_selection {{{
shift_selection <- function(x) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(lapply(x@meta$children, shift_selection))
    }
    if (S7::S7_inherits(x, ShiftPlan)) {
        return(list(
            state = "planned",
            run_id = NULL,
            requested = shift_persist__plan_spec(x),
            resolved = NULL,
            attempts = list(),
            cases = shift_cases(x),
            diagnostics = shift_diagnostics(x)
        ))
    }
    x <- shift_run_get(x)
    if (x@meta$run$status[[1L]] %in% c("queued", "running", "stopping")) {
        cli::cli_abort("Read durable input selection after the run finishes.")
    }
    row <- x@meta$run
    events <- x@meta$events
    events <- events[events$status == "selection"]
    attempts <- lapply(events$details_json, function(value) {
        details <- jsonlite::fromJSON(value, simplifyVector = FALSE)
        if (!identical(details$kind, "input_selection")) {
            return(NULL)
        }
        shift_selection__decode(details$evidence)
    })
    attempts <- Filter(Negate(is.null), attempts)
    resolved <- row$resolved_spec_json[[1L]]
    list(
        state = if (length(attempts)) "recorded" else "not_recorded",
        run_id = row$run_id[[1L]],
        requested = jsonlite::fromJSON(
            row$spec_json[[1L]],
            simplifyVector = FALSE
        ),
        resolved = if (is.na(resolved) || !nzchar(resolved)) {
            NULL
        } else {
            jsonlite::fromJSON(resolved, simplifyVector = FALSE)
        },
        attempts = attempts,
        cases = shift_cases(x, refresh = FALSE),
        diagnostics = shift_diagnostics(x, refresh = FALSE)
    )
}
# }}}

# vim: fdm=marker :
