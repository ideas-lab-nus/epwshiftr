#' @include shift-batch.R
NULL

# Fold recorded states using the same precedence as live batch status.
shift_inspect__status <- function(statuses) {
    statuses <- as.character(statuses)
    if (!length(statuses)) return("empty")
    if (length(unique(statuses)) == 1L) return(statuses[[1L]])
    order <- c("unavailable", "failed", "blocked", "stopping", "running", "queued",
        "waiting", "planned", "partial", "cancelled", "completed")
    selected <- order[order %in% statuses]
    if (length(selected)) selected[[1L]] else "partial"
}

# Use a typed empty result so JSON consumers receive a stable history schema.
shift_inspect__history_empty <- function() {
    data.table::data.table(type = character(), id = character(), status = character(),
        batch_id = character(), method = character(), model = character(),
        updated_at = character(), store = character(), error = character())
}

# Convert run rows without changing their store, artifacts, or scientific intent.
shift_inspect__history_runs <- function(rows, store, batch_id = NA_character_,
                                        method = NA_character_, model = NA_character_) {
    if (!nrow(rows)) return(shift_inspect__history_empty())
    data.table::data.table(type = "run", id = rows$run_id, status = rows$status,
        batch_id = batch_id, method = method, model = model,
        updated_at = as.character(rows$updated_at), store = store, error = NA_character_)
}

# Inspect one receipt as a unit so malformed metadata cannot abort the history
# listing or publish a partially interpreted batch as healthy.
shift_inspect__history_batch <- function(path, type) {
    id <- basename(path)
    receipt <- shift_batch__receipt_read(path, id)
    if (is.null(receipt)) stop("Batch receipt is unreadable.", call. = FALSE)
    manifest <- data.table::as.data.table(receipt$manifest)
    if (!all(c("child_key", "method", "model") %in% names(manifest))) {
        stop("Batch receipt manifest is missing child_key, method, or model.", call. = FALSE)
    }
    checkmate::assert_character(manifest$child_key, min.len = 1L,
        any.missing = FALSE, unique = TRUE)
    checkmate::assert_character(manifest$method, any.missing = FALSE)
    checkmate::assert_character(manifest$model, any.missing = FALSE)
    checkmate::assert_list(receipt$children, types = "list", min.len = 1L)
    child_keys <- vapply(receipt$children, function(child) {
        checkmate::assert_string(child$child_key, min.chars = 1L)
        checkmate::assert_string(child$store_path, min.chars = 1L)
        checkmate::assert_character(child$run_id, len = 1L, null.ok = TRUE)
        child$child_key
    }, character(1L))
    if (anyDuplicated(child_keys) || !setequal(child_keys, manifest$child_key)) {
        stop("Batch receipt children do not match its manifest.", call. = FALSE)
    }
    pieces <- list()
    states <- errors <- character()
    updated <- as.character(receipt$updated_at)
    for (child in receipt$children) {
        run_id <- store__chr1(child$run_id)
        if (is.na(run_id)) {
            states <- c(states, "planned")
            next
        }
        # Read saved rows directly; opening a workflow handle can reconcile
        # jobs and write status changes as an inspection side effect.
        run <- tryCatch({
            rows <- shift_runs(child$store_path)
            matching <- which(rows$run_id == run_id)
            if (!length(matching)) stop(sprintf("Saved run '%s' is missing.", run_id), call. = FALSE)
            rows[matching]
        }, error = identity)
        child_identity <- manifest[manifest$child_key == child$child_key]
        if (inherits(run, "error")) {
            states <- c(states, "unavailable")
            errors <- c(errors, conditionMessage(run))
            rows <- data.table::data.table(run_id = run_id, status = "unavailable",
                updated_at = NA_character_)
        } else {
            rows <- run
            states <- c(states, rows$status)
            updated <- c(updated, as.character(rows$updated_at))
        }
        if (type != "batch") {
            value <- shift_inspect__history_runs(rows, child$store_path, id,
                store__chr1(child_identity$method), store__chr1(child_identity$model))
            if (inherits(run, "error")) value$error <- conditionMessage(run)
            pieces[[length(pieces) + 1L]] <- value
        }
    }
    if (type != "run") {
        updated <- updated[!is.na(updated) & nzchar(updated)]
        pieces[[length(pieces) + 1L]] <- data.table::data.table(type = "batch", id = id,
            status = shift_inspect__status(states), batch_id = id,
            method = paste(unique(manifest$method), collapse = ", "),
            model = paste(unique(manifest$model), collapse = ", "),
            updated_at = if (length(updated)) max(updated) else NA_character_, store = path,
            error = if (length(errors)) paste(unique(errors), collapse = "; ") else NA_character_)
    }
    data.table::rbindlist(c(list(shift_inspect__history_empty()), pieces), fill = TRUE)
}

#' List saved workflow runs and batches
#'
#' @description
#' Read local run records and batch receipts without contacting climate services
#' or creating stores. Child runs include their batch ID and exact store path.
#' Unreadable records are returned with status `unavailable` and an error message.
#' @param store Root workflow store, batch directory, or an open `EsgStore`.
#' @param type Include all records, only runs, or only batches.
#' @param status Optional character vector of states to retain.
#' @return A data.table with type, full ID, status, parent batch, method, model,
#'   update time, store path, and inspection error, sorted newest first.
#' @export
shift_history <- function(store = NULL, type = c("all", "run", "batch"), status = NULL) {
    type <- match.arg(type)
    checkmate::assert_character(status, any.missing = FALSE, null.ok = TRUE)
    root <- shift_coalesce(store, store_dir(init = FALSE))
    root <- if (inherits(root, "EsgStore")) root$path else
        normalizePath(path.expand(root), winslash = "/", mustWork = FALSE)
    if (!dir.exists(root)) return(shift_inspect__history_empty())
    pieces <- list()
    if (type != "batch" && (file.exists(file.path(root, "manifest.duckdb")) ||
        dir.exists(file.path(root, "logs", "shift")))) {
        runs <- tryCatch(shift_runs(root), error = identity)
        pieces[[length(pieces) + 1L]] <- if (inherits(runs, "error")) {
            data.table::data.table(type = "run", id = basename(root), status = "unavailable",
                batch_id = NA_character_, method = NA_character_, model = NA_character_,
                updated_at = NA_character_, store = root, error = conditionMessage(runs))
        } else shift_inspect__history_runs(runs, root)
    }
    paths <- if (file.exists(shift_batch__receipt_path(root))) root else
        list.dirs(file.path(root, "batches"), recursive = FALSE, full.names = TRUE)
    for (path in paths) {
        # Isolate every receipt, including readable RDS files with invalid
        # nested structures, while keeping filters and healthy rows intact.
        pieces[[length(pieces) + 1L]] <- tryCatch(
            shift_inspect__history_batch(path, type), error = function(error) {
                id <- basename(path)
                data.table::data.table(type = "batch", id = id,
                    status = "unavailable", batch_id = id, method = NA_character_, model = NA_character_,
                    updated_at = NA_character_, store = path, error = conditionMessage(error))
            })
    }
    out <- data.table::rbindlist(c(list(shift_inspect__history_empty()), pieces), fill = TRUE)
    # Evaluate filters outside data.table's column scope: both argument names
    # also occur as columns and must keep their caller-supplied meaning.
    if (type != "all") {
        keep <- out$type == type
        out <- out[keep]
    }
    if (!is.null(status)) {
        keep <- out$status %in% status
        out <- out[keep]
    }
    out <- unique(out, by = c("type", "id", "store"))
    data.table::setorderv(out, c("updated_at", "type", "id"), c(-1L, 1L, 1L), na.last = TRUE)
    out[]
}

# Read optional comparison statistics one file at a time. EPW missing sentinels
# become NA before sums/counts are accumulated, so means are weighted by valid
# hourly observations, including multi-year outputs with different year lengths.
shift_inspect__weather <- function(paths) {
    fields <- c("dry_bulb_temperature", "relative_humidity", "wind_speed",
        "global_horizontal_radiation")
    sums <- counts <- stats::setNames(rep(0, length(fields)), fields)
    unreadable <- 0L
    errors <- character()
    hours <- 0L
    paths <- unique(paths)
    reporter <- shift__current_reporter()
    for (index in seq_along(paths)) {
        path <- paths[[index]]
        if (!is.null(reporter)) reporter$unit_started(paste("Reading", basename(path)),
            current = index, total = length(paths), details = list(unit_type = "epw_summary"))
        weather <- tryCatch(epw_file__calculation_weather(epw_file_read(path)$data(), fields),
            error = identity)
        if (inherits(weather, "error")) {
            unreadable <- unreadable + 1L
            errors <- c(errors, conditionMessage(weather))
            if (!is.null(reporter)) reporter$unit_completed(paste("Cannot read", basename(path)),
                current = index, total = length(paths), outcome = "failed")
            next
        }
        hours <- hours + nrow(weather)
        for (field in fields) {
            values <- as.numeric(weather[[field]])
            valid <- is.finite(values)
            sums[[field]] <- sums[[field]] + sum(values[valid])
            counts[[field]] <- counts[[field]] + sum(valid)
        }
        if (!is.null(reporter)) reporter$unit_completed(paste("Read", basename(path)),
            current = index, total = length(paths))
    }
    means <- sums / counts
    means[counts == 0] <- NA_real_
    list(weather_hours = hours, unreadable_files = unreadable,
        mean_temperature_c = unname(means[[1L]]), temperature_hours = unname(counts[[1L]]),
        mean_relative_humidity_pct = unname(means[[2L]]), humidity_hours = unname(counts[[2L]]),
        mean_wind_speed_ms = unname(means[[3L]]), wind_hours = unname(counts[[3L]]),
        mean_global_horizontal_radiation_wh_m2 = unname(means[[4L]]), radiation_hours = unname(counts[[4L]]),
        weather_error = if (length(errors)) paste(unique(errors), collapse = "; ") else NA_character_)
}

# Summarize one child by scientific case identity; diagnostic counts remain
# scoped to cases where possible, with run-wide diagnostics applying to each row.
shift_inspect__summary_child <- function(child, identity, weather) {
    cases <- shift_cases(child, refresh = FALSE)
    standalone <- S7::S7_inherits(child, ShiftRun) &&
        !identical(store__chr1(child@meta$run$task), "future_epw")
    if (standalone) {
        # The root spec describes the first task (often collect). Restore the
        # result's own morphing stage to recover the transform that made it.
        result <- tryCatch(shift_result(child), error = function(error) NULL)
        outputs <- if (S7::S7_inherits(result, ShiftOutputs)) {
            shift_outputs(result, refresh = FALSE)
        } else data.table::data.table()
        morphed <- if (S7::S7_inherits(result, ShiftOutputs)) result@meta$morphed else result
        if (S7::S7_inherits(morphed, ShiftMorphed) &&
            S7::S7_inherits(morphed@meta$transform, WeatherTransformSpec)) {
            transform <- transform__spec_value(morphed@meta$transform)
            for (field in c("method", "scale", "reconstruction")) {
                identity[[field]] <- store__chr1(transform[[field]])
            }
        }
    } else {
        outputs <- shift_outputs(child, refresh = FALSE)
    }
    diagnostics <- shift_diagnostics(child, refresh = FALSE)
    dimensions <- c("source_id", "experiment_id", "variant_label", "grid_label", "period")
    if (!nrow(cases) && nrow(outputs)) {
        # Standalone tasks do not write workflow cases. Count distinct output
        # cases instead of weather-year files, keeping every scientific group.
        columns <- intersect(c(dimensions, "case_id"), names(outputs))
        cases <- unique(outputs[, columns, with = FALSE])
        cases[, status := "completed"]
    }
    groups <- intersect(dimensions, names(cases))
    partitions <- if (nrow(cases) && length(groups)) {
        split(cases, by = groups, keep.by = TRUE, drop = TRUE)
    } else list(cases)
    rows <- lapply(partitions, function(group) {
        files <- outputs
        # Workflow case IDs and morpher case IDs are separate namespaces.
        # Match their shared scientific dimensions, including member and grid
        # when the output schema records them, so multi-year files stay together.
        keys <- intersect(groups, names(files))
        if (nrow(group) && length(keys)) {
            for (key in keys) files <- files[files[[key]] %in% group[[key]]]
        } else if (nrow(group) && "case_id" %in% names(files)) {
            files <- files[files$case_id %in% group$case_id]
        }
        checks <- diagnostics
        if (nrow(checks) && "case_id" %in% names(checks)) {
            checks <- checks[is.na(checks$case_id) |
                checks$case_id %in% c(group$case_id, files$case_id)]
        }
        paths <- if (nrow(files)) vapply(seq_len(nrow(files)), function(index) {
            exported <- store__chr1(files$export_path[index])
            if (!is.na(exported) && file.exists(exported)) exported else
                store_abs_path(files$path[[index]], root = child@store_path)
        }, character(1L)) else character()
        years <- sort(unique(files$weather_year[!is.na(files$weather_year)]))
        completion <- shift__ui_completion(group, files, checks)
        row <- c(identity, list(
            model = shift_coalesce(if (nrow(group)) group$source_id[[1L]] else NULL, identity$model),
            scenario = if (nrow(group)) store__chr1(group$experiment_id) else NA_character_,
            member = if (nrow(group)) store__chr1(group$variant_label) else NA_character_,
            grid = if (nrow(group)) store__chr1(group$grid_label) else NA_character_,
            period = if (nrow(group)) store__chr1(group$period) else NA_character_,
            status = shift_status(child, refresh = FALSE),
            cases = nrow(group), completed_cases = sum(group$status == "completed"),
            epw_files = nrow(files), available_files = sum(file.exists(paths)),
            output_type = paste(unique(files$output_type), collapse = ", "),
            weather_years = paste(years, collapse = ", "),
            warnings = sum(checks$severity == "warning"), errors = sum(checks$severity == "error"),
            field_roles = shift_coalesce(completion$field_summary, NA_character_)))
        # Identity supplies a fallback model only; emit that column exactly once.
        row <- row[!duplicated(names(row), fromLast = TRUE)]
        if (weather) row <- c(row, shift_inspect__weather(paths))
        data.table::as.data.table(row)
    })
    data.table::rbindlist(rows, fill = TRUE)
}

#' Summarize future-weather outputs for comparison
#'
#' @description
#' Compare method/model/scenario/period groups without conflating cases with
#' physical EPW files. Optional weather statistics read existing local EPWs and
#' report means and valid-hour counts; they do not rank methods or imply that
#' different output types or calendar periods are scientifically equivalent.
#' For standalone staged runs, case counts describe distinct cases with saved
#' outputs; the method is recovered from the result's morphing stage.
#' @param x A `ShiftBatch`, `ShiftRun`, or `ShiftPlan`; a run ID is also accepted.
#' @param store Store used when `x` is a run ID.
#' @param refresh Whether to reload saved workflow state before inspection.
#' @param weather Whether to read local EPWs and include hourly weather means.
#'   Missing files and read errors are counted explicitly. Missing EPW sentinel
#'   codes are excluded, and every mean includes its valid-hour denominator.
#' @param ui Presentation options from [shift_ui()] for optional EPW reads.
#' @return A data.table with one row per method, model, scenario and period
#'   (and member/grid where applicable), counts, field roles and optional means.
#' @export
shift_summary <- function(x, store = NULL, refresh = TRUE, weather = FALSE, ui = shift_ui()) {
    checkmate::assert_flag(refresh)
    checkmate::assert_flag(weather)
    if (is.character(x)) x <- shift_run_get(x, store)
    if (!S7::S7_inherits(x, ShiftBatch) && !S7::S7_inherits(x, ShiftRun) &&
        !S7::S7_inherits(x, ShiftPlan)) cli::cli_abort(
            "`x` must be a ShiftBatch, ShiftRun, ShiftPlan, or saved run ID.")
    if (refresh && !S7::S7_inherits(x, ShiftPlan)) x <- shift_refresh(x)
    if (weather && is.null(shift__current_reporter())) {
        return(shift__ui_check(ui, "Summarize EPWs", function(reporter) {
            shift__with_reporter(reporter,
                shift_summary(x, refresh = FALSE, weather = TRUE, ui = ui))
        }))
    }
    if (S7::S7_inherits(x, ShiftBatch)) {
        rows <- lapply(seq_along(x@meta$children), function(index) {
            identity <- as.list(x@meta$manifest[index,
                c("child_key", "method", "scale", "reconstruction", "model"), with = FALSE])
            identity$batch_id <- x@ids$batch_id
            shift_inspect__summary_child(x@meta$children[[index]], identity, weather)
        })
        return(data.table::rbindlist(rows, fill = TRUE))
    }
    spec <- if (S7::S7_inherits(x, ShiftPlan)) shift__plan_spec(x) else
        jsonlite::fromJSON(x@meta$run$spec_json[[1L]], simplifyVector = TRUE)
    transform <- spec$transform
    shift_inspect__summary_child(x, list(method = store__chr1(transform$method),
        scale = store__chr1(transform$scale), reconstruction = store__chr1(transform$reconstruction),
        model = NA_character_, run_id = store__chr1(x@ids$run_id)), weather)
}

# Parse read-only history filters without opening or initializing a database.
cli_shift__history <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--type", "--status", "--limit"))
    epwshiftr_cli_assert_no_positionals(parsed)
    type <- shift_coalesce(parsed$options[["--type"]], "all")
    if (!type %in% c("all", "run", "batch")) epwshiftr_cli_usage_abort(
        "--type must be all, run, or batch.")
    rows <- shift_history(store, type = type, status = epwshiftr_cli_csv(parsed$options[["--status"]]))
    limit <- epwshiftr_cli_count_or_default(parsed$options[["--limit"]], "--limit", 20L, positive = FALSE)
    out <- utils::head(rows, limit)
    attr(out, "shift_history_total") <- nrow(rows)
    out
}

# Share the same comparison table between R and CLI, with optional local reads.
cli_shift__summary <- function(store, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--run", "--batch"),
        flags = c("--weather", "--no-progress", "--reduced-motion"))
    epwshiftr_cli_assert_no_positionals(parsed)
    shift_summary(cli_shift__target(parsed, store), refresh = FALSE,
        weather = isTRUE(parsed$flags[["--weather"]]),
        ui = epwshiftr_cli_task_ui(parsed, json = json, jsonl = jsonl, quiet = quiet))
}
