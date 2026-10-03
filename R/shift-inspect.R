#' @include shift-batch.R
NULL

# Fold recorded states using the same precedence as live batch status.
shift_inspect__status <- function(statuses) {
    statuses <- as.character(statuses)
    if (!length(statuses)) {
        return("empty")
    }
    if (length(unique(statuses)) == 1L) {
        return(statuses[[1L]])
    }
    order <- c(
        "unavailable",
        "failed",
        "blocked",
        "stopping",
        "running",
        "queued",
        "waiting",
        "planned",
        "partial",
        "cancelled",
        "completed"
    )
    selected <- order[order %in% statuses]
    if (length(selected)) selected[[1L]] else "partial"
}

# Use a typed empty result so JSON consumers receive a stable history schema.
shift_inspect__history_empty <- function() {
    data.table::data.table(
        type = character(),
        id = character(),
        status = character(),
        batch_id = character(),
        method = character(),
        model = character(),
        updated_at = character(),
        store = character(),
        error = character()
    )
}

# Convert run rows without changing their store, artifacts, or scientific intent.
shift_inspect__history_runs <- function(
    rows,
    store,
    batch_id = NA_character_,
    method = NA_character_,
    model = NA_character_
) {
    out <- shift_inspect__history_empty()[rep(NA_integer_, nrow(rows))]
    values <- list(
        type = "run",
        id = rows$run_id,
        status = rows$status,
        batch_id = batch_id,
        method = method,
        model = model,
        updated_at = as.character(rows$updated_at),
        store = store
    )
    if (nrow(out)) {
        for (name in names(values)) {
            data.table::set(out, j = name, value = values[[name]])
        }
    }
    out
}

# Inspect one receipt as a unit so malformed metadata cannot abort the history
# listing or publish a partially interpreted batch as healthy.
shift_inspect__history_batch <- function(path, type) {
    id <- basename(path)
    receipt <- shift_batch__receipt_read(path, id)
    if (is.null(receipt)) {
        stop("Batch receipt is unreadable.", call. = FALSE)
    }
    manifest <- data.table::as.data.table(receipt$manifest)
    if (!all(c("child_key", "method", "model") %in% names(manifest))) {
        stop(
            "Batch receipt manifest is missing child_key, method, or model.",
            call. = FALSE
        )
    }
    checkmate::assert_character(
        manifest$child_key,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    checkmate::assert_character(manifest$method, any.missing = FALSE)
    checkmate::assert_character(manifest$model, any.missing = FALSE)
    checkmate::assert_list(receipt$children, types = "list", min.len = 1L)
    child_keys <- vapply(
        receipt$children,
        function(child) {
            checkmate::assert_string(child$child_key, min.chars = 1L)
            checkmate::assert_string(child$store_path, min.chars = 1L)
            checkmate::assert_character(child$run_id, len = 1L, null.ok = TRUE)
            child$child_key
        },
        character(1L)
    )
    if (
        anyDuplicated(child_keys) || !setequal(child_keys, manifest$child_key)
    ) {
        stop("Batch receipt children do not match its manifest.", call. = FALSE)
    }
    n <- length(receipt$children)
    pieces <- vector("list", n + 1L)
    states <- rep("planned", n)
    errors <- rep(NA_character_, n)
    updated <- c(as.character(receipt$updated_at), rep(NA_character_, n))
    child_paths <- vapply(receipt$children, `[[`, character(1L), "store_path")
    run_ids <- vapply(
        receipt$children,
        function(child) {
            store__chr1(child$run_id)
        },
        character(1L)
    )
    registered <- data.table::data.table(path = child_paths, run_id = run_ids)
    registered <- registered[!is.na(registered$run_id)]
    paths <- unique(registered$path)
    data.table::setindexv(registered, "path")
    # A lock fallback remains read-only; refresh/reconciliation is deliberately
    # excluded from history inspection.
    runs <- stats::setNames(
        lapply(paths, function(path) {
            tryCatch(
                {
                    ids <- registered[list(path), on = "path"]$run_id
                    value <- shift_inspect__runs(path, unique(ids))
                    data.table::setindexv(value, "run_id")
                    value
                },
                error = identity
            )
        }),
        paths
    )
    identities <- manifest[match(child_keys, manifest$child_key)]
    for (index in seq_along(receipt$children)) {
        child <- receipt$children[[index]]
        run_id <- store__chr1(child$run_id)
        if (is.na(run_id)) {
            next
        }
        # Read saved rows directly; opening a workflow handle can reconcile
        # jobs and write status changes as an inspection side effect.
        run <- tryCatch(
            {
                rows <- runs[[child$store_path]]
                if (inherits(rows, "error")) {
                    stop(rows)
                }
                matching <- rows[list(run_id), on = "run_id", nomatch = 0L]
                if (!nrow(matching)) {
                    stop(
                        sprintf("Saved run '%s' is missing.", run_id),
                        call. = FALSE
                    )
                }
                matching
            },
            error = identity
        )
        child_identity <- identities[index]
        if (inherits(run, "error")) {
            states[[index]] <- "unavailable"
            errors[[index]] <- conditionMessage(run)
            rows <- data.table::data.table(
                run_id = run_id,
                status = "unavailable",
                updated_at = NA_character_
            )
        } else {
            rows <- run
            states[[index]] <- rows$status[[1L]]
            updated[[index + 1L]] <- as.character(rows$updated_at[[1L]])
        }
        if (type != "batch") {
            value <- shift_inspect__history_runs(
                rows,
                child$store_path,
                id,
                store__chr1(child_identity$method),
                store__chr1(child_identity$model)
            )
            if (inherits(run, "error")) {
                data.table::set(
                    value,
                    j = "error",
                    value = conditionMessage(run)
                )
            }
            pieces[[index]] <- value
        }
    }
    if (type != "run") {
        updated <- updated[!is.na(updated) & nzchar(updated)]
        pieces[[n + 1L]] <- data.table::data.table(
            type = "batch",
            id = id,
            status = shift_inspect__status(states),
            batch_id = id,
            method = paste(unique(manifest$method), collapse = ", "),
            model = paste(unique(manifest$model), collapse = ", "),
            updated_at = if (length(updated)) max(updated) else NA_character_,
            store = path,
            error = if (any(!is.na(errors))) {
                paste(unique(errors[!is.na(errors)]), collapse = "; ")
            } else {
                NA_character_
            }
        )
    }
    data.table::rbindlist(
        c(list(shift_inspect__history_empty()), pieces),
        fill = TRUE
    )
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
shift_history <- function(
    store = NULL,
    type = c("all", "run", "batch"),
    status = NULL
) {
    type <- match.arg(type)
    checkmate::assert_character(status, any.missing = FALSE, null.ok = TRUE)
    root <- shift_stage__coalesce(store, store_dir(init = FALSE))
    root <- if (inherits(root, "EsgStore")) {
        root$path
    } else {
        normalizePath(path.expand(root), winslash = "/", mustWork = FALSE)
    }
    if (!dir.exists(root)) {
        return(shift_inspect__history_empty())
    }
    pieces <- list()
    if (
        type != "batch" &&
            (file.exists(file.path(root, "manifest.duckdb")) ||
                dir.exists(file.path(root, "logs", "shift")))
    ) {
        runs <- tryCatch(shift_runs(root), error = identity)
        pieces[[length(pieces) + 1L]] <- if (inherits(runs, "error")) {
            data.table::data.table(
                type = "run",
                id = basename(root),
                status = "unavailable",
                batch_id = NA_character_,
                method = NA_character_,
                model = NA_character_,
                updated_at = NA_character_,
                store = root,
                error = conditionMessage(runs)
            )
        } else {
            shift_inspect__history_runs(runs, root)
        }
    }
    paths <- if (file.exists(shift_batch__receipt_path(root))) {
        root
    } else {
        list.dirs(
            file.path(root, "batches"),
            recursive = FALSE,
            full.names = TRUE
        )
    }
    for (path in paths) {
        # Isolate every receipt, including readable RDS files with invalid
        # nested structures, while keeping filters and healthy rows intact.
        pieces[[length(pieces) + 1L]] <- tryCatch(
            shift_inspect__history_batch(path, type),
            error = function(error) {
                id <- basename(path)
                data.table::data.table(
                    type = "batch",
                    id = id,
                    status = "unavailable",
                    batch_id = id,
                    method = NA_character_,
                    model = NA_character_,
                    updated_at = NA_character_,
                    store = path,
                    error = conditionMessage(error)
                )
            }
        )
    }
    out <- data.table::rbindlist(
        c(list(shift_inspect__history_empty()), pieces),
        fill = TRUE
    )
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
    data.table::setorderv(
        out,
        c("updated_at", "type", "id"),
        c(-1L, 1L, 1L),
        na.last = TRUE
    )
    out[]
}

# Read optional comparison statistics one file at a time. EPW missing sentinels
# become NA before sums/counts are accumulated, so means are weighted by valid
# hourly observations, including multi-year outputs with different year lengths.
shift_inspect__weather <- function(paths) {
    fields <- c(
        "dry_bulb_temperature",
        "relative_humidity",
        "wind_speed",
        "global_horizontal_radiation"
    )
    sums <- counts <- stats::setNames(rep(0, length(fields)), fields)
    unreadable <- 0L
    hours <- 0L
    paths <- unique(paths)
    errors <- rep(NA_character_, length(paths))
    reporter <- shift_run__current_reporter()
    for (index in seq_along(paths)) {
        path <- paths[[index]]
        if (!is.null(reporter)) {
            reporter$unit_started(
                paste("Reading", basename(path)),
                current = index,
                total = length(paths),
                details = list(unit_type = "epw_summary")
            )
        }
        weather <- tryCatch(
            epw_file__calculation_weather(epw_file_read(path)$data(), fields),
            error = identity
        )
        if (inherits(weather, "error")) {
            unreadable <- unreadable + 1L
            errors[[index]] <- conditionMessage(weather)
            if (!is.null(reporter)) {
                reporter$unit_completed(
                    paste("Cannot read", basename(path)),
                    current = index,
                    total = length(paths),
                    outcome = "failed"
                )
            }
            next
        }
        hours <- hours + nrow(weather)
        for (field in fields) {
            values <- as.numeric(weather[[field]])
            valid <- is.finite(values)
            sums[[field]] <- sums[[field]] + sum(values[valid])
            counts[[field]] <- counts[[field]] + sum(valid)
        }
        if (!is.null(reporter)) {
            reporter$unit_completed(
                paste("Read", basename(path)),
                current = index,
                total = length(paths)
            )
        }
    }
    errors <- errors[!is.na(errors)]
    means <- sums / counts
    means[counts == 0] <- NA_real_
    labels <- c(
        "mean_temperature_c",
        "temperature_hours",
        "mean_relative_humidity_pct",
        "humidity_hours",
        "mean_wind_speed_ms",
        "wind_hours",
        "mean_global_horizontal_radiation_wh_m2",
        "radiation_hours"
    )
    c(
        list(weather_hours = hours, unreadable_files = unreadable),
        stats::setNames(as.list(as.vector(rbind(means, counts))), labels),
        list(
            weather_error = if (length(errors)) {
                paste(unique(errors), collapse = "; ")
            } else {
                NA_character_
            }
        )
    )
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
        } else {
            data.table::data.table()
        }
        morphed <- if (S7::S7_inherits(result, ShiftOutputs)) {
            result@meta$morphed
        } else {
            result
        }
        if (
            S7::S7_inherits(morphed, ShiftMorphed) &&
                S7::S7_inherits(morphed@meta$transform, WeatherTransformSpec)
        ) {
            transform <- transform__spec_value(morphed@meta$transform)
            for (field in c("method", "scale", "reconstruction")) {
                identity[[field]] <- store__chr1(transform[[field]])
            }
        }
    } else {
        outputs <- shift_outputs(child, refresh = FALSE)
    }
    diagnostics <- shift_diagnostics(child, refresh = FALSE)
    dimensions <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "period"
    )
    if (!nrow(cases) && nrow(outputs)) {
        # Standalone tasks do not write workflow cases. Count distinct output
        # cases instead of weather-year files, keeping every scientific group.
        columns <- intersect(c(dimensions, "case_id"), names(outputs))
        cases <- unique(outputs[, columns, with = FALSE])
        data.table::set(cases, j = "status", value = "completed")
    }
    groups <- intersect(dimensions, names(cases))
    # Store integer row groups instead of copying one table for each partition.
    partitions <- if (nrow(cases) && length(groups)) {
        cases[, list(rows = list(.I)), by = groups]
    } else {
        data.table::data.table(rows = list(seq_len(nrow(cases))))
    }
    data.table::set(
        partitions,
        j = "summary_group",
        value = seq_len(nrow(partitions))
    )
    keys <- intersect(groups, names(outputs))
    if (nrow(cases) && length(keys)) {
        owners <- partitions[, c(keys, "summary_group"), with = FALSE]
        source <- data.table::copy(outputs[, keys, with = FALSE])
        data.table::set(
            source,
            j = "output_row",
            value = seq_len(nrow(outputs))
        )
        links <- merge(
            owners,
            source,
            by = keys,
            sort = FALSE,
            allow.cartesian = TRUE
        )
    } else if (nrow(cases) && "case_id" %in% names(outputs)) {
        owners <- data.table::data.table(
            case_id = cases$case_id[unlist(partitions$rows, use.names = FALSE)],
            summary_group = rep(
                partitions$summary_group,
                lengths(partitions$rows)
            )
        )
        source <- data.table::data.table(
            case_id = outputs$case_id,
            output_row = seq_len(nrow(outputs))
        )
        links <- unique(merge(
            owners,
            source,
            by = "case_id",
            sort = FALSE,
            allow.cartesian = TRUE
        ))
    } else {
        links <- data.table::CJ(
            summary_group = partitions$summary_group,
            output_row = seq_len(nrow(outputs))
        )
    }
    file_rows <- split(
        links$output_row,
        factor(links$summary_group, levels = partitions$summary_group)
    )
    file_rows <- lapply(file_rows, sort)
    # Resolve each path and query filesystem existence once, independently of
    # the number of scientific groups which consume that file.
    source_paths <- unique(outputs$path)
    absolute <- vapply(
        source_paths,
        store_abs_path,
        character(1L),
        root = child@store_path
    )
    paths <- unname(absolute[match(outputs$path, source_paths)])
    if ("export_path" %in% names(outputs)) {
        exported <- outputs$export_path
        present <- !is.na(exported) & nzchar(exported) & file.exists(exported)
        paths[present] <- exported[present]
    }
    present <- file.exists(paths)
    status <- shift_status(child, refresh = FALSE)
    # Link both case-ID namespaces once; global diagnostics apply to every
    # group, while duplicate links must never inflate warning/error counts.
    if (nrow(diagnostics) && "case_id" %in% names(diagnostics)) {
        owners <- data.table::data.table(
            case_id = cases$case_id[unlist(partitions$rows, use.names = FALSE)],
            summary_group = rep(
                partitions$summary_group,
                lengths(partitions$rows)
            )
        )
        if ("case_id" %in% names(outputs)) {
            owners <- unique(data.table::rbindlist(list(
                owners,
                data.table::data.table(
                    case_id = outputs$case_id[links$output_row],
                    summary_group = links$summary_group
                )
            )))
        }
        source <- data.table::data.table(
            case_id = diagnostics$case_id,
            diagnostic_row = seq_len(nrow(diagnostics))
        )
        global <- which(is.na(source$case_id))
        linked <- unique(merge(
            owners[!is.na(owners$case_id)],
            source[!is.na(source$case_id)],
            by = "case_id",
            sort = FALSE,
            allow.cartesian = TRUE
        ))
        check_rows <- split(
            linked$diagnostic_row,
            factor(linked$summary_group, levels = partitions$summary_group)
        )
        check_rows <- lapply(check_rows, function(rows) {
            sort(unique(c(global, rows)))
        })
    } else {
        check_rows <- rep(list(seq_len(nrow(diagnostics))), nrow(partitions))
    }
    rows <- lapply(seq_len(nrow(partitions)), function(index) {
        group <- cases[partitions$rows[[index]]]
        indices <- file_rows[[index]]
        files <- outputs[indices]
        checks <- diagnostics[check_rows[[index]]]
        years <- sort(unique(files$weather_year[!is.na(files$weather_year)]))
        completion <- shift_inspect__completion(group, files, checks)
        row <- identity
        row$model <- shift_stage__coalesce(
            if (nrow(group)) group$source_id[[1L]] else NULL,
            identity$model
        )
        fields <- c(
            scenario = "experiment_id",
            member = "variant_label",
            grid = "grid_label",
            period = "period"
        )
        row[names(fields)] <- lapply(fields, function(field) {
            store__chr1(group[[field]])
        })
        row <- c(
            row,
            list(
                status = status,
                cases = nrow(group),
                completed_cases = sum(group$status == "completed"),
                epw_files = nrow(files),
                available_files = sum(present[indices]),
                output_type = paste(unique(files$output_type), collapse = ", "),
                weather_years = paste(years, collapse = ", "),
                warnings = sum(checks$severity == "warning"),
                errors = sum(checks$severity == "error"),
                field_roles = shift_stage__coalesce(
                    completion$field_summary,
                    NA_character_
                )
            )
        )
        if (weather) {
            row <- c(row, shift_inspect__weather(paths[indices]))
        }
        row
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
#' @return A data.table with one row per site (when supplied), method, model, scenario and period
#'   (and member/grid where applicable), counts, field roles and optional means.
#' @export
shift_summary <- function(
    x,
    store = NULL,
    refresh = TRUE,
    weather = FALSE,
    ui = shift_ui()
) {
    checkmate::assert_flag(refresh)
    checkmate::assert_flag(weather)
    if (is.character(x)) {
        x <- shift_run_get(x, store)
    }
    if (
        !S7::S7_inherits(x, ShiftBatch) &&
            !S7::S7_inherits(x, ShiftRun) &&
            !S7::S7_inherits(x, ShiftPlan)
    ) {
        cli::cli_abort(
            "`x` must be a ShiftBatch, ShiftRun, ShiftPlan, or saved run ID."
        )
    }
    if (refresh && !S7::S7_inherits(x, ShiftPlan)) {
        x <- shift_refresh(x)
    }
    if (weather && is.null(shift_run__current_reporter())) {
        return(shift_reporter__ui_check(
            ui,
            "Summarize EPWs",
            function(reporter) {
                shift_run__with_reporter(
                    reporter,
                    shift_summary(x, refresh = FALSE, weather = TRUE, ui = ui)
                )
            }
        ))
    }
    if (S7::S7_inherits(x, ShiftBatch)) {
        columns <- c(
            "site_id",
            "child_key",
            "method",
            "scale",
            "reconstruction",
            "model"
        )
        manifest <- x@meta$manifest[, columns, with = FALSE]
        rows <- lapply(seq_along(x@meta$children), function(index) {
            identity <- lapply(manifest, `[[`, index)
            identity$batch_id <- x@ids$batch_id
            shift_inspect__summary_child(
                x@meta$children[[index]],
                identity,
                weather
            )
        })
        return(data.table::rbindlist(rows, fill = TRUE))
    }
    spec <- if (S7::S7_inherits(x, ShiftPlan)) {
        shift_persist__plan_spec(x)
    } else {
        jsonlite::fromJSON(x@meta$run$spec_json[[1L]], simplifyVector = TRUE)
    }
    transform <- spec$transform
    shift_inspect__summary_child(
        x,
        list(
            method = store__chr1(transform$method),
            scale = store__chr1(transform$scale),
            reconstruction = store__chr1(transform$reconstruction),
            model = NA_character_,
            run_id = store__chr1(x@ids$run_id)
        ),
        weather
    )
}


shift_inspect__query_run <- function(store, query_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            "SELECT * FROM query_run WHERE query_id IN (%s)",
            shift_stage__query_ids(query_id)
        )
    )
}

shift_inspect__file_catalog <- function(store, query_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            "SELECT * FROM file_catalog WHERE query_id IN (%s)",
            shift_stage__query_ids(query_id)
        )
    )
}

# Summarize a persisted File catalog without materializing every record merely
# to print a ShiftFiles object.
shift_inspect__file_catalog_summary <- function(store, query_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            paste(
                "SELECT COUNT(*) AS file_count,",
                "COALESCE(SUM(size), 0) AS total_size",
                "FROM file_catalog WHERE query_id IN (%s)"
            ),
            shift_stage__query_ids(query_id)
        )
    )
}

# Read only the ordered rows needed for a console preview. An explicit infinite
# limit remains available for users who deliberately request the full print.
shift_inspect__file_catalog_preview <- function(store, query_id, n = 10L) {
    columns <- paste(
        c(
            "source_id",
            "experiment_id",
            "variable_id",
            "variant_label",
            "grid_label",
            "table_id",
            "datetime_start",
            "datetime_end",
            "size",
            "filename",
            "data_node"
        ),
        collapse = ", "
    )
    limit <- if (is.infinite(n)) "" else sprintf(" LIMIT %d", as.integer(n))
    shift_stage__query_maybe(
        store,
        sprintf(
            paste0(
                "SELECT ",
                columns,
                " FROM file_catalog WHERE query_id IN (%s)",
                " ORDER BY source_id, experiment_id, variable_id, variant_label,",
                " grid_label, table_id, datetime_start, filename",
                limit
            ),
            shift_stage__query_ids(query_id)
        )
    )
}

shift_inspect__extraction_plan <- function(store, plan_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            paste(
                "SELECT plan_id, query_id, file_key, site_id, variable_id,",
                "lon, lat, method, time_start, time_stop, status,",
                "available_time_count, attempt_count, last_error, created_at, updated_at",
                "FROM extraction_plan WHERE plan_id IN (%s)"
            ),
            shift_stage__query_ids(plan_id)
        )
    )
}

shift_inspect__extraction_result_rows <- function(store, plan_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            paste(
                "SELECT r.*,",
                "p.site_id,",
                "f.source_id, f.experiment_id, f.variant_label, f.frequency,",
                "p.variable_id",
                "FROM extraction_result r",
                "LEFT JOIN extraction_plan p ON r.plan_id = p.plan_id",
                "LEFT JOIN file_catalog f ON p.query_id = f.query_id AND p.file_key = f.file_key",
                "WHERE r.plan_id IN (%s)",
                "ORDER BY p.variable_id, r.year, r.output_path"
            ),
            shift_stage__query_ids(plan_id)
        )
    )
}

shift_inspect__morph_plan <- function(store, morph_id) {
    shift_stage__query_maybe(
        store,
        sprintf(
            "SELECT * FROM epw_morph_plan WHERE morph_id IN (%s)",
            shift_stage__query_ids(morph_id)
        )
    )
}

shift_inspect__morph_result_rows <- function(store, morph_id, case_id = NULL) {
    sql <- sprintf(
        "SELECT * FROM epw_morph_result WHERE morph_id IN (%s)",
        shift_stage__query_ids(morph_id)
    )
    if (!is.null(case_id)) {
        sql <- paste(
            sql,
            sprintf("AND case_id IN (%s)", shift_stage__query_ids(case_id))
        )
    }
    shift_stage__query_maybe(store, paste(sql, "ORDER BY case_id, output_path"))
}


# Select only requested EPW records and preserve stable case/path ordering.
shift_inspect__epw_output_rows <- function(store, morph_id, case_id = NULL) {
    sql <- sprintf(
        "SELECT * FROM epw_output WHERE morph_id IN (%s)",
        shift_stage__query_ids(morph_id)
    )
    if (!is.null(case_id)) {
        sql <- paste(
            sql,
            sprintf("AND case_id IN (%s)", shift_stage__query_ids(case_id))
        )
    }
    shift_stage__query_maybe(store, paste(sql, "ORDER BY case_id, path"))
}


shift_inspect__artifact_rows <- function(store, artifact_id) {
    artifact_id <- unique(as.character(artifact_id))
    artifact_id <- artifact_id[!is.na(artifact_id) & nzchar(artifact_id)]
    if (!length(artifact_id)) {
        return(data.table::data.table())
    }
    shift_stage__query_maybe(
        store,
        sprintf(
            "SELECT * FROM artifact WHERE artifact_id IN (%s)",
            shift_stage__query_ids(artifact_id)
        )
    )
}

shift_inspect__relative_paths_exist <- function(store, paths) {
    paths <- as.character(paths)
    paths <- paths[!is.na(paths) & nzchar(paths)]
    length(paths) > 0L && all(file.exists(file.path(store$path, paths)))
}

shift_inspect__data_limit <- function(n) {
    if (is.null(n) || identical(n, Inf)) {
        return(Inf)
    }
    checkmate::assert_count(n, positive = FALSE)
    if (is.na(n)) {
        cli::cli_abort("`n` cannot be missing.")
    }
    as.integer(n)
}

shift_inspect__read_parquet <- function(store, path, n = Inf, columns = NULL) {
    conn <- morpher__private_store(store)$conn
    select <- if (is.null(columns)) {
        "*"
    } else {
        paste(
            vapply(
                columns,
                function(column) ddb_ident(conn, column),
                character(1L)
            ),
            collapse = ", "
        )
    }
    sql <- sprintf(
        "SELECT %s FROM read_parquet(%s)",
        select,
        ddb_literal(conn, path)
    )
    if (!is.infinite(n)) {
        sql <- paste(sql, sprintf("LIMIT %d", n))
    }
    data.table::as.data.table(ddb_query(conn, sql))
}

shift_inspect__select_data_columns <- function(dt, columns, stage) {
    if (is.null(columns)) {
        return(dt)
    }
    unknown <- setdiff(columns, names(dt))
    if (length(unknown)) {
        cli::cli_abort("Unknown {stage} data column(s): {.val {unknown}}.")
    }
    dt[, columns, with = FALSE]
}

shift_inspect__add_constant_columns <- function(dt, values) {
    for (name in names(values)) {
        data.table::set(dt, j = name, value = values[[name]])
    }
    data.table::setcolorder(
        dt,
        c(names(values), setdiff(names(dt), names(values)))
    )
    dt
}

# Read ordered artifact records under one global row allowance.
shift_inspect__read_artifact_rows <- function(
    store,
    records,
    n,
    columns = NULL,
    path_column,
    reader,
    metadata = NULL,
    missing,
    stage
) {
    checkmate::assert_choice(path_column, names(records))
    checkmate::assert_function(reader)
    checkmate::assert_function(metadata, null.ok = TRUE)
    checkmate::assert_character(missing, any.missing = FALSE, min.len = 1L)
    checkmate::assert_string(stage, min.chars = 1L)

    pieces <- vector("list", nrow(records))
    remaining <- n
    for (i in seq_len(nrow(records))) {
        if (!is.infinite(remaining) && remaining <= 0L) {
            break
        }
        path <- store_abs_path(records[[path_column]][[i]], root = store$path)
        if (!file.exists(path)) {
            cli::cli_abort(missing)
        }

        limit <- if (is.infinite(remaining)) Inf else remaining
        dt <- data.table::as.data.table(reader(path, limit, columns))
        if (!is.null(metadata)) {
            # Add record-level identity before selecting requested columns so
            # callers may request both stored and manifest-backed fields.
            dt <- shift_inspect__add_constant_columns(dt, metadata(records, i))
        }
        pieces[[i]] <- shift_inspect__select_data_columns(dt, columns, stage)
        if (!is.infinite(remaining)) {
            remaining <- remaining - nrow(dt)
        }
    }

    pieces <- Filter(Negate(is.null), pieces)
    if (!length(pieces)) {
        return(data.table::data.table())
    }
    data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

# Read morphed Parquet artifacts with their persisted result identity columns.
shift_inspect__read_morph_data <- function(store, results, n, columns) {
    shift_inspect__read_artifact_rows(
        store,
        results,
        n = n,
        columns = columns,
        path_column = "output_path",
        reader = function(path, limit, columns) {
            shift_inspect__read_parquet(store, path, n = limit)
        },
        metadata = function(records, i) {
            list(
                result_id = records$result_id[[i]],
                morph_id = records$morph_id[[i]],
                case_id = records$case_id[[i]],
                output_path = records$output_path[[i]],
                output_type = records$output_type[[i]],
                sequence_id = records$sequence_id[[i]],
                weather_year = records$weather_year[[i]],
                calendar = records$calendar[[i]],
                stochastic_seed = records$stochastic_seed[[i]]
            )
        },
        missing = c(
            "Morphed Parquet data file is missing.",
            "x" = "{.path {path}}",
            "i" = "Run {.fn shift_morph} again or inspect {.fn shift_artifacts}."
        ),
        stage = "morphed"
    )
}

# Read EPW artifacts with output-manifest identity and bounded weather rows.
shift_inspect__read_epw_output_data <- function(store, outputs, n, columns) {
    shift_inspect__read_artifact_rows(
        store,
        outputs,
        n = n,
        columns = columns,
        path_column = "path",
        reader = function(path, limit, columns) {
            dt <- epw_file_read(path)$data()
            if (!is.infinite(limit)) {
                dt <- utils::head(dt, limit)
            }
            dt
        },
        metadata = function(records, i) {
            list(
                output_id = records$output_id[[i]],
                morph_id = records$morph_id[[i]],
                case_id = records$case_id[[i]],
                source_id = records$source_id[[i]],
                experiment_id = records$experiment_id[[i]],
                variant_label = records$variant_label[[i]],
                period = records$period[[i]],
                output_type = records$output_type[[i]],
                sequence_id = records$sequence_id[[i]],
                weather_year = records$weather_year[[i]],
                calendar = records$calendar[[i]],
                stochastic_seed = records$stochastic_seed[[i]],
                path = records$path[[i]]
            )
        },
        missing = c(
            "EPW output file is missing.",
            "x" = "{.path {path}}",
            "i" = "Run {.fn shift_epw} again or inspect {.fn shift_outputs}."
        ),
        stage = "EPW output"
    )
}

shift_inspect__stage_query_result <- function(
    store,
    query_id,
    result_type = NULL
) {
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_choice(
        result_type,
        c("File", "Aggregation"),
        null.ok = TRUE
    )

    runs <- shift_inspect__query_run(store, query_id)
    if (!nrow(runs)) {
        cli::cli_abort(
            "No stored File query result was found for this shift stage."
        )
    }

    run <- runs[1L]
    if (
        !is.null(result_type) && !identical(run$result_type[[1L]], result_type)
    ) {
        cli::cli_abort(
            "The stored query result has type {.val {run$result_type[[1L]]}}, not {.val {result_type}}."
        )
    }

    query_file <- file.path(store$path, run$query_file[[1L]])
    if (!file.exists(query_file)) {
        cli::cli_abort(
            "The stored query result file no longer exists: {.path {query_file}}."
        )
    }

    schema <- switch(
        run$result_type[[1L]],
        File = SCHEMA_RESULT_FILE,
        Aggregation = SCHEMA_RESULT_AGGREGATION,
        cli::cli_abort(
            "Unsupported stored query result type: {.val {run$result_type[[1L]]}}."
        )
    )
    loaded <- query__load(query_file, schema)
    generator <- switch(
        run$result_type[[1L]],
        File = EsgResultFile,
        Aggregation = EsgResultAggregation,
        cli::cli_abort(
            "Unsupported stored query result type: {.val {run$result_type[[1L]]}}."
        )
    )
    query_result__new(
        generator,
        index_node = loaded$index_node,
        params = loaded$parameter,
        result = loaded$response,
        context = loaded$context
    )
}

#' @rdname shift_api
#' @export
shift_explain <- function(x, ...) {
    # Preserve method/model identity when explaining independent child plans
    # or runs restored from a batch receipt.
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            function(child) shift_explain(child, ...)
        ))
    }
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftPlan)) {
        return(shift_print__plan_explain(x))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
        row <- x@meta$run
        out <- data.table::data.table(
            field = c(
                "run_id",
                "status",
                "current_stage",
                "spec_hash",
                "output_dir",
                "last_error"
            ),
            value = as.character(unlist(
                row[,
                    c(
                        "run_id",
                        "status",
                        "current_stage",
                        "spec_hash",
                        "output_dir",
                        "last_error"
                    ),
                    with = FALSE
                ],
                use.names = FALSE
            ))
        )
        steps <- data.table::as.data.table(x@meta$steps)
        if (nrow(steps)) {
            out <- data.table::rbindlist(
                list(
                    out,
                    data.table::data.table(
                        field = c("steps", "latest_step", "latest_task"),
                        value = c(
                            nrow(steps),
                            steps$step_id[[nrow(steps)]],
                            steps$task[[nrow(steps)]]
                        )
                    )
                ),
                use.names = TRUE
            )
        }
        return(out)
    }
    cli::cli_abort(
        "{.fn shift_explain} expects a {.cls ShiftPlan} or {.cls ShiftRun}."
    )
}

# public inspectors
#' @rdname shift_api
#' @export
shift_refresh <- function(x) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__refresh(x))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        return(shift_run_get(x@ids$run_id, store = x@store_path))
    }
    if (S7::S7_inherits(x, ShiftRequest) || S7::S7_inherits(x, ShiftSite)) {
        return(x)
    }
    x@diagnostics <- shift_stage__diagnostics_empty()
    x@diagnostics <- shift_check(x, strict = FALSE)
    x
}

#' @rdname shift_api
#' @export
shift_ids <- function(x, refresh = TRUE) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(refresh)
    if (S7::S7_inherits(x, ShiftBatch)) {
        if (isTRUE(refresh)) {
            x <- shift_batch__refresh(x)
        }
        return(x@ids)
    }
    if (isTRUE(refresh) && S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
    }
    x@ids
}

#' @rdname shift_api
#' @export
shift_cases <- function(x, refresh = TRUE) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(refresh)
    if (S7::S7_inherits(x, ShiftBatch)) {
        if (isTRUE(refresh)) {
            x <- shift_batch__refresh(x)
        }
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            function(child) shift_cases(child, refresh = FALSE)
        ))
    }
    if (S7::S7_inherits(x, ShiftPlan)) {
        return(data.table::as.data.table(data.table::copy(
            x@meta$expected_cases
        )))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        return(data.table::as.data.table(data.table::copy(x@meta$cases)))
    }
    if (S7::S7_inherits(x, ShiftStage)) {
        return(data.table::data.table())
    }
    cli::cli_abort("{.fn shift_cases} expects a shift stage or persisted run.")
}

#' @rdname shift_api
#' @export
shift_missing <- function(x) {
    cases <- shift_cases(x)
    if (!nrow(cases)) {
        return(cases)
    }
    cases[required %in% TRUE & !status %in% "completed"]
}

#' @rdname shift_api
#' @export
shift_runs <- function(store = NULL) {
    shift_inspect__runs(store)
}

# Share read-only run inspection while allowing batch history to request only
# its own runs. Locked stores use the same saved live snapshots as public history.
shift_inspect__runs <- function(store, run_ids = NULL) {
    store_value <- shift_stage__coalesce(store, store_dir(init = FALSE))
    store_path <- if (inherits(store_value, "EsgStore")) {
        store_value$path
    } else {
        normalizePath(path.expand(store_value), winslash = "/", mustWork = TRUE)
    }
    opened <- tryCatch(
        shift_store(store_value, create = FALSE),
        error = function(e) e
    )
    if (!inherits(opened, "error")) {
        if (!inherits(store_value, "EsgStore")) {
            on.exit(try(opened$close(), silent = TRUE), add = TRUE)
        }
        rows <- if (is.null(run_ids)) {
            morpher__private_store(opened)$read_table("shift_run")
        } else {
            shift_inspect__rows(opened, "shift_run", "run_id", run_ids)
        }
        data.table::setorderv(rows, "started_at", -1L, na.last = TRUE)
        return(rows)
    }
    if (!shift_job__manifest_locked(opened)) {
        stop(opened)
    }
    live <- list.files(
        file.path(store_path, "logs", "shift"),
        pattern = "[.]live[.]json$",
        full.names = TRUE
    )
    if (!is.null(run_ids)) {
        live <- live[basename(live) %in% paste0(run_ids, ".live.json")]
    }
    rows <- lapply(live, function(path) {
        value <- tryCatch(
            jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
            error = function(e) NULL
        )
        if (is.null(value)) NULL else shift_job__live_table(value$run)
    })
    rows <- Filter(function(x) !is.null(x) && nrow(x), rows)
    if (!length(rows)) {
        stop(opened)
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)[order(
        -started_at
    )]
}

#' @rdname shift_api
#' @param run_id Persisted workflow run ID.
#' @export
shift_run_get <- function(run_id, store = NULL) {
    if (inherits(run_id, "EsgResultDataset")) {
        result <- run_id
        value <- attr(result, "epwshiftr.run_id", exact = TRUE)
        if (
            is.null(value) ||
                !length(value) ||
                is.na(value[[1L]]) ||
                !nzchar(value[[1L]])
        ) {
            cli::cli_abort(
                "This Dataset result is not associated with a persisted shift run."
            )
        }
        store <- shift_stage__coalesce(
            store,
            attr(result, "epwshiftr.store", exact = TRUE)
        )
        run_id <- as.character(value[[1L]])
    }
    if (S7::S7_inherits(run_id, ShiftStage)) {
        stage <- run_id
        value <- stage@ids$run_id
        if (
            is.null(value) ||
                !length(value) ||
                is.na(value[[1L]]) ||
                !nzchar(value[[1L]])
        ) {
            cli::cli_abort(
                "This shift stage is not associated with a persisted run."
            )
        }
        store <- shift_stage__coalesce(store, stage@store_path)
        run_id <- as.character(value[[1L]])
    }
    checkmate::assert_string(run_id, min.chars = 1L)
    store_value <- shift_stage__coalesce(store, store_dir(init = FALSE))
    store_path <- if (inherits(store_value, "EsgStore")) {
        store_value$path
    } else {
        normalizePath(path.expand(store_value), winslash = "/", mustWork = TRUE)
    }
    if (!inherits(store_value, "EsgStore")) {
        live <- shift_job__live_run_get(run_id, store_path)
        if (shift_job__live_process_is_active(live)) {
            # Active process jobs publish authoritative live state. Avoiding a
            # speculative DuckDB read here also prevents status/watch calls
            # from racing a newly launched worker for the manifest lock.
            return(live)
        }
    }
    opened <- tryCatch(
        shift_store(store_value, create = FALSE),
        error = function(e) e
    )
    if (inherits(opened, "error")) {
        if (!shift_job__manifest_locked(opened)) {
            stop(opened)
        }
        live <- shift_job__live_run_get(run_id, store_path)
        if (!is.null(live)) {
            return(live)
        }
        stop(opened)
    }
    if (!inherits(store_value, "EsgStore")) {
        on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    }
    shift_job__reconcile_background_download(opened, run_id)
    shift_job__reconcile_run_job(opened, run_id)
    shift_job__run_handle(opened, run_id)
}

# Reconstruct the latest completed standalone result from its persisted stage
# reference. Future EPW runs continue to return their existing output stage
# when it is available on the in-process handle.
#' @rdname shift_api
#' @export
shift_result <- function(x, store = NULL) {
    run <- if (S7::S7_inherits(x, ShiftRun)) {
        shift_refresh(x)
    } else {
        shift_run_get(x, store = store)
    }
    if (S7::S7_inherits(run@meta$output_stage, ShiftStage)) {
        stage <- run@meta$output_stage
        if (S7::S7_inherits(stage, ShiftDatasets)) {
            return(shift_run__datasets_attach_run(
                shift_run__datasets_result(stage),
                stage
            ))
        }
        return(stage)
    }
    opened <- shift_store(run)
    on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    step <- shift_job__latest_step(opened, run@ids$run_id, completed = TRUE)
    if (!nrow(step)) {
        cli::cli_abort(
            "Shift run {.val {run@ids$run_id}} has no completed stage result."
        )
    }
    ref <- jsonlite::fromJSON(
        step$output_stage_json[[1L]],
        simplifyVector = FALSE
    )
    stage <- shift_persist__stage_from_ref(ref)
    if (S7::S7_inherits(stage, ShiftDatasets)) {
        return(shift_run__datasets_attach_run(
            shift_run__datasets_result(stage),
            stage
        ))
    }
    stage
}

#' @rdname shift_api
#' @param tail Maximum number of trailing execution log lines to return.
#' @export
shift_logs <- function(x, store = NULL, tail = 100L) {
    checkmate::assert_count(tail, positive = FALSE)
    if (S7::S7_inherits(x, ShiftBatch)) {
        job <- shift_batch_execution__job_read(x@store_path)
        if (!is.null(job) && isTRUE(job$background)) {
            # The coordinator captures shared reads and child stdout together;
            # return it once instead of repeating it for every child.
            path <- file.path(x@store_path, paste0(job$id, ".log"))
            lines <- if (file.exists(path)) {
                utils::tail(readLines(path, warn = FALSE), tail)
            } else {
                character()
            }
            return(data.table::data.table(
                job_id = rep(job$id, length(lines)),
                source = rep("process", length(lines)),
                line = seq_along(lines),
                message = lines
            ))
        }
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            function(child) {
                if (S7::S7_inherits(child, ShiftPlan)) {
                    return(data.table::data.table())
                }
                shift_logs(child, tail = tail)
            }
        ))
    }
    run <- shift_job__as_run(x, store = store)
    run_store <- shift_store(run)
    download_context <- shift_job__background_download_context(
        run_store,
        run@ids$run_id,
        active_only = FALSE
    )
    if (!is.null(download_context) && nrow(download_context$jobs)) {
        downloader_job_id <- as.character(download_context$jobs$job_id[[
            nrow(download_context$jobs)
        ]])
        downloader_logs <- data.table::as.data.table(
            download_context$downloader$job_logs(downloader_job_id, tail = tail)
        )
        run_store$close()
        if (nrow(downloader_logs)) {
            downloader_logs[, source := "downloader"]
            # Character column selection avoids data.table's NSE here so R CMD
            # check does not mistake Downloader log fields for global symbols.
            return(downloader_logs[,
                c("job_id", "source", "line", "message"),
                with = FALSE
            ])
        }
    } else {
        run_store$close()
    }
    jobs <- data.table::as.data.table(run@meta$jobs)
    job <- if (nrow(jobs)) jobs[which.max(jobs[["attempt"]])] else jobs
    if (!nrow(job)) {
        return(data.table::data.table())
    }
    path <- as.character(job$log_path[[1L]])
    has_file_log <- !is.na(path) && nzchar(path) && file.exists(path)
    foreground_events <- identical(as.character(job$mode[[1L]]), "foreground")
    lines <- if (has_file_log) {
        readLines(path, warn = FALSE)
    } else if (foreground_events) {
        # Foreground attempts have no redirected stdout file. Their durable
        # workflow events remain a useful execution log and make the failure
        # hint valid for both foreground and background runs.
        event_rows <- data.table::as.data.table(run@meta$events)
        if (tail == 0L || !nrow(event_rows)) {
            character()
        } else {
            event_rows <- utils::tail(event_rows, tail)
            detail <- tryCatch(
                {
                    value <- jsonlite::fromJSON(
                        job$ui_json[[1L]],
                        simplifyVector = TRUE
                    )
                    as.character(shift_stage__coalesce(value$detail, "normal"))
                },
                error = function(e) "normal"
            )
            vapply(
                seq_len(nrow(event_rows)),
                function(i) {
                    shift_ui_view__ui_persisted_event_line(
                        event_rows[i],
                        detail = detail,
                        width = NULL
                    )
                },
                character(1L)
            )
        }
    } else {
        character()
    }
    if (has_file_log) {
        lines <- utils::tail(lines, tail)
    }
    data.table::data.table(
        job_id = rep(job$job_id[[1L]], length(lines)),
        source = rep(if (has_file_log) "process" else "event", length(lines)),
        line = seq_along(lines),
        message = lines
    )
}

#' @rdname shift_api
#' @export
shift_files <- function(x) {
    shift_stage__assert_stage(x)
    ids <- shift_ids(x)
    if (
        is.null(ids$query_id) ||
            !length(ids$query_id) ||
            is.na(ids$query_id[[1L]])
    ) {
        cli::cli_abort(
            "No File result is available before {.fn shift_collect}."
        )
    }

    store <- shift_store(x)
    shift_inspect__stage_query_result(
        store,
        ids$query_id[[1L]],
        result_type = "File"
    )
}

#' @rdname shift_api
#' @param n Maximum number of data rows to read. Use `Inf` to read all rows.
#' @param case_id Optional morphing case IDs to read from morphed or EPW output
#'   stages.
#' @param columns Optional data columns to keep.
#' @param refresh In [shift_control()], whether to refresh remote catalogs and
#'   service addresses instead of resuming persisted inputs. In [shift_ui()],
#'   minimum seconds between visual animation frames. In `ShiftRun` inspectors,
#'   whether to reload persisted state first.
#' @export
shift_data <- function(
    x,
    n = 100L,
    variables = NULL,
    case_id = NULL,
    columns = NULL,
    refresh = TRUE
) {
    shift_stage__assert_stage(x)
    n <- shift_inspect__data_limit(n)
    checkmate::assert_character(
        variables,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        case_id,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        columns,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (S7::S7_inherits(x, ShiftBatch)) {
        # Read independent child stores with one overall row limit. Identity
        # columns belong to the batch manifest, not the child Parquet schema.
        if (isTRUE(refresh)) {
            x <- shift_batch__refresh(x)
        }
        identity <- c(
            "site_id",
            "child_key",
            "method",
            "scale",
            "reconstruction",
            "model",
            "member",
            "grid"
        )
        child_columns <- if (is.null(columns)) {
            NULL
        } else {
            setdiff(columns, identity)
        }
        if (!length(child_columns)) {
            child_columns <- NULL
        }
        rows <- list()
        remaining <- n
        for (index in seq_along(x@meta$children)) {
            if (remaining <= 0) {
                break
            }
            child <- x@meta$children[[index]]
            if (S7::S7_inherits(child, ShiftPlan)) {
                next
            }
            value <- shift_data(
                child,
                n = remaining,
                variables = variables,
                case_id = case_id,
                columns = child_columns,
                refresh = FALSE
            )
            value <- shift_batch__decorate(value, x@meta$manifest[index])
            if (nrow(value) && !is.null(columns)) {
                value <- value[,
                    intersect(c(identity, columns), names(value)),
                    with = FALSE
                ]
            }
            rows[[index]] <- value
            remaining <- remaining - nrow(value)
        }
        return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        if (
            isTRUE(x@meta$live) &&
                shift_status(x, refresh = FALSE) %in%
                    c("queued", "running", "stopping")
        ) {
            return(data.table::data.table())
        }
        if (!identical(as.character(x@meta$run$task[[1L]]), "future_epw")) {
            stage <- tryCatch(shift_result(x), error = function(e) NULL)
            supported <- !is.null(stage) &&
                any(vapply(
                    list(ShiftClimate, ShiftMorphed, ShiftOutputs),
                    function(class) S7::S7_inherits(stage, class),
                    logical(1L)
                ))
            if (!isTRUE(supported)) {
                return(data.table::data.table())
            }
            return(shift_data(
                stage,
                n = n,
                variables = variables,
                case_id = case_id,
                columns = columns,
                refresh = FALSE
            ))
        }
        stage <- x@meta$output_stage
        if (!S7::S7_inherits(stage, ShiftOutputs)) {
            morph_id <- x@ids$morph_id
            if (is.na(morph_id) || !nzchar(morph_id)) {
                return(data.table::data.table())
            }
            stage <- shift_stage__new(
                ShiftOutputs,
                "epw",
                store_path = x@store_path,
                ids = list(morph_id = morph_id),
                meta = list(outputs = shift_outputs(x))
            )
        }
        return(shift_data(
            stage,
            n = n,
            variables = variables,
            case_id = case_id,
            columns = columns,
            refresh = FALSE
        ))
    }
    if (
        !S7::S7_inherits(x, ShiftClimate) &&
            !S7::S7_inherits(x, ShiftMorphed) &&
            !S7::S7_inherits(x, ShiftOutputs)
    ) {
        cli::cli_abort(
            "{.fn shift_data} reads data from {.cls ShiftClimate}, {.cls ShiftMorphed}, or {.cls ShiftOutputs} stages."
        )
    }
    if (identical(n, 0L)) {
        return(data.table::data.table())
    }

    ids <- shift_ids(x)
    store <- shift_store(x)

    if (S7::S7_inherits(x, ShiftClimate)) {
        if (!is.null(case_id)) {
            cli::cli_abort(
                "`case_id` is only supported for morphed and EPW output stages."
            )
        }
        if (is.null(ids$plan_id) || !length(ids$plan_id)) {
            return(data.table::data.table())
        }
        results <- shift_inspect__extraction_result_rows(store, ids$plan_id)
        if (!is.null(variables)) {
            results <- results[results[["variable_id"]] %in% variables]
        }
        if (!nrow(results)) {
            return(data.table::data.table())
        }

        # Keep Parquet projection in DuckDB for extracted climate while the
        # shared artifact loop owns ordering and the cross-file row allowance.
        return(shift_inspect__read_artifact_rows(
            store,
            results,
            n = n,
            columns = columns,
            path_column = "output_path",
            reader = function(path, limit, columns) {
                shift_inspect__read_parquet(
                    store,
                    path,
                    n = limit,
                    columns = columns
                )
            },
            missing = c(
                "Extracted Parquet data file is missing.",
                "x" = "{.path {path}}",
                "i" = "Run {.fn shift_extract} again or inspect {.fn shift_coverage}."
            ),
            stage = "extracted"
        ))
    }

    if (!is.null(variables)) {
        cli::cli_abort(
            "`variables` is only supported for extracted climate stages."
        )
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        if (is.null(ids$morph_id) || !length(ids$morph_id)) {
            return(data.table::data.table())
        }
        results <- shift_inspect__morph_result_rows(
            store,
            ids$morph_id,
            case_id = case_id
        )
        if (!nrow(results)) {
            return(data.table::data.table())
        }
        return(shift_inspect__read_morph_data(
            store,
            results,
            n = n,
            columns = columns
        ))
    }

    if (S7::S7_inherits(x, ShiftOutputs)) {
        if (is.null(ids$morph_id) || !length(ids$morph_id)) {
            return(data.table::data.table())
        }
        outputs <- shift_inspect__epw_output_rows(
            store,
            ids$morph_id,
            case_id = case_id
        )
        if (!nrow(outputs)) {
            return(data.table::data.table())
        }
        return(shift_inspect__read_epw_output_data(
            store,
            outputs,
            n = n,
            columns = columns
        ))
    }

    data.table::data.table()
}

#' @rdname shift_api
#' @param severity Optional diagnostic severities to keep.
#' @export
shift_diagnostics <- function(x, severity = NULL, refresh = TRUE) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(refresh)
    checkmate::assert_character(
        severity,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__diagnostics(
            x@meta$children,
            x@meta$manifest,
            severity = severity,
            refresh = refresh,
            shared_failure = x@meta[["shared_failure"]]
        ))
    }
    if (isTRUE(refresh) && S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
    }
    out <- shift_stage__diagnostics_normalize(x@diagnostics)
    if (!is.null(severity)) {
        out <- out[out$severity %in% severity]
    }
    out[]
}

#' @rdname shift_api
#' @param create Whether to create a store when `x` is a path.
#' @export
shift_store <- function(x, create = FALSE) {
    checkmate::assert_flag(create)
    if (inherits(x, "EsgStore")) {
        return(x)
    }
    if (is.character(x) && length(x) == 1L) {
        return(EsgStore$new(x, create = create))
    }
    shift_stage__assert_stage(x)
    path <- x@store_path
    if (is.null(path) || !nzchar(path)) {
        cli::cli_abort("This shift stage is not associated with an EsgStore.")
    }
    EsgStore$new(path, create = create)
}

#' @rdname shift_api
#' @export
shift_target <- function(x) {
    if (S7::S7_inherits(x, ShiftSite)) {
        return(x)
    }
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_target(x@meta$children[[1L]]))
    }
    meta <- x@meta
    if (S7::S7_inherits(meta$site, ShiftSite)) {
        return(meta$site)
    }
    for (name in c("download", "files", "climate", "morphed")) {
        value <- meta[[name]]
        if (S7::S7_inherits(value, ShiftStage)) {
            target <- tryCatch(shift_target(value), error = function(e) NULL)
            if (!is.null(target)) {
                return(target)
            }
        }
    }
    cli::cli_abort("No shift site target was found for this stage.")
}

#' @rdname shift_api
#' @export
shift_coverage <- function(x) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            shift_coverage
        ))
    }
    if (S7::S7_inherits(x, ShiftClimate)) {
        return(data.table::as.data.table(shift_stage__coalesce(
            x@meta$coverage,
            data.table::data.table()
        )))
    }
    ids <- shift_ids(x)
    if (is.null(ids$plan_id)) {
        return(data.table::data.table())
    }
    store <- shift_store(x)
    store$coverage(plan_id = ids$plan_id)
}

#' @rdname shift_api
#' @export
shift_outputs <- function(x, refresh = TRUE) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(refresh)
    if (S7::S7_inherits(x, ShiftBatch)) {
        if (isTRUE(refresh)) {
            x <- shift_batch__refresh(x)
        }
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            function(child) shift_outputs(child, refresh = FALSE)
        ))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        if (!identical(as.character(x@meta$run$task[[1L]]), "future_epw")) {
            stage <- tryCatch(shift_result(x), error = function(e) NULL)
            if (!S7::S7_inherits(stage, ShiftOutputs)) {
                return(data.table::data.table())
            }
            return(shift_outputs(stage, refresh = FALSE))
        }
        morph_id <- x@ids$morph_id
        if (is.na(morph_id) || !nzchar(morph_id)) {
            return(data.table::data.table())
        }
        run_store <- tryCatch(shift_store(x), error = function(e) NULL)
        outputs <- if (is.null(run_store)) {
            data.table::as.data.table(shift_stage__coalesce(
                x@meta$outputs,
                data.table::data.table()
            ))
        } else {
            shift_inspect__epw_output_rows(run_store, morph_id)
        }
        cases <- shift_cases(x, refresh = FALSE)
        if (nrow(outputs) && nrow(cases)) {
            exports <- cases[!is.na(output_id), .(output_id, export_path)]
            outputs <- merge(
                outputs,
                exports,
                by = "output_id",
                all.x = TRUE,
                sort = FALSE
            )
        }
        return(outputs[])
    }
    if (S7::S7_inherits(x, ShiftOutputs)) {
        return(data.table::as.data.table(shift_stage__coalesce(
            x@meta$outputs,
            data.table::data.table()
        )))
    }
    ids <- shift_ids(x)
    if (is.null(ids$morph_id)) {
        return(data.table::data.table())
    }
    store <- shift_store(x)
    shift_inspect__epw_output_rows(store, ids$morph_id)
}

#' @rdname shift_api
#' @export
shift_artifacts <- function(x) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__inspect(
            x@meta$children,
            x@meta$manifest,
            shift_artifacts
        ))
    }
    ids <- shift_ids(x)

    if (S7::S7_inherits(x, ShiftMorphed) && !is.null(ids$morph_id)) {
        store <- shift_store(x)
        results <- shift_inspect__morph_result_rows(store, ids$morph_id)
        return(shift_inspect__artifact_rows(store, results$artifact_id))
    }

    if (S7::S7_inherits(x, ShiftOutputs) && !is.null(ids$morph_id)) {
        store <- shift_store(x)
        outputs <- shift_inspect__epw_output_rows(store, ids$morph_id)
        return(shift_inspect__artifact_rows(store, outputs$artifact_id))
    }

    ids <- ids[!vapply(ids, is.null, logical(1L))]
    if (!length(ids)) {
        return(data.table::data.table())
    }
    store <- shift_store(x)
    values <- unique(unlist(ids, use.names = FALSE))
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) {
        return(data.table::data.table())
    }
    quoted <- shift_stage__query_ids(values)
    shift_stage__query_maybe(
        store,
        sprintf(
            paste(
                "SELECT * FROM artifact",
                "WHERE query_id IN (%1$s)",
                "OR file_key IN (%1$s)",
                "OR artifact_id IN (%1$s)"
            ),
            quoted
        )
    )
}

#' @rdname shift_api
#' @export
shift_status <- function(x, refresh = TRUE) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(refresh)

    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__status(x, refresh = refresh))
    }

    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        return(as.character(x@meta$run$status[[1L]]))
    }

    if (shift_stage__has_errors(x@diagnostics)) {
        return("blocked")
    }
    if (S7::S7_inherits(x, ShiftRequest) || S7::S7_inherits(x, ShiftSite)) {
        return("new")
    }
    if (S7::S7_inherits(x, ShiftPlan)) {
        return("planned")
    }

    ids <- shift_ids(x)
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (is.null(store)) {
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftDatasets)) {
        path <- store_abs_path(x@meta$result_path, root = store$path)
        count <- as.integer(shift_stage__coalesce(x@meta$dataset_count, 0L))
        return(if (file.exists(path) && count > 0L) "collected" else "partial")
    }

    if (S7::S7_inherits(x, ShiftFiles)) {
        files <- shift_inspect__file_catalog(store, ids$query_id)
        return(if (nrow(files)) "collected" else "partial")
    }

    if (S7::S7_inherits(x, ShiftDownload)) {
        files <- shift_inspect__file_catalog(store, ids$query_id)
        if (!nrow(files)) {
            return("partial")
        }
        if ("local_path" %in% names(files)) {
            has_path <- !is.na(files$local_path) & nzchar(files$local_path)
            if (
                any(has_path) &&
                    all(file.exists(file.path(
                        store$path,
                        files$local_path[has_path]
                    )))
            ) {
                return("downloaded")
            }
        }
        tasks <- if (!is.null(ids$session_id) && !is.na(ids$session_id)) {
            tryCatch(
                store$download_status(session_id = ids$session_id),
                error = function(e) data.table::data.table()
            )
        } else {
            data.table::data.table()
        }
        if (nrow(tasks) && any(tasks$status %in% c("error", "cancelled"))) {
            return("failed")
        }
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftClimate)) {
        coverage <- tryCatch(
            store$coverage(plan_id = ids$plan_id),
            error = function(e) data.table::data.table()
        )
        if (!nrow(coverage)) {
            return("partial")
        }
        if (any(coverage$status %in% "failed")) {
            return("failed")
        }
        if (all(coverage$complete %in% TRUE)) {
            return("extracted")
        }
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        plans <- shift_inspect__morph_plan(store, ids$morph_id)
        if (!nrow(plans)) {
            return("partial")
        }
        status <- unique(plans$status)
        if (any(status %in% "failed")) {
            return("failed")
        }
        if (any(status %in% "blocked")) {
            return("blocked")
        }
        if (all(status %in% c("result_done", "epw_written"))) {
            return("morphed")
        }
        if (any(status %in% c("result_partial", "epw_partial"))) {
            return("partial")
        }
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- shift_outputs(x)
        path_col <- intersect(
            c("path", "output_path", "relative_path"),
            names(outputs)
        )
        if (
            length(path_col) &&
                shift_inspect__relative_paths_exist(
                    store,
                    outputs[[path_col[[1L]]]]
                )
        ) {
            return("written")
        }
        if ("status" %in% names(outputs) && any(outputs$status %in% "failed")) {
            return("failed")
        }
        return(if (nrow(outputs)) "written" else "partial")
    }

    "partial"
}

S7::method(summary, ShiftStage) <- function(object, ...) {
    data.table::data.table(
        class = class(object)[[1L]],
        stage = object@stage,
        status = tryCatch(shift_status(object), error = function(e) "unknown"),
        diagnostic_count = nrow(shift_diagnostics(object))
    )
}

shift_inspect__stage_as_data_table <- function(x, ...) {
    if (S7::S7_inherits(x, ShiftRequest)) {
        filters <- x@meta$filters
        return(data.table::data.table(
            provider = x@meta$provider,
            project = shift_stage__coalesce(x@meta$project, NA_character_),
            source = paste(
                shift_stage__coalesce(x@meta$source, character()),
                collapse = ","
            ),
            experiment = paste(
                shift_stage__coalesce(x@meta$experiment, character()),
                collapse = ","
            ),
            variant = paste(
                shift_stage__coalesce(x@meta$variant, character()),
                collapse = ","
            ),
            variables = paste(
                shift_stage__coalesce(x@meta$variables, character()),
                collapse = ","
            ),
            frequency = paste(
                shift_stage__coalesce(x@meta$frequency, character()),
                collapse = ","
            ),
            filter_count = length(filters)
        ))
    }

    if (S7::S7_inherits(x, ShiftSite)) {
        return(data.table::data.table(
            id = x@id,
            lon = x@lon,
            lat = x@lat,
            label = shift_stage__coalesce(x@label, NA_character_),
            has_epw = !is.null(x@epw)
        ))
    }

    store <- tryCatch(shift_store(x), error = function(e) NULL)
    ids <- shift_ids(x)
    if (S7::S7_inherits(x, ShiftFiles) && !is.null(store)) {
        return(shift_inspect__file_catalog(store, ids$query_id))
    }
    if (S7::S7_inherits(x, ShiftDownload) && !is.null(store)) {
        tasks <- if (!is.null(ids$session_id) && !is.na(ids$session_id)) {
            tryCatch(
                store$download_status(session_id = ids$session_id),
                error = function(e) data.table::data.table()
            )
        } else {
            data.table::data.table()
        }
        return(tasks)
    }
    if (S7::S7_inherits(x, ShiftClimate) && !is.null(store)) {
        return(store$coverage(plan_id = ids$plan_id))
    }
    if (S7::S7_inherits(x, ShiftMorphed) && !is.null(store)) {
        return(shift_inspect__morph_plan(store, ids$morph_id))
    }
    if (S7::S7_inherits(x, ShiftOutputs)) {
        return(shift_outputs(x))
    }

    data.table::data.table()
}

# Derive terminal facts from persisted cases and output manifests. A method's
# multi-year files never inflate the number of completed scientific cases.
shift_inspect__completion <- function(cases, outputs, diagnostics) {
    warnings <- diagnostics[diagnostics$severity == "warning"]
    roles <- if (nrow(outputs) && "provenance_json" %in% names(outputs)) {
        tryCatch(
            jsonlite::fromJSON(outputs$provenance_json[[
                1L
            ]])$weather_field_roles,
            error = function(error) NULL
        )
    } else {
        NULL
    }
    list(
        result_summary = sprintf(
            "%d/%d cases completed \u00b7 %d EPW files \u00b7 %d warnings",
            sum(cases$status == "completed"),
            nrow(cases),
            nrow(outputs),
            nrow(warnings)
        ),
        warning_messages = unique(warnings$message),
        field_summary = if (is.null(roles)) {
            NULL
        } else {
            sprintf(
                "%d transformed \u00b7 %d derived \u00b7 %d physically closed \u00b7 %d inherited",
                length(roles$transformed_fields),
                length(roles$derived_fields),
                length(roles$physically_closed_fields),
                length(roles$inherited_fields)
            )
        }
    )
}

# Read only the requested identity rows; lifecycle errors must not become empty
# successful results. Callers decide explicitly whether a read failure is optional.
shift_inspect__rows <- function(store, table, key, ids) {
    conn <- morpher__private_store(store)$conn
    store$query(sprintf(
        "SELECT * FROM %s WHERE %s IN (%s)",
        ddb_ident(conn, table),
        ddb_ident(conn, key),
        shift_stage__query_ids(ids)
    ))
}
