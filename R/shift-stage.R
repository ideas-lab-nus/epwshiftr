#' @include query.R store.R epw-morpher.R utils.R
NULL

# shift diagnostics -----------------------------------------------------------

SHIFT_DIAGNOSTIC_COLUMNS <- c(
    "stage",
    "severity",
    "code",
    "message",
    "query_id",
    "session_id",
    "plan_id",
    "summary_id",
    "baseline_id",
    "morph_id",
    "case_id",
    "variable_id",
    "epw_field",
    "period",
    "month",
    "action"
)

shift_diagnostic_columns <- function() {
    SHIFT_DIAGNOSTIC_COLUMNS
}

shift_diagnostics_empty <- function() {
    out <- stats::setNames(rep(list(character()), length(SHIFT_DIAGNOSTIC_COLUMNS)), SHIFT_DIAGNOSTIC_COLUMNS)
    data.table::as.data.table(out)
}

shift_diagnostics_normalize <- function(x = NULL) {
    if (is.null(x)) {
        return(shift_diagnostics_empty())
    }
    out <- data.table::as.data.table(x)
    for (col in SHIFT_DIAGNOSTIC_COLUMNS) {
        if (!col %in% names(out)) {
            out[[col]] <- rep(NA_character_, nrow(out))
        }
    }
    out <- out[, SHIFT_DIAGNOSTIC_COLUMNS, with = FALSE]
    for (col in SHIFT_DIAGNOSTIC_COLUMNS) {
        out[[col]] <- as.character(out[[col]])
    }
    out[]
}

shift_diagnostic <- function(stage, severity, code, message, ..., action = NA_character_) {
    dots <- list(...)
    row <- stats::setNames(as.list(rep(NA_character_, length(SHIFT_DIAGNOSTIC_COLUMNS))), SHIFT_DIAGNOSTIC_COLUMNS)
    row$stage <- stage
    row$severity <- severity
    row$code <- code
    row$message <- message
    row$action <- action
    for (name in intersect(names(dots), SHIFT_DIAGNOSTIC_COLUMNS)) {
        row[[name]] <- as.character(dots[[name]])
    }
    shift_diagnostics_normalize(data.table::as.data.table(row))
}

shift_bind_diagnostics <- function(...) {
    parts <- list(...)
    parts <- Filter(function(x) !is.null(x) && nrow(x), parts)
    if (!length(parts)) {
        return(shift_diagnostics_empty())
    }
    shift_diagnostics_normalize(data.table::rbindlist(parts, fill = TRUE))
}

shift_stage_has_errors <- function(x) {
    diagnostics <- shift_diagnostics_normalize(x)
    any(diagnostics$severity %in% "error")
}

shift_abort_diagnostics <- function(diagnostics) {
    diagnostics <- shift_diagnostics_normalize(diagnostics)
    errors <- diagnostics[diagnostics[["severity"]] %in% "error"]
    if (!nrow(errors)) {
        return(invisible(diagnostics))
    }
    cli::cli_abort(c(
        "Blocking shift workflow diagnostic(s) were found.",
        "x" = errors$message
    ))
}

# shift S7 stage classes ------------------------------------------------------

ShiftDiagnostics <- S7::new_S3_class("data.frame")

shift_prop_string <- function(null.ok = FALSE, min.chars = NULL, default = NULL) {
    checkmate_property(
        S7::class_any,
        checkmate::check_string,
        null.ok = null.ok,
        min.chars = min.chars,
        default = default
    )
}

shift_prop_number <- function(lower = -Inf, upper = Inf) {
    checkmate_property(
        S7::class_any,
        checkmate::check_number,
        lower = lower,
        upper = upper,
        finite = TRUE
    )
}

ShiftStage <- S7::new_class(
    "ShiftStage",
    abstract = TRUE,
    properties = list(
        stage = shift_prop_string(min.chars = 1L),
        store_path = shift_prop_string(null.ok = TRUE, min.chars = 1L, default = NULL),
        ids = S7::new_property(S7::class_list, default = list()),
        meta = S7::new_property(S7::class_list, default = list()),
        diagnostics = S7::new_property(ShiftDiagnostics, default = shift_diagnostics_empty())
    )
)

ShiftRequest <- S7::new_class("ShiftRequest", parent = ShiftStage)
ShiftFiles <- S7::new_class("ShiftFiles", parent = ShiftStage)
ShiftDownload <- S7::new_class("ShiftDownload", parent = ShiftStage)
ShiftClimate <- S7::new_class("ShiftClimate", parent = ShiftStage)
ShiftMorphed <- S7::new_class("ShiftMorphed", parent = ShiftStage)
ShiftOutputs <- S7::new_class("ShiftOutputs", parent = ShiftStage)

ShiftReferenceSpec <- S7::new_class(
    "ShiftReferenceSpec",
    properties = list(
        mode = shift_prop_string(min.chars = 1L),
        plan_id = S7::new_property(S7::class_any, default = NULL),
        periods = S7::new_property(S7::class_any, default = NULL),
        experiment = shift_prop_string(null.ok = TRUE, min.chars = 1L, default = NULL),
        activity = shift_prop_string(null.ok = TRUE, min.chars = 1L, default = NULL),
        match = S7::new_property(S7::class_character, default = character()),
        filters = S7::new_property(S7::class_list, default = list()),
        options = S7::new_property(S7::class_list, default = list()),
        collect = S7::new_property(S7::class_list, default = list()),
        extract = S7::new_property(S7::class_list, default = list())
    )
)

ShiftSite <- S7::new_class(
    "ShiftSite",
    parent = ShiftStage,
    properties = list(
        id = shift_prop_string(min.chars = 1L),
        lon = shift_prop_number(lower = -180, upper = 360),
        lat = shift_prop_number(lower = -90, upper = 90),
        label = shift_prop_string(null.ok = TRUE, min.chars = 1L, default = NULL),
        epw = S7::new_property(S7::class_any, default = NULL),
        metadata = S7::new_property(S7::class_list, default = list())
    )
)

shift_stage_new <- function(class, stage, store_path = NULL, ids = list(), meta = list(), diagnostics = NULL, ...) {
    class(
        stage = stage,
        store_path = store_path,
        ids = ids,
        meta = meta,
        diagnostics = shift_diagnostics_normalize(diagnostics),
        ...
    )
}

shift_assert_stage <- function(x) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        cli::cli_abort("`x` must be a shift stage object.")
    }
    invisible(x)
}

shift_coalesce <- function(x, y) {
    if (is.null(x)) y else x
}

shift_sql_string <- function(x) {
    paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

shift_query_maybe <- function(store, sql) {
    tryCatch(store$query(sql), error = function(e) data.table::data.table())
}

shift_stage_query_ids <- function(ids) {
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (!length(ids)) {
        return("NULL")
    }
    paste(vapply(ids, shift_sql_string, character(1L)), collapse = ", ")
}

shift_query_run <- function(store, query_id) {
    shift_query_maybe(store, sprintf(
        "SELECT * FROM query_run WHERE query_id IN (%s)",
        shift_stage_query_ids(query_id)
    ))
}

shift_file_catalog <- function(store, query_id) {
    shift_query_maybe(store, sprintf(
        "SELECT * FROM file_catalog WHERE query_id IN (%s)",
        shift_stage_query_ids(query_id)
    ))
}

shift_extraction_plan <- function(store, plan_id) {
    shift_query_maybe(store, sprintf(
        "SELECT * FROM extraction_plan WHERE plan_id IN (%s)",
        shift_stage_query_ids(plan_id)
    ))
}

shift_extraction_result_rows <- function(store, plan_id) {
    shift_query_maybe(store, sprintf(
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
        shift_stage_query_ids(plan_id)
    ))
}

shift_morph_plan <- function(store, morph_id) {
    shift_query_maybe(store, sprintf(
        "SELECT * FROM epw_morph_plan WHERE morph_id IN (%s)",
        shift_stage_query_ids(morph_id)
    ))
}

shift_morph_result_rows <- function(store, morph_id, case_id = NULL) {
    sql <- sprintf(
        "SELECT * FROM epw_morph_result WHERE morph_id IN (%s)",
        shift_stage_query_ids(morph_id)
    )
    if (!is.null(case_id)) {
        sql <- paste(sql, sprintf("AND case_id IN (%s)", shift_stage_query_ids(case_id)))
    }
    shift_query_maybe(store, paste(sql, "ORDER BY case_id, output_path"))
}

shift_epw_output_rows <- function(store, morph_id) {
    shift_query_maybe(store, sprintf(
        "SELECT * FROM epw_output WHERE morph_id IN (%s)",
        shift_stage_query_ids(morph_id)
    ))
}

shift_epw_output_rows_for_cases <- function(store, morph_id, case_id = NULL) {
    rows <- shift_epw_output_rows(store, morph_id)
    if (!is.null(case_id) && nrow(rows)) {
        target_case_id <- case_id
        rows <- rows[rows[["case_id"]] %in% target_case_id]
    }
    rows[order(rows$case_id, rows$path)]
}

shift_artifact_rows <- function(store, artifact_id) {
    artifact_id <- unique(as.character(artifact_id))
    artifact_id <- artifact_id[!is.na(artifact_id) & nzchar(artifact_id)]
    if (!length(artifact_id)) {
        return(data.table::data.table())
    }
    shift_query_maybe(store, sprintf(
        "SELECT * FROM artifact WHERE artifact_id IN (%s)",
        shift_stage_query_ids(artifact_id)
    ))
}

shift_relative_paths_exist <- function(store, paths) {
    paths <- as.character(paths)
    paths <- paths[!is.na(paths) & nzchar(paths)]
    length(paths) > 0L && all(file.exists(file.path(store$path, paths)))
}

shift_data_limit <- function(n) {
    if (is.null(n) || identical(n, Inf)) {
        return(Inf)
    }
    checkmate::assert_count(n, positive = FALSE)
    if (is.na(n)) {
        cli::cli_abort("`n` cannot be missing.")
    }
    as.integer(n)
}

shift_read_parquet <- function(store, path, n = Inf, columns = NULL) {
    conn <- morpher__private_store(store)$conn
    select <- if (is.null(columns)) {
        "*"
    } else {
        paste(vapply(columns, function(column) ddb_ident(conn, column), character(1L)), collapse = ", ")
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

shift_select_data_columns <- function(dt, columns, stage) {
    if (is.null(columns)) {
        return(dt)
    }
    unknown <- setdiff(columns, names(dt))
    if (length(unknown)) {
        cli::cli_abort("Unknown {stage} data column(s): {.val {unknown}}.")
    }
    dt[, columns, with = FALSE]
}

shift_add_constant_columns <- function(dt, values) {
    for (name in names(values)) {
        dt[, (name) := values[[name]]]
    }
    data.table::setcolorder(dt, c(names(values), setdiff(names(dt), names(values))))
    dt
}

shift_read_morph_data <- function(store, results, n, columns) {
    pieces <- vector("list", nrow(results))
    remaining <- n
    for (i in seq_len(nrow(results))) {
        if (!is.infinite(remaining) && remaining <= 0L) {
            break
        }
        path <- store_abs_path(results$output_path[[i]], root = store$path)
        if (!file.exists(path)) {
            cli::cli_abort(c(
                "Morphed Parquet data file is missing.",
                "x" = "{.path {path}}",
                "i" = "Run {.fn shift_morph} again or inspect {.fn shift_artifacts}."
            ))
        }
        limit <- if (is.infinite(remaining)) Inf else remaining
        dt <- shift_read_parquet(store, path, n = limit)
        dt <- shift_add_constant_columns(dt, list(
            result_id = results$result_id[[i]],
            morph_id = results$morph_id[[i]],
            case_id = results$case_id[[i]],
            output_path = results$output_path[[i]]
        ))
        pieces[[i]] <- shift_select_data_columns(dt, columns, "morphed")
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

shift_read_epw_output_data <- function(store, outputs, n, columns) {
    pieces <- vector("list", nrow(outputs))
    remaining <- n
    for (i in seq_len(nrow(outputs))) {
        if (!is.infinite(remaining) && remaining <= 0L) {
            break
        }
        path <- store_abs_path(outputs$path[[i]], root = store$path)
        if (!file.exists(path)) {
            cli::cli_abort(c(
                "EPW output file is missing.",
                "x" = "{.path {path}}",
                "i" = "Run {.fn shift_epw} again or inspect {.fn shift_outputs}."
            ))
        }
        dt <- data.table::as.data.table(eplusr::read_epw(path)$data())
        if (!is.infinite(remaining)) {
            dt <- utils::head(dt, remaining)
        }
        dt <- shift_add_constant_columns(dt, list(
            output_id = outputs$output_id[[i]],
            morph_id = outputs$morph_id[[i]],
            case_id = outputs$case_id[[i]],
            source_id = outputs$source_id[[i]],
            experiment_id = outputs$experiment_id[[i]],
            variant_label = outputs$variant_label[[i]],
            period = outputs$period[[i]],
            path = outputs$path[[i]]
        ))
        pieces[[i]] <- shift_select_data_columns(dt, columns, "EPW output")
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

shift_display_path <- function(path) {
    if (is.null(path) || !nzchar(path)) {
        return(path)
    }
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    temp <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    if (startsWith(path, temp)) {
        return(sub(temp, "<tempdir>", path, fixed = TRUE))
    }
    path
}

shift_periods_time <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    years <- as.integer(periods$year)
    years <- years[!is.na(years)]
    if (!length(years)) {
        cli::cli_abort("`periods` must contain at least one non-missing `year`.")
    }
    c(
        sprintf("%d-01-01T00:00:00Z", min(years)),
        sprintf("%d-12-31T23:59:59Z", max(years))
    )
}

shift_time_window <- function(time) {
    if (is.null(time)) {
        return(NULL)
    }
    if (is.numeric(time) && !inherits(time, c("Date", "POSIXt"))) {
        checkmate::assert_integerish(time, any.missing = FALSE, min.len = 1L, max.len = 2L)
        years <- as.integer(time)
        years <- range(years)
        return(c(
            sprintf("%04d-01-01T00:00:00Z", years[[1L]]),
            sprintf("%04d-12-31T23:59:59Z", years[[2L]])
        ))
    }
    time
}

shift_display_values <- function(x, max = 7L) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) {
        return(NULL)
    }
    if (length(x) > max) {
        return(sprintf("%s, ... (%d total)", paste(utils::head(x, max), collapse = ", "), length(x)))
    }
    paste(x, collapse = ", ")
}

shift_request_filter <- function(x, name) {
    value <- x@meta$filters[[name]]
    if (is.null(value)) {
        return(NULL)
    }
    value
}

shift_stage_print_line <- function(label, value) {
    value <- shift_display_values(value)
    if (!is.null(value)) {
        cat(sprintf("  %s: %s\n", label, value))
    }
}

shift_stage_print_details <- function(x) {
    if (S7::S7_inherits(x, ShiftRequest)) {
        if (!identical(x@meta$provider, "esgf")) {
            shift_stage_print_line("provider", x@meta$provider)
        }
        shift_stage_print_line("project", x@meta$project)
        shift_stage_print_line("source", shift_coalesce(x@meta$source, shift_request_filter(x, "source_id")))
        shift_stage_print_line("experiment", shift_coalesce(x@meta$experiment, shift_request_filter(x, "experiment_id")))
        shift_stage_print_line("variant", shift_coalesce(x@meta$variant, shift_request_filter(x, "variant_label")))
        shift_stage_print_line("frequency", shift_coalesce(x@meta$frequency, shift_request_filter(x, "frequency")))
        shift_stage_print_line("variables", shift_coalesce(x@meta$variables, shift_request_filter(x, "variable_id")))
        if (!is.null(x@meta$time)) {
            cat(sprintf("  time:   %s\n", paste(as.character(x@meta$time), collapse = " -> ")))
        }
        return(invisible())
    }

    if (S7::S7_inherits(x, ShiftFiles)) {
        cat(sprintf("  files:  %s\n", shift_coalesce(x@meta$file_count, NA_integer_)))
        shift_stage_print_line("variables", x@meta$variables)
        return(invisible())
    }

    if (S7::S7_inherits(x, ShiftDownload)) {
        tasks <- tryCatch(data.table::as.data.table(x), error = function(e) data.table::data.table())
        if (nrow(tasks) && "status" %in% names(tasks)) {
            counts <- table(tasks$status)
            complete <- sum(tasks$status %in% c("done", "skipped"))
            percent <- round(100 * complete / nrow(tasks))
            cat(sprintf(
                "  tasks:  %d/%d complete (%d%%); %s\n",
                complete, nrow(tasks), percent,
                paste(sprintf("%s=%s", names(counts), counts), collapse = ", ")
            ))
        }
        return(invisible())
    }

    if (S7::S7_inherits(x, ShiftClimate)) {
        coverage <- tryCatch(shift_coverage(x), error = function(e) data.table::data.table())
        if (nrow(coverage) && "complete" %in% names(coverage)) {
            cat(sprintf("  coverage: %d/%d complete\n", sum(coverage$complete %in% TRUE), nrow(coverage)))
        }
        return(invisible())
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        plan <- data.table::as.data.table(shift_coalesce(x@meta$plan, data.table::data.table()))
        if (nrow(plan)) {
            shift_stage_print_line("morph", unique(plan$status))
            cat(sprintf("  cases:  %d\n", nrow(plan)))
        }
        return(invisible())
    }

    if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- data.table::as.data.table(shift_coalesce(x@meta$outputs, data.table::data.table()))
        if (nrow(outputs)) {
            cat(sprintf("  outputs: %d\n", nrow(outputs)))
        }
        return(invisible())
    }

    invisible()
}

shift_stage_root <- function(x) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    meta <- x@meta
    for (name in c("request", "files", "download", "climate", "morphed")) {
        value <- meta[[name]]
        if (S7::S7_inherits(value, ShiftStage)) {
            root <- shift_stage_root(value)
            if (!is.null(root)) {
                return(root)
            }
        }
    }
    if (S7::S7_inherits(x, ShiftRequest)) {
        return(x)
    }
    NULL
}

shift_stage_value <- function(x, name) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    if (name %in% names(x@meta)) {
        return(x@meta[[name]])
    }
    root <- shift_stage_root(x)
    if (!is.null(root) && name %in% names(root@meta)) {
        return(root@meta[[name]])
    }
    NULL
}

shift_stage_variables <- function(x) {
    for (name in c("variables", "variable_id")) {
        value <- shift_stage_value(x, name)
        if (!is.null(value)) {
            return(as.character(value))
        }
    }
    NULL
}

shift_stage_nested <- function(x, classes = list()) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    if (!length(classes) || any(vapply(classes, function(class) S7::S7_inherits(x, class), logical(1L)))) {
        return(x)
    }
    for (name in c("files", "download", "climate", "morphed")) {
        value <- x@meta[[name]]
        if (S7::S7_inherits(value, ShiftStage)) {
            hit <- shift_stage_nested(value, classes)
            if (!is.null(hit)) {
                return(hit)
            }
        }
    }
    NULL
}

shift_stage_query_result <- function(store, query_id, result_type = NULL) {
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_choice(result_type, c("File", "Aggregation"), null.ok = TRUE)

    runs <- shift_query_run(store, query_id)
    if (!nrow(runs)) {
        cli::cli_abort("No stored File query result was found for this shift stage.")
    }

    run <- runs[1L]
    if (!is.null(result_type) && !identical(run$result_type[[1L]], result_type)) {
        cli::cli_abort(
            "The stored query result has type {.val {run$result_type[[1L]]}}, not {.val {result_type}}."
        )
    }

    query_file <- file.path(store$path, run$query_file[[1L]])
    if (!file.exists(query_file)) {
        cli::cli_abort("The stored query result file no longer exists: {.path {query_file}}.")
    }

    schema <- switch(
        run$result_type[[1L]],
        File = SCHEMA_RESULT_FILE,
        Aggregation = SCHEMA_RESULT_AGGREGATION,
        cli::cli_abort("Unsupported stored query result type: {.val {run$result_type[[1L]]}}.")
    )
    loaded <- query__load(query_file, schema)
    generator <- switch(
        run$result_type[[1L]],
        File = EsgResultFile,
        Aggregation = EsgResultAggregation,
        cli::cli_abort("Unsupported stored query result type: {.val {run$result_type[[1L]]}}.")
    )
    query_result__new(
        generator,
        index_node = loaded$index_node,
        params = loaded$parameter,
        result = loaded$response,
        context = loaded$context
    )
}

shift_is_epw_object <- function(x) {
    checkmate::test_r6(x, "Epw")
}

shift_is_epw_path <- function(x) {
    is.character(x) && length(x) == 1L && identical(tolower(tools::file_ext(x)), "epw")
}

shift_location_value <- function(location, names) {
    if (is.null(location)) {
        return(NULL)
    }
    if (is.data.frame(location)) {
        if (!nrow(location)) {
            return(NULL)
        }
        for (name in names) {
            if (name %in% names(location)) {
                value <- location[[name]][[1L]]
                if (!is.na(value) && nzchar(as.character(value))) {
                    return(value)
                }
            }
        }
        return(NULL)
    }
    for (name in names) {
        value <- location[[name]]
        if (!is.null(value) && length(value) &&
            !is.na(value[[1L]]) && nzchar(as.character(value[[1L]]))) {
            return(value[[1L]])
        }
    }
    NULL
}

shift_epw_location <- function(epw) {
    if (is.null(epw)) {
        return(NULL)
    }
    epw_obj <- if (shift_is_epw_path(epw)) {
        if (!file.exists(epw)) {
            cli::cli_abort("EPW file does not exist: {.path {epw}}.")
        }
        eplusr::read_epw(epw)
    } else if (shift_is_epw_object(epw)) {
        epw
    } else {
        cli::cli_abort("`epw` must be an EPW file path or {.cls eplusr::Epw} object.")
    }
    epw_obj$location()
}

shift_site_default_id <- function(epw, location) {
    if (shift_is_epw_path(epw)) {
        return(tools::file_path_sans_ext(basename(epw)))
    }
    id <- shift_location_value(location, c("wmo_number", "city", "location"))
    if (is.null(id)) {
        return("site")
    }
    as.character(id)
}

shift_resolve_epw <- function(x) {
    if (S7::S7_inherits(x, ShiftSite)) {
        x <- x@epw
    }
    if (is.null(x)) {
        cli::cli_abort("A baseline EPW file or {.cls eplusr::Epw} object is required.")
    }
    if (is.character(x) && length(x) == 1L) {
        return(eplusr::read_epw(x))
    }
    x
}

# constructors ---------------------------------------------------------------

#' Store-native shift workflow API
#'
#' @description
#' `shift_*()` functions provide a stage-oriented workflow facade over
#' [EsgQuery], [EsgStore], [Downloader], and [EpwMorpher]. Each step returns a
#' small S7 stage object that can be printed, inspected, saved, and passed to the
#' next step without manually passing manifest IDs.
#'
#' @param provider Climate data provider. The first implementation supports
#'   `"esgf"`.
#' @param project Optional provider project, for example `"CMIP6"`.
#' @param source,experiment,variant,frequency Provider-neutral request aliases.
#'   In `shift_reference_historical()`, `experiment` is the historical
#'   reference experiment filter.
#' @param time Optional request or extraction time filter. Numeric years such as
#'   `2060L` are expanded to the full UTC year; otherwise supply one or two
#'   date-time values accepted by the provider/store.
#' @param variables Provider-neutral request alias in [shift_request()], optional
#'   extraction variables in [shift_extract()], or optional variables to read in
#'   `shift_data()`.
#' @param filters Provider-specific query filters in [shift_request()], or
#'   extraction filters in [shift_extract()].
#' @param options Provider-specific request options. For ESGF, `index_node` and
#'   `time_filter_method` are recognized.
#' @param id Optional site identifier. If `id` is an EPW file path or
#'   [eplusr::Epw] object and `epw` is `NULL`, it is treated as `epw`.
#' @param lon,lat Optional site longitude and latitude. Missing values are read
#'   from `epw$location()` when `epw` is supplied.
#' @param label Optional human-readable label.
#' @param epw Optional baseline EPW path or [eplusr::Epw] object.
#' @param metadata Optional site metadata.
#' @param ... Additional provider-specific filters or workflow options.
#'
#' @return A shift stage object.
#'
#' @name shift_api
NULL

#' @rdname shift_api
#' @export
shift_request <- function(provider = "esgf", project = NULL, source = NULL, experiment = NULL,
                          variant = NULL, variables = NULL, frequency = NULL, time = NULL,
                          filters = list(), options = list(), ...) {
    checkmate::assert_string(provider, min.chars = 1L)
    checkmate::assert_string(project, null.ok = TRUE)
    checkmate::assert_character(source, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(experiment, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(variant, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(frequency, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    if (!is.null(time)) {
        checkmate::assert_atomic_vector(time, any.missing = FALSE, min.len = 1L, max.len = 2L)
        time <- shift_time_window(time)
    }
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")

    dots <- list(...)
    if (length(dots)) {
        nms <- names(dots)
        if (is.null(nms) || any(!nzchar(nms))) {
            cli::cli_abort("Additional request filters supplied in `...` must be named.")
        }
        filters <- utils::modifyList(filters, dots)
    }

    meta <- list(
        provider = tolower(provider),
        project = project,
        source = source,
        experiment = experiment,
        variant = variant,
        variables = variables,
        frequency = frequency,
        time = time,
        filters = filters,
        options = options
    )

    shift_stage_new(ShiftRequest, "request", meta = meta)
}

#' @rdname shift_api
#' @export
shift_site <- function(id = NULL, lon = NULL, lat = NULL, label = NULL, epw = NULL, metadata = list()) {
    if (is.null(epw) && (shift_is_epw_path(id) || shift_is_epw_object(id))) {
        epw <- id
        id <- NULL
    }

    needs_location <- is.null(id) || is.null(lon) || is.null(lat)
    location <- if (needs_location) shift_epw_location(epw) else NULL
    if (is.null(lon)) {
        lon <- shift_location_value(location, c("longitude", "lon"))
    }
    if (is.null(lat)) {
        lat <- shift_location_value(location, c("latitude", "lat"))
    }
    if (is.null(id)) {
        id <- shift_site_default_id(epw, location)
    }
    if (is.null(label)) {
        label <- shift_location_value(location, c("city", "location"))
    }

    checkmate::assert_string(id, min.chars = 1L)
    checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
    checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
    checkmate::assert_string(label, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(metadata, names = "unique")

    ShiftSite(
        stage = "site",
        store_path = NULL,
        ids = list(),
        meta = list(),
        diagnostics = shift_diagnostics_empty(),
        id = id,
        lon = lon,
        lat = lat,
        label = label,
        epw = epw,
        metadata = metadata
    )
}

#' @rdname shift_api
#' @param plan_id Store extraction plan IDs for manually selected reference
#'   climate data.
#' @export
shift_reference_plan <- function(plan_id, periods) {
    checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
    periods <- shift_reference_periods(periods)

    ShiftReferenceSpec(
        mode = "plan",
        plan_id = plan_id,
        periods = periods,
        experiment = NULL,
        activity = NULL,
        match = character(),
        filters = list(),
        options = list(),
        collect = list(),
        extract = list()
    )
}

#' @rdname shift_api
#' @param activity Historical reference activity filter used by
#'   `shift_reference_historical()`.
#' @param match File metadata fields copied from the future climate stage when
#'   resolving an automatic historical reference.
#' @param collect,extract Named option lists passed to the automatic
#'   historical collect and extract steps. `collect` may contain `fields`,
#'   `all`, `limit`, and `label`; `extract` may contain `variables`, `time`,
#'   `filters`, `nearest`, and `fallback`.
#' @export
shift_reference_historical <- function(periods, experiment = "historical", activity = "CMIP",
                                       match = c("source_id", "variant_label", "frequency", "table_id"),
                                       filters = list(), options = list(),
                                       collect = list(), extract = list(fallback = "auto")) {
    periods <- shift_reference_periods(periods)
    checkmate::assert_string(experiment, min.chars = 1L)
    checkmate::assert_string(activity, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_character(match, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")
    checkmate::assert_list(collect, names = "unique")
    checkmate::assert_subset(names(collect), c("fields", "all", "limit", "label"))
    checkmate::assert_list(extract, names = "unique")
    checkmate::assert_subset(names(extract), c("variables", "time", "filters", "nearest", "fallback"))

    ShiftReferenceSpec(
        mode = "historical",
        plan_id = NULL,
        periods = periods,
        experiment = experiment,
        activity = activity,
        match = match,
        filters = filters,
        options = options,
        collect = collect,
        extract = extract
    )
}

shift_reference_periods <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    data.table::as.data.table(periods)
}

# generics -------------------------------------------------------------------

#' @rdname shift_api
#' @param x A shift stage object.
#' @param store An [EsgStore], store path, or `NULL`.
#' @param fields File fields collected from Dataset records. The default
#'   requests all fields and lets the result/store layers preserve and validate
#'   provider response metadata.
#' @param all,limit Collection controls passed to [EsgQuery] / [EsgResultDataset].
#' @param label Optional label recorded with collected File records.
#' @export
shift_collect <- S7::new_generic(
    "shift_collect",
    "x",
    function(x, store = NULL, fields = "*", all = TRUE, limit = FALSE, label = NULL, ...) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param downloader Optional [Downloader] instance.
#' @param run Whether to run queued downloads immediately. Downloading full
#'   NetCDF files is optional for the normal workflow because [shift_extract()]
#'   can use OPeNDAP first and only download as a fallback when requested.
#' @param background Whether to run downloads in a background job.
#' @param resume Whether to reuse complete existing downloads, extraction
#'   outputs, morphing results, or EPW outputs.
#' @param overwrite Whether to overwrite existing downloads, extraction outputs,
#'   morphing results, or EPW outputs.
#' @param session_label Optional download session label.
#' @export
shift_download <- S7::new_generic(
    "shift_download",
    "x",
    function(x, downloader = NULL, run = TRUE, background = FALSE,
             resume = TRUE, overwrite = FALSE, session_label = NULL, ...) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param site A `shift_site()` object.
#' @param periods A period table, usually from [epw_morph_periods()].
#' @param nearest Number of nearest grid points to extract.
#' @param fallback Extraction fallback policy.
#' @export
shift_extract <- S7::new_generic(
    "shift_extract",
    "x",
    function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
             filters = list(), nearest = 1L, fallback = c("auto", "error"),
             overwrite = FALSE, resume = TRUE) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param baseline Optional baseline EPW path, [eplusr::Epw] object, or
#'   `shift_site()` object containing `epw`.
#' @param recipe Morphing recipe, usually from [epw_morph_recipe()].
#' @param reference Optional reference `ShiftClimate` stage for change-factor
#'   morphing.
#' @param reference_plan_id,reference_periods Optional store plan IDs and period
#'   table for reference climate data.
#' @param by Grouping columns used to create morphing cases.
#' @export
shift_morph <- S7::new_generic(
    "shift_morph",
    "x",
    function(x, baseline = NULL, recipe = epw_morph_recipe("belcher"),
             reference = NULL, reference_plan_id = NULL, reference_periods = NULL,
             strict = TRUE,
             by = c("source_id", "experiment_id", "variant_label", "period"),
             overwrite = FALSE, resume = TRUE) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param dir Store-relative output directory for generated EPW files. If `NULL`,
#'   [shift_epw()] uses `"outputs/future-epw"`.
#' @param separate Whether to create separate output directories per morphing case.
#' @export
shift_epw <- S7::new_generic(
    "shift_epw",
    "x",
    function(x, dir = NULL, separate = TRUE, overwrite = FALSE, resume = TRUE) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param strict If `TRUE`, abort when diagnostics contain errors.
#' @export
shift_check <- S7::new_generic("shift_check", "x", function(x, strict = FALSE, ...) {
    S7::S7_dispatch()
})

# public inspectors -----------------------------------------------------------

#' @rdname shift_api
#' @export
shift_refresh <- function(x) {
    shift_assert_stage(x)
    if (S7::S7_inherits(x, ShiftRequest) || S7::S7_inherits(x, ShiftSite)) {
        return(x)
    }
    x@diagnostics <- shift_diagnostics_empty()
    x@diagnostics <- shift_check(x, strict = FALSE)
    x
}

#' @rdname shift_api
#' @export
shift_ids <- function(x) {
    shift_assert_stage(x)
    x@ids
}

#' @rdname shift_api
#' @export
shift_datasets <- function(x, all = TRUE, limit = FALSE) {
    shift_assert_stage(x)
    checkmate::assert_flag(all)

    if (S7::S7_inherits(x, ShiftRequest)) {
        return(shift_as_query(x)$collect(type = "Dataset", all = all, limit = limit))
    }

    files <- shift_stage_nested(x, list(ShiftFiles))
    if (!is.null(files) && !is.null(files@meta$datasets)) {
        return(files@meta$datasets)
    }

    request <- shift_stage_root(x)
    if (!is.null(request)) {
        return(shift_datasets(request, all = all, limit = limit))
    }

    cli::cli_abort("No Dataset result is available for this shift stage.")
}

#' @rdname shift_api
#' @export
shift_files <- function(x) {
    shift_assert_stage(x)
    ids <- shift_ids(x)
    if (is.null(ids$query_id) || !length(ids$query_id) || is.na(ids$query_id[[1L]])) {
        cli::cli_abort("No File result is available before {.fn shift_collect}.")
    }

    store <- shift_store(x)
    shift_stage_query_result(store, ids$query_id[[1L]], result_type = "File")
}

#' @rdname shift_api
#' @param n Maximum number of data rows to read. Use `Inf` to read all rows.
#' @param case_id Optional morphing case IDs to read from morphed or EPW output
#'   stages.
#' @param columns Optional data columns to keep.
#' @export
shift_data <- function(x, n = 100L, variables = NULL, case_id = NULL, columns = NULL) {
    shift_assert_stage(x)
    n <- shift_data_limit(n)
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(case_id, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(columns, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (!S7::S7_inherits(x, ShiftClimate) &&
        !S7::S7_inherits(x, ShiftMorphed) &&
        !S7::S7_inherits(x, ShiftOutputs)) {
        cli::cli_abort("{.fn shift_data} reads data from {.cls ShiftClimate}, {.cls ShiftMorphed}, or {.cls ShiftOutputs} stages.")
    }
    if (identical(n, 0L)) {
        return(data.table::data.table())
    }

    ids <- shift_ids(x)
    store <- shift_store(x)

    if (S7::S7_inherits(x, ShiftClimate)) {
        if (!is.null(case_id)) {
            cli::cli_abort("`case_id` is only supported for morphed and EPW output stages.")
        }
        if (is.null(ids$plan_id) || !length(ids$plan_id)) {
            return(data.table::data.table())
        }
        results <- shift_extraction_result_rows(store, ids$plan_id)
        if (!is.null(variables)) {
            results <- results[results[["variable_id"]] %in% variables]
        }
        if (!nrow(results)) {
            return(data.table::data.table())
        }

        pieces <- vector("list", nrow(results))
        remaining <- n
        for (i in seq_len(nrow(results))) {
            if (!is.infinite(remaining) && remaining <= 0L) {
                break
            }
            path <- store_abs_path(results$output_path[[i]], root = store$path)
            if (!file.exists(path)) {
                cli::cli_abort(c(
                    "Extracted Parquet data file is missing.",
                    "x" = "{.path {path}}",
                    "i" = "Run {.fn shift_extract} again or inspect {.fn shift_coverage}."
                ))
            }

            limit <- if (is.infinite(remaining)) Inf else remaining
            dt <- shift_read_parquet(store, path, n = limit, columns = columns)
            pieces[[i]] <- dt
            if (!is.infinite(remaining)) {
                remaining <- remaining - nrow(dt)
            }
        }

        pieces <- Filter(Negate(is.null), pieces)
        if (!length(pieces)) {
            return(data.table::data.table())
        }
        return(data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE))
    }

    if (!is.null(variables)) {
        cli::cli_abort("`variables` is only supported for extracted climate stages.")
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        if (is.null(ids$morph_id) || !length(ids$morph_id)) {
            return(data.table::data.table())
        }
        results <- shift_morph_result_rows(store, ids$morph_id, case_id = case_id)
        if (!nrow(results)) {
            return(data.table::data.table())
        }
        return(shift_read_morph_data(store, results, n = n, columns = columns))
    }

    if (S7::S7_inherits(x, ShiftOutputs)) {
        if (is.null(ids$morph_id) || !length(ids$morph_id)) {
            return(data.table::data.table())
        }
        outputs <- shift_epw_output_rows_for_cases(store, ids$morph_id, case_id = case_id)
        if (!nrow(outputs)) {
            return(data.table::data.table())
        }
        return(shift_read_epw_output_data(store, outputs, n = n, columns = columns))
    }

    data.table::data.table()
}

#' @rdname shift_api
#' @param severity Optional diagnostic severities to keep.
#' @export
shift_diagnostics <- function(x, severity = NULL) {
    shift_assert_stage(x)
    checkmate::assert_character(severity, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    out <- shift_diagnostics_normalize(x@diagnostics)
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
    shift_assert_stage(x)
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
    shift_assert_stage(x)
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
    shift_assert_stage(x)
    if (S7::S7_inherits(x, ShiftClimate)) {
        return(data.table::as.data.table(shift_coalesce(x@meta$coverage, data.table::data.table())))
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
shift_outputs <- function(x) {
    shift_assert_stage(x)
    if (S7::S7_inherits(x, ShiftOutputs)) {
        return(data.table::as.data.table(shift_coalesce(x@meta$outputs, data.table::data.table())))
    }
    ids <- shift_ids(x)
    if (is.null(ids$morph_id)) {
        return(data.table::data.table())
    }
    store <- shift_store(x)
    shift_epw_output_rows(store, ids$morph_id)
}

#' @rdname shift_api
#' @export
shift_artifacts <- function(x) {
    shift_assert_stage(x)
    ids <- shift_ids(x)

    if (S7::S7_inherits(x, ShiftMorphed) && !is.null(ids$morph_id)) {
        store <- shift_store(x)
        results <- shift_morph_result_rows(store, ids$morph_id)
        return(shift_artifact_rows(store, results$artifact_id))
    }

    if (S7::S7_inherits(x, ShiftOutputs) && !is.null(ids$morph_id)) {
        store <- shift_store(x)
        outputs <- shift_epw_output_rows_for_cases(store, ids$morph_id)
        return(shift_artifact_rows(store, outputs$artifact_id))
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
    quoted <- shift_stage_query_ids(values)
    shift_query_maybe(store, sprintf(
        paste(
            "SELECT * FROM artifact",
            "WHERE query_id IN (%1$s)",
            "OR file_key IN (%1$s)",
            "OR artifact_id IN (%1$s)"
        ),
        quoted
    ))
}

#' @rdname shift_api
#' @export
shift_status <- function(x) {
    shift_assert_stage(x)

    if (shift_stage_has_errors(x@diagnostics)) {
        return("blocked")
    }
    if (S7::S7_inherits(x, ShiftRequest) || S7::S7_inherits(x, ShiftSite)) {
        return("new")
    }

    ids <- shift_ids(x)
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (is.null(store)) {
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftFiles)) {
        files <- shift_file_catalog(store, ids$query_id)
        return(if (nrow(files)) "collected" else "partial")
    }

    if (S7::S7_inherits(x, ShiftDownload)) {
        files <- shift_file_catalog(store, ids$query_id)
        if (!nrow(files)) {
            return("partial")
        }
        if ("local_path" %in% names(files)) {
            has_path <- !is.na(files$local_path) & nzchar(files$local_path)
            if (any(has_path) && all(file.exists(file.path(store$path, files$local_path[has_path])))) {
                return("downloaded")
            }
        }
        tasks <- if (!is.null(ids$session_id) && !is.na(ids$session_id)) {
            tryCatch(store$download_status(session_id = ids$session_id), error = function(e) data.table::data.table())
        } else {
            data.table::data.table()
        }
        if (nrow(tasks) && any(tasks$status %in% c("error", "cancelled"))) {
            return("failed")
        }
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftClimate)) {
        coverage <- tryCatch(store$coverage(plan_id = ids$plan_id), error = function(e) data.table::data.table())
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
        plans <- shift_morph_plan(store, ids$morph_id)
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
        return("partial")
    }

    if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- shift_outputs(x)
        path_col <- intersect(c("path", "output_path", "relative_path"), names(outputs))
        if (length(path_col) && shift_relative_paths_exist(store, outputs[[path_col[[1L]]]])) {
            return("written")
        }
        if ("status" %in% names(outputs) && any(outputs$status %in% "failed")) {
            return("failed")
        }
        return(if (nrow(outputs)) "written" else "partial")
    }

    "partial"
}

# provider adapter ------------------------------------------------------------

shift_as_query <- function(x) {
    shift_assert_stage(x)
    if (!S7::S7_inherits(x, ShiftRequest)) {
        cli::cli_abort("Only {.cls ShiftRequest} can be converted to an ESGF query.")
    }
    provider <- x@meta$provider
    switch(
        provider,
        esgf = shift_as_esg_query(x),
        cli::cli_abort("Unsupported shift provider: {.val {provider}}.")
    )
}

shift_query_set <- function(query, name, value) {
    if (is.null(value)) {
        return(invisible(query))
    }
    args <- stats::setNames(list(value), name)
    do.call(query$params, args)
    invisible(query)
}

shift_as_esg_query <- function(x) {
    options <- x@meta$options
    query <- if (!is.null(options$index_node)) {
        esg_query(index_node = options$index_node)
    } else {
        esg_query()
    }

    aliases <- list(
        project = x@meta$project,
        source_id = x@meta$source,
        experiment_id = x@meta$experiment,
        variant_label = x@meta$variant,
        variable_id = x@meta$variables,
        frequency = x@meta$frequency
    )
    for (name in names(aliases)) {
        shift_query_set(query, name, aliases[[name]])
    }
    if (!is.null(x@meta$time)) {
        time <- as.character(x@meta$time)
        if (length(time) == 1L) {
            query$datetime_range(time[[1L]], time[[1L]])
        } else {
            query$datetime_range(time[[1L]], time[[2L]])
        }
    }
    filters <- x@meta$filters
    for (name in names(filters)) {
        shift_query_set(query, name, filters[[name]])
    }

    query
}

# workflow methods ------------------------------------------------------------

S7::method(shift_collect, ShiftRequest) <- function(x, store = NULL, fields = "*", all = TRUE, limit = FALSE,
                                                    label = NULL, ...) {
    checkmate::assert_character(fields, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_flag(all)
    checkmate::assert_string(label, null.ok = TRUE)
    if (is.null(store)) {
        cli::cli_abort("`store` is required for {.fn shift_collect}.")
    }
    store <- shift_store(store, create = TRUE)
    datasets <- shift_datasets(x, all = all, limit = limit)
    files <- datasets$collect(type = "File", fields = fields, all = TRUE, limit = NULL, ...)

    if (!is.null(x@meta$time)) {
        time <- as.character(x@meta$time)
        method <- shift_coalesce(x@meta$options$time_filter_method, "drs")
        if (length(time) == 1L) {
            files <- files$filter_time(time[[1L]], time[[1L]], method = method)
        } else {
            files <- files$filter_time(time[[1L]], time[[2L]], method = method)
        }
    }
    query_id <- store$add_files(files, label = label)
    file_dt <- files$to_data_table()
    variables <- if ("variable_id" %in% names(file_dt)) unique(file_dt$variable_id) else character()
    variables <- variables[!is.na(variables) & nzchar(variables)]

    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(
            request = x,
            dataset_count = datasets$count(),
            datasets = datasets,
            file_count = files$count(),
            variables = variables,
            fields = fields
        )
    )
}

S7::method(shift_download, ShiftFiles) <- function(x, downloader = NULL, run = TRUE, background = FALSE,
                                                   resume = TRUE, overwrite = FALSE, session_label = NULL, ...) {
    checkmate::assert_flag(run)
    checkmate::assert_flag(background)
    checkmate::assert_flag(resume)
    checkmate::assert_flag(overwrite)

    store <- shift_store(x)
    if (is.null(downloader) && !isTRUE(run)) {
        downloader <- store$downloader(n_workers = 0L)
    }
    session <- store$download_files(
        query_id = x@ids$query_id,
        downloader = downloader,
        run = run,
        background = background,
        resume = resume,
        overwrite = overwrite,
        session_label = session_label,
        ...
    )
    session_id <- if (is.character(session) && length(session) == 1L) {
        session
    } else if (is.data.frame(session) && "session_id" %in% names(session)) {
        session$session_id[[1L]]
    } else {
        NA_character_
    }
    diagnostics <- shift_check(
        shift_stage_new(
            ShiftDownload,
            "download",
            store_path = x@store_path,
            ids = utils::modifyList(x@ids, list(session_id = session_id)),
            meta = list(files = x, session = session)
        )
    )

    shift_stage_new(
        ShiftDownload,
        "download",
        store_path = x@store_path,
        ids = utils::modifyList(x@ids, list(session_id = session_id)),
        meta = list(files = x, session = session),
        diagnostics = diagnostics
    )
}

shift_extract_stage <- function(x, upstream_name, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                filters = list(), nearest = 1L,
                                fallback = c("auto", "error"), overwrite = FALSE,
                                resume = TRUE) {
    checkmate::assert_choice(upstream_name, c("files", "download"))
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("`site` must be created by {.fn shift_site}.")
    }
    checkmate::assert_data_frame(periods)
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_int(nearest, lower = 1L)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    fallback <- match.arg(fallback)

    store <- shift_store(x)
    ids <- shift_ids(x)
    variables <- shift_coalesce(variables, shift_stage_variables(x))
    time <- shift_time_window(shift_coalesce(time, shift_periods_time(periods)))

    plan <- store$plan_region(
        query_id = ids$query_id,
        lon = site@lon,
        lat = site@lat,
        time = time,
        site_id = site@id,
        variable_id = variables,
        filters = filters,
        nearest = nearest
    )
    plan_id <- unique(plan$plan_id)
    processed <- store$extract(plan_id = plan_id, fallback = fallback, overwrite = overwrite, resume = resume)
    coverage <- store$coverage(plan_id = plan_id)
    diagnostics <- shift_diagnostics_from_coverage(coverage)
    upstream <- stats::setNames(list(x), upstream_name)

    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = x@store_path,
        ids = utils::modifyList(ids, list(plan_id = plan_id)),
        meta = c(
            upstream,
            list(
                site = site,
                periods = data.table::as.data.table(periods),
                variables = variables,
                plan = plan,
                processed = processed,
                coverage = coverage
            )
        ),
        diagnostics = diagnostics
    )
}

S7::method(shift_extract, ShiftFiles) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                  filters = list(), nearest = 1L,
                                                  fallback = c("auto", "error"), overwrite = FALSE,
                                                  resume = TRUE) {
    shift_extract_stage(
        x,
        upstream_name = "files",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        nearest = nearest,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume
    )
}

S7::method(shift_extract, ShiftDownload) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                     filters = list(), nearest = 1L,
                                                     fallback = c("auto", "error"), overwrite = FALSE,
                                                     resume = TRUE) {
    shift_extract_stage(
        x,
        upstream_name = "download",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        nearest = nearest,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume
    )
}

shift_reference_has_legacy_args <- function(reference_plan_id = NULL, reference_periods = NULL) {
    !is.null(reference_plan_id) || !is.null(reference_periods)
}

shift_reference_resolve <- function(x, recipe, site, reference = NULL,
                                    reference_plan_id = NULL, reference_periods = NULL,
                                    overwrite = FALSE, resume = TRUE) {
    if (!is.null(reference) && shift_reference_has_legacy_args(reference_plan_id, reference_periods)) {
        cli::cli_abort("Use either `reference` or `reference_plan_id`/`reference_periods`, not both.")
    }

    if (!is.null(reference_plan_id) && is.null(reference_periods)) {
        cli::cli_abort("`reference_periods` must be supplied when `reference_plan_id` is supplied.")
    }
    if (is.null(reference_plan_id) && !is.null(reference_periods)) {
        cli::cli_abort("`reference_plan_id` must be supplied when `reference_periods` is supplied.")
    }

    if (is.null(reference)) {
        periods <- if (is.null(reference_periods)) NULL else shift_reference_periods(reference_periods)
        return(list(
            reference = NULL,
            spec = NULL,
            plan_id = reference_plan_id,
            periods = periods
        ))
    }

    if (S7::S7_inherits(reference, ShiftClimate)) {
        reference_ids <- shift_ids(reference)
        return(list(
            reference = reference,
            spec = NULL,
            plan_id = reference_ids$plan_id,
            periods = shift_reference_periods(reference@meta$periods)
        ))
    }

    if (!S7::S7_inherits(reference, ShiftReferenceSpec)) {
        cli::cli_abort("`reference` must be a {.cls ShiftClimate} stage or a {.cls ShiftReferenceSpec}.")
    }

    if (identical(reference@mode, "plan")) {
        return(list(
            reference = reference,
            spec = reference,
            plan_id = reference@plan_id,
            periods = shift_reference_periods(reference@periods)
        ))
    }

    if (identical(reference@mode, "historical")) {
        climate <- shift_reference_resolve_historical(
            x = x,
            recipe = recipe,
            site = site,
            spec = reference,
            overwrite = overwrite,
            resume = resume
        )
        climate_ids <- shift_ids(climate)
        return(list(
            reference = climate,
            spec = reference,
            plan_id = climate_ids$plan_id,
            periods = shift_reference_periods(climate@meta$periods)
        ))
    }

    cli::cli_abort("Unsupported reference mode: {.val {reference@mode}}.")
}

shift_reference_resolve_historical <- function(x, recipe, site, spec, overwrite = FALSE, resume = TRUE) {
    root <- shift_stage_root(x)
    if (!is.null(root) && !S7::S7_inherits(root, ShiftRequest)) {
        root <- NULL
    }
    provider <- if (is.null(root)) "esgf" else root@meta$provider
    if (!identical(provider, "esgf")) {
        cli::cli_abort("Automatic historical reference resolution currently supports only ESGF-backed shift requests.")
    }

    store <- shift_store(x)
    ids <- shift_ids(x)
    catalog <- if (!is.null(ids$query_id)) shift_file_catalog(store, ids$query_id) else data.table::data.table()

    periods <- shift_reference_periods(spec@periods)
    variables <- shift_coalesce(spec@extract$variables, epw_morph_variables(recipe))
    variables <- as.character(variables)
    variables <- variables[!is.na(variables) & nzchar(variables)]
    if (!length(variables)) {
        cli::cli_abort("Automatic historical reference resolution could not determine required climate variables.")
    }

    filters <- shift_reference_historical_filters(
        catalog = catalog,
        request = root,
        spec = spec,
        variables = variables
    )
    options <- utils::modifyList(if (is.null(root)) list() else root@meta$options, spec@options)
    project <- shift_coalesce(if (is.null(root)) NULL else root@meta$project, "CMIP6")
    request <- shift_request(
        provider = provider,
        project = project,
        time = shift_periods_time(periods),
        filters = filters,
        options = options
    )

    collect_args <- utils::modifyList(
        list(store = store, fields = "*", all = TRUE, limit = FALSE, label = "historical-reference"),
        spec@collect
    )
    files <- do.call(shift_collect, c(list(request), collect_args))
    if (is.null(files@meta$file_count) || files@meta$file_count < 1L) {
        cli::cli_abort("Automatic historical reference query returned no File records.")
    }

    extract_filters <- filters[intersect(
        names(filters),
        c("experiment_id", "activity_id", "source_id", "variant_label", "frequency", "table_id", "grid_label")
    )]
    extract_defaults <- list(
        site = site,
        periods = periods,
        variables = variables,
        time = shift_periods_time(periods),
        filters = extract_filters,
        nearest = 1L,
        fallback = "auto",
        overwrite = overwrite,
        resume = resume
    )
    extract_overrides <- spec@extract
    if (!is.null(extract_overrides$filters)) {
        extract_overrides$filters <- utils::modifyList(extract_filters, extract_overrides$filters)
    }
    extract_args <- utils::modifyList(extract_defaults, extract_overrides)
    extract_args$site <- site
    extract_args$periods <- periods
    extract_args$overwrite <- overwrite
    extract_args$resume <- resume

    do.call(shift_extract, c(list(files), extract_args))
}

shift_reference_historical_filters <- function(catalog, request, spec, variables) {
    filters <- list(
        experiment_id = spec@experiment,
        variable_id = variables
    )
    if (!is.null(spec@activity)) {
        filters$activity_id <- spec@activity
    }

    missing <- character()
    for (field in spec@match) {
        if (!is.null(spec@filters[[field]])) {
            next
        }
        values <- shift_reference_infer_field(field, catalog, request)
        if (!length(values)) {
            missing <- c(missing, field)
        } else {
            filters[[field]] <- values
        }
    }
    if (length(missing)) {
        cli::cli_abort(c(
            "Automatic historical reference resolution could not infer required match field(s).",
            "x" = "{.field {missing}}",
            "i" = "Supply explicit values through `shift_reference_historical(filters = ...)` or reduce `match`."
        ))
    }

    utils::modifyList(filters, spec@filters)
}

shift_reference_infer_field <- function(field, catalog, request) {
    values <- character()
    if (field %in% names(catalog) && nrow(catalog)) {
        values <- unique(as.character(unlist(catalog[[field]], use.names = FALSE)))
    }
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values)) {
        return(values)
    }

    if (!is.null(request)) {
        alias <- switch(
            field,
            source_id = request@meta$source,
            experiment_id = request@meta$experiment,
            variant_label = request@meta$variant,
            frequency = request@meta$frequency,
            variable_id = request@meta$variables,
            NULL
        )
        values <- unique(as.character(unlist(alias, use.names = FALSE)))
        values <- values[!is.na(values) & nzchar(values)]
        if (length(values)) {
            return(values)
        }

        filter_value <- request@meta$filters[[field]]
        values <- unique(as.character(unlist(filter_value, use.names = FALSE)))
        return(values[!is.na(values) & nzchar(values)])
    }

    character()
}

S7::method(shift_morph, ShiftClimate) <- function(x, baseline = NULL, recipe = epw_morph_recipe("belcher"),
                                                  reference = NULL, reference_plan_id = NULL,
                                                  reference_periods = NULL,
                                                  strict = TRUE,
                                                  by = c("source_id", "experiment_id", "variant_label", "period"),
                                                  overwrite = FALSE, resume = TRUE) {
    checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (!is.null(reference_periods)) {
        checkmate::assert_data_frame(reference_periods)
        checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
    }
    checkmate::assert_flag(strict)
    checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    store <- shift_store(x)
    ids <- shift_ids(x)
    site <- shift_target(x)
    baseline <- shift_coalesce(baseline, site)
    epw <- shift_resolve_epw(baseline)
    periods <- x@meta$periods
    reference_resolved <- shift_reference_resolve(
        x = x,
        recipe = recipe,
        site = site,
        reference = reference,
        reference_plan_id = reference_plan_id,
        reference_periods = reference_periods,
        overwrite = overwrite,
        resume = resume
    )

    morpher <- epw_morpher(store, epw, site_id = site@id, recipe = recipe, label = site@label)
    workflow <- morpher$workflow(
        plan_id = ids$plan_id,
        periods = periods,
        reference_plan_id = reference_resolved$plan_id,
        reference_periods = reference_resolved$periods,
        by = by,
        strict = strict,
        dir = NULL,
        overwrite = overwrite,
        resume = resume
    )
    summary_id <- unique(workflow$climate$summary_id)[[1L]]
    baseline_id <- unique(workflow$baseline$baseline_id)[[1L]]
    morph_id <- unique(workflow$plan$morph_id)[[1L]]
    diagnostics <- shift_diagnostics_normalize(workflow$diagnostics)

    shift_stage_new(
        ShiftMorphed,
        "morphed",
        store_path = x@store_path,
        ids = utils::modifyList(ids, list(
            summary_id = summary_id,
            baseline_id = baseline_id,
            morph_id = morph_id
        )),
        meta = list(
            climate = x,
            baseline = baseline,
            reference = reference_resolved$reference,
            reference_spec = reference_resolved$spec,
            reference_plan_id = reference_resolved$plan_id,
            reference_periods = reference_resolved$periods,
            recipe = recipe,
            workflow = workflow,
            preflight = workflow$preflight,
            climate_summary = workflow$climate,
            baseline_summary = workflow$baseline,
            preview = workflow$preview,
            plan = workflow$plan,
            results = workflow$results
        ),
        diagnostics = diagnostics
    )
}

S7::method(shift_epw, ShiftMorphed) <- function(x, dir = NULL, separate = TRUE, overwrite = FALSE, resume = TRUE) {
    dir <- shift_coalesce(dir, "outputs/future-epw")
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    store <- shift_store(x)
    ids <- shift_ids(x)
    site <- shift_target(x)
    epw <- shift_resolve_epw(shift_coalesce(x@meta$baseline, site))
    morpher <- epw_morpher(store, epw, site_id = site@id, recipe = x@meta$recipe, label = site@label)
    outputs <- morpher$write_epw(
        morph_id = ids$morph_id,
        dir = dir,
        separate = separate,
        overwrite = overwrite,
        resume = resume
    )
    path_col <- intersect(c("path", "output_path", "relative_path"), names(outputs))
    paths <- if (length(path_col)) outputs[[path_col[[1L]]]] else character()

    shift_stage_new(
        ShiftOutputs,
        "outputs",
        store_path = x@store_path,
        ids = ids,
        meta = list(morphed = x, format = "epw", outputs = outputs, paths = paths),
        diagnostics = shift_diagnostics_empty()
    )
}

# check methods ---------------------------------------------------------------

S7::method(shift_check, ShiftStage) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    diagnostics <- shift_diagnostics_normalize(x@diagnostics)
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftRequest) <- function(x, strict = FALSE, ...) {
    diagnostics <- shift_diagnostics_empty()
    if (!identical(x@meta$provider, "esgf")) {
        diagnostics <- shift_diagnostic(
            stage = "request",
            severity = "error",
            code = "unsupported_provider",
            message = sprintf("Unsupported shift provider: %s", x@meta$provider),
            action = "Use provider = 'esgf' or add a provider adapter."
        )
    }
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftFiles) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    diagnostics <- shift_diagnostics_empty()
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (is.null(store)) {
        diagnostics <- shift_diagnostic(
            "files", "error", "missing_store",
            "The store for this collected file stage cannot be opened.",
            query_id = x@ids$query_id,
            action = "Check `shift_store(x)` and the stored path."
        )
    } else {
        files <- shift_file_catalog(store, x@ids$query_id)
        if (!nrow(files)) {
            diagnostics <- shift_diagnostic(
                "files", "error", "missing_file_catalog",
                "No file catalog rows were found for this collected file stage.",
                query_id = x@ids$query_id,
                action = "Run `shift_collect()` again."
            )
        }
    }
    diagnostics <- shift_bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftDownload) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    diagnostics <- shift_diagnostics_empty()
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (!is.null(store)) {
        tasks <- if (!is.null(x@ids$session_id) && !is.na(x@ids$session_id)) {
            tryCatch(store$download_status(session_id = x@ids$session_id), error = function(e) data.table::data.table())
        } else {
            data.table::data.table()
        }
        if (nrow(tasks)) {
            failed <- tasks[tasks$status %in% c("error", "cancelled")]
            if (nrow(failed)) {
                diagnostics <- shift_bind_diagnostics(
                    diagnostics,
                    shift_diagnostic(
                        "download", "error", "download_failed",
                        sprintf("%d download task(s) failed or were cancelled.", nrow(failed)),
                        query_id = x@ids$query_id,
                        session_id = x@ids$session_id,
                        action = "Retry `shift_download()` with resume = TRUE."
                    )
                )
            }
        }
    }
    diagnostics <- shift_bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftClimate) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    store <- shift_store(x)
    coverage <- store$coverage(plan_id = x@ids$plan_id)
    diagnostics <- shift_bind_diagnostics(x@diagnostics, shift_diagnostics_from_coverage(coverage))
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftMorphed) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    diagnostics <- shift_diagnostics_normalize(x@diagnostics)
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftOutputs) <- function(x, strict = FALSE, ...) {
    checkmate::assert_flag(strict)
    diagnostics <- shift_diagnostics_empty()
    store <- shift_store(x)
    outputs <- shift_outputs(x)
    path_col <- intersect(c("path", "output_path", "relative_path"), names(outputs))
    if (!nrow(outputs) || !length(path_col) || !shift_relative_paths_exist(store, outputs[[path_col[[1L]]]])) {
        diagnostics <- shift_diagnostic(
            "outputs", "error", "missing_epw_output",
            "Expected EPW output files were not found.",
            morph_id = x@ids$morph_id,
            action = "Run `shift_epw()` again or check the output directory."
        )
    }
    diagnostics <- shift_bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics
}

shift_diagnostics_from_coverage <- function(coverage) {
    coverage <- data.table::as.data.table(coverage)
    if (!nrow(coverage)) {
        return(shift_diagnostics_empty())
    }
    diagnostics <- vector("list", nrow(coverage))
    for (i in seq_len(nrow(coverage))) {
        row <- coverage[i]
        if (isTRUE(row$complete[[1L]])) {
            diagnostics[[i]] <- shift_diagnostics_empty()
            next
        }
        severity <- if (identical(row$status[[1L]], "failed")) "error" else "warning"
        message <- if (!is.na(row$last_error[[1L]]) && nzchar(row$last_error[[1L]])) {
            row$last_error[[1L]]
        } else {
            "Extraction coverage is incomplete."
        }
        diagnostics[[i]] <- shift_diagnostic(
            "extract",
            severity,
            "incomplete_extraction",
            message,
            query_id = row$query_id[[1L]],
            plan_id = row$plan_id[[1L]],
            variable_id = row$variable_id[[1L]],
            action = "Run `shift_extract()` again or inspect `shift_coverage()`."
        )
    }
    do.call(shift_bind_diagnostics, diagnostics)
}

# display and conversion ------------------------------------------------------

S7::method(print, ShiftStage) <- function(x, ...) {
    status <- tryCatch(shift_status(x), error = function(e) "unknown")
    cls <- class(x)[[1L]]
    cat(sprintf("<%s>\n", cls))
    cat(sprintf("  stage:  %s\n", x@stage))
    cat(sprintf("  status: %s\n", status))
    if (!is.null(x@store_path)) {
        cat(sprintf("  store:  %s\n", shift_display_path(x@store_path)))
    }
    shift_stage_print_details(x)
    ids <- shift_ids(x)
    ids <- ids[!vapply(ids, is.null, logical(1L))]
    ids <- ids[vapply(ids, function(value) any(!is.na(value)), logical(1L))]
    if (length(ids)) {
        cat(sprintf("  ids:    %s\n", paste(names(ids), collapse = ", ")))
    }
    diagnostics <- shift_diagnostics(x)
    if (nrow(diagnostics)) {
        counts <- table(diagnostics$severity)
        cat(sprintf("  diagnostics: %s\n", paste(sprintf("%s=%s", names(counts), counts), collapse = ", ")))
    }
    invisible(x)
}

S7::method(print, ShiftSite) <- function(x, ...) {
    cat("<ShiftSite>\n")
    cat(sprintf("  id:     %s\n", x@id))
    cat(sprintf("  lonlat: %.6f, %.6f\n", x@lon, x@lat))
    if (!is.null(x@label)) {
        cat(sprintf("  label:  %s\n", x@label))
    }
    if (!is.null(x@epw)) {
        epw <- if (is.character(x@epw)) x@epw else class(x@epw)[[1L]]
        if (is.character(epw)) {
            epw <- shift_display_path(epw)
        }
        cat(sprintf("  epw:    %s\n", epw))
    }
    invisible(x)
}

S7::method(summary, ShiftStage) <- function(object, ...) {
    data.table::data.table(
        class = class(object)[[1L]],
        stage = object@stage,
        status = tryCatch(shift_status(object), error = function(e) "unknown"),
        diagnostic_count = nrow(shift_diagnostics(object))
    )
}

shift_stage_as_data_table <- function(x, ...) {
    if (S7::S7_inherits(x, ShiftRequest)) {
        filters <- x@meta$filters
        return(data.table::data.table(
            provider = x@meta$provider,
            project = shift_coalesce(x@meta$project, NA_character_),
            source = paste(shift_coalesce(x@meta$source, character()), collapse = ","),
            experiment = paste(shift_coalesce(x@meta$experiment, character()), collapse = ","),
            variant = paste(shift_coalesce(x@meta$variant, character()), collapse = ","),
            variables = paste(shift_coalesce(x@meta$variables, character()), collapse = ","),
            frequency = paste(shift_coalesce(x@meta$frequency, character()), collapse = ","),
            filter_count = length(filters)
        ))
    }

    if (S7::S7_inherits(x, ShiftSite)) {
        return(data.table::data.table(
            id = x@id,
            lon = x@lon,
            lat = x@lat,
            label = shift_coalesce(x@label, NA_character_),
            has_epw = !is.null(x@epw)
        ))
    }

    store <- tryCatch(shift_store(x), error = function(e) NULL)
    ids <- shift_ids(x)
    if (S7::S7_inherits(x, ShiftFiles) && !is.null(store)) {
        return(shift_file_catalog(store, ids$query_id))
    }
    if (S7::S7_inherits(x, ShiftDownload) && !is.null(store)) {
        tasks <- if (!is.null(ids$session_id) && !is.na(ids$session_id)) {
            tryCatch(store$download_status(session_id = ids$session_id), error = function(e) data.table::data.table())
        } else {
            data.table::data.table()
        }
        return(tasks)
    }
    if (S7::S7_inherits(x, ShiftClimate) && !is.null(store)) {
        return(store$coverage(plan_id = ids$plan_id))
    }
    if (S7::S7_inherits(x, ShiftMorphed) && !is.null(store)) {
        return(shift_morph_plan(store, ids$morph_id))
    }
    if (S7::S7_inherits(x, ShiftOutputs)) {
        return(shift_outputs(x))
    }

    data.table::data.table()
}
