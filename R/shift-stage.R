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

# Workflow collection uses an explicit ESGF field contract so provider-extra
# metadata cannot change store writes or CMIP6 resolution decisions.
SHIFT_WORKFLOW_FILE_FIELDS <- c(
    "id", "dataset_id", "master_id", "instance_id", "tracking_id", "version",
    "title", "filename", "checksum", "checksum_type", "size", "latest",
    "replica", "retracted", "deprecated", "data_node", "activity_id",
    "institution_id", "source_id", "experiment_id", "variant_label",
    "frequency", "table_id", "variable_id", "grid_label", "datetime_start",
    "datetime_end", "url"
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
    out <- data.table::copy(data.table::as.data.table(x))
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
# ShiftDatasets is an internal persistence envelope for a standalone Dataset
# catalog query. The public API continues to return EsgResultDataset so its
# established query-result methods remain available without an adapter layer.
ShiftDatasets <- S7::new_class("ShiftDatasets", parent = ShiftStage)
ShiftFiles <- S7::new_class("ShiftFiles", parent = ShiftStage)
ShiftDownload <- S7::new_class("ShiftDownload", parent = ShiftStage)
ShiftClimate <- S7::new_class("ShiftClimate", parent = ShiftStage)
ShiftMorphed <- S7::new_class("ShiftMorphed", parent = ShiftStage)
ShiftOutputs <- S7::new_class("ShiftOutputs", parent = ShiftStage)
# ShiftPlan stores a deferred end-to-end workflow that can be explained or run.
ShiftPlan <- S7::new_class("ShiftPlan", parent = ShiftStage)
# ShiftRun is a lightweight handle to a persisted end-to-end workflow run.
ShiftRun <- S7::new_class("ShiftRun", parent = ShiftStage)

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

# ShiftMorphMethod binds a low-level morphing recipe to its explicit workflow
# inputs without making the EpwMorpher engine depend on ESGF reference specs.
ShiftMorphMethod <- S7::new_class(
    "ShiftMorphMethod",
    properties = list(
        name = shift_prop_string(min.chars = 1L),
        recipe = S7::new_property(S7::class_any),
        reference = S7::new_property(S7::class_any, default = NULL),
        observed_reference = S7::new_property(
            S7::class_any,
            default = NULL
        ),
        requires_reference = S7::new_property(S7::class_logical),
        requires_observed_reference = S7::new_property(
            S7::class_logical
        )
    )
)

# ShiftCmip6Spec keeps the complete future-climate identity together so model
# and scenarios cannot drift away from member/grid and discovery constraints.
ShiftCmip6Spec <- S7::new_class(
    "ShiftCmip6Spec",
    properties = list(
        model = S7::new_property(S7::class_character),
        scenarios = S7::new_property(S7::class_character),
        member = S7::new_property(S7::class_any, default = NULL),
        grid = S7::new_property(S7::class_any, default = NULL),
        frequency = shift_prop_string(min.chars = 1L),
        table = S7::new_property(S7::class_any, default = NULL),
        activity = shift_prop_string(min.chars = 1L),
        index_nodes = S7::new_property(S7::class_character),
        data_node = S7::new_property(S7::class_any, default = NULL),
        filters = S7::new_property(S7::class_list, default = list())
    )
)

# ShiftControl centralises workflow-wide execution and fulfilment policies so
# stage option lists cannot silently override them.
ShiftControl <- S7::new_class(
    "ShiftControl",
    properties = list(
        strict = S7::new_property(S7::class_logical),
        allow_partial = S7::new_property(S7::class_logical),
        download = shift_prop_string(min.chars = 1L),
        resume = S7::new_property(S7::class_logical),
        overwrite = S7::new_property(S7::class_logical),
        extraction_method = shift_prop_string(min.chars = 1L),
        output_layout = shift_prop_string(min.chars = 1L)
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

# Summarize a persisted File catalog without materializing every record merely
# to print a ShiftFiles object.
shift__file_catalog_summary <- function(store, query_id) {
    shift_query_maybe(store, sprintf(
        paste(
            "SELECT COUNT(*) AS file_count,",
            "COALESCE(SUM(size), 0) AS total_size",
            "FROM file_catalog WHERE query_id IN (%s)"
        ),
        shift_stage_query_ids(query_id)
    ))
}

# Read only the ordered rows needed for a console preview. An explicit infinite
# limit remains available for users who deliberately request the full print.
shift__file_catalog_preview <- function(store, query_id, n = 10L) {
    columns <- paste(
        c(
            "source_id", "experiment_id", "variable_id", "variant_label",
            "grid_label", "table_id", "datetime_start", "datetime_end",
            "size", "filename", "data_node"
        ),
        collapse = ", "
    )
    limit <- if (is.infinite(n)) "" else sprintf(" LIMIT %d", as.integer(n))
    shift_query_maybe(store, sprintf(
        paste0(
            "SELECT ", columns,
            " FROM file_catalog WHERE query_id IN (%s)",
            " ORDER BY source_id, experiment_id, variable_id, variant_label,",
            " grid_label, table_id, datetime_start, filename", limit
        ),
        shift_stage_query_ids(query_id)
    ))
}

shift_extraction_plan <- function(store, plan_id) {
    shift_query_maybe(store, sprintf(
        paste(
            "SELECT plan_id, query_id, file_key, site_id, variable_id,",
            "lon, lat, method, time_start, time_stop, status,",
            "available_time_count, attempt_count, last_error, created_at, updated_at",
            "FROM extraction_plan WHERE plan_id IN (%s)"
        ),
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
            output_path = results$output_path[[i]],
            output_type = results$output_type[[i]],
            sequence_id = results$sequence_id[[i]],
            weather_year = results$weather_year[[i]],
            calendar = results$calendar[[i]],
            stochastic_seed = results$stochastic_seed[[i]]
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
        dt <- epw_file_read(path)$data()
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
            output_type = outputs$output_type[[i]],
            sequence_id = outputs$sequence_id[[i]],
            weather_year = outputs$weather_year[[i]],
            calendar = outputs$calendar[[i]],
            stochastic_seed = outputs$stochastic_seed[[i]],
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

# Compact paths below the session temp directory before they reach cli's fact
# renderer. Lexical comparison handles planned paths that do not exist yet;
# normalized parent comparison covers Windows short/long path aliases.
shift__display_path <- function(path, temp_root = tempdir()) {
    if (is.null(path) || !nzchar(path)) {
        return(path)
    }
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_string(temp_root, min.chars = 1L)

    # Use one separator for lexical comparison. Drive-letter paths are
    # case-insensitive even when this pure branch is exercised on Unix CI.
    lexical <- function(value) {
        sub("/+$", "", gsub("\\\\", "/", path.expand(value)))
    }
    compact <- function(candidate, root) {
        windows_path <- grepl("^[A-Za-z]:/", candidate) ||
            grepl("^[A-Za-z]:/", root)
        candidate_key <- if (windows_path) tolower(candidate) else candidate
        root_key <- if (windows_path) tolower(root) else root
        inside <- identical(candidate_key, root_key) ||
            startsWith(candidate_key, paste0(root_key, "/"))
        if (!inside) {
            return(NULL)
        }
        paste0("<tempdir>", substring(candidate, nchar(root) + 1L))
    }

    expanded <- lexical(path)
    temp_expanded <- lexical(temp_root)
    displayed <- compact(expanded, temp_expanded)
    if (!is.null(displayed)) {
        return(displayed)
    }

    normalized <- lexical(normalizePath(path, winslash = "/",
        mustWork = FALSE))
    temp_normalized <- lexical(normalizePath(temp_root, winslash = "/",
        mustWork = FALSE))
    displayed <- compact(normalized, temp_normalized)
    if (!is.null(displayed)) {
        return(displayed)
    }

    # On Windows an existing temp root may normalize to an 8.3 alias while its
    # not-yet-created child retains the long form. Normalize the existing
    # parent independently and then reconstruct the planned child path.
    parent <- lexical(normalizePath(dirname(expanded), winslash = "/",
        mustWork = FALSE))
    reconstructed <- paste0(parent, "/", basename(expanded))
    displayed <- compact(reconstructed, temp_normalized)
    if (!is.null(displayed)) {
        return(displayed)
    }
    normalized
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

# Map public standalone functions onto stable task IDs and concise dashboard
# titles. These IDs also form the persisted object-carried step sequence.
shift__task_label <- function(task) {
    labels <- c(
        datasets = "Collect Datasets",
        collect = "Collect CMIP6",
        download = "Download CMIP6",
        extract = "Extract Climate",
        morph = "Morph EPW",
        write_epw = "Write EPW",
        export_epw = "Export EPW"
    )
    key <- as.character(task)[[1L]]
    # Keep package extensions safe even before they add a dedicated title.
    if (key %in% names(labels)) {
        return(unname(labels[[key]]))
    }
    shift__ui_stage_label(key)
}

# A private dynamic stack transports the one active reporter through S7 dispatch
# and nested stage calls without exposing an implementation parameter publicly.
# Unlike the removed workflow-session scope, this stack lives only for the
# duration of one synchronous operation and never identifies scientific state.
SHIFT_REPORTER_STACK <- new.env(parent = emptyenv())
SHIFT_REPORTER_STACK$values <- list()

# A second private stack carries the number of catalog units owned by a nested
# collect operation. This prevents Dataset helpers from guessing the parent
# task name when they run inside resolve, morph, or another composite stage.
SHIFT_CATALOG_UNIT_TOTAL_STACK <- new.env(parent = emptyenv())
SHIFT_CATALOG_UNIT_TOTAL_STACK$values <- integer()

# Return the reporter owned by the current operation, if one exists.
shift__current_reporter <- function() {
    values <- SHIFT_REPORTER_STACK$values
    if (!length(values)) NULL else values[[length(values)]]
}

# Evaluate one expression with a reporter installed for internal stage methods.
# Nested calls restore the preceding reporter deterministically on every exit.
shift__with_reporter <- function(reporter, code) {
    SHIFT_REPORTER_STACK$values <- c(SHIFT_REPORTER_STACK$values,
        list(reporter))
    on.exit({
        values <- SHIFT_REPORTER_STACK$values
        SHIFT_REPORTER_STACK$values <- if (length(values) > 1L) {
            values[-length(values)]
        } else {
            list()
        }
    }, add = TRUE)
    force(code)
}

# Return the catalog-unit scale selected by the nearest composite operation.
shift__catalog_unit_total <- function(default = 1L) {
    values <- SHIFT_CATALOG_UNIT_TOTAL_STACK$values
    if (!length(values)) as.integer(default) else values[[length(values)]]
}

# Evaluate one nested Dataset query on its parent's catalog-unit scale.
shift__with_catalog_unit_total <- function(total, code) {
    checkmate::assert_int(total, lower = 1L)
    SHIFT_CATALOG_UNIT_TOTAL_STACK$values <- c(
        SHIFT_CATALOG_UNIT_TOTAL_STACK$values, total)
    on.exit({
        values <- SHIFT_CATALOG_UNIT_TOTAL_STACK$values
        SHIFT_CATALOG_UNIT_TOTAL_STACK$values <- if (length(values) > 1L) {
            values[-length(values)]
        } else {
            integer()
        }
    }, add = TRUE)
    force(code)
}

# Apply an internal stage call under the reporter scope without adding a public
# reporter formal to the shift API.
shift__do_call_with_reporter <- function(reporter, what, args) {
    shift__with_reporter(reporter, do.call(what, args))
}

# Resume uses a short-lived internal override to append a new attempt to the
# same failed run. This is not ambient user state: it exists only while one
# public stage call is synchronously rebuilt from its persisted step spec.
SHIFT_RUN_OVERRIDE_STACK <- new.env(parent = emptyenv())
SHIFT_RUN_OVERRIDE_STACK$values <- list()

# Return the run selected by the active resume operation, if any.
shift__current_run_override <- function() {
    values <- SHIFT_RUN_OVERRIDE_STACK$values
    if (!length(values)) NULL else values[[length(values)]]
}

# Evaluate one reconstructed stage call under a durable run identity.
shift__with_run_override <- function(run_id, code) {
    checkmate::assert_string(run_id, min.chars = 1L)
    SHIFT_RUN_OVERRIDE_STACK$values <- c(SHIFT_RUN_OVERRIDE_STACK$values,
        list(run_id))
    on.exit({
        values <- SHIFT_RUN_OVERRIDE_STACK$values
        SHIFT_RUN_OVERRIDE_STACK$values <- if (length(values) > 1L) {
            values[-length(values)]
        } else {
            list()
        }
    }, add = TRUE)
    force(code)
}

# Resolve presentation once per operation. UI state is deliberately not
# inherited from persisted scientific objects and never enters a spec hash.
shift__task_ui <- function(ui = NULL) {
    value <- shift_coalesce(ui, shift_ui())
    if (!S7::S7_inherits(value, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    value
}

# Find the one authoritative store for a task and reject accidental cross-store
# input before any artifact or run row is written.
shift__task_store_value <- function(x, store = NULL) {
    input_path <- if (S7::S7_inherits(x, ShiftStage)) x@store_path else NULL
    supplied_path <- if (inherits(store, "EsgStore")) store$path else store
    candidates <- c(input_path, supplied_path)
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    normalized <- unique(vapply(candidates, function(path) {
        normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    }, character(1L)))
    if (length(normalized) > 1L) {
        cli::cli_abort(c(
            "A shift task cannot span multiple stores.",
            "x" = "Session, input stage, and `store` must resolve to the same directory."
        ))
    }
    if (inherits(store, "EsgStore")) return(store)
    if (length(normalized)) normalized[[1L]] else store_dir()
}

# Read the completed task lineage recursively. Child runs inherit display
# history without mutating a terminal parent or copying its durable steps.
shift__run_task_history <- function(store, run_id, seen = character()) {
    if (is.null(run_id) || is.na(run_id) || !nzchar(run_id) || run_id %in% seen) {
        return(character())
    }
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    row <- runs[runs[["run_id"]] == wanted_run_id]
    if (!nrow(row)) return(character())
    spec <- tryCatch(jsonlite::fromJSON(row$spec_json[[1L]],
        simplifyVector = TRUE), error = function(e) list())
    parent <- store__chr1(spec$parent_run_id)
    inherited <- shift__run_task_history(store, parent, c(seen, run_id))
    steps <- morpher__private_store(store)$read_table("shift_run_step")
    steps <- steps[steps[["run_id"]] == wanted_run_id]
    completed <- as.character(steps[steps[["status"]] %in%
        c("completed", "partial")]$task)
    unique(c(inherited, completed))
}

# Build the cumulative stage rail from the input run lineage plus the current
# task. A branched child therefore remains visually connected to its source.
shift__task_sequence <- function(store, run_id, task) {
    completed <- shift__run_task_history(store, run_id)
    list(
        sequence = unique(c(completed, task)),
        completed = unique(completed)
    )
}

# Decide whether an input stage can append to its run. Only the latest completed
# step of a waiting run is a valid continuation point; terminal or stale inputs
# fork a child run so persisted history remains append-only.
shift__task_run_context <- function(x, store) {
    ids <- if (S7::S7_inherits(x, ShiftStage)) x@ids else list()
    input_run_id <- store__chr1(ids$run_id)
    input_step_id <- store__chr1(ids$step_id)
    if (is.na(input_run_id) || !nzchar(input_run_id)) {
        return(list(run_id = NULL, parent_run_id = NULL,
            lineage_id = NULL, continued = FALSE))
    }

    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    row <- runs[runs[["run_id"]] == input_run_id]
    if (!nrow(row)) {
        cli::cli_abort(c(
            "Input stage refers to an unknown shift run {.val {input_run_id}}.",
            "i" = "Use the store that created the stage or recreate the upstream stage."
        ))
    }
    latest <- shift__latest_step(store, input_run_id)
    can_continue <- identical(row$status[[1L]], "waiting") &&
        nrow(latest) && !is.na(input_step_id) && nzchar(input_step_id) &&
        identical(latest$step_id[[1L]], input_step_id) &&
        latest$status[[1L]] %in% c("completed", "partial")
    spec <- tryCatch(jsonlite::fromJSON(row$spec_json[[1L]],
        simplifyVector = TRUE), error = function(e) list())
    lineage_id <- as.character(shift_coalesce(spec$lineage_id,
        input_run_id))[[1L]]
    if (isTRUE(can_continue)) {
        return(list(run_id = input_run_id, parent_run_id = NULL,
            lineage_id = lineage_id, continued = TRUE))
    }
    list(run_id = NULL, parent_run_id = input_run_id,
        lineage_id = lineage_id, continued = FALSE)
}

# Describe a stage operation without embedding low-level objects in reporter
# state. Detailed IDs remain available through shift_explain() and the store.
shift__task_context <- function(task, x, store) {
    input <- if (S7::S7_inherits(x, ShiftStage)) {
        sprintf("input %s", x@stage)
    } else {
        "new request"
    }
    list(
        title = shift__task_label(task),
        items = c(shift__task_label(task), input),
        store = store$path,
        message = paste("Preparing", tolower(shift__task_label(task)))
    )
}

# Summarize the persisted artifact rather than repeating implementation-level
# callbacks in the terminal completion receipt.
shift__task_summary <- function(task, result) {
    switch(task,
        datasets = sprintf("%d dataset(s) collected",
            as.integer(shift_coalesce(result@meta$dataset_count, 0L))),
        collect = sprintf("%d dataset(s) and %d file(s) collected",
            as.integer(shift_coalesce(result@meta$dataset_count, 0L)),
            as.integer(shift_coalesce(result@meta$file_count, 0L))),
        download = {
            session_id <- shift_coalesce(result@ids$session_id, "download session")
            sprintf("download session %s registered", session_id)
        },
        extract = sprintf("%d extraction plan(s) processed",
            length(shift_coalesce(result@ids$plan_id, character()))),
        morph = sprintf("morph result %s ready",
            shift_coalesce(result@ids$morph_id, "registered")),
        write_epw = sprintf("%d EPW output(s) written", nrow(shift_outputs(result))),
        export_epw = sprintf("%d EPW output(s) exported", nrow(shift_outputs(result))),
        sprintf("%s completed", shift__task_label(task))
    )
}

# Return delivery paths for the generic result receipt while leaving other
# stages path-free.
shift__task_output_paths <- function(result) {
    if (!S7::S7_inherits(result, ShiftOutputs)) return(character())
    rows <- shift_outputs(result)
    path <- if ("export_path" %in% names(rows)) rows$export_path else rows$path
    as.character(path[!is.na(path) & nzchar(path)])
}

# Attach durable recovery coordinates to the original stage condition without
# replacing its message or call. Callers can inspect run_id/step_id/store while
# interactive users continue to see the reporter's single failure receipt.
shift__task_condition <- function(condition, run_id, step_id, store_path) {
    condition$run_id <- run_id
    condition$step_id <- step_id
    condition$store <- store_path
    class(condition) <- unique(c("epwshiftr_shift_error", class(condition)))
    condition
}

# Execute one public standalone stage through the shared reporter and durable
# run/step state machine. The stage-specific closure remains responsible only
# for scientific work and business-unit progress.
shift__task_execute <- function(task, x, code, store = NULL, ui = NULL,
                                spec = list(), resumable = TRUE,
                                nonresumable_reason = NULL,
                                auto_complete = FALSE) {
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_function(code)
    checkmate::assert_list(spec)
    checkmate::assert_flag(resumable)
    checkmate::assert_flag(auto_complete)
    ui <- shift__task_ui(ui)
    store_value <- shift__task_store_value(x, store)
    opened <- shift_store(store_value, create = TRUE)
    own_store <- !inherits(store_value, "EsgStore")
    if (isTRUE(own_store)) on.exit(try(opened$close(), silent = TRUE), add = TRUE)

    override_run_id <- shift__current_run_override()
    context <- if (is.null(override_run_id)) {
        shift__task_run_context(x, opened)
    } else {
        list(run_id = override_run_id, parent_run_id = NULL,
            lineage_id = NULL, continued = TRUE)
    }
    run_id <- context$run_id
    if (!is.null(override_run_id)) {
        override_run <- shift__run_handle(opened, override_run_id)
        override_status <- shift_status(override_run, refresh = FALSE)
        if (!identical(override_status, "waiting")) {
            cli::cli_abort("Resume target {.val {override_run_id}} is not waiting.")
        }
    }
    if (is.null(run_id)) {
        run_spec <- c(spec, list(
            store = opened$path,
            parent_run_id = context$parent_run_id,
            lineage_id = shift_coalesce(context$lineage_id, NULL)
        ))
        run_id <- shift__task_run_register(opened, task,
            spec = run_spec, status = "queued")
    }
    step <- shift__step_create(opened, run_id, task, spec,
        input_stage = if (S7::S7_inherits(x, ShiftStage)) x else NULL,
        resumable = resumable,
        nonresumable_reason = nonresumable_reason)
    step_id <- step$step_id[[1L]]
    job <- shift__job_create(opened, run_id, mode = "foreground", ui = ui,
        step_id = step_id)
    job_id <- job$job_id[[1L]]
    reporter <- shift__reporter(ui, store = opened, run_id = run_id,
        job_id = job_id, step_id = step_id)
    sequence <- shift__task_sequence(opened, run_id, task)
    reporter$operation_started(
        task,
        shift__task_label(task),
        context = shift__task_context(task, x, opened),
        stage_sequence = sequence$sequence,
        completed_stages = sequence$completed
    )
    shift__run_update(opened, run_id, status = "running",
        current_stage = task, completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_)

    tryCatch({
        result <- code(reporter, opened)
        shift_assert_stage(result)
        result@ids <- utils::modifyList(result@ids,
            list(run_id = run_id, step_id = step_id))
        artifact_status <- shift_status(result)
        step_status <- if (artifact_status %in% c("partial", "blocked", "failed")) {
            "partial"
        } else {
            "completed"
        }
        session_id <- store__chr1(result@ids$session_id)
        detached <- identical(task, "download") &&
            isTRUE(spec$background) && !is.na(session_id) && nzchar(session_id)
        if (isTRUE(detached)) {
            # The Downloader owns the long-running process after registration.
            # Keep this step open until shift_run_get() reconciles its durable
            # session instead of claiming that the next stage is ready.
            shift__step_update(opened, step_id,
                status = "running",
                output_stage_json = shift__spec_json(shift__stage_ref(result)),
                completed_at = as.POSIXct(NA, tz = "UTC"),
                last_error = NA_character_)
        } else {
            shift__step_finish(opened, step_id, step_status,
                output_stage = result)
        }
        shift__job_update(opened, job_id, status = "completed",
            completed_at = store__now(), exit_code = 0L,
            last_error = NA_character_)
        ids <- result@ids
        run_updates <- list()
        # Each step owns only part of the artifact graph. Preserve identifiers
        # written by upstream steps instead of replacing them with missing
        # fields from the current result object.
        if (!is.null(ids$query_id)) {
            run_updates$query_id <- store__chr1(ids$query_id)
        }
        if (!is.null(ids$plan_id)) {
            run_updates$plan_ids_json <-
                shift__spec_json(as.character(ids$plan_id))
        }
        if (!is.null(ids$morph_id)) {
            run_updates$morph_id <- store__chr1(ids$morph_id)
        }
        if (!is.null(result@meta$export_dir)) {
            run_updates$output_dir <- store__chr1(result@meta$export_dir)
        }
        # An empty catalog is not a hand-off point: extraction cannot do useful
        # work without a File record. Other partial stages may still contain a
        # complete subset and therefore retain the established continuation
        # semantics.
        empty_collection <- identical(task, "collect") &&
            as.integer(shift_coalesce(result@meta$file_count, 0L)) < 1L
        terminal <- isTRUE(auto_complete) || isTRUE(empty_collection)
        if (isTRUE(detached)) {
            do.call(shift__run_update, c(list(store = opened, run_id = run_id,
                status = "running", current_stage = task,
                last_error = NA_character_), run_updates))
        } else if (isTRUE(terminal)) {
            final_status <- shift__run_completion_status(opened, run_id)
            do.call(shift__run_finish, c(list(store = opened, run_id = run_id,
                status = final_status, current_stage = task,
                last_error = NA_character_), run_updates))
        } else {
            do.call(shift__run_update, c(list(store = opened, run_id = run_id,
                status = "waiting", current_stage = task,
                last_error = NA_character_), run_updates))
        }
        summary <- shift__task_summary(task, result)
        paths <- shift__task_output_paths(result)
        event_status <- if (isTRUE(detached)) {
            "running"
        } else if (isTRUE(terminal)) {
            step_status
        } else {
            "waiting"
        }
        shift__run_event(opened, run_id, task, event_status, summary,
            details = list(
                phase = "operation",
                stage = task,
                stage_sequence = sequence$sequence,
                step_id = step_id,
                outcome = if (isTRUE(detached)) "running" else step_status
            ),
            step_id = step_id)
        if (isTRUE(detached)) {
            reporter$operation_detached(summary, output_paths = paths,
                output_dir = result@meta$export_dir)
        } else if (isTRUE(empty_collection)) {
            reporter$operation_partial(summary, output_paths = paths,
                output_dir = result@meta$export_dir)
        } else if (isTRUE(terminal)) {
            reporter$operation_completed(summary, output_paths = paths,
                output_dir = result@meta$export_dir)
        } else {
            reporter$operation_waiting(summary, output_paths = paths,
                output_dir = result@meta$export_dir)
        }
        result
    }, interrupt = function(e) {
        message <- sprintf("%s was cancelled.", shift__task_label(task))
        try(shift__step_finish(opened, step_id, "cancelled",
            last_error = message), silent = TRUE)
        try(shift__job_update(opened, job_id, status = "cancelled",
            completed_at = store__now(), exit_code = 130L,
            last_error = message), silent = TRUE)
        try(shift__run_finish(opened, run_id, "cancelled",
            current_stage = task, last_error = message), silent = TRUE)
        try(shift__run_event(opened, run_id, task, "cancelled", message,
            details = list(phase = "operation", stage = task,
                step_id = step_id, outcome = "cancelled"),
            step_id = step_id), silent = TRUE)
        reporter$operation_failed(message, cancelled = TRUE)
        stop(shift__task_condition(e, run_id, step_id, opened$path))
    }, error = function(e) {
        message <- conditionMessage(e)
        try(shift__step_finish(opened, step_id, "failed",
            last_error = message), silent = TRUE)
        try(shift__job_update(opened, job_id, status = "failed",
            completed_at = store__now(), exit_code = 1L,
            last_error = message), silent = TRUE)
        try(shift__run_finish(opened, run_id, "failed",
            current_stage = task, last_error = message), silent = TRUE)
        try(shift__run_event(opened, run_id, task, "failed", message,
            details = list(phase = "operation", stage = task,
                step_id = step_id, outcome = "failed", cause = message),
            step_id = step_id), silent = TRUE)
        reporter$operation_failed(message, details = list(cause = message))
        stop(shift__task_condition(e, run_id, step_id, opened$path))
    })
}

# Mark a successful intermediate stage as the intentional endpoint of its run.
# Normal pipelines do not need this helper because EPW export completes the run
# automatically; it exists for workflows that deliberately stop after collect,
# download, extract, morph, or store-local EPW writing.
#' @rdname shift_api
#' @export
shift_complete <- function(x) {
    shift_assert_stage(x)
    if (S7::S7_inherits(x, ShiftRun)) {
        run <- shift_refresh(x)
        input_step_id <- NA_character_
    } else {
        run <- shift_run_get(x)
        input_step_id <- store__chr1(x@ids$step_id)
    }
    status <- shift_status(run, refresh = FALSE)
    if (status %in% c("completed", "partial")) return(run)
    if (!identical(status, "waiting")) {
        cli::cli_abort(c(
            "Only a waiting shift run can be completed; current status is {.val {status}}.",
            "i" = "Use {.fn shift_resume} for failed or cancelled work."
        ))
    }

    store <- shift_store(run)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    latest <- shift__latest_step(store, run@ids$run_id)
    if (!nrow(latest)) {
        cli::cli_abort("Shift run {.val {run@ids$run_id}} has no stage to complete.")
    }
    if (!S7::S7_inherits(x, ShiftRun) &&
        (is.na(input_step_id) || !identical(input_step_id,
            latest$step_id[[1L]]))) {
        cli::cli_abort(c(
            "The supplied stage is not the latest result of shift run {.val {run@ids$run_id}}.",
            "i" = "Complete the latest stage or continue from this older stage to create a child run."
        ))
    }
    final_status <- shift__run_completion_status(store, run@ids$run_id)
    shift__run_finish(store, run@ids$run_id, final_status,
        current_stage = latest$task[[1L]], last_error = NA_character_)
    shift__run_event(store, run@ids$run_id, latest$task[[1L]],
        final_status, sprintf("%s marked as the final stage.",
            shift__task_label(latest$task[[1L]])),
        details = list(step_id = latest$step_id[[1L]],
            outcome = final_status),
        step_id = latest$step_id[[1L]])
    shift__run_handle(store, run@ids$run_id)
}

# Parse user-facing year inputs used by workflow plans and presets.
shift__years_value <- function(value, arg = "years") {
    if (is.numeric(value) && !inherits(value, c("Date", "POSIXt"))) {
        checkmate::assert_integerish(value, any.missing = FALSE, min.len = 1L)
        return(as.integer(value))
    }
    if (is.character(value)) {
        pieces <- trimws(unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE))
        pieces <- pieces[nzchar(pieces)]
        years <- integer()
        for (piece in pieces) {
            if (grepl(":", piece, fixed = TRUE)) {
                bounds <- suppressWarnings(as.integer(trimws(strsplit(piece, ":", fixed = TRUE)[[1L]])))
                if (length(bounds) != 2L || any(is.na(bounds))) {
                    cli::cli_abort("`{arg}` contains an invalid year range: {.val {piece}}.")
                }
                years <- c(years, seq.int(min(bounds), max(bounds)))
            } else {
                year <- suppressWarnings(as.integer(piece))
                if (length(year) != 1L || is.na(year)) {
                    cli::cli_abort("`{arg}` contains an invalid year: {.val {piece}}.")
                }
                years <- c(years, year)
            }
        }
        return(unique(years))
    }
    cli::cli_abort("`{arg}` must be numeric years or character year ranges.")
}

# Normalize period inputs so users can pass either epw_morph_periods() output or
# a named list such as list(`2060s` = 2055:2065).
shift__periods_from_input <- function(periods, arg = "periods") {
    if (is.data.frame(periods)) {
        checkmate::assert_names(names(periods), must.include = c("period", "year"))
        return(data.table::as.data.table(periods))
    }
    if (!is.list(periods) || is.null(names(periods)) || any(!nzchar(names(periods)))) {
        cli::cli_abort("`{arg}` must be a period table or a named list of years.")
    }
    values <- lapply(seq_along(periods), function(i) {
        shift__years_value(periods[[i]], sprintf("%s$%s", arg, names(periods)[[i]]))
    })
    do.call(epw_morph_periods, stats::setNames(values, names(periods)))
}

# Build a one-period table from the common years + period_name shorthand.
shift__periods_from_years <- function(years, period = "future", arg = "years") {
    checkmate::assert_string(period, min.chars = 1L)
    years <- shift__years_value(years, arg = arg)
    do.call(epw_morph_periods, stats::setNames(list(years), period))
}

# Resolve recipe strings early so later workflow stages can rely on a recipe
# object and its required variable set.
shift__recipe_value <- function(recipe) {
    if (inherits(recipe, "epw_morph_recipe")) {
        return(recipe)
    }
    if (is.character(recipe) && length(recipe) == 1L) {
        return(epw_morph_recipe(recipe))
    }
    cli::cli_abort("`recipe` must be a recipe name or an {.cls epw_morph_recipe} object.")
}

# Let high-level APIs accept named variable sets while leaving explicit CMIP
# variable IDs untouched.
shift__variables_value <- function(variables, recipe = NULL) {
    if (is.null(variables)) {
        return(epw_morph_variables(shift_coalesce(recipe, "recommended")))
    }
    if (inherits(variables, "epw_morph_recipe") || inherits(variables, "EpwMorphBackend")) {
        return(epw_morph_variables(variables))
    }
    variables <- as.character(variables)
    if (length(variables) == 1L &&
        variables %in% c(names(EPW_MORPH_VARIABLE_LEVELS), epw_morph_backends())) {
        return(epw_morph_variables(variables))
    }
    variables[!is.na(variables) & nzchar(variables)]
}

# Store paths are normalized before planning so plans are portable and printable
# even when execution is deferred.
shift__store_path_value <- function(store, create = FALSE) {
    checkmate::assert_flag(create)
    if (inherits(store, "EsgStore")) {
        return(normalizePath(store$path, winslash = "/", mustWork = FALSE))
    }
    checkmate::assert_string(store, min.chars = 1L)
    if (isTRUE(create) && !dir.exists(store)) {
        dir.create(store, recursive = TRUE, showWarnings = FALSE)
    }
    normalizePath(store, winslash = "/", mustWork = FALSE)
}

# Drop NULL values from named lists before forwarding them to stage functions.
shift__compact_list <- function(x) {
    x[vapply(x, Negate(is.null), logical(1L))]
}

# Keep only arguments accepted by the target workflow stage.
shift__list_subset <- function(x, allowed) {
    x[intersect(names(x), allowed)]
}

# Validate middle-layer stage options before a plan is created so misspellings
# and attempts to override workflow-wide policies cannot be silently ignored.
shift__validate_stage_options <- function(x, stage, allowed) {
    checkmate::assert_list(x, names = "unique")
    if (length(x) && (is.null(names(x)) || any(!nzchar(names(x))))) {
        cli::cli_abort("Every `{stage}` stage option must be named.")
    }
    workflow_fields <- c(
        "strict", "allow_partial", "resume", "overwrite", "complete_only",
        "run", "download", "method"
    )
    duplicated_policy <- intersect(names(x), workflow_fields)
    if (length(duplicated_policy)) {
        cli::cli_abort(c(
            "`{stage}` cannot override workflow control field(s): {.field {duplicated_policy}}.",
            "i" = "Configure workflow-wide behaviour with {.fn shift_control}."
        ))
    }
    unknown <- setdiff(names(x), allowed)
    if (length(unknown)) {
        cli::cli_abort("Unknown `{stage}` stage option(s): {.field {unknown}}.")
    }
    x
}

# Build the immutable user case matrix before member and grid auto-selection;
# unresolved dimensions remain explicit missing values until the resolver pins
# them for the persisted run.
shift__expected_cases <- function(request, periods) {
    request_meta <- request@meta
    sources <- shift_coalesce(request_meta$source, request_meta$filters$source_id)
    experiments <- shift_coalesce(request_meta$experiment, request_meta$filters$experiment_id)
    members <- shift_coalesce(request_meta$variant, request_meta$filters$variant_label)
    grids <- request_meta$filters$grid_label
    scalar_or_missing <- function(value) {
        value <- as.character(value)
        if (length(value)) value else NA_character_
    }
    sources <- scalar_or_missing(sources)
    experiments <- scalar_or_missing(experiments)
    members <- scalar_or_missing(members)
    grids <- scalar_or_missing(grids)
    period_names <- unique(as.character(periods$period))

    cases <- data.table::CJ(
        source_id = sources,
        experiment_id = experiments,
        variant_label = members,
        grid_label = grids,
        period = period_names,
        unique = TRUE
    )
    # Keep the exact requested year set as a list column because coverage is a
    # case-level contract, not just a min/max time filter.
    cases[, years := lapply(period, function(value) {
        as.integer(periods[periods[["period"]] == value, year])
    })]
    cases[, case_id := vapply(
        seq_len(.N),
        function(i) store__hash(
            source_id[[i]], experiment_id[[i]], variant_label[[i]],
            grid_label[[i]], period[[i]], years[[i]]
        ),
        character(1L)
    )]
    cases[, `:=`(
        required = TRUE,
        status = "planned",
        output_id = NA_character_,
        export_path = NA_character_,
        missing_reason = NA_character_
    )]
    data.table::setcolorder(
        cases,
        c("case_id", "source_id", "experiment_id", "variant_label", "grid_label",
          "period", "years", "required", "status", "output_id", "export_path",
          "missing_reason")
    )
    cases[]
}

# Record the durable baseline EPW identity used for run hashing and resume.
shift__epw_identity <- function(epw) {
    if (shift_is_epw_path(epw)) {
        path <- normalizePath(path.expand(epw), winslash = "/", mustWork = TRUE)
        return(list(path = path, checksum = store_hash_file(path, "sha256"), checksum_type = "sha256"))
    }
    if (shift_is_epw_object(epw)) {
        path <- epw_file_coerce(epw)$path()
        return(list(path = path, checksum = store_hash_file(path, "sha256"), checksum_type = "sha256"))
    }
    cli::cli_abort("`epw` must be an EPW file path or an object inheriting from {.cls Epw} or {.cls EpwFile}.")
}

# Choose CMIP table defaults that match the most common atmospheric frequencies.
shift__cmip6_table_id <- function(frequency) {
    frequency <- as.character(frequency)[[1L]]
    switch(
        frequency,
        mon = "Amon",
        day = "day",
        `3hr` = "3hr",
        `6hr` = "6hr",
        NULL
    )
}

# Validate the two supported table-selection forms. An unnamed scalar pins all
# variables to one table, while a fully named vector overrides only the named
# variables and leaves the remainder on their automatic tables.
shift__cmip6_table_spec <- function(table, null.ok = TRUE) {
    if (is.null(table)) {
        if (isTRUE(null.ok)) {
            return(NULL)
        }
        cli::cli_abort("`table` cannot be `NULL` here.")
    }
    if (is.list(table)) {
        if (is.null(names(table)) || any(!nzchar(names(table)))) {
            cli::cli_abort("A list supplied as `table` must have one name for every variable override.")
        }
        table <- vapply(table, function(value) {
            checkmate::assert_string(value, min.chars = 1L)
            value
        }, character(1L))
    }
    checkmate::assert_character(table, any.missing = FALSE, min.len = 1L)
    table_names <- names(table)
    table <- as.character(table)
    names(table) <- table_names
    named <- !is.null(names(table)) && any(nzchar(names(table)))
    if (isTRUE(named) && any(!nzchar(names(table)))) {
        cli::cli_abort("A named `table` vector must name every element.")
    }
    if (!isTRUE(named) && length(table) != 1L) {
        cli::cli_abort("An unnamed `table` value must be a single table ID.")
    }
    if (isTRUE(named) && anyDuplicated(names(table))) {
        cli::cli_abort("Variable names in `table` must be unique.")
    }
    table
}

# Resolve each requested source variable to its CMIP6 table. Snow depth is a
# land-state variable in LImon; all other monthly inputs retain the atmospheric
# Amon default unless the caller pins or overrides them explicitly.
shift__cmip6_variable_tables <- function(variables, frequency, table = NULL) {
    variables <- unique(as.character(variables))
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L)
    table <- shift__cmip6_table_spec(table)
    default <- shift__cmip6_table_id(frequency)
    if (is.null(default)) {
        cli::cli_abort("Cannot infer a CMIP6 table for frequency {.val {frequency}}; set `table` explicitly.")
    }
    out <- stats::setNames(rep(default, length(variables)), variables)
    if (identical(as.character(frequency)[[1L]], "mon") && "snd" %in% variables) {
        out[["snd"]] <- "LImon"
    }
    if (is.null(table)) {
        return(out)
    }
    if (is.null(names(table))) {
        out[] <- table[[1L]]
        return(out)
    }
    unknown <- setdiff(names(table), variables)
    if (length(unknown)) {
        cli::cli_abort("`table` contains override(s) for variables not used by the recipe: {.val {unknown}}.")
    }
    out[names(table)] <- unname(table)
    out
}

# Collapse a possibly long vector into a stable console summary while retaining
# its cardinality for scientific identities such as variables and scenarios.
shift__display_values <- function(x, max = 7L) {
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
    inherits(x, "EpwFile") || epw_file_is_external(x)
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
        epw_file_read(epw)
    } else if (shift_is_epw_object(epw)) {
        epw_file_coerce(epw)
    } else {
        cli::cli_abort("`epw` must be an EPW file path or an object inheriting from {.cls Epw} or {.cls EpwFile}.")
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
        cli::cli_abort("A baseline EPW file is required.")
    }
    if (is.character(x) && length(x) == 1L) {
        return(epw_file_read(x))
    }
    if (shift_is_epw_object(x)) {
        return(epw_file_coerce(x))
    }
    cli::cli_abort("A baseline EPW must be a file path or an object inheriting from {.cls Epw} or {.cls EpwFile}.")
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
#' @param source,experiment,variant,frequency Provider-neutral request fields.
#'   Values must use the selected provider's controlled vocabulary.
#'   In `shift_reference_historical()`, `experiment` is the historical
#'   reference experiment filter. Values are not translated; for ESGF, use
#'   exact facet values such as `project = "CMIP6"` and `frequency = "mon"`.
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
#' @param id Optional site identifier. If `id` is an EPW file path and `epw`
#'   is `NULL`, it is treated as `epw`.
#' @param lon,lat Optional site longitude and latitude. Missing values are read
#'   from the EPW LOCATION header when `epw` is supplied.
#' @param label Optional human-readable label.
#' @param epw A baseline EPW path, internal `EpwFile`, or external object
#'   inheriting from `"Epw"` in site and task APIs; in [shift_plan()], a named
#'   EPW export option list.
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
    provider <- tolower(provider)
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
        provider = provider,
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
    if (epw_file_is_external(epw)) {
        # Convert once at the public site boundary so every downstream stage
        # sees only the internal EPW protocol.
        epw <- epw_file_coerce(epw)
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
#' @param recipe A low-level [epw_morph_recipe()] object.
#' @param reference An explicit `ShiftReferenceSpec`, extracted `ShiftClimate`
#'   stage, or `NULL` containing historical model output. Optional-reference
#'   methods such as [belcher()] use the baseline EPW climatology when `NULL`.
#' @param observed_reference An explicit plan-backed `ShiftReferenceSpec`,
#'   extracted `ShiftClimate` stage, or `NULL` containing multi-year observed
#'   daily weather. It is never substituted for `reference`.
#' @export
shift_morph_method <- function(
    recipe,
    reference = NULL,
    observed_reference = NULL
) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    for (name in c("reference", "observed_reference")) {
        value <- get(name, inherits = FALSE)
        if (!is.null(value) &&
            !S7::S7_inherits(value, ShiftReferenceSpec) &&
            !S7::S7_inherits(value, ShiftClimate)) {
            cli::cli_abort(
                "{.arg {name}} must be a {.cls ShiftReferenceSpec}, a {.cls ShiftClimate} stage, or `NULL`."
            )
        }
    }
    if (S7::S7_inherits(observed_reference, ShiftReferenceSpec) &&
        !identical(observed_reference@mode, "plan")) {
        cli::cli_abort(
            paste(
                "{.arg observed_reference} must use a plan-backed reference;",
                "automatic historical CMIP resolution produces model output,",
                "not observations."
            )
        )
    }

    requires_reference <- isTRUE(morpher__recipe_requires_reference(recipe))
    accepts_reference <- isTRUE(morpher__recipe_accepts_reference(recipe))
    requires_observed <- isTRUE(
        morpher__recipe_requires_observed_reference(recipe)
    )
    accepts_observed <- isTRUE(
        morpher__recipe_accepts_observed_reference(recipe)
    )
    if (requires_reference && is.null(reference)) {
        cli::cli_abort(c(
            "The selected morphing method requires an explicit reference containing historical model output.",
            "i" = "Supply `reference` as a reference spec or extracted climate when constructing the method."
        ))
    }
    if (!accepts_reference && !is.null(reference)) {
        cli::cli_abort("The selected morphing method does not accept reference climate data.")
    }
    if (requires_observed && is.null(observed_reference)) {
        cli::cli_abort(c(
            "The selected morphing method requires an explicit observed reference.",
            "i" = "Supply `observed_reference` as a plan-backed reference spec or extracted climate when constructing the method."
        ))
    }
    if (!accepts_observed && !is.null(observed_reference)) {
        cli::cli_abort(
            "The selected morphing method does not accept observed reference data."
        )
    }

    ShiftMorphMethod(
        name = recipe$name,
        recipe = recipe,
        reference = reference,
        observed_reference = observed_reference,
        requires_reference = requires_reference,
        requires_observed_reference = requires_observed
    )
}

#' @rdname shift_api
#' @param methods Optional named Belcher step method overrides.
#' @param profile Belcher compatibility profile, `"enhanced"` by default.
#' @param options For [belcher()], an optional named list created by
#'   [belcher_options()]. For [shift_request()], provider-specific request
#'   options; ESGF recognizes `index_node` and `time_filter_method`.
#' @export
belcher <- function(reference = NULL, methods = NULL, profile = "enhanced",
                    options = NULL) {
    shift_morph_method(
        epw_morph_recipe(
            name = "belcher",
            methods = methods,
            profile = profile,
            options = options
        ),
        reference = reference
    )
}

#' Daily temperature projection method
#'
#' @description
#' `daily_temperature()` creates a future-EPW method from matching future and
#' historical daily CMIP temperature data. It requires `frequency = "day"`.
#' The configured workflow retains the selected registered recipe identity and
#' `"harmonized"` execution policy while preserving the public method name.
#'
#' Daily `tas` changes are estimated with a circular climatology on a common
#' 365-day phase grid. When paired `tasmin` and `tasmax` are available for both
#' periods, each baseline 24-hour profile is constrained to the requested daily
#' mean, minimum, and maximum while retaining its hourly ordering. Otherwise,
#' the daily mean change is applied additively and the baseline daily range is
#' inherited.
#'
#' `reconstruction = "power"` uses epwshiftr's monotone power projection.
#' `"btws"` instead selects the bounded temperature weighted stretch published
#' by Eames et al. (2024). The latter remains a composite comparison: only its
#' hourly reconstruction comes from Eames, while the daily CMIP6 signal,
#' calendar mapping, humidity closure, and output policy remain epwshiftr
#' components.
#'
#' EPW fields outside dry-bulb temperature and its coupled humidity state remain
#' unchanged. After dry-bulb temperature is projected, baseline specific
#' humidity is retained and relative humidity and dew point are recomputed;
#' moisture is clipped only when necessary to avoid supersaturation.
#'
#' @param reference A required [historical_reference()],
#'   [shift_reference_plan()], or extracted `ShiftClimate` stage.
#' @param window_days Odd circular climatology-window width in days.
#' @param reconstruction Hourly temperature reconstruction component:
#'   `"power"` uses the default constrained power projection; `"btws"` uses
#'   bounded temperature weighted stretch and requires paired daily
#'   `tasmin` and `tasmax`.
#'
#' @return A complete `ShiftMorphMethod` for [shift_future_epw()].
#'
#' @references
#' Eames, M. E., Ramallo-González, A. P., and Wood, M. J. (2024).
#' A revised morphing algorithm for creating future weather for building
#' performance evaluation.
#' \doi{10.1177/01436244231218861}
#'
#' @seealso [shift_cmip6()], [shift_future_epw()]
#' @export
daily_temperature <- function(
    reference = NULL,
    window_days = 31L,
    reconstruction = c("power", "btws")
) {
    reconstruction <- match.arg(reconstruction)
    spec <- switch(
        reconstruction,
        power = "epwshiftr_daily_power",
        btws = "epwshiftr_daily_btws"
    )
    shift_morph_method(
        epw_morph_recipe(
            name = "daily_temperature",
            options = list(window_days = window_days),
            policy = "harmonized",
            spec = spec
        ),
        reference = reference
    )
}

#' Eames monthly temperature method
#'
#' @description
#' `eames_temperature()` creates a temperature-only future-EPW method using the
#' monthly temperature signal and bounded temperature weighted stretch (BTWS)
#' described by Eames et al. (2024). Matching historical and future daily
#' `tas`, `tasmin`, and `tasmax` inputs are required.
#'
#' The daily CMIP6 inputs are aggregated into 12 calendar-month values for mean
#' temperature, average daily minimum temperature, and average daily maximum
#' temperature. One future-minus-historical set is applied to every baseline
#' day in that EPW month before BTWS reconstructs the hourly profile. The method
#' therefore does not use daily-varying change factors.
#'
#' The published method used monthly UKCP18 factors. This implementation adapts
#' its temperature calculation to monthly statistics derived from daily CMIP6
#' data. It retains epwshiftr's specific-humidity closure and EPW output policy,
#' and does not implement the paper's non-temperature transformations.
#'
#' @param reference A required [historical_reference()],
#'   [shift_reference_plan()], or extracted `ShiftClimate` stage.
#'
#' @return A complete `ShiftMorphMethod` for [shift_future_epw()].
#'
#' @references
#' Eames, M. E., Ramallo-González, A. P., and Wood, M. J. (2024).
#' A revised morphing algorithm for creating future weather for building
#' performance evaluation.
#' \doi{10.1177/01436244231218861}
#'
#' @seealso [daily_temperature()], [shift_cmip6()], [shift_future_epw()]
#' @export
eames_temperature <- function(reference = NULL) {
    shift_morph_method(
        epw_morph_recipe(
            name = "eames_monthly_temperature",
            policy = "harmonized"
        ),
        reference = reference
    )
}

#' Ek daily temperature method
#'
#' @description
#' `ek_daily_temperature()` creates the temperature-focused daily change-factor
#' workflow described by Ek et al. (2018). Matching historical and future daily
#' `tasmin` and `tasmax` inputs are required.
#'
#' The method constructs one climate baseline for each day of the annual cycle,
#' derives daily mean temperature and DTR from the paired extrema, and applies
#' the Ek combined shift-and-stretch equation to every baseline EPW hour. It
#' does not apply an undocumented smoothing window and it preserves the
#' baseline day order and within-day timing.
#'
#' The paper does not fully reconcile its generic stretch-factor equation,
#' combined temperature equation, and variance description. This implementation
#' uses relative DTR change as the anomaly multiplier because that interpretation
#' gives zero-change identity and the stated daily mean and variance behavior.
#' The selected equation and calendar adaptation are retained in result
#' provenance.
#'
#' The `"paper_faithful"` policy changes dry-bulb temperature while preserving
#' the baseline humidity fields. The `"harmonized"` policy instead preserves
#' feasible baseline specific humidity and recomputes relative humidity and dew
#' point against the projected temperature.
#'
#' @param reference A required [historical_reference()],
#'   [shift_reference_plan()], or extracted `ShiftClimate` stage.
#' @param policy Physical execution policy: `"paper_faithful"` preserves the
#'   baseline humidity fields; `"harmonized"` applies shared specific-humidity
#'   closure.
#'
#' @return A complete `ShiftMorphMethod` for [shift_future_epw()].
#'
#' @references
#' Ek, M., Murdock, T. Q., Sobie, S. R., Cavka, B., Coughlin, B., and
#' Wells, R. (2018). Future weather files to support climate resilient
#' building design in Vancouver.
#' \url{https://hdl.handle.net/1828/21874}
#'
#' @seealso [daily_temperature()], [sobie_curry_daily()], [shift_cmip6()],
#'   [shift_future_epw()]
#' @export
ek_daily_temperature <- function(
    reference = NULL,
    policy = c("paper_faithful", "harmonized")
) {
    policy <- match.arg(policy)
    shift_morph_method(
        epw_morph_recipe(
            name = "ek_daily_factors",
            policy = policy
        ),
        reference = reference
    )
}

#' Arima month-wise quantile-mapping temperature method
#'
#' @description
#' `arima_temperature()` creates the temperature-focused future-weather method
#' described by Arima et al. (2024). It requires a baseline EPW, matching
#' historical and future daily model `tas`, and multi-year observed daily `tas`
#' for the target location.
#'
#' For each calendar month, the method calculates historical and future model
#' inverse CDFs and subtracts values at common percentiles. The resulting
#' change function is smoothed with the published endpoint-aware nine-point
#' moving mean repeated three times. Each baseline EPW daily mean is located in
#' the observed monthly empirical CDF, and the corresponding additive factor is
#' applied to all 24 hours of that baseline day.
#'
#' The publications do not specify empirical plotting positions, quantile
#' interpolation, or endpoint evaluation. This implementation records its
#' deterministic midpoint probability grid, type-7 quantiles, linear factor
#' interpolation, and endpoint clamping in result provenance.
#'
#' The `"paper_faithful"` policy changes dry-bulb temperature while preserving
#' baseline humidity fields. The `"harmonized"` policy instead retains feasible
#' baseline specific humidity and recomputes relative humidity and dew point.
#'
#' @param reference A required [historical_reference()],
#'   [shift_reference_plan()], or extracted `ShiftClimate` stage containing
#'   historical daily model output.
#' @param observed_reference A required [shift_reference_plan()] or extracted
#'   `ShiftClimate` stage containing multi-year observed daily weather.
#' @param policy Physical execution policy: `"paper_faithful"` preserves
#'   baseline humidity fields; `"harmonized"` applies shared specific-humidity
#'   closure.
#'
#' @return A complete `ShiftMorphMethod` for [shift_future_epw()].
#'
#' @references
#' Arima, Y., Ozaki, A., Kuma, Y., Iseda, H., and Abe, G. (2024).
#' Development of Future Weather Data Using the Quantile Mapping Technique and
#' its Application in Japan. \doi{10.69357/asim2024.1178}
#'
#' @seealso [ek_daily_temperature()], [sobie_curry_daily()], [shift_cmip6()],
#'   [shift_future_epw()]
#' @export
arima_temperature <- function(
    reference = NULL,
    observed_reference = NULL,
    policy = c("paper_faithful", "harmonized")
) {
    policy <- match.arg(policy)
    shift_morph_method(
        epw_morph_recipe(
            name = "monthly_percentile_temperature",
            policy = policy
        ),
        reference = reference,
        observed_reference = observed_reference
    )
}

#' Sobie-Curry daily morphing method
#'
#' @description
#' `sobie_curry_daily()` creates the daily method described by Sobie and Curry
#' (2025). It requires matching historical and future daily `tas`, `tasmin`,
#' `tasmax`, `huss`, and `ps` inputs.
#'
#' The method estimates calendar-neutral change factors with the published
#' 21-day circular window, preserves the baseline CWEC/EPW sequence, and
#' transforms dry-bulb temperature and surface pressure. The default
#' `"paper_faithful"` policy independently transforms dew point and relative
#' humidity as published. The `"harmonized"` policy instead applies the
#' smoothed daily specific-humidity change and derives a physically closed
#' humidity state from projected temperature and pressure. Other hourly EPW
#' fields remain unchanged.
#'
#' @param reference A required [historical_reference()],
#'   [shift_reference_plan()], or extracted `ShiftClimate` stage.
#' @param window_days Odd circular climatology-window width in days. The
#'   published setting is `21`.
#' @param policy Physical execution policy: `"paper_faithful"` reproduces the
#'   published independent thermodynamic transformations; `"harmonized"` uses
#'   epwshiftr's specific-humidity closure.
#'
#' @return A complete `ShiftMorphMethod` for [shift_future_epw()].
#'
#' @seealso [daily_temperature()], [shift_cmip6()], [shift_future_epw()]
#' @export
sobie_curry_daily <- function(
    reference = NULL,
    window_days = 21L,
    policy = c("paper_faithful", "harmonized")
) {
    policy <- match.arg(policy)
    shift_morph_method(
        epw_morph_recipe(
            name = "sobie_curry_daily",
            options = list(window_days = window_days),
            policy = policy
        ),
        reference = reference
    )
}

#' @rdname shift_api
#' @param model CMIP6 source/model IDs.
#' @param scenarios CMIP6 future scenario experiment IDs.
#' @param member Optional CMIP6 variant labels. `NULL` asks the task workflow to
#'   choose one complete member.
#' @param grid Optional single CMIP6 grid label.
#' @param table Optional CMIP6 table selection. `NULL` automatically maps each
#'   recipe input to its native table (including `snd` to `LImon`); an unnamed
#'   scalar pins every variable to one table; a named character vector
#'   overrides individual variables.
#' @param activity CMIP6 activity ID.
#' @param index_nodes Ordered ESGF index nodes used for failover.
#' @param data_node Optional ESGF data-node filter.
#' @export
shift_cmip6 <- function(model, scenarios, member = NULL, grid = NULL,
                        frequency = "mon", table = NULL,
                        activity = "ScenarioMIP", index_nodes = NULL,
                        data_node = NULL, filters = list()) {
    checkmate::assert_character(model, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_character(scenarios, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_character(member, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    checkmate::assert_string(grid, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(frequency, min.chars = 1L)
    table <- shift__cmip6_table_spec(table)
    checkmate::assert_string(activity, min.chars = 1L)
    checkmate::assert_character(index_nodes, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")

    if (is.null(index_nodes)) {
        index_nodes <- unname(INDEX_NODES[c("DKRZ", "CEDA", "ORNL", "LLNL", "NCI", "IPSL", "LIU")])
    }
    # Normalize before de-duplication because the legacy LLNL endpoint resolves
    # to the same operational ORNL bridge and must not create a second attempt.
    index_nodes <- unique(vapply(index_nodes, query__normalize_node, character(1L)))
    ShiftCmip6Spec(
        model = model,
        scenarios = scenarios,
        member = member,
        grid = grid,
        frequency = frequency,
        table = table,
        activity = activity,
        index_nodes = index_nodes,
        data_node = data_node,
        filters = filters
    )
}

# Translate one complete CMIP6 climate specification into the lower-level
# request consumed by the staged workflow and ESGF collector.
shift__request_from_cmip6 <- function(climate, periods, method) {
    variables <- morpher__input_variables(method@recipe)
    tables <- shift__cmip6_variable_tables(
        variables, climate@frequency, climate@table
    )
    shift_cmip6_scenario(
        source = climate@model,
        scenario = climate@scenarios,
        member = climate@member,
        years = periods$year,
        variables = variables,
        frequency = climate@frequency,
        activity = climate@activity,
        table_id = unique(unname(tables)),
        grid_label = climate@grid,
        data_node = climate@data_node,
        index_node = climate@index_nodes[[1L]],
        filters = climate@filters,
        # Dataset metadata constrains the remote search, while File metadata is
        # completed from DRS filenames before records enter the store.
        options = list(time_filter_method = "auto")
    )
}

#' @rdname shift_api
#' @param allow_partial Whether a task-level run may complete with missing cases.
#' @param download Source-data policy in [shift_control()] (`"auto"`,
#'   `"always"`, or `"never"`), or a named download-stage option list in
#'   [shift_plan()].
#' @param extraction_method Grid extraction method.
#' @param output_layout Output directory layout.
#' @export
shift_control <- function(strict = TRUE, allow_partial = FALSE,
                          download = c("auto", "always", "never"),
                          resume = TRUE, overwrite = FALSE,
                          extraction_method = "nearest",
                          output_layout = c("nested", "flat")) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(allow_partial)
    download <- match.arg(download)
    checkmate::assert_flag(resume)
    checkmate::assert_flag(overwrite)
    extraction_method <- match.arg(extraction_method, ESG_GRID_METHOD_CHOICES)
    output_layout <- match.arg(output_layout)

    ShiftControl(
        strict = strict,
        allow_partial = allow_partial,
        download = download,
        resume = resume,
        overwrite = overwrite,
        extraction_method = extraction_method,
        output_layout = output_layout
    )
}

#' @rdname shift_api
#' @param scenario CMIP6 scenario experiment IDs, for example
#'   `"ssp126"` or `"ssp585"`.
#' @param member CMIP6 variant label, for example `"r1i1p1f1"`.
#' @param years Optional years used to constrain the future request time window.
#' @param activity CMIP6 activity ID. `shift_cmip6_scenario()` defaults to
#'   `"ScenarioMIP"` and `shift_reference_historical()` defaults to `"CMIP"`.
#' @param table_id One or more CMIP6 table IDs. If `NULL`, a common atmospheric
#'   table is inferred from `frequency`.
#' @param grid_label Optional CMIP6 grid label.
#' @param data_node Optional ESGF data node filter.
#' @param index_node Optional ESGF index node.
#' @export
shift_cmip6_scenario <- function(source, scenario, member = NULL,
                                 years = NULL, variables = "recommended", frequency = "mon",
                                 activity = "ScenarioMIP", table_id = NULL, grid_label = NULL,
                                 data_node = NULL, index_node = NULL, filters = list(), options = list()) {
    checkmate::assert_character(source, any.missing = FALSE, min.len = 1L)
    checkmate::assert_character(scenario, any.missing = FALSE, min.len = 1L)
    checkmate::assert_character(member, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(frequency, any.missing = FALSE, min.len = 1L)
    checkmate::assert_string(activity, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_character(table_id, any.missing = FALSE, min.len = 1L,
        unique = TRUE, null.ok = TRUE)
    checkmate::assert_string(grid_label, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(index_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")

    time <- if (is.null(years)) NULL else shift_time_window(range(shift__years_value(years)))
    table_id <- shift_coalesce(table_id, shift__cmip6_table_id(frequency))
    defaults <- shift__compact_list(list(
        activity_id = activity,
        table_id = table_id,
        grid_label = grid_label,
        data_node = data_node
    ))
    options <- utils::modifyList(shift__compact_list(list(index_node = index_node)), options)

    shift_request(
        provider = "esgf",
        project = "CMIP6",
        source = source,
        experiment = scenario,
        variant = member,
        variables = shift__variables_value(variables),
        frequency = frequency,
        time = time,
        filters = utils::modifyList(defaults, filters),
        options = options
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
#' @param period Reference period name used when constructing periods from
#'   `years`.
#' @export
historical_reference <- function(years = 1995:2014, period = "reference", ...) {
    shift_reference_historical(shift__periods_from_years(years, period = period, arg = "years"), ...)
}

# Validate a method-specific frequency contract before a task writes store state
# or attempts remote CMIP6 discovery.
shift__validate_method_frequency <- function(method, frequency) {
    if (!S7::S7_inherits(method, ShiftMorphMethod)) {
        cli::cli_abort("`method` must be a complete {.cls ShiftMorphMethod}.")
    }
    required <- morpher__recipe_required_frequency(method@recipe)
    if (is.null(required)) {
        return(invisible(TRUE))
    }
    actual <- unique(tolower(as.character(frequency)))
    actual <- actual[!is.na(actual) & nzchar(actual)]
    if (!identical(actual, required)) {
        shown <- if (length(actual)) actual else "<missing>"
        cli::cli_abort(c(
            "Morphing method {.val {method@name}} requires CMIP frequency {.val {required}}.",
            "x" = "The climate request uses {.val {shown}}.",
            "i" = "Set {.code frequency = \"{required}\"} in the climate specification."
        ))
    }
    invisible(TRUE)
}

#' @rdname shift_api
#' @param request A [shift_request()] object, commonly from
#'   `shift_cmip6_scenario()`.
#' @param morph Named morph-stage options. Stage option lists are validated and
#'   cannot override task-level controls or the method recipe/reference.
#' @export
shift_plan <- function(request, site, periods, store, method,
                       control = shift_control(), collect = list(),
                       download = list(), extract = list(), morph = list(),
                       epw = list()) {
    if (!S7::S7_inherits(request, ShiftRequest)) {
        cli::cli_abort("`request` must be a {.cls ShiftRequest}, usually from {.fn shift_request} or {.fn shift_cmip6_scenario}.")
    }
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("`site` must be a {.cls ShiftSite}.")
    }
    if (!S7::S7_inherits(method, ShiftMorphMethod)) {
        cli::cli_abort("`method` must be a complete {.cls ShiftMorphMethod}, for example {.code belcher()}.")
    }
    if (!S7::S7_inherits(control, ShiftControl)) {
        cli::cli_abort("`control` must be created by {.fn shift_control}.")
    }
    shift__validate_method_frequency(method, request@meta$frequency)
    periods <- shift__periods_from_input(periods)
    store_path <- shift__store_path_value(store, create = FALSE)
    if (shift_is_epw_object(site@epw)) {
        # Object-backed inputs may originate from unsaved external state or a
        # temporary conversion. Persist their exact snapshot before the run is
        # registered so cross-session resume never depends on tempdir().
        site@epw <- epw_file_coerce(
            site@epw,
            dir = file.path(store_path, "sources", "epw-input")
        )
    }
    collect <- shift__validate_stage_options(collect, "collect", c("fields", "all", "limit", "label"))
    download <- shift__validate_stage_options(
        download,
        "download",
        c("downloader", "background", "session_label", "replica", "service", "probe",
          "probe_concurrency", "probe_cache_seconds", "strategy", "mode")
    )
    extract <- shift__validate_stage_options(extract, "extract", c("variables", "time", "filters", "fallback"))
    morph <- shift__validate_stage_options(morph, "morph", "by")
    epw <- shift__validate_stage_options(epw, "epw", c("dir", "separate", "export_dir"))

    shift_stage_new(
        ShiftPlan,
        "plan",
        store_path = store_path,
        meta = list(
            request = request,
            site = site,
            periods = periods,
            method = method,
            control = control,
            collect = collect,
            download = download,
            extract = extract,
            morph = morph,
            epw = epw,
            expected_cases = shift__expected_cases(request, periods)
        )
    )
}

#' @rdname shift_api
#' @param climate A complete future-climate specification from [shift_cmip6()].
#' @param method A complete [shift_morph_method()] object. Methods that require
#'   reference climate data must contain it when they are constructed.
#' @param dir User-facing directory that receives only exported EPW files.
#' @param control Workflow controls from [shift_control()].
#' @param dry_run If `TRUE`, return the planned workflow without running it.
#' @param ui Runtime presentation options from [shift_ui()]. These options are
#'   excluded from persisted scientific intent and `spec_hash`.
#' @export
shift_future_epw <- function(epw, climate, periods, method, dir,
                             control = shift_control(), ui = shift_ui(),
                             store = NULL, dry_run = FALSE,
                             background = FALSE) {
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(dry_run)
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(method, ShiftMorphMethod)) {
        cli::cli_abort("`method` must be a complete {.cls ShiftMorphMethod}; method names such as {.val belcher} are not accepted.")
    }
    if (!S7::S7_inherits(climate, ShiftCmip6Spec)) {
        cli::cli_abort("`climate` must be a complete {.cls ShiftCmip6Spec} created by {.fn shift_cmip6}.")
    }
    if (!S7::S7_inherits(control, ShiftControl)) {
        cli::cli_abort("`control` must be created by {.fn shift_control}.")
    }
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (isTRUE(dry_run) && isTRUE(background)) {
        cli::cli_abort("`dry_run = TRUE` cannot be combined with `background = TRUE`.")
    }

    periods <- shift__periods_from_input(periods)
    store <- shift_coalesce(store, store_dir(init = FALSE))
    output_dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
    # Keep delivery files outside the persistent workflow store. Besides
    # preserving the public directory contract, this prevents export cleanup
    # and store lifecycle operations from ever sharing a directory tree.
    shift__validate_delivery_store_paths(output_dir, store)
    request <- shift__request_from_cmip6(climate, periods, method)
    site <- shift_site(epw = epw)

    plan <- shift_plan(
        request = request,
        site = site,
        periods = periods,
        store = store,
        method = method,
        control = control,
        epw = list(
            dir = "outputs/future-epw",
            separate = identical(control@output_layout, "nested"),
            export_dir = output_dir
        )
    )
    # Store task intent and immutable EPW identity on the plan so persisted
    # runs can be explained and resumed without reinterpreting call arguments.
    plan@meta$climate <- climate
    plan@meta$epw_identity <- shift__epw_identity(plan@meta$site@epw)
    if (isTRUE(dry_run)) {
        return(plan)
    }
    shift_run(plan, background = background, ui = ui)
}

#' @rdname shift_api
#' @param match File metadata fields copied from the future climate stage when
#'   resolving an automatic historical reference.
#' @param collect Named collection options. Historical reference collection may
#'   use `fields`, `all`, `limit`, `label`, and `time`; [shift_plan()] applies
#'   the same strict field validation to its collection stage.
#' @param extract Named extraction options. Historical reference extraction may
#'   use `variables`, `time`, `filters`, `method`, and `fallback`;
#'   [shift_plan()] applies the same strict field validation to its extraction
#'   stage.
#' @export
shift_reference_historical <- function(periods, experiment = "historical", activity = "CMIP",
                                       match = c("source_id", "variant_label", "frequency", "table_id", "grid_label"),
                                       filters = list(), options = list(),
                                       collect = list(), extract = list(fallback = "auto")) {
    periods <- shift_reference_periods(periods)
    checkmate::assert_string(experiment, min.chars = 1L)
    checkmate::assert_string(activity, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_character(match, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")
    checkmate::assert_list(collect, names = "unique")
    checkmate::assert_subset(names(collect), c("fields", "all", "limit", "label", "time"))
    checkmate::assert_list(extract, names = "unique")
    checkmate::assert_subset(names(extract), c("variables", "time", "filters", "method", "fallback"))

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
#' @param all,limit Collection controls passed to [EsgQuery] /
#'   [EsgResultDataset]. If a numeric `limit` is supplied without explicitly
#'   setting `all`, it caps the Dataset result count. With `all = TRUE`, a
#'   numeric `limit` retains the low-level meaning of pagination page size.
#' @param label Optional label recorded with collected File records.
#' @export
shift_collect <- S7::new_generic(
    "shift_collect",
    "x",
    function(x, store = NULL, fields = "*", all = TRUE, limit = FALSE,
             label = NULL, ui = NULL, ...) {
        # At the task API, a bare numeric limit means a user-facing result cap.
        # Callers that need the low-level ESGF page-size meaning can request it
        # explicitly with `all = TRUE, limit = n`.
        if (missing(all) && is.numeric(limit) && length(limit) == 1L &&
            !is.na(limit) && is.finite(limit)) {
            all <- FALSE
        }
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            options <- list(...)
            return(shift__task_execute(
                "collect", x, store = store, ui = ui,
                spec = list(fields = fields, all = all, limit = limit,
                    label = label, options = options),
                code = function(reporter, task_store) {
                    shift__with_reporter(reporter, do.call(shift_collect,
                        c(list(x = x, store = task_store, fields = fields,
                            all = all, limit = limit, label = label),
                        options)))
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param downloader Optional [Downloader] instance.
#' @param run Whether to run queued downloads immediately. Downloading full
#'   NetCDF files is optional for the normal workflow because [shift_extract()]
#'   can use OPeNDAP first and only download as a fallback when requested.
#' @param background For [shift_download()], whether to run queued downloads in
#'   a background job. For task-level run/resume functions, whether to launch a
#'   detached `Rscript` worker and return a queued `ShiftRun` immediately.
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
             resume = TRUE, overwrite = FALSE, session_label = NULL,
             ui = NULL, ...) {
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            options <- list(...)
            reconstructible <- is.null(downloader)
            return(shift__task_execute(
                "download", x, ui = ui,
                spec = list(run = run, background = background,
                    resume = resume, overwrite = overwrite,
                    session_label = session_label, options = options),
                resumable = reconstructible,
                nonresumable_reason = if (reconstructible) NULL else
                    "A session-local Downloader instance cannot be reconstructed.",
                code = function(reporter, task_store) {
                    shift__with_reporter(reporter, do.call(shift_download,
                        c(list(x = x, downloader = downloader, run = run,
                            background = background, resume = resume,
                            overwrite = overwrite,
                            session_label = session_label), options)))
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param site A `shift_site()` object.
#' @param periods A period table, usually from [epw_morph_periods()].
#' @param method In task-level planning, a complete [shift_morph_method()]
#'   object. In [shift_extract()], the grid extraction method.
#' @param fallback Extraction fallback policy.
#' @export
shift_extract <- S7::new_generic(
    "shift_extract",
    "x",
    function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
             filters = list(), method = "nearest", fallback = c("auto", "error"),
             overwrite = FALSE, resume = TRUE, ui = NULL) {
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            return(shift__task_execute(
                "extract", x, ui = ui,
                spec = list(site = shift__site_ref(site),
                    periods = if (is.null(periods)) NULL else
                        split(as.integer(periods$year), periods$period),
                    variables = variables, time = time, filters = filters,
                    method = method, fallback = fallback,
                    overwrite = overwrite, resume = resume),
                code = function(reporter, task_store) {
                    shift__with_reporter(reporter,
                        shift_extract(x, site = site, periods = periods,
                            variables = variables, time = time,
                            filters = filters, method = method,
                            fallback = fallback, overwrite = overwrite,
                            resume = resume))
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param baseline Optional baseline EPW path or
#'   `shift_site()` object containing `epw`.
#' @param recipe Morphing recipe, usually from [epw_morph_recipe()].
#' @param reference Optional `ShiftReferenceSpec` or `ShiftClimate` stage for
#'   change-factor morphing.
#' @param reference_plan_id,reference_periods Optional store plan IDs and period
#'   table for reference climate data.
#' @param observed_reference Optional plan-backed `ShiftReferenceSpec` or
#'   `ShiftClimate` stage containing multi-year observed daily weather.
#' @param observed_plan_id,observed_periods Optional store plan IDs and period
#'   table for observed daily weather.
#' @param complete_only Whether [shift_morph()] should morph only complete
#'   extraction plans when a climate stage also contains failed or incomplete
#'   plans.
#' @param by Grouping columns used to create morphing cases.
#' @export
shift_morph <- S7::new_generic(
    "shift_morph",
    "x",
    function(x, baseline = NULL, recipe = epw_morph_recipe("belcher"),
             reference = NULL, reference_plan_id = NULL, reference_periods = NULL,
             observed_reference = NULL, observed_plan_id = NULL,
             observed_periods = NULL,
             strict = TRUE, complete_only = TRUE,
             by = c("source_id", "experiment_id", "variant_label", "period"),
             overwrite = FALSE, resume = TRUE, ui = NULL) {
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            baseline_path <- if (shift_is_epw_path(baseline)) baseline else NULL
            backend <- tryCatch(recipe$backend, error = function(e) NULL)
            reconstructible <- is.null(baseline) || !is.null(baseline_path)
            if (!is.null(backend) && !backend %in% names(morpher__default_backend_specs())) {
                reconstructible <- FALSE
            }
            return(shift__task_execute(
                "morph", x, ui = ui,
                spec = list(baseline = baseline_path,
                    recipe = shift__recipe_ref(recipe),
                    reference = shift__reference_spec_value(reference),
                    reference_plan_id = reference_plan_id,
                    reference_periods = if (is.null(reference_periods)) NULL else
                        split(as.integer(reference_periods$year), reference_periods$period),
                    observed_reference = shift__reference_spec_value(
                        observed_reference
                    ),
                    observed_plan_id = observed_plan_id,
                    observed_periods = if (is.null(observed_periods)) NULL else
                        split(
                            as.integer(observed_periods$year),
                            observed_periods$period
                        ),
                    strict = strict, complete_only = complete_only, by = by,
                    overwrite = overwrite, resume = resume),
                resumable = reconstructible,
                nonresumable_reason = if (reconstructible) NULL else
                    "The baseline or morph backend exists only in this R session.",
                code = function(reporter, task_store) {
                    shift__with_reporter(reporter,
                        shift_morph(x, baseline = baseline, recipe = recipe,
                            reference = reference,
                            reference_plan_id = reference_plan_id,
                            reference_periods = reference_periods,
                            observed_reference = observed_reference,
                            observed_plan_id = observed_plan_id,
                            observed_periods = observed_periods,
                            strict = strict, complete_only = complete_only,
                            by = by, overwrite = overwrite,
                            resume = resume))
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param dir In [shift_future_epw()], the user-facing delivery directory. In
#'   [shift_epw()], an output directory inside the store root; relative paths
#'   are resolved under the store root.
#' @param separate Whether to create separate output directories per morphing case.
#' @param export_dir Optional directory outside or inside the store where EPW
#'   files should also be copied for user-facing delivery.
#' @export
shift_epw <- S7::new_generic(
    "shift_epw",
    "x",
    function(x, dir = NULL, separate = TRUE, export_dir = NULL, overwrite = FALSE,
             resume = TRUE, ui = NULL) {
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            return(shift__task_execute(
                "write_epw", x, ui = ui,
                spec = list(dir = dir, separate = separate,
                    export_dir = export_dir, overwrite = overwrite,
                    resume = resume),
                auto_complete = !is.null(export_dir),
                code = function(reporter, task_store) {
                    shift__with_reporter(reporter,
                        shift_epw(x, dir = dir, separate = separate,
                            export_dir = export_dir, overwrite = overwrite,
                            resume = resume))
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @export
shift_explain <- function(x, ...) {
    shift_assert_stage(x)
    if (S7::S7_inherits(x, ShiftPlan)) {
        return(shift__plan_explain(x))
    }
    if (S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
        row <- x@meta$run
        out <- data.table::data.table(
            field = c("run_id", "status", "current_stage", "spec_hash", "output_dir", "last_error"),
            value = as.character(unlist(row[, c(
                "run_id", "status", "current_stage", "spec_hash", "output_dir", "last_error"
            ), with = FALSE], use.names = FALSE))
        )
        steps <- data.table::as.data.table(x@meta$steps)
        if (nrow(steps)) {
            out <- data.table::rbindlist(list(out, data.table::data.table(
                field = c("steps", "latest_step", "latest_task"),
                value = c(nrow(steps), steps$step_id[[nrow(steps)]],
                    steps$task[[nrow(steps)]])
            )), use.names = TRUE)
        }
        return(out)
    }
    cli::cli_abort("{.fn shift_explain} expects a {.cls ShiftPlan} or {.cls ShiftRun}.")
}

#' @rdname shift_api
#' @export
shift_run <- function(x, background = FALSE, ui = shift_ui(), ...) {
    shift_assert_stage(x)
    if (!S7::S7_inherits(x, ShiftPlan)) {
        cli::cli_abort("{.fn shift_run} currently expects a {.cls ShiftPlan}.")
    }
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (isTRUE(background)) {
        shift__validate_background_plan(x)
    }
    run_id <- shift__run_register(x)
    store <- shift_store(x, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    mode <- if (isTRUE(background)) "process" else "foreground"
    job <- shift__job_create(store, run_id, mode = mode, ui = ui)
    job_id <- job$job_id[[1L]]
    reporter <- shift__reporter(ui, store = store, run_id = run_id,
        job_id = job_id, background = background)
    reporter$run_started(x, run_id, background = background)

    if (isTRUE(background)) {
        # Materialize the queued handle before releasing DuckDB. Reopening the
        # manifest after launch would race the detached worker for DuckDB's
        # exclusive process lock and could make an otherwise valid job fail.
        store_path <- store$path
        log_path <- job$log_path[[1L]]
        handle <- shift__run_handle(store, run_id)
        store$close()
        shift__launch_job(store_path, run_id, job_id, log_path)
        return(handle)
    }

    shift__plan_run(x, run_id = run_id, job_id = job_id,
        reporter = reporter, ...)
}

#' @rdname shift_api
#' @export
shift_export_epw <- function(x, dir, separate = TRUE, overwrite = FALSE,
                             resume = TRUE, ui = NULL) {
    shift_assert_stage(x)
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    reporter <- shift__current_reporter()
    if (is.null(reporter)) {
        return(shift__task_execute(
            "export_epw", x, ui = ui,
            spec = list(dir = normalizePath(path.expand(dir), winslash = "/",
                mustWork = FALSE), separate = separate,
                overwrite = overwrite, resume = resume),
            auto_complete = TRUE,
            code = function(reporter, task_store) {
                shift__with_reporter(reporter,
                    shift_export_epw(x, dir = dir, separate = separate,
                        overwrite = overwrite, resume = resume))
            }
        ))
    }

    if (S7::S7_inherits(x, ShiftMorphed)) {
        x <- shift_epw(x, separate = separate, overwrite = overwrite,
            resume = resume)
    }
    if (!S7::S7_inherits(x, ShiftOutputs)) {
        cli::cli_abort("{.fn shift_export_epw} expects a {.cls ShiftOutputs} or {.cls ShiftMorphed} stage.")
    }

    shift__export_outputs(x, dir = dir, separate = separate,
        overwrite = overwrite, resume = resume, reporter = reporter)
}

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
    if (S7::S7_inherits(x, ShiftRun)) {
        return(shift_run_get(x@ids$run_id, store = x@store_path))
    }
    if (S7::S7_inherits(x, ShiftRequest) || S7::S7_inherits(x, ShiftSite)) {
        return(x)
    }
    x@diagnostics <- shift_diagnostics_empty()
    x@diagnostics <- shift_check(x, strict = FALSE)
    x
}

#' @rdname shift_api
#' @export
shift_ids <- function(x, refresh = TRUE) {
    shift_assert_stage(x)
    checkmate::assert_flag(refresh)
    if (isTRUE(refresh) && S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
    }
    x@ids
}

#' @rdname shift_api
#' @export
shift_cases <- function(x, refresh = TRUE) {
    shift_assert_stage(x)
    checkmate::assert_flag(refresh)
    if (S7::S7_inherits(x, ShiftPlan)) {
        return(data.table::as.data.table(data.table::copy(x@meta$expected_cases)))
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
    store_value <- shift_coalesce(store, store_dir(init = FALSE))
    store_path <- if (inherits(store_value, "EsgStore")) store_value$path else {
        normalizePath(path.expand(store_value), winslash = "/", mustWork = TRUE)
    }
    opened <- tryCatch(shift_store(store_value, create = FALSE), error = function(e) e)
    if (!inherits(opened, "error")) {
        if (!inherits(store_value, "EsgStore")) {
            on.exit(try(opened$close(), silent = TRUE), add = TRUE)
        }
        rows <- morpher__private_store(opened)$read_table("shift_run")
        return(rows[order(-started_at)])
    }
    if (!shift__manifest_locked(opened)) {
        stop(opened)
    }
    live <- list.files(file.path(store_path, "logs", "shift"),
        pattern = "[.]live[.]json$", full.names = TRUE)
    rows <- lapply(live, function(path) {
        value <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
            error = function(e) NULL)
        if (is.null(value)) NULL else shift__live_table(value$run)
    })
    rows <- Filter(function(x) !is.null(x) && nrow(x), rows)
    if (!length(rows)) {
        stop(opened)
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)[order(-started_at)]
}

#' @rdname shift_api
#' @param run_id Persisted workflow run ID.
#' @export
shift_run_get <- function(run_id, store = NULL) {
    if (inherits(run_id, "EsgResultDataset")) {
        result <- run_id
        value <- attr(result, "epwshiftr.run_id", exact = TRUE)
        if (is.null(value) || !length(value) || is.na(value[[1L]]) ||
            !nzchar(value[[1L]])) {
            cli::cli_abort(
                "This Dataset result is not associated with a persisted shift run.")
        }
        store <- shift_coalesce(store,
            attr(result, "epwshiftr.store", exact = TRUE))
        run_id <- as.character(value[[1L]])
    }
    if (S7::S7_inherits(run_id, ShiftStage)) {
        stage <- run_id
        value <- stage@ids$run_id
        if (is.null(value) || !length(value) || is.na(value[[1L]]) ||
            !nzchar(value[[1L]])) {
            cli::cli_abort("This shift stage is not associated with a persisted run.")
        }
        store <- shift_coalesce(store, stage@store_path)
        run_id <- as.character(value[[1L]])
    }
    checkmate::assert_string(run_id, min.chars = 1L)
    store_value <- shift_coalesce(store, store_dir(init = FALSE))
    store_path <- if (inherits(store_value, "EsgStore")) store_value$path else {
        normalizePath(path.expand(store_value), winslash = "/", mustWork = TRUE)
    }
    if (!inherits(store_value, "EsgStore")) {
        live <- shift__live_run_get(run_id, store_path)
        if (shift__live_process_is_active(live)) {
            # Active process jobs publish authoritative live state. Avoiding a
            # speculative DuckDB read here also prevents status/watch calls
            # from racing a newly launched worker for the manifest lock.
            return(live)
        }
    }
    opened <- tryCatch(shift_store(store_value, create = FALSE), error = function(e) e)
    if (inherits(opened, "error")) {
        if (!shift__manifest_locked(opened)) {
            stop(opened)
        }
        live <- shift__live_run_get(run_id, store_path)
        if (!is.null(live)) {
            return(live)
        }
        stop(opened)
    }
    if (!inherits(store_value, "EsgStore")) {
        on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    }
    shift__reconcile_background_download(opened, run_id)
    shift__reconcile_run_job(opened, run_id)
    shift__run_handle(opened, run_id)
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
            return(shift__datasets_attach_run(
                shift__datasets_result(stage), stage))
        }
        return(stage)
    }
    opened <- shift_store(run)
    on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    step <- shift__latest_step(opened, run@ids$run_id, completed = TRUE)
    if (!nrow(step)) {
        cli::cli_abort("Shift run {.val {run@ids$run_id}} has no completed stage result.")
    }
    ref <- jsonlite::fromJSON(step$output_stage_json[[1L]],
        simplifyVector = FALSE)
    stage <- shift__stage_from_ref(ref)
    if (S7::S7_inherits(stage, ShiftDatasets)) {
        return(shift__datasets_attach_run(
            shift__datasets_result(stage), stage))
    }
    stage
}

# Rebuild one failed, cancelled, or partial standalone step from its immutable
# input and scientific spec. UI choices are supplied by the new attempt and are
# intentionally absent from the persisted step hash.
shift__resume_generic_task <- function(run, step, ui, background = FALSE) {
    if (!isTRUE(step$resumable[[1L]])) {
        reason <- store__chr1(step$nonresumable_reason[[1L]])
        cli::cli_abort(c(
            "Shift step {.val {step$step_id[[1L]]}} cannot be resumed across sessions.",
            "x" = if (is.na(reason)) "Its original input is session-local." else reason
        ))
    }
    if (is.na(step$input_stage_json[[1L]]) ||
        !nzchar(step$input_stage_json[[1L]])) {
        cli::cli_abort("Shift step {.val {step$step_id[[1L]]}} has no reconstructible input stage.")
    }
    input_ref <- jsonlite::fromJSON(step$input_stage_json[[1L]],
        simplifyVector = FALSE)
    input <- shift__stage_from_ref(input_ref)
    spec <- jsonlite::fromJSON(step$spec_json[[1L]], simplifyVector = TRUE)
    task <- as.character(step$task[[1L]])
    if (isTRUE(background) && !identical(task, "download")) {
        cli::cli_abort("Background resume is currently supported only for standalone download steps.")
    }

    call <- switch(task,
        datasets = list(
            what = shift_datasets,
            args = list(input, store = run@store_path,
                all = isTRUE(spec$all), limit = spec$limit, ui = ui)
        ),
        collect = list(
            what = shift_collect,
            args = c(list(input, store = run@store_path,
                fields = as.character(spec$fields),
                all = isTRUE(spec$all), limit = spec$limit,
                label = store__chr1(spec$label), ui = ui),
                shift_coalesce(spec$options, list()))
        ),
        download = list(
            what = shift_download,
            args = c(list(input, run = isTRUE(spec$run),
                background = isTRUE(background) || isTRUE(spec$background),
                resume = TRUE, overwrite = isTRUE(spec$overwrite),
                session_label = store__chr1(spec$session_label), ui = ui),
                shift_coalesce(spec$options, list()))
        ),
        extract = list(
            what = shift_extract,
            args = list(input,
                site = shift__site_from_ref(spec$site),
                periods = shift__periods_from_input(spec$periods),
                variables = if (is.null(spec$variables)) NULL else
                    as.character(spec$variables),
                time = spec$time,
                filters = shift_coalesce(spec$filters, list()),
                method = as.character(spec$method),
                fallback = as.character(spec$fallback),
                overwrite = isTRUE(spec$overwrite), resume = TRUE, ui = ui)
        ),
        morph = list(
            what = shift_morph,
            args = list(input,
                baseline = if (is.null(spec$baseline)) NULL else
                    as.character(spec$baseline),
                recipe = shift__recipe_from_ref(spec$recipe),
                reference = shift__reference_from_spec(spec$reference),
                reference_plan_id = if (is.null(spec$reference_plan_id)) NULL else
                    as.character(spec$reference_plan_id),
                reference_periods = if (is.null(spec$reference_periods)) NULL else
                    shift__periods_from_input(spec$reference_periods),
                observed_reference = shift__reference_from_spec(
                    spec$observed_reference
                ),
                observed_plan_id = if (is.null(spec$observed_plan_id)) NULL else
                    as.character(spec$observed_plan_id),
                observed_periods = if (is.null(spec$observed_periods)) NULL else
                    shift__periods_from_input(spec$observed_periods),
                strict = isTRUE(spec$strict),
                complete_only = isTRUE(spec$complete_only),
                by = as.character(spec$by),
                overwrite = isTRUE(spec$overwrite), resume = TRUE, ui = ui)
        ),
        write_epw = list(
            what = shift_epw,
            args = list(input, dir = store__chr1(spec$dir),
                separate = isTRUE(spec$separate),
                export_dir = store__chr1(spec$export_dir),
                overwrite = isTRUE(spec$overwrite), resume = TRUE, ui = ui)
        ),
        export_epw = list(
            what = shift_export_epw,
            args = list(input, dir = as.character(spec$dir),
                separate = isTRUE(spec$separate),
                overwrite = isTRUE(spec$overwrite), resume = TRUE, ui = ui)
        ),
        cli::cli_abort("Unsupported standalone shift task: {.val {task}}.")
    )
    # Remove JSON nulls restored as NA scalar strings before public validation.
    call$args <- lapply(call$args, function(value) {
        if (is.character(value) && length(value) == 1L && is.na(value)) NULL else value
    })
    shift__with_run_override(run@ids$run_id,
        do.call(call$what, call$args))
}

#' @rdname shift_api
#' @export
shift_resume <- function(x, store = NULL, background = FALSE, ui = shift_ui()) {
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    run <- if (S7::S7_inherits(x, ShiftRun)) {
        shift_refresh(x)
    } else if (S7::S7_inherits(x, ShiftStage)) {
        shift_run_get(x, store = store)
    } else {
        checkmate::assert_string(x, min.chars = 1L)
        shift_run_get(x, store = store)
    }
    status <- shift_status(run, refresh = FALSE)
    if (status %in% "completed") {
        return(run)
    }
    if (status %in% c("queued", "running", "stopping")) {
        cli::cli_abort("Shift run {.val {run@ids$run_id}} is already active with status {.val {status}}.")
    }
    row <- run@meta$run
    task <- as.character(row$task[[1L]])
    if (identical(status, "waiting")) {
        cli::cli_abort(c(
            "Shift run {.val {run@ids$run_id}} is waiting for its next stage, not interrupted.",
            "i" = "Pass the latest stage object to the next {.fn shift_*} function, or call {.fn shift_complete}."
        ))
    }
    if (!identical(task, "future_epw")) {
        run_store <- shift_store(run)
        on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
        step <- shift__latest_step(run_store, run@ids$run_id)
        if (!nrow(step)) {
            cli::cli_abort("Shift run {.val {run@ids$run_id}} has no resumable step.")
        }
        if (!isTRUE(step$resumable[[1L]])) {
            reason <- store__chr1(step$nonresumable_reason[[1L]])
            cli::cli_abort(c(
                "Shift step {.val {step$step_id[[1L]]}} cannot be resumed across sessions.",
                "x" = if (is.na(reason)) {
                    "Its original input is session-local."
                } else {
                    reason
                }
            ))
        }
        if (is.na(step$input_stage_json[[1L]]) ||
            !nzchar(step$input_stage_json[[1L]])) {
            cli::cli_abort("Shift step {.val {step$step_id[[1L]]}} has no reconstructible input stage.")
        }
        shift__run_update(run_store, run@ids$run_id,
            status = "waiting", current_stage = step$task[[1L]],
            completed_at = as.POSIXct(NA, tz = "UTC"),
            last_error = NA_character_)
        shift__run_event(run_store, run@ids$run_id, "resume", "waiting",
            sprintf("Resume requested for %s.", step$task[[1L]]),
            details = list(step_id = step$step_id[[1L]]),
            step_id = step$step_id[[1L]])
        refreshed <- shift__run_handle(run_store, run@ids$run_id)
        return(tryCatch(
            shift__resume_generic_task(refreshed, step,
                ui = ui, background = background),
            error = function(e) {
                latest_run <- shift__run_handle(run_store, run@ids$run_id)
                if (identical(shift_status(latest_run, refresh = FALSE),
                    "waiting")) {
                    shift__run_finish(run_store, run@ids$run_id, "failed",
                        current_stage = step$task[[1L]],
                        last_error = conditionMessage(e))
                }
                stop(e)
            }
        ))
    }
    spec <- jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE)
    plan <- shift__plan_from_spec(spec, store = run@store_path)
    resolved <- row$resolved_spec_json[[1L]]
    if (!is.na(resolved) && nzchar(resolved)) {
        # Resolved member/grid/node choices are immutable across resume.
        plan@meta$resolved <- jsonlite::fromJSON(resolved, simplifyVector = TRUE)
    }
    if (isTRUE(background)) {
        shift__validate_background_plan(plan)
    }
    run_store <- shift_store(run)
    on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
    shift__run_event(run_store, run@ids$run_id, "resume", "running", "Workflow resume requested.")
    job <- shift__job_create(run_store, run@ids$run_id,
        mode = if (isTRUE(background)) "process" else "foreground", ui = ui)
    job_id <- job$job_id[[1L]]
    reporter <- shift__reporter(ui, store = run_store, run_id = run@ids$run_id,
        job_id = job_id, background = background)
    reporter$run_started(plan, run@ids$run_id, background = background)
    if (isTRUE(background)) {
        # Capture the new attempt before launch so the parent never reopens and
        # races the detached worker for DuckDB's process-level write lock.
        store_path <- run_store$path
        log_path <- job$log_path[[1L]]
        handle <- shift__run_handle(run_store, run@ids$run_id)
        run_store$close()
        shift__launch_job(store_path, run@ids$run_id, job_id, log_path)
        return(handle)
    }
    shift__plan_run(plan, run_id = run@ids$run_id, job_id = job_id,
        reporter = reporter, resume_existing = TRUE)
}

# Resolve either a ShiftRun handle or a run ID to a fresh persisted snapshot.
shift__as_run <- function(x, store = NULL) {
    if (S7::S7_inherits(x, ShiftRun)) {
        return(shift_refresh(x))
    }
    if (S7::S7_inherits(x, ShiftStage)) {
        return(shift_run_get(x, store = store))
    }
    checkmate::assert_string(x, min.chars = 1L)
    shift_run_get(x, store = store)
}

# Isolate watch-loop wall-clock reads so cadence tests can advance a deterministic
# clock without depending on runner speed or covr instrumentation overhead.
shift__watch_now <- function() {
    Sys.time()
}

# Isolate frame waiting for the same deterministic watch-loop tests while the
# production path continues to yield normally between dashboard updates.
shift__watch_sleep <- function(seconds) {
    Sys.sleep(seconds)
}

#' @rdname shift_api
#' @param follow Whether to continue watching until the run reaches a terminal
#'   status.
#' @param interval Polling interval in seconds.
#' @param events Number of recent events to display or return.
#' @param ui Runtime presentation options from [shift_ui()].
#' @export
shift_watch <- function(x, store = NULL, follow = TRUE, interval = 1,
                        events = 10L, ui = shift_ui()) {
    checkmate::assert_flag(follow)
    checkmate::assert_number(interval, lower = 0.1, finite = TRUE)
    checkmate::assert_count(events, positive = FALSE)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    run <- shift__as_run(x, store = store)
    run_id <- run@ids$run_id
    store_path <- run@store_path
    mode <- shift__ui_mode(ui)
    motion <- shift__ui_motion(ui, mode)
    terminal <- c("waiting", "completed", "partial", "failed", "cancelled")
    renderer <- tryCatch(shift__ui_renderer(mode), error = function(e) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) {
        mode <- "log"
        motion <- "none"
    }
    frame <- 0L
    last_event_id <- NA_character_
    event_cursor_initialized <- FALSE
    # Keep one atomic framebuffer alive for the same dashboard used by
    # foreground runs; constrained IDE consoles receive its compact form.
    update_dynamic <- function(view) {
        ok <- !is.null(renderer) &&
            isTRUE(renderer$draw(view$lines, compact = view$compact))
        if (!isTRUE(ok)) {
            if (!is.null(renderer)) renderer$close(result = "failed")
            renderer <<- NULL
            mode <<- "log"
            motion <<- "none"
        }
        ok
    }
    close_dynamic <- function(result = "done") {
        if (!is.null(renderer)) {
            renderer$close(result = result)
        }
        invisible(NULL)
    }
    on.exit(close_dynamic(), add = TRUE)
    emit_snapshot <- function(snapshot, initial = FALSE, final = FALSE) {
        view <- shift__ui_run_view(snapshot, width = shift__ui_width(),
            detail = ui@detail, motion = motion, frame = frame)
        if (identical(mode, "dynamic") && !isTRUE(final)) {
            if (!isTRUE(update_dynamic(view))) {
                shift__ui_print_view(view, include_tables = FALSE)
            }
        } else if (identical(mode, "dynamic") && isTRUE(final)) {
            close_dynamic(result = "done")
            shift__ui_print_view(view, include_tables = TRUE)
        } else if (identical(mode, "log")) {
            delta <- shift__ui_event_delta(
                snapshot@meta$events,
                last_event_id = last_event_id,
                initial_limit = events,
                initial = !event_cursor_initialized
            )
            rows <- delta$rows
            if (isTRUE(initial)) {
                shift__ui_print_view(view, include_tables = TRUE)
            } else {
                if (isTRUE(delta$gap)) {
                    cli::cli_alert_info(paste(
                        "Some older workflow events are no longer available",
                        "in the live buffer; continuing from its oldest event."
                    ))
                }
                for (i in seq_len(nrow(rows))) {
                    cli::cli_text("{shift__ui_persisted_event_line(rows[i], detail = ui@detail)}")
                }
            }
            last_event_id <<- delta$cursor
            event_cursor_initialized <<- TRUE
            if (isTRUE(final) && !isTRUE(initial)) {
                shift__ui_print_view(view, include_tables = TRUE)
            }
        }
        invisible(snapshot)
    }
    if (!isTRUE(follow)) {
        if (!identical(mode, "none")) {
            shift__ui_print_view(shift__ui_run_view(run,
                detail = ui@detail, motion = "none"), include_tables = TRUE)
        }
        return(run)
    }
    tryCatch({
        first <- TRUE
        last_poll <- as.POSIXct(NA)
        frame_interval <- if (identical(motion, "full")) {
            ui@refresh
        } else if (identical(motion, "reduced")) {
            max(1, ui@refresh)
        } else {
            interval
        }
        repeat {
            now <- shift__watch_now()
            poll_due <- isTRUE(first) || is.na(last_poll) ||
                as.numeric(difftime(now, last_poll, units = "secs")) >= interval
            if (isTRUE(poll_due)) {
                # Poll durable/live state at the requested interval while the
                # cached snapshot is animated independently between polls.
                run <- shift_run_get(run_id, store = store_path)
                last_poll <- now
            }
            done <- shift_status(run, refresh = FALSE) %in% terminal
            if (isTRUE(poll_due) || identical(mode, "dynamic")) {
                frame <- frame + 1L
                emit_snapshot(run, initial = first, final = done)
            }
            first <- FALSE
            if (done) break
            shift__watch_sleep(frame_interval)
        }
    }, interrupt = function(e) {
        close_dynamic(result = "cancelled")
        if (!identical(mode, "none")) {
            cli::cli_alert_info("Stopped watching {.val {run_id}}; the workflow continues. Use {.fn shift_cancel} to cancel it.")
        }
    })
    shift_run_get(run_id, store = store_path)
}

#' @rdname shift_api
#' @param force If `FALSE`, request cancellation at the next safe workflow
#'   boundary. If `TRUE`, persist the request and then terminate the recorded
#'   background worker process immediately.
#' @export
shift_cancel <- function(x, store = NULL, force = FALSE) {
    checkmate::assert_flag(force)
    run <- shift__as_run(x, store = store)
    status <- shift_status(run, refresh = FALSE)
    if (status %in% c("completed", "partial", "failed", "cancelled")) {
        return(run)
    }
    if (status %in% c("running", "stopping")) {
        download_store <- shift_store(run)
        download_context <- shift__background_download_context(
            download_store, run@ids$run_id)
        if (!is.null(download_context)) {
            on.exit(try(download_store$close(), silent = TRUE), add = TRUE)
            downloader_job_id <- if (nrow(download_context$jobs)) {
                as.character(download_context$jobs$job_id[[
                    nrow(download_context$jobs)]])
            } else {
                NA_character_
            }
            # Stop the owning Downloader job first, then mark any queued or
            # active tasks so its session reaches a deterministic terminal
            # state that the shared run reconciler can observe.
            if (!is.na(downloader_job_id) && nzchar(downloader_job_id)) {
                download_context$downloader$stop_job(downloader_job_id,
                    force = force)
            }
            download_context$downloader$cancel(
                session_id = download_context$session_id)
            shift__run_update(download_store, run@ids$run_id,
                status = "stopping", last_error = "Cancelled by user.")
            shift__run_event(download_store, run@ids$run_id, "download",
                "stopping", "Background download cancellation requested.",
                details = list(step_id = download_context$step$step_id[[1L]],
                    session_id = download_context$session_id,
                    downloader_job_id = downloader_job_id, force = force),
                step_id = download_context$step$step_id[[1L]])
            shift__reconcile_background_download(download_store,
                run@ids$run_id)
            return(shift__run_handle(download_store, run@ids$run_id))
        }
        try(download_store$close(), silent = TRUE)
    }
    if (identical(status, "waiting")) {
        # No process is active between object-carried stages. Cancelling here
        # closes the resumable run immediately instead of creating a stopping
        # state that no worker could ever acknowledge.
        run_store <- shift_store(run)
        on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
        shift__run_finish(run_store, run@ids$run_id, "cancelled",
            current_stage = run@meta$run$current_stage[[1L]],
            last_error = "Cancelled by user while waiting for the next stage.")
        shift__run_event(run_store, run@ids$run_id,
            run@meta$run$current_stage[[1L]], "cancelled",
            "Waiting shift run cancelled by user.")
        return(shift__run_handle(run_store, run@ids$run_id))
    }
    job <- data.table::as.data.table(run@meta$jobs)
    if (nrow(job)) {
        job <- job[which.max(job[["attempt"]])]
    }
    if (!nrow(job)) {
        cli::cli_abort("Shift run {.val {run@ids$run_id}} has no execution job to cancel.")
    }
    now <- store__now()
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    shift__cancel_request_write(run@store_path, run@ids$run_id,
        job$job_id[[1L]], force = force)

    # A detached worker owns DuckDB's write lock for the duration of an active
    # stage. If the manifest cannot be opened, the sidecar marker is the
    # cooperative signal and the live handle is updated in memory immediately.
    run_store <- tryCatch(EsgStore$new(run@store_path, create = FALSE),
        error = function(e) e)
    if (inherits(run_store, "error")) {
        if (!shift__manifest_locked(run_store)) {
            stop(run_store)
        }
        live_status <- if (isTRUE(force)) "cancelled" else "stopping"
        marked <- shift__live_cancel_mark(run@store_path, run@ids$run_id,
            job$job_id[[1L]], live_status)
        if (!is.null(marked)) {
            run <- marked
        }
        if (isTRUE(force) && !is.na(pid)) {
            downloader__pid_kill(pid)
            # Wait briefly for DuckDB to release its process lock, then let the
            # normal stale reconciliation record a cancelled terminal state.
            for (i in seq_len(40L)) {
                if (!downloader__pid_alive(pid)) break
                Sys.sleep(0.05)
            }
            refreshed <- tryCatch(shift_run_get(run@ids$run_id, run@store_path),
                error = function(e) NULL)
            if (!is.null(refreshed)) {
                return(refreshed)
            }
        }
        return(run)
    }
    on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
    job <- shift__latest_job(run_store, run@ids$run_id)
    immediate <- identical(job$status[[1L]], "queued") && is.na(pid)
    job_status <- if (isTRUE(immediate) || isTRUE(force)) "cancelled" else "stopping"
    shift__job_update(run_store, job$job_id[[1L]],
        status = job_status,
        cancel_requested_at = now,
        completed_at = if (identical(job_status, "cancelled")) now else job$completed_at,
        exit_code = if (identical(job_status, "cancelled")) 130L else job$exit_code,
        last_error = "Cancelled by user.")
    if (identical(job_status, "cancelled")) {
        shift__run_finish(run_store, run@ids$run_id,
            status = job_status, last_error = "Cancelled by user.")
    } else {
        shift__run_update(run_store, run@ids$run_id,
            status = job_status, last_error = "Cancelled by user.")
    }
    shift__run_event(run_store, run@ids$run_id, run@meta$run$current_stage[[1L]],
        job_status, "Cancellation requested by user.",
        details = list(job_id = job$job_id[[1L]], force = force, pid = pid))
    if (isTRUE(force) && !is.na(pid)) {
        downloader__pid_kill(pid)
    }
    shift__run_handle(run_store, run@ids$run_id)
}

#' @rdname shift_api
#' @param tail Maximum number of trailing execution log lines to return.
#' @export
shift_logs <- function(x, store = NULL, tail = 100L) {
    checkmate::assert_count(tail, positive = FALSE)
    run <- shift__as_run(x, store = store)
    run_store <- shift_store(run)
    download_context <- shift__background_download_context(
        run_store, run@ids$run_id, active_only = FALSE)
    if (!is.null(download_context) && nrow(download_context$jobs)) {
        downloader_job_id <- as.character(download_context$jobs$job_id[[
            nrow(download_context$jobs)]])
        downloader_logs <- data.table::as.data.table(
            download_context$downloader$job_logs(downloader_job_id,
                tail = tail))
        run_store$close()
        if (nrow(downloader_logs)) {
            downloader_logs[, source := "downloader"]
            # Character column selection avoids data.table's NSE here so R CMD
            # check does not mistake Downloader log fields for global symbols.
            return(downloader_logs[, c("job_id", "source", "line", "message"),
                with = FALSE])
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
            detail <- tryCatch({
                value <- jsonlite::fromJSON(job$ui_json[[1L]],
                    simplifyVector = TRUE)
                as.character(shift_coalesce(value$detail, "normal"))
            }, error = function(e) "normal")
            vapply(seq_len(nrow(event_rows)), function(i) {
                shift__ui_persisted_event_line(event_rows[i],
                    detail = detail, width = NULL)
            }, character(1L))
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

# Persist a Dataset result outside the relational File catalog. The JSON keeps
# the original EsgResultDataset contract intact while the lightweight stage
# provides stable run/step recovery coordinates.
shift__datasets_stage <- function(result, request, store) {
    if (!inherits(result, "EsgResultDataset")) {
        cli::cli_abort("A Dataset task must return an {.cls EsgResultDataset} object.")
    }
    if (!S7::S7_inherits(request, ShiftRequest)) {
        cli::cli_abort("A Dataset task must retain its originating {.cls ShiftRequest}.")
    }
    payload <- list(
        index_node = priv(result)$index_node,
        parameter = priv(result)$parameter$serialize(null = TRUE),
        records = result$to_data_table()
    )
    result_id <- store__hash(payload)
    path <- file.path(store$path, "queries",
        sprintf("datasets-%s.json", result_id))
    result$save(path)
    artifact_id <- store$register_artifact(
        kind = "query",
        path = path,
        role = "input",
        project = "CMIP6",
        metadata = list(result_type = "Dataset")
    )
    shift_stage_new(
        ShiftDatasets,
        "datasets",
        store_path = store$path,
        ids = list(result_id = result_id, artifact_id = artifact_id),
        meta = list(
            request = request,
            dataset_count = result$count(),
            result_path = store_rel_path(path, root = store$path),
            datasets = result
        )
    )
}

# Load the persisted Dataset result when the live R6 object is no longer
# available, for example after shift_result() reconstructs a previous run.
shift__datasets_result <- function(x) {
    if (!S7::S7_inherits(x, ShiftDatasets)) {
        cli::cli_abort("`x` must be an internal Dataset catalog stage.")
    }
    live <- x@meta$datasets
    if (inherits(live, "EsgResultDataset")) {
        return(live)
    }
    path <- store_abs_path(x@meta$result_path, root = x@store_path)
    if (!file.exists(path)) {
        cli::cli_abort(c(
            "The persisted Dataset result is unavailable.",
            "x" = "Missing file: {.path {path}}"
        ))
    }
    esg_result("dataset")$load(path)
}

# Carry run coordinates on the returned R6 result without changing its class or
# method surface. shift_run_get() uses these attributes as a convenience only;
# the store remains authoritative.
shift__datasets_attach_run <- function(result, stage) {
    attr(result, "epwshiftr.run_id") <- store__chr1(stage@ids$run_id)
    attr(result, "epwshiftr.step_id") <- store__chr1(stage@ids$step_id)
    attr(result, "epwshiftr.store") <- stage@store_path
    result
}

#' @rdname shift_api
#' @export
shift_datasets <- function(x, all = TRUE, limit = FALSE, store = NULL,
                           ui = NULL) {
    shift_assert_stage(x)
    checkmate::assert_flag(all)

    if (S7::S7_inherits(x, ShiftRequest)) {
        reporter <- shift__current_reporter()
        if (is.null(reporter)) {
            stage <- shift__task_execute(
                "datasets", x, store = store, ui = ui,
                spec = list(all = all, limit = limit),
                auto_complete = TRUE,
                code = function(reporter, task_store) {
                    result <- shift__with_reporter(reporter,
                        shift_datasets(x, all = all, limit = limit,
                            store = task_store))
                    shift__datasets_stage(result, x, task_store)
                }
            )
            return(shift__datasets_attach_run(
                shift__datasets_result(stage), stage))
        }

        query <- shift_as_query(x)
        node <- query$index_node()
        unit_total <- shift__catalog_unit_total()
        reporter$unit_started("Collecting Dataset catalog",
            current = 1L, total = unit_total,
            details = list(unit_type = "catalog", catalog_role = "Dataset",
                node = node))
        result <- shift__with_query_reporter(
            reporter, node, "Dataset",
            query$collect(type = "Dataset", all = all, limit = limit,
                progress = FALSE)
        )
        reporter$unit_completed(sprintf("Collected %d Dataset record(s)",
            result$count()), current = 1L, total = unit_total,
            details = list(unit_type = "catalog", catalog_role = "Dataset",
                node = node, records = result$count()))
        return(result)
    }

    if (S7::S7_inherits(x, ShiftDatasets)) {
        return(shift__datasets_result(x))
    }

    files <- shift_stage_nested(x, list(ShiftFiles))
    if (!is.null(files) && !is.null(files@meta$datasets)) {
        return(files@meta$datasets)
    }

    request <- shift_stage_root(x)
    if (!is.null(request)) {
        return(shift_datasets(request, all = all, limit = limit,
            store = store, ui = ui))
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
#' @param refresh In [shift_ui()], minimum seconds between visual animation
#'   frames. In `ShiftRun` inspectors, whether to reload persisted state first.
#' @export
shift_data <- function(x, n = 100L, variables = NULL, case_id = NULL,
                       columns = NULL, refresh = TRUE) {
    shift_assert_stage(x)
    n <- shift_data_limit(n)
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(case_id, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_character(columns, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        if (isTRUE(x@meta$live) && shift_status(x, refresh = FALSE) %in%
            c("queued", "running", "stopping")) {
            return(data.table::data.table())
        }
        if (!identical(as.character(x@meta$run$task[[1L]]), "future_epw")) {
            stage <- tryCatch(shift_result(x), error = function(e) NULL)
            supported <- !is.null(stage) && any(vapply(
                list(ShiftClimate, ShiftMorphed, ShiftOutputs),
                function(class) S7::S7_inherits(stage, class), logical(1L)))
            if (!isTRUE(supported)) {
                return(data.table::data.table())
            }
            return(shift_data(stage, n = n, variables = variables,
                case_id = case_id, columns = columns, refresh = FALSE))
        }
        stage <- x@meta$output_stage
        if (!S7::S7_inherits(stage, ShiftOutputs)) {
            morph_id <- x@ids$morph_id
            if (is.na(morph_id) || !nzchar(morph_id)) {
                return(data.table::data.table())
            }
            stage <- shift_stage_new(
                ShiftOutputs,
                "epw",
                store_path = x@store_path,
                ids = list(morph_id = morph_id),
                meta = list(outputs = shift_outputs(x))
            )
        }
        return(shift_data(stage, n = n, variables = variables,
            case_id = case_id, columns = columns, refresh = FALSE))
    }
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
shift_diagnostics <- function(x, severity = NULL, refresh = TRUE) {
    shift_assert_stage(x)
    checkmate::assert_flag(refresh)
    checkmate::assert_character(severity, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (isTRUE(refresh) && S7::S7_inherits(x, ShiftRun)) {
        x <- shift_refresh(x)
    }
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
shift_outputs <- function(x, refresh = TRUE) {
    shift_assert_stage(x)
    checkmate::assert_flag(refresh)
    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        if (!identical(as.character(x@meta$run$task[[1L]]),
            "future_epw")) {
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
            data.table::as.data.table(shift_coalesce(x@meta$outputs, data.table::data.table()))
        } else {
            shift_epw_output_rows(run_store, morph_id)
        }
        cases <- shift_cases(x, refresh = FALSE)
        if (nrow(outputs) && nrow(cases)) {
            exports <- cases[!is.na(output_id), .(output_id, export_path)]
            outputs <- merge(outputs, exports, by = "output_id", all.x = TRUE, sort = FALSE)
        }
        return(outputs[])
    }
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
shift_status <- function(x, refresh = TRUE) {
    shift_assert_stage(x)
    checkmate::assert_flag(refresh)

    if (S7::S7_inherits(x, ShiftRun)) {
        if (isTRUE(refresh)) {
            x <- shift_refresh(x)
        }
        return(as.character(x@meta$run$status[[1L]]))
    }

    if (shift_stage_has_errors(x@diagnostics)) {
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
        count <- as.integer(shift_coalesce(x@meta$dataset_count, 0L))
        return(if (file.exists(path) && count > 0L) "collected" else "partial")
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

# Apply a stage request constraint to the ESGF query through the most specific
# setter available so control parameters such as `latest` and `replica` keep
# their typed validation instead of falling through to ad hoc `$params()`.
shift_query_set <- function(query, name, value) {
    if (is.null(value)) {
        return(invisible(query))
    }
    if (name %in% names(QUERY_PARAM__DEF) && is.function(query[[name]])) {
        query[[name]](value)
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
        # Preserve provider facet values exactly so the query and EsgDict
        # diagnostics describe precisely what the user supplied.
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
                                                    label = NULL, ui = NULL, ...) {
    reporter <- shift__current_reporter()
    dots <- list(...)
    if ("progress" %in% names(dots)) {
        cli::cli_abort(c(
            "{.fn shift_collect} no longer accepts a logical `progress` argument.",
            "i" = "Use `ui = shift_ui(progress = ...)`; low-level {.cls EsgQuery} collection still accepts native progress controls."
        ))
    }
    checkmate::assert_character(fields, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_flag(all)
    checkmate::assert_string(label, null.ok = TRUE)
    if (is.null(store)) {
        cli::cli_abort("`store` is required for {.fn shift_collect}.")
    }
    store <- shift_store(store, create = TRUE)
    datasets <- shift__with_catalog_unit_total(2L,
        shift_datasets(x, all = all, limit = limit, store = store))
    node <- priv(datasets)$index_node
    if (!is.null(reporter)) {
        reporter$unit_started("Collecting File catalog",
            current = 2L, total = 2L,
            details = list(unit_type = "catalog", catalog_role = "File",
                node = node))
    }
    files <- shift__with_query_reporter(
        reporter, node, "File",
        do.call(datasets$collect, c(list(type = "File", fields = fields,
            all = TRUE, limit = NULL, progress = FALSE), dots))
    )

    file_time <- shift_coalesce(x@meta$options$file_time, x@meta$time)
    if (!is.null(file_time) &&
        !identical(x@meta$options$time_filter_method, "metadata")) {
        time <- as.character(file_time)
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

    if (!is.null(reporter)) {
        size <- if ("size" %in% names(file_dt)) {
            sum(suppressWarnings(as.numeric(file_dt$size)), na.rm = TRUE)
        } else {
            NA_real_
        }
        reporter$unit_completed(sprintf("Collected %d File record(s)",
            files$count()), current = 2L, total = 2L,
            details = list(unit_type = "catalog", catalog_role = "File",
                node = node, records = files$count(), bytes_total = size))
    }

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
            fields = fields,
            # Keep the provider response field set with the stage so its
            # persisted receipt can reproduce EsgResultFile's established
            # summary without loading the complete saved result.
            result_fields = files$fields
        )
    )
}

# Summarize downloader task state into workflow-specific byte and file metrics.
shift__download_metrics <- function(downloader, session_id, variables = 0L) {
    tasks <- tryCatch(downloader$tasks(session_id = session_id),
        error = function(e) data.frame())
    total <- nrow(tasks)
    completed <- if (total) sum(tasks$status %in% c("done", "skipped")) else 0L
    failed <- if (total) sum(tasks$status %in% c("error", "cancelled")) else 0L
    bytes_done <- if (total && "bytes_done" %in% names(tasks)) {
        sum(suppressWarnings(as.numeric(tasks$bytes_done)), na.rm = TRUE)
    } else {
        0
    }
    sizes <- if (total && "size" %in% names(tasks)) {
        suppressWarnings(as.numeric(tasks$size))
    } else {
        numeric()
    }
    bytes_total <- if (length(sizes) && all(is.finite(sizes) & sizes >= 0)) {
        sum(sizes)
    } else {
        NA_real_
    }
    active <- if (total) tasks$status %in% "downloading" else logical()
    speeds <- if (any(active) && "speed_bps" %in% names(tasks)) {
        suppressWarnings(as.numeric(tasks$speed_bps[active]))
    } else {
        numeric()
    }
    speed_bps <- if (length(speeds) && any(is.finite(speeds) & speeds > 0)) {
        sum(speeds[is.finite(speeds) & speeds > 0])
    } else {
        NA_real_
    }
    eta_seconds <- if (is.finite(bytes_total) && is.finite(speed_bps) && speed_bps > 0) {
        max(0, bytes_total - bytes_done) / speed_bps
    } else {
        NA_real_
    }
    active_files <- if (any(active)) {
        column <- if ("filename" %in% names(tasks)) {
            tasks$filename
        } else if ("target_path" %in% names(tasks)) {
            basename(tasks$target_path)
        } else {
            rep(NA_character_, total)
        }
        values <- basename(as.character(column[active]))
        unique(values[!is.na(values) & nzchar(values)])
    } else {
        character()
    }
    list(
        current = completed,
        total = total,
        failed = failed,
        bytes_done = bytes_done,
        bytes_total = bytes_total,
        speed_bps = speed_bps,
        eta_seconds = eta_seconds,
        active_task_count = sum(active),
        active_files = active_files,
        variables = as.integer(variables)
    )
}

# Format one task-specific download status shared by progress, completion, and
# persisted workflow events.
shift__download_label <- function(role, metrics, active = NULL) {
    label <- sprintf("%s download \u00b7 %d/%d files \u00b7 %s/%s \u00b7 %d variables",
        role, metrics$current, metrics$total,
        shift__ui_bytes(metrics$bytes_done), shift__ui_bytes(metrics$bytes_total),
        metrics$variables)
    if (is.finite(metrics$speed_bps) && metrics$speed_bps > 0) {
        label <- paste0(label, " \u00b7 ", shift__ui_bytes(metrics$speed_bps), "/s")
    }
    if (is.finite(metrics$eta_seconds)) {
        label <- paste0(label, " \u00b7 ETA ", shift__format_elapsed(metrics$eta_seconds))
    }
    if (!is.null(active) && length(active) && !is.na(active) && nzchar(active)) {
        paste0(label, " \u00b7 ", basename(active))
    } else {
        label
    }
}

# Bridge downloader callbacks into the workflow reporter. Progress callbacks
# are throttled by ShiftReporter while task/fallback milestones remain durable.
shift__download_reporter_bind <- function(downloader, reporter, role,
                                            variables = 0L, nested = FALSE) {
    checkmate::assert_flag(nested)
    tokens <- character()
    callback <- function(event, dl) {
        metrics <- shift__download_metrics(dl, event$session_id,
            variables = variables)
        active <- if (length(metrics$active_files)) {
            paste(utils::head(metrics$active_files, 2L), collapse = " + ")
        } else {
            shift_coalesce(event$filename, event$target_path)
        }
        label <- shift__download_label(role, metrics,
            active = if (shift__ui_at_least(reporter$ui(), "detail")) active else NULL)
        details <- list(
            unit_type = "download_session",
            catalog_role = role,
            current = metrics$current,
            total = metrics$total,
            bytes_done = metrics$bytes_done,
            bytes_total = metrics$bytes_total,
            speed_bps = metrics$speed_bps,
            eta_seconds = metrics$eta_seconds,
            active_task_count = metrics$active_task_count,
            active_files = utils::head(metrics$active_files, 2L),
            variables = metrics$variables,
            data_node = event$data_node,
            access_method = "HTTPServer"
        )
        switch(event$event,
            session_start = if (isTRUE(nested)) {
                reporter$unit_updated(label,
                    current = metrics$current, total = metrics$total,
                    details = details)
            } else {
                reporter$unit_started(label,
                    current = metrics$current, total = metrics$total,
                    details = details)
            },
            task_start = reporter$unit_updated(label,
                current = metrics$current, total = metrics$total,
                details = details),
            task_progress = reporter$heartbeat(label, details = details),
            candidate_error = reporter$notice(sprintf(
                "%s download \u00b7 %s unavailable \u00b7 %s",
                role, shift_coalesce(event$data_node, "candidate"),
                shift__error_summary(event$error)), outcome = "fallback",
                details = details),
            task_done = reporter$unit_updated(label,
                current = metrics$current, total = metrics$total,
                details = details),
            task_error = reporter$unit_updated(label,
                current = metrics$current, total = metrics$total,
                details = utils::modifyList(details,
                    list(outcome = "failed", error = event$error))),
            task_cancelled = reporter$unit_updated(label,
                current = metrics$current, total = metrics$total,
                details = utils::modifyList(details,
                    list(outcome = "cancelled", error = event$error))),
            session_done = if (isTRUE(nested)) {
                reporter$unit_updated(label,
                    current = metrics$current, total = metrics$total,
                    details = utils::modifyList(details, list(
                        outcome = if (metrics$failed) "failed" else "completed")))
            } else {
                reporter$unit_completed(label,
                    current = metrics$current, total = metrics$total,
                    outcome = if (metrics$failed) "failed" else "completed",
                    details = details)
            }
        )
        invisible(TRUE)
    }
    for (event in DOWNLOADER_CALLBACK_EVENTS) {
        tokens <- c(tokens, downloader$on(event, callback))
    }
    function() {
        for (token in tokens) {
            try(downloader$off(token), silent = TRUE)
        }
        invisible(NULL)
    }
}

S7::method(shift_download, ShiftFiles) <- function(x, downloader = NULL, run = TRUE, background = FALSE,
                                                   resume = TRUE, overwrite = FALSE, session_label = NULL,
                                                   ui = NULL, ...) {
    reporter <- shift__current_reporter()
    checkmate::assert_flag(run)
    checkmate::assert_flag(background)
    checkmate::assert_flag(resume)
    checkmate::assert_flag(overwrite)

    store <- shift_store(x)
    if (is.null(downloader) && (!isTRUE(run) || !is.null(reporter))) {
        downloader <- if (isTRUE(run)) {
            store$downloader()
        } else {
            store$downloader(n_workers = 0L)
        }
    }
    cleanup <- NULL
    if (!is.null(reporter)) {
        role <- shift_coalesce(session_label, "CMIP6")
        cleanup <- shift__download_reporter_bind(
            downloader, reporter, role = role,
            variables = length(x@meta$variables)
        )
        on.exit(cleanup(), add = TRUE)
    }
    dots <- list(...)
    if (!is.null(reporter)) {
        # The workflow renderer owns progress; native downloader bars would
        # create a second, competing live region.
        dots$progress <- FALSE
    }
    session <- do.call(store$download_files, c(list(
        query_id = x@ids$query_id,
        downloader = downloader,
        run = run,
        background = background,
        resume = resume,
        overwrite = overwrite,
        session_label = session_label
    ), dots))
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
                                filters = list(), method = "nearest",
                                fallback = c("auto", "error"), overwrite = FALSE,
                                resume = TRUE, reporter = NULL) {
    checkmate::assert_choice(upstream_name, c("files", "download"))
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("`site` must be created by {.fn shift_site}.")
    }
    checkmate::assert_data_frame(periods)
    checkmate::assert_list(filters, names = "unique")
    method <- match.arg(method, ESG_GRID_METHOD_CHOICES)
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
        method = method
    )
    plan_id <- unique(plan$plan_id)
    processed <- store$extract(plan_id = plan_id, fallback = fallback,
        overwrite = overwrite, resume = resume, reporter = reporter)
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

# Run pre-existing extraction plan IDs through the same durable task boundary
# used by shift_extract(). This adapter lets the CLI retain its plan/run split
# without creating a second progress or persistence implementation.
shift__extract_plans_task <- function(store, plan_id,
                                      fallback = c("auto", "error"),
                                      overwrite = FALSE, resume = TRUE,
                                      ui = NULL) {
    store <- shift_store(store, create = FALSE)
    plan_id <- as.character(plan_id)
    checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L,
        unique = TRUE)
    fallback <- match.arg(fallback)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    plans <- shift_extraction_plan(store, plan_id)
    if (!nrow(plans)) {
        cli::cli_abort("No extraction plan rows were found.")
    }
    identity <- unique(plans[, .(site_id, lon, lat, method)])
    if (nrow(identity) != 1L) {
        cli::cli_abort("One extraction task cannot mix sites or extraction methods.")
    }
    query_id <- unique(plans$query_id)
    query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
    if (!length(query_id)) {
        cli::cli_abort("Extraction plans do not contain a source query ID.")
    }
    time_start <- min(plans$time_start, na.rm = TRUE)
    time_stop <- max(plans$time_stop, na.rm = TRUE)
    years <- seq.int(as.integer(format(time_start, "%Y", tz = "UTC")),
        as.integer(format(time_stop, "%Y", tz = "UTC")))
    periods <- epw_morph_periods(extract = years)
    site <- shift_site(identity$site_id[[1L]], identity$lon[[1L]],
        identity$lat[[1L]])
    files <- shift_stage_new(ShiftFiles, "files", store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(request = NULL, dataset_count = NA_integer_,
            file_count = nrow(shift_file_catalog(store, query_id)),
            variables = unique(plans$variable_id), fields = "*"))
    spec <- list(
        site = shift__site_ref(site),
        periods = split(as.integer(periods$year), periods$period),
        variables = unique(plans$variable_id),
        time = c(time_start, time_stop),
        filters = list(), method = identity$method[[1L]],
        fallback = fallback, overwrite = overwrite, resume = resume
    )
    shift__task_execute("extract", files, store = store, ui = ui, spec = spec,
        code = function(reporter, task_store) {
            processed <- task_store$extract(plan_id = plan_id,
                fallback = fallback, overwrite = overwrite, resume = resume,
                reporter = reporter)
            coverage <- task_store$coverage(plan_id = plan_id)
            shift_stage_new(ShiftClimate, "climate",
                store_path = task_store$path,
                ids = list(query_id = query_id, plan_id = plan_id),
                meta = list(files = files, site = site, periods = periods,
                    variables = unique(plans$variable_id), plan = plans,
                    processed = processed, coverage = coverage),
                diagnostics = shift_diagnostics_from_coverage(coverage))
        })
}

S7::method(shift_extract, ShiftFiles) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                  filters = list(), method = "nearest",
                                                  fallback = c("auto", "error"), overwrite = FALSE,
                                                  resume = TRUE, ui = NULL) {
    shift_extract_stage(
        x,
        upstream_name = "files",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        method = method,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume,
        reporter = shift__current_reporter()
    )
}

S7::method(shift_extract, ShiftDownload) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                     filters = list(), method = "nearest",
                                                     fallback = c("auto", "error"), overwrite = FALSE,
                                                     resume = TRUE, ui = NULL) {
    shift_extract_stage(
        x,
        upstream_name = "download",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        method = method,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume,
        reporter = shift__current_reporter()
    )
}

shift_reference_has_legacy_args <- function(reference_plan_id = NULL, reference_periods = NULL) {
    !is.null(reference_plan_id) || !is.null(reference_periods)
}

shift_reference_resolve <- function(x, recipe, site, reference = NULL,
                                    reference_plan_id = NULL, reference_periods = NULL,
                                    overwrite = FALSE, resume = TRUE,
                                    reporter = NULL) {
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
            resume = resume,
            reporter = reporter
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

# Resolve observed daily weather only from an already extracted climate stage
# or explicit plan IDs. Automatic CMIP historical discovery cannot satisfy the
# observational role and is rejected before any store work begins.
shift__observed_reference_resolve <- function(
    x,
    recipe,
    site,
    observed_reference = NULL,
    observed_plan_id = NULL,
    observed_periods = NULL,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    if (S7::S7_inherits(observed_reference, ShiftReferenceSpec) &&
        !identical(observed_reference@mode, "plan")) {
        cli::cli_abort(
            paste(
                "{.arg observed_reference} must use an existing extraction",
                "plan; historical CMIP output is not an observation."
            )
        )
    }
    shift_reference_resolve(
        x = x,
        recipe = recipe,
        site = site,
        reference = observed_reference,
        reference_plan_id = observed_plan_id,
        reference_periods = observed_periods,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
}

shift_reference_resolve_historical <- function(x, recipe, site, spec,
                                               overwrite = FALSE,
                                               resume = TRUE,
                                               reporter = NULL) {
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
    variables <- shift_coalesce(spec@extract$variables,
        morpher__input_variables(recipe))
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
    # Historical Dataset records often span the full CMIP run; only constrain
    # ESGF collection by time when the caller explicitly requests it.
    collect_time <- if ("time" %in% names(spec@collect)) spec@collect$time else NULL
    request <- shift_request(
        provider = provider,
        project = project,
        time = collect_time,
        filters = filters,
        options = options
    )

    collect_overrides <- spec@collect
    collect_overrides$time <- NULL
    collect_args <- utils::modifyList(
        list(store = store, fields = "*", all = TRUE, limit = FALSE,
            label = "historical-reference"),
        collect_overrides
    )
    files <- shift__do_call_with_reporter(reporter, shift_collect,
        c(list(request), collect_args))
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
        method = "nearest",
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
    climate <- shift__do_call_with_reporter(reporter, shift_extract,
        c(list(files), extract_args))
    shift__derive_hurs_climate(
        climate, recipe, overwrite = overwrite, resume = resume,
        reporter = reporter
    )
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

# Select the complete subset of extraction plans for morphing while keeping
# incomplete-plan diagnostics visible on the morphed stage.
shift_morph_complete_plan_selection <- function(store, plan_id, complete_only = TRUE, stage = "morph") {
    if (is.null(plan_id) || !length(plan_id) || !isTRUE(complete_only)) {
        return(list(plan_id = plan_id, diagnostics = shift_diagnostics_empty()))
    }

    coverage <- store$coverage(plan_id = plan_id)
    if (!nrow(coverage) || !"complete" %in% names(coverage)) {
        return(list(plan_id = plan_id, diagnostics = shift_diagnostics_empty()))
    }

    complete_ids <- unique(coverage$plan_id[coverage$complete %in% TRUE])
    selected <- plan_id[plan_id %in% complete_ids]
    if (!length(selected)) {
        cli::cli_abort("No complete extraction plan is available for the `{stage}` stage.")
    }
    if (identical(selected, plan_id)) {
        return(list(plan_id = plan_id, diagnostics = shift_diagnostics_empty()))
    }

    skipped <- setdiff(plan_id, selected)
    list(
        plan_id = selected,
        diagnostics = shift_diagnostic(
            stage,
            "warning",
            "ignored_incomplete_extraction",
            sprintf("Ignoring %d incomplete extraction plan(s) while morphing.", length(skipped)),
            plan_id = paste(skipped, collapse = ", "),
            action = "Inspect `shift_coverage()` for skipped plans, or set `complete_only = FALSE` to include them."
        )
    )
}

# Serialize an explicit workflow reference without introducing an implicit
# historical mode during persistence or reconstruction.
shift__reference_spec_value <- function(reference) {
    if (is.null(reference)) {
        return(NULL)
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return(list(
            mode = "plan",
            plan_id = shift_ids(reference)$plan_id,
            periods = split(as.integer(reference@meta$periods$year), reference@meta$periods$period)
        ))
    }
    if (!S7::S7_inherits(reference, ShiftReferenceSpec)) {
        cli::cli_abort("Cannot persist an unsupported shift reference object.")
    }
    list(
        mode = reference@mode,
        plan_id = reference@plan_id,
        periods = split(as.integer(reference@periods$year), reference@periods$period),
        experiment = reference@experiment,
        activity = reference@activity,
        match = reference@match,
        filters = reference@filters,
        options = reference@options,
        collect = reference@collect,
        extract = reference@extract
    )
}

# Rebuild only the reference mode that was serialized; a missing value remains
# missing and is never converted into a historical reference.
shift__reference_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    periods <- shift__periods_from_input(spec$periods, arg = "method$reference$periods")
    if (identical(spec$mode, "plan")) {
        return(shift_reference_plan(as.character(spec$plan_id), periods))
    }
    if (identical(spec$mode, "historical")) {
        return(shift_reference_historical(
            periods = periods,
            experiment = as.character(spec$experiment),
            activity = as.character(spec$activity),
            match = as.character(spec$match),
            filters = shift_coalesce(spec$filters, list()),
            options = shift_coalesce(spec$options, list()),
            collect = shift_coalesce(spec$collect, list()),
            extract = shift_coalesce(spec$extract, list())
        ))
    }
    cli::cli_abort("Unsupported persisted reference mode: {.val {spec$mode}}.")
}

# Serialize the complete CMIP6 identity as the sole scientific source of truth;
# the lower-level request is derived from this value when a run is resumed.
shift__climate_spec_value <- function(climate) {
    if (is.null(climate)) {
        return(NULL)
    }
    list(
        provider = "cmip6",
        model = climate@model,
        scenarios = climate@scenarios,
        member = climate@member,
        grid = climate@grid,
        frequency = climate@frequency,
        # JSON objects preserve variable names; named atomic vectors do not
        # when `auto_unbox = TRUE`, so overrides are persisted as a named list.
        table = if (!is.null(names(climate@table))) {
            as.list(climate@table)
        } else {
            climate@table
        },
        activity = climate@activity,
        index_nodes = climate@index_nodes,
        data_node = climate@data_node,
        filters = climate@filters
    )
}

# Rebuild only explicitly supported climate specifications from persisted task
# intent instead of inferring provider or model fields from request artifacts.
shift__climate_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (!identical(as.character(spec$provider), "cmip6")) {
        cli::cli_abort("Unsupported persisted climate provider: {.val {spec$provider}}.")
    }
    do.call(shift_cmip6, spec[setdiff(names(spec), "provider")])
}

# Convert a plan into a canonical, JSON-safe task specification. Deterministic
# artifacts can be reused by spec hash while each invocation still gets a
# unique run ID.
shift__plan_spec <- function(x) {
    meta <- x@meta
    request <- meta$request@meta
    method <- meta$method
    control <- meta$control
    climate <- meta$climate
    epw_path <- if (is.character(meta$site@epw) && length(meta$site@epw) == 1L) {
        normalizePath(path.expand(meta$site@epw), winslash = "/", mustWork = FALSE)
    } else {
        shift_coalesce(meta$epw_identity$path, NULL)
    }
    list(
        version = 1L,
        task = "future_epw",
        request = if (is.null(climate)) request else NULL,
        site = list(
            id = meta$site@id,
            lon = meta$site@lon,
            lat = meta$site@lat,
            label = meta$site@label,
            epw = epw_path,
            metadata = meta$site@metadata,
            identity = meta$epw_identity
        ),
        periods = split(as.integer(meta$periods$year), meta$periods$period),
        method = list(
            name = method@name,
            recipe = list(
                name = method@recipe$name,
                backend = method@recipe$backend,
                profile = method@recipe$profile,
                recipe_spec = method@recipe$recipe_spec,
                recipe_version = method@recipe$recipe_version,
                policy = method@recipe$policy,
                options = unclass(method@recipe$options),
                methods = as.list(method@recipe$methods),
                rules_identity = store__hash(morpher__json(method@recipe))
            ),
            requires_reference = method@requires_reference,
            requires_observed_reference =
                method@requires_observed_reference,
            reference_mode = if (is.null(method@reference)) {
                if (isTRUE(morpher__recipe_accepts_reference(method@recipe))) "baseline_epw" else "none"
            } else if (S7::S7_inherits(method@reference, ShiftReferenceSpec)) {
                method@reference@mode
            } else {
                "plan"
            },
            reference = shift__reference_spec_value(method@reference),
            observed_reference = shift__reference_spec_value(
                method@observed_reference
            )
        ),
        climate = shift__climate_spec_value(climate),
        control = list(
            strict = control@strict,
            allow_partial = control@allow_partial,
            download = control@download,
            resume = control@resume,
            overwrite = control@overwrite,
            extraction_method = control@extraction_method,
            output_layout = control@output_layout
        ),
        store = x@store_path,
        stages = list(
            collect = meta$collect,
            download = meta$download,
            extract = meta$extract,
            morph = meta$morph,
            epw = meta$epw
        )
    )
}

# Encode workflow specs with stable key order inherited from the constructor
# lists so identical scientific intent produces the same hash.
shift__spec_json <- function(spec) {
    as.character(jsonlite::toJSON(
        spec,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = 15,
        POSIXt = "ISO8601"
    ))
}

# Convert one site into the JSON-safe identity required by later extraction and
# morph steps. EPW objects are persisted through their backing path only.
shift__site_ref <- function(site) {
    if (is.null(site)) return(NULL)
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("Cannot persist a non-ShiftSite task target.")
    }
    epw <- site@epw
    epw_path <- if (shift_is_epw_path(epw)) {
        normalizePath(path.expand(epw), winslash = "/", mustWork = FALSE)
    } else if (shift_is_epw_object(epw)) {
        epw_file_coerce(epw)$path()
    } else {
        NULL
    }
    list(id = site@id, lon = site@lon, lat = site@lat, label = site@label,
        epw = epw_path, metadata = site@metadata)
}

# Rebuild a persisted site without inferring or replacing a missing EPW path.
shift__site_from_ref <- function(ref) {
    if (is.null(ref)) return(NULL)
    shift_site(
        id = as.character(ref$id),
        lon = as.numeric(ref$lon),
        lat = as.numeric(ref$lat),
        label = if (is.null(ref$label)) NULL else as.character(ref$label),
        epw = if (is.null(ref$epw)) NULL else as.character(ref$epw),
        metadata = shift_coalesce(ref$metadata, list())
    )
}

# Persist the recipe identity needed to reconstruct package-provided morphing
# backends while keeping executable backend closures out of the manifest.
shift__recipe_ref <- function(recipe) {
    if (is.null(recipe)) return(NULL)
    list(
        name = recipe$name,
        backend = recipe$backend,
        profile = recipe$profile,
        recipe_spec = recipe$recipe_spec,
        recipe_version = recipe$recipe_version,
        policy = recipe$policy,
        options = unclass(recipe$options),
        methods = as.list(recipe$methods)
    )
}

# Rebuild a package recipe from stable fields. Recipes persisted before profile
# support are deliberately interpreted as legacy rather than silently upgraded.
shift__recipe_from_ref <- function(ref) {
    if (is.null(ref)) return(NULL)
    methods <- unlist(ref$methods, use.names = TRUE)
    if (!length(methods)) methods <- NULL
    backend <- as.character(ref$backend)
    is_belcher <- backend %in% c("belcher", "belcher_absolute")
    profile <- if (is.null(ref$profile)) {
        if (is_belcher) "legacy" else "default"
    } else {
        as.character(ref$profile)
    }
    epw_morph_recipe(
        name = as.character(ref$name),
        backend = backend,
        methods = methods,
        profile = profile,
        options = shift_coalesce(ref$options, NULL),
        policy = shift_coalesce(ref$policy, NULL),
        version = if (is.null(ref$recipe_version)) {
            NULL
        } else {
            as.integer(ref$recipe_version)
        },
        spec = shift_coalesce(ref$recipe_spec, NULL)
    )
}

# Reduce a stage to stable store IDs plus the minimum scientific metadata
# required to continue the normal collect-to-export chain in another session.
shift__stage_ref <- function(x) {
    if (is.null(x)) return(NULL)
    shift_assert_stage(x)
    base <- list(
        version = 1L,
        class = class(x)[[1L]],
        stage = x@stage,
        store_path = x@store_path,
        ids = x@ids
    )
    meta <- if (S7::S7_inherits(x, ShiftRequest)) {
        x@meta
    } else if (S7::S7_inherits(x, ShiftDatasets)) {
        list(
            request = shift__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            result_path = x@meta$result_path
        )
    } else if (S7::S7_inherits(x, ShiftFiles)) {
        list(
            request = shift__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            file_count = x@meta$file_count,
            variables = x@meta$variables,
            fields = x@meta$fields
        )
    } else if (S7::S7_inherits(x, ShiftDownload)) {
        list(files = shift__stage_ref(x@meta$files))
    } else if (S7::S7_inherits(x, ShiftClimate)) {
        upstream <- shift_coalesce(x@meta$download, x@meta$files)
        list(
            upstream = shift__stage_ref(upstream),
            site = shift__site_ref(x@meta$site),
            periods = split(as.integer(x@meta$periods$year),
                x@meta$periods$period),
            variables = x@meta$variables
        )
    } else if (S7::S7_inherits(x, ShiftMorphed)) {
        baseline <- x@meta$baseline
        list(
            climate = shift__stage_ref(x@meta$climate),
            baseline = if (S7::S7_inherits(baseline, ShiftSite)) {
                list(type = "site", value = shift__site_ref(baseline))
            } else if (is.character(baseline) && length(baseline) == 1L) {
                list(type = "path", value = normalizePath(path.expand(baseline),
                    winslash = "/", mustWork = FALSE))
            } else {
                NULL
            },
            recipe = shift__recipe_ref(x@meta$recipe),
            reference_plan_id = x@meta$reference_plan_id,
            reference_periods = if (is.null(x@meta$reference_periods)) NULL else
                split(as.integer(x@meta$reference_periods$year),
                    x@meta$reference_periods$period)
        )
    } else if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- data.table::as.data.table(shift_coalesce(
            x@meta$outputs, data.table::data.table()))
        exports <- if (all(c("output_id", "export_path") %in% names(outputs))) {
            list(output_id = outputs$output_id,
                export_path = outputs$export_path)
        } else {
            NULL
        }
        list(
            morphed = shift__stage_ref(x@meta$morphed),
            format = x@meta$format,
            paths = x@meta$paths,
            export_dir = x@meta$export_dir,
            exports = exports
        )
    } else {
        list()
    }
    base$meta <- meta
    base
}

# Reconstruct a lightweight but actionable stage from persisted IDs. Large
# datasets and workflow objects are queried from the store instead of being
# embedded in JSON step rows.
shift__stage_from_ref <- function(ref) {
    if (is.null(ref)) return(NULL)
    stage <- as.character(ref$stage)
    store_path <- if (is.null(ref$store_path)) NULL else
        as.character(ref$store_path)
    ids <- lapply(shift_coalesce(ref$ids, list()), function(value) {
        unlist(value, use.names = FALSE)
    })
    meta <- shift_coalesce(ref$meta, list())
    if (identical(stage, "request")) {
        return(do.call(shift_request, meta))
    }
    if (identical(stage, "datasets")) {
        request <- shift__stage_from_ref(meta$request)
        return(shift_stage_new(ShiftDatasets, "datasets",
            store_path = store_path, ids = ids, meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                result_path = as.character(meta$result_path)
            )))
    }
    if (identical(stage, "files")) {
        request <- shift__stage_from_ref(meta$request)
        return(shift_stage_new(ShiftFiles, "files", store_path = store_path,
            ids = ids, meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                file_count = as.integer(meta$file_count),
                variables = as.character(unlist(meta$variables, use.names = FALSE)),
                fields = as.character(unlist(meta$fields, use.names = FALSE))
            )))
    }
    if (identical(stage, "download")) {
        files <- shift__stage_from_ref(meta$files)
        return(shift_stage_new(ShiftDownload, "download",
            store_path = store_path, ids = ids,
            meta = list(files = files, session = NULL)))
    }
    if (identical(stage, "climate")) {
        upstream <- shift__stage_from_ref(meta$upstream)
        site <- shift__site_from_ref(meta$site)
        periods <- shift__periods_from_input(meta$periods)
        upstream_name <- if (S7::S7_inherits(upstream, ShiftDownload)) {
            "download"
        } else {
            "files"
        }
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        # Coverage is a computed store view rather than a persisted table. Use
        # the public store boundary so stage restoration stays aligned with the
        # extraction schema.
        coverage <- store$coverage(plan_id = ids$plan_id)
        return(shift_stage_new(ShiftClimate, "climate",
            store_path = store_path, ids = ids,
            meta = c(stats::setNames(list(upstream), upstream_name), list(
                site = site,
                periods = periods,
                variables = as.character(unlist(meta$variables, use.names = FALSE)),
                coverage = coverage
            ))))
    }
    if (identical(stage, "morphed")) {
        climate <- shift__stage_from_ref(meta$climate)
        baseline <- if (is.null(meta$baseline)) {
            shift_target(climate)
        } else if (identical(as.character(meta$baseline$type), "site")) {
            shift__site_from_ref(meta$baseline$value)
        } else {
            as.character(meta$baseline$value)
        }
        return(shift_stage_new(ShiftMorphed, "morphed",
            store_path = store_path, ids = ids,
            meta = list(
                climate = climate,
                baseline = baseline,
                recipe = shift__recipe_from_ref(meta$recipe),
                reference_plan_id = unlist(meta$reference_plan_id,
                    use.names = FALSE),
                reference_periods = if (is.null(meta$reference_periods)) NULL else
                    shift__periods_from_input(meta$reference_periods)
            )))
    }
    if (identical(stage, "outputs")) {
        morphed <- shift__stage_from_ref(meta$morphed)
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        outputs <- shift_epw_output_rows_for_cases(store, ids$morph_id)
        if (!is.null(meta$exports)) {
            exports <- data.table::data.table(
                output_id = as.character(unlist(meta$exports$output_id,
                    use.names = FALSE)),
                export_path = as.character(unlist(meta$exports$export_path,
                    use.names = FALSE))
            )
            outputs <- merge(outputs, exports, by = "output_id", all.x = TRUE,
                sort = FALSE)
        }
        return(shift_stage_new(ShiftOutputs, "outputs",
            store_path = store_path, ids = ids,
            meta = list(
                morphed = morphed,
                format = as.character(shift_coalesce(meta$format, "epw")),
                outputs = outputs,
                paths = as.character(unlist(meta$paths, use.names = FALSE)),
                export_dir = if (is.null(meta$export_dir)) NULL else
                    as.character(meta$export_dir)
            )))
    }
    cli::cli_abort("Unsupported persisted shift stage: {.val {stage}}.")
}

# Resolve the Downloader session that owns an open standalone download step.
# The returned context joins the shift run identity to the existing persistent
# downloader manifest without duplicating its task or process tables.
shift__background_download_context <- function(store, run_id,
                                               active_only = TRUE) {
    checkmate::assert_flag(active_only)
    step <- shift__latest_step(store, run_id)
    if (!nrow(step) || !identical(step$task[[1L]], "download") ||
        (isTRUE(active_only) && !identical(step$status[[1L]], "running")) ||
        is.na(step$output_stage_json[[1L]]) ||
        !nzchar(step$output_stage_json[[1L]])) {
        return(NULL)
    }
    ref <- tryCatch(jsonlite::fromJSON(step$output_stage_json[[1L]],
        simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(ref)) return(NULL)
    stage <- shift__stage_from_ref(ref)
    session_id <- store__chr1(stage@ids$session_id)
    if (is.na(session_id) || !nzchar(session_id)) return(NULL)

    downloader <- store$downloader()
    sessions <- data.table::as.data.table(downloader$sessions())
    wanted_session_id <- session_id
    session <- sessions[sessions[["session_id"]] == wanted_session_id]
    jobs <- data.table::as.data.table(downloader$jobs())
    if ("session_id" %in% names(jobs)) {
        jobs <- jobs[jobs[["session_id"]] == wanted_session_id]
    } else {
        jobs <- jobs[0]
    }
    if (nrow(jobs) && "created_at" %in% names(jobs)) {
        jobs <- jobs[order(jobs[["created_at"]])]
    }
    list(step = step, stage = stage, session_id = session_id,
        session = session, jobs = jobs, downloader = downloader)
}

# Reconcile an existing Downloader process into the shared ShiftRun lifecycle.
# Polling is read-only while work is active; only terminal downloader states
# create shift step/run events, so watch refreshes do not become heartbeat spam.
shift__reconcile_background_download <- function(store, run_id) {
    context <- shift__background_download_context(store, run_id)
    if (is.null(context)) return(invisible(NULL))
    session_status <- if (nrow(context$session)) {
        as.character(context$session$status[[nrow(context$session)]])
    } else {
        NA_character_
    }
    job_status <- if (nrow(context$jobs)) {
        as.character(context$jobs$status[[nrow(context$jobs)]])
    } else {
        NA_character_
    }
    status <- if (job_status %in% c("error", "cancelled", "stale")) {
        job_status
    } else if (!is.na(session_status) && nzchar(session_status)) {
        session_status
    } else {
        job_status
    }
    if (is.na(status) || status %in% c("queued", "running", "downloading",
        "stopping")) {
        return(invisible(context))
    }

    step_id <- context$step$step_id[[1L]]
    details <- list(phase = "operation", stage = "download",
        step_id = step_id, session_id = context$session_id,
        downloader_status = status)
    if (identical(status, "done")) {
        # The detached downloader has released its manifest and output files;
        # synchronize those files before exposing the next-stage boundary.
        store$sync_downloads(context$downloader)
        shift__step_finish(store, step_id, "completed",
            output_stage = context$stage)
        shift__run_update(store, run_id, status = "waiting",
            current_stage = "download", last_error = NA_character_)
        shift__run_event(store, run_id, "download", "waiting",
            "Background download completed; ready for the next stage.",
            details = c(details, list(outcome = "completed")),
            step_id = step_id)
        return(invisible(context))
    }

    terminal_status <- if (identical(status, "cancelled")) {
        "cancelled"
    } else {
        "failed"
    }
    message <- if (identical(terminal_status, "cancelled")) {
        "Background download was cancelled."
    } else {
        sprintf("Background download failed with status %s.", status)
    }
    shift__step_finish(store, step_id, terminal_status,
        last_error = message)
    shift__run_finish(store, run_id, terminal_status,
        current_stage = "download", last_error = message)
    shift__run_event(store, run_id, "download", terminal_status, message,
        details = c(details, list(outcome = terminal_status)),
        step_id = step_id)
    invisible(context)
}

# Reconstruct a persisted plan for cross-session resume. A baseline EPW object
# without a path cannot be recovered and therefore fails with a targeted error.
shift__plan_from_spec <- function(spec, store = NULL) {
    site_spec <- spec$site
    if (is.null(site_spec$epw) || !nzchar(as.character(site_spec$epw))) {
        cli::cli_abort("This run cannot be resumed across sessions because its baseline EPW was not persisted as a file path.")
    }
    site <- shift_site(
        id = as.character(site_spec$id),
        lon = as.numeric(site_spec$lon),
        lat = as.numeric(site_spec$lat),
        label = if (is.null(site_spec$label)) NULL else as.character(site_spec$label),
        epw = as.character(site_spec$epw),
        metadata = shift_coalesce(site_spec$metadata, list())
    )
    recipe_spec <- spec$method$recipe
    recipe <- shift__recipe_from_ref(recipe_spec)
    reference <- shift__reference_from_spec(spec$method$reference)
    observed_reference <- shift__reference_from_spec(
        spec$method$observed_reference
    )
    method <- shift_morph_method(
        recipe,
        reference = reference,
        observed_reference = observed_reference
    )
    control <- do.call(shift_control, spec$control)
    climate <- shift__climate_from_spec(spec$climate)
    if (is.null(climate)) {
        request_spec <- spec$request
        request <- do.call(shift_request, list(
            provider = as.character(request_spec$provider),
            project = if (is.null(request_spec$project)) NULL else as.character(request_spec$project),
            source = if (is.null(request_spec$source)) NULL else as.character(request_spec$source),
            experiment = if (is.null(request_spec$experiment)) NULL else as.character(request_spec$experiment),
            variant = if (is.null(request_spec$variant)) NULL else as.character(request_spec$variant),
            variables = if (is.null(request_spec$variables)) NULL else as.character(request_spec$variables),
            frequency = if (is.null(request_spec$frequency)) NULL else as.character(request_spec$frequency),
            time = request_spec$time,
            filters = shift_coalesce(request_spec$filters, list()),
            options = shift_coalesce(request_spec$options, list())
        ))
    } else {
        # The persisted climate spec is authoritative; regenerate request fields
        # so model/scenario/member constraints cannot diverge during resume.
        request <- shift__request_from_cmip6(
            climate,
            shift__periods_from_input(spec$periods),
            method
        )
    }
    stage <- shift_coalesce(spec$stages, list())
    plan <- shift_plan(
        request = request,
        site = site,
        periods = spec$periods,
        store = shift_coalesce(store, spec$store),
        method = method,
        control = control,
        collect = shift_coalesce(stage$collect, list()),
        download = shift_coalesce(stage$download, list()),
        extract = shift_coalesce(stage$extract, list()),
        morph = shift_coalesce(stage$morph, list()),
        epw = shift_coalesce(stage$epw, list())
    )
    if (!is.null(climate)) {
        plan@meta$climate <- climate
    }
    plan@meta$epw_identity <- site_spec$identity
    plan
}

# Append one immutable run event for status displays and recovery diagnostics.
# Reporter callers may defer the sidecar snapshot until their paired heartbeat
# update so a single milestone does not rewrite the same live state twice.
shift__run_event <- function(store, run_id, stage, status, message = NA_character_,
                             details = NULL, snapshot = TRUE, step_id = NULL) {
    now <- store__now()
    row <- data.frame(
        event_id = store__hash(run_id, stage, status, now, stats::runif(1L)),
        run_id = run_id,
        step_id = store__chr1(step_id),
        stage = stage,
        status = status,
        message = store__chr1(message),
        details_json = if (is.null(details)) NA_character_ else shift__spec_json(details),
        created_at = now,
        stringsAsFactors = FALSE
    )
    morpher__private_store(store)$append_new_rows("shift_run_event", row, "event_id")
    if (isTRUE(snapshot)) {
        shift__live_snapshot_write(store, run_id)
    }
    invisible(row)
}

# Create one durable execution attempt for a run. Foreground attempts use the
# current PID; background attempts fill their PID when the worker starts.
shift__job_create <- function(store, run_id, mode = c("foreground", "process"),
                              ui = shift_ui(), step_id = NULL) {
    mode <- match.arg(mode)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    attempts <- jobs[jobs[["run_id"]] == wanted_run_id]$attempt
    attempt <- if (length(attempts)) max(attempts, na.rm = TRUE) + 1L else 1L
    now <- store__now()
    job_id <- paste0("shift-job-", substr(store__hash(run_id, attempt, now, stats::runif(1L)), 1L, 20L))
    log_dir <- file.path(store$path, "logs", "shift")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    row <- data.frame(
        job_id = job_id,
        run_id = run_id,
        step_id = store__chr1(step_id),
        attempt = as.integer(attempt),
        mode = mode,
        status = if (identical(mode, "process")) "queued" else "running",
        pid = if (identical(mode, "foreground")) as.integer(Sys.getpid()) else NA_integer_,
        hostname = unname(shift_coalesce(Sys.info()[["nodename"]], "localhost")),
        log_path = if (identical(mode, "process")) file.path(log_dir, paste0(job_id, ".log")) else NA_character_,
        ui_json = shift__spec_json(list(
            progress = ui@progress,
            detail = ui@detail,
            motion = ui@motion,
            refresh = ui@refresh,
            heartbeat = ui@heartbeat
        )),
        cancel_requested_at = as.POSIXct(NA, tz = "UTC"),
        started_at = if (identical(mode, "foreground")) now else as.POSIXct(NA, tz = "UTC"),
        heartbeat_at = if (identical(mode, "foreground")) now else as.POSIXct(NA, tz = "UTC"),
        completed_at = as.POSIXct(NA, tz = "UTC"),
        exit_code = NA_integer_,
        last_error = NA_character_,
        created_at = now,
        updated_at = now,
        stringsAsFactors = FALSE
    )
    # A resumed attempt owns a new cancellation boundary; remove any marker
    # left by the preceding failed or cancelled attempt before registering it.
    unlink(shift__live_path(store$path, run_id, suffix = "cancel.json"), force = TRUE)
    private$append_new_rows("shift_run_job", row, "job_id")
    shift__live_snapshot_write(store, run_id)
    row
}

# Replace a job row after a process/status transition while preserving the
# immutable run, attempt, and job identities.
shift__job_update <- function(store, job_id, ..., .snapshot = TRUE,
                              .ui_state = NULL) {
    wanted_job_id <- job_id
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    row <- jobs[jobs[["job_id"]] == wanted_job_id]
    if (!nrow(row)) {
        cli::cli_abort("Shift job {.val {job_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift job field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) {
        row[[name]] <- updates[[name]]
    }
    row$updated_at <- store__now()
    private$replace_rows("shift_run_job", as.data.frame(row), "job_id")
    if (isTRUE(.snapshot)) {
        shift__live_snapshot_write(store, row$run_id[[1L]],
            ui_state = .ui_state)
    }
    invisible(row)
}

# Update the worker heartbeat only at reporter callbacks and workflow
# boundaries; this is deliberately separate from transient Console animation.
shift__job_touch <- function(store, job_id, ui_state = NULL) {
    shift__job_update(store, job_id, heartbeat_at = store__now(),
        .ui_state = ui_state)
}

# Return all attempts for a run in deterministic attempt order.
shift__run_jobs <- function(store, run_id) {
    wanted_run_id <- run_id
    jobs <- morpher__private_store(store)$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == wanted_run_id]
    jobs[order(jobs[["attempt"]])]
}

# Read the most recent attempt, which is authoritative for cancellation,
# logging, and stale-process reconciliation.
shift__latest_job <- function(store, run_id) {
    jobs <- shift__run_jobs(store, run_id)
    if (!nrow(jobs)) jobs else jobs[which.max(jobs[["attempt"]])]
}

# Reconcile detached jobs when a worker exits before it can persist a terminal
# state, preventing background runs from appearing active forever.
shift__reconcile_run_job <- function(store, run_id, startup_grace = 60) {
    job <- shift__latest_job(store, run_id)
    if (!nrow(job) || !job$status[[1L]] %in% c("queued", "running", "stopping")) {
        return(invisible(job))
    }
    now <- store__now()
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    stale <- FALSE
    reason <- NA_character_
    if (is.na(pid)) {
        age <- as.numeric(difftime(now, job$created_at[[1L]], units = "secs"))
        stale <- identical(job$mode[[1L]], "process") && is.finite(age) && age > startup_grace
        if (stale) {
            reason <- sprintf("Background worker did not report a PID within %d seconds.", as.integer(startup_grace))
        }
    } else if (identical(job$mode[[1L]], "process") && !downloader__pid_alive(pid)) {
        stale <- TRUE
        reason <- sprintf("Background worker PID %d is not running.", pid)
    }
    if (!isTRUE(stale)) {
        return(invisible(job))
    }
    cancelled <- shift__cancel_request_exists(store$path, run_id, job$job_id[[1L]])
    if (isTRUE(cancelled)) {
        reason <- "Background worker stopped after cancellation was requested."
        shift__job_update(store, job$job_id[[1L]],
            status = "cancelled", completed_at = now, exit_code = 130L,
            last_error = reason)
        shift__run_finish(store, run_id, status = "cancelled",
            last_error = reason)
        shift__run_event(store, run_id, "worker", "cancelled", reason,
            details = list(job_id = job$job_id[[1L]], pid = pid, outcome = "cancelled"))
    } else {
        shift__job_update(store, job$job_id[[1L]],
            status = "stale", completed_at = now, exit_code = 1L,
            last_error = reason)
        shift__run_finish(store, run_id, status = "failed",
            last_error = reason)
        shift__run_event(store, run_id, "worker", "failed", reason,
            details = list(job_id = job$job_id[[1L]], pid = pid, outcome = "stale"))
    }
    invisible(shift__latest_job(store, run_id))
}

# Cooperative cancellation is checked at every stage and business-unit
# boundary so partial artifacts remain resumable and manifest-consistent.
shift__job_cancel_requested <- function(store, job_id) {
    wanted_job_id <- job_id
    jobs <- morpher__private_store(store)$read_table("shift_run_job")
    row <- jobs[jobs[["job_id"]] == wanted_job_id]
    nrow(row) && (
        !is.na(row$cancel_requested_at[[1L]]) ||
            row$status[[1L]] %in% c("stopping", "cancelled") ||
            shift__cancel_request_exists(store$path, row$run_id[[1L]], job_id)
    )
}

# Abort with a dedicated condition after persisting a user cancellation request.
shift__job_check_cancel <- function(store, run_id, job_id, stage) {
    if (!is.null(job_id) && shift__job_cancel_requested(store, job_id)) {
        cli::cli_abort(
            "Future EPW workflow run {.val {run_id}} was cancelled during {.val {stage}}.",
            class = "epwshiftr_shift_cancelled",
            run_id = run_id,
            job_id = job_id,
            stage = stage
        )
    }
    invisible(FALSE)
}

# Background workers can only reconstruct package-provided backends and
# JSON-safe plans; validate that boundary before a run or job is registered.
shift__validate_background_plan <- function(plan) {
    spec <- shift__plan_spec(plan)
    backend <- as.character(spec$method$recipe$backend)
    supported <- names(morpher__default_backend_specs())
    if (!backend %in% supported) {
        cli::cli_abort(c(
            "Background execution cannot reconstruct backend {.val {backend}} in a new R process.",
            "i" = "Run with {.code background = FALSE} or package the backend registration."
        ))
    }
    tryCatch(
        shift__plan_from_spec(spec, store = plan@store_path),
        error = function(e) {
            cli::cli_abort(c(
                "The shift plan cannot be reconstructed for background execution.",
                "x" = conditionMessage(e),
                "i" = "Run with {.code background = FALSE} for session-local inputs."
            ), parent = e)
        }
    )
    invisible(TRUE)
}

# Build the detached Rscript command without serializing live R objects into
# the child process; the durable run and job IDs are its only inputs.
shift__launch_job <- function(store_path, run_id, job_id, log_path) {
    launcher <- getOption("epwshiftr.shift.launcher", NULL)
    if (is.function(launcher)) {
        return(launcher(
            store_path = store_path,
            run_id = run_id,
            job_id = job_id,
            log_path = log_path
        ))
    }
    expr <- sprintf(
        "library(epwshiftr); epwshiftr:::shift__job_main(store_path = %s, run_id = %s, job_id = %s)",
        downloader__r_literal(store_path),
        downloader__r_literal(run_id),
        downloader__r_literal(job_id)
    )
    status <- tryCatch(
        system2(downloader__rscript(), c("-e", expr), stdout = log_path,
            stderr = log_path, wait = FALSE),
        error = function(e) e
    )
    if (inherits(status, "error")) {
        failed_store <- EsgStore$new(store_path, create = FALSE)
        on.exit(try(failed_store$close(), silent = TRUE), add = TRUE)
        shift__job_update(failed_store, job_id,
            status = "failed", completed_at = store__now(), exit_code = 1L,
            last_error = conditionMessage(status))
        shift__run_finish(failed_store, run_id,
            status = "failed", last_error = conditionMessage(status))
        cli::cli_abort("Failed to launch background shift job: {conditionMessage(status)}")
    }
    invisible(status)
}

# Open the worker manifest with bounded retries because an immediate status or
# cancel call may briefly own DuckDB between process launch and worker startup.
shift__job_store_open <- function(store_path, timeout = 60, interval = 0.1) {
    checkmate::assert_number(timeout, lower = 0, finite = TRUE)
    checkmate::assert_number(interval, lower = 0.01, finite = TRUE)
    started <- Sys.time()
    repeat {
        store <- tryCatch(EsgStore$new(store_path, create = FALSE),
            error = function(e) e)
        if (!inherits(store, "error")) {
            return(store)
        }
        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        if (!shift__manifest_locked(store) || elapsed >= timeout) {
            stop(store)
        }
        Sys.sleep(interval)
    }
}

# Execute one detached workflow attempt from persisted intent. The worker uses
# log mode because its stdout/stderr are redirected to the job log.
shift__job_main <- function(store_path, run_id, job_id) {
    store <- shift__job_store_open(store_path)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    wanted_run_id <- run_id
    wanted_job_id <- job_id
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    job <- jobs[jobs[["job_id"]] == wanted_job_id &
        jobs[["run_id"]] == wanted_run_id]
    if (!nrow(job)) {
        cli::cli_abort("Background shift job {.val {job_id}} was not found.")
    }
    ui_spec <- jsonlite::fromJSON(job$ui_json[[1L]], simplifyVector = TRUE)
    ui <- shift_ui(
        # Detached workers use stable logs for every visible mode, while an
        # explicit none setting remains completely quiet.
        progress = if (identical(as.character(ui_spec$progress), "none")) "none" else "log",
        detail = as.character(ui_spec$detail),
        motion = as.character(ui_spec$motion),
        refresh = as.numeric(ui_spec$refresh),
        heartbeat = as.numeric(ui_spec$heartbeat)
    )
    now <- store__now()
    shift__job_update(store, job_id,
        status = "running", pid = as.integer(Sys.getpid()),
        hostname = unname(shift_coalesce(Sys.info()[["nodename"]], "localhost")),
        started_at = now, heartbeat_at = now, last_error = NA_character_)
    shift__run_update(store, run_id, status = "running",
        completed_at = as.POSIXct(NA, tz = "UTC"), last_error = NA_character_)

    runs <- private$read_table("shift_run")
    row <- runs[runs[["run_id"]] == wanted_run_id]
    if (!nrow(row)) {
        cli::cli_abort("Background shift run {.val {run_id}} was not found.")
    }
    spec <- jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE)
    plan <- shift__plan_from_spec(spec, store = store_path)
    resolved <- row$resolved_spec_json[[1L]]
    if (!is.na(resolved) && nzchar(resolved)) {
        # Resume always reuses the first successful node/member/grid selection.
        plan@meta$resolved <- jsonlite::fromJSON(resolved, simplifyVector = TRUE)
    }
    reporter <- shift__reporter(ui, store = store, run_id = run_id,
        job_id = job_id, background = TRUE)
    reporter$run_started(plan, run_id, background = TRUE)
    shift__plan_run(plan, run_id = run_id, job_id = job_id,
        reporter = reporter, resume_existing = job$attempt[[1L]] > 1L)
    invisible(TRUE)
}

# Replace a run row after a state transition while leaving the original spec
# and unique run identity unchanged.
shift__run_update <- function(store, run_id, ...) {
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    rows <- private$read_table("shift_run")
    row <- rows[rows[["run_id"]] == wanted_run_id]
    if (!nrow(row)) {
        cli::cli_abort("Shift run {.val {run_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift run field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) {
        row[[name]] <- updates[[name]]
    }
    row$updated_at <- store__now()
    private$replace_rows("shift_run", as.data.frame(row), "run_id")
    shift__live_snapshot_write(store, run_id)
    invisible(row)
}

# Finish one run through a single terminal-state boundary so every completed,
# partial, failed, or cancelled row freezes its elapsed time consistently.
shift__run_finish <- function(store, run_id, status, ...) {
    checkmate::assert_choice(status,
        c("completed", "partial", "failed", "cancelled"))
    updates <- list(...)
    if ("completed_at" %in% names(updates)) {
        cli::cli_abort("`completed_at` is owned by `shift__run_finish()`.")
    }
    do.call(shift__run_update, c(
        list(store = store, run_id = run_id, status = status,
            completed_at = store__now()),
        updates
    ))
}

# Persist the current case matrix as the authoritative fulfilment contract for
# this run. Each run owns independent rows even when its spec hash is reused.
shift__run_cases_write <- function(store, run_id, cases) {
    private <- morpher__private_store(store)
    cases <- data.table::as.data.table(data.table::copy(cases))
    rows <- data.frame(
        run_case_id = vapply(cases$case_id, function(value) store__hash(run_id, value), character(1L)),
        run_id = run_id,
        case_id = cases$case_id,
        source_id = cases$source_id,
        experiment_id = cases$experiment_id,
        variant_label = cases$variant_label,
        grid_label = cases$grid_label,
        period = cases$period,
        years_json = vapply(cases$years, function(value) shift__spec_json(as.integer(value)), character(1L)),
        required = as.logical(cases$required),
        status = cases$status,
        output_id = cases$output_id,
        export_path = cases$export_path,
        missing_reason = cases$missing_reason,
        updated_at = store__now(),
        stringsAsFactors = FALSE
    )
    private$delete_by_key("shift_run_case", "run_id", run_id)
    private$append_new_rows("shift_run_case", rows, "run_case_id")
    shift__live_snapshot_write(store, run_id)
    invisible(cases)
}

# Register a unique persisted run before any remote or morphing side effects.
shift__run_register <- function(plan) {
    store <- shift_store(plan, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    spec <- shift__plan_spec(plan)
    spec_json <- shift__spec_json(spec)
    spec_hash <- store__hash(spec_json)
    now <- store__now()
    run_id <- paste0("run_", substr(store__hash(spec_hash, now, stats::runif(1L)), 1L, 24L))
    output_dir <- shift_coalesce(plan@meta$epw$export_dir, NA_character_)
    row <- data.frame(
        run_id = run_id,
        task = "future_epw",
        spec_hash = spec_hash,
        spec_json = spec_json,
        resolved_spec_json = NA_character_,
        status = "queued",
        current_stage = "planned",
        query_id = NA_character_,
        reference_query_id = NA_character_,
        plan_ids_json = NA_character_,
        reference_plan_ids_json = NA_character_,
        morph_id = NA_character_,
        output_dir = output_dir,
        package_version = as.character(utils::packageVersion("epwshiftr")),
        started_at = now,
        updated_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_,
        stringsAsFactors = FALSE
    )
    morpher__private_store(store)$append_new_rows("shift_run", row, "run_id")
    shift__run_cases_write(store, run_id, plan@meta$expected_cases)
    shift__run_event(store, run_id, "planned", "queued", "Workflow run registered.")
    run_id
}

# Register a generic stage run before its first side effect. UI preferences are
# intentionally absent from the spec so changing presentation never alters
# deterministic task identity.
shift__task_run_register <- function(store, task, spec = list(),
                                     status = c("queued", "waiting")) {
    status <- match.arg(status)
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_list(spec)
    spec <- utils::modifyList(list(version = 1L, task = task), spec)
    spec_json <- shift__spec_json(spec)
    spec_hash <- store__hash(spec_json)
    now <- store__now()
    run_id <- paste0("run_", substr(store__hash(
        spec_hash, now, stats::runif(1L)), 1L, 24L))
    row <- data.frame(
        run_id = run_id,
        task = task,
        spec_hash = spec_hash,
        spec_json = spec_json,
        resolved_spec_json = NA_character_,
        status = status,
        current_stage = if (identical(status, "waiting")) "waiting" else "planned",
        query_id = NA_character_,
        reference_query_id = NA_character_,
        plan_ids_json = NA_character_,
        reference_plan_ids_json = NA_character_,
        morph_id = NA_character_,
        output_dir = store__chr1(spec$output_dir),
        package_version = as.character(utils::packageVersion("epwshiftr")),
        started_at = now,
        updated_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_,
        stringsAsFactors = FALSE
    )
    morpher__private_store(store)$append_new_rows("shift_run", row, "run_id")
    shift__run_event(store, run_id, row$current_stage[[1L]], status,
        sprintf("%s task registered.", task))
    run_id
}

# Create one ordered task step under a run. The immutable input/spec fields are
# written before execution so even an early interrupt remains diagnosable.
shift__step_create <- function(store, run_id, task, spec,
                               input_stage = NULL, resumable = TRUE,
                               nonresumable_reason = NULL) {
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_flag(resumable)
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    steps <- private$read_table("shift_run_step")
    previous <- steps[steps[["run_id"]] == wanted_run_id]$ordinal
    ordinal <- if (length(previous)) max(previous, na.rm = TRUE) + 1L else 1L
    spec_json <- shift__spec_json(spec)
    now <- store__now()
    step_id <- paste0("step_", substr(store__hash(
        run_id, ordinal, spec_json), 1L, 24L))
    row <- data.frame(
        step_id = step_id,
        run_id = run_id,
        ordinal = as.integer(ordinal),
        task = task,
        spec_hash = store__hash(spec_json),
        spec_json = spec_json,
        input_stage_json = if (is.null(input_stage)) NA_character_ else
            shift__spec_json(shift__stage_ref(input_stage)),
        output_stage_json = NA_character_,
        status = "running",
        resumable = resumable,
        nonresumable_reason = store__chr1(nonresumable_reason),
        started_at = now,
        updated_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_,
        stringsAsFactors = FALSE
    )
    private$append_new_rows("shift_run_step", row, "step_id")
    row
}

# Replace mutable step state while preserving its stable task specification.
shift__step_update <- function(store, step_id, ...) {
    wanted_step_id <- step_id
    private <- morpher__private_store(store)
    steps <- private$read_table("shift_run_step")
    row <- steps[steps[["step_id"]] == wanted_step_id]
    if (!nrow(row)) {
        cli::cli_abort("Shift step {.val {step_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift step field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) row[[name]] <- updates[[name]]
    row$updated_at <- store__now()
    private$replace_rows("shift_run_step", as.data.frame(row), "step_id")
    shift__live_snapshot_write(store, row$run_id[[1L]])
    invisible(row)
}

# Close one step independently from its object-carried workflow run.
shift__step_finish <- function(store, step_id, status,
                               output_stage = NULL, last_error = NULL) {
    checkmate::assert_choice(status,
        c("completed", "partial", "failed", "cancelled"))
    shift__step_update(
        store,
        step_id,
        status = status,
        output_stage_json = if (is.null(output_stage)) NA_character_ else
            shift__spec_json(shift__stage_ref(output_stage)),
        completed_at = store__now(),
        last_error = store__chr1(last_error)
    )
}

# Return the latest step for resume, result reconstruction, and task-aware
# inspectors without assuming that every run is a Future EPW workflow.
shift__latest_step <- function(store, run_id, completed = FALSE) {
    wanted_run_id <- run_id
    steps <- morpher__private_store(store)$read_table("shift_run_step")
    steps <- steps[steps[["run_id"]] == wanted_run_id]
    if (isTRUE(completed)) {
        steps <- steps[steps[["status"]] %in% c("completed", "partial") &
            !is.na(steps[["output_stage_json"]])]
    }
    if (!nrow(steps)) steps else steps[which.max(steps[["ordinal"]])]
}

# Derive the terminal run outcome from every durable step rather than only the
# last artifact. A later successful morph or export must not hide an upstream
# partial extraction or download.
shift__run_completion_status <- function(store, run_id) {
    wanted_run_id <- run_id
    steps <- morpher__private_store(store)$read_table("shift_run_step")
    steps <- steps[steps[["run_id"]] == wanted_run_id]
    if (nrow(steps) && any(steps[["status"]] == "partial")) {
        "partial"
    } else {
        "completed"
    }
}

# Rebuild one actionable ShiftRun diagnostic from its persisted terminal event.
# Resolver coverage failures recommend changing intent, while transient and
# later-stage errors retain resume as the recovery action.
shift__run_event_diagnostic <- function(event, run_id, store_path) {
    details <- if (!is.null(event$details_json) &&
        length(event$details_json) && !is.na(event$details_json[[1L]]) &&
        nzchar(event$details_json[[1L]])) {
        tryCatch(jsonlite::fromJSON(event$details_json[[1L]],
            simplifyVector = TRUE), error = function(e) list())
    } else {
        list()
    }
    missing <- as.character(shift_coalesce(details$missing, character()))
    missing <- missing[!is.na(missing) & nzchar(missing)]
    message <- as.character(shift_coalesce(
        details$cause,
        shift_coalesce(details$error_summary,
            shift__error_summary(event$message[[1L]]))))[[1L]]
    if (length(missing)) {
        message <- paste0(message, " First missing requirement: ",
            missing[[1L]], ".")
    }
    recovery <- as.character(shift_coalesce(details$recovery, "retry"))[[1L]]
    action <- switch(recovery,
        change_request = paste(
            "Adjust the CMIP6 selection or reference before retrying;",
            "resuming unchanged will repeat this coverage failure."
        ),
        inspect = paste(
            "Inspect the per-node diagnostics; retry only after confirming",
            "that a transient node failure could change the result."
        ),
        sprintf("Run %s.", shift__run_command(
            "shift_resume", run_id, store_path))
    )
    shift_diagnostic(
        event$stage[[1L]],
        "error",
        if (identical(details$kind, "resolver_exhausted")) {
            "shift_resolver_exhausted"
        } else {
            "shift_run_error"
        },
        message,
        action = action
    )
}

# Materialize a lightweight ShiftRun handle from persisted tables.
shift__run_handle <- function(store, run_id, output_stage = NULL, plan = NULL) {
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    row <- runs[runs[["run_id"]] == wanted_run_id]
    if (!nrow(row)) {
        cli::cli_abort("Shift run {.val {run_id}} was not found in {.path {store$path}}.")
    }
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == wanted_run_id]
    if (nrow(cases)) {
        cases[, years := lapply(years_json, function(value) {
            as.integer(jsonlite::fromJSON(value, simplifyVector = TRUE))
        })]
    }
    events <- private$read_table("shift_run_event")
    events <- events[events[["run_id"]] == wanted_run_id][order(created_at)]
    jobs <- private$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == wanted_run_id]
    jobs <- jobs[order(jobs[["attempt"]])]
    steps <- private$read_table("shift_run_step")
    steps <- steps[steps[["run_id"]] == wanted_run_id]
    steps <- steps[order(steps[["ordinal"]])]
    errors <- events[status %in% c("failed", "error")]
    diagnostics <- if (!nrow(errors)) {
        shift_diagnostics_empty()
    } else {
        do.call(shift_bind_diagnostics, lapply(seq_len(nrow(errors)), function(i) {
            shift__run_event_diagnostic(errors[i], run_id, store$path)
        }))
    }
    shift_stage_new(
        ShiftRun,
        "run",
        store_path = store$path,
        ids = list(
            run_id = run_id,
            query_id = store__chr1(row$query_id[[1L]]),
            reference_query_id = store__chr1(row$reference_query_id[[1L]]),
            morph_id = store__chr1(row$morph_id[[1L]])
        ),
        meta = list(run = row[1L], cases = cases, events = events,
            jobs = jobs, steps = steps, output_stage = output_stage, plan = plan),
        diagnostics = diagnostics
    )
}

# Use atomic sidecar snapshots as the live read channel while a detached worker
# owns DuckDB's cross-process write lock. DuckDB remains the durable authority.
shift__live_path <- function(store_path, run_id, suffix = "live.json") {
    file.path(store_path, "logs", "shift", sprintf("%s.%s", run_id, suffix))
}

# Sidecar fallback is only valid for DuckDB's expected cross-process lock
# conflict; schema, corruption, and path errors must remain visible.
shift__manifest_locked <- function(error) {
    inherits(error, "error") && grepl(
        "Could not set lock|Conflicting lock",
        conditionMessage(error),
        ignore.case = TRUE
    )
}

# Serialize the latest run tables after each durable milestone. Keeping only a
# bounded event tail prevents frequent progress snapshots from growing without
# bound during large workflows.
shift__live_snapshot_write <- function(store, run_id, event_limit = 200L,
                                       ui_state = NULL) {
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    run <- runs[runs[["run_id"]] == wanted_run_id]
    if (!nrow(run)) {
        return(invisible(NULL))
    }
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == wanted_run_id]
    events <- private$read_table("shift_run_event")
    events <- events[events[["run_id"]] == wanted_run_id][order(created_at)]
    if (nrow(events) > event_limit) {
        events <- utils::tail(events, event_limit)
    }
    jobs <- private$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == wanted_run_id]
    jobs <- jobs[order(jobs[["attempt"]])]
    steps <- private$read_table("shift_run_step")
    steps <- steps[steps[["run_id"]] == wanted_run_id]
    steps <- steps[order(steps[["ordinal"]])]
    outputs <- data.table::data.table()
    morph_id <- store__chr1(run$morph_id[[1L]])
    if (!is.na(morph_id) && nzchar(morph_id)) {
        all_outputs <- private$read_table("epw_output")
        outputs <- all_outputs[all_outputs[["morph_id"]] == morph_id]
    }
    payload <- list(
        version = 1L,
        run_id = run_id,
        store_path = store$path,
        written_at = store__now(),
        run = as.data.frame(run),
        cases = as.data.frame(cases),
        events = as.data.frame(events),
        jobs = as.data.frame(jobs),
        steps = as.data.frame(steps),
        outputs = as.data.frame(outputs),
        ui_state = ui_state
    )
    store_write_json_atomic(
        payload,
        shift__live_path(store$path, run_id),
        auto_unbox = TRUE,
        dataframe = "rows",
        null = "null",
        na = "null",
        POSIXt = "ISO8601",
        digits = 15
    )
    invisible(payload)
}

# Normalize JSON rows back to data.table form and restore timestamp columns
# needed by status age calculations and watch rendering.
shift__live_table <- function(x) {
    if (is.null(x) || !length(x)) {
        return(data.table::data.table())
    }
    out <- data.table::as.data.table(x)
    time_columns <- intersect(
        c("started_at", "updated_at", "completed_at", "created_at",
          "heartbeat_at", "cancel_requested_at"),
        names(out)
    )
    for (name in time_columns) {
        out[[name]] <- as.POSIXct(out[[name]], tz = "UTC")
    }
    out
}

# Rebuild the same lightweight ShiftRun shape from a live sidecar when opening
# the manifest fails specifically because the background worker owns its lock.
shift__live_run_get <- function(run_id, store_path) {
    path <- shift__live_path(store_path, run_id)
    if (!file.exists(path)) {
        return(NULL)
    }
    snapshot <- tryCatch(
        jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = TRUE),
        error = function(e) NULL
    )
    if (is.null(snapshot) || !identical(as.character(snapshot$run_id), run_id)) {
        return(NULL)
    }
    row <- shift__live_table(snapshot$run)
    cases <- shift__live_table(snapshot$cases)
    events <- shift__live_table(snapshot$events)
    jobs <- shift__live_table(snapshot$jobs)
    steps <- shift__live_table(snapshot$steps)
    outputs <- shift__live_table(snapshot$outputs)
    ui_state <- shift_coalesce(snapshot$ui_state, list())
    if (!nrow(row)) {
        return(NULL)
    }
    if (nrow(cases) && "years_json" %in% names(cases)) {
        years <- lapply(cases$years_json, function(value) {
            as.integer(jsonlite::fromJSON(value, simplifyVector = TRUE))
        })
        data.table::set(cases, j = "years", value = years)
    }
    errors <- events[status %in% c("failed", "error")]
    diagnostics <- if (!nrow(errors)) {
        shift_diagnostics_empty()
    } else {
        do.call(shift_bind_diagnostics, lapply(seq_len(nrow(errors)), function(i) {
            shift__run_event_diagnostic(errors[i], run_id, store_path)
        }))
    }
    shift_stage_new(
        ShiftRun,
        "run",
        store_path = store_path,
        ids = list(
            run_id = run_id,
            query_id = store__chr1(row$query_id[[1L]]),
            reference_query_id = store__chr1(row$reference_query_id[[1L]]),
            morph_id = store__chr1(row$morph_id[[1L]])
        ),
        meta = list(run = row[1L], cases = cases, events = events,
            jobs = jobs, steps = steps, outputs = outputs,
            ui_state = ui_state, live = TRUE),
        diagnostics = diagnostics
    )
}

# Decide whether an atomic live snapshot is safe to serve without opening
# DuckDB. Dead PIDs and launch attempts older than the grace period fall back
# to manifest reconciliation so stale runs still become failed.
shift__live_process_is_active <- function(run, startup_grace = 60) {
    if (is.null(run) || !S7::S7_inherits(run, ShiftRun)) {
        return(FALSE)
    }
    status <- shift_status(run, refresh = FALSE)
    if (!status %in% c("queued", "running", "stopping")) {
        return(FALSE)
    }
    jobs <- data.table::as.data.table(run@meta$jobs)
    if (!nrow(jobs)) {
        return(FALSE)
    }
    job <- jobs[which.max(jobs[["attempt"]])]
    if (!identical(job$mode[[1L]], "process")) {
        return(FALSE)
    }
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    if (!is.na(pid)) {
        return(downloader__pid_alive(pid))
    }
    age <- as.numeric(difftime(Sys.time(), job$created_at[[1L]], units = "secs"))
    is.finite(age) && age <= startup_grace
}

# Persist a cooperative cancellation request outside DuckDB so a watcher can
# signal a worker even while the manifest is exclusively locked.
shift__cancel_request_write <- function(store_path, run_id, job_id, force = FALSE) {
    store_write_json_atomic(
        list(run_id = run_id, job_id = job_id, force = force,
            requested_at = format(store__now(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")),
        shift__live_path(store_path, run_id, suffix = "cancel.json"),
        auto_unbox = TRUE,
        null = "null"
    )
}

# Reflect cancellation in the lock-free snapshot immediately; the worker will
# subsequently persist the authoritative terminal state in DuckDB.
shift__live_cancel_mark <- function(store_path, run_id, job_id, status) {
    path <- shift__live_path(store_path, run_id)
    snapshot <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
        error = function(e) NULL)
    if (is.null(snapshot)) {
        return(NULL)
    }
    snapshot$run$status[[1L]] <- status
    snapshot$run$last_error[[1L]] <- "Cancellation requested by user."
    if (!is.null(snapshot$jobs) && nrow(snapshot$jobs)) {
        hit <- which(snapshot$jobs$job_id %in% job_id)
        if (length(hit)) {
            snapshot$jobs$status[hit] <- status
            snapshot$jobs$last_error[hit] <- "Cancellation requested by user."
            snapshot$jobs$cancel_requested_at[hit] <- format(
                store__now(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
            )
        }
    }
    snapshot$written_at <- format(store__now(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    store_write_json_atomic(
        snapshot, path,
        auto_unbox = TRUE, dataframe = "rows", null = "null", na = "null",
        POSIXt = "ISO8601", digits = 15
    )
    shift__live_run_get(run_id, store_path)
}

# Read only cancellation requests for the current attempt; stale markers from
# a previous attempt cannot cancel a resumed job.
shift__cancel_request_exists <- function(store_path, run_id, job_id) {
    path <- shift__live_path(store_path, run_id, suffix = "cancel.json")
    if (!file.exists(path)) {
        return(FALSE)
    }
    request <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
        error = function(e) NULL)
    !is.null(request) && identical(as.character(request$job_id), as.character(job_id))
}

# Match catalog identity fields while treating missing values as an explicit
# identity rather than relying on data.table's NA comparison behaviour.
shift__catalog_match <- function(x, value) {
    if (is.na(value)) {
        return(is.na(x))
    }
    !is.na(x) & as.character(x) == as.character(value)
}

# Fill absent ESGF File time fields from the CMIP/DRS filename carried in the
# catalog. This defensive resolver layer also repairs cached records created by
# older runs before File-level time enrichment was applied during collection.
shift__catalog_fill_time_ranges <- function(catalog) {
    catalog <- data.table::as.data.table(data.table::copy(catalog))
    if (!nrow(catalog)) {
        return(catalog)
    }
    n <- nrow(catalog)
    ranges <- query_result__fill_time_ranges(catalog, function() {
        labels <- query_result__character_column(catalog, "title", n)
        fallback <- query_result__character_column(catalog, "filename", n)
        labels[is.na(labels) | !nzchar(labels)] <-
            fallback[is.na(labels) | !nzchar(labels)]
        fallback <- query_result__character_column(catalog, "esgf_id", n)
        labels[is.na(labels) | !nzchar(labels)] <-
            fallback[is.na(labels) | !nzchar(labels)]
        labels
    })
    catalog[["datetime_start"]] <-
        query_result__time_iso(ranges$datetime_start)
    catalog[["datetime_end"]] <- query_result__time_iso(ranges$datetime_end)
    catalog[]
}

# Normalize catalog status fields before completeness checks. Superseded,
# retracted, and deprecated records never satisfy a workflow case.
shift__catalog_current <- function(catalog) {
    catalog <- shift__catalog_fill_time_ranges(catalog)
    identity <- c(
        "source_id", "experiment_id", "variant_label", "grid_label",
        "frequency", "table_id", "variable_id", "datetime_start", "datetime_end"
    )
    for (name in setdiff(identity, names(catalog))) {
        catalog[[name]] <- rep(NA_character_, nrow(catalog))
    }
    if ("latest" %in% names(catalog)) {
        catalog <- catalog[is.na(latest) | as.logical(latest)]
    }
    if ("retracted" %in% names(catalog)) {
        catalog <- catalog[is.na(retracted) | !as.logical(retracted)]
    }
    if ("deprecated" %in% names(catalog)) {
        catalog <- catalog[is.na(deprecated) | !as.logical(deprecated)]
    }
    catalog[]
}

# Expand the declared file time ranges to a year set so gaps between files do
# not pass a simple min/max coverage test.
shift__catalog_years <- function(rows) {
    if (!nrow(rows)) {
        return(integer())
    }
    years <- integer()
    for (i in seq_len(nrow(rows))) {
        start <- suppressWarnings(as.POSIXct(rows$datetime_start[[i]], tz = "UTC"))
        stop <- suppressWarnings(as.POSIXct(rows$datetime_end[[i]], tz = "UTC"))
        if (is.na(start) || is.na(stop)) {
            next
        }
        from <- as.integer(format(start, "%Y", tz = "UTC"))
        to <- as.integer(format(stop, "%Y", tz = "UTC"))
        years <- c(years, seq.int(min(from, to), max(from, to)))
    }
    sort(unique(years))
}

# Serialize selected table/grid/variable partitions as row-oriented JSON. The
# scalar representation is stable inside persisted run specs and avoids list
# columns whose one-row shape changes during jsonlite simplification.
shift__cmip6_partition_json <- function(partitions) {
    partitions <- data.table::as.data.table(data.table::copy(partitions))
    columns <- c("variable_id", "table_id", "grid_label", "required")
    for (name in setdiff(columns, names(partitions))) {
        partitions[[name]] <- if (identical(name, "required")) {
            logical(nrow(partitions))
        } else {
            character(nrow(partitions))
        }
    }
    partitions <- unique(partitions[, columns, with = FALSE])
    if (nrow(partitions)) {
        data.table::setorderv(partitions,
            c("table_id", "grid_label", "variable_id"))
    }
    # jsonlite marks its scalar result with class `json`; stripping that class
    # keeps complete and empty candidate tables type-compatible in rbindlist().
    as.character(jsonlite::toJSON(
        as.data.frame(partitions), dataframe = "rows",
        auto_unbox = TRUE, null = "null", na = "null"
    ))
}

# Restore a persisted partition map and normalize the zero/one-row cases to the
# same typed table used by fresh resolution.
shift__cmip6_partitions <- function(value) {
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) {
        return(data.table::data.table(
            variable_id = character(), table_id = character(),
            grid_label = character(), required = logical()
        ))
    }
    out <- jsonlite::fromJSON(value[[1L]], simplifyDataFrame = TRUE)
    out <- data.table::as.data.table(out)
    for (name in c("variable_id", "table_id", "grid_label")) {
        out[[name]] <- as.character(out[[name]])
    }
    out[["required"]] <- as.logical(out[["required"]])
    out[]
}

# Test one variable at one table/grid against every requested year for a single
# experiment. File ranges are expanded rather than inferred from min/max so a
# gap in the middle cannot satisfy the contract.
shift__cmip6_input_complete <- function(catalog, identity, experiment,
                                        variable, table, grid, years) {
    # ESGF providers may return convenience columns named `variable` and
    # `grid`. Local aliases prevent data.table from resolving those columns
    # instead of this helper's scalar arguments inside the row expression.
    wanted_source_id <- identity$source_id[[1L]]
    wanted_variant_label <- identity$variant_label[[1L]]
    wanted_experiment <- experiment
    wanted_variable <- variable
    wanted_table <- table
    wanted_grid <- grid
    files <- catalog[
        shift__catalog_match(source_id, wanted_source_id) &
            shift__catalog_match(variant_label, wanted_variant_label) &
            shift__catalog_match(experiment_id, wanted_experiment) &
            shift__catalog_match(variable_id, wanted_variable) &
            shift__catalog_match(table_id, wanted_table) &
            shift__catalog_match(grid_label, wanted_grid)
    ]
    nrow(files) > 0L && !length(setdiff(years, shift__catalog_years(files)))
}

# Expand the per-table grid choices for one model/member. Missing tables retain
# an explicit NA choice so the resolver can report the absent requirement
# instead of discarding the near-match identity entirely.
shift__cmip6_grid_combinations <- function(catalog, identity, tables,
                                            grid = NULL) {
    choices <- stats::setNames(vector("list", length(tables)), tables)
    for (table_id in tables) {
        wanted_table_id <- table_id
        values <- unique(catalog[
            shift__catalog_match(source_id, identity$source_id[[1L]]) &
                shift__catalog_match(variant_label, identity$variant_label[[1L]]) &
                shift__catalog_match(table_id, wanted_table_id)
        ]$grid_label)
        values <- sort(values[!is.na(values) & nzchar(values)])
        if (!is.null(grid)) {
            values <- intersect(values, grid)
            if (!length(values)) {
                values <- as.character(grid)
            }
        }
        if (!length(values)) {
            values <- NA_character_
        }
        choices[[table_id]] <- values
    }
    as.data.frame(do.call(expand.grid, c(
        choices,
        list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    )), check.names = FALSE, stringsAsFactors = FALSE)
}

# Compute complete model/member candidates while allowing each CMIP table to
# use its own grid. The selected partition JSON is subsequently authoritative
# for both download and extraction, so broad catalog queries cannot create
# table/grid cross-products downstream.
shift__cmip6_candidates <- function(catalog, models, experiments, variables,
                                    years, frequency, table = NULL,
                                    requirements = NULL, grid = NULL) {
    catalog <- shift__catalog_current(catalog)
    models <- as.character(models)
    experiments <- as.character(experiments)
    variables <- unique(as.character(variables))
    years <- sort(unique(as.integer(years)))
    wanted_frequency <- as.character(frequency)
    if (is.null(requirements)) {
        requirements <- stats::setNames(
            lapply(variables, function(variable) list(variable)),
            variables
        )
    }
    table_map <- shift__cmip6_variable_tables(variables, frequency, table)
    required_inputs <- unique(unlist(requirements, recursive = TRUE,
        use.names = FALSE))
    if (!all(required_inputs %in% names(table_map))) {
        cli::cli_abort("CMIP6 table mapping is missing one or more required recipe inputs.")
    }
    required_tables <- unique(unname(table_map[required_inputs]))
    wanted_tables <- unique(unname(table_map))
    catalog <- catalog[
        source_id %in% models &
            experiment_id %in% experiments &
            variable_id %in% variables &
            frequency %in% wanted_frequency &
            table_id %in% wanted_tables
    ]
    identities <- unique(catalog[, .(source_id, variant_label, frequency)])
    empty <- data.table::data.table(
        source_id = character(), variant_label = character(),
        grid_label = character(), frequency = character(),
        table_id = character(), required_partition_key = character(),
        requirement_key = character(), partition_key = character(),
        partitions_json = character(), required_native_grid = logical(),
        all_native_grid = logical(),
        complete = logical(), missing = character()
    )
    if (!nrow(identities)) {
        return(empty)
    }

    rows <- list()
    for (identity_index in seq_len(nrow(identities))) {
        identity <- identities[identity_index]
        combinations <- shift__cmip6_grid_combinations(
            catalog, identity, required_tables, grid = grid
        )
        for (combination_index in seq_len(nrow(combinations))) {
            grid_map <- stats::setNames(
                as.character(combinations[combination_index, , drop = TRUE]),
                names(combinations)
            )
            missing <- character()
            selected_sources <- list()

            # One alternative must work for every future scenario. This is the
            # whole-case source rule that prevents future/reference or
            # scenario-level mixing of HUSS and HURS.
            for (canonical in names(requirements)) {
                alternatives <- requirements[[canonical]]
                matched <- NULL
                for (alternative in alternatives) {
                    input_ok <- vapply(experiments, function(experiment) {
                        all(vapply(alternative, function(input) {
                            shift__cmip6_input_complete(
                                catalog, identity, experiment, input,
                                table_map[[input]], grid_map[[table_map[[input]]]],
                                years
                            )
                        }, logical(1L)))
                    }, logical(1L))
                    if (all(input_ok)) {
                        matched <- as.character(alternative)
                        break
                    }
                }
                if (is.null(matched)) {
                    labels <- vapply(alternatives, paste, character(1L),
                        collapse = "+")
                    for (experiment in experiments) {
                        missing <- c(missing, sprintf(
                            "%s/%s: requires %s", experiment, canonical,
                            paste(labels, collapse = " or ")
                        ))
                    }
                    matched <- as.character(alternatives[[1L]])
                }
                selected_sources[[canonical]] <- matched
            }

            required_variables <- unique(unlist(selected_sources,
                use.names = FALSE))
            required_partitions <- data.table::data.table(
                variable_id = required_variables,
                table_id = unname(table_map[required_variables]),
                grid_label = unname(vapply(
                    unname(table_map[required_variables]),
                    function(value) grid_map[[value]], character(1L)
                )),
                required = TRUE
            )

            optional_variables <- setdiff(variables, required_inputs)
            optional_partitions <- list()
            for (table_id in unique(unname(table_map[optional_variables]))) {
                wanted_table_id <- table_id
                table_variables <- optional_variables[
                    unname(table_map[optional_variables]) == table_id
                ]
                if (!length(table_variables)) {
                    next
                }
                if (table_id %in% names(grid_map)) {
                    optional_grids <- grid_map[[table_id]]
                } else {
                    optional_grids <- unique(catalog[
                        shift__catalog_match(source_id, identity$source_id[[1L]]) &
                            shift__catalog_match(variant_label, identity$variant_label[[1L]]) &
                            shift__catalog_match(table_id, wanted_table_id)
                    ]$grid_label)
                    optional_grids <- sort(optional_grids[
                        !is.na(optional_grids) & nzchar(optional_grids)
                    ])
                    if (!is.null(grid)) {
                        optional_grids <- intersect(optional_grids, grid)
                    }
                }
                if (!length(optional_grids) || all(is.na(optional_grids))) {
                    next
                }
                scored <- lapply(optional_grids, function(optional_grid) {
                    complete_variables <- table_variables[vapply(
                        table_variables,
                        function(variable) all(vapply(experiments,
                            function(experiment) {
                                shift__cmip6_input_complete(
                                    catalog, identity, experiment, variable,
                                    table_id, optional_grid, years
                                )
                            }, logical(1L))),
                        logical(1L)
                    )]
                    list(grid = optional_grid,
                        variables = complete_variables,
                        score = length(complete_variables))
                })
                scores <- vapply(scored, `[[`, integer(1L), "score")
                if (!length(scores) || max(scores) == 0L) {
                    next
                }
                scored <- scored[scores == max(scores)]
                primary_grid <- if (length(grid_map)) grid_map[[1L]] else NA_character_
                preferred <- vapply(scored, function(value) {
                    if (!is.na(primary_grid) && identical(value$grid, primary_grid)) {
                        return(1L)
                    }
                    if (identical(value$grid, "gn")) 2L else 3L
                }, integer(1L))
                chosen <- scored[[order(preferred,
                    vapply(scored, `[[`, character(1L), "grid"))[[1L]]]]
                optional_partitions[[length(optional_partitions) + 1L]] <-
                    data.table::data.table(
                        variable_id = chosen$variables,
                        table_id = table_id,
                        grid_label = chosen$grid,
                        required = FALSE
                    )
            }
            partitions <- data.table::rbindlist(
                c(list(required_partitions), optional_partitions),
                use.names = TRUE, fill = TRUE
            )
            partitions <- unique(partitions,
                by = c("variable_id", "table_id", "grid_label"))
            required_grid_rows <- unique(required_partitions[, .(
                table_id, grid_label
            )])
            data.table::setorderv(required_grid_rows,
                c("table_id", "grid_label"))
            required_partition_key <- paste(
                paste(required_grid_rows$table_id,
                    required_grid_rows$grid_label, sep = "="),
                collapse = ";"
            )
            all_grid_rows <- unique(partitions[, .(table_id, grid_label)])
            data.table::setorderv(all_grid_rows, c("table_id", "grid_label"))
            partition_key <- paste(
                paste(all_grid_rows$table_id, all_grid_rows$grid_label,
                    sep = "="),
                collapse = ";"
            )
            requirement_key <- paste(vapply(names(selected_sources),
                function(canonical) sprintf("%s=%s", canonical,
                    paste(selected_sources[[canonical]], collapse = "+")),
                character(1L)), collapse = ";")
            primary_table <- shift__cmip6_table_id(frequency)
            if (is.null(primary_table) || !primary_table %in% required_grid_rows$table_id) {
                primary_table <- required_grid_rows$table_id[[1L]]
            }
            primary_grid <- required_grid_rows[
                table_id == primary_table, grid_label
            ][[1L]]
            display_tables <- sort(unique(partitions$table_id))
            rows[[length(rows) + 1L]] <- data.table::data.table(
                source_id = identity$source_id[[1L]],
                variant_label = identity$variant_label[[1L]],
                grid_label = primary_grid,
                frequency = identity$frequency[[1L]],
                table_id = paste(display_tables, collapse = "+"),
                required_partition_key = required_partition_key,
                requirement_key = requirement_key,
                partition_key = partition_key,
                partitions_json = shift__cmip6_partition_json(partitions),
                required_native_grid = all(
                    !is.na(required_grid_rows$grid_label) &
                        required_grid_rows$grid_label == "gn"
                ),
                all_native_grid = all(!is.na(all_grid_rows$grid_label) &
                    all_grid_rows$grid_label == "gn"),
                complete = !length(missing),
                missing = if (length(missing)) {
                    paste(unique(missing), collapse = "; ")
                } else {
                    NA_character_
                }
            )
        }
    }
    if (!length(rows)) empty else
        data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Apply explicit selection constraints and the locked r1i1p1f1/gn preference;
# unresolved ties are structural ambiguities and must be shown to the user.
shift__choose_cmip6_candidates <- function(candidates, models, member = NULL,
                                           grid = NULL, diagnostic = NULL) {
    candidates <- candidates[complete %in% TRUE]
    if (!is.null(member)) {
        candidates <- candidates[variant_label %in% member]
    }
    if (!is.null(grid)) {
        candidates <- candidates[grid_label %in% grid]
    }
    selected <- list()
    for (model in models) {
        available <- candidates[source_id == model]
        if (!nrow(available)) {
            if (!is.null(diagnostic)) {
                diagnostic$reason <- "selection_incomplete"
                diagnostic$summary <- sprintf(
                    "No complete CMIP6 member/grid candidate satisfies the selection for model %s.",
                    model
                )
                explicit <- c(
                    if (!is.null(member)) paste("member", member),
                    if (!is.null(grid)) paste("grid", grid)
                )
                if (length(explicit)) {
                    diagnostic$missing <- c(
                        paste("explicit selection unavailable:",
                            paste(explicit, collapse = ", ")),
                        diagnostic$missing
                    )
                }
                shift__abort_cmip6_resolution(diagnostic)
            }
            cli::cli_abort(
                "No complete CMIP6 member/grid candidate was found for model {.val {model}}."
            )
        }
        if (!is.null(member)) {
            missing_members <- setdiff(member, unique(available$variant_label))
            if (length(missing_members)) {
                cli::cli_abort("Explicit member(s) are incomplete for model {.val {model}}: {.val {missing_members}}.")
            }
            common_partitions <- Reduce(
                intersect,
                lapply(member, function(value) unique(
                    available[variant_label == value]$required_partition_key
                ))
            )
            common_partitions <- common_partitions[
                !is.na(common_partitions) & nzchar(common_partitions)
            ]
            if (is.null(grid) && length(common_partitions) > 1L) {
                native <- common_partitions[vapply(common_partitions,
                    function(value) all(grepl("=gn$", strsplit(
                        value, ";", fixed = TRUE)[[1L]])), logical(1L))]
                if (length(native)) {
                    common_partitions <- native
                }
            }
            if (length(common_partitions) != 1L) {
                cli::cli_abort(
                    c(
                        "CMIP6 table/grid selection is ambiguous for model {.val {model}} and explicit member(s) {.val {member}}.",
                        "i" = "Candidate partitions: {.val {common_partitions}}. Set `grid` explicitly."
                    ),
                    class = "epwshiftr_shift_resolution_ambiguity"
                )
            }
            selected[[model]] <- available[
                variant_label %in% member &
                    required_partition_key == common_partitions[[1L]]
            ]
            next
        }

        if (any(available$variant_label %in% "r1i1p1f1")) {
            available <- available[variant_label == "r1i1p1f1"]
        }
        if (is.null(grid) && any(available$required_native_grid %in% TRUE)) {
            available <- available[required_native_grid %in% TRUE]
        }
        if ("case_count" %in% names(available) && nrow(available)) {
            available <- available[case_count == max(case_count)]
        }
        if (nrow(available) != 1L) {
            labels <- sprintf("%s/%s", available$variant_label,
                available$partition_key)
            cli::cli_abort(
                c(
                    "CMIP6 member/grid selection is ambiguous for model {.val {model}}.",
                    "i" = "Candidates: {.val {labels}}. Set `member` and/or `grid` explicitly."
                ),
                class = "epwshiftr_shift_resolution_ambiguity"
            )
        }
        selected[[model]] <- available
    }
    data.table::rbindlist(selected, use.names = TRUE, fill = TRUE)[, missing := NULL][]
}

# For partial-enabled runs, retain identities that cover at least one complete
# scenario and record how many requested scenarios each identity can fulfil.
shift__cmip6_partial_candidates <- function(catalog, models, experiments,
                                             variables, years, frequency, table,
                                             requirements = NULL, grid = NULL) {
    parts <- lapply(experiments, function(experiment) {
        rows <- shift__cmip6_candidates(
            catalog,
            models = models,
            experiments = experiment,
            variables = variables,
            years = years,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
        rows[, requested_experiment := experiment]
        rows
    })
    rows <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    if (!nrow(rows)) {
        return(rows)
    }
    rows[, .(
        complete = any(complete %in% TRUE),
        case_count = sum(complete %in% TRUE),
        missing = paste(stats::na.omit(missing), collapse = "; ")
    ), by = .(
        source_id, variant_label, grid_label, frequency, table_id,
        required_partition_key, requirement_key, partition_key,
        partitions_json, required_native_grid, all_native_grid
    )]
}

# Split the candidate contract into individual missing requirements while
# preserving the exact scenario/variable/year phrases produced by the resolver.
shift__cmip6_missing_items <- function(value) {
    value <- as.character(shift_coalesce(value, character()))
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) {
        return(character())
    }
    trimws(unlist(strsplit(value, ";", fixed = TRUE), use.names = FALSE))
}

# Build one structured explanation before complete candidate tables are
# filtered or intersected. This keeps the closest identity and exact missing
# requirements available to the terminal UI, persisted events, and callers.
shift__cmip6_resolution_diagnostic <- function(future, reference = NULL,
                                                models,
                                                reference_required = FALSE) {
    identity <- c(
        "source_id", "variant_label", "frequency",
        "required_partition_key", "requirement_key"
    )
    display <- c("grid_label", "table_id")
    future <- data.table::as.data.table(data.table::copy(future))
    for (name in setdiff(c(identity, display, "complete", "missing"), names(future))) {
        future[[name]] <- if (identical(name, "complete")) {
            logical(nrow(future))
        } else {
            rep(NA_character_, nrow(future))
        }
    }
    future <- future[, c(identity, display, "complete", "missing"),
        with = FALSE]
    data.table::setnames(future,
        c(display, "complete", "missing"),
        c("future_grid_label", "future_table_id",
            "future_complete", "future_missing"))

    if (isTRUE(reference_required)) {
        reference <- data.table::as.data.table(data.table::copy(reference))
        for (name in setdiff(c(identity, display, "complete", "missing"), names(reference))) {
            reference[[name]] <- if (identical(name, "complete")) {
                logical(nrow(reference))
            } else {
                rep(NA_character_, nrow(reference))
            }
        }
        reference <- reference[, c(identity, display, "complete", "missing"),
            with = FALSE]
        data.table::setnames(reference,
            c(display, "complete", "missing"),
            c("reference_grid_label", "reference_table_id",
                "reference_complete", "reference_missing"))
        combined <- merge(future, reference, by = identity, all = TRUE,
            sort = FALSE)
    } else {
        combined <- data.table::copy(future)
        combined[, `:=`(
            reference_grid_label = future_grid_label,
            reference_table_id = future_table_id,
            reference_complete = TRUE,
            reference_missing = NA_character_
        )]
    }

    future_complete <- sum(future$future_complete %in% TRUE)
    reference_complete <- if (isTRUE(reference_required)) {
        sum(reference$reference_complete %in% TRUE)
    } else {
        NA_integer_
    }
    shared_complete <- sum(
        combined$future_complete %in% TRUE &
            combined$reference_complete %in% TRUE
    )
    reason <- if (!future_complete) {
        "future_incomplete"
    } else if (isTRUE(reference_required) && !reference_complete) {
        "reference_incomplete"
    } else if (isTRUE(reference_required) && !shared_complete) {
        "no_shared_identity"
    } else {
        "selection_incomplete"
    }
    summary <- switch(reason,
        future_incomplete = paste(
            "No member/grid covers all requested future scenarios,",
            "variables, and years."
        ),
        reference_incomplete = paste(
            "No historical member/grid covers all reference variables",
            "and years."
        ),
        no_shared_identity = paste(
            "Future and historical catalogs have no complete member/grid",
            "identity in common."
        ),
        "No complete candidate satisfies the requested member/grid selection."
    )

    # Rank the most useful near-match from identities that actually exist in
    # the future catalog before comparing missing contract counts. A
    # reference-only identity must never appear closer merely because its
    # entire absent future side collapses to one generic diagnostic item.
    closest <- NULL
    missing <- character()
    if (nrow(combined)) {
        # Use explicit column access here because these temporary diagnostic
        # columns are local implementation details, not package-level
        # data.table symbols that should be registered as global variables.
        combined[["future_items"]] <- lapply(seq_len(nrow(combined)), function(i) {
            if (is.na(combined[["future_complete"]][[i]])) {
                "future: identity unavailable"
            } else if (isTRUE(combined[["future_complete"]][[i]])) {
                character()
            } else {
                paste0("future: ", shift__cmip6_missing_items(
                    combined[["future_missing"]][[i]]))
            }
        })
        combined[["reference_items"]] <- lapply(seq_len(nrow(combined)), function(i) {
            if (!isTRUE(reference_required)) {
                character()
            } else if (is.na(combined[["reference_complete"]][[i]])) {
                "reference: identity unavailable"
            } else if (isTRUE(combined[["reference_complete"]][[i]])) {
                character()
            } else {
                paste0("reference: ", shift__cmip6_missing_items(
                    combined[["reference_missing"]][[i]]))
            }
        })
        combined[["missing_count"]] <- lengths(combined[["future_items"]]) +
            lengths(combined[["reference_items"]])
        combined[["future_available"]] <-
            !is.na(combined[["future_complete"]])
        combined[["shared_available"]] <-
            combined[["future_available"]] &
            (!isTRUE(reference_required) |
                !is.na(combined[["reference_complete"]]))
        combined[["preferred_member"]] <-
            combined[["variant_label"]] %in% "r1i1p1f1"
        combined[["preferred_grid"]] <- vapply(
            combined[["required_partition_key"]], function(value) {
                if (is.na(value) || !nzchar(value)) {
                    return(FALSE)
                }
                all(grepl("=gn$", strsplit(value, ";", fixed = TRUE)[[1L]]))
            }, logical(1L))
        data.table::setorderv(
            combined,
            c("future_available", "shared_available", "missing_count",
                "preferred_member", "preferred_grid", "source_id",
                "variant_label", "required_partition_key"),
            order = c(-1L, -1L, 1L, -1L, -1L, 1L, 1L, 1L),
            na.last = TRUE
        )
        row <- combined[1L]
        missing <- c(row$future_items[[1L]], row$reference_items[[1L]])
        closest_grid <- row$future_grid_label[[1L]]
        if (is.na(closest_grid) || !nzchar(closest_grid)) {
            closest_grid <- row$reference_grid_label[[1L]]
        }
        closest_table <- row$future_table_id[[1L]]
        if (is.na(closest_table) || !nzchar(closest_table)) {
            closest_table <- row$reference_table_id[[1L]]
        }
        closest <- list(
            model = as.character(row$source_id[[1L]]),
            member = as.character(row$variant_label[[1L]]),
            grid = as.character(closest_grid),
            frequency = as.character(row$frequency[[1L]]),
            table = as.character(closest_table),
            partitions = as.character(row$required_partition_key[[1L]])
        )
    }
    list(
        kind = "coverage",
        reason = reason,
        summary = summary,
        models = as.character(models),
        future_complete_candidates = as.integer(future_complete),
        reference_complete_candidates = as.integer(reference_complete),
        shared_complete_candidates = as.integer(shared_complete),
        closest = closest,
        missing = missing
    )
}

# Raise a typed resolver condition whose concise message remains useful in log
# mode while its structured fields drive the final dashboard and recovery text.
shift__abort_cmip6_resolution <- function(diagnostic) {
    closest <- diagnostic$closest
    closest_label <- if (is.null(closest)) {
        "No near-match identity was available."
    } else {
        sprintf("Closest identity: %s/%s/%s.",
            shift_coalesce(closest$model, "?"),
            shift_coalesce(closest$member, "?"),
            shift_coalesce(closest$grid, "?"))
    }
    missing <- utils::head(diagnostic$missing, 3L)
    cli::cli_abort(
        c(
            diagnostic$summary,
            "i" = closest_label,
            if (length(missing)) c("x" = missing)
        ),
        class = c(
            "epwshiftr_shift_resolution_incomplete",
            "epwshiftr_shift_resolution_error"
        ),
        resolution = diagnostic,
        call = NULL
    )
}

# Intersect optional future/reference variables on their exact table and grid
# while retaining each side's required rows. This makes optional SND and
# extrema available only when both periods can support the same calculation.
shift__cmip6_shared_partitions <- function(future, reference) {
    future <- shift__cmip6_partitions(future)
    reference <- shift__cmip6_partitions(reference)
    keys <- c("variable_id", "table_id", "grid_label")
    future_required <- future[required %in% TRUE]
    reference_required <- reference[required %in% TRUE]
    shared_optional <- merge(
        future[required %in% FALSE], reference[required %in% FALSE],
        by = keys, all = FALSE, sort = FALSE
    )
    shared_optional <- if (nrow(shared_optional)) {
        shared_optional[, c(keys), with = FALSE][, required := FALSE]
    } else {
        future[0L]
    }
    list(
        future = unique(data.table::rbindlist(
            list(future_required, shared_optional),
            use.names = TRUE, fill = TRUE
        )),
        reference = unique(data.table::rbindlist(
            list(reference_required, shared_optional),
            use.names = TRUE, fill = TRUE
        ))
    )
}

# Recompute display fields after optional partitions have been intersected.
# Required partitions remain the selection identity; all partitions describe
# the exact files that download and extraction are allowed to consume.
shift__cmip6_partition_summary <- function(partitions, frequency) {
    partitions <- data.table::as.data.table(partitions)
    grids <- unique(partitions[, .(table_id, grid_label)])
    data.table::setorderv(grids, c("table_id", "grid_label"))
    required_grids <- unique(partitions[required %in% TRUE,
        .(table_id, grid_label)])
    data.table::setorderv(required_grids, c("table_id", "grid_label"))
    primary_table <- shift__cmip6_table_id(frequency)
    if (is.null(primary_table) || !primary_table %in% required_grids$table_id) {
        primary_table <- required_grids$table_id[[1L]]
    }
    list(
        grid_label = required_grids[table_id == primary_table,
            grid_label][[1L]],
        table_id = paste(sort(unique(partitions$table_id)), collapse = "+"),
        required_partition_key = paste(
            paste(required_grids$table_id, required_grids$grid_label,
                sep = "="), collapse = ";"
        ),
        partition_key = paste(
            paste(grids$table_id, grids$grid_label, sep = "="),
            collapse = ";"
        ),
        required_native_grid = all(required_grids$grid_label == "gn"),
        all_native_grid = all(grids$grid_label == "gn")
    )
}

# Resolve future and, only when explicitly requested by the method, historical
# catalogs against one shared model/member/frequency identity and a matching
# grid for every required table.
shift__resolve_cmip6_selection <- function(plan, future_catalog, reference_catalog = NULL) {
    meta <- plan@meta
    request <- meta$request@meta
    climate <- meta$climate
    models <- if (is.null(climate)) as.character(request$source) else climate@model
    scenarios <- if (is.null(climate)) as.character(request$experiment) else climate@scenarios
    requirements <- morpher__variable_requirements(meta$method@recipe)
    variables <- morpher__input_variables(meta$method@recipe)
    member <- if (is.null(climate)) request$variant else climate@member
    grid <- if (is.null(climate)) request$filters$grid_label else climate@grid
    frequency <- if (is.null(climate)) request$frequency else climate@frequency
    table <- if (is.null(climate)) request$filters$table_id else climate@table
    future <- if (isTRUE(meta$control@allow_partial)) {
        shift__cmip6_partial_candidates(
            future_catalog,
            models = models,
            experiments = scenarios,
            variables = variables,
            years = meta$periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
    } else {
        shift__cmip6_candidates(
            future_catalog,
            models = models,
            experiments = scenarios,
            variables = variables,
            years = meta$periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
    }

    reference <- meta$method@reference
    if (S7::S7_inherits(reference, ShiftReferenceSpec) && identical(reference@mode, "historical")) {
        # Monthly CMIP datasets usually end at a representative timestamp such
        # as December 16, not at the last second of the calendar year. An empty
        # reference result therefore needs its own diagnosis instead of being
        # collapsed into the later member/grid intersection error.
        if (is.null(reference_catalog) || !nrow(reference_catalog)) {
            year_range <- range(reference@periods$year)
            activity_label <- shift_coalesce(reference@activity, "<any activity>")
            frequency_label <- paste(shift_coalesce(frequency, "<any frequency>"),
                collapse = ", ")
            table_label <- if (is.null(climate)) {
                paste(shift_coalesce(table, "<any table>"), collapse = ", ")
            } else {
                paste(unique(unname(shift__cmip6_variable_tables(
                    variables, frequency, table
                ))), collapse = ", ")
            }
            cli::cli_abort(
                c(
                    "Historical reference catalog is empty for model(s) {.val {models}}.",
                    "x" = paste0(
                        "No File records matched experiment ", reference@experiment,
                        ", activity ", activity_label,
                        ", frequency ", frequency_label,
                        ", table ", table_label, "."
                    ),
                    "i" = sprintf(
                        "Requested reference years: %d\u2013%d.",
                        year_range[[1L]], year_range[[2L]]
                    )
                ),
                class = "epwshiftr_shift_reference_catalog_empty"
            )
        }
        historical_candidates <- shift__cmip6_candidates(
            reference_catalog,
            models = models,
            experiments = reference@experiment,
            variables = variables,
            years = reference@periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
        diagnostic <- shift__cmip6_resolution_diagnostic(
            future,
            reference = historical_candidates,
            models = models,
            reference_required = TRUE
        )
        if (!diagnostic$shared_complete_candidates) {
            shift__abort_cmip6_resolution(diagnostic)
        }
        identity <- c(
            "source_id", "variant_label", "frequency",
            "required_partition_key", "requirement_key"
        )
        historical <- historical_candidates[complete %in% TRUE,
            c(identity, "partitions_json"), with = FALSE]
        data.table::setnames(historical, "partitions_json",
            "reference_partitions_json")
        future <- merge(
            future[complete %in% TRUE],
            historical,
            by = identity,
            all = FALSE,
            sort = FALSE
        )
        if (nrow(future)) {
            for (i in seq_len(nrow(future))) {
                shared <- shift__cmip6_shared_partitions(
                    future$partitions_json[[i]],
                    future$reference_partitions_json[[i]]
                )
                future$partitions_json[[i]] <-
                    shift__cmip6_partition_json(shared$future)
                future$reference_partitions_json[[i]] <-
                    shift__cmip6_partition_json(shared$reference)
                summary <- shift__cmip6_partition_summary(
                    shared$future, future$frequency[[i]]
                )
                for (name in names(summary)) {
                    future[[name]][[i]] <- summary[[name]]
                }
            }
        }
    } else {
        diagnostic <- shift__cmip6_resolution_diagnostic(
            future,
            models = models,
            reference_required = FALSE
        )
        if (!diagnostic$shared_complete_candidates) {
            shift__abort_cmip6_resolution(diagnostic)
        }
        future[, reference_partitions_json := NA_character_]
    }
    selected <- shift__choose_cmip6_candidates(
        future,
        models,
        member = member,
        grid = grid,
        diagnostic = diagnostic
    )
    selected[, future_partitions_json := partitions_json]
    selected[]
}

# Clone a request with a specific index node while preserving every scientific
# filter and time constraint.
shift__request_at_node <- function(request, node) {
    meta <- request@meta
    options <- meta$options
    options$index_node <- node
    shift_request(
        provider = meta$provider,
        project = meta$project,
        source = meta$source,
        experiment = meta$experiment,
        variant = meta$variant,
        variables = meta$variables,
        frequency = meta$frequency,
        time = meta$time,
        filters = meta$filters,
        options = options
    )
}

# Build a historical request only for an explicit historical reference spec;
# manual plan and ShiftClimate references never reach this function.
shift__historical_request <- function(plan, node) {
    meta <- plan@meta
    reference <- meta$method@reference
    if (!S7::S7_inherits(reference, ShiftReferenceSpec) || !identical(reference@mode, "historical")) {
        return(NULL)
    }
    request <- meta$request@meta
    climate <- meta$climate
    member <- if (is.null(climate)) request$variant else climate@member
    grid <- if (is.null(climate)) request$filters$grid_label else climate@grid
    variables <- morpher__input_variables(meta$method@recipe)
    frequency <- if (is.null(climate)) request$frequency else climate@frequency
    tables <- if (is.null(climate)) {
        as.character(request$filters$table_id)
    } else {
        unique(unname(shift__cmip6_variable_tables(
            variables, frequency, climate@table
        )))
    }
    filters <- utils::modifyList(
        shift__compact_list(list(
            activity_id = reference@activity,
            table_id = tables,
            grid_label = grid,
            data_node = if (is.null(climate)) request$filters$data_node else climate@data_node
        )),
        reference@filters
    )
    shift_request(
        provider = request$provider,
        project = request$project,
        source = request$source,
        experiment = reference@experiment,
        variant = member,
        variables = variables,
        frequency = frequency,
        # Do not turn calendar-year intent into exact Dataset datetime bounds.
        # CMIP monthly metadata commonly ends on December 16, so requiring a
        # stop at December 31 incorrectly removes otherwise complete datasets.
        # Reference periods remain authoritative in candidate selection,
        # extraction planning, coverage checks, and the persisted method spec.
        time = NULL,
        filters = filters,
        # Keep exact reference dates out of the Dataset query, but use them to
        # select File records after filling missing ranges from DRS filenames.
        options = utils::modifyList(reference@options, list(
            index_node = node,
            time_filter_method = "auto",
            file_time = shift_periods_time(reference@periods)
        ))
    )
}

# Recreate a ShiftFiles stage from a pinned query ID during resume without
# contacting an ESGF node or changing the resolved member/grid choice.
shift__files_from_query <- function(store, request, query_id) {
    catalog <- shift_file_catalog(store, query_id)
    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(
            request = request,
            dataset_count = NA_integer_,
            datasets = NULL,
            file_count = nrow(catalog),
            variables = unique(catalog$variable_id),
            fields = SHIFT_WORKFLOW_FILE_FIELDS
        )
    )
}

# Render a stable node name inside messages. Debug renderers obtain the full URL
# from structured event details, avoiding duplicated label-plus-URL text.
shift__report_node <- function(reporter, node) {
    shift__node_label(node)
}

# Install a query callback only for the duration of one catalog collection.
# This keeps low-level EsgQuery APIs independent of workflow reporter classes.
shift__with_query_reporter <- function(reporter, node, phase, expr) {
    if (is.null(reporter)) {
        return(force(expr))
    }
    callback <- function(progress) {
        state <- shift_coalesce(progress$state, "transfer")
        reporter$heartbeat(
            "Waiting for catalog response",
            details = list(unit_type = "catalog", node = node,
                phase = "query", catalog_role = phase,
                transfer_state = state,
                bytes_done = shift_coalesce(progress$download, progress$downloaded))
        )
        invisible(TRUE)
    }
    old <- options(epwshiftr.query.progress_callback = callback)
    on.exit(options(old), add = TRUE)
    force(expr)
}

# Aggregate index-node failures into one domain-level diagnosis. Index nodes
# are fallback catalog mirrors, so repeated coverage rejections should become
# one count and one scientific explanation rather than duplicate errors.
shift__resolver_failure_diagnostic <- function(records) {
    records <- Filter(Negate(is.null), records)
    kinds <- vapply(records, function(record) record$kind, character(1L))
    # Count one or several normalized failure categories without repeatedly
    # exposing table mechanics throughout the aggregate constructor.
    count <- function(kind) sum(kinds %in% kind)
    structured <- Filter(function(record) !is.null(record$resolution), records)
    useful <- Filter(function(record) {
        !is.null(record$resolution$closest)
    }, structured)
    closest_record <- NULL
    if (length(useful)) {
        missing_counts <- vapply(useful, function(record) {
            length(shift_coalesce(record$resolution$missing, character()))
        }, integer(1L))
        closest_record <- useful[[which.min(missing_counts)]]
    } else if (length(structured)) {
        closest_record <- structured[[1L]]
    }
    closest <- if (is.null(closest_record)) NULL else
        closest_record$resolution$closest
    missing <- if (is.null(closest_record)) character() else
        as.character(shift_coalesce(
            closest_record$resolution$missing, character()))
    cause <- if (is.null(closest_record)) {
        "Every configured ESGF index node failed before a complete input set could be resolved."
    } else {
        as.character(closest_record$resolution$summary)[[1L]]
    }
    transient <- kinds %in% c("timeout", "network")
    all_transient <- length(transient) > 0L && all(transient)
    any_transient <- any(transient)
    recovery <- if (isTRUE(all_transient)) {
        "retry"
    } else if (count("coverage") > 0L && !isTRUE(any_transient)) {
        "change_request"
    } else {
        "inspect"
    }
    attempts <- lapply(records, function(record) {
        list(
            node = record$node,
            kind = record$kind,
            future_files = record$future_files,
            reference_files = record$reference_files
        )
    })
    list(
        kind = "resolver_exhausted",
        summary = "No ESGF index node resolved a complete CMIP6 input set.",
        cause = cause,
        nodes_checked = as.integer(length(records)),
        usable_nodes = 0L,
        coverage_failures = as.integer(count("coverage")),
        timeout_failures = as.integer(count("timeout")),
        network_failures = as.integer(count("network")),
        other_failures = as.integer(count("error")),
        # A single timed-out mirror does not make a mixed set of deterministic
        # coverage failures safely retryable. Recommend retry only when every
        # configured node failed for a transient transport reason.
        retryable = isTRUE(all_transient),
        recovery = recovery,
        closest = closest,
        missing = missing,
        attempts = attempts
    )
}

# Raise one typed exhaustion error after all fallback nodes have been tried.
# The compact message serves log mode while complete records remain attached
# for dashboard, watch, and programmatic diagnostics.
shift__abort_resolver_exhausted <- function(records) {
    diagnostic <- shift__resolver_failure_diagnostic(records)
    counts <- c(
        if (diagnostic$coverage_failures) sprintf(
            "%d incomplete", diagnostic$coverage_failures),
        if (diagnostic$timeout_failures) sprintf(
            "%d timed out", diagnostic$timeout_failures),
        if (diagnostic$network_failures) sprintf(
            "%d network errors", diagnostic$network_failures),
        if (diagnostic$other_failures) sprintf(
            "%d other errors", diagnostic$other_failures)
    )
    evidence <- sprintf("%d node%s checked%s.",
        diagnostic$nodes_checked,
        if (diagnostic$nodes_checked == 1L) "" else "s",
        if (length(counts)) paste0(": ", paste(counts, collapse = ", ")) else "")
    cli::cli_abort(
        c(
            diagnostic$summary,
            "x" = diagnostic$cause,
            "i" = evidence
        ),
        class = c(
            "epwshiftr_shift_resolver_exhausted",
            "epwshiftr_shift_resolution_error"
        ),
        resolution = diagnostic,
        call = NULL
    )
}

# Collect both catalogs from one index node and fail over in the declared order;
# catalogs from different nodes are never merged.
shift__collect_resolved_inputs <- function(plan, run_id, reporter = NULL,
                                           job_id = NULL) {
    store <- shift_store(plan, create = TRUE)
    wanted_run_id <- run_id
    run_row <- morpher__private_store(store)$read_table("shift_run")
    run_row <- run_row[run_row[["run_id"]] == wanted_run_id]
    resolved <- plan@meta$resolved
    if (!is.null(resolved) && nrow(run_row) && !is.na(run_row$query_id[[1L]])) {
        request <- shift__request_at_node(plan@meta$request, as.character(resolved$index_node))
        files <- shift__files_from_query(store, request, run_row$query_id[[1L]])
        reference_files <- NULL
        if (!is.na(run_row$reference_query_id[[1L]]) && nzchar(run_row$reference_query_id[[1L]])) {
            reference_request <- shift__historical_request(plan, as.character(resolved$index_node))
            reference_files <- shift__files_from_query(store, reference_request, run_row$reference_query_id[[1L]])
        }
        if (!is.null(reporter)) {
            pinned_selection <- data.table::as.data.table(resolved$selection)
            pinned_partitions <- shift_coalesce(
                shift__format_cmip6_partitions(pinned_selection),
                "partitions unavailable")
            reporter$unit_started(
                sprintf("Loading pinned future%s catalogs",
                    if (is.null(reference_files)) "" else " + reference"),
                current = 1L,
                total = 1L,
                details = list(
                    unit_type = "index_node",
                    node = as.character(resolved$index_node)
                )
            )
            reporter$unit_skipped(
                sprintf("Reused pinned selection \u00b7 %s \u00b7 future %d \u00b7 reference %d files",
                    pinned_partitions,
                    as.integer(files@meta$file_count),
                    if (is.null(reference_files)) 0L else {
                        as.integer(reference_files@meta$file_count)
                    }),
                current = 1L,
                total = 1L,
                details = list(
                    unit_type = "index_node",
                    node = as.character(resolved$index_node),
                    future_files = as.integer(files@meta$file_count),
                    reference_files = if (is.null(reference_files)) 0L else {
                        as.integer(reference_files@meta$file_count)
                    },
                    partitions = pinned_partitions,
                    result = sprintf("reused pinned selection \u00b7 %s",
                        pinned_partitions)
                )
            )
        }
        return(list(files = files, reference_files = reference_files, selection = data.table::as.data.table(resolved$selection), index_node = as.character(resolved$index_node)))
    }

    climate <- plan@meta$climate
    nodes <- if (is.null(climate)) plan@meta$request@meta$options$index_node else climate@index_nodes
    if (is.null(nodes) || !length(nodes)) {
        nodes <- INDEX_NODES[["ORNL"]]
    }
    fields <- unique(c(SHIFT_WORKFLOW_FILE_FIELDS, plan@meta$collect$fields))
    failures <- list()
    for (node_index in seq_along(nodes)) {
        node <- nodes[[node_index]]
        reference_request_for_node <- shift__historical_request(plan, node)
        catalog_roles <- if (is.null(reference_request_for_node)) {
            "future"
        } else {
            "future + reference"
        }
        node_future_files <- NA_integer_
        node_reference_files <- if (is.null(reference_request_for_node)) 0L else NA_integer_
        if (!is.null(reporter)) {
            reporter$check_cancel("resolve")
            reporter$unit_started(
                sprintf("Checking %s catalogs", catalog_roles),
                current = node_index,
                total = length(nodes),
                details = list(unit_type = "index_node", node = node)
            )
            reporter$notice("Collecting catalog",
                details = list(unit_type = "catalog", node = node,
                    catalog_role = "future"))
        }
        attempt <- tryCatch({
            request <- shift__request_at_node(plan@meta$request, node)
            collect_args <- utils::modifyList(
                list(store = store, fields = fields, all = TRUE, limit = FALSE,
                    label = "future-epw"),
                plan@meta$collect[setdiff(names(plan@meta$collect), "fields")]
            )
            files <- shift__with_query_reporter(
                reporter, node, "future",
                shift__do_call_with_reporter(reporter, shift_collect,
                    c(list(request), collect_args))
            )
            node_future_files <- as.integer(files@meta$file_count)
            if (!is.null(reporter)) {
                future_dataset_count <- as.integer(shift_coalesce(
                    files@meta$dataset_count, 0L))
                reporter$notice(sprintf(
                    "Found %d dataset(s), %d file(s)",
                    future_dataset_count, node_future_files),
                    outcome = "completed",
                    details = list(
                        unit_type = "catalog", node = node,
                        catalog_role = "future",
                        datasets = future_dataset_count,
                        files = node_future_files
                    ))
            }
            reference_request <- reference_request_for_node
            reference_files <- if (is.null(reference_request)) {
                NULL
            } else {
                if (!is.null(reporter)) {
                    reporter$notice("Collecting catalog",
                        details = list(unit_type = "catalog", node = node,
                            catalog_role = "reference"))
                }
                collected_reference <- shift__with_query_reporter(
                    reporter, node, "reference",
                    shift__do_call_with_reporter(reporter, shift_collect, c(
                            list(reference_request),
                            utils::modifyList(collect_args,
                                list(label = "historical-reference"))
                        ))
                )
                node_reference_files <- as.integer(collected_reference@meta$file_count)
                if (!is.null(reporter)) {
                    reference_dataset_count <- as.integer(shift_coalesce(
                        collected_reference@meta$dataset_count, 0L))
                    reporter$notice(sprintf(
                        "Found %d dataset(s), %d file(s)",
                        reference_dataset_count,
                        node_reference_files),
                        outcome = "completed",
                        details = list(
                            unit_type = "catalog", node = node,
                            catalog_role = "reference",
                            datasets = reference_dataset_count,
                            files = node_reference_files
                        ))
                }
                collected_reference
            }
            selection <- shift__resolve_cmip6_selection(
                plan,
                future_catalog = shift_file_catalog(store, files@ids$query_id),
                reference_catalog = if (is.null(reference_files)) NULL else shift_file_catalog(store, reference_files@ids$query_id)
            )
            list(files = files, reference_files = reference_files, selection = selection, index_node = node)
        }, error = function(e) e)
        if (!inherits(attempt, "error")) {
            if (!is.null(reporter)) {
                selected_members <- paste(unique(
                    attempt$selection$variant_label), collapse = ", ")
                selected_partitions <- shift_coalesce(
                    shift__format_cmip6_partitions(attempt$selection),
                    "partitions unavailable")
                selected_result <- sprintf("%s \u00b7 %s",
                    selected_members, selected_partitions)
                reporter$unit_completed(
                    sprintf("Selected member %s \u00b7 %s",
                        selected_members, selected_partitions),
                    current = node_index,
                    total = length(nodes),
                    outcome = "completed",
                    details = list(
                        unit_type = "index_node",
                        node = node,
                        future_files = node_future_files,
                        reference_files = node_reference_files,
                        member = unique(attempt$selection$variant_label),
                        partitions = selected_partitions,
                        result = selected_result
                    )
                )
            }
            return(attempt)
        }
        if (inherits(attempt, "epwshiftr_shift_resolution_ambiguity")) {
            stop(attempt)
        }
        resolution <- if (inherits(attempt,
            "epwshiftr_shift_resolution_error")) {
            attempt$resolution
        } else {
            NULL
        }
        error_kind <- if (is.null(resolution)) {
            shift__ui_error_kind(conditionMessage(attempt))
        } else {
            "coverage"
        }
        if (!is.null(reporter)) {
            reporter$unit_completed(
                sprintf("Rejected: %s",
                    shift__error_summary(conditionMessage(attempt))),
                current = node_index,
                total = length(nodes),
                # Rejection is an expected resolver decision while other
                # nodes remain. Only exhaustion of every candidate is a run
                # failure and therefore an error diagnostic.
                outcome = "rejected",
                details = list(unit_type = "index_node", node = node,
                    future_files = node_future_files,
                    reference_files = node_reference_files,
                    error_kind = error_kind,
                    error = conditionMessage(attempt),
                    resolution = resolution)
            )
        }
        failures[[length(failures) + 1L]] <- list(
            node = shift__node_label(node),
            kind = error_kind,
            message = conditionMessage(attempt),
            future_files = node_future_files,
            reference_files = node_reference_files,
            resolution = resolution
        )
    }
    shift__abort_resolver_exhausted(failures)
}

# Expand unresolved plan cases with the member/grid identities selected by the
# resolver and regenerate their stable case IDs.
shift__resolved_expected_cases <- function(plan, selection) {
    original <- plan@meta$expected_cases
    rows <- list()
    for (i in seq_len(nrow(original))) {
        case <- original[i]
        choices <- selection[source_id == case$source_id[[1L]]]
        if (!is.na(case$variant_label[[1L]])) {
            choices <- choices[variant_label == case$variant_label[[1L]]]
        }
        if (!is.na(case$grid_label[[1L]])) {
            choices <- choices[grid_label == case$grid_label[[1L]]]
        }
        for (j in seq_len(nrow(choices))) {
            row <- data.table::copy(case)
            row$variant_label <- choices$variant_label[[j]]
            row$grid_label <- choices$grid_label[[j]]
            row$case_id <- store__hash(
                row$source_id, row$experiment_id, row$variant_label,
                row$grid_label, row$period, row$years[[1L]]
            )
            rows[[length(rows) + 1L]] <- row
        }
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Expand persisted selection JSON into exact variable/table/grid rows and attach
# the model/member identity that owns each partition.
shift__selection_partition_rows <- function(selection,
                                             role = c("future", "reference")) {
    role <- match.arg(role)
    selection <- data.table::as.data.table(selection)
    field <- if (identical(role, "future")) {
        if ("future_partitions_json" %in% names(selection)) {
            "future_partitions_json"
        } else {
            "partitions_json"
        }
    } else {
        "reference_partitions_json"
    }
    if (!field %in% names(selection)) {
        cli::cli_abort("Resolved CMIP6 selection has no {role} partition map.")
    }
    rows <- list()
    for (i in seq_len(nrow(selection))) {
        partitions <- data.table::copy(
            shift__cmip6_partitions(selection[[field]][[i]])
        )
        if (!nrow(partitions)) {
            next
        }
        partitions[, `:=`(
            source_id = selection$source_id[[i]],
            variant_label = selection$variant_label[[i]],
            frequency = selection$frequency[[i]]
        )]
        rows[[length(rows) + 1L]] <- partitions
    }
    if (!length(rows)) {
        cli::cli_abort("Resolved CMIP6 selection contains no {role} partitions.")
    }
    unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

# Match File-result or catalog rows against the resolved partitions. Every
# facet is tested together, preventing a union of tables and grids from
# admitting combinations that the resolver never selected.
shift__partition_row_match <- function(rows, partitions, experiments) {
    rows <- data.table::as.data.table(rows)
    keep <- rep(FALSE, nrow(rows))
    for (i in seq_len(nrow(partitions))) {
        partition <- partitions[i]
        keep <- keep | (
            shift__catalog_match(rows$source_id,
                partition$source_id[[1L]]) &
            shift__catalog_match(rows$variant_label,
                partition$variant_label[[1L]]) &
            shift__catalog_match(rows$frequency,
                partition$frequency[[1L]]) &
            shift__catalog_match(rows$table_id,
                partition$table_id[[1L]]) &
            shift__catalog_match(rows$grid_label,
                partition$grid_label[[1L]]) &
            shift__catalog_match(rows$variable_id,
                partition$variable_id[[1L]]) &
            rows$experiment_id %in% experiments
        )
    }
    keep
}

# Create a stored child File result containing only resolved partitions. The
# downloader accepts this child query ID, so an explicit download cannot fetch
# unrelated table/grid combinations from the broad discovery query.
shift__files_for_partitions <- function(files, selection, experiments,
                                        role = c("future", "reference")) {
    role <- match.arg(role)
    partitions <- shift__selection_partition_rows(selection, role)
    store <- shift_store(files)
    result <- shift_stage_query_result(
        store, files@ids$query_id, result_type = "File"
    )
    selected <- result$filter(function(rows) {
        shift__partition_row_match(rows, partitions, experiments)
    })
    if (!selected$count()) {
        cli::cli_abort("Resolved {role} CMIP6 partitions contain no downloadable File records.")
    }
    query_id <- store$add_files(selected,
        label = sprintf("resolved-%s", role))
    selected_rows <- selected$to_data_table()
    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = files@store_path,
        ids = list(query_id = query_id),
        meta = list(
            request = files@meta$request,
            dataset_count = files@meta$dataset_count,
            datasets = NULL,
            file_count = selected$count(),
            variables = unique(as.character(selected_rows$variable_id)),
            fields = files@meta$fields,
            result_fields = selected$fields
        )
    )
}

# Combine independently planned extraction partitions into one climate stage.
# Coverage is re-read from the store for the union of plan IDs so resume and
# diagnostics use the same durable view as an ordinary shift_extract() call.
shift__combine_climate_stages <- function(stages) {
    stages <- Filter(function(stage) S7::S7_inherits(stage, ShiftClimate),
        stages)
    if (!length(stages)) {
        cli::cli_abort("No CMIP6 extraction partition produced a climate stage.")
    }
    if (length(stages) == 1L) {
        return(stages[[1L]])
    }
    first <- stages[[1L]]
    plan_id <- unique(unlist(lapply(stages,
        function(stage) stage@ids$plan_id), use.names = FALSE))
    query_id <- unique(unlist(lapply(stages,
        function(stage) stage@ids$query_id), use.names = FALSE))
    store <- shift_store(first)
    coverage <- store$coverage(plan_id = plan_id)
    bind_meta <- function(name) {
        values <- lapply(stages, function(stage) stage@meta[[name]])
        values <- Filter(is.data.frame, values)
        if (!length(values)) NULL else
            data.table::rbindlist(values, use.names = TRUE, fill = TRUE)
    }
    upstream_name <- if (S7::S7_inherits(first@meta$download, ShiftDownload)) {
        "download"
    } else {
        "files"
    }
    upstream <- first@meta[[upstream_name]]
    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = first@store_path,
        ids = list(query_id = query_id, plan_id = plan_id),
        meta = c(stats::setNames(list(upstream), upstream_name), list(
            site = first@meta$site,
            periods = first@meta$periods,
            variables = unique(unlist(lapply(stages,
                function(stage) stage@meta$variables), use.names = FALSE)),
            plan = bind_meta("plan"),
            processed = bind_meta("processed"),
            coverage = coverage
        )),
        diagnostics = shift_diagnostics_from_coverage(coverage)
    )
}

# Extract each exact source/member/table/grid partition separately and merge the
# resulting plan IDs only after planning. Selection facets are re-applied after
# user extraction overrides so workflow intent cannot be widened accidentally.
shift__extract_selected_partitions <- function(
    stage, selection, experiments, site, periods,
    role = c("future", "reference"), time = NULL,
    method = "nearest", fallback = "auto", overwrite = FALSE,
    resume = TRUE, overrides = list(), reporter = NULL
) {
    role <- match.arg(role)
    partitions <- shift__selection_partition_rows(selection, role)
    groups <- unique(partitions[, .(
        source_id, variant_label, frequency, table_id, grid_label
    )])
    stages <- vector("list", nrow(groups))
    custom_filters <- shift_coalesce(overrides$filters, list())
    overrides$filters <- NULL
    for (i in seq_len(nrow(groups))) {
        group <- groups[i]
        variables <- unique(partitions[
            shift__catalog_match(source_id, group$source_id[[1L]]) &
                shift__catalog_match(variant_label,
                    group$variant_label[[1L]]) &
                shift__catalog_match(frequency, group$frequency[[1L]]) &
                shift__catalog_match(table_id, group$table_id[[1L]]) &
                shift__catalog_match(grid_label, group$grid_label[[1L]]),
            variable_id
        ])
        exact_filters <- list(
            source_id = group$source_id[[1L]],
            experiment_id = experiments,
            variant_label = group$variant_label[[1L]],
            grid_label = group$grid_label[[1L]],
            frequency = group$frequency[[1L]],
            table_id = group$table_id[[1L]]
        )
        args <- utils::modifyList(list(
            site = site,
            periods = periods,
            variables = variables,
            time = time,
            filters = utils::modifyList(custom_filters, exact_filters),
            method = method,
            fallback = fallback,
            overwrite = overwrite,
            resume = resume
        ), overrides)
        # Re-pin scientific selection fields after generic overrides.
        args$site <- site
        args$periods <- periods
        args$variables <- variables
        args$filters <- utils::modifyList(custom_filters, exact_filters)
        args$overwrite <- overwrite
        args$resume <- resume
        if (identical(fallback, "error")) {
            args$fallback <- "error"
        }
        stages[[i]] <- shift__do_call_with_reporter(
            reporter, shift_extract, c(list(stage), args)
        )
    }
    shift__combine_climate_stages(stages)
}

# Build a compact, user-facing execution plan without touching remote services.
shift__plan_explain <- function(x) {
    meta <- x@meta
    request <- meta$request@meta
    epw <- meta$epw
    method <- meta$method
    reference <- method@reference
    reference_detail <- if (is.null(reference) &&
        isTRUE(morpher__recipe_accepts_reference(method@recipe))) {
        "baseline EPW"
    } else {
        "none"
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        reference_periods <- paste(
            sprintf(
                "%s=%s:%s",
                unique(reference@periods$period),
                vapply(unique(reference@periods$period), function(value) min(reference@periods$year[reference@periods$period == value]), integer(1L)),
                vapply(unique(reference@periods$period), function(value) max(reference@periods$year[reference@periods$period == value]), integer(1L))
            ),
            collapse = ", "
        )
        reference_detail <- sprintf(
            "%s; periods: %s%s",
            reference@mode,
            reference_periods,
            if (length(reference@match)) sprintf("; match: %s", paste(reference@match, collapse = ", ")) else ""
        )
    } else if (S7::S7_inherits(reference, ShiftClimate)) {
        reference_detail <- "supplied ShiftClimate"
    }
    observed_detail <- shift__format_reference(
        method@observed_reference
    )
    climate <- meta$climate
    member <- if (!is.null(climate)) climate@member else request$variant
    grid <- if (!is.null(climate)) climate@grid else request$filters$grid_label
    nodes <- if (!is.null(climate)) climate@index_nodes else request$options$index_node
    control <- meta$control
    data.table::data.table(
        step = c(
            "request",
            "method",
            "reference",
            "observed_reference",
            "cases",
            "selection",
            "index_nodes",
            "partial",
            "store",
            "output"
        ),
        detail = c(
            sprintf(
                "%s %s %s",
                shift_coalesce(request$project, "CMIP"),
                shift_coalesce(shift__display_values(request$source), "<any source>"),
                shift_coalesce(shift__display_values(request$experiment), "<any experiment>")
            ),
            method@name,
            reference_detail,
            observed_detail,
            sprintf("%d expected EPW output(s)", nrow(meta$expected_cases)),
            sprintf(
                "member=%s; grid=%s",
                shift_coalesce(shift__display_values(member), "<auto>"),
                shift_coalesce(shift__display_values(grid), "<auto>")
            ),
            shift_coalesce(shift__display_values(nodes), "<provider default>"),
            if (isTRUE(control@allow_partial)) "allow partial outputs" else "all requested cases required",
            shift__display_path(x@store_path),
            shift__display_path(shift_coalesce(epw$export_dir, epw$dir))
        )
    )
}

# Match extraction rows to one resolved CMIP identity without relying on
# data.table's NA comparison behaviour. The same helper is used for manifest
# coverage and Parquet data so a derived artifact cannot cross scenarios,
# members, grids, or sites.
shift__humidity_identity_match <- function(rows, identity, columns) {
    keep <- rep(TRUE, nrow(rows))
    for (column in intersect(columns, names(rows))) {
        keep <- keep & shift__catalog_match(
            rows[[column]], identity[[column]][[1L]]
        )
    }
    keep
}

# Persist canonical hurs extraction plans and Parquet artifacts when a resolved
# identity has no direct hurs but has complete huss, tas, and ps inputs. This
# occurs before task-level coverage, so strict coverage and EpwMorpher consume
# the same durable canonical evidence on initial and resumed runs.
shift__derive_hurs_climate <- function(climate, recipe, overwrite = FALSE,
                                       resume = TRUE, reporter = NULL) {
    if (!S7::S7_inherits(climate, ShiftClimate)) {
        cli::cli_abort("`climate` must be a {.cls ShiftClimate} stage.")
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    requirements <- morpher__variable_requirements(recipe)
    humidity_alternatives <- requirements[["hurs"]]
    if (is.null(humidity_alternatives) ||
        !any(vapply(humidity_alternatives, function(value) {
            identical(as.character(value), c("huss", "tas", "ps"))
        }, logical(1L)))) {
        return(climate)
    }

    store <- shift_store(climate)
    private <- priv(store)
    coverage <- store$coverage(plan_id = climate@ids$plan_id)
    coverage <- coverage[complete %in% TRUE]
    if (!nrow(coverage)) {
        return(climate)
    }
    identity_columns <- intersect(
        c("source_id", "experiment_id", "variant_label", "grid_label",
          "frequency", "table_id", "site_id"),
        names(coverage)
    )
    identities <- unique(coverage[, identity_columns, with = FALSE])
    raw <- NULL
    derived_ids <- character()
    provenance <- list()

    for (i in seq_len(nrow(identities))) {
        identity <- identities[i]
        rows <- coverage[
            shift__humidity_identity_match(coverage, identity,
                identity_columns)
        ]
        # Direct hurs is always preferred, even when the alternative source
        # variables were returned by the broad capability query.
        if (any(rows$variable_id == "hurs" & rows$complete %in% TRUE)) {
            next
        }
        inputs <- c("huss", "tas", "ps")
        source_rows <- lapply(inputs, function(variable) {
            rows[variable_id == variable & complete %in% TRUE]
        })
        # A zero-row data.table still has a non-zero length because `length()`
        # counts columns. Check rows so optional table partitions without the
        # three humidity inputs are skipped instead of being derived.
        if (!all(vapply(source_rows, nrow, integer(1L)) > 0L)) {
            next
        }
        source_plan_ids <- sort(unique(unlist(lapply(source_rows,
            function(value) value$plan_id), use.names = FALSE)))
        derived_plan_id <- store__hash(
            "derived-hurs-v1", paste(source_plan_ids, collapse = "\r")
        )
        existing <- tryCatch(store$coverage(plan_id = derived_plan_id),
            error = function(e) data.table::data.table())
        if (!isTRUE(overwrite) && isTRUE(resume) && nrow(existing) &&
            all(existing$complete %in% TRUE)) {
            derived_ids <- c(derived_ids, derived_plan_id)
            provenance[[length(provenance) + 1L]] <- list(
                plan_id = derived_plan_id,
                derived_from = inputs,
                source_plan_ids = source_plan_ids,
                reused = TRUE
            )
            if (!is.null(reporter)) {
                reporter$notice("Reused derived hurs from huss + tas + ps",
                    outcome = "skipped",
                    details = list(unit_type = "derived_variable",
                        variable = "hurs"))
            }
            next
        }

        if (is.null(raw)) {
            # Derivation must read every source partition. `shift_data()` is a
            # preview API by default and would otherwise stop after 100 rows,
            # often before tas and ps partitions are reached.
            raw <- shift_data(climate, n = Inf, variables = inputs)
        }
        data_rows <- raw[
            shift__humidity_identity_match(raw, identity, identity_columns)
        ]
        data_rows <- data_rows[plan_id %in% source_plan_ids]
        derived <- morpher__derive_hurs_rows(data_rows)
        if (!nrow(derived)) {
            cli::cli_abort(
                "Derived hurs produced no rows for the resolved CMIP identity.",
                class = "epwshiftr_hurs_derivation_error"
            )
        }

        huss_row <- source_rows[[1L]][1L]
        now <- store__now()
        plan <- data.frame(
            plan_id = derived_plan_id,
            query_id = huss_row$query_id[[1L]],
            file_key = huss_row$file_key[[1L]],
            site_id = huss_row$site_id[[1L]],
            variable_id = "hurs",
            lon = huss_row$lon[[1L]],
            lat = huss_row$lat[[1L]],
            method = huss_row$method[[1L]],
            time_start = min(derived$time, na.rm = TRUE),
            time_stop = max(derived$time, na.rm = TRUE),
            status = "done",
            available_time_count = data.table::uniqueN(derived$time),
            attempt_count = 1L,
            last_error = NA_character_,
            created_at = now,
            updated_at = now,
            stringsAsFactors = FALSE
        )
        file_catalog <- data.table::as.data.table(
            private$read_table("file_catalog")
        )
        file <- file_catalog[
            file_catalog[["file_key"]] == plan$file_key[[1L]]
        ][1L]
        if (!nrow(file)) {
            cli::cli_abort(
                "Cannot persist derived hurs because its source file catalog row is missing."
            )
        }
        derived[, `:=`(
            plan_id = derived_plan_id,
            file_key = plan$file_key[[1L]],
            query_id = plan$query_id[[1L]],
            method = plan$method[[1L]]
        )]

        # Write the plan before its result rows so a crash leaves an explicit,
        # resumable incomplete plan instead of an orphaned Parquet artifact.
        private$replace_rows("extraction_plan", plan, "plan_id")
        private$delete_by_key("extraction_result", "plan_id", derived_plan_id)
        results <- private$write_extract_partitions(
            derived, data.table::as.data.table(plan), file,
            overwrite = overwrite
        )
        private$replace_rows("extraction_result", as.data.frame(results),
            "result_id")
        derived_ids <- c(derived_ids, derived_plan_id)
        provenance[[length(provenance) + 1L]] <- list(
            plan_id = derived_plan_id,
            derived_from = inputs,
            source_plan_ids = source_plan_ids,
            equation = "e=q*p/(epsilon+(1-epsilon)*q); hurs=100*e/pws(tas)",
            reused = FALSE
        )
        if (!is.null(reporter)) {
            reporter$notice("Derived hurs from huss + tas + ps",
                outcome = "completed",
                details = list(unit_type = "derived_variable",
                    variable = "hurs", rows = nrow(derived)))
        }
    }

    if (!length(derived_ids)) {
        return(climate)
    }
    climate@ids$plan_id <- unique(c(climate@ids$plan_id, derived_ids))
    climate@meta$coverage <- store$coverage(plan_id = climate@ids$plan_id)
    climate@meta$variables <- unique(c(climate@meta$variables, "hurs"))
    climate@meta$derived_variables <- provenance
    climate
}

# Match one coverage table against the expected future cases and, when
# required, the corresponding explicit reference extraction.
shift__case_fulfilment <- function(cases, future_coverage, reference_coverage,
                                    required_variables, requires_reference,
                                    requirements = NULL) {
    cases <- data.table::as.data.table(data.table::copy(cases))
    future_coverage <- data.table::as.data.table(future_coverage)
    reference_coverage <- data.table::as.data.table(reference_coverage)
    coverage_columns <- c(
        "source_id", "experiment_id", "variant_label", "grid_label",
        "variable_id", "plan_id", "complete"
    )
    for (name in setdiff(coverage_columns, names(future_coverage))) {
        future_coverage[[name]] <- if (identical(name, "complete")) logical(nrow(future_coverage)) else character(nrow(future_coverage))
    }
    for (name in setdiff(coverage_columns, names(reference_coverage))) {
        reference_coverage[[name]] <- if (identical(name, "complete")) logical(nrow(reference_coverage)) else character(nrow(reference_coverage))
    }
    match_identity <- function(rows, case, include_experiment = TRUE) {
        keep <- shift__catalog_match(rows$source_id, case$source_id[[1L]]) &
            shift__catalog_match(rows$variant_label, case$variant_label[[1L]])
        # Grid is a per-table selection in enhanced workflows. Exact
        # partitions already restrict the climate stage, so case fulfilment is
        # intentionally keyed only by model/member (and future experiment).
        if (isTRUE(include_experiment)) {
            keep <- keep & shift__catalog_match(rows$experiment_id, case$experiment_id[[1L]])
        }
        rows[keep]
    }

    for (i in seq_len(nrow(cases))) {
        case <- cases[i]
        missing <- character()
        future <- match_identity(future_coverage, case, include_experiment = TRUE)
        for (variable in required_variables) {
            alternatives <- if (is.null(requirements[[variable]])) {
                list(variable)
            } else {
                requirements[[variable]]
            }
            complete_variables <- unique(future[complete %in% TRUE]$variable_id)
            if (!length(morpher__requirement_match(
                complete_variables, alternatives))) {
                missing <- c(missing, sprintf("future/%s", variable))
            }
        }
        if (isTRUE(requires_reference)) {
            reference <- match_identity(reference_coverage, case, include_experiment = FALSE)
            for (variable in required_variables) {
                alternatives <- if (is.null(requirements[[variable]])) {
                    list(variable)
                } else {
                    requirements[[variable]]
                }
                complete_variables <- unique(
                    reference[complete %in% TRUE]$variable_id
                )
                if (!length(morpher__requirement_match(
                    complete_variables, alternatives))) {
                    missing <- c(missing, sprintf("reference/%s", variable))
                }
            }
        }
        cases$status[[i]] <- if (length(missing)) "missing" else "ready"
        cases$missing_reason[[i]] <- if (length(missing)) paste(missing, collapse = ", ") else NA_character_
    }
    cases[]
}

# Restrict a ShiftClimate stage to the complete plans that belong to ready user
# cases while retaining the original extraction evidence in metadata.
shift__climate_for_cases <- function(climate, cases, reference = FALSE) {
    coverage <- shift_coverage(climate)
    ready <- cases[status == "ready"]
    keep <- logical(nrow(coverage))
    for (i in seq_len(nrow(ready))) {
        case <- ready[i]
        identity <- shift__catalog_match(coverage$source_id, case$source_id[[1L]]) &
            shift__catalog_match(coverage$variant_label, case$variant_label[[1L]])
        # Coverage may legitimately contain different grids for Amon and
        # LImon; the selected plan IDs, not one display grid, are authoritative.
        if (!isTRUE(reference)) {
            identity <- identity & shift__catalog_match(coverage$experiment_id, case$experiment_id[[1L]])
        }
        keep <- keep | identity
    }
    keep <- keep & coverage$complete %in% TRUE
    selected_ids <- unique(coverage$plan_id[keep])
    if (!length(selected_ids)) {
        cli::cli_abort("No complete extraction plans remain after applying the user case contract.")
    }
    climate@ids$plan_id <- selected_ids
    climate@meta$coverage <- coverage[plan_id %in% selected_ids]
    climate
}

# Attach output IDs and exported paths to the expected case matrix using the
# public CMIP identity while allowing one case to own a complete year sequence.
shift__complete_output_cases <- function(cases, outputs) {
    cases <- data.table::as.data.table(data.table::copy(cases))
    outputs <- data.table::as.data.table(outputs)
    for (i in seq_len(nrow(cases))) {
        if (!cases$status[[i]] %in% "ready") {
            next
        }
        hit <- outputs[
            source_id == cases$source_id[[i]] &
                experiment_id == cases$experiment_id[[i]] &
                variant_label == cases$variant_label[[i]] &
                period == cases$period[[i]]
        ]
        expected <- if (!nrow(hit) || !"member_count" %in% names(hit)) {
            1L
        } else {
            count <- unique(as.integer(hit$member_count))
            count <- count[!is.na(count)]
            if (length(count) == 1L) count else NA_integer_
        }
        member_keys <- if (nrow(hit)) {
            paste(
                hit$output_type,
                hit$sequence_id,
                hit$weather_year,
                sep = "\r"
            )
        } else {
            character()
        }
        if (nrow(hit) && !is.na(expected) && nrow(hit) == expected &&
            !anyDuplicated(member_keys)) {
            cases$status[[i]] <- "completed"
            # The case table retains its original scalar compatibility field;
            # all member IDs remain authoritative in the output manifest.
            cases$output_id[[i]] <- hit$output_id[[1L]]
            if ("export_path" %in% names(hit)) {
                cases$export_path[[i]] <- hit$export_path[[1L]]
            }
        } else {
            cases$status[[i]] <- "missing"
            cases$missing_reason[[i]] <- if (!nrow(hit)) {
                "final EPW was not produced"
            } else {
                "the expected future-weather sequence is incomplete"
            }
        }
    }
    cases[]
}

# Record a run stage transition before executing it so failures always point to
# the last durable workflow boundary.
shift__run_transition <- function(store, run_id, stage, message,
                                  reporter = NULL, current = NULL, total = NULL) {
    shift__run_update(store, run_id, status = "running", current_stage = stage, last_error = NA_character_)
    if (!is.null(reporter)) {
        reporter$stage_started(stage, message, current = current, total = total)
    } else {
        shift__run_event(store, run_id, stage, "running", message)
    }
    invisible(stage)
}

# Format copyable run commands without repeating the package's default store
# path. Non-default stores remain explicit so recovery never targets the wrong
# persisted run after a failure.
shift__run_command <- function(name, run_id, store_path, extra = NULL) {
    default_store <- store_normalize_path(store_dir(init = FALSE))
    actual_store <- store_normalize_path(store_path)
    arguments <- c(
        encodeString(run_id, quote = '"'),
        if (!identical(actual_store, default_store)) {
            sprintf("store = %s", encodeString(actual_store, quote = '"'))
        },
        extra
    )
    sprintf("%s(%s)", name, paste(arguments, collapse = ", "))
}

# Summarize structured resolver evidence in one scan-friendly line for the
# final cli condition; the committed dashboard retains the same source fields.
shift__resolution_evidence <- function(diagnostic) {
    if (is.null(diagnostic) || !length(diagnostic)) {
        return(character())
    }
    # Resolution conditions from custom or older workflow components may omit
    # aggregate node counters. Normalize them here so the presentation layer
    # never replaces the original scientific error with a formatting error.
    number <- function(name) {
        value <- suppressWarnings(as.integer(diagnostic[[name]]))
        if (!length(value) || is.na(value[[1L]])) 0L else value[[1L]]
    }
    counts <- c(
        if (number("coverage_failures") > 0L) sprintf(
            "%d incomplete", number("coverage_failures")),
        if (number("timeout_failures") > 0L) sprintf(
            "%d timed out", number("timeout_failures")),
        if (number("network_failures") > 0L) sprintf(
            "%d network errors", number("network_failures")),
        if (number("other_failures") > 0L) sprintf(
            "%d other errors", number("other_failures"))
    )
    evidence <- if (!is.null(diagnostic$nodes_checked)) {
        checked <- number("nodes_checked")
        sprintf("%d node%s checked%s.", checked,
            if (checked == 1L) "" else "s",
            if (length(counts)) paste0(": ", paste(counts, collapse = ", ")) else "")
    } else {
        character()
    }
    closest <- shift_coalesce(diagnostic$closest, list())
    identity <- c(closest$model, closest$member, closest$grid)
    identity <- as.character(identity[!vapply(identity, is.null, logical(1L))])
    identity <- identity[!is.na(identity) & nzchar(identity)]
    missing <- as.character(shift_coalesce(diagnostic$missing, character()))
    missing <- missing[!is.na(missing) & nzchar(missing)]
    c(
        evidence,
        if (length(identity)) sprintf("Closest identity: %s.",
            paste(identity, collapse = "/")),
        if (length(missing)) sprintf("First missing requirement: %s.",
            missing[[1L]])
    )
}

# Format the last business unit into a compact terminal diagnostic while the
# structured form remains available in shift_run_event$details_json.
shift__failure_context <- function(details, debug = FALSE) {
    if (is.null(details) || !length(details)) {
        return("")
    }
    fields <- c(
        node = "node",
        scenario = "scenario",
        variable = "variable",
        period = "period",
        access_method = "access",
        unit_label = "unit"
    )
    values <- vapply(names(fields), function(name) {
        value <- details[[name]]
        if (is.null(value) || !length(value) || is.na(value[[1L]]) || !nzchar(as.character(value[[1L]]))) {
            return(NA_character_)
        }
        shown <- as.character(value[[1L]])
        if (identical(name, "node") && !isTRUE(debug)) {
            shown <- shift__node_label(shown)
        }
        sprintf("%s=%s", fields[[name]], shown)
    }, character(1L))
    values <- unique(values[!is.na(values)])
    if (!length(values)) "" else paste0("Last activity: ", paste(values, collapse = ", "), ".")
}

# Reduce a nested cli/rlang message to the primary cause shown in the one
# user-facing failure block; the complete message remains persisted on the run.
shift__error_summary <- function(message) {
    message <- cli::ansi_strip(as.character(shift_coalesce(message, "Unknown error.")))
    lines <- trimws(unlist(strsplit(message, "[\r\n]+")))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) {
        return("Unknown error.")
    }
    sub("^[!xX][[:space:]]*", "", lines[[1L]])
}

# Build a meaningful interrupt condition after a foreground Ctrl-C so callers
# retain interrupt semantics without rethrowing cli's message-less condition.
shift__cancelled_interrupt <- function(message, run_id, store, stage) {
    structure(
        list(
            message = message,
            call = NULL,
            run_id = run_id,
            store = store,
            stage = stage
        ),
        class = c("epwshiftr_shift_cancelled", "interrupt", "condition")
    )
}

# Execute a persisted ShiftPlan through the existing stage primitives while
# enforcing task-level selection, coverage, and completion contracts.
shift__plan_run <- function(x, run_id, job_id = NULL, reporter = NULL,
                            resume_existing = FALSE, ...) {
    meta <- x@meta
    control <- meta$control
    store <- shift_store(x, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    overwrite <- isTRUE(control@overwrite)
    resume <- isTRUE(control@resume) || isTRUE(resume_existing)
    current_stage <- "planned"
    if (is.null(reporter)) {
        reporter <- shift__reporter(shift_ui("none"), store = store,
            run_id = run_id, job_id = job_id)
    }
    reference_expected <- S7::S7_inherits(meta$method@reference, ShiftReferenceSpec) &&
        identical(meta$method@reference@mode, "historical")
    stage_total <- 5L + as.integer(identical(control@download, "always")) +
        as.integer(reference_expected)
    stage_index <- 0L
    next_stage <- function(stage, message) {
        stage_index <<- stage_index + 1L
        shift__job_check_cancel(store, run_id, job_id, stage)
        shift__run_transition(store, run_id, stage, message,
            reporter = reporter, current = stage_index, total = stage_total)
    }
    # Reopen the elapsed-time clock for a resumed attempt while preserving the
    # original run start and all prior immutable scientific selections.
    shift__run_update(store, run_id, status = "running",
        completed_at = as.POSIXct(NA, tz = "UTC"), last_error = NA_character_)

    result <- tryCatch({
        current_stage <- next_stage("resolve", "Resolving complete CMIP6 workflow inputs.")
        resolved_inputs <- shift__collect_resolved_inputs(x, run_id,
            reporter = reporter, job_id = job_id)
        selection <- data.table::as.data.table(resolved_inputs$selection)
        selected_partitions <- shift__format_cmip6_partitions(selection)
        cases <- shift__resolved_expected_cases(x, selection)
        resolved <- list(
            index_node = resolved_inputs$index_node,
            selection = as.data.frame(selection),
            member = unique(selection$variant_label),
            grid = unique(selection$grid_label),
            partitions = selected_partitions
        )
        x@meta$resolved <- resolved
        future_query_id <- resolved_inputs$files@ids$query_id
        reference_query_id <- if (is.null(resolved_inputs$reference_files)) NA_character_ else resolved_inputs$reference_files@ids$query_id
        shift__run_update(
            store,
            run_id,
            resolved_spec_json = shift__spec_json(resolved),
            query_id = future_query_id,
            reference_query_id = reference_query_id
        )
        shift__run_cases_write(store, run_id, cases)
        reporter$cases_updated(cases)
        resolved_node_label <- shift__report_node(reporter,
            resolved_inputs$index_node)
        reporter$stage_completed(sprintf(
            "Resolved %s with member %s and partitions %s.",
            resolved_node_label,
            paste(unique(selection$variant_label), collapse = ", "),
            selected_partitions
        ), details = list(
            node = resolved_inputs$index_node,
            future_files = as.integer(resolved_inputs$files@meta$file_count),
            reference_files = if (is.null(resolved_inputs$reference_files)) {
                0L
            } else {
                as.integer(resolved_inputs$reference_files@meta$file_count)
            },
            member = unique(selection$variant_label),
            grid = unique(selection$grid_label),
            partitions = selected_partitions
        ))

        future_stage <- resolved_inputs$files
        reference_stage <- resolved_inputs$reference_files
        if (identical(control@download, "always")) {
            current_stage <- next_stage("download", "Downloading selected CMIP6 source files.")
            future_stage <- shift__files_for_partitions(
                future_stage, selection,
                experiments = if (is.null(meta$climate)) {
                    meta$request@meta$experiment
                } else {
                    meta$climate@scenarios
                },
                role = "future"
            )
            if (!is.null(reference_stage)) {
                reference_stage <- shift__files_for_partitions(
                    reference_stage, selection,
                    experiments = meta$method@reference@experiment,
                    role = "reference"
                )
            }
            download_args <- utils::modifyList(
                list(
                    run = TRUE,
                    background = FALSE,
                    resume = resume,
                    overwrite = overwrite,
                    # The workflow reporter owns presentation. Native downloader
                    # bars remain disabled while callbacks publish byte/file
                    # metrics into the shared fixed status region.
                    progress = FALSE
                ),
                meta$download
            )
            future_stage <- shift__do_call_with_reporter(reporter,
                shift_download, c(list(future_stage),
                    utils::modifyList(download_args,
                        list(session_label = "future"))))
            if (!is.null(reference_stage)) {
                reference_stage <- shift__do_call_with_reporter(reporter,
                    shift_download, c(list(reference_stage),
                        utils::modifyList(download_args,
                            list(session_label = "reference"))))
            }
            reporter$stage_completed("Downloaded selected CMIP6 source files.")
        }

        current_stage <- next_stage("extract_future", "Extracting future climate data.")
        future_experiments <- if (is.null(meta$climate)) {
            meta$request@meta$experiment
        } else {
            meta$climate@scenarios
        }
        fallback <- if (identical(control@download, "never")) "error" else "auto"
        extract_overrides <- meta$extract
        climate <- shift__extract_selected_partitions(
            future_stage,
            selection = selection,
            experiments = future_experiments,
            site = meta$site,
            periods = meta$periods,
            role = "future",
            time = NULL,
            method = control@extraction_method,
            fallback = fallback,
            overwrite = overwrite,
            resume = resume,
            overrides = extract_overrides,
            reporter = reporter
        )
        climate <- shift__derive_hurs_climate(
            climate,
            meta$method@recipe,
            overwrite = overwrite,
            resume = resume,
            reporter = reporter
        )
        future_coverage <- shift_coverage(climate)
        reporter$stage_completed(sprintf(
            "Extracted future climate: %d/%d plan(s) complete.",
            sum(future_coverage$complete %in% TRUE),
            nrow(future_coverage)
        ), details = list(
            plans_completed = sum(future_coverage$complete %in% TRUE),
            plans_total = nrow(future_coverage),
            variables = length(unique(future_coverage$variable_id))
        ))

        reference_climate <- NULL
        method_reference <- meta$method@reference
        if (!is.null(reference_stage)) {
            current_stage <- next_stage("extract_reference", "Extracting historical reference climate data.")
            reference_spec <- method_reference
            reference_climate <- shift__extract_selected_partitions(
                reference_stage,
                selection = selection,
                experiments = reference_spec@experiment,
                site = meta$site,
                periods = reference_spec@periods,
                role = "reference",
                time = shift_periods_time(reference_spec@periods),
                method = control@extraction_method,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume,
                overrides = reference_spec@extract,
                reporter = reporter
            )
            reference_climate <- shift__derive_hurs_climate(
                reference_climate,
                meta$method@recipe,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            )
            method_reference <- reference_climate
            extracted_reference_coverage <- shift_coverage(reference_climate)
            reporter$stage_completed(sprintf(
                "Extracted reference climate: %d/%d plan(s) complete.",
                sum(extracted_reference_coverage$complete %in% TRUE),
                nrow(extracted_reference_coverage)
            ), details = list(
                plans_completed = sum(extracted_reference_coverage$complete %in% TRUE),
                plans_total = nrow(extracted_reference_coverage),
                variables = length(unique(extracted_reference_coverage$variable_id))
            ))
        }

        current_stage <- next_stage("coverage", "Checking requested case and reference coverage.")
        reference_coverage <- if (!is.null(reference_climate)) {
            shift_coverage(reference_climate)
        } else if (S7::S7_inherits(method_reference, ShiftClimate)) {
            shift_coverage(method_reference)
        } else if (S7::S7_inherits(method_reference, ShiftReferenceSpec) && identical(method_reference@mode, "plan")) {
            store$coverage(plan_id = method_reference@plan_id)
        } else {
            data.table::data.table()
        }
        if (S7::S7_inherits(method_reference, ShiftClimate) &&
            is.null(reference_stage)) {
            # Manual ShiftClimate references receive the same canonical
            # derivation contract as automatically extracted historical data.
            method_reference <- shift__derive_hurs_climate(
                method_reference,
                meta$method@recipe,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            )
            reference_coverage <- shift_coverage(method_reference)
        }
        cases <- shift__case_fulfilment(
            cases,
            future_coverage = shift_coverage(climate),
            reference_coverage = reference_coverage,
            required_variables = epw_morph_variables(meta$method@recipe),
            requires_reference = !is.null(method_reference),
            requirements = morpher__variable_requirements(meta$method@recipe)
        )
        shift__run_cases_write(store, run_id, cases)
        ready <- cases[status == "ready"]
        missing <- cases[status == "missing"]
        if (nrow(missing)) {
            for (i in seq_len(nrow(missing))) {
                reporter$notice(
                    sprintf("Missing %s/%s: %s",
                        missing$experiment_id[[i]], missing$period[[i]],
                        missing$missing_reason[[i]]),
                    outcome = if (isTRUE(control@allow_partial)) "skipped" else "failed",
                    details = list(
                        unit_type = "future_epw_case",
                        scenario = missing$experiment_id[[i]],
                        period = missing$period[[i]],
                        outcome = if (isTRUE(control@allow_partial)) "skipped" else "failed"
                    )
                )
            }
        }
        if (!nrow(ready)) {
            cli::cli_abort("Zero requested future EPW cases have complete required climate inputs.")
        }
        if (nrow(missing) && !isTRUE(control@allow_partial)) {
            cli::cli_abort(c(
                "Not all requested future EPW cases are complete.",
                "x" = sprintf("%s/%s/%s: %s", missing$source_id, missing$experiment_id, missing$period, missing$missing_reason),
                "i" = "Set `allow_partial = TRUE` in shift_control() to process only complete cases."
            ))
        }
        reporter$stage_completed(sprintf(
            "Coverage ready for %d/%d requested case(s).",
            nrow(ready), nrow(cases)
        ), details = list(ready = nrow(ready), missing = nrow(missing)))
        reporter$cases_updated(cases, show = TRUE)
        climate <- shift__climate_for_cases(climate, cases, reference = FALSE)
        if (S7::S7_inherits(method_reference, ShiftClimate)) {
            method_reference <- shift__climate_for_cases(method_reference, cases, reference = TRUE)
        }
        shift__run_update(
            store,
            run_id,
            plan_ids_json = shift__spec_json(climate@ids$plan_id),
            reference_plan_ids_json = if (S7::S7_inherits(method_reference, ShiftClimate)) shift__spec_json(method_reference@ids$plan_id) else NA_character_
        )

        current_stage <- next_stage("morph", "Morphing all complete requested cases.")
        morph_args <- utils::modifyList(
            list(
                baseline = meta$site,
                recipe = meta$method@recipe,
                reference = method_reference,
                observed_reference =
                    meta$method@observed_reference,
                strict = control@strict,
                complete_only = TRUE,
                by = c("source_id", "experiment_id", "variant_label", "period"),
                overwrite = overwrite,
                resume = resume
            ),
            meta$morph
        )
        morphed <- shift__do_call_with_reporter(reporter, shift_morph,
            c(list(climate), morph_args))
        morph_id <- morphed@ids$morph_id
        shift__run_update(store, run_id, morph_id = morph_id)
        reporter$stage_completed(sprintf("Morphed %d requested case(s).", nrow(ready)))

        current_stage <- next_stage("write_epw", "Writing and exporting final EPW files.")
        epw_args <- utils::modifyList(
            list(
                dir = "outputs/future-epw",
                separate = identical(control@output_layout, "nested"),
                export_dir = NULL,
                overwrite = overwrite,
                resume = resume
            ),
            meta$epw
        )
        outputs_stage <- shift__do_call_with_reporter(reporter, shift_epw,
            c(list(morphed), epw_args))
        cases <- shift__complete_output_cases(cases, shift_outputs(outputs_stage))
        shift__run_cases_write(store, run_id, cases)
        reporter$cases_updated(cases,
            show = shift__ui_at_least(reporter$ui(), "detail"))
        output_count <- nrow(shift_outputs(outputs_stage))
        if (!output_count) {
            cli::cli_abort("The workflow produced zero final EPW files.")
        }
        reporter$stage_completed(sprintf("Wrote and exported %d EPW file(s).", output_count))
        final_status <- if (all(cases[required %in% TRUE]$status == "completed")) "completed" else "partial"
        shift__run_finish(
            store,
            run_id,
            status = final_status,
            current_stage = "completed",
            last_error = NA_character_
        )
        shift__run_event(store, run_id, "completed", final_status, sprintf("Produced %d final EPW file(s).", output_count))
        if (!is.null(job_id)) {
            shift__job_update(store, job_id,
                status = final_status, completed_at = store__now(),
                heartbeat_at = store__now(), exit_code = 0L,
                last_error = NA_character_)
        }
        run <- shift__run_handle(store, run_id, output_stage = outputs_stage, plan = x)
        reporter$run_completed(run, shift_outputs(run, refresh = FALSE))
        run
    }, interrupt = function(e) {
        requested <- !is.null(job_id) && tryCatch(
            shift__job_cancel_requested(store, job_id),
            error = function(err) FALSE
        )
        message <- if (isTRUE(requested)) {
            "Cancellation requested by user."
        } else {
            conditionMessage(e)
        }
        if (is.null(message) || !length(message) || is.na(message) || !nzchar(message)) {
            message <- "Interrupted by user."
        }
        failure_details <- utils::modifyList(reporter$context(),
            list(outcome = "cancelled"))
        try(shift__run_finish(store, run_id,
            status = "cancelled", current_stage = current_stage,
            last_error = message), silent = TRUE)
        try(shift__run_event(store, run_id, current_stage, "cancelled", message,
            details = failure_details), silent = TRUE)
        if (!is.null(job_id)) {
            try(shift__job_update(store, job_id,
                status = "cancelled", completed_at = store__now(),
                exit_code = 130L, last_error = message), silent = TRUE)
        }
        cancellation_context <- shift__failure_context(failure_details,
            debug = shift__ui_at_least(reporter$ui(), "debug"))
        reporter$run_failed(paste(
            sprintf("Future EPW run %s cancelled during %s.", run_id, current_stage),
            cancellation_context
        ), cancelled = TRUE)
        stop(shift__cancelled_interrupt(
            message, run_id = run_id, store = store$path, stage = current_stage
        ))
    }, error = function(e) {
        message <- conditionMessage(e)
        cancelled <- inherits(e, "epwshiftr_shift_cancelled")
        final_status <- if (isTRUE(cancelled)) "cancelled" else "failed"
        resolution <- if (inherits(e, "epwshiftr_shift_resolution_error")) {
            e$resolution
        } else {
            NULL
        }
        failure_summary <- if (is.null(resolution)) {
            shift__error_summary(message)
        } else {
            as.character(resolution$summary)[[1L]]
        }
        failure_details <- utils::modifyList(
            reporter$context(),
            c(list(outcome = final_status, error_summary = failure_summary),
                shift_coalesce(resolution, list()))
        )
        try(shift__run_finish(
            store,
            run_id,
            status = final_status,
            current_stage = current_stage,
            last_error = message
        ), silent = TRUE)
        try(shift__run_event(store, run_id, current_stage, final_status, message,
            details = failure_details), silent = TRUE)
        if (!is.null(job_id)) {
            try(shift__job_update(store, job_id,
                status = final_status, completed_at = store__now(),
                heartbeat_at = store__now(),
                exit_code = if (isTRUE(cancelled)) 130L else 1L,
                last_error = message), silent = TRUE)
        }
        reporter$run_failed(
            message = failure_summary,
            cancelled = cancelled,
            details = failure_details
        )
        if (isTRUE(cancelled)) {
            stop(e)
        }
        failure_context <- if (is.null(resolution)) {
            shift__failure_context(failure_details,
                debug = shift__ui_at_least(reporter$ui(), "debug"))
        } else {
            ""
        }
        evidence <- shift__resolution_evidence(resolution)
        get_command <- shift__run_command(
            "shift_run_get", run_id, store$path)
        inspect_command <- sprintf("shift_diagnostics(%s)", get_command)
        resume_command <- shift__run_command(
            "shift_resume", run_id, store$path)
        logs_command <- shift__run_command(
            "shift_logs", run_id, store$path, "tail = 20L")
        abort_message <- c(
                "Future EPW run {.val {run_id}} failed during {.val {current_stage}}.",
                "x" = paste0("Cause: ", if (is.null(resolution)) {
                    shift__error_summary(message)
                } else {
                    shift_coalesce(resolution$cause, resolution$summary)
                }),
                if (length(evidence)) stats::setNames(evidence,
                    rep("i", length(evidence))),
                if (nzchar(failure_context)) {
                    c("i" = failure_context)
                },
                if (!is.null(resolution) &&
                    identical(resolution$recovery, "change_request")) {
                    c("!" = paste(
                        "Resuming this request unchanged will repeat the",
                        "coverage failure. Adjust the climate selection or reference first."
                    ))
                },
                "i" = "Inspect: {.code {inspect_command}}",
                if (is.null(resolution) || isTRUE(resolution$retryable)) {
                    c("i" = "Retry: {.code {resume_command}}")
                },
                "i" = "Logs: {.code {logs_command}}"
            )
        cli::cli_abort(
            abort_message,
            class = "epwshiftr_shift_error",
            run_id = run_id,
            store = store$path,
            stage = current_stage,
            original_message = message,
            source_error = e,
            call = NULL
        )
    })
    result
}

# Compute the user-facing export path for one generated EPW row.
shift__export_target_path <- function(row, dir, separate = TRUE) {
    path <- row$path[[1L]]
    filename <- basename(path)
    if (isTRUE(separate)) {
        parts <- unlist(row[, intersect(c(
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "sequence_id",
            "weather_year"
        ), names(row)), with = FALSE], use.names = FALSE)
        parts <- morpher__safe_path(parts[!is.na(parts) & nzchar(parts)])
        return(do.call(file.path, as.list(c(dir, parts, filename))))
    }
    file.path(dir, filename)
}

# Copy registered EPW outputs to a user-facing directory and annotate the stage
# with absolute export paths.
shift__export_outputs <- function(x, dir, separate = TRUE, overwrite = FALSE,
                                  resume = TRUE, reporter = NULL) {
    dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
    outputs <- data.table::copy(shift_outputs(x))
    if (!nrow(outputs)) {
        return(x)
    }
    store <- shift_store(x)
    export_path <- character(nrow(outputs))
    for (i in seq_len(nrow(outputs))) {
        if (!is.null(reporter)) {
            reporter$check_cancel("write_epw")
            label <- sprintf("Exporting %s/%s/%s",
                outputs$experiment_id[[i]], outputs$variant_label[[i]], outputs$period[[i]])
            reporter$unit_started(label, current = i, total = nrow(outputs),
                details = list(
                    unit_type = "epw_export",
                    scenario = outputs$experiment_id[[i]],
                    period = outputs$period[[i]]
                ))
        }
        source <- store_abs_path(outputs$path[[i]], root = store$path)
        target <- shift__export_target_path(outputs[i], dir = dir, separate = separate)
        if (!file.exists(source)) {
            cli::cli_abort("Cannot export missing EPW output: {.path {source}}.")
        }
        if (file.exists(target) && !isTRUE(overwrite) && !isTRUE(resume)) {
            cli::cli_abort("Export target already exists: {.path {target}}.")
        }
        reused <- file.exists(target) && !isTRUE(overwrite) && isTRUE(resume)
        if (!file.exists(target) || isTRUE(overwrite)) {
            dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
            ok <- file.copy(source, target, overwrite = overwrite)
            if (!isTRUE(ok)) {
                cli::cli_abort("Failed to export EPW output to {.path {target}}.")
            }
        }
        export_path[[i]] <- normalizePath(target, winslash = "/", mustWork = TRUE)
        if (!is.null(reporter)) {
            if (isTRUE(reused)) {
                reporter$unit_skipped(sprintf("Reused export %s", basename(target)),
                    current = i, total = nrow(outputs),
                    details = list(export_path = export_path[[i]]))
            } else {
                reporter$unit_completed(sprintf("Exported %s", basename(target)),
                    current = i, total = nrow(outputs), outcome = "completed",
                    details = list(export_path = export_path[[i]]))
            }
        }
    }
    outputs[, export_path := export_path]
    x@meta$outputs <- outputs
    x@meta$export_dir <- dir
    x@meta$paths <- outputs$path
    x
}

S7::method(shift_morph, ShiftClimate) <- function(x, baseline = NULL, recipe = epw_morph_recipe("belcher"),
                                                  reference = NULL, reference_plan_id = NULL,
                                                  reference_periods = NULL,
                                                  observed_reference = NULL,
                                                  observed_plan_id = NULL,
                                                  observed_periods = NULL,
                                                  strict = TRUE, complete_only = TRUE,
                                                  by = c("source_id", "experiment_id", "variant_label", "period"),
                                                  overwrite = FALSE, resume = TRUE,
                                                  ui = NULL) {
    reporter <- shift__current_reporter()
    checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (!is.null(reference_periods)) {
        checkmate::assert_data_frame(reference_periods)
        checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
    }
    checkmate::assert_character(
        observed_plan_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (!is.null(observed_periods)) {
        checkmate::assert_data_frame(observed_periods)
        checkmate::assert_names(
            names(observed_periods),
            must.include = c("period", "year")
        )
    }
    checkmate::assert_flag(strict)
    checkmate::assert_flag(complete_only)
    checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    if (isTRUE(morpher__recipe_requires_reference(recipe)) &&
        is.null(reference) &&
        !shift_reference_has_legacy_args(reference_plan_id, reference_periods)) {
        cli::cli_abort(c(
            "The selected morphing recipe requires an explicit reference.",
            "i" = "Supply a {.cls ShiftReferenceSpec}, {.cls ShiftClimate}, or matching reference plan inputs."
        ))
    }
    if ((!is.null(reference) || shift_reference_has_legacy_args(reference_plan_id, reference_periods)) &&
        !isTRUE(morpher__recipe_accepts_reference(recipe))) {
        cli::cli_abort("The selected morphing recipe does not accept reference climate data.")
    }
    if (isTRUE(morpher__recipe_requires_observed_reference(recipe)) &&
        is.null(observed_reference) &&
        !shift_reference_has_legacy_args(
            observed_plan_id,
            observed_periods
        )) {
        cli::cli_abort(c(
            "The selected morphing recipe requires an explicit observed reference.",
            "i" = "Supply a plan-backed {.cls ShiftReferenceSpec}, {.cls ShiftClimate}, or matching observed plan inputs."
        ))
    }
    if ((!is.null(observed_reference) ||
        shift_reference_has_legacy_args(
            observed_plan_id,
            observed_periods
        )) &&
        !isTRUE(morpher__recipe_accepts_observed_reference(recipe))) {
        cli::cli_abort(
            "The selected morphing recipe does not accept observed reference data."
        )
    }

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
        resume = resume,
        reporter = reporter
    )
    observed_resolved <- shift__observed_reference_resolve(
        x = x,
        recipe = recipe,
        site = site,
        observed_reference = observed_reference,
        observed_plan_id = observed_plan_id,
        observed_periods = observed_periods,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )

    plan_selection <- shift_morph_complete_plan_selection(
        store,
        ids$plan_id,
        complete_only = complete_only,
        stage = "morph"
    )
    reference_selection <- shift_morph_complete_plan_selection(
        store,
        reference_resolved$plan_id,
        complete_only = complete_only,
        stage = "reference"
    )
    observed_selection <- shift_morph_complete_plan_selection(
        store,
        observed_resolved$plan_id,
        complete_only = complete_only,
        stage = "observed reference"
    )
    morpher <- epw_morpher(store, epw, site_id = site@id, recipe = recipe, label = site@label)
    workflow <- morpher$workflow(
        plan_id = plan_selection$plan_id,
        periods = periods,
        reference_plan_id = reference_selection$plan_id,
        reference_periods = reference_resolved$periods,
        observed_plan_id = observed_selection$plan_id,
        observed_periods = observed_resolved$periods,
        by = by,
        strict = strict,
        dir = NULL,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
    summary_id <- unique(workflow$climate$summary_id)[[1L]]
    baseline_id <- unique(workflow$baseline$baseline_id)[[1L]]
    morph_id <- unique(workflow$plan$morph_id)[[1L]]
    diagnostics <- shift_bind_diagnostics(
        plan_selection$diagnostics,
        reference_selection$diagnostics,
        observed_selection$diagnostics,
        shift_diagnostics_normalize(workflow$diagnostics)
    )

    shift_stage_new(
        ShiftMorphed,
        "morphed",
        store_path = x@store_path,
        ids = utils::modifyList(ids, list(
            plan_id = plan_selection$plan_id,
            summary_id = summary_id,
            baseline_id = baseline_id,
            morph_id = morph_id
        )),
        meta = list(
            climate = x,
            baseline = baseline,
            reference = reference_resolved$reference,
            reference_spec = reference_resolved$spec,
            reference_plan_id = reference_selection$plan_id,
            observed_reference = observed_resolved$reference,
            observed_reference_spec = observed_resolved$spec,
            observed_plan_id = observed_selection$plan_id,
            original_plan_id = ids$plan_id,
            original_reference_plan_id = reference_resolved$plan_id,
            original_observed_plan_id = observed_resolved$plan_id,
            complete_only = complete_only,
            reference_periods = reference_resolved$periods,
            observed_periods = observed_resolved$periods,
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

S7::method(shift_epw, ShiftMorphed) <- function(x, dir = NULL, separate = TRUE,
                                                export_dir = NULL, overwrite = FALSE,
                                                resume = TRUE, ui = NULL) {
    reporter <- shift__current_reporter()
    dir <- shift_coalesce(dir, "outputs/future-epw")
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_string(export_dir, min.chars = 1L, null.ok = TRUE)
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
        resume = resume,
        reporter = reporter
    )
    path_col <- intersect(c("path", "output_path", "relative_path"), names(outputs))
    paths <- if (length(path_col)) outputs[[path_col[[1L]]]] else character()

    stage <- shift_stage_new(
        ShiftOutputs,
        "outputs",
        store_path = x@store_path,
        ids = ids,
        meta = list(morphed = x, format = "epw", outputs = outputs, paths = paths, export_dir = export_dir),
        diagnostics = shift_diagnostics_empty()
    )
    if (!is.null(export_dir)) {
        stage <- shift_export_epw(stage, dir = export_dir, separate = separate,
            overwrite = overwrite, resume = resume)
    }
    stage
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

# Parse the shared console controls accepted by modern Shift object printers.
# Unknown arguments fail early so misspelled display options are not ignored.
shift__print_options <- function(dots, default_n = 10L) {
    if (is.null(names(dots))) {
        names(dots) <- rep("", length(dots))
    }
    unknown <- setdiff(names(dots), c("n", "width", "verbose"))
    unknown <- unknown[nzchar(unknown)]
    if (any(!nzchar(names(dots))) || length(unknown)) {
        supplied <- c(names(dots)[!nzchar(names(dots))], unknown)
        supplied[!nzchar(supplied)] <- "<unnamed>"
        cli::cli_abort("Unsupported print argument(s): {paste(supplied, collapse = ', ')}.")
    }

    n <- if ("n" %in% names(dots)) dots$n else default_n
    if (is.null(n)) {
        n <- Inf
    }
    checkmate::assert_number(n, lower = 1, finite = FALSE)
    if (!is.infinite(n)) {
        n <- as.integer(n)
    }
    width <- dots$width
    checkmate::assert_integerish(width, lower = 40L, len = 1L, null.ok = TRUE)
    verbose <- shift_coalesce(dots$verbose, FALSE)
    checkmate::assert_flag(verbose)
    list(n = n, width = if (is.null(width)) NULL else as.integer(width), verbose = verbose)
}

# Apply an explicit print width only for the duration of one object receipt.
shift__print_use_width <- function(width, env = parent.frame()) {
    if (is.null(width)) {
        return(invisible(NULL))
    }
    # `cli.width` takes precedence over base `width` in snapshot and redirected
    # output. Set both so an explicit print width remains authoritative in every
    # renderer, then restore the caller's complete option state on exit.
    old <- options(width = width, cli.width = width)
    withr::defer(options(old), envir = env)
    invisible(NULL)
}

# Format persisted timestamps whether DuckDB returns POSIXct or an ISO string.
shift__print_time <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NULL)
    }
    if (inherits(x[[1L]], "POSIXt")) {
        return(format(x[[1L]], tz = Sys.timezone(), usetz = TRUE))
    }
    as.character(x[[1L]])
}

# Apply the shared Shift receipt vocabulary on top of the established ESGF
# header renderer without changing the lower-level query/result presentation.
shift__print_header <- function(title) {
    esg__print_header(title)
}

# Render semantic Shift facts with the same bullet rhythm as ESGF receipts.
# Values are formatted by callers so scientific concepts remain class-aware.
shift__print_facts <- function(x) {
    esg__print_facts(x)
}

# Compress integer years into readable consecutive ranges so period specs do
# not expand into one console row per year.
shift__format_years <- function(years) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    if (!length(years)) {
        return(NULL)
    }
    groups <- cumsum(c(TRUE, diff(years) != 1L))
    ranges <- split(years, groups)
    paste(vapply(ranges, function(value) {
        if (length(value) == 1L) {
            as.character(value)
        } else {
            sprintf("%d\u2013%d", value[[1L]], value[[length(value)]])
        }
    }, character(1L)), collapse = ", ")
}

# Format normalized period tables and named year lists through one compact
# representation shared by plans, references, and extracted climate stages.
shift__format_periods <- function(periods) {
    if (is.null(periods)) {
        return(NULL)
    }
    if (is.list(periods) && !is.data.frame(periods)) {
        if (is.null(names(periods))) {
            return(shift__format_years(unlist(periods, use.names = FALSE)))
        }
        return(paste(vapply(names(periods), function(name) {
            sprintf("%s %s", name, shift__format_years(periods[[name]]))
        }, character(1L)), collapse = " \u00b7 "))
    }
    periods <- data.table::as.data.table(periods)
    if (!all(c("period", "year") %in% names(periods)) || !nrow(periods)) {
        return(NULL)
    }
    labels <- unique(as.character(periods$period))
    paste(vapply(labels, function(label) {
        sprintf("%s %s", label,
            shift__format_years(periods[period == label, year]))
    }, character(1L)), collapse = " \u00b7 ")
}

# Describe an optional workflow reference without exposing its full S7 object,
# extraction metadata, or one-row-per-year period table.
shift__format_reference <- function(reference, recipe = NULL) {
    if (is.null(reference)) {
        if (!is.null(recipe) &&
            isTRUE(morpher__recipe_accepts_reference(recipe))) {
            return("baseline EPW")
        }
        return("none")
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        periods <- shift__format_periods(reference@periods)
        parts <- c(reference@mode, periods)
        parts <- parts[!is.na(parts) & nzchar(parts)]
        return(paste(parts, collapse = " \u00b7 "))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied ShiftClimate")
    }
    class(reference)[[1L]]
}

# Display unresolved workflow selections explicitly instead of letting NULL
# disappear from a compact receipt.
shift__format_auto <- function(x) {
    shift_coalesce(shift__display_values(x), "auto")
}

# Format the public method name together with its persisted compatibility
# profile. Old Belcher specs did not carry a profile and must remain visibly
# legacy when they are rendered without first reconstructing the recipe.
shift__format_morph_method <- function(name, recipe = NULL,
                                       missing_belcher_profile = NULL) {
    name <- as.character(shift_coalesce(name, "method"))[[1L]]
    backend <- as.character(shift_coalesce(recipe$backend, name))[[1L]]
    profile <- recipe$profile
    if ((is.null(profile) || !length(profile)) &&
        backend %in% c("belcher", "belcher_absolute")) {
        profile <- missing_belcher_profile
    }
    if (is.null(profile) || !length(profile) || is.na(profile[[1L]]) ||
        !nzchar(as.character(profile[[1L]]))) {
        return(name)
    }
    sprintf("%s [%s]", name, as.character(profile[[1L]]))
}

# Describe scalar table forcing and named per-variable overrides distinctly.
# This makes the automatic Amon/LImon routing visible without expanding the
# complete recipe variable map in normal receipts.
shift__format_cmip6_tables <- function(table) {
    if (is.null(table) || !length(table)) {
        return("auto by variable")
    }
    if (is.list(table) && !is.data.frame(table)) {
        table <- unlist(table, use.names = TRUE)
    }
    table_names <- names(table)
    table <- as.character(table)
    names(table) <- table_names
    named <- !is.null(names(table)) && any(nzchar(names(table)))
    if (!named) {
        return(sprintf("%s (forced)",
            shift__display_values(table, max = Inf)))
    }
    overrides <- paste(sprintf("%s=%s", names(table), table),
        collapse = " \u00b7 ")
    sprintf("auto by variable \u00b7 %s", overrides)
}

# Render the exact table/grid partitions selected for download and extraction.
# `grid_label` remains a compatibility summary, while `partition_key` is the
# authoritative multi-table identity persisted by the resolver.
shift__format_cmip6_partitions <- function(selection) {
    selection <- data.table::as.data.table(shift_coalesce(selection,
        data.table::data.table()))
    if (!nrow(selection)) {
        return(NULL)
    }
    keys <- if ("partition_key" %in% names(selection)) {
        as.character(selection$partition_key)
    } else {
        character()
    }
    keys <- unique(keys[!is.na(keys) & nzchar(keys)])
    if (!length(keys) && all(c("table_id", "grid_label") %in%
        names(selection))) {
        rows <- unique(selection[, .(table_id, grid_label)])
        rows <- rows[!is.na(table_id) & nzchar(table_id) &
            !is.na(grid_label) & nzchar(grid_label)]
        if (nrow(rows)) {
            data.table::setorderv(rows, c("table_id", "grid_label"))
            keys <- paste(paste(rows$table_id, rows$grid_label, sep = "="),
                collapse = ";")
        }
    }
    if (!length(keys)) {
        return(NULL)
    }
    paste(gsub(";", " \u00b7 ", keys, fixed = TRUE), collapse = " / ")
}

# Format named provider or workflow option lists without printing nested
# environments or arbitrary objects by structure.
shift__format_options <- function(x) {
    if (is.null(x) || !length(x)) {
        return(NULL)
    }
    values <- vapply(names(x), function(name) {
        value <- x[[name]]
        if (is.atomic(value)) {
            sprintf("%s=%s", name,
                shift_coalesce(shift__display_values(value), "<empty>"))
        } else {
            sprintf("%s=<%s>", name, class(value)[[1L]])
        }
    }, character(1L))
    paste(values, collapse = " \u00b7 ")
}

# Read optional persisted data for a receipt and return a printable diagnostic
# rather than making print() fail when a store is temporarily unavailable.
shift__print_store_read <- function(x, reader) {
    opened <- tryCatch(shift_store(x), error = identity)
    if (inherits(opened, "condition")) {
        return(list(data = data.table::data.table(),
            error = conditionMessage(opened)))
    }
    on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    value <- tryCatch(reader(opened), error = identity)
    if (inherits(value, "condition")) {
        return(list(data = data.table::data.table(),
            error = conditionMessage(value)))
    }
    list(data = data.table::as.data.table(value), error = NULL)
}

# Render a bounded, width-aware table preview and preserve the total row count
# in the continuation hint even when only the requested rows were materialized.
shift__print_table <- function(x, title, columns, n = 10L,
                               total_rows = NULL, empty = "No rows.",
                               more_hint = "use the corresponding shift_*() inspector for all rows.") {
    checkmate::assert_string(title, min.chars = 1L)
    x <- data.table::as.data.table(shift_coalesce(x,
        data.table::data.table()))
    if (is.null(total_rows)) {
        total_rows <- nrow(x)
    }
    cli::cli_rule(title)
    if (!nrow(x)) {
        cli::cli_alert_info(empty)
        return(invisible(NULL))
    }
    shown <- if (is.infinite(n)) x else utils::head(x, n)
    epwshiftr_cli_render_table(
        shown,
        columns = columns,
        max_rows = if (is.infinite(n)) nrow(shown) else n,
        show_types = FALSE,
        more_hint = more_hint,
        hidden_hint = "Use the corresponding shift_*() inspector for all columns.",
        total_rows = as.integer(total_rows)
    )
    invisible(NULL)
}

# Print a consistent stage heading and status fact before class-specific
# scientific context is added.
shift__print_stage_intro <- function(x, title, facts = list()) {
    shift__print_header(title)
    shift__print_facts(c(list(
        "Status" = tryCatch(shift_status(x), error = function(e) "unknown")
    ), facts))
    invisible(NULL)
}

# Render optional workflow provenance after the scientific query/result view.
shift__print_workflow <- function(x, verbose = FALSE) {
    ids <- shift_ids(x)
    diagnostics <- shift_diagnostics(x)
    if (isTRUE(verbose)) {
        cli::cli_rule("Workflow")
        esg__print_facts(list(
            "Status" = tryCatch(shift_status(x), error = function(e) "unknown"),
            "Store" = shift__display_path(x@store_path),
            "Query ID" = ids$query_id,
            "Run ID" = ids$run_id,
            "Step ID" = ids$step_id
        ))
    }
    if (nrow(diagnostics)) {
        counts <- table(diagnostics$severity)
        cli::cli_rule("Diagnostics")
        esg__print_facts(list(
            "Counts" = paste(sprintf("%s %s", counts, names(counts)), collapse = " \u00b7 ")
        ))
    }
    invisible(NULL)
}

# Print a ShiftRequest through the same canonical parameter renderer as
# EsgQuery while retaining the workflow's explicit auto-node semantics.
shift__print_request <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    query <- shift_as_query(x)
    state <- query$state()
    pinned_node <- x@meta$options$index_node
    node <- if (is.null(pinned_node)) "auto" else query$index_node()
    esg__print_query(node, state$parameter, title = "ESGF request")
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Print a persisted ShiftFiles catalog as an ESGF result receipt plus a
# width-aware table preview, without reading the complete catalog into R.
shift__print_files <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    ids <- shift_ids(x)
    result_fields <- unique(as.character(x@meta$result_fields))
    result_fields <- result_fields[!is.na(result_fields) & nzchar(result_fields)]
    store <- tryCatch(shift_store(x), error = identity)
    if (inherits(store, "condition")) {
        # A detached or temporarily unavailable store must not make the object
        # itself unprintable. Preserve the established result header and expose
        # only metadata already cached on the ShiftFiles handle.
        request <- shift_stage_root(x)
        node <- if (!is.null(request)) {
            shift_coalesce(request@meta$options$index_node, "auto")
        } else {
            "unavailable"
        }
        fields <- if (length(result_fields)) {
            cli::format_inline("{length(result_fields)} | [ {result_fields} ]")
        } else {
            "unavailable"
        }
        esg__print_header("ESGF Query Result [File]")
        esg__print_facts(list(
            "Index Node" = node,
            "Result count" = shift_coalesce(x@meta$file_count, "unavailable"),
            "Fields" = fields
        ))
        if (!is.null(request)) {
            query <- shift_as_query(request)
            esg__print_parameters(query$state()$parameter)
        }
        cli::cli_rule("Files")
        cli::cli_alert_info("Cached File rows are not available on this handle.")
        shift__print_store_notice(conditionMessage(store))
        shift__print_workflow(x, verbose = verbose)
        return(invisible(x))
    }
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    summary <- shift__file_catalog_summary(store, ids$query_id)
    if (!nrow(summary)) {
        summary <- data.table::data.table(
            file_count = 0L,
            total_size = 0
        )
    }
    summary <- summary[1L]
    runs <- shift_query_run(store, ids$query_id)
    run <- if (nrow(runs)) runs[1L] else data.table::data.table()
    file_count <- as.integer(summary$file_count[[1L]])
    created <- if (nrow(run)) shift__print_time(run$created_at) else NULL
    node <- if (nrow(run)) run$index_node[[1L]] else NULL
    if (!length(result_fields)) {
        # Stages created before response fields were persisted fall back to
        # the stable catalog preview schema rather than reading every record.
        result_fields <- names(shift__file_catalog_preview(store, ids$query_id, n = 1L))
    }
    fields <- if (length(result_fields)) {
        # cli's vector interpolation matches the established EsgResultFile
        # punctuation and wrapping, including the final conjunction.
        cli::format_inline("{length(result_fields)} | [ {result_fields} ]")
    } else {
        "0"
    }

    esg__print_header("ESGF Query Result [File]")
    facts <- list(
        "Index Node" = node,
        "Collected at" = created,
        "Result count" = format(file_count, big.mark = ",", scientific = FALSE),
        "Total size" = format_size_units(summary$total_size[[1L]]),
        "Fields" = fields
    )
    esg__print_facts(facts)

    request <- shift_stage_root(x)
    if (!is.null(request)) {
        query <- shift_as_query(request)
        esg__print_parameters(query$state()$parameter)
    }

    cli::cli_rule("Files")
    if (file_count < 1L) {
        cli::cli_alert_info("No matching file records. Review the ESGF query constraints and collect again.")
    } else {
        preview <- shift__file_catalog_preview(store, ids$query_id, n = n)
        epwshiftr_cli_render_table(
            preview,
            columns = c(
                "source_id", "experiment_id", "variable_id", "variant_label",
                "grid_label", "table_id", "datetime_start", "datetime_end",
                "size", "filename", "data_node"
            ),
            max_rows = if (is.infinite(n)) nrow(preview) else n,
            show_types = FALSE,
            more_hint = "use `shift_files()` for all records.",
            hidden_hint = "Use `shift_files()` for all columns.",
            total_rows = file_count
        )
    }
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Describe an EPW input by its stable path when available, falling back to the
# adapter class rather than dumping an R6 or external Epw object.
shift__format_epw <- function(epw, full = FALSE) {
    if (is.null(epw)) {
        return(NULL)
    }
    path <- if (is.character(epw) && length(epw) == 1L) {
        epw
    } else {
        tryCatch(epw_file_coerce(epw)$path(), error = function(e) NULL)
    }
    if (is.null(path)) {
        return(class(epw)[[1L]])
    }
    if (isTRUE(full)) {
        normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    } else {
        basename(path)
    }
}

# Add a non-fatal store-read notice after a cached object summary so temporary
# filesystem problems remain visible without masking the object itself.
shift__print_store_notice <- function(error) {
    if (is.null(error) || !nzchar(error)) {
        return(invisible(NULL))
    }
    cli::cli_rule("Diagnostics")
    cli::cli_alert_warning("Persisted preview unavailable: {error}")
    invisible(NULL)
}

# Render the deferred Future EPW intent and expected case matrix without
# resolving ESGF nodes or mutating the plan.
shift__print_plan <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    meta <- x@meta
    climate <- meta$climate
    request <- meta$request@meta
    method <- meta$method
    model <- if (!is.null(climate)) climate@model else request$source
    scenarios <- if (!is.null(climate)) climate@scenarios else request$experiment
    member <- if (!is.null(climate)) climate@member else request$variant
    grid <- if (!is.null(climate)) climate@grid else request$filters$grid_label
    climate_parts <- c(shift__display_values(model),
        shift__display_values(scenarios))
    climate_parts <- climate_parts[!is.na(climate_parts) &
        nzchar(climate_parts)]
    cases <- data.table::copy(data.table::as.data.table(meta$expected_cases))
    if ("years" %in% names(cases)) {
        cases[, years := vapply(years, shift__format_years, character(1L))]
    }

    shift__print_stage_intro(x, "Future EPW Plan", list(
        "Climate" = paste(climate_parts, collapse = " \u00b7 "),
        "Periods" = shift__format_periods(meta$periods),
        "Method" = shift__format_morph_method(method@name, method@recipe),
        "Reference" = shift__format_reference(method@reference,
            method@recipe),
        "Observed reference" = shift__format_reference(
            method@observed_reference
        ),
        "Selection" = sprintf("member %s \u00b7 grid %s \u00b7 tables %s",
            shift__format_auto(member), shift__format_auto(grid),
            shift__format_cmip6_tables(if (!is.null(climate)) {
                climate@table
            } else {
                request$filters$table_id
            })),
        "Expected outputs" = nrow(cases),
        "Output directory" = shift__display_path(meta$epw$export_dir)
    ))
    if (isTRUE(verbose)) {
        nodes <- if (!is.null(climate)) climate@index_nodes else
            request$options$index_node
        control <- meta$control
        cli::cli_rule("Discovery")
        shift__print_facts(list(
            "Frequency" = if (!is.null(climate)) climate@frequency else
                request$frequency,
            "Table" = shift__format_cmip6_tables(
                if (!is.null(climate)) climate@table else
                    request$filters$table_id),
            "Index nodes" = shift__display_values(nodes, max = Inf),
            "Download" = control@download,
            "Partial outputs" = control@allow_partial,
            "Output layout" = control@output_layout
        ))
    }
    shift__print_table(
        cases,
        "Expected outputs",
        columns = c("source_id", "experiment_id", "variant_label",
            "grid_label", "period", "years", "status", "missing_reason"),
        n = n,
        empty = "No expected output cases.",
        more_hint = "use `shift_cases()` for all expected cases."
    )
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Summarize persistent download task state and expose only a bounded task table
# in the default console receipt.
shift__print_download <- function(x, n = 10L, width = NULL,
                                  verbose = FALSE) {
    shift__print_use_width(width)
    ids <- shift_ids(x)
    cached <- if (is.data.frame(x@meta$session)) {
        data.table::as.data.table(x@meta$session)
    } else {
        data.table::data.table()
    }
    read <- if (nrow(cached)) {
        list(data = cached, error = NULL)
    } else {
        shift__print_store_read(x, function(store) {
            if (is.null(ids$session_id) || is.na(ids$session_id)) {
                return(data.table::data.table())
            }
            store$download_status(session_id = ids$session_id)
        })
    }
    tasks <- read$data
    counts <- if (nrow(tasks) && "status" %in% names(tasks))
        table(tasks$status) else integer()
    complete <- if (nrow(tasks) && "status" %in% names(tasks))
        sum(tasks$status %in% c("done", "skipped", "verified")) else 0L
    bytes_done <- if ("bytes_done" %in% names(tasks))
        sum(tasks$bytes_done, na.rm = TRUE) else 0
    bytes_total <- if ("size" %in% names(tasks))
        sum(tasks$size, na.rm = TRUE) else 0

    shift__print_stage_intro(x, "CMIP6 Download", list(
        "Session" = ids$session_id,
        "Tasks" = if (nrow(tasks)) sprintf("%d/%d complete%s", complete,
            nrow(tasks), if (length(counts)) sprintf(" \u00b7 %s",
                paste(sprintf("%s %d", names(counts), counts),
                    collapse = " \u00b7 ")) else "") else "none",
        "Transfer" = if (bytes_total > 0) sprintf("%s / %s",
            format_size_units(bytes_done), format_size_units(bytes_total)) else NULL
    ))
    shift__print_table(
        tasks,
        "Tasks",
        columns = c("status", "filename", "bytes_done", "size",
            "speed_bps", "eta_seconds", "data_node", "attempts",
            "last_error"),
        n = n,
        empty = "No download tasks are registered.",
        more_hint = "use `shift_data()` or the Downloader inspectors for all tasks."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Summarize extraction coverage by scientific identity while keeping the full
# plan and extracted time-series data behind their dedicated inspectors.
shift__print_climate <- function(x, n = 10L, width = NULL,
                                 verbose = FALSE) {
    shift__print_use_width(width)
    cached <- data.table::as.data.table(shift_coalesce(x@meta$coverage,
        data.table::data.table()))
    read <- if (nrow(cached)) {
        list(data = cached, error = NULL)
    } else {
        ids <- shift_ids(x)
        shift__print_store_read(x, function(store) {
            store$coverage(plan_id = ids$plan_id)
        })
    }
    coverage <- read$data
    site <- tryCatch(shift_target(x), error = function(e) NULL)
    complete <- if (nrow(coverage) && "complete" %in% names(coverage))
        sum(coverage$complete %in% TRUE) else 0L
    rows <- if ("output_rows" %in% names(coverage))
        sum(coverage$output_rows, na.rm = TRUE) else 0

    shift__print_stage_intro(x, "Extracted Climate", list(
        "Site" = if (!is.null(site)) shift_coalesce(site@label, site@id) else NULL,
        "Periods" = shift__format_periods(x@meta$periods),
        "Coverage" = sprintf("%d/%d complete", complete, nrow(coverage)),
        "Variables" = if ("variable_id" %in% names(coverage))
            shift__display_values(unique(coverage$variable_id)) else NULL,
        "Rows" = if (rows > 0) format(rows, big.mark = ",",
            scientific = FALSE) else NULL
    ))
    shift__print_table(
        coverage,
        "Coverage",
        columns = c("complete", "status", "experiment_id", "variable_id",
            "variant_label", "grid_label", "time_start", "time_stop",
            "output_time_count", "output_rows", "last_error"),
        n = n,
        empty = "No extraction coverage is available.",
        more_hint = "use `shift_coverage()` for all extraction plans."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Select the most informative available morph result source in a deterministic
# order so old and resumed stages remain printable across process boundaries.
shift__morph_print_rows <- function(x) {
    cached <- data.table::as.data.table(shift_coalesce(x@meta$results,
        data.table::data.table()))
    if (nrow(cached)) {
        return(list(data = cached, error = NULL))
    }
    ids <- shift_ids(x)
    persisted <- shift__print_store_read(x, function(store) {
        shift_morph_result_rows(store, ids$morph_id)
    })
    if (nrow(persisted$data)) {
        return(persisted)
    }
    plan <- data.table::as.data.table(shift_coalesce(x@meta$plan,
        data.table::data.table()))
    if (nrow(plan)) {
        persisted$data <- plan
    }
    persisted
}

# Render morphing method/reference identity and a bounded result/case preview
# without printing hourly morphed weather data.
shift__print_morphed <- function(x, n = 10L, width = NULL,
                                 verbose = FALSE) {
    shift__print_use_width(width)
    recipe <- x@meta$recipe
    read <- shift__morph_print_rows(x)
    rows <- read$data
    case_count <- if ("case_id" %in% names(rows))
        data.table::uniqueN(rows$case_id) else nrow(rows)
    reference <- shift_coalesce(x@meta$reference_spec, x@meta$reference)

    shift__print_stage_intro(x, "Morphed EPW", list(
        "Method" = shift__format_morph_method(
            shift_coalesce(recipe$name, recipe$backend), recipe,
            missing_belcher_profile = "legacy"),
        "Reference" = shift__format_reference(reference, recipe),
        "Cases" = case_count,
        "Results" = nrow(rows)
    ))
    shift__print_table(
        rows,
        "Morph results",
        columns = c("case_id", "source_id", "experiment_id",
            "variant_label", "period", "status", "row_count",
            "output_path", "last_error"),
        n = n,
        empty = "No morph results are available.",
        more_hint = "use `shift_data()` or `shift_artifacts()` for complete morph data."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render generated and exported EPW paths by user case while keeping weather
# rows behind shift_data().
shift__print_outputs_stage <- function(x, n = 10L, width = NULL,
                                       verbose = FALSE) {
    shift__print_use_width(width)
    outputs <- data.table::as.data.table(shift_coalesce(x@meta$outputs,
        data.table::data.table()))
    read_error <- NULL
    if (!nrow(outputs)) {
        read <- shift__print_store_read(x, function(store) {
            shift_epw_output_rows(store, shift_ids(x)$morph_id)
        })
        outputs <- read$data
        read_error <- read$error
    }
    paths <- intersect(c("export_path", "path", "output_path"),
        names(outputs))
    existing <- if (length(paths) && nrow(outputs)) {
        path <- outputs[[paths[[1L]]]]
        sum(!is.na(path) & nzchar(path))
    } else 0L

    shift__print_stage_intro(x, "EPW Outputs", list(
        "Outputs" = sprintf("%d registered \u00b7 %d path%s", nrow(outputs),
            existing, if (existing == 1L) "" else "s"),
        "Export directory" = if (!is.null(x@meta$export_dir))
            shift__display_path(x@meta$export_dir) else NULL
    ))
    shift__print_table(
        outputs,
        "Outputs",
        columns = c("source_id", "experiment_id", "variant_label", "period",
            "path", "export_path", "created_at"),
        n = n,
        empty = "No EPW outputs are registered.",
        more_hint = "use `shift_outputs()` for all output records."
    )
    shift__print_store_notice(read_error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render a site target as scientific context rather than exposing its inherited
# ShiftStage storage fields.
shift__print_site <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("EPW Site")
    shift__print_facts(list(
        "ID" = x@id,
        "Label" = x@label,
        "Coordinates" = sprintf("%.6f, %.6f", x@lon, x@lat),
        "EPW" = shift__format_epw(x@epw, full = verbose)
    ))
    if (isTRUE(verbose) && length(x@metadata)) {
        cli::cli_rule("Metadata")
        shift__print_facts(list("Values" = shift__format_options(x@metadata)))
    }
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render a complete CMIP6 scientific specification without listing every
# failover URL unless verbose output was explicitly requested.
shift__print_cmip6 <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("CMIP6 Climate")
    shift__print_facts(list(
        "Model" = shift__display_values(x@model),
        "Scenarios" = shift__display_values(x@scenarios),
        "Member" = shift__format_auto(x@member),
        "Grid" = shift__format_auto(x@grid),
        "Frequency" = x@frequency,
        "Table" = shift__format_cmip6_tables(x@table),
        "Activity" = x@activity,
        "Index nodes" = sprintf("%d-node failover", length(x@index_nodes)),
        "Data node" = shift__format_auto(x@data_node)
    ))
    if (isTRUE(verbose)) {
        nodes <- data.table::data.table(
            priority = seq_along(x@index_nodes),
            index_node = x@index_nodes
        )
        shift__print_table(nodes, "Discovery", c("priority", "index_node"),
            n = n, more_hint = "increase `n` to show every index node.")
        if (length(x@filters)) {
            cli::cli_rule("Filters")
            shift__print_facts(list("Values" = shift__format_options(x@filters)))
        }
    }
    invisible(x)
}

# Render workflow control policy as explicit semantic choices instead of a raw
# S7 property dump.
shift__print_control <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("Shift Control")
    shift__print_facts(list(
        "Strict" = x@strict,
        "Allow partial" = x@allow_partial,
        "Download" = x@download,
        "Resume" = x@resume,
        "Overwrite" = x@overwrite,
        "Extraction" = x@extraction_method,
        "Output layout" = x@output_layout
    ))
    invisible(x)
}

# Render a reference specification with compact periods and keep provider and
# stage option detail behind verbose output.
shift__print_reference <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("Climate Reference")
    shift__print_facts(list(
        "Mode" = x@mode,
        "Periods" = shift__format_periods(x@periods),
        "Plan IDs" = shift__display_values(x@plan_id),
        "Experiment" = x@experiment,
        "Activity" = x@activity,
        "Match" = shift__display_values(x@match)
    ))
    if (isTRUE(verbose)) {
        details <- list(
            "Filters" = shift__format_options(x@filters),
            "Options" = shift__format_options(x@options),
            "Collect" = shift__format_options(x@collect),
            "Extract" = shift__format_options(x@extract)
        )
        if (any(vapply(details, function(value) !is.null(value) &&
            nzchar(value), logical(1L)))) {
            cli::cli_rule("Workflow options")
            shift__print_facts(details)
        }
    }
    invisible(x)
}

# Render the method/reference contract and expose bounded rule identity only in
# verbose mode; backend closures and environments are never printed.
shift__print_morph_method <- function(x, n = 10L, width = NULL,
                                      verbose = FALSE) {
    shift__print_use_width(width)
    recipe <- x@recipe
    shift__print_header("Morph Method")
    shift__print_facts(list(
        "Name" = x@name,
        "Backend" = recipe$backend,
        "Profile" = recipe$profile,
        "Reference" = shift__format_reference(x@reference, recipe),
        "Requires reference" = x@requires_reference,
        "Accepts reference" = morpher__recipe_accepts_reference(recipe),
        "Observed reference" = shift__format_reference(
            x@observed_reference
        ),
        "Requires observed reference" =
            x@requires_observed_reference,
        "Accepts observed reference" =
            morpher__recipe_accepts_observed_reference(recipe),
        "Variables" = shift__display_values(epw_morph_variables(recipe))
    ))
    if (isTRUE(verbose)) {
        option_rows <- data.table::data.table(
            option = names(recipe$options),
            value = vapply(unclass(recipe$options), function(value) {
                shift_coalesce(shift__display_values(value, max = Inf),
                    "<empty>")
            }, character(1L))
        )
        # Options are the durable scientific contract, so verbose output shows
        # every value even when `n` bounds the longer method and rule previews.
        shift__print_table(option_rows, "Options", c("option", "value"),
            n = Inf, empty = "No profile options.")
        methods <- recipe$methods
        method_rows <- data.table::data.table(
            field = names(methods), method = unname(methods)
        )
        shift__print_table(method_rows, "Method overrides",
            c("field", "method"), n = n,
            empty = "No method overrides.",
            more_hint = "increase `n` to show every method override.")
        rules <- data.table::as.data.table(recipe$rules)
        shift__print_table(rules, "Rules",
            c("step", "epw_field", "variable_id", "method", "required"),
            n = n, empty = "No backend rules.",
            more_hint = "increase `n` to show every backend rule.")
    }
    invisible(x)
}

# Bound a dashboard table after it has been rendered so ShiftRun can honour the
# same `n` contract without duplicating the watch renderer's table semantics.
shift__print_view_rows <- function(lines, n, width, label) {
    if (!length(lines) || is.infinite(n)) {
        return(lines)
    }
    # Resolver and case views both own a title plus one header row. Preserve
    # those rows and limit only the underlying business records.
    prefix <- min(2L, length(lines))
    records <- max(0L, length(lines) - prefix)
    if (records <= n) {
        return(lines)
    }
    hint <- shift__ui_fit(sprintf("  \u2026 %d more %s", records - n, label),
        width)
    c(lines[seq_len(prefix + n)], cli::style_dim(hint))
}

# Print one non-animated snapshot through the same state/view pipeline used by
# foreground completion receipts and shift_watch(). A failed refresh falls back
# to the handle's cached snapshot and is reported after the dashboard.
shift__print_run <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    refresh_error <- NULL
    run <- x
    # A cached cross-session handle without store identity cannot be refreshed.
    # Do not silently fall back to the user's default store, which may refer to
    # an unrelated run or produce an environment-specific filesystem error.
    if (is.null(x@store_path) || !nzchar(x@store_path)) {
        refresh_error <- "No store is associated with this cached run."
    } else {
        refreshed <- tryCatch(shift_refresh(x), error = identity)
        if (inherits(refreshed, "condition")) {
            refresh_error <- conditionMessage(refreshed)
        } else {
            run <- refreshed
        }
    }
    view <- tryCatch(
        shift__ui_run_view(
            run,
            width = shift__ui_width(width),
            detail = if (isTRUE(verbose)) "detail" else "normal",
            motion = "none"
        ),
        error = identity
    )
    if (inherits(view, "condition")) {
        shift__print_stage_intro(run, "Shift Run", list(
            "Run" = shift_ids(run)$run_id,
            "Stage" = run@meta$run$current_stage,
            "Snapshot" = "cached metadata only"
        ))
        refresh_error <- paste(c(refresh_error, conditionMessage(view)),
            collapse = "; ")
    } else {
        view$nodes <- shift__print_view_rows(view$nodes, n,
            shift__ui_width(width), "resolver attempt(s)")
        view$cases <- shift__print_view_rows(view$cases, n,
            shift__ui_width(width), "case(s)")
        shift__ui_print_view(view, include_tables = TRUE)
    }
    shift__print_store_notice(refresh_error)
    invisible(x)
}

# ShiftRequest has a query-oriented static receipt rather than the generic
# internal stage dump used by data-processing stages.
S7::method(print, ShiftRequest) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_request(x, width = opts$width, verbose = opts$verbose)
}

# ShiftFiles combines the shared ESGF result hierarchy with a semantic CMIP6
# catalog preview whose row count and terminal width are user-controllable.
S7::method(print, ShiftFiles) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_files(x, n = opts$n, width = opts$width, verbose = opts$verbose)
}

# ShiftPlan prints immutable scientific intent and its expected case contract;
# it never invokes the resolver or touches remote services.
S7::method(print, ShiftPlan) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_plan(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftRun reuses the static dashboard view so print, watch, and foreground
# completion receipts cannot drift in status or diagnostic wording.
S7::method(print, ShiftRun) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_run(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftDownload prints persistent transfer state without starting or resuming a
# Downloader job.
S7::method(print, ShiftDownload) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_download(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftClimate prints coverage plans rather than materializing extracted
# Parquet weather rows.
S7::method(print, ShiftClimate) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_climate(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftMorphed prints result identity and artifacts, leaving hourly weather
# values behind shift_data().
S7::method(print, ShiftMorphed) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_morphed(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftOutputs prints generated/exported paths by user case.
S7::method(print, ShiftOutputs) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_outputs_stage(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftCmip6Spec prints the complete future climate identity while collapsing
# failover nodes until verbose output is requested.
S7::method(print, ShiftCmip6Spec) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_cmip6(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# ShiftControl prints the workflow-wide policy choices that cannot be
# overridden by individual stage option lists.
S7::method(print, ShiftControl) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_control(x, width = opts$width, verbose = opts$verbose)
}

# ShiftReferenceSpec prints compact reference periods and identity rather than
# its raw S7 properties.
S7::method(print, ShiftReferenceSpec) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_reference(x, width = opts$width, verbose = opts$verbose)
}

# ShiftMorphMethod prints the algorithm/reference contract and only expands
# method rules in verbose mode.
S7::method(print, ShiftMorphMethod) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_morph_method(x, n = opts$n, width = opts$width,
        verbose = opts$verbose)
}

# Extension ShiftStage classes without a dedicated print method still receive
# the shared receipt hierarchy instead of the historical angle-bracket dump.
S7::method(print, ShiftStage) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_use_width(opts$width)
    shift__print_stage_intro(x, "Shift Stage", list(
        "Class" = class(x)[[1L]],
        "Stage" = x@stage
    ))
    shift__print_workflow(x, verbose = opts$verbose)
    invisible(x)
}

# ShiftSite prints user-facing geographic and EPW identity.
S7::method(print, ShiftSite) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_site(x, width = opts$width, verbose = opts$verbose)
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
