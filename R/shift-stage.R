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
        requires_reference = S7::new_property(S7::class_logical)
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
#'   stage, or `NULL`. Optional-reference methods such as [belcher()] use the
#'   baseline EPW climatology when `NULL`.
#' @export
shift_morph_method <- function(recipe, reference = NULL) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    if (!is.null(reference) &&
        !S7::S7_inherits(reference, ShiftReferenceSpec) &&
        !S7::S7_inherits(reference, ShiftClimate)) {
        cli::cli_abort("`reference` must be a {.cls ShiftReferenceSpec}, a {.cls ShiftClimate} stage, or `NULL`.")
    }

    requires_reference <- isTRUE(morpher__recipe_requires_reference(recipe))
    accepts_reference <- isTRUE(morpher__recipe_accepts_reference(recipe))
    if (requires_reference && is.null(reference)) {
        cli::cli_abort(c(
            "The selected morphing method requires an explicit reference.",
            "i" = "Supply a reference spec or extracted reference climate when constructing the method."
        ))
    }
    if (!accepts_reference && !is.null(reference)) {
        cli::cli_abort("The selected morphing method does not accept reference climate data.")
    }

    ShiftMorphMethod(
        name = recipe$name,
        recipe = recipe,
        reference = reference,
        requires_reference = requires_reference
    )
}

#' @rdname shift_api
#' @param methods Optional named Belcher step method overrides.
#' @export
belcher <- function(reference = NULL, methods = NULL) {
    shift_morph_method(
        epw_morph_recipe(name = "belcher", methods = methods),
        reference = reference
    )
}

#' @rdname shift_api
#' @param model CMIP6 source/model IDs.
#' @param scenarios CMIP6 future scenario experiment IDs.
#' @param member Optional CMIP6 variant labels. `NULL` asks the task workflow to
#'   choose one complete member.
#' @param grid Optional single CMIP6 grid label.
#' @param table Optional CMIP6 table ID inferred from `frequency` when `NULL`.
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
    checkmate::assert_string(table, min.chars = 1L, null.ok = TRUE)
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
        table = shift_coalesce(table, shift__cmip6_table_id(frequency)),
        activity = activity,
        index_nodes = index_nodes,
        data_node = data_node,
        filters = filters
    )
}

# Translate one complete CMIP6 climate specification into the lower-level
# request consumed by the staged workflow and ESGF collector.
shift__request_from_cmip6 <- function(climate, periods, method) {
    shift_cmip6_scenario(
        source = climate@model,
        scenario = climate@scenarios,
        member = climate@member,
        years = periods$year,
        variables = epw_morph_variables(method@recipe),
        frequency = climate@frequency,
        activity = climate@activity,
        table_id = climate@table,
        grid_label = climate@grid,
        data_node = climate@data_node,
        index_node = climate@index_nodes[[1L]],
        filters = climate@filters,
        options = list(time_filter_method = "metadata")
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
#' @param table_id CMIP6 table ID. If `NULL`, a common atmospheric table is
#'   inferred from `frequency`.
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
    checkmate::assert_string(table_id, min.chars = 1L, null.ok = TRUE)
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
#' @param all,limit Collection controls passed to [EsgQuery] / [EsgResultDataset].
#' @param label Optional label recorded with collected File records.
#' @export
shift_collect <- S7::new_generic(
    "shift_collect",
    "x",
    function(x, store = NULL, fields = "*", all = TRUE, limit = FALSE,
             label = NULL, progress = getOption("epwshiftr.progress", interactive()), ...) {
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
             reporter = NULL, ...) {
    S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param site A `shift_site()` object.
#' @param periods A period table, usually from [epw_morph_periods()].
#' @param method In task-level planning, a complete [shift_morph_method()]
#'   object. In [shift_extract()], the grid extraction method.
#' @param fallback Extraction fallback policy.
#' @param reporter Optional internal workflow reporter. Low-level callers may
#'   leave this as `NULL`.
#' @export
shift_extract <- S7::new_generic(
    "shift_extract",
    "x",
    function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
             filters = list(), method = "nearest", fallback = c("auto", "error"),
             overwrite = FALSE, resume = TRUE, reporter = NULL) {
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
             strict = TRUE, complete_only = TRUE,
             by = c("source_id", "experiment_id", "variant_label", "period"),
             overwrite = FALSE, resume = TRUE, reporter = NULL) {
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
             resume = TRUE, reporter = NULL) {
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
        return(data.table::data.table(
            field = c("run_id", "status", "current_stage", "spec_hash", "output_dir", "last_error"),
            value = as.character(unlist(row[, c(
                "run_id", "status", "current_stage", "spec_hash", "output_dir", "last_error"
            ), with = FALSE], use.names = FALSE))
        ))
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
                             resume = TRUE, reporter = NULL) {
    shift_assert_stage(x)
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    if (S7::S7_inherits(x, ShiftMorphed)) {
        x <- shift_epw(x, separate = separate, overwrite = overwrite, resume = resume)
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
    cli::cli_abort("{.fn shift_cases} expects a {.cls ShiftPlan} or {.cls ShiftRun}.")
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
    shift__reconcile_run_job(opened, run_id)
    shift__run_handle(opened, run_id)
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
    checkmate::assert_string(x, min.chars = 1L)
    shift_run_get(x, store = store)
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
    terminal <- c("completed", "partial", "failed", "cancelled")
    bar_id <- NULL
    last_event_id <- NA_character_
    event_cursor_initialized <- FALSE
    # Keep one multiline progress frame alive for the same four-row state view
    # used by foreground runs. A vanished IDE bar is recreated once.
    update_dynamic <- function(view) {
        refreshed <- shift__ui_progress_refresh(bar_id, view$lines)
        bar_id <<- refreshed$ids
        refreshed$ok
    }
    close_dynamic <- function(result = "done") {
        if (length(bar_id)) {
            shift__ui_progress_close(bar_id, result = result)
            bar_id <<- character()
        }
        invisible(NULL)
    }
    on.exit(close_dynamic(), add = TRUE)
    emit_snapshot <- function(snapshot, initial = FALSE, final = FALSE) {
        view <- shift__ui_run_view(snapshot, width = shift__ui_width(),
            detail = ui@detail)
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
                detail = ui@detail), include_tables = TRUE)
        }
        return(run)
    }
    tryCatch({
        first <- TRUE
        repeat {
            run <- shift_run_get(run_id, store = store_path)
            done <- shift_status(run, refresh = FALSE) %in% terminal
            emit_snapshot(run, initial = first, final = done)
            first <- FALSE
            if (done) break
            Sys.sleep(interval)
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
        line = seq_along(lines),
        message = lines
    )
}

#' @rdname shift_api
#' @export
shift_datasets <- function(x, all = TRUE, limit = FALSE,
                           progress = getOption("epwshiftr.progress", interactive())) {
    shift_assert_stage(x)
    checkmate::assert_flag(all)
    checkmate::assert_flag(progress)

    if (S7::S7_inherits(x, ShiftRequest)) {
        return(shift_as_query(x)$collect(type = "Dataset", all = all,
            limit = limit, progress = progress))
    }

    files <- shift_stage_nested(x, list(ShiftFiles))
    if (!is.null(files) && !is.null(files@meta$datasets)) {
        return(files@meta$datasets)
    }

    request <- shift_stage_root(x)
    if (!is.null(request)) {
        return(shift_datasets(request, all = all, limit = limit, progress = progress))
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
#' @param refresh Whether a `ShiftRun` inspector should reload the latest state
#'   from its store before returning.
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
                                                    label = NULL,
                                                    progress = getOption("epwshiftr.progress", interactive()), ...) {
    checkmate::assert_character(fields, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
    checkmate::assert_flag(all)
    checkmate::assert_flag(progress)
    checkmate::assert_string(label, null.ok = TRUE)
    if (is.null(store)) {
        cli::cli_abort("`store` is required for {.fn shift_collect}.")
    }
    store <- shift_store(store, create = TRUE)
    datasets <- shift_datasets(x, all = all, limit = limit, progress = progress)
    files <- datasets$collect(type = "File", fields = fields, all = TRUE,
        limit = NULL, progress = progress, ...)

    if (!is.null(x@meta$time) && !identical(x@meta$options$time_filter_method, "metadata")) {
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
    bytes_total <- if (total && "size" %in% names(tasks)) {
        sum(suppressWarnings(as.numeric(tasks$size)), na.rm = TRUE)
    } else {
        NA_real_
    }
    list(
        current = completed,
        total = total,
        failed = failed,
        bytes_done = bytes_done,
        bytes_total = bytes_total,
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
    if (!is.null(active) && length(active) && !is.na(active) && nzchar(active)) {
        paste0(label, " \u00b7 ", basename(active))
    } else {
        label
    }
}

# Bridge downloader callbacks into the workflow reporter. Progress callbacks
# are throttled by ShiftReporter while task/fallback milestones remain durable.
shift__download_reporter_bind <- function(downloader, reporter, role,
                                            variables = 0L) {
    tokens <- character()
    callback <- function(event, dl) {
        metrics <- shift__download_metrics(dl, event$session_id,
            variables = variables)
        active <- shift_coalesce(event$filename, event$target_path)
        label <- shift__download_label(role, metrics,
            active = if (shift__ui_at_least(reporter$ui(), "detail")) active else NULL)
        details <- list(
            unit_type = "download_session",
            catalog_role = role,
            current = metrics$current,
            total = metrics$total,
            bytes_done = metrics$bytes_done,
            bytes_total = metrics$bytes_total,
            variables = metrics$variables,
            data_node = event$data_node,
            access_method = "HTTPServer"
        )
        switch(event$event,
            session_start = reporter$unit_started(label,
                current = metrics$current, total = metrics$total,
                details = details),
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
            session_done = reporter$unit_completed(label,
                current = metrics$current, total = metrics$total,
                outcome = if (metrics$failed) "failed" else "completed",
                details = details)
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
                                                   reporter = NULL, ...) {
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

S7::method(shift_extract, ShiftFiles) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                  filters = list(), method = "nearest",
                                                  fallback = c("auto", "error"), overwrite = FALSE,
                                                  resume = TRUE, reporter = NULL) {
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
        reporter = reporter
    )
}

S7::method(shift_extract, ShiftDownload) <- function(x, site = NULL, periods = NULL, variables = NULL, time = NULL,
                                                     filters = list(), method = "nearest",
                                                     fallback = c("auto", "error"), overwrite = FALSE,
                                                     resume = TRUE, reporter = NULL) {
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
        reporter = reporter
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
        list(store = store, fields = "*", all = TRUE, limit = FALSE, label = "historical-reference"),
        collect_overrides
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
        table = climate@table,
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
                methods = as.list(method@recipe$methods),
                rules_identity = store__hash(morpher__json(method@recipe))
            ),
            requires_reference = method@requires_reference,
            reference_mode = if (is.null(method@reference)) {
                if (isTRUE(morpher__recipe_accepts_reference(method@recipe))) "baseline_epw" else "none"
            } else if (S7::S7_inherits(method@reference, ShiftReferenceSpec)) {
                method@reference@mode
            } else {
                "plan"
            },
            reference = shift__reference_spec_value(method@reference)
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
    methods <- unlist(recipe_spec$methods, use.names = TRUE)
    if (!length(methods)) {
        methods <- NULL
    }
    recipe <- epw_morph_recipe(
        name = as.character(recipe_spec$name),
        backend = as.character(recipe_spec$backend),
        methods = methods
    )
    reference <- shift__reference_from_spec(spec$method$reference)
    method <- shift_morph_method(recipe, reference = reference)
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
                             details = NULL, snapshot = TRUE) {
    now <- store__now()
    row <- data.frame(
        event_id = store__hash(run_id, stage, status, now, stats::runif(1L)),
        run_id = run_id,
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
                              ui = shift_ui()) {
    mode <- match.arg(mode)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    attempts <- jobs[jobs[["run_id"]] == run_id]$attempt
    attempt <- if (length(attempts)) max(attempts, na.rm = TRUE) + 1L else 1L
    now <- store__now()
    job_id <- paste0("shift-job-", substr(store__hash(run_id, attempt, now, stats::runif(1L)), 1L, 20L))
    log_dir <- file.path(store$path, "logs", "shift")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    row <- data.frame(
        job_id = job_id,
        run_id = run_id,
        attempt = as.integer(attempt),
        mode = mode,
        status = if (identical(mode, "process")) "queued" else "running",
        pid = if (identical(mode, "foreground")) as.integer(Sys.getpid()) else NA_integer_,
        hostname = unname(shift_coalesce(Sys.info()[["nodename"]], "localhost")),
        log_path = if (identical(mode, "process")) file.path(log_dir, paste0(job_id, ".log")) else NA_character_,
        ui_json = shift__spec_json(list(
            progress = ui@progress,
            detail = ui@detail,
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
shift__job_update <- function(store, job_id, ...) {
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    row <- jobs[jobs[["job_id"]] == job_id]
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
    shift__live_snapshot_write(store, row$run_id[[1L]])
    invisible(row)
}

# Update the worker heartbeat only at reporter callbacks and workflow
# boundaries; this is deliberately separate from transient Console animation.
shift__job_touch <- function(store, job_id) {
    shift__job_update(store, job_id, heartbeat_at = store__now())
}

# Return all attempts for a run in deterministic attempt order.
shift__run_jobs <- function(store, run_id) {
    jobs <- morpher__private_store(store)$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == run_id]
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
    jobs <- morpher__private_store(store)$read_table("shift_run_job")
    row <- jobs[jobs[["job_id"]] == job_id]
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
    private <- morpher__private_store(store)
    jobs <- private$read_table("shift_run_job")
    job <- jobs[jobs[["job_id"]] == job_id & jobs[["run_id"]] == run_id]
    if (!nrow(job)) {
        cli::cli_abort("Background shift job {.val {job_id}} was not found.")
    }
    ui_spec <- jsonlite::fromJSON(job$ui_json[[1L]], simplifyVector = TRUE)
    ui <- shift_ui(
        # Detached workers use stable logs for every visible mode, while an
        # explicit none setting remains completely quiet.
        progress = if (identical(as.character(ui_spec$progress), "none")) "none" else "log",
        detail = as.character(ui_spec$detail),
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
    row <- runs[runs[["run_id"]] == run_id]
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
    private <- morpher__private_store(store)
    rows <- private$read_table("shift_run")
    row <- rows[rows[["run_id"]] == run_id]
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

# Materialize a lightweight ShiftRun handle from persisted tables.
shift__run_handle <- function(store, run_id, output_stage = NULL, plan = NULL) {
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    row <- runs[runs[["run_id"]] == run_id]
    if (!nrow(row)) {
        cli::cli_abort("Shift run {.val {run_id}} was not found in {.path {store$path}}.")
    }
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == run_id]
    if (nrow(cases)) {
        cases[, years := lapply(years_json, function(value) {
            as.integer(jsonlite::fromJSON(value, simplifyVector = TRUE))
        })]
    }
    events <- private$read_table("shift_run_event")
    events <- events[events[["run_id"]] == run_id][order(created_at)]
    jobs <- private$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == run_id]
    jobs <- jobs[order(jobs[["attempt"]])]
    errors <- events[status %in% c("failed", "error")]
    diagnostics <- if (!nrow(errors)) {
        shift_diagnostics_empty()
    } else {
        do.call(shift_bind_diagnostics, lapply(seq_len(nrow(errors)), function(i) {
            shift_diagnostic(
                errors$stage[[i]],
                "error",
                "shift_run_error",
                errors$message[[i]],
                action = sprintf("Run shift_resume(\"%s\", store = \"%s\").", run_id, store$path)
            )
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
            jobs = jobs, output_stage = output_stage, plan = plan),
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
shift__live_snapshot_write <- function(store, run_id, event_limit = 200L) {
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    run <- runs[runs[["run_id"]] == run_id]
    if (!nrow(run)) {
        return(invisible(NULL))
    }
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == run_id]
    events <- private$read_table("shift_run_event")
    events <- events[events[["run_id"]] == run_id][order(created_at)]
    if (nrow(events) > event_limit) {
        events <- utils::tail(events, event_limit)
    }
    jobs <- private$read_table("shift_run_job")
    jobs <- jobs[jobs[["run_id"]] == run_id]
    jobs <- jobs[order(jobs[["attempt"]])]
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
        outputs = as.data.frame(outputs)
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
    outputs <- shift__live_table(snapshot$outputs)
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
            shift_diagnostic(
                errors$stage[[i]], "error", "shift_run_error", errors$message[[i]],
                action = sprintf("Run shift_resume(\"%s\", store = \"%s\").", run_id, store_path)
            )
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
            jobs = jobs, outputs = outputs, live = TRUE),
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

# Normalize catalog status fields before completeness checks. Superseded,
# retracted, and deprecated records never satisfy a workflow case.
shift__catalog_current <- function(catalog) {
    catalog <- data.table::as.data.table(data.table::copy(catalog))
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

# Compute complete member/grid candidates for one experiment set and exact
# required year/variable contract.
shift__cmip6_candidates <- function(catalog, models, experiments, variables,
                                    years, frequency, table) {
    catalog <- shift__catalog_current(catalog)
    models <- as.character(models)
    experiments <- as.character(experiments)
    variables <- as.character(variables)
    years <- sort(unique(as.integer(years)))
    wanted_frequency <- as.character(frequency)
    wanted_table <- as.character(table)
    catalog <- catalog[
        source_id %in% models &
            experiment_id %in% experiments &
            variable_id %in% variables &
            frequency %in% wanted_frequency &
            table_id %in% wanted_table
    ]
    identities <- unique(catalog[, .(
        source_id, variant_label, grid_label, frequency, table_id
    )])
    if (!nrow(identities)) {
        return(data.table::data.table(
            source_id = character(), variant_label = character(), grid_label = character(),
            frequency = character(), table_id = character(), complete = logical(),
            missing = character()
        ))
    }

    rows <- vector("list", nrow(identities))
    for (i in seq_len(nrow(identities))) {
        identity <- identities[i]
        selected <- catalog[
            shift__catalog_match(source_id, identity$source_id[[1L]]) &
                shift__catalog_match(variant_label, identity$variant_label[[1L]]) &
                shift__catalog_match(grid_label, identity$grid_label[[1L]]) &
                shift__catalog_match(frequency, identity$frequency[[1L]]) &
                shift__catalog_match(table_id, identity$table_id[[1L]])
        ]
        missing <- character()
        for (experiment in experiments) {
            for (variable in variables) {
                files <- selected[experiment_id == experiment & variable_id == variable]
                absent_years <- setdiff(years, shift__catalog_years(files))
                if (!nrow(files)) {
                    missing <- c(missing, sprintf("%s/%s: no files", experiment, variable))
                } else if (length(absent_years)) {
                    missing <- c(
                        missing,
                        sprintf("%s/%s: missing years %s", experiment, variable, paste(absent_years, collapse = ","))
                    )
                }
            }
        }
        rows[[i]] <- cbind(
            identity,
            data.table::data.table(
                complete = !length(missing),
                missing = if (length(missing)) paste(missing, collapse = "; ") else NA_character_
            )
        )
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Apply explicit selection constraints and the locked r1i1p1f1/gn preference;
# unresolved ties are structural ambiguities and must be shown to the user.
shift__choose_cmip6_candidates <- function(candidates, models, member = NULL, grid = NULL) {
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
            cli::cli_abort("No complete CMIP6 member/grid candidate was found for model {.val {model}}.")
        }
        if (!is.null(member)) {
            missing_members <- setdiff(member, unique(available$variant_label))
            if (length(missing_members)) {
                cli::cli_abort("Explicit member(s) are incomplete for model {.val {model}}: {.val {missing_members}}.")
            }
            common_grids <- Reduce(
                intersect,
                lapply(member, function(value) unique(available[variant_label == value]$grid_label))
            )
            common_grids <- common_grids[!is.na(common_grids) & nzchar(common_grids)]
            if (!is.null(grid)) {
                common_grids <- intersect(common_grids, grid)
            } else if ("gn" %in% common_grids) {
                common_grids <- "gn"
            }
            if (length(common_grids) != 1L) {
                cli::cli_abort(
                    c(
                        "CMIP6 grid selection is ambiguous for model {.val {model}} and explicit member(s) {.val {member}}.",
                        "i" = "Candidate grids: {.val {common_grids}}. Set `grid` explicitly."
                    ),
                    class = "epwshiftr_shift_resolution_ambiguity"
                )
            }
            selected[[model]] <- available[
                variant_label %in% member & grid_label == common_grids[[1L]]
            ]
            next
        }

        if (any(available$variant_label %in% "r1i1p1f1")) {
            available <- available[variant_label == "r1i1p1f1"]
        }
        if (is.null(grid) && any(available$grid_label %in% "gn")) {
            available <- available[grid_label == "gn"]
        }
        if ("case_count" %in% names(available) && nrow(available)) {
            available <- available[case_count == max(case_count)]
        }
        if (nrow(available) != 1L) {
            labels <- sprintf("%s/%s", available$variant_label, available$grid_label)
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
                                             variables, years, frequency, table) {
    parts <- lapply(experiments, function(experiment) {
        rows <- shift__cmip6_candidates(
            catalog,
            models = models,
            experiments = experiment,
            variables = variables,
            years = years,
            frequency = frequency,
            table = table
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
    ), by = .(source_id, variant_label, grid_label, frequency, table_id)]
}

# Resolve future and, only when explicitly requested by the method, historical
# catalogs against one shared model/member/frequency/table/grid identity.
shift__resolve_cmip6_selection <- function(plan, future_catalog, reference_catalog = NULL) {
    meta <- plan@meta
    request <- meta$request@meta
    climate <- meta$climate
    models <- if (is.null(climate)) as.character(request$source) else climate@model
    scenarios <- if (is.null(climate)) as.character(request$experiment) else climate@scenarios
    variables <- epw_morph_variables(meta$method@recipe)
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
            table = table
        )
    } else {
        shift__cmip6_candidates(
            future_catalog,
            models = models,
            experiments = scenarios,
            variables = variables,
            years = meta$periods$year,
            frequency = frequency,
            table = table
        )
    }

    reference <- meta$method@reference
    if (S7::S7_inherits(reference, ShiftReferenceSpec) && identical(reference@mode, "historical")) {
        historical <- shift__cmip6_candidates(
            reference_catalog,
            models = models,
            experiments = reference@experiment,
            variables = variables,
            years = reference@periods$year,
            frequency = frequency,
            table = table
        )
        historical <- historical[complete %in% TRUE, .(
            source_id, variant_label, grid_label, frequency, table_id
        )]
        future <- merge(
            future,
            historical,
            by = c("source_id", "variant_label", "grid_label", "frequency", "table_id"),
            all = FALSE,
            sort = FALSE
        )
    }
    shift__choose_cmip6_candidates(future, models, member = member, grid = grid)
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
    filters <- utils::modifyList(
        shift__compact_list(list(
            activity_id = reference@activity,
            table_id = if (is.null(climate)) request$filters$table_id else climate@table,
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
        variables = epw_morph_variables(meta$method@recipe),
        frequency = if (is.null(climate)) request$frequency else climate@frequency,
        time = shift_periods_time(reference@periods),
        filters = filters,
        options = utils::modifyList(reference@options, list(index_node = node, time_filter_method = "metadata"))
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
        shown_node <- shift__report_node(reporter, node)
        reporter$heartbeat(
            sprintf("%s \u00b7 %s catalog \u00b7 waiting", shown_node, phase),
            details = list(unit_type = "catalog", node = node,
                phase = "query", catalog_role = phase,
                transfer_state = state)
        )
        invisible(TRUE)
    }
    old <- options(epwshiftr.query.progress_callback = callback)
    on.exit(options(old), add = TRUE)
    force(expr)
}

# Collect both catalogs from one index node and fail over in the declared order;
# catalogs from different nodes are never merged.
shift__collect_resolved_inputs <- function(plan, run_id, reporter = NULL,
                                           job_id = NULL) {
    store <- shift_store(plan, create = TRUE)
    run_row <- morpher__private_store(store)$read_table("shift_run")
    run_row <- run_row[run_row[["run_id"]] == run_id]
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
            node_label <- shift__report_node(reporter,
                as.character(resolved$index_node))
            reporter$unit_started(
                sprintf("%s \u00b7 loading pinned future%s catalogs",
                    node_label,
                    if (is.null(reference_files)) "" else " + reference"),
                current = 1L,
                total = 1L,
                details = list(
                    unit_type = "index_node",
                    node = as.character(resolved$index_node)
                )
            )
            reporter$unit_skipped(
                sprintf("%s \u00b7 reused pinned selection \u00b7 future %d \u00b7 reference %d files",
                    node_label, as.integer(files@meta$file_count),
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
                    result = "reused pinned selection"
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
    failures <- character()
    for (node_index in seq_along(nodes)) {
        node <- nodes[[node_index]]
        shown_node <- shift__report_node(reporter, node)
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
                sprintf("%s \u00b7 checking %s catalogs", shown_node, catalog_roles),
                current = node_index,
                total = length(nodes),
                details = list(unit_type = "index_node", node = node)
            )
            reporter$notice(sprintf("%s \u00b7 future catalog \u00b7 collecting", shown_node),
                details = list(unit_type = "catalog", node = node,
                    catalog_role = "future"))
        }
        attempt <- tryCatch({
            request <- shift__request_at_node(plan@meta$request, node)
            collect_args <- utils::modifyList(
                list(store = store, fields = fields, all = TRUE, limit = FALSE,
                    label = "future-epw", progress = FALSE),
                plan@meta$collect[setdiff(names(plan@meta$collect), "fields")]
            )
            files <- shift__with_query_reporter(
                reporter, node, "future",
                do.call(shift_collect, c(list(request), collect_args))
            )
            node_future_files <- as.integer(files@meta$file_count)
            if (!is.null(reporter)) {
                future_dataset_count <- as.integer(shift_coalesce(
                    files@meta$dataset_count, 0L))
                reporter$notice(sprintf(
                    "%s \u00b7 future catalog \u00b7 %d dataset(s), %d file(s)",
                    shown_node, future_dataset_count, node_future_files),
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
                    reporter$notice(sprintf(
                        "%s \u00b7 reference catalog \u00b7 collecting", shown_node),
                        details = list(unit_type = "catalog", node = node,
                            catalog_role = "reference"))
                }
                collected_reference <- shift__with_query_reporter(
                    reporter, node, "reference",
                    do.call(shift_collect, c(
                        list(reference_request),
                        utils::modifyList(collect_args, list(label = "historical-reference"))
                    ))
                )
                node_reference_files <- as.integer(collected_reference@meta$file_count)
                if (!is.null(reporter)) {
                    reference_dataset_count <- as.integer(shift_coalesce(
                        collected_reference@meta$dataset_count, 0L))
                    reporter$notice(sprintf(
                        "%s \u00b7 reference catalog \u00b7 %d dataset(s), %d file(s)",
                        shown_node, reference_dataset_count,
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
                reporter$unit_completed(
                    sprintf("%s \u00b7 selected %s / %s", shown_node,
                        paste(unique(attempt$selection$variant_label), collapse = ", "),
                        paste(unique(attempt$selection$grid_label), collapse = ", ")),
                    current = node_index,
                    total = length(nodes),
                    outcome = "completed",
                    details = list(
                        unit_type = "index_node",
                        node = node,
                        future_files = node_future_files,
                        reference_files = node_reference_files,
                        result = sprintf("selected %s / %s",
                            paste(unique(attempt$selection$variant_label), collapse = ", "),
                            paste(unique(attempt$selection$grid_label), collapse = ", "))
                    )
                )
            }
            return(attempt)
        }
        if (inherits(attempt, "epwshiftr_shift_resolution_ambiguity")) {
            stop(attempt)
        }
        if (!is.null(reporter)) {
            reporter$unit_completed(
                sprintf("%s \u00b7 rejected \u00b7 %s", shown_node,
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
                    error = conditionMessage(attempt))
            )
        }
        failures <- c(failures, sprintf("%s: %s", node, conditionMessage(attempt)))
    }
    cli::cli_abort(c(
        "No ESGF index node produced a complete, resolvable workflow input set.",
        "x" = failures
    ))
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
                shift_coalesce(shift_display_values(request$source), "<any source>"),
                shift_coalesce(shift_display_values(request$experiment), "<any experiment>")
            ),
            method@name,
            reference_detail,
            sprintf("%d expected EPW output(s)", nrow(meta$expected_cases)),
            sprintf(
                "member=%s; grid=%s",
                shift_coalesce(shift_display_values(member), "<auto>"),
                shift_coalesce(shift_display_values(grid), "<auto>")
            ),
            shift_coalesce(shift_display_values(nodes), "<provider default>"),
            if (isTRUE(control@allow_partial)) "allow partial outputs" else "all requested cases required",
            shift_display_path(x@store_path),
            shift_display_path(shift_coalesce(epw$export_dir, epw$dir))
        )
    )
}

# Match one coverage table against the expected future cases and, when
# required, the corresponding explicit reference extraction.
shift__case_fulfilment <- function(cases, future_coverage, reference_coverage,
                                    required_variables, requires_reference) {
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
        if ("grid_label" %in% names(rows)) {
            keep <- keep & shift__catalog_match(rows$grid_label, case$grid_label[[1L]])
        }
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
            rows <- future[variable_id == variable]
            if (!nrow(rows) || !all(rows$complete %in% TRUE)) {
                missing <- c(missing, sprintf("future/%s", variable))
            }
        }
        if (isTRUE(requires_reference)) {
            reference <- match_identity(reference_coverage, case, include_experiment = FALSE)
            for (variable in required_variables) {
                rows <- reference[variable_id == variable]
                if (!nrow(rows) || !all(rows$complete %in% TRUE)) {
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
        if ("grid_label" %in% names(coverage)) {
            identity <- identity & shift__catalog_match(coverage$grid_label, case$grid_label[[1L]])
        }
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
# public CMIP identity rather than the internal morph case hash.
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
        if (nrow(hit) == 1L) {
            cases$status[[i]] <- "completed"
            cases$output_id[[i]] <- hit$output_id[[1L]]
            if ("export_path" %in% names(hit)) {
                cases$export_path[[i]] <- hit$export_path[[1L]]
            }
        } else {
            cases$status[[i]] <- "missing"
            cases$missing_reason[[i]] <- if (!nrow(hit)) "final EPW was not produced" else "multiple final EPWs matched one expected case"
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

# Format the last business unit into a compact terminal diagnostic while the
# structured form remains available in shift_run_event$details_json.
shift__failure_context <- function(details) {
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
        sprintf("%s=%s", fields[[name]], as.character(value[[1L]]))
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
        cases <- shift__resolved_expected_cases(x, selection)
        resolved <- list(
            index_node = resolved_inputs$index_node,
            selection = as.data.frame(selection),
            member = unique(selection$variant_label),
            grid = unique(selection$grid_label)
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
            "Resolved %s with member %s and grid %s.",
            resolved_node_label,
            paste(unique(selection$variant_label), collapse = ", "),
            paste(unique(selection$grid_label), collapse = ", ")
        ), details = list(
            node = resolved_inputs$index_node,
            future_files = as.integer(resolved_inputs$files@meta$file_count),
            reference_files = if (is.null(resolved_inputs$reference_files)) {
                0L
            } else {
                as.integer(resolved_inputs$reference_files@meta$file_count)
            },
            member = unique(selection$variant_label),
            grid = unique(selection$grid_label)
        ))

        future_stage <- resolved_inputs$files
        reference_stage <- resolved_inputs$reference_files
        if (identical(control@download, "always")) {
            current_stage <- next_stage("download", "Downloading selected CMIP6 source files.")
            download_args <- utils::modifyList(
                list(
                    run = TRUE,
                    background = FALSE,
                    resume = resume,
                    overwrite = overwrite,
                    # The workflow reporter owns presentation. Native downloader
                    # bars remain disabled while callbacks publish byte/file
                    # metrics into the shared fixed status region.
                    progress = FALSE,
                    reporter = reporter
                ),
                meta$download
            )
            future_stage <- do.call(shift_download, c(list(future_stage),
                utils::modifyList(download_args, list(session_label = "future"))))
            if (!is.null(reference_stage)) {
                reference_stage <- do.call(shift_download, c(list(reference_stage),
                    utils::modifyList(download_args, list(session_label = "reference"))))
            }
            reporter$stage_completed("Downloaded selected CMIP6 source files.")
        }

        current_stage <- next_stage("extract_future", "Extracting future climate data.")
        selection_filters <- list(
            source_id = unique(selection$source_id),
            experiment_id = if (is.null(meta$climate)) {
                meta$request@meta$experiment
            } else {
                meta$climate@scenarios
            },
            variant_label = unique(selection$variant_label),
            grid_label = unique(selection$grid_label),
            frequency = unique(selection$frequency),
            table_id = unique(selection$table_id)
        )
        fallback <- if (identical(control@download, "never")) "error" else "auto"
        extract_overrides <- meta$extract
        if (!is.null(extract_overrides$filters)) {
            selection_filters <- utils::modifyList(selection_filters, extract_overrides$filters)
        }
        extract_overrides$filters <- NULL
        extract_args <- utils::modifyList(
            list(
                site = meta$site,
                periods = meta$periods,
                variables = epw_morph_variables(meta$method@recipe),
                time = NULL,
                filters = selection_filters,
                method = control@extraction_method,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            ),
            extract_overrides
        )
        # `download = "never"` is a workflow run policy, so a middle-layer
        # extraction override must not silently re-enable HTTP fallback.
        if (identical(control@download, "never")) {
            extract_args$fallback <- "error"
        }
        extract_args$site <- meta$site
        extract_args$periods <- meta$periods
        extract_args$overwrite <- overwrite
        extract_args$resume <- resume
        climate <- do.call(shift_extract, c(list(future_stage), extract_args))
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
            reference_filters <- utils::modifyList(
                selection_filters,
                list(experiment_id = reference_spec@experiment)
            )
            reference_args <- list(
                site = meta$site,
                periods = reference_spec@periods,
                variables = epw_morph_variables(meta$method@recipe),
                time = shift_periods_time(reference_spec@periods),
                filters = reference_filters,
                method = control@extraction_method,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            )
            reference_climate <- do.call(shift_extract, c(list(reference_stage), reference_args))
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
        cases <- shift__case_fulfilment(
            cases,
            future_coverage = shift_coverage(climate),
            reference_coverage = reference_coverage,
            required_variables = epw_morph_variables(meta$method@recipe),
            requires_reference = !is.null(method_reference)
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
                strict = control@strict,
                complete_only = TRUE,
                by = c("source_id", "experiment_id", "variant_label", "period"),
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            ),
            meta$morph
        )
        morphed <- do.call(shift_morph, c(list(climate), morph_args))
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
                resume = resume,
                reporter = reporter
            ),
            meta$epw
        )
        outputs_stage <- do.call(shift_epw, c(list(morphed), epw_args))
        cases <- shift__complete_output_cases(cases, shift_outputs(outputs_stage))
        shift__run_cases_write(store, run_id, cases)
        reporter$cases_updated(cases,
            show = shift__ui_at_least(reporter$ui(), "detail"))
        output_count <- nrow(cases[status == "completed"])
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
        cancellation_context <- shift__failure_context(failure_details)
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
        failure_details <- utils::modifyList(reporter$context(),
            list(outcome = final_status))
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
        reporter$run_failed(cancelled = cancelled)
        if (isTRUE(cancelled)) {
            stop(e)
        }
        failure_context <- shift__failure_context(failure_details)
        abort_message <- c(
                "Future EPW run {.val {run_id}} failed during {.val {current_stage}}.",
                "x" = paste0("Cause: ", shift__error_summary(message)),
                if (nzchar(failure_context)) {
                    c("i" = failure_context)
                },
                "i" = "Resume: {.code shift_resume(\"{run_id}\", store = \"{store$path}\")}",
                "i" = "Logs: {.code shift_logs(\"{run_id}\", store = \"{store$path}\")}"
            )
        cli::cli_abort(
            abort_message,
            class = "epwshiftr_shift_error",
            run_id = run_id,
            store = store$path,
            stage = current_stage,
            original_message = message,
            source_error = e
        )
    })
    result
}

# Compute the user-facing export path for one generated EPW row.
shift__export_target_path <- function(row, dir, separate = TRUE) {
    path <- row$path[[1L]]
    filename <- basename(path)
    if (isTRUE(separate)) {
        parts <- unlist(row[, intersect(c("source_id", "experiment_id", "variant_label", "period"), names(row)), with = FALSE], use.names = FALSE)
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
                                                  strict = TRUE, complete_only = TRUE,
                                                  by = c("source_id", "experiment_id", "variant_label", "period"),
                                                  overwrite = FALSE, resume = TRUE,
                                                  reporter = NULL) {
    checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
    if (!is.null(reference_periods)) {
        checkmate::assert_data_frame(reference_periods)
        checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
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
    morpher <- epw_morpher(store, epw, site_id = site@id, recipe = recipe, label = site@label)
    workflow <- morpher$workflow(
        plan_id = plan_selection$plan_id,
        periods = periods,
        reference_plan_id = reference_selection$plan_id,
        reference_periods = reference_resolved$periods,
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
            original_plan_id = ids$plan_id,
            original_reference_plan_id = reference_resolved$plan_id,
            complete_only = complete_only,
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

S7::method(shift_epw, ShiftMorphed) <- function(x, dir = NULL, separate = TRUE,
                                                export_dir = NULL, overwrite = FALSE,
                                                resume = TRUE, reporter = NULL) {
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
            overwrite = overwrite, resume = resume, reporter = reporter)
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
