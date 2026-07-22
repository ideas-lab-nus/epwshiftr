epwshiftr_cli_shift <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        run = epwshiftr_cli_shift_run(store, args, json = json, jsonl = jsonl, quiet = quiet),
        show = epwshiftr_cli_shift_show(store, args),
        config = epwshiftr_cli_shift_config(store, args),
        watch = epwshiftr_cli_shift_watch(store, args,
            json = json, jsonl = jsonl, quiet = quiet),
        cancel = epwshiftr_cli_shift_cancel(store, args),
        logs = epwshiftr_cli_shift_logs(store, args),
        status = epwshiftr_cli_shift_status(store, args),
        diagnostics = epwshiftr_cli_shift_diagnostics(store, args),
        outputs = epwshiftr_cli_shift_outputs(store, args),
        data = epwshiftr_cli_shift_data(store, args),
        resume = epwshiftr_cli_shift_resume(store, args, json = json, jsonl = jsonl, quiet = quiet),
        epwshiftr_cli_usage_abort(sprintf("Unknown shift command: %s", command))
    )
}

# shift -----------------------------------------------------------------------

epwshiftr_cli_shift_run <- function(store, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--dry-run", "--background", "--no-progress",
            "--reduced-motion", "--verbose", "--debug"),
        options = c("--config")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    plan <- epwshiftr_cli_config_plan(config, store = store)
    background <- isTRUE(parsed$flags[["--background"]])
    if (isTRUE(parsed$flags[["--dry-run"]]) && background) {
        epwshiftr_cli_usage_abort("--dry-run and --background cannot be used together.")
    }
    if (isTRUE(parsed$flags[["--dry-run"]])) {
        return(list(
            status = "dry_run",
            config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
            cases = shift_cases(plan),
            explain = shift_explain(plan)
        ))
    }
    # Machine-readable and quiet modes must never mix reporter text into their
    # stdout contract. Human TTY runs share the dynamic R dashboard.
    progress <- if (isTRUE(quiet) || isTRUE(json) || isTRUE(jsonl) ||
        isTRUE(parsed$flags[["--no-progress"]])) {
        "none"
    } else if (isTRUE(cli::is_dynamic_tty())) {
        "dynamic"
    } else {
        "log"
    }
    ui <- shift_ui(
        progress = progress,
        detail = epwshiftr_cli_shift_detail(parsed),
        motion = epwshiftr_cli_shift_motion(parsed)
    )
    epwshiftr_cli_shift_stage_result(shift_run(plan, background = background, ui = ui))
}

epwshiftr_cli_shift_status <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--run")
    epwshiftr_cli_assert_no_positionals(parsed)
    run <- shift_run_get(epwshiftr_cli_required_single_id(parsed, "--run"), store)
    run@meta$run
}


epwshiftr_cli_shift_diagnostics <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--run")
    epwshiftr_cli_assert_no_positionals(parsed)
    shift_diagnostics(shift_run_get(epwshiftr_cli_required_single_id(parsed, "--run"), store))
}


epwshiftr_cli_shift_outputs <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--run")
    epwshiftr_cli_assert_no_positionals(parsed)
    shift_outputs(shift_run_get(epwshiftr_cli_required_single_id(parsed, "--run"), store))
}


epwshiftr_cli_shift_data <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--run", "--case", "--columns", "--limit")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    columns <- epwshiftr_cli_csv(parsed$options[["--columns"]])
    limit <- epwshiftr_cli_count_or_default(parsed$options[["--limit"]], "--limit", 20L, positive = FALSE)
    shift_data(
        shift_run_get(epwshiftr_cli_required_single_id(parsed, "--run"), store),
        n = limit,
        case_id = epwshiftr_cli_csv(parsed$options[["--case"]]),
        columns = columns
    )
}


# Expose cooperative and force cancellation through the same persisted job
# state used by the R API and detached worker.
epwshiftr_cli_shift_cancel <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = "--force",
        options = "--run"
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    epwshiftr_cli_shift_stage_result(shift_cancel(
        epwshiftr_cli_required_single_id(parsed, "--run"),
        store = store,
        force = isTRUE(parsed$flags[["--force"]])
    ))
}


# Return the persisted stdout/stderr tail for the latest workflow attempt.
epwshiftr_cli_shift_logs <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--run", "--tail")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    tail <- epwshiftr_cli_count_or_default(
        parsed$options[["--tail"]], "--tail", 100L, positive = FALSE
    )
    shift_logs(
        epwshiftr_cli_required_single_id(parsed, "--run"),
        store = store,
        tail = tail
    )
}


# Resume an existing persisted run without rebuilding its resolved CMIP6
# member/grid/index-node selection.
epwshiftr_cli_shift_resume <- function(store, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--background", "--no-progress", "--reduced-motion",
            "--verbose", "--debug"),
        options = "--run"
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    progress <- if (isTRUE(quiet) || isTRUE(json) || isTRUE(jsonl) ||
        isTRUE(parsed$flags[["--no-progress"]])) {
        "none"
    } else if (isTRUE(cli::is_dynamic_tty())) {
        "dynamic"
    } else {
        "log"
    }
    epwshiftr_cli_shift_stage_result(
        shift_resume(
            epwshiftr_cli_required_single_id(parsed, "--run"),
            store = store,
            background = isTRUE(parsed$flags[["--background"]]),
            ui = shift_ui(
                progress = progress,
                detail = epwshiftr_cli_shift_detail(parsed),
                motion = epwshiftr_cli_shift_motion(parsed)
            )
        )
    )
}

# Translate human CLI flags into the same ordered detail contract as the R API.
epwshiftr_cli_shift_detail <- function(parsed) {
    if (isTRUE(parsed$flags[["--debug"]])) {
        return("debug")
    }
    if (isTRUE(parsed$flags[["--verbose"]])) {
        return("detail")
    }
    "normal"
}


# Map the CLI accessibility switch onto the presentation-only motion policy.
epwshiftr_cli_shift_motion <- function(parsed) {
    if (isTRUE(parsed$flags[["--reduced-motion"]])) "reduced" else "auto"
}


# shared parsing --------------------------------------------------------------

epwshiftr_cli_required_option <- function(parsed, option) {
    value <- parsed$options[[option]]
    if (is.null(value) || !length(value) || !nzchar(as.character(value[[1L]]))) {
        epwshiftr_cli_usage_abort(sprintf("%s is required.", option))
    }
    value
}


epwshiftr_cli_ids <- function(value, name, required = TRUE) {
    ids <- epwshiftr_cli_csv(value)
    if (isTRUE(required) && !length(ids)) {
        epwshiftr_cli_usage_abort(sprintf("%s is required.", name))
    }
    ids
}


epwshiftr_cli_required_ids <- function(parsed, option) {
    epwshiftr_cli_ids(parsed$options[[option]], option, required = TRUE)
}


epwshiftr_cli_required_single_id <- function(parsed, option) {
    ids <- epwshiftr_cli_required_ids(parsed, option)
    if (length(ids) != 1L) {
        epwshiftr_cli_usage_abort(sprintf("%s requires exactly one ID.", option))
    }
    ids[[1L]]
}


epwshiftr_cli_number <- function(value, name) {
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out) || !is.finite(out)) {
        epwshiftr_cli_usage_abort(sprintf("%s must be a finite number.", name))
    }
    out
}


epwshiftr_cli_choice <- function(value, choices, name, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    value <- as.character(value[[1L]])
    if (!value %in% choices) {
        epwshiftr_cli_usage_abort(sprintf("%s must be one of: %s.", name, paste(choices, collapse = ", ")))
    }
    value
}


epwshiftr_cli_time_range <- function(value) {
    time <- epwshiftr_cli_csv(value)
    if (length(time) != 2L) {
        epwshiftr_cli_usage_abort("--time must contain START,STOP.")
    }
    time
}


epwshiftr_cli_key_value_list <- function(values, name = "--filter") {
    if (is.null(values) || !length(values)) {
        return(list())
    }
    out <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf("%s expects key=value, got: %s", name, value))
        }
        key <- sub("=.*$", "", value)
        val <- sub("^[^=]*=", "", value)
        key <- trimws(key)
        if (!nzchar(key)) {
            epwshiftr_cli_usage_abort(sprintf("%s has an empty key: %s", name, value))
        }
        out[[key]] <- epwshiftr_cli_csv(val)
    }
    out
}


epwshiftr_cli_periods_from_cli <- function(values) {
    if (is.null(values) || !length(values)) {
        epwshiftr_cli_usage_abort("At least one --period PERIOD=YEARS value is required.")
    }
    periods <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf("--period expects PERIOD=YEARS, got: %s", value))
        }
        name <- trimws(sub("=.*$", "", value))
        years <- sub("^[^=]*=", "", value)
        if (!nzchar(name)) {
            epwshiftr_cli_usage_abort("--period requires a non-empty period name.")
        }
        periods[[name]] <- epwshiftr_cli_years(years)
    }
    do.call(epw_morph_periods, periods)
}


epwshiftr_cli_years <- function(value) {
    pieces <- epwshiftr_cli_csv(as.character(value))
    years <- integer()
    for (piece in pieces) {
        if (grepl(":", piece, fixed = TRUE)) {
            bounds <- trimws(strsplit(piece, ":", fixed = TRUE)[[1L]])
            if (length(bounds) != 2L) {
                epwshiftr_cli_usage_abort(sprintf("Invalid year range: %s", piece))
            }
            rng <- suppressWarnings(as.integer(bounds))
            if (any(is.na(rng))) {
                epwshiftr_cli_usage_abort(sprintf("Invalid year range: %s", piece))
            }
            years <- c(years, seq.int(min(rng), max(rng)))
        } else {
            year <- suppressWarnings(as.integer(piece))
            if (length(year) != 1L || is.na(year)) {
                epwshiftr_cli_usage_abort(sprintf("Invalid year: %s", piece))
            }
            years <- c(years, year)
        }
    }
    unique(years)
}


# config coercion -------------------------------------------------------------

epwshiftr_cli_validate_shift_config <- function(config) {
    if (!identical(as.integer(config$version), 1L)) {
        cli::cli_abort("Only shift workflow config version 1 is supported.")
    }
    epwshiftr_cli_periods_from_config(config$periods, "periods")
    epwshiftr_cli_config_method(config$method)
    epwshiftr_cli_config_climate(config$climate)
    epwshiftr_cli_config_control(config$control)
    invisible(config)
}


epwshiftr_cli_config_section <- function(config, name) {
    value <- config[[name]]
    if (is.null(value)) list() else value
}


# Build the same task-level ShiftPlan used by the R API; the CLI does not own a
# second collect/extract/morph execution path.
epwshiftr_cli_config_plan <- function(config, store) {
    shift_future_epw(
        epw = epwshiftr_cli_config_string(config$epw),
        climate = epwshiftr_cli_config_climate(config$climate),
        periods = config$periods,
        method = epwshiftr_cli_config_method(config$method),
        dir = epwshiftr_cli_config_string(config$dir),
        control = epwshiftr_cli_config_control(config$control),
        store = store,
        dry_run = TRUE
    )
}


# Parse only the nested reference object owned by a method. Missing references
# remain NULL: optional-reference methods use their baseline input, while a
# genuinely required-reference backend rejects the method during construction.
epwshiftr_cli_config_reference <- function(reference) {
    if (is.null(reference)) {
        return(NULL)
    }

    reference <- epwshiftr_cli_config_section(list(reference = reference), "reference")
    mode <- epwshiftr_cli_config_choice(reference$mode, c("historical", "plan"), default = NULL)
    if (is.null(mode)) {
        epwshiftr_cli_usage_abort("method.reference.mode is required.")
    }
    periods <- epwshiftr_cli_periods_from_config(reference$periods, "method.reference.periods")

    if (identical(mode, "plan")) {
        plan_id <- epwshiftr_cli_config_character(reference$plan_id, default = NULL)
        if (is.null(plan_id)) {
            epwshiftr_cli_usage_abort("method.reference.plan_id is required when method.reference.mode is plan.")
        }
        return(shift_reference_plan(plan_id, periods))
    }

    shift_reference_historical(
        periods = periods,
        experiment = epwshiftr_cli_config_string(reference$experiment, default = "historical"),
        activity = epwshiftr_cli_config_string(reference$activity, default = "CMIP"),
        match = epwshiftr_cli_config_character(
            reference$match,
            default = c("source_id", "variant_label", "frequency", "table_id", "grid_label")
        ),
        filters = epwshiftr_cli_config_named_list(reference$filters),
        options = epwshiftr_cli_config_named_list(reference$options)
    )
}


# Construct a complete morph method and preserve the distinction between a
# missing optional reference and an explicitly configured one.
epwshiftr_cli_config_method <- function(config) {
    config <- epwshiftr_cli_config_section(list(method = config), "method")
    name <- tolower(epwshiftr_cli_config_string(config$name))
    methods <- epwshiftr_cli_recipe_methods(config$methods)
    profile <- epwshiftr_cli_config_string(config$profile, default = NULL)
    options <- cli_shift__belcher_options(epwshiftr_cli_config_named_list(config$options))
    reference <- epwshiftr_cli_config_reference(config$reference)
    if (identical(name, "belcher")) {
        return(belcher(
            reference = reference,
            methods = methods,
            profile = shift_coalesce(profile, "enhanced"),
            options = options
        ))
    }
    shift_morph_method(
        epw_morph_recipe(
            name = name,
            backend = name,
            methods = methods,
            profile = profile,
            options = options
        ),
        reference = reference
    )
}


# Construct one complete climate specification; required model/scenario fields
# are never recovered from other config sections or filled with defaults.
epwshiftr_cli_config_climate <- function(config) {
    config <- epwshiftr_cli_config_section(list(climate = config), "climate")
    provider <- epwshiftr_cli_config_string(config$provider)
    if (!identical(tolower(provider), "cmip6")) {
        epwshiftr_cli_usage_abort(sprintf("Unsupported climate provider: %s", provider))
    }
    shift_cmip6(
        model = epwshiftr_cli_config_character(config$model),
        scenarios = epwshiftr_cli_config_character(config$scenarios),
        member = epwshiftr_cli_config_character(config$member, default = NULL),
        grid = epwshiftr_cli_config_string(config$grid, default = NULL),
        frequency = epwshiftr_cli_config_string(config$frequency, default = "mon"),
        table = cli_shift__table_spec(config$table),
        activity = epwshiftr_cli_config_string(config$activity, default = "ScenarioMIP"),
        index_nodes = epwshiftr_cli_config_character(config$index_nodes, default = NULL),
        data_node = epwshiftr_cli_config_string(config$data_node, default = NULL),
        filters = epwshiftr_cli_config_named_list(config$filters)
    )
}


# Decode either a scalar table pin or a JSON object of variable-specific table
# overrides without flattening away object names.
cli_shift__table_spec <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    if (is.list(value)) {
        nms <- names(value)
        if (is.null(nms) || any(!nzchar(nms))) {
            epwshiftr_cli_usage_abort(
                "climate.table must be a string or a named variable-to-table object."
            )
        }
        value <- vapply(value, epwshiftr_cli_config_string,
            character(1L))
    }
    value_names <- names(value)
    value <- as.character(value)
    names(value) <- value_names
    if (length(value) == 1L && is.null(names(value))) {
        return(value[[1L]])
    }
    if (is.null(names(value)) || any(!nzchar(names(value)))) {
        epwshiftr_cli_usage_abort(
            "climate.table must be a string or a named variable-to-table object."
        )
    }
    value
}


# Parse task-wide completion and I/O policy without stage-list overrides.
epwshiftr_cli_config_control <- function(config) {
    config <- epwshiftr_cli_config_section(list(control = config), "control")
    shift_control(
        strict = epwshiftr_cli_config_flag(config$strict, default = TRUE),
        allow_partial = epwshiftr_cli_config_flag(config$allow_partial, default = FALSE),
        download = epwshiftr_cli_config_choice(config$download, c("auto", "always", "never"), default = "auto"),
        resume = epwshiftr_cli_config_flag(config$resume, default = TRUE),
        overwrite = epwshiftr_cli_config_flag(config$overwrite, default = FALSE),
        extraction_method = epwshiftr_cli_config_choice(config$extraction_method, ESG_GRID_METHOD_CHOICES, default = "nearest"),
        output_layout = epwshiftr_cli_config_choice(config$output_layout, c("nested", "flat"), default = "nested")
    )
}


epwshiftr_cli_periods_from_config <- function(value, field = "extract.periods") {
    if (is.null(value)) {
        epwshiftr_cli_usage_abort(sprintf("Config field %s is required.", field))
    }
    if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value)))) {
        epwshiftr_cli_usage_abort(sprintf("Config field %s must be a named object.", field))
    }
    periods <- lapply(value, epwshiftr_cli_years)
    do.call(epw_morph_periods, periods)
}


epwshiftr_cli_config_character <- function(value, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    if (is.list(value)) {
        value <- unlist(value, use.names = FALSE)
    }
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) default else value
}


epwshiftr_cli_config_string <- function(value, default = NULL) {
    value <- epwshiftr_cli_config_character(value, default = NULL)
    if (is.null(value)) {
        return(default)
    }
    if (length(value) != 1L) {
        epwshiftr_cli_usage_abort("Expected a single string value.")
    }
    value[[1L]]
}


epwshiftr_cli_config_number <- function(value, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out) || !is.finite(out)) {
        epwshiftr_cli_usage_abort("Expected a finite numeric value.")
    }
    out
}


epwshiftr_cli_config_count <- function(value, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    out <- epwshiftr_cli_config_number(value)
    checkmate::assert_count(out, positive = TRUE)
    as.integer(out)
}


epwshiftr_cli_config_flag <- function(value, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    if (is.logical(value) && length(value) == 1L && !is.na(value)) {
        return(value)
    }
    epwshiftr_cli_bool(as.character(value), "config flag", default = default)
}


epwshiftr_cli_config_choice <- function(value, choices, default = NULL) {
    value <- epwshiftr_cli_config_string(value, default = default)
    if (!is.null(value) && !value %in% choices) {
        epwshiftr_cli_usage_abort(sprintf("Expected one of: %s.", paste(choices, collapse = ", ")))
    }
    value
}


epwshiftr_cli_config_time <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    epwshiftr_cli_config_character(value, default = NULL)
}


epwshiftr_cli_config_limit <- function(value, default = FALSE) {
    if (is.null(value)) {
        return(default)
    }
    if (is.logical(value) && length(value) == 1L && !is.na(value)) {
        return(value)
    }
    epwshiftr_cli_config_count(value, default = default)
}


epwshiftr_cli_config_named_list <- function(value) {
    if (is.null(value)) {
        return(list())
    }
    if (!is.list(value)) {
        epwshiftr_cli_usage_abort("Expected a named object.")
    }
    nms <- names(value)
    if (is.null(nms) || any(!nzchar(nms))) {
        epwshiftr_cli_usage_abort("Expected a named object.")
    }
    value
}


epwshiftr_cli_list_value <- function(x, name, default = NULL) {
    value <- x[[name]]
    if (is.null(value)) default else value
}


epwshiftr_cli_download_args_from_config <- function(config) {
    allowed <- c(
        "session_label", "replica", "service", "probe", "probe_concurrency",
        "probe_cache_seconds", "strategy", "mode"
    )
    out <- list()
    for (name in allowed) {
        value <- config[[name]]
        if (!is.null(value)) {
            out[[name]] <- switch(
                name,
                probe = epwshiftr_cli_config_flag(value, default = TRUE),
                probe_concurrency = epwshiftr_cli_config_count(value, default = NULL),
                probe_cache_seconds = epwshiftr_cli_config_count(value, default = 3600L),
                value
            )
        }
    }
    out
}


epwshiftr_cli_recipe <- function(value = "belcher", methods = NULL,
                                 profile = NULL, options = NULL) {
    value <- tolower(epwshiftr_cli_config_string(value, default = "belcher"))
    if (!value %in% epw_morph_backends()) {
        epwshiftr_cli_usage_abort(sprintf("Unknown morph recipe/backend: %s", value))
    }
    methods <- epwshiftr_cli_recipe_methods(methods)
    epw_morph_recipe(
        value,
        methods = methods,
        profile = profile,
        options = cli_shift__belcher_options(options)
    )
}


# Coerce only the typed Belcher values accepted by command-line key/value
# inputs. Other option values remain character strings for central validation.
cli_shift__belcher_options <- function(options) {
    if (is.null(options) || !length(options)) {
        return(NULL)
    }
    if (!is.list(options)) {
        options <- as.list(options)
    }
    if ("transition_hours" %in% names(options)) {
        transition_hours <- suppressWarnings(as.integer(options$transition_hours[[1L]]))
        if (is.na(transition_hours)) {
            epwshiftr_cli_usage_abort("Belcher option transition_hours must be an integer between 0 and 336.")
        }
        options$transition_hours <- transition_hours
    }
    options
}


epwshiftr_cli_recipe_methods <- function(methods) {
    if (is.null(methods) || !length(methods)) {
        return(NULL)
    }
    if (is.list(methods)) {
        if (is.null(names(methods)) || any(!nzchar(names(methods)))) {
            epwshiftr_cli_usage_abort("morph.methods must be a named object.")
        }
        methods <- vapply(methods, function(x) as.character(x[[1L]]), character(1L))
    } else {
        methods <- as.character(methods)
    }
    if (is.null(names(methods)) || any(!nzchar(names(methods)))) {
        epwshiftr_cli_usage_abort("morph.methods must be named.")
    }
    methods
}


# store backed helpers --------------------------------------------------------

epwshiftr_cli_query_status <- function(store, query_id = NULL) {
    query_id <- epwshiftr_cli_ids(query_id, "--query", required = FALSE)
    native <- tryCatch(store$workflow_status(query_id = query_id), error = function(e) data.table::data.table())
    native_ids <- if (nrow(native) && "query_id" %in% names(native)) native$query_id else character()
    fallback_ids <- if (is.null(query_id)) {
        rows <- tryCatch(store$query("SELECT DISTINCT query_id FROM file_catalog"), error = function(e) data.table::data.table())
        if (nrow(rows)) rows$query_id else character()
    } else {
        setdiff(query_id, native_ids)
    }
    fallback <- epwshiftr_cli_file_catalog_status(store, fallback_ids)
    data.table::rbindlist(list(native, fallback), use.names = TRUE, fill = TRUE)
}


epwshiftr_cli_file_catalog_status <- function(store, query_id) {
    query_id <- unique(as.character(query_id))
    query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
    if (!length(query_id)) {
        return(data.table::data.table())
    }
    catalog <- shift_query_maybe(store, sprintf(
        paste(
            "SELECT query_id,",
            "COUNT(*) AS file_current,",
            "COUNT(DISTINCT file_key) AS file_total",
            "FROM file_catalog",
            "WHERE query_id IN (%s)",
            "GROUP BY query_id"
        ),
        shift_stage_query_ids(query_id)
    ))
    if (!nrow(catalog)) {
        return(catalog)
    }
    catalog[, `:=`(
        label = NA_character_,
        tracked = NA,
        bytes_missing = NA_real_,
        download_retryable = 0L,
        download_incomplete = FALSE,
        last_download_session_id = NA_character_
    )]
    plans <- shift_query_maybe(store, sprintf(
        paste(
            "SELECT query_id, status, COUNT(*) AS n",
            "FROM extraction_plan",
            "WHERE query_id IN (%s)",
            "GROUP BY query_id, status"
        ),
        shift_stage_query_ids(catalog$query_id)
    ))
    if (nrow(plans)) {
        wide <- data.table::dcast(plans, query_id ~ status, value.var = "n", fill = 0L)
        data.table::setnames(wide, setdiff(names(wide), "query_id"), paste0("extract_", setdiff(names(wide), "query_id")))
        catalog <- merge(catalog, wide, by = "query_id", all.x = TRUE, sort = FALSE)
    }
    catalog[]
}


epwshiftr_cli_morph_output_rows <- function(store, morph_id = NULL) {
    if (is.null(morph_id)) {
        return(store$query("SELECT * FROM epw_output"))
    }
    shift_epw_output_rows(store, morph_id)
}


epwshiftr_cli_morph_status_rows <- function(store, morph_id = NULL) {
    if (is.null(morph_id)) {
        return(store$query("SELECT * FROM epw_morph_plan"))
    }
    shift_morph_plan(store, morph_id)
}


epwshiftr_cli_climate_stage_from_plan <- function(store, plan_id, periods, epw) {
    plans <- shift_extraction_plan(store, plan_id)
    if (!nrow(plans)) {
        epwshiftr_cli_usage_abort("No extraction plan rows were found for --plan.")
    }
    query_id <- unique(plans$query_id)
    query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
    if (!length(query_id)) {
        epwshiftr_cli_usage_abort("Could not resolve File query IDs from --plan.")
    }

    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = store$path,
        ids = list(query_id = query_id, plan_id = plan_id),
        meta = list(
            site = shift_site(epw = epw),
            periods = periods,
            plan = plans
        )
    )
}


epwshiftr_cli_morpher_from_morph_id <- function(store, morph_id) {
    if (length(morph_id) != 1L) {
        epwshiftr_cli_usage_abort("morph epw requires exactly one morph ID.")
    }
    row <- shift_query_maybe(store, sprintf(
        paste(
            "SELECT p.morph_id, p.recipe_json, s.path, s.site_id, s.label",
            "FROM epw_morph_plan p",
            "LEFT JOIN epw_source s ON p.epw_id = s.epw_id",
            "WHERE p.morph_id IN (%s)"
        ),
        shift_stage_query_ids(morph_id)
    ))
    if (!nrow(row) || is.na(row$path[[1L]]) || !nzchar(row$path[[1L]])) {
        cli::cli_abort("Could not resolve the baseline EPW path for morph ID {.val {morph_id}}.")
    }
    epw <- store_abs_path(row$path[[1L]], root = store$path)
    epw_morpher(
        store,
        epw,
        site_id = epwshiftr_cli_na_null(row$site_id[[1L]]),
        label = epwshiftr_cli_na_null(row$label[[1L]]),
        recipe = epwshiftr_cli_recipe_from_json(row$recipe_json[[1L]])
    )
}

# Reconstruct the minimum ShiftMorphed graph required by the public EPW and
# retry APIs. Climate data remain store-backed; only stable IDs, period labels,
# the baseline EPW identity, and the recipe are materialized here.
epwshiftr_cli_morphed_stage_from_morph_id <- function(store, morph_id) {
    if (length(morph_id) != 1L) {
        epwshiftr_cli_usage_abort("Exactly one morph ID is required.")
    }
    row <- shift_query_maybe(store, sprintf(
        paste(
            "SELECT p.*, s.path, s.site_id, s.label",
            "FROM epw_morph_plan p",
            "LEFT JOIN epw_source s ON p.epw_id = s.epw_id",
            "WHERE p.morph_id IN (%s)"
        ),
        shift_stage_query_ids(morph_id)
    ))
    if (!nrow(row) || is.na(row$path[[1L]]) || !nzchar(row$path[[1L]])) {
        cli::cli_abort("Could not reconstruct morph ID {.val {morph_id}}.")
    }
    summary <- shift_query_maybe(store, sprintf(
        "SELECT * FROM epw_climate_summary WHERE summary_id = %s",
        shift_sql_string(row$summary_id[[1L]])
    ))
    if (!nrow(summary)) {
        cli::cli_abort("Morph ID {.val {morph_id}} has no climate summary.")
    }
    period_rows <- unique(summary[, .(period, years_json)])
    period_values <- lapply(seq_len(nrow(period_rows)), function(i) {
        as.integer(jsonlite::fromJSON(period_rows$years_json[[i]],
            simplifyVector = TRUE))
    })
    names(period_values) <- period_rows$period
    periods <- do.call(epw_morph_periods, period_values)
    epw <- store_abs_path(row$path[[1L]], root = store$path)
    site <- shift_site(
        id = epwshiftr_cli_na_null(row$site_id[[1L]]),
        label = epwshiftr_cli_na_null(row$label[[1L]]),
        epw = epw
    )
    plan_id <- unique(summary$plan_id)
    climate <- shift_stage_new(ShiftClimate, "climate",
        store_path = store$path,
        ids = list(plan_id = plan_id, summary_id = row$summary_id[[1L]]),
        meta = list(site = site, periods = periods,
            variables = unique(summary$variable_id),
            coverage = store$coverage(plan_id = plan_id)))

    reference_plan_id <- NULL
    reference_periods <- NULL
    if (!is.na(row$reference_summary_id[[1L]]) &&
        nzchar(row$reference_summary_id[[1L]])) {
        reference <- shift_query_maybe(store, sprintf(
            "SELECT * FROM epw_climate_summary WHERE summary_id = %s",
            shift_sql_string(row$reference_summary_id[[1L]])
        ))
        if (nrow(reference)) {
            reference_plan_id <- unique(reference$plan_id)
            reference_rows <- unique(reference[, .(period, years_json)])
            reference_values <- lapply(seq_len(nrow(reference_rows)),
                function(i) as.integer(jsonlite::fromJSON(
                    reference_rows$years_json[[i]], simplifyVector = TRUE)))
            names(reference_values) <- reference_rows$period
            reference_periods <- do.call(epw_morph_periods, reference_values)
        }
    }
    by <- tryCatch(as.character(jsonlite::fromJSON(row$by_json[[1L]],
        simplifyVector = TRUE)), error = function(e) {
        c("source_id", "experiment_id", "variant_label", "period")
    })
    shift_stage_new(ShiftMorphed, "morphed", store_path = store$path,
        ids = list(plan_id = plan_id, summary_id = row$summary_id[[1L]],
            baseline_id = row$baseline_id[[1L]], morph_id = morph_id),
        meta = list(climate = climate, baseline = site,
            recipe = epwshiftr_cli_recipe_from_json(row$recipe_json[[1L]]),
            reference_plan_id = reference_plan_id,
            reference_periods = reference_periods,
            by = by,
            strict = isTRUE(row$strict[[1L]])))
}


epwshiftr_cli_recipe_from_json <- function(json) {
    parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$name)) {
        return(epw_morph_recipe("belcher"))
    }
    backend <- if (is.null(parsed$backend)) parsed$name else parsed$backend
    is_belcher <- backend %in% c("belcher", "belcher_absolute")
    # Records written before profiles existed remain on the historical
    # numerical path instead of adopting enhanced defaults during recovery.
    profile <- if (is.null(parsed$profile)) {
        if (is_belcher) "legacy" else "default"
    } else {
        parsed$profile
    }
    methods <- parsed$methods
    if (is.list(methods) && !is.data.frame(methods)) {
        methods <- unlist(methods, use.names = TRUE)
    }
    # Pre-profile recipe JSON encoded the full named vector as an unnamed
    # array. Recover those positions from the backend method contract; newer
    # records use a named JSON object and do not enter this compatibility path.
    if (!is.null(methods) && (is.null(names(methods)) || any(!nzchar(names(methods))))) {
        backend_spec <- suppressWarnings(epw_morph_backend(backend))
        defaults <- if (is_belcher) {
            morpher__belcher_profile_methods(backend_spec, profile)
        } else {
            backend_spec$methods()
        }
        if (length(methods) == length(defaults)) {
            names(methods) <- names(defaults)
        } else {
            methods <- NULL
        }
    }
    epw_morph_recipe(
        parsed$name,
        backend = backend,
        methods = methods,
        profile = profile,
        options = cli_shift__belcher_options(parsed$options)
    )
}


epwshiftr_cli_na_null <- function(value) {
    if (is.null(value) || length(value) == 0L || is.na(value) || !nzchar(as.character(value))) {
        return(NULL)
    }
    as.character(value)
}


epwshiftr_cli_query_diagnostics <- function(store, query_id = NULL) {
    status <- epwshiftr_cli_query_status(store, query_id)
    if (!nrow(status)) {
        return(shift_diagnostics_empty())
    }
    diagnostics <- vector("list", nrow(status))
    for (i in seq_len(nrow(status))) {
        row <- status[i]
        rows <- list()
        if ("download_incomplete" %in% names(row) && isTRUE(row$download_incomplete[[1L]])) {
            rows[[length(rows) + 1L]] <- shift_diagnostic(
                "download", "warning", "download_incomplete",
                "One or more downloads are incomplete.",
                query_id = row$query_id[[1L]],
                action = "Run `download run` or inspect `download status`."
            )
        }
        extract_failed <- names(row)[startsWith(names(row), "extract_failed")]
        if (length(extract_failed) && suppressWarnings(as.numeric(row[[extract_failed[[1L]]]][[1L]])) > 0) {
            rows[[length(rows) + 1L]] <- shift_diagnostic(
                "extract", "error", "extract_failed",
                "One or more extraction plans failed.",
                query_id = row$query_id[[1L]],
                action = "Run `extract coverage` and inspect the affected plan ID."
            )
        }
        diagnostics[[i]] <- if (length(rows)) do.call(shift_bind_diagnostics, rows) else shift_diagnostics_empty()
    }
    do.call(shift_bind_diagnostics, diagnostics)
}


epwshiftr_cli_morph_diagnostics <- function(store, morph_id = NULL) {
    status <- epwshiftr_cli_morph_status_rows(store, morph_id)
    if (!nrow(status)) {
        return(shift_diagnostics_empty())
    }
    diagnostics <- vector("list", nrow(status))
    for (i in seq_len(nrow(status))) {
        id <- status$morph_id[[i]]
        morpher <- tryCatch(epwshiftr_cli_morpher_from_morph_id(store, id), error = function(e) NULL)
        diagnostics[[i]] <- if (is.null(morpher)) {
            shift_diagnostic(
                "morph", "error", "missing_morph_inputs",
                "The morphing plan exists but its baseline EPW inputs could not be resolved.",
                morph_id = id,
                action = "Inspect `morph status` and the `epw_source` manifest rows."
            )
        } else {
            tryCatch(morpher$diagnose(id), error = function(e) {
                shift_diagnostic(
                    "morph", "error", "morph_diagnostics_failed",
                    conditionMessage(e),
                    morph_id = id,
                    action = "Inspect `morph status` and rerun `morph run` if needed."
                )
            })
        }
    }
    do.call(shift_bind_diagnostics, diagnostics)
}


epwshiftr_cli_read_extracted_data <- function(store, results, n = 20L, columns = NULL) {
    results <- data.table::as.data.table(results)
    if (!nrow(results)) {
        return(data.table::data.table())
    }
    pieces <- vector("list", nrow(results))
    remaining <- shift_data_limit(n)
    for (i in seq_len(nrow(results))) {
        if (!is.infinite(remaining) && remaining <= 0L) {
            break
        }
        path <- store_abs_path(results$output_path[[i]], root = store$path)
        if (!file.exists(path)) {
            cli::cli_abort("Extracted Parquet data file is missing: {.path {path}}.")
        }
        limit <- if (is.infinite(remaining)) Inf else remaining
        dt <- shift_read_parquet(store, path, n = limit)
        dt <- shift_add_constant_columns(dt, list(
            result_id = results$result_id[[i]],
            plan_id = results$plan_id[[i]],
            output_path = results$output_path[[i]]
        ))
        pieces[[i]] <- shift_select_data_columns(dt, columns, "extracted")
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


# compact workflow results ----------------------------------------------------

epwshiftr_cli_shift_stage_result <- function(stage) {
    if (!S7::S7_inherits(stage, ShiftRun)) {
        epwshiftr_cli_usage_abort("The unified shift runner did not return a ShiftRun.")
    }
    ids <- shift_ids(stage)
    diagnostics <- shift_diagnostics(stage)
    outputs <- shift_outputs(stage)
    list(
        status = shift_status(stage),
        run_id = ids$run_id,
        query_id = ids$query_id,
        reference_query_id = ids$reference_query_id,
        morph_id = ids$morph_id,
        diagnostic_count = nrow(diagnostics),
        cases = shift_cases(stage),
        missing = shift_missing(stage),
        outputs = outputs,
        next_steps = epwshiftr_cli_shift_next_steps(ids$run_id)
    )
}


epwshiftr_cli_shift_next_steps <- function(run_id) {
    data.frame(
        step = c("watch", "status", "show", "outputs", "diagnostics", "logs", "cancel", "resume"),
        command = sprintf(
            "epwshiftr shift %s --run %s",
            c("watch", "status", "show", "outputs", "diagnostics", "logs", "cancel", "resume"),
            run_id
        ),
        stringsAsFactors = FALSE
    )
}

epwshiftr_cli_coverage_summary <- function(coverage) {
    coverage <- data.table::as.data.table(coverage)
    if (!nrow(coverage)) {
        return(data.frame(plan_count = 0L, complete = 0L, failed = 0L, output_rows = 0L))
    }
    data.frame(
        plan_count = nrow(coverage),
        complete = sum(coverage$complete %in% TRUE),
        failed = sum(coverage$status %in% "failed"),
        output_rows = sum(coverage$output_rows, na.rm = TRUE)
    )
}
