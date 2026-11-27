# epwshiftr_cli_shift {{{
epwshiftr_cli_shift <- function(
    store,
    command,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    ui_cmds <- list(
        run = epwshiftr_cli_shift_run,
        config = epwshiftr_cli_shift_config,
        summary = cli_shift__summary,
        watch = epwshiftr_cli_shift_watch,
        resume = epwshiftr_cli_shift_resume
    )
    plain_cmds <- list(
        show = epwshiftr_cli_shift_show,
        list = cli_shift__history,
        cancel = epwshiftr_cli_shift_cancel,
        logs = epwshiftr_cli_shift_logs,
        status = epwshiftr_cli_shift_status,
        diagnostics = epwshiftr_cli_shift_diagnostics,
        outputs = epwshiftr_cli_shift_outputs,
        data = epwshiftr_cli_shift_data
    )
    if (command %in% names(ui_cmds)) {
        return(ui_cmds[[command]](
            store,
            args,
            json = json,
            jsonl = jsonl,
            quiet = quiet
        ))
    }
    if (command %in% names(plain_cmds)) {
        return(plain_cmds[[command]](store, args))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown shift command: %s", command))
}
# }}}

# shift
# epwshiftr_cli_shift_run {{{
epwshiftr_cli_shift_run <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--dry-run", "--background", CLI_UI_FLAGS),
        options = "--config",
        no_positionals = TRUE
    )
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    background <- isTRUE(parsed$flags[["--background"]])
    if (isTRUE(parsed$flags[["--dry-run"]]) && background) {
        epwshiftr_cli_usage_abort(
            "--dry-run and --background cannot be used together."
        )
    }
    ui <- epwshiftr_cli_task_ui(
        parsed,
        json = json,
        jsonl = jsonl,
        quiet = quiet
    )
    ui@batch_receipt <- FALSE
    if (isTRUE(parsed$flags[["--dry-run"]])) {
        plan <- epwshiftr_cli_config_plan(config, store = store, ui = ui)
        result <- shift_batch_ui__snapshot(plan, refresh = FALSE)
        result$status <- "dry_run"
        result$batch_id <- plan@ids$batch_id
        result$config <- normalizePath(
            config_path,
            winslash = "/",
            mustWork = TRUE
        )
        result$intent <- cli_shift__config_intent(config)
        result$explain <- shift_explain(plan)
        result$next_steps <- epwshiftr_cli_shift_next_steps(
            plan@ids$batch_id,
            batch = TRUE,
            store = store
        )
        attr(result, "shift_ui_detail") <- ui@detail
        return(result)
    }
    # Execute the high-level entry point directly so a completed batch can use
    # its receipt fast path before any catalog or child planning work.
    result <- epwshiftr_cli_shift_stage_result(epwshiftr_cli_config_plan(
        config,
        store = store,
        dry_run = FALSE,
        background = background,
        ui = ui
    ))
    attr(result, "shift_ui_detail") <- ui@detail
    result
}
# }}}

# cli_shift__parse_target {{{
cli_shift__parse_target <- function(
    store,
    args,
    flags = character(),
    options = character()
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = flags,
        options = c("--run", "--batch", options),
        no_positionals = TRUE
    )
    list(parsed = parsed, target = cli_shift__target(parsed, store))
}
# }}}

# epwshiftr_cli_shift_status {{{
epwshiftr_cli_shift_status <- function(store, args) {
    run <- cli_shift__parse_target(store, args)$target
    if (S7::S7_inherits(run, ShiftBatch)) {
        snapshot <- shift_batch_ui__snapshot(run, refresh = FALSE)
        return(list(batch = snapshot$batch, children = snapshot$children))
    }
    run@meta$run
}
# }}}

# epwshiftr_cli_shift_diagnostics {{{
epwshiftr_cli_shift_diagnostics <- function(store, args) {
    shift_diagnostics(cli_shift__parse_target(store, args)$target)
}
# }}}

# epwshiftr_cli_shift_outputs {{{
epwshiftr_cli_shift_outputs <- function(store, args) {
    cli_shift__output_metadata(shift_outputs(
        cli_shift__parse_target(store, args)$target
    ))
}
# }}}

# epwshiftr_cli_shift_data {{{
epwshiftr_cli_shift_data <- function(store, args) {
    req <- cli_shift__parse_target(
        store,
        args,
        options = c("--case", "--columns", "--limit")
    )
    limit <- epwshiftr_cli_count_or_default(
        req$parsed$options[["--limit"]],
        "--limit",
        20L,
        positive = FALSE
    )
    shift_data(
        req$target,
        n = limit,
        case_id = epwshiftr_cli_csv(req$parsed$options[["--case"]]),
        columns = epwshiftr_cli_csv(req$parsed$options[["--columns"]])
    )
}
# }}}

# Expose cooperative and force cancellation through the same persisted job
# state used by the R API and detached worker.
# epwshiftr_cli_shift_cancel {{{
epwshiftr_cli_shift_cancel <- function(store, args) {
    req <- cli_shift__parse_target(store, args, flags = "--force")
    epwshiftr_cli_shift_stage_result(shift_cancel(
        req$target,
        force = isTRUE(req$parsed$flags[["--force"]])
    ))
}
# }}}

# Return the persisted stdout/stderr tail for the latest workflow attempt.
# epwshiftr_cli_shift_logs {{{
epwshiftr_cli_shift_logs <- function(store, args) {
    req <- cli_shift__parse_target(store, args, options = "--tail")
    tail <- epwshiftr_cli_count_or_default(
        req$parsed$options[["--tail"]],
        "--tail",
        100L,
        positive = FALSE
    )
    shift_logs(req$target, tail = tail)
}
# }}}

# Resume an existing persisted run without rebuilding its resolved CMIP6
# member/grid/index-node selection.
# epwshiftr_cli_shift_resume {{{
epwshiftr_cli_shift_resume <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    req <- cli_shift__parse_target(
        store,
        args,
        flags = c("--background", CLI_UI_FLAGS)
    )
    ui <- epwshiftr_cli_task_ui(
        req$parsed,
        json = json,
        jsonl = jsonl,
        quiet = quiet
    )
    ui@batch_receipt <- FALSE
    result <- epwshiftr_cli_shift_stage_result(
        shift_resume(
            req$target,
            background = isTRUE(req$parsed$flags[["--background"]]),
            ui = ui
        )
    )
    attr(result, "shift_ui_detail") <- ui@detail
    result
}
# }}}

# Translate human CLI flags into the same ordered detail contract as the R API.
# epwshiftr_cli_shift_detail {{{
epwshiftr_cli_shift_detail <- function(parsed) {
    epwshiftr_cli_task_ui(parsed)@detail
}
# }}}

# Map the CLI accessibility switch onto the presentation-only motion policy.
# epwshiftr_cli_shift_motion {{{
epwshiftr_cli_shift_motion <- function(parsed) {
    epwshiftr_cli_task_ui(parsed)@motion
}
# }}}

# shared parsing
# Require an explicit, mutually exclusive identity before opening any store.
# Batch IDs address independent child stores through the persisted receipt.
# cli_shift__target {{{
cli_shift__target <- function(parsed, store) {
    batch <- parsed$options[["--batch"]]
    run <- parsed$options[["--run"]]
    if (is.null(batch) == is.null(run)) {
        epwshiftr_cli_usage_abort("Supply exactly one of --run or --batch.")
    }
    if (!is.null(batch)) {
        return(shift_batch_get(
            epwshiftr_cli_required_single_id(parsed, "--batch"),
            store = store
        ))
    }
    shift_run_get(
        epwshiftr_cli_required_single_id(parsed, "--run"),
        store = store
    )
}
# }}}

# Promote the manifest's scientific field roles to inspectable output columns.
# These lists are descriptive provenance and do not introduce comparison scores.
# cli_shift__output_metadata {{{
cli_shift__output_metadata <- function(outputs) {
    outputs <- data.table::as.data.table(data.table::copy(outputs))
    if (!nrow(outputs) || !"provenance_json" %in% names(outputs)) {
        return(outputs)
    }
    roles <- lapply(outputs$provenance_json, function(value) {
        if (is.na(value) || !nzchar(value)) {
            return(list())
        }
        tryCatch(
            jsonlite::fromJSON(value)$weather_field_roles,
            error = function(error) list()
        )
    })
    for (name in c(
        "transformed_fields",
        "derived_fields",
        "physically_closed_fields",
        "inherited_fields"
    )) {
        data.table::set(
            outputs,
            j = name,
            value = lapply(roles, function(role) {
                as.character(shift_stage__coalesce(role[[name]], character()))
            })
        )
    }
    outputs
}
# }}}

# epwshiftr_cli_required_option {{{
epwshiftr_cli_required_option <- function(parsed, option) {
    value <- parsed$options[[option]]
    if (!length(value) || !nzchar(as.character(value[[1L]]))) {
        epwshiftr_cli_usage_abort(sprintf("%s is required.", option))
    }
    value
}
# }}}

# epwshiftr_cli_ids {{{
epwshiftr_cli_ids <- function(value, name, required = TRUE) {
    ids <- epwshiftr_cli_csv(value)
    if (isTRUE(required) && !length(ids)) {
        epwshiftr_cli_usage_abort(sprintf("%s is required.", name))
    }
    ids
}
# }}}

# epwshiftr_cli_required_ids {{{
epwshiftr_cli_required_ids <- function(parsed, option) {
    epwshiftr_cli_ids(parsed$options[[option]], option, required = TRUE)
}
# }}}

# epwshiftr_cli_required_single_id {{{
epwshiftr_cli_required_single_id <- function(parsed, option) {
    ids <- epwshiftr_cli_required_ids(parsed, option)
    if (length(ids) != 1L) {
        epwshiftr_cli_usage_abort(sprintf(
            "%s requires exactly one ID.",
            option
        ))
    }
    ids[[1L]]
}
# }}}

# epwshiftr_cli_number {{{
epwshiftr_cli_number <- function(value, name) {
    out <- suppressWarnings(as.numeric(value))
    if (!checkmate::test_number(out, finite = TRUE)) {
        epwshiftr_cli_usage_abort(sprintf("%s must be a finite number.", name))
    }
    out
}
# }}}

# epwshiftr_cli_choice {{{
epwshiftr_cli_choice <- function(value, choices, name, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    value <- as.character(value[[1L]])
    if (!value %in% choices) {
        epwshiftr_cli_usage_abort(sprintf(
            "%s must be one of: %s.",
            name,
            paste(choices, collapse = ", ")
        ))
    }
    value
}
# }}}

# epwshiftr_cli_time_range {{{
epwshiftr_cli_time_range <- function(value) {
    time <- epwshiftr_cli_csv(value)
    if (length(time) != 2L) {
        epwshiftr_cli_usage_abort("--time must contain START,STOP.")
    }
    time
}
# }}}

# epwshiftr_cli_key_value_list {{{
epwshiftr_cli_key_value_list <- function(values, name = "--filter") {
    if (!length(values)) {
        return(list())
    }
    out <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf(
                "%s expects key=value, got: %s",
                name,
                value
            ))
        }
        key <- trimws(sub("=.*$", "", value))
        val <- sub("^[^=]*=", "", value)
        if (!nzchar(key)) {
            epwshiftr_cli_usage_abort(sprintf(
                "%s has an empty key: %s",
                name,
                value
            ))
        }
        out[[key]] <- epwshiftr_cli_csv(val)
    }
    out
}
# }}}

# epwshiftr_cli_periods_from_cli {{{
epwshiftr_cli_periods_from_cli <- function(values) {
    if (!length(values)) {
        epwshiftr_cli_usage_abort(
            "At least one --period PERIOD=YEARS value is required."
        )
    }
    periods <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf(
                "--period expects PERIOD=YEARS, got: %s",
                value
            ))
        }
        name <- trimws(sub("=.*$", "", value))
        years <- sub("^[^=]*=", "", value)
        if (!nzchar(name)) {
            epwshiftr_cli_usage_abort(
                "--period requires a non-empty period name."
            )
        }
        periods[[name]] <- epwshiftr_cli_years(years)
    }
    do.call(epw_morph_periods, periods)
}
# }}}

# Share year syntax with the R API while retaining CLI usage-error reporting.
# epwshiftr_cli_years {{{
epwshiftr_cli_years <- function(value) {
    tryCatch(shift_spec__years_value(value), error = function(error) {
        epwshiftr_cli_usage_abort(conditionMessage(error))
    })
}
# }}}

# config coercion
# epwshiftr_cli_validate_shift_config {{{
epwshiftr_cli_validate_shift_config <- function(config) {
    if (!identical(config$version, 3L)) {
        cli::cli_abort("Only shift workflow config version 3 is supported.")
    }
    epwshiftr_cli_periods_from_config(config$periods, "periods")
    shift_batch__transforms(
        methods = config$methods,
        transform = cli_shift__config_transform(config$transform)
    )
    cli_shift__config_reference(config$reference, "reference")
    cli_shift__config_reference(
        config$observed_reference,
        "observed_reference"
    )
    cli_shift__config_reference(config$calibration, "calibration")
    if (!is.null(config$calibration) && !is.null(config$observed_reference)) {
        epwshiftr_cli_usage_abort(
            "Supply either calibration or observed_reference, not both."
        )
    }
    epwshiftr_cli_config_climate(config$climate)
    epwshiftr_cli_config_control(config$control)
    invisible(config)
}
# }}}

# epwshiftr_cli_config_section {{{
epwshiftr_cli_config_section <- function(config, name) {
    shift_stage__coalesce(config[[name]], list())
}
# }}}

# Build the same task-level ShiftPlan used by the R API; the CLI does not own a
# second collect/extract/morph execution path.
# epwshiftr_cli_config_plan {{{
epwshiftr_cli_config_plan <- function(
    config,
    store,
    dry_run = TRUE,
    background = FALSE,
    ui = shift_ui()
) {
    shift_epw_future(
        sites = lapply(config$sites, function(site) do.call(shift_site, site)),
        climate = epwshiftr_cli_config_climate(config$climate),
        periods = config$periods,
        transform = cli_shift__config_transform(config$transform),
        methods = config$methods,
        dir = epwshiftr_cli_config_string(config$dir),
        reference = cli_shift__config_reference(
            config$reference,
            "reference"
        ),
        observed_reference = cli_shift__config_reference(
            config$observed_reference,
            "observed_reference"
        ),
        calibration = cli_shift__config_reference(
            config$calibration,
            "calibration"
        ),
        control = epwshiftr_cli_config_control(config$control),
        store = store,
        dry_run = dry_run,
        background = background,
        ui = ui
    )
}
# }}}

# Parse one execution-owned reference object without attaching it to the
# reusable transform specification.
# cli_shift__config_reference {{{
cli_shift__config_reference <- function(reference, field) {
    checkmate::assert_string(field, min.chars = 1L)
    if (is.null(reference)) {
        return(NULL)
    }

    # Reanalysis intent contains dataset settings only. The source constructor
    # validates provider options and reads credentials only at execution time.
    observational <- field %in% c("observed_reference", "calibration")
    if (observational && !is.null(reference$dataset)) {
        dataset <- epwshiftr_cli_config_choice(
            reference$dataset,
            c("era5", "era6")
        )
        constructor <- get(paste0("shift_", dataset), mode = "function")
        return(do.call(
            constructor,
            c(
                list(
                    years = epwshiftr_cli_years(as.character(reference$years)),
                    product = epwshiftr_cli_config_string(
                        reference$product,
                        default = "single_levels"
                    ),
                    variables = epwshiftr_cli_config_character(
                        reference$variables
                    ),
                    frequency = cli_shift__variable_spec(
                        reference$frequency,
                        paste0(field, ".frequency")
                    ),
                    access = epwshiftr_cli_config_string(
                        reference$access,
                        default = "auto"
                    )
                ),
                epwshiftr_cli_config_named_list(reference$options)
            )
        ))
    }

    reference <- epwshiftr_cli_config_section(
        list(reference = reference),
        "reference"
    )
    modes <- if (observational) {
        "plan"
    } else {
        c("historical", "plan")
    }
    mode <- epwshiftr_cli_config_choice(
        reference$mode,
        modes,
        default = NULL
    )
    if (is.null(mode)) {
        epwshiftr_cli_usage_abort(sprintf("%s.mode is required.", field))
    }
    periods <- epwshiftr_cli_periods_from_config(
        reference$periods,
        sprintf("%s.periods", field)
    )

    if (identical(mode, "plan")) {
        plan_id <- epwshiftr_cli_config_character(
            reference$plan_id,
            default = NULL
        )
        if (is.null(plan_id)) {
            epwshiftr_cli_usage_abort(sprintf(
                "%s.plan_id is required when %s.mode is plan.",
                field,
                field
            ))
        }
        role <- if (observational) {
            "observed_reference"
        } else {
            "model_historical"
        }
        return(shift_reference_from_plan(plan_id, periods, role = role))
    }

    shift_reference__historical(
        periods = periods,
        experiment = epwshiftr_cli_config_string(
            reference$experiment,
            default = "historical"
        ),
        activity = epwshiftr_cli_config_string(
            reference$activity,
            default = "CMIP"
        ),
        match = epwshiftr_cli_config_character(
            reference$match,
            default = c(
                "source_id",
                "variant_label",
                "frequency",
                "table_id",
                "grid_label"
            )
        ),
        filters = epwshiftr_cli_config_named_list(reference$filters),
        options = epwshiftr_cli_config_named_list(reference$options)
    )
}
# }}}

# Construct one reusable transform from the same scale-specific public
# constructors available to R callers.
# cli_shift__config_transform {{{
cli_shift__config_transform <- function(config) {
    if (is.null(config)) {
        return(NULL)
    }
    # Arrays preserve explicit scale/reconstruction choices for each method.
    # A named object retains the established single-transform return type.
    if (is.list(config) && is.null(names(config))) {
        if (!length(config)) {
            epwshiftr_cli_usage_abort("transform must not be an empty array.")
        }
        return(lapply(config, cli_shift__config_transform))
    }
    config <- epwshiftr_cli_config_section(
        list(transform = config),
        "transform"
    )
    scale <- epwshiftr_cli_config_choice(
        config$scale,
        WEATHER_TRANSFORM_SCALES,
        default = NULL
    )
    if (is.null(scale)) {
        epwshiftr_cli_usage_abort("transform.scale is required.")
    }
    method <- tolower(epwshiftr_cli_config_string(config$method))
    reconstruction <- epwshiftr_cli_config_string(
        config$reconstruction,
        default = NULL
    )
    options <- epwshiftr_cli_config_named_list(config$options)
    constructor <- get(
        paste0(scale, "_transform"),
        mode = "function",
        inherits = TRUE
    )
    do.call(
        constructor,
        c(
            list(method = method, reconstruction = reconstruction),
            options
        )
    )
}
# }}}

# Construct one complete climate specification; required model/scenario fields
# are never recovered from other config sections or filled with defaults.
# epwshiftr_cli_config_climate {{{
epwshiftr_cli_config_climate <- function(config) {
    config <- epwshiftr_cli_config_section(list(climate = config), "climate")
    provider <- epwshiftr_cli_config_string(config$provider)
    if (!identical(tolower(provider), "cmip6")) {
        epwshiftr_cli_usage_abort(sprintf(
            "Unsupported climate provider: %s",
            provider
        ))
    }
    shift_cmip6(
        model = config$model,
        scenarios = epwshiftr_cli_config_character(config$scenarios),
        member = epwshiftr_cli_config_character(config$member, default = NULL),
        grid = epwshiftr_cli_config_string(config$grid, default = NULL),
        frequency = cli_shift__variable_spec(
            config$frequency,
            "climate.frequency",
            default = NULL
        ),
        table = cli_shift__table_spec(config$table),
        activity = epwshiftr_cli_config_string(
            config$activity,
            default = "ScenarioMIP"
        ),
        index_nodes = epwshiftr_cli_config_character(
            config$index_nodes,
            default = NULL
        ),
        data_node = epwshiftr_cli_config_string(
            config$data_node,
            default = NULL
        ),
        filters = epwshiftr_cli_config_named_list(config$filters),
        common = epwshiftr_cli_config_flag(config$common, default = TRUE)
    )
}
# }}}

# Decode either a scalar table pin or a JSON object of variable-specific table
# overrides without flattening away object names.
# cli_shift__table_spec {{{
cli_shift__table_spec <- function(value) {
    cli_shift__variable_spec(value, "climate.table", default = NULL)
}
# }}}

# Decode either a scalar value or a JSON object of variable-specific values
# without flattening away the variable names needed by mixed-frequency input.
# cli_shift__variable_spec {{{
cli_shift__variable_spec <- function(value, field, default = NULL) {
    checkmate::assert_string(field, min.chars = 1L)
    if (is.null(value)) {
        return(default)
    }
    if (is.list(value)) {
        nms <- names(value)
        if (is.null(nms) || any(!nzchar(nms))) {
            epwshiftr_cli_usage_abort(sprintf(
                "%s must be a string or a named variable-to-value object.",
                field
            ))
        }
        value <- vapply(value, epwshiftr_cli_config_string, character(1L))
    }
    value_names <- names(value)
    value <- as.character(value)
    names(value) <- value_names
    if (length(value) == 1L && is.null(names(value))) {
        return(value[[1L]])
    }
    if (is.null(names(value)) || any(!nzchar(names(value)))) {
        epwshiftr_cli_usage_abort(sprintf(
            "%s must be a string or a named variable-to-value object.",
            field
        ))
    }
    value
}
# }}}

# Parse task-wide completion and I/O policy without stage-list overrides.
# epwshiftr_cli_config_control {{{
epwshiftr_cli_config_control <- function(config) {
    config <- epwshiftr_cli_config_section(list(control = config), "control")
    shift_control(
        strict = epwshiftr_cli_config_flag(config$strict, default = TRUE),
        allow_partial = epwshiftr_cli_config_flag(
            config$allow_partial,
            default = FALSE
        ),
        download = epwshiftr_cli_config_choice(
            config$download,
            c("auto", "always", "never"),
            default = "auto"
        ),
        resume = epwshiftr_cli_config_flag(config$resume, default = TRUE),
        overwrite = epwshiftr_cli_config_flag(
            config$overwrite,
            default = FALSE
        ),
        refresh = epwshiftr_cli_config_flag(config$refresh, default = FALSE),
        extraction_method = epwshiftr_cli_config_choice(
            config$extraction_method,
            ESG_GRID_METHOD_CHOICES,
            default = "nearest"
        ),
        output_layout = epwshiftr_cli_config_choice(
            config$output_layout,
            c("nested", "flat"),
            default = "nested"
        )
    )
}
# }}}

# epwshiftr_cli_periods_from_config {{{
epwshiftr_cli_periods_from_config <- function(
    value,
    field = "extract.periods"
) {
    if (is.null(value)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Config field %s is required.",
            field
        ))
    }
    if (
        !is.list(value) || is.null(names(value)) || any(!nzchar(names(value)))
    ) {
        epwshiftr_cli_usage_abort(sprintf(
            "Config field %s must be a named object.",
            field
        ))
    }
    periods <- lapply(value, epwshiftr_cli_years)
    do.call(epw_morph_periods, periods)
}
# }}}

# epwshiftr_cli_config_character {{{
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
# }}}

# epwshiftr_cli_config_string {{{
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
# }}}

# epwshiftr_cli_config_flag {{{
epwshiftr_cli_config_flag <- function(value, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    if (is.logical(value) && length(value) == 1L && !is.na(value)) {
        return(value)
    }
    epwshiftr_cli_bool(as.character(value), "config flag", default = default)
}
# }}}

# epwshiftr_cli_config_choice {{{
epwshiftr_cli_config_choice <- function(value, choices, default = NULL) {
    value <- epwshiftr_cli_config_string(value, default = default)
    if (!is.null(value) && !value %in% choices) {
        epwshiftr_cli_usage_abort(sprintf(
            "Expected one of: %s.",
            paste(choices, collapse = ", ")
        ))
    }
    value
}
# }}}

# epwshiftr_cli_config_named_list {{{
epwshiftr_cli_config_named_list <- function(value) {
    if (is.null(value) || (is.list(value) && !length(value))) {
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
# }}}

# store backed helpers
# epwshiftr_cli_climate_stage_from_plan {{{
epwshiftr_cli_climate_stage_from_plan <- function(
    store,
    plan_id,
    periods,
    epw
) {
    plans <- shift_inspect__extraction_plan(store, plan_id)
    if (!nrow(plans)) {
        epwshiftr_cli_usage_abort(
            "No extraction plan rows were found for --plan."
        )
    }
    query_id <- unique(plans$query_id)
    query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
    if (!length(query_id)) {
        epwshiftr_cli_usage_abort(
            "Could not resolve File query IDs from --plan."
        )
    }

    shift_stage__new(
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
# }}}

# Load persisted summary rows, decode period definitions, and verify source
# extraction-plan lineage for primary, reference, and observed morph inputs.
# cli_shift__summary_lineage {{{
cli_shift__summary_lineage <- function(
    store,
    summary_id,
    label,
    required = FALSE,
    morph_id = NULL
) {
    if (
        !isTRUE(required) &&
            (is.null(summary_id) ||
                !length(summary_id) ||
                is.na(summary_id) ||
                !nzchar(summary_id))
    ) {
        return(NULL)
    }
    summary <- store$query(sprintf(
        "SELECT * FROM epw_climate_summary WHERE summary_id = %s",
        shift_stage__sql_string(summary_id)
    ))
    if (!nrow(summary)) {
        if (isTRUE(required)) {
            cli::cli_abort("Morph ID {.val {morph_id}} has no climate summary.")
        }
        return(NULL)
    }
    plan_id <- morpher__summary_plan_ids(store, summary)
    if (!length(plan_id)) {
        cli::cli_abort(
            "{label} summary ID {.val {summary_id}} has no source extraction-plan lineage."
        )
    }
    period_rows <- unique(summary[, .(period, years_json)])
    period_values <- lapply(seq_len(nrow(period_rows)), function(i) {
        as.integer(jsonlite::fromJSON(
            period_rows$years_json[[i]],
            simplifyVector = TRUE
        ))
    })
    names(period_values) <- period_rows$period
    list(
        summary = summary,
        plan_id = plan_id,
        periods = do.call(epw_morph_periods, period_values)
    )
}
# }}}

# Reconstruct the minimum ShiftMorphed graph required by the public EPW and
# retry APIs. Climate data remain store-backed; only stable IDs, period labels,
# the baseline EPW identity, and the recipe are materialized here.
# epwshiftr_cli_morphed_stage_from_morph_id {{{
epwshiftr_cli_morphed_stage_from_morph_id <- function(store, morph_id) {
    if (length(morph_id) != 1L) {
        epwshiftr_cli_usage_abort("Exactly one morph ID is required.")
    }
    row <- store$query(sprintf(
        paste(
            "SELECT p.*, s.path, s.site_id, s.label",
            "FROM epw_morph_plan p",
            "LEFT JOIN epw_source s ON p.epw_id = s.epw_id",
            "WHERE p.morph_id IN (%s)"
        ),
        shift_stage__query_ids(morph_id)
    ))
    if (!nrow(row) || is.na(row$path[[1L]]) || !nzchar(row$path[[1L]])) {
        cli::cli_abort("Could not reconstruct morph ID {.val {morph_id}}.")
    }
    primary <- cli_shift__summary_lineage(
        store,
        row$summary_id[[1L]],
        label = "Climate",
        required = TRUE,
        morph_id = morph_id
    )
    epw <- store_abs_path(row$path[[1L]], root = store$path)
    site <- shift_site(
        id = epwshiftr_cli_na_null(row$site_id[[1L]]),
        label = epwshiftr_cli_na_null(row$label[[1L]]),
        epw = epw
    )
    climate <- shift_stage__new(
        ShiftClimate,
        "climate",
        store_path = store$path,
        ids = list(
            plan_id = primary$plan_id,
            summary_id = row$summary_id[[1L]]
        ),
        meta = list(
            site = site,
            periods = primary$periods,
            variables = unique(primary$summary$variable_id),
            coverage = store$coverage(plan_id = primary$plan_id)
        )
    )

    ref_lineage <- cli_shift__summary_lineage(
        store,
        row$reference_summary_id[[1L]],
        label = "Reference"
    )
    obs_id <- if ("observed_summary_id" %in% names(row)) {
        row$observed_summary_id[[1L]]
    } else {
        NULL
    }
    obs_lineage <- cli_shift__summary_lineage(
        store,
        obs_id,
        label = "Observed"
    )
    by <- tryCatch(
        as.character(jsonlite::fromJSON(
            row$by_json[[1L]],
            simplifyVector = TRUE
        )),
        error = function(e) {
            c("source_id", "experiment_id", "variant_label", "period")
        }
    )
    recipe <- cli_shift__recipe_from_json(row$recipe_json[[1L]])
    transform <- transform__from_recipe_object(recipe)
    reference <- if (is.null(ref_lineage)) {
        NULL
    } else {
        shift_reference_from_plan(ref_lineage$plan_id, ref_lineage$periods)
    }
    observed_reference <- if (is.null(obs_lineage)) {
        NULL
    } else {
        shift_reference_from_plan(
            obs_lineage$plan_id,
            obs_lineage$periods,
            role = "observed_reference"
        )
    }
    shift_stage__new(
        ShiftMorphed,
        "morphed",
        store_path = store$path,
        ids = list(
            plan_id = primary$plan_id,
            summary_id = row$summary_id[[1L]],
            baseline_id = row$baseline_id[[1L]],
            morph_id = morph_id
        ),
        meta = list(
            climate = climate,
            baseline = site,
            transform = transform,
            recipe = recipe,
            reference = reference,
            reference_plan_id = ref_lineage$plan_id,
            reference_periods = ref_lineage$periods,
            observed_reference = observed_reference,
            observed_plan_id = obs_lineage$plan_id,
            observed_periods = obs_lineage$periods,
            by = by,
            strict = isTRUE(row$strict[[1L]])
        )
    )
}
# }}}

# Restore only the canonical recipe identity written by the current transform
# API. Older profile/method-array payloads are intentionally not inferred.
# cli_shift__recipe_from_json {{{
cli_shift__recipe_from_json <- function(json) {
    parsed <- tryCatch(
        jsonlite::fromJSON(json, simplifyVector = TRUE),
        error = function(error) NULL
    )
    required <- c(
        "name",
        "recipe_spec",
        "recipe_version",
        "policy",
        "options"
    )
    if (
        is.null(parsed) ||
            !is.list(parsed) ||
            length(setdiff(required, names(parsed)))
    ) {
        cli::cli_abort(c(
            "Persisted morph plan uses an unsupported recipe schema.",
            "i" = "Create a new plan with the weather transform API."
        ))
    }

    recipe_name <- as.character(parsed$recipe_spec)
    if (!identical(as.character(parsed$name), recipe_name)) {
        cli::cli_abort(
            "Persisted morph plan does not identify one canonical recipe."
        )
    }
    recipe <- epw_morph_recipe(
        recipe_name,
        version = as.integer(parsed$recipe_version),
        options = shift_stage__coalesce(parsed$options, list())
    )
    if (!identical(as.character(parsed$policy), recipe$policy)) {
        cli::cli_abort(c(
            "Persisted morph plan uses a non-canonical physical policy.",
            "i" = "Create a new plan with the current weather transform registry."
        ))
    }
    recipe
}
# }}}

# epwshiftr_cli_na_null {{{
epwshiftr_cli_na_null <- function(value) {
    if (
        is.null(value) ||
            length(value) == 0L ||
            is.na(value) ||
            !nzchar(as.character(value))
    ) {
        return(NULL)
    }
    as.character(value)
}
# }}}

# compact workflow results
# epwshiftr_cli_shift_stage_result {{{
epwshiftr_cli_shift_stage_result <- function(stage) {
    if (S7::S7_inherits(stage, ShiftBatch)) {
        snapshot <- shift_batch_ui__snapshot(stage, refresh = FALSE)
        return(c(
            list(status = snapshot$batch$status, batch_id = stage@ids$batch_id),
            snapshot,
            list(
                next_steps = epwshiftr_cli_shift_next_steps(
                    stage@ids$batch_id,
                    batch = TRUE,
                    store = stage@store_path
                )
            )
        ))
    }
    if (!S7::S7_inherits(stage, ShiftRun)) {
        epwshiftr_cli_usage_abort(
            "The unified shift runner did not return a ShiftRun."
        )
    }
    ids <- shift_inspect__ids(stage)
    diagnostics <- shift_diagnostics(stage)
    outputs <- cli_shift__output_metadata(shift_outputs(stage))
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
        next_steps = epwshiftr_cli_shift_next_steps(
            ids$run_id,
            store = stage@store_path
        )
    )
}
# }}}

# Keep follow-up commands tied to the exact receipt and store returned to the user.
# epwshiftr_cli_shift_next_steps {{{
epwshiftr_cli_shift_next_steps <- function(
    run_id,
    batch = FALSE,
    store = NULL
) {
    steps <- c(
        "watch",
        "status",
        "show",
        "outputs",
        "summary",
        "diagnostics",
        "logs",
        "cancel",
        "resume"
    )
    data.frame(
        step = steps,
        command = sprintf(
            "epwshiftr%s shift %s --%s %s",
            if (is.null(store)) "" else paste0(" --store ", shQuote(store)),
            steps,
            if (isTRUE(batch)) "batch" else "run",
            run_id
        ),
        stringsAsFactors = FALSE
    )
}
# }}}

# Parse read-only history filters without opening or initializing a database.
# cli_shift__history {{{
cli_shift__history <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--type", "--status", "--limit"),
        no_positionals = TRUE
    )
    type <- shift_stage__coalesce(parsed$options[["--type"]], "all")
    if (!type %in% c("all", "run", "batch")) {
        epwshiftr_cli_usage_abort(
            "--type must be all, run, or batch."
        )
    }
    rows <- shift_history(
        store,
        type = type,
        status = epwshiftr_cli_csv(parsed$options[["--status"]])
    )
    limit <- epwshiftr_cli_count_or_default(
        parsed$options[["--limit"]],
        "--limit",
        20L,
        positive = FALSE
    )
    out <- utils::head(rows, limit)
    attr(out, "shift_history_total") <- nrow(rows)
    out
}
# }}}

# Share the same comparison table between R and CLI, with optional local reads.
# cli_shift__summary {{{
cli_shift__summary <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    req <- cli_shift__parse_target(
        store,
        args,
        flags = c("--weather", "--no-progress", "--reduced-motion")
    )
    shift_summary(
        req$target,
        refresh = FALSE,
        weather = isTRUE(req$parsed$flags[["--weather"]]),
        ui = epwshiftr_cli_task_ui(
            req$parsed,
            json = json,
            jsonl = jsonl,
            quiet = quiet
        )
    )
}
# }}}

# epwshiftr_cli_shift_config {{{
epwshiftr_cli_shift_config <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort(
            "Missing shift config command: example or validate."
        )
    }
    action <- args[[1L]]
    rest <- args[-1L]
    switch(
        action,
        example = epwshiftr_cli_shift_config_example(rest),
        validate = epwshiftr_cli_shift_config_validate(
            store,
            rest,
            json = json,
            jsonl = jsonl,
            quiet = quiet
        ),
        epwshiftr_cli_usage_abort(sprintf(
            "Unknown shift config command: %s",
            action
        ))
    )
}
# }}}

# epwshiftr_cli_read_shift_config {{{
epwshiftr_cli_read_shift_config <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)
    if (!file.exists(path)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Config file does not exist: %s",
            path
        ))
    }
    config <- tryCatch(
        jsonlite::read_json(
            path,
            simplifyVector = TRUE,
            simplifyDataFrame = FALSE
        ),
        error = function(e) {
            epwshiftr_cli_usage_abort(sprintf(
                "Failed to read JSON config: %s",
                conditionMessage(e)
            ))
        }
    )
    tryCatch(
        {
            schema_validate(
                schema__compiled("SHIFT_WORKFLOW_CONFIG"),
                config,
                name = "config"
            )
            epwshiftr_cli_validate_shift_config(config)
        },
        error = function(e) {
            epwshiftr_cli_usage_abort(sprintf(
                "Invalid shift workflow config: %s",
                conditionMessage(e)
            ))
        }
    )
    invisible(config)
}
# }}}

# epwshiftr_cli_shift_config_example {{{
epwshiftr_cli_shift_config_example <- function(args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = "--overwrite",
        options = c(
            "--output",
            "--methods",
            "--scale",
            "--method",
            "--reconstruction",
            "--model"
        ),
        multi_options = "--option",
        no_positionals = TRUE
    )
    config <- epwshiftr_cli_shift_example_config()
    methods <- epwshiftr_cli_csv(parsed$options[["--methods"]])
    if (!is.null(methods)) {
        if (
            any(
                vapply(
                    parsed$options[c(
                        "--scale",
                        "--method",
                        "--reconstruction",
                        "--option"
                    )],
                    length,
                    integer(1L)
                ) >
                    0L
            )
        ) {
            epwshiftr_cli_usage_abort(
                "--methods cannot be combined with single-transform options."
            )
        }
        transforms <- shift_batch__transforms(methods = methods)
        config$transform <- NULL
        config$methods <- methods
        config$climate$model <- 3L
    } else {
        transform <- cli_morph__transform(parsed)
        transforms <- list(transform)
        config$transform <- list(
            scale = transform@scale,
            method = transform@method
        )
        if (!is.null(parsed$options[["--reconstruction"]])) {
            config$transform$reconstruction <- parsed$options[[
                "--reconstruction"
            ]]
        }
        options <- cli_morph__parse_options(parsed$options[["--option"]])
        if (length(options)) config$transform$options <- options
    }
    if (
        !any(vapply(
            transforms,
            transform__accepts_input,
            logical(1L),
            role = "model_historical"
        ))
    ) {
        config["reference"] <- list(NULL)
    }
    if (
        any(vapply(
            transforms,
            transform__requires_input,
            logical(1L),
            role = "observed_reference"
        ))
    ) {
        config$calibration <- list(
            dataset = "era5",
            years = "1995:2014",
            product = "single_levels",
            access = "auto"
        )
    }
    model <- parsed$options[["--model"]]
    if (!is.null(model)) {
        config$climate["model"] <- list(
            if (identical(model, "all")) {
                NULL
            } else if (grepl("^[0-9]+$", model)) {
                epwshiftr_cli_count(model, "--model")
            } else {
                epwshiftr_cli_csv(model)
            }
        )
    }
    output <- parsed$options[["--output"]]
    if (!is.null(output)) {
        if (file.exists(output) && !isTRUE(parsed$flags[["--overwrite"]])) {
            epwshiftr_cli_usage_abort(sprintf(
                "Output file already exists: %s",
                output
            ))
        }
        dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(
            config,
            output,
            auto_unbox = TRUE,
            pretty = TRUE,
            null = "null"
        )
    }
    list(
        action = "example",
        status = if (is.null(output)) "printed" else "written",
        output = if (is.null(output)) {
            NA_character_
        } else {
            normalizePath(output, winslash = "/", mustWork = FALSE)
        },
        config = config
    )
}
# }}}

# cli_shift__config_components {{{
cli_shift__config_components <- function(config) {
    list(
        transforms = shift_batch__transforms(
            methods = config$methods,
            transform = cli_shift__config_transform(config$transform)
        ),
        climate = epwshiftr_cli_config_climate(config$climate),
        reference = cli_shift__config_reference(config$reference, "reference"),
        calibration = cli_shift__config_reference(
            shift_stage__coalesce(
                config$calibration,
                config$observed_reference
            ),
            "observed_reference"
        ),
        periods = shift_spec__periods_from_input(config$periods)
    )
}
# }}}

# cli_shift__format_models {{{
cli_shift__format_models <- function(climate) {
    if (!is.null(climate@model)) {
        paste(climate@model, collapse = ", ")
    } else if (is.null(climate@n_models)) {
        "all compatible models"
    } else {
        sprintf("%d compatible models", climate@n_models)
    }
}
# }}}

# epwshiftr_cli_shift_config_validate {{{
epwshiftr_cli_shift_config_validate <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = "--config",
        flags = c("--network", "--no-progress", "--reduced-motion"),
        no_positionals = TRUE
    )
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    parts <- cli_shift__config_components(config)
    transforms <- parts$transforms
    climate <- parts$climate
    observed <- parts$calibration
    references <- lapply(transforms, function(transform) {
        shift_spec__validate_transform_periods(transform, parts$periods)
        if (!is.null(climate@frequency)) {
            shift_spec__validate_transform_frequency(
                transform,
                climate@frequency
            )
        }
        shift_batch__references(transform, parts$reference, observed)
    })
    locations <- shift_batch__sites(lapply(config$sites, function(site) {
        do.call(shift_site, site)
    }))
    shift_path__validate_delivery_store_paths(config$dir, store)
    network <- isTRUE(parsed$flags[["--network"]])
    ui <- epwshiftr_cli_task_ui(
        parsed,
        json = json,
        jsonl = jsonl,
        quiet = quiet
    )
    checks <- if (S7::S7_inherits(observed, ShiftReanalysisSpec)) {
        shift_reporter__ui_check(
            ui,
            "Calibration readiness",
            function(reporter) {
                reporter$stage_started(
                    "check",
                    if (network) {
                        "Checking CDS credentials and network access."
                    } else {
                        "Checking local calibration settings."
                    }
                )
                shift_check(observed, network = network)
            }
        )
    } else {
        shift_stage__diagnostics_empty()
    }
    discovery <- if (network) {
        shift_batch_ui__discover_models(
            climate,
            transforms,
            periods = parts$periods,
            references = references,
            store = store,
            ui = ui
        )$identities
    } else {
        data.table::data.table()
    }
    list(
        action = "validate",
        intent = cli_shift__config_intent(config, components = parts),
        status = "valid",
        validation = if (network) "network" else "local",
        readiness = if (any(checks$severity == "error")) {
            "blocked"
        } else if (network) {
            "network_checks_passed"
        } else {
            "local_checks_passed"
        },
        config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
        cases = data.table::data.table(),
        explain = data.table::data.table(
            step = c("methods", "models", "discovery"),
            detail = c(
                paste(
                    vapply(
                        transforms,
                        function(transform) {
                            paste(
                                transform@scale,
                                transform@method,
                                transform@reconstruction
                            )
                        },
                        character(1L)
                    ),
                    collapse = "; "
                ),
                cli_shift__format_models(climate),
                if (network) {
                    "Network coverage checked"
                } else {
                    "Not checked locally; use --network or shift run --dry-run"
                }
            )
        ),
        sites = locations[, c("site_id", "epw"), with = FALSE],
        selected_models = discovery,
        diagnostics = checks
    )
}
# }}}

# Summarize the user's scientific choices before execution without opening a
# store or resolving remote models. Keep calibration separate from historical
# model reference data so the two input roles remain unambiguous.
# cli_shift__config_intent {{{
cli_shift__config_intent <- function(config, components = NULL) {
    parts <- shift_stage__coalesce(
        components,
        cli_shift__config_components(config)
    )
    climate <- parts$climate
    list(
        Baseline = paste(
            vapply(
                config$sites,
                function(site) paste0(site$id, ": ", site$epw),
                character(1L)
            ),
            collapse = "; "
        ),
        Methods = paste(
            vapply(
                parts$transforms,
                function(transform) {
                    sprintf(
                        "%s / %s / %s [%s]",
                        transform@method,
                        transform@scale,
                        transform@reconstruction,
                        transform@status
                    )
                },
                character(1L)
            ),
            collapse = "; "
        ),
        Models = cli_shift__format_models(climate),
        `Common models` = climate@common,
        Scenarios = paste(climate@scenarios, collapse = ", "),
        Periods = shift_ui_view__ui_periods(parts$periods),
        Reference = shift_print__format_reference(parts$reference),
        Calibration = shift_print__format_reference(parts$calibration),
        Output = config$dir
    )
}
# }}}

# epwshiftr_cli_shift_example_config {{{
epwshiftr_cli_shift_example_config <- function() {
    list(
        version = 3L,
        sites = list(list(
            id = "Singapore",
            epw = system.file(
                "extdata/examples/SGP_Singapore.486980_IWEC.epw",
                package = "epwshiftr",
                mustWork = TRUE
            )
        )),
        climate = list(
            provider = "cmip6",
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            member = NULL,
            grid = NULL,
            frequency = NULL,
            table = NULL
        ),
        periods = list(`2060s` = "2055:2065"),
        transform = list(
            scale = "monthly",
            method = "original_morphing"
        ),
        reference = list(
            mode = "historical",
            periods = list(reference = "1995:2014")
        ),
        dir = "future-epw",
        control = list(
            strict = TRUE,
            allow_partial = FALSE,
            download = "auto",
            resume = TRUE,
            overwrite = FALSE
        )
    )
}
# }}}

# Preserve the requested detail level as presentation metadata for both target
# types; attributes do not change the machine-readable snapshot fields.
# epwshiftr_cli_shift_show {{{
epwshiftr_cli_shift_show <- function(store, args) {
    req <- cli_shift__parse_target(
        store,
        args,
        flags = c("--verbose", "--debug")
    )
    run <- req$target
    snapshot <- if (S7::S7_inherits(run, ShiftBatch)) {
        out <- shift_batch_ui__snapshot(
            run,
            event_count = Inf,
            refresh = FALSE
        )
        out$explain <- shift_explain(run)
        out
    } else {
        list(
            run = run@meta$run,
            cases = shift_cases(run),
            events = run@meta$events,
            outputs = shift_outputs(run),
            diagnostics = shift_diagnostics(run),
            explain = shift_explain(run)
        )
    }
    attr(snapshot, "shift_ui_detail") <- epwshiftr_cli_shift_detail(req$parsed)
    snapshot
}
# }}}

# cli_shift__read_watch_snapshot {{{
cli_shift__read_watch_snapshot <- function(
    store,
    run_id,
    batch_id = NULL,
    event_count = 10L
) {
    if (is.null(batch_id)) {
        return(epwshiftr_cli_shift_watch_snapshot(
            store,
            run_id = run_id,
            event_count = event_count
        ))
    }
    shift_batch_ui__snapshot(
        shift_batch_get(batch_id, store),
        event_count = event_count,
        refresh = FALSE
    )
}
# }}}

# Route watch presentation so JSON emits one final snapshot, JSONL emits the
# event stream, and neither mode is contaminated by human-readable rendering.
# epwshiftr_cli_shift_watch {{{
epwshiftr_cli_shift_watch <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--follow", CLI_UI_FLAGS),
        options = c("--run", "--batch", "--interval", "--count", "--events"),
        no_positionals = TRUE
    )
    batch_id <- parsed$options[["--batch"]]
    if (is.null(batch_id) == is.null(parsed$options[["--run"]])) {
        epwshiftr_cli_usage_abort("Supply exactly one of --run or --batch.")
    }
    run_id <- epwshiftr_cli_required_single_id(
        parsed,
        if (is.null(batch_id)) "--run" else "--batch"
    )
    detail <- epwshiftr_cli_shift_detail(parsed)
    progress <- if (isTRUE(parsed$flags[["--no-progress"]])) {
        "none"
    } else if (isTRUE(cli::is_dynamic_tty())) {
        "dynamic"
    } else {
        "log"
    }
    event_count <- epwshiftr_cli_count_or_default(
        parsed$options[["--events"]],
        "--events",
        10L,
        positive = FALSE
    )
    if (isTRUE(parsed$flags[["--follow"]])) {
        snapshot <- epwshiftr_cli_shift_watch_follow(
            store,
            run_id = run_id,
            event_count = event_count,
            interval = epwshiftr_cli_download_interval(
                parsed$options[["--interval"]],
                1
            ),
            count = epwshiftr_cli_count_or_default(
                parsed$options[["--count"]],
                "--count",
                Inf,
                positive = FALSE
            ),
            jsonl = jsonl,
            quiet = isTRUE(quiet) || isTRUE(json),
            progress = progress,
            detail = detail,
            motion = epwshiftr_cli_shift_motion(parsed),
            batch_id = batch_id
        )
        if (isTRUE(json)) {
            # JSON follow suppresses intermediate snapshots and lets the
            # top-level emitter serialize exactly one terminal document.
            class(snapshot) <- setdiff(class(snapshot), "epwshiftr_cli_emitted")
        }
        return(snapshot)
    }
    snapshot <- cli_shift__read_watch_snapshot(
        store,
        run_id = run_id,
        batch_id = batch_id,
        event_count = event_count
    )
    attr(snapshot, "shift_ui_detail") <- detail
    snapshot
}
# }}}

# Read one durable run snapshot without reconstructing state from unrelated
# query, extraction, or morph tables.
# epwshiftr_cli_shift_watch_snapshot {{{
epwshiftr_cli_shift_watch_snapshot <- function(
    store,
    run_id,
    event_count = 10L
) {
    run <- shift_run_get(run_id, store)
    all_events <- run@meta$events
    events <- all_events
    if (nrow(events) > event_count) {
        events <- utils::tail(events, event_count)
    }
    snapshot <- list(
        run = run@meta$run,
        cases = shift_cases(run),
        outputs = shift_outputs(run),
        diagnostics = shift_diagnostics(run),
        events = events
    )
    # Preserve the full event history only as renderer metadata; JSON/JSONL
    # contracts continue to expose the requested recent-event count.
    attr(snapshot, "shift_ui_events") <- all_events
    attr(snapshot, "shift_ui_state") <- run@meta$ui_state
    snapshot
}
# }}}

# Emit one typed JSONL record for workflow automation. Snapshot, event, gap, and
# terminal records remain self-describing and never contain human progress text.
# epwshiftr_cli_shift_jsonl_record {{{
epwshiftr_cli_shift_jsonl_record <- function(type, ...) {
    epwshiftr_cli_emit_jsonl(c(
        list(
            type = type,
            emitted_at = store__now()
        ),
        list(...)
    ))
}
# }}}

# Select the correct event cursor for single-run and independently polled
# batch histories before any public recent-event limit is applied.
# cli_shift__watch_event_delta {{{
cli_shift__watch_event_delta <- function(
    snapshot,
    cursor,
    initial_limit,
    initial
) {
    events <- shift_stage__coalesce(
        attr(snapshot, "shift_ui_events"),
        snapshot$events
    )
    if (!is.null(snapshot$batch)) {
        shift_batch_ui__event_delta(events, cursor, initial_limit, initial)
    } else {
        shift_ui_state__ui_event_delta(events, cursor, initial_limit, initial)
    }
}
# }}}

# Follow persisted snapshots with shared R/CLI views and lossless event deltas.
# epwshiftr_cli_shift_watch_follow {{{
epwshiftr_cli_shift_watch_follow <- function(
    store,
    run_id,
    event_count = 10L,
    interval = 1,
    count = Inf,
    jsonl = FALSE,
    quiet = FALSE,
    progress = c("dynamic", "log", "none"),
    detail = "normal",
    motion = c("auto", "full", "reduced", "none"),
    batch_id = NULL
) {
    progress <- match.arg(progress)
    motion <- match.arg(motion)
    effective_progress <- if (isTRUE(quiet) || isTRUE(jsonl)) {
        "none"
    } else if (identical(progress, "dynamic")) {
        "dynamic"
    } else {
        "log"
    }
    ui <- shift_ui(
        progress = effective_progress,
        detail = detail,
        motion = motion
    )
    initial_cursor <- if (is.null(batch_id)) NA_character_ else list()
    jsonl_cursor <- initial_cursor
    on_poll <- if (!isTRUE(quiet) && isTRUE(jsonl)) {
        function(snapshot, first, done) {
            delta <- cli_shift__watch_event_delta(
                snapshot,
                cursor = jsonl_cursor,
                initial_limit = event_count,
                initial = isTRUE(first)
            )
            if (isTRUE(first)) {
                epwshiftr_cli_shift_jsonl_record(
                    "snapshot",
                    snapshot = snapshot
                )
            } else {
                if (isTRUE(delta$gap)) {
                    epwshiftr_cli_shift_jsonl_record(
                        "gap",
                        message = "Older workflow events are no longer available in the live buffer."
                    )
                }
                for (j in seq_len(nrow(delta$rows))) {
                    epwshiftr_cli_shift_jsonl_record(
                        "event",
                        event = epwshiftr_cli_row_object(delta$rows, j)
                    )
                }
            }
            jsonl_cursor <<- delta$cursor
            if (isTRUE(done)) {
                epwshiftr_cli_shift_jsonl_record(
                    "terminal",
                    snapshot = snapshot
                )
            }
        }
    }
    tick <- 0
    snapshot <- shift_tui__watch_loop(
        poll = function(polls) {
            cli_shift__read_watch_snapshot(
                store,
                run_id = run_id,
                batch_id = batch_id,
                event_count = event_count
            )
        },
        is_done = function(snapshot, polls) {
            !isTRUE(epwshiftr_cli_shift_watch_active(snapshot))
        },
        render_view = function(snapshot, motion, frame) {
            if (!is.null(snapshot$batch)) {
                return(shift_batch_ui__view(
                    snapshot,
                    detail = detail,
                    motion = motion,
                    frame = frame,
                    height = shift_ui__ui_height()
                ))
            }
            view_events <- shift_stage__coalesce(
                attr(snapshot, "shift_ui_events"),
                snapshot$events
            )
            shift_ui_view__ui_table_view(
                snapshot$run,
                snapshot$cases,
                view_events,
                detail = detail,
                motion = motion,
                frame = frame,
                outputs = snapshot$outputs,
                diagnostics = snapshot$diagnostics,
                ui_state = attr(snapshot, "shift_ui_state")
            )
        },
        print_view = function(snapshot, final = FALSE) {
            epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
        },
        ui = ui,
        interval = interval,
        event_delta = function(snapshot, cursor, initial) {
            cli_shift__watch_event_delta(
                snapshot,
                cursor = cursor,
                initial_limit = event_count,
                initial = initial
            )
        },
        initial_cursor = initial_cursor,
        on_poll = on_poll,
        max_polls = count,
        animate = FALSE,
        now_fn = function() {
            tick <<- tick + max(interval, 1)
            as.POSIXct(tick, origin = "1970-01-01", tz = "UTC")
        }
    )
    if (is.null(snapshot)) {
        return(invisible(NULL))
    }
    attr(snapshot, "shift_ui_detail") <- detail
    structure(snapshot, class = c("epwshiftr_cli_emitted", class(snapshot)))
}
# }}}

# Keep following the coordinator while source work precedes child registration.
# epwshiftr_cli_shift_watch_active {{{
epwshiftr_cli_shift_watch_active <- function(snapshot) {
    active <- c("queued", "running", "stopping")
    if (!is.null(snapshot$batch)) {
        return(
            snapshot$batch$status %in%
                active ||
                any(snapshot$children$status %in% active)
        )
    }
    nrow(snapshot$run) && snapshot$run$status[[1L]] %in% active
}
# }}}

# epwshiftr_cli_doctor {{{
epwshiftr_cli_doctor <- function(
    store_path = NULL,
    args = character(),
    ui = shift_ui("none")
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--network", "--no-progress"),
        options = c("--index-node", "--timeout", "--config"),
        no_positionals = TRUE
    )
    if (isTRUE(parsed$flags[["--no-progress"]])) {
        ui <- shift_ui("none")
    }

    timeout <- epwshiftr_cli_count_or_default(
        parsed$options[["--timeout"]],
        "--timeout",
        10L
    )
    index_node <- shift_stage__coalesce(
        parsed$options[["--index-node"]],
        "https://esgf-node.ornl.gov"
    )

    checks <- list()
    add <- function(check, status, message, detail = NA_character_) {
        checks[[length(checks) + 1L]] <<- data.frame(
            check = check,
            status = status,
            message = message,
            detail = epwshiftr_cli_doctor_detail(detail),
            stringsAsFactors = FALSE
        )
    }
    add_dir_writable <- function(
        name,
        path,
        label,
        missing_status = "skipped",
        missing_message = sprintf("%s is missing.", label)
    ) {
        if (dir.exists(path)) {
            writable <- unname(file.access(path, 2L)) == 0L
            add(
                name,
                if (writable) "ok" else "error",
                sprintf(
                    if (writable) "%s is writable." else "%s is not writable.",
                    label
                ),
                path
            )
        } else {
            add(name, missing_status, missing_message, path)
        }
    }
    add_manifest_checks <- function(prefix, manifest, table, label) {
        meta <- epwshiftr_cli_doctor_manifest_meta(manifest, table)
        add(paste0(prefix, "_manifest"), meta$status, meta$message, manifest)
        schema_version <- epwshiftr_cli_doctor_meta_value(
            meta$data,
            "schema_version"
        )
        ok <- identical(meta$status, "ok")
        add(
            paste0(prefix, "_schema"),
            if (ok) "ok" else "skipped",
            if (!is.na(schema_version)) {
                sprintf("%s schema version is %s.", label, schema_version)
            } else if (ok) {
                sprintf("%s schema version is missing.", label)
            } else {
                sprintf("%s manifest is not readable.", label)
            },
            if (!is.na(schema_version)) schema_version else NA_character_
        )
    }

    add("r_version", "ok", paste("R", getRversion()), R.home())
    rscript <- file.path(R.home("bin"), "Rscript")
    has_rscript <- nzchar(rscript) && file.exists(rscript)
    add(
        "rscript",
        if (has_rscript) "ok" else "warning",
        if (has_rscript) {
            "Rscript is available."
        } else {
            "Rscript was not found under R.home()."
        },
        rscript
    )
    version <- tryCatch(
        as.character(utils::packageVersion("epwshiftr")),
        error = function(e) NA_character_
    )
    add(
        "package",
        if (is.na(version)) "warning" else "ok",
        if (is.na(version)) {
            "Could not resolve epwshiftr package version."
        } else {
            paste("epwshiftr", version)
        },
        NA_character_
    )
    launcher <- Sys.which("epwshiftr")
    add(
        "launcher",
        if (nzchar(launcher)) "ok" else "warning",
        if (nzchar(launcher)) {
            "epwshiftr launcher is on PATH."
        } else {
            "epwshiftr launcher was not found on PATH."
        },
        if (nzchar(launcher)) unname(launcher) else NA_character_
    )

    resolved_store <- epwshiftr_cli_doctor_store_path(store_path)
    store_exists <- dir.exists(resolved_store)
    add(
        "store_path",
        if (store_exists) "ok" else "warning",
        if (store_exists) {
            "Store directory exists."
        } else {
            "Store directory does not exist."
        },
        resolved_store
    )
    add_dir_writable("store_writable", resolved_store, "Store directory")
    add_manifest_checks(
        "store",
        file.path(resolved_store, "manifest.duckdb"),
        "store_meta",
        "Store"
    )

    downloader_manifest <- file.path(
        resolved_store,
        "downloads",
        "_downloader",
        "manifest.duckdb"
    )
    config_check <- epwshiftr_cli_doctor_downloader_config(downloader_manifest)
    add(
        "downloader_config",
        config_check$status,
        config_check$message,
        downloader_manifest
    )
    add_manifest_checks(
        "downloader",
        downloader_manifest,
        "download_meta",
        "Downloader"
    )

    add_dir_writable(
        "tmp_downloads",
        file.path(resolved_store, "tmp", "downloads"),
        "Temporary download directory",
        missing_status = "warning",
        missing_message = "Temporary download directory does not exist."
    )

    if (isTRUE(parsed$flags[["--network"]])) {
        network <- shift_reporter__ui_check(
            ui,
            "Network readiness",
            function(reporter) {
                reporter$stage_started("check", paste("Checking", index_node))
                epwshiftr_cli_doctor_network(index_node, timeout)
            }
        )
        add("index_node", network$status, network$message, network$detail)
    } else {
        add(
            "index_node",
            "skipped",
            "Network check skipped. Use --network to enable it.",
            index_node
        )
    }

    # Reuse provider readiness checks without accepting credentials in JSON or
    # printing secrets. Remote authentication is checked only with --network.
    config_path <- parsed$options[["--config"]]
    if (!is.null(config_path)) {
        config <- epwshiftr_cli_read_shift_config(config_path)
        observed <- cli_shift__config_reference(
            shift_stage__coalesce(
                config$calibration,
                config$observed_reference
            ),
            "observed_reference"
        )
        if (S7::S7_inherits(observed, ShiftReanalysisSpec)) {
            diagnostics <- shift_reporter__ui_check(
                ui,
                "Calibration readiness",
                function(reporter) {
                    reporter$stage_started(
                        "check",
                        "Checking CDS credentials and readiness."
                    )
                    shift_check(
                        observed,
                        network = isTRUE(parsed$flags[["--network"]])
                    )
                }
            )
            if (!nrow(diagnostics)) {
                add(
                    "reanalysis",
                    "ok",
                    sprintf(
                        "%s credential checks passed.",
                        toupper(observed@dataset)
                    )
                )
            } else {
                for (index in seq_len(nrow(diagnostics))) {
                    row <- diagnostics[index]
                    add(
                        row$code,
                        if (row$severity == "error") "error" else "warning",
                        row$message,
                        row$action
                    )
                }
            }
        }
    }
    checks <- do.call(rbind, checks)
    rownames(checks) <- NULL
    status <- if (any(checks$status == "error")) {
        "error"
    } else if (any(checks$status == "warning")) {
        "warning"
    } else {
        "ok"
    }
    summary <- data.frame(
        status = status,
        ok = sum(checks$status == "ok"),
        warning = sum(checks$status == "warning"),
        error = sum(checks$status == "error"),
        skipped = sum(checks$status == "skipped"),
        store = resolved_store,
        stringsAsFactors = FALSE
    )
    list(summary = summary, checks = checks)
}
# }}}

# epwshiftr_cli_doctor_store_path {{{
epwshiftr_cli_doctor_store_path <- function(path = NULL) {
    if (is.null(path)) {
        return(store_dir(init = FALSE))
    }
    normalizePath(path.expand(path), mustWork = FALSE, winslash = "/")
}
# }}}

# epwshiftr_cli_doctor_detail {{{
epwshiftr_cli_doctor_detail <- function(detail) {
    if (is.null(detail) || !length(detail)) {
        return(NA_character_)
    }
    detail <- as.character(detail[[1L]])
    if (!nzchar(detail)) NA_character_ else detail
}
# }}}

# epwshiftr_cli_doctor_manifest_meta {{{
epwshiftr_cli_doctor_manifest_meta <- function(path, table) {
    if (!file.exists(path)) {
        return(list(
            status = "warning",
            message = "Manifest file does not exist.",
            data = data.frame()
        ))
    }
    conn <- NULL
    tryCatch(
        {
            conn <- ddb_connect(path, read_only = TRUE)
            tables <- ddb_list_tables(conn)
            if (!table %in% tables) {
                return(list(
                    status = "warning",
                    message = sprintf(
                        "Manifest table %s does not exist.",
                        table
                    ),
                    data = data.frame()
                ))
            }
            data <- ddb_query(
                conn,
                sprintf("SELECT key, value FROM %s", ddb_ident(conn, table))
            )
            list(
                status = "ok",
                message = "Manifest is readable.",
                data = as.data.frame(data, stringsAsFactors = FALSE)
            )
        },
        error = function(e) {
            list(
                status = "error",
                message = conditionMessage(e),
                data = data.frame()
            )
        },
        finally = {
            if (!is.null(conn)) {
                try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE)
            }
        }
    )
}
# }}}

# epwshiftr_cli_doctor_meta_value {{{
epwshiftr_cli_doctor_meta_value <- function(meta, key) {
    if (
        !is.data.frame(meta) ||
            !nrow(meta) ||
            !"key" %in% names(meta) ||
            !"value" %in% names(meta)
    ) {
        return(NA_character_)
    }
    row <- meta[meta$key == key, , drop = FALSE]
    if (!nrow(row)) {
        return(NA_character_)
    }
    value <- as.character(row$value[[1L]])
    if (is.na(value) || !nzchar(value)) NA_character_ else value
}
# }}}

# epwshiftr_cli_doctor_downloader_config {{{
epwshiftr_cli_doctor_downloader_config <- function(path) {
    if (!file.exists(path)) {
        return(list(
            status = "warning",
            message = "Downloader manifest file does not exist."
        ))
    }
    conn <- NULL
    tryCatch(
        {
            conn <- ddb_connect(path, read_only = TRUE)
            tables <- ddb_list_tables(conn)
            if (!"download_config" %in% tables) {
                return(list(
                    status = "warning",
                    message = "Downloader config table does not exist."
                ))
            }
            rows <- ddb_query(
                conn,
                "SELECT * FROM download_config WHERE config_id = 'default'"
            )
            rows <- as.data.frame(rows, stringsAsFactors = FALSE)
            if (!nrow(rows)) {
                return(list(
                    status = "warning",
                    message = "Downloader config row is missing."
                ))
            }
            downloader__config_unflatten(rows, manifest = path)
            list(status = "ok", message = "Downloader config is readable.")
        },
        error = function(e) {
            list(status = "error", message = conditionMessage(e))
        },
        finally = {
            if (!is.null(conn)) {
                try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE)
            }
        }
    )
}
# }}}

# epwshiftr_cli_doctor_network {{{
epwshiftr_cli_doctor_network <- function(index_node, timeout = 10L) {
    tryCatch(
        {
            query <- esg_query(index_node)$limit(0L)
            handle <- curl::new_handle(
                timeout = timeout,
                connecttimeout = min(timeout, 10L)
            )
            response <- curl::curl_fetch_memory(query$url(), handle = handle)
            code <- response$status_code
            list(
                status = if (code >= 200L && code < 400L) "ok" else "error",
                message = sprintf("Index node responded with HTTP %s.", code),
                detail = query$url()
            )
        },
        error = function(e) {
            list(
                status = "error",
                message = conditionMessage(e),
                detail = index_node
            )
        }
    )
}
# }}}

# vim: fdm=marker :
