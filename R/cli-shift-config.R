epwshiftr_cli_shift_config <- function(store, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort("Missing shift config command: example or validate.")
    }
    action <- args[[1L]]
    rest <- args[-1L]
    switch(
        action,
        example = epwshiftr_cli_shift_config_example(rest),
        validate = epwshiftr_cli_shift_config_validate(store, rest,
            json = json, jsonl = jsonl, quiet = quiet),
        epwshiftr_cli_usage_abort(sprintf("Unknown shift config command: %s", action))
    )
}


epwshiftr_cli_read_shift_config <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)
    if (!file.exists(path)) {
        epwshiftr_cli_usage_abort(sprintf("Config file does not exist: %s", path))
    }
    config <- tryCatch(
        jsonlite::read_json(path, simplifyVector = TRUE, simplifyDataFrame = FALSE),
        error = function(e) epwshiftr_cli_usage_abort(sprintf("Failed to read JSON config: %s", conditionMessage(e)))
    )
    tryCatch(
        {
            schema_validate(SCHEMA_SHIFT_WORKFLOW_CONFIG, config, name = "config")
            epwshiftr_cli_validate_shift_config(config)
        },
        error = function(e) epwshiftr_cli_usage_abort(sprintf("Invalid shift workflow config: %s", conditionMessage(e)))
    )
    invisible(config)
}


epwshiftr_cli_shift_config_example <- function(args) {
    parsed <- epwshiftr_cli_parse_command(args, flags = "--overwrite",
        options = c("--output", "--methods", "--scale", "--method",
            "--reconstruction", "--model"), multi_options = "--option")
    epwshiftr_cli_assert_no_positionals(parsed)
    config <- epwshiftr_cli_shift_example_config()
    methods <- epwshiftr_cli_csv(parsed$options[["--methods"]])
    if (!is.null(methods)) {
        if (any(vapply(parsed$options[c("--scale", "--method",
            "--reconstruction", "--option")], length, integer(1L)) > 0L)) {
            epwshiftr_cli_usage_abort("--methods cannot be combined with single-transform options.")
        }
        transforms <- shift_batch__transforms(methods = methods)
        config$transform <- NULL
        config$methods <- methods
        config$climate$model <- 3L
    } else {
        transform <- cli_morph__transform(parsed)
        transforms <- list(transform)
        config$transform <- list(scale = transform@scale,
            method = transform@method)
        if (!is.null(parsed$options[["--reconstruction"]])) {
            config$transform$reconstruction <- parsed$options[["--reconstruction"]]
        }
        options <- cli_morph__parse_options(parsed$options[["--option"]])
        if (length(options)) config$transform$options <- options
    }
    if (!any(vapply(transforms, transform__accepts_input,
        logical(1L), role = "model_historical"))) {
        config["reference"] <- list(NULL)
    }
    if (any(vapply(transforms, transform__requires_input,
        logical(1L), role = "observed_reference"))) {
        config$calibration <- list(dataset = "era5", years = "1995:2014",
            product = "single_levels", access = "auto")
    }
    model <- parsed$options[["--model"]]
    if (!is.null(model)) {
        config$climate["model"] <- list(if (identical(model, "all")) {
            NULL
        } else if (grepl("^[0-9]+$", model)) {
            epwshiftr_cli_count(model, "--model")
        } else {
            epwshiftr_cli_csv(model)
        })
    }
    output <- parsed$options[["--output"]]
    if (!is.null(output)) {
        if (file.exists(output) && !isTRUE(parsed$flags[["--overwrite"]])) {
            epwshiftr_cli_usage_abort(sprintf("Output file already exists: %s", output))
        }
        dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(config, output, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
    list(
        action = "example",
        status = if (is.null(output)) "printed" else "written",
        output = if (is.null(output)) NA_character_ else normalizePath(output, winslash = "/", mustWork = FALSE),
        config = config
    )
}


epwshiftr_cli_shift_config_validate <- function(store, args,
    json = FALSE, jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--config",
        flags = c("--network", "--no-progress", "--reduced-motion"))
    epwshiftr_cli_assert_no_positionals(parsed)
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    transforms <- shift_batch__transforms(methods = config$methods,
        transform = cli_shift__config_transform(config$transform))
    climate <- epwshiftr_cli_config_climate(config$climate)
    reference <- cli_shift__config_reference(config$reference, "reference")
    observed <- cli_shift__config_reference(shift_coalesce(
        config$calibration, config$observed_reference), "observed_reference")
    batch <- !is.null(config$methods) || length(transforms) > 1L ||
        is.null(climate@model)
    references <- lapply(transforms, function(transform) {
        if (batch) return(shift_batch__references(transform, reference, observed))
        transform__validate_execution_inputs(transform, reference, observed)
        list(reference = reference, observed_reference = observed)
    })
    shift__epw_identity(config$epw)
    network <- isTRUE(parsed$flags[["--network"]])
    ui <- epwshiftr_cli_task_ui(parsed, json = json, jsonl = jsonl, quiet = quiet)
    checks <- if (S7::S7_inherits(observed, ShiftReanalysisSpec)) {
        shift__ui_check(ui, "Calibration readiness", function(reporter) {
            reporter$stage_started("check", if (network) {
                "Checking CDS credentials and network access."
            } else "Checking local calibration settings.")
            shift_check(observed, network = network)
        })
    } else {
        shift_diagnostics_empty()
    }
    discovery <- if (network) {
        shift_batch__discover_models(climate, transforms,
            periods = shift__periods_from_input(config$periods),
            references = references, store = store,
            ui = ui)$identities
    } else {
        data.table::data.table()
    }
    # Single-transform local plans preserve their existing case preview. A
    # batch's common model matrix is resolved only with explicit --network or
    # shift run --dry-run; ordinary validation never queries ESGF.
    plan <- if (!batch) epwshiftr_cli_config_plan(config, store = store,
        ui = shift_ui(progress = "none")) else NULL
    list(
        action = "validate",
        intent = cli_shift__config_intent(config),
        status = "valid",
        validation = if (network) "network" else "local",
        readiness = if (any(checks$severity == "error")) "blocked" else {
            if (network) "network_checks_passed" else "local_checks_passed"
        },
        config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
        cases = if (is.null(plan)) data.table::data.table() else shift_cases(plan),
        explain = if (is.null(plan)) data.table::data.table(
            step = c("methods", "models", "discovery"),
            detail = c(paste(vapply(transforms, function(transform) {
                paste(transform@scale, transform@method, transform@reconstruction)
            }, character(1L)), collapse = "; "),
            if (!is.null(climate@model)) paste(climate@model, collapse = ", ") else {
                if (is.null(climate@n_models)) "all compatible models" else
                    sprintf("%d compatible models", climate@n_models)
            },
            if (network) "Network coverage checked" else
                "Not checked locally; use --network or shift run --dry-run")
        ) else shift_explain(plan),
        selected_models = discovery,
        diagnostics = checks
    )
}

# Summarize the user's scientific choices before execution without opening a
# store or resolving remote models. Keep calibration separate from historical
# model reference data so the two input roles remain unambiguous.
cli_shift__config_intent <- function(config) {
    transforms <- shift_batch__transforms(methods = config$methods,
        transform = cli_shift__config_transform(config$transform))
    climate <- epwshiftr_cli_config_climate(config$climate)
    reference <- cli_shift__config_reference(config$reference, "reference")
    calibration <- cli_shift__config_reference(shift_coalesce(
        config$calibration, config$observed_reference), "observed_reference")
    list(Baseline = config$epw,
        Methods = paste(vapply(transforms, function(transform) {
            sprintf("%s / %s / %s [%s]", transform@method, transform@scale,
                transform@reconstruction, transform@status)
        }, character(1L)), collapse = "; "),
        Models = if (!is.null(climate@model)) paste(climate@model, collapse = ", ") else {
            if (is.null(climate@n_models)) "all compatible models" else
                sprintf("%d compatible models", climate@n_models)
        },
        Scenarios = paste(climate@scenarios, collapse = ", "),
        Periods = shift__ui_periods(shift__periods_from_input(config$periods)),
        Reference = shift__format_reference(reference),
        Calibration = shift__format_reference(calibration),
        Output = config$dir)
}


epwshiftr_cli_shift_example_config <- function() {
    list(
        version = 2L,
        epw = system.file(
            "extdata/examples/SGP_Singapore.486980_IWEC.epw",
            package = "epwshiftr",
            mustWork = TRUE
        ),
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
