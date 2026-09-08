epwshiftr_cli_morph <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        variables = epwshiftr_cli_morph_variables(args),
        transforms = cli_morph__transforms(args),
        run = epwshiftr_cli_morph_run(store, args, json = json,
            jsonl = jsonl, quiet = quiet),
        epw = epwshiftr_cli_morph_epw(store, args, json = json,
            jsonl = jsonl, quiet = quiet),
        retry = epwshiftr_cli_morph_retry(store, args, json = json,
            jsonl = jsonl, quiet = quiet),
        status = epwshiftr_cli_morph_status(store, args),
        outputs = epwshiftr_cli_morph_outputs(store, args),
        epwshiftr_cli_usage_abort(sprintf("Unknown morph command: %s", command))
    )
}


epwshiftr_cli_morph_variables <- function(args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--scale", "--method", "--reconstruction")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    transform <- cli_morph__transform(parsed)
    variables <- epw_morph_variables(transform__recipe(transform))
    data.table::data.table(variable_id = variables)
}


# Return the public transform catalog through the standalone morph command
# without exposing backend or internal recipe identifiers.
cli_morph__transforms <- function(args) {
    parsed <- epwshiftr_cli_parse_command(args)
    epwshiftr_cli_assert_no_positionals(parsed)
    columns <- c(
        "scale",
        "method",
        "label",
        "reconstruction",
        "statistical_grouping",
        "output_type",
        "status"
    )
    # Explicit character-column selection keeps the CLI projection visible to
    # R CMD check without changing the public catalog's data.table type.
    weather_transforms()[, columns, with = FALSE]
}


# Coerce scalar CLI option values before the public constructor validates the
# selected scientific method's option schema.
cli_morph__option_value <- function(value) {
    if (length(value) != 1L) {
        return(value)
    }
    lowered <- tolower(value)
    if (lowered %in% c("true", "false")) {
        return(identical(lowered, "true"))
    }
    numeric_value <- suppressWarnings(as.numeric(value))
    if (!is.na(numeric_value) && is.finite(numeric_value)) {
        if (grepl("^[+-]?[0-9]+$", value)) {
            return(as.integer(numeric_value))
        }
        return(numeric_value)
    }
    value
}


# Build a public transform from standalone morph command flags so CLI and R
# callers share the same registry, validation, and canonical defaults.
cli_morph__transform <- function(parsed) {
    scale <- epwshiftr_cli_choice(
        parsed$options[["--scale"]],
        WEATHER_TRANSFORM_SCALES,
        "--scale",
        default = "monthly"
    )
    method <- epwshiftr_cli_config_string(
        parsed$options[["--method"]],
        default = "belcher"
    )
    reconstruction <- epwshiftr_cli_config_string(
        parsed$options[["--reconstruction"]],
        default = NULL
    )
    options <- epwshiftr_cli_key_value_list(
        parsed$options[["--option"]],
        "--option"
    )
    options <- lapply(options, cli_morph__option_value)
    constructor <- get(
        paste0(scale, "_transform"),
        mode = "function",
        inherits = TRUE
    )
    do.call(
        constructor,
        c(list(method = method, reconstruction = reconstruction), options)
    )
}


epwshiftr_cli_morph_run <- function(store, args, json = FALSE,
                                    jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume", "--no-progress",
            "--reduced-motion", "--verbose", "--debug"),
        options = c(
            "--plan", "--reference", "--reference-plan", "--epw",
            "--scale", "--method", "--reconstruction", "--strict", "--by",
            "--observed-plan"
        ),
        multi_options = c(
            "--period", "--reference-period", "--reference-filter",
            "--reference-option", "--observed-period", "--option"
        )
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    periods <- epwshiftr_cli_periods_from_cli(parsed$options[["--period"]])
    reference_mode <- epwshiftr_cli_choice(parsed$options[["--reference"]], c("historical", "plan"), "--reference", default = NULL)
    reference_plan_id <- epwshiftr_cli_ids(parsed$options[["--reference-plan"]], "--reference-plan", required = FALSE)
    if (is.null(reference_mode) && length(reference_plan_id)) {
        reference_mode <- "plan"
    }
    if (is.null(reference_mode) && length(parsed$options[["--reference-period"]])) {
        epwshiftr_cli_usage_abort("--reference-period requires --reference or --reference-plan.")
    }
    if (!identical(reference_mode, "historical") &&
        (length(parsed$options[["--reference-filter"]]) || length(parsed$options[["--reference-option"]]))) {
        epwshiftr_cli_usage_abort("--reference-filter and --reference-option require --reference historical.")
    }
    reference_periods <- if (!is.null(reference_mode)) {
        epwshiftr_cli_periods_from_cli(parsed$options[["--reference-period"]])
    } else {
        NULL
    }
    strict <- epwshiftr_cli_bool(parsed$options[["--strict"]], "--strict", default = TRUE)
    plan_id <- epwshiftr_cli_required_ids(parsed, "--plan")
    epw <- epwshiftr_cli_required_option(parsed, "--epw")
    transform <- cli_morph__transform(parsed)
    by <- epwshiftr_cli_config_character(
        parsed$options[["--by"]],
        default = c("source_id", "experiment_id", "variant_label", "period")
    )

    reference <- NULL
    if (identical(reference_mode, "historical")) {
        if (length(reference_plan_id)) {
            epwshiftr_cli_usage_abort("--reference-plan cannot be used with --reference historical.")
        }
        reference <- shift_reference_historical(
            reference_periods,
            filters = epwshiftr_cli_key_value_list(parsed$options[["--reference-filter"]], "--reference-filter"),
            options = epwshiftr_cli_key_value_list(parsed$options[["--reference-option"]], "--reference-option")
        )
    }

    if (identical(reference_mode, "plan") && !length(reference_plan_id)) {
        epwshiftr_cli_usage_abort("--reference-plan is required when --reference is plan.")
    }
    if (identical(reference_mode, "plan")) {
        reference <- shift_reference_plan(reference_plan_id, reference_periods)
    }
    observed_reference <- NULL
    observed_plan_id <- epwshiftr_cli_ids(
        parsed$options[["--observed-plan"]],
        "--observed-plan",
        required = FALSE
    )
    if (length(observed_plan_id)) {
        observed_periods <- epwshiftr_cli_periods_from_cli(
            parsed$options[["--observed-period"]]
        )
        observed_reference <- shift_reference_plan(
            observed_plan_id,
            observed_periods
        )
    } else if (length(parsed$options[["--observed-period"]])) {
        epwshiftr_cli_usage_abort(
            "--observed-period requires --observed-plan."
        )
    }
    climate <- epwshiftr_cli_climate_stage_from_plan(store, plan_id, periods, epw)
    morphed <- shift_morph(
        climate,
        baseline = epw,
        transform = transform,
        reference = reference,
        observed_reference = observed_reference,
        by = by,
        strict = strict,
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]]),
        ui = epwshiftr_cli_task_ui(parsed, json = json, jsonl = jsonl,
            quiet = quiet)
    )
    result <- epwshiftr_cli_morph_workflow_result(morphed@meta$workflow)
    result$run_id <- shift_ids(morphed)$run_id
    result$step_id <- shift_ids(morphed)$step_id
    result
}


epwshiftr_cli_morph_epw <- function(store, args, json = FALSE,
                                    jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume", "--no-progress",
            "--reduced-motion", "--verbose", "--debug"),
        options = c("--morph", "--dir", "--separate")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    morph_id <- epwshiftr_cli_required_ids(parsed, "--morph")
    morphed <- epwshiftr_cli_morphed_stage_from_morph_id(store, morph_id)
    outputs <- shift_epw(
        morphed,
        dir = epwshiftr_cli_config_string(parsed$options[["--dir"]], default = "outputs/future-epw"),
        separate = epwshiftr_cli_bool(parsed$options[["--separate"]], "--separate", default = TRUE),
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]]),
        ui = epwshiftr_cli_task_ui(parsed, json = json, jsonl = jsonl,
            quiet = quiet)
    )
    result <- shift_outputs(outputs)
    result[, `:=`(run_id = shift_ids(outputs)$run_id,
        step_id = shift_ids(outputs)$step_id)]
    result[]
}


epwshiftr_cli_morph_retry <- function(store, args, json = FALSE,
                                      jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--run", "--overwrite", "--no-resume", "--no-progress",
            "--reduced-motion", "--verbose", "--debug"),
        options = c("--morph", "--status")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    status_choices <- c("planned", "running", "blocked", "failed", "result_done", "epw_written")
    statuses <- cli_retry__resolve_statuses(
        parsed$options[["--status"]],
        status_choices
    )
    candidates <- epwshiftr_cli_morph_status_rows(
        store,
        epwshiftr_cli_ids(parsed$options[["--morph"]], "--morph", required = FALSE)
    )
    retry <- cli_retry__prepare_candidates(
        candidates,
        statuses,
        parsed$flags[["--run"]]
    )
    if (!retry$execute) {
        return(retry$candidates)
    }
    candidates <- retry$candidates
    results <- vector("list", nrow(candidates))
    for (i in seq_len(nrow(candidates))) {
        morph_id <- candidates$morph_id[[i]]
        previous <- epwshiftr_cli_morphed_stage_from_morph_id(store, morph_id)
        morphed <- shift_morph(
            previous@meta$climate,
            baseline = previous@meta$baseline,
            transform = previous@meta$transform,
            reference = previous@meta$reference,
            observed_reference = previous@meta$observed_reference,
            by = previous@meta$by,
            strict = previous@meta$strict,
            overwrite = isTRUE(parsed$flags[["--overwrite"]]),
            resume = !isTRUE(parsed$flags[["--no-resume"]]),
            ui = epwshiftr_cli_task_ui(parsed, json = json, jsonl = jsonl,
                quiet = quiet)
        )
        row <- data.table::as.data.table(morphed@meta$results)
        row[, `:=`(run_id = shift_ids(morphed)$run_id,
            step_id = shift_ids(morphed)$step_id)]
        results[[i]] <- row
    }
    data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}


epwshiftr_cli_morph_status <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--morph"))
    epwshiftr_cli_assert_no_positionals(parsed)
    epwshiftr_cli_morph_status_rows(
        store,
        epwshiftr_cli_ids(parsed$options[["--morph"]], "--morph", required = FALSE)
    )
}


epwshiftr_cli_morph_outputs <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--morph"))
    epwshiftr_cli_assert_no_positionals(parsed)
    epwshiftr_cli_morph_output_rows(
        store,
        epwshiftr_cli_ids(parsed$options[["--morph"]], "--morph", required = FALSE)
    )
}


epwshiftr_cli_morph_workflow_result <- function(workflow) {
    list(
        status = if (nrow(workflow$plan)) workflow$plan$status[[1L]] else NA_character_,
        plan_id = unique(workflow$climate$plan_id),
        morph_id = unique(workflow$plan$morph_id),
        diagnostic_count = nrow(workflow$diagnostics),
        plan = workflow$plan,
        results = workflow$results
    )
}
