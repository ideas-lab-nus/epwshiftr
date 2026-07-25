epwshiftr_cli_morph <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        variables = epwshiftr_cli_morph_variables(args),
        backends = epwshiftr_cli_morph_backends(args),
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
    parsed <- epwshiftr_cli_parse_command(args, options = c("--recipe"))
    epwshiftr_cli_assert_no_positionals(parsed)
    recipe <- epwshiftr_cli_config_string(parsed$options[["--recipe"]], default = "recommended")
    variables <- if (recipe %in% c("recommended", "minimal", "extended")) {
        epw_morph_variables(recipe)
    } else {
        epw_morph_variables(epwshiftr_cli_recipe(recipe))
    }
    data.table::data.table(variable_id = variables)
}


epwshiftr_cli_morph_backends <- function(args) {
    parsed <- epwshiftr_cli_parse_command(args)
    epwshiftr_cli_assert_no_positionals(parsed)
    names <- epw_morph_backends()
    data.table::rbindlist(lapply(names, function(name) {
        backend <- suppressWarnings(epw_morph_backend(name))
        data.table::data.table(
            backend = backend$name,
            label = backend$label,
            requires_reference = backend$requires_reference,
            required_variables = paste(backend$required_variables(), collapse = ","),
            methods = paste(names(backend$methods()), collapse = ",")
        )
    }), use.names = TRUE, fill = TRUE)
}


epwshiftr_cli_morph_run <- function(store, args, json = FALSE,
                                    jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume", "--no-progress",
            "--reduced-motion", "--verbose", "--debug"),
        options = c("--plan", "--reference", "--reference-plan", "--epw", "--recipe", "--profile", "--policy", "--strict", "--by"),
        multi_options = c("--period", "--reference-period", "--reference-filter", "--reference-option", "--method", "--option")
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
    recipe <- epwshiftr_cli_recipe(
        epwshiftr_cli_config_string(parsed$options[["--recipe"]], default = "belcher"),
        methods = epwshiftr_cli_key_value_list(parsed$options[["--method"]], "--method"),
        profile = epwshiftr_cli_config_string(parsed$options[["--profile"]], default = NULL),
        options = epwshiftr_cli_key_value_list(parsed$options[["--option"]], "--option"),
        policy = epwshiftr_cli_config_string(
            parsed$options[["--policy"]],
            default = NULL
        )
    )
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
    climate <- epwshiftr_cli_climate_stage_from_plan(store, plan_id, periods, epw)
    morphed <- shift_morph(
        climate,
        baseline = epw,
        recipe = recipe,
        reference = reference,
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
    status <- epwshiftr_cli_csv(parsed$options[["--status"]])
    if (is.null(status)) {
        status <- "failed"
    }
    status_choices <- c("planned", "running", "blocked", "failed", "result_done", "epw_written")
    if (any(!status %in% status_choices)) {
        epwshiftr_cli_usage_abort(sprintf(
            "--status must be one of: %s.",
            paste(status_choices, collapse = ", ")
        ))
    }
    candidates <- epwshiftr_cli_morph_status_rows(
        store,
        epwshiftr_cli_ids(parsed$options[["--morph"]], "--morph", required = FALSE)
    )
    if (nrow(candidates)) {
        candidates <- candidates[candidates[["status"]] %in% status]
    }
    if (!isTRUE(parsed$flags[["--run"]]) || !nrow(candidates)) {
        if (nrow(candidates)) {
            candidates[, dry_run := TRUE]
        }
        return(candidates)
    }
    results <- vector("list", nrow(candidates))
    for (i in seq_len(nrow(candidates))) {
        morph_id <- candidates$morph_id[[i]]
        previous <- epwshiftr_cli_morphed_stage_from_morph_id(store, morph_id)
        morphed <- shift_morph(
            previous@meta$climate,
            baseline = previous@meta$baseline,
            recipe = previous@meta$recipe,
            reference_plan_id = previous@meta$reference_plan_id,
            reference_periods = previous@meta$reference_periods,
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
