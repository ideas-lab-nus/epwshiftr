epwshiftr_cli_morph <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        variables = epwshiftr_cli_morph_variables(args),
        backends = epwshiftr_cli_morph_backends(args),
        run = epwshiftr_cli_morph_run(store, args),
        epw = epwshiftr_cli_morph_epw(store, args),
        retry = epwshiftr_cli_morph_retry(store, args),
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
        backend <- epw_morph_backend(name)
        data.table::data.table(
            backend = backend$name,
            label = backend$label,
            required_variables = paste(backend$required_variables(), collapse = ","),
            methods = paste(names(backend$methods()), collapse = ",")
        )
    }), use.names = TRUE, fill = TRUE)
}


epwshiftr_cli_morph_run <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume"),
        options = c("--plan", "--epw", "--recipe", "--strict", "--by"),
        multi_options = c("--period")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    periods <- epwshiftr_cli_periods_from_cli(parsed$options[["--period"]])
    strict <- epwshiftr_cli_bool(parsed$options[["--strict"]], "--strict", default = TRUE)
    morpher <- epw_morpher(
        store,
        epwshiftr_cli_required_option(parsed, "--epw"),
        recipe = epwshiftr_cli_recipe(epwshiftr_cli_config_string(parsed$options[["--recipe"]], default = "belcher"))
    )
    workflow <- morpher$workflow(
        plan_id = epwshiftr_cli_required_ids(parsed, "--plan"),
        periods = periods,
        by = epwshiftr_cli_config_character(
            parsed$options[["--by"]],
            default = c("source_id", "experiment_id", "variant_label", "period")
        ),
        strict = strict,
        dir = NULL,
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )
    epwshiftr_cli_morph_workflow_result(workflow)
}


epwshiftr_cli_morph_epw <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume"),
        options = c("--morph", "--dir", "--separate")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    morph_id <- epwshiftr_cli_required_ids(parsed, "--morph")
    morpher <- epwshiftr_cli_morpher_from_morph_id(store, morph_id)
    morpher$write_epw(
        morph_id = morph_id[[1L]],
        dir = epwshiftr_cli_config_string(parsed$options[["--dir"]], default = "outputs/future-epw"),
        separate = epwshiftr_cli_bool(parsed$options[["--separate"]], "--separate", default = TRUE),
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )
}


epwshiftr_cli_morph_retry <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--run", "--overwrite", "--no-resume"),
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
        morpher <- epwshiftr_cli_morpher_from_morph_id(store, morph_id)
        results[[i]] <- morpher$run(
            morph_id,
            overwrite = isTRUE(parsed$flags[["--overwrite"]]),
            resume = !isTRUE(parsed$flags[["--no-resume"]])
        )
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
