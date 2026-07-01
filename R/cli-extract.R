epwshiftr_cli_extract <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        plan = epwshiftr_cli_extract_plan(store, args),
        run = epwshiftr_cli_extract_run(store, args),
        retry = epwshiftr_cli_extract_retry(store, args),
        coverage = epwshiftr_cli_extract_coverage(store, args),
        artifacts = epwshiftr_cli_extract_artifacts(store, args),
        epwshiftr_cli_usage_abort(sprintf("Unknown extract command: %s", command))
    )
}


epwshiftr_cli_extract_plan <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--query", "--site-id", "--lon", "--lat", "--time", "--variable", "--method"),
        multi_options = c("--filter")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    query_id <- epwshiftr_cli_required_single_id(parsed, "--query")
    site_id <- epwshiftr_cli_required_option(parsed, "--site-id")
    lon <- epwshiftr_cli_number(epwshiftr_cli_required_option(parsed, "--lon"), "--lon")
    lat <- epwshiftr_cli_number(epwshiftr_cli_required_option(parsed, "--lat"), "--lat")
    time <- epwshiftr_cli_time_range(epwshiftr_cli_required_option(parsed, "--time"))
    store$plan_region(
        query_id = query_id,
        site_id = site_id,
        lon = lon,
        lat = lat,
        time = time,
        variable_id = epwshiftr_cli_csv(parsed$options[["--variable"]]),
        filters = epwshiftr_cli_key_value_list(parsed$options[["--filter"]]),
        method = epwshiftr_cli_choice(parsed$options[["--method"]], ESG_GRID_METHOD_CHOICES, "--method", default = "nearest")
    )
}


epwshiftr_cli_extract_run <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--overwrite", "--no-resume"),
        options = c("--plan", "--fallback")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    fallback <- epwshiftr_cli_choice(parsed$options[["--fallback"]], c("auto", "error"), "--fallback", default = "auto")
    store$extract(
        plan_id = epwshiftr_cli_required_ids(parsed, "--plan"),
        fallback = fallback,
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )
}


epwshiftr_cli_extract_retry <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--run", "--overwrite", "--no-resume"),
        options = c("--plan", "--status", "--fallback")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    status <- epwshiftr_cli_csv(parsed$options[["--status"]])
    if (is.null(status)) {
        status <- "failed"
    }
    status_choices <- c("pending", "failed", "empty", "done")
    if (any(!status %in% status_choices)) {
        epwshiftr_cli_usage_abort(sprintf(
            "--status must be one of: %s.",
            paste(status_choices, collapse = ", ")
        ))
    }
    fallback <- epwshiftr_cli_choice(parsed$options[["--fallback"]], c("auto", "error"), "--fallback", default = "auto")
    candidates <- store$coverage(plan_id = epwshiftr_cli_ids(parsed$options[["--plan"]], "--plan", required = FALSE))
    if (nrow(candidates)) {
        candidates <- candidates[candidates[["status"]] %in% status]
    }
    if (!isTRUE(parsed$flags[["--run"]]) || !nrow(candidates)) {
        if (nrow(candidates)) {
            candidates[, dry_run := TRUE]
        }
        return(candidates)
    }
    store$extract(
        plan_id = unique(candidates$plan_id),
        fallback = fallback,
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )
}


epwshiftr_cli_extract_coverage <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--plan"))
    epwshiftr_cli_assert_no_positionals(parsed)
    store$coverage(plan_id = epwshiftr_cli_ids(parsed$options[["--plan"]], "--plan", required = FALSE))
}


epwshiftr_cli_extract_artifacts <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--plan"))
    epwshiftr_cli_assert_no_positionals(parsed)
    results <- shift_extraction_result_rows(store, epwshiftr_cli_required_ids(parsed, "--plan"))
    shift_artifact_rows(store, results$artifact_id)
}
