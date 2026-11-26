# epwshiftr_cli_extract {{{
epwshiftr_cli_extract <- function(
    store,
    command,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    if (command %in% c("run", "retry")) {
        fn <- if (identical(command, "run")) {
            epwshiftr_cli_extract_run
        } else {
            epwshiftr_cli_extract_retry
        }
        return(fn(store, args, json = json, jsonl = jsonl, quiet = quiet))
    }
    switch(
        command,
        plan = epwshiftr_cli_extract_plan(store, args),
        coverage = epwshiftr_cli_extract_coverage(store, args),
        artifacts = epwshiftr_cli_extract_artifacts(store, args),
        epwshiftr_cli_usage_abort(sprintf(
            "Unknown extract command: %s",
            command
        ))
    )
}
# }}}

# cli_extract__run_plans {{{
cli_extract__run_plans <- function(
    store,
    plan_id,
    fallback,
    parsed,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    climate <- shift_run__extract_plans_task(
        store,
        plan_id = plan_id,
        fallback = fallback,
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]]),
        ui = epwshiftr_cli_task_ui(
            parsed,
            json = json,
            jsonl = jsonl,
            quiet = quiet
        )
    )
    cli_morph__attach_ids(climate@meta$processed, climate)
}
# }}}

# epwshiftr_cli_extract_plan {{{
epwshiftr_cli_extract_plan <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c(
            "--query",
            "--site-id",
            "--lon",
            "--lat",
            "--time",
            "--variable",
            "--method"
        ),
        multi_options = c("--filter"),
        no_positionals = TRUE
    )
    query_id <- epwshiftr_cli_required_single_id(parsed, "--query")
    site_id <- epwshiftr_cli_required_option(parsed, "--site-id")
    lon <- epwshiftr_cli_number(
        epwshiftr_cli_required_option(parsed, "--lon"),
        "--lon"
    )
    lat <- epwshiftr_cli_number(
        epwshiftr_cli_required_option(parsed, "--lat"),
        "--lat"
    )
    time <- epwshiftr_cli_time_range(epwshiftr_cli_required_option(
        parsed,
        "--time"
    ))
    store$plan_region(
        query_id = query_id,
        site_id = site_id,
        lon = lon,
        lat = lat,
        time = time,
        variable_id = epwshiftr_cli_csv(parsed$options[["--variable"]]),
        filters = epwshiftr_cli_key_value_list(parsed$options[["--filter"]]),
        method = epwshiftr_cli_choice(
            parsed$options[["--method"]],
            ESG_GRID_METHOD_CHOICES,
            "--method",
            default = "nearest"
        )
    )
}
# }}}

# epwshiftr_cli_extract_run {{{
epwshiftr_cli_extract_run <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = CLI_RUN_FLAGS,
        options = c("--plan", "--fallback"),
        no_positionals = TRUE
    )
    fallback <- epwshiftr_cli_choice(
        parsed$options[["--fallback"]],
        c("auto", "error"),
        "--fallback",
        default = "auto"
    )
    cli_extract__run_plans(
        store,
        plan_id = epwshiftr_cli_required_ids(parsed, "--plan"),
        fallback = fallback,
        parsed = parsed,
        json = json,
        jsonl = jsonl,
        quiet = quiet
    )
}
# }}}

# epwshiftr_cli_extract_retry {{{
epwshiftr_cli_extract_retry <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--run", CLI_RUN_FLAGS),
        options = c("--plan", "--status", "--fallback"),
        no_positionals = TRUE
    )
    statuses <- cli_retry__resolve_statuses(
        parsed$options[["--status"]],
        c("pending", "failed", "empty", "done")
    )
    fallback <- epwshiftr_cli_choice(
        parsed$options[["--fallback"]],
        c("auto", "error"),
        "--fallback",
        default = "auto"
    )
    candidates <- store$coverage(
        plan_id = epwshiftr_cli_ids(
            parsed$options[["--plan"]],
            "--plan",
            required = FALSE
        )
    )
    retry <- cli_retry__prepare_candidates(
        candidates,
        statuses,
        parsed$flags[["--run"]]
    )
    if (!retry$execute) {
        return(retry$candidates)
    }
    cli_extract__run_plans(
        store,
        plan_id = unique(retry$candidates$plan_id),
        fallback = fallback,
        parsed = parsed,
        json = json,
        jsonl = jsonl,
        quiet = quiet
    )
}
# }}}

# epwshiftr_cli_extract_coverage {{{
epwshiftr_cli_extract_coverage <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = "--plan",
        no_positionals = TRUE
    )
    store$coverage(
        plan_id = epwshiftr_cli_ids(
            parsed$options[["--plan"]],
            "--plan",
            required = FALSE
        )
    )
}
# }}}

# epwshiftr_cli_extract_artifacts {{{
epwshiftr_cli_extract_artifacts <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = "--plan",
        no_positionals = TRUE
    )
    results <- shift_inspect__extraction_result_rows(
        store,
        epwshiftr_cli_required_ids(parsed, "--plan")
    )
    shift_inspect__artifact_rows(store, results$artifact_id)
}
# }}}

# vim: fdm=marker :
