epwshiftr_cli_shift_config <- function(store, args) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort("Missing shift config command: example or validate.")
    }
    action <- args[[1L]]
    rest <- args[-1L]
    switch(
        action,
        example = epwshiftr_cli_shift_config_example(rest),
        validate = epwshiftr_cli_shift_config_validate(rest),
        epwshiftr_cli_usage_abort(sprintf("Unknown shift config command: %s", action))
    )
}


epwshiftr_cli_read_shift_config <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)
    if (!file.exists(path)) {
        epwshiftr_cli_usage_abort(sprintf("Config file does not exist: %s", path))
    }
    config <- tryCatch(
        jsonlite::read_json(path, simplifyVector = TRUE),
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
    parsed <- epwshiftr_cli_parse_command(args, flags = "--overwrite", options = "--output")
    epwshiftr_cli_assert_no_positionals(parsed)
    config <- epwshiftr_cli_shift_example_config()
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


epwshiftr_cli_shift_config_validate <- function(args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--config")
    epwshiftr_cli_assert_no_positionals(parsed)
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    list(
        action = "validate",
        status = "valid",
        config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
        request = data.table::as.data.table(epwshiftr_cli_config_request(config$request)),
        site = data.table::as.data.table(epwshiftr_cli_config_site(config$site)),
        periods = epwshiftr_cli_periods_from_config(config$extract$periods)
    )
}


epwshiftr_cli_shift_example_config <- function() {
    list(
        request = list(
            project = "CMIP6",
            experiment = "ssp585",
            variables = c("tas", "hurs", "psl", "rsds", "rlds", "sfcWind", "clt"),
            frequency = "day",
            filters = list(
                source_id = "EC-Earth3",
                variant_label = "r1i1p1f1"
            )
        ),
        site = list(
            id = "SIN",
            lon = 103.98,
            lat = 1.37,
            label = "singapore",
            epw = "baseline/SIN.epw"
        ),
        collect = list(
            label = "singapore-ssp585"
        ),
        download = list(
            run = FALSE,
            background = FALSE,
            session_label = "singapore-ssp585"
        ),
        extract = list(
            periods = list(`2060s` = 2055L:2064L),
            time = c("2055-01-01T00:00:00Z", "2064-12-31T23:59:59Z"),
            nearest = 1L,
            fallback = "auto"
        ),
        morph = list(
            recipe = "belcher",
            strict = TRUE,
            by = c("source_id", "experiment_id", "variant_label", "period")
        ),
        epw = list(
            dir = "outputs/future-epw",
            separate = TRUE
        )
    )
}
