epwshiftr_cli_shift_config <- function(store, args) {
    if (!length(args)) {
        epwshiftr_cli_usage_abort("Missing shift config command: example or validate.")
    }
    action <- args[[1L]]
    rest <- args[-1L]
    switch(
        action,
        example = epwshiftr_cli_shift_config_example(rest),
        validate = epwshiftr_cli_shift_config_validate(store, rest),
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


epwshiftr_cli_shift_config_validate <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--config")
    epwshiftr_cli_assert_no_positionals(parsed)
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    plan <- epwshiftr_cli_config_plan(config, store = store)
    list(
        action = "validate",
        status = "valid",
        config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
        cases = shift_cases(plan),
        explain = shift_explain(plan)
    )
}


epwshiftr_cli_shift_example_config <- function() {
    list(
        version = 1L,
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
            frequency = "mon",
            table = NULL
        ),
        periods = list(`2060s` = "2055:2065"),
        # Prefer a matching historical climate reference. Users may omit this
        # block only when no suitable reference data are available; omission
        # never triggers an implicit historical request.
        method = list(
            name = "belcher",
            profile = "enhanced",
            options = unclass(belcher_options()),
            reference = list(
                mode = "historical",
                periods = list(reference = "1995:2014")
            )
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
