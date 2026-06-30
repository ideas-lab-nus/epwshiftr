epwshiftr_cli_shift <- function(store, command, args, json = FALSE, jsonl = FALSE, quiet = FALSE) {
    switch(
        command,
        run = epwshiftr_cli_shift_run(store, args),
        show = epwshiftr_cli_shift_show(store, args),
        config = epwshiftr_cli_shift_config(store, args),
        watch = epwshiftr_cli_shift_watch(store, args, jsonl = jsonl, quiet = quiet),
        status = epwshiftr_cli_shift_status(store, args),
        diagnostics = epwshiftr_cli_shift_diagnostics(store, args),
        outputs = epwshiftr_cli_shift_outputs(store, args),
        data = epwshiftr_cli_shift_data(store, args),
        epwshiftr_cli_usage_abort(sprintf("Unknown shift command: %s", command))
    )
}

# shift -----------------------------------------------------------------------

epwshiftr_cli_shift_run <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--dry-run", "--download", "--overwrite", "--no-resume", "--no-progress"),
        options = c("--config")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    config_path <- epwshiftr_cli_required_option(parsed, "--config")
    config <- epwshiftr_cli_read_shift_config(config_path)
    request <- epwshiftr_cli_config_request(config$request)
    site <- epwshiftr_cli_config_site(config$site)
    periods <- epwshiftr_cli_periods_from_config(config$extract$periods)
    if (isTRUE(parsed$flags[["--dry-run"]])) {
        return(list(
            status = "dry_run",
            config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
            request = data.table::as.data.table(request),
            site = data.table::as.data.table(site),
            periods = periods
        ))
    }

    collect <- epwshiftr_cli_config_section(config, "collect")
    collect_store <- epwshiftr_cli_list_value(collect, "store", store)
    collect_args <- list(
        store = collect_store,
        fields = epwshiftr_cli_config_character(collect$fields, default = "*"),
        all = epwshiftr_cli_config_flag(collect$all, default = TRUE),
        limit = epwshiftr_cli_config_limit(collect$limit, default = FALSE),
        label = epwshiftr_cli_config_string(collect$label, default = NULL)
    )
    files <- do.call(shift_collect, c(list(request), collect_args))
    stage <- files

    download_cfg <- epwshiftr_cli_config_section(config, "download")
    download_run <- isTRUE(parsed$flags[["--download"]]) || isTRUE(epwshiftr_cli_config_flag(download_cfg$run, default = FALSE))
    if (download_run) {
        download_args <- epwshiftr_cli_download_args_from_config(download_cfg)
        download_args$run <- if (isTRUE(parsed$flags[["--download"]])) TRUE else epwshiftr_cli_config_flag(download_cfg$run, default = TRUE)
        download_args$background <- epwshiftr_cli_config_flag(download_cfg$background, default = FALSE)
        download_args$resume <- !isTRUE(parsed$flags[["--no-resume"]])
        download_args$overwrite <- isTRUE(parsed$flags[["--overwrite"]])
        download_args$progress <- !isTRUE(parsed$flags[["--no-progress"]])
        stage <- do.call(shift_download, c(list(stage), download_args))
    }

    extract <- epwshiftr_cli_config_section(config, "extract")
    if (!is.null(extract$nearest)) {
        epwshiftr_cli_usage_abort("extract.nearest is no longer supported; use extract.method.")
    }
    climate <- shift_extract(
        stage,
        site = site,
        periods = periods,
        variables = epwshiftr_cli_config_character(extract$variables, default = NULL),
        time = epwshiftr_cli_config_time(extract$time),
        filters = epwshiftr_cli_config_named_list(extract$filters),
        method = epwshiftr_cli_config_choice(extract$method, ESG_GRID_METHOD_CHOICES, default = "nearest"),
        fallback = epwshiftr_cli_config_choice(extract$fallback, c("auto", "error"), default = "auto"),
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )

    morph <- epwshiftr_cli_config_section(config, "morph")
    recipe <- epwshiftr_cli_recipe(
        epwshiftr_cli_config_string(morph$recipe, default = "belcher"),
        methods = morph$methods
    )
    reference_args <- epwshiftr_cli_config_reference(morph)
    morphed <- do.call(
        shift_morph,
        c(
            list(
                climate,
                recipe = recipe,
                strict = epwshiftr_cli_config_flag(morph$strict, default = TRUE),
                by = epwshiftr_cli_config_character(morph$by, default = c("source_id", "experiment_id", "variant_label", "period")),
                overwrite = isTRUE(parsed$flags[["--overwrite"]]),
                resume = !isTRUE(parsed$flags[["--no-resume"]])
            ),
            reference_args
        )
    )

    epw <- epwshiftr_cli_config_section(config, "epw")
    outputs <- shift_epw(
        morphed,
        dir = epwshiftr_cli_config_string(epw$dir, default = "outputs/future-epw"),
        separate = epwshiftr_cli_config_flag(epw$separate, default = TRUE),
        overwrite = isTRUE(parsed$flags[["--overwrite"]]),
        resume = !isTRUE(parsed$flags[["--no-resume"]])
    )
    epwshiftr_cli_shift_stage_result(outputs)
}

epwshiftr_cli_shift_status <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--query", "--plan", "--morph"))
    epwshiftr_cli_assert_no_positionals(parsed)
    selector <- epwshiftr_cli_selector(parsed)
    if (identical(selector$type, "query")) {
        return(epwshiftr_cli_query_status(store, selector$ids))
    }
    if (identical(selector$type, "plan")) {
        return(store$coverage(plan_id = selector$ids))
    }
    if (identical(selector$type, "morph")) {
        return(epwshiftr_cli_morph_status_rows(store, selector$ids))
    }
    epwshiftr_cli_query_status(store, NULL)
}


epwshiftr_cli_shift_diagnostics <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--query", "--plan", "--morph"))
    epwshiftr_cli_assert_no_positionals(parsed)
    selector <- epwshiftr_cli_selector(parsed)
    if (identical(selector$type, "query")) {
        return(epwshiftr_cli_query_diagnostics(store, selector$ids))
    }
    if (identical(selector$type, "plan")) {
        return(shift_diagnostics_from_coverage(store$coverage(plan_id = selector$ids)))
    }
    if (identical(selector$type, "morph")) {
        return(epwshiftr_cli_morph_diagnostics(store, selector$ids))
    }
    data.table::rbindlist(
        list(
            epwshiftr_cli_query_diagnostics(store, NULL),
            shift_diagnostics_from_coverage(store$coverage()),
            epwshiftr_cli_morph_diagnostics(store, NULL)
        ),
        use.names = TRUE,
        fill = TRUE
    )
}


epwshiftr_cli_shift_outputs <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--morph"))
    epwshiftr_cli_assert_no_positionals(parsed)
    morph_id <- epwshiftr_cli_required_ids(parsed, "--morph")
    shift_epw_output_rows(store, morph_id)
}


epwshiftr_cli_shift_data <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        options = c("--plan", "--morph", "--case", "--columns", "--limit")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    has_plan <- !is.null(parsed$options[["--plan"]])
    has_morph <- !is.null(parsed$options[["--morph"]])
    if (identical(has_plan, has_morph)) {
        epwshiftr_cli_usage_abort("shift data requires exactly one of --plan or --morph.")
    }
    columns <- epwshiftr_cli_csv(parsed$options[["--columns"]])
    limit <- epwshiftr_cli_count_or_default(parsed$options[["--limit"]], "--limit", 20L, positive = FALSE)
    if (has_plan) {
        if (!is.null(parsed$options[["--case"]])) {
            epwshiftr_cli_usage_abort("--case is only supported with --morph.")
        }
        results <- shift_extraction_result_rows(store, epwshiftr_cli_required_ids(parsed, "--plan"))
        return(epwshiftr_cli_read_extracted_data(store, results, n = limit, columns = columns))
    }
    results <- shift_morph_result_rows(
        store,
        epwshiftr_cli_required_ids(parsed, "--morph"),
        case_id = epwshiftr_cli_csv(parsed$options[["--case"]])
    )
    shift_read_morph_data(store, results, n = limit, columns = columns)
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


epwshiftr_cli_selector <- function(parsed) {
    values <- list(
        query = parsed$options[["--query"]],
        plan = parsed$options[["--plan"]],
        morph = parsed$options[["--morph"]]
    )
    present <- names(values)[vapply(values, Negate(is.null), logical(1L))]
    if (length(present) > 1L) {
        epwshiftr_cli_usage_abort("Use only one of --query, --plan, or --morph.")
    }
    if (!length(present)) {
        return(list(type = NULL, ids = NULL))
    }
    type <- present[[1L]]
    list(type = type, ids = epwshiftr_cli_ids(values[[type]], paste0("--", type)))
}


# config coercion -------------------------------------------------------------

epwshiftr_cli_validate_shift_config <- function(config) {
    extract <- config$extract
    if (!is.null(extract$nearest)) {
        cli::cli_abort("extract.nearest is no longer supported; use extract.method.")
    }
    morph <- config$morph
    if (is.null(morph)) {
        return(invisible(config))
    }
    if (!is.null(morph$reference) && (!is.null(morph$reference_plan) || !is.null(morph$reference_periods))) {
        cli::cli_abort("Use either morph.reference or morph.reference_plan/morph.reference_periods, not both.")
    }
    invisible(config)
}


epwshiftr_cli_config_section <- function(config, name) {
    value <- config[[name]]
    if (is.null(value)) list() else value
}


epwshiftr_cli_config_request <- function(config) {
    config <- epwshiftr_cli_config_section(list(request = config), "request")
    shift_request(
        provider = epwshiftr_cli_config_string(config$provider, default = "esgf"),
        project = epwshiftr_cli_config_string(config$project, default = NULL),
        source = epwshiftr_cli_config_character(config$source, default = NULL),
        experiment = epwshiftr_cli_config_character(config$experiment, default = NULL),
        variant = epwshiftr_cli_config_character(config$variant, default = NULL),
        variables = epwshiftr_cli_config_character(config$variables, default = NULL),
        frequency = epwshiftr_cli_config_character(config$frequency, default = NULL),
        time = epwshiftr_cli_config_time(config$time),
        filters = epwshiftr_cli_config_named_list(config$filters),
        options = epwshiftr_cli_config_named_list(config$options)
    )
}


epwshiftr_cli_config_site <- function(config) {
    config <- epwshiftr_cli_config_section(list(site = config), "site")
    shift_site(
        id = epwshiftr_cli_config_string(config$id, default = NULL),
        lon = epwshiftr_cli_config_number(config$lon, default = NULL),
        lat = epwshiftr_cli_config_number(config$lat, default = NULL),
        label = epwshiftr_cli_config_string(config$label, default = NULL),
        epw = epwshiftr_cli_config_string(config$epw, default = NULL),
        metadata = epwshiftr_cli_config_named_list(config$metadata)
    )
}


epwshiftr_cli_config_reference <- function(morph) {
    reference <- morph$reference
    legacy <- !is.null(morph$reference_plan) || !is.null(morph$reference_periods)
    if (!is.null(reference) && legacy) {
        epwshiftr_cli_usage_abort("Use either morph.reference or morph.reference_plan/morph.reference_periods, not both.")
    }

    if (is.null(reference)) {
        return(list(
            reference_plan_id = epwshiftr_cli_config_character(morph$reference_plan, default = NULL),
            reference_periods = if (is.null(morph$reference_periods)) {
                NULL
            } else {
                epwshiftr_cli_periods_from_config(morph$reference_periods, "morph.reference_periods")
            }
        ))
    }

    reference <- epwshiftr_cli_config_section(list(reference = reference), "reference")
    mode <- epwshiftr_cli_config_choice(reference$mode, c("historical", "plan"), default = NULL)
    if (is.null(mode)) {
        epwshiftr_cli_usage_abort("morph.reference.mode is required.")
    }
    periods <- epwshiftr_cli_periods_from_config(reference$periods, "morph.reference.periods")

    if (identical(mode, "plan")) {
        plan_id <- epwshiftr_cli_config_character(shift_coalesce(reference$plan, reference$plan_id), default = NULL)
        if (is.null(plan_id)) {
            epwshiftr_cli_usage_abort("morph.reference.plan is required when morph.reference.mode is plan.")
        }
        return(list(reference = shift_reference_plan(plan_id, periods)))
    }

    list(reference = shift_reference_historical(
        periods = periods,
        experiment = epwshiftr_cli_config_string(reference$experiment, default = "historical"),
        activity = epwshiftr_cli_config_string(reference$activity, default = "CMIP"),
        match = epwshiftr_cli_config_character(
            reference$match,
            default = c("source_id", "variant_label", "frequency", "table_id")
        ),
        filters = epwshiftr_cli_config_named_list(reference$filters),
        options = epwshiftr_cli_config_named_list(reference$options),
        collect = epwshiftr_cli_config_named_list(reference$collect),
        extract = epwshiftr_cli_config_named_list(reference$extract)
    ))
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


epwshiftr_cli_recipe <- function(value = "belcher", methods = NULL) {
    value <- tolower(epwshiftr_cli_config_string(value, default = "belcher"))
    if (!value %in% epw_morph_backends()) {
        epwshiftr_cli_usage_abort(sprintf("Unknown morph recipe/backend: %s", value))
    }
    methods <- epwshiftr_cli_recipe_methods(methods)
    epw_morph_recipe(value, methods = methods)
}


epwshiftr_cli_recipe_methods <- function(methods) {
    if (is.null(methods)) {
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


epwshiftr_cli_recipe_from_json <- function(json) {
    parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$name)) {
        return(epw_morph_recipe("belcher"))
    }
    methods <- parsed$methods
    if (!is.null(methods) && (is.null(names(methods)) || any(!nzchar(names(methods))))) {
        methods <- NULL
    }
    epw_morph_recipe(parsed$name, backend = if (is.null(parsed$backend)) parsed$name else parsed$backend, methods = methods)
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
                action = "Run `extract coverage` or inspect `shift diagnostics --plan`."
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
    ids <- shift_ids(stage)
    diagnostics <- shift_diagnostics(stage)
    coverage <- tryCatch(shift_coverage(stage), error = function(e) data.table::data.table())
    outputs <- tryCatch(shift_outputs(stage), error = function(e) data.table::data.table())
    list(
        status = shift_status(stage),
        query_id = ids$query_id,
        plan_id = ids$plan_id,
        morph_id = ids$morph_id,
        diagnostic_count = nrow(diagnostics),
        coverage = epwshiftr_cli_coverage_summary(coverage),
        outputs = outputs,
        next_steps = epwshiftr_cli_shift_next_steps(ids)
    )
}


epwshiftr_cli_shift_next_steps <- function(ids) {
    rows <- list()
    if (length(ids$query_id)) {
        rows[[length(rows) + 1L]] <- data.frame(
            step = "status",
            command = sprintf("epwshiftr shift status --query %s", ids$query_id[[1L]]),
            stringsAsFactors = FALSE
        )
    }
    if (length(ids$plan_id)) {
        rows[[length(rows) + 1L]] <- data.frame(
            step = "diagnostics",
            command = sprintf("epwshiftr shift diagnostics --plan %s", paste(ids$plan_id, collapse = ",")),
            stringsAsFactors = FALSE
        )
    }
    if (length(ids$morph_id)) {
        rows[[length(rows) + 1L]] <- data.frame(
            step = c("outputs", "data"),
            command = c(
                sprintf("epwshiftr shift outputs --morph %s", ids$morph_id[[1L]]),
                sprintf("epwshiftr shift data --morph %s --limit 5", ids$morph_id[[1L]])
            ),
            stringsAsFactors = FALSE
        )
    }
    if (!length(rows)) {
        return(data.frame(step = character(), command = character()))
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
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
