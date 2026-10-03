#' @include shift-stage.R
NULL

#' @rdname shift_api
#' @param plan_id Store extraction plan IDs for manually selected reference
#'   climate data.
#' @param role Semantic role of the plan-backed climate. Use
#'   `"observed_reference"` only for an observational extraction plan.
#' @export
shift_reference_plan <- function(
    plan_id,
    periods,
    role = c("model_historical", "observed_reference")
) {
    checkmate::assert_character(
        plan_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    periods <- shift_reference__periods(periods)
    role <- match.arg(role)

    ShiftReferenceSpec(
        mode = "plan",
        role = role,
        plan_id = plan_id,
        periods = periods,
        experiment = NULL,
        activity = NULL,
        match = character(),
        filters = list(),
        options = list(),
        collect = list(),
        extract = list()
    )
}

#' @rdname shift_api
#' @param period Reference period name used when constructing periods from
#'   `years`.
#' @export
historical_reference <- function(years = 1995:2014, period = "reference", ...) {
    shift_reference_historical(
        shift_spec__periods_from_years(years, period = period, arg = "years"),
        ...
    )
}

#' @rdname shift_api
#' @param match File metadata fields copied from the future climate stage when
#'   resolving an automatic historical reference.
#' @param collect Named collection options. Historical reference collection may
#'   use `fields`, `all`, `limit`, `label`, and `time`; [shift_plan()] applies
#'   the same strict field validation to its collection stage.
#' @param extract Named extraction options. Historical reference extraction may
#'   use `variables`, `time`, `filters`, `method`, and `fallback`;
#'   [shift_plan()] applies the same strict field validation to its extraction
#'   stage.
#' @export
shift_reference_historical <- function(
    periods,
    experiment = "historical",
    activity = "CMIP",
    match = c(
        "source_id",
        "variant_label",
        "frequency",
        "table_id",
        "grid_label"
    ),
    filters = list(),
    options = list(),
    collect = list(),
    extract = list(fallback = "auto")
) {
    periods <- shift_reference__periods(periods)
    checkmate::assert_string(experiment, min.chars = 1L)
    checkmate::assert_string(activity, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_character(
        match,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")
    checkmate::assert_list(collect, names = "unique")
    checkmate::assert_subset(
        names(collect),
        c("fields", "all", "limit", "label", "time")
    )
    checkmate::assert_list(extract, names = "unique")
    checkmate::assert_subset(
        names(extract),
        c("variables", "time", "filters", "method", "fallback")
    )

    ShiftReferenceSpec(
        mode = "historical",
        role = "model_historical",
        plan_id = NULL,
        periods = periods,
        experiment = experiment,
        activity = activity,
        match = match,
        filters = filters,
        options = options,
        collect = collect,
        extract = extract
    )
}

shift_reference__periods <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    data.table::as.data.table(periods)
}

shift_reference__resolve <- function(
    x,
    recipe,
    site,
    reference = NULL,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    if (is.null(reference)) {
        return(list(
            reference = NULL,
            spec = NULL,
            plan_id = NULL,
            periods = NULL
        ))
    }

    if (S7::S7_inherits(reference, ShiftClimate)) {
        reference_ids <- shift_ids(reference)
        return(list(
            reference = reference,
            spec = NULL,
            plan_id = reference_ids$plan_id,
            periods = shift_reference__periods(reference@meta$periods)
        ))
    }

    if (!S7::S7_inherits(reference, ShiftReferenceSpec)) {
        cli::cli_abort(
            "`reference` must be a {.cls ShiftClimate} stage or a {.cls ShiftReferenceSpec}."
        )
    }

    if (identical(reference@mode, "plan")) {
        return(list(
            reference = reference,
            spec = reference,
            plan_id = reference@plan_id,
            periods = shift_reference__periods(reference@periods)
        ))
    }

    if (identical(reference@mode, "historical")) {
        climate <- shift_reference__resolve_historical(
            x = x,
            recipe = recipe,
            site = site,
            spec = reference,
            overwrite = overwrite,
            resume = resume,
            reporter = reporter
        )
        climate_ids <- shift_ids(climate)
        return(list(
            reference = climate,
            spec = reference,
            plan_id = climate_ids$plan_id,
            periods = shift_reference__periods(climate@meta$periods)
        ))
    }

    cli::cli_abort("Unsupported reference mode: {.val {reference@mode}}.")
}

# Resolve observed daily weather only from an already extracted climate stage
# or explicit plan IDs. Automatic CMIP historical discovery cannot satisfy the
# observational role and is rejected before any store work begins.
shift_reference__observed_reference_resolve <- function(
    x,
    recipe,
    site,
    observed_reference = NULL,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    if (S7::S7_inherits(observed_reference, ShiftReanalysisSpec)) {
        climate <- reanalysis__materialize(
            x = x,
            recipe = recipe,
            site = site,
            spec = observed_reference,
            overwrite = overwrite,
            resume = resume,
            reporter = reporter
        )
        climate_ids <- shift_ids(climate)
        return(list(
            reference = climate,
            spec = observed_reference,
            plan_id = climate_ids$plan_id,
            periods = shift_reference__periods(climate@meta$periods)
        ))
    }
    if (
        S7::S7_inherits(observed_reference, ShiftReferenceSpec) &&
            !identical(observed_reference@mode, "plan")
    ) {
        cli::cli_abort(
            paste(
                "{.arg observed_reference} must use an existing extraction",
                "plan; historical CMIP output is not an observation."
            )
        )
    }
    shift_reference__resolve(
        x = x,
        recipe = recipe,
        site = site,
        reference = observed_reference,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
}

shift_reference__resolve_historical <- function(
    x,
    recipe,
    site,
    spec,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    root <- shift_stage__root(x)
    if (!is.null(root) && !S7::S7_inherits(root, ShiftRequest)) {
        root <- NULL
    }
    provider <- if (is.null(root)) "esgf" else root@meta$provider
    if (!identical(provider, "esgf")) {
        cli::cli_abort(
            "Automatic historical reference resolution currently supports only ESGF-backed shift requests."
        )
    }

    store <- shift_store(x)
    ids <- shift_ids(x)
    catalog <- if (!is.null(ids$query_id)) {
        shift_inspect__file_catalog(store, ids$query_id)
    } else {
        data.table::data.table()
    }

    periods <- shift_reference__periods(spec@periods)
    variables <- shift_stage__coalesce(
        spec@extract$variables,
        morpher__input_variables(recipe)
    )
    variables <- as.character(variables)
    variables <- variables[!is.na(variables) & nzchar(variables)]
    if (!length(variables)) {
        cli::cli_abort(
            "Automatic historical reference resolution could not determine required climate variables."
        )
    }

    filters <- shift_reference__historical_filters(
        catalog = catalog,
        request = root,
        spec = spec,
        variables = variables
    )
    options <- utils::modifyList(
        if (is.null(root)) list() else root@meta$options,
        spec@options
    )
    project <- shift_stage__coalesce(
        if (is.null(root)) NULL else root@meta$project,
        "CMIP6"
    )
    # Historical Dataset records often span the full CMIP run; only constrain
    # ESGF collection by time when the caller explicitly requests it.
    collect_time <- if ("time" %in% names(spec@collect)) {
        spec@collect$time
    } else {
        NULL
    }
    request <- shift_request(
        provider = provider,
        project = project,
        time = collect_time,
        filters = filters,
        options = options
    )

    collect_overrides <- spec@collect
    collect_overrides$time <- NULL
    collect_args <- utils::modifyList(
        list(
            store = store,
            fields = "*",
            all = TRUE,
            limit = FALSE,
            label = "historical-reference"
        ),
        collect_overrides
    )
    files <- shift_run__do_call_with_reporter(
        reporter,
        shift_collect,
        c(list(request), collect_args)
    )
    if (is.null(files@meta$file_count) || files@meta$file_count < 1L) {
        cli::cli_abort(
            "Automatic historical reference query returned no File records."
        )
    }

    extract_filters <- filters[intersect(
        names(filters),
        c(
            "experiment_id",
            "activity_id",
            "source_id",
            "variant_label",
            "frequency",
            "table_id",
            "grid_label"
        )
    )]
    extract_defaults <- list(
        site = site,
        periods = periods,
        variables = variables,
        time = shift_spec__method_time_window(periods, recipe),
        filters = extract_filters,
        method = "nearest",
        fallback = "auto",
        overwrite = overwrite,
        resume = resume
    )
    extract_overrides <- spec@extract
    if (!is.null(extract_overrides$filters)) {
        extract_overrides$filters <- utils::modifyList(
            extract_filters,
            extract_overrides$filters
        )
    }
    extract_args <- utils::modifyList(extract_defaults, extract_overrides)
    extract_args$site <- site
    extract_args$periods <- periods
    extract_args$overwrite <- overwrite
    extract_args$resume <- resume
    climate <- shift_run__do_call_with_reporter(
        reporter,
        shift_extract,
        c(list(files), extract_args)
    )
    shift_climate__derive_hurs_climate(
        climate,
        recipe,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
}

shift_reference__historical_filters <- function(
    catalog,
    request,
    spec,
    variables
) {
    filters <- list(
        experiment_id = spec@experiment,
        variable_id = variables
    )
    if (!is.null(spec@activity)) {
        filters$activity_id <- spec@activity
    }

    missing <- character()
    for (field in spec@match) {
        if (!is.null(spec@filters[[field]])) {
            next
        }
        values <- shift_reference__infer_field(field, catalog, request)
        if (!length(values)) {
            missing <- c(missing, field)
        } else {
            filters[[field]] <- values
        }
    }
    if (length(missing)) {
        cli::cli_abort(c(
            "Automatic historical reference resolution could not infer required match field(s).",
            "x" = "{.field {missing}}",
            "i" = "Supply explicit values through `shift_reference_historical(filters = ...)` or reduce `match`."
        ))
    }

    utils::modifyList(filters, spec@filters)
}

shift_reference__infer_field <- function(field, catalog, request) {
    values <- character()
    if (field %in% names(catalog) && nrow(catalog)) {
        values <- unique(as.character(unlist(
            catalog[[field]],
            use.names = FALSE
        )))
    }
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values)) {
        return(values)
    }

    if (!is.null(request)) {
        alias <- switch(
            field,
            source_id = request@meta$source,
            experiment_id = request@meta$experiment,
            variant_label = request@meta$variant,
            frequency = request@meta$frequency,
            variable_id = request@meta$variables,
            NULL
        )
        values <- unique(as.character(unlist(alias, use.names = FALSE)))
        values <- values[!is.na(values) & nzchar(values)]
        if (length(values)) {
            return(values)
        }

        filter_value <- request@meta$filters[[field]]
        values <- unique(as.character(unlist(filter_value, use.names = FALSE)))
        return(values[!is.na(values) & nzchar(values)])
    }

    character()
}
