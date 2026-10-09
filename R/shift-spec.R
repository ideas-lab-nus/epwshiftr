#' @include shift-stage.R
NULL

# Construct workflow intent without running source reads or jobs.

# Convert validated study years to an inclusive UTC request interval.

# shift_spec__periods_time {{{
shift_spec__periods_time <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    years <- as.integer(periods$year)
    years <- years[!is.na(years)]
    if (!length(years)) {
        cli::cli_abort(
            "`periods` must contain at least one non-missing `year`."
        )
    }
    shift_spec__time_window(range(years))
}
# }}}

# Expand a requested period by the method's declared temporal support while
# preserving the original years as the case and coverage contract.
# shift_spec__method_time_window {{{
shift_spec__method_time_window <- function(periods, recipe) {
    window <- as.POSIXct(
        shift_spec__periods_time(periods),
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
    )
    padding <- morpher__recipe_time_padding_seconds(recipe)
    window <- window + c(-padding, padding)
    format(window, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
# }}}

# Expand one or two integer years; already explicit time intervals pass through.
# shift_spec__time_window {{{
shift_spec__time_window <- function(time) {
    if (is.null(time)) {
        return(NULL)
    }
    if (is.numeric(time) && !inherits(time, c("Date", "POSIXt"))) {
        checkmate::assert_integerish(
            time,
            any.missing = FALSE,
            min.len = 1L,
            max.len = 2L
        )
        years <- as.integer(time)
        years <- range(years)
        return(c(
            sprintf("%04d-01-01T00:00:00Z", years[[1L]]),
            sprintf("%04d-12-31T23:59:59Z", years[[2L]])
        ))
    }
    time
}
# }}}

# Parse year tokens once for R and CLI. Validate complete tokens before integer
# conversion so decimal years cannot be silently truncated. Range expansion is
# variable-length; collect its pieces and concatenate once rather than growing.
# shift_spec__years_value {{{
shift_spec__years_value <- function(value, arg = "years") {
    if (is.numeric(value) && !inherits(value, c("Date", "POSIXt"))) {
        checkmate::assert_integerish(value, any.missing = FALSE, min.len = 1L)
        return(as.integer(value))
    }
    checkmate::assert_character(
        value,
        any.missing = FALSE,
        min.len = 1L,
        .var.name = arg
    )
    if (any(grepl(",[[:space:]]*$", value))) {
        cli::cli_abort("`{arg}` contains an empty year after a comma.")
    }
    pieces <- trimws(unlist(
        strsplit(value, ",", fixed = TRUE),
        use.names = FALSE
    ))
    valid <- grepl("^[0-9]+([[:space:]]*:[[:space:]]*[0-9]+)?$", pieces)
    if (!length(pieces) || !all(valid)) {
        cli::cli_abort(
            "`{arg}` contains an invalid year or year range: {.val {pieces[!valid]}}."
        )
    }
    bounds <- strsplit(pieces, ":", fixed = TRUE)
    parsed <- lapply(bounds, function(piece) {
        years <- suppressWarnings(as.integer(trimws(piece)))
        if (anyNA(years)) {
            cli::cli_abort("`{arg}` contains years outside the integer range.")
        }
        if (length(years) == 1L) years else seq.int(min(years), max(years))
    })
    unique(unlist(parsed, use.names = FALSE))
}
# }}}

# Normalize period inputs so individual target years, explicit period tables,
# and named multi-year windows all reach the same canonical two-column form.
# shift_spec__periods_from_input {{{
shift_spec__periods_from_input <- function(periods, arg = "periods") {
    if (is.data.frame(periods)) {
        checkmate::assert_names(
            names(periods),
            must.include = c("period", "year")
        )
        return(data.table::as.data.table(periods))
    }
    if (is.numeric(periods) && !inherits(periods, c("Date", "POSIXt"))) {
        years <- shift_spec__years_value(periods, arg)
        checkmate::assert_integerish(
            years,
            lower = 1900,
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE
        )
        values <- as.list(as.integer(years))
        names(values) <- as.character(years)
        return(do.call(epw_morph_periods, values))
    }
    if (
        !is.list(periods) ||
            is.null(names(periods)) ||
            any(!nzchar(names(periods)))
    ) {
        cli::cli_abort(
            "`{arg}` must be target years, a period table, or a named list of years."
        )
    }
    values <- lapply(seq_along(periods), function(i) {
        shift_spec__years_value(
            periods[[i]],
            sprintf("%s$%s", arg, names(periods)[[i]])
        )
    })
    do.call(epw_morph_periods, stats::setNames(values, names(periods)))
}
# }}}

# Build a one-period table from the common years + period_name shorthand.
# shift_spec__periods_from_years {{{
shift_spec__periods_from_years <- function(
    years,
    period = "future",
    arg = "years"
) {
    checkmate::assert_string(period, min.chars = 1L)
    years <- shift_spec__years_value(years, arg = arg)
    do.call(epw_morph_periods, stats::setNames(list(years), period))
}
# }}}

# Resolve recipe strings early so later workflow stages can rely on a recipe
# object and its required variable set.
# shift_spec__recipe_value {{{
shift_spec__recipe_value <- function(recipe) {
    if (inherits(recipe, "epw_morph_recipe")) {
        return(recipe)
    }
    if (is.character(recipe) && length(recipe) == 1L) {
        return(epw_morph_recipe(recipe))
    }
    cli::cli_abort(
        "`recipe` must be a recipe name or an {.cls epw_morph_recipe} object."
    )
}
# }}}

# Let high-level APIs accept named variable sets while leaving explicit CMIP
# variable IDs untouched.
# shift_spec__variables_value {{{
shift_spec__variables_value <- function(variables, recipe = NULL) {
    if (is.null(variables)) {
        return(epw_morph_variables(shift_stage__coalesce(
            recipe,
            "recommended"
        )))
    }
    if (
        inherits(variables, "epw_morph_recipe") ||
            inherits(variables, "EpwMorphBackend")
    ) {
        return(epw_morph_variables(variables))
    }
    variables <- as.character(variables)
    if (
        length(variables) == 1L &&
            variables %in%
                c(names(EPW_MORPH_VARIABLE_LEVELS), epw_morph_backends())
    ) {
        return(epw_morph_variables(variables))
    }
    variables[!is.na(variables) & nzchar(variables)]
}
# }}}

# Validate middle-layer stage options before a plan is created so misspellings
# and attempts to override workflow-wide policies cannot be silently ignored.
# shift_spec__validate_stage_options {{{
shift_spec__validate_stage_options <- function(x, stage, allowed) {
    checkmate::assert_list(x, names = "unique")
    if (length(x) && (is.null(names(x)) || any(!nzchar(names(x))))) {
        cli::cli_abort("Every `{stage}` stage option must be named.")
    }
    workflow_fields <- c(
        "strict",
        "allow_partial",
        "resume",
        "overwrite",
        "complete_only",
        "run",
        "download",
        "method"
    )
    duplicated_policy <- intersect(names(x), workflow_fields)
    if (length(duplicated_policy)) {
        cli::cli_abort(c(
            "`{stage}` cannot override workflow control field(s): {.field {duplicated_policy}}.",
            "i" = "Configure workflow-wide behaviour with {.fn shift_control}."
        ))
    }
    unknown <- setdiff(names(x), allowed)
    if (length(unknown)) {
        cli::cli_abort("Unknown `{stage}` stage option(s): {.field {unknown}}.")
    }
    x
}
# }}}

# Build the immutable user case matrix before member and grid auto-selection;
# unresolved dimensions remain explicit missing values until the resolver pins
# them for the persisted run.
# shift_spec__expected_cases {{{
shift_spec__expected_cases <- function(request, periods) {
    request_meta <- request@meta
    sources <- shift_stage__coalesce(
        request_meta$source,
        request_meta$filters$source_id
    )
    experiments <- shift_stage__coalesce(
        request_meta$experiment,
        request_meta$filters$experiment_id
    )
    members <- shift_stage__coalesce(
        request_meta$variant,
        request_meta$filters$variant_label
    )
    grids <- request_meta$filters$grid_label
    scalar_or_missing <- function(value) {
        value <- as.character(value)
        if (length(value)) value else NA_character_
    }
    sources <- scalar_or_missing(sources)
    experiments <- scalar_or_missing(experiments)
    members <- scalar_or_missing(members)
    grids <- scalar_or_missing(grids)
    period_names <- unique(as.character(periods$period))

    cases <- data.table::CJ(
        source_id = sources,
        experiment_id = experiments,
        variant_label = members,
        grid_label = grids,
        period = period_names,
        unique = TRUE
    )
    # Keep the exact requested year set as a list column because coverage is a
    # case-level contract, not just a min/max time filter.
    cases[,
        years := lapply(period, function(value) {
            as.integer(periods[periods[["period"]] == value, year])
        })
    ]
    cases[,
        case_id := vapply(
            seq_len(.N),
            function(i) {
                store__hash(
                    source_id[[i]],
                    experiment_id[[i]],
                    variant_label[[i]],
                    grid_label[[i]],
                    period[[i]],
                    years[[i]]
                )
            },
            character(1L)
        )
    ]
    cases[, `:=`(
        required = TRUE,
        status = "planned",
        output_id = NA_character_,
        export_path = NA_character_,
        missing_reason = NA_character_
    )]
    data.table::setcolorder(
        cases,
        c(
            "case_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "period",
            "years",
            "required",
            "status",
            "output_id",
            "export_path",
            "missing_reason"
        )
    )
    cases[]
}
# }}}

# Record the durable baseline EPW identity used for run hashing and resume.
# shift_spec__epw_identity {{{
shift_spec__epw_identity <- function(epw) {
    if (shift_spec__is_epw_path(epw)) {
        path <- normalizePath(path.expand(epw), winslash = "/", mustWork = TRUE)
        return(list(
            path = path,
            checksum = store_hash_file(path, "sha256"),
            checksum_type = "sha256"
        ))
    }
    if (shift_spec__is_epw_object(epw)) {
        path <- epw_file_coerce(epw)$path()
        return(list(
            path = path,
            checksum = store_hash_file(path, "sha256"),
            checksum_type = "sha256"
        ))
    }
    cli::cli_abort(
        "`epw` must be an EPW file path or an object inheriting from {.cls Epw} or {.cls EpwFile}."
    )
}
# }}}

# Choose CMIP table defaults that match the most common atmospheric frequencies.
# shift_spec__cmip6_table_id {{{
shift_spec__cmip6_table_id <- function(frequency) {
    frequency <- as.character(frequency)[[1L]]
    switch(
        frequency,
        mon = "Amon",
        day = "day",
        `3hr` = "3hr",
        `3hrPt` = "3hr",
        `6hr` = "6hr",
        `6hrPt` = "6hr",
        NULL
    )
}
# }}}

# Validate scalar and variable-specific CMIP6 frequency specifications without
# discarding names that are needed after a broad multi-frequency ESGF query.
# shift_spec__cmip6_frequency_spec {{{
shift_spec__cmip6_frequency_spec <- function(frequency, variables = NULL) {
    if (is.list(frequency)) {
        if (
            is.null(names(frequency)) ||
                anyNA(names(frequency)) ||
                any(!nzchar(names(frequency)))
        ) {
            cli::cli_abort(
                "A list supplied as `frequency` must name every variable."
            )
        }
        frequency <- vapply(
            frequency,
            function(value) {
                checkmate::assert_string(value, min.chars = 1L)
                value
            },
            character(1L)
        )
    }
    checkmate::assert_character(
        frequency,
        any.missing = FALSE,
        min.len = 1L
    )
    frequency_names <- names(frequency)
    frequency <- as.character(frequency)
    names(frequency) <- frequency_names
    if (!is.null(frequency_names) && anyNA(frequency_names)) {
        cli::cli_abort(
            "A variable-specific `frequency` vector cannot have missing names."
        )
    }
    named <- !is.null(frequency_names) && any(nzchar(frequency_names))
    if (!isTRUE(named)) {
        if (length(frequency) != 1L) {
            cli::cli_abort(
                "An unnamed `frequency` value must contain one CMIP6 frequency."
            )
        }
        names(frequency) <- NULL
        return(frequency)
    }
    if (any(!nzchar(frequency_names)) || anyDuplicated(frequency_names)) {
        cli::cli_abort(
            "A variable-specific `frequency` vector must have unique, non-empty variable names."
        )
    }
    if (!is.null(variables)) {
        variables <- unique(as.character(variables))
        unknown <- setdiff(frequency_names, variables)
        missing <- setdiff(variables, frequency_names)
        if (length(unknown)) {
            cli::cli_abort(
                "`frequency` contains variable(s) not used by the request: {.val {unknown}}."
            )
        }
        if (length(missing)) {
            cli::cli_abort(
                "`frequency` must specify every requested variable; missing {.val {missing}}."
            )
        }
        frequency <- frequency[variables]
    }
    frequency
}
# }}}

# Expand one scalar CMIP6 frequency or retain an explicit variable mapping so
# downstream table selection and File coverage use the same source semantics.
# shift_spec__cmip6_variable_frequencies {{{
shift_spec__cmip6_variable_frequencies <- function(variables, frequency) {
    variables <- unique(as.character(variables))
    checkmate::assert_character(
        variables,
        any.missing = FALSE,
        min.len = 1L
    )
    frequency <- shift_spec__cmip6_frequency_spec(frequency, variables)
    if (is.null(names(frequency))) {
        return(stats::setNames(
            rep(frequency[[1L]], length(variables)),
            variables
        ))
    }
    frequency
}
# }}}

# Validate the two supported table-selection forms. An unnamed scalar pins all
# variables to one table, while a fully named vector overrides only the named
# variables and leaves the remainder on their automatic tables.
# shift_spec__cmip6_table_spec {{{
shift_spec__cmip6_table_spec <- function(table, null.ok = TRUE) {
    if (is.null(table)) {
        if (isTRUE(null.ok)) {
            return(NULL)
        }
        cli::cli_abort("`table` cannot be `NULL` here.")
    }
    if (is.list(table)) {
        if (is.null(names(table)) || any(!nzchar(names(table)))) {
            cli::cli_abort(
                "A list supplied as `table` must have one name for every variable override."
            )
        }
        table <- vapply(
            table,
            function(value) {
                checkmate::assert_string(value, min.chars = 1L)
                value
            },
            character(1L)
        )
    }
    checkmate::assert_character(table, any.missing = FALSE, min.len = 1L)
    table_names <- names(table)
    table <- as.character(table)
    names(table) <- table_names
    named <- !is.null(names(table)) && any(nzchar(names(table)))
    if (isTRUE(named) && any(!nzchar(names(table)))) {
        cli::cli_abort("A named `table` vector must name every element.")
    }
    if (!isTRUE(named) && length(table) != 1L) {
        cli::cli_abort("An unnamed `table` value must be a single table ID.")
    }
    if (isTRUE(named) && anyDuplicated(names(table))) {
        cli::cli_abort("Variable names in `table` must be unique.")
    }
    table
}
# }}}

# Resolve each requested source variable to its CMIP6 table. Snow depth is a
# land-state variable in LImon; all other monthly inputs retain the atmospheric
# Amon default unless the caller pins or overrides them explicitly.
# shift_spec__cmip6_variable_tables {{{
shift_spec__cmip6_variable_tables <- function(
    variables,
    frequency,
    table = NULL
) {
    variables <- unique(as.character(variables))
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L)
    table <- shift_spec__cmip6_table_spec(table)
    frequencies <- shift_spec__cmip6_variable_frequencies(variables, frequency)
    defaults <- vapply(
        frequencies,
        function(value) {
            shift_stage__coalesce(
                shift_spec__cmip6_table_id(value),
                NA_character_
            )
        },
        character(1L)
    )
    unresolved <- names(defaults)[is.na(defaults)]
    if (length(unresolved) && is.null(table)) {
        cli::cli_abort(
            "Cannot infer a CMIP6 table for variable(s) {.val {unresolved}}; set `table` explicitly."
        )
    }
    out <- defaults
    if ("snd" %in% variables && identical(frequencies[["snd"]], "mon")) {
        out[["snd"]] <- "LImon"
    }
    if (is.null(table)) {
        return(out)
    }
    if (is.null(names(table))) {
        out[] <- table[[1L]]
        return(out)
    }
    unknown <- setdiff(names(table), variables)
    if (length(unknown)) {
        cli::cli_abort(
            "`table` contains override(s) for variables not used by the recipe: {.val {unknown}}."
        )
    }
    out[names(table)] <- unname(table)
    if (anyNA(out)) {
        unresolved <- names(out)[is.na(out)]
        cli::cli_abort(
            "`table` must specify variable(s) whose CMIP6 table cannot be inferred: {.val {unresolved}}."
        )
    }
    out
}
# }}}

# Interpret one direct request table as a pin, while treating a multi-table
# query filter as discovery breadth whose variable mapping must be inferred.
# shift_spec__cmip6_request_table_spec {{{
shift_spec__cmip6_request_table_spec <- function(table_id) {
    if (is.null(table_id) || length(table_id) != 1L) {
        return(NULL)
    }
    table_id
}
# }}}

# shift_spec__is_epw_object {{{
shift_spec__is_epw_object <- function(x) {
    inherits(x, "EpwFile") || epw_file_is_external(x)
}
# }}}

# shift_spec__is_epw_path {{{
shift_spec__is_epw_path <- function(x) {
    is.character(x) &&
        length(x) == 1L &&
        identical(tolower(tools::file_ext(x)), "epw")
}
# }}}

# shift_spec__location_value {{{
shift_spec__location_value <- function(location, names) {
    if (is.null(location)) {
        return(NULL)
    }
    if (is.data.frame(location)) {
        if (!nrow(location)) {
            return(NULL)
        }
        for (name in names) {
            if (name %in% names(location)) {
                value <- location[[name]][[1L]]
                if (!is.na(value) && nzchar(as.character(value))) {
                    return(value)
                }
            }
        }
        return(NULL)
    }
    for (name in names) {
        value <- location[[name]]
        if (
            !is.null(value) &&
                length(value) &&
                !is.na(value[[1L]]) &&
                nzchar(as.character(value[[1L]]))
        ) {
            return(value[[1L]])
        }
    }
    NULL
}
# }}}

# Read only the LOCATION header for path-backed site defaults; weather data
# remain unopened until extraction or generation actually needs them.
# shift_spec__epw_location {{{
shift_spec__epw_location <- function(epw) {
    if (is.null(epw)) {
        return(NULL)
    }
    epw_obj <- if (shift_spec__is_epw_path(epw)) {
        if (!file.exists(epw)) {
            cli::cli_abort("EPW file does not exist: {.path {epw}}.")
        }
        return(epw_file_location(readLines(epw, n = 1L, warn = FALSE)))
    } else if (shift_spec__is_epw_object(epw)) {
        epw_file_coerce(epw)
    } else {
        cli::cli_abort(
            "`epw` must be an EPW file path or an object inheriting from {.cls Epw} or {.cls EpwFile}."
        )
    }
    epw_obj$location()
}
# }}}

# shift_spec__site_default_id {{{
shift_spec__site_default_id <- function(epw, location) {
    if (shift_spec__is_epw_path(epw)) {
        return(tools::file_path_sans_ext(basename(epw)))
    }
    id <- shift_spec__location_value(
        location,
        c("wmo_number", "city", "location")
    )
    if (is.null(id)) {
        return("site")
    }
    as.character(id)
}
# }}}

# shift_spec__resolve_epw {{{
shift_spec__resolve_epw <- function(x) {
    if (S7::S7_inherits(x, ShiftSite)) {
        x <- x@epw
    }
    if (is.null(x)) {
        cli::cli_abort("A baseline EPW file is required.")
    }
    if (is.character(x) && length(x) == 1L) {
        return(epw_file_read(x))
    }
    if (shift_spec__is_epw_object(x)) {
        return(epw_file_coerce(x))
    }
    cli::cli_abort(
        "A baseline EPW must be a file path or an object inheriting from {.cls Epw} or {.cls EpwFile}."
    )
}
# }}}

# constructors
#' Store-native shift workflow API
#'
#' @description
#' `shift_*()` functions provide a stage-oriented workflow facade over
#' [EsgQuery], [EsgStore], [Downloader], and [EpwMorpher]. Each step returns a
#' small S7 stage object that can be printed, inspected, saved, and passed to the
#' next step without manually passing manifest IDs.
#' Method/model batches can be reopened with [shift_batch_get()] and inspected
#' with the same status, case, output, diagnostic, and explanation functions.
#' [shift_watch()] follows all active children; [shift_resume()] starts saved
#' child plans or resumes interrupted runs independently.
#'
#' @param provider Climate data provider. The first implementation supports
#'   `"esgf"`.
#' @param project Optional provider project, for example `"CMIP6"`.
#' @param source,experiment,variant,frequency Provider-neutral request fields.
#'   Values must use the selected provider's controlled vocabulary.
#'   In `shift_cmip6()`, an unnamed scalar `frequency` applies to every transform
#'   input, while a named character vector assigns one frequency to every
#'   source variable so `3hrPt`, `3hr`, and `day` data can be collected together.
#'   In `shift_reference_historical()`, `experiment` is the historical
#'   reference experiment filter. Values are not translated; for ESGF, use
#'   exact facet values such as `project = "CMIP6"` and `frequency = "mon"`.
#' @param time Optional request or extraction time filter. Numeric years such as
#'   `2060L` are expanded to the full UTC year; otherwise supply one or two
#'   date-time values accepted by the provider/store.
#' @param variables Provider-neutral request alias in [shift_request()], optional
#'   extraction variables in [shift_extract()], or optional variables to read in
#'   `shift_data()`.
#' @param filters Provider-specific query filters in [shift_request()], or
#'   extraction filters in [shift_extract()].
#' @param options Provider-specific request options. For ESGF, `index_node` and
#'   `time_filter_method` are recognized.
#' @param id Optional site identifier. If `id` is an EPW file path and `epw`
#'   is `NULL`, it is treated as `epw`.
#' @param lon,lat Optional site longitude and latitude. Missing values are read
#'   from the EPW LOCATION header when `epw` is supplied.
#' @param epw A baseline EPW path, internal `EpwFile`, or external object
#'   inheriting from `"Epw"` in site and task APIs; in [shift_plan()], a named
#'   EPW export option list.
#' @param metadata Optional site metadata.
#' @param ... Additional provider-specific filters or workflow options.
#'
#' @return A shift stage object.
#'
#' @name shift_api
NULL

#' @rdname shift_api
#' @export
# shift_request {{{
shift_request <- function(
    provider = "esgf",
    project = NULL,
    source = NULL,
    experiment = NULL,
    variant = NULL,
    variables = NULL,
    frequency = NULL,
    time = NULL,
    filters = list(),
    options = list(),
    ...
) {
    checkmate::assert_string(provider, min.chars = 1L)
    provider <- tolower(provider)
    checkmate::assert_string(project, null.ok = TRUE)
    checkmate::assert_character(
        source,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        experiment,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        variant,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        variables,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_character(
        frequency,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    if (!is.null(time)) {
        checkmate::assert_atomic_vector(
            time,
            any.missing = FALSE,
            min.len = 1L,
            max.len = 2L
        )
        time <- shift_spec__time_window(time)
    }
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")

    dots <- list(...)
    if (length(dots)) {
        nms <- names(dots)
        if (is.null(nms) || any(!nzchar(nms))) {
            cli::cli_abort(
                "Additional request filters supplied in `...` must be named."
            )
        }
        filters <- utils::modifyList(filters, dots)
    }

    meta <- list(
        provider = provider,
        project = project,
        source = source,
        experiment = experiment,
        variant = variant,
        variables = variables,
        frequency = frequency,
        time = time,
        filters = filters,
        options = options
    )

    shift_stage__new(ShiftRequest, "request", meta = meta)
}
# }}}

#' @rdname shift_api
#' @export
# shift_site {{{
shift_site <- function(
    id = NULL,
    lon = NULL,
    lat = NULL,
    label = NULL,
    epw = NULL,
    metadata = list()
) {
    if (
        is.null(epw) &&
            (shift_spec__is_epw_path(id) || shift_spec__is_epw_object(id))
    ) {
        epw <- id
        id <- NULL
    }
    if (epw_file_is_external(epw)) {
        # Convert once at the public site boundary so every downstream stage
        # sees only the internal EPW protocol.
        epw <- epw_file_coerce(epw)
    }

    needs_location <- is.null(id) ||
        is.null(lon) ||
        is.null(lat) ||
        is.null(label)
    location <- if (needs_location) shift_spec__epw_location(epw) else NULL
    if (is.null(lon)) {
        lon <- shift_spec__location_value(location, c("longitude", "lon"))
    }
    if (is.null(lat)) {
        lat <- shift_spec__location_value(location, c("latitude", "lat"))
    }
    if (is.null(id)) {
        id <- shift_spec__site_default_id(epw, location)
    }
    if (is.null(label)) {
        label <- shift_spec__location_value(location, c("city", "location"))
    }

    checkmate::assert_string(id, min.chars = 1L)
    checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
    checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
    checkmate::assert_string(label, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(metadata, names = "unique")

    ShiftSite(
        stage = "site",
        store_path = NULL,
        ids = list(),
        meta = list(),
        diagnostics = shift_stage__diagnostics_empty(),
        id = id,
        lon = lon,
        lat = lat,
        label = label,
        epw = epw,
        metadata = metadata
    )
}
# }}}

#' @rdname shift_api
#' @param model CMIP6 model selection. A positive whole number selects that
#'   many compatible models after complete File-year coverage is verified,
#'   preferring candidates that require fewer source files and then sorting by
#'   model/member/grid identity; a character vector selects explicit
#'   source/model IDs; `NULL` selects every compatible model. The default
#'   selects three models.
#' @param common A logical flag for batch candidate selection. `TRUE`
#'   (default) uses the same model/member/grid identities across methods.
#'   `FALSE` selects independently: a numeric `model` requests that many models
#'   per method, `NULL` keeps all compatible models, and explicit IDs limit the
#'   eligible pool. Every explicit model must qualify for at least one method.
#'   Different pools are reported in batch diagnostics; their results confound
#'   method differences with model selection. File-year coverage is checked
#'   before either selection policy is applied.
#' @param scenarios CMIP6 future scenario experiment IDs.
#' @param member Optional CMIP6 variant labels. In high-level automatic model
#'   discovery, `NULL` uses the required default `"r1i1p1f1"`.
#' @param grid Optional single CMIP6 grid label.
#' @param table Optional CMIP6 table selection. `NULL` automatically maps each
#'   transform input to its native table (including `snd` to `LImon`); an unnamed
#'   scalar pins every variable to one table; a named character vector
#'   overrides individual variables.
#' @param activity CMIP6 activity ID.
#' @param index_nodes Ordered ESGF index nodes used for failover.
#' @param data_node Optional ESGF data-node filter.
#' @export
# shift_cmip6 {{{
shift_cmip6 <- function(
    model = 3L,
    scenarios,
    member = NULL,
    grid = NULL,
    frequency = NULL,
    table = NULL,
    activity = "ScenarioMIP",
    index_nodes = NULL,
    data_node = NULL,
    filters = list(),
    common = TRUE
) {
    # Numeric model input is a bounded automatic selection request. Internally
    # it remains distinct from explicit model IDs so persistence and discovery
    # do not confuse a count with a CMIP6 source identifier.
    n_models <- if (is.numeric(model)) {
        checkmate::assert_count(model, positive = TRUE)
        as.integer(model)
    } else {
        NULL
    }
    if (is.numeric(model)) {
        model <- NULL
    }
    if (!is.null(frequency)) {
        frequency <- shift_spec__cmip6_frequency_spec(frequency)
    }
    table <- shift_spec__cmip6_table_spec(table)
    checkmate::assert_character(
        index_nodes,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )

    if (is.null(index_nodes)) {
        index_nodes <- unname(INDEX_NODES[c(
            "DKRZ",
            "CEDA",
            "ORNL",
            "LLNL",
            "NCI",
            "IPSL",
            "LIU"
        )])
    }
    # Normalize before de-duplication because the legacy LLNL endpoint resolves
    # to the same operational ORNL bridge and must not create a second attempt.
    index_nodes <- unique(vapply(
        index_nodes,
        query__normalize_node,
        character(1L)
    ))
    ShiftCmip6Spec(
        model = model,
        n_models = n_models,
        scenarios = scenarios,
        member = member,
        grid = grid,
        frequency = frequency,
        table = table,
        activity = activity,
        index_nodes = index_nodes,
        data_node = data_node,
        filters = filters,
        common = common
    )
}
# }}}

# Resolve the exact CMIP6 frequency of every source variable from either an
# explicit climate override or the selected weather method's role contract.
# shift_spec__transform_cmip6_frequencies {{{
shift_spec__transform_cmip6_frequencies <- function(
    transform,
    variables,
    frequency = NULL
) {
    variables <- unique(as.character(variables))
    if (!is.null(frequency)) {
        return(shift_spec__cmip6_variable_frequencies(variables, frequency))
    }
    recipe <- transform__recipe(transform)
    declared <- morpher__recipe_required_frequency(recipe)
    if (is.null(declared) || !length(declared)) {
        cli::cli_abort(
            "Weather transformation {.val {transform@method}} does not declare a CMIP6 source frequency."
        )
    }
    if (is.null(names(declared))) {
        return(stats::setNames(
            rep(
                as.character(declared[[1L]]),
                length(variables)
            ),
            variables
        ))
    }
    output <- stats::setNames(rep(NA_character_, length(variables)), variables)
    shared <- unique(as.character(unname(declared)))
    optional <- transform@optional_variable_frequencies[["model_future"]]
    for (variable in variables) {
        value <- declared[[variable]]
        if (is.null(value) && !is.null(optional)) {
            value <- optional[[variable]]
        }
        if (is.null(value) && length(shared) == 1L) {
            value <- shared
        }
        if (is.null(value) || !length(value)) {
            cli::cli_abort(
                "Cannot infer a CMIP6 frequency for {.val {variable}} in weather transformation {.val {transform@method}}."
            )
        }
        output[[variable]] <- as.character(value[[1L]])
    }
    output
}
# }}}

# Translate one complete CMIP6 climate specification into the lower-level
# request consumed by the staged workflow and ESGF collector.
# shift_spec__request_from_cmip6 {{{
shift_spec__request_from_cmip6 <- function(climate, periods, transform) {
    recipe <- transform__recipe(transform)
    variables <- morpher__input_variables(recipe)
    frequencies <- shift_spec__transform_cmip6_frequencies(
        transform,
        variables,
        climate@frequency
    )
    tables <- shift_spec__cmip6_variable_tables(
        variables,
        frequencies,
        climate@table
    )
    request <- shift_cmip6_scenario(
        source = climate@model,
        scenario = climate@scenarios,
        member = climate@member,
        years = periods$year,
        variables = variables,
        frequency = frequencies,
        activity = climate@activity,
        table_id = unique(unname(tables)),
        grid_label = climate@grid,
        data_node = climate@data_node,
        index_node = climate@index_nodes[[1L]],
        filters = climate@filters,
        # Dataset metadata constrains the remote search, while File metadata is
        # completed from DRS filenames before records enter the store.
        options = list(time_filter_method = "auto")
    )
    request_meta <- request@meta
    request_meta$time <- shift_spec__method_time_window(
        periods,
        recipe
    )
    request@meta <- request_meta
    request
}
# }}}

#' @rdname shift_api
#' @param allow_partial Whether a task-level run may complete with missing cases.
#' @param download Source-data policy in [shift_control()] (`"auto"`,
#'   `"always"`, or `"never"`), or a named download-stage option list in
#'   [shift_plan()].
#' @param extraction_method Grid extraction method.
#' @param output_layout Output directory layout.
#' @export
# shift_control {{{
shift_control <- function(
    strict = TRUE,
    allow_partial = FALSE,
    download = c("auto", "always", "never"),
    resume = TRUE,
    overwrite = FALSE,
    refresh = FALSE,
    extraction_method = "nearest",
    output_layout = c("nested", "flat")
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(allow_partial)
    download <- match.arg(download)
    checkmate::assert_flag(resume)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(refresh)
    extraction_method <- match.arg(extraction_method, ESG_GRID_METHOD_CHOICES)
    output_layout <- match.arg(output_layout)

    ShiftControl(
        strict = strict,
        allow_partial = allow_partial,
        download = download,
        resume = resume,
        overwrite = overwrite,
        refresh = refresh,
        extraction_method = extraction_method,
        output_layout = output_layout
    )
}
# }}}

#' @rdname shift_api
#' @param scenario CMIP6 scenario experiment IDs, for example
#'   `"ssp126"` or `"ssp585"`.
#' @param member CMIP6 variant label, for example `"r1i1p1f1"`.
#' @param years Optional years used to constrain the future request time window.
#' @param activity CMIP6 activity ID. `shift_cmip6_scenario()` defaults to
#'   `"ScenarioMIP"` and `shift_reference_historical()` defaults to `"CMIP"`.
#' @param table_id One or more CMIP6 table IDs. If `NULL`, the native table is
#'   inferred for each variable's `frequency`.
#' @param grid_label Optional CMIP6 grid label.
#' @param data_node Optional ESGF data node filter.
#' @param index_node Optional ESGF index node.
#' @export
# shift_cmip6_scenario {{{
shift_cmip6_scenario <- function(
    source,
    scenario,
    member = NULL,
    years = NULL,
    variables = "recommended",
    frequency = "mon",
    activity = "ScenarioMIP",
    table_id = NULL,
    grid_label = NULL,
    data_node = NULL,
    index_node = NULL,
    filters = list(),
    options = list()
) {
    checkmate::assert_character(source, any.missing = FALSE, min.len = 1L)
    checkmate::assert_character(scenario, any.missing = FALSE, min.len = 1L)
    checkmate::assert_character(
        member,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    variables <- shift_spec__variables_value(variables)
    frequencies <- shift_spec__cmip6_variable_frequencies(variables, frequency)
    checkmate::assert_string(activity, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_character(
        table_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_string(grid_label, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(index_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")
    checkmate::assert_list(options, names = "unique")

    time <- if (is.null(years)) {
        NULL
    } else {
        shift_spec__time_window(range(shift_spec__years_value(years)))
    }
    if (is.null(table_id)) {
        table_id <- unique(unname(shift_spec__cmip6_variable_tables(
            variables,
            frequencies
        )))
    }
    defaults <- compact_list(list(
        activity_id = activity,
        table_id = table_id,
        grid_label = grid_label,
        data_node = data_node
    ))
    options <- utils::modifyList(
        compact_list(list(index_node = index_node)),
        options
    )

    shift_request(
        provider = "esgf",
        project = "CMIP6",
        source = source,
        experiment = scenario,
        variant = member,
        variables = variables,
        frequency = frequencies,
        time = time,
        filters = utils::modifyList(defaults, filters),
        options = options
    )
}
# }}}

# Validate a transform-specific frequency contract before a task writes store state
# or attempts remote CMIP6 discovery.
# shift_spec__validate_transform_frequency {{{
shift_spec__validate_transform_frequency <- function(transform, frequency) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort("`transform` must be a {.cls WeatherTransformSpec}.")
    }
    recipe <- transform__recipe(transform)
    required <- morpher__recipe_required_frequency(recipe)
    if (is.null(required)) {
        return(invisible(TRUE))
    }
    variables <- morpher__input_variables(recipe)
    actual <- tryCatch(
        shift_spec__cmip6_variable_frequencies(variables, frequency),
        error = identity
    )
    required <- shift_spec__cmip6_variable_frequencies(variables, required)
    if (
        inherits(actual, "error") ||
            !identical(
                unname(actual[names(required)]),
                unname(required)
            )
    ) {
        shown <- paste(unique(as.character(frequency)), collapse = ", ")
        if (!nzchar(shown)) {
            shown <- "<missing>"
        }
        required_label <- paste(
            paste(names(required), required, sep = "="),
            collapse = ", "
        )
        cli::cli_abort(c(
            "Weather transformation {.val {transform@method}} requires CMIP frequencies {.val {required_label}}.",
            "x" = "The climate request uses {.val {shown}}.",
            "i" = "Set a variable-specific {.arg frequency} vector in the climate specification."
        ))
    }
    invisible(TRUE)
}
# }}}

# Reject a single-year case before store or network work when the selected
# recipe promises an explicitly addressable multi-year result.
# shift_spec__validate_transform_periods {{{
shift_spec__validate_transform_periods <- function(transform, periods) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort("`transform` must be a {.cls WeatherTransformSpec}.")
    }
    recipe <- transform__recipe(transform)
    specification <- morpher__recipe_spec(recipe)
    if (
        is.null(specification) ||
            !identical(specification@output_type, "multi_year")
    ) {
        return(invisible(TRUE))
    }
    counts <- table(as.character(periods[["period"]]))
    incomplete <- names(counts)[counts < 2L]
    if (length(incomplete)) {
        cli::cli_abort(c(
            "Weather transformation {.val {transform@method}} requires at least two weather years in every period.",
            "x" = "Period(s) with fewer than two years: {.val {incomplete}}."
        ))
    }
    invisible(TRUE)
}
# }}}

#' @rdname shift_api
#' @param request A [shift_request()] object, commonly from
#'   `shift_cmip6_scenario()`.
#' @param morph Named morph-stage options. Stage option lists are validated and
#'   cannot override task-level controls or the transform/reference inputs.
#' @export
# shift_plan {{{
shift_plan <- function(
    request,
    site,
    periods,
    store,
    transform,
    reference = NULL,
    observed_reference = NULL,
    control = shift_control(),
    collect = list(),
    download = list(),
    extract = list(),
    morph = list(),
    epw = list()
) {
    if (!S7::S7_inherits(request, ShiftRequest)) {
        cli::cli_abort(
            "`request` must be a {.cls ShiftRequest}, usually from {.fn shift_request} or {.fn shift_cmip6_scenario}."
        )
    }
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("`site` must be a {.cls ShiftSite}.")
    }
    transform__validate_execution_inputs(
        transform,
        reference,
        observed_reference
    )
    if (!S7::S7_inherits(control, ShiftControl)) {
        cli::cli_abort("`control` must be created by {.fn shift_control}.")
    }
    shift_spec__validate_transform_frequency(transform, request@meta$frequency)
    periods <- shift_spec__periods_from_input(periods)
    shift_spec__validate_transform_periods(transform, periods)
    store_path <- shift_path__store_path_value(store)
    if (shift_spec__is_epw_object(site@epw)) {
        # Object-backed inputs may originate from unsaved external state or a
        # temporary conversion. Persist their exact snapshot before the run is
        # registered so cross-session resume never depends on tempdir().
        site@epw <- epw_file_coerce(
            site@epw,
            dir = file.path(store_path, "sources", "epw-input")
        )
    }
    collect <- shift_spec__validate_stage_options(
        collect,
        "collect",
        c("fields", "all", "limit", "label")
    )
    download <- shift_spec__validate_stage_options(
        download,
        "download",
        c(
            "downloader",
            "background",
            "session_label",
            "replica",
            "service",
            "probe",
            "probe_concurrency",
            "probe_cache_seconds",
            "strategy",
            "mode"
        )
    )
    extract <- shift_spec__validate_stage_options(
        extract,
        "extract",
        c("variables", "time", "filters", "fallback")
    )
    morph <- shift_spec__validate_stage_options(morph, "morph", "by")
    epw <- shift_spec__validate_stage_options(
        epw,
        "epw",
        c("dir", "separate", "export_dir")
    )

    shift_stage__new(
        ShiftPlan,
        "plan",
        store_path = store_path,
        meta = list(
            request = request,
            site = site,
            periods = periods,
            transform = transform,
            recipe = transform__recipe(transform),
            reference = reference,
            observed_reference = observed_reference,
            control = control,
            collect = collect,
            download = download,
            extract = extract,
            morph = morph,
            epw = epw,
            expected_cases = shift_spec__expected_cases(request, periods)
        )
    )
}
# }}}

#' @rdname shift_api
#' @param climate A complete future-climate specification from [shift_cmip6()].
#' @param transform A reusable specification from [monthly_transform()],
#'   [daily_transform()], or [hourly_transform()].
#' @param sites A [shift_site()] object or a non-empty list of them, each with
#'   a baseline EPW. The constructor derives omitted coordinates and labels
#'   from the EPW header. Time zone and elevation remain those of the baseline.
#'   Always returns a `ShiftBatch`, ordered by site ID, with separate output
#'   and store directories per location, method, and model. Candidate discovery
#'   and bounded native reads are shared; each child owns its persisted outputs.
#'   Use declarative
#'   historical/reanalysis references. For previously extracted site-specific
#'   references, use [shift_plan()] with the matching site and store.
#' @param methods One or more unambiguous method keys from
#'   [weather_transforms()]. This high-level form creates a `ShiftBatch` across
#'   every selected method and model.
#' @param calibration Alias for `observed_reference` in high-level multi-method
#'   workflows. It is routed only to methods that accept observational input.
#' @param control Workflow controls from [shift_control()].
#' @param dry_run If `TRUE`, discover eligible datasets and return a planned
#'   batch without extracting climate values or generating EPWs.
#' @export
# shift_epw_future {{{
shift_epw_future <- function(
    sites,
    climate,
    periods,
    transform = NULL,
    dir,
    reference = NULL,
    observed_reference = NULL,
    control = shift_control(),
    ui = shift_ui(),
    store = NULL,
    dry_run = FALSE,
    background = FALSE,
    methods = NULL,
    calibration = NULL
) {
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(dry_run)
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(climate, ShiftCmip6Spec)) {
        cli::cli_abort(
            "`climate` must be a complete {.cls ShiftCmip6Spec} created by {.fn shift_cmip6}."
        )
    }
    if (!S7::S7_inherits(control, ShiftControl)) {
        cli::cli_abort("`control` must be created by {.fn shift_control}.")
    }
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (isTRUE(dry_run) && isTRUE(background)) {
        cli::cli_abort(
            "`dry_run = TRUE` cannot be combined with `background = TRUE`."
        )
    }
    if (!is.null(observed_reference) && !is.null(calibration)) {
        cli::cli_abort(
            "Supply either `observed_reference` or `calibration`, not both."
        )
    }
    calibration <- shift_stage__coalesce(calibration, observed_reference)
    transforms <- shift_batch__transforms(
        methods = methods,
        transform = transform
    )
    locations <- shift_batch__sites(sites)
    shift_batch__future_epw(
        sites = locations,
        climate = climate,
        periods = periods,
        transforms = transforms,
        dir = dir,
        reference = reference,
        calibration = calibration,
        control = control,
        ui = ui,
        store = store,
        dry_run = dry_run,
        background = background
    )
}
# }}}

# vim: fdm=marker :
