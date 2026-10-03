#' @include shift-stage.R
NULL

# Construct and serialize workflow intent without running source reads or jobs.

shift_periods_time <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    years <- as.integer(periods$year)
    years <- years[!is.na(years)]
    if (!length(years)) {
        cli::cli_abort(
            "`periods` must contain at least one non-missing `year`."
        )
    }
    c(
        sprintf("%d-01-01T00:00:00Z", min(years)),
        sprintf("%d-12-31T23:59:59Z", max(years))
    )
}

# Expand a requested period by the method's declared temporal support while
# preserving the original years as the case and coverage contract.
shift__method_time_window <- function(periods, recipe) {
    window <- as.POSIXct(
        shift_periods_time(periods),
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
    )
    padding <- morpher__recipe_time_padding_seconds(recipe)
    window <- window + c(-padding, padding)
    format(window, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

shift_time_window <- function(time) {
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

# Parse user-facing year inputs used by workflow plans and presets.
shift__years_value <- function(value, arg = "years") {
    if (is.numeric(value) && !inherits(value, c("Date", "POSIXt"))) {
        checkmate::assert_integerish(value, any.missing = FALSE, min.len = 1L)
        return(as.integer(value))
    }
    if (is.character(value)) {
        pieces <- trimws(unlist(
            strsplit(value, ",", fixed = TRUE),
            use.names = FALSE
        ))
        pieces <- pieces[nzchar(pieces)]
        years <- integer()
        for (piece in pieces) {
            if (grepl(":", piece, fixed = TRUE)) {
                bounds <- suppressWarnings(as.integer(trimws(strsplit(
                    piece,
                    ":",
                    fixed = TRUE
                )[[1L]])))
                if (length(bounds) != 2L || any(is.na(bounds))) {
                    cli::cli_abort(
                        "`{arg}` contains an invalid year range: {.val {piece}}."
                    )
                }
                years <- c(years, seq.int(min(bounds), max(bounds)))
            } else {
                year <- suppressWarnings(as.integer(piece))
                if (length(year) != 1L || is.na(year)) {
                    cli::cli_abort(
                        "`{arg}` contains an invalid year: {.val {piece}}."
                    )
                }
                years <- c(years, year)
            }
        }
        return(unique(years))
    }
    cli::cli_abort("`{arg}` must be numeric years or character year ranges.")
}

# Normalize period inputs so individual target years, explicit period tables,
# and named multi-year windows all reach the same canonical two-column form.
shift__periods_from_input <- function(periods, arg = "periods") {
    if (is.data.frame(periods)) {
        checkmate::assert_names(
            names(periods),
            must.include = c("period", "year")
        )
        return(data.table::as.data.table(periods))
    }
    if (is.numeric(periods) && !inherits(periods, c("Date", "POSIXt"))) {
        years <- shift__years_value(periods, arg)
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
        shift__years_value(
            periods[[i]],
            sprintf("%s$%s", arg, names(periods)[[i]])
        )
    })
    do.call(epw_morph_periods, stats::setNames(values, names(periods)))
}

# Build a one-period table from the common years + period_name shorthand.
shift__periods_from_years <- function(years, period = "future", arg = "years") {
    checkmate::assert_string(period, min.chars = 1L)
    years <- shift__years_value(years, arg = arg)
    do.call(epw_morph_periods, stats::setNames(list(years), period))
}

# Resolve recipe strings early so later workflow stages can rely on a recipe
# object and its required variable set.
shift__recipe_value <- function(recipe) {
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

# Let high-level APIs accept named variable sets while leaving explicit CMIP
# variable IDs untouched.
shift__variables_value <- function(variables, recipe = NULL) {
    if (is.null(variables)) {
        return(epw_morph_variables(shift_coalesce(recipe, "recommended")))
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

# Store paths are normalized before planning so plans are portable and printable
# even when execution is deferred.
shift__store_path_value <- function(store, create = FALSE) {
    checkmate::assert_flag(create)
    if (inherits(store, "EsgStore")) {
        return(normalizePath(store$path, winslash = "/", mustWork = FALSE))
    }
    checkmate::assert_string(store, min.chars = 1L)
    if (isTRUE(create) && !dir.exists(store)) {
        dir.create(store, recursive = TRUE, showWarnings = FALSE)
    }
    normalizePath(store, winslash = "/", mustWork = FALSE)
}

# Drop NULL values from named lists before forwarding them to stage functions.
shift__compact_list <- function(x) {
    x[vapply(x, Negate(is.null), logical(1L))]
}

# Keep only arguments accepted by the target workflow stage.
shift__list_subset <- function(x, allowed) {
    x[intersect(names(x), allowed)]
}

# Validate middle-layer stage options before a plan is created so misspellings
# and attempts to override workflow-wide policies cannot be silently ignored.
shift__validate_stage_options <- function(x, stage, allowed) {
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

# Build the immutable user case matrix before member and grid auto-selection;
# unresolved dimensions remain explicit missing values until the resolver pins
# them for the persisted run.
shift__expected_cases <- function(request, periods) {
    request_meta <- request@meta
    sources <- shift_coalesce(
        request_meta$source,
        request_meta$filters$source_id
    )
    experiments <- shift_coalesce(
        request_meta$experiment,
        request_meta$filters$experiment_id
    )
    members <- shift_coalesce(
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

# Record the durable baseline EPW identity used for run hashing and resume.
shift__epw_identity <- function(epw) {
    if (shift_is_epw_path(epw)) {
        path <- normalizePath(path.expand(epw), winslash = "/", mustWork = TRUE)
        return(list(
            path = path,
            checksum = store_hash_file(path, "sha256"),
            checksum_type = "sha256"
        ))
    }
    if (shift_is_epw_object(epw)) {
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

# Choose CMIP table defaults that match the most common atmospheric frequencies.
shift__cmip6_table_id <- function(frequency) {
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

# Validate scalar and variable-specific CMIP6 frequency specifications without
# discarding names that are needed after a broad multi-frequency ESGF query.
shift__cmip6_frequency_spec <- function(frequency, variables = NULL) {
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

# Expand one scalar CMIP6 frequency or retain an explicit variable mapping so
# downstream table selection and File coverage use the same source semantics.
shift__cmip6_variable_frequencies <- function(variables, frequency) {
    variables <- unique(as.character(variables))
    checkmate::assert_character(
        variables,
        any.missing = FALSE,
        min.len = 1L
    )
    frequency <- shift__cmip6_frequency_spec(frequency, variables)
    if (is.null(names(frequency))) {
        return(stats::setNames(
            rep(frequency[[1L]], length(variables)),
            variables
        ))
    }
    frequency
}

# Validate the two supported table-selection forms. An unnamed scalar pins all
# variables to one table, while a fully named vector overrides only the named
# variables and leaves the remainder on their automatic tables.
shift__cmip6_table_spec <- function(table, null.ok = TRUE) {
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

# Resolve each requested source variable to its CMIP6 table. Snow depth is a
# land-state variable in LImon; all other monthly inputs retain the atmospheric
# Amon default unless the caller pins or overrides them explicitly.
shift__cmip6_variable_tables <- function(variables, frequency, table = NULL) {
    variables <- unique(as.character(variables))
    checkmate::assert_character(variables, any.missing = FALSE, min.len = 1L)
    table <- shift__cmip6_table_spec(table)
    frequencies <- shift__cmip6_variable_frequencies(variables, frequency)
    defaults <- vapply(
        frequencies,
        function(value) {
            shift_coalesce(shift__cmip6_table_id(value), NA_character_)
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

# Interpret one direct request table as a pin, while treating a multi-table
# query filter as discovery breadth whose variable mapping must be inferred.
shift__cmip6_request_table_spec <- function(table_id) {
    if (is.null(table_id) || length(table_id) != 1L) {
        return(NULL)
    }
    table_id
}

shift_is_epw_object <- function(x) {
    inherits(x, "EpwFile") || epw_file_is_external(x)
}

shift_is_epw_path <- function(x) {
    is.character(x) &&
        length(x) == 1L &&
        identical(tolower(tools::file_ext(x)), "epw")
}

shift_location_value <- function(location, names) {
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

# Read only the LOCATION header for path-backed site defaults; weather data
# remain unopened until extraction or generation actually needs them.
shift__epw_location <- function(epw) {
    if (is.null(epw)) {
        return(NULL)
    }
    epw_obj <- if (shift_is_epw_path(epw)) {
        if (!file.exists(epw)) {
            cli::cli_abort("EPW file does not exist: {.path {epw}}.")
        }
        return(epw_file_location(readLines(epw, n = 1L, warn = FALSE)))
    } else if (shift_is_epw_object(epw)) {
        epw_file_coerce(epw)
    } else {
        cli::cli_abort(
            "`epw` must be an EPW file path or an object inheriting from {.cls Epw} or {.cls EpwFile}."
        )
    }
    epw_obj$location()
}

shift_site_default_id <- function(epw, location) {
    if (shift_is_epw_path(epw)) {
        return(tools::file_path_sans_ext(basename(epw)))
    }
    id <- shift_location_value(location, c("wmo_number", "city", "location"))
    if (is.null(id)) {
        return("site")
    }
    as.character(id)
}

shift_resolve_epw <- function(x) {
    if (S7::S7_inherits(x, ShiftSite)) {
        x <- x@epw
    }
    if (is.null(x)) {
        cli::cli_abort("A baseline EPW file is required.")
    }
    if (is.character(x) && length(x) == 1L) {
        return(epw_file_read(x))
    }
    if (shift_is_epw_object(x)) {
        return(epw_file_coerce(x))
    }
    cli::cli_abort(
        "A baseline EPW must be a file path or an object inheriting from {.cls Epw} or {.cls EpwFile}."
    )
}

# constructors ---------------------------------------------------------------

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
        time <- shift_time_window(time)
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

    shift_stage_new(ShiftRequest, "request", meta = meta)
}

#' @rdname shift_api
#' @export
shift_site <- function(
    id = NULL,
    lon = NULL,
    lat = NULL,
    label = NULL,
    epw = NULL,
    metadata = list()
) {
    if (is.null(epw) && (shift_is_epw_path(id) || shift_is_epw_object(id))) {
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
    location <- if (needs_location) shift__epw_location(epw) else NULL
    if (is.null(lon)) {
        lon <- shift_location_value(location, c("longitude", "lon"))
    }
    if (is.null(lat)) {
        lat <- shift_location_value(location, c("latitude", "lat"))
    }
    if (is.null(id)) {
        id <- shift_site_default_id(epw, location)
    }
    if (is.null(label)) {
        label <- shift_location_value(location, c("city", "location"))
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
        diagnostics = shift_diagnostics_empty(),
        id = id,
        lon = lon,
        lat = lat,
        label = label,
        epw = epw,
        metadata = metadata
    )
}

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
    checkmate::assert_flag(common)
    # Numeric model input is a bounded automatic selection request. Internally
    # it remains distinct from explicit model IDs so persistence and discovery
    # do not confuse a count with a CMIP6 source identifier.
    n_models <- if (is.numeric(model)) {
        checkmate::assert_count(model, positive = TRUE)
        as.integer(model)
    } else {
        checkmate::assert_character(
            model,
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE,
            null.ok = TRUE
        )
        NULL
    }
    if (is.numeric(model)) {
        model <- NULL
    }
    checkmate::assert_character(
        scenarios,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_character(
        member,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_string(grid, min.chars = 1L, null.ok = TRUE)
    if (!is.null(frequency)) {
        frequency <- shift__cmip6_frequency_spec(frequency)
    }
    table <- shift__cmip6_table_spec(table)
    checkmate::assert_string(activity, min.chars = 1L)
    checkmate::assert_character(
        index_nodes,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")

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

# Resolve the exact CMIP6 frequency of every source variable from either an
# explicit climate override or the selected weather method's role contract.
shift__transform_cmip6_frequencies <- function(
    transform,
    variables,
    frequency = NULL
) {
    variables <- unique(as.character(variables))
    if (!is.null(frequency)) {
        return(shift__cmip6_variable_frequencies(variables, frequency))
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

# Translate one complete CMIP6 climate specification into the lower-level
# request consumed by the staged workflow and ESGF collector.
shift__request_from_cmip6 <- function(climate, periods, transform) {
    recipe <- transform__recipe(transform)
    variables <- morpher__input_variables(recipe)
    frequencies <- shift__transform_cmip6_frequencies(
        transform,
        variables,
        climate@frequency
    )
    tables <- shift__cmip6_variable_tables(
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
    request_meta$time <- shift__method_time_window(
        periods,
        recipe
    )
    request@meta <- request_meta
    request
}

#' @rdname shift_api
#' @param allow_partial Whether a task-level run may complete with missing cases.
#' @param download Source-data policy in [shift_control()] (`"auto"`,
#'   `"always"`, or `"never"`), or a named download-stage option list in
#'   [shift_plan()].
#' @param extraction_method Grid extraction method.
#' @param output_layout Output directory layout.
#' @export
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
    variables <- shift__variables_value(variables)
    frequencies <- shift__cmip6_variable_frequencies(variables, frequency)
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
        shift_time_window(range(shift__years_value(years)))
    }
    if (is.null(table_id)) {
        table_id <- unique(unname(shift__cmip6_variable_tables(
            variables,
            frequencies
        )))
    }
    defaults <- shift__compact_list(list(
        activity_id = activity,
        table_id = table_id,
        grid_label = grid_label,
        data_node = data_node
    ))
    options <- utils::modifyList(
        shift__compact_list(list(index_node = index_node)),
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
    periods <- shift_reference_periods(periods)
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
        shift__periods_from_years(years, period = period, arg = "years"),
        ...
    )
}

# Validate a transform-specific frequency contract before a task writes store state
# or attempts remote CMIP6 discovery.
shift__validate_transform_frequency <- function(transform, frequency) {
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
        shift__cmip6_variable_frequencies(variables, frequency),
        error = identity
    )
    required <- shift__cmip6_variable_frequencies(variables, required)
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

# Reject a single-year case before store or network work when the selected
# recipe promises an explicitly addressable multi-year result.
shift__validate_transform_periods <- function(transform, periods) {
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

#' @rdname shift_api
#' @param request A [shift_request()] object, commonly from
#'   `shift_cmip6_scenario()`.
#' @param morph Named morph-stage options. Stage option lists are validated and
#'   cannot override task-level controls or the transform/reference inputs.
#' @export
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
    shift__validate_transform_frequency(transform, request@meta$frequency)
    periods <- shift__periods_from_input(periods)
    shift__validate_transform_periods(transform, periods)
    store_path <- shift__store_path_value(store, create = FALSE)
    if (shift_is_epw_object(site@epw)) {
        # Object-backed inputs may originate from unsaved external state or a
        # temporary conversion. Persist their exact snapshot before the run is
        # registered so cross-session resume never depends on tempdir().
        site@epw <- epw_file_coerce(
            site@epw,
            dir = file.path(store_path, "sources", "epw-input")
        )
    }
    collect <- shift__validate_stage_options(
        collect,
        "collect",
        c("fields", "all", "limit", "label")
    )
    download <- shift__validate_stage_options(
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
    extract <- shift__validate_stage_options(
        extract,
        "extract",
        c("variables", "time", "filters", "fallback")
    )
    morph <- shift__validate_stage_options(morph, "morph", "by")
    epw <- shift__validate_stage_options(
        epw,
        "epw",
        c("dir", "separate", "export_dir")
    )

    shift_stage_new(
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
            expected_cases = shift__expected_cases(request, periods)
        )
    )
}

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
shift_future_epw <- function(
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
    calibration <- shift_coalesce(calibration, observed_reference)
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
    periods <- shift_reference_periods(periods)
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

shift_reference_periods <- function(periods) {
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    data.table::as.data.table(periods)
}

# Serialize an explicit workflow reference with the role assigned by its
# execution argument. ShiftClimate stages do not otherwise carry enough
# provenance to distinguish model output from observations.
shift__reference_spec_value <- function(reference, role) {
    if (is.null(reference)) {
        return(NULL)
    }
    checkmate::assert_choice(role, SHIFT_REFERENCE_ROLES)
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        if (!identical(role, "observed_reference")) {
            cli::cli_abort(
                "A reanalysis source cannot be persisted as {.val {role}}."
            )
        }
        return(reanalysis__spec_value(reference))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return(list(
            mode = "plan",
            role = role,
            plan_id = shift_ids(reference)$plan_id,
            periods = split(
                as.integer(reference@meta$periods$year),
                reference@meta$periods$period
            )
        ))
    }
    if (!S7::S7_inherits(reference, ShiftReferenceSpec)) {
        cli::cli_abort("Cannot persist an unsupported shift reference object.")
    }
    if (!identical(reference@role, role)) {
        cli::cli_abort(
            "Cannot persist reference role {.val {reference@role}} as {.val {role}}."
        )
    }
    list(
        mode = reference@mode,
        role = reference@role,
        plan_id = reference@plan_id,
        periods = split(
            as.integer(reference@periods$year),
            reference@periods$period
        ),
        experiment = reference@experiment,
        activity = reference@activity,
        match = reference@match,
        filters = reference@filters,
        options = reference@options,
        collect = reference@collect,
        extract = reference@extract
    )
}

# Rebuild only the reference mode that was serialized; a missing value remains
# missing and is never converted into a historical reference.
shift__reference_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (is.null(spec$role)) {
        cli::cli_abort(
            "Persisted reference is missing its semantic input role."
        )
    }
    if (identical(spec$mode, "reanalysis")) {
        if (!identical(as.character(spec$role), "observed_reference")) {
            cli::cli_abort(
                "Persisted reanalysis input has an invalid semantic role."
            )
        }
        return(reanalysis__from_spec(spec))
    }
    periods <- shift__periods_from_input(
        spec$periods,
        arg = "reference$periods"
    )
    if (identical(spec$mode, "plan")) {
        return(shift_reference_plan(
            as.character(spec$plan_id),
            periods,
            role = as.character(spec$role)
        ))
    }
    if (identical(spec$mode, "historical")) {
        if (
            !identical(
                as.character(spec$role),
                "model_historical"
            )
        ) {
            cli::cli_abort(
                "Persisted automatic historical reference has an invalid semantic role."
            )
        }
        return(shift_reference_historical(
            periods = periods,
            experiment = as.character(spec$experiment),
            activity = as.character(spec$activity),
            match = as.character(spec$match),
            filters = shift_coalesce(spec$filters, list()),
            options = shift_coalesce(spec$options, list()),
            collect = shift_coalesce(spec$collect, list()),
            extract = shift_coalesce(spec$extract, list())
        ))
    }
    cli::cli_abort("Unsupported persisted reference mode: {.val {spec$mode}}.")
}

# Serialize the complete CMIP6 identity as the sole scientific source of truth;
# the lower-level request is derived from this value when a run is resumed.
shift__climate_spec_value <- function(climate) {
    if (is.null(climate)) {
        return(NULL)
    }
    spec <- list(
        provider = "cmip6",
        model = climate@model,
        n_models = if (is.null(climate@model)) climate@n_models else NULL,
        scenarios = climate@scenarios,
        member = climate@member,
        grid = climate@grid,
        # Preserve variable names across JSON round-trips for mixed-frequency
        # climate specifications.
        frequency = if (!is.null(names(climate@frequency))) {
            as.list(climate@frequency)
        } else {
            climate@frequency
        },
        # JSON objects preserve variable names; named atomic vectors do not
        # when `auto_unbox = TRUE`, so overrides are persisted as a named list.
        table = if (!is.null(names(climate@table))) {
            as.list(climate@table)
        } else {
            climate@table
        },
        activity = climate@activity,
        index_nodes = climate@index_nodes,
        data_node = climate@data_node,
        filters = climate@filters
    )
    # Omit the historical default so existing common-pool task hashes and
    # receipts remain valid. Only a different selection policy changes intent.
    if (!climate@common) {
        spec$common <- climate@common
    }
    spec
}

# Rebuild only explicitly supported climate specifications from persisted task
# intent instead of inferring provider or model fields from request artifacts.
shift__climate_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (!identical(as.character(spec$provider), "cmip6")) {
        cli::cli_abort(
            "Unsupported persisted climate provider: {.val {spec$provider}}."
        )
    }
    # Persisted specifications retain a private count field so plans created by
    # earlier development builds can be resumed through the public `model`
    # argument without reintroducing `n_models` into the user API.
    arguments <- spec[setdiff(names(spec), c("provider", "n_models"))]
    model <- if (!is.null(spec$model)) {
        as.character(unlist(spec$model, use.names = FALSE))
    } else if (!is.null(spec$n_models)) {
        as.integer(unlist(spec$n_models, use.names = FALSE))
    } else {
        NULL
    }
    # Single-bracket assignment preserves an explicit NULL list element;
    # `$<- NULL` would delete it and accidentally restore the default count.
    arguments["model"] <- list(model)
    do.call(shift_cmip6, arguments)
}

# Preserve variable names on request frequency mappings because jsonlite
# serializes named atomic vectors as arrays when automatic unboxing is enabled.
shift__request_spec_value <- function(request) {
    if (is.null(request)) {
        return(NULL)
    }
    out <- request@meta
    if (!is.null(names(out$frequency))) {
        out$frequency <- as.list(out$frequency)
    }
    out
}

# Restore request frequencies without allowing character coercion to discard
# names from a JSON object that represents a variable-specific mapping.
shift__request_frequency_from_spec <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    value <- unlist(value, use.names = TRUE)
    value_names <- names(value)
    value <- as.character(value)
    names(value) <- value_names
    value
}

# Convert a plan into a canonical, JSON-safe task specification. Identical
# resumable intent resolves to the original run ID, while explicit refresh or
# overwrite requests remain distinct executions.
shift__plan_spec <- function(x) {
    meta <- x@meta
    request <- meta$request@meta
    transform <- meta$transform
    control <- meta$control
    climate <- meta$climate
    epw_path <- if (
        is.character(meta$site@epw) && length(meta$site@epw) == 1L
    ) {
        normalizePath(
            path.expand(meta$site@epw),
            winslash = "/",
            mustWork = FALSE
        )
    } else {
        shift_coalesce(meta$epw_identity$path, NULL)
    }
    spec <- list(
        version = 2L,
        task = "future_epw",
        request = if (is.null(climate)) {
            shift__request_spec_value(meta$request)
        } else {
            NULL
        },
        site = list(
            id = meta$site@id,
            lon = meta$site@lon,
            lat = meta$site@lat,
            label = meta$site@label,
            epw = epw_path,
            metadata = meta$site@metadata,
            identity = meta$epw_identity
        ),
        periods = split(as.integer(meta$periods$year), meta$periods$period),
        transform = transform__spec_value(transform),
        reference = shift__reference_spec_value(
            meta$reference,
            role = "model_historical"
        ),
        observed_reference = shift__reference_spec_value(
            meta$observed_reference,
            role = "observed_reference"
        ),
        climate = shift__climate_spec_value(climate),
        control = list(
            strict = control@strict,
            allow_partial = control@allow_partial,
            download = control@download,
            resume = control@resume,
            overwrite = control@overwrite,
            refresh = control@refresh,
            extraction_method = control@extraction_method,
            output_layout = control@output_layout
        ),
        store = x@store_path,
        stages = list(
            collect = meta$collect,
            download = meta$download,
            extract = meta$extract,
            morph = meta$morph,
            epw = meta$epw
        )
    )
    # Only resolved batch children carry shared inputs; ordinary task identity
    # stays independent of batch scheduling.
    spec$stages$shared_inputs <- meta$shared_inputs
    spec
}

# Encode workflow specs with stable key order inherited from the constructor
# lists so identical scientific intent produces the same hash.
shift__spec_json <- function(spec) {
    as.character(jsonlite::toJSON(
        spec,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = 15,
        POSIXt = "ISO8601"
    ))
}

# Convert one site into the JSON-safe identity required by later extraction and
# morph steps. EPW objects are persisted through their backing path only.
shift__site_ref <- function(site) {
    if (is.null(site)) {
        return(NULL)
    }
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("Cannot persist a non-ShiftSite task target.")
    }
    epw <- site@epw
    epw_path <- if (shift_is_epw_path(epw)) {
        normalizePath(path.expand(epw), winslash = "/", mustWork = FALSE)
    } else if (shift_is_epw_object(epw)) {
        epw_file_coerce(epw)$path()
    } else {
        NULL
    }
    list(
        id = site@id,
        lon = site@lon,
        lat = site@lat,
        label = site@label,
        epw = epw_path,
        metadata = site@metadata
    )
}

# Rebuild a persisted site without inferring or replacing a missing EPW path.
shift__site_from_ref <- function(ref) {
    if (is.null(ref)) {
        return(NULL)
    }
    shift_site(
        id = as.character(ref$id),
        lon = as.numeric(ref$lon),
        lat = as.numeric(ref$lat),
        label = if (is.null(ref$label)) NULL else as.character(ref$label),
        epw = if (is.null(ref$epw)) NULL else as.character(ref$epw),
        metadata = shift_coalesce(ref$metadata, list())
    )
}

# Reduce a stage to stable store IDs plus the minimum scientific metadata
# required to continue the normal collect-to-export chain in another session.
shift__stage_ref <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    shift_assert_stage(x)
    base <- list(
        version = 1L,
        class = class(x)[[1L]],
        stage = x@stage,
        store_path = x@store_path,
        ids = x@ids
    )
    meta <- if (S7::S7_inherits(x, ShiftRequest)) {
        x@meta
    } else if (S7::S7_inherits(x, ShiftDatasets)) {
        list(
            request = shift__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            result_path = x@meta$result_path
        )
    } else if (S7::S7_inherits(x, ShiftFiles)) {
        list(
            request = shift__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            file_count = x@meta$file_count,
            variables = x@meta$variables,
            fields = x@meta$fields
        )
    } else if (S7::S7_inherits(x, ShiftDownload)) {
        list(files = shift__stage_ref(x@meta$files))
    } else if (S7::S7_inherits(x, ShiftClimate)) {
        upstream <- shift_coalesce(x@meta$download, x@meta$files)
        list(
            upstream = shift__stage_ref(upstream),
            site = shift__site_ref(x@meta$site),
            periods = split(
                as.integer(x@meta$periods$year),
                x@meta$periods$period
            ),
            variables = x@meta$variables
        )
    } else if (S7::S7_inherits(x, ShiftMorphed)) {
        baseline <- x@meta$baseline
        list(
            climate = shift__stage_ref(x@meta$climate),
            baseline = if (S7::S7_inherits(baseline, ShiftSite)) {
                list(type = "site", value = shift__site_ref(baseline))
            } else if (is.character(baseline) && length(baseline) == 1L) {
                list(
                    type = "path",
                    value = normalizePath(
                        path.expand(baseline),
                        winslash = "/",
                        mustWork = FALSE
                    )
                )
            } else {
                NULL
            },
            transform = transform__spec_value(x@meta$transform),
            reference = shift__reference_spec_value(
                shift_coalesce(
                    x@meta$reference_spec,
                    x@meta$reference
                ),
                role = "model_historical"
            ),
            observed_reference = shift__reference_spec_value(
                shift_coalesce(
                    x@meta$observed_reference_spec,
                    x@meta$observed_reference
                ),
                role = "observed_reference"
            ),
            reference_plan_id = x@meta$reference_plan_id,
            reference_periods = if (is.null(x@meta$reference_periods)) {
                NULL
            } else {
                split(
                    as.integer(x@meta$reference_periods$year),
                    x@meta$reference_periods$period
                )
            },
            observed_plan_id = x@meta$observed_plan_id,
            observed_periods = if (is.null(x@meta$observed_periods)) {
                NULL
            } else {
                split(
                    as.integer(x@meta$observed_periods$year),
                    x@meta$observed_periods$period
                )
            }
        )
    } else if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- data.table::as.data.table(shift_coalesce(
            x@meta$outputs,
            data.table::data.table()
        ))
        exports <- if (all(c("output_id", "export_path") %in% names(outputs))) {
            list(
                output_id = outputs$output_id,
                export_path = outputs$export_path
            )
        } else {
            NULL
        }
        list(
            morphed = shift__stage_ref(x@meta$morphed),
            format = x@meta$format,
            paths = x@meta$paths,
            export_dir = x@meta$export_dir,
            exports = exports
        )
    } else {
        list()
    }
    base$meta <- meta
    base
}

# Reconstruct a lightweight but actionable stage from persisted IDs. Large
# datasets and workflow objects are queried from the store instead of being
# embedded in JSON step rows.
shift__stage_from_ref <- function(ref) {
    if (is.null(ref)) {
        return(NULL)
    }
    stage <- as.character(ref$stage)
    store_path <- if (is.null(ref$store_path)) {
        NULL
    } else {
        as.character(ref$store_path)
    }
    ids <- lapply(shift_coalesce(ref$ids, list()), function(value) {
        unlist(value, use.names = FALSE)
    })
    meta <- shift_coalesce(ref$meta, list())
    if (identical(stage, "request")) {
        return(do.call(shift_request, meta))
    }
    if (identical(stage, "datasets")) {
        request <- shift__stage_from_ref(meta$request)
        return(shift_stage_new(
            ShiftDatasets,
            "datasets",
            store_path = store_path,
            ids = ids,
            meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                result_path = as.character(meta$result_path)
            )
        ))
    }
    if (identical(stage, "files")) {
        request <- shift__stage_from_ref(meta$request)
        return(shift_stage_new(
            ShiftFiles,
            "files",
            store_path = store_path,
            ids = ids,
            meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                file_count = as.integer(meta$file_count),
                variables = as.character(unlist(
                    meta$variables,
                    use.names = FALSE
                )),
                fields = as.character(unlist(meta$fields, use.names = FALSE))
            )
        ))
    }
    if (identical(stage, "download")) {
        files <- shift__stage_from_ref(meta$files)
        return(shift_stage_new(
            ShiftDownload,
            "download",
            store_path = store_path,
            ids = ids,
            meta = list(files = files, session = NULL)
        ))
    }
    if (identical(stage, "climate")) {
        upstream <- shift__stage_from_ref(meta$upstream)
        site <- shift__site_from_ref(meta$site)
        periods <- shift__periods_from_input(meta$periods)
        upstream_name <- if (S7::S7_inherits(upstream, ShiftDownload)) {
            "download"
        } else {
            "files"
        }
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        # Coverage is a computed store view rather than a persisted table. Use
        # the public store boundary so stage restoration stays aligned with the
        # extraction schema.
        coverage <- store$coverage(plan_id = ids$plan_id)
        return(shift_stage_new(
            ShiftClimate,
            "climate",
            store_path = store_path,
            ids = ids,
            meta = c(
                stats::setNames(list(upstream), upstream_name),
                list(
                    site = site,
                    periods = periods,
                    variables = as.character(unlist(
                        meta$variables,
                        use.names = FALSE
                    )),
                    coverage = coverage
                )
            )
        ))
    }
    if (identical(stage, "morphed")) {
        climate <- shift__stage_from_ref(meta$climate)
        transform <- transform__from_spec(meta$transform)
        baseline <- if (is.null(meta$baseline)) {
            shift_target(climate)
        } else if (identical(as.character(meta$baseline$type), "site")) {
            shift__site_from_ref(meta$baseline$value)
        } else {
            as.character(meta$baseline$value)
        }
        return(shift_stage_new(
            ShiftMorphed,
            "morphed",
            store_path = store_path,
            ids = ids,
            meta = list(
                climate = climate,
                baseline = baseline,
                transform = transform,
                recipe = transform__recipe(transform),
                reference = shift__reference_from_spec(meta$reference),
                observed_reference = shift__reference_from_spec(
                    meta$observed_reference
                ),
                reference_plan_id = unlist(
                    meta$reference_plan_id,
                    use.names = FALSE
                ),
                reference_periods = if (is.null(meta$reference_periods)) {
                    NULL
                } else {
                    shift__periods_from_input(meta$reference_periods)
                },
                observed_plan_id = unlist(
                    meta$observed_plan_id,
                    use.names = FALSE
                ),
                observed_periods = if (is.null(meta$observed_periods)) {
                    NULL
                } else {
                    shift__periods_from_input(meta$observed_periods)
                }
            )
        ))
    }
    if (identical(stage, "outputs")) {
        morphed <- shift__stage_from_ref(meta$morphed)
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        outputs <- shift_epw_output_rows_for_cases(store, ids$morph_id)
        if (!is.null(meta$exports)) {
            exports <- data.table::data.table(
                output_id = as.character(unlist(
                    meta$exports$output_id,
                    use.names = FALSE
                )),
                export_path = as.character(unlist(
                    meta$exports$export_path,
                    use.names = FALSE
                ))
            )
            outputs <- merge(
                outputs,
                exports,
                by = "output_id",
                all.x = TRUE,
                sort = FALSE
            )
        }
        return(shift_stage_new(
            ShiftOutputs,
            "outputs",
            store_path = store_path,
            ids = ids,
            meta = list(
                morphed = morphed,
                format = as.character(shift_coalesce(meta$format, "epw")),
                outputs = outputs,
                paths = as.character(unlist(meta$paths, use.names = FALSE)),
                export_dir = if (is.null(meta$export_dir)) {
                    NULL
                } else {
                    as.character(meta$export_dir)
                }
            )
        ))
    }
    cli::cli_abort("Unsupported persisted shift stage: {.val {stage}}.")
}

# Reconstruct a persisted plan for cross-session resume. A baseline EPW object
# without a path cannot be recovered and therefore fails with a targeted error.
shift__plan_from_spec <- function(spec, store = NULL) {
    version <- as.integer(shift_coalesce(spec$version, 1L))
    if (!identical(version, 2L)) {
        cli::cli_abort(c(
            "Persisted future-weather plan uses unsupported schema version {.val {version}}.",
            "i" = "Create a new plan with the weather transform API."
        ))
    }
    site_spec <- spec$site
    if (is.null(site_spec$epw) || !nzchar(as.character(site_spec$epw))) {
        cli::cli_abort(
            "This run cannot be resumed across sessions because its baseline EPW was not persisted as a file path."
        )
    }
    site <- shift_site(
        id = as.character(site_spec$id),
        lon = as.numeric(site_spec$lon),
        lat = as.numeric(site_spec$lat),
        label = if (is.null(site_spec$label)) {
            NULL
        } else {
            as.character(site_spec$label)
        },
        epw = as.character(site_spec$epw),
        metadata = shift_coalesce(site_spec$metadata, list())
    )
    transform <- transform__from_spec(spec$transform)
    reference <- shift__reference_from_spec(spec$reference)
    observed_reference <- shift__reference_from_spec(
        spec$observed_reference
    )
    control <- do.call(shift_control, spec$control)
    climate <- shift__climate_from_spec(spec$climate)
    if (is.null(climate)) {
        request_spec <- spec$request
        request <- do.call(
            shift_request,
            list(
                provider = as.character(request_spec$provider),
                project = if (is.null(request_spec$project)) {
                    NULL
                } else {
                    as.character(request_spec$project)
                },
                source = if (is.null(request_spec$source)) {
                    NULL
                } else {
                    as.character(request_spec$source)
                },
                experiment = if (is.null(request_spec$experiment)) {
                    NULL
                } else {
                    as.character(request_spec$experiment)
                },
                variant = if (is.null(request_spec$variant)) {
                    NULL
                } else {
                    as.character(request_spec$variant)
                },
                variables = if (is.null(request_spec$variables)) {
                    NULL
                } else {
                    as.character(request_spec$variables)
                },
                frequency = shift__request_frequency_from_spec(
                    request_spec$frequency
                ),
                time = request_spec$time,
                filters = shift_coalesce(request_spec$filters, list()),
                options = shift_coalesce(request_spec$options, list())
            )
        )
    } else {
        # The persisted climate spec is authoritative; regenerate request fields
        # so model/scenario/member constraints cannot diverge during resume.
        request <- shift__request_from_cmip6(
            climate,
            shift__periods_from_input(spec$periods),
            transform
        )
    }
    stage <- shift_coalesce(spec$stages, list())
    plan <- shift_plan(
        request = request,
        site = site,
        periods = spec$periods,
        store = shift_coalesce(store, spec$store),
        transform = transform,
        reference = reference,
        observed_reference = observed_reference,
        control = control,
        collect = shift_coalesce(stage$collect, list()),
        download = shift_coalesce(stage$download, list()),
        extract = shift_coalesce(stage$extract, list()),
        morph = shift_coalesce(stage$morph, list()),
        epw = shift_coalesce(stage$epw, list())
    )
    if (!is.null(climate)) {
        plan@meta$climate <- climate
    }
    plan@meta$epw_identity <- site_spec$identity
    plan@meta$shared_inputs <- stage$shared_inputs
    plan
}
