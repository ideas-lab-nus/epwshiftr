# Build the shared Dataset request used by public discovery and batch plans.
# Variable-specific frequency and table checks remain in the local reduction.
availability__request <- function(
    variables,
    frequency,
    experiments,
    source,
    member,
    grid,
    tables,
    activity,
    historical_activity,
    index_node,
    data_node,
    filters
) {
    activities <- unique(c(
        activity,
        if ("historical" %in% experiments) historical_activity
    ))
    filters$table_id <- NULL
    query_filters <- utils::modifyList(
        filters,
        shift__compact_list(list(
            activity_id = activities,
            table_id = if (is.null(tables)) NULL else unique(unname(tables)),
            grid_label = grid,
            data_node = data_node,
            latest = TRUE,
            replica = FALSE,
            fields = AVAILABILITY__DATASET_FIELDS
        ))
    )
    shift_request(
        provider = "esgf",
        project = "CMIP6",
        source = source,
        experiment = experiments,
        variant = member,
        variables = variables,
        frequency = frequency,
        filters = query_filters,
        options = list(index_node = index_node)
    )
}

# Dataset fields retained by the public CMIP6 availability query.
AVAILABILITY__DATASET_FIELDS <- c(
    "id",
    "source_id",
    "experiment_id",
    "variant_label",
    "member_id",
    "frequency",
    "table_id",
    "variable_id",
    "grid_label",
    "data_node",
    "index_node",
    "instance_id",
    "master_id",
    "version",
    "latest",
    "replica",
    "number_of_files",
    "size"
)

# Return one Dataset column as a character vector of the requested row count.
availability__character_column <- function(catalog, name) {
    value <- catalog[[name]]
    if (is.null(value)) {
        return(rep(NA_character_, nrow(catalog)))
    }
    as.character(value)
}

# Fill missing or empty values in the first vector from later alternatives.
availability__coalesce_character <- function(...) {
    values <- list(...)
    if (!length(values)) {
        return(character())
    }
    output <- as.character(values[[1L]])
    for (value in values[-1L]) {
        value <- as.character(value)
        replace <- (is.na(output) | !nzchar(output)) &
            !is.na(value) &
            nzchar(value)
        output[replace] <- value[replace]
    }
    output
}

# Normalize provider Dataset records to the identity fields used by the
# availability reduction and reapply requested filters defensively.
availability__normalize_datasets <- function(
    datasets,
    experiments,
    variables,
    frequency,
    tables = NULL
) {
    checkmate::assert_data_frame(datasets)
    catalog <- data.table::as.data.table(data.table::copy(datasets))
    frequencies <- shift__cmip6_variable_frequencies(variables, frequency)

    catalog[["source_id"]] <- availability__character_column(
        catalog,
        "source_id"
    )
    catalog[["experiment_id"]] <- availability__character_column(
        catalog,
        "experiment_id"
    )
    catalog[["variant_label"]] <- availability__coalesce_character(
        availability__character_column(catalog, "variant_label"),
        availability__character_column(catalog, "member_id")
    )
    catalog[["grid_label"]] <- availability__character_column(
        catalog,
        "grid_label"
    )
    catalog[["frequency"]] <- availability__character_column(
        catalog,
        "frequency"
    )
    catalog[["table_id"]] <- availability__character_column(
        catalog,
        "table_id"
    )
    catalog[["variable_id"]] <- availability__character_column(
        catalog,
        "variable_id"
    )

    identity_fields <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "frequency",
        "table_id",
        "variable_id"
    )
    complete_identity <- Reduce(
        `&`,
        lapply(identity_fields, function(name) {
            !is.na(catalog[[name]]) & nzchar(catalog[[name]])
        })
    )
    wanted_frequency <- unname(frequencies[catalog$variable_id])
    catalog <- catalog[
        complete_identity &
            experiment_id %in% experiments &
            variable_id %in% variables &
            frequency == wanted_frequency
    ]
    if (!is.null(tables) && nrow(catalog)) {
        # An explicit table specification is variable-specific. Compare each
        # row with its variable's resolved table instead of accepting any of
        # the tables used elsewhere in the request.
        selected_tables <- unname(tables[catalog$variable_id])
        catalog <- catalog[catalog$table_id == selected_tables]
    }
    unique(catalog[, identity_fields, with = FALSE])
}

# Choose one table for every variable within a stable model/member/grid
# identity. Coverage across requested experiments is preferred, followed by
# the frequency's conventional table and then a lexical tie-break.
availability__select_tables <- function(
    catalog,
    variables,
    frequency,
    tables = NULL
) {
    if (!is.null(tables)) {
        return(tables)
    }

    frequencies <- shift__cmip6_variable_frequencies(variables, frequency)
    selected <- stats::setNames(
        rep(NA_character_, length(variables)),
        variables
    )
    for (target_variable in variables) {
        data <- catalog[variable_id == target_variable]
        if (!nrow(data)) {
            next
        }
        preferred_table <- shift__cmip6_table_id(
            frequencies[[target_variable]]
        )
        scores <- unique(data[, .(experiment_id, table_id)])[,
            .(coverage = data.table::uniqueN(experiment_id)),
            by = table_id
        ]
        if (is.null(preferred_table)) {
            scores[["preferred"]] <- 1L
        } else {
            scores[["preferred"]] <- as.integer(
                scores$table_id != preferred_table
            )
        }
        data.table::setorderv(
            scores,
            c("coverage", "preferred", "table_id"),
            c(-1L, 1L, 1L),
            na.last = TRUE
        )
        selected[[target_variable]] <- scores$table_id[[1L]]
    }
    selected
}

# Return a typed empty availability table with the public column contract.
availability__empty <- function() {
    data.table::data.table(
        source_id = character(),
        variant_label = character(),
        grid_label = character(),
        frequency = character(),
        frequency_spec = vector("list", 0L),
        table_id = character(),
        table = vector("list", 0L),
        complete = logical(),
        complete_experiments = integer(),
        required_experiments = integer(),
        available_pairs = integer(),
        required_pairs = integer(),
        missing = character(),
        index_node = character()
    )
}

# Reduce variable-specific Dataset records to one row per stable CMIP6 identity.
availability__summarize <- function(
    datasets,
    experiments,
    variables,
    frequency,
    table,
    index_node
) {
    frequencies <- shift__cmip6_variable_frequencies(variables, frequency)
    table <- shift__cmip6_table_spec(table)
    tables <- if (is.null(table)) {
        NULL
    } else {
        shift__cmip6_variable_tables(variables, frequency, table)
    }
    catalog <- availability__normalize_datasets(
        datasets,
        experiments = experiments,
        variables = variables,
        frequency = frequencies,
        tables = tables
    )
    if (!nrow(catalog)) {
        return(availability__empty())
    }

    identity_fields <- c(
        "source_id",
        "variant_label",
        "grid_label"
    )
    identities <- unique(catalog[, identity_fields, with = FALSE])
    required <- data.table::CJ(
        experiment_id = as.character(experiments),
        variable_id = as.character(variables),
        unique = TRUE
    )

    rows <- vector("list", nrow(identities))
    for (identity_index in seq_len(nrow(identities))) {
        identity <- identities[identity_index]
        identity_catalog <- catalog[
            source_id == identity$source_id[[1L]] &
                variant_label == identity$variant_label[[1L]] &
                grid_label == identity$grid_label[[1L]]
        ]
        selected_tables <- availability__select_tables(
            identity_catalog,
            variables = variables,
            frequency = frequency,
            tables = tables
        )
        # A variable must use the same selected table in every experiment;
        # records split across tables cannot be combined into false coverage.
        wanted_tables <- unname(selected_tables[identity_catalog$variable_id])
        observed <- unique(identity_catalog[
            table_id == wanted_tables,
            .(experiment_id, variable_id)
        ])
        observed[, present := TRUE]
        coverage <- observed[required, on = c("experiment_id", "variable_id")]
        coverage[is.na(present), present := FALSE]
        missing_rows <- coverage[present == FALSE]
        experiment_status <- coverage[,
            .(complete = all(present)),
            by = experiment_id
        ]
        display_tables <- sort(unique(unname(selected_tables)))
        display_tables <- display_tables[
            !is.na(display_tables) & nzchar(display_tables)
        ]

        rows[[identity_index]] <- data.table::data.table(
            source_id = identity$source_id[[1L]],
            variant_label = identity$variant_label[[1L]],
            grid_label = identity$grid_label[[1L]],
            frequency = paste(unique(unname(frequencies)), collapse = "+"),
            frequency_spec = list(frequencies),
            table_id = paste(display_tables, collapse = "+"),
            table = list(selected_tables),
            complete = all(coverage$present),
            complete_experiments = sum(experiment_status$complete),
            required_experiments = data.table::uniqueN(
                coverage$experiment_id
            ),
            available_pairs = sum(coverage$present),
            required_pairs = nrow(coverage),
            missing = if (nrow(missing_rows)) {
                paste(
                    sprintf(
                        "%s:%s",
                        missing_rows$experiment_id,
                        missing_rows$variable_id
                    ),
                    collapse = "; "
                )
            } else {
                NA_character_
            }
        )
    }
    summary <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    summary[, index_node := rep(index_node, .N)]
    data.table::setorderv(
        summary,
        c("complete", "source_id", "variant_label", "grid_label"),
        c(-1L, 1L, 1L, 1L),
        na.last = TRUE
    )
    summary
}

# Collect Dataset records through the existing store-native query workflow.
availability__collect <- function(request, store, ui) {
    result <- shift_datasets(
        request,
        all = TRUE,
        limit = FALSE,
        store = store,
        ui = ui
    )
    data.table::as.data.table(result$to_data_table())
}

# Resolve a public index-node name or URL to the endpoint used by EsgQuery.
availability__index_node <- function(index_node) {
    if (is.null(index_node)) {
        index_node <- "DKRZ"
    }

    node_name <- toupper(index_node)
    if (
        !grepl("://", index_node, fixed = TRUE) &&
            node_name %in% names(INDEX_NODES)
    ) {
        # Known names use the package node registry; ORNL and LLNL are then
        # normalized by the query layer to the shared ESGF 1.5 Bridge endpoint.
        index_node <- unname(INDEX_NODES[[node_name]])
    }
    query__normalize_node(index_node)
}

# Attach the chosen future input specification to each public method row.
# Full requirement diagnostics remain internal; rejected rows retain missing reasons.
availability__method_summary <- function(evaluated, index_node) {
    identity <- c("source_id", "variant_label", "grid_label")
    role <- NULL
    details <- evaluated$requirements[
        role == "model_future",
        c(
            identity,
            "transform_key",
            "path_id",
            "experiment_id",
            "variables",
            "frequency_spec",
            "table"
        ),
        with = FALSE
    ]
    data.table::setnames(details, "experiment_id", "scenario")
    result <- merge(
        evaluated$matrix,
        details,
        by = c(identity, "transform_key", "path_id", "scenario"),
        all.x = TRUE,
        sort = FALSE
    )
    data.table::set(
        result,
        j = "index_node",
        value = rep(index_node, nrow(result))
    )
    data.table::setcolorder(
        result,
        c(
            names(evaluated$matrix),
            "variables",
            "frequency_spec",
            "table",
            "index_node"
        )
    )
    data.table::setorderv(result, c(identity, "transform_key", "scenario"))
    result
}

#' Query CMIP6 availability by variables or weather methods
#'
#' Query shared CMIP6 Dataset metadata and identify model/member/grid identities
#' that satisfy requested variables or registered weather-method inputs.
#' ESGF treats multiple variables as OR alternatives; required AND combinations
#' are evaluated locally with `data.table`.
#'
#' @param variables CMIP6 variable IDs that must all be present. Supply these,
#'   `methods`, or `transform`.
#' @param scenarios Future CMIP6 experiment IDs.
#' @param include_historical For variable queries, require the same variables
#'   for the `"historical"` experiment. For method queries, historical inputs
#'   come from the method contract; leave this argument unspecified.
#' @param source Optional CMIP6 source/model IDs. `NULL` discovers all models.
#' @param member Optional CMIP6 variant labels. `NULL` discovers all members.
#' @param grid Optional single CMIP6 grid label.
#' @param frequency For variable queries, a scalar frequency or named vector
#'   assigning a frequency to each variable. Method queries infer frequencies
#'   from their contracts; leave this argument unspecified.
#' @param table For variable queries, an optional scalar or named per-variable
#'   table selection. `NULL` discovers tables. Method queries discover tables
#'   at the frequencies declared by their contracts; leave this unspecified.
#' @param activity Future CMIP6 activity ID.
#' @param historical_activity Historical CMIP6 activity ID.
#' @param index_node ESGF index-node name or URL. `NULL` uses DKRZ;
#'   `"ORNL"` and `"LLNL"` use the ORNL ESGF 1.5 Bridge endpoint.
#' @param data_node Optional ESGF data-node filter.
#' @param filters Additional named ESGF filters. Core availability arguments
#'   take precedence when names overlap.
#' @param store Optional [EsgStore] or store path forwarded to [shift_datasets()].
#' @param ui Optional shift UI configuration forwarded to [shift_datasets()].
#' @param methods Unique weather-method keys, such as `c("qdm", "sobie_curry")`.
#'   Ambiguous keys require an explicit `transform`.
#' @param transform One `WeatherTransformSpec` or a list of them, created by
#'   [monthly_transform()], [daily_transform()], or [hourly_transform()]. Use
#'   this to specify method options, such as the variables adjusted by morphing.
#' @param common For method queries, a logical flag. `FALSE` (default)
#'   selects candidates separately for each method; `TRUE` requires eligibility
#'   for every selected method.
#' @param include_optional_historical For method queries, include historical
#'   model inputs that the method declares optional. Mandatory inputs are always
#'   required regardless of this flag.
#'
#' @return A `data.table`, including when there are no matching identities.
#'   Variable queries return one row per model/member/grid identity, retaining
#'   the existing `complete`, coverage counts, `missing`, `frequency_spec`, and
#'   `table` columns. Method queries return one row per identity/method/scenario:
#'   `catalog_eligible` describes that scenario including required history;
#'   `method_eligible` requires a single input path across all requested scenarios;
#'   `common_eligible` requires all methods; `selected` applies `common`.
#'   `missing` explains rejections. List columns `variables`, `frequency_spec`,
#'   and `table` describe the chosen future input path. `transform_key`
#'   distinguishes method configurations. `period_coverage`, `readability`,
#'   and `quality` remain `"not_checked"`.
#'
#' @details
#' All selected methods share one logical Dataset query for the union of their
#' required variables, frequencies, and experiments. The existing query layer
#' handles pagination and response caching; one logical query may require
#' multiple HTTP requests. No File catalog or NetCDF values are downloaded.
#' Dataset availability does not establish requested-year coverage or scientific
#' quality. Incomplete identities found in the shared catalog remain visible.
#' Entirely absent identities cannot be inferred from an empty catalog.
#' Future and historical model inputs must share a variable combination,
#' matching the requirements of the execution resolver.
#'
#' @examples
#' \dontrun{
#' models <- shift_cmip6_avail(
#'     methods = c("qdm", "sobie_curry"),
#'     scenarios = c("ssp245", "ssp585")
#' )
#' models[selected == TRUE]
#'
#' temperature <- shift_cmip6_avail(
#'     variables = "tas", scenarios = "ssp245", frequency = "day"
#' )
#' temperature[complete == TRUE]
#' }
#'
#' @export
shift_cmip6_avail <- function(
    variables = NULL,
    scenarios = c("ssp245", "ssp585"),
    include_historical = TRUE,
    source = NULL,
    member = NULL,
    grid = NULL,
    frequency = "day",
    table = NULL,
    activity = "ScenarioMIP",
    historical_activity = "CMIP",
    index_node = NULL,
    data_node = NULL,
    filters = list(),
    store = NULL,
    ui = NULL,
    methods = NULL,
    transform = NULL,
    common = FALSE,
    include_optional_historical = FALSE
) {
    checkmate::assert_character(
        scenarios,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_character(
        source,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_character(
        member,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_string(grid, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(activity, min.chars = 1L)
    checkmate::assert_string(historical_activity, min.chars = 1L)
    checkmate::assert_string(index_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")
    index_node <- availability__index_node(index_node)
    method_query <- !is.null(methods) || !is.null(transform)
    if (method_query) {
        if (!is.null(variables)) {
            cli::cli_abort(
                "Supply `variables`, `methods`, or `transform`, not a mixture."
            )
        }
        if (
            !missing(frequency) ||
                !missing(table) ||
                !missing(include_historical)
        ) {
            cli::cli_abort(paste(
                "Method queries derive `frequency`, `table`, and required history",
                "from the transform. Leave these arguments unspecified; use",
                "`include_optional_historical` to include optional history."
            ))
        }
        if (any(!nzchar(scenarios)) || "historical" %in% scenarios) {
            cli::cli_abort("Scenarios must be non-empty future experiment IDs.")
        }
        checkmate::assert_flag(common)
        checkmate::assert_flag(include_optional_historical)
        transforms <- shift_batch__transforms(methods, transform)
        requirements <- eligibility__requirements(
            transforms,
            scenarios,
            include_optional_historical
        )
        # Canonical union order shares cache keys even when methods are reordered.
        query_variables <- sort(unique(requirements$pairs$variable_id))
        query_frequencies <- sort(unique(unlist(
            requirements$pairs$allowed,
            use.names = FALSE
        )))
        experiments <- sort(unique(requirements$pairs$experiment_id))
        tables <- NULL
        source <- sort(source)
        member <- sort(member)
        filters[c(
            "project",
            "source_id",
            "experiment_id",
            "variant_label",
            "member_id",
            "variable_id",
            "frequency",
            "type"
        )] <- NULL
    } else {
        if (!missing(common) || !missing(include_optional_historical)) {
            cli::cli_abort(
                "`common` and `include_optional_historical` require `methods` or `transform`."
            )
        }
        checkmate::assert_character(
            variables,
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE
        )
        checkmate::assert_flag(include_historical)
        frequencies <- shift__cmip6_variable_frequencies(variables, frequency)
        table <- shift__cmip6_table_spec(table)
        tables <- if (is.null(table)) {
            NULL
        } else {
            shift__cmip6_variable_tables(variables, frequency, table)
        }
        query_variables <- variables
        query_frequencies <- unique(unname(frequencies))
        experiments <- unique(c(
            scenarios,
            if (include_historical) "historical"
        ))
    }
    request <- availability__request(
        query_variables,
        query_frequencies,
        experiments,
        source,
        member,
        grid,
        tables,
        activity,
        historical_activity,
        index_node,
        data_node,
        filters
    )
    datasets <- availability__collect(request, store = store, ui = ui)
    if (method_query) {
        catalog <- eligibility__catalog(datasets)
        if (!is.null(source)) {
            catalog <- catalog[source_id %in% source]
        }
        if (!is.null(member)) {
            catalog <- catalog[variant_label %in% member]
        }
        if (!is.null(grid)) {
            catalog <- catalog[grid_label == grid]
        }
        evaluated <- eligibility__evaluate(catalog, requirements, common)
        return(availability__method_summary(evaluated, index_node))
    }
    availability__summarize(
        datasets,
        experiments = experiments,
        variables = variables,
        frequency = frequencies,
        table = table,
        index_node = index_node
    )
}
