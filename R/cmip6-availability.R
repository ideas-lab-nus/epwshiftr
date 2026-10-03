# Apply Dataset filter precedence shared by public and batch discovery.
# Request identity is supplied directly to shift_request() by each caller.
availability__filters <- function(filters, selections) {
    # Core selections have one owner in both discovery entry points.
    filters[c(
        "project",
        "source_id",
        "experiment_id",
        "variant_label",
        "member_id",
        "variable_id",
        "frequency",
        "type",
        "table_id"
    )] <- NULL
    utils::modifyList(
        filters,
        c(
            compact_list(selections),
            list(
                latest = TRUE,
                # Honor an explicit replica filter; keep primary-only discovery
                # as the default when the caller has not selected a policy.
                replica = if ("replica" %in% names(filters)) {
                    filters$replica
                } else {
                    FALSE
                },
                fields = AVAILABILITY__DATASET_FIELDS
            )
        )
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
    frequencies <- shift_spec__cmip6_variable_frequencies(variables, frequency)
    table <- shift_spec__cmip6_table_spec(table)
    tables <- if (is.null(table)) {
        NULL
    } else {
        shift_spec__cmip6_variable_tables(variables, frequency, table)
    }
    # Share the narrow catalog with method discovery. Variable queries require
    # usable partitions; method queries retain them to explain rejections.
    catalog <- eligibility__catalog(datasets)
    catalog <- catalog[
        experiment_id %in% experiments & !is.na(table_id) & nzchar(table_id)
    ]
    requested <- data.table::data.table(
        variable_id = variables,
        frequency = unname(frequencies)
    )
    match_fields <- c("variable_id", "frequency")
    if (!is.null(tables)) {
        data.table::set(requested, j = "table_id", value = unname(tables))
        match_fields <- c(match_fields, "table_id")
    }
    catalog <- catalog[requested, on = match_fields, nomatch = 0L]
    if (!nrow(catalog)) {
        return(availability__empty())
    }

    identity_fields <- c("source_id", "variant_label", "grid_label")
    if (is.null(tables)) {
        # Rank tables across all identities together: experiment coverage,
        # conventional frequency table, then lexical order. A variable must
        # keep the same table across experiments; never stitch tables together.
        scores <- catalog[,
            .(coverage = data.table::uniqueN(experiment_id)),
            by = c(identity_fields, "variable_id", "table_id", "frequency")
        ]
        defaults <- vapply(
            unique(unname(frequencies)),
            function(value) {
                shift_stage__coalesce(
                    shift_spec__cmip6_table_id(value),
                    NA_character_
                )
            },
            character(1L)
        )
        preferred <- unname(defaults[scores$frequency])
        data.table::set(
            scores,
            j = "preferred",
            value = as.integer(
                is.na(preferred) | scores$table_id != preferred
            )
        )
        data.table::setorderv(
            scores,
            c("coverage", "preferred", "table_id"),
            c(-1L, 1L, 1L)
        )
        selected <- unique(scores, by = c(identity_fields, "variable_id"))
        selection_fields <- c(identity_fields, "variable_id", "table_id")
        catalog <- catalog[
            selected[, selection_fields, with = FALSE],
            on = selection_fields,
            nomatch = 0L
        ]
    }
    required <- data.table::CJ(
        experiment_id = as.character(experiments),
        variable_id = as.character(variables),
        unique = TRUE
    )
    frequency_label <- paste(unique(unname(frequencies)), collapse = "+")
    required_experiments <- data.table::uniqueN(required$experiment_id)

    # Each group receives only its own rows, avoiding a full catalog scan for
    # every model/member/grid. The small required grid preserves missing order.
    summary <- catalog[,
        {
            selected_tables <- if (is.null(tables)) {
                stats::setNames(
                    table_id[match(variables, variable_id)],
                    variables
                )
            } else {
                tables
            }
            observed <- unique(.SD[, .(experiment_id, variable_id)])
            missing_rows <- required[
                !observed,
                on = c("experiment_id", "variable_id")
            ]
            display_tables <- sort(unique(unname(selected_tables)))
            display_tables <- display_tables[
                !is.na(display_tables) & nzchar(display_tables)
            ]
            list(
                frequency = frequency_label,
                frequency_spec = list(frequencies),
                table_id = paste(display_tables, collapse = "+"),
                table = list(selected_tables),
                complete = !nrow(missing_rows),
                complete_experiments = required_experiments -
                    data.table::uniqueN(missing_rows$experiment_id),
                required_experiments = required_experiments,
                available_pairs = nrow(observed),
                required_pairs = nrow(required),
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
        },
        by = identity_fields
    ]
    data.table::set(
        summary,
        j = "index_node",
        value = rep(index_node, nrow(summary))
    )
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
        historical <- vapply(
            transforms,
            function(value) {
                "model_historical" %in%
                    names(value@required_inputs) ||
                    (include_optional_historical &&
                        "model_historical" %in% names(value@optional_inputs))
            },
            logical(1L)
        )
        requirements <- eligibility__requirements(
            transforms,
            scenarios,
            historical
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
        frequencies <- shift_spec__cmip6_variable_frequencies(
            variables,
            frequency
        )
        table <- shift_spec__cmip6_table_spec(table)
        tables <- if (is.null(table)) {
            NULL
        } else {
            shift_spec__cmip6_variable_tables(variables, frequency, table)
        }
        query_variables <- variables
        query_frequencies <- unique(unname(frequencies))
        experiments <- unique(c(
            scenarios,
            if (include_historical) "historical"
        ))
    }
    query_filters <- availability__filters(
        filters,
        list(
            activity_id = unique(c(
                activity,
                if ("historical" %in% experiments) historical_activity
            )),
            table_id = if (is.null(tables)) NULL else unique(unname(tables)),
            grid_label = grid,
            data_node = data_node
        )
    )
    request <- shift_request(
        provider = "esgf",
        project = "CMIP6",
        source = source,
        experiment = experiments,
        variant = member,
        variables = query_variables,
        frequency = query_frequencies,
        filters = query_filters,
        options = list(index_node = index_node)
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
        evaluated <- eligibility__summarize(
            eligibility__match(catalog, requirements),
            scenarios,
            common
        )
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
