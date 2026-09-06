# Dataset fields retained by the public CMIP6 availability query.
AVAILABILITY__DATASET_FIELDS <- c(
    "id", "source_id", "experiment_id", "variant_label", "member_id",
    "frequency", "table_id", "variable_id", "grid_label", "data_node",
    "index_node", "instance_id", "master_id", "version", "latest",
    "replica", "number_of_files", "size"
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
            !is.na(value) & nzchar(value)
        output[replace] <- value[replace]
    }
    output
}

# Normalize provider Dataset records to the identity fields used by the
# availability reduction and reapply requested filters defensively.
availability__normalize_datasets <- function(datasets, experiments, variables,
                                             frequency, tables = NULL) {
    checkmate::assert_data_frame(datasets)
    catalog <- data.table::as.data.table(data.table::copy(datasets))
    wanted_frequency <- frequency[[1L]]

    catalog[["source_id"]] <- availability__character_column(
        catalog, "source_id")
    catalog[["experiment_id"]] <- availability__character_column(
        catalog, "experiment_id")
    catalog[["variant_label"]] <- availability__coalesce_character(
        availability__character_column(catalog, "variant_label"),
        availability__character_column(catalog, "member_id")
    )
    catalog[["grid_label"]] <- availability__character_column(
        catalog, "grid_label")
    catalog[["frequency"]] <- availability__character_column(
        catalog, "frequency")
    catalog[["table_id"]] <- availability__character_column(
        catalog, "table_id")
    catalog[["variable_id"]] <- availability__character_column(
        catalog, "variable_id")

    identity_fields <- c(
        "source_id", "experiment_id", "variant_label", "grid_label",
        "frequency", "table_id", "variable_id"
    )
    complete_identity <- Reduce(
        `&`,
        lapply(identity_fields, function(name) {
            !is.na(catalog[[name]]) & nzchar(catalog[[name]])
        })
    )
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
availability__select_tables <- function(catalog, variables, frequency,
                                        tables = NULL) {
    if (!is.null(tables)) {
        return(tables)
    }

    selected <- stats::setNames(rep(NA_character_, length(variables)), variables)
    preferred_table <- shift__cmip6_table_id(frequency)
    for (target_variable in variables) {
        data <- catalog[variable_id == target_variable]
        if (!nrow(data)) {
            next
        }
        scores <- unique(data[, .(experiment_id, table_id)])[
            , .(coverage = data.table::uniqueN(experiment_id)), by = table_id
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
    data.frame(
        source_id = character(),
        variant_label = character(),
        grid_label = character(),
        frequency = character(),
        table_id = character(),
        table = I(vector("list", 0L)),
        complete = logical(),
        complete_experiments = integer(),
        required_experiments = integer(),
        available_pairs = integer(),
        required_pairs = integer(),
        missing = character(),
        index_node = character(),
        stringsAsFactors = FALSE
    )
}

# Reduce variable-specific Dataset records to one row per stable CMIP6 identity.
availability__summarize <- function(datasets, experiments, variables,
                                    frequency, table, index_node) {
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
        frequency = frequency,
        tables = tables
    )
    if (!nrow(catalog)) {
        return(availability__empty())
    }

    identity_fields <- c(
        "source_id", "variant_label", "grid_label", "frequency"
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
                grid_label == identity$grid_label[[1L]] &
                frequency == identity$frequency[[1L]]
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
        experiment_status <- coverage[, .(complete = all(present)),
            by = experiment_id]
        display_tables <- sort(unique(unname(selected_tables)))
        display_tables <- display_tables[
            !is.na(display_tables) & nzchar(display_tables)
        ]

        rows[[identity_index]] <- data.table::data.table(
            source_id = identity$source_id[[1L]],
            variant_label = identity$variant_label[[1L]],
            grid_label = identity$grid_label[[1L]],
            frequency = identity$frequency[[1L]],
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
    as.data.frame(summary, row.names = NULL)
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
    if (!grepl("://", index_node, fixed = TRUE) &&
            node_name %in% names(INDEX_NODES)) {
        # Known names use the package node registry; ORNL and LLNL are then
        # normalized by the query layer to the shared ESGF 1.5 Bridge endpoint.
        index_node <- unname(INDEX_NODES[[node_name]])
    }
    query__normalize_node(index_node)
}

#' Query CMIP6 variable availability
#'
#' Query CMIP6 Dataset metadata and identify model/member/grid identities that
#' contain every requested variable for every requested experiment. ESGF
#' interprets multiple `variable_id` values as OR alternatives; this function
#' applies the required AND reduction locally.
#'
#' @param variables CMIP6 variable IDs that must all be present.
#' @param scenarios Future CMIP6 experiment IDs.
#' @param include_historical Whether the same identity must also contain the
#'   requested variables for the `"historical"` experiment.
#' @param source Optional CMIP6 source/model IDs. `NULL` leaves the source
#'   unconstrained and discovers all matching models.
#' @param member Optional CMIP6 variant labels. `NULL`, the default, discovers
#'   every returned member and evaluates each identity independently.
#' @param grid Optional single CMIP6 grid label.
#' @param frequency CMIP6 frequency. Defaults to daily data.
#' @param table Optional CMIP6 table selection. `NULL` discovers a table for
#'   each variable at the requested frequency. An unnamed scalar pins every
#'   variable to one table. A named character vector or list overrides the
#'   named variables and leaves the remainder on their frequency defaults.
#' @param activity Future CMIP6 activity ID.
#' @param historical_activity Historical CMIP6 activity ID.
#' @param index_node ESGF index-node name or URL. Names are matched
#'   case-insensitively against the package node registry. `NULL` uses DKRZ;
#'   `"ORNL"` and `"LLNL"` use the ORNL ESGF 1.5 Bridge endpoint.
#' @param data_node Optional ESGF data-node filter.
#' @param filters Additional named ESGF filters. Core availability filters take
#'   precedence when names overlap.
#' @param store Optional [EsgStore] or store path used by [shift_datasets()].
#' @param ui Optional shift UI configuration forwarded to [shift_datasets()].
#'
#' @return A data frame with one row per model/member/grid identity.
#'   `complete` is `TRUE` only when every requested experiment-variable pair is
#'   present. For complete rows, `table` is a list-column containing the named
#'   per-variable table selection accepted by [shift_cmip6()]. Incomplete rows
#'   use `NA` for variables with no available table. `table_id` is the compact
#'   display value, and `missing` lists absent pairs as `experiment:variable`.
#'
#' @details
#' This function reports Dataset metadata availability. It does not download
#' NetCDF data or verify year-by-year File coverage. Requested period coverage
#' is checked later by the existing `shift_*` workflow resolver.
#'
#' @examples
#' \dontrun{
#' daily_models <- shift_cmip6_avail(
#'     variables = c("tas", "hurs", "pr", "rsds", "rlds", "sfcWind"),
#'     scenarios = c("ssp245", "ssp585"),
#'     frequency = "day"
#' )
#' subset(daily_models, complete)
#' }
#'
#' @export
shift_cmip6_avail <- function(
    variables,
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
    ui = NULL
) {
    checkmate::assert_character(
        variables, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_character(
        scenarios, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_flag(include_historical)
    checkmate::assert_character(
        source, any.missing = FALSE, min.len = 1L, unique = TRUE,
        null.ok = TRUE)
    checkmate::assert_character(
        member, any.missing = FALSE, min.len = 1L, unique = TRUE,
        null.ok = TRUE)
    checkmate::assert_string(grid, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(frequency, min.chars = 1L)
    table <- shift__cmip6_table_spec(table)
    checkmate::assert_string(activity, min.chars = 1L)
    checkmate::assert_string(historical_activity, min.chars = 1L)
    checkmate::assert_string(index_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")

    tables <- if (is.null(table)) {
        NULL
    } else {
        shift__cmip6_variable_tables(variables, frequency, table)
    }
    index_node <- availability__index_node(index_node)
    experiments <- unique(c(
        scenarios,
        if (isTRUE(include_historical)) "historical"
    ))
    activities <- unique(c(
        activity,
        if (isTRUE(include_historical)) historical_activity
    ))

    # `table = NULL` is the public all-table discovery form, so an additional
    # filter cannot silently restore the former single-table behaviour.
    filters$table_id <- NULL
    # Reapply these core constraints after user filters so the returned table
    # always describes the function arguments printed in its rows.
    query_filters <- utils::modifyList(filters, shift__compact_list(list(
        activity_id = activities,
        table_id = if (is.null(tables)) NULL else unique(unname(tables)),
        grid_label = grid,
        data_node = data_node,
        latest = TRUE,
        replica = FALSE,
        fields = AVAILABILITY__DATASET_FIELDS
    )))
    request <- shift_request(
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
    datasets <- availability__collect(request, store = store, ui = ui)
    availability__summarize(
        datasets,
        experiments = experiments,
        variables = variables,
        frequency = frequency,
        table = table,
        index_node = index_node
    )
}
