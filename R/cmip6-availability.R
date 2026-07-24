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
                                             frequency, table) {
    checkmate::assert_data_frame(datasets)
    catalog <- data.table::as.data.table(data.table::copy(datasets))
    wanted_frequency <- frequency[[1L]]
    wanted_table <- table[[1L]]

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
            frequency == wanted_frequency &
            table_id == wanted_table
    ]
    unique(catalog[, ..identity_fields])
}

# Return a typed empty availability table with the public column contract.
availability__empty <- function() {
    data.frame(
        source_id = character(),
        variant_label = character(),
        grid_label = character(),
        frequency = character(),
        table_id = character(),
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
    catalog <- availability__normalize_datasets(
        datasets,
        experiments = experiments,
        variables = variables,
        frequency = frequency,
        table = table
    )
    if (!nrow(catalog)) {
        return(availability__empty())
    }

    identity_fields <- c(
        "source_id", "variant_label", "grid_label", "frequency", "table_id"
    )
    identities <- unique(catalog[, ..identity_fields])
    identities[, availability_join_key := 1L]
    required <- data.table::CJ(
        experiment_id = as.character(experiments),
        variable_id = as.character(variables),
        unique = TRUE
    )
    required[, availability_join_key := 1L]
    targets <- merge(
        identities,
        required,
        by = "availability_join_key",
        allow.cartesian = TRUE,
        sort = FALSE
    )
    targets[, availability_join_key := NULL]

    observed <- unique(catalog[
        ,
        c(identity_fields, "experiment_id", "variable_id"),
        with = FALSE
    ])
    observed[, present := TRUE]
    coverage <- observed[
        targets,
        on = c(identity_fields, "experiment_id", "variable_id")
    ]
    coverage[is.na(present), present := FALSE]

    summary <- coverage[, {
        missing_rows <- .SD[!present]
        experiment_status <- .SD[, .(complete = all(present)),
            by = experiment_id]
        list(
            complete = all(present),
            complete_experiments = sum(experiment_status$complete),
            required_experiments = data.table::uniqueN(experiment_id),
            available_pairs = sum(present),
            required_pairs = .N,
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
    }, by = identity_fields]
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
#' @param member Optional CMIP6 variant labels. The default limits discovery to
#'   the first realization; use `NULL` to inspect every returned member.
#' @param grid Optional single CMIP6 grid label.
#' @param frequency CMIP6 frequency. Defaults to daily data.
#' @param table Optional single CMIP6 table ID. `NULL` infers the usual table
#'   from `frequency`, for example `"day"` for daily data.
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
#' @return A data frame with one row per model/member/grid/table identity.
#'   `complete` is `TRUE` only when every requested experiment-variable pair is
#'   present. `missing` lists absent pairs as `experiment:variable`.
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
    member = "r1i1p1f1",
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
    checkmate::assert_string(table, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(activity, min.chars = 1L)
    checkmate::assert_string(historical_activity, min.chars = 1L)
    checkmate::assert_string(index_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(data_node, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_list(filters, names = "unique")

    if (is.null(table)) {
        table <- shift__cmip6_table_id(frequency)
        if (is.null(table)) {
            cli::cli_abort(
                "Cannot infer a CMIP6 table for frequency {.val {frequency}}; set `table` explicitly."
            )
        }
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

    # Reapply these core constraints after user filters so the returned table
    # always describes the function arguments printed in its rows.
    query_filters <- utils::modifyList(filters, shift__compact_list(list(
        activity_id = activities,
        table_id = table,
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
