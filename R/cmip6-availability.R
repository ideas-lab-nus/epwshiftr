#' @include weather-transform.R
NULL

# Apply Dataset filter precedence shared by public and batch discovery.
# Request identity is supplied directly to shift_request() by each caller.
# availability__filters {{{
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
# }}}

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
# availability__empty {{{
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
# }}}

# Reduce variable-specific Dataset records to one row per stable CMIP6 identity.
# availability__summarize {{{
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
# }}}

# Collect Dataset records through the existing store-native query workflow.
# availability__collect {{{
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
# }}}

# Resolve a public index-node name or URL to the endpoint used by EsgQuery.
# availability__index_node {{{
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
# }}}

# Attach the chosen future input specification to each public method row.
# Full requirement diagnostics remain internal; rejected rows retain missing reasons.
# availability__method_summary {{{
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
# }}}

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
# shift_cmip6_avail {{{
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
# }}}

# Keep typed data.table schemas even when discovery returns no identities.
# eligibility__empty {{{
eligibility__empty <- function() {
    list(
        matrix = data.table::data.table(
            source_id = character(),
            variant_label = character(),
            grid_label = character(),
            transform_key = character(),
            method = character(),
            scenario = character(),
            path_id = integer(),
            catalog_eligible = logical(),
            method_eligible = logical(),
            common_eligible = logical(),
            selected = logical(),
            common = logical(),
            missing = character(),
            period_coverage = character(),
            readability = character(),
            quality = character()
        ),
        requirements = data.table::data.table(
            source_id = character(),
            variant_label = character(),
            grid_label = character(),
            transform_key = character(),
            method = character(),
            path_id = integer(),
            role = character(),
            alternative = integer(),
            experiment_id = character(),
            variables = vector("list", 0L),
            allowed_frequencies = vector("list", 0L),
            frequency_spec = vector("list", 0L),
            table = vector("list", 0L),
            calendars = vector("list", 0L),
            complete = logical(),
            missing = character()
        )
    )
}
# }}}

# Normalize one provider catalog without copying unrelated Dataset columns.
# Keep incomplete partitions as rejections, but discard unusable identities.
# eligibility__catalog {{{
eligibility__catalog <- function(datasets) {
    checkmate::assert_data_frame(datasets)
    fields <- c(
        "source_id",
        "experiment_id",
        "grid_label",
        "variable_id",
        "frequency",
        "table_id",
        "variant_label",
        "member_id"
    )
    # Build only these eight columns, with consistent types, then discard the
    # member alias. The input and its unrelated metadata remain untouched.
    catalog <- data.table::as.data.table(stats::setNames(
        lapply(fields, function(field) {
            value <- datasets[[field]]
            if (is.null(value)) {
                rep(NA_character_, nrow(datasets))
            } else {
                as.character(value)
            }
        }),
        fields
    ))
    fallback <- which(
        (is.na(catalog$variant_label) | !nzchar(catalog$variant_label)) &
            !is.na(catalog$member_id) &
            nzchar(catalog$member_id)
    )
    data.table::set(
        catalog,
        i = fallback,
        j = "variant_label",
        value = catalog$member_id[fallback]
    )
    data.table::set(catalog, j = "member_id", value = NULL)
    unique(catalog[
        !is.na(source_id) &
            nzchar(source_id) &
            !is.na(variant_label) &
            nzchar(variant_label) &
            !is.na(grid_label) &
            nzchar(grid_label)
    ])
}
# }}}

# Stage 2: flatten method contracts once, then expand alternatives and scenarios
# with table joins. Only the small method and role lists need explicit loops.
# eligibility__requirements {{{
eligibility__requirements <- function(
    transforms,
    scenarios,
    historical
) {
    checkmate::assert_character(
        scenarios,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    if (any(!nzchar(scenarios)) || "historical" %in% scenarios) {
        cli::cli_abort("Scenarios must be non-empty future experiment IDs.")
    }
    checkmate::assert_logical(
        historical,
        any.missing = FALSE,
        len = length(transforms)
    )
    checkmate::assert_names(names(historical), identical.to = names(transforms))
    compiled <- vector("list", length(transforms))
    for (key in names(transforms)) {
        transform <- transforms[[key]]
        # Select actual catalog roles before intersecting variable alternatives
        # and frequencies; unused local/optional history must not constrain them.
        roles <- if (historical[[key]]) {
            c("model_future", "model_historical")
        } else {
            "model_future"
        }
        requirements <- transform@required_inputs[
            intersect(roles, names(transform@required_inputs))
        ]
        if (
            historical[[key]] &&
                "model_historical" %in% names(transform@optional_inputs)
        ) {
            requirements$model_historical <- transform@optional_inputs$model_historical
        }
        if (!"model_future" %in% names(requirements)) {
            cli::cli_abort(
                "The transform must declare a required model_future input."
            )
        }
        role_rows <- list()
        # The execution resolver pins one variable combination across model
        # periods. Match alternatives by variables, not their declaration order.
        alternatives <- lapply(requirements, function(requirement) {
            vapply(
                requirement@variable_sets,
                function(variables) {
                    paste(sort(as.character(variables)), collapse = "\r")
                },
                character(1L)
            )
        })
        for (role in names(requirements)) {
            requirement <- requirements[[role]]
            if (!length(requirement@variable_sets)) {
                cli::cli_abort(
                    "Input role {.val {role}} has no variable alternatives."
                )
            }
            variables <- unlist(requirement@variable_sets, use.names = FALSE)
            unique_variables <- unique(variables)
            frequencies <- stats::setNames(
                lapply(unique_variables, function(variable) {
                    allowed <- requirement@variable_frequencies[[variable]]
                    if (is.null(allowed)) {
                        allowed <- requirement@frequencies
                    }
                    if (!length(allowed)) {
                        cli::cli_abort(
                            "No source frequency is declared for {.val {variable}}."
                        )
                    }
                    as.character(allowed)
                }),
                unique_variables
            )
            counts <- lengths(requirement@variable_sets)
            role_rows[[role]] <- data.table::data.table(
                role = role,
                role_order = match(role, names(requirements)),
                alternative = rep(seq_along(counts), counts),
                variable_order = sequence(counts),
                variable_id = variables,
                allowed = unname(frequencies[variables]),
                calendars = rep(list(requirement@calendars), length(variables))
            )
        }
        paths <- data.table::data.table(
            model_future = seq_along(alternatives$model_future)
        )
        for (role in setdiff(names(alternatives), "model_future")) {
            data.table::set(
                paths,
                j = role,
                value = match(
                    alternatives$model_future,
                    alternatives[[role]]
                )
            )
        }
        paths <- paths[stats::complete.cases(paths)]
        if (!nrow(paths)) {
            cli::cli_abort(paste(
                "Future and historical inputs must share",
                "a variable combination supported by the execution resolver."
            ))
        }
        data.table::set(paths, j = "path_id", value = seq_len(nrow(paths)))
        path_roles <- data.table::melt(
            paths,
            id.vars = "path_id",
            measure.vars = names(requirements),
            variable.name = "role",
            value.name = "alternative",
            variable.factor = FALSE
        )
        rows <- data.table::rbindlist(role_rows)[
            path_roles,
            on = c("role", "alternative"),
            allow.cartesian = TRUE
        ]
        data.table::setorderv(
            rows,
            c("path_id", "role_order", "variable_order")
        )

        # Repeat future rows for the requested scenarios in one indexed expansion.
        # Sorting restores role/scenario/variable priority without per-scenario tables.
        future <- rows$role == "model_future"
        counts <- data.table::fifelse(future, length(scenarios), 1L)
        pairs <- rows[rep(seq_len(nrow(rows)), counts)]
        experiment_order <- sequence(counts)
        data.table::set(pairs, j = "experiment_order", value = experiment_order)
        data.table::set(
            pairs,
            j = "experiment_id",
            value = data.table::fifelse(
                pairs$role == "model_future",
                scenarios[experiment_order],
                "historical"
            )
        )
        data.table::setorderv(
            pairs,
            c("path_id", "role_order", "experiment_order", "variable_order")
        )

        # Intersect shared-role frequencies once per path/variable, attach them
        # to the requested experiments, then unnest the frequency lists once.
        allowed <- NULL
        shared <- rows[,
            list(allowed = list(Reduce(intersect, allowed))),
            by = c("path_id", "variable_id")
        ]
        lookup <- shared[
            unique(pairs[,
                c("path_id", "variable_id", "experiment_id"),
                with = FALSE
            ]),
            on = c("path_id", "variable_id"),
            allow.cartesian = TRUE
        ]
        counts <- lengths(lookup$allowed)
        choices <- lookup$allowed
        lookup <- lookup[
            rep(seq_len(nrow(lookup)), counts),
            c("path_id", "variable_id", "experiment_id"),
            with = FALSE
        ]
        data.table::set(
            lookup,
            j = "frequency",
            value = as.character(unlist(choices, use.names = FALSE))
        )
        data.table::set(lookup, j = "frequency_rank", value = sequence(counts))
        data.table::set(pairs, j = "transform_key", value = key)
        data.table::set(pairs, j = "method", value = transform@method)
        data.table::set(lookup, j = "transform_key", value = key)
        compiled[[match(key, names(transforms))]] <- list(
            pairs = pairs[,
                c(
                    "transform_key",
                    "method",
                    "path_id",
                    "role",
                    "alternative",
                    "experiment_id",
                    "variable_id",
                    "allowed",
                    "calendars"
                ),
                with = FALSE
            ],
            lookup = lookup[,
                c(
                    "transform_key",
                    "path_id",
                    "variable_id",
                    "experiment_id",
                    "frequency",
                    "frequency_rank"
                ),
                with = FALSE
            ]
        )
    }
    list(
        pairs = data.table::rbindlist(lapply(compiled, `[[`, "pairs")),
        lookup = data.table::rbindlist(lapply(compiled, `[[`, "lookup"))
    )
}
# }}}

# Stage 3: match all candidates in bulk, choosing one frequency/table per
# variable across experiments, then record completeness and missing pairs.
# eligibility__match {{{
eligibility__match <- function(catalog, requirements) {
    pairs <- requirements$pairs
    catalog <- catalog[
        experiment_id %in%
            pairs$experiment_id &
            variable_id %in% pairs$variable_id
    ]
    fields <- c("source_id", "variant_label", "grid_label")
    candidates <- unique(catalog[, fields, with = FALSE])
    if (!nrow(candidates)) {
        return(eligibility__empty()$requirements)
    }
    data.table::setorderv(candidates, fields)
    data.table::set(
        candidates,
        j = "identity_id",
        value = seq_len(nrow(candidates))
    )
    catalog <- candidates[catalog, on = fields]

    experiment_id <- frequency <- frequency_rank <- table_id <- coverage <-
        preferred <- NULL
    # Index the compact copy once for the bulk availability joins below.
    data.table::setkeyv(
        catalog,
        c(
            "variable_id",
            "experiment_id",
            "frequency",
            "table_id",
            "identity_id"
        )
    )
    lookup <- requirements$lookup
    pairs <- requirements$pairs
    matches <- catalog[
        lookup,
        on = c("variable_id", "experiment_id", "frequency"),
        nomatch = 0L,
        allow.cartesian = TRUE
    ]
    matches <- matches[!is.na(table_id) & nzchar(table_id)]
    group <- c("identity_id", "transform_key", "path_id", "variable_id")
    # Catalog rows are unique by identity/variable/experiment/partition, and
    # compiled lookups are unique by path/variable/experiment/frequency. Their
    # join therefore has exactly one row per experiment in each score group.
    scores <- matches[,
        list(coverage = .N),
        by = c(group, "frequency", "frequency_rank", "table_id")
    ]
    # Map the small frequency vocabulary once, then index all score rows.
    frequencies <- unique(scores$frequency)
    conventional <- vapply(
        frequencies,
        function(value) {
            table <- shift_spec__cmip6_table_id(value)
            if (is.null(table)) NA_character_ else table
        },
        character(1L)
    )
    conventional <- conventional[match(scores$frequency, frequencies)]
    scores[,
        preferred := as.integer(is.na(conventional) | table_id != conventional)
    ]
    data.table::setorderv(
        scores,
        c(group, "coverage", "preferred", "frequency_rank", "table_id"),
        c(rep(1L, length(group)), -1L, 1L, 1L, 1L)
    )
    partitions <- unique(scores, by = group)[,
        c(group, "frequency", "table_id"),
        with = FALSE
    ]

    # Expand only the small requirements table, never the full provider catalog.
    identity_id <- pair_id <- frequency <- table_id <- present <- variable_id <-
        allowed <- calendars <- experiment_id <- NULL
    i.frequency <- i.table_id <- NULL
    axes <- data.table::CJ(
        identity_id = candidates$identity_id,
        pair_id = seq_len(nrow(pairs)),
        sorted = FALSE
    )
    expected <- cbind(axes[, "identity_id", with = FALSE], pairs[axes$pair_id])
    expected[, `:=`(
        frequency = NA_character_,
        table_id = NA_character_,
        present = FALSE
    )]
    expected[
        partitions,
        on = c("identity_id", "transform_key", "path_id", "variable_id"),
        `:=`(frequency = i.frequency, table_id = i.table_id)
    ]
    # NA partitions always remain missing, even when a provider has NA fields.
    available <- catalog[!is.na(frequency) & !is.na(table_id)]
    expected[
        available,
        on = c(
            "identity_id",
            "variable_id",
            "experiment_id",
            "frequency",
            "table_id"
        ),
        present := TRUE
    ]
    details <- expected[,
        list(
            variables = list(variable_id),
            allowed_frequencies = list(stats::setNames(allowed, variable_id)),
            frequency_spec = list(stats::setNames(frequency, variable_id)),
            table = list(stats::setNames(table_id, variable_id)),
            calendars = list(calendars[[1L]]),
            complete = all(present),
            missing = if (all(present)) {
                NA_character_
            } else {
                paste(
                    paste(.BY$experiment_id, variable_id[!present], sep = ":"),
                    collapse = "; "
                )
            }
        ),
        by = c(
            "identity_id",
            "transform_key",
            "method",
            "path_id",
            "role",
            "alternative",
            "experiment_id"
        )
    ]
    details <- candidates[details, on = "identity_id"]
    data.table::set(details, j = "identity_id", value = NULL)
    data.table::setcolorder(details, names(eligibility__empty()$requirements))
    data.table::setorderv(
        details,
        c(fields, "transform_key", "path_id", "role", "experiment_id")
    )
    details
}
# }}}

# Stage 4: summarize future and historical evidence, apply selection policies,
# and return ordered eligibility and requirement data.tables with fixed schemas.
# eligibility__summarize {{{
eligibility__summarize <- function(details, scenarios, common) {
    checkmate::assert_flag(common)
    if (!nrow(details)) {
        return(eligibility__empty())
    }
    role <- complete <- missing <- catalog_eligible <- historical_complete <-
        historical_missing <- NULL
    group <- c(
        "source_id",
        "variant_label",
        "grid_label",
        "transform_key",
        "method",
        "path_id"
    )
    # Explicit row positions avoid adding automatic indexes to caller-owned evidence.
    future <- details[
        which(details$role == "model_future"),
        c(group, "experiment_id", "complete", "missing"),
        with = FALSE
    ]
    data.table::setnames(
        future,
        c("experiment_id", "complete"),
        c("scenario", "catalog_eligible")
    )
    historical <- details[
        which(details$role == "model_historical"),
        list(
            historical_complete = all(complete),
            historical_missing = if (all(complete)) {
                NA_character_
            } else {
                paste(
                    missing[!is.na(missing)],
                    collapse = "; "
                )
            }
        ),
        by = group
    ]
    future <- merge(future, historical, by = group, all.x = TRUE, sort = FALSE)
    future[,
        catalog_eligible := catalog_eligible &
            (is.na(historical_complete) | historical_complete)
    ]
    # Keep future and historical missing pairs in their original role order.
    future[,
        missing := data.table::fifelse(
            is.na(missing),
            historical_missing,
            data.table::fifelse(
                is.na(historical_missing),
                missing,
                paste(missing, historical_missing, sep = "; ")
            )
        )
    ]
    future[, c("historical_complete", "historical_missing") := NULL]
    matrix <- eligibility__select(future, scenarios, common)
    data.table::setcolorder(matrix, names(eligibility__empty()$matrix))
    data.table::setorderv(
        matrix,
        c(
            "source_id",
            "variant_label",
            "grid_label",
            "transform_key",
            "scenario"
        )
    )
    list(matrix = matrix, requirements = details)
}
# }}}

# Apply selection policies separately from metadata matching. Preserve one
# joint path across scenarios and optionally intersect the method-specific pools.
# eligibility__select {{{
eligibility__select <- function(future, scenarios, common) {
    catalog_eligible <- method_eligible <- common_eligible <- selected <- score <-
        path_id <- missing <- NULL
    group <- c(
        "source_id",
        "variant_label",
        "grid_label",
        "transform_key",
        "method",
        "path_id"
    )
    scores <- future[, list(score = sum(catalog_eligible)), by = group]
    data.table::setorderv(
        scores,
        c(
            "source_id",
            "variant_label",
            "grid_label",
            "transform_key",
            "score",
            "path_id"
        ),
        c(1L, 1L, 1L, 1L, -1L, 1L)
    )
    chosen <- unique(
        scores,
        by = c("source_id", "variant_label", "grid_label", "transform_key")
    )
    matrix <- future[chosen, on = group, nomatch = 0L]
    matrix[, method_eligible := score == length(scenarios)]
    matrix[
        is.na(missing) & !method_eligible,
        missing := "No single input path covers all requested scenarios."
    ]
    matrix[,
        common_eligible := all(method_eligible),
        by = c("source_id", "variant_label", "grid_label")
    ]
    matrix[,
        selected := if (common) common_eligible else method_eligible
    ]
    matrix[, `:=`(
        common = common,
        score = NULL,
        period_coverage = "not_checked",
        readability = "not_checked",
        quality = "not_checked"
    )]
    matrix
}
# }}}

# vim: fdm=marker :
