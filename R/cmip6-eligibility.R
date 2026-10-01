#' @include weather-transform.R cmip6-availability.R
NULL

# Keep typed data.table schemas even when discovery returns no identities.
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

# Normalize one provider catalog without copying unrelated Dataset columns.
# Keep incomplete partitions as rejections, but discard unusable identities.
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

# Stage 2: flatten method contracts once, then expand alternatives and scenarios
# with table joins. Only the small method and role lists need explicit loops.
eligibility__requirements <- function(
    transforms,
    scenarios,
    include_optional_historical
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
    checkmate::assert_flag(include_optional_historical)
    compiled <- vector("list", length(transforms))
    for (key in names(transforms)) {
        transform <- transforms[[key]]
        roles <- c("model_future", "model_historical")
        requirements <- transform@required_inputs[
            intersect(roles, names(transform@required_inputs))
        ]
        if (
            include_optional_historical &&
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
        counts <- ifelse(future, length(scenarios), 1L)
        pairs <- rows[rep(seq_len(nrow(rows)), counts)]
        experiment_order <- sequence(counts)
        data.table::set(pairs, j = "experiment_order", value = experiment_order)
        data.table::set(
            pairs,
            j = "experiment_id",
            value = ifelse(
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

# Stage 3: match all candidates in bulk, choosing one frequency/table per
# variable across experiments, then record completeness and missing pairs.
eligibility__match <- function(catalog, candidates, requirements) {
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
    scores <- matches[,
        list(coverage = data.table::uniqueN(experiment_id)),
        by = c(group, "frequency", "frequency_rank", "table_id")
    ]
    # Map the small frequency vocabulary once, then index all score rows.
    frequencies <- unique(scores$frequency)
    conventional <- vapply(
        frequencies,
        function(value) {
            table <- shift__cmip6_table_id(value)
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
    details
}

# Stage 4: summarize future and historical evidence, apply selection policies,
# and return ordered eligibility and requirement data.tables with fixed schemas.
eligibility__summarize <- function(details, candidates, scenarios, common) {
    role <- complete <- missing <- catalog_eligible <- historical_complete <-
        historical_missing <- NULL
    group <- c("identity_id", "transform_key", "method", "path_id")
    future <- details[
        role == "model_future",
        c(group, "experiment_id", "complete", "missing"),
        with = FALSE
    ]
    data.table::setnames(
        future,
        c("experiment_id", "complete"),
        c("scenario", "catalog_eligible")
    )
    historical <- details[
        role == "model_historical",
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
    result <- list(
        matrix = candidates[matrix, on = "identity_id"],
        requirements = candidates[details, on = "identity_id"]
    )
    schemas <- eligibility__empty()
    fields <- c("source_id", "variant_label", "grid_label")
    for (name in names(result)) {
        data.table::set(result[[name]], j = "identity_id", value = NULL)
        data.table::setcolorder(result[[name]], names(schemas[[name]]))
        # Output order is stable even when the provider changes catalog order.
        order <- c(
            fields,
            "transform_key",
            if (name == "matrix") {
                "scenario"
            } else {
                c("path_id", "role", "experiment_id")
            }
        )
        data.table::setorderv(result[[name]], order)
    }
    result
}

# Apply selection policies separately from metadata matching. Preserve one
# joint path across scenarios and optionally intersect the method-specific pools.
eligibility__select <- function(future, scenarios, common) {
    catalog_eligible <- method_eligible <- common_eligible <- selected <- score <-
        path_id <- missing <- NULL
    group <- c("identity_id", "transform_key", "method", "path_id")
    scores <- future[, list(score = sum(catalog_eligible)), by = group]
    data.table::setorderv(
        scores,
        c("identity_id", "transform_key", "score", "path_id"),
        c(1L, 1L, -1L, 1L)
    )
    chosen <- unique(scores, by = c("identity_id", "transform_key"))
    matrix <- future[chosen, on = group, nomatch = 0L]
    matrix[, method_eligible := score == length(scenarios)]
    matrix[
        is.na(missing) & !method_eligible,
        missing := "No single input path covers all requested scenarios."
    ]
    matrix[, common_eligible := all(method_eligible), by = "identity_id"]
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

# Match a normalized catalog against already compiled requirements. Both
# discovery entry points reuse this reducer; it never fetches metadata or values.
eligibility__evaluate <- function(catalog, requirements, common = FALSE) {
    checkmate::assert_flag(common)
    experiment_id <- variable_id <- role <- NULL
    pairs <- requirements$pairs
    catalog <- catalog[
        experiment_id %in%
            pairs$experiment_id &
            variable_id %in% pairs$variable_id
    ]
    fields <- c("source_id", "variant_label", "grid_label")
    candidates <- unique(catalog[, fields, with = FALSE])
    if (!nrow(candidates)) {
        return(eligibility__empty())
    }
    data.table::setorderv(candidates, fields)
    data.table::set(
        candidates,
        j = "identity_id",
        value = seq_len(nrow(candidates))
    )
    details <- eligibility__match(
        candidates[catalog, on = fields],
        candidates,
        requirements
    )
    scenarios <- unique(pairs[role == "model_future", experiment_id])
    eligibility__summarize(details, candidates, scenarios, common)
}
