#' @include weather-transform.R cmip6-availability.R
NULL

# Keep typed data.table schemas even when discovery returns no identities.
eligibility__empty <- function() {
    list(
        matrix = data.table::data.table(
            source_id = character(), variant_label = character(),
            grid_label = character(), transform_key = character(),
            method = character(), scenario = character(), path_id = integer(),
            catalog_eligible = logical(), method_eligible = logical(),
            common_eligible = logical(), selected = logical(), pool = character(),
            missing = character(), period_coverage = character(),
            readability = character(), quality = character()
        ),
        requirements = data.table::data.table(
            source_id = character(), variant_label = character(),
            grid_label = character(), transform_key = character(),
            method = character(), path_id = integer(), role = character(),
            alternative = integer(), experiment_id = character(),
            variables = vector("list", 0L),
            allowed_frequencies = vector("list", 0L),
            frequency_spec = vector("list", 0L), table = vector("list", 0L),
            calendars = vector("list", 0L), complete = logical(),
            missing = character()
        )
    )
}

# Copy only needed columns, normalize provider member aliases, and deduplicate
# Dataset replicas before matching. Caller-owned data.table keys stay intact.
eligibility__catalog <- function(catalog) {
    checkmate::assert_data_frame(catalog)
    fields <- c("source_id", "experiment_id", "grid_label", "variable_id",
        "frequency", "table_id")
    absent <- setdiff(fields, names(catalog))
    if (length(absent) ||
        !any(c("variant_label", "member_id") %in% names(catalog))) {
        cli::cli_abort(c(
            "The Dataset catalog lacks required fields.",
            "x" = paste(c(absent, if (!any(c("variant_label", "member_id") %in%
                names(catalog))) "variant_label or member_id"), collapse = ", ")
        ))
    }
    # Select columns with [[ rather than [ so data.table and base inputs have
    # identical semantics; normalize only the compact metadata projection.
    result <- data.table::as.data.table(stats::setNames(
        lapply(fields, function(field) as.character(catalog[[field]])), fields))
    data.table::set(result, j = "variant_label", value =
        availability__coalesce_character(
            availability__character_column(catalog, "variant_label"),
            availability__character_column(catalog, "member_id")))
    unique(result)
}

# Preserve explicitly requested absent identities as rejection rows.
eligibility__identities <- function(identities) {
    checkmate::assert_data_frame(identities)
    fields <- c("source_id", "variant_label", "grid_label")
    if (!all(fields %in% names(identities))) {
        cli::cli_abort("Identities require source_id, variant_label, and grid_label.")
    }
    result <- data.table::as.data.table(stats::setNames(
        lapply(fields, function(field) as.character(identities[[field]])), fields))
    for (field in fields) {
        checkmate::assert_character(result[[field]], any.missing = FALSE)
        if (any(!nzchar(result[[field]]))) {
            cli::cli_abort("Identity field {.val {field}} contains an empty value.")
        }
    }
    unique(result)
}

# Derive required roles from the existing registry; optional historical model
# input affects eligibility only when the caller explicitly includes it.
eligibility__roles <- function(transform, include_optional_historical) {
    roles <- c("model_future", "model_historical")
    requirements <- transform@required_inputs[
        intersect(roles, names(transform@required_inputs))]
    if (include_optional_historical &&
        "model_historical" %in% names(transform@optional_inputs)) {
        requirements$model_historical <- transform@optional_inputs$model_historical
    }
    if (!"model_future" %in% names(requirements)) {
        cli::cli_abort("The transform must declare a required model_future input.")
    }
    for (role in names(requirements)) {
        if (!length(requirements[[role]]@variable_sets)) {
            cli::cli_abort("Input role {.val {role}} has no variable alternatives.")
        }
    }
    requirements
}

# Honor variable-specific frequency alternatives instead of a universal rate.
eligibility__frequencies <- function(requirement, variable) {
    allowed <- requirement@variable_frequencies[[variable]]
    if (is.null(allowed)) {
        allowed <- requirement@frequencies
    }
    if (!length(allowed)) {
        cli::cli_abort("No source frequency is declared for {.val {variable}}.")
    }
    as.character(allowed)
}

# Compile method paths once, before touching candidate identities. Match rows
# use intersected frequencies across roles; detail rows retain original needs.
eligibility__compile <- function(transform, transform_key, scenarios,
                                 include_optional_historical) {
    requirements <- eligibility__roles(transform, include_optional_historical)
    alternatives <- lapply(requirements, function(requirement) {
        seq_along(requirement@variable_sets)
    })
    # Reverse CJ axes to retain the declared first-role-fast path numbering.
    paths <- do.call(data.table::CJ, c(rev(alternatives), list(sorted = FALSE)))
    data.table::setcolorder(paths, names(requirements))
    pairs <- lookups <- list()
    for (path_id in seq_len(nrow(paths))) {
        path <- paths[path_id]
        variables <- unique(unlist(lapply(names(requirements), function(role) {
            requirements[[role]]@variable_sets[[path[[role]]]]
        }), use.names = FALSE))
        for (variable in variables) {
            roles <- names(requirements)[vapply(names(requirements), function(role) {
                variable %in% requirements[[role]]@variable_sets[[path[[role]]]]
            }, logical(1L))]
            allowed <- Reduce(intersect, lapply(roles, function(role) {
                eligibility__frequencies(requirements[[role]], variable)
            }))
            experiments <- unique(unlist(lapply(roles, function(role) {
                if (role == "model_future") scenarios else "historical"
            }), use.names = FALSE))
            lookups[[length(lookups) + 1L]] <- data.table::data.table(
                transform_key = transform_key, path_id = path_id,
                variable_id = variable,
                experiment_id = rep(experiments, each = length(allowed)),
                frequency = rep(allowed, times = length(experiments)),
                frequency_rank = rep(seq_along(allowed), times = length(experiments)))
        }
        for (role in names(requirements)) {
            requirement <- requirements[[role]]
            alternative <- path[[role]]
            variables <- requirement@variable_sets[[alternative]]
            allowed <- lapply(variables, function(variable) {
                eligibility__frequencies(requirement, variable)
            })
            experiments <- if (role == "model_future") scenarios else "historical"
            for (experiment in experiments) {
                pairs[[length(pairs) + 1L]] <- data.table::data.table(
                    transform_key = transform_key, method = transform@method,
                    path_id = path_id, role = role, alternative = alternative,
                    experiment_id = experiment, variable_id = variables,
                    allowed = allowed, calendars = rep(list(requirement@calendars),
                        length(variables)))
            }
        }
    }
    list(pairs = data.table::rbindlist(pairs),
        lookup = data.table::rbindlist(lookups))
}

# Match the whole compact catalog once and choose frequency/table partitions
# by identity, method, path, and variable. Replicas cannot inflate coverage.
eligibility__partitions <- function(catalog, lookup) {
    experiment_id <- frequency <- frequency_rank <- table_id <- coverage <-
        preferred <- NULL
    matches <- catalog[lookup, on = c("variable_id", "experiment_id", "frequency"),
        nomatch = 0L, allow.cartesian = TRUE]
    matches <- matches[!is.na(table_id) & nzchar(table_id)]
    group <- c("identity_id", "transform_key", "path_id", "variable_id")
    scores <- matches[, list(coverage = data.table::uniqueN(experiment_id)),
        by = c(group, "frequency", "frequency_rank", "table_id")]
    conventional <- vapply(scores$frequency, function(value) {
        table <- shift__cmip6_table_id(value)
        if (is.null(table)) NA_character_ else table
    }, character(1L))
    scores[, preferred := as.integer(is.na(conventional) | table_id != conventional)]
    data.table::setorderv(scores,
        c(group, "coverage", "preferred", "frequency_rank", "table_id"),
        c(rep(1L, length(group)), -1L, 1L, 1L, 1L))
    unique(scores, by = group)[, c(group, "frequency", "table_id"), with = FALSE]
}

# Expand small compiled requirements against identities, then fill chosen
# partitions and presence with indexed joins rather than per-row scans.
eligibility__details <- function(catalog, candidates, pairs, partitions) {
    identity_id <- pair_id <- frequency <- table_id <- present <- variable_id <-
        allowed <- calendars <- experiment_id <- NULL
    i.frequency <- i.table_id <- NULL
    axes <- data.table::CJ(identity_id = candidates$identity_id,
        pair_id = seq_len(nrow(pairs)), sorted = FALSE)
    expected <- cbind(axes[, "identity_id", with = FALSE], pairs[axes$pair_id])
    expected[, `:=`(frequency = NA_character_, table_id = NA_character_,
        present = FALSE)]
    expected[partitions, on = c("identity_id", "transform_key", "path_id", "variable_id"),
        `:=`(frequency = i.frequency, table_id = i.table_id)]
    # NA partitions always remain missing, even when a provider has NA fields.
    available <- catalog[!is.na(frequency) & !is.na(table_id)]
    expected[available, on = c("identity_id", "variable_id", "experiment_id",
        "frequency", "table_id"), present := TRUE]
    details <- expected[, list(
        variables = list(variable_id),
        allowed_frequencies = list(stats::setNames(allowed, variable_id)),
        frequency_spec = list(stats::setNames(frequency, variable_id)),
        table = list(stats::setNames(table_id, variable_id)),
        calendars = list(calendars[[1L]]), complete = all(present),
        missing = if (all(present)) NA_character_ else paste(
            paste(.BY$experiment_id, variable_id[!present], sep = ":"),
            collapse = "; ")
    ), by = c("identity_id", "transform_key", "method", "path_id", "role",
        "alternative", "experiment_id")]
    details
}

# Score complete joint paths across scenarios and preserve rejected rows.
# Method-specific pools and a common comparison pool share the same evidence.
eligibility__matrix <- function(details, scenarios, pool) {
    role <- complete <- missing <- catalog_eligible <- method_eligible <-
        common_eligible <- selected <- score <- path_id <- historical_complete <-
        historical_missing <- NULL
    group <- c("identity_id", "transform_key", "method", "path_id")
    future <- details[role == "model_future", c(group, "experiment_id",
        "complete", "missing"), with = FALSE]
    data.table::setnames(future, c("experiment_id", "complete"),
        c("scenario", "catalog_eligible"))
    historical <- details[role == "model_historical", list(
        historical_complete = all(complete),
        historical_missing = if (all(complete)) NA_character_ else paste(
            missing[!is.na(missing)], collapse = "; ")
    ), by = group]
    future <- merge(future, historical, by = group, all.x = TRUE, sort = FALSE)
    future[, catalog_eligible := catalog_eligible &
        (is.na(historical_complete) | historical_complete)]
    # Keep future and historical missing pairs in their original role order.
    future[, missing := vapply(seq_len(.N), function(index) {
        values <- c(missing[[index]], historical_missing[[index]])
        values <- values[!is.na(values)]
        if (length(values)) paste(values, collapse = "; ") else NA_character_
    }, character(1L))]
    future[, c("historical_complete", "historical_missing") := NULL]
    scores <- future[, list(score = sum(catalog_eligible)), by = group]
    data.table::setorderv(scores, c("identity_id", "transform_key", "score", "path_id"),
        c(1L, 1L, -1L, 1L))
    chosen <- unique(scores, by = c("identity_id", "transform_key"))
    matrix <- future[chosen, on = group, nomatch = 0L]
    matrix[, method_eligible := score == length(scenarios)]
    matrix[is.na(missing) & !method_eligible,
        missing := "No single input path covers all requested scenarios."]
    matrix[, common_eligible := all(method_eligible), by = "identity_id"]
    matrix[, selected := if (pool == "common") common_eligible else method_eligible]
    matrix[, `:=`(pool = pool, score = NULL,
        period_coverage = "not_checked", readability = "not_checked",
        quality = "not_checked")]
    matrix
}

# Internal catalog reducer for online discovery and cached-result reuse. The
# caller resolves registered transforms; this layer never searches or reads
# weather values and deliberately exposes no separate public offline API.
eligibility__evaluate <- function(catalog, transforms, scenarios,
                                  pool = c("per_method", "common"),
                                  include_optional_historical = FALSE,
                                  identities = NULL) {
    checkmate::assert_list(transforms, min.len = 1L, names = "unique")
    if (!all(vapply(transforms, function(transform) {
        S7::S7_inherits(transform, WeatherTransformSpec)
    }, logical(1L)))) {
        cli::cli_abort("Transforms must contain resolved WeatherTransformSpec objects.")
    }
    checkmate::assert_character(scenarios, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    if (any(!nzchar(scenarios)) || "historical" %in% scenarios) {
        cli::cli_abort("Scenarios must be non-empty future experiment IDs.")
    }
    pool <- match.arg(pool)
    checkmate::assert_flag(include_optional_historical)
    catalog <- eligibility__catalog(catalog)
    fields <- c("source_id", "variant_label", "grid_label")
    candidates <- eligibility__identities(catalog[, fields, with = FALSE])
    if (!is.null(identities)) {
        candidates <- unique(data.table::rbindlist(list(candidates,
            eligibility__identities(identities))))
    }
    data.table::setorderv(candidates, fields)
    compiled <- lapply(names(transforms), function(key) {
        eligibility__compile(transforms[[key]], key, scenarios,
            include_optional_historical)
    })
    if (!nrow(candidates)) {
        return(eligibility__empty())
    }
    data.table::set(candidates, j = "identity_id", value = seq_len(nrow(candidates)))
    catalog <- candidates[catalog, on = fields]
    data.table::setkeyv(catalog,
        c("variable_id", "experiment_id", "frequency", "table_id", "identity_id"))
    pairs <- data.table::rbindlist(lapply(compiled, `[[`, "pairs"))
    lookup <- data.table::rbindlist(lapply(compiled, `[[`, "lookup"))
    partitions <- eligibility__partitions(catalog, lookup)
    details <- eligibility__details(catalog, candidates, pairs, partitions)
    matrix <- eligibility__matrix(details, scenarios, pool)
    result <- list(matrix = candidates[matrix, on = "identity_id"],
        requirements = candidates[details, on = "identity_id"])
    schemas <- eligibility__empty()
    for (name in names(result)) {
        data.table::set(result[[name]], j = "identity_id", value = NULL)
        data.table::setcolorder(result[[name]], names(schemas[[name]]))
        # Stable identity/method/scenario order is independent of provider order.
        order <- c(fields, "transform_key", if (name == "matrix") "scenario" else
            c("path_id", "role", "experiment_id"))
        data.table::setorderv(result[[name]], order)
    }
    result
}
