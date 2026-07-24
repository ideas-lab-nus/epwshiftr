# Daily CMIP6 research profiles separate the minimum M1 inputs from the
# temperature-range inputs used by the enhanced prototype.
DAILY__PROFILES <- c("core", "enhanced")

# Return the canonical daily-variable contract and its permitted ESGF source
# alternatives for one research profile.
daily__requirements <- function(profile = DAILY__PROFILES) {
    profile <- match.arg(profile, DAILY__PROFILES)

    requirements <- list(
        tas = list("tas"),
        hurs = list(c("huss", "tas", "ps"), "hurs"),
        pr = list("pr"),
        rsds = list("rsds"),
        rlds = list("rlds"),
        sfcWind = list("sfcWind")
    )
    if (identical(profile, "enhanced")) {
        # Daily extrema are an enhanced-profile requirement because they let a
        # later backend alter the diurnal temperature range independently.
        requirements <- append(
            requirements,
            list(tasmax = list("tasmax"), tasmin = list("tasmin")),
            after = 1L
        )
    }
    requirements
}

# Expand canonical requirements to the exact ESGF variables worth querying.
daily__input_variables <- function(profiles = DAILY__PROFILES) {
    checkmate::assert_subset(profiles, DAILY__PROFILES, empty.ok = FALSE)
    unique(unlist(
        lapply(profiles, function(profile) {
            unlist(daily__requirements(profile), recursive = TRUE,
                use.names = FALSE)
        }),
        use.names = FALSE
    ))
}

# Normalize variable-specific ESGF Dataset records into the File-catalog shape
# consumed by the shared identity evaluator. Synthetic bounds support only the
# presence screen; File metadata remains authoritative for every year and gap.
daily__dataset_catalog <- function(datasets, years) {
    checkmate::assert_data_frame(datasets)
    checkmate::assert_integerish(years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)

    catalog <- data.table::as.data.table(data.table::copy(datasets))
    n <- nrow(catalog)
    character_column <- function(name) {
        value <- catalog[[name]]
        if (is.null(value)) {
            return(rep(NA_character_, n))
        }
        as.character(value)
    }
    coalesce_columns <- function(...) {
        values <- list(...)
        output <- rep(NA_character_, n)
        for (value in values) {
            use <- (is.na(output) | !nzchar(output)) &
                !is.na(value) & nzchar(value)
            output[use] <- value[use]
        }
        output
    }

    # CMIP6 Dataset responses commonly use member_id and datetime_stop while
    # File catalogs and the production resolver use the canonical aliases.
    catalog[["variant_label"]] <- coalesce_columns(
        character_column("variant_label"),
        character_column("member_id")
    )
    catalog[["advertised_datetime_start"]] <-
        character_column("datetime_start")
    catalog[["advertised_datetime_end"]] <- coalesce_columns(
        character_column("datetime_end"),
        character_column("datetime_stop")
    )
    for (name in c(
        "source_id", "experiment_id", "grid_label", "variable_id",
        "frequency", "table_id"
    )) {
        catalog[[name]] <- character_column(name)
    }
    catalog[
        is.na(frequency) | !nzchar(frequency),
        frequency := "day"
    ]
    catalog[
        is.na(table_id) | !nzchar(table_id),
        table_id := "day"
    ]

    # Dataset discovery deliberately avoids unreliable index-level time fields.
    # Synthetic bounds let the shared evaluator reuse its variable/identity
    # logic without treating Dataset rows as evidence of temporal coverage.
    catalog[["datetime_start"]] <- rep(
        sprintf("%d-01-01T00:00:00Z", min(as.integer(years))),
        n
    )
    catalog[["datetime_end"]] <- rep(
        sprintf("%d-12-31T23:59:59Z", max(as.integer(years))),
        n
    )
    required_identity <- c(
        "source_id", "experiment_id", "variant_label", "grid_label",
        "variable_id"
    )
    complete_identity <- Reduce(
        `&`,
        lapply(required_identity, function(name) {
            !is.na(catalog[[name]]) & nzchar(catalog[[name]])
        })
    )
    catalog <- catalog[
        complete_identity &
            frequency == "day" &
            table_id == "day"
    ]
    catalog[]
}

# Evaluate global Dataset discovery results with the same variable alternatives
# and cross-period identity contract later applied to File records.
daily__evaluate_datasets <- function(future_datasets, historical_datasets,
                                     scenarios, future_years,
                                     historical_years, models = NULL,
                                     profiles = DAILY__PROFILES) {
    checkmate::assert_data_frame(future_datasets)
    checkmate::assert_data_frame(historical_datasets)
    checkmate::assert_character(scenarios, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(future_years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(historical_years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_character(models, any.missing = FALSE,
        min.len = 1L, unique = TRUE, null.ok = TRUE)
    checkmate::assert_subset(profiles, DAILY__PROFILES, empty.ok = FALSE)

    future_catalog <- daily__dataset_catalog(
        future_datasets, future_years)
    historical_catalog <- daily__dataset_catalog(
        historical_datasets, historical_years)
    if (is.null(models)) {
        # The union keeps partial models visible in the discovery receipt even
        # when only one side of the historical/future contract is present.
        models <- sort(unique(c(
            future_catalog$source_id,
            historical_catalog$source_id
        )))
    }
    models <- as.character(models)
    if (!length(models)) {
        return(list(
            models = character(),
            future_catalog = future_catalog,
            historical_catalog = historical_catalog,
            candidates = data.table::data.table(),
            intersections = data.table::data.table(),
            summary = data.table::data.table(),
            variable_coverage = data.table::data.table()
        ))
    }

    evaluation <- daily__evaluate_catalogs(
        future_catalog = future_catalog,
        historical_catalog = historical_catalog,
        models = models,
        scenarios = scenarios,
        future_years = future_years,
        historical_years = historical_years,
        profiles = profiles
    )
    c(list(
        models = models,
        future_catalog = future_catalog,
        historical_catalog = historical_catalog
    ), evaluation)
}

# Evaluate one historical or future catalog with the same whole-identity rules
# used by the production CMIP6 resolver.
daily__candidate_rows <- function(catalog, models, experiments, years,
                                  profile, period_role) {
    checkmate::assert_data_frame(catalog)
    checkmate::assert_character(models, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_character(experiments, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_choice(profile, DAILY__PROFILES)
    checkmate::assert_choice(period_role, c("future", "historical"))

    requirements <- daily__requirements(profile)
    candidates <- shift__cmip6_candidates(
        catalog = catalog,
        models = models,
        experiments = experiments,
        variables = unique(unlist(requirements, recursive = TRUE,
            use.names = FALSE)),
        years = years,
        frequency = "day",
        table = "day",
        requirements = requirements
    )
    candidates[, `:=`(
        profile = rep(profile, .N),
        period_role = rep(period_role, .N)
    )]
    data.table::setcolorder(
        candidates,
        c("profile", "period_role",
            setdiff(names(candidates), c("profile", "period_role")))
    )
    candidates[]
}

# Add a role prefix to non-identity fields before future and historical
# candidates are joined.
daily__prefix_candidate_fields <- function(candidates, role, keys) {
    candidates <- data.table::copy(candidates)
    candidates[, candidate_present := rep(TRUE, .N)]
    drop <- intersect("period_role", names(candidates))
    if (length(drop)) {
        candidates[, (drop) := NULL]
    }
    fields <- setdiff(names(candidates), keys)
    data.table::setnames(candidates, fields, paste0(role, "_", fields))
    candidates[]
}

# Count structured missing-requirement clauses while assigning a large penalty
# to a catalog side that has no matching identity at all.
daily__missing_count <- function(value, present) {
    value <- as.character(value)
    present <- as.logical(present)
    vapply(seq_along(value), function(i) {
        if (!isTRUE(present[[i]])) {
            return(1000L)
        }
        item <- value[[i]]
        if (is.na(item) || !nzchar(item)) {
            return(0L)
        }
        length(strsplit(item, "; ", fixed = TRUE)[[1L]])
    }, integer(1L))
}

# Intersect future and historical candidates without allowing model, member,
# grid/table partition, or humidity-source paths to change between periods.
daily__intersections <- function(candidates, models,
                                 profiles = DAILY__PROFILES) {
    checkmate::assert_data_frame(candidates)
    checkmate::assert_character(models, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_subset(profiles, DAILY__PROFILES, empty.ok = FALSE)

    keys <- c(
        "profile", "source_id", "variant_label", "frequency",
        "required_partition_key", "requirement_key"
    )
    future <- candidates[period_role == "future"]
    historical <- candidates[period_role == "historical"]
    future <- daily__prefix_candidate_fields(future, "future", keys)
    historical <- daily__prefix_candidate_fields(
        historical, "historical", keys)
    intersections <- merge(
        future,
        historical,
        by = keys,
        all = TRUE,
        sort = FALSE
    )

    intersections[, `:=`(
        future_present = future_candidate_present %in% TRUE,
        historical_present = historical_candidate_present %in% TRUE,
        future_complete = future_complete %in% TRUE,
        historical_complete = historical_complete %in% TRUE
    )]
    intersections[
        !future_present &
            (is.na(future_missing) | !nzchar(future_missing)),
        future_missing := "no matching future daily catalog identity"
    ]
    intersections[
        !historical_present &
            (is.na(historical_missing) | !nzchar(historical_missing)),
        historical_missing := "no matching historical daily catalog identity"
    ]
    intersections[, complete :=
        future_complete & historical_complete &
        future_present & historical_present]
    intersections[, grid_label := data.table::fcoalesce(
        future_grid_label, historical_grid_label)]
    intersections[, table_id := data.table::fcoalesce(
        future_table_id, historical_table_id)]
    intersections[, future_missing_count := daily__missing_count(
        future_missing, future_present)]
    intersections[, historical_missing_count := daily__missing_count(
        historical_missing, historical_present)]
    intersections[, missing_total :=
        future_missing_count + historical_missing_count]

    intersections[, status := data.table::fcase(
        complete, "complete",
        !future_present & !historical_present, "absent",
        !future_present, "future_absent",
        !historical_present, "historical_absent",
        !future_complete & !historical_complete, "both_incomplete",
        !future_complete, "future_incomplete",
        !historical_complete, "historical_incomplete",
        default = "incomplete"
    )]

    # Every requested model/profile receives a row, including models that did
    # not appear in either ESGF catalog.
    requested <- data.table::CJ(
        profile = as.character(profiles),
        source_id = as.character(models),
        unique = TRUE
    )
    present <- unique(intersections[, .(profile, source_id)])
    absent <- requested[!present, on = .(profile, source_id)]
    if (nrow(absent)) {
        placeholders <- absent[, .(
            profile,
            source_id,
            variant_label = NA_character_,
            frequency = "day",
            required_partition_key = NA_character_,
            requirement_key = NA_character_,
            future_present = FALSE,
            historical_present = FALSE,
            future_complete = FALSE,
            historical_complete = FALSE,
            complete = FALSE,
            grid_label = NA_character_,
            table_id = "day",
            future_missing_count = 1000L,
            historical_missing_count = 1000L,
            missing_total = 2000L,
            status = "absent",
            future_missing = "no future daily catalog identity",
            historical_missing = "no historical daily catalog identity"
        )]
        intersections <- data.table::rbindlist(
            list(intersections, placeholders),
            use.names = TRUE,
            fill = TRUE
        )
    }

    intersections[, `:=`(
        both_present = future_present & historical_present,
        member_preference = data.table::fcase(
            variant_label == "r1i1p1f1", 0L,
            !is.na(variant_label) & nzchar(variant_label), 1L,
            default = 2L
        ),
        grid_preference = data.table::fcase(
            grid_label == "gn", 0L,
            !is.na(grid_label) & nzchar(grid_label), 1L,
            default = 2L
        )
    )]
    data.table::setorderv(
        intersections,
        c(
            "profile", "source_id", "complete", "both_present",
            "missing_total", "member_preference", "grid_preference",
            "variant_label", "required_partition_key"
        ),
        c(1L, 1L, -1L, -1L, 1L, 1L, 1L, 1L, 1L),
        na.last = TRUE
    )
    intersections[, candidate_rank := seq_len(.N),
        by = .(profile, source_id)]
    intersections[]
}

# Select the deterministic best identity for each requested model and profile.
daily__model_summary <- function(intersections) {
    checkmate::assert_data_frame(intersections)
    summary <- data.table::as.data.table(intersections)[candidate_rank == 1L]
    columns <- c(
        "profile", "source_id", "status", "complete", "variant_label",
        "grid_label", "table_id", "requirement_key",
        "future_present", "historical_present", "future_complete",
        "historical_complete", "future_missing", "historical_missing",
        "missing_total", "candidate_rank"
    )
    summary[, intersect(columns, names(summary)), with = FALSE][]
}

# Test whether a catalog row advertises one usable remote access URL.
daily__has_access <- function(rows, field) {
    value <- rows[[field]]
    if (is.null(value)) {
        return(rep(FALSE, nrow(rows)))
    }
    value <- as.character(value)
    !is.na(value) & nzchar(value)
}

# Expand one catalog into an auditable per-variable coverage matrix for all
# experiment/identity combinations observed on that side of the comparison.
daily__variable_coverage <- function(catalog, models, experiments, years,
                                     variables, period_role) {
    checkmate::assert_data_frame(catalog)
    checkmate::assert_character(models, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_character(experiments, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_character(variables, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_choice(period_role, c("future", "historical"))

    catalog <- shift__catalog_current(catalog)
    catalog <- catalog[
        source_id %in% models &
            experiment_id %in% experiments &
            frequency == "day" &
            table_id == "day" &
            variable_id %in% variables
    ]
    empty <- data.table::data.table(
        period_role = character(),
        source_id = character(),
        variant_label = character(),
        grid_label = character(),
        table_id = character(),
        experiment_id = character(),
        variable_id = character(),
        file_count = integer(),
        data_node_count = integer(),
        opendap_file_count = integer(),
        http_file_count = integer(),
        required_year_count = integer(),
        available_year_count = integer(),
        coverage_start_year = integer(),
        coverage_end_year = integer(),
        complete = logical(),
        missing_years = character()
    )
    if (!nrow(catalog)) {
        return(empty)
    }

    identity_fields <- c(
        "source_id", "variant_label", "grid_label", "table_id"
    )
    identities <- unique(catalog[, ..identity_fields])
    identities[, daily_join_key := 1L]
    requested_fields <- data.table::CJ(
        experiment_id = as.character(experiments),
        variable_id = as.character(variables),
        unique = TRUE
    )
    requested_fields[, daily_join_key := 1L]
    targets <- merge(
        identities,
        requested_fields,
        by = "daily_join_key",
        allow.cartesian = TRUE
    )
    targets[, daily_join_key := NULL]
    observed <- catalog[, {
        observed_years <- shift__catalog_years(.SD)
        available_years <- intersect(years, observed_years)
        missing_years <- setdiff(years, observed_years)
        list(
            file_count = .N,
            data_node_count = data.table::uniqueN(
                data_node[!is.na(data_node) & nzchar(data_node)]),
            opendap_file_count = sum(daily__has_access(.SD, "url_opendap")),
            http_file_count = sum(daily__has_access(.SD, "url_download")),
            required_year_count = length(years),
            available_year_count = length(available_years),
            coverage_start_year = if (length(observed_years)) {
                min(observed_years)
            } else {
                NA_integer_
            },
            coverage_end_year = if (length(observed_years)) {
                max(observed_years)
            } else {
                NA_integer_
            },
            complete = !length(missing_years),
            missing_years = if (length(missing_years)) {
                paste(missing_years, collapse = ",")
            } else {
                NA_character_
            }
        )
    }, by = c(identity_fields, "experiment_id", "variable_id")]
    coverage <- observed[targets,
        on = c(identity_fields, "experiment_id", "variable_id")]

    # Explicit zeroes make a missing variable distinguishable from missing
    # metadata values in a file that was actually returned.
    integer_fields <- c(
        "file_count", "data_node_count", "opendap_file_count",
        "http_file_count", "available_year_count"
    )
    for (field in integer_fields) {
        data.table::set(
            coverage,
            which(is.na(coverage[[field]])),
            field,
            0L
        )
    }
    coverage[is.na(required_year_count),
        required_year_count := length(years)]
    coverage[is.na(complete), complete := FALSE]
    coverage[!complete & is.na(missing_years),
        missing_years := paste(years, collapse = ",")]
    coverage[, period_role := period_role]
    data.table::setcolorder(
        coverage,
        c("period_role", identity_fields, "experiment_id", "variable_id",
            setdiff(names(coverage),
                c("period_role", identity_fields, "experiment_id",
                    "variable_id")))
    )
    data.table::setorderv(
        coverage,
        c(
            "source_id", "variant_label", "grid_label",
            "experiment_id", "variable_id"
        )
    )
    coverage[]
}

# Produce every deterministic table needed by the metadata-only research
# probe from two already-collected ESGF File catalogs.
daily__evaluate_catalogs <- function(future_catalog, historical_catalog,
                                     models, scenarios, future_years,
                                     historical_years,
                                     profiles = DAILY__PROFILES) {
    checkmate::assert_data_frame(future_catalog)
    checkmate::assert_data_frame(historical_catalog)
    checkmate::assert_character(models, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_character(scenarios, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(future_years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_integerish(historical_years, any.missing = FALSE,
        min.len = 1L, unique = TRUE)
    checkmate::assert_subset(profiles, DAILY__PROFILES, empty.ok = FALSE)

    candidate_tables <- lapply(profiles, function(profile) {
        list(
            daily__candidate_rows(
                future_catalog,
                models = models,
                experiments = scenarios,
                years = future_years,
                profile = profile,
                period_role = "future"
            ),
            daily__candidate_rows(
                historical_catalog,
                models = models,
                experiments = "historical",
                years = historical_years,
                profile = profile,
                period_role = "historical"
            )
        )
    })
    candidates <- data.table::rbindlist(
        unlist(candidate_tables, recursive = FALSE),
        use.names = TRUE,
        fill = TRUE
    )
    intersections <- daily__intersections(
        candidates, models = models, profiles = profiles)
    variables <- daily__input_variables(profiles)
    variable_coverage <- data.table::rbindlist(list(
        daily__variable_coverage(
            future_catalog,
            models = models,
            experiments = scenarios,
            years = future_years,
            variables = variables,
            period_role = "future"
        ),
        daily__variable_coverage(
            historical_catalog,
            models = models,
            experiments = "historical",
            years = historical_years,
            variables = variables,
            period_role = "historical"
        )
    ), use.names = TRUE, fill = TRUE)

    list(
        candidates = candidates,
        intersections = intersections,
        summary = daily__model_summary(intersections),
        variable_coverage = variable_coverage
    )
}
