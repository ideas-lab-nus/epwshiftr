#' @include shift-stage.R
NULL

# Resolve catalog coverage and pinned climate inputs for workflow execution.

# Match catalog identity fields while treating missing values as an explicit
# identity rather than relying on data.table's NA comparison behaviour.
shift__catalog_match <- function(x, value) {
    if (is.na(value)) {
        return(is.na(x))
    }
    !is.na(x) & as.character(x) == as.character(value)
}

# Fill absent ESGF File time fields from the CMIP/DRS filename carried in the
# catalog. This defensive resolver layer also repairs cached records created by
# older runs before File-level time enrichment was applied during collection.
shift__catalog_fill_time_ranges <- function(catalog) {
    catalog <- data.table::as.data.table(data.table::copy(catalog))
    if (!nrow(catalog)) {
        return(catalog)
    }
    n <- nrow(catalog)
    ranges <- query_result__fill_time_ranges(catalog, function() {
        labels <- query_result__character_column(catalog, "title", n)
        fallback <- query_result__character_column(catalog, "filename", n)
        labels[is.na(labels) | !nzchar(labels)] <-
            fallback[is.na(labels) | !nzchar(labels)]
        fallback <- query_result__character_column(catalog, "esgf_id", n)
        labels[is.na(labels) | !nzchar(labels)] <-
            fallback[is.na(labels) | !nzchar(labels)]
        labels
    })
    catalog[["datetime_start"]] <-
        query_result__time_iso(ranges$datetime_start)
    catalog[["datetime_end"]] <- query_result__time_iso(ranges$datetime_end)
    catalog[]
}

# Normalize catalog status fields before completeness checks. Superseded,
# retracted, and deprecated records never satisfy a workflow case.
shift__catalog_current <- function(catalog) {
    catalog <- shift__catalog_fill_time_ranges(catalog)
    identity <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "frequency",
        "table_id",
        "variable_id",
        "datetime_start",
        "datetime_end"
    )
    for (name in setdiff(identity, names(catalog))) {
        catalog[[name]] <- rep(NA_character_, nrow(catalog))
    }
    if ("latest" %in% names(catalog)) {
        catalog <- catalog[is.na(latest) | as.logical(latest)]
    }
    if ("retracted" %in% names(catalog)) {
        catalog <- catalog[is.na(retracted) | !as.logical(retracted)]
    }
    if ("deprecated" %in% names(catalog)) {
        catalog <- catalog[is.na(deprecated) | !as.logical(deprecated)]
    }
    catalog[]
}

# Expand the declared file time ranges to a year set so gaps between files do
# not pass a simple min/max coverage test.
shift__catalog_years <- function(rows) {
    if (!nrow(rows)) {
        return(integer())
    }
    years <- integer()
    for (i in seq_len(nrow(rows))) {
        start <- suppressWarnings(as.POSIXct(
            rows$datetime_start[[i]],
            tz = "UTC"
        ))
        stop <- suppressWarnings(as.POSIXct(rows$datetime_end[[i]], tz = "UTC"))
        if (is.na(start) || is.na(stop)) {
            next
        }
        from <- as.integer(format(start, "%Y", tz = "UTC"))
        to <- as.integer(format(stop, "%Y", tz = "UTC"))
        years <- c(years, seq.int(min(from, to), max(from, to)))
    }
    sort(unique(years))
}

# Serialize selected table/grid/variable partitions as row-oriented JSON. The
# scalar representation is stable inside persisted run specs and avoids list
# columns whose one-row shape changes during jsonlite simplification.
shift__cmip6_partition_json <- function(partitions) {
    partitions <- data.table::as.data.table(data.table::copy(partitions))
    columns <- c(
        "variable_id",
        "frequency",
        "table_id",
        "grid_label",
        "required"
    )
    for (name in setdiff(columns, names(partitions))) {
        partitions[[name]] <- if (identical(name, "required")) {
            logical(nrow(partitions))
        } else {
            character(nrow(partitions))
        }
    }
    partitions <- unique(partitions[, columns, with = FALSE])
    if (nrow(partitions)) {
        data.table::setorderv(
            partitions,
            c("frequency", "table_id", "grid_label", "variable_id")
        )
    }
    # jsonlite marks its scalar result with class `json`; stripping that class
    # keeps complete and empty candidate tables type-compatible in rbindlist().
    as.character(jsonlite::toJSON(
        as.data.frame(partitions),
        dataframe = "rows",
        auto_unbox = TRUE,
        null = "null",
        na = "null"
    ))
}

# Restore a persisted partition map and normalize the zero/one-row cases to the
# same typed table used by fresh resolution.
shift__cmip6_partitions <- function(value) {
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) {
        return(data.table::data.table(
            variable_id = character(),
            frequency = character(),
            table_id = character(),
            grid_label = character(),
            required = logical()
        ))
    }
    out <- jsonlite::fromJSON(value[[1L]], simplifyDataFrame = TRUE)
    out <- data.table::as.data.table(out)
    # Older persisted selections predate per-variable frequency partitions.
    # Their missing frequency is filled from the selection row at read time.
    if (!"frequency" %in% names(out)) {
        out[["frequency"]] <- rep(NA_character_, nrow(out))
    }
    for (name in c("variable_id", "frequency", "table_id", "grid_label")) {
        out[[name]] <- as.character(out[[name]])
    }
    out[["required"]] <- as.logical(out[["required"]])
    out[]
}

# Test one variable at one table/grid against every requested year for a single
# experiment. File ranges are expanded rather than inferred from min/max so a
# gap in the middle cannot satisfy the contract.
shift__cmip6_input_complete <- function(
    catalog,
    identity,
    experiment,
    variable,
    frequency,
    table,
    grid,
    years
) {
    # ESGF providers may return convenience columns named `variable` and
    # `grid`. Local aliases prevent data.table from resolving those columns
    # instead of this helper's scalar arguments inside the row expression.
    wanted_source_id <- identity$source_id[[1L]]
    wanted_variant_label <- identity$variant_label[[1L]]
    wanted_experiment <- experiment
    wanted_variable <- variable
    wanted_frequency <- frequency
    wanted_table <- table
    wanted_grid <- grid
    files <- catalog[
        shift__catalog_match(source_id, wanted_source_id) &
            shift__catalog_match(variant_label, wanted_variant_label) &
            shift__catalog_match(experiment_id, wanted_experiment) &
            shift__catalog_match(variable_id, wanted_variable) &
            shift__catalog_match(frequency, wanted_frequency) &
            shift__catalog_match(table_id, wanted_table) &
            shift__catalog_match(grid_label, wanted_grid)
    ]
    nrow(files) > 0L && !length(setdiff(years, shift__catalog_years(files)))
}

# Construct a stable key for one frequency/table partition. Frequency remains
# explicit because CMIP6 can store point states and interval means in one table.
shift__cmip6_partition_id <- function(frequency, table) {
    paste(as.character(frequency), as.character(table), sep = "/")
}

# Expand the per-frequency/table grid choices for one model/member. Missing
# partitions retain an explicit NA choice so near matches remain diagnosable.
shift__cmip6_grid_combinations <- function(
    catalog,
    identity,
    partitions,
    grid = NULL
) {
    partitions <- unique(data.table::as.data.table(partitions)[, .(
        frequency,
        table_id
    )])
    partition_ids <- shift__cmip6_partition_id(
        partitions$frequency,
        partitions$table_id
    )
    choices <- stats::setNames(vector("list", nrow(partitions)), partition_ids)
    for (i in seq_len(nrow(partitions))) {
        wanted_frequency <- partitions$frequency[[i]]
        wanted_table_id <- partitions$table_id[[i]]
        values <- unique(
            catalog[
                shift__catalog_match(source_id, identity$source_id[[1L]]) &
                    shift__catalog_match(
                        variant_label,
                        identity$variant_label[[1L]]
                    ) &
                    shift__catalog_match(frequency, wanted_frequency) &
                    shift__catalog_match(table_id, wanted_table_id)
            ]$grid_label
        )
        values <- sort(values[!is.na(values) & nzchar(values)])
        if (!is.null(grid)) {
            values <- intersect(values, grid)
            if (!length(values)) {
                values <- as.character(grid)
            }
        }
        if (!length(values)) {
            values <- NA_character_
        }
        choices[[partition_ids[[i]]]] <- values
    }
    as.data.frame(
        do.call(
            expand.grid,
            c(
                choices,
                list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
            )
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

# Compute complete model/member candidates while allowing each frequency/table
# partition to use its own grid. The selected partition JSON is authoritative
# for download and extraction, so broad catalog queries cannot create invalid
# frequency/variable or table/grid cross-products downstream.
shift__cmip6_candidates <- function(
    catalog,
    models,
    experiments,
    variables,
    years,
    frequency,
    table = NULL,
    requirements = NULL,
    grid = NULL
) {
    catalog <- shift__catalog_current(catalog)
    models <- as.character(models)
    experiments <- as.character(experiments)
    variables <- unique(as.character(variables))
    years <- sort(unique(as.integer(years)))
    frequency_map <- shift__cmip6_variable_frequencies(variables, frequency)
    if (is.null(requirements)) {
        requirements <- stats::setNames(
            lapply(variables, function(variable) list(variable)),
            variables
        )
    }
    table_map <- shift__cmip6_variable_tables(
        variables,
        frequency_map,
        table
    )
    required_inputs <- unique(unlist(
        requirements,
        recursive = TRUE,
        use.names = FALSE
    ))
    if (!all(required_inputs %in% names(table_map))) {
        cli::cli_abort(
            "CMIP6 table mapping is missing one or more required transform inputs."
        )
    }
    variable_specs <- data.table::data.table(
        variable_id = variables,
        frequency = unname(frequency_map[variables]),
        table_id = unname(table_map[variables])
    )
    required_specs <- unique(variable_specs[
        variable_id %in% required_inputs,
        .(frequency, table_id)
    ])
    wanted_tables <- unique(unname(table_map))
    catalog <- catalog[
        source_id %in%
            models &
            experiment_id %in% experiments &
            variable_id %in% variables &
            frequency %in% unique(unname(frequency_map)) &
            table_id %in% wanted_tables
    ]
    # A broad ESGF query may return another requested frequency for the wrong
    # variable. Enforce the declared pair before evaluating time coverage.
    row_variables <- as.character(catalog$variable_id)
    wanted_row_frequencies <- unname(frequency_map[row_variables])
    wanted_row_tables <- unname(table_map[row_variables])
    keep <- !is.na(catalog$frequency) &
        as.character(catalog$frequency) == wanted_row_frequencies &
        !is.na(catalog$table_id) &
        as.character(catalog$table_id) == wanted_row_tables
    catalog <- catalog[keep]
    identities <- unique(catalog[, .(source_id, variant_label)])
    empty <- data.table::data.table(
        source_id = character(),
        variant_label = character(),
        grid_label = character(),
        frequency = character(),
        table_id = character(),
        required_partition_key = character(),
        requirement_key = character(),
        partition_key = character(),
        partitions_json = character(),
        required_native_grid = logical(),
        all_native_grid = logical(),
        complete = logical(),
        missing = character()
    )
    if (!nrow(identities)) {
        return(empty)
    }

    rows <- list()
    for (identity_index in seq_len(nrow(identities))) {
        identity <- identities[identity_index]
        combinations <- shift__cmip6_grid_combinations(
            catalog,
            identity,
            required_specs,
            grid = grid
        )
        for (combination_index in seq_len(nrow(combinations))) {
            grid_map <- stats::setNames(
                as.character(combinations[combination_index, , drop = TRUE]),
                names(combinations)
            )
            missing <- character()
            selected_sources <- list()

            # One alternative must work for every future scenario. This is the
            # whole-case source rule that prevents future/reference or
            # scenario-level mixing of HUSS and HURS.
            for (canonical in names(requirements)) {
                alternatives <- requirements[[canonical]]
                matched <- NULL
                for (alternative in alternatives) {
                    input_ok <- vapply(
                        experiments,
                        function(experiment) {
                            all(vapply(
                                alternative,
                                function(input) {
                                    partition_id <- shift__cmip6_partition_id(
                                        frequency_map[[input]],
                                        table_map[[input]]
                                    )
                                    shift__cmip6_input_complete(
                                        catalog,
                                        identity,
                                        experiment,
                                        input,
                                        frequency_map[[input]],
                                        table_map[[input]],
                                        grid_map[[partition_id]],
                                        years
                                    )
                                },
                                logical(1L)
                            ))
                        },
                        logical(1L)
                    )
                    if (all(input_ok)) {
                        matched <- as.character(alternative)
                        break
                    }
                }
                if (is.null(matched)) {
                    labels <- vapply(
                        alternatives,
                        paste,
                        character(1L),
                        collapse = "+"
                    )
                    for (experiment in experiments) {
                        missing <- c(
                            missing,
                            sprintf(
                                "%s/%s: requires %s",
                                experiment,
                                canonical,
                                paste(labels, collapse = " or ")
                            )
                        )
                    }
                    matched <- as.character(alternatives[[1L]])
                }
                selected_sources[[canonical]] <- matched
            }

            required_variables <- unique(unlist(
                selected_sources,
                use.names = FALSE
            ))
            required_partitions <- data.table::data.table(
                variable_id = required_variables,
                frequency = unname(frequency_map[required_variables]),
                table_id = unname(table_map[required_variables]),
                grid_label = unname(vapply(
                    required_variables,
                    function(variable) {
                        grid_map[[shift__cmip6_partition_id(
                            frequency_map[[variable]],
                            table_map[[variable]]
                        )]]
                    },
                    character(1L)
                )),
                required = TRUE
            )

            optional_variables <- setdiff(variables, required_inputs)
            optional_partitions <- list()
            optional_specs <- unique(variable_specs[
                variable_id %in% optional_variables,
                .(frequency, table_id)
            ])
            for (optional_index in seq_len(nrow(optional_specs))) {
                wanted_frequency <- optional_specs$frequency[[optional_index]]
                wanted_table_id <- optional_specs$table_id[[optional_index]]
                table_variables <- optional_variables[
                    unname(frequency_map[optional_variables]) ==
                        wanted_frequency &
                        unname(table_map[optional_variables]) == wanted_table_id
                ]
                if (!length(table_variables)) {
                    next
                }
                partition_id <- shift__cmip6_partition_id(
                    wanted_frequency,
                    wanted_table_id
                )
                if (partition_id %in% names(grid_map)) {
                    optional_grids <- grid_map[[partition_id]]
                } else {
                    optional_grids <- unique(
                        catalog[
                            shift__catalog_match(
                                source_id,
                                identity$source_id[[1L]]
                            ) &
                                shift__catalog_match(
                                    variant_label,
                                    identity$variant_label[[1L]]
                                ) &
                                shift__catalog_match(
                                    frequency,
                                    wanted_frequency
                                ) &
                                shift__catalog_match(table_id, wanted_table_id)
                        ]$grid_label
                    )
                    optional_grids <- sort(optional_grids[
                        !is.na(optional_grids) & nzchar(optional_grids)
                    ])
                    if (!is.null(grid)) {
                        optional_grids <- intersect(optional_grids, grid)
                    }
                }
                if (!length(optional_grids) || all(is.na(optional_grids))) {
                    next
                }
                scored <- lapply(optional_grids, function(optional_grid) {
                    complete_variables <- table_variables[vapply(
                        table_variables,
                        function(variable) {
                            all(vapply(
                                experiments,
                                function(experiment) {
                                    shift__cmip6_input_complete(
                                        catalog,
                                        identity,
                                        experiment,
                                        variable,
                                        wanted_frequency,
                                        wanted_table_id,
                                        optional_grid,
                                        years
                                    )
                                },
                                logical(1L)
                            ))
                        },
                        logical(1L)
                    )]
                    list(
                        grid = optional_grid,
                        variables = complete_variables,
                        score = length(complete_variables)
                    )
                })
                scores <- vapply(scored, `[[`, integer(1L), "score")
                if (!length(scores) || max(scores) == 0L) {
                    next
                }
                scored <- scored[scores == max(scores)]
                primary_grid <- if (nrow(required_partitions)) {
                    required_partitions$grid_label[[1L]]
                } else {
                    NA_character_
                }
                preferred <- vapply(
                    scored,
                    function(value) {
                        if (
                            !is.na(primary_grid) &&
                                identical(value$grid, primary_grid)
                        ) {
                            return(1L)
                        }
                        if (identical(value$grid, "gn")) 2L else 3L
                    },
                    integer(1L)
                )
                chosen <- scored[[order(
                    preferred,
                    vapply(scored, `[[`, character(1L), "grid")
                )[[1L]]]]
                optional_partitions[[length(optional_partitions) + 1L]] <-
                    data.table::data.table(
                        variable_id = chosen$variables,
                        frequency = wanted_frequency,
                        table_id = wanted_table_id,
                        grid_label = chosen$grid,
                        required = FALSE
                    )
            }
            partitions <- data.table::rbindlist(
                c(list(required_partitions), optional_partitions),
                use.names = TRUE,
                fill = TRUE
            )
            partitions <- unique(
                partitions,
                by = c(
                    "variable_id",
                    "frequency",
                    "table_id",
                    "grid_label"
                )
            )
            required_grid_rows <- unique(required_partitions[, .(
                frequency,
                table_id,
                grid_label
            )])
            data.table::setorderv(
                required_grid_rows,
                c("frequency", "table_id", "grid_label")
            )
            required_partition_key <- paste(
                paste(
                    shift__cmip6_partition_id(
                        required_grid_rows$frequency,
                        required_grid_rows$table_id
                    ),
                    required_grid_rows$grid_label,
                    sep = "="
                ),
                collapse = ";"
            )
            all_grid_rows <- unique(partitions[, .(
                frequency,
                table_id,
                grid_label
            )])
            data.table::setorderv(
                all_grid_rows,
                c("frequency", "table_id", "grid_label")
            )
            partition_key <- paste(
                paste(
                    shift__cmip6_partition_id(
                        all_grid_rows$frequency,
                        all_grid_rows$table_id
                    ),
                    all_grid_rows$grid_label,
                    sep = "="
                ),
                collapse = ";"
            )
            requirement_key <- paste(
                vapply(
                    names(selected_sources),
                    function(canonical) {
                        sprintf(
                            "%s=%s",
                            canonical,
                            paste(selected_sources[[canonical]], collapse = "+")
                        )
                    },
                    character(1L)
                ),
                collapse = ";"
            )
            primary_grid <- required_partitions$grid_label[[1L]]
            display_tables <- sort(unique(partitions$table_id))
            rows[[length(rows) + 1L]] <- data.table::data.table(
                source_id = identity$source_id[[1L]],
                variant_label = identity$variant_label[[1L]],
                grid_label = primary_grid,
                frequency = paste(
                    unique(unname(frequency_map)),
                    collapse = "+"
                ),
                table_id = paste(display_tables, collapse = "+"),
                required_partition_key = required_partition_key,
                requirement_key = requirement_key,
                partition_key = partition_key,
                partitions_json = shift__cmip6_partition_json(partitions),
                required_native_grid = all(
                    !is.na(required_grid_rows$grid_label) &
                        required_grid_rows$grid_label == "gn"
                ),
                all_native_grid = all(
                    !is.na(all_grid_rows$grid_label) &
                        all_grid_rows$grid_label == "gn"
                ),
                complete = !length(missing),
                missing = if (length(missing)) {
                    paste(unique(missing), collapse = "; ")
                } else {
                    NA_character_
                }
            )
        }
    }
    if (!length(rows)) {
        empty
    } else {
        data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    }
}

# Build the File query shared by batch discovery and high-level CMIP6
# resolution when exact future or historical year coverage must be verified.
shift__cmip6_coverage_request <- function(
    climate,
    transform,
    variables,
    frequency,
    table,
    sources,
    members,
    periods,
    node,
    reference = NULL
) {
    historical <- S7::S7_inherits(reference, ShiftReferenceSpec) &&
        identical(reference@mode, "historical")
    experiments <- if (historical) {
        reference@experiment
    } else {
        climate@scenarios
    }
    activity <- if (historical) reference@activity else climate@activity
    filters <- if (historical) {
        # Future-only climate filters must not override an explicit historical
        # reference activity or experiment.
        reference@filters
    } else {
        climate@filters
    }
    options <- if (historical) reference@options else list()
    options <- utils::modifyList(
        options,
        list(
            index_node = node,
            time_filter_method = "auto",
            file_time = shift__method_time_window(
                periods,
                transform__recipe(transform)
            )
        )
    )

    # Monthly Dataset timestamps often use representative mid-month values,
    # so exact dates are applied to File records rather than the Dataset query.
    shift_cmip6_scenario(
        source = unique(as.character(sources)),
        scenario = experiments,
        member = unique(as.character(members)),
        years = NULL,
        variables = variables,
        frequency = frequency,
        activity = activity,
        table_id = unique(unname(table)),
        grid_label = NULL,
        data_node = climate@data_node,
        index_node = node,
        filters = filters,
        options = options
    )
}

# Collect one coverage catalog and close its short-lived store connection
# before another table group or index node is evaluated.
shift__cmip6_coverage_catalog <- function(request, store, ui, label) {
    # Discovery owns the reporter, so the standalone task wrapper no longer
    # owns this connection. Close locally opened stores on every exit path.
    file_store <- shift_store(store, create = TRUE)
    if (!inherits(store, "EsgStore")) {
        on.exit(file_store$close(), add = TRUE)
    }
    files <- shift_collect(
        request,
        store = file_store,
        fields = SHIFT_WORKFLOW_FILE_FIELDS,
        all = TRUE,
        limit = FALSE,
        label = label,
        ui = ui
    )
    shift_file_catalog(file_store, files@ids$query_id)
}

# Count the distinct logical files needed by one complete CMIP6 candidate after
# applying its exact variable/table/grid partitions and requested year union.
# This is an execution-cost hint only; it never relaxes scientific coverage.
shift__cmip6_candidate_file_count <- function(
    catalog,
    candidate,
    experiments,
    years
) {
    partitions <- shift__cmip6_partitions(candidate$partitions_json[[1L]])
    data.table::set(
        partitions,
        j = "source_id",
        value = rep(candidate$source_id[[1L]], nrow(partitions))
    )
    data.table::set(
        partitions,
        j = "variant_label",
        value = rep(candidate$variant_label[[1L]], nrow(partitions))
    )
    catalog <- shift__catalog_current(catalog)
    keep <- shift__partition_row_match(catalog, partitions, experiments) &
        shift__file_year_match(catalog, years)
    files <- catalog[keep]
    if (!nrow(files)) {
        return(0L)
    }
    logical_ids <- tryCatch(
        store__logical_file_id(files),
        error = function(error) NULL
    )
    if (!is.null(logical_ids)) {
        return(as.integer(data.table::uniqueN(logical_ids)))
    }
    # Synthetic adapters may omit all provenance identifiers. Their exact
    # partition and time tuple is sufficient for a deterministic cost hint.
    fields <- intersect(
        c(
            "source_id",
            "experiment_id",
            "variant_label",
            "frequency",
            "table_id",
            "variable_id",
            "grid_label",
            "datetime_start",
            "datetime_end"
        ),
        names(files)
    )
    as.integer(nrow(unique(files[, fields, with = FALSE])))
}

# Resolve one exact File request to complete identities and file-count costs.
# A caller-owned cache shares both catalog I/O and reduction across methods;
# copies keep subsequent historical joins from modifying cached evidence.
shift__cmip6_coverage_candidates <- function(
    request,
    years,
    table,
    store,
    ui,
    cache = NULL
) {
    key <- if (!is.null(cache)) {
        store__hash(shift__spec_json(list(
            request = request@meta,
            years = sort(unique(years)),
            table = table
        )))
    }
    if (!is.null(cache) && !is.null(cache[[key]])) {
        return(data.table::copy(cache[[key]]))
    }
    meta <- request@meta
    catalog <- shift__cmip6_coverage_catalog(
        request,
        store,
        ui,
        "batch-file-coverage"
    )
    candidates <- shift__cmip6_candidates(
        catalog,
        models = meta$source,
        experiments = meta$experiment,
        variables = meta$variables,
        years = years,
        frequency = meta$frequency,
        table = table,
        requirements = stats::setNames(as.list(meta$variables), meta$variables)
    )
    candidates <- candidates[complete %in% TRUE]
    counts <- vapply(
        seq_len(nrow(candidates)),
        function(index) {
            shift__cmip6_candidate_file_count(
                catalog,
                candidates[index],
                meta$experiment,
                years
            )
        },
        integer(1L)
    )
    data.table::set(candidates, j = "source_file_count", value = counts)
    # Failed queries never become cached empty results. Successful zero-row
    # results are reusable because the entire request and year union are keyed.
    if (!is.null(cache)) {
        cache[[key]] <- data.table::copy(candidates)
    }
    candidates
}

# Reduce Dataset-level candidates to model/member/grid identities whose File
# records cover every required future and automatic historical calendar year.
shift__cmip6_period_coverage <- function(
    candidates,
    climate,
    transform,
    variables,
    frequency,
    periods,
    reference,
    node,
    store,
    ui,
    cache = NULL
) {
    candidates <- data.table::as.data.table(data.table::copy(candidates))
    if (!nrow(candidates)) {
        return(candidates)
    }
    # Named lists preserve singleton variable names during map matching.
    mappings <- lapply(candidates$table, as.list)
    distinct <- unique(mappings)
    keys <- vapply(distinct, shift__spec_json, character(1L))
    candidate_groups <- split(
        seq_len(nrow(candidates)),
        keys[match(mappings, distinct)]
    )
    complete_rows <- rep(FALSE, nrow(candidates))
    source_file_count <- rep(NA_integer_, nrow(candidates))

    for (group_index in seq_along(candidate_groups)) {
        candidate_rows <- candidate_groups[[group_index]]
        rows <- candidates[candidate_rows]
        table <- rows$table[[1L]]
        future_request <- shift__cmip6_coverage_request(
            climate,
            transform,
            variables,
            frequency,
            table,
            rows$source_id,
            rows$variant_label,
            periods,
            node
        )
        shift_batch__discovery_update(
            list(
                scope = "Future coverage",
                scope_periods = shift__ui_periods(periods)
            ),
            reset = TRUE
        )
        future <- shift__cmip6_coverage_candidates(
            future_request,
            periods$year,
            table,
            store,
            ui,
            cache
        )
        group_keys <- paste(
            future$source_id,
            future$variant_label,
            future$grid_label,
            sep = "\r"
        )

        if (
            S7::S7_inherits(reference, ShiftReferenceSpec) &&
                identical(reference@mode, "historical")
        ) {
            historical_request <- shift__cmip6_coverage_request(
                climate,
                transform,
                variables,
                frequency,
                table,
                rows$source_id,
                rows$variant_label,
                reference@periods,
                node,
                reference = reference
            )
            shift_batch__discovery_update(
                list(
                    scope = "Historical coverage",
                    scope_periods = shift__ui_periods(reference@periods)
                ),
                reset = TRUE
            )
            historical <- shift__cmip6_coverage_candidates(
                historical_request,
                reference@periods$year,
                table,
                store,
                ui,
                cache
            )
            historical_keys <- paste(
                historical$source_id,
                historical$variant_label,
                historical$grid_label,
                sep = "\r"
            )
            group_keys <- intersect(group_keys, historical_keys)
            historical_counts <- historical[,
                .(
                    historical_file_count = if (.N) {
                        min(source_file_count)
                    } else {
                        NA_integer_
                    }
                ),
                by = .(source_id, variant_label, grid_label)
            ]
            future <- merge(
                future,
                historical_counts,
                by = c("source_id", "variant_label", "grid_label"),
                all.x = TRUE,
                sort = FALSE
            )
            future[,
                source_file_count := .SD[["source_file_count"]] +
                    .SD[["historical_file_count"]],
                .SDcols = c("source_file_count", "historical_file_count")
            ]
        }
        row_keys <- paste(
            rows$source_id,
            rows$variant_label,
            rows$grid_label,
            sep = "\r"
        )
        # Retain completion on the exact table-mapping group. A valid daily
        # candidate must not make an Amon candidate with the same model/member/
        # grid identity appear complete.
        complete_rows[candidate_rows] <- row_keys %in% group_keys
        counts <- future[,
            .(
                source_file_count = if (.N) {
                    min(source_file_count)
                } else {
                    NA_integer_
                }
            ),
            by = .(source_id, variant_label, grid_label)
        ]
        count_keys <- paste(
            counts$source_id,
            counts$variant_label,
            counts$grid_label,
            sep = "\r"
        )
        source_file_count[candidate_rows] <- counts$source_file_count[
            match(row_keys, count_keys)
        ]
    }

    candidates <- candidates[complete_rows]
    candidates[, source_file_count := source_file_count[complete_rows]]
    candidates[,
        identity := paste(
            source_id,
            variant_label,
            grid_label,
            sep = "\r"
        )
    ]
    candidates[]
}

# Apply explicit selection constraints and the locked r1i1p1f1/gn preference;
# unresolved ties are structural ambiguities and must be shown to the user.
shift__choose_cmip6_candidates <- function(
    candidates,
    models,
    member = NULL,
    grid = NULL,
    diagnostic = NULL
) {
    candidates <- candidates[complete %in% TRUE]
    if (!is.null(member)) {
        candidates <- candidates[variant_label %in% member]
    }
    if (!is.null(grid)) {
        candidates <- candidates[grid_label %in% grid]
    }
    selected <- list()
    for (model in models) {
        available <- candidates[source_id == model]
        if (!nrow(available)) {
            if (!is.null(diagnostic)) {
                diagnostic$reason <- "selection_incomplete"
                diagnostic$summary <- sprintf(
                    "No complete CMIP6 member/grid candidate satisfies the selection for model %s.",
                    model
                )
                explicit <- c(
                    if (!is.null(member)) paste("member", member),
                    if (!is.null(grid)) paste("grid", grid)
                )
                if (length(explicit)) {
                    diagnostic$missing <- c(
                        paste(
                            "explicit selection unavailable:",
                            paste(explicit, collapse = ", ")
                        ),
                        diagnostic$missing
                    )
                }
                shift__abort_cmip6_resolution(diagnostic)
            }
            cli::cli_abort(
                "No complete CMIP6 member/grid candidate was found for model {.val {model}}."
            )
        }
        if (!is.null(member)) {
            missing_members <- setdiff(member, unique(available$variant_label))
            if (length(missing_members)) {
                cli::cli_abort(
                    "Explicit member(s) are incomplete for model {.val {model}}: {.val {missing_members}}."
                )
            }
            common_partitions <- Reduce(
                intersect,
                lapply(member, function(value) {
                    unique(
                        available[variant_label == value]$required_partition_key
                    )
                })
            )
            common_partitions <- common_partitions[
                !is.na(common_partitions) & nzchar(common_partitions)
            ]
            if (is.null(grid) && length(common_partitions) > 1L) {
                native <- common_partitions[vapply(
                    common_partitions,
                    function(value) {
                        all(grepl(
                            "=gn$",
                            strsplit(
                                value,
                                ";",
                                fixed = TRUE
                            )[[1L]]
                        ))
                    },
                    logical(1L)
                )]
                if (length(native)) {
                    common_partitions <- native
                }
            }
            if (length(common_partitions) != 1L) {
                cli::cli_abort(
                    c(
                        "CMIP6 table/grid selection is ambiguous for model {.val {model}} and explicit member(s) {.val {member}}.",
                        "i" = "Candidate partitions: {.val {common_partitions}}. Set `grid` explicitly."
                    ),
                    class = "epwshiftr_shift_resolution_ambiguity"
                )
            }
            selected[[model]] <- available[
                variant_label %in%
                    member &
                    required_partition_key == common_partitions[[1L]]
            ]
            next
        }

        if (any(available$variant_label %in% "r1i1p1f1")) {
            available <- available[variant_label == "r1i1p1f1"]
        }
        if (is.null(grid) && any(available$required_native_grid %in% TRUE)) {
            available <- available[required_native_grid %in% TRUE]
        }
        if ("case_count" %in% names(available) && nrow(available)) {
            available <- available[case_count == max(case_count)]
        }
        if (nrow(available) != 1L) {
            labels <- sprintf(
                "%s/%s",
                available$variant_label,
                available$partition_key
            )
            cli::cli_abort(
                c(
                    "CMIP6 member/grid selection is ambiguous for model {.val {model}}.",
                    "i" = "Candidates: {.val {labels}}. Set `member` and/or `grid` explicitly."
                ),
                class = "epwshiftr_shift_resolution_ambiguity"
            )
        }
        selected[[model]] <- available
    }
    data.table::rbindlist(selected, use.names = TRUE, fill = TRUE)[,
        missing := NULL
    ][]
}

# For partial-enabled runs, retain identities that cover at least one complete
# scenario and record how many requested scenarios each identity can fulfil.
shift__cmip6_partial_candidates <- function(
    catalog,
    models,
    experiments,
    variables,
    years,
    frequency,
    table,
    requirements = NULL,
    grid = NULL
) {
    parts <- lapply(experiments, function(experiment) {
        rows <- shift__cmip6_candidates(
            catalog,
            models = models,
            experiments = experiment,
            variables = variables,
            years = years,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
        rows[, requested_experiment := experiment]
        rows
    })
    rows <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    if (!nrow(rows)) {
        return(rows)
    }
    rows[,
        .(
            complete = any(complete %in% TRUE),
            case_count = sum(complete %in% TRUE),
            missing = paste(stats::na.omit(missing), collapse = "; ")
        ),
        by = .(
            source_id,
            variant_label,
            grid_label,
            frequency,
            table_id,
            required_partition_key,
            requirement_key,
            partition_key,
            partitions_json,
            required_native_grid,
            all_native_grid
        )
    ]
}

# Split the candidate contract into individual missing requirements while
# preserving the exact scenario/variable/year phrases produced by the resolver.
shift__cmip6_missing_items <- function(value) {
    value <- as.character(shift_coalesce(value, character()))
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) {
        return(character())
    }
    trimws(unlist(strsplit(value, ";", fixed = TRUE), use.names = FALSE))
}

# Build one structured explanation before complete candidate tables are
# filtered or intersected. This keeps the closest identity and exact missing
# requirements available to the terminal UI, persisted events, and callers.
shift__cmip6_resolution_diagnostic <- function(
    future,
    reference = NULL,
    models,
    reference_required = FALSE
) {
    identity <- c(
        "source_id",
        "variant_label",
        "frequency",
        "required_partition_key",
        "requirement_key"
    )
    display <- c("grid_label", "table_id")
    future <- data.table::as.data.table(data.table::copy(future))
    for (name in setdiff(
        c(identity, display, "complete", "missing"),
        names(future)
    )) {
        future[[name]] <- if (identical(name, "complete")) {
            logical(nrow(future))
        } else {
            rep(NA_character_, nrow(future))
        }
    }
    future <- future[,
        c(identity, display, "complete", "missing"),
        with = FALSE
    ]
    data.table::setnames(
        future,
        c(display, "complete", "missing"),
        c(
            "future_grid_label",
            "future_table_id",
            "future_complete",
            "future_missing"
        )
    )

    if (isTRUE(reference_required)) {
        reference <- data.table::as.data.table(data.table::copy(reference))
        for (name in setdiff(
            c(identity, display, "complete", "missing"),
            names(reference)
        )) {
            reference[[name]] <- if (identical(name, "complete")) {
                logical(nrow(reference))
            } else {
                rep(NA_character_, nrow(reference))
            }
        }
        reference <- reference[,
            c(identity, display, "complete", "missing"),
            with = FALSE
        ]
        data.table::setnames(
            reference,
            c(display, "complete", "missing"),
            c(
                "reference_grid_label",
                "reference_table_id",
                "reference_complete",
                "reference_missing"
            )
        )
        combined <- merge(
            future,
            reference,
            by = identity,
            all = TRUE,
            sort = FALSE
        )
    } else {
        combined <- data.table::copy(future)
        combined[, `:=`(
            reference_grid_label = future_grid_label,
            reference_table_id = future_table_id,
            reference_complete = TRUE,
            reference_missing = NA_character_
        )]
    }

    future_complete <- sum(future$future_complete %in% TRUE)
    reference_complete <- if (isTRUE(reference_required)) {
        sum(reference$reference_complete %in% TRUE)
    } else {
        NA_integer_
    }
    shared_complete <- sum(
        combined$future_complete %in%
            TRUE &
            combined$reference_complete %in% TRUE
    )
    reason <- if (!future_complete) {
        "future_incomplete"
    } else if (isTRUE(reference_required) && !reference_complete) {
        "reference_incomplete"
    } else if (isTRUE(reference_required) && !shared_complete) {
        "no_shared_identity"
    } else {
        "selection_incomplete"
    }
    summary <- switch(
        reason,
        future_incomplete = paste(
            "No member/grid covers all requested future scenarios,",
            "variables, and years."
        ),
        reference_incomplete = paste(
            "No historical member/grid covers all reference variables",
            "and years."
        ),
        no_shared_identity = paste(
            "Future and historical catalogs have no complete member/grid",
            "identity in common."
        ),
        "No complete candidate satisfies the requested member/grid selection."
    )

    # Rank the most useful near-match from identities that actually exist in
    # the future catalog before comparing missing contract counts. A
    # reference-only identity must never appear closer merely because its
    # entire absent future side collapses to one generic diagnostic item.
    closest <- NULL
    missing <- character()
    if (nrow(combined)) {
        # Use explicit column access here because these temporary diagnostic
        # columns are local implementation details, not package-level
        # data.table symbols that should be registered as global variables.
        combined[["future_items"]] <- lapply(
            seq_len(nrow(combined)),
            function(i) {
                if (is.na(combined[["future_complete"]][[i]])) {
                    "future: identity unavailable"
                } else if (isTRUE(combined[["future_complete"]][[i]])) {
                    character()
                } else {
                    paste0(
                        "future: ",
                        shift__cmip6_missing_items(
                            combined[["future_missing"]][[i]]
                        )
                    )
                }
            }
        )
        combined[["reference_items"]] <- lapply(
            seq_len(nrow(combined)),
            function(i) {
                if (!isTRUE(reference_required)) {
                    character()
                } else if (is.na(combined[["reference_complete"]][[i]])) {
                    "reference: identity unavailable"
                } else if (isTRUE(combined[["reference_complete"]][[i]])) {
                    character()
                } else {
                    paste0(
                        "reference: ",
                        shift__cmip6_missing_items(
                            combined[["reference_missing"]][[i]]
                        )
                    )
                }
            }
        )
        combined[["missing_count"]] <- lengths(combined[["future_items"]]) +
            lengths(combined[["reference_items"]])
        combined[["future_available"]] <-
            !is.na(combined[["future_complete"]])
        combined[["shared_available"]] <-
            combined[["future_available"]] &
            (!isTRUE(reference_required) |
                !is.na(combined[["reference_complete"]]))
        combined[["preferred_member"]] <-
            combined[["variant_label"]] %in% "r1i1p1f1"
        combined[["preferred_grid"]] <- vapply(
            combined[["required_partition_key"]],
            function(value) {
                if (is.na(value) || !nzchar(value)) {
                    return(FALSE)
                }
                all(grepl("=gn$", strsplit(value, ";", fixed = TRUE)[[1L]]))
            },
            logical(1L)
        )
        data.table::setorderv(
            combined,
            c(
                "future_available",
                "shared_available",
                "missing_count",
                "preferred_member",
                "preferred_grid",
                "source_id",
                "variant_label",
                "required_partition_key"
            ),
            order = c(-1L, -1L, 1L, -1L, -1L, 1L, 1L, 1L),
            na.last = TRUE
        )
        row <- combined[1L]
        missing <- c(row$future_items[[1L]], row$reference_items[[1L]])
        closest_grid <- row$future_grid_label[[1L]]
        if (is.na(closest_grid) || !nzchar(closest_grid)) {
            closest_grid <- row$reference_grid_label[[1L]]
        }
        closest_table <- row$future_table_id[[1L]]
        if (is.na(closest_table) || !nzchar(closest_table)) {
            closest_table <- row$reference_table_id[[1L]]
        }
        closest <- list(
            model = as.character(row$source_id[[1L]]),
            member = as.character(row$variant_label[[1L]]),
            grid = as.character(closest_grid),
            frequency = as.character(row$frequency[[1L]]),
            table = as.character(closest_table),
            partitions = as.character(row$required_partition_key[[1L]])
        )
    }
    list(
        kind = "coverage",
        reason = reason,
        summary = summary,
        models = as.character(models),
        future_complete_candidates = as.integer(future_complete),
        reference_complete_candidates = as.integer(reference_complete),
        shared_complete_candidates = as.integer(shared_complete),
        closest = closest,
        missing = missing
    )
}

# Raise a typed resolver condition whose concise message remains useful in log
# mode while its structured fields drive the final dashboard and recovery text.
shift__abort_cmip6_resolution <- function(diagnostic) {
    closest <- diagnostic$closest
    closest_label <- if (is.null(closest)) {
        "No near-match identity was available."
    } else {
        sprintf(
            "Closest identity: %s/%s/%s.",
            shift_coalesce(closest$model, "?"),
            shift_coalesce(closest$member, "?"),
            shift_coalesce(closest$grid, "?")
        )
    }
    missing <- utils::head(diagnostic$missing, 3L)
    cli::cli_abort(
        c(
            diagnostic$summary,
            "i" = closest_label,
            if (length(missing)) c("x" = missing)
        ),
        class = c(
            "epwshiftr_shift_resolution_incomplete",
            "epwshiftr_shift_resolution_error"
        ),
        resolution = diagnostic,
        call = NULL
    )
}

# Intersect optional future/reference variables on their exact table and grid
# while retaining each side's required rows. This makes optional SND and
# extrema available only when both periods can support the same calculation.
shift__cmip6_shared_partitions <- function(future, reference) {
    future <- shift__cmip6_partitions(future)
    reference <- shift__cmip6_partitions(reference)
    keys <- c("variable_id", "frequency", "table_id", "grid_label")
    future_required <- future[required %in% TRUE]
    reference_required <- reference[required %in% TRUE]
    shared_optional <- merge(
        future[required %in% FALSE],
        reference[required %in% FALSE],
        by = keys,
        all = FALSE,
        sort = FALSE
    )
    shared_optional <- if (nrow(shared_optional)) {
        shared_optional[, c(keys), with = FALSE][, required := FALSE]
    } else {
        future[0L]
    }
    list(
        future = unique(data.table::rbindlist(
            list(future_required, shared_optional),
            use.names = TRUE,
            fill = TRUE
        )),
        reference = unique(data.table::rbindlist(
            list(reference_required, shared_optional),
            use.names = TRUE,
            fill = TRUE
        ))
    )
}

# Recompute display fields after optional partitions have been intersected.
# Required frequency/table/grid partitions remain the selection identity.
shift__cmip6_partition_summary <- function(partitions) {
    partitions <- data.table::as.data.table(partitions)
    grids <- unique(partitions[, .(frequency, table_id, grid_label)])
    data.table::setorderv(
        grids,
        c("frequency", "table_id", "grid_label")
    )
    required_grids <- unique(partitions[
        required %in% TRUE,
        .(frequency, table_id, grid_label)
    ])
    data.table::setorderv(
        required_grids,
        c("frequency", "table_id", "grid_label")
    )
    list(
        grid_label = required_grids$grid_label[[1L]],
        table_id = paste(sort(unique(partitions$table_id)), collapse = "+"),
        required_partition_key = paste(
            paste(
                shift__cmip6_partition_id(
                    required_grids$frequency,
                    required_grids$table_id
                ),
                required_grids$grid_label,
                sep = "="
            ),
            collapse = ";"
        ),
        partition_key = paste(
            paste(
                shift__cmip6_partition_id(
                    grids$frequency,
                    grids$table_id
                ),
                grids$grid_label,
                sep = "="
            ),
            collapse = ";"
        ),
        required_native_grid = all(required_grids$grid_label == "gn"),
        all_native_grid = all(grids$grid_label == "gn")
    )
}

# Resolve future and, only when explicitly requested by the method, historical
# catalogs against one shared model/member identity and a matching grid for
# every required frequency/table partition.
shift__resolve_cmip6_selection <- function(
    plan,
    future_catalog,
    reference_catalog = NULL
) {
    meta <- plan@meta
    request <- meta$request@meta
    climate <- meta$climate
    models <- if (is.null(climate)) {
        as.character(request$source)
    } else {
        climate@model
    }
    scenarios <- if (is.null(climate)) {
        as.character(request$experiment)
    } else {
        climate@scenarios
    }
    requirements <- morpher__variable_requirements(meta$recipe)
    variables <- morpher__input_variables(meta$recipe)
    member <- if (is.null(climate)) request$variant else climate@member
    grid <- if (is.null(climate)) request$filters$grid_label else climate@grid
    frequency <- if (is.null(climate) || is.null(climate@frequency)) {
        request$frequency
    } else {
        climate@frequency
    }
    table <- if (is.null(climate)) {
        shift__cmip6_request_table_spec(request$filters$table_id)
    } else {
        climate@table
    }
    future <- if (isTRUE(meta$control@allow_partial)) {
        shift__cmip6_partial_candidates(
            future_catalog,
            models = models,
            experiments = scenarios,
            variables = variables,
            years = meta$periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
    } else {
        shift__cmip6_candidates(
            future_catalog,
            models = models,
            experiments = scenarios,
            variables = variables,
            years = meta$periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
    }

    reference <- meta$reference
    if (
        S7::S7_inherits(reference, ShiftReferenceSpec) &&
            identical(reference@mode, "historical")
    ) {
        # Monthly CMIP datasets usually end at a representative timestamp such
        # as December 16, not at the last second of the calendar year. An empty
        # reference result therefore needs its own diagnosis instead of being
        # collapsed into the later member/grid intersection error.
        if (is.null(reference_catalog) || !nrow(reference_catalog)) {
            year_range <- range(reference@periods$year)
            activity_label <- shift_coalesce(
                reference@activity,
                "<any activity>"
            )
            frequency_label <- paste(
                shift_coalesce(frequency, "<any frequency>"),
                collapse = ", "
            )
            table_label <- if (is.null(climate)) {
                paste(shift_coalesce(table, "<any table>"), collapse = ", ")
            } else {
                paste(
                    unique(unname(shift__cmip6_variable_tables(
                        variables,
                        frequency,
                        table
                    ))),
                    collapse = ", "
                )
            }
            cli::cli_abort(
                c(
                    "Historical reference catalog is empty for model(s) {.val {models}}.",
                    "x" = paste0(
                        "No File records matched experiment ",
                        reference@experiment,
                        ", activity ",
                        activity_label,
                        ", frequency ",
                        frequency_label,
                        ", table ",
                        table_label,
                        "."
                    ),
                    "i" = sprintf(
                        "Requested reference years: %d\u2013%d.",
                        year_range[[1L]],
                        year_range[[2L]]
                    )
                ),
                class = "epwshiftr_shift_reference_catalog_empty"
            )
        }
        historical_candidates <- shift__cmip6_candidates(
            reference_catalog,
            models = models,
            experiments = reference@experiment,
            variables = variables,
            years = reference@periods$year,
            frequency = frequency,
            table = table,
            requirements = requirements,
            grid = grid
        )
        diagnostic <- shift__cmip6_resolution_diagnostic(
            future,
            reference = historical_candidates,
            models = models,
            reference_required = TRUE
        )
        if (!diagnostic$shared_complete_candidates) {
            shift__abort_cmip6_resolution(diagnostic)
        }
        identity <- c(
            "source_id",
            "variant_label",
            "frequency",
            "required_partition_key",
            "requirement_key"
        )
        historical <- historical_candidates[
            complete %in% TRUE,
            c(identity, "partitions_json"),
            with = FALSE
        ]
        data.table::setnames(
            historical,
            "partitions_json",
            "reference_partitions_json"
        )
        future <- merge(
            future[complete %in% TRUE],
            historical,
            by = identity,
            all = FALSE,
            sort = FALSE
        )
        if (nrow(future)) {
            for (i in seq_len(nrow(future))) {
                shared <- shift__cmip6_shared_partitions(
                    future$partitions_json[[i]],
                    future$reference_partitions_json[[i]]
                )
                future$partitions_json[[i]] <-
                    shift__cmip6_partition_json(shared$future)
                future$reference_partitions_json[[i]] <-
                    shift__cmip6_partition_json(shared$reference)
                summary <- shift__cmip6_partition_summary(shared$future)
                for (name in names(summary)) {
                    future[[name]][[i]] <- summary[[name]]
                }
            }
        }
    } else {
        diagnostic <- shift__cmip6_resolution_diagnostic(
            future,
            models = models,
            reference_required = FALSE
        )
        if (!diagnostic$shared_complete_candidates) {
            shift__abort_cmip6_resolution(diagnostic)
        }
        future[, reference_partitions_json := NA_character_]
    }
    selected <- shift__choose_cmip6_candidates(
        future,
        models,
        member = member,
        grid = grid,
        diagnostic = diagnostic
    )
    selected[, future_partitions_json := partitions_json]
    selected[]
}

# Clone a request with a specific index node while preserving every scientific
# filter and time constraint.
shift__request_at_node <- function(request, node) {
    meta <- request@meta
    options <- meta$options
    options$index_node <- node
    shift_request(
        provider = meta$provider,
        project = meta$project,
        source = meta$source,
        experiment = meta$experiment,
        variant = meta$variant,
        variables = meta$variables,
        frequency = meta$frequency,
        time = meta$time,
        filters = meta$filters,
        options = options
    )
}

# Build a historical request only for an explicit historical reference spec;
# manual plan and ShiftClimate references never reach this function.
shift__historical_request <- function(plan, node) {
    meta <- plan@meta
    reference <- meta$reference
    if (
        !S7::S7_inherits(reference, ShiftReferenceSpec) ||
            !identical(reference@mode, "historical")
    ) {
        return(NULL)
    }
    request <- meta$request@meta
    climate <- meta$climate
    member <- if (is.null(climate)) request$variant else climate@member
    grid <- if (is.null(climate)) request$filters$grid_label else climate@grid
    variables <- morpher__input_variables(meta$recipe)
    frequency <- if (is.null(climate) || is.null(climate@frequency)) {
        request$frequency
    } else {
        climate@frequency
    }
    tables <- if (is.null(climate)) {
        as.character(request$filters$table_id)
    } else {
        unique(unname(shift__cmip6_variable_tables(
            variables,
            frequency,
            climate@table
        )))
    }
    filters <- utils::modifyList(
        shift__compact_list(list(
            activity_id = reference@activity,
            table_id = tables,
            grid_label = grid,
            data_node = if (is.null(climate)) {
                request$filters$data_node
            } else {
                climate@data_node
            }
        )),
        reference@filters
    )
    shift_request(
        provider = request$provider,
        project = request$project,
        source = request$source,
        experiment = reference@experiment,
        variant = member,
        variables = variables,
        frequency = frequency,
        # Do not turn calendar-year intent into exact Dataset datetime bounds.
        # CMIP monthly metadata commonly ends on December 16, so requiring a
        # stop at December 31 incorrectly removes otherwise complete datasets.
        # Reference periods remain authoritative in candidate selection,
        # extraction planning, coverage checks, and the persisted method spec.
        time = NULL,
        filters = filters,
        # Keep exact reference dates out of the Dataset query, but use them to
        # select File records after filling missing ranges from DRS filenames.
        options = utils::modifyList(
            reference@options,
            list(
                index_node = node,
                time_filter_method = "auto",
                file_time = shift__method_time_window(
                    reference@periods,
                    meta$recipe
                )
            )
        )
    )
}

# Recreate a ShiftFiles stage from a pinned query ID during resume without
# contacting an ESGF node or changing the resolved member/grid choice.
shift__files_from_query <- function(store, request, query_id) {
    catalog <- shift_file_catalog(store, query_id)
    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(
            request = request,
            dataset_count = NA_integer_,
            datasets = NULL,
            file_count = nrow(catalog),
            variables = unique(catalog$variable_id),
            fields = SHIFT_WORKFLOW_FILE_FIELDS
        )
    )
}

# Resolve usable service endpoints for one collected File stage before its
# catalog is used for scientific selection or extraction. Each service is
# selected independently so a recoverable HTTP path cannot replace an OPeNDAP
# URL chosen from another compatible replica.
shift__resolve_file_services <- function(
    files,
    role,
    reporter = NULL,
    refresh = FALSE
) {
    checkmate::assert_string(role, min.chars = 1L)
    checkmate::assert_flag(refresh)
    store <- shift_store(files)
    on.exit(store$close(), add = TRUE)
    result <- shift_stage_query_result(
        store,
        files@ids$query_id,
        result_type = "File"
    )
    resolve <- query_result__resolve_file_services
    resolved <- resolve(
        result,
        index_node = NULL,
        check = list(
            level = "url",
            timeout = 5,
            concurrency = 32L,
            # Explicit refresh bypasses both successful and failed endpoint
            # health entries; ordinary runs retain the shared performance
            # cache used by adjacent method children.
            cache_seconds = if (isTRUE(refresh)) 0L else 3600L,
            # A failed endpoint is retained for the package cache lifetime so
            # adjacent method children do not repeat the same timeout.
            cache_failures_seconds = if (isTRUE(refresh)) 0L else 1800L
        )
    )
    if (
        !is.list(resolved) ||
            !inherits(resolved$result, "EsgResultFile") ||
            !data.table::is.data.table(resolved$diagnostics)
    ) {
        cli::cli_abort(
            "ESGF file-service resolver must return a File result and diagnostics."
        )
    }
    result <- resolved$result
    checks <- resolved$diagnostics
    query_id <- store$add_files(
        result,
        label = sprintf("resolved-%s-services", role)
    )
    rows <- result$to_data_table()
    if (!is.null(reporter)) {
        available <- checks[,
            .(
                available = sum(.SD[["selected"]] %in% TRUE),
                unavailable = sum(!(.SD[["selected"]] %in% TRUE))
            ),
            by = "service",
            .SDcols = "selected"
        ]
        reporter$notice(
            sprintf(
                "Resolved service candidates for %d %s file(s)",
                nrow(rows),
                role
            ),
            outcome = "completed",
            details = list(
                unit_type = "catalog",
                catalog_role = role,
                files = nrow(rows),
                services = as.data.frame(available)
            )
        )
    }
    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = files@store_path,
        ids = list(query_id = query_id),
        meta = list(
            request = files@meta$request,
            dataset_count = files@meta$dataset_count,
            datasets = NULL,
            file_count = nrow(rows),
            variables = unique(as.character(rows$variable_id)),
            fields = files@meta$fields,
            result_fields = result$fields
        )
    )
}

# Render a stable node name inside messages. Debug renderers obtain the full URL
# from structured event details, avoiding duplicated label-plus-URL text.
shift__report_node <- function(reporter, node) {
    shift__node_label(node)
}

# Install a query callback only for the duration of one catalog collection.
# This keeps low-level EsgQuery APIs independent of workflow reporter classes.
shift__with_query_reporter <- function(reporter, query, phase, expr) {
    if (is.null(reporter)) {
        return(force(expr))
    }
    node <- if (inherits(query, "EsgQuery")) {
        query$index_node()
    } else {
        priv(query)$index_node
    }
    started <- as.numeric(Sys.time())
    last_response <- NULL
    responses <- 0L
    cache_hits <- 0L
    records <- 0L
    # libcurl's download vector contains total bytes and bytes received. Count
    # responses only after successful parsing, including empty count queries.
    callback <- function(progress) {
        state <- shift_coalesce(progress$state, "transfer")
        now <- as.numeric(Sys.time())
        if (state %in% c("started", "cached")) {
            started <<- now
        }
        if (state %in% c("completed", "parsed")) {
            last_response <<- now
        }
        if (identical(state, "parsed")) {
            responses <<- responses + 1L
        }
        if (identical(state, "cached")) {
            cache_hits <<- cache_hits + 1L
        }
        if (state %in% c("parsed", "cached")) {
            records <<- records + shift_coalesce(progress$records, 0L)
        }
        bytes <- if (length(progress$download) >= 2L) {
            progress$download[[2L]]
        } else {
            shift_coalesce(progress$downloaded, 0)
        }
        message <- switch(
            state,
            cached = "Reading cached catalog response",
            completed = "Parsing catalog response",
            parsed = "Processing catalog records",
            if (isTRUE(bytes > 0)) {
                "Receiving catalog response"
            } else {
                "Waiting for catalog response"
            }
        )
        reporter$heartbeat(
            message,
            details = list(
                unit_type = "catalog",
                node = node,
                phase = "query",
                catalog_role = phase,
                transfer_state = state,
                bytes_done = bytes,
                request_started_at = started,
                request_seconds = now - started,
                last_response_at = last_response,
                responses = responses,
                cache_hits = cache_hits,
                records_received = records,
                query_timeout = getOption("epwshiftr.query.timeout", 300)
            ),
            force = state %in% c("started", "cached", "parsed")
        )
        invisible(TRUE)
    }
    query_private <- priv(query)
    old <- query_private$progress_callback
    query_private$progress_callback <- callback
    on.exit(query_private$progress_callback <- old, add = TRUE)
    force(expr)
}

# Aggregate index-node failures into one domain-level diagnosis. Index nodes
# are fallback catalog mirrors, so repeated coverage rejections should become
# one count and one scientific explanation rather than duplicate errors.
shift__resolver_failure_diagnostic <- function(records) {
    records <- Filter(Negate(is.null), records)
    kinds <- vapply(records, function(record) record$kind, character(1L))
    # Count one or several normalized failure categories without repeatedly
    # exposing table mechanics throughout the aggregate constructor.
    count <- function(kind) sum(kinds %in% kind)
    structured <- Filter(function(record) !is.null(record$resolution), records)
    useful <- Filter(
        function(record) {
            !is.null(record$resolution$closest)
        },
        structured
    )
    closest_record <- NULL
    if (length(useful)) {
        missing_counts <- vapply(
            useful,
            function(record) {
                length(shift_coalesce(record$resolution$missing, character()))
            },
            integer(1L)
        )
        closest_record <- useful[[which.min(missing_counts)]]
    } else if (length(structured)) {
        closest_record <- structured[[1L]]
    }
    closest <- if (is.null(closest_record)) {
        NULL
    } else {
        closest_record$resolution$closest
    }
    missing <- if (is.null(closest_record)) {
        character()
    } else {
        as.character(shift_coalesce(
            closest_record$resolution$missing,
            character()
        ))
    }
    cause <- if (is.null(closest_record)) {
        "Every configured ESGF index node failed before a complete input set could be resolved."
    } else {
        as.character(closest_record$resolution$summary)[[1L]]
    }
    transient <- kinds %in% c("timeout", "network")
    all_transient <- length(transient) > 0L && all(transient)
    any_transient <- any(transient)
    recovery <- if (isTRUE(all_transient)) {
        "retry"
    } else if (count("coverage") > 0L && !isTRUE(any_transient)) {
        "change_request"
    } else {
        "inspect"
    }
    attempts <- lapply(records, function(record) {
        list(
            node = record$node,
            kind = record$kind,
            future_files = record$future_files,
            reference_files = record$reference_files
        )
    })
    list(
        kind = "resolver_exhausted",
        summary = "No ESGF index node resolved a complete CMIP6 input set.",
        cause = cause,
        nodes_checked = as.integer(length(records)),
        usable_nodes = 0L,
        coverage_failures = as.integer(count("coverage")),
        timeout_failures = as.integer(count("timeout")),
        network_failures = as.integer(count("network")),
        other_failures = as.integer(count("error")),
        # A single timed-out mirror does not make a mixed set of deterministic
        # coverage failures safely retryable. Recommend retry only when every
        # configured node failed for a transient transport reason.
        retryable = isTRUE(all_transient),
        recovery = recovery,
        closest = closest,
        missing = missing,
        attempts = attempts
    )
}

# Raise one typed exhaustion error after all fallback nodes have been tried.
# The compact message serves log mode while complete records remain attached
# for dashboard, watch, and programmatic diagnostics.
shift__abort_resolver_exhausted <- function(records) {
    diagnostic <- shift__resolver_failure_diagnostic(records)
    counts <- c(
        if (diagnostic$coverage_failures) {
            sprintf(
                "%d incomplete",
                diagnostic$coverage_failures
            )
        },
        if (diagnostic$timeout_failures) {
            sprintf(
                "%d timed out",
                diagnostic$timeout_failures
            )
        },
        if (diagnostic$network_failures) {
            sprintf(
                "%d network errors",
                diagnostic$network_failures
            )
        },
        if (diagnostic$other_failures) {
            sprintf(
                "%d other errors",
                diagnostic$other_failures
            )
        }
    )
    evidence <- sprintf(
        "%d node%s checked%s.",
        diagnostic$nodes_checked,
        if (diagnostic$nodes_checked == 1L) "" else "s",
        if (length(counts)) paste0(": ", paste(counts, collapse = ", ")) else ""
    )
    cli::cli_abort(
        c(
            diagnostic$summary,
            "x" = diagnostic$cause,
            "i" = evidence
        ),
        class = c(
            "epwshiftr_shift_resolver_exhausted",
            "epwshiftr_shift_resolution_error"
        ),
        resolution = diagnostic,
        call = NULL
    )
}

# Import immutable, already resolved File query snapshots into a child's own
# store. The same selection drives shared reads, foreground and background runs;
# no catalog discovery or service selection is repeated for another city.
shift__import_shared_inputs <- function(inputs, store) {
    stages <- lapply(c("files", "reference_files"), function(role) {
        ref <- inputs[[role]]
        if (is.null(ref)) {
            return(NULL)
        }
        # Shared snapshots are separate from child databases, so background
        # workers never open another city's active DuckDB store.
        checkmate::assert_file_exists(ref$snapshot, access = "r")
        if (!identical(checksum_file(ref$snapshot), ref$sha256)) {
            cli::cli_abort("The shared File query snapshot has changed.")
        }
        loaded <- query__load(ref$snapshot, SCHEMA_RESULT_FILE)
        result <- query_result__new(
            EsgResultFile,
            index_node = loaded$index_node,
            params = loaded$parameter,
            result = loaded$response,
            context = loaded$context
        )
        query_id <- store$add_files(result, label = "shared-resolved-inputs")
        # add_files hashes both the query and its full source records. Reject a
        # changed snapshot rather than silently selecting different input data.
        if (!identical(query_id, as.character(ref$ids$query_id))) {
            cli::cli_abort("The shared File query snapshot has changed.")
        }
        shift__files_from_query(
            store,
            shift__stage_from_ref(ref$meta$request),
            query_id
        )
    })
    list(
        files = stages[[1L]],
        reference_files = stages[[2L]],
        selection = data.table::as.data.table(inputs$selection),
        index_node = as.character(inputs$index_node)
    )
}

# Collect both catalogs from one index node and fail over in the declared order;
# catalogs from different nodes are never merged.
shift__collect_resolved_inputs <- function(
    plan,
    run_id,
    reporter = NULL,
    job_id = NULL
) {
    store <- shift_store(plan, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    wanted_run_id <- run_id
    run_row <- morpher__private_store(store)$read_table("shift_run")
    run_row <- run_row[run_row[["run_id"]] == wanted_run_id]
    resolved <- plan@meta$resolved
    if (!is.null(resolved) && nrow(run_row) && !is.na(run_row$query_id[[1L]])) {
        request <- shift__request_at_node(
            plan@meta$request,
            as.character(resolved$index_node)
        )
        files <- shift__files_from_query(store, request, run_row$query_id[[1L]])
        reference_files <- NULL
        if (
            !is.na(run_row$reference_query_id[[1L]]) &&
                nzchar(run_row$reference_query_id[[1L]])
        ) {
            reference_request <- shift__historical_request(
                plan,
                as.character(resolved$index_node)
            )
            reference_files <- shift__files_from_query(
                store,
                reference_request,
                run_row$reference_query_id[[1L]]
            )
        }
        if (!is.null(reporter)) {
            pinned_selection <- data.table::as.data.table(resolved$selection)
            pinned_partitions <- shift_coalesce(
                shift__format_cmip6_partitions(pinned_selection),
                "partitions unavailable"
            )
            reporter$unit_started(
                sprintf(
                    "Loading pinned future%s catalogs",
                    if (is.null(reference_files)) "" else " + reference"
                ),
                current = 1L,
                total = 1L,
                details = list(
                    unit_type = "index_node",
                    node = as.character(resolved$index_node)
                )
            )
            reporter$unit_skipped(
                sprintf(
                    "Reused pinned selection \u00b7 %s \u00b7 future %d \u00b7 reference %d files",
                    pinned_partitions,
                    as.integer(files@meta$file_count),
                    if (is.null(reference_files)) {
                        0L
                    } else {
                        as.integer(reference_files@meta$file_count)
                    }
                ),
                current = 1L,
                total = 1L,
                details = list(
                    unit_type = "index_node",
                    node = as.character(resolved$index_node),
                    future_files = as.integer(files@meta$file_count),
                    reference_files = if (is.null(reference_files)) {
                        0L
                    } else {
                        as.integer(reference_files@meta$file_count)
                    },
                    partitions = pinned_partitions,
                    result = sprintf(
                        "reused pinned selection \u00b7 %s",
                        pinned_partitions
                    )
                )
            )
        }
        return(list(
            files = files,
            reference_files = reference_files,
            selection = data.table::as.data.table(resolved$selection),
            index_node = as.character(resolved$index_node)
        ))
    }

    if (!is.null(plan@meta$shared_inputs)) {
        failure <- plan@meta$shared_inputs$failure
        if (is.null(failure)) {
            return(shift__import_shared_inputs(plan@meta$shared_inputs, store))
        }
        # The first attempt reports the shared failure without another request.
        # An explicit resume may retry selection through the ordinary resolver.
        job <- shift__latest_job(store, run_id)
        if (!nrow(job) || job$attempt[[1L]] == 1L) {
            cli::cli_abort(
                "{failure$message}",
                class = as.character(failure$class),
                resolution = failure$resolution
            )
        }
    }

    climate <- plan@meta$climate
    nodes <- if (is.null(climate)) {
        plan@meta$request@meta$options$index_node
    } else {
        climate@index_nodes
    }
    if (is.null(nodes) || !length(nodes)) {
        nodes <- INDEX_NODES[["ORNL"]]
    }
    fields <- unique(c(SHIFT_WORKFLOW_FILE_FIELDS, plan@meta$collect$fields))
    failures <- list()
    for (node_index in seq_along(nodes)) {
        node <- nodes[[node_index]]
        reference_request_for_node <- shift__historical_request(plan, node)
        catalog_roles <- if (is.null(reference_request_for_node)) {
            "future"
        } else {
            "future + reference"
        }
        node_future_files <- NA_integer_
        node_reference_files <- if (is.null(reference_request_for_node)) {
            0L
        } else {
            NA_integer_
        }
        if (!is.null(reporter)) {
            reporter$check_cancel("resolve")
            reporter$unit_started(
                sprintf("Checking %s catalogs", catalog_roles),
                current = node_index,
                total = length(nodes),
                details = list(unit_type = "index_node", node = node)
            )
            reporter$notice(
                "Collecting catalog",
                details = list(
                    unit_type = "catalog",
                    node = node,
                    catalog_role = "future"
                )
            )
        }
        attempt <- tryCatch(
            {
                request <- shift__request_at_node(plan@meta$request, node)
                collect_args <- utils::modifyList(
                    list(
                        store = store,
                        fields = fields,
                        all = TRUE,
                        limit = FALSE,
                        label = "future-epw"
                    ),
                    plan@meta$collect[setdiff(
                        names(plan@meta$collect),
                        "fields"
                    )]
                )
                files <- shift__do_call_with_reporter(
                    reporter,
                    shift_collect,
                    c(list(request), collect_args)
                )
                reference_request <- reference_request_for_node
                reference_files <- if (is.null(reference_request)) {
                    NULL
                } else {
                    if (!is.null(reporter)) {
                        reporter$notice(
                            "Collecting catalog",
                            details = list(
                                unit_type = "catalog",
                                node = node,
                                catalog_role = "reference"
                            )
                        )
                    }
                    collected_reference <- shift__do_call_with_reporter(
                        reporter,
                        shift_collect,
                        c(
                            list(reference_request),
                            utils::modifyList(
                                collect_args,
                                list(label = "historical-reference")
                            )
                        )
                    )
                    collected_reference
                }
                # Resolve the scientific identity before making network calls for
                # individual file services. Batch children pin one model/member/
                # grid, so this removes unrelated partitions and gap years first.
                selection <- shift__resolve_cmip6_selection(
                    plan,
                    future_catalog = shift_file_catalog(
                        store,
                        files@ids$query_id
                    ),
                    reference_catalog = if (is.null(reference_files)) {
                        NULL
                    } else {
                        shift_file_catalog(store, reference_files@ids$query_id)
                    }
                )
                future_experiments <- if (is.null(plan@meta$climate)) {
                    plan@meta$request@meta$experiment
                } else {
                    plan@meta$climate@scenarios
                }
                files <- shift__files_for_partitions(
                    files,
                    selection,
                    experiments = future_experiments,
                    years = unique(as.integer(plan@meta$periods$year)),
                    role = "future"
                )
                files <- shift__resolve_file_services(
                    files,
                    role = "future",
                    reporter = reporter,
                    refresh = plan@meta$control@refresh
                )
                node_future_files <- as.integer(files@meta$file_count)
                if (!is.null(reference_files)) {
                    reference_files <- shift__files_for_partitions(
                        reference_files,
                        selection,
                        experiments = plan@meta$reference@experiment,
                        years = unique(as.integer(
                            plan@meta$reference@periods$year
                        )),
                        role = "reference"
                    )
                    reference_files <- shift__resolve_file_services(
                        reference_files,
                        role = "reference",
                        reporter = reporter,
                        refresh = plan@meta$control@refresh
                    )
                    node_reference_files <- as.integer(
                        reference_files@meta$file_count
                    )
                }
                # Service repair can remove an unusable logical file. Re-run the
                # same coverage kernel so only executable selections are pinned.
                selection <- shift__resolve_cmip6_selection(
                    plan,
                    future_catalog = shift_file_catalog(
                        store,
                        files@ids$query_id
                    ),
                    reference_catalog = if (is.null(reference_files)) {
                        NULL
                    } else {
                        shift_file_catalog(store, reference_files@ids$query_id)
                    }
                )
                list(
                    files = files,
                    reference_files = reference_files,
                    selection = selection,
                    index_node = node
                )
            },
            error = function(e) e
        )
        if (!inherits(attempt, "error")) {
            if (!is.null(reporter)) {
                selected_members <- paste(
                    unique(
                        attempt$selection$variant_label
                    ),
                    collapse = ", "
                )
                selected_partitions <- shift_coalesce(
                    shift__format_cmip6_partitions(attempt$selection),
                    "partitions unavailable"
                )
                selected_result <- sprintf(
                    "%s \u00b7 %s",
                    selected_members,
                    selected_partitions
                )
                reporter$unit_completed(
                    sprintf(
                        "Selected member %s \u00b7 %s",
                        selected_members,
                        selected_partitions
                    ),
                    current = node_index,
                    total = length(nodes),
                    outcome = "completed",
                    details = list(
                        unit_type = "index_node",
                        node = node,
                        future_files = node_future_files,
                        reference_files = node_reference_files,
                        member = unique(attempt$selection$variant_label),
                        partitions = selected_partitions,
                        result = selected_result
                    )
                )
            }
            return(attempt)
        }
        if (inherits(attempt, "epwshiftr_shift_resolution_ambiguity")) {
            stop(attempt)
        }
        resolution <- if (
            inherits(attempt, "epwshiftr_shift_resolution_error")
        ) {
            attempt$resolution
        } else {
            NULL
        }
        error_kind <- if (is.null(resolution)) {
            shift__ui_error_kind(conditionMessage(attempt))
        } else {
            "coverage"
        }
        if (!is.null(reporter)) {
            reporter$unit_completed(
                sprintf(
                    "Rejected: %s",
                    shift__error_summary(conditionMessage(attempt))
                ),
                current = node_index,
                total = length(nodes),
                # Rejection is an expected resolver decision while other
                # nodes remain. Only exhaustion of every candidate is a run
                # failure and therefore an error diagnostic.
                outcome = "rejected",
                details = list(
                    unit_type = "index_node",
                    node = node,
                    future_files = node_future_files,
                    reference_files = node_reference_files,
                    error_kind = error_kind,
                    error = conditionMessage(attempt),
                    resolution = resolution
                )
            )
        }
        failures[[length(failures) + 1L]] <- list(
            node = shift__node_label(node),
            kind = error_kind,
            message = conditionMessage(attempt),
            future_files = node_future_files,
            reference_files = node_reference_files,
            resolution = resolution
        )
    }
    shift__abort_resolver_exhausted(failures)
}

# Expand unresolved plan cases with the member/grid identities selected by the
# resolver and regenerate their stable case IDs.
shift__resolved_expected_cases <- function(plan, selection) {
    original <- plan@meta$expected_cases
    rows <- list()
    for (i in seq_len(nrow(original))) {
        case <- original[i]
        choices <- selection[source_id == case$source_id[[1L]]]
        if (!is.na(case$variant_label[[1L]])) {
            choices <- choices[variant_label == case$variant_label[[1L]]]
        }
        if (!is.na(case$grid_label[[1L]])) {
            choices <- choices[grid_label == case$grid_label[[1L]]]
        }
        for (j in seq_len(nrow(choices))) {
            row <- data.table::copy(case)
            row$variant_label <- choices$variant_label[[j]]
            row$grid_label <- choices$grid_label[[j]]
            row$case_id <- store__hash(
                row$source_id,
                row$experiment_id,
                row$variant_label,
                row$grid_label,
                row$period,
                row$years[[1L]]
            )
            rows[[length(rows) + 1L]] <- row
        }
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Expand persisted selection JSON into exact variable/table/grid rows and attach
# the model/member identity that owns each partition.
shift__selection_partition_rows <- function(
    selection,
    role = c("future", "reference")
) {
    role <- match.arg(role)
    selection <- data.table::as.data.table(selection)
    field <- if (identical(role, "future")) {
        if ("future_partitions_json" %in% names(selection)) {
            "future_partitions_json"
        } else {
            "partitions_json"
        }
    } else {
        "reference_partitions_json"
    }
    if (!field %in% names(selection)) {
        cli::cli_abort("Resolved CMIP6 selection has no {role} partition map.")
    }
    rows <- list()
    for (i in seq_len(nrow(selection))) {
        partitions <- data.table::copy(
            shift__cmip6_partitions(selection[[field]][[i]])
        )
        if (!nrow(partitions)) {
            next
        }
        if (anyNA(partitions$frequency) || any(!nzchar(partitions$frequency))) {
            fallback <- as.character(selection$frequency[[i]])
            if (
                length(fallback) != 1L ||
                    is.na(fallback) ||
                    !nzchar(fallback) ||
                    grepl("+", fallback, fixed = TRUE)
            ) {
                cli::cli_abort(
                    "A legacy CMIP6 partition map has no unambiguous frequency."
                )
            }
            missing_frequency <- is.na(partitions$frequency) |
                !nzchar(partitions$frequency)
            data.table::set(
                partitions,
                i = which(missing_frequency),
                j = "frequency",
                value = fallback
            )
        }
        partitions[, `:=`(
            source_id = selection$source_id[[i]],
            variant_label = selection$variant_label[[i]]
        )]
        rows[[length(rows) + 1L]] <- partitions
    }
    if (!length(rows)) {
        cli::cli_abort(
            "Resolved CMIP6 selection contains no {role} partitions."
        )
    }
    unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

# Match File-result or catalog rows against the resolved partitions. Every
# facet is tested together, preventing a union of tables and grids from
# admitting combinations that the resolver never selected.
shift__partition_row_match <- function(rows, partitions, experiments) {
    rows <- data.table::as.data.table(rows)
    keep <- rep(FALSE, nrow(rows))
    for (i in seq_len(nrow(partitions))) {
        partition <- partitions[i]
        keep <- keep |
            (shift__catalog_match(rows$source_id, partition$source_id[[1L]]) &
                shift__catalog_match(
                    rows$variant_label,
                    partition$variant_label[[1L]]
                ) &
                shift__catalog_match(
                    rows$frequency,
                    partition$frequency[[1L]]
                ) &
                shift__catalog_match(rows$table_id, partition$table_id[[1L]]) &
                shift__catalog_match(
                    rows$grid_label,
                    partition$grid_label[[1L]]
                ) &
                shift__catalog_match(
                    rows$variable_id,
                    partition$variable_id[[1L]]
                ) &
                rows$experiment_id %in% experiments)
    }
    keep
}

# Keep file records intersecting at least one explicitly requested calendar
# year. Unknown ranges remain available for the final coverage check instead of
# being discarded merely because an index node omitted optional time fields.
shift__file_year_match <- function(rows, years = NULL) {
    if (is.null(years)) {
        return(rep(TRUE, nrow(rows)))
    }
    years <- sort(unique(as.integer(years)))
    if (!length(years) || anyNA(years)) {
        cli::cli_abort("`years` must contain known calendar years.")
    }
    ranges <- shift__catalog_fill_time_ranges(rows)
    keep <- rep(TRUE, nrow(ranges))
    for (i in seq_len(nrow(ranges))) {
        start <- suppressWarnings(as.POSIXct(
            ranges$datetime_start[[i]],
            tz = "UTC"
        ))
        stop <- suppressWarnings(as.POSIXct(
            ranges$datetime_end[[i]],
            tz = "UTC"
        ))
        if (is.na(start) || is.na(stop)) {
            next
        }
        first <- as.integer(format(start, "%Y", tz = "UTC"))
        last <- as.integer(format(stop, "%Y", tz = "UTC"))
        keep[[i]] <- any(years >= min(first, last) & years <= max(first, last))
    }
    keep
}

# Create a stored child File result containing only resolved partitions. The
# downloader accepts this child query ID, so an explicit download cannot fetch
# unrelated table/grid combinations from the broad discovery query.
shift__files_for_partitions <- function(
    files,
    selection,
    experiments,
    role = c("future", "reference"),
    years = NULL
) {
    role <- match.arg(role)
    partitions <- shift__selection_partition_rows(selection, role)
    store <- shift_store(files)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    result <- shift_stage_query_result(
        store,
        files@ids$query_id,
        result_type = "File"
    )
    selected <- result$filter(function(rows) {
        shift__partition_row_match(rows, partitions, experiments) &
            shift__file_year_match(rows, years)
    })
    if (!selected$count()) {
        cli::cli_abort(
            "Resolved {role} CMIP6 partitions contain no downloadable File records."
        )
    }
    query_id <- store$add_files(selected, label = sprintf("resolved-%s", role))
    selected_rows <- selected$to_data_table()
    shift_stage_new(
        ShiftFiles,
        "files",
        store_path = files@store_path,
        ids = list(query_id = query_id),
        meta = list(
            request = files@meta$request,
            dataset_count = files@meta$dataset_count,
            datasets = NULL,
            file_count = selected$count(),
            variables = unique(as.character(selected_rows$variable_id)),
            fields = files@meta$fields,
            result_fields = selected$fields
        )
    )
}

# Combine independently planned extraction partitions into one climate stage.
# Coverage is re-read from the store for the union of plan IDs so resume and
# diagnostics use the same durable view as an ordinary shift_extract() call.
shift__combine_climate_stages <- function(stages) {
    stages <- Filter(
        function(stage) S7::S7_inherits(stage, ShiftClimate),
        stages
    )
    if (!length(stages)) {
        cli::cli_abort(
            "No CMIP6 extraction partition produced a climate stage."
        )
    }
    if (length(stages) == 1L) {
        return(stages[[1L]])
    }
    first <- stages[[1L]]
    plan_id <- unique(unlist(
        lapply(stages, function(stage) stage@ids$plan_id),
        use.names = FALSE
    ))
    query_id <- unique(unlist(
        lapply(stages, function(stage) stage@ids$query_id),
        use.names = FALSE
    ))
    store <- shift_store(first)
    coverage <- store$coverage(plan_id = plan_id)
    bind_meta <- function(name) {
        values <- lapply(stages, function(stage) stage@meta[[name]])
        values <- Filter(is.data.frame, values)
        if (!length(values)) {
            NULL
        } else {
            data.table::rbindlist(values, use.names = TRUE, fill = TRUE)
        }
    }
    upstream_name <- if (S7::S7_inherits(first@meta$download, ShiftDownload)) {
        "download"
    } else {
        "files"
    }
    upstream <- first@meta[[upstream_name]]
    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = first@store_path,
        ids = list(query_id = query_id, plan_id = plan_id),
        meta = c(
            stats::setNames(list(upstream), upstream_name),
            list(
                site = first@meta$site,
                periods = first@meta$periods,
                variables = unique(unlist(
                    lapply(stages, function(stage) stage@meta$variables),
                    use.names = FALSE
                )),
                plan = bind_meta("plan"),
                processed = bind_meta("processed"),
                coverage = coverage
            )
        ),
        diagnostics = shift_diagnostics_from_coverage(coverage)
    )
}

# Extract each exact source/member/table/grid partition separately and merge the
# resulting plan IDs only after planning. Selection facets are re-applied after
# user extraction overrides so workflow intent cannot be widened accidentally.
shift__extract_selected_partitions <- function(
    stage,
    selection,
    experiments,
    site,
    periods,
    role = c("future", "reference"),
    time = NULL,
    method = "nearest",
    fallback = "auto",
    overwrite = FALSE,
    resume = TRUE,
    overrides = list(),
    reporter = NULL
) {
    role <- match.arg(role)
    partitions <- shift__selection_partition_rows(selection, role)
    groups <- unique(partitions[, .(
        source_id,
        variant_label,
        frequency,
        table_id,
        grid_label
    )])
    stages <- vector("list", nrow(groups))
    custom_filters <- shift_coalesce(overrides$filters, list())
    overrides$filters <- NULL
    for (i in seq_len(nrow(groups))) {
        group <- groups[i]
        variables <- unique(partitions[
            shift__catalog_match(source_id, group$source_id[[1L]]) &
                shift__catalog_match(variant_label, group$variant_label[[1L]]) &
                shift__catalog_match(frequency, group$frequency[[1L]]) &
                shift__catalog_match(table_id, group$table_id[[1L]]) &
                shift__catalog_match(grid_label, group$grid_label[[1L]]),
            variable_id
        ])
        exact_filters <- list(
            source_id = group$source_id[[1L]],
            experiment_id = experiments,
            variant_label = group$variant_label[[1L]],
            grid_label = group$grid_label[[1L]],
            frequency = group$frequency[[1L]],
            table_id = group$table_id[[1L]]
        )
        args <- utils::modifyList(
            list(
                site = site,
                periods = periods,
                variables = variables,
                time = time,
                filters = utils::modifyList(custom_filters, exact_filters),
                method = method,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume
            ),
            overrides
        )
        # Re-pin scientific selection fields after generic overrides.
        args$site <- site
        args$periods <- periods
        args$variables <- variables
        args$filters <- utils::modifyList(custom_filters, exact_filters)
        args$overwrite <- overwrite
        args$resume <- resume
        if (identical(fallback, "error")) {
            args$fallback <- "error"
        }
        stages[[i]] <- shift__do_call_with_reporter(
            reporter,
            shift_extract,
            c(list(stage), args)
        )
    }
    shift__combine_climate_stages(stages)
}
