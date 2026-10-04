#' @include shift-stage.R
NULL

# Describe each child's source demand before assigning physical files. Separate
# consumer rows retain site and method ownership even when source data overlap.
# shift_batch_plan__consumers {{{
shift_batch_plan__consumers <- function(children, manifest) {
    if (!length(children)) {
        return(data.table::data.table())
    }
    rows <- vector("list", length(children))
    for (index in seq_along(children)) {
        child <- children[[index]]
        meta <- child@meta
        if (!is.null(meta$shared_inputs$failure)) {
            next
        }
        climate <- meta$climate
        variables <- meta$request@meta$variables
        frequencies <- shift_spec__transform_cmip6_frequencies(
            meta$transform,
            variables,
            climate@frequency
        )
        tables <- shift_spec__cmip6_variable_tables(
            variables,
            frequencies,
            climate@table
        )
        reference <- meta$reference
        roles <- list(
            future = list(
                periods = meta$periods,
                experiments = climate@scenarios
            )
        )
        if (
            S7::S7_inherits(reference, ShiftReferenceSpec) &&
                identical(reference@mode, "historical")
        ) {
            roles$historical <- list(
                periods = reference@periods,
                experiments = reference@experiment
            )
        }
        demands <- lapply(roles, function(role) {
            # Child extraction uses one continuous time window for all named
            # periods. Match that exact window so the shared read can populate
            # the child store's existing content-addressed extraction cache.
            window <- shift_spec__method_time_window(role$periods, meta$recipe)
            grid <- data.table::CJ(
                experiment_id = role$experiments,
                variable_id = variables,
                sorted = FALSE
            )
            data.table::set(
                grid,
                j = "frequency",
                value = unname(frequencies[grid$variable_id])
            )
            data.table::set(
                grid,
                j = "table_id",
                value = unname(tables[grid$variable_id])
            )
            data.table::set(
                grid,
                j = "time_start",
                value = as.POSIXct(
                    window[[1L]],
                    format = "%Y-%m-%dT%H:%M:%SZ",
                    tz = "UTC"
                )
            )
            data.table::set(
                grid,
                j = "time_stop",
                value = as.POSIXct(
                    window[[2L]],
                    format = "%Y-%m-%dT%H:%M:%SZ",
                    tz = "UTC"
                )
            )
            grid
        })
        demand <- data.table::rbindlist(demands, idcol = "role")
        inputs <- meta$shared_inputs
        if (!is.null(inputs)) {
            # Resolve exact variable/table/grid partitions, including optional
            # inputs admitted by both periods. Do not recreate a union of all
            # recipe alternatives after scientific selection has completed.
            selected <- data.table::as.data.table(inputs$selection)
            partitions <- data.table::rbindlist(
                lapply(
                    c("future", "reference"),
                    function(role) {
                        if (
                            identical(role, "reference") &&
                                is.null(inputs$reference_files)
                        ) {
                            return(NULL)
                        }
                        rows <- shift_resolve__selection_partition_rows(
                            selected,
                            role
                        )
                        data.table::set(
                            rows,
                            j = "role",
                            value = if (identical(role, "reference")) {
                                "historical"
                            } else {
                                "future"
                            }
                        )
                        rows
                    }
                ),
                use.names = TRUE
            )
            windows <- unique(demand[,
                c(
                    "role",
                    "experiment_id",
                    "time_start",
                    "time_stop"
                ),
                with = FALSE
            ])
            demand <- merge(
                windows,
                partitions,
                by = "role",
                allow.cartesian = TRUE,
                sort = FALSE
            )
            data.table::set(demand, j = "required", value = NULL)
            data.table::set(demand, j = "input_id", value = inputs$input_id)
        } else {
            data.table::set(demand, j = "grid_label", value = climate@grid)
        }
        data.table::set(demand, j = "source_id", value = climate@model)
        data.table::set(demand, j = "variant_label", value = climate@member)
        data.table::set(
            demand,
            j = "child_key",
            value = manifest$child_key[[index]]
        )
        data.table::set(
            demand,
            j = "site_id",
            value = manifest$site_id[[index]]
        )
        data.table::set(demand, j = "method", value = manifest$method[[index]])
        data.table::set(demand, j = "lon", value = meta$site@lon)
        data.table::set(demand, j = "lat", value = meta$site@lat)
        data.table::set(
            demand,
            j = "spatial_method",
            value = meta$control@extraction_method
        )
        rows[[index]] <- demand
    }
    demands <- unique(data.table::rbindlist(rows, use.names = TRUE))
    if (!nrow(demands)) {
        return(data.table::data.table())
    }
    # Demand IDs only join the two tables within this saved plan. Sequential
    # IDs avoid hashing every site-method-variable row separately.
    data.table::set(demands, j = "demand_id", value = seq_len(nrow(demands)))
    demands
}
# }}}

# Join source demands to cached File metadata once. The file table owns shared
# acquisition intervals; the link table keeps separate child consumers. Actual
# native time indices and spatial reads are resolved by the following stage.
# shift_batch_plan__shared_plan {{{
shift_batch_plan__shared_plan <- function(catalog, consumers) {
    empty <- list(
        acquisitions = data.table::data.table(),
        consumers = data.table::data.table(),
        unmatched = data.table::copy(consumers)
    )
    if (!nrow(catalog) || !nrow(consumers)) {
        return(empty)
    }
    catalog <- shift_resolve__catalog_current(catalog)
    if (!"master_id" %in% names(catalog)) {
        data.table::set(catalog, j = "master_id", value = NA_character_)
    }
    keys <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "variable_id",
        "frequency",
        "table_id"
    )
    input_key <- intersect("input_id", names(consumers))
    catalog <- catalog[,
        c(
            keys,
            input_key,
            "file_key",
            "filename",
            "master_id",
            "version",
            "tracking_id",
            "checksum",
            "checksum_type",
            "size",
            "datetime_start",
            "datetime_end",
            "url_opendap",
            "url_download",
            "data_node"
        ),
        with = FALSE
    ]
    data.table::set(
        catalog,
        j = "file_start",
        value = as.POSIXct(catalog$datetime_start, tz = "UTC")
    )
    data.table::set(
        catalog,
        j = "file_stop",
        value = as.POSIXct(catalog$datetime_end, tz = "UTC")
    )
    valid <- !is.na(catalog$file_start) &
        !is.na(catalog$file_stop) &
        catalog$file_start <= catalog$file_stop
    catalog <- catalog[valid]
    if (!nrow(catalog)) {
        return(empty)
    }
    data.table::set(
        catalog,
        j = "logical_file_id",
        value = store__logical_file_id(catalog)
    )
    # Different versions or unverified endpoints must never share a task.
    physical_id <- vapply(
        seq_len(nrow(catalog)),
        function(index) {
            row <- catalog[index]
            source <- if (
                !is.na(row$checksum[[1L]]) &&
                    nzchar(row$checksum[[1L]])
            ) {
                row$checksum[[1L]]
            } else {
                c(row$url_opendap[[1L]], row$url_download[[1L]])
            }
            store__hash(
                "shared-file-v1",
                row$logical_file_id[[1L]],
                as.list(row[, keys, with = FALSE]),
                row$version[[1L]],
                row$tracking_id[[1L]],
                row$checksum_type[[1L]],
                source,
                row$size[[1L]]
            )
        },
        character(1L)
    )
    data.table::set(catalog, j = "physical_file_id", value = physical_id)
    # Resolve both source identity and time overlap in the indexed join. A
    # join on identity alone can materialize every file-site-period pairing.
    data.table::set(catalog, j = "catalog_row", value = seq_len(nrow(catalog)))
    lookup <- data.table::copy(consumers)
    data.table::set(lookup, j = "consumer_row", value = seq_len(nrow(lookup)))
    overlap_keys <- c(
        keys,
        input_key,
        "file_start<=time_stop",
        "file_stop>=time_start"
    )
    pairs <- catalog[
        lookup,
        on = overlap_keys,
        nomatch = 0L,
        allow.cartesian = TRUE,
        j = list(
            catalog_row = get("catalog_row"),
            consumer_row = get("i.consumer_row")
        )
    ]
    if (!nrow(pairs)) {
        return(empty)
    }
    matched <- data.table::copy(consumers[pairs$consumer_row])
    catalog_columns <- setdiff(names(catalog), c(keys, "catalog_row"))
    for (column in catalog_columns) {
        data.table::set(
            matched,
            j = column,
            value = catalog[[column]][pairs$catalog_row]
        )
    }
    data.table::set(
        matched,
        j = "start",
        value = pmax(matched$time_start, matched$file_start)
    )
    data.table::set(
        matched,
        j = "stop",
        value = pmin(matched$time_stop, matched$file_stop)
    )
    data.table::setorderv(matched, c("physical_file_id", "start", "stop"))
    # File rows are contiguous after sorting. Traverse files once, carrying
    # the furthest stop so a long window also absorbs enclosed short windows.
    count <- nrow(matched)
    start <- as.numeric(matched$start)
    stop <- as.numeric(matched$stop)
    file_first <- which(!duplicated(matched$physical_file_id))
    file_last <- c(file_first[-1L] - 1L, count)
    interval <- integer(count)
    running_end <- numeric(count)
    for (index in seq_along(file_first)) {
        positions <- seq.int(file_first[[index]], file_last[[index]])
        running_end[positions] <- cummax(stop[positions])
        new_interval <- c(
            TRUE,
            start[positions[-1L]] > running_end[positions[-length(positions)]]
        )
        interval[positions] <- cumsum(new_interval)
    }
    data.table::set(matched, j = "interval", value = interval)
    group <- data.table::rleid(matched$physical_file_id, matched$interval)
    first <- which(!duplicated(group))
    last <- c(first[-1L] - 1L, count)
    acquisition_columns <- c(
        "physical_file_id",
        "interval",
        "logical_file_id",
        "file_key",
        "filename",
        "master_id",
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "variable_id",
        "frequency",
        "table_id",
        "version",
        "tracking_id",
        "checksum",
        "checksum_type",
        "size",
        "url_opendap",
        "url_download",
        "data_node"
    )
    acquisitions <- data.table::copy(matched[
        first,
        acquisition_columns,
        with = FALSE
    ])
    data.table::set(
        acquisitions,
        j = "time_start",
        value = as.POSIXct(start[first], origin = "1970-01-01", tz = "UTC")
    )
    data.table::set(
        acquisitions,
        j = "time_stop",
        value = as.POSIXct(running_end[last], origin = "1970-01-01", tz = "UTC")
    )
    data.table::set(
        acquisitions,
        j = "acquisition_id",
        value = vapply(
            seq_len(nrow(acquisitions)),
            function(index) {
                store__hash(
                    acquisitions$physical_file_id[[index]],
                    as.numeric(acquisitions$time_start[[index]]),
                    as.numeric(acquisitions$time_stop[[index]])
                )
            },
            character(1L)
        )
    )
    link_columns <- c(
        "demand_id",
        "child_key",
        "site_id",
        "method",
        "role",
        "experiment_id",
        "variable_id",
        "lon",
        "lat",
        "spatial_method",
        "time_start",
        "time_stop",
        "start",
        "stop"
    )
    links <- data.table::copy(matched[, link_columns, with = FALSE])
    data.table::setnames(
        links,
        c("time_start", "time_stop", "start", "stop"),
        c("requested_start", "requested_stop", "time_start", "time_stop")
    )
    data.table::set(
        links,
        j = "acquisition_id",
        value = acquisitions$acquisition_id[group]
    )
    unmatched <- consumers[!consumers$demand_id %in% unique(matched$demand_id)]
    data.table::set(acquisitions, j = "interval", value = NULL)
    list(
        acquisitions = acquisitions,
        consumers = unique(links),
        unmatched = unmatched
    )
}
# }}}

# Reuse the File records already cached by model discovery. This planning read
# performs no remote query and leaves each child's execution store untouched.
# shift_batch_plan__plan_from_discovery {{{
shift_batch_plan__plan_from_discovery <- function(children, manifest, path) {
    consumers <- shift_batch_plan__consumers(children, manifest)
    if (!nrow(consumers)) {
        return(shift_batch_plan__shared_plan(
            data.table::data.table(),
            consumers
        ))
    }
    if (!file.exists(file.path(path, "manifest.duckdb"))) {
        return(shift_batch_plan__shared_plan(
            data.table::data.table(),
            consumers
        ))
    }
    store <- shift_store(path)
    on.exit(store$close(), add = TRUE)
    sql <- sprintf(
        paste(
            "SELECT * FROM file_catalog",
            "WHERE source_id IN (%s) AND experiment_id IN (%s)",
            "AND variable_id IN (%s)"
        ),
        shift_stage__query_ids(unique(consumers$source_id)),
        shift_stage__query_ids(unique(consumers$experiment_id)),
        shift_stage__query_ids(unique(consumers$variable_id))
    )
    shift_batch_plan__shared_plan(store$query(sql), consumers)
}
# }}}

# Resolve one site-independent input selection per model/method and persist it
# before shared reads begin. Dry-run discovery remains provisional; execution
# pins the same immutable File snapshots for every linked child and resume.
# shift_batch_plan__resolve_inputs {{{
shift_batch_plan__resolve_inputs <- function(batch, reporter = NULL) {
    children <- batch@meta$children
    plans <- lapply(children, shift_batch_plan__child_plan)
    pending <- which(vapply(
        seq_along(children),
        function(index) {
            status <- shift_status(children[[index]], refresh = FALSE)
            input <- plans[[index]]@meta$shared_inputs
            !status %in%
                c("completed", "queued", "running", "stopping", "waiting") &&
                (is.null(input) || !is.null(input$failure))
        },
        logical(1L)
    ))
    if (!length(pending)) {
        return(batch)
    }
    groups <- vapply(
        plans[pending],
        function(child) {
            meta <- child@meta
            store__hash(
                shift_persist__request_spec_value(meta$request),
                shift_persist__climate_spec_value(meta$climate),
                transform__spec_value(meta$transform),
                shift_persist__reference_spec_value(
                    meta$reference,
                    "model_historical"
                ),
                meta$periods,
                meta$collect,
                meta$control@allow_partial
            )
        },
        character(1L)
    )
    for (positions in split(pending, groups)) {
        if (!is.null(reporter)) {
            reporter$check_cancel()
        }
        child <- plans[[positions[[1L]]]]
        child@meta$shared_inputs <- NULL
        # Auxiliary catalog runs belong to the shared cache, not a city's
        # public workflow history.
        child@store_path <- file.path(batch@store_path, "shared-inputs")
        resolved <- tryCatch(
            shift_resolve__collect_resolved_inputs(
                child,
                run_id = NULL,
                reporter = reporter
            ),
            error = identity
        )
        if (inherits(resolved, "error")) {
            # Let each child persist its ordinary failed-run diagnostics without
            # repeating the same failed catalog request for every city.
            inputs <- list(
                failure = list(
                    message = conditionMessage(resolved),
                    class = class(resolved),
                    resolution = resolved$resolution
                )
            )
            for (index in positions) {
                children[[index]]@meta$shared_inputs <- inputs
                plans[[index]]@meta$shared_inputs <- inputs
            }
            next
        }
        # Copy immutable query evidence outside child stores before any worker
        # can write to them. Content hashes make interrupted copies detectable.
        snapshots <- lapply(
            list(resolved$files, resolved$reference_files),
            function(files) {
                if (is.null(files)) {
                    return(NULL)
                }
                store <- shift_store(files)
                on.exit(store$close(), add = TRUE)
                query <- shift_inspect__query_run(store, files@ids$query_id)
                source <- file.path(store$path, query$query_file[[1L]])
                hash <- checksum_file(source)
                path <- file.path(
                    batch@store_path,
                    "shared-inputs",
                    paste0(hash, ".json")
                )
                dir.create(
                    dirname(path),
                    recursive = TRUE,
                    showWarnings = FALSE
                )
                if (!file.exists(path) && !file.copy(source, path)) {
                    cli::cli_abort(
                        "Could not save the shared File query snapshot."
                    )
                }
                if (!identical(checksum_file(path), hash)) {
                    cli::cli_abort(
                        "The shared File query snapshot has changed."
                    )
                }
                ref <- shift_persist__stage_ref(files)
                ref$snapshot <- path
                ref$sha256 <- hash
                ref
            }
        )
        inputs <- list(
            store = child@store_path,
            files = snapshots[[1L]],
            reference_files = snapshots[[2L]],
            selection = resolved$selection,
            index_node = resolved$index_node,
            input_id = store__hash(
                resolved$files@ids$query_id,
                if (!is.null(resolved$reference_files)) {
                    resolved$reference_files@ids$query_id
                },
                resolved$selection
            )
        )
        # Only the small snapshot references are shared. Each city continues
        # to own its extraction plans, run state and output artifacts.
        for (index in positions) {
            children[[index]]@meta$shared_inputs <- inputs
            plans[[index]]@meta$shared_inputs <- inputs
        }
    }
    batch@meta$children <- children
    inputs <- lapply(plans, function(child) child@meta$shared_inputs)
    inputs <- Filter(
        function(input) !is.null(input) && is.null(input$failure),
        inputs
    )
    inputs <- inputs[
        !duplicated(vapply(
            inputs,
            function(value) {
                value$input_id
            },
            character(1L)
        ))
    ]
    catalogs <- lapply(inputs, function(input) {
        store <- shift_store(input$store)
        on.exit(store$close(), add = TRUE)
        rows <- data.table::rbindlist(
            lapply(
                c("files", "reference_files"),
                function(role) {
                    ref <- input[[role]]
                    if (is.null(ref)) {
                        return(NULL)
                    }
                    shift_inspect__file_catalog(store, ref$ids$query_id)
                }
            ),
            use.names = TRUE
        )
        data.table::set(rows, j = "input_id", value = input$input_id)
        rows
    })
    batch@meta$shared_plan <- shift_batch_plan__shared_plan(
        data.table::rbindlist(catalogs, use.names = TRUE),
        shift_batch_plan__consumers(plans, batch@meta$manifest)
    )
    shift_batch__receipt_write(batch)
    batch
}
# }}}

# Reconstruct run intent while retaining a batch's explicitly retried selection.
# Old failed run specs remain evidence; the batch receipt owns the new snapshot.
# shift_batch_plan__child_plan {{{
shift_batch_plan__child_plan <- function(child) {
    if (S7::S7_inherits(child, ShiftPlan)) {
        return(child)
    }
    plan <- shift_persist__plan_from_spec(
        jsonlite::fromJSON(
            child@meta$run$spec_json[[1L]],
            simplifyVector = TRUE
        ),
        store = child@store_path
    )
    if (!is.null(child@meta$shared_inputs)) {
        plan@meta$shared_inputs <- child@meta$shared_inputs
    }
    plan
}
# }}}

# vim: fdm=marker :
