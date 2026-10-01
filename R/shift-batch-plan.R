#' @include shift-stage.R
NULL

# Describe each child's source demand before assigning physical files. Separate
# consumer rows retain site and method ownership even when source data overlap.
shift_batch__consumers <- function(children, manifest) {
    if (!length(children)) {
        return(data.table::data.table())
    }
    rows <- vector("list", length(children))
    for (index in seq_along(children)) {
        child <- children[[index]]
        meta <- child@meta
        climate <- meta$climate
        variables <- meta$request@meta$variables
        frequencies <- shift__transform_cmip6_frequencies(
            meta$transform,
            variables,
            climate@frequency
        )
        tables <- shift__cmip6_variable_tables(
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
            periods <- split(role$periods$year, role$periods$period)
            data.table::rbindlist(
                lapply(periods, function(years) {
                    window <- shift__method_time_window(
                        data.table::data.table(
                            period = "requested",
                            year = years
                        ),
                        meta$recipe
                    )
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
                }),
                use.names = TRUE
            )
        })
        demand <- data.table::rbindlist(demands, idcol = "role")
        data.table::set(demand, j = "source_id", value = climate@model)
        data.table::set(demand, j = "variant_label", value = climate@member)
        data.table::set(demand, j = "grid_label", value = climate@grid)
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
    # Demand IDs only join the two tables within this saved plan. Sequential
    # IDs avoid hashing every site-method-variable row separately.
    data.table::set(demands, j = "demand_id", value = seq_len(nrow(demands)))
    demands
}

# Join source demands to cached File metadata once. The file table owns shared
# acquisition intervals; the link table keeps separate child consumers. Actual
# native time indices and spatial reads are resolved by the following stage.
shift_batch__shared_plan <- function(catalog, consumers) {
    empty <- list(
        acquisitions = data.table::data.table(),
        consumers = data.table::data.table(),
        unmatched = data.table::copy(consumers)
    )
    if (!nrow(catalog) || !nrow(consumers)) {
        return(empty)
    }
    catalog <- shift__catalog_current(catalog)
    keys <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "variable_id",
        "frequency",
        "table_id"
    )
    catalog <- catalog[,
        c(
            keys,
            "file_key",
            "filename",
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
    overlap_keys <- c(keys, "file_start<=time_stop", "file_stop>=time_start")
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
        "start",
        "stop"
    )
    links <- data.table::copy(matched[, link_columns, with = FALSE])
    data.table::setnames(
        links,
        c("start", "stop"),
        c("time_start", "time_stop")
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

# Reuse the File records already cached by model discovery. This planning read
# performs no remote query and leaves each child's execution store untouched.
shift_batch__plan_from_discovery <- function(children, manifest, path) {
    consumers <- shift_batch__consumers(children, manifest)
    if (!nrow(consumers)) {
        return(shift_batch__shared_plan(data.table::data.table(), consumers))
    }
    if (!file.exists(file.path(path, "manifest.duckdb"))) {
        return(shift_batch__shared_plan(data.table::data.table(), consumers))
    }
    store <- shift_store(path)
    on.exit(store$close(), add = TRUE)
    sql <- sprintf(
        paste(
            "SELECT * FROM file_catalog",
            "WHERE source_id IN (%s) AND experiment_id IN (%s)",
            "AND variable_id IN (%s)"
        ),
        shift_stage_query_ids(unique(consumers$source_id)),
        shift_stage_query_ids(unique(consumers$experiment_id)),
        shift_stage_query_ids(unique(consumers$variable_id))
    )
    shift_batch__shared_plan(store$query(sql), consumers)
}
