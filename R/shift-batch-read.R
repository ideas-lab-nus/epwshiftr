# Read one D03 acquisition for all linked consumers from a single open
# dataset. Consumer IDs remain distinct when methods reuse the same site;
# the reader deduplicates only native source cells and time slices.
shift_batch__read_acquisition <- function(dataset, acquisition, consumers) {
    if (!data.table::is.data.table(acquisition) || nrow(acquisition) != 1L) {
        stop(
            "`acquisition` must be one row of the shared file plan.",
            call. = FALSE
        )
    }
    if (!data.table::is.data.table(consumers) || !nrow(consumers)) {
        stop(
            "`consumers` must contain linked rows of the shared file plan.",
            call. = FALSE
        )
    }
    required <- c(
        "acquisition_id",
        "demand_id",
        "child_key",
        "site_id",
        "role",
        "variable_id",
        "lon",
        "lat",
        "spatial_method",
        "time_start",
        "time_stop"
    )
    if (
        !all(required %in% names(consumers)) ||
            !all(
                c("acquisition_id", "time_start", "time_stop") %in%
                    names(acquisition)
            )
    ) {
        stop(
            "The shared file plan is missing acquisition or consumer fields.",
            call. = FALSE
        )
    }
    if (
        anyNA(consumers$acquisition_id) ||
            !all(
                consumers$acquisition_id == acquisition$acquisition_id[[1L]]
            ) ||
            data.table::uniqueN(consumers$variable_id) != 1L
    ) {
        stop(
            "Consumers must reference one acquisition and one variable.",
            call. = FALSE
        )
    }

    # Positional read IDs distinguish repeated physical sites across methods
    # without changing their original site_id or demand_id provenance.
    read_id <- as.character(seq_len(nrow(consumers)))
    targets <- data.table::data.table(
        site_id = read_id,
        lon = consumers$lon,
        lat = consumers$lat,
        method = consumers$spatial_method,
        time_start = consumers$time_start,
        time_stop = consumers$time_stop
    )
    values <- dataset__read_regions(
        dataset,
        consumers$variable_id[[1L]],
        targets,
        time = c(acquisition$time_start[[1L]], acquisition$time_stop[[1L]])
    )
    sources <- attr(values, "grid_sources", exact = TRUE)
    slices <- attr(values, "read_slices", exact = TRUE)
    if (nrow(values)) {
        position <- match(values$site_id, read_id)
        data.table::setnames(values, "site_id", "consumer_id")
        data.table::set(
            values,
            j = "site_id",
            value = consumers$site_id[position]
        )
        data.table::set(
            values,
            j = "demand_id",
            value = consumers$demand_id[position]
        )
        data.table::set(
            values,
            j = "child_key",
            value = consumers$child_key[position]
        )
        data.table::set(values, j = "role", value = consumers$role[position])
    }
    if (nrow(sources)) {
        position <- match(sources$site_id, read_id)
        data.table::setnames(sources, "site_id", "consumer_id")
        data.table::set(
            sources,
            j = "site_id",
            value = consumers$site_id[position]
        )
        data.table::set(
            sources,
            j = "demand_id",
            value = consumers$demand_id[position]
        )
        data.table::set(
            sources,
            j = "child_key",
            value = consumers$child_key[position]
        )
        data.table::set(sources, j = "role", value = consumers$role[position])
    }
    attr(values, "grid_sources") <- sources
    attr(values, "read_slices") <- slices
    values
}
