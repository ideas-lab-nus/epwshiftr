# Read one planned file for all linked consumers from a single open dataset.
# Consumer IDs remain distinct when methods reuse a site, while source cells
# and native time slices are shared.
# shift_batch_read__read_acquisition {{{
shift_batch_read__read_acquisition <- function(
    dataset,
    acquisition,
    consumers
) {
    checkmate::assert_data_table(acquisition, nrows = 1L)
    checkmate::assert_data_table(consumers, min.rows = 1L)
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
    checkmate::assert_names(names(consumers), must.include = required)
    checkmate::assert_names(
        names(acquisition),
        must.include = c(
            "acquisition_id",
            "time_start",
            "time_stop",
            "url_opendap",
            "url_download"
        )
    )
    # An acquisition represents one physical file. Refuse to label values from
    # another open dataset with this file's consumer and source provenance.
    checkmate::assert_character(dataset$url, len = 1L, any.missing = FALSE)
    endpoints <- unlist(
        acquisition[1L, c("url_opendap", "url_download"), with = FALSE],
        use.names = FALSE
    )
    endpoints <- endpoints[!is.na(endpoints) & nzchar(endpoints)]
    if (!dataset$url[[1L]] %in% endpoints) {
        stop(
            "The open dataset does not match the acquisition source URL.",
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
    # The positional IDs are internal to this read; both value and provenance
    # tables must restore the same consumer columns, including on empty reads.
    # restore_consumers {{{
    restore_consumers <- function(table) {
        position <- match(table$site_id, read_id)
        data.table::setnames(table, "site_id", "consumer_id")
        for (column in c("site_id", "demand_id", "child_key")) {
            data.table::set(
                table,
                j = column,
                value = consumers[[column]][position]
            )
        }
        table
    }
    # }}}
    values <- restore_consumers(values)
    # The value table records the consumer's future/historical role. Grid
    # provenance retains its spatial role, such as nearest or a corner.
    data.table::set(
        values,
        j = "role",
        value = consumers$role[match(values$consumer_id, read_id)]
    )
    sources <- restore_consumers(sources)
    attr(values, "grid_sources") <- sources
    attr(values, "read_slices") <- slices
    values
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
