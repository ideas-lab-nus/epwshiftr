# get_nc_meta {{{
#' @importFrom data.table setattr setDT
#' @importFrom ncmeta nc_atts
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    J <- value <- name <- NULL
    verbose("Parsing meta data of NetCDF file '", file, "'...")
    # get all attributes
    atts <- data.table::setDT(ncmeta::nc_atts(file))

    # get meta data
    meta <- as.list(atts[
        J("NC_GLOBAL",
          c("mip_era", "activity_id", "institution_id", "source_id",
            "experiment_id", "variant_label", "table_id", "grid_label",
            "nominal_resolution", "variable_id", "tracking_id")
        ),
        on = c("variable", "name"),
        {v <- unlist(value); names(v) <- name; v}])

    # get variable long name and units
    meta <- c(
        meta,
        data.table::setattr(
            atts[J(meta$variable_id, c("standard_name", "units")), on = c("variable", "name"), value],
            "names", c("standard_name", "units")
        )
    )

    # get time origin and unit
    c(
        meta,
        data.table::setattr(
            atts[J("time", c("units", "calendar")), on = c("variable", "name"), value],
            "names", c("time_units", "time_calendar")
        )
    )
}
# }}}
