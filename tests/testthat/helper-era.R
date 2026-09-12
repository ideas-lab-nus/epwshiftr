# Create a small ERA-style NetCDF file with explicit CF time, latitude, and
# longitude coordinates for adapter tests.
write_test_era_netcdf <- function(
    path,
    variable = "t2m",
    values,
    time,
    latitude = 37.75,
    longitude = c(237.5, 237.75),
    units = "K"
) {
    handle <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(handle), add = TRUE)
    RNetCDF::dim.def.nc(handle, "longitude", length(longitude))
    RNetCDF::dim.def.nc(handle, "latitude", length(latitude))
    RNetCDF::dim.def.nc(handle, "time", length(time))
    RNetCDF::var.def.nc(handle, "longitude", "NC_DOUBLE", "longitude")
    RNetCDF::var.def.nc(handle, "latitude", "NC_DOUBLE", "latitude")
    RNetCDF::var.def.nc(handle, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(
        handle,
        variable,
        "NC_DOUBLE",
        c("longitude", "latitude", "time")
    )
    RNetCDF::att.put.nc(handle, "time", "units", "NC_CHAR",
        "hours since 2000-01-01 00:00:00")
    RNetCDF::att.put.nc(handle, "time", "calendar", "NC_CHAR", "standard")
    RNetCDF::att.put.nc(handle, variable, "units", "NC_CHAR", units)
    RNetCDF::var.put.nc(handle, "longitude", longitude)
    RNetCDF::var.put.nc(handle, "latitude", latitude)
    RNetCDF::var.put.nc(handle, "time", time)
    RNetCDF::var.put.nc(handle, variable, values)
    invisible(path)
}

# Return a public availability-shaped table for deterministic batch workflow
# tests without contacting an ESGF index node.
test_cmip6_availability <- function(
    variables,
    scenarios,
    member,
    frequency,
    index_node,
    source = NULL,
    grid = NULL,
    ...
) {
    models <- if (is.null(source)) {
        c("Model-A", "Model-B", "Model-C")
    } else {
        source
    }
    grid <- if (is.null(grid)) "gn" else grid
    tables <- stats::setNames(vapply(frequency, function(value) {
        shift_coalesce(shift__cmip6_table_id(value), "unknown")
    }, character(1L)), names(frequency))
    data.frame(
        source_id = models,
        variant_label = rep(member, length(models)),
        grid_label = rep(grid, length(models)),
        frequency = rep(paste(unique(frequency), collapse = "+"),
            length(models)),
        frequency_spec = I(rep(list(frequency), length(models))),
        table_id = rep(paste(unique(tables), collapse = "+"),
            length(models)),
        table = I(rep(list(tables), length(models))),
        complete = TRUE,
        complete_experiments = length(scenarios) + 1L,
        required_experiments = length(scenarios) + 1L,
        available_pairs = length(variables) * (length(scenarios) + 1L),
        required_pairs = length(variables) * (length(scenarios) + 1L),
        missing = NA_character_,
        index_node = index_node,
        stringsAsFactors = FALSE
    )
}
