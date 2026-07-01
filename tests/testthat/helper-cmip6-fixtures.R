local_cmip6_test_years <- 2059:2061

local_cmip6_nc_file <- function(year, variable_id = "tas") {
    sprintf("%s_day_EC-Earth3_ssp585_r1i1p1f1_gr_%s0101-%s1231.nc", variable_id, year, year)
}

local_is_leap_year <- function(year) {
    (year %% 4L == 0L && year %% 100L != 0L) || year %% 400L == 0L
}

local_cmip6_variable_spec <- function(variable_id) {
    specs <- list(
        tas = list(
            standard_name = "air_temperature",
            long_name = "Near-Surface Air Temperature",
            units = "K"
        ),
        hurs = list(
            standard_name = "relative_humidity",
            long_name = "Near-Surface Relative Humidity",
            units = "%"
        ),
        psl = list(
            standard_name = "air_pressure_at_mean_sea_level",
            long_name = "Sea Level Pressure",
            units = "Pa"
        ),
        rlds = list(
            standard_name = "surface_downwelling_longwave_flux_in_air",
            long_name = "Surface Downwelling Longwave Radiation",
            units = "W m-2"
        ),
        rsds = list(
            standard_name = "surface_downwelling_shortwave_flux_in_air",
            long_name = "Surface Downwelling Shortwave Radiation",
            units = "W m-2"
        ),
        sfcWind = list(
            standard_name = "wind_speed",
            long_name = "Near-Surface Wind Speed",
            units = "m s-1"
        ),
        clt = list(
            standard_name = "cloud_area_fraction",
            long_name = "Total Cloud Cover Percentage",
            units = "%"
        )
    )
    spec <- specs[[variable_id]]
    if (is.null(spec)) {
        stop(sprintf("Unsupported local CMIP6 fixture variable: %s", variable_id), call. = FALSE)
    }
    spec
}

local_cmip6_variable_array <- function(variable_id, lon, lat, time) {
    values <- array(NA_real_, dim = c(length(lon), length(lat), length(time)))
    phase <- 2 * pi * time / length(time)

    for (i in seq_along(lat)) {
        for (j in seq_along(lon)) {
            spatial <- i / 10 + j / 20
            values[j, i, ] <- switch(
                variable_id,
                tas = 299 + 5 * sin(phase) + spatial,
                hurs = pmin(95, pmax(40, 72 + 10 * cos(phase) + spatial)),
                psl = 101000 + 250 * sin(phase / 2) + 10 * spatial,
                rlds = 340 + 18 * sin(phase) + spatial,
                rsds = pmax(0, 260 + 180 * sin(phase - pi / 3) + 3 * spatial),
                sfcWind = pmax(0.1, 3 + 0.6 * cos(phase) + spatial / 8),
                clt = pmin(100, pmax(0, 52 + 22 * cos(phase - pi / 6) + spatial)),
                stop(sprintf("Unsupported local CMIP6 fixture variable: %s", variable_id), call. = FALSE)
            )
        }
    }

    values
}

write_local_cmip6_netcdf_fixture <- function(path, year, variable_id = "tas") {
    spec <- local_cmip6_variable_spec(variable_id)
    lat <- c(1.0, 2.0, 41.0)
    lon <- c(103.5, 104.0, 104.5, 254.0)
    time <- seq_len(if (local_is_leap_year(year)) 366L else 365L) - 0.5
    time_bnds <- rbind(time - 0.5, time + 0.5)
    lat_step <- min(diff(sort(lat)))
    lon_step <- min(diff(sort(lon)))
    lat_bnds <- rbind(lat - lat_step / 2, lat + lat_step / 2)
    lon_bnds <- rbind(lon - lon_step / 2, lon + lon_step / 2)
    values <- local_cmip6_variable_array(variable_id, lon, lat, time)

    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)

    RNetCDF::dim.def.nc(nc, "time", length(time))
    RNetCDF::dim.def.nc(nc, "lat", length(lat))
    RNetCDF::dim.def.nc(nc, "lon", length(lon))
    RNetCDF::dim.def.nc(nc, "bnds", 2L)
    RNetCDF::dim.def.nc(nc, "height", 1L)
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "time_bnds", "NC_DOUBLE", c("bnds", "time"))
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
    RNetCDF::var.def.nc(nc, "lat_bnds", "NC_DOUBLE", c("bnds", "lat"))
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
    RNetCDF::var.def.nc(nc, "lon_bnds", "NC_DOUBLE", c("bnds", "lon"))
    RNetCDF::var.def.nc(nc, "height", "NC_DOUBLE", "height")
    RNetCDF::var.def.nc(nc, variable_id, "NC_FLOAT", c("lon", "lat", "time"))

    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "mip_era", "NC_CHAR", "CMIP6")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "activity_id", "NC_CHAR", "ScenarioMIP")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "institution_id", "NC_CHAR", "EC-Earth-Consortium")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "source_id", "NC_CHAR", "EC-Earth3")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "experiment_id", "NC_CHAR", "ssp585")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variant_label", "NC_CHAR", "r1i1p1f1")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "table_id", "NC_CHAR", "day")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "frequency", "NC_CHAR", "day")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "grid_label", "NC_CHAR", "gr")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "nominal_resolution", "NC_CHAR", "100 km")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variable_id", "NC_CHAR", variable_id)
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "tracking_id", "NC_CHAR", sprintf("hdl:21.14100/local-test-%s-%s", variable_id, year))

    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", sprintf("days since %s-01-01 00:00:00", year))
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "proleptic_gregorian")
    RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
    RNetCDF::att.put.nc(nc, "time", "bounds", "NC_CHAR", "time_bnds")
    RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
    RNetCDF::att.put.nc(nc, "lat", "axis", "NC_CHAR", "Y")
    RNetCDF::att.put.nc(nc, "lat", "bounds", "NC_CHAR", "lat_bnds")
    RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
    RNetCDF::att.put.nc(nc, "lon", "axis", "NC_CHAR", "X")
    RNetCDF::att.put.nc(nc, "lon", "bounds", "NC_CHAR", "lon_bnds")
    RNetCDF::att.put.nc(nc, "height", "units", "NC_CHAR", "m")
    RNetCDF::att.put.nc(nc, "height", "standard_name", "NC_CHAR", "height")
    RNetCDF::att.put.nc(nc, "height", "positive", "NC_CHAR", "up")
    RNetCDF::att.put.nc(nc, variable_id, "standard_name", "NC_CHAR", spec$standard_name)
    RNetCDF::att.put.nc(nc, variable_id, "long_name", "NC_CHAR", spec$long_name)
    RNetCDF::att.put.nc(nc, variable_id, "units", "NC_CHAR", spec$units)

    RNetCDF::var.put.nc(nc, "time", time, count = length(time))
    RNetCDF::var.put.nc(nc, "time_bnds", time_bnds, count = dim(time_bnds))
    RNetCDF::var.put.nc(nc, "lat", lat, count = length(lat))
    RNetCDF::var.put.nc(nc, "lat_bnds", lat_bnds, count = dim(lat_bnds))
    RNetCDF::var.put.nc(nc, "lon", lon, count = length(lon))
    RNetCDF::var.put.nc(nc, "lon_bnds", lon_bnds, count = dim(lon_bnds))
    RNetCDF::var.put.nc(nc, "height", 2.0, count = 1L)
    RNetCDF::var.put.nc(nc, variable_id, values, count = dim(values))

    invisible(path)
}

write_local_morph_tas_fixture <- function(path, year = 2060L) {
    datetime <- seq.POSIXt(
        as.POSIXct(sprintf("%s-01-01 00:00:00", year), tz = "UTC"),
        as.POSIXct(sprintf("%s-12-31 00:00:00", year), tz = "UTC"),
        by = "day"
    )
    coords <- data.table::CJ(lat = c(1.0, 2.0), lon = c(103.5, 104.0, 104.5))
    coords[, coord_id := .I]
    data <- data.table::CJ(datetime = datetime, coord_id = coords$coord_id)
    data[coords, on = "coord_id", `:=`(lon = i.lon, lat = i.lat)]
    data[, `:=`(
        activity_drs = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        table_id = "day",
        variable = "tas",
        description = "Near-Surface Air Temperature",
        units = "K",
        value = 299 + 5 * sin(2 * pi * (as.integer(format(datetime, "%j")) - 1) / length(datetime)) + coord_id / 10
    )]
    data[, coord_id := NULL]
    data.table::setcolorder(data, c(
        "activity_drs", "institution_id", "source_id", "experiment_id",
        "member_id", "table_id", "lon", "lat", "datetime",
        "variable", "description", "units", "value"
    ))
    write_parquet_file(data, path)
    invisible(path)
}
