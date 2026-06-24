local_cmip6_test_years <- 2059:2061

local_cmip6_nc_file <- function(year) {
    sprintf("tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_%s0101-%s1231.nc", year, year)
}

local_is_leap_year <- function(year) {
    (year %% 4L == 0L && year %% 100L != 0L) || year %% 400L == 0L
}

local_tunnel_dist <- function(lat1, lon1, lat2, lon2) {
    to_rad <- function(x) x * pi / 180
    earth_radius <- 6371.009

    lat1 <- to_rad(lat1)
    lon1 <- to_rad(lon1)
    lat2 <- to_rad(lat2)
    lon2 <- to_rad(lon2)

    delta_x <- cos(lat2) * cos(lon2) - cos(lat1) * cos(lon1)
    delta_y <- cos(lat2) * sin(lon2) - cos(lat1) * sin(lon1)
    delta_z <- sin(lat2) - sin(lat1)

    sqrt(delta_x ^ 2 + delta_y ^ 2 + delta_z ^ 2) * earth_radius
}

write_local_cmip6_netcdf_fixture <- function(path, year) {
    lat <- c(1.0, 2.0, 41.0)
    lon <- c(103.5, 104.0, 104.5, 254.0)
    time <- seq_len(if (local_is_leap_year(year)) 366L else 365L) - 1L
    tas <- array(NA_real_, dim = c(length(lon), length(lat), length(time)))

    for (i in seq_along(lat)) {
        for (j in seq_along(lon)) {
            tas[j, i, ] <- 299 + 5 * sin(2 * pi * time / length(time)) + i / 10 + j / 20
        }
    }

    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)

    RNetCDF::dim.def.nc(nc, "time", length(time))
    RNetCDF::dim.def.nc(nc, "lat", length(lat))
    RNetCDF::dim.def.nc(nc, "lon", length(lon))
    RNetCDF::dim.def.nc(nc, "height", 1L)
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
    RNetCDF::var.def.nc(nc, "height", "NC_DOUBLE", "height")
    RNetCDF::var.def.nc(nc, "tas", "NC_DOUBLE", c("lon", "lat", "time"))

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
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variable_id", "NC_CHAR", "tas")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "tracking_id", "NC_CHAR", sprintf("hdl:21.14100/local-test-%s", year))

    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", sprintf("days since %s-01-01 00:00:00", year))
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "standard")
    RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
    RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
    RNetCDF::att.put.nc(nc, "lat", "axis", "NC_CHAR", "Y")
    RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
    RNetCDF::att.put.nc(nc, "lon", "axis", "NC_CHAR", "X")
    RNetCDF::att.put.nc(nc, "height", "units", "NC_CHAR", "m")
    RNetCDF::att.put.nc(nc, "height", "standard_name", "NC_CHAR", "height")
    RNetCDF::att.put.nc(nc, "height", "positive", "NC_CHAR", "up")
    RNetCDF::att.put.nc(nc, "tas", "standard_name", "NC_CHAR", "air_temperature")
    RNetCDF::att.put.nc(nc, "tas", "long_name", "NC_CHAR", "Near-Surface Air Temperature")
    RNetCDF::att.put.nc(nc, "tas", "units", "NC_CHAR", "K")

    RNetCDF::var.put.nc(nc, "time", time, count = length(time))
    RNetCDF::var.put.nc(nc, "lat", lat, count = length(lat))
    RNetCDF::var.put.nc(nc, "lon", lon, count = length(lon))
    RNetCDF::var.put.nc(nc, "height", 2.0, count = 1L)
    RNetCDF::var.put.nc(nc, "tas", tas, count = dim(tas))

    invisible(path)
}

local_cmip6_index <- function(paths) {
    data.table::rbindlist(lapply(paths, function(path) {
        meta <- get_nc_meta(path)
        time <- get_nc_time(path, range = TRUE)
        year <- sub(".*_(\\d{4})0101-\\d{4}1231\\.nc$", "\\1", basename(path))

        data.table::data.table(
            file_id = sprintf("local|tas|%s", year),
            dataset_id = sprintf("local-dataset-%s", year),
            mip_era = meta$mip_era,
            activity_drs = meta$activity_id,
            institution_id = meta$institution_id,
            source_id = meta$source_id,
            experiment_id = meta$experiment_id,
            member_id = meta$variant_label,
            table_id = meta$table_id,
            frequency = meta$table_id,
            grid_label = meta$grid_label,
            version = "v20240101",
            nominal_resolution = meta$nominal_resolution,
            variable_id = meta$variable_id,
            variable_long_name = meta$standard_name,
            variable_units = meta$units,
            datetime_start = time[[1L]],
            datetime_end = time[[2L]],
            file_size = unname(file.size(path)),
            data_node = "example.test",
            file_url = sprintf("https://example.test/%s", basename(path)),
            dataset_pid = sprintf("hdl:21.14100/local-dataset-%s", year),
            tracking_id = meta$tracking_id
        )
    }))
}

write_local_morph_tas_fixture <- function(path, year = 2060L) {
    datetime <- seq.POSIXt(
        as.POSIXct(sprintf("%s-01-01 00:00:00", year), tz = "UTC"),
        as.POSIXct(sprintf("%s-12-31 00:00:00", year), tz = "UTC"),
        by = "day"
    )
    coords <- data.table::CJ(lat = c(1.0, 2.0), lon = c(103.5, 104.0, 104.5))
    coords[, `:=`(
        coord_id = .I,
        dist = local_tunnel_dist(lat, lon, 1.37, 103.98)
    )]
    data <- data.table::CJ(datetime = datetime, coord_id = coords$coord_id)
    data[coords, on = "coord_id", `:=`(lon = i.lon, lat = i.lat, dist = i.dist)]
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
        "member_id", "table_id", "lon", "lat", "dist", "datetime",
        "variable", "description", "units", "value"
    ))
    fst::write_fst(data, path, compress = 100)
    invisible(path)
}