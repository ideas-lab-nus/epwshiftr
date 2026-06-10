# Build deterministic vignette fixtures for application-driven articles.
#
# Run from the package root:
#   Rscript data-raw/build-vignette-fixtures.R

out_dir <- file.path("inst", "extdata", "vignettes", "future-weather")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fixture_dir_placeholder <- "__FUTURE_WEATHER_FIXTURE_DIR__"
index_node <- "https://esgf-data.dkrz.de"
year <- 2060L
frequency <- "mon"
table_id <- "Amon"
data_node <- "esgf.ceda.ac.uk"
lat <- c(1.0, 2.0, 41.0)
lon <- c(103.5, 104.0, 104.5, 254.0)
site_lon <- 103.98
site_lat <- 1.37

variables <- data.frame(
    variable_id = c("tas", "psl"),
    standard_name = c(
        "air_temperature",
        "air_pressure_at_mean_sea_level"
    ),
    long_name = c(
        "Near-Surface Air Temperature",
        "Sea Level Pressure"
    ),
    units = c("K", "Pa"),
    stringsAsFactors = FALSE
)

local_is_leap_year <- function(year) {
    (year %% 4L == 0L && year %% 100L != 0L) || year %% 400L == 0L
}

fixture_path_token <- function(path) {
    file.path(fixture_dir_placeholder, basename(path), fsep = "/")
}

nc_file_name <- function(variable_id, year) {
    sprintf("%s_Amon_EC-Earth3_ssp585_r1i1p1f1_gr_%s01-%s12.nc", variable_id, year, year)
}

variable_values <- function(variable_id, i, j, time) {
    seasonal <- sin(2 * pi * time / length(time))
    base <- switch(
        variable_id,
        tas = 299 + 5 * seasonal,
        psl = 101300 + 250 * seasonal,
        1 + seasonal
    )
    base + i / 10 + j / 20
}

write_epw_fixture <- function(path) {
    datetime <- seq.POSIXt(
        as.POSIXct("2001-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2001-12-31 23:00:00", tz = "UTC"),
        by = "hour"
    )

    month <- as.integer(format(datetime, "%m"))
    day <- as.integer(format(datetime, "%d"))
    hour0 <- as.integer(format(datetime, "%H"))
    yday <- as.integer(format(datetime, "%j"))
    daylight <- pmax(0, sin(pi * (hour0 - 6) / 12))

    weather <- data.frame(
        year = 2001L,
        month = month,
        day = day,
        hour = hour0 + 1L,
        minute = 60L,
        data_source = "A7A7A7A7*0?0?0?0",
        dry_bulb = sprintf("%.1f", 27 + 2 * sin(2 * pi * (yday - 81) / 365) + 1.5 * sin(2 * pi * (hour0 - 8) / 24)),
        dew_point = sprintf("%.1f", 25 + 2 * sin(2 * pi * (yday - 81) / 365) + 1.5 * sin(2 * pi * (hour0 - 8) / 24)),
        relative_humidity = as.integer(round(pmin(pmax(78 - 12 * sin(2 * pi * (hour0 - 8) / 24), 45), 95))),
        atmospheric_pressure = 101325L,
        extraterrestrial_horizontal_radiation = as.integer(round(1000 * daylight)),
        extraterrestrial_direct_normal_radiation = as.integer(round(900 * daylight)),
        horizontal_infrared_radiation_intensity_from_sky = 350L,
        global_horizontal_radiation = as.integer(round(800 * daylight)),
        direct_normal_radiation = as.integer(round(600 * daylight)),
        diffuse_horizontal_radiation = as.integer(round(200 * daylight)),
        global_horizontal_illuminance = as.integer(round(800 * daylight * 120)),
        direct_normal_illuminance = as.integer(round(600 * daylight * 120)),
        diffuse_horizontal_illuminance = as.integer(round(200 * daylight * 120)),
        zenith_luminance = as.integer(round(pmin(9999, 800 * daylight * 12))),
        wind_direction = 180L,
        wind_speed = sprintf("%.1f", 2.5 + 0.8 * sin(2 * pi * hour0 / 24)),
        total_sky_cover = 5L,
        opaque_sky_cover = 5L,
        visibility = sprintf("%.1f", 20.0),
        ceiling_height = 77777L,
        present_weather_observation = 0L,
        present_weather_codes = "999999999",
        precipitable_water = 35L,
        aerosol_optical_depth = sprintf("%.1f", 0.1),
        snow_depth = 0L,
        days_since_last_snowfall = 88L,
        albedo = sprintf("%.2f", 0.12),
        liquid_precipitation_depth = sprintf("%.1f", 0.0),
        liquid_precipitation_quantity = sprintf("%.1f", 0.0),
        stringsAsFactors = FALSE
    )

    header <- c(
        "LOCATION,Singapore,NA,Singapore,TEST,486980,1.37,103.98,8.0,15.0",
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
        "COMMENTS 1,Generated local vignette EPW fixture",
        "COMMENTS 2,Deterministic synthetic data for epwshiftr articles",
        "DATA PERIODS,1,1,Data,Monday,1/1,12/31"
    )

    writeLines(c(header, do.call(paste, c(weather, sep = ","))), path)
    invisible(path)
}

write_cmip6_netcdf_fixture <- function(path, variable_id, year) {
    info <- variables[variables$variable_id == variable_id, , drop = FALSE]
    stopifnot(nrow(info) == 1L)

    origin <- as.POSIXct(sprintf("%s-01-01 00:00:00", year), tz = "UTC")
    month_start <- as.POSIXct(sprintf("%s-%02d-01 00:00:00", year, seq_len(12L)), tz = "UTC")
    month_end <- c(month_start[-1L], as.POSIXct(sprintf("%s-01-01 00:00:00", year + 1L), tz = "UTC"))
    time_bnds <- rbind(
        as.numeric(difftime(month_start, origin, units = "days")),
        as.numeric(difftime(month_end, origin, units = "days"))
    )
    time <- colMeans(time_bnds)
    lat_step <- min(diff(sort(lat)))
    lon_step <- min(diff(sort(lon)))
    lat_bnds <- rbind(lat - lat_step / 2, lat + lat_step / 2)
    lon_bnds <- rbind(lon - lon_step / 2, lon + lon_step / 2)
    values <- array(NA_real_, dim = c(length(lon), length(lat), length(time)))

    for (i in seq_along(lat)) {
        for (j in seq_along(lon)) {
            values[j, i, ] <- variable_values(variable_id, i, j, time)
        }
    }

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
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "table_id", "NC_CHAR", table_id)
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "frequency", "NC_CHAR", frequency)
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "grid_label", "NC_CHAR", "gr")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "nominal_resolution", "NC_CHAR", "100 km")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variable_id", "NC_CHAR", variable_id)
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "tracking_id", "NC_CHAR", sprintf("hdl:21.14100/vignette-%s-%s", variable_id, year))

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
    RNetCDF::att.put.nc(nc, variable_id, "standard_name", "NC_CHAR", info$standard_name[[1L]])
    RNetCDF::att.put.nc(nc, variable_id, "long_name", "NC_CHAR", info$long_name[[1L]])
    RNetCDF::att.put.nc(nc, variable_id, "units", "NC_CHAR", info$units[[1L]])

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

param <- function(value) {
    list(value = value, negate = FALSE)
}

query_parameter <- function(type, variable_id = variables$variable_id, fields = "*", limit = 20L) {
    list(
        facet = list(
            project = param("CMIP6"),
            activity_id = param("ScenarioMIP"),
            experiment_id = param("ssp585"),
            source_id = param("EC-Earth3"),
            variant_label = param("r1i1p1f1"),
            frequency = param(frequency),
            variable_id = param(variable_id),
            data_node = param(data_node),
            fields = param(fields)
        ),
        query = list(),
        control = list(
            latest = param(TRUE),
            type = param(type),
            offset = param(0L),
            distrib = param(TRUE),
            limit = param(limit),
            format = param("application/solr+json")
        ),
        others = list(table_id = param(table_id))
    )
}

response_object <- function(docs, params, qtime = 1L) {
    empty_map <- stats::setNames(list(), character())
    list(
        responseHeader = list(
            status = 0L,
            QTime = qtime,
            params = params
        ),
        response = list(
            numFound = nrow(docs),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
        facet_counts = list(
            facet_queries = empty_map,
            facet_fields = empty_map,
            facet_ranges = empty_map,
            facet_intervals = empty_map,
            facet_heatmaps = empty_map
        ),
        timestamp = "2026-01-01T00:00:00:000000Z"
    )
}

write_result_json <- function(file, type, docs) {
    data <- list(
        index_node = index_node,
        parameter = query_parameter(type),
        response = response_object(docs, list(type = type))
    )
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
    invisible(file)
}

write_query_json <- function(file) {
    data <- list(index_node = index_node, parameter = query_parameter("Dataset"))
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
    invisible(file)
}

epw_path <- file.path(out_dir, "SGP_Singapore.486980_IWEC.epw")
write_epw_fixture(epw_path)

nc_paths <- setNames(file.path(out_dir, nc_file_name(variables$variable_id, year)), variables$variable_id)
for (variable_id in names(nc_paths)) {
    write_cmip6_netcdf_fixture(nc_paths[[variable_id]], variable_id, year)
}

dataset_id <- sprintf(
    "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.Amon.%s.gr.v20200310|%s",
    variables$variable_id,
    data_node
)
dataset_docs <- data.frame(
    id = dataset_id,
    access = I(rep(list(c("HTTPServer", "OPENDAP")), nrow(variables))),
    activity_id = I(rep(list("ScenarioMIP"), nrow(variables))),
    experiment_id = I(rep(list("ssp585"), nrow(variables))),
    frequency = I(rep(list(frequency), nrow(variables))),
    index_node = data_node,
    number_of_files = 1L,
    number_of_aggregations = 1L,
    project = I(rep(list("CMIP6"), nrow(variables))),
    size = as.numeric(file.size(nc_paths)),
    source_id = I(rep(list("EC-Earth3"), nrow(variables))),
    variable_id = I(as.list(variables$variable_id)),
    variant_label = I(rep(list("r1i1p1f1"), nrow(variables))),
    check.names = FALSE,
    stringsAsFactors = FALSE
)

base_docs <- data.frame(
    id = sprintf("%s|file", basename(nc_paths)),
    dataset_id = dataset_id,
    size = as.numeric(file.size(nc_paths)),
    checksum = as.character(tools::md5sum(nc_paths)),
    checksum_type = "MD5",
    instance_id = sprintf("%s.instance", basename(nc_paths)),
    master_id = sprintf("CMIP6.vignette.%s.master", variables$variable_id),
    replica = FALSE,
    tracking_id = sprintf("hdl:21.14100/vignette-%s-%s", variables$variable_id, year),
    title = basename(nc_paths),
    version = 20200310L,
    data_node = data_node,
    source_id = "EC-Earth3",
    experiment_id = "ssp585",
    variant_label = "r1i1p1f1",
    frequency = frequency,
    table_id = table_id,
    variable_id = variables$variable_id,
    grid_label = "gr",
    datetime_start = sprintf("%s-01-01T00:00:00Z", year),
    datetime_end = sprintf("%s-12-31T23:59:59Z", year),
    latest = TRUE,
    retracted = FALSE,
    deprecated = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE
)
base_docs$url <- I(lapply(seq_along(nc_paths), function(i) {
    path <- fixture_path_token(nc_paths[[i]])
    c(
        sprintf("%s|application/netcdf|OPENDAP", path),
        sprintf("file://%s|application/netcdf|HTTPServer", path)
    )
}))

aggregation_docs <- base_docs
aggregation_docs$id <- sprintf("%s|aggregation", basename(nc_paths))

write_query_json(file.path(out_dir, "query.json"))
write_result_json(file.path(out_dir, "dataset-result.json"), "Dataset", dataset_docs)
write_result_json(file.path(out_dir, "file-result.json"), "File", base_docs)
write_result_json(file.path(out_dir, "aggregation-result.json"), "Aggregation", aggregation_docs)

message("Wrote vignette fixtures to ", normalizePath(out_dir, mustWork = TRUE))
