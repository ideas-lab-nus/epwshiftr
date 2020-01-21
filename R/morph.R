# preprocess_morphing {{{
#' @importFrom fancycut wafflecut
#' @importFrom checkmate assert_count
#' @importFrom data.table set
#' @export
preprocess_morphing <- function (dt, leapyear = FALSE, start_from = 2020L, cut_by = 10L) {
    # TODO: handle leapyear in mean, max, min
    if (!leapyear) dt <- dt[!J(2L, 29L), on = c("month", "day")]

    assert_count(start_from, positive = TRUE)
    assert_count(cut_by, positive = TRUE)

    intervals <- cut_intervals(start_from, max(dt$year), cut_by)
    cuts <- fancycut::wafflecut(dt$year, intervals)

    data.table::set(dt, NULL, "interval", cuts)
    on.exit(data.table::set(dt, NULL, "interval", NULL))

    res <- location_mean(dt, c("datetime", "year", "hour", "minute", "second"))
    data.table::set(res, NULL, "value", units::set_units(res$value, res$units[1], mode = "standard"))

    # remove non-useful columns
    data.table::set(res, NULL, c("variable", "description"), NULL)
}
# }}}

# daily_mean {{{
daily_mean <- function (data_epw, var, units = TRUE) {
    # calculate daily maximum, mean, and minimum values from weather file
    daily <- data_epw[,
        list(val_mean = mean(get(var)), val_max = max(get(var)), val_min = min(get(var))),
        by = c("month", "day")]

    if (units) {
        if (!inherits(data_epw[[var]], "units")) return(daily)

        # get variable unit
        u <- units(data_epw[[var]])
        daily[,
            # change units to K
            `:=`(val_mean = units::set_units(val_mean, u, mode = "standard"),
                 val_max = units::set_units(val_max, u, mode = "standard"),
                 val_min = units::set_units(val_min, u, mode = "standard")
        )]
    }

    daily
}
# }}}

# align_units {{{
align_units <- function (dt, units) {
    data.table::set(dt, NULL, "value", units::set_units(dt$value, units, mode = "standard"))
}
# }}}

# morphing_from_max_mean_min {{{
#' @importFrom units drop_units set_units
#' @export
morphing_from_max_mean_min <- function (var, data_epw, data_mean, data_max, data_min,
                                        start_from = 2020L, cut_by = 10L) {
    data_mean <- preprocess_morphing(data_mean, leapyear = FALSE, start_from, cut_by)
    data_max <- preprocess_morphing(data_max, leapyear = FALSE, start_from, cut_by)
    data_min <- preprocess_morphing(data_min, leapyear = FALSE, start_from, cut_by)

    daily <- daily_mean(data_epw, var)

    # set units
    u <- units(data_epw[[var]])
    data_mean <- align_units(data_mean, u)
    data_max <- align_units(data_max, u)
    data_min <- align_units(data_min, u)

    # calculate changes of daily maximum, mean and minimum dry bulb temperature
    data_mean[daily, on = c("month", "day"), value := value - i.val_mean]
    data_max[daily, on = c("month", "day"), value := value - i.val_max]
    data_min[daily, on = c("month", "day"), value := value - i.val_min]

    # calculate scaling factor
    alpha <- copy(data_max)[data_min, on = c(setdiff(names(data_max), "value")), alpha := value - i.value]
    alpha[daily, on = c("month", "day"), alpha := alpha / (i.val_max - i.val_min)]
    # in case tamax is equal to tamin
    alpha[is.infinite(alpha), alpha := 1.0]
    # remove units
    alpha[, alpha := units::drop_units(alpha)]
    # combine delta mean temp
    alpha[data_mean, on = c(setdiff(names(alpha), c("value", "alpha"))), value := i.value]

    data_ta <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)]
    # add meta columns from CMIP6 and alpha components
    data_ta <- data_ta[alpha, on = c("month", "day"), allow.cartesian = TRUE]
    # add daily mean temperature
    data_ta[daily, on = c("month", "day"), daily_mean := val_mean]
    # calculate delta
    data_ta[, delta := units::set_units(value + alpha * (get(var) - daily_mean), u, mode = "standard")]

    # calcuate final projection
    data_ta[, c(var) := get(var) + delta]

    data_ta[, .SD, .SDcols = c(
        # meta from CMIP6
        "activity_drs", "experiment_id", "institution_id", "source_id", "member_id",
        "table_id", "lon", "lat",
        # interval
        "interval",
        # datetime
        "datetime", "year", "month", "day", "hour", "minute",
        # value
        var, "delta"
        )]
}
# }}}

# morphing_from_mean {{{
morphing_from_mean <- function (var, data_epw, data_mean, type = c("shift", "stretch"),
                                start_from = 2020L, cut_by = 10L) {
    data_mean <- preprocess_morphing(data_mean, leapyear = FALSE, start_from, cut_by)

    daily <- daily_mean(data_epw, var)

    # set units
    u <- units(data_epw[[var]])
    data_mean <- align_units(data_mean, u)

    if (type == "shift") {
        # calculate changes of daily mean
        data_mean[daily, on = c("month", "day"), delta := value - i.val_mean]
    } else if (type == "stretch") {
        # calculate changes of daily mean
        data_mean[daily, on = c("month", "day"), alpha := value / i.val_mean]
    }

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = c("month", "day"), allow.cartesian = TRUE]

    if (type == "shift") {
        data[, c(var) := units::set_units(get(var) + delta, u, mode = "standard")]
    } else if (type == "stretch") {
        data[, c(var) := units::set_units(get(var) * alpha, u, mode = "standard")]
    }

    data[, .SD, .SDcols = c(
        # meta from CMIP6
        "activity_drs", "experiment_id", "institution_id", "source_id", "member_id",
        "table_id", "lon", "lat",
        # interval
        "interval",
        # datetime
        "datetime", "year", "month", "day", "hour", "minute",
        # value
        var, if (type == "shift") "delta" else "alpha"
        )]
}
# }}}

# morphing_tdb {{{
#' Morphing dry-bulb temperature
#'
#' Use daily mean temperature, daily maximum temperature and daily minimum
#' temperature to chagne only two statistical paramters of the timeseries of
#' temperature, namely the mean and the variance. This is achieved by shifting
#' by the value for mean temperature and stretching by the diurnal range.
#'
#' @references
#' Belcher, S., Hacker, J., Powell, D., 2005. Constructing design weather data
#' for future climates. Building Services Engineering Research and Technology
#' 26, 49–61. https://doi.org/10.1191/0143624405bt112oa
#'
#' @export
morphing_tdb <- function (data_epw, tas, tasmax, tasmin, start_from = 2020L, cut_by = 10L) {
    morphing_from_max_mean_min("dry_bulb_temperature", data_epw, tas, tasmax, tasmin, start_from, cut_by)
}
# }}}

# morphing_rh {{{
#' Morphing relative humidity
#'
#' @references
#' Belcher, S., Hacker, J., Powell, D., 2005. Constructing design weather data
#' for future climates. Building Services Engineering Research and Technology
#' 26, 49–61. https://doi.org/10.1191/0143624405bt112oa
#'
#' @export
morphing_rh <- function (data_epw, hurs, hursmax, hursmin, start_from = 2020L, cut_by = 10L) {
    morphing_from_max_mean_min("relative_humidity", data_epw, hurs, hursmax, hursmin, start_from, cut_by)
}
# }}}

# morphing_tdew {{{
morphing_tdew <- function (tdb, rh) {
    psychrolib::SetUnitSystem("SI")
    tdew <- data.table::copy(tdb)[
        rh, on = c(setdiff(names(tdb), c("dry_bulb_temperature", "delta"))),
        dew_point_temperature :=
            units::set_units(
                psychrolib::GetTDewPointFromRelHum(
                    units::drop_units(dry_bulb_temperature),
                    units::drop_units(i.relative_humidity) / 100
                ),
                degree_Celsius
            )
    ]
    tdew[data_epw, on = c("datetime"), delta := dew_point_temperature - i.dew_point_temperature]
    tdew[, c("dry_bulb_temperature") := NULL][]
}
# }}}

# morphing_pa {{{
morphing_pa <- function (data_epw, psl, start_from = 2020, cut_by = 10) {
    morphing_from_mean("atmospheric_pressure", data_epw, psl, "stretch", start_from, cut_by)
}
# }}}

# morphing_hor_ir {{{
morphing_hor_ir <- function (data_epw, rlds, start_from = 2020, cut_by = 10) {
    morphing_from_mean("horizontal_infrared_radiation_intensity_from_sky", data_epw,
        type = "stretch", rlds, start_from, cut_by)
}
# }}}

# morphing_glob_rad {{{ 
morphing_glob_rad <- function (data_epw, rsds, start_from = 2020, cut_by = 10) { 
    morphing_from_mean("global_horizontal_radiation", data_epw, rsds,
        type = "stretch", start_from, cut_by)
}
# }}}

# morphing_diff_rad {{{ 
morphing_diff_rad <- function (data_epw, glob_rad) { 
    diff_rad <- data.table::copy(glob_rad)
    diff_rad[data_epw[, .SD, .SDcols = c("month", "day", "hour", "diffuse_horizontal_radiation")],
        on = c("month", "day", "hour"),
        diffuse_horizontal_radiation := i.diffuse_horizontal_radiation * alpha]
    diff_rad[, global_horizontal_radiation := NULL]
    diff_rad[, diffuse_horizontal_radiation := units::set_units(units::drop_units(
            diffuse_horizontal_radiation), "W/m^2")][]
}
# }}}

# morphing_norm_rad {{{ 
morphing_norm_rad <- function (glob_rad, diff_rad) { 
    norm_rad <- data.table::copy(glob_rad)
    norm_rad[, diffuse_horizontal_radiation := diff_rad$diffuse_horizontal_radiation]
    # calculate solar angle
    norm_rad[, day_of_year := lubridate::yday(datetime)]
    norm_rad[, solar_angle := solar_angle(lat, lon, day_of_year, hour, 8)]
    norm_rad[, direct_normal_radiation := (global_horizontal_radiation - diffuse_horizontal_radiation) * abs(solar_angle)]
}
# }}}

# morphing_wind_speed {{{
morphing_wind_speed <- function (data_epw, sfcwind, start_from = 2020, cut_by = 10) {
    morphing_from_mean("wind_speed", data_epw, sfcwind, type = "stretch", start_from, cut_by)
}
# }}}

# morphing_total_sky_cover {{{
morphing_total_sky_cover <- function (data_epw, clt, start_from = 2020, cut_by = 10) {
    var <- "total_sky_cover"
    data_mean <- preprocess_morphing(clt, leapyear = FALSE, start_from, cut_by)
    daily <- unique(data_epw[, .SD, .SDcols = c("month", "day")])

    data_mean <- data_mean[daily, on = c("month", "day")]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = c("month", "day"), allow.cartesian = TRUE]

    data.table::set(data, NULL, "value", units::drop_units(data$value))

    data[, `:=`(total_sky_cover = as.integer(round(pmax(0, pmin(10, value/10)))),
                alpha = round(pmax(0, pmin(10, value/10))) / total_sky_cover
    )]

    data[, .SD, .SDcols = c(
        # meta from CMIP6
        "activity_drs", "experiment_id", "institution_id", "source_id", "member_id",
        "table_id", "lon", "lat",
        # interval
        "interval",
        # datetime
        "datetime", "year", "month", "day", "hour", "minute",
        # value
        var, "alpha"
        )]
}
# }}}

# morphing_opaque_sky_cover {{{
morphing_opaque_sky_cover <- function (data_epw, total_sky_cover) {
    data <- data.table::copy(total_sky_cover)[
        data_epw[, .SD, .SDcols = c("month", "day", "hour", "opaque_sky_cover")],
        on = c("month", "day", "hour"),
        opaque_sky_cover := as.integer(round(i.opaque_sky_cover * alpha))][
        , total_sky_cover := NULL]

    data[, .SD, .SDcols = c(
        # meta from CMIP6
        "activity_drs", "experiment_id", "institution_id", "source_id", "member_id",
        "table_id", "lon", "lat",
        # interval
        "interval",
        # datetime
        "datetime", "year", "month", "day", "hour", "minute",
        # value
        "opaque_sky_cover", "alpha"
        )]
}
# }}}

# morphing_precipitation {{{
morphing_precipitation <- function (data_epw, pr, start_from = 2020L, cut_by = 10L) {
    morphing_from_mean("precipitable_water", data_epw, pr, type = "stretch", start_from, cut_by)
}
# }}}

# clean_morphed {{{
clean_morphed <- function (data, var) {
    cols <- c("experiment_id", "interval", "datetime", "year", "month", "day", "hour", "minute")
    data <- data[!J("[0, 2020)"), on = "interval"][, .SD, .SDcols = c(cols, var)][
        , {x <- list(mean(get(var))); names(x) <- var; x}, by = cols]

    if (var %in% c("total_sky_cover", "opaque_sky_cover")) {
        data.table::set(data, NULL, var, as.integer(data[[var]]))
    }

    data.table::set(data, NULL, "interval", droplevels(data$interval))

    data
}
# }}}

# cut_intervals {{{
cut_intervals <- function (start, end, by) {
    pnt <- seq(start, end, by)
    if (pnt[length(pnt)] < end) pnt <- c(pnt, end)

    intervals <- paste0(pnt[-length(pnt)], ", ", pnt[-1], "]")
    intervals[[1L]] <- paste0("[", intervals[[1L]])
    intervals[-1L] <- paste0("(", intervals[-1L])
    buckets <- paste0(pnt[-length(pnt)], "-", pnt[-1])

    # create interval for values smaller than the start
    intervals <- c(paste0("[0, ", start, ")"), intervals)
    buckets <- c(paste0("<", start), buckets)

    names(intervals) <- buckets

    intervals
}
# }}}

# location_mean {{{
location_mean <- function (dt, by_exclude = NULL) {
    res <- dt[, list(lon = mean(lon), lat = mean(lat), value = mean(value)),
        by = c(setdiff(names(dt), c("lon", "lat", "value", by_exclude)))]
    data.table::setcolorder(res, setdiff(names(dt), by_exclude))[]
}
# }}}

# to_radian {{{
to_radian <- function (degree) {
    degree * pi / 180
}
# }}}

# to_degree {{{
to_degree <- function (radian) {
    radian * 180 / pi
}
# }}}

# day_angle {{{
day_angle <- function (day_of_year) {
    day_of_year * 360.0 / 365.25
}
# }}}

# equ_of_time {{{
equ_of_time <- function (day_of_year) {
    d <- day_angle(day_of_year)
    -0.128 * sin(to_radian(d - 2.8)) - 0.165 * sin(to_radian(2.0 * d + 19.7))
}
# }}}

# solar_time {{{
solar_time <- function (longitude, day_of_year, hour, timezone) {
    hour + longitude - timezone + equ_of_time(day_of_year)
}
# }}}

# hour_angle {{{
hour_angle <- function (longitude, day_of_year, hour, timezone) {
    st <- solar_time(longitude, day_of_year, hour, timezone)
    360 / 24 * (st - 12)
}
# }}}

# declination {{{
declination <- function (day_of_year) {
    d <- day_angle(day_of_year)

    to_radian(asin(to_radian(0.3978 * sin(to_radian(d - 1.4 + 0.0355 * sin(to_radian(d - 0.0489)))))))
}
# }}}

# solar_angle {{{
solar_angle <- function (latitude, longitude, day_of_year, hour, timezone) {
    decl <- declination(day_of_year)
    h_ang <- hour_angle(longitude, day_of_year, hour, timezone)
    sin(to_radian(latitude)) * sin(to_radian(decl)) + cos(to_radian(latitude)) * cos(to_radian(decl)) * cos(to_radian(h_ang))
}
# }}}

# cor_factor_solar_dis {{{
cor_factor_solar_dis <- function (day_of_year) {
    d <- day_angle(day_of_year)
    1 + 0.03344 * cos(to_radian(d - 2.8))
}
# }}}

# extra_dir_norm_rad {{{
extra_dir_norm_rad <- function (day_of_year) {
    solar_constant <- 1367 # W/m2
    cor_factor_solar_dis(day_of_year) * solar_constant
}
# }}}

# extra_hor_rad {{{
extra_hor_rad <- function (latitude, longitude, day_of_year, hour, timezone) {
    sol_ang <- solar_angle(latitude, longitude, day_of_year, hour, timezone)
    dir_norm <- extra_dir_norm_rad(day_of_year)

    sol_ang * dir_norm
}
# }}}

# hor_ir_intensity {{{
hor_ir_intensity <- function (dry_bulb_temperature, cloud_cover) {
    coloud_cover * 5.67 * 10^(-8) * (dry_bulb_temperature + 273.15)^4
}
# }}}
