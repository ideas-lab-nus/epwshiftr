# preprocess_morphing {{{
#' @importFrom checkmate assert_count
#' @importFrom data.table set
preprocess_morphing <- function (dt, leapyear = FALSE, start_from = 2020L, cut_by = 30L) {
    # TODO: handle leapyear in mean, max, min
    if (!leapyear) dt <- dt[!J(2L, 29L), on = c("month", "day")]

    assert_count(start_from, positive = TRUE)
    assert_count(cut_by, positive = TRUE)

    end <- max(dt$year)
    cuts <- seq(start_from, end, cut_by)
    if (cuts[length(cuts)] < end) cuts <- c(cuts, end)

    dt <- dt[J(cuts), on = "year"]
    data.table::set(dt, NULL, "interval", as.factor(dt$year))

    # calculate monthly mean and average value for longitude and latitude
    res <- location_mean(dt, c("datetime", "year", "day", "hour", "minute", "second"))
    data.table::set(res, NULL, "value", units::set_units(res$value, res$units[1], mode = "standard"))

    # remove non-useful columns
    data.table::set(res, NULL, c("variable", "description"), NULL)
}
# }}}

# monthly_mean {{{
monthly_mean <- function (data_epw, var, units = TRUE) {
    # calculate monthly maximum, mean, and minimum values from weather file
    monthly <- data_epw[,
        list(val_mean = mean(get(var)), val_max = max(get(var)), val_min = min(get(var))),
        by = "month"]

    if (units) {
        if (!inherits(data_epw[[var]], "units")) return(monthly)

        # get variable unit
        u <- units(data_epw[[var]])
        monthly[,
            # change units to K
            `:=`(val_mean = units::set_units(val_mean, u, mode = "standard"),
                 val_max = units::set_units(val_max, u, mode = "standard"),
                 val_min = units::set_units(val_min, u, mode = "standard")
        )]
    }

    monthly
}
# }}}

# align_units {{{
align_units <- function (dt, units) {
    data.table::set(dt, NULL, "value", units::set_units(dt$value, units, mode = "standard"))
}
# }}}

# morphing_from_mean {{{
morphing_from_mean <- function (var, data_epw, data_mean, type = c("shift", "stretch", "combined"),
                                start_from = 2020L, cut_by = 30L) {
    type <- match.arg(type)
    # add cut interval and average by lon, lat, month and day (i.e. monthly
    # avarage) in CMIP6 data
    data_mean <- preprocess_morphing(data_mean, leapyear = FALSE, start_from, cut_by)

    # calculate monthly average of EPW data
    monthly <- monthly_mean(data_epw, var)

    # set units
    u <- units(data_epw[[var]])
    # this will automatically do unit conversions like K --> C
    data_mean <- align_units(data_mean, u)

    # add datetime columns from the original EPW data into the monthly average of
    # CMIP6 data
    # after this every row in 'data' indicates a specific hour (as EPW has
    # hourly data)
    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE]

    # calculate delta, alpha and add EPW monthly average value
    data[monthly, on = "month",
        `:=`(delta = value - i.val_mean, alpha = value / i.val_mean, value_mean = i.val_mean)
    ]

    if (type == "shift") {
        data[, c(var) := units::set_units(get(var) + delta, u, mode = "standard")]
    } else if (type == "stretch") {
        data[, c(var) := units::set_units(get(var) * alpha, u, mode = "standard")]
    } else if (type == "combined") {
        data[, c(var) := units::set_units(get(var) + delta + alpha * get(var), u, mode = "standard")]
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
        var, "delta", "alpha"
    )]
}
# }}}

# morphing_tdb {{{
#' Morphing dry-bulb temperature
#'
#' Use monthly mean temperature, monthly maximum temperature and monthly minimum
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
morphing_tdb <- function (data_epw, tas, start_from = 2020L, cut_by = 10L, type = "shift") {
    morphing_from_mean(
        var = "dry_bulb_temperature",
        data_epw = data_epw,
        data_mean = tas,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
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
morphing_rh <- function (data_epw, hurs, start_from = 2020L, cut_by = 10L, type = "shift") {
    rh <- morphing_from_mean(
        var = "relative_humidity",
        data_epw = data_epw,
        data_mean = hurs,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )

    # reset RH > 100% to 100%
    rh[relative_humidity > units::set_units(100, "%"), relative_humidity := units::set_units(100, "%")]
}
# }}}

# morphing_tdew {{{
morphing_tdew <- function (tdb, rh) {
    psychrolib::SetUnitSystem("SI")
    tdew <- data.table::copy(tdb)[
        rh, on = c(setdiff(names(tdb), c("dry_bulb_temperature", "delta", "alpha"))),
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
morphing_pa <- function (data_epw, psl, start_from = 2020, cut_by = 10, type = "stretch") {
    morphing_from_mean(
        var = "atmospheric_pressure",
        data_epw = data_epw,
        data_mean = psl,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
}
# }}}

# morphing_hor_ir {{{
morphing_hor_ir <- function (data_epw, rlds, start_from = 2020, cut_by = 10, type = "stretch") {
    morphing_from_mean(
        var = "horizontal_infrared_radiation_intensity_from_sky",
        data_epw = data_epw,
        data_mean = rlds,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
}
# }}}

# morphing_glob_rad {{{
morphing_glob_rad <- function (data_epw, rsds, start_from = 2020, cut_by = 10, type = "stretch") {
    morphing_from_mean(
        var = "global_horizontal_radiation",
        data_epw = data_epw,
        data_mean = rsds,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
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
    norm_rad[, c("global_horizontal_radiation", "diffuse_horizontal_radiation",
        "day_of_year", "solar_angle") := NULL]
}
# }}}

# morphing_wind_speed {{{
morphing_wind_speed <- function (data_epw, sfcwind, start_from = 2020, cut_by = 10, type = "stretch") {
    morphing_from_mean(
        var = "wind_speed",
        data_epw = data_epw,
        data_mean = sfcwind,
        cut_by = cut_by,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
}
# }}}

# morphing_total_sky_cover {{{
morphing_total_sky_cover <- function (data_epw, clt, start_from = 2020, cut_by = 10) {
    var <- "total_sky_cover"
    data_mean <- preprocess_morphing(clt, leapyear = FALSE, start_from, cut_by)
    monthly <- unique(data_epw[, .SD, .SDcols = c("month", "day")])

    data_mean <- data_mean[monthly, on = c("month", "day")]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = c("month", "day"), allow.cartesian = TRUE]

    data.table::set(data, NULL, "value", units::drop_units(data$value))

    data[, `:=`(total_sky_cover = as.integer(round(pmax(0, pmin(10, value/10)))),
                delta = round(pmax(0, pmin(10, value/10))) - total_sky_cover,
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
        var, "delta", "alpha"
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
        "opaque_sky_cover", "delta", "alpha"
        )]
}
# }}}

# morphing_precipitation {{{
morphing_precipitation <- function (data_epw, pr, start_from = 2020L, cut_by = 10L, type = "stretch") {
    morphing_from_mean(
        var = "precipitable_water",
        data_epw = data_epw,
        data_mean = pr,
        start_from = start_from,
        cut_by = cut_by,
        type = type
    )
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
