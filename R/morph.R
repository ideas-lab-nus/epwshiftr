# preprocess_morphing {{{
#' @importFrom checkmate assert_count
#' @importFrom data.table set
preprocess_morphing <- function (dt, leapyear = FALSE, years = NULL, labels = NULL) {
    # add datetime components
    data.table::set(dt, NULL, c("year", "month", "day", "hour", "minute"),
        list(data.table::year(dt$datetime),
             data.table::month(dt$datetime),
             data.table::mday(dt$datetime),
             data.table::hour(dt$datetime),
             data.table::minute(dt$datetime)
        )
    )

    # TODO: handle leapyear in mean, max, min
    if (!leapyear) dt <- dt[!J(2L, 29L), on = c("month", "day")]

    assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)

    if (is.null(years)) {
        data.table::set(dt, NULL, "interval", as.factor(dt$year))
    } else {
        dt <- dt[J(years), on = "year"]

        # make sure all years are matched and have the same length
        if (any(na <- is.na(dt$month))) {
            mis <- unique(dt$year[na])
            stop("Input data does not contain any data of year ", paste0("'", mis, "'", collapse = ", "), ".")
        }

        if (is.null(labels)) {
            data.table::set(dt, NULL, "interval", as.factor(dt$year))
        } else {
            if (is.factor(labels)) labels <- as.character(labels)
            assert_character(labels, any.missing = FALSE, len = length(years))

            y <- data.table(year = years, interval = as.factor(labels))

            dt[y, on = "year", interval := i.interval]
        }
    }

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

# remove_units {{{
remove_units <- function (data, var) {
    # remove units
    for (v in c(var, "delta", "alpha")) {
        if (v %in% names(data) && inherits(data[[v]], "units")) {
            set(data, NULL, v, units::drop_units(data[[v]]))
        }
    }

    data
}
# }}}

# morphing_epw {{{
#' Morphing EPW weather variables
#'
#' `morphing_epw()` takes an `epw_cmip6_data` object generated using
#' [extract_data()] and calculates future core EPW weather variables using
#' Morphing Method.
#'
#' The EPW weather variables that get morphed are listed in details.
#'
#' @param data An `epw_cmip6_data`object generated using [extract_data()]
#'
#' @param years An integer vector indicating the target years to be considered.
#'        If `NULL`, all years in input data will be considered. Default: `NULL`.
#'
#' @param labels A character or factor vector used for grouping input `years`.
#'        Usually are the outputs of [base::cut()]. `labels` should have the
#'        same length as `years`. If given, climate data of `years` grouped by
#'        `labels` will be averaged. Default: `NULL`.
#'
#' @param methods A named character giving the methods of morphing procedures of
#'        each variables. Possible variable names are `tdb`, `rh`, `p`,
#'        `hor_ir`, `glob_rad`, `wind`. Possible values are: `"stretch"`,
#'        `"shift"` and `"combined"`. For example: `c(tdb = "stretch", rh =
#'        "shift")`. `"combined"` is only applicable to `tdb`. The default
#'        morphing method for each variable is listed in the *Return* section.
#'        If `NULL`, the default methods will be used. Default: `NULL`.
#'
#' @return An `epw_cmip6_morphed` object, which is basically a list of 12 elements:
#'
#' | No.  | Element        | Type                       | Morphing Method | Description                                                       |
#' | ---: | -----          | -----                      | -----           | -----                                                             |
#' | 1    | `epw`          | [eplusr::Epw]              | N/A             | The original EPW file used for morphing                           |
#' | 2    | `tdb`          | [data.table::data.table()] | Stretch         | Data of dry-bulb temperature after morphing                       |
#' | 3    | `tdew`         | [data.table::data.table()] | Derived         | Data of dew-point temperature after morphing                      |
#' | 4    | `rh`           | [data.table::data.table()] | Stretch         | Data of relative humidity after morphing                          |
#' | 5    | `p`            | [data.table::data.table()] | Stretch         | Data of atmospheric pressure after morphing                       |
#' | 6    | `hor_ir`       | [data.table::data.table()] | Stretch         | Data of horizontal infrared radiation from the sky after morphing |
#' | 7    | `glob_rad`     | [data.table::data.table()] | Stretch         | Data of global horizontal radiation after morphing                |
#' | 8    | `norm_rad`     | [data.table::data.table()] | Derived         | Data of direct normal radiation after morphing                    |
#' | 9    | `diff_rad`     | [data.table::data.table()] | Stretch         | Data of diffuse horizontal radiation after morphing               |
#' | 10   | `wind`         | [data.table::data.table()] | Stretch         | Data of wind speed after morphing                                 |
#' | 11   | `total_cover`  | [data.table::data.table()] | Derived         | Data of total sky cover after morphing                            |
#' | 12   | `opaque_cover` | [data.table::data.table()] | Derived         | Data of opaque sky cover after morphing                           |
#'
#' Each [data.table::data.table()] listed above contains x columns
#'
#' | No.  | Column            | Type      | Description                                                                            |
#' | ---: | -----             | -----     | -----                                                                                  |
#' | 1    | `activity_drs`    | Character | Activity DRS (Data Reference Syntax)                                                   |
#' | 2    | `institution_id`  | Character | Institution identifier                                                                 |
#' | 3    | `source_id`       | Character | Model identifier                                                                       |
#' | 4    | `experiment_id`   | Character | Root experiment identifier                                                             |
#' | 5    | `member_id`       | Character | A compound construction from `sub_experiment_id` and `variant_label`                   |
#' | 6    | `table_id`        | Character | Table identifier                                                                       |
#' | 7    | `lat`             | Double    | The **averaged** values of input latitude                                              |
#' | 8    | `lon`             | Double    | The **averaged** values of input longitude                                             |
#' | 9    | `interval`        | Factor    | The year value of data morphed                                                         |
#' | 10   | **Variable Name** | Double    | The morphed data, where `Variable Name` is the corresponding EPW weather variable name |
#' | 11   | `delta`           | Double    | The shift factor. Will be `NA` for derivied values                                     |
#' | 12   | `alpha`           | Double    | The stretch factor. Will be `NA` for derivied values                                   |
#'
#' @references
#' Belcher, S., Hacker, J., Powell, D., 2005. Constructing design weather data
#' for future climates. Building Services Engineering Research and Technology
#' 26, 49–61. https://doi.org/10.1191/0143624405bt112oa
#'
#' @export
morphing_epw <- function (data, years = NULL, labels = NULL, methods = NULL) {
    assert_class(data, "epw_cmip6_data")
    if (is.null(methods)) {
        methods <- c(tdb = "stretch", rh = "stretch", p = "stretch",
            hor_ir = "stretch", glob_rad = "stretch", wind = "stretch"
        )
    } else {
        assert_character(methods, any.missing = FALSE, names = "named", unique = TRUE)
        assert_names(names(methods), subset.of = c("tdb", "rh", "p", "hor_ir", "glob_rad", "diff_rad", "wind"))
        methods_def <- list(tdb = "stretch", rh = "stretch", p = "stretch",
            hor_ir = "stretch", glob_rad = "stretch", diff_rad = "stretch",
            wind = "stretch"
        )
        methods <- unlist(utils::modifyList(methods_def, as.list(methods)))
    }

    data_cmip <- data.table::setDT(data$data)
    data_epw <- suppressMessages(data$epw$add_unit()$data())

    # NODE 6: Ta
    verbose("Morphing 'dry bulb temperature'...")
    tas <- data_cmip[J("tas"), on = "variable", nomatch = NULL]
    if (!nrow(tas)) {
        verbose("WARNING: Input does not contain any data of 'near-surface air temperature'. Skip.")
        tdb <- data.table()
    } else {
        tasmax <- data_cmip[J("tasmax"), on = "variable", nomatch = NULL]
        tasmin <- data_cmip[J("tasmin"), on = "variable", nomatch = NULL]
        if (!nrow(tasmax)) tasmax <- NULL
        if (!nrow(tasmin)) tasmin <- NULL
        tdb <- morphing_tdb(data_epw, tas, tasmax, tasmin, years, labels = labels, type = methods["tdb"])
    }

    # NODE 8: RH
    verbose("Morphing 'relative humidity'...")
    hurs <- data_cmip[J("hurs"), on = "variable", nomatch = NULL]
    if (!nrow(hurs)) {
        verbose("WARNING: Input does not contain any data of 'near-surface relative humidity'. Skip.")
        rh <- data.table()
    } else {
        hursmax <- data_cmip[J("hursmax"), on = "variable", nomatch = NULL]
        hursmin <- data_cmip[J("hursmin"), on = "variable", nomatch = NULL]
        if (!nrow(hursmax)) hursmax <- NULL
        if (!nrow(hursmin)) hursmin <- NULL
        rh <- morphing_rh(data_epw, hurs, hursmax, hursmin, years, labels = labels, type = methods["rh"])
    }

    # NODE 7: Tdew
    verbose("Morphing 'dew point temperature'...")
    if (!nrow(tdb) || !nrow(rh)) {
        verbose("WARNING: Input does not contain any data of 'near-surface air temperature' or 'near-surface relative humidity'. Skip.")
        tdew <- data.table()
    } else {
        tdew <- morphing_tdew(tdb, rh)
    }

    # NODE 9 Pa
    verbose("Morphing 'atmospheric pressure'...")
    psl <- data_cmip[J("psl"), on = "variable", nomatch = NULL]
    if (!nrow(psl)) {
        verbose("WARNING: Input does not contain any data of 'sea level pressure'. Skip.")
        p <- data.table()
    } else {
        p <- morphing_pa(data_epw, psl, years, labels = labels, type = methods["p"])
    }

    # NODE 10: Extraterrestrial direct normal radiation [NOT USED in EnergyPlus]
    # NODE 11: Extraterrestrial horizontal radiation [NOT USED in EnergyPlus]

    # NODE 12: Horizontal infrared radiation from the sky
    verbose("Morphing 'horizontal infrared radiation from the sky'...")
    data_epw[, horizontal_infrared_radiation_intensity_from_sky :=
        units::set_units(units::drop_units(horizontal_infrared_radiation_intensity_from_sky), "W/m^2"
    )]
    rlds <- data_cmip[J("rlds"), on = "variable", nomatch = NULL]
    if (!nrow(rlds)) {
        verbose("WARNING: Input does not contain any data of 'surface downwelling longware radiation'. Skip.")
        hor_ir <- data.table()
    } else {
        hor_ir <- morphing_hor_ir(data_epw, rlds, years, labels = labels, type = methods["hor_ir"])
    }

    # NODE 13: Global horizontal radiation
    verbose("Morphing 'global horizontal radiation'...")
    data_epw[, global_horizontal_radiation :=
        units::set_units(units::drop_units(global_horizontal_radiation), "W/m^2"
    )]
    rsds <- data_cmip[J("rsds"), on = "variable", nomatch = NULL]
    if (!nrow(rsds)) {
        verbose("WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.")
        glob_rad <- data.table()
    } else {
        glob_rad <- morphing_glob_rad(data_epw, rsds, years, labels = labels, type = methods["glob_rad"])
    }

    #!NODE 15: Diffuse horizontal radiation
    # NOTE: Since the extraterrestrial horizontal radiation is not used in
    # EnergyPlus. Here still use the original approach
    verbose("Morphing 'diffuse horizontal radiation'...")
    if (!nrow(glob_rad)) {
        verbose("WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.")
        diff_rad <- data.table()
    } else {
        diff_rad <- morphing_diff_rad(data_epw, glob_rad)
    }

    # NODE 14: Direct normal radiation
    verbose("Morphing 'direct normal radiation'...")
    if (!nrow(glob_rad)) {
        verbose("WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.")
        norm_rad <- data.table()
    } else {
        norm_rad <- morphing_norm_rad(glob_rad, diff_rad)
    }

    # NODE 16: Global horizontal illuminance [NOT USED in EnergyPlus]
    # NODE 17: Direct normal illuminance [NOT USED in EnergyPlus]
    # NODE 18: Diffuse horizontal illuminance [NOT USED in EnergyPlus]
    # NODE 19: Zenith luminance [NOT USED in EnergyPlus]
    # NODE 20: Wind direction [Keep the same]

    # NODE 21: Wind speed
    verbose("Morphing 'wind speed'...")
    sfcWind <- data_cmip[J("sfcWind"), on = "variable", nomatch = NULL]
    if (!nrow(sfcWind)) {
        verbose("WARNING: Input does not contain any data of 'near-surface wind speed'. Skip.")
        wind <- data.table()
    } else {
        wind <- morphing_wind_speed(data_epw, sfcWind, years, labels = labels, type = methods["wind"])
    }

    # NODE 22: Total sky cover
    verbose("Morphing 'total sky cover'...")
    clt <- data_cmip[J("clt"), on = "variable", nomatch = NULL]
    if (!nrow(clt)) {
        verbose("WARNING: Input does not contain any data of 'total cloud area fraction for the whole atmospheric column'. Skip.")
        total_cover <- data.table()
    } else {
        total_cover <- morphing_total_sky_cover(data_epw, clt, years, labels = labels)
    }

    # NODE 23: Opaque sky cover
    # Instead, it was assumed that the relation between total sky cover and
    # opaque sky cover remains the same under a changed climate. Therefore, the
    # equation for generating future opaque sky cover is as follows:
    verbose("Morphing 'opaque sky cover'...")
    if (!nrow(total_cover)) {
        verbose("WARNING: Input does not contain any data of 'total cloud area fraction for the whole atmospheric column'. Skip.")
        opaque_cover <- data.table()
    } else {
        opaque_cover <- morphing_opaque_sky_cover(data_epw, total_cover)
    }

    res <- list(epw = data$epw, tdb = tdb, tdew = tdew, rh = rh, p = p, hor_ir = hor_ir,
         glob_rad = glob_rad, norm_rad = norm_rad, diff_rad = diff_rad,
         wind = wind, total_cover = total_cover, opaque_cover = opaque_cover)

    # remove all units
    data$epw$drop_unit()
    for (l in res[-1L]) remove_units(l, intersect(names(l), names(data_epw)))

    class(res) <- "epw_cmip6_morphed"
    res
}
# }}}

# morphing_from_mean {{{
morphing_from_mean <- function (var, data_epw, data_mean, data_max = NULL, data_min = NULL,
                                type = c("shift", "stretch", "combined"),
                                years = NULL, labels = NULL) {
    type <- match.arg(type)

    if (!nrow(data_mean)) return(data.table())

    # calculate monthly average of EPW data
    monthly <- monthly_mean(data_epw, var)

    # get units
    u <- units(data_epw[[var]])

    # add cut interval and average by lon, lat, month and day (i.e. monthly
    # avarage) in CMIP6 data
    data_mean <- preprocess_morphing(data_mean, leapyear = FALSE, years = years, labels = labels)
    # this will automatically do unit conversions like K --> C
    data_mean <- align_units(data_mean, u)

    if (type == "combined" && !is.null(data_max) && !is.null(data_min)) {
        data_max <- preprocess_morphing(data_max, leapyear = FALSE, years = years, labels = labels)
        data_min <- preprocess_morphing(data_min, leapyear = FALSE, years = years, labels = labels)

        # this will automatically do unit conversions like K --> C
        data_max <- align_units(data_max, u)
        data_min <- align_units(data_min, u)

        # merge max and min CMIP6 values into mean data.table
        data_mean[data_max, on = c("activity_drs", "experiment_id", "institution_id",
            "source_id", "member_id", "table_id", "lat", "lon", "units", "month", "interval"),
            value_max := i.value
        ]
        data_mean[data_min, on = c("activity_drs", "experiment_id", "institution_id",
            "source_id", "member_id", "table_id", "lat", "lon", "units", "month", "interval"),
            value_min := i.value
        ]

        # If tasmax and tasmin is missing for some GCMs, reset them to tas.
        # Otherwise the final results will be NA
        # NOTE: By doing so, alpha for those GCMS will be zero and 'shift'
        # method is used. Warnings should be issued
        i_max <- data_mean[J(NA_real_), on = "value_max", which = TRUE]
        i_min <- data_mean[J(NA_real_), on = "value_min", which = TRUE]
        i <- unique(c(i_min, i_max))

        # construct case string
        cases <- data_mean[i, unique(sprintf("CMIP6.%s.%s.%s.%s.%s.%s",
            activity_drs, institution_id, source_id, experiment_id, member_id, table_id))]
        cases <- sprintf("[%i] '%s'", seq_along(cases), sort(cases))
        # issue warnings
        warning(sprintf("Case(s) below does not contains max or min of '%s' data. ", gsub("_", " ", var)),
            "'Shift' method will be used for it.\n", paste0(cases, collapse = "\n"),
            call. = FALSE
        )

        set(data_mean, i_max, "value_max", data_mean$value[i_max])
        set(data_mean, i_min, "value_min", data_mean$value[i_max])
    }

    # add datetime columns from the original EPW data into the monthly average of
    # CMIP6 data
    # after this every row in 'data' indicates a specific hour (as EPW has
    # hourly data)
    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE]

    # calculate delta, alpha and add EPW monthly average value
    data[monthly, on = "month", `:=`(delta = value - i.val_mean,
        epw_mean = i.val_mean, epw_max = i.val_max, epw_min = i.val_min
    )]

    if (type == "combined" && all(c("value_min", "value_max") %in% names(data))) {
        data[, alpha := ((value_max - epw_max) - (value_min - epw_min)) / (epw_max - epw_min)]
    } else {
        data[, alpha := value / epw_mean]
    }

    if (type == "shift") {
        data[, c(var) := units::set_units(get(var) + delta, u, mode = "standard")]
    } else if (type == "stretch") {
        data[, c(var) := units::set_units(get(var) * alpha, u, mode = "standard")]
    } else if (type == "combined") {
        if (all(c("value_min", "value_max") %in% names(data))) {
            data[, c(var) := units::set_units(get(var) + delta + alpha * (get(var) - epw_mean), u, mode = "standard")]
        } else {
            data[, c(var) := units::set_units(get(var) + delta + alpha * get(var), u, mode = "standard")]
        }
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
        # TODO: update units column
    )]
}
# }}}

# morphing_tdb {{{
morphing_tdb <- function (data_epw, tas, tasmax = NULL, tasmin = NULL, years = NULL, labels = NULL, type = "combined") {
    morphing_from_mean(
        var = "dry_bulb_temperature",
        data_epw = data_epw,
        data_mean = tas,
        data_max = tasmax,
        data_min = tasmin,
        years = years,
        labels = labels,
        type = type
    )
}
# }}}

# morphing_rh {{{
morphing_rh <- function (data_epw, hurs, hursmax = NULL, hursmin = NULL, years = NULL, labels = NULL, type = "combined") {
    rh <- morphing_from_mean(
        var = "relative_humidity",
        data_epw = data_epw,
        data_mean = hurs,
        data_max = hursmax,
        data_min = hursmin,
        years = years,
        labels = labels,
        type = type
    )

    # reset RH > 100% to 100%
    rh[relative_humidity > units::set_units(100, "%"), relative_humidity := units::set_units(100, "%")]
    rh
}
# }}}

# morphing_tdew {{{
#' @importFrom psychrolib GetTDewPointFromRelHum SetUnitSystem
morphing_tdew <- function (tdb, rh) {
    psychrolib::SetUnitSystem("SI")

    tdew <- data.table::copy(tdb)[
        rh, on = c(setdiff(names(tdb), c("dry_bulb_temperature", "delta", "alpha"))),
        relative_humidity := i.relative_humidity
    ]

    # TODO: issue warnings if there are any NAs in tdb or RH

    tdew[!is.na(dry_bulb_temperature) & !is.na(relative_humidity),
        dew_point_temperature := units::set_units(
            psychrolib::GetTDewPointFromRelHum(
                units::drop_units(dry_bulb_temperature),
                units::drop_units(relative_humidity) / 100
            ),
            degree_Celsius
        )
    ]

    set(tdew, NULL, c("delta", "alpha"), NA_real_)
    set(tdew, NULL, c("dry_bulb_temperature", "relative_humidity"), NULL)

    setcolorder(tdew,
        c(setdiff(names(tdew), c("dew_point_temperature", "delta", "alpha")),
          "dew_point_temperature", "delta", "alpha")
    )

    tdew
}
# }}}

# morphing_pa {{{
morphing_pa <- function (data_epw, psl, years = NULL, labels = NULL, type = "stretch") {
    morphing_from_mean(
        var = "atmospheric_pressure",
        data_epw = data_epw,
        data_mean = psl,
        years = years,
        labels = labels,
        type = type
    )
}
# }}}

# morphing_hor_ir {{{
morphing_hor_ir <- function (data_epw, rlds, years = NULL, labels = NULL, type = "stretch") {
    morphing_from_mean(
        var = "horizontal_infrared_radiation_intensity_from_sky",
        data_epw = data_epw,
        data_mean = rlds,
        years = years,
        labels = labels,
        type = type
    )
}
# }}}

# morphing_glob_rad {{{
morphing_glob_rad <- function (data_epw, rsds, years = NULL, labels = NULL, type = "stretch") {
    morphing_from_mean(
        var = "global_horizontal_radiation",
        data_epw = data_epw,
        data_mean = rsds,
        years = years,
        labels = labels,
        type = type
    )
}
# }}}

# morphing_diff_rad {{{
morphing_diff_rad <- function (data_epw, glob_rad) {
    diff_rad <- data.table::copy(glob_rad)
    if (!nrow(diff_rad)) return(data.table())
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
    if (!nrow(glob_rad) || !nrow(diff_rad)) return(data.table())
    norm_rad[, diffuse_horizontal_radiation := diff_rad$diffuse_horizontal_radiation]
    # calculate solar angle
    norm_rad[, day_of_year := data.table::yday(datetime)]
    norm_rad[, solar_angle := solar_angle(lat, lon, day_of_year, hour, 8)]
    norm_rad[, direct_normal_radiation := (global_horizontal_radiation - diffuse_horizontal_radiation) * abs(solar_angle)]
    norm_rad[, c("global_horizontal_radiation", "diffuse_horizontal_radiation",
        "day_of_year", "solar_angle") := NULL]
}
# }}}

# morphing_wind_speed {{{
morphing_wind_speed <- function (data_epw, sfcWind, years = NULL, labels = NULL, type = "stretch") {
    morphing_from_mean(
        var = "wind_speed",
        data_epw = data_epw,
        data_mean = sfcWind,
        years = years,
        labels = labels,
        type = type
    )
}
# }}}

# morphing_total_sky_cover {{{
morphing_total_sky_cover <- function (data_epw, clt, years = NULL, labels = NULL) {
    var <- "total_sky_cover"
    if (!nrow(clt)) return(data.table())
    data_mean <- preprocess_morphing(clt, leapyear = FALSE, years = years, labels = labels)
    monthly <- unique(data_epw[, .SD, .SDcols = c("month")])

    data_mean <- data_mean[monthly, on = c("month")]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE]

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
    if (!nrow(total_sky_cover)) return(data.table())
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
morphing_precipitation <- function (data_epw, pr, years = NULL, labels = NULL, type = "stretch") {
    morphing_from_mean(
        var = "precipitable_water",
        data_epw = data_epw,
        data_mean = pr,
        years = years,
        labels = labels,
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

# append_epw_data {{{
# Append EPW data in each case for merely comparison purpose
append_epw_data <- function (morphed) {
    epw <- morphed$epw
    epw$drop_unit()
    epw <- epw$data()

    mor <- morphed[names(morphed) != "epw"]

    lapply(mor, function (dt) {
        # get variable name
        var <- intersect(names(dt), names(epw)[-(1:7)])

        meta <- unique(dt[, .SD, .SDcols = c("experiment_id", "institution_id",
            "source_id", "member_id", "table_id", "lat", "lon", "interval")])

        base <- meta[, as.list(epw[, .SD, .SDcols = c(names(epw)[1:6], var)]), by = c(names(meta))]

        set(base, NULL, "interval", "EPW")

        # combine
        rbindlist(list(dt, base), fill = TRUE)
    })
}
# }}}

# future_epw {{{
#' Create future EPW files using morphed data
#'
#' @param morphed An `epw_cmip6_morphed` object created using [morphing_epw()].
#' @param by A character vector of columns to be used as grouping variables when
#'        creating EPW files. Should be a subeset of:
#'
#' * `"experiment"`: root experiment identifiers
#' * `"source"`: model identifiers
#' * `"variable"`: variable identifiers
#' * `"activity"`: activity identifiers
#' * `"frequency"`: sampling frequency
#' * `"variant"`: variant label
#' * `"resolution"`: approximate horizontal resolution
#' * `"longitude"`: averaged longitude of input data
#' * `"latitude"`: averaged latitude of input data
#'
#' @param dir The parent directory to save the generated EPW files. If not
#'        exist, it will be created first. Default: `"."`, i.e., current working
#'        directory.
#' @param separate If `TRUE`, each EPW file will be saved into a separate folder
#'        using grouping variables specified in `by`.
#' @param overwrite If `TRUE`, overwrite existing files if they exist. Default:
#'        `FALSE`.
#'
#' @return A list of generated [eplusr::Epw] objects, invisibly
#' @export
future_epw <- function (morphed, by = c("experiment", "source", "interval"),
                        dir = ".", separate = TRUE, overwrite = FALSE) {
    assert_class(morphed, "epw_cmip6_morphed")
    assert_string(dir)
    assert_flag(separate)

    epw <- morphed$epw

    suppressMessages(epw$drop_unit())
    data_epw <- epw$data()

    morphed <- morphed[names(morphed) != "epw"]

    # remove empty data and give warnings
    if (any((l <- sapply(morphed, nrow)) == 0L)) {
        v <- names(which(l == 0))
        d <- c(tdb = "Dry-bulb temperature",
               tdew = "Dew-point temperature",
               rh = "Relative humidity",
               p = "Atmospheric pressure",
               hor_ir = "Horizontal infrared radiation intensity from sky",
               glob_rad = "Global horizontal radiation",
               norm_rad = "Direct normal radiation",
               diff_rad = "Diffuse horizontal radiation",
               wind = "Wind speed",
               total_cover = "Total sky cover",
               opaque_cover = "Opaque sky cover"
        )

        v <- d[names(d) %in% v]
        warning("Empty morphed data found for variables listed below. Original data from EPW will be used:\n",
            paste0(sprintf(" [%i]: %s", seq_along(v), v), collapse = "\n"),
            call. = FALSE
        )

        morphed <- morphed[sapply(morphed, nrow) != 0L]
    }

    if (!length(morphed)) {
        stop("No morphed data found. Please run 'morphing_epw' first.")
    }

    # remove delta and alpha columns
    morphed <- lapply(morphed,
        function (dt) {
            # copy the original first
            data.table::set(data.table::copy(dt), NULL, c("delta", "alpha"), NULL)
        }
    )

    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", source_id = "source", interval = "interval",
              lon = "longitude", lat = "latitude"
    )
    assert_subset(by, choices = dict)
    cols_by <- unique(names(dict)[match(by, dict, 0L)])

    # columns of datetime
    cols_dt <- c("datetime", "year", "month", "day", "hour", "minute")

    for (m in morphed) {
        set(m, NULL, setdiff(names(m), c(intersect(names(data_epw), names(m)), cols_by, cols_dt)), NULL)
    }

    # merge into one
    merged <- Reduce(function(...) merge(..., by = c(cols_by, cols_dt)), morphed)

    # average by group
    merged <- merged[, lapply(.SD, mean), by = c(cols_dt, cols_by)]

    # in case there are decimal numbers for sky cover
    if ("total_sky_cover" %in% names(merged)) {
        set(merged, NULL, "total_sky_cover", as.integer(round(merged$total_sky_cover)))
    }
    if ("opaque_sky_cover" %in% names(merged)) {
        set(merged, NULL, "opaque_sky_cover", as.integer(round(merged$opaque_sky_cover)))
    }

    # add other variables in the original epw
    cols_other <- setdiff(names(data_epw), names(merged))
    complete <- merged[data_epw[, .SD, .SDcols = c("year", "month", "day", "hour", "minute", cols_other)],
        on = c("year", "month", "day", "hour", "minute")]

    # split data by grouping variables
    spl <- split(complete, by = cols_by)

    # get base file name
    prefix <- tools::file_path_sans_ext(basename(epw$path()))
    # get default name suffix
    suffix <- names(spl)
    # combine
    fn <- paste(prefix, suffix, "epw", sep = ".")

    dir <- normalizePath(dir, mustWork = FALSE)
    if (!dir.exists(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    if (!checkmate::test_directory(dir)) {
        stop(sprintf("Failed to create output directory '%s'"),
            normalizePath(dir, mustWork = FALSE)
        )
    }

    if (separate) {
        subdir <- vapply(spl, function (dt) dt[, do.call(file.path, .SD[1]), .SDcols = cols_by], character(1))
        output <- file.path(dir, subdir, fn)
    } else {
        output <- file.path(dir, fn)
    }

    invisible(lapply(seq_along(output), function (i) {
        # clone the original EPW
        new_epw <- epw$clone()

        # construct the case string used in disclaimer comment
        dash_sep <- function (...) paste0("'", paste(..., sep = "-"), "'")
        case <- spl[[i]][, do.call(dash_sep, .SD[1]), .SDcols = cols_by]
        # set disclaimer comment
        new_epw$comment1(disclaimer_comment(case))

        # set data
        suppressMessages(new_epw$set(spl[[i]]))

        # save
        new_dir <- dirname(output[i])
        if (!dir.exists(new_dir)) dir.create(new_dir, showWarnings = FALSE, recursive = TRUE)
        if (!checkmate::test_directory(new_dir)) {
            stop(sprintf("Failed to create output directory '%s'"),
                normalizePath(new_dir, mustWork = FALSE)
            )
        }
        new_epw$save(output[i], overwrite = overwrite)

        new_epw
    }))
}
# }}}

# disclaimer_comment {{{
disclaimer_comment <- function (case) {
    cmt <- paste0(
    "This climate change adapted weather file, which bases on ", case, " ",
    "ensemble data, has been generated using the epwshiftr tool V", utils::packageVersion("epwshiftr"), ". ",
    "The original weather file used for generating this climate change ",
    "adapted weather data may be copyrighted material. Therefore, generated ",
    "weather files can only be used by persons or entities who possess the ",
    "corresponding licensed weather file. ",
    "DISCLAIMER OF WARRANTIES: ",
    "The data is provided 'as is' without warranty of any kind, either expressed or implied. ",
    "The entire risk as to the quality and performance of the calculated climate change ",
    "weather data in this file is with you. In no event will the authors of the ",
    "weather file generation tool be liable to you for any damages, including ",
    "without limitation any lost profits, lost savings, or other incidental or ",
    "consequential damages arising out of the use or inability to use this data."
    )
}
# }}}
