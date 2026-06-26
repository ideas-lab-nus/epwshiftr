#' @importFrom data.table rbindlist set setcolorder setnames
NULL

# get_nc_meta {{{
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    value <- name <- NULL
    # get all attributes
    atts <- get_nc_atts(file)

    # get metadata
    meta <- as.list(atts[
        list(
            "NC_GLOBAL",
            c("mip_era", "activity_id", "institution_id", "source_id",
              "experiment_id", "variant_label", "table_id", "grid_label",
              "nominal_resolution", "variable_id", "tracking_id")
        ),
        on = c("variable", "attribute"),
        {v <- unlist(value); names(v) <- attribute; v}])

    # get variable long name and units
    meta <- c(
        meta,
        data.table::setattr(
            atts[list(meta$variable_id, c("standard_name", "units")), on = c("variable", "attribute"), value],
            "names", c("standard_name", "units")
        )
    )

    # get time origin and unit
    c(
        meta,
        data.table::setattr(
            atts[list("time", c("units", "calendar")), on = c("variable", "attribute"), value],
            "names", c("time_units", "time_calendar")
        )
    )
}
# }}}

# get_nc_atts {{{
get_nc_atts <- function(x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    # global attributes
    glo <- rbindlist(lapply(seq_len(inq$ngatts), function (i) {
        RNetCDF::att.inq.nc(nc, "NC_GLOBAL", i - 1L)
    }))
    # set global attribute id to -1
    set(glo, NULL, "id", -1L)
    set(glo, NULL, "variable", "NC_GLOBAL")
    set(glo, NULL, c("length", "type"), NULL)
    set(glo, NULL, "value",
        lapply(seq_len(inq$ngatts), function (i) RNetCDF::att.get.nc(nc, "NC_GLOBAL", i - 1L))
    )
    setnames(glo, "name", "attribute")

    # get variables
    vars <- unique(get_nc_vars(nc)[natts > 0L, .SD, .SDcols = c("id", "name", "natts")])
    vars <- vars[, by = list(idx = seq_len(nrow(vars))), {
        nm <- lapply(seq_len(natts) - 1L, function (i) RNetCDF::att.inq.nc(nc, id, i)$name)
        att <- lapply(seq_len(natts) - 1L, function (i) RNetCDF::att.get.nc(nc, id, i))
        list(id = id, variable = rep(name, length(nm)), attribute = unlist(nm), value = att)
    }]
    set(vars, NULL, "idx", NULL)

    rbindlist(list(vars, glo), use.names = TRUE)
}
# }}}

# get_nc_vars {{{
get_nc_vars <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    vars <- rbindlist(fill = TRUE, lapply(seq_len(inq$nvars) - 1L, function (i) {
        res <- RNetCDF::var.inq.nc(nc, i)
        res <- res[names(res) != "dimids"]
        res[vapply(res, length, integer(1)) > 0L]
    }))

    vars
}
# }}}

# get_nc_dims {{{
get_nc_dims <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    rbindlist(lapply(seq_len(inq$ndims) - 1L, function (i) {
        RNetCDF::dim.inq.nc(nc, i)
    }))
}
# }}}

# get_nc_axes {{{
get_nc_axes <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    vars <- rbindlist(lapply(seq_len(inq$nvars) - 1L, function (i) {
        RNetCDF::var.inq.nc(nc, i)
    }))[, `:=`(axis = .I)]
    set(vars, NULL, setdiff(names(vars), c("axis", "name", "dimids")), NULL)
    setcolorder(vars, "axis")
    setnames(vars, c("axis", "variable", "dimension"))

    vars[]
}
# }}}

# get_nc_time {{{
normalize_cf_calendar <- function (calendar) {
    if (!length(calendar) || is.na(calendar[[1L]]) || !nzchar(trimws(calendar[[1L]]))) {
        return("standard")
    }

    tolower(as.character(calendar[[1L]]))
}

get_nc_time_att <- function (atts, attribute, default = NULL) {
    idx <- which(atts$variable == "time" & atts$attribute == attribute)

    if (!length(idx)) {
        if (!is.null(default)) return(default)
        stop(sprintf("Missing NetCDF 'time' attribute: '%s'.", attribute), call. = FALSE)
    }

    atts$value[[idx[[1L]]]]
}

CF_TIME_CALENDARS <- c(
    "standard",
    "gregorian",
    "proleptic_gregorian",
    "noleap",
    "365_day",
    "360_day",
    "366_day",
    "all_leap"
)

CF_TIME_UNIT_SECONDS <- c(
    seconds = 1,
    minutes = 60,
    hours = 3600,
    days = 86400
)

cf_time_check_calendar <- function (calendar) {
    if (!calendar %in% CF_TIME_CALENDARS) {
        stop("Invalid calendar specification", call. = FALSE)
    }
    calendar
}

cf_time_parse_unit <- function (unit) {
    aliases <- c(
        years = "years",
        year = "years",
        yr = "years",
        months = "months",
        month = "months",
        mon = "months",
        days = "days",
        day = "days",
        d = "days",
        hours = "hours",
        hour = "hours",
        hr = "hours",
        h = "hours",
        minutes = "minutes",
        minute = "minutes",
        min = "minutes",
        seconds = "seconds",
        second = "seconds",
        sec = "seconds",
        s = "seconds"
    )
    raw_unit <- tolower(unit)
    unit <- aliases[[raw_unit]]
    if (is.null(unit)) {
        stop(sprintf("Unsupported CF time unit: %s", raw_unit), call. = FALSE)
    }
    unit
}

cf_time_is_leap_gregorian <- function (year) {
    (year %% 4L == 0L & year %% 100L != 0L) | year %% 400L == 0L
}

cf_time_month_days <- function (year, month, calendar) {
    common <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    leap <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

    switch(
        calendar,
        "360_day" = rep(30L, length(month)),
        "365_day" = common[month],
        "noleap" = common[month],
        "366_day" = leap[month],
        "all_leap" = leap[month],
        ifelse(cf_time_is_leap_gregorian(year), leap[month], common[month])
    )
}

cf_time_valid_days <- function (parts, calendar) {
    valid <- !is.na(parts$year) & !is.na(parts$month) & !is.na(parts$day) &
        parts$month >= 1L & parts$month <= 12L & parts$day >= 1L
    days <- cf_time_month_days(parts$year, parts$month, calendar)
    valid & !is.na(days) & parts$day <= days
}

cf_time_parse_origin <- function (origin, calendar) {
    pattern <- paste0(
        "^\\s*",
        "([+-]?[0-9]{1,4})",
        "(?:-(0?[1-9]|1[0-2]))?",
        "(?:-(0?[1-9]|[12][0-9]|3[01]))?",
        "(?:[T ]",
        "([01]?[0-9]|2[0-3])",
        "(?::([0-5]?[0-9]))?",
        "(?::([0-5]?[0-9](?:\\.[0-9]+)?))?",
        ")?",
        "(?:\\s*(Z|UTC|[+-][0-9]{2}(?::?[0-9]{2})?))?",
        "\\s*$"
    )
    proto <- data.frame(
        year = integer(),
        month = integer(),
        day = integer(),
        hour = integer(),
        minute = integer(),
        second = numeric(),
        zone = character()
    )
    parts <- utils::strcapture(pattern, trimws(origin), proto)

    if (is.na(parts$year[[1L]])) {
        stop(
            "Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification",
            call. = FALSE
        )
    }

    parts$month[is.na(parts$month)] <- 1L
    parts$day[is.na(parts$day)] <- 1L
    parts$hour[is.na(parts$hour)] <- 0L
    parts$minute[is.na(parts$minute)] <- 0L
    parts$second[is.na(parts$second)] <- 0
    parts$tz <- ifelse(is.na(parts$zone), "+0000", parts$zone)
    parts$zone <- NULL

    if (!cf_time_valid_days(parts, calendar)) {
        stop(
            "Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification",
            call. = FALSE
        )
    }

    parts
}

cf_time_parse_definition <- function (units, calendar) {
    units <- as.character(units[[1L]])
    pattern <- "^\\s*([[:alpha:]]+)\\s+(since|after|from|ref|per)\\s+(.+?)\\s*$"
    match <- regexec(pattern, units, ignore.case = TRUE)
    parts <- regmatches(units, match)[[1L]]

    if (length(parts) != 4L) {
        stop("Definition string does not appear to be a CF-compliant time coordinate description", call. = FALSE)
    }

    list(
        unit = cf_time_parse_unit(parts[[2L]]),
        origin = cf_time_parse_origin(parts[[4L]], calendar)
    )
}

cf_time_gregorian_date2offset <- function (parts) {
    year1 <- parts$year - 1L
    corr <- ifelse(parts$month <= 2L, 0L, as.integer(cf_time_is_leap_gregorian(parts$year)) - 2L)
    365L * year1 + year1 %/% 4L - year1 %/% 100L + year1 %/% 400L +
        (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_gregorian_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    n400 <- d0 %/% 146097L
    d1 <- d0 %% 146097L
    n100 <- d1 %/% 36524L
    d2 <- d1 %% 36524L
    n4 <- d2 %/% 1461L
    d3 <- d2 %% 1461L
    n1 <- d3 %/% 365L
    year <- 400L * n400 + 100L * n100 + 4L * n4 + n1
    year <- ifelse(n100 == 4L | n1 == 4L, year, year + 1L)
    year1 <- year - 1L
    leap <- cf_time_is_leap_gregorian(year)
    jan1 <- 365L * year1 + year1 %/% 4L - year1 %/% 100L + year1 %/% 400L + 1L
    prior_days <- offsets - jan1 + ifelse(offsets < jan1 + 59L + as.integer(leap), 0L, 2L - as.integer(leap))
    month <- (12L * prior_days + 373L) %/% 367L
    day <- offsets - cf_time_gregorian_date2offset(data.frame(year = year, month = month, day = 1L)) + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_365_date2offset <- function (parts) {
    corr <- ifelse(parts$month <= 2L, 0L, -2L)
    365L * (parts$year - 1L) + (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_365_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    year <- d0 %/% 365L + 1L
    d1 <- d0 %% 365L
    corr <- ifelse(d1 < 59L, 0L, 2L)
    month <- (12L * (d1 + corr) + 373L) %/% 367L
    day <- d1 - (367L * month - 362L) %/% 12L + corr + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_366_date2offset <- function (parts) {
    corr <- ifelse(parts$month <= 2L, 0L, -1L)
    366L * (parts$year - 1L) + (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_366_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    year <- d0 %/% 366L + 1L
    d1 <- d0 %% 366L
    corr <- ifelse(d1 < 60L, 0L, 1L)
    month <- (12L * (d1 + corr) + 373L) %/% 367L
    day <- d1 - (367L * month - 362L) %/% 12L + corr + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_date2offset <- function (parts, origin, calendar) {
    switch(
        calendar,
        "360_day" = (parts$year - origin$year) * 360L +
            (parts$month - origin$month) * 30L + parts$day - origin$day,
        "365_day" = cf_time_365_date2offset(parts) - cf_time_365_date2offset(origin),
        "noleap" = cf_time_365_date2offset(parts) - cf_time_365_date2offset(origin),
        "366_day" = cf_time_366_date2offset(parts) - cf_time_366_date2offset(origin),
        "all_leap" = cf_time_366_date2offset(parts) - cf_time_366_date2offset(origin),
        cf_time_gregorian_date2offset(parts) - cf_time_gregorian_date2offset(origin)
    )
}

cf_time_offset2date <- function (offsets, origin, calendar) {
    switch(
        calendar,
        "360_day" = {
            year <- origin$year + offsets %/% 360L
            month <- origin$month + (offsets %% 360L) %/% 30L
            day <- origin$day + offsets %% 30L

            over <- day > 30L
            day[over] <- day[over] - 30L
            month[over] <- month[over] + 1L
            over <- month > 12L
            month[over] <- month[over] - 12L
            year[over] <- year[over] + 1L

            data.frame(year = year, month = month, day = day)
        },
        "365_day" = cf_time_365_offset2date(offsets + cf_time_365_date2offset(origin)),
        "noleap" = cf_time_365_offset2date(offsets + cf_time_365_date2offset(origin)),
        "366_day" = cf_time_366_offset2date(offsets + cf_time_366_date2offset(origin)),
        "all_leap" = cf_time_366_offset2date(offsets + cf_time_366_date2offset(origin)),
        cf_time_gregorian_offset2date(offsets + cf_time_gregorian_date2offset(origin))
    )
}

cf_time_origin_posix <- function (origin, tz) {
    ISOdatetime(origin$year[[1L]], origin$month[[1L]], 1L, 0L, 0L, 0, tz = tz) +
        (origin$day[[1L]] - 1L) * 86400 +
        origin$hour[[1L]] * 3600 +
        origin$minute[[1L]] * 60 +
        origin$second[[1L]]
}

cf_time_add_seconds_to_fields <- function (fields, seconds, origin, calendar) {
    total_seconds <- fields$hour * 3600 + fields$minute * 60 + fields$second + seconds
    day_offsets <- cf_time_date2offset(fields, origin, calendar) + total_seconds %/% 86400L
    seconds <- round(total_seconds %% 86400L, 3L)

    fields <- cf_time_offset2date(day_offsets, origin, calendar)
    fields$hour <- seconds %/% 3600L
    fields$minute <- (seconds %% 3600L) %/% 60L
    fields$second <- seconds %% 60L
    fields
}

cf_time_offsets2fields <- function (offsets, unit, origin, calendar) {
    if (unit %in% names(CF_TIME_UNIT_SECONDS)) {
        seconds <- offsets * CF_TIME_UNIT_SECONDS[[unit]] +
            origin$hour * 3600 + origin$minute * 60 + origin$second
        day_offsets <- seconds %/% 86400L
        seconds <- round(seconds %% 86400L, 3L)

        fields <- cf_time_offset2date(day_offsets, origin, calendar)
        fields$hour <- seconds %/% 3600L
        fields$minute <- (seconds %% 3600L) %/% 60L
        fields$second <- seconds %% 60L
        return(fields)
    }

    whole_offsets <- offsets %/% 1L
    fractional_offsets <- offsets - whole_offsets
    fields <- origin[rep(1L, length(offsets)), c("year", "month", "day", "hour", "minute", "second")]
    if (unit == "months") {
        months <- fields$month + whole_offsets - 1L
        fields$month <- months %% 12L + 1L
        fields$year <- fields$year + months %/% 12L
        fractional_seconds <- fractional_offsets * 30 * 86400
    } else {
        fields$year <- fields$year + whole_offsets
        fractional_seconds <- fractional_offsets * 365 * 86400
    }

    if (any(fractional_seconds != 0)) {
        fields <- cf_time_add_seconds_to_fields(fields, fractional_seconds, origin, calendar)
    }

    fields
}

cf_time_fields2posix <- function (fields, origin, calendar, tz) {
    if (calendar %in% c("standard", "gregorian", "proleptic_gregorian")) {
        return(as.POSIXct(
            ISOdatetime(fields$year, fields$month, fields$day, fields$hour, fields$minute, fields$second, tz = tz),
            tz = tz
        ))
    }

    origin_time <- cf_time_origin_posix(origin, tz)
    day_offsets <- cf_time_date2offset(fields, origin, calendar)
    second_offsets <- fields$hour * 3600 + fields$minute * 60 + fields$second -
        (origin$hour[[1L]] * 3600 + origin$minute[[1L]] * 60 + origin$second[[1L]])

    as.POSIXct(origin_time + day_offsets * 86400 + second_offsets, tz = tz)
}

parse_cf_time <- function (offsets, units, calendar = "standard", tz = "UTC") {
    calendar <- normalize_cf_calendar(calendar)
    calendar <- cf_time_check_calendar(calendar)
    units <- as.character(units[[1L]])

    if (!is.numeric(offsets) || anyNA(offsets) || any(!is.finite(offsets))) {
        stop("Invalid offsets for CF time coordinate.", call. = FALSE)
    }
    dim(offsets) <- NULL

    parsed <- cf_time_parse_definition(units, calendar)
    fields <- cf_time_offsets2fields(offsets, parsed$unit, parsed$origin, calendar)
    posix_time <- cf_time_fields2posix(fields, parsed$origin, calendar, tz)

    data.table::setattr(posix_time, "cf_units", units)
    data.table::setattr(posix_time, "cf_calendar", calendar)
    posix_time
}

get_nc_time <- function (x, range = FALSE) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    n <- get_nc_dims(nc)[list("time"), on = "name", length]

    # get variable attributes
    atts <- get_nc_atts(nc)

    time_units <- get_nc_time_att(atts, "units")
    time_calendar <- get_nc_time_att(atts, "calendar", default = "standard")

    if (range) {
        offsets <- c(
            RNetCDF::var.get.nc(nc, "time", 1, 1),
            RNetCDF::var.get.nc(nc, "time", n, 1)
        )
    } else {
        offsets <- RNetCDF::var.get.nc(nc, "time", 1, n)
    }

    parse_cf_time(offsets, time_units, time_calendar)
}
# }}}

# match_nc_time {{{
match_nc_time <- function (x, years = NULL) {
    checkmate::assert_integerish(years, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    time <- get_nc_time(x)

    if (is.null(years)) {
        list(datetime = list(time), which = list(seq_along(time)))
    } else {
        y <- data.table::year(time)
        i <- lapply(as.integer(years), function (x) which(y == x))

        j <- 1L
        l <- vector("list", length(i))
        l[[1L]] <- i[[1L]]
        for (m in i[-1L]) {
            if (!length(m)) next
            if (length(l[[j]]) && (l[[j]][length(l[[j]])] + 1L) == m[1L]) {
                l[[j]] <- c(l[[j]], m)
            } else {
                j <- j + 1L
                l[[j]] <- m
            }
        }

        list(datetime = lapply(l, function (idx) time[idx]), which = l)
    }
}
# }}}

# nocov start
# rechunk_nc_dims {{{
rechunk_nc_dims <- function (nc, out_file) {
    if (Sys.which("nccopy") == "") return()

    out_file <- normalizePath(out_file, "/", mustWork = FALSE)

    system2("nccopy", c(
        "-k", "nc4",
        "-u",
        "-c", "time/365,lon/1,lat/1",
        paste0("'", nc, "'"), paste0("'", out_file, "'")
    ))

    normalizePath(out_file)
}
# }}}

# permute_nc_dims {{{
permute_nc_dims <- function (nc, out_file) {
    if (Sys.which("ncpdq") == "") return()

    out_file <- normalizePath(out_file, "/", mustWork = FALSE)

    system2("ncpdq", c(
        "--no_tmp_fl",
        "-h",
        "-O",
        "-a", "lat,lon,time",
        paste0("'", nc, "'"), paste0("'", out_file, "'")
    ))

    normalizePath(out_file)
}
# }}}

# reorganize_nc_dims {{{
# Ref: https://github.com/ebimodeling/model-drivers/tree/master/met/cruncep
reorganize_nc_dims <- function (nc) {
    f <- rechunk_nc_dims(nc, tempfile(fileext = ".nc"))
    if (is.null(f)) return(nc)
    re <- permute_nc_dims(f, nc)
    unlink(f)

    if (is.null(re)) return(nc)

    re
}
# }}}
# nocov end
