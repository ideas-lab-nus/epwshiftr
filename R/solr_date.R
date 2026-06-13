# base R alternative to lubridate::parse_date_time()
parse_datetime <- function(x, tz = "UTC") {
    if (is.null(x) || all(is.na(x))) {
        return(as.POSIXct(x))
    }

    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = tz))
    }

    x_clean <- trimws(as.character(x))
    x_clean <- gsub("T", " ", x_clean, fixed = TRUE)
    x_clean <- gsub("Z$", "+0000", x_clean)
    x_clean <- gsub("[./]", "-", x_clean)

    # Case: "2017" -> "2017-01-01"
    x_clean <- gsub("^([0-9]{4})$", "\\1-01-01", x_clean)
    # Case: "201702" -> "2017-02-01"
    x_clean <- gsub("^([0-9]{4})-?([0-9]{2})$", "\\1-\\2-01", x_clean)
    # Case: "20170202" -> "2017-02-02"
    x_clean <- gsub("^([0-9]{4})-?([0-9]{2})-?([0-9]{2})", "\\1-\\2-\\3", x_clean)
    # Case: "+08" -> "+0800"
    x_clean <- gsub("([+-]\\d{2})$", "\\100", x_clean)
    # Case: "+08:00" -> "+0800"
    x_clean <- gsub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x_clean)

    formats <- c(
        "%Y-%m-%d %H:%M:%OS%z", # ISO (e.g. 2023-01-01 12:00:00+0000)
        "%Y-%m-%d %H:%M:%OS", # ymd_hms
        "%Y-%m-%d %H:%M", # ymd_hm
        "%Y-%m-%d %H", # ymd_h
        "%Y-%m-%d" # ymd
    )

    as.POSIXct(x_clean, tryFormats = formats, tz = tz, optional = TRUE)
}

#' Parse a Solr date, Date Math expression, or range
#'
#' `solr_date()` parses a scalar input into an internal S7 `SolrDate` object.
#' The resulting object can represent a single instant, a Date Math expression,
#' an unbounded boundary (`*`), or a Solr range.
#'
#' @param x A scalar input to parse. Supported inputs are:
#'
#'   * an existing `SolrDate` object, which is returned unchanged;
#'   * a scalar `Date` or `POSIXt` object;
#'   * a scalar numeric value, which is first converted to character;
#'   * a scalar character string representing either a single boundary or a
#'     complete Solr range expression.
#'
#'   `POSIXt` inputs must use the `"UTC"` timezone.
#'
#' @return An internal S7 object inheriting from `SolrDate`. The exact
#'   subclass is an implementation detail and may represent a single instant,
#'   a Date Math expression, an unbounded boundary, or a range.
#'
#' @details
#' Character inputs support the following forms:
#'
#' * Simplified dates such as `"2025"`, `"2025-02"`, `"2025-02-03"`, and
#'   `"20250203"`.
#' * Datetimes accepted by the internal parser, including ISO-like forms such as
#'   `"2025-01-15T12:30:45Z"`, timezone offsets like `"+08:00"`, and common
#'   separators such as `"/"` and `"."`.
#' * Solr Date Math expressions rooted at `NOW`, e.g. `"NOW"`,
#'   `"NOW-1YEAR"`, or `"NOW/DAY-1YEAR+6MONTHS"`.
#' * Fixed-base Date Math expressions of the form
#'   `"<datetime>Z<math>"`, e.g. `"2025-01-01T00:00:00Z+1MONTH"`.
#' * Solr range expressions using the exact separator `" TO "` and boundary
#'   brackets `\[\]` or `\{\}`, e.g. `"\[2000 TO 2010\]"`, `"\{2000 TO 2010\]"`, or
#'   `"\[* TO *\]"`.
#'
#' Supported Date Math operators are `+`, `-`, and `/`. Supported units are
#' `YEAR`, `YEARS`, `MONTH`, `MONTHS`, `DAY`, `DAYS`, `DATE`, `HOUR`, `HOURS`,
#' `MINUTE`, `MINUTES`, `SECOND`, `SECONDS`, `MILLI`, `MILLIS`, `MILLISECOND`,
#' and `MILLISECONDS`.
#'
#' Use `format()` or `as.character()` to render a parsed value. `format()`
#' supports `as = "iso"` and `as = "num"`. `as.POSIXct()` can be used on
#' instants; for ranges it returns the start boundary with a warning, and for
#' unbounded or Date Math values it errors because no single concrete instant is
#' available.
#'
#' @examples
#' solr_date("2025")
#' solr_date("2025-02")
#' solr_date("20250203")
#' solr_date("2025-01-15T12:30:45Z")
#'
#' solr_date("NOW")
#' solr_date("NOW/DAY-1YEAR+6MONTHS")
#' solr_date("2025-01-01T00:00:00Z+1MONTH")
#'
#' solr_date("[2000 TO 2010]")
#' solr_date("{2000 TO 2010]")
#' solr_date("[* TO *]")
#'
#' x <- solr_date("2025-01-15T12:30:45Z")
#' format(x)
#' format(x, as = "num")
#' as.character(x)
#' as.POSIXct(x)
#' is.solr_date(x)
#' print(x)
#'
#' @seealso [is.solr_date()]
#' @export
solr_date <- function(x) {
    if (S7::S7_inherits(x, SolrDate)) {
        return(x)
    }

    checkmate::assert_scalar(x)

    if (inherits(x, "POSIXt") || inherits(x, "Date")) {
        return(SolrDateInstant(value = x))
    }

    if (is.numeric(x)) {
        x <- as.character(x)
    }

    if (!is.character(x)) {
        stop("`x` must be a length-1 character string, `POSIXt` object, or `Date` object.")
    }

    if (grepl(" TO ", x, fixed = TRUE)) {
        solrdate_parse_range_string(x)
    } else {
        solrdate_parse_boundary_string(x)
    }
}

solrdate_parse_range_string <- function(x) {
    checkmate::assert_string(x)
    x <- trimws(x)

    start_bracket <- substr(x, 1L, 1L)
    end_bracket <- substr(x, nchar(x), nchar(x))
    parts <- strsplit(substr(x, 2L, nchar(x) - 1L), " TO ", fixed = TRUE)[[1L]]

    if (!start_bracket %in% c("[", "{") || !end_bracket %in% c("]", "}") || length(parts) != 2L) {
        stop(sprintf(
            "`x` contains invalid range syntax: '%s'. Expected format: [start TO end] or {start TO end}",
            x
        ))
    }

    SolrDateRange(
        start = solrdate_parse_boundary_string(trimws(parts[1L])),
        end = solrdate_parse_boundary_string(trimws(parts[2L])),
        start_inclusive = start_bracket == "[",
        end_inclusive = end_bracket == "]"
    )
}

solrdate_parse_boundary_string <- function(x) {
    checkmate::assert_string(x)
    x <- trimws(x)

    if (x == "*") {
        return(SolrDateUnbounded())
    }

    # Handle Date Math.
    if (grepl("^NOW", x, ignore.case = TRUE)) {
        # placeholder value for NOW-based Date Math
        base_value <- solrdate_na_posixct()
        dt_str <- "NOW"
        # strip "NOW" prefix and validate the remainder.
        math_str <- substring(x, 4L)
    } else {
        z_pos <- regexpr("Z", x, fixed = TRUE)
        if (z_pos == -1L) {
            dt_str <- x
            math_str <- ""
        } else {
            dt_str <- substring(x, 1L, z_pos)
            math_str <- substring(x, z_pos + 1L)
        }

        base_value <- solrdate_parse_datetime_value(dt_str)

        if (solrdate_value_is_missing(base_value)) {
            if (z_pos == -1L) {
                stop(sprintf(
                    "`x` contains an invalid datetime: '%s'.",
                    dt_str
                ))
            } else {
                stop(sprintf(
                    "`x` contains an invalid datetime before 'Z': '%s'.",
                    dt_str
                ))
            }
        }
    }

    if (!nzchar(math_str)) {
        SolrDateInstant(value = base_value)
    } else {
        if (!isTRUE(check_date_math_string(math_str))) {
            stop(sprintf(
                "`x` contains an invalid Date Math expression after the base datetime '%s': '%s'.",
                dt_str,
                math_str
            ))
        }
        SolrDateMath(value = base_value, math = math_str)
    }
}

solrdate_na_posixct <- function() {
    as.POSIXct(NA_real_, tz = "UTC")
}

# `checkmate::check_*()` functions returns `TRUE` if success while in S7 validators should return `NULL` on success
# this small helper allows us to convert checkmate results into the expected S7 validator output format.
`%|>%` <- function(result, name) {
    if (is.logical(result)) {
        return(NULL)
    }

    if (!is.null(name)) {
        result <- sprintf("`%s` validation failed: %s", name, result)
    }

    result
}

# Validate a Date Math suffix string (the part after NOW or after Z in ISO 8601).
# Valid Date Math operators: +, -, /
# Valid units: YEAR(S), MONTH(S), DAY(S), DATE, HOUR(S), MINUTE(S), SECOND(S),
#              MILLI, MILLIS, MILLISECOND(S)
check_date_math_string <- function(x) {
    UNITS <- paste(
        "YEARS?",
        "MONTHS?",
        "DAYS?",
        "DATE",
        "HOURS?",
        "MINUTES?",
        "SECONDS?",
        "MILLIS?",
        "MILLISECONDS?",
        sep = "|"
    )
    # Each segment: [+|-]<digits><UNIT> or /<UNIT>
    seg_pat <- sprintf("^([+\\-]\\d+(%s)|/(%s))+$", UNITS, UNITS)
    checkmate::check_string(x, pattern = seg_pat, ignore.case = TRUE)
}

check_datetime <- function(
    x,
    tz = "UTC",
    lower = NULL,
    upper = NULL,
    any.missing = TRUE,
    all.missing = TRUE,
    len = NULL,
    min.len = NULL,
    max.len = NULL,
    unique = FALSE,
    sorted = FALSE,
    null.ok = FALSE
) {
    checkmate::assert_string(tz, null.ok = TRUE)

    result <- checkmate::check_posixct(
        x,
        lower = lower,
        upper = upper,
        any.missing = any.missing,
        all.missing = all.missing,
        len = len,
        min.len = min.len,
        max.len = max.len,
        unique = unique,
        sorted = sorted,
        null.ok = null.ok
    )
    if (is.character(result)) {
        return(result)
    }

    tzone <- attr(x, "tzone", exact = TRUE)
    if (!identical(tzone, tz)) {
        wrong <- if (length(tzone)) as.character(tzone[1L]) else "<Empty>"
        return(sprintf("Must use 'UTC' timezone, not '%s'", wrong))
    }

    TRUE
}

solrdate_value_property <- function() {
    S7::new_property(
        class = S7::class_any,
        validator = function(value) {
            if (inherits(value, "POSIXct")) {
                return(checkmate_result(check_datetime(value, tz = "UTC")))
            }
            if (S7::S7_inherits(value, SolrDateTime)) {
                return(NULL)
            }
            "Must be a UTC POSIXct value or SolrDateTime object."
        }
    )
}

solrdate_value_is_missing <- function(x) {
    inherits(x, "POSIXct") && length(x) == 1L && is.na(x)
}

solrdate_parse_datetime_value <- function(x) {
    value <- parse_datetime(x, tz = "UTC")
    if (!is.na(value)) {
        return(value)
    }

    solrdate_parse_datetime_parts(x)
}

solrdate_parse_datetime_parts <- function(x) {
    x_clean <- trimws(as.character(x))
    x_clean <- gsub("T", " ", x_clean, fixed = TRUE)
    x_clean <- gsub("[./]", "-", x_clean)
    x_clean <- sub("Z$", "", x_clean)

    date_time <- strsplit(x_clean, " ", fixed = TRUE)[[1L]]
    if (!length(date_time) || length(date_time) > 2L) {
        return(solrdate_na_posixct())
    }

    date <- date_time[[1L]]
    time <- if (length(date_time) == 2L) date_time[[2L]] else ""

    date_parts <- regmatches(
        date,
        regexec("^([+-]?\\d{4,})(?:-(\\d{2})(?:-(\\d{2}))?)?$", date, perl = TRUE)
    )[[1L]]
    if (!length(date_parts)) {
        return(solrdate_na_posixct())
    }

    year <- as.integer(date_parts[[2L]])
    month <- if (nzchar(date_parts[[3L]])) as.integer(date_parts[[3L]]) else 1L
    day <- if (nzchar(date_parts[[4L]])) as.integer(date_parts[[4L]]) else 1L

    hour <- minute <- second <- millisecond <- 0L
    if (nzchar(time)) {
        time_parts <- regmatches(
            time,
            regexec("^(\\d{2})(?::(\\d{2})(?::(\\d{2})(?:\\.(\\d{1,9}))?)?)?$", time, perl = TRUE)
        )[[1L]]
        if (!length(time_parts)) {
            return(solrdate_na_posixct())
        }

        hour <- as.integer(time_parts[[2L]])
        minute <- if (nzchar(time_parts[[3L]])) as.integer(time_parts[[3L]]) else 0L
        second <- if (nzchar(time_parts[[4L]])) as.integer(time_parts[[4L]]) else 0L
        millisecond <- if (nzchar(time_parts[[5L]])) {
            as.integer(substr(paste0(time_parts[[5L]], "000"), 1L, 3L))
        } else {
            0L
        }
    }

    if (
        is.na(year) || is.na(month) || is.na(day) || is.na(hour) || is.na(minute) || is.na(second) || is.na(millisecond)
    ) {
        return(solrdate_na_posixct())
    }

    tryCatch(
        SolrDateTime(year, month, day, hour, minute, second, millisecond),
        error = function(e) solrdate_na_posixct()
    )
}

solrdate_is_leap_year <- function(year) {
    year %% 4L == 0L && (year %% 100L != 0L || year %% 400L == 0L)
}

solrdate_days_in_month <- function(year, month) {
    days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    if (month == 2L && solrdate_is_leap_year(year)) {
        return(29L)
    }
    days[[month]]
}

solrdate_check_parts <- function(year, month, day, hour, minute, second, millisecond) {
    if (month < 1L || month > 12L) {
        stop("SolrDateTime month must be between 1 and 12.", call. = FALSE)
    }
    if (day < 1L || day > solrdate_days_in_month(year, month)) {
        stop("SolrDateTime day is outside the valid range for the month.", call. = FALSE)
    }
    if (hour < 0L || hour > 23L) {
        stop("SolrDateTime hour must be between 0 and 23.", call. = FALSE)
    }
    if (minute < 0L || minute > 59L) {
        stop("SolrDateTime minute must be between 0 and 59.", call. = FALSE)
    }
    if (second < 0L || second > 59L) {
        stop("SolrDateTime second must be between 0 and 59.", call. = FALSE)
    }
    if (millisecond < 0L || millisecond > 999L) {
        stop("SolrDateTime millisecond must be between 0 and 999.", call. = FALSE)
    }
}

solrdate_parts <- function(value, now = Sys.time()) {
    if (S7::S7_inherits(value, SolrDateTime)) {
        return(value)
    }
    if (solrdate_value_is_missing(value)) {
        value <- now
    }
    if (inherits(value, "Date") && !inherits(value, "POSIXt")) {
        value <- as.POSIXct(value, tz = "UTC")
    }
    if (!inherits(value, "POSIXt")) {
        value <- solrdate_parse_datetime_value(value)
        if (S7::S7_inherits(value, SolrDateTime)) {
            return(value)
        }
    }

    lt <- as.POSIXlt(as.POSIXct(value, tz = "UTC"), tz = "UTC")
    sec <- floor(lt$sec)
    millisecond <- as.integer(round((lt$sec - sec) * 1000))
    if (millisecond == 1000L) {
        sec <- sec + 1L
        millisecond <- 0L
    }
    SolrDateTime(
        year = as.integer(lt$year + 1900L),
        month = as.integer(lt$mon + 1L),
        day = as.integer(lt$mday),
        hour = as.integer(lt$hour),
        minute = as.integer(lt$min),
        second = as.integer(sec),
        millisecond = millisecond
    )
}

solrdate_format_year <- function(year) {
    if (year < 0L) {
        sprintf("-%04d", abs(year))
    } else if (year > 9999L) {
        sprintf("+%04d", year)
    } else {
        sprintf("%04d", year)
    }
}

solrdate_format_parts <- function(x, as = c("iso", "num")) {
    as <- match.arg(as)
    year <- S7::prop(x, "year")
    month <- S7::prop(x, "month")
    day <- S7::prop(x, "day")
    hour <- S7::prop(x, "hour")
    minute <- S7::prop(x, "minute")
    second <- S7::prop(x, "second")
    millisecond <- S7::prop(x, "millisecond")
    year <- solrdate_format_year(year)

    if (identical(as, "num")) {
        if (hour > 0L || minute > 0L || second > 0L || millisecond > 0L) {
            warning(sprintf(
                "Loss of time information when rendering in 'num' format for SolrDate '%s'.",
                solrdate_format_parts(x)
            ))
        }
        return(sprintf("%s%02d%02d", year, month, day))
    }

    stamp <- sprintf("%s-%02d-%02dT%02d:%02d:%02d", year, month, day, hour, minute, second)
    if (millisecond > 0L) {
        stamp <- sprintf("%s.%03d", stamp, millisecond)
    }
    paste0(stamp, "Z")
}

solrdate_days_from_civil <- function(year, month, day) {
    year <- year - as.integer(month <= 2L)
    era <- if (year >= 0L) year %/% 400L else (year - 399L) %/% 400L
    yoe <- year - era * 400L
    month_prime <- month + if (month > 2L) -3L else 9L
    doy <- (153L * month_prime + 2L) %/% 5L + day - 1L
    doe <- yoe * 365L + yoe %/% 4L - yoe %/% 100L + doy
    era * 146097L + doe - 719468L
}

solrdate_civil_from_days <- function(days) {
    days <- days + 719468L
    era <- if (days >= 0L) days %/% 146097L else (days - 146096L) %/% 146097L
    doe <- days - era * 146097L
    yoe <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
    year <- yoe + era * 400L
    doy <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L)
    mp <- (5L * doy + 2L) %/% 153L
    day <- doy - (153L * mp + 2L) %/% 5L + 1L
    month <- mp + if (mp < 10L) 3L else -9L
    year <- year + as.integer(month <= 2L)
    list(year = as.integer(year), month = as.integer(month), day = as.integer(day))
}

solrdate_add_months <- function(x, amount) {
    year <- S7::prop(x, "year")
    month <- S7::prop(x, "month")
    month_index <- year * 12L + (month - 1L) + amount
    new_year <- month_index %/% 12L
    new_month <- month_index %% 12L + 1L
    new_day <- min(S7::prop(x, "day"), solrdate_days_in_month(new_year, new_month))
    SolrDateTime(
        new_year,
        new_month,
        new_day,
        S7::prop(x, "hour"),
        S7::prop(x, "minute"),
        S7::prop(x, "second"),
        S7::prop(x, "millisecond")
    )
}

solrdate_add_milliseconds <- function(x, amount) {
    day <- solrdate_days_from_civil(S7::prop(x, "year"), S7::prop(x, "month"), S7::prop(x, "day"))
    time <- (((S7::prop(x, "hour") * 60 + S7::prop(x, "minute")) * 60 + S7::prop(x, "second")) *
        1000 +
        S7::prop(x, "millisecond") +
        amount)
    day_offset <- floor(time / 86400000)
    time <- time - day_offset * 86400000
    date <- solrdate_civil_from_days(day + day_offset)
    hour <- time %/% 3600000
    time <- time - hour * 3600000
    minute <- time %/% 60000
    time <- time - minute * 60000
    second <- time %/% 1000
    millisecond <- time - second * 1000
    SolrDateTime(date$year, date$month, date$day, hour, minute, second, millisecond)
}

solrdate_normalize_unit <- function(unit) {
    unit <- toupper(unit)
    if (unit %in% c("YEAR", "YEARS")) {
        "YEAR"
    } else if (unit %in% c("MONTH", "MONTHS")) {
        "MONTH"
    } else if (unit %in% c("DAY", "DAYS", "DATE")) {
        "DAY"
    } else if (unit %in% c("HOUR", "HOURS")) {
        "HOUR"
    } else if (unit %in% c("MINUTE", "MINUTES")) {
        "MINUTE"
    } else if (unit %in% c("SECOND", "SECONDS")) {
        "SECOND"
    } else {
        "MILLI"
    }
}

solrdate_round <- function(x, unit) {
    unit <- solrdate_normalize_unit(unit)
    year <- S7::prop(x, "year")
    month <- S7::prop(x, "month")
    day <- S7::prop(x, "day")
    hour <- S7::prop(x, "hour")
    minute <- S7::prop(x, "minute")
    second <- S7::prop(x, "second")

    switch(
        unit,
        YEAR = SolrDateTime(year, 1L, 1L),
        MONTH = SolrDateTime(year, month, 1L),
        DAY = SolrDateTime(year, month, day),
        HOUR = SolrDateTime(year, month, day, hour),
        MINUTE = SolrDateTime(year, month, day, hour, minute),
        SECOND = SolrDateTime(year, month, day, hour, minute, second),
        MILLI = x
    )
}

solrdate_apply_math <- function(x, op, amount, unit) {
    unit <- solrdate_normalize_unit(unit)
    if (identical(op, "/")) {
        return(solrdate_round(x, unit))
    }

    amount <- if (identical(op, "-")) -amount else amount
    switch(
        unit,
        YEAR = solrdate_add_months(x, amount * 12L),
        MONTH = solrdate_add_months(x, amount),
        DAY = solrdate_add_milliseconds(x, amount * 86400000),
        HOUR = solrdate_add_milliseconds(x, amount * 3600000),
        MINUTE = solrdate_add_milliseconds(x, amount * 60000),
        SECOND = solrdate_add_milliseconds(x, amount * 1000),
        MILLI = solrdate_add_milliseconds(x, amount)
    )
}

solrdate_math_tokens <- function(x) {
    matches <- gregexpr("([+\\-])(\\d+)([A-Za-z]+)|/([A-Za-z]+)", x, perl = TRUE)[[1L]]
    if (identical(matches, -1L)) {
        return(list())
    }
    tokens <- regmatches(x, list(matches))[[1L]]
    if (!identical(paste(tokens, collapse = ""), x)) {
        stop(sprintf("Invalid Date Math expression: '%s'.", x), call. = FALSE)
    }

    lapply(tokens, function(token) {
        if (startsWith(token, "/")) {
            return(list(op = "/", amount = NA_integer_, unit = substring(token, 2L)))
        }
        parts <- regmatches(token, regexec("^([+\\-])(\\d+)([A-Za-z]+)$", token, perl = TRUE))[[1L]]
        list(op = parts[[2L]], amount = as.integer(parts[[3L]]), unit = parts[[4L]])
    })
}

solrdate_eval_math <- function(value, math, now = Sys.time()) {
    parts <- solrdate_parts(value, now = now)
    for (token in solrdate_math_tokens(math)) {
        parts <- solrdate_apply_math(parts, token$op, token$amount, token$unit)
    }
    parts
}

SolrDateTime <- S7::new_class(
    "SolrDateTime",
    properties = list(
        year = checkmate_property(S7::class_integer, checkmate::check_int),
        month = checkmate_property(S7::class_integer, checkmate::check_int, lower = 1L, upper = 12L),
        day = checkmate_property(S7::class_integer, checkmate::check_int, lower = 1L, upper = 31L),
        hour = checkmate_property(S7::class_integer, checkmate::check_int, lower = 0L, upper = 23L),
        minute = checkmate_property(S7::class_integer, checkmate::check_int, lower = 0L, upper = 59L),
        second = checkmate_property(S7::class_integer, checkmate::check_int, lower = 0L, upper = 59L),
        millisecond = checkmate_property(S7::class_integer, checkmate::check_int, lower = 0L, upper = 999L)
    ),
    constructor = function(year, month = 1L, day = 1L, hour = 0L, minute = 0L, second = 0L, millisecond = 0L) {
        year <- as.integer(year)
        month <- as.integer(month)
        day <- as.integer(day)
        hour <- as.integer(hour)
        minute <- as.integer(minute)
        second <- as.integer(second)
        millisecond <- as.integer(millisecond)
        solrdate_check_parts(year, month, day, hour, minute, second, millisecond)
        S7::new_object(
            S7::S7_object(),
            year = year,
            month = month,
            day = day,
            hour = hour,
            minute = minute,
            second = second,
            millisecond = millisecond
        )
    }
)

SolrDate <- S7::new_class("SolrDate", abstract = TRUE)

SolrDatePoint <- S7::new_class("SolrDatePoint", parent = SolrDate, abstract = TRUE)

SolrDateRange <- S7::new_class(
    "SolrDateRange",
    parent = SolrDate,
    properties = list(
        start = S7::new_property(SolrDatePoint),
        end = S7::new_property(SolrDatePoint),
        start_inclusive = checkmate_property(S7::class_logical, checkmate::check_flag, default = TRUE),
        end_inclusive = checkmate_property(S7::class_logical, checkmate::check_flag, default = TRUE)
    )
)

SolrDateUnbounded <- S7::new_class(
    "SolrDateUnbounded",
    parent = SolrDatePoint
)

SolrDateInstant <- S7::new_class(
    "SolrDateInstant",
    parent = SolrDatePoint,
    properties = list(
        value = solrdate_value_property()
    ),
    constructor = function(value) {
        if (inherits(value, "Date")) {
            value <- as.POSIXct(value, tz = "UTC")
        }
        S7::new_object(S7::S7_object(), value = value)
    }
)

SolrDateMath <- S7::new_class(
    "SolrDateMath",
    parent = SolrDatePoint,
    properties = list(
        value = solrdate_value_property(),
        math = checkmate_property(S7::class_character, check_date_math_string)
    )
)

# NOTE: have to use 'convert' instead of 'S7::convert'
# see: https://github.com/RConsortium/S7/issues/530
S7::method(convert, list(SolrDatePoint, SolrDateRange)) <- function(from, to, side = c("start", "end")) {
    if (side == "start") {
        start <- S7::prop(from, "value")
        end <- SolrDateUnbounded()
    } else if (side == "end") {
        start <- SolrDateUnbounded()
        end <- S7::prop(from, "value")
    }

    SolrDateRange(
        start = start,
        end = end,
        start_inclusive = TRUE,
        end_inclusive = TRUE
    )
}

format_datetime <- function(x, as = c("iso", "num")) {
    match.arg(as)

    if (S7::S7_inherits(x, SolrDateTime)) {
        return(solrdate_format_parts(x, as = as))
    }

    if (inherits(x, "POSIXct") && is.na(x)) {
        return("NOW")
    }

    if (as == "iso") {
        format(x, "%Y-%m-%dT%H:%M:%SZ")
    } else {
        lt <- as.POSIXlt(x)
        if (lt$hour > 0L || lt$min > 0L || lt$sec > 0) {
            warning(sprintf("Loss of time information when rendering in 'num' format for SolrDate '%s'.", format(x)))
        }
        format(x, "%Y%m%d")
    }
}

S7::method(format, SolrDateUnbounded) <- function(x, as = "iso") {
    "*"
}
S7::method(format, SolrDateInstant) <- function(x, as = "iso") {
    format_datetime(S7::prop(x, "value"), as = as)
}
S7::method(format, SolrDateMath) <- function(x, as = "iso") {
    value <- S7::prop(x, "value")
    value <- if (solrdate_value_is_missing(value)) {
        "NOW"
    } else {
        format_datetime(value, as = as)
    }

    paste0(value, S7::prop(x, "math"))
}
S7::method(format, SolrDateRange) <- function(x, as = "iso") {
    paste0(
        if (isTRUE(S7::prop(x, "start_inclusive"))) "[" else "{",
        format(S7::prop(x, "start"), as = as),
        " TO ",
        format(S7::prop(x, "end"), as = as),
        if (isTRUE(S7::prop(x, "end_inclusive"))) "]" else "}"
    )
}
S7::method(print, SolrDate) <- function(x) {
    cat(sprintf("<SolrDate>\n- \"%s\"\n", format(x)), sep = "")
}

S7::method(as.character, SolrDate) <- function(x, as = "iso", ...) {
    format(x, ...)
}
S7::method(as.POSIXct, SolrDateUnbounded) <- function(x, tz = "UTC", ...) {
    stop("Cannot coerce unbounded SolrDate to POSIXct.")
}
S7::method(as.POSIXct, SolrDateInstant) <- function(x, tz = "UTC", ...) {
    value <- x@value
    if (S7::S7_inherits(value, SolrDateTime)) {
        year <- S7::prop(value, "year")
        if (year < 0L || year > 9999L) {
            stop("Cannot coerce SolrDate with year outside 0000..9999 to POSIXct.")
        }
        value <- sprintf(
            "%04d-%02d-%02d %02d:%02d:%02d",
            year,
            S7::prop(value, "month"),
            S7::prop(value, "day"),
            S7::prop(value, "hour"),
            S7::prop(value, "minute"),
            S7::prop(value, "second")
        )
    }
    as.POSIXct(value, tz = tz, ...)
}
S7::method(as.POSIXct, SolrDateMath) <- function(x, tz = "UTC", ...) {
    stop(
        "Cannot coerce SolrDate with Date Math to POSIXct, as the actual datetime value depends on the context of evaluation."
    )
}
S7::method(as.POSIXct, SolrDateRange) <- function(x, tz = "UTC", ...) {
    warning("Using start datetime only when coercing SolrDateRange to POSIXct.")
    as.POSIXct(x@start, tz = tz, ...)
}

eval_date_math <- function(x, now = Sys.time()) {
    if (S7::S7_inherits(x, SolrDateUnbounded)) {
        return(x)
    }
    if (S7::S7_inherits(x, SolrDateInstant)) {
        if (solrdate_value_is_missing(S7::prop(x, "value"))) {
            return(SolrDateInstant(value = solrdate_parts(now)))
        }
        return(x)
    }
    if (S7::S7_inherits(x, SolrDateMath)) {
        return(SolrDateInstant(value = solrdate_eval_math(S7::prop(x, "value"), S7::prop(x, "math"), now = now)))
    }
    if (S7::S7_inherits(x, SolrDateRange)) {
        return(SolrDateRange(
            start = eval_date_math(S7::prop(x, "start"), now = now),
            end = eval_date_math(S7::prop(x, "end"), now = now),
            start_inclusive = S7::prop(x, "start_inclusive"),
            end_inclusive = S7::prop(x, "end_inclusive")
        ))
    }

    x
}

#' Check whether an object is a parsed Solr date
#'
#' `is.solr_date()` returns `TRUE` when `x` is a `SolrDate` object created by
#' [solr_date()] or returned unchanged from it.
#'
#' @param x An object to test.
#'
#' @return A single logical value.
#'
#' @examples
#' is.solr_date(solr_date("2025"))
#' is.solr_date("2025")
#'
#' @seealso [solr_date()]
#' @export
is.solr_date <- function(x) {
    S7::S7_inherits(x, SolrDate)
}

solrdt <- solr_date
is.solrdt <- is.solr_date

#' @keywords internal
#' @noRd
as.character.SolrDate <- function(x, ...) {
    format(x, ...)
}

# vim: fdm=marker :
