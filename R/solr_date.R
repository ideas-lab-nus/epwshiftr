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
#' @returns An internal S7 object inheriting from `SolrDate`. The exact
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

        is_negative <- startsWith(dt_str, "-")
        base_value <- parse_datetime(dt_str, tz = "UTC")

        if (is.na(base_value)) {
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
        return(sprintf("Must use 'UTC' timezone, not '%s'", wrong) %|>% label)
    }

    TRUE
}

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
        value = checkmate_property(S7::class_POSIXct, check_datetime, tz = "UTC")
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
        value = checkmate_property(S7::class_POSIXct, check_datetime, tz = "UTC"),
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
    value <- if (is.na(S7::prop(x, "value"))) {
        "NOW"
    } else {
        format_datetime(S7::prop(x, "value"), as = as)
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
    as.POSIXct(x@value, tz = tz, ...)
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

#' Check whether an object is a parsed Solr date
#'
#' `is.solr_date()` returns `TRUE` when `x` is a `SolrDate` object created by
#' [solr_date()] or returned unchanged from it.
#'
#' @param x An object to test.
#'
#' @returns A single logical value.
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
