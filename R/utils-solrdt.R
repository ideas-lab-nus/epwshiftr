#' Parse Solr DateTime String
#'
#' Parse datetime strings in various formats supported by Solr's DateRangeField.
#' This function handles simplified formats (year, year-month, date) as well as
#' full ISO-8601 formats with optional Date Math expressions, and range syntax.
#'
#' The function always returns a `SolrDateTime` object for consistency.
#'
#' All results are returned in UTC timezone.
#'
#' @param x A character vector of datetime strings to parse, `POSIXt` objects,
#'        or `Date` objects. `NA` values are not allowed and will cause an
#'        error.
#'
#' @return A `SolrDateTime` object, which is a vectorized S3 class containing:
#'
#'   - `start`: POSIXct vector of start datetimes (UTC). For unbounded ranges
#'     (e.g., `[* TO 2025]`), the start will be `NA`
#'   - `end`: POSIXct vector of end datetimes (UTC). For unbounded ranges (e.g.,
#'     `[2000 TO *]`), the end will be `NA`
#'   - `start_inclusive`: Logical vector, `TRUE` if start boundary is inclusive (`[`)
#'   - `end_inclusive`: Logical vector, `TRUE` if end boundary is inclusive (`]`)
#'   - `is_range`: Logical vector, `TRUE` for range inputs, `FALSE` for single datetimes
#'   - `original`: Character vector of original input strings with whitespaces trimmed
#'
#'   The `SolrDateTime` class supports vectorization and standard vector operations
#'   like subsetting (`[`), `length()`, `c()`, etc.
#'
#'   Use `is_solrdt_range(x)` to check which elements are ranges.
#'   Use `is_solrdt_start_unbounded(x)` or `is_solrdt_end_unbounded(x)` to check for unbounded ranges.
#'
#' @details
#' The function supports the following input formats:
#'
#' ## Simplified formats (DateRangeField style)
#'
#' - Year only: `"2025"` -> start of 2025 (2025-01-01 00:00:00)
#' - Year-Month: `"2025-01"` -> start of January 2025 (2025-01-01 00:00:00)
#' - Date: `"2025-01-15"` -> start of that day (2025-01-15 00:00:00)
#' - YYYYMMDD: `"20250115"` -> start of that day (2025-01-15 00:00:00)
#'
#' ## ISO-8601 formats
#'
#' - Full datetime: `"2025-01-15T12:30:45Z"`
#' - With milliseconds: `"2025-01-15T12:30:45.123Z"`
#' - Lenient format: `"2025-1-15T12:30:45Z"` (missing leading zeros)
#'
#' ## Date Math expressions (relative to NOW or a base date)
#'
#' - `"NOW"` -> current time
#' - `"NOW+1YEAR"` -> one year from now
#' - `"NOW-1DAY"` -> one day ago
#' - `"NOW/DAY"` -> start of today
#' - `"2025-01-01T00:00:00Z+5DAYS"` -> 5 days after the base date
#' - `"NOW/DAY+6MONTHS+3DAYS"` -> 6 months and 3 days from start of today
#'
#' ## Range syntax (DateRangeField specific)
#'
#' - `"[2000 TO 2014]"` -> range from 2000 to 2014 (inclusive)
#' - \code{"[2000 TO 2014\}"} -> range from 2000 to 2014 (exclusive end)
#' - \code{"\{2000 TO 2014]"} -> range from 2000 to 2014 (exclusive start)
#' - `"[* TO *]"` -> unbounded range (all time)
#' - `"[2000-01 TO 2014-12]"` -> range with month precision
#' - `"[2014-05-21T12:00:00Z TO *]"` -> from specific time to unbounded
#'
#' **Supported Date Math operators:**
#' - `+` : Addition
#' - `-` : Subtraction
#' - `/` : Round down to the start of the unit
#'
#' ## Supported time units
#' - `YEAR`, `YEARS`
#' - `MONTH`, `MONTHS`
#' - `DAY`, `DAYS`, `DATE`
#' - `HOUR`, `HOURS`
#' - `MINUTE`, `MINUTES`
#' - `SECOND`, `SECONDS`
#' - `MILLI`, `MILLIS`, `MILLISECOND`, `MILLISECONDS`
#'
#' @note
#' - The 'Z' suffix (indicating UTC) is optional for simplified formats but
#'   required for full ISO-8601 formats when not using Date Math.
#' - Negative years (BC dates) are supported: `"-1500-01-01T00:00:00Z"`
#' - All parsed times are returned in UTC timezone.
#' - Range syntax uses `*` to represent unbounded (infinite) dates, which are
#'   represented as `NA` in the start or end fields.
#' - `NA` values in input are not allowed. Parsing failures will result in an error.
#' - `POSIXt` input is automatically converted to UTC and wrapped in `SolrDateTime`.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Simplified formats
#' solrdt_parse(2024)
#' solrdt_parse("2025")
#' solrdt_parse("2025-01")
#' solrdt_parse("2025-01-15")
#'
#' # ISO-8601 formats
#' solrdt_parse("2025-01-15T12:30:45Z")
#' solrdt_parse("2025-01-15T12:30:45.123Z")
#'
#' # Date Math
#' solrdt_parse("NOW")
#' solrdt_parse("NOW+1YEAR")
#' solrdt_parse("NOW-1DAY")
#' solrdt_parse("NOW/DAY")
#' solrdt_parse("2025-01-01T00:00:00Z+5DAYS")
#'
#' # Range syntax
#' solrdt_parse("[2000 TO 2014]")
#' solrdt_parse("[* TO *]") # Unbounded range
#' solrdt_parse("[* TO 2025]") # Unbounded start
#'
#' # Exclusive boundaries
#' solrdt_parse("{2000 TO 2014]") # Exclusive start
#' solrdt_parse("[2000 TO 2014}") # Exclusive end
#'
#' # Vector input
#' solrdt_parse(c("2025", "2025-06", "NOW+1YEAR"))
#' solrdt_parse(c("[2000 TO 2014]", "2025"))
#'
#' # POSIXt input
#' solrdt_parse(Sys.time())
#' solrdt_parse(as.POSIXct("2025-01-01", tz = "UTC"))
#'
#' # Date input
#' solrdt_parse(as.Date("2025-01-01"))
#' solrdt_parse(Sys.Date())
#'
#' # Check for ranges and unbounded
#' result <- solrdt_parse(c("[* TO 2025]", "2020", "[2000 TO *]"))
#' is_solrdt_range(result) # c(TRUE, FALSE, TRUE)
#' is_solrdt_start_unbounded(result) # c(TRUE, FALSE, FALSE)
#' is_solrdt_end_unbounded(result) # c(FALSE, FALSE, TRUE)
#' }
#'
#' @export
solrdt_parse <- function(x) {
    if (is.numeric(x)) {
        x <- as.character(x)
    }
    # Handle POSIXt input
    if (inherits(x, "POSIXt")) {
        # Convert to UTC
        x_utc <- as.POSIXct(x, tz = "UTC")
        # Format as ISO-8601 string for original
        original <- format(x_utc, "%Y-%m-%dT%H:%M:%SZ")

        solrdt_new(
            start = x_utc,
            end = x_utc,
            start_inclusive = rep(TRUE, length(x)),
            end_inclusive = rep(TRUE, length(x)),
            is_range = rep(FALSE, length(x)),
            original = original
        )
    } else if (inherits(x, "Date")) {
        # Handle Date input
        # Convert to POSIXct at midnight UTC
        x_utc <- as.POSIXct(format(x, "%Y-%m-%d"), tz = "UTC")
        # Format as ISO-8601 string for original
        original <- format(x_utc, "%Y-%m-%dT%H:%M:%SZ")

        solrdt_new(
            start = x_utc,
            end = x_utc,
            start_inclusive = rep(TRUE, length(x)),
            end_inclusive = rep(TRUE, length(x)),
            is_range = rep(FALSE, length(x)),
            original = original
        )
    } else {
        # Validate character input (no NA allowed)
        checkmate::assert_character(x, any.missing = FALSE)

        # Parse all inputs (preserving original strings)
        result_list <- lapply(seq_along(x), function(i) {
            solrdt_parse_string(x[i], original = x[i])
        })

        # Extract components (all in UTC)
        starts <- lapply(result_list, function(r) r$start)
        ends <- lapply(result_list, function(r) r$end)
        start_inclusives <- vapply(result_list, function(r) r$start_inclusive, logical(1))
        end_inclusives <- vapply(result_list, function(r) r$end_inclusive, logical(1))
        is_ranges <- vapply(result_list, function(r) r$is_range, logical(1))
        originals <- vapply(result_list, function(r) r$original, character(1))

        # Combine into vectorized SolrDateTime (always UTC)
        solrdt_new(
            start = do.call(c, starts),
            end = do.call(c, ends),
            start_inclusive = start_inclusives,
            end_inclusive = end_inclusives,
            is_range = is_ranges,
            original = originals
        )
    }
}

# Create a SolrDateTime object (vectorized)
# All datetimes are in UTC
solrdt_new <- function(
    start,
    end,
    start_inclusive = TRUE,
    end_inclusive = TRUE,
    is_range = FALSE,
    original = NA_character_
) {
    # Ensure all inputs have the same length
    n <- max(
        length(start),
        length(end),
        length(start_inclusive),
        length(end_inclusive),
        length(is_range),
        length(original)
    )

    # Recycle vectors to the same length
    if (length(start) == 1L && n > 1L) {
        start <- rep(start, n)
    }
    if (length(end) == 1L && n > 1L) {
        end <- rep(end, n)
    }
    if (length(start_inclusive) == 1L && n > 1L) {
        start_inclusive <- rep(start_inclusive, n)
    }
    if (length(end_inclusive) == 1L && n > 1L) {
        end_inclusive <- rep(end_inclusive, n)
    }
    if (length(is_range) == 1L && n > 1L) {
        is_range <- rep(is_range, n)
    }
    if (length(original) == 1L && n > 1L) {
        original <- rep(original, n)
    }

    structure(
        list(
            start = as.POSIXct(start, tz = "UTC"),
            end = as.POSIXct(end, tz = "UTC"),
            start_inclusive = start_inclusive,
            end_inclusive = end_inclusive,
            is_range = is_range,
            original = as.character(original)
        ),
        class = "SolrDateTime",
        tzone = "UTC"
    )
}

# Parse a Solr datetime string to SolrDateTime object
# Handles both single datetimes and range syntax
solrdt_parse_string <- function(x, original = x) {
    x_trimmed <- trimws(x)
    # Save trimmed version as original
    original_trimmed <- x_trimmed

    # Check if it's a range syntax: [start TO end] or {start TO end}
    if (grepl(" TO ", x_trimmed, fixed = TRUE)) {
        result <- solrdt_parse_range(x_trimmed)
        result$original <- original_trimmed
        result
    } else {
        # Single datetime
        parsed <- solrdt_parse_single(x_trimmed)

        # Return as non-range object
        solrdt_new(
            start = parsed,
            end = parsed,
            start_inclusive = TRUE,
            end_inclusive = TRUE,
            is_range = FALSE,
            original = original_trimmed
        )
    }
}

# Parse range syntax (e.g., "[2000 TO 2014]") to SolrDateTime object
solrdt_parse_range <- function(x) {
    x <- trimws(x)

    # Determine bracket types
    start_bracket <- substr(x, 1, 1)
    end_bracket <- substr(x, nchar(x), nchar(x))

    if (!start_bracket %in% c("[", "{") || !end_bracket %in% c("]", "}")) {
        stop("Invalid range syntax: ", x, ". Expected format: [start TO end] or {start TO end}")
    }

    # Determine inclusiveness
    start_inclusive <- start_bracket == "["
    end_inclusive <- end_bracket == "]"

    # Extract content between brackets
    content <- substr(x, 2, nchar(x) - 1)

    # Split by " TO "
    parts <- strsplit(content, " TO ", fixed = TRUE)[[1]]

    if (length(parts) != 2) {
        stop("Invalid range syntax: ", x, ". Expected format: [start TO end]")
    }

    start_str <- trimws(parts[1])
    end_str <- trimws(parts[2])

    # Parse start date
    if (start_str == "*") {
        # Unbounded start: use NA
        start_date <- as.POSIXct(NA_real_, tz = "UTC")
    } else {
        start_date <- solrdt_parse_single(start_str)
    }

    # Parse end date
    if (end_str == "*") {
        # Unbounded end: use NA
        end_date <- as.POSIXct(NA_real_, tz = "UTC")
    } else {
        end_date <- solrdt_parse_single(end_str)
    }

    # Create range object
    solrdt_new(
        start = start_date,
        end = end_date,
        start_inclusive = start_inclusive,
        end_inclusive = end_inclusive,
        is_range = TRUE
    )
}

# Internal function to parse a single datetime string
# All results are in UTC
solrdt_parse_single <- function(x) {
    x <- trimws(x)

    # Handle NOW-based Date Math
    if (grepl("^NOW", x, ignore.case = TRUE)) {
        return(solrdt_parse_date_math(Sys.time(), substring(x, 4)))
    }

    # Check if it contains 'Z' (ISO-8601 format with possible Date Math)
    z_pos <- regexpr("Z", x, fixed = TRUE)
    if (z_pos > 0) {
        # Extract base date and math expression
        base_str <- substr(x, 1, z_pos)
        math_str <- substring(x, z_pos + 1)

        # Parse base date
        base_date <- solrdt_parse_iso8601(base_str)

        # Apply Date Math if present
        if (nchar(math_str) > 0) {
            return(solrdt_parse_date_math(base_date, math_str))
        }

        return(base_date)
    }

    # Simplified format (no 'Z'): year, year-month, date, or YYYYMMDD
    # Pattern matching for different formats
    if (grepl("^-?\\d{1,5}$", x)) {
        # Year only: "2025" or "-1500"
        year <- as.integer(x)
        # Handle negative years (BC dates)
        if (year < 0) {
            date_str <- sprintf("%05d-01-01 00:00:00", year)
        } else {
            date_str <- sprintf("%04d-01-01 00:00:00", year)
        }
        as.POSIXct(date_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else if (grepl("^\\d{8}$", x)) {
        # YYYYMMDD format: "20150101"
        as.POSIXct(x, format = "%Y%m%d", tz = "UTC")
    } else if (grepl("^-?\\d{1,5}-\\d{1,2}$", x)) {
        # Year-Month: "2025-01" or "-1500-06"
        as.POSIXct(paste0(x, "-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else if (grepl("^-?\\d{1,5}-\\d{1,2}-\\d{1,2}$", x)) {
        # Date: "2025-01-15"
        as.POSIXct(paste0(x, " 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
        stop(sprintf("Unsupported datetime format: '%s'", x))
    }
}

#' Check if object is SolrDateTime
#'
#' @param x Object to check
#' @return Logical indicating if object is SolrDateTime
#'
#' @export
is.solrdt <- function(x) {
    inherits(x, "SolrDateTime")
}

#' Print method for SolrDateTime
#'
#' Displays SolrDateTime objects with intelligent formatting. By default, shows
#' parsed results only for dynamic expressions (NOW, *, Date Math). Use
#' \code{verbose = TRUE} to always show parsed results.
#'
#' @param x A SolrDateTime object
#' @param verbose Logical. If TRUE, always show parsed results alongside original
#'   input. If FALSE (default), only show parsed results for dynamic expressions
#'   (NOW, *, Date Math operators).
#' @param ... Additional arguments (ignored)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: intelligent display
#' result <- solrdt_parse(c("2025", "NOW", "[* TO 2025]"))
#' print(result)
#' # [1] 2025
#' # [2] NOW → 2025-10-09T22:30:00Z
#' # [3] [* TO 2025] → [NA TO 2025-01-01T00:00:00Z]
#'
#' # Verbose: always show parsed results
#' print(result, verbose = TRUE)
#' # [1] 2025 → 2025-01-01T00:00:00Z
#' # [2] NOW → 2025-10-09T22:30:00Z
#' # [3] [* TO 2025] → [NA TO 2025-01-01T00:00:00Z]
#' }
print.SolrDateTime <- function(x, verbose = FALSE, ...) {
    n <- length(x$start)

    # Header
    cat("<SolrDateTime[", n, "]>\n", sep = "")

    # Show first few elements
    max_show <- min(n, 10)
    for (i in seq_len(max_show)) {
        # Get original input
        original <- if (!is.na(x$original[i])) {
            x$original[i]
        } else if (!x$is_range[i]) {
            # Single datetime - reconstruct
            if (is.na(x$start[i])) {
                "NA"
            } else {
                format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
            }
        } else {
            # Range - reconstruct
            start_bracket <- if (x$start_inclusive[i]) "[" else "{"
            end_bracket <- if (x$end_inclusive[i]) "]" else "}"
            start_str <- if (is.na(x$start[i])) "*" else format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
            end_str <- if (is.na(x$end[i])) "*" else format(x$end[i], "%Y-%m-%dT%H:%M:%SZ")
            paste0(start_bracket, start_str, " TO ", end_str, end_bracket)
        }

        # Determine if we should show parsed result
        # Show parsed result if:
        # 1. verbose = TRUE, OR
        # 2. Contains NOW (case-insensitive), OR
        # 3. Contains * (unbounded), OR
        # 4. Contains Date Math operators (Z followed by +, -, / or NOW followed by +, -, /)
        has_now <- grepl("NOW", original, ignore.case = TRUE)
        has_unbounded <- grepl("\\*", original)
        has_date_math <- grepl("(Z|NOW)[+/\\-]", original, ignore.case = TRUE)

        needs_parsed <- verbose || has_now || has_unbounded || has_date_math

        if (needs_parsed) {
            # Get parsed result (reconstructed from parsed values)
            parsed <- if (!x$is_range[i]) {
                # Single datetime
                if (is.na(x$start[i])) {
                    "NA"
                } else {
                    format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
                }
            } else {
                # Range
                start_bracket <- if (x$start_inclusive[i]) "[" else "{"
                end_bracket <- if (x$end_inclusive[i]) "]" else "}"
                start_str <- if (is.na(x$start[i])) "NA" else format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
                end_str <- if (is.na(x$end[i])) "NA" else format(x$end[i], "%Y-%m-%dT%H:%M:%SZ")
                paste0(start_bracket, start_str, " TO ", end_str, end_bracket)
            }

            # Show original → parsed
            cat("[", i, "] ", original, " \u2192 ", parsed, "\n", sep = "")
        } else {
            # Show only original
            cat("[", i, "] ", original, "\n", sep = "")
        }
    }

    if (n > max_show) {
        cat("... and ", n - max_show, " more element(s)\n", sep = "")
    }

    invisible(x)
}


#' Format method for SolrDateTime
#'
#' Formats SolrDateTime objects back to Solr query syntax. For objects with
#' original input strings, returns those strings. Otherwise, reconstructs
#' the Solr syntax from the parsed components.
#'
#' @param x A SolrDateTime object
#' @param original Logical. If TRUE (default), use original input strings
#'   when available. If FALSE, always reconstruct from parsed values.
#' @param ... Additional arguments (ignored)
#'
#' @export
format.SolrDateTime <- function(x, original = TRUE, ...) {
    n <- length(x$start)
    result <- character(n)

    for (i in seq_len(n)) {
        # Use original string if available and requested
        if (original && !is.na(x$original[i])) {
            result[i] <- x$original[i]
            next
        }

        # Otherwise reconstruct from parsed values
        if (!x$is_range[i]) {
            # Single datetime
            if (is.na(x$start[i])) {
                result[i] <- NA_character_
            } else {
                result[i] <- format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
            }
        } else {
            # Range in Solr syntax
            start_bracket <- if (x$start_inclusive[i]) "[" else "{"
            end_bracket <- if (x$end_inclusive[i]) "]" else "}"

            # Handle unbounded (NA) values
            start_str <- if (is.na(x$start[i])) "*" else format(x$start[i], "%Y-%m-%dT%H:%M:%SZ")
            end_str <- if (is.na(x$end[i])) "*" else format(x$end[i], "%Y-%m-%dT%H:%M:%SZ")

            result[i] <- paste0(
                start_bracket,
                start_str,
                " TO ",
                end_str,
                end_bracket
            )
        }
    }

    result
}

#' Convert SolrDateTime to character
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @export
as.character.SolrDateTime <- function(x, ...) {
    format(x, ...)
}

#' Extract start datetime from SolrDateTime
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @export
solrdt_start <- function(x, ...) {
    UseMethod("solrdt_start")
}

#' @export
solrdt_start.SolrDateTime <- function(x, ...) {
    x$start
}

#' Extract end datetime from SolrDateTime
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @export
solrdt_end <- function(x, ...) {
    UseMethod("solrdt_end")
}

#' @export
solrdt_end.SolrDateTime <- function(x, ...) {
    x$end
}

#' Check if SolrDateTime object represents a range
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @return Logical vector indicating which elements are ranges
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- solrdt_parse(c("[2000 TO 2014]", "2025"))
#' is_solrdt_range(result) # c(TRUE, FALSE)
#' }
is_solrdt_range <- function(x, ...) {
    UseMethod("is_solrdt_range")
}

#' @export
is_solrdt_range.SolrDateTime <- function(x, ...) {
    x$is_range
}

#' Check if SolrDateTime has unbounded start
#'
#' Checks if a SolrDateTime range has an unbounded (infinite) start,
#' represented by `*` in Solr syntax and `NA` in the start field.
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @return Logical vector indicating which elements have unbounded start
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- solrdt_parse(c("[* TO 2025]", "[2000 TO 2014]", "2025"))
#' is_solrdt_start_unbounded(result) # c(TRUE, FALSE, FALSE)
#' }
is_solrdt_start_unbounded <- function(x, ...) {
    x$is_range & is.na(x$start)
}

#' Check if SolrDateTime has unbounded end
#'
#' Checks if a SolrDateTime range has an unbounded (infinite) end,
#' represented by `*` in Solr syntax and `NA` in the end field.
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @return Logical vector indicating which elements have unbounded end
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- solrdt_parse(c("[2000 TO *]", "[2000 TO 2014]", "2025"))
#' is_solrdt_end_unbounded(result) # c(TRUE, FALSE, FALSE)
#' }
is_solrdt_end_unbounded <- function(x, ...) {
    x$is_range & is.na(x$end)
}

#' Convert SolrDateTime to POSIXct
#'
#' Converts a SolrDateTime object to POSIXct by extracting the start datetime.
#' If the object contains ranges, a warning is issued.
#'
#' @param x A SolrDateTime object
#' @param ... Additional arguments (ignored)
#'
#' @return POSIXct vector of start datetimes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- solrdt_parse(c("[2000 TO 2014]", "2025"))
#' as.POSIXct(result) # Returns start datetimes with a warning
#' }
as.POSIXct.SolrDateTime <- function(x, ...) {
    if (any(x$is_range)) {
        warning(
            "Converting SolrDateTime with ranges to POSIXct: using start datetime only. ",
            "Use solrdt_start() or solrdt_end() to extract specific datetimes."
        )
    }
    x$start
}

#' Length method for SolrDateTime
#'
#' @param x A SolrDateTime object
#'
#' @export
length.SolrDateTime <- function(x) {
    length(x$start)
}

#' Subsetting method for SolrDateTime
#'
#' @param x A SolrDateTime object
#' @param i Index
#'
#' @export
`[.SolrDateTime` <- function(x, i) {
    solrdt_new(
        start = x$start[i],
        end = x$end[i],
        start_inclusive = x$start_inclusive[i],
        end_inclusive = x$end_inclusive[i],
        is_range = x$is_range[i],
        original = x$original[i]
    )
}

#' Combine SolrDateTime objects
#'
#' @param ... SolrDateTime objects to combine
#'
#' @export
c.SolrDateTime <- function(...) {
    objects <- list(...)

    # Convert any POSIXct to SolrDateTime
    objects <- lapply(objects, function(x) {
        if (inherits(x, "SolrDateTime")) {
            x
        } else if (inherits(x, "POSIXct")) {
            # Convert POSIXct to SolrDateTime
            # Format as ISO-8601 for original
            original <- format(x, "%Y-%m-%dT%H:%M:%SZ")
            solrdt_new(
                start = x,
                end = x,
                start_inclusive = rep(TRUE, length(x)),
                end_inclusive = rep(TRUE, length(x)),
                is_range = rep(FALSE, length(x)),
                original = original
            )
        } else {
            stop("Cannot combine object of class: ", class(x)[1])
        }
    })

    # Combine all components (all should be UTC)
    starts <- do.call(c, lapply(objects, function(x) x$start))
    ends <- do.call(c, lapply(objects, function(x) x$end))
    start_inclusives <- do.call(c, lapply(objects, function(x) x$start_inclusive))
    end_inclusives <- do.call(c, lapply(objects, function(x) x$end_inclusive))
    is_ranges <- do.call(c, lapply(objects, function(x) x$is_range))
    originals <- do.call(c, lapply(objects, function(x) x$original))

    solrdt_new(
        start = starts,
        end = ends,
        start_inclusive = start_inclusives,
        end_inclusive = end_inclusives,
        is_range = is_ranges,
        original = originals
    )
}

# Parse ISO-8601 format with 'Z' (always UTC)
solrdt_parse_iso8601 <- function(x) {
    # Remove 'Z' suffix
    x <- sub("Z$", "", x)

    # Try parsing with different formats (always UTC)
    formats <- c(
        "%Y-%m-%dT%H:%M:%OS", # With fractional seconds
        "%Y-%m-%dT%H:%M:%S", # Standard
        "%Y-%m-%dT%H:%M", # Without seconds
        "%Y-%m-%dT%H" # Without minutes and seconds
    )

    for (fmt in formats) {
        result <- tryCatch(
            as.POSIXct(x, format = fmt, tz = "UTC"),
            error = function(e) NULL
        )
        if (!is.null(result) && !is.na(result)) {
            return(result)
        }
    }

    stop("Invalid ISO-8601 datetime format: ", x, "Z")
}

# Parse Date Math expression (all operations in UTC)
solrdt_parse_date_math <- function(base_date, math_expr) {
    if (nchar(math_expr) == 0) {
        return(base_date)
    }

    # Ensure base_date is POSIXct in UTC
    if (!inherits(base_date, "POSIXct")) {
        base_date <- as.POSIXct(base_date, tz = "UTC")
    }

    # Split the math expression into operations
    # Pattern: operator (+ or - or /) followed by optional number and unit
    # Note: In character class, - must be escaped or placed at the end
    pattern <- "([+/\\-])(\\d*)([A-Z]+)"
    matches <- gregexpr(pattern, math_expr, ignore.case = TRUE)
    ops <- regmatches(math_expr, matches)[[1]]

    if (length(ops) == 0) {
        stop("Invalid Date Math expression: ", math_expr)
    }

    result <- base_date

    for (op in ops) {
        # Extract operator, value, and unit
        parts <- regmatches(op, regexec(pattern, op, ignore.case = TRUE))[[1]]
        operator <- parts[2]
        value_str <- parts[3]
        unit <- toupper(parts[4])

        if (operator == "/") {
            # Rounding operation
            result <- round_datetime(result, unit)
        } else {
            # Addition or subtraction
            value <- as.integer(value_str)
            if (operator == "-") {
                value <- -value
            }
            result <- add_datetime(result, value, unit)
        }
    }

    result
}

# Round datetime down to the start of the specified unit (in UTC)
round_datetime <- function(dt, unit) {
    dt_lt <- as.POSIXlt(dt, tz = "UTC")

    unit <- toupper(unit)
    if (unit %in% c("YEAR", "YEARS")) {
        dt_lt$mon <- 0
        dt_lt$mday <- 1
        dt_lt$hour <- 0
        dt_lt$min <- 0
        dt_lt$sec <- 0
    } else if (unit %in% c("MONTH", "MONTHS")) {
        dt_lt$mday <- 1
        dt_lt$hour <- 0
        dt_lt$min <- 0
        dt_lt$sec <- 0
    } else if (unit %in% c("DAY", "DAYS", "DATE")) {
        dt_lt$hour <- 0
        dt_lt$min <- 0
        dt_lt$sec <- 0
    } else if (unit %in% c("HOUR", "HOURS")) {
        dt_lt$min <- 0
        dt_lt$sec <- 0
    } else if (unit %in% c("MINUTE", "MINUTES")) {
        dt_lt$sec <- 0
    } else if (unit %in% c("SECOND", "SECONDS")) {
        # Truncate fractional seconds
        dt_lt$sec <- floor(dt_lt$sec)
    } else if (unit %in% c("MILLI", "MILLIS", "MILLISECOND", "MILLISECONDS")) {
        # Round to millisecond (R doesn't support sub-second precision in POSIXlt)
        # This is a no-op in R
    } else {
        stop("Unsupported rounding unit: ", unit)
    }

    as.POSIXct(dt_lt, tz = "UTC")
}

# Add a value to datetime (in UTC)
add_datetime <- function(dt, value, unit) {
    dt_lt <- as.POSIXlt(dt, tz = "UTC")

    unit <- toupper(unit)
    if (unit %in% c("YEAR", "YEARS")) {
        dt_lt$year <- dt_lt$year + value
    } else if (unit %in% c("MONTH", "MONTHS")) {
        dt_lt$mon <- dt_lt$mon + value
    } else if (unit %in% c("DAY", "DAYS", "DATE")) {
        dt_lt$mday <- dt_lt$mday + value
    } else if (unit %in% c("HOUR", "HOURS")) {
        dt_lt$hour <- dt_lt$hour + value
    } else if (unit %in% c("MINUTE", "MINUTES")) {
        dt_lt$min <- dt_lt$min + value
    } else if (unit %in% c("SECOND", "SECONDS")) {
        dt_lt$sec <- dt_lt$sec + value
    } else if (unit %in% c("MILLI", "MILLIS", "MILLISECOND", "MILLISECONDS")) {
        # Add milliseconds (convert to seconds)
        dt_lt$sec <- dt_lt$sec + (value / 1000)
    } else {
        stop("Unsupported time unit: ", unit)
    }

    as.POSIXct(dt_lt, tz = "UTC")
}

# vim: fdm=marker :
