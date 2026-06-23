test_that("solrdt_parse() - simplified formats", {
    # Year only
    result <- solrdt_parse("2025")
    expect_s3_class(result, "SolrDateTime")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_false(is_solrdt_range(result))

    # Negative year (BC date)
    result <- solrdt_parse("-1500")
    expect_equal(solrdt_start(result), as.POSIXct("-1500-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

    # Year-Month
    result <- solrdt_parse("2025-01")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

    # Date
    result <- solrdt_parse("2025-01-15")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))

    # YYYYMMDD format
    result <- solrdt_parse("20250115")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))
})

test_that("solrdt_parse() - ISO-8601 formats", {
    # Full datetime
    result <- solrdt_parse("2025-01-15T12:30:45Z")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))

    # With milliseconds
    result <- solrdt_parse("2025-01-15T12:30:45.123Z")
    expect_equal(as.numeric(solrdt_start(result)), as.numeric(as.POSIXct("2025-01-15 12:30:45.123", tz = "UTC")))

    # Lenient format (missing leading zeros)
    result <- solrdt_parse("2025-1-15T12:30:45Z")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))

    # Without seconds
    result <- solrdt_parse("2025-01-15T12:30Z")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 12:30:00", tz = "UTC"))
})

test_that("solrdt_parse() - Date Math expressions", {
    # NOW
    result <- solrdt_parse("NOW")
    expect_s3_class(solrdt_start(result), "POSIXct")
    expect_equal(attr(solrdt_start(result), "tzone"), "UTC")

    # NOW with rounding
    result <- solrdt_parse("NOW/DAY")
    now_day_start <- as.POSIXct(format(Sys.time(), "%Y-%m-%d 00:00:00"), tz = "UTC")
    expect_equal(solrdt_start(result), now_day_start)

    # Base date with addition
    result <- solrdt_parse("2025-01-01T00:00:00Z+5DAYS")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-06 00:00:00", tz = "UTC"))

    # Base date with subtraction
    result <- solrdt_parse("2025-01-10T00:00:00Z-3DAYS")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-07 00:00:00", tz = "UTC"))

    # Multiple operations
    result <- solrdt_parse("2025-01-01T00:00:00Z+1MONTH+5DAYS")
    expect_equal(solrdt_start(result), as.POSIXct("2025-02-06 00:00:00", tz = "UTC"))

    # Rounding with addition
    result <- solrdt_parse("2025-01-15T12:30:45Z/DAY+1HOUR")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 01:00:00", tz = "UTC"))
})

test_that("solrdt_parse() - range syntax with inclusive boundaries", {
    # Basic range
    result <- solrdt_parse("[2000 TO 2014]")
    expect_true(is_solrdt_range(result))
    expect_equal(solrdt_start(result), as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2014-01-01 00:00:00", tz = "UTC"))
    expect_true(result$start_inclusive)
    expect_true(result$end_inclusive)

    # Range with month precision
    result <- solrdt_parse("[2000-01 TO 2014-12]")
    expect_equal(solrdt_start(result), as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2014-12-01 00:00:00", tz = "UTC"))

    # Range with full datetime
    result <- solrdt_parse("[2014-05-21T12:00:00Z TO 2015-01-01T00:00:00Z]")
    expect_equal(solrdt_start(result), as.POSIXct("2014-05-21 12:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2015-01-01 00:00:00", tz = "UTC"))
})

test_that("solrdt_parse() - range syntax with exclusive boundaries", {
    # Exclusive start
    result <- solrdt_parse("{2000 TO 2014]")
    expect_false(result$start_inclusive)
    expect_true(result$end_inclusive)

    # Exclusive end
    result <- solrdt_parse("[2000 TO 2014}")
    expect_true(result$start_inclusive)
    expect_false(result$end_inclusive)

    # Both exclusive
    result <- solrdt_parse("{2000 TO 2014}")
    expect_false(result$start_inclusive)
    expect_false(result$end_inclusive)
})

test_that("solrdt_parse() - unbounded ranges", {
    # Unbounded start
    result <- solrdt_parse("[* TO 2025]")
    expect_true(is_solrdt_range(result))
    expect_true(is_solrdt_start_unbounded(result))
    expect_false(is_solrdt_end_unbounded(result))
    expect_true(is.na(solrdt_start(result)))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

    # Unbounded end
    result <- solrdt_parse("[2000 TO *]")
    expect_false(is_solrdt_start_unbounded(result))
    expect_true(is_solrdt_end_unbounded(result))
    expect_equal(solrdt_start(result), as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
    expect_true(is.na(solrdt_end(result)))

    # Fully unbounded
    result <- solrdt_parse("[* TO *]")
    expect_true(is_solrdt_start_unbounded(result))
    expect_true(is_solrdt_end_unbounded(result))
    expect_true(is.na(solrdt_start(result)))
    expect_true(is.na(solrdt_end(result)))
})

test_that("solrdt_parse() - POSIXt input", {
    # POSIXct input
    input_time <- as.POSIXct("2025-01-15 12:30:45", tz = "UTC")
    result <- solrdt_parse(input_time)
    expect_s3_class(result, "SolrDateTime")
    expect_equal(solrdt_start(result), input_time)
    expect_equal(solrdt_end(result), input_time)
    expect_false(is_solrdt_range(result))

    # POSIXlt input
    input_time_lt <- as.POSIXlt("2025-01-15 12:30:45", tz = "UTC")
    result <- solrdt_parse(input_time_lt)
    expect_equal(solrdt_start(result), as.POSIXct(input_time_lt, tz = "UTC"))

    # Vector POSIXct input
    input_times <- as.POSIXct(c("2025-01-15 12:30:45", "2025-02-20 08:15:30"), tz = "UTC")
    result <- solrdt_parse(input_times)
    expect_length(result, 2)
    expect_equal(solrdt_start(result), input_times)
})

test_that("solrdt_parse() - Date input", {
    # Single Date
    input_date <- as.Date("2025-01-15")
    result <- solrdt_parse(input_date)
    expect_s3_class(result, "SolrDateTime")
    expect_equal(solrdt_start(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))
    expect_equal(solrdt_end(result), as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))

    # Vector Date
    input_dates <- as.Date(c("2025-01-15", "2025-02-20"))
    result <- solrdt_parse(input_dates)
    expect_length(result, 2)
    expect_equal(solrdt_start(result)[1], as.POSIXct("2025-01-15 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(result)[2], as.POSIXct("2025-02-20 00:00:00", tz = "UTC"))
})

test_that("solrdt_parse() - vector input", {
    # Mixed vector input
    input <- c("2025", "2025-06", "[2000 TO 2014]", "2025-01-15T12:30:45Z")
    result <- solrdt_parse(input)
    expect_length(result, 4)
    expect_equal(solrdt_start(result)[1], as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(result)[2], as.POSIXct("2025-06-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(result)[3], as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(result)[4], as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))

    # Check is_range for vector
    expect_equal(is_solrdt_range(result), c(FALSE, FALSE, TRUE, FALSE))
})

test_that("format.SolrDateTime() and as.character.SolrDateTime()", {
    # Single datetime
    result <- solrdt_parse("2025-01-15")
    expect_equal(format(result), "2025-01-15")
    expect_equal(as.character(result), "2025-01-15")

    # Range
    result <- solrdt_parse("[2000 TO 2014]")
    expect_equal(format(result), "[2000 TO 2014]")

    # Unbounded range
    result <- solrdt_parse("[* TO 2025]")
    expect_equal(format(result), "[* TO 2025]")

    # Exclusive boundaries
    result <- solrdt_parse("{2000 TO 2014}")
    expect_equal(format(result), "{2000 TO 2014}")

    # Format with original = FALSE
    result <- solrdt_parse("2025")
    expect_equal(format(result, original = FALSE), "2025-01-01T00:00:00Z")
})

test_that("as.POSIXct.SolrDateTime()", {
    # Single datetime
    result <- solrdt_parse("2025-01-15T12:30:45Z")
    posix_result <- as.POSIXct(result)
    expect_s3_class(posix_result, "POSIXct")
    expect_equal(posix_result, as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))

    # Range - should warn
    result <- solrdt_parse("[2000 TO 2014]")
    expect_warning(posix_result <- as.POSIXct(result))
    expect_equal(posix_result, as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
})

test_that("length.SolrDateTime()", {
    # Single element
    result <- solrdt_parse("2025")
    expect_equal(length(result), 1)

    # Vector
    result <- solrdt_parse(c("2025", "2026", "2027"))
    expect_equal(length(result), 3)
})

test_that("[.SolrDateTime() - subsetting", {
    # Create vector
    result <- solrdt_parse(c("2025", "2026", "[2000 TO 2014]", "2027"))

    # Single element subsetting
    subset <- result[1]
    expect_s3_class(subset, "SolrDateTime")
    expect_length(subset, 1)
    expect_equal(solrdt_start(subset), as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

    # Multiple element subsetting
    subset <- result[c(1, 3)]
    expect_length(subset, 2)
    expect_equal(solrdt_start(subset)[1], as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(subset)[2], as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))

    # Logical subsetting
    subset <- result[is_solrdt_range(result)]
    expect_length(subset, 1)
    expect_true(is_solrdt_range(subset))
})

test_that("c.SolrDateTime() - combining", {
    # Combine two SolrDateTime objects
    result1 <- solrdt_parse("2025")
    result2 <- solrdt_parse("2026")
    combined <- c(result1, result2)
    expect_s3_class(combined, "SolrDateTime")
    expect_length(combined, 2)
    expect_equal(solrdt_start(combined)[1], as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))
    expect_equal(solrdt_start(combined)[2], as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))

    # Combine multiple objects
    result3 <- solrdt_parse("[2000 TO 2014]")
    combined <- c(result1, result2, result3)
    expect_length(combined, 3)
    expect_equal(is_solrdt_range(combined), c(FALSE, FALSE, TRUE))

    # Combine with POSIXct
    posix_time <- as.POSIXct("2027-01-01 00:00:00", tz = "UTC")
    combined <- c(result1, posix_time)
    expect_length(combined, 2)
    expect_equal(solrdt_start(combined)[2], posix_time)
})

test_that("Error handling", {
    # NA values not allowed
    expect_error(solrdt_parse(NA_character_))
    expect_error(solrdt_parse(c("2025", NA_character_)))

    # Invalid range syntax
    expect_error(solrdt_parse("[2000 2014]"))  # Missing TO
    expect_error(solrdt_parse("2000 TO 2014"))  # Missing brackets

    # Unsupported format
    expect_error(solrdt_parse("invalid-date"))

    # Invalid Date Math
    expect_error(solrdt_parse("NOW+INVALID"))
})
