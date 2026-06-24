utc <- function(x) as.POSIXct(x, tz = "UTC")

# solrdate__parse() {{{
test_that("solrdate__parse()", {
    expect_equal(solrdate__parse(2025), utc("2025-01-01 00:00:00"))
    expect_equal(solrdate__parse(202502), utc("2025-02-01 00:00:00"))
    expect_equal(solrdate__parse(20250204), utc("2025-02-04 00:00:00"))
    expect_equal(solrdate__parse("20250204"), utc("2025-02-04 00:00:00"))

    expect_equal(solrdate__parse(" 2025-02 "), utc("2025-02-01 00:00:00"))
    expect_equal(solrdate__parse("2025/02/03"), utc("2025-02-03 00:00:00"))
    expect_equal(solrdate__parse("2025.02.03"), utc("2025-02-03 00:00:00"))

    expect_equal(solrdate__parse("2025-02-03"), utc("2025-02-03 00:00:00"))
    expect_equal(solrdate__parse("2025-02-03T01"), utc("2025-02-03 01:00:00"))
    expect_equal(solrdate__parse("2025-02-03T01:02"), utc("2025-02-03 01:02:00"))
    expect_equal(solrdate__parse("2025-02-03T01:02:03Z"), utc("2025-02-03 01:02:03"))

    expect_equal(solrdate__parse("2025-01-01T01:02:03Z"), utc("2025-01-01 01:02:03"))
    expect_equal(solrdate__parse("2025-01-01T01:02:03+1200"), utc("2024-12-31 13:02:03"))
    expect_equal(solrdate__parse("2025-01-01T01:02:03+12:00"), utc("2024-12-31 13:02:03"))
    expect_equal(solrdate__parse("2025-01-01T01:02:03+12"), utc("2024-12-31 13:02:03"))
    expect_equal(solrdate__parse("2025-01-01T01:02:03-05:30"), utc("2025-01-01 06:32:03"))
    expect_equal(solrdate__parse("2025-01-01T01:02:03-05"), utc("2025-01-01 06:02:03"))

    expect_equal(solrdate__parse(as.Date("2025-02-03")), utc("2025-02-03 00:00:00"))
    expect_equal(
        solrdate__parse(as.POSIXct("2025-02-03 01:02:03", tz = "UTC")),
        utc("2025-02-03 01:02:03")
    )

    expect_true(is.na(solrdate__parse(NA_character_)))
    expect_true(is.na(solrdate__parse("")))
    expect_true(is.na(solrdate__parse("not-a-date")))
    expect_true(is.na(solrdate__parse("2025-13-01")))
    expect_true(is.na(solrdate__parse("2025-02-30")))
    expect_true(is.na(solrdate__parse("2025020")))
})
# }}}
# solr_date() {{{
test_that("solr_date()", {
    # range syntax parsing
    left_open <- solr_date("{2000 TO 2010]")
    right_open <- solr_date("[2000 TO 2010}")
    unbounded <- solr_date("[* TO *]")
    math_range <- solr_date("[NOW-1YEAR TO NOW]")

    expect_true(S7::S7_inherits(left_open, SolrDateRange))
    expect_false(S7::prop(left_open, "start_inclusive"))
    expect_true(S7::prop(left_open, "end_inclusive"))
    expect_identical(format(left_open), "{2000-01-01T00:00:00Z TO 2010-01-01T00:00:00Z]")

    expect_true(S7::prop(right_open, "start_inclusive"))
    expect_false(S7::prop(right_open, "end_inclusive"))
    expect_identical(format(right_open), "[2000-01-01T00:00:00Z TO 2010-01-01T00:00:00Z}")

    expect_true(S7::S7_inherits(unbounded, SolrDateRange))
    expect_true(S7::S7_inherits(S7::prop(unbounded, "start"), SolrDateUnbounded))
    expect_true(S7::S7_inherits(S7::prop(unbounded, "end"), SolrDateUnbounded))
    expect_identical(format(unbounded), "[* TO *]")
    expect_identical(format(unbounded, as = "num"), "[* TO *]")

    expect_true(S7::S7_inherits(math_range, SolrDateRange))
    expect_true(S7::S7_inherits(S7::prop(math_range, "start"), SolrDateMath))
    expect_true(S7::S7_inherits(S7::prop(math_range, "end"), SolrDateInstant))
    expect_identical(format(math_range), "[NOW-1YEAR TO NOW]")

    expect_error(solr_date("[2000TO2010]"), "invalid datetime")
    expect_error(solr_date("[2000 TO 2010"), "invalid range syntax")
    expect_error(solr_date("(2000 TO 2010]"), "invalid range syntax")
    expect_error(solr_date("[2000 TO 2010)"), "invalid range syntax")

    # Date Math parsing
    complex_now <- solr_date("NOW/DAY-1YEAR+6MONTHS")
    fixed_math <- solr_date("2025-01-01T00:00:00Z+1MONTH")

    expect_true(S7::S7_inherits(complex_now, SolrDateMath))
    expect_true(is.na(S7::prop(complex_now, "value")))
    expect_identical(format(complex_now), "NOW/DAY-1YEAR+6MONTHS")

    expect_true(S7::S7_inherits(fixed_math, SolrDateMath))
    expect_equal(S7::prop(fixed_math, "value"), utc("2025-01-01 00:00:00"))
    expect_identical(format(fixed_math), "2025-01-01T00:00:00Z+1MONTH")

    expect_error(solr_date("NOW+1FORTNIGHT"), "Date Math")
    expect_error(solr_date("NOW++1DAY"), "Date Math")
    expect_error(solr_date("NOW/"), "Date Math")
    expect_error(solr_date("2025-01-01T00:00:00Z+"), "Date Math")

    # formatting and conversion
    instant <- solr_date("2025")
    range <- solr_date("[2000 TO 2010]")
    timed <- solr_date(as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))

    expect_identical(format(instant, as = "iso"), "2025-01-01T00:00:00Z")
    expect_identical(format(instant, as = "num"), "20250101")
    expect_identical(format(range, as = "iso"), "[2000-01-01T00:00:00Z TO 2010-01-01T00:00:00Z]")
    expect_identical(format(range, as = "num"), "[20000101 TO 20100101]")
    expect_warning(num <- format(timed, as = "num"), "Loss of time information")
    expect_identical(num, "20250115")
    expect_identical(as.character(instant), "2025-01-01T00:00:00Z")
    expect_identical(as.character(range), "[2000-01-01T00:00:00Z TO 2010-01-01T00:00:00Z]")
    expect_identical(as.POSIXct(instant), utc("2025-01-01 00:00:00"))
    expect_warning(ased <- as.POSIXct(range), "start")
    expect_identical(ased, utc("2000-01-01 00:00:00"))

    # robustness and wrapper behavior
    date_input <- solr_date(as.Date("2025-01-15"))
    posix_input <- solr_date(as.POSIXct("2025-01-15 12:30:45", tz = "UTC"))
    x <- solr_date("2025")

    expect_error(solr_date(c("2025", "2026")), "length 1")
    expect_error(solr_date(c(2025, 2026)), "length 1")
    expect_error(solr_date(list("2025")), "atomic scalar")
    expect_error(solr_date(TRUE))

    expect_true(S7::S7_inherits(date_input, SolrDateInstant))
    expect_true(S7::S7_inherits(posix_input, SolrDateInstant))
    expect_identical(format(date_input), "2025-01-15T00:00:00Z")
    expect_identical(format(posix_input), "2025-01-15T12:30:45Z")
    expect_identical(solr_date(x), x)
    expect_error(
        solr_date(as.POSIXct("2025-01-15 12:30:45", tz = "America/New_York")),
        "UTC"
    )
})
# }}}
# solrdate__eval() {{{
test_that("solrdate__eval() evaluates Solr Date Math for bridge rendering", {
    now <- utc("2025-06-13 12:34:56")

    expect_identical(
        format(solrdate__eval(solr_date("NOW-1YEAR"), now = now)),
        "2024-06-13T12:34:56Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("NOW"), now = now)),
        "2025-06-13T12:34:56Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("2025-06-13T00:00:00Z-1YEAR"))),
        "2024-06-13T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("2023-06-13T00:00:00Z+1YEAR"))),
        "2024-06-13T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("NOW/DAY-1YEAR+6MONTHS"), now = now)),
        "2024-12-13T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("2024-02-29T00:00:00Z+1YEAR"))),
        "2025-02-28T00:00:00Z"
    )

    expect_identical(
        format(solr_date("-0009-01-01T00:00:00Z")),
        "-0009-01-01T00:00:00Z"
    )
    expect_identical(
        format(solr_date("0000-01-01T00:00:00Z")),
        "0000-01-01T00:00:00Z"
    )
    expect_identical(
        format(solr_date("+10000-01-01T00:00:00Z")),
        "+10000-01-01T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("-0009-01-01T00:00:00Z+1YEAR"))),
        "-0008-01-01T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("+10000-01-01T00:00:00Z-1YEAR"))),
        "9999-01-01T00:00:00Z"
    )
    expect_identical(
        format(solrdate__eval(solr_date("+10000-06-13T12:34:56Z/YEAR"))),
        "+10000-01-01T00:00:00Z"
    )
})
# }}}
# vim: fdm=marker :
