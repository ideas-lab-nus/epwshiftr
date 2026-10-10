# Repeated labels must retain row order, scientific conversion and caller ownership.
test_that("hourly canonical units preserve mixed aliases and source metadata", {
    input <- data.table::data.table(
        variable_id = c("tas", "ps", "huss", "tas", "ps", "huss"),
        value = c(20, 1000, .008, 21, 990, .009),
        units = c("C", "hPa", "kg kg-1", "celsius", "hectopascal", "kg/kg"),
        time = as.POSIXct("2061-01-01", tz = "UTC") + seq_len(6L) * 3600,
        source_id = c("a", "b", "c", "a", "b", "c")
    )
    before <- data.table::copy(input)
    expected <- data.table::copy(input)
    data.table::set(
        expected,
        j = "value",
        value = c(293.15, 100000, .008, 294.15, 99000, .009)
    )
    data.table::set(expected, j = "units", value = rep(c("K", "Pa", "1"), 2L))
    actual <- hourly_kqdm_input__canonical_units(
        input,
        "model",
        c("tas", "ps", "huss")
    )
    expect_identical(actual, expected)
    expect_identical(input, before)
})

# Nonstandard column representations retain the original scalar coercion path.
test_that("hourly unit normalization keeps missing and structured input boundaries", {
    input <- data.table::data.table(
        variable_id = c("tas", "tas"),
        value = c(NA_real_, 20),
        units = c("C", "celsius")
    )
    for (units in list(
        c("C", "celsius"),
        list("C", "celsius"),
        factor(c("C", "celsius"))
    )) {
        candidate <- data.table::copy(input)
        data.table::set(candidate, j = "units", value = list(units))
        actual <- hourly_kqdm_input__canonical_units(
            candidate,
            "observed",
            "tas"
        )
        expect_identical(actual$value, c(NA_real_, 293.15))
    }
    for (units in list(
        c(NA_character_, NA_character_),
        c("K", NA_character_),
        c("", ""),
        list(NULL, "K"),
        list(character(), "K"),
        c("K", "C")
    )) {
        candidate <- data.table::copy(input)
        data.table::set(candidate, j = "units", value = list(units))
        expect_error(
            hourly_kqdm_input__canonical_units(candidate, "observed", "tas"),
            "one supported unit"
        )
    }
    data.table::set(input, j = "units", value = c("unknown", "unknown"))
    expect_error(
        hourly_kqdm_input__canonical_units(input, "observed", "tas"),
        "Unsupported unit conversion"
    )
    empty <- input[0L]
    expect_identical(
        hourly_kqdm_input__canonical_units(empty, "observed", character()),
        empty
    )
    expect_error(
        hourly_kqdm_input__canonical_units(empty, "observed", "tas"),
        "input contract"
    )
})

# Distinct-label expansion must apply each conversion to its original row.
test_that("humidity unit preparation preserves per-row conversion and scalar fallbacks", {
    expect_identical(
        morpher__humidity_input_si(
            c(280, 20, 281, 21),
            c("K", "C", "kelvin", "celsius"),
            "tas"
        ),
        c(280, 293.15, 281, 294.15)
    )
    expect_identical(
        morpher__humidity_input_si(
            c(100000, 1000, 99000, 990),
            c("Pa", "hPa", "pascal", "hectopascal"),
            "ps"
        ),
        c(100000, 100000, 99000, 99000)
    )
    expect_identical(
        morpher__humidity_input_si(
            c(.008, NA_real_, .009),
            c("1", "kg kg-1", "kg/kg"),
            "huss"
        ),
        c(.008, NA_real_, .009)
    )
    for (units in list(
        c(first = "C", second = "celsius"),
        list("C", "celsius"),
        factor(c("C", "celsius")),
        structure(c("C", "celsius"), label = "source")
    )) {
        expect_identical(
            morpher__humidity_input_si(c(20, NA_real_), units, "tas"),
            c(293.15, NA_real_)
        )
    }
    expect_identical(
        morpher__humidity_input_si(numeric(), character(), "tas"),
        numeric()
    )
    for (units in list(
        c("K", NA_character_),
        c("", ""),
        list(NULL, "K"),
        list(character(), "K"),
        c("unsupported", "K")
    )) {
        expect_error(
            morpher__humidity_input_si(c(20, 21), units, "tas"),
            class = "epwshiftr_hurs_derivation_error"
        )
    }
})

# vim: fdm=marker :
