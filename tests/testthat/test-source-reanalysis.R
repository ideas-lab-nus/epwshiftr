test_that("ERA source specifications are provider-neutral and credential-free", {
    source <- shift_era5(
        years = 1995:2014,
        product = "single_levels",
        timeout = 60,
        poll_interval = 0.5
    )

    expect_true(S7::S7_inherits(source, ShiftReanalysisSpec))
    expect_identical(source@dataset, "era5")
    expect_identical(source@years, 1995:2014)
    expect_identical(source@options$timeout, 60)
    expect_false(any(grepl("key|token|secret", names(source@options))))

    restored <- reanalysis__from_spec(reanalysis__spec_value(source))
    expect_identical(restored@dataset, source@dataset)
    expect_identical(restored@years, source@years)
    expect_identical(restored@options, source@options)
})

test_that("reanalysis checks keep local and remote authentication explicit", {
    source <- shift_era5(2000)
    calls <- 0L
    testthat::local_mocked_bindings(
        cds__config = function(...) {
            list(url = "https://example.test/api", key = "secret")
        },
        cds__check_authentication = function(config, timeout = 120) {
            calls <<- calls + 1L
            TRUE
        },
        .package = "epwshiftr"
    )

    expect_equal(nrow(shift_check(source)), 0L)
    expect_identical(calls, 0L)
    expect_equal(nrow(shift_check(source, network = TRUE)), 0L)
    expect_identical(calls, 1L)
})

test_that("reanalysis checks diagnose missing and invalid credentials", {
    source <- shift_era5(2000)
    testthat::local_mocked_bindings(
        cds__config = function(...) {
            cli::cli_abort(
                "A Copernicus Data Store API key is required.",
                class = "epwshiftr_cds_auth_error"
            )
        },
        .package = "epwshiftr"
    )
    missing <- shift_check(source)

    expect_identical(missing$code, "cds_auth_missing")
    expect_error(shift_check(source, strict = TRUE))

    testthat::local_mocked_bindings(
        cds__config = function(...) {
            list(url = "https://example.test/api", key = "secret")
        },
        cds__check_authentication = function(...) {
            cli::cli_abort(
                "CDS returned HTTP 401.",
                class = c(
                    "epwshiftr_cds_auth_error",
                    "epwshiftr_cds_request_error"
                )
            )
        },
        .package = "epwshiftr"
    )
    invalid <- shift_check(source, network = TRUE)

    expect_identical(invalid$code, "cds_auth_invalid")
    expect_false(grepl("secret", invalid$message, fixed = TRUE))
})

test_that("ERA6 is explicit and never falls back to ERA5", {
    registry <- reanalysis__registry()

    expect_false(registry$era6$available)
    expect_null(registry$era6$replacement)
    expect_error(
        shift_era6(2000:2001),
        class = "epwshiftr_reanalysis_unavailable"
    )
})

test_that("ERA5 access and requests reflect variable and time-zone needs", {
    site <- shift_site(id = "SFO", lon = -122.375, lat = 37.619,
        epw = get_cache_epw())
    source <- shift_era5(1995:2014)

    expect_identical(
        era5__resolve_access(source, c("tas", "hurs")),
        "arco"
    )
    expect_identical(era5__resolve_access(source, "clt"), "cds")

    arco <- era5__request(source, "tas", site, "arco")
    expect_identical(
        arco$date,
        "1994-12-31/2015-01-01"
    )
    expect_identical(arco$location$longitude, -122.375)

    wrapped_site <- shift_site(
        id = "SFO-360",
        lon = 237.625,
        lat = 37.619,
        epw = get_cache_epw()
    )
    wrapped <- era5__request(source, "tas", wrapped_site, "arco")
    expect_identical(wrapped$location$longitude, -122.375)

    cds <- era5__request(source, "tas", site, "cds")
    expect_identical(cds$variable, "2m_temperature")
    expect_identical(attr(cds, "requested_years"), 1994:2015)
    expect_true(all(c("area", "year", "month", "day", "time") %in%
        names(cds)))
})

test_that("ERA5 rejects unknown provider options", {
    expect_error(
        shift_era5(2000, unsupported = TRUE),
        "Unknown ERA5 provider option"
    )
})

test_that("ERA5 variables are constrained by product semantics", {
    expect_error(
        era5__source_variables("snd"),
        "single_levels.*does not provide or derive.*snd"
    )
    expect_identical(era5__source_variables("snd", "land"), "snd")
    expect_error(
        era5__source_variables("psl", "land"),
        "land.*does not provide or derive.*psl"
    )

    land <- shift_era5(2000, product = "land")
    expect_identical(era5__resolve_access(land, "snd"), "arco")
    expect_identical(era5__resolve_access(land, "rlds"), "cds")
})
