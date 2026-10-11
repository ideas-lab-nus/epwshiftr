# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_morph() uses complete extraction plans by default", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    good <- esgf_test__file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc,
        variable_id = "tas"
    )
    bad <- esgf_test__file_docs(
        "hurs_missing_opendap.nc",
        opendap_url = "https://example.org/hurs_missing_opendap.nc",
        download_url = "https://example.org/hurs_missing_opendap.nc",
        variable_id = "hurs",
        include_opendap = FALSE
    )
    docs <- data.table::rbindlist(list(good, bad), fill = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "day"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    climate <- req |>
        shift_collect(
            store = tempfile("shift-store-"),
            label = "complete-subset"
        ) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            fallback = "error"
        )

    coverage <- shift_coverage(climate)
    expect_true(any(coverage$complete))
    expect_true(any(!coverage$complete))

    transform <- daily_transform("epwshiftr")
    morphed <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE
    )
    blocked <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE,
        complete_only = FALSE,
        overwrite = TRUE
    )

    expect_equal(
        shift_ids(morphed)$plan_id,
        coverage$plan_id[coverage$complete]
    )
    expect_true(any(
        shift_diagnostics(morphed)$code %in% "ignored_incomplete_extraction"
    ))
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(blocked), "blocked")
    expect_equal(shift_status(shift_complete(morphed)), "partial")
})

# vim: fdm=marker :
