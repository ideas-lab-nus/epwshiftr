# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_epw_future() completes baseline scenarios without historical discovery", {
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    calls <- stage_morph_test__scenario_inputs()

    baseline_reference_run <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-run-baseline-reference-output-"),
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = tempfile("shift-run-baseline-reference-store-")
    )@meta$children[[1L]]
    expect_equal(shift_status(baseline_reference_run), "completed")
    expect_equal(nrow(shift_outputs(baseline_reference_run)), 2L)
    expect_equal(calls$historical_file_calls, 0L)
})

# vim: fdm=marker :
