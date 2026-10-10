# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_epw_future() preserves strict and partial missing-scenario outcomes", {
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    calls <- stage_morph_test__scenario_inputs()

    calls$future_scenarios <- "ssp585"
    missing_store <- tempfile("shift-default-missing-store-")
    missing_run <- (shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = tempfile("shift-default-missing-output-"),
        store = missing_store,
        ui = shift_ui("none")
    )@meta$children[[1L]])
    expect_identical(shift_status(missing_run), "failed")
    missing_diagnostics <- shift_diagnostics(missing_run)
    expect_true("shift_resolver_exhausted" %in% missing_diagnostics$code)
    expect_true(any(grepl("ssp126", missing_diagnostics$message, fixed = TRUE)))

    partial <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            frequency = "mon",
            table = "Amon",
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995L),
        dir = tempfile("shift-partial-output-"),
        control = shift_control(
            strict = TRUE,
            allow_partial = TRUE,
            overwrite = TRUE
        ),
        store = tempfile("shift-partial-store-")
    )@meta$children[[1L]]
    expect_equal(shift_status(partial), "partial")
    expect_equal(nrow(shift_outputs(partial)), 1L)
    expect_equal(nrow(shift_missing(partial)), 1L)
    expect_equal(shift_missing(partial)$experiment_id, "ssp126")
})

# vim: fdm=marker :
