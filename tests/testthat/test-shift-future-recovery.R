# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_epw_future() resumes referenced scenarios after an export interruption", {
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    calls <- stage_morph_test__scenario_inputs()

    calls$future_scenarios <- c("ssp126", "ssp585")
    resume_store <- tempfile("shift-resume-store-")
    export_attempts <- 0L
    original_export <- shift_export__export_outputs
    testthat::local_mocked_bindings(
        shift_export__export_outputs = function(...) {
            export_attempts <<- export_attempts + 1L
            if (export_attempts == 1L) {
                stop("simulated interruption after morphing", call. = FALSE)
            }
            original_export(...)
        },
        .package = "epwshiftr"
    )
    failed_run <- (shift_epw_future(
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
        dir = tempfile("shift-resume-output-"),
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = resume_store
    )@meta$children[[1L]])
    expect_equal(shift_status(failed_run), "failed")
    expect_false(is.na(failed_run@meta$run$completed_at[[1L]]))
    expect_gt(nrow(shift_logs(failed_run)), 0L)
    file_calls_before_resume <- calls$file_calls
    resumed <- shift_resume(failed_run)
    expect_equal(shift_status(resumed), "completed")
    expect_equal(calls$file_calls, file_calls_before_resume)
    expect_equal(nrow(shift_outputs(resumed)), 2L)
})

# vim: fdm=marker :
