# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift_epw_future() completes an explicit historical reference run", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    calls <- stage_morph_test__scenario_inputs()

    store_path <- tempfile("shift-run-store-")
    output_dir <- tempfile("shift-run-output-")
    run <- shift_epw_future(
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
        dir = output_dir,
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = store_path
    )@meta$children[[1L]]
    store_path <- run@store_path

    expect_equal(shift_status(run), "completed")
    expect_equal(nrow(shift_outputs(run)), 2L)
    expect_equal(nrow(shift_missing(run)), 0L)
    expect_true(all(file.exists(shift_outputs(run)$export_path)))
    expect_true(all(vapply(
        shift_outputs(run)$export_path,
        function(path) {
            inherits(epw_file_read(path), "EpwFile")
        },
        logical(1L)
    )))
    expect_equal(calls$historical_file_calls, 1L)
    run_tables <- c("shift_run", "shift_run_case", "shift_run_event")
    expect_true(all(vapply(
        run_tables,
        function(table) {
            nrow(morpher__private_store(shift_store(run))$read_table(table)) >=
                1L
        },
        logical(1L)
    )))
    expect_equal(nrow(shift_runs(store_path)), 1L)
    expect_equal(
        shift_status(shift_run_get(shift_ids(run)$run_id, store_path)),
        "completed"
    )
    expect_equal(shift_ids(shift_resume(run))$run_id, shift_ids(run)$run_id)
    delivery_files <- list.files(output_dir, recursive = TRUE, all.files = TRUE)
    expect_false(any(grepl("\\.(duckdb|parquet|json)$", delivery_files)))
})

# vim: fdm=marker :
