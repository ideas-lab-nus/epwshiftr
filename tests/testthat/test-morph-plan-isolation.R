# Create daily inputs with an exactly representable reference offset so two
# calibration plans in one store must produce different, predictable weather.
morph_isolation_test__plan <- function(store, root, name, year, offset = 0) {
    path <- file.path(root, paste0(name, '.nc'))
    write_local_cmip6_netcdf_fixture(path, year)
    if (offset != 0) {
        nc <- RNetCDF::open.nc(path, write = TRUE)
        values <- RNetCDF::var.get.nc(nc, 'tas')
        RNetCDF::var.put.nc(nc, 'tas', values + offset)
        RNetCDF::close.nc(nc)
    }
    start <- sprintf('%d-01-01T00:00:00Z', year)
    end <- sprintf('%d-12-31T23:59:59Z', year)
    docs <- esgf_test__file_docs(
        path,
        datetime_start = start,
        datetime_end = end
    )
    query <- store$add_files(esgf_test__file_result(docs))
    plan <- store$plan_region(
        query,
        lon = 103.98,
        lat = 1.37,
        site_id = 'SIN',
        time = c(start, end),
        variable_id = 'tas'
    )
    result <- store$extract(plan_id = plan$plan_id)
    stopifnot(all(result$status == 'done'))
    unique(plan$plan_id)
}

test_that('observed references and output status remain isolated between morph plans', {
    skip_if_not_installed('duckdb')
    skip_if_not_installed('RNetCDF')
    root <- tempfile('morph-isolation-')
    dir.create(root)
    store <- EsgStore$new(file.path(root, 'store'))
    withr::defer(store$close())
    historical <- morph_isolation_test__plan(store, root, 'historical', 2001L)
    future <- morph_isolation_test__plan(store, root, 'future', 2061L)
    warm <- morph_isolation_test__plan(store, root, 'observed-warm', 2002L, 12)
    cool <- morph_isolation_test__plan(store, root, 'observed-cool', 2002L)
    morpher <- epw_morpher(
        store,
        get_cache_epw(),
        site_id = 'SIN',
        transform = daily_transform('qdm')
    )
    # The second plan must use its own observed data, irrespective of the first
    # plan's insertion order, completed output, or subsequent failure status.
    run <- function(observed, label) {
        morpher$workflow(
            plan_id = future,
            periods = epw_morph_periods(future = 2061L),
            reference_plan_id = historical,
            reference_periods = epw_morph_periods(reference = 2001L),
            observed_plan_id = observed,
            observed_periods = epw_morph_periods(observed = 2002L),
            strict = TRUE,
            dir = file.path('outputs', label)
        )
    }
    first <- run(warm, 'warm')
    first_data <- morpher$process_data(first$plan$morph_id)[[1L]]
    second <- run(cool, 'cool')
    second_data <- morpher$process_data(second$plan$morph_id)[[1L]]
    expect_false(identical(first$plan$morph_id, second$plan$morph_id))
    expect_equal(
        first_data$parts$adjusted_series$value -
            second_data$parts$adjusted_series$value,
        rep(12, 365),
        tolerance = 1e-8
    )
    expect_equal(
        first_data$parts$daily_targets$target_mean -
            second_data$parts$daily_targets$target_mean,
        rep(12, 365),
        tolerance = 1e-8
    )
    # Re-execution must retain the second plan's reference selection as well.
    morpher$run(second$plan$morph_id, overwrite = TRUE)
    repeated <- morpher$process_data(second$plan$morph_id)[[1L]]
    expect_equal(
        repeated$parts$adjusted_series,
        second_data$parts$adjusted_series
    )
    # A reusable manifest can contain multiple cases. Only the requested case
    # may be returned even when every artifact in that manifest is complete.
    existing <- data.table::rbindlist(list(first$results, second$results))
    reused <- priv(morpher)$execute_case(
        morph_id = second$plan$morph_id,
        case = data.table::data.table(case_id = second$results$case_id),
        climate = NULL,
        reference_climate = NULL,
        observed_climate = NULL,
        by = character(),
        reference_by = character(),
        observed_by = character(),
        existing = existing,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_true(reused$reused)
    expect_identical(reused$rows$case_id, second$results$case_id)
    cases <- morpher__read_table(store, 'epw_morph_case')
    select_first <- which(cases$morph_id == first$plan$morph_id)
    data.table::set(cases, i = select_first, j = 'status', value = 'failed')
    morpher__replace_rows(store, 'epw_morph_case', cases, 'case_id')
    morpher$write_epw(
        second$plan$morph_id,
        dir = 'outputs/cool',
        overwrite = TRUE
    )
    expect_identical(morpher$status(second$plan$morph_id)$status, 'epw_written')
})

test_that('case diagnostic replacement cannot delete another plan or case', {
    store <- EsgStore$new(tempfile('diagnostic-isolation-'))
    withr::defer(store$close())
    rows <- data.table::rbindlist(list(
        morpher__diagnostic(
            'runtime',
            'warning',
            'a',
            'first',
            morph_id = 'm1',
            case_id = 'c1'
        ),
        morpher__diagnostic(
            'runtime',
            'warning',
            'b',
            'second',
            morph_id = 'm1',
            case_id = 'c2'
        ),
        morpher__diagnostic(
            'runtime',
            'warning',
            'c',
            'third',
            morph_id = 'm2',
            case_id = 'c1'
        )
    ))
    data.table::set(rows, j = 'diagnostic_id', value = c('d1', 'd2', 'd3'))
    morpher__replace_rows(store, 'epw_morph_diagnostic', rows, 'diagnostic_id')
    morpher__delete_case_diagnostics(store, 'm1', 'c1')
    kept <- morpher__read_table(store, 'epw_morph_diagnostic')
    expect_setequal(kept$diagnostic_id, c('d2', 'd3'))
    morpher__delete_case_diagnostics(store, 'missing', 'c1')
    expect_equal(morpher__read_table(store, 'epw_morph_diagnostic'), kept)
})

test_that('calibrated recipes invalidate earlier shared-store execution plans', {
    # Statistical method equations are unchanged; the complete recipe identity
    # changes because earlier executions could select another plan's inputs.
    keys <- c(
        'quantile_mapping_morphing_daily',
        'hourly_kernel_qdm',
        names(recipe__daily_adjustment_specs())
    )
    for (key in keys) {
        expect_identical(recipe__get(key)@version, 2L)
        expect_error(recipe__get(key, version = 1L), 'persisted version')
    }
})

# vim: fdm=marker :
