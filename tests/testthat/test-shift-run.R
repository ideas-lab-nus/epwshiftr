# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("empty collection is partial instead of waiting for another stage", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    calls$dataset_all <- logical()
    calls$dataset_limit <- list()
    empty <- esgf_test__file_docs("empty.nc")[0, , drop = FALSE]
    shift_test__mock_collect(empty, calls)

    files <- shift_collect(
        shift_request(project = "CMIP6", frequency = "mon"),
        store = tempfile("shift-empty-store-"),
        ui = shift_ui("none")
    )

    expect_identical(shift_status(files), "partial")
    expect_identical(shift_status(shift_run_get(files)), "partial")
})

test_that("shift_* stages run through extract, relaxed morph, and EPW output", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- file.path(tempdir(), local_cmip6_nc_file(2060L, variable_id = "tas"))
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(
        esgf_test__file_docs(basename(nc), opendap_url = nc, download_url = nc),
        calls
    )

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    store_path <- tempfile("shift-store-")

    files <- shift_collect(req, store = store_path, label = "shift-full")
    climate <- shift_extract(
        files,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    climate_resumed <- shift_extract(
        files,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    climate_after_download <- shift_extract(
        dl,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    transform <- daily_transform("epwshiftr")
    morphed <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE
    )
    epws <- shift_epw(morphed, dir = "shift-epw")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_true(S7::S7_inherits(dl, ShiftDownload))
    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_true(S7::S7_inherits(climate_resumed, ShiftClimate))
    expect_true(S7::S7_inherits(climate_after_download, ShiftClimate))
    expect_true(S7::S7_inherits(climate@meta$files, ShiftFiles))
    expect_null(climate@meta$download)
    expect_true(S7::S7_inherits(
        climate_after_download@meta$download,
        ShiftDownload
    ))
    expect_true(S7::S7_inherits(morphed, ShiftMorphed))
    expect_true(S7::S7_inherits(epws, ShiftOutputs))
    expect_identical(shift_ids(climate)$run_id, shift_ids(files)$run_id)
    expect_false(identical(
        shift_ids(climate_resumed)$run_id,
        shift_ids(files)$run_id
    ))
    expect_identical(shift_ids(morphed)$run_id, shift_ids(climate)$run_id)
    expect_identical(shift_ids(epws)$run_id, shift_ids(morphed)$run_id)
    expect_equal(shift_status(shift_run_get(epws)), "waiting")
    expect_equal(shift_status(climate), "extracted")
    expect_equal(shift_status(climate_resumed), "extracted")
    expect_equal(shift_status(climate_after_download), "extracted")
    expect_true(length(shift_ids(climate)$plan_id) >= 1L)
    expect_true(length(shift_ids(climate_after_download)$plan_id) >= 1L)
    expect_true(nrow(shift_coverage(climate)) >= 1L)
    preview <- shift_data(
        climate,
        n = 2L,
        columns = c(
            "site_id",
            "variable_id",
            "time",
            "lon",
            "lat",
            "value",
            "units"
        )
    )
    expect_equal(nrow(preview), 2L)
    expect_named(
        preview,
        c("site_id", "variable_id", "time", "lon", "lat", "value", "units")
    )
    expect_equal(unique(preview$site_id), "SIN")
    expect_equal(unique(preview$variable_id), "tas")
    expect_equal(nrow(shift_data(climate, n = 0L)), 0L)
    expect_equal(nrow(shift_data(climate, variables = "missing")), 0L)
    expect_error(shift_data(climate, case_id = "missing"), "case_id")
    expect_error(shift_data(files), "ShiftClimate")
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(epws), "written")
    morphed_preview <- shift_data(
        morphed,
        n = 2L,
        columns = c(
            "case_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "year",
            "month",
            "day",
            "hour",
            "dry_bulb_temperature",
            "relative_humidity"
        )
    )
    expect_equal(nrow(morphed_preview), 2L)
    expect_true(all(
        c("case_id", "period", "dry_bulb_temperature") %in%
            names(morphed_preview)
    ))
    expect_equal(unique(morphed_preview$period), "2060s")
    expect_equal(nrow(shift_data(morphed, case_id = "missing")), 0L)
    expect_error(shift_data(morphed, variables = "tas"), "variables")
    expect_error(
        shift_data(morphed, n = 1L, columns = "missing_column"),
        "Unknown"
    )

    epw_preview <- shift_data(
        epws,
        n = 2L,
        columns = c(
            "output_id",
            "case_id",
            "path",
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "year",
            "month",
            "day",
            "hour",
            "dry_bulb_temperature"
        )
    )
    expect_equal(nrow(epw_preview), 2L)
    expect_true(all(
        c("output_id", "case_id", "path", "dry_bulb_temperature") %in%
            names(epw_preview)
    ))
    expect_equal(unique(epw_preview$period), "2060s")
    expect_equal(nrow(shift_data(epws, case_id = "missing")), 0L)
    expect_error(shift_data(epws, variables = "tas"), "variables")

    morph_artifacts <- shift_artifacts(morphed)
    output_artifacts <- shift_artifacts(epws)
    expect_true(nrow(morph_artifacts) >= 1L)
    expect_true(nrow(output_artifacts) >= 1L)
    expect_true(all(morph_artifacts$role %in% "derived"))
    expect_true(all(output_artifacts$role %in% "output"))
    expect_named(
        morphed@meta$workflow,
        c(
            "preflight",
            "climate",
            "baseline",
            "preview",
            "plan",
            "diagnostics",
            "cases",
            "results",
            "outputs"
        )
    )
    expect_null(morphed@meta$workflow$outputs)
    expect_true(nrow(shift_outputs(epws)) >= 1L)
    epw_run <- shift_run_get(epws)
    expect_identical(epw_run@ids$query_id, shift_ids(files)$query_id)
    expect_identical(epw_run@ids$morph_id, shift_ids(morphed)$morph_id)
    expect_true(S7::S7_inherits(shift_result(epw_run), ShiftOutputs))
    # Persisted standalone results retain their scientific comparison identity.
    summary <- shift_summary(epw_run, refresh = FALSE)
    expect_identical(unique(summary$method), transform@method)
    expect_identical(unique(summary$scale), transform@scale)
    expect_identical(unique(summary$reconstruction), transform@reconstruction)
    expect_setequal(summary$model, shift_outputs(epws)$source_id)
    expect_setequal(summary$scenario, shift_outputs(epws)$experiment_id)
    expect_equal(sum(summary$epw_files), nrow(shift_outputs(epws)))
    expect_equal(
        sum(summary$cases),
        data.table::uniqueN(shift_outputs(epws)$case_id)
    )
    expect_error(shift_complete(climate), "not the latest result")
    expect_equal(shift_status(shift_complete(epws)), "completed")
})

test_that("dynamic stack scopes restore nested and failing values", {
    stack <- new.env(parent = emptyenv())
    stack$values <- integer()

    expect_identical(shift_run__stack_current(stack, empty = 7L), 7L)
    observed <- shift_run__with_stack(stack, 1L, {
        c(
            shift_run__stack_current(stack),
            shift_run__with_stack(stack, 2L, shift_run__stack_current(stack)),
            shift_run__stack_current(stack)
        )
    })

    expect_identical(observed, c(1L, 2L, 1L))
    expect_identical(stack$values, integer())
    expect_error(
        shift_run__with_stack(stack, 3L, stop("scoped failure")),
        "scoped failure"
    )
    expect_identical(stack$values, integer())
})

test_that("standalone shift APIs carry run context without session arguments", {
    apis <- list(
        shift_datasets,
        shift_collect,
        shift_download,
        shift_extract,
        shift_morph,
        shift_epw,
        shift_export_epw
    )
    for (api in apis) {
        arguments <- names(formals(api))
        expect_false("session" %in% arguments)
        expect_false(".reporter" %in% arguments)
    }
    expect_false("progress" %in% names(formals(shift_datasets)))
    expect_true(all(c("store", "ui") %in% names(formals(shift_datasets))))
})

test_that("failure commands are concise for default stores and explicit otherwise", {
    default_store <- tempfile("shift-default-command-store-")
    custom_store <- tempfile("shift-custom-command-store-")
    withr::local_options(epwshiftr.dir_store = default_store)

    expect_identical(
        shift_print__run_command("shift_resume", "run-test", default_store),
        'shift_resume("run-test")'
    )
    expect_identical(
        shift_print__run_command(
            "shift_logs",
            "run-test",
            default_store,
            "tail = 20L"
        ),
        'shift_logs("run-test", tail = 20L)'
    )
    custom <- shift_print__run_command("shift_resume", "run-test", custom_store)
    expect_match(custom, 'shift_resume\\("run-test", store = ', perl = TRUE)
    expect_match(custom, store_normalize_path(custom_store), fixed = TRUE)
})

# vim: fdm=marker :
