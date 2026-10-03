# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift__cmip6_period_coverage = test_cmip6_period_coverage
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

test_that("failed standalone steps expose recovery identity and resume in place", {
    skip_if_not_installed("duckdb")

    attempts <- 0L
    file_docs <- esgf_test__file_docs("tas_day.nc")
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            attempts <<- attempts + 1L
            if (attempts == 1L) {
                stop("temporary catalog failure")
            }
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                esgf_test__dataset_docs()
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )
    store_path <- tempfile("shift-resume-stage-store-")
    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    failure <- tryCatch(
        shift_collect(request, store = store_path, ui = shift_ui("none")),
        epwshiftr_shift_error = identity
    )

    expect_s3_class(failure, "epwshiftr_shift_error")
    expect_match(failure$run_id, "^run_")
    expect_match(failure$step_id, "^step_")
    expect_identical(
        failure$store,
        normalizePath(store_path, winslash = "/", mustWork = TRUE)
    )
    expect_equal(
        shift_status(shift_run_get(failure$run_id, store = store_path)),
        "failed"
    )

    resumed <- shift_resume(
        failure$run_id,
        store = store_path,
        ui = shift_ui("none")
    )
    expect_s7_class(resumed, ShiftFiles)
    expect_identical(shift_ids(resumed)$run_id, failure$run_id)
    expect_equal(shift_status(shift_run_get(resumed)), "waiting")
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

    expect_identical(shift__stack_current(stack, empty = 7L), 7L)
    observed <- shift__with_stack(stack, 1L, {
        c(
            shift__stack_current(stack),
            shift__with_stack(stack, 2L, shift__stack_current(stack)),
            shift__stack_current(stack)
        )
    })

    expect_identical(observed, c(1L, 2L, 1L))
    expect_identical(stack$values, integer())
    expect_error(
        shift__with_stack(stack, 3L, stop("scoped failure")),
        "scoped failure"
    )
    expect_identical(stack$values, integer())
})

test_that("artifact row reader preserves order, metadata, and global limits", {
    root <- tempfile("shift-artifact-reader-")
    dir.create(root)
    paths <- file.path(root, paste0("part-", 1:3, ".data"))
    expect_true(all(file.create(paths)))
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    records <- data.frame(
        relative_path = basename(paths),
        artifact_label = c("first", "second", "third"),
        check.names = FALSE
    )
    calls <- new.env(parent = emptyenv())
    calls$paths <- character()
    calls$limits <- numeric()
    reader <- function(path, limit, columns) {
        calls$paths <- c(calls$paths, basename(path))
        calls$limits <- c(calls$limits, limit)
        rows <- data.table::data.table(value = c(1L, 2L))
        if (!is.infinite(limit)) {
            rows <- utils::head(rows, limit)
        }
        rows
    }

    out <- shift__read_artifact_rows(
        store = list(path = root),
        records = records,
        n = 3L,
        columns = c("artifact_label", "value"),
        path_column = "relative_path",
        reader = reader,
        metadata = function(records, i) {
            list(artifact_label = records$artifact_label[[i]])
        },
        missing = c(
            "Fixture artifact is missing.",
            "x" = "{.path {path}}"
        ),
        stage = "fixture"
    )

    expect_named(out, c("artifact_label", "value"))
    expect_identical(out$artifact_label, c("first", "first", "second"))
    expect_identical(out$value, c(1L, 2L, 1L))
    expect_identical(calls$paths, basename(paths[1:2]))
    expect_identical(calls$limits, c(3, 1))

    unlink(paths[[1L]])
    expect_error(
        shift__read_artifact_rows(
            store = list(path = root),
            records = records[1L, , drop = FALSE],
            n = Inf,
            path_column = "relative_path",
            reader = reader,
            missing = c(
                "Fixture artifact is missing.",
                "x" = "{.path {path}}"
            ),
            stage = "fixture"
        ),
        "Fixture artifact is missing"
    )
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

test_that("foreground interrupts persist one meaningful cancelled state", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-interrupt-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-interrupt-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    testthat::local_mocked_bindings(
        shift__collect_resolved_inputs = function(...) {
            stop(structure(
                list(message = "", call = NULL),
                class = c("interrupt", "condition")
            ))
        },
        .package = "epwshiftr"
    )

    interrupted <- tryCatch(
        shift_run(plan, ui = shift_ui("none")),
        interrupt = function(e) e
    )
    expect_s3_class(interrupted, "epwshiftr_shift_cancelled")
    expect_equal(conditionMessage(interrupted), "Interrupted by user.")

    run <- shift_run_get(interrupted$run_id, store = store_path)
    expect_equal(shift_status(run), "cancelled")
    expect_false(is.na(run@meta$run$completed_at[[1L]]))
    expect_equal(run@meta$run$last_error[[1L]], "Interrupted by user.")
    logs <- shift_logs(run)
    expect_gt(nrow(logs), 0L)
    expect_true(all(logs$source == "event"))
    terminal <- run@meta$events[status %in% c("cancelled", "failed")]
    expect_equal(terminal$status, "cancelled")
    expect_equal(terminal$message, "Interrupted by user.")
})

test_that("an identical complete workflow returns its original durable run", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-idempotent-run-store-")
    output_dir <- tempfile("shift-idempotent-run-output-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = output_dir,
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    private <- morpher__private_store(store)
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == run_id]

    canonical <- file.path(store_path, "outputs", "future.epw")
    exported <- file.path(output_dir, "future.epw")
    dir.create(dirname(canonical), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(exported), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.create(canonical))
    expect_true(file.create(exported))
    output_id <- "output-idempotent"
    morph_id <- "morph-idempotent"
    cases[, `:=`(
        status = "completed",
        output_id = "output-idempotent",
        export_path = exported,
        missing_reason = NA_character_
    )]
    shift__run_cases_write(store, run_id, cases)
    private$append_new_rows(
        "epw_output",
        data.frame(
            output_id = output_id,
            morph_id = morph_id,
            case_id = cases$case_id[[1L]],
            result_id = "result-idempotent",
            artifact_id = NA_character_,
            path = store_rel_path(canonical, root = store_path),
            source_id = cases$source_id[[1L]],
            experiment_id = cases$experiment_id[[1L]],
            variant_label = cases$variant_label[[1L]],
            period = cases$period[[1L]],
            output_type = "representative_year",
            sequence_id = NA_character_,
            weather_year = NA_integer_,
            calendar = "365_day",
            stochastic_seed = NA_integer_,
            member_count = 1L,
            provenance_json = "[]",
            created_at = store__now(),
            stringsAsFactors = FALSE
        ),
        "output_id"
    )
    shift__run_update(
        store,
        run_id,
        status = "completed",
        current_stage = "completed",
        morph_id = morph_id,
        completed_at = store__now()
    )
    stored_run <- private$read_table("shift_run")
    stored_run <- stored_run[stored_run[["run_id"]] == run_id]
    stored_cases <- private$read_table("shift_run_case")
    stored_cases <- stored_cases[stored_cases[["run_id"]] == run_id]
    stored_outputs <- private$read_table("epw_output")
    expect_identical(stored_run$status[[1L]], "completed")
    expect_true(all(stored_cases$status == "completed"))
    expect_true(all(file.exists(stored_cases$export_path)))
    expect_identical(stored_run$morph_id[[1L]], morph_id)
    expect_true(output_id %in% stored_outputs$output_id)
    expect_true(file.exists(store_abs_path(
        stored_outputs$path[[1L]],
        root = store_path
    )))
    expect_true(shift__run_artifacts_complete(store, run_id))
    expected_hash <- store__hash(shift__spec_json(shift__plan_spec(plan)))
    expect_identical(
        stored_run[["spec_hash"]][[1L]],
        expected_hash
    )
    store$close()
    before <- file.info(c(canonical, exported))[, "mtime", drop = TRUE]
    checksums <- unname(tools::md5sum(c(canonical, exported)))

    existing <- shift__run_existing(plan)
    expect_s7_class(existing, ShiftRun)
    expect_identical(existing@ids$run_id, run_id)
    reused <- shift_run(plan, ui = shift_ui("none"))

    expect_s7_class(reused, ShiftRun)
    expect_identical(reused@ids$run_id, run_id)
    expect_identical(shift_status(reused, refresh = FALSE), "completed")
    expect_identical(
        file.info(c(canonical, exported))[, "mtime", drop = TRUE],
        before
    )
    expect_identical(
        unname(tools::md5sum(c(canonical, exported))),
        checksums
    )
    reopened <- shift_store(plan)
    on.exit(reopened$close(), add = TRUE)
    runs <- morpher__private_store(reopened)$read_table("shift_run")
    expect_equal(
        nrow(runs[
            runs[["spec_hash"]] ==
                runs[runs[["run_id"]] == run_id][["spec_hash"]][[1L]]
        ]),
        1L
    )
})

test_that("an identical interrupted workflow resumes its original run ID", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-idempotent-resume-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-idempotent-resume-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    shift__run_update(
        store,
        run_id,
        status = "failed",
        current_stage = "resolve",
        last_error = "interrupted fixture"
    )
    store$close()

    resumed_ids <- character()
    testthat::local_mocked_bindings(
        shift__resume_one = function(x, background, ui, execution = NULL) {
            resumed_ids <<- c(resumed_ids, x@ids$run_id)
            x
        },
        .package = "epwshiftr"
    )
    resumed <- shift_run(plan, ui = shift_ui("none"))

    expect_identical(resumed@ids$run_id, run_id)
    expect_identical(resumed_ids, run_id)
    reopened <- shift_store(plan)
    on.exit(reopened$close(), add = TRUE)
    runs <- morpher__private_store(reopened)$read_table("shift_run")
    expect_equal(nrow(runs), 1L)
})

test_that("background live sidecars carry transient reporter state without events", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-live-ui-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-live-ui-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    job <- shift__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none", heartbeat = 0)
    )
    initial_events <- nrow(morpher__private_store(store)$read_table(
        "shift_run_event"
    ))
    reporter <- shift__reporter(
        shift_ui("none", heartbeat = 0),
        store = store,
        run_id = run_id,
        job_id = job$job_id[[1L]]
    )
    reporter$heartbeat(
        "Reading tas",
        details = list(
            stage = "extract_future",
            unit_type = "extraction_plan",
            scenario = "ssp585",
            variable = "tas",
            access_method = "OPeNDAP",
            transfer_state = "waiting"
        ),
        force = TRUE
    )

    live <- shift__live_run_get(run_id, store_path)
    expect_s7_class(live, ShiftRun)
    expect_identical(live@meta$ui_state$current_details$variable, "tas")
    expect_identical(
        live@meta$ui_state$current_details$access_method,
        "OPeNDAP"
    )
    expect_equal(
        nrow(morpher__private_store(store)$read_table(
            "shift_run_event"
        )),
        initial_events
    )
})

test_that("successful-run scientific diagnostics survive refresh", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-scientific-diagnostic-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6("Model-A", "ssp585"),
        periods = list(`2050` = 2050L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-scientific-diagnostic-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    diagnostic <- shift_diagnostic(
        "plan",
        "warning",
        "dry_baseline_precip",
        "Baseline month contains no wet hours.",
        variable_id = "pr",
        epw_field = "liquid_precip_depth",
        period = "2050",
        month = 6L,
        action = "Keep the dry baseline month."
    )

    shift__run_diagnostics_record(store, run_id, diagnostic)
    shift__run_diagnostics_record(store, run_id, diagnostic)
    refreshed <- shift__run_handle(store, run_id)
    actual <- shift_diagnostics(refreshed, refresh = FALSE)

    expect_equal(nrow(actual), 1L)
    expect_identical(actual$code, "dry_baseline_precip")
    expect_identical(actual$severity, "warning")
    expect_identical(actual$variable_id, "pr")
    expect_equal(
        nrow(refreshed@meta$events[status == "diagnostic"]),
        1L
    )
})

test_that("background runs register live jobs before launching workers", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-background-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-background-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    launched <- new.env(parent = emptyenv())
    test_local_dependencies(list(shift__launch_job = function(
        store_path,
        run_id,
        job_id,
        log_path
    ) {
        launched$args <- list(
            store_path = store_path,
            run_id = run_id,
            job_id = job_id,
            log_path = log_path
        )
        invisible(0L)
    }))
    run <- shift_run(
        plan,
        background = TRUE,
        ui = shift_ui("none", motion = "reduced", refresh = 0.25, heartbeat = 7)
    )
    expect_equal(shift_status(run), "queued")
    expect_equal(launched$args$run_id, shift_ids(run)$run_id)
    expect_true(startsWith(
        launched$args$log_path,
        normalizePath(store_path, winslash = "/")
    ))
    expect_equal(run@meta$jobs$mode, "process")
    expect_equal(run@meta$jobs$status, "queued")
    ui_spec <- jsonlite::fromJSON(run@meta$jobs$ui_json[[1L]])
    expect_identical(ui_spec$motion, "reduced")
    expect_equal(ui_spec$refresh, 0.25)
    expect_equal(ui_spec$heartbeat, 7)
    expect_equal(nrow(shift_logs(run)), 0L)

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "cancelled")
    expect_equal(cancelled@meta$jobs$status, "cancelled")
})

test_that("live sidecars keep background handles readable while DuckDB is locked", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-live-lock-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-live-lock-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    test_local_dependencies(list(shift__launch_job = function(...) {
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))

    ready <- tempfile("shift-live-lock-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(2)",
        "dbDisconnect(conn, shutdown = TRUE)",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "-e",
            shQuote(child_code),
            shQuote(file.path(store_path, "manifest.duckdb")),
            shQuote(ready)
        ),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) {
            break
        }
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))
    expect_equal(shift_status(run), "queued")

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "stopping")
    expect_true(file.exists(shift__live_path(
        store_path,
        shift_ids(run, refresh = FALSE)$run_id,
        "cancel.json"
    )))
})

test_that("background workers retry transient DuckDB launch locks", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-worker-open-store-")
    store <- EsgStore$new(store_path)
    store$close()
    ready <- tempfile("shift-worker-open-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(0.5)",
        "dbDisconnect(conn, shutdown = TRUE)",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "-e",
            shQuote(child_code),
            shQuote(file.path(store_path, "manifest.duckdb")),
            shQuote(ready)
        ),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) {
            break
        }
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))

    # This call represents the detached worker starting while a short-lived
    # status reader still owns the manifest.
    worker_store <- shift__job_store_open(
        store_path,
        timeout = 3,
        interval = 0.05
    )
    on.exit(worker_store$close(), add = TRUE)
    expect_true(inherits(worker_store, "EsgStore"))
})

test_that("failure commands are concise for default stores and explicit otherwise", {
    default_store <- tempfile("shift-default-command-store-")
    custom_store <- tempfile("shift-custom-command-store-")
    withr::local_options(epwshiftr.dir_store = default_store)

    expect_identical(
        shift__run_command("shift_resume", "run-test", default_store),
        'shift_resume("run-test")'
    )
    expect_identical(
        shift__run_command(
            "shift_logs",
            "run-test",
            default_store,
            "tail = 20L"
        ),
        'shift_logs("run-test", tail = 20L)'
    )
    custom <- shift__run_command("shift_resume", "run-test", custom_store)
    expect_match(custom, 'shift_resume\\("run-test", store = ', perl = TRUE)
    expect_match(custom, store_normalize_path(custom_store), fixed = TRUE)
})
