# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

stage_test__site <- function() {
    shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
}

stage_test__calls <- function() {
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    calls
}

stage_test__nc_fixtures <- function(variables, year) {
    stats::setNames(
        vapply(
            variables,
            function(variable_id) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    year,
                    variable_id = variable_id
                )
                path
            },
            character(1L)
        ),
        variables
    )
}

stage_test__file_docs <- function(
    paths,
    prefix = NULL,
    frequency = NULL,
    table_id = NULL,
    datetime_start = NULL,
    datetime_end = NULL,
    activity_id = NULL,
    experiment_id = NULL
) {
    docs <- data.table::rbindlist(
        lapply(names(paths), function(variable_id) {
            args <- list(
                basename(paths[[variable_id]]),
                opendap_url = paths[[variable_id]],
                download_url = paths[[variable_id]],
                variable_id = variable_id
            )
            if (!is.null(datetime_start)) {
                args$datetime_start <- datetime_start
            }
            if (!is.null(datetime_end)) {
                args$datetime_end <- datetime_end
            }
            row <- do.call(esgf_test__file_docs, args)
            if (!is.null(frequency)) {
                row$frequency <- frequency
            }
            if (!is.null(table_id)) {
                row$table_id <- table_id
            }
            row
        }),
        fill = TRUE
    )
    if (!is.null(activity_id)) {
        data.table::set(docs, j = "activity_id", value = activity_id)
    }
    if (!is.null(experiment_id)) {
        data.table::set(docs, j = "experiment_id", value = experiment_id)
    }
    if (!is.null(prefix)) {
        tag <- paste0(prefix, "-", docs$variable_id)
        data.table::set(docs, j = "dataset_id", value = tag)
        data.table::set(docs, j = "master_id", value = tag)
        data.table::set(
            docs,
            j = "instance_id",
            value = paste0(tag, ".v20260101")
        )
        data.table::set(
            docs,
            j = "tracking_id",
            value = paste0("hdl:21.14100/", tag)
        )
        data.table::set(docs, j = "id", value = paste0(docs$title, "|", tag))
    }
    docs
}

test_that("shift diagnostics normalize empty partial tables", {
    partial <- data.table::data.table(
        stage = character(),
        severity = character()
    )
    diagnostics <- shift_stage__diagnostics_normalize(partial)

    expect_equal(nrow(diagnostics), 0L)
    expect_identical(diagnostics$code, character())
})

test_that("shift_inspect__with_store closes opened stores on return and error", {
    closed <- 0L
    fake_store <- structure(
        list(
            path = tempfile("fake-store-"),
            close = function() {
                closed <<- closed + 1L
                invisible(NULL)
            }
        ),
        class = "EsgStore"
    )
    testthat::local_mocked_bindings(
        shift_inspect__store = function(x, create = FALSE) fake_store
    )
    stage <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = fake_store$path,
        ids = list(query_id = "q-1")
    )
    expect_identical(
        shift_inspect__with_store(stage, function(store) store$path),
        fake_store$path
    )
    expect_identical(closed, 1L)
    expect_error(
        shift_inspect__with_store(stage, function(store) stop("boom")),
        "boom"
    )
    expect_identical(closed, 2L)
    # Caller-owned EsgStore instances stay open.
    expect_identical(
        shift_inspect__with_store(fake_store, function(store) store$path),
        fake_store$path
    )
    expect_identical(closed, 2L)
})

test_that("morph case failures update only their matching public case", {
    cases <- data.table::data.table(
        source_id = "IPSL-CM6A-LR",
        experiment_id = c("ssp126", "ssp585"),
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        period = "2050",
        status = "ready",
        missing_reason = NA_character_
    )
    morph_cases <- data.table::copy(cases)
    data.table::set(morph_cases, j = "status", value = c("failed", "completed"))
    data.table::set(
        morph_cases,
        j = "last_error",
        value = c("bounded target failed", NA_character_)
    )

    updated <- shift_run__apply_morph_case_status(cases, morph_cases)

    expect_identical(updated$status, c("failed", "ready"))
    expect_identical(
        updated$missing_reason,
        c("bounded target failed", NA_character_)
    )
})

test_that("shift_extract() fallback policy is available from collected files", {
    skip_on_cran()

    nc <- stage_test__nc_fixtures("tas", 2060L)[["tas"]]
    on.exit(unlink(nc), add = TRUE)

    docs <- esgf_test__file_docs(
        basename(nc),
        download_url = sprintf("https://example.org/%s", basename(nc)),
        include_opendap = FALSE
    )
    docs$size <- file.info(nc)$size
    docs$checksum <- NA_character_
    docs$checksum_type <- NA_character_

    calls <- stage_test__calls()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- stage_test__site()
    files <- shift_collect(req, store = tempfile("shift-store-"))
    periods <- epw_morph_periods(`2060s` = 2060L)
    time <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")

    remote_only <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "error"
    )
    expect_equal(shift_status(remote_only), "blocked")
    expect_true(any(shift_coverage(remote_only)$status %in% "failed"))
    expect_match(
        paste(shift_diagnostics(remote_only)$message, collapse = "\n"),
        "OPeNDAP is not available"
    )

    queued <- shift_download(files, run = FALSE, probe = FALSE)
    task <- data.table::as.data.table(queued)[1L]
    target <- task$target_path[[1L]]
    if (!shift_test__is_absolute_path(target)) {
        target <- file.path(files@store_path, target)
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(nc, target, overwrite = TRUE))

    local_fallback <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "auto"
    )
    expect_equal(shift_status(local_fallback), "extracted")
    expect_true(all(shift_coverage(local_fallback)$complete))
})

test_that("humidity fallback persists a canonical hurs extraction artifact", {
    skip_on_cran()

    variables <- c("huss", "tas", "ps", "snd")
    paths <- stage_test__nc_fixtures(variables, 2060L)
    on.exit(unlink(paths), add = TRUE)
    docs <- stage_test__file_docs(paths, prefix = "humidity")
    # Model the production layout where optional snow depth is a separate
    # complete identity that has no atmospheric humidity source variables.
    snd_rows <- which(docs$variable_id == "snd")
    data.table::set(docs, i = snd_rows, j = "frequency", value = "mon")
    data.table::set(docs, i = snd_rows, j = "table_id", value = "LImon")

    calls <- stage_test__calls()
    shift_test__mock_collect(docs, calls)

    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "day"
    )
    climate <- request |>
        shift_collect(store = tempfile("shift-derived-hurs-store-")) |>
        shift_extract(
            site = stage_test__site(),
            periods = epw_morph_periods(`2060s` = 2060L),
            fallback = "error"
        )
    derived <- shift_climate__derive_hurs_climate(
        climate,
        epw_morph_recipe("original_morphing")
    )
    coverage <- shift_coverage(derived)
    hurs <- coverage[variable_id == "hurs"]
    snd <- coverage[variable_id == "snd"]
    data <- shift_data(derived, variables = "hurs")

    expect_equal(nrow(hurs), 1L)
    expect_true(hurs$complete[[1L]])
    expect_equal(nrow(snd), 1L)
    expect_identical(snd$table_id[[1L]], "LImon")
    expect_true(all(data$units == "%"))
    expect_true(all(is.finite(data$value)))
    expect_true(all(data$derived_from == "huss,tas,ps"))
    expect_true(all(data$value > 0 & data$value < 150))

    artifact <- shift_inspect__with_store(derived, function(store) {
        artifact_id <- store$query(sprintf(
            "SELECT artifact_id FROM extraction_result WHERE plan_id = %s LIMIT 1",
            ddb_literal(priv(store)$conn, hurs$plan_id[[1L]])
        ))$artifact_id[[1L]]
        store$query(sprintf(
            "SELECT metadata_json FROM artifact WHERE artifact_id = %s",
            ddb_literal(priv(store)$conn, artifact_id)
        ))
    })
    expect_match(artifact$metadata_json[[1L]], "huss,tas,ps")

    reused <- shift_climate__derive_hurs_climate(
        derived,
        epw_morph_recipe("original_morphing"),
        resume = TRUE
    )
    expect_equal(
        shift_inspect__ids(reused)$plan_id,
        shift_inspect__ids(derived)$plan_id
    )
})

test_that("shift_morph() uses complete extraction plans by default", {
    skip_on_cran()

    nc <- stage_test__nc_fixtures("tas", 2060L)[["tas"]]
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

    calls <- stage_test__calls()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "day"
    )
    climate <- req |>
        shift_collect(
            store = tempfile("shift-store-"),
            label = "complete-subset"
        ) |>
        shift_extract(
            site = stage_test__site(),
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
        shift_inspect__ids(morphed)$plan_id,
        coverage$plan_id[coverage$complete]
    )
    expect_true(any(
        shift_diagnostics(morphed)$code %in% "ignored_incomplete_extraction"
    ))
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(blocked), "blocked")
    expect_equal(shift_status(shift_run__complete(morphed)), "partial")
})

test_that("shift_morph() resolves automatic and manual historical references", {
    skip_on_cran()

    variables <- epw_morph_variables(
        transform__recipe(monthly_transform("original_morphing"))
    )
    future_nc <- stage_test__nc_fixtures(variables, 2060L)
    reference_nc <- stage_test__nc_fixtures(variables, 1995L)
    on.exit(unlink(c(future_nc, reference_nc)), add = TRUE)

    future_docs <- stage_test__file_docs(
        future_nc,
        prefix = "future",
        frequency = "mon",
        table_id = "Amon"
    )
    reference_docs <- stage_test__file_docs(
        reference_nc,
        prefix = "historical",
        frequency = "mon",
        table_id = "Amon",
        datetime_start = "1995-01-01T00:00:00Z",
        datetime_end = "1995-12-31T23:59:59Z",
        activity_id = "CMIP",
        experiment_id = "historical"
    )
    calls <- stage_test__calls()
    shift_test__mock_collect_sequence(list(future_docs, reference_docs), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "mon"
    )
    store_path <- tempfile("shift-store-")
    future_periods <- epw_morph_periods(`2060s` = 2060L)
    reference_periods <- epw_morph_periods(reference = 1995L)

    climate <- req |>
        shift_collect(store = store_path, label = "future") |>
        shift_extract(
            site = stage_test__site(),
            periods = future_periods,
            variables = variables
        )

    transform <- monthly_transform("original_morphing")
    collect_count_before_baseline <- length(calls$collect_times)
    baseline_reference <- shift_morph(
        climate,
        transform = monthly_transform("epwshiftr"),
        strict = TRUE,
        overwrite = TRUE
    )
    expect_true(S7::S7_inherits(baseline_reference, ShiftMorphed))
    expect_null(baseline_reference@meta$reference)
    expect_equal(length(calls$collect_times), collect_count_before_baseline)
    missing_reference <- shift_inspect__with_store(climate, function(store) {
        morpher <- epw_morpher(
            store = store,
            epw = get_cache_epw(),
            transform = transform
        )
        morpher$preflight(
            plan_id = shift_inspect__ids(climate)$plan_id,
            periods = future_periods,
            strict = FALSE
        )
    })
    expect_true(any(missing_reference$code == "missing_reference_climate"))
    auto <- shift_morph(
        climate,
        transform = transform,
        reference = historical_reference(1995L),
        strict = TRUE,
        overwrite = TRUE
    )
    historical_collect_times <- calls$collect_times[3:4]
    expect_equal(
        vapply(historical_collect_times, `[[`, character(1L), "type"),
        c("Dataset", "File")
    )
    expect_true(all(vapply(
        historical_collect_times,
        function(x) {
            is.null(x$datetime_start) && is.null(x$datetime_stop)
        },
        logical(1L)
    )))
    reference_climate <- auto@meta$reference
    reference_ids <- shift_inspect__ids(reference_climate)
    plan_reference <- shift_reference_from_plan(
        reference_ids$plan_id,
        reference_periods
    )
    manual <- shift_morph(
        climate,
        transform = transform,
        reference = reference_climate,
        strict = TRUE
    )
    manual_plan <- shift_morph(
        climate,
        transform = transform,
        reference = plan_reference,
        strict = TRUE
    )

    expect_true(S7::S7_inherits(auto, ShiftMorphed))
    expect_true(S7::S7_inherits(reference_climate, ShiftClimate))
    expect_true(S7::S7_inherits(auto@meta$reference_spec, ShiftReferenceSpec))
    expect_equal(auto@meta$reference_spec@mode, "historical")
    expect_equal(shift_status(auto), "morphed")
    expect_equal(shift_status(reference_climate), "extracted")
    reference_rows <- shift_inspect__with_store(
        reference_climate,
        function(store) {
            shift_inspect__extraction_result_rows(store, reference_ids$plan_id)
        }
    )
    expect_equal(unique(reference_rows$experiment_id), "historical")
    expect_equal(shift_status(manual), "morphed")
    expect_equal(shift_status(manual_plan), "morphed")
    expect_error(
        shift_morph(
            climate,
            transform = monthly_transform("epwshiftr"),
            observed_reference = reference_climate
        ),
        "does not use.*observed_reference"
    )
    expect_true(sum(calls$values %in% "File") >= 2L)
})

test_that("ShiftRun handles continue directly across manual stage APIs", {
    skip_on_cran()

    nc <- stage_test__nc_fixtures("tas", 2060L)[["tas"]]
    on.exit(unlink(nc), add = TRUE)
    docs <- stage_test__file_docs(c(tas = nc), prefix = "run-pipe")
    calls <- stage_test__calls()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- stage_test__site()
    store_path <- tempfile("shift-run-pipe-store-")
    export_dir <- tempfile("shift-run-pipe-export-")
    on.exit(unlink(c(store_path, export_dir), recursive = TRUE), add = TRUE)

    files <- shift_collect(req, store = store_path, ui = shift_ui("none"))
    run_id <- files@ids$run_id
    run_files <- shift_run_get(run_id, store = store_path)

    expect_error(
        shift_morph(
            run_files,
            transform = daily_transform("epwshiftr"),
            strict = FALSE,
            ui = shift_ui("none")
        ),
        "currently at stage.*files.*ShiftFiles"
    )

    climate <- run_files |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            ui = shift_ui("none")
        )
    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_identical(climate@ids$run_id, run_id)

    morphed <- shift_run_get(run_id, store = store_path) |>
        shift_morph(
            transform = daily_transform("epwshiftr"),
            reference = climate,
            strict = FALSE,
            ui = shift_ui("none")
        )
    expect_true(S7::S7_inherits(morphed, ShiftMorphed))
    expect_identical(morphed@ids$run_id, run_id)

    epws <- shift_run_get(run_id, store = store_path) |>
        shift_epw_write(dir = "shift-epw", ui = shift_ui("none"))
    expect_true(S7::S7_inherits(epws, ShiftOutputs))
    expect_identical(epws@ids$run_id, run_id)
    expect_identical(
        shift_status(shift_run_get(run_id, store = store_path)),
        "waiting"
    )

    exported <- shift_run_get(run_id, store = store_path) |>
        shift_epw_write(dir = export_dir, ui = shift_ui("none"))
    expect_true(S7::S7_inherits(exported, ShiftOutputs))
    expect_identical(exported@ids$run_id, run_id)
    expect_true(all(file.exists(shift_outputs(exported)$export_path)))
    expect_identical(
        shift_status(shift_run_get(run_id, store = store_path)),
        "completed"
    )

    # ShiftRun also bridges shift_download() -> shift_extract() via ShiftDownload.
    store_dl <- tempfile("shift-run-dl-store-")
    on.exit(unlink(store_dl, recursive = TRUE), add = TRUE)
    files_dl <- shift_collect(req, store = store_dl, ui = shift_ui("none"))
    dl_run_id <- files_dl@ids$run_id
    dl <- shift_run_get(dl_run_id, store = store_dl) |>
        shift_download(run = FALSE, probe = FALSE, ui = shift_ui("none"))
    expect_true(S7::S7_inherits(dl, ShiftDownload))
    expect_identical(dl@ids$run_id, dl_run_id)
    climate_dl <- shift_run_get(dl_run_id, store = store_dl) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            ui = shift_ui("none")
        )
    expect_true(S7::S7_inherits(climate_dl, ShiftClimate))
    expect_identical(climate_dl@ids$run_id, dl_run_id)
})

test_that("shift_epw_write() exports ShiftOutputs without rewriting store EPWs", {
    skip_on_cran()

    nc <- stage_test__nc_fixtures("tas", 2060L)[["tas"]]
    on.exit(unlink(nc), add = TRUE)
    docs <- stage_test__file_docs(c(tas = nc), prefix = "export-outputs")
    calls <- stage_test__calls()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- stage_test__site()
    store_path <- tempfile("shift-export-store-")
    export_dir <- tempfile("shift-export-dir-")
    export_dir2 <- tempfile("shift-export-dir2-")
    on.exit(
        unlink(c(store_path, export_dir, export_dir2), recursive = TRUE),
        add = TRUE
    )

    climate <- shift_collect(req, store = store_path, ui = shift_ui("none")) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            ui = shift_ui("none")
        )
    morphed <- shift_morph(
        climate,
        transform = daily_transform("epwshiftr"),
        reference = climate,
        strict = FALSE,
        ui = shift_ui("none")
    )
    epws <- shift_epw_write(morphed, dir = "shift-epw", ui = shift_ui("none"))
    store_files <- file.path(store_path, shift_outputs(epws)$path)
    expect_true(all(file.exists(store_files)))

    expect_error(
        shift_epw_write(epws, ui = shift_ui("none")),
        "requires a target directory"
    )
    expect_error(
        shift_epw_write(
            epws,
            dir = export_dir,
            export_dir = export_dir2,
            ui = shift_ui("none")
        ),
        "only one of `dir` or `export_dir`"
    )

    testthat::local_mocked_bindings(
        epw_morpher = function(...) stop("Store EPWs should not be rewritten")
    )
    exported <- shift_epw_write(epws, dir = export_dir, ui = shift_ui("none"))
    expect_true(S7::S7_inherits(exported, ShiftOutputs))
    expect_true(all(file.exists(shift_outputs(exported)$export_path)))
    expect_identical(
        normalizePath(
            exported@meta$export_dir,
            winslash = "/",
            mustWork = TRUE
        ),
        normalizePath(export_dir, winslash = "/", mustWork = TRUE)
    )
    expect_identical(shift_status(shift_run_get(exported)), "completed")

    exported2 <- shift_epw_write(
        epws,
        export_dir = export_dir2,
        ui = shift_ui("none")
    )
    expect_true(all(file.exists(shift_outputs(exported2)$export_path)))
})

# vim: fdm=marker :
