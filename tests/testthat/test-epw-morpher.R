# Construct a morpher from an internal recipe for legacy-backend tests.
# epw_morpher_test__from_recipe {{{
epw_morpher_test__from_recipe <- function(
    store,
    epw,
    recipe,
    site_id = NULL,
    label = NULL
) {
    object <- epw_morpher(
        store = store,
        epw = epw,
        site_id = site_id,
        label = label
    )
    private <- priv(object)
    private$transform <- NULL
    private$recipe <- recipe
    object
}
# }}}

# Provision a temporary EsgStore populated and extracted from local CMIP6
# NetCDF fixtures so EpwMorpher tests share one clean store-setup path.
# morpher_test__extracted_store {{{
morpher_test__extracted_store <- function(
    variables = "tas",
    year = 2060L,
    n_years = 1L,
    calendar = "standard",
    frequency = "day",
    table_id = "day",
    time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
    datetime_end = sprintf("%d-12-31T23:59:59Z", year + n_years - 1L),
    experiments = NULL,
    envir = parent.frame()
) {
    root <- withr::local_tempdir(.local_envir = envir)
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close(), envir = envir)
    build_doc <- function(variable_id, experiment = NULL) {
        suffix <- paste(c(variable_id, experiment), collapse = "-")
        path <- file.path(root, paste0(suffix, ".nc"))
        write_local_cmip6_netcdf_fixture(
            path,
            year,
            variable_id = variable_id,
            frequency = frequency,
            calendar = calendar,
            n_years = n_years
        )
        rows <- esgf_test__file_docs(
            path = basename(path),
            opendap_url = path,
            download_url = path,
            variable_id = variable_id,
            frequency = frequency,
            table_id = table_id,
            datetime_end = datetime_end
        )
        if (!is.null(experiment)) {
            rows$id <- paste0(rows$id, "-", experiment)
            rows$instance_id <- paste0(rows$instance_id, "-", experiment)
            rows$master_id <- paste0(rows$master_id, "-", experiment)
            rows$experiment_id <- experiment
        }
        rows
    }
    doc_list <- if (is.null(experiments)) {
        lapply(variables, build_doc)
    } else {
        unlist(
            lapply(experiments, function(exp) {
                lapply(variables, build_doc, experiment = exp)
            }),
            recursive = FALSE
        )
    }
    docs <- as.data.frame(data.table::rbindlist(doc_list, fill = TRUE))
    query_id <- store$add_files(esgf_test__file_result(docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = time,
        site_id = "SIN",
        variable_id = if (length(variables) > 1L) variables else NULL
    )
    extracted <- store$extract(plan_id = plan$plan_id)
    expect_true(all(extracted$status == "done"))
    list(root = root, store = store, plan = plan, docs = docs)
}
# }}}

test_that("get_cache_epw() prepares a stable local EPW fixture", {
    dir <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = dir)

    path <- get_cache_epw()

    expect_true(file.exists(path))
    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")

    epw <- epw_file_read(path)
    expect_equal(epw$location()$city, "Singapore")
    expect_equal(epw$location()$country, "Singapore")
    expect_equal(nrow(epw$data()), 8760L)
    expect_gt(sum(epw$data()$liquid_precip_depth, na.rm = TRUE), 0)

    expect_identical(get_cache_epw(), path)

    stale_lines <- readLines(path, warn = FALSE)
    stale_weather <- strsplit(stale_lines[-seq_len(8L)], ",", fixed = TRUE)
    stale_weather <- lapply(stale_weather, function(x) {
        x[[34L]] <- "0.0"
        x[[35L]] <- "0.0"
        x
    })
    writeLines(
        c(
            stale_lines[seq_len(8L)],
            vapply(stale_weather, paste, character(1L), collapse = ",")
        ),
        path
    )

    expect_identical(get_cache_epw(), path)
    epw <- epw_file_read(path)
    expect_gt(sum(epw$data()$liquid_precip_depth, na.rm = TRUE), 0)
})

test_that("packaged Singapore EPW fixture is readable", {
    path <- system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr",
        mustWork = TRUE
    )

    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")
    expect_equal(epw_file_read(path)$location()$city, "Singapore")

    external <- test_external_epw(path)
    original_path <- external$path()
    converted <- epw_file_coerce(external)
    expect_true(inherits(converted, "EpwFile"))
    expect_equal(converted$location()$city, "Singapore")
    # Conversion saves a deep clone and must not mutate the caller's object.
    expect_identical(external$path(), original_path)
})

test_that("climate statistics pool raw file fragments before summarising", {
    climate <- data.table::data.table(
        plan_id = c("plan-a", "plan-a", "plan-b", "plan-b", "plan-b"),
        site_id = "SIN",
        source_id = "Model-A",
        experiment_id = "ssp370",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        period = "2040",
        month = 1L,
        units = "K",
        value = c(280, NA, 300, 300, 300),
        lon = 104,
        lat = 29.5
    )

    summary <- morpher__stat_rows(climate)
    expect_equal(nrow(summary), 3L)
    expect_true(all(is.na(summary$plan_id)))
    expect_equal(summary$n_records, rep(5L, 3L))
    expect_equal(summary$n_valid, rep(4L, 3L))
    expect_equal(summary[stat == "mean", value], 295)
    expect_equal(summary[stat == "min", value], 280)
    expect_equal(summary[stat == "max", value], 300)
})

test_that("climate statistics preserve all-missing groups as missing", {
    climate <- data.table::data.table(
        plan_id = c("plan-a", "plan-b"),
        site_id = "SIN",
        source_id = "Model-A",
        experiment_id = "ssp370",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        period = "2040",
        month = 1L,
        units = "K",
        value = NA_real_
    )

    summary <- morpher__stat_rows(climate)
    expect_true(all(is.na(summary$value)))
    expect_equal(summary$n_records, rep(2L, 3L))
    expect_equal(summary$n_valid, rep(0L, 3L))
})

test_that("climate records expand across overlapping morphing periods", {
    climate <- data.table::data.table(
        year = rep(c(2030L, 2040L, 2050L), each = 100L),
        record_id = seq_len(300L)
    )
    periods <- data.table::data.table(
        period = rep(c("2030", "2040"), each = 2L),
        year = c(2030L, 2040L, 2040L, 2050L)
    )

    assigned <- morpher__assign_periods(climate, periods)

    expect_equal(nrow(assigned), 400L)
    expect_equal(assigned[, .N, by = period]$N, c(200L, 200L))
    expect_equal(assigned[year == 2040L, data.table::uniqueN(period)], 2L)
    expect_equal(assigned[year == 2040L, data.table::uniqueN(record_id)], 100L)
})

test_that("EpwMorpher$preflight() accepts overlapping climate periods", {
    fx <- morpher_test__extracted_store(
        n_years = 2L,
        time = c("2060-01-01T00:00:00Z", "2061-12-31T23:59:59Z")
    )
    morpher <- epw_morpher_test__from_recipe(
        store = fx$store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        ))
    )
    periods <- epw_morph_periods(early = 2060:2061, late = 2061L)

    diagnostics <- morpher$preflight(
        plan_id = fx$plan$plan_id,
        periods = periods,
        strict = FALSE
    )

    expect_named(diagnostics, morpher__diagnostic_columns())
    expect_false(any(diagnostics$code == "no_period_rows"))
})

test_that("legacy fragmented mean summaries use valid record weights", {
    fragments <- data.table::data.table(
        stat = "mean",
        value = c(280, 300),
        units = "K",
        n_valid = c(1L, 3L),
        n_records = c(1L, 3L)
    )

    expect_equal(
        morpher__pooled_mean(fragments),
        295
    )

    # Old persisted summaries lack valid-value counts, so fall back to their
    # total record counts before using equal weights as a last resort.
    legacy <- fragments[, .(stat, value, units, n_records)]
    expect_equal(
        morpher__pooled_mean(legacy),
        295
    )
    uncounted <- fragments[, .(stat, value, units)]
    expect_equal(
        morpher__pooled_mean(uncounted),
        290
    )
    expect_error(
        morpher__pooled_mean(
            data.table::copy(fragments)[, `:=`(stat = "max")]
        ),
        "monthly mean"
    )
    expect_error(
        morpher__pooled_mean(
            data.table::copy(fragments)[, `:=`(units = c("K", "degC"))]
        ),
        "different units"
    )
})

test_that("factor planning reads legacy record-weighted mean fragments", {
    store <- EsgStore$new(tempfile("weighted-factor-store-"))
    on.exit(store$close(), add = TRUE)
    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        transform = monthly_transform("original_morphing")
    )
    climate <- data.table::data.table(
        source_id = "Model-A",
        experiment_id = "ssp370",
        variant_label = "r1i1p1f1",
        period = "2040",
        variable_id = "tas",
        month = 1L,
        stat = "mean",
        value = c(280, 300),
        units = "K",
        n_records = c(1L, 3L),
        years_json = "[2040]"
    )
    reference <- data.table::data.table(
        source_id = "Model-A",
        experiment_id = "historical",
        variant_label = "r1i1p1f1",
        period = "reference",
        variable_id = "tas",
        month = 1L,
        stat = "mean",
        value = c(270, 274),
        units = "K",
        n_records = c(3L, 1L),
        years_json = "[2000]"
    )
    baseline <- data.table::data.table(
        epw_field = "dry_bulb_temperature",
        month = 1L,
        value = 20,
        units = "degC"
    )
    by <- c("source_id", "experiment_id", "variant_label", "period")

    factors <- morpher$.__enclos_env__$private$factor_rows(
        "morph-fragmented",
        climate,
        baseline,
        by,
        strict = FALSE,
        reference = reference
    )
    dry_bulb <- factors[
        epw_field == "dry_bulb_temperature" & month == 1L
    ]

    expect_equal(dry_bulb$future, 21.85, tolerance = 1e-10)
    expect_equal(dry_bulb$reference, -2.15, tolerance = 1e-10)
    expect_equal(dry_bulb$delta, 24, tolerance = 1e-10)
})


test_that("baseline summaries and preflight preserve EPW missing-value evidence", {
    source <- epw_file_read(get_cache_epw())
    weather <- source$data()
    february <- weather$month == 2L
    january_partial <- weather$month == 1L & seq_len(nrow(weather)) <= 8L
    weather[
        february | january_partial,
        `:=`(
            liquid_precip_depth = 999,
            liquid_precip_rate = 99
        )
    ]
    source$set(weather)
    path <- tempfile(fileext = ".epw")
    source$save(path)

    store <- EsgStore$new(tempfile("missing-epw-store-"))
    on.exit(store$close(), add = TRUE)
    morpher <- epw_morpher_test__from_recipe(
        store = store,
        epw = path,
        site_id = "SIN",
        recipe = suppressWarnings(
            epw_morph_recipe("original_morphing_absolute")
        )
    )

    baseline <- morpher$summarise_baseline()
    expect_false(any(
        baseline$epw_field == "liquid_precip_depth" &
            baseline$month == 2L
    ))

    diagnostics <- morpher$.__enclos_env__$private$preflight_baseline(
        unique(baseline$baseline_id),
        strict = TRUE
    )
    precipitation <- diagnostics[
        epw_field == "liquid_precip_depth" & month %in% c(1L, 2L)
    ]
    expect_equal(nrow(precipitation), 2L)
    expect_equal(precipitation$code, rep("missing_epw_values", 2L))
    expect_match(
        precipitation[month == 1L, message],
        "missing for 8 of 744 hours"
    )
    expect_match(
        precipitation[month == 2L, message],
        "missing for all 672 hours"
    )
    expect_false(any(
        diagnostics$code == "missing_baseline_month" &
            diagnostics$epw_field == "liquid_precip_depth" &
            diagnostics$month == 2L
    ))

    # Case-level factor diagnostics must identify the EPW baseline as the
    # missing source instead of implying that the CMIP6 `pr` input is absent.
    factors <- data.table::data.table(
        status = "missing_baseline",
        case_id = "case-1",
        variable_id = "pr",
        epw_field = "liquid_precip_depth",
        period = "2050",
        month = 2L
    )
    factor_diagnostic <- morpher$.__enclos_env__$private$factor_diagnostics(
        factors,
        strict = FALSE,
        morph_id = "morph-1"
    )
    expect_identical(factor_diagnostic$code, "missing_baseline")
    expect_match(
        factor_diagnostic$message,
        "Baseline EPW has no valid monthly value"
    )
    expect_false(grepl("from pr", factor_diagnostic$message, fixed = TRUE))
    expect_match(factor_diagnostic$action, "valid liquid_precip_depth")
})

test_that("EpwMorpher isolates case failures and persists current case state", {
    skip_on_cran()
    fx <- morpher_test__extracted_store(experiments = c("ssp126", "ssp585"))
    store <- fx$store
    extraction <- fx$plan

    morpher <- epw_morpher_test__from_recipe(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(
            epw_morph_recipe("original_morphing_absolute")
        )
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    climate <- morpher$summarise_climate(
        extraction$plan_id,
        periods,
        strict = FALSE
    )
    summary_id <- unique(climate$summary_id)
    baseline <- morpher$summarise_baseline()
    plan <- morpher$plan(
        summary_id = summary_id,
        baseline_id = unique(baseline$baseline_id),
        by = c("source_id", "experiment_id", "variant_label", "period"),
        strict = FALSE
    )

    original_run <- morpher__run_context
    testthat::local_mocked_bindings(
        morpher__run_context = function(context) {
            if (identical(context$case$experiment_id[[1L]], "ssp126")) {
                stop("synthetic method failure", call. = FALSE)
            }
            original_run(context)
        },
        .package = "epwshiftr"
    )
    results <- morpher$run(plan$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    provenance <- jsonlite::fromJSON(results$provenance_json[[1L]])
    expect_named(
        provenance$weather_field_roles,
        c(
            "transformed_fields",
            "derived_fields",
            "physically_closed_fields",
            "inherited_fields"
        )
    )
    expect_identical(morpher$status(plan$morph_id)$status, "result_partial")

    cases <- morpher__read_table(store, "epw_morph_case")
    cases <- cases[cases$morph_id == plan$morph_id]
    expect_identical(
        cases$status[match(c("ssp126", "ssp585"), cases$experiment_id)],
        c("failed", "completed")
    )
    expect_match(
        cases[experiment_id == "ssp126", last_error],
        "synthetic method failure"
    )
    diagnostics <- morpher__read_table(store, "epw_morph_diagnostic")
    diagnostic <- diagnostics[diagnostics[["code"]] == "morph_case_failed"]
    expect_equal(nrow(diagnostic), 1L)
    expect_identical(diagnostic$morph_id, plan$morph_id[[1L]])
    expect_match(diagnostic$message, "ssp126")
    expect_match(diagnostic$message, "2060s")

    outputs <- morpher$write_epw(
        plan$morph_id,
        dir = "outputs/isolation",
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    expect_identical(morpher$status(plan$morph_id)$status, "epw_partial")
})


test_that("EpwMorpher$summarise_climate() selects 360-day CF years and months", {
    fx <- morpher_test__extracted_store(
        calendar = "360_day",
        n_years = 2L,
        time = c("2060-12-30T00:00:00Z", "2061-01-01T23:59:59Z")
    )
    morpher <- epw_morpher_test__from_recipe(
        store = fx$store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        ))
    )
    climate <- morpher$summarise_climate(
        fx$plan$plan_id,
        epw_morph_periods(`2061` = 2061L),
        strict = FALSE
    )

    expect_identical(unique(climate$period), "2061")
    expect_identical(unique(climate$month), 1L)
    expect_true(all(climate$n_records == 1L))
    expect_true(all(climate$n_valid == 1L))
    expect_true(all(is.na(climate$plan_id)))
    sources <- morpher__read_table(fx$store, "epw_climate_summary_plan")
    sources <- sources[sources$summary_id == unique(climate$summary_id)]
    expect_identical(sources$plan_id, fx$plan$plan_id)
})


test_that("epw_morpher() / EpwMorpher$required_variables() / EpwMorpher$summarise_climate() / EpwMorpher$summarise_baseline() / EpwMorpher$plan() / EpwMorpher$diagnose() / EpwMorpher$check() / EpwMorpher$run() / EpwMorpher$write_epw() / EpwMorpher$status() / EpwMorpher$outputs() create relaxed future EPW outputs from store extracts", {
    skip_on_cran()
    fx <- morpher_test__extracted_store()
    store <- fx$store
    plan <- fx$plan

    external_epw <- test_external_epw(get_cache_epw())
    original_external_path <- external_epw$path()
    morpher <- epw_morpher_test__from_recipe(
        store = store,
        epw = external_epw,
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        )),
        label = "singapore"
    )
    expect_true(inherits(morpher, "EpwMorpher"))
    expect_identical(external_epw$path(), original_external_path)
    expect_setequal(
        morpher$required_variables(),
        epw_morph_variables("recommended")
    )

    periods <- epw_morph_periods(`2060s` = 2060L)
    strict_preflight <- morpher$preflight(plan$plan_id, periods, strict = TRUE)
    expect_named(strict_preflight, morpher__diagnostic_columns())
    expect_true(all(
        c("missing_required_variable", "missing_month") %in%
            strict_preflight$code
    ))
    expect_true(any(strict_preflight$severity == "error"))
    expect_error(
        morpher$summarise_climate(plan$plan_id, periods, strict = TRUE),
        "blocking issues"
    )

    relaxed_preflight <- morpher$preflight(
        plan$plan_id,
        periods,
        strict = FALSE
    )
    expect_true(any(relaxed_preflight$severity == "warning"))
    missing_hurs <- relaxed_preflight[
        code == "missing_required_variable" & variable_id == "hurs"
    ]
    expect_match(
        missing_hurs$message,
        "requires near-surface relative humidity"
    )
    expect_match(missing_hurs$action, "relative humidity and dew point")

    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = FALSE)
    expect_equal(unique(climate$variable_id), "tas")
    expect_equal(unique(climate$units), "K")

    baseline <- morpher$summarise_baseline()
    expect_true(all(
        c("dry_bulb_temperature", "relative_humidity") %in% baseline$epw_field
    ))

    preview <- morpher$preview_plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(preview$plan$status, "blocked")
    expect_equal(nrow(morpher$status(preview$plan$morph_id)), 0L)
    expect_true(any(preview$diagnostics$severity == "error"))

    blocked <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(blocked$status, "blocked")
    expect_true(any(morpher$diagnose(blocked$morph_id)$severity == "error"))
    expect_error(morpher$check(blocked$morph_id), "blocking issues")

    relaxed <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = FALSE
    )
    expect_equal(relaxed$status, "planned")
    expect_true(any(morpher$diagnose(relaxed$morph_id)$severity == "warning"))

    results <- morpher$run(relaxed$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))
    expect_identical(
        results$result_id,
        morpher__hash(relaxed$morph_id, results$case_id, result_path)
    )

    result_data <- read_test_parquet(result_path)
    expect_true(all(
        c("source_id", "experiment_id", "variant_label", "period") %in%
            names(result_data)
    ))
    expect_equal(unique(result_data$period), "2060s")
    expect_equal(morpher$status(relaxed$morph_id)$status, "result_done")

    expect_error(
        morpher$write_epw(
            morph_id = relaxed$morph_id,
            dir = tempfile("future-epw-outside-")
        ),
        "inside the epwshiftr store root"
    )

    outputs <- morpher$write_epw(
        morph_id = relaxed$morph_id,
        dir = "outputs/future-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_identical(
        outputs$output_id,
        morpher__hash(relaxed$morph_id, outputs$case_id, output_path)
    )
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(epw_file_read(output_path), "EpwFile"))
    expect_equal(morpher$status(relaxed$morph_id)$status, "epw_written")
    expect_equal(nrow(morpher$outputs(relaxed$morph_id)), 1L)
})

test_that("epw_morpher() / EpwMorpher$summarise_climate() / EpwMorpher$summarise_baseline() / EpwMorpher$plan() / EpwMorpher$diagnose() / EpwMorpher$check() / EpwMorpher$run() / EpwMorpher$write_epw() / EpwMorpher$status() / EpwMorpher$outputs() complete strict outputs with recommended variables", {
    skip_on_cran()
    variables <- epw_morph_variables("recommended")
    fx <- morpher_test__extracted_store(
        variables = variables,
        frequency = "mon",
        table_id = "Amon",
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    store <- fx$store
    plan <- fx$plan
    expect_setequal(plan$variable_id, variables)

    morpher <- epw_morpher_test__from_recipe(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        )),
        label = "singapore"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    expect_equal(
        nrow(morpher$preflight(plan$plan_id, periods, strict = TRUE)),
        0L
    )

    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = TRUE)
    expect_setequal(unique(climate$variable_id), variables)
    baseline <- morpher$summarise_baseline()
    summary_id <- unique(climate$summary_id)
    baseline_id <- unique(baseline$baseline_id)

    preview <- morpher$preview_plan(
        summary_id = summary_id,
        baseline_id = baseline_id,
        strict = TRUE
    )
    expect_equal(preview$plan$status, "planned")
    expect_equal(nrow(preview$diagnostics), 0L)
    expect_equal(nrow(morpher$status(preview$plan$morph_id)), 0L)

    bad_climate <- data.table::copy(climate)
    bad_climate[, summary_id := paste0(summary_id, "-bad-units")]
    bad_climate[variable_id == "tas", units := "bad_unit"]
    bad_climate[,
        summary_row_id := morpher__hash_rows(
            summary_id,
            plan_id,
            variable_id,
            period,
            month,
            stat
        )
    ]
    morpher__replace_rows(
        store,
        "epw_climate_summary",
        bad_climate,
        "summary_row_id"
    )
    bad_strict <- morpher$preview_plan(
        summary_id = unique(bad_climate$summary_id),
        baseline_id = baseline_id,
        strict = TRUE
    )
    expect_equal(bad_strict$plan$status, "blocked")
    expect_true(any(bad_strict$diagnostics$code == "unit_conversion_failed"))
    bad_relaxed <- morpher$preview_plan(
        summary_id = unique(bad_climate$summary_id),
        baseline_id = baseline_id,
        strict = FALSE
    )
    expect_equal(bad_relaxed$plan$status, "planned")
    expect_true(any(bad_relaxed$diagnostics$severity == "warning"))

    strict <- morpher$plan(
        summary_id = summary_id,
        baseline_id = baseline_id,
        strict = TRUE
    )
    expect_equal(strict$status, "planned")
    expect_equal(nrow(morpher$diagnose(strict$morph_id)), 0L)
    expect_silent(morpher$check(strict$morph_id))

    results <- morpher$run(strict$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))
    expect_equal(morpher$status(strict$morph_id)$status, "result_done")

    result_data <- read_test_parquet(result_path)
    baseline_data <- data.table::as.data.table(epw_file_read(get_cache_epw())$data())
    expected_fields <- c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature",
        "atmospheric_pressure",
        "global_horizontal_radiation",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "wind_speed",
        "total_sky_cover",
        "opaque_sky_cover",
        "liquid_precip_depth",
        "liquid_precip_rate"
    )
    expect_true(all(expected_fields %in% names(result_data)))
    for (field in c(
        "dry_bulb_temperature",
        "dew_point_temperature",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "liquid_precip_depth"
    )) {
        expect_true(
            any(
                abs(result_data[[field]] - baseline_data[[field]]) > 1e-6,
                na.rm = TRUE
            ),
            info = field
        )
    }
    expect_setequal(unique(result_data$liquid_precip_rate), c(0, 1))

    resumed_results <- testthat::with_mocked_bindings(
        morpher$run(strict$morph_id, overwrite = FALSE, resume = TRUE),
        morpher__run_context = function(...) {
            stop("recomputed a complete morphing case")
        },
        .package = "epwshiftr"
    )
    expect_equal(resumed_results$result_id, results$result_id)

    override_morpher <- epw_morpher_test__from_recipe(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute",
            methods = c(tdb = "shift", rh = "shift")
        )),
        label = "singapore"
    )
    override_baseline <- override_morpher$summarise_baseline()
    override <- override_morpher$plan(
        summary_id = summary_id,
        baseline_id = unique(override_baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(override$status, "planned")
    expect_false(identical(override$morph_id, strict$morph_id))
    override_results <- override_morpher$run(
        override$morph_id,
        overwrite = TRUE
    )
    override_data <- read_test_parquet(store_abs_path(
        override_results$output_path,
        root = store$path
    ))
    # Enhanced auto temperature intentionally degrades to shift when optional
    # tasmax/tasmin are absent, so this explicit override is numerically equal.
    expect_equal(
        override_data$dry_bulb_temperature,
        result_data$dry_bulb_temperature
    )
    expect_true(any(
        abs(override_data$relative_humidity - result_data$relative_humidity) >
            1e-6,
        na.rm = TRUE
    ))

    change_morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        transform = monthly_transform("epwshiftr"),
        label = "singapore-change"
    )
    change_baseline_id <- unique(
        change_morpher$summarise_baseline()$baseline_id
    )
    change_missing_ref <- change_morpher$preview_plan(
        summary_id = summary_id,
        baseline_id = change_baseline_id,
        strict = TRUE
    )
    expect_equal(change_missing_ref$plan$status, "planned")
    expect_false(any(
        change_missing_ref$diagnostics$code == "missing_reference_climate"
    ))
    expect_true(all(!is.na(change_missing_ref$factors$reference)))
    expect_equal(
        change_missing_ref$factors$reference,
        change_missing_ref$factors$baseline
    )
    baseline_reference <- change_morpher$plan(
        summary_id = summary_id,
        baseline_id = change_baseline_id,
        strict = TRUE
    )
    baseline_reference_results <- change_morpher$run(
        baseline_reference$morph_id,
        overwrite = TRUE
    )
    baseline_reference_data <- read_test_parquet(store_abs_path(
        baseline_reference_results$output_path,
        root = store$path
    ))
    expect_true(any(
        abs(
            baseline_reference_data$dry_bulb_temperature -
                baseline_data$dry_bulb_temperature
        ) >
            1e-6,
        na.rm = TRUE
    ))
    change <- change_morpher$plan(
        summary_id = summary_id,
        reference_summary_id = summary_id,
        baseline_id = change_baseline_id,
        strict = TRUE
    )
    expect_equal(change$status, "planned")
    change_factors <- store$query(sprintf(
        "SELECT * FROM epw_morph_factor WHERE morph_id = '%s'",
        change$morph_id
    ))
    expect_true("reference" %in% names(change_factors))
    expect_true(any(!is.na(change_factors$reference)))
    change_results <- change_morpher$run(change$morph_id, overwrite = TRUE)
    change_data <- read_test_parquet(store_abs_path(
        change_results$output_path,
        root = store$path
    ))
    expect_lt(
        max(
            abs(
                change_data$dry_bulb_temperature -
                    baseline_data$dry_bulb_temperature
            ),
            na.rm = TRUE
        ),
        1e-6
    )

    outputs <- morpher$write_epw(
        morph_id = strict$morph_id,
        dir = "outputs/future-epw-strict",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(epw_file_read(output_path), "EpwFile"))
    expect_equal(morpher$status(strict$morph_id)$status, "epw_written")
    expect_equal(nrow(morpher$outputs(strict$morph_id)), 1L)

    resumed_outputs <- morpher$write_epw(
        morph_id = strict$morph_id,
        dir = "outputs/future-epw-strict",
        separate = FALSE,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_equal(resumed_outputs$output_id, outputs$output_id)

    workflow_fx <- morpher_test__extracted_store(
        variables = variables,
        frequency = "mon",
        table_id = "Amon",
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    workflow_morpher <- epw_morpher_test__from_recipe(
        store = workflow_fx$store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        )),
        label = "singapore"
    )
    workflow_fields <- c(
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
    workflow_no_epw <- workflow_morpher$workflow(
        plan_id = workflow_fx$plan$plan_id,
        periods = periods,
        strict = TRUE,
        dir = NULL,
        overwrite = TRUE
    )
    expect_named(workflow_no_epw, workflow_fields)
    expect_null(workflow_no_epw$outputs)
    expect_equal(
        workflow_morpher$status(workflow_no_epw$plan$morph_id)$status,
        "result_done"
    )

    workflow <- workflow_morpher$workflow(
        plan_id = workflow_fx$plan$plan_id,
        periods = periods,
        strict = TRUE,
        dir = "outputs/workflow-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_named(workflow, workflow_fields)
    expect_equal(workflow$plan$status, "planned")
    expect_equal(
        workflow_morpher$status(workflow$plan$morph_id)$status,
        "epw_written"
    )
    expect_equal(nrow(workflow$outputs), 1L)
})

# A controlled threshold makes multiple backend components report identical
# fallback warnings. The public workflow must persist them and still write EPW.
test_that("repeated runtime warnings do not block EPW output or resume", {
    skip_on_cran()
    local_test_cache()
    withr::local_options(epwshiftr.threshold_alpha = 0)
    variables <- unique(c(
        epw_morph_variables(transform__recipe(monthly_transform("epwshiftr"))),
        "tasmin",
        "tasmax"
    ))
    fx <- morpher_test__extracted_store(
        variables = variables,
        frequency = "mon",
        table_id = "Amon",
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z")
    )
    store <- fx$store
    plan <- fx$plan
    morpher <- epw_morpher(
        store,
        epw = get_cache_epw(),
        site_id = "SIN",
        transform = monthly_transform("epwshiftr")
    )
    # Repeat the same public workflow to check durable diagnostics on reuse.
    run <- function() {
        suppressWarnings(morpher$workflow(
            plan_id = plan$plan_id,
            periods = epw_morph_periods(future = 2060L),
            strict = TRUE,
            dir = "outputs"
        ))
    }
    result <- run()
    expect_equal(nrow(result$outputs), 1L)
    expect_true(file.exists(store_abs_path(result$outputs$path, store$path)))
    diagnostics <- morpher__read_table(store, "epw_morph_diagnostic")
    expect_identical(anyDuplicated(diagnostics$diagnostic_id), 0L)
    fallback <- diagnostics[
        diagnostics$code == "fallback_shift_alpha_threshold"
    ]
    expect_gt(nrow(fallback), 1L)
    expect_gt(data.table::uniqueN(fallback$month), 1L)
    expect_true(all(fallback$severity == "warning"))
    expect_true(all(fallback$morph_id == result$plan$morph_id))
    resumed <- run()
    expect_identical(resumed$outputs$output_id, result$outputs$output_id)
    expect_equal(
        morpher__read_table(store, "epw_morph_diagnostic"),
        diagnostics
    )
    # Read actual backend parameters through the public interface. The preview
    # still requests auto, while this controlled execution applies only shift.
    process <- morpher$process_data(result$plan$morph_id)
    expect_named(process, result$results$case_id)
    actual <- process[[1L]]
    expect_identical(actual$morph_id, result$plan$morph_id)
    expect_equal(actual$results$result_id, result$results$result_id)
    expect_identical(actual$runtime_options$epwshiftr.threshold_alpha, 0)
    expect_equal(nrow(actual$parts$tdb), 8760L)
    expect_gt(nrow(actual$factors), 0L)
    expect_true(all(actual$parts$tdb$method_applied == "shift"))
    expect_true(all(actual$parts$tdb$alpha == 0))
    expect_true(any(
        actual$diagnostics$code == "fallback_shift_alpha_threshold"
    ))
    expect_equal(nrow(actual$diagnostics), nrow(diagnostics))
    expect_true(any(result$preview$factors$method == "auto"))
    baseline <- epw_file_read(get_cache_epw())$data()
    # Match calendar positions explicitly, rather than relying on row order.
    calendar_key <- function(data) {
        paste(data$month, data$day, data$hour, data$minute)
    }
    index <- match(calendar_key(actual$parts$tdb), calendar_key(baseline))
    expect_false(anyNA(index))
    expect_equal(
        as.numeric(actual$parts$tdb$dry_bulb_temperature),
        as.numeric(baseline$dry_bulb_temperature[index]) +
            actual$parts$tdb$delta,
        tolerance = 1e-10
    )
    expect_error(
        morpher$process_data(result$plan$morph_id, case_id = "absent"),
        "No saved results"
    )
    store$close()
    store <- EsgStore$new(file.path(fx$root, "store"))
    withr::defer(store$close())
    morpher <- epw_morpher(
        store,
        get_cache_epw(),
        site_id = "SIN",
        transform = monthly_transform("epwshiftr")
    )
    reopened <- morpher$process_data(result$plan$morph_id)[[1L]]
    expect_equal(reopened$parts, actual$parts)
    expect_equal(reopened$factors, actual$factors)

    # A damaged payload must not be returned or silently accepted on resume.
    reference <- jsonlite::fromJSON(result$results$provenance_json)$process_data
    path <- store_abs_path(reference$path, store$path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    writeBin(charToRaw("damaged process data"), path)
    expect_error(
        morpher$process_data(result$plan$morph_id),
        "missing or damaged"
    )
    expect_error(morpher$run(result$plan$morph_id), "missing or damaged")
    writeBin(bytes, path)
    expect_equal(
        morpher$process_data(result$plan$morph_id)[[1L]]$factors,
        actual$factors
    )
    writeBin(charToRaw("damaged again"), path)
    repaired <- morpher$run(result$plan$morph_id, overwrite = TRUE)
    expect_equal(nrow(repaired), 1L)
    expect_equal(
        morpher$process_data(result$plan$morph_id)[[1L]]$parts,
        actual$parts
    )
})

test_that("morpher calendar columns prefer CF identity with legacy fallback", {
    climate <- data.table::data.table(
        time = as.POSIXct(
            c("2060-12-25 12:00:00", "2060-12-26 12:00:00"),
            tz = "UTC"
        ),
        year = c(2060L, 2062L),
        cf_year = c(2061L, NA_integer_),
        cf_month = c(1L, NA_integer_),
        cf_day = c(1L, NA_integer_)
    )

    resolved <- morpher__resolve_calendar_columns(
        climate,
        month = TRUE,
        day = TRUE
    )

    expect_identical(resolved$year, c(2061L, 2062L))
    expect_identical(resolved$month, c(1L, 12L))
    expect_identical(resolved$day, c(1L, 26L))

    legacy <- data.table::data.table(
        time = as.POSIXct("2061-02-03 12:00:00", tz = "UTC")
    )
    legacy <- morpher__resolve_calendar_columns(
        legacy,
        month = TRUE,
        day = TRUE
    )
    expect_identical(legacy$year, 2061L)
    expect_identical(legacy$month, 2L)
    expect_identical(legacy$day, 3L)
})

# Create daily inputs with an exactly representable reference offset so two
# calibration plans in one store must produce different, predictable weather.
# morph_isolation_test__plan {{{
morph_isolation_test__plan <- function(store, root, name, year, offset = 0) {
    path <- file.path(root, paste0(name, ".nc"))
    write_local_cmip6_netcdf_fixture(path, year)
    if (offset != 0) {
        nc <- RNetCDF::open.nc(path, write = TRUE)
        values <- RNetCDF::var.get.nc(nc, "tas")
        RNetCDF::var.put.nc(nc, "tas", values + offset)
        RNetCDF::close.nc(nc)
    }
    start <- sprintf("%d-01-01T00:00:00Z", year)
    end <- sprintf("%d-12-31T23:59:59Z", year)
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
        site_id = "SIN",
        time = c(start, end),
        variable_id = "tas"
    )
    result <- store$extract(plan_id = plan$plan_id)
    stopifnot(all(result$status == "done"))
    unique(plan$plan_id)
}
# }}}

test_that("observed references and output status remain isolated between morph plans", {
    skip_on_cran()
    root <- tempfile("morph-isolation-")
    dir.create(root)
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close())
    historical <- morph_isolation_test__plan(store, root, "historical", 2001L)
    future <- morph_isolation_test__plan(store, root, "future", 2061L)
    warm <- morph_isolation_test__plan(store, root, "observed-warm", 2002L, 12)
    cool <- morph_isolation_test__plan(store, root, "observed-cool", 2002L)
    morpher <- epw_morpher(
        store,
        get_cache_epw(),
        site_id = "SIN",
        transform = daily_transform("qdm")
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
            dir = file.path("outputs", label)
        )
    }
    first <- run(warm, "warm")
    first_data <- morpher$process_data(first$plan$morph_id)[[1L]]
    second <- run(cool, "cool")
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
    cases <- morpher__read_table(store, "epw_morph_case")
    select_first <- which(cases$morph_id == first$plan$morph_id)
    data.table::set(cases, i = select_first, j = "status", value = "failed")
    morpher__replace_rows(store, "epw_morph_case", cases, "case_id")
    morpher$write_epw(
        second$plan$morph_id,
        dir = "outputs/cool",
        overwrite = TRUE
    )
    expect_identical(morpher$status(second$plan$morph_id)$status, "epw_written")
})

test_that("case diagnostic replacement cannot delete another plan or case", {
    store <- EsgStore$new(tempfile("diagnostic-isolation-"))
    withr::defer(store$close())
    rows <- data.table::rbindlist(list(
        morpher__diagnostic(
            "runtime",
            "warning",
            "a",
            "first",
            morph_id = "m1",
            case_id = "c1"
        ),
        morpher__diagnostic(
            "runtime",
            "warning",
            "b",
            "second",
            morph_id = "m1",
            case_id = "c2"
        ),
        morpher__diagnostic(
            "runtime",
            "warning",
            "c",
            "third",
            morph_id = "m2",
            case_id = "c1"
        )
    ))
    data.table::set(rows, j = "diagnostic_id", value = c("d1", "d2", "d3"))
    morpher__replace_rows(store, "epw_morph_diagnostic", rows, "diagnostic_id")
    morpher__delete_case_diagnostics(store, "m1", "c1")
    kept <- morpher__read_table(store, "epw_morph_diagnostic")
    expect_setequal(kept$diagnostic_id, c("d2", "d3"))
    morpher__delete_case_diagnostics(store, "missing", "c1")
    expect_equal(morpher__read_table(store, "epw_morph_diagnostic"), kept)
})

test_that("calibrated recipes invalidate earlier shared-store execution plans", {
    # Statistical method equations are unchanged; the complete recipe identity
    # changes because earlier executions could select another plan's inputs.
    keys <- c(
        "quantile_mapping_morphing_daily",
        "hourly_kernel_qdm",
        names(recipe__daily_adjustment_specs())
    )
    for (key in keys) {
        expect_identical(recipe__get(key)@version, 2L)
        expect_error(recipe__get(key, version = 1L), "persisted version")
    }
})

# vim: fdm=marker :
