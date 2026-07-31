test_that("future-weather sequence contracts validate year-addressable members", {
    epw <- epw_file_read(get_cache_epw())
    weather_2061 <- epw$data()
    weather_2061[, year := 2061L]
    weather_2062 <- data.table::copy(weather_2061)
    weather_2062[, year := 2062L]

    first <- sequence__member(
        weather_2061,
        weather_year = 2061L,
        sequence_id = "realization-1",
        stochastic_seed = 41L,
        provenance = list(source = "synthetic")
    )
    second <- sequence__member(
        weather_2062,
        weather_year = 2062L,
        sequence_id = "realization-1",
        stochastic_seed = 42L
    )
    context <- morpher__context(
        epw = epw,
        climate = data.table::data.table(
            time = as.POSIXct("2061-01-01", tz = "UTC"),
            variable_id = "tas",
            period = "future",
            year = 2061L,
            lon = 104,
            lat = 1,
            units = "K",
            value = 300
        ),
        recipe = suppressWarnings(epw_morph_recipe("belcher_absolute"))
    )
    result <- sequence__result(
        context,
        list(first, second),
        provenance = list(method = "synthetic_sequence")
    )
    future_year <- sequence__result(
        context,
        list(first),
        output_type = "future_year"
    )
    records <- sequence__records(result)

    expect_s7_class(first, WeatherSequenceMember)
    expect_s7_class(result, WeatherSequenceResult)
    expect_identical(future_year@output_type, "future_year")
    expect_identical(format(first@data$datetime[[1L]], "%Y"), "2061")
    expect_identical(
        vapply(records, `[[`, integer(1L), "weather_year"),
        c(2061L, 2062L)
    )
    expect_identical(records[[1L]]$provenance, list(
        method = "synthetic_sequence",
        source = "synthetic"
    ))
    expect_error(
        sequence__member(weather_2061, weather_year = 2062L),
        "Every hourly row"
    )
    expect_error(
        sequence__result(context, list(first), output_type = "multi_year"),
        "at least two members"
    )
    expect_error(
        sequence__result(
            context,
            list(first, first),
            output_type = "multi_year"
        ),
        "must be unique"
    )
})

test_that("Shift case completion accepts only complete output sequences", {
    cases <- data.table::data.table(
        case_id = "case-1",
        source_id = "Model-A",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        period = "2060s",
        status = "ready",
        output_id = NA_character_,
        export_path = NA_character_,
        missing_reason = NA_character_
    )
    outputs <- data.table::data.table(
        output_id = c("output-2061", "output-2062"),
        source_id = "Model-A",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        period = "2060s",
        weather_year = 2061:2062,
        member_count = 2L
    )

    complete <- shift__complete_output_cases(cases, outputs)
    incomplete <- shift__complete_output_cases(cases, outputs[1L])

    expect_identical(complete$status, "completed")
    expect_identical(complete$output_id, "output-2061")
    expect_identical(incomplete$status, "missing")
    expect_match(incomplete$missing_reason, "sequence is incomplete")
})

test_that("EpwMorpher persists, resumes, and writes every sequence year", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2061L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-sequence-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- epw_morpher_test_file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(epw_morpher_test_result(docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2061-01-02T00:00:00Z", "2061-01-03T23:59:59Z"),
        site_id = "SIN"
    )
    expect_identical(store$extract(plan_id = plan$plan_id)$status, "done")

    backend_name <- paste0("testsequence", Sys.getpid())
    rules <- data.table::data.table(
        step = "dry",
        epw_field = "dry_bulb_temperature",
        variable_id = "tas",
        optional_variable_id = NA_character_,
        method = "offset",
        required = TRUE,
        derived = FALSE,
        method_choices = list("offset")
    )
    # The synthetic backend exercises only the sequence/output contract; its
    # two year members deliberately reuse one EPW template.
    runner <- function(context, backend) {
        members <- lapply(seq.int(2061L, 2062L), function(year) {
            target_year <- year
            weather <- context$epw$data()
            weather[, `:=`(
                year = target_year,
                dry_bulb_temperature =
                    dry_bulb_temperature + target_year - 2060L
            )]
            sequence__member(
                weather,
                weather_year = target_year,
                sequence_id = "realization-1",
                calendar = "noleap",
                stochastic_seed = target_year - 2000L,
                provenance = list(source_year = target_year)
            )
        })
        sequence__result(
            context,
            members,
            output_type = "multi_year",
            provenance = list(backend = backend$name)
        )
    }
    backend <- EpwMorphBackend$new(
        name = backend_name,
        methods = c(dry = "offset"),
        method_choices = "offset",
        rules = rules,
        runner = runner
    )
    epw_morph_register_backend(backend_name, backend, overwrite = TRUE)

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = epw_morph_recipe(backend_name),
        label = "sequence-test"
    )
    climate <- morpher$summarise_climate(
        plan$plan_id,
        epw_morph_periods(future = 2061L),
        strict = FALSE
    )
    baseline <- morpher$summarise_baseline()
    morph_plan <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = FALSE
    )

    results <- morpher$run(morph_plan$morph_id, overwrite = TRUE)
    result_paths <- vapply(
        results$output_path,
        store_abs_path,
        character(1L),
        root = store$path
    )
    expect_identical(nrow(results), 2L)
    expect_identical(results$output_type, rep("multi_year", 2L))
    expect_identical(results$sequence_id, rep("realization-1", 2L))
    expect_identical(results$weather_year, c(2061L, 2062L))
    expect_identical(results$member_count, rep(2L, 2L))
    expect_true(all(file.exists(result_paths)))
    expect_match(results$output_path, "sequence=realization-1")

    resumed <- morpher$run(
        morph_plan$morph_id,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_identical(resumed$result_id, results$result_id)

    # Losing one member must regenerate that member while preserving its
    # intact sibling and the complete two-row manifest.
    sibling_mtime <- file.info(result_paths[[1L]])$mtime
    unlink(result_paths[[2L]])
    repaired <- morpher$run(
        morph_plan$morph_id,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_identical(repaired$result_id, results$result_id)
    expect_true(all(file.exists(result_paths)))
    expect_identical(file.info(result_paths[[1L]])$mtime, sibling_mtime)

    outputs <- morpher$write_epw(
        morph_id = morph_plan$morph_id,
        dir = "outputs/future-sequence",
        separate = FALSE,
        overwrite = TRUE
    )
    output_paths <- vapply(
        outputs$path,
        store_abs_path,
        character(1L),
        root = store$path
    )
    expect_identical(nrow(outputs), 2L)
    expect_identical(outputs$weather_year, c(2061L, 2062L))
    expect_match(basename(output_paths), "realization-1\\.206[12]\\.epw$")
    expect_true(all(file.exists(output_paths)))
    expect_identical(
        unname(vapply(output_paths, function(path) {
            unique(epw_file_read(path)$data()$year)
        }, integer(1L))),
        c(2061L, 2062L)
    )

    resumed_outputs <- morpher$write_epw(
        morph_id = morph_plan$morph_id,
        dir = "outputs/future-sequence",
        separate = FALSE,
        overwrite = FALSE,
        resume = TRUE
    )
    expect_identical(resumed_outputs$output_id, outputs$output_id)
})
