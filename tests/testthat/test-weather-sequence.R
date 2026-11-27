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
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        ))
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
    expect_identical(
        records[[1L]]$provenance,
        list(
            method = "synthetic_sequence",
            source = "synthetic"
        )
    )
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

    complete <- shift_run__complete_output_cases(cases, outputs)
    incomplete <- shift_run__complete_output_cases(cases, outputs[1L])

    expect_identical(complete$status, "completed")
    expect_identical(complete$output_id, "output-2061")
    expect_identical(incomplete$status, "missing")
    expect_match(incomplete$missing_reason, "sequence is incomplete")
})

test_that("EpwMorpher persists, resumes, and writes every sequence year", {
    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2061L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-sequence-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- esgf_test__file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc,
        datetime_start = "2061-01-01T00:00:00Z",
        datetime_end = "2061-12-31T23:59:59Z"
    )
    query_id <- store$add_files(esgf_test__file_result(docs))
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
                dry_bulb_temperature = dry_bulb_temperature +
                    target_year -
                    2060L
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
        label = "sequence-test"
    )
    private <- priv(morpher)
    private$transform <- NULL
    private$recipe <- epw_morph_recipe(backend_name)
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
    # Both years must retain one shared execution payload after partial repair.
    process <- morpher$process_data(morph_plan$morph_id)[[1L]]
    expect_identical(process$results$weather_year, c(2061L, 2062L))
    expect_identical(process$provenance$backend, backend_name)
    expect_null(process$factors)

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
        unname(vapply(
            output_paths,
            function(path) {
                unique(epw_file_read(path)$data()$year)
            },
            integer(1L)
        )),
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

# Build one complete calendar-native year whose values expose any accidental
# change from source chronology during sequence partitioning.
# direct_sequence_test__year {{{
direct_sequence_test__year <- function(
    variable,
    year,
    calendar = "noleap",
    reverse = FALSE
) {
    year_days <- cf_time__year_days(year, calendar)[[1L]]
    fields <- cf_time_offset2date(
        seq.int(0L, year_days - 1L),
        data.frame(year = year, month = 1L, day = 1L),
        calendar
    )
    fields$hour <- 12L
    fields$minute <- 0L
    fields$second <- 0
    data <- data.frame(
        variable_id = rep.int(variable, year_days),
        value = seq_len(year_days),
        units = rep.int(
            if (identical(variable, "pr")) "kg m-2 s-1" else "K",
            year_days
        ),
        frequency = rep.int("day", year_days),
        cf_time__coordinates(fields, calendar),
        stringsAsFactors = FALSE
    )
    if (isTRUE(reverse)) {
        data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
    }
    data
}
# }}}

# direct_sequence_test__adjusted {{{
direct_sequence_test__adjusted <- function(
    variable,
    years,
    calendar = "noleap",
    reverse = FALSE,
    output_role = "model_future"
) {
    rows <- lapply(years, function(y) {
        direct_sequence_test__year(variable, y, calendar, reverse)
    })
    bias__daily_adjusted_series(
        do.call(rbind, rows),
        output_role = output_role,
        transformation = "test_adjustment",
        settings = list(window_days = 31L),
        provenance = list(method = "test_adjustment")
    )
}
# }}}

# direct_sequence_test__subdaily_year {{{
direct_sequence_test__subdaily_year <- function(
    variable,
    year,
    calendar = "noleap",
    frequency = "3hr",
    time_step_seconds = 10800,
    reverse = FALSE
) {
    year_days <- cf_time__year_days(year, calendar)[[1L]]
    offsets <- seq.int(
        0,
        year_days * 86400 - time_step_seconds,
        by = time_step_seconds
    )
    seconds <- offsets %% 86400
    fields <- cf_time_offset2date(
        offsets %/% 86400,
        data.frame(year = year, month = 1L, day = 1L),
        calendar
    )
    fields$hour <- seconds %/% 3600
    fields$minute <- (seconds %% 3600) %/% 60
    fields$second <- seconds %% 60
    data <- data.frame(
        variable_id = rep.int(variable, length(offsets)),
        value = seq_along(offsets),
        units = rep.int("K", length(offsets)),
        frequency = rep.int(frequency, length(offsets)),
        cf_time__coordinates(fields, calendar),
        cf_second_of_day = as.numeric(seconds),
        source_time_offset = as.numeric(offsets),
        stringsAsFactors = FALSE
    )
    if (isTRUE(reverse)) {
        data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
    }
    data
}
# }}}

# direct_sequence_test__subdaily_adjusted {{{
direct_sequence_test__subdaily_adjusted <- function(
    variable,
    years,
    calendar = "noleap",
    frequency = "3hr",
    time_step_seconds = 10800,
    reverse = FALSE
) {
    rows <- lapply(years, function(y) {
        direct_sequence_test__subdaily_year(
            variable,
            y,
            calendar,
            frequency,
            time_step_seconds,
            reverse
        )
    })
    bias__subdaily_adjusted_series(
        do.call(rbind, rows),
        frequency = frequency,
        time_step_seconds = time_step_seconds,
        output_role = "model_future",
        transformation = "test_adjustment",
        settings = list(window_months = 3L),
        provenance = list(method = "test_adjustment")
    )
}
# }}}

# direct_sequence_test__execution {{{
direct_sequence_test__execution <- function(
    values,
    keys = rep(list(list(site = "A")), length(values)),
    statuses = rep.int("ok", length(values))
) {
    groups <- lapply(seq_along(values), function(i) {
        adjusted <- values[[i]]
        signal__group(
            key = keys[[i]],
            inputs = list(model_future = adjusted@data),
            variables = unique(adjusted@data[["variable_id"]])
        )
    })
    variables <- unique(unlist(
        lapply(groups, function(g) g@variables),
        use.names = FALSE
    ))
    SignalExecutionResult(
        groups = groups,
        values = values,
        profiles = stats::setNames(
            lapply(variables, function(v) list(variable_id = v)),
            variables
        ),
        diagnostics = data.frame(
            method = rep.int("test_signal", length(groups)),
            group = vapply(
                seq_along(groups),
                function(i) signal__group_label(groups[[i]], i),
                character(1L)
            ),
            status = statuses,
            variables = vapply(
                groups,
                function(g) paste(g@variables, collapse = ","),
                character(1L)
            ),
            evidence = rep.int("published", length(groups)),
            message = rep.int(NA_character_, length(groups)),
            stringsAsFactors = FALSE
        )
    )
}
# }}}

# direct_sequence_test__gen {{{
direct_sequence_test__gen <- function(execution) {
    sequence__direct_model_generate(
        execution,
        inputs = NULL,
        context = NULL,
        options = list()
    )
}
# }}}

test_that("direct model sequence preserves future chronology across daily, sub-daily, and CF calendars", {
    execution <- direct_sequence_test__execution(list(
        direct_sequence_test__adjusted("tas", 2061:2062, reverse = TRUE),
        direct_sequence_test__adjusted("pr", 2061:2062)
    ))
    first <- direct_sequence_test__gen(execution)
    second <- direct_sequence_test__gen(execution)

    expect_s7_class(first, DirectModelSequence)
    expect_identical(first@frequency, "day")
    expect_identical(
        vapply(first@members, function(m) m@weather_year, integer(1L)),
        2061:2062
    )
    expect_identical(
        vapply(first@members, function(m) m@sequence_id, character(1L)),
        rep.int(DIRECT_MODEL_SEQUENCE_ID, 2L)
    )
    expect_identical(first@provenance$group_ids, second@provenance$group_ids)
    expect_identical(first@provenance$selection, "none")
    expect_identical(first@provenance$resampling, "none")

    for (member in first@members) {
        expect_identical(member@calendar, "noleap")
        expect_length(member@series, 2L)
        for (item in member@series) {
            expect_identical(item@adjusted@data$cf_day_of_year, seq_len(365L))
            expect_identical(item@adjusted@provenance$method, "test_adjustment")
        }
    }
    expect_error(
        DirectModelSequence(
            members = list(first@members[[1L]], first@members[[1L]]),
            frequency = first@frequency,
            time_step_seconds = first@time_step_seconds,
            provenance = first@provenance
        ),
        "unique ascending weather years"
    )

    # Every supported native CF calendar
    for (calendar in CF_TIME_CALENDARS) {
        seq_cal <- direct_sequence_test__gen(
            direct_sequence_test__execution(list(direct_sequence_test__adjusted(
                "tas",
                2000L,
                calendar
            )))
        )
        member <- seq_cal@members[[1L]]
        expect_identical(member@calendar, calendar, info = calendar)
        expect_identical(
            nrow(member@series[[1L]]@adjusted@data),
            cf_time__year_days(2000L, calendar)[[1L]],
            info = calendar
        )
    }

    # Sub-daily model years
    sub_seq <- direct_sequence_test__gen(
        direct_sequence_test__execution(list(
            direct_sequence_test__subdaily_adjusted(
                "tas",
                2061:2062,
                reverse = TRUE
            )
        ))
    )
    expect_identical(sub_seq@frequency, "3hr")
    expect_identical(as.numeric(sub_seq@time_step_seconds), 10800)
    expect_identical(
        vapply(sub_seq@members, function(m) m@weather_year, integer(1L)),
        2061:2062
    )
    for (member in sub_seq@members) {
        data <- member@series[[1L]]@adjusted@data
        expect_s7_class(member@series[[1L]]@adjusted, SubdailyAdjustedSeries)
        expect_identical(
            data$cf_second_of_day[1:8],
            seq.int(0, 75600, by = 10800)
        )
        expect_identical(
            data$source_time_offset[1:8],
            seq.int(0, 75600, by = 10800)
        )
    }
})

test_that("direct model component is registered and contract-compatible", {
    sequence__register_direct_model_component()
    component <- component__get("sequence", "direct_model_realization")
    hourly <- component__spec(
        name = "direct_model_hourly_test",
        stage = "hourly",
        input_kinds = "direct_model_sequence",
        output_kinds = "hourly_weather",
        scopes = "multivariate",
        operations = list(reconstruct = identity)
    )

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@stage, "sequence")
    expect_identical(
        component@input_kinds,
        c(
            "daily_adjusted_series",
            "subdaily_adjusted_series",
            "adjusted_weather_series"
        )
    )
    expect_identical(component@output_kinds, "direct_model_sequence")
    expect_false(component@stochastic)
    expect_identical(component@metadata$selection, "none")

    expect_invisible(component__validate_inputs(
        component,
        weather__new_inputs(
            model_future = weather__new_input(
                "model_future",
                direct_sequence_test__year("tas", 2061L)
            )
        )
    ))
    expect_invisible(component__validate_inputs(
        component,
        weather__new_inputs(
            model_future = weather__new_input(
                "model_future",
                direct_sequence_test__subdaily_year("tas", 2061L)
            )
        )
    ))
    expect_true(component__compatible(qdm__component(), component))
    expect_true(component__compatible(component, hourly))
})

test_that("direct model sequence rejects incomplete, misaligned, duplicate, wrong-role, and failed groups", {
    tas_2061 <- direct_sequence_test__adjusted("tas", 2061L)
    incomplete_daily <- bias__daily_adjusted_series(
        direct_sequence_test__year("pr", 2061L)[-365L, ],
        "model_future",
        "test_adjustment"
    )
    incomplete_subdaily <- bias__subdaily_adjusted_series(
        direct_sequence_test__subdaily_year("tas", 2061L)[-1L, ],
        "3hr",
        10800,
        "model_future",
        "test_adjustment"
    )
    failed <- direct_sequence_test__execution(
        list(tas_2061),
        statuses = "error"
    )
    failed@values[1L] <- list(NULL)

    bad_cases <- list(
        list(
            exec = direct_sequence_test__execution(list(incomplete_daily)),
            pat = "cover every native-calendar day"
        ),
        list(
            exec = direct_sequence_test__execution(list(
                direct_sequence_test__adjusted("tas", 2061:2062),
                direct_sequence_test__adjusted("pr", 2061L)
            )),
            pat = "same weather years"
        ),
        list(
            exec = direct_sequence_test__execution(list(
                tas_2061,
                direct_sequence_test__adjusted("pr", 2061L, "360_day")
            )),
            pat = "same CF calendar"
        ),
        list(
            exec = direct_sequence_test__execution(list(incomplete_subdaily)),
            pat = "cover every declared sub-daily timestep"
        ),
        list(
            exec = direct_sequence_test__execution(list(
                tas_2061,
                direct_sequence_test__subdaily_adjusted("pr", 2061L)
            )),
            pat = "same frequency and timestep"
        ),
        list(
            exec = direct_sequence_test__execution(list(
                direct_sequence_test__adjusted(
                    "tas",
                    2061L,
                    output_role = "observed_reference"
                )
            )),
            pat = "retain `model_future`"
        ),
        list(
            exec = direct_sequence_test__execution(
                list(tas_2061, tas_2061),
                keys = list(list(site = "A"), list(site = "A"))
            ),
            pat = "duplicate signal-group identities"
        ),
        list(exec = failed, pat = "cannot preserve failed signal groups"),
        list(exec = "not-a-signal", pat = "aligned SignalExecutionResult")
    )
    for (case in bad_cases) {
        expect_error(direct_sequence_test__gen(case$exec), case$pat)
    }
})

# vim: fdm=marker :
