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

direct_sequence_test__gen <- function(execution) {
    sequence__direct_model_generate(
        execution,
        inputs = NULL,
        context = NULL,
        options = list()
    )
}

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
        direct_sequence_test__execution(list(direct_sequence_test__subdaily_adjusted(
            "tas",
            2061:2062,
            reverse = TRUE
        )))
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
            exec = direct_sequence_test__execution(list(direct_sequence_test__adjusted(
                "tas",
                2061L,
                output_role = "observed_reference"
            ))),
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
