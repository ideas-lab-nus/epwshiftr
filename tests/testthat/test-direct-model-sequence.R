# Build one complete calendar-native year whose values expose any accidental
# change from source chronology during sequence partitioning.
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
        units = rep.int(if (identical(variable, "pr")) {
            "kg m-2 s-1"
        } else {
            "K"
        }, year_days),
        frequency = rep.int("day", year_days),
        cf_time__coordinates(fields, calendar),
        stringsAsFactors = FALSE
    )
    if (isTRUE(reverse)) {
        data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
    }
    data
}

# Construct one future-backbone adjusted series spanning the requested model
# years while retaining a visible upstream transformation record.
direct_sequence_test__adjusted <- function(
    variable,
    years,
    calendar = "noleap",
    reverse = FALSE,
    output_role = "model_future"
) {
    rows <- lapply(years, function(year) {
        direct_sequence_test__year(
            variable,
            year,
            calendar,
            reverse
        )
    })
    bias__daily_adjusted_series(
        do.call(rbind, rows),
        output_role = output_role,
        transformation = "test_adjustment",
        settings = list(window_days = 31L),
        provenance = list(method = "test_adjustment")
    )
}

# Build one complete regular sub-daily year whose exact native-calendar times
# make missing, duplicated, or reordered samples observable.
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
        from = 0,
        to = year_days * 86400 - time_step_seconds,
        by = time_step_seconds
    )
    day_offsets <- offsets %/% 86400
    seconds <- offsets %% 86400
    fields <- cf_time_offset2date(
        day_offsets,
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

# Construct a typed future-backbone sub-daily series spanning complete source
# years while preserving an upstream provenance record.
direct_sequence_test__subdaily_adjusted <- function(
    variable,
    years,
    calendar = "noleap",
    frequency = "3hr",
    time_step_seconds = 10800,
    reverse = FALSE
) {
    rows <- lapply(years, function(year) {
        direct_sequence_test__subdaily_year(
            variable,
            year,
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

# Assemble the canonical signal envelope consumed by every sequence component
# without invoking a particular bias-adjustment kernel in these focused tests.
direct_sequence_test__execution <- function(
    values,
    keys = rep(list(list(site = "A")), length(values)),
    statuses = rep.int("ok", length(values))
) {
    groups <- lapply(seq_along(values), function(index) {
        adjusted <- values[[index]]
        signal__group(
            key = keys[[index]],
            inputs = list(model_future = adjusted@data),
            variables = unique(adjusted@data[["variable_id"]])
        )
    })
    variables <- unique(unlist(
        lapply(groups, function(group) group@variables),
        use.names = FALSE
    ))
    SignalExecutionResult(
        groups = groups,
        values = values,
        profiles = stats::setNames(
            lapply(variables, function(variable) {
                list(variable_id = variable)
            }),
            variables
        ),
        diagnostics = data.frame(
            method = rep.int("test_signal", length(groups)),
            group = vapply(
                seq_along(groups),
                function(index) {
                    signal__group_label(groups[[index]], index)
                },
                character(1L)
            ),
            status = statuses,
            variables = vapply(
                groups,
                function(group) paste(group@variables, collapse = ","),
                character(1L)
            ),
            evidence = rep.int("published", length(groups)),
            message = rep.int(NA_character_, length(groups)),
            stringsAsFactors = FALSE
        )
    )
}

test_that("direct model sequence preserves and partitions future chronology", {
    execution <- direct_sequence_test__execution(list(
        direct_sequence_test__adjusted(
            "tas",
            2061:2062,
            reverse = TRUE
        ),
        direct_sequence_test__adjusted("pr", 2061:2062)
    ))

    first <- sequence__direct_model_generate(
        execution,
        inputs = NULL,
        context = NULL,
        options = list()
    )
    second <- sequence__direct_model_generate(
        execution,
        inputs = NULL,
        context = NULL,
        options = list()
    )

    expect_s7_class(first, DirectModelSequence)
    expect_identical(first@frequency, "day")
    expect_identical(
        vapply(
            first@members,
            function(member) member@weather_year,
            integer(1L)
        ),
        2061:2062
    )
    expect_identical(
        vapply(
            first@members,
            function(member) member@sequence_id,
            character(1L)
        ),
        rep.int(DIRECT_MODEL_SEQUENCE_ID, 2L)
    )
    expect_identical(
        first@provenance$group_ids,
        second@provenance$group_ids
    )
    expect_identical(first@provenance$selection, "none")
    expect_identical(first@provenance$resampling, "none")

    for (member in first@members) {
        expect_identical(member@calendar, "noleap")
        expect_length(member@series, 2L)
        for (item in member@series) {
            expect_identical(
                item@adjusted@data$cf_day_of_year,
                seq_len(365L)
            )
            expect_identical(
                item@adjusted@provenance$method,
                "test_adjustment"
            )
        }
    }
})

test_that("direct model sequence accepts every supported native CF calendar", {
    for (calendar in CF_TIME_CALENDARS) {
        execution <- direct_sequence_test__execution(list(
            direct_sequence_test__adjusted(
                "tas",
                2000L,
                calendar
            )
        ))
        sequence <- sequence__direct_model_generate(
            execution,
            NULL,
            NULL,
            list()
        )
        member <- sequence@members[[1L]]

        expect_identical(member@calendar, calendar, info = calendar)
        expect_identical(
            nrow(member@series[[1L]]@adjusted@data),
            cf_time__year_days(2000L, calendar)[[1L]],
            info = calendar
        )
    }
})

test_that("direct model sequence preserves complete sub-daily model years", {
    adjusted <- direct_sequence_test__subdaily_adjusted(
        "tas",
        2061:2062,
        reverse = TRUE
    )
    sequence <- sequence__direct_model_generate(
        direct_sequence_test__execution(list(adjusted)),
        NULL,
        NULL,
        list()
    )

    expect_s7_class(sequence, DirectModelSequence)
    expect_identical(sequence@frequency, "3hr")
    expect_identical(as.numeric(sequence@time_step_seconds), 10800)
    expect_identical(sequence@provenance$frequency, "3hr")
    expect_identical(sequence@provenance$time_step_seconds, 10800)
    expect_identical(
        vapply(
            sequence@members,
            function(member) member@weather_year,
            integer(1L)
        ),
        2061:2062
    )
    for (member in sequence@members) {
        data <- member@series[[1L]]@adjusted@data
        expect_s7_class(
            member@series[[1L]]@adjusted,
            SubdailyAdjustedSeries
        )
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
    qdm <- qdm__component()
    hourly <- component__spec(
        name = "direct_model_hourly_test",
        stage = "hourly",
        input_kinds = "direct_model_sequence",
        output_kinds = "hourly_weather",
        scopes = "multivariate",
        operations = list(reconstruct = identity)
    )
    source <- direct_sequence_test__year("tas", 2061L)
    inputs <- weather__new_inputs(
        model_future = weather__new_input("model_future", source)
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
    expect_invisible(component__validate_inputs(component, inputs))

    subdaily_source <- direct_sequence_test__subdaily_year("tas", 2061L)
    subdaily_inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            subdaily_source
        )
    )
    expect_invisible(
        component__validate_inputs(component, subdaily_inputs)
    )
    expect_true(component__compatible(qdm, component))
    expect_true(component__compatible(component, hourly))
})

test_that("direct model sequence rejects incomplete or misaligned groups", {
    complete <- direct_sequence_test__adjusted("tas", 2061:2062)
    incomplete_data <- direct_sequence_test__year("pr", 2061L)[-365L, ]
    incomplete <- bias__daily_adjusted_series(
        incomplete_data,
        "model_future",
        "test_adjustment"
    )
    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(incomplete)),
            NULL,
            NULL,
            list()
        ),
        "cover every native-calendar day"
    )

    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(
                complete,
                direct_sequence_test__adjusted("pr", 2061L)
            )),
            NULL,
            NULL,
            list()
        ),
        "same weather years"
    )

    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(
                direct_sequence_test__adjusted("tas", 2061L),
                direct_sequence_test__adjusted(
                    "pr",
                    2061L,
                    "360_day"
                )
            )),
            NULL,
            NULL,
            list()
        ),
        "same CF calendar"
    )

    incomplete_subdaily_data <- direct_sequence_test__subdaily_year(
        "tas",
        2061L
    )[-1L, ]
    incomplete_subdaily <- bias__subdaily_adjusted_series(
        incomplete_subdaily_data,
        "3hr",
        10800,
        "model_future",
        "test_adjustment"
    )
    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(incomplete_subdaily)),
            NULL,
            NULL,
            list()
        ),
        "cover every declared sub-daily timestep"
    )

    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(
                direct_sequence_test__adjusted("tas", 2061L),
                direct_sequence_test__subdaily_adjusted("pr", 2061L)
            )),
            NULL,
            NULL,
            list()
        ),
        "same frequency and timestep"
    )
})

test_that("direct model sequence rejects wrong roles, duplicates, and failures", {
    observed <- direct_sequence_test__adjusted(
        "tas",
        2061L,
        output_role = "observed_reference"
    )
    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(list(observed)),
            NULL,
            NULL,
            list()
        ),
        "retain `model_future`"
    )

    adjusted <- direct_sequence_test__adjusted("tas", 2061L)
    expect_error(
        sequence__direct_model_generate(
            direct_sequence_test__execution(
                list(adjusted, adjusted),
                keys = list(list(site = "A"), list(site = "A"))
            ),
            NULL,
            NULL,
            list()
        ),
        "duplicate signal-group identities"
    )

    failed <- direct_sequence_test__execution(
        list(adjusted),
        statuses = "error"
    )
    failed@values[1L] <- list(NULL)
    expect_error(
        sequence__direct_model_generate(failed, NULL, NULL, list()),
        "cannot preserve failed signal groups"
    )
    expect_error(
        sequence__direct_model_generate("not-a-signal", NULL, NULL, list()),
        "aligned SignalExecutionResult"
    )
})
