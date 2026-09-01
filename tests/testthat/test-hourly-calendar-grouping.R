# Build one complete hourly native-calendar year with stable role and case
# metadata so grouping tests exercise the production adjusted-series contract.
hourly_calendar_test__series <- function(
    variable = "tas",
    year = 2001L,
    calendar = "noleap",
    site_id = "A",
    source_id = "example-model",
    experiment_id = "historical",
    variant_label = "r1i1p1f1",
    grid_label = "gn",
    period = "reference",
    value_offset = 0,
    units = switch(
        variable,
        tas = "K",
        hurs = "%",
        ps = "Pa",
        sfcWind = "m s-1",
        rsds = "W m-2",
        rsdsdiff = "W m-2",
        "1"
    )
) {
    year_days <- cf_time__year_days(as.integer(year), calendar)[[1L]]
    hour_index <- seq.int(0L, year_days * 24L - 1L)
    fields <- cf_time_offset2date(
        hour_index %/% 24L,
        data.frame(year = as.integer(year), month = 1L, day = 1L),
        calendar
    )
    fields$hour <- hour_index %% 24L
    fields$minute <- 0L
    fields$second <- 0
    phase <- (hour_index + 0.5) / (year_days * 24)
    data.frame(
        site_id = rep.int(site_id, length(hour_index)),
        source_id = rep.int(source_id, length(hour_index)),
        experiment_id = rep.int(experiment_id, length(hour_index)),
        variant_label = rep.int(variant_label, length(hour_index)),
        grid_label = rep.int(grid_label, length(hour_index)),
        period = rep.int(period, length(hour_index)),
        variable_id = rep.int(variable, length(hour_index)),
        value = value_offset + sin(2 * pi * phase) + phase,
        units = rep.int(units, length(hour_index)),
        frequency = rep.int("hour", length(hour_index)),
        cf_time__coordinates(fields, calendar),
        cf_second_of_day = as.numeric(fields$hour) * 3600,
        stringsAsFactors = FALSE
    )
}

# Assemble the three materialized roles required by hourly calendar grouping.
hourly_calendar_test__inputs <- function(
    observed = hourly_calendar_test__series(
        source_id = "station",
        experiment_id = "observed"
    ),
    historical = hourly_calendar_test__series(),
    future = hourly_calendar_test__series(
        year = 2061L,
        experiment_id = "ssp585",
        period = "2060s",
        value_offset = 2
    )
) {
    weather__new_inputs(
        observed_reference = weather__new_input(
            "observed_reference",
            observed
        ),
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input(
            "model_future",
            future
        )
    )
}

# Execute the calendar component directly while supplying the same input set
# through its stage-value and role-addressable operation boundaries.
hourly_calendar_test__apply <- function(inputs) {
    hourly_calendar__apply(
        data = inputs,
        inputs = inputs,
        context = NULL,
        options = list()
    )
}

test_that("hourly calendar grouping preserves role-native calendars", {
    observed <- hourly_calendar_test__series(
        calendar = "gregorian",
        source_id = "station",
        experiment_id = "observed"
    )
    historical <- hourly_calendar_test__series(calendar = "360_day")
    future <- hourly_calendar_test__series(
        year = 2061L,
        calendar = "noleap",
        experiment_id = "ssp585",
        period = "2060s",
        value_offset = 2
    )
    result <- hourly_calendar_test__apply(
        hourly_calendar_test__inputs(observed, historical, future)
    )
    group <- result@value[[1L]]

    expect_s7_class(result, WeatherStageResult)
    expect_identical(result@stage, "calendar")
    expect_identical(result@kind, "calendar_indexed_hourly_series")
    expect_length(result@value, 1L)
    expect_s7_class(group, SignalGroup)
    expect_identical(group@variables, "tas")
    expect_identical(
        vapply(
            group@inputs,
            function(value) unique(value$cf_calendar),
            character(1L)
        ),
        c(
            observed_reference = "gregorian",
            model_historical = "360_day",
            model_future = "noleap"
        )
    )
    expect_identical(
        vapply(group@inputs, nrow, integer(1L)),
        c(
            observed_reference = 365L * 24L,
            model_historical = 360L * 24L,
            model_future = 365L * 24L
        )
    )
    expect_identical(result@provenance$date_pairing, "none")
    expect_identical(
        result@provenance$calendar_policy,
        "preserve_role_native_cf_calendar"
    )
    expect_true(all(
        result@diagnostics$hourly_calendar_grouping$complete_years == 1L
    ))
})

test_that("hourly calendar grouping separates variables and sites", {
    role_series <- function(role, year, offset) {
        source <- if (identical(role, "observed")) {
            "station"
        } else {
            "example-model"
        }
        experiment <- if (identical(role, "future")) {
            "ssp585"
        } else {
            role
        }
        do.call(rbind, lapply(c("A", "B"), function(site) {
            rbind(
                hourly_calendar_test__series(
                    "tas",
                    year,
                    site_id = site,
                    source_id = source,
                    experiment_id = experiment,
                    value_offset = offset
                ),
                hourly_calendar_test__series(
                    "hurs",
                    year,
                    site_id = site,
                    source_id = source,
                    experiment_id = experiment,
                    value_offset = 50 + offset
                )
            )
        }))
    }
    result <- hourly_calendar_test__apply(hourly_calendar_test__inputs(
        role_series("observed", 2001L, -1),
        role_series("historical", 1991L, 0),
        role_series("future", 2061L, 2)
    ))

    expect_length(result@value, 4L)
    expect_identical(
        vapply(result@value, function(group) group@key$site_id, character(1L)),
        c("A", "A", "B", "B")
    )
    expect_identical(
        vapply(result@value, function(group) group@variables, character(1L)),
        c("hurs", "tas", "hurs", "tas")
    )
    expect_identical(
        nrow(result@diagnostics$hourly_calendar_grouping),
        12L
    )
})

test_that("hourly calendar grouping rejects incompatible role contents", {
    tas_observed <- hourly_calendar_test__series(
        source_id = "station",
        experiment_id = "observed"
    )
    model_historical <- rbind(
        hourly_calendar_test__series(),
        hourly_calendar_test__series(
            "hurs",
            value_offset = 50
        )
    )
    model_future <- rbind(
        hourly_calendar_test__series(
            year = 2061L,
            experiment_id = "ssp585",
            period = "2060s",
            value_offset = 2
        ),
        hourly_calendar_test__series(
            "hurs",
            year = 2061L,
            experiment_id = "ssp585",
            period = "2060s",
            value_offset = 52
        )
    )
    expect_error(
        hourly_calendar_test__apply(hourly_calendar_test__inputs(
            tas_observed,
            model_historical,
            model_future
        )),
        "identical variable sets"
    )

    mismatched_units <- hourly_calendar_test__series(
        year = 2061L,
        experiment_id = "ssp585",
        period = "2060s",
        value_offset = 2,
        units = "degC"
    )
    expect_error(
        hourly_calendar_test__apply(hourly_calendar_test__inputs(
            future = mismatched_units
        )),
        "identical units"
    )

    mismatched_model <- hourly_calendar_test__series(
        year = 2061L,
        source_id = "other-model",
        experiment_id = "ssp585",
        period = "2060s",
        value_offset = 2
    )
    expect_error(
        hourly_calendar_test__apply(hourly_calendar_test__inputs(
            future = mismatched_model
        )),
        "identities differ.*source_id"
    )
})

test_that("hourly calendar grouping rejects duplicates and incomplete years", {
    future <- hourly_calendar_test__series(
        year = 2061L,
        experiment_id = "ssp585",
        period = "2060s",
        value_offset = 2
    )
    duplicated <- rbind(future, future[1L, ])
    expect_error(
        hourly_calendar_test__apply(hourly_calendar_test__inputs(
            future = duplicated
        )),
        "unique variable-calendar-year-month-day-second keys"
    )

    incomplete <- future[-nrow(future), ]
    expect_error(
        hourly_calendar_test__apply(hourly_calendar_test__inputs(
            future = incomplete
        )),
        "incomplete native-calendar day"
    )
})

test_that("hourly calendar grouping feeds hourly kernel QDM", {
    inputs <- hourly_calendar_test__inputs(
        observed = hourly_calendar_test__series(
            source_id = "station",
            experiment_id = "observed",
            value_offset = -1
        ),
        historical = hourly_calendar_test__series(value_offset = 0),
        future = hourly_calendar_test__series(
            year = 2061L,
            experiment_id = "ssp585",
            period = "2060s",
            value_offset = 2
        )
    )
    calendar <- hourly_calendar_test__apply(inputs)
    signal <- component__execute(
        kqdm__component(),
        "apply",
        inputs = inputs,
        groups = calendar@value,
        overrides = list(tas = list(
            grid_points = 128L,
            min_samples = 30L
        )),
        warn_experimental = FALSE
    )

    expect_s7_class(signal, SignalExecutionResult)
    expect_s7_class(signal@values[[1L]], SubdailyAdjustedSeries)
    expect_identical(signal@values[[1L]]@frequency, "hour")
    expect_identical(
        unique(signal@values[[1L]]@data$cf_calendar),
        "noleap"
    )
})

test_that("hourly calendar grouping is registered with bridge contracts", {
    hourly_calendar__register_component()
    component <- component__get("calendar", "hourly_calendar_grouping")

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@stage, "calendar")
    expect_identical(component@input_kinds, "hourly_role_inputs")
    expect_identical(
        component@output_kinds,
        "calendar_indexed_hourly_series"
    )
    expect_identical(component@scopes, "univariate")
    expect_false(component@stochastic)
    expect_true(component__compatible(
        temporal__linear_component(),
        component
    ))
    expect_true(component__compatible(
        solar__component(),
        component
    ))
    expect_true(component__compatible(component, kqdm__component()))
    expect_true(component__compatible(
        kqdm__component(),
        sequence__direct_model_component()
    ))
})

test_that("three-hour source roles compile through the real hourly bridge", {
    # Downstream placeholders isolate compilation of the implemented
    # preprocess-calendar-signal-sequence chain from later EPW assembly work.
    downstream <- list(
        hourly = component__spec(
            name = "hourly_calendar_compile_hourly",
            stage = "hourly",
            input_kinds = "direct_model_sequence",
            output_kinds = "hourly_calendar_compile_weather",
            operations = list(reconstruct = identity)
        ),
        physics = component__spec(
            name = "hourly_calendar_compile_physics",
            stage = "physics",
            input_kinds = "hourly_calendar_compile_weather",
            output_kinds = "hourly_calendar_compile_closed",
            operations = list(apply = identity)
        ),
        output = component__spec(
            name = "hourly_calendar_compile_output",
            stage = "output",
            input_kinds = "hourly_calendar_compile_closed",
            output_kinds = "hourly_calendar_compile_result",
            operations = list(write = identity)
        )
    )
    invisible(lapply(downstream, component__register, overwrite = TRUE))
    temporal__register_linear_component()
    hourly_calendar__register_component()
    kqdm__register_component()
    sequence__register_direct_model_component()

    # Minimal descriptors prove compilation uses the preprocessed intermediate
    # kind instead of requiring raw model sources to be hourly already.
    source <- function(role, frequency, calendar, experiment) {
        weather__new_input(
            role,
            data.frame(
                variable_id = "tas",
                frequency = frequency,
                cf_calendar = calendar,
                value = 1,
                units = "K",
                experiment_id = experiment,
                stringsAsFactors = FALSE
            )
        )
    }
    inputs <- weather__new_inputs(
        observed_reference = source(
            "observed_reference",
            "hour",
            "gregorian",
            "observed"
        ),
        model_historical = source(
            "model_historical",
            "3hr",
            "noleap",
            "historical"
        ),
        model_future = source(
            "model_future",
            "3hr",
            "noleap",
            "ssp585"
        )
    )
    spec <- pipeline__spec(list(
        preprocess = "linear_temporal_interpolation",
        calendar = "hourly_calendar_grouping",
        signal = "kernel_quantile_delta_mapping_hourly",
        sequence = "direct_model_realization",
        hourly = "hourly_calendar_compile_hourly",
        physics = "hourly_calendar_compile_physics",
        output = "hourly_calendar_compile_output"
    ))

    expect_s7_class(pipeline__compile(spec, inputs), WeatherPipelinePlan)
})
