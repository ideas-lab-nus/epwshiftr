# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Construct one complete hourly variable over one or more native-calendar years
# so the integration fixture can exercise every post-interpolation component.
# hourly_kqdm_test__series {{{
hourly_kqdm_test__series <- function(
    variable,
    years,
    role = c("observed", "historical", "future"),
    calendar = "noleap"
) {
    role <- match.arg(role)
    units <- switch(
        variable,
        tas = "K",
        ps = "Pa",
        huss = "kg kg-1",
        hurs = "%",
        uas = "m s-1",
        vas = "m s-1",
        sfcWind = "m s-1",
        rsds = "W m-2",
        rsdsdiff = "W m-2"
    )
    rows <- lapply(seq_along(years), function(index) {
        year <- as.integer(years[[index]])
        year_days <- cf_time__year_days(year, calendar)[[1L]]
        hour_index <- seq.int(0L, year_days * 24L - 1L)
        fields <- cf_time_offset2date(
            hour_index %/% 24L,
            data.frame(year = year, month = 1L, day = 1L),
            calendar
        )
        fields$hour <- hour_index %% 24L
        fields$minute <- 0L
        fields$second <- 0
        coordinates <- cf_time__coordinates(fields, calendar)
        annual <- coordinates$annual_phase
        daylight <- pmax(0, sin(pi * (fields$hour - 6) / 12))
        base <- switch(
            variable,
            tas = 288 +
                8 * sin(2 * pi * annual) +
                3 * sin(2 * pi * fields$hour / 24),
            ps = 101325 + 500 * sin(2 * pi * annual),
            huss = 0.008 +
                0.001 * sin(2 * pi * annual) +
                0.0002 * cos(2 * pi * fields$hour / 24),
            hurs = 55 +
                12 * sin(2 * pi * annual) +
                3 * cos(2 * pi * fields$hour / 24),
            uas = 2 +
                0.4 * sin(2 * pi * annual) +
                0.1 * cos(2 * pi * fields$hour / 24),
            vas = -3 +
                0.3 * sin(2 * pi * annual) -
                0.1 * sin(2 * pi * fields$hour / 24),
            sfcWind = 3 +
                0.6 * sin(2 * pi * annual) +
                0.2 * cos(2 * pi * fields$hour / 24),
            rsds = 1 + daylight * (450 + 80 * sin(2 * pi * annual)),
            rsdsdiff = 1 + daylight * (120 + 20 * sin(2 * pi * annual))
        )
        # Role-specific changes keep every distribution non-degenerate while
        # retaining physically valid values for the final closure policy.
        value <- if (variable %in% c("tas", "ps")) {
            base +
                switch(
                    role,
                    observed = if (identical(variable, "tas")) -1 else -100,
                    historical = 0,
                    future = if (identical(variable, "tas")) 2 else 200
                )
        } else {
            base *
                switch(
                    role,
                    observed = 1.05,
                    historical = 1,
                    future = 1.1
                )
        }
        value <- value + (index - 1L) * 0.01
        data.frame(
            site_id = rep.int("A", length(hour_index)),
            source_id = rep.int(
                if (identical(role, "observed")) "station" else "example-model",
                length(hour_index)
            ),
            experiment_id = rep.int(
                switch(
                    role,
                    observed = "observed",
                    historical = "historical",
                    future = "ssp585"
                ),
                length(hour_index)
            ),
            variant_label = rep.int("r1i1p1f1", length(hour_index)),
            grid_label = rep.int("gn", length(hour_index)),
            table_id = rep.int("hour", length(hour_index)),
            period = rep.int(
                if (identical(role, "future")) "2060s" else "reference",
                length(hour_index)
            ),
            variable_id = rep.int(variable, length(hour_index)),
            value = as.numeric(value),
            units = rep.int(units, length(hour_index)),
            frequency = rep.int("hour", length(hour_index)),
            time = as.POSIXct(sprintf("%04d-01-01", year), tz = "UTC") +
                hour_index * 3600,
            lon = rep.int(103.98, length(hour_index)),
            lat = rep.int(1.37, length(hour_index)),
            coordinates,
            cf_second_of_day = as.numeric(fields$hour) * 3600,
            stringsAsFactors = FALSE
        )
    })
    data.table::rbindlist(rows, use.names = TRUE)
}
# }}}

# Assemble the six-variable role tables required by the built-in hourly
# kernel-QDM recipe without involving remote collection or extraction.
# hourly_kqdm_test__role {{{
hourly_kqdm_test__role <- function(
    years,
    role = c("observed", "historical", "future"),
    calendar = "noleap"
) {
    role <- match.arg(role)
    data.table::rbindlist(
        lapply(
            EPW_MORPH_HOURLY_KQDM_VARIABLES,
            hourly_kqdm_test__series,
            years = years,
            role = role,
            calendar = calendar
        ),
        use.names = TRUE
    )
}
# }}}

# Reduce the hourly fixture to bounded three-hourly model samples. Point-state
# variables use the CMIP6 `3hrPt` facet and include one following boundary
# sample; radiation variables use the interval-mean `3hr` facet.
# hourly_kqdm_test__model_role {{{
hourly_kqdm_test__model_role <- function(
    years,
    role = c("historical", "future"),
    calendar = "360_day"
) {
    role <- match.arg(role)
    point_variables <- setdiff(
        EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES,
        SOLAR_RADIATION_VARIABLES
    )
    point <- lapply(point_variables, function(variable) {
        source <- hourly_kqdm_test__series(
            variable,
            c(years, max(years) + 1L),
            role,
            calendar
        )
        boundary <- source$cf_year == max(years) + 1L &
            source$cf_day_of_year == 1L &
            source$cf_second_of_day == 0
        source <- source[
            (source$cf_year %in%
                years &
                source$cf_second_of_day %% 10800 == 0) |
                boundary,
        ]
        source$frequency <- "3hrPt"
        source$table_id <- "3hr"
        native_second <- temporal__native_seconds(source, calendar)
        source$time <- as.POSIXct("2000-01-01", tz = "UTC") +
            native_second -
            native_second[[1L]]
        source
    })
    radiation <- lapply(SOLAR_RADIATION_VARIABLES, function(variable) {
        source <- hourly_kqdm_test__series(
            variable,
            years,
            role,
            calendar
        )
        source <- source[source$cf_second_of_day %% 10800 == 0, ]
        start_second <- source$cf_second_of_day
        midpoint_second <- start_second + 5400
        midpoint_hour <- midpoint_second / 3600
        daylight <- pmax(0, sin(pi * (midpoint_hour - 6) / 12))
        seasonal <- 1 + 0.15 * sin(2 * pi * source$annual_phase)
        source$value <- switch(
            variable,
            rsds = 450 * daylight * seasonal,
            rsdsdiff = 120 * daylight * seasonal
        ) *
            if (identical(role, "future")) 1.1 else 1
        source$cf_second_of_day <- midpoint_second
        source$annual_phase <- (source$cf_day_of_year -
            1 +
            midpoint_second / 86400) /
            source$cf_year_days
        native_second <- temporal__native_seconds(source, calendar)
        source$time <- as.POSIXct("2000-01-01", tz = "UTC") +
            native_second -
            native_second[[1L]]
        source$time_bound_start <- source$time - 5400
        source$time_bound_end <- source$time + 5400
        source$frequency <- "3hr"
        source$table_id <- "3hr"
        source$lon <- 0
        source$lat <- 0
        source
    })
    data.table::rbindlist(c(point, radiation), use.names = TRUE, fill = TRUE)
}
# }}}

# Use the smallest valid KDE grid in integration tests while preserving the
# same variable-specific settings resolution used by production execution.
# hourly_kqdm_test__overrides {{{
hourly_kqdm_test__overrides <- function() {
    stats::setNames(
        lapply(EPW_MORPH_HOURLY_KQDM_VARIABLES, function(variable) {
            list(grid_points = 128L, min_samples = 3L)
        }),
        EPW_MORPH_HOURLY_KQDM_VARIABLES
    )
}
# }}}

test_that("hourly kernel QDM configures an explicit site-specific shift plan", {
    reference <- historical_reference(1995:2014)
    observed <- shift_reference_from_plan(
        "observed-hourly-plan",
        epw_morph_periods(observed = 1995:2014),
        role = "observed_reference"
    )
    transform <- do.call(
        hourly_transform,
        c(
            list(method = "kernel_qdm"),
            hourly_kqdm_test__overrides()
        )
    )
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        frequency = HOURLY_KQDM_MODEL_FREQUENCIES
    )
    periods <- epw_morph_periods(`2060s` = 2061:2062)
    plan <- shift_plan(
        request = shift_spec__request_from_cmip6(climate, periods, transform),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        reference = reference,
        observed_reference = observed,
        store = tempfile("method-reference-store-")
    )
    recipe <- plan@meta$recipe
    spec <- shift_persist__plan_spec(plan)
    rebuilt <- shift_persist__plan_from_spec(spec)

    expect_identical(recipe$name, "hourly_kernel_qdm")
    expect_identical(recipe$backend, "hourly_kernel_qdm")
    expect_identical(recipe$policy, "harmonized")
    expect_identical(recipe$components, hourly_kqdm__pipeline()@components)
    expect_identical(
        epw_morph_variables(recipe),
        EPW_MORPH_HOURLY_KQDM_VARIABLES
    )
    expect_identical(
        morpher__input_variables(recipe),
        c(
            "tas",
            "ps",
            "huss",
            "uas",
            "vas",
            "rsds",
            "rsdsdiff",
            "tasmin",
            "tasmax"
        )
    )
    expect_true(transform__requires_input(transform, "model_historical"))
    expect_true(transform__requires_input(transform, "observed_reference"))
    expect_identical(
        plan@meta$request@meta$frequency,
        HOURLY_KQDM_MODEL_FREQUENCIES
    )
    expect_identical(
        plan@meta$request@meta$variables,
        c(
            EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES,
            HOURLY_WEATHER_EXTREMA_VARIABLES
        )
    )
    expect_identical(
        plan@meta$request@meta$time,
        c(
            "2060-12-31T21:00:00Z",
            "2063-01-01T02:59:59Z"
        )
    )
    expect_identical(
        rebuilt@meta$observed_reference@plan_id,
        "observed-hourly-plan"
    )
    expect_identical(
        rebuilt@meta$recipe$options$signal_overrides$tas$grid_points,
        128L
    )
    historical_request <- shift_resolve__historical_request(
        plan,
        "https://example.org"
    )
    expect_identical(
        historical_request@meta$variables,
        c(
            EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES,
            HOURLY_WEATHER_EXTREMA_VARIABLES
        )
    )
    expect_identical(
        historical_request@meta$options$file_time,
        c(
            "1994-12-31T21:00:00Z",
            "2015-01-01T02:59:59Z"
        )
    )

    expect_error(
        transform__validate_execution_inputs(hourly_transform("kernel_qdm")),
        "requires.*reference"
    )
    expect_error(
        transform__validate_execution_inputs(
            hourly_transform("kernel_qdm"),
            reference = reference
        ),
        "requires.*observed_reference"
    )
    expect_error(
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            climate = shift_cmip6(
                "EC-Earth3",
                "ssp585",
                frequency = "day",
                table = "day"
            ),
            periods = list(`2060s` = 2061:2062),
            transform = transform,
            reference = reference,
            observed_reference = observed,
            dir = tempfile("hourly-kqdm-invalid-output-"),
            store = tempfile("hourly-kqdm-invalid-store-"),
            dry_run = TRUE
        )@meta$children[[1L]],
        "requires CMIP frequencies"
    )
    expect_error(
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            climate = climate,
            periods = list(`2060s` = 2061L),
            transform = transform,
            reference = reference,
            observed_reference = observed,
            dir = tempfile("hourly-kqdm-one-year-output-"),
            store = tempfile("hourly-kqdm-one-year-store-"),
            dry_run = TRUE
        )@meta$children[[1L]],
        "requires at least two weather years"
    )
})

test_that("hourly frequency diagnostics validate only declared model variables", {
    recipe <- epw_morph_recipe("hourly_kernel_qdm")
    diagnostic <- morpher__frequency_diagnostic(
        recipe,
        frequency = c("3hrPt", "3hr", "hour"),
        variable_id = c("tas", "rsds", "observed_tas"),
        stage = "climate_summary"
    )
    invalid <- morpher__frequency_diagnostic(
        recipe,
        frequency = c("3hrPt", "3hrPt"),
        variable_id = c("tas", "rsds"),
        stage = "climate_summary"
    )

    expect_identical(nrow(diagnostic), 0L)
    expect_identical(invalid$code[[1L]], "unsupported_climate_frequency")
})

test_that("hourly kernel QDM produces two physically closed EPW years", {
    observed <- hourly_kqdm_test__role(2001L, "observed", "noleap")
    historical <- hourly_kqdm_test__model_role(
        1991L,
        "historical",
        "360_day"
    )
    future <- hourly_kqdm_test__model_role(
        2061:2062,
        "future",
        "360_day"
    )
    recipe <- epw_morph_recipe(
        "hourly_kernel_qdm",
        options = list(
            signal_overrides = hourly_kqdm_test__overrides()
        )
    )
    context <- morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        observed_reference = observed,
        recipe = recipe,
        by = "site_id"
    )
    result <- suppressWarnings(morpher__run_context(context))

    expect_identical(result@backend, "hourly_kernel_qdm")
    expect_identical(result@output_type, "multi_year")
    expect_identical(
        vapply(
            result@members,
            function(member) member@weather_year,
            integer(1L)
        ),
        2061:2062
    )
    expect_true(all(vapply(
        result@members,
        function(member) nrow(member@data) == 8760L,
        logical(1L)
    )))
    expect_true(all(
        result@diagnostics$physical_policy == "absolute_model_fields"
    ))
    expect_true(all(
        result@diagnostics$wind_direction_policy == "supplied_wind_direction"
    ))
    expect_identical(
        result@parts$component_pipeline$component,
        unname(unlist(hourly_kqdm__pipeline()@components))
    )
    # The complete output must retain signal products and expose canonical
    # runtime rows to store persistence, without expanding yearly JSON records.
    signal <- result@parts$signal
    expect_length(signal$groups, 6L)
    expect_true(all(vapply(
        signal$groups,
        function(group) {
            nrow(group$data) == nrow(group$provenance$mapping$rows) &&
                length(group$provenance$mapping$distributions) == 12L
        },
        logical(1L)
    )))
    runtime <- morpher__result_diagnostics(result)
    expect_named(runtime, morpher__diagnostic_columns())
    expect_equal(morpher__bind_diagnostics(runtime), runtime)
    expect_true(all(
        nchar(vapply(
            sequence__records(result),
            function(member) {
                as.character(morpher__json(member$provenance))
            },
            character(1L)
        )) <
            100000L
    ))
    expect_true(all(vapply(
        result@members,
        function(member) {
            weather <- member@data
            all(
                weather$dew_point_temperature <= weather$dry_bulb_temperature
            ) &&
                all(weather$relative_humidity >= 0) &&
                all(weather$relative_humidity <= 100) &&
                all(weather$wind_speed >= 0) &&
                all(weather$wind_direction >= 0) &&
                all(weather$wind_direction < 360)
        },
        logical(1L)
    )))
    expect_true(all(vapply(
        result@members,
        function(member) {
            "wind_direction" %in% member@provenance$constructed_fields
        },
        logical(1L)
    )))
})

test_that("longwave extension runs through reconstruction, QDM and EPW output", {
    observed <- hourly_kqdm_test__role(2001L, "observed")
    longwave <- data.table::copy(observed[variable_id == "tas"])
    longwave[, `:=`(
        variable_id = "rlds",
        units = "W m-2",
        value = 330 + value / 10
    )]
    observed <- data.table::rbindlist(list(observed, longwave), fill = TRUE)
    add_longwave <- function(data) {
        longwave <- data.table::copy(data[variable_id == "rsds"])
        longwave[, `:=`(variable_id = "rlds", value = 330 + value / 100)]
        data.table::rbindlist(list(data, longwave), fill = TRUE)
    }
    historical <- add_longwave(hourly_kqdm_test__model_role(
        1991L,
        "historical"
    ))
    future <- add_longwave(hourly_kqdm_test__model_role(2061:2062, "future"))
    overrides <- c(
        hourly_kqdm_test__overrides(),
        list(rlds = list(grid_points = 128L, min_samples = 3L))
    )
    recipe <- epw_morph_recipe(
        "hourly_kernel_qdm",
        options = list(
            include_longwave = TRUE,
            signal_overrides = overrides
        )
    )
    context <- morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        observed_reference = observed,
        recipe = recipe,
        by = "site_id"
    )
    result <- suppressWarnings(morpher__run_context(context))
    expect_length(result@members, 2L)
    member <- result@members[[1L]]
    expect_true(
        "horizontal_infrared_radiation_intensity_from_sky" %in%
            member@provenance$constructed_fields
    )
    expect_true(all(
        member@data$horizontal_infrared_radiation_intensity_from_sky > 0
    ))
    expect_gt(
        stats::sd(member@data$horizontal_infrared_radiation_intensity_from_sky),
        0
    )
    diagnostics <- result@parts$component_pipeline
    expect_true(nrow(diagnostics) > 0)
})

test_that("local clock shifting crosses native year boundaries without wrapping data", {
    data <- hourly_kqdm_test__series("tas", 2001L, "historical", "360_day")
    input <- weather__new_input(
        "model_historical",
        data,
        representation = "series",
        variables = "tas",
        frequencies = "hour",
        calendars = "360_day"
    )
    shifted <- weather_interp__local_piece(
        list(input = input, provenance = list()),
        8
    )$input@source
    expect_equal(shifted$value, data$value)
    expect_equal(
        as.numeric(shifted$time - data$time, units = "hours"),
        rep(8, nrow(data))
    )
    expect_equal(shifted$cf_year[nrow(shifted)], 2002L)
    expect_equal(shifted$cf_second_of_day[1], 8 * 3600)
})

# Raw model and observed sources have distinct humidity, wind and time contracts.
test_that("hourly preflight validates each source role before derivation", {
    recipe <- epw_morph_recipe("hourly_kernel_qdm")
    model <- c("tas", "ps", "huss", "uas", "vas", "rsds", "rsdsdiff")
    observed <- c("tas", "ps", "hurs", "sfcWind", "rsds", "rsdsdiff")
    expect_setequal(morpher__preflight_variables(recipe, "model_future"), model)
    expect_setequal(
        morpher__preflight_variables(recipe, "model_historical"),
        model
    )
    expect_setequal(
        morpher__preflight_variables(recipe, "observed_reference"),
        observed
    )
    expect_setequal(epw_morph_variables(recipe), observed)
    frequencies <- data.table::fifelse(
        model %in% c("rsds", "rsdsdiff"),
        "3hr",
        "3hrPt"
    )
    expect_equal(
        nrow(morpher__frequency_diagnostic(
            recipe,
            frequencies,
            variable_id = model,
            stage = "extraction"
        )),
        0L
    )
    expect_equal(
        nrow(morpher__frequency_diagnostic(
            recipe,
            rep("hour", 6L),
            variable_id = observed,
            stage = "extraction",
            input_role = "observed_reference"
        )),
        0L
    )
    bad <- morpher__frequency_diagnostic(
        recipe,
        rep("day", 6L),
        variable_id = observed,
        stage = "extraction",
        input_role = "observed_reference"
    )
    expect_identical(bad$severity, "error")
    expect_identical(bad$code, "unsupported_climate_frequency")
    bad <- morpher__frequency_diagnostic(
        recipe,
        rep("hour", 7L),
        variable_id = model,
        stage = "extraction"
    )
    expect_identical(bad$severity, "error")
})

# Boundary samples supply interpolation support without adding full extra years.
test_that("hourly period selection retains bounded native-calendar padding", {
    source <- data.table::data.table(
        year = c(2059L, 2060L, 2060L, 2061L, 2062L, 2062L),
        cf_day_of_year = c(360L, 358L, 360L, 180L, 1L, 3L),
        cf_year_days = 360L,
        value = seq_len(6L)
    )
    periods <- data.table::data.table(
        period = c("first", "second"),
        year = c(2061L, 2062L)
    )
    result <- morpher__period_climate(source, periods, TRUE)
    expect_identical(result$value[result$period == "first"], c(3L, 4L, 5L))
    expect_identical(result$value[result$period == "second"], c(5L, 6L))
    expect_false("period" %in% names(source))
    ordinary <- morpher__period_climate(source, periods)
    expect_identical(ordinary$value, c(4L, 5L, 6L))
})

# vim: fdm=marker :
