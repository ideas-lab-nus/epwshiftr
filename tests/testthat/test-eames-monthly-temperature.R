# Build deterministic daily CMIP6 rows whose future changes vary by calendar
# month, allowing the monthly signal to be distinguished from daily smoothing.
eames_monthly_test__climate <- function(
    years,
    period,
    experiment,
    mean_shift = rep(0, 12L),
    minimum_shift = rep(0, 12L),
    maximum_shift = rep(0, 12L),
    include_extrema = TRUE,
    frequency = "day"
) {
    years <- as.integer(years)
    shifts <- list(
        tas = as.numeric(mean_shift),
        tasmin = as.numeric(minimum_shift),
        tasmax = as.numeric(maximum_shift)
    )
    stopifnot(all(lengths(shifts) == 12L))

    data.table::rbindlist(lapply(years, function(year) {
        date <- as.Date(sprintf("%04d-01-01", year)) + seq.int(0L, 364L)
        time <- as.POSIXct(date, tz = "UTC") + 12 * 3600
        month <- as.integer(format(date, "%m"))
        phase <- daily__phase_grid(365L)
        seasonal <- 7 * sin(2 * pi * phase)
        values <- list(
            tas = 20 + seasonal,
            tasmin = 16 + seasonal,
            tasmax = 25 + seasonal
        )
        if (!isTRUE(include_extrema)) {
            values <- values["tas"]
        }
        data.table::rbindlist(lapply(names(values), function(variable_id) {
            data.table::data.table(
                activity_id = if (identical(experiment, "historical")) {
                    "CMIP"
                } else {
                    "ScenarioMIP"
                },
                institution_id = "EC-Earth-Consortium",
                source_id = "EC-Earth3",
                experiment_id = experiment,
                variant_label = "r1i1p1f1",
                frequency = frequency,
                table_id = "day",
                variable_id = variable_id,
                time = time,
                year = year,
                annual_phase = phase,
                period = period,
                lon = 104,
                lat = 1.37,
                units = "K",
                value = values[[variable_id]] +
                    shifts[[variable_id]][month] + 273.15
            )
        }))
    }))
}

# Assemble one direct context for the registered temperature-only Eames recipe.
eames_monthly_test__context <- function(
    mean_shift = seq(0.5, 1.6, by = 0.1),
    minimum_shift = seq(0.3, 1.4, by = 0.1),
    maximum_shift = seq(0.7, 1.8, by = 0.1)
) {
    historical <- eames_monthly_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- eames_monthly_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe("eames_monthly_temperature")
    )
}

test_that("Eames daily sources produce 12 month-constant target sets", {
    mean_shift <- seq(0.5, 1.6, by = 0.1)
    minimum_shift <- seq(0.3, 1.4, by = 0.1)
    maximum_shift <- seq(0.7, 1.8, by = 0.1)
    historical <- eames_monthly_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- eames_monthly_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift
    )

    targets <- eames__monthly_temperature_targets(future, historical)

    expect_identical(nrow(targets), 365L)
    expect_identical(sort(unique(targets$month)), seq_len(12L))
    expect_true(all(targets$dtr_status == "adjusted"))
    expect_equal(
        targets$mean_delta,
        mean_shift[targets$month],
        tolerance = 1e-12
    )
    expect_equal(
        targets$minimum_delta,
        minimum_shift[targets$month],
        tolerance = 1e-12
    )
    expect_equal(
        targets$maximum_delta,
        maximum_shift[targets$month],
        tolerance = 1e-12
    )
    expect_equal(
        targets$dtr_delta,
        (maximum_shift - minimum_shift)[targets$month],
        tolerance = 1e-12
    )
    expect_true(all(
        targets[, data.table::uniqueN(mean_delta), by = "month"]$V1 == 1L
    ))
})

test_that("Eames monthly aggregation follows CF dates and removes leap day", {
    source <- eames_monthly_test__climate(
        2001,
        period = "reference",
        experiment = "historical"
    )
    source[, `:=`(
        cf_month = as.integer(format(time, "%m")),
        cf_day = as.integer(format(time, "%d"))
    )]
    baseline <- eames__monthly_temperature_climatology(
        source,
        "historical climate"
    )

    leap_rows <- source[cf_month == 2L & cf_day == 28L]
    leap_rows[, `:=`(
        cf_day = 29L,
        time = as.POSIXct("2001-01-01", tz = "UTC"),
        value = value + 1000
    )]
    with_leap_day <- data.table::rbindlist(
        list(source, leap_rows),
        use.names = TRUE
    )
    mapped <- eames__monthly_temperature_climatology(
        with_leap_day,
        "historical climate"
    )

    expect_equal(mapped$climatology, baseline$climatology, tolerance = 0)
    expect_equal(mapped$n, baseline$n, tolerance = 0)
})

test_that("Eames recipe exposes the adapted monthly temperature boundary", {
    expect_true("eames_monthly_temperature" %in% epw_morph_backends())
    expect_true(
        "eames_monthly_temperature" %in% epw_morph_recipes()[["name"]]
    )

    backend <- epw_morph_backend("eames_monthly_temperature")
    recipe <- epw_morph_recipe("eames_monthly_temperature")
    spec <- epw_morph_recipe_spec("eames_monthly_temperature")

    expect_true(backend$requires_reference)
    expect_equal(
        backend$required_variables(),
        c("tas", "tasmin", "tasmax")
    )
    expect_identical(recipe$policy, "harmonized")
    expect_identical(
        recipe$components$signal,
        "eames_monthly_temperature_delta"
    )
    expect_identical(
        recipe$components$hourly,
        "eames_btws_temperature"
    )
    expect_identical(spec@source$type, "adapted_publication")
    expect_match(spec@source$signal_note, "daily CMIP6")
    expect_match(spec@source$signal_note, "does not apply daily-varying")
    expect_match(spec@source$implementation_note, "non-temperature")
    expect_identical(
        spec@calendar_policy,
        "cf_calendar_month_to_epw_365"
    )
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(
        eames_temperature(),
        "requires an explicit reference"
    )
})

test_that("Eames monthly temperature closes a complete future EPW year", {
    mean_shift <- seq(0.5, 1.6, by = 0.1)
    minimum_shift <- seq(0.3, 1.4, by = 0.1)
    maximum_shift <- seq(0.7, 1.8, by = 0.1)
    context <- eames_monthly_test__context(
        mean_shift,
        minimum_shift,
        maximum_shift
    )
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    baseline_data <- data.table::as.data.table(baseline$data())

    result <- morpher__run_context(context)
    weather <- result$data

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(weather), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_true(all(result$parts$component_pipeline$status == "ok"))
    expect_identical(
        result$parts$component_pipeline$component[
            result$parts$component_pipeline$stage == "signal"
        ],
        "eames_monthly_temperature_delta"
    )
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)
    expect_false(any(!is.na(result$factors$btws_fallback_reason)))

    achieved <- weather[, .(
        baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
        baseline_minimum = min(baseline_data$dry_bulb_temperature[.I]),
        baseline_maximum = max(baseline_data$dry_bulb_temperature[.I]),
        projected_mean = mean(dry_bulb_temperature),
        projected_minimum = min(dry_bulb_temperature),
        projected_maximum = max(dry_bulb_temperature)
    ), by = c("daily_target_day")]
    month <- result$factors$month
    expect_equal(
        achieved$projected_mean - achieved$baseline_mean,
        mean_shift[month],
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_minimum - achieved$baseline_minimum,
        minimum_shift[month],
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_maximum - achieved$baseline_maximum,
        maximum_shift[month],
        tolerance = 1e-8
    )
})

test_that("Eames monthly temperature records infeasible-day fallback", {
    context <- eames_monthly_test__context(
        mean_shift = rep(20, 12L),
        minimum_shift = rep(0, 12L),
        maximum_shift = rep(0, 12L)
    )

    result <- morpher__run_context(context)

    expect_true(any(
        !is.na(result$factors$btws_fallback_reason)
    ))
    expect_true(
        "eames_btws_mean_shift_fallback" %in% result$diagnostics$code
    )
})

test_that("Eames monthly temperature validates daily extrema inputs", {
    missing_extrema <- eames_monthly_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        include_extrema = FALSE
    )
    wrong_frequency <- eames_monthly_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        frequency = "mon"
    )

    expect_error(
        eames__monthly_temperature_climatology(
            missing_extrema,
            "historical climate"
        ),
        "missing required variable"
    )
    expect_error(
        daily__temperature_backend_climate(
            wrong_frequency,
            "historical climate"
        ),
        "must use CMIP frequency"
    )
    expect_error(
        epw_morph_recipe(
            "eames_monthly_temperature",
            options = list(window_days = 31L)
        ),
        "Unknown Eames monthly temperature option"
    )
})

test_that("Eames public method survives dry-run plan reconstruction", {
    method <- eames_temperature(
        historical_reference(years = 1995:2014)
    )
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2060s` = 2061L),
        method = method,
        dir = tempfile("eames-monthly-output-"),
        store = tempfile("eames-monthly-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))

    expect_identical(
        plan@meta$method@recipe$backend,
        "eames_monthly_temperature"
    )
    expect_identical(
        rebuilt@meta$method@recipe$recipe_spec,
        "eames_monthly_temperature"
    )
    expect_identical(
        rebuilt@meta$method@recipe$components$signal,
        "eames_monthly_temperature_delta"
    )
    expect_silent(shift__validate_background_plan(plan))
})
