# Construct one complete hourly variable over one or more native-calendar years
# so the integration fixture can exercise every post-interpolation component.
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
            tas = 288 + 8 * sin(2 * pi * annual) +
                3 * sin(2 * pi * fields$hour / 24),
            ps = 101325 + 500 * sin(2 * pi * annual),
            huss = 0.008 + 0.001 * sin(2 * pi * annual) +
                0.0002 * cos(2 * pi * fields$hour / 24),
            hurs = 55 + 12 * sin(2 * pi * annual) +
                3 * cos(2 * pi * fields$hour / 24),
            uas = 2 + 0.4 * sin(2 * pi * annual) +
                0.1 * cos(2 * pi * fields$hour / 24),
            vas = -3 + 0.3 * sin(2 * pi * annual) -
                0.1 * sin(2 * pi * fields$hour / 24),
            sfcWind = 3 + 0.6 * sin(2 * pi * annual) +
                0.2 * cos(2 * pi * fields$hour / 24),
            rsds = 1 + daylight * (450 + 80 * sin(2 * pi * annual)),
            rsdsdiff = 1 + daylight * (120 + 20 * sin(2 * pi * annual))
        )
        # Role-specific changes keep every distribution non-degenerate while
        # retaining physically valid values for the final closure policy.
        value <- if (variable %in% c("tas", "ps")) {
            base + switch(
                role,
                observed = if (identical(variable, "tas")) -1 else -100,
                historical = 0,
                future = if (identical(variable, "tas")) 2 else 200
            )
        } else {
            base * switch(
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
                switch(role, observed = "observed", historical = "historical", future = "ssp585"),
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

# Assemble the six-variable role tables required by the built-in hourly
# kernel-QDM recipe without involving remote collection or extraction.
hourly_kqdm_test__role <- function(
    years,
    role = c("observed", "historical", "future"),
    calendar = "noleap"
) {
    role <- match.arg(role)
    data.table::rbindlist(lapply(
        EPW_MORPH_HOURLY_KQDM_VARIABLES,
        hourly_kqdm_test__series,
        years = years,
        role = role,
        calendar = calendar
    ), use.names = TRUE)
}

# Reduce the hourly fixture to bounded three-hourly model samples. Point-state
# variables include one following boundary sample needed for the final two
# hourly values; radiation variables retain complete contiguous time bounds.
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
            (source$cf_year %in% years &
                source$cf_second_of_day %% 10800 == 0) |
                boundary,
        ]
        source$frequency <- "3hr"
        source$table_id <- "3hr"
        native_second <- temporal__native_seconds(source, calendar)
        source$time <- as.POSIXct("2000-01-01", tz = "UTC") +
            native_second - native_second[[1L]]
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
        ) * if (identical(role, "future")) 1.1 else 1
        source$cf_second_of_day <- midpoint_second
        source$annual_phase <- (
            source$cf_day_of_year - 1 + midpoint_second / 86400
        ) / source$cf_year_days
        native_second <- temporal__native_seconds(source, calendar)
        source$time <- as.POSIXct("2000-01-01", tz = "UTC") +
            native_second - native_second[[1L]]
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

# Use the smallest valid KDE grid in integration tests while preserving the
# same variable-specific settings resolution used by production execution.
hourly_kqdm_test__overrides <- function() {
    stats::setNames(
        lapply(EPW_MORPH_HOURLY_KQDM_VARIABLES, function(variable) {
            list(grid_points = 128L, min_samples = 3L)
        }),
        EPW_MORPH_HOURLY_KQDM_VARIABLES
    )
}

test_that("hourly kernel QDM configures a complete high-level shift plan", {
    reference <- historical_reference(1995:2014)
    observed <- shift_reference_plan(
        "observed-hourly-plan",
        epw_morph_periods(observed = 1995:2014)
    )
    method <- hourly_kernel_qdm(
        reference = reference,
        observed_reference = observed,
        signal_overrides = hourly_kqdm_test__overrides()
    )
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        frequency = "3hr",
        table = "3hr"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2060s` = 2061:2062),
        method = method,
        dir = tempfile("hourly-kqdm-output-"),
        store = tempfile("hourly-kqdm-store-"),
        dry_run = TRUE
    )
    recipe <- plan@meta$method@recipe
    spec <- shift__plan_spec(plan)
    rebuilt <- shift__plan_from_spec(spec)

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
        c("tas", "ps", "huss", "uas", "vas", "rsds", "rsdsdiff")
    )
    expect_true(plan@meta$method@requires_reference)
    expect_true(plan@meta$method@requires_observed_reference)
    expect_identical(plan@meta$climate@frequency, "3hr")
    expect_identical(
        plan@meta$request@meta$variables,
        EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES
    )
    expect_identical(
        plan@meta$request@meta$time,
        c(
            "2060-12-31T21:00:00Z",
            "2063-01-01T02:59:59Z"
        )
    )
    expect_identical(
        rebuilt@meta$method@observed_reference@plan_id,
        "observed-hourly-plan"
    )
    expect_identical(
        rebuilt@meta$method@recipe$options$signal_overrides$tas$grid_points,
        128L
    )
    historical_request <- shift__historical_request(
        plan,
        "https://example.org"
    )
    expect_identical(
        historical_request@meta$variables,
        EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES
    )
    expect_identical(
        historical_request@meta$options$file_time,
        c(
            "1994-12-31T21:00:00Z",
            "2015-01-01T02:59:59Z"
        )
    )

    expect_error(hourly_kernel_qdm(), "requires an explicit reference")
    expect_error(
        hourly_kernel_qdm(reference = reference),
        "requires an explicit observed reference"
    )
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6(
                "EC-Earth3",
                "ssp585",
                frequency = "day",
                table = "day"
            ),
            periods = list(`2060s` = 2061:2062),
            method = method,
            dir = tempfile("hourly-kqdm-invalid-output-"),
            store = tempfile("hourly-kqdm-invalid-store-"),
            dry_run = TRUE
        ),
        "requires CMIP frequency"
    )
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = climate,
            periods = list(`2060s` = 2061L),
            method = method,
            dir = tempfile("hourly-kqdm-one-year-output-"),
            store = tempfile("hourly-kqdm-one-year-store-"),
            dry_run = TRUE
        ),
        "requires at least two weather years"
    )
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
        vapply(result@members, function(member) member@weather_year, integer(1L)),
        2061:2062
    )
    expect_true(all(vapply(
        result@members,
        function(member) nrow(member@data) == 8760L,
        logical(1L)
    )))
    expect_true(all(result@diagnostics$physical_policy ==
        "absolute_model_fields"))
    expect_true(all(result@diagnostics$wind_direction_policy ==
        "supplied_wind_direction"))
    expect_identical(
        result@parts$component_pipeline$component,
        unname(unlist(hourly_kqdm__pipeline()@components))
    )
    expect_true(all(vapply(result@members, function(member) {
        weather <- member@data
        all(weather$dew_point_temperature <= weather$dry_bulb_temperature) &&
            all(weather$relative_humidity >= 0) &&
            all(weather$relative_humidity <= 100) &&
            all(weather$wind_speed >= 0) &&
            all(weather$wind_direction >= 0) &&
            all(weather$wind_direction < 360)
    }, logical(1L))))
    expect_true(all(vapply(
        result@members,
        function(member) {
            "wind_direction" %in% member@provenance$constructed_fields
        },
        logical(1L)
    )))
})
