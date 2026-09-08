# Build complete canonical daily tas rows for the common adjustment-to-EPW
# adapter tests without depending on another test file's local helpers.
daily_adjustment_test__climate <- function(
    years,
    period,
    experiment,
    offset = 0
) {
    rows <- lapply(as.integer(years), function(year) {
        fields <- cf_time_offset2date(
            0:364,
            data.frame(year = year, month = 1L, day = 1L),
            "noleap"
        )
        fields$hour <- 12L
        fields$minute <- 0L
        fields$second <- 0
        coordinates <- cf_time__coordinates(fields, "noleap")
        time <- as.POSIXct(
            sprintf(
                "%04d-%02d-%02d 12:00:00",
                fields$year,
                fields$month,
                fields$day
            ),
            tz = "UTC"
        )
        data.table::data.table(
            activity_id = if (identical(experiment, "historical")) {
                "CMIP"
            } else {
                "ScenarioMIP"
            },
            institution_id = "Test Institute",
            source_id = "Test-GCM",
            experiment_id = experiment,
            variant_label = "r1i1p1f1",
            grid_label = "gn",
            frequency = "day",
            table_id = "day",
            variable_id = "tas",
            time = time,
            year = year,
            period = period,
            lon = 104,
            lat = 1.37,
            units = "K",
            value = 273.15 + 20 + offset +
                7 * sin(2 * pi * coordinates$annual_phase),
            coordinates
        )
    })
    data.table::rbindlist(rows, use.names = TRUE)
}

# Construct one complete context whose model identity, periods, observations,
# and EPW template can be reused across all eight adjustment methods.
daily_adjustment_test__context <- function(recipe) {
    observed <- daily_adjustment_test__climate(
        1981:1982,
        "observed",
        "observation",
        offset = 0
    )
    historical <- daily_adjustment_test__climate(
        1991:1992,
        "historical",
        "historical",
        offset = 1
    )
    future <- daily_adjustment_test__climate(
        2061:2062,
        "future",
        "ssp585",
        offset = 3
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        observed_reference = observed,
        recipe = recipe
    )
}
