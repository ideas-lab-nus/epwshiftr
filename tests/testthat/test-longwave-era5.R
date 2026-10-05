# Build matched hourly horizontal accumulations with a known diffuse residual.
longwave_test__era <- function() {
    time <- as.POSIXct("2001-01-01", tz = "UTC") + 0:3 * 3600
    make <- function(value) {
        list(
            units = "J m-2",
            data = data.table::data.table(
                utc_time = time,
                value = value * 3600,
                grid_lat = 23,
                grid_lon = 113,
                grid_dist_km = 0
            )
        )
    }
    list(rsds = make(c(0, 100, 300, 0)), fdir = make(c(0, 40, 250, 0)))
}

test_that("ERA5 diffuse uses matched horizontal fluxes and full CDS access", {
    raw <- longwave_test__era()
    expect_equal(
        era__canonical_hourly(raw, "rsdsdiff")$data$rsdsdiff,
        c(0, 60, 50, 0)
    )
    expect_equal(
        era__canonical_hourly(raw, "rsdsdiff")$data$utc_time,
        raw$rsds$data$utc_time - 3600
    )
    expect_identical(era5__source_variables("rsdsdiff"), c("rsds", "fdir"))
    expect_identical(era5__resolve_access(shift_era5(2001), "rsdsdiff"), "cds")
    expect_error(era5__source_variables("rsdsdiff", "land"), "does not provide")
    raw$fdir$data <- raw$fdir$data[-2]
    expect_error(
        era__canonical_hourly(raw, "rsdsdiff"),
        "identical complete hourly"
    )
    raw <- longwave_test__era()
    raw$fdir$data$value[2] <- 101 * 3600
    expect_error(era__canonical_hourly(raw, "rsdsdiff"), "FDIR exceeds SSRD")
    raw <- longwave_test__era()
    raw$fdir$units <- "K"
    expect_error(era__canonical_hourly(raw, "rsdsdiff"), "units")
})

test_that("longwave opt-in extends all roles without changing default contracts", {
    base <- hourly_transform("kernel_qdm")
    disabled <- hourly_transform("kernel_qdm", include_longwave = FALSE)
    enabled <- hourly_transform("kernel_qdm", include_longwave = TRUE)
    expect_identical(base@options, disabled@options)
    original <- transform__recipe(base)
    recipe <- transform__recipe(enabled)
    expect_true(recipe$options$include_longwave)
    restored <- transform__from_spec(transform__spec_value(enabled))
    expect_identical(restored@options, enabled@options)
    expect_false(identical(
        transform__spec_value(base),
        transform__spec_value(enabled)
    ))
    expect_true("rlds" %in% morpher__input_variables(recipe))
    expect_false("rlds" %in% morpher__input_variables(original))
    expect_true(
        "rlds" %in% reanalysis__variables(shift_era5(1995:2014), recipe)
    )
    expect_equal(morpher__recipe_required_frequency(recipe)[["rlds"]], "3hr")
    expect_error(
        hourly_transform("kernel_qdm", include_longwave = "yes"),
        "flag"
    )
    expect_false(
        "rlds" %in%
            morpher__input_variables(transform__recipe(hourly_transform(
                "kernel_qdm"
            )))
    )
})

# Unit conversions must preserve interval identity before the two radiation
# series are joined; a mixed-unit input previously shifted only one series.
test_that("ERA5 diffuse aligns mixed flux and energy units without changing inputs", {
    raw <- longwave_test__era()
    raw$fdir$units <- "W m-2"
    data.table::set(
        raw$fdir$data,
        j = "value",
        value = raw$fdir$data$value / 3600
    )
    original <- data.table::copy(raw$fdir$data)
    result <- era__canonical_hourly(raw, "rsdsdiff")$data
    expect_equal(result$rsdsdiff, c(0, 60, 50, 0))
    expect_equal(result$utc_time, original$utc_time - 3600)
    expect_identical(raw$fdir$data, original)
    raw$rsds$data <- raw$rsds$data[0]
    raw$fdir$data <- raw$fdir$data[0]
    expect_error(era__canonical_hourly(raw, "rsdsdiff"), "complete hourly")
})
