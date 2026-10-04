# Exercise all spatial areas against one real local NetCDF axis. Distinct
# values at every cell also detect reshaping or ordering errors across blocks.
test_that("native time runs use the spatial group's value allowance", {
    path <- tempfile(fileext = ".nc")
    nc <- RNetCDF::create.nc(path)
    RNetCDF::dim.def.nc(nc, "time", 9000L)
    RNetCDF::dim.def.nc(nc, "lat", 3L)
    RNetCDF::dim.def.nc(nc, "lon", 3L)
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
    RNetCDF::var.def.nc(nc, "tas", "NC_DOUBLE", c("time", "lat", "lon"))
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", "days since 2060-01-01")
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "365_day")
    RNetCDF::var.put.nc(nc, "time", 0:8999)
    RNetCDF::var.put.nc(nc, "lat", c(0, 1, 2))
    RNetCDF::var.put.nc(nc, "lon", c(0, 1, 2))
    values <- array(seq_len(81000L), dim = c(9000L, 3L, 3L))
    RNetCDF::var.put.nc(nc, "tas", values)
    RNetCDF::close.nc(nc)
    withr::defer(unlink(path))
    dataset <- EsgDataset$new(path)
    dataset$open()
    withr::defer(dataset$close())
    sites <- data.table::data.table(
        site_id = letters[1:5],
        lon = c(0, 1, 0, 1, 2),
        lat = c(0, 0, 1, 1, 2),
        method = "nearest"
    )
    expected_counts <- list(
        c(8192L, 808L),
        c(4096L, 4096L, 808L),
        c(2048L, 2048L, 2048L, 2048L, 808L),
        c(2048L, 2048L, 2048L, 2048L, 8192L, 808L, 808L)
    )
    # These small fixed cases exercise actual I/O, rather than a mock planner.
    for (index in seq_along(expected_counts)) {
        size <- c(1L, 2L, 4L, 5L)[[index]]
        actual <- dataset__read_regions(dataset, "tas", sites[seq_len(size)])
        slices <- attr(actual, "read_slices")
        expect_equal(slices$time_count, expected_counts[[index]])
        expect_true(all(
            slices$time_count * slices$lat_count * slices$lon_count <= 8192L
        ))
        for (site in seq_len(size)) {
            expected <- values[, sites$lat[[site]] + 1L, sites$lon[[site]] + 1L]
            expect_identical(
                actual$value[actual$site_id == sites$site_id[[site]]],
                as.numeric(expected)
            )
        }
    }
    # A different limit must change request partitioning without changing data.
    local_mocked_bindings(DATASET_REQUEST_MAX_VALUES = 4096L)
    actual <- dataset__read_regions(dataset, "tas", sites[1:4])
    slices <- attr(actual, "read_slices")
    expect_equal(slices$time_count, c(rep(1024L, 8L), 808L))
    expect_true(all(
        slices$time_count * slices$lat_count * slices$lon_count <= 4096L
    ))
    for (site in 1:4) {
        expect_identical(
            actual$value[actual$site_id == sites$site_id[[site]]],
            as.numeric(values[, sites$lat[[site]] + 1L, sites$lon[[site]] + 1L])
        )
    }
})

# vim: fdm=marker :
