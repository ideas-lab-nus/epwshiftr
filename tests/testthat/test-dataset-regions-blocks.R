# Exercise all spatial areas against one real local NetCDF axis. Distinct
# values at every cell also detect reshaping or ordering errors across blocks.
test_that("regional reads preserve values across long time axes and site groups", {
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
    # Exercise different spatial shapes with actual local NetCDF reads.
    for (size in c(1L, 2L, 4L, 5L)) {
        actual <- dataset__read_regions(dataset, "tas", sites[seq_len(size)])
        for (site in seq_len(size)) {
            expected <- values[, sites$lat[[site]] + 1L, sites$lon[[site]] + 1L]
            expect_identical(
                actual$value[actual$site_id == sites$site_id[[site]]],
                as.numeric(expected)
            )
        }
    }
})

# vim: fdm=marker :
