# Helper: create a temporary NetCDF file for testing
create_test_nc <- function(filepath) {
    nc <- RNetCDF::create.nc(filepath)
    # add dimensions
    RNetCDF::dim.def.nc(nc, "time", 12)
    RNetCDF::dim.def.nc(nc, "lat", 5)
    RNetCDF::dim.def.nc(nc, "lon", 10)
    # add variables
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
    RNetCDF::var.def.nc(nc, "tas", "NC_FLOAT", c("lon", "lat", "time"))
    # add attributes
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", "days since 2015-01-01")
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "standard")
    RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(nc, "tas", "units", "NC_CHAR", "K")
    # put data
    RNetCDF::var.put.nc(nc, "time", 1:12)
    RNetCDF::var.put.nc(nc, "lat", seq(-90, 90, length.out = 5))
    RNetCDF::var.put.nc(nc, "lon", seq(0, 360, length.out = 10))
    set.seed(42)
    RNetCDF::var.put.nc(nc, "tas", array(rnorm(600), dim = c(10, 5, 12)))
    RNetCDF::close.nc(nc)
    filepath
}

# Creation / Initialization {{{
test_that("EsgDataset can be created with a single URL", {
    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)
    expect_equal(ds$url, url)
})

test_that("EsgDataset can be created with multiple URLs", {
    urls <- c("https://example.com/data1.nc", "https://example.com/data2.nc")
    ds <- EsgDataset$new(urls, aggregate = TRUE)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)
})

test_that("EsgDataset aggregate=FALSE with multiple URLs", {
    urls <- c("https://example.com/data1.nc", "https://example.com/data2.nc")
    ds <- EsgDataset$new(urls, aggregate = FALSE)
    expect_false(ds$is_aggregated)
})

test_that("EsgDataset rejects invalid inputs", {
    expect_error(EsgDataset$new(character(0)))
    expect_error(EsgDataset$new(NA_character_))
    expect_error(EsgDataset$new("url", aggregate = "yes"))
})
# }}}

# Open / Close with local NetCDF {{{
test_that("EsgDataset can open and close local NetCDF files", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    expect_false(ds$is_open)

    ds$open()
    expect_true(ds$is_open)

    # Opening again should warn
    expect_warning(ds$open(), "already open")

    ds$close()
    expect_false(ds$is_open)
})

test_that("EsgDataset finalize closes connections", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    expect_true(ds$is_open)

    # Trigger finalizer
    ds$close()
    expect_false(ds$is_open)
})
# }}}

# Basic layer methods {{{
test_that("EsgDataset file_inq works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true("nvars" %in% names(info))
    expect_true("ndims" %in% names(info))
    expect_equal(info$ndims, 3L)
    expect_equal(info$nvars, 4L) # time, lat, lon, tas
})

test_that("EsgDataset var_inq works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    var_info <- ds$var_inq("tas")
    expect_type(var_info, "list")
    expect_equal(var_info$name, "tas")
    expect_equal(var_info$ndims, 3L)
})

test_that("EsgDataset dim_inq works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    dim_info <- ds$dim_inq("time")
    expect_type(dim_info, "list")
    expect_equal(dim_info$name, "time")
    expect_equal(dim_info$length, 12L)
})

# Middle layer methods {{{
test_that("EsgDataset get_variables works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    vars <- ds$get_variables()
    expect_type(vars, "character")
    expect_true(length(vars) > 0)
    expect_true("tas" %in% vars)
    expect_true("time" %in% vars)
    expect_true("lat" %in% vars)
    expect_true("lon" %in% vars)
})

test_that("EsgDataset get_dimensions works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    dims <- ds$get_dimensions()
    expect_type(dims, "character")
    expect_true("time" %in% dims)
    expect_true("lat" %in% dims)
    expect_true("lon" %in% dims)
})

test_that("EsgDataset get_time_axis works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    time_info <- ds$get_time_axis()
    expect_type(time_info, "list")
    expect_true("values" %in% names(time_info))
    expect_true("units" %in% names(time_info))
    expect_true("calendar" %in% names(time_info))
    expect_true("length" %in% names(time_info))
    expect_equal(time_info$units, "days since 2015-01-01")
    expect_equal(time_info$calendar, "standard")
    expect_equal(time_info$length, 12L)
    expect_equal(length(time_info$values), 12L)

    # Test caching: second call should return same result
    time_info2 <- ds$get_time_axis()
    expect_identical(time_info, time_info2)
})

test_that("EsgDataset get_spatial_grid works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    grid <- ds$get_spatial_grid()
    expect_type(grid, "list")
    expect_true("lat" %in% names(grid))
    expect_true("lon" %in% names(grid))
    expect_equal(length(grid$lat), 5L)
    expect_equal(length(grid$lon), 10L)

    # Test caching
    grid2 <- ds$get_spatial_grid()
    expect_identical(grid, grid2)
})
# }}}

# High layer methods {{{
test_that("EsgDataset read_array works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    arr <- ds$read_array("tas", start = c(1, 1, 1), count = c(2, 3, 4))
    expect_type(arr, "double")
    expect_equal(dim(arr), c(2L, 3L, 4L))
})

test_that("EsgDataset read_data_table works on local file", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(2, 3, 4))
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    # 2 * 3 * 4 = 24 rows
    expect_equal(nrow(dt), 24L)
})
# }}}

# Error handling {{{
test_that("EsgDataset errors on closed dataset", {
    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)

    expect_error(ds$file_inq(), "not open")
    expect_error(ds$var_inq("tas"), "not open")
    expect_error(ds$dim_inq("time"), "not open")
    expect_error(ds$att_get("tas", "units"), "not open")
    expect_error(ds$var_get("tas"), "not open")
    expect_error(ds$get_variables(), "not open")
    expect_error(ds$get_dimensions(), "not open")
    expect_error(ds$get_time_axis(), "not open")
    expect_error(ds$get_spatial_grid(), "not open")
    expect_error(ds$read_array("tas"), "not open")
})

test_that("EsgDataset errors on invalid index", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    expect_error(ds$file_inq(index = 2L))
    expect_error(ds$file_inq(index = 0L))
})
# }}}

# Print method {{{
test_that("EsgDataset print works when closed", {
    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)

    # cli output goes to messages, not stdout
    expect_no_error(expect_message(print(ds)))
})

test_that("EsgDataset print works when open", {
    skip_on_cran()

    tmp <- withr::local_tempfile(fileext = ".nc")
    create_test_nc(tmp)

    ds <- EsgDataset$new(tmp)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    expect_no_error(expect_message(print(ds)))
})
# }}}

