test_that("EsgDataset can be created", {
    skip_on_cran()
    skip_if_offline()

    index_node <- INDEX_NODES[["DKRZ"]]
    q <- esg_query(index_node)$
        activity_id("ScenarioMIP")$
        source_id("AWI-CM-1-1-MR")$
        frequency("day")$
        variable_id("tas")$
        experiment_id("ssp585")$
        variant_label("r1i1p1f1")$
        fields(c("source_id", "experiment_id", "frequency"))$
        limit(2)$
        collect()

    f <- q$collect(1)
    f$url[[1]]

    # Test single URL
    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)
})

test_that("EsgDataset can open and close connections", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)

    # Test open
    ds$open()
    expect_true(ds$is_open)

    # Test close
    ds$close()
    expect_false(ds$is_open)
})

test_that("EsgDataset basic layer methods work", {
    skip_on_cran()
    skip_if_offline()

    q <- esg_query(INDEX_NODES[["DKRZ"]])$
        activity_id("ScenarioMIP")$
        source_id("AWI-CM-1-1-MR")$
        frequency("day")$
        variable_id("tas")$
        experiment_id("ssp585")$
        variant_label("r1i1p1f1")$
        fields(c("source_id", "experiment_id", "frequency"))$
        limit(2)

    # can create a new result dataset from EsgQuery$collect
    expect_s3_class(datasets <- q$collect(), "EsgResultDataset")
    files <- datasets$collect(limit = 1)
    files$url
    ds <- files$open_dataset()

    ds <- EsgDataset$new(url)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    # Test file_inq
    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true("nvars" %in% names(info))
    expect_true("ndims" %in% names(info))

    # Test var_inq
    var_info <- ds$var_inq("tas")
    expect_type(var_info, "list")
    expect_equal(var_info$name, "tas")

    # Test dim_inq
    dim_info <- ds$dim_inq("time")
    expect_type(dim_info, "list")
    expect_equal(dim_info$name, "time")

    # Test att_get
    units <- ds$att_get("tas", "units")
    expect_type(units, "character")
})

test_that("EsgDataset middle layer methods work", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    # Test get_variables
    vars <- ds$get_variables()
    expect_type(vars, "character")
    expect_true(length(vars) > 0)
    expect_true("tas" %in% vars)

    # Test get_dimensions
    dims <- ds$get_dimensions()
    expect_type(dims, "character")
    expect_true("time" %in% dims)
    expect_true("lat" %in% dims)
    expect_true("lon" %in% dims)

    # Test get_time_axis
    time_info <- ds$get_time_axis()
    expect_type(time_info, "list")
    expect_true("values" %in% names(time_info))
    expect_true("units" %in% names(time_info))
    expect_true("calendar" %in% names(time_info))

    # Test get_spatial_grid
    grid <- ds$get_spatial_grid()
    expect_type(grid, "list")
    expect_true("lat" %in% names(grid))
    expect_true("lon" %in% names(grid))
})

test_that("EsgDataset can read variable data", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    # Test var_get with subset
    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5))
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 5, 5))
})

test_that("EsgDataset high layer methods work", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    # Test read_array
    arr <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5))
    expect_type(arr, "double")
    expect_equal(dim(arr), c(1, 5, 5))

    # Test read_data_table
    dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(1, 5, 5))
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
})

test_that("EsgDataset handles errors gracefully", {
    skip_on_cran()

    url <- "https://aims3.llnl.gov/thredds/dodsC/cmip6/CMIP6.ScenarioMIP.NCAR.CESM2.ssp585.r4i1p1f1.Amon.tas.gn.v20200528.tas_Amon_CESM2_ssp585_r4i1p1f1_gn_201501-206412.nc"

    ds <- EsgDataset$new(url)

    # Test methods on closed dataset
    expect_error(ds$file_inq(), "not open")
    expect_error(ds$var_inq("tas"), "not open")
    expect_error(ds$var_get("tas"), "not open")
})

test_that("EsgDataset print works", {
    skip_on_cran()

    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)

    expect_output(print(ds), "ESGF Query Dataset")
    expect_output(print(ds), "URLs: 1")
    expect_output(print(ds), "Status: Closed")
})

