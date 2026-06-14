live_dataset_url_nc <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20250101-20251231.nc"
live_dataset_url_nc2 <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20300101-20301231.nc"

test_that("EsgDataset live OPeNDAP single-file smoke", {
    skip_live_esgf()

    ds <- EsgDataset$new(live_dataset_url_nc)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)

    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open live OPeNDAP dataset:", e$message))
    })
    expect_true(ds$is_open)

    ds$close()
    expect_false(ds$is_open)

    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open live OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)

    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true("nvars" %in% names(info))
    expect_true("ndims" %in% names(info))

    var_info <- ds$var_inq("tas")
    expect_type(var_info, "list")
    expect_equal(var_info$name, "tas")

    dim_info <- ds$dim_inq("time")
    expect_type(dim_info, "list")
    expect_equal(dim_info$name, "time")

    expect_type(ds$att_get("tas", "units"), "character")
    expect_true("tas" %in% ds$get_variables())
    expect_true(all(c("time", "lat", "lon") %in% ds$get_dimensions()))
    expect_s3_class(ds$get_time_axis()$values, "POSIXct")

    grid <- ds$get_spatial_grid()
    expect_true(all(c("lat", "lon") %in% names(grid)))

    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)
    expect_type(data, "double")
    expect_equal(dim(data), c(5, 5))

    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = FALSE)
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 5, 5))

    arr <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(5, 5))

    dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(1, 5, 5))[[1L]]
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    expect_equal(nrow(dt), 25L)
})

test_that("EsgDataset live OPeNDAP multi-file smoke", {
    skip_live_esgf()

    ds <- EsgDataset$new(c(live_dataset_url_nc, live_dataset_url_nc2))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)

    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open live OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)

    expect_s3_class(ds$get_time_axis(2)$values, "POSIXct")

    start <- c(1, 1, 1)
    count <- c(1, 5, 5)
    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(dt_list, function(x) inherits(x, "data.table"), logical(1L))))

    dt_all <- ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    expect_s3_class(dt_all, "data.table")
    expect_true("file_index" %in% names(dt_all))
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
})
