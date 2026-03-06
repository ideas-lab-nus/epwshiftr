# q <- esg_query(INDEX_NODES[["ORNL"]])$
#      activity_id("ScenarioMIP")$
#      source_id("AWI-CM-1-1-MR")$
#      frequency("day")$
#      variable_id("tas")$
#      experiment_id("ssp585")$
#      variant_label("r1i1p1f1")$
#      fields(c("source_id", "experiment_id", "frequency"))$
#      limit(2)$
#      collect()
#
# f <- q$collect(which(q$has_opendap()))
# url_nc <- f$url_opendap[1]
# url_nc2 <- f$url_opendap[2]
# this url is obtained from the query above and is used in multiple tests, so we define it here for reuse
url_nc <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20250101-20251231.nc"
url_nc2 <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20300101-20301231.nc"

test_that("EsgDataset can work with a single file", {
    skip_on_cran()
    skip_if_offline()

    # Test single URL
    ds <- EsgDataset$new(url_nc)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)

    # Test open
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    expect_true(ds$is_open)

    # Test close
    ds$close()
    expect_false(ds$is_open)

    # Test file_inq
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)
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

    # Test var_get with subset
    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)
    expect_type(data, "double")
    expect_equal(dim(data), c(5, 5))
    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = FALSE)
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 5, 5))

    # Test read_array
    arr_list <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)
    expect_type(arr_list, "list")
    expect_length(arr_list, 1L)
    arr <- arr_list[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(5, 5))

    arr_list <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = FALSE)
    expect_type(arr_list, "list")
    expect_length(arr_list, 1L)
    arr <- arr_list[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(1, 5, 5))

    # Test read_data_table
    start <- c(1, 1, 1)
    count <- c(1, 5, 5)
    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 1L)
    dt <- dt_list[[1L]]
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    expect_equal(nrow(dt), prod(count))

    vinfo <- ds$var_inq("tas")
    dim_names <- vapply(vinfo$dimids, function(id) ds$dim_inq(id)$name, character(1L))
    expect_true(all(dim_names %in% names(dt)))
    for (j in seq_along(dim_names)) {
        nm <- dim_names[[j]]
        coord <- ds$var_get(nm, start = start[[j]], count = count[[j]], collapse = TRUE)
        expect_equal(sort(unique(dt[[nm]])), sort(as.vector(coord)))
    }

    ds$close()
    expect_false(ds$is_open)
})

test_that("EsgDataset can work with multiple files", {
    skip_on_cran()
    skip_if_offline()

    # Test multiple URLs
    ds <- EsgDataset$new(c(url_nc, url_nc2))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)

    # Test open
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)

    # Test get_time_axis
    time_info <- ds$get_time_axis(2)
    expect_type(time_info, "list")
    expect_true("values" %in% names(time_info))
    expect_true("units" %in% names(time_info))
    expect_true("calendar" %in% names(time_info))

    # Test read_data_table
    start <- c(1, 1, 1)
    count <- c(1, 5, 5)

    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(dt_list, function(x) inherits(x, "data.table"), logical(1L))))

    # Coordinate values should be interpreted within each file's context
    vinfo <- ds$var_inq("tas")
    dim_names <- vapply(vinfo$dimids, function(id) ds$dim_inq(id)$name, character(1L))
    for (i in 1:2) {
        dti <- dt_list[[i]]
        for (j in seq_along(dim_names)) {
            nm <- dim_names[[j]]
            coord <- ds$var_get(nm, start = start[[j]], count = count[[j]], index = i, collapse = TRUE)
            expect_equal(sort(unique(dti[[nm]])), sort(as.vector(coord)))
        }
    }

    # Optionally row-bind with file_index
    dt_all <- ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    expect_s3_class(dt_all, "data.table")
    expect_true("file_index" %in% names(dt_all))
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
})

test_that("EsgDataset handles errors gracefully", {
    skip_on_cran()

    ds <- EsgDataset$new(url_nc)

    # Test methods on closed dataset
    expect_error(ds$file_inq(), "not open")
    expect_error(ds$var_inq("tas"), "not open")
    expect_error(ds$var_get("tas"), "not open")
})

test_that("EsgDataset print works", {
    skip_on_cran()

    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)

    msg <- capture.output(print(ds), type = "message")
    expect_true(any(grepl("ESGF Dataset", msg, fixed = TRUE)))
    expect_true(any(grepl("URLs: 1", msg, fixed = TRUE)))
    expect_true(any(grepl("Status: Closed", msg, fixed = TRUE)))
})
