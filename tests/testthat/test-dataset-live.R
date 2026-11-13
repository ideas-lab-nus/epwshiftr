# Use a real loopback DAP2 server so the transport tests are independent
# of public data hosts. The dedicated ESGF service tests retain live coverage.
test_that("EsgDataset$new() / EsgDataset$open() / EsgDataset$file_inq() / EsgDataset$var_inq() / EsgDataset$dim_inq() / EsgDataset$att_get() / EsgDataset$get_variables() / EsgDataset$get_dimensions() / EsgDataset$get_time_axis() / EsgDataset$get_spatial_grid() / EsgDataset$var_get() / EsgDataset$read_array() / EsgDataset$read_data_table() loopback OPeNDAP single-file smoke", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(
        subset_test__dap_app(calendar = "standard")
    )
    on.exit(server$stop(), add = TRUE)

    ds <- EsgDataset$new(paste0(server$url(), "fast.nc"))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)

    ds$open()
    expect_true(ds$is_open)

    ds$close()
    expect_false(ds$is_open)

    ds$open()
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

    data <- ds$var_get(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = TRUE
    )
    expect_type(data, "double")
    expect_equal(dim(data), c(2, 2))

    data <- ds$var_get(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = FALSE
    )
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 2, 2))

    arr <- ds$read_array(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = TRUE
    )[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(2, 2))

    dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(1, 2, 2))[[
        1L
    ]]
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    expect_equal(nrow(dt), 4L)
})

test_that("EsgDataset$new() / EsgDataset$open() / EsgDataset$get_time_axis() / EsgDataset$read_data_table() loopback OPeNDAP multi-file smoke", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(
        subset_test__dap_app(calendar = "standard")
    )
    on.exit(server$stop(), add = TRUE)

    ds <- EsgDataset$new(paste0(server$url(), c("fast.nc", "june.nc")))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)

    ds$open()
    on.exit(ds$close(), add = TRUE)

    expect_s3_class(ds$get_time_axis(2)$values, "POSIXct")

    start <- c(1, 1, 1)
    count <- c(1, 2, 2)
    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(
        dt_list,
        function(x) inherits(x, "data.table"),
        logical(1L)
    )))

    dt_all <- ds$read_data_table(
        "tas",
        start = start,
        count = count,
        rbind = TRUE
    )
    expect_s3_class(dt_all, "data.table")
    expect_true("file_index" %in% names(dt_all))
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
    expect_equal(nrow(dt_all), 8L)
})

# vim: fdm=marker :
