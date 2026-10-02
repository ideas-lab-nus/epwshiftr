test_that("one acquisition reads linked sites and methods from sparse cells", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    on.exit(unlink(path), add = TRUE)
    dataset <- EsgDataset$new(path)
    dataset$open()
    on.exit(dataset$close(), add = TRUE)

    acquisition <- data.table::data.table(
        acquisition_id = "source-window",
        time_start = as.POSIXct("2060-01-02", tz = "UTC"),
        time_stop = as.POSIXct("2060-01-03 23:59:59", tz = "UTC")
    )
    consumers <- data.table::data.table(
        acquisition_id = rep.int("source-window", 3L),
        demand_id = 1:3,
        child_key = c("site-a-nearest", "site-a-mean", "site-b-nearest"),
        site_id = c("site-a", "site-a", "site-b"),
        role = rep.int("future", 3L),
        variable_id = rep.int("tas", 3L),
        lon = c(103.98, 103.98, -106),
        lat = c(1.37, 1.37, 41),
        spatial_method = c("nearest", "mean", "nearest"),
        time_start = rep(acquisition$time_start, 3L),
        time_stop = rep(acquisition$time_stop, 3L)
    )
    actual <- shift_batch__read_acquisition(dataset, acquisition, consumers)
    sources <- attr(actual, "grid_sources")
    slices <- attr(actual, "read_slices")

    expect_equal(data.table::uniqueN(actual$consumer_id), 3L)
    expect_equal(nrow(actual), 6L)
    expect_equal(nrow(slices), 5L)
    expect_true(all(slices$time_count == 2L))
    expect_equal(nrow(sources), 6L)
    expect_identical(unique(actual$cf_calendar), "360_day")
    for (index in seq_len(nrow(consumers))) {
        consumer <- consumers[index]
        expected <- dataset$read_region(
            "tas",
            lon = consumer$lon[[1L]],
            lat = consumer$lat[[1L]],
            time = c(consumer$time_start[[1L]], consumer$time_stop[[1L]]),
            method = consumer$spatial_method[[1L]]
        )
        observed <- actual[actual$consumer_id == as.character(index)]
        expect_equal(observed$value, expected$value)
        expect_identical(unique(observed$site_id), consumer$site_id[[1L]])
        expect_identical(unique(observed$child_key), consumer$child_key[[1L]])
    }
})

test_that("shared File planning retains four consumers for one source interval", {
    catalog <- data.table::data.table(
        file_key = "file-a",
        filename = "tas_day_Model-A_ssp245_r1i1p1f1_gn_20500101-20501231.nc",
        source_id = "Model-A",
        experiment_id = "ssp245",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "tracking-a",
        checksum = "checksum-a",
        checksum_type = "SHA256",
        size = 1000,
        datetime_start = "2050-01-01T00:00:00Z",
        datetime_end = "2050-12-31T23:59:59Z",
        url_opendap = "https://example.org/a.nc",
        url_download = "https://example.org/a.nc",
        data_node = "example.org"
    )
    consumers <- data.table::CJ(
        site_id = c("Harbin", "Guangzhou"),
        method = c("monthly", "daily"),
        sorted = FALSE
    )
    data.table::set(
        consumers,
        j = "child_key",
        value = paste(
            consumers$site_id,
            consumers$method
        )
    )
    data.table::set(consumers, j = "demand_id", value = consumers$child_key)
    for (column in c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "variable_id",
        "frequency",
        "table_id"
    )) {
        data.table::set(consumers, j = column, value = catalog[[column]][[1L]])
    }
    data.table::set(consumers, j = "role", value = "future")
    data.table::set(
        consumers,
        j = "time_start",
        value = as.POSIXct("2050-01-01", tz = "UTC")
    )
    data.table::set(
        consumers,
        j = "time_stop",
        value = as.POSIXct("2050-12-31", tz = "UTC")
    )
    data.table::set(consumers, j = "lon", value = c(126.6, 126.6, 113.3, 113.3))
    data.table::set(consumers, j = "lat", value = c(45.8, 45.8, 23.1, 23.1))
    data.table::set(consumers, j = "spatial_method", value = "nearest")

    original_catalog <- data.table::copy(catalog)
    original_consumers <- data.table::copy(consumers)
    plan <- shift_batch__shared_plan(catalog, consumers)

    expect_equal(nrow(plan$acquisitions), 1L)
    expect_equal(nrow(plan$consumers), 4L)
    expect_equal(data.table::uniqueN(plan$consumers$acquisition_id), 1L)
    expect_setequal(plan$consumers$child_key, consumers$child_key)
    expect_equal(nrow(plan$unmatched), 0L)
    expect_identical(catalog, original_catalog)
    expect_identical(consumers, original_consumers)

    # A separate file version must retain its own acquisition identity.
    another <- data.table::copy(catalog)
    data.table::set(another, j = "file_key", value = "file-b")
    data.table::set(another, j = "version", value = "v2")
    data.table::set(another, j = "checksum", value = "checksum-b")
    versions <- shift_batch__shared_plan(
        data.table::rbindlist(list(catalog, another)),
        consumers
    )
    expect_equal(nrow(versions$acquisitions), 2L)

    # Enclosed and touching windows share one acquisition; a gap stays apart.
    windows <- data.table::copy(consumers)
    data.table::set(
        windows,
        j = "time_start",
        value = as.POSIXct(
            c("2050-01-01", "2050-02-01", "2050-04-30", "2050-08-01"),
            tz = "UTC"
        )
    )
    data.table::set(
        windows,
        j = "time_stop",
        value = as.POSIXct(
            c("2050-04-30", "2050-02-28", "2050-05-31", "2050-08-31"),
            tz = "UTC"
        )
    )
    intervals <- shift_batch__shared_plan(catalog, windows)
    expect_equal(nrow(intervals$acquisitions), 2L)
    expect_equal(nrow(intervals$consumers), 4L)
    expect_setequal(
        as.character(intervals$acquisitions$time_stop),
        as.character(as.POSIXct(c("2050-05-31", "2050-08-31"), tz = "UTC"))
    )
})

test_that("shared File planning preserves gaps and missing demands", {
    catalog <- data.table::data.table(
        file_key = "file-a",
        filename = "tas_day_Model-A_ssp245_r1i1p1f1_gn_20500101-20501231.nc",
        source_id = "Model-A",
        experiment_id = "ssp245",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "tracking-a",
        checksum = "checksum-a",
        checksum_type = "SHA256",
        size = 1000,
        datetime_start = "2050-01-01T00:00:00Z",
        datetime_end = "2050-12-31T23:59:59Z",
        url_opendap = "https://example.org/a.nc",
        url_download = "https://example.org/a.nc",
        data_node = "example.org"
    )
    consumers <- data.table::data.table(
        demand_id = c("jan", "mar", "missing"),
        child_key = c("first", "second", "third"),
        site_id = c("A", "B", "C"),
        method = "daily",
        role = "future",
        source_id = "Model-A",
        experiment_id = "ssp245",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = c("tas", "tas", "huss"),
        frequency = "day",
        table_id = "day",
        time_start = as.POSIXct(
            c(
                "2050-01-01",
                "2050-03-01",
                "2050-01-01"
            ),
            tz = "UTC"
        ),
        time_stop = as.POSIXct(
            c(
                "2050-01-31",
                "2050-03-31",
                "2050-12-31"
            ),
            tz = "UTC"
        ),
        lon = 110,
        lat = 30,
        spatial_method = "nearest"
    )

    plan <- shift_batch__shared_plan(catalog, consumers)
    expect_equal(nrow(plan$acquisitions), 2L)
    expect_setequal(plan$consumers$demand_id, c("jan", "mar"))
    expect_identical(plan$unmatched$demand_id, "missing")
})

test_that("child demands retain site and historical source roles", {
    transform <- daily_transform("epwshiftr", reconstruction = "power")
    climate <- shift_cmip6(
        model = "EC-Earth3",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        grid = "gr"
    )
    periods <- shift__periods_from_input(list(mid = 2060))
    request <- shift__request_from_cmip6(climate, periods, transform)
    reference <- historical_reference(1995:2014)
    epw <- get_cache_epw()
    sites <- list(
        shift_site("Harbin", lon = 126.6, lat = 45.8, epw = epw),
        shift_site("Guangzhou", lon = 113.3, lat = 23.1, epw = epw)
    )
    children <- lapply(sites, function(site) {
        child <- shift_plan(
            request,
            site,
            periods,
            store = tempfile("shared-child-"),
            transform = transform,
            reference = reference
        )
        child@meta$climate <- climate
        child
    })
    manifest <- data.table::data.table(
        child_key = c("Harbin--daily", "Guangzhou--daily"),
        site_id = c("Harbin", "Guangzhou"),
        method = "epwshiftr"
    )

    demands <- shift_batch__consumers(children, manifest)
    expect_s3_class(demands, "data.table")
    expect_setequal(demands$site_id, manifest$site_id)
    expect_setequal(demands$role, c("future", "historical"))
    expect_setequal(demands$experiment_id, c("ssp585", "historical"))
    expect_equal(data.table::uniqueN(demands$demand_id), nrow(demands))
    expect_equal(data.table::uniqueN(demands$child_key), 2L)
    expect_true(all(demands$spatial_method == "nearest"))

    # The discovery store is read locally and each site keeps its own link.
    discovery <- tempfile("shared-discovery-")
    store <- EsgStore$new(discovery)
    params <- query_param__as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
    response <- esgf_fixture_response("file-success.json")
    response$response$docs$checksum <- "checksum-a"
    response$response$docs$checksum_type <- "SHA256"
    response$response$docs$title <- response$response$docs$id
    response$response$docs$variant_label <- "r1i1p1f1"
    files <- query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = params,
        result = response
    )
    store$add_files(files)
    store$close()
    plan <- shift_batch__plan_from_discovery(children, manifest, discovery)
    expect_equal(nrow(plan$acquisitions), 1L)
    expect_equal(nrow(plan$consumers), 2L)
    expect_setequal(plan$consumers$site_id, manifest$site_id)
    expect_true(nrow(plan$unmatched) > 0L)
})

test_that("empty child collection creates an empty local plan", {
    consumers <- shift_batch__consumers(list(), data.table::data.table())
    plan <- shift_batch__shared_plan(data.table::data.table(), consumers)
    expect_s3_class(consumers, "data.table")
    expect_equal(nrow(plan$acquisitions), 0L)
    expect_equal(nrow(plan$consumers), 0L)
    expect_equal(nrow(plan$unmatched), 0L)
})
