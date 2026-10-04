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
        time_stop = as.POSIXct("2060-01-03 23:59:59", tz = "UTC"),
        url_opendap = path,
        url_download = path
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
    actual <- shift_batch_read__read_acquisition(
        dataset,
        acquisition,
        consumers
    )
    sources <- attr(actual, "grid_sources")
    slices <- attr(actual, "read_slices")

    expect_equal(data.table::uniqueN(actual$consumer_id), 3L)
    expect_equal(nrow(actual), 6L)
    expect_equal(nrow(slices), 2L)
    expect_true(any(slices$lat_count * slices$lon_count == 4L))
    expect_true(all(slices$time_count == 2L))
    expect_equal(nrow(sources), 6L)
    expect_identical(unique(actual$cf_calendar), "360_day")
    other_path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(other_path, 2060L, calendar = "360_day")
    on.exit(unlink(other_path), add = TRUE)
    other <- EsgDataset$new(other_path)
    other$open()
    on.exit(other$close(), add = TRUE)
    expect_error(
        shift_batch_read__read_acquisition(other, acquisition, consumers),
        "does not match the acquisition source"
    )
    empty_acquisition <- data.table::copy(acquisition)
    data.table::set(
        empty_acquisition,
        j = "time_start",
        value = as.POSIXct("2062-01-01", tz = "UTC")
    )
    data.table::set(
        empty_acquisition,
        j = "time_stop",
        value = as.POSIXct("2062-01-02", tz = "UTC")
    )
    empty <- shift_batch_read__read_acquisition(
        dataset,
        empty_acquisition,
        consumers
    )
    expect_equal(nrow(empty), 0L)
    expect_true(all(
        c("consumer_id", "site_id", "demand_id", "child_key", "role") %in%
            names(empty)
    ))
    expect_true(all(
        c("consumer_id", "site_id", "demand_id", "child_key", "role") %in%
            names(attr(empty, "grid_sources"))
    ))
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
    plan <- shift_batch_plan__shared_plan(catalog, consumers)

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
    versions <- shift_batch_plan__shared_plan(
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
    intervals <- shift_batch_plan__shared_plan(catalog, windows)
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

    plan <- shift_batch_plan__shared_plan(catalog, consumers)
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
    periods <- shift_spec__periods_from_input(list(mid = 2060))
    request <- shift_spec__request_from_cmip6(climate, periods, transform)
    reference <- historical_reference(1995:2014)
    epw <- get_cache_epw()
    sites <- list(
        shift_site("Harbin", lon = 126.6, lat = 45.8, epw = epw),
        shift_site("Guangzhou", lon = 113.3, lat = 23.1, epw = epw)
    )
    # lapply callback {{{
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
    # }}}
    manifest <- data.table::data.table(
        child_key = c("Harbin--daily", "Guangzhou--daily"),
        site_id = c("Harbin", "Guangzhou"),
        method = "epwshiftr"
    )

    demands <- shift_batch_plan__consumers(children, manifest)
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
    plan <- shift_batch_plan__plan_from_discovery(children, manifest, discovery)
    expect_equal(nrow(plan$acquisitions), 1L)
    expect_equal(nrow(plan$consumers), 2L)
    expect_setequal(plan$consumers$site_id, manifest$site_id)
    expect_true(nrow(plan$unmatched) > 0L)
    # The batch writer and an independently collected child File row must
    # resolve to the same extraction cache key.
    cache_plan <- data.table::data.table(
        variable_id = plan$consumers$variable_id[[1L]],
        lon = plan$consumers$lon[[1L]],
        lat = plan$consumers$lat[[1L]],
        method = plan$consumers$spatial_method[[1L]],
        time_start = plan$consumers$requested_start[[1L]],
        time_stop = plan$consumers$requested_stop[[1L]]
    )
    store <- EsgStore$new(discovery, create = FALSE)
    child_file <- store$query("SELECT * FROM file_catalog LIMIT 1")
    store$close()
    expect_identical(
        store__extract_cache_path(cache_plan, plan$acquisitions[1L]),
        store__extract_cache_path(cache_plan, child_file[1L])
    )
})

test_that("empty child collection creates an empty local plan", {
    consumers <- shift_batch_plan__consumers(list(), data.table::data.table())
    plan <- shift_batch_plan__shared_plan(data.table::data.table(), consumers)
    expect_s3_class(consumers, "data.table")
    expect_equal(nrow(plan$acquisitions), 0L)
    expect_equal(nrow(plan$consumers), 0L)
    expect_equal(nrow(plan$unmatched), 0L)
})

test_that("batch windows seed child caches and resume verified native reads", {
    cache_dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        path,
        2060L,
        calendar = "360_day",
        n_years = 24L
    )
    on.exit(unlink(path), add = TRUE)
    acquisition <- data.table::data.table(
        acquisition_id = "twenty-four-year-window",
        physical_file_id = "local-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20831230.nc",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "local-source",
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        size = file.info(path)$size,
        time_start = as.POSIXct("2060-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2083-12-31 23:59:59", tz = "UTC"),
        url_opendap = path,
        url_download = path
    )
    consumers <- data.table::data.table(
        acquisition_id = "twenty-four-year-window",
        demand_id = 1:2,
        child_key = c("first", "second"),
        site_id = c("Harbin", "Guangzhou"),
        role = "future",
        variable_id = "tas",
        lon = c(-106, 103.98),
        lat = c(41, 1.37),
        spatial_method = "nearest",
        time_start = acquisition$time_start[[1L]],
        time_stop = acquisition$time_stop[[1L]],
        requested_start = acquisition$time_start[[1L]],
        requested_stop = acquisition$time_stop[[1L]]
    )
    batch_root <- withr::local_tempdir()
    no_checksum <- data.table::copy(acquisition)
    data.table::set(no_checksum, j = "checksum", value = NA_character_)
    expect_error(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            no_checksum,
            consumers
        ),
        "checksum"
    )
    no_dap <- data.table::copy(acquisition)
    data.table::set(no_dap, j = "url_opendap", value = NA_character_)
    data.table::set(
        no_dap,
        j = "url_download",
        value = "https://example.org/whole-file.nc"
    )
    expect_error(
        shift_batch_window__prefetch_acquisition(batch_root, no_dap, consumers),
        "OPeNDAP endpoint or a local file"
    )
    expect_gt(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        1L
    )
    dataset <- EsgDataset$new(path)
    dataset$open()
    on.exit(dataset$close(), add = TRUE)
    for (index in seq_len(nrow(consumers))) {
        consumer <- consumers[index]
        cache_plan <- data.table::data.table(
            variable_id = "tas",
            lon = consumer$lon[[1L]],
            lat = consumer$lat[[1L]],
            method = "nearest",
            time_start = consumer$requested_start[[1L]],
            time_stop = consumer$requested_stop[[1L]]
        )
        cache <- store__extract_cache_read(
            store__extract_cache_path(cache_plan, acquisition)
        )
        expected <- dataset$read_region(
            "tas",
            consumer$lon[[1L]],
            consumer$lat[[1L]],
            time = c(cache_plan$time_start, cache_plan$time_stop)
        )
        expect_equal(cache$data$value, expected$value)
        expect_equal(cache$available_time_count, 8640L)
        expect_identical(unique(cache$data$cf_calendar), "360_day")
        expect_equal(
            cache$grid_sources$grid_lon,
            attr(expected, "grid_sources")$grid_lon
        )
        expect_identical(
            cache$grid_sources$role,
            attr(expected, "grid_sources")$role
        )
    }
    # Complete child caches skip even the source metadata open.
    expect_identical(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        0L
    )
    receipt <- list.files(
        file.path(batch_root, "shared-acquisitions"),
        pattern = "[.]json$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_gt(length(receipt), 1L)
    cache_plan <- data.table::data.table(
        variable_id = "tas",
        lon = consumers$lon[[1L]],
        lat = consumers$lat[[1L]],
        method = "nearest",
        time_start = consumers$requested_start[[1L]],
        time_stop = consumers$requested_stop[[1L]]
    )
    unlink(store__extract_cache_path(cache_plan, acquisition))
    other_plan <- data.table::data.table(
        variable_id = "tas",
        lon = consumers$lon[[2L]],
        lat = consumers$lat[[2L]],
        method = "nearest",
        time_start = consumers$requested_start[[2L]],
        time_stop = consumers$requested_stop[[2L]]
    )
    unlink(store__extract_cache_path(other_plan, acquisition))
    # Recovery reads only the verified windows and reconstructs the missing
    # site cache without issuing another source-value request.
    testthat::local_mocked_bindings(
        # shift_batch_read__read_acquisition {{{
        shift_batch_read__read_acquisition = function(...) {
            stop("Unexpected source-value read")
        }
        # }}}
    )
    expect_gt(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        1L
    )
    expect_true(file.exists(store__extract_cache_path(
        cache_plan,
        acquisition
    )))
    # A complete receipt and its native-axis summary also rebuild a missing
    # site cache while the original source is unavailable.
    unlink(store__extract_cache_path(cache_plan, acquisition))
    unavailable <- data.table::copy(acquisition)
    data.table::set(
        unavailable,
        j = "url_opendap",
        value = "https://unavailable.example/source.nc"
    )
    expect_gt(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            unavailable,
            consumers
        ),
        1L
    )
    expect_true(file.exists(store__extract_cache_path(
        cache_plan,
        acquisition
    )))
    # A readable but altered RDS must fail its receipt check instead of being
    # accepted as an interrupted or reusable window.
    unlink(store__extract_cache_path(cache_plan, acquisition))
    unlink(store__extract_cache_path(other_plan, acquisition))
    window_path <- list.files(
        sub("[.]json$", "", receipt[[1L]]),
        pattern = "[.]rds$",
        full.names = TRUE
    )[[1L]]
    cat("tampered", file = window_path, append = TRUE)
    expect_error(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        "checksum"
    )
})

# A partial recovery reads source values only for missing site cache keys and
# retains an earlier completed receipt when that window must be extended.
test_that("partial site recovery excludes cached consumers", {
    withr::local_options(list(epwshiftr.dir_cache = withr::local_tempdir()))
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    on.exit(unlink(path), add = TRUE)
    acquisition <- data.table::data.table(
        acquisition_id = "partial-sites",
        physical_file_id = "partial-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc",
        variable_id = "tas",
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        time_start = as.POSIXct("2060-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2060-01-30 23:59:59", tz = "UTC"),
        url_opendap = path,
        url_download = path
    )
    consumers <- data.table::data.table(
        acquisition_id = "partial-sites",
        demand_id = 1:2,
        child_key = c("first", "second"),
        site_id = c("first", "second"),
        role = "future",
        variable_id = "tas",
        lon = c(-106, 103.98),
        lat = c(41, 1.37),
        spatial_method = "nearest",
        time_start = rep(acquisition$time_start, 2L),
        time_stop = rep(acquisition$time_stop, 2L),
        requested_start = rep(acquisition$time_start, 2L),
        requested_stop = rep(acquisition$time_stop, 2L)
    )
    root <- withr::local_tempdir()
    expect_equal(
        shift_batch_window__prefetch_acquisition(root, acquisition, consumers),
        1L
    )
    plan <- data.table::data.table(
        variable_id = "tas",
        lon = consumers$lon[[1L]],
        lat = consumers$lat[[1L]],
        method = "nearest",
        time_start = acquisition$time_start,
        time_stop = acquisition$time_stop
    )
    unlink(store__extract_cache_path(plan, acquisition))
    receipt <- list.files(
        file.path(root, "shared-acquisitions"),
        pattern = "[.]json$",
        recursive = TRUE,
        full.names = TRUE
    )[[1L]]
    unlink(receipt)
    original_read <- shift_batch_read__read_acquisition
    counts <- integer()
    testthat::local_mocked_bindings(
        # shift_batch_read__read_acquisition {{{
        shift_batch_read__read_acquisition = function(
            dataset,
            acquisition,
            consumers
        ) {
            counts <<- c(counts, nrow(consumers))
            original_read(dataset, acquisition, consumers)
        }
        # }}}
    )
    expect_equal(
        shift_batch_window__prefetch_acquisition(root, acquisition, consumers),
        1L
    )
    expect_identical(counts, 1L)
    expect_true(file.exists(store__extract_cache_path(plan, acquisition)))
    expect_true(any(grepl(
        "-interrupted-",
        list.files(
            dirname(sub("[.]json$", "", receipt)),
            all.files = TRUE
        )
    )))
    # Distinct method children with the same native source demand share one
    # method-neutral cache entry and one recoverable window chunk.
    withr::local_options(list(epwshiftr.dir_cache = withr::local_tempdir()))
    duplicate <- consumers[1L][rep(1L, 2L)]
    data.table::set(duplicate, j = "demand_id", value = 3:4)
    data.table::set(
        duplicate,
        j = "child_key",
        value = c("method-a", "method-b")
    )
    other_root <- withr::local_tempdir()
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            other_root,
            acquisition,
            duplicate
        ),
        1L
    )
    other_receipt <- list.files(
        file.path(other_root, "shared-acquisitions"),
        pattern = "[.]json$",
        recursive = TRUE,
        full.names = TRUE
    )[[1L]]
    expect_length(jsonlite::read_json(other_receipt)$chunks, 1L)
})

test_that("shared acquisition partitions large site collections", {
    withr::local_options(list(epwshiftr.dir_cache = withr::local_tempdir()))
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    on.exit(unlink(path), add = TRUE)
    acquisition <- data.table::data.table(
        acquisition_id = "large-site-collection",
        physical_file_id = "large-local-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "large-local-source",
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        size = file.info(path)$size,
        time_start = as.POSIXct("2060-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2060-01-30 23:59:59", tz = "UTC"),
        url_opendap = path,
        url_download = path
    )
    site_count <- 257L
    consumers <- data.table::data.table(
        acquisition_id = rep.int(acquisition$acquisition_id, site_count),
        demand_id = seq_len(site_count),
        child_key = sprintf("child-%03d", seq_len(site_count)),
        site_id = sprintf("site-%03d", seq_len(site_count)),
        role = rep.int("future", site_count),
        variable_id = rep.int("tas", site_count),
        lon = seq(103.5, 104.5, length.out = site_count),
        lat = rep.int(1.37, site_count),
        spatial_method = rep.int("nearest", site_count),
        time_start = rep(acquisition$time_start, site_count),
        time_stop = rep(acquisition$time_stop, site_count),
        requested_start = rep(acquisition$time_start, site_count),
        requested_stop = rep(acquisition$time_stop, site_count)
    )
    batch_root <- withr::local_tempdir()
    original_dataset <- EsgDataset
    opens <- 0L
    testthat::local_mocked_bindings(
        # new {{{
        EsgDataset = list(new = function(...) {
            opens <<- opens + 1L
            original_dataset$new(...)
        })
        # }}}
    )
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        2L
    )
    expect_identical(opens, 1L)
    receipts <- list.files(
        file.path(batch_root, "shared-acquisitions"),
        pattern = "[.]json$",
        recursive = TRUE
    )
    expect_length(receipts, 2L)
    for (index in c(1L, site_count)) {
        consumer <- consumers[index]
        plan <- data.table::data.table(
            variable_id = "tas",
            lon = consumer$lon,
            lat = consumer$lat,
            method = "nearest",
            time_start = consumer$time_start,
            time_stop = consumer$time_stop
        )
        payload <- store__extract_cache_read(
            store__extract_cache_path(plan, acquisition)
        )
        expect_equal(nrow(payload$data), 30L)
    }
    expect_identical(
        shift_batch_window__prefetch_acquisition(
            batch_root,
            acquisition,
            consumers
        ),
        0L
    )
    expect_identical(opens, 1L)
})

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
