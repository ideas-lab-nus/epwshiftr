# Build a local 360-day CMIP6 NetCDF fixture, matching acquisition row, and
# consumer table for shared-window prefetch and recovery tests.
# window_test__fixture {{{
window_test__fixture <- function(
    acquisition_id,
    physical_file_id,
    filename,
    time_stop,
    n_years = 1L,
    site_ids = c("Harbin", "Guangzhou"),
    child_keys = c("first", "second"),
    lon = c(-106, 103.98),
    lat = c(41, 1.37),
    env = parent.frame()
) {
    withr::local_options(
        list(epwshiftr.dir_cache = withr::local_tempdir(.local_envir = env)),
        .local_envir = env
    )
    path <- withr::local_tempfile(fileext = ".nc", .local_envir = env)
    write_local_cmip6_netcdf_fixture(
        path,
        2060L,
        calendar = "360_day",
        n_years = n_years
    )
    time_start <- as.POSIXct("2060-01-01", tz = "UTC")
    acquisition <- data.table::data.table(
        acquisition_id = acquisition_id,
        physical_file_id = physical_file_id,
        filename = filename,
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = physical_file_id,
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        size = file.info(path)$size,
        time_start = time_start,
        time_stop = time_stop,
        url_opendap = path,
        url_download = path
    )
    n_sites <- length(site_ids)
    consumers <- data.table::data.table(
        acquisition_id = rep.int(acquisition_id, n_sites),
        demand_id = seq_len(n_sites),
        child_key = child_keys,
        site_id = site_ids,
        role = rep.int("future", n_sites),
        variable_id = rep.int("tas", n_sites),
        lon = lon,
        lat = lat,
        spatial_method = rep.int("nearest", n_sites),
        time_start = rep(time_start, n_sites),
        time_stop = rep(time_stop, n_sites),
        requested_start = rep(time_start, n_sites),
        requested_stop = rep(time_stop, n_sites)
    )
    list(
        path = path,
        acquisition = acquisition,
        consumers = consumers,
        root = withr::local_tempdir(.local_envir = env)
    )
}
# }}}

test_that("one acquisition reads linked sites and methods from sparse cells", {
    path <- withr::local_tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
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

    other_path <- withr::local_tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(other_path, 2060L, calendar = "360_day")
    other <- EsgDataset$new(other_path)
    other$open()
    on.exit(other$close(), add = TRUE)
    expect_error(
        shift_batch_read__read_acquisition(other, acquisition, consumers),
        "does not match the acquisition source"
    )

    empty_acquisition <- data.table::copy(acquisition)
    empty_acquisition[, `:=`(
        time_start = as.POSIXct("2062-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2062-01-02", tz = "UTC")
    )]
    empty <- shift_batch_read__read_acquisition(
        dataset,
        empty_acquisition,
        consumers
    )
    expect_equal(nrow(empty), 0L)
    expected_cols <- c(
        "consumer_id",
        "site_id",
        "demand_id",
        "child_key",
        "role"
    )
    expect_true(all(expected_cols %in% names(empty)))
    expect_true(all(expected_cols %in% names(attr(empty, "grid_sources"))))

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

test_that("batch windows seed child caches and resume verified native reads", {
    skip_on_cran()
    fx <- window_test__fixture(
        acquisition_id = "twenty-four-year-window",
        physical_file_id = "local-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20831230.nc",
        time_stop = as.POSIXct("2083-12-31 23:59:59", tz = "UTC"),
        n_years = 24L
    )
    acquisition <- fx$acquisition
    consumers <- fx$consumers
    batch_root <- fx$root

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
    no_dap[, `:=`(
        url_opendap = NA_character_,
        url_download = "https://example.org/whole-file.nc"
    )]
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
    dataset <- EsgDataset$new(fx$path)
    dataset$open()
    on.exit(dataset$close(), add = TRUE)
    for (index in seq_len(nrow(consumers))) {
        consumer <- consumers[index]
        cache_plan <- shift_test__cache_plan(consumer)
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
    receipt <- shift_test__shared_receipts(batch_root)
    expect_gt(length(receipt), 1L)
    cache_plan <- shift_test__cache_plan(consumers[1L])
    other_plan <- shift_test__cache_plan(consumers[2L])
    unlink(store__extract_cache_path(cache_plan, acquisition))
    unlink(store__extract_cache_path(other_plan, acquisition))
    # Recovery reads only the verified windows and reconstructs the missing
    # site cache without issuing another source-value request.
    testthat::local_mocked_bindings(
        shift_batch_read__read_acquisition = function(...) {
            stop("Unexpected source-value read")
        }
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
    skip_on_cran()
    fx <- window_test__fixture(
        acquisition_id = "partial-sites",
        physical_file_id = "partial-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc",
        time_stop = as.POSIXct("2060-01-30 23:59:59", tz = "UTC"),
        site_ids = c("first", "second")
    )
    acquisition <- fx$acquisition
    consumers <- fx$consumers
    root <- fx$root

    expect_equal(
        shift_batch_window__prefetch_acquisition(root, acquisition, consumers),
        1L
    )
    plan <- shift_test__cache_plan(consumers[1L])
    unlink(store__extract_cache_path(plan, acquisition))
    receipt <- shift_test__shared_receipts(root)[[1L]]
    unlink(receipt)
    original_read <- shift_batch_read__read_acquisition
    counts <- integer()
    testthat::local_mocked_bindings(
        shift_batch_read__read_acquisition = function(
            dataset,
            acquisition,
            consumers
        ) {
            counts <<- c(counts, nrow(consumers))
            original_read(dataset, acquisition, consumers)
        }
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
    duplicate[, `:=`(
        demand_id = 3:4,
        child_key = c("method-a", "method-b")
    )]
    other_root <- withr::local_tempdir()
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            other_root,
            acquisition,
            duplicate
        ),
        1L
    )
    other_receipt <- shift_test__shared_receipts(other_root)[[1L]]
    expect_length(jsonlite::read_json(other_receipt)$chunks, 1L)
})

test_that("shared acquisition partitions large site collections", {
    skip_on_cran()
    site_count <- 257L
    fx <- window_test__fixture(
        acquisition_id = "large-site-collection",
        physical_file_id = "large-local-source",
        filename = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601230.nc",
        time_stop = as.POSIXct("2060-01-30 23:59:59", tz = "UTC"),
        site_ids = sprintf("site-%03d", seq_len(site_count)),
        child_keys = sprintf("child-%03d", seq_len(site_count)),
        lon = seq(103.5, 104.5, length.out = site_count),
        lat = rep.int(1.37, site_count)
    )
    acquisition <- fx$acquisition
    consumers <- fx$consumers
    batch_root <- fx$root

    original_dataset <- EsgDataset
    opens <- 0L
    testthat::local_mocked_bindings(
        EsgDataset = list(new = function(...) {
            opens <<- opens + 1L
            original_dataset$new(...)
        })
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
    expect_length(
        shift_test__shared_receipts(batch_root, full.names = FALSE),
        2L
    )
    for (index in c(1L, site_count)) {
        plan <- shift_test__cache_plan(consumers[index])
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

test_that("prefetch excludes completed and active consumers", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    seen <- character()
    local_mocked_bindings(
        shift_status = function(child, ...) {
            if (child@meta$site@id == "one") "completed" else "planned"
        },
        source__apply = function(jobs, ...) {
            seen <<- unlist(lapply(jobs, function(job) {
                job$consumers$site_id[!job$cached]
            }))
        }
    )
    shift_batch_window__prefetch(batch)
    expect_true(length(seen) > 0L)
    expect_identical(unique(seen), "two")
    local_mocked_bindings(shift_status = function(...) "running")
    seen <- character()
    shift_batch_window__prefetch(batch)
    expect_length(seen, 0L)
})

test_that("shared failures retain ownership and do not stop other files", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    seen <- character()
    first <- batch@meta$shared_plan$acquisitions$acquisition_id[[1L]]
    local_mocked_bindings(source__read_acquisition = function(job) {
        id <- job$acquisition$acquisition_id[[1L]]
        seen <<- c(seen, id)
        if (id == first) {
            stop("unavailable source")
        }
        1L
    })
    result <- shift_batch_window__prefetch(batch)
    expect_length(seen, 6L)
    expect_equal(as.integer(result), 5L)
    expect_length(attr(result, "failures"), 1L)
    expect_setequal(
        attr(result, "failures")[[1L]]$child_keys,
        names(batch@meta$children)
    )
})

test_that("only dependent children are blocked after shared reading", {
    fixture <- shared_inputs_test__fixture()
    batch <- fixture$batch
    first <- names(batch@meta$children)[[1L]]
    started <- character()
    local_mocked_bindings(
        shift_batch_plan__resolve_inputs = function(batch, reporter = NULL) {
            batch
        },
        shift_batch_window__prefetch = function(...) {
            structure(
                1L,
                failures = list(list(
                    file = "failed.nc",
                    message = "unavailable",
                    child_keys = first
                ))
            )
        },
        shift_run__run_one = function(child, ...) {
            started <<- c(started, child@meta$site@id)
            child
        },
        shift_batch_ui__report = function(...) NULL
    )
    result <- shift_batch__execute(batch, ui = shift_ui("none"))
    expect_identical(started, "two")
    expect_identical(result@meta[["shared_failure"]]$child_keys, first)
    expect_identical(result@meta$execution$action, c("blocked", "started"))
})

test_that("warm shared caches need no worker or payload deserialization", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    shift_batch_window__prefetch(batch)
    unlink(list.files(fixture$root, pattern = "[.]nc$", full.names = TRUE))
    local_mocked_bindings(
        store__extract_cache_read = function(...) {
            stop("unexpected payload read")
        },
        source__apply = function(jobs, ...) expect_length(jobs, 0L)
    )
    expect_equal(as.integer(shift_batch_window__prefetch(batch)), 0L)
})

# Filtering finished children must keep the original window identity. Their
# missing caches are irrelevant, while unfinished consumers can reuse receipts.
test_that("partial resume keeps window identity without completed cache reads", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    shift_batch_window__prefetch(batch)
    paths <- unlist(lapply(
        seq_len(nrow(batch@meta$shared_plan$acquisitions)),
        function(i) {
            acquisition <- batch@meta$shared_plan$acquisitions[i]
            consumers <- batch@meta$shared_plan$consumers[
                batch@meta$shared_plan$consumers$acquisition_id ==
                    acquisition$acquisition_id
            ]
            shift_batch_window__cache_paths(acquisition, consumers)
        }
    ))
    unlink(paths)
    unlink(list.files(fixture$root, pattern = "[.]nc$", full.names = TRUE))
    local_mocked_bindings(shift_status = function(child, ...) {
        if (child@meta$site@id == "one") "completed" else "planned"
    })
    result <- shift_batch_window__prefetch(batch)
    expect_length(attr(result, "failures"), 0L)
    expect_equal(sum(file.exists(paths)), 6L)
})

# Altered bytes, parseable scalar JSON, and incomplete field names are not
# valid cache receipts.
test_that("cache receipts detect changed bytes and invalid shapes before shared reuse", {
    path <- file.path(withr::local_tempdir(), "payload.rds")
    payload <- list(
        data = data.table::data.table(value = 1),
        grid_sources = data.table::data.table(),
        available_time_count = 1L,
        actual_start = Sys.time(),
        actual_end = Sys.time()
    )
    store__extract_cache_write(path, payload)
    expect_true(store__extract_cache_available(path))
    payload$data$value <- 2
    saveRDS(payload, path)
    expect_false(store__extract_cache_available(path))
    expect_null(store__extract_cache_read(path))

    receipt <- paste0(path, ".json")
    for (text in c("1", "null", "[]", "{}", "{bad")) {
        writeLines(text, receipt)
        expect_false(store__extract_cache_available(path))
        expect_null(store__extract_cache_read(path))
    }
    store_write_json_atomic(list(sha256_extra = checksum_file(path)), receipt)
    expect_false(store__extract_cache_available(path))
})

# vim: fdm=marker :
