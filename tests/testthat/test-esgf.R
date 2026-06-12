test_that("esgf_query() compatibility wrapper", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- TRUE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    options(epwshiftr.verbose = FALSE)
    # Dataset query
    expect_s3_class(
        qd <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qd)) {
        fq_qd <- unlist(attr(qd, "response")$responseHeader$params$fq)
        expect_true(
            all(
                c(
                    "type:Dataset",
                    "project:\"CMIP6\"",
                    "activity_id:\"ScenarioMIP\"",
                    "experiment_id:\"ssp126\" || experiment_id:\"ssp245\" || experiment_id:\"ssp370\" || experiment_id:\"ssp585\"",
                    "source_id:\"EC-Earth3\"",
                    "variable_id:\"tas\"",
                    "nominal_resolution:\"100km\" || nominal_resolution:\"50km\" || nominal_resolution:\"100 km\" || nominal_resolution:\"50 km\"",
                    "frequency:\"day\"",
                    "replica:false",
                    "variant_label:\"r1i1p1f1\""
                ) %in%
                    fq_qd
            )
        )
        expect_named(
            qd,
            c(
                "dataset_id",
                "mip_era",
                "activity_drs",
                "institution_id",
                "source_id",
                "experiment_id",
                "member_id",
                "table_id",
                "frequency",
                "grid_label",
                "version",
                "nominal_resolution",
                "variable_id",
                "variable_long_name",
                "variable_units",
                "data_node",
                "dataset_pid"
            )
        )
    }

    # File query
    expect_s3_class(
        qf <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1L, type = "File"),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qf)) {
        fq_qf <- unlist(attr(qf, "response")$responseHeader$params$fq)
        expect_true(
            all(
                c(
                    "type:File",
                    "project:\"CMIP6\"",
                    "activity_id:\"ScenarioMIP\"",
                    "experiment_id:\"ssp126\" || experiment_id:\"ssp245\" || experiment_id:\"ssp370\" || experiment_id:\"ssp585\"",
                    "source_id:\"EC-Earth3\"",
                    "variable_id:\"tas\"",
                    "nominal_resolution:\"100km\" || nominal_resolution:\"50km\" || nominal_resolution:\"100 km\" || nominal_resolution:\"50 km\"",
                    "frequency:\"day\"",
                    "replica:false",
                    "variant_label:\"r1i1p1f1\""
                ) %in%
                    fq_qf
            )
        )
        expect_named(
            qf,
            c(
                "file_id",
                "dataset_id",
                "mip_era",
                "activity_drs",
                "institution_id",
                "source_id",
                "experiment_id",
                "member_id",
                "table_id",
                "frequency",
                "grid_label",
                "version",
                "nominal_resolution",
                "variable_id",
                "variable_long_name",
                "variable_units",
                "datetime_start",
                "datetime_end",
                "file_size",
                "data_node",
                "file_url",
                "tracking_id"
            )
        )
    }

    # empty found
    expect_s3_class(q <- esgf_query(variable = "NONSENSE"), "data.table")
    expect_equal(q, data.table::data.table(), ignore_attr = TRUE)

    # can return if no data has been found
    expect_s3_class(esgf_query(resolution = "1 m"), "data.table")
})

test_that("init_cmip6_index()", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- TRUE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    withr::local_options(list(epwshiftr.dir_store = withr::local_tempdir()))

    # can return if no data has been found
    expect_s3_class(init_cmip6_index(resolution = "1 m"), "data.table")

    expect_s3_class(
        idx <- init_cmip6_index(
            variable = "tas",
            source = "EC-Earth3",
            experiment = "ssp585",
            years = 2060,
            limit = 1,
            save = TRUE
        ),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(idx)) {
        expect_equal(
            names(idx),
            c(
                "file_id",
                "dataset_id",
                "mip_era",
                "activity_drs",
                "institution_id",
                "source_id",
                "experiment_id",
                "member_id",
                "table_id",
                "frequency",
                "grid_label",
                "version",
                "nominal_resolution",
                "variable_id",
                "variable_long_name",
                "variable_units",
                "datetime_start",
                "datetime_end",
                "file_size",
                "data_node",
                "file_url",
                "dataset_pid",
                "tracking_id"
            )
        )
        expect_true(file.exists(store_cmip6_index_active_path()))
    }
})

test_that("init_cmip6_index() keeps empty file-query joins stable", {
    qd <- data.table::data.table(
        dataset_id = "dataset-id", mip_era = "CMIP6", activity_drs = "ScenarioMIP",
        institution_id = "ECMWF", source_id = "EC-Earth3", experiment_id = "ssp585",
        member_id = "r1i1p1f1", table_id = "day", frequency = "day", grid_label = "gr",
        version = "v20240101", nominal_resolution = "100 km", variable_id = "tas",
        variable_long_name = "Near-Surface Air Temperature", variable_units = "K",
        data_node = "example.org", dataset_pid = "hdl:21.14100/mock-dataset"
    )

    testthat::local_mocked_bindings(
        esgf_query = function(..., type = "Dataset") {
            if (identical(type, "Dataset")) qd else data.table::data.table()
        },
        .package = "epwshiftr"
    )

    expect_warning(
        idx <- init_cmip6_index(
            variable = "tas",
            source = "EC-Earth3",
            experiment = "ssp585",
            years = 2060
        ),
        "did not find any matched output file after 10 retries"
    )
    expect_s3_class(idx, "data.table")
    expect_named(idx, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start", "datetime_end",
        "file_size", "data_node", "file_url", "dataset_pid", "tracking_id"
    ))
})

test_that("load_cmip6_index()", {
    old_warned <- this$esgf_query_deprecated_warned
    this$esgf_query_deprecated_warned <- TRUE
    withr::defer(this$esgf_query_deprecated_warned <- old_warned)

    skip_on_cran()

    dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_store = dir))

    # can stop if no active index artifact is available
    expect_error(load_cmip6_index(TRUE))

    # can stop if malformed input
    bad_store <- EsgStore$new(dir, create = TRUE)
    withr::defer(bad_store$close())
    index <- store_path("queries", "cmip6-index", "bad.csv", root = dir)
    dir.create(dirname(index), recursive = TRUE, showWarnings = FALSE)
    file.create(index)
    artifact_id <- bad_store$register_artifact(kind = "cmip6_index", path = index, project = "CMIP6")
    bad_store$set_meta(store_cmip6_index_active_key(), artifact_id)
    bad_store$close()
    expect_error(load_cmip6_index(TRUE))

    expect_s3_class(
        idx <- init_cmip6_index(
            variable = "tas",
            source = "EC-Earth3",
            years = 2060,
            experiment = "ssp585",
            limit = 1,
            save = TRUE
        ),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(idx)) {
        cache <- get_cache()

        expect_s3_class(idx1 <- load_cmip6_index(), "data.table")
        expect_equal(idx, idx1)

        # remove exising db
        this$index_db <- NULL
        expect_s3_class(idx2 <- load_cmip6_index(TRUE), "data.table")
        expect_equal(idx, idx2)
    }
})

test_that("set_cmip6_index()", {
    skip_on_cran()

    withr::local_options(list(epwshiftr.dir_store = withr::local_tempdir()))
    idx <- data.table::data.table(file_id = "file-1", dataset_id = "dataset-1")

    expect_s3_class(set_cmip6_index(idx, save = TRUE), "data.table")
    index_path <- store_cmip6_index_active_path()
    expect_true(file.exists(index_path))
    expect_equal(data.table::fread(index_path), idx)
})

test_that("data_node_status()", {
    response <- list(
        status = "success",
        data = list(
            result = list(
                metric = list(instance = "example.org"),
                value = list(c("0", "1"))
            )
        )
    )

    testthat::local_mocked_bindings(
        normalize_index_node = function(index_node, raw = TRUE) {
            expect_true(raw)
            list(url = "https://example.org", path = "/esgf-1-5-bridge")
        },
        with_url_cache = function(name, url, fn, validate) {
            expect_identical(name, "datanode")
            expect_identical(url, "https://example.org/proxy/status")
            res <- fn()
            expect_true(validate(res))
            res
        },
        .package = "epwshiftr"
    )
    testthat::local_mocked_bindings(
        fromJSON = function(url) {
            expect_identical(url, "https://example.org/proxy/status")
            response
        },
        .package = "jsonlite"
    )

    node <- expect_s3_class(data_node_status(), "data.table")
    expect_named(node, c("data_node", "status"))
    expect_identical(node$data_node, "example.org")
    expect_identical(node$status, "UP")

    testthat::local_mocked_bindings(
        curl_fetch_memory = function(url, handle) {
            expect_identical(url, "https://example.org/")
            list(status_code = 200L)
        },
        .package = "curl"
    )

    node <- expect_s3_class(data_node_status(speed_test = TRUE), "data.table")
    expect_named(node, c("data_node", "status", "probe_ms"))
    expect_identical(node$data_node, "example.org")
    expect_identical(node$status, "UP")
    expect_type(node$probe_ms, "double")
    expect_false(is.na(node$probe_ms))
})

test_that("data_node_status() live query", {
    skip_on_cran()

    node <- expect_s3_class(data_node_status(), "data.table")
    expect_named(node, c("data_node", "status"))

    # can test speed using HTTP probes
    node <- expect_s3_class(data_node_status(TRUE, 1), "data.table")
    expect_named(node, c("data_node", "status", "probe_ms"))
})
