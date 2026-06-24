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
                    "latest:true",
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
                    "latest:true",
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

    options(epwshiftr.dir = tempdir())

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
        expect_true(file.exists(file.path(.data_dir(), "cmip6_index.csv")))
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

    dir <- file.path(tempdir(), "test1")
    dir.create(dir, FALSE)
    options(epwshiftr.dir = dir)

    # can stop if 'cmip6_index.csv' file was not found
    expect_error(load_cmip6_index(TRUE))
    unlink(dir, TRUE)

    # can stop if malformed input
    options(epwshiftr.dir = tempdir())
    index <- file.path(tempdir(), "cmip6_index.csv")
    file.create(index)
    expect_error(load_cmip6_index(TRUE))
    unlink(index, TRUE)

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

    # can overwrite index
    if (file.exists(file.path(tempdir(), "cmip6_index.csv"))) {
        idx <- load_cmip6_index()
        expect_s3_class(set_cmip6_index(idx, save = TRUE), "data.table")
    }
})

test_that("get_data_dir()", {
    options("epwshiftr.dir" = "a")
    expect_error(get_data_dir(), "not exists")

    options("epwshiftr.dir" = NULL)

    skip_on_cran()
    .data_dir(TRUE)
    if (.Platform$OS.type == "windows") {
        expect_identical(get_data_dir(), normalizePath(user_data_dir(appauthor = "epwshiftr")))
    } else {
        expect_identical(get_data_dir(), normalizePath(user_data_dir(appname = "epwshiftr")))
    }
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
})

test_that("data_node_status() live query", {
    skip_on_cran()

    node <- expect_s3_class(data_node_status(), "data.table")
    expect_named(node, c("data_node", "status"))

    # can test speed using pingr
    node <- expect_s3_class(data_node_status(TRUE, 1), "data.table")
    expect_named(node, c("data_node", "status", "ping"))
})
