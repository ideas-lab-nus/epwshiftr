test_that("esgf_query()", {
    options(epwshiftr.verbose = FALSE)
    # Dataset query
    expect_is(
        qd <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qd)) {
        fq_qd <- unlist(attr(qd, "response")$responseHeader$params$fq)
        expect_true(
            all(c("type:Dataset",
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
            ) %in% fq_qd)
        )
        expect_equal(names(qd),
            c(
                "dataset_id", "mip_era", "activity_drs", "institution_id", "source_id",
                "experiment_id", "member_id", "table_id", "frequency", "grid_label",
                "version", "nominal_resolution", "variable_id", "variable_long_name",
                "variable_units", "data_node", "dataset_pid"
            )
        )
    }

    # File query
    expect_is(
        qf <- esgf_query(variable = "tas", source = "EC-Earth3", frequency = "day", limit = 1, type = "File"),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qf)) {
        fq_qf <- unlist(attr(qf, "response")$responseHeader$params$fq)
        expect_true(
            all(c("type:File",
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
            ) %in% fq_qf)
        )
        expect_equal(names(qf),
            c(
                "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
                "source_id", "experiment_id", "member_id", "table_id", "frequency", 
                "grid_label", "version", "nominal_resolution", "variable_id",
                "variable_long_name", "variable_units", "datetime_start",
                "datetime_end", "file_size", "data_node", "file_url", "tracking_id"
            )
        )
    }

    # empty found
    expect_is(q <- esgf_query(variable = "NONSENSE"), "data.table")
    expect_equivalent(q, data.table())

    # can return if no data has been found
    expect_is(esgf_query(resolution = "1 m"), "data.table")
})

test_that("init_cmip6_index()", {
    options(epwshiftr.dir = tempdir())

    # can return if no data has been found
    expect_is(init_cmip6_index(resolution = "1 m"), "data.table")

    expect_is(
        idx <- init_cmip6_index(variable = "tas", source = "EC-Earth3",
            experiment = "ssp858", years = 2060, limit = 1, save = TRUE
        ),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(idx)) {
        expect_equal(names(idx),
            c(
                "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
                "source_id", "experiment_id", "member_id", "table_id", "frequency",
                "grid_label", "version", "nominal_resolution", "variable_id",
                "variable_long_name", "variable_units", "datetime_start",
                "datetime_end", "file_size", "data_node", "file_url", "dataset_pid",
                "tracking_id"
            )
        )
        expect_true(file.exists(file.path(.data_dir(), "cmip6_index.csv")))
    }
})

test_that("load_cmip6_index()", {
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

    expect_is(
        idx <- init_cmip6_index(
            variable = "tas", source = "EC-Earth3", years = 2060,
            experiment = "ssp585", limit = 1, save = TRUE
        ),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(idx)) {
        # create a directory to store the data
        cache <- "~/epwshiftr-cache"
        if (!dir.exists(cache)) dir.create(cache)

        # download output files
        for (f in idx$file_url) {
            dest <- file.path(cache, basename(f))
            if (!file.exists(dest)) {
                flag <- download.file(f, , mode = "wb")
            }
        }

        expect_is(idx1 <- load_cmip6_index(), "data.table")
        expect_equal(idx, idx1)

        # remove exising db
        EPWSHIFTR_ENV$index_db <- NULL
        expect_is(idx2 <- load_cmip6_index(TRUE), "data.table")
        expect_equal(idx, idx2)
    }
})

test_that("set_cmip6_index()", {
    skip_on_cran()

    # can overwrite index
    if (file.exists(file.path(tempdir(), "cmip6_index.csv"))) {
        idx <- load_cmip6_index()
        expect_is(set_cmip6_index(idx, save = TRUE), "data.table")

    }
})

test_that("get_data_dir()", {
    options("epwshiftr.dir" = "a")
    expect_error(get_data_dir(), "not exists")

    options("epwshiftr.dir" = NULL)

    skip_on_cran()
    .data_dir(TRUE)
    if (.Platform$OS.type == "windows") {
        expect_equal(get_data_dir(), normalizePath(user_data_dir(appauthor = "epwshiftr")))
    } else {
        expect_equal(get_data_dir(), normalizePath(user_data_dir(appname = "epwshiftr")))
    }
})

test_that("get_data_node()", {
    skip_on_cran()

    expect_is(node <- get_data_node(), "data.table")
    expect_equal(names(node), c("data_node", "status"))

    # can test speed using pingr
    expect_is(node <- get_data_node(TRUE, 1), "data.table")
    expect_equal(names(node), c("data_node", "status", "ping"))
})
