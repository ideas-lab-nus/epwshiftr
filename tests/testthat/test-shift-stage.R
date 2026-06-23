shift_test_response <- function(docs) {
    list(
        responseHeader = list(status = 0L, QTime = 0L, params = stats::setNames(list(), character())),
        response = list(numFound = nrow(docs), start = 0L, docs = docs, maxScore = 1),
        facet_counts = list(
            facet_queries = stats::setNames(list(), character()),
            facet_fields = stats::setNames(list(), character()),
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
}

shift_test_dataset_docs <- function() {
    data.frame(
        id = "dataset-1",
        instance_id = "dataset-1.v20260101",
        master_id = "dataset-1",
        size = 123,
        access = I(list(c("OPENDAP", "HTTPServer"))),
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variable_id = "tas",
        frequency = "day",
        variant_label = "r1i1p1f1",
        data_node = "example.org",
        check.names = FALSE
    )
}

shift_test_file_docs <- function(path, opendap_url = path, download_url = path, variable_id = "tas") {
    docs <- data.frame(
        id = sprintf("%s|dataset-1", basename(path)),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", basename(path)),
        master_id = sprintf("%s.master", basename(path)),
        replica = FALSE,
        tracking_id = sprintf("hdl:21.14100/shift-test-%s", variable_id),
        title = basename(path),
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = variable_id,
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("%s|application/netcdf|OPENDAP", opendap_url),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

shift_test_file_result <- function(docs) {
    params <- query_param__as_store(list(
        project = "CMIP6",
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
    query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = params,
        result = shift_test_response(docs)
    )
}

shift_test_mock_collect <- function(file_docs, calls) {
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                shift_test_dataset_docs()
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- shift_test_response(docs)
            calls$values <- c(calls$values, type)
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
}

test_that("shift_request() and shift_site() create inspectable S7 stages", {
    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "mon",
        filters = list(table_id = "Amon")
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = "baseline.epw")
    site_from_path <- shift_site(epw = get_cache_epw(), id = "SIN")
    site_from_first_arg <- shift_site(get_cache_epw())
    site_from_epw <- shift_site(eplusr::read_epw(get_cache_epw()))

    expect_true(S7::S7_inherits(req, ShiftRequest))
    expect_true(S7::S7_inherits(site, ShiftSite))
    expect_true(S7::S7_inherits(site_from_path, ShiftSite))
    expect_equal(shift_status(req), "new")
    expect_equal(shift_status(site), "new")
    expect_equal(site_from_path@lon, 103.98)
    expect_equal(site_from_path@lat, 1.37)
    expect_equal(site_from_first_arg@id, "SGP_Singapore.486980_IWEC")
    expect_equal(site_from_first_arg@label, "Singapore")
    expect_equal(site_from_epw@id, "486980")
    expect_equal(site_from_epw@lon, 103.98)
    expect_equal(site_from_epw@lat, 1.37)
    expect_named(shift_diagnostics(req), shift_diagnostic_columns())
    expect_equal(data.table::as.data.table(req)$variables, "tas,hurs")
    expect_true(data.table::as.data.table(site)$has_epw)
})

test_that("shift diagnostics normalize empty partial tables", {
    partial <- data.table::data.table(stage = character(), severity = character())
    diagnostics <- shift_diagnostics_normalize(partial)

    expect_named(diagnostics, shift_diagnostic_columns())
    expect_equal(nrow(diagnostics), 0L)
})

test_that("shift_collect() uses Dataset collection before File collection", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test_mock_collect(shift_test_file_docs("tas_day.nc"), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    store_path <- tempfile("shift-store-")
    files <- req |>
        shift_collect(store = store_path, label = "shift-test")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_equal(calls$values, c("Dataset", "File"))
    expect_identical(calls$file_fields[[1L]], "*")
    expect_equal(shift_status(files), "collected")
    expect_true(length(shift_ids(files)$query_id) == 1L)
    expect_equal(nrow(data.table::as.data.table(files)), 1L)
    expect_named(shift_check(files, strict = TRUE), shift_diagnostic_columns())
    expect_equal(shift_status(shift_refresh(files)), "collected")

    store <- shift_store(files)
    store$add_files(shift_test_file_result(shift_test_file_docs("hurs_day.nc", variable_id = "hurs")))
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    expect_equal(nrow(data.table::as.data.table(dl)), 1L)

    rds <- tempfile(fileext = ".rds")
    saveRDS(files, rds)
    restored <- readRDS(rds)
    expect_equal(shift_status(restored), "collected")
    expect_equal(nrow(data.table::as.data.table(restored)), 1L)
})

test_that("shift_* stages run through extract, relaxed morph, and EPW output", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test_mock_collect(shift_test_file_docs(basename(nc), opendap_url = nc, download_url = nc), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = get_cache_epw())
    store_path <- tempfile("shift-store-")

    files <- shift_collect(req, store = store_path, label = "shift-full")
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    climate <- shift_extract(
        dl,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )
    climate_resumed <- shift_extract(
        dl,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )
    morphed <- shift_morph(climate, strict = FALSE)
    epws <- shift_epw(morphed, dir = "shift-epw")

    expect_true(S7::S7_inherits(dl, ShiftDownload))
    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_true(S7::S7_inherits(climate_resumed, ShiftClimate))
    expect_true(S7::S7_inherits(morphed, ShiftMorphed))
    expect_true(S7::S7_inherits(epws, ShiftOutputs))
    expect_equal(shift_status(climate), "extracted")
    expect_equal(shift_status(climate_resumed), "extracted")
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(epws), "written")
    expect_named(morphed@meta$workflow, c("preflight", "climate", "baseline", "preview", "plan", "diagnostics", "results", "outputs"))
    expect_null(morphed@meta$workflow$outputs)
    expect_true(nrow(shift_outputs(epws)) >= 1L)
})
