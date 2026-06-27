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

shift_test_is_absolute_path <- function(path) {
    grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

shift_test_dataset_docs <- function(variable_id = "tas") {
    data.frame(
        id = "dataset-1",
        instance_id = "dataset-1.v20260101",
        master_id = "dataset-1",
        size = 123,
        access = I(list(c("OPENDAP", "HTTPServer"))),
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variable_id = variable_id[[1L]],
        frequency = "day",
        variant_label = "r1i1p1f1",
        data_node = "example.org",
        check.names = FALSE
    )
}

shift_test_file_docs <- function(path, opendap_url = path, download_url = path, variable_id = "tas",
                                 include_opendap = TRUE, include_download = TRUE) {
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
    urls <- character()
    if (isTRUE(include_opendap)) {
        urls <- c(urls, sprintf("%s|application/netcdf|OPENDAP", opendap_url))
    }
    if (isTRUE(include_download)) {
        urls <- c(urls, sprintf("%s|application/netcdf|HTTPServer", download_url))
    }
    docs$url <- I(list(urls))
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

shift_test_param_value <- function(params, name) {
    state <- tryCatch(params$serialize(null = TRUE), error = function(e) list())
    value <- state[[name]]
    if (is.null(value)) {
        return(NULL)
    }
    if (is.list(value) && "value" %in% names(value)) {
        return(value$value)
    }
    value
}

shift_test_mock_collect_filtered <- function(file_docs, calls) {
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            filter_fields <- c("experiment_id", "activity_id", "source_id", "variant_label", "frequency", "table_id", "variable_id")
            filter_values <- stats::setNames(vector("list", length(filter_fields)), filter_fields)
            for (field in filter_fields) {
                values <- shift_test_param_value(params, field)
                values <- as.character(values)
                filter_values[[field]] <- values[!is.na(values) & nzchar(values)]
            }
            docs <- if (identical(type, "Dataset")) {
                calls$last_filters <- filter_values
                dataset_variable <- if (length(filter_values$variable_id)) {
                    filter_values$variable_id
                } else {
                    unique(file_docs$variable_id)
                }
                dataset <- shift_test_dataset_docs(dataset_variable[[1L]])
                for (field in intersect(filter_fields, names(dataset))) {
                    if (length(filter_values[[field]])) {
                        dataset[[field]] <- filter_values[[field]][[1L]]
                    }
                }
                dataset
            } else {
                data.table::as.data.table(file_docs)
            }
            if (identical(type, "File")) {
                for (field in filter_fields) {
                    values <- filter_values[[field]]
                    if (!length(values) && !is.null(calls$last_filters[[field]])) {
                        values <- calls$last_filters[[field]]
                    }
                    if (length(values) && field %in% names(docs)) {
                        docs <- docs[docs[[field]] %in% values]
                    }
                }
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- shift_test_response(as.data.frame(docs))
            calls$values <- c(calls$values, type)
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
}

shift_test_mock_collect_sequence <- function(file_doc_sets, calls) {
    calls$file_calls <- 0L
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            variables <- as.character(shift_test_param_value(params, "variable_id"))
            variables <- variables[!is.na(variables) & nzchar(variables)]
            docs <- if (identical(type, "Dataset")) {
                shift_test_dataset_docs(if (length(variables)) variables[[1L]] else "tas")
            } else {
                calls$file_calls <- calls$file_calls + 1L
                idx <- min(calls$file_calls, length(file_doc_sets))
                if (idx > 1L) {
                    params$experiment_id("historical")
                    params$activity_id("CMIP")
                } else {
                    params$experiment_id("ssp585")
                    params$activity_id("ScenarioMIP")
                }
                data.table::as.data.table(file_doc_sets[[idx]])
            }
            if (identical(type, "File") && length(variables) && "variable_id" %in% names(docs)) {
                docs <- docs[docs$variable_id %in% variables]
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- shift_test_response(as.data.frame(docs))
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
        time = 2060L,
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
    expect_equal(req@meta$time, c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"))
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

test_that("shift reference specs validate manual and automatic reference inputs", {
    periods <- epw_morph_periods(reference = 1995L)

    historical <- shift_reference_historical(periods)
    manual <- shift_reference_plan("plan-reference", periods)

    expect_true(S7::S7_inherits(historical, ShiftReferenceSpec))
    expect_true(S7::S7_inherits(manual, ShiftReferenceSpec))
    expect_equal(historical@mode, "historical")
    expect_equal(historical@experiment, "historical")
    expect_equal(historical@activity, "CMIP")
    expect_equal(manual@mode, "plan")
    expect_equal(manual@plan_id, "plan-reference")
    expect_error(shift_reference_historical(NULL), "data.frame")
    expect_error(shift_reference_plan(character(), periods), "length >= 1")
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
    datasets <- shift_datasets(req)
    expect_equal(datasets$count(), 1L)
    expect_equal(calls$values, "Dataset")

    files <- req |>
        shift_collect(store = store_path, label = "shift-test")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_equal(calls$values, c("Dataset", "Dataset", "File"))
    expect_identical(calls$file_fields[[1L]], "*")
    expect_equal(shift_status(files), "collected")
    expect_true(length(shift_ids(files)$query_id) == 1L)
    expect_equal(nrow(data.table::as.data.table(files)), 1L)
    expect_equal(shift_datasets(files)$count(), 1L)
    file_result <- shift_files(files)
    expect_s3_class(file_result, "EsgResultFile")
    expect_equal(file_result$count(), 1L)
    expect_equal(file_result$filename, "tas_day.nc")
    expect_error(shift_files(req), "No File result")
    expect_named(shift_check(files, strict = TRUE), shift_diagnostic_columns())
    expect_equal(shift_status(shift_refresh(files)), "collected")

    store <- shift_store(files)
    store$add_files(shift_test_file_result(shift_test_file_docs("hurs_day.nc", variable_id = "hurs")))
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    expect_equal(nrow(data.table::as.data.table(dl)), 1L)
    expect_equal(shift_datasets(dl)$count(), 1L)
    expect_equal(shift_files(dl)$filename, "tas_day.nc")

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
    climate <- shift_extract(
        files,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )
    climate_resumed <- shift_extract(
        files,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    climate_after_download <- shift_extract(
        dl,
        site = site,
        periods = epw_morph_periods(`2060s` = 2060L),
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )
    morph_recipe <- suppressWarnings(epw_morph_recipe("belcher_absolute", methods = c(tdb = "shift")))
    expect_equal(epw_morph_variables(morph_recipe), epw_morph_variables("recommended"))
    morphed <- shift_morph(climate, recipe = morph_recipe, strict = FALSE)
    epws <- shift_epw(morphed, dir = "shift-epw")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_true(S7::S7_inherits(dl, ShiftDownload))
    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_true(S7::S7_inherits(climate_resumed, ShiftClimate))
    expect_true(S7::S7_inherits(climate_after_download, ShiftClimate))
    expect_true(S7::S7_inherits(climate@meta$files, ShiftFiles))
    expect_null(climate@meta$download)
    expect_true(S7::S7_inherits(climate_after_download@meta$download, ShiftDownload))
    expect_true(S7::S7_inherits(morphed, ShiftMorphed))
    expect_true(S7::S7_inherits(epws, ShiftOutputs))
    expect_equal(shift_status(climate), "extracted")
    expect_equal(shift_status(climate_resumed), "extracted")
    expect_equal(shift_status(climate_after_download), "extracted")
    expect_true(length(shift_ids(climate)$plan_id) >= 1L)
    expect_true(length(shift_ids(climate_after_download)$plan_id) >= 1L)
    expect_true(nrow(shift_coverage(climate)) >= 1L)
    preview <- shift_data(
        climate,
        n = 2L,
        columns = c("site_id", "variable_id", "time", "lon", "lat", "value", "units")
    )
    expect_equal(nrow(preview), 2L)
    expect_named(preview, c("site_id", "variable_id", "time", "lon", "lat", "value", "units"))
    expect_equal(unique(preview$site_id), "SIN")
    expect_equal(unique(preview$variable_id), "tas")
    expect_equal(nrow(shift_data(climate, n = 0L)), 0L)
    expect_equal(nrow(shift_data(climate, variables = "missing")), 0L)
    expect_error(shift_data(climate, case_id = "missing"), "case_id")
    expect_error(shift_data(files), "ShiftClimate")
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(epws), "written")
    morphed_preview <- shift_data(
        morphed,
        n = 2L,
        columns = c(
            "case_id", "source_id", "experiment_id", "variant_label", "period",
            "year", "month", "day", "hour", "dry_bulb_temperature", "relative_humidity"
        )
    )
    expect_equal(nrow(morphed_preview), 2L)
    expect_true(all(c("case_id", "period", "dry_bulb_temperature") %in% names(morphed_preview)))
    expect_equal(unique(morphed_preview$period), "2060s")
    expect_equal(nrow(shift_data(morphed, case_id = "missing")), 0L)
    expect_error(shift_data(morphed, variables = "tas"), "variables")
    expect_error(shift_data(morphed, n = 1L, columns = "missing_column"), "Unknown")

    epw_preview <- shift_data(
        epws,
        n = 2L,
        columns = c(
            "output_id", "case_id", "path", "source_id", "experiment_id",
            "variant_label", "period", "year", "month", "day", "hour",
            "dry_bulb_temperature"
        )
    )
    expect_equal(nrow(epw_preview), 2L)
    expect_true(all(c("output_id", "case_id", "path", "dry_bulb_temperature") %in% names(epw_preview)))
    expect_equal(unique(epw_preview$period), "2060s")
    expect_equal(nrow(shift_data(epws, case_id = "missing")), 0L)
    expect_error(shift_data(epws, variables = "tas"), "variables")

    morph_artifacts <- shift_artifacts(morphed)
    output_artifacts <- shift_artifacts(epws)
    expect_true(nrow(morph_artifacts) >= 1L)
    expect_true(nrow(output_artifacts) >= 1L)
    expect_true(all(morph_artifacts$role %in% "derived"))
    expect_true(all(output_artifacts$role %in% "output"))
    expect_named(morphed@meta$workflow, c("preflight", "climate", "baseline", "preview", "plan", "diagnostics", "results", "outputs"))
    expect_null(morphed@meta$workflow$outputs)
    expect_true(nrow(shift_outputs(epws)) >= 1L)
})

test_that("shift_morph() resolves automatic and manual historical references", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    variables <- epw_morph_variables("recommended")
    future_nc <- stats::setNames(vapply(variables, function(variable_id) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = variable_id)
        path
    }, character(1L)), variables)
    reference_nc <- stats::setNames(vapply(variables, function(variable_id) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 1995L, variable_id = variable_id)
        path
    }, character(1L)), variables)
    on.exit(unlink(c(future_nc, reference_nc)), add = TRUE)

    future_docs <- data.table::rbindlist(lapply(variables, function(variable_id) {
        shift_test_file_docs(
            basename(future_nc[[variable_id]]),
            opendap_url = future_nc[[variable_id]],
            download_url = future_nc[[variable_id]],
            variable_id = variable_id
        )
    }), fill = TRUE)
    reference_docs <- data.table::rbindlist(lapply(variables, function(variable_id) {
        shift_test_file_docs(
            basename(reference_nc[[variable_id]]),
            opendap_url = reference_nc[[variable_id]],
            download_url = reference_nc[[variable_id]],
            variable_id = variable_id
        )
    }), fill = TRUE)
    future_docs[, `:=`(
        dataset_id = paste0("future-", variable_id),
        master_id = paste0("future-", variable_id),
        instance_id = paste0("future-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/future-", variable_id),
        id = paste0(title, "|future-", variable_id)
    )]
    reference_docs[, `:=`(activity_id = "CMIP", experiment_id = "historical")]
    reference_docs[, `:=`(
        dataset_id = paste0("historical-", variable_id),
        master_id = paste0("historical-", variable_id),
        instance_id = paste0("historical-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/historical-", variable_id),
        id = paste0(title, "|historical-", variable_id)
    )]
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test_mock_collect_sequence(list(future_docs, reference_docs), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = get_cache_epw())
    store_path <- tempfile("shift-store-")
    future_periods <- epw_morph_periods(`2060s` = 2060L)
    reference_periods <- epw_morph_periods(reference = 1995L)

    climate <- req |>
        shift_collect(store = store_path, label = "future") |>
        shift_extract(site = site, periods = future_periods, variables = variables)

    recipe <- epw_morph_recipe("belcher")
    auto_warnings <- character()
    auto <- withCallingHandlers(
        shift_morph(
            climate,
            recipe = recipe,
            reference = shift_reference_historical(reference_periods),
            strict = TRUE,
            overwrite = TRUE
        ),
        warning = function(w) {
            msg <- conditionMessage(w)
            expected <- grepl("Time filtering with method = 'drs'", msg, fixed = TRUE) ||
                grepl("Could not parse a DRS time range", msg, fixed = TRUE)
            if (!expected) {
                return()
            }
            auto_warnings <<- c(auto_warnings, msg)
            invokeRestart("muffleWarning")
        }
    )
    expect_true(any(grepl("Time filtering with method = 'drs'", auto_warnings, fixed = TRUE)))
    expect_true(any(grepl("Could not parse a DRS time range", auto_warnings, fixed = TRUE)))
    reference_climate <- auto@meta$reference
    reference_ids <- shift_ids(reference_climate)
    plan_reference <- shift_reference_plan(reference_ids$plan_id, reference_periods)
    manual <- shift_morph(
        climate,
        recipe = recipe,
        reference = reference_climate,
        strict = TRUE
    )
    manual_plan <- shift_morph(
        climate,
        recipe = recipe,
        reference = plan_reference,
        strict = TRUE
    )

    expect_true(S7::S7_inherits(auto, ShiftMorphed))
    expect_true(S7::S7_inherits(reference_climate, ShiftClimate))
    expect_true(S7::S7_inherits(auto@meta$reference_spec, ShiftReferenceSpec))
    expect_equal(auto@meta$reference_spec@mode, "historical")
    expect_equal(shift_status(auto), "morphed")
    expect_equal(shift_status(reference_climate), "extracted")
    reference_rows <- shift_extraction_result_rows(shift_store(reference_climate), reference_ids$plan_id)
    expect_equal(unique(reference_rows$experiment_id), "historical")
    expect_equal(shift_status(manual), "morphed")
    expect_equal(shift_status(manual_plan), "morphed")
    expect_error(
        shift_morph(
            climate,
            recipe = recipe,
            reference = reference_climate,
            reference_plan_id = reference_ids$plan_id,
            reference_periods = reference_periods
        ),
        "either `reference` or `reference_plan_id`"
    )
    expect_true(sum(calls$values %in% "File") >= 2L)
})

test_that("shift_extract() fallback policy is available from collected files", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    docs <- shift_test_file_docs(
        basename(nc),
        download_url = sprintf("https://example.org/%s", basename(nc)),
        include_opendap = FALSE
    )
    docs$size <- file.info(nc)$size
    docs$checksum <- NA_character_
    docs$checksum_type <- NA_character_

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test_mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = get_cache_epw())
    files <- shift_collect(req, store = tempfile("shift-store-"))
    periods <- epw_morph_periods(`2060s` = 2060L)
    time <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")

    remote_only <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "error"
    )
    expect_equal(shift_status(remote_only), "blocked")
    expect_true(any(shift_coverage(remote_only)$status %in% "failed"))
    expect_match(
        paste(shift_diagnostics(remote_only)$message, collapse = "\n"),
        "OPeNDAP is not available"
    )

    queued <- shift_download(files, run = FALSE, probe = FALSE)
    task <- data.table::as.data.table(queued)[1L]
    target <- task$target_path[[1L]]
    if (!shift_test_is_absolute_path(target)) {
        target <- file.path(shift_store(files)$path, target)
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(nc, target, overwrite = TRUE))

    local_fallback <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "auto"
    )
    expect_equal(shift_status(local_fallback), "extracted")
    expect_true(all(shift_coverage(local_fallback)$complete))
})
