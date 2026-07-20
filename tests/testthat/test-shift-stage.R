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

# Build a complete ESGF File document so workflow resolver tests exercise the
# same fixed identity, status, access, and time-coverage fields as production.
shift_test_file_docs <- function(path, opendap_url = path, download_url = path, variable_id = "tas",
                                 include_opendap = TRUE, include_download = TRUE,
                                 datetime_start = "2060-01-01T00:00:00Z",
                                 datetime_end = "2060-12-31T23:59:59Z") {
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
        latest = TRUE,
        retracted = FALSE,
        deprecated = FALSE,
        datetime_start = datetime_start,
        datetime_end = datetime_end,
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
    calls$collect_times <- list()
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            calls$collect_times <- c(calls$collect_times, list(list(
                type = type,
                datetime_start = shift_test_param_value(params, "datetime_start"),
                datetime_stop = shift_test_param_value(params, "datetime_stop")
            )))
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
    site_from_epw <- shift_site(epw_file_read(get_cache_epw()))
    site_from_external_epw <- shift_site(test_external_epw(get_cache_epw()))

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
    expect_true(inherits(site_from_external_epw@epw, "EpwFile"))
    expect_equal(site_from_external_epw@id, "486980")
    expect_named(shift_diagnostics(req), shift_diagnostic_columns())
    expect_equal(data.table::as.data.table(req)$variables, "tas,hurs")
    expect_true(data.table::as.data.table(site)$has_epw)
})

test_that("shift_request() applies ESGF control filters through typed setters", {
    req <- shift_request(
        project = "CMIP6",
        variables = "tas",
        filters = list(latest = TRUE, replica = FALSE, table_id = "Amon")
    )
    query <- shift_as_query(req)

    expect_true(query_param__value(query$latest()))
    expect_false(query_param__value(query$replica()))
    expect_identical(query_param__value(query$params()$table_id), "Amon")
})

test_that("shift_cmip6_scenario() and shift_plan() describe future EPW workflows", {
    req <- shift_cmip6_scenario(
        source = "BCC-CSM2-MR",
        scenario = c("ssp126", "ssp585"),
        member = "r1i1p1f1",
        years = 2055:2065,
        variables = "belcher",
        frequency = "mon",
        grid_label = "gn",
        data_node = "esgf.ceda.ac.uk",
        index_node = "https://esgf-data.dkrz.de"
    )

    expect_equal(req@meta$project, "CMIP6")
    expect_equal(req@meta$experiment, c("ssp126", "ssp585"))
    expect_equal(req@meta$time, c("2055-01-01T00:00:00Z", "2065-12-31T23:59:59Z"))
    expect_equal(req@meta$filters$activity_id, "ScenarioMIP")
    expect_equal(req@meta$filters$table_id, "Amon")
    expect_true(all(c("tas", "hurs", "pr") %in% req@meta$variables))

    site <- shift_site(id = "SIN", epw = get_cache_epw())
    plan <- shift_plan(
        request = req,
        site = site,
        periods = list(`2060s` = "2055:2065"),
        store = tempfile("shift-store-"),
        method = belcher(reference = historical_reference("1995:2014")),
        epw = list(export_dir = tempfile("future-epw-"))
    )
    explain <- shift_explain(plan)

    expect_equal(shift_status(plan), "planned")
    expect_true(all(c("request", "method", "reference", "output") %in% explain$step))
    expect_match(explain$detail[explain$step == "request"], "BCC-CSM2-MR")
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

test_that("morph methods bind optional or explicit references at construction", {
    historical <- historical_reference(1995:2014)
    manual <- shift_reference_plan("plan-reference", epw_morph_periods(reference = 1995L))

    expect_true(S7::S7_inherits(belcher(), ShiftMorphMethod))
    expect_null(belcher()@reference)
    expect_false(belcher()@requires_reference)
    expect_true(S7::S7_inherits(belcher(reference = NULL), ShiftMorphMethod))
    expect_true(S7::S7_inherits(belcher(reference = historical), ShiftMorphMethod))
    expect_true(S7::S7_inherits(belcher(reference = manual), ShiftMorphMethod))
    expect_true(S7::S7_inherits(shift_morph_method(epw_morph_recipe("belcher")), ShiftMorphMethod))
    expect_error(
        shift_morph_method(
            suppressWarnings(epw_morph_recipe("belcher_absolute")),
            reference = historical
        ),
        "does not accept reference"
    )
    expect_error(
        shift_morph_method(epw_morph_recipe("belcher"), reference = 1995:2014),
        "ShiftReferenceSpec"
    )
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

    nc <- file.path(tempdir(), local_cmip6_nc_file(2060L, variable_id = "tas"))
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

test_that("shift_future_epw() requires a complete method and returns a task plan", {
    method <- shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute")))
    climate <- shift_cmip6(
        model = "EC-Earth3", scenarios = "ssp585",
        member = "r1i1p1f1", grid = "gr", frequency = "day", table = "day"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2060s` = 2060L),
        method = method,
        dir = tempfile("future-epw-"),
        control = shift_control(strict = FALSE),
        store = tempfile("shift-store-"),
        dry_run = TRUE
    )

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_true(S7::S7_inherits(plan@meta$climate, ShiftCmip6Spec))
    expect_equal(plan@meta$climate@model, "EC-Earth3")
    expect_equal(plan@meta$climate@scenarios, "ssp585")
    expect_equal(shift_status(plan), "planned")
    expect_equal(nrow(shift_cases(plan)), 1L)
    spec <- shift__plan_spec(plan)
    expect_null(spec$request)
    expect_equal(spec$climate$model, "EC-Earth3")
    expect_equal(spec$climate$scenarios, "ssp585")
    expect_true(S7::S7_inherits(shift__plan_from_spec(spec)@meta$climate, ShiftCmip6Spec))

    external_store <- tempfile("shift-external-epw-store-")
    external <- test_external_epw(get_cache_epw())
    original_external_path <- external$path()
    external_plan <- shift_future_epw(
        epw = external,
        climate = climate,
        periods = list(`2060s` = 2060L),
        method = method,
        dir = tempfile("future-epw-"),
        control = shift_control(strict = FALSE),
        store = external_store,
        dry_run = TRUE
    )
    expect_true(inherits(external_plan@meta$site@epw, "EpwFile"))
    expect_true(startsWith(
        external_plan@meta$site@epw$path(),
        normalizePath(external_store, winslash = "/", mustWork = TRUE)
    ))
    expect_identical(external$path(), original_external_path)
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(), climate = shift_cmip6("EC-Earth3", "ssp585"),
            periods = list(`2060s` = 2060L), method = "belcher",
            dir = tempfile("future-epw-"), dry_run = TRUE
        ),
        "ShiftMorphMethod"
    )
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(), model = "EC-Earth3", scenarios = "ssp585",
            periods = list(`2060s` = 2060L), method = method,
            dir = tempfile("future-epw-"), dry_run = TRUE
        ),
        "unused arguments"
    )
})

test_that("shift_ui() validates presentation options without changing scientific intent", {
    expect_true(S7::S7_inherits(shift_ui(), ShiftUiOptions))
    expect_equal(shift_ui("log", detail = "detail", heartbeat = 2)@progress, "log")
    expect_equal(shift_ui(detail = "debug")@detail, "debug")
    expect_error(shift_ui("invalid"), "arg")
    expect_error(shift_ui(heartbeat = -1), "not >= 0")
    expect_equal(shift__ui_mode(shift_ui("none")), "none")

    store <- tempfile("shift-ui-store-")
    output <- tempfile("shift-ui-output-")
    method <- shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute")))
    climate <- shift_cmip6(
        model = "EC-Earth3", scenarios = "ssp585",
        member = "r1i1p1f1", grid = "gr", frequency = "day", table = "day"
    )
    make_plan <- function(ui) {
        shift_future_epw(
            epw = get_cache_epw(), climate = climate,
            periods = list(`2060s` = 2060L), method = method,
            dir = output, store = store, dry_run = TRUE, ui = ui
        )
    }
    expect_identical(
        shift__plan_spec(make_plan(shift_ui("none"))),
        shift__plan_spec(make_plan(shift_ui("log", detail = "debug", heartbeat = 1)))
    )
})

test_that("ShiftReporter persists structured milestones while none mode stays silent", {
    skip_if_not_installed("duckdb")

    store <- EsgStore$new(tempfile("shift-reporter-store-"))
    on.exit(store$close(), add = TRUE)
    reporter <- shift__reporter(shift_ui("none"), store = store, run_id = "reporter-run")
    expect_silent({
        reporter$stage_started("resolve", "Resolving inputs.")
        reporter$unit_started(
            "Querying future catalog.", current = 1L, total = 2L,
            details = list(unit_type = "query", node = "example.org", scenario = "ssp585")
        )
        reporter$unit_completed(
            "Future catalog resolved.", current = 1L, total = 2L,
            details = list(variable = "tas", access_method = "OPeNDAP")
        )
        reporter$stage_completed("Inputs resolved.")
    })
    events <- morpher__private_store(store)$read_table("shift_run_event")
    expect_equal(nrow(events), 4L)
    details <- lapply(events$details_json, jsonlite::fromJSON, simplifyVector = TRUE)
    unit <- details[[which(events$message == "Future catalog resolved.")]]
    expect_equal(unit$stage, "resolve")
    expect_equal(unit$unit_type, "query")
    expect_equal(unit$node, "example.org")
    expect_equal(unit$variable, "tas")
    expect_equal(unit$outcome, "completed")
})

test_that("ShiftReporter recreates its four-row dynamic view when one row disappears", {
    bars <- character()
    updates <- character()
    increments <- integer()
    alive <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
        cli_progress_bar = function(name = NULL, total = NA, current = TRUE,
                                    auto_terminate = TRUE, ...) {
            id <- sprintf("bar-%d", length(bars) + 1L)
            bars <<- c(bars, id)
            alive[[id]] <- TRUE
            expect_false(current)
            expect_false(auto_terminate)
            id
        },
        cli_progress_update = function(id = NULL, inc = NULL, set = NULL, ...) {
            if (!isTRUE(alive[[id]])) {
                stop(sprintf("Cannot find progress bar `%s`", id), call. = FALSE)
            }
            updates <<- c(updates, id)
            increments <<- c(increments,
                if (is.null(set)) as.integer(inc) else NA_integer_)
            invisible(id)
        },
        cli_progress_done = function(id = NULL, ...) {
            alive[[id]] <- FALSE
            invisible(TRUE)
        },
        .package = "cli"
    )

    reporter <- shift__reporter(shift_ui("dynamic", heartbeat = 0))
    reporter$stage_started("resolve", "Resolving inputs.")
    reporter$unit_started("Trying node one", current = 1L, total = 2L)
    alive[["bar-1"]] <- FALSE

    expect_silent(reporter$heartbeat("Waiting for future catalog", force = TRUE))
    expect_equal(length(bars), 8L)
    expect_equal(utils::tail(updates, 4L), paste0("bar-", 5:8))
    expect_true(all(utils::tail(increments, 4L) == 0L))
    expect_message(reporter$unit_completed(
        "Node one failed", current = 1L, total = 2L, outcome = "failed"
    ), "Node one failed")
})

test_that("ShiftReporter falls back to logs when dynamic rows cannot be created", {
    testthat::local_mocked_bindings(
        cli_progress_bar = function(...) {
            stop("dynamic renderer unavailable", call. = FALSE)
        },
        .package = "cli"
    )
    reporter <- shift__reporter(shift_ui("dynamic"))

    expect_message(
        reporter$stage_started("resolve", "Resolving inputs."),
        "switched to line-by-line logs"
    )
    expect_identical(reporter$mode(), "log")
    expect_message(
        reporter$unit_started("Trying DKRZ", current = 1L, total = 6L),
        "Trying DKRZ"
    )
})

test_that("foreground interrupts persist one meaningful cancelled state", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-interrupt-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r1i1p1f1", grid = "gr",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L),
        method = shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute"))),
        dir = tempfile("shift-interrupt-output-"),
        store = store_path,
        dry_run = TRUE
    )
    testthat::local_mocked_bindings(
        shift__collect_resolved_inputs = function(...) {
            stop(structure(
                list(message = "", call = NULL),
                class = c("interrupt", "condition")
            ))
        },
        .package = "epwshiftr"
    )

    interrupted <- tryCatch(
        shift_run(plan, ui = shift_ui("none")),
        interrupt = function(e) e
    )
    expect_s3_class(interrupted, "epwshiftr_shift_cancelled")
    expect_equal(conditionMessage(interrupted), "Interrupted by user.")

    run <- shift_run_get(interrupted$run_id, store = store_path)
    expect_equal(shift_status(run), "cancelled")
    expect_false(is.na(run@meta$run$completed_at[[1L]]))
    expect_equal(run@meta$run$last_error[[1L]], "Interrupted by user.")
    expect_gt(nrow(shift_logs(run)), 0L)
    terminal <- run@meta$events[status %in% c("cancelled", "failed")]
    expect_equal(terminal$status, "cancelled")
    expect_equal(terminal$message, "Interrupted by user.")
})

test_that("rejected resolver nodes remain results rather than diagnostics", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-rejected-node-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "BCC-CSM2-MR", "ssp585",
            member = "r1i1p1f1", grid = "gn"
        ),
        periods = list(`2060s` = 2060L),
        method = belcher(),
        dir = tempfile("shift-rejected-node-output-"),
        store = store_path,
        dry_run = TRUE
    )
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    shift__run_event(
        store, run_id, "resolve", "rejected", "DKRZ rejected: missing hurs.",
        details = list(
            stage = "resolve", phase = "unit", unit_type = "index_node",
            node = INDEX_NODES[["DKRZ"]], future_files = 12L,
            reference_files = 4L, error = "missing hurs",
            outcome = "rejected"
        )
    )

    run <- shift__run_handle(store, run_id)
    expect_equal(nrow(shift_diagnostics(run, refresh = FALSE)), 0L)
    nodes <- shift__ui_event_nodes(run@meta$events)
    expect_equal(nodes$node, "DKRZ")
    expect_equal(nodes$result, "missing hurs")
})

test_that("background runs register live jobs before launching workers", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-background-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r1i1p1f1", grid = "gr",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L),
        method = shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute"))),
        dir = tempfile("shift-background-output-"),
        store = store_path,
        dry_run = TRUE
    )
    launched <- new.env(parent = emptyenv())
    withr::local_options(list(epwshiftr.shift.launcher = function(store_path, run_id, job_id, log_path) {
        launched$args <- list(
            store_path = store_path, run_id = run_id,
            job_id = job_id, log_path = log_path
        )
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
    expect_equal(shift_status(run), "queued")
    expect_equal(launched$args$run_id, shift_ids(run)$run_id)
    expect_true(startsWith(launched$args$log_path, normalizePath(store_path, winslash = "/")))
    expect_equal(run@meta$jobs$mode, "process")
    expect_equal(run@meta$jobs$status, "queued")
    expect_equal(nrow(shift_logs(run)), 0L)

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "cancelled")
    expect_equal(cancelled@meta$jobs$status, "cancelled")
})

test_that("live sidecars keep background handles readable while DuckDB is locked", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-live-lock-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r1i1p1f1", grid = "gr",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L),
        method = shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute"))),
        dir = tempfile("shift-live-lock-output-"), store = store_path, dry_run = TRUE
    )
    withr::local_options(list(epwshiftr.shift.launcher = function(...) invisible(0L)))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))

    ready <- tempfile("shift-live-lock-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(2)",
        "dbDisconnect(conn, shutdown = TRUE)",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c("-e", shQuote(child_code),
          shQuote(file.path(store_path, "manifest.duckdb")), shQuote(ready)),
        wait = FALSE, stdout = FALSE, stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) break
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))
    expect_equal(shift_status(run), "queued")

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "stopping")
    expect_true(file.exists(shift__live_path(
        store_path, shift_ids(run, refresh = FALSE)$run_id, "cancel.json"
    )))
})

test_that("background workers retry transient DuckDB launch locks", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-worker-open-store-")
    store <- EsgStore$new(store_path)
    store$close()
    ready <- tempfile("shift-worker-open-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(0.5)",
        "dbDisconnect(conn, shutdown = TRUE)",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c("-e", shQuote(child_code),
          shQuote(file.path(store_path, "manifest.duckdb")), shQuote(ready)),
        wait = FALSE, stdout = FALSE, stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) break
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))

    # This call represents the detached worker starting while a short-lived
    # status reader still owns the manifest.
    worker_store <- shift__job_store_open(store_path, timeout = 3, interval = 0.05)
    on.exit(worker_store$close(), add = TRUE)
    expect_true(inherits(worker_store, "EsgStore"))
})

test_that("shift_future_epw() completes baseline and explicit-reference scenario cases", {
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

    # Represent each scenario-variable pair with a distinct ESGF identity while
    # reusing compact local NetCDF fixtures for the two scenario catalogs.
    workflow_docs <- function(paths, experiments, activity, start, end) {
        data.table::rbindlist(lapply(experiments, function(experiment_id) {
            data.table::rbindlist(lapply(names(paths), function(variable_id) {
                docs <- shift_test_file_docs(
                    basename(paths[[variable_id]]),
                    opendap_url = paths[[variable_id]],
                    download_url = paths[[variable_id]],
                    variable_id = variable_id,
                    datetime_start = start,
                    datetime_end = end
                )
                docs$activity_id <- activity
                docs$source_id <- "BCC-CSM2-MR"
                docs$experiment_id <- experiment_id
                docs$grid_label <- "gn"
                docs$dataset_id <- sprintf("dataset-%s-%s", experiment_id, variable_id)
                docs$master_id <- sprintf("master-%s-%s", experiment_id, variable_id)
                docs$instance_id <- sprintf("instance-%s-%s.v1", experiment_id, variable_id)
                docs$tracking_id <- sprintf("hdl:test/%s-%s", experiment_id, variable_id)
                docs$id <- sprintf("%s-%s|%s", experiment_id, variable_id, docs$dataset_id)
                docs
            }), fill = TRUE)
        }), fill = TRUE)
    }
    future_docs <- workflow_docs(
        future_nc, c("ssp126", "ssp585"), "ScenarioMIP",
        "2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"
    )
    reference_docs <- workflow_docs(
        reference_nc, "historical", "CMIP",
        "1995-01-01T00:00:00Z", "1995-12-31T23:59:59Z"
    )

    calls <- new.env(parent = emptyenv())
    calls$file_calls <- 0L
    calls$historical_file_calls <- 0L
    calls$future_scenarios <- c("ssp126", "ssp585")
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            experiments <- as.character(shift_test_param_value(params, "experiment_id"))
            variables_requested <- as.character(shift_test_param_value(params, "variable_id"))
            experiments <- experiments[!is.na(experiments) & nzchar(experiments)]
            variables_requested <- variables_requested[!is.na(variables_requested) & nzchar(variables_requested)]
            if (identical(type, "Dataset")) {
                # File discovery is constrained through the selected Dataset
                # identity, so remember the preceding Dataset experiments for
                # the subsequent mocked File request.
                calls$dataset_experiments <- experiments
            }
            requested_experiments <- if (length(experiments)) experiments else calls$dataset_experiments
            docs <- if (identical(type, "Dataset")) {
                dataset <- shift_test_dataset_docs(if (length(variables_requested)) variables_requested[[1L]] else "tas")
                dataset$source_id <- "BCC-CSM2-MR"
                dataset$experiment_id <- if (length(experiments)) experiments[[1L]] else "ssp585"
                dataset
            } else {
                calls$file_calls <- calls$file_calls + 1L
                # Select historical fixtures only when the method explicitly
                # requested that experiment; baseline-reference runs never do.
                historical <- "historical" %in% requested_experiments
                if (historical) {
                    calls$historical_file_calls <- calls$historical_file_calls + 1L
                }
                catalog <- if (historical) reference_docs else future_docs
                if (!historical) {
                    catalog <- catalog[catalog$experiment_id %in% calls$future_scenarios]
                }
                if (length(requested_experiments) && !historical) {
                    catalog <- catalog[catalog$experiment_id %in% requested_experiments]
                }
                if (length(variables_requested)) {
                    catalog <- catalog[catalog$variable_id %in% variables_requested]
                }
                as.data.frame(catalog)
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- shift_test_response(docs)
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    store_path <- tempfile("shift-run-store-")
    output_dir <- tempfile("shift-run-output-")
    baseline_reference_run <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
            frequency = "day", table = "day", index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        method = belcher(),
        dir = tempfile("shift-run-baseline-reference-output-"),
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = tempfile("shift-run-baseline-reference-store-")
    )
    expect_equal(shift_status(baseline_reference_run), "completed")
    expect_equal(nrow(shift_outputs(baseline_reference_run)), 2L)
    expect_equal(calls$historical_file_calls, 0L)

    run <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
            frequency = "day", table = "day", index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        method = belcher(reference = historical_reference(1995L)),
        dir = output_dir,
        control = shift_control(strict = TRUE, overwrite = TRUE),
        store = store_path
    )

    expect_equal(shift_status(run), "completed")
    expect_equal(nrow(shift_outputs(run)), 2L)
    expect_equal(nrow(shift_missing(run)), 0L)
    expect_true(all(file.exists(shift_outputs(run)$export_path)))
    expect_true(all(vapply(shift_outputs(run)$export_path, function(path) {
        inherits(epw_file_read(path), "EpwFile")
    }, logical(1L))))
    expect_equal(calls$historical_file_calls, 1L)
    run_tables <- c("shift_run", "shift_run_case", "shift_run_event")
    expect_true(all(vapply(run_tables, function(table) {
        nrow(morpher__private_store(shift_store(run))$read_table(table)) >= 1L
    }, logical(1L))))
    expect_equal(nrow(shift_runs(store_path)), 1L)
    expect_equal(shift_status(shift_run_get(shift_ids(run)$run_id, store_path)), "completed")
    expect_equal(shift_ids(shift_resume(run))$run_id, shift_ids(run)$run_id)
    delivery_files <- list.files(output_dir, recursive = TRUE, all.files = TRUE)
    expect_false(any(grepl("\\.(duckdb|parquet|json)$", delivery_files)))

    calls$future_scenarios <- "ssp585"
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6(
                model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
                frequency = "day", table = "day", index_nodes = "https://example.org"
            ),
            periods = list(`2060s` = 2060L),
            method = belcher(reference = historical_reference(1995L)),
            dir = tempfile("shift-default-missing-output-"),
            store = tempfile("shift-default-missing-store-")
        ),
        class = "epwshiftr_shift_error"
    )

    partial <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
            frequency = "day", table = "day", index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2060L),
        method = belcher(reference = historical_reference(1995L)),
        dir = tempfile("shift-partial-output-"),
        control = shift_control(strict = TRUE, allow_partial = TRUE, overwrite = TRUE),
        store = tempfile("shift-partial-store-")
    )
    expect_equal(shift_status(partial), "partial")
    expect_equal(nrow(shift_outputs(partial)), 1L)
    expect_equal(nrow(shift_missing(partial)), 1L)
    expect_equal(shift_missing(partial)$experiment_id, "ssp126")

    calls$future_scenarios <- c("ssp126", "ssp585")
    resume_store <- tempfile("shift-resume-store-")
    export_attempts <- 0L
    original_export <- shift__export_outputs
    testthat::local_mocked_bindings(
        shift__export_outputs = function(...) {
            export_attempts <<- export_attempts + 1L
            if (export_attempts == 1L) {
                rlang::abort("simulated interruption after morphing")
            }
            original_export(...)
        },
        .package = "epwshiftr"
    )
    interrupted <- tryCatch(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6(
                model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
                frequency = "day", table = "day", index_nodes = "https://example.org"
            ),
            periods = list(`2060s` = 2060L),
            method = belcher(reference = historical_reference(1995L)),
            dir = tempfile("shift-resume-output-"),
            control = shift_control(strict = TRUE, overwrite = TRUE),
            store = resume_store
        ),
        epwshiftr_shift_error = identity
    )
    expect_s3_class(interrupted, "epwshiftr_shift_error")
    expect_null(interrupted$parent)
    expect_s3_class(interrupted$source_error, "error")
    expect_equal(lengths(regmatches(conditionMessage(interrupted),
        gregexpr("Future EPW run", conditionMessage(interrupted), fixed = TRUE))), 1L)
    expect_match(conditionMessage(interrupted), "Cause:")
    failed_run <- shift_run_get(interrupted$run_id, resume_store)
    expect_equal(shift_status(failed_run), "failed")
    expect_false(is.na(failed_run@meta$run$completed_at[[1L]]))
    expect_gt(nrow(shift_logs(failed_run)), 0L)
    file_calls_before_resume <- calls$file_calls
    resumed <- shift_resume(interrupted$run_id, store = resume_store)
    expect_equal(shift_status(resumed), "completed")
    expect_equal(calls$file_calls, file_calls_before_resume)
    expect_equal(nrow(shift_outputs(resumed)), 2L)
})

test_that("CMIP6 resolver preserves explicit member/grid choices and rejects ties", {
    variables <- epw_morph_variables("recommended")
    method <- shift_morph_method(suppressWarnings(epw_morph_recipe("belcher_absolute")))

    # Create two otherwise equivalent non-native grids so automatic preference
    # rules cannot choose one without user input.
    catalogs <- data.table::rbindlist(lapply(c("gr1", "gr2"), function(grid) {
        data.table::rbindlist(lapply(variables, function(variable_id) {
            docs <- shift_test_file_docs(sprintf("%s_%s.nc", variable_id, grid), variable_id = variable_id)
            docs$grid_label <- grid
            docs$id <- sprintf("%s-%s", variable_id, grid)
            docs$dataset_id <- sprintf("dataset-%s-%s", variable_id, grid)
            docs
        }), fill = TRUE)
    }), fill = TRUE)

    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("EC-Earth3", "ssp585", frequency = "day", table = "day"),
        periods = list(`2060s` = 2060L), method = method,
        dir = tempfile("resolver-output-"),
        store = tempfile("resolver-store-"), dry_run = TRUE
    )
    expect_error(
        shift__resolve_cmip6_selection(plan, catalogs),
        class = "epwshiftr_shift_resolution_ambiguity"
    )

    explicit <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r1i1p1f1", grid = "gr1",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L), method = method,
        dir = tempfile("resolver-explicit-output-"),
        store = tempfile("resolver-explicit-store-"), dry_run = TRUE
    )
    expect_equal(shift__resolve_cmip6_selection(explicit, catalogs)$grid_label, "gr1")

    missing_member <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r2i1p1f1", grid = "gr1",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L), method = method,
        dir = tempfile("resolver-member-output-"),
        store = tempfile("resolver-member-store-"), dry_run = TRUE
    )
    expect_error(
        shift__resolve_cmip6_selection(missing_member, catalogs),
        "No complete CMIP6 member/grid candidate"
    )
})

test_that("shift_morph() uses complete extraction plans by default", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    good <- shift_test_file_docs(basename(nc), opendap_url = nc, download_url = nc, variable_id = "tas")
    bad <- shift_test_file_docs(
        "hurs_missing_opendap.nc",
        opendap_url = "https://example.org/hurs_missing_opendap.nc",
        download_url = "https://example.org/hurs_missing_opendap.nc",
        variable_id = "hurs",
        include_opendap = FALSE
    )
    docs <- data.table::rbindlist(list(good, bad), fill = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test_mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, label = "singapore", epw = get_cache_epw())
    climate <- req |>
        shift_collect(store = tempfile("shift-store-"), label = "complete-subset") |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
            fallback = "error"
        )

    coverage <- shift_coverage(climate)
    expect_true(any(coverage$complete))
    expect_true(any(!coverage$complete))

    morph_recipe <- suppressWarnings(epw_morph_recipe("belcher_absolute", methods = c(tdb = "shift")))
    morphed <- shift_morph(climate, recipe = morph_recipe, strict = FALSE)
    blocked <- shift_morph(climate, recipe = morph_recipe, strict = FALSE, complete_only = FALSE, overwrite = TRUE)

    expect_equal(shift_ids(morphed)$plan_id, coverage$plan_id[coverage$complete])
    expect_true(any(shift_diagnostics(morphed)$code %in% "ignored_incomplete_extraction"))
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(blocked), "blocked")
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
            variable_id = variable_id,
            datetime_start = "1995-01-01T00:00:00Z",
            datetime_end = "1995-12-31T23:59:59Z"
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
    collect_count_before_baseline <- length(calls$collect_times)
    baseline_reference <- shift_morph(
        climate, recipe = recipe, strict = TRUE, overwrite = TRUE
    )
    expect_true(S7::S7_inherits(baseline_reference, ShiftMorphed))
    expect_null(baseline_reference@meta$reference)
    expect_equal(length(calls$collect_times), collect_count_before_baseline)
    morpher <- EpwMorpher$new(epw = get_cache_epw(), store = shift_store(climate), recipe = recipe)
    missing_reference <- morpher$preflight(
        plan_id = shift_ids(climate)$plan_id,
        periods = future_periods,
        strict = FALSE
    )
    expect_false(any(missing_reference$code == "missing_reference_climate"))
    auto <- shift_morph(
        climate,
        recipe = recipe,
        reference = shift_reference_historical(reference_periods),
        strict = TRUE,
        overwrite = TRUE
    )
    historical_collect_times <- calls$collect_times[3:4]
    expect_equal(vapply(historical_collect_times, `[[`, character(1L), "type"), c("Dataset", "File"))
    expect_true(all(vapply(historical_collect_times, function(x) {
        is.null(x$datetime_start) && is.null(x$datetime_stop)
    }, logical(1L))))
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
