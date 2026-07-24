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

shift_test_print_objects <- function(objects, width = 80L, n = 10L,
                                     verbose = FALSE) {
    for (object in objects) {
        print(object, width = width, n = n, verbose = verbose)
    }
    invisible(NULL)
}

shift_test_normalize_print <- function(x) {
    roots <- unique(c(tempdir(), normalizePath(tempdir(), winslash = "/",
        mustWork = FALSE)))
    for (root in roots[nzchar(roots)]) {
        x <- gsub(root, "<tempdir>", x, fixed = TRUE)
    }
    x
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
            calls$query_reporter <- c(calls$query_reporter,
                is.function(getOption("epwshiftr.query.progress_callback")))
            docs <- if (identical(type, "Dataset")) {
                calls$dataset_all <- c(calls$dataset_all, all)
                calls$dataset_limit <- c(calls$dataset_limit, limit)
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

test_that("shift_request() preserves provider facet values", {
    req <- shift_request(
        project = "cmip6",
        frequency = c("monthly", "daily", "3hr")
    )
    query <- shift_as_query(req)

    expect_identical(req@meta$project, "cmip6")
    expect_identical(req@meta$frequency, c("monthly", "daily", "3hr"))
    expect_identical(query_param__value(query$project()), "cmip6")
    expect_identical(query_param__value(query$frequency()),
        c("monthly", "daily", "3hr"))
})

test_that("ShiftRequest print shares the ESGF query renderer", {
    req <- shift_request(
        project = "CMIP6",
        experiment = c("ssp126", "ssp585"),
        variables = c("tas", "hurs"),
        frequency = "mon",
        filters = list(table_id = "Amon")
    )

    printed <- capture.output(print(req, width = 60L), type = "message")
    expect_true(any(grepl("ESGF request", printed, fixed = TRUE)))
    expect_true(any(grepl("^[=═]{2} ESGF request", printed)))
    expect_true(any(grepl("^[*•] Index node: auto", printed)))
    expect_true(any(grepl("Query parameters", printed, fixed = TRUE)))
    expect_true(any(grepl("experiment_id = ssp126, ssp585", printed, fixed = TRUE)))
    expect_true(any(grepl("table_id = Amon", printed, fixed = TRUE)))
    expect_false(any(grepl("stage:  request", printed, fixed = TRUE)))

    verbose <- capture.output(print(req, verbose = TRUE), type = "message")
    expect_true(any(grepl("Workflow", verbose, fixed = TRUE)))
    expect_true(any(grepl("Status: new", verbose, fixed = TRUE)))
})

test_that("Shift configuration printers use compact semantic receipts", {
    withr::local_options(cli.num_colors = 1L)
    climate <- shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585"))
    control <- shift_control()
    ui <- shift_ui()
    reference <- historical_reference(1995:2014)
    method <- belcher()
    site <- shift_site("SIN", 103.98, 1.37, label = "Singapore")

    climate_text <- capture.output(print(climate, width = 72L),
        type = "message")
    expect_true(any(grepl("CMIP6 Climate", climate_text, fixed = TRUE)))
    expect_true(any(grepl("6-node failover", climate_text, fixed = TRUE)))
    expect_true(any(grepl("Table: auto by variable", climate_text,
        fixed = TRUE)))
    expect_false(any(grepl("https://", climate_text, fixed = TRUE)))
    climate_verbose <- capture.output(print(climate, width = 72L,
        verbose = TRUE, n = 3L), type = "message")
    expect_true(any(grepl("https://", climate_verbose, fixed = TRUE)))
    expect_true(any(grepl("3 more rows", climate_verbose, fixed = TRUE)))

    reference_text <- capture.output(print(reference, width = 72L),
        type = "message")
    expect_true(any(grepl("reference 1995–2014", reference_text,
        fixed = TRUE)))
    expect_false(any(grepl("1996, 1997", reference_text, fixed = TRUE)))

    method_text <- capture.output(print(method, width = 72L),
        type = "message")
    expect_true(any(grepl("Reference: baseline EPW", method_text,
        fixed = TRUE)))
    expect_true(any(grepl("Profile: enhanced", method_text, fixed = TRUE)))
    expect_false(any(grepl("── Rules", method_text)))
    method_verbose <- capture.output(print(method, width = 72L,
        verbose = TRUE, n = 3L), type = "message")
    expect_true(any(grepl("Rules", method_verbose, fixed = TRUE)))
    expect_true(any(grepl("Options", method_verbose, fixed = TRUE)))
    expect_true(any(grepl("snow_depth", method_verbose, fixed = TRUE)))
    expect_true(any(grepl("11 more rows", method_verbose, fixed = TRUE)))

    visible <- NULL
    capture.output(visible <- withVisible(print(control, width = 72L)),
        type = "message")
    expect_false(visible$visible)
    expect_identical(visible$value, control)

    expect_snapshot(shift_test_print_objects(
        list(climate, control, ui, reference, method, site), width = 72L))
    expect_snapshot(shift_test_print_objects(
        list(climate, control, ui, reference, method, site),
        width = 100L, n = 3L, verbose = TRUE))
})

test_that("Shift scientific labels preserve profiles, table policy, and partitions", {
    expect_identical(
        shift__format_morph_method("belcher", belcher()@recipe),
        "belcher [enhanced]"
    )
    expect_identical(
        shift__format_morph_method(
            "belcher", list(backend = "belcher"),
            missing_belcher_profile = "legacy"
        ),
        "belcher [legacy]"
    )
    expect_identical(shift__format_cmip6_tables(NULL), "auto by variable")
    expect_identical(shift__format_cmip6_tables("Amon"), "Amon (forced)")
    expect_identical(
        shift__format_cmip6_tables(c(snd = "LImon")),
        "auto by variable · snd=LImon"
    )
    selection <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        partition_key = "Amon=gn;LImon=gr"
    )
    expect_identical(
        shift__format_cmip6_partitions(selection),
        "Amon=gn · LImon=gr"
    )

    climate <- shift_cmip6(
        "BCC-CSM2-MR", "ssp585", table = c(snd = "LImon")
    )
    encoded <- shift__spec_json(list(
        climate = shift__climate_spec_value(climate)
    ))
    decoded <- jsonlite::fromJSON(encoded, simplifyVector = TRUE)$climate
    expect_identical(decoded$table$snd, "LImon")
    expect_identical(
        shift__climate_from_spec(decoded)@table,
        c(snd = "LImon")
    )
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

test_that("historical workflow queries preserve years without exact datetime bounds", {
    reference_years <- 1995:2014
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "BCC-CSM2-MR", c("ssp126", "ssp585"),
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2055:2065),
        method = belcher(reference = historical_reference(reference_years)),
        dir = tempfile("historical-query-output-"),
        store = tempfile("historical-query-store-"),
        dry_run = TRUE
    )
    request <- shift__historical_request(plan, "https://example.org")
    query <- shift_as_esg_query(request)

    expect_null(request@meta$time)
    expect_equal(plan@meta$method@reference@periods$year, reference_years)
    expect_false(grepl("datetime_start|datetime_stop", query$url()))

    # Real monthly CMIP6 Dataset metadata uses representative mid-month
    # timestamps. A December 16 endpoint still covers the calendar year 2014.
    variables <- epw_morph_variables(plan@meta$method@recipe)
    reference_catalog <- data.table::rbindlist(lapply(variables, function(variable) {
        docs <- shift_test_file_docs(
            sprintf("historical_%s.nc", variable),
            variable_id = variable,
            datetime_start = "1850-01-16T12:00:00Z",
            datetime_end = "2014-12-16T12:00:00Z"
        )
        docs$source_id <- "BCC-CSM2-MR"
        docs$experiment_id <- "historical"
        docs$frequency <- "mon"
        docs$table_id <- "Amon"
        docs$grid_label <- "gn"
        docs
    }), fill = TRUE)
    candidates <- shift__cmip6_candidates(
        reference_catalog,
        models = "BCC-CSM2-MR", experiments = "historical",
        variables = variables, years = reference_years,
        frequency = "mon", table = "Amon"
    )
    expect_true(any(candidates$complete))

    error <- expect_error(
        shift__resolve_cmip6_selection(
            plan,
            future_catalog = data.table::data.table(),
            reference_catalog = data.table::data.table()
        ),
        class = "epwshiftr_shift_reference_catalog_empty"
    )
    expect_match(conditionMessage(error), "Historical reference catalog is empty")
    expect_match(conditionMessage(error), "1995–2014")
})

test_that("workflow File collection fills omitted ESGF times from DRS names", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    docs <- shift_test_file_docs(
        "tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_205501-206512.nc",
        datetime_start = NA_character_,
        datetime_end = NA_character_
    )
    shift_test_mock_collect(docs, calls)
    request <- shift_request(
        project = "CMIP6",
        source = "BCC-CSM2-MR",
        experiment = "ssp585",
        variables = "tas",
        frequency = "mon",
        time = c(2055L, 2065L),
        filters = list(table_id = "Amon", grid_label = "gn"),
        options = list(time_filter_method = "auto")
    )
    files <- shift_collect(
        request, store = tempfile("shift-drs-time-store-"))
    catalog <- shift_file_catalog(
        shift_store(files), shift_ids(files)$query_id)

    expect_false(is.na(catalog$datetime_start[[1L]]))
    expect_false(is.na(catalog$datetime_end[[1L]]))
    expect_equal(format(catalog$datetime_start[[1L]], "%Y", tz = "UTC"),
        "2055")
    expect_equal(format(catalog$datetime_end[[1L]], "%Y", tz = "UTC"),
        "2065")
})

test_that("resolver coverage defensively repairs cached catalogs without times", {
    catalog <- shift_test_file_docs(
        "tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_205501-206512.nc",
        variable_id = "tas",
        datetime_start = NA_character_,
        datetime_end = NA_character_
    )
    catalog$source_id <- "BCC-CSM2-MR"
    catalog$experiment_id <- "ssp585"
    catalog$frequency <- "mon"
    catalog$table_id <- "Amon"
    catalog$grid_label <- "gn"
    candidates <- shift__cmip6_candidates(
        catalog,
        models = "BCC-CSM2-MR",
        experiments = "ssp585",
        variables = "tas",
        years = 2055:2065,
        frequency = "mon",
        table = "Amon"
    )

    expect_true(candidates$complete[[1L]])
    expect_true(is.na(candidates$missing[[1L]]))
})

test_that("resolver inputs are not masked by provider convenience columns", {
    catalog <- data.table::as.data.table(shift_test_file_docs(
        "tas_day_Model-A_ssp245_r1i1p1f1_gn_20410101-20411231.nc",
        variable_id = "tas",
        datetime_start = "2041-01-01T00:00:00Z",
        datetime_end = "2041-12-31T23:59:59Z"
    ))
    catalog$source_id <- "Model-A"
    catalog$experiment_id <- "ssp245"
    catalog$variant_label <- "r1i1p1f1"
    catalog$frequency <- "day"
    catalog$table_id <- "day"
    catalog$grid_label <- "gn"
    # These provider aliases deliberately disagree with the canonical fields.
    catalog$variable <- "provider-variable"
    catalog$grid <- "provider-grid"
    identity <- data.table::data.table(
        source_id = "Model-A",
        variant_label = "r1i1p1f1"
    )

    expect_true(shift__cmip6_input_complete(
        catalog,
        identity,
        experiment = "ssp245",
        variable = "tas",
        table = "day",
        grid = "gn",
        years = 2041L
    ))
})

test_that("resolver satisfies canonical hurs only from direct data or huss plus tas and ps", {
    make_catalog <- function(variables) {
        data.table::rbindlist(lapply(variables, function(variable) {
            docs <- shift_test_file_docs(
                sprintf("%s_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_205501-206512.nc",
                    variable),
                variable_id = variable,
                datetime_start = "2055-01-01T00:00:00Z",
                datetime_end = "2065-12-31T23:59:59Z"
            )
            docs$source_id <- "BCC-CSM2-MR"
            docs$experiment_id <- "ssp126"
            docs$frequency <- "mon"
            docs$table_id <- "Amon"
            docs$grid_label <- "gn"
            docs
        }), fill = TRUE)
    }
    requirements <- list(hurs = list("hurs", c("huss", "tas", "ps")))
    candidates <- function(variables) {
        shift__cmip6_candidates(
            make_catalog(variables),
            models = "BCC-CSM2-MR",
            experiments = "ssp126",
            variables = unique(unlist(requirements, recursive = TRUE)),
            years = 2055:2065,
            frequency = "mon",
            table = "Amon",
            requirements = requirements
        )
    }

    expect_true(candidates("hurs")$complete[[1L]])
    expect_true(candidates(c("huss", "tas", "ps"))$complete[[1L]])
    psl_only <- candidates(c("huss", "tas", "psl"))
    expect_false(psl_only$complete[[1L]])
    expect_match(psl_only$missing[[1L]], "huss\\+tas\\+ps")
})

test_that("humidity fallback persists a canonical hurs extraction artifact", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    inputs <- c("huss", "tas", "ps")
    variables <- c(inputs, "snd")
    paths <- stats::setNames(vapply(variables, function(variable) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L,
            variable_id = variable)
        path
    }, character(1L)), variables)
    on.exit(unlink(paths), add = TRUE)
    docs <- data.table::rbindlist(lapply(variables, function(variable) {
        row <- shift_test_file_docs(
            basename(paths[[variable]]),
            opendap_url = paths[[variable]],
            download_url = paths[[variable]],
            variable_id = variable
        )
        row$master_id <- sprintf("humidity-%s", variable)
        row$tracking_id <- sprintf("hdl:test/humidity-%s", variable)
        row$id <- sprintf("humidity-%s|dataset", variable)
        row
    }), fill = TRUE)
    # Model the production layout where optional snow depth is a separate
    # complete identity that has no atmospheric humidity source variables.
    docs[variable_id == "snd", `:=`(
        frequency = "mon",
        table_id = "LImon"
    )]
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test_mock_collect(docs, calls)

    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37,
        epw = get_cache_epw())
    climate <- request |>
        shift_collect(store = tempfile("shift-derived-hurs-store-")) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            fallback = "error"
        )
    derived <- shift__derive_hurs_climate(
        climate, epw_morph_recipe("belcher")
    )
    coverage <- shift_coverage(derived)
    hurs <- coverage[variable_id == "hurs"]
    snd <- coverage[variable_id == "snd"]
    data <- shift_data(derived, variables = "hurs")

    expect_equal(nrow(hurs), 1L)
    expect_true(hurs$complete[[1L]])
    expect_equal(nrow(snd), 1L)
    expect_identical(snd$table_id[[1L]], "LImon")
    expect_true(all(data$units == "%"))
    expect_true(all(is.finite(data$value)))
    expect_true(all(data$derived_from == "huss,tas,ps"))
    expect_true(all(data$value > 0 & data$value < 150))

    store <- shift_store(derived)
    artifact_id <- store$query(sprintf(
        "SELECT artifact_id FROM extraction_result WHERE plan_id = %s LIMIT 1",
        ddb_literal(priv(store)$conn, hurs$plan_id[[1L]])
    ))$artifact_id[[1L]]
    artifact <- store$query(sprintf(
        "SELECT metadata_json FROM artifact WHERE artifact_id = %s",
        ddb_literal(priv(store)$conn, artifact_id)
    ))
    expect_match(artifact$metadata_json[[1L]], "huss,tas,ps")

    reused <- shift__derive_hurs_climate(
        derived, epw_morph_recipe("belcher"), resume = TRUE
    )
    expect_equal(shift_ids(reused)$plan_id, shift_ids(derived)$plan_id)
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
    dataset_store_path <- tempfile("shift-datasets-store-")
    dataset_output <- capture.output(
        datasets <- shift_datasets(req, store = dataset_store_path,
            ui = shift_ui("log")),
        type = "message"
    )
    expect_equal(datasets$count(), 1L)
    expect_true(any(grepl("Collect Datasets", dataset_output, fixed = TRUE)))
    expect_equal(calls$values, "Dataset")
    expect_true(calls$query_reporter[[1L]])
    dataset_run <- shift_run_get(datasets)
    expect_equal(shift_status(dataset_run), "completed")
    expect_equal(dataset_run@meta$run$task[[1L]], "datasets")
    expect_equal(shift_result(dataset_run)$count(), 1L)
    expect_true(nzchar(attr(datasets, "epwshiftr.run_id", exact = TRUE)))
    expect_true(nzchar(attr(datasets, "epwshiftr.step_id", exact = TRUE)))
    dataset_event <- dataset_run@meta$events[
        message == "Collecting Dataset catalog"]
    dataset_details <- jsonlite::fromJSON(
        dataset_event$details_json[[1L]], simplifyVector = TRUE)
    expect_identical(dataset_details$total, 1L)
    expect_error(shift_collect(req, store = tempfile("shift-store-"),
        progress = FALSE), "no longer accepts")

    files <- req |>
        shift_collect(store = store_path, label = "shift-test")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_equal(calls$values, c("Dataset", "Dataset", "File"))
    expect_true(all(calls$query_reporter))
    expect_identical(calls$file_fields[[1L]], "*")
    expect_equal(shift_status(files), "collected")
    expect_true(length(shift_ids(files)$query_id) == 1L)
    expect_true(nzchar(shift_ids(files)$run_id))
    expect_true(nzchar(shift_ids(files)$step_id))
    expect_equal(shift_status(shift_run_get(files)), "waiting")
    collect_run <- shift_run_get(files)
    collect_event <- collect_run@meta$events[
        message == "Collecting Dataset catalog"]
    collect_details <- jsonlite::fromJSON(
        collect_event$details_json[[1L]], simplifyVector = TRUE)
    expect_identical(collect_details$total, 2L)
    expect_error(shift_resume(files), "waiting for its next stage")
    expect_equal(nrow(data.table::as.data.table(files)), 1L)
    expect_equal(shift_datasets(files)$count(), 1L)
    file_result <- shift_files(files)
    expect_s3_class(file_result, "EsgResultFile")
    expect_equal(file_result$count(), 1L)
    expect_equal(file_result$filename, "tas_day.nc")
    expect_error(shift_files(req), "No File result")
    expect_named(shift_check(files, strict = TRUE), shift_diagnostic_columns())
    expect_equal(shift_status(shift_refresh(files)), "collected")

    printed <- capture.output(print(files), type = "message")
    expect_true(any(grepl("ESGF Query Result [File]", printed, fixed = TRUE)))
    expect_true(any(grepl("^[=═]{2} ESGF Query Result", printed)))
    expect_true(any(grepl("^[*•] Index Node:", printed)))
    expect_true(any(grepl("^[*•] Result count: 1", printed)))
    expect_true(any(grepl("^[*•] Fields:", printed)))
    expect_true(any(grepl("EC-Earth3", printed, fixed = TRUE)))
    expect_true(any(grepl("tas", printed, fixed = TRUE)))
    expect_false(any(grepl("<char>", printed, fixed = TRUE)))
    expect_false(any(grepl("Catalog:", printed, fixed = TRUE)))
    expect_false(any(grepl("Identity:", printed, fixed = TRUE)))

    verbose <- capture.output(print(files, width = 60L, verbose = TRUE), type = "message")
    expect_true(any(grepl("Workflow", verbose, fixed = TRUE)))
    expect_true(any(grepl("Store:", verbose, fixed = TRUE)))
    expect_true(any(grepl("Hidden columns", verbose, fixed = TRUE)))

    detached <- shift_stage_new(
        ShiftFiles,
        "files",
        store_path = tempfile("missing-shift-store-"),
        ids = list(query_id = "query-detached"),
        meta = list(
            request = req,
            file_count = 1L,
            result_fields = c("source_id", "variable_id")
        )
    )
    detached_text <- capture.output(print(detached), type = "message")
    expect_true(any(grepl("Result count: 1", detached_text, fixed = TRUE)))
    expect_true(any(grepl("Persisted preview unavailable", detached_text,
        fixed = TRUE)))

    capped_calls <- new.env(parent = emptyenv())
    capped_calls$values <- character()
    capped_calls$file_fields <- list()
    capped_calls$dataset_all <- logical()
    capped_calls$dataset_limit <- list()
    shift_test_mock_collect(shift_test_file_docs("tas_day.nc"), capped_calls)
    shift_collect(req, store = tempfile("shift-capped-store-"), limit = 10L,
        ui = shift_ui("none"))
    expect_false(capped_calls$dataset_all[[1L]])
    expect_identical(capped_calls$dataset_limit[[1L]], 10L)

    store <- shift_store(files)
    store$add_files(shift_test_file_result(shift_test_file_docs("hurs_day.nc", variable_id = "hurs")))
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    expect_identical(shift_ids(dl)$run_id, shift_ids(files)$run_id)
    expect_false(identical(shift_ids(dl)$step_id, shift_ids(files)$step_id))
    expect_equal(nrow(data.table::as.data.table(dl)), 1L)
    expect_equal(shift_datasets(dl)$count(), 1L)
    expect_equal(shift_files(dl)$filename, "tas_day.nc")

    rds <- tempfile(fileext = ".rds")
    saveRDS(files, rds)
    restored <- readRDS(rds)
    expect_equal(shift_status(restored), "collected")
    expect_equal(nrow(data.table::as.data.table(restored)), 1L)

    completed <- shift_complete(dl)
    expect_equal(shift_status(completed), "partial")
})

test_that("empty collection is partial instead of waiting for another stage", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    calls$dataset_all <- logical()
    calls$dataset_limit <- list()
    empty <- shift_test_file_docs("empty.nc")[0, , drop = FALSE]
    shift_test_mock_collect(empty, calls)

    files <- shift_collect(
        shift_request(project = "CMIP6", frequency = "mon"),
        store = tempfile("shift-empty-store-"),
        ui = shift_ui("none")
    )

    expect_identical(shift_status(files), "partial")
    expect_identical(shift_status(shift_run_get(files)), "partial")
})

test_that("failed standalone steps expose recovery identity and resume in place", {
    skip_if_not_installed("duckdb")

    attempts <- 0L
    file_docs <- shift_test_file_docs("tas_day.nc")
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params,
                                  required_fields = NULL, all = FALSE,
                                  limit = TRUE, constraints = TRUE,
                                  dict_check = FALSE) {
            attempts <<- attempts + 1L
            if (attempts == 1L) stop("temporary catalog failure")
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                shift_test_dataset_docs()
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) fields <- names(docs)
            params$fields(unique(c(fields, required_fields)))
            response <- shift_test_response(docs)
            list(response = response, docs = response$response$docs,
                parameter = params)
        },
        .package = "epwshiftr"
    )
    store_path <- tempfile("shift-resume-stage-store-")
    request <- shift_request(project = "CMIP6", experiment = "ssp585",
        variables = "tas", frequency = "day")
    failure <- tryCatch(
        shift_collect(request, store = store_path, ui = shift_ui("none")),
        epwshiftr_shift_error = identity
    )

    expect_s3_class(failure, "epwshiftr_shift_error")
    expect_match(failure$run_id, "^run_")
    expect_match(failure$step_id, "^step_")
    expect_identical(failure$store,
        normalizePath(store_path, winslash = "/", mustWork = TRUE))
    expect_equal(shift_status(shift_run_get(failure$run_id,
        store = store_path)), "failed")

    resumed <- shift_resume(failure$run_id, store = store_path,
        ui = shift_ui("none"))
    expect_s7_class(resumed, ShiftFiles)
    expect_identical(shift_ids(resumed)$run_id, failure$run_id)
    expect_equal(shift_status(shift_run_get(resumed)), "waiting")
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
    expect_identical(shift_ids(climate)$run_id, shift_ids(files)$run_id)
    expect_false(identical(shift_ids(climate_resumed)$run_id,
        shift_ids(files)$run_id))
    expect_identical(shift_ids(morphed)$run_id, shift_ids(climate)$run_id)
    expect_identical(shift_ids(epws)$run_id, shift_ids(morphed)$run_id)
    expect_equal(shift_status(shift_run_get(epws)), "waiting")
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
    epw_run <- shift_run_get(epws)
    expect_identical(epw_run@ids$query_id, shift_ids(files)$query_id)
    expect_identical(epw_run@ids$morph_id, shift_ids(morphed)$morph_id)
    expect_true(S7::S7_inherits(shift_result(epw_run),
        ShiftOutputs))
    expect_error(shift_complete(climate), "not the latest result")
    expect_equal(shift_status(shift_complete(epws)), "completed")
})

test_that("standalone shift APIs carry run context without session arguments", {
    apis <- list(shift_datasets, shift_collect, shift_download, shift_extract,
        shift_morph, shift_epw, shift_export_epw)
    for (api in apis) {
        arguments <- names(formals(api))
        expect_false("session" %in% arguments)
        expect_false(".reporter" %in% arguments)
    }
    expect_false("progress" %in% names(formals(shift_datasets)))
    expect_true(all(c("store", "ui") %in% names(formals(shift_datasets))))
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

test_that("Shift display paths compact Windows temp roots lexically", {
    root <- "C:\\Users\\runneradmin\\AppData\\Local\\Temp\\Rtmp123"
    path <- paste0(
        "c:/users/runneradmin/AppData/Local/Temp/Rtmp123/",
        "shift-print-output"
    )
    expect_identical(
        shift__display_path(path, temp_root = root),
        "<tempdir>/shift-print-output"
    )
})


test_that("Shift plan and stage printers use bounded semantic previews", {
    withr::local_options(cli.num_colors = 1L)
    site <- shift_site("SIN", 103.98, 1.37, label = "Singapore")
    periods <- epw_morph_periods(`2060s` = 2055:2065)
    method <- belcher(reference = historical_reference(1995:2014))
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
        periods = periods,
        method = method,
        dir = file.path(tempdir(), "shift-print-output"),
        store = file.path(tempdir(), "shift-print-store"),
        dry_run = TRUE
    )
    tasks <- data.table::data.table(
        status = c("done", "done", "queued", "error"),
        filename = sprintf("tas_%02d.nc", 1:4),
        bytes_done = c(100, 200, 0, 20),
        size = c(100, 200, 300, 400),
        speed_bps = c(10, 20, 0, 0),
        eta_seconds = c(0, 0, 30, NA),
        data_node = "example.org",
        attempts = c(1L, 1L, 0L, 2L),
        last_error = c(NA, NA, NA, "connection failed")
    )
    download <- shift_stage_new(
        ShiftDownload, "download",
        ids = list(session_id = "session-print"),
        meta = list(session = tasks)
    )
    coverage <- data.table::data.table(
        complete = c(TRUE, TRUE, FALSE),
        status = c("done", "done", "failed"),
        experiment_id = c("ssp126", "ssp585", "ssp585"),
        variable_id = c("tas", "tas", "hurs"),
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        time_start = as.POSIXct("2055-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2065-12-31", tz = "UTC"),
        output_time_count = c(132L, 132L, 0L),
        output_rows = c(132L, 132L, 0L),
        last_error = c(NA, NA, "missing years")
    )
    climate <- shift_stage_new(
        ShiftClimate, "climate",
        meta = list(site = site, periods = periods, coverage = coverage)
    )
    morph_rows <- data.table::data.table(
        case_id = sprintf("case-%d", 1:4),
        source_id = "BCC-CSM2-MR",
        experiment_id = rep(c("ssp126", "ssp585"), each = 2L),
        variant_label = "r1i1p1f1",
        period = "2060s",
        status = "result_done",
        row_count = 8760L,
        output_path = sprintf("morph/case-%d.parquet", 1:4)
    )
    morphed <- shift_stage_new(
        ShiftMorphed, "morphed",
        meta = list(
            recipe = method@recipe,
            reference_spec = method@reference,
            results = morph_rows
        )
    )
    output_rows <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        experiment_id = rep(c("ssp126", "ssp585"), 6L),
        variant_label = "r1i1p1f1",
        period = "2060s",
        path = sprintf("outputs/future-%02d.epw", 1:12),
        export_path = sprintf("/exports/future-%02d.epw", 1:12),
        created_at = as.POSIXct("2026-01-01", tz = "UTC")
    )
    outputs <- shift_stage_new(
        ShiftOutputs, "outputs",
        meta = list(outputs = output_rows, export_dir = "/exports")
    )

    output_text <- capture.output(print(outputs, width = 72L, n = 3L),
        type = "message")
    expect_true(any(grepl("9 more rows", output_text, fixed = TRUE)))
    output_default <- capture.output(print(outputs, width = 72L),
        type = "message")
    expect_true(any(grepl("2 more rows", output_default, fixed = TRUE)))
    output_all <- capture.output(print(outputs, width = 72L, n = Inf),
        type = "message")
    expect_false(any(grepl("more rows", output_all, fixed = TRUE)))

    for (width in c(60L, 80L, 120L)) {
        for (object in list(plan, download, climate, morphed, outputs)) {
            lines <- cli::ansi_strip(capture.output(
                print(object, width = width, n = 3L), type = "message"))
            expect_lte(max(cli::ansi_nchar(lines, type = "width")), width)
        }
    }

    expect_snapshot(shift_test_print_objects(
        list(plan, download, climate, morphed, outputs),
        width = 80L, n = 3L), transform = shift_test_normalize_print)
    expect_snapshot(shift_test_print_objects(
        list(plan, download, climate, morphed, outputs),
        width = 100L, n = 3L, verbose = TRUE),
        transform = shift_test_normalize_print)
})

test_that("ShiftRun print refreshes state and reuses the static dashboard", {
    skip_if_not_installed("duckdb")
    withr::local_options(cli.num_colors = 1L)
    store_path <- tempfile("shift-print-run-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
        periods = list(`2060s` = 2055:2065),
        method = belcher(reference = historical_reference(1995:2014)),
        dir = tempfile("shift-print-run-output-"),
        store = store_path,
        dry_run = TRUE
    )
    run_id <- shift__run_register(plan)
    stale <- shift_run_get(run_id, store = store_path)
    store <- shift_store(plan)
    shift__run_update(store, run_id, status = "waiting",
        current_stage = "collect")
    store$close()

    printed <- capture.output(print(stale, width = 72L), type = "message")
    # The persisted state remains `waiting`; the receipt intentionally uses the
    # user-facing READY label to describe an interactive continuation point.
    expect_true(any(grepl("READY", printed, fixed = TRUE)))
    expect_true(any(grepl("Future EPW", printed, fixed = TRUE)))
    expect_true(any(grepl("Cases", printed, fixed = TRUE)))
    expect_false(any(grepl("MemberNA|StatusNA|plannedNA", printed)))
    expect_lte(max(cli::ansi_nchar(cli::ansi_strip(printed), type = "width")),
        72L)

    view <- shift__ui_run_view(shift_refresh(stale), width = 72L,
        detail = "normal", motion = "none")
    direct <- capture.output(shift__ui_print_view(view,
        include_tables = TRUE), type = "message")
    expect_identical(printed, direct)
})

test_that("ShiftRun print falls back to a cached static snapshot", {
    withr::local_options(cli.num_colors = 1L)
    spec <- list(
        task = "future_epw",
        climate = list(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            member = NULL,
            grid = NULL
        ),
        periods = list(`2060s` = 2055:2065),
        method = list(
            name = "belcher",
            reference_mode = "historical",
            reference = list(periods = list(reference = 1995:2014))
        ),
        control = list(download = "auto")
    )
    run_row <- data.table::data.table(
        run_id = "run_print_12345678",
        task = "future_epw",
        spec_json = jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"),
        status = "completed",
        current_stage = "write_epw",
        output_dir = "/exports",
        started_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        updated_at = as.POSIXct("2026-01-01 00:00:05", tz = "UTC"),
        completed_at = as.POSIXct("2026-01-01 00:00:05", tz = "UTC"),
        last_error = NA_character_
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        variant_label = NA_character_,
        period = "2060s",
        status = "completed",
        required = TRUE,
        export_path = c("/exports/ssp126.epw", "/exports/ssp585.epw")
    )
    events <- data.table::data.table(
        stage = character(), status = character(), message = character(),
        details_json = character(), created_at = as.POSIXct(character(),
            tz = "UTC")
    )
    run <- shift_stage_new(
        ShiftRun, "run",
        ids = list(run_id = "run_print_12345678"),
        meta = list(run = run_row, cases = cases, events = events)
    )

    printed <- capture.output(print(run, width = 72L), type = "message")
    expect_true(any(grepl("COMPLETED", printed, fixed = TRUE)))
    expect_true(any(grepl("Persisted preview unavailable", printed,
        fixed = TRUE)))
    expect_false(any(grepl("MemberNA|StatusNA|completedNA", printed)))
    limited <- capture.output(print(run, width = 72L, n = 1L),
        type = "message")
    expect_true(any(grepl("1 more case", limited, fixed = TRUE)))
    expect_snapshot(print(run, width = 72L))
    expect_snapshot(print(run, width = 100L, verbose = TRUE))
})

test_that("shift_ui() validates presentation options without changing scientific intent", {
    expect_true(S7::S7_inherits(shift_ui(), ShiftUiOptions))
    expect_equal(shift_ui("log", detail = "detail", heartbeat = 2)@progress, "log")
    expect_equal(shift_ui(detail = "debug")@detail, "debug")
    expect_equal(shift_ui(motion = "reduced")@motion, "reduced")
    expect_equal(shift_ui(refresh = 0.2)@refresh, 0.2)
    expect_error(shift_ui("invalid"), "arg")
    expect_error(shift_ui(motion = "invalid"), "arg")
    expect_error(shift_ui(refresh = 0.01), "not >= 0.05")
    expect_error(shift_ui(heartbeat = -1), "not >= 0")
    expect_equal(shift__ui_mode(shift_ui("none")), "none")
    expect_equal(shift__ui_motion(shift_ui("dynamic", motion = "auto")), "full")
    expect_equal(shift__ui_motion(shift_ui("log", motion = "full")), "none")

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
        shift__plan_spec(make_plan(shift_ui("log", detail = "debug",
            motion = "full", refresh = 0.05, heartbeat = 1)))
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

test_that("ShiftReporter submits one complete frame per dynamic refresh", {
    frames <- list()
    compacts <- character()
    closes <- 0L
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(lines, compact = NULL) {
                frames[[length(frames) + 1L]] <<- lines
                compacts <<- c(compacts, compact)
                TRUE
            },
            suspend = function(code) code(),
            close = function(...) {
                closes <<- closes + 1L
                invisible(NULL)
            }
        ),
        .package = "epwshiftr"
    )

    reporter <- shift__reporter(shift_ui("dynamic", heartbeat = 0))
    reporter$stage_started("resolve", "Resolving inputs.")
    reporter$unit_started("Trying node one", current = 1L, total = 2L)
    expect_silent(reporter$heartbeat("Waiting for future catalog", force = TRUE))
    expect_equal(length(frames), 3L)
    expect_true(all(lengths(frames) == 11L))
    expect_length(compacts, 3L)
    expect_true(all(nzchar(compacts)))
    expect_message(reporter$unit_completed(
        "Node one failed", current = 1L, total = 2L, outcome = "failed"
    ), "Node one failed")
    reporter$close()
    expect_equal(closes, 1L)
})

test_that("ShiftReporter falls back to logs when frame painting fails", {
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(...) FALSE,
            suspend = function(code) code(),
            close = function(...) invisible(NULL)
        ),
        .package = "epwshiftr"
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
    logs <- shift_logs(run)
    expect_gt(nrow(logs), 0L)
    expect_true(all(logs$source == "event"))
    terminal <- run@meta$events[status %in% c("cancelled", "failed")]
    expect_equal(terminal$status, "cancelled")
    expect_equal(terminal$message, "Interrupted by user.")
})

test_that("background live sidecars carry transient reporter state without events", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-live-ui-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "EC-Earth3", "ssp585", member = "r1i1p1f1", grid = "gr",
            frequency = "day", table = "day"
        ),
        periods = list(`2060s` = 2060L),
        method = shift_morph_method(suppressWarnings(
            epw_morph_recipe("belcher_absolute"))),
        dir = tempfile("shift-live-ui-output-"),
        store = store_path,
        dry_run = TRUE
    )
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    job <- shift__job_create(store, run_id, mode = "foreground",
        ui = shift_ui("none", heartbeat = 0))
    initial_events <- nrow(morpher__private_store(store)$read_table(
        "shift_run_event"))
    reporter <- shift__reporter(
        shift_ui("none", heartbeat = 0), store = store,
        run_id = run_id, job_id = job$job_id[[1L]]
    )
    reporter$heartbeat(
        "Reading tas",
        details = list(
            stage = "extract_future", unit_type = "extraction_plan",
            scenario = "ssp585", variable = "tas",
            access_method = "OPeNDAP", transfer_state = "waiting"
        ),
        force = TRUE
    )

    live <- shift__live_run_get(run_id, store_path)
    expect_s7_class(live, ShiftRun)
    expect_identical(live@meta$ui_state$current_details$variable, "tas")
    expect_identical(live@meta$ui_state$current_details$access_method,
        "OPeNDAP")
    expect_equal(nrow(morpher__private_store(store)$read_table(
        "shift_run_event")), initial_events)
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
    expect_equal(nodes$result, "coverage: missing hurs")
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
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none",
        motion = "reduced", refresh = 0.25, heartbeat = 7))
    expect_equal(shift_status(run), "queued")
    expect_equal(launched$args$run_id, shift_ids(run)$run_id)
    expect_true(startsWith(launched$args$log_path, normalizePath(store_path, winslash = "/")))
    expect_equal(run@meta$jobs$mode, "process")
    expect_equal(run@meta$jobs$status, "queued")
    ui_spec <- jsonlite::fromJSON(run@meta$jobs$ui_json[[1L]])
    expect_identical(ui_spec$motion, "reduced")
    expect_equal(ui_spec$refresh, 0.25)
    expect_equal(ui_spec$heartbeat, 7)
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

    # Exercise the production fallback for both future scenarios and the
    # explicit historical reference instead of supplying direct hurs.
    variables <- unique(c(
        setdiff(epw_morph_variables("recommended"), "hurs"),
        "huss", "ps"
    ))
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
    missing_store <- tempfile("shift-default-missing-store-")
    missing_error <- tryCatch(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6(
                model = "BCC-CSM2-MR", scenarios = c("ssp126", "ssp585"),
                frequency = "day", table = "day", index_nodes = "https://example.org"
            ),
            periods = list(`2060s` = 2060L),
            method = belcher(reference = historical_reference(1995L)),
            dir = tempfile("shift-default-missing-output-"),
            store = missing_store,
            ui = shift_ui("none")
        ),
        epwshiftr_shift_error = identity
    )
    expect_s3_class(missing_error, "epwshiftr_shift_error")
    expect_null(conditionCall(missing_error))
    expect_match(conditionMessage(missing_error), "1 node checked",
        fixed = TRUE)
    expect_match(conditionMessage(missing_error),
        "Resuming this request unchanged", fixed = TRUE)
    expect_false(grepl("Retry:", conditionMessage(missing_error), fixed = TRUE))
    missing_run <- shift_run_get(missing_error$run_id, missing_store)
    missing_diagnostics <- shift_diagnostics(missing_run)
    expect_identical(missing_diagnostics$code,
        "shift_resolver_exhausted")
    expect_match(missing_diagnostics$message, "ssp126", fixed = TRUE)
    expect_match(missing_diagnostics$action, "resuming unchanged",
        fixed = TRUE)

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
                stop("simulated interruption after morphing", call. = FALSE)
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
    expect_null(conditionCall(interrupted))
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

test_that("resolver exhaustion preserves closest candidate and recovery semantics", {
    future <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        frequency = "mon",
        table_id = "Amon",
        complete = FALSE,
        missing = paste(
            "ssp126/hurs: no files;",
            "ssp585/tas: missing years 2055"
        )
    )
    reference <- data.table::rbindlist(list(
        data.table::copy(future),
        data.table::copy(future)[, variant_label := "r2i1p1f1"]
    ))
    reference[, `:=`(complete = TRUE, missing = NA_character_)]
    node_diagnostic <- shift__cmip6_resolution_diagnostic(
        future, reference, "BCC-CSM2-MR", reference_required = TRUE
    )
    records <- list(
        list(
            node = "DKRZ", kind = "coverage",
            future_files = 28L, reference_files = 39L,
            resolution = node_diagnostic
        ),
        list(
            node = "IPSL", kind = "timeout",
            future_files = NA_integer_, reference_files = NA_integer_,
            resolution = NULL
        )
    )
    aggregate <- shift__resolver_failure_diagnostic(records)
    condition <- tryCatch(
        shift__abort_resolver_exhausted(records),
        epwshiftr_shift_resolver_exhausted = identity
    )

    expect_identical(node_diagnostic$reason, "future_incomplete")
    # The reference-only r2 identity has one generic unavailable marker, but
    # r1 is the real future near-match and must remain the diagnostic identity.
    expect_identical(node_diagnostic$closest$member, "r1i1p1f1")
    expect_match(node_diagnostic$missing[[1L]], "ssp126/hurs", fixed = TRUE)
    expect_equal(aggregate$nodes_checked, 2L)
    expect_equal(aggregate$coverage_failures, 1L)
    expect_equal(aggregate$timeout_failures, 1L)
    expect_false(aggregate$retryable)
    expect_identical(aggregate$recovery, "inspect")
    expect_s3_class(condition, "epwshiftr_shift_resolution_error")
    expect_null(conditionCall(condition))
    expect_match(conditionMessage(condition), "2 nodes checked", fixed = TRUE)
})

test_that("resolver recommends retry only when every node failure is transient", {
    records <- list(
        list(node = "IPSL", kind = "timeout", future_files = NA_integer_,
            reference_files = NA_integer_, resolution = NULL),
        list(node = "LIU", kind = "network", future_files = NA_integer_,
            reference_files = NA_integer_, resolution = NULL)
    )
    diagnostic <- shift__resolver_failure_diagnostic(records)

    expect_true(diagnostic$retryable)
    expect_identical(diagnostic$recovery, "retry")
})

test_that("failure commands are concise for default stores and explicit otherwise", {
    default_store <- tempfile("shift-default-command-store-")
    custom_store <- tempfile("shift-custom-command-store-")
    withr::local_options(epwshiftr.dir_store = default_store)

    expect_identical(
        shift__run_command("shift_resume", "run-test", default_store),
        'shift_resume("run-test")'
    )
    expect_identical(
        shift__run_command("shift_logs", "run-test", default_store,
            "tail = 20L"),
        'shift_logs("run-test", tail = 20L)'
    )
    custom <- shift__run_command("shift_resume", "run-test", custom_store)
    expect_match(custom, 'shift_resume\\("run-test", store = ', perl = TRUE)
    expect_match(custom, store_normalize_path(custom_store), fixed = TRUE)
})

test_that("resolution evidence tolerates omitted aggregate counters", {
    evidence <- shift__resolution_evidence(list(
        summary = "Selection incomplete",
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "future: ssp585/hurs"
    ))

    expect_length(evidence, 2L)
    expect_match(evidence[[1L]], "BCC-CSM2-MR/r1i1p1f1/gn", fixed = TRUE)
    expect_match(evidence[[2L]], "ssp585/hurs", fixed = TRUE)
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
    expect_equal(shift_status(shift_complete(morphed)), "partial")
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
