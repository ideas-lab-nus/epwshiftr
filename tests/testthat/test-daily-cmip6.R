# Build compact daily File-catalog rows for deterministic availability tests.
daily_test__catalog <- function(model = "Model-A", experiment,
                                variables, years,
                                member = "r1i1p1f1", grid = "gn",
                                opendap = TRUE, http = TRUE) {
    data.table::rbindlist(lapply(variables, function(variable) {
        data.table::data.table(
            source_id = model,
            experiment_id = experiment,
            variant_label = member,
            grid_label = grid,
            frequency = "day",
            table_id = "day",
            variable_id = variable,
            datetime_start = sprintf(
                "%d-01-01T00:00:00Z", min(years)),
            datetime_end = sprintf(
                "%d-12-31T23:59:59Z", max(years)),
            latest = TRUE,
            replica = FALSE,
            retracted = FALSE,
            deprecated = FALSE,
            data_node = "example.org",
            filename = sprintf(
                "%s_day_%s_%s_%s_%s_%d0101-%d1231.nc",
                variable, model, experiment, member, grid,
                min(years), max(years)),
            size = 123,
            url_opendap = if (opendap) {
                sprintf("https://example.org/opendap/%s", variable)
            } else {
                NA_character_
            },
            url_download = if (http) {
                sprintf("https://example.org/http/%s", variable)
            } else {
                NA_character_
            }
        )
    }), use.names = TRUE, fill = TRUE)
}

# Build Dataset-level discovery rows using the member_id alias commonly returned
# by CMIP6 index nodes and deliberately omit File-level time fields.
daily_test__datasets <- function(model = "Model-A", experiment, variables,
                                 member = "r1i1p1f1", grid = "gn") {
    data.table::rbindlist(lapply(variables, function(variable) {
        data.table::data.table(
            source_id = model,
            experiment_id = experiment,
            member_id = member,
            grid_label = grid,
            frequency = "day",
            table_id = "day",
            variable_id = variable,
            latest = TRUE,
            replica = FALSE,
            data_node = "example.org",
            instance_id = sprintf(
                "CMIP6.%s.%s.%s.%s.%s",
                model, experiment, member, variable, grid)
        )
    }), use.names = TRUE, fill = TRUE)
}

# Load the installed probe implementation so source, package-check, and
# coverage environments exercise the same script without repository paths.
daily_test__probe_environment <- function() {
    script <- system.file(
        "tools",
        "probe-daily-cmip6-availability.R",
        package = "epwshiftr",
        mustWork = TRUE
    )
    environment <- new.env(parent = globalenv())
    sys.source(script, envir = environment)
    environment
}

test_that("daily profiles declare the M1 variable contracts", {
    core <- daily__requirements("core")
    enhanced <- daily__requirements("enhanced")

    expect_named(
        core,
        c("tas", "hurs", "pr", "rsds", "rlds", "sfcWind"))
    expect_named(
        enhanced,
        c(
            "tas", "tasmax", "tasmin", "hurs",
            "pr", "rsds", "rlds", "sfcWind"
        ))
    expect_identical(
        enhanced$hurs,
        list(c("huss", "tas", "ps"), "hurs"))
    expect_setequal(
        daily__input_variables("enhanced"),
        c(
            "tas", "tasmax", "tasmin", "huss", "ps", "hurs",
            "pr", "rsds", "rlds", "sfcWind"
        ))
})

test_that("daily evaluation finds complete shared identities", {
    variables <- daily__input_variables("enhanced")
    future <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "ssp245", variables = variables,
            years = 2041:2070),
        daily_test__catalog(
            experiment = "ssp585", variables = variables,
            years = 2041:2070)
    ))
    historical <- daily_test__catalog(
        experiment = "historical", variables = variables,
        years = 1995:2014)

    result <- daily__evaluate_catalogs(
        future,
        historical,
        models = c("Model-A", "Model-B"),
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_true(all(result$summary[source_id == "Model-A"]$complete))
    expect_true(all(
        result$summary[source_id == "Model-B"]$status == "absent"))
    expect_true(all(grepl(
        "hurs=huss\\+tas\\+ps",
        result$summary[source_id == "Model-A"]$requirement_key
    )))
    expect_true(all(result$variable_coverage$complete))
    expect_true(all(result$variable_coverage$opendap_file_count == 1L))
    expect_true(all(result$variable_coverage$http_file_count == 1L))
})

test_that("daily evaluation accepts direct hurs consistently", {
    variables <- setdiff(
        daily__input_variables("enhanced"), c("huss", "ps"))
    future <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "ssp245", variables = variables,
            years = 2041:2070),
        daily_test__catalog(
            experiment = "ssp585", variables = variables,
            years = 2041:2070)
    ))
    historical <- daily_test__catalog(
        experiment = "historical", variables = variables,
        years = 1995:2014)

    result <- daily__evaluate_catalogs(
        future,
        historical,
        models = "Model-A",
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_true(all(result$summary$complete))
    expect_true(all(grepl(
        "hurs=hurs", result$summary$requirement_key, fixed = TRUE)))
})

test_that("daily evaluation rejects humidity-path mixing across periods", {
    all_variables <- daily__input_variables("enhanced")
    future_variables <- setdiff(all_variables, "hurs")
    historical_variables <- setdiff(
        all_variables, c("huss", "ps"))
    future <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "ssp245", variables = future_variables,
            years = 2041:2070),
        daily_test__catalog(
            experiment = "ssp585", variables = future_variables,
            years = 2041:2070)
    ))
    historical <- daily_test__catalog(
        experiment = "historical", variables = historical_variables,
        years = 1995:2014)

    result <- daily__evaluate_catalogs(
        future,
        historical,
        models = "Model-A",
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_false(any(result$summary$complete))
    expect_false(any(result$intersections$both_present))
    expect_setequal(
        unique(result$intersections$status),
        c("future_absent", "historical_absent"))
    expect_true(all(nzchar(result$intersections$future_missing)))
    expect_true(all(nzchar(result$intersections$historical_missing)))
})

test_that("enhanced availability can fail while core remains complete", {
    variables <- daily__input_variables("enhanced")
    future <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "ssp245", variables = variables,
            years = 2041:2070),
        daily_test__catalog(
            experiment = "ssp585",
            variables = setdiff(variables, "tasmin"),
            years = 2041:2070)
    ))
    historical <- daily_test__catalog(
        experiment = "historical", variables = variables,
        years = 1995:2014)

    result <- daily__evaluate_catalogs(
        future,
        historical,
        models = "Model-A",
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_true(result$summary[profile == "core"]$complete)
    expect_false(result$summary[profile == "enhanced"]$complete)
    expect_match(
        result$summary[profile == "enhanced"]$future_missing,
        "ssp585/tasmin")
    tasmin <- result$variable_coverage[
        period_role == "future" &
            experiment_id == "ssp585" &
            variable_id == "tasmin"
    ]
    expect_equal(tasmin$file_count, 0L)
    expect_false(tasmin$complete)
})

test_that("daily identity ranking prefers r1i1p1f1 on native grid", {
    variables <- daily__input_variables("enhanced")
    future <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "ssp245", variables = variables,
            years = 2041:2070, member = "r2i1p1f1", grid = "gr"),
        daily_test__catalog(
            experiment = "ssp585", variables = variables,
            years = 2041:2070, member = "r2i1p1f1", grid = "gr"),
        daily_test__catalog(
            experiment = "ssp245", variables = variables,
            years = 2041:2070),
        daily_test__catalog(
            experiment = "ssp585", variables = variables,
            years = 2041:2070)
    ))
    historical <- data.table::rbindlist(list(
        daily_test__catalog(
            experiment = "historical", variables = variables,
            years = 1995:2014, member = "r2i1p1f1", grid = "gr"),
        daily_test__catalog(
            experiment = "historical", variables = variables,
            years = 1995:2014)
    ))

    result <- daily__evaluate_catalogs(
        future,
        historical,
        models = "Model-A",
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_true(all(result$summary$complete))
    expect_identical(
        unique(result$summary$variant_label), "r1i1p1f1")
    expect_identical(unique(result$summary$grid_label), "gn")
})

test_that("Dataset discovery intersects variables locally across all models", {
    variables <- daily__input_variables("enhanced")
    future <- data.table::rbindlist(list(
        daily_test__datasets(
            experiment = "ssp245", variables = variables),
        daily_test__datasets(
            experiment = "ssp585", variables = variables),
        daily_test__datasets(
            model = "Model-B", experiment = "ssp245",
            variables = variables),
        daily_test__datasets(
            model = "Model-B", experiment = "ssp585",
            variables = setdiff(variables, c("hurs", "huss", "ps")))
    ))
    historical <- data.table::rbindlist(list(
        daily_test__datasets(
            experiment = "historical", variables = variables),
        daily_test__datasets(
            model = "Model-B", experiment = "historical",
            variables = variables)
    ))

    result <- daily__evaluate_datasets(
        future,
        historical,
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_setequal(result$models, c("Model-A", "Model-B"))
    expect_true(all(result$summary[source_id == "Model-A"]$complete))
    expect_false(any(result$summary[source_id == "Model-B"]$complete))
    expect_identical(
        unique(result$summary[source_id == "Model-A"]$variant_label),
        "r1i1p1f1"
    )
    expect_true(all(result$future_catalog$datetime_start ==
        "2041-01-01T00:00:00Z"))
    expect_true(all(is.na(
        result$future_catalog$advertised_datetime_start)))
})

test_that("Dataset discovery ignores provider convenience field names", {
    variables <- daily__input_variables("core")
    future <- data.table::rbindlist(list(
        daily_test__datasets(
            experiment = "ssp245", variables = variables),
        daily_test__datasets(
            experiment = "ssp585", variables = variables)
    ))
    historical <- daily_test__datasets(
        experiment = "historical", variables = variables)

    # ORNL currently supplies both normalized CMIP6 facets and these
    # convenience aliases; the aliases must not mask scalar resolver inputs.
    future[, `:=`(
        variable = paste0("provider-", variable_id),
        grid = "provider-grid"
    )]
    historical[, `:=`(
        variable = paste0("provider-", variable_id),
        grid = "provider-grid"
    )]

    result <- daily__evaluate_datasets(
        future,
        historical,
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014,
        profiles = "core"
    )

    expect_true(result$summary$complete)
})

test_that("Dataset discovery handles empty global query results", {
    result <- daily__evaluate_datasets(
        data.table::data.table(),
        data.table::data.table(),
        scenarios = c("ssp245", "ssp585"),
        future_years = 2041:2070,
        historical_years = 1995:2014
    )

    expect_length(result$models, 0L)
    expect_equal(nrow(result$future_catalog), 0L)
    expect_equal(nrow(result$historical_catalog), 0L)
    expect_equal(nrow(result$summary), 0L)
})

test_that("Dataset discovery request leaves source open and keeps variable OR", {
    environment <- daily_test__probe_environment()
    variables <- c("tas", "hurs", "pr")

    request <- environment$daily_probe__discovery_request(
        models = NULL,
        member = "r1i1p1f1",
        experiments = c("ssp245", "ssp585"),
        activity = "ScenarioMIP",
        variables = variables,
        node = "https://example.org"
    )
    query <- shift_as_query(request)

    expect_null(request@meta$source)
    expect_null(request@meta$time)
    expect_identical(request@meta$frequency, "day")
    expect_setequal(
        query_param__value(query$variable_id()), variables)
    expect_setequal(
        query_param__value(query$experiment_id()),
        c("ssp245", "ssp585")
    )
})

test_that("File verification applies years after Dataset discovery", {
    environment <- daily_test__probe_environment()

    request <- environment$daily_probe__request(
        model = "Model-A",
        member = "r1i1p1f1",
        experiments = "historical",
        years = 1995:2014,
        activity = "CMIP",
        variables = c("tas", "hurs"),
        node = "https://example.org"
    )

    expect_null(request@meta$time)
    expect_identical(
        request@meta$options$time_filter_method, "auto")
    expect_identical(
        as.character(request@meta$options$file_time),
        c(
            "1995-01-01T00:00:00Z",
            "2014-12-31T23:59:59Z"
        )
    )
})

test_that("daily probe CLI parsing is deterministic and offline", {
    environment <- daily_test__probe_environment()
    now <- as.POSIXct("2026-07-24 00:00:00", tz = "UTC")

    config <- environment$daily_probe__parse_args(c(
        "--models=Model-A,Model-B",
        "--scenarios=ssp245",
        "--member=r2i1p1f1",
        "--future-years=2051:2060",
        "--historical-years=2001,2002",
        "--index-nodes=DKRZ",
        "--max-nodes=1",
        "--query-timeout=30",
        "--connect-timeout=5",
        "--output=/tmp/daily-probe",
        "--ui=none",
        "--plan"
    ), now = now)

    expect_identical(config$models, c("Model-A", "Model-B"))
    expect_identical(config$member, "r2i1p1f1")
    expect_identical(config$future_years, 2051:2060)
    expect_identical(config$historical_years, 2001:2002)
    expect_identical(config$max_nodes, 1L)
    expect_identical(config$query_timeout, 30)
    expect_identical(config$connect_timeout, 5)
    expect_true(config$plan)
    expect_error(
        environment$daily_probe__parse_args("--unknown=value", now = now),
        "Unknown option")
    expect_error(
        environment$daily_probe__parse_args(
            "--future-years=2060:2050", now = now),
        "range start")
    expect_error(
        environment$daily_probe__parse_args(
            "--query-timeout=0", now = now),
        "at least one second")
    expect_null(environment$daily_probe__parse_args(
        character(), now = now)$models)
    expect_null(environment$daily_probe__parse_args(
        "--models=auto", now = now)$models)
    expect_null(environment$daily_probe__parse_args(
        "--member=auto", now = now)$member)
})

test_that("daily probe marks the selected node without NSE ambiguity", {
    environment <- daily_test__probe_environment()
    environment$daily_probe__evaluate_node <- function(
            model, node, config, store_path, ui) {
        list(
            node = node,
            future = list(dataset_count = 2L, file_count = 4L),
            historical = list(dataset_count = 1L, file_count = 2L),
            evaluation = list(summary = data.table::data.table(
                profile = c("core", "enhanced"),
                complete = TRUE,
                future_present = TRUE,
                historical_present = TRUE,
                missing_total = 0L
            )),
            duration_seconds = 0.1
        )
    }
    config <- environment$daily_probe__parse_args(
        c("--models=Model-A", "--ui=none"))

    result <- environment$daily_probe__run_model(
        "Model-A",
        nodes = "https://example.org",
        config = config,
        store_path = tempfile("daily-probe-store-"),
        ui = NULL
    )

    expect_true(result$attempts$selected)
    expect_identical(result$selected$node, "https://example.org")
})
