# local_query_test_response() / local_query_listing_response() / local_query_test_esgdict() / local_esgdict_default() {{{
local_test_cache(scope = "persist")

# local_query_test_response {{{
local_query_test_response <- function(docs = NULL) {
    if (is.null(docs)) {
        docs <- data.frame(
            id = "dataset-id",
            size = 1,
            index_node = "example.org",
            number_of_files = 1L,
            number_of_aggregations = 0L,
            project = "CMIP6",
            activity_id = "CMIP",
            experiment_id = "historical",
            source_id = "EC-Earth3",
            table_id = "day",
            variable_id = "tas",
            check.names = FALSE
        )
        docs$url <- I(list(
            "https://example.org/file.nc|application/netcdf|HTTPServer"
        ))
        docs$access <- I(list("HTTPServer"))
    }

    esgf_test__response(docs, timestamp = Sys.time())
}
# }}}

# local_query_listing_response {{{
local_query_listing_response <- function(
    params = stats::setNames(list(), character()),
    docs = list(list(id = "dataset-id")),
    facet_fields = stats::setNames(list(), character()),
    num_found = NULL
) {
    if (is.null(num_found)) {
        num_found <- if (is.data.frame(docs)) nrow(docs) else length(docs)
    }

    esgf_test__response(
        docs,
        timestamp = Sys.time(),
        params = params,
        facet_fields = facet_fields,
        num_found = num_found
    )
}
# }}}

# local_query_test_esgdict {{{
local_query_test_esgdict <- function() {
    dict <- EsgDict$new(project = "CMIP6")
    values <- data.table::data.table(
        field = c(
            "project",
            "mip_era",
            "activity_id",
            "activity_id",
            "experiment_id",
            "experiment_id",
            "source_id",
            "table_id",
            "table_id",
            "frequency",
            "frequency",
            "realm",
            "realm",
            "variable_id",
            "variable_id"
        ),
        value = c(
            "CMIP6",
            "CMIP6",
            "CMIP",
            "ScenarioMIP",
            "historical",
            "ssp585",
            "EC-Earth3",
            "day",
            "fx",
            "day",
            "fx",
            "atmos",
            "land",
            "tas",
            "sftlf"
        ),
        description = NA_character_,
        source = "test"
    )
    indices <- list(
        values = values,
        activity_experiment = data.table::data.table(
            activity_id = c("CMIP", "ScenarioMIP"),
            experiment_id = c("historical", "ssp585"),
            sub_experiment_id = c("none", "none")
        ),
        activity_source = data.table::data.table(
            activity_id = c("CMIP", "ScenarioMIP"),
            source_id = c("EC-Earth3", "EC-Earth3"),
            institution_id = c("EC-Earth-Consortium", "EC-Earth-Consortium")
        ),
        variable = data.table::data.table(
            variable_id = c("tas", "sftlf"),
            table_id = c("day", "fx"),
            frequency = c("day", "fx"),
            realm = c("atmos", "land"),
            long_name = c(
                "Near-Surface Air Temperature",
                "Percentage of the grid cell occupied by land"
            ),
            units = c("K", "%")
        )
    )

    priv(dict)$replace(
        list(
            project = "CMIP6",
            profile = "cmip6",
            version = list(vocab = "test", request = "test"),
            sources = list(),
            timestamps = list(),
            built_time = as.POSIXct("2025-01-01", tz = "UTC"),
            data = list(
                vocab = list(activity_id = c("CMIP", "ScenarioMIP")),
                request = data.table::data.table(variable = c("tas", "sftlf"))
            ),
            indices = indices
        ),
        status = "built"
    )
    dict
}
# }}}

# local_esgdict_default {{{
local_esgdict_default <- function(
    dict = NULL,
    project = "CMIP6",
    env = parent.frame()
) {
    project <- dict__project(project)
    default_env <- dict__env()
    old_exists <- exists(project, envir = default_env, inherits = FALSE)
    old <- if (old_exists) {
        get(project, envir = default_env, inherits = FALSE)
    } else {
        NULL
    }

    withr::defer(
        {
            if (old_exists) {
                assign(project, old, envir = default_env)
            } else if (exists(project, envir = default_env, inherits = FALSE)) {
                rm(list = project, envir = default_env)
            }
        },
        envir = env
    )

    if (is.null(dict)) {
        if (exists(project, envir = default_env, inherits = FALSE)) {
            rm(list = project, envir = default_env)
        }
    } else {
        esgdict_set_default(dict)
    }
}
# }}}

# query_test__mock_pages {{{
query_test__mock_pages <- function(
    pages,
    num_found = NULL,
    events = NULL,
    env = parent.frame()
) {
    state <- new.env(parent = emptyenv())
    state$urls <- character()
    state$reads <- 0L
    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            state$reads <- state$reads + 1L
            state$urls <- c(state$urls, url)
            if (!is.null(events)) {
                events$log <- c(events$log, sprintf("read:%d", state$reads))
            }
            docs <- pages[[min(state$reads, length(pages))]]
            total <- if (is.null(num_found)) nrow(docs) else num_found
            list(response = list(numFound = as.integer(total), docs = docs))
        },
        .package = "epwshiftr",
        .env = env
    )
    state
}
# }}}

# query_test__mock_cli_progress {{{
query_test__mock_cli_progress <- function(
    events = NULL,
    env = parent.frame()
) {
    state <- new.env(parent = emptyenv())
    state$bars <- list()
    state$updates <- list()
    state$dones <- list()
    testthat::local_mocked_bindings(
        cli_progress_bar = function(name = NULL, total = NA, ...) {
            state$bars[[length(state$bars) + 1L]] <- list(
                name = name,
                total = total
            )
            "progress-id"
        },
        cli_progress_update = function(
            id = NULL,
            set = NULL,
            total = NULL,
            force = FALSE,
            ...
        ) {
            if (!is.null(events)) {
                events$log <- c(events$log, sprintf("update:%d", set))
            }
            state$updates[[length(state$updates) + 1L]] <- list(
                id = id,
                set = as.integer(set),
                total = if (is.null(total)) NA_integer_ else as.integer(total),
                force = isTRUE(force)
            )
        },
        cli_progress_done = function(id = NULL, result = "done", ...) {
            state$dones[[length(state$dones) + 1L]] <- list(
                id = id,
                result = result
            )
        },
        .package = "cli",
        .env = env
    )
    state
}
# }}}
# }}}
# esg_query() {{{
test_that("esg_query()", {
    # can build query url
    index_node <- "https://example.org"
    without_project <- query__build(index_node, list(project = NULL))
    expect_false(grepl("project=", without_project, fixed = TRUE))
    expect_true(grepl("type=Dataset", without_project, fixed = TRUE))

    flat_url <- query__build(
        index_node,
        list(project = "CMIP5", table_id = "Amon")
    )
    expect_true(grepl("project=CMIP5", flat_url, fixed = TRUE))
    expect_true(grepl("table_id=Amon", flat_url, fixed = TRUE))

    typed_flat_url <- query__build(
        index_node,
        list(
            project = query_param__as("project", "CMIP5"),
            table_id = query_param__as("table_id", "Amon")
        )
    )
    expect_true(grepl("project=CMIP5", typed_flat_url, fixed = TRUE))
    expect_true(grepl("table_id=Amon", typed_flat_url, fixed = TRUE))

    encoded_url <- esg_query("https://example.org")$nominal_resolution(
        "100 km"
    )$datetime_range(
        start = "2050",
        stop = "2080"
    )$url()
    expect_true(grepl(
        "format=application%2Fsolr%2Bjson",
        encoded_url,
        fixed = TRUE
    ))
    expect_true(grepl(
        "nominal_resolution=100+km,100km",
        encoded_url,
        fixed = TRUE
    ))
    expect_true(grepl(
        "query=datetime_start%3A%5B%2A%20TO%202050-01-01T00%3A00%3A00Z%5D",
        encoded_url,
        fixed = TRUE
    ))
    expect_true(grepl(
        "%20AND%20datetime_stop%3A%5B2080-01-01T00%3A00%3A00Z%20TO%20%2A%5D",
        encoded_url,
        fixed = TRUE
    ))

    bridge_date_url <- esg_query("https://esgf-node.ornl.gov")$datetime_range(
        start = "2050",
        stop = "2080"
    )$url()
    expect_true(grepl(
        "query=datetime_start%3A%5B%2A%20TO%20%222050-01-01T00%3A00%3A00Z%22%5D",
        bridge_date_url,
        fixed = TRUE
    ))
    expect_true(grepl(
        "%20AND%20%28datetime_stop%3A%5B%222080-01-01T00%3A00%3A00Z%22%20TO%20%2A%5D%20OR%20datetime_end%3A%5B%222080-01-01T00%3A00%3A00Z%22%20TO%20%2A%5D%29",
        bridge_date_url,
        fixed = TRUE
    ))

    bridge_url <- query__build(
        "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        list(
            activity_id = query_param__as(
                "activity_id",
                list(value = "ScenarioMIP", negate = TRUE)
            )
        )
    )
    expect_true(grepl(
        "query=NOT%20%28activity_id%3A%22ScenarioMIP%22%29",
        bridge_url,
        fixed = TRUE
    ))

    expect_error(
        query__build(
            "https://esgf-node.ornl.gov/esgf-1-5-bridge",
            list(type = "Aggregation")
        ),
        "Bridge index nodes do not support.*Aggregation"
    )
    expect_true(grepl(
        "type=Aggregation",
        query__build("https://esgf-data.dkrz.de", list(type = "Aggregation")),
        fixed = TRUE
    ))

    expect_s3_class(EsgQuery$new(), "EsgQuery")
    expect_s3_class(esg_query(), "EsgQuery")

    q_state <- esg_query("https://esgf.ceda.ac.uk")$experiment_id(
        "ssp585"
    )$fields("source_id")
    expect_identical(q_state$index_node(), "https://esgf.ceda.ac.uk")

    state <- q_state$state()
    expect_named(state, c("index_node", "parameter"))
    expect_identical(state$index_node, "https://esgf.ceda.ac.uk")
    expect_true(all(c("experiment_id", "fields") %in% names(state$parameter)))
    expect_identical(
        query_param__value(state$parameter$experiment_id),
        "ssp585"
    )

    state_all <- q_state$state(null = TRUE)
    expect_named(state_all$parameter, names(QUERY_PARAM__DEF))
    expect_null(state_all$parameter$datetime_start)

    expect_s3_class(q_state$index_node("esgf.nci.org.au"), "EsgQuery")
    expect_identical(q_state$index_node(), "https://esgf.nci.org.au")
    expect_identical(query_param__value(q_state$experiment_id()), "ssp585")
    expect_identical(query_param__value(q_state$fields()), "source_id")

    expect_s3_class(q_state$reset(), "EsgQuery")
    expect_identical(q_state$index_node(), "https://esgf.nci.org.au")
    expect_null(q_state$experiment_id())
    expect_identical(query_param__value(q_state$project()), "CMIP6")
    expect_identical(query_param__value(q_state$fields()), "*")

    q <- expect_s3_class(EsgQuery$new("https://example.org"), "EsgQuery")
    q <- expect_s3_class(esg_query("https://example.org"), "EsgQuery")
})
# }}}
# EsgQuery$list_facets() {{{
test_that("EsgQuery$list_facets()", {
    index_node_normal <- "https://example.org"
    index_node_bridge <- INDEX_NODES[["ORNL"]]
    calls <- character()

    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            calls <<- c(calls, url)
            local_query_listing_response(
                params = list(facet.field = c("activity_id", "source_id"))
            )
        },
        .package = "epwshiftr"
    )

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_identical(q$list_facets(force = TRUE), c("activity_id", "source_id"))
    expect_length(calls, 1L)

    # bridge node directly returns predefined facet names
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_type(q$list_facets(), "character")
    expect_equal(q$list_facets(), FIELDS_FACETS_COMMON)
    expect_length(calls, 1L)
})
# }}}
# EsgQuery$list_fields() {{{
test_that("EsgQuery$list_fields()", {
    index_node_normal <- "https://example.org"
    index_node_bridge <- INDEX_NODES[["ORNL"]]
    fields <- c("id", "activity_id", "source_id")

    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            local_query_listing_response(
                docs = list(stats::setNames(as.list(seq_along(fields)), fields))
            )
        },
        .package = "epwshiftr"
    )

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_identical(q$list_fields(force = TRUE), fields)
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_identical(q$list_fields(force = TRUE), fields)
})
# }}}
# EsgQuery$list_shards() {{{
test_that("EsgQuery$list_shards()", {
    index_node_normal <- "https://example.org"
    index_node_bridge <- INDEX_NODES[["ORNL"]]
    shards <- "localhost:8983/solr/datasets,esgf.example.org/solr/datasets"

    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            local_query_listing_response(params = list(shards = shards))
        },
        .package = "epwshiftr"
    )

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_identical(
        q$list_shards(force = TRUE),
        c("example.org:8983/solr/datasets", "esgf.example.org/solr/datasets")
    )
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_identical(
        q$list_shards(force = TRUE),
        c(
            "esgf-node.ornl.gov:8983/solr/datasets",
            "esgf.example.org/solr/datasets"
        )
    )
})
# }}}
# EsgQuery$list_values() {{{
test_that("EsgQuery$list_values()", {
    index_node_normal <- "https://example.org"
    index_node_bridge <- INDEX_NODES[["ORNL"]]
    facet_fields <- list(
        activity_id = list("CMIP", 2L, "ScenarioMIP", 1L),
        experiment_id = list("historical", 3L, "ssp585", 4L)
    )

    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            local_query_listing_response(
                facet_fields = facet_fields,
                num_found = 4L
            )
        },
        .package = "epwshiftr"
    )

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_identical(
        q$list_values("activity_id", force = TRUE),
        c(CMIP = 2L, ScenarioMIP = 1L)
    )
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    values <- expect_type(
        q$list_values(c("activity_id", "experiment_id"), force = TRUE),
        "list"
    )
    expect_named(values, c("activity_id", "experiment_id"))
    expect_identical(values$experiment_id, c(historical = 3L, ssp585 = 4L))
})
# }}}
# query_listing_cached() {{{
test_that("query_listing_cached() respects max_age in normal and offline modes", {
    dir <- tempfile("epwshiftr-expired-cache-")
    cache <- DiskCache$new(dir, max_age = 0.1)
    old_cache <- cache__set(cache)
    withr::defer({
        cache__set(old_cache)
        cache$destroy()
    })

    url <- "https://example.org/esg-search/search?project=CMIP6"
    key <- cache__response_key(url)
    expire_entry <- function() {
        cache$set(key, list(timestamp = Sys.time() - 3600, value = "old"))
        Sys.setFileTime(
            file.path(cache$info()$dir, paste0(key, ".rds")),
            Sys.time() - 3600
        )
    }

    local_cache_mode("normal")
    expire_entry()
    calls <- 0L
    fetched <- list(timestamp = Sys.time(), value = "new")
    testthat::local_mocked_bindings(
        cache__read_json = function(...) {
            calls <<- calls + 1L
            fetched
        },
        .package = "epwshiftr"
    )
    q <- esg_query("https://example.org")
    expect_identical(
        priv(q)$query_listing_cached(url, force = FALSE, type = "facet"),
        fetched
    )
    expect_equal(calls, 1L)

    local_cache_mode("offline")
    expire_entry()
    expect_error(
        priv(q)$query_listing_cached(url, force = FALSE, type = "facet"),
        "Cache miss in offline mode"
    )
})
# }}}
# EsgQuery facet and control setters {{{
test_that("EsgQuery facet setters forward values, negation, clearing, and resolution encoding", {
    q <- esg_query()
    expect_equal(query_param__value(q$project()), "CMIP6")
    expect_equal(query_param__value(q$project("CMIP5")$project()), "CMIP5")
    expect_equal(query_param__value(q$project(-"CMIP5")$project()), "CMIP5")
    expect_equal(query_param__value(q$project(!"CMIP5")$project()), "CMIP5")
    expect_true(q$project()@negate)

    facets <- list(
        activity_id = c("CFMIP", "ScenarioMIP"),
        experiment_id = c("ssp126", "ssp585"),
        source_id = c("BCC-CSM2-MR", "CESM2"),
        variable_id = c("tas", "pr"),
        frequency = c("1hr", "day"),
        variant_label = c("r1i1p1f1", "r1i2p1f1"),
        data_node = "esgf-node.ornl.gov"
    )
    for (nm in names(facets)) {
        q <- esg_query()
        vals <- facets[[nm]]
        expect_null(q[[nm]]())
        expect_equal(query_param__value(q[[nm]](!vals)[[nm]]()), vals)
        expect_true(q[[nm]]()@negate)
        expect_null(q[[nm]](NULL)[[nm]]())
    }

    q <- esg_query()
    expect_null(q$nominal_resolution())
    expect_equal(
        query_param__value(q$nominal_resolution(c(
            "100 km",
            "1x1 degree"
        ))$nominal_resolution()),
        c("100+km", "1x1+degree", "100km")
    )
    expect_true(q$nominal_resolution()@encoded)
    expect_null(q$nominal_resolution(NULL)$nominal_resolution())
})

test_that("EsgQuery control setters forward facets, fields, shards, replica, latest, limit, and offset", {
    expect_null(esg_query()$facets())
    expect_equal(
        query_param__value(esg_query()$facets(c(
            "activity_id",
            "source_id"
        ))$facets()),
        c("activity_id", "source_id")
    )
    expect_null(esg_query()$facets(NULL)$facets())

    expect_equal(query_param__value(esg_query()$fields()), "*")
    expect_equal(
        query_param__value(esg_query()$fields(c(
            "activity_id",
            "source_id"
        ))$fields()),
        c("activity_id", "source_id")
    )
    expect_null(esg_query()$fields(NULL)$fields())

    q <- esg_query()
    expect_null(q$shards())
    expect_false(query_param__value(q$distrib(FALSE)$distrib()))
    expect_error(q$shards("a"), "distrib")
    expect_true(query_param__value(q$distrib(TRUE)$distrib()))
    expect_equal(
        query_param__value(q$shards("esgf-solr.ceda.ac.uk:8983/solr")$shards()),
        "esgf-solr.ceda.ac.uk:8983/solr"
    )
    expect_null(q$shards(NULL)$shards())

    expect_null(esg_query()$replica())
    expect_true(query_param__value(esg_query()$replica(TRUE)$replica()))
    expect_false(query_param__value(esg_query()$replica(FALSE)$replica()))
    expect_null(esg_query()$replica(NULL)$replica())

    expect_null(esg_query()$latest())
    expect_false(query_param__value(esg_query()$latest(FALSE)$latest()))
    expect_true(query_param__value(esg_query()$latest(TRUE)$latest()))
    expect_null(esg_query()$latest(TRUE)$latest(NULL)$latest())

    expect_equal(query_param__value(esg_query()$limit()), 10L)
    expect_warning(lim <- esg_query()$limit(12000)$limit(), "10,000")
    expect_equal(query_param__value(lim), 10000L)

    expect_equal(query_param__value(esg_query()$offset()), 0L)
    expect_equal(query_param__value(esg_query()$offset(5L)$offset()), 5L)
})
# }}}
# decode_query() {{{
decode_query <- function(url) {
    m <- regmatches(url, regexpr("(?<=query=)[^&]*", url, perl = TRUE))
    if (!length(m)) {
        return(character(0L))
    }
    URLdecode(m)
}
# }}}
# EsgQuery range rendering {{{
test_that("EsgQuery range setters render standard and bridge Solr URLs", {
    test_local_dependencies(
        query_time = as.POSIXct("2025-06-13 12:34:56", tz = "UTC")
    )

    q <- esg_query()
    expect_identical(q$datetime_range(), list(start = NULL, stop = NULL))
    expect_identical(q$timestamp_range(), list(from = NULL, to = NULL))
    expect_identical(q$version_range(), list(min = NULL, max = NULL))

    # Bridge datetime_range: quoted ISO dates, datetime_stop OR datetime_end, local Date Math
    expect_match(
        decode_query(esg_query()$datetime_range(start = "2017")$url()),
        'datetime_start:[* TO "2017-01-01T00:00:00Z"]',
        fixed = TRUE
    )
    expect_match(
        decode_query(esg_query()$datetime_range(
            stop = "2025-01-01T00:00:00Z"
        )$url()),
        '(datetime_stop:["2025-01-01T00:00:00Z" TO *] OR datetime_end:["2025-01-01T00:00:00Z" TO *])',
        fixed = TRUE
    )
    expect_match(
        decode_query(esg_query()$datetime_range(start = "NOW-1YEAR")$url()),
        'datetime_start:[* TO "2024-06-13T12:34:56Z"]',
        fixed = TRUE
    )
    expect_match(
        decode_query(esg_query()$datetime_range(stop = "NOW+6MONTHS")$url()),
        '(datetime_stop:["2025-12-13T12:34:56Z" TO *] OR datetime_end:["2025-12-13T12:34:56Z" TO *])',
        fixed = TRUE
    )
    expect_match(
        decode_query(esg_query()$datetime_range(
            start = "2025-06-13T00:00:00Z-1YEAR"
        )$url()),
        'datetime_start:[* TO "2024-06-13T00:00:00Z"]',
        fixed = TRUE
    )
    expect_match(
        decode_query(esg_query()$datetime_range(
            start = "[2017-01-01T00:00:00Z TO 2020-01-01T00:00:00Z]"
        )$url()),
        'datetime_start:["2017-01-01T00:00:00Z" TO "2020-01-01T00:00:00Z"]',
        fixed = TRUE
    )
    both_dt <- decode_query(esg_query()$datetime_range(
        start = "2017-01-01T00:00:00Z",
        stop = "2025-01-01T00:00:00Z"
    )$url())
    expect_match(
        both_dt,
        'datetime_start:[* TO "2017-01-01T00:00:00Z"]',
        fixed = TRUE
    )
    expect_match(both_dt, " AND ", fixed = TRUE)
    expect_null(
        esg_query()$datetime_range(start = "2017")$datetime_range(
            start = NULL
        )$datetime_range()$start
    )

    # Standard Solr node preserves unquoted Date Math
    normal_query <- decode_query(
        esg_query("https://example.org")$datetime_range(
            start = "NOW-1YEAR",
            stop = "2025-06-13T00:00:00Z+1YEAR"
        )$url()
    )
    expect_match(normal_query, "datetime_start:[* TO NOW-1YEAR]", fixed = TRUE)
    expect_match(
        normal_query,
        "datetime_stop:[2025-06-13T00:00:00Z+1YEAR TO *]",
        fixed = TRUE
    )

    # timestamp_range incremental updates, Date Math, and "*" clearing
    q_ts <- esg_query()$timestamp_range(from = "NOW-1YEAR")
    expect_match(
        decode_query(q_ts$url()),
        '_timestamp:["2024-06-13T12:34:56Z" TO *]',
        fixed = TRUE
    )
    q_ts$timestamp_range(to = "2021")
    expect_match(
        decode_query(q_ts$url()),
        '_timestamp:["2024-06-13T12:34:56Z" TO "2021-01-01T00:00:00Z"]',
        fixed = TRUE
    )
    expect_identical(
        decode_query(esg_query()$timestamp_range(from = "*", to = "*")$url()),
        character(0L)
    )
    expect_null(
        esg_query()$timestamp_range(from = "2020")$timestamp_range(
            from = NULL
        )$timestamp_range()$from
    )

    # version_range normalization to YYYYMMDD
    ver_q <- decode_query(esg_query()$version_range(
        min = "2020",
        max = "2020-06"
    )$url())
    expect_match(ver_q, "version:[20200101 TO *]", fixed = TRUE)
    expect_match(ver_q, "version:[* TO 20200601]", fixed = TRUE)
    expect_null(
        esg_query()$version_range(min = "20200101")$version_range(
            min = NULL
        )$version_range()$min
    )
})
# }}}
# EsgQuery$params() {{{
test_that("EsgQuery$params()", {
    q <- expect_s3_class(esg_query(), "EsgQuery")

    # can use existing method for common parameters
    expect_false("type" %in% names(q))
    expect_false("format" %in% names(q))
    expect_equal(q$params(), list())
    expect_equal(
        q$params(nominal_resolution = !c("10 km", "25 km"))$params(),
        list()
    )
    expect_error(q$params(table_id = "day", table_id = "hour"), "unique names")

    # can reset existing parameters
    expect_equal(query_param__value(q$frequency("day")$frequency()), "day")
    expect_equal(q$params(frequency = NULL)$params(), list())
    expect_null(q$frequency())
    params <- q$params(table_id = "Amon", member_id = "r1i1p1f1")$params()
    expect_named(params, c("table_id", "member_id"))
    expect_s3_class(params$table_id, "S7_object")
    expect_s3_class(params$member_id, "S7_object")
    expect_identical(query_param__value(params$table_id), "Amon")
    expect_identical(query_param__value(params$member_id), "r1i1p1f1")

    # cannot set query orchestration controls
    expect_error(q$params(format = "xml"), "cannot be set")
    expect_error(q$params(type = "File"), "cannot be set")
    expect_named(q$params(), c("table_id", "member_id"))

    # can restore original values in case of error
    expect_equal(query_param__value(q$frequency("day")$frequency()), "day")
    expect_error(q$params(frequency = "1hr", frequency = "a"), "Assertion")
    expect_equal(query_param__value(q$frequency()), "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())
})
# }}}
# EsgQuery$url() {{{
test_that("EsgQuery$url()", {
    index_node <- "https://example.org"

    expect_type(
        EsgQuery$new(index_node)$nominal_resolution("100 km")$url(),
        "character"
    )
    expect_type(
        EsgQuery$new(index_node)$nominal_resolution("100 km")$url(TRUE),
        "character"
    )
    expect_type(
        EsgQuery$new(index_node)$params(
            project = "CMIP5",
            table_id = "Amon"
        )$url(),
        "character"
    )
})
# }}}
# EsgQuery$count() {{{
test_that("EsgQuery$count()", {
    index_node <- "https://example.org"

    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            decoded <- utils::URLdecode(url)
            facet_fields <- if (
                grepl("facets=activity_id", decoded, fixed = TRUE)
            ) {
                list(activity_id = list("CMIP", 2L, "ScenarioMIP", 1L))
            } else {
                stats::setNames(list(), character())
            }
            local_query_listing_response(
                facet_fields = facet_fields,
                num_found = 3L
            )
        },
        .package = "epwshiftr"
    )

    expect_identical(EsgQuery$new(index_node)$frequency("1hr")$count(FALSE), 3L)
    expect_identical(EsgQuery$new(index_node)$frequency("1hr")$count(TRUE), 3L)
    cnt <- expect_type(
        EsgQuery$new(index_node)$frequency("1hr")$count("activity_id"),
        "list"
    )
    expect_equal(names(cnt), c("total", "activity_id"))
    expect_identical(cnt$activity_id, c(CMIP = 2L, ScenarioMIP = 1L))
})
# }}}
# EsgQuery$collect() {{{
test_that("EsgQuery$collect(type=) collects child results through Dataset workflow", {
    q <- esg_query("https://example.org")$project("CMIP6")$datetime_range(
        start = "2050-01-01T00:00:00Z",
        stop = "2080-12-31T23:59:59Z"
    )$limit(3L)

    calls <- list()
    local_response <- function(docs) {
        esgf_test__response(docs, timestamp = Sys.time())
    }
    local_dataset_docs <- data.frame(
        id = c("dataset-1", "dataset-2"),
        source_id = c("source-a", "source-b"),
        experiment_id = c("ssp126", "ssp585"),
        size = c(1, 2),
        access = I(list(c("OPENDAP", "HTTPServer"), "HTTPServer")),
        check.names = FALSE
    )
    local_file_docs <- data.frame(
        id = "file-1",
        dataset_id = "dataset-1",
        size = 1,
        checksum = "abc",
        checksum_type = "SHA256",
        tracking_id = "hdl:21.14100/mock-file",
        title = "file.nc",
        data_node = "example.org",
        check.names = FALSE
    )
    local_file_docs$url <- I(list(c(
        "https://example.org/dods/file.nc.html|application/netcdf|OPENDAP",
        "https://example.org/file.nc|application/netcdf|HTTPServer"
    )))

    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints,
                dict_check = dict_check
            )

            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                local_dataset_docs
            } else {
                local_file_docs
            }
            response <- local_response(docs)
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    files <- expect_s3_class(
        q$collect(type = "file", fields = "id", data_node = "example.org"),
        "EsgResultFile"
    )
    expect_length(calls, 2L)
    expect_true(calls[[1L]]$all)
    expect_false(calls[[1L]]$limit)
    expect_false(calls[[1L]]$dict_check)
    expect_identical(query_param__value(calls[[1L]]$params$type()), "Dataset")
    expect_false(calls[[2L]]$all)
    expect_equal(calls[[2L]]$limit, 3L)
    expect_true(calls[[2L]]$dict_check)
    expect_identical(query_param__value(calls[[2L]]$params$type()), "File")
    expect_null(calls[[2L]]$params$source_id())
    expect_identical(
        query_param__value(calls[[2L]]$params$data_node()),
        "example.org"
    )
    expect_identical(
        calls[[2L]]$params$render(c("datetime_start", "datetime_stop")),
        character()
    )
    expect_identical(files$count(), 1L)

    expect_error(
        q$collect(type = "Dataset", source_id = "AWI-CM-1-1-MR"),
        "Additional query filters"
    )
    expect_error(q$collect(type = "Dataset", fields = "id"), "`fields`")
})

test_that("EsgQuery$collect() passes progress to Dataset and child collection", {
    calls <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress = FALSE,
            progress_label = NULL,
            progress_callback = NULL
        ) {
            calls[[length(calls) + 1L]] <<- list(
                type = query_param__value(params$type()),
                all = all,
                limit = limit,
                progress = progress,
                progress_label = progress_label
            )
            esgf_fixture_collect(params)
        },
        .package = "epwshiftr"
    )

    q <- esg_query("https://example.org")$experiment_id("ssp585")$variable_id(
        "tas"
    )$limit(2L)
    expect_s3_class(q$collect(progress = TRUE), "EsgResultDataset")
    expect_length(calls, 1L)
    expect_true(calls[[1L]]$progress)
    expect_identical(calls[[1L]]$progress_label, "Collecting Dataset records")

    calls <- list()
    expect_s3_class(
        q$collect(type = "File", fields = "id", progress = TRUE),
        "EsgResultFile"
    )
    expect_length(calls, 2L)
    expect_identical(
        vapply(calls, `[[`, character(1L), "type"),
        c("Dataset", "File")
    )
    expect_true(all(vapply(calls, `[[`, logical(1L), "progress")))
    expect_identical(
        vapply(calls, `[[`, character(1L), "progress_label"),
        c("Collecting Dataset records", "Collecting File records")
    )

    calls <- list()
    expect_s3_class(q$collect(progress = FALSE), "EsgResultDataset")
    expect_false(calls[[1L]]$progress)
    expect_null(calls[[1L]]$progress_label)
})
# }}}
# query__collect() {{{
test_that("query__collect() normalizes fields and effective parameters", {
    mock <- query_test__mock_pages(list(data.frame(
        id = "dataset-id",
        source_id = "source",
        project = "CMIP6",
        activity_id = "CMIP",
        table_id = "Amon",
        score = 1,
        check.names = FALSE
    )))

    expect_warning(
        params <- QueryParamStore$new()$activity_id("CMIP")$fields(
            "source_id"
        )$facets("source_id")$shards("node")$params(
            table_id = "Amon",
            bbox = "0,0,1,1",
            start = "2020",
            end = "2021",
            from = "2020",
            to = "2021"
        ),
        NA
    )

    res <- query__collect(
        "https://example.org",
        params,
        required_fields = "id",
        constraints = TRUE
    )
    decoded_url <- utils::URLdecode(mock$urls[[1L]])
    fields <- strsplit(
        regmatches(
            decoded_url,
            regexpr("(?<=fields=)[^&]*", decoded_url, perl = TRUE)
        ),
        ",",
        fixed = TRUE
    )[[1L]]
    expect_identical(
        fields,
        c("source_id", "id", "project", "activity_id", "table_id")
    )
    expect_false(any(c("bbox", "start", "facets") %in% fields))
    expect_named(
        res$docs,
        c("id", "source_id", "project", "activity_id", "table_id"),
        ignore.order = TRUE
    )
    expect_identical(
        query_param__value(res$parameter$fields()),
        c("source_id", "id", "project", "activity_id", "table_id")
    )

    params2 <- QueryParamStore$new()$fields("id")$limit(5L)$offset(2L)
    res2 <- query__collect(
        "https://example.org",
        params2,
        required_fields = c("size", "url"),
        all = TRUE,
        limit = 3L
    )
    expect_identical(
        query_param__value(res2$parameter$fields()),
        c("id", "size", "url", "project")
    )
    expect_identical(query_param__value(res2$parameter$limit()), 3L)
    expect_identical(query_param__value(res2$parameter$offset()), 0L)

    bridge <- query__collect(
        "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        params2,
        required_fields = c("size", "url")
    )
    expect_null(bridge$parameter$fields())
})

test_that("query__collect() records paginated URLs and warns on stalled pagination", {
    page1 <- data.frame(
        id = c("dataset-1", "dataset-2"),
        score = 1,
        check.names = FALSE
    )
    mock <- query_test__mock_pages(
        list(
            page1,
            data.frame(id = "dataset-3", score = 1, check.names = FALSE)
        ),
        num_found = 3L
    )
    res <- query__collect(
        "https://example.org",
        QueryParamStore$new()$limit(2L),
        all = TRUE,
        limit = 2L
    )
    expect_equal(nrow(res$docs), 3L)
    expect_identical(res$context$query_url, mock$urls)
    expect_true(grepl(
        "offset=0",
        utils::URLdecode(mock$urls[[1L]]),
        fixed = TRUE
    ))
    expect_true(grepl(
        "offset=2",
        utils::URLdecode(mock$urls[[2L]]),
        fixed = TRUE
    ))

    stalled <- query_test__mock_pages(
        list(
            page1,
            data.frame(id = character(), score = numeric(), check.names = FALSE)
        ),
        num_found = 3L
    )
    expect_warning(
        res_partial <- query__collect(
            "https://example.org",
            QueryParamStore$new()$limit(2L),
            all = TRUE,
            limit = 2L
        ),
        "ESGF pagination stopped"
    )
    expect_identical(res_partial$docs$id, c("dataset-1", "dataset-2"))
    expect_length(stalled$urls, 2L)
    expect_identical(res_partial$context$query_url, stalled$urls)
})

test_that("query__collect() reports progress for multi-page and single-page requests", {
    events <- new.env(parent = emptyenv())
    events$log <- character()
    query_test__mock_pages(
        list(
            data.frame(
                id = c("dataset-1", "dataset-2"),
                score = 1,
                check.names = FALSE
            ),
            data.frame(id = "dataset-3", score = 1, check.names = FALSE)
        ),
        num_found = 3L,
        events = events
    )
    prog <- query_test__mock_cli_progress(events = events)
    res <- query__collect(
        "https://example.org",
        QueryParamStore$new()$limit(2L),
        all = TRUE,
        limit = 2L,
        progress = TRUE,
        progress_label = "Collecting test records"
    )
    expect_equal(nrow(res$docs), 3L)
    expect_identical(
        prog$bars,
        list(list(name = "Collecting test records", total = 1L))
    )
    expect_identical(
        events$log,
        c("update:0", "read:1", "update:2", "read:2", "update:3")
    )
    expect_equal(vapply(prog$updates, `[[`, integer(1L), "set"), c(0L, 2L, 3L))
    expect_equal(
        vapply(prog$updates, `[[`, integer(1L), "total"),
        c(1L, 3L, 3L)
    )
    expect_equal(
        vapply(prog$updates, `[[`, logical(1L), "force"),
        c(TRUE, FALSE, FALSE)
    )
    expect_equal(prog$dones, list(list(id = "progress-id", result = "done")))

    query_test__mock_pages(
        list(data.frame(id = "dataset-1", score = 1, check.names = FALSE)),
        num_found = 3L
    )
    prog2 <- query_test__mock_cli_progress()
    query__collect(
        "https://example.org",
        QueryParamStore$new()$limit(1L),
        progress = TRUE
    )
    expect_equal(
        prog2$updates,
        list(
            list(id = "progress-id", set = 0L, total = 1L, force = TRUE),
            list(id = "progress-id", set = 1L, total = 3L, force = FALSE)
        )
    )
})
# }}}
# EsgQuery$collect() {{{
test_that("EsgQuery$collect() checks local dictionary only when present", {
    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) local_query_test_response(),
        .package = "epwshiftr"
    )
    q <- esg_query("https://example.org")$activity_id(
        "ScenarioMIP"
    )$experiment_id("historical")$limit(1L)

    local_esgdict_default(local_query_test_esgdict())
    expect_warning(
        res <- q$collect(),
        "ESG dictionary check found invalid query constraint"
    )
    expect_s3_class(res, "EsgResultDataset")

    local_esgdict_default(NULL)
    withr::local_options(epwshiftr.dir_store = withr::local_tempdir())
    expect_warning(res2 <- q$collect(), NA)
    expect_s3_class(res2, "EsgResultDataset")
})

test_that("EsgQuery$collect() collects fixture-backed Dataset results", {
    index_node <- "https://example.org"

    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            esgf_fixture_collect(params)
        },
        .package = "epwshiftr"
    )

    q <- expect_s3_class(
        esg_query(index_node)$experiment_id("ssp585")$frequency("1hr")$fields(
            "source_id"
        ),
        "EsgQuery"
    )

    # can collect the specified limit number of records
    res <- expect_s3_class(q$limit(1)$collect(), "EsgResultDataset")

    # can collect fields with constraints
    expect_true(all(c("project", "frequency", "source_id") %in% names(res)))

    # can collect required fields
    # NOTE: it is possible that some index nodes do not have
    # 'number_of_aggregations' field
    expect_true(
        all(
            setdiff(
                EsgResultDataset$private_fields$required_fields,
                "number_of_aggregations"
            ) %in%
                names(res)
        )
    )

    # can collect all results with auto-pagination
    ## with maximum batch size
    expect_s3_class(q$collect(all = TRUE, limit = FALSE), "EsgResultDataset")
    ## with specified limits
    expect_s3_class(q$collect(all = TRUE, limit = 30), "EsgResultDataset")
})
# }}}
# EsgQuery$save() / EsgQuery$load() {{{
test_that("EsgQuery$save() / EsgQuery$load()", {
    index_node <- INDEX_NODES[["CEDA"]]
    testthat::local_mocked_bindings(
        cache__read_json = function(url, ...) {
            esgf_fixture_response("dataset-success.json")
        },
        .package = "epwshiftr"
    )

    q <- EsgQuery$new(index_node)$activity_id("ScenarioMIP")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(2)$params(
        table_id = c("Amon", "day")
    )

    # empty query object
    file_empty <- tempfile(fileext = ".json")
    expect_snapshot_file(q$save(file_empty), "query_empty.json")
    q_empty <- expect_s3_class(esg_query()$load(file_empty), "EsgQuery")
    expect_equal(priv(q_empty)$index_node, priv(q)$index_node)
    expect_equal(
        priv(q_empty)$parameter$serialize(null = TRUE),
        priv(q)$parameter$serialize(null = TRUE)
    )

    # structured query= parameters round-trip through save/load
    q_query <- EsgQuery$new(index_node)$datetime_range(
        start = "2017"
    )$timestamp_range(
        from = "NOW-1YEAR",
        to = "2021"
    )$version_range(min = "2020", max = "2021")$params(
        table_id = c("Amon", "day")
    )
    file_query <- withr::local_tempfile(fileext = ".json")
    expect_type(q_query$save(file_query), "character")
    q_query_loaded <- expect_s3_class(esg_query()$load(file_query), "EsgQuery")
    expect_equal(
        priv(q_query_loaded)$parameter$serialize(null = TRUE),
        priv(q_query)$parameter$serialize(null = TRUE)
    )

    query_json <- jsonlite::fromJSON(
        file_query,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    expect_true(all(
        c("project", "datetime_start", "limit", "type", "table_id") %in%
            names(query_json$parameter)
    ))

    write_mutated <- function(payload) {
        path <- withr::local_tempfile(
            fileext = ".json",
            .local_envir = parent.frame()
        )
        jsonlite::write_json(payload, path, null = "null", auto_unbox = TRUE)
        path
    }
    bad_type <- query_json
    bad_type$parameter$type$value <- "File"
    expect_error(esg_query()$load(write_mutated(bad_type)), "Dataset queries")

    bad_format <- query_json
    bad_format$parameter$format$value <- "application/xml"
    expect_error(
        esg_query()$load(write_mutated(bad_format)),
        "JSON response format"
    )

    bucketed <- query_json
    bucketed$parameter <- list(
        facet = list(project = query_json$parameter$project),
        query = list(datetime_start = query_json$parameter$datetime_start),
        control = list(type = query_json$parameter$type),
        others = list(table_id = query_json$parameter$table_id)
    )
    expect_error(esg_query()$load(write_mutated(bucketed)), "Bucketed")

    # query object with results
    q$collect()
    file_collected <- withr::local_tempfile(fileext = ".json")
    expect_type(q$save(file_collected), "character")
    expect_true(file.exists(file_collected))
    file_collected_copied <- withr::local_tempfile(fileext = ".json")
    expect_true(file.copy(file_collected, file_collected_copied))
    expect_snapshot_file(
        file_collected_copied,
        "query_collected.json",
        transform = transform_json
    )
    q_collected <- expect_s3_class(esg_query()$load(file_collected), "EsgQuery")
    expect_equal(priv(q_collected)$index_node, priv(q)$index_node)
    expect_equal(
        priv(q_collected)$parameter$serialize(null = TRUE),
        priv(q)$parameter$serialize(null = TRUE)
    )
})
# }}}
# EsgQuery$print() {{{
test_that("EsgQuery$print()", {
    expect_snapshot(
        EsgQuery$new("a")$params(
            table_id = "Amon",
            member_id = "r1i1p1f1"
        )$print()
    )
})
# }}}

# vim: fdm=marker :
