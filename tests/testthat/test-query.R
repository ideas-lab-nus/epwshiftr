# QueryParam helpers {{{
local_test_cache(scope = "persist")

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
        docs$url <- I(list("https://example.org/file.nc|application/netcdf|HTTPServer"))
        docs$access <- I(list("HTTPServer"))
    }

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
        timestamp = Sys.time()
    )
}

local_query_test_esgdict <- function() {
    dict <- EsgDict$new(project = "CMIP6")
    values <- data.table::data.table(
        field = c(
            "project", "mip_era",
            "activity_id", "activity_id",
            "experiment_id", "experiment_id",
            "source_id",
            "table_id", "table_id",
            "frequency", "frequency",
            "realm", "realm",
            "variable_id", "variable_id"
        ),
        value = c(
            "CMIP6", "CMIP6",
            "CMIP", "ScenarioMIP",
            "historical", "ssp585",
            "EC-Earth3",
            "day", "fx",
            "day", "fx",
            "atmos", "land",
            "tas", "sftlf"
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
            long_name = c("Near-Surface Air Temperature", "Percentage of the grid cell occupied by land"),
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

local_esgdict_default <- function(dict = NULL, project = "CMIP6", env = parent.frame()) {
    project <- esgdict__normalize_project(project)
    default_env <- esgdict__default_env()
    old_exists <- exists(project, envir = default_env, inherits = FALSE)
    old <- if (old_exists) get(project, envir = default_env, inherits = FALSE) else NULL

    withr::defer({
        if (old_exists) {
            assign(project, old, envir = default_env)
        } else if (exists(project, envir = default_env, inherits = FALSE)) {
            rm(list = project, envir = default_env)
        }
    }, envir = env)

    if (is.null(dict)) {
        if (exists(project, envir = default_env, inherits = FALSE)) {
            rm(list = project, envir = default_env)
        }
    } else {
        esgdict_set_default(dict)
    }
}

test_that("QueryParam helpers use typed objects", {
    param <- expect_s3_class(
        as_query_param("x", list(value = LETTERS[1:3], negate = TRUE)),
        "S7_object"
    )
    expect_identical(query_param_kind(param), "facet")
    expect_identical(query_param_name(param), "x")
    expect_identical(query_param_value(param), LETTERS[1:3])
    expect_true(query_param_negate(param))

    query_param <- expect_s3_class(as_query_param("datetime_start", "2017"), "S7_object")
    expect_s3_class(as_query_param("datetime_start", "2017"), "S7_object")
    expect_identical(query_param_kind(query_param), "datetime_start")
    expect_identical(query_param_name(query_param), "datetime_start")
    expect_true(is.solrdt(query_param_value(query_param)))

    expect_identical(query_param_spec("project")$class, "facet")
    expect_identical(query_param_spec("_timestamp")$class, "query")
    expect_identical(
        query_param_names("query"),
        c(
            "datetime_start",
            "datetime_stop",
            "timestamp_from",
            "timestamp_to",
            "_timestamp",
            "version_min",
            "version_max"
        )
    )

    flat_params <- query_param_flat(list(
        datetime_start = solrdt("2017"),
        project = "CMIP6",
        others = list(variable_id = "tas")
    ))
    expect_s3_class(flat_params$datetime_start, "S7_object")
    expect_s3_class(flat_params$project, "S7_object")
    expect_s3_class(flat_params$variable_id, "S7_object")

    expect_identical(query_param_render(as_query_param("x", list(value = TRUE, negate = TRUE))), "x=false")
    expect_identical(query_param_render(as_query_param("x", list(value = 1.0, negate = TRUE))), "x!=1")
    expect_identical(
        query_param_render(as_query_param("x", list(value = "solr+json", negate = TRUE))),
        "x!=solr%2Bjson"
    )

    expect_identical(query_param_render(as_query_param("x", TRUE), space = TRUE), "x = true")
    expect_identical(query_param_render(as_query_param("x", 1.0), space = TRUE), "x = 1")
    expect_identical(query_param_render(as_query_param("x", "solr+json"), space = TRUE), "x = solr%2Bjson")

    # can build query url
    index_node <- "https://esgf-node.llnl.gov/esg-search"
    expect_null(query_build(index_node, list(project = NULL)))
    expect_true(grepl(
        "CMIP5",
        query_build(index_node, list(project = "CMIP6", others = list(project = "CMIP5"))),
        fixed = TRUE
    ))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(index_node, list(project = "CMIP6", others = list(project = "CMIP5", table_id = "Amon"))),
        fixed = TRUE
    ))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(
            index_node,
            list(
                project = as_query_param("project", "CMIP6"),
                others = list(
                    project = "CMIP5",
                    table_id = as_query_param("table_id", "Amon")
                )
            )
        ),
        fixed = TRUE
    ))

    encoded_url <- esg_query("https://example.org")$
        nominal_resolution("100 km")$
        datetime_range(start = "2050", stop = "2080")$
        url()
    expect_true(grepl("format=application%2Fsolr%2Bjson", encoded_url, fixed = TRUE))
    expect_true(grepl("nominal_resolution=100+km,100km", encoded_url, fixed = TRUE))
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

    bridge_date_url <- esg_query("https://esgf-node.ornl.gov")$
        datetime_range(start = "2050", stop = "2080")$
        url()
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

    bridge_url <- query_build(
        "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        list(activity_id = as_query_param("activity_id", list(value = "ScenarioMIP", negate = TRUE)))
    )
    expect_true(grepl("query=NOT%20%28activity_id%3A%22ScenarioMIP%22%29", bridge_url, fixed = TRUE))

    expect_error(
        query_build("https://esgf-node.ornl.gov/esgf-1-5-bridge", list(type = "Aggregation")),
        "Bridge index nodes do not support.*Aggregation"
    )
    expect_true(grepl(
        "type=Aggregation",
        query_build("https://esgf-data.dkrz.de", list(type = "Aggregation")),
        fixed = TRUE
    ))
})
# }}}

# esg_query() {{{
test_that("esg_query()", {
    expect_s3_class(EsgQuery$new(), "EsgQuery")
    expect_s3_class(esg_query(), "EsgQuery")

    q_state <- esg_query("https://esgf.ceda.ac.uk")$experiment_id("ssp585")$fields("source_id")
    expect_identical(q_state$index_node(), "https://esgf.ceda.ac.uk")

    state <- q_state$state()
    expect_named(state, c("index_node", "parameter"))
    expect_identical(state$index_node, "https://esgf.ceda.ac.uk")
    expect_named(state$parameter, c("facet", "control"))
    expect_identical(query_param_value(state$parameter$facet$experiment_id), "ssp585")

    state_all <- q_state$state(null = TRUE)
    expect_named(state_all$parameter, c("facet", "query", "control", "others"))
    expect_null(state_all$parameter$query$datetime_start)

    expect_s3_class(q_state$index_node("esgf.nci.org.au"), "EsgQuery")
    expect_identical(q_state$index_node(), "https://esgf.nci.org.au")
    expect_identical(query_param_value(q_state$experiment_id()), "ssp585")
    expect_identical(query_param_value(q_state$fields()), "source_id")

    expect_s3_class(q_state$reset(), "EsgQuery")
    expect_identical(q_state$index_node(), "https://esgf.nci.org.au")
    expect_null(q_state$experiment_id())
    expect_identical(query_param_value(q_state$project()), "CMIP6")
    expect_identical(query_param_value(q_state$fields()), "*")

    skip_on_cran()

    index_node <- get_fast_index_node()
    q <- expect_s3_class(EsgQuery$new(index_node), "EsgQuery")
    q <- expect_s3_class(esg_query(index_node), "EsgQuery")
})
# }}}

# EsgQuery$list_facets() {{{
test_that("EsgQuery$list_facets()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_type(q$list_facets(), "character")

    # bridge node directly returns predefined facet names
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_type(q$list_facets(), "character")
    expect_equal(q$list_facets(), FIELDS_FACETS_COMMON)
})
# }}}

# EsgQuery$list_fields() {{{
test_that("EsgQuery$list_fields()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_type(q$list_fields(), "character")
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_type(q$list_fields(), "character")
})
# }}}

# EsgQuery$list_shards() {{{
test_that("EsgQuery$list_shards()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_type(q$list_shards(), "character")
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_type(q$list_shards(), "character")
})
# }}}

# EsgQuery$list_values() {{{
test_that("EsgQuery$list_values()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    q <- expect_s3_class(esg_query(index_node_normal), "EsgQuery")
    expect_type(q$list_values("activity_id"), "integer")
    q <- expect_s3_class(esg_query(index_node_bridge), "EsgQuery")
    expect_type(q$list_values(c("activity_id", "experiment_id")), "list")
    expect_named(q$list_values(c("activity_id", "experiment_id")), c("activity_id", "experiment_id"))
})
# }}}

# EsgQuery listing cache {{{
test_that("EsgQuery listing cache respects max_age", {
    dir <- tempfile("epwshiftr-expired-cache-")
    cache <- DiskCache$new(dir, max_age = 0.1)
    old_cache <- set_cache(cache)
    withr::defer({
        set_cache(old_cache)
        cache$destroy()
    })
    local_cache_mode("normal")

    url <- "https://example.org/esg-search/search?project=CMIP6"
    key <- get_response_cache_key(url)
    cached <- list(timestamp = Sys.time() - 3600, value = "old")
    cache$set(key, cached)
    Sys.setFileTime(file.path(cache$info()$dir, paste0(key, ".rds")), Sys.time() - 3600)

    calls <- 0L
    fetched <- list(timestamp = Sys.time(), value = "new")
    testthat::local_mocked_bindings(
        read_json_response = function(...) {
            calls <<- calls + 1L
            fetched
        },
        .package = "epwshiftr"
    )

    q <- esg_query("https://example.org")
    expect_identical(priv(q)$query_listing_cached(url, force = FALSE, type = "facet"), fetched)
    expect_equal(calls, 1L)
})

test_that("EsgQuery listing cache treats expired offline entries as misses", {
    dir <- tempfile("epwshiftr-expired-cache-")
    cache <- DiskCache$new(dir, max_age = 0.1)
    old_cache <- set_cache(cache)
    withr::defer({
        set_cache(old_cache)
        cache$destroy()
    })
    local_cache_mode("offline")

    url <- "https://example.org/esg-search/search?project=CMIP6"
    key <- get_response_cache_key(url)
    cache$set(key, list(timestamp = Sys.time() - 3600, value = "old"))
    Sys.setFileTime(file.path(cache$info()$dir, paste0(key, ".rds")), Sys.time() - 3600)

    q <- esg_query("https://example.org")
    expect_error(
        priv(q)$query_listing_cached(url, force = FALSE, type = "facet"),
        "Cache miss in offline mode"
    )
})
# }}}

# EsgQuery$project() and other facet methods {{{
test_that("EsgQuery$project() and other facet methods", {
    q <- esg_query()

    # project
    expect_equal(query_param_value(q$project()), "CMIP6")
    expect_equal(query_param_value(q$project("CMIP5")$project()), "CMIP5")
    expect_equal(query_param_value(q$project(-"CMIP5")$project()), "CMIP5")
    expect_equal(query_param_value(q$project(!"CMIP5")$project()), "CMIP5")

    # activity id
    expect_null(q$activity_id())
    expect_equal(query_param_value(q$activity_id(!c("CFMIP", "ScenarioMIP"))$activity_id()), c("CFMIP", "ScenarioMIP"))
    expect_null(q$activity_id(NULL)$activity_id())

    # experiment_id
    expect_null(q$experiment_id())
    expect_equal(query_param_value(q$experiment_id(!c("ssp126", "ssp585"))$experiment_id()), c("ssp126", "ssp585"))
    expect_null(q$experiment_id(NULL)$experiment_id())

    # source_id
    expect_null(q$source_id())
    expect_equal(query_param_value(q$source_id(!c("BCC-CSM2-MR", "CESM2"))$source_id()), c("BCC-CSM2-MR", "CESM2"))
    expect_null(q$source_id(NULL)$source_id())

    # variable_id
    expect_null(q$variable_id())
    expect_equal(query_param_value(q$variable_id(!c("tas", "pr"))$variable_id()), c("tas", "pr"))
    expect_null(q$variable_id(NULL)$variable_id())

    # frequency
    expect_null(q$frequency())
    expect_equal(query_param_value(q$frequency(!c("1hr", "day"))$frequency()), c("1hr", "day"))
    expect_null(q$frequency(NULL)$frequency())

    # variant_label
    expect_null(q$variant_label())
    expect_equal(
        query_param_value(q$variant_label(!c("r1i1p1f1", "r1i2p1f1"))$variant_label()),
        c("r1i1p1f1", "r1i2p1f1")
    )
    expect_null(q$variant_label(NULL)$variant_label())

    # nominal_resolution
    expect_null(q$nominal_resolution())
    expect_equal(query_param_value(q$nominal_resolution(c("100 km", "1x1 degree"))$nominal_resolution()), {
        c("100+km", "1x1+degree", "100km")
    })
    expect_true(q$nominal_resolution()@encoded)
    expect_null(q$nominal_resolution(NULL)$nominal_resolution())

    # data_node
    expect_null(q$data_node())
    expect_equal(query_param_value(q$data_node("esgf-node.ornl.gov")$data_node()), "esgf-node.ornl.gov")
    expect_null(q$data_node(NULL)$data_node())
})
# }}}

# EsgQuery$facets() {{{
test_that("EsgQuery$facets()", {
    expect_null(esg_query()$facets())
    expect_equal(
        query_param_value(esg_query()$facets(c("activity_id", "source_id"))$facets()),
        c("activity_id", "source_id")
    )
    expect_null(esg_query()$facets(NULL)$facets())
})
# }}}

# EsgQuery$fields() {{{
test_that("EsgQuery$fields()", {
    expect_equal(query_param_value(esg_query()$fields()), "*")
    expect_equal(
        query_param_value(esg_query()$fields(c("activity_id", "source_id"))$fields()),
        c("activity_id", "source_id")
    )
    expect_equal(query_param_value(esg_query()$fields("*")$fields()), "*")
    expect_null(esg_query()$fields(NULL)$fields())
})
# }}}

# EsgQuery$shards() {{{
test_that("EsgQuery$shards()", {
    q <- expect_s3_class(esg_query(), "EsgQuery")

    expect_null(q$shards())
    expect_false(query_param_value(q$distrib(FALSE)$distrib()))
    expect_error(q$shards("a"), "distrib")
    expect_true(query_param_value(q$distrib(TRUE)$distrib()))
    expect_s3_class(q$shards("a"), "EsgQuery")
    expect_equal(
        query_param_value(q$shards("esgf-solr.ceda.ac.uk:8983/solr")$shards()),
        "esgf-solr.ceda.ac.uk:8983/solr"
    )
    expect_null(q$shards(NULL)$shards())
})
# }}}

# EsgQuery$replica() {{{
test_that("EsgQuery$replica()", {
    expect_null(esg_query()$replica())
    expect_equal(query_param_value(esg_query()$replica(TRUE)$replica()), TRUE)
    expect_equal(query_param_value(esg_query()$replica(FALSE)$replica()), FALSE)
    expect_null(esg_query()$replica(NULL)$replica())
})
# }}}

# helper: decode a query URL and return the query= value as plain text
decode_query <- function(url) {
    m <- regmatches(url, regexpr("(?<=query=)[^&]*", url, perl = TRUE))
    if (!length(m)) {
        return(character(0L))
    }
    URLdecode(m)
}

# EsgQuery$datetime_range() {{{
test_that("EsgQuery$datetime_range()", {
    withr::local_options(epwshiftr.solr_date_math_now = utc("2025-06-13 12:34:56"))

    q <- esg_query()

    # getter: initially returns NULL for both
    result <- q$datetime_range()
    expect_null(result$start)
    expect_null(result$stop)

    # --- normal inputs ---

    # full ISO 8601: start -> datetime_start:[* TO ...]
    q$datetime_range(start = "2017-01-01T00:00:00Z")
    expect_match(decode_query(q$url()), 'datetime_start:[* TO "2017-01-01T00:00:00Z"]', fixed = TRUE)

    # full ISO 8601: stop -> datetime_stop:[... TO *]
    q2 <- esg_query()$datetime_range(stop = "2025-01-01T00:00:00Z")
    expect_match(
        decode_query(q2$url()),
        '(datetime_stop:["2025-01-01T00:00:00Z" TO *] OR datetime_end:["2025-01-01T00:00:00Z" TO *])',
        fixed = TRUE
    )

    # simplified date: auto-completed to ISO 8601
    q3 <- esg_query()$datetime_range(start = "2017")
    expect_match(decode_query(q3$url()), 'datetime_start:[* TO "2017-01-01T00:00:00Z"]', fixed = TRUE)

    # Bridge Date Math is evaluated locally because the bridge API does not support it.
    q4 <- esg_query()$datetime_range(start = "NOW-1YEAR")
    expect_match(decode_query(q4$url()), 'datetime_start:[* TO "2024-06-13T12:34:56Z"]', fixed = TRUE)

    q5 <- esg_query()$datetime_range(stop = "NOW+6MONTHS")
    expect_match(
        decode_query(q5$url()),
        '(datetime_stop:["2025-12-13T12:34:56Z" TO *] OR datetime_end:["2025-12-13T12:34:56Z" TO *])',
        fixed = TRUE
    )

    q_fixed_math <- esg_query()$datetime_range(start = "2025-06-13T00:00:00Z-1YEAR")
    expect_match(
        decode_query(q_fixed_math$url()),
        'datetime_start:[* TO "2024-06-13T00:00:00Z"]',
        fixed = TRUE
    )

    normal_math <- esg_query("https://example.org")$datetime_range(
        start = "NOW-1YEAR",
        stop = "2025-06-13T00:00:00Z+1YEAR"
    )
    normal_query <- decode_query(normal_math$url())
    expect_match(normal_query, "datetime_start:[* TO NOW-1YEAR]", fixed = TRUE)
    expect_match(normal_query, "datetime_stop:[2025-06-13T00:00:00Z+1YEAR TO *]", fixed = TRUE)

    # complete Range expression: used directly as the field value
    q6 <- esg_query()$datetime_range(start = "[2017-01-01T00:00:00Z TO 2020-01-01T00:00:00Z]")
    expect_match(
        decode_query(q6$url()),
        'datetime_start:["2017-01-01T00:00:00Z" TO "2020-01-01T00:00:00Z"]',
        fixed = TRUE
    )

    # both start and end: two conditions joined by AND in query=
    q7 <- esg_query()$datetime_range(
        start = "2017-01-01T00:00:00Z",
        stop = "2025-01-01T00:00:00Z"
    )
    query7 <- decode_query(q7$url())
    expect_match(query7, 'datetime_start:[* TO "2017-01-01T00:00:00Z"]', fixed = TRUE)
    expect_match(
        query7,
        '(datetime_stop:["2025-01-01T00:00:00Z" TO *] OR datetime_end:["2025-01-01T00:00:00Z" TO *])',
        fixed = TRUE
    )
    expect_match(query7, " AND ", fixed = TRUE)

    # NULL: clears the parameter
    q8 <- esg_query()$datetime_range(start = "2017-01-01T00:00:00Z")
    q8$datetime_range(start = NULL)
    expect_null(q8$datetime_range()$start)

    # raw REST temporal keywords are supported when no structured helper is set
    raw <- esg_query()$params(start = "2020", end = "2021")
    raw_url <- utils::URLdecode(raw$url())
    expect_match(raw_url, "start=2020", fixed = TRUE)
    expect_match(raw_url, "end=2021", fixed = TRUE)

    # structured helper takes precedence over raw REST temporal keywords
    helper_first <- esg_query()$datetime_range(start = "2020")
    expect_warning(
        helper_first$params(start = "2019", end = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    helper_first_query <- decode_query(helper_first$url())
    helper_first_url <- utils::URLdecode(helper_first$url())
    expect_match(helper_first_query, 'datetime_start:[* TO "2020-01-01T00:00:00Z"]', fixed = TRUE)
    expect_false(grepl("start=2019", helper_first_url, fixed = TRUE))
    expect_false(grepl("end=2021", helper_first_url, fixed = TRUE))

    raw_first <- esg_query()$params(start = "2019", end = "2021")
    expect_warning(
        raw_first$datetime_range(stop = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    raw_first_query <- decode_query(raw_first$url())
    raw_first_url <- utils::URLdecode(raw_first$url())
    expect_match(
        raw_first_query,
        '(datetime_stop:["2020-01-01T00:00:00Z" TO *] OR datetime_end:["2020-01-01T00:00:00Z" TO *])',
        fixed = TRUE
    )
    expect_false(grepl("start=2019", raw_first_url, fixed = TRUE))
    expect_false(grepl("end=2021", raw_first_url, fixed = TRUE))

    # --- error inputs ---

    # invalid format: solrdt_parse() errors
    expect_error(esg_query()$datetime_range(start = "not-a-date"))
})
# }}}

# EsgQuery$timestamp_range() {{{
test_that("EsgQuery$timestamp_range()", {
    withr::local_options(epwshiftr.solr_date_math_now = utc("2025-06-13 12:34:56"))

    q <- esg_query()

    # getter: initially returns NULL for both
    result <- q$timestamp_range()
    expect_null(result$from)
    expect_null(result$to)

    # --- normal inputs ---

    # full ISO 8601 from + to -> _timestamp:[from TO to]
    q1 <- esg_query()$timestamp_range(
        from = "2020-01-01T00:00:00Z",
        to = "2021-01-01T00:00:00Z"
    )
    ts1 <- q1$timestamp_range()
    expect_s3_class(ts1$from, "S7_object")
    expect_s3_class(ts1$to, "S7_object")
    expect_true(is.solrdt(query_param_value(ts1$from)))
    expect_true(is.solrdt(query_param_value(ts1$to)))
    expect_identical(
        unname(priv(q1)$parameter$render("_timestamp")),
        "_timestamp:[2020-01-01T00:00:00Z TO 2021-01-01T00:00:00Z]"
    )
    expect_match(
        decode_query(q1$url()),
        '_timestamp:["2020-01-01T00:00:00Z" TO "2021-01-01T00:00:00Z"]',
        fixed = TRUE
    )

    # simplified date: auto-completed, to defaults to *
    q2 <- esg_query()$timestamp_range(from = "2020")
    expect_match(decode_query(q2$url()), '_timestamp:["2020-01-01T00:00:00Z" TO *]', fixed = TRUE)

    # Date Math
    q3 <- esg_query()$timestamp_range(from = "NOW-1YEAR")
    expect_match(decode_query(q3$url()), '_timestamp:["2024-06-13T12:34:56Z" TO *]', fixed = TRUE)

    # only to
    q4 <- esg_query()$timestamp_range(to = "2021-01-01T00:00:00Z")
    expect_match(decode_query(q4$url()), '_timestamp:[* TO "2021-01-01T00:00:00Z"]', fixed = TRUE)

    # update existing lower boundary by setting upper boundary later
    q5 <- esg_query()$timestamp_range(from = "2020")
    q5$timestamp_range(to = "2021")
    expect_match(
        decode_query(q5$url()),
        '_timestamp:["2020-01-01T00:00:00Z" TO "2021-01-01T00:00:00Z"]',
        fixed = TRUE
    )

    # existing Date Math boundary is preserved when updating the other side
    q6 <- esg_query()$timestamp_range(from = "NOW-1YEAR")
    q6$timestamp_range(to = "2021")
    expect_match(
        decode_query(q6$url()),
        '_timestamp:["2024-06-13T12:34:56Z" TO "2021-01-01T00:00:00Z"]',
        fixed = TRUE
    )

    # NULL clears the parameter
    q7 <- esg_query()$timestamp_range(from = "2020-01-01T00:00:00Z")
    q7$timestamp_range(from = NULL)
    expect_null(q7$timestamp_range()$from)
    expect_null(q7$timestamp_range()$to)

    # fully unbounded range is normalized away
    q8 <- esg_query()$timestamp_range(from = "*", to = "*")
    expect_null(q8$timestamp_range()$from)
    expect_null(q8$timestamp_range()$to)
    expect_identical(decode_query(q8$url()), character(0L))

    # raw REST timestamp keywords are supported when no structured helper is set
    raw <- esg_query()$params(from = "2020", to = "2021")
    raw_url <- utils::URLdecode(raw$url())
    expect_match(raw_url, "from=2020", fixed = TRUE)
    expect_match(raw_url, "to=2021", fixed = TRUE)

    # structured helper takes precedence over raw REST timestamp keywords
    helper_first <- esg_query()$timestamp_range(from = "2020")
    expect_warning(
        helper_first$params(from = "2019", to = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    helper_first_query <- decode_query(helper_first$url())
    helper_first_url <- utils::URLdecode(helper_first$url())
    expect_match(helper_first_query, '_timestamp:["2020-01-01T00:00:00Z" TO *]', fixed = TRUE)
    expect_false(grepl("from=2019", helper_first_url, fixed = TRUE))
    expect_false(grepl("to=2021", helper_first_url, fixed = TRUE))

    raw_first <- esg_query()$params(from = "2019", to = "2021")
    expect_warning(
        raw_first$timestamp_range(to = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    raw_first_query <- decode_query(raw_first$url())
    raw_first_url <- utils::URLdecode(raw_first$url())
    expect_match(raw_first_query, '_timestamp:[* TO "2020-01-01T00:00:00Z"]', fixed = TRUE)
    expect_false(grepl("from=2019", raw_first_url, fixed = TRUE))
    expect_false(grepl("to=2021", raw_first_url, fixed = TRUE))

    # --- error inputs ---

    # Range syntax not supported for from/to
    expect_error(esg_query()$timestamp_range(from = "[2020 TO 2021]"), "range")

    # invalid format
    expect_error(esg_query()$timestamp_range(from = "not-a-date"))
})
# }}}

# EsgQuery$version_range() {{{
test_that("EsgQuery$version_range()", {
    q <- esg_query()

    # getter: initially returns NULL for both
    result <- q$version_range()
    expect_null(result$min)
    expect_null(result$max)

    # --- normal inputs ---

    # YYYYMMDD: used as-is
    q1 <- esg_query()$version_range(min = "20200101")
    expect_match(decode_query(q1$url()), "version:[20200101 TO *]", fixed = TRUE)

    # simplified year: converted to YYYYMMDD
    q2 <- esg_query()$version_range(min = "2020")
    expect_match(decode_query(q2$url()), "version:[20200101 TO *]", fixed = TRUE)

    # simplified year-month: converted to YYYYMMDD
    q3 <- esg_query()$version_range(max = "2020-06")
    expect_match(decode_query(q3$url()), "version:[* TO 20200601]", fixed = TRUE)

    # both min and max
    q4 <- esg_query()$version_range(min = "20200101", max = "20211231")
    query4 <- decode_query(q4$url())
    expect_match(query4, "version:[20200101 TO *]", fixed = TRUE)
    expect_match(query4, "version:[* TO 20211231]", fixed = TRUE)

    # NULL clears the parameter
    q5 <- esg_query()$version_range(min = "20200101")
    q5$version_range(min = NULL)
    expect_null(q5$version_range()$min)

    # --- error inputs ---

    # Range syntax not supported
    expect_error(esg_query()$version_range(min = "[2020 TO 2025]"), "range")

    # Date Math not supported
    expect_error(esg_query()$version_range(min = "NOW-1YEAR"), "[Dd]ate [Mm]ath")

    # invalid format
    expect_error(esg_query()$version_range(min = "not-a-date"))
})
# }}}

# EsgQuery$latest() {{{
test_that("EsgQuery$latest()", {
    expect_null(esg_query()$latest())
    expect_false(query_param_value(esg_query()$latest(FALSE)$latest()))
    expect_true(query_param_value(esg_query()$latest(TRUE)$latest()))
    expect_null(esg_query()$latest(TRUE)$latest(NULL)$latest())
})
# }}}

# EsgQuery$limit() {{{
test_that("EsgQuery$limit()", {
    expect_equal(query_param_value(esg_query()$limit()), 10L)
    expect_warning(lim <- esg_query()$limit(12000)$limit(), "10,000")
    expect_equal(query_param_value(lim), 10000L)
    expect_equal(query_param_value(esg_query()$limit(10L)$limit()), 10L)
})
# }}}

# EsgQuery$offset() {{{
test_that("EsgQuery$offset()", {
    expect_equal(query_param_value(esg_query()$offset()), 0L)
    expect_equal(query_param_value(esg_query()$offset(0)$offset()), 0L)
})
# }}}

# EsgQuery$params() {{{
test_that("EsgQuery$params()", {
    q <- expect_s3_class(esg_query(), "EsgQuery")

    # can use existing method for common parameters
    expect_false("type" %in% names(q))
    expect_false("format" %in% names(q))
    expect_equal(q$params(), list())
    expect_equal(q$params(nominal_resolution = !c("10 km", "25 km"))$params(), list())
    expect_error(q$params(table_id = "day", table_id = "hour"), "unique names")

    # can reset existing parameters
    expect_equal(query_param_value(q$frequency("day")$frequency()), "day")
    expect_equal(q$params(frequency = NULL)$params(), list())
    expect_null(q$frequency())
    params <- q$params(table_id = "Amon", member_id = "r1i1p1f1")$params()
    expect_named(params, c("table_id", "member_id"))
    expect_s3_class(params$table_id, "S7_object")
    expect_s3_class(params$member_id, "S7_object")
    expect_identical(query_param_value(params$table_id), "Amon")
    expect_identical(query_param_value(params$member_id), "r1i1p1f1")

    # cannot set query orchestration controls
    expect_error(q$params(format = "xml"), "cannot be set")
    expect_error(q$params(type = "File"), "cannot be set")
    expect_named(q$params(), c("table_id", "member_id"))

    # can restore original values in case of error
    expect_equal(query_param_value(q$frequency("day")$frequency()), "day")
    expect_error(q$params(frequency = "1hr", frequency = "a"), "Assertion")
    expect_equal(query_param_value(q$frequency()), "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())
})
# }}}

# EsgQuery$url(), EsgQuery$count() {{{
test_that("EsgQuery$url(), EsgQuery$count()", {
    skip_on_cran()
    index_node <- INDEX_NODES[["CEDA"]]

    # can get url
    expect_type(EsgQuery$new(index_node)$nominal_resolution("100 km")$url(), "character")
    expect_type(EsgQuery$new(index_node)$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(EsgQuery$new(index_node)$params(project = "CMIP5", table_id = "Amon")$url(), "character")

    # can get count
    expect_type(EsgQuery$new(index_node)$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgQuery$new(index_node)$frequency("1hr")$count(TRUE), "integer")
    cnt <- expect_type(EsgQuery$new(index_node)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))
})
# }}}

# EsgQuery$collect() {{{
test_that("EsgQuery$collect(type=) collects child results through Dataset workflow", {
    q <- esg_query("https://example.org")$
        project("CMIP6")$
        datetime_range(start = "2050-01-01T00:00:00Z", stop = "2080-12-31T23:59:59Z")$
        limit(3L)

    calls <- list()
    local_response <- function(docs) {
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
            timestamp = Sys.time()
        )
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
        query_collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            calls[[length(calls) + 1L]] <<- list(
                index_node = index_node,
                params = params,
                required_fields = required_fields,
                all = all,
                limit = limit,
                constraints = constraints,
                dict_check = dict_check
            )

            type <- query_param_value(params$type())
            docs <- if (identical(type, "Dataset")) {
                local_dataset_docs
            } else {
                local_file_docs
            }
            response <- local_response(docs)
            params$fields(c(query_param_value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
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
    expect_identical(query_param_value(calls[[1L]]$params$type()), "Dataset")
    expect_false(calls[[2L]]$all)
    expect_equal(calls[[2L]]$limit, 3L)
    expect_true(calls[[2L]]$dict_check)
    expect_identical(query_param_value(calls[[2L]]$params$type()), "File")
    expect_null(calls[[2L]]$params$source_id())
    expect_identical(query_param_value(calls[[2L]]$params$data_node()), "example.org")
    expect_identical(calls[[2L]]$params$render(c("datetime_start", "datetime_stop")), character())
    expect_identical(files$count(), 1L)

    expect_error(q$collect(type = "Dataset", source_id = "AWI-CM-1-1-MR"), "Additional query filters")
    expect_error(q$collect(type = "Dataset", fields = "id"), "`fields`")
})

test_that("query_collect includes only result-field constraints in fields", {
    captured_url <- character()
    testthat::local_mocked_bindings(
        read_json_response = function(url, ...) {
            captured_url <<- c(captured_url, url)
            list(response = list(
                numFound = 1L,
                docs = data.frame(
                    id = "dataset-id",
                    source_id = "source",
                    project = "CMIP6",
                    activity_id = "CMIP",
                    table_id = "Amon",
                    score = 1,
                    check.names = FALSE
                )
            ))
        },
        .package = "epwshiftr"
    )

    expect_warning(
        params <- QueryParamStore$new()$activity_id("CMIP")$fields("source_id")$facets("source_id")$shards("node")$params(
            table_id = "Amon",
            bbox = "0,0,1,1",
            start = "2020",
            end = "2021",
            from = "2020",
            to = "2021"
        ),
        NA
    )

    res <- query_collect(
        "https://example.org",
        params,
        required_fields = "id",
        constraints = TRUE
    )
    decoded_url <- utils::URLdecode(captured_url[[1L]])
    fields <- strsplit(
        regmatches(decoded_url, regexpr("(?<=fields=)[^&]*", decoded_url, perl = TRUE)),
        ",",
        fixed = TRUE
    )[[1L]]

    expect_identical(fields, c("source_id", "id", "project", "activity_id", "table_id"))
    expect_false("bbox" %in% fields)
    expect_false("start" %in% fields)
    expect_false("facets" %in% fields)
    expect_named(res$docs, c("id", "source_id", "project", "activity_id", "table_id"), ignore.order = TRUE)
    expect_s3_class(res$parameter, "QueryParamStore")
    expect_identical(
        query_param_value(res$parameter$fields()),
        c("source_id", "id", "project", "activity_id", "table_id")
    )
})

test_that("query_collect returns normalized effective parameters", {
    captured_url <- character()
    testthat::local_mocked_bindings(
        read_json_response = function(url, ...) {
            captured_url <<- c(captured_url, url)
            list(response = list(
                numFound = 1L,
                docs = data.frame(id = "dataset-id", score = 1, check.names = FALSE)
            ))
        },
        .package = "epwshiftr"
    )

    params <- QueryParamStore$new()$fields("id")$limit(5L)$offset(2L)
    res <- query_collect(
        "https://example.org",
        params,
        required_fields = c("size", "url"),
        all = TRUE,
        limit = 3L
    )

    expect_s3_class(res$parameter, "QueryParamStore")
    expect_identical(query_param_value(res$parameter$fields()), c("id", "size", "url", "project"))
    expect_identical(query_param_value(res$parameter$limit()), 3L)
    expect_identical(query_param_value(res$parameter$offset()), 0L)

    bridge <- query_collect(
        "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        params,
        required_fields = c("size", "url")
    )
    expect_null(bridge$parameter$fields())
})

test_that("query_collect records actual page query URLs", {
    captured_url <- character()
    testthat::local_mocked_bindings(
        read_json_response = function(url, ...) {
            captured_url <<- c(captured_url, url)
            docs <- if (length(captured_url) == 1L) {
                data.frame(id = c("dataset-1", "dataset-2"), score = 1, check.names = FALSE)
            } else {
                data.frame(id = "dataset-3", score = 1, check.names = FALSE)
            }
            list(response = list(numFound = 3L, docs = docs))
        },
        .package = "epwshiftr"
    )

    res <- query_collect(
        "https://example.org",
        QueryParamStore$new()$limit(2L),
        all = TRUE,
        limit = 2L
    )

    expect_equal(nrow(res$docs), 3L)
    expect_identical(res$context$query_url, captured_url)
    expect_true(grepl("offset=0", utils::URLdecode(captured_url[[1L]]), fixed = TRUE))
    expect_true(grepl("offset=2", utils::URLdecode(captured_url[[2L]]), fixed = TRUE))
})

test_that("EsgQuery$collect() warns for invalid local dictionary constraints", {
    local_esgdict_default(local_query_test_esgdict())
    testthat::local_mocked_bindings(
        read_json_response = function(url, ...) local_query_test_response(),
        .package = "epwshiftr"
    )

    q <- esg_query("https://example.org")$
        activity_id("ScenarioMIP")$
        experiment_id("historical")$
        limit(1L)

    expect_warning(
        res <- q$collect(),
        "ESG dictionary check found invalid query constraint"
    )
    expect_s3_class(res, "EsgResultDataset")
})

test_that("EsgQuery$collect() skips dictionary check when no local dictionary is available", {
    local_esgdict_default(NULL)
    withr::local_options(epwshiftr.dir_store = withr::local_tempdir())
    testthat::local_mocked_bindings(
        read_json_response = function(url, ...) local_query_test_response(),
        .package = "epwshiftr"
    )

    q <- esg_query("https://example.org")$
        activity_id("ScenarioMIP")$
        experiment_id("historical")$
        limit(1L)

    expect_warning(res <- q$collect(), NA)
    expect_s3_class(res, "EsgResultDataset")
})

test_that("EsgQuery$collect()", {
    skip_on_cran()
    index_node <- get_fast_index_node()

    q <- expect_s3_class(esg_query(index_node)$experiment_id("ssp585")$frequency("1hr")$fields("source_id"), "EsgQuery")

    # can collect the specified limit number of records
    res <- expect_s3_class(q$limit(1)$collect(), "EsgResultDataset")

    # can collect fields with constraints
    expect_true(all(c("project", "frequency", "source_id") %in% names(res)))

    # can collect required fields
    # NOTE: it is possible that some index nodes do not have
    # 'number_of_aggregations' field
    expect_true(
        all(setdiff(EsgResultDataset$private_fields$required_fields, "number_of_aggregations") %in% names(res))
    )

    # can collect all results with auto-pagination
    ## with maximum batch size
    expect_s3_class(q$collect(all = TRUE, limit = FALSE), "EsgResultDataset")
    ## with specified limits
    expect_s3_class(q$collect(all = TRUE, limit = 30), "EsgResultDataset")
})
# }}}

# EsgQuery local save/load round-trip {{{
test_that("EsgQuery$save() & EsgQuery$load() round-trip without network", {
    q <- EsgQuery$new("https://example.org")$activity_id("ScenarioMIP")$experiment_id("ssp585")$variable_id("tas")$limit(
        2L
    )$datetime_range(start = "2017")$timestamp_range(from = "NOW-1YEAR", to = "2021")$version_range(
        min = "2020",
        max = "2021"
    )$params(table_id = c("Amon", "day"))

    file <- tempfile(fileext = ".json")
    expect_type(q$save(file), "character")
    q_loaded <- expect_s3_class(esg_query()$load(file), "EsgQuery")
    expect_identical(
        priv(q_loaded)$parameter$serialize(null = TRUE),
        priv(q)$parameter$serialize(null = TRUE)
    )

    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    expect_named(json$parameter, c("facet", "query", "control", "others"))

    invalid_type <- json
    invalid_type$parameter$control$type$value <- "File"
    invalid_type_file <- tempfile(fileext = ".json")
    jsonlite::write_json(invalid_type, invalid_type_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(invalid_type_file), "Dataset queries")

    invalid_format <- json
    invalid_format$parameter$control$format$value <- "application/xml"
    invalid_format_file <- tempfile(fileext = ".json")
    jsonlite::write_json(invalid_format, invalid_format_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(invalid_format_file), "JSON response format")

    flat_parameter <- json
    flat_parameter$parameter <- c(
        json$parameter$facet,
        json$parameter$query,
        json$parameter$control,
        json$parameter$others
    )
    flat_parameter_file <- tempfile(fileext = ".json")
    jsonlite::write_json(flat_parameter, flat_parameter_file, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(flat_parameter_file), "subset")

    unlink(c(file, invalid_type_file, invalid_format_file, flat_parameter_file))
})
# }}}

# EsgQuery$save() & EsgQuery$load() {{{
test_that("EsgQuery$save() & EsgQuery$load()", {
    skip_on_cran()
    index_node <- INDEX_NODES[["CEDA"]]

    q <- EsgQuery$new(index_node)$activity_id("ScenarioMIP")$experiment_id("ssp585")$variable_id("tas")$limit(2)$params(
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
    q_query <- EsgQuery$new(index_node)$datetime_range(start = "2017")$timestamp_range(
        from = "NOW-1YEAR",
        to = "2021"
    )$version_range(min = "2020")
    expect_s3_class(priv(q_query)$parameter$datetime_range()$start, "S7_object")
    expect_s3_class(priv(q_query)$parameter$timestamp_range()$from, "S7_object")
    expect_s3_class(priv(q_query)$parameter$timestamp_range()$to, "S7_object")
    expect_s3_class(priv(q_query)$parameter$version_range()$min, "S7_object")
    file_query <- tempfile(fileext = ".json")
    expect_type(q_query$save(file_query), "character")
    q_query_loaded <- expect_s3_class(esg_query()$load(file_query), "EsgQuery")
    expect_equal(
        priv(q_query_loaded)$parameter$serialize(null = TRUE),
        priv(q_query)$parameter$serialize(null = TRUE)
    )

    query_json <- jsonlite::fromJSON(file_query, simplifyVector = TRUE, simplifyMatrix = FALSE)

    file_invalid_type <- tempfile(fileext = ".json")
    query_json$parameter$control$type$value <- "File"
    jsonlite::write_json(query_json, file_invalid_type, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(file_invalid_type), "Dataset queries")

    file_invalid_format <- tempfile(fileext = ".json")
    query_json$parameter$control$type$value <- "Dataset"
    query_json$parameter$control$format$value <- "application/xml"
    jsonlite::write_json(query_json, file_invalid_format, null = "null", auto_unbox = TRUE)
    expect_error(esg_query()$load(file_invalid_format), "JSON response format")

    # query object with results
    q$collect()
    file_collected <- tempfile(fileext = ".json")
    expect_type(q$save(file_collected), "character")
    expect_true(file.exists(file_collected))
    file_collected_copied <- tempfile(fileext = ".json")
    expect_true(file.copy(file_collected, file_collected_copied))
    expect_snapshot_file(file_collected_copied, "query_collected.json", transform = transform_json)
    q_collected <- expect_s3_class(esg_query()$load(file_collected), "EsgQuery")
    expect_equal(priv(q_collected)$index_node, priv(q)$index_node)
    expect_equal(
        priv(q_collected)$parameter$serialize(null = TRUE),
        priv(q)$parameter$serialize(null = TRUE)
    )

    unlink(c(file_query, file_invalid_type, file_invalid_format, file_collected, file_collected_copied))
})
# }}}

# EsgQuery$print() {{{
test_that("EsgQuery$print()", {
    skip_on_cran()

    expect_snapshot(
        EsgQuery$new("a")$params(table_id = "Amon", member_id = "r1i1p1f1")$print()
    )
})
# }}}

# vim: fdm=marker :
