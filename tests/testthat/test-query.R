# QueryParam helpers {{{
local_test_cache(scope = "persist")
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
        c("datetime_start", "datetime_stop", "_timestamp", "version_min", "version_max")
    )

    restored_facet <- query_param_deserialize("project", list(kind = "facet", value = "CMIP6", negate = FALSE))
    expect_s3_class(restored_facet, "S7_object")
    expect_identical(query_param_value(restored_facet), "CMIP6")

    restored_query <- query_param_deserialize(
        "datetime_start",
        list(kind = "datetime_start", value = "datetime_start:[* TO 2017-12-31T23:59:59Z]")
    )
    expect_s3_class(restored_query, "S7_object")
    expect_identical(
        query_param_serialize(restored_query),
        list(kind = "datetime_start", value = "datetime_start:[* TO 2017-12-31T23:59:59Z]")
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
})
# }}}

# esg_query() {{{
test_that("esg_query()", {
    expect_s3_class(EsgQuery$new(), "EsgQuery")
    expect_s3_class(esg_query(), "EsgQuery")

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
        res <- c("100+km", "1x1+degree", "100km")
        attr(res, "encoded") <- TRUE
        res
    })
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
    q <- esg_query()

    # getter: initially returns NULL for both
    result <- q$datetime_range()
    expect_null(result$start)
    expect_null(result$end)

    # --- normal inputs ---

    # full ISO 8601: start -> datetime_start:[* TO ...]
    q$datetime_range(start = "2017-01-01T00:00:00Z")
    expect_match(decode_query(q$url()), "datetime_start:[* TO 2017-01-01T00:00:00Z]", fixed = TRUE)

    # full ISO 8601: end -> datetime_stop:[... TO *]
    q2 <- esg_query()$datetime_range(end = "2025-01-01T00:00:00Z")
    expect_match(decode_query(q2$url()), "datetime_stop:[2025-01-01T00:00:00Z TO *]", fixed = TRUE)

    # simplified date: auto-completed to ISO 8601
    q3 <- esg_query()$datetime_range(start = "2017")
    expect_match(decode_query(q3$url()), "datetime_start:[* TO 2017-01-01T00:00:00Z]", fixed = TRUE)

    # Date Math: passed through verbatim
    q4 <- esg_query()$datetime_range(start = "NOW-1YEAR")
    expect_match(decode_query(q4$url()), "datetime_start:[* TO NOW-1YEAR]", fixed = TRUE)

    q5 <- esg_query()$datetime_range(end = "NOW+6MONTHS")
    expect_match(decode_query(q5$url()), "datetime_stop:[NOW+6MONTHS TO *]", fixed = TRUE)

    # complete Range expression: used directly as the field value
    q6 <- esg_query()$datetime_range(start = "[2017-01-01T00:00:00Z TO 2020-01-01T00:00:00Z]")
    expect_match(
        decode_query(q6$url()),
        "datetime_start:[2017-01-01T00:00:00Z TO 2020-01-01T00:00:00Z]",
        fixed = TRUE
    )

    # both start and end: two conditions joined by AND in query=
    q7 <- esg_query()$datetime_range(
        start = "2017-01-01T00:00:00Z",
        end = "2025-01-01T00:00:00Z"
    )
    query7 <- decode_query(q7$url())
    expect_match(query7, "datetime_start:[* TO 2017-01-01T00:00:00Z]", fixed = TRUE)
    expect_match(query7, "datetime_stop:[2025-01-01T00:00:00Z TO *]", fixed = TRUE)
    expect_match(query7, " AND ", fixed = TRUE)

    # NULL: clears the parameter
    q8 <- esg_query()$datetime_range(start = "2017-01-01T00:00:00Z")
    q8$datetime_range(start = NULL)
    expect_null(q8$datetime_range()$start)

    # --- error inputs ---

    # invalid format: solrdt_parse() errors
    expect_error(esg_query()$datetime_range(start = "not-a-date"))
})
# }}}

# EsgQuery$timestamp_range() {{{
test_that("EsgQuery$timestamp_range()", {
    q <- esg_query()

    # getter: initially NULL
    expect_null(q$timestamp_range())

    # --- normal inputs ---

    # full ISO 8601 from + to -> _timestamp:[from TO to]
    q1 <- esg_query()$timestamp_range(
        from = "2020-01-01T00:00:00Z",
        to = "2021-01-01T00:00:00Z"
    )
    expect_s3_class(priv(q1)$parameter$`_timestamp`, "S7_object")
    expect_identical(query_param_kind(priv(q1)$parameter$`_timestamp`), "timestamp_range")
    expect_true(is.solrdt(query_param_value(priv(q1)$parameter$`_timestamp`)))
    expect_match(
        decode_query(q1$url()),
        "_timestamp:[2020-01-01T00:00:00Z TO 2021-01-01T00:00:00Z]",
        fixed = TRUE
    )

    # simplified date: auto-completed, to defaults to *
    q2 <- esg_query()$timestamp_range(from = "2020")
    expect_match(decode_query(q2$url()), "_timestamp:[2020-01-01T00:00:00Z TO *]", fixed = TRUE)

    # Date Math
    q3 <- esg_query()$timestamp_range(from = "NOW-1YEAR")
    expect_match(decode_query(q3$url()), "_timestamp:[NOW-1YEAR TO *]", fixed = TRUE)

    # only to
    q4 <- esg_query()$timestamp_range(to = "2021-01-01T00:00:00Z")
    expect_match(decode_query(q4$url()), "_timestamp:[* TO 2021-01-01T00:00:00Z]", fixed = TRUE)

    # update existing lower boundary by setting upper boundary later
    q5 <- esg_query()$timestamp_range(from = "2020")
    q5$timestamp_range(to = "2021")
    expect_match(
        decode_query(q5$url()),
        "_timestamp:[2020-01-01T00:00:00Z TO 2021-01-01T00:00:00Z]",
        fixed = TRUE
    )

    # existing Date Math boundary is preserved when updating the other side
    q6 <- esg_query()$timestamp_range(from = "NOW-1YEAR")
    q6$timestamp_range(to = "2021")
    expect_match(
        decode_query(q6$url()),
        "_timestamp:[NOW-1YEAR TO 2021-01-01T00:00:00Z]",
        fixed = TRUE
    )

    # NULL clears the parameter
    q7 <- esg_query()$timestamp_range(from = "2020-01-01T00:00:00Z")
    q7$timestamp_range(from = NULL)
    expect_null(q7$timestamp_range())

    # fully unbounded range is normalized away
    q8 <- esg_query()$timestamp_range(from = "*", to = "*")
    expect_null(q8$timestamp_range())
    expect_identical(decode_query(q8$url()), character(0L))

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
    expect_null(result$start)
    expect_null(result$end)

    # --- normal inputs ---

    # YYYYMMDD: used as-is
    q1 <- esg_query()$version_range(start = "20200101")
    expect_match(decode_query(q1$url()), "version:[20200101 TO *]", fixed = TRUE)

    # simplified year: converted to YYYYMMDD
    q2 <- esg_query()$version_range(start = "2020")
    expect_match(decode_query(q2$url()), "version:[20200101 TO *]", fixed = TRUE)

    # simplified year-month: converted to YYYYMMDD
    q3 <- esg_query()$version_range(end = "2020-06")
    expect_match(decode_query(q3$url()), "version:[* TO 20200601]", fixed = TRUE)

    # both start and end
    q4 <- esg_query()$version_range(start = "20200101", end = "20211231")
    query4 <- decode_query(q4$url())
    expect_match(query4, "version:[20200101 TO *]", fixed = TRUE)
    expect_match(query4, "version:[* TO 20211231]", fixed = TRUE)

    # NULL clears the parameter
    q5 <- esg_query()$version_range(start = "20200101")
    q5$version_range(start = NULL)
    expect_null(q5$version_range()$start)

    # --- error inputs ---

    # Range syntax not supported
    expect_error(esg_query()$version_range(start = "[2020 TO 2025]"), "range")

    # Date Math not supported
    expect_error(esg_query()$version_range(start = "NOW-1YEAR"), "[Dd]ate [Mm]ath")

    # invalid format
    expect_error(esg_query()$version_range(start = "not-a-date"))
})
# }}}

# EsgQuery$latest() {{{
test_that("EsgQuery$latest()", {
    expect_true(query_param_value(esg_query()$latest()))
    expect_false(query_param_value(esg_query()$latest(FALSE)$latest()))
    expect_true(query_param_value(esg_query()$latest(TRUE)$latest()))
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

    # can reset format
    expect_warning(q$params(format = "xml"), "JSON")

    # can reset type
    expect_warning(expect_warning(q$params(type = "File")))

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
    expect_equal(priv(q_empty)$url_index_node, priv(q)$url_index_node)
    expect_equal(priv(q_empty)$parameter, priv(q)$parameter)
    expect_equal(priv(q_empty)$response, priv(q)$response)

    # structured query= parameters round-trip through save/load
    q_query <- EsgQuery$new(index_node)$datetime_range(start = "2017")$timestamp_range(
        from = "NOW-1YEAR",
        to = "2021"
    )$version_range(start = "2020")
    expect_s3_class(priv(q_query)$parameter$datetime_start, "S7_object")
    expect_s3_class(priv(q_query)$parameter$`_timestamp`, "S7_object")
    expect_s3_class(priv(q_query)$parameter$version_min, "S7_object")
    file_query <- tempfile(fileext = ".json")
    expect_type(q_query$save(file_query), "character")
    q_query_loaded <- expect_s3_class(esg_query()$load(file_query), "EsgQuery")
    expect_equal(priv(q_query_loaded)$parameter, priv(q_query)$parameter)

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
    expect_equal(priv(q_collected)$parameter, priv(q)$parameter)

    unlink(c(file_query, file_collected, file_collected_copied))
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
