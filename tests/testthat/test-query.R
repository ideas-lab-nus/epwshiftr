# new_query_param() {{{
test_that("ESGF Query Parameter works", {
    # can create new query parameter
    param <- expect_s3_class(
        new_query_param("x", list(value = LETTERS[1:3], negate = TRUE)),
        "EsgQueryParam"
    )

    expect_snapshot(format(new_query_param("x", list(value = TRUE, negate = TRUE))))
    expect_snapshot(format(new_query_param("x", list(value = 1.0, negate = TRUE))))
    # can encode parameter
    expect_snapshot(format(new_query_param("x", list(value = "solr+json", negate = TRUE))))

    # can add spaces
    expect_snapshot(format(new_query_param("x", TRUE), space = TRUE))
    expect_snapshot(format(new_query_param("x", 1.0), space = TRUE))
    expect_snapshot(format(new_query_param("x", "solr+json"), space = TRUE))

    # can print query parameter
    expect_snapshot(print(new_query_param("x", list(value = TRUE, negate = TRUE))))
    expect_snapshot(print(new_query_param("x", list(value = 1.0, negate = TRUE))))
    expect_snapshot(print(new_query_param("x", list(value = "solr+json", negate = TRUE))))

    # can build query url
    index_node <- "https://esgf-node.llnl.gov/esg-search"
    expect_null(query_build(index_node, list(project = NULL)))
    expect_true(grepl("CMIP5", query_build(index_node, list(project = "CMIP6", others = list(project = "CMIP5")))))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(index_node, list(project = "CMIP6", others = list(project = "CMIP5", table_id = "Amon")))
    ))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(
            index_node,
            list(
                project = new_query_param("project", "CMIP6"),
                others = list(
                    project = "CMIP5",
                    table_id = new_query_param("table_id", "Amon")
                )
            )
        )
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
    expect_equal(q$project()$value, "CMIP6")
    expect_equal(q$project("CMIP5")$project()$value, "CMIP5")
    expect_equal(q$project(-"CMIP5")$project()$value, "CMIP5")
    expect_equal(q$project(!"CMIP5")$project()$value, "CMIP5")

    # activity id
    expect_null(q$activity_id())
    expect_equal(q$activity_id(!c("CFMIP", "ScenarioMIP"))$activity_id()$value, c("CFMIP", "ScenarioMIP"))
    expect_null(q$activity_id(NULL)$activity_id())

    # experiment_id
    expect_null(q$experiment_id())
    expect_equal(q$experiment_id(!c("ssp126", "ssp585"))$experiment_id()$value, c("ssp126", "ssp585"))
    expect_null(q$experiment_id(NULL)$experiment_id())

    # source_id
    expect_null(q$source_id())
    expect_equal(q$source_id(!c("BCC-CSM2-MR", "CESM2"))$source_id()$value, c("BCC-CSM2-MR", "CESM2"))
    expect_null(q$source_id(NULL)$source_id())

    # variable_id
    expect_null(q$variable_id())
    expect_equal(q$variable_id(!c("tas", "pr"))$variable_id()$value, c("tas", "pr"))
    expect_null(q$variable_id(NULL)$variable_id())

    # frequency
    expect_null(q$frequency())
    expect_equal(q$frequency(!c("1hr", "day"))$frequency()$value, c("1hr", "day"))
    expect_null(q$frequency(NULL)$frequency())

    # variant_label
    expect_null(q$variant_label())
    expect_equal(q$variant_label(!c("r1i1p1f1", "r1i2p1f1"))$variant_label()$value, c("r1i1p1f1", "r1i2p1f1"))
    expect_null(q$variant_label(NULL)$variant_label())

    # nominal_resolution
    expect_null(q$nominal_resolution())
    expect_equal(q$nominal_resolution(c("100 km", "1x1 degree"))$nominal_resolution()$value, {
        res <- c("100+km", "1x1+degree", "100km")
        attr(res, "encoded") <- TRUE
        res
    })
    expect_null(q$nominal_resolution(NULL)$nominal_resolution())

    # data_node
    expect_null(q$data_node())
    expect_equal(q$data_node("esgf-node.ornl.gov")$data_node()$value, "esgf-node.ornl.gov")
    expect_null(q$data_node(NULL)$data_node())
})
# }}}

# EsgQuery$facets() {{{
test_that("EsgQuery$facets()", {
    expect_null(esg_query()$facets())
    expect_equal(esg_query()$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(esg_query()$facets(NULL)$facets())
})
# }}}

# EsgQuery$fields() {{{
test_that("EsgQuery$fields()", {
    expect_equal(esg_query()$fields()$value, "*")
    expect_equal(esg_query()$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(esg_query()$fields("*")$fields()$value, "*")
    expect_null(esg_query()$fields(NULL)$fields())
})
# }}}

# EsgQuery$shards() {{{
test_that("EsgQuery$shards()", {
    expect_s3_class(q <- esg_query(), "EsgQuery")

    expect_null(q$shards())
    expect_false(q$distrib(FALSE)$distrib()$value)
    expect_error(q$shards("a"), "distrib")
    expect_true(q$distrib(TRUE)$distrib()$value)
    expect_s3_class(q$shards("a"), "EsgQuery")
    expect_equal(
        q$shards("esgf-solr.ceda.ac.uk:8983/solr")$shards()$value,
        "esgf-solr.ceda.ac.uk:8983/solr"
    )
    expect_null(q$shards(NULL)$shards())
})
# }}}

# EsgQuery$replica() {{{
test_that("EsgQuery$replica()", {
    expect_null(esg_query()$replica())
    expect_equal(esg_query()$replica(TRUE)$replica()$value, TRUE)
    expect_equal(esg_query()$replica(FALSE)$replica()$value, FALSE)
    expect_null(esg_query()$replica(NULL)$replica())
})
# }}}

# EsgQuery$latest() {{{
test_that("EsgQuery$latest()", {
    expect_true(esg_query()$latest()$value)
    expect_false(esg_query()$latest(FALSE)$latest()$value)
    expect_true(esg_query()$latest(TRUE)$latest()$value)
})
# }}}

# EsgQuery$limit() {{{
test_that("EsgQuery$limit()", {
    expect_equal(esg_query()$limit()$value, 10L)
    expect_warning(lim <- esg_query()$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(esg_query()$limit(10L)$limit()$value, 10L)
})
# }}}

# EsgQuery$offset() {{{
test_that("EsgQuery$offset()", {
    expect_equal(esg_query()$offset()$value, 0L)
    expect_equal(esg_query()$offset(0)$offset()$value, 0L)
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
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_equal(q$params(frequency = NULL)$params(), list())
    expect_null(q$frequency())
    expect_equal(
        q$params(table_id = "Amon", member_id = "r1i1p1f1")$params(),
        list(
            table_id = new_query_param("table_id", "Amon"),
            member_id = new_query_param("member_id", "r1i1p1f1")
        )
    )

    # can reset format
    expect_warning(q$params(format = "xml"), "JSON")

    # can reset type
    expect_warning(expect_warning(q$params(type = "File")))

    # can restore original values in case of error
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_error(q$params(frequency = "1hr", frequency = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")

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
    expect_type(cnt <- EsgQuery$new(index_node)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))
})
# }}}

# EsgQuery$collect() {{{
test_that("EsgQuery$collect()", {
    skip_on_cran()
    index_node <- get_fast_index_node()

    q <- expect_s3_class(esg_query(index_node)$experiment_id("ssp585")$frequency("1hr")$fields("source_id"), "EsgQuery")

    # can collect the specified limit number of records
    res <- expect_s3_class(
        q$limit(1)$collect(),
        "EsgResultDataset"
    )

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

    unlink(c(file_collected, file_collected_copied))
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
