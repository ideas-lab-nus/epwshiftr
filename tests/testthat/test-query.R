# new_query_param() {{{
test_that("ESGF Query Parameter works", {
    # can create new query parameter
    expect_s3_class(
        param <- new_query_param("x", list(value = LETTERS[1:3], negate = TRUE)),
        "EsgfQueryParam"
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
        query_build(index_node,
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

# query_esgf() {{{
test_that("query_esgf()", {
    expect_s3_class(EsgfQuery$new(), "EsgfQuery")
    expect_s3_class(query_esgf(), "EsgfQuery")

    skip_on_cran()

    index_node <- get_fast_index_node()
    expect_s3_class(q <- EsgfQuery$new(index_node), "EsgfQuery")
    expect_s3_class(q <- query_esgf(index_node), "EsgfQuery")
})
# }}}

# EsgfQuery$list_facets() {{{
test_that("EsgfQuery$list_facets()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    expect_s3_class(q <- query_esgf(index_node_normal), "EsgfQuery")
    expect_type(q$list_facets(), "character")

    # bridge node directly returns predefined facet names
    expect_s3_class(q <- query_esgf(index_node_bridge), "EsgfQuery")
    expect_type(q$list_facets(), "character")
    expect_equal(q$list_facets(), FIELDS_FACETS_COMMON)
})
# }}}

# EsgfQuery$list_fields() {{{
test_that("EsgfQuery$list_fields()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    expect_s3_class(q <- query_esgf(index_node_normal), "EsgfQuery")
    expect_type(q$list_fields(), "character")
    expect_s3_class(q <- query_esgf(index_node_bridge), "EsgfQuery")
    expect_type(q$list_fields(), "character")
})
# }}}

# EsgfQuery$list_shards() {{{
test_that("EsgfQuery$list_shards()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    expect_s3_class(q <- query_esgf(index_node_normal), "EsgfQuery")
    expect_type(q$list_shards(), "character")
    expect_s3_class(q <- query_esgf(index_node_bridge), "EsgfQuery")
    expect_type(q$list_shards(), "character")
})
# }}}

# EsgfQuery$list_values() {{{
test_that("EsgfQuery$list_values()", {
    skip_on_cran()
    index_node_normal <- INDEX_NODES[["CEDA"]]
    index_node_bridge <- INDEX_NODES[["ORNL"]]

    expect_s3_class(q <- query_esgf(index_node_normal), "EsgfQuery")
    expect_type(q$list_values("activity_id"), "integer")
    expect_s3_class(q <- query_esgf(index_node_bridge), "EsgfQuery")
    expect_type(q$list_values(c("activity_id", "experiment_id")), "list")
    expect_named(q$list_values(c("activity_id", "experiment_id")), c("activity_id", "experiment_id"))
})
# }}}

# EsgfQuery$project() and other facet methods {{{
test_that("EsgfQuery$project() and other facet methods", {
    q <- query_esgf()

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
    expect_equal(q$nominal_resolution(c("100 km", "1x1 degree"))$nominal_resolution()$value,
        {
            res <- c("100+km", "1x1+degree", "100km")
            attr(res, "encoded") <- TRUE
            res
        }
    )
    expect_null(q$nominal_resolution(NULL)$nominal_resolution())

    # data_node
    expect_null(q$data_node())
    expect_equal(q$data_node("esgf-node.ornl.gov")$data_node()$value, "esgf-node.ornl.gov")
    expect_null(q$data_node(NULL)$data_node())
})
# }}}

# EsgfQuery$facets() {{{
test_that("EsgfQuery$facets()", {
    expect_null(query_esgf()$facets())
    expect_equal(query_esgf()$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(query_esgf()$facets(NULL)$facets())
})
# }}}

# EsgfQuery$fields() {{{
test_that("EsgfQuery$fields()", {
    expect_equal(query_esgf()$fields()$value, "*")
    expect_equal(query_esgf()$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(query_esgf()$fields("*")$fields()$value, "*")
    expect_null(query_esgf()$fields(NULL)$fields())
})
# }}}

# EsgfQuery$shards() {{{
test_that("EsgfQuery$shards()", {
    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_null(q$shards())
    expect_false(q$distrib(FALSE)$distrib()$value)
    expect_error(q$shards("a"), "distrib")
    expect_true(q$distrib(TRUE)$distrib()$value)
    expect_s3_class(q$shards("a"), "EsgfQuery")
    expect_equal(
        q$shards("esgf-solr.ceda.ac.uk:8983/solr")$shards()$value,
        "esgf-solr.ceda.ac.uk:8983/solr"
    )
    expect_null(q$shards(NULL)$shards())
})
# }}}

# EsgfQuery$replica() {{{
test_that("EsgfQuery$replica()", {
    expect_null(query_esgf()$replica())
    expect_equal(query_esgf()$replica(TRUE)$replica()$value, TRUE)
    expect_equal(query_esgf()$replica(FALSE)$replica()$value, FALSE)
    expect_null(query_esgf()$replica(NULL)$replica())
})
# }}}

# EsgfQuery$latest() {{{
test_that("EsgfQuery$latest()", {
    expect_true(query_esgf()$latest()$value)
    expect_false(query_esgf()$latest(FALSE)$latest()$value)
    expect_true(query_esgf()$latest(TRUE)$latest()$value)
})
# }}}

# EsgfQuery$limit() {{{
test_that("EsgfQuery$limit()", {
    expect_equal(query_esgf()$limit()$value, 10L)
    expect_warning(lim <- query_esgf()$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(query_esgf()$limit(10L)$limit()$value, 10L)
})
# }}}

# EsgfQuery$offset() {{{
test_that("EsgfQuery$offset()", {
    expect_equal(query_esgf()$offset()$value, 0L)
    expect_equal(query_esgf()$offset(0)$offset()$value, 0L)
})
# }}}

# EsgfQuery$params() {{{
test_that("EsgfQuery$params()", {
    expect_s3_class(q <- query_esgf(), "EsgfQuery")

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
    expect_warning(q$params(type = "File"))

    # can restore original values in case of error
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_error(q$params(frequency = "1hr", frequency = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())
})
# }}}

# EsgfQuery$url(), EsgfQuery$count() {{{
test_that("EsgfQuery$url(), EsgfQuery$count()", {
    skip_on_cran()
    index_node <- INDEX_NODES[["CEDA"]]

    # can get url
    expect_type(EsgfQuery$new(index_node)$nominal_resolution("100 km")$url(), "character")
    expect_type(EsgfQuery$new(index_node)$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(EsgfQuery$new(index_node)$params(project = "CMIP5", table_id = "Amon")$url(), "character")

    # can get count
    expect_type(EsgfQuery$new(index_node)$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgfQuery$new(index_node)$frequency("1hr")$count(TRUE), "integer")
    expect_type(cnt <- EsgfQuery$new(index_node)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))
})
# }}}

# EsgfQuery$collect() {{{
test_that("EsgfQuery$collect()", {
    skip_on_cran()
    index_node <- get_fast_index_node()

    expect_s3_class(q <- query_esgf(index_node)$experiment_id("ssp585")$frequency("1hr")$fields("source_id"), "EsgfQuery")

    # can collect the specified limit number of records
    expect_s3_class(
        res <- q$limit(1)$collect(),
        "EsgfQueryResultDataset"
    )

    # can collect fields with constraints
    expect_true(all(c("project", "frequency", "source_id") %in% names(res)))

    # can collect required fields
    # NOTE: it is possible that some index nodes do not have
    # 'number_of_aggregations' field
    expect_true(
        all(setdiff(EsgfQueryResultDataset$private_fields$required_fields, "number_of_aggregations") %in% names(res))
    )

    # can collect all results with auto-pagination
    ## with maximum batch size
    expect_s3_class(res <- q$collect(all = TRUE, limit = FALSE), "EsgfQueryResultDataset")
    ## with specified limits
    expect_s3_class(res <- q$collect(all = TRUE, limit = 30), "EsgfQueryResultDataset")
})
# }}}

# EsgfQuery$save() & EsgfQuery$load() {{{
test_that("EsgfQuery$save() & EsgfQuery$load()", {
    skip_on_cran()
    index_node <- INDEX_NODES[["CEDA"]]

    q <- EsgfQuery$new(index_node)$
        activity_id("ScenarioMIP")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2)$
        params(table_id = c("Amon", "day"))

    # empty query object
    file_empty <- tempfile(fileext = ".json")
    expect_snapshot_file(q$save(file_empty), "query_empty.json")
    expect_s3_class(q_empty <- query_esgf()$load(file_empty), "EsgfQuery")
    expect_equal(priv(q_empty)$url_index_node, priv(q)$url_index_node)
    expect_equal(priv(q_empty)$parameter,      priv(q)$parameter)
    expect_equal(priv(q_empty)$response,       priv(q)$response)

    # query object with results
    q$collect()
    file_collected <- tempfile(fileext = ".json")
    expect_type(q$save(file_collected), "character")
    expect_true(file.exists(file_collected))
    file_collected_copied <- tempfile(fileext = ".json")
    expect_true(file.copy(file_collected, file_collected_copied))
    expect_snapshot_file(file_collected_copied, "query_collected.json", transform = transform_json)
    expect_s3_class(q_collected <- query_esgf()$load(file_collected), "EsgfQuery")
    expect_equal(priv(q_collected)$index_node, priv(q)$index_node)
    expect_equal(priv(q_collected)$parameter,  priv(q)$parameter)

    unlink(c(file_collected, file_collected_copied))
})
# }}}

# EsgfQuery$print() {{{
test_that("EsgfQuery$print()", {
    skip_on_cran()

    expect_snapshot(
        EsgfQuery$new("a")$params(table_id = "Amon", member_id = "r1i1p1f1")$print()
    )
})
# }}}

# vim: fdm=marker :
