# this host that currently supports facet listing
host <- "https://esgf.ceda.ac.uk/esg-search"

test_that("ESGF Query Parameter works", {
    # can create new query parameter
    expect_s3_class(
        param <- new_query_param("x", list(value = LETTERS[1:3], negate = TRUE)),
        "EsgfQueryParam"
    )

    expect_snapshot_output(format(new_query_param("x", list(value = TRUE, negate = TRUE))))
    expect_snapshot_output(format(new_query_param("x", list(value = 1.0, negate = TRUE))))
    # can encode parameter
    expect_snapshot_output(format(new_query_param("x", list(value = "solr+json", negate = TRUE))))

    # can add spaces
    expect_snapshot_output(format(new_query_param("x", TRUE), space = TRUE))
    expect_snapshot_output(format(new_query_param("x", 1.0), space = TRUE))
    expect_snapshot_output(format(new_query_param("x", "solr+json"), space = TRUE))

    # can print query parameter
    expect_snapshot_output(print(new_query_param("x", list(value = TRUE, negate = TRUE))))
    expect_snapshot_output(print(new_query_param("x", list(value = 1.0, negate = TRUE))))
    expect_snapshot_output(print(new_query_param("x", list(value = "solr+json", negate = TRUE))))

    # can build query url
    host <- "https://esgf-node.llnl.gov/esg-search"
    expect_null(query_build(host, list(project = NULL)))
    expect_true(grepl("CMIP5", query_build(host, list(project = "CMIP6", others = list(project = "CMIP5")))))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(host, list(project = "CMIP6", others = list(project = "CMIP5", table_id = "Amon")))
    ))
    expect_true(grepl(
        "project=CMIP5&table_id=Amon",
        query_build(host,
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

test_that("ESGF Query works", {
    skip_on_cran()

    host <- "https://esgf.ceda.ac.uk/esg-search"
    expect_s3_class(q <- EsgfQuery$new(host), "EsgfQuery")
    expect_s3_class(q <- query_esgf(host), "EsgfQuery")

    # listing
    expect_type(q$list_all_facets(), "character")
    expect_type(q$list_all_shards(), "character")
    expect_type(q$list_all_values("activity_id"), "character")

    # project
    expect_equal(q$project()$value, "CMIP6")
    expect_equal(q$project("CMIP5")$project()$value, "CMIP5")

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
    dn <- q$list_all_values("data_node")
    expect_equal(q$data_node(dn)$data_node()$value, dn)
    expect_null(q$data_node(NULL)$data_node())

    # facets
    expect_null(q$facets())
    expect_equal(q$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(q$facets(NULL)$facets())

    # fields
    expect_equal(q$fields()$value, "*")
    expect_equal(q$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(q$fields("*")$fields()$value, "*")
    expect_null(q$fields(NULL)$fields())

    # shards
    expect_null(q$shards())
    expect_false(q$distrib(FALSE)$distrib()$value)
    expect_error(q$shards("a"), "distrib")
    expect_true(q$distrib(TRUE)$distrib()$value)
    expect_error(q$shards("a"), "Assertion")
    shard <- gsub("(?<=/solr).+", "", q$list_all_shards()[1L], perl = TRUE)
    expect_equal(q$shards(shard)$shards()$value, shard)
    expect_null(q$shards(NULL)$shards())

    # replica
    expect_null(q$replica())
    expect_equal(q$replica(TRUE)$replica()$value, TRUE)
    expect_equal(q$replica(FALSE)$replica()$value, FALSE)
    expect_null(q$replica(NULL)$replica())

    # latest
    expect_true(q$latest()$value)
    expect_false(q$latest(FALSE)$latest()$value)
    expect_true(q$latest(TRUE)$latest()$value)

    # type
    expect_equal(q$type()$value, "Dataset")
    expect_equal(q$type("Aggregation")$type()$value, "Aggregation")
    expect_equal(q$type("Dataset")$type()$value, "Dataset")

    # limit
    expect_equal(q$limit()$value, 10L)
    expect_warning(lim <- q$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(q$limit(10L)$limit()$value, 10L)

    # offset
    expect_equal(q$offset()$value, 0L)
    expect_equal(q$offset(0)$offset()$value, 0L)

    # params
    ## can use existing method for common parameters
    expect_equal(q$params(), list())
    expect_error(q$params(nominal_resolution = !c("a", "b")), "Assertion")
    expect_equal(q$params(nominal_resolution = !c("10 km", "25 km"))$params(), list())
    expect_error(q$params(table_id = "1"), "Assertion")
    expect_error(q$params(table_id = "day", table_id = "hour"), "unique names")
    ## can reset existing parameters
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
    ## can reset format
    expect_warning(q$params(format = "xml"), "JSON")
    # can restore original values in case of error
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_error(q$params(frequency = "1hr", source_id = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")
    ## can remove all paramters
    expect_equal(q$params(NULL)$params(), list())

    # can get url
    expect_type(EsgfQuery$new(host)$nominal_resolution("100 km")$url(), "character")
    expect_type(EsgfQuery$new(host)$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(EsgfQuery$new(host)$params(project = "CMIP5", table_id = "Amon")$url(), "character")

    # can get count
    expect_type(EsgfQuery$new(host)$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgfQuery$new(host)$frequency("1hr")$count(TRUE), "integer")
    expect_type(cnt <- EsgfQuery$new(host)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))

    # can collect data
    expect_equal(EsgfQuery$new(host)$limit(0)$frequency("1hr")$collect(), data.table::setDT(list()))
    expect_s3_class(res <- EsgfQuery$new(host)$limit(1)$fields("source_id")$collect(), "data.table")
    expect_equal(names(res), "source_id")

    # can return last response
    expect_null(EsgfQuery$new(host)$response())
    expect_s3_class(q <- EsgfQuery$new(host), "EsgfQuery")
    expect_type(q$limit(0)$frequency("1hr")$count(), "integer")
    expect_type(q$response(), "list")
    expect_equal(names(q$response()), c("responseHeader", "response", "facet_counts"))

    expect_snapshot_output(EsgfQuery$new(host)$params(table_id = "Amon", member_id = "r1i1p1f1")$print())
})
