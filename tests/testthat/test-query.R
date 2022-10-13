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

test_that("query_esgf()", {
    skip_on_cran()

    attach_facet_cache()
    expect_s3_class(q <- EsgfQuery$new(), "EsgfQuery")
    expect_s3_class(q <- query_esgf(), "EsgfQuery")
})

test_that("EsgfQuery$list_all_facets()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")
    expect_type(q$list_all_facets(), "character")
})

test_that("EsgfQuery$list_all_shards()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")
    expect_type(q$list_all_shards(), "character")
})

test_that("EsgfQuery$list_all_values()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")
    expect_type(q$list_all_values("activity_id"), "character")
})

test_that("EsgfQuery$project() and other facet methods", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

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
    expect_equal(q$data_node("esg.lasg.ac.cn")$data_node()$value, "esg.lasg.ac.cn")
    expect_null(q$data_node(NULL)$data_node())
})

test_that("EsgfQuery$facets()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_null(q$facets())
    expect_equal(q$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(q$facets(NULL)$facets())
})

test_that("EsgfQuery$fields()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_equal(q$fields()$value, "*")
    expect_equal(q$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(q$fields("*")$fields()$value, "*")
    expect_null(q$fields(NULL)$fields())
})

test_that("EsgfQuery$shards()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_null(q$shards())
    expect_false(q$distrib(FALSE)$distrib()$value)
    expect_error(q$shards("a"), "distrib")
    expect_true(q$distrib(TRUE)$distrib()$value)
    expect_error(q$shards("a"), "Assertion")
    expect_equal(
        q$shards("esgf-node.llnl.gov:8985/solr")$shards()$value,
        "esgf-node.llnl.gov:8985/solr"
    )
    expect_null(q$shards(NULL)$shards())
})

test_that("EsgfQuery$replica()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_null(q$replica())
    expect_equal(q$replica(TRUE)$replica()$value, TRUE)
    expect_equal(q$replica(FALSE)$replica()$value, FALSE)
    expect_null(q$replica(NULL)$replica())
})

test_that("EsgfQuery$latest()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_true(q$latest()$value)
    expect_false(q$latest(FALSE)$latest()$value)
    expect_true(q$latest(TRUE)$latest()$value)
})

test_that("EsgfQuery$limit()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_equal(q$limit()$value, 10L)
    expect_warning(lim <- q$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(q$limit(10L)$limit()$value, 10L)
})

test_that("EsgfQuery$offset()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_equal(q$offset()$value, 0L)
    expect_equal(q$offset(0)$offset()$value, 0L)
})

test_that("EsgfQuery$params()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    # can use existing method for common parameters
    expect_equal(q$params(), list())
    expect_error(q$params(nominal_resolution = !c("a", "b")), "Assertion")
    expect_equal(q$params(nominal_resolution = !c("10 km", "25 km"))$params(), list())
    expect_error(q$params(table_id = "1"), "Assertion")
    expect_error(q$params(table_id = "day", table_id = "hour"), "unique names")

    # can reset existing parameters
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_equal(q$params(frequency = NULL)$params(), list())
    expect_null(q$frequency())
    expect_equal(
        q$params(table_id = "Amon", member_id = "00")$params(),
        list(
            table_id = new_query_param("table_id", "Amon"),
            member_id = new_query_param("member_id", "00")
        )
    )

    # can reset format
    expect_warning(q$params(format = "xml"), "JSON")

    # can restore original values in case of error
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_error(q$params(frequency = "1hr", source_id = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())
})

test_that("EsgfQuery$url()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_type(EsgfQuery$new()$nominal_resolution("100 km")$url(), "character")
    expect_type(EsgfQuery$new()$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(EsgfQuery$new()$params(project = "CMIP5", table_id = "Amon")$url(), "character")
})

test_that("EsgfQuery$count()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(), "EsgfQuery")

    expect_type(EsgfQuery$new()$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgfQuery$new()$frequency("1hr")$count(TRUE), "integer")
    expect_type(cnt <- EsgfQuery$new()$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))
})

test_that("EsgfQuery$collect()", {
    skip_on_cran()

    attach_facet_cache()
    expect_s3_class(q <- query_esgf()$experiment_id("ssp585")$frequency("1hr")$fields("source_id"), "EsgfQuery")

    # can collect the specified limit number of records
    expect_s3_class(
        res <- q$limit(1)$collect(),
        "EsgfQueryResultDataset"
    )

    # can collect fields with constraints
    expect_true(all(c("project", "frequency", "source_id") %in% names(res)))

    # can collect required fields
    expect_true(
        all(EsgfQueryResultDataset$private_fields$required_fields %in% names(res))
    )

    # can collect all results with auto-pagination
    ## with maximum batch size
    expect_s3_class(res <- q$collect(all = TRUE, limit = FALSE), "EsgfQueryResultDataset")
    expect_equal(q$response()$response$numFound, res$count())
    ## with specified limits
    expect_s3_class(res <- q$collect(all = TRUE, limit = 30), "EsgfQueryResultDataset")
    expect_equal(q$response()$response$numFound, res$count())
})

test_that("EsgfQuery$response()", {
    skip_on_cran()

    expect_null(EsgfQuery$new()$response())

    expect_s3_class(q <- EsgfQuery$new(), "EsgfQuery")
    expect_type(q$limit(0)$frequency("1hr")$count(), "integer")
    expect_type(q$response(), "list")
    expect_equal(names(q$response()), c("responseHeader", "response"))
})

test_that("EsgfQuery$print()", {
    skip_on_cran()
    expect_snapshot(EsgfQuery$new()$params(table_id = "Amon", member_id = "r1i1p1f1")$print())
})
