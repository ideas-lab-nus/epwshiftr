host <- "https://esgf.ceda.ac.uk/esg-search"

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
    expect_s3_class(EsgfQuery$new(listing = FALSE), "EsgfQuery")
    expect_s3_class(query_esgf(listing = FALSE), "EsgfQuery")

    skip_on_cran()

    # # NOTE: attached facet listing cache is used throughout tests
    # attach_facet_cache()
    expect_s3_class(q <- EsgfQuery$new(host, TRUE), "EsgfQuery")
    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")
})

test_that("EsgfQuery$list_all_facets()", {
    expect_null(query_esgf(listing = FALSE)$list_all_facets())

    skip_on_cran()

    options("epwshiftr.verbose" = TRUE)
    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")
    expect_type(q$list_all_facets(), "character")
})

test_that("EsgfQuery$list_all_fields()", {
    expect_null(query_esgf(listing = FALSE)$list_all_fields())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")
    expect_type(q$list_all_fields(), "character")
})

test_that("EsgfQuery$list_all_shards()", {
    expect_null(query_esgf(listing = FALSE)$list_all_shards())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")
    expect_type(q$list_all_shards(), "character")
})

test_that("EsgfQuery$list_all_values()", {
    expect_null(query_esgf(listing = FALSE)$list_all_values("activity_id"))

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")
    expect_type(q$list_all_values("activity_id"), "character")
})

test_that("EsgfQuery$project() and other facet methods", {
    q <- query_esgf(listing = FALSE)

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

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

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
    expect_equal(q$data_node("esgf.ceda.ac.uk")$data_node()$value, "esgf.ceda.ac.uk")
    expect_null(q$data_node(NULL)$data_node())
})

test_that("EsgfQuery$facets()", {
    expect_null(query_esgf(listing = FALSE)$facets())
    expect_equal(query_esgf(listing = FALSE)$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(query_esgf(listing = FALSE)$facets(NULL)$facets())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_null(q$facets())
    expect_equal(q$facets(c("activity_id", "source_id"))$facets()$value, c("activity_id", "source_id"))
    expect_null(q$facets(NULL)$facets())
})

test_that("EsgfQuery$fields()", {
    expect_equal(query_esgf(listing = FALSE)$fields()$value, "*")
    expect_equal(query_esgf(listing = FALSE)$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(query_esgf(listing = FALSE)$fields("*")$fields()$value, "*")
    expect_null(query_esgf(listing = FALSE)$fields(NULL)$fields())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_equal(q$fields()$value, "*")
    expect_equal(q$fields(c("activity_id", "source_id"))$fields()$value, c("activity_id", "source_id"))
    expect_equal(q$fields("*")$fields()$value, "*")
    expect_null(q$fields(NULL)$fields())
})

test_that("EsgfQuery$shards()", {
    expect_s3_class(q <- query_esgf(listing = FALSE), "EsgfQuery")

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

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_null(q$shards())
    expect_false(q$distrib(FALSE)$distrib()$value)
    expect_error(q$shards("a"), "distrib")
    expect_true(q$distrib(TRUE)$distrib()$value)
    expect_error(q$shards("a"), "Assertion")
    expect_equal(
        q$shards("esgf-solr.ceda.ac.uk:8983/solr")$shards()$value,
        "esgf-solr.ceda.ac.uk:8983/solr"
    )
    expect_null(q$shards(NULL)$shards())
})

test_that("EsgfQuery$replica()", {
    expect_null(query_esgf(listing = FALSE)$replica())
    expect_equal(query_esgf(listing = FALSE)$replica(TRUE)$replica()$value, TRUE)
    expect_equal(query_esgf(listing = FALSE)$replica(FALSE)$replica()$value, FALSE)
    expect_null(query_esgf(listing = FALSE)$replica(NULL)$replica())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_null(q$replica())
    expect_equal(q$replica(TRUE)$replica()$value, TRUE)
    expect_equal(q$replica(FALSE)$replica()$value, FALSE)
    expect_null(q$replica(NULL)$replica())
})

test_that("EsgfQuery$latest()", {
    expect_true(query_esgf(listing = FALSE)$latest()$value)
    expect_false(query_esgf(listing = FALSE)$latest(FALSE)$latest()$value)
    expect_true(query_esgf(listing = FALSE)$latest(TRUE)$latest()$value)

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_true(q$latest()$value)
    expect_false(q$latest(FALSE)$latest()$value)
    expect_true(q$latest(TRUE)$latest()$value)
})

test_that("EsgfQuery$limit()", {
    expect_equal(query_esgf(listing = FALSE)$limit()$value, 10L)
    expect_warning(lim <- query_esgf(listing = FALSE)$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(query_esgf(listing = FALSE)$limit(10L)$limit()$value, 10L)

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_equal(q$limit()$value, 10L)
    expect_warning(lim <- q$limit(12000)$limit(), "10,000")
    expect_equal(lim$value, 10000L)
    expect_equal(q$limit(10L)$limit()$value, 10L)
})

test_that("EsgfQuery$offset()", {
    expect_equal(query_esgf(listing = FALSE)$offset()$value, 0L)
    expect_equal(query_esgf(listing = FALSE)$offset(0)$offset()$value, 0L)

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_equal(q$offset()$value, 0L)
    expect_equal(q$offset(0)$offset()$value, 0L)
})

test_that("EsgfQuery$params()", {
    expect_s3_class(q <- query_esgf(listing = FALSE), "EsgfQuery")

    # can use existing method for common parameters
    expect_equal(q$params(), list())
    expect_equal(q$params(nominal_resolution = !c("10 km", "25 km"))$params(), list())
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

    # can reset type
    expect_warning(q$params(type = "File"))

    # can restore original values in case of error
    expect_equal(q$frequency("day")$frequency()$value, "day")
    expect_error(q$params(frequency = "1hr", frequency = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    # can use existing method for common parameters
    expect_equal(q$params(), list())
    expect_error(q$params(nominal_resolution = !c("a", "b")), "Assertion")
    expect_error(q$params(table_id = "1"), "Assertion")
    expect_error(q$params(table_id = "day", table_id = "hour"), "unique names")
    expect_equal(q$params(), list())

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
    expect_error(q$params(frequency = "1hr", source_id = "a"), "Assertion")
    expect_equal(q$frequency()$value, "day")

    # can remove all paramters
    expect_equal(q$params(NULL)$params(), list())
})

test_that("EsgfQuery$url()", {
    expect_type(EsgfQuery$new(listing = FALSE)$nominal_resolution("100 km")$url(), "character")
    expect_type(EsgfQuery$new(listing = FALSE)$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(EsgfQuery$new(listing = FALSE)$params(project = "CMIP5", table_id = "Amon")$url(), "character")

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_type(q$nominal_resolution("100 km")$url(), "character")
    expect_type(q$nominal_resolution("100 km")$url(TRUE), "character")
    expect_type(q$params(project = "CMIP5", table_id = "Amon")$url(), "character")
})

test_that("EsgfQuery$count()", {
    expect_type(EsgfQuery$new(host, FALSE)$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgfQuery$new(host, FALSE)$frequency("1hr")$count(TRUE), "integer")
    expect_type(cnt <- EsgfQuery$new(host, FALSE)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))

    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE), "EsgfQuery")

    expect_type(EsgfQuery$new(host)$frequency("1hr")$count(FALSE), "integer")
    expect_type(EsgfQuery$new(host)$frequency("1hr")$count(TRUE), "integer")
    expect_type(cnt <- EsgfQuery$new(host)$frequency("1hr")$count("activity_id"), "list")
    expect_equal(names(cnt), c("total", "activity_id"))
})

test_that("EsgfQuery$collect()", {
    skip_on_cran()

    expect_s3_class(q <- query_esgf(host, TRUE)$experiment_id("ssp585")$frequency("1hr")$fields("source_id"), "EsgfQuery")

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

test_that("EsgfQuery$save() & EsgfQuery$load()", {
    q <- EsgfQuery$new(host = host, listing = FALSE)$
        activity_id("ScenarioMIP")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2)$
        params(table_id = c("Amon", "day"))

    # empty query object
    fe <- tempfile(fileext = ".json")
    expect_snapshot_file(q$save(fe), "query_empty.json")
    expect_s3_class(qe <- query_esgf()$load(fe), "EsgfQuery")
    expect_equal(priv(qe)$url_host,      priv(q)$url_host)
    expect_equal(priv(qe)$parameter,     priv(q)$parameter)
    expect_equal(priv(qe)$facet_listing, priv(q)$facet_listing)
    expect_equal(priv(qe)$last_result,   priv(q)$last_result)

    # query object with results
    q$collect()
    fc <- tempfile(fileext = ".json")
    # reset timestamp before snapshoting so that it keeps the same between tests
    q$.__enclos_env__$private$last_result$response$timestamp <- as.POSIXct(
        "2020-02-02 22:22:22.123456", "UTC"
    )
    expect_snapshot_file(q$save(fc), "query_collected.json")
    expect_s3_class(qc <- query_esgf()$load(fc), "EsgfQuery")
    expect_equal(priv(qc)$url_host,      priv(q)$url_host)
    expect_equal(priv(qc)$parameter,     priv(q)$parameter)
    expect_equal(priv(qc)$facet_listing, priv(q)$facet_listing)
    expect_equal(priv(qc)$last_result,   priv(q)$last_result)

    # query with facet listing
    q <- query_esgf(host = host, listing = TRUE)
    fl <- tempfile(fileext = ".json")
    # reset timestamp before snapshoting so that it keeps the same between tests
    q$.__enclos_env__$private$facet_listing$timestamp <- as.POSIXct(
        "2020-02-02 22:22:22.123456", "UTC"
    )
    expect_snapshot_file(q$save(fl), "query_collected.json")
    expect_s3_class(ql <- query_esgf()$load(fl), "EsgfQuery")
    expect_equal(priv(ql)$url_host,      priv(q)$url_host)
    expect_equal(priv(ql)$parameter,     priv(q)$parameter)
    expect_equal(priv(ql)$facet_listing, priv(q)$facet_listing)
    expect_equal(priv(ql)$last_result,   priv(q)$last_result)
})

test_that("EsgfQuery$print()", {
    skip_on_cran()

    expect_snapshot(
        EsgfQuery$new("a", listing = FALSE)$params(table_id = "Amon", member_id = "r1i1p1f1")$print()
    )

    expect_snapshot(
        EsgfQuery$new(host, listing = FALSE)$params(table_id = "Amon", member_id = "r1i1p1f1")$print(),
        transform = function(out) {
            out[grepl("Facet listing built at: \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", out)] <- "* Facet listing built at: yyyy-mm-dd HH:MM:SS"
            out
        }
    )
})
