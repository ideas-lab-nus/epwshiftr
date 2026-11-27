# query_param_test_complex_store() {{{
# query_param_test_complex_store {{{
query_param_test_complex_store <- function() {
    QueryParamStore$new()$activity_id(
        !c("CFMIP", "ScenarioMIP")
    )$datetime_range(start = "2017", stop = "2018")$timestamp_range(
        from = "NOW-1YEAR",
        to = "2021"
    )$version_range(min = "2020", max = "2021")$params(table_id = "Amon")
}
# }}}
# }}}
# query_param_test_expect_facet_method() {{{
# query_param_test_expect_facet_method {{{
query_param_test_expect_facet_method <- function(method, value = "CMIP6") {
    store <- QueryParamStore$new()
    expect_identical(store[[method]](value), store)
    expect_s7_class(store[[method]](), QueryParamFacet)
    expect_identical(store[[method]]()@value, value)

    vals <- c("CFMIP", "ScenarioMIP")
    negated <- QueryParamStore$new()
    expect_identical(negated[[method]](!vals), negated)
    expect_true(negated[[method]]()@negate)
    expect_identical(negated[[method]]()@value, vals)

    cleared <- QueryParamStore$new()
    expect_identical(cleared[[method]](NULL), cleared)
    expect_null(cleared[[method]]())
}
# }}}
# }}}
# QueryParamFacet() {{{
test_that("QueryParamFacet()", {
    facet <- QueryParamFacet(c("CMIP6", "CMIP5"))
    expect_s3_class(facet, "S7_object")
    expect_equal(
        S7::props(facet),
        list(value = c("CMIP6", "CMIP5"), negate = FALSE, encoded = FALSE)
    )
    expect_equal(render(facet, "project"), "project=CMIP6,CMIP5")
    expect_equal(
        render(facet, "project", space = TRUE),
        "project = CMIP6, CMIP5"
    )

    facet_negate <- QueryParamFacet(c("100km", "100 km"), negate = TRUE)
    expect_equal(
        S7::props(facet_negate),
        list(value = c("100km", "100 km"), negate = TRUE, encoded = FALSE)
    )
    expect_equal(
        render(facet_negate, "nominal_resolution", encode = TRUE),
        "nominal_resolution!=100km&nominal_resolution!=100%20km"
    )
    expect_equal(
        render(facet_negate, "nominal_resolution", encode = TRUE, space = TRUE),
        "nominal_resolution != 100km & nominal_resolution != 100%20km"
    )

    facet_encoded <- QueryParamFacet(c("100km", "100 km"), encoded = TRUE)
    expect_equal(
        S7::props(facet_encoded),
        list(value = c("100km", "100 km"), negate = FALSE, encoded = TRUE)
    )
    expect_equal(
        render(facet_encoded, "nominal_resolution", encode = TRUE),
        "nominal_resolution=100km,100 km"
    )

    facet_double <- QueryParamFacet(c(90, 180))
    expect_equal(
        S7::props(facet_double),
        list(value = c(90, 180), negate = FALSE, encoded = FALSE)
    )
})
# }}}
# QueryParamCtrl() {{{
test_that("QueryParamCtrl()", {
    ctrl <- QueryParamCtrl(TRUE)
    expect_s3_class(ctrl, "S7_object")
    expect_equal(
        S7::props(ctrl),
        list(value = TRUE)
    )
    expect_equal(render(ctrl, "latest"), "latest=true")
    expect_equal(render(QueryParamCtrl(0), "offset"), "offset=0")
    expect_equal(render(QueryParamCtrl(1L), "limit"), "limit=1")
    expect_equal(
        render(QueryParamCtrl(QUERY_PARAM__FORMAT_JSON), "format"),
        "format=application%2Fsolr%2Bjson"
    )
    expect_equal(
        render(
            QueryParamCtrl(QUERY_PARAM__FORMAT_JSON),
            "format",
            encode = FALSE
        ),
        "format=application/solr+json"
    )
    expect_equal(
        render(
            QueryParamCtrl(QUERY_PARAM__FORMAT_JSON),
            "format",
            encode = FALSE,
            space = TRUE
        ),
        "format = application/solr+json"
    )
})
# }}}
# QueryParamDate() {{{
test_that("QueryParamDate()", {
    date <- QueryParamDate(solr_date("[2017-02-03T05:06:07Z+2MONTHS TO *]"))
    expect_s3_class(date, "S7_object")
    expect_equal(
        S7::props(date),
        list(value = solr_date("[2017-02-03T05:06:07Z+2MONTHS TO *]"))
    )
    expect_equal(
        render(date, "datetime_start"),
        "datetime_start:[2017-02-03T05:06:07Z+2MONTHS TO *]"
    )
    expect_equal(
        render(date, "datetime_start", space = TRUE),
        "datetime_start: [2017-02-03T05:06:07Z+2MONTHS TO *]"
    )
    expect_equal(
        render(date, "datetime_start", quote_date = TRUE),
        'datetime_start:["2017-02-03T05:06:07Z+2MONTHS" TO *]'
    )
})
# }}}
# QueryParamStore$new() {{{
test_that("QueryParamStore$new()", {
    store <- QueryParamStore$new()

    expect_s3_class(store, "QueryParamStore")

    state_all <- store$state(null = TRUE)
    expect_named(state_all, names(QUERY_PARAM__DEF))
    expect_s3_class(state_all$project, "S7_object")
    expect_identical(state_all$project@value, "CMIP6")
    expect_s3_class(store$fields(), "S7_object")
    expect_identical(store$fields()@value, "*")
    expect_null(store$latest())
    expect_identical(store$offset()@value, 0L)
    expect_identical(store$distrib()@value, TRUE)
    expect_identical(store$format()@value, QUERY_PARAM__FORMAT_JSON)
    expect_null(state_all$datetime_start)
})
# }}}
# QueryParamStore facet setters {{{
test_that("QueryParamStore facet setters support values, negation, clearing, and resolution encoding", {
    facets <- c(
        project = "CMIP6",
        activity_id = "ScenarioMIP",
        experiment_id = "ssp585",
        source_id = "EC-Earth3",
        variable_id = "tas",
        frequency = "day",
        variant_label = "r1i1p1f1",
        data_node = "esgf.example.org"
    )
    for (method in names(facets)) {
        query_param_test_expect_facet_method(method, facets[[method]])
    }

    nr <- QueryParamStore$new()$nominal_resolution("100 km")
    expect_identical(nr$nominal_resolution()@value, c("100+km", "100km"))
    expect_true(nr$nominal_resolution()@encoded)
    expect_true(any(grepl("^nominal_resolution=100\\+km,100km$", nr$render())))
    expect_null(nr$nominal_resolution(NULL)$nominal_resolution())
})
# }}}
# QueryParamStore control setters {{{
test_that("QueryParamStore non-negated facet controls (facets, fields, shards)", {
    store <- QueryParamStore$new()
    expect_identical(store$facets("source_id"), store)
    expect_s7_class(store$facets(), QueryParamFacet)
    expect_identical(store$facets()@value, "source_id")
    expect_false(store$facets()@negate)

    expect_s7_class(store$fields(), QueryParamFacet)
    expect_identical(store$fields()@value, "*")
    expect_identical(store$fields("source_id"), store)
    expect_identical(store$fields()@value, "source_id")

    expect_identical(store$shards("node"), store)
    expect_s7_class(store$shards(), QueryParamFacet)
    expect_identical(store$shards()@value, "node")
    expect_false(store$shards()@negate)
    expect_error(
        QueryParamStore$new()$distrib(FALSE)$shards("node"),
        "distributed queries"
    )

    store$project(NULL)$fields(NULL)
    state_after_null <- store$state(null = TRUE)
    expect_null(state_after_null$project)
    expect_null(state_after_null$fields)
})

test_that("QueryParamStore scalar controls (replica, latest, type, limit, offset, distrib, format)", {
    store <- QueryParamStore$new()
    expect_identical(store$replica(TRUE), store)
    expect_s7_class(store$replica(), QueryParamCtrl)
    expect_true(store$replica()@value)
    expect_null(store$replica(NULL)$replica())

    expect_null(store$latest())
    expect_identical(store$latest(TRUE), store)
    expect_s7_class(store$latest(), QueryParamCtrl)
    expect_true(store$latest()@value)
    expect_null(store$latest(NULL)$latest())

    expect_identical(QueryParamStore$new()$type("File")$type()@value, "File")
    expect_identical(
        QueryParamStore$new()$type("Aggregation")$type()@value,
        "Aggregation"
    )

    expect_s7_class(store$limit(), QueryParamCtrl)
    expect_identical(store$limit()@value, 10L)
    expect_warning(capped <- store$limit(12000L), "maximum value")
    expect_identical(capped$limit()@value, 10000L)

    expect_s7_class(store$offset(), QueryParamCtrl)
    expect_identical(store$offset()@value, 0L)
    expect_identical(store$offset(5L), store)
    expect_identical(store$offset()@value, 5L)

    expect_s7_class(store$distrib(), QueryParamCtrl)
    expect_identical(store$distrib()@value, TRUE)
    expect_identical(store$distrib(FALSE), store)
    expect_identical(store$distrib()@value, FALSE)

    expect_s7_class(store$format(), QueryParamCtrl)
    expect_identical(store$format()@value, QUERY_PARAM__FORMAT_JSON)
    expect_error(QueryParamStore$new()$format("application/xml"), "Only JSON")
    expect_null(QueryParamStore$new()$format(NULL)$format())
})
# }}}
# QueryParamStore$params() {{{
test_that("QueryParamStore$params() handles ad hoc, raw REST, predefined, duplicate, and reserved inputs", {
    store <- QueryParamStore$new()$params(table_id = "Amon")
    expect_s3_class(store$state()$table_id, "S7_object")
    expect_identical(store$params()$table_id@value, "Amon")

    expect_warning(
        QueryParamStore$new()$params(
            id = "record-id",
            dataset_id = "dataset-id",
            master_id = "master-id",
            instance_id = "instance-id",
            checksum = "abc",
            checksum_type = "SHA256",
            tracking_id = "hdl:21.14100/mock",
            retracted = FALSE,
            number_of_files = 1L,
            number_of_aggregations = 1L,
            variable_units = "K",
            north_degrees = 90,
            east_degrees = 180,
            south_degrees = -90,
            west_degrees = -180,
            height_bottom = 100000,
            height_top = 1000,
            height_units = "Pa"
        ),
        NA
    )

    expect_warning(
        raw_store <- QueryParamStore$new()$params(
            bbox = "0,0,1,1",
            start = "2020",
            end = "2021",
            from = "2020",
            to = "2021"
        ),
        NA
    )
    expect_setequal(
        names(raw_store$params()),
        c("bbox", "start", "end", "from", "to")
    )

    multi <- QueryParamStore$new()$params(
        project = "CMIP6",
        activity_id = !c("CFMIP", "ScenarioMIP"),
        table_id = "Amon",
        realm = !c("atmos", "ocean")
    )
    expect_identical(multi$project()@value, "CMIP6")
    expect_true(multi$activity_id()@negate)
    expect_identical(multi$activity_id()@value, c("CFMIP", "ScenarioMIP"))
    expect_identical(multi$params()$table_id@value, "Amon")
    expect_true(multi$params()$realm@negate)
    expect_identical(multi$params()$realm@value, c("atmos", "ocean"))

    expect_identical(
        QueryParamStore$new()$params(type = "File")$type()@value,
        "File"
    )
    expect_error(
        QueryParamStore$new()$params(format = "application/xml"),
        "Only JSON"
    )

    expect_error(
        QueryParamStore$new()$params(table_id = "Amon", table_id = "Omon"),
        "unique"
    )
    cleared <- QueryParamStore$new()$params(table_id = "Amon", realm = "atmos")
    expect_named(cleared$params(), c("table_id", "realm"))
    expect_identical(cleared$params(NULL)$params(), list())
    expect_identical(QueryParamStore$new()$params(NULL)$params(), list())
    expect_error(
        QueryParamStore$new()$params(`_timestamp` = "NOW-1YEAR"),
        "reserved for query conditions"
    )
    expect_error(
        QueryParamStore$new()$params(limit = 1L),
        "reserved for control conditions"
    )
})
# }}}
# QueryParamStore range helpers {{{
test_that("QueryParamStore range helpers handle bounds, precedence, and syntax errors", {
    q <- QueryParamStore$new()$project("CMIP6")$datetime_range(
        start = "2017",
        stop = "2018"
    )
    dt <- q$datetime_range()
    expect_named(dt, c("start", "stop"))
    expect_s3_class(dt$start, "S7_object")
    expect_s3_class(dt$stop, "S7_object")
    expect_match(render(dt$start, "datetime_start"), "^datetime_start:")
    expect_match(render(dt$stop, "datetime_stop"), "^datetime_stop:")

    helper_first <- QueryParamStore$new()$datetime_range(start = "2020")
    expect_warning(
        helper_first$params(bbox = "0,0,1,1", start = "2019", end = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_setequal(names(helper_first$params()), "bbox")
    raw_first <- QueryParamStore$new()$params(start = "2019", end = "2021")
    expect_warning(
        raw_first$datetime_range(stop = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(raw_first$params(), 0L)

    q_ts <- QueryParamStore$new()$timestamp_range(
        from = "NOW-1YEAR",
        to = "2021"
    )
    ts <- q_ts$timestamp_range()
    expect_named(ts, c("from", "to"))
    expect_s3_class(ts$from, "S7_object")
    expect_s3_class(ts$to, "S7_object")
    expect_true(any(grepl("^_timestamp:", q_ts$render())))
    expect_error(
        QueryParamStore$new()$timestamp_range(from = "[2020 TO 2021]"),
        "does not support range syntax"
    )

    ts_helper <- QueryParamStore$new()$timestamp_range(from = "2020")
    expect_warning(
        ts_helper$params(from = "2019", to = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(ts_helper$params(), 0L)
    ts_raw <- QueryParamStore$new()$params(from = "2019", to = "2021")
    expect_warning(
        ts_raw$timestamp_range(to = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(ts_raw$params(), 0L)

    q_ver <- QueryParamStore$new()$version_range(min = "2020", max = "2021")
    expect_identical(
        unname(q_ver$render(name = "version")),
        c("version:[20200101 TO *]", "version:[* TO 20210101]")
    )
    expect_error(
        QueryParamStore$new()$version_range(min = "[2020 TO 2021]"),
        "must not contain range syntax"
    )
    expect_error(
        QueryParamStore$new()$version_range(min = "NOW-1YEAR"),
        "does not support Solr Date Math"
    )
})
# }}}
# QueryParamStore lifecycle methods {{{
test_that("QueryParamStore supports render, state, copy, serialize, restore, and print", {
    q <- query_param_test_complex_store()

    expect_equal(
        q$render(name = c("limit", "project")),
        c(limit = "limit=10", project = "project=CMIP6")
    )
    expect_true(any(grepl("^_timestamp:", q$render(name = "_timestamp"))))
    expect_true(any(grepl("^version:", q$render(name = "version"))))

    expect_named(
        q$state(
            name = c("activity_id", "version_max", "table_id", "limit"),
            null = TRUE
        ),
        c("activity_id", "version_max", "table_id", "limit")
    )
    expect_setequal(
        names(q$state())[query_param__field(names(q$state()))],
        c("project", "activity_id", "table_id")
    )

    copied <- q$copy()
    expect_s3_class(copied, "QueryParamStore")
    expect_identical(copied$state(), q$state())
    copied$params(table_id = "Omon")
    expect_identical(q$params()$table_id@value, "Amon")
    expect_identical(copied$params()$table_id@value, "Omon")

    serialized <- q$serialize()
    expect_true(serialized$activity_id$negate)
    expect_identical(serialized$table_id$value, "Amon")
    expect_true("datetime_start" %in% names(serialized))
    expect_identical(serialized$version_min$value, "[20200101 TO *]")
    expect_identical(serialized$version_max$value, "[* TO 20210101]")
    expect_named(
        q$serialize(name = c("activity_id", "version_max")),
        c("activity_id", "version_max")
    )
    expect_s3_class(q$serialize(null = TRUE, type = "json"), "json")

    restored <- QueryParamStore$new()$restore(serialized)
    expect_true(restored$activity_id()@negate)
    expect_identical(restored$params()$table_id@value, "Amon")
    expect_s3_class(restored$datetime_range()$start, "S7_object")
    expect_s3_class(restored$version_range()$max, "S7_object")
    expect_identical(restored$render(), q$render())
    expect_error(
        QueryParamStore$new()$restore(list(
            facet = list(project = serialized$project)
        )),
        "Bucketed"
    )
    expect_identical(
        QueryParamStore$new()$restore(q$serialize(null = TRUE))$state(),
        q$state(null = FALSE)
    )

    display_store <- QueryParamStore$new()$params(table_id = c("A mon", "B+C"))
    store_print <- paste(
        capture.output(display_store$print(), type = "message"),
        collapse = "\n"
    )
    expect_true(grepl(
        "format = application/solr+json",
        store_print,
        fixed = TRUE
    ))
    expect_true(grepl("table_id = A mon, B+C", store_print, fixed = TRUE))
    expect_false(grepl("application%2Fsolr%2Bjson|A%20mon|B%2BC", store_print))
})
# }}}
# query_param__* internal helpers {{{
test_that("query_param__* internal helpers normalize, classify, render, and display parameters", {
    param <- query_param__as("x", list(value = LETTERS[1:3], negate = TRUE))
    expect_s7_class(param, QueryParamFacet)
    expect_identical(query_param__value(param), LETTERS[1:3])
    expect_true(query_param__negate(param))

    date_param <- query_param__as("datetime_start", "2017")
    expect_s7_class(date_param, QueryParamDate)
    expect_true(is.solr_date(query_param__value(date_param)))

    expect_true(query_param__field("project"))
    expect_false(any(query_param__field(c(
        "fields",
        "bbox",
        "datetime_start",
        "datetime_stop",
        "timestamp_from",
        "timestamp_to",
        "version_min",
        "version_max",
        "type",
        "format",
        "facets",
        "shards",
        "start",
        "end",
        "from",
        "to"
    ))))
    expect_true(all(QUERY_PARAM__REST_KEYS %in% query_param__names("all")))
    expect_identical(
        query_param__names("date"),
        c(
            "datetime_start",
            "datetime_stop",
            "timestamp_from",
            "timestamp_to",
            "version_min",
            "version_max"
        )
    )

    flat <- query_param__as_store(list(
        datetime_start = solr_date("2017"),
        project = "CMIP6",
        variable_id = "tas"
    ))$state()
    expect_s7_class(flat$datetime_start, QueryParamDate)
    expect_s7_class(flat$project, QueryParamFacet)
    expect_s7_class(flat$variable_id, QueryParamFacet)

    expect_render <- function(val, want, nm = "x", ...) {
        expect_identical(
            query_param__render(query_param__as(nm, val), nm, ...),
            want
        )
    }
    expect_render("B+C", "table_id=B+C", nm = "table_id", encode = FALSE)
    expect_render(list(value = TRUE, negate = TRUE), "x=false")
    expect_render(list(value = 1.0, negate = TRUE), "x!=1")
    expect_render(list(value = "solr+json", negate = TRUE), "x!=solr%2Bjson")
    expect_render(TRUE, "x = true", space = TRUE)
    expect_render(1.0, "x = 1", space = TRUE)
    expect_render("solr+json", "x = solr%2Bjson", space = TRUE)

    display_store <- QueryParamStore$new()$params(table_id = c("A mon", "B+C"))
    expect_equal(
        query_param__display(display_store)[c("format", "table_id")],
        c(
            format = "{.strong format =} application/solr+json",
            table_id = "{.strong table_id =} A mon, B+C"
        )
    )
    helper_print <- paste(
        capture.output(query_param__print(display_store), type = "message"),
        collapse = "\n"
    )
    expect_true(grepl(
        "format = application/solr+json",
        helper_print,
        fixed = TRUE
    ))
    expect_true(grepl("table_id = A mon, B+C", helper_print, fixed = TRUE))
})
# }}}

# vim: fdm=marker :
