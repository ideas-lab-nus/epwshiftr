test_that("QueryParamFacet", {
    facet <- QueryParamFacet(c("CMIP6", "CMIP5"))
    expect_s3_class(facet, "S7_object")
    expect_equal(
        S7::props(facet),
        list(value = c("CMIP6", "CMIP5"), negate = FALSE, encoded = FALSE)
    )
    expect_equal(render(facet, "project"), "project=CMIP6,CMIP5")

    facet_negate <- QueryParamFacet(c("100km", "100 km"), negate = TRUE)
    expect_equal(
        S7::props(facet_negate),
        list(value = c("100km", "100 km"), negate = TRUE, encoded = FALSE)
    )
    expect_equal(
        render(facet_negate, "nominal_resolution", encode = TRUE),
        "nominal_resolution!=100km&nominal_resolution!=100%20km"
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
})

test_that("QueryParamCtrl", {
    ctrl <- QueryParamCtrl(TRUE)
    expect_s3_class(ctrl, "S7_object")
    expect_equal(
        S7::props(ctrl),
        list(value = TRUE)
    )
    expect_equal(render(ctrl, "latest"), "latest=true")
    expect_equal(render(QueryParamCtrl(1L), "limit"), "limit=1")
    expect_equal(
        render(QueryParamCtrl(QUERY_PARAM__FORMAT_JSON), "format"),
        "format=application%2Fsolr%2Bjson"
    )
})

test_that("QueryParamDate", {
    date <- QueryParamDate(solr_date("[2017-02-03T05:06:07Z+2MONTHS TO *]"))
    expect_s3_class(date, "S7_object")
    expect_equal(
        S7::props(date),
        list(value = solr_date("[2017-02-03T05:06:07Z+2MONTHS TO *]"))
    )
    expect_equal(render(date, "datetime_start"), "datetime_start:[2017-02-03T05:06:07Z+2MONTHS TO *]")
    expect_equal(
        render(date, "datetime_start", quote_date = TRUE),
        'datetime_start:["2017-02-03T05:06:07Z+2MONTHS" TO *]'
    )
})

test_that("QueryParamStore", {
    store <- QueryParamStore$new()

    expect_s3_class(store, "QueryParamStore")
    expect_true(query_param__field("project"))
    expect_false(query_param__field("fields"))
    expect_false(query_param__field("bbox"))
    expect_true(all(QUERY_PARAM__REST_KEYS %in% query_param__names("all")))

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
    expect_error(QueryParamStore$new()$format("application/xml"), "Only JSON")
    expect_null(QueryParamStore$new()$format(NULL)$format())
    expect_identical(QueryParamStore$new()$type("File")$type()@value, "File")
    expect_identical(QueryParamStore$new()$type("Aggregation")$type()@value, "Aggregation")
    expect_error(QueryParamStore$new()$params(format = "application/xml"), "Only JSON")
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
    expect_setequal(names(raw_store$params()), c("bbox", "start", "end", "from", "to"))
    expect_true(all(c("bbox", "start", "end", "from", "to") %in% raw_store$param_names(role = "keyword")))

    helper_first <- QueryParamStore$new()$datetime_range(start = "2020")
    expect_warning(
        helper_first$params(bbox = "0,0,1,1", start = "2019", end = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_setequal(names(helper_first$params()), "bbox")
    expect_s3_class(helper_first$datetime_range()$start, "S7_object")

    raw_first <- QueryParamStore$new()$params(start = "2019", end = "2021")
    expect_warning(
        raw_first$datetime_range(stop = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(raw_first$params(), 0L)

    timestamp_helper_first <- QueryParamStore$new()$timestamp_range(from = "2020")
    expect_warning(
        timestamp_helper_first$params(from = "2019", to = "2021"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(timestamp_helper_first$params(), 0L)

    timestamp_raw_first <- QueryParamStore$new()$params(from = "2019", to = "2021")
    expect_warning(
        timestamp_raw_first$timestamp_range(to = "2020"),
        "structured helper .* takes precedence over raw REST keyword"
    )
    expect_length(timestamp_raw_first$params(), 0L)
    expect_identical(QueryParamStore$new()$params(type = "File")$type()@value, "File")
    expect_null(state_all$datetime_start)

    store$project(NULL)$fields(NULL)
    state_after_null <- store$state(null = TRUE)
    expect_null(state_after_null$project)
    expect_null(state_after_null$fields)

    store$project("CMIP6")$fields("*")
    store$params(table_id = "Amon")
    state <- store$state()
    expect_s3_class(state$table_id, "S7_object")
    expect_identical(state$table_id@value, "Amon")
    expect_identical(store$params()$table_id@value, "Amon")

    vals <- c("CFMIP", "ScenarioMIP")
    negated <- QueryParamStore$new()$activity_id(!vals)
    expect_true(negated$activity_id()@negate)
    expect_identical(negated$activity_id()@value, vals)

    q <- QueryParamStore$new()$project("CMIP6")$datetime_range(start = "2017", stop = "2018")
    expect_s3_class(q$project(), "S7_object")
    expect_identical(q$project()@value, "CMIP6")

    dt <- q$datetime_range()
    expect_named(dt, c("start", "stop"))
    expect_s3_class(dt$start, "S7_object")
    expect_s3_class(dt$stop, "S7_object")
    expect_match(render(dt$start, "datetime_start"), "^datetime_start:")
    expect_match(render(dt$stop, "datetime_stop"), "^datetime_stop:")

    q$timestamp_range(from = "NOW-1YEAR", to = "2021")
    ts <- q$timestamp_range()
    expect_named(ts, c("from", "to"))
    expect_s3_class(ts$from, "S7_object")
    expect_s3_class(ts$to, "S7_object")
    expect_true(any(grepl("^_timestamp:", q$render())))

    q$version_range(min = "2020", max = "2021")
    expect_true(any(grepl("^version:", q$render())))

    nr <- QueryParamStore$new()$nominal_resolution("100 km")
    expect_identical(nr$nominal_resolution()@value, c("100+km", "100km"))
    expect_true(nr$nominal_resolution()@encoded)
    expect_true(any(grepl("^nominal_resolution=100\\+km,100km$", nr$render())))
    expect_null(nr$nominal_resolution(NULL)$nominal_resolution())

    expect_error(
        QueryParamStore$new()$timestamp_range(from = "[2020 TO 2021]"),
        "does not support range syntax"
    )
    expect_error(
        QueryParamStore$new()$version_range(min = "[2020 TO 2021]"),
        "must not contain range syntax"
    )
    expect_error(
        QueryParamStore$new()$version_range(min = "NOW-1YEAR"),
        "does not support Solr Date Math"
    )
    expect_error(
        QueryParamStore$new()$params(`_timestamp` = "NOW-1YEAR"),
        "reserved for query conditions"
    )
    expect_error(
        QueryParamStore$new()$params(limit = 1L),
        "reserved for control conditions"
    )

    multi_params <- QueryParamStore$new()$params(
        project = "CMIP6",
        activity_id = !c("CFMIP", "ScenarioMIP"),
        table_id = "Amon",
        realm = !c("atmos", "ocean")
    )
    expect_identical(multi_params$project()@value, "CMIP6")
    expect_true(multi_params$activity_id()@negate)
    expect_identical(multi_params$activity_id()@value, c("CFMIP", "ScenarioMIP"))
    expect_identical(multi_params$params()$table_id@value, "Amon")
    expect_true(multi_params$params()$realm@negate)
    expect_identical(multi_params$params()$realm@value, c("atmos", "ocean"))

    repeated_vals <- c("Amon", "Omon")
    expect_error(
        QueryParamStore$new()$params(table_id = repeated_vals[[1]], table_id = repeated_vals[[2]]),
        "unique"
    )

    cleared_params <- QueryParamStore$new()$params(table_id = "Amon", realm = "atmos")
    expect_named(cleared_params$params(), c("table_id", "realm"))
    expect_identical(cleared_params$params(NULL)$params(), list())

    expect_warning(capped <- QueryParamStore$new()$limit(12000L), "maximum value")
    expect_identical(capped$limit()@value, 10000L)

    expect_identical(QueryParamStore$new()$params(NULL)$params(), list())

    q <- QueryParamStore$new()$activity_id(!c("CFMIP", "ScenarioMIP"))$datetime_range(
        start = "2017",
        stop = "2018"
    )$timestamp_range(from = "NOW-1YEAR", to = "2021")$version_range(min = "2020", max = "2021")$params(
        table_id = "Amon"
    )

    serialized <- q$serialize()
    restored <- QueryParamStore$new()$restore(serialized)

    subset_state <- q$state(name = c("activity_id", "version_max", "table_id", "limit"), null = TRUE)
    expect_named(subset_state, c("activity_id", "version_max", "table_id", "limit"))

    subset_serialized <- q$serialize(name = c("activity_id", "version_max"))
    expect_named(subset_serialized, c("activity_id", "version_max"))

    expect_equal(
        q$render(name = c("limit", "project")),
        c(limit = "limit=10", project = "project=CMIP6")
    )
    expect_true(any(grepl("^_timestamp:", q$render(name = "_timestamp"))))
    expect_true(any(grepl("^version:", q$render(name = "version"))))

    expect_true(serialized$activity_id$negate)
    expect_identical(serialized$table_id$value, "Amon")
    expect_true("datetime_start" %in% names(serialized))
    expect_identical(serialized$version_min$value, "[20200101 TO *]")
    expect_identical(serialized$version_max$value, "[* TO 20210101]")

    expect_s3_class(restored$activity_id(), "S7_object")
    expect_true(restored$activity_id()@negate)
    expect_identical(restored$params()$table_id@value, "Amon")
    expect_s3_class(restored$datetime_range()$start, "S7_object")
    expect_s3_class(restored$version_range()$max, "S7_object")
    expect_identical(restored$render(), q$render())
    expect_error(QueryParamStore$new()$restore(list(facet = list(project = serialized$project))), "Bucketed")
    expect_setequal(
        q$param_names(role = "result_field"),
        c("project", "activity_id", "table_id")
    )
    expect_setequal(
        q$param_names(role = "query"),
        c("datetime_start", "datetime_stop", "timestamp_from", "timestamp_to", "version_min", "version_max")
    )

    expect_warning(
        role_store <- QueryParamStore$new()$activity_id("CMIP")$fields("source_id")$facets("source_id")$shards("node")$params(
            table_id = "Amon",
            bbox = "0,0,1,1",
            start = "2020",
            end = "2021",
            from = "2020",
            to = "2021"
        ),
        NA
    )
    expect_setequal(
        role_store$param_names(role = "result_field"),
        c("project", "activity_id", "table_id")
    )

    serialized_all <- q$serialize(null = TRUE)
    restored_all <- QueryParamStore$new()$restore(serialized_all)
    expect_identical(restored_all$state(), q$state(null = FALSE))

    serialized_all <- q$serialize(null = TRUE, type = "json")
    expect_s3_class(serialized_all, "json")
})
