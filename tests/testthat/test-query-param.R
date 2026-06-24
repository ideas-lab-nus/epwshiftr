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
        render(QueryParamCtrl(FORMAT_JSON), "format"),
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
    expect_identical(query_param_spec("project")$role, "result_field")
    expect_identical(query_param_spec("fields")$role, "facet")
    expect_identical(query_param_spec("datetime_start")$role, "query")
    expect_identical(query_param_spec("limit")$role, "control")
    expect_identical(query_param_spec("bbox")$role, "facet")
    expect_true(all(vapply(
        QUERY_PARAM_NON_RESULT_FIELDS,
        function(name) query_param_spec(name)$role != "result_field",
        logical(1L)
    )))

    state_all <- store$state(null = TRUE)
    expect_named(state_all, c("facet", "query", "control", "others"))
    expect_s3_class(state_all$facet$project, "S7_object")
    expect_identical(state_all$facet$project@value, "CMIP6")
    expect_s3_class(store$fields(), "S7_object")
    expect_identical(store$fields()@value, "*")
    expect_s3_class(store$latest(), "S7_object")
    expect_true(store$latest()@value)
    expect_identical(store$offset()@value, 0L)
    expect_identical(store$distrib()@value, TRUE)
    expect_identical(store$format()@value, FORMAT_JSON)
    expect_error(QueryParamStore$new()$format("application/xml"), "Only JSON")
    expect_null(QueryParamStore$new()$format(NULL)$format())
    expect_identical(QueryParamStore$new()$type("File")$type()@value, "File")
    expect_identical(QueryParamStore$new()$type("Aggregation")$type()@value, "Aggregation")
    expect_error(QueryParamStore$new()$params(format = "application/xml"), "Only JSON")
    expect_identical(QueryParamStore$new()$params(type = "File")$type()@value, "File")
    expect_null(state_all$query$datetime_start)

    store$project(NULL)$fields(NULL)
    state_after_null <- store$state(null = TRUE)
    expect_null(state_after_null$facet$project)
    expect_null(state_after_null$facet$fields)

    store$project("CMIP6")$fields("*")
    store$params(table_id = "Amon")
    state <- store$state()
    expect_s3_class(state$others$table_id, "S7_object")
    expect_identical(state$others$table_id@value, "Amon")
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
    expect_named(subset_state, c("facet", "query", "others", "control"))
    expect_named(subset_state$facet, "activity_id")
    expect_named(subset_state$query, "version_max")
    expect_named(subset_state$control, "limit")
    expect_named(subset_state$others, "table_id")

    subset_serialized <- q$serialize(name = c("activity_id", "version_max"))
    expect_named(subset_serialized$facet, "activity_id")
    expect_named(subset_serialized$query, "version_max")
    expect_identical(subset_serialized$control, list())
    expect_identical(subset_serialized$others, list())

    expect_equal(
        q$render(name = c("limit", "project")),
        c(limit = "limit=10", project = "project=CMIP6")
    )
    expect_equal(
        QueryParamStore$new()$project("CMIP6")$fields("source_id")$limit(2L)$render(),
        c(
            project = "project=CMIP6",
            fields = "fields=source_id",
            latest = "latest=true",
            type = "type=Dataset",
            offset = "offset=0",
            distrib = "distrib=true",
            limit = "limit=2",
            format = "format=application%2Fsolr%2Bjson"
        )
    )
    expect_true(any(grepl("^_timestamp:", q$render(name = "_timestamp"))))
    expect_true(any(grepl("^version:", q$render(name = "version"))))

    expect_named(serialized, c("facet", "query", "control", "others"))
    expect_true(serialized$facet$activity_id$negate)
    expect_identical(serialized$others$table_id$value, "Amon")
    expect_true("datetime_start" %in% names(serialized$query))
    expect_identical(serialized$query$version_min$value, "[20200101 TO *]")
    expect_identical(serialized$query$version_max$value, "[* TO 20210101]")

    expect_s3_class(restored$activity_id(), "S7_object")
    expect_true(restored$activity_id()@negate)
    expect_identical(restored$params()$table_id@value, "Amon")
    expect_s3_class(restored$datetime_range()$start, "S7_object")
    expect_s3_class(restored$version_range()$max, "S7_object")
    expect_identical(restored$render(), q$render())
    expect_error(QueryParamStore$new()$restore(list(project = serialized$facet$project)), "subset")
    expect_setequal(
        q$param_names(role = "result_field"),
        c("project", "activity_id", "table_id")
    )
    expect_setequal(
        q$param_names(role = "query"),
        c("datetime_start", "datetime_stop", "timestamp_from", "timestamp_to", "version_min", "version_max")
    )

    role_store <- suppressWarnings(
        QueryParamStore$new()$activity_id("CMIP")$fields("source_id")$facets("source_id")$shards("node")$params(
            table_id = "Amon",
            bbox = "0,0,1,1",
            start = "2020",
            end = "2021",
            from = "2020",
            to = "2021"
        )
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
