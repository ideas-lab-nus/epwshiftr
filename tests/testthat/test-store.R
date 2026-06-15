# store_test__response() / store_test__params() / store_test__result() / store_test__file_docs() / store_test__completed_store() {{{
store_test__response <- function(docs) {
    list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = stats::setNames(list(), character())
        ),
        response = list(
            numFound = nrow(docs),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
        facet_counts = list(
            facet_queries = stats::setNames(list(), character()),
            facet_fields = stats::setNames(list(), character()),
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
}

store_test__params <- function(type = "File") {
    query_param__as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = type,
        format = QUERY_PARAM__FORMAT_JSON
    ))
}

store_test__result <- function(type = "File", docs, context = NULL) {
    generator <- switch(
        type,
        File = EsgResultFile,
        Aggregation = EsgResultAggregation
    )
    query_result__new(
        generator,
        index_node = "https://example.org",
        params = store_test__params(type),
        result = store_test__response(docs),
        context = context
    )
}

store_test__file_docs <- function(path = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                                         source_id = "EC-Earth3",
                                         experiment_id = "ssp585",
                                         variable_id = "tas",
                                         opendap_url = NULL,
                                         download_url = NULL) {
    if (is.null(opendap_url)) {
        opendap_url <- sprintf("https://example.org/dods/%s.html", path)
    }
    if (is.null(download_url)) {
        download_url <- sprintf("https://example.org/fileServer/%s", path)
    }

    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", path),
        master_id = sprintf("%s.master", path),
        replica = FALSE,
        tracking_id = "hdl:21.14100/local-test-2060",
        title = path,
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = source_id,
        experiment_id = experiment_id,
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = variable_id,
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("%s|application/netcdf|OPENDAP", opendap_url),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

store_test__completed_store <- function() {
    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    docs <- store_test__file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(store_test__result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN",
        nearest = 1L
    )
    store$extract(plan_id = plan$plan_id)

    list(store = store, dir = dir, nc = nc, plan = plan)
}

store_test__with_downloaded_query <- function(code) {
    src <- tempfile(fileext = ".nc")
    writeLines("tracked query netcdf placeholder", src)
    on.exit(unlink(src), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, track = TRUE)

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 1,
        access = I(list(c("HTTPServer"))),
        check.names = FALSE
    )
    file_docs <- store_test__file_docs(
        path = "tracked-download.nc",
        download_url = paste0("file://", normalizePath(src, winslash = "/"))
    )
    file_docs$checksum <- as.character(tools::md5sum(src))
    file_docs$checksum_type <- "MD5"
    file_docs$master_id <- "CMIP6.mock.tracked-download"
    file_docs$tracking_id <- "hdl:21.14100/tracked-download"
    file_docs$latest <- TRUE
    file_docs$retracted <- FALSE

    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            docs <- if (identical(query_param__value(params$type()), "Dataset")) dataset_docs else file_docs
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    dl <- store$downloader(n_workers = 0L, retries = 1L)
    session_id <- store$download_query(
        query_id,
        downloader = dl,
        replica = "current",
        probe = FALSE,
        progress = FALSE
    )

    code(store, dl, query_id, session_id)
}
# }}}
# EsgStore$new() {{{
test_that("EsgStore$new()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    expect_true(dir.exists(dir))
    expect_true(dir.exists(file.path(dir, "queries")))
    expect_true(dir.exists(file.path(dir, "dicts")))
    expect_true(dir.exists(file.path(dir, "sources")))
    expect_true(dir.exists(file.path(dir, "downloads")))
    expect_true(dir.exists(file.path(dir, "extracts")))
    expect_true(dir.exists(file.path(dir, "outputs")))
    expect_true(dir.exists(file.path(dir, "tmp")))
    expect_true(dir.exists(file.path(dir, "logs")))
    expect_equal(store$path, normalizePath(dir, mustWork = TRUE))
    expect_true(file.exists(store$manifest))
    expect_true(store$is_open)

    tables <- ddb_list_tables(priv(store)$conn)
    expect_setequal(
        tables,
        c(
            "store_meta", "artifact", "query_run", "esg_query",
            "esg_file", "esg_query_file", "file_catalog",
            "esg_query_update", "esg_query_update_file",
            "esg_query_tag", "esg_query_dependency",
            "extraction_plan", "extraction_result",
            "epw_source", "epw_baseline_summary", "epw_climate_summary",
            "epw_morph_plan", "epw_morph_factor", "epw_morph_result",
            "epw_output"
        )
    )
})

test_that("EsgStore$new(create = FALSE)", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    EsgStore$new(dir)$close()

    store <- EsgStore$new(dir, create = FALSE)
    on.exit(store$close(), add = TRUE)

    expect_true(store$is_open)
    expect_true(file.exists(file.path(dir, "manifest.duckdb")))
})

test_that("EsgStore$new() migrates older manifests", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    store$close()

    manifest <- file.path(dir, "manifest.duckdb")
    conn <- ddb_connect(manifest, read_only = FALSE)
    on.exit(if (!is.null(conn) && ddb_is_valid(conn)) ddb_disconnect(conn, shutdown = TRUE), add = TRUE)
    ddb_exec(conn, "DELETE FROM store_meta WHERE key = 'schema_version'")
    ddb_exec(conn, "ALTER TABLE esg_query_update DROP COLUMN IF EXISTS download_session_id")
    ddb_exec(conn, "ALTER TABLE esg_query_update DROP COLUMN IF EXISTS last_error")
    ddb_disconnect(conn, shutdown = TRUE)
    conn <- NULL

    store <- EsgStore$new(dir, create = FALSE)
    on.exit(store$close(), add = TRUE)

    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    cols <- names(ddb_read_table(priv(store)$conn, "esg_query_update"))
    expect_true(all(c("download_session_id", "last_error") %in% cols))
})
# }}}
# EsgStore$close() {{{
test_that("EsgStore$close()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    expect_true(store$is_open)

    store$close()
    expect_false(store$is_open)
})
# }}}
# EsgStore$downloader() {{{
test_that("EsgStore$downloader()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    dl <- store$downloader(n_workers = 0L)
    expect_s3_class(dl, "Downloader")
    expect_equal(dl$data_dir, normalizePath(file.path(dir, "downloads"), mustWork = TRUE, winslash = "/"))
    expect_equal(dl$tmp_dir, normalizePath(file.path(dir, "tmp", "downloads"), mustWork = TRUE, winslash = "/"))
    expect_equal(dl$manifest, normalizePath(file.path(dir, "downloads", "_downloader", "manifest.duckdb"), mustWork = FALSE, winslash = "/"))
    expect_true(file.exists(dl$manifest))
    dl_conn <- ddb_connect(dl$manifest, read_only = TRUE)
    on.exit(ddb_disconnect(dl_conn, shutdown = TRUE), add = TRUE)
    expect_true("download_config" %in% ddb_list_tables(dl_conn))
    dl_config <- ddb_read_table(dl_conn, "download_config")
    expect_equal(nrow(dl_config), 1L)
    expect_equal(dl_config$config_id, "default")
})
# }}}
# EsgStore$get_meta() / EsgStore$set_meta() {{{
test_that("EsgStore$get_meta() / EsgStore$set_meta()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    expect_null(store$get_meta("active_cmip6_index_artifact_id"))
    expect_invisible(store$set_meta("active_cmip6_index_artifact_id", "artifact-1"))
    expect_identical(store$get_meta("active_cmip6_index_artifact_id"), "artifact-1")
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
})
# }}}
# EsgStore$add_query() {{{
test_that("EsgStore$add_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)

    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)
    expect_type(query_id, "character")
    expect_identical(store$add_query(query), query_id)

    artifacts <- ddb_read_table(priv(store)$conn, "artifact")
    expect_equal(nrow(artifacts[artifacts$query_id == query_id & artifacts$kind == "query", ]), 1L)
})
# }}}
# EsgStore$queries() {{{
test_that("EsgStore$queries()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)

    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)
    queries <- store$queries()
    expect_equal(nrow(queries), 1L)
    expect_identical(queries$query_id[[1L]], query_id)
    expect_identical(queries$label[[1L]], "ssp585 tas")
    expect_true(queries$tracked[[1L]])
    expect_true(file.exists(file.path(dir, queries$query_file[[1L]])))
    expect_true(grepl("ssp585", queries$parameter_json[[1L]], fixed = TRUE))
    expect_equal(nrow(store$queries(tracked = TRUE)), 1L)
    expect_equal(nrow(store$queries(tracked = FALSE)), 0L)
})
# }}}
# EsgStore$track_query() / EsgStore$untrack_query() {{{
test_that("EsgStore$untrack_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)

    expect_invisible(store$untrack_query(query_id))
    expect_false(store$queries()$tracked[[1L]])
    expect_equal(nrow(store$queries(tracked = FALSE)), 1L)
})

test_that("EsgStore$track_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = FALSE)

    expect_invisible(store$track_query(query_id))
    expect_true(store$queries()$tracked[[1L]])
    expect_error(store$track_query("missing-query"), "was not found")
})
# }}}
# EsgStore$tag_query() {{{
test_that("EsgStore$tag_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)

    tags <- store$tag_query(query_id, c("ssp585", "tas"))
    expect_setequal(tags$tag, c("ssp585", "tas"))
})
# }}}
# EsgStore$query_tags() {{{
test_that("EsgStore$query_tags()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)
    store$tag_query(query_id, c("ssp585", "tas"))

    expect_identical(store$query_tags(query_id)$query_id[[1L]], query_id)
})
# }}}
# EsgStore$untag_query() {{{
test_that("EsgStore$untag_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)
    store$tag_query(query_id, c("ssp585", "tas"))

    expect_equal(nrow(store$untag_query(query_id, "tas")), 1L)
})
# }}}
# EsgStore$require_query() {{{
test_that("EsgStore$require_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)

    child_query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("pr")$
        limit(1L)
    child_id <- store$add_query(child_query, label = "child", track = TRUE)
    edges <- store$require_query(child_id, query_id)
    expect_equal(edges$query_id[[1L]], child_id)
})
# }}}
# EsgStore$query_graph() {{{
test_that("EsgStore$query_graph()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)

    child_query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("pr")$
        limit(1L)
    child_id <- store$add_query(child_query, label = "child", track = TRUE)
    store$require_query(child_id, query_id)

    expect_true(child_id %in% store$query_graph(query_id, direction = "children")$query_id)
})
# }}}
# EsgStore$unrequire_query() {{{
test_that("EsgStore$unrequire_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)

    child_query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("pr")$
        limit(1L)
    child_id <- store$add_query(child_query, label = "child", track = TRUE)
    store$require_query(child_id, query_id)

    expect_equal(nrow(store$unrequire_query(child_id, query_id)), 0L)
})
# }}}
# EsgStore$preview_update_queries() {{{
test_that("EsgStore$preview_update_queries()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, label = "preview tas", track = TRUE)
    qid <- query_id
    before_query <- store$queries()[query_id == qid]

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 1,
        access = I(list(c("HTTPServer"))),
        check.names = FALSE
    )
    file_docs <- store_test__file_docs(path = "preview-update.nc")
    file_docs$master_id <- "CMIP6.mock.preview-update"
    file_docs$tracking_id <- "hdl:21.14100/preview-update"
    file_docs$latest <- TRUE
    file_docs$retracted <- FALSE

    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            docs <- if (identical(query_param__value(params$type()), "Dataset")) dataset_docs else file_docs
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    preview <- store$preview_update_queries(query_id = query_id)
    expect_equal(preview$query_id, query_id)
    expect_equal(preview$file_total, 1L)
    expect_equal(preview$new_count, 1L)
    expect_equal(preview$bytes_total, 123)
    expect_equal(preview$bytes_new, 123)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_file")), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_update")), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_update_file")), 0L)
    after_preview_query <- store$queries()[query_id == qid]
    expect_identical(after_preview_query$last_checked_at[[1L]], before_query$last_checked_at[[1L]])

    detailed <- store$preview_update_queries(query_id = query_id, detail = TRUE)
    expect_named(detailed, c("summary", "changes"))
    expect_equal(detailed$summary$new_count, 1L)
    expect_equal(detailed$changes$change_type, "new")

    updated <- store$update_queries(query_id = query_id)
    update <- store$query_updates(query_id, latest = TRUE)
    expect_equal(preview$new_count, update$new_count)
    expect_equal(preview$stale_count, update$stale_count)
    expect_equal(preview$changed_count, update$changed_count)
    expect_equal(nrow(updated), 1L)
})
# }}}
# EsgStore$update_queries() / EsgStore$query_updates() / EsgStore$query_changes() / EsgStore$query_files() {{{
test_that("EsgStore$update_queries()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, track = TRUE)

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 2,
        access = I(list(c("OPENDAP", "HTTPServer"))),
        check.names = FALSE
    )
    file_one <- store_test__file_docs(path = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc")
    file_one$tracking_id <- "hdl:21.14100/mock-file-1"
    file_one$master_id <- "CMIP6.mock.master.file-1"
    file_one$latest <- TRUE
    file_one$retracted <- FALSE
    file_two <- store_test__file_docs(path = "tas_day_EC-Earth3_ssp585_r2i1p1f1_gr_20600101-20601231.nc")
    file_two$tracking_id <- "hdl:21.14100/mock-file-2"
    file_two$master_id <- "CMIP6.mock.master.file-2"
    file_two$latest <- TRUE
    file_two$retracted <- TRUE

    first_files <- data.table::rbindlist(list(file_one, file_two), fill = TRUE)
    second_files <- data.table::rbindlist(list(file_one), fill = TRUE)
    file_calls <- 0L
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                dataset_docs
            } else {
                file_calls <<- file_calls + 1L
                if (identical(file_calls, 1L)) first_files else second_files
            }
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    links <- store$update_queries()
    expect_equal(nrow(links), 2L)
    expect_true("update_id" %in% names(links))
    expect_setequal(links$change_type, "new")
    expect_equal(nrow(store$query_files(query_id, status = "current")), 1L)
    expect_equal(nrow(store$query_files(query_id, status = "retracted")), 1L)

    files <- ddb_read_table(priv(store)$conn, "esg_file")
    expect_equal(nrow(files), 2L)
    expect_setequal(files$file_key, c("master:CMIP6.mock.master.file-1", "master:CMIP6.mock.master.file-2"))
    links_db <- ddb_read_table(priv(store)$conn, "esg_query_file")
    expect_equal(nrow(links_db), 2L)
    catalog <- ddb_read_table(priv(store)$conn, "file_catalog")
    expect_equal(nrow(catalog), 1L)
    expect_identical(catalog$file_key[[1L]], "master:CMIP6.mock.master.file-1")

    links <- store$update_queries(query_id = query_id)
    status_by_file <- stats::setNames(links$status, links$file_key)
    expect_identical(status_by_file[["master:CMIP6.mock.master.file-1"]], "current")
    expect_identical(status_by_file[["master:CMIP6.mock.master.file-2"]], "missing")
    change_by_file <- stats::setNames(links$change_type, links$file_key)
    expect_identical(change_by_file[["master:CMIP6.mock.master.file-1"]], "current")
    expect_identical(change_by_file[["master:CMIP6.mock.master.file-2"]], "stale")
    updates <- store$query_updates(query_id)
    expect_equal(nrow(updates), 2L)
    latest <- store$query_updates(query_id, latest = TRUE)
    expect_equal(latest$stale_count, 1L)
    expect_equal(nrow(store$query_changes(update_id = latest$update_id, change_type = "stale")), 1L)
    expect_false(is.na(store$queries()$last_checked_at[[1L]]))
})
# }}}
# EsgStore$download_preflight() {{{
test_that("EsgStore$download_preflight()", {
    skip_if_not_installed("duckdb")

    src <- tempfile(fileext = ".nc")
    writeLines("preflight netcdf placeholder", src)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, label = "preflight tas", track = TRUE)

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 1,
        access = I(list(c("HTTPServer"))),
        check.names = FALSE
    )
    file_docs <- store_test__file_docs(
        path = "preflight-download.nc",
        download_url = paste0("file://", normalizePath(src, winslash = "/"))
    )
    file_docs$checksum <- as.character(tools::md5sum(src))
    file_docs$checksum_type <- "MD5"
    file_docs$master_id <- "CMIP6.mock.preflight-download"
    file_docs$tracking_id <- "hdl:21.14100/preflight-download"
    file_docs$latest <- TRUE
    file_docs$retracted <- FALSE

    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            docs <- if (identical(query_param__value(params$type()), "Dataset")) dataset_docs else file_docs
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    dl <- store$downloader(n_workers = 0L)
    preflight <- store$download_preflight(query_id, downloader = dl, replica = "current", probe = FALSE)
    expect_named(preflight, c("summary", "changes", "files", "candidates"))
    expect_equal(preflight$summary$query_id, query_id)
    expect_equal(preflight$summary$file_total, 1L)
    expect_equal(preflight$summary$current_count, 1L)
    expect_equal(preflight$summary$candidate_count, 1L)
    expect_equal(preflight$summary$bytes_total, 123)
    expect_equal(preflight$summary$local_available, 0L)
    expect_equal(preflight$summary$needs_download, 1L)
    expect_equal(preflight$summary$no_httpserver, 0L)
    expect_equal(preflight$summary$required_bytes, 123)
    expect_equal(preflight$summary$size_unknown_count, 0L)
    expect_false(preflight$summary$disk_would_block)
    expect_equal(preflight$changes$change_type, "new")
    expect_equal(preflight$files$status, "current")
    expect_equal(preflight$candidates$file_key, preflight$files$file_key)
    expect_equal(nrow(dl$sessions()), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_file")), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_update")), 0L)

    dry_run <- store$download_query(
        query_id,
        downloader = dl,
        replica = "current",
        dry_run = TRUE,
        probe = FALSE
    )
    expect_equal(dry_run$summary$needs_download, 1L)
    expect_equal(nrow(dl$sessions()), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_update")), 0L)
})
# }}}
# EsgStore$download_layout() / EsgStore$set_download_layout() {{{
test_that("EsgStore$download_layout() / EsgStore$set_download_layout()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    expect_equal(store$download_layout()$layout, "flat")
    store$set_download_layout(layout = "drs")
    expect_equal(store$download_layout()$layout, "drs")

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, track = TRUE)

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 1,
        access = I(list(c("HTTPServer"))),
        check.names = FALSE
    )
    file_docs <- store_test__file_docs(path = "layout.nc")
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            docs <- if (identical(query_param__value(params$type()), "Dataset")) dataset_docs else file_docs
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    dl <- store$downloader(n_workers = 0L)
    session_id <- store$download_query(
        query_id,
        downloader = dl,
        replica = "current",
        run = FALSE,
        probe = FALSE
    )
    tasks <- dl$tasks(session_id = session_id)

    expect_equal(tasks$subdir, file.path(
        "CMIP6",
        "ScenarioMIP",
        "EC-Earth-Consortium",
        "EC-Earth3",
        "ssp585",
        "r1i1p1f1",
        "day",
        "tas",
        "gr",
        "v20260101"
    ))
    expect_match(tasks$target_path, "downloads/CMIP6/ScenarioMIP/.*/v20260101/layout[.]nc$")
})
# }}}
# EsgStore$download_preflight() {{{
test_that("EsgStore$download_preflight() reports layout issues", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(2L)
    query_id <- store$add_query(query, track = TRUE)

    dataset_docs <- data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = 2,
        access = I(list(c("HTTPServer"))),
        check.names = FALSE
    )
    first <- store_test__file_docs(path = "collision.nc")
    second <- store_test__file_docs(path = "collision.nc")
    second$id <- "collision-second.nc|dataset-1"
    second$master_id <- "collision-second.master"
    second$tracking_id <- "hdl:21.14100/collision-second"
    second$checksum <- "def"
    file_docs <- rbind(first, second)

    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
            docs <- if (identical(query_param__value(params$type()), "Dataset")) dataset_docs else file_docs
            response <- store_test__response(docs)
            params$fields(c(query_param__value(params$fields()), required_fields))
            list(response = response, docs = response$response$docs, parameter = params)
        },
        .package = "epwshiftr"
    )

    store$set_download_layout(layout = "flat", collision = "suffix")
    preflight <- store$download_preflight(query_id, replica = "current", probe = FALSE)
    expect_equal(preflight$summary$target_path_collision_count, 1L)
    expect_true(all(preflight$candidates$target_path_collision))
    expect_true(all(grepl("file=", preflight$candidates$subdir, fixed = TRUE)))

    store$set_download_layout(layout = "drs", missing = "fallback", collision = "suffix")
    file_docs$activity_id <- NA_character_
    preflight <- store$download_preflight(query_id, replica = "current", probe = FALSE)
    expect_equal(preflight$summary$missing_layout_field_count, 2L)
    expect_true(all(startsWith(preflight$candidates$subdir, "datasets/")))
})
# }}}
# EsgStore$download_query() {{{
test_that("EsgStore$download_query()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        tasks <- dl$tasks(session_id = session_id)
        expect_equal(tasks$status, "done")
        qfiles <- store$query_files(query_id, status = "current")
        expect_equal(nrow(qfiles), 1L)
        expect_false(is.na(qfiles$local_path[[1L]]))
        expect_true(file.exists(file.path(store$path, qfiles$local_path[[1L]])))
        expect_false(is.na(qfiles$local_artifact_id[[1L]]))
    })
})
# }}}
# EsgStore$download_status() {{{
test_that("EsgStore$download_status()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        status <- store$download_status(query_id, downloader = dl)
        expect_equal(status$status, "done")
        expect_equal(status$query_file_status, "current")
    })
})
# }}}
# EsgStore$query_status() {{{
test_that("EsgStore$query_status()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        query_status <- store$query_status(query_id, downloader = dl)
        expect_equal(query_status$file_total, 1L)
        expect_equal(query_status$file_current, 1L)
        expect_equal(query_status$download_done, 1L)
        expect_equal(query_status$local_available, 1L)
        expect_true(query_status$complete)
    })
})
# }}}
# EsgStore$workflow_status() {{{
test_that("EsgStore$workflow_status()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        workflow <- store$workflow_status(query_id, downloader = dl)
        expect_equal(workflow$query_id, query_id)
        expect_equal(workflow$download_done, 1L)
        expect_equal(workflow$local_available, 1L)
        expect_equal(workflow$download_session_id, session_id)
        expect_equal(workflow$last_download_session_id, session_id)
        expect_equal(workflow$download_retryable, 0)
        expect_false(workflow$download_incomplete)
        expect_equal(workflow$new_count, 1L)
        expect_true("bytes_missing" %in% names(workflow))
    })
})
# }}}
# EsgStore$workflow_report() {{{
test_that("EsgStore$workflow_report()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        report <- store$workflow_report(query_id, downloader = dl)
        expect_named(report, c("summary", "updates", "changes", "downloads", "nodes"))
        expect_equal(report$summary$query_id, query_id)
        expect_equal(report$summary$last_download_session_id, session_id)
        expect_equal(report$summary$download_retryable, 0)
        expect_equal(report$changes$change_type, "new")
        expect_equal(report$downloads$status, "done")
        expect_equal(report$downloads$query_file_status, "current")
    })
})
# }}}
# EsgStore$retry_downloads() {{{
test_that("EsgStore$retry_downloads()", {
    skip_if_not_installed("duckdb")

    store_test__with_downloaded_query(function(store, dl, query_id, session_id) {
        expect_equal(nrow(store$retry_downloads(query_id, downloader = dl, run = FALSE)), 0L)
    })
})
# }}}
# EsgStore$remove_query() {{{
test_that("EsgStore$remove_query()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, track = TRUE)

    docs <- store_test__file_docs(path = "remove-query.nc")
    docs$master_id <- "CMIP6.mock.remove-query"
    files <- store_test__result(docs = docs)
    priv(store)$update_query_files(query_id, files)
    file_key <- store$query_files(query_id)$file_key[[1L]]

    removed <- store$remove_query(query_id, delete = "none")
    expect_equal(removed$query_id, query_id)
    expect_equal(removed$removed_file_links, 1L)
    expect_equal(nrow(store$queries()), 0L)

    links <- ddb_read_table(priv(store)$conn, "esg_query_file")
    expect_equal(nrow(links), 0L)
})
# }}}
# EsgStore$prune_orphans() {{{
test_that("EsgStore$prune_orphans()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query <- esg_query("https://example.org")$
        experiment_id("ssp585")$
        variable_id("tas")$
        limit(1L)
    query_id <- store$add_query(query, track = TRUE)

    docs <- store_test__file_docs(path = "remove-query.nc")
    docs$master_id <- "CMIP6.mock.remove-query"
    files <- store_test__result(docs = docs)
    priv(store)$update_query_files(query_id, files)
    file_key <- store$query_files(query_id)$file_key[[1L]]
    store$remove_query(query_id, delete = "none")

    orphans <- store$prune_orphans(delete_local = FALSE)
    expect_equal(orphans$file_key, file_key)
})
# }}}
# EsgStore$remove_files() {{{
test_that("EsgStore$remove_files()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "remove-file.nc"))
    query_id <- store$add_files(files, label = "remove file test")
    file_key <- store$query("SELECT file_key FROM esg_file")$file_key[[1L]]
    local_file <- file.path(store$path, "downloads", "remove-file.nc")
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("local netcdf placeholder", local_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = local_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )

    rel <- store_rel_path(local_file, store$path)
    conn <- priv(store)$conn
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))

    expect_error(store$remove_files(file_key), "still linked")
    removed <- store$remove_files(file_key, delete_local = TRUE, force = TRUE)
    expect_equal(removed$file_key, file_key)
    expect_true(removed$deleted_local)
    expect_false(file.exists(local_file))
    expect_equal(nrow(ddb_read_table(conn, "esg_file")), 0L)
    expect_equal(nrow(ddb_read_table(conn, "file_catalog")), 0L)
    expect_equal(nrow(ddb_read_table(conn, "artifact")), 1L)
})
# }}}
# EsgStore$storage_report() {{{
test_that("EsgStore$storage_report()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    untracked_file <- file.path(store$path, "downloads", "untracked.nc")
    tmp_file <- file.path(store$path, "tmp", "downloads", "task.part")
    dir.create(dirname(untracked_file), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(tmp_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("untracked netcdf placeholder", untracked_file)
    writeLines("partial download placeholder", tmp_file)

    report <- store$storage_report(detail = TRUE)
    expect_named(report, c("summary", "downloads", "registered", "untracked_files", "missing_records", "tmp", "orphan_records"))
    expect_equal(report$summary$untracked_file_count, 1L)
    expect_equal(report$summary$tmp_file_count, 1L)
    expect_equal(report$summary$registered_file_count, 0L)
    expect_true(store_rel_path(untracked_file, store$path) %in% report$untracked_files$relative_path)
    expect_true(store_rel_path(tmp_file, store$path) %in% report$tmp$relative_path)
})
# }}}
# EsgStore$cleanup_downloads() {{{
test_that("EsgStore$cleanup_downloads()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    untracked_file <- file.path(store$path, "downloads", "untracked.nc")
    tmp_file <- file.path(store$path, "tmp", "downloads", "task.part")
    dir.create(dirname(untracked_file), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(tmp_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("untracked netcdf placeholder", untracked_file)
    writeLines("partial download placeholder", tmp_file)

    dry <- store$cleanup_downloads(scope = c("tmp", "untracked_files"), dry_run = TRUE)
    expect_equal(sort(dry$scope), c("tmp", "untracked_files"))
    expect_true(all(dry$dry_run))
    expect_true(file.exists(untracked_file))
    expect_true(file.exists(tmp_file))

    cleaned <- store$cleanup_downloads(scope = c("tmp", "untracked_files"), dry_run = FALSE)
    expect_equal(sort(cleaned$scope), c("tmp", "untracked_files"))
    expect_true(all(cleaned$deleted))
    expect_false(file.exists(untracked_file))
    expect_false(file.exists(tmp_file))

    summary <- store$storage_report()
    expect_equal(summary$untracked_file_count, 0L)
    expect_equal(summary$tmp_file_count, 0L)
})
# }}}
# EsgStore$validate_files() {{{
test_that("EsgStore$validate_files()", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "validate-file.nc"))
    query_id <- store$add_files(files, label = "validate file test")
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]

    local_file <- file.path(store$path, "downloads", "validate-file.nc")
    artifact_file <- file.path(store$path, "downloads", "artifact-file.nc")
    untracked_file <- file.path(store$path, "downloads", "untracked-validate.nc")
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("local placeholder", local_file)
    writeLines("artifact placeholder", artifact_file)
    writeLines("untracked placeholder", untracked_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = artifact_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )

    rel <- store_rel_path(local_file, store$path)
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    catalog_before <- ddb_read_table(conn, "file_catalog")
    artifacts_before <- ddb_read_table(conn, "artifact")

    no_hash <- store$validate_files(query_id = query_id, checksum = FALSE)
    expect_named(no_hash, c("summary", "files", "artifacts", "untracked", "actions"))
    expect_equal(no_hash$summary$file_count, 1L)
    expect_equal(no_hash$summary$bad_size_count, 1L)
    expect_equal(no_hash$summary$bad_checksum_count, 0L)
    expect_equal(no_hash$summary$artifact_mismatch_count, 1L)
    expect_equal(no_hash$summary$untracked_file_count, 1L)
    expect_true(no_hash$files$exists)
    expect_false(no_hash$files$size_ok)
    expect_true(is.na(no_hash$files$checksum_ok))
    expect_false(no_hash$files$artifact_path_matches)
    expect_true(store_rel_path(untracked_file, store$path) %in% no_hash$untracked$relative_path)

    with_hash <- store$validate_files(query_id = query_id, checksum = TRUE)
    expect_equal(with_hash$summary$bad_checksum_count, 1L)
    expect_false(with_hash$files$checksum_ok)
    expect_false(is.na(with_hash$files$checksum_actual))

    expect_equal(ddb_read_table(conn, "file_catalog"), catalog_before)
    expect_equal(ddb_read_table(conn, "artifact"), artifacts_before)
    expect_true(file.exists(local_file))
    expect_true(file.exists(artifact_file))
    expect_true(file.exists(untracked_file))
})
# }}}
# EsgStore$validate_files() {{{
test_that("EsgStore$validate_files(layout = TRUE)", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "layout-target.nc"))
    query_id <- store$add_files(files, label = "validate layout test")
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]

    wrong_file <- file.path(store$path, "downloads", "wrong-layout.nc")
    dir.create(dirname(wrong_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("layout placeholder", wrong_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = wrong_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )
    rel <- store_rel_path(wrong_file, store$path)
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))

    layout <- store$validate_files(query_id = query_id, layout = TRUE)
    expect_equal(layout$summary$layout_mismatch_count, 1L)
    expect_true("move_to_layout" %in% layout$actions$action)
    expect_match(layout$actions[action == "move_to_layout"]$to_path, "downloads/layout-target[.]nc$")

    unlink(wrong_file)
    missing <- store$validate_files(query_id = query_id, layout = TRUE)
    expect_equal(missing$summary$missing_file_count, 1L)
    expect_true(all(c("clear_missing_local_ref", "remove_missing_artifact") %in% missing$actions$action))
    expect_false(file.exists(wrong_file))
})
# }}}
# EsgStore$repair_files() {{{
test_that("EsgStore$repair_files() clears missing records", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "repair-missing.nc"))
    query_id <- store$add_files(files, label = "repair missing test")
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]

    local_file <- file.path(store$path, "downloads", "repair-missing.nc")
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("missing repair placeholder", local_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = local_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )
    rel <- store_rel_path(local_file, store$path)
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    unlink(local_file)

    actions <- store$validate_files(query_id = query_id)$actions
    dry <- store$repair_files(actions, dry_run = TRUE)
    expect_true(all(dry$dry_run))
    expect_false(any(dry$done))
    expect_false(is.na(ddb_read_table(conn, "file_catalog")$local_path[[1L]]))
    expect_true(artifact_id %in% ddb_read_table(conn, "artifact")$artifact_id)

    repaired <- store$repair_files(actions, dry_run = FALSE)
    expect_true(all(repaired$done))
    catalog <- ddb_read_table(conn, "file_catalog")
    esg_file <- ddb_read_table(conn, "esg_file")
    artifacts <- ddb_read_table(conn, "artifact")
    expect_true(is.na(catalog$local_path[[1L]]))
    expect_true(is.na(catalog$local_artifact_id[[1L]]))
    expect_true(is.na(esg_file$local_path[[1L]]))
    expect_true(is.na(esg_file$local_artifact_id[[1L]]))
    expect_false(artifact_id %in% artifacts$artifact_id)
})
# }}}
# EsgStore$repair_files() {{{
test_that("EsgStore$repair_files() moves layout mismatches", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "repair-layout.nc"))
    query_id <- store$add_files(files, label = "repair layout test")
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]

    old_dir <- file.path(store$path, "downloads", "old-layout")
    old_file <- file.path(old_dir, "repair-layout-old.nc")
    target_file <- file.path(store$path, "downloads", "repair-layout.nc")
    dir.create(old_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("layout repair placeholder", old_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = old_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )
    rel <- store_rel_path(old_file, store$path)
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))

    actions <- store$validate_files(query_id = query_id)$actions
    move <- actions[action == "move_to_layout"]
    expect_equal(nrow(move), 1L)

    dry <- store$repair_files(move, dry_run = TRUE)
    expect_true(dry$dry_run)
    expect_false(dry$done)
    expect_true(file.exists(old_file))
    expect_false(file.exists(target_file))

    repaired <- store$repair_files(move, dry_run = FALSE)
    expect_true(repaired$done)
    expect_false(file.exists(old_file))
    expect_true(file.exists(target_file))
    expect_false(dir.exists(old_dir))
    catalog <- ddb_read_table(conn, "file_catalog")
    esg_file <- ddb_read_table(conn, "esg_file")
    artifact <- ddb_read_table(conn, "artifact")
    target_rel <- store_rel_path(target_file, store$path)
    expect_equal(catalog$local_path[[1L]], target_rel)
    expect_equal(esg_file$local_path[[1L]], target_rel)
    expect_equal(artifact$relative_path[artifact$artifact_id == artifact_id], target_rel)
})
# }}}
# EsgStore$cleanup_downloads() {{{
test_that("EsgStore$cleanup_downloads(scope = 'missing_records')", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(docs = store_test__file_docs(path = "missing-record.nc"))
    query_id <- store$add_files(files, label = "missing local file test")
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]

    local_file <- file.path(store$path, "downloads", "missing-record.nc")
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("local netcdf placeholder", local_file)
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = local_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )

    rel <- store_rel_path(local_file, store$path)
    ddb_exec(conn, sprintf(
        "UPDATE file_catalog SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    ddb_exec(conn, sprintf(
        "UPDATE esg_file SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
        ddb_literal(conn, rel),
        ddb_literal(conn, artifact_id),
        ddb_literal(conn, file_key)
    ))
    unlink(local_file)

    report <- store$storage_report(detail = TRUE)
    expect_gt(report$summary$missing_record_count, 0L)
    expect_true(artifact_id %in% report$missing_records$artifact_id)
    expect_true(file_key %in% report$missing_records$file_key)

    dry <- store$cleanup_downloads(scope = "missing_records", dry_run = TRUE)
    expect_true(all(dry$dry_run))
    expect_false(is.na(ddb_read_table(conn, "file_catalog")$local_path[[1L]]))
    expect_true(artifact_id %in% ddb_read_table(conn, "artifact")$artifact_id)

    cleaned <- store$cleanup_downloads(scope = "missing_records", dry_run = FALSE)
    expect_true(all(cleaned$record_removed))
    catalog <- ddb_read_table(conn, "file_catalog")
    esg_file <- ddb_read_table(conn, "esg_file")
    artifacts <- ddb_read_table(conn, "artifact")
    expect_true(is.na(catalog$local_path[[1L]]))
    expect_true(is.na(catalog$local_artifact_id[[1L]]))
    expect_true(is.na(esg_file$local_path[[1L]]))
    expect_true(is.na(esg_file$local_artifact_id[[1L]]))
    expect_false(artifact_id %in% artifacts$artifact_id)
})
# }}}
# EsgStore$add_files() {{{
test_that("EsgStore$add_files() catalogs File records", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- store_test__result(
        docs = store_test__file_docs(),
        context = list(time_filter = list(
            start = "2060-01-01T00:00:00Z",
            stop = "2060-12-31T23:59:59Z",
            method = "drs"
        ))
    )

    query_id <- store$add_files(files, label = "cmip6 test")
    conn <- priv(store)$conn
    runs <- ddb_read_table(conn, "query_run")
    catalog <- ddb_read_table(conn, "file_catalog")
    artifacts <- ddb_read_table(conn, "artifact")

    expect_match(query_id, "^[0-9a-f]{64}$")
    expect_equal(nrow(runs), 1L)
    expect_equal(runs$label, "cmip6 test")
    expect_equal(runs$result_type, "File")
    expect_equal(runs$time_filter_method, "drs")
    expect_true(file.exists(file.path(dir, runs$query_file)))
    expect_equal(nrow(catalog), 1L)
    expect_match(catalog$file_key, "^(master|tracking|checksum|id|fallback):")
    expect_equal(catalog$query_id, query_id)
    expect_equal(catalog$source_id, "EC-Earth3")
    expect_equal(catalog$experiment_id, "ssp585")
    expect_equal(catalog$url_opendap, "https://example.org/dods/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc")
    expect_equal(catalog$url_download, "https://example.org/fileServer/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc")
    expect_equal(nrow(artifacts), 1L)
    expect_equal(artifacts$kind, "query")
    expect_true(file.exists(store$artifact_path(artifacts$artifact_id)))
})

test_that("EsgStore$add_files() deduplicates File replicas", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    replica <- store_test__file_docs(
        opendap_url = "https://replica.example.org/dods/tas.nc",
        download_url = "https://replica.example.org/fileServer/tas.nc"
    )
    replica$id <- "tas-replica|dataset-1"
    replica$replica <- TRUE
    replica$data_node <- "replica.example.org"

    master <- store_test__file_docs(
        opendap_url = "https://master.example.org/dods/tas.nc",
        download_url = "https://master.example.org/fileServer/tas.nc"
    )
    master$id <- "tas-master|dataset-1"
    master$replica <- FALSE
    master$data_node <- "master.example.org"

    files <- store_test__result(
        docs = data.table::rbindlist(list(replica, master), fill = TRUE)
    )
    query_id <- store$add_files(files, label = "cmip6 duplicate replica test")

    catalog <- ddb_read_table(priv(store)$conn, "file_catalog")
    esg_file <- ddb_read_table(priv(store)$conn, "esg_file")
    links <- ddb_read_table(priv(store)$conn, "esg_query_file")

    expect_equal(nrow(catalog), 1L)
    expect_equal(nrow(esg_file), 1L)
    expect_equal(nrow(links), 1L)
    expect_equal(catalog$query_id, query_id)
    expect_equal(catalog$data_node, "master.example.org")
    expect_equal(catalog$url_download, "https://master.example.org/fileServer/tas.nc")
})
# }}}
# EsgStore$plan_region() {{{
test_that("EsgStore$plan_region() respects variable filters", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    tas <- store_test__file_docs(variable_id = "tas")
    hurs <- store_test__file_docs(
        path = "hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
        variable_id = "hurs"
    )
    files <- store_test__result(docs = data.table::rbindlist(list(tas, hurs), fill = TRUE))
    query_id <- store$add_files(files, label = "cmip6 multi-variable test")

    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        variable_id = "tas",
        nearest = 1L
    )

    expect_equal(nrow(plan), 1L)
    expect_equal(plan$variable_id, "tas")
})
# }}}
# EsgStore$download_files() / EsgStore$sync_downloads() {{{
test_that("EsgStore$download_files() / EsgStore$sync_downloads()", {
    skip_if_not_installed("duckdb")

    src <- tempfile(fileext = ".nc")
    writeLines("local netcdf placeholder", src)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- store_test__file_docs(
        path = "local-download.nc",
        download_url = paste0("file://", normalizePath(src, winslash = "/"))
    )
    docs$checksum <- as.character(tools::md5sum(src))
    docs$checksum_type <- "MD5"
    files <- store_test__result(docs = docs)

    dl <- store$downloader(n_workers = 0L, retries = 1L)
    session_id <- store$download_files(
        files = files,
        replica = "current",
        downloader = dl,
        run = TRUE,
        session_label = "local-download",
        probe = FALSE,
        progress = FALSE
    )

    tasks <- dl$tasks(session_id = session_id)
    expect_equal(tasks$status, "done")
    expect_true(file.exists(file.path(dir, "downloads", "local-download.nc")))

    catalog <- store$query("SELECT local_path, local_artifact_id FROM file_catalog")
    expect_equal(nrow(catalog), 1L)
    expect_false(is.na(catalog$local_path))
    expect_true(file.exists(file.path(store$path, catalog$local_path)))
    expect_false(is.na(catalog$local_artifact_id))
    expect_true(file.exists(store$artifact_path(catalog$local_artifact_id)))
})
# }}}
# EsgStore$add_files() {{{
test_that("EsgStore$add_files() catalogs Aggregation records", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    aggs <- store_test__result(
        type = "Aggregation",
        docs = store_test__file_docs("tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20610101-20611231.nc")
    )

    query_id_1 <- store$add_files(aggs)
    query_id_2 <- store$add_files(aggs)
    conn <- priv(store)$conn

    expect_identical(query_id_2, query_id_1)
    expect_equal(nrow(ddb_read_table(conn, "query_run")), 1L)
    expect_equal(nrow(ddb_read_table(conn, "file_catalog")), 1L)
    expect_equal(nrow(ddb_read_table(conn, "artifact")), 1L)
    expect_equal(ddb_read_table(conn, "query_run")$result_type, "Aggregation")
})

test_that("EsgStore$add_files() records empty child query runs", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    empty_docs <- store_test__file_docs()[0L, ]

    for (type in c("File", "Aggregation")) {
        generator <- switch(type, File = EsgResultFile, Aggregation = EsgResultAggregation)
        response <- store_test__response(empty_docs)
        response$response$docs <- list()
        result <- query_result__new(
            generator,
            index_node = "https://example.org",
            params = store_test__params(type),
            result = response
        )

        query_id <- store$add_files(result, label = paste("empty", type))
        expect_match(query_id, "^[0-9a-f]{64}$")
    }

    conn <- priv(store)$conn
    runs <- ddb_read_table(conn, "query_run")
    catalog <- ddb_read_table(conn, "file_catalog")
    artifacts <- ddb_read_table(conn, "artifact")
    validation <- store$validate()

    expect_equal(nrow(runs), 2L)
    expect_setequal(runs$result_type, c("File", "Aggregation"))
    expect_equal(nrow(catalog), 0L)
    expect_equal(nrow(artifacts), 2L)
    expect_true(all(validation$exists))
    expect_true(all(validation$checksum_ok))
    expect_true(all(validation$size_ok))
})
# }}}
# EsgStore$plan_region() {{{
test_that("EsgStore$plan_region() creates extraction plans", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- data.table::rbindlist(list(
        store_test__file_docs(
            "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "EC-Earth3",
            experiment_id = "ssp585",
            variable_id = "tas"
        ),
        store_test__file_docs(
            "hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "EC-Earth3",
            experiment_id = "ssp585",
            variable_id = "hurs"
        ),
        store_test__file_docs(
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "AWI-CM-1-1-MR",
            experiment_id = "ssp585",
            variable_id = "tas"
        )
    ), fill = TRUE)
    files <- store_test__result(docs = as.data.frame(docs))
    query_id <- store$add_files(files)

    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01", "2060-12-31"),
        site_id = "SIN",
        filters = list(source_id = "EC-Earth3"),
        variable_id = c("tas", "hurs"),
        nearest = 2L
    )

    expect_s3_class(plan, "data.table")
    expect_equal(nrow(plan), 2L)
    expect_setequal(plan$variable_id, c("tas", "hurs"))
    expect_equal(unique(plan$site_id), "SIN")
    expect_equal(unique(plan$status), "pending")
    expect_equal(unique(plan$nearest), 2L)
    expect_match(plan$plan_id, "^[0-9a-f]{64}$")

    conn <- priv(store)$conn
    ddb_exec(conn, "UPDATE extraction_plan SET status = 'done'")
    plan_again <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01", "2060-12-31"),
        site_id = "SIN",
        filters = list(source_id = "EC-Earth3"),
        variable_id = c("tas", "hurs"),
        nearest = 2L
    )
    expect_equal(nrow(ddb_read_table(conn, "extraction_plan")), 2L)
    expect_equal(unique(plan_again$status), "done")
})

test_that("EsgStore$plan_region() rejects invalid plans", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query_id <- store$add_files(store_test__result(docs = store_test__file_docs()))

    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-12-31", "2060-01-01")
        ),
        "greater than or equal"
    )
    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-01-01", "2060-12-31"),
            filters = list(unknown = "x")
        ),
        "Unknown file catalog filter"
    )
    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-01-01", "2060-12-31"),
            filters = list(source_id = "no-match")
        ),
        "No cataloged file records match"
    )
})
# }}}
# EsgStore$extract() {{{
test_that("EsgStore$extract()", {
    skip_if_not_installed("duckdb")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- store_test__file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(store_test__result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN",
        nearest = 1L
    )

    processed <- store$extract(plan_id = plan$plan_id)
    conn <- priv(store)$conn
    plans <- ddb_read_table(conn, "extraction_plan")
    results <- ddb_read_table(conn, "extraction_result")
    artifacts <- ddb_read_table(conn, "artifact")

    expect_equal(processed$status, "done")
    expect_equal(plans$status, "done")
    expect_equal(plans$available_time_count, 2L)
    expect_equal(nrow(results), 1L)
    expect_equal(results$year, 2060L)
    expect_equal(results$row_count, 2L)
    expect_equal(results$unique_time_count, 2L)
    expect_match(results$artifact_id, "^[0-9a-f]{64}$")
    expect_true(results$artifact_id %in% artifacts$artifact_id)
    expect_true(any(artifacts$kind == "extract"))

    parquet <- file.path(dir, results$output_path)
    expect_true(file.exists(parquet))
    rows <- ddb_query(conn, sprintf(
        "SELECT site_id, source_id, experiment_id, variable_id, units, COUNT(*) AS n FROM read_parquet(%s) GROUP BY ALL",
        ddb_literal(conn, parquet)
    ))
    expect_equal(rows$site_id, "SIN")
    expect_equal(rows$source_id, "EC-Earth3")
    expect_equal(rows$experiment_id, "ssp585")
    expect_equal(rows$variable_id, "tas")
    expect_equal(rows$units, "K")
    expect_equal(rows$n, 2)

    validation <- store$validate()
    expect_true(all(validation$exists))
    expect_true(all(validation$checksum_ok, na.rm = TRUE))
    expect_true(all(validation$size_ok, na.rm = TRUE))
})

test_that("EsgStore$extract() records failed plans", {
    skip_if_not_installed("duckdb")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- store_test__file_docs(
        path = basename(nc),
        variable_id = "hurs",
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(store_test__result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )

    processed <- store$extract(plan_id = plan$plan_id)
    plans <- ddb_read_table(priv(store)$conn, "extraction_plan")

    expect_equal(processed$status, "failed")
    expect_equal(plans$status, "failed")
    expect_equal(plans$attempt_count, 1L)
    expect_match(plans$last_error, "None of the requested variable")
})
# }}}
# EsgStore$summarise() {{{
test_that("EsgStore$summarise()", {
    skip_if_not_installed("duckdb")

    fixture <- store_test__completed_store()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)

    summary <- store$summarise()
    expect_s3_class(summary, "data.table")
    expect_equal(nrow(summary), 1L)
    expect_equal(summary$source_id, "EC-Earth3")
    expect_equal(summary$site_id, "SIN")
    expect_equal(summary$variable_id, "tas")
    expect_equal(summary$year, 2060L)
    expect_equal(summary$row_count, 2)
    expect_equal(summary$unique_time_count, 2)
})
# }}}
# EsgStore$coverage() {{{
test_that("EsgStore$coverage()", {
    skip_if_not_installed("duckdb")

    fixture <- store_test__completed_store()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)

    cov <- store$coverage()
    expect_s3_class(cov, "data.table")
    expect_true(cov$complete)
    expect_equal(cov$output_time_count, 2)
    expect_equal(cov$output_rows, 2)
})

test_that("EsgStore$coverage() detects incomplete outputs", {
    skip_if_not_installed("duckdb")

    fixture <- store_test__completed_store()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)

    results <- ddb_read_table(priv(store)$conn, "extraction_result")
    unlink(file.path(fixture$dir, results$output_path))
    cov <- store$coverage()
    expect_false(cov$complete)
    expect_false(cov$output_files_exist)
    validation <- store$validate()
    expect_true(any(!validation$exists))
    expect_error(store$assert_complete(), "incomplete")
})
# }}}
# EsgStore$assert_complete() {{{
test_that("EsgStore$assert_complete()", {
    skip_if_not_installed("duckdb")

    fixture <- store_test__completed_store()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)

    expect_silent(store$assert_complete())
})
# }}}
# EsgStore$query() {{{
test_that("EsgStore$query()", {
    skip_if_not_installed("duckdb")

    fixture <- store_test__completed_store()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)

    sql <- store$query("SELECT COUNT(*) AS n FROM extraction_result")
    expect_equal(sql$n, 1)
})
# }}}
