# store_test__response() / store_test__params() / store_test__result() / store_test__file_docs() / store_test__completed_store() {{{
# store_test__response {{{
store_test__response <- function(docs) {
    esgf_test__response(docs)
}
# }}}

# store_test__params {{{
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
# }}}

# store_test__result {{{
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
# }}}

# store_test__file_docs {{{
store_test__file_docs <- function(
    path = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
    source_id = "EC-Earth3",
    experiment_id = "ssp585",
    variable_id = "tas",
    opendap_url = NULL,
    download_url = NULL
) {
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
# }}}

# store_test__new {{{
store_test__new <- function(
    dir = withr::local_tempdir(.local_envir = env),
    env = parent.frame()
) {
    store <- EsgStore$new(dir)
    withr::defer(store$close(), envir = env)
    store
}
# }}}

# store_test__mock_collect {{{
store_test__mock_collect <- function(
    dataset_docs,
    file_docs,
    on_call = NULL,
    env = parent.frame()
) {
    file_calls <- 0L
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            if (is.function(on_call)) {
                on_call(params)
            }
            docs <- if (
                identical(query_param__value(params$type()), "Dataset")
            ) {
                dataset_docs
            } else if (is.function(file_docs)) {
                file_calls <<- file_calls + 1L
                file_docs(file_calls)
            } else {
                file_docs
            }
            response <- store_test__response(docs)
            params$fields(c(
                query_param__value(params$fields()),
                required_fields
            ))
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = env
    )
}
# }}}

# store_test__dataset_docs {{{
store_test__dataset_docs <- function(
    size = 1,
    access = c("HTTPServer")
) {
    data.frame(
        id = "dataset-1",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        size = size,
        access = I(list(access)),
        check.names = FALSE
    )
}
# }}}

# store_test__link_local_file {{{
store_test__link_local_file <- function(
    store,
    filename,
    local_rel = file.path("downloads", filename),
    artifact_rel = local_rel,
    content = "local netcdf placeholder",
    label = filename
) {
    query_id <- store$add_files(
        store_test__result(docs = store_test__file_docs(path = filename)),
        label = label
    )
    conn <- priv(store)$conn
    file_key <- ddb_read_table(conn, "file_catalog")$file_key[[1L]]
    local_file <- file.path(store$path, local_rel)
    artifact_file <- file.path(store$path, artifact_rel)
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(content, local_file)
    if (!identical(artifact_file, local_file)) {
        dir.create(
            dirname(artifact_file),
            recursive = TRUE,
            showWarnings = FALSE
        )
        writeLines("artifact placeholder", artifact_file)
    }
    artifact_id <- store$register_artifact(
        kind = "netcdf",
        path = artifact_file,
        role = "download",
        project = "CMIP6",
        query_id = query_id,
        file_key = file_key
    )
    rel <- store_rel_path(local_file, store$path)
    for (tbl in c("file_catalog", "esg_file")) {
        ddb_exec(
            conn,
            sprintf(
                "UPDATE %s SET local_path = %s, local_artifact_id = %s WHERE file_key = %s",
                tbl,
                ddb_literal(conn, rel),
                ddb_literal(conn, artifact_id),
                ddb_literal(conn, file_key)
            )
        )
    }
    list(
        query_id = query_id,
        file_key = file_key,
        artifact_id = artifact_id,
        local_file = local_file,
        artifact_file = artifact_file,
        rel = rel
    )
}
# }}}

# store_test__local_nc_plan {{{
store_test__local_nc_plan <- function(
    store,
    year = 2060L,
    calendar = NULL,
    n_years = 1L,
    variable_id = "tas",
    time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
    site_id = "SIN",
    env = parent.frame()
) {
    nc <- tempfile(fileext = ".nc")
    args <- list(nc, year)
    if (!is.null(calendar)) {
        args$calendar <- calendar
    }
    if (!identical(n_years, 1L)) {
        args$n_years <- n_years
    }
    do.call(write_local_cmip6_netcdf_fixture, args)
    withr::defer(unlink(nc), envir = env)
    docs <- store_test__file_docs(
        path = basename(nc),
        variable_id = variable_id,
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(store_test__result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = time,
        site_id = site_id
    )
    list(nc = nc, query_id = query_id, plan = plan)
}
# }}}

# store_test__completed_store {{{
store_test__completed_store <- function(env = parent.frame()) {
    dir <- withr::local_tempdir(.local_envir = env)
    store <- store_test__new(dir = dir, env = env)
    setup <- store_test__local_nc_plan(store, env = env)
    store$extract(plan_id = setup$plan$plan_id)
    list(store = store, dir = dir, nc = setup$nc, plan = setup$plan)
}
# }}}

# Create one real catalog and extraction plan while exposing the internal rows
# needed to exercise failure recovery without relying on an external service.
# store_test__planned_extract {{{
store_test__planned_extract <- function() {
    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    docs <- store_test__file_docs(
        path = basename(nc),
        opendap_url = "https://remote.example.org/dods/tas.nc",
        download_url = "https://download.example.org/files/tas.nc"
    )
    query_id <- store$add_files(store_test__result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN"
    )
    file <- data.table::as.data.table(
        ddb_read_table(priv(store)$conn, "file_catalog")
    )[1L]
    list(store = store, dir = dir, nc = nc, plan = plan, file = file)
}
# }}}

# Mimic an already-open remote dataset that fails at a chosen access phase;
# read-phase fixtures reuse a genuine CF time axis to isolate the failure.
# store_test__failing_dataset {{{
store_test__failing_dataset <- function(phase, time_info = NULL) {
    checkmate::assert_choice(phase, c("metadata", "read"))
    ds <- R6::R6Class(
        private = list(progress_callback = NULL),
        lock_objects = FALSE
    )$new()
    ds$is_open <- TRUE
    ds$close <- function() {
        ds$is_open <- FALSE
        invisible(NULL)
    }
    private <- priv(ds)
    private$nc_handles <- list()
    private$opened <- TRUE
    private$check_open <- function() {
        if (identical(phase, "metadata")) {
            stop("remote metadata timed out", call. = FALSE)
        }
    }
    private$check_index <- function(index) invisible(NULL)
    private$metadata_cache <- list(time_coordinates_1 = time_info)
    ds$read_region <- function(...) {
        stop("remote read timed out", call. = FALSE)
    }
    ds$att_get <- function(...) "K"
    ds
}
# }}}

# Capture reporter events in memory so tests can verify the exact structured
# fields later persisted by the workflow reporter in shift_run_event.
# store_test__access_reporter {{{
store_test__access_reporter <- function() {
    reporter <- new.env(parent = emptyenv())
    reporter$events <- list()
    reporter$notice <- function(message, outcome, details) {
        reporter$events[[length(reporter$events) + 1L]] <- list(
            message = message,
            outcome = outcome,
            details = details
        )
        invisible(NULL)
    }
    reporter$detail <- function(...) invisible(NULL)
    reporter$heartbeat <- function(...) invisible(NULL)
    reporter
}
# }}}

# Temporarily replace one locked R6 private method and restore it when the
# current test exits, keeping fault injection local to the owning store object.
# store_test__mock_private {{{
store_test__mock_private <- function(store, name, value, env = parent.frame()) {
    private <- priv(store)
    original <- private[[name]]
    unlockBinding(name, private)
    assign(name, value, envir = private)
    lockBinding(name, private)
    withr::defer(
        {
            unlockBinding(name, private)
            assign(name, original, envir = private)
            lockBinding(name, private)
        },
        envir = env
    )
    invisible(NULL)
}
# }}}

# store_test__lock_check_downloader {{{
store_test__lock_check_downloader <- function(store) {
    R6::R6Class(
        "StoreLockCheckDownloader",
        public = list(
            n_workers = 1L,
            network_policy = NULL,
            node_policy = NULL,
            calls = character(),
            plan = NULL,
            assert_unlocked = function(where) {
                self$calls <- c(self$calls, where)
                testthat::expect_identical(
                    priv(store)$lock_depth,
                    0L,
                    info = where
                )
            },
            data_nodes = function(service = "HTTPServer") {
                self$assert_unlocked("data_nodes")
                NULL
            },
            record_probes = function(plan, probed = TRUE) {
                self$assert_unlocked("record_probes")
                invisible(NULL)
            },
            enqueue = function(plan, session_label = NULL) {
                self$assert_unlocked("enqueue")
                self$plan <- data.table::as.data.table(plan)
                "session-lock-check"
            },
            run = function(
                session_id = NULL,
                progress = TRUE,
                overwrite = FALSE,
                resume = TRUE,
                ...
            ) {
                self$assert_unlocked("run")
                p <- self$plan
                data.table::data.table(
                    task_id = "task-lock-check",
                    session_id = session_id,
                    file_key = p$file_key[[1L]],
                    logical_file_id = p$logical_file_id[[1L]],
                    status = "done",
                    target_path = NA_character_,
                    selected_url = p$url[[1L]],
                    checksum = p$checksum[[1L]],
                    checksum_type = p$checksum_type[[1L]],
                    filename = p$filename[[1L]]
                )
            },
            start = function(
                session_id = NULL,
                overwrite = FALSE,
                resume = TRUE,
                mode = c("process", "daemon"),
                store_path = NULL
            ) {
                self$assert_unlocked("start")
                data.table::data.table(
                    job_id = "job-lock-check",
                    session_id = session_id,
                    status = "queued"
                )
            },
            tasks = function(...) data.table::data.table()
        )
    )$new()
}
# }}}

# }}}
# EsgStore$new() {{{
test_that("EsgStore$new(), metadata, and close lifecycle", {
    dir <- withr::local_tempdir()
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    dirs <- strsplit(
        "queries dicts sources downloads extracts outputs tmp logs",
        " ",
        fixed = TRUE
    )[[1L]]
    expect_true(all(dir.exists(file.path(dir, dirs))))
    expect_equal(
        store$path,
        normalizePath(dir, winslash = "/", mustWork = TRUE)
    )
    expect_true(file.exists(store$manifest) && store$is_open)

    expected_tables <- strsplit(
        paste(
            "store_meta artifact query_run esg_query esg_file esg_query_file file_catalog",
            "esg_query_update esg_query_update_file esg_query_tag esg_query_dependency",
            "extraction_plan extraction_result extraction_grid_source epw_source",
            "epw_baseline_summary epw_climate_summary epw_climate_summary_plan",
            "epw_morph_plan epw_morph_factor epw_morph_observed_reference epw_morph_case",
            "epw_morph_diagnostic epw_morph_result epw_output",
            "shift_run shift_run_case shift_run_event shift_run_job shift_run_step"
        ),
        " ",
        fixed = TRUE
    )[[1L]]
    expect_setequal(ddb_list_tables(priv(store)$conn), expected_tables)
    for (table in c(
        "artifact",
        "esg_file",
        "file_catalog",
        "extraction_plan",
        "epw_source",
        "epw_output",
        "shift_run",
        "shift_run_event"
    )) {
        rows <- ddb_read_table(priv(store)$conn, table)
        expect_identical(nrow(rows), 0L)
        expect_gt(ncol(rows), 0L)
    }
    expect_true(
        "provenance_json" %in%
            names(ddb_read_table(priv(store)$conn, "epw_output"))
    )

    expect_null(store$get_meta("active_cmip6_index_artifact_id"))
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    expect_invisible(store$set_meta(
        "active_cmip6_index_artifact_id",
        "artifact-1"
    ))
    expect_identical(
        store$get_meta("active_cmip6_index_artifact_id"),
        "artifact-1"
    )
    store$close()
    expect_false(store$is_open)
})

test_that("site extraction cache is shared without store-local identities", {
    cache_dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    plan <- data.table::data.table(
        plan_id = "plan-a",
        query_id = "query-a",
        variable_id = "tas",
        lon = -122.375,
        lat = 37.619,
        method = "nearest",
        time_start = as.POSIXct("2041-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2060-12-31 23:59:59", tz = "UTC")
    )
    file <- data.table::data.table(
        filename = "tas_day_Model_ssp585_r1i1p1f1_gn_20410101-20601231.nc",
        checksum = "abc123",
        checksum_type = "sha256",
        size = 100,
        version = "v20260101",
        source_id = "Model",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        grid_label = "gn",
        url_opendap = "https://node-a.example/tas.nc",
        url_download = "https://node-a.example/tas.nc"
    )
    payload <- list(
        data = data.table::data.table(
            time = as.POSIXct("2050-01-01", tz = "UTC"),
            value = 280
        ),
        grid_sources = NULL,
        available_time_count = 1L,
        actual_start = as.POSIXct("2041-01-01", tz = "UTC"),
        actual_end = as.POSIXct("2060-12-31", tz = "UTC")
    )
    calls <- 0L
    generate <- function() {
        calls <<- calls + 1L
        list(
            payload = payload,
            opened = list(access_method = "OPeNDAP", target = "remote"),
            recovery_error = NULL
        )
    }

    first <- store__extract_cache_resolve(plan, file, generate)
    second_plan <- data.table::copy(plan)[, `:=`(
        plan_id = "plan-b",
        query_id = "query-b"
    )]
    second_file <- data.table::copy(file)[, `:=`(
        url_opendap = "https://node-b.example/tas.nc",
        url_download = "https://node-b.example/tas.nc"
    )]
    second <- store__extract_cache_resolve(
        second_plan,
        second_file,
        generate
    )

    expect_identical(calls, 1L)
    expect_false(first$cache_reused)
    expect_true(second$cache_reused)
    expect_identical(second$opened$access_method, "shared_cache")
    expect_identical(second$payload$data$value, 280)
})

test_that("store_reset() handles backup, force removal, guards, and missing dirs", {
    root <- withr::local_tempdir()

    # Backup and recreate an older schema store.
    dir_backup <- file.path(root, "backup-store")
    store <- EsgStore$new(dir_backup)
    writeLines("keep", file.path(dir_backup, "sentinel.txt"))
    store$close()
    conn <- ddb_connect(
        file.path(dir_backup, "manifest.duckdb"),
        read_only = FALSE
    )
    ddb_exec(
        conn,
        "UPDATE store_meta SET value = '2.7.0' WHERE key = 'schema_version'"
    )
    ddb_disconnect(conn, shutdown = TRUE)
    result <- store_reset(dir_backup)
    expect_identical(
        result$path,
        normalizePath(dir_backup, winslash = "/", mustWork = TRUE)
    )
    expect_identical(result$previous_schema, "2.7.0")
    expect_identical(result$schema, STORE_SCHEMA_VERSION)
    expect_identical(result$action, "backed_up")
    expect_true(dir.exists(result$backup_path))
    expect_true(file.exists(file.path(result$backup_path, "sentinel.txt")))
    expect_false(dir.exists(file.path(
        result$backup_path,
        "manifest.duckdb.lock"
    )))
    current <- EsgStore$new(dir_backup, create = FALSE)
    withr::defer(current$close())
    expect_identical(current$get_meta("schema_version"), STORE_SCHEMA_VERSION)

    # Permanent removal requires force = TRUE.
    dir_force <- file.path(root, "force-store")
    s_force <- EsgStore$new(dir_force)
    sentinel <- file.path(dir_force, "sentinel.txt")
    writeLines("remove", sentinel)
    s_force$close()
    expect_error(
        store_reset(dir_force, backup = FALSE),
        "Permanent store removal requires explicit confirmation"
    )
    res_force <- store_reset(dir_force, backup = FALSE, force = TRUE)
    expect_null(res_force$backup_path)
    expect_identical(res_force$action, "removed")
    expect_false(file.exists(sentinel))
    s_rebuilt <- EsgStore$new(dir_force, create = FALSE)
    withr::defer(s_rebuilt$close())
    expect_identical(s_rebuilt$get_meta("schema_version"), STORE_SCHEMA_VERSION)

    # Refuses non-store and protected directories.
    dir_non <- file.path(root, "not-a-store")
    dir.create(dir_non)
    writeLines("unrelated", file.path(dir_non, "keep.txt"))
    expect_error(store_reset(dir_non), "not an epwshiftr store")
    expect_true(file.exists(file.path(dir_non, "keep.txt")))
    expect_error(store_reset(getwd()), "protected or overly broad")

    # Initializes a missing directory.
    dir_new <- file.path(root, "missing-store")
    res_new <- store_reset(dir_new)
    expect_identical(res_new$action, "created")
    expect_null(res_new$backup_path)
    expect_true(file.exists(file.path(dir_new, "manifest.duckdb")))
})
# }}}
# EsgStore$downloader() {{{
test_that("EsgStore$downloader()", {
    dir <- withr::local_tempdir()
    store <- store_test__new(dir = dir)
    dl <- store$downloader(n_workers = 0L)
    expect_s3_class(dl, "Downloader")
    expect_equal(
        dl$data_dir,
        normalizePath(
            file.path(dir, "downloads"),
            mustWork = TRUE,
            winslash = "/"
        )
    )
    expect_equal(
        dl$tmp_dir,
        normalizePath(
            file.path(dir, "tmp", "downloads"),
            mustWork = TRUE,
            winslash = "/"
        )
    )
    expect_equal(
        dl$manifest,
        normalizePath(
            file.path(dir, "downloads", "_downloader", "manifest.duckdb"),
            mustWork = FALSE,
            winslash = "/"
        )
    )
    expect_true(file.exists(dl$manifest))
    on.exit(priv(dl)$disconnect_manifest(), add = TRUE)
    # Inspect the persisted configuration only after releasing its writer.
    priv(dl)$disconnect_manifest()
    dl_conn <- ddb_connect(dl$manifest, read_only = TRUE)
    on.exit(ddb_disconnect(dl_conn, shutdown = TRUE), add = TRUE)
    expect_true("download_config" %in% ddb_list_tables(dl_conn))
    dl_config <- ddb_read_table(dl_conn, "download_config")
    expect_equal(nrow(dl_config), 1L)
    expect_equal(dl_config$config_id, "default")
})
# }}}
# EsgStore query registry, tracking, tags, and dependency graph {{{
test_that("EsgStore query registry, tracking, tags, and dependency graph", {
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(2L)
    query_id <- store$add_query(query, label = "ssp585 tas", track = TRUE)
    expect_type(query_id, "character")
    expect_identical(store$add_query(query), query_id)

    artifacts <- ddb_read_table(priv(store)$conn, "artifact")
    expect_equal(
        nrow(artifacts[
            artifacts$query_id == query_id & artifacts$kind == "query",
        ]),
        1L
    )

    queries <- store$queries()
    expect_equal(nrow(queries), 1L)
    expect_identical(queries$query_id[[1L]], query_id)
    expect_identical(queries$label[[1L]], "ssp585 tas")
    expect_true(queries$tracked[[1L]])
    expect_true(file.exists(file.path(store$path, queries$query_file[[1L]])))
    expect_true(grepl("ssp585", queries$parameter_json[[1L]], fixed = TRUE))
    expect_equal(nrow(store$queries(tracked = TRUE)), 1L)
    expect_equal(nrow(store$queries(tracked = FALSE)), 0L)

    expect_invisible(store$untrack_query(query_id))
    expect_false(store$queries()$tracked[[1L]])
    expect_equal(nrow(store$queries(tracked = FALSE)), 1L)
    expect_invisible(store$track_query(query_id))
    expect_true(store$queries()$tracked[[1L]])
    expect_error(store$track_query("missing-query"), "was not found")

    tags <- store$tag_query(query_id, c("ssp585", "tas"))
    expect_setequal(tags$tag, c("ssp585", "tas"))
    expect_identical(store$query_tags(query_id)$query_id[[1L]], query_id)
    expect_equal(nrow(store$untag_query(query_id, "tas")), 1L)

    child_query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("pr")$limit(1L)
    child_id <- store$add_query(child_query, label = "child", track = TRUE)
    edges <- store$require_query(child_id, query_id)
    expect_equal(edges$query_id[[1L]], child_id)
    expect_true(
        child_id %in%
            store$query_graph(query_id, direction = "children")$query_id
    )
    expect_equal(nrow(store$unrequire_query(child_id, query_id)), 0L)
})
# }}}
# EsgStore$preview_update_queries() {{{
test_that("EsgStore$preview_update_queries()", {
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(1L)
    query_id <- store$add_query(query, label = "preview tas", track = TRUE)
    qid <- query_id
    before_query <- store$queries()[query_id == qid]

    file_docs <- store_test__file_docs(path = "preview-update.nc")
    file_docs$master_id <- "CMIP6.mock.preview-update"
    file_docs$tracking_id <- "hdl:21.14100/preview-update"
    file_docs$latest <- TRUE
    file_docs$retracted <- FALSE
    store_test__mock_collect(store_test__dataset_docs(), file_docs)

    preview <- store$preview_update_queries(query_id = query_id)
    expect_equal(preview$query_id, query_id)
    expect_equal(preview$file_total, 1L)
    expect_equal(preview$new_count, 1L)
    expect_equal(preview$bytes_total, 123)
    expect_equal(preview$bytes_new, 123)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_file")), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_update")), 0L)
    expect_equal(
        nrow(ddb_read_table(priv(store)$conn, "esg_query_update_file")),
        0L
    )
    expect_identical(
        store$queries()[query_id == qid]$last_checked_at[[1L]],
        before_query$last_checked_at[[1L]]
    )

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
# EsgStore$update_queries() / query_files() / query_updates() / query_changes() {{{
test_that("EsgStore$update_queries(), query_files(), query_updates(), and query_changes()", {
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(2L)
    query_id <- store$add_query(query, track = TRUE)

    file_one <- store_test__file_docs(
        path = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    )
    file_one$tracking_id <- "hdl:21.14100/mock-file-1"
    file_one$master_id <- "CMIP6.mock.master.file-1"
    file_one$latest <- TRUE
    file_one$retracted <- FALSE
    file_two <- store_test__file_docs(
        path = "tas_day_EC-Earth3_ssp585_r2i1p1f1_gr_20600101-20601231.nc"
    )
    file_two$tracking_id <- "hdl:21.14100/mock-file-2"
    file_two$master_id <- "CMIP6.mock.master.file-2"
    file_two$latest <- TRUE
    file_two$retracted <- TRUE
    first_files <- data.table::rbindlist(list(file_one, file_two), fill = TRUE)
    second_files <- data.table::rbindlist(list(file_one), fill = TRUE)
    store_test__mock_collect(
        store_test__dataset_docs(size = 2, access = c("OPENDAP", "HTTPServer")),
        function(n) if (identical(n, 1L)) first_files else second_files
    )

    first_links <- store$update_queries()
    expect_equal(nrow(first_links), 2L)
    expect_true("update_id" %in% names(first_links))
    expect_setequal(first_links$change_type, "new")
    expect_equal(nrow(store$query_files(query_id, status = "current")), 1L)
    expect_equal(nrow(store$query_files(query_id, status = "retracted")), 1L)

    conn <- priv(store)$conn
    files <- ddb_read_table(conn, "esg_file")
    expect_equal(nrow(files), 2L)
    expect_setequal(
        files$file_key,
        c("master:CMIP6.mock.master.file-1", "master:CMIP6.mock.master.file-2")
    )
    expect_equal(nrow(ddb_read_table(conn, "esg_query_file")), 2L)
    catalog <- ddb_read_table(conn, "file_catalog")
    expect_equal(nrow(catalog), 1L)
    expect_identical(catalog$file_key[[1L]], "master:CMIP6.mock.master.file-1")

    second_links <- store$update_queries(query_id = query_id)
    status_by_file <- stats::setNames(
        second_links$status,
        second_links$file_key
    )
    expect_identical(
        status_by_file[["master:CMIP6.mock.master.file-1"]],
        "current"
    )
    expect_identical(
        status_by_file[["master:CMIP6.mock.master.file-2"]],
        "missing"
    )
    change_by_file <- stats::setNames(
        second_links$change_type,
        second_links$file_key
    )
    expect_identical(
        change_by_file[["master:CMIP6.mock.master.file-1"]],
        "current"
    )
    expect_identical(
        change_by_file[["master:CMIP6.mock.master.file-2"]],
        "stale"
    )
    expect_false(is.na(store$queries()$last_checked_at[[1L]]))

    updates <- store$query_updates(query_id)
    latest <- store$query_updates(query_id, latest = TRUE)
    expect_equal(nrow(updates), 2L)
    expect_equal(latest$stale_count, 1L)
    expect_equal(
        nrow(store$query_changes(
            update_id = latest$update_id,
            change_type = "stale"
        )),
        1L
    )
})

test_that("EsgStore query update and download workflows release store lock around external work", {
    run_case <- function(fn, expect_calls, forbid_run = FALSE) {
        s <- store_test__new()
        qid <- s$add_query(
            esg_query("https://example.org")$experiment_id(
                "ssp585"
            )$variable_id("tas")$limit(1L),
            track = TRUE
        )
        fd <- store_test__file_docs(path = "lock-check.nc")
        fd$master_id <- "CMIP6.mock.lock-check"
        fd$tracking_id <- "hdl:21.14100/lock-check"
        fd$latest <- TRUE
        fd$retracted <- FALSE
        store_test__mock_collect(
            store_test__dataset_docs(),
            fd,
            on_call = function(p) {
                expect_identical(
                    priv(s)$lock_depth,
                    0L,
                    info = "query__collect"
                )
            }
        )
        dl <- store_test__lock_check_downloader(s)
        res <- fn(s, qid, dl)
        expect_true(all(expect_calls %in% dl$calls))
        if (forbid_run) {
            expect_false("run" %in% dl$calls)
        }
        latest_sid <- s$query_updates(qid, latest = TRUE)$download_session_id
        list(res = res, latest_sid = latest_sid)
    }

    r1 <- run_case(
        function(s, q, dl) {
            s$update_queries(
                query_id = q,
                enqueue = TRUE,
                downloader = dl,
                replica = "current",
                probe = FALSE
            )
        },
        c("data_nodes", "record_probes", "enqueue")
    )
    expect_equal(nrow(r1$res), 1L)
    expect_equal(r1$res$download_session_id, "session-lock-check")
    expect_equal(r1$latest_sid, "session-lock-check")

    r2 <- run_case(
        function(s, q, dl) {
            s$download_query(
                q,
                downloader = dl,
                replica = "current",
                run = FALSE,
                probe = FALSE
            )
        },
        c("data_nodes", "record_probes", "enqueue"),
        forbid_run = TRUE
    )
    expect_equal(r2$res, "session-lock-check")
    expect_equal(r2$latest_sid, "session-lock-check")

    r3 <- run_case(
        function(s, q, dl) {
            s$download_query(
                q,
                downloader = dl,
                replica = "current",
                run = TRUE,
                probe = FALSE
            )
        },
        c("record_probes", "enqueue", "run")
    )
    expect_equal(r3$res, "session-lock-check")

    r4 <- run_case(
        function(s, q, dl) {
            s$download_query(
                q,
                downloader = dl,
                replica = "current",
                run = TRUE,
                background = TRUE,
                probe = FALSE
            )
        },
        c("record_probes", "enqueue", "start"),
        forbid_run = TRUE
    )
    expect_equal(r4$res$job_id, "job-lock-check")
})
# }}}
# EsgStore$download_preflight() {{{
test_that("EsgStore$download_preflight()", {
    src <- tempfile(fileext = ".nc")
    writeLines("preflight netcdf placeholder", src)
    on.exit(unlink(src), add = TRUE)
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(1L)
    query_id <- store$add_query(query, label = "preflight tas", track = TRUE)
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
    store_test__mock_collect(store_test__dataset_docs(), file_docs)

    dl <- store$downloader(n_workers = 0L)
    preflight <- store$download_preflight(
        query_id,
        downloader = dl,
        replica = "current",
        probe = FALSE
    )
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
# EsgStore download layout & preflight issues {{{
test_that("EsgStore download layout policy, DRS paths, and collision preflight", {
    store <- store_test__new()
    expect_equal(store$download_layout()$layout, "flat")
    store$set_download_layout(layout = "drs")
    expect_equal(store$download_layout()$layout, "drs")

    q1 <- store$add_query(
        esg_query("https://example.org")$experiment_id("ssp585")$variable_id(
            "tas"
        )$limit(1L),
        track = TRUE
    )
    store_test__mock_collect(
        store_test__dataset_docs(),
        store_test__file_docs(path = "layout.nc")
    )
    dl <- store$downloader(n_workers = 0L)
    session_id <- store$download_query(
        q1,
        downloader = dl,
        replica = "current",
        run = FALSE,
        probe = FALSE
    )
    tasks <- dl$tasks(session_id = session_id)
    expect_equal(
        tasks$subdir,
        file.path(
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
        )
    )
    expect_match(
        tasks$target_path,
        "downloads/CMIP6/ScenarioMIP/.*/v20260101/layout[.]nc$"
    )

    q2 <- store$add_query(
        esg_query("https://example.org")$experiment_id("ssp585")$variable_id(
            "tas"
        )$limit(2L),
        track = TRUE
    )
    first <- store_test__file_docs(path = "collision.nc")
    second <- store_test__file_docs(path = "collision.nc")
    second$id <- "collision-second.nc|dataset-1"
    second$master_id <- "collision-second.master"
    second$tracking_id <- "hdl:21.14100/collision-second"
    second$checksum <- "def"
    file_docs <- rbind(first, second)
    store_test__mock_collect(store_test__dataset_docs(size = 2), function(n) {
        file_docs
    })

    store$set_download_layout(layout = "flat", collision = "suffix")
    preflight <- store$download_preflight(
        q2,
        replica = "current",
        probe = FALSE
    )
    expect_equal(preflight$summary$target_path_collision_count, 1L)
    expect_true(all(preflight$candidates$target_path_collision))
    expect_true(all(grepl("file=", preflight$candidates$subdir, fixed = TRUE)))

    store$set_download_layout(
        layout = "drs",
        missing = "fallback",
        collision = "suffix"
    )
    file_docs$activity_id <- NA_character_
    preflight <- store$download_preflight(
        q2,
        replica = "current",
        probe = FALSE
    )
    expect_equal(preflight$summary$missing_layout_field_count, 2L)
    expect_true(all(startsWith(preflight$candidates$subdir, "datasets/")))
})
# }}}
# EsgStore downloaded-query workflow, status, report, and retry {{{
test_that("EsgStore downloaded-query workflow, status, report, and retry", {
    src <- tempfile(fileext = ".nc")
    writeLines("tracked query netcdf placeholder", src)
    on.exit(unlink(src), add = TRUE)
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(1L)
    query_id <- store$add_query(query, track = TRUE)

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
    store_test__mock_collect(store_test__dataset_docs(), file_docs)

    dl <- store$downloader(n_workers = 0L, retries = 1L)
    session_id <- store$download_query(
        query_id,
        downloader = dl,
        replica = "current",
        probe = FALSE,
        progress = FALSE
    )

    expect_equal(dl$tasks(session_id = session_id)$status, "done")
    qfiles <- store$query_files(query_id, status = "current")
    expect_equal(nrow(qfiles), 1L)
    expect_false(is.na(qfiles$local_path[[1L]]))
    expect_true(file.exists(file.path(store$path, qfiles$local_path[[1L]])))
    expect_false(is.na(qfiles$local_artifact_id[[1L]]))

    status <- store$download_status(query_id, downloader = dl)
    expect_equal(status$status, "done")
    expect_equal(status$query_file_status, "current")

    qstatus <- store$query_status(query_id, downloader = dl)
    expect_equal(qstatus$file_total, 1L)
    expect_equal(qstatus$file_current, 1L)
    expect_equal(qstatus$download_done, 1L)
    expect_equal(qstatus$local_available, 1L)
    expect_true(qstatus$complete)

    report <- store$workflow_report(query_id, downloader = dl)
    expect_named(
        report,
        c("summary", "updates", "changes", "downloads", "nodes")
    )
    expect_equal(report$summary$query_id, query_id)
    expect_equal(report$summary$last_download_session_id, session_id)
    expect_equal(report$summary$download_retryable, 0)
    expect_equal(report$changes$change_type, "new")
    expect_equal(report$downloads$status, "done")
    expect_equal(report$downloads$query_file_status, "current")

    expect_equal(
        nrow(store$retry_downloads(query_id, downloader = dl, run = FALSE)),
        0L
    )

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

    # A failed task read must not be reported as an empty successful result.
    unreadable <- list(tasks = function(...) stop("task read failed"))
    expect_error(
        store$workflow_status(query_id, downloader = unreadable),
        "task read failed"
    )
    expect_error(
        store$download_status(query_id, downloader = unreadable),
        "task read failed"
    )
})
# }}}
# EsgStore$remove_query() / prune_orphans() / remove_files() {{{
test_that("EsgStore$remove_query(), prune_orphans(), and remove_files()", {
    store <- store_test__new()
    query <- esg_query("https://example.org")$experiment_id(
        "ssp585"
    )$variable_id("tas")$limit(1L)
    query_id <- store$add_query(query, track = TRUE)
    docs <- store_test__file_docs(path = "remove-query.nc")
    docs$master_id <- "CMIP6.mock.remove-query"
    priv(store)$update_query_files(query_id, store_test__result(docs = docs))
    file_key <- store$query_files(query_id)$file_key[[1L]]

    removed <- store$remove_query(query_id, delete = "none")
    expect_equal(removed$query_id, query_id)
    expect_equal(removed$removed_file_links, 1L)
    expect_equal(nrow(store$queries()), 0L)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "esg_query_file")), 0L)
    expect_equal(store$prune_orphans(delete_local = FALSE)$file_key, file_key)

    s2 <- store_test__new()
    linked <- store_test__link_local_file(
        s2,
        "remove-file.nc",
        label = "remove file test"
    )
    expect_error(s2$remove_files(linked$file_key), "still linked")
    res <- s2$remove_files(linked$file_key, delete_local = TRUE, force = TRUE)
    expect_equal(res$file_key, linked$file_key)
    expect_true(res$deleted_local)
    expect_false(file.exists(linked$local_file))
    conn <- priv(s2)$conn
    expect_equal(nrow(ddb_read_table(conn, "esg_file")), 0L)
    expect_equal(nrow(ddb_read_table(conn, "file_catalog")), 0L)
    expect_equal(nrow(ddb_read_table(conn, "artifact")), 1L)
})
# }}}
# EsgStore$storage_report() / cleanup_downloads() {{{
test_that("EsgStore$storage_report() and cleanup_downloads()", {
    store <- store_test__new()
    untracked_file <- file.path(store$path, "downloads", "untracked.nc")
    tmp_file <- file.path(store$path, "tmp", "downloads", "task.part")
    dir.create(dirname(untracked_file), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(tmp_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("untracked netcdf placeholder", untracked_file)
    writeLines("partial download placeholder", tmp_file)

    report <- store$storage_report(detail = TRUE)
    expect_named(
        report,
        c(
            "summary",
            "downloads",
            "registered",
            "untracked_files",
            "missing_records",
            "tmp",
            "orphan_records"
        )
    )
    expect_equal(report$summary$untracked_file_count, 1L)
    expect_equal(report$summary$tmp_file_count, 1L)
    expect_equal(report$summary$registered_file_count, 0L)
    expect_true(
        store_rel_path(untracked_file, store$path) %in%
            report$untracked_files$relative_path
    )
    expect_true(
        store_rel_path(tmp_file, store$path) %in% report$tmp$relative_path
    )

    dry <- store$cleanup_downloads(
        scope = c("tmp", "untracked_files"),
        dry_run = TRUE
    )
    expect_equal(sort(dry$scope), c("tmp", "untracked_files"))
    expect_true(all(dry$dry_run))
    expect_true(file.exists(untracked_file))
    expect_true(file.exists(tmp_file))

    cleaned <- store$cleanup_downloads(
        scope = c("tmp", "untracked_files"),
        dry_run = FALSE
    )
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
test_that("EsgStore$validate_files() reports integrity, artifact, and layout issues", {
    store <- store_test__new()
    linked <- store_test__link_local_file(
        store,
        "validate-file.nc",
        artifact_rel = file.path("downloads", "artifact-file.nc"),
        content = "local placeholder",
        label = "validate file test"
    )
    untracked_file <- file.path(
        store$path,
        "downloads",
        "untracked-validate.nc"
    )
    writeLines("untracked placeholder", untracked_file)
    conn <- priv(store)$conn
    catalog_before <- ddb_read_table(conn, "file_catalog")
    artifacts_before <- ddb_read_table(conn, "artifact")

    no_hash <- store$validate_files(
        query_id = linked$query_id,
        checksum = FALSE
    )
    expect_named(
        no_hash,
        c("summary", "files", "artifacts", "untracked", "actions")
    )
    expect_equal(no_hash$summary$file_count, 1L)
    expect_equal(no_hash$summary$bad_size_count, 1L)
    expect_equal(no_hash$summary$bad_checksum_count, 0L)
    expect_equal(no_hash$summary$artifact_mismatch_count, 1L)
    expect_equal(no_hash$summary$untracked_file_count, 1L)
    expect_true(no_hash$files$exists)
    expect_false(no_hash$files$size_ok)
    expect_true(is.na(no_hash$files$checksum_ok))
    expect_false(no_hash$files$artifact_path_matches)
    expect_true(
        store_rel_path(untracked_file, store$path) %in%
            no_hash$untracked$relative_path
    )

    with_hash <- store$validate_files(
        query_id = linked$query_id,
        checksum = TRUE
    )
    expect_equal(with_hash$summary$bad_checksum_count, 1L)
    expect_false(with_hash$files$checksum_ok)
    expect_false(is.na(with_hash$files$checksum_actual))
    expect_equal(ddb_read_table(conn, "file_catalog"), catalog_before)
    expect_equal(ddb_read_table(conn, "artifact"), artifacts_before)
    expect_true(all(file.exists(c(
        linked$local_file,
        linked$artifact_file,
        untracked_file
    ))))

    s2 <- store_test__new()
    misplaced <- store_test__link_local_file(
        s2,
        "layout-target.nc",
        local_rel = file.path("downloads", "wrong-layout.nc"),
        content = "layout placeholder",
        label = "validate layout test"
    )
    layout <- s2$validate_files(query_id = misplaced$query_id, layout = TRUE)
    expect_equal(layout$summary$layout_mismatch_count, 1L)
    expect_true("move_to_layout" %in% layout$actions$action)
    expect_match(
        layout$actions[action == "move_to_layout"]$to_path,
        "downloads/layout-target[.]nc$"
    )

    unlink(misplaced$local_file)
    missing <- s2$validate_files(query_id = misplaced$query_id, layout = TRUE)
    expect_equal(missing$summary$missing_file_count, 1L)
    expect_true(all(
        c("clear_missing_local_ref", "remove_missing_artifact") %in%
            missing$actions$action
    ))
    expect_false(file.exists(misplaced$local_file))
})
# }}}
# EsgStore$repair_files() / cleanup_downloads(missing_records) {{{
test_that("EsgStore$repair_files() and cleanup_downloads(scope = 'missing_records')", {
    expect_refs_cleared <- function(conn, aid) {
        for (tbl in c("file_catalog", "esg_file")) {
            r <- ddb_read_table(conn, tbl)
            expect_true(
                is.na(r$local_path[[1L]]) && is.na(r$local_artifact_id[[1L]])
            )
        }
        expect_false(aid %in% ddb_read_table(conn, "artifact")$artifact_id)
    }

    # 1. Clear missing records via repair_files().
    s1 <- store_test__new()
    m1 <- store_test__link_local_file(
        s1,
        "repair-missing.nc",
        content = "missing repair placeholder"
    )
    unlink(m1$local_file)
    c1 <- priv(s1)$conn
    acts <- s1$validate_files(query_id = m1$query_id)$actions
    dry <- s1$repair_files(acts, dry_run = TRUE)
    expect_true(all(dry$dry_run) && !any(dry$done))
    expect_false(is.na(ddb_read_table(c1, "file_catalog")$local_path[[1L]]))
    expect_true(m1$artifact_id %in% ddb_read_table(c1, "artifact")$artifact_id)
    expect_true(all(s1$repair_files(acts, dry_run = FALSE)$done))
    expect_refs_cleared(c1, m1$artifact_id)

    # 2. Move layout mismatches via repair_files().
    s2 <- store_test__new()
    m2 <- store_test__link_local_file(
        s2,
        "repair-layout.nc",
        local_rel = file.path(
            "downloads",
            "old-layout",
            "repair-layout-old.nc"
        ),
        content = "layout repair placeholder"
    )
    target <- file.path(s2$path, "downloads", "repair-layout.nc")
    move <- s2$validate_files(query_id = m2$query_id)$actions[
        action == "move_to_layout"
    ]
    expect_equal(nrow(move), 1L)
    dry_m <- s2$repair_files(move, dry_run = TRUE)
    expect_true(
        dry_m$dry_run &&
            !dry_m$done &&
            file.exists(m2$local_file) &&
            !file.exists(target)
    )
    expect_true(s2$repair_files(move, dry_run = FALSE)$done)
    expect_true(
        !file.exists(m2$local_file) &&
            file.exists(target) &&
            !dir.exists(dirname(m2$local_file))
    )
    c2 <- priv(s2)$conn
    rel <- store_rel_path(target, s2$path)
    expect_equal(ddb_read_table(c2, "file_catalog")$local_path[[1L]], rel)
    expect_equal(ddb_read_table(c2, "esg_file")$local_path[[1L]], rel)
    art2 <- ddb_read_table(c2, "artifact")
    expect_equal(art2$relative_path[art2$artifact_id == m2$artifact_id], rel)

    # 3. Clean up missing records via cleanup_downloads(scope = 'missing_records').
    s3 <- store_test__new()
    m3 <- store_test__link_local_file(s3, "missing-record.nc")
    unlink(m3$local_file)
    c3 <- priv(s3)$conn
    rep3 <- s3$storage_report(detail = TRUE)
    expect_gt(rep3$summary$missing_record_count, 0L)
    expect_true(m3$artifact_id %in% rep3$missing_records$artifact_id)
    expect_true(m3$file_key %in% rep3$missing_records$file_key)
    expect_true(all(
        s3$cleanup_downloads(scope = "missing_records", dry_run = TRUE)$dry_run
    ))
    expect_false(is.na(ddb_read_table(c3, "file_catalog")$local_path[[1L]]))
    expect_true(m3$artifact_id %in% ddb_read_table(c3, "artifact")$artifact_id)
    expect_true(all(
        s3$cleanup_downloads(
            scope = "missing_records",
            dry_run = FALSE
        )$record_removed
    ))
    expect_refs_cleared(c3, m3$artifact_id)
})
# }}}
# EsgStore$add_files() {{{
test_that("EsgStore$add_files() catalogs File/Aggregation records, deduplicates replicas, and keeps publish_path", {
    # 1. File records with context & publish_path.
    s1 <- store_test__new()
    docs <- store_test__file_docs()
    docs$publish_path <- "/css03_data/CMIP6/ScenarioMIP/mock/tas.nc"
    files <- store_test__result(
        docs = docs,
        context = list(
            time_filter = list(
                start = "2060-01-01T00:00:00Z",
                stop = "2060-12-31T23:59:59Z",
                method = "drs"
            )
        )
    )
    query_id <- s1$add_files(files, label = "cmip6 test")
    conn1 <- priv(s1)$conn
    runs <- ddb_read_table(conn1, "query_run")
    catalog <- ddb_read_table(conn1, "file_catalog")
    artifacts <- ddb_read_table(conn1, "artifact")
    expect_match(query_id, "^[0-9a-f]{64}$")
    expect_equal(nrow(runs), 1L)
    expect_equal(runs$label, "cmip6 test")
    expect_equal(runs$result_type, "File")
    expect_equal(runs$time_filter_method, "drs")
    query_file <- store_abs_path(runs$query_file[[1L]], root = s1$path)
    expect_true(file.exists(query_file))
    expect_equal(nrow(catalog), 1L)
    expect_match(catalog$file_key, "^(master|tracking|checksum|id|fallback):")
    expect_equal(catalog$query_id, query_id)
    expect_equal(catalog$source_id, "EC-Earth3")
    expect_equal(catalog$experiment_id, "ssp585")
    expect_equal(
        catalog$url_opendap,
        "https://example.org/dods/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    )
    expect_equal(
        catalog$url_download,
        "https://example.org/fileServer/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    )
    expect_equal(nrow(artifacts), 1L)
    expect_equal(artifacts$kind, "query")
    expect_true(file.exists(s1$artifact_path(artifacts$artifact_id)))
    loaded <- esg_result("file")$load(query_file)
    expect_true("publish_path" %in% loaded$fields)
    expect_identical(loaded$publish_path, docs$publish_path)
    expect_false("publish_path" %in% names(catalog))

    # 2. Replica deduplication prefers master node.
    s2 <- store_test__new()
    replica <- store_test__file_docs(
        opendap_url = "https://replica.example.org/dods/tas.nc",
        download_url = "https://replica.example.org/fileServer/tas.nc"
    )
    replica$id <- "tas-replica|dataset-1"
    replica$master_id <- NA_character_
    replica$tracking_id <- NA_character_
    replica$checksum <- NA_character_
    replica$replica <- TRUE
    replica$data_node <- "replica.example.org"
    master <- store_test__file_docs(
        opendap_url = "https://master.example.org/dods/tas.nc",
        download_url = "https://master.example.org/fileServer/tas.nc"
    )
    master$id <- "tas-master|dataset-1"
    master$replica <- FALSE
    master$data_node <- "master.example.org"
    q2 <- s2$add_files(
        store_test__result(
            docs = data.table::rbindlist(list(replica, master), fill = TRUE)
        ),
        label = "cmip6 duplicate replica test"
    )
    conn2 <- priv(s2)$conn
    cat2 <- ddb_read_table(conn2, "file_catalog")
    expect_equal(nrow(cat2), 1L)
    expect_equal(nrow(ddb_read_table(conn2, "esg_file")), 1L)
    expect_equal(nrow(ddb_read_table(conn2, "esg_query_file")), 1L)
    expect_equal(cat2$query_id, q2)
    expect_equal(cat2$data_node, "master.example.org")
    expect_equal(
        cat2$url_download,
        "https://master.example.org/fileServer/tas.nc"
    )

    # 3. Aggregation records are idempotent.
    s3 <- store_test__new()
    aggs <- store_test__result(
        type = "Aggregation",
        docs = store_test__file_docs(
            "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20610101-20611231.nc"
        )
    )
    q3_1 <- s3$add_files(aggs)
    expect_identical(s3$add_files(aggs), q3_1)
    conn3 <- priv(s3)$conn
    expect_equal(nrow(ddb_read_table(conn3, "query_run")), 1L)
    expect_equal(nrow(ddb_read_table(conn3, "file_catalog")), 1L)
    expect_equal(nrow(ddb_read_table(conn3, "artifact")), 1L)
    expect_equal(ddb_read_table(conn3, "query_run")$result_type, "Aggregation")
})

test_that("EsgStore$add_files() records empty child query runs", {
    store <- store_test__new()
    empty_docs <- store_test__file_docs()[0L, ]
    for (type in c("File", "Aggregation")) {
        response <- store_test__response(empty_docs)
        response$response$docs <- list()
        result <- query_result__new(
            switch(
                type,
                File = EsgResultFile,
                Aggregation = EsgResultAggregation
            ),
            index_node = "https://example.org",
            params = store_test__params(type),
            result = response
        )
        expect_match(
            store$add_files(result, label = paste("empty", type)),
            "^[0-9a-f]{64}$"
        )
    }
    conn <- priv(store)$conn
    runs <- ddb_read_table(conn, "query_run")
    expect_equal(nrow(runs), 2L)
    expect_setequal(runs$result_type, c("File", "Aggregation"))
    expect_equal(nrow(ddb_read_table(conn, "file_catalog")), 0L)
    expect_equal(nrow(ddb_read_table(conn, "artifact")), 2L)
    validation <- store$validate()
    expect_true(all(
        validation$exists & validation$checksum_ok & validation$size_ok
    ))
})
# }}}
# EsgStore$download_files() / EsgStore$sync_downloads() {{{
test_that("EsgStore$download_files() / sync_downloads() and catalog candidate selection", {
    # 1. Direct download_files() + sync_downloads().
    src <- tempfile(fileext = ".nc")
    writeLines("local netcdf placeholder", src)
    on.exit(unlink(src), add = TRUE)
    s1 <- store_test__new()
    docs <- store_test__file_docs(
        path = "local-download.nc",
        download_url = paste0("file://", normalizePath(src, winslash = "/"))
    )
    docs$checksum <- as.character(tools::md5sum(src))
    docs$checksum_type <- "MD5"
    dl1 <- s1$downloader(n_workers = 0L, retries = 1L)
    sid1 <- s1$download_files(
        files = store_test__result(docs = docs),
        replica = "current",
        downloader = dl1,
        run = TRUE,
        session_label = "local-download",
        probe = FALSE,
        progress = FALSE
    )
    expect_equal(dl1$tasks(session_id = sid1)$status, "done")
    expect_true(file.exists(file.path(
        s1$path,
        "downloads",
        "local-download.nc"
    )))
    cat1 <- s1$query("SELECT local_path, local_artifact_id FROM file_catalog")
    expect_equal(nrow(cat1), 1L)
    expect_true(file.exists(file.path(s1$path, cat1$local_path)))
    expect_true(file.exists(s1$artifact_path(cat1$local_artifact_id)))

    # 2. Catalog candidate ranking selects newer version.
    make_cand <- function(path, tag, node, csum, ver) {
        d <- store_test__file_docs(
            path = path,
            download_url = sprintf("https://%s/fileServer/%s", node, path)
        )
        d$master_id <- paste0(sub("[.]nc$", "", path), ".", tag)
        d$tracking_id <- paste0(
            "hdl:21.14100/",
            sub("[.]nc$", "", path),
            "-",
            tag
        )
        d$checksum <- csum
        d$version <- ver
        d$data_node <- node
        d
    }
    s2 <- store_test__new()
    q2 <- s2$add_files(store_test__result(
        docs = data.table::rbindlist(
            list(
                make_cand(
                    "candidate.nc",
                    "old",
                    "old.example.org",
                    "old-checksum",
                    20250101L
                ),
                make_cand(
                    "candidate.nc",
                    "new",
                    "new.example.org",
                    "new-checksum",
                    20260101L
                )
            ),
            fill = TRUE
        )
    ))
    plan <- priv(s2)$catalog_download_plan(query_id = q2)
    dl2 <- s2$downloader(n_workers = 0L)
    sid2 <- s2$download_files(
        query_id = q2,
        downloader = dl2,
        run = FALSE,
        probe = FALSE
    )
    tasks2 <- dl2$tasks(session_id = sid2)
    expect_equal(nrow(plan), 1L)
    expect_equal(plan$data_node, "new.example.org")
    expect_equal(plan$url, "https://new.example.org/fileServer/candidate.nc")
    expect_equal(plan$candidate_count, 2L)
    expect_equal(plan$candidate_rank, 1L)
    expect_equal(plan$candidate_selection, "ranked_by_catalog_metadata")
    expect_equal(nrow(tasks2), 1L)
    expect_equal(tasks2$file_key, plan$file_key)

    # 3. Ambiguous catalog target collisions abort.
    s3 <- store_test__new()
    q3 <- s3$add_files(store_test__result(
        docs = data.table::rbindlist(
            list(
                make_cand(
                    "ambiguous.nc",
                    "a",
                    "a.example.org",
                    "aaa",
                    20260101L
                ),
                make_cand(
                    "ambiguous.nc",
                    "b",
                    "b.example.org",
                    "bbb",
                    20260101L
                )
            ),
            fill = TRUE
        )
    ))
    expect_error(
        s3$download_files(
            query_id = q3,
            downloader = s3$downloader(n_workers = 0L),
            run = FALSE,
            probe = FALSE
        ),
        "different checksums"
    )
})

test_that("EsgStore$download_files() releases store lock around downloader work", {
    docs <- store_test__file_docs(path = "lock-download-files.nc")
    docs$master_id <- "CMIP6.mock.lock-download-files"
    docs$tracking_id <- "hdl:21.14100/lock-download-files"
    docs$latest <- TRUE
    docs$retracted <- FALSE
    files <- store_test__result(docs = docs)

    store_files <- store_test__new()
    dl_files <- store_test__lock_check_downloader(store_files)
    expect_equal(
        store_files$download_files(
            files = files,
            replica = "current",
            downloader = dl_files,
            run = FALSE,
            probe = FALSE
        ),
        "session-lock-check"
    )
    expect_true(all(
        c("data_nodes", "record_probes", "enqueue") %in% dl_files$calls
    ))
    expect_false("run" %in% dl_files$calls)

    store_catalog <- store_test__new()
    query_id <- store_catalog$add_files(files)
    dl_catalog <- store_test__lock_check_downloader(store_catalog)
    expect_equal(
        store_catalog$download_files(
            query_id = query_id,
            downloader = dl_catalog,
            run = TRUE,
            probe = FALSE,
            progress = FALSE
        ),
        "session-lock-check"
    )
    expect_true(all(c("record_probes", "enqueue", "run") %in% dl_catalog$calls))
})

test_that("store download plan helpers and decoration preserve catalog identity", {
    catalog <- data.table::data.table(
        tracking_id = c("tracking-a", NA_character_, NA_character_),
        checksum = c("checksum-a", "checksum-b", NA_character_),
        filename = c("a.nc", "b.nc", NA_character_),
        esgf_id = c("esgf-a", "esgf-b", NA_character_),
        file_key = c("file-a", "file-b", "file-c"),
        dataset_id = c("dataset-a", "dataset-b", "dataset-c"),
        checksum_type = c("SHA256", "MD5", NA_character_),
        size = c("123", "invalid", NA_character_),
        url_download = c(
            "https://example.org/a.nc",
            "https://example.org/b.nc",
            NA_character_
        ),
        data_node = c("a.example.org", "b.example.org", NA_character_)
    )
    expect_identical(
        store__logical_file_id(catalog),
        c(
            "tracking:tracking-a",
            "checksum:checksum-b:invalid:b.nc",
            "file-key:file-c"
        )
    )
    plan <- store__download_plan_rows(catalog)
    expect_identical(
        names(plan),
        strsplit(
            "logical_file_id file_key esgf_id dataset_id filename subdir checksum checksum_type size url service data_node priority probe_latency probe_throughput",
            " ",
            fixed = TRUE
        )[[1L]]
    )
    expect_identical(plan$logical_file_id, store__logical_file_id(catalog))
    expect_identical(plan$size, c(123, NA_real_, NA_real_))
    expect_identical(plan$priority, 1:3)
    expect_identical(plan$service, rep("HTTPServer", 3L))
    expect_identical(
        plan$data_node,
        c("example.org", "example.org", NA_character_)
    )
    expect_true(all(
        is.na(plan$subdir) &
            is.na(plan$probe_latency) &
            is.na(plan$probe_throughput)
    ))

    store <- store_test__new()
    dec_plan <- data.table::data.table(
        logical_file_id = c("logical-a", "logical-b", "logical-c"),
        file_key = c("keep-this-key", NA_character_, NA_character_),
        esgf_id = c("esgf-a", "esgf-b", "esgf-unmatched"),
        filename = c("a.nc", "b.nc", "fallback.nc"),
        checksum = c("checksum-a", "checksum-b", "checksum-c"),
        subdir = NA_character_
    )
    file_rows <- data.table::data.table(
        file_key = c("replacement-key", "esgf-key", "checksum-key"),
        esgf_id = c("esgf-a", "esgf-b", "different-esgf-id"),
        filename = c("a.nc", "b.nc", "fallback.nc"),
        checksum = c("checksum-a", "checksum-b", "checksum-c")
    )
    dec <- priv(store)$decorate_download_plan(dec_plan, file_rows)
    expect_identical(
        dec$file_key,
        c("keep-this-key", "esgf-key", "checksum-key")
    )
    expect_identical(dec$target_rel_path, dec_plan$filename)
    expect_identical(
        dec$target_path,
        file.path(store$path, "downloads", dec_plan$filename)
    )
})
# }}}
# EsgStore$plan_region() {{{
test_that("EsgStore$plan_region() creates, filters, and validates extraction plans", {
    store <- store_test__new()
    docs <- data.table::rbindlist(
        list(
            store_test__file_docs(
                "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                source_id = "EC-Earth3",
                variable_id = "tas"
            ),
            store_test__file_docs(
                "hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                source_id = "EC-Earth3",
                variable_id = "hurs"
            ),
            store_test__file_docs(
                "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                source_id = "AWI-CM-1-1-MR",
                variable_id = "tas"
            )
        ),
        fill = TRUE
    )
    qid <- store$add_files(store_test__result(docs = as.data.frame(docs)))
    yr <- c("2060-01-01", "2060-12-31")
    mk <- function(...) {
        store$plan_region(
            qid,
            103.98,
            1.37,
            yr,
            site_id = "SIN",
            filters = list(source_id = "EC-Earth3"),
            ...
        )
    }

    tas_only <- store$plan_region(
        qid,
        103.98,
        1.37,
        c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        filters = list(source_id = "EC-Earth3"),
        variable_id = "tas"
    )
    expect_equal(nrow(tas_only), 1L)
    expect_equal(tas_only$variable_id, "tas")

    plan <- mk(variable_id = c("tas", "hurs"), method = "idw")
    expect_s3_class(plan, "data.table")
    expect_equal(nrow(plan), 2L)
    expect_setequal(plan$variable_id, c("tas", "hurs"))
    expect_equal(unique(plan$site_id), "SIN")
    expect_equal(unique(plan$status), "pending")
    expect_equal(unique(plan$method), "idw")
    expect_false("nearest" %in% names(plan))
    expect_match(plan$plan_id, "^[0-9a-f]{64}$")

    nearest <- mk(variable_id = c("tas", "hurs"), method = "nearest")
    expect_false(any(nearest$plan_id %in% plan$plan_id))

    conn <- priv(store)$conn
    ddb_exec(conn, "UPDATE extraction_plan SET status = 'done'")
    again <- mk(variable_id = c("tas", "hurs"), method = "idw")
    expect_equal(nrow(ddb_read_table(conn, "extraction_plan")), 5L)
    expect_equal(unique(again$status), "done")

    expect_error(
        store$plan_region(qid, 103.98, 1.37, c("2060-12-31", "2060-01-01")),
        "greater than or equal"
    )
    expect_error(
        store$plan_region(qid, 103.98, 1.37, yr, filters = list(unknown = "x")),
        "Unknown file catalog filter"
    )
    expect_error(
        store$plan_region(
            qid,
            103.98,
            1.37,
            yr,
            filters = list(source_id = "no-match")
        ),
        "No cataloged file records match"
    )
})
# }}}
# EsgStore$extract() {{{
test_that("EsgStore$extract()", {
    store <- store_test__new()
    setup <- store_test__local_nc_plan(store)

    processed <- store$extract(plan_id = setup$plan$plan_id)
    conn <- priv(store)$conn
    plans <- ddb_read_table(conn, "extraction_plan")
    results <- ddb_read_table(conn, "extraction_result")
    grid_sources <- ddb_read_table(conn, "extraction_grid_source")
    artifacts <- ddb_read_table(conn, "artifact")

    expect_equal(processed$status, "done")
    expect_equal(plans$status, "done")
    expect_equal(plans$method, "nearest")
    expect_equal(plans$available_time_count, 2L)
    expect_equal(nrow(results), 1L)
    expect_equal(nrow(grid_sources), 1L)
    expect_equal(grid_sources$method, "nearest")
    expect_equal(grid_sources$weight, 1)
    expect_equal(grid_sources$grid_lon, 104)
    expect_equal(grid_sources$grid_lat, 1)
    expect_equal(results$year, 2060L)
    expect_equal(results$row_count, 2L)
    expect_equal(results$unique_time_count, 2L)
    expect_match(results$artifact_id, "^[0-9a-f]{64}$")
    expect_true(results$artifact_id %in% artifacts$artifact_id)
    expect_true(any(artifacts$kind == "extract"))

    parquet <- store_abs_path(results$output_path[[1L]], root = store$path)
    expect_true(file.exists(parquet))
    rows <- ddb_query(
        conn,
        sprintf(
            "SELECT site_id, source_id, experiment_id, variable_id, method, units, COUNT(*) AS n FROM read_parquet(%s) GROUP BY ALL",
            ddb_literal(conn, parquet)
        )
    )
    expect_equal(rows$site_id, "SIN")
    expect_equal(rows$source_id, "EC-Earth3")
    expect_equal(rows$experiment_id, "ssp585")
    expect_equal(rows$variable_id, "tas")
    expect_equal(rows$method, "nearest")
    expect_equal(rows$units, "K")
    expect_equal(rows$n, 2)

    validation <- store$validate()
    expect_true(all(validation$exists))
    expect_true(all(validation$checksum_ok, na.rm = TRUE))
    expect_true(all(validation$size_ok, na.rm = TRUE))

    expect_equal(store$extract(plan_id = setup$plan$plan_id)$status, "done")
    expect_equal(nrow(ddb_read_table(conn, "extraction_result")), 1L)
})

test_that("EsgStore open failures use HTTP fallback only in auto mode", {
    skip_on_cran()
    withr::local_options(epwshiftr.cache = FALSE)

    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)
    invalid <- tempfile(fileext = ".nc")
    writeBin(charToRaw("not a NetCDF file"), invalid)
    on.exit(unlink(invalid), add = TRUE)

    file <- data.table::copy(fixture$file)
    file[, `:=`(local_path = NA_character_, url_opendap = invalid)]
    download_calls <- 0L
    store_test__mock_private(
        store,
        "download_plan_file",
        function(file, overwrite = FALSE, reporter = NULL) {
            download_calls <<- download_calls + 1L
            fixture$nc
        }
    )

    result <- priv(store)$extract_one(fixture$plan, file, fallback = "auto")
    expect_identical(result$status, "done")
    expect_identical(attr(result, "access_method"), "HTTPServer")
    expect_identical(download_calls, 1L)

    error <- expect_error(
        priv(store)$extract_one(fixture$plan, file, fallback = "error"),
        class = "epwshiftr_store_access_error"
    )
    expect_identical(error$phase, "open")
    expect_identical(error$service, "OPeNDAP")
    expect_identical(error$target, invalid)
    expect_identical(download_calls, 1L)
})

test_that("EsgStore metadata and read failures retry once through HTTP", {
    skip_on_cran()
    open_dataset <- store__open_dataset

    for (phase in c("metadata", "read")) {
        fixture <- store_test__planned_extract()
        store <- fixture$store
        source <- EsgDataset$new(fixture$nc)
        source$open()
        time_info <- source$get_time_axis(index = 1L)
        source$close()
        remote <- store_test__failing_dataset(phase, time_info)
        reporter <- store_test__access_reporter()
        download_calls <- 0L

        testthat::local_mocked_bindings(store__open_dataset = function(
            target,
            service
        ) {
            if (identical(service, "OPeNDAP")) {
                list(dataset = remote, target = target, access_method = service)
            } else {
                open_dataset(target, service)
            }
        })
        store_test__mock_private(
            store,
            "download_plan_file",
            function(file, overwrite = FALSE, reporter = NULL) {
                download_calls <<- download_calls + 1L
                fixture$nc
            }
        )

        result <- priv(store)$extract_one(
            fixture$plan,
            fixture$file,
            fallback = "auto",
            reporter = reporter
        )
        expect_identical(result$status, "done", info = phase)
        expect_identical(
            attr(result, "access_method"),
            "HTTPServer",
            info = phase
        )
        expect_identical(download_calls, 1L, info = phase)
        expect_false(remote$is_open, info = phase)
        persisted <- ddb_read_table(
            priv(store)$conn,
            "extraction_result"
        )
        expect_equal(nrow(persisted), 1L, info = phase)
        expect_equal(persisted$row_count, 2L, info = phase)
        expect_equal(persisted$unique_time_count, 2L, info = phase)
        expect_length(reporter$events, 1L)
        event <- reporter$events[[1L]]
        expect_identical(event$outcome, "fallback", info = phase)
        expect_identical(event$details$access_method, "OPeNDAP", info = phase)
        expect_identical(event$details$access_phase, phase, info = phase)
        expect_identical(event$details$attempt, 1L, info = phase)
        expect_match(
            event$details$target,
            "remote\\.example\\.org",
            info = phase
        )
        expect_true(is.numeric(event$details$elapsed_seconds), info = phase)
        expect_true(nzchar(event$details$error_class), info = phase)
        expect_true(nzchar(event$details$error), info = phase)

        store$close()
        unlink(fixture$nc)
    }
})

test_that("EsgStore fallback errors retain both attempts in last_error", {
    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)
    remote <- store_test__failing_dataset("metadata")
    download_calls <- 0L
    testthat::local_mocked_bindings(store__open_dataset = function(
        target,
        service
    ) {
        list(dataset = remote, target = target, access_method = service)
    })
    store_test__mock_private(
        store,
        "download_plan_file",
        function(file, overwrite = FALSE, reporter = NULL) {
            download_calls <<- download_calls + 1L
            stop("HTTP download timed out", call. = FALSE)
        }
    )

    processed <- store$extract(
        plan_id = fixture$plan$plan_id,
        fallback = "auto"
    )
    expect_identical(processed$status, "failed")
    expect_identical(download_calls, 1L)
    expect_match(processed$last_error, "OPeNDAP extraction and HTTP fallback")
    expect_match(processed$last_error, "OPeNDAP metadata failed")
    expect_match(processed$last_error, "HTTPServer download failed")
    expect_match(processed$last_error, "remote\\.example\\.org")
    expect_match(processed$last_error, "download\\.example\\.org")
})

test_that("EsgStore fallback error mode does not download after read failure", {
    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)
    source <- EsgDataset$new(fixture$nc)
    source$open()
    time_info <- source$get_time_axis(index = 1L)
    source$close()
    remote <- store_test__failing_dataset("read", time_info)
    download_calls <- 0L
    testthat::local_mocked_bindings(store__open_dataset = function(
        target,
        service
    ) {
        list(dataset = remote, target = target, access_method = service)
    })
    store_test__mock_private(
        store,
        "download_plan_file",
        function(...) {
            download_calls <<- download_calls + 1L
            fixture$nc
        }
    )

    error <- expect_error(
        priv(store)$extract_one(
            fixture$plan,
            fixture$file,
            fallback = "error"
        ),
        class = "epwshiftr_store_access_error"
    )
    expect_identical(error$phase, "read")
    expect_identical(error$service, "OPeNDAP")
    expect_identical(download_calls, 0L)
})

test_that("EsgStore classifies persistence failures without another read", {
    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)
    file <- data.table::copy(fixture$file)
    data.table::set(file, j = "url_opendap", value = fixture$nc)
    reporter <- store_test__access_reporter()
    read_calls <- 0L
    read_dataset <- store__read_extract_dataset
    testthat::local_mocked_bindings(store__read_extract_dataset = function(
        ...
    ) {
        read_calls <<- read_calls + 1L
        read_dataset(...)
    })
    store_test__mock_private(
        store,
        "persist_extract_payload",
        function(...) stop("manifest write failed", call. = FALSE)
    )

    error <- expect_error(
        priv(store)$extract_one(
            fixture$plan,
            file,
            fallback = "auto",
            reporter = reporter
        ),
        class = "epwshiftr_store_access_error"
    )
    expect_identical(error$phase, "persist")
    expect_identical(error$service, "local")
    expect_identical(read_calls, 1L)
    expect_length(reporter$events, 1L)
    expect_identical(reporter$events[[1L]]$details$access_phase, "persist")
    expect_identical(reporter$events[[1L]]$details$attempt, 1L)
})

test_that("EsgStore$extract() persists and partitions calendar-native years", {
    store <- store_test__new()
    setup <- store_test__local_nc_plan(
        store,
        calendar = "360_day",
        n_years = 2L,
        time = c("2060-12-30T00:00:00Z", "2061-01-01T23:59:59Z")
    )
    processed <- store$extract(plan_id = setup$plan$plan_id)
    conn <- priv(store)$conn
    results <- data.table::as.data.table(ddb_read_table(
        conn,
        "extraction_result"
    ))
    data.table::setorder(results, year)

    expect_equal(processed$status, "done")
    expect_equal(processed$available_time_count, 2L)
    expect_identical(results$year, c(2060L, 2061L))
    expect_identical(results$row_count, c(1L, 1L))
    # Both the partitioned and compact layouts identify the native year.
    for (i in seq_len(nrow(results))) {
        year <- results$year[[i]]
        path <- results$output_path[[i]]
        if (startsWith(path, "extracts/by-plan/")) {
            expect_true(endsWith(path, sprintf("-%d.parquet", year)))
        } else {
            expect_true(grepl(sprintf("year=%d/", year), path, fixed = TRUE))
        }
    }

    rows <- data.table::rbindlist(lapply(results$output_path, function(path) {
        parquet <- store_abs_path(path, root = store$path)
        ddb_query(
            conn,
            sprintf(
                "SELECT year, cf_calendar, cf_year, cf_month, cf_day, cf_day_of_year, cf_year_days, cf_second_of_day, annual_phase FROM read_parquet(%s)",
                ddb_literal(conn, parquet)
            )
        )
    }))
    data.table::setorder(rows, cf_year)
    expect_equal(rows$year, c(2060L, 2061L))
    expect_identical(rows$cf_calendar, rep.int("360_day", 2L))
    expect_equal(rows$cf_year, c(2060L, 2061L))
    expect_equal(rows$cf_month, c(12L, 1L))
    expect_equal(rows$cf_day, c(30L, 1L))
    expect_equal(rows$cf_day_of_year, c(360L, 1L))
    expect_equal(rows$cf_year_days, rep.int(360L, 2L))
    expect_identical(rows$cf_second_of_day, rep.int(43200, 2L))
    expect_equal(rows$annual_phase, c(359.5 / 360, 0.5 / 360))
})

test_that("EsgStore$extract() records failed plans and detects output conflicts", {
    s1 <- store_test__new()
    setup1 <- store_test__local_nc_plan(s1, variable_id = "hurs")
    processed <- s1$extract(plan_id = setup1$plan$plan_id)
    plans <- ddb_read_table(priv(s1)$conn, "extraction_plan")
    expect_equal(processed$status, "failed")
    expect_equal(plans$status, "failed")
    expect_equal(plans$attempt_count, 1L)
    expect_match(plans$last_error, "None of the requested variable")

    s2 <- store_test__new()
    setup2 <- store_test__local_nc_plan(s2)
    expect_equal(s2$extract(plan_id = setup2$plan$plan_id)$status, "done")
    conn <- priv(s2)$conn
    output_path <- file.path(
        s2$path,
        ddb_read_table(conn, "extraction_result")$output_path[[1L]]
    )
    expect_true(file.exists(output_path))
    ddb_exec(
        conn,
        sprintf(
            "DELETE FROM extraction_result WHERE plan_id = %s",
            ddb_literal(conn, setup2$plan$plan_id[[1L]])
        )
    )
    expect_error(
        s2$extract(plan_id = setup2$plan$plan_id, overwrite = FALSE),
        "Parquet output already exists"
    )
    expect_equal(
        s2$extract(plan_id = setup2$plan$plan_id, overwrite = TRUE)$status,
        "done"
    )
    expect_equal(nrow(ddb_read_table(conn, "extraction_result")), 1L)
})
# }}}
# EsgStore post-extract inspection (summarise, coverage, assert_complete, query) {{{
test_that("EsgStore post-extract inspection (summarise, coverage, assert_complete, query)", {
    fixture <- store_test__completed_store()
    store <- fixture$store

    summary <- store$summarise()
    expect_s3_class(summary, "data.table")
    expect_equal(nrow(summary), 1L)
    expect_equal(summary$source_id, "EC-Earth3")
    expect_equal(summary$site_id, "SIN")
    expect_equal(summary$variable_id, "tas")
    expect_equal(summary$year, 2060L)
    expect_equal(summary$row_count, 2)
    expect_equal(summary$unique_time_count, 2)

    cov <- store$coverage()
    expect_s3_class(cov, "data.table")
    expect_true(cov$complete)
    expect_equal(cov$output_time_count, 2)
    expect_equal(cov$output_rows, 2)
    expect_equal(nrow(store$coverage(plan_id = "missing-plan")), 0L)
    expect_equal(store$coverage(plan_id = cov$plan_id)$plan_id, cov$plan_id)

    expect_silent(store$assert_complete())
    expect_equal(
        store$query("SELECT COUNT(*) AS n FROM extraction_result")$n,
        1
    )

    results <- ddb_read_table(priv(store)$conn, "extraction_result")
    unlink(store_abs_path(results$output_path[[1L]], root = fixture$dir))
    cov_missing <- store$coverage()
    expect_false(cov_missing$complete)
    expect_false(cov_missing$output_files_exist)
    expect_true(any(!store$validate()$exists))
    expect_error(store$assert_complete(), "incomplete")
})
# }}}

# A failed worker read must enter HTTP recovery without opening OPeNDAP again.
test_that("worker access failures preserve fallback without remote resubmission", {
    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    withr::local_options(epwshiftr.cache = FALSE)
    original <- store__open_dataset
    testthat::local_mocked_bindings(store__open_dataset = function(
        target,
        service
    ) {
        if (identical(service, "OPeNDAP")) {
            stop("Unexpected second OPeNDAP attempt")
        }
        original(target, service)
    })
    store_test__mock_private(store, "download_plan_file", function(...) {
        fixture$nc
    })
    error <- store__access_error(
        simpleError("source disconnected"),
        "read",
        "OPeNDAP",
        fixture$file$url_opendap[[1L]],
        proc.time()[["elapsed"]]
    )
    expect_error(
        priv(store)$extract_one(
            fixture$plan,
            fixture$file,
            fallback = "error",
            resolved = error
        ),
        "source disconnected"
    )
    result <- priv(store)$extract_one(
        fixture$plan,
        fixture$file,
        fallback = "auto",
        resolved = error
    )
    expect_identical(result$status, "done")
    expect_identical(attr(result, "access_method"), "HTTPServer")
})

# Disabling disk caching must not collect every site's weather array in one worker.
test_that("uncached source tasks return one site plan at a time", {
    fixture <- store_test__planned_extract()
    store <- fixture$store
    on.exit(store$close(), add = TRUE)
    on.exit(unlink(fixture$nc), add = TRUE)
    withr::local_options(epwshiftr.cache = FALSE)
    store$plan_region(
        query_id = fixture$plan$query_id[[1L]],
        lon = -106,
        lat = 41,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "USA"
    )
    task_count <- 0L
    testthat::local_mocked_bindings(source__apply = function(
        jobs,
        read,
        collect,
        reporter = NULL
    ) {
        task_count <<- length(jobs)
        for (job in rev(jobs)) {
            expect_equal(nrow(job$plans), 1L)
            data.table::set(job$file, j = "url_opendap", value = fixture$nc)
            collect(job, read(job))
        }
    })
    reporter <- store_test__access_reporter()
    progress <- integer(2L)
    count <- 0L
    reporter$check_cancel <- function(...) invisible(NULL)
    reporter$unit_started <- function(...) invisible(NULL)
    reporter$unit_completed <- function(message, current, ...) {
        count <<- count + 1L
        progress[[count]] <<- current
    }
    expected_order <- store$query("SELECT plan_id FROM extraction_plan")$plan_id
    result <- store$extract(fallback = "error", reporter = reporter)
    expect_identical(progress, 1:2)
    expect_identical(result$plan_id, expected_order)
    expect_identical(task_count, 2L)
    expect_true(all(result$status == "done"))
    expect_equal(nrow(store$query("SELECT * FROM extraction_result")), 2L)
})

# Exercise the public extraction error path. After rollback only the explicit
# failure record survives; the Parquet follows the existing overwrite policy.
test_that("extraction rolls back a failed commit and publishes once on retry", {
    local_cache_mode("off")
    fixture <- exchange_test__fixture()
    withr::defer(lapply(fixture$stores, function(store) store$close()))
    store <- fixture$stores[[1L]]
    id <- fixture$plans$plan_id[[1L]]
    before <- exchange_test__snapshot(store)
    before_file <- store$query("SELECT * FROM file_catalog ORDER BY file_key")
    fault <- exchange_test__fail_publication_commit(store, id)

    failed <- store$extract(id, fallback = "error")
    expect_identical(fault$staged$status, "done")
    expect_equal(fault$staged$results, 1L)
    expect_equal(fault$staged$artifacts, 1L)
    expect_gt(fault$staged$grid_sources, 0L)
    expect_identical(failed$status, "failed")
    expect_match(failed$last_error, "injected publication commit failure")
    expect_equal(
        failed$attempt_count,
        before$extraction_plan$attempt_count[
            before$extraction_plan$plan_id == id
        ] +
            1L
    )
    expect_identical(exchange_test__snapshot(store)[-1L], before[-1L])
    expect_identical(
        store$query("SELECT * FROM file_catalog ORDER BY file_key"),
        before_file
    )

    parquet <- list.files(store$path, pattern = "\\.parquet$", recursive = TRUE)
    expect_length(parquet, 1L)
    expect_error(
        store$extract(id, fallback = "error"),
        class = "epwshiftr_store_extract_conflict"
    )
    expect_identical(
        store$extract(id, fallback = "error", overwrite = TRUE)$status,
        "done"
    )
    ready <- exchange_test__snapshot(store)
    expect_equal(nrow(ready$extraction_result), 1L)
    expect_equal(sum(ready$artifact$kind == "extract"), 1L)
    expect_equal(nrow(ready$extraction_grid_source), fault$staged$grid_sources)
    expect_identical(
        list.files(store$path, pattern = "\\.parquet$", recursive = TRUE),
        parquet
    )
    expect_identical(store$extract(id, fallback = "error")$status, "done")
    expect_identical(exchange_test__snapshot(store), ready)
})

test_that("EsgStore$plan_region() isolates plans by query while sharing scientific cache", {
    # 1. Identical files across two queries get distinct plan_ids yet share cache.
    store <- store_test__new()
    docs <- store_test__file_docs()
    first_query_id <- store$add_files(store_test__result(docs = docs))
    second_query_id <- store$add_files(query_result__new(
        EsgResultFile,
        index_node = "https://mirror.example.org",
        params = store_test__params("File"),
        result = store_test__response(docs)
    ))
    expect_false(identical(first_query_id, second_query_id))
    first_plan <- store$plan_region(
        first_query_id,
        103.98,
        1.37,
        c("2060-01-01", "2060-12-31")
    )
    second_plan <- store$plan_region(
        second_query_id,
        103.98,
        1.37,
        c("2060-01-01", "2060-12-31")
    )
    expect_identical(unique(first_plan$query_id), first_query_id)
    expect_identical(unique(second_plan$query_id), second_query_id)
    expect_false(any(first_plan$plan_id %in% second_plan$plan_id))
    expect_identical(
        store$plan_region(
            second_query_id,
            103.98,
            1.37,
            c("2060-01-01", "2060-12-31")
        )$plan_id,
        second_plan$plan_id
    )
    expect_identical(first_plan$file_key, second_plan$file_key)
    expect_equal(nrow(ddb_read_table(priv(store)$conn, "extraction_plan")), 2L)

    cache_dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    file <- data.table::as.data.table(ddb_read_table(
        priv(store)$conn,
        "file_catalog"
    ))[1L]
    calls <- 0L
    generate <- function() {
        calls <<- calls + 1L
        list(
            payload = list(
                data = data.table::data.table(value = 280),
                grid_sources = NULL,
                available_time_count = 1L,
                actual_start = first_plan$time_start,
                actual_end = first_plan$time_stop
            ),
            opened = list(access_method = "OPeNDAP", target = "remote"),
            recovery_error = NULL
        )
    }
    expect_false(
        store__extract_cache_resolve(first_plan, file, generate)$cache_reused
    )
    cached <- store__extract_cache_resolve(second_plan, file, generate)
    expect_true(cached$cache_reused)
    expect_identical(calls, 1L)
    expect_identical(cached$payload$data$value, 280)

    # 2. Separate catalogs remain isolated by query_id.
    s2 <- store_test__new()
    q_a <- s2$add_files(store_test__result(
        docs = store_test__file_docs("first.nc")
    ))
    q_b <- s2$add_files(store_test__result(
        docs = store_test__file_docs("second.nc")
    ))
    catalog <- data.table::as.data.table(ddb_read_table(
        priv(s2)$conn,
        "file_catalog"
    ))
    for (id in c(q_a, q_b)) {
        plan <- s2$plan_region(id, 103.98, 1.37, c("2060-01-01", "2060-12-31"))
        expect_equal(nrow(plan), 1L)
        expect_identical(plan$query_id, id)
        expect_identical(
            plan$file_key,
            catalog$file_key[catalog$query_id == id]
        )
    }
    expect_error(
        s2$plan_region(
            "absent-query",
            103.98,
            1.37,
            c("2060-01-01", "2060-12-31")
        ),
        "No cataloged file records"
    )
})

# A physical catalog row may outlive the first query that added it.
test_that("File inspection follows current query membership without double counting", {
    store <- store_test__new()
    docs <- store_test__file_docs()
    add <- function(node) {
        store$add_files(query_result__new(
            EsgResultFile,
            index_node = node,
            params = store_test__params("File"),
            result = store_test__response(docs)
        ))
    }
    first <- add("https://first.example.org")
    second <- add("https://second.example.org")
    expect_false(identical(first, second))
    file_stage <- function(ids) {
        shift_stage__new(
            ShiftFiles,
            "files",
            store_path = store$path,
            ids = list(query_id = ids)
        )
    }
    individual <- lapply(c(first, second), function(id) {
        data.table::as.data.table(file_stage(id))
    })
    expect_true(all(lengths(lapply(individual, `[[`, "file_key")) == 1L))
    expect_identical(individual[[1L]]$file_key, individual[[2L]]$file_key)
    combined <- shift_inspect__file_catalog_summary(store, c(first, second))
    expect_equal(combined$file_count, 1)
    expect_equal(
        as.numeric(combined$total_size),
        as.numeric(individual[[1L]]$size)
    )
    expect_identical(add("https://second.example.org"), second)
    expect_equal(
        nrow(shift_inspect__file_catalog_preview(store, c(first, second))),
        1L
    )
})

# Use generated catalogue controls only; planning must never open their URLs.
store_test__multi_site_files <- function() {
    docs <- data.table::rbindlist(lapply(seq_len(4L), function(i) {
        esgf_test__file_docs(
            path = sprintf("generated-plan-%d.nc", i),
            opendap_url = sprintf("https://example.invalid/%d.nc", i)
        )
    }))
    esgf_test__file_result(docs)
}

# Compare the real multi-city caller against successive existing public calls,
# including preserved failed/done state and literal site identifiers.
test_that("multi-site plans match scalar plans and retain existing state", {
    files <- store_test__multi_site_files()
    old <- store_test__new()
    new <- store_test__new()
    q1 <- old$add_files(files)
    q2 <- new$add_files(files)
    sites <- data.table::data.table(
        site_id = c("same-point-a", "same-point-b", "city'quoted", "four"),
        lon = c(110, 110, -180, 360),
        lat = c(30, 30, -90, 90)
    )
    saved_sites <- data.table::copy(sites)
    time <- c("2060-01-01", "2060-12-31")
    for (method in c("nearest", "idw", "bilinear", "mean")) {
        expected <- data.table::rbindlist(lapply(
            seq_len(nrow(sites)),
            function(i) {
                old$plan_region(
                    q1,
                    sites$lon[[i]],
                    sites$lat[[i]],
                    time,
                    site_id = sites$site_id[[i]],
                    method = method
                )
            }
        ))
        actual <- new$plan_regions(q2, sites, time, method = method)
        columns <- setdiff(names(expected), c("created_at", "updated_at"))
        expect_identical(
            actual[, columns, with = FALSE],
            expected[, columns, with = FALSE]
        )
        expect_identical(sites, saved_sites)
        expect_false(anyDuplicated(actual$plan_id) > 0L)
    }
    ddb_exec(
        priv(new)$conn,
        paste0(
            "UPDATE extraction_plan SET status='failed',attempt_count=3,last_error='retained failure' WHERE plan_id=",
            ddb_literal(priv(new)$conn, actual$plan_id[[1L]])
        )
    )
    before <- new$query("SELECT * FROM extraction_plan")
    repeated <- new$plan_regions(q2, sites, time, method = "mean")
    expect_identical(new$query("SELECT * FROM extraction_plan"), before)
    row <- repeated[repeated$plan_id == actual$plan_id[[1L]]]
    expect_identical(row$status, "failed")
    expect_identical(row$attempt_count, 3L)
    expect_identical(row$last_error, "retained failure")
})

# Invalid batches fail before any rows are appended; empty input retains the
# ordinary typed plan schema without manufacturing a source or site.
test_that("multi-site planning validates whole batches before publication", {
    store <- store_test__new()
    q <- store$add_files(store_test__multi_site_files())
    sites <- data.frame(site_id = c("a", "b"), lon = c(1, 2), lat = c(3, 4))
    time <- c("2060-01-01", "2060-12-31")
    empty <- store$plan_regions(q, sites[FALSE, ], time)
    expect_s3_class(empty, "data.table")
    expect_equal(nrow(empty), 0L)
    expect_named(empty, names(store$query("SELECT * FROM extraction_plan")))
    expect_s3_class(empty$time_start, "POSIXct")
    bad_sites <- list(
        sites[, c("site_id", "lon")],
        transform(sites, site_id = c("a", "a")),
        transform(sites, lon = c(1, NA_real_)),
        transform(sites, lat = c(3, Inf)),
        transform(sites, lon = c(1, 361)),
        transform(sites, lat = c(3, -91)),
        transform(sites, site_id = c("a", NA))
    )
    for (bad in bad_sites) {
        expect_error(store$plan_regions(q, bad, time))
    }
    expect_error(
        store$plan_regions(q, sites, time, filters = list(unknown_field = "x")),
        "Unknown"
    )
    expect_error(
        store$plan_regions(q, sites, time, variable_id = "absent"),
        "variable"
    )
    expect_equal(nrow(store$query("SELECT * FROM extraction_plan")), 0L)
    keys <- store$query("SELECT file_key FROM file_catalog LIMIT 2")$file_key
    plan <- store$plan_regions(q, sites, time, filters = list(file_key = keys))
    expect_equal(nrow(plan), 4L)
    expect_setequal(plan$file_key, keys)
    expect_identical(plan$site_id, rep(sites$site_id, each = 2L))
})

# Public extraction must preserve native calendar labels and values after
# batching sites, and resume completed outputs when their source is absent.
test_that("multi-site extraction matches scalar output and reuses completed data", {
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    collect <- function(results) {
        expect_true(all(results$status %in% c("done", "empty")))
    }
    values <- function(store) {
        paths <- file.path(
            store$path,
            store$query("SELECT output_path FROM extraction_result")$output_path
        )
        store$query(paste0(
            "SELECT * FROM read_parquet([",
            paste(ddb_literal(priv(store)$conn, paths), collapse = ","),
            "]) ORDER BY site_id,cf_year,cf_month,cf_day,cf_second_of_day"
        ))
    }
    sites <- data.frame(
        site_id = c("a", "b"),
        lon = c(103.98, 104.4),
        lat = c(1.37, 2)
    )
    time <- c("2060-01-01T00:00:00Z", "2060-01-03T23:59:59Z")
    for (calendar in c("proleptic_gregorian", "365_day", "360_day")) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = calendar)
        files <- esgf_test__file_result(esgf_test__file_docs(path))
        old <- EsgStore$new(tempfile("scalar-generated-"))
        new <- EsgStore$new(tempfile("multi-generated-"))
        q1 <- old$add_files(files)
        q2 <- new$add_files(files)
        scalar <- data.table::rbindlist(lapply(
            seq_len(nrow(sites)),
            function(i) {
                old$plan_region(
                    q1,
                    sites$lon[[i]],
                    sites$lat[[i]],
                    time,
                    site_id = sites$site_id[[i]]
                )
            }
        ))
        batch <- new$plan_regions(q2, sites, time)
        collect(old$extract(plan_id = scalar$plan_id, fallback = "error"))
        collect(new$extract(plan_id = batch$plan_id, fallback = "error"))
        expect_identical(values(new), values(old))
        hashes <- new$validate()
        expect_true(all(hashes$checksum_ok))
        expect_true(file.rename(path, paste0(path, ".absent")))
        collect(new$extract(plan_id = batch$plan_id, fallback = "error"))
        expect_identical(new$validate(), hashes)
        old$close()
        new$close()
    }
})

# Build only catalogue controls: plan-source inspection must never open URLs.
store_test__source_key_files <- function() {
    docs <- data.table::rbindlist(lapply(1:3, function(i) {
        esgf_test__file_docs(
            path = sprintf("generated-source-map-%d.nc", i),
            opendap_url = sprintf("https://example.invalid/source-%d.nc", i)
        )
    }))
    esgf_test__file_result(docs)
}

# The public mapping uses normalized manifest metadata and exact input order,
# while its source keys agree with the pool's identity implementation.
test_that("plan source mapping preserves order and manifest state", {
    store <- store_test__new()
    query_id <- store$add_files(store_test__source_key_files())
    plans <- store$plan_regions(
        query_id,
        data.frame(
            site_id = c("city-a", "city-b"),
            lon = c(110, 120),
            lat = c(20, 30)
        ),
        c("2060-01-01", "2060-12-31")
    )
    before <- store$query("SELECT * FROM extraction_plan")
    requested <- rev(plans$plan_id)
    actual <- store$plan_sources(requested)
    expect_s3_class(actual, "data.table")
    expect_identical(actual$plan_id, requested)
    expect_identical(actual$file_key, rev(plans$file_key))
    catalog <- store$query("SELECT * FROM file_catalog")
    expected <- vapply(
        seq_len(nrow(actual)),
        function(i) {
            source__physical_key(catalog[
                catalog$file_key == actual$file_key[[i]]
            ])
        },
        character(1)
    )
    expect_identical(actual$source_key, expected)
    expect_equal(data.table::uniqueN(actual$source_key), 3L)
    expect_identical(store$query("SELECT * FROM extraction_plan"), before)
    expect_equal(nrow(store$query("SELECT * FROM extraction_result")), 0L)
})

# Empty/invalid/missing identifiers cannot create fake unblocked work. Catalog
# corruption must fail before a caller submits an extraction to the pool.
test_that("plan source mapping validates complete requested identities", {
    store <- store_test__new()
    expect_identical(
        store$plan_sources(character()),
        data.table::data.table(
            plan_id = character(),
            file_key = character(),
            source_key = character()
        )
    )
    for (bad in list(NA_character_, "", c("a", "a"))) {
        expect_error(store$plan_sources(bad))
    }
    expect_error(store$plan_sources("absent'quoted"), "not found")
    q <- store$add_files(store_test__source_key_files())
    plans <- store$plan_region(q, 110, 30, c("2060-01-01", "2060-12-31"))
    expect_error(
        store$plan_sources(c(plans$plan_id[[1L]], "absent")),
        "not found"
    )
    ddb_exec(
        priv(store)$conn,
        paste0(
            "UPDATE extraction_plan SET file_key='absent-file' WHERE plan_id=",
            ddb_literal(priv(store)$conn, plans$plan_id[[1L]])
        )
    )
    expect_error(store$plan_sources(plans$plan_id[[1L]]), "file catalog")
    store$close()
    expect_error(store$plan_sources(character()), "closed")
})

# Published replica access URLs do not create another physical reader slot;
# independently identified files remain distinct even within one query.
test_that("plan source mapping groups replicas by physical identity", {
    store <- EsgStore$new(tempfile("plan-source-replica-"))
    on.exit(store$close(), add = TRUE)
    q <- store$add_files(store_test__source_key_files())
    plans <- store$plan_region(q, 110, 30, c("2060-01-01", "2060-12-31"))
    catalog <- store$query("SELECT * FROM file_catalog")
    keys <- catalog$file_key[1:2]
    fields <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "table_id",
        "variable_id",
        "title",
        "version",
        "tracking_id",
        "checksum_type",
        "checksum"
    )
    for (name in fields) {
        ddb_exec(
            priv(store)$conn,
            paste0(
                "UPDATE file_catalog SET ",
                name,
                "=",
                ddb_literal(priv(store)$conn, catalog[[name]][[1L]]),
                " WHERE file_key=",
                ddb_literal(priv(store)$conn, keys[[2L]])
            )
        )
    }
    ids <- plans$plan_id[match(keys, plans$file_key)]
    mapped <- store$plan_sources(ids)
    expect_identical(mapped$source_key[[1L]], mapped$source_key[[2L]])
    other <- store$plan_sources(plans$plan_id[
        plans$file_key == catalog$file_key[[3L]]
    ])
    expect_false(other$source_key[[1L]] == mapped$source_key[[1L]])
})

# Exercise subset validation, empty selections, and literal IDs through the
# public interface, including corrupt artifacts outside the current job.
test_that("EsgStore validates requested artifacts, empty selections, and literal IDs", {
    skip_if_not_installed("duckdb")
    store <- store_test__new()
    empty <- store$validate()
    expect_identical(store$validate(character()), empty)
    expect_identical(store$artifact_path(character()), character())
    expect_error(store$validate("unknown"), "not found")

    paths <- file.path(
        store$path,
        c("first.bin", "second.bin", "unrelated.bin")
    )
    ids <- vapply(
        seq_along(paths),
        function(i) {
            writeBin(as.raw(rep(i, 32L)), paths[[i]])
            store$register_artifact("output", paths[[i]])
        },
        character(1)
    )
    expect_identical(store$validate(character()), empty)
    expect_error(store$validate("x' OR TRUE --"), "not found")
    writeBin(as.raw(rep(9L, 32L)), paths[[3L]])

    selected <- store$validate(ids[c(2L, 1L, 2L)])
    expect_identical(selected$artifact_id, ids[c(2L, 1L)])
    expect_true(all(selected$exists & selected$checksum_ok & selected$size_ok))
    expect_identical(
        selected,
        store$validate()[match(selected$artifact_id, artifact_id)]
    )
    full <- store$validate()
    expect_false(full$checksum_ok[match(ids[[3L]], full$artifact_id)])
    expect_true(full$size_ok[match(ids[[3L]], full$artifact_id)])
    expect_identical(
        store$artifact_path(ids[c(2L, 1L, 2L)]),
        paths[c(2L, 1L, 2L)]
    )
    expect_identical(store$artifact_path(ids[[1L]]), paths[[1L]])
    expect_error(store$validate(c(ids[[1L]], "unknown-id")), "not found")
    expect_error(store$artifact_path(c(ids[[1L]], "unknown-id")), "not found")
    for (bad in list(NA_character_, "", 1)) {
        expect_error(store$validate(bad))
    }
    expect_error(store$artifact_path(NULL))

    unlink(paths[[2L]])
    checked <- store$validate(ids[[2L]])
    expect_false(checked$exists)
    expect_true(is.na(checked$checksum_ok) && is.na(checked$size_ok))
})

# Long Windows roots must retain unique immutable extraction identities without
# requiring registry changes or moving previously completed Parquet files.
test_that("extraction paths compact only when Windows needs a shorter path", {
    p <- c("project=CMIP6", "source_id=MODEL", "year=2060")
    id <- strrep("a", 64L)
    out <- function(r, yr = 2060L, pid = id, win = TRUE) {
        store__extract_output_path(r, p, pid, yr, windows = win)
    }
    expect_identical(
        out("extracts"),
        file.path(
            "extracts",
            p[1L],
            p[2L],
            p[3L],
            paste0("part-", id, ".parquet")
        )
    )
    root <- paste0("C:/", strrep("r", 120L), "/extracts")
    win <- out(root)
    expect_true(nchar(out(root, win = FALSE)) > 240L && nchar(win) < 240L)
    expect_identical(
        win,
        file.path(root, "by-plan", paste0("part-", id, "-2060.parquet"))
    )
    expect_false(identical(win, out(root, yr = 2080L)))
    expect_false(identical(win, out(root, pid = strrep("b", 64L))))
})

# A real Parquet write checks the requested codec and the unchanged mask/value
# representation instead of just matching the implementation's SQL string.
test_that("store extraction Parquet uses Zstandard and retains native values", {
    store <- store_test__new()
    path <- file.path(store$path, "extracts", "test.parquet")
    input <- data.table::data.table(value = c(280.25, NA_real_, 281.5))
    priv(store)$write_parquet(input, path)
    quoted <- ddb_literal(priv(store)$conn, path)
    actual <- store$query(paste0("SELECT * FROM read_parquet(", quoted, ")"))
    codec <- store$query(paste0(
        "SELECT DISTINCT compression FROM parquet_metadata(",
        quoted,
        ")"
    ))
    expect_identical(actual$value, input$value)
    expect_identical(codec$compression, "ZSTD")
})

# On Windows, exercise the normal Store entry with a deeply nested disposable
# root and read back the actual artifact registered after extraction.
test_that("Windows Store extraction publishes and reads compact artifacts", {
    if (.Platform$OS.type != "windows") {
        output_path <- store__extract_output_path
        local_mocked_bindings(
            store__extract_output_path = function(
                extract_dir,
                partitions,
                plan_id,
                year
            ) {
                output_path(
                    extract_dir,
                    partitions,
                    plan_id,
                    year,
                    windows = TRUE
                )
            }
        )
    }
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    store <- store_test__new(
        dir = file.path(withr::local_tempdir(), strrep("r", 62L))
    )
    setup <- store_test__local_nc_plan(
        store,
        time = c("2060-01-01", "2060-01-03"),
        site_id = strrep("site", 20L)
    )
    expect_identical(
        store$extract(plan_id = setup$plan$plan_id, fallback = "error")$status,
        "done"
    )
    actual <- store$query(
        "SELECT output_path FROM extraction_result"
    )$output_path
    expect_match(actual, "by-plan")
    expect_true(file.exists(file.path(store$path, actual)))
    expect_true(all(store$validate()$checksum_ok))
})

# Serve generated NetCDF bytes with a deliberately failed DAP endpoint. The
# server owns its counters so tests distinguish reuse from another download.
store_test__fallback_app <- function(bytes, delay = 0) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$delay <- delay
    app$locals$gets <- 0L
    app$locals$dods <- 0L
    app$locals$ranges <- character()
    for (path in c(
        "/dods/source.nc",
        "/dods/source.nc.dds",
        "/dods/source.nc.das",
        "/dods/source.nc.dods"
    )) {
        app$all(path, function(req, res, locals) {
            locals$dods <- locals$dods + 1L
            res$set_status(504L)$send(
                "<html><body>504 Gateway Time-out</body></html>"
            )
        })
    }
    app$all("/files/source.nc", function(req, res, locals) {
        size <- length(locals$bytes)
        res$set_header("Content-Type", "application/x-netcdf")$set_header(
            "Accept-Ranges",
            "bytes"
        )$set_header("Last-Modified", "Mon, 01 Jan 2024 00:00:00 GMT")
        if (identical(req$method, "HEAD")) {
            return(res$set_header("Content-Length", size)$send(raw()))
        }
        locals$gets <- locals$gets + 1L
        Sys.sleep(locals$delay)
        header <- req$get_header("Range")
        if (!is.null(header) && nzchar(header)) {
            locals$ranges <- c(locals$ranges, header)
            start <- as.integer(sub("^bytes=([0-9]+)-.*$", "\\1", header))
            if (start >= size) {
                return(res$set_status(416L)$set_header(
                    "Content-Range",
                    sprintf("bytes */%d", size)
                )$send(raw()))
            }
            body <- locals$bytes[seq.int(start + 1L, size)]
            return(res$set_status(206L)$set_header(
                "Content-Range",
                sprintf("bytes %d-%d/%d", start, size - 1L, size)
            )$set_header("Content-Length", length(body))$send(body))
        }
        res$set_header("Content-Length", size)$send(locals$bytes)
    })
    app$get("/counts", function(req, res, locals) {
        res$set_header(
            "Content-Type",
            "application/json"
        )$send(jsonlite::toJSON(
            list(
                gets = locals$gets,
                dods = locals$dods,
                ranges = locals$ranges
            ),
            auto_unbox = TRUE
        ))
    })
    app$post("/fast", function(req, res, locals) {
        locals$delay <- 0
        res$send("ok")
    })
    app
}
environment(store_test__fallback_app) <- baseenv()

# Build the same public catalogue used by extraction with real size/checksum.
store_test__fallback_files <- function(
    path,
    server,
    checksum = checksum_file(path)
) {
    docs <- esgf_test__file_docs(
        path,
        opendap_url = server$url("/dods/source.nc"),
        download_url = server$url("/files/source.nc")
    )
    data.table::set(docs, j = "checksum", value = checksum)
    data.table::set(docs, j = "size", value = as.double(file.info(path)$size))
    esgf_test__file_result(docs)
}

# Poll the actual persistent pool and retain per-ticket completion ordering.
store_test__drain_fallback <- function(pool, tickets, limit = 25) {
    end <- as.double(Sys.time()) + limit
    finished <- rep(NA_real_, length(tickets))
    while (
        any(vapply(tickets, function(x) x$status == "pending", logical(1)))
    ) {
        pool$poll()
        for (i in seq_along(tickets)) {
            if (is.na(finished[[i]]) && tickets[[i]]$status != "pending") {
                finished[[i]] <- as.double(Sys.time())
            }
        }
        stopifnot(as.double(Sys.time()) < end)
        Sys.sleep(.01)
    }
    finished
}

# A failed remote read must recover inside its own reader, while another reader
# completes independently and only the owner writes the extraction manifests.
test_that("persistent extraction recovers 504 through HTTP without blocking peers", {
    skip_if_not_installed("webfakes")
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    path <- tempfile(fileext = ".nc")
    peer_path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    write_local_cmip6_netcdf_fixture(peer_path, 2060L, variable_id = "huss")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(store_test__fallback_app(
        bytes,
        delay = 1
    ))
    withr::defer(server$stop())
    remote <- EsgStore$new(tempfile("fallback-remote-"))
    peer <- EsgStore$new(tempfile("fallback-peer-"))
    withr::defer(remote$close())
    withr::defer(peer$close())
    pool <- source_pool(2L, timeout = 15)
    withr::defer(pool$close(force = TRUE))
    q1 <- remote$add_files(store_test__fallback_files(path, server))
    q2 <- peer$add_files(esgf_test__file_result(esgf_test__file_docs(
        peer_path,
        variable_id = "huss"
    )))
    time <- c("2060-01-01", "2060-01-03")
    p1 <- remote$plan_region(q1, 104, 1.37, time)
    p2 <- peer$plan_region(q2, 104, 1.37, time)
    first <- remote$extract(
        plan_id = p1$plan_id,
        fallback = "auto",
        pool = pool
    )
    second <- peer$extract(
        plan_id = p2$plan_id,
        fallback = "error",
        pool = pool
    )
    finished <- store_test__drain_fallback(pool, list(first, second))
    expect_identical(first$status, "done")
    expect_identical(second$status, "done")
    expect_lt(finished[[2L]], finished[[1L]])
    expect_true(
        all(first$value$status == "done"),
        info = paste(first$value$last_error, collapse = "\n")
    )
    expect_true(all(second$value$status == "done"))
    expect_true(all(remote$validate()$checksum_ok))
    result <- remote$query("select * from extraction_result")
    expected <- EsgStore$new(tempfile("fallback-reference-"))
    withr::defer(expected$close())
    q3 <- expected$add_files(esgf_test__file_result(esgf_test__file_docs(path)))
    p3 <- expected$plan_region(q3, 104, 1.37, time)
    expected$extract(plan_id = p3$plan_id, fallback = "error")
    reference <- expected$query("select * from extraction_result")
    read_values <- function(store, rows) {
        store$query(paste0(
            "select cf_year,cf_month,cf_day,cf_second_of_day,value,units from read_parquet(",
            ddb_literal(
                priv(store)$conn,
                file.path(store$path, rows$output_path[[1L]])
            ),
            ") order by cf_year,cf_month,cf_day,cf_second_of_day"
        ))
    }
    expect_identical(
        read_values(remote, result),
        read_values(expected, reference)
    )
    before <- jsonlite::fromJSON(
        rawToChar(curl::curl_fetch_memory(server$url("/counts"))$content)
    )$gets
    repeated <- remote$extract(
        plan_id = p1$plan_id,
        fallback = "auto",
        pool = pool
    )
    store_test__drain_fallback(pool, list(repeated))
    expect_identical(repeated$status, "done")
    after <- jsonlite::fromJSON(
        rawToChar(curl::curl_fetch_memory(server$url("/counts"))$content)
    )$gets
    expect_identical(after, before)
    expect_identical(remote$query("select * from extraction_result"), result)
})

# A valid HTTP status cannot publish a corrupted source as a completed plan.
test_that("persistent HTTP fallback retains checksum failures", {
    skip_if_not_installed("webfakes")
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(store_test__fallback_app(bytes))
    withr::defer(server$stop())
    store <- EsgStore$new(tempfile("fallback-checksum-"))
    withr::defer(store$close())
    pool <- source_pool(1L, timeout = 15)
    withr::defer(pool$close(force = TRUE))
    q <- store$add_files(store_test__fallback_files(
        path,
        server,
        strrep("0", 64L)
    ))
    plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    ticket <- store$extract(
        plan_id = plan$plan_id,
        fallback = "auto",
        pool = pool
    )
    store_test__drain_fallback(pool, list(ticket))
    expect_identical(ticket$status, "done")
    expect_identical(ticket$value$status, "failed")
    expect_match(ticket$value$last_error, "HTTP|checksum", ignore.case = TRUE)
    expect_equal(nrow(store$query("select * from extraction_result")), 0L)
})

# The HTTP response is a valid NetCDF with the declared size and hash, but
# its contents cannot satisfy the requested variable after the file opens.
test_that("both Store recovery modes retain remote and local read failures", {
    skip_if_not_installed("webfakes")
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = "huss")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(store_test__fallback_app(bytes))
    withr::defer(server$stop())

    pool <- source_pool(1L, timeout = 15)
    withr::defer(pool$close(force = TRUE))
    for (mode in c("ordinary", "pooled")) {
        store <- EsgStore$new(file.path(root, mode))
        withr::defer(store$close())
        query <- store$add_files(store_test__fallback_files(path, server))
        plan <- store$plan_region(
            query,
            104,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        result <- store$extract(
            plan_id = plan$plan_id,
            fallback = "auto",
            pool = if (mode == "pooled") pool
        )
        if (mode == "pooled") {
            store_test__drain_fallback(pool, list(result))
            expect_identical(result$status, "done")
            result <- result$value
        }
        expect_identical(result$status, "failed", info = mode)
        expect_match(result$last_error, "OPeNDAP open failed", info = mode)
        expect_match(result$last_error, "HTTPServer read failed", info = mode)
        expect_equal(nrow(store$query("select * from extraction_result")), 0L)
    }
})

# A real worker deadline leaves the original task/session and seeded partial
# bytes recoverable. The next worker resumes Range bytes without reopening DAP.
test_that("HTTP fallback resumes one retained session after a reader deadline", {
    skip_if_not_installed("webfakes")
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    server <- webfakes::new_app_process(store_test__fallback_app(
        bytes,
        delay = 3
    ))
    withr::defer(server$stop())
    root <- tempfile("retained-http-cache-")
    store <- EsgStore$new(tempfile("fallback-deadline-"))
    withr::defer(store$close())
    q <- store$add_files(store_test__fallback_files(path, server))
    plan <- store$plan_region(q, 104, 1.37, c("2060-01-01", "2060-01-03"))
    file <- store$query("select * from file_catalog")
    config <- store__pool_download_config(file, root, 10L)
    downloader <- do.call(Downloader$new, config)
    session <- downloader$enqueue(store__download_plan_rows(file))
    task <- downloader$tasks()
    partial <- file.path(config$temp, paste0(task$task_id[[1L]], ".part"))
    writeBin(bytes[1:1024], partial)
    priv(downloader)$disconnect_manifest()
    pool <- source_pool(1L, timeout = .5, download_dir = root)
    withr::defer(pool$close(force = TRUE))
    old <- ps::ps_handle(pool$status()[[1L]]$pid)
    ticket <- store$extract(
        plan_id = plan$plan_id,
        fallback = "auto",
        pool = pool
    )
    store_test__drain_fallback(pool, list(ticket))
    expect_identical(ticket$value$status, "failed")
    expect_false(ps::ps_is_running(old))
    expect_true(file.exists(partial))
    expect_identical(readBin(partial, "raw", n = 1024L), bytes[1:1024])
    pool$close()
    curl::curl_fetch_memory(
        server$url("/fast"),
        curl::new_handle(postfields = "")
    )
    resumed_pool <- source_pool(1L, timeout = 15, download_dir = root)
    withr::defer(resumed_pool$close(force = TRUE))
    resumed <- store$extract(
        plan_id = plan$plan_id,
        fallback = "auto",
        pool = resumed_pool
    )
    store_test__drain_fallback(resumed_pool, list(resumed))
    expect_identical(
        resumed$status,
        "done",
        info = if (!is.null(resumed$error)) conditionMessage(resumed$error)
    )
    expect_gt(nrow(resumed$value), 0L)
    expect_true(
        all(resumed$value$status == "done"),
        info = resumed$value$last_error
    )
    downloader <- Downloader$new(manifest = config$manifest, n_workers = 0L)
    withr::defer(priv(downloader)$disconnect_manifest())
    after <- downloader$tasks()
    expect_identical(after$session_id, session)
    expect_identical(after$task_id, task$task_id)
    expect_true(all(downloader$verify()$checksum_ok))
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    expect_length(references, 1L)
    expect_match(
        basename(references[[1L]]),
        "^pool-download-[a-f0-9]{64}[.]json$"
    )
    reference <- jsonlite::read_json(references[[1L]])
    expect_true(reference$checksum_verified_by_reader)
    expect_identical(
        reference$cache_path,
        store_normalize_path(after$target_path[[1L]])
    )
    expect_identical(reference$checksum, after$checksum[[1L]])
    expect_true(file.exists(reference$cache_path))
    expect_length(
        list.files(
            file.path(store$path, "downloads"),
            "[.]nc$",
            recursive = TRUE
        ),
        0L
    )
    expect_error(
        store$register_artifact("netcdf", path = reference$cache_path),
        "outside"
    )
    reference_time <- file.info(references[[1L]])$mtime
    store__sync_pool_download(store, config$manifest)
    expect_identical(file.info(references[[1L]])$mtime, reference_time)
    counts <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/counts"))$content
    ))
    expect_true(any(counts$ranges == "bytes=1024-"))
    expect_identical(counts$dods, 0L)
    priv(downloader)$disconnect_manifest()
    # Another store/query may reuse the same content without another DAP or
    # HTTP request; its manifest must still receive its own native artifact.
    alias <- EsgStore$new(tempfile("fallback-other-store-"))
    withr::defer(alias$close())
    files <- store_test__fallback_files(path, server)
    query <- alias$add_files(files, label = "independent-query")
    other_plan <- alias$plan_region(
        query,
        104,
        1.37,
        c("2060-01-01", "2060-01-03")
    )
    other <- alias$extract(
        plan_id = other_plan$plan_id,
        fallback = "auto",
        pool = resumed_pool
    )
    store_test__drain_fallback(resumed_pool, list(other))
    expect_identical(other$status, "done")
    expect_gt(nrow(other$value), 0L)
    expect_true(
        all(other$value$status == "done"),
        info = other$value$last_error
    )
    expect_true(all(alias$validate()$checksum_ok))
    repeated <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/counts"))$content
    ))
    expect_identical(repeated$gets, counts$gets)
    expect_identical(repeated$dods, counts$dods)
    expect_error(source_pool(1L, download_dir = TRUE), "download_dir")
})

# append_new_rows() {{{
# append_new_rows() inserts only rows whose key is not yet stored. It must keep
# its earlier behavior while asking DuckDB only about the keys being inserted
# instead of reading the whole key column back into R.
store_append_test__store <- function(env = parent.frame()) {
    store <- store_test__new(env = env)
    ddb_exec(
        priv(store)$conn,
        "CREATE TABLE append_t (k VARCHAR PRIMARY KEY, v VARCHAR, n INTEGER)"
    )
    store
}

store_append_test__rows <- function(keys, v = "new") {
    data.frame(
        k = keys,
        v = v,
        n = seq_along(keys),
        stringsAsFactors = FALSE
    )
}

# Read all persisted fields in key order to check insertion outcomes.
store_append_test__state <- function(store) {
    out <- ddb_query(priv(store)$conn, "SELECT k, v, n FROM append_t")
    out[order(out$k), , drop = FALSE]
}

test_that("append_new_rows preserves stored values and inserts only new keys", {
    store <- store_append_test__store()
    seed <- store_append_test__rows(c("a", "b", "c"), v = "seed")
    ddb_append_table(priv(store)$conn, "append_t", seed)
    batch <- store_append_test__rows(c("b", "d", "e", "a"), v = "batch")
    priv(store)$append_new_rows("append_t", batch, "k")
    state <- store_append_test__state(store)
    expect_identical(state$k, c("a", "b", "c", "d", "e"))
    expect_identical(state$v, c("seed", "seed", "seed", "batch", "batch"))
    expect_identical(state$n, c(1L, 2L, 3L, 2L, 3L))

    # Replaying the batch or submitting no rows must preserve every field.
    priv(store)$append_new_rows("append_t", batch, "k")
    priv(store)$append_new_rows("append_t", batch[0, ], "k")
    expect_identical(store_append_test__state(store), state)
    empty <- store_append_test__store()
    priv(empty)$append_new_rows(
        "append_t",
        store_append_test__rows("first"),
        "k"
    )
    expect_identical(store_append_test__state(empty)$k, "first")
})

test_that("append_new_rows rejects bad keys without partial insertion", {
    store <- store_append_test__store()
    dup <- store_append_test__rows(c("x", "x"))
    expect_error(priv(store)$append_new_rows("append_t", dup, "k"))
    expect_identical(nrow(store_append_test__state(store)), 0L)
    missing <- store_append_test__rows(c("y", NA))
    expect_error(priv(store)$append_new_rows("append_t", missing, "k"))
    expect_identical(nrow(store_append_test__state(store)), 0L)
})

test_that("append_new_rows handles factor keys and keys that need quoting", {
    store <- store_append_test__store()
    odd <- c(
        "it's",
        "semi;colon",
        "quote\"d",
        "CaSe",
        "case",
        "tab\tkey",
        "ünï"
    )
    ddb_append_table(
        priv(store)$conn,
        "append_t",
        store_append_test__rows(odd[1:4], v = "seed")
    )
    batch <- store_append_test__rows(odd)
    batch$k <- factor(batch$k, levels = odd)
    priv(store)$append_new_rows("append_t", batch, "k")
    state <- store_append_test__state(store)
    expect_setequal(state$k, odd)
    expect_identical(sum(state$v == "seed"), 4L)
})

test_that("append_new_rows preserves results across multiple batches", {
    store <- store_append_test__store()
    stored <- sprintf("k%06d", seq(1L, 9000L, by = 2L))
    ddb_append_table(
        priv(store)$conn,
        "append_t",
        store_append_test__rows(stored, v = "seed")
    )
    # 4500 keys span three chunks; half of them already exist.
    keys <- sprintf("k%06d", 1:4500)
    priv(store)$append_new_rows("append_t", store_append_test__rows(keys), "k")
    state <- store_append_test__state(store)
    expected <- sort(union(stored, keys))
    expect_identical(state$k, expected)
    # Pre-existing rows were not overwritten; new ones carry the batch value.
    expect_identical(
        state$v[state$k %in% stored],
        rep("seed", length(stored))
    )
    expect_true(all(state$v[!state$k %in% stored] == "new"))
})
# }}}

# Schema initialization and repair {{{
# Reopening preserves stored data and the schema.
test_that("current store reopen preserves rows and schema", {
    path <- tempfile("store-schema-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$set_meta("sentinel", "keep")
    before <- ddb_query(
        priv(store)$conn,
        "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, ordinal_position"
    )
    store$close()
    store <- EsgStore$new(path, create = FALSE)
    on.exit(store$close(), add = TRUE)
    expect_identical(store$get_meta("sentinel"), "keep")
    expect_identical(store$get_meta("schema_version"), STORE_SCHEMA_VERSION)
    expect_identical(
        ddb_query(
            priv(store)$conn,
            "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, ordinal_position"
        ),
        before
    )
})

# A same-version manifest can lose a table or an additive column between opens.
# Verify recovery against a fresh complete catalog, including persisted rows.
test_that("current store repairs missing tables and additive columns", {
    path <- tempfile("store-schema-repair-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    store$set_meta("sentinel", "keep")
    catalog <- ddb_query(
        priv(store)$conn,
        "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, column_name"
    )
    ddb_exec(priv(store)$conn, "DROP TABLE shift_run_event")
    ddb_exec(
        priv(store)$conn,
        "ALTER TABLE epw_output DROP COLUMN provenance_json"
    )
    ddb_exec(priv(store)$conn, "ALTER TABLE file_catalog DROP COLUMN latest")
    store$close()
    store <- EsgStore$new(path, create = FALSE)
    expect_identical(
        ddb_query(
            priv(store)$conn,
            "SELECT table_name, column_name, data_type FROM information_schema.columns ORDER BY table_name, column_name"
        ),
        catalog
    )
    expect_identical(store$get_meta("sentinel"), "keep")
})

# Checking a valid store once must not authorize later opens after its version
# or metadata has changed; no connection/path-level success cache is allowed.
test_that("reopening still rejects changed or missing schema versions", {
    path <- tempfile("store-schema-version-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    store$close()
    for (version in c("2.6.0", "newer", NA_character_)) {
        conn <- ddb_connect(file.path(path, "manifest.duckdb"))
        ddb_exec(conn, "DELETE FROM store_meta WHERE key = 'schema_version'")
        if (!is.na(version)) {
            ddb_exec(
                conn,
                sprintf(
                    "INSERT INTO store_meta VALUES ('schema_version', '%s', NULL)",
                    version
                )
            )
        }
        ddb_exec(conn, "DROP TABLE IF EXISTS shift_run_event")
        ddb_disconnect(conn)
        message <- if (is.na(version)) {
            "Store schema version .*missing"
        } else {
            "store_reset"
        }
        expect_error(EsgStore$new(path, create = FALSE), message)
        conn <- ddb_connect(file.path(path, "manifest.duckdb"))
        expect_false("shift_run_event" %in% ddb_list_tables(conn))
        stored <- ddb_query(
            conn,
            "SELECT value FROM store_meta WHERE key = 'schema_version'"
        )
        expect_identical(stored$value, version[!is.na(version)])
        ddb_disconnect(conn)
    }
})

# An interrupted additive repair must be retried from the database state. The
# original live instance owns the connection so cleanup remains deterministic.
test_that("schema repair retries after a failed DDL statement", {
    path <- tempfile("store-schema-retry-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    ddb_exec(priv(store)$conn, "DROP TABLE shift_run_event")
    original <- ddb_exec
    attempts <- 0L
    local_mocked_bindings(
        ddb_exec = function(conn, sql) {
            if (grepl("CREATE TABLE IF NOT EXISTS shift_run_event", sql)) {
                attempts <<- attempts + 1L
                if (attempts == 1L) stop("interrupted schema repair")
            }
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    expect_error(priv(store)$init_schema(), "interrupted schema repair")
    expect_false("shift_run_event" %in% ddb_list_tables(priv(store)$conn))
    priv(store)$init_schema()
    expect_true("shift_run_event" %in% ddb_list_tables(priv(store)$conn))
})
# }}}

# vim: fdm=marker :
