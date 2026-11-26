# OPeNDAP Subset Acquisition & Frozen Recovery {{{
local_test_cache(scope = "persist")
withr::local_options(list(epwshiftr.progress = FALSE))

# Shared DAP, ESGF index, DuckDB, and recovery test helpers {{{
qr_test__file_env <- environment()
qr_test__shared_dap_state <- new.env(parent = emptyenv())
qr_test__shared_dap_state$server <- NULL

qr_test__shared_dap_server <- function() {
    skip_if_not_installed("webfakes")
    srv <- qr_test__shared_dap_state$server
    if (is.null(srv)) {
        srv <- webfakes::new_app_process(subset_test__dap_app())
        qr_test__shared_dap_state$server <- srv
        withr::defer(
            {
                qr_test__shared_dap_state$server <- NULL
                try(srv$stop(), silent = TRUE)
            },
            envir = qr_test__file_env
        )
    }
    qr_test__dap_control(srv, "reset")
    srv
}

qr_test__dap_server <- function(
    ...,
    init = NULL,
    app = NULL,
    shared = NULL,
    env = parent.frame()
) {
    skip_if_not_installed("webfakes")
    dots <- list(...)
    fail_only <- length(dots) == 1L &&
        identical(names(dots), "fail_first") &&
        is.null(init) &&
        is.null(app)
    use_shared <- if (is.null(shared)) {
        (length(dots) == 0L && is.null(init) && is.null(app)) || fail_only
    } else {
        isTRUE(shared)
    }
    if (use_shared) {
        server <- qr_test__shared_dap_server()
        if (fail_only && isTRUE(dots$fail_first)) {
            qr_test__dap_control(server, "fail")
        }
        return(server)
    }
    if (is.null(app)) {
        app <- subset_test__dap_app(...)
    }
    if (!is.null(init)) {
        init(app)
    }
    server <- webfakes::new_app_process(app)
    withr::defer(server$stop(), envir = env)
    server
}

qr_test__dap_docs <- function(
    server,
    endpoints,
    nodes = NULL,
    ids = NULL,
    titles = NULL,
    variable_id = "tas",
    distinct = FALSE,
    suffix = "",
    ...
) {
    n <- length(endpoints)
    docs <- query_result_test_file_docs()[rep(1L, n), , drop = FALSE]
    row.names(docs) <- NULL
    docs$variable_id <- variable_id
    if (!is.null(nodes)) {
        docs$data_node <- nodes
    }
    if (!is.null(ids)) {
        docs$id <- docs$instance_id <- docs$master_id <- ids
    } else if (isTRUE(distinct)) {
        docs$id <- docs$instance_id <- docs$master_id <- paste0(
            "file-",
            seq_len(n)
        )
    }
    if (!is.null(titles)) {
        docs$title <- titles
    }
    extra <- list(...)
    for (nm in names(extra)) {
        docs[[nm]] <- extra[[nm]]
    }
    docs$url <- I(lapply(endpoints, function(ep) {
        paste0(server$url(), ep, suffix, "|application/netcdf|OPENDAP")
    }))
    docs
}

qr_test__dap_files <- function(server, endpoints, ...) {
    query_result_test_object("File", qr_test__dap_docs(server, endpoints, ...))
}

qr_test__dap_request <- function(
    time_start = "2060-01-01",
    time_stop = time_start,
    request_id = if (length(time_start) == 1L) {
        "one"
    } else {
        paste0("req-", seq_along(time_start))
    },
    case_id = NULL,
    site_id = "city",
    lon = 10.25,
    lat = 0.25,
    ...
) {
    df <- data.frame(
        request_id = request_id,
        site_id = site_id,
        lon = lon,
        lat = lat,
        time_start = time_start,
        time_stop = time_stop,
        ...,
        stringsAsFactors = FALSE
    )
    if (!is.null(case_id)) {
        df$case_id <- case_id
    }
    df
}

qr_test__dap_control <- function(server, path) {
    rawToChar(curl::curl_fetch_memory(paste0(server$url(), path))$content)
}

qr_test__dap_traffic <- function(server, weather_only = FALSE) {
    calls <- jsonlite::fromJSON(qr_test__dap_control(server, "requests"))
    if (isTRUE(weather_only)) calls[grepl("[.]dods tas", calls)] else calls
}

qr_test__fetch <- function(
    files,
    requests,
    dest,
    workers = 1L,
    retries = 0L,
    discover = FALSE,
    ...
) {
    files$fetch_subset(
        requests,
        dest,
        workers = workers,
        retries = retries,
        discover = discover,
        ...
    )
}

qr_test__vals <- function(dest, ...) {
    sort(read_subset(dest, ...)$values$value)
}

qr_test__index_server <- function(
    docs,
    changed = NULL,
    conflict = NULL,
    weather_server = NULL,
    paginate = TRUE,
    env = parent.frame()
) {
    skip_if_not_installed("webfakes")
    encode <- function(tbl) {
        if (!isTRUE(paginate)) {
            return(list(jsonlite::toJSON(
                esgf_test__response(tbl),
                auto_unbox = TRUE
            )))
        }
        lapply(seq_len(nrow(tbl)), function(i) {
            r <- esgf_test__response(
                tbl[i, , drop = FALSE],
                num_found = nrow(tbl)
            )
            r$response$start <- i - 1L
            jsonlite::toJSON(r, auto_unbox = TRUE)
        })
    }
    app <- webfakes::new_app()
    app$locals$pages <- encode(docs)
    app$locals$changed_pages <- if (is.null(changed)) NULL else encode(changed)
    app$locals$conflict_pages <- if (is.null(conflict)) {
        NULL
    } else {
        encode(conflict)
    }
    app$locals$changed <- app$locals$conflict <- FALSE
    app$locals$queries <- list()
    app$locals$weather <- if (is.null(weather_server)) {
        NULL
    } else {
        paste0(weather_server$url(), "requests")
    }
    app$locals$paginate <- isTRUE(paginate)
    app$get("/esg-search/search", function(req, res, locals) {
        started <- if (is.null(locals$weather)) {
            NA
        } else {
            grepl(
                "dods tas",
                rawToChar(curl::curl_fetch_memory(locals$weather)$content)
            )
        }
        locals$queries[[length(locals$queries) + 1L]] <- list(
            query = req$query,
            known_read_started = started
        )
        off <- if (!locals$paginate || is.null(req$query$offset)) {
            0L
        } else {
            as.integer(req$query$offset)
        }
        pages <- if (locals$conflict && !is.null(locals$conflict_pages)) {
            locals$conflict_pages
        } else if (locals$changed && !is.null(locals$changed_pages)) {
            locals$changed_pages
        } else {
            locals$pages
        }
        res$set_header("Content-Type", "application/json")$send(pages[[
            off + 1L
        ]])
    })
    app$get("/conflict", function(req, res, locals) {
        locals$conflict <- TRUE
        res$send("ok")
    })
    app$get("/change", function(req, res, locals) {
        locals$changed <- TRUE
        res$send("ok")
    })
    app$get("/queries", function(req, res, locals) {
        res$send_json(locals$queries)
    })
    index <- webfakes::new_app_process(app)
    withr::defer(index$stop(), envir = env)
    index
}

qr_test__ddb_exec <- function(dest, sql) {
    conn <- ddb_connect(file.path(dest, "manifest.duckdb"))
    on.exit(ddb_disconnect(conn), add = TRUE)
    for (stmt in sql) {
        ddb_exec(conn, stmt)
    }
    invisible(NULL)
}

qr_test__interrupt_pending <- function(expr, msg) {
    rename <- base::file.rename
    hit <- FALSE
    local_mocked_bindings(
        file.rename = function(from, to) {
            out <- rename(from, to)
            if (
                !hit && isTRUE(out) && grepl("/pending-[^/]+[.]rds[.]json$", to)
            ) {
                hit <<- TRUE
                stop(msg, call. = FALSE)
            }
            out
        },
        .package = "base"
    )
    expect_error(force(expr), msg)
}

qr_test__assert_no_weather_reread <- function(
    files,
    request,
    dest,
    server,
    artifacts,
    hashes,
    attempts,
    expected_values,
    endpoint = "fast-bounds.nc.dods tas"
) {
    qr_test__dap_control(server, "reset")
    expect_true(qr_test__fetch(files, request, dest)$complete)
    expect_identical(subset_status(dest, "attempts"), attempts)
    expect_identical(tools::sha256sum(artifacts), hashes)
    expect_equal(read_subset(dest)$values, expected_values)
    expect_false(any(grepl(
        endpoint,
        qr_test__dap_traffic(server),
        fixed = TRUE
    )))
}

qr_test__attempt_history <- function(
    source_key = "a",
    url = "u",
    data_node = "node",
    client = "client",
    observed_at = 1000,
    elapsed_seconds = 1,
    native_values = 0,
    success = FALSE,
    error = "NetCDF: DAP failure",
    error_kind = "transport",
    ...
) {
    data.table::data.table(
        source_key = source_key,
        url = url,
        data_node = data_node,
        client = client,
        observed_at = observed_at,
        elapsed_seconds = elapsed_seconds,
        native_values = native_values,
        success = success,
        error = error,
        error_kind = error_kind,
        ...
    )
}

qr_test__frozen_plan <- function(files, requests, key, root) {
    plan <- file.path(root, "plan")
    dir.create(plan, recursive = TRUE, showWarnings = FALSE)
    catalogue <- files$save(file.path(plan, "catalogue.json"))
    selection <- data.frame(
        source_key = key,
        owner = "pc",
        batch = "pc-fixture",
        catalogue = basename(catalogue),
        catalogue_sha256 = checksum_file(catalogue),
        record_indices = "[1]"
    )
    selected <- file.path(plan, "files.parquet")
    requested <- file.path(plan, "requests.parquet")
    write_parquet_file(selection, selected)
    write_parquet_file(requests, requested)
    meta <- list(
        files_sha256 = checksum_file(selected),
        requests_sha256 = checksum_file(requested),
        capacities = list(pc = 1L)
    )
    jsonlite::write_json(
        meta,
        file.path(plan, "manifest.json"),
        auto_unbox = TRUE
    )
    denom <- file.path(root, "denominator.rds")
    saveRDS(
        list(
            source_files_sha256 = meta$files_sha256,
            requests_sha256 = meta$requests_sha256,
            demands = cbind(
                requests,
                selection[
                    rep(1L, nrow(requests)),
                    c("source_key", "owner", "batch")
                ]
            )
        ),
        denom
    )
    journal <- file.path(root, "journal")
    dir.create(journal, recursive = TRUE, showWarnings = FALSE)
    state <- opendap_recovery__persist(
        list(
            generation = 0L,
            started = as.double(Sys.time()),
            configuration = list(
                version = 2L,
                identity = list(
                    node = "pc",
                    files = meta$files_sha256,
                    requests = meta$requests_sha256,
                    denominator = checksum_file(denom)
                ),
                max_rounds = Inf,
                max_wait = Inf,
                max_elapsed = Inf
            ),
            phase = "blocked_error",
            round = 1L,
            admissions = 1L,
            wait_seconds = 0,
            attempted = c("pc-fixture" = FALSE),
            in_flight = list(
                batch = "pc-fixture",
                source_keys = key,
                started = as.double(Sys.time())
            )
        ),
        journal
    )
    list(plan = plan, denominator = denom, journal = journal, state = state)
}
# }}}

# G1. Candidate plans & replica switching {{{
# Candidate identity compares checksums only when their algorithms agree,
# preserves interleaved ordering, and rejects contradictions across sparse rows.
test_that("raw candidate plans preserve digest algorithms, interleaved ordering, and reject contradictions", {
    dap <- function(u) paste0(u, "|application/netcdf|OPENDAP")
    docs <- query_result_test_file_docs()[c(1L, 1L), ]
    docs$variable_id <- "tas"
    docs$checksum_type <- c("SHA256", "MD5")
    docs$checksum <- c("sha-value", "md5-value")
    docs$url <- I(as.list(dap(c(
        "https://one.example/a.nc",
        "https://two.example/a.nc"
    ))))
    plan <- query_result_test_object("File", docs)$access_plan(expand = FALSE)
    expect_equal(nrow(plan), 2L)
    expect_length(unique(plan$source_key), 1L)
    docs$checksum_type <- c("SHA256", "SHA256")
    expect_error(
        query_result_test_object("File", docs)$access_plan(expand = FALSE),
        "Conflicting"
    )

    inter <- query_result_test_file_docs()[rep(1L, 7L), ]
    inter$id <- paste0("row-", 1:7)
    inter$instance_id <- inter$master_id <- c("a", "b", "a", "c", "a", "b", "a")
    inter$version <- c(1L, 1L, 2L, 1L, 1L, NA_integer_, NA_integer_)
    inter$checksum_type <- c(
        "SHA256",
        "SHA256",
        "SHA256",
        "SHA256",
        "MD5",
        NA,
        NA
    )
    inter$checksum <- c("a-sha", "b-sha", "a2-sha", "c-sha", "a-md5", NA, NA)
    inter$size <- c(1, 1, 2, 1, 1, NA_real_, NA_real_)
    inter$url <- I(as.list(dap(paste0(
        "https://node",
        1:7,
        ".example/file.nc"
    ))))
    iplan <- query_result_test_object("File", inter)$access_plan(expand = FALSE)
    expect_identical(iplan$content_group, c(1L, 2L, 3L, 4L, 1L, 2L, 1L))
    expect_identical(
        iplan$source_key[c(1L, 2L, 3L, 4L, 1L, 2L, 1L)],
        iplan$source_key
    )
    expect_equal(
        nrow(query_result_test_object("File", inter[FALSE, ])$access_plan(
            expand = FALSE
        )),
        0L
    )

    sparse <- query_result_test_file_docs()[rep(1L, 4L), ]
    sparse$instance_id <- sparse$master_id <- c("a", "b", "a", "a")
    sparse$checksum <- c("first", "other", NA_character_, "conflicting")
    sparse$url <- I(as.list(dap(paste0(
        "https://node",
        1:4,
        ".example/file.nc"
    ))))
    expect_error(
        query_result_test_object("File", sparse)$access_plan(expand = FALSE),
        "Conflicting replica content"
    )
})

# A real DAP2 server and real source workers exercise transport faults without
# substituting package scheduling, native selection or durable storage.
test_that("raw subsets switch failed replicas and repair only missing native corners", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(
        srv,
        c("bad.nc", "fast.nc"),
        nodes = c("first", "second"),
        suffix = "#show=url"
    )
    req <- qr_test__dap_request(
        c("2060-01-01", "2060-01-04"),
        request_id = c("first-day", "fourth-day"),
        case_id = "original"
    )
    expect_true(qr_test__fetch(f, req, d, workers = 2L)$complete)
    att <- subset_status(d, "attempts")
    expect_identical(
        att$data_node[att$native_values > 0],
        c("first", rep("second", 4L))
    )
    expect_equal(sum(!att$success), 1L)
    expect_equal(att$native_values[att$native_values > 0], c(4, rep(1, 4L)))

    saved <- read_subset(d, case_id = "original")
    expect_equal(sort(saved$values$value), c(1:4, 13:16))
    expect_equal(nrow(saved$neighborhoods), 8L)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(sort(unique(saved$values$cf_day)), c(1L, 4L))

    # DAP request evidence excludes hidden libnetcdf whole-variable prefetch.
    before <- qr_test__dap_control(srv, "requests")
    weather <- qr_test__dap_traffic(srv, weather_only = TRUE)
    expect_true(all(grepl(
        "tas%5b[03](:[03])?%5d%5b[01](:[01])?%5d%5b[01](:[01])?%5d$",
        weather
    )))
    expect_length(weather, 6L)

    req$request_id <- paste0(req$request_id, "-linked")
    req$case_id <- "linked"
    expect_true(qr_test__fetch(f, req, d, workers = 2L)$complete)
    expect_equal(nrow(read_subset(d, case_id = "linked")$values), 8L)
    expect_identical(qr_test__dap_control(srv, "requests"), before)

    writeBin(as.raw(0), subset_status(d, "blocks")$path[[1L]])
    expect_true(qr_test__fetch(f, req, d, workers = 2L)$complete)
    expect_equal(qr_test__vals(d, case_id = "linked"), c(1:4, 13:16))
    expect_equal(
        sum(subset_status(d, "attempts")$native_values) -
            sum(att$native_values),
        4
    )
    expect_error(f$fetch_subset(req[0L, ], d), "rows")
    unmatched <- req[1L, ]
    unmatched$variable_id <- "rsds"
    miss <- f$fetch_subset(unmatched, d, discover = FALSE)
    expect_false(miss$complete)
    expect_equal(nrow(miss$unmatched_requests), 1L)
})

# Measured reads affect the next public acquisition; clients on another
# network start from catalogue order instead of inheriting local latency.
test_that("raw subset acquisition ranks actual node reads within the current client", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    req <- qr_test__dap_request()
    acq <- function(id, eps, nodes, net = "local") {
        f <- qr_test__dap_files(
            srv,
            eps,
            nodes = nodes,
            ids = rep(id, length(eps))
        )
        qr_test__fetch(f, req, d, network_id = net)
    }
    expect_true(acq("learn-slow", "slow.nc", "slow")$complete)
    expect_true(acq("learn-fast", "fast.nc", "fast")$complete)
    expect_true(
        acq("ranked", c("slow.nc", "fast.nc"), c("slow", "fast"))$complete
    )
    att <- subset_status(d, "attempts")
    expect_identical(
        att$data_node[att$native_values > 0],
        c("slow", "fast", "fast")
    )
    expect_true(
        acq(
            "new-network",
            c("slow.nc", "fast.nc"),
            c("slow", "fast"),
            "other"
        )$complete
    )
    expect_identical(tail(subset_status(d, "attempts")$data_node, 1L), "slow")
})

# Every OPeNDAP service advertised by one File remains a real execution option;
# expanding per-record services preserves File rows and missing-service gaps.
test_that("raw subsets retain all advertised OPeNDAP endpoints and preserve File rows with missing services", {
    inv <- query_result_test_file_docs()[rep(1L, 3L), ]
    inv$id <- paste0("record-", 1:3)
    inv$instance_id <- inv$master_id <- paste0("logical-", 1:3)
    inv$variable_id <- "tas"
    inv$title <- paste0(
        "tas_Amon_model_historical_member_grid_",
        c("206001-206001.nc", "206006-206006.nc", "206012-206012.nc")
    )
    inv$data_node <- NA_character_
    inv$url <- I(list(
        c(
            "https://one.example/a.nc.html|application/netcdf|OPENDAP",
            "https://one.example/a.nc|application/netcdf|OPENDAP",
            "https://two.example/a.nc|application/netcdf|OPENDAP"
        ),
        "https://three.example/b.nc|application/netcdf|OPENDAP",
        "https://http.example/c.nc|application/netcdf|HTTPServer"
    ))
    inv_f <- query_result_test_object("File", inv)
    ip <- inv_f$access_plan(expand = FALSE)
    expect_identical(ip$record_index, c(1L, 1L, 2L, 3L))
    expect_identical(
        ip$url,
        c(
            "https://one.example/a.nc",
            "https://two.example/a.nc",
            "https://three.example/b.nc",
            NA_character_
        )
    )
    expect_identical(
        ip$data_node,
        c("one.example", "two.example", "three.example", NA_character_)
    )
    expect_identical(
        ip$file_start_key,
        c(
            "20600100000000",
            "20600100000000",
            "20600600000000",
            "20601200000000"
        )
    )
    expect_length(unique(ip$source_key), 3L)
    expect_identical(ip$source_key[[1L]], ip$source_key[[2L]])
    expect_identical(attr(ip, "selection"), inv_f$selection())

    md <- withr::local_tempdir()
    mreq <- qr_test__dap_request(
        "2060-12-01",
        request_id = "missing",
        case_id = "case"
    )
    expect_false(qr_test__fetch(inv_f$slice(3L), mreq, md)$complete)
    expect_equal(sum(subset_status(md, "attempts")$native_values), 0)
    expect_equal(nrow(subset_status(md, "blocks")), 0L)

    # Unreachable first advertised OPeNDAP URL falls back to second, then reuses cache
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    bad <- "http://127.0.0.1:1/unavailable.nc"
    good <- paste0(srv$url(), "fast.nc")
    docs <- query_result_test_file_docs()
    docs$variable_id <- "tas"
    docs$url <- I(list(paste0(
        c(bad, good, paste0(good, ".html")),
        "|application/netcdf|OPENDAP"
    )))
    f <- query_result_test_object("File", docs)
    plan <- f$access_plan(expand = FALSE)
    expect_identical(plan$url, c(bad, good))
    expect_identical(plan$record_index, c(1L, 1L))
    expect_length(unique(plan$source_key), 1L)

    req <- qr_test__dap_request(request_id = "one-day", case_id = "original")
    expect_true(qr_test__fetch(f, req, d, timeout = 5)$complete)
    att <- subset_status(d, "attempts")
    expect_true(any(!att$success))
    expect_equal(sum(att$native_values), 4)
    saved <- read_subset(d, case_id = "original")
    expect_equal(sort(saved$values$value), 1:4)
    expect_equal(nrow(saved$neighborhoods), 4L)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_identical(unique(saved$values$cf_day), 1L)

    before <- qr_test__dap_control(srv, "requests")
    req$request_id <- "linked-day"
    req$case_id <- "linked"
    expect_true(qr_test__fetch(f, req, d, timeout = 5)$complete)
    expect_identical(subset_status(d, "attempts"), att)
    expect_identical(qr_test__dap_control(srv, "requests"), before)
    expect_equal(qr_test__vals(d, case_id = "linked"), 1:4)
})
# }}}

# G2. Fresh invocation & incompatible bounds/metadata {{{
# Fresh invocations ignore older cache while retaining committed replica progress
# and validating native interval bounds and source metadata before replacement.
test_that("raw subset fresh invocation retains progress and rejects incompatible bounds and metadata", {
    srv <- qr_test__dap_server()
    req <- qr_test__dap_request(
        c("2060-01-01", "2060-01-04"),
        request_id = c("one", "four")
    )

    # resume=FALSE retains progress from failed endpoint, then refreshes on a second fresh call
    d1 <- withr::local_tempdir()
    f1 <- qr_test__dap_files(
        srv,
        c("bad-bounds.nc", "fast-bounds.nc"),
        nodes = c("first", "second")
    )
    expect_true(qr_test__fetch(f1, req, d1, resume = FALSE)$complete)
    att <- subset_status(d1, "attempts")
    expect_equal(sum(att$native_values), 8)
    expect_identical(
        att$data_node[att$native_values > 0],
        c("first", rep("second", 4L))
    )
    raw <- read_subset(d1)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_equal(sort(unique(raw$values$bound_start_value)), c(0, 3))
    expect_true(qr_test__fetch(f1, req, d1, resume = FALSE)$complete)
    expect_equal(sum(subset_status(d1, "attempts")$native_values), 16)

    # Incompatible bounds replica is rejected without reading weather or full time_bnds
    qr_test__dap_control(srv, "reset")
    d2 <- withr::local_tempdir()
    f2 <- qr_test__dap_files(
        srv,
        c("bad-bounds.nc", "wrong-bounds.nc", "fast-bounds.nc"),
        nodes = c("partial", "wrong-bounds", "consistent")
    )
    expect_true(qr_test__fetch(f2, req, d2, resume = FALSE)$complete)
    att2 <- subset_status(d2, "attempts")
    expect_true(any(
        att2$data_node == "wrong-bounds" & att2$error_kind == "source_metadata"
    ))
    expect_equal(sum(att2$native_values), 8)
    tr <- qr_test__dap_traffic(srv)
    expect_false(any(grepl("wrong-bounds.nc.dods tas", tr, fixed = TRUE)))
    breads <- tr[grepl("[.]dods time_bnds", tr)]
    expect_true(length(breads) > 0L)
    expect_false(any(grepl("time_bnds%5b0:7%5d", breads, fixed = TRUE)))

    # Without a consistent fallback, incompatible bounds or units leave explicit partial gaps
    for (spec in list(
        list(
            eps = c("bad-bounds.nc", "wrong-bounds.nc"),
            nodes = c("partial", "wrong-bounds"),
            gap = FALSE
        ),
        list(
            eps = c("bad.nc", "wrong.nc"),
            nodes = c("partial", "wrong-units"),
            gap = TRUE
        )
    )) {
        qr_test__dap_control(srv, "reset")
        db <- withr::local_tempdir()
        fb <- qr_test__dap_files(srv, spec$eps, nodes = spec$nodes)
        expect_false(qr_test__fetch(fb, req, db)$complete)
        ab <- subset_status(db, "attempts")
        bad <- ab[ab$data_node == spec$nodes[[2L]], ]
        expect_true(all(!bad$success))
        expect_identical(bad$error_kind, "source_metadata")
        pr <- read_subset(db)
        expect_equal(sort(pr$values$value), 1:4)
        if (spec$gap) expect_true(nrow(pr$gaps$incomplete_sources) > 0L)
    }
})

# Disabling old coverage reuse must retain its scientific interval identity.
test_that("raw subset refresh validates saved native bounds without reusing coverage", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    req <- qr_test__dap_request()
    f0 <- qr_test__dap_files(srv, "fast-bounds.nc")
    expect_true(qr_test__fetch(f0, req, d, network_id = "original")$complete)
    orig <- read_subset(d)
    f <- qr_test__dap_files(
        srv,
        c("wrong-bounds.nc", "fast-bounds.nc"),
        nodes = c("wrong-bounds", "consistent")
    )

    assert_valid <- function(n_before) {
        att <- subset_status(d, "attempts")[-seq_len(n_before)]
        expect_identical(unique(att$data_node), c("wrong-bounds", "consistent"))
        expect_identical(att$error_kind[[1L]], "source_metadata")
        expect_equal(att$native_values[att$data_node == "wrong-bounds"], 0)
        expect_equal(sum(att$native_values), 4)
        s <- read_subset(d)
        expect_equal(sort(s$values$value), 1:4)
        expect_equal(unique(s$values$bound_start_value), 0)
        expect_equal(unique(s$values$bound_end_value), 1)
        s
    }

    n0 <- nrow(subset_status(d, "attempts"))
    expect_true(
        qr_test__fetch(
            f,
            req,
            d,
            resume = FALSE,
            network_id = "refresh"
        )$complete
    )
    saved <- assert_valid(n0)
    expect_equal(
        saved$values[, c("bound_start_value", "bound_end_value")],
        orig$values[, c("bound_start_value", "bound_end_value")]
    )
    tr <- qr_test__dap_traffic(srv)
    expect_false(any(grepl("wrong-bounds.nc.dods tas", tr, fixed = TRUE)))
    expect_equal(sum(grepl("fast-bounds.nc.dods tas", tr, fixed = TRUE)), 2L)

    for (damage in c("missing", "corrupt")) {
        blk <- subset_status(d, "blocks")
        if (damage == "missing") {
            unlink(blk$path)
        } else {
            writeBin(as.raw(0), blk$path)
        }
        n0 <- nrow(subset_status(d, "attempts"))
        expect_true(
            qr_test__fetch(
                f,
                req,
                d,
                network_id = paste0("repair-", damage)
            )$complete
        )
        assert_valid(n0)
        if (damage == "corrupt") {
            expect_false(identical(subset_status(d, "blocks")$path, blk$path))
            expect_identical(readBin(blk$path, "raw", n = 1L), as.raw(0))
        }
    }
    tr <- qr_test__dap_traffic(srv)
    expect_false(any(grepl("wrong-bounds.nc.dods tas", tr, fixed = TRUE)))
    expect_equal(sum(grepl("fast-bounds.nc.dods tas", tr, fixed = TRUE)), 4L)
})
# }}}

# G3. Directory completion & partial directory provenance {{{
# An earlier contiguous block covers both requested days, but refreshing just
# one day cannot authorize that older block during directory completion.
test_that("raw subset fresh invocation excludes old blocks across directory phases", {
    local_cache_mode("off")
    # One initial contiguous read succeeds; fresh call commits one day, then fails
    srv <- qr_test__dap_server(init = function(app) app$locals$weather <- -1L)
    docs <- qr_test__dap_docs(srv, c("bad.nc", "fast.nc"), distinct = TRUE)
    idx <- qr_test__index_server(docs)
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", idx$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs[1L, ], num_found = 2L)
    )
    d <- withr::local_tempdir()
    r0 <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-04",
        request_id = "initial"
    )
    expect_true(qr_test__fetch(partial$slice(1L), r0, d)$complete)
    expect_equal(sum(subset_status(d, "attempts")$native_values), 16)

    req <- qr_test__dap_request(
        c("2060-01-01", "2060-01-04"),
        request_id = c("one", "four")
    )
    res <- qr_test__fetch(partial, req, d, discover = TRUE, resume = FALSE)
    expect_true(res$directory_complete)
    expect_false(res$complete)
    att <- subset_status(d, "attempts")
    expect_equal(sum(att$native_values), 28)
    expect_gte(sum(!att$success & grepl("bad.nc", att$url, fixed = TRUE)), 2L)
})

# An original unsliced File query can finish its pages after known weather
# reads start. The frozen completion is reused by the same original input.
test_that("raw subset directory completion preserves explicit selection and local resume", {
    local_cache_mode("off")
    srv <- qr_test__dap_server()
    docs <- qr_test__dap_docs(srv, c("fast.nc", "fast.nc"), distinct = TRUE)
    changed <- docs
    changed$id <- c("different-1", "different-2")
    changed$instance_id <- c("different-instance-1", "different-instance-2")
    changed$master_id <- c("different-master-1", "different-master-2")
    conflict <- docs
    conflict$checksum <- "conflicting-published-content"

    idx <- qr_test__index_server(
        docs,
        changed = changed,
        conflict = conflict,
        weather_server = srv
    )
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", idx$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs[1L, ], num_found = 2L)
    )
    req <- qr_test__dap_request()
    d <- withr::local_tempdir()
    q_now <- function() qr_test__dap_control(idx, "queries")
    n_keys <- function(p) length(unique(read_subset(p)$links$source_key))

    first <- qr_test__fetch(partial, req, d, discover = TRUE)
    expect_true(first$complete && first$directory_complete)
    expect_equal(n_keys(d), 2L)
    expect_equal(nrow(read_subset(d)$values), 8L)

    queries <- q_now()
    ev <- jsonlite::fromJSON(queries)
    expect_equal(nrow(ev), 2L)
    expect_true(all(unlist(ev$known_read_started)))
    expect_true(
        all(ev$query$type == "File") && all(ev$query$project == "CMIP6")
    )

    before <- subset_status(d, "attempts")
    expect_true(
        qr_test__fetch(partial, req, d, discover = TRUE)$directory_complete
    )
    expect_identical(subset_status(d, "attempts"), before)
    expect_identical(q_now(), queries)
    expect_equal(n_keys(d), 2L)

    # Explicit slice() and complete catalogs freeze science inputs without re-querying
    for (sel in list(
        partial$slice(1L),
        query_result_test_object("File", docs)$slice(1L)
    )) {
        sd <- withr::local_tempdir()
        qr_test__fetch(sel, req, sd, discover = TRUE)
        expect_equal(n_keys(sd), 1L)
        expect_identical(q_now(), queries)
    }

    # Legacy contextless saved partial also freezes without directory completion
    sp <- withr::local_tempfile(fileext = ".json")
    partial$save(sp)
    old <- jsonlite::read_json(sp, simplifyVector = FALSE)
    old$context$directory_collection <- NULL
    jsonlite::write_json(old, sp, auto_unbox = TRUE, null = "null")
    od <- withr::local_tempdir()
    expect_false(
        qr_test__fetch(
            esg_result("file")$load(sp),
            req,
            od,
            discover = TRUE
        )$directory_complete
    )
    expect_equal(n_keys(od), 1L)
    expect_identical(q_now(), queries)

    # resume=FALSE refreshes each source once across the two phases
    rd <- withr::local_tempdir()
    full <- partial$clone(deep = TRUE)
    priv(full)$response$response$docs <- docs
    qr_test__fetch(full, req, rd, discover = TRUE)
    expect_identical(q_now(), queries)
    prior <- sum(subset_status(rd, "attempts")$native_values)
    expect_true(
        qr_test__fetch(
            partial,
            req,
            rd,
            discover = TRUE,
            resume = FALSE
        )$directory_complete
    )
    expect_equal(sum(subset_status(rd, "attempts")$native_values) - prior, 8)

    # Damaged frozen completion + changed index retains data and exposes directory gap
    frozen <- list.files(
        file.path(d, "subsets"),
        "^directory-.*[.]rds$",
        full.names = TRUE
    )
    expect_length(frozen, 1L)
    writeBin(as.raw(0), frozen)
    qr_test__dap_control(idx, "change")
    att <- subset_status(d, "attempts")
    expect_warning(
        cres <- qr_test__fetch(partial, req, d, discover = TRUE, timeout = 1),
        "Ignoring invalid saved File directory"
    )
    expect_false(cres$directory_complete)
    expect_true(cres$complete)
    expect_identical(subset_status(d, "attempts"), att)
    expect_equal(qr_test__vals(d), 1:4)
    expect_equal(nrow(read_subset(d)$gaps$directory), 1L)

    # Published content contradictions are source errors, preserving phase-1 native data
    qr_test__dap_control(idx, "conflict")
    cd <- withr::local_tempdir()
    expect_error(
        qr_test__fetch(partial, req, cd, discover = TRUE),
        "Conflicting published content metadata"
    )
    expect_equal(qr_test__vals(cd), 1:4)

    # Index unavailability cannot prevent a known healthy File from finishing
    unavail <- partial$clone(deep = TRUE)
    priv(unavail)$index_node <- "http://127.0.0.1:1"
    fd <- withr::local_tempdir()
    failed <- qr_test__fetch(unavail, req, fd, discover = TRUE, timeout = 2)
    expect_true(failed$complete && !failed$directory_complete)
    expect_equal(nrow(read_subset(fd)$gaps$directory), 1L)
    expect_equal(qr_test__vals(fd), 1:4)
})

# A complete selected File can be reusable while its originating catalogue is
# still partial. Directory gaps stay visible offline and after cache-only resume.
test_that("raw subset reports partial directories independently of native completion", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    docs <- qr_test__dap_docs(srv, "fast.nc")
    req <- qr_test__dap_request()
    idx <- qr_test__index_server(docs, paginate = FALSE)
    partial <- query_result__new(
        EsgResultFile,
        index_node = sub("/$", "", idx$url()),
        params = query_result_test_params("File"),
        result = esgf_test__response(docs, num_found = 2L),
        context = NULL
    )
    expect_false(attr(
        partial$access_plan(expand = FALSE),
        "directory_complete"
    ))
    expect_false(attr(partial$access_plan(), "directory_complete"))
    first <- qr_test__fetch(partial, req, d)
    expect_true(first$complete && !first$directory_complete)
    expect_false(subset_status(d)$directory_complete)
    expect_equal(read_subset(d)$gaps$directory$request_id, "one")

    before <- subset_status(d, "attempts")
    expect_false(qr_test__fetch(partial, req, d)$directory_complete)
    expect_identical(subset_status(d, "attempts"), before)

    all_docs <- docs[c(1L, 1L), ]
    all_docs$id[[2L]] <- "other-file"
    all_docs$instance_id[[2L]] <- "other-file-instance"
    sel <- query_result_test_object("File", all_docs)$slice(1L)
    expect_true(attr(sel$access_plan(expand = FALSE), "directory_complete"))
    res <- qr_test__fetch(sel, req, d)
    expect_true(res$complete && res$directory_complete)
    expect_true(subset_status(d)$directory_complete)
    expect_equal(nrow(read_subset(d)$gaps$directory), 0L)
    expect_identical(subset_status(d, "attempts"), before)
    expect_true(attr(
        sel$slice(integer())$access_plan(expand = FALSE),
        "directory_complete"
    ))

    toggle_view <- function(enable, drop_dir = FALSE) {
        sql <- if (isTRUE(enable)) {
            "ALTER TABLE subset_links RENAME TO saved_links; CREATE VIEW subset_links AS SELECT * FROM saved_links"
        } else {
            paste0(
                "DROP VIEW subset_links; ALTER TABLE saved_links RENAME TO subset_links",
                if (isTRUE(drop_dir)) "; DROP TABLE subset_directory" else ""
            )
        }
        qr_test__ddb_exec(d, sql)
    }
    toggle_view(TRUE)
    changed <- req
    changed$lon <- 10.75
    expect_error(
        partial$fetch_subset(changed, d, discover = TRUE),
        "View|view|base table"
    )
    expect_true(subset_status(d)$directory_complete)
    expect_equal(read_subset(d)$links$lon, 10.25)
    toggle_view(FALSE)

    preq <- qr_test__dap_request(request_id = "other", site_id = "other-city")
    partial$fetch_subset(preq, d, discover = TRUE)
    expect_false(subset_status(d)$directory_complete)
    expect_equal(nrow(read_subset(d, site_id = "city")$gaps$directory), 0L)
    expect_equal(
        read_subset(d, site_id = "other-city")$gaps$directory$request_id,
        "other"
    )

    # Model an older persisted schema without subset_directory after a failed write
    gc(FALSE)
    toggle_view(TRUE)
    expect_error(
        partial$fetch_subset(changed, d, discover = TRUE),
        "View|view|base table"
    )
    toggle_view(FALSE, drop_dir = TRUE)
    gc(FALSE)
    expect_true(is.na(subset_status(d)$directory_complete))
    expect_true(all(is.na(read_subset(d)$gaps$directory$directory_complete)))
    sel$fetch_subset(req, d, discover = TRUE)
    expect_true(is.na(subset_status(d)$directory_complete))
    expect_equal(nrow(read_subset(d, site_id = "city")$gaps$directory), 0L)
    expect_identical(subset_status(d, "attempts"), before)
    empty <- read_subset(d, site_id = "absent")
    expect_equal(
        c(nrow(empty$values), nrow(empty$links), nrow(empty$gaps$directory)),
        c(0L, 0L, 0L)
    )
})
# }}}

# G4. Worker timeouts, progress extension & bootstrap cleanup {{{
# A blocked remote weather read retires its own reader; an unrelated source
# uses another slot and remains available through the same public result.
test_that("raw subset timeouts release workers while unrelated sources finish", {
    slow <- qr_test__dap_server(shared = FALSE)
    healthy <- qr_test__dap_server()
    d <- withr::local_tempdir()
    docs <- query_result_test_file_docs()[c(1L, 1L), ]
    docs$master_id <- docs$instance_id <- c("blocked-source", "healthy-source")
    docs$variable_id <- "tas"
    docs$data_node <- c("blocked", "healthy")
    docs$url <- I(list(
        paste0(slow$url(), "hang.nc|application/netcdf|OPENDAP"),
        paste0(healthy$url(), "fast.nc|application/netcdf|OPENDAP")
    ))
    owned_before <- vapply(
        ps::ps_children(ps::ps_handle()),
        ps::ps_pid,
        integer(1L)
    )
    res <- qr_test__fetch(
        query_result_test_object("File", docs),
        qr_test__dap_request(),
        d,
        workers = 2L,
        timeout = 0.8
    )
    expect_false(res$complete)
    expect_equal(sort(res$jobs$status), c("done", "failed"))
    att <- subset_status(d, "attempts")
    expect_true(any(att$data_node == "blocked" & att$error_kind == "transport"))
    expect_equal(qr_test__vals(d), 1:4)
    after <- ps::ps_children(ps::ps_handle())
    expect_true(all(vapply(after, ps::ps_pid, integer(1L)) %in% owned_before))
})

# Saved-block progress keeps a healthy native reader alive beyond stall timeout.
test_that("raw subset block progress extends a healthy reader beyond the stall timeout", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "progress.nc")
    req <- qr_test__dap_request(
        sprintf("2060-01-%02d", c(1, 3, 5, 7)),
        request_id = paste0("day-", 1:4)
    )
    expect_true(qr_test__fetch(f, req, d, timeout = 1.2)$complete)
    expect_equal(qr_test__vals(d), c(1:4, 9:12, 17:20, 25:28))
    att <- subset_status(d, "attempts")
    expect_true(all(att$success))
    expect_true(sum(att$elapsed_seconds[att$native_values > 0]) > 1.2)
})

# Bootstrap failure after a real worker is running retires its exact owned profile.
test_that("raw subset worker bootstrap failure retires its owned profile", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast.nc")
    req <- qr_test__dap_request()
    handle <- profile <- NULL
    withr::defer({
        if (!is.null(handle) && ps::ps_is_running(handle)) {
            ps::ps_kill(handle)
        }
        if (!is.null(profile)) mirai::daemons(0L, .compute = profile)
    })
    local({
        daemons <- mirai::daemons
        local_mocked_bindings(
            daemons = function(n, ..., .compute = NULL) {
                if (!is.null(n) && n > 0L && !is.null(.compute)) {
                    profile <<- .compute
                }
                daemons(n, ..., .compute = .compute)
            },
            .package = "mirai"
        )
        command <- ps::ps_cmdline
        local_mocked_bindings(
            ps_cmdline = function(p = ps::ps_handle()) {
                val <- command(p)
                if (is.null(handle) && ps::ps_pid(p) != Sys.getpid()) {
                    handle <<- p
                    stop("controlled bootstrap identity failure", call. = FALSE)
                }
                val
            },
            .package = "ps"
        )
        expect_error(
            qr_test__fetch(f, req, d),
            "controlled bootstrap identity failure"
        )
    })
    expect_false(is.null(handle))
    deadline <- proc.time()[["elapsed"]] + 5
    while (ps::ps_is_running(handle) && proc.time()[["elapsed"]] < deadline) {
        Sys.sleep(.01)
    }
    expect_false(ps::ps_is_running(handle))
    expect_identical(mirai::status(.compute = profile)$connections, 0L)
    expect_identical(mirai::status(.compute = profile)$daemons, 0L)
    expect_equal(nrow(subset_status(d, "attempts")), 0L)
})
# }}}

# G5. Immutable publication, receipt adoption & rollback {{{
# Interrupting after pending receipt is durable adopts existing data on restart.
test_that("raw subset restart adopts published blocks before manifest registration", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast.nc")
    req <- qr_test__dap_request()
    local(qr_test__interrupt_pending(
        qr_test__fetch(f, req, d),
        "controlled filesystem interruption"
    ))
    before <- length(qr_test__dap_traffic(srv, weather_only = TRUE))
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_equal(qr_test__vals(d), 1:4)
    expect_equal(length(qr_test__dap_traffic(srv, weather_only = TRUE)), before)
    expect_equal(sum(subset_status(d, "attempts")$native_values), 0)
})

# Repeated publication never deletes committed demand; sidecar loss preserves it.
test_that("raw subset receipt publication preserves committed demand coverage", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast.nc")
    req <- qr_test__dap_request(case_id = "original")
    receipt <- NULL
    interrupted <- FALSE
    local({
        rename <- base::file.rename
        local_mocked_bindings(
            file.rename = function(from, to) {
                if (grepl("/demand-[^/]+[.]rds$", to)) {
                    if (identical(to, receipt)) {
                        interrupted <<- TRUE
                        stop(
                            "controlled repeated receipt publication",
                            call. = FALSE
                        )
                    }
                    receipt <<- to
                }
                rename(from, to)
            },
            .package = "base"
        )
        tryCatch(
            qr_test__fetch(f, req, d),
            error = function(e) {
                if (
                    !grepl("controlled repeated receipt", conditionMessage(e))
                ) {
                    stop(e)
                }
            }
        )
    })
    expect_true(file.exists(receipt))
    expect_equal(qr_test__vals(d), 1:4)
    expect_equal(nrow(read_subset(d)$gaps$incomplete_requests), 0L)
    expect_false(interrupted)

    before <- subset_status(d, "attempts")
    req$request_id <- req$case_id <- "linked"
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_identical(subset_status(d, "attempts"), before)
    saved <- read_subset(d, case_id = "linked")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$links$request_id), "linked")

    unlink(paste0(receipt, ".json"))
    local({
        rename <- base::file.rename
        local_mocked_bindings(
            file.rename = function(from, to) {
                if (identical(to, receipt)) {
                    stop("committed receipt replacement")
                }
                rename(from, to)
            },
            .package = "base"
        )
        expect_true(qr_test__fetch(f, req, d, resume = FALSE)$complete)
    })
    expect_equal(qr_test__vals(d, case_id = "linked"), 1:4)
})

# Pre-pending interruption, refresh adoption, and DuckDB transaction rollback preserve prior cases.
test_that("raw subset refresh preserves committed artifacts before pending publication", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast.nc")
    orig <- qr_test__dap_request(request_id = "original", case_id = "original")
    expect_true(qr_test__fetch(f, orig, d)$complete)
    committed <- subset_status(d, "blocks")
    expect_equal(nrow(committed), 1L)
    previous <- read_subset(d, case_id = "original")

    changed <- qr_test__dap_request(
        request_id = "changed",
        case_id = "changed",
        lon = 10.75
    )
    meta_written <- NULL
    local({
        write <- store__extract_cache_write
        local_mocked_bindings(
            store__extract_cache_write = function(path, payload) {
                out <- write(path, payload)
                if (
                    startsWith(basename(path), committed$block_id[[1L]]) &&
                        endsWith(path, ".rds")
                ) {
                    meta_written <<- path
                    stop("controlled pre-pending interruption", call. = FALSE)
                }
                out
            }
        )
        expect_error(
            qr_test__fetch(f, changed, d, resume = FALSE),
            "controlled pre-pending interruption"
        )
    })
    expect_true(file.exists(meta_written))
    expect_equal(readRDS(meta_written)$neighborhoods$target_lon, rep(10.75, 4L))
    expect_length(list.files(file.path(d, "subsets"), "^pending-"), 0L)
    expect_identical(checksum_file(committed$path), committed$sha256[[1L]])
    expect_identical(
        checksum_file(committed$metadata_path),
        committed$metadata_sha256[[1L]]
    )
    retained <- subset_status(d, "blocks")
    expect_identical(retained$path, committed$path)
    expect_identical(retained$metadata_path, committed$metadata_path)
    saved <- read_subset(d, case_id = "original")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$values$cf_day), 1L)
    expect_equal(saved$neighborhoods, previous$neighborhoods)
    expect_equal(nrow(saved$gaps$incomplete_sources), 0L)
    expect_equal(nrow(saved$gaps$incomplete_requests), 0L)
    expect_gt(
        nrow(read_subset(d, case_id = "changed")$gaps$incomplete_requests),
        0L
    )

    ref <- subset_status(d, "coverage")[request_id == "changed"]
    expect_true(ref$axis_known && !ref$complete)
    expect_equal(
        c(
            ref$expected_native_values,
            ref$corner_count,
            ref$available_native_values
        ),
        c(4, 4, 0)
    )

    att <- subset_status(d, "attempts")
    tr <- qr_test__dap_control(srv, "requests")
    qr_test__fetch(f, orig, d)
    expect_identical(subset_status(d, "attempts"), att)
    expect_identical(qr_test__dap_control(srv, "requests"), tr)

    # Refresh interruption after pending receipt adopts refreshed descriptor on restart
    local(qr_test__interrupt_pending(
        qr_test__fetch(f, changed, d, resume = FALSE),
        "controlled pending refresh interruption"
    ))
    expect_identical(
        checksum_file(committed$metadata_path),
        committed$metadata_sha256[[1L]]
    )
    att <- subset_status(d, "attempts")
    tr <- qr_test__dap_control(srv, "requests")
    expect_true(qr_test__fetch(f, changed, d)$complete)
    adopted <- subset_status(d, "blocks")
    expect_identical(adopted$block_id, committed$block_id)
    expect_false(identical(adopted$metadata_path, committed$metadata_path))
    expect_equal(
        readRDS(adopted$metadata_path)$neighborhoods$target_lon,
        rep(10.75, 4L)
    )
    expect_identical(subset_status(d, "attempts"), att)
    expect_identical(qr_test__dap_control(srv, "requests"), tr)
    expect_equal(qr_test__vals(d, case_id = "changed"), 1:4)
    expect_equal(qr_test__vals(d, case_id = "original"), 1:4)

    # DuckDB transaction rollback on subset_blocks preserves earlier cases intact
    rb <- qr_test__dap_request(
        request_id = "rollback",
        case_id = "rollback",
        lon = 10.5
    )
    local({
        append <- ddb_append_table
        local_mocked_bindings(
            ddb_append_table = function(conn, table, rows, ...) {
                out <- append(conn, table, rows, ...)
                if (identical(table, "subset_blocks")) {
                    stop("controlled block transaction rollback", call. = FALSE)
                }
                out
            }
        )
        expect_error(
            qr_test__fetch(f, rb, d, resume = FALSE),
            "controlled block transaction rollback"
        )
    })
    expect_identical(subset_status(d, "blocks"), adopted)
    expect_identical(checksum_file(adopted$path), adopted$sha256[[1L]])
    expect_identical(
        checksum_file(adopted$metadata_path),
        adopted$metadata_sha256[[1L]]
    )
    expect_equal(qr_test__vals(d, case_id = "original"), 1:4)
    expect_equal(qr_test__vals(d, case_id = "changed"), 1:4)
})

# Check idempotent receipt registration without weakening corruption or rollback guards.
test_that("serial overhead fixes register only changed demand receipts", {
    d <- withr::local_tempdir()
    store <- opendap__open(d)
    withr::defer(store$close())
    conn <- priv(store)$conn
    dem <- data.table::data.table(
        source_key = "serial-source",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-08",
        native_ranges = "[[1,8]]"
    )
    data.table::set(dem, j = "demand_id", value = opendap__demand_ids(dem))
    appended <- 0L
    append <- ddb_append_table
    local_mocked_bindings(
        .package = "epwshiftr",
        ddb_append_table = function(conn, table, rows, ...) {
            if (table == "subset_demands") {
                appended <<- appended + nrow(rows)
            }
            append(conn, table, rows, ...)
        }
    )
    opendap__save_demands(conn, dem, d)
    first <- ddb_read_table(conn, "subset_demands")
    sha <- tools::sha256sum(first$receipt_path)
    opendap__save_demands(conn, dem, d)
    expect_identical(appended, 1L)
    expect_identical(ddb_read_table(conn, "subset_demands"), first)
    expect_identical(tools::sha256sum(first$receipt_path), sha)

    changed <- data.table::copy(dem)
    data.table::set(changed, j = "native_ranges", value = "[[1,4]]")
    opendap__save_demands(conn, changed, d)
    expect_identical(appended, 2L)
    expect_true(file.exists(first$receipt_path))
    latest <- ddb_read_table(conn, "subset_demands")
    expect_false(identical(latest$receipt_path, first$receipt_path))

    writeBin(as.raw(1L), latest$receipt_path)
    opendap__save_demands(conn, changed, d)
    expect_identical(
        unname(tools::sha256sum(latest$receipt_path)),
        latest$receipt_sha256
    )
    expect_identical(appended, 2L)
    opendap__save_demands(conn, dem[0L], d)
    expect_identical(ddb_read_table(conn, "subset_demands"), latest)

    data.table::set(changed, j = "native_ranges", value = "[[1,2]]")
    with_mocked_bindings(
        expect_error(opendap__save_demands(conn, changed, d), "append failure"),
        ddb_append_table = function(conn, table, rows, ...) {
            if (table == "subset_demands") {
                stop("append failure")
            }
            append(conn, table, rows, ...)
        },
        .package = "epwshiftr"
    )
    expect_identical(ddb_read_table(conn, "subset_demands"), latest)
    expect_identical(
        unname(tools::sha256sum(latest$receipt_path)),
        latest$receipt_sha256
    )
})
# }}}

# G6. Per-case coverage, content evidence & multi-city sharing {{{
# Historical demand receipts keep gaps visible per case, reject empty windows,
# recover corrupt demand receipts without weather rereads, and reject conflicts.
test_that("raw subset coverage follows each case, rejects empty native windows, and rejects conflicting content", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    docs <- qr_test__dap_docs(srv, "fast.nc")
    f <- query_result_test_object("File", docs)
    a <- qr_test__dap_request("2060-01-01", request_id = "A", case_id = "A")
    expect_true(qr_test__fetch(f, a, d)$complete)

    unlink(subset_status(d, "blocks")$path[[1L]])
    b <- qr_test__dap_request("2060-01-02", request_id = "B", case_id = "B")
    expect_true(qr_test__fetch(f, b, d)$complete)
    expect_equal(qr_test__vals(d, case_id = "B"), 5:8)
    hist_a <- read_subset(d, case_id = "A")
    expect_equal(nrow(hist_a$values), 0L)
    expect_true(nrow(hist_a$gaps$incomplete_sources) > 0L)

    c_req <- qr_test__dap_request("2060-01-20", request_id = "C", case_id = "C")
    expect_false(qr_test__fetch(f, c_req, d)$complete)
    expect_true(
        nrow(read_subset(d, case_id = "C")$gaps$incomplete_sources) > 0L
    )

    before <- sum(subset_status(d, "attempts")$native_values)
    expect_true(qr_test__fetch(f, a, d)$complete)
    expect_equal(sum(subset_status(d, "attempts")$native_values) - before, 4)
    expect_equal(qr_test__vals(d, case_id = "A"), 1:4)
    expect_equal(
        nrow(read_subset(d, case_id = "B")$gaps$incomplete_sources),
        0L
    )

    for (p in list.files(
        file.path(d, "subsets"),
        "^demand-.*[.]rds$",
        full.names = TRUE
    )) {
        writeBin(as.raw(0), p)
    }
    expect_true(
        nrow(read_subset(d, case_id = "A")$gaps$incomplete_sources) > 0L
    )
    before <- sum(subset_status(d, "attempts")$native_values)
    expect_true(qr_test__fetch(f, a, d)$complete)
    expect_equal(sum(subset_status(d, "attempts")$native_values), before)

    # Local block hashes cannot override contradictory published checksum evidence
    bad_docs <- docs
    bad_docs$checksum <- "contradictory-published-sha256"
    expect_error(
        qr_test__fetch(query_result_test_object("File", bad_docs), a, d),
        "Conflicting published content"
    )
    expect_equal(qr_test__vals(d, case_id = "A"), 1:4)
})

# Many distinct target coordinates share the same enclosing cell without extra I/O.
test_that("raw subset acquisition shares native coverage across many overlapping cities", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast-bounds.nc")
    req <- qr_test__dap_request(
        rep(c("2060-01-01", "2060-01-04"), 40L),
        request_id = paste0("request-", 1:80),
        site_id = rep(paste0("city-", 1:40), each = 2L),
        lon = rep(seq(10.1, 10.9, length.out = 40L), each = 2L)
    )
    expect_true(qr_test__fetch(f, req, d)$complete)
    raw <- read_subset(d)
    expect_equal(sort(raw$values$value), c(1:4, 13:16))
    expect_equal(nrow(raw$neighborhoods), 320L)
    expect_equal(sum(subset_status(d, "attempts")$native_values), 8)
    expect_length(qr_test__dap_traffic(srv, weather_only = TRUE), 2L)
})
# }}}

# G7. Native File periods & non-Gregorian/subdaily DRS {{{
# Published subannual periods exclude unrelated Files before native OPeNDAP reads.
test_that("raw subset windows match native File periods at their published precision", {
    srv <- qr_test__dap_server()
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(
        srv,
        c("fast.nc", "june.nc"),
        ids = c("january", "june"),
        titles = c("tas_206001-206001.nc", "tas_206006-206006.nc")
    )
    req <- qr_test__dap_request(
        "2060-06-01",
        request_id = "june",
        case_id = "summer"
    )
    expect_true(qr_test__fetch(f, req, d)$complete)
    saved <- read_subset(d, case_id = "summer")
    expect_equal(sort(saved$values$value), 1:4)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_equal(unique(saved$values$cf_month), 6L)
    expect_equal(nrow(saved$neighborhoods), 4L)
    expect_equal(nrow(saved$gaps$incomplete_requests), 0L)

    att <- subset_status(d, "attempts")
    expect_identical(unique(att$url), paste0(srv$url(), "june.nc"))
    tr <- qr_test__dap_control(srv, "requests")
    expect_false(any(grepl("fast.nc", jsonlite::fromJSON(tr), fixed = TRUE)))

    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_identical(subset_status(d, "attempts"), att)
    expect_identical(qr_test__dap_control(srv, "requests"), tr)

    mreq <- qr_test__dap_request(
        "2060-05-01",
        request_id = "missing",
        case_id = "missing"
    )
    expect_false(qr_test__fetch(f, mreq, d)$complete)
    expect_gt(
        nrow(read_subset(d, case_id = "missing")$gaps$incomplete_requests),
        0L
    )
    expect_identical(subset_status(d, "attempts"), att)
    expect_identical(qr_test__dap_control(srv, "requests"), tr)

    sreq <- qr_test__dap_request(
        "2060-01-01",
        "2060-06-01",
        request_id = "spanning",
        case_id = "spanning"
    )
    expect_true(qr_test__fetch(f, sreq, d)$complete)
    span <- read_subset(d, case_id = "spanning")
    expect_equal(sort(span$values$value), sort(c(1:32, 1:4)))
    expect_equal(sort(unique(span$values$cf_month)), c(1L, 6L))
})

# DRS boundaries retain non-Gregorian (Feb 30) and subdaily endpoints; malformed labels fall back to CF.
test_that("raw subset File windows retain nonGregorian and subdaily endpoints", {
    srv <- qr_test__dap_server()
    root <- withr::local_tempdir()
    ex <- data.frame(
        first = c(
            "20600229",
            "2060010100",
            "206001010000",
            "20600101000000",
            "20600101"
        ),
        last = c(
            "20600230",
            "2060010103",
            "206001010003",
            "20600101000003",
            "20600104"
        ),
        origin = c(
            "february.nc",
            "hour.nc",
            "minute.nc",
            "second.nc",
            "fast.nc"
        ),
        endpoint = c(
            "2060-02-30",
            "2060-01-01T03:00:00Z",
            "2060-01-01T00:03:00Z",
            "2060-01-01T00:00:03Z",
            "2060-01-04"
        ),
        value = c(29L, 13L, 13L, 13L, 13L)
    )
    for (i in seq_len(nrow(ex))) {
        d <- file.path(root, as.character(i))
        f <- qr_test__dap_files(
            srv,
            c(paste0("outside-", ex$origin[[i]]), ex$origin[[i]]),
            ids = c("outside", "inside"),
            titles = sprintf(
                "tas_%s-%s.nc",
                c(ex$first[[i]], ex$last[[i]]),
                c(ex$first[[i]], ex$last[[i]])
            )
        )
        req <- qr_test__dap_request(
            ex$endpoint[[i]],
            request_id = "native",
            case_id = "native"
        )
        expect_true(qr_test__fetch(f, req, d)$complete)
        expect_equal(qr_test__vals(d, case_id = "native"), ex$value[[i]] + 0:3)
        expect_identical(
            unique(subset_status(d, "attempts")$url),
            paste0(srv$url(), ex$origin[[i]])
        )
        expect_equal(
            f$access_plan(expand = FALSE)$file_start_year,
            c(2060L, 2060L)
        )
    }

    for (label in c(
        "unknown.nc",
        "tas_206013-206013.nc",
        "tas_206006-206001.nc"
    )) {
        d <- file.path(root, label)
        f <- qr_test__dap_files(srv, "june.nc", titles = label)
        req <- qr_test__dap_request("2060-06-01", request_id = "unknown")
        expect_true(qr_test__fetch(f, req, d)$complete)
        expect_equal(qr_test__vals(d), 1:4)
    }
    expect_false(any(grepl(
        "outside-",
        qr_test__dap_traffic(srv),
        fixed = TRUE
    )))
})
# }}}

# G8. Transport budgets & cooldowns (unit + E2E) {{{
# Transport partitioning must not enter native identity.
test_that("acquisition transport budgets preserve raw corners and exact resume", {
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(srv, "fast-bounds.nc")
    req <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-08",
        request_id = "native",
        case_id = "case"
    )
    small <- withr::local_tempdir()
    large <- withr::local_tempdir()
    expect_true(qr_test__fetch(f, req, small, max_values = 8L)$complete)
    blocks <- subset_status(small, "blocks")
    expect_equal(nrow(blocks), 4L)

    canon <- function(v) {
        v <- data.table::copy(v)
        data.table::setorderv(v, c("ind_lon", "ind_lat", "time_index"))
        v
    }
    exp <- read_subset(small)
    exp$values <- canon(exp$values)
    expect_equal(nrow(exp$values), 32L)
    expect_equal(
        data.table::uniqueN(exp$values[, c("grid_lat", "grid_lon")]),
        4L
    )

    expect_true(qr_test__fetch(f, req, large, max_values = 32L)$complete)
    expect_equal(nrow(subset_status(large, "blocks")), 1L)
    act <- read_subset(large)
    expect_equal(canon(act$values), exp$values)
    expect_equal(act$neighborhoods, exp$neighborhoods)

    att <- subset_status(small, "attempts")
    expect_true(qr_test__fetch(f, req, small, max_values = 32L)$complete)
    expect_equal(subset_status(small, "attempts"), att)

    writeBin(as.raw(0), blocks$path[[2L]])
    expect_true(qr_test__fetch(f, req, small, max_values = 32L)$complete)
    expect_equal(canon(read_subset(small)$values), exp$values)
    expect_equal(
        sum(subset_status(small, "attempts")$native_values) -
            sum(att$native_values),
        8
    )
    expect_error(f$fetch_subset(req, small, max_values = 3L))
})

# File-specific absences cool only their URL; node quarantine requires 3
# independent corroborated service failures, caps backoff at 480s, and resets on native progress.
test_that("acquisition replica and node cooldowns scope failures, cap backoff, and reset on progress", {
    cands <- data.table::data.table(
        source_key = c("a", "b", "c"),
        url = c("u", "v", "w"),
        data_node = c("node", "node", "other")
    )
    cool <- function(
        rows,
        now = 1020,
        sec = 60,
        client = "client",
        cd = cands
    ) {
        opendap__cooling_until(cd, rows, client, sec, now)
    }

    h404 <- qr_test__attempt_history(error = "NetCDF: HTTP 404 Not Found")
    expect_equal(cool(h404), c(1480, 0, 0))
    expect_equal(cool(h404, client = "other"), c(0, 0, 0))
    expect_equal(cool(h404, sec = 0), c(0, 0, 0))
    expect_true(all(cool(h404, now = 1500) <= 1500))
    expect_equal(
        cool(transform(h404, error = "NetCDF: I/O failure")),
        c(1060, 0, 0)
    )
    expect_equal(
        cool(transform(h404, error_kind = "source_metadata")),
        c(0, 0, 0)
    )
    expect_length(cool(h404, cd = cands[0L]), 0L)

    svc <- qr_test__attempt_history(
        source_key = c("a", "d", "e"),
        url = c("u", "x", "y"),
        error = "connection reset"
    )
    expect_equal(cool(svc), c(1060, 1060, 0))
    ok <- qr_test__attempt_history(
        source_key = "f",
        url = "z",
        observed_at = 1001,
        native_values = 4,
        success = TRUE,
        error = NA_character_,
        error_kind = "data"
    )
    expect_equal(cool(rbind(svc, ok)), c(1060, 0, 0))
    expect_equal(
        cool(rbind(h404, transform(ok, source_key = "a", url = "u"))),
        c(0, 0, 0)
    )

    nc <- data.table::data.table(
        source_key = c("next", "other"),
        url = c("next", "other"),
        data_node = c("node", "other")
    )
    sh <- qr_test__attempt_history(
        source_key = c("a", "b", "c"),
        url = c("u", "v", "w"),
        observed_at = c(1000, 1090, 1180),
        elapsed_seconds = 90
    )
    ncool <- function(rows, now = 1181, sec = 60, cd = nc) {
        opendap__cooling_until(cd, rows, "client", sec, now)
    }
    expect_equal(ncool(sh), c(1240, 0))
    expect_equal(ncool(sh[1:2]), c(0, 0))
    expect_equal(ncool(transform(sh, source_key = "one")), c(0, 0))
    for (err in c(
        "NetCDF: DAP failure HTTP 404",
        "NetCDF: file not found",
        "NetCDF: DAP failure; HTTP/1.1 404",
        "NetCDF: I/O failure; HTTP/2 410"
    )) {
        expect_equal(ncool(transform(sh, error = err)), c(0, 0))
    }
    for (err in c(
        "execution deadline",
        "NetCDF: HTTP/1.1 503",
        "Failed to open OPeNDAP connection: NetCDF: I/O failure"
    )) {
        expect_equal(ncool(transform(sh, error = err)), c(1240, 0))
    }
    expect_equal(ncool(sh, sec = 0), c(0, 0))
    expect_true(all(ncool(sh, now = 2000) <= 2000))
    expect_equal(ncool(transform(sh, client = "other")), c(0, 0))
    expect_equal(ncool(transform(sh, error_kind = "source_metadata")), c(0, 0))

    sok <- transform(
        sh[1L],
        observed_at = 1180.5,
        success = TRUE,
        native_values = 4,
        error = NA_character_
    )
    expect_equal(ncool(rbind(sh, sok)), c(0, 0))
    expect_equal(
        ncool(rbind(sh, transform(sok, native_values = 0))),
        c(1240, 0)
    )

    th <- transform(
        sh,
        observed_at = c(1000, 1275, 1550),
        elapsed_seconds = 275
    )
    expect_equal(ncool(th, now = 1551, cd = nc[1L]), 1610)
    for (dur in c(NA_real_, Inf, -1)) {
        expect_equal(
            ncool(
                transform(th, elapsed_seconds = dur),
                now = 1551,
                cd = nc[1L]
            ),
            0
        )
    }
    lh <- qr_test__attempt_history(
        source_key = paste0("file", 1:9),
        url = paste0("url", 1:9),
        observed_at = 1001:1009
    )
    dl <- vapply(
        3:9,
        function(n) ncool(lh[seq_len(n)], now = 1010, cd = nc[1L]),
        numeric(1L)
    )
    expect_equal(dl, 1003:1009 + c(60, 120, 240, 480, 480, 480, 480))

    ep <- transform(lh, source_key = "one", url = "one")
    expect_equal(opendap__cooldown_deadline(ep, 60), 1489)
    ep_ok <- transform(
        ep[1L],
        observed_at = 1010,
        native_values = 4,
        success = TRUE
    )
    expect_equal(opendap__cooldown_deadline(rbind(ep, ep_ok), 60), 0)
    expect_equal(
        opendap__cooldown_deadline(
            transform(ep[1L], error = "NetCDF: DAP failure; HTTP/2 410"),
            60
        ),
        1481
    )
})

# Down service releases worker, cools client-side, and still permits replica discovery.
test_that("acquisition replica cooldown avoids hot retries and keeps later recovery possible", {
    skip_if_not_installed("webfakes")
    app <- webfakes::new_app()
    app$use(function(req, res) {
        Sys.sleep(.05)
        res$set_status(503L)$send("temporarily unavailable")
    })
    srv <- qr_test__dap_server(app = app)
    f <- qr_test__dap_files(srv, "offline.nc")
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "gap")
    d <- withr::local_tempdir()

    expect_false(
        qr_test__fetch(f, req, d, retries = 2L, replica_cooldown = 60)$complete
    )
    before <- subset_status(d, "attempts")
    expect_equal(sum(!before$success), 1L)
    expect_false(
        qr_test__fetch(f, req, d, retries = 2L, replica_cooldown = 60)$complete
    )
    expect_equal(subset_status(d, "attempts"), before)

    qr_test__ddb_exec(
        d,
        "UPDATE subset_attempts SET observed_at = observed_at - 61"
    )
    expect_false(qr_test__fetch(f, req, d, replica_cooldown = 60)$complete)
    expect_equal(sum(!subset_status(d, "attempts")$success), 2L)
    expect_error(f$fetch_subset(req, d, replica_cooldown = -1))

    healthy <- qr_test__dap_server()
    additions <- qr_test__dap_files(healthy, "fast.nc")$access_plan(
        expand = FALSE
    )
    discover <- function(job) additions
    environment(discover) <- list2env(
        list(additions = additions),
        parent = baseenv()
    )
    local_mocked_bindings(opendap__discover = discover)
    expect_true(
        qr_test__fetch(
            f,
            req,
            d,
            discover = TRUE,
            replica_cooldown = 60
        )$complete
    )
    expect_equal(qr_test__vals(d), 1:32)
    expect_equal(sum(!subset_status(d, "attempts")$success), 2L)
})

# Pending Files on an occupied node must not repeatedly materialize unchanged attempts.
test_that("acquisition scheduling refreshes history only for new observations", {
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(
        srv,
        c("slow.nc", "slow.nc"),
        nodes = rep("same-node", 2L),
        ids = c("first", "second")
    )
    req <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-08",
        request_id = "bounded"
    )
    d <- withr::local_tempdir()
    orig <- ddb_read_table
    reads <- 0L
    local_mocked_bindings(ddb_read_table = function(conn, name, ...) {
        if (identical(name, "subset_attempts")) {
            reads <<- reads + 1L
        }
        orig(conn, name, ...)
    })
    expect_true(
        qr_test__fetch(f, req, d, workers = 2L, node_workers = 1L)$complete
    )
    expect_lte(reads, 5L)
    expect_equal(nrow(read_subset(d)$values), 64L)
})

# Coordinator quarantines sick node, keeps healthy raw corners, and recovers deferred Files.
test_that("acquisition node cooldown preserves raw corners and later exact recovery", {
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(
        srv,
        paste0("source", 1:6, ".nc"),
        nodes = c(rep("unavailable", 5L), "healthy"),
        ids = paste0("node-file-", 1:6)
    )
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "raw")
    d <- withr::local_tempdir()
    orig <- opendap__read_source
    reader <- function(job) {
        if (job$data_node == "unavailable") {
            stop("NetCDF: DAP failure")
        }
        orig(job)
    }
    environment(reader) <- list2env(list(orig = orig), parent = baseenv())
    local_mocked_bindings(opendap__read_source = reader)

    expect_false(
        qr_test__fetch(
            f,
            req,
            d,
            replica_cooldown = 60,
            max_values = 4L
        )$complete
    )
    att <- subset_status(d, "attempts")
    expect_equal(sum(!att$success), 3L)
    expect_equal(qr_test__vals(d), 1:32)
    hashes <- tools::md5sum(subset_status(d, "blocks")$path)

    expect_false(
        qr_test__fetch(
            f,
            req,
            d,
            replica_cooldown = 60,
            max_values = 64L
        )$complete
    )
    expect_equal(subset_status(d, "attempts"), att)

    qr_test__ddb_exec(
        d,
        "UPDATE subset_attempts SET observed_at = observed_at - 1000"
    )
    local_mocked_bindings(opendap__read_source = orig)
    expect_true(
        qr_test__fetch(
            f,
            req,
            d,
            replica_cooldown = 60,
            max_values = 64L
        )$complete
    )
    raw <- read_subset(d)
    expect_equal(sort(raw$values$value), sort(rep(1:32, 6L)))
    expect_identical(unique(raw$values$cf_calendar), "360_day")
    expect_equal(sort(unique(raw$values$cf_day)), 1:8)
    expect_equal(tools::md5sum(names(hashes)), hashes)
    pts <- data.table::as.data.table(raw$values)[,
        list(
            points = data.table::uniqueN(paste(ind_lon, ind_lat)),
            times = data.table::uniqueN(time_index)
        ),
        by = "source_key"
    ]
    expect_true(all(pts$points == 4L) && all(pts$times == 8L))

    saved <- subset_status(d, "attempts")
    expect_true(qr_test__fetch(f, req, d, replica_cooldown = 60)$complete)
    expect_equal(subset_status(d, "attempts"), saved)
})
# }}}

# G9. Recovery metadata, coverage eligibility & node admission {{{
# First-block native contract denominator survives failure and exact recovery.
test_that("acquisition recovery retains first-block metadata and checked coverage", {
    srv <- qr_test__dap_server(fail_first = TRUE)
    f <- qr_test__dap_files(srv, "source.nc", ids = "metadata-first-source")
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "raw")
    d <- withr::local_tempdir()

    expect_false(qr_test__fetch(f, req, d)$complete)
    cov <- subset_status(d, "coverage")
    expect_true(cov$axis_known && !cov$complete && cov$retryable)
    expect_equal(
        c(
            cov$corner_count,
            cov$expected_native_values,
            cov$available_native_values
        ),
        c(4, 32, 0)
    )
    expect_identical(c(cov$status, cov$gap_kind), c("failed", "transport"))

    tm <- subset_status(d, "timing")
    expect_true(
        is.finite(tm$recorded_metadata_seconds) && tm$failed_task_seconds > 0
    )
    expect_equal(
        c(tm$successful_native_values, tm$successful_read_seconds),
        c(0, 0)
    )

    dem <- list.files(
        file.path(d, "subsets"),
        "^demand-.*[.]rds$",
        full.names = TRUE
    )
    hashes <- tools::sha256sum(dem)
    qr_test__dap_control(srv, "recover")
    expect_true(qr_test__fetch(f, req, d, max_values = 4L)$complete)
    cov <- subset_status(d, "coverage")
    expect_true(cov$complete && !cov$retryable)
    expect_equal(cov$available_native_values, 32)
    raw <- read_subset(d)
    expect_identical(unique(raw$values$cf_calendar), "360_day")
    expect_equal(sort(raw$values$value), as.double(1:32))
    expect_equal(tools::sha256sum(names(hashes)), hashes)

    saved <- subset_status(d, "attempts")
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_equal(subset_status(d, "attempts"), saved)
})

# Read-only coverage inspects exact receipts/blocks, ignoring stale job labels and corrupt artifacts.
test_that("acquisition recovery coverage ignores stale labels and invalid artifacts", {
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(srv, "source.nc")
    req <- qr_test__dap_request(
        rep("2060-01-01", 2L),
        rep("2060-01-08", 2L),
        request_id = c("a", "b"),
        case_id = c("case-a", "case-b")
    )
    d <- withr::local_tempdir()
    expect_true(qr_test__fetch(f, req, d, max_values = 4L)$complete)

    cov <- subset_status(d, "coverage")
    expect_equal(c(nrow(cov), length(unique(cov$demand_id))), c(2L, 1L))
    expect_true(all(cov$complete))
    expect_equal(
        subset_status(d, "coverage", source_key = unique(cov$source_key)),
        cov
    )
    expect_equal(nrow(subset_status(d, "coverage", source_key = "absent")), 0L)
    expect_equal(nrow(subset_status(d, "timing", source_key = "absent")), 0L)
    expect_error(
        subset_status(d, "blocks", source_key = unique(cov$source_key)),
        "only coverage"
    )

    qr_test__ddb_exec(d, "UPDATE subset_jobs SET status='running'")
    expect_true(all(subset_status(d, "coverage")$complete))

    corrupt <- function(path, byte) {
        con <- file(path, "ab")
        on.exit(close(con), add = TRUE)
        writeBin(as.raw(byte), con)
    }
    blocks <- subset_status(d, "blocks")
    corrupt(blocks$path[[1L]], 0L)
    part <- subset_status(d, "coverage")
    expect_false(any(part$complete))
    expect_true(all(
        part$status == "partial" &
            part$available_native_values == 28 &
            part$retryable
    ))
    expect_equal(nrow(subset_status(d, "blocks")), nrow(blocks))

    unk <- transform(
        req,
        request_id = c("c", "d"),
        time_start = "2060-01-09",
        time_stop = "2060-01-10"
    )
    expect_false(qr_test__fetch(f, unk, d)$complete)
    empty <- subset_status(d, "coverage")[request_id %in% unk$request_id]
    expect_true(all(
        empty$axis_known &
            !empty$complete &
            empty$gap_kind == "empty_selection" &
            !empty$retryable
    ))
    expect_equal(empty$expected_native_values, c(0, 0))

    conn <- ddb_connect(file.path(d, "manifest.duckdb"))
    sel <- ddb_read_table(conn, "subset_metadata")
    ddb_disconnect(conn)
    for (p in sel$metadata_path) {
        corrupt(p, 1L)
    }
    expect_true(all(is.na(subset_status(d, "timing")$recorded_open_seconds)))
})

# Restoration is gated by corroborated failures until a real native value succeeds.
test_that("acquisition recovery node admission requires real native success", {
    rows <- qr_test__attempt_history(
        source_key = letters[1:3],
        url = letters[1:3],
        data_node = "recovering",
        client = "local",
        observed_at = 10:12,
        error = "NetCDF: I/O failure"
    )
    cands <- data.table::data.table(data_node = c("recovering", "healthy"))
    lim <- function(hist, client = "local", sec = 60, now = 800) {
        opendap__recovery_limits(cands, hist, client, sec, 4L, now = now)
    }
    expect_equal(lim(rows), c(1L, 4L))
    expect_equal(lim(rows, sec = 0), c(4L, 4L))
    expect_equal(lim(rows, client = "other"), c(4L, 4L))
    expect_equal(lim(rows, now = 90000), c(4L, 4L))

    meta <- data.table::copy(rows[1L])
    data.table::set(
        meta,
        j = c("success", "observed_at"),
        value = list(TRUE, 799)
    )
    expect_equal(lim(rbind(rows, meta)), c(1L, 4L))
    data.table::set(meta, j = "native_values", value = 1)
    expect_equal(lim(rbind(rows, meta)), c(4L, 4L))
    data.table::set(
        rows,
        j = "error",
        value = "NetCDF: I/O failure; HTTP/2 404"
    )
    expect_equal(lim(rows), c(4L, 4L))

    reason <- opendap__gap_reason(
        "Compatible OPeNDAP replicas are cooling until 2026-10-25 00:00:00 UTC."
    )
    expect_identical(reason$kind, "deferred")
    expect_true(reason$retryable)
    expect_equal(
        reason$retry_after,
        as.double(as.POSIXct("2026-10-25", tz = "UTC"))
    )
    expect_false(
        opendap__gap_reason("Replica grid identity conflicts")$retryable
    )
    expect_true(opendap__gap_reason("NetCDF: file not found")$retryable)
    expect_false(
        opendap__gap_reason("Replica identity 404 conflicts")$retryable
    )
    expect_true(
        opendap__gap_reason("No compatible OPeNDAP candidate.")$retryable
    )

    pieces <- data.table::data.table(
        time_start_index = c(1L, 3L, 7L),
        time_count = c(4L, 4L, 2L)
    )
    expect_equal(opendap__range_values(list(c(1L, 8L)), pieces), 8)
    expect_equal(opendap__range_values(list(c(1L, 2L), c(7L, 2L)), pieces), 4)
})

# Eligibility is shared node evidence aggregated once per policy; disabled cooldown skips queries.
test_that("acquisition recovery coverage shares eligibility history", {
    srv <- qr_test__dap_server(fail_first = TRUE)
    f <- qr_test__dap_files(
        srv,
        rep("source.nc", 4L),
        ids = paste0("eligibility-", 1:4)
    )
    req <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-08",
        request_id = "shared"
    )
    d <- withr::local_tempdir()
    expect_false(qr_test__fetch(f, req, d)$complete)

    qr_test__ddb_exec(d, "UPDATE subset_controls SET replica_cooldown=60")
    calls <- 0L
    cooling <- opendap__cooling_until
    local_mocked_bindings(opendap__cooling_until = function(...) {
        calls <<- calls + 1L
        cooling(...)
    })
    cov <- subset_status(d, "coverage")
    expect_equal(nrow(cov), 4L)
    expect_true(all(is.finite(cov$retry_after)))
    expect_equal(calls, 1L)

    keys <- cov$source_key
    conn <- ddb_connect(file.path(d, "manifest.duckdb"))
    for (s in c(
        paste0(
            "UPDATE subset_controls SET replica_cooldown=120 WHERE source_key=",
            ddb_literal(conn, keys[[1L]])
        ),
        paste0(
            "UPDATE subset_controls SET client='isolated' WHERE source_key=",
            ddb_literal(conn, keys[[2L]])
        ),
        paste0(
            "UPDATE subset_controls SET replica_cooldown=0 WHERE source_key=",
            ddb_literal(conn, keys[[3L]])
        )
    )) {
        ddb_exec(conn, s)
    }
    ddb_disconnect(conn)

    calls <- 0L
    mixed <- subset_status(d, "coverage")
    expect_equal(calls, 3L)
    expect_true(
        is.na(mixed$retry_after[[2L]]) && is.na(mixed$retry_after[[3L]])
    )
    expect_gt(mixed$retry_after[[1L]] - mixed$retry_after[[4L]], 50)
    expect_equal(
        subset_status(d, "coverage", source_key = keys[[1L]]),
        mixed[1L]
    )

    qr_test__ddb_exec(d, "UPDATE subset_controls SET replica_cooldown=0")
    queries <- 0L
    query <- ddb_query
    local_mocked_bindings(ddb_query = function(conn, sql, ...) {
        if (grepl("SELECT * FROM subset_attempts WHERE", sql, fixed = TRUE)) {
            queries <<- queries + 1L
        }
        query(conn, sql, ...)
    })
    expect_true(all(is.na(subset_status(d, "coverage")$retry_after)))
    expect_equal(queries, 0L)
})
# }}}

# G10. CF calendars, UTC offset clock refresh & climatology {{{
# Julian coordinates with instantaneous point bounds resume exact blocks without I/O.
test_that("raw subset acquisition resumes Julian instantaneous bounds exactly", {
    srv <- qr_test__dap_server(calendar = "julian", point_bounds = TRUE)
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast-bounds.nc")
    req <- qr_test__dap_request("2060-01-01", "2060-01-08")
    expect_true(qr_test__fetch(f, req, d)$complete)
    raw <- read_subset(d)
    expect_equal(sort(raw$values$value), as.double(1:32))
    expect_equal(nrow(raw$neighborhoods), 4L)
    expect_identical(unique(raw$values$cf_calendar), "julian")
    expect_equal(raw$values$bound_start_value, raw$values$bound_end_value)

    art <- list.files(file.path(d, "subsets"), "[.]parquet$", full.names = TRUE)
    qr_test__assert_no_weather_reread(
        f,
        req,
        d,
        srv,
        art,
        tools::sha256sum(art),
        subset_status(d, "attempts"),
        raw$values
    )
})

# Corrected decoder reuses native payloads and rechecks legacy v1 selection receipts without weather rereads.
test_that("CF normalization resumes raw bounds after legacy selection rechecks", {
    units <- "hours since 2060-01-01 00:00:00 +02:00"
    srv <- qr_test__dap_server(
        calendar = "standard",
        point_bounds = TRUE,
        time_units = units
    )
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast-bounds.nc")
    req <- qr_test__dap_request("2060-01-01", "2060-01-01T05:00:00")
    expect_true(qr_test__fetch(f, req, d)$complete)
    raw <- read_subset(d)
    expect_equal(sort(raw$values$value), as.double(9:32))
    expect_equal(sort(unique(raw$values$time_value)), as.double(2:7))
    expect_identical(unique(raw$values$time_units), units)
    expect_equal(raw$values$bound_start_value, raw$values$time_value)
    expect_equal(raw$values$bound_end_value, raw$values$time_value)
    expect_equal(raw$values$time_bound_start, raw$values$time)
    expect_equal(nrow(raw$neighborhoods), 4L)

    stale <- data.table::copy(raw$values)
    data.table::set(
        stale,
        j = c("time", "cf_second_of_day", "time_bound_start"),
        value = list(
            stale$time + 7200,
            stale$cf_second_of_day + 7200,
            stale$time_bound_start + 7200
        )
    )
    expect_equal(opendap__refresh_clock(stale), raw$values)

    blocks <- subset_status(d, "blocks")
    conn <- ddb_connect(file.path(d, "manifest.duckdb"))
    dem <- data.table::as.data.table(ddb_read_table(conn, "subset_demands"))
    desc <- readRDS(dem$receipt_path[[1L]])
    leg_id <- store__hash(
        "raw-demand-receipt-v1",
        desc$source_key[[1L]],
        desc$lon[[1L]],
        desc$lat[[1L]],
        desc$time_start[[1L]],
        desc$time_stop[[1L]]
    )
    desc$demand_id <- leg_id
    leg_path <- file.path(d, "subsets", "legacy-selection.rds")
    store__extract_cache_write(leg_path, desc)
    data.table::set(
        dem,
        j = c("demand_id", "receipt_path", "receipt_sha256"),
        value = list(leg_id, leg_path, checksum_file(leg_path))
    )
    ddb_write_table(
        conn,
        "subset_demands",
        as.data.frame(dem),
        overwrite = TRUE
    )
    for (i in seq_len(nrow(blocks))) {
        md <- readRDS(blocks$metadata_path[[i]])
        if (!is.null(md$demands)) {
            md$demands$demand_id <- leg_id
        }
        mp <- file.path(d, "subsets", paste0("legacy-metadata-", i, ".rds"))
        store__extract_cache_write(mp, md)
        bp <- file.path(d, "subsets", paste0("legacy-block-", i, ".parquet"))
        ddb_exec(
            conn,
            sprintf(
                paste(
                    "COPY (SELECT * REPLACE (time + INTERVAL '2 hours' AS time,",
                    "cf_second_of_day + 7200 AS cf_second_of_day,",
                    "time_bound_start + INTERVAL '2 hours' AS time_bound_start,",
                    "time_bound_end + INTERVAL '2 hours' AS time_bound_end)",
                    "FROM read_parquet(%s)) TO %s (FORMAT PARQUET)"
                ),
                ddb_literal(conn, blocks$path[[i]]),
                ddb_literal(conn, bp)
            )
        )
        data.table::set(
            blocks,
            i = i,
            j = c("metadata_path", "metadata_sha256", "path", "sha256"),
            value = list(mp, checksum_file(mp), bp, checksum_file(bp))
        )
    }
    ddb_write_table(
        conn,
        "subset_blocks",
        as.data.frame(blocks),
        overwrite = TRUE
    )
    ddb_disconnect(conn)

    art <- c(blocks$path, blocks$metadata_path)
    hashes <- tools::sha256sum(art)
    expect_equal(read_subset(d)$values, raw$values)
    expect_equal(nrow(read_subset(d)$gaps$incomplete_requests), 1L)
    expect_false(all(subset_status(d, "coverage")$complete))

    qr_test__dap_control(srv, "reset")
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_identical(tools::sha256sum(art), hashes)
    expect_equal(read_subset(d)$values, raw$values)
    expect_equal(nrow(read_subset(d)$gaps$incomplete_requests), 0L)
    tr <- qr_test__dap_traffic(srv)
    expect_false(any(grepl("fast-bounds.nc.dods tas", tr, fixed = TRUE)))
    expect_true(any(grepl("fast-bounds.nc.dods time", tr, fixed = TRUE)))

    qr_test__assert_no_weather_reread(
        f,
        req,
        d,
        srv,
        art,
        hashes,
        subset_status(d, "attempts"),
        raw$values
    )
})

# Persisted climatological values remain statistical through DAP transport, read-back, and resume.
test_that("CF climatology acquisition preserves statistics and exact cached bounds", {
    check_order <- function(reversed) {
        method <- "time: minimum within years time: mean over years"
        srv <- qr_test__dap_server(
            climatology_method = method,
            bounds_reversed = reversed
        )
        d <- withr::local_tempdir()
        f <- qr_test__dap_files(srv, "fast-bounds.nc")
        req <- qr_test__dap_request("2060-01-01", "2060-01-08")
        expect_true(qr_test__fetch(f, req, d)$complete)

        raw <- read_subset(d)
        v <- raw$values
        expect_equal(sort(v$value), as.double(1:32))
        expect_identical(unique(v$time_kind), "climatology")
        expect_identical(unique(v$climatology_cell_methods), method)
        expect_identical(
            jsonlite::fromJSON(v$climatology_methods[[1L]])$method,
            c("minimum", "mean")
        )
        expect_equal(v$climatology_bound_start_value, v$time_value)
        expect_equal(
            v$climatology_bound_end_value - v$time_value,
            rep(10801, 32L)
        )
        expect_false("time_bound_start" %in% names(v))
        expect_equal(nrow(raw$neighborhoods), 4L)

        blocks <- subset_status(d, "blocks")
        art <- unique(c(
            blocks$path,
            blocks$metadata_path,
            list.files(
                file.path(d, "subsets"),
                "[.](rds|parquet|json)$",
                recursive = TRUE,
                full.names = TRUE
            )
        ))
        hashes <- tools::sha256sum(art)
        saved <- readRDS(blocks$metadata_path[[1L]])$bounds
        dims <- if (reversed) c("time", "bnds") else c("bnds", "time")
        pos <- match("time", dims)
        expect_identical(unique(saved$time_kind), "climatology")
        expect_identical(unique(saved$cell_methods), method)
        expect_identical(unique(saved$bounds_name), "time_bnds")
        expect_identical(unique(saved$bounds_time_dimension), pos)
        expect_identical(
            jsonlite::fromJSON(saved$bounds_dimensions[[1L]]),
            dims
        )
        expect_identical(unique(v$climatology_bounds_time_dimension), pos)
        expect_identical(
            jsonlite::fromJSON(v$climatology_bounds_dimensions[[1L]]),
            dims
        )
        rec <- cbind(saved$bound_start_value, saved$bound_end_value)
        exp_b <- cbind(as.double(0:7), as.double(0:7 + 10801))
        if (pos == 2L) {
            rec <- t(rec)
            exp_b <- t(exp_b)
        }
        expect_identical(rec, exp_b)

        for (spec in list(
            list(
                c(
                    "climatology_bounds_time_dimension",
                    "climatology_bounds_dimensions"
                ),
                NULL,
                "Incomplete cached CF climatology"
            ),
            list("climatology_bounds_time_dimension", 3L, "dimension"),
            list("time_kind", NULL, "climatolog")
        )) {
            bad <- data.table::copy(v)
            data.table::set(bad, j = spec[[1L]], value = spec[[2L]])
            expect_error(opendap__refresh_clock(bad), spec[[3L]])
        }
        stale <- data.table::copy(v)
        data.table::set(stale, j = "climatology_start_year", value = 1L)
        expect_equal(opendap__refresh_clock(stale), v)

        qr_test__assert_no_weather_reread(
            f,
            req,
            d,
            srv,
            art,
            hashes,
            subset_status(d, "attempts"),
            v
        )
    }
    check_order(FALSE)
    check_order(TRUE)
})

# Ordinary nested daily statistics survive transport and resume without becoming climatologies.
test_that("CMIP6 monthly statistics resume ordinary native blocks unchanged", {
    method <- "area: mean time: maximum within days time: mean over days"
    srv <- qr_test__dap_server(ordinary_method = method, time_values = 30 * 0:7)
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(srv, "fast-bounds.nc")
    req <- qr_test__dap_request("2060-01-01", "2060-08-30")
    expect_true(qr_test__fetch(f, req, d)$complete)

    v <- read_subset(d)$values
    expect_equal(sort(v$value), as.double(1:32))
    expect_identical(unique(v$cell_methods), method)
    expect_equal(sort(unique(v$time_value)), as.double(30 * 0:7))
    expect_equal(v$bound_end_value - v$bound_start_value, rep(30, 32L))
    expect_false(any(startsWith(names(v), "climatology_")))

    for (ep in c("bound_start_value", "bound_end_value")) {
        inv <- data.table::copy(v)
        data.table::set(inv, j = ep, value = NULL)
        expect_error(opendap__refresh_clock(inv), "explicit time bounds")
        inv <- data.table::copy(v)
        data.table::set(inv, i = 1L, j = ep, value = NA_real_)
        expect_error(opendap__refresh_clock(inv), "explicit time bounds")
    }

    blocks <- subset_status(d, "blocks")
    art <- c(blocks$path, blocks$metadata_path)
    qr_test__assert_no_weather_reread(
        f,
        req,
        d,
        srv,
        art,
        tools::sha256sum(art),
        subset_status(d, "attempts"),
        v
    )
})
# }}}

# G11. Serial overhead fixes (links, pool reuse, single decode) {{{
# Public call keeps other associations and avoids whole-table rewrites.
test_that("serial overhead fixes update only current associations", {
    skip_on_cran()
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(srv, "source.nc")
    d <- withr::local_tempdir()
    req <- qr_test__dap_request("2060-01-01", "2060-01-08")
    expect_true(qr_test__fetch(f, req, d)$complete)
    vals <- read_subset(d)$values
    att <- subset_status(d, "attempts")

    orig <- ddb_write_table
    local_mocked_bindings(
        .package = "epwshiftr",
        ddb_write_table = function(conn, table, rows, ...) {
            if (
                table %in%
                    c("subset_links", "subset_candidates", "subset_content")
            ) {
                stop("Unscoped ledger rewrite")
            }
            orig(conn, table, rows, ...)
        }
    )
    other <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-08",
        request_id = "two"
    )
    expect_true(qr_test__fetch(f, other, d)$complete)
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_identical(subset_status(d, "attempts"), att)
    expect_equal(read_subset(d)$values, vals)
    store <- opendap__open(d)
    withr::defer(store$close())
    expect_setequal(
        ddb_read_table(priv(store)$conn, "subset_links")$request_id,
        c("one", "two")
    )
})

# Reuse one private pool across calls, and retire only it after a caller-side failure.
test_that("serial overhead fixes reuse and reset only the owned acquisition pool", {
    skip_on_cran()
    srv <- qr_test__dap_server()
    session <- opendap__session(1L, 2)
    withr::defer(session$close())
    peer <- source__pool(1L, timeout = 2)
    withr::defer(peer$close(force = TRUE))
    peer_pid <- peer$status()[[1L]]$pid
    d <- withr::local_tempdir()
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "city")
    run <- function(id) {
        f <- qr_test__dap_files(
            srv,
            paste0("source", id, ".nc"),
            ids = paste0("serial-", id)
        )
        opendap__fetch(
            f,
            req,
            d,
            workers = 1L,
            timeout = 2,
            discover = FALSE,
            session = session
        )
    }
    expect_true(run(1L)$complete)
    pid <- session$acquire(1L, 2)$status()[[1L]]$pid
    handle <- ps::ps_handle(pid)
    expect_true(run(2L)$complete)
    expect_identical(session$acquire(1L, 2)$status()[[1L]]$pid, pid)
    expect_error(session$acquire(2L, 2))

    state <- opendap__job_state
    with_mocked_bindings(
        .package = "epwshiftr",
        expect_error(run(3L), "owned caller failure"),
        opendap__job_state = function(store, key, signature, status, ...) {
            if (status == "running") {
                stop("owned caller failure")
            }
            state(store, key, signature, status, ...)
        }
    )
    expect_false(ps::ps_is_running(handle))
    expect_true(ps::ps_is_running(ps::ps_handle(peer_pid)))
    expect_true(run(3L)$complete)
    expect_false(identical(session$acquire(1L, 2)$status()[[1L]]$pid, pid))
    session$close()
    expect_error(session$acquire(1L, 2))
})

# Inbox transfers decode once, while byte/receipt tamper before commit fails closed.
test_that("serial overhead fixes decode raw transfers once and reject changed bytes", {
    skip_on_cran()
    srv <- qr_test__dap_server()
    f <- qr_test__dap_files(srv, "source.nc")
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "city")
    decoded <- commits <- 0L
    mutate <- "none"
    shadow <- new.env(parent = environment(opendap__drain))
    shadow$readRDS <- function(file, ...) {
        if (grepl("^block-[0-9]+[.]rds$", basename(file))) {
            decoded <<- decoded + 1L
        }
        base::readRDS(file, ...)
    }
    drain <- opendap__drain
    environment(drain) <- shadow
    commit <- opendap__commit
    local_mocked_bindings(
        opendap__drain = drain,
        opendap__commit = function(
            store,
            job,
            path,
            progress,
            received = NULL
        ) {
            stopifnot(!is.null(received))
            commits <<- commits + 1L
            if (mutate == "data") {
                writeBin(as.raw(1L), path)
            }
            if (mutate == "receipt") {
                jsonlite::write_json(
                    list(sha256 = "changed"),
                    paste0(path, ".json"),
                    auto_unbox = TRUE
                )
            }
            commit(store, job, path, progress, received)
        },
        .package = "epwshiftr"
    )
    d <- withr::local_tempdir()
    expect_true(qr_test__fetch(f, req, d)$complete)
    expect_gt(commits, 0L)
    expect_identical(decoded, commits)
    expect_equal(nrow(read_subset(d)$values), 32L)
    for (mode in c("data", "receipt")) {
        mutate <- mode
        expect_error(
            qr_test__fetch(f, req, withr::local_tempdir()),
            "changed before commit"
        )
    }
})
# }}}

# G12. CF offset rejection & frozen recovery {{{
# Coordinate read rejection stays retryable and local to its endpoint.
test_that("CF offset rejection stays retryable and local to its endpoint", {
    err <- tryCatch(
        parse_cf_time(c(0, NA_real_), "days since 2000-01-01", "noleap"),
        error = identity
    )
    expect_s3_class(err, "error")
    expect_identical(
        conditionMessage(err),
        "Invalid offsets for CF time coordinate."
    )
    expect_identical(opendap__failure_kind(err), "source_coordinate")
    reason <- opendap__gap_reason(conditionMessage(err))
    expect_identical(reason$kind, "source_coordinate")
    expect_true(reason$retryable)

    hist <- qr_test__attempt_history(
        source_key = paste0("coordinate", 1:3),
        url = paste0("url", 1:3),
        data_node = "one-node",
        client = "local",
        observed_at = 1001:1003,
        open_seconds = 0,
        error = conditionMessage(err),
        error_kind = "source_coordinate"
    )
    cands <- hist[, c("source_key", "url", "data_node")]
    expect_equal(opendap__cooldown_deadline(hist[1L], 60), 1061)
    expect_equal(opendap__cooldown_deadline(hist, 60, node = TRUE), 0)
    expect_equal(
        opendap__cooling_until(cands, hist, "local", 60, now = 1004),
        1061:1063
    )
    expect_equal(
        opendap__recovery_limits(cands, hist, "local", 60, 4L, now = 1004),
        rep(4L, 3L)
    )
    peer <- data.table::copy(cands[1L])
    data.table::set(peer, j = "source_key", value = "independent")
    expect_equal(opendap__cooling_until(peer, hist, "local", 60, now = 1004), 0)

    for (msg in c(
        "Invalid offsets for CF time coordinate. extra",
        "Invalid offsets for CF time coordinate",
        "Manifest publication failed"
    )) {
        expect_identical(opendap__failure_kind(msg), "internal")
        expect_false(opendap__gap_reason(msg)$retryable)
    }
    expect_identical(
        opendap__failure_kind("Replica grid identity conflicts"),
        "source_metadata"
    )
    expect_false(
        opendap__gap_reason("Replica grid identity conflicts")$retryable
    )
    expect_error(
        parse_cf_time(Inf, "days since 2000-01-01"),
        "Invalid offsets",
        fixed = TRUE
    )
})

# Healthy peer files survive and resume cleanly, and same-identity replicas fall back.
test_that("CF offset rejection preserves healthy files, recovers later, and permits replica fallback", {
    srv <- qr_test__dap_server()
    req <- qr_test__dap_request("2060-01-01", "2060-01-08", request_id = "raw")
    orig <- opendap__read_source
    reader <- function(job) {
        if (job$data_node == "invalid-coordinate") {
            get("parse_cf_time", asNamespace("epwshiftr"))(
                c(0, NA_real_),
                "days since 2000-01-01",
                "noleap"
            )
        }
        orig(job)
    }
    environment(reader) <- list2env(list(orig = orig), parent = baseenv())
    local_mocked_bindings(opendap__read_source = reader)

    d <- withr::local_tempdir()
    f <- qr_test__dap_files(
        srv,
        c("coordinate.nc", "healthy.nc"),
        nodes = c("invalid-coordinate", "healthy"),
        ids = c("coordinate-file", "healthy-file")
    )
    expect_false(qr_test__fetch(f, req, d)$complete)
    att <- subset_status(d, "attempts")
    expect_identical(att$error_kind[!att$success], "source_coordinate")
    miss <- subset_status(d, "coverage")[!complete]
    expect_identical(miss$gap_kind, "source_coordinate")
    expect_true(miss$retryable && !miss$axis_known)
    hashes <- tools::sha256sum(subset_status(d, "blocks")$path)

    dr <- withr::local_tempdir()
    rf <- qr_test__dap_files(
        srv,
        c("coordinate.nc", "consistent.nc"),
        nodes = c("invalid-coordinate", "consistent")
    )
    expect_true(qr_test__fetch(rf, req, dr)$complete)
    ratt <- subset_status(dr, "attempts")
    expect_equal(length(unique(ratt$source_key)), 1L)
    expect_identical(ratt$error_kind[[1L]], "source_coordinate")
    expect_true(ratt$success[[2L]])
    expect_equal(qr_test__vals(dr), as.double(1:32))

    local_mocked_bindings(opendap__read_source = orig)
    expect_true(qr_test__fetch(f, req, d)$complete)
    saved <- subset_status(d, "attempts")
    resumed <- saved[seq.int(nrow(att) + 1L, nrow(saved)), ]
    expect_identical(unique(resumed$source_key), miss$source_key[[1L]])
    expect_equal(tools::sha256sum(names(hashes)), hashes)
    expect_equal(qr_test__vals(d), sort(rep(as.double(1:32), 2L)))
    expect_true(all(subset_status(d, "coverage")$complete))
})

test_that("frozen recovery reuses imported blocks with verified routes", {
    id_attr <- c(
        tracking_id = "hdl:21.14100/mock-file",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day"
    )
    srv <- qr_test__dap_server(global_attributes = id_attr)
    docs <- qr_test__dap_docs(
        srv,
        "missing.nc",
        suffix = "#show=url",
        titles = "fast.nc",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day",
        checksum = strrep("a", 64L)
    )
    f <- query_result_test_object("File", docs)
    cat_plan <- f$access_plan(expand = FALSE)
    key <- unique(cat_plan$source_key)
    routes <- data.frame(
        source_key = key,
        url = paste0(srv$url(), "fast.nc#show=url"),
        logical_version = cat_plan$version,
        physical_version = "20260102",
        tracking_id = docs$tracking_id,
        checksum = docs$checksum,
        size = as.character(docs$size),
        receipt_sha256 = strrep("b", 64L)
    )
    req <- qr_test__dap_request(
        c("2060-01-01", "2060-01-04"),
        request_id = c("first", "missing"),
        case_id = "fixture-case",
        site_id = "fixture-site",
        source_id = docs$source_id,
        experiment_id = docs$experiment_id,
        table_id = docs$table_id,
        variable_id = docs$variable_id
    )
    root <- withr::local_tempdir()
    orig_d <- file.path(root, "original")
    dest_d <- file.path(root, "destination")
    tr <- function() qr_test__dap_traffic(srv, weather_only = TRUE)

    wrong <- transform(routes, tracking_id = "hdl:wrong")
    expect_error(
        f$fetch_subset(req[1L, ], orig_d, verified_replicas = wrong),
        "does not match the frozen File identity"
    )
    expect_true(
        qr_test__fetch(
            f,
            req[1L, ],
            orig_d,
            verified_replicas = routes
        )$complete
    )
    before <- subset_status(orig_d, "blocks")
    expect_length(tr(), 1L)
    expect_equal(
        import_subset_blocks(orig_d, dest_d, key)[["subset_blocks"]],
        1L
    )
    expect_true(all(import_subset_blocks(orig_d, dest_d, key) == 0L))

    frozen <- qr_test__frozen_plan(f, req, key, root)
    old <- frozen$state
    rec <- function(n) {
        resume_subsets(
            frozen$plan,
            frozen$denominator,
            frozen$journal,
            dest_d,
            verified_replicas = routes,
            max_batches = n,
            discover = FALSE
        )
    }
    expect_identical(rec(0L), old)

    # Real filesystem collision preserves inherited admission and moved-aside payload
    subsets <- file.path(dest_d, "subsets")
    held <- file.path(dest_d, "held")
    stopifnot(file.rename(subsets, held), file.create(subsets))
    failed <- suppressWarnings(rec(1L))
    expect_identical(failed$phase, "blocked_error")
    expect_identical(failed$in_flight, old$in_flight)
    expect_identical(failed$admissions, old$admissions)
    expect_identical(
        checksum_file(file.path(held, basename(before$path))),
        before$sha256
    )
    expect_length(tr(), 1L)
    stopifnot(unlink(subsets) == 0L, file.rename(held, subsets))

    after <- rec(1L)
    expect_identical(after$phase, "complete")
    expect_identical(after$admissions, old$admissions)
    expect_identical(after$started, old$started)
    expect_null(after$in_flight)
    blocks <- subset_status(dest_d, "blocks")
    expect_identical(
        blocks$sha256[match(before$block_id, blocks$block_id)],
        before$sha256
    )
    added <- blocks[!blocks$block_id %in% before$block_id]
    expect_true(all(added$time_start_index == 4L))
    expect_equal(sum(added$lat_count * added$lon_count * added$time_count), 4L)
    sent <- tr()
    expect_length(sent, 1L + nrow(added))
    expect_true(all(grepl("tas%5b3(:3)?%5d", sent[-1L], ignore.case = TRUE)))
    expect_identical(rec(1L)$phase, "complete")
    expect_identical(tr(), sent)
    expect_identical(
        as.integer(qr_test__vals(dest_d, case_id = "fixture-case")),
        c(1:4, 13:16)
    )
})

test_that("native DAP slabs preserve their exact time indices", {
    src <- array(as.integer(1:12), dim = c(2L, 2L, 3L))
    src[1L, 1L, 2L] <- NA_integer_
    requested <- list()
    read <- function(var, start, count, index, collapse) {
        requested[[length(requested) + 1L]] <<- c(start, count)
        if (count[[3L]] > 1L) {
            stop("NetCDF: DAP server error")
        }
        src[,, start[[3L]], drop = FALSE]
    }
    args <- list(
        var = "tas",
        start = c(1L, 1L, 1L),
        count = c(2L, 2L, 3L),
        index = 1L,
        collapse = FALSE
    )
    expect_identical(
        do.call(
            dataset__read_dap_weather,
            c(list(dataset = list(var_get = read)), args)
        ),
        src
    )
    expect_identical(
        vapply(Filter(function(x) x[[6L]] == 1L, requested), "[[", 0L, 3L),
        1:3
    )
    expect_error(
        do.call(
            dataset__read_dap_weather,
            c(
                list(
                    dataset = list(var_get = function(...) {
                        stop("Permission denied")
                    })
                ),
                args
            )
        ),
        "Permission denied"
    )
    expect_identical(
        dataset__boundary_calendar("noleap"),
        dataset__boundary_calendar("365_day")
    )
    expect_false(identical(
        dataset__boundary_calendar("360_day"),
        dataset__boundary_calendar("365_day")
    ))
    expect_identical(normalize_cf_calendar("noleap"), "noleap")
})

test_that("fetch, read-back, status and frozen recovery agree on the missing corner", {
    srv <- qr_test__dap_server(fail_last_corner = TRUE)
    d <- withr::local_tempdir()
    f <- qr_test__dap_files(
        srv,
        "source.nc",
        nodes = "fixture",
        titles = "source.nc",
        id = "coverage-file",
        dataset_id = "coverage-dataset",
        size = 32,
        checksum = strrep("a", 64L),
        instance_id = "coverage-instance",
        master_id = "coverage-master",
        tracking_id = "hdl:21.14100/coverage-fixture",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day"
    )
    req <- qr_test__dap_request(
        "2060-01-01",
        "2060-01-02",
        request_id = "two-days",
        case_id = "city",
        source_id = "fixture",
        experiment_id = "ssp245",
        table_id = "day",
        variable_id = "tas"
    )
    fetch <- function(r, resume = TRUE) {
        qr_test__fetch(f, r, d, resume = resume, max_values = 4L)
    }

    # Seed day one so day two reads only missing points; (2,2,2) value 8 fails
    first <- transform(req, request_id = "one-day", time_stop = time_start)
    expect_true(fetch(first)$complete)
    expect_false(fetch(req)$complete)

    raw <- read_subset(d)
    expect_equal(sort(raw$values$value), as.double(1:7))
    expect_identical(raw$gaps$incomplete_requests$request_id, "two-days")
    chk <- subset_status(d, "coverage")[request_id == "two-days"]
    expect_false(chk$complete)
    expect_equal(
        c(chk$available_native_values, chk$expected_native_values),
        c(7, 8)
    )
    before <- subset_status(d, "blocks")
    old_hashes <- tools::sha256sum(c(before$path, before$metadata_path))

    # Interrupted forced refresh for a second site publishes no coverage for it
    qr_test__dap_control(srv, "fail")
    pend <- transform(
        req,
        request_id = "pending",
        case_id = "pending",
        site_id = "pending",
        lon = 10.75
    )
    expect_false(fetch(pend, resume = FALSE)$complete)
    chk <- subset_status(d, "coverage")
    expect_equal(
        chk$available_native_values[match(
            c("two-days", "pending"),
            chk$request_id
        )],
        c(7, 0)
    )
    w <- chk[request_id == "pending"]
    expect_true(w$axis_known && !w$complete)
    expect_equal(w$expected_native_values, 8)
    pr <- read_subset(d, case_id = "pending")
    expect_equal(nrow(pr$values), 0)
    expect_identical(pr$gaps$incomplete_requests$request_id, "pending")

    frozen <- qr_test__frozen_plan(
        f,
        req,
        chk$source_key[[1L]],
        withr::local_tempdir()
    )
    qr_test__dap_control(srv, "recover")
    qr_test__dap_control(srv, "reset")

    # Transport error after worker writes final block still drains and commits it
    drain <- opendap__drain
    local_mocked_bindings(opendap__drain = function(store, state, client) {
        changed <- drain(store, state, client)
        if (is.null(state$error) && !is.null(state$result)) {
            state$error <- simpleError("NetCDF: connection reset after data")
        }
        changed
    })
    resume_batch <- function() {
        resume_subsets(
            frozen$plan,
            frozen$denominator,
            frozen$journal,
            d,
            max_batches = 1L,
            discover = FALSE
        )
    }
    res <- resume_batch()
    expect_equal(
        c(
            res$summary$files_complete,
            res$summary$available_native_values_known_axes
        ),
        c(1, 8)
    )
    expect_identical(res$admissions, frozen$state$admissions)
    expect_identical(tools::sha256sum(names(old_hashes)), old_hashes)

    added <- subset_status(d, "blocks")[!block_id %in% before$block_id]
    expect_equal(nrow(added), 1)
    cols <- c(
        "time_start_index",
        "ind_lat",
        "ind_lon",
        "time_count",
        "lat_count",
        "lon_count"
    )
    expect_equal(
        unlist(added[, cols, with = FALSE], use.names = FALSE),
        c(2, 2, 2, 1, 1, 1)
    )

    tr <- qr_test__dap_traffic(srv, weather_only = TRUE)
    expect_length(tr, 1L)
    expect_match(
        utils::URLdecode(tr),
        "tas[[]1(:1(:1)?)?[]][[]1(:1(:1)?)?[]][[]1(:1(:1)?)?[]]"
    )
    sv <- subset_status(d, "coverage")
    expect_true(sv$complete[sv$request_id == "two-days"])
    expect_equal(sv$available_native_values[sv$request_id == "two-days"], 8)
    expect_false(sv$complete[sv$request_id == "pending"])
    expect_equal(sv$available_native_values[sv$request_id == "pending"], 0)

    raw <- read_subset(d, case_id = "city")
    expect_equal(sort(raw$values$value), as.double(1:8))
    expect_equal(nrow(raw$gaps$incomplete_requests), 0)
    expect_true(fetch(req)$complete)
    expect_identical(resume_batch()$phase, "complete")
    expect_identical(qr_test__dap_traffic(srv, weather_only = TRUE), tr)

    # Still report verified coverage when the attempt failed after committing
    late <- fetch(req, resume = FALSE)
    expect_identical(late$jobs$status, "failed")
    expect_match(late$jobs$error, "connection reset after data")
    expect_true(late$complete)

    # An unsuccessful forced refresh cannot claim blocks from a prior call
    qr_test__dap_control(srv, "fail")
    expect_false(fetch(req, resume = FALSE)$complete)
    expect_true(subset_status(d, "coverage")[request_id == "two-days"]$complete)
})
# }}}

# }}}
# vim: fdm=marker :
