# Generate two distinct one-day nc files, two cities and the original common
# two-day plan. Only synthetic fixture generation uses the shorter year length;
# the extraction reader and CF calendar implementation remain unchanged.
exchange_test__fixture <- function(
    sites = NULL,
    root = tempfile("extract-exchange-")
) {
    # Integration checks can request exact grid cells while the exchange
    # regression retains its original off-grid nearest-neighbour examples.
    if (is.null(sites)) {
        sites <- data.frame(
            site_id = c("a", "b"),
            lon = c(103.98, 104.2),
            lat = c(1.37, 1.5)
        )
    }
    dir.create(root)
    writer <- cmip6_fixture__write
    environment(writer) <- new.env(parent = environment(writer))
    environment(writer)$cf_time__year_days <- function(...) 1L
    paths <- file.path(root, c("part-a.nc", "part-b.nc"))
    for (i in seq_along(paths)) {
        writer(paths[[i]], 2060L)
        nc <- RNetCDF::open.nc(paths[[i]], write = TRUE)
        RNetCDF::var.put.nc(nc, "time", i - 0.5)
        RNetCDF::var.put.nc(nc, "time_bnds", matrix(c(i - 1, i), nrow = 2L))
        RNetCDF::close.nc(nc)
    }
    docs <- data.table::rbindlist(lapply(paths, esgf_test__file_docs))
    files <- esgf_test__file_result(docs)
    paths_store <- file.path(
        root,
        "annual-owner",
        c("file-parts/file-a", "file-parts/file-b", "")
    )
    stores <- lapply(paths_store, function(path) {
        store <- EsgStore$new(path)
        q <- store$add_files(files)
        store$plan_regions(
            q,
            sites,
            c("2060-01-01", "2060-01-02T23:59:59Z")
        )
        store
    })
    list(
        root = root,
        stores = stores,
        plans = stores[[1L]]$query("SELECT * FROM extraction_plan")
    )
}

# Snapshot all destination manifest tables to verify atomic failure and exact
# idempotent reuse, independently of receipt or file names.
exchange_test__snapshot <- function(store) {
    tables <- c(
        "extraction_plan",
        "extraction_result",
        "extraction_grid_source",
        "artifact"
    )
    stats::setNames(
        lapply(tables, function(table) {
            store$query(paste0("SELECT * FROM ", table, " ORDER BY 1"))
        }),
        tables
    )
}

# Fail the owning connection's first commit and observe the rows staged inside
# the transaction so a failure before registration cannot satisfy the test.
exchange_test__fail_publication_commit <- function(
    store,
    plan_ids,
    env = parent.frame()
) {
    original <- ddb_exec
    write_conn <- priv(store)$conn
    fault <- new.env(parent = emptyenv())
    fault$armed <- TRUE
    fault$staged <- NULL
    testthat::local_mocked_bindings(
        .env = env,
        ddb_exec = function(conn, sql) {
            if (
                fault$armed &&
                    identical(conn, write_conn) &&
                    identical(sql, "COMMIT")
            ) {
                fault$armed <- FALSE
                state <- exchange_test__snapshot(store)
                status <- state$extraction_plan[
                    match(plan_ids, state$extraction_plan$plan_id)
                ]$status
                fault$staged <- list(
                    status = status,
                    results = nrow(state$extraction_result),
                    grid_sources = nrow(state$extraction_grid_source),
                    artifacts = sum(state$artifact$kind == "extract")
                )
                stop("injected publication commit failure", call. = FALSE)
            }
            original(conn, sql)
        }
    )
    fault
}

# vim: fdm=marker :
