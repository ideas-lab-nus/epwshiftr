# Shared local fixture/import helpers for store exchange tests.
# Copy pre-initialized store templates into each test's isolated directory so
# every test owns a fresh DuckDB file without repeating DDL and NetCDF writes.
ex__copy_dir <- function(src, dest) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(
        src,
        full.names = TRUE,
        all.files = TRUE,
        no.. = TRUE
    )
    if (length(entries)) {
        stopifnot(all(file.copy(entries, dest, recursive = TRUE)))
    }
    invisible(dest)
}

ex__templates <- local({
    empty_dir <- NULL
    planned <- NULL
    list(
        empty = function() {
            if (is.null(empty_dir) || !dir.exists(empty_dir)) {
                empty_dir <<- tempfile("exchange-empty-tpl-")
                s <- EsgStore$new(empty_dir)
                s$close()
                withr::defer(
                    unlink(empty_dir, recursive = TRUE),
                    envir = testthat::teardown_env()
                )
            }
            empty_dir
        },
        planned = function() {
            if (
                is.null(planned) ||
                    !dir.exists(planned$nc_dir) ||
                    !dir.exists(planned$store_dir)
            ) {
                nc_dir <- tempfile("exchange-nc-tpl-")
                dir.create(nc_dir)
                writer <- cmip6_fixture__write
                environment(writer) <- new.env(parent = environment(writer))
                environment(writer)$cf_time__year_days <- function(...) 1L
                paths <- file.path(nc_dir, c("part-a.nc", "part-b.nc"))
                for (i in seq_along(paths)) {
                    writer(paths[[i]], 2060L)
                    nc <- RNetCDF::open.nc(paths[[i]], write = TRUE)
                    RNetCDF::var.put.nc(nc, "time", i - 0.5)
                    RNetCDF::var.put.nc(
                        nc,
                        "time_bnds",
                        matrix(c(i - 1, i), nrow = 2L)
                    )
                    RNetCDF::close.nc(nc)
                }
                docs <- data.table::rbindlist(lapply(
                    paths,
                    esgf_test__file_docs
                ))
                files <- esgf_test__file_result(docs)
                sites <- data.frame(
                    site_id = c("a", "b"),
                    lon = c(103.98, 104.2),
                    lat = c(1.37, 1.5)
                )
                store_dir <- tempfile("exchange-planned-tpl-")
                ex__copy_dir(ex__templates$empty(), store_dir)
                s <- EsgStore$new(store_dir, create = FALSE)
                q <- s$add_files(files)
                s$plan_regions(
                    q,
                    sites,
                    c("2060-01-01", "2060-01-02T23:59:59Z")
                )
                plans <- s$query("SELECT * FROM extraction_plan")
                s$close()
                planned <<- list(
                    nc_dir = nc_dir,
                    store_dir = store_dir,
                    plans = plans
                )
                withr::defer(
                    unlink(c(nc_dir, store_dir), recursive = TRUE),
                    envir = testthat::teardown_env()
                )
            }
            planned
        }
    )
})

ex__setup <- function(root = NULL, env = parent.frame(), need_second = FALSE) {
    tpl <- ex__templates$planned()
    if (is.null(root)) {
        root <- tempfile("extract-exchange-")
        withr::defer(unlink(root, recursive = TRUE), envir = env)
    }
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    owner_dir <- file.path(root, "annual-owner")
    ex__copy_dir(tpl$store_dir, owner_dir)
    part_a_dir <- file.path(owner_dir, "file-parts/file-a")
    ex__copy_dir(tpl$store_dir, part_a_dir)
    store_a <- EsgStore$new(part_a_dir, create = FALSE)
    store_b <- if (isTRUE(need_second)) {
        part_b_dir <- file.path(owner_dir, "file-parts/file-b")
        ex__copy_dir(tpl$store_dir, part_b_dir)
        EsgStore$new(part_b_dir, create = FALSE)
    } else {
        NULL
    }
    store_owner <- EsgStore$new(owner_dir, create = FALSE)
    stores <- list(store_a, store_b, store_owner)
    withr::defer(
        lapply(stores, function(s) if (!is.null(s)) s$close()),
        envir = env
    )
    plans <- data.table::copy(tpl$plans)
    list(
        root = root,
        stores = stores,
        plans = plans,
        source = store_a,
        owner = store_owner,
        ids = plans$plan_id[plans$file_key == plans$file_key[[1L]]]
    )
}
ex__store <- function(path, env = parent.frame()) {
    if (!file.exists(file.path(path, "manifest.duckdb"))) {
        ex__copy_dir(ex__templates$empty(), path)
    }
    s <- EsgStore$new(path, create = FALSE)
    withr::defer(s$close(), envir = env)
    s
}
ex__plans <- function(s, r, csum = r$checksum, qp = r$query_paths) {
    s$import_plans(r$path, csum, qp)
}
ex__ext <- function(s, r, csum = r$checksum, ap = r$artifact_paths) {
    s$import_extracts(r$path, csum, ap)
}
ex__json <- function(obj, path) {
    jsonlite::write_json(
        obj,
        path,
        auto_unbox = TRUE,
        digits = NA,
        null = "null"
    )
    path
}

# Plan transfer uses generated NetCDF and fresh stores. It never clones a live
# DuckDB file, changes original IDs, or copies native/extracted weather bytes.
test_that("pending physical-file plans execute with exact original identities", {
    local_cache_mode("off")
    f <- ex__setup()
    r <- f$source$export_plans(f$ids, file.path(f$root, "plans.json"))
    dest <- ex__store(file.path(f$owner$path, "file-parts/transferred"))
    imported <- ex__plans(dest, r)
    expect_identical(imported$plan_id, f$ids)
    expect_false(any(imported$reused))
    expect_identical(
        dest$query("SELECT plan_id FROM extraction_plan")$plan_id,
        f$ids
    )
    expect_equal(nrow(dest$query("SELECT * FROM file_catalog")), 1L)
    expect_equal(nrow(dest$query("SELECT * FROM extraction_result")), 0L)
    expect_length(list.files(dest$path, "nc$", recursive = TRUE), 0L)
    expect_true(all(dest$extract(f$ids)$status == "done"))
    before <- exchange_test__snapshot(dest)
    expect_true(all(ex__plans(dest, r)$reused))
    expect_identical(exchange_test__snapshot(dest), before)
    done <- dest$export_extracts(f$ids, file.path(f$root, "completed.json"))
    ex__ext(f$owner, done)
    expect_equal(sum(f$owner$coverage()$status == "done"), 2L)
    expect_error(
        dest$export_plans(f$ids, file.path(f$root, "done-plan.json")),
        "pending or failed"
    )
})

# Metadata transport authenticates bytes, then intrinsic ID and relation checks
# reject a faulty producer before any destination table or query file is changed.
test_that("missing query bytes, altered coordinates, and duplicate retry delivery", {
    f <- ex__setup()
    r <- f$source$export_plans(f$ids, file.path(f$root, "plans.json"))
    dest <- ex__store(file.path(f$root, "fresh-destination"))
    expect_error(ex__plans(dest, r, csum = strrep("0", 64L)), "checksum")
    missing <- r$query_paths
    missing[[1L]] <- file.path(f$root, "missing-control.json")
    expect_error(ex__plans(dest, r, qp = missing), "missing or has changed")
    expect_equal(nrow(dest$query("SELECT * FROM extraction_plan")), 0L)
    expect_equal(nrow(dest$query("SELECT * FROM query_run")), 0L)
    data <- jsonlite::read_json(r$path, simplifyVector = FALSE)
    data$tables$extraction_plan$lon[[1L]] <- "104.12345678901234"
    changed <- ex__json(data, file.path(f$root, "changed.json"))
    expect_error(
        dest$import_plans(
            changed,
            store_hash_file(changed, "sha256"),
            r$query_paths
        ),
        "identity mismatch"
    )
    expect_equal(nrow(dest$query("SELECT * FROM extraction_plan")), 0L)

    # Duplicate delivery preserves existing retry history; conflicting identities fail.
    ex__plans(dest, r)
    ddb_exec(
        priv(dest)$conn,
        "UPDATE extraction_plan SET status='failed', attempt_count=9, last_error='original generated failure'"
    )
    before <- dest$query("SELECT * FROM extraction_plan")
    expect_true(all(ex__plans(dest, r)$reused))
    expect_identical(dest$query("SELECT * FROM extraction_plan"), before)
    ddb_exec(
        priv(dest)$conn,
        "UPDATE file_catalog SET version='different-version'"
    )
    expect_error(ex__plans(dest, r), "identity mismatch")
    expect_identical(dest$query("SELECT * FROM extraction_plan"), before)
})

# Failed metadata installation rolls back the DB and removes only metadata files
# created by this attempt, without touching any source/destination payload.
test_that("pending-plan transaction failure leaves the fresh store unchanged", {
    f <- ex__setup()
    r <- f$source$export_plans(f$ids, file.path(f$root, "plans.json"))
    dest <- ex__store(file.path(f$root, "rollback-destination"))
    original <- ddb_append_table
    local_mocked_bindings(ddb_append_table = function(conn, table, ...) {
        if (table == "extraction_plan") {
            stop("injected plan installation failure")
        }
        original(conn, table, ...)
    })
    expect_error(ex__plans(dest, r), "injected plan installation failure")
    for (tbl in c(
        "query_run",
        "esg_file",
        "esg_query_file",
        "file_catalog",
        "extraction_plan"
    )) {
        expect_equal(nrow(dest$query(paste("SELECT * FROM", tbl))), 0L)
    }
    expect_length(list.files(file.path(dest$path, "queries"), "json$"), 0L)
})

# Epoch-adjacent doubles expose a one-ULP decimal conversion error on this R.
# The exact receipt wire must preserve bits, nulls and signed zero, not merely
# approximate equality at a scientific tolerance.
test_that("typed numeric receipt columns preserve adversarial double bits", {
    value <- 1792470000 + (0:511) * 2^-22
    rows <- data.table::data.table(
        value = c(value, -0, NA_real_),
        time = as.POSIXct(
            c(value, -0, NA_real_),
            origin = "1970-01-01",
            tz = "UTC"
        )
    )
    wire <- jsonlite::toJSON(
        store_exchange__encode(rows),
        auto_unbox = FALSE,
        digits = NA,
        na = "null"
    )
    actual <- store_exchange__decode(
        jsonlite::fromJSON(wire, simplifyVector = FALSE),
        rows[0]
    )
    expect_identical(writeBin(actual$value, raw()), writeBin(rows$value, raw()))
    expect_identical(
        writeBin(as.double(actual$time), raw()),
        writeBin(as.double(rows$time), raw())
    )
    expect_identical(class(actual$time), class(rows$time))
    expect_error(
        store_exchange__numeric_values(list("1;stop(1)")),
        "invalid typed"
    )
    expect_error(store_exchange__numeric_values(list("1e999")), "non-finite")
})

# Generated exchange fixtures are shared through helper-store-exchange.R.
test_that("independent file fragments assemble without copying or re-extracting", {
    local_cache_mode("off")
    f <- ex__setup(need_second = TRUE)
    file_keys <- unique(f$plans$file_key)
    receipts <- vector("list", 2L)
    for (i in 1:2) {
        ids <- f$plans$plan_id[f$plans$file_key == file_keys[[i]]]
        expect_true(all(f$stores[[i]]$extract(ids)$status == "done"))
        receipts[[i]] <- f$stores[[i]]$export_extracts(
            ids,
            file.path(f$root, paste0("receipt-", i, ".json"))
        )
        f$stores[[i]]$close()
        imported <- ex__ext(f$owner, receipts[[i]])
        expect_identical(imported$plan_id, ids)
        expect_false(any(imported$reused))
        expect_equal(sum(f$owner$coverage()$status == "done"), 2L * i)
        expect_equal(
            nrow(f$owner$query(
                "SELECT DISTINCT output_path FROM extraction_result"
            )),
            2L * i
        )
        expect_length(
            list.files(
                file.path(f$owner$path, "extracts"),
                recursive = TRUE,
                pattern = "parquet$"
            ),
            0L
        )
    }
    paths <- unlist(lapply(receipts, `[[`, "artifact_paths"), use.names = FALSE)
    data <- f$owner$query(paste0(
        "SELECT * FROM read_parquet([",
        paste(ddb_literal(priv(f$owner)$conn, paths), collapse = ","),
        "])"
    ))
    expect_equal(data.table::uniqueN(data$plan_id), 4L)
    expect_equal(sort(unique(data$cf_day)), 1:2)
    expect_equal(data[, .N, by = "site_id"]$N, c(2L, 2L))
    expect_true(all(f$owner$coverage()$status == "done"))
    before <- exchange_test__snapshot(f$owner)
    for (r in receipts) {
        expect_true(all(ex__ext(f$owner, r)$reused))
    }
    expect_identical(exchange_test__snapshot(f$owner), before)
})

# A receipt with multiple plans must commit as a unit, including on retry.
# Replaying it should keep every row and the original Parquet bytes unchanged.
test_that("import rolls back a failed receipt commit and reuses its retry", {
    local_cache_mode("off")
    f <- ex__setup()
    f$source$extract(f$ids)
    r <- f$source$export_extracts(f$ids, file.path(f$root, "receipt.json"))
    before <- exchange_test__snapshot(f$owner)
    bytes <- vapply(r$artifact_paths, store_hash_file, character(1L), "sha256")
    fault <- exchange_test__fail_publication_commit(f$owner, f$ids)

    expect_error(ex__ext(f$owner, r), "injected publication commit failure")
    expect_identical(fault$staged$status, rep("done", length(f$ids)))
    expect_equal(fault$staged$results, length(f$ids))
    expect_equal(fault$staged$artifacts, length(f$ids))
    expect_gt(fault$staged$grid_sources, 0L)
    expect_identical(exchange_test__snapshot(f$owner), before)

    expect_false(any(ex__ext(f$owner, r)$reused))
    ready <- exchange_test__snapshot(f$owner)
    expect_equal(nrow(ready$extraction_result), length(f$ids))
    expect_equal(sum(ready$artifact$kind == "extract"), length(f$ids))
    expect_equal(nrow(ready$extraction_grid_source), fault$staged$grid_sources)
    expect_true(all(ex__ext(f$owner, r)$reused))
    expect_identical(exchange_test__snapshot(f$owner), ready)
    expect_identical(
        vapply(r$artifact_paths, store_hash_file, character(1L), "sha256"),
        bytes
    )
})

test_that("receipt type reconstruction preserves exact doubles and nulls", {
    rows <- data.table::data.table(
        precise = c(pi, 1.0000000000000002, NA_real_),
        count = c(1L, NA_integer_, 3L),
        name = c("one", NA_character_, "three"),
        time = as.POSIXct(
            c(0.123456, 123456.987654, NA_real_),
            origin = "1970-01-01",
            tz = "UTC"
        )
    )
    json <- jsonlite::toJSON(
        store_exchange__encode(rows),
        auto_unbox = FALSE,
        digits = NA,
        na = "null"
    )
    actual <- store_exchange__decode(
        jsonlite::fromJSON(json, simplifyVector = FALSE),
        rows[0]
    )
    expect_identical(actual, rows)
})

# Hashes authenticate receipt transport, while identity, lineage and coverage
# validation still reject missing bytes or inconsistent receipts atomically.
test_that("invalid checksums, identities, lineage, counts, and outside-root paths fail before adoption", {
    local_cache_mode("off")
    f <- ex__setup()
    expect_error(
        f$source$export_extracts(f$ids, file.path(f$root, "pending.json")),
        "completed or empty"
    )
    f$source$extract(f$ids)
    r <- f$source$export_extracts(f$ids, file.path(f$root, "receipt.json"))
    before <- exchange_test__snapshot(f$owner)

    expect_error(ex__ext(f$owner, r, csum = strrep("0", 64L)), "checksum")
    missing <- r$artifact_paths
    missing[[length(missing)]] <- file.path(f$owner$path, "missing.parquet")
    expect_error(ex__ext(f$owner, r, ap = missing), "missing or has changed")
    expect_identical(exchange_test__snapshot(f$owner), before)

    conn <- priv(f$owner)$conn
    ddb_exec(conn, "UPDATE extraction_plan SET lon=lon+1")
    expect_error(ex__ext(f$owner, r), "identity mismatch")
    ddb_exec(conn, "UPDATE extraction_plan SET lon=lon-1")
    ddb_exec(conn, "UPDATE file_catalog SET checksum='wrong-content'")
    expect_error(ex__ext(f$owner, r), "identity mismatch")
    expect_equal(nrow(f$owner$query("SELECT * FROM extraction_result")), 0L)
    # Restore exact checksum for remaining receipt variant checks.
    orig_csum <- f$source$query("SELECT checksum FROM file_catalog")$checksum[[
        1L
    ]]
    ddb_exec(
        conn,
        paste0(
            "UPDATE file_catalog SET checksum=",
            ddb_literal(conn, orig_csum)
        )
    )

    original <- jsonlite::read_json(r$path, simplifyVector = FALSE)
    variants <- list(
        grid_identity = function(x) {
            x$tables$extraction_grid_source$plan_id[[1L]] <- "unknown"
            x
        },
        row_count = function(x) {
            x$tables$extraction_result$row_count[[1L]] <- 999L
            x
        },
        fractional_count = function(x) {
            x$tables$extraction_result$row_count[[1L]] <- 1.5
            x
        },
        nonterminal = function(x) {
            x$tables$extraction_plan$status[[1L]] <- "pending"
            x
        }
    )
    for (name in names(variants)) {
        p <- ex__json(
            variants[[name]](original),
            file.path(f$root, paste0(name, ".json"))
        )
        expect_error(f$owner$import_extracts(
            p,
            store_hash_file(p, "sha256"),
            r$artifact_paths
        ))
        expect_identical(exchange_test__snapshot(f$owner), before)
    }
    outside <- r$artifact_paths
    copy <- file.path(f$root, "outside.parquet")
    expect_true(file.copy(outside[[1L]], copy))
    outside[[1L]] <- copy
    expect_error(ex__ext(f$owner, r, ap = outside), "outside")
    expect_identical(exchange_test__snapshot(f$owner), before)
    empty <- f$source$export_extracts(
        character(),
        file.path(f$root, "empty.json")
    )
    expect_equal(nrow(ex__ext(f$owner, empty)), 0L)
    expect_identical(exchange_test__snapshot(f$owner), before)
})

# Exchange must adopt previously published Parquet by identity, including an
# older codec and paths nested beyond the default Windows full-path limit.
test_that("exchange reads a legacy Parquet at its original long path", {
    skip_if(
        .Platform$OS.type == "windows",
        "Windows compact output is covered by its native path test"
    )
    withr::local_options(epwshiftr.cache = FALSE, epwshiftr.progress = FALSE)
    root <- file.path(withr::local_tempdir(), strrep("nested", 22L))
    f <- ex__setup(root = root)
    private <- priv(f$source)
    unlockBinding("write_parquet", private)
    private$write_parquet <- function(dt, path, overwrite = FALSE) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        conn <- private$conn
        duckdb::duckdb_register(conn, "legacy_exchange_input", dt)
        on.exit(
            duckdb::duckdb_unregister(conn, "legacy_exchange_input"),
            add = TRUE
        )
        ddb_exec(
            conn,
            paste0(
                "COPY legacy_exchange_input TO ",
                ddb_literal(conn, path),
                " (FORMAT PARQUET, COMPRESSION SNAPPY)"
            )
        )
        invisible(path)
    }
    lockBinding("write_parquet", private)
    id <- f$plans$plan_id[[1L]]
    expect_identical(f$source$extract(id, fallback = "error")$status, "done")
    r <- f$source$export_extracts(id, file.path(root, "legacy.json"))
    path <- r$artifact_paths[[1L]]
    expect_gt(nchar(path), 260L)
    f$source$close()
    expect_identical(ex__ext(f$owner, r)$plan_id, id)
    local_id <- f$owner$query(
        "SELECT artifact_id FROM extraction_result"
    )$artifact_id
    expect_identical(f$owner$artifact_path(local_id), path)
    expect_true(f$owner$validate(local_id)$checksum_ok)
    lineage <- f$owner$query(
        "SELECT metadata_json FROM artifact WHERE kind = 'extract'"
    )
    expect_identical(
        jsonlite::fromJSON(lineage$metadata_json)$original_artifact_id,
        names(r$artifact_paths)
    )
    quoted <- ddb_literal(priv(f$owner)$conn, path)
    expect_true(all(is.finite(
        f$owner$query(paste0(
            "SELECT value FROM read_parquet(",
            quoted,
            ")"
        ))$value
    )))
    expect_identical(
        f$owner$query(paste0(
            "SELECT DISTINCT compression FROM parquet_metadata(",
            quoted,
            ")"
        ))$compression,
        "SNAPPY"
    )
    expect_true(ex__ext(f$owner, r)$reused)
})

# Dataset-scoped IDs returned by a bridge must not merge distinct time slices.
test_that("dataset master IDs retain each native file and deduplicate replicas", {
    ds <- "CMIP6.CMIP.CMCC.CMCC-ESM2.historical.r1i1p1f1.day.tas.gn"
    fns <- paste0(
        "tas_day_CMCC-ESM2_historical_r1i1p1f1_gn_",
        c("19750101-19991231", "20000101-20141231"),
        ".nc"
    )
    docs <- data.table::rbindlist(lapply(fns, esgf_test__file_docs))
    set_cols <- function(dt, vals) {
        for (nm in names(vals)) {
            data.table::set(dt, j = nm, value = vals[[nm]])
        }
    }
    set_cols(
        docs,
        list(
            master_id = ds,
            instance_id = paste0(ds, ".v20210114"),
            dataset_id = paste0(ds, ".v20210114|example.org")
        )
    )
    original <- data.table::copy(docs)
    replica <- data.table::copy(docs[1L])
    set_cols(
        replica,
        list(
            master_id = paste0(ds, ".", fns[1L]),
            replica = TRUE,
            data_node = "replica.example.org",
            id = paste0(fns[1L], "|replica.example.org")
        )
    )

    path <- withr::local_tempdir()
    store <- ex__store(path)
    cat_q <- function(id) {
        store$query(sprintf(
            "SELECT * FROM file_catalog WHERE query_id = '%s'",
            id
        ))
    }
    expected <- paste0("master:", ds, ".", fns)
    expect_setequal(
        cat_q(store$add_files(esgf_test__file_result(docs[1L])))$file_key,
        expected[1L]
    )
    expect_setequal(
        cat_q(store$add_files(esgf_test__file_result(docs[2L])))$file_key,
        expected[2L]
    )
    combined <- store$add_files(esgf_test__file_result(data.table::rbindlist(list(
        docs,
        replica
    ))))
    expect_setequal(cat_q(combined)$file_key, expected)
    expect_equal(nrow(store$query("SELECT * FROM esg_file")), 2L)
    expect_equal(docs, original)
    expect_equal(cat_q(combined)$master_id, rep(ds, 2L))
    store$close()
    store <- ex__store(path)
    expect_setequal(cat_q(combined)$file_key, expected)
})

# Existing valid identities and fallback semantics are unchanged by the repair.
test_that("file identity only qualifies a confirmed dataset-scoped master ID", {
    expect_identical(store__file_keys(data.table::data.table()), character())
    rows <- data.table::data.table(
        master_id = c(
            "dataset",
            "dataset.file.nc",
            "generic",
            "dataset",
            NA_character_
        ),
        dataset_id = c(
            "dataset.v20210114|node",
            "dataset.v20210114|node",
            NA,
            "dataset|node",
            NA
        ),
        filename = c("file.nc", "file.nc", "file.nc", NA, NA),
        tracking_id = c(NA, NA, NA, "tracked-file", "fallback-file")
    )
    expect_identical(
        store__file_keys(rows),
        c(
            "master:dataset.file.nc",
            "master:dataset.file.nc",
            "master:generic",
            "tracking:tracked-file",
            "tracking:fallback-file"
        )
    )
    expect_identical(store__file_keys(rows[5L]), "tracking:fallback-file")
})

# vim: fdm=marker :
