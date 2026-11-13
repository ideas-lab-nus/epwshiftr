# Plan transfer uses generated NetCDF and fresh stores. It never clones a live
# DuckDB file, changes original IDs, or copies native/extracted weather bytes.
test_that("pending physical-file plans execute with exact original identities", {
    local_cache_mode("off")
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    owner <- f$stores[[3L]]
    selected <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    receipt <- source$export_plans(selected, file.path(f$root, "plans.json"))
    destination <- EsgStore$new(file.path(owner$path, "file-parts/transferred"))
    on.exit(destination$close(), add = TRUE)
    imported <- destination$import_plans(
        receipt$path,
        receipt$checksum,
        receipt$query_paths
    )
    expect_identical(imported$plan_id, selected)
    expect_false(any(imported$reused))
    expect_identical(
        destination$query("SELECT plan_id FROM extraction_plan")$plan_id,
        selected
    )
    expect_equal(nrow(destination$query("SELECT * FROM file_catalog")), 1L)
    expect_equal(nrow(destination$query("SELECT * FROM extraction_result")), 0L)
    expect_length(list.files(destination$path, "nc$", recursive = TRUE), 0L)
    expect_true(all(destination$extract(selected)$status == "done"))
    before <- exchange_test__snapshot(destination)
    expect_true(all(
        destination$import_plans(
            receipt$path,
            receipt$checksum,
            receipt$query_paths
        )$reused
    ))
    expect_identical(exchange_test__snapshot(destination), before)
    completed <- destination$export_extracts(
        selected,
        file.path(f$root, "completed.json")
    )
    owner$import_extracts(
        completed$path,
        completed$checksum,
        completed$artifact_paths
    )
    expect_equal(sum(owner$coverage()$status == "done"), 2L)
    expect_error(
        destination$export_plans(selected, file.path(f$root, "done-plan.json")),
        "pending or failed"
    )
})

# Metadata transport authenticates bytes, then intrinsic ID and relation checks
# reject a faulty producer before any destination table or query file is changed.
test_that("missing query bytes and altered plan coordinates are atomic failures", {
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    selected <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    receipt <- source$export_plans(selected, file.path(f$root, "plans.json"))
    destination <- EsgStore$new(file.path(f$root, "fresh-destination"))
    on.exit(destination$close(), add = TRUE)
    expect_error(
        destination$import_plans(
            receipt$path,
            paste(rep("0", 64), collapse = ""),
            receipt$query_paths
        ),
        "checksum"
    )
    missing <- receipt$query_paths
    missing[[1L]] <- file.path(f$root, "missing-control.json")
    expect_error(
        destination$import_plans(receipt$path, receipt$checksum, missing),
        "missing or has changed"
    )
    expect_equal(nrow(destination$query("SELECT * FROM extraction_plan")), 0L)
    expect_equal(nrow(destination$query("SELECT * FROM query_run")), 0L)
    data <- jsonlite::read_json(receipt$path, simplifyVector = FALSE)
    data$tables$extraction_plan$lon[[1L]] <- "104.12345678901234"
    changed <- file.path(f$root, "changed.json")
    jsonlite::write_json(
        data,
        changed,
        auto_unbox = TRUE,
        digits = NA,
        null = "null"
    )
    expect_error(
        destination$import_plans(
            changed,
            store_hash_file(changed, "sha256"),
            receipt$query_paths
        ),
        "identity mismatch"
    )
    expect_equal(nrow(destination$query("SELECT * FROM extraction_plan")), 0L)
})

# A retry or a duplicate delivery cannot roll back progress already recorded by
# the destination's sole writer, including the original attempt/error evidence.
test_that("duplicate plan delivery preserves existing retry history", {
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    selected <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    receipt <- source$export_plans(selected, file.path(f$root, "plans.json"))
    destination <- EsgStore$new(file.path(f$root, "retry-destination"))
    on.exit(destination$close(), add = TRUE)
    destination$import_plans(
        receipt$path,
        receipt$checksum,
        receipt$query_paths
    )
    ddb_exec(
        priv(destination)$conn,
        "UPDATE extraction_plan SET status='failed', attempt_count=9, last_error='original generated failure'"
    )
    before <- destination$query("SELECT * FROM extraction_plan")
    expect_true(all(
        destination$import_plans(
            receipt$path,
            receipt$checksum,
            receipt$query_paths
        )$reused
    ))
    expect_identical(destination$query("SELECT * FROM extraction_plan"), before)
    # Conflicting identities are refused even when a transport hash is valid.
    ddb_exec(
        priv(destination)$conn,
        "UPDATE file_catalog SET version='different-version'"
    )
    expect_error(
        destination$import_plans(
            receipt$path,
            receipt$checksum,
            receipt$query_paths
        ),
        "identity mismatch"
    )
    expect_identical(destination$query("SELECT * FROM extraction_plan"), before)
})

# Failed metadata installation rolls back the DB and removes only metadata files
# created by this attempt, without touching any source/destination payload.
test_that("pending-plan transaction failure leaves the fresh store unchanged", {
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    selected <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    receipt <- f$stores[[1L]]$export_plans(
        selected,
        file.path(f$root, "plans.json")
    )
    destination <- EsgStore$new(file.path(f$root, "rollback-destination"))
    on.exit(destination$close(), add = TRUE)
    original <- ddb_append_table
    local_mocked_bindings(ddb_append_table = function(conn, table, ...) {
        if (table == "extraction_plan") {
            stop("injected plan installation failure")
        }
        original(conn, table, ...)
    })
    expect_error(
        destination$import_plans(
            receipt$path,
            receipt$checksum,
            receipt$query_paths
        ),
        "injected plan installation failure"
    )
    for (table in c(
        "query_run",
        "esg_file",
        "esg_query_file",
        "file_catalog",
        "extraction_plan"
    )) {
        expect_equal(nrow(destination$query(paste("SELECT * FROM", table))), 0L)
    }
    expect_length(
        list.files(file.path(destination$path, "queries"), "json$"),
        0L
    )
})

# Epoch-adjacent doubles expose a one-ULP decimal conversion error on this R.
# The exact receipt wire must preserve bits, nulls and signed zero, not merely
# approximate equality at a scientific tolerance.
test_that("typed numeric receipt columns preserve adversarial double bits", {
    value <- 1792470000 + (0:65535) * 2^-22
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
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    owner <- f$stores[[3L]]
    file_keys <- unique(f$plans$file_key)
    receipts <- vector("list", 2L)
    for (i in 1:2) {
        ids <- f$plans$plan_id[f$plans$file_key == file_keys[[i]]]
        expect_true(all(f$stores[[i]]$extract(ids)$status == "done"))
        receipts[[i]] <- f$stores[[i]]$export_extracts(
            ids,
            file.path(f$root, paste0("receipt-", i, ".json"))
        )
        # The source manifest can close before the other node sees the receipt.
        f$stores[[i]]$close()
        imported <- owner$import_extracts(
            receipts[[i]]$path,
            receipts[[i]]$checksum,
            receipts[[i]]$artifact_paths
        )
        expect_identical(imported$plan_id, ids)
        expect_false(any(imported$reused))
        expect_equal(sum(owner$coverage()$status == "done"), 2L * i)
        expect_equal(
            nrow(owner$query(
                "SELECT DISTINCT output_path FROM extraction_result"
            )),
            2L * i
        )
        # Import registers original files by reference, no second Parquet copy.
        expect_length(
            list.files(
                file.path(owner$path, "extracts"),
                recursive = TRUE,
                pattern = "parquet$"
            ),
            0L
        )
    }
    paths <- unlist(lapply(receipts, `[[`, "artifact_paths"), use.names = FALSE)
    data <- owner$query(paste0(
        "SELECT * FROM read_parquet([",
        paste(ddb_literal(priv(owner)$conn, paths), collapse = ","),
        "])"
    ))
    expect_equal(data.table::uniqueN(data$plan_id), 4L)
    expect_equal(sort(unique(data$cf_day)), 1:2)
    expect_equal(data[, .N, by = "site_id"]$N, c(2L, 2L))
    expect_true(all(owner$coverage()$status == "done"))
    before <- exchange_test__snapshot(owner)
    for (receipt in receipts) {
        expect_true(all(
            owner$import_extracts(
                receipt$path,
                receipt$checksum,
                receipt$artifact_paths
            )$reused
        ))
    }
    expect_identical(exchange_test__snapshot(owner), before)
})

test_that("missing bytes and changed plan or file identity never partially import", {
    local_cache_mode("off")
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    owner <- f$stores[[3L]]
    ids <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    source$extract(ids)
    receipt <- source$export_extracts(ids, file.path(f$root, "receipt.json"))
    before <- exchange_test__snapshot(owner)
    expect_error(
        owner$import_extracts(
            receipt$path,
            paste(rep("0", 64), collapse = ""),
            receipt$artifact_paths
        ),
        "checksum"
    )
    missing <- receipt$artifact_paths
    missing[[length(missing)]] <- file.path(owner$path, "missing.parquet")
    expect_error(
        owner$import_extracts(receipt$path, receipt$checksum, missing),
        "missing or has changed"
    )
    expect_identical(exchange_test__snapshot(owner), before)
    conn <- priv(owner)$conn
    ddb_exec(conn, "UPDATE extraction_plan SET lon=lon+1")
    expect_error(
        owner$import_extracts(
            receipt$path,
            receipt$checksum,
            receipt$artifact_paths
        ),
        "identity mismatch"
    )
    ddb_exec(conn, "UPDATE extraction_plan SET lon=lon-1")
    # Restore exact doubles from the original plans rather than subtracting an
    # offset in a production path; these test values happen to round-trip here.
    ddb_exec(conn, "UPDATE file_catalog SET checksum='wrong-content'")
    expect_error(
        owner$import_extracts(
            receipt$path,
            receipt$checksum,
            receipt$artifact_paths
        ),
        "identity mismatch"
    )
    expect_equal(nrow(owner$query("SELECT * FROM extraction_result")), 0L)
})

test_that("transaction failure restores every destination table", {
    local_cache_mode("off")
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    owner <- f$stores[[3L]]
    ids <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    source$extract(ids)
    receipt <- source$export_extracts(ids, file.path(f$root, "receipt.json"))
    before <- exchange_test__snapshot(owner)
    original <- ddb_append_table
    local_mocked_bindings(ddb_append_table = function(conn, table, ...) {
        if (table == "extraction_grid_source") {
            stop("injected registration failure")
        }
        original(conn, table, ...)
    })
    expect_error(
        owner$import_extracts(
            receipt$path,
            receipt$checksum,
            receipt$artifact_paths
        ),
        "injected registration failure"
    )
    expect_identical(exchange_test__snapshot(owner), before)
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
    expect_error(
        store_exchange__decode(
            list(count = list(1.5)),
            rows[0, "count", with = FALSE]
        ),
        "non-integer"
    )
})

# Hashes authenticate receipt transport, while lineage and coverage validation
# still reject internally inconsistent receipts from a faulty producer.
test_that("invalid lineage, counts and outside-root paths fail before adoption", {
    local_cache_mode("off")
    f <- exchange_test__fixture()
    on.exit(lapply(f$stores, function(s) s$close()), add = TRUE)
    source <- f$stores[[1L]]
    owner <- f$stores[[3L]]
    ids <- f$plans$plan_id[f$plans$file_key == f$plans$file_key[[1L]]]
    expect_error(
        source$export_extracts(ids, file.path(f$root, "pending.json")),
        "completed or empty"
    )
    source$extract(ids)
    receipt <- source$export_extracts(ids, file.path(f$root, "receipt.json"))
    before <- exchange_test__snapshot(owner)
    original <- jsonlite::read_json(receipt$path, simplifyVector = FALSE)
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
        path <- file.path(f$root, paste0(name, ".json"))
        jsonlite::write_json(
            variants[[name]](original),
            path,
            auto_unbox = TRUE,
            digits = NA,
            null = "null"
        )
        expect_error(owner$import_extracts(
            path,
            store_hash_file(path, "sha256"),
            receipt$artifact_paths
        ))
        expect_identical(exchange_test__snapshot(owner), before)
    }
    outside <- receipt$artifact_paths
    copy <- file.path(f$root, "outside.parquet")
    expect_true(file.copy(outside[[1L]], copy))
    outside[[1L]] <- copy
    expect_error(
        owner$import_extracts(receipt$path, receipt$checksum, outside),
        "outside"
    )
    expect_identical(exchange_test__snapshot(owner), before)
    empty <- source$export_extracts(
        character(),
        file.path(f$root, "empty.json")
    )
    expect_equal(
        nrow(owner$import_extracts(
            empty$path,
            empty$checksum,
            empty$artifact_paths
        )),
        0L
    )
    expect_identical(exchange_test__snapshot(owner), before)
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
    fixture <- exchange_test__fixture(root = root)
    on.exit(lapply(fixture$stores, function(store) store$close()), add = TRUE)
    source <- fixture$stores[[1L]]
    owner <- fixture$stores[[3L]]
    # Emit the previously supported Snappy format before the original Store
    # publishes the artifact, so its manifest records the original byte hash.
    private <- priv(source)
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
    id <- fixture$plans$plan_id[[1L]]
    expect_identical(source$extract(id, fallback = "error")$status, "done")
    receipt <- source$export_extracts(id, file.path(root, "legacy.json"))
    path <- receipt$artifact_paths[[1L]]
    expect_gt(nchar(path), 260L)
    source$close()
    imported <- owner$import_extracts(
        receipt$path,
        receipt$checksum,
        receipt$artifact_paths
    )
    expect_identical(imported$plan_id, id)
    local_id <- owner$query(
        "SELECT artifact_id FROM extraction_result"
    )$artifact_id
    expect_identical(owner$artifact_path(local_id), path)
    expect_true(owner$validate(local_id)$checksum_ok)
    lineage <- owner$query(
        "SELECT metadata_json FROM artifact WHERE kind = 'extract'"
    )
    expect_identical(
        jsonlite::fromJSON(lineage$metadata_json)$original_artifact_id,
        names(receipt$artifact_paths)
    )
    quoted <- ddb_literal(priv(owner)$conn, path)
    values <- owner$query(paste0(
        "SELECT value FROM read_parquet(",
        quoted,
        ")"
    ))
    codec <- owner$query(paste0(
        "SELECT DISTINCT compression FROM parquet_metadata(",
        quoted,
        ")"
    ))
    expect_true(all(is.finite(values$value)))
    expect_identical(codec$compression, "SNAPPY")
    expect_true(
        owner$import_extracts(
            receipt$path,
            receipt$checksum,
            receipt$artifact_paths
        )$reused
    )
})

# vim: fdm=marker :
