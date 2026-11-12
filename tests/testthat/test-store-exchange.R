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

# vim: fdm=marker :
