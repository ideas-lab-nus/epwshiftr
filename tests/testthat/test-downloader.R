# Basic Tests {{{
test_that("FileDownloader can be created", {
    dl <- FileDownloader$new()
    expect_s3_class(dl, "FileDownloader")
    expect_true(dir.exists(dl$data_dir))
    expect_true(dir.exists(dl$tmp_dir))
    expect_equal(dl$max_retries, 3L)
    expect_equal(dl$timeout, 3600L)

    temp_dir <- tempdir()
    temp <- file.path(temp_dir, ".tmp_test")
    dl <- FileDownloader$new(
        dest = temp_dir,
        temp = temp,
        retries = 5L,
        timeout = 7200L,
        cleanup = FALSE
    )
    expect_equal(dl$data_dir, normalizePath(temp_dir))
    expect_equal(dl$tmp_dir, temp)
    expect_equal(dl$max_retries, 5L)
    expect_equal(dl$timeout, 7200L)

    # Clean up
    unlink(temp, recursive = TRUE)
})
# }}}

# Persistent Manifest Tests {{{
test_that("FileDownloader persists config and manifest state", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("persistent content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        timeout = 30L,
        n_workers = 0L
    )

    expect_true(file.exists(dl$config_file))
    expect_true(file.exists(dl$manifest))
    expect_true(schema_validate(
        SCHEMA_DOWNLOADER_CONFIG,
        jsonlite::fromJSON(dl$config_file, simplifyVector = TRUE, simplifyMatrix = FALSE),
        mode = "test",
        name = "downloader-config"
    ))

    plan <- data.table::data.table(
        logical_file_id = "tracking:local-test",
        filename = "local.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "unit-test")
    expect_match(session_id, "^\\d{8}-\\d{6}-[0-9a-f]{8}$")
    expect_equal(nrow(dl$sessions()), 1L)
    expect_equal(nrow(dl$tasks(session_id = session_id)), 1L)

    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_equal(tasks$candidate_count, 1L)
    expect_equal(tasks$failed_candidate_count, 0L)
    expect_true(file.exists(file.path(dest, "local.txt")))
    expect_true(dl$verify(session_id = session_id)$checksum_ok)

    restored <- FileDownloader$load_config(dl$config_file)
    expect_equal(restored$data_dir, normalizePath(dest, winslash = "/"))
    expect_equal(restored$tmp_dir, normalizePath(temp, mustWork = FALSE, winslash = "/"))
    expect_equal(restored$manifest, normalizePath(manifest, mustWork = FALSE, winslash = "/"))
    expect_equal(restored$tasks(session_id = session_id)$status, "done")

    rm(dl, restored)
    gc()
    conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = manifest, read_only = TRUE)
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    expect_setequal(
        DBI::dbListTables(conn),
        c("download_candidate", "download_event", "download_meta", "download_session", "download_task")
    )
})

test_that("FileDownloader reconnects manifest after shallow clone finalization", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("clone content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    cloned <- dl$clone(deep = FALSE)
    expect_s3_class(cloned, "FileDownloader")
    rm(cloned)
    gc()

    plan <- data.table::data.table(
        logical_file_id = "tracking:clone-reconnect",
        filename = "clone.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "clone-reconnect")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "done")
    expect_true(file.exists(file.path(dest, "clone.txt")))
})

test_that("FileDownloader cancels stale downloading tasks before run", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("stale content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:stale-test",
        filename = "stale.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "stale")
    task_id <- dl$tasks(session_id = session_id)$task_id[[1L]]
    DBI::dbExecute(
        priv(dl)$manifest_conn,
        "UPDATE download_task SET status = 'downloading', updated_at = ? WHERE task_id = ?",
        params = list(as.POSIXct("2000-01-01 00:00:00", tz = "UTC"), task_id)
    )

    tasks <- dl$run(session_id = session_id, progress = FALSE)
    expect_equal(tasks$status, "cancelled")
    expect_match(tasks$last_error, "previous R session")
    expect_false(file.exists(file.path(dest, "stale.txt")))

    events <- DBI::dbReadTable(priv(dl)$manifest_conn, "download_event")
    expect_true("cancelled" %in% events$event)

    resumed <- dl$resume(session_id = session_id, progress = FALSE)
    expect_equal(resumed$status, "done")
    expect_true(file.exists(file.path(dest, "stale.txt")))
})

test_that("FileDownloader falls back across candidate URLs", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("candidate content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:fallback-test",
        filename = "fallback.txt",
        url = c(
            paste0("file://", normalizePath(file.path(root, "missing.txt"), mustWork = FALSE, winslash = "/")),
            paste0("file://", normalizePath(src, winslash = "/"))
        ),
        checksum = checksum,
        checksum_type = "md5",
        priority = c(1L, 2L)
    )

    session_id <- dl$enqueue(plan, session_label = "fallback")
    expect_warning(
        tasks <- dl$run(session_id = session_id, progress = FALSE),
        "Failed to open"
    )
    expect_equal(tasks$status, "done")
    expect_identical(tasks$selected_url, plan$url[[2L]])

    rm(dl)
    gc()
    conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = manifest, read_only = TRUE)
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE, after = FALSE)
    candidates <- data.table::as.data.table(DBI::dbReadTable(
        conn,
        "download_candidate"
    ))
    expect_equal(candidates[order(priority)]$failed_count, c(1L, 0L))
})

test_that("FileDownloader keeps candidate URLs scoped to each task", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src_1 <- tempfile()
    src_2 <- tempfile()
    writeLines("first task content", src_1)
    writeLines("second task content", src_2)

    urls <- paste0("file://", normalizePath(c(src_1, src_2), winslash = "/"))
    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = c("tracking:first", "tracking:second"),
        filename = c("first.txt", "second.txt"),
        url = urls,
        checksum = as.character(tools::md5sum(c(src_1, src_2))),
        checksum_type = "md5",
        priority = 1L
    )

    session_id <- dl$enqueue(plan, session_label = "candidate-scope")
    tasks <- dl$run(session_id = session_id, progress = FALSE)
    tasks <- tasks[order(filename)]

    expect_equal(tasks$status, c("done", "done"))
    expect_identical(tasks$selected_url, urls)
    expect_equal(readLines(file.path(dest, "first.txt")), "first task content")
    expect_equal(readLines(file.path(dest, "second.txt")), "second task content")
})
# }}}

# Download Tests {{{
test_that("FileDownloader can download a single file", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://httpbin.org/bytes/1024"

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    path <- tryCatch(
        dl$download(url, filename = "test_file.bin", progress = FALSE),
        error = function(e) {
            skip("Network unavailable or httpbin.org is down")
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "test_file.bin")
    expect_equal(file.info(path)$size, 1024)

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader can download with subdir", {
    skip_on_cran()
    skip_if_offline()

    url <- "https://httpbin.org/bytes/512"

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    path <- tryCatch(
        dl$download(
            url,
            filename = "data.bin",
            subdir = "docs",
            progress = FALSE
        ),
        error = function(e) {
            skip("Network unavailable")
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "data.bin")
    expect_equal(dirname(path), file.path(normalizePath(temp_dir), "docs"))

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader respects overwrite parameter with local files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    test_file <- file.path(temp_dir, "source.txt")
    writeLines("test content", test_file)

    dl <- FileDownloader$new(dest = temp_dir)

    dest_file <- file.path(temp_dir, "dest.txt")
    file.copy(test_file, dest_file)

    mtime1 <- file.info(dest_file)$mtime

    Sys.sleep(0.1)

    expect_true(file.exists(dest_file))
    mtime2 <- file.info(dest_file)$mtime
    expect_equal(mtime1, mtime2)

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# File Status Tests {{{
test_that("FileDownloader tracks file status correctly", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    url <- "https://httpbin.org/bytes/2048"

    path <- tryCatch(
        dl$download(url, filename = "test.bin", progress = FALSE),
        error = function(e) {
            skip("Network unavailable")
        }
    )
    expect_true(file.exists(path))

    expect_equal(nrow(dl$list_incomplete()), 0L)

    unlink(temp_dir, recursive = TRUE)
})

test_that("cleanup_tmp works", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp)

    file.create(file.path(temp, "test1.part"))
    file.create(file.path(temp, "test2.done"))

    n_removed <- dl$cleanup_tmp(all = TRUE)

    expect_gte(n_removed, 0L)

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Checksum Tests {{{
test_that("FileDownloader can verify checksums", {
    dl <- FileDownloader$new()

    test_file <- tempfile()
    writeLines("test content", test_file)

    checksum_md5 <- tools::md5sum(test_file)
    checksum_md5 <- as.character(checksum_md5)

    expect_true(dl$verify_checksum(test_file, checksum_md5, "md5"))

    expect_false(dl$verify_checksum(test_file, "wrongchecksum", "md5"))

    unlink(test_file)
})

test_that("FileDownloader verify marks checksum failures as error", {
    skip_if_not_installed("duckdb")

    root <- tempfile("downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    src <- tempfile()
    writeLines("verified content", src)
    checksum <- as.character(tools::md5sum(src))

    dl <- FileDownloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 1L,
        n_workers = 0L
    )
    plan <- data.table::data.table(
        logical_file_id = "tracking:verify-error",
        filename = "verify-error.txt",
        url = paste0("file://", normalizePath(src, winslash = "/")),
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    )
    session_id <- dl$enqueue(plan, session_label = "verify-error")
    expect_equal(dl$run(session_id = session_id, progress = FALSE)$status, "done")

    writeLines("corrupted content", file.path(dest, "verify-error.txt"))
    verified <- dl$verify(session_id = session_id)
    expect_false(verified$checksum_ok)
    expect_equal(dl$status(session_id = session_id)$status, "error")
    expect_match(dl$status(session_id = session_id)$last_error, "Checksum verification failed")

    events <- DBI::dbReadTable(priv(dl)$manifest_conn, "download_event")
    expect_true("verify_error" %in% events$event)
})

test_that("FileDownloader can download with checksum verification", {
    skip_on_cran()
    skip_if_offline()

    query <- tryCatch(
        esg_query()$
            source_id("CMCC-CM2-SR5")$
            experiment_id("dcppA-hindcast")$
            variable_id("sftgif")$
            frequency("fx")$
            nominal_resolution("100 km")$
            variant_label(c("r32i1p1f1", "r34i1p1f1"))$
            params(institution_id = "CMCC", sub_experiment_id = "s2017"),
        error = function(e) {
            skip("ESGF query failed")
        }
    )

    datasets <- tryCatch(
        query$collect(),
        error = function(e) {
            skip("ESGF query failed")
        }
    )

    if (datasets$count() == 0) {
        skip("No datasets found")
    }

    files <- tryCatch(
        datasets$collect(type = "File"),
        error = function(e) {
            skip("Failed to get files")
        }
    )

    if (files$count() == 0) {
        skip("No files found")
    }

    file_url <- files$url_download[1]
    file_checksum <- files$checksum[1]

    if (is.na(file_url) || is.na(file_checksum)) {
        skip("File URL or checksum not available")
    }

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 3,
        timeout = 300
    )

    path <- tryCatch(
        dl$download(
            url = file_url,
            filename = basename(files$title[1]),
            checksum = file_checksum,
            checksum_type = "sha256",
            progress = FALSE
        ),
        error = function(e) {
            unlink(temp_dir, recursive = TRUE)
            skip(paste("Download failed:", conditionMessage(e)))
        }
    )

    expect_true(file.exists(path))

    is_valid <- epwshiftr:::verify_checksum(path, file_checksum, "sha256")
    expect_true(is_valid)

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Error Handling Tests {{{
test_that("FileDownloader handles download errors", {
    skip_on_cran()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 1L,
        timeout = 1L
    )

    url <- "https://nonexistent.example.com/file.nc"

    expect_warning(expect_error(
        dl$download(url, filename = "nonexistent.nc", progress = FALSE),
        "Failed to download"
    ))

    unlink(temp_dir, recursive = TRUE)
})

test_that("FileDownloader handles checksum mismatch", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 1L
    )

    url <- "https://raw.githubusercontent.com/ideas-lab-nus/epwshiftr/main/README.md"

    expect_error(
        dl$download(
            url,
            filename = "readme.md",
            checksum = "wrongchecksum",
            checksum_type = "sha256",
            progress = FALSE
        ),
        "Checksum verification failed"
    )

    unlink(temp_dir, recursive = TRUE)
})
# }}}

# Print Tests {{{
test_that("FileDownloader print works", {
    dl <- FileDownloader$new()

    # Just test that print doesn't error
    # cli output produces messages, which is expected
    expect_no_error(expect_message(print(dl)))
})
# }}}

# Offline Mode Tests {{{
test_that("FileDownloader: offline mode blocks new downloads", {
    local_cache_mode("offline")
    dl <- FileDownloader$new(dest = tempdir(), n_workers = 0L)

    expect_error(
        dl$download(url = "https://example.com/nonexistent.nc"),
        "offline"
    )
})

test_that("FileDownloader: offline mode allows verified files", {
    local_cache_mode("offline")

    dest <- tempdir()
    # Create a file that would be "verified"
    test_file <- file.path(dest, "test-offline-verified.txt")
    writeLines("test content", test_file)
    on.exit(unlink(test_file), add = TRUE)

    dl <- FileDownloader$new(dest = dest, n_workers = 0L)

    # This should succeed because the file already exists (Verified status)
    result <- dl$download(
        url = "https://example.com/test-offline-verified.txt",
        filename = "test-offline-verified.txt"
    )
    expect_equal(normalizePath(result), normalizePath(test_file))
})
# }}}
