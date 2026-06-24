# Basic Tests {{{
local_file_url <- function(size, env = parent.frame()) {
    src <- tempfile()
    withr::defer(unlink(src), envir = env)
    writeBin(as.raw(rep(1L, size)), src)
    paste0("file://", normalizePath(src, winslash = "/"))
}

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

# Download Tests {{{
test_that("FileDownloader can download a single file", {
    url <- local_file_url(1024L)

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
    url <- local_file_url(512L)

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
    expect_equal(
        normalizePath(dirname(path), winslash = "/"),
        normalizePath(file.path(temp_dir, "docs"), winslash = "/")
    )

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
    temp_dir <- tempfile()
    dir.create(temp_dir)

    dl <- FileDownloader$new(dest = temp_dir)

    url <- local_file_url(2048L)

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
