# Utility functions {{{
test_that("verify_checksum works with md5", {
    test_file <- withr::local_tempfile()
    writeLines("test content for checksum", test_file)

    checksum_md5 <- as.character(tools::md5sum(test_file))

    expect_true(epwshiftr:::verify_checksum(test_file, checksum_md5, "md5"))
    expect_false(epwshiftr:::verify_checksum(test_file, "wrongchecksum", "md5"))
})

test_that("verify_checksum works with sha256", {
    test_file <- withr::local_tempfile()
    writeLines("test content for sha256", test_file)

    con <- file(test_file, "rb")
    checksum_sha256 <- as.character(openssl::sha256(con))
    close(con)

    expect_true(epwshiftr:::verify_checksum(test_file, checksum_sha256, "sha256"))
    expect_false(epwshiftr:::verify_checksum(test_file, "wrongchecksum", "sha256"))
})

test_that("verify_checksum rejects invalid algo", {
    test_file <- withr::local_tempfile()
    writeLines("test", test_file)
    expect_error(epwshiftr:::verify_checksum(test_file, "abc", "crc32"))
})

test_that("format_bytes formats correctly", {
    expect_equal(epwshiftr:::format_bytes(0), "0 B")
    expect_equal(epwshiftr:::format_bytes(512), "512 B")
    expect_equal(epwshiftr:::format_bytes(1024), "1.00 KB")
    expect_equal(epwshiftr:::format_bytes(1048576), "1.00 MB")
    expect_equal(epwshiftr:::format_bytes(1073741824), "1.00 GB")
    expect_equal(epwshiftr:::format_bytes(NA), "? B")
    expect_equal(epwshiftr:::format_bytes(-1), "? B")
})
# }}}

# Creation {{{
test_that("FileDownloader can be created with defaults", {
    dl <- FileDownloader$new(n_workers = 0L)
    expect_s3_class(dl, "FileDownloader")
    expect_true(dir.exists(dl$data_dir))
    expect_true(dir.exists(dl$tmp_dir))
    expect_equal(dl$max_retries, 3L)
    expect_equal(dl$timeout, 3600L)
})

test_that("FileDownloader can be created with custom parameters", {
    temp_dir <- withr::local_tempdir()

    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 5L,
        timeout = 7200L,
        cleanup = FALSE,
        n_workers = 0L
    )
    expect_equal(dl$data_dir, normalizePath(temp_dir))
    expect_equal(dl$max_retries, 5L)
    expect_equal(dl$timeout, 7200L)
})

test_that("FileDownloader rejects invalid parameters", {
    expect_error(FileDownloader$new(dest = "/nonexistent/path/xyz", n_workers = 0L))
    expect_error(FileDownloader$new(retries = 0L, n_workers = 0L))
    expect_error(FileDownloader$new(timeout = -1L, n_workers = 0L))
})
# }}}

# File status {{{
test_that("FileDownloader check_file_status logic works", {
    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(dest = temp_dir, n_workers = 0L)

    # No files exist -> Missing status
    incomplete <- dl$list_incomplete()
    expect_equal(nrow(incomplete), 0L)
})
# }}}

# Cleanup {{{
test_that("cleanup_tmp removes all temp files when all=TRUE", {
    temp_dir <- withr::local_tempdir()
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp, n_workers = 0L)

    # Create some temp files
    file.create(file.path(temp, "test1.part"))
    file.create(file.path(temp, "test2.done"))
    file.create(file.path(temp, "test3.part"))

    n_removed <- dl$cleanup_tmp(all = TRUE)
    expect_equal(n_removed, 3L)

    # Verify files are gone
    remaining <- list.files(temp, pattern = "\\.(part|done)$")
    expect_equal(length(remaining), 0L)
})

test_that("cleanup_tmp smart cleanup removes orphaned files", {
    temp_dir <- withr::local_tempdir()
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp, n_workers = 0L)

    # Create orphaned .part file (no matching .done)
    file.create(file.path(temp, "orphan1.part"))
    # Create .done file (should be removed as orphaned)
    file.create(file.path(temp, "completed1.done"))
    # Create .part file with matching .done (should NOT be removed)
    file.create(file.path(temp, "inprogress.part"))
    file.create(file.path(temp, "inprogress.done"))

    n_removed <- dl$cleanup_tmp(all = FALSE)
    expect_gte(n_removed, 1L)
})

test_that("cleanup_tmp handles empty temp dir", {
    temp_dir <- withr::local_tempdir()
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp, n_workers = 0L)

    n_removed <- dl$cleanup_tmp(all = TRUE)
    expect_equal(n_removed, 0L)
})
# }}}

# Incomplete listing {{{
test_that("list_incomplete returns empty data.frame when no temp files", {
    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(dest = temp_dir, n_workers = 0L)

    incomplete <- dl$list_incomplete()
    expect_s3_class(incomplete, "data.frame")
    expect_equal(nrow(incomplete), 0L)
    expect_true("file" %in% names(incomplete))
    expect_true("status" %in% names(incomplete))
})

test_that("list_incomplete detects temp files", {
    temp_dir <- withr::local_tempdir()
    temp <- file.path(temp_dir, ".tmp")
    dir.create(temp)

    dl <- FileDownloader$new(dest = temp_dir, temp = temp, n_workers = 0L)

    # Create temp files
    writeLines("partial data", file.path(temp, "abc123.part"))
    writeLines("complete data", file.path(temp, "def456.done"))

    incomplete <- dl$list_incomplete()
    expect_s3_class(incomplete, "data.frame")
    expect_equal(nrow(incomplete), 2L)
})
# }}}

# Checksum verification via class method {{{
test_that("FileDownloader verify_checksum method works", {
    dl <- FileDownloader$new(n_workers = 0L)

    test_file <- withr::local_tempfile()
    writeLines("test content for verify", test_file)

    checksum_md5 <- as.character(tools::md5sum(test_file))

    expect_true(dl$verify_checksum(test_file, checksum_md5, "md5"))
    expect_false(dl$verify_checksum(test_file, "wrongchecksum", "md5"))
})
# }}}

# Download tests {{{
test_that("FileDownloader can download a file", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(dest = temp_dir, n_workers = 0L)

    # Use a small, reliable test URL
    url <- "https://raw.githubusercontent.com/ideas-lab-nus/epwshiftr/master/README.md"

    path <- tryCatch(
        dl$download(url, filename = "test_readme.md", progress = FALSE),
        error = function(e) {
            skip(paste("Network unavailable:", conditionMessage(e)))
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "test_readme.md")
    expect_gt(file.info(path)$size, 0)
})

test_that("FileDownloader can download with subdir", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(dest = temp_dir, n_workers = 0L)

    url <- "https://raw.githubusercontent.com/ideas-lab-nus/epwshiftr/master/README.md"

    path <- tryCatch(
        dl$download(
            url,
            filename = "data.md",
            subdir = "docs",
            progress = FALSE
        ),
        error = function(e) {
            skip("Network unavailable")
        }
    )

    expect_true(file.exists(path))
    expect_equal(basename(path), "data.md")
    expect_equal(dirname(path), file.path(normalizePath(temp_dir), "docs"))
})

test_that("FileDownloader skips existing verified file", {
    skip_on_cran()
    skip_if_offline()

    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(dest = temp_dir, n_workers = 0L)

    url <- "https://raw.githubusercontent.com/ideas-lab-nus/epwshiftr/master/README.md"

    # Download first time
    path1 <- tryCatch(
        dl$download(url, filename = "readme.md", progress = FALSE),
        error = function(e) {
            skip("Network unavailable")
        }
    )

    mtime1 <- file.info(path1)$mtime

    # Download again - should skip
    path2 <- dl$download(url, filename = "readme.md", progress = FALSE, overwrite = FALSE)
    mtime2 <- file.info(path2)$mtime

    expect_equal(path1, path2)
    expect_equal(mtime1, mtime2)
})
# }}}

# Error handling {{{
test_that("FileDownloader handles download errors", {
    skip_on_cran()

    temp_dir <- withr::local_tempdir()
    dl <- FileDownloader$new(
        dest = temp_dir,
        retries = 1L,
        timeout = 5L,
        n_workers = 0L
    )

    url <- "https://nonexistent.example.com/file.nc"

    expect_error(
        suppressWarnings(dl$download(url, filename = "nonexistent.nc", progress = FALSE)),
        "Failed to download|Could not resolve"
    )
})
# }}}

# Print method {{{
test_that("FileDownloader print works", {
    dl <- FileDownloader$new(n_workers = 0L)

    # cli output produces messages, which is expected
    expect_no_error(expect_message(print(dl)))
})
# }}}

