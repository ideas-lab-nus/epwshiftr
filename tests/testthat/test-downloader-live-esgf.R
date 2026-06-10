skip_live_esgf_downloader <- function() {
    run <- tolower(Sys.getenv("EPWSHIFTR_RUN_LIVE_ESGF", "false"))
    testthat::skip_if_not(
        run %in% c("1", "true", "yes"),
        "Set EPWSHIFTR_RUN_LIVE_ESGF=true to run live ESGF downloader tests."
    )
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("curl")
    skip_if_not_installed("duckdb")
}

live_esgf_files <- function() {
    q <- esg_query(INDEX_NODES[["DKRZ"]])$
        source_id("CMCC-CM2-SR5")$
        experiment_id("dcppA-hindcast")$
        variable_id("sftgif")$
        frequency("fx")$
        nominal_resolution("100 km")$
        variant_label(c("r32i1p1f1", "r34i1p1f1"))$
        params(institution_id = "CMCC", sub_experiment_id = "s2017")$
        limit(2L)

    datasets <- q$collect()
    files <- datasets$collect(type = "File", limit = 2L)
    if (files$count() < 2L) {
        skip("Live ESGF query returned fewer than two file records.")
    }

    plan <- files$download_plan(replica = "current", probe = TRUE, strategy = "fastest")
    plan <- plan[!is.na(url) & nzchar(url)]
    if (nrow(plan) < 2L) {
        skip("Live ESGF query returned fewer than two HTTPServer URLs.")
    }
    if (any(plan$size[seq_len(2L)] > 1024^2)) {
        skip("Live ESGF query returned files larger than the live downloader budget.")
    }
    checksums <- plan$checksum[seq_len(2L)]
    if (any(is.na(checksums) | !nzchar(checksums))) {
        skip("Live ESGF query returned files without checksums.")
    }

    list(files = files, plan = plan[seq_len(2L)])
}

write_live_partial <- function(url, file, bytes = 1024L) {
    handle <- curl::new_handle()
    curl::handle_setopt(handle, followlocation = TRUE, timeout = 60)
    curl::handle_setheaders(handle, Range = sprintf("bytes=0-%d", bytes - 1L))
    curl::curl_fetch_disk(url, file, handle = handle)
    invisible(file)
}

capture_live_warnings <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(
        force(expr),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(value = value, warnings = warnings)
}

test_that("Downloader live ESGF workflow covers persistent methods and resume", {
    skip_live_esgf_downloader()

    live <- tryCatch(live_esgf_files(), error = function(e) skip(conditionMessage(e)))
    files <- live$files
    plan <- live$plan
    bad_url <- "http://127.0.0.1:1/epwshiftr-live-missing.nc"

    root <- tempfile("live-esgf-downloader-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dest <- file.path(root, "downloads")
    temp <- file.path(root, "tmp")
    manifest <- file.path(root, "_downloader", "manifest.duckdb")

    dl <- Downloader$new(
        dest = dest,
        temp = temp,
        manifest = manifest,
        retries = 2L,
        timeout = 240L,
        n_workers = 0L
    )

    expect_s3_class(dl, "Downloader")
    expect_s3_class(dl$clone(deep = FALSE), "Downloader")
    expect_s3_class(print(dl), "Downloader")
    expect_identical(dl$get_tasks(), list())
    expect_identical(dl$wait_for_tasks(), list())
    expect_error(dl$get_task_status("missing-task"), "not found")
    expect_error(dl$cancel_task("missing-task"), "not found")

    config_file <- dl$save_config()
    expect_true(file.exists(config_file))

    shortcut_dir <- file.path(root, "shortcut")
    shortcut <- Downloader$new(dest = shortcut_dir, temp = file.path(root, "shortcut-tmp"), retries = 2L, timeout = 240L, n_workers = 0L)
    shortcut_path <- shortcut$download(
        url = plan$url[[1L]],
        filename = plan$filename[[1L]],
        checksum = plan$checksum[[1L]],
        checksum_type = plan$checksum_type[[1L]],
        progress = FALSE,
        block = FALSE
    )
    expect_true(file.exists(shortcut_path))
    expect_true(shortcut$verify_checksum(shortcut_path, plan$checksum[[1L]], tolower(plan$checksum_type[[1L]])))
    file.create(file.path(shortcut$tmp_dir, "manual.part"))
    expect_equal(nrow(shortcut$list_incomplete()), 1L)
    expect_equal(shortcut$cleanup_tmp(all = TRUE), 1L)

    resume_plan <- data.table::copy(plan[1L])
    resume_plan$logical_file_id <- paste0(resume_plan$logical_file_id, ":resume")
    resume_session <- dl$enqueue(resume_plan, session_label = "live-resume")
    resume_task <- dl$tasks(session_id = resume_session)
    partial_path <- file.path(dl$tmp_dir, paste0(resume_task$task_id[[1L]], ".part"))
    write_live_partial(resume_plan$url[[1L]], partial_path)
    expect_true(file.exists(partial_path))
    expect_gt(file.info(partial_path)$size, 0)
    expect_equal(nrow(dl$list_incomplete()), 1L)

    resumed <- dl$resume(session_id = resume_session, progress = FALSE)
    expect_equal(resumed$status, "done")
    expect_true(file.exists(resumed$target_path[[1L]]))
    expect_true(dl$verify(session_id = resume_session)$checksum_ok)
    expect_true(dl$verify(task_id = resume_task$task_id[[1L]])$checksum_ok)
    expect_equal(dl$status(task_id = resume_task$task_id[[1L]])$status, "done")

    fallback_plan <- data.table::copy(plan[2L])
    fallback_plan <- data.table::rbindlist(list(
        data.table::copy(fallback_plan)[, `:=`(
            url = bad_url,
            priority = 1L
        )],
        data.table::copy(fallback_plan)[, priority := 2L]
    ), use.names = TRUE, fill = TRUE)
    fallback_session <- dl$enqueue(fallback_plan, session_label = "live-fallback")
    fallback_run <- capture_live_warnings(dl$run(session_id = fallback_session, progress = FALSE))
    expect_true(any(grepl("Failed to open|Download failed", fallback_run$warnings)))
    fallback_tasks <- fallback_run$value
    expect_equal(fallback_tasks$status, "done")
    expect_identical(fallback_tasks$selected_url, plan$url[[2L]])

    error_plan <- data.table::copy(plan[1L])
    error_plan$logical_file_id <- paste0(error_plan$logical_file_id, ":error")
    error_plan$filename <- paste0("error-", error_plan$filename)
    error_plan$url <- bad_url
    error_plan$checksum <- NA_character_
    error_plan$checksum_type <- "sha256"
    error_session <- dl$enqueue(error_plan, session_label = "live-error")
    error_run <- capture_live_warnings(dl$run(session_id = error_session, progress = FALSE))
    expect_true(any(grepl("Failed to open|Download failed", error_run$warnings)))
    error_tasks <- error_run$value
    expect_equal(error_tasks$status, "error")
    expect_equal(dl$status(session_id = error_session)$status, "error")
    retried <- dl$retry(task_id = error_tasks$task_id[[1L]])
    expect_equal(retried$status, "queued")
    retry_run <- capture_live_warnings(dl$run(task_id = error_tasks$task_id[[1L]], progress = FALSE))
    expect_true(any(grepl("Failed to open|Download failed", retry_run$warnings)))
    expect_equal(retry_run$value$status, "error")

    sessions <- dl$sessions()
    expect_true(all(c(resume_session, fallback_session, error_session) %in% sessions$session_id))
    expect_gte(nrow(dl$tasks()), 3L)

    rm(dl)
    gc()
    conn <- ddb_connect(manifest, read_only = TRUE)
    candidates <- data.table::as.data.table(ddb_read_table(conn, "download_candidate"))
    fallback_tid <- fallback_tasks$task_id[[1L]]
    failed_candidate <- candidates[candidates[["task_id"]] == fallback_tid][order(priority)][1L]
    expect_equal(failed_candidate$failed_count, 1L)
    ddb_disconnect(conn, shutdown = TRUE)

    restored <- Downloader$load_config(config_file)
    expect_equal(restored$manifest, normalizePath(manifest, mustWork = FALSE, winslash = "/"))
    expect_true(all(c(resume_session, fallback_session, error_session) %in% restored$sessions()$session_id))
    expect_true(restored$verify(session_id = resume_session)$checksum_ok)

    store_dir <- file.path(root, "store")
    store <- EsgStore$new(store_dir)
    on.exit(store$close(), add = TRUE)
    store_dl <- store$downloader(n_workers = 0L, retries = 2L, timeout = 240L)
    store_session <- store$download_files(
        files = files,
        replica = "current",
        downloader = store_dl,
        run = TRUE,
        session_label = "live-store-download",
        probe = TRUE,
        progress = FALSE
    )
    store_tasks <- store_dl$tasks(session_id = store_session)
    expect_equal(sort(store_tasks$status), c("done", "done"))
    expect_true(all(file.exists(store_tasks$target_path)))
    expect_true(all(store_dl$verify(session_id = store_session)$checksum_ok))

    catalog <- store$query("SELECT filename, local_path, local_artifact_id FROM file_catalog")
    expect_equal(nrow(catalog), 2L)
    expect_false(any(is.na(catalog$local_path)))
    expect_false(any(is.na(catalog$local_artifact_id)))

    for (path in store_tasks$target_path) {
        ds <- EsgDataset$new(path)
        ds$open()
        on.exit(ds$close(), add = TRUE)
        expect_true(length(ds$get_variables()) > 0L)
        expect_true(length(ds$get_dimensions()) > 0L)
    }
})
