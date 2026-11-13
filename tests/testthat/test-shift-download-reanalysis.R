# A real OGC-shaped local service counts submissions and supplies asset bytes.
# Handlers have no package bindings and never contact Copernicus.
transfer_test__server <- function(mode = "ok") {
    app <- webfakes::new_app()
    app$locals$mode <- mode
    app$locals$posts <- 0L
    app$post(
        "/api/retrieve/v1/processes/:dataset/execution",
        function(req, res, locals) {
            locals$posts <- locals$posts + 1L
            if (locals$mode == "lost_submit") {
                Sys.sleep(0.3)
            }
            res$send_json(
                list(
                    status = "accepted",
                    links = list(list(
                        rel = "monitor",
                        href = paste0(
                            if (locals$mode == "relative") {
                                "../../jobs/job-"
                            } else {
                                "/api/retrieve/v1/jobs/job-"
                            },
                            locals$posts
                        )
                    ))
                ),
                auto_unbox = TRUE
            )
        }
    )
    app$get("/api/retrieve/v1/jobs/:job", function(req, res, locals) {
        res$send_json(
            list(
                status = if (locals$mode == "waiting") {
                    "running"
                } else {
                    "successful"
                }
            ),
            auto_unbox = TRUE
        )
    })
    app$get("/api/retrieve/v1/jobs/:job/results", function(req, res, locals) {
        res$send_json(
            list(
                asset = list(
                    value = list(
                        href = if (locals$mode == "relative") {
                            "asset?signature=secret-signed-url"
                        } else {
                            "/asset?signature=secret-signed-url"
                        },
                        `file:size` = if (locals$mode == "wrong_length") {
                            1024
                        } else if (
                            locals$mode %in% c("short_header", "long_header")
                        ) {
                            512
                        } else {
                            NULL
                        }
                    )
                )
            ),
            auto_unbox = TRUE,
            null = "null"
        )
    })
    app$get("/asset", function(req, res, locals) {
        res$set_header("Content-Type", "application/octet-stream")
        if (locals$mode == "relative") {
            return(res$set_status(404L)$send("wrong relative URL"))
        }
        if (locals$mode == "partial_response") {
            res$set_status(206L)
        }
        if (locals$mode == "slow") {
            res$send_chunk(as.raw(rep(1L, 128)))
            Sys.sleep(0.3)
            res$send_chunk(as.raw(rep(2L, 128)))
        } else if (locals$mode == "chunked") {
            res$send_chunk(as.raw(rep(1L, 2048)))
            res$send_chunk(as.raw(rep(2L, 2048)))
        } else if (locals$mode == "large") {
            res$send(as.raw(rep(1L, 512 * 1024)))
        } else {
            if (locals$mode %in% c("short_header", "long_header")) {
                res$set_header(
                    "Content-Length",
                    if (locals$mode == "short_header") "256" else "1024"
                )
                res$set_header("Connection", "close")
            }
            res$send(as.raw(rep(1L, 512)))
        }
    })
    app$get("/count", function(req, res, locals) {
        res$send_json(list(posts = locals$posts))
    })
    app$get("/api/retrieve/v1/jobs/:job/asset", function(req, res, locals) {
        res$send(as.raw(rep(1L, 512)))
    })
    webfakes::new_app_process(app)
}
# Avoid serializing the test/package namespace into each disposable HTTP server.
environment(transfer_test__server) <- baseenv()

test_that("raw transfers persist locators and reuse completed bytes without POST", {
    server <- transfer_test__server()
    withr::defer(server$stop())
    withr::local_envvar(c(
        ECMWF_DATASTORES_URL = paste0(sub("/$", "", server$url()), "/api"),
        ECMWF_DATASTORES_KEY = "test-token-never-store"
    ))
    plan <- shift_plan_reanalysis(
        shift_era5(
            2001,
            variables = "tas",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104)
    )
    directory <- tempfile()
    withr::defer(unlink(directory, recursive = TRUE))
    result <- shift_download_reanalysis(plan, directory)
    expect_identical(result$status, "downloaded")
    expect_equal(result$execution$submissions, 1)
    expect_equal(file.info(result$files)$size, 512)
    expect_identical(result$requests[[1]]$locator, "job-1")
    expect_equal(result$requests[[1]]$attempts[[4]]$bytes_received, 512)
    again <- shift_download_reanalysis(plan, directory)
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
    expect_identical(again$files, result$files)
    ledger <- readLines(result$receipt)
    expect_false(any(grepl("test-token|secret-signed|signature=", ledger)))
})

# Scope fake credentials, service and transfer directory to the calling test.
transfer_test__case <- function(
    mode = "ok",
    variables = "tas",
    env = parent.frame()
) {
    server <- transfer_test__server(mode)
    withr::defer(server$stop(), envir = env)
    withr::local_envvar(
        c(
            ECMWF_DATASTORES_URL = paste0(sub("/$", "", server$url()), "/api"),
            ECMWF_DATASTORES_KEY = "test-token-never-store"
        ),
        .local_envir = env
    )
    directory <- tempfile()
    withr::defer(unlink(directory, recursive = TRUE), envir = env)
    list(
        server = server,
        dir = directory,
        plan = shift_plan_reanalysis(
            shift_era5(
                2001,
                variables = variables,
                frequency = "hour",
                access = "arco"
            ),
            shift_site("point", lat = 1.25, lon = 104)
        )
    )
}

# Observe the server's submission counter through its external HTTP boundary.
transfer_test__posts <- function(server) {
    jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(paste0(server$url(), "count"))$content
    ))$posts
}

test_that("byte thresholds stop unknown-length bodies and retain failed traffic", {
    case <- transfer_test__case("chunked", c("tas", "ps"))
    result <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_file_bytes = 1024)
    )
    expect_identical(result$status, "incomplete")
    expect_length(result$files, 0)
    expect_identical(result$requests[[1]]$state, "budget_exhausted")
    expect_identical(result$requests[[1]]$error, "file_budget")
    expect_gt(result$execution$bytes_received, 1024)
    expect_lte(result$execution$asset_bytes_written, 1024)
    expect_identical(result$requests[[2]]$state, "not_submitted")
    expect_equal(transfer_test__posts(case$server), 1)
    again <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_file_bytes = Inf)
    )
    expect_identical(again$status, "downloaded")
    expect_equal(again$execution$submissions, 1)
    expect_equal(transfer_test__posts(case$server), 2)
    saved <- jsonlite::read_json(again$receipt)
    expect_gt(saved$executions[[1]]$bytes_received, 1024)
    expect_equal(file.info(again$files)$size, c(4096, 4096))
})

test_that("unsubmitted jobs remain eligible after a submission budget stop", {
    case <- transfer_test__case(variables = c("tas", "ps"))
    first <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_submissions = 1)
    )
    expect_length(first$files, 1)
    expect_equal(first$execution$submissions, 1)
    expect_identical(first$requests[[2]]$state, "budget_exhausted")
    unchanged <- shift_download_reanalysis(case$plan, case$dir)
    expect_equal(unchanged$execution$submissions, 0)
    expect_length(unchanged$files, 1)
    again <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_submissions = 2)
    )
    expect_identical(again$status, "downloaded")
    expect_equal(transfer_test__posts(case$server), 2)
})

test_that("uncertain POSTs are retained and never automatically resubmitted", {
    case <- transfer_test__case("lost_submit")
    first <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(wait_timeout = 0.1, total_timeout = 2)
    )
    expect_identical(first$requests[[1]]$state, "ambiguous")
    expect_equal(first$execution$submissions, 1)
    again <- shift_download_reanalysis(case$plan, case$dir)
    expect_identical(again$requests[[1]]$state, "ambiguous")
    expect_equal(again$execution$submissions, 0)
    expect_length(again$files, 0)
    expect_equal(transfer_test__posts(case$server), 1)
})

test_that("slow downloads keep partial-byte charges and resume the same job", {
    case <- transfer_test__case("slow")
    first <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(download_timeout = 0.1)
    )
    expect_identical(first$requests[[1]]$state, "budget_exhausted")
    expect_identical(first$requests[[1]]$error, "download_timeout")
    expect_length(first$files, 0)
    expect_gt(first$execution$asset_bytes_written, 0)
    again <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(download_timeout = 2)
    )
    expect_identical(again$status, "downloaded")
    expect_equal(again$execution$submissions, 0)
    expect_equal(transfer_test__posts(case$server), 1)
    expect_equal(file.info(again$files)$size, 256)
})

test_that("total response budgets carry failed traffic across resume calls", {
    case <- transfer_test__case(variables = c("tas", "ps"))
    first <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_total_bytes = 1200)
    )
    expect_identical(first$status, "incomplete")
    expect_gt(first$execution$bytes_received, 1200)
    expect_length(first$files, 1)
    again <- shift_download_reanalysis(case$plan, case$dir)
    expect_equal(again$execution$bytes_received, 0)
    expect_equal(again$execution$submissions, 0)
    last <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_total_bytes = 3000)
    )
    expect_identical(last$status, "downloaded")
    expect_equal(transfer_test__posts(case$server), 2)
    expect_lte(last$cumulative$bytes_received, 3000)
    expect_gt(last$cumulative$bytes_received, last$execution$bytes_received)
})

test_that("incorrect declared lengths never publish a completed asset", {
    for (mode in c("wrong_length", "short_header", "long_header")) {
        case <- transfer_test__case(mode)
        result <- shift_download_reanalysis(
            case$plan,
            case$dir,
            limits = list(download_timeout = 1)
        )
        expect_identical(result$status, "incomplete", info = mode)
        expect_length(result$files, 0)
        expect_gt(result$execution$bytes_received, 0)
        expect_null(result$requests[[1]]$raw_asset)
        case$server$stop()
    }
})

test_that("wait and total time budgets terminate without starting another job", {
    case <- transfer_test__case("waiting", c("tas", "ps"))
    result <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(wait_timeout = 0.15),
        poll_interval = 0.02
    )
    expect_identical(result$requests[[1]]$error, "wait_timeout")
    expect_identical(result$requests[[2]]$state, "not_submitted")
    expect_lt(result$execution$elapsed_seconds, 2)
    case$server$stop()
    case <- transfer_test__case("slow")
    result <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(total_timeout = 0.2, download_timeout = 2)
    )
    expect_identical(result$requests[[1]]$error, "total_timeout")
    expect_length(result$files, 0)
    expect_lt(result$execution$elapsed_seconds, 2)
})

test_that("other owners and changed ledgers cannot be reused or overwritten", {
    case <- transfer_test__case()
    dir.create(case$dir)
    lock <- file.path(case$dir, ".transfer-lock")
    dir.create(lock)
    writeLines('other-owner', file.path(lock, 'owner.json'))
    expect_error(shift_download_reanalysis(case$plan, case$dir), "locked")
    expect_identical(readLines(file.path(lock, "owner.json")), "other-owner")
    unlink(lock, recursive = TRUE)
    result <- shift_download_reanalysis(case$plan, case$dir)
    expect_error(
        shift_download_reanalysis(case$plan, case$dir, resume = FALSE),
        "already exists"
    )
    broken <- jsonlite::read_json(result$receipt)
    broken$requests[[1]]$request$variable <- "other_variable"
    jsonlite::write_json(
        broken,
        result$receipt,
        auto_unbox = TRUE,
        null = "null"
    )
    expect_error(
        shift_download_reanalysis(case$plan, case$dir),
        "receipt|ledger"
    )
    expect_equal(transfer_test__posts(case$server), 1)
})

test_that("crash-shaped ledgers preserve ambiguous state and accounting uncertainty", {
    case <- transfer_test__case()
    result <- shift_download_reanalysis(case$plan, case$dir)
    saved <- jsonlite::read_json(result$receipt)
    # This is an explicit crash fixture at the documented durable-file boundary,
    # not a claim that a hard-kill experiment or stale-lock takeover was run.
    saved$requests[[1]]$state <- "submitting"
    saved$requests[[1]]$locator <- NULL
    saved$requests[[1]]$raw_asset <- NULL
    saved$requests[[1]]$attempts <- saved$requests[[1]]$attempts[1]
    saved$requests[[1]]$attempts[[1]]$outcome <- "in_flight"
    jsonlite::write_json(
        saved,
        result$receipt,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
    )
    again <- shift_download_reanalysis(case$plan, case$dir)
    expect_identical(again$requests[[1]]$state, "ambiguous")
    expect_true(again$accounting_uncertain)
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
    expect_identical(
        again$requests[[1]]$attempts[[1]]$outcome,
        "interrupted_accounting_lower_bound"
    )
    expect_equal(transfer_test__posts(case$server), 1)
})

test_that("corrupted assets and invalid limits do not start fresh requests", {
    case <- transfer_test__case()
    for (limits in list(
        list(max_total_bytes = 0),
        list(max_submissions = 1.5),
        list(download_timeout = NA_real_),
        list(other = 1),
        list(total_timeout = -1)
    )) {
        expect_error(shift_download_reanalysis(
            case$plan,
            case$dir,
            limits = limits
        ))
    }
    expect_equal(transfer_test__posts(case$server), 0)
    result <- shift_download_reanalysis(case$plan, case$dir)
    writeBin(as.raw(1:3), result$files[[1]])
    again <- shift_download_reanalysis(case$plan, case$dir)
    expect_length(again$files, 0)
    expect_identical(again$requests[[1]]$error, "cached_asset_changed")
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
})

test_that("crossing a byte cap interrupts streaming before another callback", {
    case <- transfer_test__case("large")
    result <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(max_file_bytes = 1024)
    )
    attempt <- tail(result$requests[[1]]$attempts, 1)[[1]]
    expect_identical(attempt$outcome, "file_budget")
    expect_gt(attempt$bytes_received, 1024)
    # The curl stream reads at most 32768 bytes per callback in this runtime.
    # All 512 KiB would be consumed if the callback only returned FALSE.
    expect_lte(attempt$bytes_received, 32768)
    expect_equal(attempt$bytes_written, 0)
    expect_length(result$files, 0)
})

test_that("an early cache failure never returns later unverified assets", {
    case <- transfer_test__case(variables = c("tas", "ps"))
    first <- shift_download_reanalysis(case$plan, case$dir)
    for (path in first$files) {
        writeBin(as.raw(1:3), path)
    }
    again <- shift_download_reanalysis(case$plan, case$dir)
    expect_length(again$files, 0)
    expect_identical(again$status, "incomplete")
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
})

test_that("provider relative links use their response URL as the base", {
    case <- transfer_test__case("relative")
    result <- shift_download_reanalysis(case$plan, case$dir)
    expect_identical(result$status, "downloaded")
    expect_equal(file.info(result$files)$size, 512)
    expect_equal(transfer_test__posts(case$server), 1)
})

test_that("unsolicited partial-content responses are never complete assets", {
    case <- transfer_test__case("partial_response")
    result <- shift_download_reanalysis(case$plan, case$dir)
    expect_identical(result$status, "incomplete")
    expect_identical(result$requests[[1]]$error, "HTTP_206")
    expect_length(result$files, 0)
    expect_gt(result$execution$asset_bytes_written, 0)
})

test_that("restoring missing credentials resumes the original known job", {
    case <- transfer_test__case("slow")
    first <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(download_timeout = 0.1)
    )
    missing <- withr::with_envvar(c(ECMWF_DATASTORES_KEY = ""), {
        shift_download_reanalysis(case$plan, case$dir)
    })
    expect_identical(missing$requests[[1]]$error, "missing_credentials")
    expect_length(
        missing$requests[[1]]$attempts,
        length(first$requests[[1]]$attempts)
    )
    expect_equal(missing$execution$bytes_received, 0)
    expect_false(missing$accounting_uncertain)
    again <- shift_download_reanalysis(
        case$plan,
        case$dir,
        limits = list(download_timeout = 2)
    )
    expect_identical(again$status, "downloaded")
    expect_false(again$accounting_uncertain)
    expect_equal(again$execution$submissions, 0)
    expect_equal(transfer_test__posts(case$server), 1)
})

test_that("saved request plans download and reopen without duplicate submissions", {
    case <- transfer_test__case()
    saved <- tempfile(fileext = ".rds")
    withr::defer(unlink(saved))
    saveRDS(case$plan, saved)
    restored <- readRDS(saved)
    before <- serialize(restored, NULL)
    first <- shift_download_reanalysis(restored, case$dir)
    expect_identical(first$status, "downloaded")
    expect_equal(first$execution$submissions, 1)
    expect_identical(serialize(restored, NULL), before)
    second <- shift_download_reanalysis(readRDS(saved), case$dir)
    expect_equal(second$execution$submissions, 0)
    expect_equal(second$execution$bytes_received, 0)
    expect_equal(transfer_test__posts(case$server), 1)
    changed <- readRDS(saved)
    changed$requests$latitude[[1L]] <- changed$requests$latitude[[1L]] + 1e-10
    expect_error(shift_download_reanalysis(changed, case$dir), "unchanged")
    expect_equal(transfer_test__posts(case$server), 1)
})

# CDS exact-date plans use the same bounded ledger and never reset submission
# accounting when a partially completed plan resumes with an explicit new cap.
test_that("CDS date-limited plans retain submission budgets across resume", {
    case <- transfer_test__case()
    plan <- shift_plan_reanalysis(
        shift_era5(2001, variables = "clt", frequency = "hour", access = "cds"),
        shift_site("point", lat = 1.25, lon = 104),
        dates = c("2001-01-02", "2001-01-04")
    )
    first <- shift_download_reanalysis(
        plan,
        case$dir,
        limits = list(max_submissions = 1),
        poll_interval = 0.01
    )
    expect_equal(first$execution$submissions, 1)
    expect_equal(length(first$files), 1)
    second <- shift_download_reanalysis(
        plan,
        case$dir,
        limits = list(max_submissions = 2),
        poll_interval = 0.01
    )
    expect_identical(second$status, "downloaded")
    expect_equal(second$execution$submissions, 1)
    expect_equal(second$cumulative$submissions, 2)
    third <- shift_download_reanalysis(plan, case$dir)
    expect_equal(third$execution$submissions, 0)
    expect_equal(third$execution$bytes_received, 0)
    expect_equal(transfer_test__posts(case$server), 2)
})

# vim: fdm=marker :
