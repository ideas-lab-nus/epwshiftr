# A real process restart must trust complete saved output evidence while missing
# or conflicting lineage rejects only its plan and preserves the historical bytes.
test_that("fresh R processes verify offline Store lineage per requested plan", {
    withr::local_options(
        epwshiftr.cache = TRUE,
        epwshiftr.progress = FALSE,
        epwshiftr.dir_cache = cache <- withr::local_tempdir()
    )
    root <- withr::local_tempdir()
    paths <- file.path(root, c("source.nc", "peer.nc"))
    write_local_cmip6_netcdf_fixture(paths[[1L]], 2060L, calendar = "360_day")
    file.copy(paths[[1L]], paths[[2L]])
    nc <- RNetCDF::open.nc(paths[[2L]], write = TRUE)
    RNetCDF::var.put.nc(nc, "tas", RNetCDF::var.get.nc(nc, "tas") + 7)
    RNetCDF::close.nc(nc)
    expected <- lapply(seq_along(paths), function(index) {
        nc <- RNetCDF::open.nc(paths[[index]])
        on.exit(RNetCDF::close.nc(nc))
        as.double(RNetCDF::var.get.nc(
            nc,
            "tas",
            start = c(index + 1L, 1L, 1L),
            count = c(1L, 1L, 2L)
        ))
    })
    logs <- file.path(root, c("source-requests.txt", "peer-requests.txt"))
    servers <- lapply(seq_along(paths), function(index) {
        webfakes::new_app_process(durable_test__app(
            readBin(paths[[index]], "raw", file.info(paths[[index]])$size),
            logs[[index]]
        ))
    })
    withr::defer(lapply(servers, function(server) server$stop()))
    labels <- paste(
        rep(c("ordinary", "pooled"), each = 3L),
        rep(c("valid", "missing", "conflict"), 2L),
        sep = "-"
    )
    cases <- before <- stats::setNames(vector("list", length(labels)), labels)

    for (mode in c("ordinary", "pooled")) {
        for (fault in c("valid", "missing", "conflict")) {
            label <- paste(mode, fault, sep = "-")
            producer <- durable_test__producer(
                file.path(root, label),
                mode,
                servers,
                paths
            )
            store <- producer$store
            artifact <- store$query(
                "SELECT * FROM artifact WHERE kind = 'extract'"
            )
            plans <- store$query("SELECT * FROM extraction_plan")
            key <- plans$file_key[match(producer$plans[[1L]], plans$plan_id)]
            bad <- artifact[artifact$file_key == key, ]
            metadata <- jsonlite::fromJSON(
                bad$metadata_json,
                simplifyVector = FALSE
            )
            if (fault == "missing") {
                metadata$source_lineage <- NULL
            }
            if (fault == "conflict") {
                metadata$source_lineage$request$identity$source_id <- "WrongModel"
            }
            if (fault != "valid") {
                store$register_artifact(
                    "extract",
                    store$artifact_path(bad$artifact_id),
                    file_key = bad$file_key,
                    query_id = bad$query_id,
                    metadata = metadata
                )
            }
            before[[label]] <- durable_test__snapshot(store)
            durable_test__remove_native(store)
            cases[[label]] <- list(
                path = store$path,
                plans = producer$plans,
                mode = mode,
                options = identical(fault, "valid")
            )
            store$close()
        }
    }
    requests <- durable_test__requests(logs)
    durable_test__record(
        "offline-output-inputs",
        list(
            expected = expected,
            before = before,
            source_sha256 = vapply(paths, checksum_file, character(1L)),
            requests = requests
        ),
        c(paths, logs)
    )
    unlink(paths)

    lapply(servers, function(server) server$stop())
    readback <- durable_test__launch(cases, cache)
    expect_false(identical(readback$pid, Sys.getpid()))
    expect_identical(readback$parent_pid, Sys.getpid())
    expect_identical(durable_test__requests(logs), requests)
    for (label in names(cases)) {
        answer <- readback$answers[[match(label, names(cases))]]
        result <- answer$result[
            match(cases[[label]]$plans, answer$result$plan_id),
        ]
        expect_identical(result$status[[2L]], "done", info = label)
        if (grepl("valid$", label)) {
            expect_true(all(result$status == "done"), info = label)
            expect_true(
                "epwshiftr_store_extract_conflict" %in% answer$conflict,
                info = label
            )
            expect_true(all(answer$replacement$status == "done"), info = label)
            expect_identical(nrow(answer$snapshot$results), 2L, info = label)
        } else {
            expect_identical(result$status[[1L]], "failed", info = label)
            expect_match(
                result$last_error[[1L]],
                if (grepl("missing$", label)) {
                    "insufficient.*lineage"
                } else {
                    "source conflict"
                },
                info = label
            )
            expect_match(
                result$last_error[[1L]],
                cases[[label]]$plans[[1L]],
                fixed = TRUE,
                info = label
            )
        }
        for (index in seq_along(cases[[label]]$plans)) {
            row <- match(
                cases[[label]]$plans[[index]],
                answer$snapshot$results$plan_id
            )
            expect_equal(
                answer$snapshot$values[[row]],
                expected[[index]],
                tolerance = 1e-10,
                info = label
            )
        }
        expect_identical(
            answer$snapshot$output_md5,
            before[[label]]$output_md5,
            info = label
        )
        expect_identical(
            answer$snapshot$reference_md5,
            before[[label]]$reference_md5,
            info = label
        )
        expect_equal(
            answer$snapshot$values,
            before[[label]]$values,
            tolerance = 1e-10,
            info = label
        )
    }
})

# vim: fdm=marker :
