# A terminal receipt can precede the coordinator's final store reads and exit.
# Readers must keep using the published snapshot until that external owner exits.
for (terminal_status in c("completed", "failed", "cancelled")) {
    test_that(
        paste("terminal process snapshot remains readable:", terminal_status),
        {
            fixture <- shared_inputs_test__fixture()
            plan <- fixture$batch@meta$children[[1L]]
            test_local_dependencies(list(shift_job__launch_job = function(...) {
                invisible(0L)
            }))
            run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
            run_id <- run@ids$run_id
            store <- shift_store(run)
            on.exit(store$close(), add = TRUE)
            job_id <- run@meta$jobs$job_id[[1L]]
            shift_job__job_update(
                store,
                job_id,
                pid = Sys.getpid() + 100000L,
                status = terminal_status
            )
            shift_job__run_update(store, run_id, status = terminal_status)
            store$close()
            alive <- TRUE
            local_mocked_bindings(
                downloader__pid_alive = function(pid) alive,
                shift_store = function(...) stop("manifest opened by observer")
            )
            restored <- shift_run_get(run_id, store = run@store_path)
            expect_identical(restored@ids$run_id, run_id)
            expect_identical(
                shift_status(restored, refresh = FALSE),
                terminal_status
            )
            expect_true(shift_job__live_process_is_active(restored))
            # An owner can verify its own durable state; terminal launches without a
            # process identity cannot claim the startup grace reserved for active jobs.
            restored@meta$jobs <- transform(
                restored@meta$jobs,
                pid = Sys.getpid()
            )
            expect_false(shift_job__live_process_is_active(restored))
            restored@meta$jobs <- transform(
                restored@meta$jobs,
                pid = NA_integer_
            )
            expect_false(shift_job__live_process_is_active(restored))
            alive <- FALSE
            expect_error(
                shift_run_get(run_id, store = run@store_path),
                "manifest opened by observer"
            )
        }
    )
}
