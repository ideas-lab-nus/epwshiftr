# Record the production discovery reporter with scripted responses, including
# a long server wait, future/historical checks, and an alternative input set.
# This exercises the same single framebuffer used by live catalog discovery.
readme__discovery_demo <- function() {
    # Recorders have no physical TTY: use the explicit frame backend, just as
    # the other README demos do, while the reporter owns all semantic state.
    ui <- epwshiftr::shift_ui("none", motion = "reduced")
    ui@batch_context <- list(kind = "discovery", total = 3L,
        target_models = 3L, site = "San Francisco International Airport",
        scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"),
        periods = "2041-2060 + 2071-2090")
    reporter <- epwshiftr:::shift__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    renderer <- epwshiftr:::ShiftFrameRenderer$new(
        output = cli::cli_output_connection(), backend = "frame")
    on.exit(renderer$close(), add = TRUE)
    withr::local_options(epwshiftr.ui_height = 26L)
    # Paint each milestone with the production formatter and terminal writer.
    show <- function(hold = 0.25) {
        state <- reporter$snapshot()
        # Compress the recording while keeping its scripted forty-second
        # request wait shorter than the displayed overall operation time.
        state$elapsed_seconds <- state$elapsed_seconds + 60
        renderer$draw(epwshiftr:::shift__ui_status_lines(state, width = 112L,
            motion = "reduced"), epwshiftr:::shift__ui_compact_line(state, width = 112L))
        Sys.sleep(hold)
    }
    reporter$operation_started("discovery", "Discover CMIP6 models",
        context = list(items = "San Francisco / 3 GCMs / 4 scenarios / 3 methods"))
    variables <- c("tas", "tasmax", "tasmin", "hurs", "psl", "rlds",
        "rsds", "sfcWind", "clt", "pr")
    methods <- c("original_morphing", "bws_btws", "epwshiftr")
    # Each sample represents a real reporter milestone, not a handcrafted box.
    for (index in seq_along(methods)) {
        transform <- epwshiftr::monthly_transform(methods[[index]])
        reporter$discovery_updated(list(current = index, method = methods[[index]],
            method_label = transform@label, alternative = 1L, alternatives = 2L,
            variables = variables, scope = "Candidate catalog", scope_periods = NULL,
            node = "https://esgf-data.dkrz.de",
            node_index = 1L, node_total = 3L), reset = TRUE)
        reporter$unit_started("Querying Dataset catalog",
            details = list(unit_type = "catalog", catalog_role = "Dataset"))
        show()
        reporter$unit_completed("Indexed 964 Dataset catalog records")
        reporter$discovery_updated(list(scope = "Future coverage",
            scope_periods = "2041-2060 + 2071-2090"), reset = TRUE)
        reporter$unit_started("Querying File catalog",
            details = list(unit_type = "catalog", catalog_role = "File"))
        reporter$heartbeat("Waiting for catalog response", details = list(
            request_started_at = as.numeric(Sys.time()) - 40,
            last_response_at = as.numeric(Sys.time()) - 42,
            query_timeout = 300, transfer_state = "transfer",
            responses = 2L, cache_hits = 1L, records_received = 2400L), force = TRUE)
        show(if (index == 1L) 1 else 0.25)
        reporter$unit_completed("Indexed 17185 File catalog records")
        reporter$discovery_updated(list(scope = "Historical coverage",
            scope_periods = "1973-2005"), reset = TRUE)
        reporter$unit_started("Querying File catalog",
            details = list(unit_type = "catalog", catalog_role = "File"))
        show()
        reporter$unit_completed("Indexed 2844 File catalog records")
        reporter$discovery_updated(list(alternative = 2L,
            variables = c(setdiff(variables, "hurs"), "huss", "ps"),
            scope = "Candidate catalog", scope_periods = NULL), reset = TRUE)
        reporter$unit_started("Querying Dataset catalog",
            details = list(unit_type = "catalog", catalog_role = "Dataset"))
        show()
        reporter$notice(paste(transform@label, "/ combination 2: 5 GCMs with complete coverage"),
            outcome = "completed")
    }
    reporter$discovery_updated(list(common_models = 4L,
        selected_models = c("Model-A", "Model-B", "Model-C")), reset = TRUE)
    reporter$operation_completed("3 GCMs selected with complete coverage across all 3 methods")
    show(0)
    renderer$commit()
}

# Render a deterministic representative Future EPW run for the README. The
# README must exercise the production dashboard formatter without depending on
# live ESGF services or opening the persistent DuckDB store during a build.
readme__future_epw_demo <- function() {
    stages <- c(
        "resolve", "extract_future", "extract_reference",
        "coverage", "morph", "write_epw"
    )
    # Canonicalising the existing temporary root lets the production path
    # formatter show `<tempdir>` instead of embedding a build-specific path.
    output_dir <- file.path(
        normalizePath(tempdir(), winslash = "/", mustWork = TRUE),
        "epwshiftr"
    )
    output_paths <- file.path(output_dir, c(
        "SGP_Singapore_BCC-CSM2-MR_ssp126_r1i1p1f1_2060s.epw",
        "SGP_Singapore_BCC-CSM2-MR_ssp585_r1i1p1f1_2060s.epw"
    ))
    plan_context <- list(
        items = c(
            "BCC-CSM2-MR",
            "ssp126 + ssp585",
            "2060s (2055–2065)",
            paste(epwshiftr::monthly_transform("original_morphing")@label,
                "/ historical 1995–2014"),
            "2 EPWs"
        ),
        selection = "member r1i1p1f1 · grid gn",
        output = output_dir
    )
    state <- list(
        run_id = "run_readme8",
        status = "running",
        stage = "resolve",
        stage_sequence = stages,
        completed_stages = character(),
        stage_message = "Resolving complete CMIP6 workflow inputs",
        unit_label = "Checking future + reference catalogs",
        unit_current = 2L,
        unit_total = 6L,
        current_details = list(
            current = 2L,
            total = 6L,
            node = "https://esgf.ceda.ac.uk",
            catalog_role = "future + reference"
        ),
        node_rows = data.table::data.table(
            node = "DKRZ",
            future = 28L,
            reference = 39L,
            outcome = "rejected",
            duration = "3s",
            result = "coverage: incomplete member/grid coverage"
        ),
        plan_context = plan_context,
        cases_ready = 0L,
        cases_total = 2L,
        outputs_completed = 0L,
        recent_events = character(),
        recent_outcomes = character(),
        elapsed_seconds = 5
    )

    renderer <- epwshiftr:::ShiftFrameRenderer$new(
        output = cli::cli_output_connection(),
        backend = "frame"
    )
    on.exit(renderer$close("done"), add = TRUE)

    # Each state uses the same semantic fields emitted by ShiftReporter. Short
    # spinner updates make motion visible while keeping the recording compact.
    draw_state <- function(next_state, hold = 0.45) {
        # `modifyList()` recursively merges nested lists, but reporter details
        # belong only to the active unit; replace them so node metadata from
        # resolve cannot leak into extraction or morph frames.
        if ("current_details" %in% names(next_state)) {
            state$current_details <<- next_state$current_details
            next_state$current_details <- NULL
        }
        state <<- utils::modifyList(state, next_state)
        for (frame in seq_len(4L)) {
            lines <- epwshiftr:::shift__ui_status_lines(
                state,
                width = 112L,
                motion = "full",
                frame = frame
            )
            compact <- epwshiftr:::shift__ui_compact_line(
                state,
                width = 112L,
                motion = "full",
                frame = frame
            )
            renderer$draw(lines, compact)
            Sys.sleep(0.06)
        }
        Sys.sleep(hold)
    }

    draw_state(list())
    draw_state(list(
        stage = "extract_future",
        completed_stages = "resolve",
        unit_label = "ssp126 · hurs · 2055–2065",
        unit_current = 7L,
        unit_total = 20L,
        current_details = list(
            current = 7L,
            total = 20L,
            scenario = "ssp126",
            variable = "hurs",
            access_method = "OPeNDAP"
        ),
        recent_events = "Selected CEDA · r1i1p1f1 / gn",
        recent_outcomes = "completed",
        elapsed_seconds = 12
    ))
    draw_state(list(
        stage = "extract_reference",
        completed_stages = c("resolve", "extract_future"),
        unit_label = "historical · tas · 1995–2014",
        unit_current = 4L,
        unit_total = 10L,
        current_details = list(
            current = 4L,
            total = 10L,
            scenario = "historical",
            variable = "tas",
            access_method = "OPeNDAP"
        ),
        recent_events = c(
            "Selected CEDA · r1i1p1f1 / gn",
            "Extracted 20 future plans"
        ),
        recent_outcomes = c("completed", "completed"),
        elapsed_seconds = 18
    ))
    draw_state(list(
        stage = "coverage",
        completed_stages = c(
            "resolve", "extract_future", "extract_reference"
        ),
        unit_label = "Checking requested scenarios, variables, and years",
        unit_current = 2L,
        unit_total = 2L,
        current_details = list(current = 2L, total = 2L),
        cases_ready = 2L,
        recent_events = c(
            "Extracted 20 future plans",
            "Extracted 10 historical reference plans"
        ),
        elapsed_seconds = 21
    ))
    draw_state(list(
        stage = "morph",
        completed_stages = c(
            "resolve", "extract_future", "extract_reference", "coverage"
        ),
        unit_label = "ssp585 · 2060s · Belcher change factors",
        unit_current = 2L,
        unit_total = 2L,
        current_details = list(
            current = 2L,
            total = 2L,
            scenario = "ssp585",
            period = "2060s"
        ),
        recent_events = c(
            "2/2 requested cases have complete coverage",
            "Morphed ssp126 · 2060s"
        ),
        elapsed_seconds = 24
    ))
    draw_state(list(
        stage = "write_epw",
        completed_stages = c(
            "resolve", "extract_future", "extract_reference", "coverage",
            "morph"
        ),
        unit_label = "Exporting ssp585 · 2060s",
        unit_current = 2L,
        unit_total = 2L,
        current_details = list(current = 2L, total = 2L),
        outputs_completed = 2L,
        recent_events = c(
            "Morphed ssp585 · 2060s",
            "Exported ssp126 · 2060s"
        ),
        elapsed_seconds = 26
    ))

    # Commit the durable completion receipt exactly as a foreground run does,
    # so the README demonstrates what remains in terminal scrollback.
    state <- utils::modifyList(state, list(
        status = "completed",
        completed_stages = stages,
        unit_label = "Exported final EPWs",
        current_details = list(
            current = 2L,
            total = 2L,
            outcome = "completed"
        ),
        output_dir = output_dir,
        output_paths = output_paths,
        elapsed_seconds = 27
    ))
    # Use the production completion facts so case/file semantics cannot drift
    # back to the legacy assumption that every case always writes one file.
    completion <- epwshiftr:::shift__ui_completion(
        data.table::data.table(status = rep("completed", 2L)),
        data.table::data.table(export_path = output_paths),
        epwshiftr:::shift_diagnostics_empty()
    )
    state[names(completion)] <- completion
    renderer$draw(
        epwshiftr:::shift__ui_status_lines(
            state,
            width = 112L,
            motion = "full",
            frame = 1L
        ),
        epwshiftr:::shift__ui_compact_line(
            state,
            width = 112L,
            motion = "full",
            frame = 1L
        )
    )
    Sys.sleep(1.5)
    renderer$commit("done")
    invisible(NULL)
}

# Build representative batch snapshots from real transform metadata. The
# counts describe eight cases, while no climate data or files are fabricated.
readme__batch_states <- function() {
    transforms <- list(
        epwshiftr::monthly_transform("original_morphing"),
        epwshiftr::daily_transform("qdm")
    )
    models <- c("BCC-CSM2-MR", "MPI-ESM1-2-HR")
    children <- data.table::rbindlist(lapply(transforms, function(transform) {
        record <- epwshiftr:::transform__record(transform@scale, transform@method)
        data.table::data.table(
            method = transform@method,
            scale = transform@scale,
            reconstruction = transform@reconstruction,
            model = models,
            method_status = epwshiftr:::recipe__get(record$recipe)@status,
            status = "queued",
            current_stage = NA_character_
        )
    }))
    children[, child_key := paste0("child_readme", seq_len(.N))]
    summary <- data.table::data.table(
        batch_id = "batch_readme8",
        status = "running",
        configurations = 2L,
        models = 2L,
        children = 4L,
        completed = 0L,
        active = 4L,
        failed = 0L,
        partial = 0L,
        waiting = 0L,
        cancelled = 0L,
        cases = 8L,
        epw_files = 0L,
        warnings = 0L,
        output_dir = "future-epw"
    )
    snapshot <- list(
        batch = summary,
        children = children,
        cases = data.table::data.table(),
        outputs = data.table::data.table(),
        diagnostics = epwshiftr:::shift_diagnostics_empty(),
        execution = data.table::data.table()
    )
    children[1:2, `:=`(status = "running", current_stage = "extract_future")]
    states <- list(data.table::copy(snapshot))

    # Advance both methods through the same renderer used by shift_watch().
    # Copies keep earlier frames stable despite data.table's reference updates.
    children[1:2, `:=`(status = "completed", current_stage = "write_epw")]
    children[3:4, `:=`(status = "running", current_stage = "morph")]
    summary[, `:=`(completed = 2L, active = 2L, epw_files = 4L)]
    states[[2L]] <- data.table::copy(snapshot)

    children[, `:=`(status = "completed", current_stage = "write_epw")]
    summary[, `:=`(status = "completed", completed = 4L, active = 0L,
        epw_files = 8L, warnings = 2L)]
    snapshot$diagnostics <- data.table::data.table(
        severity = "warning",
        method = "qdm",
        model = models,
        message = "Signal defaults for 'tas' are experimental."
    )
    snapshot$execution <- data.table::data.table(
        child_key = children$child_key,
        action = "started",
        elapsed_seconds = c(12, 14, 18, 20)
    )
    snapshot$call_elapsed_seconds <- 64
    states[[3L]] <- data.table::copy(snapshot)

    # A repeated completed request retains its warnings and reports reuse
    # separately from the elapsed time of the original work.
    snapshot$execution[, `:=`(action = "reused", elapsed_seconds = 0.05)]
    snapshot$call_elapsed_seconds <- 0.2
    states[[4L]] <- data.table::copy(snapshot)
    states
}

# Extend the small-batch recording with a scripted large matrix and actionable
# failure. All frames still use the production view and framebuffer renderer.
readme__batch_review_states <- function() {
    states <- readme__batch_states()
    snapshot <- data.table::copy(states[[1L]])
    snapshot$children <- snapshot$children[rep(c(1L, 3L), each = 8L)]
    snapshot$children[, `:=`(
        child_key = paste0("child_large", seq_len(.N)),
        model = rep(paste0("Model-", seq_len(8L)), 2L),
        status = "completed", current_stage = "write_epw")]
    snapshot$children[14:16, `:=`(status = "running", current_stage = "extract_future")]
    snapshot$batch[, `:=`(batch_id = "batch_readme_large", models = 8L,
        children = 16L, cases = 32L, completed = 13L, active = 3L, epw_files = 26L)]
    snapshot$activity <- list(child_large14 = list(
        stage = "extract_future", status = "running",
        unit_label = "ssp585 · tas · 2055–2065", elapsed_seconds = 42,
        updated_at = "2026-09-16 10:00:42",
        current_details = list(unit_type = "extract_plan", phase = "unit",
            current = 7L, total = 20L, access_method = "OPeNDAP")))
    states[[5L]] <- data.table::copy(snapshot)

    # Show a failure and a cancellation while another child remains active.
    # This exercises priority, diagnostic actions, and the hidden-child hint.
    snapshot$children[15L, status := "failed"]
    snapshot$children[16L, status := "cancelled"]
    snapshot$batch[, `:=`(status = "failed", active = 1L, failed = 1L, cancelled = 1L)]
    snapshot$diagnostics <- data.table::data.table(severity = "error",
        method = "qdm", model = "Model-7", message = "Calibration file is missing.",
        action = "Restore the calibration file, then resume this batch.")
    states[[6L]] <- data.table::copy(snapshot)
    states
}

# Record the production batch view with the same atomic framebuffer used by
# foreground workflows. Only the representative input states are scripted.
readme__batch_demo <- function() {
    old <- options(epwshiftr.ui_height = 24L)
    on.exit(options(old), add = TRUE)
    renderer <- epwshiftr:::ShiftFrameRenderer$new(
        output = cli::cli_output_connection(),
        backend = "frame"
    )
    on.exit(renderer$close("done"), add = TRUE)
    for (snapshot in readme__batch_review_states()) {
        for (frame in seq_len(4L)) {
            view <- epwshiftr:::shift_batch__view(snapshot, width = 112L,
                motion = "full", frame = frame, height = epwshiftr:::shift__ui_height())
            renderer$draw(view$lines, view$compact)
            Sys.sleep(0.06)
        }
        Sys.sleep(1.2)
    }
    renderer$commit("done")
    invisible(NULL)
}
