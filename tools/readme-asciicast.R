# Render a deterministic representative Future EPW run for the README. The
# README must exercise the production dashboard formatter without depending on
# live ESGF services or opening the persistent DuckDB store during a build.
readme_future_epw_demo <- function() {
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
            "belcher / historical 1995–2014",
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
