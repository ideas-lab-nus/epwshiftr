# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("ShiftRequest print shares the ESGF query renderer", {
    req <- shift_request(
        project = "CMIP6",
        experiment = c("ssp126", "ssp585"),
        variables = c("tas", "hurs"),
        frequency = "mon",
        filters = list(table_id = "Amon")
    )

    printed <- capture.output(print(req, width = 60L), type = "message")
    expect_true(any(grepl("ESGF request", printed, fixed = TRUE)))
    expect_true(any(grepl("^[=═]{2} ESGF request", printed)))
    expect_true(any(grepl("^[*•] Index node: auto", printed)))
    expect_true(any(grepl("Query parameters", printed, fixed = TRUE)))
    expect_true(any(grepl(
        "experiment_id = ssp126, ssp585",
        printed,
        fixed = TRUE
    )))
    expect_true(any(grepl("table_id = Amon", printed, fixed = TRUE)))
    expect_false(any(grepl("stage:  request", printed, fixed = TRUE)))

    verbose <- capture.output(print(req, verbose = TRUE), type = "message")
    expect_true(any(grepl("Workflow", verbose, fixed = TRUE)))
    expect_true(any(grepl("Status: new", verbose, fixed = TRUE)))
})

test_that("Shift configuration printers use compact semantic receipts", {
    withr::local_options(cli.num_colors = 1L)
    climate <- shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585"))
    control <- shift_control()
    expect_false(control@refresh)
    expect_true(shift_control(refresh = TRUE)@refresh)
    expect_error(shift_control(refresh = NA), "May not be NA")
    ui <- shift_ui()
    reference <- historical_reference(1995:2014)
    transform <- monthly_transform("epwshiftr")
    site <- shift_site("SIN", 103.98, 1.37, label = "Singapore")

    climate_text <- capture.output(
        print(climate, width = 72L),
        type = "message"
    )
    expect_true(any(grepl("CMIP6 Climate", climate_text, fixed = TRUE)))
    expect_true(any(grepl("6-node failover", climate_text, fixed = TRUE)))
    expect_true(any(grepl(
        "Table: auto by variable",
        climate_text,
        fixed = TRUE
    )))
    expect_false(any(grepl("https://", climate_text, fixed = TRUE)))
    climate_verbose <- capture.output(
        print(climate, width = 72L, verbose = TRUE, n = 3L),
        type = "message"
    )
    expect_true(any(grepl("https://", climate_verbose, fixed = TRUE)))
    expect_true(any(grepl("3 more rows", climate_verbose, fixed = TRUE)))

    reference_text <- capture.output(
        print(reference, width = 72L),
        type = "message"
    )
    expect_true(any(grepl("reference 1995–2014", reference_text, fixed = TRUE)))
    expect_false(any(grepl("1996, 1997", reference_text, fixed = TRUE)))

    transform_text <- capture.output(
        print(transform, width = 72L),
        type = "message"
    )
    expect_true(any(grepl(
        "Transformation scale: monthly",
        transform_text,
        fixed = TRUE
    )))
    expect_true(any(grepl(
        "Required source frequency: model_future: mon",
        transform_text,
        fixed = TRUE
    )))
    expect_true(any(grepl(
        "Hourly reconstruction",
        transform_text,
        fixed = TRUE
    )))
    expect_false(any(grepl("Profile|Policy|Backend", transform_text)))

    visible <- NULL
    capture.output(
        visible <- withVisible(print(control, width = 72L)),
        type = "message"
    )
    expect_false(visible$visible)
    expect_identical(visible$value, control)

    expect_snapshot(shift_test__print_objects(
        list(climate, control, ui, reference, transform, site),
        width = 72L
    ))
    expect_snapshot(shift_test__print_objects(
        list(climate, control, ui, reference, transform, site),
        width = 100L,
        n = 3L,
        verbose = TRUE
    ))
})

test_that("Shift display paths compact Windows temp roots lexically", {
    root <- "C:\\Users\\runneradmin\\AppData\\Local\\Temp\\Rtmp123"
    path <- paste0(
        "c:/users/runneradmin/AppData/Local/Temp/Rtmp123/",
        "shift-print-output"
    )
    expect_identical(
        shift_print__display_path(path, temp_root = root),
        "<tempdir>/shift-print-output"
    )
})

test_that("Shift plan and stage printers use bounded semantic previews", {
    withr::local_options(cli.num_colors = 1L)
    site <- shift_site("SIN", 103.98, 1.37, label = "Singapore")
    periods <- epw_morph_periods(`2060s` = 2055:2065)
    transform <- monthly_transform("original_morphing")
    climate_spec <- shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585"))
    plan <- shift_plan(
        request = shift_spec__request_from_cmip6(
            climate_spec,
            periods,
            transform
        ),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        reference = historical_reference(1995:2014),
        epw = list(
            dir = "outputs/future-epw",
            separate = TRUE,
            export_dir = file.path(tempdir(), "shift-print-output")
        ),
        store = file.path(tempdir(), "shift-print-store")
    )
    plan@meta$climate <- climate_spec
    tasks <- data.table::data.table(
        status = c("done", "done", "queued", "error"),
        filename = sprintf("tas_%02d.nc", 1:4),
        bytes_done = c(100, 200, 0, 20),
        size = c(100, 200, 300, 400),
        speed_bps = c(10, 20, 0, 0),
        eta_seconds = c(0, 0, 30, NA),
        data_node = "example.org",
        attempts = c(1L, 1L, 0L, 2L),
        last_error = c(NA, NA, NA, "connection failed")
    )
    download <- shift_stage__new(
        ShiftDownload,
        "download",
        ids = list(session_id = "session-print"),
        meta = list(session = tasks)
    )
    coverage <- data.table::data.table(
        complete = c(TRUE, TRUE, FALSE),
        status = c("done", "done", "failed"),
        experiment_id = c("ssp126", "ssp585", "ssp585"),
        variable_id = c("tas", "tas", "hurs"),
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        time_start = as.POSIXct("2055-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2065-12-31", tz = "UTC"),
        output_time_count = c(132L, 132L, 0L),
        output_rows = c(132L, 132L, 0L),
        last_error = c(NA, NA, "missing years")
    )
    climate <- shift_stage__new(
        ShiftClimate,
        "climate",
        meta = list(site = site, periods = periods, coverage = coverage)
    )
    morph_rows <- data.table::data.table(
        case_id = sprintf("case-%d", 1:4),
        source_id = "BCC-CSM2-MR",
        experiment_id = rep(c("ssp126", "ssp585"), each = 2L),
        variant_label = "r1i1p1f1",
        period = "2060s",
        status = "result_done",
        row_count = 8760L,
        output_path = sprintf("morph/case-%d.parquet", 1:4)
    )
    morphed <- shift_stage__new(
        ShiftMorphed,
        "morphed",
        meta = list(
            transform = transform,
            recipe = transform__recipe(transform),
            reference_spec = historical_reference(1995:2014),
            results = morph_rows
        )
    )
    # Use an absolute temporary fixture path so production normalization has
    # the same identity on Windows and POSIX before console-width layout.
    export_dir <- file.path(tempdir(), "exports")
    output_rows <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        experiment_id = rep(c("ssp126", "ssp585"), 6L),
        variant_label = "r1i1p1f1",
        period = "2060s",
        path = sprintf("outputs/future-%02d.epw", 1:12),
        export_path = file.path(export_dir, sprintf("future-%02d.epw", 1:12)),
        created_at = as.POSIXct("2026-01-01", tz = "UTC")
    )
    outputs <- shift_stage__new(
        ShiftOutputs,
        "outputs",
        meta = list(outputs = output_rows, export_dir = export_dir)
    )

    output_text <- capture.output(
        print(outputs, width = 72L, n = 3L),
        type = "message"
    )
    expect_true(any(grepl("9 more rows", output_text, fixed = TRUE)))
    output_default <- capture.output(
        print(outputs, width = 72L),
        type = "message"
    )
    expect_true(any(grepl("2 more rows", output_default, fixed = TRUE)))
    output_all <- capture.output(
        print(outputs, width = 72L, n = Inf),
        type = "message"
    )
    expect_false(any(grepl("more rows", output_all, fixed = TRUE)))

    for (width in c(60L, 80L, 120L)) {
        for (object in list(plan, download, climate, morphed, outputs)) {
            lines <- cli::ansi_strip(capture.output(
                print(object, width = width, n = 3L),
                type = "message"
            ))
            expect_lte(max(cli::ansi_nchar(lines, type = "width")), width)
        }
    }

    expect_snapshot(
        shift_test__print_objects(
            list(plan, download, climate, morphed, outputs),
            width = 80L,
            n = 3L
        ),
        transform = shift_test__normalize_print
    )
    expect_snapshot(
        shift_test__print_objects(
            list(plan, download, climate, morphed, outputs),
            width = 100L,
            n = 3L,
            verbose = TRUE
        ),
        transform = shift_test__normalize_print
    )
})

test_that("ShiftRun print refreshes state and reuses the static dashboard", {
    skip_if_not_installed("duckdb")
    # Select disposable DuckDB storage explicitly so its repeated home-choice
    # advice cannot become part of the dashboard's captured message output.
    withr::local_options(
        cli.num_colors = 1L,
        duckdb.home = withr::local_tempdir()
    )
    store_path <- tempfile("shift-print-run-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
        periods = list(`2060s` = 2055:2065),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(1995:2014),
        dir = tempfile("shift-print-run-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    stale <- shift_run_get(run_id, store = store_path)
    store <- shift_store(plan)
    shift_job__run_update(
        store,
        run_id,
        status = "waiting",
        current_stage = "collect"
    )
    store$close()

    printed <- capture.output(print(stale, width = 72L), type = "message")
    # The persisted state remains `waiting`; the receipt intentionally uses the
    # user-facing READY label to describe an interactive continuation point.
    expect_true(any(grepl("READY", printed, fixed = TRUE)))
    expect_true(any(grepl("Future EPW", printed, fixed = TRUE)))
    expect_true(any(grepl("Cases", printed, fixed = TRUE)))
    expect_false(any(grepl("MemberNA|StatusNA|plannedNA", printed)))
    expect_lte(
        max(cli::ansi_nchar(cli::ansi_strip(printed), type = "width")),
        72L
    )

    view <- shift_ui_view__ui_run_view(
        shift_refresh(stale),
        width = 72L,
        detail = "normal",
        motion = "none"
    )
    direct <- capture.output(
        shift_ui_view__ui_print_view(view, include_tables = TRUE),
        type = "message"
    )
    expect_identical(printed, direct)
})

test_that("ShiftRun print falls back to a cached static snapshot", {
    withr::local_options(cli.num_colors = 1L)
    spec <- list(
        task = "future_epw",
        climate = list(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585"),
            member = NULL,
            grid = NULL
        ),
        periods = list(`2060s` = 2055:2065),
        transform = list(
            scale = "monthly",
            method = "original_morphing",
            recipe = "original_morphing_monthly",
            recipe_version = 1L,
            reconstruction = "original_morphing_field_equations",
            options = list()
        ),
        reference = list(
            mode = "historical",
            periods = list(reference = 1995:2014)
        ),
        control = list(download = "auto")
    )
    # The cached preview must render the same real path across platforms.
    export_dir <- file.path(tempdir(), "exports")
    run_row <- data.table::data.table(
        run_id = "run_print_12345678",
        task = "future_epw",
        spec_json = jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"),
        status = "completed",
        current_stage = "write_epw",
        output_dir = export_dir,
        started_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        updated_at = as.POSIXct("2026-01-01 00:00:05", tz = "UTC"),
        completed_at = as.POSIXct("2026-01-01 00:00:05", tz = "UTC"),
        last_error = NA_character_
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        variant_label = NA_character_,
        period = "2060s",
        status = "completed",
        required = TRUE,
        export_path = file.path(export_dir, c("ssp126.epw", "ssp585.epw"))
    )
    events <- data.table::data.table(
        stage = character(),
        status = character(),
        message = character(),
        details_json = character(),
        created_at = as.POSIXct(character(), tz = "UTC")
    )
    run <- shift_stage__new(
        ShiftRun,
        "run",
        ids = list(run_id = "run_print_12345678"),
        meta = list(run = run_row, cases = cases, events = events)
    )

    printed <- capture.output(print(run, width = 72L), type = "message")
    expect_true(any(grepl("COMPLETED", printed, fixed = TRUE)))
    expect_true(any(grepl(
        "Persisted preview unavailable",
        printed,
        fixed = TRUE
    )))
    expect_false(any(grepl("MemberNA|StatusNA|completedNA", printed)))
    limited <- capture.output(print(run, width = 72L, n = 1L), type = "message")
    expect_true(any(grepl("1 more case", limited, fixed = TRUE)))
    expect_snapshot(
        print(run, width = 72L),
        transform = shift_test__normalize_print
    )
    expect_snapshot(
        print(run, width = 100L, verbose = TRUE),
        transform = shift_test__normalize_print
    )
})

test_that("shift_ui() validates presentation options without changing scientific intent", {
    expect_true(S7::S7_inherits(shift_ui(), ShiftUiOptions))
    expect_equal(
        shift_ui("log", detail = "detail", heartbeat = 2)@progress,
        "log"
    )
    expect_equal(shift_ui(detail = "debug")@detail, "debug")
    expect_equal(shift_ui(motion = "reduced")@motion, "reduced")
    expect_equal(shift_ui(refresh = 0.2)@refresh, 0.2)
    expect_error(shift_ui("invalid"), "arg")
    expect_error(shift_ui(motion = "invalid"), "arg")
    expect_error(shift_ui(refresh = 0.01), "not >= 0.05")
    expect_error(shift_ui(heartbeat = -1), "not >= 0")
    expect_equal(shift_ui__ui_mode(shift_ui("none")), "none")
    expect_equal(
        shift_ui__ui_motion(shift_ui("dynamic", motion = "auto")),
        "full"
    )
    expect_equal(shift_ui__ui_motion(shift_ui("log", motion = "full")), "none")

    store <- tempfile("shift-ui-store-")
    output <- tempfile("shift-ui-output-")
    transform <- monthly_transform("epwshiftr")
    climate <- shift_cmip6(
        model = "EC-Earth3",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        frequency = "mon",
        table = "Amon"
    )
    make_plan <- function(ui) {
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            climate = climate,
            periods = list(`2060s` = 2060L),
            transform = transform,
            dir = output,
            store = store,
            dry_run = TRUE,
            ui = ui
        )@meta$children[[1L]]
    }
    expect_identical(
        shift_persist__plan_spec(make_plan(shift_ui("none"))),
        shift_persist__plan_spec(make_plan(shift_ui(
            "log",
            detail = "debug",
            motion = "full",
            refresh = 0.05,
            heartbeat = 1
        )))
    )
})

test_that("ShiftReporter persists structured milestones while none mode stays silent", {
    skip_if_not_installed("duckdb")

    store <- EsgStore$new(tempfile("shift-reporter-store-"))
    on.exit(store$close(), add = TRUE)
    reporter <- shift_reporter__reporter(
        shift_ui("none"),
        store = store,
        run_id = "reporter-run"
    )
    expect_silent({
        reporter$stage_started("resolve", "Resolving inputs.")
        reporter$unit_started(
            "Querying future catalog.",
            current = 1L,
            total = 2L,
            details = list(
                unit_type = "query",
                node = "example.org",
                scenario = "ssp585"
            )
        )
        reporter$unit_completed(
            "Future catalog resolved.",
            current = 1L,
            total = 2L,
            details = list(variable = "tas", access_method = "OPeNDAP")
        )
        reporter$stage_completed("Inputs resolved.")
    })
    events <- morpher__private_store(store)$read_table("shift_run_event")
    expect_equal(nrow(events), 4L)
    details <- lapply(
        events$details_json,
        jsonlite::fromJSON,
        simplifyVector = TRUE
    )
    unit <- details[[which(events$message == "Future catalog resolved.")]]
    expect_equal(unit$stage, "resolve")
    expect_equal(unit$unit_type, "query")
    expect_equal(unit$node, "example.org")
    expect_equal(unit$variable, "tas")
    expect_equal(unit$outcome, "completed")
})

test_that("ShiftReporter submits one complete frame per dynamic refresh", {
    frames <- list()
    compacts <- character()
    closes <- 0L
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = function(...) {
            list(
                draw = function(lines, compact = NULL) {
                    frames[[length(frames) + 1L]] <<- lines
                    compacts <<- c(compacts, compact)
                    TRUE
                },
                suspend = function(code) code(),
                close = function(...) {
                    closes <<- closes + 1L
                    invisible(NULL)
                }
            )
        },
        .package = "epwshiftr"
    )

    reporter <- shift_reporter__reporter(shift_ui("dynamic", heartbeat = 0))
    reporter$stage_started("resolve", "Resolving inputs.")
    reporter$unit_started("Trying node one", current = 1L, total = 2L)
    expect_silent(reporter$heartbeat(
        "Waiting for future catalog",
        force = TRUE
    ))
    expect_equal(length(frames), 3L)
    expect_true(all(lengths(frames) == 10L))
    expect_length(compacts, 3L)
    expect_true(all(nzchar(compacts)))
    expect_message(
        reporter$unit_completed(
            "Node one failed",
            current = 1L,
            total = 2L,
            outcome = "failed"
        ),
        "Node one failed"
    )
    reporter$close()
    expect_equal(closes, 1L)
})

test_that("ShiftReporter falls back to logs when frame painting fails", {
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = function(...) {
            list(
                draw = function(...) FALSE,
                suspend = function(code) code(),
                close = function(...) invisible(NULL)
            )
        },
        .package = "epwshiftr"
    )
    reporter <- shift_reporter__reporter(shift_ui("dynamic"))

    expect_message(
        reporter$stage_started("resolve", "Resolving inputs."),
        "switched to line-by-line logs"
    )
    expect_identical(reporter$mode(), "log")
    expect_message(
        reporter$unit_started("Trying DKRZ", current = 1L, total = 6L),
        "Trying DKRZ"
    )
})

# vim: fdm=marker :
