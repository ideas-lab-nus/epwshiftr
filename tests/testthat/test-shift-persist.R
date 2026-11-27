# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("an identical complete workflow returns its original durable run", {
    store_path <- tempfile("shift-idempotent-run-store-")
    output_dir <- tempfile("shift-idempotent-run-output-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = output_dir,
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_inspect__store(plan)
    private <- morpher__private_store(store)
    cases <- private$read_table("shift_run_case")
    cases <- cases[cases[["run_id"]] == run_id]

    canonical <- file.path(store_path, "outputs", "future.epw")
    exported <- file.path(output_dir, "future.epw")
    dir.create(dirname(canonical), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(exported), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.create(canonical))
    expect_true(file.create(exported))
    output_id <- "output-idempotent"
    morph_id <- "morph-idempotent"
    cases[, `:=`(
        status = "completed",
        output_id = "output-idempotent",
        export_path = exported,
        missing_reason = NA_character_
    )]
    shift_job__run_cases_write(store, run_id, cases)
    private$append_new_rows(
        "epw_output",
        data.frame(
            output_id = output_id,
            morph_id = morph_id,
            case_id = cases$case_id[[1L]],
            result_id = "result-idempotent",
            artifact_id = NA_character_,
            path = store_rel_path(canonical, root = store_path),
            source_id = cases$source_id[[1L]],
            experiment_id = cases$experiment_id[[1L]],
            variant_label = cases$variant_label[[1L]],
            period = cases$period[[1L]],
            output_type = "representative_year",
            sequence_id = NA_character_,
            weather_year = NA_integer_,
            calendar = "365_day",
            stochastic_seed = NA_integer_,
            member_count = 1L,
            provenance_json = "[]",
            created_at = store__now(),
            stringsAsFactors = FALSE
        ),
        "output_id"
    )
    shift_job__run_update(
        store,
        run_id,
        status = "completed",
        current_stage = "completed",
        morph_id = morph_id,
        completed_at = store__now()
    )
    stored_run <- private$read_table("shift_run")
    stored_run <- stored_run[stored_run[["run_id"]] == run_id]
    stored_cases <- private$read_table("shift_run_case")
    stored_cases <- stored_cases[stored_cases[["run_id"]] == run_id]
    stored_outputs <- private$read_table("epw_output")
    expect_identical(stored_run$status[[1L]], "completed")
    expect_true(all(stored_cases$status == "completed"))
    expect_true(all(file.exists(stored_cases$export_path)))
    expect_identical(stored_run$morph_id[[1L]], morph_id)
    expect_true(output_id %in% stored_outputs$output_id)
    expect_true(file.exists(store_abs_path(
        stored_outputs$path[[1L]],
        root = store_path
    )))
    expect_true(shift_run__run_artifacts_complete(store, run_id))
    expected_hash <- store__hash(shift_persist__spec_json(shift_persist__plan_spec(
        plan
    )))
    expect_identical(
        stored_run[["spec_hash"]][[1L]],
        expected_hash
    )
    store$close()
    before <- file.info(c(canonical, exported))[, "mtime", drop = TRUE]
    checksums <- unname(tools::md5sum(c(canonical, exported)))

    # A receipt produced before mandatory-fragment validation cannot certify
    # the corrected workflow merely because its EPW files still exist.
    current_spec <- shift_persist__plan_spec(plan)
    expect_identical(current_spec$coverage_version, 2L)
    earlier_spec <- current_spec
    earlier_spec$coverage_version <- NULL
    earlier_json <- shift_persist__spec_json(earlier_spec)
    earlier_hash <- store__hash(earlier_json)
    expect_false(identical(earlier_hash, expected_hash))
    store <- shift_inspect__store(plan)
    shift_job__run_update(
        store,
        run_id,
        spec_json = earlier_json,
        spec_hash = earlier_hash
    )
    store$close()
    expect_null(shift_run__run_existing(plan))
    store <- shift_inspect__store(plan)
    shift_job__run_update(
        store,
        run_id,
        spec_json = shift_persist__spec_json(current_spec),
        spec_hash = expected_hash
    )
    store$close()

    existing <- shift_run__run_existing(plan)
    expect_s7_class(existing, ShiftRun)
    expect_identical(existing@ids$run_id, run_id)
    reused <- shift_run(plan, ui = shift_ui("none"))

    expect_s7_class(reused, ShiftRun)
    expect_identical(reused@ids$run_id, run_id)
    expect_identical(shift_status(reused, refresh = FALSE), "completed")
    expect_identical(
        file.info(c(canonical, exported))[, "mtime", drop = TRUE],
        before
    )
    expect_identical(
        unname(tools::md5sum(c(canonical, exported))),
        checksums
    )
    reopened <- shift_inspect__store(plan)
    on.exit(reopened$close(), add = TRUE)
    runs <- morpher__private_store(reopened)$read_table("shift_run")
    expect_equal(
        nrow(runs[
            runs[["spec_hash"]] ==
                runs[runs[["run_id"]] == run_id][["spec_hash"]][[1L]]
        ]),
        1L
    )
})

test_that("an identical interrupted workflow resumes its original run ID", {
    store_path <- tempfile("shift-idempotent-resume-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-idempotent-resume-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_inspect__store(plan)
    shift_job__run_update(
        store,
        run_id,
        status = "failed",
        current_stage = "resolve",
        last_error = "interrupted fixture"
    )
    store$close()

    resumed_ids <- character()
    testthat::local_mocked_bindings(
        shift_job__resume_one = function(x, background, ui, execution = NULL) {
            resumed_ids <<- c(resumed_ids, x@ids$run_id)
            x
        },
        .package = "epwshiftr"
    )
    resumed <- shift_run(plan, ui = shift_ui("none"))

    expect_identical(resumed@ids$run_id, run_id)
    expect_identical(resumed_ids, run_id)
    reopened <- shift_inspect__store(plan)
    on.exit(reopened$close(), add = TRUE)
    runs <- morpher__private_store(reopened)$read_table("shift_run")
    expect_equal(nrow(runs), 1L)
})

test_that("delivery and store paths may be distinct siblings", {
    root <- withr::local_tempdir()
    paths <- shift_path__validate_delivery_store_paths(
        file.path(root, "delivery"),
        file.path(root, "store")
    )

    expect_named(paths, c("dir", "store"))
    expect_false(identical(paths$dir, paths$store))

    # A lexical prefix alone is not an ancestor relationship.
    expect_no_error(shift_path__validate_delivery_store_paths(
        file.path(root, "output"),
        file.path(root, "output-cache")
    ))
})

test_that("equal delivery and store paths are rejected canonically", {
    root <- withr::local_tempdir()
    output <- file.path(root, "output")

    expect_error(
        shift_path__validate_delivery_store_paths(
            output,
            file.path(output, ".")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift_path__validate_delivery_store_paths(
            output,
            file.path(root, "missing", "..", "output")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift_path__validate_delivery_store_paths(output, output),
        "dir.*receives exported EPW files only"
    )
})

test_that("delivery and store paths cannot contain one another", {
    root <- withr::local_tempdir()

    # Neither descendant needs to exist for the relationship to be detected.
    expect_error(
        shift_path__validate_delivery_store_paths(
            file.path(root, "delivery"),
            file.path(root, "delivery", "internal", "store")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift_path__validate_delivery_store_paths(
            file.path(root, "store", "exports"),
            file.path(root, "store")
        ),
        "omit.*store.*epwshiftr.dir_store"
    )
})

test_that("symbolic-link aliases cannot bypass path isolation", {
    skip_on_os("windows")
    root <- withr::local_tempdir()
    real <- file.path(root, "real")
    link <- file.path(root, "alias")
    dir.create(real)
    if (!isTRUE(file.symlink(real, link))) {
        skip("Symbolic links are unavailable on this filesystem")
    }

    expect_error(
        shift_path__validate_delivery_store_paths(
            file.path(real, "delivery"),
            file.path(link, "delivery", "store")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
})

test_that("path comparison follows case-insensitive filesystem semantics", {
    root <- withr::local_tempdir()
    output <- file.path(root, "delivery")
    case_variant <- file.path(root, "DELIVERY")

    if (isTRUE(shift_path__workflow_path_case_sensitive(root))) {
        expect_no_error(shift_path__validate_delivery_store_paths(
            output,
            case_variant
        ))
    } else {
        expect_error(
            shift_path__validate_delivery_store_paths(output, case_variant),
            "separate, non-overlapping directories",
            fixed = TRUE
        )
    }
})

test_that("shift_epw_future rejects an overlapping delivery and store tree", {
    root <- withr::local_tempdir()
    common <- file.path(root, "workflow")
    make_plan <- function(dir, store) {
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            climate = shift_cmip6("BCC-CSM2-MR", "ssp126"),
            periods = list(`2060s` = 2060L),
            transform = monthly_transform("epwshiftr"),
            dir = dir,
            store = store,
            dry_run = TRUE
        )
    }

    expect_error(
        make_plan(common, common),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        make_plan(file.path(common, "delivery"), common),
        "dir.*receives exported EPW files only"
    )
    expect_s7_class(
        make_plan(file.path(root, "delivery"), file.path(root, "store")),
        ShiftBatch
    )
})

# vim: fdm=marker :
