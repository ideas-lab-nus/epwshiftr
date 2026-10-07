# Plan against local availability so these tests never contact ESGF.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Construct an explicit one-model plan with automatic member choice.
# selection_test__plan {{{
selection_test__plan <- function(member = NULL, reference = TRUE) {
    transform <- monthly_transform("epwshiftr")
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = member,
        frequency = "mon",
        table = "Amon",
        index_nodes = "https://example.org"
    )
    periods <- epw_morph_periods(future = 2060L)
    plan <- shift_plan(
        shift_spec__request_from_cmip6(climate, periods, transform),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        reference = if (reference) historical_reference(1991L) else NULL,
        store = tempfile("selection-store-")
    )
    plan@meta$climate <- climate
    plan
}
# }}}

# Catalog alternatives distinguish coverage loss, historical pairing, and an
# eligible non-preferred member without changing the scientific selector.
# selection_test__catalog {{{
selection_test__catalog <- function(plan, reference = FALSE) {
    variables <- epw_morph_variables(plan@meta$recipe)
    members <- if (reference) {
        c("r1i1p1f1", "r2i1p1f1", "r3i1p1f1")
    } else {
        paste0("r", 1:4, "i1p1f1")
    }
    rows <- lapply(members, function(member) {
        wanted <- if (!reference && member == "r3i1p1f1") {
            setdiff(variables, "tas")
        } else {
            variables
        }
        data.table::rbindlist(
            lapply(wanted, function(variable) {
                docs <- esgf_test__file_docs(
                    paste(member, variable, "nc", sep = "."),
                    variable_id = variable,
                    datetime_start = if (reference) {
                        "1991-01-16T12:00:00Z"
                    } else {
                        "2060-01-16T12:00:00Z"
                    },
                    datetime_end = if (reference) {
                        "1991-12-16T12:00:00Z"
                    } else {
                        "2060-12-16T12:00:00Z"
                    }
                )
                docs$variant_label <- member
                docs$experiment_id <- if (reference) "historical" else "ssp585"
                docs$activity_id <- if (reference) "CMIP" else "ScenarioMIP"
                docs$frequency <- "mon"
                docs$table_id <- "Amon"
                docs$grid_label <- "gn"
                docs$id <- paste(
                    docs$experiment_id,
                    member,
                    variable,
                    sep = "-"
                )
                # Unique logical files keep different members and periods
                # from being interpreted as replicas of the same input.
                docs$tracking_id <- paste0("hdl:synthetic/", docs$id)
                docs$dataset_id <- paste0("dataset-", docs$id)
                docs$instance_id <- paste0(docs$id, ".v20260101")
                docs$master_id <- docs$id
                docs
            }),
            fill = TRUE
        )
    })
    data.table::rbindlist(rows, fill = TRUE)
}
# }}}

test_that("selection evidence captures actual eligibility and explicit constraints", {
    plan <- selection_test__plan()
    future <- selection_test__catalog(plan)
    reference <- selection_test__catalog(plan, TRUE)
    saved <- NULL
    selected <- shift_resolve__resolve_cmip6_selection(
        plan,
        future,
        reference,
        record = function(value) saved <<- value
    )
    expect_identical(selected$variant_label, "r1i1p1f1")
    reason <- stats::setNames(saved$future$reason, saved$future$variant_label)
    expect_identical(
        unname(reason[paste0("r", 1:4, "i1p1f1")]),
        c(
            "selected",
            "eligible_alternative_not_selected",
            "future_coverage_incomplete",
            "no_complete_matching_reference"
        )
    )
    expect_match(
        saved$future$missing[saved$future$variant_label == "r3i1p1f1"],
        "tas"
    )
    expect_true(saved$reference_required)
    expect_length(saved$models_without_candidates, 0L)
    expect_false("reason" %in% names(future))

    explicit <- selection_test__plan("r2i1p1f1")
    selected <- shift_resolve__resolve_cmip6_selection(
        explicit,
        future,
        reference,
        record = function(value) saved <<- value
    )
    expect_identical(selected$variant_label, "r2i1p1f1")
    expect_true(all(
        saved$future$reason[saved$future$variant_label != "r2i1p1f1"] ==
            "user_member_constraint"
    ))
    expect_error(
        shift_resolve__resolve_cmip6_selection(
            plan,
            future,
            reference[0L],
            record = function(value) saved <<- value
        ),
        "Historical reference catalog is empty"
    )
    expect_equal(nrow(saved$future), 4L)
    expect_equal(nrow(saved$reference), 0L)
    expect_false(any(saved$future$selected))
})

test_that("public selection reads durable records and marks legacy absence", {
    plan <- selection_test__plan()
    saved <- NULL
    shift_resolve__resolve_cmip6_selection(
        plan,
        selection_test__catalog(plan),
        selection_test__catalog(plan, TRUE),
        record = function(value) saved <<- value
    )
    saved$phase <- "catalog"
    record <- list(
        schema_version = 1L,
        index_node = "https://example.org",
        outcome = "resolved",
        checks = list(saved),
        error = NULL
    )
    expect_identical(shift_selection(plan)$state, "planned")
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    expect_error(
        shift_selection(shift_run_get(run_id, store)),
        "after the run finishes"
    )
    shift_job__run_update(store, run_id, status = "failed")
    legacy <- shift_selection(shift_run_get(run_id, store))
    expect_identical(legacy$state, "not_recorded")
    expect_length(legacy$attempts, 0L)
    shift_selection__persist(store, run_id, record)
    shift_selection__persist(store, run_id, record)
    store$close()
    result <- shift_selection(shift_run_get(run_id, plan@store_path))
    expect_identical(result$state, "recorded")
    expect_length(result$attempts, 1L)
    expect_length(result$attempts[[1L]]$checks, 1L)
    expect_s3_class(result$attempts[[1L]]$checks[[1L]]$future, "data.table")
    expect_identical(result$requested$climate$model, "EC-Earth3")
    expect_identical(result$requested$periods$future, 2060L)
    expect_equal(
        result$attempts[[1L]]$checks[[1L]]$future$reason,
        saved$future$reason
    )
})

test_that("shared selection records persist without changing workflow intent", {
    plan <- selection_test__plan()
    original <- shift_persist__plan_spec(plan)
    expect_null(original$stages$shared_inputs)
    plan@meta$shared_inputs <- list(index_node = "https://example.org")
    before <- shift_persist__plan_spec(plan)
    saved <- NULL
    shift_resolve__resolve_cmip6_selection(
        plan,
        selection_test__catalog(plan),
        selection_test__catalog(plan, TRUE),
        record = function(value) saved <<- value
    )
    record <- list(
        schema_version = 1L,
        index_node = "https://example.org",
        outcome = "failed",
        checks = list(saved),
        error = "Service unavailable"
    )
    plan@meta$shared_inputs$selection_records <- list(shift_persist__spec_json(
        record
    ))
    # Recreate the receipt shape used when a later session starts a batch child.
    plan@meta$shared_inputs <- jsonlite::fromJSON(
        shift_persist__spec_json(plan@meta$shared_inputs),
        simplifyVector = TRUE
    )
    expect_identical(shift_persist__plan_spec(plan), before)
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    shift_job__run_update(store, run_id, status = "failed")
    read <- shift_selection(shift_run_get(run_id, store))
    expect_identical(read$attempts[[1L]]$error, "Service unavailable")
    expect_length(read$attempts[[1L]]$checks, 1L)
    expect_equal(
        read$attempts[[1L]]$checks[[1L]]$future$reason,
        saved$future$reason
    )
    expect_equal(nrow(read$attempts[[1L]]$checks[[1L]]$reference), 3L)
})

# Only network responses and the post-selection extraction stage are replaced.
# The public workflow executes real catalog storage, candidate resolution and
# durable run inspection on both success and service-failure paths.
test_that("public workflow keeps pre-service candidates and service outcomes", {
    plan <- selection_test__plan()
    docs <- data.table::rbindlist(
        list(
            selection_test__catalog(plan),
            selection_test__catalog(plan, TRUE)
        ),
        fill = TRUE
    )
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect_filtered(docs, calls)
    service_failed <- FALSE
    testthat::local_mocked_bindings(
        query_result__resolve_file_services = function(value, ...) {
            if (service_failed) {
                stop("Synthetic service access failure")
            }
            list(
                result = value,
                diagnostics = data.table::data.table(
                    service = "OPENDAP",
                    selected = TRUE
                )
            )
        },
        shift_extract = function(...) stop("Synthetic stop after selection"),
        .package = "epwshiftr"
    )
    error <- tryCatch(shift_run(plan, ui = shift_ui("none")), error = identity)
    expect_match(conditionMessage(error), "Synthetic stop after selection")
    read <- shift_selection(shift_run_get(error$run_id, plan@store_path))
    expect_identical(read$state, "recorded")
    expect_identical(read$attempts[[1L]]$outcome, "resolved")
    checks <- read$attempts[[1L]]$checks
    expect_identical(
        vapply(checks, `[[`, character(1L), "phase"),
        c("catalog", "services")
    )
    expect_equal(nrow(checks[[1L]]$future), 4L)
    expect_equal(nrow(checks[[2L]]$future), 1L)
    expect_true(nzchar(checks[[1L]]$query_ids$future))
    expect_false(identical(
        checks[[1L]]$query_ids$future,
        checks[[2L]]$query_ids$future
    ))

    service_failed <- TRUE
    failed_plan <- selection_test__plan()
    error <- tryCatch(
        shift_run(failed_plan, ui = shift_ui("none")),
        error = identity
    )
    read <- shift_selection(shift_run_get(error$run_id, failed_plan@store_path))
    expect_identical(read$attempts[[1L]]$outcome, "failed")
    expect_identical(read$attempts[[1L]]$failure_phase, "services")
    expect_match(read$attempts[[1L]]$error, "Synthetic service access failure")
    expect_length(read$attempts[[1L]]$checks, 1L)
    expect_true(any(read$attempts[[1L]]$checks[[1L]]$future$selected))
})

# vim: fdm=marker :
