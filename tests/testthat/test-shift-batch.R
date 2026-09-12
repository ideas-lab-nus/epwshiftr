test_that("method keys resolve through the transform registry", {
    transforms <- shift_batch__transforms(c(
        "original_morphing",
        "bws_btws",
        "isimip3basd"
    ))

    expect_identical(
        unname(vapply(
            transforms,
            function(value) value@method,
            character(1L)
        )),
        c("original_morphing", "bws_btws", "isimip3basd")
    )
    expect_error(
        shift_batch__transforms("epwshiftr"),
        "more than one configuration"
    )
    expect_error(
        shift_batch__transforms(character()),
        "unique, non-empty method keys"
    )
})

test_that("high-level workflows select common models and retain child plans", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability
    ))
    output <- tempfile("batch-output-")
    store <- tempfile("batch-store-")

    batch <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 1995:2014),
        dir = output,
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )

    expect_true(S7::S7_inherits(batch, ShiftBatch))
    expect_identical(shift_status(batch), "planned")
    expect_length(batch@meta$children, 6L)
    expect_identical(
        batch@meta$selected_models$source_id,
        c("Model-A", "Model-B")
    )
    expect_true(all(batch@meta$selected_models$variant_label == "r1i1p1f1"))
    expect_equal(nrow(shift_cases(batch)), 24L)
    expect_true(all(vapply(
        batch@meta$children,
        function(child) S7::S7_inherits(child, ShiftPlan),
        logical(1L)
    )))
    manifest <- batch@meta$manifest
    expect_true(all(manifest[method == "isimip3basd", calibration_used]))
    expect_false(any(manifest[method != "isimip3basd", calibration_used]))
    expect_true(all(file.path(shift_batch__store_root(store), "batches") ==
        dirname(dirname(manifest$store))))
    expect_silent(shift_logs(batch))

    # Delivery paths do not change the scientific batch identity or reusable
    # store root selected for the same EPW, climate, periods, and methods.
    second <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 1995:2014),
        dir = tempfile("another-batch-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    expect_identical(second@ids$batch_id, batch@ids$batch_id)

    changed_calibration <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 2000:2019),
        dir = tempfile("another-batch-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    expect_false(identical(
        changed_calibration@ids$batch_id,
        batch@ids$batch_id
    ))
})

test_that("batch discovery keeps r1i1p1f1 as a hard default", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability
    ))
    climate <- shift_cmip6(
        model = 1L,
        scenarios = "ssp585",
        member = "r2i1p1f1"
    )

    expect_error(
        shift_batch__discover_models(
            climate,
            shift_batch__transforms("isimip3basd"),
            tempfile("discovery-"),
            shift_ui(progress = "none")
        ),
        "requires member `r1i1p1f1`"
    )
})

test_that("batch discovery applies historical coverage per method", {
    calls <- list()
    collect <- function(
        variables,
        scenarios,
        include_historical,
        member,
        frequency,
        index_node,
        ...
    ) {
        calls[[length(calls) + 1L]] <<- list(
            variables = variables,
            include_historical = include_historical
        )
        test_cmip6_availability(
            variables = variables,
            scenarios = scenarios,
            member = member,
            frequency = frequency,
            index_node = index_node
        )
    }
    withr::local_options(list(epwshiftr.cmip6.availability = collect))
    transforms <- list(
        monthly = monthly_transform("epwshiftr"),
        daily = daily_transform("isimip3basd")
    )

    shift_batch__discover_models(
        shift_cmip6(model = 1L, scenarios = "ssp585"),
        transforms,
        tempfile("discovery-"),
        shift_ui(progress = "none")
    )

    monthly_calls <- Filter(function(value) {
        length(value$variables) > 1L
    }, calls)
    daily_calls <- Filter(function(value) {
        identical(value$variables, "tas")
    }, calls)
    expect_true(length(monthly_calls) > 0L)
    expect_true(all(!vapply(
        monthly_calls,
        `[[`,
        logical(1L),
        "include_historical"
    )))
    expect_true(length(daily_calls) > 0L)
    expect_true(all(vapply(
        daily_calls,
        `[[`,
        logical(1L),
        "include_historical"
    )))
})

test_that("NULL model selection retains every compatible common model", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability
    ))
    selection <- shift_batch__discover_models(
        shift_cmip6(model = NULL, scenarios = "ssp585"),
        shift_batch__transforms("isimip3basd"),
        tempfile("discovery-"),
        shift_ui(progress = "none")
    )

    expect_identical(
        selection$identities$source_id,
        c("Model-A", "Model-B", "Model-C")
    )
})

test_that("single explicit transforms preserve the established plan API", {
    climate <- shift_cmip6(
        model = "Model-A",
        scenarios = "ssp585"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2050` = 2049:2050),
        transform = daily_transform("isimip3basd"),
        observed_reference = shift_reference_plan(
            "observed-plan",
            shift__periods_from_years(1995:2014),
            role = "observed_reference"
        ),
        reference = historical_reference(1995:2014),
        dir = tempfile("single-output-"),
        store = tempfile("single-store-"),
        dry_run = TRUE
    )

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_identical(unique(plan@meta$request@meta$frequency), "day")
})
