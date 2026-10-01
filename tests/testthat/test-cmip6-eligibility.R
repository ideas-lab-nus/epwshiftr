# Resolve public transform constructors only in this test convenience helper.
eligibility_test__evaluate <- function(
    catalog,
    methods = NULL,
    transform = NULL,
    scenarios = c("ssp245", "ssp585"),
    common = FALSE,
    include_optional_historical = FALSE
) {
    requirements <- eligibility__requirements(
        shift_batch__transforms(methods, transform),
        scenarios,
        include_optional_historical
    )
    eligibility__evaluate(eligibility__catalog(catalog), requirements, common)
}

# Build self-contained Dataset rows without querying any external service.
eligibility_test__catalog <- function(
    variables = "tas",
    source = "Model-A",
    experiments = c("historical", "ssp245"),
    frequency = "day",
    table = "day",
    member = "r1i1p1f1",
    grid = "gn"
) {
    data.table::CJ(
        source_id = source,
        experiment_id = experiments,
        member_id = member,
        grid_label = grid,
        variable_id = variables,
        frequency = frequency,
        table_id = table,
        sorted = FALSE
    )
}

test_that("method eligibility expands candidates without a universal variable list", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(source = "Temperature-only"),
        eligibility_test__catalog(
            c("tas", "tasmin", "tasmax", "huss", "ps"),
            source = "Full-inputs"
        )
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )

    expect_equal(nrow(result$matrix), 4L)
    expect_equal(sum(result$matrix$selected), 3L)
    incomplete <- subset(
        result$matrix,
        source_id == "Temperature-only" & method == "sobie_curry"
    )
    expect_false(incomplete$selected)
    expect_match(incomplete$missing, "historical:huss")
    expect_match(incomplete$missing, "ssp245:ps")
    expect_identical(unique(result$matrix$period_coverage), "not_checked")
    expect_identical(unique(result$matrix$readability), "not_checked")
    expect_identical(unique(result$matrix$quality), "not_checked")
    common <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245",
        common = TRUE
    )
    expect_equal(sum(common$matrix$selected), 2L)
    expect_identical(
        unique(common$matrix$source_id[common$matrix$selected]),
        "Full-inputs"
    )
    expect_identical(
        result$matrix$method_eligible,
        common$matrix$method_eligible
    )
    # A different pool policy must not change matching evidence or input paths.
    expect_identical(result$requirements, common$requirements)
    expect_identical(
        result$matrix$catalog_eligible,
        common$matrix$catalog_eligible
    )
    expect_identical(result$matrix$path_id, common$matrix$path_id)
})

test_that("required historical roles remain required", {
    result <- eligibility_test__evaluate(
        eligibility_test__catalog(experiments = "ssp245"),
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(result$matrix), 1L)
    expect_false(result$matrix$selected)
    expect_match(result$matrix$missing, "historical:tas")
})

test_that("alternative input paths preserve optional historical policy", {
    transform <- monthly_transform("epwshiftr")
    # Use the published registered relative-humidity alternative, not a
    # hand-maintained list that can drift from the weather method contract.
    alternatives <- transform@required_inputs$model_future@variable_sets
    alternative <- which(vapply(
        alternatives,
        function(variables) {
            "hurs" %in% variables && !"huss" %in% variables
        },
        logical(1L)
    ))
    variables <- alternatives[[alternative]]
    catalog <- eligibility_test__catalog(
        variables,
        experiments = "ssp245",
        frequency = "mon",
        table = "Amon"
    )
    result <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )

    expect_true(result$matrix$selected)
    chosen <- subset(result$requirements, path_id == result$matrix$path_id)
    expect_identical(chosen$alternative, as.integer(alternative))
    expect_identical(chosen$variables[[1L]], variables)
    expect_identical(unique(result$requirements$role), "model_future")
    expect_true(any(!result$requirements$complete))
    optional <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(optional$matrix$selected)
    expect_match(optional$matrix$missing, "historical:")
    expect_true("model_historical" %in% optional$requirements$role)
})

test_that("joint alternatives cannot switch across scenarios or shared roles", {
    transform <- monthly_transform("epwshiftr")
    alternatives <- transform@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(
            alternatives[[1L]],
            experiments = "ssp245",
            frequency = "mon",
            table = "Amon"
        ),
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "ssp585",
            frequency = "mon",
            table = "Amon"
        )
    ))
    result <- eligibility_test__evaluate(catalog, transform = transform)
    expect_false(any(result$matrix$selected))
    expect_true(any(result$matrix$catalog_eligible))
    expect_match(
        result$matrix$missing[result$matrix$catalog_eligible],
        "No single input path"
    )
})

test_that("source members and grids cannot be stitched together", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(experiments = "ssp245", grid = "gn"),
        eligibility_test__catalog(experiments = "historical", grid = "gr"),
        eligibility_test__catalog(source = "Model-B", experiments = "ssp245"),
        eligibility_test__catalog(
            source = "Model-B",
            experiments = "historical",
            member = "r2i1p1f1"
        )
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(result$matrix), 4L)
    expect_false(any(result$matrix$selected))
    expect_true(all(!is.na(result$matrix$missing)))
})

test_that("frequency and table partitions cannot be stitched across experiments", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(experiments = "ssp245", table = "day"),
        eligibility_test__catalog(experiments = "historical", table = "Eday"),
        eligibility_test__catalog(source = "Wrong-frequency", frequency = "mon")
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_false(any(result$matrix$selected))
    valid_frequency <- subset(result$requirements, source_id == "Model-A")
    expect_identical(unique(unlist(valid_frequency$table)), "day")
    expect_match(result$matrix$missing[[1L]], "historical:tas")
    expect_true(all(
        !subset(result$requirements, source_id == "Wrong-frequency")$complete
    ))
})

test_that("mixed frequencies and calendar requirements come from registered roles", {
    transform <- hourly_transform("kernel_qdm")
    requirement <- transform@required_inputs$model_future
    variables <- requirement@variable_sets[[1L]]
    catalog <- data.table::rbindlist(lapply(variables, function(variable) {
        frequency <- requirement@variable_frequencies[[variable]][[1L]]
        eligibility_test__catalog(
            variable,
            frequency = frequency,
            table = if (frequency == "day") "day" else "3hr"
        )
    }))
    result <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )
    expect_true(result$matrix$selected)
    detail <- subset(result$requirements, role == "model_future")
    expect_identical(
        unname(detail$frequency_spec[[1L]]),
        unname(vapply(
            variables,
            function(variable) {
                requirement@variable_frequencies[[variable]][[1L]]
            },
            character(1L)
        ))
    )
    expect_identical(detail$calendars[[1L]], requirement@calendars)
    # Removing one point-frequency variable must reject the method even when
    # a mean-frequency Dataset with the same variable ID is present.
    point <- variables[vapply(
        requirement@variable_frequencies[variables],
        function(value) "3hrPt" %in% value,
        logical(1L)
    )][[1L]]
    catalog$frequency[catalog$variable_id == point] <- "3hr"
    rejected <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )
    expect_false(rejected$matrix$selected)
    expect_match(rejected$matrix$missing, paste0(":", point))
})

test_that("allowed frequencies are not reduced to their first choice", {
    transform <- daily_transform("qdm")
    # An explicit transform can extend its own contract; the offline evaluator
    # must inspect that object rather than reconstructing a default recipe.
    inputs <- transform@required_inputs
    for (role in c("model_future", "model_historical")) {
        inputs[[role]]@frequencies <- c("day", "mon")
    }
    transform@required_inputs <- inputs
    result <- eligibility_test__evaluate(
        eligibility_test__catalog(
            frequency = "mon",
            table = "Amon"
        ),
        transform = transform,
        scenarios = "ssp245"
    )
    expect_true(result$matrix$selected)
    expect_identical(unique(unlist(result$requirements$frequency_spec)), "mon")
    inputs$model_historical@frequencies <- "day"
    transform@required_inputs <- inputs
    incompatible <- eligibility_test__evaluate(
        eligibility_test__catalog(
            frequency = "mon",
            table = "Amon"
        ),
        transform = transform,
        scenarios = "ssp245"
    )
    expect_false(incompatible$matrix$selected)
})

test_that("offline results are deterministic and do not mutate or query the catalog", {
    catalog <- data.table::as.data.table(eligibility_test__catalog())
    original <- data.table::copy(catalog)
    local_mocked_bindings(
        availability__collect = function(...) stop("Unexpected Dataset query"),
        shift__cmip6_period_coverage = function(...) {
            stop("Unexpected File query")
        },
        .package = "epwshiftr"
    )
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_identical(catalog, original)
    reversed <- eligibility_test__evaluate(
        catalog[nrow(catalog):1L],
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_identical(result, reversed)
    # Missing provider identity aliases must fall back to member_id.
    catalog$variant_label <- NA_character_
    expect_identical(
        result,
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = "ssp245"
        )
    )
})

test_that("empty catalogs and malformed inputs have explicit contracts", {
    catalog <- eligibility_test__catalog()[0L]
    empty <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(empty$matrix), 0L)
    expect_equal(nrow(empty$requirements), 0L)
    expect_type(empty$matrix$selected, "logical")
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = "historical"
        ),
        "future experiment"
    )
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = c("ssp245", "ssp245")
        ),
        "duplicated"
    )
    expect_error(eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        common = "unknown"
    ))
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            transform = daily_transform("qdm")
        ),
        "either"
    )
    expect_error(
        eligibility_test__evaluate(catalog, methods = "missing"),
        "Unknown"
    )
})

test_that("internal eligibility preserves data.table outputs and caller indexes", {
    catalog <- eligibility_test__catalog(c(
        "tas",
        "tasmin",
        "tasmax",
        "huss",
        "ps"
    ))
    data.table::setkeyv(catalog, c("member_id", "variable_id"))
    data.table::setindexv(catalog, "experiment_id")
    original <- data.table::copy(catalog)
    indexes <- data.table::indices(catalog)
    result <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )
    expect_true(data.table::is.data.table(result$matrix))
    expect_true(data.table::is.data.table(result$requirements))
    expect_identical(catalog, original)
    expect_identical(data.table::key(catalog), data.table::key(original))
    expect_identical(data.table::indices(catalog), indexes)
    empty <- eligibility_test__evaluate(
        catalog[0L],
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_true(data.table::is.data.table(empty$matrix))
    expect_true(data.table::is.data.table(empty$requirements))
    expect_identical(names(result$matrix), names(empty$matrix))
    expect_identical(names(result$requirements), names(empty$requirements))
    # Base input support is an explicit compatibility boundary, not an
    # internal storage format; compare it against the data.table result.
    compatible <- eligibility_test__evaluate(
        as.data.frame(catalog),
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )
    expect_identical(result, compatible)
})

test_that("replicas and missing table partitions cannot manufacture coverage", {
    catalog <- eligibility_test__catalog(experiments = "ssp245")
    once <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    repeated <- data.table::rbindlist(rep(list(catalog), 100L))
    expect_identical(
        once,
        eligibility_test__evaluate(
            repeated,
            methods = "qdm",
            scenarios = "ssp245"
        )
    )
    invalid <- eligibility_test__catalog(experiments = "historical")
    invalid[, table_id := NA_character_]
    combined <- data.table::rbindlist(list(catalog, invalid))
    result <- eligibility_test__evaluate(
        combined,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_false(result$matrix$selected)
    expect_match(result$matrix$missing, "historical:tas")
    expect_identical(unname(result$requirements$table[[1L]]), "day")
})

test_that("joint path numbering retains declared alternative priority", {
    transform <- monthly_transform("epwshiftr")
    roles <- c(transform@required_inputs, transform@optional_inputs)
    variables <- unique(unlist(
        lapply(roles, function(role) {
            unlist(role@variable_sets, use.names = FALSE)
        }),
        use.names = FALSE
    ))
    catalog <- eligibility_test__catalog(
        variables,
        experiments = c("historical", "ssp245", "ssp585"),
        frequency = "mon",
        table = "Amon"
    )
    result <- eligibility_test__evaluate(
        catalog,
        transform = monthly_transform("epwshiftr"),
        include_optional_historical = TRUE
    )
    expect_true(all(result$matrix$selected))
    expect_identical(unique(result$matrix$path_id), 1L)
    expect_true(all(result$requirements$complete))
})

# Cross-period humidity substitutions must not be advertised as executable.
test_that("historical and future periods require the same variable alternative", {
    transform <- monthly_transform("epwshiftr")
    alternatives <- transform@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(
            alternatives[[1L]],
            experiments = "historical",
            frequency = "mon",
            table = "Amon"
        ),
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "ssp245",
            frequency = "mon",
            table = "Amon"
        )
    ))
    mixed <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(mixed$matrix$selected)
    complete <- data.table::rbindlist(list(
        catalog,
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "historical",
            frequency = "mon",
            table = "Amon"
        )
    ))
    # Equivalent contracts with different alternative order must still match.
    optional <- transform@optional_inputs
    optional$model_historical@variable_sets <- rev(
        optional$model_historical@variable_sets
    )
    transform@optional_inputs <- optional
    result <- eligibility_test__evaluate(
        complete,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_true(result$matrix$selected)
})
