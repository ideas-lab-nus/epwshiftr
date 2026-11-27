test_that("signal profiles keep published and experimental defaults distinct", {
    expect_error(
        signal__variable_profile(
            "tas",
            settings = list(offset = 2),
            evidence = "published"
        ),
        "must provide at least one reference"
    )

    published <- signal__variable_profile(
        "tas",
        settings = list(offset = 2),
        evidence = "published",
        references = "doi:10.1000/example"
    )
    experimental <- signal__variable_profile(
        "pr",
        settings = list(factor = 1.1),
        evidence = "experimental"
    )

    expect_identical(published@evidence, "published")
    expect_identical(published@references, "doi:10.1000/example")
    expect_identical(experimental@evidence, "experimental")
})

test_that("signal settings helpers preserve complete method schemas", {
    settings <- list(tas = list(alpha = 1, iterations = 3))

    expect_identical(
        signal__resolve_settings(
            settings,
            c("alpha", "iterations"),
            "Example Method"
        ),
        settings$tas
    )
    expect_error(
        signal__resolve_settings(
            list(),
            c("alpha", "iterations"),
            "Example Method"
        ),
        "requires settings for exactly one variable"
    )
    expect_error(
        signal__resolve_settings(
            list(tas = list(alpha = 1, extra = 2)),
            c("alpha", "iterations"),
            "Example Method"
        ),
        "complete supported schema"
    )
    expect_identical(
        signal__integer_setting(3, "iterations", lower = 1L),
        3L
    )
    expect_identical(signal__random_seed(17), 17L)
    expect_error(
        signal__integer_setting(0, "iterations", lower = 1L),
        "resolved\\$iterations"
    )
    expect_no_error(
        signal__ordered_bounds(c(1, 1), "Bounds must be ordered.")
    )
    expect_error(
        signal__ordered_bounds(
            c(1, 1),
            "Bounds must be strictly ordered.",
            strict = TRUE
        ),
        "strictly ordered"
    )
})

test_that("threshold randomization keeps method boundary semantics", {
    series <- list(
        observed_reference = data.frame(value = c(0, 0.5, 1)),
        model_historical = data.frame(value = c(0.25, 0.5, 2))
    )
    key <- list(site = "A")
    set.seed(2718)
    rng_state <- .Random.seed

    inclusive <- signal__randomize_threshold_values(
        series,
        random_seed = 19L,
        key = key,
        variable = "pr",
        threshold = 0.5,
        inclusive = TRUE
    )
    strict <- signal__randomize_threshold_values(
        series,
        random_seed = 19L,
        key = key,
        variable = "pr",
        threshold = 0.5,
        inclusive = FALSE
    )

    expect_identical(.Random.seed, rng_state)
    expect_identical(
        inclusive$counts,
        c(observed_reference = 2L, model_historical = 2L)
    )
    expect_identical(
        strict$counts,
        c(observed_reference = 1L, model_historical = 1L)
    )
    expect_identical(strict$values$observed_reference[[2L]], 0.5)
    expect_identical(strict$values$model_historical[[2L]], 0.5)
    expect_false(
        identical(
            inclusive$seeds[["observed_reference"]],
            inclusive$seeds[["model_historical"]]
        )
    )
    expect_identical(
        inclusive,
        signal__randomize_threshold_values(
            series,
            random_seed = 19L,
            key = key,
            variable = "pr",
            threshold = 0.5,
            inclusive = TRUE
        )
    )
})

test_that("different signal kernels share one execution lifecycle", {
    requirement <- component__input_requirement(
        "model_future",
        representations = "series",
        frequencies = "day",
        variable_sets = "tas"
    )
    profile <- signal__variable_profile(
        "tas",
        settings = list(offset = 2, factor = 3),
        evidence = "published",
        references = "doi:10.1000/example"
    )
    additive <- signal__component(
        "additive_test",
        required_inputs = list(model_future = requirement),
        profiles = list(profile),
        apply_group = function(inputs, settings, key) {
            inputs$model_future + settings$tas$offset
        },
        operations = list(
            validate_result = function(value, inputs, key) {
                if (
                    is.numeric(value) &&
                        length(value) == length(inputs$model_future)
                ) {
                    return(TRUE)
                }
                "Signal result must match the future input length."
            }
        )
    )
    multiplicative <- signal__component(
        "multiplicative_test",
        required_inputs = list(model_future = requirement),
        profiles = list(profile),
        apply_group = function(inputs, settings, key) {
            inputs$model_future * settings$tas$factor
        }
    )
    inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )
    groups <- list(
        signal__group(
            key = list(site = "A"),
            inputs = list(model_future = c(1, 2, 3)),
            variables = "tas"
        ),
        signal__group(
            key = list(site = "B"),
            inputs = list(model_future = c(4, 5)),
            variables = "tas"
        )
    )

    additive_result <- component__execute(
        additive,
        "apply",
        inputs = inputs,
        groups = groups
    )
    multiplicative_result <- component__execute(
        multiplicative,
        "apply",
        inputs = inputs,
        groups = groups
    )

    expect_true(S7::S7_inherits(additive_result, SignalExecutionResult))
    expect_identical(additive_result@values, list(c(3, 4, 5), c(6, 7)))
    expect_identical(
        multiplicative_result@values,
        list(c(3, 6, 9), c(12, 15))
    )
    expect_identical(additive_result@diagnostics$status, c("ok", "ok"))
    expect_identical(
        additive_result@diagnostics$group,
        c("site=A", "site=B")
    )
    expect_identical(
        additive@metadata$signal_profiles$tas$evidence,
        "published"
    )
    expect_identical(
        additive@metadata$signal_profiles$tas$settings$offset,
        2
    )
})

test_that("signal execution resolves overrides without changing provenance", {
    requirement <- component__input_requirement(
        "model_future",
        frequencies = "day",
        variable_sets = "tas"
    )
    component <- signal__component(
        "override_test",
        required_inputs = list(model_future = requirement),
        profiles = list(signal__variable_profile(
            "tas",
            settings = list(offset = 1),
            evidence = "experimental"
        )),
        apply_group = function(inputs, settings, key) {
            inputs$model_future + settings$tas$offset
        }
    )
    inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )
    groups <- list(
        signal__group(
            key = list(site = "A"),
            inputs = list(model_future = 1:2),
            variables = "tas"
        ),
        signal__group(
            key = list(site = "B"),
            inputs = list(model_future = 3:4),
            variables = "tas"
        )
    )

    expect_warning(
        result <- component__execute(
            component,
            "apply",
            inputs = inputs,
            groups = groups,
            overrides = list(tas = list(offset = 5))
        ),
        "experimental"
    )
    expect_identical(result@values, list(c(6, 7), c(8, 9)))
    expect_identical(
        result@diagnostics$evidence,
        c("experimental", "experimental")
    )
    expect_identical(
        component@metadata$signal_profiles$tas$settings$offset,
        1
    )
    expect_identical(result@profiles$tas$settings$offset, 5)
})

test_that("signal group failures are explicit instead of silent NaN output", {
    requirement <- component__input_requirement(
        "model_future",
        frequencies = "day",
        variable_sets = "tas"
    )
    component <- signal__component(
        "failure_test",
        required_inputs = list(model_future = requirement),
        profiles = list(signal__variable_profile(
            "tas",
            evidence = "published",
            references = "doi:10.1000/example"
        )),
        apply_group = function(inputs, settings, key) {
            if (any(inputs$model_future < 0)) {
                stop("negative input")
            }
            inputs$model_future
        }
    )
    inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )
    groups <- list(
        signal__group(
            key = list(site = "A"),
            inputs = list(model_future = 1:2),
            variables = "tas"
        ),
        signal__group(
            key = list(site = "B"),
            inputs = list(model_future = c(-1, 2)),
            variables = "tas"
        )
    )

    collected <- component__execute(
        component,
        "apply",
        inputs = inputs,
        groups = groups,
        error_policy = "collect"
    )

    expect_length(collected@values, 2L)
    expect_identical(collected@values[[1L]], 1:2)
    expect_null(collected@values[[2L]])
    expect_identical(collected@diagnostics$status, c("ok", "error"))
    expect_match(collected@diagnostics$message[[2L]], "negative input")
    expect_error(
        component__execute(
            component,
            "apply",
            inputs = inputs,
            groups = groups
        ),
        "failed for.*site=B"
    )
})

test_that("signal execution validates source and group role contracts", {
    observed <- component__input_requirement(
        "observed_reference",
        frequencies = "day",
        variable_sets = "tas"
    )
    future <- component__input_requirement(
        "model_future",
        frequencies = "day",
        variable_sets = "tas"
    )
    component <- signal__component(
        "role_test",
        required_inputs = list(
            observed_reference = observed,
            model_future = future
        ),
        profiles = list(signal__variable_profile(
            "tas",
            evidence = "published",
            references = "doi:10.1000/example"
        )),
        apply_group = function(inputs, settings, key) {
            inputs$model_future - mean(inputs$observed_reference)
        }
    )
    inputs <- weather__new_inputs(
        observed_reference = weather__new_input(
            "observed_reference",
            data.frame(variable_id = "tas", frequency = "day")
        ),
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )
    incomplete <- list(signal__group(
        inputs = list(model_future = 1:2),
        variables = "tas"
    ))

    result <- component__execute(
        component,
        "apply",
        inputs = inputs,
        groups = incomplete,
        error_policy = "collect"
    )

    expect_identical(result@diagnostics$status, "error")
    expect_match(
        result@diagnostics$message,
        "missing required input role.*observed_reference"
    )
})

test_that("parametric distributions support optional fixed locations", {
    probability <- c(0.1, 0.5, 0.9)
    gamma <- list(
        family = "gamma",
        parameters = list(shape = 2, scale = 3, location = 5),
        sample_size = 20L,
        method = "test"
    )
    weibull <- list(
        family = "weibull",
        parameters = list(shape = 1.5, scale = 2, location = 0.25),
        sample_size = 20L,
        method = "test"
    )

    for (fit in list(gamma, weibull)) {
        expect_equal(
            distribution__cdf(
                fit,
                distribution__quantile(fit, probability)
            ),
            probability,
            tolerance = 1e-12
        )
        expect_equal(
            distribution__cdf(fit, fit$parameters$location - 1),
            0
        )
    }
})

test_that("parametric distribution validation rejects incomplete fits", {
    expect_error(
        distribution__cdf(
            list(
                family = "weibull",
                parameters = list(shape = 2),
                sample_size = 10L,
                method = "test"
            ),
            1
        ),
        "requires `shape` and `scale`"
    )
})

test_that("shared signal profile builder and numeric validators enforce contracts", {
    settings <- list(
        tas = list(offset = 1),
        pr = list(scale = 2)
    )
    by_var <- signal__build_profiles(
        settings_by_variable = settings,
        published_variables = "tas",
        references = "doi:10.1000/example",
        method = "demo_method",
        metadata_fn = function(variable, published) {
            list(
                default_source = if (published) "literature" else "package",
                optional_note = NULL
            )
        }
    )
    indexed <- signal__profiles(by_var)
    expect_identical(indexed$tas@evidence, "published")
    expect_identical(indexed$tas@references, "doi:10.1000/example")
    expect_identical(indexed$tas@metadata$default_source, "literature")
    expect_true("optional_note" %in% names(indexed$tas@metadata))
    expect_null(indexed$tas@metadata$optional_note)
    expect_identical(indexed$pr@evidence, "experimental")
    expect_identical(indexed$pr@references, character())

    all_exp <- signal__build_profiles(
        settings_by_variable = settings,
        published_variables = "tas",
        references = "doi:10.1000/example",
        method = "demo_method",
        evidence_mode = "all_experimental"
    )
    expect_identical(all_exp[[1L]]@evidence, "experimental")
    expect_identical(all_exp[[1L]]@references, "doi:10.1000/example")

    expect_identical(signal__positive_number(0.25, "alpha"), 0.25)
    expect_identical(
        signal__positive_number(0.5, "alpha", upper = 0.5),
        0.5
    )
    expect_error(
        signal__positive_number(0, "alpha"),
        "`alpha` must be positive"
    )
    expect_error(
        signal__positive_number(0.5, "alpha", upper = 0.5, strict_upper = TRUE),
        "strictly between zero and 0.5"
    )
    expect_error(
        signal__positive_number(
            1,
            "detrend_alpha",
            upper = 1,
            strict_upper = TRUE,
            message = "`detrend_alpha` must lie strictly between zero and one."
        ),
        "strictly between zero and one"
    )

    valid_window <- signal__validate_future_block_window(list(
        future_window_years = 17,
        output_block_years = 9
    ))
    expect_identical(valid_window$future_window_years, 17L)
    expect_identical(valid_window$output_block_years, 9L)
    expect_error(
        signal__validate_future_block_window(list(
            future_window_years = 16L,
            output_block_years = 9L
        )),
        "even, non-negative"
    )
    expect_error(
        signal__validate_future_block_window(list(
            future_window_years = 9L,
            output_block_years = 11L
        )),
        "even, non-negative"
    )
})

# vim: fdm=marker :
