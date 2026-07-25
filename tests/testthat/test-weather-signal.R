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
                if (is.numeric(value) &&
                    length(value) == length(inputs$model_future)) {
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
