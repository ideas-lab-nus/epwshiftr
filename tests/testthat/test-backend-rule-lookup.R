# Exercise the existing backend interface where duplicate rules and overrides
# have distinct validation and update semantics.
test_that("backend rule lookup preserves duplicate steps and isolated results", {
    rules <- data.table::data.table(
        step = c("dry", "fixed", "dry"),
        epw_field = c(
            "dry_bulb_temperature",
            "wind_speed",
            "dew_point_temperature"
        ),
        variable_id = c("tas", "sfcWind", "tas"),
        method = c("offset", "fixed", "offset"),
        required = TRUE,
        method_choices = list(c("offset", "scale"), "fixed", "offset")
    )
    backend <- EpwMorphBackend$new(
        name = "rule_lookup_check",
        methods = c(dry = "offset", absent = "offset"),
        method_choices = c("offset", "scale"),
        rules = rules,
        runner = function(context, backend) context
    )
    original <- backend$rules()
    expect_identical(
        backend$validate_methods(c(dry = "scale")),
        c(dry = "scale", absent = "offset")
    )
    out <- backend$rules_with_methods(c(dry = "scale"))
    expect_identical(out$step, c("dry", "fixed", "dry"))
    expect_identical(out$method, c("scale", "fixed", "scale"))
    expect_equal(backend$rules(), original)
    data.table::set(out, j = "method", value = "changed")
    expect_equal(backend$rules(), original)
    expect_identical(
        backend$validate_methods(c(absent = "scale"))[["absent"]],
        "scale"
    )
    expect_error(backend$validate_methods(c(dry = "invalid")), "Unsupported")
    expect_error(backend$validate_methods(c(unknown = "offset")), "Unknown")
    expect_error(backend$validate_methods(c(dry = NA_character_)))
    expect_identical(backend$validate_methods(character()), backend$methods())
    # Duplicate named overrides retain the original first-name lookup behavior.
    expect_identical(
        backend$rules_with_methods(c(dry = "scale", dry = "offset"))$method,
        c("scale", "fixed", "scale")
    )
})

# Empty rule tables use the same interface and preserve their column types.
test_that("backend rule lookup keeps empty rules and no overrides", {
    backend <- EpwMorphBackend$new(
        name = "empty_rule_lookup_check",
        methods = character(),
        method_choices = character(),
        rules = data.table::data.table(
            step = character(),
            epw_field = character(),
            variable_id = character(),
            method = character(),
            required = logical()
        ),
        runner = function(context, backend) context
    )
    expect_equal(backend$rules_with_methods(), backend$rules())
    expect_identical(backend$rules_with_methods()$method, character())
})

# vim: fdm=marker :
