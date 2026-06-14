# esgdict_option() {{{

test_that("esgdict_option() uses bidirectional relation indices and reports ignored constraints", {
    dict <- local_test_esgdict()

    exp <- esgdict_option("experiment", activity = "CMIP", dict = dict)
    expect_true("historical" %in% exp$value)
    expect_false("ssp585" %in% exp$value)

    activity <- esgdict_option("activity_id", experiment_id = "historical", dict = dict)
    expect_equal(activity$value, "CMIP")

    source <- esgdict_option("source_id", activity_id = "ScenarioMIP", dict = dict)
    expect_equal(source$value, "EC-Earth3")

    institution <- esgdict_option("institution_id", source_id = "EC-Earth3", dict = dict)
    expect_equal(institution$value, "EC-Earth-Consortium")

    variables <- esgdict_option("variable_id", table_id = "day", dict = dict)
    expect_equal(variables$value, "tas")

    tables <- esgdict_option("table_id", variable_id = "tas", dict = dict)
    expect_setequal(tables$value, c("day", "Amon"))

    freq <- esgdict_option("frequency", variable_id = "tas", table_id = "day", dict = dict)
    expect_equal(freq$value, "day")

    expect_warning(esgdict_option("activity_id", variable_id = "tas", dict = dict), "Ignored constraint")
    ignored <- suppressWarnings(esgdict_option("activity_id", variable_id = "tas", dict = dict))
    expect_equal(attr(ignored, "ignored_constraints"), "variable_id")

    variant <- esgdict_option("variant_label", dict = dict)
    expect_equal(variant$pattern, CMIP6DICT_VARIANT_PATTERN)

    expect_equal(dict$options("experiment_id", activity_id = "ScenarioMIP")$value, "ssp585")
})

# }}}

# esgdict_check() {{{

test_that("esgdict_check() returns rich value and relationship diagnostics", {
    dict <- local_test_esgdict()

    ok <- esgdict_check(
        activity = "CMIP",
        experiment = "historical",
        table_id = "day",
        variable = "tas",
        variant = "r1i1p1f1",
        dict = dict
    )
    expect_s3_class(ok, "esgdict_check_result")
    expect_true(all(ok$valid))
    expect_true(all(c("rule", "source", "constraint_fields", "compatible_values") %in% names(ok)))

    typo <- esgdict_check(experiment = "historial", dict = dict)
    expect_false(typo[field == "experiment_id"]$valid)
    expect_true("historical" %in% typo[field == "experiment_id"]$suggestions[[1L]])

    bad_exp <- esgdict_check(activity = "ScenarioMIP", experiment = "historical", dict = dict)
    expect_true(any(!bad_exp$valid & bad_exp$type == "relationship"))
    expect_true("CMIP" %in% unlist(bad_exp[type == "relationship"]$compatible_values))

    any_ok <- esgdict_check(table_id = c("day", "fx"), variable_id = c("tas", "sftlf"), dict = dict)
    expect_true(all(any_ok$valid))

    all_pairs <- esgdict_check(
        table_id = c("day", "fx"),
        variable_id = c("tas", "sftlf"),
        dict = dict,
        relationship = "all_pairs"
    )
    expect_true(any(!all_pairs$valid & all_pairs$rule == "variable"))

    expect_error(esgdict_check(experiment = "historial", dict = dict, error = TRUE))
    expect_s3_class(dict$check(activity = "CMIP", experiment = "historical"), "esgdict_check_result")
})

# }}}
