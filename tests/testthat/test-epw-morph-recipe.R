test_that("epw_morph_recipe() accepts morph.R statistical downscaling method overrides", {
    recipe <- epw_morph_recipe(methods = c(tdb = "shift", rh = "shift"))

    expect_s3_class(recipe, "epw_morph_recipe")
    expect_equal(recipe$backend, "belcher")
    expect_equal(recipe$methods[c("tdb", "rh")], c(tdb = "shift", rh = "shift"))
    expect_equal(
        recipe$rules[epw_field == "dry_bulb_temperature", method],
        "shift"
    )
    expect_equal(
        recipe$rules[epw_field == "relative_humidity", method],
        "shift"
    )
    expect_equal(epw_morph_variables(recipe), epw_morph_variables("recommended"))
    expect_error(epw_morph_recipe(methods = c(foo = "shift")), "Unknown")
    expect_error(epw_morph_recipe(methods = c(tdb = "scale")), "Unsupported")
})
