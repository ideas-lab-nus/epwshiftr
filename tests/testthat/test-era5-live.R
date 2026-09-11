test_that("ERA5 can be retrieved, normalized, persisted, and reused", {
    era5_live__skip_unless_enabled()
    store_path <- tempfile("era5-live-store-")
    store <- EsgStore$new(store_path)
    store$close()
    site <- shift_site(epw = get_cache_epw(), id = "SIN")
    stage <- shift_stage_new(
        ShiftFiles,
        "files",
        store_path = store_path
    )
    recipe <- transform__recipe(daily_transform("isimip3basd"))
    source <- shift_era5(
        years = 2000,
        variables = "tas",
        frequency = "day",
        access = "arco",
        timeout = 1800,
        poll_interval = 2
    )

    climate <- reanalysis__materialize(stage, recipe, site, source)
    files <- list.files(
        file.path(store_path, "sources", "reanalysis", "era5"),
        pattern = "[.]nc$",
        recursive = TRUE,
        full.names = TRUE
    )

    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_true(all(shift_coverage(climate)$complete))
    expect_length(files, 1L)
    expect_gt(file.info(files)$size, 0)

    reused <- reanalysis__materialize(stage, recipe, site, source)
    expect_true(S7::S7_inherits(reused, ShiftClimate))
    expect_true(reused@meta$reused)
    expect_identical(files, list.files(
        file.path(store_path, "sources", "reanalysis", "era5"),
        pattern = "[.]nc$",
        recursive = TRUE,
        full.names = TRUE
    ))
})
