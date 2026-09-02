test_that("R6 EPW morphing backends can be looked up, registered, and selected", {
    expect_true("belcher" %in% epw_morph_backends())
    expect_true("belcher_absolute" %in% epw_morph_backends())
    expect_false("belcher_change_factor" %in% epw_morph_backends())

    belcher <- epw_morph_backend("belcher")
    expect_true(inherits(belcher, "EpwMorphBackend"))
    expect_false(belcher$requires_reference)
    expect_true(belcher$accepts_reference)
    expect_equal(belcher$required_variables(), c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", "pr"))
    expect_equal(epw_morph_variables(belcher), epw_morph_variables("recommended"))
    expect_equal(epw_morph_variables("belcher"), epw_morph_variables("recommended"))
    expect_equal(belcher$validate_methods(c(tdb = "shift"))[["tdb"]], "shift")
    expect_false(morpher__recipe_requires_reference(epw_morph_recipe("belcher")))
    expect_true(morpher__recipe_accepts_reference(epw_morph_recipe("belcher")))
    if (exists("belcher_absolute", envir = EPW_MORPH_BACKEND_WARNINGS, inherits = FALSE)) {
        rm("belcher_absolute", envir = EPW_MORPH_BACKEND_WARNINGS)
    }
    expect_warning(legacy <- epw_morph_backend("belcher_absolute"), "legacy absolute-target")
    expect_false(legacy$requires_reference)
    expect_false(legacy$accepts_reference)
    expect_false(morpher__recipe_requires_reference(epw_morph_recipe("belcher_absolute")))
    expect_error(epw_morph_backend("missing-backend"), "Unknown")
    expect_error(epw_morph_register_backend("not-a-backend", list()), "EpwMorphBackend")

    backend_name <- paste0("testbackend", Sys.getpid())
    rules <- data.table::data.table(
        step = "dry",
        epw_field = "dry_bulb_temperature",
        variable_id = "tas",
        optional_variable_id = NA_character_,
        method = "offset",
        required = TRUE,
        derived = FALSE,
        method_choices = list(c("offset", "plus_two"))
    )
    runner <- function(context, backend) {
        epw <- context$epw$clone()
        suppressMessages(epw$drop_unit())
        data <- data.table::as.data.table(epw$data())
        offset <- if (identical(context$recipe$methods[["dry"]], "plus_two")) 2 else 1
        data[, `:=`(
            dry_bulb_temperature = dry_bulb_temperature + offset,
            custom_backend = backend$name
        )]
        epw_morph_result(context, epw = epw, data = data)
    }
    custom <- EpwMorphBackend$new(
        name = backend_name,
        methods = c(dry = "offset"),
        method_choices = c("offset", "plus_two"),
        rules = rules,
        runner = runner
    )
    required_name <- paste0(backend_name, "required")
    required <- EpwMorphBackend$new(
        name = required_name,
        methods = c(dry = "offset"),
        method_choices = c("offset", "plus_two"),
        rules = rules,
        requires_reference = TRUE,
        runner = runner
    )
    expect_true(required$requires_reference)
    expect_true(required$accepts_reference)
    expect_error(
        EpwMorphBackend$new(
            name = paste0(required_name, "invalid"),
            methods = c(dry = "offset"),
            method_choices = "offset",
            rules = rules,
            requires_reference = TRUE,
            accepts_reference = FALSE,
            runner = runner
        ),
        "must also accept"
    )

    epw_morph_register_backend(backend_name, custom, overwrite = TRUE)
    epw_morph_register_backend(required_name, required, overwrite = TRUE)
    expect_identical(epw_morph_backend(backend_name), custom)
    expect_error(epw_morph_register_backend(backend_name, custom), "already registered")

    recipe <- epw_morph_recipe(name = backend_name, backend = backend_name, methods = c(dry = "plus_two"))
    expect_equal(recipe$methods[["dry"]], "plus_two")
    expect_equal(epw_morph_variables(recipe), "tas")
    expect_error(
        epw_morph_recipe(name = backend_name, backend = backend_name, methods = c(dry = "scale")),
        "Unsupported"
    )
    required_recipe <- epw_morph_recipe(name = required_name, backend = required_name)
    expect_true(morpher__recipe_requires_reference(required_recipe))
    expect_error(shift_morph_method(required_recipe), "requires an explicit reference")
    context <- morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = data.table::data.table(
            time = as.POSIXct("2060-01-01", tz = "UTC"),
            variable_id = "tas",
            period = "2060s",
            year = 2060L,
            lon = 104,
            lat = 1,
            units = "K",
            value = 300
        ),
        recipe = recipe
    )
    result <- morpher__run_context(context)

    expect_s3_class(result, "epw_morph_result")
    expect_equal(unique(result$data$custom_backend), backend_name)
})
