# The approved coastal pilot is a worked request example, not a generated snapshot.
test_that("an independent ERA5 plan describes the six-point pilot without EPW", {
    coordinates <- list(
        c(104, 1.25),
        c(104, 1.5),
        c(55.25, 25.25),
        c(55.5, 25.25),
        c(18.5, -34),
        c(18.75, -34)
    )
    sites <- lapply(seq_along(coordinates), function(i) {
        shift_site(
            paste0("point-", i),
            coordinates[[i]][1],
            coordinates[[i]][2]
        )
    })
    source <- shift_era5(
        2001,
        variables = c("tas", "tdps", "ps", "uas", "vas", "rsds"),
        frequency = "hour",
        access = "arco"
    )
    plan <- shift_plan_reanalysis(source, sites)

    expect_identical(plan$status, "planned")
    expect_equal(nrow(plan$requests), 36L)
    expect_equal(unique(plan$requests$expected_hours), 8808L)
    expect_equal(unique(plan$requests$date_start), as.Date("2000-12-31"))
    expect_equal(unique(plan$requests$date_end), as.Date("2002-01-01"))
    expect_true(all(plan$requests$status == "not_submitted"))
    expect_length(unique(plan$requests$request_id), 36L)
    expect_identical(plan$submitted, 0L)
})

# A gap year must never become an implicit full year of remote acquisition.
test_that("separate year ranges preserve gaps and leap-year request sizes", {
    site <- shift_site("point", 104, 1.25)
    source <- shift_era5(
        c(2001, 2003, 2004),
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    requests <- shift_plan_reanalysis(source, site)$requests
    requests <- requests[order(date_start)]
    expect_equal(requests$date_start, as.Date(c("2000-12-31", "2002-12-31")))
    expect_equal(requests$date_end, as.Date(c("2002-01-01", "2005-01-01")))
    expect_equal(requests$expected_hours, c(8808L, 17592L))
})

# Equivalent longitude and derived-variable aliases share a physical request.
test_that("request identity shares raw dependencies and equivalent points", {
    source <- shift_era5(
        2001,
        variables = c("hurs", "huss", "tas"),
        frequency = "hour",
        access = "arco"
    )
    sites <- list(shift_site("east", 180, 90), shift_site("west", -180, 90))
    plan <- shift_plan_reanalysis(source, sites)
    expect_equal(nrow(plan$requests), 3L)
    expect_setequal(plan$requests$variable_id, c("tas", "tdps", "ps"))
    expect_true(all(vapply(
        plan$requests$site_ids,
        function(x) identical(x, c("east", "west")),
        logical(1)
    )))
    expect_identical(
        plan$requests$request_id,
        shift_plan_reanalysis(source, rev(sites))$requests$request_id
    )
    changed <- shift_era5(
        2002,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    expect_false(any(
        shift_plan_reanalysis(changed, sites)$requests$request_id %in%
            plan$requests$request_id
    ))
    expect_identical(sites[[1]]@lon, 180)
})

# Fail-closed planning must not replace an unavailable route or infer fields.
test_that("independent plans reject implicit or unsupported source intent", {
    site <- shift_site("point", 104, 1.25)
    expect_error(
        shift_plan_reanalysis(shift_era5(2001), site),
        "explicit access"
    )
    expect_error(
        shift_plan_reanalysis(
            shift_era5(
                2001,
                product = "land",
                variables = "tas",
                frequency = "hour",
                access = "arco"
            ),
            site
        ),
        "single_levels"
    )
    expect_error(
        shift_plan_reanalysis(
            shift_era5(2001, frequency = "hour", access = "arco"),
            site
        ),
        "explicit variables"
    )
    expect_error(
        shift_plan_reanalysis(
            shift_era5(
                2001,
                variables = "tas",
                frequency = "day",
                access = "arco"
            ),
            site
        ),
        "frequency"
    )
    expect_error(
        shift_plan_reanalysis(
            shift_era5(
                2001,
                variables = "clt",
                frequency = "hour",
                access = "arco"
            ),
            site
        ),
        "no CDS fallback"
    )
    expect_error(
        shift_plan_reanalysis(
            shift_era5(
                2001,
                variables = "unknown",
                frequency = "hour",
                access = "arco"
            ),
            site
        ),
        "requested CF variable"
    )
})

# Empty/duplicate sites are errors; unrelated metadata and credentials stay out of plans.
test_that("site boundaries and credential-free planning remain explicit", {
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site(
        "point",
        104,
        1.25,
        metadata = list(key = "private-metadata")
    )
    expect_error(shift_plan_reanalysis(source, list()), "length")
    expect_error(shift_plan_reanalysis(source, list(site, site)), "unique IDs")
    expect_error(
        shift_plan_reanalysis(source, list(list(lat = 1.25))),
        "ShiftSite"
    )
    expect_error(shift_site("bad", 361, 0))
    expect_error(shift_site("bad", 0, 91))
    expect_error(shift_era5(2001, variables = character()))
    withr::local_envvar(c(
        CDSAPI_KEY = "private-credential",
        CDSAPI_URL = "http://127.0.0.1:1/never-connect"
    ))
    plan <- shift_plan_reanalysis(source, site)
    serialized <- jsonlite::toJSON(plan)
    expect_false(grepl("private-", serialized, fixed = TRUE))
    expect_identical(plan$submitted, 0L)
    expect_equal(
        shift_plan_reanalysis(
            source,
            shift_site("south", 360, -90)
        )$requests$longitude,
        0
    )
})

# vim: fdm=marker :
