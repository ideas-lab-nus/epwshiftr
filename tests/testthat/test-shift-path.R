test_that("delivery and store paths may be distinct siblings", {
    root <- withr::local_tempdir()
    paths <- shift__validate_delivery_store_paths(
        file.path(root, "delivery"),
        file.path(root, "store")
    )

    expect_named(paths, c("dir", "store"))
    expect_false(identical(paths$dir, paths$store))

    # A lexical prefix alone is not an ancestor relationship.
    expect_no_error(shift__validate_delivery_store_paths(
        file.path(root, "output"),
        file.path(root, "output-cache")
    ))
})

test_that("equal delivery and store paths are rejected canonically", {
    root <- withr::local_tempdir()
    output <- file.path(root, "output")

    expect_error(
        shift__validate_delivery_store_paths(output, file.path(output, ".")),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift__validate_delivery_store_paths(
            output,
            file.path(root, "missing", "..", "output")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift__validate_delivery_store_paths(output, output),
        "dir.*receives exported EPW files only"
    )
})

test_that("delivery and store paths cannot contain one another", {
    root <- withr::local_tempdir()

    # Neither descendant needs to exist for the relationship to be detected.
    expect_error(
        shift__validate_delivery_store_paths(
            file.path(root, "delivery"),
            file.path(root, "delivery", "internal", "store")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        shift__validate_delivery_store_paths(
            file.path(root, "store", "exports"),
            file.path(root, "store")
        ),
        "omit.*store.*epwshiftr.dir_store"
    )
})

test_that("symbolic-link aliases cannot bypass path isolation", {
    skip_on_os("windows")
    root <- withr::local_tempdir()
    real <- file.path(root, "real")
    link <- file.path(root, "alias")
    dir.create(real)
    if (!isTRUE(file.symlink(real, link))) {
        skip("Symbolic links are unavailable on this filesystem")
    }

    expect_error(
        shift__validate_delivery_store_paths(
            file.path(real, "delivery"),
            file.path(link, "delivery", "store")
        ),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
})

test_that("path comparison follows case-insensitive filesystem semantics", {
    root <- withr::local_tempdir()
    output <- file.path(root, "delivery")
    case_variant <- file.path(root, "DELIVERY")

    if (isTRUE(shift__workflow_path_case_sensitive(root))) {
        expect_no_error(shift__validate_delivery_store_paths(output, case_variant))
    } else {
        expect_error(
            shift__validate_delivery_store_paths(output, case_variant),
            "separate, non-overlapping directories",
            fixed = TRUE
        )
    }
})

test_that("shift_future_epw rejects an overlapping delivery and store tree", {
    root <- withr::local_tempdir()
    common <- file.path(root, "workflow")
    make_plan <- function(dir, store) {
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6("BCC-CSM2-MR", "ssp126"),
            periods = list(`2060s` = 2060L),
            method = belcher(),
            dir = dir,
            store = store,
            dry_run = TRUE
        )
    }

    expect_error(
        make_plan(common, common),
        "separate, non-overlapping directories",
        fixed = TRUE
    )
    expect_error(
        make_plan(file.path(common, "delivery"), common),
        "dir.*receives exported EPW files only"
    )
    expect_s7_class(
        make_plan(file.path(root, "delivery"), file.path(root, "store")),
        ShiftPlan
    )
})
