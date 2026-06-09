test_that("Verbose message", {
    withr::with_options(
        list("epwshiftr.verbose" = FALSE),
        expect_silent(vmsg("a"))
    )
    withr::with_options(
        list("epwshiftr.verbose" = TRUE),
        expect_message(vmsg("a"), "a")
    )
})

test_that("now()", {
    expect_s3_class(now(), "POSIXct")
    expect_equal(attr(now(), "tzone"), "UTC")
})

test_that("checkmate_result()", {
    expect_null(checkmate_result(TRUE))
    expect_identical(
        checkmate_result("Must be >= 0"),
        "Must be >= 0"
    )
    expect_identical(
        checkmate_result("Must be >= 0", label = "value"),
        "`value` validation failed: Must be >= 0"
    )
})

test_that("checkmate_validator()", {
    validator <- checkmate_validator(
        checkmate::check_integer,
        lower = 1L,
        upper = 3L,
        len = 1L
    )

    expect_null(validator(2L))
    expect_match(validator(0L), ">= 1")

    labeled <- checkmate_validator(
        checkmate::check_string,
        min.chars = 1L,
        label = "value"
    )

    expect_match(labeled(""), "`value` validation failed:")
    expect_match(labeled(""), "at least 1 characters")
})

test_that("checkmate_property()", {
    CountHolderForTest <- S7::new_class(
        "CountHolderForTest",
        properties = list(
            n = checkmate_property(
                S7::class_integer,
                checkmate::check_int,
                lower = 0L,
                upper = 10L,
                default = 1L
            )
        )
    )

    expect_identical(CountHolderForTest()@n, 1L)
    expect_identical(CountHolderForTest(5L)@n, 5L)
    expect_error(CountHolderForTest(-1L), ">= 0")
})

test_that("checkmate_rule() validates rule inputs", {
    rule <- checkmate_rule(
        S7::class_integer,
        checkmate::check_integer,
        lower = 0L,
        branch = "int"
    )

    expect_s3_class(rule, "CheckmateRule")
    expect_identical(rule$class, S7::class_integer)
    expect_identical(rule$args$lower, 0L)
    expect_identical(rule$branch, "int")

    expect_error(
        checkmate_rule(
            S7::class_integer | S7::class_character,
            checkmate::check_integer
        ),
        "S7_union"
    )
})

test_that("checkmate_any() builds a union spec", {
    spec <- checkmate_any(
        checkmate_rule(S7::class_integer, checkmate::check_integer),
        checkmate_rule(S7::class_character, checkmate::check_character)
    )

    expect_s3_class(spec, "CheckmateSpec")
    expect_s3_class(spec, "CheckmateSpecAny")
    expect_true(inherits(spec$class, "S7_union"))
    expect_length(spec$rules, 2)

    expect_error(
        checkmate_any(checkmate_rule(S7::class_integer, checkmate::check_integer), 1),
        "CheckmateRule"
    )
})

test_that("checkmate_property() supports union specs", {
    UnionHolderForTest <- S7::new_class(
        "UnionHolderForTest",
        properties = list(
            value = checkmate_property(
                checkmate_any(
                    checkmate_rule(
                        S7::class_integer,
                        checkmate::check_integer,
                        lower = 0L,
                        branch = "int"
                    ),
                    checkmate_rule(
                        S7::class_character,
                        checkmate::check_string,
                        min.chars = 1L,
                        branch = "chr"
                    )
                ),
                default = 1L
            )
        )
    )

    expect_identical(UnionHolderForTest()@value, 1L)
    expect_identical(UnionHolderForTest(2L)@value, 2L)
    expect_identical(UnionHolderForTest("x")@value, "x")
    expect_error(UnionHolderForTest(-1L), "\\[int\\]")
    expect_error(UnionHolderForTest(-1L), ">= 0")
    expect_error(UnionHolderForTest(""), "\\[chr\\]")
    expect_error(UnionHolderForTest(1.5), "must be <integer> or <character>")
})

test_that("checkmate_property() union spec uses first matching branch", {
    PriorityParentForTest <- S7::new_class("PriorityParentForTest")
    PriorityChildForTest <- S7::new_class(
        "PriorityChildForTest",
        parent = PriorityParentForTest
    )

    PriorityHolderForTest <- S7::new_class(
        "PriorityHolderForTest",
        properties = list(
            value = checkmate_property(
                checkmate_any(
                    checkmate_rule(
                        PriorityChildForTest,
                        function(x) "child branch failed",
                        branch = "child"
                    ),
                    checkmate_rule(
                        PriorityParentForTest,
                        function(x) TRUE,
                        branch = "parent"
                    )
                )
            )
        )
    )

    expect_error(PriorityHolderForTest(PriorityChildForTest()), "\\[child\\]")
    expect_error(PriorityHolderForTest(PriorityChildForTest()), "child branch failed")
    expect_s3_class(PriorityHolderForTest(PriorityParentForTest())@value, "S7_object")
})

test_that("checkmate_property() union spec supports NULL branches", {
    OptionalHolderForTest <- S7::new_class(
        "OptionalHolderForTest",
        properties = list(
            value = checkmate_property(
                checkmate_any(
                    checkmate_rule(NULL, checkmate::check_null, branch = "null"),
                    checkmate_rule(
                        S7::class_integer,
                        checkmate::check_integer,
                        lower = 0L,
                        branch = "int"
                    )
                ),
                default = NULL
            )
        )
    )

    expect_null(OptionalHolderForTest()@value)
    expect_null(OptionalHolderForTest(NULL)@value)
    expect_identical(OptionalHolderForTest(3L)@value, 3L)
    expect_error(OptionalHolderForTest(-1L), "\\[int\\]")
})

test_that("set_size_units()", {
    expect_null(set_size_units(logical()))
    expect_warning(set_size_units(units::set_units(1:3, "Kelvin")))

    suppressWarnings(
        expect_equal(
            set_size_units(units::set_units(1, "Kelvin")),
            units::set_units(1, "Kelvin")
        )
    )

    expect_equal(
        units(set_size_units(units::set_units(1, "GByte")))$numerator,
        "MiB"
    )

    expect_equal(attr(now(), "tzone"), "UTC")
})


test_that("store_dir() resolves and creates the persistent store root", {
    root <- tempfile("epwshiftr-store-")
    withr::local_options(list(epwshiftr.dir_store = root, epwshiftr.verbose = FALSE))

    expect_false(dir.exists(root))
    expect_identical(store_dir(init = FALSE), store_normalize_path(root))
    expect_true(dir.exists(store_dir()))
    expect_identical(store_dir(), store_normalize_path(root))
})

test_that("store path helpers keep paths inside the store", {
    root <- tempfile("epwshiftr-store-")
    withr::local_options(list(epwshiftr.dir_store = root))

    path <- store_cmip6_index_path(strrep("a", 64L))
    expect_identical(store_rel_path(path), sprintf("queries/cmip6-index/%s.csv", strrep("a", 64L)))
    expect_identical(store_abs_path(sprintf("queries/cmip6-index/%s.csv", strrep("a", 64L))), path)
    expect_error(store_rel_path(tempfile()), "outside")
})

test_that("store_write_json_atomic() writes valid JSON", {
    root <- tempfile("epwshiftr-store-")
    withr::local_options(list(epwshiftr.dir_store = root))

    path <- store_path("queries", "payload.json")
    expect_identical(
        store_write_json_atomic(list(a = 1L), path, auto_unbox = TRUE),
        normalizePath(path, winslash = "/", mustWork = FALSE)
    )
    expect_identical(jsonlite::read_json(path, simplifyVector = TRUE)$a, 1L)
})
