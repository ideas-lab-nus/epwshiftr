test_that("vmsg() respects epwshiftr.verbose", {
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

test_that("checkmate_class_match() handles S7 base classes", {
    expect_true(checkmate_class_match(90, S7::class_double))
    expect_true(checkmate_class_match(1L, S7::class_integer))
    expect_true(checkmate_class_match(TRUE, S7::class_logical))
    expect_true(checkmate_class_match(1 + 2i, S7::class_complex))
    expect_true(checkmate_class_match("x", S7::class_character))
    expect_true(checkmate_class_match(as.raw(1), S7::class_raw))
    expect_true(checkmate_class_match(list(1), S7::class_list))
    expect_true(checkmate_class_match(expression(x), S7::class_expression))
    expect_true(checkmate_class_match(as.name("x"), S7::class_name))
    expect_true(checkmate_class_match(quote(f(x)), S7::class_call))
    expect_true(checkmate_class_match(function() NULL, S7::class_function))
    expect_true(checkmate_class_match(sum, S7::class_function))
    expect_true(checkmate_class_match(`if`, S7::class_function))
    expect_true(checkmate_class_match(new.env(), S7::class_environment))

    expect_false(checkmate_class_match(90, S7::class_integer))
    expect_false(checkmate_class_match(1L, S7::class_double))
})

test_that("checkmate_class_match() handles S7 union classes", {
    expect_true(checkmate_class_match(90, S7::class_numeric))
    expect_true(checkmate_class_match(1L, S7::class_numeric))

    expect_true(checkmate_class_match(TRUE, S7::class_atomic))
    expect_true(checkmate_class_match(1L, S7::class_atomic))
    expect_true(checkmate_class_match(1, S7::class_atomic))
    expect_true(checkmate_class_match(1 + 2i, S7::class_atomic))
    expect_true(checkmate_class_match("x", S7::class_atomic))
    expect_true(checkmate_class_match(as.raw(1), S7::class_atomic))
    expect_false(checkmate_class_match(list(1), S7::class_atomic))

    expect_true(checkmate_class_match(TRUE, S7::class_vector))
    expect_true(checkmate_class_match(list(1), S7::class_vector))
    expect_true(checkmate_class_match(expression(x), S7::class_vector))
    expect_false(checkmate_class_match(new.env(), S7::class_vector))

    expect_true(checkmate_class_match(as.name("x"), S7::class_language))
    expect_true(checkmate_class_match(quote(f(x)), S7::class_language))
    expect_false(checkmate_class_match("x", S7::class_language))
})

test_that("checkmate_class_match() handles S3 and S4 class boundaries", {
    CheckmateS4ForTest <- methods::setClass("CheckmateS4ForTest", slots = c(value = "numeric"))

    expect_true(checkmate_class_match(Sys.Date(), S7::class_Date))
    expect_false(checkmate_class_match(1, S7::class_Date))
    expect_false(checkmate_class_match(CheckmateS4ForTest(value = 1), S7::class_Date))

    expect_true(checkmate_class_match(CheckmateS4ForTest(value = 1), methods::getClass("CheckmateS4ForTest")))
    expect_false(checkmate_class_match(list(value = 1), methods::getClass("CheckmateS4ForTest")))
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

test_that("checkmate_property() union spec accepts double branches", {
    NumericHolderForTest <- S7::new_class(
        "NumericHolderForTest",
        properties = list(
            value = checkmate_property(
                checkmate_any(
                    checkmate_rule(
                        S7::class_integer,
                        checkmate::check_integer,
                        branch = "int"
                    ),
                    checkmate_rule(
                        S7::class_double,
                        checkmate::check_double,
                        branch = "dbl"
                    )
                )
            )
        )
    )

    expect_identical(NumericHolderForTest(2L)@value, 2L)
    expect_identical(NumericHolderForTest(2)@value, 2)
    expect_error(NumericHolderForTest("2"), "must be <integer> or <double>")
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

test_that(".onLoad() initializes directory options without requiring existing dirs", {
    dir_opts <- c("epwshiftr.dir_store", "epwshiftr.dir_cache")
    old_present <- dir_opts %in% names(options())
    old_values <- options()[dir_opts]
    withr::defer({
        for (i in seq_along(dir_opts)) {
            value <- if (old_present[[i]]) old_values[[dir_opts[[i]]]] else NULL
            do.call(options, stats::setNames(list(value), dir_opts[[i]]))
        }
    })
    for (opt in dir_opts) {
        do.call(options, stats::setNames(list(NULL), opt))
    }

    store_default <- store_normalize_path(tools::R_user_dir("epwshiftr", "data"))
    cache_default <- store_normalize_path(tools::R_user_dir("epwshiftr", "cache"))
    store_exists <- dir.exists(store_default)
    cache_exists <- dir.exists(cache_default)

    on_load <- get(".onLoad", envir = asNamespace("epwshiftr"))
    expect_warning(on_load("", "epwshiftr"), NA)

    expect_identical(cache__env$package, "epwshiftr")
    expect_identical(cache__env$option_prefix, "epwshiftr")
    expect_identical(getOption("epwshiftr.dir_store"), store_default)
    expect_identical(getOption("epwshiftr.dir_cache"), cache_default)
    if (!store_exists) {
        expect_false(dir.exists(store_default))
    }
    if (!cache_exists) {
        expect_false(dir.exists(cache_default))
    }
})

test_that(".onLoad() preserves explicit directory options", {
    store <- tempfile("epwshiftr-store-explicit-")
    cache <- tempfile("epwshiftr-cache-explicit-")
    withr::local_options(list(
        epwshiftr.dir_store = store,
        epwshiftr.dir_cache = cache
    ))

    on_load <- get(".onLoad", envir = asNamespace("epwshiftr"))
    expect_warning(on_load("", "epwshiftr"), NA)

    expect_identical(cache__env$package, "epwshiftr")
    expect_identical(cache__env$option_prefix, "epwshiftr")
    expect_identical(getOption("epwshiftr.dir_store"), store)
    expect_identical(getOption("epwshiftr.dir_cache"), cache)
    expect_false(dir.exists(store))
    expect_false(dir.exists(cache))
})

test_that("store_cmip6_index_path() / store_rel_path() / store_abs_path() keep paths inside the store", {
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
