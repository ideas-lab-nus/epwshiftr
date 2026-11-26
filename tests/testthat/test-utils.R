test_that("now()", {
    expect_s3_class(now(), "POSIXct")
    expect_equal(attr(now(), "tzone"), "UTC")
})

test_that("checkmate_* S7 property validators support rules, unions, priority, and NULL branches", {
    expect_null(checkmate_result(TRUE))
    expect_identical(checkmate_result("Must be >= 0"), "Must be >= 0")
    expect_identical(
        checkmate_result("Must be >= 0", label = "value"),
        "`value` validation failed: Must be >= 0"
    )

    v_int <- checkmate_validator(
        checkmate::check_integer,
        lower = 1L,
        upper = 3L,
        len = 1L
    )
    expect_null(v_int(2L))
    expect_match(v_int(0L), ">= 1")
    v_str <- checkmate_validator(
        checkmate::check_string,
        min.chars = 1L,
        label = "value"
    )
    expect_match(v_str(""), "`value` validation failed:.*at least 1 characters")

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

    spec <- checkmate_any(
        rule,
        checkmate_rule(
            S7::class_character,
            checkmate::check_string,
            min.chars = 1L,
            branch = "chr"
        )
    )
    expect_s3_class(spec, c("CheckmateSpecAny", "CheckmateSpec"))
    expect_true(inherits(spec$class, "S7_union"))
    expect_length(spec$rules, 2L)
    expect_error(checkmate_any(rule, 1), "CheckmateRule")

    CheckmateS4ForTest <- methods::setClass(
        "CheckmateS4ForTest",
        slots = c(value = "numeric")
    )
    s4_val <- CheckmateS4ForTest(value = 1)
    s4_cls <- methods::getClass("CheckmateS4ForTest")
    expect_cls <- function(cls, yes, no = list()) {
        for (x in yes) {
            expect_true(checkmate_class_match(x, cls))
        }
        for (x in no) {
            expect_false(checkmate_class_match(x, cls))
        }
    }
    expect_cls(S7::class_double, list(90), list(1L))
    expect_cls(S7::class_integer, list(1L), list(90))
    expect_cls(S7::class_logical, list(TRUE))
    expect_cls(S7::class_complex, list(1 + 2i))
    expect_cls(S7::class_character, list("x"))
    expect_cls(S7::class_raw, list(as.raw(1)))
    expect_cls(S7::class_list, list(list(1)))
    expect_cls(S7::class_expression, list(expression(x)))
    expect_cls(S7::class_name, list(as.name("x")))
    expect_cls(S7::class_call, list(quote(f(x))))
    expect_cls(S7::class_function, list(function() NULL, sum, `if`))
    expect_cls(S7::class_environment, list(new.env()))
    expect_cls(S7::class_numeric, list(90, 1L))
    expect_cls(
        S7::class_atomic,
        list(TRUE, 1L, 1, 1 + 2i, "x", as.raw(1)),
        list(list(1))
    )
    expect_cls(
        S7::class_vector,
        list(TRUE, list(1), expression(x)),
        list(new.env())
    )
    expect_cls(S7::class_language, list(as.name("x"), quote(f(x))), list("x"))
    expect_cls(S7::class_Date, list(Sys.Date()), list(1, s4_val))
    expect_cls(s4_cls, list(s4_val), list(list(value = 1)))

    holder <- function(nm, prop) {
        S7::new_class(nm, properties = list(value = prop))
    }

    CountH <- holder(
        "CountHolderForTest",
        checkmate_property(
            S7::class_integer,
            checkmate::check_int,
            lower = 0L,
            upper = 10L,
            default = 1L
        )
    )
    expect_identical(CountH()@value, 1L)
    expect_identical(CountH(5L)@value, 5L)
    expect_error(CountH(-1L), ">= 0")

    UnionH <- holder(
        "UnionHolderForTest",
        checkmate_property(spec, default = 1L)
    )
    expect_identical(UnionH()@value, 1L)
    expect_identical(UnionH(2L)@value, 2L)
    expect_identical(UnionH("x")@value, "x")
    expect_error(UnionH(-1L), "\\[int\\].*>= 0")
    expect_error(UnionH(""), "\\[chr\\]")
    expect_error(UnionH(1.5), "must be <integer> or <character>")

    NumH <- holder(
        "NumericHolderForTest",
        checkmate_property(checkmate_any(
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
        ))
    )
    expect_identical(NumH(2L)@value, 2L)
    expect_identical(NumH(2)@value, 2)
    expect_error(NumH("2"), "must be <integer> or <double>")

    ParentC <- S7::new_class("PriorityParentForTest")
    ChildC <- S7::new_class("PriorityChildForTest", parent = ParentC)
    PrioH <- holder(
        "PriorityHolderForTest",
        checkmate_property(checkmate_any(
            checkmate_rule(
                ChildC,
                function(x) "child branch failed",
                branch = "child"
            ),
            checkmate_rule(ParentC, function(x) TRUE, branch = "parent")
        ))
    )
    expect_error(PrioH(ChildC()), "\\[child\\].*child branch failed")
    expect_s3_class(PrioH(ParentC())@value, "S7_object")

    OptH <- holder(
        "OptionalHolderForTest",
        checkmate_property(
            checkmate_any(
                checkmate_rule(NULL, checkmate::check_null, branch = "null"),
                rule
            ),
            default = NULL
        )
    )
    expect_null(OptH()@value)
    expect_null(OptH(NULL)@value)
    expect_identical(OptH(3L)@value, 3L)
    expect_error(OptH(-1L), "\\[int\\]")
})

test_that("set_size_units()", {
    expect_null(set_size_units(logical()))
    sizes <- set_size_units(c(0, 1024, 1024^2, NA_real_))
    expect_s3_class(sizes, "epwshiftr_bytes")
    expect_equal(as.numeric(sizes), c(0, 1024, 1024^2, NA_real_))
    expect_equal(
        format(sizes),
        c("0 Byte", "1.00 KiB", "1.00 MiB", NA_character_)
    )
    expect_output(print(sizes), "1.00 MiB", fixed = TRUE)

    expect_equal(attr(now(), "tzone"), "UTC")
})


test_that("store_dir() resolves and creates the persistent store root", {
    root <- tempfile("epwshiftr-store-")
    withr::local_options(list(
        epwshiftr.dir_store = root,
        epwshiftr.verbose = FALSE
    ))

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

    store_default <- store_normalize_path(tools::R_user_dir(
        "epwshiftr",
        "data"
    ))
    cache_default <- store_normalize_path(tools::R_user_dir(
        "epwshiftr",
        "cache"
    ))
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

test_that("store_rel_path() / store_abs_path() keep paths inside the store", {
    root <- tempfile("epwshiftr-store-")
    withr::local_options(list(epwshiftr.dir_store = root))

    path <- store_path(
        "queries",
        "cmip6-index",
        sprintf("%s.csv", strrep("a", 64L)),
        root = store_dir(init = TRUE)
    )
    expect_identical(
        store_rel_path(path),
        sprintf("queries/cmip6-index/%s.csv", strrep("a", 64L))
    )
    expect_identical(
        store_abs_path(sprintf("queries/cmip6-index/%s.csv", strrep("a", 64L))),
        path
    )
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

# Exercise the file-backed connection lifecycle without changing an open
# database instance's access mode.
test_that("ddb_connect() opens persisted databases read-only after closing writers", {
    path <- tempfile(fileext = ".duckdb")
    conn <- ddb_connect(path)
    on.exit(
        {
            if (ddb_is_valid(conn)) {
                ddb_disconnect(conn)
            }
            unlink(path)
        },
        add = TRUE
    )
    ddb_exec(conn, "CREATE TABLE lifecycle (value INTEGER)")
    ddb_exec(conn, "INSERT INTO lifecycle VALUES (1)")
    ddb_disconnect(conn)

    conn <- ddb_connect(path, read_only = TRUE)
    expect_equal(ddb_read_table(conn, "lifecycle")$value, 1L)
    expect_error(
        ddb_exec(conn, "INSERT INTO lifecycle VALUES (2)"),
        "[Rr]ead.only"
    )
})

# Pool lifecycle checks retain real concurrent execution and exercise shutdown
# immediately after cancellation or timeout, where native IPC waits occurred.
test_that("owned mirai pools preserve concurrency and survive interrupted tasks", {
    skip_on_cran()
    profile <- paste0("epwshiftr-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    mirai__start_pool(2L, .compute = profile)
    jobs <- lapply(seq_len(8L), function(i) {
        mirai::mirai(
            {
                Sys.sleep(0.03)
                Sys.getpid()
            },
            .compute = profile
        )
    })
    expect_length(unique(vapply(jobs, mirai::collect_mirai, integer(1L))), 2L)
    mirai::daemons(0L, .compute = profile)

    for (iteration in seq_len(20L)) {
        mirai__start_pool(1L, .compute = profile)
        job <- if (iteration %% 2L) {
            mirai::mirai(Sys.sleep(5), .timeout = 25, .compute = profile)
        } else {
            job <- mirai::mirai(Sys.sleep(5), .compute = profile)
            mirai::stop_mirai(job)
            job
        }
        expect_true(mirai::is_error_value(mirai::collect_mirai(job)))
        mirai::daemons(0L, .compute = profile)
        expect_identical(mirai::status(.compute = profile)$connections, 0L)
    }
})

# A persistent session retains its healthy slot when a different owned reader times out.
test_that("subset sessions reject busy reuse and replace only timed out readers", {
    skip_on_cran()
    session <- opendap__session(2L, .15)
    on.exit(session$close(), add = TRUE)
    pool <- session$acquire(2L, .15)
    identities <- vapply(pool$status(), function(s) s$pid, integer(1L))
    timed_out <- 0L
    # Keep the reader closure isolated from test environments during IPC serialization.
    read <- function(job) {
        Sys.sleep(job$delay)
        Sys.getpid()
    }
    environment(read) <- baseenv()
    ticket <- pool$submit(
        list(
            list(source_key = "slow", delay = 3),
            list(source_key = "fast", delay = 0)
        ),
        read = read,
        collect = function(job, value) NULL,
        on_error = function(job, error) {
            stopifnot(inherits(error, "epwshiftr_source_timeout"))
            timed_out <<- timed_out + 1L
        }
    )
    expect_error(session$acquire(2L, .15))
    deadline <- proc.time()[["elapsed"]] + 10
    while (ticket$status == "pending") {
        stopifnot(proc.time()[["elapsed"]] < deadline)
        pool$poll()
        Sys.sleep(.01)
    }
    expect_identical(timed_out, 1L)
    expect_identical(ticket$status, "done")
    expect_true(pool$idle())
    after <- vapply(pool$status(), function(s) s$pid, integer(1L))
    expect_false(identical(identities[[1L]], after[[1L]]))
    expect_identical(identities[[2L]], after[[2L]])
    expect_identical(session$acquire(2L, .15), pool)
})

# vim: fdm=marker :
