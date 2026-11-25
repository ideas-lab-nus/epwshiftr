# cache_disk_deterministic {{{
cache_disk_deterministic <- function(dir, ...) {
    cache <- DiskCache$new(dir = dir, ...)
    priv(cache)$set_count <- 0L
    cache
}
# }}}

# DiskCache initialization and config {{{
test_that("DiskCache$new() validates arguments and parses size/age strings", {
    for (bad_dir in list(NULL, c("a", "b"), "", NA_character_)) {
        expect_error(DiskCache$new(bad_dir), "'dir' must be a single string")
    }

    cache_dir <- tempfile("cache-invalid-")
    expect_error(
        DiskCache$new(cache_dir, max_size = -1),
        "'max_size' must be a non-negative number"
    )
    expect_error(
        DiskCache$new(cache_dir, max_age = -1),
        "'max_age' must be a non-negative number"
    )
    expect_error(
        DiskCache$new(cache_dir, max_n = -1),
        "'max_n' must be a non-negative number"
    )
    expect_error(
        DiskCache$new(cache_dir, prune_rate = 0),
        "'prune_rate' must be a positive integer"
    )
    expect_error(
        DiskCache$new(cache_dir, prune_rate = 1.5),
        "'prune_rate' must be a positive integer"
    )
    expect_error(
        DiskCache$new(cache_dir, prune_limit = -1),
        "'prune_limit' must be a non-negative number"
    )
    expect_error(
        DiskCache$new(cache_dir, prune_on_init = NA),
        "'prune_on_init' must be TRUE or FALSE"
    )
    expect_error(
        DiskCache$new(cache_dir, max_size = "invalid"),
        "When 'max_size' is a string"
    )
    expect_error(
        DiskCache$new(cache_dir, max_age = "invalid"),
        "When 'max_age' is a string"
    )

    size_cases <- list(
        "500 KB" = 500 * 1024,
        "100 MB" = 100 * 1024^2,
        "1 GB" = 1024^3
    )
    for (label in names(size_cases)) {
        cache <- DiskCache$new(cache_dir, max_size = label)
        expect_equal(cache$info()$max_size, size_cases[[label]])
        cache$destroy()
    }

    age_cases <- list(
        "100 secs" = 100,
        "30 mins" = 1800,
        "1 hour" = 3600,
        "2 days" = 2 * 24 * 3600
    )
    for (label in names(age_cases)) {
        cache <- DiskCache$new(cache_dir, max_age = label)
        expect_equal(cache$info()$max_age, age_cases[[label]])
        cache$destroy()
    }
})

test_that("DiskCache$new() persists metadata, detects config changes, and prunes on init", {
    skip_on_cran()
    cache_dir <- tempfile("cache-persist-")
    cache <- DiskCache$new(
        cache_dir,
        max_n = 10,
        max_size = 1000,
        prune_rate = 100,
        prune_on_init = FALSE
    )
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    for (k in c("a", "b", "c", "d", "e")) {
        cache$set(k, k)
        Sys.sleep(0.01)
    }
    expect_true(file.exists(file.path(cache_dir, ".metadata.rds")))
    rm(cache)
    gc()

    reloaded <- DiskCache$new(
        cache_dir,
        max_n = 10,
        max_size = 1000,
        prune_on_init = FALSE
    )
    expect_true(reloaded$exists("a"))
    rm(reloaded)
    gc()

    expect_message(
        pruned <- DiskCache$new(
            cache_dir,
            max_n = 3,
            max_size = 1000,
            prune_rate = 100,
            prune_on_init = TRUE
        ),
        "Cache configuration has changed"
    )
    expect_equal(pruned$size(), 3L)
    pruned$destroy()
})
# }}}

# DiskCache CRUD, types, key validation, and shared visibility {{{
test_that("DiskCache supports CRUD, overwrites, diverse R types, info(), and key validation", {
    cache_dir <- tempfile("cache-crud-")
    cache <- DiskCache$new(
        cache_dir,
        max_size = 1000,
        max_age = 3600,
        max_n = 10,
        prune_rate = 100,
        prune_limit = 5
    )
    on.exit(if (!cache$is_destroyed()) cache$destroy(), add = TRUE)

    expect_true(dir.exists(cache_dir))
    expect_s3_class(cache, "DiskCache")
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)
    expect_false(cache$exists("missing"))
    missing_val <- cache$get("missing")
    expect_true(cache__missing(missing_val))
    expect_s3_class(missing_val, "key_missing")

    # Empty cache no-ops
    cache$prune()
    cache$reset()
    expect_false(cache$remove("missing"))

    # Store and overwrite diverse R types and special strings
    env <- new.env()
    env$x <- 1
    values <- list(
        num = 123.456,
        int = 123L,
        char = "你好世界\nline2\ttab!@#$%^&*()",
        bool = TRUE,
        null = NULL,
        nested = list(a = 1, b = list(c = 2)),
        df = data.frame(x = 1:3, y = c("a", "b", "c")),
        mat = matrix(1:9, nrow = 3),
        fn = function(x) x + 1,
        env = env
    )
    for (nm in names(values)) {
        cache$set(nm, values[[nm]])
        expect_true(cache$exists(nm))
    }

    expect_false(cache__missing(cache$get("num")))
    expect_equal(cache$get("num"), 123.456)
    expect_equal(cache$get("char"), values$char)
    expect_null(cache$get("null"))
    expect_equal(cache$get("df"), values$df)
    expect_equal(cache$get("mat"), values$mat)
    expect_equal(cache$get("fn")(5), 6)
    expect_equal(cache$get("env")$x, 1)

    # Atomic overwrite
    cache$set("num", rnorm(2000))
    expect_length(cache$get("num"), 2000L)

    expect_equal(cache$size(), length(values))
    expect_setequal(cache$keys(), names(values))

    info <- cache$info()
    expect_named(
        info,
        c(
            "dir",
            "max_size",
            "max_age",
            "max_n",
            "prune_rate",
            "prune_limit",
            "n",
            "size"
        )
    )
    expect_equal(info$n, length(values))
    expect_gt(info$size, 0)

    # Remove & reset
    expect_true(cache$remove("num"))
    expect_false(cache$exists("num"))
    cache$reset()
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)

    # Key validation
    expect_error(cache$get(123), "Key must be a single string")
    expect_error(cache$get(c("a", "b")), "Key must be a single string")
    expect_error(cache$set(NULL, 1), "Key must be a single string")
    expect_error(cache$get(""), "Key must not be empty")
    expect_error(
        cache$get(strrep("a", 81L)),
        "Key must be shorter than 80 characters"
    )
    for (bad_key in c("a/b", "a:b", "a*b")) {
        expect_error(
            cache$get(bad_key),
            "Key must not contain any of the following characters"
        )
    }
})

test_that("DiskCache instances observe shared directory writes", {
    skip_on_cran()
    cache_dir <- tempfile("cache-shared-")
    cache1 <- DiskCache$new(cache_dir, prune_rate = 100)
    cache2 <- DiskCache$new(cache_dir, prune_rate = 100)
    on.exit(if (!cache1$is_destroyed()) cache1$destroy(), add = TRUE)

    cache1$set("k1", 100)
    cache2$set("k2", 200)
    expect_true(cache1$exists("k2"))
    expect_equal(cache2$get("k1"), 100)
})
# }}}

# DiskCache pruning, expiry, and throttling {{{
test_that("DiskCache$prune() enforces max_n, max_size, and combined limits", {
    skip_on_cran()
    delay <- 0.01

    dir_n <- tempfile("cache-prune-n-")
    cache_n <- cache_disk_deterministic(dir_n, max_n = 3, prune_rate = 1)
    on.exit(cache_n$destroy(), add = TRUE)
    for (k in c("a", "b", "c", "d", "e")) {
        cache_n$set(k, rnorm(100))
        Sys.sleep(delay)
    }
    cache_n$prune()
    expect_equal(sort(cache_n$keys()), c("c", "d", "e"))

    dir_sz <- tempfile("cache-prune-size-")
    cache_sz <- cache_disk_deterministic(
        dir_sz,
        max_n = 3,
        max_size = 200,
        prune_rate = 1
    )
    on.exit(cache_sz$destroy(), add = TRUE)
    for (k in c("a", "b", "c", "d")) {
        cache_sz$set(k, rnorm(100))
        Sys.sleep(delay)
    }
    cache_sz$set("small", 1)
    Sys.sleep(delay)
    cache_sz$prune()
    expect_equal(cache_sz$keys(), "small")

    cache_sz$set("too_big", rnorm(100))
    Sys.sleep(delay)
    cache_sz$prune()
    expect_length(cache_sz$keys(), 0L)
})

test_that("DiskCache honors max_age expiry and prune_rate/prune_limit throttling", {
    skip_on_cran()
    delay <- 0.01

    # max_age on get() and prune()
    dir_age <- tempfile("cache-age-")
    cache_age <- cache_disk_deterministic(
        dir_age,
        max_age = 0.5,
        prune_rate = 1
    )
    on.exit(cache_age$destroy(), add = TRUE)
    cache_age$set("a", 1)
    cache_age$set("b", 2)
    expect_equal(cache_age$get("a"), 1)
    Sys.sleep(0.6)
    expect_true(cache__missing(cache_age$get("a")))
    expect_false(cache_age$exists("a"))
    cache_age$prune()
    expect_equal(cache_age$size(), 0L)

    # prune_rate throttling
    dir_rate <- tempfile("cache-rate-")
    cache_slow <- cache_disk_deterministic(dir_rate, max_n = 2, prune_rate = 20)
    for (k in c("a", "b", "c", "d")) {
        cache_slow$set(k, 1)
        Sys.sleep(delay)
    }
    expect_equal(sort(cache_slow$keys()), c("a", "b", "c", "d"))
    cache_slow$destroy()

    cache_fast <- cache_disk_deterministic(dir_rate, max_n = 2, prune_rate = 2)
    on.exit(cache_fast$destroy(), add = TRUE)
    for (k in c("a", "b", "c", "d")) {
        cache_fast$set(k, 1)
        Sys.sleep(delay)
    }
    expect_equal(sort(cache_fast$keys()), c("c", "d"))

    # prune_limit time throttling
    dir_lim <- tempfile("cache-limit-")
    cache_lim <- cache_disk_deterministic(
        dir_lim,
        max_n = 2,
        prune_rate = 100,
        prune_limit = 1
    )
    on.exit(cache_lim$destroy(), add = TRUE)
    cache_lim$set("a", 1)
    cache_lim$set("b", 1)
    cache_lim$set("c", 1)
    expect_equal(cache_lim$size(), 3L)
    Sys.sleep(1.1)
    cache_lim$set("d", 1)
    expect_equal(sort(cache_lim$keys()), c("c", "d"))
})
# }}}

# DiskCache$destroy() and $print() {{{
test_that("DiskCache$destroy() removes directory and blocks further operations", {
    cache_dir <- tempfile("cache-destroy-")
    cache <- DiskCache$new(cache_dir, prune_rate = 100)
    cache$set("a", 1)

    expect_false(cache$is_destroyed())
    expect_true(cache$destroy())
    expect_true(cache$is_destroyed())
    expect_false(dir.exists(cache_dir))
    expect_false(cache$destroy())

    expect_error(cache$get("key"), "Cache .* has been destroyed")
    expect_error(cache$set("key", 1), "Cache .* has been destroyed")
    expect_error(cache$exists("key"), "Cache .* has been destroyed")
    expect_error(cache$remove("key"), "Cache .* has been destroyed")
    expect_error(cache$reset(), "Cache .* has been destroyed")
    expect_error(cache$keys(), "Cache .* has been destroyed")
    expect_error(cache$size(), "Cache .* has been destroyed")
    expect_error(cache$prune(), "Cache .* has been destroyed")
})

test_that("DiskCache$print()", {
    cache_dir <- tempfile("cache-print-")
    cache <- DiskCache$new(cache_dir)
    on.exit(cache$destroy(), add = TRUE)

    expect_snapshot(
        print(cache),
        transform = function(lines) {
            lines <- gsub("^(\\s*)dir: .+$", "\\1dir: <cache-dir>", lines)
            lines <- gsub(
                "^(\\s*)last_prune_time: .+$",
                "\\1last_prune_time: <time>",
                lines
            )
            gsub("^(\\s*)set_count: .+$", "\\1set_count: <count>", lines)
        }
    )
})
# }}}

# cache__mode(), cache__offline(), cache__key(), and singleton lifecycle {{{
test_that("cache__mode(), cache__offline(), and cache__key() work across modes", {
    local_cache_mode("normal")
    expect_equal(cache__mode(), "normal")
    expect_false(cache__offline())

    local_cache_mode("off")
    expect_equal(cache__mode(), "off")
    expect_false(cache__offline())

    local_cache_mode("offline")
    expect_equal(cache__mode(), "offline")
    expect_true(cache__offline())

    withr::local_options(epwshiftr.cache = "bogus")
    expect_warning(result <- cache__mode(), "Unknown")
    expect_equal(result, "normal")

    k1 <- cache__key("myprefix", "a", "b")
    expect_identical(k1, cache__key("myprefix", "a", "b"))
    expect_match(k1, "^myprefix-[0-9a-f]{8}$")
    expect_false(identical(k1, cache__key("myprefix", "a", "c")))
    expect_false(identical(k1, cache__key("other", "a", "b")))
})

test_that("cache__set(), cache__get(), and cache__reset() manage the global cache", {
    original <- cache__set(NULL)
    cache_dir <- tempfile("epwshiftr-dir-cache-")
    dir1 <- tempfile("cache-set1-")
    expected_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    withr::defer({
        cache__reset()
        unlink(c(cache_dir, dir1), recursive = TRUE)
        cache__set(original)
    })

    # cache__get() initializes lazily from epwshiftr.dir_cache
    expect_false(dir.exists(cache_dir))
    default_cache <- cache__get()
    expect_s3_class(default_cache, "DiskCache")
    expect_true(dir.exists(cache_dir))
    expect_identical(default_cache$info()$dir, expected_dir)

    # cache__set() swaps and returns previous cache
    cache1 <- DiskCache$new(dir = dir1, max_size = "100 MB")
    old <- cache__set(cache1)
    expect_identical(old, default_cache)
    expect_identical(cache__get(), cache1)

    # cache__reset() clears singleton so next cache__get() builds a new instance
    cache__reset()
    rebuilt <- cache__get()
    expect_s3_class(rebuilt, "DiskCache")
    expect_false(identical(rebuilt, cache1))
})
# }}}

# cache__read_json() {{{
test_that("cache__read_json() handles modes, Solr null maxScore, non-strict failures, and long URLs", {
    cache <- local_test_cache()
    local_cache_mode("off")

    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    jsonlite::write_json(
        list(response = list(numFound = 1L, docs = list())),
        path,
        auto_unbox = TRUE
    )

    res <- cache__read_json(path, cache = TRUE, simplifyVector = FALSE)
    expect_equal(res$response$numFound, 1L)
    expect_equal(cache$size(), 1L)

    offline <- cache__read_json(path, cache = "offline", simplifyVector = FALSE)
    expect_equal(offline$response$numFound, 1L)
    expect_error(
        cache__read_json(tempfile(fileext = ".json"), cache = "offline"),
        "Cache miss in offline mode"
    )

    # Empty Solr maxScore normalization
    empty_solr <- suppressWarnings(cache__read_json(
        '{"response":{"numFound":0,"start":0,"docs":[],"maxScore":null}}',
        cache = FALSE,
        simplifyVector = FALSE
    ))
    expect_identical(empty_solr$response$numFound, 0L)
    expect_identical(empty_solr$response$maxScore, 0)

    # Non-strict invalid JSON is not cached
    local_cache_mode("normal")
    cache$reset()
    expect_warning(
        bad <- cache__read_json("not valid json", strict = FALSE, cache = TRUE),
        "Failed to read the JSON response"
    )
    expect_null(bad)
    expect_equal(cache$size(), 0L)

    # Long HTTP URLs (>2084 bytes) routed via curl with progress callbacks
    long_url <- paste0(
        "https://example.org/esg-search/search?",
        strrep("a", 2100L)
    )
    seen <- NULL
    states <- character()
    testthat::local_mocked_bindings(
        curl_fetch_memory = function(url, handle) {
            seen <<- list(url = url, handle = handle)
            list(
                content = charToRaw('{"response":{"numFound":1,"docs":[]}}'),
                status_code = 200L,
                headers = raw(),
                url = url
            )
        },
        .package = "curl"
    )

    long_res <- cache__read_json(
        long_url,
        simplifyVector = FALSE,
        progress_callback = function(progress) {
            states <<- c(states, progress$state)
        }
    )
    expect_equal(seen$url, long_url)
    expect_s3_class(seen$handle, "curl_handle")
    expect_equal(states, c("started", "completed", "parsed"))
    expect_equal(long_res$response$numFound, 1L)
    expect_equal(cache$size(), 1L)
})
# }}}

# cache__url() and cache__download() {{{
test_that("cache__url() handles off, normal, offline modes and validate predicates", {
    local_test_cache()

    calls <- 0L
    fetch_val <- function() {
        calls <<- calls + 1L
        list(data = 42)
    }

    local_cache_mode("off")
    expect_equal(cache__url("test", "k1", fetch_val)$data, 42)
    expect_equal(cache__url("test", "k1", fetch_val)$data, 42)
    expect_equal(calls, 2L)

    local_cache_mode("normal")
    expect_equal(cache__url("test", "k1", fetch_val)$data, 42)
    expect_equal(cache__url("test", "k1", fetch_val)$data, 42)
    expect_equal(calls, 3L)
    expect_equal(cache__url("test", "k2", fetch_val)$data, 42)
    expect_equal(calls, 4L)

    # validate predicate vs default NULL
    null_calls <- 0L
    fetch_null <- function() {
        null_calls <<- null_calls + 1L
        NULL
    }
    expect_null(cache__url("test", "vkey", fetch_null, validate = function(x) {
        !is.null(x)
    }))
    expect_null(cache__url("test", "vkey", fetch_null, validate = function(x) {
        !is.null(x)
    }))
    expect_equal(null_calls, 2L)

    expect_null(cache__url("test", "nkey", fetch_null))
    expect_null(cache__url("test", "nkey", fetch_null))
    expect_equal(null_calls, 3L)

    local_cache_mode("offline")
    expect_equal(
        cache__url("test", "k1", function() stop("should not run"))$data,
        42
    )
    expect_error(
        cache__url("test", "missing", function() stop("should not run")),
        "offline"
    )
})

test_that("cache__download() handles off, normal, and offline modes", {
    local_test_cache()
    f1 <- tempfile("dl-1-")
    f2 <- tempfile("dl-2-")
    f3 <- tempfile("dl-3-")
    on.exit(unlink(c(f1, f2, f3)), add = TRUE)

    calls <- 0L
    dl <- function(dest) {
        function() {
            calls <<- calls + 1L
            writeBin(charToRaw("downloaded data"), dest)
            dest
        }
    }

    local_cache_mode("off")
    expect_equal(cache__download("http://example.com/a.bin", f1, dl(f1)), f1)
    expect_equal(calls, 1L)

    local_cache_mode("normal")
    expect_equal(cache__download("http://example.com/data.bin", f1, dl(f1)), f1)
    expect_equal(calls, 2L)
    unlink(f1)
    expect_equal(
        cache__download("http://example.com/data.bin", f2, function() {
            stop("should not run")
        }),
        f2
    )
    expect_equal(readBin(f2, "raw", 100), charToRaw("downloaded data"))

    local_cache_mode("offline")
    expect_equal(
        cache__download("http://example.com/data.bin", f3, function() {
            stop("should not run")
        }),
        f3
    )
    expect_equal(readBin(f3, "raw", 100), charToRaw("downloaded data"))
    expect_error(
        cache__download(
            "http://example.com/missing.bin",
            tempfile(),
            function() stop("no")
        ),
        "offline"
    )
})
# }}}

# vim: fdm=marker :
