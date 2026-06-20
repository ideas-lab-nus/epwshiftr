# cache_disk_deterministic() {{{
cache_disk_deterministic <- function(dir, ...) {
    cache <- DiskCache$new(dir = dir, ...)
    cache$.__enclos_env__$private$set_count <- 0L
    cache
}
# }}}
# DiskCache$new() {{{
test_that("DiskCache$new()", {
    cache_dir <- tempfile("cache-init-")
    cache <- DiskCache$new(cache_dir)

    expect_true(dir.exists(cache_dir))
    expect_s3_class(cache, "DiskCache")
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)

    cache$destroy()
})

test_that("DiskCache$new() validates parameters", {
    expect_error(DiskCache$new(NULL), "'dir' must be a single string")
    expect_error(DiskCache$new(c("a", "b")), "'dir' must be a single string")
    expect_error(DiskCache$new(""), "'dir' must be a single string")
    expect_error(DiskCache$new(NA_character_), "'dir' must be a single string")

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
})

test_that("DiskCache$new() parses human-readable max_size", {
    cache_dir <- tempfile("cache-size-")

    cache <- DiskCache$new(cache_dir, max_size = "100 MB")
    expect_equal(cache$info()$max_size, 100 * 1024^2)
    cache$destroy()

    cache <- DiskCache$new(cache_dir, max_size = "1 GB")
    expect_equal(cache$info()$max_size, 1024^3)
    cache$destroy()

    cache <- DiskCache$new(cache_dir, max_size = "500 KB")
    expect_equal(cache$info()$max_size, 500 * 1024)
    cache$destroy()

    expect_error(
        DiskCache$new(cache_dir, max_size = "invalid"),
        "When 'max_size' is a string"
    )
})

test_that("DiskCache$new() parses human-readable max_age", {
    cache_dir <- tempfile("cache-age-")

    cache <- DiskCache$new(cache_dir, max_age = "1 hour")
    expect_equal(cache$info()$max_age, 3600)
    cache$destroy()

    cache <- DiskCache$new(cache_dir, max_age = "30 mins")
    expect_equal(cache$info()$max_age, 1800)
    cache$destroy()

    cache <- DiskCache$new(cache_dir, max_age = "2 days")
    expect_equal(cache$info()$max_age, 2 * 24 * 3600)
    cache$destroy()

    cache <- DiskCache$new(cache_dir, max_age = "100 secs")
    expect_equal(cache$info()$max_age, 100)
    cache$destroy()

    expect_error(
        DiskCache$new(cache_dir, max_age = "invalid"),
        "When 'max_age' is a string"
    )
})

test_that("DiskCache$new() detects configuration changes", {
    skip_on_cran()

    cache_dir <- tempfile("cache-config-")
    cache <- DiskCache$new(cache_dir, max_n = 10, prune_on_init = FALSE)

    cache$set("a", 1)
    cache$set("b", 2)

    rm(cache)
    gc()

    expect_message(
        cache <- DiskCache$new(cache_dir, max_n = 5, prune_on_init = FALSE),
        "Cache configuration has changed"
    )

    cache$destroy()
})

test_that("DiskCache$new(prune_on_init = TRUE)", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-prune-init-")
    cache <- DiskCache$new(cache_dir, max_n = 3, prune_rate = 100, prune_on_init = FALSE)

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 2); Sys.sleep(delay)
    cache$set("c", 3); Sys.sleep(delay)
    cache$set("d", 4); Sys.sleep(delay)
    cache$set("e", 5); Sys.sleep(delay)

    cache$prune()
    expect_equal(cache$size(), 3L)

    rm(cache)
    gc()

    cache <- DiskCache$new(cache_dir, max_n = 3, prune_rate = 100, prune_on_init = TRUE)
    expect_equal(cache$size(), 3L)

    cache$destroy()
})

test_that("DiskCache$new() loads persisted metadata", {
    skip_on_cran()

    cache_dir <- tempfile("cache-metadata-")
    cache <- DiskCache$new(cache_dir, max_n = 10, max_size = 1000)

    cache$set("a", 1)

    metadata_file <- file.path(cache_dir, ".metadata.rds")
    expect_true(file.exists(metadata_file))

    rm(cache)
    gc()

    cache <- DiskCache$new(cache_dir, max_n = 10, max_size = 1000, prune_on_init = FALSE)
    expect_true(cache$exists("a"))

    cache$destroy()
})
# }}}
# DiskCache$get() {{{
test_that("DiskCache$get() returns cache__missing() for missing keys", {
    cache_dir <- tempfile("cache-missing-")
    cache <- DiskCache$new(cache_dir)

    expect_true(cache__missing(cache$get("abcd")))

    cache$destroy()
})

test_that("DiskCache$get() reads stored values", {
    cache_dir <- tempfile("cache-basic-get-")
    cache <- DiskCache$new(cache_dir)

    cache$set("key1", 123)
    cache$set("key2", "hello")
    cache$set("key3", list(a = 1, b = 2))
    cache$set("key1", 456)

    expect_equal(cache$get("key1"), 456)
    expect_equal(cache$get("key2"), "hello")
    expect_equal(cache$get("key3"), list(a = 1, b = 2))

    cache$destroy()
})

test_that("DiskCache$get() honors max_age", {
    skip_on_cran()

    cache_dir <- tempfile("cache-age-get-")
    cache <- DiskCache$new(cache_dir, max_age = 0.5)

    cache$set("key1", 123)
    expect_equal(cache$get("key1"), 123)

    Sys.sleep(0.6)

    expect_true(cache__missing(cache$get("key1")))
    expect_false(cache$exists("key1"))

    cache$destroy()
})

test_that("DiskCache$get() rejects invalid keys", {
    cache_dir <- tempfile("cache-get-key-validation-")
    cache <- DiskCache$new(cache_dir)

    expect_error(cache$get(123), "Key must be a single string")
    expect_error(cache$get(c("a", "b")), "Key must be a single string")
    expect_error(cache$get(""), "Key must not be empty")

    long_key <- paste(rep("a", 81), collapse = "")
    expect_error(cache$get(long_key), "Key must be shorter than 80 characters")

    expect_error(cache$get("key/with/slash"), "Key must not contain any of the following characters")
    expect_error(cache$get("key:with:colon"), "Key must not contain any of the following characters")
    expect_error(cache$get("key*with*star"), "Key must not contain any of the following characters")

    cache$destroy()
})
# }}}
# DiskCache$set() {{{
test_that("DiskCache$set() stores values", {
    cache_dir <- tempfile("cache-basic-set-")
    cache <- DiskCache$new(cache_dir)

    cache$set("key1", 123)
    cache$set("key2", "hello")
    cache$set("key3", list(a = 1, b = 2))

    expect_true(cache$exists("key1"))
    expect_true(cache$exists("key2"))
    expect_true(cache$exists("key3"))

    cache$destroy()
})

test_that("DiskCache$set() rejects invalid keys", {
    cache_dir <- tempfile("cache-set-key-validation-")
    cache <- DiskCache$new(cache_dir)

    expect_error(cache$set(NULL, 1), "Key must be a single string")

    cache$destroy()
})

test_that("DiskCache$set() stores different data types", {
    cache_dir <- tempfile("cache-types-")
    cache <- DiskCache$new(cache_dir, prune_rate = 100)

    cache$set("num", 123.456)
    expect_equal(cache$get("num"), 123.456)

    cache$set("int", 123L)
    expect_equal(cache$get("int"), 123L)

    cache$set("char", "hello world")
    expect_equal(cache$get("char"), "hello world")

    cache$set("bool", TRUE)
    expect_equal(cache$get("bool"), TRUE)

    cache$set("null", NULL)
    expect_null(cache$get("null"))

    cache$set("list", list(a = 1, b = "two", c = list(d = 3)))
    expect_equal(cache$get("list"), list(a = 1, b = "two", c = list(d = 3)))

    df <- data.frame(x = 1:3, y = c("a", "b", "c"))
    cache$set("df", df)
    expect_equal(cache$get("df"), df)

    mat <- matrix(1:9, nrow = 3)
    cache$set("mat", mat)
    expect_equal(cache$get("mat"), mat)

    fn <- function(x) x + 1
    cache$set("fn", fn)
    expect_equal(cache$get("fn")(5), 6)

    env <- new.env()
    env$x <- 1
    cache$set("env", env)
    expect_equal(cache$get("env")$x, 1)

    cache$destroy()
})

test_that("DiskCache$set() uses atomic writes", {
    cache_dir <- tempfile("cache-atomic-")
    cache <- DiskCache$new(cache_dir)

    cache$set("key1", rnorm(1000))
    expect_length(cache$get("key1"), 1000)

    cache$set("key1", rnorm(2000))
    expect_length(cache$get("key1"), 2000)

    cache$destroy()
})

test_that("DiskCache$set() preserves special values", {
    cache_dir <- tempfile("cache-special-")
    cache <- DiskCache$new(cache_dir)

    cache$set("unicode", "你好世界")
    expect_equal(cache$get("unicode"), "你好世界")

    cache$set("special", "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    expect_equal(cache$get("special"), "!@#$%^&*()_+-=[]{}|;':\",./<>?")

    cache$set("whitespace", "line1\nline2\ttab")
    expect_equal(cache$get("whitespace"), "line1\nline2\ttab")

    cache$destroy()
})
# }}}
# DiskCache$exists() {{{
test_that("DiskCache$exists()", {
    cache_dir <- tempfile("cache-exists-")
    cache <- DiskCache$new(cache_dir)

    expect_false(cache$exists("key1"))
    cache$set("key1", 123)
    expect_true(cache$exists("key1"))

    cache$remove("key1")
    expect_false(cache$exists("key1"))

    cache$destroy()
})

test_that("DiskCache$exists() observes shared directories", {
    skip_on_cran()

    cache_dir <- tempfile("cache-concurrent-")
    cache1 <- DiskCache$new(cache_dir, prune_rate = 100)
    cache2 <- DiskCache$new(cache_dir, prune_rate = 100)

    cache1$set("key1", 100)
    cache2$set("key2", 200)

    expect_true(cache1$exists("key1"))
    expect_true(cache1$exists("key2"))
    expect_true(cache2$exists("key1"))
    expect_true(cache2$exists("key2"))

    cache1$destroy()
})
# }}}
# DiskCache$keys() {{{
test_that("DiskCache$keys()", {
    cache_dir <- tempfile("cache-keys-")
    cache <- DiskCache$new(cache_dir)

    expect_length(cache$keys(), 0L)

    cache$set("a", 1)
    cache$set("b", 2)
    cache$set("c", 3)

    expect_length(cache$keys(), 3L)
    expect_setequal(cache$keys(), c("a", "b", "c"))

    cache$destroy()
})

test_that("DiskCache$keys() handles many keys", {
    skip_on_cran()

    cache_dir <- tempfile("cache-many-keys-")
    cache <- DiskCache$new(cache_dir, prune_rate = 200)

    n <- 100
    for (i in seq_len(n)) {
        cache$set(paste0("key", i), i)
    }

    expect_length(cache$keys(), n)

    for (i in seq_len(n)) {
        expect_true(cache$exists(paste0("key", i)))
        expect_equal(cache$get(paste0("key", i)), i)
    }

    cache$destroy()
})
# }}}
# DiskCache$remove() {{{
test_that("DiskCache$remove()", {
    cache_dir <- tempfile("cache-remove-")
    cache <- DiskCache$new(cache_dir)

    cache$set("key1", 123)
    expect_true(cache$exists("key1"))

    result <- cache$remove("key1")
    expect_true(result)
    expect_false(cache$exists("key1"))

    result <- cache$remove("nonexistent")
    expect_false(result)

    cache$destroy()
})
# }}}
# DiskCache$reset() {{{
test_that("DiskCache$reset()", {
    cache_dir <- tempfile("cache-reset-")
    cache <- DiskCache$new(cache_dir)

    cache$set("a", 1)
    cache$set("b", 2)
    cache$set("c", 3)
    expect_equal(cache$size(), 3L)

    cache$reset()
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)

    cache$destroy()
})

test_that("DiskCache$reset() handles empty caches", {
    cache_dir <- tempfile("cache-empty-reset-")
    cache <- DiskCache$new(cache_dir)

    cache$reset()
    expect_equal(cache$size(), 0L)

    cache$destroy()
})
# }}}
# DiskCache$size() {{{
test_that("DiskCache$size()", {
    cache_dir <- tempfile("cache-size-method-")
    cache <- DiskCache$new(cache_dir)

    expect_equal(cache$size(), 0L)

    cache$set("a", 1)
    cache$set("b", 2)
    cache$set("c", 3)

    expect_equal(cache$size(), 3L)

    cache$destroy()
})

test_that("DiskCache$size() handles many keys", {
    skip_on_cran()

    cache_dir <- tempfile("cache-many-size-")
    cache <- DiskCache$new(cache_dir, prune_rate = 200)

    n <- 100
    for (i in seq_len(n)) {
        cache$set(paste0("key", i), i)
    }

    expect_equal(cache$size(), n)

    cache$destroy()
})
# }}}
# DiskCache$prune() {{{
test_that("DiskCache$prune() respects max_n", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-prune-n-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 3, prune_rate = 1)

    cache$set("a", rnorm(100)); Sys.sleep(delay)
    cache$set("b", rnorm(100)); Sys.sleep(delay)
    cache$set("c", rnorm(100)); Sys.sleep(delay)
    cache$set("d", rnorm(100)); Sys.sleep(delay)
    cache$set("e", rnorm(100)); Sys.sleep(delay)
    cache$prune()

    expect_equal(sort(cache$keys()), c("c", "d", "e"))

    cache$destroy()
})

test_that("DiskCache$prune() respects max_size", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-prune-size-")
    cache <- cache_disk_deterministic(cache_dir, max_size = 200, prune_rate = 1)

    cache$set("a", rnorm(100)); Sys.sleep(delay)
    cache$set("b", rnorm(100)); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$prune()

    expect_equal(sort(cache$keys()), "c")

    cache$set("d", rnorm(100)); Sys.sleep(delay)
    cache$prune()
    expect_length(cache$keys(), 0L)

    cache$set("e", 2); Sys.sleep(delay)
    cache$set("f", 3); Sys.sleep(delay)
    cache$prune()
    expect_equal(sort(cache$keys()), c("e", "f"))

    cache$destroy()
})

test_that("DiskCache$prune() respects max_n and max_size", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-prune-both-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 3, max_size = 200, prune_rate = 1)

    cache$set("a", rnorm(100)); Sys.sleep(delay)
    cache$set("b", rnorm(100)); Sys.sleep(delay)
    cache$set("c", rnorm(100)); Sys.sleep(delay)
    cache$set("d", rnorm(100)); Sys.sleep(delay)
    cache$set("e", rnorm(100)); Sys.sleep(delay)
    cache$set("f", 1); Sys.sleep(delay)
    cache$prune()

    expect_equal(cache$keys(), "f")

    cache$destroy()
})

test_that("DiskCache$prune() respects max_age", {
    skip_on_cran()

    cache_dir <- tempfile("cache-prune-age-")
    cache <- cache_disk_deterministic(cache_dir, max_age = 0.5, prune_rate = 1)

    cache$set("a", 1)
    cache$set("b", 2)

    expect_equal(cache$size(), 2L)

    Sys.sleep(0.6)

    cache$prune()
    expect_equal(cache$size(), 0L)

    cache$destroy()
})

test_that("DiskCache$prune() honors prune_rate throttling", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-throttle-rate-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 20)

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$set("d", 1); Sys.sleep(delay)

    expect_equal(sort(cache$keys()), c("a", "b", "c", "d"))

    cache$destroy()
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 2)

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$set("d", 1); Sys.sleep(delay)

    expect_equal(sort(cache$keys()), c("c", "d"))

    cache$destroy()
})

test_that("DiskCache$prune() honors prune_limit throttling", {
    skip_on_cran()

    cache_dir <- tempfile("cache-throttle-limit-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 100, prune_limit = 1)

    cache$set("a", 1)
    cache$set("b", 1)
    cache$set("c", 1)

    expect_equal(cache$size(), 3L)

    Sys.sleep(1.1)

    cache$set("d", 1)

    expect_equal(sort(cache$keys()), c("c", "d"))

    cache$destroy()
})

test_that("DiskCache$prune() handles empty caches", {
    cache_dir <- tempfile("cache-empty-prune-")
    cache <- DiskCache$new(cache_dir)

    cache$prune()
    expect_equal(cache$size(), 0L)

    cache$destroy()
})
# }}}
# DiskCache$info() {{{
test_that("DiskCache$info()", {
    cache_dir <- tempfile("cache-info-")
    cache <- DiskCache$new(
        cache_dir,
        max_size = 1000,
        max_age = 3600,
        max_n = 10,
        prune_rate = 20,
        prune_limit = 5
    )

    info <- cache$info()
    expect_type(info, "list")
    expect_named(info, c("dir", "max_size", "max_age", "max_n", "prune_rate", "prune_limit", "n", "size"))
    expect_equal(info$max_size, 1000)
    expect_equal(info$max_age, 3600)
    expect_equal(info$max_n, 10)
    expect_equal(info$prune_rate, 20)
    expect_equal(info$prune_limit, 5)
    expect_equal(info$n, 0L)

    cache$set("a", 1)
    info <- cache$info()
    expect_equal(info$n, 1L)
    expect_true(info$size > 0)

    cache$destroy()
})

test_that("DiskCache$info() handles empty caches", {
    cache_dir <- tempfile("cache-empty-info-")
    cache <- DiskCache$new(cache_dir)

    info <- cache$info()
    expect_equal(info$n, 0L)
    expect_equal(info$size, 0L)

    cache$destroy()
})
# }}}
# DiskCache$destroy() / DiskCache$is_destroyed() {{{
test_that("DiskCache$destroy() / DiskCache$is_destroyed()", {
    cache_dir <- tempfile("cache-destroy-")
    cache <- DiskCache$new(cache_dir, prune_rate = 100)

    cache$set("a", 1)
    cache$set("b", 2)

    expect_false(cache$is_destroyed())
    expect_true(dir.exists(cache_dir))

    result <- cache$destroy()
    expect_true(result)
    expect_true(cache$is_destroyed())
    expect_false(dir.exists(cache_dir))

    result <- cache$destroy()
    expect_false(result)
})

test_that("DiskCache$get() / DiskCache$set() / DiskCache$exists() / DiskCache$remove() / DiskCache$reset() / DiskCache$keys() / DiskCache$size() / DiskCache$prune() reject operations after destroy", {
    cache_dir <- tempfile("cache-after-destroy-")
    cache <- DiskCache$new(cache_dir)

    cache$destroy()

    expect_error(cache$get("key"), "Cache .* has been destroyed")
    expect_error(cache$set("key", 1), "Cache .* has been destroyed")
    expect_error(cache$exists("key"), "Cache .* has been destroyed")
    expect_error(cache$remove("key"), "Cache .* has been destroyed")
    expect_error(cache$reset(), "Cache .* has been destroyed")
    expect_error(cache$keys(), "Cache .* has been destroyed")
    expect_error(cache$size(), "Cache .* has been destroyed")
    expect_error(cache$prune(), "Cache .* has been destroyed")
})
# }}}
# DiskCache$print() {{{
test_that("DiskCache$print()", {
    cache_dir <- tempfile("cache-print-")
    cache <- DiskCache$new(cache_dir)
    on.exit(cache$destroy(), add = TRUE)

    expect_snapshot(
        print(cache),
        transform = function(lines) {
            lines <- gsub("^(\\s*)dir: .+$", "\\1dir: <cache-dir>", lines)
            lines <- gsub("^(\\s*)last_prune_time: .+$", "\\1last_prune_time: <time>", lines)
            gsub("^(\\s*)set_count: .+$", "\\1set_count: <count>", lines)
        }
    )
})
# }}}
# cache__missing() {{{
test_that("cache__missing()", {
    cache_dir <- tempfile("cache-key-missing-")
    cache <- DiskCache$new(cache_dir)

    result <- cache$get("nonexistent")
    expect_true(cache__missing(result))
    expect_s3_class(result, "key_missing")

    cache$set("exists", 123)
    result <- cache$get("exists")
    expect_false(cache__missing(result))

    cache$destroy()
})
# }}}
# cache__mode() {{{
test_that("cache__mode() returns correct mode", {
    local_cache_mode("normal")
    expect_equal(cache__mode(), "normal")

    local_cache_mode("off")
    expect_equal(cache__mode(), "off")

    local_cache_mode("offline")
    expect_equal(cache__mode(), "offline")

    withr::local_options(epwshiftr.cache = "bogus")
    expect_warning(result <- cache__mode(), "Unknown")
    expect_equal(result, "normal")
})
# }}}
# cache__offline() {{{
test_that("cache__offline() returns correct values", {
    local_cache_mode("normal")
    expect_false(cache__offline())

    local_cache_mode("off")
    expect_false(cache__offline())

    local_cache_mode("offline")
    expect_true(cache__offline())
})
# }}}
# cache__read_json() {{{
test_that("cache__read_json() honors explicit cache mode", {
    cache <- local_test_cache()
    local_cache_mode("off")

    path <- tempfile(fileext = ".json")
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
})

test_that("cache__read_json() parses long HTTP URLs through an explicit connection", {
    cache <- local_test_cache()
    local_cache_mode("normal")
    long_url <- paste0("https://example.org/esg-search/search?", paste(rep("a", 2100), collapse = ""))
    seen <- NULL

    testthat::local_mocked_bindings(
        fromJSON = function(txt, bigint_as_char = FALSE, ...) {
            seen <<- list(
                is_connection = inherits(txt, "connection"),
                is_character = is.character(txt),
                bigint_as_char = bigint_as_char
            )
            close(txt)
            list(response = list(numFound = 1L, docs = list()))
        },
        .package = "jsonlite"
    )

    expect_gt(nchar(long_url, type = "bytes"), 2084L)
    res <- cache__read_json(long_url, simplifyVector = FALSE)
    expect_true(seen$is_connection)
    expect_false(seen$is_character)
    expect_true(seen$bigint_as_char)
    expect_equal(res$response$numFound, 1L)
    expect_equal(cache$size(), 1L)
})

test_that("cache__read_json() does not cache failed non-strict reads", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    expect_warning(
        res <- cache__read_json("not valid json", strict = FALSE, cache = TRUE),
        "Failed to read the JSON response"
    )
    expect_null(res)
    expect_equal(cache$size(), 0L)
})
# }}}
# cache__key() {{{
test_that("cache__key() is deterministic", {
    key1 <- cache__key("test", "a", "b")
    key2 <- cache__key("test", "a", "b")
    expect_equal(key1, key2)
})

test_that("cache__key() produces different keys for different inputs", {
    key1 <- cache__key("test", "a", "b")
    key2 <- cache__key("test", "a", "c")
    expect_false(key1 == key2)

    key3 <- cache__key("other", "a", "b")
    expect_false(key1 == key3)
})

test_that("cache__key() has correct format", {
    key <- cache__key("myprefix", "data")
    expect_match(key, "^myprefix-[0-9a-f]{8}$")
})
# }}}
# cache__url() {{{
test_that("cache__url() bypasses cache in off mode", {
    cache <- local_test_cache()
    local_cache_mode("off")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; list(data = 42) }

    result <- cache__url("test", "key1", fn)
    expect_equal(result$data, 42)
    expect_equal(call_count, 1L)

    result2 <- cache__url("test", "key1", fn)
    expect_equal(result2$data, 42)
    expect_equal(call_count, 2L)
})

test_that("cache__url() works in normal mode", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; list(data = 42) }

    result1 <- cache__url("test", "key1", fn)
    expect_equal(result1$data, 42)
    expect_equal(call_count, 1L)

    result2 <- cache__url("test", "key1", fn)
    expect_equal(result2$data, 42)
    expect_equal(call_count, 1L)

    result3 <- cache__url("test", "key2", fn)
    expect_equal(result3$data, 42)
    expect_equal(call_count, 2L)
})

test_that("cache__url() works in offline mode", {
    cache <- local_test_cache()

    local_cache_mode("normal")
    cache__url("test", "existing_key", function() "cached_value")

    local_cache_mode("offline")

    result <- cache__url("test", "existing_key", function() stop("should not be called"))
    expect_equal(result, "cached_value")

    expect_error(
        cache__url("test", "missing_key", function() stop("should not be called")),
        "offline"
    )
})

test_that("cache__url() validate parameter controls caching", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; NULL }

    result1 <- cache__url("test", "validate_key", fn, validate = function(x) !is.null(x))
    expect_null(result1)
    expect_equal(call_count, 1L)

    result2 <- cache__url("test", "validate_key", fn, validate = function(x) !is.null(x))
    expect_null(result2)
    expect_equal(call_count, 2L)
})

test_that("cache__url() validate=NULL caches everything (default)", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; NULL }

    result1 <- cache__url("test", "null_key", fn)
    expect_null(result1)
    expect_equal(call_count, 1L)

    result2 <- cache__url("test", "null_key", fn)
    expect_null(result2)
    expect_equal(call_count, 1L)
})
# }}}
# cache__download() {{{
test_that("cache__download() bypasses cache in off mode", {
    cache <- local_test_cache()
    local_cache_mode("off")

    destfile <- tempfile("dl-test-")
    on.exit(unlink(destfile), add = TRUE)

    call_count <- 0L
    fn <- function() {
        call_count <<- call_count + 1L
        writeBin(charToRaw("file content"), destfile)
        destfile
    }

    result <- cache__download("http://example.com/file.txt", destfile, fn)
    expect_equal(result, destfile)
    expect_equal(call_count, 1L)
    expect_equal(readLines(destfile, warn = FALSE), "file content")
})

test_that("cache__download() works in normal mode", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    destfile1 <- tempfile("dl-test1-")
    destfile2 <- tempfile("dl-test2-")
    on.exit(unlink(c(destfile1, destfile2)), add = TRUE)

    call_count <- 0L
    fn <- function() {
        call_count <<- call_count + 1L
        writeBin(charToRaw("downloaded data"), destfile1)
        destfile1
    }

    result1 <- cache__download("http://example.com/data.bin", destfile1, fn)
    expect_equal(result1, destfile1)
    expect_equal(call_count, 1L)
    expect_equal(readBin(destfile1, "raw", 100), charToRaw("downloaded data"))

    unlink(destfile1)

    result2 <- cache__download("http://example.com/data.bin", destfile2, function() {
        stop("should not be called")
    })
    expect_equal(result2, destfile2)
    expect_equal(call_count, 1L)
    expect_equal(readBin(destfile2, "raw", 100), charToRaw("downloaded data"))
})

test_that("cache__download() works in offline mode", {
    cache <- local_test_cache()

    destfile <- tempfile("dl-offline-")
    on.exit(unlink(destfile), add = TRUE)

    local_cache_mode("normal")
    fn_populate <- function() {
        writeBin(charToRaw("cached file"), destfile)
        destfile
    }
    cache__download("http://example.com/cached.bin", destfile, fn_populate)

    unlink(destfile)

    local_cache_mode("offline")

    destfile2 <- tempfile("dl-offline2-")
    on.exit(unlink(destfile2), add = TRUE)
    result <- cache__download("http://example.com/cached.bin", destfile2, function() {
        stop("should not be called")
    })
    expect_equal(result, destfile2)
    expect_equal(readBin(destfile2, "raw", 100), charToRaw("cached file"))

    expect_error(
        cache__download("http://example.com/missing.bin", tempfile(), function() {
            stop("should not be called")
        }),
        "offline"
    )
})
# }}}
# cache__set() {{{
test_that("cache__set() sets and returns old cache", {
    original <- cache__set(NULL)
    on.exit(cache__set(original), add = TRUE)

    dir1 <- tempfile("cache-set1-")
    cache1 <- DiskCache$new(dir = dir1, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit(cache1$destroy(), add = TRUE)

    old <- cache__set(cache1)
    expect_null(old)

    expect_identical(cache__get(), cache1)

    dir2 <- tempfile("cache-set2-")
    cache2 <- DiskCache$new(dir = dir2, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit(cache2$destroy(), add = TRUE)

    old2 <- cache__set(cache2)
    expect_identical(old2, cache1)
    expect_identical(cache__get(), cache2)
})
# }}}
# cache__get() {{{
test_that("cache__get() uses epwshiftr.dir_cache", {
    original <- cache__set(NULL)
    cache_dir <- tempfile("epwshiftr-dir-cache-")
    expected_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    withr::defer({
        cache__reset()
        unlink(cache_dir, recursive = TRUE)
        cache__set(original)
    })

    expect_false(dir.exists(cache_dir))
    cache <- cache__get()

    expect_s3_class(cache, "DiskCache")
    expect_true(dir.exists(cache_dir))
    expect_identical(cache$info()$dir, expected_dir)

    cache$set("dir-cache-option", list(value = 1L))
    expect_equal(cache$get("dir-cache-option"), list(value = 1L))
})
# }}}
# cache__reset() {{{
test_that("cache__reset() sets cache to NULL", {
    original <- cache__set(NULL)
    on.exit(cache__set(original), add = TRUE)

    dir <- tempfile("cache-reset-test-")
    cache <- DiskCache$new(dir = dir, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit({
        unlink(dir, recursive = TRUE)
    }, add = TRUE)

    cache__set(cache)
    expect_identical(cache__get(), cache)

    cache__reset()

    new_cache <- cache__get()
    expect_false(identical(new_cache, cache))
    expect_s3_class(new_cache, "DiskCache")

    cache__reset()
})
# }}}
