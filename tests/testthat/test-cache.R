# Helper function to create cache with low prune_rate for testing
cache_disk_deterministic <- function(dir, ...) {
    cache <- DiskCache$new(dir = dir, ...)
    priv(cache)$set_count <- 0L
    cache
}

# Basic functionality tests
test_that("DiskCache: initialization with valid parameters", {
    cache_dir <- tempfile("cache-init-")
    cache <- DiskCache$new(cache_dir)

    expect_true(dir.exists(cache_dir))
    expect_s3_class(cache, "DiskCache")
    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)

    cache$destroy()
})

test_that("DiskCache: initialization with invalid parameters", {
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

test_that("DiskCache: human-readable size formats", {
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

test_that("DiskCache: human-readable age formats", {
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

test_that("DiskCache: handling missing values", {
    cache_dir <- tempfile("cache-missing-")
    cache <- DiskCache$new(cache_dir)

    expect_true(is.key_missing(cache$get("abcd")))
    cache$set("a", 100)
    expect_identical(cache$get("a"), 100)

    cache$destroy()
})

test_that("DiskCache: basic get/set operations", {
    cache_dir <- tempfile("cache-basic-")
    cache <- DiskCache$new(cache_dir)

    # Set and get simple values
    cache$set("key1", 123)
    expect_equal(cache$get("key1"), 123)

    cache$set("key2", "hello")
    expect_equal(cache$get("key2"), "hello")

    cache$set("key3", list(a = 1, b = 2))
    expect_equal(cache$get("key3"), list(a = 1, b = 2))

    # Overwrite existing key
    cache$set("key1", 456)
    expect_equal(cache$get("key1"), 456)

    cache$destroy()
})

test_that("DiskCache: exists() method", {
    cache_dir <- tempfile("cache-exists-")
    cache <- DiskCache$new(cache_dir)

    expect_false(cache$exists("key1"))
    cache$set("key1", 123)
    expect_true(cache$exists("key1"))

    cache$remove("key1")
    expect_false(cache$exists("key1"))

    cache$destroy()
})

test_that("DiskCache: keys() and size() methods", {
    cache_dir <- tempfile("cache-keys-")
    cache <- DiskCache$new(cache_dir)

    expect_length(cache$keys(), 0L)
    expect_equal(cache$size(), 0L)

    cache$set("a", 1)
    cache$set("b", 2)
    cache$set("c", 3)

    expect_length(cache$keys(), 3L)
    expect_equal(cache$size(), 3L)
    expect_setequal(cache$keys(), c("a", "b", "c"))

    cache$destroy()
})

test_that("DiskCache: remove() method", {
    cache_dir <- tempfile("cache-remove-")
    cache <- DiskCache$new(cache_dir)

    cache$set("key1", 123)
    expect_true(cache$exists("key1"))

    result <- cache$remove("key1")
    expect_true(result)
    expect_false(cache$exists("key1"))

    # Removing non-existent key returns FALSE
    result <- cache$remove("nonexistent")
    expect_false(result)

    cache$destroy()
})

test_that("DiskCache: reset() method", {
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

test_that("DiskCache: key validation", {
    cache_dir <- tempfile("cache-key-validation-")
    cache <- DiskCache$new(cache_dir)

    # Invalid key types
    expect_error(cache$get(123), "Key must be a single string")
    expect_error(cache$get(c("a", "b")), "Key must be a single string")
    expect_error(cache$set(NULL, 1), "Key must be a single string")

    # Empty key
    expect_error(cache$get(""), "Key must not be empty")

    # Key too long
    long_key <- paste(rep("a", 81), collapse = "")
    expect_error(cache$get(long_key), "Key must be shorter than 80 characters")

    # Invalid characters
    expect_error(cache$get("key/with/slash"), "Key must not contain any of the following characters")
    expect_error(cache$get("key:with:colon"), "Key must not contain any of the following characters")
    expect_error(cache$get("key*with*star"), "Key must not contain any of the following characters")

    cache$destroy()
})

test_that("DiskCache: info() method", {
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

test_that("DiskCache: pruning respects max_n", {
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

test_that("DiskCache: pruning respects max_size", {
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

test_that("DiskCache: pruning respects both max_n and max_size", {
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

test_that("DiskCache: pruning respects max_age", {
    skip_on_cran()

    cache_dir <- tempfile("cache-prune-age-")
    cache <- cache_disk_deterministic(cache_dir, max_age = 0.5, prune_rate = 1)

    cache$set("a", 1)
    cache$set("b", 2)

    expect_equal(cache$size(), 2L)

    # Wait for items to expire
    Sys.sleep(0.6)

    cache$prune()
    expect_equal(cache$size(), 0L)

    cache$destroy()
})

test_that("DiskCache: max_age affects get()", {
    skip_on_cran()

    cache_dir <- tempfile("cache-age-get-")
    cache <- DiskCache$new(cache_dir, max_age = 0.5)

    cache$set("key1", 123)
    expect_equal(cache$get("key1"), 123)

    # Wait for item to expire
    Sys.sleep(0.6)

    # Should return missing
    expect_true(is.key_missing(cache$get("key1")))
    expect_false(cache$exists("key1"))

    cache$destroy()
})

test_that("DiskCache: pruning throttling with prune_rate", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-throttle-rate-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 20)
    priv(cache)$set_count <- 0L

    # Pruning won't happen when set_count < prune_rate
    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$set("d", 1); Sys.sleep(delay)

    expect_equal(sort(cache$keys()), c("a", "b", "c", "d"))

    # Pruning will happen with lower prune_rate
    cache$destroy()
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 2)
    priv(cache)$set_count <- 0L

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$set("d", 1); Sys.sleep(delay)

    # With prune_rate = 2, pruning happens on the 2nd and 4th sets.
    # The 4th set is the first throttled prune where max_n pruning removes keys.
    expect_equal(sort(cache$keys()), c("c", "d"))

    cache$destroy()
})

test_that("DiskCache: pruning throttling with prune_limit", {
    skip_on_cran()

    cache_dir <- tempfile("cache-throttle-limit-")
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 100, prune_limit = 1)

    cache$set("a", 1)
    cache$set("b", 1)
    cache$set("c", 1)

    # No pruning yet (prune_rate not reached)
    expect_equal(cache$size(), 3L)

    # Wait for prune_limit
    Sys.sleep(1.1)

    cache$set("d", 1)

    # Should prune now (time limit exceeded)
    expect_equal(sort(cache$keys()), c("c", "d"))

    cache$destroy()
})

test_that("DiskCache: destroy() and is_destroyed()", {
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

    # Second destroy should return FALSE
    result <- cache$destroy()
    expect_false(result)
})

test_that("DiskCache: operations after destroy throw errors", {
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

test_that("DiskCache: configuration change detection", {
    skip_on_cran()

    cache_dir <- tempfile("cache-config-")
    cache <- DiskCache$new(cache_dir, max_n = 10, prune_on_init = FALSE)

    cache$set("a", 1)
    cache$set("b", 2)

    # Destroy and recreate with different config
    rm(cache)
    gc()

    expect_message(
        cache <- DiskCache$new(cache_dir, max_n = 5, prune_on_init = FALSE),
        "Cache configuration has changed"
    )

    cache$destroy()
})

test_that("DiskCache: prune_on_init parameter", {
    skip_on_cran()
    delay <- 0.01

    cache_dir <- tempfile("cache-prune-init-")
    cache <- DiskCache$new(cache_dir, max_n = 3, prune_rate = 100, prune_on_init = FALSE)

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 2); Sys.sleep(delay)
    cache$set("c", 3); Sys.sleep(delay)
    cache$set("d", 4); Sys.sleep(delay)
    cache$set("e", 5); Sys.sleep(delay)

    # Manually prune
    cache$prune()
    expect_equal(cache$size(), 3L)

    # Destroy and recreate with prune_on_init = TRUE
    rm(cache)
    gc()

    # Should not prune on init since config is the same
    cache <- DiskCache$new(cache_dir, max_n = 3, prune_rate = 100, prune_on_init = TRUE)
    expect_equal(cache$size(), 3L)

    cache$destroy()
})

test_that("DiskCache: storing different data types", {
    cache_dir <- tempfile("cache-types-")
    cache <- DiskCache$new(cache_dir, prune_rate = 100)

    # Numeric
    cache$set("num", 123.456)
    expect_equal(cache$get("num"), 123.456)

    # Integer
    cache$set("int", 123L)
    expect_equal(cache$get("int"), 123L)

    # Character
    cache$set("char", "hello world")
    expect_equal(cache$get("char"), "hello world")

    # Logical
    cache$set("bool", TRUE)
    expect_equal(cache$get("bool"), TRUE)

    # NULL
    cache$set("null", NULL)
    expect_null(cache$get("null"))

    # List
    cache$set("list", list(a = 1, b = "two", c = list(d = 3)))
    expect_equal(cache$get("list"), list(a = 1, b = "two", c = list(d = 3)))

    # Data frame
    df <- data.frame(x = 1:3, y = c("a", "b", "c"))
    cache$set("df", df)
    expect_equal(cache$get("df"), df)

    # Matrix
    mat <- matrix(1:9, nrow = 3)
    cache$set("mat", mat)
    expect_equal(cache$get("mat"), mat)

    # Function
    fn <- function(x) x + 1
    cache$set("fn", fn)
    expect_equal(cache$get("fn")(5), 6)

    # Environment
    env <- new.env()
    env$x <- 1
    cache$set("env", env)
    expect_equal(cache$get("env")$x, 1)

    cache$destroy()
})

test_that("DiskCache: atomic write prevents corruption", {
    cache_dir <- tempfile("cache-atomic-")
    cache <- DiskCache$new(cache_dir)

    # Set a value
    cache$set("key1", rnorm(1000))

    # Verify it can be read
    expect_length(cache$get("key1"), 1000)

    # Overwrite with new value
    cache$set("key1", rnorm(2000))

    # Verify new value
    expect_length(cache$get("key1"), 2000)

    cache$destroy()
})

test_that("DiskCache: empty cache operations", {
    cache_dir <- tempfile("cache-empty-")
    cache <- DiskCache$new(cache_dir)

    expect_equal(cache$size(), 0L)
    expect_length(cache$keys(), 0L)

    # Reset on empty cache
    cache$reset()
    expect_equal(cache$size(), 0L)

    # Prune on empty cache
    cache$prune()
    expect_equal(cache$size(), 0L)

    # Info on empty cache
    info <- cache$info()
    expect_equal(info$n, 0L)
    expect_equal(info$size, 0L)

    cache$destroy()
})

test_that("DiskCache: large number of keys", {
    skip_on_cran()

    cache_dir <- tempfile("cache-many-keys-")
    cache <- DiskCache$new(cache_dir, prune_rate = 200)

    n <- 100
    for (i in seq_len(n)) {
        cache$set(paste0("key", i), i)
    }

    expect_equal(cache$size(), n)
    expect_length(cache$keys(), n)

    # Verify all keys exist
    for (i in seq_len(n)) {
        expect_true(cache$exists(paste0("key", i)))
        expect_equal(cache$get(paste0("key", i)), i)
    }

    cache$destroy()
})

test_that("DiskCache: concurrent access simulation", {
    skip_on_cran()

    cache_dir <- tempfile("cache-concurrent-")
    cache1 <- DiskCache$new(cache_dir, prune_rate = 100)
    cache2 <- DiskCache$new(cache_dir, prune_rate = 100)

    # Both caches point to same directory
    cache1$set("key1", 100)
    cache2$set("key2", 200)

    # Both should see both keys after refresh
    expect_true(cache1$exists("key1"))
    expect_true(cache1$exists("key2"))
    expect_true(cache2$exists("key1"))
    expect_true(cache2$exists("key2"))

    cache1$destroy()
})

test_that("DiskCache: is.key_missing() helper", {
    cache_dir <- tempfile("cache-key-missing-")
    cache <- DiskCache$new(cache_dir)

    result <- cache$get("nonexistent")
    expect_true(is.key_missing(result))
    expect_s3_class(result, "key_missing")

    cache$set("exists", 123)
    result <- cache$get("exists")
    expect_false(is.key_missing(result))

    cache$destroy()
})

test_that("DiskCache: special characters in values", {
    cache_dir <- tempfile("cache-special-")
    cache <- DiskCache$new(cache_dir)

    # Unicode
    cache$set("unicode", "你好世界")
    expect_equal(cache$get("unicode"), "你好世界")

    # Special characters
    cache$set("special", "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    expect_equal(cache$get("special"), "!@#$%^&*()_+-=[]{}|;':\",./<>?")

    # Newlines and tabs
    cache$set("whitespace", "line1\nline2\ttab")
    expect_equal(cache$get("whitespace"), "line1\nline2\ttab")

    cache$destroy()
})

test_that("DiskCache: metadata persistence", {
    skip_on_cran()

    cache_dir <- tempfile("cache-metadata-")
    cache <- DiskCache$new(cache_dir, max_n = 10, max_size = 1000)

    cache$set("a", 1)

    # Check metadata file exists
    metadata_file <- file.path(cache_dir, ".metadata.rds")
    expect_true(file.exists(metadata_file))

    # Destroy and recreate
    rm(cache)
    gc()

    # Metadata should be loaded
    cache <- DiskCache$new(cache_dir, max_n = 10, max_size = 1000, prune_on_init = FALSE)
    expect_true(cache$exists("a"))

    cache$destroy()
})


# ============================================================================
# Cache infrastructure tests (cache_mode, with_url_cache, etc.)
# ============================================================================

test_that("cache_mode() returns correct mode", {
    local_cache_mode("normal")
    expect_equal(cache_mode(), "normal")

    local_cache_mode("off")
    expect_equal(cache_mode(), "off")

    local_cache_mode("offline")
    expect_equal(cache_mode(), "offline")

    # unknown value warns and falls back to "normal"
    withr::local_options(epwshiftr.cache = "bogus")
    expect_warning(result <- cache_mode(), "Unknown")
    expect_equal(result, "normal")
})

test_that("is_cache_enabled() returns correct values", {
    local_cache_mode("normal")
    expect_true(is_cache_enabled())

    local_cache_mode("off")
    expect_false(is_cache_enabled())

    local_cache_mode("offline")
    expect_true(is_cache_enabled())
})

test_that("is_cache_offline() returns correct values", {
    local_cache_mode("normal")
    expect_false(is_cache_offline())

    local_cache_mode("off")
    expect_false(is_cache_offline())

    local_cache_mode("offline")
    expect_true(is_cache_offline())
})

test_that("with_cache_mode() scopes correctly and restores original", {
    local_cache_mode("normal")
    expect_equal(cache_mode(), "normal")

    # Temporarily switch to off
    result <- with_cache_mode("off", {
        expect_equal(cache_mode(), "off")
        "return_value"
    })
    expect_equal(result, "return_value")
    expect_equal(cache_mode(), "normal")

    # Temporarily switch to offline
    with_cache_mode("offline", {
        expect_equal(cache_mode(), "offline")
    })
    expect_equal(cache_mode(), "normal")

    with_cache_mode(FALSE, {
        expect_equal(cache_mode(), "off")
    })
    expect_equal(cache_mode(), "normal")

    with_cache_mode(TRUE, {
        expect_equal(cache_mode(), "normal")
    })
    expect_equal(cache_mode(), "normal")

    # Invalid mode
    expect_error(with_cache_mode("invalid", NULL), "Unknown cache mode")
})

test_that("read_json_response() honors explicit cache mode", {
    cache <- local_test_cache()
    local_cache_mode("off")

    path <- tempfile(fileext = ".json")
    jsonlite::write_json(
        list(response = list(numFound = 1L, docs = list())),
        path,
        auto_unbox = TRUE
    )

    res <- read_json_response(path, cache = TRUE, simplifyVector = FALSE)
    expect_equal(res$response$numFound, 1L)
    expect_equal(cache$size(), 1L)

    offline <- read_json_response(path, cache = "offline", simplifyVector = FALSE)
    expect_equal(offline$response$numFound, 1L)

    expect_error(
        read_json_response(tempfile(fileext = ".json"), cache = "offline"),
        "Cache miss in offline mode"
    )
})

test_that("read_json_response() does not cache failed non-strict reads", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    expect_warning(
        res <- read_json_response("not valid json", strict = FALSE, cache = TRUE),
        "Failed to read the JSON response"
    )
    expect_null(res)
    expect_equal(cache$size(), 0L)
})

test_that("make_cache_key() is deterministic", {
    key1 <- make_cache_key("test", "a", "b")
    key2 <- make_cache_key("test", "a", "b")
    expect_equal(key1, key2)
})

test_that("make_cache_key() produces different keys for different inputs", {
    key1 <- make_cache_key("test", "a", "b")
    key2 <- make_cache_key("test", "a", "c")
    expect_false(key1 == key2)

    # Different prefix
    key3 <- make_cache_key("other", "a", "b")
    expect_false(key1 == key3)
})

test_that("make_cache_key() has correct format", {
    key <- make_cache_key("myprefix", "data")
    expect_match(key, "^myprefix-[0-9a-f]{8}$")
})

test_that("with_url_cache() bypasses cache in off mode", {
    cache <- local_test_cache()
    local_cache_mode("off")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; list(data = 42) }

    result <- with_url_cache("test", "key1", fn)
    expect_equal(result$data, 42)
    expect_equal(call_count, 1L)

    # Second call: fn is called again (no caching)
    result2 <- with_url_cache("test", "key1", fn)
    expect_equal(result2$data, 42)
    expect_equal(call_count, 2L)
})

test_that("with_url_cache() works in normal mode", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; list(data = 42) }

    # First call: fn is executed
    result1 <- with_url_cache("test", "key1", fn)
    expect_equal(result1$data, 42)
    expect_equal(call_count, 1L)

    # Second call: fn is NOT executed (cached)
    result2 <- with_url_cache("test", "key1", fn)
    expect_equal(result2$data, 42)
    expect_equal(call_count, 1L) # still 1

    # Different key: fn IS executed
    result3 <- with_url_cache("test", "key2", fn)
    expect_equal(result3$data, 42)
    expect_equal(call_count, 2L)
})

test_that("with_url_cache() works in offline mode", {
    cache <- local_test_cache()

    # Pre-populate cache in normal mode
    local_cache_mode("normal")
    with_url_cache("test", "existing_key", function() "cached_value")

    # Switch to offline
    local_cache_mode("offline")

    # Cache hit works
    result <- with_url_cache("test", "existing_key", function() stop("should not be called"))
    expect_equal(result, "cached_value")

    # Cache miss throws error
    expect_error(
        with_url_cache("test", "missing_key", function() stop("should not be called")),
        "offline"
    )
})

test_that("with_url_cache() validate parameter controls caching", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; NULL }

    # With validate that rejects NULL: result returned but NOT cached
    result1 <- with_url_cache("test", "validate_key", fn, validate = function(x) !is.null(x))
    expect_null(result1)
    expect_equal(call_count, 1L)

    # Second call: fn IS called again (not cached)
    result2 <- with_url_cache("test", "validate_key", fn, validate = function(x) !is.null(x))
    expect_null(result2)
    expect_equal(call_count, 2L)
})

test_that("with_url_cache() validate=NULL caches everything (default)", {
    cache <- local_test_cache()
    local_cache_mode("normal")

    call_count <- 0L
    fn <- function() { call_count <<- call_count + 1L; NULL }

    # Without validate: NULL IS cached
    result1 <- with_url_cache("test", "null_key", fn)
    expect_null(result1)
    expect_equal(call_count, 1L)

    # Second call: fn NOT called (cached)
    result2 <- with_url_cache("test", "null_key", fn)
    expect_null(result2)
    expect_equal(call_count, 1L)
})


test_that("with_download_cache() bypasses cache in off mode", {
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

    result <- with_download_cache("http://example.com/file.txt", destfile, fn)
    expect_equal(result, destfile)
    expect_equal(call_count, 1L)
    expect_equal(readLines(destfile, warn = FALSE), "file content")
})

test_that("with_download_cache() works in normal mode", {
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

    # First call: fn is executed, file is downloaded and cached
    result1 <- with_download_cache("http://example.com/data.bin", destfile1, fn)
    expect_equal(result1, destfile1)
    expect_equal(call_count, 1L)
    expect_equal(readBin(destfile1, "raw", 100), charToRaw("downloaded data"))

    # Remove the file to prove cache writes it back
    unlink(destfile1)

    # Second call: fn is NOT executed, file is written from cache
    result2 <- with_download_cache("http://example.com/data.bin", destfile2, function() {
        stop("should not be called")
    })
    expect_equal(result2, destfile2)
    expect_equal(call_count, 1L) # still 1
    expect_equal(readBin(destfile2, "raw", 100), charToRaw("downloaded data"))
})

test_that("with_download_cache() works in offline mode", {
    cache <- local_test_cache()

    destfile <- tempfile("dl-offline-")
    on.exit(unlink(destfile), add = TRUE)

    # Pre-populate cache in normal mode
    local_cache_mode("normal")
    fn_populate <- function() {
        writeBin(charToRaw("cached file"), destfile)
        destfile
    }
    with_download_cache("http://example.com/cached.bin", destfile, fn_populate)

    # Remove the file
    unlink(destfile)

    # Switch to offline
    local_cache_mode("offline")

    # Cache hit: writes file from cache
    destfile2 <- tempfile("dl-offline2-")
    on.exit(unlink(destfile2), add = TRUE)
    result <- with_download_cache("http://example.com/cached.bin", destfile2, function() {
        stop("should not be called")
    })
    expect_equal(result, destfile2)
    expect_equal(readBin(destfile2, "raw", 100), charToRaw("cached file"))

    # Cache miss: throws error
    expect_error(
        with_download_cache("http://example.com/missing.bin", tempfile(), function() {
            stop("should not be called")
        }),
        "offline"
    )
})

test_that("set_cache() sets and returns old cache", {
    # Save original cache
    original <- set_cache(NULL)
    on.exit(set_cache(original), add = TRUE)

    dir1 <- tempfile("cache-set1-")
    cache1 <- DiskCache$new(dir = dir1, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit(cache1$destroy(), add = TRUE)

    # set_cache returns old (NULL)
    old <- set_cache(cache1)
    expect_null(old)

    # get_cache returns the one we set
    expect_identical(get_cache(), cache1)

    dir2 <- tempfile("cache-set2-")
    cache2 <- DiskCache$new(dir = dir2, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit(cache2$destroy(), add = TRUE)

    # set_cache returns old (cache1)
    old2 <- set_cache(cache2)
    expect_identical(old2, cache1)
    expect_identical(get_cache(), cache2)
})

test_that("get_cache() uses epwshiftr.dir_cache", {
    original <- set_cache(NULL)
    cache_dir <- tempfile("epwshiftr-dir-cache-")
    expected_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    withr::local_options(list(epwshiftr.dir_cache = cache_dir))
    withr::defer({
        reset_cache()
        unlink(cache_dir, recursive = TRUE)
        set_cache(original)
    })

    expect_false(dir.exists(cache_dir))
    cache <- get_cache()

    expect_s3_class(cache, "DiskCache")
    expect_true(dir.exists(cache_dir))
    expect_identical(cache$info()$dir, expected_dir)

    cache$set("dir-cache-option", list(value = 1L))
    expect_equal(cache$get("dir-cache-option"), list(value = 1L))
})

test_that("reset_cache() sets cache to NULL", {
    # Save original cache
    original <- set_cache(NULL)
    on.exit(set_cache(original), add = TRUE)

    dir <- tempfile("cache-reset-test-")
    cache <- DiskCache$new(dir = dir, max_size = "100 MB", max_age = Inf, max_n = Inf)
    on.exit({
        unlink(dir, recursive = TRUE)
    }, add = TRUE)

    set_cache(cache)
    expect_identical(get_cache(), cache)

    reset_cache()

    # After reset, get_cache() creates a new cache (not the same instance)
    new_cache <- get_cache()
    expect_false(identical(new_cache, cache))
    expect_s3_class(new_cache, "DiskCache")

    # Clean up the auto-created cache
    reset_cache()
})
