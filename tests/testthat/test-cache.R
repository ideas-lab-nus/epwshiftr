# Helper function to create cache with low prune_rate for testing
cache_disk_deterministic <- function(dir, ...) {
    DiskCache$new(dir = dir, ...)
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
        "'prune_rate' must be a positive number"
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

    # Pruning won't happen when set_count < prune_rate
    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)
    cache$set("d", 1); Sys.sleep(delay)

    expect_equal(sort(cache$keys()), c("a", "b", "c", "d"))

    # Pruning will happen with lower prune_rate
    cache$destroy()
    cache <- cache_disk_deterministic(cache_dir, max_n = 2, prune_rate = 2)

    cache$set("a", 1); Sys.sleep(delay)
    cache$set("b", 1); Sys.sleep(delay)
    cache$set("c", 1); Sys.sleep(delay)

    # After 3 sets with prune_rate=2, should have pruned
    expect_equal(sort(cache$keys()), c("b", "c"))

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
    metadata_file <- file.path(cache_dir, ".metadata.qs")
    expect_true(file.exists(metadata_file))

    # Destroy and recreate
    rm(cache)
    gc()

    # Metadata should be loaded
    cache <- DiskCache$new(cache_dir, max_n = 10, max_size = 1000, prune_on_init = FALSE)
    expect_true(cache$exists("a"))

    cache$destroy()
})
