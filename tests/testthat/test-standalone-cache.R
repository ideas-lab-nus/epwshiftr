# cache_disk_deterministic / cache_set_seq {{{
cache_disk_deterministic <- function(dir, ...) {
    cache <- DiskCache$new(dir = dir, ...)
    priv(cache)$set_count <- 0L
    cache
}

cache_set_seq <- function(cache, keys, fn = function() 1) {
    for (k in keys) {
        cache$set(k, fn())
        Sys.sleep(0.01)
    }
}
# }}}

# DiskCache initialization and config {{{
test_that("DiskCache$new() validates arguments and parses size/age strings", {
    for (bad_dir in list(NULL, c("a", "b"), "", NA_character_)) {
        expect_error(DiskCache$new(bad_dir), "'dir' must be a single string")
    }

    d <- tempfile("cache-invalid-")
    expect_error(DiskCache$new(d, max_size = -1), "non-negative number")
    expect_error(DiskCache$new(d, max_age = -1), "non-negative number")
    expect_error(DiskCache$new(d, max_n = -1), "non-negative number")
    expect_error(DiskCache$new(d, prune_rate = 0), "positive integer")
    expect_error(DiskCache$new(d, prune_rate = 1.5), "positive integer")
    expect_error(DiskCache$new(d, prune_limit = -1), "non-negative number")
    expect_error(DiskCache$new(d, prune_on_init = NA), "TRUE or FALSE")
    expect_error(DiskCache$new(d, max_size = "invalid"), "is a string")
    expect_error(DiskCache$new(d, max_age = "invalid"), "is a string")

    sz <- c("500 KB" = 512000, "100 MB" = 104857600, "1 GB" = 1073741824)
    for (nm in names(sz)) {
        c1 <- DiskCache$new(d, max_size = nm)
        expect_equal(c1$info()$max_size, sz[[nm]])
        c1$destroy()
    }
    ag <- c(
        "100 secs" = 100,
        "30 mins" = 1800,
        "1 hour" = 3600,
        "2 days" = 172800
    )
    for (nm in names(ag)) {
        c1 <- DiskCache$new(d, max_age = nm)
        expect_equal(c1$info()$max_age, ag[[nm]])
        c1$destroy()
    }
})

test_that("DiskCache$new() persists metadata, detects config changes, and prunes on init", {
    skip_on_cran()
    d <- withr::local_tempdir("cache-persist-")
    new_c <- function(...) DiskCache$new(d, max_size = 1000, ...)
    cache <- new_c(max_n = 10, prune_rate = 100, prune_on_init = FALSE)
    cache_set_seq(cache, c("a", "b", "c", "d", "e"))
    expect_true(file.exists(file.path(d, ".metadata.rds")))
    rm(cache)
    gc()

    reloaded <- new_c(max_n = 10, prune_on_init = FALSE)
    expect_true(reloaded$exists("a"))
    rm(reloaded)
    gc()

    expect_message(
        pruned <- new_c(max_n = 3, prune_rate = 100, prune_on_init = TRUE),
        "Cache configuration has changed"
    )
    expect_equal(pruned$size(), 3L)
    pruned$destroy()
})
# }}}

# DiskCache CRUD, types, key validation, and shared visibility {{{
test_that("DiskCache supports CRUD, overwrites, diverse R types, info(), and key validation", {
    d <- withr::local_tempdir("cache-crud-")
    cache <- DiskCache$new(d, max_age = 3600, prune_rate = 100, prune_limit = 5)
    withr::defer(if (!cache$is_destroyed()) cache$destroy())

    expect_true(dir.exists(d))
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
    for (k in list(123, c("a", "b"), NULL)) {
        expect_error(cache$get(k), "Key must be a single string")
    }
    expect_error(cache$set(NULL, 1), "Key must be a single string")
    expect_error(cache$get(""), "Key must not be empty")
    expect_error(cache$get(strrep("a", 81L)), "shorter than 80 characters")
    for (k in c("a/b", "a:b", "a*b")) {
        expect_error(cache$get(k), "must not contain")
    }
})

test_that("DiskCache instances observe shared directory writes", {
    skip_on_cran()
    d <- withr::local_tempdir("cache-shared-")
    cache1 <- DiskCache$new(d, prune_rate = 100)
    cache2 <- DiskCache$new(d, prune_rate = 100)
    withr::defer(if (!cache1$is_destroyed()) cache1$destroy())

    cache1$set("k1", 100)
    cache2$set("k2", 200)
    expect_true(cache1$exists("k2"))
    expect_equal(cache2$get("k1"), 100)
})
# }}}

# DiskCache pruning, expiry, and throttling {{{
test_that("DiskCache$prune() enforces max_n, max_size, and combined limits", {
    skip_on_cran()
    dn <- withr::local_tempdir("cache-prune-n-")
    cn <- cache_disk_deterministic(dn, max_n = 3, prune_rate = 1)
    withr::defer(cn$destroy())
    cache_set_seq(cn, c("a", "b", "c", "d", "e"), function() rnorm(100))
    cn$prune()
    expect_equal(sort(cn$keys()), c("c", "d", "e"))

    ds <- withr::local_tempdir("cache-prune-size-")
    cs <- cache_disk_deterministic(
        ds,
        max_n = 3,
        max_size = 200,
        prune_rate = 1
    )
    withr::defer(cs$destroy())
    cache_set_seq(cs, c("a", "b", "c", "d"), function() rnorm(100))
    cache_set_seq(cs, "small")
    cs$prune()
    expect_equal(cs$keys(), "small")

    cache_set_seq(cs, "too_big", function() rnorm(100))
    cs$prune()
    expect_length(cs$keys(), 0L)
})

test_that("DiskCache honors max_age expiry and prune_rate/prune_limit throttling", {
    skip_on_cran()
    # max_age on get() and prune()
    da <- withr::local_tempdir("cache-age-")
    ca <- cache_disk_deterministic(da, max_age = 0.2, prune_rate = 1)
    withr::defer(ca$destroy())
    ca$set("a", 1)
    ca$set("b", 2)
    expect_equal(ca$get("a"), 1)
    Sys.sleep(0.25)
    expect_true(cache__missing(ca$get("a")))
    expect_false(ca$exists("a"))
    ca$prune()
    expect_equal(ca$size(), 0L)

    # prune_rate throttling
    dr <- withr::local_tempdir("cache-rate-")
    cs <- cache_disk_deterministic(dr, max_n = 2, prune_rate = 20)
    cache_set_seq(cs, c("a", "b", "c", "d"))
    expect_equal(sort(cs$keys()), c("a", "b", "c", "d"))
    cs$destroy()

    cf <- cache_disk_deterministic(dr, max_n = 2, prune_rate = 2)
    withr::defer(cf$destroy())
    cache_set_seq(cf, c("a", "b", "c", "d"))
    expect_equal(sort(cf$keys()), c("c", "d"))

    # prune_limit time throttling
    dl <- withr::local_tempdir("cache-limit-")
    cl <- cache_disk_deterministic(
        dl,
        max_n = 2,
        prune_rate = 100,
        prune_limit = 0.3
    )
    withr::defer(cl$destroy())
    for (k in c("a", "b", "c")) {
        cl$set(k, 1)
    }
    expect_equal(cl$size(), 3L)
    Sys.sleep(0.35)
    cl$set("d", 1)
    expect_equal(sort(cl$keys()), c("c", "d"))
})
# }}}

# DiskCache$destroy() and $print() {{{
test_that("DiskCache$destroy() removes directory and blocks further operations", {
    d <- tempfile("cache-destroy-")
    cache <- DiskCache$new(d, prune_rate = 100)
    cache$set("a", 1)

    expect_false(cache$is_destroyed())
    expect_true(cache$destroy())
    expect_true(cache$is_destroyed())
    expect_false(dir.exists(d))
    expect_false(cache$destroy())

    msg <- "Cache .* has been destroyed"
    expect_error(cache$get("key"), msg)
    expect_error(cache$set("key", 1), msg)
    expect_error(cache$exists("key"), msg)
    expect_error(cache$remove("key"), msg)
    expect_error(cache$reset(), msg)
    expect_error(cache$keys(), msg)
    expect_error(cache$size(), msg)
    expect_error(cache$prune(), msg)
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
    modes <- c(normal = FALSE, off = FALSE, offline = TRUE)
    for (m in names(modes)) {
        local_cache_mode(m)
        expect_equal(cache__mode(), m)
        expect_identical(cache__offline(), modes[[m]])
    }

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
    d0 <- tempfile("epwshiftr-dir-cache-")
    d1 <- tempfile("cache-set1-")
    expected_dir <- normalizePath(d0, "/", mustWork = FALSE)
    withr::local_options(list(epwshiftr.dir_cache = d0))
    withr::defer({
        cache__reset()
        unlink(c(d0, d1), recursive = TRUE)
        cache__set(original)
    })

    expect_false(dir.exists(d0))
    c0 <- cache__get()
    expect_s3_class(c0, "DiskCache")
    expect_true(dir.exists(d0))
    expect_identical(c0$info()$dir, expected_dir)

    c1 <- DiskCache$new(dir = d1, max_size = "100 MB")
    expect_identical(cache__set(c1), c0)
    expect_identical(cache__get(), c1)

    cache__reset()
    c2 <- cache__get()
    expect_s3_class(c2, "DiskCache")
    expect_false(identical(c2, c1))
})
# }}}

# cache__read_json() {{{
test_that("cache__read_json() handles modes, Solr null maxScore, non-strict failures, and long URLs", {
    cache <- local_test_cache()
    local_cache_mode("off")

    p <- withr::local_tempfile(fileext = ".json")
    body <- list(response = list(numFound = 1L, docs = list()))
    jsonlite::write_json(body, p, auto_unbox = TRUE)

    res <- cache__read_json(p, cache = TRUE, simplifyVector = FALSE)
    expect_equal(res$response$numFound, 1L)
    expect_equal(cache$size(), 1L)

    off <- cache__read_json(p, cache = "offline", simplifyVector = FALSE)
    expect_equal(off$response$numFound, 1L)
    expect_error(
        cache__read_json(tempfile(fileext = ".json"), cache = "offline"),
        "Cache miss in offline mode"
    )

    # Empty Solr maxScore normalization
    s <- '{"response":{"numFound":0,"start":0,"docs":[],"maxScore":null}}'
    empty_solr <- suppressWarnings(
        cache__read_json(s, cache = FALSE, simplifyVector = FALSE)
    )
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
    url <- paste0("https://example.org/esg-search/search?", strrep("a", 2100L))
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

    cb <- function(p) states <<- c(states, p$state)
    long_res <- cache__read_json(
        url,
        simplifyVector = FALSE,
        progress_callback = cb
    )
    expect_equal(seen$url, url)
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
    no_run <- function() stop("should not run")

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
    ok <- function(x) !is.null(x)
    expect_null(cache__url("test", "vkey", fetch_null, validate = ok))
    expect_null(cache__url("test", "vkey", fetch_null, validate = ok))
    expect_equal(null_calls, 2L)
    expect_null(cache__url("test", "nkey", fetch_null))
    expect_null(cache__url("test", "nkey", fetch_null))
    expect_equal(null_calls, 3L)

    local_cache_mode("offline")
    expect_equal(cache__url("test", "k1", no_run)$data, 42)
    expect_error(cache__url("test", "missing", no_run), "offline")
})

test_that("cache__download() handles off, normal, and offline modes", {
    local_test_cache()
    d <- withr::local_tempdir("cache-dl-")
    f1 <- file.path(d, "1")
    f2 <- file.path(d, "2")
    f3 <- file.path(d, "3")
    u <- "http://example.com/data.bin"
    no_run <- function() stop("should not run")
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
    expect_equal(cache__download(u, f1, dl(f1)), f1)
    expect_equal(calls, 2L)
    unlink(f1)
    expect_equal(cache__download(u, f2, no_run), f2)
    expect_equal(readBin(f2, "raw", 100), charToRaw("downloaded data"))

    local_cache_mode("offline")
    expect_equal(cache__download(u, f3, no_run), f3)
    expect_equal(readBin(f3, "raw", 100), charToRaw("downloaded data"))
    expect_error(
        cache__download("http://example.com/m.bin", f1, no_run),
        "offline"
    )
})
# }}}

# vim: fdm=marker :
