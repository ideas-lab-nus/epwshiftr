get_test_cache <- function(dir = Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA), max_size = "1 GB", max_age = "24 hours") {
    if (is.na(dir)) {
        dir <- tempfile("epwshiftr-test-cache-", tmpdir = tempdir())
    }

    DiskCache$new(
        dir = dir,
        max_size = max_size,
        max_age = max_age,
        max_n = Inf
    )
}

get_cache_epw <- function() {
    dir <- get_test_cache()$info()$dir
    file <- "SGP_Singapore.486980_IWEC.epw"
    epw <- file.path(dir, file)
    if (!file.exists(epw)) {
        eplusr::download_weather("SGP_Singapore.486980_IWEC", dir = dir, type = "epw", ask = FALSE, max_match = 1L)
    }
    normalizePath(epw)
}

get_cache_nc <- function(reset = FALSE) {
    dir <- get_test_cache()$info()$dir
    withr::with_options(
        list(epwshiftr.dir = dir),
        {
            if (!reset && file.exists(file.path(dir, "cmip6_index.csv"))) {
                idx <- load_cmip6_index()
            } else {
                idx <- init_cmip6_index(
                    variable = "tas",
                    source = "EC-Earth3",
                    years = 2060L,
                    experiment = "ssp585",
                    limit = 1L,
                    save = TRUE
                )
            }

            # download output files
            for (f in idx$file_url) {
                dest <- file.path(dir, basename(f))
                if (!file.exists(dest)) {
                    old <- getOption("timeout")
                    options(timeout = 60L * 100L)
                    on.exit(options(timeout = old), add = TRUE)
                    download.file(f, dest, mode = "wb")
                }
            }
        }
    )

    normalizePath(dir)
}

# Scoped cache mode switch for tests
local_cache_mode <- function(mode, env = parent.frame()) {
    old <- getOption("epwshiftr.cache")
    withr::defer(options(epwshiftr.cache = old), envir = env)
    opt_val <- switch(
        mode,
        "normal" = TRUE,
        "off" = FALSE,
        "offline" = "offline",
        stop(sprintf("Unknown cache mode: '%s'", mode))
    )
    options(epwshiftr.cache = opt_val)
}

# Create a temporary test cache and set it as the package cache
local_test_cache <- function(env = parent.frame()) {
    dir <- tempfile("epwshiftr-test-cache-")
    cache <- DiskCache$new(dir = dir, max_size = "100 MB", max_age = Inf, max_n = Inf)
    old_cache <- set_cache(cache)
    withr::defer(
        {
            set_cache(old_cache)
            unlink(dir, recursive = TRUE)
        },
        envir = env
    )
    cache
}
