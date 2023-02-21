get_cache <- function(path = Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA), reset = FALSE) {
    if (is.na(path)) path <- tools::R_user_dir("epwshiftr", "cache")

    cache <- normalizePath(path, mustWork = FALSE)
    if (identical(Sys.getenv("NOT_CRAN"), "true") && !dir.exists(cache)) {
        dir.create(cache, recursive = TRUE)
    }

    # cache ESGF facet listing query
    facets <- file.path(cache, "facets")
    if (!file.exists(facets)) {
        query_build_facet_listing(formals(query_esgf)$host)
        cache <- this$cache
        saveRDS(cache, facets)
        clear_facet_cache()
    }

    # download weather
    file <- "SGP_Singapore.486980_IWEC.epw"
    epw <- file.path(cache, file)
    if (!file.exists(epw)) {
        eplusr::download_weather("SGP_Singapore.486980_IWEC", dir = cache, type = "epw", ask = FALSE, max_match = 1L)
    }

    # download NetCDF
    withr::with_options(
        list(epwshiftr.dir = cache),
        {
            if (!reset && file.exists(file.path(cache, "cmip6_index.csv"))) {
                idx <- load_cmip6_index()
            } else {
                idx <- init_cmip6_index(
                    variable = "tas", source = "EC-Earth3", years = 2060L,
                    experiment = "ssp585", limit = 1L, save = TRUE
                )
            }

            # download output files
            for (f in idx$file_url) {
                dest <- file.path(cache, basename(f))
                if (!file.exists(dest)) {
                    old <- getOption("timeout")
                    options(timeout = 60L * 100L)
                    on.exit(options(timeout = old), add = TRUE)
                    download.file(f, dest, mode = "wb")
                }
            }
        }
    )

    cache
}

save_facet_cache <- function() {
    if (length(this$cache)) {
        saveRDS(this$cache, file.path(get_cache(), "facets"))
    }
}

attach_facet_cache <- function() {
    if (file.exists(cache <- file.path(get_cache(), "facets"))) {
        attach_cache(readRDS(cache))
    }
}

clear_facet_cache <- function() {
    attach_cache(list())
}
