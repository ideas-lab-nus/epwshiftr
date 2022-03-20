get_cache <- function(path = Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA)) {
    if (is.na(path)) path <- tools::R_user_dir("epwshiftr", "cache")

    cache <- normalizePath(path, mustWork = FALSE)
    if (identical(Sys.getenv("NOT_CRAN"), "true") && !dir.exists(cache)) {
        dir.create(cache, recursive = TRUE)
    }
    cache

    # download weather
    file <- "SGP_Singapore.486980_IWEC.epw"
    epw <- file.path(cache, file)
    if (!file.exists(path)) {
        eplusr::download_weather("SGP_Singapore.486980_IWEC", dir = cache, type = "epw", ask = FALSE, max_match = 1)
    }

    # download NetCDF
    withr::with_options(
        list(epwshiftr.dir = cache),
        {
            idx <- init_cmip6_index(
                variable = "tas", source = "EC-Earth3", years = 2060,
                experiment = "ssp585", limit = 1, save = TRUE
            )

            # download output files
            for (f in idx$file_url) {
                dest <- file.path(cache, basename(f))
                if (!file.exists(dest)) {
                    flag <- download.file(f, dest, mode = "wb")
                }
            }
        }
    )

    cache
}
