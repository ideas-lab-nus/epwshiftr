# Skip the real ERA5 workflow unless a developer has deliberately enabled it.
# Once enabled, missing credentials are a test failure rather than a skip so a
# scheduled workflow cannot appear healthy without exercising CDS.
era5_live__skip_unless_enabled <- function() {
    run <- tolower(Sys.getenv("EPWSHIFTR_RUN_LIVE_ERA5", "false"))
    testthat::skip_if_not(
        run %in% c("1", "true", "yes"),
        "Set EPWSHIFTR_RUN_LIVE_ERA5=true to run the live ERA5 test."
    )
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("curl")
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")
    cds__config()
    invisible(TRUE)
}
