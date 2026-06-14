skip_live_esgf <- function() {
    run <- tolower(Sys.getenv("EPWSHIFTR_RUN_LIVE_ESGF", "false"))
    testthat::skip_if_not(
        run %in% c("1", "true", "yes"),
        "Set EPWSHIFTR_RUN_LIVE_ESGF=true to run live ESGF tests."
    )
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("curl")
    skip_if_not_installed("duckdb")
}

is_live_esgf_dict_warning <- function(message) {
    grepl("^ESG dictionary check found invalid query constraint", message)
}

allow_live_esgf_dict_warnings <- function(expr) {
    withCallingHandlers(
        force(expr),
        warning = function(w) {
            if (is_live_esgf_dict_warning(conditionMessage(w))) {
                invokeRestart("muffleWarning")
            }
        }
    )
}

is_live_esgf_transient_warning <- function(message) {
    grepl(
        paste(
            c(
                "Failed to open",
                "Download failed \\(attempt",
                "Empty reply from server",
                "Timeout was reached",
                "Could not resolve host",
                "SSL connect error",
                "Recv failure"
            ),
            collapse = "|"
        ),
        message
    )
}

allow_live_esgf_transient_warnings <- function(expr) {
    withCallingHandlers(
        force(expr),
        warning = function(w) {
            if (is_live_esgf_transient_warning(conditionMessage(w))) {
                invokeRestart("muffleWarning")
            }
        }
    )
}
