library(testthat)
library(epwshiftr)

# Linux CI enables every test, including the authenticated live services. Keep
# ordinary package checks portable, but never accept skips as a full test run.
require_all_tests <- identical(
    Sys.getenv("EPWSHIFTR_REQUIRE_ALL_TESTS"),
    "true"
)
if (require_all_tests) {
    required_flags <- c(
        "NOT_CRAN",
        "EPWSHIFTR_RUN_LIVE_ESGF",
        "EPWSHIFTR_RUN_LIVE_ERA5"
    )
    if (any(Sys.getenv(required_flags) != "true")) {
        stop("Full testing requires NOT_CRAN and both live service flags.")
    }
}

results <- test_check("epwshiftr")
if (require_all_tests) {
    result_summary <- as.data.frame(results)
    skipped_tests <- result_summary[result_summary$skipped, c("file", "test")]
    if (nrow(skipped_tests)) {
        stop(
            "Full testing does not allow skipped tests:\n",
            paste(
                paste(skipped_tests$file, skipped_tests$test, sep = ": "),
                collapse = "\n"
            ),
            call. = FALSE
        )
    }
}

# vim: fdm=marker :
