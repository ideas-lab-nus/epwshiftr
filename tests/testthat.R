library(testthat)
library(epwshiftr)

# Coverage runs in a separate R CMD BATCH process. Emit file-level progress
# there so its supervisor can locate a stalled suite without changing tests.
if (identical(Sys.getenv("R_COVR"), "true")) {
    options(testthat.default_check_reporter = "summary")
}

test_check("epwshiftr")

# vim: fdm=marker :
