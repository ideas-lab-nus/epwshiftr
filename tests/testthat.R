library(testthat)
library(epwshiftr)

# The built package keeps its ordinary test entry. Repository CI explicitly
# supplies an external runner that is excluded from the source package.
runner <- Sys.getenv("EPWSHIFTR_TEST_RUNNER", "")
if (nzchar(runner)) {
    source(runner, local = TRUE)
} else {
    test_check("epwshiftr")
}

# vim: fdm=marker :
