library(testthat)
library(epwshiftr)

# CI can shard complete files while ordinary local checks retain a serial
# diagnostic entry point. The worker calls test_check directly, never this entry.
shard_setting <- Sys.getenv("EPWSHIFTR_TEST_SHARDS", "1")
if (identical(shard_setting, "auto")) {
    source("support/check-parallel.R")
    shards <- checks__default_shards()
} else {
    shards <- suppressWarnings(as.integer(shard_setting))
}
if (is.na(shards) || shards < 1L || shards > 8L) {
    stop("EPWSHIFTR_TEST_SHARDS must be 'auto' or an integer from 1 to 8.")
}
if (shards == 1L) {
    test_check("epwshiftr")
} else {
    if (!identical(shard_setting, "auto")) {
        source("support/check-parallel.R")
    }
    checks__run(
        library = dirname(find.package("epwshiftr")),
        output = tempfile("parallel-tests-"),
        shards = shards,
        durations = read.csv("support/test-durations.csv"),
        test_root = normalizePath(".", winslash = "/")
    )
}

# vim: fdm=marker :
