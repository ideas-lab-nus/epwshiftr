# Run from R CMD check's tests directory. The workflow supplies this absolute
# source path so neither the scheduler nor its dependencies enter the package.
runner_dir <- dirname(Sys.getenv("EPWSHIFTR_TEST_RUNNER"))
source(file.path(runner_dir, "check-parallel.R"), local = TRUE)
shard_setting <- Sys.getenv("EPWSHIFTR_TEST_SHARDS", "auto")
shards <- if (identical(shard_setting, "auto")) {
    checks__default_shards()
} else {
    suppressWarnings(as.integer(shard_setting))
}
if (is.na(shards) || shards < 1L || shards > 8L) {
    stop("EPWSHIFTR_TEST_SHARDS must be 'auto' or an integer from 1 to 8.")
}
if (shards == 1L) {
    test_check("epwshiftr")
} else {
    checks__run(
        library = dirname(find.package("epwshiftr")),
        output = tempfile("parallel-tests-"),
        shards = shards,
        test_root = normalizePath(".", winslash = "/")
    )
}

# vim: fdm=marker :
