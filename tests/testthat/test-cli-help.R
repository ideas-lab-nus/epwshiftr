# epwshiftr_cli_help() {{{
test_that("epwshiftr_cli_help() resolves root, group, and command topics without creating a store", {
    missing_dir <- tempfile("missing-store-")
    topics <- list(
        list(args = "help", pattern = "Usage: epwshiftr"),
        list(args = c("query", "help"), pattern = "Usage: epwshiftr query"),
        list(
            args = c("download", "run", "--help"),
            pattern = "Usage: epwshiftr download run"
        ),
        list(
            args = c("help", "shift", "config", "validate"),
            pattern = "Usage: epwshiftr shift config validate"
        ),
        list(
            args = c("help", "storage", "validate"),
            pattern = "Usage: epwshiftr storage validate"
        ),
        list(args = c("doctor", "--help"), pattern = "Usage: epwshiftr doctor")
    )

    for (topic in topics) {
        res <- epwshiftr_cli(c("--quiet", "--store", missing_dir, topic$args))
        expect_equal(res$status, 0L)
        expect_match(res$result[[1L]], topic$pattern)
        expect_false(dir.exists(missing_dir))
    }

    morph_run <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        missing_dir,
        "help",
        "morph",
        "run"
    ))
    expect_equal(morph_run$status, 0L)
    expect_match(
        morph_run$result[[1L]],
        "--scale monthly|daily|hourly",
        fixed = TRUE
    )
    expect_match(morph_run$result[[1L]], "--method NAME", fixed = TRUE)

    bad_help <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        missing_dir,
        "help",
        "missing"
    ))
    expect_equal(bad_help$status, 2L)
    expect_match(bad_help$error, "Unknown help topic")
    expect_false(dir.exists(missing_dir))
})
# }}}

# vim: fdm=marker :
