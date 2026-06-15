# epwshiftr_cli_parse_command() {{{
test_that("epwshiftr_cli_parse_command() separates flags, options, and positionals", {
    parsed <- epwshiftr_cli_parse_command(
        c("--dry-run", "--limit", "2", "--tag", "tas", "--tag", "daily", "project=CMIP6"),
        flags = "--dry-run",
        options = "--limit",
        multi_options = "--tag"
    )
    expect_true(parsed$flags[["--dry-run"]])
    expect_equal(parsed$options[["--limit"]], "2")
    expect_equal(parsed$options[["--tag"]], c("tas", "daily"))
    expect_equal(parsed$positionals, "project=CMIP6")
})
# }}}
# epwshiftr_cli_csv() / epwshiftr_cli_count() / epwshiftr_cli_count_or_default() / epwshiftr_cli_bool() / epwshiftr_cli_empty_to_null() {{{
test_that("epwshiftr_cli_csv() / epwshiftr_cli_count() / epwshiftr_cli_count_or_default() / epwshiftr_cli_bool() / epwshiftr_cli_empty_to_null() validate scalar inputs", {
    expect_equal(epwshiftr_cli_csv("tas, pr"), c("tas", "pr"))
    expect_equal(epwshiftr_cli_count("2", "limit"), 2L)
    expect_equal(epwshiftr_cli_count_or_default(NULL, "limit", 5L), 5L)
    expect_true(epwshiftr_cli_bool("true", "latest"))
    expect_false(epwshiftr_cli_bool("false", "latest"))
    expect_equal(epwshiftr_cli_empty_to_null(""), "")
    expect_error(epwshiftr_cli_count("0", "limit"), ">= 1", fixed = TRUE)
    expect_error(epwshiftr_cli_bool("maybe", "latest"), "true or false")
})
# }}}
