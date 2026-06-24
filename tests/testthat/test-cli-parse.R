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
# epwshiftr_cli_csv() {{{
test_that("epwshiftr_cli_csv() validates scalar inputs", {
    expect_equal(epwshiftr_cli_csv("tas, pr"), c("tas", "pr"))
})
# }}}
# epwshiftr_cli_count() {{{
test_that("epwshiftr_cli_count() validates scalar inputs", {
    expect_equal(epwshiftr_cli_count("2", "limit"), 2L)
    expect_error(epwshiftr_cli_count("0", "limit"), ">= 1", fixed = TRUE)
})
# }}}
# epwshiftr_cli_count_or_default() {{{
test_that("epwshiftr_cli_count_or_default() validates scalar inputs", {
    expect_equal(epwshiftr_cli_count_or_default(NULL, "limit", 5L), 5L)
})
# }}}
# epwshiftr_cli_bool() {{{
test_that("epwshiftr_cli_bool() validates scalar inputs", {
    expect_true(epwshiftr_cli_bool("true", "latest"))
    expect_false(epwshiftr_cli_bool("false", "latest"))
    expect_error(epwshiftr_cli_bool("maybe", "latest"), "true or false")
})
# }}}
# epwshiftr_cli_empty_to_null() {{{
test_that("epwshiftr_cli_empty_to_null() validates scalar inputs", {
    expect_equal(epwshiftr_cli_empty_to_null(""), "")
})
# }}}
