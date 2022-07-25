test_that("gh_token() works", {
    skip_on_cran()

    pat <- Sys.getenv("GITHUB_PAT")
    Sys.setenv("GITHUB_PAT" = "")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = "   ")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = "abc")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = pat)
    expect_null(names(gh_token(header = FALSE)))
})
