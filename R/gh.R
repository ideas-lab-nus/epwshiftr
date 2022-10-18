gh_token <- function(token = NULL, header = TRUE) {
    checkmate::assert_string(token, null.ok = TRUE)

    if (is.null(token)) {
        token <- Sys.getenv("GITHUB_PAT")

        if (token == "") {
            token <- Sys.getenv("GITHUB_TOKEN")
        }

        if (token == "") return(NULL)
    }

    token <- trim_ws(token)
    if (!nchar(token)) return(NULL)

    if (!grepl("^gh[pousr]_[A-Za-z0-9_]{36,251}$", token) &&
        !grepl("[[:xdigit:]]{40}", token)) {

        return(NULL)
    } else {
        if (!header) {
            return(token)
        } else {
            return(c("Authorization" = paste("token", token)))
        }
    }
}

gh <- function(path, token = NULL) {
    base_url <- "https://api.github.com"
    link <- file.path(base_url, path)

    # use GitHub token if possible
    headers <- c("Accept" = "application/vnd.github+json")

    if (!is.null(token <- gh_token(token))) {
        headers <- c(headers, token)
    }

    jsonlite::fromJSON(base::url(link, headers = headers))
}

# TODO: pagination for tags
gh_tags <- function(repo, token = NULL) {
    gh(sprintf("repos/%s/tags", repo), token)
}

download_gh_file <- function(repo, tag, file, dir = tempdir(), token = NULL) {
    url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, tag, file)
    dest <- file.path(dir, file)

    utils::download.file(url, dest, mode = "wb", headers = gh_token(token), quiet = TRUE)

    normalizePath(dest, mustWork = TRUE)
}

download_gh_tag <- function(repo, tag, dir = tempdir(), token = NULL) {
    url <- sprintf("https://api.github.com/repos/%s/zipball/refs/tags/%s", repo, tag)
    dest <- file.path(dir, sprintf("%s-%s.zip", basename(repo), tag))

    utils::download.file(url, dest, mode = "wb", headers = gh_token(token), quiet = TRUE)

    normalizePath(dest, mustWork = TRUE)
}
