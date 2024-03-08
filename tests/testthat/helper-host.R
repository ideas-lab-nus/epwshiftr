get_fast_host <- function(force = FALSE) {
    if (is.null(this$fast_host)) force <- TRUE
    if (!force) return(this$fast_host)

    hosts <- c(
        llnl = "https://esgf-node.llnl.gov/esg-search",
        ipsl = "https://esgf-node.ipsl.upmc.fr/esg-search",
        ceda = "https://esgf.ceda.ac.uk/esg-search",
        dkrz = "https://esgf-data.dkrz.de/esg-search"
    )

    conn_time <- function(host) {
        before <- now()
        res <- try(readLines(substr(host, 1L, nchar(host) - 11L), n = 1L, warn = FALSE), silent = TRUE)
        if (inherits(res, "try-error")) res <- Inf
        after <- now()
        as.numeric(after - before)
    }

    times <- vapply(hosts, conn_time, double(1L))
    # all failed to connect, just use the first one
    if (all(is.infinite(times))) return(hosts[1L])

    (this$fast_host <- hosts[which.min(times)])
}
