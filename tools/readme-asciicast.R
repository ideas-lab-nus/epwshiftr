# Execute the displayed README example in a real terminal and retain its cast
# outside the repository. Only playback timing changes in the rendered SVGs.
# readme__terminal {{{
readme__terminal <- function(options) {
    options$engine <- "r"
    if (!isTRUE(options$eval)) {
        return(knitr::engine_output(options, options$code, ""))
    }
    root <- normalizePath(".", winslash = "/")
    cache <- file.path(tools::R_user_dir("epwshiftr", "cache"), "readme")
    inputs <- c(
        "DESCRIPTION",
        "tools/readme-asciicast.R",
        list.files("R", pattern = "[.]R$", full.names = TRUE),
        file.path(cache, c("Singapore.epw", "SanFrancisco.epw"))
    )
    key <- digest::digest(
        list(
            code = options$code,
            inputs = unname(tools::md5sum(inputs)),
            cache = cache
        ),
        algo = "sha256"
    )
    recording <- file.path(cache, "terminal", key)
    dir.create(recording, recursive = TRUE, showWarnings = FALSE)
    cast_path <- file.path(recording, "session.cast")
    result_path <- file.path(recording, "results.rds")

    if (!file.exists(result_path) || !file.exists(cast_path)) {
        if (!nzchar(Sys.which("asciinema"))) {
            stop(
                "Install asciinema to record a changed README example.",
                call. = FALSE
            )
        }
        code_path <- file.path(recording, "example.R")
        runner <- file.path(recording, "run.R")
        writeLines(options$code, code_path)
        # The child uses this R and its libraries; no shell startup or user
        # profile is needed. The displayed example is the only workflow call.
        literal <- function(x) paste(capture.output(dput(x)), collapse = "\n")
        writeLines(
            c(
                paste0(".libPaths(", literal(.libPaths()), ")"),
                paste0(
                    "pkgload::load_all(",
                    literal(root),
                    ", quiet = TRUE, export_all = FALSE)"
                ),
                paste0("setwd(", literal(cache), ")"),
                # Match an ordinary colour-capable terminal. Choose DuckDB's
                # default storage explicitly so repeated driver notices cannot
                # interrupt the package's live framebuffer.
                "options(width = 110, cli.width = 110, cli.num_colors = 256, epwshiftr.ui_height = 30)",
                "readme_driver <- duckdb::duckdb(shared_home = TRUE)",
                "stopifnot(isatty(stdout()), isatty(stderr()))",
                "cat('\\033[2J\\033[H')",
                paste0(
                    "expressions <- parse(",
                    literal(code_path),
                    ", keep.source = TRUE)"
                ),
                # Execute each original top-level expression with a brief reading
                # pause. Both command echo and output come from R's own source().
                "for (reference in attr(expressions, 'srcref')) {",
                "    source(textConnection(as.character(reference)), local = .GlobalEnv,",
                "        echo = TRUE, keep.source = TRUE, max.deparse.length = Inf)",
                "    Sys.sleep(8)",
                "}",
                paste0(
                    "saveRDS(list(batch_id = shift_ids(batch, refresh = FALSE)$batch_id), ",
                    literal(result_path),
                    ")"
                ),
                "Sys.sleep(8)"
            ),
            runner
        )
        command <- paste(
            shQuote(file.path(R.home("bin"), "Rscript")),
            "--vanilla",
            shQuote(runner)
        )
        status <- system2(
            "asciinema",
            c(
                "record",
                "--headless",
                "--return",
                "--quiet",
                "--output-format",
                "asciicast-v2",
                "--window-size",
                "110x34",
                "--overwrite",
                "--command",
                shQuote(command),
                shQuote(cast_path)
            ),
            env = c("TERM=xterm-256color", "R_PROFILE_USER=", "R_ENVIRON_USER=")
        )
        if (status != 0L || !file.exists(result_path)) {
            stop("README recording failed; inspect ", recording, call. = FALSE)
        }
    }

    # Keep every captured output event. Speed up processing and cap long waits;
    # the original, unmodified timings remain in the locally cached cast.
    cast <- asciicast::read_cast(cast_path)
    times <- cast$output$time
    cast$output$time <- cumsum(pmin(c(times[1L], diff(times)), 8)) / 4
    files <- file.path(
        "man/figures/README",
        c("example.svg", "example-dark.svg")
    )
    dir.create(dirname(files[1L]), recursive = TRUE, showWarnings = FALSE)
    for (index in seq_along(files)) {
        asciicast::write_svg(
            cast,
            files[index],
            at = "all",
            rows = 34,
            cols = 110,
            theme = c("github-light", "github-dark")[index],
            window = TRUE,
            cursor = FALSE,
            omit_last_line = FALSE,
            show = FALSE
        )
    }
    # Leave a machine-readable pointer for local verification without adding
    # raw recordings, EPWs, or cache receipts to the published package.
    writeLines(recording, file.path(cache, "terminal", "latest"))
    picture <- paste0(
        '<picture>\n<source media="(prefers-color-scheme: dark)" srcset="',
        files[2L],
        '">\n',
        '<img src="',
        files[1L],
        '" width="100%" alt="Actual terminal recording of ',
        'two-city future EPW generation.">\n</picture>'
    )
    knitr::engine_output(options, options$code, "", knitr::asis_output(picture))
}
# }}}

# vim: fdm=marker :
