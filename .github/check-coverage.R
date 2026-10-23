# Compare unmodified covr, cached instrumentation and a deliberately killed worker.
args <- commandArgs(TRUE)
source(if (length(args)) args[[1L]] else ".github/coverage.R")
root <- tempfile('coverage-parity-')
dir.create(root)
pkg <- file.path(root, 'coveragefixture')
dir.create(file.path(pkg, 'R'), recursive = TRUE)
writeLines(
    c(
        'Package: coveragefixture',
        'Version: 0.0.1',
        'Title: Coverage fixture',
        'Description: Exercise source counters.',
        'License: MIT',
        'Author: Test',
        'Maintainer: Test <test@example.org>',
        'Imports: R6'
    ),
    file.path(pkg, 'DESCRIPTION')
)
writeLines(c('export(value)', 'export(Reader)'), file.path(pkg, 'NAMESPACE'))
writeLines(
    c(
        '# Include nested closures and branches in cached bodies.',
        'value <- function(x = c(1, 2), unused) {',
        '    transform <- function(y) { if (y > 0) y * 2 else -y }',
        '    vapply(x, transform, numeric(1))',
        '}',
        '# R6 method environments must remain tied to their current generator.',
        'Reader <- R6::R6Class("Reader", public = list(',
        '    x = 3, read = function() { value(self$x) }',
        '))'
    ),
    file.path(pkg, 'R', 'fixture.R')
)
work <- 'library(coveragefixture); stopifnot(identical(value(), c(2, 4))); stopifnot(identical(Reader$new()$read(), 6))'
normal <- sprintf('callr::r(function() { %s }, libpath = .libPaths())', work)
marker <- file.path(root, 'done')
forced <- sprintf(
    paste0(
        'p <- callr::r_bg(function(marker) { %s; writeLines("done", marker); repeat Sys.sleep(1) }, args = list(%s), libpath = .libPaths(), supervise = TRUE); ',
        'end <- Sys.time() + 30; while (!file.exists(%s) && p$is_alive() && Sys.time() < end) Sys.sleep(.01); ',
        'stopifnot(file.exists(%s)); p$kill(); p$wait(5000); stopifnot(!p$is_alive())'
    ),
    work,
    deparse(marker),
    deparse(marker),
    deparse(marker)
)
baseline <- covr::package_coverage(
    pkg,
    type = 'none',
    code = normal,
    quiet = FALSE,
    install_path = file.path(root, 'baseline')
)
adapted <- coverage__run(
    pkg,
    type = 'none',
    code = normal,
    quiet = FALSE,
    install_path = file.path(root, 'adapted')
)
killed <- coverage__run(
    pkg,
    type = 'none',
    code = forced,
    quiet = FALSE,
    install_path = file.path(root, 'killed')
)
# Extract comparable covr hit counts without result attributes.
coverage__counts <- function(x) vapply(x, function(z) z$value, numeric(1))
stopifnot(
    identical(coverage__counts(baseline), coverage__counts(adapted)),
    identical(coverage__counts(baseline), coverage__counts(killed))
)
cat(
    'PASS: exact source keys and hit counts match original covr, including R6, cached worker and hard-killed worker.\n'
)

# vim: fdm=marker :
