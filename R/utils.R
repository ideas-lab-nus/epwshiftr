verbose <- function (..., sep = "\n") {
    if (getOption("epwshiftr.verbose", FALSE)) {
        cat(..., sep = sep)
    }
}
