verbose <- function (..., sep = "") {
    if (getOption("epwshiftr.verbose", FALSE)) {
        cat(..., "\n", sep = sep)
    }
}
