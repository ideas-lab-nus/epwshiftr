if (!file.exists("DESCRIPTION")) {
    stop("Run this script from the package root.", call. = FALSE)
}

source("tools/raw-vignette-freshness.R")

vignette__check_checksum_manifest()

message("Precompiled raw vignettes are fresh.")
