# Build a dependency-free stand-in for the public eplusr Epw protocol so input
# compatibility can be tested without declaring eplusr in Suggests.
test_external_epw <- function(path) {
    generator <- R6::R6Class(
        "Epw",
        public = list(
            source_path = NULL,
            initialize = function(path) {
                self$source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
            },
            path = function() {
                self$source_path
            },
            location = function() {
                epw_file_read(self$source_path)$location()
            },
            save = function(path, overwrite = FALSE) {
                if (file.exists(path) && !isTRUE(overwrite)) {
                    stop("Target exists.")
                }
                dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
                if (!isTRUE(file.copy(self$source_path, path, overwrite = overwrite))) {
                    stop("Failed to save fake Epw.")
                }
                self$source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
                invisible(self$source_path)
            }
        )
    )
    generator$new(path)
}
