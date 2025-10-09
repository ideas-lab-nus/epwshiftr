transform_lines <- function(lines) {
    json <- jsonlite::fromJSON(lines, simplifyVector = TRUE, simplifyMatrix = FALSE)

    if (!is.null(json$index_node)) json$index_node <- "..."

    if (!is.null(json$last_result)) {
        if (!is.null(json$last_result$index_node)) {
            json$last_result$index_node <- "..."
        }

        if (!is.null(json$last_result$response)) {
            json$last_result$response <- transform_json_response(json$last_result$response)
        }
    }

    strsplit(jsonlite::toJSON(json, null = "null", na = "null", digits = 8, pretty = TRUE), "\n")[[1L]]
}

transform_json_response <- function(response) {
    response$responseHeader$QTime <- -1L
    response$responseHeader$params$shards <- "..."
    response$response$numFound <- -1L
    response$response$maxScore <- 0.5

    if (!is.null(response$timestamp)) {
        response$timestamp <- as.POSIXct("2020-02-02 22:22:22.123456", "UTC")
    }
    if (!is.null(response$cache)) {
        response$cache <- "response-a1b2c3d4"
    }

    docs <- response$response$docs
    if (NROW(docs)) {
        for (i in seq_along(docs)) {
            col <- docs[[i]]

            if (is.list(col)) {
                for (j in seq_along(col)) {
                    elem <- col[[j]]
                    if (is.character(elem)) {
                        col[[j]] <- rep("...", length(elem))
                    } else if (is.logical(elem)) {
                        col[[j]] <- rep(TRUE, length(elem))
                    } else if (is.integer(elem)) {
                        col[[j]] <- rep(-1L, length(elem))
                    } else if (is.numeric(elem)) {
                        col[[j]] <- rep(-1.5, length(elem))
                    }
                }
                docs[[i]] <- col
            } else {
                if (is.character(col)) {
                    docs[[i]] <- rep("...", length(col))
                } else if (is.logical(col)) {
                    docs[[i]] <- rep(TRUE, length(col))
                } else if (is.integer(col)) {
                    docs[[i]] <- rep(-1L, length(col))
                } else if (is.numeric(col)) {
                    docs[[i]] <- rep(-1.5, length(col))
                }
            }
        }
        response$response$docs <- docs
    }

    response
}
