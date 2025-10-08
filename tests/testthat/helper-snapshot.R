transform_lines <- function(lines) {
    lines <- readLines("/var/folders/8f/t8sk2pps6135xbp47cs8qb2r0000gn/T//RtmpUqcafq/file91e62791276.json")

    json <- jsonlite::fromJSON(lines, simplifyVector = TRUE, simplifyMatrix = FALSE)

    json$index_node <- "..."

    json$last_result$index_node <- "..."
    json$last_result$response$responseHeader$QTime <- -1L
    json$last_result$response$responseHeader$params$shards <- "..."
    json$last_result$response$response$numFound <- -1L
    json$last_result$response$response$maxScore <- 0.5
    json$last_result$response$timestamp <- as.POSIXct("2020-02-02 22:22:22.123456", "UTC")
    if (!is.null(json$last_result$response$cache)) {
        json$last_result$response$cache <- "response-a1b2c3d4"
    }

    docs <- json$last_result$response$response$docs
    if (nrow(docs)) {
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
        json$last_result$response$response$docs <- docs
    }

    strsplit(jsonlite::toJSON(json, null = "null", na = "null", digits = 8, pretty = TRUE), "\n")[[1L]]
}
