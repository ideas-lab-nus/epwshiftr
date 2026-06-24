transform_json <- function(lines) {
    json <- jsonlite::fromJSON(lines, simplifyVector = TRUE, simplifyMatrix = FALSE)

    if (!is.null(json$index_node)) json$index_node <- "..."

    if (!is.null(json$response)) {
        json$response <- transform_json_response(json$response)
    }

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

transform_print <- function(lines) {
    lines[grepl("^\\* Collected at: \\d", lines)] <- "* Collected at: yyyy-mm-dd HH:MM:SS"
    lines[grepl("^\\* Total size: [1-9]", lines)] <- "* Total size: XX [GiB]"

    # replace date and data node
    # datasets
    lines <- gsub("ssp[0-9]+", "sspXXX", lines)
    lines <- gsub("\\d{8}\\|\\w.+(,)?$", "20200202|esgf.data.node\\1", lines)
    # files
    lines <- gsub("(_g\\w+_)\\d{8}-\\d{8}\\.nc\\|\\w.+$", "\\120200101-20211231.nc|esgf.data.node", lines)
    # aggregations
    lines <- gsub("\\d{8}.aggregation.*?\\|\\w.+$", "20200101.aggregration|esgf.data.node", lines)

    # replace file size and access methods
    lines <- gsub("^\\* Fields: [0-9]+ \\| \\[", "* Fields: XX | [", lines)
    # datasets
    lines <- gsub("\\d+ Files, \\d+\\.\\d+ [MG]iB \\| \\d+ Aggregation[s]?", "XX Files, XX GiB | X Aggregations", lines)
    lines <- gsub("\\[ Access: <.+> \\]$", "[ Access: <...> ]", lines)
    # files
    lines <- gsub("\\d+\\.\\d+ [MGT]iB \\| Access: <.+>", "XX MiB | Access: <...>", lines)
    # aggregations
    lines <- gsub("(<Unknown> Byte \\| Access: )<.+>", "\\1<...>", lines)

    dataset_param <- grepl("^\\s+CMIP6\\.", lines)
    duplicated_previous <- c(FALSE, lines[-1L] == lines[-length(lines)])
    lines <- lines[!(dataset_param & duplicated_previous)]

    lines
}
