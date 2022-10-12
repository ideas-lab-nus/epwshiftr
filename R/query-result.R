EsgfQueryResult <- R6::R6Class("EsgfQueryResult",
    lock_class = TRUE,
    public = list(
        initialize = function(host, result) {
            private$url_host <- host
            private$result <- result
            self
        },
        to_dt = function(fields = NULL, formatted = NULL) {
            docs <- private$get_docs()

            assert_subset(fields, names(docs))
            assert_character(formatted, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

            if (length(fields)) {
                docs <- docs[match(fields, names(docs))]
            }
            if (!is.null(formatted)) {
                for (field in formatted) {
                    if (is.null(docs[[field]])) next
                    docs[[field]] <- self[[field]]
                }
            }

            res <- data.table::setDT(
                lapply(docs, function(doc) {
                    if (typeof(doc) == "list" && all(lengths(doc) == 1L)) {
                        doc <- unlst(doc)
                    }
                    doc
                })
            )

            res
        },
        count = function() {
            length(self$id)
        }
    ),

    active = list(
        id = function() {
            private$get_field("id")
        },
        url = function() {
            urls <- private$get_field("url")
            if (!length(urls)) return(NULL)

            lapply(urls, function(url) {
                if (!length(url)) return(NULL) # nocov

                s <- strsplit(url, "|", fixed = TRUE)
                # nocov start
                if (any(unreco <- lengths(s) != 3L)) s[unreco] <- NULL
                if (!length(s)) return(NULL)
                # nocov end

                res <- data.table::setDT(data.table::transpose(s))
                data.table::setcolorder(res, c(3L, 1L, 2L))
                data.table::setnames(res, c("service", "url", "mime_type"))
                res
            })
        },
        size = function() {
            size <- private$get_field("size")
            set_size_units(size)
        },
        fields = function() {
            sort(names(private$get_docs()))
        }
    ),
    private = list(
        url_host = NULL,
        result = NULL,
        response = NULL,
        params = list(),
        required_fields = c("id", "url", "size"),

        get_docs = function() {
            private$result$response$docs
        },

        get_field = function(field) {
            val <- private$result$response$docs[[field]]
            if (all(lengths(val) == 1L)) unlst(val) else val
        },

        has_facet_cache = function() {
            !is.null(private$url_host) && !is.null(this$cache[[private$url_host]])
        },

        facet_cache = function() {
            this$cache[[private$url_host]]
        },

        validate_fields = function(fields) {
            if (is.null(fields)) return(NULL)
            if ("*" %in% fields) fields <- "*"
            choices <- unlst(private$facet_cache()$responseHeader$params$facet.field)
            choices <- c("*", choices)

            # add required fields
            if (!"*" %in% fields && any(miss <- !private$required_fields %in% fields)) {
                fields <- c(fields, private$required_fields[miss])
            }

            assert_subset(fields, empty.ok = TRUE, .var.name = "fields", choices = choices)
            unique(fields)
        },

        validate_shards = function(shards) {
            if (is.null(shards)) return(NULL)
            choices <- query_esgf(private$url_host)$list_all_shards()
            # suffix should be excluded when query
            choices <- self$list_all_shards()
            if (length(choices)) {
                choices <- gsub("(?<=/solr).+", "", choices, perl = TRUE)
            }
            assert_subset(shards, empty.ok = TRUE, .var.name = "shards", choices = choices)
            unique(shards)
        },

        build_params = function(fields = NULL, shards = NULL, replica = NULL, latest = TRUE, type = "File") {
            assert_flag(replica, null.ok = TRUE)
            assert_flag(latest)
            assert_choice(type, c("File", "Aggregation"))
            fields <- private$validate_fields(fields)
            shards <- private$validate_shards(shards)

            private$params <- list(
                dataset_id = self$id,
                fields = fields,
                shards = shards,
                replica = replica,
                latest = latest,
                type = type,
                format = "application/solr+json"
            )
            private$params
        }
    )
)

EsgfQueryResultDataset <- R6::R6Class("EsgfQueryResultDataset",
    inherit = EsgfQueryResult, lock_class = TRUE,
    public = list(
        to_dt = function(fields = NULL, formatted = FALSE) {
            assert_flag(formatted)
            super$to_dt(fields, if (formatted) c("url", "size"))
        },
        has_opendap = function() {
            vapply(self$access, function(acc) "OPENDAP" %in% acc, logical(1L))
        },
        has_download = function() {
            vapply(self$access, function(acc) "HTTPServer" %in% acc, logical(1L))
        },
        collect = function(fields = NULL, shards = NULL, replica = NULL, latest = TRUE, type = "File") {
            params <- private$build_params(
                fields = fields,
                shards = shards,
                replica = replica,
                latest = latest,
                type = type
            )

            url <- query_build(private$url_host, params)
            private$response <- read_json_response(url)

            if (type == "File") {
                new_query_result(
                    EsgfQueryResultFile,
                    private$url_host, private$response
                )
            } else if (type == "Aggregation") {
                new_query_result(
                    EsgfQueryResultAggregation,
                    private$url_host, private$response
                )
            }
        },
        print = function() {
            d <- cli::cli_div(
                theme = list(rule = list("line-type" = "double"))
            )
            cli::cli_rule("ESGF Query Result [Dataset]")
            cli::cli_li("Host: {private$url_host}")

            d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
            ul <- cli::cli_ul()

            cli::cli_li("Dataset ID: {self$id}")

            cli::cli_end(ul)
            cli::cli_end(d)

            invisible(self)
        }
    ),
    private = list(
        required_fields = c(
            EsgfQueryResult$private_fields$required_fields,
            "index_node", "number_of_fields", "access"
        )
    )
)

EsgfQueryResultFile <- R6::R6Class("EsgfQueryResultFile",
    inherit = EsgfQueryResult, lock_class = TRUE,
    active = list(
        filename = function() {
            private$get_field("title")
        },
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")
            has_html <- tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },
        url_download = function() {
            private$get_url("HTTPServer")
        },
        fields = function() {
            c("filename", "url_opendap", "url_download", super$fields)
        }
    ),
    private = list(
        required_fields = c(
            EsgfQueryResult$private_fields$required_fields,
            "dataset_id", "checksum", "checksum_type", "tracking_id", "title"
        ),

        get_url = function(type, name = type) {
            vapply(self$url, function(dt_url) {
                if (!length(dt_url)) return(NA_character_)

                res <- dt_url$url[dt_url$service == type]
                if (!length(res)) return(NA_character_)
                # nocov start
                if (length(res) > 1L) {
                    warning(sprintf("Multiple %s URL found. Only the first is returned.", name))
                    res <- res[[1L]]
                }
                # nocov end

                res
            }, character(1L))
        }
    )
)

EsgfQueryResultAggregation <- R6::R6Class("EsgfQueryResultAggregation",
    inherit = EsgfQueryResult, lock_class = TRUE
)

new_query_result <- function(generator, host, result, ..., .env = parent.frame()) {
    if (generator$is_locked()) {
        generator$unlock()
        on.exit(generator$lock(), add = TRUE)
    }

    fld_exist <- names(generator$active)
    # also check super class
    super <- generator$inherit
    while (!is.null(super <- eval(super, .env))) {
        fld_exist <- union(fld_exist, names(super$active))
        super <- super$inherit
    }

    fld_all <- names(result$response$docs)
    fld_miss <- setdiff(fld_all, fld_exist)
    if (length(fld_miss)) {
        for (field in fld_miss) {
            eval(substitute(
                generator$set(
                    "active", field,
                    function() {
                        private$get_field(field)
                    }
                ),
                list(field = field)
            ))
        }

        on.exit({
            for (field in fld_miss) {
                generator$active[[field]] <- NULL
            }
        }, add = TRUE)
    }
    generator$new(host, result, ...)
}
