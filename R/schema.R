# schema_validate {{{
schema_validate <- function(data, schema, name = NULL, mode = "assert") {
    mode <- checkmate::assert_choice(mode, c("check", "expect", "test", "assert"))
    if (is.null(name)) name <- deparse(substitute(data))

    cmfun <- function(type) {
        get(paste0(mode, "_", type), asNamespace("checkmate"), inherits = FALSE)
    }

    compose_args <- function(x, args, loc) {
        args <- c(list(x = x), args)
        if (mode == "expect") args <- c(args, list(label = loc))
        if (mode == "assert") args <- c(args, list(.var.name = loc))
        args
    }

    check_name <- function(node, names, loc, type) {
        if (!length(names) || (is.list(type) && checkmate::test_true(type$null.ok))) {
            return(switch(mode,
                assert = invisible(node),
                check = TRUE,
                test = TRUE,
                expect = checkmate::makeExpectation(node, TRUE)
            ))
        }

        do.call(cmfun("names"), compose_args(names(node), names, loc))
    }

    check_type <- function(node, type, loc) {
        if (checkmate::test_string(type)) {
            do.call(cmfun(type), compose_args(node, type[-1L], loc))
        } else if (checkmate::test_list(type)) {
            if (!checkmate::test_string(type[[1L]])) {
                stop(paste(
                    "Invalid schema definition for 'type' found.",
                    "When it is a list, the first element should be a string",
                    "indicating the type of this node,",
                    "but a", sprintf("['%s']", typeof(type)), "is found."
                ))
            }

            fun <- cmfun(type[[1L]])
            args <- type[-1L]
            rules <- args$rules
            # for qassertr, qtestr and qexpectr
            if (do_rules <- type[[1L]] == "list" && !is.null(rules)) {
                args <- args[names(args) != "rules"]
            }
            do.call(fun, compose_args(node, args, loc))
            if (do_rules) {
                if (mode == "check") mode <- "test"
                quick <- get(paste0("q", mode, "r"), asNamespace("checkmate"), inherits = FALSE)
                do.call(quick, compose_args(node, list(rules = rules), loc))
            }
        } else {
            stop(paste(
                "Invalid schema definition for 'type' found.",
                "It should be either a string or a list,",
                "but a", sprintf("['%s']", typeof(type)), "is found."
            ))
        }
    }

    is_empty_list <- function(node, schema) {
        is.list(schema$type) && isTRUE(schema$type$null.ok) && is.list(node) && length(node) == 0L
    }

    check <- function(node, schema, loc) {
        check_name(node, schema$names, loc, schema$type)
        check_type(node, schema$type, loc)

        # check fields only if current node is not NULL or is not an empty list
        if (length(schema$fields) && !is.null(node) && !is_empty_list(node, schema)) {
            if ("..." %in% names(schema$fields)) {
                fld_loc <- paste(loc, "{ELEMENT}", sep = "$")
                # all other nodes
                fld_schema <- .subset2(.subset2(schema, "fields"), "...")
                for (fld_node in node[!names(node) %in% c("...", names(schema$fields))]) {
                    check(fld_node, fld_schema, fld_loc)
                }
            }
            for (field in setdiff(names(schema$fields), "...")) {
                fld_loc <- paste(loc, field, sep = "$")
                fld_node <- .subset2(node, field)
                fld_schema <- .subset2(.subset2(schema, "fields"), field)

                check(fld_node, fld_schema, fld_loc)
            }
        }
    }

    check(data, schema, name)
}
# }}}

# SCHEMA_RESPONSE {{{
SCHEMA_RESPONSE <- local({
    list(
        type = list("list", null.ok = TRUE, names = "unique"),
        names = list(must.include = c("responseHeader", "response", "facet_counts", "timestamp")),
        fields = list(
            responseHeader = list(
                type = list("list",
                    c("logical", "numeric", "character", "null", "list"),
                    null.ok = TRUE, names = "unique"
                ),
                names = list(must.include = c("status", "QTime", "params")),
                fields = list(
                    status = list(type = "int"),
                    QTime = list(type = "int"),
                    params = list(type = "list",
                        names = list(must.include = c("facet.field", "df",
                            "q.alt", "indent", "echoParams", "fl", "start", "fq",
                            "rows", "q", "tie", "facet.limit", "qf",
                            "facet.method", "facet.mincount", "facet", "wt",
                            "facet.sort", "shards", "fields"
                        )),
                        fields = list(
                            facet.field = list(
                                type = list("list", "character", any.missing = FALSE),
                                "..." = list(type = list("list", rules = "S1"))
                            ),
                            df = list(type = "string"),
                            q.alt = list(type = "string"),
                            indent = list(type = list("choice", c("true", "false"))),
                            echoParams = list(type = "string"),
                            fl = list(type = "string"),
                            start = list(type = "string", pattern = "[0-9]+"),
                            fq = list(type = "character", any.missing = FALSE),
                            rows = list(type = "string"),
                            q = list(type = "string"),
                            tie = list(type = "string"),
                            facet.limit = list(type = "string", pattern = "[-]?[0-9]+"),
                            qf = list(type = "string"),
                            facet.method = list(type = "string"),
                            facet.mincount = list(type = "string", pattern = "[0-9]+"),
                            facet = list(type = list("choice", c("true", "false"))),
                            wt = list(type = list("choice", c("json", "xml"))),
                            facet.sort = list(type = list("choice", "lex")),
                            shards = list(type = "string"),
                            fields = list(
                                type = list("list", "character", any.missing = FALSE),
                                "..." = list(type = list("list", rules = "S1"))
                            )
                        )
                    )
                )
            ),
            response = list(
                type = "list",
                names = list(must.include = c("numFound", "start", "maxScore", "docs")),
                fields = list(
                    numFound = list(type = "int"),
                    start = list(type = "int"),
                    maxScore = list(type = "int"),
                    docs = list(type = "data_frame")
                )
            ),
            facet_counts = list(
                type = "list",
                name = list(must.include = "facet_fields"),
                fields = list(
                    facet_fields = list(
                        type = list("list", "list", any.missing = FALSE),
                        "..." = list(type = list("list", rules = c("S1", "I1")))
                    )
                )
            )
        )
    )
})
# }}}

# SCHEMA_QUERY {{{
SCHEMA_QUERY <- local({
    sch_list <- list(
        type = list("list", c("logical", "numeric", "character", "null"), null.ok = TRUE, names = "unique"),
        names = list(must.include = c("value", "negate"))
    )
    sch_param <- c(
        sch_list,
        list(
            fields = list(
                value = list(
                    type = list("character", any.missing = FALSE, unique = TRUE, null.ok = TRUE)
                ),
                negate = list(type = "flag")
            )
        )
    )
    sch_params <- list(
        type = list("list", names = "unique"),
        names = list(must.include = c(
            "project", "activity_id", "source_id", "variable_id",
            "variant_label", "nominal_resolution", "data_node", "facets",
            "fields", "shards", "replica", "latest", "distrib", "limit",
            "offset", "format"
        )),
        fields = list(
            project            = sch_param,
            activity_id        = sch_param,
            source_id          = sch_param,
            variable_id        = sch_param,
            variant_label      = sch_param,
            nominal_resolution = sch_param,
            data_node          = sch_param,
            facets             = sch_param,
            shards             = sch_param,
            dataset_id         = sch_param,
            replica = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = list("flag", null.ok = TRUE)),
                        negate = list(type = "flag")
                    )
                )
            ),
            latest  = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = "flag"),
                        negate = list(type = "flag")
                    )
                )
            ),
            distrib = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = "flag"),
                        negate = list(type = "flag")
                    )
                )
            ),
            limit   = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = "count"),
                        negate = list(type = "flag")
                    )
                )
            ),
            offset  = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = "count"),
                        negate = list(type = "flag")
                    )
                )
            ),
            type = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = list("choice", "Dataset")),
                        negate = list(type = "flag")
                    )
                )
            ),
            format = c(
                sch_list,
                list(
                    fields = list(
                        value = list(type = list("choice", "application/solr+json")),
                        negate = list(type = "flag")
                    )
                )
            )
        )
    )

    sch_resp <- SCHEMA_RESPONSE
    sch_resp$fields$responseHeader$fields$params$names$must.include <- setdiff(
        sch_resp$fields$responseHeader$fields$params$names$must.include,
        c("facet.field", "fields")
    )
    sch_resp$fields$responseHeader$fields$params$fields$facet.field <- NULL
    sch_resp$fields$responseHeader$fields$params$fields$fields <- NULL

    list(
        type = "list",
        names = list(must.include = c("host", "parameter", "last_result", "facet_listing")),
        fields = list(
            host = list(type = "string"),
            parameter = sch_params,
            last_result = list(
                type = list(
                    "list",
                    c("logical", "numeric", "character", "null", "list"),
                    null.ok = TRUE, names = "unique"
                ),
                names = list(must.include = c("url_host", "parameter", "response")),
                fields = list(
                    url_host = list(type = "string"),
                    parameter = sch_params,
                    response = sch_resp
                )
            ),
            facet_listing = SCHEMA_RESPONSE
        )
    )
})
# }}}

# SCHEMA_RESULT_DATASET {{{
SCHEMA_RESULT_DATASET <- local({
    schema <- SCHEMA_QUERY

    schema$names$must.include <- c("host", "parameter", "response", "last_result")
    schema$fields$response <- schema$fields$last_result$response
    schema$fields$facet_listing <- NULL

    # only require parameters for File and Aggregation queries
    schema$fields$last_result$fields$parameter$names$must.include <- c("dataset_id",
        "fields", "latest", "distrib", "limit", "type", "format")
    schema$fields$last_result$fields$parameter$fields$dataset_id <- schema$fields$parameter$fields$project

    # redefine 'type' values
    schema$fields$last_result$fields$parameter$fields$type$fields$value$type[[2L]] <- c("File", "Aggregation")

    # remove 'facet' requirement in the response of last result
    schema$fields$last_result$fields$response$fields$responseHeader$fields$params$names$must.include <- setdiff(
        schema$fields$last_result$fields$response$fields$responseHeader$fields$params$names$must.include,
        "facet"
    )
    schema$fields$last_result$fields$response$fields$responseHeader$fields$params$fields$facet <- NULL

    # remove 'facet_counts' requirement in the response of last result
    schema$fields$last_result$fields$response$fields$facet_counts <- NULL

    schema
})
# }}}

# vim: fdm=marker :
