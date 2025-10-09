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
    structure(
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
                                facet = list(type = list("choice", c("true", "false"), null.ok = TRUE)),
                                wt = list(type = list("choice", c("json", "xml"))),
                                facet.sort = list(type = list("choice", "lex")),
                                shards = list(type = "string"),
                                fields = list(type = list("character", any.missing = FALSE))
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
                        maxScore = list(type = "number"),
                        docs = list(
                            type = list("data_frame", null.ok = TRUE, col.names = "unique"),
                            names = list(subset.of = FIELDS_FACETS_ALL)
                        )
                    )
                ),
                facet_counts = list(
                    type = list("list", null.ok = TRUE),
                    name = list(must.include = c(
                        "facet_queries", "facet_fields", "facet_ranges",
                        "facet_intervals", "facet_heatmaps"
                    )),
                    fields = list(
                        facet_fields = list(
                            type = list("list", "list", any.missing = FALSE)
                        ),
                        "..." = list(type = list("list", rules = c("S1", "I1")))
                    )
                ),
                timestamp = list(type = "string")
            )
        ),
        class = "Schema"
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

    structure(
        list(
            type = "list",
            names = list(must.include = c("index_node", "parameter")),
            fields = list(
                index_node = list(type = "string"),
                parameter = sch_params
            )
        ),
        class = "Schema"
    )
})
# }}}

# SCHEMA_RESULT_DATASET {{{
SCHEMA_RESULT_DATASET <- local({
    schema <- SCHEMA_QUERY

    schema$names$must.include <- c("index_node", "parameter", "response")

    sch_resp <- SCHEMA_RESPONSE
    sch_resp$fields$responseHeader$fields$params$names$must.include <- setdiff(
        sch_resp$fields$responseHeader$fields$params$names$must.include,
        c("facet.field", "fields", "facet")
    )
    sch_resp$fields$responseHeader$fields$params$fields$facet.field <- NULL
    sch_resp$fields$responseHeader$fields$params$fields$fields <- NULL
    sch_resp$fields$responseHeader$fields$params$fields$facet <- NULL

    schema$fields$response <- sch_resp

    structure(schema, class = "Schema")
})
# }}}

# print.Schema {{{
#' Print a Schema object
#'
#' @param x A Schema object
#' @param ... Additional arguments (currently unused)
#' @param max_depth Maximum depth to display. Default is 3.
#' @param show_constraints Whether to show constraint information. Default is TRUE.
#' @param compact Whether to use compact mode (collapse long lists). Default is FALSE.
#'
#' @return Invisibly returns the input schema object
#' @export
print.Schema <- function(x, ..., max_depth = 3, show_constraints = TRUE, compact = FALSE) {
    cat("<Schema>\n\n")

    # Display root type
    cat("Type: ", format_type(x$type, show_constraints = show_constraints), "\n", sep = "")

    # Display required fields
    if (!is.null(x$names$must.include)) {
        required <- x$names$must.include
        if (length(required) > 5 && compact) {
            cat("Required: ", paste(required[1:5], collapse = ", "),
                sprintf(" ... (%d more)", length(required) - 5), "\n", sep = "")
        } else {
            cat("Required: ", paste(required, collapse = ", "), "\n", sep = "")
        }
    }

    # Display subset.of constraint
    if (!is.null(x$names$subset.of)) {
        subset_of <- x$names$subset.of
        if (length(subset_of) > 5 && compact) {
            cat("Subset of: ", paste(subset_of[1:5], collapse = ", "),
                sprintf(" ... (%d more)", length(subset_of) - 5), "\n", sep = "")
        } else if (length(subset_of) <= 10) {
            cat("Subset of: ", paste(subset_of, collapse = ", "), "\n", sep = "")
        } else {
            cat("Subset of: ", length(subset_of), " fields\n", sep = "")
        }
    }

    # Display field structure
    if (!is.null(x$fields) && length(x$fields) > 0) {
        cat("\nFields:\n")
        field_names <- names(x$fields)
        for (i in seq_along(field_names)) {
            is_last <- i == length(field_names)
            print_field(
                name = field_names[i],
                schema = x$fields[[i]],
                depth = 0,
                max_depth = max_depth,
                is_last = is_last,
                show_constraints = show_constraints,
                compact = compact
            )
        }
    }

    invisible(x)
}
# }}}

# format_type {{{
format_type <- function(type, show_constraints = TRUE) {
    if (is.character(type) && length(type) == 1) {
        return(type)
    }

    if (is.list(type)) {
        main_type <- type[[1]]
        constraints <- type[-1]

        # Format constraints
        constraint_strs <- character(0)

        # Only show constraints if show_constraints is TRUE
        if (!show_constraints) {
            # For choice type, still show the choices even when show_constraints is FALSE
            if (!is.na(main_type) && main_type == "choice" && length(type) >= 2 && !is.null(type[[2]])) {
                choices <- type[[2]]
                if (length(choices) <= 3) {
                    return(sprintf("choice(%s)", paste(sapply(choices, deparse), collapse = ", ")))
                } else {
                    return(sprintf("choice(%d options)", length(choices)))
                }
            }
            return(main_type)
        }

        if (!is.null(constraints$null.ok) && isTRUE(constraints$null.ok)) {
            constraint_strs <- c(constraint_strs, "null.ok")
        }

        if (!is.null(constraints$names)) {
            constraint_strs <- c(constraint_strs, sprintf("names=%s", deparse(constraints$names)))
        }

        if (!is.null(constraints$any.missing) && isFALSE(constraints$any.missing)) {
            constraint_strs <- c(constraint_strs, "any.missing=FALSE")
        }

        if (!is.null(constraints$unique) && isTRUE(constraints$unique)) {
            constraint_strs <- c(constraint_strs, "unique")
        }

        if (!is.null(constraints$min.len)) {
            constraint_strs <- c(constraint_strs, sprintf("min.len=%d", constraints$min.len))
        }

        if (!is.null(constraints$max.len)) {
            constraint_strs <- c(constraint_strs, sprintf("max.len=%d", constraints$max.len))
        }

        if (!is.null(constraints$min.rows)) {
            constraint_strs <- c(constraint_strs, sprintf("min.rows=%d", constraints$min.rows))
        }

        if (!is.null(constraints$col.names)) {
            constraint_strs <- c(constraint_strs, sprintf("col.names=%s", deparse(constraints$col.names)))
        }

        if (!is.null(constraints$pattern)) {
            constraint_strs <- c(constraint_strs, sprintf("pattern=%s", deparse(constraints$pattern)))
        }

        if (!is.null(constraints$rules)) {
            rules <- constraints$rules
            if (length(rules) == 1) {
                constraint_strs <- c(constraint_strs, sprintf("rules=%s", deparse(rules)))
            } else {
                rules_str <- paste(sapply(rules, deparse), collapse = ", ")
                constraint_strs <- c(constraint_strs, sprintf("rules=c(%s)", rules_str))
            }
        }

        # Handle choice type
        if (!is.na(main_type) && main_type == "choice" && length(type) >= 2 && !is.null(type[[2]])) {
            choices <- type[[2]]
            if (length(choices) <= 3) {
                main_type <- sprintf("choice(%s)", paste(sapply(choices, deparse), collapse = ", "))
            } else {
                main_type <- sprintf("choice(%d options)", length(choices))
            }
        }

        if (length(constraint_strs) > 0) {
            return(sprintf("%s (%s)", main_type, paste(constraint_strs, collapse = ", ")))
        } else {
            return(main_type)
        }
    }

    "unknown"
}
# }}}

# print_field {{{
print_field <- function(name, schema, depth, max_depth, is_last,
                       show_constraints, compact, prefix = "") {
    # Tree characters
    branch <- if (is_last) "\u2514\u2500" else "\u251c\u2500"  # └─ or ├─
    continuation <- if (is_last) "  " else "\u2502 "  # │

    # Special handling for "..." (dynamic fields)
    display_name <- if (name == "...") "<dynamic fields>" else name

    # Print field name and type
    type_str <- format_type(schema$type, show_constraints = show_constraints)
    cat(prefix, branch, " ", display_name, " [", type_str, "]", sep = "")

    # Show constraints
    if (show_constraints) {
        constraint_info <- character(0)

        if (!is.null(schema$names$must.include)) {
            n_required <- length(schema$names$must.include)
            if (n_required <= 5 || !compact) {
                constraint_info <- c(constraint_info,
                    sprintf("required: %s", paste(schema$names$must.include, collapse = ", ")))
            } else {
                constraint_info <- c(constraint_info, sprintf("%d required", n_required))
            }
        }

        if (!is.null(schema$names$subset.of)) {
            n_subset <- length(schema$names$subset.of)
            constraint_info <- c(constraint_info, sprintf("subset of %d fields", n_subset))
        }

        if (length(constraint_info) > 0) {
            cat(" (", paste(constraint_info, collapse = ", "), ")", sep = "")
        }
    }

    cat("\n")

    # Recursively print subfields
    if (depth < max_depth && !is.null(schema$fields) && length(schema$fields) > 0) {
        subfields <- names(schema$fields)
        new_prefix <- paste0(prefix, continuation, " ")

        for (i in seq_along(subfields)) {
            is_last_sub <- i == length(subfields)
            print_field(
                name = subfields[i],
                schema = schema$fields[[i]],
                depth = depth + 1,
                max_depth = max_depth,
                is_last = is_last_sub,
                show_constraints = show_constraints,
                compact = compact,
                prefix = new_prefix
            )
        }
    } else if (!is.null(schema$fields) && length(schema$fields) > 0) {
        # Exceeded max depth
        n_fields <- length(schema$fields)
        cat(prefix, continuation, "   ... (", n_fields, " more field",
            if (n_fields > 1) "s" else "", ")\n", sep = "")
    }
}
# }}}

# summary.Schema {{{
#' Summarize a Schema object
#'
#' @param object A Schema object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns a list with summary statistics
#' @export
summary.Schema <- function(object, ...) {
    stats <- list(
        total_fields = count_fields(object),
        max_depth = get_max_depth(object),
        required_fields = length(object$names$must.include %||% character(0)),
        root_type = if (is.character(object$type)) object$type else object$type[[1]]
    )

    cat("<Schema Summary>\n\n")
    cat("Root type:      ", stats$root_type, "\n", sep = "")
    cat("Total fields:   ", stats$total_fields, "\n", sep = "")
    cat("Max depth:      ", stats$max_depth, "\n", sep = "")
    cat("Required fields:", stats$required_fields, "\n", sep = "")

    if (stats$required_fields > 0) {
        cat("\nRequired field names:\n")
        required <- object$names$must.include
        if (length(required) <= 10) {
            cat("  ", paste(required, collapse = ", "), "\n", sep = "")
        } else {
            cat("  ", paste(required[1:10], collapse = ", "),
                sprintf("\n  ... (%d more)", length(required) - 10), "\n", sep = "")
        }
    }

    invisible(stats)
}
# }}}

# str.Schema {{{
#' Display the structure of a Schema object
#'
#' @param object A Schema object
#' @param ... Additional arguments passed to print.Schema
#' @param max.level Maximum depth to display. Default is 99.
#'
#' @return Invisibly returns the input Schema object
#' @export
str.Schema <- function(object, ..., max.level = 99) {
    print.Schema(object, max_depth = max.level, show_constraints = TRUE, ...)
}
# }}}

# count_fields {{{
count_fields <- function(schema, include_root = FALSE) {
    count <- if (include_root) 1L else 0L

    if (!is.null(schema$fields) && length(schema$fields) > 0) {
        for (field in schema$fields) {
            count <- count + 1L + count_fields(field, include_root = FALSE)
        }
    }

    count
}
# }}}

# get_max_depth {{{
get_max_depth <- function(schema, current_depth = 0L) {
    if (is.null(schema$fields) || length(schema$fields) == 0) {
        return(current_depth)
    }

    max_child_depth <- current_depth
    for (field in schema$fields) {
        child_depth <- get_max_depth(field, current_depth + 1L)
        if (child_depth > max_child_depth) {
            max_child_depth <- child_depth
        }
    }

    max_child_depth
}
# }}}

# as.data.frame.Schema {{{
#' Convert Schema to a data.frame
#'
#' @param x A Schema object
#' @param row.names NULL or a character vector giving the row names for the data frame
#' @param optional Logical. If TRUE, setting row names and converting column names is optional
#' @param ... Additional arguments (currently unused)
#'
#' @return A data.frame with columns: path, type, constraints
#' @export
as.data.frame.Schema <- function(x, row.names = NULL, optional = FALSE, ...) {
    paths <- character(0)
    types <- character(0)
    constraints <- character(0)

    collect_fields <- function(schema, path = "") {
        if (!is.null(schema$fields) && length(schema$fields) > 0) {
            for (field_name in names(schema$fields)) {
                field <- schema$fields[[field_name]]
                field_path <- if (nchar(path) > 0) {
                    paste0(path, "$", field_name)
                } else {
                    paste0("$", field_name)
                }

                # Get type
                type_str <- if (is.character(field$type)) {
                    field$type
                } else if (is.list(field$type)) {
                    field$type[[1]]
                } else {
                    "unknown"
                }

                # Get constraints
                constraint_parts <- character(0)

                if (is.list(field$type) && length(field$type) > 1) {
                    type_constraints <- field$type[-1]
                    if (!is.null(type_constraints$null.ok) && isTRUE(type_constraints$null.ok)) {
                        constraint_parts <- c(constraint_parts, "null.ok")
                    }
                    if (!is.null(type_constraints$any.missing) && isFALSE(type_constraints$any.missing)) {
                        constraint_parts <- c(constraint_parts, "any.missing=FALSE")
                    }
                    if (!is.null(type_constraints$unique) && isTRUE(type_constraints$unique)) {
                        constraint_parts <- c(constraint_parts, "unique")
                    }
                }

                if (!is.null(field$names$must.include)) {
                    n_req <- length(field$names$must.include)
                    constraint_parts <- c(constraint_parts, sprintf("required=%d", n_req))
                }

                constraint_str <- if (length(constraint_parts) > 0) {
                    paste(constraint_parts, collapse = ", ")
                } else {
                    ""
                }

                paths <<- c(paths, field_path)
                types <<- c(types, type_str)
                constraints <<- c(constraints, constraint_str)

                # Recurse
                collect_fields(field, field_path)
            }
        }
    }

    collect_fields(x)

    df <- data.frame(
        path = paths,
        type = types,
        constraints = constraints,
        stringsAsFactors = FALSE
    )

    if (!is.null(row.names)) {
        rownames(df) <- row.names
    }

    df
}
# }}}

# vim: fdm=marker :
