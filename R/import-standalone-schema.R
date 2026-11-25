# Pruned standalone schema engine (read / compile / validate subset)
# Upstream: https://github.com/hongyuanjia/schemate/blob/standalone/R/standalone-schema.R
# Upstream source-commit: 35e1fb58f1f315d033aa214c6210530f26527120 (MIT)
# ----------------------------------------------------------------------
#
# Retained sections:
# - schema-utils.R    (checkmate property helpers and S7 conversion generic)
# - schema-spec.R     (S7 schema AST classes and DSL parser)
# - schema-doc.R      (`SchemaDoc` and `schema_doc()`)
# - schema-flat.R     (`SchemaFlat` and `schema_flat__compile()`)
# - schema-json.R     (`schema_read()`)
# - schema-validate.R (`schema_validate()` for `SchemaFlat`, flat nodes, and `SchemaDoc`)
#
# Pruned upstream sections and helpers not used by epwshiftr:
# - schema-compact.R, schema-edit.R, schema-infer.R, schema-query.R
# - Serialization, writer, and print helpers (`schema_write()`, `as.list()`
#   methods, `schema_utils__as_json`, `schema_utils__json_*`, `print.SchemaSpec`)
# - Unused `CheckmateSpec` rule helpers and `schema_utils__coalesce()`
#
# Kept at `R/import-standalone-schema.R` so `DESCRIPTION` `Collate:` needs no change.
#
# nocov start

# -------------------- schema-utils.R
# shared utilities {{{
schema_utils__checkmate_result <- function(result, label = NULL) {
    if (isTRUE(result)) {
        return(NULL)
    }

    substr(result, 1L, 1L) <- tolower(substr(result, 1L, 1L))

    if (!is.null(label)) {
        return(sprintf("%s: %s", label, result))
    }

    result
}

schema_utils__formal_args <- function(def) {
    names(formals(def, envir = parent.frame()))
}

schema_utils__require_namespace <- function(pkg, reason) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        return(invisible(TRUE))
    }

    stop(
        sprintf(
            "Package `%s` is required to %s. Install it with `install.packages(\"%s\")`.",
            pkg,
            reason,
            pkg
        ),
        call. = FALSE
    )
}

schema_utils__checkmate_fun <- function(kind) {
    checkmate::assert_string(kind)
    utils::getFromNamespace(paste0("check_", kind), asNamespace("checkmate"))
}

schema_utils__checkmate_args <- function(kind) {
    schema_utils__formal_args(schema_utils__checkmate_fun(kind))[-1L]
}

schema_utils__checkmate_validator <- function(check, ..., label = NULL) {
    checkmate::assert_function(check)

    args <- list(...)
    force(label)

    function(value) {
        schema_utils__checkmate_result(do.call(check, c(list(value), args)), label = label)
    }
}

schema_utils__checkmate_property <- function(
    class = S7::class_any,
    check,
    ...,
    getter = NULL,
    setter = NULL,
    default = NULL,
    name = NULL,
    label = NULL
) {
    if (missing(check)) {
        stop("`check` must be supplied.")
    }

    S7::new_property(
        class = class,
        getter = getter,
        setter = setter,
        validator = schema_utils__checkmate_validator(check, ..., label = label),
        default = default,
        name = name
    )
}

schema_utils__convert <- S7::new_generic(
    "schema_utils__convert",
    "from",
    function(from, to, ...) S7::S7_dispatch()
)
# }}}

# schema_utils__prop {{{
schema_utils__prop <- function(class, check, null.ok = FALSE, ...) {
    checkmate::assert_flag(null.ok)

    if (null.ok) {
        if (!"null.ok" %in% schema_utils__formal_args(check)) {
            stop("input checkmate function does not support `null.ok` argument, but `null.ok = TRUE` was specified.")
        }
        class <- NULL | class
    }

    schema_utils__checkmate_property(
        class,
        check,
        null.ok = null.ok,
        ...
    )
}

schema_utils__prop_string <- function(min.chars = 1L, null.ok = TRUE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_string,
        null.ok = null.ok,
        min.chars = min.chars,
        default = default,
        ...
    )
}

schema_utils__prop_character <- function(any.missing = FALSE, min.chars = 1L, null.ok = FALSE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_character,
        null.ok = null.ok,
        any.missing = any.missing,
        min.chars = min.chars,
        default = default,
        ...
    )
}

schema_utils__prop_choice <- function(choices, null.ok = TRUE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_choice,
        choices = choices,
        null.ok = null.ok,
        default = default,
        ...
    )
}

schema_utils__prop_list <- function(types = character(), names = "unique", null.ok = TRUE, default = list(), ...) {
    if (length(types) && !is.null(utils::packageName())) {
        types <- unique(c(paste0(utils::packageName(), "::", types), types))
    }

    schema_utils__prop(
        S7::class_list,
        checkmate::check_list,
        types = types,
        names = names,
        null.ok = null.ok,
        default = default,
        ...
    )
}

schema_utils__prop_ref <- function(null.ok = TRUE) {
    schema_utils__prop_string(
        null.ok = null.ok,
        min.chars = 1L,
        pattern = "^#/\\$defs/[^/]+$"
    )
}
# }}}

# -------------------- schema-spec.R

SCHEMA_SPEC_KINDS <- sort(sub("^check_", "", grep("^check_", getNamespaceExports("checkmate"), value = TRUE)))
SCHEMA_SPEC_KINDS_CONTAINER <- c("list", "data_frame", "data_table", "tibble")
SCHEMA_SPEC_OPERATORS <- c("check", "$ref", "all", "any", "one", "not")
SCHEMA_SPEC_KEYWORDS <- c(SCHEMA_SPEC_OPERATORS, "fields", "groups", "patterns", "positions", "rest", "keys", "description", "$defs", "version")

# SchemaSpec {{{
# - node variants become distinct classes instead of a single tagged union
# - rule payload is represented by shared `SchemaRule*` classes
# - exact and pattern bindings are represented by separate classes
# - positions child list represents unnamed prefixItems semantics
# - rest child is made explicit
# - n-ary combinators share common abstract parents
schema_spec__kind_is_container <- function(kind) {
    kind %in% SCHEMA_SPEC_KINDS_CONTAINER
}

SchemaSpec <- S7::new_class("SchemaSpec", abstract = TRUE)

SchemaRule <- S7::new_class(
    "SchemaRule",
    parent = SchemaSpec,
    abstract = TRUE
)

SchemaRuleCheck <- S7::new_class(
    "SchemaRuleCheck",
    parent = SchemaRule,
    properties = list(
        kind = schema_utils__prop_choice(SCHEMA_SPEC_KINDS, null.ok = FALSE),
        args = schema_utils__prop_list(null.ok = FALSE, names = "unique", default = list())
    ),
    validator = function(self) {
        if (length(self@args)) {
            schema_utils__checkmate_result(
                checkmate::check_subset(
                    names(self@args),
                    schema_utils__checkmate_args(self@kind),
                    empty.ok = FALSE
                ),
                sprintf("invalid arguments for check kind `%s`", self@kind)
            )
        }
    }
)

SchemaRuleNames <- S7::new_class(
    "SchemaRuleNames",
    parent = SchemaRule,
    properties = list(
        args = schema_utils__prop_list(null.ok = FALSE, names = "unique", default = list())
    ),
    validator = function(self) {
        if (!length(self@args)) {
            return(NULL)
        }

        schema_utils__checkmate_result(
            checkmate::check_subset(names(self@args), schema_utils__formal_args(checkmate::check_names)[-1L]),
            "invalid arguments for `checkmate::check_names()`"
        )
    }
)

schema_spec__name_type <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    x@args$type
}

SchemaNode <- S7::new_class(
    "SchemaNode",
    parent = SchemaSpec,
    properties = list(desc = schema_utils__prop_string()),
    abstract = TRUE
)

SchemaBindingExact <- S7::new_class(
    "SchemaBindingExact",
    parent = SchemaSpec,
    abstract = TRUE,
    properties = list(
        keys = schema_utils__prop_character(any.missing = FALSE, min.len = 1L, unique = TRUE),
        target = S7::new_property(SchemaNode)
    )
)

SchemaBindingExactCmpt <- S7::new_class(
    "SchemaBindingExactCmpt",
    parent = SchemaBindingExact
)

SchemaBindingPattern <- S7::new_class(
    "SchemaBindingPattern",
    parent = SchemaSpec,
    abstract = TRUE,
    properties = list(
        pattern = schema_utils__prop_string(null.ok = FALSE, min.chars = 1L),
        target = S7::new_property(SchemaNode)
    ),
    validator = function(self) {
        ok <- tryCatch({
            grepl(self@pattern, "")
            TRUE
        }, error = function(e) FALSE)
        if (!ok) {
            return(sprintf("`pattern` must be a valid regular expression: %s", self@pattern))
        }
    }
)

SchemaBindingPatternCmpt <- S7::new_class(
    "SchemaBindingPatternCmpt",
    parent = SchemaBindingPattern
)

SchemaNodeCheck <- S7::new_class(
    "SchemaNodeCheck",
    parent = SchemaNode,
    properties = list(
        value = S7::new_property(SchemaRuleCheck),
        name = S7::new_property(
            NULL | SchemaRuleNames,
            default = NULL
        )
    ),
    abstract = TRUE
)

SchemaNodeLeaf <- S7::new_class(
    "SchemaNodeLeaf",
    parent = SchemaNodeCheck,
    validator = function(self) {
        if (schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "`SchemaNodeLeaf` does not allow container kind `%s`; container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }
    }
)

SchemaNodeContainer <- S7::new_class(
    "SchemaNodeContainer",
    parent = SchemaNodeCheck,
    abstract = TRUE
)

SchemaNodeContainerCmpt <- S7::new_class(
    "SchemaNodeContainerCmpt",
    parent = SchemaNodeContainer,
    properties = list(
        exact = schema_utils__prop_list("SchemaBindingExactCmpt", names = "unnamed", default = list()),
        patterns = schema_utils__prop_list("SchemaBindingPatternCmpt", names = "unnamed", default = list()),
        positions = schema_utils__prop_list("SchemaNode", names = "unnamed", default = list()),
        rest = S7::new_property(NULL | SchemaNode, default = NULL)
    ),
    validator = function(self) {
        if (!schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "@value requires a container kind; got `%s`. Allowed container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }

        if (length(self@exact)) {
            keys <- unlist(lapply(self@exact, function(x) x@keys), use.names = FALSE)
            msg <- schema_utils__checkmate_result(checkmate::check_character(keys, unique = TRUE), label = "@exact")
            if (!is.null(msg)) {
                return(sprintf("%s ('%s')", msg, keys[duplicated(keys)][[1L]]))
            }
        }

        if (length(self@positions) && !identical(schema_spec__name_type(self@name), "unnamed")) {
            return("`positions` requires `keys$type = 'unnamed'`.")
        }

        if (identical(schema_spec__name_type(self@name), "unnamed") && (length(self@exact) || length(self@patterns))) {
            return("`keys$type = 'unnamed'` only allows `positions` and `rest` constraints.")
        }
    }
)

SchemaNodeRef <- S7::new_class(
    "SchemaNodeRef",
    parent = SchemaNode,
    properties = list(
        ref = schema_utils__prop_ref(null.ok = FALSE)
    )
)

SchemaNodeNary <- S7::new_class(
    "SchemaNodeNary",
    parent = SchemaNode,
    abstract = TRUE
)

SchemaNodeNaryCmpt <- S7::new_class(
    "SchemaNodeNaryCmpt",
    parent = SchemaNodeNary,
    properties = list(
        branches = schema_utils__prop_list("SchemaNode", names = "unnamed", min.len = 1L, default = list())
    ),
    abstract = TRUE
)

SchemaNodeAllCmpt <- S7::new_class(
    "SchemaNodeAllCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeAnyCmpt <- S7::new_class(
    "SchemaNodeAnyCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeOneCmpt <- S7::new_class(
    "SchemaNodeOneCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeNot <- S7::new_class(
    "SchemaNodeNot",
    parent = SchemaNode,
    abstract = TRUE
)

SchemaNodeNotCmpt <- S7::new_class(
    "SchemaNodeNotCmpt",
    parent = SchemaNodeNot,
    properties = list(
        branch = S7::new_property(SchemaNode)
    )
)
# }}}

# schame_spec_node {{{
schema_spec__error <- function(path, message) {
    stop(sprintf("Path at '%s' is invalid:\n- %s", path, message), call. = FALSE)
}

schema_spec__assert <- function(path, what, check) {
    if (!isTRUE(check)) {
        schema_spec__error(path, sprintf("%s: %s", what, check))
    }
}

schema_spec__assert_list <- function(path, what, x, names = "unique", ...) {
    schema_spec__assert(path, what, checkmate::check_list(x, names = names, ...))
}

schema_spec__assert_names <- function(path, what, x, ...) {
    schema_spec__assert(path, what, checkmate::check_names(names(x), ...))
}

schema_spec__rule <- function(x, path) {
    schema_spec__assert_list(path, "'check' rule", x)
    schema_spec__assert_names(path, "'check' rule", x, must.include = "kind")

    args <- if (any(names(x) != "kind")) {
        unclass(x[names(x) != "kind"])
    } else {
        list()
    }

    SchemaRuleCheck(
        kind = x$kind,
        args = args
    )
}

schema_spec__name_rule <- function(x, path) {
    schema_spec__assert_list(path, "'keys' rule", x)
    SchemaRuleNames(args = unclass(x))
}

schema_spec__node_check <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "'check'", x)
    schema_spec__assert_names(
        path,
        "'check'",
        x,
        must.include = "check",
        subset.of = c("check", "keys", "fields", "groups", "patterns", "positions", "rest", "description")
    )

    parts <- list(desc = x$description)
    parts$value <- schema_spec__rule(x$check, paste0(path, "$check"))
    if (!is.null(x$keys)) {
        parts$name <- schema_spec__name_rule(x$keys, paste0(path, "$keys"))
    }

    if (!schema_spec__kind_is_container(parts$value@kind)) {
        if (!is.null(x$fields)) {
            schema_spec__error(path, "'fields' is only allowed on container check nodes.")
        }
        if (!is.null(x$groups)) {
            schema_spec__error(path, "'groups' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$patterns)) {
            schema_spec__error(path, "'patterns' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$positions)) {
            schema_spec__error(path, "'positions' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$rest)) {
            schema_spec__error(path, "'rest' is only allowed on container 'check' nodes.")
        }

        return(do.call(SchemaNodeLeaf, parts))
    }

    parts$exact <- c(
        schema_spec__binding_fields(x$fields, paste0(path, "$fields"), defs),
        schema_spec__binding_groups(x$groups, paste0(path, "$groups"), defs)
    )
    parts$patterns <- schema_spec__binding_patterns(x$patterns, paste0(path, "$patterns"), defs)
    parts$positions <- schema_spec__positions(x$positions, paste0(path, "$positions"), defs)

    if (!is.null(x$rest)) {
        parts$rest <- schema_spec__node(x$rest, paste0(path, "$rest"), defs)
    }

    do.call(SchemaNodeContainerCmpt, parts)
}

schema_spec__has_operator <- function(x) {
    any(names(x) %in% SCHEMA_SPEC_OPERATORS)
}

schema_spec__operator <- function(x, path) {
    op <- names(x)
    op <- op[op %in% SCHEMA_SPEC_OPERATORS]

    if (length(op) == 0L) {
        schema_spec__error(
            path,
            sprintf(
                "primary operator: Must be element of set {%s}, but is missing.",
                paste0("'", SCHEMA_SPEC_OPERATORS, "'", collapse = ", ")
            )
        )
    } else if (length(op) > 1L) {
        schema_spec__error(
            path,
            sprintf(
                "primary operator: Must be element of set {%s}, but multiple found: {%s}.",
                paste0("'", SCHEMA_SPEC_OPERATORS, "'", collapse = ", "),
                paste0("'", op, "'", collapse = ", ")
            )
        )
    }

    op
}

schema_spec__binding_fields <- function(fields, path, defs) {
    if (is.null(fields)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'fields'", fields, types = "list")

    lapply(names(fields), function(name) {
        SchemaBindingExactCmpt(
            keys = name,
            target = schema_spec__node(
                x = fields[[name]],
                path = paste0(path, "$", name),
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__binding_groups <- function(groups, path, defs) {
    if (is.null(groups)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'groups'", groups, types = "list", names = "unnamed")

    lapply(seq_along(groups), function(i) {
        group <- groups[[i]]
        loc <- paste0(path, "[", i, "]")

        schema_spec__assert_list(loc, "group item", group)

        schema_spec__assert_names(
            loc,
            "group item",
            group,
            type = "unique",
            must.include = "names",
            subset.of = c("names", setdiff(SCHEMA_SPEC_KEYWORDS, c("version", "$defs")))
        )

        target <- group[names(group) != "names"]
        SchemaBindingExactCmpt(
            keys = group$names,
            target = schema_spec__node(
                x = target,
                path = loc,
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__binding_patterns <- function(patterns, path, defs) {
    if (is.null(patterns)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'patterns'", patterns, types = "list", names = "unique")

    lapply(names(patterns), function(pattern) {
        SchemaBindingPatternCmpt(
            pattern = pattern,
            target = schema_spec__node(
                x = patterns[[pattern]],
                path = paste0(path, "$", pattern),
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__positions <- function(positions, path, defs) {
    if (is.null(positions)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'positions'", positions, types = "list", names = "unnamed")

    lapply(seq_along(positions), function(i) {
        schema_spec__node(
            x = positions[[i]],
            path = paste0(path, "[", i, "]"),
            defs = defs,
            root = FALSE
        )
    })
}

schema_spec__node_ref <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "$`$ref`", x, types = "character")
    schema_spec__assert_names(path, "$`$ref`", x, must.include = "$ref", subset.of = c("$ref", "description"))

    checkmate::assert_character(defs, any.missing = FALSE, min.chars = 1L, names = "unnamed", null.ok = TRUE)
    if (!is.null(defs)) {
        def_name <- sub("^#/\\$defs/", "", x$`$ref`)
        schema_spec__assert(
            path,
            "'$ref' target",
            checkmate::check_choice(def_name, defs)
        )
    }

    SchemaNodeRef(ref = x$`$ref`, desc = x$description)
}

schema_spec__node_branch <- function(x, path, defs, operator) {
    schema_spec__assert_list(path, "branch node", x)

    if (schema_spec__has_operator(x)) {
        schema_spec__node(x, path = path, defs = defs, root = FALSE)
    } else if ("kind" %in% names(x)) {
        if (any(SCHEMA_SPEC_KEYWORDS %in% names(x))) {
            schema_spec__error(
                path,
                sprintf(
                    "- @branches in shorthand check node format must not contain reserved node-level keys or any primary operator, but {%s} %s found.",
                    paste0("'", SCHEMA_SPEC_KEYWORDS[SCHEMA_SPEC_KEYWORDS %in% names(x)], "'", collapse = ","),
                    if (sum(SCHEMA_SPEC_KEYWORDS %in% names(x)) > 1L) "were" else "was"
                )
            )
        }

        schema_spec__node_check(list(check = x), path = path, defs = defs, root = FALSE)
    } else {
        schema_spec__error(
            path,
            "branch node must be a valid schema node with a primary operator or a shorthand 'check' rule"
        )
    }
}

schema_spec__node_nary <- function(x, path, defs, operator, constructor, root = FALSE) {
    schema_spec__assert_list(path, sprintf("'%s' node", operator), x, types = c("list", "character"))
    schema_spec__assert_names(
        path,
        sprintf("'%s' node", operator),
        x,
        type = "unique",
        must.include = operator,
        subset.of = c("description", operator)
    )
    schema_spec__assert_names(
        sprintf("%s$%s", path, operator),
        sprintf("'%s' node", operator),
        x[[operator]],
        type = "unnamed"
    )

    do.call(
        constructor,
        list(
            branches = lapply(seq_along(x[[operator]]), function(i) {
                schema_spec__node_branch(
                    x[[operator]][[i]],
                    path = sprintf("%s$%s[%d]", path, operator, i),
                    defs = defs,
                    operator = operator
                )
            }),
            desc = x$description
        )
    )
}

schema_spec__node_not <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "'not' node", x, types = c("list", "character"))
    schema_spec__assert_names(
        path,
        sprintf("'%s' node", "not"),
        x,
        type = "unique",
        must.include = "not",
        subset.of = c("description", "not")
    )

    do.call(
        SchemaNodeNotCmpt,
        list(
            branch = schema_spec__node_branch(
                x$not,
                paste0(path, "$not"),
                defs = defs,
                operator = "not"
            ),
            desc = x$description
        )
    )
}

schema_spec__node <- function(x, path = "$", defs = character(), root = FALSE) {
    if (S7::S7_inherits(x, SchemaNode)) {
        return(x)
    }

    schema_spec__assert_list(path, "schema node", x)
    schema_spec__assert_names(path, "schema node", x)
    if (!root && !is.null(x$`$defs`)) {
        schema_spec__error(path, "`$defs` is only allowed at the root schema document.")
    }

    switch(
        schema_spec__operator(x, path),
        check = schema_spec__node_check(x, path, defs = defs, root = root),
        `$ref` = schema_spec__node_ref(x, path, defs = defs, root = root),
        all = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "all",
            constructor = SchemaNodeAllCmpt,
            root = root
        ),
        any = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "any",
            constructor = SchemaNodeAnyCmpt,
            root = root
        ),
        one = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "one",
            constructor = SchemaNodeOneCmpt,
            root = root
        ),
        not = schema_spec__node_not(x, path, defs = defs, root = root),
        schema_spec__error(path, "unsupported primary operator.")
    )
}
# }}}

# -------------------- schema-doc.R

# SchemaDoc {{{
SchemaDoc <- S7::new_class(
    "SchemaDoc",
    parent = SchemaSpec,
    properties = list(
        version = schema_utils__prop_string(null.ok = TRUE),
        path = schema_utils__prop_string(),
        root = S7::new_property(SchemaNode),
        defs = schema_utils__prop_list("SchemaNode")
    )
)
# }}}

# schema_doc {{{
schema_doc__defs <- function(x, path = "$`$defs`") {
    if (!length(x)) {
        return(stats::setNames(list(), character()))
    }

    schema_spec__assert_list(path, "'$defs'", x, types = "list", names = "unique")
    nms <- names(x)
    if (any(grepl("/", nms, fixed = TRUE))) {
        schema_spec__error(path, "'$defs': Names must not contain '/'.")
    }

    stats::setNames(
        lapply(nms, function(name) {
            schema_spec__node(
                x[[name]],
                path = sprintf("%s[['%s']]", path, name),
                defs = nms,
                root = FALSE
            )
        }),
        nms
    )
}

#' Parse schema documents
#'
#' `schema_doc()` parses a schema DSL list into a schemate schema document
#' object.
#'
#' @param x A schema DSL list or an existing schemate schema document.
#' @param path Optional source path stored as runtime metadata.
#'
#' @return A schemate schema document object.
#'
#' @noRd
schema_doc <- function(x, path = NULL) {
    if (S7::S7_inherits(x, SchemaDoc)) {
        return(x)
    }

    schema_spec__assert_list("$", "schema document", x, names = "unique")
    schema_spec__assert_names("$", "schema document", x, subset.of = SCHEMA_SPEC_KEYWORDS)

    defs <- schema_doc__defs(x$`$defs`)

    root <- x[!names(x) %in% c("$defs", "version")]
    if (!length(root)) {
        schema_spec__error("$", "'root': Schema document must contain a root schema node.")
    }

    SchemaDoc(
        path = path,
        version = x$version,
        root = schema_spec__node(root, path = "$", defs = names(defs), root = TRUE),
        defs = defs
    )
}
# }}}

# -------------------- schema-flat.R

# SchemaFlat {{{
schema_flat__node_is_flat <- function(x) {
    S7::S7_inherits(x, SchemaNodeLeaf) ||
        S7::S7_inherits(x, SchemaNodeContainerFlat) ||
        S7::S7_inherits(x, SchemaNodeNaryFlat) ||
        S7::S7_inherits(x, SchemaNodeNotFlat)
}

schema_flat__binding_name <- function(x) {
    x@keys[[1L]]
}

schema_flat__binding_names <- function(x) {
    vapply(x, schema_flat__binding_name, character(1L))
}

SchemaBindingExactFlat <- S7::new_class(
    "SchemaBindingExactFlat",
    parent = SchemaBindingExact,
    validator = function(self) {
        if (length(self@keys) != 1L) {
            return("@keys requires exactly one key.")
        }

        if (!schema_flat__node_is_flat(self@target)) {
            return("@target must be a flat schema node.")
        }
    }
)

SchemaBindingPatternFlat <- S7::new_class(
    "SchemaBindingPatternFlat",
    parent = SchemaBindingPattern,
    validator = function(self) {
        if (!schema_flat__node_is_flat(self@target)) {
            return("@target must be a flat schema node.")
        }
    }
)

SchemaNodeContainerFlat <- S7::new_class(
    "SchemaNodeContainerFlat",
    parent = SchemaNodeContainer,
    properties = list(
        exact = schema_utils__prop_list(
            "SchemaBindingExactFlat",
            names = "unnamed",
            default = list()
        ),
        patterns = schema_utils__prop_list(
            "SchemaBindingPatternFlat",
            names = "unnamed",
            default = list()
        ),
        positions = schema_utils__prop_list(
            "SchemaNode",
            names = "unnamed",
            default = list()
        ),
        rest = S7::new_property(
            NULL | SchemaNode,
            default = NULL
        )
    ),
    validator = function(self) {
        if (!schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "@value requires a container kind; got `%s`. Allowed container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }

        if (!is.null(self@rest) && !schema_flat__node_is_flat(self@rest)) {
            return("@rest must be a flat schema node.")
        }

        if (length(self@positions)) {
            bad <- !vapply(self@positions, schema_flat__node_is_flat, logical(1L))
            if (any(bad)) {
                return("@positions must all be flat schema nodes.")
            }
        }

        if (length(self@exact) > 0L) {
            keys <- vapply(self@exact, function(x) x@keys, character(1L))
            msg <- schema_utils__checkmate_result(checkmate::check_character(keys, unique = TRUE), label = "@exact")
            if (!is.null(msg)) {
                return(sprintf("%s ('%s')", msg, keys[duplicated(keys)][[1L]]))
            }
        }

        if (length(self@positions) && !identical(schema_spec__name_type(self@name), "unnamed")) {
            return("`positions` requires `keys$type = 'unnamed'`.")
        }

        if (identical(schema_spec__name_type(self@name), "unnamed") && (length(self@exact) || length(self@patterns))) {
            return("`keys$type = 'unnamed'` only allows `positions` and `rest` constraints.")
        }
    }
)

SchemaNodeNaryFlat <- S7::new_class(
    "SchemaNodeNaryFlat",
    parent = SchemaNodeNary,
    properties = list(
        branches = schema_utils__prop_list(
            "SchemaNode",
            names = "unnamed",
            min.len = 1L,
            default = list()
        )
    ),
    validator = function(self) {
        bad <- !vapply(self@branches, schema_flat__node_is_flat, logical(1L))
        if (any(bad)) {
            return("@branches must all be flat schema nodes.")
        }
    },
    abstract = TRUE
)

SchemaNodeAllFlat <- S7::new_class("SchemaNodeAllFlat", parent = SchemaNodeNaryFlat)
SchemaNodeAnyFlat <- S7::new_class("SchemaNodeAnyFlat", parent = SchemaNodeNaryFlat)
SchemaNodeOneFlat <- S7::new_class("SchemaNodeOneFlat", parent = SchemaNodeNaryFlat)

SchemaNodeNotFlat <- S7::new_class(
    "SchemaNodeNotFlat",
    parent = SchemaNodeNot,
    properties = list(
        branch = S7::new_property(
            SchemaNode,
            validator = function(value) {
                if (!schema_flat__node_is_flat(value)) {
                    return("@branch must be a flat schema node.")
                }
            }
        )
    )
)

SchemaFlat <- S7::new_class(
    "SchemaFlat",
    parent = SchemaSpec,
    properties = list(
        path = schema_utils__prop_string(null.ok = TRUE, default = NULL),
        version = schema_utils__prop_string(null.ok = TRUE),
        root = S7::new_property(
            SchemaNode,
            validator = function(value) {
                if (!schema_flat__node_is_flat(value)) {
                    return("@root must be a flat schema node.")
                }
            }
        )
    )
)
# }}}

# schema_flat__compile {{{
schema_flat__rule_check <- function(x) {
    if (!length(x@args)) {
        return(SchemaRuleCheck(kind = x@kind))
    }

    SchemaRuleCheck(kind = x@kind, args = x@args)
}

schema_flat__rule_names <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    SchemaRuleNames(args = x@args)
}

schema_flat__ref_name <- function(ref) {
    sub("^#/\\$defs/", "", ref)
}

schema_flat__context <- function(defs = list()) {
    ctx <- new.env(parent = emptyenv())
    ctx$defs <- defs
    ctx$cache <- list()
    ctx$stack <- character()
    ctx
}

schema_flat__overlay_desc <- S7::new_generic(
    "schema_flat__overlay_desc",
    "x",
    function(x, desc) S7::S7_dispatch()
)

S7::method(schema_flat__overlay_desc, SchemaNodeLeaf) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeLeaf(value = x@value, name = x@name, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeContainerFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeContainerFlat(
        value = x@value,
        name = x@name,
        exact = x@exact,
        patterns = x@patterns,
        positions = x@positions,
        rest = x@rest,
        desc = desc
    )
}

S7::method(schema_flat__overlay_desc, SchemaNodeAllFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeAllFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeAnyFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeAnyFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeOneFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeOneFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeNotFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeNotFlat(branch = x@branch, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNode) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    stop("unsupported compiled schema node.", call. = FALSE)
}

schema_flat__def <- function(name, ctx) {
    if (is.null(ctx$defs[[name]])) {
        stop(sprintf("`$ref` target `#/$defs/%s` is not available during compilation.", name), call. = FALSE)
    }

    if (name %in% ctx$stack) {
        stop(
            sprintf(
                "circular `$ref` detected while compiling: %s.",
                paste(c(ctx$stack, name), collapse = " -> ")
            ),
            call. = FALSE
        )
    }

    if (!is.null(ctx$cache[[name]])) {
        return(ctx$cache[[name]])
    }

    ctx$stack <- c(ctx$stack, name)
    on.exit(
        {
            ctx$stack <- utils::head(ctx$stack, -1L)
        },
        add = TRUE
    )

    compiled <- schema_flat__node(ctx$defs[[name]], ctx)
    ctx$cache[[name]] <- compiled
    compiled
}

schema_flat__ref <- function(x, ctx) {
    schema_flat__overlay_desc(schema_flat__def(schema_flat__ref_name(x@ref), ctx), x@desc)
}

S7::method(schema_utils__convert, SchemaBindingExactCmpt) <- function(from, to, ...) {
    if (!identical(to, SchemaBindingExactFlat)) {
        stop("`SchemaBindingExactCmpt` can only be converted to `SchemaBindingExactFlat`.", call. = FALSE)
    }
    if (length(from@keys) != 1L) {
        stop("`SchemaBindingExactCmpt` must contain exactly one key to convert to `SchemaBindingExactFlat`.", call. = FALSE)
    }

    SchemaBindingExactFlat(keys = from@keys, target = from@target)
}

schema_flat__binding <- function(binding, ctx) {
    target <- schema_flat__node(binding@target, ctx)
    lapply(binding@keys, function(key) {
        schema_utils__convert(SchemaBindingExactCmpt(keys = key, target = target), SchemaBindingExactFlat)
    })
}

schema_flat__bindings <- function(bindings, ctx) {
    compiled <- unlist(lapply(bindings, schema_flat__binding, ctx = ctx), recursive = FALSE)
    if (length(compiled)) {
        keys <- schema_flat__binding_names(compiled)
        dup_keys <- unique(keys[duplicated(keys)])
        if (length(dup_keys)) {
            stop(sprintf("duplicate compiled field key(s): %s.", paste(dup_keys, collapse = ", ")), call. = FALSE)
        }
    }

    compiled
}

schema_flat__pattern <- function(binding, ctx) {
    SchemaBindingPatternFlat(
        pattern = binding@pattern,
        target = schema_flat__node(binding@target, ctx)
    )
}

schema_flat__patterns <- function(patterns, ctx) {
    lapply(patterns, schema_flat__pattern, ctx = ctx)
}

schema_flat__positions <- function(positions, ctx) {
    lapply(positions, schema_flat__node, ctx = ctx)
}

schema_flat__branches <- function(branches, ctx) {
    lapply(branches, schema_flat__node, ctx = ctx)
}

schema_flat__doc <- function(x) {
    ctx <- schema_flat__context(x@defs)

    SchemaFlat(
        path = x@path,
        version = x@version,
        root = schema_flat__node(x@root, ctx)
    )
}

schema_flat__node <- S7::new_generic(
    "schema_flat__node",
    "x",
    function(x, ctx) S7::S7_dispatch()
)

S7::method(schema_flat__node, SchemaNodeLeaf) <- function(x, ctx) {
    SchemaNodeLeaf(
        value = schema_flat__rule_check(x@value),
        name = schema_flat__rule_names(x@name),
        desc = x@desc
    )
}

S7::method(schema_flat__node, SchemaNodeContainerCmpt) <- function(x, ctx) {
    exact <- schema_flat__bindings(x@exact, ctx)
    patterns <- schema_flat__patterns(x@patterns, ctx)
    positions <- schema_flat__positions(x@positions, ctx)
    rest <- if (is.null(x@rest)) NULL else schema_flat__node(x@rest, ctx)

    SchemaNodeContainerFlat(
        value = schema_flat__rule_check(x@value),
        name = schema_flat__rule_names(x@name),
        exact = exact,
        patterns = patterns,
        positions = positions,
        rest = rest,
        desc = x@desc
    )
}

S7::method(schema_flat__node, SchemaNodeRef) <- function(x, ctx) {
    schema_flat__ref(x, ctx)
}

S7::method(schema_flat__node, SchemaNodeAllCmpt) <- function(x, ctx) {
    SchemaNodeAllFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeAnyCmpt) <- function(x, ctx) {
    SchemaNodeAnyFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeOneCmpt) <- function(x, ctx) {
    SchemaNodeOneFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeNotCmpt) <- function(x, ctx) {
    SchemaNodeNotFlat(branch = schema_flat__node(x@branch, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNode) <- function(x, ctx) {
    stop("unsupported authoring schema node.", call. = FALSE)
}

S7::method(schema_flat__node, S7::class_any) <- function(x, ctx) {
    stop("unsupported authoring schema node.", call. = FALSE)
}

schema_flat__compile <- S7::new_generic("schema_flat__compile", "x", function(x) S7::S7_dispatch())

S7::method(schema_flat__compile, SchemaFlat) <- function(x) {
    x
}

S7::method(schema_flat__compile, SchemaNodeLeaf) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeContainerFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeAllFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeAnyFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeOneFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeNotFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaDoc) <- function(x) {
    schema_flat__doc(x)
}

S7::method(schema_flat__compile, S7::class_any) <- function(x) {
    stop(
        paste(
            "`schema_flat__compile()` only accepts `SchemaDoc`, flat runtime `SchemaNode`, or `SchemaFlat`.",
            "Use `schema_flat__node()` internally for authoring `SchemaNode` values."
        ),
        call. = FALSE
    )
}
# }}}

# -------------------- schema-json.R
schema_json__read_json <- function(txt) {
    schema_utils__require_namespace("jsonlite", "read schema JSON")
    jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

#' Read schema JSON
#'
#' `schema_read()` reads schema JSON into a `SchemaDoc`. Requires `jsonlite`.
#'
#' @param txt JSON text, local file path, or URL accepted by
#'   `jsonlite::fromJSON()`.
#'
#' @return A `SchemaDoc`.
#'
#' @noRd
schema_read <- function(txt) {
    json <- schema_json__read_json(txt)
    path <- NULL
    if (checkmate::test_string(txt) && !jsonlite::validate(txt)) {
        if (grepl("^https?://", txt, useBytes = TRUE) || file.exists(txt)) {
            path <- txt
        }
    }
    schema_doc(json, path)
}

# -------------------- schema-validate.R
# schema_validate {{{
#' Validate input against a schema
#'
#' `schema_validate()` validates an R object against a `SchemaDoc`,
#' `SchemaFlat`, or compiled flat schema node.
#'
#' @param schema A `SchemaDoc`, `SchemaFlat`, or compiled flat schema node.
#' @param x Input object to validate.
#' @param mode One of `"assert"`, `"check"`, `"test"`, or `"expect"`.
#' @param name Optional display name used in validation messages.
#' @param ... Reserved for future extension.
#'
#' @return In `"assert"` mode, invisibly returns `x` or throws an error. In
#'   `"check"` mode, returns `TRUE` or a diagnostic string. In `"test"` mode,
#'   returns `TRUE` or `FALSE`. In `"expect"` mode, returns a testthat-style
#'   expectation object.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     fields = list(id = list(check = list(kind = "int", lower = 1)))
#' ))
#' schema
#'
#' schema_validate(schema, list(id = 1L), mode = "test")
#' schema_validate(schema, list(id = 0L), mode = "check", name = "payload")
#'
#' @noRd
schema_validate <- S7::new_generic(
    "schema_validate",
    "schema",
    function(schema, x, mode = "assert", name = NULL, ...) S7::S7_dispatch()
)

schema_validate__or <- function(x, y) {
    if (is.null(x)) y else x
}

schema_validate__make_expectation <- function(ok, message = NULL) {
    structure(
        list(
            message = schema_validate__or(message, if (ok) "Validation passed." else "Validation failed."),
            srcref = NULL,
            trace = NULL,
            passed = ok
        ),
        class = if (ok) {
            c("expectation_success", "expectation", "condition")
        } else {
            c("expectation_failure", "expectation", "error", "condition")
        }
    )
}

schema_validate__prefix_message <- function(result, path) {
    if (isTRUE(result)) {
        return(TRUE)
    }

    sprintf("%s: %s", path, result)
}

schema_validate__call_check <- function(fun, x, args = list()) {
    do.call(fun, c(list(x), args))
}

schema_validate__field_path <- function(path, key) {
    paste0(path, "$", key)
}

schema_validate__item_path <- function(path, i) {
    sprintf("%s[[%d]]", path, i)
}

schema_validate__keys_type <- function(schema) {
    if (is.null(schema@name)) {
        return(NULL)
    }

    schema@name@args$type
}

schema_validate__container_has_keyed_children <- function(schema) {
    length(schema@exact) ||
        length(schema@patterns) ||
        !is.null(schema@rest)
}

schema_validate__rule <- function(value, x, path) {
    schema_validate__prefix_message(
        schema_validate__call_check(
            schema_utils__checkmate_fun(value@kind),
            x,
            args = value@args
        ),
        path
    )
}

schema_validate__names_rule <- function(value, x, path) {
    schema_validate__prefix_message(
        schema_validate__call_check(checkmate::check_names, names(x), args = value@args),
        path
    )
}

schema_validate__impl <- S7::new_generic(
    "schema_validate__impl",
    "schema",
    function(schema, x, path) S7::S7_dispatch()
)

S7::method(schema_validate__impl, SchemaNodeLeaf) <- function(schema, x, path) {
    res <- schema_validate__rule(schema@value, x, path)
    if (!isTRUE(res)) {
        return(res)
    }

    if (!is.null(schema@name)) {
        return(schema_validate__names_rule(schema@name, x, path))
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeContainerFlat) <- function(schema, x, path) {
    res <- schema_validate__rule(schema@value, x, path)
    if (!isTRUE(res)) {
        return(res)
    }

    if (!is.null(schema@name)) {
        res <- schema_validate__names_rule(schema@name, x, path)
        if (!isTRUE(res)) {
            return(res)
        }
    }

    if (identical(schema_validate__keys_type(schema), "unnamed")) {
        if (length(schema@exact) || length(schema@patterns)) {
            return(sprintf("%s cannot use named field constraints with `keys$type = 'unnamed'`.", path))
        }

        n_positions <- length(schema@positions)
        n_present <- min(length(x), n_positions)

        if (n_present) {
            for (i in seq_len(n_present)) {
                res <- schema_validate__impl(schema@positions[[i]], x[[i]], schema_validate__item_path(path, i))
                if (!isTRUE(res)) {
                    return(res)
                }
            }
        }

        if (length(x) <= n_positions) {
            return(TRUE)
        }

        extra <- seq.int(n_positions + 1L, length(x))
        if (is.null(schema@rest)) {
            return(TRUE)
        }

        for (i in extra) {
            res <- schema_validate__impl(schema@rest, x[[i]], schema_validate__item_path(path, i))
            if (!isTRUE(res)) {
                return(res)
            }
        }

        return(TRUE)
    }

    raw_names <- names(x)
    if (length(x) && is.null(schema@name) && schema_validate__container_has_keyed_children(schema)) {
        if (!checkmate::test_character(raw_names, null.ok = FALSE, any.missing = FALSE, min.chars = 1L)) {
            return(sprintf(
                "%s must be a named object because this schema declares keyed child constraints.",
                path
            ))
        }
    }

    present <- schema_validate__or(raw_names, character())
    declared <- schema_flat__binding_names(schema@exact)

    for (binding in schema@exact) {
        nm <- schema_flat__binding_name(binding)
        if (!nm %in% present) {
            next
        }

        res <- schema_validate__impl(binding@target, x[[nm]], schema_validate__field_path(path, nm))
        if (!isTRUE(res)) {
            return(res)
        }
    }

    extra <- setdiff(present, declared)
    pattern_matched <- character()
    for (nm in extra) {
        matched <- Filter(function(binding) grepl(binding@pattern, nm), schema@patterns)
        if (!length(matched)) {
            next
        }

        pattern_matched <- c(pattern_matched, nm)
        for (binding in matched) {
            res <- schema_validate__impl(binding@target, x[[nm]], schema_validate__field_path(path, nm))
            if (!isTRUE(res)) {
                return(res)
            }
        }
    }

    extra <- setdiff(extra, pattern_matched)
    if (!length(extra)) {
        return(TRUE)
    }

    if (is.null(schema@rest)) {
        return(TRUE)
    }

    for (nm in extra) {
        res <- schema_validate__impl(schema@rest, x[[nm]], schema_validate__field_path(path, nm))
        if (!isTRUE(res)) {
            return(res)
        }
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeAllFlat) <- function(schema, x, path) {
    for (branch in schema@branches) {
        res <- schema_validate__impl(branch, x, path)
        if (!isTRUE(res)) {
            return(res)
        }
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeAnyFlat) <- function(schema, x, path) {
    msgs <- character()
    for (i in seq_along(schema@branches)) {
        branch <- schema@branches[[i]]
        res <- schema_validate__impl(branch, x, path)
        if (isTRUE(res)) {
            return(TRUE)
        }
        msgs <- c(msgs, sprintf("[%d] %s", i, res))
    }

    sprintf("%s failed all branches of `any`: %s", path, paste(msgs, collapse = " | "))
}

S7::method(schema_validate__impl, SchemaNodeOneFlat) <- function(schema, x, path) {
    ok <- 0L
    msgs <- character()
    for (i in seq_along(schema@branches)) {
        branch <- schema@branches[[i]]
        res <- schema_validate__impl(branch, x, path)
        if (isTRUE(res)) {
            ok <- ok + 1L
        } else {
            msgs <- c(msgs, sprintf("[%d] %s", i, res))
        }
    }

    if (ok == 1L) {
        return(TRUE)
    }
    if (ok == 0L) {
        return(sprintf("%s matched no branches of `one`: %s", path, paste(msgs, collapse = " | ")))
    }

    sprintf("%s matched multiple branches of `one` (%d).", path, ok)
}

S7::method(schema_validate__impl, SchemaNodeNotFlat) <- function(schema, x, path) {
    res <- schema_validate__impl(schema@branch, x, path)
    if (isTRUE(res)) {
        return(sprintf("%s: `not` branch matched.", path))
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNode) <- function(schema, x, path) {
    stop("unsupported compiled schema node.", call. = FALSE)
}

S7::method(schema_validate__impl, S7::class_any) <- function(schema, x, path) {
    stop("unsupported compiled schema node.", call. = FALSE)
}

schema_validate__dispatch <- function(result, x, mode) {
    checkmate::assert_choice(mode, c("assert", "check", "test", "expect"))

    if (identical(mode, "check")) {
        return(result)
    }
    if (identical(mode, "test")) {
        return(isTRUE(result))
    }
    if (identical(mode, "expect")) {
        return(schema_validate__make_expectation(isTRUE(result), if (isTRUE(result)) NULL else result))
    }

    if (isTRUE(result)) {
        return(invisible(x))
    }
    stop(result, call. = FALSE)
}

S7::method(schema_validate, SchemaFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema@root, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeLeaf) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeContainerFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeAllFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeAnyFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeOneFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeNotFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaDoc) <- function(schema, x, mode = "assert", name = NULL, ...) {
    schema_validate(schema_flat__compile(schema), x, mode = mode, name = name, ...)
}
# }}}

# nocov end


# vim: fdm=marker :
