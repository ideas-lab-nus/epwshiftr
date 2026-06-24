ESGDICT_RELATION_FIELDS <- list(
    variable = c("variable_id", "table_id", "frequency", "realm"),
    activity_experiment = c("activity_id", "experiment_id", "sub_experiment_id"),
    activity_source = c("activity_id", "source_id", "institution_id")
)

ESGDICT_FIELD_ALIASES <- c(
    activity = "activity_id",
    activity_drs = "activity_id",
    experiment = "experiment_id",
    source = "source_id",
    variable = "variable_id",
    variant = "variant_label",
    member_id = "variant_label",
    resolution = "nominal_resolution",
    modeling_realm = "realm"
)

#' Discover Valid ESG Dictionary Options
#'
#' `esgdict_option()` returns valid values for an ESG dictionary field,
#' optionally constrained by other supplied fields.
#'
#' @param field An ESG dictionary field name or supported alias.
#' @param ... Optional field constraints, such as `activity_id = "CMIP"` or
#'   `table_id = "day"` for CMIP6.
#' @param project ESG project identifier. If `NULL`, the project is inferred
#'   from `dict` when supplied, otherwise `"CMIP6"` is used.
#' @param dict Optional [EsgDict] object. If `NULL`, the package-level default
#'   dictionary for `project` is used when available; otherwise the project
#'   default JSON file is loaded from `getOption("epwshiftr.dir", ".")`.
#' @param warn_ignored If `TRUE`, warn when supplied constraints cannot be used
#'   by any available relation index.
#'
#' @return A [data.table::data.table()] with at least `field`, `value`, and
#'   `description` columns. The `ignored_constraints` attribute records
#'   constraints that were not used.
#'
#' @export
esgdict_option <- function(field, ..., project = NULL, dict = NULL, warn_ignored = TRUE) {
    dict <- esgdict__resolve(project, dict)
    esgdict__options(dict, field, list(...), warn_ignored = warn_ignored)
}

#' Check ESG Dictionary Parameter Values
#'
#' `esgdict_check()` validates project parameter values against the local ESG
#' dictionary. It checks individual field values and cross-field relationships
#' represented in the dictionary's normalized query indices.
#'
#' @param ... ESG dictionary field values.
#' @inheritParams esgdict_option
#' @param error If `TRUE`, throw an error when invalid values or relationships
#'   are found.
#' @param suggest If `TRUE`, include near-match suggestions for invalid values.
#' @param n_suggestions Maximum number of suggestions to keep for each invalid
#'   value.
#' @param relationship Relationship validation mode. `"any"` validates
#'   ESGF-query style OR semantics. `"all_pairs"` requires every supplied
#'   combination inside each relation index to exist.
#'
#' @return An `esgdict_check_result` [data.table::data.table()].
#'
#' @export
esgdict_check <- function(
    ...,
    project = NULL,
    dict = NULL,
    error = FALSE,
    suggest = TRUE,
    n_suggestions = 5L,
    relationship = c("any", "all_pairs")
) {
    dict <- esgdict__resolve(project, dict)
    esgdict__check(
        dict,
        list(...),
        error = error,
        suggest = suggest,
        n_suggestions = n_suggestions,
        relationship = relationship
    )
}

#' @export
print.esgdict_check_result <- function(x, ...) {
    out <- data.table::copy(x)
    data.table::setattr(out, "class", setdiff(class(out), "esgdict_check_result"))
    print(out, ...)
    invisible(x)
}

esgdict__resolve <- function(project = NULL, dict = NULL) {
    if (!is.null(dict) && !inherits(dict, "EsgDict")) {
        stop("`dict` must be an `EsgDict` object.", call. = FALSE)
    }
    if (!is.null(dict) && is.null(project)) {
        project <- dict$project()
    }
    if (is.null(project)) {
        project <- "CMIP6"
    }
    project <- esgdict__normalize_project(project)
    esgdict__assert_implemented(project)

    if (is.null(dict)) {
        dict <- esgdict_get_default(project)
        if (is.null(dict)) {
            dict <- EsgDict$new(project = project)
            dict$load()
        }
    }

    if (!identical(dict$project(), project)) {
        stop(
            sprintf(
                "`dict` project `%s` does not match requested project `%s`.",
                dict$project(),
                project
            ),
            call. = FALSE
        )
    }

    if (!dict$has_data()) {
        stop(
            "The ESG dictionary does not contain usable data. Build it with ",
            "`esgdict()$build()` or load a local project dictionary JSON first.",
            call. = FALSE
        )
    }

    dict
}

esgdict__relation_fields <- function(project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    fields <- ESGDICT_RELATION_FIELDS
    if (is.null(esgdict__project_spec(project)$request)) {
        fields <- fields[names(fields) != "variable"]
    }
    fields
}

esgdict__indices <- function(project, data) {
    indices <- list(values = esgdict__index_values(project, data))

    variable <- esgdict__index_request(data$request)
    if (nrow(variable)) {
        indices$variable <- variable
    }

    activity_experiment <- cmip6dict__index_activity_experiment(data$vocab$experiment_id)
    if (nrow(activity_experiment)) {
        indices$activity_experiment <- activity_experiment
    }

    activity_source <- cmip6dict__index_activity_source(data$vocab$source_id)
    if (nrow(activity_source)) {
        indices$activity_source <- activity_source
    }

    indices
}

cmip6dict__indices <- function(data) {
    if (is.null(data$vocab) && !is.null(data$cvs)) data$vocab <- data$cvs
    if (is.null(data$request) && !is.null(data$dreq)) data$request <- data$dreq
    esgdict__indices("CMIP6", data)
}

esgdict__index_values <- function(project, data) {
    project <- esgdict__normalize_project(project)
    rows <- list(
        cmip6dict__value_rows("project", project, sprintf("%s project", project), "constant"),
        cmip6dict__value_rows("mip_era", project, sprintf("%s project", project), "constant")
    )
    if (identical(project, "CMIP6")) {
        rows[[length(rows) + 1L]] <- data.table::data.table(
            field = "variant_label",
            value = NA_character_,
            description = "CMIP6 realization-initialization-physics-forcing member label",
            pattern = CMIP6DICT_VARIANT_PATTERN,
            source = "pattern"
        )
    }

    for (field in names(data$vocab)) {
        if (field %in% c("drs", "required_global_attributes")) next
        cv <- data$vocab[[field]]
        if (is.null(cv)) next

        if (is.data.frame(cv)) {
            rows[[length(rows) + 1L]] <- esgdict__value_rows_from_table(field, cv)
        } else if (is.list(cv)) {
            value <- names(cv)
            if (is.null(value)) value <- cmip6dict__as_character(cv)
            rows[[length(rows) + 1L]] <- cmip6dict__value_rows(
                field,
                value,
                vapply(cv, cmip6dict__description, character(1L), USE.NAMES = FALSE),
                "vocab"
            )
        } else {
            rows[[length(rows) + 1L]] <- cmip6dict__value_rows(field, unclass(cv), NA_character_, "vocab")
        }
    }

    if (!is.null(data$request) && nrow(data$request)) {
        rows[[length(rows) + 1L]] <- cmip6dict__value_rows(
            "variable_id",
            unique(data$request$variable),
            NA_character_,
            "request"
        )
    }

    unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

cmip6dict__index_values <- function(data) {
    if (is.null(data$vocab) && !is.null(data$cvs)) data$vocab <- data$cvs
    if (is.null(data$request) && !is.null(data$dreq)) data$request <- data$dreq
    esgdict__index_values("CMIP6", data)
}

esgdict__value_rows_from_table <- function(field, dt) {
    value_col <- if (field %in% names(dt)) {
        field
    } else if ("value" %in% names(dt)) {
        "value"
    } else {
        names(dt)[[1L]]
    }

    desc_cols <- intersect(c("description", "label_extended", "label", "title", "name"), names(dt))
    desc <- rep(NA_character_, nrow(dt))
    for (col in desc_cols) {
        replacement <- as.character(dt[[col]])
        empty <- is.na(desc) | !nzchar(desc)
        desc[empty] <- replacement[empty]
    }

    cmip6dict__value_rows(field, dt[[value_col]], desc, "vocab")
}

cmip6dict__value_rows <- function(field, value, description = NA_character_, source = NA_character_) {
    if (is.null(description)) {
        description <- NA_character_
    }
    if (is.null(source)) {
        source <- NA_character_
    }
    data.table::data.table(
        field = field,
        value = as.character(value),
        description = as.character(description),
        pattern = NA_character_,
        source = source
    )
}

cmip6dict__description <- function(x) {
    x <- unlst(x)
    if (length(x) == 1L) return(as.character(x))
    NA_character_
}

esgdict__index_request <- function(request) {
    if (is.null(request) || !nrow(request)) {
        return(data.table::data.table(
            variable_id = character(),
            table_id = character(),
            frequency = character(),
            realm = character(),
            long_name = character(),
            units = character()
        ))
    }

    out <- data.table::data.table(
        variable_id = request$variable,
        table_id = request$table_id,
        frequency = request$frequency,
        realm = request$modeling_realm,
        long_name = request$long_name,
        units = request$units
    )
    unique(out)
}

cmip6dict__index_dreq <- esgdict__index_request

cmip6dict__index_activity_experiment <- function(dt) {
    cmip6dict__expand_relation(
        dt,
        c("activity_id", "experiment_id", "sub_experiment_id")
    )
}

cmip6dict__index_activity_source <- function(dt) {
    if (is.null(dt) || !nrow(dt)) {
        return(data.table::data.table(
            activity_id = character(),
            source_id = character(),
            institution_id = character()
        ))
    }

    rows <- vector("list", nrow(dt))
    for (i in seq_len(nrow(dt))) {
        rows[[i]] <- data.table::CJ(
            activity_id = cmip6dict__cell_values(dt$activity_participation, i),
            source_id = cmip6dict__cell_values(dt$source_id, i),
            institution_id = cmip6dict__cell_values(dt$institution_id, i),
            sorted = FALSE
        )
    }
    unique(data.table::rbindlist(rows, use.names = TRUE))
}

cmip6dict__expand_relation <- function(dt, fields) {
    empty <- data.table::as.data.table(stats::setNames(rep(list(character()), length(fields)), fields))
    if (is.null(dt) || !nrow(dt)) return(empty)

    rows <- vector("list", nrow(dt))
    for (i in seq_len(nrow(dt))) {
        args <- lapply(fields, function(field) cmip6dict__cell_values(dt[[field]], i))
        names(args) <- fields
        args$sorted <- FALSE
        rows[[i]] <- do.call(data.table::CJ, args)
    }
    unique(data.table::rbindlist(rows, use.names = TRUE))
}

cmip6dict__cell_values <- function(col, i) {
    if (is.list(col) && !inherits(col, c("Date", "POSIXt", "numeric_version"))) {
        return(cmip6dict__as_character(col[[i]]))
    }
    cmip6dict__as_character(col[i])
}

esgdict__options <- function(dict, field, constraints = list(), warn_ignored = TRUE) {
    field <- esgdict__normalize_field(field, dict)
    constraints <- esgdict__normalize_constraints(constraints, dict)
    constraints[[field]] <- NULL

    values <- esgdict__candidate_values(dict, field, constraints)
    out <- dict$indices("values")
    target_field <- field
    target_values <- values$value
    out <- out[out[["field"]] == target_field & (is.na(out[["value"]]) | out[["value"]] %in% target_values)]

    if (!nrow(out) && length(values$value)) {
        out <- data.table::data.table(
            field = field,
            value = values$value,
            description = NA_character_,
            pattern = NA_character_,
            source = values$source
        )
    }

    ignored <- setdiff(names(constraints), values$used_constraints)
    data.table::setattr(out, "ignored_constraints", ignored)
    if (warn_ignored && length(ignored)) {
        warning(
            sprintf(
                "Ignored constraint%s: %s",
                if (length(ignored) == 1L) "" else "s",
                paste(ignored, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    out
}

cmip6dict__options <- esgdict__options

esgdict__candidate_values <- function(dict, field, constraints) {
    all_values <- esgdict__field_values(dict, field)
    candidates <- all_values$value
    used_constraints <- character()
    used_source <- all_values$source

    indices <- dict$indices()
    relation_fields <- esgdict__relation_fields(dict$project())
    for (idx_name in intersect(names(relation_fields), names(indices))) {
        idx_fields <- relation_fields[[idx_name]]
        if (!(field %in% idx_fields)) next

        active <- intersect(names(constraints), setdiff(idx_fields, field))
        if (!length(active)) next

        idx <- data.table::copy(indices[[idx_name]])
        for (constraint in active) {
            values <- cmip6dict__as_character(constraints[[constraint]])
            idx <- idx[idx[[constraint]] %in% values]
        }
        candidates <- intersect(candidates, unique(idx[[field]]))
        used_constraints <- union(used_constraints, active)
        used_source <- idx_name
    }

    list(
        value = unique(candidates),
        source = used_source,
        used_constraints = used_constraints
    )
}

cmip6dict__candidate_values <- esgdict__candidate_values

esgdict__field_values <- function(dict, field) {
    values <- dict$indices("values")
    target_field <- field
    out <- values[
        values[["field"]] == target_field & !is.na(values[["value"]]),
        c("value", "source"),
        with = FALSE
    ]
    unique(out)
}

cmip6dict__field_values <- esgdict__field_values

esgdict__check <- function(
    dict,
    args,
    error = FALSE,
    suggest = TRUE,
    n_suggestions = 5L,
    relationship = c("any", "all_pairs")
) {
    checkmate::assert_flag(error)
    checkmate::assert_flag(suggest)
    checkmate::assert_count(n_suggestions)
    relationship <- match.arg(relationship)

    args <- esgdict__normalize_constraints(args, dict)
    args <- lapply(args, cmip6dict__as_character)
    args <- args[lengths(args) > 0L]

    rows <- list()
    for (field in names(args)) {
        choices <- esgdict__field_values(dict, field)$value
        # Relation-only fields may be unknown for CV-only projects. In that
        # case, the relationship row below should report `not_checked`.
        if (!length(choices) && field %in% unlist(ESGDICT_RELATION_FIELDS, use.names = FALSE)) {
            next
        }
        for (value in args[[field]]) {
            valid <- cmip6dict__value_is_valid(field, value, choices)
            rows[[length(rows) + 1L]] <- cmip6dict__check_row(
                field = field,
                value = value,
                valid = valid,
                type = "value",
                rule = "field_value",
                source = esgdict__check_source(dict, field),
                constraint_fields = character(),
                message = if (valid) NA_character_ else sprintf("`%s` is not a valid `%s`.", value, field),
                suggestions = if (!valid && suggest) cmip6dict__suggest(value, choices, n_suggestions) else character(),
                compatible_values = character()
            )
        }
    }

    relation_rows <- esgdict__check_relations(dict, args, relationship = relationship)
    res <- esgdict__check_result(c(rows, relation_rows))
    invalid <- res[!is.na(res$valid) & !res$valid]

    if (error && nrow(invalid)) {
        stop(paste(stats::na.omit(invalid$message), collapse = "\n"), call. = FALSE)
    }

    res
}

cmip6dict__check <- esgdict__check

esgdict__check_source <- function(dict, field) {
    values <- dict$indices("values")
    target_field <- field
    src <- values[values[["field"]] == target_field & !is.na(values[["source"]]), unique(source)]
    if (length(src)) src[[1L]] else NA_character_
}

cmip6dict__check_source <- esgdict__check_source

cmip6dict__value_is_valid <- function(field, value, choices) {
    if (identical(field, "variant_label")) {
        return(grepl(CMIP6DICT_VARIANT_PATTERN, value))
    }
    value %in% choices
}

cmip6dict__suggest <- function(value, choices, n) {
    choices <- unique(stats::na.omit(choices))
    if (!length(choices)) return(character())

    exact <- choices[tolower(choices) == tolower(value)]
    if (length(exact)) return(utils::head(exact, n))

    dist <- utils::adist(value, choices, ignore.case = TRUE)
    choices[utils::head(order(dist[1L, ]), n)]
}

esgdict__check_relations <- function(dict, args, relationship) {
    rows <- list()
    indices <- dict$indices()
    for (idx_name in names(ESGDICT_RELATION_FIELDS)) {
        fields <- intersect(names(args), ESGDICT_RELATION_FIELDS[[idx_name]])
        if (length(fields) < 2L) next

        idx <- indices[[idx_name]]
        if (is.null(idx) || !nrow(idx)) {
            rows[[length(rows) + 1L]] <- esgdict__not_checked_row(idx_name, fields, args)
            next
        }
        if (identical(relationship, "any")) {
            rows <- c(rows, cmip6dict__check_relation_any(idx, idx_name, fields, args))
        } else {
            rows <- c(rows, cmip6dict__check_relation_all_pairs(idx, idx_name, fields, args))
        }
    }
    rows
}

cmip6dict__check_relations <- esgdict__check_relations

esgdict__not_checked_row <- function(idx_name, fields, args) {
    value <- paste(sprintf("%s=%s", fields, vapply(fields, function(field) {
        paste(cmip6dict__as_character(args[[field]]), collapse = "|")
    }, character(1L))), collapse = ", ")

    cmip6dict__check_row(
        field = paste(fields, collapse = "+"),
        value = value,
        valid = NA,
        type = "not_checked",
        rule = idx_name,
        source = NA_character_,
        constraint_fields = fields,
        message = sprintf("Combination cannot be checked because `%s` relation data is not available.", idx_name),
        suggestions = character(),
        compatible_values = character()
    )
}

cmip6dict__check_relation_any <- function(idx, idx_name, fields, args) {
    rows <- list()
    valid_args <- lapply(fields, function(field) intersect(args[[field]], unique(idx[[field]])))
    names(valid_args) <- fields
    if (any(lengths(valid_args) == 0L)) return(rows)

    for (field in fields) {
        other_fields <- setdiff(fields, field)
        for (value in valid_args[[field]]) {
            filtered <- idx[idx[[field]] == value]
            for (other in other_fields) {
                filtered <- filtered[filtered[[other]] %in% valid_args[[other]]]
            }
            if (!nrow(filtered)) {
                rows[[length(rows) + 1L]] <- cmip6dict__relation_row(
                    field = field,
                    value = value,
                    rule = idx_name,
                    source = idx_name,
                    constraint_fields = other_fields,
                    message = sprintf(
                        "`%s` is not compatible with supplied %s constraints.",
                        value,
                        paste(other_fields, collapse = ", ")
                    ),
                    compatible_values = cmip6dict__compatible_values(idx, field, value, other_fields)
                )
            }
        }
    }
    rows
}

cmip6dict__check_relation_all_pairs <- function(idx, idx_name, fields, args) {
    grids <- lapply(fields, function(field) intersect(args[[field]], unique(idx[[field]])))
    names(grids) <- fields
    if (any(lengths(grids) == 0L)) return(list())

    combos <- do.call(data.table::CJ, c(grids, sorted = FALSE))
    rows <- list()
    for (i in seq_len(nrow(combos))) {
        filtered <- idx
        for (field in fields) {
            filtered <- filtered[filtered[[field]] == combos[[field]][[i]]]
        }
        if (!nrow(filtered)) {
            combo_value <- paste(sprintf("%s=%s", fields, unlist(combos[i], use.names = FALSE)), collapse = ", ")
            rows[[length(rows) + 1L]] <- cmip6dict__relation_row(
                field = paste(fields, collapse = "+"),
                value = combo_value,
                rule = idx_name,
                source = idx_name,
                constraint_fields = fields,
                message = sprintf("Combination is not valid in `%s`: %s.", idx_name, combo_value),
                compatible_values = character()
            )
        }
    }
    rows
}

cmip6dict__compatible_values <- function(idx, field, value, other_fields) {
    filtered <- idx[idx[[field]] == value]
    unique(unlist(filtered[, other_fields, with = FALSE], use.names = FALSE))
}

cmip6dict__relation_row <- function(field, value, rule, source, constraint_fields, message, compatible_values) {
    cmip6dict__check_row(
        field = field,
        value = value,
        valid = FALSE,
        type = "relationship",
        rule = rule,
        source = source,
        constraint_fields = constraint_fields,
        message = message,
        suggestions = character(),
        compatible_values = compatible_values
    )
}

cmip6dict__check_row <- function(
    field,
    value,
    valid,
    type,
    rule,
    source,
    constraint_fields,
    message,
    suggestions = character(),
    compatible_values = character()
) {
    list(
        field = field,
        value = value,
        valid = valid,
        type = type,
        rule = rule,
        source = source,
        constraint_fields = list(constraint_fields),
        message = message,
        suggestions = list(suggestions),
        compatible_values = list(compatible_values)
    )
}

esgdict__check_result <- function(rows) {
    if (!length(rows)) {
        out <- data.table::data.table(
            field = character(),
            value = character(),
            valid = logical(),
            type = character(),
            rule = character(),
            source = character(),
            constraint_fields = list(),
            message = character(),
            suggestions = list(),
            compatible_values = list()
        )
    } else {
        out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    }
    data.table::setattr(out, "class", c("esgdict_check_result", class(out)))
    out
}

esgdict__normalize_field <- function(field, dict = NULL) {
    checkmate::assert_string(field, min.chars = 1L)
    field <- tolower(field)
    if (field %in% names(ESGDICT_FIELD_ALIASES)) {
        field <- unname(ESGDICT_FIELD_ALIASES[[field]])
    }

    choices <- CMIP6DICT_FIELDS
    if (!is.null(dict) && inherits(dict, "EsgDict") && dict$has_data()) {
        choices <- unique(c(dict$fields(), unlist(ESGDICT_RELATION_FIELDS, use.names = FALSE)))
    }
    checkmate::assert_choice(field, choices)
    field
}

cmip6dict__normalize_field <- esgdict__normalize_field

esgdict__normalize_constraints <- function(args, dict = NULL) {
    if (!length(args)) return(list())

    nms <- names(args)
    if (is.null(nms) || any(!nzchar(nms))) {
        stop("ESG dictionary field values must be named.", call. = FALSE)
    }

    keep <- !vapply(args, is.null, logical(1L))
    args <- args[keep]
    nms <- nms[keep]
    if (!length(args)) return(list())

    nms <- vapply(nms, esgdict__normalize_field, character(1L), dict = dict, USE.NAMES = FALSE)
    names(args) <- nms

    out <- list()
    for (nm in unique(nms)) {
        out[[nm]] <- unlist(args[nms == nm], recursive = FALSE, use.names = FALSE)
    }
    out
}

cmip6dict__normalize_constraints <- esgdict__normalize_constraints

cmip6dict__as_character <- function(x) {
    if (is.null(x)) return(character())
    x <- unlst(x)
    x <- x[!is.na(x)]
    as.character(x)
}
