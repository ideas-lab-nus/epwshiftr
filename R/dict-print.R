esgdict__print_trunc <- function(x, n, newline_before = is.data.frame(x)) {
    d <- cli::cli_div(theme = list(body = list(`padding-left` = 0L, `margin-left` = 0L)))
    total <- if (is.data.frame(x)) nrow(x) else length(x)
    if (n < total) {
        if (newline_before) cli::cli_text()
        cli::cli_text(cli::col_grey("# ... with {total - n} more item{?s}"))
    }
    cli::cli_end(d)
}

esgdict__print_list <- function(x, elem = "") {
    if (!length(x)) return()

    if (length(x) > 1L) {
        cli::cli_li("{.strong {to_title_case(elem)}}:")
        ul <- cli::cli_ul()
        for (nm in names(x)) {
            esgdict__print_list(x[[nm]], nm)
        }
        cli::cli_end(ul)
    } else {
        cli::cli_li("{.strong {to_title_case(elem)}}: {.val {unlst(x)}}")
    }
}

esgdict__print_cv_rule <- function(name) {
    d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
    cli::cli_rule("{.strong Cmip6CV {name}}", right = "{.strong CMIP6 Dictionary}")
    cli::cli_end(d)
}

esgdict__print_cv_version <- function(cv, name = "") {
    ver <- attr(cv, "version", TRUE)

    d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
    ul <- cli::cli_ul()
    cli::cli_li("{.strong CV Version}: {.var {ver$CV_collection_version}}")
    cli::cli_li("{.strong CV Modified}: {format(ver$CV_collection_modified, '%F %T %Z')}")
    cli::cli_li("{.strong {name} Modified}: {format(ver$CV_modified, '%F %T %Z')}")
    cli::cli_li("{.strong {name} Note}: {.val {ver$CV_note}}")
    cli::cli_end(ul)
    cli::cli_end(d)

    invisible(cv)
}

esgdict__print_cv_vec <- function(cv, n = 5L) {
    cli::cli_h1("Values {.cls {typeof(cv)}}")

    n <- min(n, length(cv))
    txt <- cli::cli_vec(unclass(cv), list(vec_trunc = n))
    cli::cli_text("{.val {txt}}")

    esgdict__print_trunc(cv, n)
    invisible(cv)
}

esgdict__print_cv_list <- function(cv, n = 5L, to_title = FALSE) {
    cli::cli_h1("Values {.cls list}")

    n <- min(n, length(cv))
    nms <- names(cv)
    if (to_title) nms <- to_title_case(nms)

    d <- cli::cli_div(theme = list(
        ul = list(`margin-left` = 0L, `padding-left` = 0L),
        li = list(`margin-left` = 0L, `padding-left` = 2L)
    ))
    ul <- cli::cli_ul()
    for (i in seq.int(n)) {
        cli::cli_li("{.strong {nms[i]}}: {.val {unlst(cv[i])}}")
    }
    cli::cli_end(ul)
    cli::cli_end(d)

    esgdict__print_trunc(cv, n)
    invisible(cv)
}

esgdict__print_cv_table <- function(cv, n = 3L) {
    n <- min(n, nrow(cv))
    cols <- names(cv)

    cli::cli_h1("Values {.cls data.table}")
    for (i in seq.int(n)) {
        dt <- cv[i]
        d <- cli::cli_div(theme = list(
            h2 = list("margin-left" = 2L, "margin-bottom" = 0L),
            li = list("padding-left" = 2L)
        ))
        cli::cli_h2("[{to_title_case(cols[1])}: {.strong {.val {dt[[cols[1]]]}}}]")
        ul <- cli::cli_ul()
        for (col in cols[-1L]) {
            if (is.list(dt[[col]][[1L]])) {
                esgdict__print_list(dt[[col]][[1L]], col)
            } else {
                cli::cli_li("{.strong {to_title_case(col)}}: {.val {unlst(dt[[col]])}}")
            }
        }
        cli::cli_end(ul)
        cli::cli_end(d)
    }

    esgdict__print_trunc(cv, n)
    invisible(cv)
}

#' @export
print.Cmip6CV <- function(x, n = NULL, ...) {
    cls <- sub("Cmip6CV_", "", class(x)[[1L]], fixed = TRUE)
    if (is.null(n)) {
        n <- if (is.data.frame(x)) 3L else if (is.list(x)) 5L else 10L
    }
    switch(cls,
        DRS = {
            esgdict__print_cv_rule("Data Reference Syntax (DRS)")
            esgdict__print_cv_version(x, "DRS")
            esgdict__print_cv_list(x, n, TRUE)
        },
        ActivityId = {
            esgdict__print_cv_rule("Activity ID")
            esgdict__print_cv_version(x, "ActivityId")
            esgdict__print_cv_list(x, n)
        },
        ExperimentId = {
            esgdict__print_cv_rule("Experiment ID")
            esgdict__print_cv_version(x, "ExperimentId")
            esgdict__print_cv_table(x, n)
        },
        Frequency = {
            esgdict__print_cv_rule("Frequency")
            esgdict__print_cv_version(x, "Frequency")
            esgdict__print_cv_list(x, n)
        },
        GridLabel = {
            esgdict__print_cv_rule("Grid Label")
            esgdict__print_cv_version(x, "GridLabel")
            esgdict__print_cv_list(x, n)
        },
        InstitutionId = {
            esgdict__print_cv_rule("Institution ID")
            esgdict__print_cv_version(x, "InstitutionId")
            esgdict__print_cv_list(x, n)
        },
        Resolution = {
            esgdict__print_cv_rule("Nominal Resolution")
            esgdict__print_cv_version(x, "NominalResolution")
            esgdict__print_cv_vec(x, n)
        },
        Realm = {
            esgdict__print_cv_rule("Realm")
            esgdict__print_cv_version(x, "Realm")
            esgdict__print_cv_list(x, n)
        },
        ReqGlobAttr = {
            esgdict__print_cv_rule("Required Global Attributes")
            esgdict__print_cv_version(x, "ReqGlobAttr")
            esgdict__print_cv_vec(x, n)
        },
        SourceId = {
            esgdict__print_cv_rule("Source ID")
            esgdict__print_cv_version(x, "SourceId")
            esgdict__print_cv_table(x, n)
        },
        SourceType = {
            esgdict__print_cv_rule("Source Type")
            esgdict__print_cv_version(x, "SourceType")
            esgdict__print_cv_list(x, n)
        },
        SubExperimentId = {
            esgdict__print_cv_rule("Sub Experiment ID")
            esgdict__print_cv_version(x, "SubExperimentId")
            esgdict__print_cv_list(x, n)
        },
        TableId = {
            esgdict__print_cv_rule("Table ID")
            esgdict__print_cv_version(x, "TableId")
            esgdict__print_cv_vec(x, n)
        }
    )

    invisible(x)
}

esgdict__print_dreq_rule <- function() {
    d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
    cli::cli_rule("{.strong CMIP6 Data Request}", right = "{.strong CMIP6 Dictionary}")
    cli::cli_end(d)
}

esgdict__print_dreq_meta <- function(dreq) {
    meta <- attr(dreq, "metadata", TRUE)

    cli::cli_h1("<Header Metadata>")

    d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
    ul <- cli::cli_ul()
    cli::cli_li("{.strong DReq Version}: {.var {meta$dreq_version[[1L]]}}")
    cli::cli_li("{.strong CMOR Version}: {.var {meta$cmor_version[[1L]]}}")
    cli::cli_li("{.strong MIP Era}: {.var {meta$mip_era[[1L]]}}")
    cli::cli_li("{.strong Missing Value}:")
    d2 <- cli::cli_div(theme = list(`li` = list(`margin-left` = 2L, `padding-left` = 2L)))
    ul2 <- cli::cli_ul()
    cli::cli_li("Real: {.var {meta$dbl_missing_value[[1L]]}}")
    cli::cli_li("Int: {.var {meta$int_missing_value[[1L]]}}")
    cli::cli_end(ul2)
    cli::cli_end(d2)
    cli::cli_li("{.strong Conventions}: {.var {meta$conventions[[1L]]}}")
    cli::cli_li("{.var {nrow(dreq)}} Variables from {.var {length(unique(meta$table_id))}} Tables and {.var {length(unique(meta$realm))}} Realms")
    cli::cli_end(ul)
    cli::cli_end(d)

    invisible(dreq)
}

#' @export
print.Cmip6DReq <- function(x, n = 3L, ...) {
    esgdict__print_dreq_rule()
    esgdict__print_dreq_meta(x)
    esgdict__print_cv_table(x, n)
    invisible(x)
}
