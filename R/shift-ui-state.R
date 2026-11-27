#' @include shift-stage.R
NULL

# Zero-ANSI single source of truth for Shift UI state, ViewModels, snapshots,
# event deltas, and plain-text domain formatters. Shared by the terminal TUI
# (R/shift-ui-render.R), CLI --json/--jsonl emitters, and future Web App
# consumers. This file must never emit ANSI escapes or call terminal APIs.

# ShiftUiOptions {{{
ShiftUiOptions <- S7::new_class(
    "ShiftUiOptions",
    properties = list(
        progress = shift_stage__prop_string(min.chars = 1L),
        detail = shift_stage__prop_string(min.chars = 1L),
        motion = shift_stage__prop_string(min.chars = 1L),
        refresh = S7::new_property(S7::class_numeric),
        heartbeat = S7::new_property(S7::class_numeric),
        # Transient batch context never participates in a scientific plan hash.
        batch_context = S7::new_property(S7::class_list, default = list()),
        # CLI result emitters own their final batch receipt; child progress is
        # still rendered normally. This flag never enters persisted intent.
        batch_receipt = S7::new_property(S7::class_logical, default = TRUE)
    )
)
# }}}

SHIFT_UI_DETAIL_LEVELS <- c("normal", "detail", "debug")

#' @rdname shift_api
#' @param progress In [shift_ui()], workflow presentation mode: `"auto"`,
#'   `"dynamic"`, `"log"`, or `"none"`.
#' @param detail Presentation detail level. `"normal"` shows task progress,
#'   `"detail"` adds selection, reuse, and fallback decisions, and `"debug"`
#'   also shows full URLs, paths, and low-level transfer context.
#' @param motion Dynamic-terminal animation policy: `"auto"`, `"full"`,
#'   `"reduced"`, or `"none"`. This never changes log, JSON, or workflow data.
#' @param refresh In [shift_control()], whether to refresh remote catalogs and
#'   service addresses instead of resuming persisted inputs. In [shift_ui()],
#'   minimum seconds between visual animation frames. In `ShiftRun` inspectors,
#'   whether to reload persisted state first.
#' @param heartbeat Minimum seconds between job-liveness updates. Durable
#'   writes are always throttled to at least one second.
#'   Use `progress = "log"` for screen readers, reduced-motion use, redirected
#'   output, and stable captured logs.
#' @export
# shift_ui {{{
shift_ui <- function(
    progress = c("auto", "dynamic", "log", "none"),
    detail = c("normal", "detail", "debug"),
    motion = c("auto", "full", "reduced", "none"),
    refresh = 0.12,
    heartbeat = 10
) {
    progress <- match.arg(progress)
    detail <- match.arg(detail)
    motion <- match.arg(motion)
    checkmate::assert_number(refresh, lower = 0.05, finite = TRUE)
    checkmate::assert_number(heartbeat, lower = 0, finite = TRUE)
    ShiftUiOptions(
        progress = progress,
        detail = detail,
        motion = motion,
        refresh = as.numeric(refresh),
        heartbeat = as.numeric(heartbeat)
    )
}
# }}}

# shift_ui__ui_at_least {{{
shift_ui__ui_at_least <- function(ui, level = c("normal", "detail", "debug")) {
    match(ui@detail, SHIFT_UI_DETAIL_LEVELS) >=
        match(match.arg(level), SHIFT_UI_DETAIL_LEVELS)
}
# }}}

# Pure scalar and domain formatters (zero ANSI) {{{

# Return a non-empty scalar character value or NULL.
# shift_ui_state__chr1 {{{
shift_ui_state__chr1 <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NULL)
    }
    val <- as.character(x[[1L]])
    if (nzchar(val)) val else NULL
}
# }}}

# Keep only non-missing, non-empty character elements.
# shift_ui_state__nonempty {{{
shift_ui_state__nonempty <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & nzchar(x)]
}
# }}}

# Extract common scientific fields from a ShiftPlan once for state & printers.
# shift_ui_state__plan_fields {{{
shift_ui_state__plan_fields <- function(plan) {
    meta <- plan@meta
    climate <- meta$climate
    req <- meta$request@meta
    has_c <- !is.null(climate)
    list(
        meta = meta,
        climate = climate,
        request = req,
        model = if (has_c) climate@model else req$source,
        scenarios = if (has_c) climate@scenarios else req$experiment,
        member = if (has_c) climate@member else req$variant,
        grid = if (has_c) climate@grid else req$filters$grid_label,
        table = if (has_c) climate@table else req$filters$table_id,
        frequency = if (has_c && !is.null(climate@frequency)) {
            climate@frequency
        } else {
            req$frequency
        },
        nodes = if (has_c) climate@index_nodes else req$options$index_node,
        output_dir = shift_stage__coalesce(
            meta$epw$export_dir,
            "<output directory>"
        ),
        transform = meta$transform,
        expected_cases = meta$expected_cases,
        control = meta$control
    )
}
# }}}

# Extract normalized failure counters and evidence fields for renderers.
# shift_ui_state__failure_fields {{{
shift_ui_state__failure_fields <- function(details) {
    details <- shift_stage__coalesce(details, list())
    num <- function(name) {
        v <- suppressWarnings(as.integer(details[[name]]))
        if (!length(v) || is.na(v[[1L]])) 0L else v[[1L]]
    }
    cl <- shift_stage__coalesce(details$closest, list())
    id <- unlist(list(cl$model, cl$member, cl$grid), use.names = FALSE)
    list(
        has_checked = !is.null(details$nodes_checked),
        checked = num("nodes_checked"),
        usable = num("usable_nodes"),
        coverage = num("coverage_failures"),
        timeout = num("timeout_failures"),
        network = num("network_failures"),
        other = num("other_failures"),
        identity = shift_ui_state__nonempty(id),
        missing = shift_ui_state__nonempty(shift_stage__coalesce(
            details$missing,
            character()
        ))
    )
}
# }}}

# Compact paths below the session temp directory lexically or via parent norm.
# shift_print__display_path {{{
shift_print__display_path <- function(path, temp_root = tempdir()) {
    if (is.null(path) || !nzchar(path)) {
        return(path)
    }
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_string(temp_root, min.chars = 1L)
    lex <- function(v) sub("/+$", "", gsub("\\\\", "/", path.expand(v)))
    norm <- function(p) lex(normalizePath(p, winslash = "/", mustWork = FALSE))
    compact <- function(cand, root) {
        win <- grepl("^[A-Za-z]:/", cand) || grepl("^[A-Za-z]:/", root)
        ck <- if (win) tolower(cand) else cand
        rk <- if (win) tolower(root) else root
        if (identical(ck, rk) || startsWith(ck, paste0(rk, "/"))) {
            paste0("<tempdir>", substring(cand, nchar(root) + 1L))
        }
    }
    exp_p <- lex(path)
    if (!is.null(hit <- compact(exp_p, lex(temp_root)))) {
        return(hit)
    }
    norm_p <- norm(path)
    norm_t <- norm(temp_root)
    if (!is.null(hit <- compact(norm_p, norm_t))) {
        return(hit)
    }
    rebuilt <- paste0(norm(dirname(exp_p)), "/", basename(exp_p))
    shift_stage__coalesce(compact(rebuilt, norm_t), norm_p)
}
# }}}

# Collapse a vector into a bounded comma-separated summary.
# shift_print__display_values {{{
shift_print__display_values <- function(x, max = 7L) {
    x <- shift_ui_state__nonempty(x)
    if (!length(x)) {
        return(NULL)
    }
    if (length(x) > max) {
        sprintf(
            "%s, ... (%d total)",
            paste(utils::head(x, max), collapse = ", "),
            length(x)
        )
    } else {
        paste(x, collapse = ", ")
    }
}
# }}}

# Compress integer years into consecutive ranges (e.g. "2041–2060, 2071–2090").
# shift_print__format_years {{{
shift_print__format_years <- function(years) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    if (!length(years)) {
        return(NULL)
    }
    ranges <- split(years, cumsum(c(TRUE, diff(years) != 1L)))
    parts <- vapply(
        ranges,
        function(v) {
            if (length(v) == 1L) {
                as.character(v)
            } else {
                sprintf("%d\u2013%d", v[[1L]], v[[length(v)]])
            }
        },
        character(1L)
    )
    paste(parts, collapse = ", ")
}
# }}}

# Format period tables, named lists, or named vectors through one helper.
# shift_print__format_period_map {{{
shift_print__format_period_map <- function(
    periods,
    paren = FALSE,
    collapse = " \u00b7 ",
    empty = NULL,
    compact_ranges = TRUE
) {
    if (is.null(periods) || !length(periods)) {
        return(empty)
    }
    if (is.data.frame(periods)) {
        periods <- data.table::as.data.table(periods)
        if (!nrow(periods) || !all(c("period", "year") %in% names(periods))) {
            return(empty)
        }
        lbls <- unique(as.character(periods$period))
        periods <- stats::setNames(
            lapply(lbls, function(l) periods[period == l, year]),
            lbls
        )
    } else if (is.atomic(periods) && !is.null(names(periods))) {
        periods <- split(as.integer(periods), names(periods))
    } else if (is.list(periods) && is.null(names(periods))) {
        if (!isTRUE(paren)) {
            return(shift_print__format_years(unlist(
                periods,
                use.names = FALSE
            )))
        }
        names(periods) <- rep("period", length(periods))
    }
    labels <- names(periods)
    if (!length(labels)) {
        labels <- rep("period", length(periods))
    }
    fmt <- if (isTRUE(paren)) "%s (%s)" else "%s %s"
    parts <- vapply(
        seq_along(periods),
        function(i) {
            yrs <- suppressWarnings(as.integer(periods[[i]]))
            yrs <- yrs[!is.na(yrs)]
            if (!length(yrs)) {
                return(labels[[i]])
            }
            yr_str <- if (isTRUE(compact_ranges)) {
                shift_print__format_years(yrs)
            } else if (length(unique(yrs)) == 1L) {
                as.character(yrs[[1L]])
            } else {
                sprintf("%d\u2013%d", min(yrs), max(yrs))
            }
            sprintf(fmt, labels[[i]], yr_str)
        },
        character(1L)
    )
    paste(parts, collapse = collapse)
}
# }}}

# shift_print__format_periods {{{
shift_print__format_periods <- function(periods) {
    shift_print__format_period_map(periods, FALSE, " \u00b7 ", NULL, TRUE)
}
# }}}

# shift_ui_view__ui_periods {{{
shift_ui_view__ui_periods <- function(periods) {
    shift_print__format_period_map(
        data.table::as.data.table(periods),
        TRUE,
        ", ",
        "no periods",
        FALSE
    )
}
# }}}

# shift_ui_view__reanalysis_label {{{
shift_ui_view__reanalysis_label <- function(
    dataset,
    product,
    years,
    access = NULL,
    sep = " "
) {
    yrs <- as.integer(unlist(years, use.names = FALSE))
    base <- paste(
        toupper(dataset),
        product,
        sprintf("%d\u2013%d", min(yrs), max(yrs)),
        sep = sep
    )
    if (is.null(access)) base else sprintf("%s (%s)", base, access)
}
# }}}

# shift_print__format_reference {{{
shift_print__format_reference <- function(reference, recipe = NULL) {
    if (is.null(reference)) {
        accepts <- !is.null(recipe) &&
            isTRUE(morpher__recipe_accepts_reference(recipe))
        return(if (accepts) "baseline EPW" else "none")
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        parts <- c(
            reference@role,
            reference@mode,
            shift_print__format_periods(reference@periods)
        )
        return(paste(shift_ui_state__nonempty(parts), collapse = " \u00b7 "))
    }
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        return(shift_ui_view__reanalysis_label(
            reference@dataset,
            reference@product,
            reference@years,
            sep = " \u00b7 "
        ))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied ShiftClimate")
    }
    class(reference)[[1L]]
}
# }}}

# shift_ui_view__ui_reference {{{
shift_ui_view__ui_reference <- function(reference) {
    if (is.null(reference)) {
        return("no reference")
    }
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        return(shift_ui_view__reanalysis_label(
            reference@dataset,
            reference@product,
            reference@years,
            access = reference@access
        ))
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        tbl <- data.table::as.data.table(reference@periods)
        periods <- if (length(unique(tbl$period)) == 1L) {
            yrs <- tbl$year
            if (length(unique(yrs)) == 1L) {
                as.character(yrs[[1L]])
            } else {
                sprintf("%d\u2013%d", min(yrs), max(yrs))
            }
        } else {
            shift_ui_view__ui_periods(tbl)
        }
        return(sprintf("%s %s %s", reference@role, reference@mode, periods))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied climate reference")
    }
    "reference supplied"
}
# }}}

# shift_print__format_auto {{{
shift_print__format_auto <- function(x) {
    shift_stage__coalesce(shift_print__display_values(x), "auto")
}
# }}}

# shift_print__format_morph_method {{{
shift_print__format_morph_method <- function(
    name,
    recipe = NULL,
    missing_original_morphing_profile = NULL
) {
    name <- as.character(shift_stage__coalesce(name, "method"))[[1L]]
    backend <- as.character(shift_stage__coalesce(recipe$backend, name))[[1L]]
    profile <- shift_ui_state__chr1(recipe$profile)
    if (
        is.null(profile) &&
            backend %in% c("original_morphing", "original_morphing_absolute")
    ) {
        profile <- shift_ui_state__chr1(missing_original_morphing_profile)
    }
    if (is.null(profile)) name else sprintf("%s [%s]", name, profile)
}
# }}}

# shift_print__normalize_named_vec {{{
shift_print__normalize_named_vec <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
        x <- unlist(x, use.names = TRUE)
    }
    nms <- names(x)
    x <- stats::setNames(as.character(x), nms)
    list(values = x, named = !is.null(nms) && any(nzchar(nms)))
}
# }}}

# shift_print__format_cmip6_tables {{{
shift_print__format_cmip6_tables <- function(table) {
    if (is.null(table) || !length(table)) {
        return("auto by variable")
    }
    spec <- shift_print__normalize_named_vec(table)
    if (!spec$named) {
        return(sprintf(
            "%s (forced)",
            shift_print__display_values(spec$values, max = Inf)
        ))
    }
    ov <- paste(
        sprintf("%s=%s", names(spec$values), spec$values),
        collapse = " \u00b7 "
    )
    sprintf("auto by variable \u00b7 %s", ov)
}
# }}}

# shift_print__format_cmip6_frequencies {{{
shift_print__format_cmip6_frequencies <- function(frequency) {
    if (is.null(frequency) || !length(frequency)) {
        return(NULL)
    }
    spec <- shift_print__normalize_named_vec(frequency)
    if (!spec$named) {
        return(shift_print__display_values(spec$values, max = Inf))
    }
    paste(sprintf("%s=%s", names(spec$values), spec$values), collapse = " | ")
}
# }}}

# shift_print__format_cmip6_partitions {{{
shift_print__format_cmip6_partitions <- function(selection) {
    sel <- data.table::as.data.table(shift_stage__coalesce(
        selection,
        data.table::data.table()
    ))
    if (!nrow(sel)) {
        return(NULL)
    }
    keys <- if ("partition_key" %in% names(sel)) {
        unique(shift_ui_state__nonempty(sel$partition_key))
    } else {
        character()
    }
    if (!length(keys) && all(c("table_id", "grid_label") %in% names(sel))) {
        rows <- unique(sel[, .(table_id, grid_label)])
        rows <- rows[
            !is.na(table_id) &
                nzchar(table_id) &
                !is.na(grid_label) &
                nzchar(grid_label)
        ]
        if (nrow(rows)) {
            data.table::setorderv(rows, c("table_id", "grid_label"))
            keys <- paste(
                paste0(rows$table_id, "=", rows$grid_label),
                collapse = ";"
            )
        }
    }
    if (!length(keys)) {
        return(NULL)
    }
    paste(gsub(";", " \u00b7 ", keys, fixed = TRUE), collapse = " / ")
}
# }}}

# shift_print__format_options {{{
shift_print__format_options <- function(x) {
    if (is.null(x) || !length(x)) {
        return(NULL)
    }
    vals <- vapply(
        names(x),
        function(nm) {
            v <- x[[nm]]
            if (is.atomic(v)) {
                disp <- shift_stage__coalesce(
                    shift_print__display_values(v),
                    "<empty>"
                )
                sprintf("%s=%s", nm, disp)
            } else {
                sprintf("%s=<%s>", nm, class(v)[[1L]])
            }
        },
        character(1L)
    )
    paste(vals, collapse = " \u00b7 ")
}
# }}}

# shift_ui_view__format_selection {{{
shift_ui_view__format_selection <- function(
    member_value,
    grid_value,
    table_value
) {
    fmt <- function(k, v) {
        if (is.null(v)) {
            paste(k, "auto")
        } else {
            paste(k, paste(v, collapse = ", "))
        }
    }
    paste(
        fmt("member", member_value),
        fmt("grid", grid_value),
        paste("tables", shift_print__format_cmip6_tables(table_value)),
        sep = " \u00b7 "
    )
}
# }}}

# shift_ui_view__ui_selection {{{
shift_ui_view__ui_selection <- function(plan) {
    f <- shift_ui_state__plan_fields(plan)
    shift_ui_view__format_selection(f$member, f$grid, f$table)
}
# }}}

SHIFT_UI_STAGE_LABELS <- c(
    planned = "Plan",
    datasets = "Datasets",
    collect = "Collect",
    resolve = "Resolve",
    download = "Download",
    extract = "Extract",
    extract_future = "Extract future",
    extract_reference = "Extract reference",
    coverage = "Coverage",
    morph = "Morph",
    write_epw = "Write EPW",
    export_epw = "Export EPW",
    completed = "Completed",
    resume = "Resume",
    reanalysis = "Reanalysis",
    extract_observed_reference = "Observed reference",
    batch = "Batch"
)

# shift_ui_view__ui_stage_label {{{
shift_ui_view__ui_stage_label <- function(stage) {
    key <- as.character(shift_stage__coalesce(stage, "planned"))[[1L]]
    if (key %in% names(SHIFT_UI_STAGE_LABELS)) {
        unname(SHIFT_UI_STAGE_LABELS[[key]])
    } else {
        gsub("_", " ", key, fixed = TRUE)
    }
}
# }}}

# shift_ui_view__ui_run_short {{{
shift_ui_view__ui_run_short <- function(run_id) {
    id <- sub(
        "^run_",
        "",
        as.character(shift_stage__coalesce(run_id, ""))[[1L]]
    )
    if (!nzchar(id) || nchar(id) <= 8L) {
        id
    } else {
        substr(id, nchar(id) - 7L, nchar(id))
    }
}
# }}}

# shift_ui_view__ui_metric_number {{{
shift_ui_view__ui_metric_number <- function(details, name, default = NA_real_) {
    val <- details[[name]]
    if (is.null(val) || !length(val)) {
        return(default)
    }
    val <- suppressWarnings(as.numeric(val[[1L]]))
    if (!length(val) || is.na(val) || !is.finite(val)) default else val
}
# }}}

# shift_ui_view__format_elapsed {{{
shift_ui_view__format_elapsed <- function(seconds) {
    s <- max(0, round(as.numeric(seconds)))
    h <- s %/% 3600L
    m <- (s %% 3600L) %/% 60L
    sec <- s %% 60L
    if (h > 0L) {
        return(sprintf("%dh %02dm %02ds", h, m, sec))
    }
    if (m > 0L) {
        return(sprintf("%dm %02ds", m, sec))
    }
    sprintf("%ds", sec)
}
# }}}

# shift_ui_view__ui_eta {{{
shift_ui_view__ui_eta <- function(seconds) {
    if (
        is.null(seconds) ||
            !length(seconds) ||
            is.na(seconds) ||
            !is.finite(seconds)
    ) {
        return(NULL)
    }
    paste("ETA", shift_ui_view__format_elapsed(seconds))
}
# }}}

# shift_ui_view__format_bytes {{{
shift_ui_view__format_bytes <- function(x, binary = TRUE, na = "?") {
    if (is.null(x) || !length(x)) {
        return(na)
    }
    bytes <- suppressWarnings(as.numeric(x))
    out <- rep(na, length(bytes))
    ok <- !is.na(bytes) & is.finite(bytes)
    if (!any(ok)) {
        return(out)
    }
    units <- if (isTRUE(binary)) {
        c("B", "KiB", "MiB", "GiB", "TiB")
    } else {
        c("B", "KB", "MB", "GB", "TB", "PB")
    }
    for (i in which(ok)) {
        v <- bytes[[i]]
        u <- 1L
        if (!isTRUE(binary) || v > 0) {
            while (abs(v) >= 1024 && u < length(units)) {
                v <- v / 1024
                u <- u + 1L
            }
        }
        out[[i]] <- sprintf(
            if (u == 1L) "%.0f %s" else "%.1f %s",
            v,
            units[[u]]
        )
    }
    out
}
# }}}

# shift_ui_view__ui_bytes {{{
shift_ui_view__ui_bytes <- function(bytes) {
    shift_ui_view__format_bytes(if (length(bytes)) bytes[[1L]], TRUE, "?")
}
# }}}

# shift_ui_view__node_label {{{
shift_ui_view__node_label <- function(node) {
    node <- as.character(shift_stage__coalesce(node, "unknown"))[[1L]]
    norm <- function(v) {
        tryCatch(query__normalize_node(v), error = function(e) v)
    }
    normalized <- norm(node)
    known <- vapply(
        INDEX_NODES,
        function(v) identical(norm(v), normalized),
        logical(1L)
    )
    if (any(known)) {
        return(names(INDEX_NODES)[which(known)[[1L]]])
    }
    parsed <- tryCatch(curl::curl_parse_url(normalized), error = function(e) {
        NULL
    })
    shift_stage__coalesce(shift_ui_state__chr1(parsed$host), normalized)
}
# }}}

# shift_ui_view__ui_error_kind {{{
shift_ui_view__ui_error_kind <- function(message) {
    msg <- tolower(as.character(shift_stage__coalesce(message, ""))[[1L]])
    if (grepl("timed? out|timeout|operation too slow", msg)) {
        return("timeout")
    }
    if (
        grepl("http|status code|could not resolve|connect|ssl|certificate", msg)
    ) {
        return("network")
    }
    if (
        grepl(
            "missing|coverage|complete|required variable|year|empty|no .*files?",
            msg
        )
    ) {
        return("coverage")
    }
    if (grepl("ambiguous|multiple|more than one", msg)) {
        return("ambiguity")
    }
    "error"
}
# }}}

# shift_print__plan_explain {{{
shift_print__plan_explain <- function(x) {
    f <- shift_ui_state__plan_fields(x)
    ref <- f$meta$reference
    ref_detail <- "none"
    if (S7::S7_inherits(ref, ShiftReferenceSpec)) {
        ps <- unique(ref@periods$period)
        rp <- vapply(
            ps,
            function(p) {
                yrs <- ref@periods$year[ref@periods$period == p]
                sprintf("%s=%s:%s", p, min(yrs), max(yrs))
            },
            character(1L)
        )
        match_str <- if (length(ref@match)) {
            sprintf("; match: %s", paste(ref@match, collapse = ", "))
        } else {
            ""
        }
        ref_detail <- sprintf(
            "%s %s; periods: %s%s",
            ref@role,
            ref@mode,
            paste(rp, collapse = ", "),
            match_str
        )
    } else if (S7::S7_inherits(ref, ShiftClimate)) {
        ref_detail <- "supplied ShiftClimate"
    }
    disp <- function(v, empty) {
        shift_stage__coalesce(shift_print__display_values(v), empty)
    }
    data.table::data.table(
        step = c(
            "request",
            "transform",
            "reference",
            "observed_reference",
            "cases",
            "selection",
            "index_nodes",
            "partial",
            "store",
            "output"
        ),
        detail = c(
            sprintf(
                "%s %s %s",
                shift_stage__coalesce(f$request$project, "CMIP"),
                disp(f$request$source, "<any source>"),
                disp(f$request$experiment, "<any experiment>")
            ),
            f$transform@label,
            ref_detail,
            shift_print__format_reference(f$meta$observed_reference),
            sprintf("%d expected EPW output(s)", nrow(f$expected_cases)),
            sprintf(
                "member=%s; grid=%s",
                disp(f$member, "<auto>"),
                disp(f$grid, "<auto>")
            ),
            disp(f$nodes, "<provider default>"),
            if (isTRUE(f$control@allow_partial)) {
                "allow partial outputs"
            } else {
                "all requested cases required"
            },
            shift_print__display_path(x@store_path),
            shift_print__display_path(shift_stage__coalesce(
                f$meta$epw$export_dir,
                f$meta$epw$dir
            ))
        )
    )
}
# }}}

# shift_print__run_command {{{
shift_print__run_command <- function(name, run_id, store_path, extra = NULL) {
    default_store <- store_normalize_path(store_dir(init = FALSE))
    actual_store <- store_normalize_path(store_path)
    args <- c(
        encodeString(run_id, quote = '"'),
        if (!identical(actual_store, default_store)) {
            sprintf("store = %s", encodeString(actual_store, quote = '"'))
        },
        extra
    )
    sprintf("%s(%s)", name, paste(args, collapse = ", "))
}
# }}}

# shift_print__resolution_evidence {{{
shift_print__resolution_evidence <- function(diagnostic) {
    if (is.null(diagnostic) || !length(diagnostic)) {
        return(character())
    }
    f <- shift_ui_state__failure_fields(diagnostic)
    counts <- c(
        if (f$coverage > 0L) sprintf("%d incomplete", f$coverage),
        if (f$timeout > 0L) sprintf("%d timed out", f$timeout),
        if (f$network > 0L) sprintf("%d network errors", f$network),
        if (f$other > 0L) sprintf("%d other errors", f$other)
    )
    detail <- if (length(counts)) {
        paste0(": ", paste(counts, collapse = ", "))
    } else {
        ""
    }
    c(
        if (f$has_checked) {
            sprintf(
                "%d node%s checked%s.",
                f$checked,
                if (f$checked == 1L) "" else "s",
                detail
            )
        },
        if (length(f$identity)) {
            sprintf("Closest identity: %s.", paste(f$identity, collapse = "/"))
        },
        if (length(f$missing)) {
            sprintf("First missing requirement: %s.", f$missing[[1L]])
        }
    )
}
# }}}

# shift_print__failure_context {{{
shift_print__failure_context <- function(details, debug = FALSE) {
    if (is.null(details) || !length(details)) {
        return("")
    }
    fields <- c(
        node = "node",
        scenario = "scenario",
        variable = "variable",
        period = "period",
        access_method = "access",
        unit_label = "unit"
    )
    vals <- vapply(
        names(fields),
        function(nm) {
            shown <- shift_ui_state__chr1(details[[nm]])
            if (is.null(shown)) {
                return(NA_character_)
            }
            if (identical(nm, "node") && !isTRUE(debug)) {
                shown <- shift_ui_view__node_label(shown)
            }
            sprintf("%s=%s", fields[[nm]], shown)
        },
        character(1L)
    )
    vals <- unique(vals[!is.na(vals)])
    if (!length(vals)) {
        ""
    } else {
        paste0("Last activity: ", paste(vals, collapse = ", "), ".")
    }
}
# }}}

# }}}

# Pure ViewModel, Snapshot, and Event-Delta Builders (zero ANSI) {{{

# shift_ui_state__ui_stage_sequence {{{
shift_ui_state__ui_stage_sequence <- function(plan) {
    ref <- plan@meta$reference
    has_hist <- S7::S7_inherits(ref, ShiftReferenceSpec) &&
        identical(ref@mode, "historical")
    c(
        "resolve",
        if (identical(plan@meta$control@download, "always")) "download",
        "extract_future",
        if (isTRUE(has_hist)) "extract_reference",
        "coverage",
        "morph",
        "write_epw"
    )
}
# }}}

# shift_ui_state__ui_plan_context {{{
shift_ui_state__ui_plan_context <- function(plan) {
    f <- shift_ui_state__plan_fields(plan)
    model <- paste(
        as.character(shift_stage__coalesce(f$request$source, "<model>")),
        collapse = ", "
    )
    scenarios <- paste(
        as.character(shift_stage__coalesce(f$request$experiment, "<scenario>")),
        collapse = " + "
    )
    n_exp <- nrow(f$expected_cases)
    items <- c(
        model,
        scenarios,
        shift_ui_view__ui_periods(f$meta$periods),
        sprintf(
            "%s / %s",
            f$transform@label,
            shift_ui_view__ui_reference(f$meta$reference)
        ),
        if (identical(f$transform@output_type, "multi_year")) {
            sprintf("%d cases; one EPW per weather year", n_exp)
        } else {
            sprintf("%d EPW%s", n_exp, if (n_exp == 1L) "" else "s")
        },
        if (!is.null(f$meta$observed_reference)) {
            paste(
                "Observed",
                shift_ui_view__ui_reference(f$meta$observed_reference)
            )
        }
    )
    list(
        line = paste(items, collapse = " \u00b7 "),
        items = items,
        selection = shift_ui_view__ui_selection(plan),
        output = f$output_dir
    )
}
# }}}

# shift_ui_state__ui_progress_values {{{
shift_ui_state__ui_progress_values <- function(state) {
    details <- shift_stage__coalesce(state$current_details, list())
    cur <- shift_ui_view__ui_metric_number(
        details,
        "current",
        shift_stage__coalesce(state$unit_current, NA_real_)
    )
    tot <- shift_ui_view__ui_metric_number(
        details,
        "total",
        shift_stage__coalesce(state$unit_total, NA_real_)
    )
    if (identical(state$stage, "coverage")) {
        cur <- as.numeric(shift_stage__coalesce(state$cases_ready, 0L))
        tot <- as.numeric(shift_stage__coalesce(state$cases_total, 0L))
    } else if (
        !identical(state$stage, "resolve") &&
            identical(details$phase, "unit") &&
            is.null(details$outcome) &&
            !is.na(cur)
    ) {
        cur <- max(0L, cur - 1L)
    }
    list(current = cur, total = tot)
}
# }}}

# shift_ui_state__ui_event_details {{{
shift_ui_state__ui_event_details <- function(events) {
    if (!nrow(events)) {
        return(list())
    }
    if (!"details_json" %in% names(events)) {
        return(rep(list(list()), nrow(events)))
    }
    lapply(events$details_json, function(v) {
        raw <- shift_ui_state__chr1(v)
        if (is.null(raw)) {
            return(list())
        }
        tryCatch(
            jsonlite::fromJSON(raw, simplifyVector = TRUE),
            error = function(e) list()
        )
    })
}
# }}}

# shift_ui_state__parse_row_spec {{{
shift_ui_state__parse_row_spec <- function(row) {
    row <- data.table::as.data.table(row)
    raw <- if (nrow(row) && "spec_json" %in% names(row)) {
        shift_ui_state__chr1(row$spec_json[[1L]])
    }
    if (is.null(raw)) {
        return(NULL)
    }
    tryCatch(
        jsonlite::fromJSON(raw, simplifyVector = TRUE),
        error = function(e) NULL
    )
}
# }}}

# shift_ui_state__ui_stage_sequence_from_row {{{
shift_ui_state__ui_stage_sequence_from_row <- function(row) {
    row <- data.table::as.data.table(row)
    spec <- shift_ui_state__parse_row_spec(row)
    if (is.null(spec)) {
        return(character())
    }
    task <- as.character(shift_stage__coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        return(as.character(shift_stage__coalesce(
            row$current_stage[[1L]],
            task
        )))
    }
    is_hist <- function(x) {
        !is.null(x) &&
            identical(
                as.character(shift_stage__coalesce(x$mode, "none"))[[1L]],
                "historical"
            )
    }
    dl <- as.character(shift_stage__coalesce(spec$control$download, "auto"))[[
        1L
    ]]
    c(
        "resolve",
        if (identical(dl, "always")) "download",
        "extract_future",
        if (is_hist(spec$reference)) "extract_reference",
        if (is_hist(spec$observed_reference)) "extract_observed_reference",
        "coverage",
        "morph",
        "write_epw"
    )
}
# }}}

# shift_ui_state__ui_periods_from_spec {{{
shift_ui_state__ui_periods_from_spec <- function(periods) {
    shift_print__format_period_map(periods, TRUE, ", ", "no periods", FALSE)
}
# }}}

# shift_ui_state__ui_reference_from_spec {{{
shift_ui_state__ui_reference_from_spec <- function(reference) {
    mode <- as.character(shift_stage__coalesce(reference$mode, "none"))[[1L]]
    if (identical(mode, "none")) {
        return("no reference")
    }
    if (identical(mode, "reanalysis")) {
        return(shift_ui_view__reanalysis_label(
            reference$dataset,
            reference$product,
            reference$years,
            access = reference$access
        ))
    }
    if (identical(mode, "historical")) {
        p <- shift_ui_state__ui_periods_from_spec(reference$periods)
        return(paste("historical", sub("\\)$", "", sub("^[^(]+ \\(", "", p))))
    }
    "supplied reference"
}
# }}}

# shift_ui_state__ui_plan_context_from_row {{{
shift_ui_state__ui_plan_context_from_row <- function(row, cases_total = 0L) {
    row <- data.table::as.data.table(row)
    spec <- shift_ui_state__parse_row_spec(row)
    if (is.null(spec)) {
        return(list())
    }
    output <- if ("output_dir" %in% names(row)) row$output_dir[[1L]] else NULL
    task <- as.character(shift_stage__coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        label <- shift_run__task_label(as.character(shift_stage__coalesce(
            row$current_stage[[1L]],
            task
        )))
        store_str <- shift_print__display_path(shift_stage__coalesce(
            spec$store,
            "<store>"
        ))
        return(list(
            title = label,
            line = label,
            items = c(label, paste("store", store_str)),
            selection = NULL,
            output = output
        ))
    }
    clim <- shift_stage__coalesce(spec$climate, spec$request)
    model <- paste(
        as.character(shift_stage__coalesce(clim$model, clim$source)),
        collapse = ", "
    )
    scens <- paste(
        as.character(shift_stage__coalesce(clim$scenarios, clim$experiment)),
        collapse = " + "
    )
    transform <- shift_stage__coalesce(spec$transform$method, "transform")
    ref <- shift_ui_state__ui_reference_from_spec(spec$reference)
    n_exp <- as.integer(cases_total)
    obs <- spec$observed_reference
    line <- c(
        if (nzchar(model)) model,
        if (nzchar(scens)) scens,
        shift_ui_state__ui_periods_from_spec(spec$periods),
        sprintf("%s / %s", transform, ref),
        if (n_exp > 0L) sprintf("%d cases", n_exp),
        if (!is.null(obs) && !identical(obs$mode, "none")) {
            paste("Observed", shift_ui_state__ui_reference_from_spec(obs))
        }
    )
    tbl <- if (!is.null(spec$climate)) {
        spec$climate$table
    } else {
        spec$request$filters$table_id
    }
    list(
        line = paste(line, collapse = " \u00b7 "),
        items = line,
        selection = shift_ui_view__format_selection(
            shift_stage__coalesce(spec$climate$member, spec$request$variant),
            shift_stage__coalesce(
                spec$climate$grid,
                spec$request$filters$grid_label
            ),
            tbl
        ),
        output = output
    )
}
# }}}

# Build one normalized resolver-attempt row shared by watch state and reporter.
# shift_ui_state__node_row {{{
shift_ui_state__node_row <- function(
    details,
    outcome,
    message,
    elapsed_default = NULL
) {
    outcome <- as.character(shift_stage__coalesce(outcome, "rejected"))[[1L]]
    secs <- shift_stage__coalesce(details$elapsed_seconds, elapsed_default)
    duration <- if (is.null(secs)) {
        "\u2014"
    } else {
        shift_ui_view__format_elapsed(secs)
    }
    result <- if (outcome %in% c("completed", "skipped")) {
        shift_stage__coalesce(details$result, "selected")
    } else {
        err <- shift_stage__coalesce(details$error, message)
        kind <- shift_stage__coalesce(
            details$error_kind,
            shift_ui_view__ui_error_kind(err)
        )
        sprintf("%s: %s", kind, err)
    }
    data.table::data.table(
        node = shift_ui_view__node_label(details$node),
        future = shift_stage__coalesce(details$future_files, NA_integer_),
        reference = shift_stage__coalesce(details$reference_files, NA_integer_),
        outcome = outcome,
        duration = duration,
        result = result
    )
}
# }}}

# shift_ui_state__ui_event_nodes {{{
shift_ui_state__ui_event_nodes <- function(events) {
    events <- data.table::as.data.table(events)
    details <- shift_ui_state__ui_event_details(events)
    term <- c("completed", "skipped", "rejected", "failed")
    rows <- lapply(seq_along(details), function(i) {
        v <- details[[i]]
        if (
            !identical(v$unit_type, "index_node") ||
                !events$status[[i]] %in% term
        ) {
            return(NULL)
        }
        shift_ui_state__node_row(v, events$status[[i]], events$message[[i]])
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
# }}}

# Reconstruct semantic live state from persisted run, event, and case tables.
# shift_ui_state__ui_table_state {{{
shift_ui_state__ui_table_state <- function(row, events, cases) {
    row <- data.table::as.data.table(row)
    events <- data.table::as.data.table(events)
    cases <- data.table::as.data.table(cases)
    details <- shift_ui_state__ui_event_details(events)
    stage <- row$current_stage[[1L]]
    status <- row$status[[1L]]
    last_of <- function(idx) {
        if (length(idx)) utils::tail(idx, 1L) else NA_integer_
    }
    pick <- function(idx, vec, default) if (is.na(idx)) default else vec[[idx]]

    stage_idx <- last_of(which(vapply(
        details,
        function(x) {
            isTRUE(x$phase %in% c("stage", "operation")) &&
                identical(x$stage, stage)
        },
        logical(1L)
    )))
    unit_idx <- last_of(which(vapply(
        details,
        function(x) identical(x$phase, "unit") && identical(x$stage, stage),
        logical(1L)
    )))
    milestones <- which(
        events$status %in%
            c(
                "completed",
                "skipped",
                "rejected",
                "fallback",
                "failed",
                "cancelled",
                "partial"
            )
    )
    last_idx <- last_of(milestones)
    fail_idx <- last_of(which(events$status %in% c("failed", "cancelled")))
    recent_idx <- utils::tail(milestones, 3L)

    done_stages <- unique(vapply(
        seq_along(details),
        function(i) {
            d <- details[[i]]
            ok <- (identical(d$phase, "stage") &&
                identical(events$status[[i]], "completed")) ||
                (identical(d$phase, "operation") &&
                    isTRUE(d$outcome %in% c("completed", "partial")))
            if (ok) as.character(d$stage) else NA_character_
        },
        character(1L)
    ))
    done_stages <- done_stages[!is.na(done_stages)]

    started <- shift_stage__coalesce(
        row$started_at[[1L]],
        as.POSIXct(NA, tz = "UTC")
    )
    stopped <- shift_stage__coalesce(
        row$completed_at[[1L]],
        as.POSIXct(NA, tz = "UTC")
    )
    if (is.na(stopped)) {
        is_term <- status %in%
            c("waiting", "completed", "partial", "failed", "cancelled")
        stopped <- if (
            is_term &&
                "updated_at" %in% names(row) &&
                !is.na(row$updated_at[[1L]])
        ) {
            row$updated_at[[1L]]
        } else if (
            is_term &&
                nrow(events) &&
                "created_at" %in% names(events) &&
                !is.na(events$created_at[[nrow(events)]])
        ) {
            events$created_at[[nrow(events)]]
        } else {
            Sys.time()
        }
    }
    elapsed <- if (is.na(started)) {
        0
    } else {
        as.numeric(difftime(stopped, started, units = "secs"))
    }

    stage_d <- pick(stage_idx, details, list())
    unit_d <- pick(unit_idx, details, list())
    seq_stages <- as.character(shift_stage__coalesce(
        stage_d$stage_sequence,
        character()
    ))
    if (!length(seq_stages)) {
        seq_stages <- shift_ui_state__ui_stage_sequence_from_row(row)
    }

    fallback_msg <- switch(
        status,
        queued = "Waiting for background worker",
        waiting = "Ready for the next shift stage",
        completed = "Workflow completed",
        partial = "Workflow completed with missing cases",
        failed = "Workflow failed",
        cancelled = "Workflow cancelled",
        stopping = "Waiting for cancellation boundary",
        "Waiting for next workflow event"
    )
    plan_ctx <- shift_ui_state__ui_plan_context_from_row(row, nrow(cases))
    out_paths <- if ("export_path" %in% names(cases)) {
        as.character(cases$export_path)
    } else {
        character()
    }
    exported_ev <- vapply(
        details,
        function(v) {
            if (
                identical(v$unit_type, "epw_export") &&
                    isTRUE(v$outcome %in% c("completed", "skipped"))
            ) {
                as.integer(shift_stage__coalesce(v$current, 0L))
            } else {
                0L
            }
        },
        integer(1L)
    )
    exported <- max(c(
        0L,
        exported_ev,
        length(unique(shift_ui_state__nonempty(out_paths)))
    ))

    list(
        run_id = row$run_id[[1L]],
        task_label = shift_stage__coalesce(plan_ctx$title, "Future EPW"),
        status = status,
        stage = stage,
        stage_message = pick(stage_idx, events$message, fallback_msg),
        stage_current = stage_d$current,
        stage_total = stage_d$total,
        unit_label = pick(unit_idx, events$message, NULL),
        unit_current = unit_d$current,
        unit_total = unit_d$total,
        current_details = unit_d,
        next_stage = stage_d$next_stage,
        stage_sequence = seq_stages,
        completed_stages = done_stages,
        plan_context = plan_ctx,
        cases_ready = sum(
            cases$status %in% c("ready", "morphing", "morphed", "completed")
        ),
        cases_total = if (nrow(cases)) nrow(cases) else 0L,
        outputs_completed = exported,
        output_dir = plan_ctx$output,
        output_paths = out_paths,
        output_path_limit = 5L,
        result_summary = if (status %in% c("waiting", "completed", "partial")) {
            pick(last_idx, events$message, NULL)
        },
        last_event = pick(last_idx, events$message, "No completed event yet"),
        recent_events = if (length(recent_idx)) {
            as.character(events$message[recent_idx])
        } else {
            character()
        },
        recent_outcomes = if (length(recent_idx)) {
            as.character(events$status[recent_idx])
        } else {
            character()
        },
        node_rows = shift_ui_state__ui_event_nodes(events),
        failure_details = pick(fail_idx, details, list()),
        elapsed_seconds = elapsed
    )
}
# }}}

# shift_ui_state__ui_live_state {{{
shift_ui_state__ui_live_state <- function(row, state, ui_state = NULL) {
    if (
        !length(ui_state) ||
            !nrow(row) ||
            !row$status[[1L]] %in% c("queued", "running", "stopping")
    ) {
        return(state)
    }
    state[names(ui_state)] <- ui_state
    state$status <- row$status[[1L]]
    if (length(row$started_at) && !is.na(row$started_at[[1L]])) {
        state$elapsed_seconds <- max(
            0,
            as.numeric(difftime(
                shift_job__watch_now(),
                row$started_at[[1L]],
                units = "secs"
            ))
        )
    }
    state
}
# }}}

# Tail helper shared by single-run and batch event delta selectors.
# shift_ui_state__tail_events {{{
shift_ui_state__tail_events <- function(events, limit) {
    if (limit == 0L) events[0] else utils::tail(events, limit)
}
# }}}

# shift_ui_state__ui_event_delta {{{
shift_ui_state__ui_event_delta <- function(
    events,
    last_event_id = NA_character_,
    initial_limit = 10L,
    initial = is.na(last_event_id)
) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    newest <- if (nrow(events)) {
        as.character(events$event_id[[nrow(events)]])
    } else {
        NA_character_
    }
    if (isTRUE(initial)) {
        return(list(
            rows = shift_ui_state__tail_events(events, initial_limit),
            cursor = newest,
            gap = FALSE
        ))
    }
    if (!nrow(events)) {
        return(list(rows = events, cursor = last_event_id, gap = FALSE))
    }
    if (is.na(last_event_id) || !nzchar(last_event_id)) {
        return(list(rows = events, cursor = newest, gap = FALSE))
    }
    pos <- match(last_event_id, events$event_id)
    if (is.na(pos)) {
        return(list(rows = events, cursor = newest, gap = TRUE))
    }
    rows <- if (pos < nrow(events)) {
        events[seq.int(pos + 1L, nrow(events))]
    } else {
        events[0]
    }
    list(rows = rows, cursor = newest, gap = FALSE)
}
# }}}

# Read each batch child once into a pure structured snapshot (zero ANSI).
# shift_batch_ui__snapshot {{{
shift_batch_ui__snapshot <- function(x, event_count = 10L, refresh = TRUE) {
    if (isTRUE(refresh)) {
        x <- shift_batch__refresh(x)
    }
    children <- data.table::copy(x@meta$manifest)
    ch_list <- x@meta$children
    chr_map <- function(fn) vapply(ch_list, fn, character(1L))
    statuses <- chr_map(function(ch) shift_status(ch, refresh = FALSE))
    method_status <- vapply(
        seq_len(nrow(children)),
        function(i) {
            rec <- transform__record(children$scale[[i]], children$method[[i]])
            recipe__get(rec$recipe)@status
        },
        character(1L)
    )
    data.table::set(
        children,
        j = c("status", "run_id", "current_stage", "method_status"),
        value = list(
            statuses,
            chr_map(function(ch) store__chr1(ch@ids$run_id)),
            chr_map(function(ch) store__chr1(ch@meta$run$current_stage)),
            method_status
        )
    )
    activity <- stats::setNames(
        lapply(ch_list, function(ch) {
            if (!S7::S7_inherits(ch, ShiftRun)) {
                return(list())
            }
            st <- shift_ui_state__ui_live_state(
                ch@meta$run,
                shift_ui_state__ui_table_state(
                    ch@meta$run,
                    ch@meta$events,
                    ch@meta$cases
                ),
                ch@meta$ui_state
            )
            ts <- c(ch@meta$run$updated_at, ch@meta$jobs$heartbeat_at)
            ts <- ts[!is.na(ts)]
            st$updated_at <- if (length(ts)) {
                as.character(max(ts))
            } else {
                NA_character_
            }
            st
        }),
        children$child_key
    )
    cases <- shift_cases(x, refresh = FALSE)
    outputs <- shift_outputs(x, refresh = FALSE)
    diagnostics <- shift_diagnostics(x, refresh = FALSE)
    events <- shift_batch__inspect(ch_list, x@meta$manifest, function(ch) {
        data.table::as.data.table(shift_stage__coalesce(
            ch@meta$events,
            data.table::data.table()
        ))
    })
    if (nrow(events)) {
        data.table::setorderv(
            events,
            intersect(c("created_at", "event_id"), names(events))
        )
    }
    ch_time <- function(fn) {
        vapply(
            ch_list,
            function(ch) {
                v <- fn(ch)
                if (length(v)) as.numeric(v[[1L]]) else NA_real_
            },
            numeric(1L)
        )
    }
    starts <- ch_time(function(ch) ch@meta$run$started_at)
    ends <- ch_time(function(ch) {
        v <- ch@meta$run$completed_at
        if (!length(v) || is.na(v[[1L]])) ch@meta$run$updated_at else v
    })
    active <- any(statuses %in% c("queued", "running", "stopping"))
    elapsed <- if (any(is.finite(starts)) && (active || any(is.finite(ends)))) {
        end <- if (active) as.numeric(Sys.time()) else max(ends, na.rm = TRUE)
        max(0, end - min(starts, na.rm = TRUE))
    } else {
        0
    }
    summary <- data.table::data.table(
        batch_id = x@ids$batch_id,
        status = shift_status(x, refresh = FALSE),
        methods = data.table::uniqueN(children$method),
        configurations = data.table::uniqueN(children[,
            c("method", "scale", "reconstruction"),
            with = FALSE
        ]),
        models = data.table::uniqueN(children$model),
        children = nrow(children),
        completed = sum(statuses == "completed"),
        active = sum(statuses %in% c("queued", "running", "stopping")),
        failed = sum(statuses %in% c("failed", "blocked")),
        partial = sum(statuses == "partial"),
        waiting = sum(statuses %in% c("planned", "waiting")),
        cancelled = sum(statuses == "cancelled"),
        cases = nrow(cases),
        epw_files = nrow(outputs),
        warnings = sum(diagnostics$severity == "warning"),
        elapsed_seconds = elapsed,
        output_dir = x@meta$output_dir
    )
    result <- list(
        batch = summary,
        children = children,
        cases = cases,
        outputs = outputs,
        diagnostics = diagnostics,
        events = if (is.infinite(event_count)) {
            events
        } else {
            utils::tail(events, event_count)
        },
        source_progress = shift_batch_execution__job_read(
            x@store_path
        )$progress,
        call_elapsed_seconds = x@meta$call_elapsed_seconds,
        execution = shift_stage__coalesce(
            x@meta$execution,
            data.table::data.table()
        ),
        activity = activity
    )
    attr(result, "shift_ui_events") <- events
    result
}
# }}}

# Track observed event IDs per child run for lossless multi-worker batch watch.
# shift_batch_ui__event_delta {{{
shift_batch_ui__event_delta <- function(
    events,
    cursor = list(),
    initial_limit = 10L,
    initial = FALSE
) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    fresh <- rep(FALSE, nrow(events))
    gap <- FALSE
    keys <- as.character(shift_stage__coalesce(
        events$run_id,
        shift_stage__coalesce(events$child_key, rep("batch", nrow(events)))
    ))
    keys[is.na(keys) | !nzchar(keys)] <- "batch"
    for (key in unique(keys)) {
        pos <- which(keys %in% key)
        ids <- as.character(events$event_id[pos])
        seen <- cursor[[key]]
        fresh[pos] <- !ids %in% seen
        if (!initial && length(seen) && !any(ids %in% seen)) {
            gap <- TRUE
        }
        cursor[[key]] <- union(seen, ids)
    }
    rows <- if (initial) {
        shift_ui_state__tail_events(events, initial_limit)
    } else {
        events[fresh]
    }
    list(rows = rows, cursor = cursor, gap = gap)
}
# }}}

# }}}

# vim: fdm=marker :
