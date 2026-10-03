#' @include query.R store.R epw-morpher.R utils.R weather-transform.R
#' @include source-reanalysis.R source-era5.R source-cds.R
NULL

# shift diagnostics
SHIFT_DIAGNOSTIC_COLUMNS <- c(
    "stage",
    "severity",
    "code",
    "message",
    "query_id",
    "session_id",
    "plan_id",
    "summary_id",
    "baseline_id",
    "morph_id",
    "case_id",
    "variable_id",
    "epw_field",
    "period",
    "month",
    "action"
)

shift_stage__diagnostic_columns <- function() {
    SHIFT_DIAGNOSTIC_COLUMNS
}

shift_stage__diagnostics_empty <- function() {
    out <- stats::setNames(
        rep(list(character()), length(SHIFT_DIAGNOSTIC_COLUMNS)),
        SHIFT_DIAGNOSTIC_COLUMNS
    )
    data.table::as.data.table(out)
}

shift_stage__diagnostics_normalize <- function(x = NULL) {
    if (is.null(x)) {
        return(shift_stage__diagnostics_empty())
    }
    out <- data.table::copy(data.table::as.data.table(x))
    for (col in SHIFT_DIAGNOSTIC_COLUMNS) {
        if (!col %in% names(out)) {
            out[[col]] <- rep(NA_character_, nrow(out))
        }
    }
    out <- out[, SHIFT_DIAGNOSTIC_COLUMNS, with = FALSE]
    for (col in SHIFT_DIAGNOSTIC_COLUMNS) {
        out[[col]] <- as.character(out[[col]])
    }
    out[]
}

shift_stage__diagnostic <- function(
    stage,
    severity,
    code,
    message,
    ...,
    action = NA_character_
) {
    dots <- list(...)
    row <- stats::setNames(
        as.list(rep(NA_character_, length(SHIFT_DIAGNOSTIC_COLUMNS))),
        SHIFT_DIAGNOSTIC_COLUMNS
    )
    row$stage <- stage
    row$severity <- severity
    row$code <- code
    row$message <- message
    row$action <- action
    for (name in intersect(names(dots), SHIFT_DIAGNOSTIC_COLUMNS)) {
        row[[name]] <- as.character(dots[[name]])
    }
    shift_stage__diagnostics_normalize(data.table::as.data.table(row))
}

shift_stage__bind_diagnostics <- function(...) {
    parts <- list(...)
    parts <- Filter(function(x) !is.null(x) && nrow(x), parts)
    if (!length(parts)) {
        return(shift_stage__diagnostics_empty())
    }
    shift_stage__diagnostics_normalize(data.table::rbindlist(
        parts,
        fill = TRUE
    ))
}

shift_stage__has_errors <- function(x) {
    diagnostics <- shift_stage__diagnostics_normalize(x)
    any(diagnostics$severity %in% "error")
}

shift_stage__abort_diagnostics <- function(diagnostics) {
    diagnostics <- shift_stage__diagnostics_normalize(diagnostics)
    errors <- diagnostics[diagnostics[["severity"]] %in% "error"]
    if (!nrow(errors)) {
        return(invisible(diagnostics))
    }
    cli::cli_abort(c(
        "Blocking shift workflow diagnostic(s) were found.",
        "x" = errors$message
    ))
}

# shift S7 stage classes
ShiftDiagnostics <- S7::new_S3_class("data.frame")

shift_stage__prop_string <- function(
    null.ok = FALSE,
    min.chars = NULL,
    default = NULL
) {
    checkmate_property(
        S7::class_any,
        checkmate::check_string,
        null.ok = null.ok,
        min.chars = min.chars,
        default = default
    )
}

shift_stage__prop_number <- function(lower = -Inf, upper = Inf) {
    checkmate_property(
        S7::class_any,
        checkmate::check_number,
        lower = lower,
        upper = upper,
        finite = TRUE
    )
}

ShiftStage <- S7::new_class(
    "ShiftStage",
    abstract = TRUE,
    properties = list(
        stage = shift_stage__prop_string(min.chars = 1L),
        store_path = shift_stage__prop_string(
            null.ok = TRUE,
            min.chars = 1L,
            default = NULL
        ),
        ids = S7::new_property(S7::class_list, default = list()),
        meta = S7::new_property(S7::class_list, default = list()),
        diagnostics = S7::new_property(
            ShiftDiagnostics,
            default = shift_stage__diagnostics_empty()
        )
    )
)

ShiftRequest <- S7::new_class("ShiftRequest", parent = ShiftStage)
# ShiftDatasets is an internal persistence envelope for a standalone Dataset
# catalog query. The public API continues to return EsgResultDataset so its
# established query-result methods remain available without an adapter layer.
ShiftDatasets <- S7::new_class("ShiftDatasets", parent = ShiftStage)
ShiftFiles <- S7::new_class("ShiftFiles", parent = ShiftStage)
ShiftDownload <- S7::new_class("ShiftDownload", parent = ShiftStage)
ShiftClimate <- S7::new_class("ShiftClimate", parent = ShiftStage)
ShiftMorphed <- S7::new_class("ShiftMorphed", parent = ShiftStage)
ShiftOutputs <- S7::new_class("ShiftOutputs", parent = ShiftStage)
# ShiftPlan stores a deferred end-to-end workflow that can be explained or run.
ShiftPlan <- S7::new_class("ShiftPlan", parent = ShiftStage)
# ShiftRun is a lightweight handle to a persisted end-to-end workflow run.
ShiftRun <- S7::new_class("ShiftRun", parent = ShiftStage)

# Reference roles distinguish historical model output from observations before
# either source is attached to a reusable transformation.
SHIFT_REFERENCE_ROLES <- c("model_historical", "observed_reference")

ShiftReferenceSpec <- S7::new_class(
    "ShiftReferenceSpec",
    properties = list(
        mode = shift_stage__prop_string(min.chars = 1L),
        role = shift_stage__prop_string(min.chars = 1L),
        plan_id = S7::new_property(S7::class_any, default = NULL),
        periods = S7::new_property(S7::class_any, default = NULL),
        experiment = shift_stage__prop_string(
            null.ok = TRUE,
            min.chars = 1L,
            default = NULL
        ),
        activity = shift_stage__prop_string(
            null.ok = TRUE,
            min.chars = 1L,
            default = NULL
        ),
        match = S7::new_property(S7::class_character, default = character()),
        filters = S7::new_property(S7::class_list, default = list()),
        options = S7::new_property(S7::class_list, default = list()),
        collect = S7::new_property(S7::class_list, default = list()),
        extract = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (!self@mode %in% c("historical", "plan")) {
            return("`mode` must be `historical` or `plan`.")
        }
        if (!self@role %in% SHIFT_REFERENCE_ROLES) {
            return(
                "`role` must be `model_historical` or `observed_reference`."
            )
        }
        if (
            identical(self@mode, "historical") &&
                !identical(self@role, "model_historical")
        ) {
            return(
                "Automatic historical resolution can only provide model_historical input."
            )
        }
        NULL
    }
)

ShiftSite <- S7::new_class(
    "ShiftSite",
    parent = ShiftStage,
    properties = list(
        id = shift_stage__prop_string(min.chars = 1L),
        lon = shift_stage__prop_number(lower = -180, upper = 360),
        lat = shift_stage__prop_number(lower = -90, upper = 90),
        label = shift_stage__prop_string(
            null.ok = TRUE,
            min.chars = 1L,
            default = NULL
        ),
        epw = S7::new_property(S7::class_any, default = NULL),
        metadata = S7::new_property(S7::class_list, default = list())
    )
)

# Validate a normalized scalar or fully named variable mapping without coercion.
# Constructors handle list input; the same invariant also protects S7 mutation.
shift_stage__check_mapping <- function(x, null.ok = TRUE) {
    valid <- checkmate::check_character(
        x,
        any.missing = FALSE,
        min.len = 1L,
        min.chars = 1L,
        null.ok = null.ok
    )
    if (!isTRUE(valid) || is.null(x)) {
        return(valid)
    }
    nms <- names(x)
    if (is.null(nms)) {
        if (length(x) != 1L) {
            return("An unnamed mapping must contain exactly one value.")
        }
    } else {
        valid <- checkmate::check_names(nms, type = "unique")
        if (!isTRUE(valid)) return(valid)
    }
    TRUE
}

# ShiftCmip6Spec keeps future-climate identity together and validates both
# construction and subsequent field assignments.
ShiftCmip6Spec <- S7::new_class(
    "ShiftCmip6Spec",
    properties = list(
        model = checkmate_property(
            check = checkmate::check_character,
            any.missing = FALSE,
            min.len = 1L,
            min.chars = 1L,
            unique = TRUE,
            null.ok = TRUE
        ),
        n_models = checkmate_property(
            check = checkmate::check_integer,
            lower = 1L,
            len = 1L,
            any.missing = FALSE,
            null.ok = TRUE
        ),
        scenarios = checkmate_property(
            S7::class_character,
            checkmate::check_character,
            any.missing = FALSE,
            min.len = 1L,
            min.chars = 1L,
            unique = TRUE,
            default = character()
        ),
        member = checkmate_property(
            check = checkmate::check_character,
            any.missing = FALSE,
            min.len = 1L,
            min.chars = 1L,
            unique = TRUE,
            null.ok = TRUE
        ),
        grid = shift_stage__prop_string(null.ok = TRUE, min.chars = 1L),
        frequency = checkmate_property(
            check = shift_stage__check_mapping,
            null.ok = TRUE
        ),
        table = checkmate_property(
            check = shift_stage__check_mapping,
            null.ok = TRUE
        ),
        activity = shift_stage__prop_string(min.chars = 1L),
        index_nodes = checkmate_property(
            S7::class_character,
            checkmate::check_character,
            any.missing = FALSE,
            min.len = 1L,
            min.chars = 1L,
            unique = TRUE,
            default = character()
        ),
        data_node = shift_stage__prop_string(null.ok = TRUE, min.chars = 1L),
        filters = checkmate_property(
            S7::class_list,
            checkmate::check_list,
            names = "unique",
            default = list()
        ),
        common = checkmate_property(
            S7::class_logical,
            checkmate::check_flag,
            default = TRUE
        )
    ),
    validator = function(self) {
        if (!is.null(self@model) && !is.null(self@n_models)) {
            return(
                "Explicit model IDs and an automatic model count cannot be combined."
            )
        }
        NULL
    }
)

# ShiftControl centralises workflow-wide execution and fulfilment policies so
# stage option lists cannot silently override them.
ShiftControl <- S7::new_class(
    "ShiftControl",
    properties = list(
        strict = S7::new_property(S7::class_logical),
        allow_partial = S7::new_property(S7::class_logical),
        download = shift_stage__prop_string(min.chars = 1L),
        resume = S7::new_property(S7::class_logical),
        overwrite = S7::new_property(S7::class_logical),
        refresh = S7::new_property(S7::class_logical),
        extraction_method = shift_stage__prop_string(min.chars = 1L),
        output_layout = shift_stage__prop_string(min.chars = 1L)
    )
)

shift_stage__new <- function(
    class,
    stage,
    store_path = NULL,
    ids = list(),
    meta = list(),
    diagnostics = NULL,
    ...
) {
    class(
        stage = stage,
        store_path = store_path,
        ids = ids,
        meta = meta,
        diagnostics = shift_stage__diagnostics_normalize(diagnostics),
        ...
    )
}

shift_stage__assert_stage <- function(x) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        cli::cli_abort("`x` must be a shift stage object.")
    }
    invisible(x)
}

shift_stage__coalesce <- function(x, y) {
    if (is.null(x)) y else x
}

shift_stage__sql_string <- function(x) {
    paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

shift_stage__query_maybe <- function(store, sql) {
    tryCatch(store$query(sql), error = function(e) data.table::data.table())
}

shift_stage__query_ids <- function(ids) {
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (!length(ids)) {
        return("NULL")
    }
    paste(vapply(ids, shift_stage__sql_string, character(1L)), collapse = ", ")
}

shift_stage__root <- function(x) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    meta <- x@meta
    for (name in c("request", "files", "download", "climate", "morphed")) {
        value <- meta[[name]]
        if (S7::S7_inherits(value, ShiftStage)) {
            root <- shift_stage__root(value)
            if (!is.null(root)) {
                return(root)
            }
        }
    }
    if (S7::S7_inherits(x, ShiftRequest)) {
        return(x)
    }
    NULL
}

shift_stage__value <- function(x, name) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    if (name %in% names(x@meta)) {
        return(x@meta[[name]])
    }
    root <- shift_stage__root(x)
    if (!is.null(root) && name %in% names(root@meta)) {
        return(root@meta[[name]])
    }
    NULL
}

shift_stage__variables <- function(x) {
    for (name in c("variables", "variable_id")) {
        value <- shift_stage__value(x, name)
        if (!is.null(value)) {
            return(as.character(value))
        }
    }
    NULL
}

shift_stage__nested <- function(x, classes = list()) {
    if (!S7::S7_inherits(x, ShiftStage)) {
        return(NULL)
    }
    if (
        !length(classes) ||
            any(vapply(
                classes,
                function(class) S7::S7_inherits(x, class),
                logical(1L)
            ))
    ) {
        return(x)
    }
    for (name in c("files", "download", "climate", "morphed")) {
        value <- x@meta[[name]]
        if (S7::S7_inherits(value, ShiftStage)) {
            hit <- shift_stage__nested(value, classes)
            if (!is.null(hit)) {
                return(hit)
            }
        }
    }
    NULL
}

# generics
#' @rdname shift_api
#' @param x A shift stage object.
#' @param store An [EsgStore], store path, or `NULL`.
#' @param fields File fields collected from Dataset records. The default
#'   requests all fields and lets the result/store layers preserve and validate
#'   provider response metadata.
#' @param all,limit Collection controls passed to [EsgQuery] /
#'   [EsgResultDataset]. If a numeric `limit` is supplied without explicitly
#'   setting `all`, it caps the Dataset result count. With `all = TRUE`, a
#'   numeric `limit` retains the low-level meaning of pagination page size.
#' @param label Optional label recorded with collected File records.
#' @export
shift_collect <- S7::new_generic(
    "shift_collect",
    "x",
    function(
        x,
        store = NULL,
        fields = "*",
        all = TRUE,
        limit = FALSE,
        label = NULL,
        ui = NULL,
        ...
    ) {
        # At the task API, a bare numeric limit means a user-facing result cap.
        # Callers that need the low-level ESGF page-size meaning can request it
        # explicitly with `all = TRUE, limit = n`.
        if (
            missing(all) &&
                is.numeric(limit) &&
                length(limit) == 1L &&
                !is.na(limit) &&
                is.finite(limit)
        ) {
            all <- FALSE
        }
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            options <- list(...)
            return(shift_run__task_execute(
                "collect",
                x,
                store = store,
                ui = ui,
                spec = list(
                    fields = fields,
                    all = all,
                    limit = limit,
                    label = label,
                    options = options
                ),
                code = function(reporter, task_store) {
                    shift_run__with_reporter(
                        reporter,
                        do.call(
                            shift_collect,
                            c(
                                list(
                                    x = x,
                                    store = task_store,
                                    fields = fields,
                                    all = all,
                                    limit = limit,
                                    label = label
                                ),
                                options
                            )
                        )
                    )
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param downloader Optional [Downloader] instance.
#' @param run Whether to run queued downloads immediately. Downloading full
#'   NetCDF files is optional for the normal workflow because [shift_extract()]
#'   can use OPeNDAP first and only download as a fallback when requested.
#' @param background For [shift_download()], whether to run queued downloads in
#'   a background job. For task-level run/resume functions, whether to launch a
#'   detached `Rscript` worker. Single plans return a queued `ShiftRun`; batches
#'   return a queued `ShiftBatch` whose single coordinator shares source reads
#'   and starts child workflows. Both modes honor the same source-worker limit
#'   from `options(epwshiftr.mirai_workers = 4L)`. Use [shift_refresh()] or
#'   [shift_watch()] to follow source reading and newly registered child runs.
#' @param resume Whether to reuse complete existing downloads, extraction
#'   outputs, morphing results, or EPW outputs.
#' @param overwrite Whether to overwrite existing downloads, extraction outputs,
#'   morphing results, or EPW outputs.
#' @param session_label Optional download session label.
#' @export
shift_download <- S7::new_generic(
    "shift_download",
    "x",
    function(
        x,
        downloader = NULL,
        run = TRUE,
        background = FALSE,
        resume = TRUE,
        overwrite = FALSE,
        session_label = NULL,
        ui = NULL,
        ...
    ) {
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            options <- list(...)
            reconstructible <- is.null(downloader)
            return(shift_run__task_execute(
                "download",
                x,
                ui = ui,
                spec = list(
                    run = run,
                    background = background,
                    resume = resume,
                    overwrite = overwrite,
                    session_label = session_label,
                    options = options
                ),
                resumable = reconstructible,
                nonresumable_reason = if (reconstructible) {
                    NULL
                } else {
                    "A session-local Downloader instance cannot be reconstructed."
                },
                code = function(reporter, task_store) {
                    shift_run__with_reporter(
                        reporter,
                        do.call(
                            shift_download,
                            c(
                                list(
                                    x = x,
                                    downloader = downloader,
                                    run = run,
                                    background = background,
                                    resume = resume,
                                    overwrite = overwrite,
                                    session_label = session_label
                                ),
                                options
                            )
                        )
                    )
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param site A `shift_site()` object.
#' @param periods A period table, usually from [epw_morph_periods()].
#'   [shift_future_epw()] and [shift_plan()] also accept a numeric vector of
#'   target years; each year becomes an independently named output period.
#' @param method Grid extraction method used by [shift_extract()].
#' @param fallback Extraction fallback policy.
#' @export
shift_extract <- S7::new_generic(
    "shift_extract",
    "x",
    function(
        x,
        site = NULL,
        periods = NULL,
        variables = NULL,
        time = NULL,
        filters = list(),
        method = "nearest",
        fallback = c("auto", "error"),
        overwrite = FALSE,
        resume = TRUE,
        ui = NULL
    ) {
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            return(shift_run__task_execute(
                "extract",
                x,
                ui = ui,
                spec = list(
                    site = shift_persist__site_ref(site),
                    periods = if (is.null(periods)) {
                        NULL
                    } else {
                        split(as.integer(periods$year), periods$period)
                    },
                    variables = variables,
                    time = time,
                    filters = filters,
                    method = method,
                    fallback = fallback,
                    overwrite = overwrite,
                    resume = resume
                ),
                code = function(reporter, task_store) {
                    shift_run__with_reporter(
                        reporter,
                        shift_extract(
                            x,
                            site = site,
                            periods = periods,
                            variables = variables,
                            time = time,
                            filters = filters,
                            method = method,
                            fallback = fallback,
                            overwrite = overwrite,
                            resume = resume
                        )
                    )
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param baseline Optional baseline EPW path or
#'   `shift_site()` object containing `epw`.
#' @param reference Optional `ShiftReferenceSpec` or `ShiftClimate` stage
#'   containing historical model climate when required by `transform`.
#' @param observed_reference Optional plan-backed `ShiftReferenceSpec` or
#'   `ShiftClimate` stage containing observed weather when required by
#'   `transform`.
#' @param complete_only Whether [shift_morph()] should morph only complete
#'   extraction plans when a climate stage also contains failed or incomplete
#'   plans.
#' @param by Grouping columns used to create morphing cases.
#' @export
shift_morph <- S7::new_generic(
    "shift_morph",
    "x",
    function(
        x,
        baseline = NULL,
        transform,
        reference = NULL,
        observed_reference = NULL,
        strict = TRUE,
        complete_only = TRUE,
        by = c("source_id", "experiment_id", "variant_label", "period"),
        overwrite = FALSE,
        resume = TRUE,
        ui = NULL
    ) {
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            transform__validate_execution_inputs(
                transform,
                reference,
                observed_reference
            )
            baseline_path <- if (shift_spec__is_epw_path(baseline)) {
                baseline
            } else {
                NULL
            }
            reconstructible <- is.null(baseline) || !is.null(baseline_path)
            return(shift_run__task_execute(
                "morph",
                x,
                ui = ui,
                spec = list(
                    baseline = baseline_path,
                    transform = transform__spec_value(transform),
                    reference = shift_persist__reference_spec_value(
                        reference,
                        role = "model_historical"
                    ),
                    observed_reference = shift_persist__reference_spec_value(
                        observed_reference,
                        role = "observed_reference"
                    ),
                    strict = strict,
                    complete_only = complete_only,
                    by = by,
                    overwrite = overwrite,
                    resume = resume
                ),
                resumable = reconstructible,
                nonresumable_reason = if (reconstructible) {
                    NULL
                } else {
                    "The baseline exists only in this R session."
                },
                code = function(reporter, task_store) {
                    shift_run__with_reporter(
                        reporter,
                        shift_morph(
                            x,
                            baseline = baseline,
                            transform = transform,
                            reference = reference,
                            observed_reference = observed_reference,
                            strict = strict,
                            complete_only = complete_only,
                            by = by,
                            overwrite = overwrite,
                            resume = resume
                        )
                    )
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param dir In [shift_future_epw()], the user-facing delivery directory. In
#'   [shift_epw()], an output directory inside the store root; relative paths
#'   are resolved under the store root.
#' @param separate Whether to create separate output directories per morphing case.
#' @param export_dir Optional directory outside or inside the store where EPW
#'   files should also be copied for user-facing delivery.
#' @export
shift_epw <- S7::new_generic(
    "shift_epw",
    "x",
    function(
        x,
        dir = NULL,
        separate = TRUE,
        export_dir = NULL,
        overwrite = FALSE,
        resume = TRUE,
        ui = NULL
    ) {
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            return(shift_run__task_execute(
                "write_epw",
                x,
                ui = ui,
                spec = list(
                    dir = dir,
                    separate = separate,
                    export_dir = export_dir,
                    overwrite = overwrite,
                    resume = resume
                ),
                auto_complete = !is.null(export_dir),
                code = function(reporter, task_store) {
                    shift_run__with_reporter(
                        reporter,
                        shift_epw(
                            x,
                            dir = dir,
                            separate = separate,
                            export_dir = export_dir,
                            overwrite = overwrite,
                            resume = resume
                        )
                    )
                }
            ))
        }
        S7::S7_dispatch()
    }
)

#' @rdname shift_api
#' @param strict If `TRUE`, abort when diagnostics contain errors.
#' @param network For a reanalysis source, whether to verify the configured
#'   token against the provider. The default performs local configuration
#'   checks only and never submits a data request.
#' @export
shift_check <- S7::new_generic(
    "shift_check",
    "x",
    function(
        x,
        strict = FALSE,
        network = FALSE,
        ...
    ) {
        S7::S7_dispatch()
    }
)

# check methods
S7::method(shift_check, ShiftStage) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_normalize(x@diagnostics)
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftRequest) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_empty()
    if (!identical(x@meta$provider, "esgf")) {
        diagnostics <- shift_stage__diagnostic(
            stage = "request",
            severity = "error",
            code = "unsupported_provider",
            message = sprintf(
                "Unsupported shift provider: %s",
                x@meta$provider
            ),
            action = "Use provider = 'esgf' or add a provider adapter."
        )
    }
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

# Validate local CDS configuration for reanalysis sources and optionally
# authenticate it remotely without submitting a dataset retrieval request.
S7::method(shift_check, ShiftReanalysisSpec) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_empty()
    config <- tryCatch(
        cds__config(),
        epwshiftr_cds_auth_error = function(error) error
    )
    if (inherits(config, "epwshiftr_cds_auth_error")) {
        diagnostics <- shift_stage__diagnostic(
            stage = "source",
            severity = "error",
            code = "cds_auth_missing",
            message = conditionMessage(config),
            action = paste(
                "Register for CDS access, create a personal access token,",
                "and configure `ECMWF_DATASTORES_KEY` or `~/.cdsapirc`."
            )
        )
    } else if (isTRUE(network)) {
        remote_error <- tryCatch(
            {
                cds__check_authentication(config = config)
                NULL
            },
            epwshiftr_cds_auth_error = function(error) error,
            epwshiftr_cds_request_error = function(error) error
        )
        if (!is.null(remote_error)) {
            diagnostics <- shift_stage__diagnostic(
                stage = "source",
                severity = "error",
                code = if (
                    inherits(
                        remote_error,
                        "epwshiftr_cds_auth_error"
                    )
                ) {
                    "cds_auth_invalid"
                } else {
                    "cds_auth_unavailable"
                },
                message = conditionMessage(remote_error),
                action = if (
                    inherits(
                        remote_error,
                        "epwshiftr_cds_auth_error"
                    )
                ) {
                    "Replace the configured CDS personal access token."
                } else {
                    "Check network access and the CDS service status, then retry."
                }
            )
        }
    }
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftFiles) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_empty()
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (is.null(store)) {
        diagnostics <- shift_stage__diagnostic(
            "files",
            "error",
            "missing_store",
            "The store for this collected file stage cannot be opened.",
            query_id = x@ids$query_id,
            action = "Check `shift_store(x)` and the stored path."
        )
    } else {
        files <- shift_inspect__file_catalog(store, x@ids$query_id)
        if (!nrow(files)) {
            diagnostics <- shift_stage__diagnostic(
                "files",
                "error",
                "missing_file_catalog",
                "No file catalog rows were found for this collected file stage.",
                query_id = x@ids$query_id,
                action = "Run `shift_collect()` again."
            )
        }
    }
    diagnostics <- shift_stage__bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftDownload) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_empty()
    store <- tryCatch(shift_store(x), error = function(e) NULL)
    if (!is.null(store)) {
        tasks <- if (!is.null(x@ids$session_id) && !is.na(x@ids$session_id)) {
            tryCatch(
                store$download_status(session_id = x@ids$session_id),
                error = function(e) data.table::data.table()
            )
        } else {
            data.table::data.table()
        }
        if (nrow(tasks)) {
            failed <- tasks[tasks$status %in% c("error", "cancelled")]
            if (nrow(failed)) {
                diagnostics <- shift_stage__bind_diagnostics(
                    diagnostics,
                    shift_stage__diagnostic(
                        "download",
                        "error",
                        "download_failed",
                        sprintf(
                            "%d download task(s) failed or were cancelled.",
                            nrow(failed)
                        ),
                        query_id = x@ids$query_id,
                        session_id = x@ids$session_id,
                        action = "Retry `shift_download()` with resume = TRUE."
                    )
                )
            }
        }
    }
    diagnostics <- shift_stage__bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftClimate) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    store <- shift_store(x)
    coverage <- store$coverage(plan_id = x@ids$plan_id)
    diagnostics <- shift_stage__bind_diagnostics(
        x@diagnostics,
        shift_stage__diagnostics_from_coverage(coverage)
    )
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftMorphed) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_normalize(x@diagnostics)
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

S7::method(shift_check, ShiftOutputs) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_stage__diagnostics_empty()
    store <- shift_store(x)
    outputs <- shift_outputs(x)
    path_col <- intersect(
        c("path", "output_path", "relative_path"),
        names(outputs)
    )
    if (
        !nrow(outputs) ||
            !length(path_col) ||
            !shift_inspect__relative_paths_exist(
                store,
                outputs[[path_col[[1L]]]]
            )
    ) {
        diagnostics <- shift_stage__diagnostic(
            "outputs",
            "error",
            "missing_epw_output",
            "Expected EPW output files were not found.",
            morph_id = x@ids$morph_id,
            action = "Run `shift_epw()` again or check the output directory."
        )
    }
    diagnostics <- shift_stage__bind_diagnostics(x@diagnostics, diagnostics)
    if (isTRUE(strict)) {
        shift_stage__abort_diagnostics(diagnostics)
    }
    diagnostics
}

shift_stage__diagnostics_from_coverage <- function(coverage) {
    coverage <- data.table::as.data.table(coverage)
    if (!nrow(coverage)) {
        return(shift_stage__diagnostics_empty())
    }
    diagnostics <- vector("list", nrow(coverage))
    for (i in seq_len(nrow(coverage))) {
        row <- coverage[i]
        if (isTRUE(row$complete[[1L]])) {
            diagnostics[[i]] <- shift_stage__diagnostics_empty()
            next
        }
        severity <- if (identical(row$status[[1L]], "failed")) {
            "error"
        } else {
            "warning"
        }
        message <- if (
            !is.na(row$last_error[[1L]]) && nzchar(row$last_error[[1L]])
        ) {
            row$last_error[[1L]]
        } else {
            "Extraction coverage is incomplete."
        }
        diagnostics[[i]] <- shift_stage__diagnostic(
            "extract",
            severity,
            "incomplete_extraction",
            message,
            query_id = row$query_id[[1L]],
            plan_id = row$plan_id[[1L]],
            variable_id = row$variable_id[[1L]],
            action = "Run `shift_extract()` again or inspect `shift_coverage()`."
        )
    }
    do.call(shift_stage__bind_diagnostics, diagnostics)
}
