# Recognize portable paths while testing CLI recovery instructions.
# shift_test__is_absolute_path {{{
shift_test__is_absolute_path <- function(path) {
    grepl("^(/|[A-Za-z]:[/\\\\])", path)
}
# }}}

# Capture several workflow objects under the same console settings.
# shift_test__print_objects {{{
shift_test__print_objects <- function(
    objects,
    width = 80L,
    n = 10L,
    verbose = FALSE
) {
    for (object in objects) {
        print(object, width = width, n = n, verbose = verbose)
    }
    invisible(NULL)
}
# }}}

# Remove machine-specific temporary roots from console assertions.
# shift_test__normalize_print {{{
shift_test__normalize_print <- function(x) {
    roots <- unique(c(
        tempdir(),
        normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    ))
    for (root in roots[nzchar(roots)]) {
        x <- gsub(root, "<tempdir>", x, fixed = TRUE)
    }
    x
}
# }}}

# Record catalog calls while supplying deterministic local File documents.
# shift_test__mock_collect {{{
shift_test__mock_collect <- function(file_docs, calls) {
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            type <- query_param__value(params$type())
            calls$query_reporter <- c(
                calls$query_reporter,
                is.function(progress_callback)
            )
            docs <- if (identical(type, "Dataset")) {
                calls$dataset_all <- c(calls$dataset_all, all)
                calls$dataset_limit <- c(calls$dataset_limit, limit)
                esgf_test__dataset_docs()
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            calls$values <- c(calls$values, type)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
}
# }}}

# Read a serialized query facet for offline catalog filtering.
# shift_test__param_value {{{
shift_test__param_value <- function(params, name) {
    state <- tryCatch(params$serialize(null = TRUE), error = function(e) list())
    value <- state[[name]]
    if (is.null(value)) {
        return(NULL)
    }
    if (is.list(value) && "value" %in% names(value)) {
        return(value$value)
    }
    value
}
# }}}

# Apply requested facets to the shared offline catalog.
# shift_test__mock_collect_filtered {{{
shift_test__mock_collect_filtered <- function(file_docs, calls) {
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            type <- query_param__value(params$type())
            filter_fields <- c(
                "experiment_id",
                "activity_id",
                "source_id",
                "variant_label",
                "frequency",
                "table_id",
                "variable_id"
            )
            filter_values <- stats::setNames(
                vector("list", length(filter_fields)),
                filter_fields
            )
            for (field in filter_fields) {
                values <- shift_test__param_value(params, field)
                values <- as.character(values)
                filter_values[[field]] <- values[
                    !is.na(values) & nzchar(values)
                ]
            }
            docs <- if (identical(type, "Dataset")) {
                calls$last_filters <- filter_values
                dataset_variable <- if (length(filter_values$variable_id)) {
                    filter_values$variable_id
                } else {
                    unique(file_docs$variable_id)
                }
                dataset <- esgf_test__dataset_docs(dataset_variable[[1L]])
                for (field in intersect(filter_fields, names(dataset))) {
                    if (length(filter_values[[field]])) {
                        dataset[[field]] <- filter_values[[field]][[1L]]
                    }
                }
                dataset
            } else {
                data.table::as.data.table(file_docs)
            }
            if (identical(type, "File")) {
                for (field in filter_fields) {
                    values <- filter_values[[field]]
                    if (
                        !length(values) && !is.null(calls$last_filters[[field]])
                    ) {
                        values <- calls$last_filters[[field]]
                    }
                    if (length(values) && field %in% names(docs)) {
                        docs <- docs[docs[[field]] %in% values]
                    }
                }
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            calls$values <- c(calls$values, type)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
}
# }}}

# Supply successive catalog snapshots to exercise refresh and recovery.
# shift_test__mock_collect_sequence {{{
shift_test__mock_collect_sequence <- function(file_doc_sets, calls) {
    calls$file_calls <- 0L
    calls$collect_times <- list()
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            type <- query_param__value(params$type())
            calls$collect_times <- c(
                calls$collect_times,
                list(list(
                    type = type,
                    datetime_start = shift_test__param_value(
                        params,
                        "datetime_start"
                    ),
                    datetime_stop = shift_test__param_value(
                        params,
                        "datetime_stop"
                    )
                ))
            )
            variables <- as.character(shift_test__param_value(
                params,
                "variable_id"
            ))
            variables <- variables[!is.na(variables) & nzchar(variables)]
            docs <- if (identical(type, "Dataset")) {
                esgf_test__dataset_docs(
                    if (length(variables)) variables[[1L]] else "tas"
                )
            } else {
                calls$file_calls <- calls$file_calls + 1L
                idx <- min(calls$file_calls, length(file_doc_sets))
                if (idx > 1L) {
                    params$experiment_id("historical")
                    params$activity_id("CMIP")
                } else {
                    params$experiment_id("ssp585")
                    params$activity_id("ScenarioMIP")
                }
                data.table::as.data.table(file_doc_sets[[idx]])
            }
            if (
                identical(type, "File") &&
                    length(variables) &&
                    "variable_id" %in% names(docs)
            ) {
                docs <- docs[docs$variable_id %in% variables]
            }
            fields <- query_param__value(params$fields())
            if (identical(type, "File")) {
                calls$file_fields <- c(calls$file_fields, list(fields))
            }
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            calls$values <- c(calls$values, type)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr",
        .env = parent.frame()
    )
}
# }}}

# Keep deterministic renderer fixtures with the tests: installed-package checks
# cannot load the repository-only README recording script from tools/.
# ui_workflows__states {{{
ui_workflows__states <- function() {
    transforms <- list(
        monthly_transform("original_morphing"),
        daily_transform("qdm")
    )
    children <- data.table::rbindlist(lapply(transforms, function(transform) {
        record <- transform__record(transform@scale, transform@method)
        data.table::data.table(
            method = transform@method,
            scale = transform@scale,
            reconstruction = transform@reconstruction,
            model = c("Model-A", "Model-B"),
            method_status = recipe__get(record$recipe)@status,
            status = "queued",
            current_stage = NA_character_
        )
    }))
    children[, child_key := paste0("child_ui", seq_len(.N))]
    children[1:2, `:=`(status = "running", current_stage = "extract_future")]
    summary <- data.table::data.table(
        batch_id = "batch_ui",
        status = "running",
        configurations = 2L,
        models = 2L,
        children = 4L,
        completed = 0L,
        active = 4L,
        failed = 0L,
        partial = 0L,
        waiting = 0L,
        cancelled = 0L,
        cases = 8L,
        epw_files = 0L,
        warnings = 0L,
        output_dir = "future-epw"
    )
    snapshot <- list(
        batch = summary,
        children = children,
        cases = data.table::data.table(),
        outputs = data.table::data.table(),
        diagnostics = shift_stage__diagnostics_empty(),
        execution = data.table::data.table()
    )
    states <- list(data.table::copy(snapshot))

    # Copy each stage before further data.table mutations so earlier snapshots
    # remain stable when a watch test advances to the next state.
    children[1:2, `:=`(status = "completed", current_stage = "write_epw")]
    children[3:4, `:=`(status = "running", current_stage = "morph")]
    summary[, `:=`(completed = 2L, active = 2L, epw_files = 4L)]
    states[[2L]] <- data.table::copy(snapshot)

    children[, `:=`(status = "completed", current_stage = "write_epw")]
    summary[, `:=`(
        status = "completed",
        completed = 4L,
        active = 0L,
        epw_files = 8L,
        warnings = 2L
    )]
    snapshot$diagnostics <- data.table::data.table(
        severity = "warning",
        method = "qdm",
        model = c("Model-A", "Model-B"),
        message = "Signal defaults for 'tas' are experimental."
    )
    snapshot$execution <- data.table::data.table(
        child_key = children$child_key,
        action = "started",
        elapsed_seconds = c(12, 14, 18, 20)
    )
    snapshot$call_elapsed_seconds <- 64
    states[[3L]] <- data.table::copy(snapshot)
    states
}
# }}}

# Plan a real offline matrix with saved receipts, avoiding remote catalog work.
# ui_workflows__batch {{{
ui_workflows__batch <- function(root) {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    shift_future_epw(
        shift_site(epw = get_cache_epw()),
        shift_cmip6(model = 2L, scenarios = "ssp585"),
        periods = list(mid = 2049:2050),
        methods = c("original_morphing", "bws_btws"),
        reference = shift_reference_historical(data.frame(
            period = "reference",
            year = 1995:2014
        )),
        dir = paste0(root, "-exports"),
        store = root,
        dry_run = TRUE,
        ui = shift_ui("none")
    )
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
