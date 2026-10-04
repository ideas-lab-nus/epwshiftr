# Keep transport substitutions inside the test harness; production code has no
# global switches for replacing services, clocks or process launchers.
test_dependency_originals <- mget(
    c(
        "shift_batch__candidate_reader",
        "shift_resolve__cmip6_period_coverage",
        "query_result__resolve_file_services",
        "shift_job__launch_job",
        "downloader__launch_process",
        "cds__retrieve",
        "era__read_netcdf",
        "query__now"
    ),
    envir = asNamespace("epwshiftr")
)

# Scope mixed user configuration and mocked dependencies to one test or fixture.
# test_local_dependencies {{{
test_local_dependencies <- function(..., .local_envir = parent.frame()) {
    values <- list(...)
    if (
        length(values) == 1L && is.list(values[[1L]]) && is.null(names(values))
    ) {
        values <- values[[1L]]
    }
    if ("availability" %in% names(values)) {
        adapter <- values$availability
        values["availability"] <- NULL
        values$shift_batch__candidate_reader <- if (is.null(adapter)) {
            test_dependency_originals$shift_batch__candidate_reader
        } else {
            test_candidate_reader(adapter)
        }
    }
    if ("query_time" %in% names(values)) {
        instant <- values$query_time
        values["query_time"] <- NULL
        values$query__now <- function() instant
    }
    bindings <- intersect(names(values), names(test_dependency_originals))
    if (length(bindings)) {
        do.call(
            testthat::local_mocked_bindings,
            c(
                values[bindings],
                list(.package = "epwshiftr", .env = .local_envir)
            )
        )
    }
    withr::local_options(
        values[setdiff(names(values), bindings)],
        .local_envir = .local_envir
    )
    invisible(NULL)
}
# }}}

# Adapt a deterministic availability fixture to the shared candidate reader.
# test_candidate_reader {{{
test_candidate_reader <- function(adapter) {
    force(adapter)
    function(climate, transforms, references, store, ui) {
        member <- shift_stage__coalesce(climate@member, "r1i1p1f1")
        historical <- vapply(
            references,
            function(value) {
                S7::S7_inherits(value$reference, ShiftReferenceSpec) &&
                    identical(value$reference@mode, "historical")
            },
            logical(1L)
        )
        names(historical) <- names(transforms)
        return(function(transform_key, alternative, index_node) {
            transform <- transforms[[transform_key]]
            variables <- as.character(shift_batch__future_requirement(
                transform
            )@variable_sets[[alternative]])
            adapter(
                variables = variables,
                scenarios = climate@scenarios,
                include_historical = historical[[transform_key]],
                source = climate@model,
                member = member,
                grid = climate@grid,
                frequency = shift_spec__transform_cmip6_frequencies(
                    transform,
                    variables,
                    climate@frequency
                ),
                table = shift_batch__table_spec(climate@table, variables),
                activity = climate@activity,
                index_node = index_node,
                data_node = climate@data_node,
                filters = climate@filters,
                store = store,
                ui = ui
            )
        })
    }
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
