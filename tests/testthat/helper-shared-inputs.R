# Make a real two-period File catalog and a public two-city POWER plan. Only
# catalog transport is replaced; selection, snapshots and native reads are real.
# shared_inputs_test__fixture {{{
shared_inputs_test__fixture <- function(env = parent.frame()) {
    root <- withr::local_tempdir(.local_envir = env)
    test_local_dependencies(
        list(
            epwshiftr.dir_cache = file.path(root, "cache"),
            epwshiftr.mirai_workers = 1L,
            availability = test_cmip6_availability,
            shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
        ),
        .local_envir = env
    )
    specs <- data.table::CJ(
        experiment = c("historical", "ssp585"),
        variable = c("tas", "tasmin", "tasmax")
    )
    docs <- data.table::rbindlist(lapply(seq_len(nrow(specs)), function(i) {
        variable <- specs$variable[[i]]
        experiment <- specs$experiment[[i]]
        year <- if (experiment == "historical") 2000L else 2060L
        path <- file.path(root, paste0(experiment, "-", variable, ".nc"))
        write_local_cmip6_netcdf_fixture(
            path,
            year,
            variable,
            calendar = "360_day"
        )
        rows <- data.table::as.data.table(esgf_test__file_docs(
            path,
            variable_id = variable,
            datetime_start = sprintf("%d-01-01T00:00:00Z", year),
            datetime_end = sprintf("%d-12-31T23:59:59Z", year)
        ))
        data.table::set(rows, j = "experiment_id", value = experiment)
        data.table::set(rows, j = "checksum", value = checksum_file(path))
        data.table::set(rows, j = "size", value = file.size(path))
        rows
    }))
    sites <- list(
        shift_site(id = "one", lon = 104, lat = 1, epw = get_cache_epw()),
        shift_site(id = "two", lon = 254, lat = 41, epw = get_cache_epw())
    )
    batch <- shift_epw_future(
        sites,
        shift_cmip6(
            model = "EC-Earth3",
            member = "r1i1p1f1",
            grid = "gr",
            scenarios = "ssp585",
            index_nodes = "https://example.org"
        ),
        periods = list(future = 2060L),
        transform = daily_transform("epwshiftr", reconstruction = "power"),
        reference = historical_reference(2000L),
        store = file.path(root, "store"),
        dir = file.path(root, "output"),
        dry_run = TRUE,
        ui = shift_ui("none")
    )
    list(batch = batch, docs = docs, root = root)
}
# }}}

# vim: fdm=marker :
