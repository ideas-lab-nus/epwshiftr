# Dataset-scoped IDs returned by a bridge must not merge distinct time slices.
test_that("dataset master IDs retain each native file and deduplicate replicas", {
    skip_if_not_installed("duckdb")
    dataset <- "CMIP6.CMIP.CMCC.CMCC-ESM2.historical.r1i1p1f1.day.tas.gn"
    filenames <- paste0(
        "tas_day_CMCC-ESM2_historical_r1i1p1f1_gn_",
        c("19750101-19991231", "20000101-20141231"),
        ".nc"
    )
    docs <- data.table::rbindlist(lapply(filenames, esgf_test__file_docs))
    data.table::set(docs, j = "master_id", value = dataset)
    data.table::set(
        docs,
        j = "instance_id",
        value = paste0(dataset, ".v20210114")
    )
    data.table::set(
        docs,
        j = "dataset_id",
        value = paste0(dataset, ".v20210114|example.org")
    )
    original <- data.table::copy(docs)
    replica <- data.table::copy(docs[1L])
    data.table::set(
        replica,
        j = "master_id",
        value = paste0(dataset, ".", filenames[1L])
    )
    data.table::set(replica, j = "replica", value = TRUE)
    data.table::set(replica, j = "data_node", value = "replica.example.org")
    data.table::set(
        replica,
        j = "id",
        value = paste0(filenames[1L], "|replica.example.org")
    )

    path <- tempfile("store-native-file-identity-")
    store <- EsgStore$new(path)
    on.exit(store$close(), add = TRUE)
    # add_files records query snapshots in file_catalog, not the tracked-query registry.
    catalog <- function(id) {
        store$query(sprintf(
            "SELECT * FROM file_catalog WHERE query_id = '%s'",
            id
        ))
    }
    expected <- paste0("master:", dataset, ".", filenames)
    first <- store$add_files(esgf_test__file_result(docs[1L]))
    expect_setequal(catalog(first)$file_key, expected[1L])
    second <- store$add_files(esgf_test__file_result(docs[2L]))
    expect_setequal(catalog(second)$file_key, expected[2L])
    combined <- store$add_files(esgf_test__file_result(
        data.table::rbindlist(list(docs, replica))
    ))
    expect_setequal(catalog(combined)$file_key, expected)
    expect_equal(nrow(store$query("SELECT * FROM esg_file")), 2L)
    expect_equal(docs, original)
    expect_equal(catalog(combined)$master_id, rep(dataset, 2L))
    store$close()
    store <- EsgStore$new(path)
    expect_setequal(catalog(combined)$file_key, expected)
})

# Existing valid identities and fallback semantics are unchanged by the repair.
test_that("file identity only qualifies a confirmed dataset-scoped master ID", {
    expect_identical(store__file_keys(data.table::data.table()), character())
    rows <- data.table::data.table(
        master_id = c(
            "dataset",
            "dataset.file.nc",
            "generic",
            "dataset",
            NA_character_
        ),
        dataset_id = c(
            "dataset.v20210114|node",
            "dataset.v20210114|node",
            NA,
            "dataset|node",
            NA
        ),
        filename = c("file.nc", "file.nc", "file.nc", NA, NA),
        tracking_id = c(NA, NA, NA, "tracked-file", "fallback-file")
    )
    expect_identical(
        store__file_keys(rows),
        c(
            "master:dataset.file.nc",
            "master:dataset.file.nc",
            "master:generic",
            "tracking:tracked-file",
            "tracking:fallback-file"
        )
    )
    expect_identical(store__file_keys(rows[5L]), "tracking:fallback-file")
})

# vim: fdm=marker :
