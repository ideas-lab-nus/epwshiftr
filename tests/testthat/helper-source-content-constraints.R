# Produce one stable public catalogue record while varying only the publisher
# content constraint; paths can be native files or real local HTTP services.
content_constraints__files <- function(
    path,
    opendap,
    download,
    checksum = NA_character_,
    algorithm = "sha256"
) {
    docs <- esgf_test__file_docs(
        path,
        opendap_url = opendap,
        download_url = download
    )
    data.table::set(docs, j = "master_id", value = "same-record")
    data.table::set(docs, j = "tracking_id", value = NA_character_)
    data.table::set(docs, j = "checksum", value = checksum)
    data.table::set(docs, j = "checksum_type", value = algorithm)
    data.table::set(docs, j = "size", value = as.double(file.info(path)$size))
    esgf_test__file_result(docs)
}

# vim: fdm=marker :
