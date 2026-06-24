cmip6dict__parse_dreq_header <- function(lst) {
    res <- list()

    ori <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", ori), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    res$dreq_version <- numeric_version(lst$data_specs_version)
    res$cmor_version <- numeric_version(lst$cmor_version)
    res$table_id <- sub("Table ", "", lst$table_id, fixed = TRUE)
    res$table_date <- as.Date(lst$table_date, format = "%d %b %Y")
    res$realm <- lst$realm
    res$dbl_missing_value <- as.double(lst$missing_value)
    res$int_missing_value <- as.integer(lst$int_missing_value)
    res$mip_era <- lst$mip_era
    res$conventions <- lst$Conventions

    res
}

cmip6dict__parse_dreq_file <- function(file) {
    json <- jsonlite::read_json(file)
    header <- cmip6dict__parse_dreq_header(json[["Header"]])

    d <- cmip6dict__format_cv_nest(json[["variable_entry"]])
    data.table::set(d, NULL, "variable", names(json[["variable_entry"]]))
    data.table::setcolorder(d, "variable")

    empty_to_na <- function(x) {
        x[x == ""] <- NA_character_
        x
    }
    for (col in names(d)) {
        data.table::set(d, NULL, col, empty_to_na(unlist(d[[col]], FALSE, FALSE)))
    }

    data.table::setattr(d, "metadata", header)

    d
}
