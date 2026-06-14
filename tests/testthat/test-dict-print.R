# dict print methods {{{

test_that("print.Cmip6CV() and print.Cmip6DReq()", {
    dict <- local_test_esgdict()
    normalize_dreq_indent <- function(x) {
        gsub(
            "(?m)^\\s+(\\* (Table id|Modeling realm|Standard name|Long name|Frequency|Units|Cell methods|Cell measures|Comment|Dimensions|Out name|Type|Positive|Valid min|Valid max|Ok min mean abs|Ok max mean abs):)",
            "\\1",
            x,
            perl = TRUE
        )
    }

    expect_snapshot(print(dict$get("activity_id")))
    expect_snapshot(print(dict$get("request"), n = 1L), transform = normalize_dreq_indent)
})

# }}}
