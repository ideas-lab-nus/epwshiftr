# Repeated default recovery cannot replace rejected shared publisher bytes.
test_that("pooled Store preserves edited publisher sources until explicit independent repair", {
    retained_native_test__check(TRUE)
})

# A request without publisher content evidence still carries explicit overwrite
# through the pool and obtains a fresh native target without losing old proof.
test_that("pooled Store independently repairs edited sources without publisher checksums", {
    retained_native_test__check(FALSE)
})

# vim: fdm=marker :
