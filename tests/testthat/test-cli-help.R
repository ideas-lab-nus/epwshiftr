# epwshiftr_cli_help() {{{
test_that("epwshiftr_cli_help() resolves root, group, and command topics", {
    skip_if_not_installed("duckdb")

    missing_dir <- tempfile("missing-store-")
    root_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help"))
    expect_equal(root_help$status, 0L)
    expect_match(root_help$result[[1L]], "Usage: epwshiftr")
    expect_false(dir.exists(missing_dir))

    group_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "query", "help"))
    expect_equal(group_help$status, 0L)
    expect_match(group_help$result[[1L]], "Usage: epwshiftr query")
    expect_false(dir.exists(missing_dir))

    command_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "download", "run", "--help"))
    expect_equal(command_help$status, 0L)
    expect_match(command_help$result[[1L]], "Usage: epwshiftr download run")
    expect_false(dir.exists(missing_dir))

    watch_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "download", "watch"))
    expect_equal(watch_help$status, 0L)
    expect_match(watch_help$result[[1L]], "Usage: epwshiftr download watch")
    expect_false(dir.exists(missing_dir))

    shift_show_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "shift", "show"))
    expect_equal(shift_show_help$status, 0L)
    expect_match(shift_show_help$result[[1L]], "Usage: epwshiftr shift show")
    expect_false(dir.exists(missing_dir))

    shift_config_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "shift", "config", "validate"))
    expect_equal(shift_config_help$status, 0L)
    expect_match(shift_config_help$result[[1L]], "Usage: epwshiftr shift config validate")
    expect_false(dir.exists(missing_dir))

    extract_retry_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "extract", "retry"))
    expect_equal(extract_retry_help$status, 0L)
    expect_match(extract_retry_help$result[[1L]], "Usage: epwshiftr extract retry")
    expect_false(dir.exists(missing_dir))

    morph_retry_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "morph", "retry"))
    expect_equal(morph_retry_help$status, 0L)
    expect_match(morph_retry_help$result[[1L]], "Usage: epwshiftr morph retry")
    expect_false(dir.exists(missing_dir))

    help_topic <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "storage", "validate"))
    expect_equal(help_topic$status, 0L)
    expect_match(help_topic$result[[1L]], "Usage: epwshiftr storage validate")
    expect_false(dir.exists(missing_dir))

    bad_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "help", "missing"))
    expect_equal(bad_help$status, 2L)
    expect_match(bad_help$error, "Unknown help topic")
    expect_false(dir.exists(missing_dir))

    doctor_help <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "doctor", "--help"))
    expect_equal(doctor_help$status, 0L)
    expect_match(doctor_help$result[[1L]], "Usage: epwshiftr doctor")
    expect_false(dir.exists(missing_dir))
})
# }}}
