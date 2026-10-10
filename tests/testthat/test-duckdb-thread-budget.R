# Both connection factories must obey the same opt-in process budget, including
# the standalone Downloader entry point used outside the installed package.
for (factory_name in c("ddb_connect", "downloader__ddb_connect")) {
    test_that(
        paste(factory_name, "keeps default instance behavior without a budget"),
        {
            skip_if_not_installed("duckdb")
            withr::local_envvar(c(EPWSHIFTR_DB_THREADS = NA_character_))
            factory <- get(factory_name)
            default <- duckdb::dbConnect(duckdb::duckdb())
            on.exit(ddb_disconnect(default), add = TRUE)
            expected <- ddb_query(
                default,
                "SELECT current_setting('threads') AS threads"
            )
            conn <- factory(":memory:")
            on.exit(ddb_disconnect(conn), add = TRUE)
            expect_identical(
                ddb_query(conn, "SELECT current_setting('threads') AS threads"),
                expected
            )
            expect_null(downloader__ddb_thread_config(list(threads = "2")))
        }
    )

    test_that(
        paste(
            factory_name,
            "applies the budget before creating a memory instance"
        ),
        {
            skip_if_not_installed("duckdb")
            withr::local_envvar(c(EPWSHIFTR_DB_THREADS = "1"))
            factory <- get(factory_name)
            conn <- factory(":memory:")
            on.exit(ddb_disconnect(conn), add = TRUE)
            expect_equal(
                ddb_query(
                    conn,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                1
            )
            explicit <- factory(":memory:", config = list(threads = "2"))
            on.exit(ddb_disconnect(explicit), add = TRUE)
            expect_equal(
                ddb_query(
                    explicit,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                2
            )
            alias <- factory(":memory:", config = list(worker_threads = "2"))
            on.exit(ddb_disconnect(alias), add = TRUE)
            expect_equal(
                ddb_query(
                    alias,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                2
            )
            # Independently created memory instances never share configuration.
            expect_equal(
                ddb_query(
                    conn,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                1
            )
        }
    )

    test_that(
        paste(
            factory_name,
            "preserves file instance reuse and read-only boundaries"
        ),
        {
            skip_if_not_installed("duckdb")
            withr::local_envvar(c(EPWSHIFTR_DB_THREADS = "1"))
            factory <- get(factory_name)
            path <- tempfile(fileext = ".duckdb")
            on.exit(unlink(path), add = TRUE)
            first <- factory(path)
            on.exit(if (ddb_is_valid(first)) ddb_disconnect(first), add = TRUE)
            ddb_exec(first, "CREATE TABLE sentinel AS SELECT 42 AS value")
            second <- factory(path)
            on.exit(
                if (ddb_is_valid(second)) ddb_disconnect(second),
                add = TRUE
            )
            expect_equal(
                ddb_query(
                    second,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                1
            )
            expect_identical(
                ddb_query(second, "SELECT * FROM sentinel")$value,
                42L
            )
            expect_error(factory(path, read_only = TRUE), "read_only")
            expect_error(
                factory(path, config = list(threads = "2")),
                "config.*threads"
            )
            ddb_disconnect(first)
            expect_identical(
                ddb_query(second, "SELECT * FROM sentinel")$value,
                42L
            )
            ddb_disconnect(second)
            # Error conditions can retain the old driver's call frame. Release
            # that instance explicitly before changing its read-only setting.
            duckdb::duckdb_shutdown(duckdb::duckdb(
                dbdir = path,
                config = list(threads = "1")
            ))
            readonly <- factory(path, read_only = TRUE)
            on.exit(ddb_disconnect(readonly), add = TRUE)
            expect_identical(
                ddb_query(readonly, "SELECT * FROM sentinel")$value,
                42L
            )
            expect_equal(
                ddb_query(
                    readonly,
                    "SELECT current_setting('threads') AS threads"
                )$threads,
                1
            )
            expect_error(
                ddb_exec(readonly, "INSERT INTO sentinel VALUES (43)"),
                "read.only|read-only"
            )
        }
    )
}

# Validate before creating native instances, and keep unrelated caller config
# on its established dbConnect path rather than silently changing its meaning.
test_that("DuckDB thread budget parsing is strict and explicit config wins", {
    withr::local_envvar(c(EPWSHIFTR_DB_THREADS = "3"))
    expect_identical(downloader__ddb_thread_config(), list(threads = "3"))
    expect_identical(
        downloader__ddb_thread_config(list(
            threads = "2",
            memory_limit = "1GB"
        )),
        list(threads = "2")
    )
    expect_identical(
        downloader__ddb_thread_config(list(worker_threads = "2")),
        list(worker_threads = "2")
    )
    expect_identical(
        downloader__ddb_thread_config(list(memory_limit = "1GB")),
        list(threads = "3")
    )
    for (value in c("0", "-1", "1.5", "Inf", "unknown", "2147483648")) {
        Sys.setenv(EPWSHIFTR_DB_THREADS = value)
        expect_error(
            downloader__ddb_thread_config(),
            "must be a positive integer"
        )
    }
    Sys.setenv(EPWSHIFTR_DB_THREADS = "")
    expect_null(downloader__ddb_thread_config())
})

# vim: fdm=marker :
