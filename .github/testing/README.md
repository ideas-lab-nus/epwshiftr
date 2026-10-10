# Repository test execution

These files support CI and explicit developer performance runs. `.Rbuildignore`
excludes `.github`, so the scheduler, cost tables, regression scripts and
performance command are not shipped in the R source package.

## Entry points and dependencies

- Normal `R CMD check` runs `tests/testthat.R` serially. CI sets
  `EPWSHIFTR_TEST_RUNNER` to the absolute path of `check-package.R`, which loads
  `check-parallel.R` from this directory even when the check changes directories.
- `check-package.R` supports `EPWSHIFTR_TEST_SHARDS=auto` or an explicit 1–8.
  Automatic selection allows up to three shards within CPU and memory budgets.
- The workflows install `processx` and `ps` explicitly. `callr` remains a package
  `Suggests` dependency because ordinary package tests use it for real process
  isolation and the DuckDB lock handshake.
- `../coverage.R` prepares per-process instrumentation and validates complete
  registered traces before merging counters and writing reports. Coverage CI
  executes the regression scripts in `tests/` before the full coverage run.

The scheduler assigns every complete test file exactly once, preserves
alphabetical order within a shard, isolates temporary files/caches and verifies
owned process exits by PID and creation time. Missing traces and unresolved
owned lifetimes fail the run; incomplete execution does not count as success.

## Scheduling costs

`test-durations.csv` and `coverage-durations.csv` are load-balancing inputs,
not fixtures, timing assertions or acceptance results. They contain file names
and cost estimates; new files use the median known cost. The ordinary table
includes estimates for moved assertions and consolidated preparation. The
coverage table contains measured costs from a Windows full-suite run. They may
be refreshed from `files.csv` after a complete representative run; machine and
concurrency differences mean these weights do not predict exact elapsed time.

## Explicit local performance runs

Use a provisioned R environment with package test dependencies plus `processx`,
`ps`, `covr` and `xml2`. Local environment/lock files are not repository inputs.
From the package root:

```sh
Rscript .github/testing/check-performance.R ordinary /path/to/new-output 4 3
Rscript .github/testing/check-performance.R coverage /path/to/another-output 4 3
```

Preparation is timed separately. Each round includes startup, dynamic
instrumentation, tests, teardown, exit verification and coverage merge/report.
Each round has independent directories and no generated cross-round cache.
The performance command retains an over-600-second result and stops; this local
acceptance threshold is not imposed on ordinary package users. Synchronous native
calls cannot be interrupted by a deadline check inside R.

Explicit shard counts bypass automatic resource selection. More workers can
increase total CPU and memory as well as contention. Metrics retain sampled RSS,
process-exit CPU and observation gaps; short-lived children may be missed and
shared memory may be counted more than once. Background time is not deducted.
JIT defaults are unchanged; explicit `R_ENABLE_JIT` values are recorded and
checked across coverage receipts. The Windows `R_ENABLE_JIT=0` experiments do not
set a production or cross-platform default.

Run individual tooling regressions from the package root, for example:

```sh
Rscript .github/testing/tests/test-check-resources.R
Rscript .github/testing/tests/test-coverage-adapter.R
```

Monitor tests cover PID reuse, transient query failures and deadline boundaries;
adapter/report tests cover missing/corrupt receipts, fresh counters and report
equivalence. They are kept outside package tests because some deliberately build
and install a tiny package. Host-specific long-path and executable-permission
diagnostics belong to the local environment, not these workflows.
