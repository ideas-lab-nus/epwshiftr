# CI test execution

These files implement the ordinary-test and coverage workflows. `.Rbuildignore`
excludes `.github`; none is needed to load epwshiftr or run its weather APIs.

Normal `R CMD check` uses the serial `tests/testthat.R` entry. CI explicitly sets
`EPWSHIFTR_TEST_RUNNER` to the absolute path of `check-package.R`, which loads the
scheduler even after the check changes working directories. Automatic selection
allows up to three shards within CPU and memory budgets; an explicit count may
be 1–8.

`check-parallel.R` assigns every complete test file exactly once, isolates each
process's temporary files and caches, gathers test failures, verifies owned
process exits, and coordinates coverage collection. CI uses equal file costs;
it does not read or require machine-specific timing tables. This balances file
counts, not measured execution time. Earlier weighted timings cannot establish
the performance of this unweighted configuration.

The workflows install `processx` and `ps` explicitly. `callr` remains a package
`Suggests` dependency because ordinary tests use real independent R processes to
check cache isolation and DuckDB lock handshakes. Production APIs do not use it.

`../coverage.R` instruments each process and validates complete registered traces
before merging counters and writing reports. Missing traces or unresolved owned
lifetimes fail a run; test assertions alone do not establish complete execution.

The coverage workflow runs the regression scripts in `tests/` first:

- Monitor and resource checks cover ownership, PID reuse, query failures,
  resource selection and deadline handling.
- Adapter and report checks cover missing/corrupt receipts, fresh counters and
  output equivalence, using a tiny installed test package when required.

These test the CI implementation, not scientific formulas. They prevent the
runner from reporting success after dropping tests, worker failures or coverage.
Run an individual check from the repository root with an environment containing
the workflow dependencies, for example:

```sh
Rscript .github/testing/tests/test-check-resources.R
```

Machine-specific benchmarks, multi-round acceptance drivers, long-path diagnosis
and local environment locks are not CI inputs and are not tracked here.
