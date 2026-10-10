# Local Windows checks

Before running the package tests on Windows, run
`uvr run tools/check-windows-paths.R` in the project's configured R environment.
The script exercises an RDS write, rename and read beyond 260 characters without
changing system settings. Extraction partitions and content-addressed morphing
artifacts can legitimately exceed this length even under a short temporary root.

If the check fails, verify that the installed R supports long paths and enable
**Enable Win32 long paths** with administrator approval. The corresponding
registry value is
`HKLM\SYSTEM\CurrentControlSet\Control\FileSystem\LongPathsEnabled` (`DWORD`, `1`).
Record the previous value before changing it. Start a new R process, rerun the
diagnostic and then run the complete package tests. A restart may be necessary
for applications that already cached the setting. See the
[Microsoft documentation](https://learn.microsoft.com/windows/win32/fileio/maximum-file-path-limitation).

Do not shorten hashes or change store identities to work around a disabled
Windows setting. A passing path diagnostic is only an environment check; it does
not substitute for passing package tests.

## Complete performance checks

Run `uvr sync`, then use the same entry point for isolated ordinary tests and
coverage. The following commands prepare the package once and execute three
fresh rounds, each with four independent test processes:

```sh
uvr run tools/check-performance.R -- ordinary /absolute/path/ordinary-results 4 3
uvr run tools/check-performance.R -- coverage /absolute/path/coverage-results 4 3
```

The output directory must not already exist. Preparation (including instrumented
package installation) is timed separately. Each test round includes fresh R
startup, dynamic coverage instrumentation, fixture setup, all selected tests,
teardown, worker exit checks, complete trace merging and report writes. A round
at or above 600 seconds fails the performance target; a timeout or incomplete
trace never passes. The runner checks one total deadline after exit verification,
before and after coverage merge/report, and before successful return. Trace
collection receives only the remaining budget, capped at its existing 30-second
allowance. Synchronous merge/report calls are checked when they return; the runner
cannot forcibly interrupt a synchronous R or native call while it is executing.
Avoid host sleep and other heavy workloads during comparison.

Ordinary checks retain the CI platform settings: `NOT_CRAN=false` on Windows and
`true` elsewhere. Coverage uses `NOT_CRAN=true` on every platform. Each round has
its own temporary, fixture, cache and trace directories; no generated fixture
cache is carried into another round. Test files retain alphabetical order within
each process. `tests/support/test-durations.csv` contains ordinary weights from
the measured Windows full suite; `coverage-durations.csv` uses measured Windows
coverage costs. Newly split workflow contracts initially share their prior case
cost in proportion to the retained end-to-end executions. These are scheduling weights, not timing assertions. New files
receive the median known weight. All files must appear exactly once in the result.

The runner fixes each test process's data.table, DuckDB and common OpenMP/BLAS
thread budget at one. The internal `EPWSHIFTR_DB_THREADS` setting is inherited by
workers; outside the runner, an unset value preserves production defaults and
explicit connection thread settings take precedence. Tests that exercise actual mirai concurrency still create their
required workers and dispatchers. CI selects up to three processes within its CPU and memory budget. Set
`EPWSHIFTR_TEST_SHARDS=1` for the original serial `R CMD check` test entry point;
values 2–8 enable isolated file scheduling and `auto` uses the CI resource budget. The standalone performance command
uses its explicit process-count argument.

Each round retains assertion results, file timings, the assignment, sampled peak
RSS and process count, CPU accounting and process-exit audit. CPU combines exit
records with OS samples; very short native children can be undercounted. Peak RSS
is sampled aggregate resident memory and can count shared pages more than once.
The `monitor_timing` table separates observation, receipt polling and exit
verification wall/CPU cost from test work. All test roots share one process
snapshot; Windows exit verification requires PID absence or a changed creation
time, while inaccessible identities remain pending until verified or timed out.
Coverage additionally requires identical complete source keys and source
locations, including zero counts, plus matching registration and trace receipts.
The coverage report includes the process trace manifest. Compare the saved
results by file and test description against a matching baseline before claiming
that assertions and skip conditions were preserved.

JIT observations are query-only: round metrics record the supervisor's `jit_level`
and `jit_env`, and each coverage process receipt and trace manifest records the
same fields before instrumentation. If inherited `R_ENABLE_JIT` is exactly `0`,
`1`, `2` or `3`, the timed collection phase requires every registered process and
the supervisor to match that level and environment value. Without an explicit
recognized value, the runner records observations without imposing a default.

For a Windows coverage comparison, set the environment before launching a fresh R
process, and restore the previous value afterwards. For example, in PowerShell:

```powershell
$previousJit = [Environment]::GetEnvironmentVariable('R_ENABLE_JIT', 'Process')
try {
    $env:R_ENABLE_JIT = '0'
    uvr run tools/check-performance.R -- coverage C:/path/to/new-output 6 1
} finally {
    [Environment]::SetEnvironmentVariable('R_ENABLE_JIT', $previousJit, 'Process')
}
```

This is an explicit per-session measurement setting, not an automatic policy.
The command prepares its instrumented package separately before the timed round.
The tools do not change JIT settings for ordinary runs, Mac runs or production.
The Mac coverage CI job retains its default JIT 3 setting; Windows local coverage
uses the explicit setting shown above.

The coverage preparation artifact reuses only source parse data and source
coordinates. Every process still creates fresh counters and instruments its own
namespace; cache identity binds the installed source and supported covr version.
The report reuses one complete line tally for CSV, summary and equivalent bulk
Cobertura XML generation.

Run `uvr run tools/test-check-runner-boundaries.R` for the lightweight runner
regressions. Durable fake shard results and a controlled clock check late exit,
merge/report deadline rejection, and inherited live-test opt-ins without starting
workers or using timed sleeps. Run `uvr run tools/test-check-monitor.R` separately
for PID/creation-time ownership and transient OS-query failures.

`tools/test-coverage-adapter.R` checks the collector against a small real installed
package, including subprocess traces and corrupt or missing receipts. The
performance entry point also detects missing executable permissions in a private
project copy of processx on Unix; it repairs only known, nonlinked, unshared
programs under this project's `.uvr/library`, and refuses shared cache changes.

The duration tables guide scheduling only. `coverage-durations.csv` contains the
measured costs of all 148 files from the v15 ThinkPad six-process coverage run;
these values are not estimates obtained by subtracting removed preparation. The
ordinary-test table retains estimates that scale earlier Windows timings for
removed preparation and moved assertions; those estimates are not measured
speedups. Acceptance uses actual complete-run wall time, CPU, process-exit checks
and coverage receipts.

Explicit local runs may use up to eight processes on a sufficiently provisioned
host. This does not raise CI's automatic three-process cap. Explicit counts bypass
automatic resource selection: use the CI budget of 6 GiB per process as a planning
baseline and leave room for the OS and other workloads. Compare total CPU and
peak RSS as well as elapsed time; more processes can increase both resource
costs and need not be faster. The
performance command retains a completed over-target round and stops immediately
instead of repeating an already disqualified candidate.
