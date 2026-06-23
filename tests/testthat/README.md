# Test organization

- Prefer one primary `test-foo.R` file for each `R/foo.R` source file.
- Use suffix files only for clear boundaries:
  - `test-foo-live.R` for explicit opt-in tests against external services.
  - `test-foo-compat.R` for deprecated or legacy compatibility contracts.
- For R6 classes, keep public method contracts in method-named `test_that()` blocks, for example `Class$method()`.
- Snapshot public `print()` methods so CLI output stays intentional.
- Name helpers by domain and keep cross-cutting behavior small, for example `helper-esgf-fixtures.R` or `helper-live-esgf.R`.
