# Version 0.1.0

## Resubmission

### Test environments

* local R installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel)

### R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

### Comments

Comments from CRAN maintainer:

```
Thanks, please replace \dontrun{} by \donttest{} in your Rd-files, if
the functions do not need an API key.

Please ensure that your functions do not modify (save or delete) the
user's home filespace in your examples/vignettes/tests. That is not
allow by CRAN policies. Please only write/save files if the user has
specified a directory. In your examples/vignettes/tests you can write to
tempdir().

Please fix and resubmit.
```

Revisions:

* I change the example of `get_data_node()` to `\donttest{}`.
* Now all functions will not write to user home filespace by default.
* I change Tests so that it only writes to `tempdir()`.
