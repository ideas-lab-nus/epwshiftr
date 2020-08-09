# Version 0.1.1

## Resubmission

### Test environments

* local R installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel)

### R CMD check results

0 errors | 0 warnings | 0 note

### Comments

Comments from CRAN maintainer:

```
'Packages which use Internet resources should fail gracefully with an
informative message if the resource is not available or has changed (and
not give a check warning nor error).'
```

Revisions:

* I added an informative message when the resource is not available.
* I updated tests accordingly to run certain tests only when the resource is
  available.
