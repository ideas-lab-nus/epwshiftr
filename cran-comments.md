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
   Found the following (possibly) invalid URLs:
     URL: https://pcmdi.llnl.gov/CMIP6 (moved to
https://pcmdi.llnl.gov/CMIP6/)
       From: DESCRIPTION
             man/epwshiftr-package.Rd
       Status: 200
       Message: OK
     URL: https://pcmdi.llnl.gov/CMIP6/TermsOfUse (moved to
https://pcmdi.llnl.gov/CMIP6/TermsOfUse/)
       From: README.md
       Status: 200
       Message: OK

-> trailing slashes, please.

Please fix and resubmit.
```

Revisions:

* I have revised corresponding files to add trailing slashes.
