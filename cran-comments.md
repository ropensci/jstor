## Test environments
* local OS X install, R 4.0.0
* ubuntu 16.04 (on travis-ci), devel and release
* win-builder (devel and release)
* R-hub (windows, ubuntu, fedora)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a fix adressing changes in the current release of dplyr (v1.0.0)

Checking on R-hub fails for windows at the moment, but I believe this is 
unrelated to this package (installation issue within the tidyverse).
