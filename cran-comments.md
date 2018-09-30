## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), devel and release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a fix adressing changes on the servers of JSTOR. They are now blocking
requests from scripts, which is why the CRAN checks failed.
This is also the reason for the note regarding possibly invalid URLs. The links
in the vignettes are correct and work when used in a browser.
