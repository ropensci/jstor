## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), devel and release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a quick fix to an issue raised by Kurt Hornik per mail. One test was
writing to a directory which was not the R session's temporary directory due to
a typo. In response `paste0(tmp, "/something")` has been replaced in many places
with `file.path(tmp, "something")`, as suggested. 
