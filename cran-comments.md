## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), devel and release
* win-builder (devel and release)
* R-hub (windows, ubuntu, fedora)

## R CMD check results

0 errors | 0 warnings | 2 note

* This is a fix adressing changes in the upcoming release of tidyr v1.0.0

The links in the vignettes are correct and work when used in a browser, as in
the last release.

The note on "Missing or unexported objects" is about the function
`'tidyr::unnest_legacy'`, previously not part of `tidyr` that I utilize for 
users that have a tidyr version before v1.0.0.
