## Test environments
* local Windows 10 install, R 4.1.2
* ubuntu 20.04 (GitHub actions), devel and release
* macOS-latest (GitHub actions), release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a fix addressing changes in readr (v2.0.0). Changes in readr where the
reason for the check errors that led to the removal of `jstor` from CRAN.
As the tests and checks noted above indicate, all errors have been resolved.

* The notes regarding the links are spurious (jstor.org blocking access), as in
previous submissions.

* Changed all occurences of `T` or `F` to `TRUE` and `FALSE`, as requested.

* Removed file LICENSE and mention of it from DESCRIPTION, as requested.

* Added `return` statements to the documentation of all requested files.


