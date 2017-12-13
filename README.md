[![Travis build status](https://travis-ci.org/tklebel/jstor.svg?branch=master)](https://travis-ci.org/tklebel/jstor)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tklebel/jstor?branch=master&svg=true)](https://ci.appveyor.com/project/tklebel/jstor)
[![CRAN status](http://www.r-pkg.org/badges/version/jstor)](https://cran.r-project.org/package=jstor)

# jstor

The tool [Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR is a
valuable source for citation analysis and text mining. Requests for datasets can
be made for small excerpts (max. 25,000 records) or large ones, which require an
agreement between the researcher and JSTOR.

The package `jstor` provides functions and suggests workflows for importing
datasets from DfR. It was developed to deal with very large datasets which
require an agreement, but can be used with smaller ones as well.

The most important set of functions is a group of `find_*` functions:

- `find_meta`
- `find_authors`
- `find_references`
- `find_footnotes`
- `find_fulltext`

All functions which are concerned with meta data (therefore excluding
`find_fulltext`) operate along the same lines:

1. The file is read with `xml2::read_xml()`.
2. Content of the file is extracted via XPATH or CSS-expressions.
3. The resulting data is returned in a `data.frame`.

# Installation
Currently `jstor` is not available on CRAN. The development version can be
installed with `devtools`:
```r
devtools::install_github("tklebel/jstor")
```

