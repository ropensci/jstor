
<!-- README.md is generated from README.Rmd. Please edit that file -->
jstor
=====

[![Travis build status](https://travis-ci.org/tklebel/jstor.svg?branch=master)](https://travis-ci.org/tklebel/jstor) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tklebel/jstor?branch=master&svg=true)](https://ci.appveyor.com/project/tklebel/jstor) [![CRAN status](http://www.r-pkg.org/badges/version/jstor)](https://cran.r-project.org/package=jstor)

The tool [Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR is a valuable source for citation analysis and text mining. `jstor` provides functions and suggests workflows for importing datasets from DfR.

The package `jstor` provides functions and suggests workflows for importing datasets from DfR. It was developed to deal with very large datasets which require an agreement, but can be used with smaller ones as well.

The most important set of functions is a group of `find_*` functions:

-   `find_metadata`
-   `find_authors`
-   `find_references`
-   `find_footnotes`
-   `find_fulltext`

All functions which are concerned with meta data (therefore excluding `find_fulltext`) operate along the same lines:

1.  The file is read with `xml2::read_xml()`.
2.  Content of the file is extracted via XPATH or CSS-expressions.
3.  The resulting data is returned in a `data.frame`.

Installation
------------

You can install `jstor` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("tklebel/jstor")
```

Usage
-----

In order to use `jstor`, you first need to load it:

``` r
library(jstor)
```

The basic usage is simple: supply one of the `find_*`-functions with a path and it will return a data.frame with the extracted information.

``` r
find_metadata(jstor_example("sample_with_references.xml"))
#>         journal_id            basename_id      article_id
#> 1 tranamermicrsoci sample_with_references 10.2307/3221896
#>                        article_title volume issue language pub_day
#> 1 On the Protozoa Parasitic in Frogs     41     2      eng       1
#>   pub_month pub_year first_page last_page
#> 1         4     1922         59        76

find_authors(jstor_example("sample_with_references.xml"))
#>              basename_id prefix given_name surname string_name suffix
#> 1 sample_with_references   <NA>         R.    Kudo        <NA>   <NA>
#>   author_number
#> 1             1
```

Further explanations, especially on how to use jstor's functions for importing many files, can be found in the vignettes.
