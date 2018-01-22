
<!-- README.md is generated from README.Rmd. Please edit that file -->
jstor
=====

[![Travis build status](https://travis-ci.org/tklebel/jstor.svg?branch=master)](https://travis-ci.org/tklebel/jstor) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tklebel/jstor?branch=master&svg=true)](https://ci.appveyor.com/project/tklebel/jstor) [![Coverage status](https://codecov.io/gh/tklebel/jstor/branch/master/graph/badge.svg)](https://codecov.io/github/tklebel/jstor?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/jstor)](https://cran.r-project.org/package=jstor) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/jstor)](https://CRAN.R-project.org/package=jstor)

The tool [Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR is a valuable source for citation analysis and text mining. `jstor` provides functions and suggests workflows for importing datasets from DfR. It was developed to deal with very large datasets which require an agreement, but can be used with smaller ones as well.

The most important set of functions is a group of `find_*` functions:

-   `find_article`
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
find_article(jstor_example("sample_with_references.xml")) %>% knitr::kable()
```

| journal\_id      | basename\_id             | article\_id     | article\_type    | article\_title                     | volume | issue | language |  pub\_day|  pub\_month|  pub\_year|  first\_page|  last\_page|
|:-----------------|:-------------------------|:----------------|:-----------------|:-----------------------------------|:-------|:------|:---------|---------:|-----------:|----------:|------------:|-----------:|
| tranamermicrsoci | sample\_with\_references | 10.2307/3221896 | research-article | On the Protozoa Parasitic in Frogs | 41     | 2     | eng      |         1|           4|       1922|           59|          76|

``` r

find_authors(jstor_example("sample_with_references.xml")) %>% knitr::kable()
```

| basename\_id             | prefix | given\_name | surname | string\_name | suffix |  author\_number|
|:-------------------------|:-------|:------------|:--------|:-------------|:-------|---------------:|
| sample\_with\_references | NA     | R.          | Kudo    | NA           | NA     |               1|

Further explanations, especially on how to use jstor's functions for importing many files, can be found in the vignettes.

Getting started
---------------

In order to use `jstor`, you need some data from DfR. From the [main page](http://www.jstor.org/dfr/) you can create a dataset by searching for terms and restricting the search regarding time, subject and content type. After you created an account, you can download your selection. In case you don't have access to the content, you can download [sample datasets](http://www.jstor.org/dfr/about/sample-datasets) with documents from before 1923 for the US, and before 1870 for all other countries.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Citation
--------

To cite `jstor`, please refer to `citation(package = "jstor")`.

Acknowledgements
----------------

Work on `jstor` benefited from financial support for the project "Academic Super-Elites in Sociology and Economics" by the Austrian Science Fund (FWF), project number "P 29211 Einzelprojekte".
