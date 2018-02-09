
<!-- README.md is generated from README.Rmd. Please edit that file -->
jstor
=====

[![Travis build status](https://travis-ci.org/tklebel/jstor.svg?branch=master)](https://travis-ci.org/tklebel/jstor) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tklebel/jstor?branch=master&svg=true)](https://ci.appveyor.com/project/tklebel/jstor) [![Coverage status](https://codecov.io/gh/tklebel/jstor/branch/master/graph/badge.svg)](https://codecov.io/github/tklebel/jstor?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/jstor)](https://cran.r-project.org/package=jstor) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/jstor)](https://CRAN.R-project.org/package=jstor) [![rOpenSci badge](https://badges.ropensci.org/189_status.svg)](https://github.com/ropensci/onboarding/issues/189) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1169862.svg)](https://doi.org/10.5281/zenodo.1169862)

The tool [Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR is a valuable source for citation analysis and text mining. `jstor` provides functions and suggests workflows for importing datasets from DfR. It was developed to deal with very large datasets which require an agreement, but can be used with smaller ones as well.

The most important set of functions is a group of `find_*` functions:

-   `find_article`
-   `find_authors`
-   `find_references`
-   `find_footnotes`
-   `find_fulltext`
-   `find_book`
-   `find_chapters`

All functions which are concerned with meta data (therefore excluding `find_fulltext`) operate along the same lines:

1.  The file is read with `xml2::read_xml()`.
2.  Content of the file is extracted via XPATH or CSS-expressions.
3.  The resulting data is returned in a `tibble`.

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

The basic usage is simple: supply one of the `find_*`-functions with a path and it will return a tibble with the extracted information.

``` r
find_article(jstor_example("sample_with_references.xml")) %>% knitr::kable()
```

| basename\_id             | journal\_doi | journal\_jcode   | journal\_pub\_id | article\_doi    | article\_pub\_id | article\_jcode | article\_type    | article\_title                     | volume | issue | language | pub\_day | pub\_month |  pub\_year|  first\_page|  last\_page|
|:-------------------------|:-------------|:-----------------|:-----------------|:----------------|:-----------------|:---------------|:-----------------|:-----------------------------------|:-------|:------|:---------|:---------|:-----------|----------:|------------:|-----------:|
| sample\_with\_references | NA           | tranamermicrsoci | NA               | 10.2307/3221896 | NA               | NA             | research-article | On the Protozoa Parasitic in Frogs | 41     | 2     | eng      | 1        | 4          |       1922|           59|          76|

``` r

find_authors(jstor_example("sample_with_references.xml")) %>% knitr::kable()
```

| basename\_id             | prefix | given\_name | surname | string\_name | suffix |  author\_number|
|:-------------------------|:-------|:------------|:--------|:-------------|:-------|---------------:|
| sample\_with\_references | NA     | R.          | Kudo    | NA           | NA     |               1|

Further explanations, especially on how to use jstor's functions for importing many files, can be found in the vignettes.

Getting started
---------------

In order to use `jstor`, you need some data from DfR. From the [main page](http://www.jstor.org/dfr/) you can create a dataset by searching for terms and restricting the search regarding time, subject and content type. After you created an account, you can download your selection. Alternatively, you can download [sample datasets](http://www.jstor.org/dfr/about/sample-datasets) with documents from before 1923 for the US, and before 1870 for all other countries.

Supported Elements
------------------

In their [technical specifications](http://www.jstor.org/dfr/about/technical-specifications), DfR lists fields which should be reliably present in all articles and books.

The following table gives an overview, which elements are supported by `jstor`.

### Articles

| `xml`-field                      | reliably present | supported in `jstor` |
|:---------------------------------|:-----------------|:---------------------|
| journal-id (type="jstor")        | x                | x                    |
| journal-id (type="publisher-id") | x                | x                    |
| journal-id (type="doi")          |                  | x                    |
| issn                             | x                |                      |
| journal-title                    | x                |                      |
| publisher-name                   | x                |                      |
| article-id (type="doi")          | x                | x                    |
| article-id (type="jstor")        | x                | x                    |
| article-id (type="publisher-id") |                  | x                    |
| article-type                     |                  | x                    |
| volume                           |                  | x                    |
| issue                            |                  | x                    |
| article-categories               | x                |                      |
| article-title                    | x                | x                    |
| contrib-group                    | x                | x                    |
| pub-date                         | x                | x                    |
| fpage                            | x                | x                    |
| lpage                            |                  | x                    |
| product                          | x                |                      |
| self-uri                         | x                |                      |
| kwd-group                        | x                |                      |
| custom-meta-group                | x                | x                    |
| fn-group (footnotes)             |                  | x                    |
| ref-list (references)            |                  | x                    |

### Books

| `xml`-field            | reliably present | supported in `jstor` |
|:-----------------------|:-----------------|:---------------------|
| book-id (type="jstor") | x                | x                    |
| discipline             | x                | x                    |
| call-number            | x                |                      |
| lcsh                   | x                |                      |
| book-title             | x                | x                    |
| book-subtitle          |                  | x                    |
| contrib-group          | x                | x                    |
| pub-date               | x                | x                    |
| isbn                   | x                | x                    |
| publisher-name         | x                | x                    |
| publisher-loc          | x                | x                    |
| permissions            | x                |                      |
| self-uri               | x                |                      |
| counts                 | x                | x                    |
| custom-meta-group      | x                | x                    |

### Book Chapters

| `xml`-field            | reliably present | supported in `jstor` |
|:-----------------------|:-----------------|:---------------------|
| book-id (type="jstor") | x                | x                    |
| part\_id               | x                | x                    |
| part\_label            | x                | x                    |
| part-title             | x                | x                    |
| part-subtitle          |                  | x                    |
| contrib-group          | x                | x                    |
| fpage                  | x                | x                    |
| abstract               | x                | x                    |

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Citation
--------

To cite `jstor`, please refer to `citation(package = "jstor")`.

Acknowledgements
----------------

Work on `jstor` benefited from financial support for the project "Academic Super-Elites in Sociology and Economics" by the Austrian Science Fund (FWF), project number "P 29211 Einzelprojekte".
