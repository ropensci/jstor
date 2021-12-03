
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jstor: Import and Analyse Data from Scientific Articles

**Author:** [Thomas Klebel](https://thomasklebel.eu) <br> **License:**
[GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html)

[![R-CMD-check](https://github.com/ropensci/jstor/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/ropensci/jstor/actions/workflows/check-standard.yaml)
[![AppVeyorBuild
status](https://ci.appveyor.com/api/projects/status/sry2gtwam7qyfw6l?svg=true)](https://ci.appveyor.com/project/tklebel/jstor)
[![Coverage
status](https://codecov.io/gh/ropensci/jstor/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/jstor?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](http://www.r-pkg.org/badges/version/jstor)](https://cran.r-project.org/package=jstor)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/jstor)](https://CRAN.R-project.org/package=jstor)
[![rOpenSci
badge](https://badges.ropensci.org/189_status.svg)](https://github.com/ropensci/onboarding/issues/189)
[![JOSS
badge](http://joss.theoj.org/papers/ba29665c4bff35c37c0ef68cfe356e44/status.svg)](http://joss.theoj.org/papers/ba29665c4bff35c37c0ef68cfe356e44)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1169861.svg)](https://doi.org/10.5281/zenodo.1169861)

The tool [Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR
is a valuable source for citation analysis and text mining. `jstor`
provides functions and suggests workflows for importing datasets from
DfR. It was developed to deal with very large datasets which require an
agreement, but can be used with smaller ones as well.

The most important set of functions is a group of `jst_get_*` functions:

  - `jst_get_article`
  - `jst_get_authors`
  - `jst_get_references`
  - `jst_get_footnotes`
  - `jst_get_book`
  - `jst_get_chapters`
  - `jst_get_full_text`
  - `jst_get_ngram`

All functions which are concerned with meta data (therefore excluding
`jst_get_full_text` and `jst_get_ngram`) operate along the same lines:

1.  The file is read with `xml2::read_xml()`.
2.  Content of the file is extracted via XPATH or CSS-expressions.
3.  The resulting data is returned in a `tibble`.

## Installation

To install the package use:

``` r
install.packages("jstor")
```

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/jstor")
```

## Usage

In order to use `jstor`, you first need to load it:

``` r
library(jstor)
library(magrittr)
```

The basic usage is simple: supply one of the `jst_get_*`-functions with
a path and it will return a tibble with the extracted
information.

``` r
jst_get_article(jst_example("article_with_references.xml")) %>% knitr::kable()
```

| file\_name                | journal\_doi | journal\_jcode   | journal\_pub\_id | journal\_title                                     | article\_doi    | article\_pub\_id | article\_jcode | article\_type    | article\_title                     | volume | issue | language | pub\_day | pub\_month | pub\_year | first\_page | last\_page | page\_range |
| :------------------------ | :----------- | :--------------- | :--------------- | :------------------------------------------------- | :-------------- | :--------------- | :------------- | :--------------- | :--------------------------------- | :----- | :---- | :------- | :------- | :--------- | --------: | :---------- | :--------- | :---------- |
| article\_with\_references | NA           | tranamermicrsoci | NA               | Transactions of the American Microscopical Society | 10.2307/3221896 | NA               | NA             | research-article | On the Protozoa Parasitic in Frogs | 41     | 2     | eng      | 1        | 4          |      1922 | 59          | 76         | 59-76       |

``` r

jst_get_authors(jst_example("article_with_references.xml")) %>% knitr::kable()
```

| file\_name                | prefix | given\_name | surname | string\_name | suffix | author\_number |
| :------------------------ | :----- | :---------- | :------ | :----------- | :----- | -------------: |
| article\_with\_references | NA     | R.          | Kudo    | NA           | NA     |              1 |

Further explanations, especially on how to use jstor’s functions for
importing many files, can be found in the vignettes.

## Getting started

In order to use `jstor`, you need some data from DfR. From the [main
page](http://www.jstor.org/dfr/) you can create a dataset by searching
for terms and restricting the search regarding time, subject and content
type. After you created an account, you can download your selection.
Alternatively, you can download [sample
datasets](http://www.jstor.org/dfr/about/sample-datasets) with documents
from before 1923 for the US, and before 1870 for all other countries.

## Supported Elements

In their [technical
specifications](http://www.jstor.org/dfr/about/technical-specifications),
DfR lists fields which should be reliably present in all articles and
books.

The following table gives an overview, which elements are supported by
`jstor`.

### Articles

| `xml`-field                      | reliably present | supported in `jstor` |
| :------------------------------- | :--------------- | :------------------- |
| journal-id (type=“jstor”)        | x                | x                    |
| journal-id (type=“publisher-id”) | x                | x                    |
| journal-id (type=“doi”)          |                  | x                    |
| issn                             | x                |                      |
| journal-title                    | x                | x                    |
| publisher-name                   | x                |                      |
| article-id (type=“doi”)          | x                | x                    |
| article-id (type=“jstor”)        | x                | x                    |
| article-id (type=“publisher-id”) |                  | x                    |
| article-type                     |                  | x                    |
| volume                           |                  | x                    |
| issue                            |                  | x                    |
| article-categories               | x                |                      |
| article-title                    | x                | x                    |
| contrib-group                    | x                | x                    |
| pub-date                         | x                | x                    |
| fpage                            | x                | x                    |
| lpage                            |                  | x                    |
| page-range                       |                  | x                    |
| product                          | x                |                      |
| self-uri                         | x                |                      |
| kwd-group                        | x                |                      |
| custom-meta-group                | x                | x                    |
| fn-group (footnotes)             |                  | x                    |
| ref-list (references)            |                  | x                    |

### Books

| `xml`-field            | reliably present | supported in `jstor` |
| :--------------------- | :--------------- | :------------------- |
| book-id (type=“jstor”) | x                | x                    |
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
| :--------------------- | :--------------- | :------------------- |
| book-id (type=“jstor”) | x                | x                    |
| part\_id               | x                | x                    |
| part\_label            | x                | x                    |
| part-title             | x                | x                    |
| part-subtitle          |                  | x                    |
| contrib-group          | x                | x                    |
| fpage                  | x                | x                    |
| abstract               | x                | x                    |

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

## Citation

To cite `jstor`, please refer to `citation(package =
    "jstor")`:

    Klebel (2018). jstor: Import and Analyse Data from Scientific Texts. Journal of 
    Open Source Software, 3(28), 883, https://doi.org/10.21105/joss.00883

## Acknowledgements

Work on `jstor` benefited from financial support for the project
“Academic Super-Elites in Sociology and Economics” by the Austrian
Science Fund (FWF), project number “P 29211 Einzelprojekte”.

Some internal functions regarding file paths and example files were
adapted from the package
`readr`.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
