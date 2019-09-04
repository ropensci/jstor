# jstor 0.3.7
This is a small release to fix compatibility with `tidyr v1.0.0`. There
are no other changes.

# jstor 0.3.6
This is another small release to fix compatibility with `readr v1.3.0` and
`tibble v2.0.0`. There are no other changes.


# jstor 0.3.5
This is a small release, mainly to fix compatibility with version `1.2.0` of 
`readr`. There is one breaking change however:

## Breaking changes
* Output column names for `jst_get_refernces` have been renamed to avoid 
ambiguity when matching with output from `jst_get_article`. All columns now have
a `ref_*` prefix.


# jstor 0.3.4
* Added option to parse references, if the information is available. #32
* Output from error messages is "re-imported" properly as well.
* Example files have been renamed for clarity. `sample_` has been replaced with
`article_` or removed altogether.
* Reworked internals for catching misspecifications in `jst_define_import` due 
to upcoming release of `rlang v0.3.0`.

# jstor 0.3.3

## Removed functionality
* The option to download the most recent data on journals directly from JSTOR
(as in `jst_get_journal_overview(most_recent = T)`)
had to be removed due changes on their server. I will try to find a solution
with JSTOR support so we can add the functionality again.

## New features
* `jst_define_import` now prints the specification in a pretty and informative
way.
* `jst_define_import` now checks the definition more extensively: 
`jst_define_import(article = jst_get_book)` or similar mis-specifications 
will raise an error.
* Import the crayon package for more colourful error messages in some places,
which are easier to read.

## Bug fixes
* Removed an outdated function from the vignette on batch importing files.

## Other changes
* The old group of `find_*` functions is now defunct (they raise an error).
* Updated cached version of journal data.

# jstor 0.3.2
This is a hotfix to resolve an issue with writing to other directories than
temporary folders during tests, which should not have happend in the first 
place.


# jstor 0.3.1

* `jstor` is now part of rOpenSci.
* removed arguments `new_col` for `jst_unify_journal_id` and
`jst_add_total_pages`, since both built on the dev version of rlang. Once this
version is on CRAN, they will be re-introduced.
* fixed a few issues in README and man files regarding changes introduced with 
0.3.0.

# jstor 0.3.0

## Breaking changes
`jst_import` and `jst_import_zip` now use futures as a backend for parallel 
processing. This makes internals more compact and reduces dependencies. 
Furthermore this reduces the number of arguments, since the argument `cores` 
has been removed. By default, the functions run sequentially. If you want them
to execute in parallel, use futures:
```
library(future)
plan(multiprocess)

jst_import_zip("zip-archive.zip",
               import_spec = jst_define_import(article = jst_get_article),
               out_file = "outfile")
```
If you want to terminate the proceses, at least on *nix-systems you need to kill
them manually (once again).

* All functions have been renamed to use a unified naming scheme: `jst_*`.
The former group of `find_*` functions is now called `jst_get_*`, as in
`jst_get_article()`. The previous functions have been deprecated and will be 
removed before submission to CRAN.
* The unique identifier for matching across files has been renamed to 
`file_name`, and the corresponding helper to get this file name from
`get_basename` to `jst_get_file_name`.

## Importing data directly from zip-files
There is a new set of functions which lets you directly import files from 
.zip-archives: `jst_import_zip()` and `jst_define_import()`.

In the following example, we have a zip-archive from DfR and want to import
metadata on books and articles. For all articles we want to apply
`jst_get_article()` and `jst_get_authors()`, for books only `jst_get_book()`, 
and we want to read unigrams (ngram1).

First we specify what we want, and then we apply it to our zip-archive:
```r
# specify definition
import_spec <- jst_define_import(article = c(jst_get_article, jst_get_authors),
                                 book = jst_get_book,
                                 ngram1 = jst_get_ngram)

# apply definition to archive
jst_import_zip("zip_archive.zip",
               import_spec = import_spec,
               out_file = "out_path")
```

If the archive contains also research reports, pamphlets or other ngrams, they
will not be imported. We could however change our specification, if we wanted
to import all kinds of ngrams (given that we originally requested them from
DfR):

```r
# import multiple forms of ngrams
import_spec <- jst_define_import(article = c(jst_get_article, jst_get_authors),
                                 book = jst_get_book,
                                 ngram1 = jst_get_ngram,
                                 ngram2 = jst_get_ngram,
                                 ngram3 = jst_get_ngram)
```

Note however that for larger archives, importing all ngrams takes a very long
time. It is thus advisable to only import ngrams for articles which you
want to analyse, i.e. most likely a subset of the initial request. The new 
function `jst_subset_ngrams()` helps you with this (see also the section on
importing bigrams in the 
[case study](https://ropensci.github.io/jstor/articles/analysing-n-grams.html#importing-bigrams).

Before importing all files from a zip-archive, you can get a quick overview with
`jst_preview_zip()`.

## New vignette
The new `vignette("known-quirks")` lists common problems with data from
JSTOR/DfR. Contributions with further cases are welcome!


## New functions
* New function `jst_get_journal_overview()` supplies a tibble with contextual
information about the journals in JSTOR.
* New function `jst_combine_outputs()` applies `jst_re_import()` to a whole 
directory and lets you combine all related files in one go. It uses the file 
structure that `jst_import()` and `jst_import_zip()` provide as a heuristic: a
filename with a dash and one or multiple digits at its end (`filename-1.csv`). 
All files
with identical names (disregarding dash and digits) are combined into one file.
* New function `jst_re_import()` lets you re_import a `.csv` file that
`jstor_import()` or `jst_import_zip()` had exported. It tries to guess the type
of
content based on the column names or, if column names are not available, from
the number of columns, raising a warning if guessing fails and reverting to a
generic import.
* A new function `jst_subset_ngrams()` lets you create a subset of ngram files
within a zip-file which you can import with `jst_get_ngram()`.
* A new set of convenience functions for taking a few cleaning steps:
`jst_clean_page()` tries to turn a character vector with pages into a numeric
one, `jst_unify_journal_id()` merges different specifications of journals into
one, `jst_add_total_pages()` adds a total count of pages per article, and
`jst_augment()` calls all three functions to clean the data set in one go.


## Minor changes
* Improved documentation regarding endnotes (thanks @elinw)
* jst_import and jst_import_zip have a new argument: `n_batches` which lets you 
specify the number of batches directly


# jstor 0.2.6

* added lengthy case study at https://tklebel.github.io/jstor/articles/analysing-n-grams.html
* added a pkgdown site at https://tklebel.github.io/jstor/
* changed implementation of parallel execution in `jstor_import` from 
`parallel::mclapply` to `foreach::foreach` with `snow` as a backend for
`%dopar%`. 
* added support for progress bars #34
* `jstor_import` now writes column names by default #29
* new helper `get_basename` helps to get the basename of a file without its
extension
* `find_article` does not coerce days and months to integer any more, since there
might be information stored as text.
* Added a `NEWS.md` file to track changes to the package.
