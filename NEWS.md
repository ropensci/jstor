# jstor 0.2.6.9000

## Major changes
There is a new set of functions which lets you directly import files from 
.zip-archives: `jst_import_zip()` and `jst_define_import()`.

In the following example, we have a zip-archive from DfR and want to import
metadata on books and articles. For all articles we want to apply
`find_article()` and `find_authors()`, for books only `find_book()`, and we want
to read unigrams (ngram1).

First we specify what we want, and then we apply it to our zip-archive.
```r
# specify definition
import_spec <- jst_define_import(article = c(find_article, find_authors),
                                 book = find_book,
                                 ngram1 = jst_read_ngram)

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
import_spec <- jst_define_import(article = c(find_article, find_authors),
                                 book = find_book,
                                 ngram1 = jst_read_ngram,
                                 ngram2 = jst_read_ngram,
                                 ngram3 = jst_read_ngram)
```

Before importing all files from a zip-archive, you can get a quick overview with
`jst_preview_zip()`.


## Other changes
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
* A new set of convenience functions for taking a few cleaning steps:
`jst_clean_page()` tries to turn a character vector with pages into a numeric
one, `jst_unify_journal_id()` merges different specifications of journals into
one, `jst_add_total_pages()` adds a total count of pages per article, and
`jst_augment()` calls all three functions to clean the data set in one go.
* A new function `jst_subset_ngrams` lets you create a subset of ngram files
within a zip-file which you can import with `jst_get_ngram`.
* The unique identifier for matching across files has been renamed to 
`file_name`, and the corresponding helper to get this file name from
`get_basename` to `jst_get_file_name`.
* Functions have been renamed: All `find_*` functions are now called
`jst_get_*`. 
* New function `jst_combine_outputs` applies `jst_re_import` to a whole 
directory and lets you combine all related files in one go. It uses the file 
structure that `jst_import` and `jst_import_zip` provide as a heuristic: a
filename
with a dash and one or multiple digits at its end (`filename-1.csv`). All files
with identical names (disregarding dash and digits) are combined into one file.
* New function `jst_re_import` lets you re_import a `.csv` file that
`jstor_import` or `jst_import_zip` had exported. It tries to guess the type of
content based on the column names or, if column names are not available, from
the number of columns, raising a warning if guessing fails and reverting to a
generic import.
* New function `jst_get_journal_overview` supplies a tibble with contextual
information about the journals in JSTOR.
* Improved documentation regarding endnotes (thanks @elinw)
* jstor_import has a new argument: `n_batches` which lets you specify the number
of batches directly



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
