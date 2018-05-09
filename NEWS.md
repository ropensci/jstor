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


## Minor changes
* improved documentation regarding endnotes (thanks @elinw)
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
